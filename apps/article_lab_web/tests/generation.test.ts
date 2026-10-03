import { beforeAll, afterAll, describe, it, expect } from "vitest";
import { Miniflare, convertV4MiniflareOptions } from "miniflare";
import { readFile } from "node:fs/promises";
import { createApp } from "../worker/index";
import type { Env } from "../worker/index";

const origin = "https://review.test";
const runnerToken = "integration-runner-token-0123456789abcdef";
const routes = [
  {
    id: "glm-omniroute",
    label: "GLM subscription · OmniRoute",
    provider: "glm",
    transport: "omniroute",
    model: "glm/glm-5.3",
    response_models: ["glm-5.3"],
  },
];

let mf: Miniflare;
let env: Env;
const app = createApp(async (req) => {
  const email = req.headers.get("test-user") ?? "owner@example.test";
  return {
    email,
    name: email,
    authId: email.toLowerCase(),
  };
});
function png(width = 640, height = 400): Uint8Array<ArrayBuffer> {
  const bytes = new Uint8Array(8 + 4 + 4 + 13);
  bytes.set([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a], 0);
  const view = new DataView(bytes.buffer);
  view.setUint32(8, 13);
  bytes.set([0x49, 0x48, 0x44, 0x52], 12);
  view.setUint32(16, width);
  view.setUint32(20, height);
  return bytes;
}
async function call(
  path: string,
  method = "GET",
  data?: unknown,
  user = "owner@example.test",
  headers: Record<string, string> = {},
) {
  const response = await app.fetch(
    new Request(`${origin}/api${path}`, {
      method,
      headers: {
        "test-user": user,
        Origin: origin,
        "Content-Type": "application/json",
        ...headers,
      },
      body: data === undefined ? undefined : JSON.stringify(data),
    }),
    env,
  );
  return { status: response.status, body: (await response.json()) as any };
}
async function machine(
  path: string,
  data?: unknown,
  headers: Record<string, string> = {},
) {
  const response = await app.fetch(
    new Request(`${origin}/api/generation-runner${path}`, {
      method: "POST",
      headers: {
        Authorization: `Bearer ${runnerToken}`,
        "Content-Type": "application/json",
        ...headers,
      },
      body: JSON.stringify(data ?? {}),
    }),
    env,
  );
  return { status: response.status, body: (await response.json()) as any };
}
async function createWorkspace(topic: string) {
  const created = await call("/admin/lab/workspaces", "POST", { topic });
  expect(created.status).toBe(201);
  return created.body.article_id as string;
}
async function enqueue(article: string, overrides = {}) {
  return call(`/admin/lab/workspaces/${article}/jobs`, "POST", {
    id: crypto.randomUUID(),
    kind: "titles",
    count: 2,
    resolved_prompt: "Ground every claim in the attached evidence.",
    route_id: routes[0].id,
    ...overrides,
  });
}
async function publishVersion(
  article: string,
  body: string,
  thumbnailAssetId?: string,
) {
  return call("/admin/versions", "POST", {
    article_id: article,
    title: "A tested title",
    subtitle: "A tested subtitle",
    body,
    ...(thumbnailAssetId ? { thumbnail_asset_id: thumbnailAssetId } : {}),
  });
}

/** D1 exec needs the repo's newline-collapsed form, but a leading `--`
 * comment would otherwise comment out the entire collapsed script. */
async function applyMigration(sql: string) {
  await env.DB.exec(
    sql
      .split("\n")
      .filter((line) => !line.trimStart().startsWith("--"))
      .join(" "),
  );
}

beforeAll(async () => {
  mf = new Miniflare(
    convertV4MiniflareOptions({
      modules: true,
      script: "export default {fetch(){return new Response()}}",
      d1Databases: ["DB"],
      r2Buckets: ["IMAGES"],
    }),
  );
  env = {
    DB: (await mf.getD1Database("DB")) as unknown as D1Database,
    IMAGES: (await mf.getR2Bucket("IMAGES")) as never,
    BETTER_AUTH_URL: origin,
    BOOTSTRAP_ADMIN_EMAIL: "owner@example.test",
    ASSETS: {} as Fetcher,
    ARTICLE_LAB_ROUTES: JSON.stringify(routes),
    ARTICLE_LAB_RUNNER_TOKEN: runnerToken,
    ARTICLE_LAB_IMAGES: JSON.stringify({ models: ["gpt-image-1"] }),
  };
  for (const file of [
    "migrations/0001_review.sql",
    "migrations/0002_better_auth.sql",
    "migrations/0003_generation_foundation.sql",
    "migrations/0004_image_assets.sql",
  ])
    await applyMigration(await readFile(file, "utf8"));
  for (const email of ["owner@example.test", "jane@example.test"])
    await env.DB.prepare(
      "INSERT INTO auth_user(id,name,email,emailVerified,createdAt,updatedAt) VALUES(?,?,?,1,?,?) ON CONFLICT DO NOTHING",
    )
      .bind(email, email, email, Date.now(), Date.now())
      .run();
});
afterAll(async () => {
  await mf.dispose();
});

describe("admin lab namespace authorization", () => {
  it("rejects anonymous browsers, reviewers and non-approved accounts", async () => {
    // Real anonymous denial runs through Better Auth in auth.test.ts; the
    // synthetic resolver here pins the reviewer/pending/disabled boundaries.
    const article = await createWorkspace("Reviewer boundary test");
    expect(
      (
        await call(
          `/admin/lab/workspaces/${article}`,
          "GET",
          undefined,
          "jane@example.test",
        )
      ).status,
    ).toBe(403);
    await env.DB.prepare("UPDATE users SET status='pending' WHERE email=?")
      .bind("jane@example.test")
      .run();
    expect(
      (
        await call(
          `/admin/lab/workspaces/${article}`,
          "GET",
          undefined,
          "jane@example.test",
        )
      ).status,
    ).toBe(403);
    await env.DB.prepare("UPDATE users SET status='approved' WHERE email=?")
      .bind("jane@example.test")
      .run();
  });

  it("enforces same-origin JSON on mutations inside the namespace", async () => {
    expect(
      (
        await call(
          "/admin/lab/workspaces",
          "POST",
          { topic: "x" },
          "owner@example.test",
          {
            Origin: "https://evil.example",
          },
        )
      ).status,
    ).toBe(403);
    expect(
      (
        await call(
          "/admin/lab/workspaces",
          "POST",
          { topic: "x" },
          "owner@example.test",
          {
            "Content-Type": "text/plain",
          },
        )
      ).status,
    ).toBe(400);
  });

  it("keeps the runner token useless against admin endpoints and browsers out of the machine API", async () => {
    const article = await createWorkspace("Token scope test");
    expect(
      (
        await call(
          `/admin/lab/workspaces/${article}/jobs`,
          "POST",
          {
            id: crypto.randomUUID(),
            kind: "titles",
            resolved_prompt: "x",
            route_id: routes[0].id,
          },
          "owner@example.test",
          { Authorization: `Bearer ${runnerToken}` },
        )
      ).status,
    ).toBe(202);
    const scopedId = (
      (await call(`/admin/lab/workspaces/${article}`)).body.jobs as {
        id: string;
        status: string;
      }[]
    )[0]!.id;
    expect(
      (
        await call(`/admin/lab/workspaces/${article}/jobs`, "PATCH", {
          id: scopedId,
          action: "cancel",
        })
      ).status,
    ).toBe(200);
    const browserOnMachine = await app.fetch(
      new Request(`${origin}/api/generation-runner/claim`, {
        method: "POST",
        headers: {
          "test-user": "owner@example.test",
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          runner_id: "browser",
          route_ids: ["glm-omniroute"],
        }),
      }),
      env,
    );
    expect(browserOnMachine.status).toBe(401);
    expect(
      (
        await machine(
          "/claim",
          { runner_id: "x", route_ids: ["glm-omniroute"] },
          { Origin: origin },
        )
      ).status,
    ).toBe(403);
    expect(
      (
        await machine(
          "/claim",
          { runner_id: "x", route_ids: ["glm-omniroute"] },
          { Authorization: "Bearer nope" },
        )
      ).status,
    ).toBe(401);
  });

  it("disables the machine API and queueing without configuration", async () => {
    const previous = env.ARTICLE_LAB_RUNNER_TOKEN;
    env.ARTICLE_LAB_RUNNER_TOKEN = "";
    expect(
      (
        await machine("/claim", {
          runner_id: "x",
          route_ids: ["glm-omniroute"],
        })
      ).status,
    ).toBe(503);
    const article = await createWorkspace("Disabled runner test");
    expect((await enqueue(article)).status).toBe(503);
    env.ARTICLE_LAB_RUNNER_TOKEN = previous;
  });
});

describe("queue, save and selection semantics over D1", () => {
  it("admits one job per request ID and rejects conflicting reuse", async () => {
    const article = await createWorkspace("Idempotency test");
    const id = crypto.randomUUID();
    expect((await enqueue(article, { id })).status).toBe(202);
    expect((await enqueue(article, { id })).status).toBe(200);
    expect(
      (await enqueue(article, { id, resolved_prompt: "Changed" })).status,
    ).toBe(409);
    const jobs = (await call(`/admin/lab/workspaces/${article}`)).body.jobs;
    expect(jobs).toHaveLength(1);
  });

  it("hands one queued job to exactly one competing claim and hides the lease", async () => {
    const article = await createWorkspace("Claim race test");
    // Drain earlier tests' leftovers so exactly one job is queued for the race.
    for (;;) {
      const drain = await machine("/claim", {
        runner_id: "drain",
        route_ids: ["glm-omniroute"],
      });
      if (!drain.body.job) break;
      await machine(`/jobs/${drain.body.job.id}/fail`, {
        lease_token: drain.body.job.lease_token,
        code: "runner_configuration",
      });
    }
    await enqueue(article);
    const claims = await Promise.all([
      machine("/claim", { runner_id: "a", route_ids: ["glm-omniroute"] }),
      machine("/claim", { runner_id: "b", route_ids: ["glm-omniroute"] }),
    ]);
    expect(claims.filter((claim) => claim.body.job).length).toBe(1);
    const winner = claims.find((claim) => claim.body.job)!.body.job;
    expect(winner.route.response_models).toEqual(["glm-5.3"]);
    expect(
      "lease_token" in
        (await call(`/admin/lab/workspaces/${article}`)).body.jobs[0],
    ).toBe(false);
    expect(winner.lease_token).toBeTruthy();
  });

  it("rejects stale saves with 409 while exact whitespace persists", async () => {
    const article = await createWorkspace("Stale save test");
    const draft = "  Keep\n\nexact whitespace  ";
    expect(
      (
        await call(`/admin/lab/workspaces/${article}`, "PUT", {
          revision: 0,
          topic: "T",
          brief: "",
          evidence: "",
          draft_body: draft,
        })
      ).status,
    ).toBe(200);
    expect(
      (
        await call(`/admin/lab/workspaces/${article}`, "PUT", {
          revision: 0,
          topic: "T",
          brief: "",
          evidence: "",
          draft_body: "stale",
        })
      ).status,
    ).toBe(409);
    expect(
      (await call(`/admin/lab/workspaces/${article}`)).body.workspace
        .draft_body,
    ).toBe(draft);
  });

  it("refuses completions with unqualified models and replays only identical duplicates", async () => {
    const article = await createWorkspace("Completion test");
    await enqueue(article, { count: 2 });
    const job = (
      await machine("/claim", { runner_id: "a", route_ids: ["glm-omniroute"] })
    ).body.job;
    expect(
      (
        await machine(`/jobs/${job.id}/complete`, {
          lease_token: job.lease_token,
          actual_model: "glm-4.7",
          output: '{"candidates":["One","Two"]}',
        })
      ).status,
    ).toBe(409);
    const completion = {
      lease_token: job.lease_token,
      actual_model: "glm-5.3",
      output: '{"candidates":["One","Two"]}',
    };
    expect((await machine(`/jobs/${job.id}/complete`, completion)).status).toBe(
      200,
    );
    expect(
      (await machine(`/jobs/${job.id}/complete`, completion)).body.duplicate,
    ).toBe(true);
    expect(
      (
        await machine(`/jobs/${job.id}/complete`, {
          lease_token: job.lease_token,
          actual_model: "glm-5.3",
          output: '{"candidates":["Different","Out"]}',
        })
      ).status,
    ).toBe(409);
    const view = (await call(`/admin/lab/workspaces/${article}`)).body;
    expect(
      view.candidates.map((c: { value: string }) => c.value).sort(),
    ).toEqual(["One", "Two"]);
  });

  it("marks expired leases uncertain instead of requeueing them", async () => {
    const article = await createWorkspace("Lease expiry test");
    await enqueue(article);
    const job = (
      await machine("/claim", { runner_id: "a", route_ids: ["glm-omniroute"] })
    ).body.job;
    await env.DB.prepare(
      "UPDATE generation_jobs SET lease_expires_at='2000-01-01T00:00:00.000Z' WHERE id=?",
    )
      .bind(job.id)
      .run();
    await machine("/claim", { runner_id: "a", route_ids: ["glm-omniroute"] });
    const row = await env.DB.prepare(
      "SELECT status,error_code FROM generation_jobs WHERE id=?",
    )
      .bind(job.id)
      .first<{ status: string; error_code: string }>();
    expect(row!.status).toBe("uncertain");
    expect(row!.error_code).toBe("lease_expired");
    expect(
      (
        await machine(`/jobs/${job.id}/complete`, {
          lease_token: job.lease_token,
          actual_model: "glm-5.3",
          output: '{"candidates":["Too late","Nope"]}',
        })
      ).status,
    ).toBe(409);
  });

  it("only cancels queued jobs", async () => {
    const article = await createWorkspace("Cancellation test");
    const job = crypto.randomUUID();
    await enqueue(article, { id: job });
    expect(
      (
        await call(`/admin/lab/workspaces/${article}/jobs`, "PATCH", {
          id: job,
          action: "cancel",
        })
      ).status,
    ).toBe(200);
    await enqueue(article);
    const running = (
      await machine("/claim", { runner_id: "a", route_ids: ["glm-omniroute"] })
    ).body.job;
    expect(
      (
        await call(`/admin/lab/workspaces/${article}/jobs`, "PATCH", {
          id: running.id,
          action: "cancel",
        })
      ).status,
    ).toBe(409);
  });

  it("exposes heartbeat availability separately from token existence", async () => {
    const before = (await call("/admin/lab/routes")).body;
    expect(before.runner_configured).toBe(true);
    expect(before.runners).toEqual([]);
    await machine("/heartbeat", {
      runner_id: "rtx-article-lab",
      route_ids: ["glm-omniroute"],
      image_ready: true,
    });
    const after = (await call("/admin/lab/routes")).body;
    expect(after.runners).toEqual([
      {
        runner_id: "rtx-article-lab",
        route_ids: ["glm-omniroute"],
        image_ready: true,
        last_seen: expect.any(String),
      },
    ]);
  });
});

describe("image jobs, private assets and version authorization", () => {
  it("serves assets only to admins or reviewers assigned to the freezing version", async () => {
    const article = await createWorkspace("Asset flow test");
    const upload = await app.fetch(
      new Request(
        `${origin}/api/admin/lab/workspaces/${article}/image-upload`,
        {
          method: "POST",
          headers: { "test-user": "owner@example.test", Origin: origin },
          body: (() => {
            const form = new FormData();
            form.append(
              "file",
              new File([png()], "cover.png", { type: "image/png" }),
            );
            form.append("alt_text", "Indexed funds line chart");
            form.append("caption", "Growth over ten years");
            return form;
          })(),
        },
      ),
      env,
    );
    expect(upload.status).toBe(201);
    const assetId = ((await upload.json()) as any).image_asset_id as string;

    const markdown = `Intro paragraph.\n\n![Indexed funds line chart](/api/assets/${assetId})\n\nClosing thought.`;
    const published = await publishVersion(article, markdown);
    expect(published.status).toBe(201);
    const versionId = published.body.id as string;

    const frozen = await env.DB.prepare(
      "SELECT rendered_html FROM article_versions WHERE id=?",
    )
      .bind(versionId)
      .first<{ rendered_html: string }>();
    expect(frozen!.rendered_html).toContain(`src="/api/assets/${assetId}"`);
    expect(frozen!.rendered_html).toContain(`alt="Indexed funds line chart"`);

    const adminBytes = await app.fetch(
      new Request(`${origin}/api/assets/${assetId}`, {
        headers: { "test-user": "owner@example.test" },
      }),
      env,
    );
    expect(adminBytes.status).toBe(200);
    expect(adminBytes.headers.get("Content-Type")).toBe("image/png");
    expect(new Uint8Array(await adminBytes.arrayBuffer())[0]).toBe(0x89);

    const unassigned = await app.fetch(
      new Request(`${origin}/api/assets/${assetId}`, {
        headers: { "test-user": "jane@example.test" },
      }),
      env,
    );
    expect(unassigned.status).toBe(403);

    const jane = await env.DB.prepare(
      "SELECT id FROM users WHERE email='jane@example.test'",
    ).first<{ id: string }>();
    await call("/admin/assignments", "POST", {
      version_id: versionId,
      user_id: jane!.id,
    });
    const assigned = await app.fetch(
      new Request(`${origin}/api/assets/${assetId}`, {
        headers: { "test-user": "jane@example.test" },
      }),
      env,
    );
    expect(assigned.status).toBe(200);
  });

  it("rejects publishing references to missing, archived or foreign assets", async () => {
    const article = await createWorkspace("Foreign asset test");
    const other = await createWorkspace("Other article");
    const upload = await app.fetch(
      new Request(`${origin}/api/admin/lab/workspaces/${other}/image-upload`, {
        method: "POST",
        headers: { "test-user": "owner@example.test", Origin: origin },
        body: (() => {
          const form = new FormData();
          form.append(
            "file",
            new File([png()], "other.png", { type: "image/png" }),
          );
          return form;
        })(),
      }),
      env,
    );
    const foreignId = ((await upload.json()) as any).image_asset_id as string;
    expect(
      (await publishVersion(article, `![steal](/api/assets/${foreignId})`))
        .status,
    ).toBe(400);
    expect(
      (
        await publishVersion(
          article,
          "![missing](/api/assets/11111111-2222-4333-8444-555555555555)",
        )
      ).status,
    ).toBe(400);
  });

  it("rejects non-image uploads and accepts webp/jpeg magic numbers only", async () => {
    const article = await createWorkspace("Upload validation test");
    const reject = await app.fetch(
      new Request(
        `${origin}/api/admin/lab/workspaces/${article}/image-upload`,
        {
          method: "POST",
          headers: { "test-user": "owner@example.test", Origin: origin },
          body: (() => {
            const form = new FormData();
            form.append(
              "file",
              new File(
                [new TextEncoder().encode("<svg xmlns='x'></svg>")],
                "evil.svg",
                { type: "image/svg+xml" },
              ),
            );
            return form;
          })(),
        },
      ),
      env,
    );
    expect(reject.status).toBe(400);
    const jpeg = new Uint8Array(new ArrayBuffer(600));
    // SOI + SOF0 segment: length 17, precision 8, height 400, width 640.
    jpeg.set([0xff, 0xd8, 0xff, 0xc0, 0x00, 0x11, 0x08], 0);
    new DataView(jpeg.buffer).setUint16(7, 400);
    new DataView(jpeg.buffer).setUint16(9, 640);
    const accept = await app.fetch(
      new Request(
        `${origin}/api/admin/lab/workspaces/${article}/image-upload`,
        {
          method: "POST",
          headers: { "test-user": "owner@example.test", Origin: origin },
          body: (() => {
            const form = new FormData();
            form.append(
              "file",
              new File([jpeg], "photo.jpg", { type: "image/jpeg" }),
            );
            return form;
          })(),
        },
      ),
      env,
    );
    expect(accept.status).toBe(201);
  });

  it("runs image jobs end to end from enqueue to stored asset with lease safety", async () => {
    const article = await createWorkspace("Image job test");
    const jobId = crypto.randomUUID();
    expect(
      (
        await call(`/admin/lab/workspaces/${article}/image-jobs`, "POST", {
          id: jobId,
          prompt: "Flat illustration of a diversified portfolio",
          model: "gpt-image-1",
          size: "1536x1024",
          quality: "medium",
        })
      ).status,
    ).toBe(202);
    expect(
      (
        await call(`/admin/lab/workspaces/${article}/image-jobs`, "POST", {
          id: jobId,
          prompt: "Different prompt",
          model: "gpt-image-1",
        })
      ).status,
    ).toBe(409);
    expect(
      (
        await call(`/admin/lab/workspaces/${article}/image-jobs`, "POST", {
          id: crypto.randomUUID(),
          prompt: "x",
          model: "dall-e-3",
        })
      ).status,
    ).toBe(400);
    const claimed = await machine("/images/claim", { runner_id: "a" });
    expect(claimed.body.job.id).toBe(jobId);
    expect(claimed.body.job.model).toBe("gpt-image-1");
    const encoded = btoa(String.fromCharCode(...png(1536, 1024)));
    const done = await machine(`/images/jobs/${jobId}/complete`, {
      lease_token: claimed.body.job.lease_token,
      image_base64: encoded,
    });
    expect(done.status).toBe(200);
    expect(done.body.image_asset_id).toBeTruthy();
    const view = (await call(`/admin/lab/workspaces/${article}`)).body;
    expect(view.image_jobs[0].status).toBe("succeeded");
    expect(view.image_jobs[0].asset_id).toBe(done.body.image_asset_id);
    const asset = view.image_assets[0];
    expect(asset.source).toBe("generated");
    expect(asset.width).toBe(1536);
    expect(asset.model).toBe("gpt-image-1");
  });

  it("cancels queued image jobs but not running ones", async () => {
    const article = await createWorkspace("Image cancel test");
    const first = crypto.randomUUID();
    await call(`/admin/lab/workspaces/${article}/image-jobs`, "POST", {
      id: first,
      prompt: "queued",
      model: "gpt-image-1",
    });
    expect(
      (
        await call(`/admin/lab/workspaces/${article}/image-jobs`, "PATCH", {
          id: first,
          action: "cancel",
        })
      ).status,
    ).toBe(200);
    const second = crypto.randomUUID();
    await call(`/admin/lab/workspaces/${article}/image-jobs`, "POST", {
      id: second,
      prompt: "running",
      model: "gpt-image-1",
    });
    const claimed = await machine("/images/claim", { runner_id: "a" });
    expect(
      (
        await call(`/admin/lab/workspaces/${article}/image-jobs`, "PATCH", {
          id: claimed.body.job.id,
          action: "cancel",
        })
      ).status,
    ).toBe(409);
  });
});

describe("published version freezing", () => {
  it("creates new immutable versions per publish and keeps old reviewer anchors", async () => {
    const article = await createWorkspace("Versioning test");
    const first = await publishVersion(article, "# First\n\nOriginal body.");
    expect(first.status).toBe(201);
    const second = await publishVersion(article, "# Second\n\nChanged body.");
    expect(second.status).toBe(201);
    expect(second.body.id).not.toBe(first.body.id);
    const numbers = await env.DB.prepare(
      "SELECT version_number FROM article_versions WHERE article_id=? ORDER BY version_number",
    )
      .bind(article)
      .all();
    expect(numbers.results.map((row) => row.version_number)).toEqual([1, 2]);
  });
});
