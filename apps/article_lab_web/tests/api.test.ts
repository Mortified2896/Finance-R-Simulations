import { beforeAll, afterAll, describe, it, expect } from "vitest";
import { Miniflare, convertV4MiniflareOptions } from "miniflare";
import { readFile } from "node:fs/promises";
import { createApp } from "../worker/index";
import type { Env } from "../worker/index";
import { renderMarkdown } from "../shared/markdown";
let mf: Miniflare, env: Env;
const app = createApp(async (req) => {
  const email = req.headers.get("test-user") ?? "jane@example.test";
  return {
    email: email.toLowerCase(),
    name: email,
    subject: req.headers.get("test-sub") ?? email,
    issuer: "test",
  };
});
async function call(
  path: string,
  method = "GET",
  data?: unknown,
  user = "owner@example.test",
  headers: Record<string, string> = {},
) {
  const response = await app.fetch(
    new Request(`https://review.test/api${path}`, {
      method,
      headers: {
        "test-user": user,
        Origin: "https://review.test",
        "Content-Type": "application/json",
        ...headers,
      },
      body: data === undefined ? undefined : JSON.stringify(data),
    }),
    env,
  );
  return { status: response.status, body: (await response.json()) as any };
}
beforeAll(async () => {
  mf = new Miniflare(
    convertV4MiniflareOptions({
      modules: true,
      script: "export default {fetch(){return new Response()}}",
      d1Databases: ["DB"],
    }),
  );
  env = {
    DB: (await mf.getD1Database("DB")) as unknown as D1Database,
    BOOTSTRAP_ADMIN_EMAIL: "owner@example.test",
    ASSETS: {} as Fetcher,
  };
  await env.DB.exec(
    (await readFile("migrations/0001_review.sql", "utf8")).replace(/\n/g, " "),
  );
});
afterAll(async () => {
  await mf.dispose();
});
describe("D1 review lifecycle and authorization", () => {
  let jane: string,
    paul: string,
    version: string,
    assignment: string,
    revision = 0;
  it("unknown reviewer is pending; cannot see article or admin data", async () => {
    const r = await call("/me", "GET", undefined, "jane@example.test");
    jane = r.body.user.id;
    expect(r.body.user.status).toBe("pending");
    expect(
      (await call("/assignments", "GET", undefined, "jane@example.test"))
        .status,
    ).toBe(403);
    expect(
      (await call("/admin/users", "GET", undefined, "jane@example.test"))
        .status,
    ).toBe(403);
  });
  it("bootstraps only explicit owner; approval survives alternate login subject", async () => {
    expect((await call("/me")).body.user.role).toBe("admin");
    expect(
      (await call(`/admin/users/${jane}`, "PATCH", { status: "approved" }))
        .status,
    ).toBe(200);
    const r = await call("/me", "GET", undefined, "JANE@example.test", {
      "test-sub": "google-identity",
    });
    expect(r.body.user.id).toBe(jane);
    expect(r.body.user.status).toBe("approved");
    expect(
      (await call("/admin/users", "GET", undefined, "jane@example.test"))
        .status,
    ).toBe(403);
  });
  it("rejected identity is never recreated or automatically approved", async () => {
    paul = (await call("/me", "GET", undefined, "paul@example.test")).body.user
      .id;
    await call(`/admin/users/${paul}`, "PATCH", { status: "rejected" });
    const r = await call("/me", "GET", undefined, "paul@example.test");
    expect(r.body.user.id).toBe(paul);
    expect(r.body.user.status).toBe("rejected");
  });
  it("creates safe immutable Markdown versions", async () => {
    const r = await call("/admin/versions", "POST", {
      title: "A better article",
      body: "Hello **reviewer**. A useful passage.\n\n<script>alert(1)</script>\n\n[bad](javascript:alert(1))",
    });
    expect(r.status).toBe(201);
    version = r.body.id;
    const stored = await env.DB.prepare(
      "SELECT * FROM article_versions WHERE id=?",
    )
      .bind(version)
      .first<any>();
    expect(stored.rendered_html).not.toContain("<script");
    expect(stored.rendered_html).not.toContain("javascript:");
    await expect(
      env.DB.prepare("UPDATE article_versions SET title=? WHERE id=?")
        .bind("changed", version)
        .run(),
    ).rejects.toThrow("immutable");
    const next = await call("/admin/versions", "POST", {
      article_id: r.body.article_id,
      title: "Next version",
      body: "Changed text",
    });
    expect(next.status).toBe(201);
    expect(next.body.id).not.toBe(version);
  });
  it("filters assignments; prevents ID manipulation", async () => {
    await call("/admin/assignments", "POST", {
      version_id: version,
      user_id: jane,
    });
    const r = await call("/assignments", "GET", undefined, "jane@example.test");
    expect(r.body).toHaveLength(1);
    assignment = r.body[0].id;
    expect(r.body[0].status).toBe("not started");
    expect((await call("/assignments")).body).toHaveLength(0);
    expect(
      (
        await call(
          `/reviews/${assignment}/open`,
          "POST",
          {},
          "owner@example.test",
        )
      ).status,
    ).toBe(404);
    await call(`/admin/users/${paul}`, "PATCH", { status: "approved" });
    expect(
      (
        await call(
          `/reviews/${assignment}/open`,
          "POST",
          {},
          "paul@example.test",
        )
      ).status,
    ).toBe(404);
    expect(
      (await call(`/reviews/${assignment}`, "PUT", {}, "paul@example.test"))
        .status,
    ).toBe(404);
  });
  let annotation: any;
  it("opens draft tied to exact version and saves validated annotations", async () => {
    const r = await call(
      `/reviews/${assignment}/open`,
      "POST",
      {},
      "jane@example.test",
    );
    expect(r.body.version.id).toBe(version);
    expect(r.body.review.status).toBe("draft");
    const text = r.body.version.anchor_text;
    annotation = {
      id: crypto.randomUUID(),
      exact_quote: text.slice(6, 14),
      start_offset: 6,
      end_offset: 14,
      prefix: text.slice(0, 6),
      suffix: text.slice(14, 78),
      comment: "Please clarify.",
    };
    const result = await call(
      `/reviews/${assignment}`,
      "PUT",
      {
        revision,
        mutation_id: crypto.randomUUID(),
        general_feedback: "Useful overall.",
        annotations: [annotation],
      },
      "jane@example.test",
    );
    expect(result.status).toBe(200);
    revision = result.body.revision;
    const restored = await call(
      `/reviews/${assignment}/open`,
      "POST",
      {},
      "jane@example.test",
    );
    expect(restored.body.review.annotations[0].comment).toBe("Please clarify.");
    expect(restored.body.review.general_feedback).toBe("Useful overall.");
  });
  it("rejects invalid anchors and foreign origins", async () => {
    expect(
      (
        await call(
          `/reviews/${assignment}`,
          "PUT",
          {
            revision,
            mutation_id: crypto.randomUUID(),
            general_feedback: "",
            annotations: [{ ...annotation, exact_quote: "wrong" }],
          },
          "jane@example.test",
        )
      ).status,
    ).toBe(400);
    expect(
      (
        await call(
          `/reviews/${assignment}/submit`,
          "POST",
          { revision },
          "jane@example.test",
          { Origin: "https://evil.test" },
        )
      ).status,
    ).toBe(403);
  });
  it("atomic compare-and-swap lets only one competing save win; retry is idempotent", async () => {
    const payload = {
      revision,
      mutation_id: crypto.randomUUID(),
      general_feedback: "Winner A",
      annotations: [{ ...annotation, comment: "Edited" }],
    };
    const responses = await Promise.all([
      call(`/reviews/${assignment}`, "PUT", payload, "jane@example.test"),
      call(
        `/reviews/${assignment}`,
        "PUT",
        {
          ...payload,
          mutation_id: crypto.randomUUID(),
          general_feedback: "Winner B",
        },
        "jane@example.test",
      ),
    ]);
    expect(responses.map((r) => r.status).sort()).toEqual([200, 409]);
    revision++;
    if (responses[0].status === 200)
      expect(
        (
          await call(
            `/reviews/${assignment}`,
            "PUT",
            payload,
            "jane@example.test",
          )
        ).status,
      ).toBe(200);
    const restored = await call(
      `/reviews/${assignment}/open`,
      "POST",
      {},
      "jane@example.test",
    );
    expect(restored.body.review.annotations).toHaveLength(1);
    expect(restored.body.review.annotations[0].comment).toBe("Edited");
  });
  it("deletes annotation and preserves general feedback", async () => {
    const r = await call(
      `/reviews/${assignment}`,
      "PUT",
      {
        revision,
        mutation_id: crypto.randomUUID(),
        general_feedback: "Final thoughts",
        annotations: [],
      },
      "jane@example.test",
    );
    expect(r.status).toBe(200);
    revision++;
    expect(
      (
        await call(
          `/reviews/${assignment}/open`,
          "POST",
          {},
          "jane@example.test",
        )
      ).body.review.annotations,
    ).toHaveLength(0);
  });
  it("rejects stale submission; submits once and locks all edits", async () => {
    expect(
      (
        await call(
          `/reviews/${assignment}/submit`,
          "POST",
          { revision: 0 },
          "jane@example.test",
        )
      ).status,
    ).toBe(409);
    const results = await Promise.all([
      call(
        `/reviews/${assignment}/submit`,
        "POST",
        { revision },
        "jane@example.test",
      ),
      call(
        `/reviews/${assignment}/submit`,
        "POST",
        { revision },
        "jane@example.test",
      ),
    ]);
    expect(results.map((r) => r.status)).toEqual([200, 200]);
    expect(results[0].body.submitted_at).toBe(results[1].body.submitted_at);
    expect(
      (
        await call(
          `/reviews/${assignment}`,
          "PUT",
          {
            revision,
            mutation_id: crypto.randomUUID(),
            general_feedback: "Oops",
            annotations: [],
          },
          "jane@example.test",
        )
      ).status,
    ).toBe(409);
    const feedback = await call(`/admin/reviews/${assignment}`);
    expect(feedback.body.review.general_feedback).toBe("Final thoughts");
    expect(feedback.body.version.id).toBe(version);
    expect(
      (
        await call(
          `/admin/reviews/${assignment}`,
          "GET",
          undefined,
          "paul@example.test",
        )
      ).status,
    ).toBe(403);
  });
  it("disabling immediately removes article access and does not reset on login", async () => {
    await call(`/admin/users/${jane}`, "PATCH", { status: "disabled" });
    expect(
      (await call("/assignments", "GET", undefined, "jane@example.test"))
        .status,
    ).toBe(403);
    expect(
      (await call("/me", "GET", undefined, "jane@example.test")).body.user
        .status,
    ).toBe("disabled");
  });
  it("production entry has no fake-auth configuration or header bypass", async () => {
    const prod = createApp();
    let r = await prod.fetch(
      new Request("https://review.test/api/me", {
        headers: {
          "test-user": "owner@example.test",
          "Cf-Access-Authenticated-User-Email": "owner@example.test",
        },
      }),
      env,
    );
    expect(r.status).toBe(503);
    r = await prod.fetch(new Request("https://review.test/api/me"), {
      ...env,
      ACCESS_TEAM_DOMAIN: "https://example.cloudflareaccess.com",
      ACCESS_AUD: "aud",
    });
    expect(r.status).toBe(401);
  });
  it("sanitizes raw HTML, unsafe links, images and preserves exact text", () => {
    const r = renderMarkdown(
      "A **bold** word.\n\n![x](https://remote.test/a.png)\n\n<script>evil()</script>",
    );
    expect(r.anchor_text).toContain("A bold word.");
    expect(r.rendered_html).not.toMatch(/<img|<script/);
  });
});
