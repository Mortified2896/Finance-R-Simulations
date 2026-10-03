import { Hono } from "hono";
import { HTTPException } from "hono/http-exception";
import { z } from "zod";
import { createAuth, authReady, type AuthEnv, type Identity } from "./auth";
import {
  handleLabRequest,
  handleRunnerRequest,
  serveAsset,
  type ObjectStore,
} from "./generation";
import { renderMarkdown } from "../shared/markdown";
import { uuid } from "../shared/generation";
import type { User, ArticleVersion, Review, Annotation } from "../shared/types";
export type Env = AuthEnv & {
  ASSETS: Fetcher;
  BOOTSTRAP_ADMIN_EMAIL?: string;
  ARTICLE_LAB_ROUTES?: string;
  ARTICLE_LAB_RUNNER_TOKEN?: string;
  ARTICLE_LAB_IMAGES?: string;
  IMAGES?: ObjectStore;
};
type AppEnv = { Bindings: Env; Variables: { user: User } };
export function createApp(
  resolveIdentity?: (request: Request) => Promise<Identity>,
) {
  const app = new Hono<AppEnv>();
  const now = () => new Date().toISOString();
  const fail = (
    status: 400 | 401 | 403 | 404 | 409 | 413 | 503,
    message: string,
  ): never => {
    throw new HTTPException(status, { message });
  };
  const stmt = (db: D1Database, sql: string, ...args: unknown[]) =>
    db.prepare(sql).bind(...args);
  app.use("*", async (c, next) => {
    const url = new URL(c.req.url);
    if (url.hostname.endsWith(".workers.dev") && c.env.BETTER_AUTH_URL) {
      return c.redirect(
        new URL(url.pathname + url.search, c.env.BETTER_AUTH_URL).href,
        308,
      );
    }
    await next();
    let styleSources = "'self'";
    if (c.res.headers.get("Content-Type")?.startsWith("text/html")) {
      // Radix's scroll lock creates a dynamic stylesheet. Authorize it with a
      // fresh CSS-only nonce; scripts still require a same-origin external file.
      const nonce = btoa(
        String.fromCharCode(...crypto.getRandomValues(new Uint8Array(24))),
      );
      styleSources += ` 'nonce-${nonce}'`;
      const headers = new Headers(c.res.headers);
      headers.delete("ETag");
      headers.delete("Content-Length");
      headers.set("Cache-Control", "no-store");
      c.res = new Response(
        (await c.res.text()).replace(
          "<head>",
          `<head><meta name="article-style-nonce" content="${nonce}">`,
        ),
        { status: c.res.status, headers },
      );
      // Hono merges the original response headers when replacing c.res.
      c.header("ETag", undefined);
      c.header("Content-Length", undefined);
      c.header("Cache-Control", "no-store");
    }
    c.header("X-Content-Type-Options", "nosniff");
    c.header("Referrer-Policy", "no-referrer");
    c.header("X-Frame-Options", "DENY");
    c.header("Permissions-Policy", "camera=(), microphone=(), geolocation=()");
    c.header(
      "Content-Security-Policy",
      // Radix Select's fixed viewport stylesheet has no nonce API in MDXEditor.
      // Permit only its exact bytes, rather than arbitrary inline stylesheets.
      `default-src 'self'; script-src 'self'; style-src 'self'; style-src-elem ${styleSources} 'sha256-441zG27rExd4/il+NvIqyL8zFx5XmyNQtE381kSkUJk='; img-src 'self'; connect-src 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'; form-action 'self'`,
    );
    c.header("Strict-Transport-Security", "max-age=31536000");
    if (c.req.path.startsWith("/api/")) c.header("Cache-Control", "no-store");
  });
  app.onError((err, c) =>
    c.json(
      {
        error:
          err instanceof HTTPException
            ? err.message
            : "The request failed. Please retry; if it persists, contact the administrator.",
      },
      err instanceof HTTPException ? err.status : 500,
    ),
  );
  app.get("/api/auth-config", (c) => c.json({ ready: authReady(c.env) }));
  // Auth owns its callback/state validation; mount before application approval.
  app.on(["GET", "POST"], "/api/auth/*", async (c) => {
    if (!authReady(c.env))
      return c.json({ error: "Google sign-in setup is not complete." }, 503);
    const response = await createAuth(c.env).handler(c.req.raw);
    if (response.status >= 500)
      return c.json(
        { error: "Sign-in failed. Please retry or contact the administrator." },
        500,
      );
    return response;
  });
  // Dedicated machine authentication; this token never grants browser/admin access.
  app.all("/api/generation-runner/*", (c) =>
    handleRunnerRequest(c.req.raw, c.env),
  );
  app.use("/api/*", async (c, next) => {
    if (!["GET", "HEAD"].includes(c.req.method)) {
      if (c.req.header("Origin") !== new URL(c.req.url).origin)
        fail(403, "Request origin rejected.");
      // Only image uploads are multipart; the same-origin check above still applies.
      const upload =
        c.req.method === "POST" &&
        /^\/api\/admin\/lab\/workspaces\/[0-9a-f-]+\/image-upload$/.test(
          c.req.path,
        );
      if (
        !upload &&
        !c.req.header("Content-Type")?.startsWith("application/json")
      )
        fail(400, "JSON request required.");
    }
    let identity: Identity;
    if (resolveIdentity) identity = await resolveIdentity(c.req.raw);
    else {
      if (!authReady(c.env))
        return c.json({ error: "Google sign-in setup is not complete." }, 503);
      const sessionResponse = await createAuth(c.env).api.getSession({
        headers: c.req.raw.headers,
        asResponse: true,
      });
      // Preserve normal Better Auth refresh cookies on our protected API calls.
      for (const cookie of sessionResponse.headers.getSetCookie())
        c.header("Set-Cookie", cookie, { append: true });
      if (!sessionResponse.ok)
        return c.json({ error: "Sign-in could not be verified." }, 401);
      const session = (await sessionResponse.json()) as {
        user?: {
          id: string;
          email: string;
          emailVerified: boolean;
          name: string;
        };
      } | null;
      if (!session?.user?.emailVerified)
        return c.json({ error: "Sign in with Google to continue." }, 401);
      identity = {
        authId: session.user.id,
        email: session.user.email.trim().toLowerCase(),
        name: session.user.name,
      };
    }
    const db = c.env.DB,
      t = now();
    // Bootstrap only affects creation, never overrides a revoked account on login.
    const bootstrap =
      identity.email === c.env.BOOTSTRAP_ADMIN_EMAIL?.trim().toLowerCase();
    await stmt(
      db,
      `INSERT INTO users(id,auth_user_id,email,display_name,status,role,created_at,approved_at,last_login_at) VALUES(?,?,?,?,?,?,?,?,?) ON CONFLICT(auth_user_id) DO UPDATE SET email=excluded.email,display_name=excluded.display_name,last_login_at=excluded.last_login_at ON CONFLICT(email) DO UPDATE SET auth_user_id=COALESCE(users.auth_user_id,excluded.auth_user_id),last_login_at=excluded.last_login_at WHERE users.auth_user_id IS NULL OR users.auth_user_id=excluded.auth_user_id`,
      crypto.randomUUID(),
      identity.authId,
      identity.email,
      identity.name,
      bootstrap ? "approved" : "pending",
      bootstrap ? "admin" : "reviewer",
      t,
      bootstrap ? t : null,
      t,
    ).run();
    const user = await stmt(
      db,
      "SELECT * FROM users WHERE auth_user_id=?",
      identity.authId,
    ).first<User>();
    if (!user)
      return c.json(
        {
          error:
            "Account identity could not be linked. Contact the administrator.",
        },
        403,
      );
    c.set("user", user!);
    await next();
  });
  app.get("/api/me", (c) => c.json({ user: c.get("user") }));
  app.use("/api/*", async (c, next) => {
    if (c.get("user").status !== "approved")
      fail(403, "Your account is not approved.");
    await next();
  });
  // Session-authorized image bytes: admin, or reviewer assigned to the exact
  // immutable version that froze this asset. No public or guessable URL access.
  app.get("/api/assets/:id", (c) =>
    serveAsset(c.req.raw, c.env, c.get("user")),
  );
  app.use("/api/admin/*", async (c, next) => {
    if (c.get("user").role !== "admin")
      fail(403, "Administrator access required.");
    await next();
  });
  app.all("/api/admin/lab/*", (c) =>
    handleLabRequest(c.req.raw, c.env, c.get("user")),
  );
  async function body<T>(
    c: { req: { raw: Request } },
    schema: z.ZodType<T>,
  ): Promise<T> {
    const reader = c.req.raw.body?.getReader();
    if (!reader) return fail(400, "Request body required.");
    let size = 0;
    const chunks: Uint8Array[] = [];
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      size += value.byteLength;
      if (size > 500_000) {
        await reader.cancel();
        return fail(413, "Request is too large.");
      }
      chunks.push(value);
    }
    const bytes = new Uint8Array(size);
    let offset = 0;
    for (const chunk of chunks) {
      bytes.set(chunk, offset);
      offset += chunk.length;
    }
    let value;
    try {
      value = JSON.parse(new TextDecoder().decode(bytes));
    } catch {
      return fail(400, "Invalid JSON.");
    }
    const parsed = schema.safeParse(value);
    if (!parsed.success)
      return fail(
        400,
        parsed.error.issues
          .map((i) => `${i.path.join(".")}: ${i.message}`)
          .join("; "),
      );
    return parsed.data;
  }
  app.get("/api/assignments", async (c) => {
    const rows = await stmt(
      c.env.DB,
      `SELECT a.id,a.version_id,v.title,v.subtitle,v.version_number,CASE WHEN r.status='submitted' THEN 'submitted' WHEN r.id IS NOT NULL THEN 'in progress' ELSE 'not started' END status FROM review_assignments a JOIN article_versions v ON v.id=a.version_id LEFT JOIN reviews r ON r.id=a.id WHERE a.user_id=? ORDER BY a.created_at DESC`,
      c.get("user").id,
    ).all();
    return c.json(rows.results);
  });
  async function owned(db: D1Database, id: string, user: User) {
    const a = await stmt(
      db,
      "SELECT * FROM review_assignments WHERE id=? AND user_id=?",
      id,
      user.id,
    ).first<{ version_id: string }>();
    if (!a) return fail(404, "Review not found.");
    return a;
  }
  async function detail(db: D1Database, id: string, versionId: string) {
    const [version, review, annotations] = await Promise.all([
      stmt(
        db,
        "SELECT * FROM article_versions WHERE id=?",
        versionId,
      ).first<ArticleVersion>(),
      stmt(db, "SELECT * FROM reviews WHERE id=?", id).first<Review>(),
      stmt(
        db,
        "SELECT * FROM annotations WHERE review_id=? ORDER BY start_offset,created_at",
        id,
      ).all<Annotation>(),
    ]);
    return {
      version,
      review: review ? { ...review, annotations: annotations.results } : null,
    };
  }
  app.post("/api/reviews/:id/open", async (c) => {
    const id = c.req.param("id"),
      a = await owned(c.env.DB, id, c.get("user")),
      t = now();
    await stmt(
      c.env.DB,
      "INSERT INTO reviews(id,created_at,updated_at) VALUES(?,?,?) ON CONFLICT DO NOTHING",
      id,
      t,
      t,
    ).run();
    return c.json(await detail(c.env.DB, id, a.version_id));
  });
  const annotationSchema = z.object({
    id: z.string().uuid(),
    exact_quote: z.string().min(1).max(10000),
    start_offset: z.number().int().nonnegative(),
    end_offset: z.number().int().positive(),
    prefix: z.string().max(64),
    suffix: z.string().max(64),
    comment: z.string().max(10000),
  });
  const saveSchema = z.object({
    revision: z.number().int().nonnegative(),
    mutation_id: z.string().uuid(),
    general_feedback: z.string().max(30000),
    annotations: z.array(annotationSchema).max(200),
  });
  app.put("/api/reviews/:id", async (c) => {
    const db = c.env.DB,
      id = c.req.param("id"),
      a = await owned(db, id, c.get("user")),
      input = await body(c, saveSchema);
    const version = await stmt(
      db,
      "SELECT anchor_text FROM article_versions WHERE id=?",
      a.version_id,
    ).first<{ anchor_text: string }>();
    if (
      new Set(input.annotations.map((a) => a.id)).size !==
      input.annotations.length
    )
      fail(400, "Duplicate annotation IDs.");
    for (const x of input.annotations) {
      const text = version!.anchor_text;
      if (
        x.end_offset <= x.start_offset ||
        text.slice(x.start_offset, x.end_offset) !== x.exact_quote ||
        text.slice(Math.max(0, x.start_offset - 64), x.start_offset) !==
          x.prefix ||
        text.slice(x.end_offset, x.end_offset + 64) !== x.suffix
      )
        fail(400, "The selected passage does not match this article version.");
    }
    const old = await stmt(db, "SELECT * FROM reviews WHERE id=?", id).first<
      Review & { last_mutation: string }
    >();
    if (!old) return fail(404, "Open this review before saving.");
    if (old.last_mutation === input.mutation_id)
      return c.json({ revision: old.revision });
    if (old.status === "submitted")
      return fail(409, "This review has been submitted and is read-only.");
    const t = now(),
      next = input.revision + 1;
    // D1 batch is transactional. Every annotation write is guarded by the CAS
    // mutation token. A losing concurrent writer cannot delete winning comments.
    const guard =
      "EXISTS(SELECT 1 FROM reviews WHERE id=? AND revision=? AND last_mutation=? AND status='draft')";
    const statements = [
      stmt(
        db,
        `UPDATE reviews SET general_feedback=?,revision=revision+1,last_mutation=?,updated_at=? WHERE id=? AND revision=? AND status='draft'`,
        input.general_feedback,
        input.mutation_id,
        t,
        id,
        input.revision,
      ),
    ];
    const prior = await stmt(
      db,
      "SELECT id,created_at FROM annotations WHERE review_id=?",
      id,
    ).all<{ id: string; created_at: string }>();
    statements.push(
      stmt(
        db,
        `DELETE FROM annotations WHERE review_id=? AND ${guard}`,
        id,
        id,
        next,
        input.mutation_id,
      ),
    );
    for (const x of input.annotations)
      statements.push(
        stmt(
          db,
          `INSERT INTO annotations(id,review_id,version_id,exact_quote,start_offset,end_offset,prefix,suffix,comment,created_at,updated_at) SELECT ?,?,?,?,?,?,?,?,?,?,? WHERE ${guard}`,
          x.id,
          id,
          a.version_id,
          x.exact_quote,
          x.start_offset,
          x.end_offset,
          x.prefix,
          x.suffix,
          x.comment,
          prior.results.find((p) => p.id === x.id)?.created_at ?? t,
          t,
          id,
          next,
          input.mutation_id,
        ),
      );
    const results = await db.batch(statements);
    if (!results[0].meta.changes)
      return fail(
        409,
        "This draft changed in another tab or device. Reload before editing; your unsaved text is still shown here.",
      );
    return c.json({ revision: next });
  });
  app.post("/api/reviews/:id/submit", async (c) => {
    const db = c.env.DB,
      id = c.req.param("id");
    await owned(db, id, c.get("user"));
    const input = await body(
        c,
        z.object({ revision: z.number().int().nonnegative() }),
      ),
      t = now();
    const result = await stmt(
      db,
      `UPDATE reviews SET status='submitted',submitted_at=?,updated_at=?,revision=revision+1 WHERE id=? AND revision=? AND status='draft'`,
      t,
      t,
      id,
      input.revision,
    ).run();
    const review = await stmt(
      db,
      "SELECT * FROM reviews WHERE id=?",
      id,
    ).first<Review>();
    if (!result.meta.changes && review?.status !== "submitted")
      return fail(409, "The draft changed. Save or reload before submitting.");
    return c.json(review);
  });
  app.get("/api/admin/users", async (c) =>
    c.json(
      (
        await c.env.DB.prepare(
          "SELECT * FROM users ORDER BY created_at DESC",
        ).all()
      ).results,
    ),
  );
  app.patch("/api/admin/users/:id", async (c) => {
    const id = c.req.param("id");
    if (id === c.get("user").id)
      fail(400, "You cannot change your own status.");
    const input = await body(
      c,
      z.object({ status: z.enum(["approved", "rejected", "disabled"]) }),
    );
    const result = await stmt(
      c.env.DB,
      `UPDATE users SET status=?,approved_at=CASE WHEN ?='approved' THEN COALESCE(approved_at,?) ELSE approved_at END WHERE id=? AND role='reviewer'`,
      input.status,
      input.status,
      now(),
      id,
    ).run();
    if (!result.meta.changes) fail(404, "Reviewer not found.");
    return c.json({ ok: true });
  });
  app.get("/api/admin/versions", async (c) =>
    c.json(
      (
        await c.env.DB.prepare(
          "SELECT id,article_id,version_number,title,subtitle,created_at FROM article_versions ORDER BY created_at DESC",
        ).all()
      ).results,
    ),
  );
  app.post("/api/admin/versions", async (c) => {
    const input = await body(
        c,
        z.object({
          article_id: z.string().uuid().optional(),
          title: z.string().trim().min(1).max(250),
          subtitle: z.string().max(500).default(""),
          body: z.string().min(1).max(150000),
          thumbnail_asset_id: z.string().uuid().optional(),
        }),
      ),
      db = c.env.DB,
      article = input.article_id ?? crypto.randomUUID(),
      id = crypto.randomUUID(),
      t = now();
    if (
      input.article_id &&
      !(await stmt(db, "SELECT id FROM articles WHERE id=?", article).first())
    )
      fail(404, "Article not found.");
    // Freeze exactly which stored assets this immutable version references.
    // Later thumbnail or alt-text edits never reach a reviewer of this version.
    // Every /api/assets/<token> in the draft must be a valid asset reference;
    // anything else is refused rather than silently dropped.
    const tokens = [
      ...new Set(
        [...input.body.matchAll(/\/api\/assets\/([^)\s"']+)/gi)].map(
          (match) => match[1]!,
        ),
      ),
    ];
    const linked = new Map<string, "inline" | "thumbnail">();
    for (const token of tokens) {
      let assetId = "";
      try {
        assetId = uuid(token);
      } catch {
        assetId = "";
      }
      if (!assetId)
        fail(
          400,
          "The draft contains an invalid image reference. Insert images from the workspace, or remove the broken /api/assets/ link.",
        );
      linked.set(assetId, "inline");
    }
    if (input.thumbnail_asset_id)
      linked.set(input.thumbnail_asset_id, "thumbnail");
    const assetRows = linked.size
      ? (
          await stmt(
            db,
            `SELECT id FROM image_assets WHERE archived_at IS NULL AND article_id=? AND id IN (${[...linked.keys()].map(() => "?").join(",")})`,
            article,
            ...linked.keys(),
          ).all()
        ).results.map((row) => row.id as string)
      : [];
    if (assetRows.length !== linked.size)
      fail(
        400,
        "The draft references an image that is missing, archived or belongs to another article.",
      );
    const rendered = renderMarkdown(input.body),
      batch = [];
    if (!input.article_id)
      batch.push(
        stmt(
          db,
          "INSERT INTO articles(id,created_by,created_at) VALUES(?,?,?)",
          article,
          c.get("user").id,
          t,
        ),
      );
    batch.push(
      stmt(
        db,
        `INSERT INTO article_versions(id,article_id,version_number,title,subtitle,body,body_format,rendered_html,anchor_text,created_at) SELECT ?,?,COALESCE(MAX(version_number),0)+1,?,?,?,'markdown',?,?,? FROM article_versions WHERE article_id=?`,
        id,
        article,
        input.title,
        input.subtitle,
        input.body,
        rendered.rendered_html,
        rendered.anchor_text,
        t,
        article,
      ),
    );
    for (const assetId of assetRows)
      batch.push(
        stmt(
          db,
          "INSERT INTO version_assets(version_id,image_asset_id,role) VALUES(?,?,?)",
          id,
          assetId,
          linked.get(assetId),
        ),
      );
    await db.batch(batch);
    return c.json({ id, article_id: article }, 201);
  });
  app.get("/api/admin/versions/:id", async (c) => {
    const db = c.env.DB,
      id = c.req.param("id");
    const [version, reviews, annotations] = await Promise.all([
      stmt(
        db,
        "SELECT * FROM article_versions WHERE id=?",
        id,
      ).first<ArticleVersion>(),
      stmt(
        db,
        `SELECT r.*,u.id reviewer_id,u.display_name,u.email FROM reviews r JOIN review_assignments a ON a.id=r.id JOIN users u ON u.id=a.user_id WHERE a.version_id=? AND r.status='submitted' ORDER BY r.submitted_at,r.id`,
        id,
      ).all<
        Review & { reviewer_id: string; display_name: string; email: string }
      >(),
      stmt(
        db,
        `SELECT x.* FROM annotations x JOIN reviews r ON r.id=x.review_id WHERE x.version_id=? AND r.status='submitted' ORDER BY x.start_offset,x.created_at`,
        id,
      ).all<Annotation & { review_id: string }>(),
    ]);
    if (!version) fail(404, "Article version not found.");
    return c.json({
      version,
      feedback: reviews.results.map(
        ({ reviewer_id, display_name, email, ...review }) => ({
          reviewer: { id: reviewer_id, display_name, email },
          review: {
            ...review,
            annotations: annotations.results.filter(
              (a) => a.review_id === review.id,
            ),
          },
        }),
      ),
    });
  });
  app.post("/api/admin/assignments", async (c) => {
    const input = await body(
        c,
        z.object({ version_id: z.string().uuid(), user_id: z.string().uuid() }),
      ),
      db = c.env.DB;
    if (
      !(await stmt(
        db,
        "SELECT id FROM users WHERE id=? AND status='approved'",
        input.user_id,
      ).first()) ||
      !(await stmt(
        db,
        "SELECT id FROM article_versions WHERE id=?",
        input.version_id,
      ).first())
    )
      fail(400, "Choose an approved user and an existing version.");
    await stmt(
      db,
      "INSERT INTO review_assignments(id,version_id,user_id,assigned_by,created_at) VALUES(?,?,?,?,?) ON CONFLICT(version_id,user_id) DO NOTHING",
      crypto.randomUUID(),
      input.version_id,
      input.user_id,
      c.get("user").id,
      now(),
    ).run();
    return c.json({ ok: true });
  });
  app.get("/api/admin/assignments", async (c) =>
    c.json(
      (
        await c.env.DB.prepare(
          `SELECT a.id,a.version_id,u.email,u.display_name,v.title,v.version_number,r.submitted_at,CASE WHEN r.status='submitted' THEN 'submitted' WHEN r.id IS NOT NULL THEN 'in progress' ELSE 'not started' END status FROM review_assignments a JOIN users u ON u.id=a.user_id JOIN article_versions v ON v.id=a.version_id LEFT JOIN reviews r ON r.id=a.id ORDER BY a.created_at DESC`,
        ).all()
      ).results,
    ),
  );
  app.get("/api/admin/reviews/:id", async (c) => {
    const db = c.env.DB,
      id = c.req.param("id");
    const row = await stmt(
      db,
      `SELECT a.version_id,a.user_id FROM review_assignments a JOIN reviews r ON r.id=a.id WHERE a.id=? AND r.status='submitted'`,
      id,
    ).first<{ version_id: string; user_id: string }>();
    if (!row) return fail(404, "Submitted review not found.");
    return c.json({
      ...(await detail(db, id, row.version_id)),
      reviewer: await stmt(
        db,
        "SELECT * FROM users WHERE id=?",
        row.user_id,
      ).first(),
    });
  });
  app.all("/api/*", (c) => c.json({ error: "Endpoint not found." }, 404));
  app.get("*", (c) => c.env.ASSETS.fetch(c.req.raw));
  return app;
}
export default createApp();
