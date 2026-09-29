import { Hono } from "hono";
import { HTTPException } from "hono/http-exception";
import { z } from "zod";
import { verifyIdentity, type Identity } from "./auth";
import { renderMarkdown } from "../shared/markdown";
import type { User, ArticleVersion, Review, Annotation } from "../shared/types";
export type Env = {
  DB: D1Database;
  ASSETS: Fetcher;
  ACCESS_TEAM_DOMAIN?: string;
  ACCESS_AUD?: string;
  BOOTSTRAP_ADMIN_EMAIL?: string;
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
    await next();
    c.header("X-Content-Type-Options", "nosniff");
    c.header("Referrer-Policy", "no-referrer");
    c.header("X-Frame-Options", "DENY");
    c.header("Permissions-Policy", "camera=(), microphone=(), geolocation=()");
    c.header(
      "Content-Security-Policy",
      "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self'; connect-src 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'; form-action 'self'",
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
  app.use("/api/*", async (c, next) => {
    if (!["GET", "HEAD"].includes(c.req.method)) {
      if (c.req.header("Origin") !== new URL(c.req.url).origin)
        fail(403, "Request origin rejected.");
      if (!c.req.header("Content-Type")?.startsWith("application/json"))
        fail(400, "JSON request required.");
    }
    if (!resolveIdentity && (!c.env.ACCESS_TEAM_DOMAIN || !c.env.ACCESS_AUD))
      fail(
        503,
        "Sign-in setup is not complete. The administrator must configure Cloudflare Access.",
      );
    const token = c.req.header("Cf-Access-Jwt-Assertion");
    if (!resolveIdentity && !token)
      fail(401, "Sign in with Google or email through Cloudflare Access.");
    let identity: Identity;
    try {
      identity = resolveIdentity
        ? await resolveIdentity(c.req.raw)
        : await verifyIdentity(
            token!,
            c.env.ACCESS_TEAM_DOMAIN!,
            c.env.ACCESS_AUD!,
          );
    } catch {
      return c.json(
        { error: "Your sign-in could not be verified. Sign in again." },
        401,
      );
    }
    const db = c.env.DB,
      t = now();
    // Bootstrap only affects creation, never overrides a revoked account on login.
    const bootstrap =
      identity.email === c.env.BOOTSTRAP_ADMIN_EMAIL?.trim().toLowerCase();
    await stmt(
      db,
      `INSERT INTO users(id,email,display_name,status,role,created_at,approved_at,last_login_at) VALUES(?,?,?,?,?,?,?,?) ON CONFLICT(email) DO UPDATE SET last_login_at=excluded.last_login_at`,
      crypto.randomUUID(),
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
      "SELECT * FROM users WHERE email=?",
      identity.email,
    ).first<User>();
    await stmt(
      db,
      "INSERT INTO access_identities(issuer,subject,user_id) VALUES(?,?,?) ON CONFLICT DO NOTHING",
      identity.issuer,
      identity.subject,
      user!.id,
    ).run();
    c.set("user", user!);
    await next();
  });
  app.get("/api/me", (c) => c.json({ user: c.get("user") }));
  app.use("/api/*", async (c, next) => {
    if (c.get("user").status !== "approved")
      fail(403, "Your account is not approved.");
    await next();
  });
  app.use("/api/admin/*", async (c, next) => {
    if (c.get("user").role !== "admin")
      fail(403, "Administrator access required.");
    await next();
  });
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
      }),
    );
    const db = c.env.DB,
      article = input.article_id ?? crypto.randomUUID(),
      id = crypto.randomUUID(),
      t = now();
    if (
      input.article_id &&
      !(await stmt(db, "SELECT id FROM articles WHERE id=?", article).first())
    )
      fail(404, "Article not found.");
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
    await db.batch(batch);
    return c.json({ id, article_id: article }, 201);
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
