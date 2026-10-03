import {
  generationInput,
  generationRoutes,
  parseCandidates,
  record,
  text,
  uuid,
  InputError,
  imageCatalog,
  imageJobInput,
  referencedAssetIds,
} from "../shared/generation.ts";
import type {
  GenerationInput,
  GenerationRoute,
  ImageCatalog,
} from "../shared/generation.ts";

// Structural D1 subset keeps this module executable in dependency-free SQLite tests.
export interface Statement {
  bind(...values: unknown[]): Statement;
  first<T = Record<string, unknown>>(): Promise<T | null>;
  all<T = Record<string, unknown>>(): Promise<{ results: T[] }>;
  run(): Promise<unknown>;
}
export interface Database {
  prepare(sql: string): Statement;
  batch(statements: Statement[]): Promise<unknown[]>;
}
// Structural R2 subset: pixels stay private and are served only after
// per-version authorization; there is no public bucket URL.
export interface ObjectStore {
  put(
    key: string,
    value: ArrayBuffer | ReadableStream | string,
    options?: { httpMetadata?: Record<string, string> },
  ): Promise<unknown>;
  get(key: string): Promise<{ body: ReadableStream<Uint8Array> } | null>;
}
export type GenerationEnv = {
  DB: Database;
  ARTICLE_LAB_ROUTES?: string;
  ARTICLE_LAB_RUNNER_TOKEN?: string;
  ARTICLE_LAB_IMAGES?: string;
  IMAGES?: ObjectStore;
};
export type Actor = { id: string; role: string; status: string };
type Job = {
  id: string;
  article_id: string;
  requested_by: string;
  kind: string;
  route_id: string;
  request_json: string;
  request_hash: string;
  route_json: string;
  status: string;
  lease_token: string | null;
  lease_expires_at: string | null;
  completion_hash: string | null;
};
type ImageJob = {
  id: string;
  article_id: string;
  requested_by: string;
  prompt: string;
  model: string;
  size: string;
  quality: string;
  request_hash: string;
  status: string;
  lease_token: string | null;
  lease_expires_at: string | null;
  error_code: string | null;
  asset_id: string | null;
};
class ApiError extends Error {
  status: number;
  constructor(status: number, message: string) {
    super(message);
    this.status = status;
  }
}
const now = () => new Date().toISOString();
const sql = (db: Database, query: string, ...values: unknown[]) =>
  db.prepare(query).bind(...values);
const json = (value: unknown, status = 200) =>
  new Response(JSON.stringify(value), {
    status,
    headers: {
      "Content-Type": "application/json",
      "Cache-Control": "no-store",
    },
  });
async function digest(value: string): Promise<string> {
  const bytes = await crypto.subtle.digest(
    "SHA-256",
    new TextEncoder().encode(value),
  );
  return [...new Uint8Array(bytes)]
    .map((v) => v.toString(16).padStart(2, "0"))
    .join("");
}
async function body(
  request: Request,
  max = 240_000,
): Promise<Record<string, unknown>> {
  if (!request.headers.get("Content-Type")?.startsWith("application/json"))
    throw new ApiError(400, "JSON required.");
  const reader = request.body?.getReader();
  if (!reader) throw new ApiError(400, "Body required.");
  const chunks: Uint8Array[] = [];
  let size = 0;
  for (;;) {
    const { value, done } = await reader.read();
    if (done) break;
    size += value.byteLength;
    if (size > max) {
      await reader.cancel();
      throw new ApiError(413, "Request too large.");
    }
    chunks.push(value);
  }
  const merged = new Uint8Array(size);
  let offset = 0;
  for (const chunk of chunks) {
    merged.set(chunk, offset);
    offset += chunk.length;
  }
  try {
    return record(JSON.parse(new TextDecoder().decode(merged)));
  } catch {
    throw new ApiError(400, "Invalid JSON object.");
  }
}
function catalog(env: GenerationEnv): GenerationRoute[] {
  try {
    return generationRoutes(JSON.parse(env.ARTICLE_LAB_ROUTES ?? "[]"));
  } catch {
    throw new ApiError(
      503,
      "Generation route configuration needs administrator attention.",
    );
  }
}
function images(env: GenerationEnv): ImageCatalog {
  try {
    return imageCatalog(env.ARTICLE_LAB_IMAGES || undefined);
  } catch {
    throw new ApiError(
      503,
      "Image generation configuration needs administrator attention.",
    );
  }
}
function store(env: GenerationEnv): ObjectStore {
  if (!env.IMAGES) throw new ApiError(503, "Image storage is not configured.");
  return env.IMAGES;
}
function responseError(error: unknown): Response {
  if (error instanceof ApiError)
    return json({ error: error.message }, error.status);
  if (error instanceof InputError) return json({ error: error.message }, 400);
  return json(
    {
      error:
        "Generation request failed. No provider details or credentials are exposed.",
    },
    500,
  );
}
async function existing(db: Database, article: string): Promise<void> {
  if (
    !(await sql(
      db,
      "SELECT article_id FROM article_workspaces WHERE article_id=?",
      article,
    ).first())
  )
    throw new ApiError(404, "Workspace not found.");
}
function revision(value: unknown): number {
  if (!Number.isSafeInteger(value) || (value as number) < 0)
    throw new InputError("Invalid revision.");
  return value as number;
}
const publicJob = `id,article_id,kind,route_id,status,actual_model,error_code,created_at,started_at,finished_at`;
const publicImageJob = `id,article_id,status,model,size,quality,error_code,asset_id,created_at,started_at,finished_at`;

/**
 * Decode actual image bytes, not a filename or Content-Type claim. PNG, JPEG
 * and WebP only; SVG and other formats are rejected before storage.
 */
export function sniffImage(
  bytes: Uint8Array,
): { content_type: string; width: number; height: number } | null {
  const u32 = (offset: number) =>
    (bytes[offset]! * 2 ** 24 +
      bytes[offset + 1]! * 2 ** 16 +
      bytes[offset + 2]! * 2 ** 8 +
      bytes[offset + 3]!) >>>
    0;
  const u16 = (offset: number) => bytes[offset]! * 256 + bytes[offset + 1]!;
  if (bytes.length < 24) return null;
  if (
    bytes[0] === 0x89 &&
    bytes[1] === 0x50 &&
    bytes[2] === 0x4e &&
    bytes[3] === 0x47
  ) {
    if (u32(8) !== 13 || String.fromCharCode(...bytes.slice(12, 16)) !== "IHDR")
      return null;
    const width = u32(16),
      height = u32(20);
    if (!width || !height || width > 10000 || height > 10000) return null;
    return { content_type: "image/png", width, height };
  }
  if (bytes[0] === 0xff && bytes[1] === 0xd8 && bytes[2] === 0xff) {
    let offset = 2;
    while (offset + 9 < bytes.length) {
      if (bytes[offset] !== 0xff) return null;
      const marker = bytes[offset + 1]!;
      const length = u16(offset + 2);
      if (
        marker >= 0xc0 &&
        marker <= 0xcf &&
        marker !== 0xc4 &&
        marker !== 0xc8 &&
        marker !== 0xcc
      ) {
        const height = u16(offset + 5),
          width = u16(offset + 7);
        if (!width || !height || width > 10000 || height > 10000) return null;
        return { content_type: "image/jpeg", width, height };
      }
      if (marker === 0xda) return null;
      offset += 2 + length;
    }
    return null;
  }
  if (
    String.fromCharCode(...bytes.slice(0, 4)) === "RIFF" &&
    String.fromCharCode(...bytes.slice(8, 12)) === "WEBP"
  ) {
    const chunk = String.fromCharCode(...bytes.slice(12, 16));
    if (chunk === "VP8X") {
      const width = 1 + (bytes[20]! + bytes[21]! * 256 + bytes[22]! * 65536);
      const height = 1 + (bytes[23]! + bytes[24]! * 256 + bytes[25]! * 65536);
      if (width > 10000 || height > 10000) return null;
      return { content_type: "image/webp", width, height };
    }
    if (chunk === "VP8 ") {
      const width = u16(26) & 0x3fff,
        height = u16(28) & 0x3fff;
      if (!width || !height) return null;
      return { content_type: "image/webp", width, height };
    }
    if (chunk === "VP8L") {
      const bits = bytes[21]! + bytes[22]! * 256 + bytes[23]! * 65536;
      const width = 1 + (bits & 0x3fff);
      const height = 1 + ((bits >> 14) & 0x3fff);
      return { content_type: "image/webp", width, height };
    }
    return null;
  }
  return null;
}
const IMAGE_EXTENSIONS = {
  "image/png": "png",
  "image/jpeg": "jpg",
  "image/webp": "webp",
} as const;
async function storeImageAsset(
  env: GenerationEnv,
  actorId: string,
  article: string,
  bytes: Uint8Array,
  provenance: {
    source: "generated" | "upload";
    job_id: string | null;
    model: string | null;
    prompt: string | null;
  },
): Promise<string> {
  const sniffed = sniffImage(bytes);
  if (!sniffed)
    throw new InputError("Only valid PNG, JPEG or WebP images are accepted.");
  if (sniffed.width < 200 || sniffed.height < 200)
    throw new InputError("Image is too small; use at least 200 × 200 pixels.");
  if (bytes.byteLength > 10_000_000)
    throw new InputError("Image exceeds the 10 MB limit.");
  const id = crypto.randomUUID();
  const objectKey = `articles/${article}/images/${id}.${
    IMAGE_EXTENSIONS[sniffed.content_type as keyof typeof IMAGE_EXTENSIONS]
  }`;
  await store(env).put(objectKey, bytes.slice().buffer as ArrayBuffer, {
    httpMetadata: { contentType: sniffed.content_type },
  });
  await sql(
    env.DB,
    `INSERT INTO image_assets(id,article_id,job_id,source,object_key,content_type,byte_size,width,height,requested_by,model,prompt,created_at)
     VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)`,
    id,
    article,
    provenance.job_id,
    provenance.source,
    objectKey,
    sniffed.content_type,
    bytes.byteLength,
    sniffed.width,
    sniffed.height,
    actorId,
    provenance.model,
    provenance.prompt,
    now(),
  ).run();
  return id;
}

/** Mount ONLY after the existing Better Auth + approved-admin checks. Defense in depth below. */
export async function handleLabRequest(
  request: Request,
  env: GenerationEnv,
  actor: Actor,
): Promise<Response> {
  try {
    if (actor.role !== "admin" || actor.status !== "approved")
      throw new ApiError(403, "Administrator access required.");
    const url = new URL(request.url);
    if (
      !["GET", "HEAD"].includes(request.method) &&
      request.headers.get("Origin") !== url.origin
    )
      throw new ApiError(403, "Request origin rejected.");
    const path = url.pathname.replace(/^\/api\/admin\/lab/, "");
    const db = env.DB;
    if (path === "/routes" && request.method === "GET") {
      const runners = (
        await db
          .prepare(
            "SELECT runner_id,last_seen,route_ids,image_ready FROM runner_heartbeats ORDER BY last_seen DESC LIMIT 5",
          )
          .all()
      ).results as {
        runner_id: string;
        last_seen: string;
        route_ids: string;
        image_ready: number;
      }[];
      let imageCatalogValue:
        ImageCatalog | { models: never[]; sizes: never[]; qualities: never[] } =
        {
          models: [],
          sizes: [],
          qualities: [],
        };
      try {
        imageCatalogValue = images(env);
      } catch {
        // Unconfigured images are reported honestly as unavailable, not as an error page.
      }
      return json({
        routes: catalog(env),
        runner_configured: Boolean(
          env.ARTICLE_LAB_RUNNER_TOKEN &&
          env.ARTICLE_LAB_RUNNER_TOKEN.length >= 32,
        ),
        images: imageCatalogValue,
        images_configured: Boolean(env.ARTICLE_LAB_IMAGES && env.IMAGES),
        runners: runners.map((runner) => ({
          runner_id: runner.runner_id,
          last_seen: runner.last_seen,
          route_ids: JSON.parse(runner.route_ids) as string[],
          image_ready: Boolean(runner.image_ready),
        })),
      });
    }
    if (path === "/workspaces" && request.method === "GET")
      return json(
        (
          await db
            .prepare(
              "SELECT article_id,topic,revision,updated_at FROM article_workspaces ORDER BY updated_at DESC",
            )
            .all()
        ).results,
      );
    if (path === "/workspaces" && request.method === "POST") {
      const input = await body(request),
        article = input.article_id
          ? uuid(input.article_id)
          : crypto.randomUUID(),
        t = now();
      const topic = text(input.topic, "topic", 250),
        statements: Statement[] = [];
      if (input.article_id) {
        if (
          !(await sql(
            db,
            "SELECT id FROM articles WHERE id=?",
            article,
          ).first())
        )
          throw new ApiError(404, "Article not found.");
      } else
        statements.push(
          sql(
            db,
            "INSERT INTO articles(id,created_by,created_at) VALUES(?,?,?)",
            article,
            actor.id,
            t,
          ),
        );
      statements.push(
        sql(
          db,
          "INSERT INTO article_workspaces(article_id,topic,brief,evidence,draft_body,created_at,updated_at) VALUES(?,?,?,?,?,?,?) ON CONFLICT(article_id) DO NOTHING",
          article,
          topic,
          "",
          "",
          "",
          t,
          t,
        ),
      );
      await db.batch(statements);
      return json({ article_id: article }, 201);
    }
    const match = path.match(
      /^\/workspaces\/([^/]+)(?:\/(jobs|selections|candidates|image-jobs|image-assets|image-upload|thumbnail))?$/,
    );
    if (!match) throw new ApiError(404, "Endpoint not found.");
    const article = uuid(match[1]);
    await existing(db, article);
    const action = match[2];
    if (!action && request.method === "GET") {
      const [
        workspace,
        candidates,
        selections,
        jobs,
        imageJobs,
        imageAssets,
        thumbnail,
      ] = await Promise.all([
        sql(
          db,
          "SELECT * FROM article_workspaces WHERE article_id=?",
          article,
        ).first(),
        sql(
          db,
          "SELECT * FROM generation_candidates WHERE article_id=? ORDER BY created_at,id",
          article,
        ).all(),
        sql(
          db,
          "SELECT * FROM article_selections WHERE article_id=?",
          article,
        ).all(),
        sql(
          db,
          `SELECT ${publicJob} FROM generation_jobs WHERE article_id=? ORDER BY created_at DESC LIMIT 100`,
          article,
        ).all(),
        sql(
          db,
          `SELECT ${publicImageJob} FROM image_jobs WHERE article_id=? ORDER BY created_at DESC LIMIT 50`,
          article,
        ).all(),
        sql(
          db,
          "SELECT * FROM image_assets WHERE article_id=? ORDER BY created_at DESC LIMIT 60",
          article,
        ).all(),
        sql(
          db,
          "SELECT image_asset_id,selected_at FROM article_thumbnail WHERE article_id=?",
          article,
        ).first(),
      ]);
      return json({
        workspace,
        candidates: candidates.results,
        selections: selections.results,
        jobs: jobs.results,
        image_jobs: imageJobs.results,
        image_assets: imageAssets.results,
        thumbnail,
      });
    }
    if (!action && request.method === "PUT") {
      const input = await body(request),
        rev = revision(input.revision);
      const topic = text(input.topic, "topic", 250),
        brief = text(input.brief ?? "", "brief", 30_000, true),
        evidence = text(input.evidence ?? "", "evidence", 60_000, true);
      // Do not trim draft text; exact author content is preserved.
      if (
        typeof input.draft_body !== "string" ||
        input.draft_body.length > 150_000
      )
        throw new InputError("Invalid draft body.");
      let promptSettings = "{}";
      if (input.prompt_settings !== undefined) {
        const entries = record(input.prompt_settings);
        for (const value of Object.values(entries))
          if (!value || typeof value !== "object" || Array.isArray(value))
            throw new InputError("Invalid prompt settings.");
        promptSettings = JSON.stringify(entries);
        if (promptSettings.length > 20_000)
          throw new InputError("Prompt settings are too large.");
      }
      const saved = await sql(
        db,
        `UPDATE article_workspaces SET topic=?,brief=?,evidence=?,draft_body=?,prompt_settings=?,revision=revision+1,updated_at=? WHERE article_id=? AND revision=? RETURNING revision`,
        topic,
        brief,
        evidence,
        input.draft_body,
        promptSettings,
        now(),
        article,
        rev,
      ).first();
      if (!saved)
        throw new ApiError(
          409,
          "Workspace changed elsewhere. Keep your unsaved text and reload before retrying.",
        );
      return json(saved);
    }
    if (action === "jobs" && request.method === "POST") {
      if (
        !env.ARTICLE_LAB_RUNNER_TOKEN ||
        env.ARTICLE_LAB_RUNNER_TOKEN.length < 32
      )
        throw new ApiError(
          503,
          "The generation runner has not been configured.",
        );
      const input = await body(request),
        jobId = uuid(input.id),
        generation = generationInput(input);
      const route = catalog(env).find((r) => r.id === input.route_id);
      if (!route)
        throw new ApiError(400, "Choose a configured subscription route.");
      const requestJson = JSON.stringify(generation),
        routeJson = JSON.stringify(route),
        hash = await digest(
          JSON.stringify([article, actor.id, generation, route]),
        );
      const previous = await sql(
        db,
        "SELECT id,request_hash FROM generation_jobs WHERE id=?",
        jobId,
      ).first<{ id: string; request_hash: string }>();
      if (previous) {
        if (previous.request_hash !== hash)
          throw new ApiError(
            409,
            "This request ID was already used for different input.",
          );
        return json({ id: previous.id }, 200);
      }
      // Atomic admission, including concurrent browser clicks. No implicit retries.
      await sql(
        db,
        `INSERT INTO generation_jobs(id,article_id,requested_by,kind,request_json,request_hash,route_json,route_id,created_at)
        SELECT ?,?,?,?,?,?,?,?,? WHERE (SELECT COUNT(*) FROM generation_jobs WHERE status IN ('queued','running'))<20
        AND (SELECT COUNT(*) FROM generation_jobs WHERE article_id=? AND status IN ('queued','running'))<3 ON CONFLICT(id) DO NOTHING`,
        jobId,
        article,
        actor.id,
        generation.kind,
        requestJson,
        hash,
        routeJson,
        route.id,
        now(),
        article,
      ).run();
      const saved = await sql(
        db,
        "SELECT id,request_hash FROM generation_jobs WHERE id=?",
        jobId,
      ).first<{ id: string; request_hash: string }>();
      if (!saved)
        throw new ApiError(
          409,
          "Generation queue is full. Wait for existing jobs to finish.",
        );
      if (saved.request_hash !== hash)
        throw new ApiError(409, "Conflicting request ID.");
      return json({ id: saved.id }, 202);
    }
    if (action === "image-jobs" && request.method === "POST") {
      if (
        !env.ARTICLE_LAB_RUNNER_TOKEN ||
        env.ARTICLE_LAB_RUNNER_TOKEN.length < 32 ||
        !env.ARTICLE_LAB_IMAGES ||
        !env.IMAGES
      )
        throw new ApiError(503, "Image generation has not been configured.");
      const allowed = images(env),
        input = await body(request),
        jobId = uuid(input.id),
        job = imageJobInput(input, allowed);
      const hash = await digest(JSON.stringify([article, actor.id, job]));
      const previous = await sql(
        db,
        "SELECT id,request_hash FROM image_jobs WHERE id=?",
        jobId,
      ).first<{ id: string; request_hash: string }>();
      if (previous) {
        if (previous.request_hash !== hash)
          throw new ApiError(
            409,
            "This request ID was already used for different input.",
          );
        return json({ id: previous.id }, 200);
      }
      await sql(
        db,
        `INSERT INTO image_jobs(id,article_id,requested_by,prompt,model,size,quality,request_hash,created_at)
         SELECT ?,?,?,?,?,?,?,?,? WHERE (SELECT COUNT(*) FROM image_jobs WHERE status IN ('queued','running'))<10
         AND (SELECT COUNT(*) FROM image_jobs WHERE article_id=? AND status IN ('queued','running'))<3 ON CONFLICT(id) DO NOTHING`,
        jobId,
        article,
        actor.id,
        job.prompt,
        job.model,
        job.size,
        job.quality,
        hash,
        now(),
        article,
      ).run();
      const saved = await sql(
        db,
        "SELECT id,request_hash FROM image_jobs WHERE id=?",
        jobId,
      ).first<{ id: string; request_hash: string }>();
      if (!saved)
        throw new ApiError(
          409,
          "Image queue is full. Wait for existing jobs to finish.",
        );
      return json({ id: saved.id }, 202);
    }
    if (
      (action === "jobs" || action === "image-jobs") &&
      request.method === "PATCH"
    ) {
      const input = await body(request),
        id = uuid(input.id);
      if (input.action !== "cancel")
        throw new InputError("Unsupported job action.");
      const table = action === "jobs" ? "generation_jobs" : "image_jobs";
      const saved = await sql(
        db,
        `UPDATE ${table} SET status='cancelled',finished_at=? WHERE id=? AND article_id=? AND status='queued' RETURNING id`,
        now(),
        id,
        article,
      ).first();
      if (!saved)
        throw new ApiError(
          409,
          "Only queued jobs can be cancelled. In-flight requests are not replayed or silently discarded.",
        );
      return json({ ok: true });
    }
    if (action === "selections" && request.method === "PUT") {
      const input = await body(request),
        candidate = uuid(input.candidate_id),
        rev = revision(input.revision),
        t = now(),
        mutation = crypto.randomUUID();
      const item = await sql(
        db,
        "SELECT kind FROM generation_candidates WHERE id=? AND article_id=? AND archived_at IS NULL",
        candidate,
        article,
      ).first<{ kind: string }>();
      if (!item)
        throw new ApiError(404, "Active candidate not found in this article.");
      await db.batch([
        sql(
          db,
          `INSERT INTO article_selections(article_id,kind,candidate_id,selected_at)
          SELECT ?,?,?,? WHERE EXISTS(SELECT 1 FROM article_workspaces WHERE article_id=? AND revision=?)
          AND EXISTS(SELECT 1 FROM generation_candidates WHERE id=? AND article_id=? AND archived_at IS NULL)
          ON CONFLICT(article_id,kind) DO UPDATE SET candidate_id=excluded.candidate_id,selected_at=excluded.selected_at`,
          article,
          item.kind,
          candidate,
          t,
          article,
          rev,
          candidate,
          article,
        ),
        sql(
          db,
          "UPDATE article_workspaces SET revision=revision+1,updated_at=?,last_mutation=? WHERE article_id=? AND revision=? AND EXISTS(SELECT 1 FROM generation_candidates WHERE id=? AND article_id=? AND archived_at IS NULL)",
          t,
          mutation,
          article,
          rev,
          candidate,
          article,
        ),
      ]);
      const chosen = await sql(
        db,
        "SELECT last_mutation FROM article_workspaces WHERE article_id=?",
        article,
      ).first<{ last_mutation: string }>();
      if (!chosen || chosen.last_mutation !== mutation)
        throw new ApiError(409, "Workspace changed. Reload before selecting.");
      return json({ revision: rev + 1 });
    }
    if (action === "thumbnail" && request.method === "PUT") {
      const input = await body(request),
        asset = uuid(input.image_asset_id),
        rev = revision(input.revision),
        t = now(),
        mutation = crypto.randomUUID();
      await db.batch([
        sql(
          db,
          `INSERT INTO article_thumbnail(article_id,image_asset_id,selected_at)
           SELECT ?,?,? WHERE EXISTS(SELECT 1 FROM article_workspaces WHERE article_id=? AND revision=?)
           AND EXISTS(SELECT 1 FROM image_assets WHERE id=? AND article_id=? AND archived_at IS NULL)
           ON CONFLICT(article_id) DO UPDATE SET image_asset_id=excluded.image_asset_id,selected_at=excluded.selected_at`,
          article,
          asset,
          t,
          article,
          rev,
          asset,
          article,
        ),
        sql(
          db,
          "UPDATE article_workspaces SET revision=revision+1,updated_at=?,last_mutation=? WHERE article_id=? AND revision=? AND EXISTS(SELECT 1 FROM image_assets WHERE id=? AND article_id=? AND archived_at IS NULL)",
          t,
          mutation,
          article,
          rev,
          asset,
          article,
        ),
      ]);
      const chosen = await sql(
        db,
        "SELECT last_mutation FROM article_workspaces WHERE article_id=?",
        article,
      ).first<{ last_mutation: string }>();
      if (!chosen || chosen.last_mutation !== mutation)
        throw new ApiError(409, "Workspace changed. Reload before selecting.");
      return json({ revision: rev + 1 });
    }
    if (action === "candidates" && request.method === "PATCH") {
      const input = await body(request),
        candidate = uuid(input.candidate_id);
      if (typeof input.archived !== "boolean")
        throw new InputError("Invalid archive flag.");
      // A selected candidate cannot disappear without an explicit replacement.
      const saved = await sql(
        db,
        `UPDATE generation_candidates SET archived_at=? WHERE id=? AND article_id=?
        AND NOT EXISTS(SELECT 1 FROM article_selections WHERE candidate_id=?) RETURNING id`,
        input.archived ? now() : null,
        candidate,
        article,
        candidate,
      ).first();
      if (!saved)
        throw new ApiError(
          409,
          "Candidate is selected or not part of this article.",
        );
      return json({ ok: true });
    }
    if (action === "image-assets" && request.method === "PATCH") {
      const input = await body(request),
        asset = uuid(input.image_asset_id);
      const alt =
        input.alt_text === undefined
          ? undefined
          : text(input.alt_text, "alt text", 1_000, true);
      const caption =
        input.caption === undefined
          ? undefined
          : text(input.caption, "caption", 2_000, true);
      // Metadata edits never touch stored pixels or provenance.
      const saved = await sql(
        db,
        `UPDATE image_assets SET
           alt_text=COALESCE(?,alt_text),caption=COALESCE(?,caption),
           archived_at=CASE WHEN ? THEN ? WHEN ? THEN NULL ELSE archived_at END
         WHERE id=? AND article_id=? AND NOT EXISTS(
           SELECT 1 FROM article_thumbnail WHERE image_asset_id=?
           UNION SELECT 1 FROM version_assets WHERE image_asset_id=image_assets.id) RETURNING id,alt_text,caption,archived_at`,
        alt,
        caption,
        input.archived === true ? 1 : 0,
        now(),
        input.archived === false ? 1 : 0,
        asset,
        article,
        asset,
      ).first();
      if (!saved)
        throw new ApiError(
          409,
          "Asset not found in this article, or it is already referenced by a published version.",
        );
      return json(saved);
    }
    if (action === "image-upload" && request.method === "POST") {
      const contentType = request.headers.get("Content-Type") ?? "";
      const boundary = contentType.match(/boundary=(?:"([^"]+)"|([^;]+))/);
      if (!contentType.startsWith("multipart/form-data") || !boundary)
        throw new ApiError(400, "Multipart image upload required.");
      const form = await request.formData();
      const file = form.get("file");
      if (!(file instanceof File) || !file.size || file.size > 10_000_000)
        throw new InputError("Attach one image file up to 10 MB.");
      const alt = text(form.get("alt_text") ?? "", "alt text", 1_000, true);
      const caption = text(form.get("caption") ?? "", "caption", 2_000, true);
      const bytes = new Uint8Array(await file.arrayBuffer());
      const id = await storeImageAsset(env, actor.id, article, bytes, {
        source: "upload",
        job_id: null,
        model: null,
        prompt: null,
      });
      await sql(
        db,
        "UPDATE image_assets SET alt_text=?,caption=? WHERE id=?",
        alt,
        caption,
        id,
      ).run();
      return json({ image_asset_id: id }, 201);
    }
    throw new ApiError(404, "Endpoint not found.");
  } catch (error) {
    return responseError(error);
  }
}

/** Serve a stored asset after session authorization; admin or assignment to a
 * version that froze this exact asset. Never a shareable public URL. */
export async function serveAsset(
  request: Request,
  env: GenerationEnv,
  actor: Actor,
): Promise<Response> {
  try {
    const id = uuid(new URL(request.url).pathname.split("/").pop());
    const asset = await sql(
      env.DB,
      "SELECT object_key,content_type,byte_size FROM image_assets WHERE id=? AND archived_at IS NULL",
      id,
    ).first<{ object_key: string; content_type: string; byte_size: number }>();
    if (!asset) return json({ error: "Image not found." }, 404);
    if (actor.role !== "admin") {
      const allowed = await sql(
        env.DB,
        `SELECT 1 AS ok FROM version_assets va JOIN review_assignments a ON a.version_id=va.version_id
         WHERE va.image_asset_id=? AND a.user_id=?`,
        id,
        actor.id,
      ).first();
      if (!allowed)
        return json(
          { error: "This image is not part of a version assigned to you." },
          403,
        );
    }
    const object = await store(env).get(asset.object_key);
    if (!object) return json({ error: "Image not found." }, 404);
    return new Response(object.body, {
      headers: {
        "Content-Type": asset.content_type,
        "Cache-Control": "private, max-age=31536000, immutable",
        "X-Content-Type-Options": "nosniff",
        "Content-Length": String(asset.byte_size),
      },
    });
  } catch {
    return json({ error: "Image request failed." }, 400);
  }
}

/** Machine-only routes; never accept this token as a browser/admin identity. */
export async function handleRunnerRequest(
  request: Request,
  env: GenerationEnv,
): Promise<Response> {
  try {
    if (
      !env.ARTICLE_LAB_RUNNER_TOKEN ||
      env.ARTICLE_LAB_RUNNER_TOKEN.length < 32
    )
      throw new ApiError(503, "Runner disabled.");
    if (request.headers.has("Origin"))
      throw new ApiError(403, "Browser requests are not accepted here.");
    const received = request.headers.get("Authorization") ?? "";
    const [actual, expected] = await Promise.all([
      digest(received),
      digest(`Bearer ${env.ARTICLE_LAB_RUNNER_TOKEN}`),
    ]);
    let different = 0;
    for (let i = 0; i < actual.length; i++)
      different |= actual.charCodeAt(i) ^ expected.charCodeAt(i);
    if (different) throw new ApiError(401, "Runner authentication required.");
    if (request.method !== "POST")
      throw new ApiError(404, "Endpoint not found.");
    const path = new URL(request.url).pathname.replace(
      /^\/api\/generation-runner/,
      "",
    );
    const input = await body(request),
      db = env.DB,
      t = now();
    if (path === "/heartbeat") {
      const runner = text(input.runner_id, "runner ID", 80);
      if (!Array.isArray(input.route_ids) || input.route_ids.length > 30)
        throw new InputError("Invalid runner routes.");
      const routes = input.route_ids.map((r) => text(r, "route ID", 80));
      await sql(
        db,
        `INSERT INTO runner_heartbeats(runner_id,last_seen,route_ids,image_ready) VALUES(?,?,?,?)
         ON CONFLICT(runner_id) DO UPDATE SET last_seen=excluded.last_seen,route_ids=excluded.route_ids,image_ready=excluded.image_ready`,
        runner,
        t,
        JSON.stringify(routes),
        input.image_ready === true ? 1 : 0,
      ).run();
      return json({ ok: true });
    }
    const expire = async (table: string) =>
      sql(
        db,
        `UPDATE ${table} SET status='uncertain',error_code='lease_expired',finished_at=? WHERE status='running' AND lease_expires_at<?`,
        t,
        t,
      ).run();
    if (path === "/claim") {
      const runner = text(input.runner_id, "runner ID", 80);
      if (
        !Array.isArray(input.route_ids) ||
        !input.route_ids.length ||
        input.route_ids.length > 30
      )
        throw new InputError("Invalid runner routes.");
      const routes = input.route_ids.map((r) => text(r, "route ID", 80));
      // Expired work may already have consumed quota. Never automatically replay it.
      await expire("generation_jobs");
      const enabled = catalog(env).filter((r) => routes.includes(r.id));
      if (!enabled.length) return json({ job: null });
      const token = crypto.randomUUID(),
        expires = new Date(Date.now() + 15 * 60_000).toISOString();
      const placeholders = enabled.map(() => "?").join(",");
      const job = await sql(
        db,
        `UPDATE generation_jobs SET status='running',lease_token=?,runner_id=?,lease_expires_at=?,started_at=?
        WHERE id=(SELECT j.id FROM generation_jobs j JOIN users u ON u.id=j.requested_by
          WHERE j.status='queued' AND u.role='admin' AND u.status='approved' AND j.route_id IN (${placeholders})
          ORDER BY j.created_at,j.id LIMIT 1) AND status='queued' RETURNING *`,
        token,
        runner,
        expires,
        t,
        ...enabled.map((r) => r.id),
      ).first<Job>();
      if (!job) return json({ job: null });
      const route = generationRoutes([JSON.parse(job.route_json)])[0];
      const current = enabled.find((r) => r.id === route.id);
      if (JSON.stringify(current) !== JSON.stringify(route)) {
        await sql(
          db,
          "UPDATE generation_jobs SET status='failed',error_code='route_changed',finished_at=? WHERE id=? AND lease_token=?",
          t,
          job.id,
          token,
        ).run();
        return json({ job: null });
      }
      return json({
        job: {
          id: job.id,
          article_id: job.article_id,
          ...generationInput(JSON.parse(job.request_json)),
          route,
          lease_token: token,
        },
      });
    }
    if (path === "/images/claim") {
      const runner = text(input.runner_id, "runner ID", 80);
      if (!env.ARTICLE_LAB_IMAGES || !env.IMAGES) return json({ job: null });
      const allowed = images(env);
      await expire("image_jobs");
      const token = crypto.randomUUID(),
        expires = new Date(Date.now() + 15 * 60_000).toISOString();
      const job = await sql(
        db,
        `UPDATE image_jobs SET status='running',lease_token=?,runner_id=?,lease_expires_at=?,started_at=?
         WHERE id=(SELECT j.id FROM image_jobs j JOIN users u ON u.id=j.requested_by
           WHERE j.status='queued' AND u.role='admin' AND u.status='approved' AND j.model IN (${allowed.models.map(() => "?").join(",")})
           ORDER BY j.created_at,j.id LIMIT 1) AND status='queued' RETURNING *`,
        token,
        runner,
        expires,
        t,
        ...allowed.models,
      ).first<ImageJob>();
      if (!job) return json({ job: null });
      return json({
        job: {
          id: job.id,
          article_id: job.article_id,
          prompt: job.prompt,
          model: job.model,
          size: job.size,
          quality: job.quality,
          lease_token: token,
        },
      });
    }
    const imageMatch = path.match(/^\/images\/jobs\/([^/]+)\/(complete|fail)$/);
    if (imageMatch) {
      const jobId = uuid(imageMatch[1]),
        token = uuid(input.lease_token);
      const job = await sql(
        db,
        "SELECT * FROM image_jobs WHERE id=?",
        jobId,
      ).first<ImageJob>();
      if (!job || job.lease_token !== token)
        throw new ApiError(409, "Job lease does not match.");
      if (imageMatch[2] === "fail") {
        if (
          ![
            "provider_rejected",
            "invalid_output",
            "transport_uncertain",
            "runner_configuration",
          ].includes(String(input.code))
        )
          throw new InputError("Invalid failure code.");
        const status =
          input.code === "transport_uncertain" ? "uncertain" : "failed";
        const saved = await sql(
          db,
          "UPDATE image_jobs SET status=?,error_code=?,finished_at=? WHERE id=? AND lease_token=? AND status='running' AND lease_expires_at>=? RETURNING id",
          status,
          input.code,
          t,
          jobId,
          token,
          t,
        ).first();
        if (!saved) throw new ApiError(409, "Job is no longer running.");
        return json({ ok: true });
      }
      if (job.status === "succeeded" && job.asset_id)
        return json({ ok: true, duplicate: true });
      if (
        job.status !== "running" ||
        !job.lease_expires_at ||
        job.lease_expires_at < t
      )
        throw new ApiError(409, "Job lease expired or already completed.");
      const encoded = input.image_base64;
      if (
        typeof encoded !== "string" ||
        encoded.length > 14_000_000 ||
        !/^[A-Za-z0-9+/]+={0,2}$/.test(encoded)
      )
        throw new InputError("Invalid image payload.");
      const bytes = Uint8Array.from(atob(encoded), (c) => c.charCodeAt(0));
      const requested = await sql(
        db,
        "SELECT requested_by FROM image_jobs WHERE id=?",
        jobId,
      ).first<{ requested_by: string }>();
      const assetId = await storeImageAsset(
        env,
        requested!.requested_by,
        job.article_id,
        bytes,
        {
          source: "generated",
          job_id: job.id,
          model: job.model,
          prompt: job.prompt,
        },
      );
      const saved = await sql(
        db,
        "UPDATE image_jobs SET status='succeeded',asset_id=?,finished_at=? WHERE id=? AND lease_token=? AND status='running' AND lease_expires_at>=? RETURNING id",
        assetId,
        t,
        jobId,
        token,
        t,
      ).first();
      if (!saved)
        throw new ApiError(409, "Job lease expired or already completed.");
      return json({ ok: true, image_asset_id: assetId });
    }
    const match = path.match(/^\/jobs\/([^/]+)\/(complete|fail)$/);
    if (!match) throw new ApiError(404, "Endpoint not found.");
    const jobId = uuid(match[1]),
      token = uuid(input.lease_token);
    const job = await sql(
      db,
      "SELECT * FROM generation_jobs WHERE id=?",
      jobId,
    ).first<Job>();
    if (!job || job.lease_token !== token)
      throw new ApiError(409, "Job lease does not match.");
    if (match[2] === "fail") {
      if (
        ![
          "provider_rejected",
          "invalid_output",
          "transport_uncertain",
          "route_mismatch",
          "runner_configuration",
        ].includes(String(input.code))
      )
        throw new InputError("Invalid failure code.");
      const status =
        input.code === "transport_uncertain" ? "uncertain" : "failed";
      const saved = await sql(
        db,
        "UPDATE generation_jobs SET status=?,error_code=?,finished_at=? WHERE id=? AND lease_token=? AND status='running' AND lease_expires_at>=? RETURNING id",
        status,
        input.code,
        t,
        jobId,
        token,
        t,
      ).first();
      if (!saved) throw new ApiError(409, "Job is no longer running.");
      return json({ ok: true });
    }
    const generation: GenerationInput = generationInput(
      JSON.parse(job.request_json),
    );
    const route = generationRoutes([JSON.parse(job.route_json)])[0],
      model = text(input.actual_model, "actual model", 160);
    if (!route.response_models.includes(model))
      throw new ApiError(409, "Provider returned an unqualified model.");
    const candidates = parseCandidates(input.output, generation),
      hash = await digest(JSON.stringify([model, candidates]));
    if (job.status === "succeeded" && job.completion_hash === hash)
      return json({ ok: true, duplicate: true });
    if (
      job.status !== "running" ||
      !job.lease_expires_at ||
      job.lease_expires_at < t
    )
      throw new ApiError(409, "Job lease expired or already completed.");
    const allowed =
      "EXISTS(SELECT 1 FROM generation_jobs WHERE id=? AND lease_token=? AND status='running' AND lease_expires_at>=?)";
    await db.batch([
      ...candidates.map((value) =>
        sql(
          db,
          `INSERT INTO generation_candidates(id,article_id,job_id,kind,value,created_at) SELECT ?,?,?,?,?,? WHERE ${allowed}`,
          crypto.randomUUID(),
          job.article_id,
          job.id,
          generation.kind,
          value,
          t,
          jobId,
          token,
          t,
        ),
      ),
      sql(
        db,
        "UPDATE generation_jobs SET status='succeeded',actual_model=?,completion_hash=?,finished_at=? WHERE id=? AND lease_token=? AND status='running' AND lease_expires_at>=?",
        model,
        hash,
        t,
        jobId,
        token,
        t,
      ),
    ]);
    const completed = await sql(
      db,
      "SELECT status,completion_hash FROM generation_jobs WHERE id=?",
      jobId,
    ).first<{ status: string; completion_hash: string }>();
    if (completed?.status !== "succeeded" || completed.completion_hash !== hash)
      throw new ApiError(409, "Conflicting completion.");
    return json({ ok: true, candidate_count: candidates.length });
  } catch (error) {
    return responseError(error);
  }
}
