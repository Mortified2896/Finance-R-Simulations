import { generationInput, generationRoutes, parseCandidates, record, text, uuid, InputError } from "../shared/generation.ts";
import type { GenerationInput, GenerationRoute } from "../shared/generation.ts";

// Structural D1 subset keeps this module executable in dependency-free SQLite tests.
export interface Statement {
  bind(...values: unknown[]): Statement;
  first<T = Record<string, unknown>>(): Promise<T | null>;
  all<T = Record<string, unknown>>(): Promise<{ results: T[] }>;
  run(): Promise<unknown>;
}
export interface Database { prepare(sql: string): Statement; batch(statements: Statement[]): Promise<unknown[]>; }
export type GenerationEnv = { DB: Database; ARTICLE_LAB_ROUTES?: string; ARTICLE_LAB_RUNNER_TOKEN?: string };
export type Actor = { id: string; role: string; status: string };
type Job = {
  id: string; article_id: string; requested_by: string; kind: string; route_id: string;
  request_json: string; request_hash: string; route_json: string; status: string;
  lease_token: string | null; lease_expires_at: string | null; completion_hash: string | null;
};
class ApiError extends Error { status: number; constructor(status: number, message: string) { super(message); this.status = status; } }
const now = () => new Date().toISOString();
const sql = (db: Database, query: string, ...values: unknown[]) => db.prepare(query).bind(...values);
const json = (value: unknown, status = 200) => new Response(JSON.stringify(value), { status, headers: { "Content-Type": "application/json", "Cache-Control": "no-store" } });
async function digest(value: string): Promise<string> {
  const bytes = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(value));
  return [...new Uint8Array(bytes)].map((v) => v.toString(16).padStart(2, "0")).join("");
}
async function body(request: Request): Promise<Record<string, unknown>> {
  if (!request.headers.get("Content-Type")?.startsWith("application/json")) throw new ApiError(400, "JSON required.");
  const reader = request.body?.getReader();
  if (!reader) throw new ApiError(400, "Body required.");
  const chunks: Uint8Array[] = []; let size = 0;
  for (;;) {
    const { value, done } = await reader.read(); if (done) break;
    size += value.byteLength;
    if (size > 240_000) { await reader.cancel(); throw new ApiError(413, "Request too large."); }
    chunks.push(value);
  }
  const merged = new Uint8Array(size); let offset = 0;
  for (const chunk of chunks) { merged.set(chunk, offset); offset += chunk.length; }
  try { return record(JSON.parse(new TextDecoder().decode(merged))); } catch { throw new ApiError(400, "Invalid JSON object."); }
}
function catalog(env: GenerationEnv): GenerationRoute[] {
  try { return generationRoutes(JSON.parse(env.ARTICLE_LAB_ROUTES ?? "[]")); }
  catch { throw new ApiError(503, "Generation route configuration needs administrator attention."); }
}
function responseError(error: unknown): Response {
  if (error instanceof ApiError) return json({ error: error.message }, error.status);
  if (error instanceof InputError) return json({ error: error.message }, 400);
  return json({ error: "Generation request failed. No provider details or credentials are exposed." }, 500);
}
async function existing(db: Database, article: string): Promise<void> {
  if (!await sql(db, "SELECT article_id FROM article_workspaces WHERE article_id=?", article).first()) throw new ApiError(404, "Workspace not found.");
}
function revision(value: unknown): number {
  if (!Number.isSafeInteger(value) || (value as number) < 0) throw new InputError("Invalid revision.");
  return value as number;
}
const publicJob = `id,article_id,kind,route_id,status,actual_model,error_code,created_at,started_at,finished_at`;

/** Mount ONLY after the existing Better Auth + approved-admin checks. Defense in depth below. */
export async function handleLabRequest(request: Request, env: GenerationEnv, actor: Actor): Promise<Response> {
  try {
    if (actor.role !== "admin" || actor.status !== "approved") throw new ApiError(403, "Administrator access required.");
    const url = new URL(request.url);
    if (!["GET", "HEAD"].includes(request.method) && request.headers.get("Origin") !== url.origin) throw new ApiError(403, "Request origin rejected.");
    const path = url.pathname.replace(/^\/api\/admin\/lab/, "");
    const db = env.DB;
    if (path === "/routes" && request.method === "GET") return json({ routes: catalog(env), runner_configured: Boolean(env.ARTICLE_LAB_RUNNER_TOKEN && env.ARTICLE_LAB_RUNNER_TOKEN.length >= 32) });
    if (path === "/workspaces" && request.method === "GET") return json((await db.prepare("SELECT article_id,topic,revision,updated_at FROM article_workspaces ORDER BY updated_at DESC").all()).results);
    if (path === "/workspaces" && request.method === "POST") {
      const input = await body(request), article = input.article_id ? uuid(input.article_id) : crypto.randomUUID(), t = now();
      const topic = text(input.topic, "topic", 250), statements: Statement[] = [];
      if (input.article_id) {
        if (!await sql(db, "SELECT id FROM articles WHERE id=?", article).first()) throw new ApiError(404, "Article not found.");
      } else statements.push(sql(db, "INSERT INTO articles(id,created_by,created_at) VALUES(?,?,?)", article, actor.id, t));
      statements.push(sql(db, "INSERT INTO article_workspaces(article_id,topic,created_at,updated_at) VALUES(?,?,?,?) ON CONFLICT(article_id) DO NOTHING", article, topic, t, t));
      await db.batch(statements); return json({ article_id: article }, 201);
    }
    const match = path.match(/^\/workspaces\/([^/]+)(?:\/(jobs|selections|candidates))?$/);
    if (!match) throw new ApiError(404, "Endpoint not found.");
    const article = uuid(match[1]); await existing(db, article);
    const action = match[2];
    if (!action && request.method === "GET") {
      const [workspace, candidates, selections, jobs] = await Promise.all([
        sql(db, "SELECT * FROM article_workspaces WHERE article_id=?", article).first(),
        sql(db, "SELECT * FROM generation_candidates WHERE article_id=? ORDER BY created_at,id", article).all(),
        sql(db, "SELECT * FROM article_selections WHERE article_id=?", article).all(),
        sql(db, `SELECT ${publicJob} FROM generation_jobs WHERE article_id=? ORDER BY created_at DESC LIMIT 100`, article).all(),
      ]);
      return json({ workspace, candidates: candidates.results, selections: selections.results, jobs: jobs.results });
    }
    if (!action && request.method === "PUT") {
      const input = await body(request), rev = revision(input.revision);
      const topic = text(input.topic, "topic", 250), brief = text(input.brief ?? "", "brief", 30_000, true), evidence = text(input.evidence ?? "", "evidence", 60_000, true);
      // Do not trim draft text; exact author content is preserved.
      if (typeof input.draft_body !== "string" || input.draft_body.length > 150_000) throw new InputError("Invalid draft body.");
      const saved = await sql(db, "UPDATE article_workspaces SET topic=?,brief=?,evidence=?,draft_body=?,revision=revision+1,updated_at=? WHERE article_id=? AND revision=? RETURNING revision", topic, brief, evidence, input.draft_body, now(), article, rev).first();
      if (!saved) throw new ApiError(409, "Workspace changed elsewhere. Keep your unsaved text and reload before retrying.");
      return json(saved);
    }
    if (action === "jobs" && request.method === "POST") {
      if (!env.ARTICLE_LAB_RUNNER_TOKEN || env.ARTICLE_LAB_RUNNER_TOKEN.length < 32) throw new ApiError(503, "The generation runner has not been configured.");
      const input = await body(request), jobId = uuid(input.id), generation = generationInput(input);
      const route = catalog(env).find((r) => r.id === input.route_id);
      if (!route) throw new ApiError(400, "Choose a configured subscription route.");
      const requestJson = JSON.stringify(generation), routeJson = JSON.stringify(route), hash = await digest(JSON.stringify([article, actor.id, generation, route]));
      const previous = await sql(db, "SELECT id,request_hash FROM generation_jobs WHERE id=?", jobId).first<{ id: string; request_hash: string }>();
      if (previous) {
        if (previous.request_hash !== hash) throw new ApiError(409, "This request ID was already used for different input.");
        return json({ id: previous.id }, 200);
      }
      // Atomic admission, including concurrent browser clicks. No implicit retries.
      await sql(db, `INSERT INTO generation_jobs(id,article_id,requested_by,kind,request_json,request_hash,route_json,route_id,created_at)
        SELECT ?,?,?,?,?,?,?,?,? WHERE (SELECT COUNT(*) FROM generation_jobs WHERE status IN ('queued','running'))<20
        AND (SELECT COUNT(*) FROM generation_jobs WHERE article_id=? AND status IN ('queued','running'))<3 ON CONFLICT(id) DO NOTHING`,
        jobId, article, actor.id, generation.kind, requestJson, hash, routeJson, route.id, now(), article).run();
      const saved = await sql(db, "SELECT id,request_hash FROM generation_jobs WHERE id=?", jobId).first<{ id: string; request_hash: string }>();
      if (!saved) throw new ApiError(409, "Generation queue is full. Wait for existing jobs to finish.");
      if (saved.request_hash !== hash) throw new ApiError(409, "Conflicting request ID.");
      return json({ id: saved.id }, 202);
    }
    if (action === "jobs" && request.method === "PATCH") {
      const input = await body(request), id = uuid(input.id);
      if (input.action !== "cancel") throw new InputError("Unsupported job action.");
      const saved = await sql(db, "UPDATE generation_jobs SET status='cancelled',finished_at=? WHERE id=? AND article_id=? AND status='queued' RETURNING id", now(), id, article).first();
      if (!saved) throw new ApiError(409, "Only queued jobs can be cancelled. In-flight requests are not replayed or silently discarded.");
      return json({ ok: true });
    }
    if (action === "selections" && request.method === "PUT") {
      const input = await body(request), candidate = uuid(input.candidate_id), rev = revision(input.revision), t = now(), mutation = crypto.randomUUID();
      const item = await sql(db, "SELECT kind FROM generation_candidates WHERE id=? AND article_id=? AND archived_at IS NULL", candidate, article).first<{ kind: string }>();
      if (!item) throw new ApiError(404, "Active candidate not found in this article.");
      await db.batch([
        sql(db, `INSERT INTO article_selections(article_id,kind,candidate_id,selected_at)
          SELECT ?,?,?,? WHERE EXISTS(SELECT 1 FROM article_workspaces WHERE article_id=? AND revision=?)
          AND EXISTS(SELECT 1 FROM generation_candidates WHERE id=? AND article_id=? AND archived_at IS NULL)
          ON CONFLICT(article_id,kind) DO UPDATE SET candidate_id=excluded.candidate_id,selected_at=excluded.selected_at`, article, item.kind, candidate, t, article, rev, candidate, article),
        sql(db, "UPDATE article_workspaces SET revision=revision+1,updated_at=?,last_mutation=? WHERE article_id=? AND revision=? AND EXISTS(SELECT 1 FROM generation_candidates WHERE id=? AND article_id=? AND archived_at IS NULL)", t, mutation, article, rev, candidate, article),
      ]);
      const chosen = await sql(db, "SELECT last_mutation FROM article_workspaces WHERE article_id=?", article).first<{ last_mutation: string }>();
      if (!chosen || chosen.last_mutation !== mutation) throw new ApiError(409, "Workspace changed. Reload before selecting.");
      return json({ revision: rev + 1 });
    }
    if (action === "candidates" && request.method === "PATCH") {
      const input = await body(request), candidate = uuid(input.candidate_id);
      if (typeof input.archived !== "boolean") throw new InputError("Invalid archive flag.");
      // A selected candidate cannot disappear without an explicit replacement.
      const saved = await sql(db, `UPDATE generation_candidates SET archived_at=? WHERE id=? AND article_id=?
        AND NOT EXISTS(SELECT 1 FROM article_selections WHERE candidate_id=?) RETURNING id`, input.archived ? now() : null, candidate, article, candidate).first();
      if (!saved) throw new ApiError(409, "Candidate is selected or not part of this article.");
      return json({ ok: true });
    }
    throw new ApiError(404, "Endpoint not found.");
  } catch (error) { return responseError(error); }
}

/** Machine-only routes; never accept this token as a browser/admin identity. */
export async function handleRunnerRequest(request: Request, env: GenerationEnv): Promise<Response> {
  try {
    if (!env.ARTICLE_LAB_RUNNER_TOKEN || env.ARTICLE_LAB_RUNNER_TOKEN.length < 32) throw new ApiError(503, "Runner disabled.");
    if (request.headers.has("Origin")) throw new ApiError(403, "Browser requests are not accepted here.");
    const received = request.headers.get("Authorization") ?? "";
    const [actual, expected] = await Promise.all([digest(received), digest(`Bearer ${env.ARTICLE_LAB_RUNNER_TOKEN}`)]);
    let different = 0; for (let i = 0; i < actual.length; i++) different |= actual.charCodeAt(i) ^ expected.charCodeAt(i);
    if (different) throw new ApiError(401, "Runner authentication required.");
    if (request.method !== "POST") throw new ApiError(404, "Endpoint not found.");
    const path = new URL(request.url).pathname.replace(/^\/api\/generation-runner/, ""), input = await body(request), db = env.DB, t = now();
    if (path === "/claim") {
      const runner = text(input.runner_id, "runner ID", 80);
      if (!Array.isArray(input.route_ids) || !input.route_ids.length || input.route_ids.length > 30) throw new InputError("Invalid runner routes.");
      const routes = input.route_ids.map((r) => text(r, "route ID", 80));
      // Expired work may already have consumed quota. Never automatically replay it.
      await sql(db, "UPDATE generation_jobs SET status='uncertain',error_code='lease_expired',finished_at=? WHERE status='running' AND lease_expires_at<?", t, t).run();
      const enabled = catalog(env).filter((r) => routes.includes(r.id));
      if (!enabled.length) return json({ job: null });
      const token = crypto.randomUUID(), expires = new Date(Date.now() + 15 * 60_000).toISOString();
      const placeholders = enabled.map(() => "?").join(",");
      const job = await sql(db, `UPDATE generation_jobs SET status='running',lease_token=?,runner_id=?,lease_expires_at=?,started_at=?
        WHERE id=(SELECT j.id FROM generation_jobs j JOIN users u ON u.id=j.requested_by
          WHERE j.status='queued' AND u.role='admin' AND u.status='approved' AND j.route_id IN (${placeholders})
          ORDER BY j.created_at,j.id LIMIT 1) AND status='queued' RETURNING *`, token, runner, expires, t, ...enabled.map((r) => r.id)).first<Job>();
      if (!job) return json({ job: null });
      const route = generationRoutes([JSON.parse(job.route_json)])[0];
      const current = enabled.find((r) => r.id === route.id);
      if (JSON.stringify(current) !== JSON.stringify(route)) {
        await sql(db, "UPDATE generation_jobs SET status='failed',error_code='route_changed',finished_at=? WHERE id=? AND lease_token=?", t, job.id, token).run();
        return json({ job: null });
      }
      return json({ job: { id: job.id, article_id: job.article_id, ...generationInput(JSON.parse(job.request_json)), route, lease_token: token } });
    }
    const match = path.match(/^\/jobs\/([^/]+)\/(complete|fail)$/);
    if (!match) throw new ApiError(404, "Endpoint not found.");
    const jobId = uuid(match[1]), token = uuid(input.lease_token);
    const job = await sql(db, "SELECT * FROM generation_jobs WHERE id=?", jobId).first<Job>();
    if (!job || job.lease_token !== token) throw new ApiError(409, "Job lease does not match.");
    if (match[2] === "fail") {
      if (!["provider_rejected", "invalid_output", "transport_uncertain", "route_mismatch", "runner_configuration"].includes(String(input.code))) throw new InputError("Invalid failure code.");
      const status = input.code === "transport_uncertain" ? "uncertain" : "failed";
      const saved = await sql(db, "UPDATE generation_jobs SET status=?,error_code=?,finished_at=? WHERE id=? AND lease_token=? AND status='running' AND lease_expires_at>=? RETURNING id", status, input.code, t, jobId, token, t).first();
      if (!saved) throw new ApiError(409, "Job is no longer running.");
      return json({ ok: true });
    }
    const generation: GenerationInput = generationInput(JSON.parse(job.request_json));
    const route = generationRoutes([JSON.parse(job.route_json)])[0], model = text(input.actual_model, "actual model", 160);
    if (!route.response_models.includes(model)) throw new ApiError(409, "Provider returned an unqualified model.");
    const candidates = parseCandidates(input.output, generation), hash = await digest(JSON.stringify([model, candidates]));
    if (job.status === "succeeded" && job.completion_hash === hash) return json({ ok: true, duplicate: true });
    if (job.status !== "running" || !job.lease_expires_at || job.lease_expires_at < t) throw new ApiError(409, "Job lease expired or already completed.");
    const allowed = "EXISTS(SELECT 1 FROM generation_jobs WHERE id=? AND lease_token=? AND status='running' AND lease_expires_at>=?)";
    await db.batch([
      ...candidates.map((value) => sql(db, `INSERT INTO generation_candidates(id,article_id,job_id,kind,value,created_at) SELECT ?,?,?,?,?,? WHERE ${allowed}`, crypto.randomUUID(), job.article_id, job.id, generation.kind, value, t, jobId, token, t)),
      sql(db, "UPDATE generation_jobs SET status='succeeded',actual_model=?,completion_hash=?,finished_at=? WHERE id=? AND lease_token=? AND status='running' AND lease_expires_at>=?", model, hash, t, jobId, token, t),
    ]);
    const completed = await sql(db, "SELECT status,completion_hash FROM generation_jobs WHERE id=?", jobId).first<{ status: string; completion_hash: string }>();
    if (completed?.status !== "succeeded" || completed.completion_hash !== hash) throw new ApiError(409, "Conflicting completion.");
    return json({ ok: true, candidate_count: candidates.length });
  } catch (error) { return responseError(error); }
}
