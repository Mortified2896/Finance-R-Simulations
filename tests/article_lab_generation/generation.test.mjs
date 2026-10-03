import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import { DatabaseSync } from "node:sqlite";
import { generationInput, generationRoutes, parseCandidates, providerPrompt } from "../../apps/article_lab_web/shared/generation.ts";
import { handleLabRequest, handleRunnerRequest } from "../../apps/article_lab_web/worker/generation.ts";

const route = { id: "glm-direct", label: "GLM subscription", provider: "glm", transport: "direct", model: "glm-test", response_models: ["glm-test"] };
const token = "synthetic-test-only-".repeat(3);
const actor = { id: "00000000-0000-4000-8000-000000000001", role: "admin", status: "approved" };
const origin = "https://article-lab.example";
function fixture() {
  const native = new DatabaseSync(":memory:");
  for (const filename of ["0001_review.sql", "0003_generation_foundation.sql", "0004_image_assets.sql"]) native.exec(fs.readFileSync(new URL(`../../apps/article_lab_web/migrations/${filename}`, import.meta.url), "utf8"));
  native.prepare("INSERT INTO users(id,email,display_name,status,role,created_at,last_login_at) VALUES(?,?,?,?,?,?,?)").run(actor.id, "owner@example.test", "Owner", "approved", "admin", "2026-01-01", "2026-01-01");
  class Prepared {
    constructor(query, values = []) { this.query = query; this.values = values; }
    bind(...values) { return new Prepared(this.query, values); }
    async first() { return native.prepare(this.query).get(...this.values) ?? null; }
    async all() { return { results: native.prepare(this.query).all(...this.values) }; }
    async run() { return { meta: native.prepare(this.query).run(...this.values) }; }
  }
  const db = { prepare: (query) => new Prepared(query), async batch(statements) {
    native.exec("BEGIN");
    try { const result = []; for (const statement of statements) result.push(await statement.run()); native.exec("COMMIT"); return result; }
    catch (error) { native.exec("ROLLBACK"); throw error; }
  }};
  const env = { DB: db, ARTICLE_LAB_ROUTES: JSON.stringify([route]), ARTICLE_LAB_RUNNER_TOKEN: token };
  async function admin(path, method = "GET", input, overrides = {}) {
    const request = new Request(`${origin}/api/admin/lab${path}`, { method, headers: { "Origin": origin, "Content-Type": "application/json", ...overrides.headers }, ...(input === undefined ? {} : { body: JSON.stringify(input) }) });
    const response = await handleLabRequest(request, overrides.env ?? env, overrides.actor ?? actor);
    return { status: response.status, data: await response.json() };
  }
  async function machine(path, input, headers = {}) {
    const request = new Request(`${origin}/api/generation-runner${path}`, { method: "POST", headers: { "Authorization": `Bearer ${token}`, "Content-Type": "application/json", ...headers }, body: JSON.stringify(input) });
    const response = await handleRunnerRequest(request, env);
    return { status: response.status, data: await response.json() };
  }
  const create = async () => (await admin("/workspaces", "POST", { topic: "A test finance article" })).data.article_id;
  const enqueue = (article, overrides = {}) => admin(`/workspaces/${article}/jobs`, "POST", { id: crypto.randomUUID(), kind: "titles", count: 2, resolved_prompt: "Explain diversification using only the supplied research.", route_id: route.id, ...overrides });
  const claim = () => machine("/claim", { runner_id: "test-runner", route_ids: [route.id] });
  const complete = (job, overrides = {}) => machine(`/jobs/${job.id}/complete`, { lease_token: job.lease_token, actual_model: "glm-test", output: '{"candidates":["Diversify Without the Guesswork","A Clearer Guide to Index Funds"]}', ...overrides });
  return { native, env, admin, machine, create, enqueue, claim, complete };
}

for (const count of [0, -1, 26, 1.5]) test(`reject candidate count ${count}`, () => assert.throws(() => generationInput({ kind: "titles", count, resolved_prompt: "Test" })));
test("drafts remain manual and outlines have one candidate", () => {
  assert.throws(() => generationInput({ kind: "draft", count: 1, resolved_prompt: "Test" }));
  assert.throws(() => generationInput({ kind: "outline", count: 2, resolved_prompt: "Test" }));
});
test("prompt retains resolved context and legacy title limits", () => {
  const prompt = providerPrompt(generationInput({ kind: "titles", count: 2, resolved_prompt: "The user's exact approved context" }));
  assert.match(prompt, /40–75/); assert.match(prompt, /140/); assert.match(prompt, /exact approved context/);
});
test("candidate parsing accepts JSON fences, normalizes spaces and respects Unicode", () => {
  const input = generationInput({ kind: "titles", count: 2, resolved_prompt: "Test" });
  assert.deepEqual(parseCandidates('```json\n{"candidates":["One   title", "Two"]}\n```', input), ["One title", "Two"]);
  assert.equal([...parseCandidates(JSON.stringify({ candidates: ["😀".repeat(140)] }), input)[0]].length, 140);
});
test("invalid, duplicated and overlong output is rejected rather than silently truncated", () => {
  const input = generationInput({ kind: "titles", count: 2, resolved_prompt: "Test" });
  for (const output of ["1. A title", '{"candidates":[]}', '{"candidates":["Same","same"]}', JSON.stringify({ candidates: ["x".repeat(141)] })]) assert.throws(() => parseCandidates(output, input));
});
test("route catalog excludes generic paid OpenAI routes and implicit alias inference", () => {
  assert.throws(() => generationRoutes([{ ...route, provider: "openai" }]));
  assert.throws(() => generationRoutes([{ ...route, response_models: [] }]));
  assert.throws(() => generationRoutes([{ ...route, provider: "codex", transport: "direct" }]));
  assert.throws(() => generationRoutes([{ ...route, transport: "omniroute", model: "openai/gpt-test" }]));
});
test("reviewers, pending and disabled accounts cannot access generation", async () => {
  const f = fixture();
  for (const changed of [{ role: "reviewer" }, { status: "pending" }, { status: "disabled" }]) assert.equal((await f.admin("/routes", "GET", undefined, { actor: { ...actor, ...changed } })).status, 403);
});
test("admin mutations require same-origin JSON and bound body size", async () => {
  const f = fixture();
  assert.equal((await f.admin("/workspaces", "POST", { topic: "Test" }, { headers: { Origin: "https://other.example" } })).status, 403);
  assert.equal((await f.admin("/workspaces", "POST", { topic: "Test" }, { headers: { "Content-Type": "text/plain" } })).status, 400);
  assert.equal((await f.admin("/workspaces", "POST", { topic: "x".repeat(240_001) })).status, 413);
});
test("workspace imports an existing article ID without copying or touching frozen versions", async () => {
  const f = fixture(), id = crypto.randomUUID();
  f.native.prepare("INSERT INTO articles(id,created_by,created_at) VALUES(?,?,?)").run(id, actor.id, "now");
  f.native.prepare("INSERT INTO article_versions VALUES(?,?,1,?,'',?,'markdown',?,?,'now')").run(crypto.randomUUID(), id, "Frozen title", "Frozen body", "<p>Frozen body</p>", "Frozen body");
  assert.equal((await f.admin("/workspaces", "POST", { article_id: id, topic: "Working title" })).status, 201);
  assert.equal(f.native.prepare("SELECT COUNT(*) n FROM articles").get().n, 1);
  assert.equal(f.native.prepare("SELECT body FROM article_versions").get().body, "Frozen body");
});
test("safe saves preserve exact draft whitespace and reject stale revisions", async () => {
  const f = fixture(), article = await f.create();
  const input = { revision: 0, topic: "Topic", brief: "Brief", evidence: "Source", draft_body: "  Draft\n\n" };
  assert.equal((await f.admin(`/workspaces/${article}`, "PUT", input)).status, 200);
  assert.equal((await f.admin(`/workspaces/${article}`, "PUT", { ...input, draft_body: "stale" })).status, 409);
  const saved = await f.admin(`/workspaces/${article}`); assert.equal(saved.data.workspace.draft_body, input.draft_body);
});
test("jobs fail closed without an explicitly configured runner", async () => {
  const f = fixture(), article = await f.create(); f.env.ARTICLE_LAB_RUNNER_TOKEN = "";
  assert.equal((await f.enqueue(article)).status, 503);
});
test("queue admission is idempotent and rejects conflicting request IDs", async () => {
  const f = fixture(), article = await f.create(), id = crypto.randomUUID();
  assert.equal((await f.enqueue(article, { id })).status, 202);
  assert.equal((await f.enqueue(article, { id })).status, 200);
  assert.equal((await f.enqueue(article, { id, resolved_prompt: "Changed" })).status, 409);
  assert.equal(f.native.prepare("SELECT COUNT(*) n FROM generation_jobs").get().n, 1);
});
test("per-article queue cap limits repeated clicks", async () => {
  const f = fixture(), article = await f.create();
  for (let i = 0; i < 3; i++) assert.equal((await f.enqueue(article)).status, 202);
  assert.equal((await f.enqueue(article)).status, 409);
});
test("runner auth is separate from browser identity and rejects Origin headers", async () => {
  const f = fixture(), payload = { runner_id: "test", route_ids: [route.id] };
  assert.equal((await f.machine("/claim", payload, { Authorization: "Bearer wrong" })).status, 401);
  assert.equal((await f.machine("/claim", payload, { Origin: origin })).status, 403);
});
test("only configured matching routes can claim; one job is claimed once", async () => {
  const f = fixture(), article = await f.create(); await f.enqueue(article);
  assert.equal((await f.machine("/claim", { runner_id: "test", route_ids: ["not-configured"] })).data.job, null);
  const jobs = await Promise.all([f.claim(), f.claim()]);
  assert.equal(jobs.filter((result) => result.data.job).length, 1);
  const view = await f.admin(`/workspaces/${article}`); assert.equal("lease_token" in view.data.jobs[0], false);
});
test("revoked requester cannot start a queued generation", async () => {
  const f = fixture(), article = await f.create(); await f.enqueue(article);
  f.native.prepare("UPDATE users SET status='disabled' WHERE id=?").run(actor.id);
  assert.equal((await f.claim()).data.job, null);
});
test("changed route identity is not silently substituted", async () => {
  const f = fixture(), article = await f.create(); await f.enqueue(article);
  f.env.ARTICLE_LAB_ROUTES = JSON.stringify([{ ...route, model: "glm-other", response_models: ["glm-other"] }]);
  assert.equal((await f.claim()).data.job, null);
  assert.equal(f.native.prepare("SELECT error_code FROM generation_jobs").get().error_code, "route_changed");
});
test("completion persists alternatives, does not select or publish automatically, and is idempotent", async () => {
  const f = fixture(), article = await f.create(); await f.enqueue(article); const job = (await f.claim()).data.job;
  assert.equal((await f.complete(job)).status, 200);
  assert.equal((await f.complete(job)).data.duplicate, true);
  assert.equal(f.native.prepare("SELECT COUNT(*) n FROM generation_candidates").get().n, 2);
  assert.equal(f.native.prepare("SELECT COUNT(*) n FROM article_selections").get().n, 0);
  assert.equal(f.native.prepare("SELECT COUNT(*) n FROM article_versions").get().n, 0);
  assert.equal((await f.complete(job, { output: '{"candidates":["Different"]}' })).status, 409);
});
test("completion requires the matching lease and a qualified returned model", async () => {
  const f = fixture(), article = await f.create(); await f.enqueue(article); const job = (await f.claim()).data.job;
  assert.equal((await f.complete(job, { lease_token: crypto.randomUUID() })).status, 409);
  assert.equal((await f.complete(job, { actual_model: "unqualified" })).status, 409);
  assert.equal(f.native.prepare("SELECT COUNT(*) n FROM generation_candidates").get().n, 0);
});
test("expired jobs become uncertain, never automatically queued again", async () => {
  const f = fixture(), article = await f.create(); await f.enqueue(article); const job = (await f.claim()).data.job;
  f.native.prepare("UPDATE generation_jobs SET lease_expires_at='2000-01-01' WHERE id=?").run(job.id);
  assert.equal((await f.claim()).data.job, null);
  assert.equal(f.native.prepare("SELECT status FROM generation_jobs").get().status, "uncertain");
  assert.equal((await f.complete(job)).status, 409);
});
test("selection is version checked; selected candidates cannot be archived", async () => {
  const f = fixture(), article = await f.create(); await f.enqueue(article); await f.complete((await f.claim()).data.job);
  const candidates = (await f.admin(`/workspaces/${article}`)).data.candidates;
  const select = (id, revision) => f.admin(`/workspaces/${article}/selections`, "PUT", { candidate_id: id, revision });
  assert.equal((await select(candidates[0].id, 0)).status, 200);
  assert.equal((await select(candidates[1].id, 0)).status, 409);
  assert.equal((await f.admin(`/workspaces/${article}/candidates`, "PATCH", { candidate_id: candidates[0].id, archived: true })).status, 409);
  assert.equal((await select(candidates[1].id, 1)).status, 200);
  assert.equal((await f.admin(`/workspaces/${article}/candidates`, "PATCH", { candidate_id: candidates[0].id, archived: true })).status, 200);
  assert.equal((await f.admin(`/workspaces/${article}/candidates`, "PATCH", { candidate_id: candidates[0].id, archived: false })).status, 200);
  assert.throws(() => f.native.prepare("UPDATE generation_candidates SET value='corrupted' WHERE id=?").run(candidates[0].id));
});
test("candidates cannot be selected across article boundaries", async () => {
  const f = fixture(), a = await f.create(), b = await f.create(); await f.enqueue(a); await f.complete((await f.claim()).data.job);
  const candidate = (await f.admin(`/workspaces/${a}`)).data.candidates[0];
  assert.equal((await f.admin(`/workspaces/${b}/selections`, "PUT", { candidate_id: candidate.id, revision: 0 })).status, 404);
  assert.throws(() => f.native.prepare("INSERT INTO article_selections VALUES(?,?,?,?)").run(b, "titles", candidate.id, "now"));
});
test("transport uncertainty records a visible safe error code, not provider payloads", async () => {
  const f = fixture(), article = await f.create(); await f.enqueue(article); const job = (await f.claim()).data.job;
  assert.equal((await f.machine(`/jobs/${job.id}/fail`, { lease_token: job.lease_token, code: "transport_uncertain" })).status, 200);
  const view = (await f.admin(`/workspaces/${article}`)).data;
  assert.equal(view.jobs[0].status, "uncertain"); assert.equal(view.jobs[0].error_code, "transport_uncertain");
});

test("only queued jobs can be cancelled, without a provider request", async () => {
  const f = fixture(), article = await f.create(), queued = await f.enqueue(article);
  assert.equal((await f.admin(`/workspaces/${article}/jobs`, "PATCH", { id: queued.data.id, action: "cancel" })).status, 200);
  assert.equal((await f.claim()).data.job, null);
  await f.enqueue(article); const job = (await f.claim()).data.job;
  assert.equal((await f.admin(`/workspaces/${article}/jobs`, "PATCH", { id: job.id, action: "cancel" })).status, 409);
});
