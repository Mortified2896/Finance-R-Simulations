import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { validateLocalRoutes, executeTextJob, generateImage } from "../../scripts/article_lab_worker/providers.mjs";
import { runOnce, replayCompletion, validateOrigin } from "../../scripts/article_lab_worker/runner.mjs";
const glm = { id: "glm-direct", label: "GLM test", provider: "glm", transport: "direct", model: "glm-test", response_models: ["glm-test"], base_url: "https://api.z.ai/api/coding/paas/v4", api_key_env: "ZAI_CODING_API_KEY" };
const codex = { id: "codex-omniroute", label: "Codex test", provider: "codex", transport: "omniroute", model: "codex/gpt-test", response_models: ["gpt-test"], base_url: "http://127.0.0.1:9999/v1", api_key_env: "OMNIROUTE_API_KEY" };
const glmGateway = { ...glm, id: "glm-omniroute", transport: "omniroute", model: "glm/glm-test", base_url: "http://127.0.0.1:9999/v1", api_key_env: "OMNIROUTE_API_KEY" };
const env = { ZAI_CODING_API_KEY: "synthetic-glm-key", OMNIROUTE_API_KEY: "synthetic-gateway-key", OPENAI_API_KEY: "must-not-be-used-for-text", ARTICLE_LAB_RUNNER_TOKEN: "synthetic-runner-token-".repeat(3) };
const makeJob = (route = glm) => ({ id: crypto.randomUUID(), article_id: crypto.randomUUID(), lease_token: crypto.randomUUID(), kind: "titles", count: 2, resolved_prompt: "A supplied evidence-based brief", route: validateLocalRoutes([route])[0].route });
const chat = (extra = {}) => new Response(JSON.stringify({ model: "glm-test", choices: [{ finish_reason: "stop", message: { content: '{"candidates":["A clear title","Another clear title"]}' } }], ...extra }));
const response = (data) => new Response(JSON.stringify(data));

test("direct GLM uses the existing coding subscription endpoint, not the balance endpoint", async () => {
  const calls = [], job = makeJob();
  const result = await executeTextJob(job, validateLocalRoutes([glm]), { env, fetcher: async (url, options) => { calls.push({ url, options }); return chat(); } });
  assert.equal(calls.length, 1); assert.equal(calls[0].url, "https://api.z.ai/api/coding/paas/v4/chat/completions");
  assert.equal(calls[0].options.headers.Authorization, "Bearer synthetic-glm-key");
  assert.equal(JSON.parse(calls[0].options.body).model, "glm-test"); assert.equal(result.actual_model, "glm-test");
});
test("GLM OmniRoute preserves the qualified glm/ model ID", async () => {
  await executeTextJob(makeJob(glmGateway), validateLocalRoutes([glmGateway]), { env, fetcher: async (url, options) => {
    assert.equal(url, "http://127.0.0.1:9999/v1/chat/completions"); assert.equal(JSON.parse(options.body).model, "glm/glm-test"); return chat();
  }});
});
test("Codex uses the configured subscription route through Responses without tools or API-key fallback", async () => {
  const result = await executeTextJob(makeJob(codex), validateLocalRoutes([codex]), { env, fetcher: async (url, options) => {
    const payload = JSON.parse(options.body);
    assert.equal(url, "http://127.0.0.1:9999/v1/responses"); assert.equal(payload.model, "codex/gpt-test"); assert.equal(payload.store, false);
    assert.equal(payload.tools, undefined); assert.equal(options.headers.Authorization, "Bearer synthetic-gateway-key");
    return response({ model: "gpt-test", status: "completed", output: [{ type: "message", content: [{ type: "output_text", text: '{"candidates":["First","Second"]}' }] }] });
  }}); assert.equal(result.actual_model, "gpt-test");
});
test("text adapters reject generic OpenAI API credential configuration", () => assert.throws(() => validateLocalRoutes([{ ...glm, api_key_env: "OPENAI_API_KEY" }])));
test("nonlocal gateway URLs, URL credentials and the general GLM balance endpoint are rejected", () => {
  for (const route of [{ ...codex, base_url: "https://example.test/v1" }, { ...codex, base_url: "http://secret@localhost/v1" }, { ...glm, base_url: "https://api.z.ai/api/paas/v4" }]) assert.throws(() => validateLocalRoutes([route]));
});
test("missing credentials fail before any provider request and do not read OPENAI_API_KEY", async () => {
  let calls = 0;
  await assert.rejects(() => executeTextJob(makeJob(), validateLocalRoutes([glm]), { env: { OPENAI_API_KEY: "not-a-fallback" }, fetcher: async () => { calls++; return chat(); } }), { code: "runner_configuration" });
  assert.equal(calls, 0);
});
test("job cannot change endpoint/model or silently switch routes", async () => {
  let calls = 0; const job = makeJob(); job.route.model = "glm-other";
  await assert.rejects(() => executeTextJob(job, validateLocalRoutes([glm]), { env, fetcher: async () => { calls++; return chat(); } }), { code: "route_mismatch" });
  assert.equal(calls, 0);
});
test("provider errors and ambiguous transport failures are not retried or exposed", async () => {
  for (const [status, code] of [[429, "provider_rejected"], [502, "transport_uncertain"]]) {
    let calls = 0;
    await assert.rejects(() => executeTextJob(makeJob(), validateLocalRoutes([glm]), { env, fetcher: async () => { calls++; return new Response("secret-provider-payload", { status }); } }), (error) => error.code === code && !error.message.includes("secret"));
    assert.equal(calls, 1);
  }
  await assert.rejects(() => executeTextJob(makeJob(), validateLocalRoutes([glm]), { env, fetcher: async () => { throw new Error("private url/token"); } }), { code: "transport_uncertain" });
});
test("unqualified actual models, tool calls, truncated output and broken JSON are rejected", async () => {
  for (const [data, code] of [
    [{ model: "wrong" }, "route_mismatch"],
    [{ choices: [{ finish_reason: "length", message: { content: "partial" } }] }, "invalid_output"],
    [{ choices: [{ finish_reason: "stop", message: { content: "not JSON" } }] }, "invalid_output"],
    [{ choices: [{ finish_reason: "stop", message: { content: '{}', tool_calls: [{}] } }] }, "invalid_output"],
  ]) await assert.rejects(() => executeTextJob(makeJob(), validateLocalRoutes([glm]), { env, fetcher: async () => chat(data) }), { code });
});
test("image adapter makes one explicitly configured, separate OpenAI API call", async () => {
  const bytes = Buffer.from([137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 0]); let calls = 0;
  const result = await generateImage({ model: "configured-image-model", prompt: "A minimalist editorial thumbnail" }, { apiKey: "synthetic-image-api-key", fetcher: async (url, options) => {
    calls++; assert.equal(url, "https://api.openai.com/v1/images/generations");
    const payload = JSON.parse(options.body); assert.equal(payload.n, 1); assert.equal(payload.model, "configured-image-model");
    assert.equal(options.headers.Authorization, "Bearer synthetic-image-api-key"); return response({ data: [{ b64_json: bytes.toString("base64") }] });
  }}); assert.equal(calls, 1); assert.equal(result.content_type, "image/png"); assert.deepEqual(result.bytes, bytes);
});
test("image adapter rejects non-image bytes and has no implicit model selection", async () => {
  await assert.rejects(() => generateImage({ model: "configured-image-model", prompt: "Test" }, { apiKey: "key", fetcher: async () => response({ data: [{ b64_json: Buffer.from("<script>bad</script>").toString("base64") }] }) }), { code: "invalid_output" });
  await assert.rejects(() => generateImage({ prompt: "Test" }, { apiKey: "key" }));
});
test("the image lane is explicit: disabled without a distinct image key or config flag", async () => {
  const spool = await fs.mkdtemp(path.join(os.tmpdir(), "article-lab-test-"));
  const calls = [];
  try {
    for (const [extraEnv, imageConfig] of [[{}, { enabled: true }], [{ OPENAI_IMAGE_API_KEY: env.OPENAI_API_KEY }, { enabled: true }], [{ OPENAI_IMAGE_API_KEY: "synthetic-image-key" }, { enabled: false }]]) {
      await runOnce({ origin: "https://lab.example", runner_id: "test", spool_dir: spool, routes: [glm], images: imageConfig }, { env: { ...env, ...extraEnv }, fetcher: async (url) => {
        calls.push(url);
        if (url.endsWith("/heartbeat")) return response({ ok: true });
        return response({ job: null });
      } });
    }
    assert.equal(calls.filter((url) => url.endsWith("/images/claim")).length, 0);
    assert.deepEqual(await fs.readdir(spool), []);
  } finally { await fs.rm(spool, { recursive: true, force: true }); }
});
test("image jobs generate once from the separate image key and spool before upload", async () => {
  const spool = await fs.mkdtemp(path.join(os.tmpdir(), "article-lab-test-"));
  const imageJob = { id: crypto.randomUUID(), article_id: crypto.randomUUID(), lease_token: crypto.randomUUID(), prompt: "A minimalist editorial thumbnail", model: "configured-image-model", size: "1536x1024", quality: "medium" };
  const png = Buffer.from([137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 2, 128, 0, 0, 1, 144, 8, 6, 0, 0, 0]);
  try {
    const result = await runOnce({ origin: "https://lab.example", runner_id: "test", spool_dir: spool, routes: [glm], images: { enabled: true } }, { env: { ...env, OPENAI_IMAGE_API_KEY: "synthetic-image-key" }, fetcher: async (url, options) => {
      if (url.endsWith("/heartbeat")) return response({ ok: true });
      if (url.endsWith("/images/claim")) return response({ job: imageJob });
      if (url.endsWith("/claim")) return response({ job: null });
      if (url.includes("api.openai.com")) {
        assert.equal(JSON.parse(options.body).model, "configured-image-model");
        assert.equal(options.headers.Authorization, "Bearer synthetic-image-key");
        return response({ data: [{ b64_json: png.toString("base64") }] });
      }
      const payload = JSON.parse(options.body);
      assert.equal(payload.lease_token, imageJob.lease_token);
      assert.equal(JSON.stringify(payload).includes("synthetic-image-key"), false);
      return new Response("offline", { status: 503 });
    }});
    assert.equal(result.status, "completion_pending");
    const spooled = JSON.parse(await fs.readFile(result.saved, "utf8"));
    assert.equal(spooled.kind, "image");
    assert.equal(spooled.result.image_base64.length > 0, true);
    await replayCompletion({ origin: "https://lab.example", runner_id: "test", spool_dir: spool, routes: [glm] }, result.saved, { env: { ...env, OPENAI_IMAGE_API_KEY: "synthetic-image-key" }, fetcher: async (url) => {
      assert.match(url, /\/images\/jobs\/[0-9a-f-]+\/complete$/);
      return response({ ok: true });
    } });
    assert.deepEqual(await fs.readdir(spool), []);
  } finally { await fs.rm(spool, { recursive: true, force: true }); }
});
test("image provider failures are reported once and never retried or spooled", async () => {
  const spool = await fs.mkdtemp(path.join(os.tmpdir(), "article-lab-test-"));
  const imageJob = { id: crypto.randomUUID(), article_id: crypto.randomUUID(), lease_token: crypto.randomUUID(), prompt: "x", model: "configured-image-model", size: "1536x1024", quality: "medium" };
  try {
    const result = await runOnce({ origin: "https://lab.example", runner_id: "test", spool_dir: spool, routes: [glm], images: { enabled: true } }, { env: { ...env, OPENAI_IMAGE_API_KEY: "synthetic-image-key" }, fetcher: async (url) => {
      if (url.endsWith("/heartbeat")) return response({ ok: true });
      if (url.endsWith("/images/claim")) return response({ job: imageJob });
      if (url.endsWith("/claim")) return response({ job: null });
      if (url.includes("api.openai.com")) return new Response("quota", { status: 429 });
      assert.match(url, /\/images\/jobs\/[0-9a-f-]+\/fail$/);
      return response({ ok: true });
    }});
    assert.equal(result.status, "image_failed");
    assert.deepEqual(await fs.readdir(spool), []);
  } finally { await fs.rm(spool, { recursive: true, force: true }); }
});
test("runner requires a bare HTTPS origin; credentials cannot be redirected", () => {
  for (const url of ["http://lab.example", "https://secret@lab.example", "https://lab.example/path", "https://lab.example/?token=x"]) assert.throws(() => validateOrigin(url));
});
test("runner claims once, generates once and uploads a result without leaking keys", async () => {
  const spool = await fs.mkdtemp(path.join(os.tmpdir(), "article-lab-test-")), job = makeJob(), calls = [];
  try {
    const result = await runOnce({ origin: "https://lab.example", runner_id: "test", spool_dir: spool, routes: [glm] }, { env, fetcher: async (url, options) => {
      calls.push(url);
      if (url.endsWith("/heartbeat")) { assert.match(options.body, /"image_ready":false/); return response({ ok: true }); }
      if (url.endsWith("/claim")) { assert.equal(options.headers.Origin, undefined); return response({ job }); }
      if (url.includes("api.z.ai")) return chat();
      const payload = JSON.parse(options.body); assert.equal(payload.actual_model, "glm-test"); assert.equal(JSON.stringify(payload).includes("synthetic-glm-key"), false);
      return response({ ok: true });
    }});
    assert.equal(result.status, "succeeded"); assert.equal(calls.length, 4); assert.deepEqual(await fs.readdir(spool), []);
  } finally { await fs.rm(spool, { recursive: true, force: true }); }
});
test("failed completion leaves a private spool and replay never calls the provider", async () => {
  const spool = await fs.mkdtemp(path.join(os.tmpdir(), "article-lab-test-")), job = makeJob();
  const config = { origin: "https://lab.example", runner_id: "test", spool_dir: spool, routes: [glm] };
  try {
    const result = await runOnce(config, { env, fetcher: async (url) => url.endsWith("/heartbeat") ? response({ ok: true }) : url.endsWith("/claim") ? response({ job }) : url.includes("api.z.ai") ? chat() : new Response("offline", { status: 503 }) });
    assert.equal(result.status, "completion_pending"); assert.equal((await fs.stat(result.saved)).mode & 0o777, 0o600);
    assert.equal((await fs.stat(spool)).mode & 0o777, 0o700);
    let calls = 0;
    await replayCompletion(config, result.saved, { env, fetcher: async (url) => { calls++; assert.match(url, /\/complete$/); assert.equal(url.includes("api.z.ai"), false); return response({ ok: true }); } });
    assert.equal(calls, 1); assert.deepEqual(await fs.readdir(spool), []);
  } finally { await fs.rm(spool, { recursive: true, force: true }); }
});
