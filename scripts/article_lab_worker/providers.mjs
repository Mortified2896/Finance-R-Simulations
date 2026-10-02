import { generationInput, generationRoutes, parseCandidates, providerPrompt, record, text, uuid } from "../../apps/article_lab_web/shared/generation.ts";

export class ProviderFailure extends Error {
  constructor(code) { super(code); this.code = code; }
}
export function validateLocalRoutes(value) {
  if (!Array.isArray(value)) throw new ProviderFailure("runner_configuration");
  const publicRoutes = generationRoutes(value);
  return publicRoutes.map((route, i) => {
    const row = record(value[i]);
    const url = new URL(text(row.base_url, "base URL", 400));
    if (url.username || url.password || url.search || url.hash) throw new ProviderFailure("runner_configuration");
    if (route.transport === "direct") {
      if (url.href.replace(/\/$/, "") !== "https://api.z.ai/api/coding/paas/v4") throw new ProviderFailure("runner_configuration");
    } else {
      // A local RTX worker can reach its existing OmniRoute listener directly.
      // Never forward its gateway credential to an arbitrary public endpoint.
      if (!["127.0.0.1", "localhost", "[::1]"].includes(url.hostname) || !["http:", "https:"].includes(url.protocol) || !/^\/v1\/?$/.test(url.pathname)) throw new ProviderFailure("runner_configuration");
    }
    const keyEnv = text(row.api_key_env, "credential environment variable", 100);
    if (!/^[A-Z][A-Z0-9_]*$/.test(keyEnv) || keyEnv === "OPENAI_API_KEY") throw new ProviderFailure("runner_configuration");
    return { route, base_url: url.href.replace(/\/$/, ""), api_key_env: keyEnv };
  });
}
export async function readJsonResponse(response, max = 1_000_000) {
  if (!response.ok) throw new ProviderFailure(response.status >= 500 ? "transport_uncertain" : "provider_rejected");
  const reader = response.body?.getReader();
  if (!reader) throw new ProviderFailure("invalid_output");
  const chunks = []; let size = 0;
  for (;;) {
    const { value, done } = await reader.read(); if (done) break;
    size += value.byteLength;
    if (size > max) { await reader.cancel(); throw new ProviderFailure("invalid_output"); }
    chunks.push(value);
  }
  try { return JSON.parse(Buffer.concat(chunks).toString("utf8")); }
  catch { throw new ProviderFailure("invalid_output"); }
}
export async function executeTextJob(job, localRoutes, { env = process.env, fetcher = fetch } = {}) {
  uuid(job.id); uuid(job.article_id); uuid(job.lease_token);
  const input = generationInput(job), route = generationRoutes([job.route])[0];
  const configured = localRoutes.find((item) => item.route.id === route.id);
  if (!configured || JSON.stringify(configured.route) !== JSON.stringify(route)) throw new ProviderFailure("route_mismatch");
  const key = env[configured.api_key_env]?.trim();
  if (!key) throw new ProviderFailure("runner_configuration");
  const prompt = providerPrompt(input), codex = route.provider === "codex";
  const payload = codex
    ? { model: route.model, input: prompt, stream: false, store: false }
    : { model: route.model, messages: [{ role: "user", content: prompt }], stream: false, response_format: { type: "json_object" } };
  let data;
  try {
    const response = await fetcher(`${configured.base_url}/${codex ? "responses" : "chat/completions"}`, {
      method: "POST", redirect: "error", signal: AbortSignal.timeout(300_000),
      headers: { "Authorization": `Bearer ${key}`, "Content-Type": "application/json" }, body: JSON.stringify(payload),
    });
    data = await readJsonResponse(response);
  } catch (error) { if (error instanceof ProviderFailure) throw error; throw new ProviderFailure("transport_uncertain"); }
  if (!data || !route.response_models.includes(data.model)) throw new ProviderFailure("route_mismatch");
  let output;
  if (codex) {
    if (data.status && data.status !== "completed") throw new ProviderFailure("invalid_output");
    if (data.output?.some((entry) => !["message", "reasoning"].includes(entry.type))) throw new ProviderFailure("invalid_output");
    output = data.output_text ?? (data.output ?? []).flatMap((entry) => entry.content ?? [])
      .filter((content) => content.type === "output_text").map((content) => content.text).join("\n");
  } else {
    const choice = data.choices?.[0];
    if (choice?.finish_reason !== "stop" || choice.message?.tool_calls?.length) throw new ProviderFailure("invalid_output");
    output = choice.message?.content;
  }
  try { output = JSON.stringify({ candidates: parseCandidates(output, input) }); }
  catch { throw new ProviderFailure("invalid_output"); }
  return { lease_token: job.lease_token, actual_model: data.model, output };
}

/** Image pixels are a separately billed, explicitly configured OpenAI API call. */
export async function generateImage({ prompt, model, size = "1536x1024", quality = "medium" }, { apiKey, fetcher = fetch } = {}) {
  text(prompt, "image prompt", 12_000); text(model, "image model", 160);
  if (!apiKey || !["1024x1024", "1536x1024", "1024x1536"].includes(size) || !["low", "medium", "high"].includes(quality)) throw new ProviderFailure("runner_configuration");
  let data;
  try {
    data = await readJsonResponse(await fetcher("https://api.openai.com/v1/images/generations", {
      method: "POST", redirect: "error", signal: AbortSignal.timeout(300_000),
      headers: { "Authorization": `Bearer ${apiKey}`, "Content-Type": "application/json" },
      body: JSON.stringify({ model, prompt, size, quality, n: 1, output_format: "png" }),
    }), 16_000_000);
  } catch (error) { if (error instanceof ProviderFailure) throw error; throw new ProviderFailure("transport_uncertain"); }
  const encoded = data?.data?.[0]?.b64_json;
  if (typeof encoded !== "string" || encoded.length > 15_000_000 || !/^[A-Za-z0-9+/]+={0,2}$/.test(encoded)) throw new ProviderFailure("invalid_output");
  const bytes = Buffer.from(encoded, "base64");
  if (bytes.length > 10_000_000 || !bytes.subarray(0, 8).equals(Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]))) throw new ProviderFailure("invalid_output");
  return { bytes, content_type: "image/png", requested_model: model };
}
