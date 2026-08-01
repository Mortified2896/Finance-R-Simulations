import "dotenv/config";
import fs from "node:fs/promises";
import { createOpenAIClient, flushLangfuse, withLangfuseRun } from "./langfuse.mjs";

const DEFAULT_MODEL = "gpt-5.5";
const DEFAULT_VARIANTS = 3;
const VALID_SIZES = new Set(["1024x1024", "1536x1024", "1024x1536", "auto"]);
const VALID_QUALITIES = new Set(["low", "medium", "high", "auto"]);
const VALID_FORMATS = new Set(["png", "webp", "jpeg"]);
const VALID_BACKGROUNDS = new Set(["transparent", "opaque", "auto"]);

function cleanText(value) {
  if (value === null || value === undefined) return null;
  const normalized = String(value).replace(/\u00a0/g, " ").replace(/\s+/g, " ").trim();
  return normalized.length > 0 ? normalized : null;
}

function assertOption(value, allowed, label) {
  if (!allowed.has(value)) throw new Error(`Invalid ${label}: ${value}. Allowed values: ${[...allowed].join(", ")}.`);
}

function normalizeSettings(payload) {
  const outputFormat = cleanText(payload.output_format) ?? "png";
  const background = cleanText(payload.background) ?? "auto";
  const size = cleanText(payload.size) ?? "1536x1024";
  const quality = cleanText(payload.quality) ?? "auto";
  assertOption(size, VALID_SIZES, "image size");
  assertOption(quality, VALID_QUALITIES, "image quality");
  assertOption(outputFormat, VALID_FORMATS, "output format");
  assertOption(background, VALID_BACKGROUNDS, "background");
  if (background === "transparent" && outputFormat === "jpeg") {
    throw new Error("Transparent backgrounds require PNG or WebP output; JPEG is not supported.");
  }
  let outputCompression = null;
  if (payload.output_compression !== null && payload.output_compression !== undefined && payload.output_compression !== "") {
    outputCompression = Number(payload.output_compression);
    if (!Number.isInteger(outputCompression) || outputCompression < 0 || outputCompression > 100) {
      throw new Error("Output compression must be an integer from 0 to 100.");
    }
    if (outputFormat === "png") throw new Error("Output compression is supported only for WebP and JPEG output.");
  }
  if (payload.streaming === true) throw new Error("Streaming thumbnail generation is not implemented for this workflow.");
  if (payload.partial_images !== null && payload.partial_images !== undefined) {
    throw new Error("Partial-image previews require streaming, which is not implemented for this workflow.");
  }
  return { size, quality, output_format: outputFormat, output_compression: outputCompression, background };
}

function normalizeRequests(values) {
  return Array.isArray(values) ? values.map((entry) => ({
    subtitle_id: cleanText(entry.subtitle_id), candidate_id: cleanText(entry.candidate_id),
    batch_id: cleanText(entry.batch_id), title: cleanText(entry.title), subtitle: cleanText(entry.subtitle),
    variant_index: Number.parseInt(entry.variant_index, 10), resolved_prompt: typeof entry.resolved_prompt === "string" ? entry.resolved_prompt : null
  })).filter((entry) => entry.subtitle_id && entry.candidate_id && entry.batch_id && entry.title && entry.subtitle && Number.isInteger(entry.variant_index) && entry.resolved_prompt !== null) : [];
}

export function buildThumbnailRequests(payload) {
  const promptRequests = normalizeRequests(payload.requests);
  if (!promptRequests.length) throw new Error("At least one resolved thumbnail prompt request is required.");
  const model = cleanText(payload.model) ?? DEFAULT_MODEL;
  const reasoningEffort = cleanText(payload.reasoning_effort);
  const reasoningMode = cleanText(payload.reasoning_mode) ?? "standard";
  const variantsPerPackage = Math.max(1, Math.min(4, Number.parseInt(payload.variants_per_package, 10) || DEFAULT_VARIANTS));
  const image = normalizeSettings(payload);
  const records = [];
  for (const promptRequest of promptRequests) {
      const pkg = promptRequest;
      const variantIndex = promptRequest.variant_index;
      const submittedPrompt = promptRequest.resolved_prompt;
      const tool = { type: "image_generation", action: "generate", size: image.size, quality: image.quality, output_format: image.output_format, background: image.background };
      if (image.output_compression !== null) tool.output_compression = image.output_compression;
      const request = { model, input: submittedPrompt, tools: [tool], tool_choice: { type: "image_generation" }, stream: false };
      if (reasoningEffort) request.reasoning = { effort: reasoningEffort };
      if (reasoningMode === "pro") request.reasoning = { ...(request.reasoning ?? {}), mode: "pro" };
      records.push({ pkg, variant_index: variantIndex, submitted_prompt: submittedPrompt, request });
  }
  return { model, reasoning_effort: reasoningEffort, reasoning_mode: reasoningMode, image_settings: image, records };
}

function sanitizeRequest(request) {
  return JSON.parse(JSON.stringify(request, (key, value) => {
    if (/api[_-]?key|authorization|secret/i.test(key)) return undefined;
    if (typeof value === "string" && /^data:image\//.test(value)) return "[image data omitted]";
    return value;
  }));
}

export function buildThumbnailPreview(payload) {
  const built = buildThumbnailRequests(payload);
  return {
    operation: "client.responses.create",
    endpoint: "POST /v1/responses",
    request_count: built.records.length,
    requests: built.records.map((record) => ({
      subtitle_id: record.pkg.subtitle_id,
      candidate_id: record.pkg.candidate_id,
      batch_id: record.pkg.batch_id,
      variant_index: record.variant_index,
      sanitized_request: sanitizeRequest(record.request),
      property_status: {
        output_compression: record.request.tools[0].output_compression === undefined ? "Omitted — OpenAI API default" : "Included",
        execution_mode: payload.execution_mode_supported === false ? "Not supported by selected model" : (record.request.reasoning?.mode === undefined ? "Omitted — OpenAI API default" : "Included"),
        partial_images: "Not supported by selected model or workflow: streaming is disabled"
      }
    }))
  };
}

function extractImages(response) {
  const images = [];
  for (const item of response.output ?? []) {
    if (item.type === "image_generation_call" && item.result) images.push({ b64: item.result, output_id: item.id ?? null, call_id: item.call_id ?? item.id ?? null, revised_prompt: item.revised_prompt ?? null });
    for (const content of item.content ?? []) if (content.type === "output_image" && content.image_base64) images.push({ b64: content.image_base64, output_id: item.id ?? null, call_id: item.call_id ?? item.id ?? null, revised_prompt: content.revised_prompt ?? null });
  }
  return images;
}

async function main() {
  const previewOnly = process.argv.includes("--preview");
  const requestPath = process.argv.find((arg, index) => index > 1 && arg !== "--preview");
  if (!requestPath) throw new Error("Usage: node scripts/writing_api/generate_thumbnails.mjs [--preview] <request.json>");
  const payload = JSON.parse(await fs.readFile(requestPath, "utf8"));
  const built = buildThumbnailRequests(payload);
  if (previewOnly) {
    process.stdout.write(JSON.stringify(buildThumbnailPreview(payload)));
    return;
  }
  const apiKey = process.env.OPENAI_API_KEY?.trim();
  if (!apiKey || apiKey === "...") throw new Error("Missing OPENAI_API_KEY. Add it to the local .env file before using live thumbnail generation.");
  try {
    await withLangfuseRun({ name: "generate-medium-thumbnails-run", input: { packageCount: new Set(built.records.map((record) => record.pkg.subtitle_id)).size, variantsPerPackage: payload.variants_per_package, hasPrompt: built.records.every((record) => typeof record.submitted_prompt === "string") }, metadata: { script: "generateThumbnails", model: built.model, mode: "writingApi" }, tags: ["writing-api", "thumbnail-generation"], sessionId: requestPath, traceName: "generate-medium-thumbnails" }, async () => {
      const client = await createOpenAIClient(apiKey, { generationName: "generate-medium-thumbnails", generationMetadata: { requestCount: built.records.length }, tags: ["writing-api", "thumbnail-generation"], sessionId: requestPath });
      const grouped = new Map();
      const generationRunId = new Date().toISOString().replace(/[-:.TZ]/g, "").slice(0, 14);
      const generationRunLabel = new Date().toISOString().slice(11, 16) + " UTC";
      for (const record of built.records) {
        let response;
        try { response = await client.responses.create(record.request); }
        catch (error) { throw new Error(`OpenAI API failure for variant ${record.variant_index}: ${error.message}`); }
        const images = extractImages(response);
        if (!images.length) throw new Error(`OpenAI response ${response.id ?? "(no response ID)"} for variant ${record.variant_index} did not contain an image_generation result.`);
        const image = images[0];
        const key = record.pkg.subtitle_id;
        if (!grouped.has(key)) grouped.set(key, { ...record.pkg, thumbnails: [] });
        grouped.get(key).thumbnails.push({
          thumbnail_label: `API concept ${record.variant_index} · ${generationRunLabel}`,
          thumbnail_data_uri: `data:image/${built.image_settings.output_format === "jpeg" ? "jpeg" : built.image_settings.output_format};base64,${image.b64}`,
          created_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"), model: built.model,
          reasoning_effort: built.reasoning_effort, reasoning_mode: built.reasoning_mode, generation_mode: "api",
          submitted_prompt: record.submitted_prompt, revised_prompt: image.revised_prompt,
          response_id: response.id ?? null, image_generation_call_id: image.call_id,
          variant_index: record.variant_index, generation_run_id: generationRunId, image_settings: built.image_settings,
          raw_json: { response_id: response.id ?? null, output_id: image.output_id, call_id: image.call_id, revised_prompt: image.revised_prompt, submitted_prompt: record.submitted_prompt, effective_request: sanitizeRequest(record.request), usage: response.usage ?? null, model: built.model, variant_index: record.variant_index, generation_run_id: generationRunId }
        });
      }
      process.stdout.write(JSON.stringify({ mode: "api", model: built.model, reasoning_effort: built.reasoning_effort, reasoning_mode: built.reasoning_mode, results: [...grouped.values()] }));
    });
  } finally { await flushLangfuse(); }
}

const isMain = process.argv[1] && import.meta.url === new URL(`file://${process.argv[1]}`).href;
if (isMain) main().catch((error) => { console.error(error.message); process.exitCode = 1; });
