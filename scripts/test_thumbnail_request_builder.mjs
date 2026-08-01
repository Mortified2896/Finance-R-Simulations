import assert from "node:assert/strict";
import { buildThumbnailPreview, buildThumbnailRequests } from "./writing_api/generate_thumbnails.mjs";

const base = {
  model: "gpt-5.4-mini",
  reasoning_effort: "none",
  reasoning_mode: "standard",
  execution_mode_supported: false,
  prompt: "Package:\n{{input_context}}\nVariant {{variant_index}}/{{variants_per_package}}",
  variants_per_package: 3,
  size: "1536x1024",
  quality: "low",
  output_format: "webp",
  output_compression: 82,
  background: "opaque",
  streaming: false,
  partial_images: null,
  packages: [{ subtitle_id: "sub_1", candidate_id: "candidate_1", batch_id: "batch_1", title: "Title", subtitle: "Subtitle" }]
};

const built = buildThumbnailRequests(base);
assert.equal(built.records.length, 3, "candidate count must create independent requests");
for (const [index, record] of built.records.entries()) {
  assert.equal(record.variant_index, index + 1);
  assert.match(record.request.input, new RegExp(`Variant ${index + 1}/3`));
  assert.equal(record.request.model, base.model);
  assert.deepEqual(record.request.reasoning, { effort: "none" });
  assert.deepEqual(record.request.tool_choice, { type: "image_generation" });
  assert.equal(record.request.tools[0].type, "image_generation");
  assert.equal(record.request.tools[0].action, "generate");
  assert.equal(record.request.tools[0].size, base.size);
  assert.equal(record.request.tools[0].quality, base.quality);
  assert.equal(record.request.tools[0].output_format, base.output_format);
  assert.equal(record.request.tools[0].output_compression, base.output_compression);
  assert.equal(record.request.tools[0].background, base.background);
}

const preview = buildThumbnailPreview(base);
assert.equal(preview.request_count, 3);
assert.deepEqual(preview.requests.map((item) => item.sanitized_request), built.records.map((item) => item.request), "preview must exactly match canonical requests");
assert.equal(preview.requests[0].property_status.partial_images.includes("Not supported"), true);
assert.equal(preview.requests[0].property_status.execution_mode, "Not supported by selected model");

const png = buildThumbnailRequests({ ...base, variants_per_package: 1, output_format: "png", output_compression: null });
assert.equal("output_compression" in png.records[0].request.tools[0], false, "omitted settings must not be serialized as placeholders");
assert.equal(buildThumbnailPreview({ ...base, variants_per_package: 1, output_format: "png", output_compression: null }).requests[0].property_status.output_compression, "Omitted — OpenAI API default");

assert.throws(() => buildThumbnailRequests({ ...base, output_format: "png", output_compression: 80 }), /only for WebP and JPEG/);
assert.throws(() => buildThumbnailRequests({ ...base, output_format: "jpeg", background: "transparent" }), /require PNG or WebP/);
assert.throws(() => buildThumbnailRequests({ ...base, streaming: false, partial_images: 1 }), /require streaming/);
assert.throws(() => buildThumbnailRequests({ ...base, quality: "ultra" }), /Invalid image quality/);

const secretPreview = JSON.stringify(buildThumbnailPreview({ ...base, api_key: "sk-secret", packages: [{ ...base.packages[0], image: "data:image/png;base64,SECRET" }] }));
assert.equal(secretPreview.includes("sk-secret"), false);
assert.equal(secretPreview.includes("base64,SECRET"), false);

const responseFixture = {
  submitted_prompt: built.records[0].submitted_prompt,
  revised_prompt: "OpenAI revised prompt",
  response_id: "resp_123",
  image_generation_call_id: "ig_123",
  variant_index: 1
};
assert.notEqual(responseFixture.submitted_prompt, responseFixture.revised_prompt, "submitted and revised prompts must remain distinct metadata fields");

console.log("Thumbnail canonical request builder tests passed.");
