import "dotenv/config";
import fs from "node:fs/promises";
import { createOpenAIClient, flushLangfuse, withLangfuseRun } from "./langfuse.mjs";

const DEFAULT_MODEL = "gpt-5.5";
const DEFAULT_VARIANTS = 3;

function cleanText(value) {
  if (value === null || value === undefined) return null;
  const normalized = String(value).replace(/\u00a0/g, " ").replace(/\s+/g, " ").trim();
  return normalized.length > 0 ? normalized : null;
}

function extractImages(response) {
  const images = [];
  for (const item of response.output ?? []) {
    if (item.type === "image_generation_call" && item.result) {
      images.push({
        b64: item.result,
        output_id: item.id ?? null,
        call_id: item.call_id ?? null,
        revised_prompt: item.revised_prompt ?? null
      });
    }
    for (const content of item.content ?? []) {
      if (content.type === "output_image" && content.image_base64) {
        images.push({
          b64: content.image_base64,
          output_id: item.id ?? null,
          call_id: item.call_id ?? null,
          revised_prompt: content.revised_prompt ?? null
        });
      }
    }
  }
  return images;
}

function buildPrompt({ prompt, pkg, variantIndex, variantsPerPackage }) {
  const basePrompt = cleanText(prompt) || [
    "Generate Medium-style thumbnail candidates for approved title and subtitle packages.",
    "Keep the visual direction clear, editorial, credible, and readable at a glance.",
    "Create a finished thumbnail image suitable for a Medium preview card.",
    "Keep the concept aligned with the title and subtitle without adding clickbait or clutter."
  ].join("\n");

  return [
    basePrompt,
    "",
    "Create one 1200x720 editorial thumbnail image.",
    "Do not include logos, watermarks, fake UI chrome, or tiny unreadable text.",
    "If text appears in the image, keep it minimal and legible.",
    `Variant ${variantIndex} of ${variantsPerPackage}. Make this variant visually distinct from the others.`,
    "",
    `subtitle_id: ${pkg.subtitle_id}`,
    `candidate_id: ${pkg.candidate_id}`,
    `batch_id: ${pkg.batch_id}`,
    `Title: ${pkg.title}`,
    `Subtitle: ${pkg.subtitle}`
  ].join("\n");
}

function normalizePackages(values) {
  return Array.isArray(values)
    ? values
        .map((entry) => ({
          subtitle_id: cleanText(entry.subtitle_id),
          candidate_id: cleanText(entry.candidate_id),
          batch_id: cleanText(entry.batch_id),
          title: cleanText(entry.title),
          subtitle: cleanText(entry.subtitle)
        }))
        .filter((entry) => entry.subtitle_id && entry.candidate_id && entry.batch_id && entry.title && entry.subtitle)
    : [];
}

async function main() {
  const requestPath = process.argv[2];
  if (!requestPath) {
    console.error("Usage: node scripts/writing_api/generate_thumbnails.mjs <request.json>");
    process.exitCode = 1;
    return;
  }

  const apiKey = process.env.OPENAI_API_KEY?.trim();
  if (!apiKey || apiKey === "...") {
    console.error("Missing OPENAI_API_KEY. Add it to the local .env file before using live thumbnail generation.");
    process.exitCode = 1;
    return;
  }

  const payload = JSON.parse(await fs.readFile(requestPath, "utf8"));
  const model = cleanText(payload.model) ?? DEFAULT_MODEL;
  const prompt = cleanText(payload.prompt);
  const variantsPerPackage = Math.max(1, Math.min(4, Number.parseInt(payload.variants_per_package, 10) || DEFAULT_VARIANTS));
  const packages = normalizePackages(payload.packages);

  if (packages.length === 0) {
    console.error("At least one title/subtitle package is required.");
    process.exitCode = 1;
    return;
  }

  try {
    await withLangfuseRun(
      {
        name: "generate-medium-thumbnails-run",
        input: { requestPath, packageCount: packages.length, variantsPerPackage, hasPrompt: Boolean(prompt) },
        metadata: { script: "generateThumbnails", model, mode: "writingApi", requestPath },
        tags: ["writing-api", "thumbnail-generation"],
        sessionId: requestPath,
        traceName: "generate-medium-thumbnails"
      },
      async () => {
        const client = await createOpenAIClient(apiKey, {
          generationName: "generate-medium-thumbnails",
          generationMetadata: { packageCount: packages.length, variantsPerPackage, hasPrompt: Boolean(prompt) },
          tags: ["writing-api", "thumbnail-generation"],
          sessionId: requestPath
        });

        const results = [];
        for (const pkg of packages) {
          const thumbnails = [];
          for (let variantIndex = 1; variantIndex <= variantsPerPackage; variantIndex += 1) {
            let response;
            try {
              response = await client.responses.create({
                model,
                input: buildPrompt({ prompt, pkg, variantIndex, variantsPerPackage }),
                tools: [{ type: "image_generation", size: "1536x1024" }]
              });
            } catch (error) {
              console.error(`OpenAI API failure: ${error.message}`);
              process.exitCode = 1;
              return;
            }

            const images = extractImages(response);
            if (images.length === 0) {
              console.error("OpenAI thumbnail response did not contain an image_generation result.");
              process.exitCode = 1;
              return;
            }

            const image = images[0];
            thumbnails.push({
              thumbnail_label: `API concept ${variantIndex}`,
              thumbnail_data_uri: `data:image/png;base64,${image.b64}`,
              created_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
              model,
              generation_mode: "api",
              raw_json: {
                response_id: response.id ?? null,
                output_id: image.output_id,
                call_id: image.call_id,
                revised_prompt: image.revised_prompt,
                usage: response.usage ?? null,
                model,
                variant_index: variantIndex
              }
            });
          }
          results.push({ ...pkg, thumbnails });
        }

        process.stdout.write(JSON.stringify({ mode: "api", model, results }));
      }
    );
  } finally {
    await flushLangfuse();
  }
}

main().catch((error) => {
  console.error(error.message);
  process.exitCode = 1;
});
