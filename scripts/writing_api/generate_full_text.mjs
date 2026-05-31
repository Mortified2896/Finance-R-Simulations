import "dotenv/config";
import fsSync from "node:fs";
import fs from "node:fs/promises";
import { createOpenAIClient, flushLangfuse, withLangfuseRun } from "./langfuse.mjs";

function cleanText(value) {
  if (value === null || value === undefined) return null;
  const normalized = String(value).replace(/\u00a0/g, " ").trim();
  return normalized.length > 0 ? normalized : null;
}

function extractText(response) {
  if (response.output_text) return response.output_text.trim();
  const parts = [];
  for (const item of response.output ?? []) {
    for (const content of item.content ?? []) {
      if (content.type === "output_text" && content.text) parts.push(content.text);
    }
  }
  return parts.join("\n").trim();
}

function stripCodeFences(text) {
  const trimmed = text.trim();
  const fenced = trimmed.match(/^```(?:json|markdown|md)?\s*([\s\S]*?)\s*```$/i);
  return fenced ? fenced[1].trim() : trimmed;
}

function previewText(text, maxLength = 1200) {
  const value = String(text ?? "").replace(/\s+/g, " ").trim();
  return value.length > maxLength ? `${value.slice(0, maxLength)}...` : value;
}

function isPlaceholderDraft(text) {
  const value = cleanText(text);
  if (!value) return true;
  const compact = value.toLowerCase().replace(/\s+/g, " ").trim();
  return compact === "..." || compact === "# title ..." || compact.includes("full_text") || compact.includes("markdown_article_here") || compact.includes("placeholder");
}

function stripPackageHeader(text) {
  const value = cleanText(text);
  if (!value) return null;
  const firstHeading = value.search(/^#{1,3}\s+/m);
  const looksLikePackageHeader = /^outline_id=.*\nthumbnail_id=.*\nsubtitle_id=.*\ncandidate_id=.*\nbatch_id=/m.test(value);
  if (looksLikePackageHeader && firstHeading > 0) return value.slice(firstHeading).trim();
  return value;
}

function parseResults(rawText, packages = []) {
  let parsed;
  try {
    parsed = JSON.parse(stripCodeFences(rawText));
  } catch (error) {
    if (packages.length === 1 && cleanText(rawText) && !isPlaceholderDraft(rawText)) {
      const entry = packages[0];
      return [{
        outline_id: entry.outline_id,
        thumbnail_id: entry.thumbnail_id,
        subtitle_id: entry.subtitle_id,
        candidate_id: entry.candidate_id,
        batch_id: entry.batch_id,
        source_context_mode: entry.source_context_mode ?? "none",
        full_text: stripPackageHeader(stripCodeFences(rawText))
      }];
    }
    throw error;
  }
  const results = Array.isArray(parsed.results) ? parsed.results : [];
  if (results.length === 0 && packages.length === 1 && cleanText(parsed.full_text) && !isPlaceholderDraft(parsed.full_text)) {
    const entry = packages[0];
    return [{
      outline_id: entry.outline_id,
      thumbnail_id: entry.thumbnail_id,
      subtitle_id: entry.subtitle_id,
      candidate_id: entry.candidate_id,
      batch_id: entry.batch_id,
      source_context_mode: entry.source_context_mode ?? "none",
      full_text: stripPackageHeader(parsed.full_text)
    }];
  }
  return results.map((entry) => ({
    outline_id: cleanText(entry.outline_id),
    thumbnail_id: cleanText(entry.thumbnail_id),
    subtitle_id: cleanText(entry.subtitle_id),
    candidate_id: cleanText(entry.candidate_id),
    batch_id: cleanText(entry.batch_id),
    source_context_mode: cleanText(entry.source_context_mode) ?? "none",
    full_text: stripPackageHeader(entry.full_text)
  })).filter((entry) => entry.outline_id && entry.thumbnail_id && entry.subtitle_id && entry.candidate_id && entry.batch_id && entry.full_text && !isPlaceholderDraft(entry.full_text));
}

function buildPrompt({ prompt, packages }) {
  const basePrompt = cleanText(prompt) || [
    "Draft complete Medium articles from approved title/subtitle/thumbnail/outline packages.",
    "Use the provided source context when available and do not invent unsupported research claims.",
  ].join("\n");

  const responseInstructions = [
    "Return valid JSON only.",
    "Return JSON only in this shape: {\"results\":[{\"outline_id\":string,\"thumbnail_id\":string,\"subtitle_id\":string,\"candidate_id\":string,\"batch_id\":string,\"source_context_mode\":\"pdf_attachment\"|\"summary_fallback\"|\"none\",\"full_text\":string}]}",
    "Copy ids exactly from the package. The full_text value must be the complete Markdown article draft, not a schema example, MARKDOWN_ARTICLE_HERE, placeholder, excerpt, note, or explanation.",
    "Ignore any earlier placeholder value such as MARKDOWN_ARTICLE_HERE; replace it with the actual full Markdown article."
  ].join("\n");

  const packageList = packages.map((entry, index) => {
    const lines = [
      `${index + 1}. outline_id=${entry.outline_id} | thumbnail_id=${entry.thumbnail_id} | subtitle_id=${entry.subtitle_id} | candidate_id=${entry.candidate_id} | batch_id=${entry.batch_id}`,
      `Title: ${entry.title}`,
      `Subtitle: ${entry.subtitle}`,
      `Thumbnail concept: ${entry.thumbnail_label ?? "approved thumbnail"}`,
      "Approved outline:",
      entry.outline_text,
      `Source context mode: ${entry.source_context_mode ?? "none"}`
    ];
    if (entry.article_summary) {
      lines.push("Research summary/full text fallback:", entry.article_summary);
    }
    if (entry.pdf_path) {
      lines.push("Research PDF: attached as input_file");
    }
    return lines.join("\n");
  }).join("\n\n");

  return [
    basePrompt,
    responseInstructions,
    "Return one full article draft per package, preserving all ids exactly.",
    "Packages:",
    packageList
  ].join("\n\n");
}

async function buildResponsesInput({ client, prompt, packages }) {
  const fileIds = [];
  const seenPaths = new Set();
  for (const entry of packages) {
    if (!entry.pdf_path || seenPaths.has(entry.pdf_path) || !fsSync.existsSync(entry.pdf_path)) continue;
    seenPaths.add(entry.pdf_path);
    const file = await client.files.create({ file: fsSync.createReadStream(entry.pdf_path), purpose: "user_data" });
    fileIds.push(file.id);
  }

  const content = [{ type: "input_text", text: buildPrompt({ prompt, packages }) }];
  for (const fileId of fileIds) content.push({ type: "input_file", file_id: fileId });
  return [{ role: "user", content }];
}

async function main() {
  const requestPath = process.argv[2];
  if (!requestPath) {
    console.error("Usage: node scripts/writing_api/generate_full_text.mjs <request.json>");
    process.exitCode = 1;
    return;
  }

  const apiKey = process.env.OPENAI_API_KEY?.trim();
  if (!apiKey || apiKey === "...") {
    console.error("Missing OPENAI_API_KEY. Add it to the local .env file before using live full article generation.");
    process.exitCode = 1;
    return;
  }

  const payload = JSON.parse(await fs.readFile(requestPath, "utf8"));
  const model = cleanText(payload.model) ?? "gpt-5-mini";
  const prompt = cleanText(payload.prompt);
  const promptKey = cleanText(payload.prompt_key) ?? "full_text_default";
  const packages = Array.isArray(payload.packages)
    ? payload.packages.map((entry) => ({
        outline_id: cleanText(entry.outline_id),
        thumbnail_id: cleanText(entry.thumbnail_id),
        subtitle_id: cleanText(entry.subtitle_id),
        candidate_id: cleanText(entry.candidate_id),
        batch_id: cleanText(entry.batch_id),
        title: cleanText(entry.title),
        subtitle: cleanText(entry.subtitle),
        thumbnail_label: cleanText(entry.thumbnail_label),
        outline_text: cleanText(entry.outline_text),
        source_context_mode: cleanText(entry.source_context_mode) ?? "none",
        article_summary: cleanText(entry.article_summary),
        pdf_path: cleanText(entry.pdf_path)
      })).filter((entry) => entry.outline_id && entry.thumbnail_id && entry.subtitle_id && entry.candidate_id && entry.batch_id && entry.title && entry.subtitle && entry.outline_text)
    : [];

  if (packages.length === 0) {
    console.error("At least one approved outline package is required.");
    process.exitCode = 1;
    return;
  }

  try {
    await withLangfuseRun(
      {
        name: "generate-medium-full-text-run",
        input: { requestPath, packageCount: packages.length, promptKey, hasPrompt: Boolean(prompt) },
        metadata: { script: "generateFullText", model, promptKey, mode: "writingApi", requestPath },
        tags: ["writing-api", "full-text-generation"],
        sessionId: requestPath,
        traceName: "generate-medium-full-text"
      },
      async () => {
        let response;
        try {
          const client = await createOpenAIClient(apiKey, {
            generationName: "generate-medium-full-text",
            generationMetadata: { packageCount: packages.length, promptKey, hasPrompt: Boolean(prompt) },
            tags: ["writing-api", "full-text-generation"],
            sessionId: requestPath
          });
          response = await client.responses.create({ model, input: await buildResponsesInput({ client, prompt, packages }) });
        } catch (error) {
          console.error(`OpenAI API failure: ${error.message}`);
          process.exitCode = 1;
          return;
        }

        const rawText = extractText(response);
        let results;
        try {
          results = parseResults(rawText, packages);
        } catch (error) {
          console.error(`Could not parse full article response: ${error.message}. Raw response preview: ${previewText(rawText)}`);
          process.exitCode = 1;
          return;
        }

        process.stdout.write(JSON.stringify({
          mode: "api",
          model,
          prompt_key: promptKey,
          response_id: response.id ?? null,
          results,
          raw_text: rawText,
          usage: response.usage ?? null
        }));
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
