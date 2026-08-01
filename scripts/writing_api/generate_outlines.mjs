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
  const fenced = trimmed.match(/^```(?:json)?\s*([\s\S]*?)\s*```$/i);
  return fenced ? fenced[1].trim() : trimmed;
}

function previewText(text, maxLength = 1200) {
  const value = String(text ?? "").replace(/\s+/g, " ").trim();
  return value.length > maxLength ? `${value.slice(0, maxLength)}...` : value;
}

function parseResults(rawText, packages = []) {
  let parsed;
  try {
    parsed = JSON.parse(stripCodeFences(rawText));
  } catch (error) {
    if (packages.length === 1 && cleanText(rawText)) {
      const entry = packages[0];
      return [{
        thumbnail_id: entry.thumbnail_id,
        subtitle_id: entry.subtitle_id,
        candidate_id: entry.candidate_id,
        batch_id: entry.batch_id,
        outline_text: cleanText(rawText)
      }];
    }
    throw error;
  }
  const results = Array.isArray(parsed.results) ? parsed.results : [];
  return results.map((entry) => ({
    thumbnail_id: cleanText(entry.thumbnail_id),
    subtitle_id: cleanText(entry.subtitle_id),
    candidate_id: cleanText(entry.candidate_id),
    batch_id: cleanText(entry.batch_id),
    outline_text: cleanText(entry.outline_text)
  })).filter((entry) => entry.thumbnail_id && entry.subtitle_id && entry.candidate_id && entry.batch_id && entry.outline_text);
}

function buildPrompt({ prompt, packages, contextNotes }) {
  const parts = [];
  const contextNote = cleanText(contextNotes);
  if (contextNote) parts.push(`Author comment:\n${contextNote}`);
  const basePrompt = cleanText(prompt) || [
    "Generate practical Medium article outlines for approved title/subtitle/thumbnail packages.",
    "Return valid JSON only.",
    "Use this exact shape:",
    "{\"results\":[{\"thumbnail_id\":\"...\",\"subtitle_id\":\"...\",\"candidate_id\":\"...\",\"batch_id\":\"...\",\"outline_text\":\"# Outline\\n...\"}]}",
    "Each outline should use Markdown headings and bullets.",
    "Include a hook, 4-6 main sections, key points, caveats, and a closing angle.",
    "Do not draft the full article."
  ].join("\n");

  const packageList = packages.map((entry, index) => {
    const lines = [
      `${index + 1}. thumbnail_id=${entry.thumbnail_id} | subtitle_id=${entry.subtitle_id} | candidate_id=${entry.candidate_id} | batch_id=${entry.batch_id}`,
      `Title: ${entry.title}`,
      `Subtitle: ${entry.subtitle}`,
      `Thumbnail label: ${entry.thumbnail_label ?? "approved thumbnail"}`
    ];
    if (entry.article_summary) {
      lines.push("Research summary context:", entry.article_summary);
    }
    if (entry.pdf_path) {
      lines.push("Research PDF: attached as input_file");
    }
    return lines.join("\n");
  }).join("\n\n");

  parts.push(...[
    basePrompt,
    "Return one outline per package, preserving all ids exactly.",
    "Packages:",
    packageList
  ].join("\n\n"));
  return parts.join("\n\n");
}

async function buildResponsesInput({ client, prompt, packages, contextNotes }) {
  const fileIds = [];
  const seenPaths = new Set();
  for (const entry of packages) {
    if (!entry.pdf_path || seenPaths.has(entry.pdf_path) || !fsSync.existsSync(entry.pdf_path)) continue;
    seenPaths.add(entry.pdf_path);
    const file = await client.files.create({ file: fsSync.createReadStream(entry.pdf_path), purpose: "user_data" });
    fileIds.push(file.id);
  }

  const content = [{ type: "input_text", text: buildPrompt({ prompt, packages, contextNotes }) }];
  for (const fileId of fileIds) {
    content.push({ type: "input_file", file_id: fileId });
  }
  return [{ role: "user", content }];
}

async function main() {
  const requestPath = process.argv[2];
  if (!requestPath) {
    console.error("Usage: node scripts/writing_api/generate_outlines.mjs <request.json>");
    process.exitCode = 1;
    return;
  }

  const apiKey = process.env.OPENAI_API_KEY?.trim();
  if (!apiKey || apiKey === "...") {
    console.error("Missing OPENAI_API_KEY. Add it to the local .env file before using live outline generation.");
    process.exitCode = 1;
    return;
  }

  const payload = JSON.parse(await fs.readFile(requestPath, "utf8"));
  const model = cleanText(payload.model) ?? "gpt-5-mini";
  const reasoningEffort = cleanText(payload.reasoning_effort);
  const reasoningMode = cleanText(payload.reasoning_mode) ?? "standard";
  const prompt = cleanText(payload.prompt);
  const contextNotes = cleanText(payload.context_notes);
  const packages = Array.isArray(payload.packages)
    ? payload.packages.map((entry) => ({
        thumbnail_id: cleanText(entry.thumbnail_id),
        subtitle_id: cleanText(entry.subtitle_id),
        candidate_id: cleanText(entry.candidate_id),
        batch_id: cleanText(entry.batch_id),
        title: cleanText(entry.title),
        subtitle: cleanText(entry.subtitle),
        thumbnail_label: cleanText(entry.thumbnail_label),
        article_summary: cleanText(entry.article_summary),
        pdf_path: cleanText(entry.pdf_path)
      })).filter((entry) => entry.thumbnail_id && entry.subtitle_id && entry.candidate_id && entry.batch_id && entry.title && entry.subtitle)
    : [];

  if (packages.length === 0) {
    console.error("At least one approved package is required.");
    process.exitCode = 1;
    return;
  }

  try {
    await withLangfuseRun(
      {
        name: "generate-medium-outlines-run",
        input: { requestPath, packageCount: packages.length, hasPrompt: Boolean(prompt) },
        metadata: { script: "generateOutlines", model, mode: "writingApi", requestPath },
        tags: ["writing-api", "outline-generation"],
        sessionId: requestPath,
        traceName: "generate-medium-outlines"
      },
      async () => {
        let response;
        try {
          const client = await createOpenAIClient(apiKey, {
            generationName: "generate-medium-outlines",
            generationMetadata: { packageCount: packages.length, hasPrompt: Boolean(prompt) },
            tags: ["writing-api", "outline-generation"],
            sessionId: requestPath
          });
          const request = { model, input: await buildResponsesInput({ client, prompt, packages, contextNotes }) };
          if (reasoningEffort) request.reasoning = { effort: reasoningEffort };
          if (reasoningMode === "pro") request.reasoning = { ...(request.reasoning ?? {}), mode: "pro" };
          response = await client.responses.create(request);
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
          console.error(`Could not parse outline response: ${error.message}. Raw response preview: ${previewText(rawText)}`);
          process.exitCode = 1;
          return;
        }

        process.stdout.write(JSON.stringify({
          mode: "api",
          model,
          reasoning_effort: reasoningEffort,
          reasoning_mode: reasoningMode,
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
