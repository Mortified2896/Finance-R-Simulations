import "dotenv/config";
import fs from "node:fs/promises";
import { pathToFileURL } from "node:url";
import { createOpenAIClient, flushLangfuse, withLangfuseRun } from "./langfuse.mjs";

const MAX_SUBTITLE_CHARS = 90;

function cleanText(value) {
  if (value === null || value === undefined) return null;
  const normalized = String(value).replace(/\u00a0/g, " ").replace(/\s+/g, " ").trim();
  return normalized.length > 0 ? normalized : null;
}

function extractText(response) {
  if (response.output_text) {
    return response.output_text.trim();
  }

  const parts = [];
  for (const item of response.output ?? []) {
    for (const content of item.content ?? []) {
      if (content.type === "output_text" && content.text) {
        parts.push(content.text);
      }
    }
  }

  return parts.join("\n").trim();
}

function stripCodeFences(text) {
  const trimmed = text.trim();
  const fenced = trimmed.match(/^```(?:json)?\s*([\s\S]*?)\s*```$/i);
  return fenced ? fenced[1].trim() : trimmed;
}

function normalizeSubtitles(values, requestedCount) {
  const unique = [];
  for (const value of values) {
    const cleaned = cleanText(value)?.replace(/^["“”']+|["“”']+$/g, "");
    if (!cleaned) continue;
    if (Array.from(cleaned).length > MAX_SUBTITLE_CHARS) continue;
    if (!unique.includes(cleaned)) unique.push(cleaned);
    if (unique.length >= requestedCount) break;
  }
  return unique;
}

export function mapSubtitleResults(rawText, variantsPerTitle, candidates) {
  const stripped = stripCodeFences(rawText);
  const parsed = JSON.parse(stripped);
  const results = Array.isArray(parsed.results) ? parsed.results : [];
  const byAlias = new Map(candidates.map((entry) => [entry.item_alias, entry]));
  return results.map((entry) => {
    const source = byAlias.get(cleanText(entry.item_alias));
    if (!source) return null;
    return {
    item_alias: source.item_alias,
    candidate_id: source.candidate_id,
    batch_id: source.batch_id,
    subtitles: normalizeSubtitles(Array.isArray(entry.subtitles) ? entry.subtitles : [], variantsPerTitle)
  };}).filter(Boolean);
}

function buildPrompt({ prompt, candidates, variantsPerTitle }) {
  if (typeof prompt !== "string") throw new Error("A resolved subtitle prompt is required.");
  return prompt;
}

async function main() {
  const requestPath = process.argv[2];
  if (!requestPath) {
    console.error("Usage: node scripts/writing_api/generate_subtitles.mjs <request.json>");
    process.exitCode = 1;
    return;
  }

  const apiKey = process.env.OPENAI_API_KEY?.trim();
  if (!apiKey || apiKey === "...") {
    console.error("Missing OPENAI_API_KEY. Add it to the local .env file before using live subtitle generation.");
    process.exitCode = 1;
    return;
  }

  const payload = JSON.parse(await fs.readFile(requestPath, "utf8"));
  const model = cleanText(payload.model) ?? "gpt-5-mini";
  const reasoningEffort = cleanText(payload.reasoning_effort);
  const reasoningMode = cleanText(payload.reasoning_mode) ?? "standard";
  const variantsPerTitle = Math.max(1, Math.min(8, Number.parseInt(payload.variants_per_title, 10) || 4));
  const prompt = typeof payload.resolved_prompt === "string" ? payload.resolved_prompt : null;
  const candidates = Array.isArray(payload.candidates)
    ? payload.candidates
        .map((entry) => ({
          item_alias: cleanText(entry.item_alias),
          candidate_id: cleanText(entry.candidate_id),
          batch_id: cleanText(entry.batch_id),
          title: cleanText(entry.title),
          article_summary: cleanText(entry.article_summary)
        }))
        .filter((entry) => entry.item_alias && entry.candidate_id && entry.batch_id && entry.title)
    : [];

  if (candidates.length === 0) {
    console.error("At least one title is required.");
    process.exitCode = 1;
    return;
  }

  try {
    await withLangfuseRun(
      {
        name: "generate-medium-subtitles-run",
        input: {
          requestPath,
          candidateCount: candidates.length,
          variantsPerTitle,
          hasPrompt: Boolean(prompt)
        },
        metadata: {
          script: "generateSubtitles",
          model,
          mode: "writingApi",
          requestPath
        },
        tags: ["writing-api", "subtitle-generation"],
        sessionId: requestPath,
        traceName: "generate-medium-subtitles"
      },
      async () => {
        let response;
        try {
          const client = await createOpenAIClient(apiKey, {
            generationName: "generate-medium-subtitles",
            generationMetadata: {
              candidateCount: candidates.length,
              variantsPerTitle,
              hasPrompt: Boolean(prompt)
            },
            tags: ["writing-api", "subtitle-generation"],
            sessionId: requestPath
          });
          const request = { model, input: buildPrompt({ prompt, candidates, variantsPerTitle }) };
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
          results = mapSubtitleResults(rawText, variantsPerTitle, candidates);
        } catch (error) {
          console.error(`Could not parse subtitle response: ${error.message}`);
          process.exitCode = 1;
          return;
        }

        process.stdout.write(JSON.stringify({
          mode: "api",
          model,
          reasoning_effort: reasoningEffort,
          reasoning_mode: reasoningMode,
          request_id: response._request_id ?? response.request_id ?? null,
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

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) main().catch((error) => {
  console.error(error.message);
  process.exitCode = 1;
});
