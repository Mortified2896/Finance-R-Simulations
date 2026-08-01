import "dotenv/config";
import fs from "node:fs/promises";
import { createOpenAIClient, flushLangfuse, withLangfuseRun } from "./langfuse.mjs";

function cleanText(value) {
  if (value === null || value === undefined) return null;
  const normalized = String(value).replace(/\u00a0/g, " ").replace(/\s+/g, " ").trim();
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

function normalizeTags(values) {
  const unique = [];
  for (const value of values) {
    const cleaned = cleanText(value)?.replace(/^#+/, "").replace(/^["“”']+|["“”']+$/g, "");
    if (!cleaned) continue;
    if (!unique.some((tag) => tag.toLowerCase() === cleaned.toLowerCase())) unique.push(cleaned);
    if (unique.length >= 5) break;
  }
  return unique;
}

function parseTags(rawText) {
  const stripped = stripCodeFences(rawText);
  const parsed = JSON.parse(stripped);
  if (Array.isArray(parsed)) return normalizeTags(parsed);
  return normalizeTags(Array.isArray(parsed.tags) ? parsed.tags : []);
}

function buildPrompt({ prompt, article }) {
  const basePrompt = cleanText(prompt) || [
    "Generate Medium tags for the approved article package.",
    "Return valid JSON only in the shape {\"tags\":[\"...\",\"...\"]}.",
    "Return exactly 5 tags unless fewer are clearly appropriate.",
    "Each tag must be short, specific, Medium-friendly, and useful for personal finance or investing readers.",
    "Do not include hashtags, numbering, markdown, or explanations."
  ].join("\n");

  return [
    basePrompt,
    "Article package:",
    `full_text_draft_id=${article.full_text_draft_id ?? ""} | candidate_id=${article.candidate_id ?? ""} | batch_id=${article.batch_id ?? ""}`,
    `Title: ${article.title ?? ""}`,
    `Subtitle: ${article.subtitle ?? ""}`,
    "Article body:",
    article.body ?? ""
  ].join("\n\n");
}

async function main() {
  const requestPath = process.argv[2];
  if (!requestPath) {
    console.error("Usage: node scripts/writing_api/generate_medium_tags.mjs <request.json>");
    process.exitCode = 1;
    return;
  }

  const apiKey = process.env.OPENAI_API_KEY?.trim();
  if (!apiKey || apiKey === "...") {
    console.error("Missing OPENAI_API_KEY. Add it to the local .env file before using live Medium tag generation.");
    process.exitCode = 1;
    return;
  }

  const payload = JSON.parse(await fs.readFile(requestPath, "utf8"));
  const model = cleanText(payload.model) ?? "gpt-5-mini";
  const reasoningEffort = cleanText(payload.reasoning_effort);
  const reasoningMode = cleanText(payload.reasoning_mode) ?? "standard";
  const prompt = cleanText(payload.prompt);
  const article = {
    full_text_draft_id: cleanText(payload.article?.full_text_draft_id),
    candidate_id: cleanText(payload.article?.candidate_id),
    batch_id: cleanText(payload.article?.batch_id),
    title: cleanText(payload.article?.title),
    subtitle: cleanText(payload.article?.subtitle),
    body: cleanText(payload.article?.body)
  };

  if (!article.title || !article.body) {
    console.error("An article title and body are required.");
    process.exitCode = 1;
    return;
  }

  try {
    await withLangfuseRun(
      {
        name: "generate-medium-tags-run",
        input: { requestPath, hasPrompt: Boolean(prompt), articleId: article.full_text_draft_id },
        metadata: { script: "generateMediumTags", model, mode: "writingApi", requestPath },
        tags: ["writing-api", "medium-tags"],
        sessionId: requestPath,
        traceName: "generate-medium-tags"
      },
      async () => {
        let response;
        try {
          const client = await createOpenAIClient(apiKey, {
            generationName: "generate-medium-tags",
            generationMetadata: { articleId: article.full_text_draft_id, hasPrompt: Boolean(prompt) },
            tags: ["writing-api", "medium-tags"],
            sessionId: requestPath
          });
          const request = { model, input: buildPrompt({ prompt, article }) };
          if (reasoningEffort) request.reasoning = { effort: reasoningEffort };
          if (reasoningMode === "pro") request.reasoning = { ...(request.reasoning ?? {}), mode: "pro" };
          response = await client.responses.create(request);
        } catch (error) {
          console.error(`OpenAI API failure: ${error.message}`);
          process.exitCode = 1;
          return;
        }

        const rawText = extractText(response);
        let tags;
        try {
          tags = parseTags(rawText);
        } catch (error) {
          console.error(`Could not parse Medium tag response: ${error.message}`);
          process.exitCode = 1;
          return;
        }

        process.stdout.write(JSON.stringify({
          mode: "api",
          model,
          reasoning_effort: reasoningEffort,
          reasoning_mode: reasoningMode,
          response_id: response.id ?? null,
          tags,
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
