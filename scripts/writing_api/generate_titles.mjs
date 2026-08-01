import "dotenv/config";
import fs from "node:fs/promises";
import { createOpenAIClient, flushLangfuse, withLangfuseRun } from "./langfuse.mjs";

const MAX_TITLE_CHARS = 140;
const PREFERRED_TITLE_LENGTH = "40-75";

function usage() {
  console.error("Usage: node scripts/writing_api/generate_titles.mjs <request.json>");
}

function cleanText(value) {
  if (value === null || value === undefined) return null;
  const normalized = String(value).replace(/\u00a0/g, " ").replace(/\s+/g, " ").trim();
  return normalized.length > 0 ? normalized : null;
}

function cleanMultilineText(value) {
  if (value === null || value === undefined) return null;
  const normalized = String(value)
    .replace(/\r\n?/g, "\n")
    .replace(/\u00a0/g, " ")
    .split("\n")
    .map((line) => line.replace(/[ \t]+$/g, ""))
    .join("\n")
    .trim();
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

function normalizeTitles(values, requestedCount) {
  const unique = [];
  for (const value of values) {
    const cleaned = cleanText(value)?.replace(/^["“”']+|["“”']+$/g, "");
    if (!cleaned) continue;
    if (!unique.includes(cleaned)) unique.push(cleaned);
    if (unique.length >= requestedCount) break;
  }
  return unique;
}

function titleLength(value) {
  return Array.from(String(value ?? "")).length;
}

function splitByLength(titles, maxChars = MAX_TITLE_CHARS) {
  const valid = [];
  const invalid = [];
  for (const title of titles) {
    if (titleLength(title) <= maxChars) valid.push(title);
    else invalid.push(title);
  }
  return { valid, invalid };
}

function parseTitles(rawText, requestedCount) {
  const stripped = stripCodeFences(rawText);

  try {
    const parsed = JSON.parse(stripped);
    if (Array.isArray(parsed)) {
      return normalizeTitles(parsed, requestedCount);
    }
    if (Array.isArray(parsed.titles)) {
      return normalizeTitles(parsed.titles, requestedCount);
    }
  } catch (_) {
    // fall through to line parsing
  }

  const lineCandidates = stripped
    .split("\n")
    .map((line) => line.replace(/^\s*(?:[-*]|\d+[.)])\s*/, ""))
    .filter(Boolean);
  return normalizeTitles(lineCandidates, requestedCount);
}

function buildPrompt({ prompt, manualPrompt, batchSize, seedTopic, inspirationSource, exampleTitles }) {
  const sections = [
    "You generate Medium-style article title candidates for personal finance and investing.",
    "Return valid JSON only in the shape {\"titles\": [\"...\", \"...\"]}.",
    `Return exactly ${batchSize} titles.`,
    `Every title must be at most ${MAX_TITLE_CHARS} characters, including spaces.`,
    `Prefer ${PREFERRED_TITLE_LENGTH} characters when possible. Do not make titles long unless the extra words clearly improve clarity or curiosity.`,
    "Do not include explanations, numbering, markdown, or code fences.",
    "Do not copy any example title verbatim.",
    "Keep the titles credible, science-based, beginner-friendly, and not clickbait.",
    "If a title would exceed the limit, rewrite it shorter instead of truncating it."
  ];

  if (seedTopic) {
    sections.push(`Seed topic: ${seedTopic}`);
  }

  if (inspirationSource) {
    sections.push(`Inspiration source: ${inspirationSource}`);
  }

  if (exampleTitles.length > 0) {
    sections.push(
      "Reference examples from top-performing historical titles. Use them only as inspiration for tone/patterns:",
      exampleTitles.map((title, index) => `${index + 1}. ${title}`).join("\n")
    );
  }

  if (manualPrompt) {
    sections.push("Manual/default prompt:", manualPrompt);
  }

  sections.push("Article summary:", prompt);
  return sections.join("\n\n");
}

function buildPromptWithContext({ prompt, manualPrompt, batchSize, seedTopic, inspirationSource, exampleTitles, contextNotes }) {
  if (typeof prompt !== "string") throw new Error("A resolved title prompt is required.");
  return prompt;
}

function buildRetryPrompt({ originalPrompt, batchSize, invalidTitles }) {
  return [
    "Your previous title candidates were too long.",
    `Rewrite them so every title is at most ${MAX_TITLE_CHARS} characters, including spaces.`,
    `Prefer ${PREFERRED_TITLE_LENGTH} characters when possible. Do not make titles long unless the extra words clearly improve clarity or curiosity.`,
    `Return valid JSON only in the shape {\"titles\": [\"...\", \"...\"]}.`,
    `Return exactly ${batchSize} titles.`,
    "Do not explain anything.",
    "Original prompt:",
    originalPrompt,
    "Too-long titles to shorten:",
    invalidTitles.map((title, index) => `${index + 1}. ${title}`).join("\n")
  ].join("\n\n");
}

async function main() {
  const requestPath = process.argv[2];
  if (!requestPath) {
    usage();
    process.exitCode = 1;
    return;
  }

  const apiKey = process.env.OPENAI_API_KEY?.trim();
  if (!apiKey || apiKey === "...") {
    console.error("Missing OPENAI_API_KEY. Add it to the local .env file before using live title generation.");
    process.exitCode = 1;
    return;
  }

  const payload = JSON.parse(await fs.readFile(requestPath, "utf8"));
  const prompt = typeof payload.resolved_prompt === "string" ? payload.resolved_prompt : null;
  const manualPrompt = null;
  const model = cleanText(payload.model) ?? "gpt-5-mini";
  const reasoningEffort = cleanText(payload.reasoning_effort);
  const reasoningMode = cleanText(payload.reasoning_mode) ?? "standard";
  const seedTopic = cleanText(payload.seed_topic);
  const inspirationSource = cleanText(payload.inspiration_source);
  const contextNotes = cleanMultilineText(payload.context_notes);
  const exampleTitles = Array.isArray(payload.example_titles)
    ? payload.example_titles.map(cleanText).filter(Boolean)
    : [];
  const batchSize = Math.max(1, Math.min(25, Number.parseInt(payload.batch_size, 10) || 12));

  if (!prompt) {
    console.error("Prompt is required.");
    process.exitCode = 1;
    return;
  }

  const builtPrompt = buildPromptWithContext({
    prompt,
    manualPrompt,
    batchSize,
    seedTopic,
    inspirationSource,
    exampleTitles,
    contextNotes
  });

  try {
    await withLangfuseRun(
      {
        name: "generate-medium-titles-run",
        input: {
          requestPath,
          batchSize,
          seedTopic,
          hasInspirationSource: Boolean(inspirationSource),
          exampleTitlesCount: exampleTitles.length
        },
        metadata: {
          script: "generateTitles",
          model,
          mode: "writingApi",
          requestPath
        },
        tags: ["writing-api", "title-generation"],
        sessionId: requestPath,
        traceName: "generate-medium-titles"
      },
      async () => {
        let response;
        try {
          const client = await createOpenAIClient(apiKey, {
            generationName: "generate-medium-titles",
            generationMetadata: {
              batchSize,
              seedTopic,
              inspirationSource,
              exampleTitlesCount: exampleTitles.length
            },
            tags: ["writing-api", "title-generation"],
            sessionId: requestPath
          });
          const request = { model, input: builtPrompt };
          if (reasoningEffort) request.reasoning = { effort: reasoningEffort };
          if (reasoningMode === "pro") request.reasoning = { ...(request.reasoning ?? {}), mode: "pro" };
          response = await client.responses.create(request);
        } catch (error) {
          console.error(`OpenAI API failure: ${error.message}`);
          process.exitCode = 1;
          return;
        }

        const rawText = extractText(response);
        let titles = parseTitles(rawText, batchSize);
        let { valid, invalid } = splitByLength(titles, MAX_TITLE_CHARS);
        let retryUsed = false;
        let retryRawText = null;

        if (invalid.length > 0) {
          retryUsed = true;
          let retryResponse;
          try {
            const retryClient = await createOpenAIClient(apiKey, {
              generationName: "rewrite-overlong-medium-titles",
              generationMetadata: {
                batchSize,
                invalidCount: invalid.length
              },
              tags: ["writing-api", "title-generation", "retry"],
              sessionId: requestPath
            });
            const retryRequest = {
              model,
              input: buildRetryPrompt({
                originalPrompt: builtPrompt,
                batchSize,
                invalidTitles: invalid
              })
            };
            if (reasoningEffort) retryRequest.reasoning = { effort: reasoningEffort };
            if (reasoningMode === "pro") retryRequest.reasoning = { ...(retryRequest.reasoning ?? {}), mode: "pro" };
            retryResponse = await retryClient.responses.create(retryRequest);
          } catch (error) {
            console.error(`OpenAI API retry failure: ${error.message}`);
            process.exitCode = 1;
            return;
          }

          retryRawText = extractText(retryResponse);
          titles = parseTitles(retryRawText, batchSize);
          ({ valid, invalid } = splitByLength(titles, MAX_TITLE_CHARS));
        }

        if (valid.length === 0) {
          console.error("The API response did not contain any usable titles.");
          process.exitCode = 1;
          return;
        }

        const output = {
          mode: "api",
          model,
          reasoning_effort: reasoningEffort,
          reasoning_mode: reasoningMode,
          response_id: response.id ?? null,
          titles: valid,
          raw_text: rawText,
          retry_used: retryUsed,
          retry_raw_text: retryRawText,
          dropped_count: invalid.length,
          dropped_titles: invalid,
          usage: response.usage ?? null
        };

        process.stdout.write(JSON.stringify(output));
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
