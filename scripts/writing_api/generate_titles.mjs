import "dotenv/config";
import fs from "node:fs/promises";
import OpenAI from "openai";

function usage() {
  console.error("Usage: node scripts/writing_api/generate_titles.mjs <request.json>");
}

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

function buildPrompt({ prompt, batchSize, seedTopic, inspirationSource, exampleTitles }) {
  const sections = [
    "You generate Medium-style article title candidates for personal finance and investing.",
    "Return valid JSON only in the shape {\"titles\": [\"...\", \"...\"]}.",
    `Return exactly ${batchSize} titles.`,
    "Do not include explanations, numbering, markdown, or code fences.",
    "Do not copy any example title verbatim.",
    "Keep the titles credible, science-based, beginner-friendly, and not clickbait."
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

  sections.push("User prompt:", prompt);
  return sections.join("\n\n");
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
  const prompt = cleanText(payload.prompt);
  const model = cleanText(payload.model) ?? "gpt-5-mini";
  const seedTopic = cleanText(payload.seed_topic);
  const inspirationSource = cleanText(payload.inspiration_source);
  const exampleTitles = Array.isArray(payload.example_titles)
    ? payload.example_titles.map(cleanText).filter(Boolean)
    : [];
  const batchSize = Math.max(1, Math.min(25, Number.parseInt(payload.batch_size, 10) || 12));

  if (!prompt) {
    console.error("Prompt is required.");
    process.exitCode = 1;
    return;
  }

  const client = new OpenAI({ apiKey });
  const builtPrompt = buildPrompt({
    prompt,
    batchSize,
    seedTopic,
    inspirationSource,
    exampleTitles
  });

  let response;
  try {
    response = await client.responses.create({
      model,
      input: builtPrompt
    });
  } catch (error) {
    console.error(`OpenAI API failure: ${error.message}`);
    process.exitCode = 1;
    return;
  }

  const rawText = extractText(response);
  const titles = parseTitles(rawText, batchSize);
  if (titles.length === 0) {
    console.error("The API response did not contain any usable titles.");
    process.exitCode = 1;
    return;
  }

  const output = {
    mode: "api",
    model,
    response_id: response.id ?? null,
    titles,
    raw_text: rawText,
    usage: response.usage ?? null
  };

  process.stdout.write(JSON.stringify(output));
}

main().catch((error) => {
  console.error(error.message);
  process.exitCode = 1;
});
