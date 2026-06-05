import "dotenv/config";
import fs from "node:fs/promises";
import { createOpenAIClient, flushLangfuse, withLangfuseRun } from "./langfuse.mjs";

function usage() {
  console.error("Usage: node scripts/writing_api/select_summary_evidence.mjs <request.json>");
}

function cleanText(value) {
  if (value === null || value === undefined) return null;
  const cleaned = String(value).replace(/\r\n?/g, "\n").trim();
  return cleaned.length > 0 ? cleaned : null;
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

function parseJsonObject(text) {
  const cleaned = cleanText(text);
  if (!cleaned) throw new Error("The API response did not include output text.");
  try {
    return JSON.parse(cleaned);
  } catch {
    const match = cleaned.match(/\{[\s\S]*\}/);
    if (!match) throw new Error("The API response was not JSON.");
    return JSON.parse(match[0]);
  }
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
    console.error("Missing OPENAI_API_KEY. Add it to the local .env file before using evidence generation.");
    process.exitCode = 1;
    return;
  }

  const payload = JSON.parse(await fs.readFile(requestPath, "utf8"));
  const model = cleanText(payload.model) ?? "gpt-5.4-mini";
  const reasoningEffort = cleanText(payload.reasoning_effort) ?? "low";
  const resolvedPrompt = cleanText(payload.resolved_prompt);
  const step = cleanText(payload.step) ?? "summary-evidence";
  if (!resolvedPrompt) {
    console.error("resolved_prompt is required.");
    process.exitCode = 1;
    return;
  }

  try {
    await withLangfuseRun(
      {
        name: `${step}-run`,
        input: {
          requestPath,
          step,
          model,
          reasoningEffort,
          summaryId: payload.summary_id ?? null,
          researchSourceId: payload.research_source_id ?? null
        },
        metadata: {
          script: "selectSummaryEvidence",
          step,
          model,
          reasoningEffort,
          mode: "writingApi",
          requestPath
        },
        tags: ["writing-api", "summary-evidence", step],
        sessionId: requestPath,
        traceName: step
      },
      async () => {
        let response;
        try {
          const client = await createOpenAIClient(apiKey, {
            generationName: step,
            generationMetadata: {
              summaryId: payload.summary_id ?? null,
              researchSourceId: payload.research_source_id ?? null,
              reasoningEffort
            },
            tags: ["writing-api", "summary-evidence", step],
            sessionId: requestPath
          });
          const request = {
            model,
            input: [{ role: "user", content: [{ type: "input_text", text: resolvedPrompt }] }]
          };
          if (reasoningEffort) request.reasoning = { effort: reasoningEffort };
          response = await client.responses.create(request);
        } catch (error) {
          console.error(`OpenAI API failure: ${error.message}`);
          process.exitCode = 1;
          return;
        }

        const rawText = extractText(response);
        let parsed;
        try {
          parsed = parseJsonObject(rawText);
        } catch (error) {
          console.error(error.message);
          process.exitCode = 1;
          return;
        }

        process.stdout.write(JSON.stringify({
          step,
          model,
          reasoning_effort: reasoningEffort,
          raw_text: rawText,
          response_id: response.id ?? null,
          usage: response.usage ?? null,
          parsed
        }));
      }
    );
  } finally {
    await flushLangfuse();
  }
}

main().catch(async (error) => {
  console.error(error?.stack ?? error?.message ?? String(error));
  await flushLangfuse();
  process.exitCode = 1;
});
