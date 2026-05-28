import "dotenv/config";
import fs from "node:fs/promises";
import path from "node:path";
import { createOpenAIClient, flushLangfuse, withLangfuseRun } from "./langfuse.mjs";

function usage() {
  console.error("Usage: node scripts/writing_api/summarize_research_pdf.mjs <request.json>");
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
    .replace(/[ \t]+$/gm, "")
    .trim();
  return normalized.length > 0 ? normalized : null;
}

function formatSummaryText(value) {
  let text = cleanMultilineText(value);
  if (!text) return null;
  const headings = [
    "Short summary",
    "Main findings",
    "Why it matters for investors",
    "Interesting details",
    "Caveats / limitations",
    "What not to overclaim",
    "Possible article directions"
  ];

  for (const heading of headings) {
    const escaped = heading.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    text = text.replace(new RegExp(`(?:^|\\n)\\s*#{1,6}\\s*${escaped}\\s*:?(?=\\s|$)`, "gi"), `\n\n${heading}:\n\n`);
    text = text.replace(new RegExp(`(?:^|\\n)\\s*${escaped}\\s*:(?=\\s|$)`, "gi"), `\n\n${heading}:\n\n`);
  }

  text = text
    .replace(/\n{3,}/g, "\n\n")
    .replace(/:\n\n[ \t]*[-*] /g, ":\n\n- ")
    .trim();

  return text.length > 0 ? text : null;
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

function buildMetadataText(payload) {
  return [
    "Source metadata:",
    `Research source ID: ${cleanText(payload.research_source_id) ?? ""}`,
    `Source title: ${cleanText(payload.source_title) ?? ""}`,
    `Source URL: ${cleanText(payload.source_url) ?? ""}`,
    `PDF URL: ${cleanText(payload.pdf_url) ?? ""}`,
    "Main idea:",
    cleanMultilineText(payload.main_idea) ?? "",
    "",
    "Abstract:",
    cleanMultilineText(payload.abstract) ?? "",
    "",
    "User prompt:",
    cleanMultilineText(payload.prompt) ?? "Summarize this research paper for a beginner-friendly personal finance writing workflow."
  ].join("\n");
}

async function resolveLocalPdfPath(localPdfPath) {
  if (path.isAbsolute(localPdfPath)) return localPdfPath;
  const scriptDir = path.dirname(new URL(import.meta.url).pathname);
  const projectRoot = path.resolve(scriptDir, "..", "..");
  const candidates = [
    path.resolve(process.cwd(), localPdfPath),
    path.resolve(projectRoot, localPdfPath)
  ];
  for (const candidate of candidates) {
    try {
      await fs.access(candidate);
      return candidate;
    } catch {
      // Try the next likely base directory.
    }
  }
  return candidates[0];
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
    console.error("Missing OPENAI_API_KEY. Add it to the local .env file before using live research summary generation.");
    process.exitCode = 1;
    return;
  }

  const payload = JSON.parse(await fs.readFile(requestPath, "utf8"));
  const model = cleanText(payload.model) ?? "gpt-5-mini";
  const promptVersion = cleanText(payload.prompt_version) ?? "research_summary_v1";
  const localPdfPath = cleanText(payload.local_pdf_path);
  if (!localPdfPath) {
    console.error("local_pdf_path is required.");
    process.exitCode = 1;
    return;
  }

  const resolvedLocalPdfPath = await resolveLocalPdfPath(localPdfPath);
  let pdfBytes;
  try {
    pdfBytes = await fs.readFile(resolvedLocalPdfPath);
  } catch (error) {
    console.error(`Could not read local PDF: ${error.message}`);
    process.exitCode = 1;
    return;
  }

  const filename = path.basename(resolvedLocalPdfPath) || "research-paper.pdf";
  const fileData = `data:application/pdf;base64,${pdfBytes.toString("base64")}`;
  const metadataText = buildMetadataText(payload);

  try {
    await withLangfuseRun(
      {
        name: "summarize-research-pdf-run",
        input: {
          requestPath,
          researchSourceId: payload.research_source_id ?? null,
          sourceTitle: cleanText(payload.source_title),
          localPdfPath: resolvedLocalPdfPath
        },
        metadata: {
          script: "summarizeResearchPdf",
          model,
          promptVersion,
          mode: "writingApi",
          requestPath
        },
        tags: ["writing-api", "research-summary"],
        sessionId: requestPath,
        traceName: "summarize-research-pdf"
      },
      async () => {
        let response;
        try {
          const client = await createOpenAIClient(apiKey, {
            generationName: "summarize-research-pdf",
            generationMetadata: {
              researchSourceId: payload.research_source_id ?? null,
              sourceTitle: cleanText(payload.source_title),
              promptVersion
            },
            tags: ["writing-api", "research-summary"],
            sessionId: requestPath
          });
          response = await client.responses.create({
            model,
            input: [
              {
                role: "user",
                content: [
                  { type: "input_file", filename, file_data: fileData },
                  { type: "input_text", text: metadataText }
                ]
              }
            ]
          });
        } catch (error) {
          console.error(`OpenAI API failure: ${error.message}`);
          process.exitCode = 1;
          return;
        }

        const rawText = formatSummaryText(extractText(response));
        if (!rawText) {
          console.error("The API response did not contain summary text.");
          process.exitCode = 1;
          return;
        }

        const output = {
          summary_text: rawText,
          model,
          prompt_version: promptVersion,
          raw_text: rawText,
          response_id: response.id ?? null,
          usage: response.usage ?? null
        };
        process.stdout.write(JSON.stringify(output));
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
