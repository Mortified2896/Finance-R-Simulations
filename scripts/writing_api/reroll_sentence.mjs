import "dotenv/config";
import fs from "node:fs/promises";
import path from "node:path";
import process from "node:process";
import { createOpenAIClient, flushLangfuse, withLangfuseRun } from "./langfuse.mjs";

const MODEL = "gpt-5.5";
const PROJECT_DIR = path.join("article_projects", "sp500-finfluencers");
const STYLE_RULES_PATH = path.join(PROJECT_DIR, "style_rules.md");
const BRIEF_PATH = path.join(PROJECT_DIR, "brief.md");
const OUTPUT_DIR = path.join(PROJECT_DIR, "api_outputs");

function usage() {
  console.log(`Usage:
node scripts/writing_api/reroll_sentence.mjs "Text to rewrite"

Example:
node scripts/writing_api/reroll_sentence.mjs "The S&P 500 is not the problem. The problem is treating it as the default."`);
}

function formatTimestamp(date = new Date()) {
  const pad = (value) => String(value).padStart(2, "0");
  return [
    date.getFullYear(),
    pad(date.getMonth() + 1),
    pad(date.getDate()),
    "_",
    pad(date.getHours()),
    pad(date.getMinutes()),
    pad(date.getSeconds()),
  ].join("");
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

async function readRequiredFile(filePath) {
  try {
    return await fs.readFile(filePath, "utf8");
  } catch (error) {
    if (error.code === "ENOENT") {
      throw new Error(`Missing source file: ${filePath}`);
    }
    throw error;
  }
}

async function main() {
  const inputText = process.argv.slice(2).join(" ").trim();

  if (!inputText) {
    usage();
    return;
  }

  const [styleRules, brief] = await Promise.all([
    readRequiredFile(STYLE_RULES_PATH),
    readRequiredFile(BRIEF_PATH),
  ]);

  const apiKey = process.env.OPENAI_API_KEY?.trim();
  if (!apiKey || apiKey === "...") {
    console.error("Missing OPENAI_API_KEY. Add it to a local .env file as OPENAI_API_KEY=... before running this helper.");
    process.exitCode = 1;
    return;
  }

  const prompt = `You are helping rewrite a selected sentence or paragraph for a Medium finance article.

Article brief:
${brief}

Style rules:
${styleRules}

Rewrite this text in exactly 8 labeled alternatives:
1. clearer
2. punchier
3. more thoughtful
4. more beginner-friendly
5. more Medium-friendly
6. more science-aligned
7. shorter
8. stronger but still fair

Writing constraints:
- no em dashes
- do not overclaim
- do not say the S&P 500 is bad
- keep the tone fair and science-based
- preserve the core argument
- avoid sounding like a textbook
- avoid personal attacks on financial influencers
- return only the 8 alternatives

Text to rewrite:
${inputText}`;

  try {
    await withLangfuseRun(
      {
        name: "reroll-sentence-run",
        input: {
          inputLength: inputText.length,
          projectDir: PROJECT_DIR
        },
        metadata: {
          script: "rerollSentence",
          model: MODEL,
          project: "sp500finfluencers"
        },
        tags: ["writing-api", "reroll-sentence"],
        traceName: "reroll-sentence"
      },
      async () => {
        let generatedText;
        try {
          const client = await createOpenAIClient(apiKey, {
            generationName: "reroll-sentence",
            generationMetadata: {
              projectDir: PROJECT_DIR,
              inputLength: inputText.length
            },
            tags: ["writing-api", "reroll-sentence"]
          });
          const response = await client.responses.create({
            model: MODEL,
            input: prompt,
          });

          generatedText = extractText(response);
          if (!generatedText) {
            throw new Error("The API response did not include generated text.");
          }
        } catch (error) {
          console.error("OpenAI API failure:", error.message);
          process.exitCode = 1;
          return;
        }

        const timestamp = formatTimestamp();
        const outputPath = path.join(OUTPUT_DIR, `reroll_sentence_${timestamp}.md`);
        const markdown = `# Reroll Sentence Output

Timestamp: ${timestamp}

Input text:
${inputText}

Model used:
${MODEL}

Generated alternatives:

${generatedText}
`;

        await fs.mkdir(OUTPUT_DIR, { recursive: true });
        await fs.writeFile(outputPath, markdown, "utf8");

        console.log(generatedText);
        console.log(`\nSaved output to ${outputPath}`);
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
