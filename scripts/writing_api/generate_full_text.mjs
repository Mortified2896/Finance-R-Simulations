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

const INTERNAL_EVIDENCE_MARKERS = [
  /\{\{EVID:/i,
  /\{\{evidence:/i,
  /\[Q\d+\]/i,
  /\bs\d{2,}\b/i,
  /\bSENTENCE_ID[:=]/i,
  /\bPAGE_ID[:=]/i
];

function findInternalMarkers(text) {
  if (!text) return [];
  return INTERNAL_EVIDENCE_MARKERS
    .map((pattern) => {
      const match = text.match(pattern);
      return match ? match[0] : null;
    })
    .filter((value) => value !== null);
}

const CITATION_LIKE_PATTERN = /\([A-Z][\w'\- .,&]+(?:,)?\s*\d{4}[a-z]?(?:,?\s*(?:p\.|pp\.|pages?)\s*[\d\-\u2013, ]+)?\)|\b[A-Z][\w'\- .,&]+\s\(\d{4}[a-z]?\)/g;

function findCitationLikeMatches(text) {
  if (!text) return [];
  const matches = text.match(CITATION_LIKE_PATTERN) || [];
  return Array.from(new Set(matches));
}

function normalizeCitationMapEntry(entry) {
  if (!entry || typeof entry !== "object") return null;
  const text_value = cleanText(entry.citation_text);
  const source_title = cleanText(entry.source_title);
  const supporting_quote = cleanText(entry.supporting_quote);
  const sentence_ids = Array.isArray(entry.sentence_ids)
    ? entry.sentence_ids
        .map((value) => {
          if (value === null || value === undefined) return null;
          const text = String(value).trim();
          return text.length > 0 ? text : null;
        })
        .filter((value) => value !== null)
    : [];
  const page = entry.page;
  const evidence_status = cleanText(entry.evidence_status);
  const allowed_statuses = new Set(["checked", "unchecked"]);
  return {
    citation_text: text_value,
    article_sentence: cleanText(entry.article_sentence),
    source_title,
    source_author_or_org: cleanText(entry.source_author_or_org),
    source_year: cleanText(entry.source_year),
    page: page === null || page === undefined || page === "" ? null : String(page),
    sentence_ids,
    supporting_quote,
    verification_note: cleanText(entry.verification_note) || "",
    evidence_status: allowed_statuses.has(evidence_status) ? evidence_status : "unchecked"
  };
}

function validateCitationMapEntry(entry) {
  const issues = [];
  if (!entry.citation_text) issues.push("missing citation_text");
  if (!entry.source_title) issues.push("missing source_title");
  if (!entry.supporting_quote) issues.push("missing supporting_quote");
  if (!entry.article_sentence) issues.push("missing article_sentence");
  return issues;
}

function buildPrompt({ prompt, packages }) {
  const basePrompt = cleanText(prompt) || [
    "Draft complete Medium articles from approved title/subtitle/thumbnail/outline packages.",
    "Use the provided source context when available and do not invent unsupported research claims.",
  ].join("\n");

  const responseInstructions = [
    "Return valid JSON only.",
    "Return JSON only in this shape: {\"results\":[{\"outline_id\":string,\"thumbnail_id\":string,\"subtitle_id\":string,\"candidate_id\":string,\"batch_id\":string,\"source_context_mode\":\"pdf_attachment\"|\"summary_fallback\"|\"checked_summary_evidence\"|\"none\",\"full_text\":string,\"citation_map\":[{\"citation_text\":string,\"article_sentence\":string,\"source_title\":string,\"source_author_or_org\":string,\"source_year\":string|null,\"page\":string|null,\"sentence_ids\":[string],\"supporting_quote\":string|null,\"verification_note\":string,\"evidence_status\":\"checked\"|\"unchecked\"}]}]}",
    "full_text is the public Medium-style Markdown article. The body must use indirect citations or paraphrases only, and must not include direct quotes, internal {{EVID:...}} or [Q1] tags, sentence IDs, or page IDs.",
    "Every reader-facing in-text citation in full_text must appear in citation_map. citation_map entries must include citation_text, article_sentence, source_title, source_author_or_org, source_year (or null), page (or null), sentence_ids (array, possibly empty), supporting_quote (or null), verification_note, and evidence_status.",
    "If a sentence has no direct supporting source sentence or page, leave page or supporting_quote as null, leave sentence_ids as an empty array, mark evidence_status as 'unchecked', and explain the gap in verification_note.",
    "If the article has no reader-facing citations, return citation_map as an empty array.",
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
    if (entry.checked_evidence && entry.checked_evidence.length > 0) {
      lines.push("Checked summary evidence (use only these to ground the article):");
      for (const item of entry.checked_evidence) {
        const ids = (item.sentence_ids || []).join(", ") || "n/a";
        const page = item.page ? `p. ${item.page}` : "page n/a";
        const quote = item.supporting_quote ? `Supporting quote: ${item.supporting_quote}` : "Supporting quote: n/a";
        const claim = item.claim_text ? `Claim: ${item.claim_text}` : "Claim: n/a";
        lines.push(`- ${claim} | ${quote} | ${page} | sentence_ids=[${ids}] | status=${item.selection_status || "n/a"} | confidence=${item.confidence || "n/a"}`);
      }
    }
    if (entry.pdf_path) {
      lines.push("Research PDF: attached as input_file");
    }
    return lines.join("\n");
  }).join("\n\n");

  if (basePrompt.includes("{{input_context}}")) {
    const rendered = basePrompt.replaceAll("{{input_context}}", packageList);
    const unresolved = rendered.match(/\{\{[a-z_]+\}\}/g) ?? [];
    if (unresolved.length) throw new Error(`Unknown full-text prompt variable: ${[...new Set(unresolved)].join(", ")}`);
    return rendered;
  }

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

function parseResults(rawText, packages = []) {
  const warnings = [];
  let parsed;
  try {
    parsed = JSON.parse(stripCodeFences(rawText));
  } catch (error) {
    if (packages.length === 1 && cleanText(rawText) && !isPlaceholderDraft(rawText)) {
      const entry = packages[0];
      const fullText = stripPackageHeader(stripCodeFences(rawText));
      const internalMarkers = findInternalMarkers(fullText);
      if (internalMarkers.length > 0) {
        warnings.push(`internal evidence marker in single-package response: ${internalMarkers.join(", ")}`);
      }
      const citations = findCitationLikeMatches(fullText);
      if (citations.length > 0) warnings.push(`reader-facing citations found without citation_map: ${citations.join("; ")}`);
      return [{
        outline_id: entry.outline_id,
        thumbnail_id: entry.thumbnail_id,
        subtitle_id: entry.subtitle_id,
        candidate_id: entry.candidate_id,
        batch_id: entry.batch_id,
        source_context_mode: entry.source_context_mode ?? "none",
        full_text: fullText,
        citation_map: [],
        warnings
      }];
    }
    throw error;
  }
  const results = Array.isArray(parsed.results) ? parsed.results : [];
  if (results.length === 0 && packages.length === 1 && cleanText(parsed.full_text) && !isPlaceholderDraft(parsed.full_text)) {
    const entry = packages[0];
    const fullText = stripPackageHeader(parsed.full_text);
    const internalMarkers = findInternalMarkers(fullText);
    if (internalMarkers.length > 0) {
      warnings.push(`internal evidence marker in single-object response: ${internalMarkers.join(", ")}`);
    }
    const citations = findCitationLikeMatches(fullText);
    if (citations.length > 0) warnings.push(`reader-facing citations found without citation_map: ${citations.join("; ")}`);
    return [{
      outline_id: entry.outline_id,
      thumbnail_id: entry.thumbnail_id,
      subtitle_id: entry.subtitle_id,
      candidate_id: entry.candidate_id,
      batch_id: entry.batch_id,
      source_context_mode: entry.source_context_mode ?? "none",
      full_text: fullText,
      citation_map: [],
      warnings
    }];
  }
  return results.map((entry) => {
    const entryWarnings = [];
    const fullText = stripPackageHeader(entry.full_text);
    const internalMarkers = findInternalMarkers(fullText);
    if (internalMarkers.length > 0) {
      entryWarnings.push(`internal evidence marker: ${internalMarkers.join(", ")}`);
    }
    const citationMap = Array.isArray(entry.citation_map)
      ? entry.citation_map
          .map((item) => normalizeCitationMapEntry(item))
          .filter((item) => item !== null)
      : [];
    for (const item of citationMap) {
      const issues = validateCitationMapEntry(item);
      if (issues.length > 0) {
        entryWarnings.push(`citation_map entry "${item.citation_text || "(missing)"}" missing fields: ${issues.join(", ")}`);
      }
    }
    const citationTexts = new Set(citationMap.map((item) => item.citation_text).filter(Boolean));
    const articleCitations = findCitationLikeMatches(fullText);
    for (const citation of articleCitations) {
      if (!citationTexts.has(citation)) {
        entryWarnings.push(`reader-facing citation in full_text not present in citation_map: ${citation}`);
      }
    }
    return {
      outline_id: cleanText(entry.outline_id),
      thumbnail_id: cleanText(entry.thumbnail_id),
      subtitle_id: cleanText(entry.subtitle_id),
      candidate_id: cleanText(entry.candidate_id),
      batch_id: cleanText(entry.batch_id),
      source_context_mode: cleanText(entry.source_context_mode) ?? "none",
      full_text: fullText,
      citation_map: citationMap,
      warnings: entryWarnings
    };
  }).filter((entry) => entry.outline_id && entry.thumbnail_id && entry.subtitle_id && entry.candidate_id && entry.batch_id && entry.full_text && !isPlaceholderDraft(entry.full_text));
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
  const reasoningEffort = cleanText(payload.reasoning_effort);
  const reasoningMode = cleanText(payload.reasoning_mode) ?? "standard";
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
        checked_evidence: Array.isArray(entry.checked_evidence) ? entry.checked_evidence : [],
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
          const request = { model, input: await buildResponsesInput({ client, prompt, packages }) };
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
          console.error(`Could not parse full article response: ${error.message}. Raw response preview: ${previewText(rawText)}`);
          process.exitCode = 1;
          return;
        }

        const globalWarnings = [];
        for (const result of results) {
          if (result.warnings && result.warnings.length > 0) {
            globalWarnings.push(`outline_id=${result.outline_id}: ${result.warnings.join("; ")}`);
          }
        }
        if (globalWarnings.length > 0) {
          console.error(`Full-text validation warnings:\n- ${globalWarnings.join("\n- ")}`);
        }

        process.stdout.write(JSON.stringify({
          mode: "api",
          model,
          reasoning_effort: reasoningEffort,
          reasoning_mode: reasoningMode,
          prompt_key: promptKey,
          response_id: response.id ?? null,
          results,
          warnings: globalWarnings,
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
