article_lab_default_prompt <- paste(
  "Generate Medium-style article titles for personal finance and investing readers.",
  "The titles should be science-based, beginner-friendly, credible, and clearly useful.",
  "Avoid clickbait, overclaiming, and hype.",
  "Lean into strong emotional tension or curiosity without sounding manipulative.",
  "Prefer specific, human, readable titles that feel plausible on Medium.",
  sep = "\n"
)

article_lab_manual_prompt_key <- "manual_default"

article_lab_default_model <- local({
  configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_HEADLINE_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})
article_lab_model_choices <- c(
  "gpt-5",
  "gpt-5-mini",
  "gpt-5-nano",
  "gpt-4.1",
  "gpt-4.1-mini",
  "gpt-4.1-nano",
  "o3",
  "o3-mini",
  "o4-mini"
)
article_lab_model_choices_with_default <- function(default_model, base_choices = article_lab_model_choices) {
  default_model <- as.character(default_model %||% "")
  default_model <- trimws(default_model[[1]])
  if (nzchar(default_model) && !default_model %in% base_choices) {
    return(c(default_model, base_choices))
  }
  base_choices
}
article_lab_title_generation_model_choices <- article_lab_model_choices_with_default(article_lab_default_model)
article_lab_default_score_model <- local({
  configured <- Sys.getenv("OPENAI_TITLE_SCORING_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})
article_lab_score_model_choices <- article_lab_model_choices_with_default(article_lab_default_score_model)
article_lab_default_subtitle_model <- local({
  configured <- Sys.getenv("OPENAI_SUBTITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})
article_lab_subtitle_model_choices <- article_lab_model_choices_with_default(article_lab_default_subtitle_model)
article_lab_default_thumbnail_model <- local({
  configured <- Sys.getenv("OPENAI_THUMBNAIL_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_THUMBNAIL_RESPONSES_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_SUBTITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5.5"
  configured
})
article_lab_thumbnail_model_choices <- article_lab_model_choices_with_default(article_lab_default_thumbnail_model, base_choices = c("gpt-5.5", "gpt-5.4", "gpt-5.4-mini"))
article_lab_default_outline_model <- local({
  configured <- Sys.getenv("OPENAI_OUTLINE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_SUBTITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})
article_lab_outline_helper_timeout_seconds <- max(30L, suppressWarnings(as.integer(Sys.getenv("OPENAI_OUTLINE_GENERATION_TIMEOUT_SECONDS", unset = "180"))) %||% 180L)
article_lab_outline_model_choices <- article_lab_model_choices_with_default(article_lab_default_outline_model)
article_lab_default_full_text_model <- local({
  configured <- Sys.getenv("OPENAI_FULL_TEXT_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_OUTLINE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})
article_lab_full_text_helper_timeout_seconds <- max(60L, suppressWarnings(as.integer(Sys.getenv("OPENAI_FULL_TEXT_GENERATION_TIMEOUT_SECONDS", unset = "300"))) %||% 300L)
article_lab_full_text_model_choices <- article_lab_model_choices_with_default(article_lab_default_full_text_model)
article_lab_default_research_summary_model <- local({
  configured <- Sys.getenv("OPENAI_RESEARCH_SUMMARY_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})
article_lab_research_summary_model_choices <- article_lab_model_choices_with_default(article_lab_default_research_summary_model)
article_lab_default_research_summary_prompt_version <- "research_summary_v1"
article_lab_research_summary_prompt_version_choices <- c(
  "research_summary_v1"
)
if (!article_lab_default_research_summary_prompt_version %in% article_lab_research_summary_prompt_version_choices) {
  article_lab_research_summary_prompt_version_choices <- c(article_lab_default_research_summary_prompt_version, article_lab_research_summary_prompt_version_choices)
}
article_lab_default_research_summary_prompt <- paste(
  "Summarize this research paper for a beginner-friendly, evidence-based personal finance writing workflow.",
  "",
  "Do not write an article. Do not generate titles. Do not overclaim. Separate what the paper says from what an investor might infer. Be transparent about uncertainty and limitations.",
  "",
  "Use each section heading on its own line, with a blank line after the heading and blank lines between paragraphs or bullet groups so the saved draft remains easy to edit. Do not put body text on the same line as a heading.",
  "",
  "Use this exact structure:",
  "",
  "Short summary:",
  "",
  "Main findings:",
  "",
  "Why it matters for investors:",
  "",
  "Interesting details:",
  "",
  "Caveats / limitations:",
  "",
  "What not to overclaim:",
  "",
  "Possible article directions:",
  sep = "\n"
)
article_lab_default_claim_extraction_model <- local({
  configured <- Sys.getenv("OPENAI_CLAIM_EXTRACTION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5.4-mini"
  configured
})
article_lab_claim_extraction_model_choices <- article_lab_model_choices_with_default(
  article_lab_default_claim_extraction_model,
  base_choices = c("gpt-5.4-mini", "gpt-5-mini", "gpt-5-nano", "gpt-5.4", "gpt-5")
)
article_lab_default_evidence_selection_model <- local({
  configured <- Sys.getenv("OPENAI_EVIDENCE_SELECTION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5.4-mini"
  configured
})
article_lab_evidence_selection_model_choices <- article_lab_model_choices_with_default(
  article_lab_default_evidence_selection_model,
  base_choices = c("gpt-5.4-mini", "gpt-5-mini", "gpt-5-nano", "gpt-5.4", "gpt-5")
)
article_lab_default_evidence_fallback_model <- local({
  configured <- Sys.getenv("OPENAI_EVIDENCE_FALLBACK_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5.4"
  configured
})
article_lab_evidence_fallback_model_choices <- article_lab_model_choices_with_default(
  article_lab_default_evidence_fallback_model,
  base_choices = c("gpt-5.4", "gpt-5", "gpt-5.4-mini", "gpt-5-mini")
)
article_lab_evidence_reasoning_choices <- c("minimal", "low", "medium")
article_lab_default_evidence_reasoning_effort <- Sys.getenv("OPENAI_EVIDENCE_REASONING_EFFORT", unset = "low")
if (!article_lab_default_evidence_reasoning_effort %in% article_lab_evidence_reasoning_choices) {
  article_lab_default_evidence_reasoning_effort <- "low"
}
article_lab_default_claim_extraction_prompt <- paste(
  "Identify atomic factual claims in the numbered summary sentences that should eventually be supported by the source paper.",
  "Return JSON only in this exact shape: {\"claims\":[{\"sentence_index\":1,\"claim_text\":\"atomic claim text\",\"original_text\":\"exact source summary sentence or bullet\",\"placement_hint\":\"after_sentence\",\"importance\":\"high|medium|low\"}]}",
  "Rules:",
  "- Return at most {{max_claims}} atomic claims.",
  "- Select only from the numbered summary sentences provided below.",
  "- If a sentence or bullet contains multiple factual claims, split it into smaller atomic claims.",
  "- Atomic claims may be shorter than the original sentence, but they must preserve the original meaning and numbers.",
  "- Do not add facts that are not present in the summary.",
  "- Put the exact original numbered sentence/bullet in original_text.",
  "- Use placement_hint \"after_sentence\" unless there is an obvious clause-level placement.",
  "- Prefer concrete measured findings, data, causal/mechanism claims, and source limitations.",
  "- Avoid headings, article ideas, generic commentary, and sentences that do not assert a source-checkable fact.",
  "",
  "Source metadata:",
  "Research source ID: {{research_source_id}}",
  "Source title: {{source_title}}",
  "Summary ID: {{summary_id}}",
  "",
  "Numbered summary sentences:",
  "{{summary_sentence_payload_json}}",
  sep = "\n"
)
article_lab_default_evidence_selection_prompt <- paste(
  "Choose up to three exact source sentence IDs that best support each atomic summary claim.",
  "Return JSON only in this exact shape: {\"results\":[{\"claim_id\":1,\"sentence_ids\":[123,456],\"support_status\":\"supports|partially_supports|generally_supported_no_direct_quote|weak_support|contradicts|no_match\",\"confidence\":\"high|medium|low|none\",\"reason\":\"short reason\"}]}",
  "Rules:",
  "- Select only from the provided candidate sentences.",
  "- Return sentence_ids as an empty array when no direct quote should be selected.",
  "- Do not quote, paraphrase, rewrite, or invent source text; choose IDs only.",
  "- Do not use no_match just because one sentence does not support every detail; use up to three sentences or partially_supports when appropriate.",
  "- Use supports when selected sentence(s) directly support the atomic claim.",
  "- Use partially_supports when selected sentence(s) support part, but not all, of the claim.",
  "- Use generally_supported_no_direct_quote when the candidates indicate broad support but no clean quote sentence should be selected.",
  "- Use weak_support for related but indirect evidence.",
  "- Use contradicts if candidate evidence conflicts with the claim.",
  "- Use no_match only when candidates provide no useful support.",
  "- Prefer a sentence that directly states the evidence over one that only hints at it.",
  "- Page numbers are metadata only; do not use them as evidence.",
  "",
  "Claims and candidate sentences:",
  "{{claim_candidate_payload_json}}",
  sep = "\n"
)
article_lab_default_subtitle_prompt <- paste(
  "Generate Medium-style subtitle candidates for approved personal finance and investing article titles.",
  "Return valid JSON only.",
  "Use this exact shape:",
  "{\"results\":[{\"candidate_id\":\"...\",\"batch_id\":\"...\",\"subtitles\":[\"...\",\"...\"]}]}",
  "Return exactly the requested number of subtitle candidates per title.",
  "Every subtitle must be at most 90 characters, including spaces.",
  "Keep subtitles clear, credible, useful, and not sensational.",
  "Do not repeat the title verbatim.",
  "Do not include numbering, markdown, or explanations.",
  sep = "\n"
)
article_lab_default_thumbnail_prompt <- paste(
  "Generate Medium-style thumbnail candidate concepts for approved title and subtitle packages.",
  "Keep the visual direction clear, editorial, credible, and readable at a glance.",
  "Return data that can be rendered into preview-card style thumbnail concepts.",
  "Keep the concept aligned with the title and subtitle without adding clickbait or clutter.",
  sep = "\n"
)
article_lab_default_outline_prompt <- paste(
  "Generate a practical Medium article outline for the approved title, subtitle, and thumbnail concept.",
  "Use Markdown headings and bullets. Include a short hook, 4-6 main sections, key points for each section, and a concise closing angle.",
  "Keep it reader-facing, credible, specific, and useful. Do not draft the full article yet.",
  sep = "\n"
)
article_lab_outline_prompt_key <- "outline_default"
article_lab_default_full_text_prompt <- paste(
  "Draft a complete Medium article from the approved package and outline.",
  "Use the title, subtitle, thumbnail concept, approved outline, and available source context.",
  "Default to the source material when it is available; do not invent research claims beyond the provided context.",
  "Write in clear, beginner-friendly personal finance language with practical examples, caveats, and a measured conclusion.",
  "The article body should be Markdown. Do not include notes or explanations inside the article draft.",
  sep = "\n"
)
article_lab_full_text_prompt_key <- "full_text_default"
article_lab_default_medium_tags_prompt <- paste(
  "Generate Medium tags for the approved article package.",
  "Return valid JSON only in the shape {\"tags\":[\"...\",\"...\"]}.",
  "Return exactly 5 tags unless fewer are clearly appropriate.",
  "Each tag must be short, specific, Medium-friendly, and useful for personal finance or investing readers.",
  "Do not include hashtags, numbering, markdown, or explanations.",
  sep = "\n"
)
article_lab_default_medium_tags_model <- local({
  configured <- Sys.getenv("OPENAI_MEDIUM_TAGS_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})
article_lab_medium_tags_model_choices <- article_lab_model_choices_with_default(article_lab_default_medium_tags_model)
article_lab_default_score_prompt_version <- "v2_2"
article_lab_default_score_scope <- "title_only"
article_lab_all_batches_value <- "__all_article_lab_batches__"
article_lab_default_thumbnail_variants <- 3L
article_lab_publish_target_choices <- c(
  "Publish on my own Medium profile",
  "Submit to Medium publication",
  "Publish on own website",
  "Both Medium and own website",
  "Do not publish yet"
)
article_lab_monetization_choices <- c(
  "Member-only / monetized",
  "Free article",
  "Undecided"
)
