article_lab_legacy_default_prompt <- paste(
  "Generate Medium-style article titles for personal finance and investing readers.",
  "The titles should be science-based, beginner-friendly, credible, and clearly useful.",
  "Avoid clickbait, overclaiming, and hype.",
  "Lean into strong emotional tension or curiosity without sounding manipulative.",
  "Prefer specific, human, readable titles that feel plausible on Medium.",
  sep = "\n"
)

article_lab_default_prompt <- paste(
  "You generate Medium-style article title candidates for personal finance and investing.",
  "",
  "Article idea and supporting context:",
  "{{idea_context}}",
  "",
  "Article/research summary (when selected): {{article_summary}}",
  "",
  "Optional seed topic: {{seed_topic}}",
  "Optional inspiration source: {{inspiration_source}}",
  "",
  "Reference title examples (when selected): {{example_titles}}",
  "",
  "Generate exactly {{batch_size}} titles as valid JSON in the shape {\"titles\": [\"...\", \"...\"]}.",
  "Every title must be at most {{max_title_chars}} characters, including spaces.",
  "Prefer {{preferred_title_length}} characters when possible.",
  "The titles should be science-based, beginner-friendly, credible, and clearly useful.",
  "Avoid clickbait, overclaiming, hype, explanations, numbering, markdown, and code fences.",
  "Do not copy reference titles verbatim. Rewrite any title that exceeds the limit instead of truncating it.",
  "Lean into strong emotional tension or curiosity without sounding manipulative.",
  "Prefer specific, human, readable titles that feel plausible on Medium.",
  sep = "\n"
)

article_lab_title_prompt_variables <- c(
  "idea_context", "article_summary", "batch_size", "seed_topic",
  "inspiration_source", "example_titles", "max_title_chars", "preferred_title_length"
)

article_lab_manual_prompt_key <- "manual_default"

article_lab_default_model <- local({
  configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_HEADLINE_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5.6-terra"
  configured
})
article_lab_model_choices <- c(
  "gpt-5.6-sol",
  "gpt-5.6-terra",
  "gpt-5.6-luna",
  "gpt-5.5",
  "gpt-5.4",
  "gpt-5.4-mini",
  "gpt-5.4-nano",
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

# Canonical capability policy for models that may coordinate the Responses API
# built-in image_generation tool. Keep UI choices and request validation derived
# from this single list so the two surfaces cannot drift.
article_lab_image_generation_models <- c(
  "gpt-5.6-sol",
  "gpt-5.6-terra",
  "gpt-5.6-luna",
  "gpt-5.5",
  "gpt-5.4",
  "gpt-5.4-mini"
)

article_lab_validate_image_generation_model <- function(model, context = "Images-tab model") {
  model <- trimws(as.character(model %||% "")[[1]])
  if (!nzchar(model) || !model %in% article_lab_image_generation_models) {
    invalid <- if (nzchar(model)) model else "<empty>"
    stop(
      context, " '", invalid, "' is not supported for the Responses API built-in image_generation tool. ",
      "Allowed image-capable models: ", paste(article_lab_image_generation_models, collapse = ", "), ".",
      call. = FALSE
    )
  }
  model
}

article_lab_reasoning_capabilities <- function(model) {
  model <- trimws(as.character(model %||% "")[[1]])
  if (grepl("^gpt-5\\.6(?:-|$)", model)) return(c("none", "low", "medium", "high", "xhigh", "max"))
  if (grepl("^gpt-5\\.[45](?:-|$)", model)) return(c("none", "low", "medium", "high", "xhigh"))
  if (grepl("^gpt-5(?:-|$)", model)) return(c("minimal", "low", "medium", "high"))
  if (grepl("^(o3|o4)(?:-|$)", model)) return(c("low", "medium", "high"))
  character()
}

article_lab_supports_pro_mode <- function(model) {
  grepl("^gpt-5\\.6(?:-|$)", trimws(as.character(model %||% "")[[1]]))
}

article_lab_reasoning_mode_choices <- c("standard", "pro")
article_lab_generation_env <- function(name, fallback) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (nzchar(value)) value else fallback
}
article_lab_generation_workflows <- list(
  titles = list(model_id = "article_lab_model", reasoning_id = "article_lab_reasoning_effort", mode_id = "article_lab_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_TITLE_GENERATION_REASONING_EFFORT", "low"), default_mode = article_lab_generation_env("OPENAI_TITLE_GENERATION_REASONING_MODE", "standard")),
  scoring = list(model_id = "article_lab_score_model", reasoning_id = "article_lab_score_reasoning_effort", mode_id = "article_lab_score_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_TITLE_SCORING_REASONING_EFFORT", "low"), default_mode = article_lab_generation_env("OPENAI_TITLE_SCORING_REASONING_MODE", "standard")),
  subtitles = list(model_id = "article_lab_subtitle_model", reasoning_id = "article_lab_subtitle_reasoning_effort", mode_id = "article_lab_subtitle_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_SUBTITLE_GENERATION_REASONING_EFFORT", "low"), default_mode = article_lab_generation_env("OPENAI_SUBTITLE_GENERATION_REASONING_MODE", "standard")),
  thumbnails = list(model_id = "article_lab_thumbnail_model", reasoning_id = "article_lab_thumbnail_reasoning_effort", mode_id = "article_lab_thumbnail_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_THUMBNAIL_GENERATION_REASONING_EFFORT", "none"), default_mode = article_lab_generation_env("OPENAI_THUMBNAIL_GENERATION_REASONING_MODE", "standard")),
  outlines = list(model_id = "article_lab_outline_model", reasoning_id = "article_lab_outline_reasoning_effort", mode_id = "article_lab_outline_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_OUTLINE_GENERATION_REASONING_EFFORT", "medium"), default_mode = article_lab_generation_env("OPENAI_OUTLINE_GENERATION_REASONING_MODE", "standard")),
  full_text = list(model_id = "article_lab_full_text_model", reasoning_id = "article_lab_full_text_reasoning_effort", mode_id = "article_lab_full_text_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_FULL_TEXT_GENERATION_REASONING_EFFORT", "medium"), default_mode = article_lab_generation_env("OPENAI_FULL_TEXT_GENERATION_REASONING_MODE", "standard")),
  research_summary = list(model_id = "research_summary_model", reasoning_id = "research_summary_reasoning_effort", mode_id = "research_summary_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_RESEARCH_SUMMARY_REASONING_EFFORT", "medium"), default_mode = article_lab_generation_env("OPENAI_RESEARCH_SUMMARY_REASONING_MODE", "standard")),
  research_claims = list(model_id = "research_claim_model", reasoning_id = "research_claim_reasoning_effort", mode_id = "research_claim_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_CLAIM_EXTRACTION_REASONING_EFFORT", "medium"), default_mode = article_lab_generation_env("OPENAI_CLAIM_EXTRACTION_REASONING_MODE", "standard")),
  research_evidence = list(model_id = "research_evidence_model", reasoning_id = "research_evidence_reasoning_effort", mode_id = "research_evidence_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_EVIDENCE_REASONING_EFFORT", "medium"), default_mode = article_lab_generation_env("OPENAI_EVIDENCE_REASONING_MODE", "standard")),
  research_evidence_fallback = list(model_id = "research_evidence_fallback_model", reasoning_id = "research_evidence_fallback_reasoning_effort", mode_id = "research_evidence_fallback_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_EVIDENCE_FALLBACK_REASONING_EFFORT", "medium"), default_mode = article_lab_generation_env("OPENAI_EVIDENCE_FALLBACK_REASONING_MODE", "standard")),
  medium_tags = list(model_id = "article_lab_medium_tags_model", reasoning_id = "article_lab_medium_tags_reasoning_effort", mode_id = "article_lab_medium_tags_reasoning_mode", default_reasoning = article_lab_generation_env("OPENAI_MEDIUM_TAGS_REASONING_EFFORT", "low"), default_mode = article_lab_generation_env("OPENAI_MEDIUM_TAGS_REASONING_MODE", "standard"))
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
article_lab_builtin_thumbnail_model <- "gpt-5.4-mini"
article_lab_default_thumbnail_model <- local({
  configured <- Sys.getenv("OPENAI_THUMBNAIL_GENERATION_MODEL", unset = "")
  source_name <- "OPENAI_THUMBNAIL_GENERATION_MODEL"
  if (!nzchar(configured)) {
    configured <- Sys.getenv("OPENAI_THUMBNAIL_RESPONSES_MODEL", unset = "")
    source_name <- "OPENAI_THUMBNAIL_RESPONSES_MODEL"
  }
  if (!nzchar(configured)) {
    configured <- article_lab_builtin_thumbnail_model
    source_name <- "built-in Images-tab default"
  }
  article_lab_validate_image_generation_model(
    configured,
    paste0("Configured Images-tab default from ", source_name)
  )
})
article_lab_thumbnail_model_choices <- article_lab_image_generation_models
article_lab_thumbnail_size_choices <- c("Landscape (1536x1024)" = "1536x1024", "Square (1024x1024)" = "1024x1024", "Portrait (1024x1536)" = "1024x1536", "OpenAI API default" = "auto")
article_lab_thumbnail_quality_choices <- c("Low" = "low", "Medium" = "medium", "High" = "high", "OpenAI API default" = "auto")
article_lab_thumbnail_output_format_choices <- c("PNG" = "png", "WebP" = "webp", "JPEG" = "jpeg")
article_lab_thumbnail_background_choices <- c("OpenAI API default" = "auto", "Opaque" = "opaque", "Transparent" = "transparent")
article_lab_default_thumbnail_size <- "1536x1024"
article_lab_default_thumbnail_quality <- "low"
article_lab_default_thumbnail_output_format <- "png"
article_lab_default_thumbnail_background <- "auto"
article_lab_default_outline_model <- local({
  configured <- Sys.getenv("OPENAI_OUTLINE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_SUBTITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})
article_lab_outline_helper_timeout_seconds <- article_lab_positive_integer_env("OPENAI_OUTLINE_GENERATION_TIMEOUT_SECONDS", 180L, 30L)
article_lab_outline_model_choices <- article_lab_model_choices_with_default(article_lab_default_outline_model)
article_lab_default_full_text_model <- local({
  configured <- Sys.getenv("OPENAI_FULL_TEXT_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_OUTLINE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})
article_lab_full_text_helper_timeout_seconds <- article_lab_positive_integer_env("OPENAI_FULL_TEXT_GENERATION_TIMEOUT_SECONDS", 300L, 60L)
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
  "",
  "Source metadata used for this request:",
  "{{input_context}}",
  sep = "\n"
)
article_lab_legacy_default_research_summary_prompt <- sub("\n\nSource metadata used for this request:[\\s\\S]*$", "", article_lab_default_research_summary_prompt, perl = TRUE)
article_lab_default_claim_extraction_model <- local({
  configured <- Sys.getenv("OPENAI_CLAIM_EXTRACTION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5.4-mini"
  configured
})
article_lab_claim_extraction_model_choices <- article_lab_model_choices_with_default(
  article_lab_default_claim_extraction_model,
  base_choices = c("gpt-5.6-sol", "gpt-5.6-terra", "gpt-5.6-luna", "gpt-5.4-mini", "gpt-5-mini", "gpt-5-nano", "gpt-5.4", "gpt-5")
)
article_lab_default_evidence_selection_model <- local({
  configured <- Sys.getenv("OPENAI_EVIDENCE_SELECTION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5.4-mini"
  configured
})
article_lab_evidence_selection_model_choices <- article_lab_model_choices_with_default(
  article_lab_default_evidence_selection_model,
  base_choices = c("gpt-5.6-sol", "gpt-5.6-terra", "gpt-5.6-luna", "gpt-5.4-mini", "gpt-5-mini", "gpt-5-nano", "gpt-5.4", "gpt-5")
)
article_lab_default_evidence_fallback_model <- local({
  configured <- Sys.getenv("OPENAI_EVIDENCE_FALLBACK_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5.4"
  configured
})
article_lab_evidence_fallback_model_choices <- article_lab_model_choices_with_default(
  article_lab_default_evidence_fallback_model,
  base_choices = c("gpt-5.6-sol", "gpt-5.6-terra", "gpt-5.6-luna", "gpt-5.4", "gpt-5", "gpt-5.4-mini", "gpt-5-mini")
)
article_lab_evidence_reasoning_choices <- c("minimal", "low", "medium")
article_lab_default_evidence_reasoning_effort <- Sys.getenv("OPENAI_EVIDENCE_REASONING_EFFORT", unset = "low")
if (!article_lab_default_evidence_reasoning_effort %in% article_lab_evidence_reasoning_choices) {
  stop(
    "Invalid OPENAI_EVIDENCE_REASONING_EFFORT: '", article_lab_default_evidence_reasoning_effort, "'. ",
    "Supported values are: ", paste(article_lab_evidence_reasoning_choices, collapse = ", "), ".",
    call. = FALSE
  )
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
  "Use the attached article summary when available to ground subtitles in the paper's actual content.",
  "",
  "Titles and attached summaries:",
  "{{input_context}}",
  sep = "\n"
)
article_lab_default_thumbnail_prompt <- paste(
  "Generate Medium-style thumbnail candidate concepts for approved title and subtitle packages.",
  "Keep the visual direction clear, editorial, credible, and readable at a glance.",
  "Return data that can be rendered into preview-card style thumbnail concepts.",
  "Keep the concept aligned with the title and subtitle without adding clickbait or clutter.",
  "",
  "Selected package:",
  "{{input_context}}",
  sep = "\n"
)
article_lab_default_outline_prompt <- paste(
  "Generate a practical Medium article outline for the approved title, subtitle, and thumbnail concept.",
  "Use Markdown headings and bullets. Include a short hook, 4-6 main sections, key points for each section, and a concise closing angle.",
  "Keep it reader-facing, credible, specific, and useful. Do not draft the full article yet.",
  "",
  "Author context notes:",
  "{{context_notes}}",
  "",
  "Selected packages and research context:",
  "{{input_context}}",
  sep = "\n"
)
article_lab_legacy_default_outline_prompt <- sub("\n\nAuthor context notes:[\\s\\S]*$", "", article_lab_default_outline_prompt, perl = TRUE)
article_lab_outline_prompt_key <- "outline_default"
article_lab_default_full_text_prompt <- paste(
  "Draft a complete Medium article from the approved package and outline.",
  "Use the title, subtitle, thumbnail concept, approved outline, and the available source context.",
  "Default to the source material when it is available; do not invent research claims beyond the provided context.",
  "Write in clear, beginner-friendly personal finance language with practical examples, caveats, and a measured conclusion.",
  "The article body should be Markdown. Do not include notes or explanations inside the article draft.",
  "",
  "Reader-facing citation rules:",
  "- Use indirect citations and paraphrases by default. Do not include direct quotes unless the prompt or outline explicitly asks for a direct quote.",
  "- Use readable Medium-friendly, APA-inspired in-text citations such as 'Vanguard (2019) argues that ...' or '... (Vanguard, 2019)'.",
  "- Add a page number like (Vanguard, 2019, p. 7) only when it helps the reader verify a precise paraphrase. Do not make the whole article feel like an academic paper.",
  "- Do not invent authors, organizations, years, page numbers, statistics, findings, or references. If evidence is weak or missing, soften the claim, mark it as [needs verification], or omit it.",
  "- Do not include internal evidence tags such as {{EVID:...}}, [Q1], or sentence or page IDs in the public article text. Page and sentence IDs belong only in the citation_map below.",
  "",
  "Internal citation_map rules:",
  "- Every reader-facing in-text citation in the article must appear in the citation_map array.",
  "- For each citation, return one citation_map entry that includes:",
  "  - citation_text: the exact in-text citation as it appears in the article, e.g. '(Vanguard, 2019)' or 'Vanguard (2019)'.",
  "  - article_sentence: the article sentence (or short paragraph) that contains the citation, copied from full_text.",
  "  - source_title: the source title from the provided context.",
  "  - source_author_or_org: the author or organization as it appears in the citation.",
  "  - source_year: the publication year, or null if not available.",
  "  - page: the page number if known, or null.",
  "  - sentence_ids: the internal PDF sentence IDs that support the paraphrase, or an empty array if not available.",
  "  - supporting_quote: a short quote from the source that supports the paraphrase, or null if not available.",
  "  - verification_note: a short note flagging any uncertainty, e.g. 'Supports the paraphrase, but does not prove X.' or 'Needs manual verification: page not yet mapped.'.",
  "  - evidence_status: 'checked' when the paraphrase is grounded in a confirmed summary claim or verified evidence row, otherwise 'unchecked'.",
  "- Return an empty citation_map when the article contains no reader-facing citations.",
  "",
  "Selected approved package and source context:",
  "{{input_context}}",
  sep = "\n"
)
article_lab_legacy_default_full_text_prompt <- sub("\n\nSelected approved package and source context:[\\s\\S]*$", "", article_lab_default_full_text_prompt, perl = TRUE)
article_lab_full_text_prompt_key <- "full_text_default"
article_lab_default_medium_tags_prompt <- paste(
  "Generate Medium tags for the approved article package.",
  "Return valid JSON only in the shape {\"tags\":[\"...\",\"...\"]}.",
  "Return exactly 5 tags unless fewer are clearly appropriate.",
  "Each tag must be short, specific, Medium-friendly, and useful for personal finance or investing readers.",
  "Do not include hashtags, numbering, markdown, or explanations.",
  "",
  "Approved article package:",
  "{{input_context}}",
  sep = "\n"
)
article_lab_default_medium_tags_model <- local({
  configured <- Sys.getenv("OPENAI_MEDIUM_TAGS_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})
article_lab_medium_tags_model_choices <- article_lab_model_choices_with_default(article_lab_default_medium_tags_model)

article_lab_render_prompt_template <- function(template, variables = list()) {
  rendered <- article_lab_input_multiline(template)
  if (is.null(rendered) || is.na(rendered)) return("")
  values <- lapply(variables, function(value) {
    cleaned <- article_lab_input_multiline(value)
    if (is.null(cleaned) || is.na(cleaned)) "" else cleaned
  })
  for (key in names(values)) {
    if (!nzchar(values[[key]])) rendered <- gsub(sprintf("(?m)^[^\\n]*\\{\\{%s\\}\\}[^\\n]*\\n?", key), "", rendered, perl = TRUE)
    rendered <- gsub(sprintf("{{%s}}", key), values[[key]], rendered, fixed = TRUE)
  }
  unresolved <- unique(regmatches(rendered, gregexpr("\\{\\{[a-z_]+\\}\\}", rendered, perl = TRUE))[[1]])
  unresolved <- unresolved[nzchar(unresolved) & unresolved != "-1"]
  if (length(unresolved) > 0L) stop(sprintf("Unknown prompt variable%s: %s", ifelse(length(unresolved) == 1L, "", "s"), paste(unresolved, collapse = ", ")), call. = FALSE)
  trimws(gsub("\\n{3,}", "\n\n", rendered, perl = TRUE))
}

article_lab_prompt_variable_help <- function(...) {
  variables <- unlist(list(...), use.names = FALSE)
  sprintf("Available variables: %s. The exact resolved prompt below is what is sent to the API.", paste(sprintf("{{%s}}", variables), collapse = ", "))
}
article_lab_default_score_prompt_version <- "v2_2"
article_lab_default_score_scope <- "title_only"
article_lab_default_score_prompt <- paste(
  "Score the reader-facing pre-click appeal of this Medium finance title.",
  "Use only the supplied title. Do not infer a subtitle or use performance history.",
  "Do not estimate click-through rate. Calibrate scores relative to typical Medium personal-finance articles and use the full 1-5 scale.",
  "Score curiosity, emotional_pull, medium_comment_potential, overall_article_potential, and trust_risk.",
  "Return JSON matching the supplied response schema exactly; short_reason must be one short sentence.",
  "",
  "Prompt version: {{prompt_version}}",
  "Score scope: {{scope}}",
  "Title: {{title}}",
  sep = "\n"
)
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


list_article_lab_prompt_keys <- function(con, default_key = article_lab_manual_prompt_key) {
  if (!dbExistsTable(con, "article_lab_prompts")) return(default_key)
  rows <- dbGetQuery(con, "
    SELECT prompt_key
    FROM article_lab_prompts
    WHERE prompt_key IS NOT NULL AND TRIM(prompt_key) <> ''
    ORDER BY updated_at DESC, prompt_key ASC
  ")
  keys <- unique(c(default_key, rows$prompt_key %||% character()))
  keys[nzchar(keys)]
}

load_article_lab_prompt <- function(con, prompt_key = article_lab_manual_prompt_key, default_prompt = article_lab_default_prompt) {
  key <- article_lab_input_string(prompt_key) %||% article_lab_manual_prompt_key
  fallback <- article_lab_input_multiline(default_prompt) %||% article_lab_default_prompt
  if (!dbExistsTable(con, "article_lab_prompts")) return(fallback)
  rows <- dbGetQuery(con, "
    SELECT prompt_text
    FROM article_lab_prompts
    WHERE prompt_key = ?
    LIMIT 1
  ", params = list(key))
  if (nrow(rows) == 0) return(fallback)
  stored <- article_lab_input_multiline(rows$prompt_text[[1]]) %||% fallback
  legacy_default <- switch(
    key,
    manual_default = article_lab_legacy_default_prompt,
    outline_default = article_lab_legacy_default_outline_prompt,
    full_text_default = article_lab_legacy_default_full_text_prompt,
    NULL
  )
  if (!is.null(legacy_default) && identical(stored, legacy_default)) fallback else stored
}

save_article_lab_prompt <- function(con, prompt_text, prompt_key = article_lab_manual_prompt_key, default_prompt = article_lab_default_prompt) {
  key <- article_lab_input_string(prompt_key) %||% article_lab_manual_prompt_key
  text <- article_lab_input_multiline(prompt_text) %||% (article_lab_input_multiline(default_prompt) %||% article_lab_default_prompt)
  timestamp <- now_utc()
  rows <- dbGetQuery(con, "SELECT prompt_key FROM article_lab_prompts WHERE prompt_key = ? LIMIT 1", params = list(key))
  if (nrow(rows) > 0) {
    dbExecute(con, "
      UPDATE article_lab_prompts
      SET updated_at = ?, prompt_text = ?
      WHERE prompt_key = ?
    ", params = list(timestamp, text, key))
    return(invisible(key))
  }
  dbExecute(con, "
    INSERT INTO article_lab_prompts (prompt_key, created_at, updated_at, prompt_text)
    VALUES (?, ?, ?, ?)
  ", params = list(key, timestamp, timestamp, text))
  invisible(key)
}
