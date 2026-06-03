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
