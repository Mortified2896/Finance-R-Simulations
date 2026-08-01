article_lab_workflow_sections <- c(
  "research_inbox",
  "summary",
  "article_evidence",
  "title_lab",
  "generate",
  "api_scoring",
  "subtitle_generation",
  "thumbnails",
  "outline",
  "full_text",
  "review_publish"
)
article_lab_page_meta <- list(
  home = list(
    nav_title = "Home",
    nav_subtitle = "Current rating workflow"
  ),
  research_inbox = list(
    nav_title = "Article Inbox",
    nav_subtitle = "Capture ideas and explore research angles",
    title = "Article Inbox",
    subtitle = "Capture ideas and explore research angles."
  ),
  summary = list(
    nav_title = "Summary",
    nav_subtitle = "Check paper summary",
    title = "Article Lab - Summary",
    subtitle = "Check paper summary."
  ),
  article_evidence = list(
    nav_title = "Article Evidence",
    nav_subtitle = "Build the evidence base",
    title = "Article Evidence",
    subtitle = "Connect an article idea to sources, claims, counterarguments, and open research questions."
  ),
  title_lab = list(
    nav_title = "Title Lab",
    nav_subtitle = "Generate, score, and shortlist",
    title = "Title Lab",
    subtitle = "Generate, review, score, compare, shortlist, and approve article titles."
  ),
  generate = list(
    nav_title = "Generate",
    nav_subtitle = "Generate & triage titles",
    title = "Article Lab – Generate",
    subtitle = "Generate title candidates, disqualify bad-fit ideas, and move selected titles to the API queue."
  ),
  api_scoring = list(
    nav_title = "API Scoring",
    nav_subtitle = "Score with API & approve",
    title = "Article Lab – API Scoring",
    subtitle = "Score queued titles with the API and manually approve selected titles for subtitle generation."
  ),
  subtitle_generation = list(
    nav_title = "Subtitle Generation",
    nav_subtitle = "Generate subtitles",
    title = "Article Lab – Subtitle Generation",
    subtitle = "Generate subtitle candidates for approved titles."
  ),
  thumbnails = list(
    nav_title = "Thumbnails",
    nav_subtitle = "Generate thumbnails",
    title = "Article Lab – Thumbnails",
    subtitle = "Create and evaluate thumbnail candidates."
  ),
  outline = list(
    nav_title = "Outline",
    nav_subtitle = "Create article outline",
    title = "Article Lab – Outline",
    subtitle = "Build the article structure before drafting."
  ),
  full_text = list(
    nav_title = "Full Text",
    nav_subtitle = "Write full article",
    title = "Article Lab – Full Text",
    subtitle = "Draft the full article."
  ),
  review_publish = list(
    nav_title = "Review & Publish",
    nav_subtitle = "Prepare publishing",
    title = "Article Lab – Review & Publish",
    subtitle = "Set publishing metadata, export/copy the approved draft, and track publishing status."
  )
)

article_lab_nav_meta <- function(section) {
  meta <- article_lab_page_meta[[section]]
  if (is.null(meta)) {
    list(nav_title = section, nav_subtitle = "")
  } else {
    meta
  }
}

article_lab_is_workflow_section <- function(section) {
  !is.na(section) && identical(length(section), 1L) && section %in% article_lab_workflow_sections
}

article_lab_row_input_id <- function(prefix, candidate_id) {
  paste0(prefix, "_", gsub("[^A-Za-z0-9]+", "_", candidate_id))
}
