suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(jsonlite)
})

app_dir <- file.path("apps", "human_preview_rating_app")
source(file.path(app_dir, "R", "text_helpers.R"))
source(file.path(app_dir, "R", "db_helpers.R"))
source(file.path(app_dir, "R", "input_helpers.R"))
source(file.path(app_dir, "R", "app_config.R"))
source(file.path(app_dir, "R", "status_helpers.R"))
source(file.path(app_dir, "R", "scoring_helpers.R"))
source(file.path(app_dir, "R", "title_subtitle_helpers.R"))
source(file.path(app_dir, "R", "id_helpers.R"))
project_root <- normalizePath(".", mustWork = TRUE)
source(file.path(app_dir, "R", "article_lab_config.R"))
source(file.path(app_dir, "R", "schema_research.R"))
source(file.path(app_dir, "R", "schema_article_inbox.R"))
source(file.path(app_dir, "R", "schema_article_lab.R"))
source(file.path(app_dir, "R", "article_inbox_helpers.R"))
source(file.path(app_dir, "R", "api_helpers.R"))
source(file.path(app_dir, "R", "db_article_lab_read_helpers.R"))
source(file.path(app_dir, "R", "db_article_lab_write_helpers.R"))
thumbnail_queue_path <- tempfile(pattern = "missing_thumbnail_queue_")
source(file.path(app_dir, "R", "rating_helpers.R"))

expect <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)
test_db <- tempfile(pattern = "article_production_regression_", fileext = ".sqlite")
on.exit(unlink(test_db, force = TRUE), add = TRUE)
production_db <- normalizePath(file.path("data", "db", "medium_articles.sqlite"), mustWork = FALSE)
expect(!identical(normalizePath(test_db, mustWork = FALSE), production_db), "Regression test must never use the production database.")

con <- dbConnect(SQLite(), test_db)
dbExecute(con, "PRAGMA foreign_keys = ON")
ensure_research_workflow_schema(con)
ensure_article_inbox_schema(con)
ensure_article_lab_schema(con)
expect(nrow(load_candidates(con, exclude_rated = FALSE)) == 0L, "A clean database without the legacy analysis view should return an empty rating candidate set without error.")

timestamp <- "2026-08-01T12:00:00Z"
candidate_a <- create_quick_idea_candidate(con, "E2E Regression Test — Delete After Validation", "Project A core idea", "Audience: cautious beginners", timestamp)
candidate_b <- create_quick_idea_candidate(con, "E2E Isolation Control", "Project B core idea", "Audience: advanced readers", timestamp)
project_a <- develop_article_candidate(con, candidate_a, timestamp)
project_b <- develop_article_candidate(con, candidate_b, timestamp)

project_a_row <- load_article_project(con, article_project_id = project_a)
all_context <- article_project_build_title_context(project_a_row, additional_context = "Prefer practical, non-alarmist framing.")
expect(grepl("Working title:\nE2E Regression Test", all_context, fixed = TRUE), "All-fields context omitted the working title.")
expect(grepl("Core idea / angle:\nProject A core idea", all_context, fixed = TRUE), "All-fields context omitted the core idea.")
expect(grepl("Project notes:\nAudience: cautious beginners", all_context, fixed = TRUE), "All-fields context omitted project notes.")
expect(grepl("Origin details:\nOrigin type: quick_idea", all_context, fixed = TRUE), "All-fields context omitted origin details.")
expect(grepl("Additional directions for this title batch:\nPrefer practical, non-alarmist framing.", all_context, fixed = TRUE), "All-fields context omitted additional directions.")

subset_context <- article_project_build_title_context(project_a_row, included_keys = c("working_title", "origin_details"), additional_context = "One extra direction")
expect(grepl("Working title:", subset_context, fixed = TRUE) && grepl("Origin details:", subset_context, fixed = TRUE), "Selected context fields were not included.")
expect(!grepl("Core idea / angle:", subset_context, fixed = TRUE) && !grepl("Project notes:", subset_context, fixed = TRUE), "Unchecked context fields leaked into the composed prompt.")
expect(sum(gregexpr("One extra direction", subset_context, fixed = TRUE)[[1]] > 0L) == 1L, "Additional context was duplicated in the composed prompt.")

exact_prompt <- article_lab_effective_title_prompt_text(article_lab_default_prompt, 3L, seed_topic = "Test seed", context_notes = subset_context)
expect(grepl("Article idea and supporting context:\nWorking title:", exact_prompt, fixed = TRUE), "The effective API prompt did not resolve the idea-context variable.")
expect(sum(gregexpr("One extra direction", exact_prompt, fixed = TRUE)[[1]] > 0L) == 1L, "Additional context was duplicated in the effective API prompt.")
expect(grepl("Generate exactly 3 titles", exact_prompt, fixed = TRUE), "The exact prompt preview omitted the requested batch size.")
expect(!grepl("{{", exact_prompt, fixed = TRUE), "The exact prompt preview retained an unresolved known variable.")

workflow_templates <- list(
  subtitle = list(template = article_lab_default_subtitle_prompt, variables = list(input_context = "Title: Test", variants_per_title = 4L, max_subtitle_chars = article_lab_subtitle_max_chars)),
  thumbnail = list(template = article_lab_default_thumbnail_prompt, variables = list(input_context = "Title: Test\nSubtitle: Test subtitle", variant_index = 1L, variants_per_package = 3L)),
  outline = list(template = article_lab_default_outline_prompt, variables = list(input_context = "Title: Test\nThumbnail label: Test", context_notes = "Prefer a practical structure.")),
  full_text = list(template = article_lab_default_full_text_prompt, variables = list(input_context = "Title: Test\nApproved outline:\n# Test")),
  medium_tags = list(template = article_lab_default_medium_tags_prompt, variables = list(input_context = "Title: Test\nArticle body:\nTest body")),
  research_summary = list(template = article_lab_default_research_summary_prompt, variables = list(input_context = "Source title: Test paper"))
)
for (workflow_name in names(workflow_templates)) {
  rendered_workflow_prompt <- article_lab_render_prompt_template(workflow_templates[[workflow_name]]$template, workflow_templates[[workflow_name]]$variables)
  expect(!grepl("\\{\\{[a-z_]+\\}\\}", rendered_workflow_prompt, perl = TRUE), sprintf("%s prompt retained an unresolved variable.", workflow_name))
  expect(nzchar(rendered_workflow_prompt), sprintf("%s prompt rendered empty.", workflow_name))
}
unknown_variable_error <- tryCatch({ article_lab_render_prompt_template("Test {{unknown_variable}}", list(input_context = "x")); NULL }, error = identity)
expect(inherits(unknown_variable_error, "error"), "Unknown prompt variables should fail loudly.")

legacy_prompt <- article_lab_effective_title_prompt_text(article_lab_legacy_default_prompt, 3L, seed_topic = "Legacy seed", context_notes = subset_context)
expect(startsWith(legacy_prompt, paste0("Article context:\n", subset_context)), "A saved legacy prompt no longer receives article context.")

batch_a <- save_article_lab_batch(con, article_lab_default_prompt, "Project A", "manual", 1L, "fixture-model", "Project A approved title", generation_mode = "fixture", article_context_notes = all_context, article_project_id = project_a)
batch_b <- save_article_lab_batch(con, article_lab_default_prompt, "Project B", "manual", 1L, "fixture-model", "Project B private title", generation_mode = "fixture", article_context_notes = "Project B context", article_project_id = project_b)
expect(identical(load_article_lab_batches(con, project_a)$batch_id, batch_a), "Project A batch lookup leaked another project's batch.")
expect(identical(load_article_lab_batches(con, project_b)$batch_id, batch_b), "Project B batch lookup leaked another project's batch.")
expect(identical(load_latest_article_lab_batch(con, project_a)$article_context_notes[[1]], all_context), "Saved title batch did not preserve the exact composed article context snapshot.")

title_a <- dbGetQuery(con, "SELECT candidate_id FROM article_lab_title_candidates WHERE batch_id = ?", params = list(batch_a))$candidate_id[[1]]
dbExecute(con, "UPDATE article_lab_title_candidates SET status = 'approved_for_subtitle' WHERE candidate_id = ?", params = list(title_a))
replacement_batch <- save_article_lab_batch(con, article_lab_default_prompt, "Project A regenerated", "manual", 1L, "fixture-model", "Project A regenerated title", generation_mode = "fixture", article_project_id = project_a)
expect(dbGetQuery(con, "SELECT status FROM article_lab_title_candidates WHERE candidate_id = ?", params = list(title_a))$status[[1]] == "approved_for_subtitle", "Regeneration changed the approved title.")
expect(setequal(load_article_lab_batches(con, project_a)$batch_id, c(batch_a, replacement_batch)), "Project A regeneration was not version-preserving.")

subtitle_a <- paste0("sub_", title_a)
dbExecute(con, "INSERT INTO article_lab_subtitle_candidates (subtitle_id, candidate_id, batch_id, created_at, subtitle, status, model, generation_mode) VALUES (?, ?, ?, ?, ?, 'approved', 'fixture-model', 'fixture')", params = list(subtitle_a, title_a, batch_a, timestamp, "Project A subtitle"))
generated_thumbnail_a <- paste0("thumb_generated_", title_a)
dbExecute(con, "INSERT INTO article_lab_thumbnail_candidates (thumbnail_id, subtitle_id, candidate_id, batch_id, created_at, thumbnail_label, thumbnail_data_uri, status, model, generation_mode) VALUES (?, ?, ?, ?, ?, ?, ?, 'generated', 'fixture-model', 'fixture')", params = list(generated_thumbnail_a, subtitle_a, title_a, batch_a, timestamp, "Earlier generated thumbnail", "data:image/svg+xml;base64,PHN2Zy8+"))
expect(subtitle_a %in% load_article_lab_thumbnail_packages(con, batch_a)$subtitle_id, "A package with unapproved generated thumbnails must remain available for another generation batch.")
thumbnail_a <- paste0("thumb_", title_a)
dbExecute(con, "INSERT INTO article_lab_thumbnail_candidates (thumbnail_id, subtitle_id, candidate_id, batch_id, created_at, thumbnail_label, thumbnail_data_uri, status, model, generation_mode) VALUES (?, ?, ?, ?, ?, ?, ?, 'approved', 'fixture-model', 'fixture')", params = list(thumbnail_a, subtitle_a, title_a, batch_a, timestamp, "Project A thumbnail", "data:image/svg+xml;base64,PHN2Zy8+"))
expect(!(subtitle_a %in% load_article_lab_thumbnail_packages(con, batch_a)$subtitle_id), "A package must leave thumbnail generation only after a thumbnail is approved.")
outline_a <- paste0("outline_", title_a)
dbExecute(con, "INSERT INTO article_lab_outlines (outline_id, thumbnail_id, subtitle_id, candidate_id, batch_id, created_at, updated_at, outline_text, status, model, generation_mode) VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'approved', 'fixture-model', 'fixture')", params = list(outline_a, thumbnail_a, subtitle_a, title_a, batch_a, timestamp, timestamp, "# Project A outline\n\n## Evidence"))
draft_a <- paste0("draft_", title_a)
dbExecute(con, "INSERT INTO article_lab_full_text_drafts (full_text_draft_id, outline_id, thumbnail_id, subtitle_id, candidate_id, batch_id, original_generated_text, current_draft_text, status, is_approved, model, generation_mode, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'approved', 1, 'fixture-model', 'fixture', ?, ?)", params = list(draft_a, outline_a, thumbnail_a, subtitle_a, title_a, batch_a, "Project A original draft", "Project A manually edited draft", timestamp, timestamp))
expect(article_lab_update_outlines(con, list(list(outline_id = outline_a, outline_text = "# Approved outline edited", notes = "manual"))) == 1L, "Approved outline edit was silently ignored.")
expect(dbGetQuery(con, "SELECT outline_text FROM article_lab_outlines WHERE outline_id = ?", params = list(outline_a))$outline_text[[1]] == "# Approved outline edited", "Approved outline edit did not persist.")
expect(article_lab_update_full_text_drafts(con, list(list(full_text_draft_id = draft_a, current_draft_text = "Project A approved draft edited", notes = "manual"))) == 1L, "Approved full-text edit was silently ignored.")
expect(dbGetQuery(con, "SELECT current_draft_text FROM article_lab_full_text_drafts WHERE full_text_draft_id = ?", params = list(draft_a))$current_draft_text[[1]] == "Project A approved draft edited", "Approved full-text edit did not persist.")
expect(dbGetQuery(con, "SELECT COUNT(*) AS n FROM article_lab_full_text_draft_revisions WHERE full_text_draft_id = ?", params = list(draft_a))$n[[1]] == 1L, "Approved full-text edit did not record a revision.")

expect(nrow(load_article_lab_subtitle_rows(con, batch_b)) == 0L, "Project B could see Project A subtitles.")
expect(nrow(load_article_lab_thumbnail_rows(con, batch_b)) == 0L, "Project B could see Project A thumbnails.")
expect(nrow(load_article_lab_full_text_rows(con, batch_b)) == 0L, "Project B could see Project A drafts.")
expect(grepl("Project A approved draft edited", load_article_lab_review_publish_rows(con, batch_a)$current_draft_text[[1]], fixed = TRUE), "Review did not read the saved Project A draft.")

dbDisconnect(con)
con <- dbConnect(SQLite(), test_db)
on.exit(if (dbIsValid(con)) dbDisconnect(con), add = TRUE)
dbExecute(con, "PRAGMA foreign_keys = ON")
expect(dbGetQuery(con, "SELECT current_draft_text FROM article_lab_full_text_drafts WHERE full_text_draft_id = ?", params = list(draft_a))$current_draft_text[[1]] == "Project A approved draft edited", "Draft did not survive database reload.")
expect(dbGetQuery(con, "PRAGMA foreign_key_check") |> nrow() == 0L, "Fixture database has foreign-key violations.")

old_root <- project_root
old_key <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
project_root <- tempfile(pattern = "missing_article_helpers_")
dir.create(project_root)
Sys.unsetenv("OPENAI_API_KEY")
failed_titles <- generate_title_candidates(con, "test", 1L, model = "fixture-missing-provider")
expect(identical(failed_titles$mode, "failed") && nrow(failed_titles$titles) == 0L, "Provider failure silently fell back to generated title content.")
project_root <- old_root
if (is.na(old_key)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = old_key)

legacy_db <- tempfile(pattern = "article_production_legacy_", fileext = ".sqlite")
on.exit(unlink(legacy_db, force = TRUE), add = TRUE)
legacy <- dbConnect(SQLite(), legacy_db)
on.exit(if (dbIsValid(legacy)) dbDisconnect(legacy), add = TRUE)
dbExecute(legacy, "CREATE TABLE article_lab_title_batches (batch_id TEXT PRIMARY KEY, created_at TEXT NOT NULL, prompt TEXT NOT NULL, seed_topic TEXT, inspiration_source TEXT, requested_batch_size INTEGER, model TEXT, status TEXT NOT NULL DEFAULT 'generated', notes TEXT, article_context_notes TEXT)")
dbExecute(legacy, "INSERT INTO article_lab_title_batches (batch_id, created_at, prompt, status) VALUES ('legacy_batch', ?, 'legacy prompt', 'generated')", params = list(timestamp))
ensure_research_workflow_schema(legacy)
ensure_article_inbox_schema(legacy)
ensure_article_lab_schema(legacy)
expect("article_project_id" %in% dbGetQuery(legacy, "PRAGMA table_info(article_lab_title_batches)")$name, "Migration did not add article_project_id.")
expect(dbGetQuery(legacy, "SELECT COUNT(*) AS n FROM article_lab_title_batches WHERE batch_id = 'legacy_batch'")$n[[1]] == 1L, "Migration lost a legacy title batch.")
ensure_article_lab_schema(legacy)
expect(dbGetQuery(legacy, "SELECT COUNT(*) AS n FROM article_lab_title_batches WHERE batch_id = 'legacy_batch'")$n[[1]] == 1L, "Rerunning migration was not idempotent.")

message("Article production regression tests passed.")
