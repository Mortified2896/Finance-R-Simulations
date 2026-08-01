suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

app_dir <- file.path("apps", "human_preview_rating_app")
source(file.path(app_dir, "R", "text_helpers.R"))
source(file.path(app_dir, "R", "input_helpers.R"))
source(file.path(app_dir, "R", "app_config.R"))
source(file.path(app_dir, "R", "db_helpers.R"))
source(file.path(app_dir, "R", "id_helpers.R"))
project_root <- normalizePath(".", mustWork = TRUE)
source(file.path(app_dir, "R", "article_lab_config.R"))
source(file.path(app_dir, "R", "prompt_template_helpers.R"))
source(file.path(app_dir, "R", "schema_article_lab.R"))
source(file.path(app_dir, "R", "api_helpers.R"))

expect <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)
expect(identical(article_lab_reasoning_capabilities("gpt-5.6-terra"), c("none", "low", "medium", "high", "xhigh", "max")), "GPT-5.6 reasoning levels changed unexpectedly.")
expect(article_lab_supports_pro_mode("gpt-5.6-sol"), "GPT-5.6 Sol should support Pro mode.")
expect(!article_lab_supports_pro_mode("gpt-5.4"), "GPT-5.4 must not expose GPT-5.6 Pro mode.")
expect(length(article_lab_reasoning_capabilities("gpt-4.1")) == 0L, "GPT-4.1 should disable configurable reasoning.")
expect(identical(article_lab_validate_generation_settings("gpt-4.1", NA_character_, "standard")$reasoning_mode, "standard"), "Non-Pro models should retain standard execution.")
expect(inherits(try(article_lab_validate_generation_settings("gpt-4.1", "low", "standard"), silent = TRUE), "try-error"), "Unsupported reasoning should fail before an API call.")
expect(inherits(try(article_lab_validate_generation_settings("gpt-5.4", "medium", "pro"), silent = TRUE), "try-error"), "Unsupported Pro mode should fail before an API call.")

expect(identical(article_lab_validate_image_generation_model("gpt-5.6-luna"), "gpt-5.6-luna"), "An allow-listed image-generation model should pass validation.")
text_only_error <- try(article_lab_validate_image_generation_model("gpt-5-mini"), silent = TRUE)
expect(inherits(text_only_error, "try-error"), "A text-only model must be rejected for image generation.")
expect(grepl("gpt-5-mini", as.character(text_only_error), fixed = TRUE), "Image capability errors must identify the invalid model.")
expect(identical(article_lab_thumbnail_model_choices, article_lab_image_generation_models), "The Images-tab dropdown must exactly match the canonical image-capable allow-list.")
expect(!"gpt-5-mini" %in% article_lab_thumbnail_model_choices, "Text-only models must not appear in the Images-tab dropdown.")

api_called <- FALSE
original_has_api_key <- article_lab_has_api_key
article_lab_has_api_key <- function() {
  api_called <<- TRUE
  TRUE
}
invalid_request_error <- try(
  article_lab_thumbnail_api_request(data.frame(title = "Test"), model = "gpt-5-mini"),
  silent = TRUE
)
article_lab_has_api_key <- original_has_api_key
expect(inherits(invalid_request_error, "try-error"), "The server boundary must reject a non-image-capable model.")
expect(!api_called, "Image capability validation must run before API setup or execution.")
expect(grepl("gpt-5-mini", as.character(invalid_request_error), fixed = TRUE), "Server rejection must report the selected invalid model without falling back.")

payload <- article_lab_thumbnail_request_payload(
  data.frame(subtitle_id = "sub_1", candidate_id = "candidate_1", batch_id = "batch_1", title = "Title", subtitle = "Subtitle"),
  variants_per_package = 2L, model = "gpt-5.4-mini", reasoning_effort = "none", reasoning_mode = "standard",
  prompt = "{{input_context}}",
  size = "1536x1024", quality = "low", output_format = "webp", output_compression = 81L, background = "opaque"
)
expect(identical(payload$size, "1536x1024") && identical(payload$quality, "low"), "Image size or quality did not reach the canonical helper payload.")
expect(identical(payload$output_format, "webp") && identical(payload$output_compression, 81L) && identical(payload$background, "opaque"), "Image output settings did not reach the canonical helper payload.")
expect(identical(payload$streaming, FALSE) && is.null(payload$partial_images), "Fixed non-streaming settings changed unexpectedly.")
disabled_mode_payload <- article_lab_thumbnail_request_payload(payload$packages |> as.data.frame(), 1L, "gpt-5.4-mini", "none", "__unsupported__")
expect(identical(disabled_mode_payload$reasoning_mode, "standard"), "A disabled execution-mode sentinel must be omitted rather than sent to validation or the API.")

test_db <- tempfile(pattern = "article_lab_generation_preferences_", fileext = ".sqlite")
on.exit(unlink(test_db, force = TRUE), add = TRUE)
con <- dbConnect(SQLite(), test_db)
dbExecute(con, "CREATE TABLE article_projects (article_project_id TEXT PRIMARY KEY)")
ensure_article_lab_schema(con)

initial <- article_lab_load_generation_preference(con, "titles", "gpt-5.6-terra", "low", "standard")
expect(identical(initial$model, "gpt-5.6-terra") && identical(initial$reasoning_effort, "low") && identical(initial$reasoning_mode, "standard"), "Code defaults were not used for an unsaved workflow.")
article_lab_save_generation_preference(con, "titles", "gpt-5.6-terra", "high", "pro", "high", "pro")
dbDisconnect(con)

con <- dbConnect(SQLite(), test_db)
saved <- article_lab_load_generation_preference(con, "titles", "gpt-5-mini", "low", "standard")
expect(identical(saved$model, "gpt-5.6-terra") && identical(saved$reasoning_effort, "high") && identical(saved$reasoning_mode, "pro"), "Saved workflow preferences did not survive a database restart.")
dbDisconnect(con)

message("Article Lab generation settings tests passed.")
