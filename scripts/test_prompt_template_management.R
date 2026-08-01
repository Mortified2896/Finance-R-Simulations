suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

app_dir <- file.path("apps", "human_preview_rating_app")
source(file.path(app_dir, "R", "text_helpers.R"))
source(file.path(app_dir, "R", "input_helpers.R"))
source(file.path(app_dir, "R", "app_config.R"))
source(file.path(app_dir, "R", "db_helpers.R"))
source(file.path(app_dir, "R", "article_lab_config.R"))
source(file.path(app_dir, "R", "prompt_template_helpers.R"))
source(file.path(app_dir, "R", "schema_article_lab.R"))

expect <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)
test_db <- tempfile(pattern = "prompt_templates_", fileext = ".sqlite")
on.exit(unlink(test_db, force = TRUE), add = TRUE)
con <- dbConnect(SQLite(), test_db)
on.exit(dbDisconnect(con), add = TRUE)
dbExecute(con, "CREATE TABLE article_projects (article_project_id TEXT PRIMARY KEY)")
ensure_article_lab_schema(con)

title_default <- article_lab_prompt_template_rows(con, "titles")
thumbnail_default <- article_lab_prompt_template_rows(con, "thumbnails")
expect(nrow(title_default) >= 1L && nrow(thumbnail_default) >= 1L, "Shipped templates were not initialized.")
expect(length(intersect(title_default$template_id, thumbnail_default$template_id)) == 0L, "Workflow template IDs must be isolated.")

created <- article_lab_create_prompt_template(con, "thumbnails", "Editorial", "Draw {{input_context}}")
expect(created %in% article_lab_prompt_template_rows(con, "thumbnails")$template_id, "Created template was not persisted.")
expect(!created %in% article_lab_prompt_template_rows(con, "titles")$template_id, "Created template leaked across workflows.")
expect(identical(article_lab_prompt_template_active(con, "thumbnails"), created), "New template was not selected.")

article_lab_update_prompt_template(con, created, "Editorial renamed", "Updated {{input_context}}")
updated <- article_lab_prompt_template_rows(con, "thumbnails")
expect(updated$template_name[updated$template_id == created] == "Editorial renamed", "Rename was not persisted.")
expect(updated$prompt_text[updated$template_id == created] == "Updated {{input_context}}", "Edit was not persisted.")
duplicate <- try(article_lab_create_prompt_template(con, "thumbnails", "EDITORIAL RENAMED", "x"), silent = TRUE)
expect(inherits(duplicate, "try-error"), "Duplicate names must be rejected case-insensitively.")
unknown <- try(article_lab_validate_prompt_variables("{{unknown}}", "input_context"), silent = TRUE)
expect(inherits(unknown, "try-error"), "Unknown variables must be rejected.")

for (id in article_lab_prompt_template_rows(con, "medium_tags")$template_id) article_lab_delete_prompt_template(con, id)
expect(nrow(article_lab_prompt_template_rows(con, "medium_tags")) == 0L, "Deleting the final template must leave an empty library.")
ensure_article_lab_schema(con)
reseeded <- article_lab_prompt_template_rows(con, "medium_tags")
expect(nrow(reseeded) == 1L && identical(reseeded$template_name[[1]], "Default"), "Each workflow must retain one current shipped default.")

for (workflow in names(article_lab_prompt_variable_registry)) {
  default_rows <- article_lab_prompt_template_rows(con, workflow)
  default_rows <- default_rows[tolower(default_rows$template_name) == "default", , drop = FALSE]
  expect(nrow(default_rows) == 1L, paste(workflow, "must have exactly one current default"))
  attempt <- article_lab_record_generation_attempt(con, workflow, default_rows$template_id[[1]], "Default",
    default_rows$prompt_text[[1]], "byte-exact prompt", list(model = "fixture", input = "byte-exact prompt", api_key = "secret"),
    "fixture", "low", "standard", list(), status = "succeeded")
  persisted <- dbGetQuery(con, "SELECT resolved_prompt, canonical_request_json, status FROM article_lab_generation_attempts WHERE attempt_id = ?", params = list(attempt))
  expect(identical(charToRaw(persisted$resolved_prompt[[1]]), charToRaw("byte-exact prompt")), paste(workflow, "resolved prompt did not persist byte-exactly"))
  expect(!grepl("secret", persisted$canonical_request_json[[1]], fixed = TRUE), paste(workflow, "canonical request persisted a secret"))
}

message("Prompt template management tests passed.")
