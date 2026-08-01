suppressPackageStartupMessages({ library(jsonlite); library(DBI); library(RSQLite) })
app_dir <- file.path("apps", "human_preview_rating_app")
source(file.path(app_dir, "R", "text_helpers.R"))
source(file.path(app_dir, "R", "input_helpers.R"))
source(file.path(app_dir, "R", "app_config.R"))
source(file.path(app_dir, "R", "db_helpers.R"))
source(file.path(app_dir, "R", "title_subtitle_helpers.R"))
source(file.path(app_dir, "R", "scoring_helpers.R"))
project_root <- normalizePath(".", mustWork = TRUE)
source(file.path(app_dir, "R", "article_lab_config.R"))
source(file.path(app_dir, "R", "prompt_template_helpers.R"))
source(file.path(app_dir, "R", "api_helpers.R"))

expect <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)
sample_value <- function(name) paste0("VALUE_", toupper(name))

# The registry is the bidirectional contract: every documented variable is
# accepted/resolvable, and cross-workflow or unknown variables fail with help.
for (workflow in names(article_lab_prompt_variable_registry)) {
  variables <- article_lab_prompt_registry_variables(workflow)
  help <- paste(article_lab_prompt_registry_help(workflow), collapse = "\n")
  expect(setequal(variables, names(article_lab_prompt_variable_registry[[workflow]])), paste(workflow, "registry/help drift"))
  for (name in variables) {
    expect(grepl(sprintf("{{%s}}", name), help, fixed = TRUE), paste(workflow, name, "missing from help"))
    rendered <- article_lab_render_prompt_template(sprintf("before {{%s}} after", name), setNames(list(sample_value(name)), name), variables)
    expect(identical(rendered, paste("before", sample_value(name), "after")), paste(workflow, name, "did not resolve"))
  }
  foreign <- setdiff(unique(unlist(lapply(article_lab_prompt_variable_registry, names))), variables)
  if (length(foreign)) {
    error <- tryCatch(article_lab_render_prompt_template(sprintf("{{%s}}", foreign[[1]]), list(), variables), error = identity)
    expect(inherits(error, "error"), paste(workflow, "accepted a foreign variable"))
    expect(all(vapply(variables, function(name) grepl(sprintf("{{%s}}", name), conditionMessage(error), fixed = TRUE), logical(1))), paste(workflow, "unknown-variable error omitted valid variables"))
  }
}

# Unreferenced optional content cannot enter model text.
title_only <- article_lab_effective_title_prompt_text("Template literal only", 7L, seed_topic = "HIDDEN_SEED", context_notes = "HIDDEN_CONTEXT")
expect(identical(title_only, "Template literal only"), "Title workflow appended text absent from its template.")
summary_only <- article_lab_render_prompt_template("Summarize the attached PDF.", list(input_context = "HIDDEN_METADATA"), article_lab_prompt_registry_variables("research_summary"))
expect(identical(summary_only, "Summarize the attached PDF."), "Optional research metadata leaked without a variable reference.")

# Broad context variables have a documented, limited source-field set.
for (workflow in c("subtitles", "thumbnails", "outlines", "full_text", "research_summary", "medium_tags")) {
  spec <- article_lab_prompt_variable_registry[[workflow]]$input_context
  expect(length(spec$sources) > 0L && nzchar(spec$format), paste(workflow, "input_context is undocumented"))
}

# Thumbnail creative text excludes internal IDs and variant bookkeeping while
# metadata still maps each independent request to its records.
packages <- data.frame(subtitle_id = "sub_SECRET", candidate_id = "cand_SECRET", batch_id = "batch_SECRET", title = "Public title", subtitle = "Public subtitle", stringsAsFactors = FALSE)
payload <- article_lab_thumbnail_request_payload(packages, 2L, model = article_lab_default_thumbnail_model, prompt = "Creative brief:\n{{input_context}}")
expect(length(payload$requests) == 2L, "Thumbnail request topology changed.")
for (request in payload$requests) {
  expect(!grepl("SECRET|variant_index|variants_per_package|/Users/", request$resolved_prompt), "Internal metadata leaked into thumbnail creative prompt.")
  expect(identical(request$subtitle_id, "sub_SECRET") && identical(request$candidate_id, "cand_SECRET") && identical(request$batch_id, "batch_SECRET"), "Thumbnail metadata mapping was lost.")
}

preview <- fromJSON(article_lab_thumbnail_helper_call(payload, preview_only = TRUE), simplifyVector = FALSE)
for (i in seq_along(payload$requests)) {
  expect(identical(charToRaw(payload$requests[[i]]$resolved_prompt), charToRaw(preview$requests[[i]]$sanitized_request$input)), "Preview/bridge/API prompt bytes differ.")
  expect(!grepl("OPENAI_API_KEY|sk-[A-Za-z0-9]", toJSON(preview$requests[[i]]$sanitized_request), perl = TRUE), "A secret appeared in request preview.")
}

# Retry code must reuse the exact same resolved prompt and settings.
title_helper <- paste(readLines(file.path("scripts", "writing_api", "generate_titles.mjs"), warn = FALSE), collapse = "\n")
expect(grepl("input: builtPrompt", title_helper, fixed = TRUE), "Title retry does not reuse the resolved prompt.")
expect(!grepl("buildRetryPrompt", title_helper, fixed = TRUE), "Title retry still augments prompt text.")

message("Prompt input transparency tests passed.")
