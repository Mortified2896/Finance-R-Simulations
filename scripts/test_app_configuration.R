suppressPackageStartupMessages(library(shiny))

app_dir <- file.path("apps", "human_preview_rating_app")
config_names <- c(
  "ARTICLE_LAB_UI_VERSION",
  "HUMAN_RATING_MODE",
  "HUMAN_RATING_TARGET_N",
  "OPENAI_EVIDENCE_REASONING_EFFORT",
  "OPENAI_THUMBNAIL_GENERATION_MODEL",
  "OPENAI_THUMBNAIL_RESPONSES_MODEL",
  "OPENAI_SUBTITLE_GENERATION_MODEL",
  "OPENAI_TITLE_GENERATION_MODEL",
  "OPENAI_FULL_TEXT_GENERATION_TIMEOUT_SECONDS",
  "OPENAI_OUTLINE_GENERATION_TIMEOUT_SECONDS"
)
original_values <- Sys.getenv(config_names, unset = NA_character_)
on.exit({
  Sys.unsetenv(config_names)
  present <- !is.na(original_values)
  if (any(present)) do.call(Sys.setenv, as.list(original_values[present]))
}, add = TRUE)

expect <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)

load_config <- function(values = list(), include_article_lab = FALSE) {
  Sys.unsetenv(config_names)
  if (length(values) > 0) do.call(Sys.setenv, values)
  target <- new.env(parent = globalenv())
  source(file.path(app_dir, "R", "app_config.R"), local = target)
  if (isTRUE(include_article_lab)) source(file.path(app_dir, "R", "article_lab_config.R"), local = target)
  target
}

expect_error <- function(values, pattern, include_article_lab = FALSE) {
  error <- tryCatch({ load_config(values, include_article_lab = include_article_lab); NULL }, error = identity)
  expect(inherits(error, "error"), paste("Expected configuration error matching", pattern))
  expect(grepl(pattern, conditionMessage(error), fixed = TRUE), paste("Unexpected configuration error:", conditionMessage(error)))
}

defaults <- load_config()
expect(identical(defaults$requested_rating_mode, ""), "Unset HUMAN_RATING_MODE should select normal rating.")
expect(!isTRUE(defaults$article_lab_design_v2), "Unset ARTICLE_LAB_UI_VERSION should select the stable UI.")
expect(is.infinite(defaults$default_target_n), "Unset HUMAN_RATING_TARGET_N should leave the queue target unlimited.")

dimensions <- load_config(list(HUMAN_RATING_MODE = "dimensions_v2"))
expect(isTRUE(dimensions$is_dimension_v2_mode), "dimensions_v2 should select the validated dimension workflow.")
design_v2 <- load_config(list(ARTICLE_LAB_UI_VERSION = "v2", HUMAN_RATING_TARGET_N = "5"))
expect(isTRUE(design_v2$article_lab_design_v2) && identical(design_v2$default_target_n, 5L), "Design v2 and a positive queue target should be accepted.")

expect_error(list(HUMAN_RATING_MODE = "dimensions_v1"), "Invalid HUMAN_RATING_MODE")
expect_error(list(ARTICLE_LAB_UI_VERSION = "preview"), "Invalid ARTICLE_LAB_UI_VERSION")
expect_error(list(HUMAN_RATING_TARGET_N = "0"), "Invalid HUMAN_RATING_TARGET_N")
expect_error(list(HUMAN_RATING_TARGET_N = "many"), "Invalid HUMAN_RATING_TARGET_N")
expect_error(list(OPENAI_OUTLINE_GENERATION_TIMEOUT_SECONDS = "10"), "Invalid OPENAI_OUTLINE_GENERATION_TIMEOUT_SECONDS", include_article_lab = TRUE)
expect_error(list(OPENAI_FULL_TEXT_GENERATION_TIMEOUT_SECONDS = "later"), "Invalid OPENAI_FULL_TEXT_GENERATION_TIMEOUT_SECONDS", include_article_lab = TRUE)
expect_error(list(OPENAI_EVIDENCE_REASONING_EFFORT = "extreme"), "Invalid OPENAI_EVIDENCE_REASONING_EFFORT", include_article_lab = TRUE)
expect_error(
  list(OPENAI_THUMBNAIL_GENERATION_MODEL = "gpt-5-mini"),
  "Configured Images-tab default from OPENAI_THUMBNAIL_GENERATION_MODEL 'gpt-5-mini'",
  include_article_lab = TRUE
)
expect_error(
  list(OPENAI_THUMBNAIL_RESPONSES_MODEL = "gpt-4.1"),
  "Configured Images-tab default from OPENAI_THUMBNAIL_RESPONSES_MODEL 'gpt-4.1'",
  include_article_lab = TRUE
)

generation_precedence <- load_config(
  list(
    OPENAI_THUMBNAIL_GENERATION_MODEL = "gpt-5.6-luna",
    OPENAI_THUMBNAIL_RESPONSES_MODEL = "gpt-5.4"
  ),
  include_article_lab = TRUE
)
expect(identical(generation_precedence$article_lab_default_thumbnail_model, "gpt-5.6-luna"), "OPENAI_THUMBNAIL_GENERATION_MODEL must have highest Images-model precedence.")

responses_precedence <- load_config(
  list(OPENAI_THUMBNAIL_RESPONSES_MODEL = "gpt-5.4-mini"),
  include_article_lab = TRUE
)
expect(identical(responses_precedence$article_lab_default_thumbnail_model, "gpt-5.4-mini"), "OPENAI_THUMBNAIL_RESPONSES_MODEL must supply the Images default when the generation variable is absent.")

built_in_images_config <- load_config(include_article_lab = TRUE)
expect(identical(built_in_images_config$article_lab_default_thumbnail_model, built_in_images_config$article_lab_builtin_thumbnail_model), "The built-in Images default must be used when neither thumbnail-specific variable is set.")
expect(identical(built_in_images_config$article_lab_builtin_thumbnail_model, "gpt-5.4-mini"), "The built-in Images repair-test default changed unexpectedly.")

title_isolation <- load_config(
  list(OPENAI_TITLE_GENERATION_MODEL = "gpt-4.1"),
  include_article_lab = TRUE
)
expect(identical(title_isolation$article_lab_default_thumbnail_model, title_isolation$article_lab_builtin_thumbnail_model), "Title model configuration must not influence the Images default.")

subtitle_isolation <- load_config(
  list(OPENAI_SUBTITLE_GENERATION_MODEL = "gpt-5-mini"),
  include_article_lab = TRUE
)
expect(identical(subtitle_isolation$article_lab_default_thumbnail_model, subtitle_isolation$article_lab_builtin_thumbnail_model), "Subtitle model configuration must not influence the Images default.")

text_config_isolation <- load_config(
  list(
    OPENAI_TITLE_GENERATION_MODEL = "text-only-title-model",
    OPENAI_SUBTITLE_GENERATION_MODEL = "text-only-subtitle-model"
  ),
  include_article_lab = TRUE
)
expect(identical(text_config_isolation$article_lab_default_thumbnail_model, text_config_isolation$article_lab_builtin_thumbnail_model), "Incompatible title and subtitle models must not fail or alter Images configuration.")
expect(identical(text_config_isolation$article_lab_thumbnail_model_choices, text_config_isolation$article_lab_image_generation_models), "Title and subtitle configuration must not mutate the Images dropdown.")
expect(identical(generation_precedence$article_lab_thumbnail_model_choices, generation_precedence$article_lab_image_generation_models), "A configured thumbnail default must not mutate the Images dropdown contents.")

message("Application configuration tests passed.")
