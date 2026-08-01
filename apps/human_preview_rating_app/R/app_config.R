requested_rating_mode <- trimws(Sys.getenv("HUMAN_RATING_MODE", unset = ""))
supported_rating_modes <- c("", "dimensions_v2")
if (!(requested_rating_mode %in% supported_rating_modes)) {
  stop(
    "Invalid HUMAN_RATING_MODE: '", requested_rating_mode, "'. ",
    "Supported values are an empty/unset value for normal stable rating or 'dimensions_v2' for dimension-v2 rating.",
    call. = FALSE
  )
}
article_lab_ui_version <- tolower(trimws(Sys.getenv("ARTICLE_LAB_UI_VERSION", unset = "")))
supported_article_lab_ui_versions <- c("", "v1", "v2")
if (!(article_lab_ui_version %in% supported_article_lab_ui_versions)) {
  stop(
    "Invalid ARTICLE_LAB_UI_VERSION: '", article_lab_ui_version, "'. ",
    "Supported values are an empty/unset value or 'v1' for the stable UI, and 'v2' for Design v2.",
    call. = FALSE
  )
}
article_lab_design_v2 <- identical(article_lab_ui_version, "v2")
is_dimension_v2_mode <- identical(requested_rating_mode, "dimensions_v2")
is_dimension_mode <- is_dimension_v2_mode
interface_version <- if (is_dimension_mode) {
  "human_preview_rating_app_v4_dimensions_v2_validated_manifest"
} else {
  "human_preview_rating_app_v2_unrated_thumbnails"
}
rating_mode <- if (is_dimension_v2_mode) {
  "human_preview_dimensions_v2"
} else {
  "feed_preview_1_5"
}
manifest_version <- if (is_dimension_v2_mode) "human_rated_thumbnail_valid_cohort_v2" else NA_character_
rating_prompt <- if (is_dimension_mode) {
  "Score only the active dimension for this pass."
} else {
  "Based only on the title, subtitle, and thumbnail, how likely is this article to perform well on Medium?"
}
dimension_fields <- c(
  "ai_low_effort_flag",
  "visual_hook",
  "title_hook_strength",
  "emotional_pull_preview",
  "personal_click_appeal"
)
dimension_numeric_fields <- setdiff(dimension_fields, "ai_low_effort_flag")
dimension_labels <- c(
  ai_low_effort_flag = "AI / low-effort thumbnail",
  visual_hook = "Visual hook",
  title_hook_strength = "Title hook strength",
  emotional_pull_preview = "Emotional pull",
  personal_click_appeal = "Personal click appeal"
)
dimension_questions <- c(
  ai_low_effort_flag = "Does this thumbnail look AI-generated, generic, sloppy, or low-effort?",
  visual_hook = "Does the thumbnail catch attention visually?",
  title_hook_strength = "How strong is the title as a hook?",
  emotional_pull_preview = "Does the full preview create curiosity, concern, aspiration, tension, or emotion?",
  personal_click_appeal = "Would I personally want to click/read this based on the preview?"
)
dimension_focus <- c(
  ai_low_effort_flag = "thumbnail only",
  visual_hook = "thumbnail only",
  title_hook_strength = "title only; subtitle and thumbnail are hidden behind placeholders",
  emotional_pull_preview = "full preview: title, subtitle, and thumbnail",
  personal_click_appeal = "full preview: title, subtitle, and thumbnail"
)
dimension_scale <- list(
  ai_low_effort_flag = c(yes = "yes", unsure = "unsure", no = "no"),
  visual_hook = c(`1` = "visually boring", `2` = "weak", `3` = "okay", `4` = "strong", `5` = "very strong"),
  title_hook_strength = c(`1` = "weak/generic", `2` = "below average", `3` = "okay", `4` = "strong", `5` = "excellent"),
  emotional_pull_preview = c(`1` = "emotionally flat", `2` = "weak", `3` = "moderate", `4` = "strong", `5` = "very strong"),
  personal_click_appeal = c(`1` = "definitely no", `2` = "probably no", `3` = "maybe / unclear", `4` = "probably yes", `5` = "definitely yes")
)
thumbnail_only_dimension_fields <- c("ai_low_effort_flag", "visual_hook")
active_dimension_fields <- dimension_fields
title_isolation_dimension_fields <- c("title_hook_strength")
text_only_dimension_fields <- title_isolation_dimension_fields
title_only_placeholder_subtitle <- "[Subtitle hidden for title-only rating]"
title_only_placeholder_thumbnail_label <- "Placeholder image\nThumbnail hidden for title-only rating"
target_n_env <- Sys.getenv("HUMAN_RATING_TARGET_N", unset = "")
default_target_n <- suppressWarnings(as.integer(target_n_env))
if (!nzchar(target_n_env)) {
  default_target_n <- Inf
} else if (is.na(default_target_n) || default_target_n < 1L) {
  stop(
    "Invalid HUMAN_RATING_TARGET_N: '", target_n_env, "'. Expected a positive integer or an empty/unset value.",
    call. = FALSE
  )
}

article_lab_positive_integer_env <- function(name, default, minimum) {
  raw_value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(raw_value)) return(as.integer(default))
  parsed <- suppressWarnings(as.integer(raw_value))
  if (is.na(parsed) || parsed < minimum) {
    stop(
      "Invalid ", name, ": '", raw_value, "'. Expected an integer greater than or equal to ", minimum, ".",
      call. = FALSE
    )
  }
  parsed
}

if (!exists("find_project_root", mode = "function")) {
  find_project_root <- function() {
    env_root <- Sys.getenv("MEDIUM_PROJECT_ROOT", unset = "")
    candidates <- unique(c(
      env_root,
      getwd(),
      normalizePath(file.path(getwd(), ".."), mustWork = FALSE),
      normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE)
    ))
    candidates <- candidates[nzchar(candidates)]
    for (candidate in candidates) {
      if (file.exists(file.path(candidate, "data", "db", "medium_articles.sqlite"))) {
        return(normalizePath(candidate, mustWork = TRUE))
      }
    }
    stop("Could not find project root with data/db/medium_articles.sqlite.", call. = FALSE)
  }
}

project_root <- find_project_root()
db_path <- Sys.getenv(
  "MEDIUM_RATING_DB",
  unset = file.path(project_root, "data", "db", "medium_articles.sqlite")
)
thumbnail_queue_path <- file.path(
  project_root,
  "data",
  "analysis",
  "medium_images",
  "medium_image_download_queue.csv"
)
dimension_cohort_path <- file.path(
  project_root,
  "data",
  "analysis",
  "medium_images",
  "human_rated_thumbnail_valid_cohort_v2.csv"
)
