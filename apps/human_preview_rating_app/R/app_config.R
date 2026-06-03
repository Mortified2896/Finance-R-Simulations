requested_rating_mode <- Sys.getenv("HUMAN_RATING_MODE", unset = "feed_preview_1_5")
is_dimension_v1_mode <- requested_rating_mode %in% c("dimensions_v1", "human_preview_dimensions_v1")
is_dimension_v2_mode <- requested_rating_mode %in% c("dimensions_v2", "human_preview_dimensions_v2")
is_dimension_mode <- is_dimension_v1_mode || is_dimension_v2_mode
interface_version <- if (is_dimension_mode) {
  if (is_dimension_v2_mode) "human_preview_rating_app_v4_dimensions_v2_validated_manifest" else "human_preview_rating_app_v3_dimensions_v1"
} else {
  "human_preview_rating_app_v2_unrated_thumbnails"
}
rating_mode <- if (is_dimension_v2_mode) {
  "human_preview_dimensions_v2"
} else if (is_dimension_v1_mode) {
  "human_preview_dimensions_v1"
} else {
  "feed_preview_1_5"
}
manifest_version <- if (is_dimension_v2_mode) "human_rated_thumbnail_valid_cohort_v2" else NA_character_
dimension_rating_table <- if (is_dimension_v2_mode) "human_preview_dimension_ratings_v2" else "human_preview_dimension_ratings"
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
text_only_dimension_fields <- if (is_dimension_v2_mode) title_isolation_dimension_fields else character()
title_only_placeholder_subtitle <- "[Subtitle hidden for title-only rating]"
title_only_placeholder_thumbnail_label <- "Placeholder image\nThumbnail hidden for title-only rating"
target_n_env <- Sys.getenv("HUMAN_RATING_TARGET_N", unset = "")
default_target_n <- suppressWarnings(as.integer(target_n_env))
if (!nzchar(target_n_env) || is.na(default_target_n) || default_target_n < 1L) default_target_n <- Inf

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
  if (is_dimension_v2_mode) "medium_images" else "title_api_score_samples",
  if (is_dimension_v2_mode) "human_rated_thumbnail_valid_cohort_v2.csv" else "human_rated_thumbnail_all_v1.csv"
)
