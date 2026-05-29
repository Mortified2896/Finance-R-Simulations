required_packages <- c("shiny", "DBI", "RSQLite", "jsonlite", "DT")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ", paste(missing_packages, collapse = ", "), "\n\n",
    "Install them in R with:\n",
    'install.packages(c("', paste(missing_packages, collapse = '", "'), '"))',
    call. = FALSE
  )
}

library(shiny)
library(DBI)
library(RSQLite)
library(jsonlite)
library(DT)

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

clean_text <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

clean_multiline_text <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\r\n?", "\n", y)
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("[ \t]+$", "", y, perl = TRUE)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

now_utc <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

file_sha256 <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_character_)
  hash <- suppressWarnings(tools::sha256sum(path))
  if (length(hash) == 0 || is.na(hash[[1]])) return(NA_character_)
  unname(hash[[1]])
}

first_value <- function(row, column, default = NA_character_) {
  if (is.null(row) || nrow(row) == 0 || !(column %in% names(row))) return(default)
  value <- row[[column]][[1]]
  if (length(value) == 0) default else value
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

article_lab_format_duration <- function(seconds) {
  seconds <- suppressWarnings(as.numeric(seconds))
  if (is.na(seconds) || seconds < 0) seconds <- 0
  seconds <- as.integer(round(seconds))
  minutes <- seconds %/% 60L
  remaining_seconds <- seconds %% 60L
  if (minutes <= 0L) return(sprintf("%s sec", remaining_seconds))
  if (remaining_seconds == 0L) return(sprintf("%s min", minutes))
  sprintf("%s min %s sec", minutes, remaining_seconds)
}

article_lab_thumbnail_estimate <- function(total_expected) {
  total_expected <- suppressWarnings(as.integer(total_expected))
  if (is.na(total_expected) || total_expected < 1L) total_expected <- 1L
  list(
    total_expected = total_expected,
    lower_seconds = total_expected * 45,
    upper_seconds = total_expected * 90,
    label = sprintf(
      "%s-%s",
      article_lab_format_duration(total_expected * 45),
      article_lab_format_duration(total_expected * 90)
    )
  )
}

article_lab_estimate_comparison <- function(actual_seconds, lower_seconds, upper_seconds) {
  actual_seconds <- suppressWarnings(as.numeric(actual_seconds))
  lower_seconds <- suppressWarnings(as.numeric(lower_seconds))
  upper_seconds <- suppressWarnings(as.numeric(upper_seconds))
  if (is.na(actual_seconds) || is.na(lower_seconds) || is.na(upper_seconds)) return("within estimate")
  if (actual_seconds < lower_seconds) "faster than expected" else if (actual_seconds > upper_seconds) "slower than expected" else "within estimate"
}

displayed_subtitle_for_field <- function(item, field) {
  if (!is.null(field) && !is.na(field) && field %in% title_isolation_dimension_fields) {
    return(title_only_placeholder_subtitle)
  }
  first_value(item, "subtitle")
}

displayed_thumbnail_path_for_field <- function(item, field) {
  if (!is.null(field) && !is.na(field) && field %in% title_isolation_dimension_fields) {
    return(NA_character_)
  }
  first_value(item, "local_thumbnail_path")
}

v2_render_info <- function(item) {
  if (!is_dimension_v2_mode || is.null(item) || nrow(item) == 0) {
    return(list(
      path = NA_character_,
      path_abs = NA_character_,
      manifest_hash = NA_character_,
      rendered_hash = NA_character_,
      valid = FALSE,
      reason = "not_dimensions_v2"
    ))
  }

  path <- first_value(item, "local_thumbnail_path", first_value(item, "local_image_path"))
  path_abs <- first_value(item, "local_thumbnail_path_abs")
  if (is.na(path_abs)) path_abs <- as_abs_path(path)[[1]]
  manifest_hash <- first_value(item, "image_sha256", first_value(item, "manifest_image_sha256"))
  rendered_hash <- first_value(item, "current_image_sha256")
  exists <- !is.na(path_abs) && nzchar(path_abs) && file.exists(path_abs)
  manifest_flag <- suppressWarnings(as.logical(first_value(item, "hash_matches_manifest")))
  computed_hash_matches <- exists &&
    !is.na(rendered_hash) &&
    !is.na(manifest_hash) &&
    identical(rendered_hash, manifest_hash)
  hash_matches <- exists &&
    !is.na(manifest_hash) &&
    !is.na(rendered_hash) &&
    isTRUE(manifest_flag) &&
    computed_hash_matches
  reason <- if (!exists) {
    "missing_file"
  } else if (is.na(manifest_hash)) {
    "missing_manifest_hash"
  } else if (is.na(rendered_hash)) {
    "missing_rendered_hash"
  } else if (is.na(manifest_flag)) {
    "missing_manifest_hash_flag"
  } else if (!isTRUE(manifest_flag)) {
    "manifest_hash_flag_false"
  } else if (!computed_hash_matches) {
    "hash_mismatch"
  } else {
    "ok"
  }

  list(
    path = path,
    path_abs = path_abs,
    manifest_hash = manifest_hash,
    rendered_hash = rendered_hash,
    valid = hash_matches,
    reason = reason
  )
}

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

as_abs_path <- function(path) {
  path <- clean_text(path)
  vapply(path, function(one_path) {
    if (is.na(one_path)) return(NA_character_)
    if (grepl("^/", one_path)) one_path else file.path(project_root, one_path)
  }, character(1), USE.NAMES = FALSE)
}

split_keys <- function(x) {
  value <- clean_text(x)
  if (length(value) == 0 || is.na(value)) return(character())
  parts <- unlist(strsplit(value, "[,;|]", perl = TRUE), use.names = FALSE)
  parts <- clean_text(parts)
  unique(parts[!is.na(parts)])
}

normalize_image_url <- function(url) {
  value <- clean_text(url)
  vapply(value, function(one_url) {
    if (is.na(one_url)) return(NA_character_)
    without_fragment <- sub("#.*$", "", one_url)
    split_url <- strsplit(without_fragment, "\\?", fixed = FALSE)[[1]]
    base_url <- split_url[[1]]
    if (length(split_url) == 1 || split_url[[2]] == "") return(base_url)
    query_params <- unlist(strsplit(split_url[[2]], "&", fixed = TRUE), use.names = FALSE)
    parameter_names <- sub("=.*$", "", query_params)
    tracking_param <- grepl("^utm_", parameter_names, ignore.case = TRUE) |
      tolower(parameter_names) %in% c("fbclid", "gclid")
    kept_params <- query_params[!tracking_param & query_params != ""]
    if (length(kept_params) == 0) base_url else paste0(base_url, "?", paste(kept_params, collapse = "&"))
  }, character(1), USE.NAMES = FALSE)
}

connect_db <- function() {
  con <- dbConnect(SQLite(), db_path)
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

db_add_column_if_missing <- function(con, table, column, definition) {
  columns <- dbGetQuery(con, sprintf("PRAGMA table_info(%s)", dbQuoteIdentifier(con, table)))
  if (!(column %in% columns$name)) {
    dbExecute(con, sprintf(
      "ALTER TABLE %s ADD COLUMN %s %s",
      dbQuoteIdentifier(con, table),
      dbQuoteIdentifier(con, column),
      definition
    ))
  }
}

ensure_rating_schema <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_rating_sessions (
      rating_session_id TEXT PRIMARY KEY,
      created_at TEXT,
      interface_version TEXT,
      rating_mode TEXT,
      queue_seed INTEGER,
      target_n INTEGER,
      notes TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_rating_queue (
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      status TEXT DEFAULT 'pending',
      shown_at TEXT,
      completed_at TEXT,
      PRIMARY KEY (rating_session_id, queue_position)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_ratings (
      id INTEGER PRIMARY KEY,
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      interface_version TEXT,
      rating_prompt TEXT,
      shown_title TEXT,
      shown_subtitle TEXT,
      shown_thumbnail_path TEXT,
      human_feed_success_potential INTEGER,
      human_feed_success_note TEXT,
      skipped INTEGER DEFAULT 0,
      shown_at TEXT,
      rated_at TEXT,
      seconds_spent REAL
    )
  ")

  queue_columns <- c(
    source_type = "TEXT",
    article_lab_candidate_id = "TEXT"
  )
  for (column_name in names(queue_columns)) {
    db_add_column_if_missing(con, "human_preview_rating_queue", column_name, queue_columns[[column_name]])
  }

  rating_columns <- c(
    source_type = "TEXT",
    article_lab_candidate_id = "TEXT"
  )
  for (column_name in names(rating_columns)) {
    db_add_column_if_missing(con, "human_preview_ratings", column_name, rating_columns[[column_name]])
  }

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_dimension_rating_queue (
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      canonical_article_key TEXT,
      status TEXT DEFAULT 'pending',
      shown_at TEXT,
      completed_at TEXT,
      PRIMARY KEY (rating_session_id, queue_position)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_dimension_ratings (
      id INTEGER PRIMARY KEY,
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      canonical_article_key TEXT,
      interface_version TEXT,
      rating_mode TEXT,
      shown_title TEXT,
      shown_subtitle TEXT,
      shown_thumbnail_path TEXT,
      personal_click_appeal INTEGER,
      title_hook_strength INTEGER,
      visual_hook INTEGER,
      emotional_pull_preview INTEGER,
      ai_low_effort_flag TEXT,
      human_dimension_note TEXT,
      skipped INTEGER DEFAULT 0,
      shown_at TEXT,
      rated_at TEXT,
      seconds_spent REAL
    )
  ")

  dimension_rating_columns <- c(
    rating_session_id = "TEXT",
    queue_position = "INTEGER",
    interface_version = "TEXT",
    skipped = "INTEGER DEFAULT 0",
    shown_at = "TEXT",
    rated_at = "TEXT",
    seconds_spent = "REAL",
    created_at = "TEXT",
    updated_at = "TEXT"
  )
  for (column_name in names(dimension_rating_columns)) {
    db_add_column_if_missing(con, "human_preview_dimension_ratings", column_name, dimension_rating_columns[[column_name]])
  }

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_dimension_ratings_v2 (
      id INTEGER PRIMARY KEY,
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      canonical_article_key TEXT,
      interface_version TEXT,
      rating_mode TEXT,
      manifest_version TEXT,
      shown_title TEXT,
      shown_subtitle TEXT,
      shown_thumbnail_path TEXT,
      shown_image_sha256 TEXT,
      personal_click_appeal INTEGER,
      title_hook_strength INTEGER,
      visual_hook INTEGER,
      emotional_pull_preview INTEGER,
      ai_low_effort_flag TEXT,
      human_dimension_note TEXT,
      skipped INTEGER DEFAULT 0,
      shown_at TEXT,
      rated_at TEXT,
      seconds_spent REAL,
      created_at TEXT,
      updated_at TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_dimension_pass_queue (
      rating_mode TEXT,
      active_dimension TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      canonical_article_key TEXT,
      status TEXT DEFAULT 'pending',
      shown_at TEXT,
      completed_at TEXT,
      seconds_spent REAL,
      PRIMARY KEY (rating_mode, active_dimension, queue_position)
    )
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_queue_status
    ON human_preview_rating_queue (rating_session_id, status, queue_position)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_ratings_session
    ON human_preview_ratings (rating_session_id, rated_at, id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_queue_source
    ON human_preview_rating_queue (rating_session_id, source_type, article_lab_candidate_id, status)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_ratings_source
    ON human_preview_ratings (source_type, article_lab_candidate_id, rated_at, id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_dimension_queue_status
    ON human_preview_dimension_rating_queue (rating_session_id, status, queue_position)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_mode
    ON human_preview_dimension_ratings (rating_mode, rated_at, id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_v2_mode
    ON human_preview_dimension_ratings_v2 (rating_mode, manifest_version, updated_at, id)
  ")

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_canonical_unique
    ON human_preview_dimension_ratings (rating_mode, canonical_article_key)
    WHERE canonical_article_key IS NOT NULL
  "), silent = TRUE)

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_article_unique
    ON human_preview_dimension_ratings (rating_mode, article_id)
    WHERE canonical_article_key IS NULL AND article_id IS NOT NULL
  "), silent = TRUE)

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_post_unique
    ON human_preview_dimension_ratings (rating_mode, medium_post_id)
    WHERE canonical_article_key IS NULL AND article_id IS NULL AND medium_post_id IS NOT NULL
  "), silent = TRUE)

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_v2_canonical_unique
    ON human_preview_dimension_ratings_v2 (rating_mode, manifest_version, canonical_article_key)
    WHERE canonical_article_key IS NOT NULL
  "), silent = TRUE)

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_v2_article_unique
    ON human_preview_dimension_ratings_v2 (rating_mode, manifest_version, article_id)
    WHERE canonical_article_key IS NULL AND article_id IS NOT NULL
  "), silent = TRUE)

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_v2_post_unique
    ON human_preview_dimension_ratings_v2 (rating_mode, manifest_version, medium_post_id)
    WHERE canonical_article_key IS NULL AND article_id IS NULL AND medium_post_id IS NOT NULL
  "), silent = TRUE)

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_dimension_pass_queue_status
    ON human_preview_dimension_pass_queue (rating_mode, active_dimension, status, queue_position)
  ")
}

article_lab_default_prompt <- paste(
  "Generate Medium-style article titles for personal finance and investing readers.",
  "The titles should be science-based, beginner-friendly, credible, and clearly useful.",
  "Avoid clickbait, overclaiming, and hype.",
  "Lean into strong emotional tension or curiosity without sounding manipulative.",
  "Prefer specific, human, readable titles that feel plausible on Medium.",
  sep = "\n"
)

article_lab_manual_prompt_key <- "manual_default"

load_article_lab_prompt <- function(con, prompt_key = article_lab_manual_prompt_key) {
  key <- article_lab_input_string(prompt_key) %||% article_lab_manual_prompt_key
  if (!dbExistsTable(con, "article_lab_prompts")) return(article_lab_default_prompt)
  rows <- dbGetQuery(con, "
    SELECT prompt_text
    FROM article_lab_prompts
    WHERE prompt_key = ?
    LIMIT 1
  ", params = list(key))
  if (nrow(rows) == 0) return(article_lab_default_prompt)
  article_lab_input_multiline(rows$prompt_text[[1]]) %||% article_lab_default_prompt
}

save_article_lab_prompt <- function(con, prompt_text, prompt_key = article_lab_manual_prompt_key) {
  key <- article_lab_input_string(prompt_key) %||% article_lab_manual_prompt_key
  text <- article_lab_input_multiline(prompt_text) %||% article_lab_default_prompt
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
article_lab_outline_model_choices <- article_lab_model_choices_with_default(article_lab_default_outline_model)
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
article_lab_default_score_prompt_version <- "v2_2"
article_lab_default_score_scope <- "title_only"
article_lab_all_batches_value <- "__all_article_lab_batches__"
article_lab_title_max_chars <- 140L
article_lab_title_preferred_min_chars <- 40L
article_lab_title_preferred_max_chars <- 75L
article_lab_title_mobile_safe_chars <- 45L
article_lab_title_good_chars <- 60L
article_lab_title_long_allowed_chars <- 90L
article_lab_subtitle_max_chars <- 90L
article_lab_default_thumbnail_variants <- 3L
article_lab_candidate_status_values <- c(
  "generated",
  "disqualified",
  "ready_for_api_scoring",
  "api_pending",
  "api_scored",
  "approved_for_subtitle",
  "ready_for_thumbnail",
  "ready_for_outline",
  "ready_for_draft",
  "archived",
  "rejected"
)
article_lab_candidate_status_labels <- c(
  generated = "New",
  disqualified = "Disqualified",
  ready_for_api_scoring = "API queue",
  api_pending = "API scoring",
  api_scored = "API scored",
  approved_for_subtitle = "Approved",
  ready_for_thumbnail = "Ready for thumbnail",
  ready_for_outline = "Ready for outline",
  ready_for_draft = "Ready for draft",
  archived = "Archived",
  rejected = "Rejected",
  draft = "Draft"
)
article_lab_subtitle_status_labels <- c(
  generated = "Generated",
  approved = "Approved",
  rejected = "Rejected"
)
article_lab_thumbnail_status_labels <- c(
  generated = "Generated",
  approved = "Approved",
  rejected = "Rejected"
)
article_lab_workflow_sections <- c(
  "research_inbox",
  "summary",
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
    nav_title = "Research Inbox",
    nav_subtitle = "Track papers and article angles",
    title = "Article Lab - Research Inbox",
    subtitle = "Track papers and article angles."
  ),
  summary = list(
    nav_title = "Summary",
    nav_subtitle = "Check paper summary",
    title = "Article Lab - Summary",
    subtitle = "Check paper summary."
  ),
  generate = list(
    nav_title = "Generate",
    nav_subtitle = "Generate & triage titles",
    title = "Article Lab \u2013 Generate",
    subtitle = "Generate title candidates, disqualify bad-fit ideas, and move selected titles to the API queue."
  ),
  api_scoring = list(
    nav_title = "API Scoring",
    nav_subtitle = "Score with API & approve",
    title = "Article Lab \u2013 API Scoring",
    subtitle = "Score queued titles with the API and manually approve selected titles for subtitle generation."
  ),
  subtitle_generation = list(
    nav_title = "Subtitle Generation",
    nav_subtitle = "Generate subtitles",
    title = "Article Lab \u2013 Subtitle Generation",
    subtitle = "Generate subtitle candidates for approved titles."
  ),
  thumbnails = list(
    nav_title = "Thumbnails",
    nav_subtitle = "Generate thumbnails",
    title = "Article Lab \u2013 Thumbnails",
    subtitle = "Create and evaluate thumbnail candidates."
  ),
  outline = list(
    nav_title = "Outline",
    nav_subtitle = "Create article outline",
    title = "Article Lab \u2013 Outline",
    subtitle = "Build the article structure before drafting."
  ),
  full_text = list(
    nav_title = "Full Text",
    nav_subtitle = "Write full article",
    title = "Article Lab \u2013 Full Text",
    subtitle = "Draft the full article."
  ),
  review_publish = list(
    nav_title = "Review & Publish",
    nav_subtitle = "Review and publish",
    title = "Article Lab \u2013 Review & Publish",
    subtitle = "Review the final article package before publishing."
  ),
  settings = list(
    nav_title = "Settings",
    nav_subtitle = "App settings"
  )
)
article_lab_score_fields <- c(
  "clarity",
  "curiosity",
  "specificity",
  "beginner_appeal",
  "credibility",
  "emotional_pull",
  "promise_strength",
  "trust_risk",
  "medium_clap_potential",
  "medium_comment_potential",
  "overall_article_potential"
)

article_lab_title_length_flag <- function(char_count) {
  count <- suppressWarnings(as.integer(char_count))
  ifelse(
    is.na(count),
    NA_character_,
    ifelse(
      count <= article_lab_title_mobile_safe_chars,
      "mobile_safe",
      ifelse(
        count <= article_lab_title_good_chars,
        "good",
        ifelse(
          count <= article_lab_title_long_allowed_chars,
          "long_but_allowed",
          ifelse(count <= article_lab_title_max_chars, "very_long_but_allowed", "too_long")
        )
      )
    )
  )
}

article_lab_normalize_score <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  normalized <- ((value - 1) / 4) * 100
  normalized[is.na(value)] <- NA_real_
  pmax(0, pmin(100, normalized))
}

article_lab_combined_title_score <- function(curiosity, emotional_pull, medium_comment_potential, overall_article_potential, trust_risk, title_char_count = NA_integer_) {
  curiosity_norm <- article_lab_normalize_score(curiosity)
  emotional_norm <- article_lab_normalize_score(emotional_pull)
  comment_norm <- article_lab_normalize_score(medium_comment_potential)
  overall_norm <- article_lab_normalize_score(overall_article_potential)
  trust_norm <- article_lab_normalize_score(trust_risk)
  char_count <- suppressWarnings(as.integer(title_char_count))
  trust_penalty <- ifelse(is.na(trust_norm), 0, 0.18 * trust_norm)
  length_penalty <- ifelse(
    is.na(char_count),
    0,
    ifelse(
      char_count > article_lab_title_max_chars,
      35,
      ifelse(char_count > article_lab_title_long_allowed_chars, 20, ifelse(char_count > article_lab_title_preferred_max_chars, 8, 0))
    )
  )
  raw_score <- (0.25 * curiosity_norm) +
    (0.30 * emotional_norm) +
    (0.25 * comment_norm) +
    (0.20 * overall_norm) -
    trust_penalty -
    length_penalty
  round(pmax(0, pmin(100, raw_score)), 1)
}

article_lab_normalize_candidate_status <- function(status, ready_for_human_rating = 0, promoted = 0, archived = 0) {
  status_value <- clean_text(status)
  status_value <- if (length(status_value) == 0 || is.na(status_value[[1]])) NA_character_ else status_value[[1]]
  ready_value <- suppressWarnings(as.integer(ready_for_human_rating))
  promoted_value <- suppressWarnings(as.integer(promoted))
  archived_value <- suppressWarnings(as.integer(archived))

  if (!is.na(archived_value) && archived_value == 1L) return("archived")
  if (!is.na(promoted_value) && promoted_value == 1L) return("approved_for_subtitle")
  if (!is.na(status_value) && identical(status_value, "draft")) return("draft")
  if (!is.na(status_value) && status_value %in% c("promoted", "approved", "approved_for_subtitle")) return("approved_for_subtitle")
  if (!is.na(status_value) && identical(status_value, "ready_for_human_rating")) return("api_scored")
  if (!is.na(ready_value) && ready_value == 1L) return("api_scored")
  if (!is.na(status_value) && status_value %in% article_lab_candidate_status_values) return(status_value)
  "generated"
}

article_lab_status_label <- function(status) {
  label <- article_lab_candidate_status_labels[[status]]
  if (is.null(label) || is.na(label) || !nzchar(label)) status else label
}

article_lab_subtitle_status_label <- function(status) {
  label <- article_lab_subtitle_status_labels[[status]]
  if (is.null(label) || is.na(label) || !nzchar(label)) status else label
}

article_lab_thumbnail_status_label <- function(status) {
  label <- article_lab_thumbnail_status_labels[[status]]
  if (is.null(label) || is.na(label) || !nzchar(label)) status else label
}

article_lab_status_choices <- function(values) {
  setNames(values, vapply(values, article_lab_status_label, character(1)))
}

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

article_lab_normalize_candidate_rows <- function(rows) {
  if (nrow(rows) == 0) return(rows)
  ready_column <- if ("ready_for_human_rating" %in% names(rows)) rows$ready_for_human_rating else rep(0L, nrow(rows))
  promoted_column <- if ("promoted" %in% names(rows)) rows$promoted else rep(0L, nrow(rows))
  archived_column <- if ("archived" %in% names(rows)) rows$archived else rep(0L, nrow(rows))
  rows$normalized_status <- vapply(seq_len(nrow(rows)), function(i) {
    article_lab_normalize_candidate_status(
      status = rows$status[[i]],
      ready_for_human_rating = ready_column[[i]],
      promoted = promoted_column[[i]],
      archived = archived_column[[i]]
    )
  }, character(1))
  rows$status_label <- vapply(rows$normalized_status, article_lab_status_label, character(1))
  rows
}

ensure_article_lab_schema <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_prompts (
      prompt_key TEXT PRIMARY KEY,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      prompt_text TEXT NOT NULL
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_title_batches (
      batch_id TEXT PRIMARY KEY,
      created_at TEXT NOT NULL,
      prompt TEXT NOT NULL,
      seed_topic TEXT,
      inspiration_source TEXT,
      requested_batch_size INTEGER,
      model TEXT,
      status TEXT NOT NULL DEFAULT 'generated',
      notes TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_title_candidates (
      candidate_id TEXT PRIMARY KEY,
      batch_id TEXT NOT NULL,
      created_at TEXT NOT NULL,
      title TEXT NOT NULL,
      title_char_count INTEGER,
      title_length_flag TEXT,
      status TEXT NOT NULL DEFAULT 'generated',
      source TEXT NOT NULL DEFAULT 'article_lab',
      ready_for_human_rating INTEGER NOT NULL DEFAULT 0,
      promoted INTEGER NOT NULL DEFAULT 0,
      archived INTEGER NOT NULL DEFAULT 0,
      notes TEXT,
      raw_json TEXT,
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  db_add_column_if_missing(con, "article_lab_title_candidates", "title_char_count", "INTEGER")
  db_add_column_if_missing(con, "article_lab_title_candidates", "title_length_flag", "TEXT")
  db_add_column_if_missing(con, "article_lab_title_candidates", "notes", "TEXT")

  prompt_columns <- list(
    prompt_key = "TEXT NOT NULL DEFAULT ''", created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''", prompt_text = "TEXT NOT NULL DEFAULT ''"
  )
  for (column_name in names(prompt_columns)) db_add_column_if_missing(con, "article_lab_prompts", column_name, prompt_columns[[column_name]])

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_title_api_scores (
      score_id TEXT PRIMARY KEY,
      candidate_id TEXT NOT NULL,
      batch_id TEXT NOT NULL,
      scored_at TEXT NOT NULL,
      model TEXT NOT NULL,
      prompt_version TEXT NOT NULL,
      scope TEXT NOT NULL,
      clarity REAL,
      curiosity REAL,
      specificity REAL,
      beginner_appeal REAL,
      credibility REAL,
      emotional_pull REAL,
      promise_strength REAL,
      trust_risk REAL,
      medium_clap_potential REAL,
      medium_comment_potential REAL,
      overall_article_potential REAL,
      combined_title_score REAL,
      predicted_success_bucket TEXT,
      short_reason TEXT,
      raw_json TEXT,
      error TEXT,
      FOREIGN KEY(candidate_id) REFERENCES article_lab_title_candidates(candidate_id),
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_subtitle_candidates (
      subtitle_id TEXT PRIMARY KEY,
      candidate_id TEXT NOT NULL,
      batch_id TEXT NOT NULL,
      created_at TEXT NOT NULL,
      subtitle TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'generated',
      notes TEXT,
      model TEXT,
      generation_mode TEXT NOT NULL DEFAULT 'generated',
      raw_json TEXT,
      approved_at TEXT,
      rejected_at TEXT,
      FOREIGN KEY(candidate_id) REFERENCES article_lab_title_candidates(candidate_id),
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "notes", "TEXT")
  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "model", "TEXT")
  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "generation_mode", "TEXT")
  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "raw_json", "TEXT")
  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "approved_at", "TEXT")
  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "rejected_at", "TEXT")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_thumbnail_candidates (
      thumbnail_id TEXT PRIMARY KEY,
      subtitle_id TEXT NOT NULL,
      candidate_id TEXT NOT NULL,
      batch_id TEXT NOT NULL,
      created_at TEXT NOT NULL,
      thumbnail_label TEXT,
      thumbnail_data_uri TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'generated',
      notes TEXT,
      model TEXT,
      generation_mode TEXT NOT NULL DEFAULT 'generated',
      raw_json TEXT,
      approved_at TEXT,
      rejected_at TEXT,
      FOREIGN KEY(subtitle_id) REFERENCES article_lab_subtitle_candidates(subtitle_id),
      FOREIGN KEY(candidate_id) REFERENCES article_lab_title_candidates(candidate_id),
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "thumbnail_label", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "notes", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "model", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "generation_mode", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "raw_json", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "approved_at", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "rejected_at", "TEXT")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_outlines (
      outline_id TEXT PRIMARY KEY,
      thumbnail_id TEXT NOT NULL,
      subtitle_id TEXT NOT NULL,
      candidate_id TEXT NOT NULL,
      batch_id TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      outline_text TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'draft',
      notes TEXT,
      model TEXT,
      generation_mode TEXT NOT NULL DEFAULT 'generated',
      raw_json TEXT,
      approved_at TEXT,
      FOREIGN KEY(thumbnail_id) REFERENCES article_lab_thumbnail_candidates(thumbnail_id),
      FOREIGN KEY(subtitle_id) REFERENCES article_lab_subtitle_candidates(subtitle_id),
      FOREIGN KEY(candidate_id) REFERENCES article_lab_title_candidates(candidate_id),
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  db_add_column_if_missing(con, "article_lab_outlines", "updated_at", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "notes", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "model", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "generation_mode", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "raw_json", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "approved_at", "TEXT")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_batches_created_at
    ON article_lab_title_batches (created_at, batch_id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_candidates_batch
    ON article_lab_title_candidates (batch_id, created_at, candidate_id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_candidates_status
    ON article_lab_title_candidates (status, ready_for_human_rating, archived, promoted)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_api_scores_batch
    ON article_lab_title_api_scores (batch_id, scored_at, candidate_id)
  ")

  dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_article_lab_title_api_scores_cache
    ON article_lab_title_api_scores (candidate_id, model, prompt_version, scope)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_api_scores_prompt
    ON article_lab_title_api_scores (prompt_version, model, scope)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_subtitle_candidates_batch
    ON article_lab_subtitle_candidates (batch_id, candidate_id, created_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_subtitle_candidates_status
    ON article_lab_subtitle_candidates (status, candidate_id, created_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_thumbnail_candidates_batch
    ON article_lab_thumbnail_candidates (batch_id, subtitle_id, created_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_thumbnail_candidates_status
    ON article_lab_thumbnail_candidates (status, subtitle_id, created_at)
  ")

  dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_article_lab_thumbnail_candidates_one_approved_per_package
    ON article_lab_thumbnail_candidates (subtitle_id)
    WHERE status = 'approved'
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_outlines_batch
    ON article_lab_outlines (batch_id, thumbnail_id, updated_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_outlines_status
    ON article_lab_outlines (status, candidate_id, updated_at)
  ")

  dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_article_lab_outlines_one_active_per_thumbnail
    ON article_lab_outlines (thumbnail_id)
    WHERE status IN ('draft', 'approved')
  ")

  dbExecute(con, "
    UPDATE article_lab_title_candidates
    SET title_char_count = COALESCE(title_char_count, LENGTH(COALESCE(title, ''))),
        title_length_flag = CASE
          WHEN LENGTH(COALESCE(title, '')) <= 45 THEN 'mobile_safe'
          WHEN LENGTH(COALESCE(title, '')) <= 60 THEN 'good'
          WHEN LENGTH(COALESCE(title, '')) <= 90 THEN 'long_but_allowed'
          WHEN LENGTH(COALESCE(title, '')) <= 140 THEN 'very_long_but_allowed'
          ELSE 'too_long'
        END
    WHERE title_char_count IS NULL
       OR title_length_flag IS NULL
       OR title_length_flag = 'risky'
       OR (title_length_flag = 'too_long' AND LENGTH(COALESCE(title, '')) <= 140)
       OR title_length_flag NOT IN ('mobile_safe', 'good', 'long_but_allowed', 'very_long_but_allowed', 'too_long')
  ")

  dbExecute(con, "
    UPDATE article_lab_title_candidates
    SET ready_for_human_rating = 0,
        status = CASE
          WHEN candidate_id IN (
            SELECT DISTINCT candidate_id
            FROM article_lab_title_api_scores
          ) THEN 'api_scored'
          ELSE 'generated'
        END
    WHERE ready_for_human_rating = 1
      AND status = 'ready_for_human_rating'
      AND archived = 0
  ")

  dbExecute(con, "
    UPDATE article_lab_title_candidates
    SET status = 'approved_for_subtitle',
        promoted = 1,
        ready_for_human_rating = 0,
        archived = 0
    WHERE promoted = 1
       OR status IN ('promoted', 'approved', 'approved_for_subtitle')
  ")

  dbExecute(con, "
    UPDATE article_lab_title_candidates
    SET status = 'ready_for_thumbnail',
        promoted = 0,
        ready_for_human_rating = 0,
        archived = 0
    WHERE candidate_id IN (
      SELECT DISTINCT candidate_id
      FROM article_lab_subtitle_candidates
      WHERE status = 'approved'
    )
  ")

  dbExecute(con, "
    UPDATE article_lab_title_candidates
    SET status = 'ready_for_outline',
        promoted = 0,
        ready_for_human_rating = 0,
        archived = 0
    WHERE candidate_id IN (
      SELECT DISTINCT candidate_id
      FROM article_lab_thumbnail_candidates
      WHERE status = 'approved'
    )
  ")

  dbExecute(con, "
    UPDATE article_lab_title_candidates
    SET status = 'archived',
        ready_for_human_rating = 0
    WHERE archived = 1
  ")
}

ensure_research_workflow_schema <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS research_sources (
      research_source_id INTEGER PRIMARY KEY,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      source_title TEXT NOT NULL,
      source_url TEXT,
      pdf_url TEXT,
      main_idea TEXT,
      abstract TEXT,
      source_type TEXT DEFAULT 'paper',
      source_name TEXT,
      manual_sort_order INTEGER,
      status TEXT NOT NULL DEFAULT 'new',
      notes TEXT,
      imported_from_table TEXT,
      imported_from_id TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS research_article_angles (
      research_angle_id INTEGER PRIMARY KEY,
      research_source_id INTEGER,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      angle_title TEXT NOT NULL,
      main_idea TEXT,
      manual_sort_order INTEGER,
      status TEXT NOT NULL DEFAULT 'idea',
      notes TEXT,
      article_lab_batch_id TEXT,
      FOREIGN KEY(research_source_id) REFERENCES research_sources(research_source_id)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS research_source_summaries (
      summary_id INTEGER PRIMARY KEY,
      research_source_id INTEGER NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      summary_text TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'draft',
      confirmed_at TEXT,
      model TEXT,
      prompt_version TEXT,
      notes TEXT,
      raw_json TEXT,
      FOREIGN KEY(research_source_id) REFERENCES research_sources(research_source_id)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS research_source_assets (
      asset_id INTEGER PRIMARY KEY,
      research_source_id INTEGER NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      asset_type TEXT NOT NULL DEFAULT 'pdf',
      source_url TEXT,
      local_path TEXT,
      original_filename TEXT,
      file_sha256 TEXT,
      status TEXT NOT NULL DEFAULT 'missing',
      error TEXT,
      notes TEXT,
      FOREIGN KEY(research_source_id) REFERENCES research_sources(research_source_id)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS research_summary_prompts (
      prompt_version TEXT PRIMARY KEY,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      prompt_text TEXT NOT NULL
    )
  ")

  source_columns <- list(
    source_url = "TEXT", pdf_url = "TEXT", main_idea = "TEXT", abstract = "TEXT",
    source_type = "TEXT DEFAULT 'paper'", source_name = "TEXT", manual_sort_order = "INTEGER",
    notes = "TEXT", imported_from_table = "TEXT", imported_from_id = "TEXT"
  )
  for (column_name in names(source_columns)) db_add_column_if_missing(con, "research_sources", column_name, source_columns[[column_name]])

  angle_columns <- list(
    research_source_id = "INTEGER", main_idea = "TEXT", manual_sort_order = "INTEGER",
    notes = "TEXT", article_lab_batch_id = "TEXT"
  )
  for (column_name in names(angle_columns)) db_add_column_if_missing(con, "research_article_angles", column_name, angle_columns[[column_name]])

  summary_columns <- list(
    research_source_id = "INTEGER NOT NULL DEFAULT 0", created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''", summary_text = "TEXT NOT NULL DEFAULT ''",
    status = "TEXT NOT NULL DEFAULT 'draft'", confirmed_at = "TEXT", model = "TEXT",
    prompt_version = "TEXT", notes = "TEXT", raw_json = "TEXT"
  )
  for (column_name in names(summary_columns)) db_add_column_if_missing(con, "research_source_summaries", column_name, summary_columns[[column_name]])

  asset_columns <- list(
    research_source_id = "INTEGER NOT NULL DEFAULT 0", created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''", asset_type = "TEXT NOT NULL DEFAULT 'pdf'",
    source_url = "TEXT", local_path = "TEXT", original_filename = "TEXT", file_sha256 = "TEXT",
    status = "TEXT NOT NULL DEFAULT 'missing'", error = "TEXT", notes = "TEXT"
  )
  for (column_name in names(asset_columns)) db_add_column_if_missing(con, "research_source_assets", column_name, asset_columns[[column_name]])

  prompt_columns <- list(
    prompt_version = "TEXT NOT NULL DEFAULT ''", created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''", prompt_text = "TEXT NOT NULL DEFAULT ''"
  )
  for (column_name in names(prompt_columns)) db_add_column_if_missing(con, "research_summary_prompts", column_name, prompt_columns[[column_name]])

  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_sources_status_sort_updated ON research_sources (status, manual_sort_order, updated_at)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_sources_name_type ON research_sources (source_name, source_type)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_article_angles_status_sort_updated ON research_article_angles (status, manual_sort_order, updated_at)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_article_angles_source ON research_article_angles (research_source_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_source_summaries_source_status_updated ON research_source_summaries (research_source_id, status, updated_at)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_source_summaries_status_confirmed_updated ON research_source_summaries (status, confirmed_at, updated_at)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_source_assets_source_type_status_updated ON research_source_assets (research_source_id, asset_type, status, updated_at)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_source_assets_file_sha256 ON research_source_assets (file_sha256)")
}

article_lab_batch_id <- function() {
  paste0("alb_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(99999L, 1))
}

article_lab_candidate_id <- function(batch_id, index) {
  paste0("alc_", batch_id, "_", sprintf("%02d", as.integer(index)))
}

article_lab_outline_id <- function(thumbnail_id) {
  paste0("alo_", gsub("[^A-Za-z0-9]+", "_", thumbnail_id), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(99999L, 1))
}

research_workflow_sort_sql <- "CASE WHEN manual_sort_order IS NULL THEN 1 ELSE 0 END, manual_sort_order ASC, updated_at DESC"
research_source_sort_sql <- "CASE WHEN s.manual_sort_order IS NULL THEN 1 ELSE 0 END, s.manual_sort_order ASC, s.updated_at DESC"
research_ranked_source_sort_sql <- "s.manual_sort_order ASC, s.updated_at DESC"
research_unranked_source_sort_sql <- "s.updated_at DESC"
research_angle_sort_sql <- "CASE WHEN a.manual_sort_order IS NULL THEN 1 ELSE 0 END, a.manual_sort_order ASC, a.updated_at DESC"

research_input_value <- function(value) {
  cleaned <- clean_text(value)
  if (length(cleaned) == 0 || is.na(cleaned[[1]])) NA_character_ else cleaned[[1]]
}

research_multiline_value <- function(value) {
  cleaned <- clean_multiline_text(value)
  if (length(cleaned) == 0 || is.na(cleaned[[1]])) NA_character_ else cleaned[[1]]
}

research_input_default <- function(value, default) {
  cleaned <- research_input_value(value)
  if (is.na(cleaned)) default else cleaned
}

research_input_integer <- function(value) {
  cleaned <- research_input_value(value)
  number <- suppressWarnings(as.integer(cleaned))
  if (is.na(number)) NA_integer_ else number
}

research_numeric_default <- function(value) {
  number <- suppressWarnings(as.integer(value))
  if (length(number) == 0 || is.na(number[[1]])) NULL else number[[1]]
}

load_research_sources <- function(con, status = "__all__", ranked = NULL) {
  if (!dbExistsTable(con, "research_sources")) return(data.frame())
  status_value <- research_input_value(status)
  where <- character()
  params <- list()
  if (!is.na(status_value) && !identical(status_value, "__all__")) {
    where <- c(where, "s.status = ?")
    params <- c(params, list(status_value))
  }
  if (isTRUE(ranked)) {
    where <- c(where, "s.manual_sort_order IS NOT NULL")
    order_sql <- research_ranked_source_sort_sql
  } else if (identical(ranked, FALSE)) {
    where <- c(where, "s.manual_sort_order IS NULL")
    order_sql <- research_unranked_source_sort_sql
  } else {
    order_sql <- research_source_sort_sql
  }
  source_query <- "
    SELECT s.*, COUNT(a.research_angle_id) AS angles_count
    FROM research_sources s
    LEFT JOIN research_article_angles a ON a.research_source_id = s.research_source_id
  "
  where_sql <- if (length(where) > 0) paste0(" WHERE ", paste(where, collapse = " AND ")) else ""
  query <- paste0(source_query, where_sql, " GROUP BY s.research_source_id ORDER BY ", order_sql)
  if (length(params) > 0) dbGetQuery(con, query, params = params) else dbGetQuery(con, query)
}

load_research_source_by_id <- function(con, source_id) {
  source_id_value <- research_input_integer(source_id)
  if (is.na(source_id_value) || !dbExistsTable(con, "research_sources")) return(data.frame())
  dbGetQuery(con, "
    SELECT s.*, COUNT(a.research_angle_id) AS angles_count
    FROM research_sources s
    LEFT JOIN research_article_angles a ON a.research_source_id = s.research_source_id
    WHERE s.research_source_id = ?
    GROUP BY s.research_source_id
    LIMIT 1
  ", params = list(source_id_value))
}

load_research_angles <- function(con, source_id = NULL) {
  if (!dbExistsTable(con, "research_article_angles")) return(data.frame())
  source_id_value <- research_input_integer(source_id)
  angle_query <- paste0("
    SELECT a.*, s.source_title, s.source_url, s.pdf_url, s.main_idea AS source_main_idea, s.abstract AS source_abstract
    FROM research_article_angles a
    LEFT JOIN research_sources s ON s.research_source_id = a.research_source_id
  ")
  if (!is.na(source_id_value)) {
    return(dbGetQuery(con, paste0(angle_query, " WHERE a.research_source_id = ? ORDER BY ", research_angle_sort_sql), params = list(source_id_value)))
  }
  dbGetQuery(con, paste0(angle_query, " ORDER BY ", research_angle_sort_sql))
}

research_truncate <- function(value, max_chars = 90L) {
  value <- research_input_value(value)
  if (is.na(value)) return("")
  if (nchar(value, type = "chars") <= max_chars) return(value)
  paste0(substr(value, 1L, max_chars - 3L), "...")
}

research_link <- function(url, label) {
  value <- research_input_value(url)
  if (is.na(value)) return("")
  sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">%s</a>', htmltools::htmlEscape(value), htmltools::htmlEscape(label))
}

research_links <- function(source_url, pdf_url) {
  links <- c(research_link(source_url, "Open"), research_link(pdf_url, "PDF"))
  links <- links[nzchar(links)]
  paste(links, collapse = " &middot; ")
}

research_summary_template <- paste(
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

load_research_source_summary <- function(con, source_id, status = NULL) {
  source_id_value <- research_input_integer(source_id)
  if (is.na(source_id_value) || !dbExistsTable(con, "research_source_summaries")) return(data.frame())
  status_value <- research_input_value(status)
  if (!is.na(status_value)) {
    return(dbGetQuery(con, "
      SELECT *
      FROM research_source_summaries
      WHERE research_source_id = ? AND status = ?
      ORDER BY updated_at DESC, summary_id DESC
      LIMIT 1
    ", params = list(source_id_value, status_value)))
  }
  dbGetQuery(con, "
    SELECT *
    FROM research_source_summaries
    WHERE research_source_id = ?
    ORDER BY CASE status WHEN 'draft' THEN 0 WHEN 'confirmed' THEN 1 ELSE 2 END, updated_at DESC, summary_id DESC
    LIMIT 1
  ", params = list(source_id_value))
}

load_confirmed_research_summaries <- function(con) {
  if (!dbExistsTable(con, "research_source_summaries") || !dbExistsTable(con, "research_sources")) return(data.frame())
  dbGetQuery(con, "
    SELECT ss.*, s.source_title, s.source_url, s.pdf_url, s.main_idea, s.abstract,
      s.status AS source_status, s.manual_sort_order
    FROM research_source_summaries ss
    JOIN research_sources s ON s.research_source_id = ss.research_source_id
    WHERE ss.status = 'confirmed'
    ORDER BY CASE WHEN s.manual_sort_order IS NULL THEN 1 ELSE 0 END,
      s.manual_sort_order ASC, ss.confirmed_at DESC, ss.updated_at DESC, ss.summary_id DESC
  ")
}

research_summary_prompt <- function(summary_row) {
  paste(
    paste("Source title:", summary_row$source_title[[1]] %||% ""),
    paste("Source link:", summary_row$source_url[[1]] %||% ""),
    paste("PDF link:", summary_row$pdf_url[[1]] %||% ""),
    paste("Confirmed summary:", summary_row$summary_text[[1]] %||% ""),
    sep = "\n\n"
  )
}

article_lab_research_summary_id_from_source <- function(inspiration_source) {
  value <- article_lab_input_string(inspiration_source)
  if (length(value) != 1L || is.na(value) || !isTRUE(grepl("^research_summary:[0-9]+$", value))) return(NA_integer_)
  suppressWarnings(as.integer(sub("^research_summary:", "", value)))
}

load_article_lab_batch_summary_contexts <- function(con, batch_ids) {
  batch_ids <- clean_text(batch_ids)
  batch_ids <- unique(batch_ids[!is.na(batch_ids)])
  if (length(batch_ids) == 0 || !dbExistsTable(con, "article_lab_title_batches")) return(data.frame())
  placeholders <- paste(rep("?", length(batch_ids)), collapse = ", ")
  batches <- dbGetQuery(
    con,
    sprintf("SELECT batch_id, inspiration_source FROM article_lab_title_batches WHERE batch_id IN (%s)", placeholders),
    params = as.list(batch_ids)
  )
  if (nrow(batches) == 0) return(data.frame())
  batches$summary_id <- vapply(batches$inspiration_source, article_lab_research_summary_id_from_source, integer(1))
  batches <- batches[!is.na(batches$summary_id), , drop = FALSE]
  if (nrow(batches) == 0 || !dbExistsTable(con, "research_source_summaries") || !dbExistsTable(con, "research_sources")) return(data.frame())

  summary_ids <- unique(batches$summary_id)
  summary_placeholders <- paste(rep("?", length(summary_ids)), collapse = ", ")
  summaries <- dbGetQuery(
    con,
    sprintf(
      "SELECT ss.summary_id, ss.summary_text, s.source_title, s.source_url, s.pdf_url
       FROM research_source_summaries ss
       JOIN research_sources s ON s.research_source_id = ss.research_source_id
       WHERE ss.summary_id IN (%s)",
      summary_placeholders
    ),
    params = as.list(summary_ids)
  )
  if (nrow(summaries) == 0) return(data.frame())

  contexts <- merge(batches[, c("batch_id", "summary_id"), drop = FALSE], summaries, by = "summary_id", all.x = FALSE, all.y = FALSE)
  contexts$article_summary <- vapply(seq_len(nrow(contexts)), function(i) research_summary_prompt(contexts[i, , drop = FALSE]), character(1))
  contexts[, c("batch_id", "summary_id", "source_title", "article_summary"), drop = FALSE]
}

research_pdf_dir <- file.path(project_root, "data", "research_pdfs")

research_pdf_status_labels <- c(
  missing = "Missing",
  downloaded = "Downloaded",
  uploaded = "Uploaded manually",
  failed = "Download failed"
)

load_research_summary_prompt <- function(con, prompt_version) {
  version <- article_lab_input_string(prompt_version) %||% article_lab_default_research_summary_prompt_version
  if (!dbExistsTable(con, "research_summary_prompts")) return(article_lab_default_research_summary_prompt)
  rows <- dbGetQuery(con, "
    SELECT prompt_text
    FROM research_summary_prompts
    WHERE prompt_version = ?
    LIMIT 1
  ", params = list(version))
  if (nrow(rows) == 0) return(article_lab_default_research_summary_prompt)
  article_lab_input_multiline(rows$prompt_text[[1]]) %||% article_lab_default_research_summary_prompt
}

save_research_summary_prompt <- function(con, prompt_version, prompt_text) {
  version <- article_lab_input_string(prompt_version) %||% article_lab_default_research_summary_prompt_version
  text <- article_lab_input_multiline(prompt_text) %||% article_lab_default_research_summary_prompt
  timestamp <- now_utc()
  rows <- dbGetQuery(con, "SELECT prompt_version FROM research_summary_prompts WHERE prompt_version = ? LIMIT 1", params = list(version))
  if (nrow(rows) > 0) {
    dbExecute(con, "
      UPDATE research_summary_prompts
      SET updated_at = ?, prompt_text = ?
      WHERE prompt_version = ?
    ", params = list(timestamp, text, version))
    return(invisible(version))
  }
  dbExecute(con, "
    INSERT INTO research_summary_prompts (prompt_version, created_at, updated_at, prompt_text)
    VALUES (?, ?, ?, ?)
  ", params = list(version, timestamp, timestamp, text))
  invisible(version)
}

research_safe_file_slug <- function(value) {
  value <- research_input_default(value, "research-source")
  value <- iconv(value, to = "ASCII//TRANSLIT", sub = "")
  value <- tolower(gsub("[^a-z0-9]+", "-", value))
  value <- gsub("(^-+|-+$)", "", value)
  if (!nzchar(value)) "research-source" else substr(value, 1L, 80L)
}

research_pdf_local_path <- function(source_id, title, original_filename = NULL) {
  dir.create(research_pdf_dir, recursive = TRUE, showWarnings = FALSE)
  extension <- tolower(tools::file_ext(original_filename %||% ""))
  if (!identical(extension, "pdf")) extension <- "pdf"
  file.path(research_pdf_dir, sprintf("research_source_%s_%s.%s", as.integer(source_id), research_safe_file_slug(title), extension))
}

research_resolve_local_pdf_path <- function(path) {
  value <- research_input_value(path)
  if (is.na(value)) return(NA_character_)
  candidates <- if (grepl("^(/|[A-Za-z]:[/\\\\])", value)) {
    value
  } else {
    c(value, file.path(project_root, value))
  }
  for (candidate in candidates) {
    if (file.exists(candidate)) return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }
  value
}

research_pdf_sha256 <- function(path) {
  value <- tools::sha256sum(path)
  unname(as.character(value[[1]]))
}

research_file_is_pdf <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 5) return(FALSE)
  header <- readBin(path, what = "raw", n = 5L)
  identical(rawToChar(header), "%PDF-")
}

research_pdf_source_url <- function(source) {
  pdf_url <- research_input_value(source$pdf_url[[1]])
  if (!is.na(pdf_url)) return(pdf_url)
  source_url <- research_input_value(source$source_url[[1]])
  if (!is.na(source_url) && grepl("\\.pdf($|[?#])", source_url, ignore.case = TRUE)) return(source_url)
  NA_character_
}

load_research_pdf_asset <- function(con, source_id) {
  source_id_value <- research_input_integer(source_id)
  if (is.na(source_id_value) || !dbExistsTable(con, "research_source_assets")) return(data.frame())
  dbGetQuery(con, "
    SELECT *
    FROM research_source_assets
    WHERE research_source_id = ? AND asset_type = 'pdf'
    ORDER BY updated_at DESC, asset_id DESC
    LIMIT 1
  ", params = list(source_id_value))
}

save_research_pdf_asset <- function(con, source_id, status, source_url = NA_character_, local_path = NA_character_, original_filename = NA_character_, file_sha256 = NA_character_, error = NA_character_) {
  timestamp <- now_utc()
  existing <- load_research_pdf_asset(con, source_id)
  local_path <- research_resolve_local_pdf_path(local_path)
  values <- list(timestamp, source_url, local_path, original_filename, file_sha256, status, error)
  if (nrow(existing) > 0) {
    dbExecute(con, "
      UPDATE research_source_assets
      SET updated_at = ?, source_url = ?, local_path = ?, original_filename = ?, file_sha256 = ?, status = ?, error = ?
      WHERE asset_id = ?
    ", params = c(values, list(existing$asset_id[[1]])))
    return(existing$asset_id[[1]])
  }
  dbExecute(con, "
    INSERT INTO research_source_assets
      (research_source_id, created_at, updated_at, asset_type, source_url, local_path, original_filename, file_sha256, status, error)
    VALUES (?, ?, ?, 'pdf', ?, ?, ?, ?, ?, ?)
  ", params = list(source_id, timestamp, timestamp, source_url, local_path, original_filename, file_sha256, status, error))
  dbGetQuery(con, "SELECT last_insert_rowid() AS asset_id")$asset_id[[1]]
}

research_summary_api_request <- function(source, asset, model = NA_character_, prompt_version = NA_character_, prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "summarize_research_pdf.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/summarize_research_pdf.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(source) == 0) stop("Select a source before generating a summary.", call. = FALSE)
  if (nrow(asset) == 0 || !(asset$status[[1]] %in% c("downloaded", "uploaded"))) stop("Download or upload a PDF before generating an API summary.", call. = FALSE)
  local_pdf_path <- research_resolve_local_pdf_path(asset$local_path[[1]])
  if (is.na(local_pdf_path) || !file.exists(local_pdf_path)) stop("The selected PDF asset does not exist on disk.", call. = FALSE)

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_research_summary_model,
    prompt_version = article_lab_input_string(prompt_version) %||% article_lab_default_research_summary_prompt_version,
    prompt = article_lab_input_multiline(prompt) %||% article_lab_default_research_summary_prompt,
    research_source_id = source$research_source_id[[1]],
    source_title = article_lab_input_string(source$source_title[[1]]),
    source_url = article_lab_input_string(source$source_url[[1]]),
    pdf_url = article_lab_input_string(source$pdf_url[[1]]),
    main_idea = article_lab_input_multiline(source$main_idea[[1]]),
    abstract = article_lab_input_multiline(source$abstract[[1]]),
    local_pdf_path = local_pdf_path
  )

  request_file <- tempfile(pattern = "research_summary_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "research_summary_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "research_summary_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2("node", args = c(helper_path, request_file), stdout = stdout_file, stderr = stderr_file)
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Research summary helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Research summary helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  summary_text <- article_lab_input_multiline(parsed$summary_text)
  if (is.null(summary_text) || is.na(summary_text)) stop("Research summary helper returned no summary_text.", call. = FALSE)
  list(
    summary_text = summary_text,
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    prompt_version = article_lab_input_string(parsed$prompt_version) %||% request_payload$prompt_version,
    raw_json = stdout_text,
    response_id = article_lab_input_string(parsed$response_id)
  )
}

research_title_prompt <- function(source, angle) {
  source_context <- research_input_default(source$main_idea[[1]], research_input_default(source$abstract[[1]], ""))
  paste(
    "Generate reader-facing Medium titles, not academic paper summary titles.",
    "Stay credible, beginner-friendly, science-based, and do not overclaim what the source proves.",
    paste("Source title:", source$source_title[[1]] %||% ""),
    paste("Source link:", source$source_url[[1]] %||% ""),
    paste("PDF link:", source$pdf_url[[1]] %||% ""),
    paste("Main idea or abstract:", source_context),
    paste("Article angle title:", angle$angle_title[[1]] %||% ""),
    paste("Angle main idea:", angle$main_idea[[1]] %||% ""),
    sep = "\n\n"
  )
}

article_lab_title_length <- function(x) {
  nchar(enc2utf8(as.character(x)), type = "chars", allowNA = TRUE, keepNA = TRUE)
}

article_lab_validate_titles <- function(titles, max_chars = article_lab_title_max_chars) {
  title_values <- clean_text(titles)
  lengths <- article_lab_title_length(title_values)
  valid <- !is.na(title_values) & !is.na(lengths) & lengths <= max_chars
  valid[is.na(valid)] <- FALSE
  list(
    titles = title_values[valid],
    dropped_titles = title_values[!valid & !is.na(title_values)],
    kept_n = sum(valid, na.rm = TRUE),
    dropped_n = sum(!valid & !is.na(title_values), na.rm = TRUE)
  )
}

article_lab_parse_manual_titles <- function(value) {
  text_value <- as.character(value %||% "")
  if (length(text_value) == 0 || is.na(text_value[[1]]) || !nzchar(text_value[[1]])) return(character())
  pieces <- unlist(strsplit(text_value[[1]], "\n", fixed = TRUE), use.names = FALSE)
  pieces <- clean_text(pieces)
  pieces <- pieces[!is.na(pieces)]
  unique(pieces[nzchar(pieces)])
}

article_lab_normalize_titles <- function(titles) {
  title_values <- clean_text(titles)
  unique(title_values[!is.na(title_values)])
}

stub_title_candidates <- function(prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_) {
  prompt_value <- clean_text(prompt)
  if (length(prompt_value) == 0 || is.na(prompt_value[[1]])) prompt_value <- article_lab_default_prompt
  topic_value <- clean_text(seed_topic)
  source_value <- clean_text(inspiration_source)
  model_value <- clean_text(model)
  n <- suppressWarnings(as.integer(batch_size))
  if (is.na(n) || n < 1L) n <- 10L
  n <- min(n, 25L)

  topic_phrase <- if (length(topic_value) > 0 && !is.na(topic_value[[1]])) {
    topic_value[[1]]
  } else {
    "building wealth without getting lost in noise"
  }

  if (length(source_value) == 0 || is.na(source_value[[1]])) source_value <- "manual prompt"
  if (length(model_value) == 0 || is.na(model_value[[1]])) model_value <- article_lab_default_model

  seed_key <- paste(prompt_value[[1]], topic_phrase, source_value[[1]], model_value[[1]], sep = "|")
  set.seed(sum(utf8ToInt(seed_key)) %% .Machine$integer.max)

  opening <- c(
    "What Most Beginners Miss About",
    "The Quiet Truth About",
    "Why Smart People Still Struggle With",
    "A Better Way To Think About",
    "The Science-Backed Case For",
    "The Hidden Emotional Cost Of",
    "What Finally Helped Me Understand",
    "The Beginner-Friendly Guide To",
    "Why So Many People Overcomplicate",
    "The Calm, Credible Take On"
  )
  topic_suffix <- c(
    "index fund investing",
    "retirement planning",
    "financial independence",
    "building wealth slowly",
    "market volatility",
    "saving without burnout",
    "long-term investing",
    "money habits that actually stick",
    "avoiding expensive investing mistakes",
    "staying rational when headlines get loud"
  )
  payoff <- c(
    "Before Your Next Money Decision",
    "If You Want Progress Without Hype",
    "When You Want Less Stress And Better Odds",
    "Without Pretending The Future Is Predictable",
    "If You Are Tired Of Generic Advice",
    "For People Who Want A Realistic Plan",
    "Without Turning Finance Into A Full-Time Job",
    "If You Want Confidence, Not False Certainty",
    "For Beginners Who Value Evidence",
    "Without Falling For Clickbait"
  )

  titles <- character()
  attempts <- 0L
  while (length(titles) < n && attempts < n * 12L) {
    attempts <- attempts + 1L
    candidate <- paste(
      sample(opening, 1),
      if (!is.na(topic_phrase) && nzchar(topic_phrase) && runif(1) < 0.65) topic_phrase else sample(topic_suffix, 1)
    )
    if (runif(1) < 0.78) {
      candidate <- paste(candidate, sample(payoff, 1), sep = ": ")
    }
    titles <- unique(c(titles, candidate))
  }

  if (length(titles) < n) {
    filler <- vapply(seq_len(n - length(titles)), function(i) {
      paste("A Smarter Beginner's Way To Approach", topic_phrase, sprintf("(%s)", i))
    }, character(1))
    titles <- c(titles, filler)
  }

  validated <- article_lab_validate_titles(titles[seq_len(n)], max_chars = article_lab_title_max_chars)
  data.frame(
    row_number = seq_along(validated$titles),
    title = validated$titles,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

article_lab_input_string <- function(x) {
  value <- clean_text(x)
  if (length(value) == 0 || is.na(value[[1]])) NULL else value[[1]]
}

article_lab_input_multiline <- function(x) {
  value <- clean_multiline_text(x)
  if (length(value) == 0 || is.na(value[[1]])) NULL else value[[1]]
}

article_lab_has_api_key <- function() {
  env_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (nzchar(trimws(env_key))) return(TRUE)
  env_path <- file.path(project_root, ".env")
  if (!file.exists(env_path)) return(FALSE)
  lines <- tryCatch(readLines(env_path, warn = FALSE), error = function(e) character())
  any(grepl("^\\s*OPENAI_API_KEY\\s*=\\s*.+", lines))
}

article_lab_python_candidates <- function() {
  env_candidates <- clean_text(c(
    Sys.getenv("ARTICLE_LAB_PYTHON", unset = ""),
    Sys.getenv("WRITING_API_PYTHON", unset = "")
  ))
  project_candidates <- clean_text(c(
    file.path(project_root, ".local_gitignored", "article_lab_venv", "bin", "python"),
    file.path(project_root, ".venv", "bin", "python")
  ))
  project_candidates <- project_candidates[file.exists(project_candidates)]
  path_candidates <- clean_text(c(Sys.which("python3"), Sys.which("python")))
  unique(c(env_candidates[!is.na(env_candidates)], project_candidates[!is.na(project_candidates)], path_candidates[!is.na(path_candidates)]))
}

article_lab_resolve_python <- function() {
  candidates <- article_lab_python_candidates()
  if (length(candidates) == 0) {
    stop(
      "No Python interpreter found for Article Lab API scoring. ",
      "Set ARTICLE_LAB_PYTHON to the Python executable that has the OpenAI package installed.",
      call. = FALSE
    )
  }
  checks <- lapply(candidates, function(candidate) {
    check <- article_lab_python_package_check(candidate)
    check$python_bin <- candidate
    check
  })
  for (check in checks) {
    if (isTRUE(check$ok)) {
      message("Article Lab API scoring using Python: ", check$python_bin)
      return(check$python_bin)
    }
  }

  details <- vapply(checks, function(check) {
    detail <- clean_text(check$stderr) %||% clean_text(check$stdout) %||% "package import check failed"
    paste0(shQuote(check$python_bin), ": ", detail)
  }, character(1))
  stop(
    paste0(
      "No Python interpreter available to Article Lab API scoring can import the required package(s). ",
      article_lab_python_setup_message(candidates[[1]]),
      " Tried: ", paste(details, collapse = " | ")
    ),
    call. = FALSE
  )
}

article_lab_python_package_check <- function(python_bin) {
  stdout_file <- tempfile(pattern = "article_lab_python_check_stdout_", fileext = ".log")
  stderr_file <- tempfile(pattern = "article_lab_python_check_stderr_", fileext = ".log")
  on.exit(unlink(c(stdout_file, stderr_file), force = TRUE), add = TRUE)
  check_code <- paste(
    "import os",
    "import openai",
    "tracing = all((os.environ.get(name) or '').strip() for name in ('LANGFUSE_PUBLIC_KEY', 'LANGFUSE_SECRET_KEY')) and ((os.environ.get('LANGFUSE_BASE_URL') or os.environ.get('LANGFUSE_HOST') or '').strip())",
    "if tracing:",
    "    import langfuse",
    "    import langfuse.openai",
    sep = "\n"
  )

  # system2() does not preserve spaces inside -c code unless the argument is quoted explicitly.
  status <- suppressWarnings(system2(
    python_bin,
    args = c("-c", shQuote(check_code)),
    stdout = stdout_file,
    stderr = stderr_file
  ))
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  list(
    ok = is.numeric(status) && length(status) == 1 && !is.na(status) && status == 0,
    status = status,
    stdout = stdout_text,
    stderr = stderr_text
  )
}

article_lab_python_setup_message <- function(python_bin) {
  python_label <- shQuote(python_bin)
  paste0(
    "Article Lab API scoring is using Python interpreter ", python_label, ". ",
    "Install the required package(s) into that interpreter with: ",
    python_label, " -m pip install openai",
    ". If you want the app to use a different interpreter or virtualenv, set ARTICLE_LAB_PYTHON before starting the Shiny app."
  )
}

article_lab_top_title_examples <- function(con, limit = 8L) {
  objects <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type IN ('table', 'view')")
  if (!("v_medium_title_prediction_dataset_v2" %in% objects$name)) return(character())

  query <- sprintf("
    SELECT title
    FROM v_medium_title_prediction_dataset_v2
    WHERE NULLIF(TRIM(title), '') IS NOT NULL
      AND success_score IS NOT NULL
    ORDER BY COALESCE(CAST(top_20_percent AS INTEGER), 0) DESC, success_score DESC
    LIMIT %s
  ", as.integer(limit))
  rows <- dbGetQuery(con, query)
  titles <- clean_text(rows$title)
  unique(titles[!is.na(titles)])
}

article_lab_api_request <- function(prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_, example_titles = character(), manual_prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_titles.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_titles.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)

  request_payload <- list(
    prompt = article_lab_input_string(prompt) %||% article_lab_default_prompt,
    manual_prompt = article_lab_input_multiline(manual_prompt),
    batch_size = as.integer(batch_size),
    seed_topic = article_lab_input_string(seed_topic),
    inspiration_source = article_lab_input_string(inspiration_source),
    model = article_lab_input_string(model) %||% article_lab_default_model,
    example_titles = unname(example_titles)
  )

  request_file <- tempfile(pattern = "article_lab_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2(
    "node",
    args = c(helper_path, request_file),
    stdout = stdout_file,
    stderr = stderr_file
  )
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Title generation helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Title generation helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  titles <- unlist(parsed$titles %||% list(), use.names = FALSE)
  titles <- clean_text(titles)
  titles <- unique(titles[!is.na(titles)])
  if (length(titles) == 0) stop("API helper returned no usable titles.", call. = FALSE)

  list(
    titles = data.frame(
      row_number = seq_along(titles),
      title = titles,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    mode = article_lab_input_string(parsed$mode) %||% "api",
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    raw_json = stdout_text,
    example_titles_used = as.integer(length(example_titles)),
    response_id = article_lab_input_string(parsed$response_id),
    retry_used = isTRUE(parsed$retry_used),
    dropped_n = as.integer(parsed$dropped_count %||% 0L),
    dropped_titles = unname(unlist(parsed$dropped_titles %||% list(), use.names = FALSE))
  )
}

generate_title_candidates <- function(con, prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_, manual_prompt = NA_character_) {
  inspiration_value <- article_lab_input_string(inspiration_source)
  example_titles <- if (identical(inspiration_value, "top performing titles")) article_lab_top_title_examples(con, limit = 8L) else character()

  tryCatch({
    api_result <- article_lab_api_request(
      prompt = prompt,
      batch_size = batch_size,
      seed_topic = seed_topic,
      inspiration_source = inspiration_source,
      model = model,
      example_titles = example_titles,
      manual_prompt = manual_prompt
    )
    api_result$fallback_reason <- NULL
    api_result$validated <- article_lab_validate_titles(api_result$titles$title, max_chars = article_lab_title_max_chars)
    api_result$titles <- data.frame(
      row_number = seq_along(api_result$validated$titles),
      title = api_result$validated$titles,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (is.null(api_result$dropped_n) || is.na(api_result$dropped_n)) api_result$dropped_n <- api_result$validated$dropped_n
    api_result
  }, error = function(e) {
    stub_rows <- stub_title_candidates(
      prompt = prompt,
      batch_size = batch_size,
      seed_topic = seed_topic,
      inspiration_source = inspiration_source,
      model = model
    )
    list(
      titles = stub_rows,
      mode = "stub",
      model = article_lab_input_string(model) %||% article_lab_default_model,
      raw_json = NULL,
      example_titles_used = as.integer(length(example_titles)),
      response_id = NULL,
      fallback_reason = conditionMessage(e),
      dropped_n = 0L
    )
  })
}

article_lab_score_id <- function(candidate_id) {
  paste0("als_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", gsub("[^A-Za-z0-9]+", "_", candidate_id))
}

article_lab_score_system_prompt <- paste(
  "You score the reader-facing pre-click appeal of Medium finance titles.",
  "Use only the supplied title. Do not infer or use claps, responses, rank, age, publication performance, or observation history.",
  "Do not estimate click potential. Return calibrated JSON scores from 1 to 5."
)

article_lab_score_user_prompt <- function(prompt_version, scope, title) {
  prompt_version <- article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version
  scope <- article_lab_input_string(scope) %||% article_lab_default_score_scope
  title <- article_lab_input_string(title) %||% ""
  title_json <- toJSON(list(title = title), auto_unbox = TRUE, pretty = TRUE)

  if (identical(prompt_version, "v2_3")) {
    return(paste0(
      "Prompt version: ", prompt_version, "\n\n",
      "Score scope: ", scope, "\n",
      "You are scoring only the title. Do not infer a subtitle. Treat missing context as missing.\n\n",
      "Important measurement note:\n",
      "Do not estimate click potential. For competitor Medium articles, we do not have impressions, views, reads, or click-through data, so click potential is not directly testable in this dataset.\n\n",
      "Focus instead on outcomes that can be compared against observed public metrics:\n",
      "- medium_comment_potential: how likely this article is to receive written responses/comments, especially because it invites disagreement, personal stories, debate, strong opinions, corrections, or follow-up questions.\n",
      "- overall_article_potential: overall expected Medium performance based on the title only, considering likely reader interest, topic strength, emotional pull, trust, and engagement potential.\n\n",
      "Calibrate scores relative to typical Medium personal finance articles, not in isolation.\n\n",
      "Use the full 1-5 scale aggressively:\n",
      "1 = very weak, likely below average\n",
      "2 = below average or generic\n",
      "3 = average / okay for Medium finance\n",
      "4 = clearly above average, likely stronger than most articles\n",
      "5 = exceptional, rare, top-tier potential\n\n",
      "Most normal articles should receive 2 or 3.\n",
      "Do not give 4 unless the title has a clearly strong hook, strong topic demand, meaningful emotional or discussion pull, and a clear reader payoff.\n",
      "Do not give 5 unless the title looks unusually compelling and would plausibly belong among the strongest articles in the dataset.\n",
      "Avoid defaulting to 4 for merely competent, useful, or credible articles.\n\n",
      "Input fields, and no other article data:\n",
      title_json, "\n\n",
      "Rubric:\n",
      "- curiosity: How much the title creates a genuine desire to know more.\n",
      "- emotional_pull: How much the title creates emotional interest, concern, excitement, surprise, or urgency.\n",
      "- medium_comment_potential: Estimate how likely the article is to generate written Medium responses/comments. Higher scores should go to title wording that invites disagreement, debate, personal experiences, corrections, strong opinions, or nuanced discussion. A useful but straightforward article can have high clap potential but low comment potential. Use the full scale.\n",
      "- overall_article_potential: Estimate overall Medium performance potential from the title only. This should be a relative ranking judgment, not a quality compliment. Consider topic demand, emotional stakes, trust, likely engagement, and whether the title feels meaningfully differentiated from generic finance content. Use 5 sparingly for likely top-decile potential.\n",
      "- trust_risk: Risk that the title feels exaggerated, misleading, too clickbaity, or credibility-damaging. Higher means more risk. A title can create curiosity or emotion while still carrying trust risk.\n\n",
      "predicted_success_bucket:\n",
      "- low = likely below median or weak relative to typical Medium finance articles.\n",
      "- medium = around median to moderately above average.\n",
      "- high = likely top 20 percent potential. Use high sparingly. Do not classify most articles as high.\n\n",
      "Return JSON matching the schema exactly. short_reason must be one short sentence."
    ))
  }

  paste0(
    "Prompt version: ", prompt_version, "\n\n",
    "Score scope: ", scope, "\n",
    "You are scoring only the title. Do not infer a subtitle. Treat missing context as missing.\n\n",
    "Input fields, and no other article data:\n",
    title_json, "\n\n",
    "Return JSON matching the schema exactly."
  )
}

article_lab_score_api_request <- function(candidates, model = NA_character_, prompt_version = NA_character_, scope = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "score_article_lab_titles.py")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/score_article_lab_titles.py", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(candidates) == 0) return(list(scores = data.frame(), errors = list()))
  python_bin <- article_lab_resolve_python()

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_score_model,
    prompt_version = article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
    scope = article_lab_input_string(scope) %||% article_lab_default_score_scope,
    candidates = unname(lapply(seq_len(nrow(candidates)), function(i) {
      list(
        candidate_id = candidates$candidate_id[[i]],
        batch_id = candidates$batch_id[[i]],
        title = candidates$title[[i]],
        title_char_count = suppressWarnings(as.integer(candidates$title_char_count[[i]])),
        title_length_flag = candidates$title_length_flag[[i]]
      )
    }))
  )

  request_file <- tempfile(pattern = "article_lab_score_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_score_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_score_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2(
    python_bin,
    args = c(helper_path, request_file),
    stdout = stdout_file,
    stderr = stderr_file
  )
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    failure_text <- clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Article Lab scoring helper failed."
    if (grepl("Missing Python package", failure_text, fixed = TRUE) || grepl("No module named 'openai'", failure_text, fixed = TRUE)) {
      failure_text <- paste(failure_text, article_lab_python_setup_message(python_bin))
    }
    stop(failure_text, call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Article Lab scoring helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  raw_scores <- parsed$scores %||% list()
  raw_errors <- parsed$errors %||% list()
  if (!is.list(raw_scores)) raw_scores <- list()
  if (!is.list(raw_errors)) raw_errors <- list()

  score_rows <- lapply(raw_scores, function(entry) {
    row <- data.frame(
      candidate_id = article_lab_input_string(entry$candidate_id),
      batch_id = article_lab_input_string(entry$batch_id),
      scored_at = article_lab_input_string(entry$scored_at),
      model = article_lab_input_string(entry$model) %||% request_payload$model,
      prompt_version = article_lab_input_string(entry$prompt_version) %||% request_payload$prompt_version,
      scope = article_lab_input_string(entry$scope) %||% request_payload$scope,
      predicted_success_bucket = article_lab_input_string(entry$predicted_success_bucket),
      short_reason = article_lab_input_string(entry$short_reason),
      raw_json = if (is.null(entry$raw_json)) NA_character_ else toJSON(entry$raw_json, auto_unbox = TRUE, null = "null"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    for (field in article_lab_score_fields) {
      field_value <- entry[[field]]
      row[[field]] <- if (is.null(field_value) || length(field_value) == 0) {
        NA_real_
      } else {
        suppressWarnings(as.numeric(field_value[[1]]))
      }
    }
    row
  })
  score_frame <- if (length(score_rows) == 0) data.frame() else do.call(rbind, score_rows)
  if (nrow(score_frame) > 0) {
    for (field in article_lab_score_fields) score_frame[[field]] <- suppressWarnings(as.numeric(score_frame[[field]]))
  }

  list(
    scores = score_frame,
    errors = raw_errors,
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    prompt_version = article_lab_input_string(parsed$prompt_version) %||% request_payload$prompt_version,
    scope = article_lab_input_string(parsed$scope) %||% request_payload$scope,
    raw_json = stdout_text
  )
}

article_lab_subtitle_id <- function(candidate_id, index = 1L) {
  paste0(
    "alsub_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    gsub("[^A-Za-z0-9]+", "_", candidate_id),
    "_",
    sprintf("%02d", suppressWarnings(as.integer(index)) %||% 1L),
    "_",
    sample.int(99999L, 1)
  )
}

article_lab_normalize_subtitle <- function(values, max_chars = article_lab_subtitle_max_chars) {
  unique_values <- unique(clean_text(values))
  unique_values <- unique_values[!is.na(unique_values)]
  char_counts <- nchar(enc2utf8(unique_values), type = "chars", allowNA = TRUE, keepNA = TRUE)
  unique_values[!is.na(char_counts) & char_counts <= max_chars]
}

article_lab_manual_subtitle_choice_map <- function(target_rows, pending_rows) {
  target_rows <- if (is.null(target_rows)) data.frame() else target_rows
  pending_rows <- if (is.null(pending_rows)) data.frame() else pending_rows

  target_titles <- if (nrow(target_rows) > 0) {
    target_rows[, intersect(c("candidate_id", "batch_id", "title", "created_at"), names(target_rows)), drop = FALSE]
  } else {
    data.frame()
  }
  pending_titles <- if (nrow(pending_rows) > 0) {
    pending_rows[, intersect(c("candidate_id", "batch_id", "title", "created_at"), names(pending_rows)), drop = FALSE]
  } else {
    data.frame()
  }
  rows <- unique(rbind(target_titles, pending_titles))
  if (nrow(rows) == 0) return(character())

  rows$title <- clean_text(rows$title)
  rows$candidate_id <- clean_text(rows$candidate_id)
  rows$batch_id <- clean_text(rows$batch_id)
  rows$created_at <- clean_text(rows$created_at)
  rows <- rows[!is.na(rows$candidate_id) & nzchar(rows$candidate_id) & !is.na(rows$title) & nzchar(rows$title), , drop = FALSE]
  if (nrow(rows) == 0) return(character())

  duplicate_title <- ave(rows$title, rows$title, FUN = length) > 1L
  labels <- rows$title
  if (any(duplicate_title)) {
    labels[duplicate_title] <- paste0(
      rows$title[duplicate_title],
      " (",
      substr(rows$candidate_id[duplicate_title], 1L, 12L),
      ")"
    )
  }
  choices <- as.list(rows$candidate_id)
  names(choices) <- labels
  choices
}

article_lab_thumbnail_id <- function(subtitle_id, index = 1L) {
  paste0(
    "alth_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    gsub("[^A-Za-z0-9]+", "_", subtitle_id),
    "_",
    sprintf("%02d", suppressWarnings(as.integer(index)) %||% 1L),
    "_",
    sample.int(99999L, 1)
  )
}

article_lab_xml_escape <- function(text) {
  value <- enc2utf8(as.character(text %||% ""))
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub("\"", "&quot;", value, fixed = TRUE)
  value <- gsub("'", "&apos;", value, fixed = TRUE)
  value
}

article_lab_thumbnail_text_lines <- function(text, width = 22L, max_lines = 3L) {
  value <- article_lab_input_string(text) %||% ""
  if (!nzchar(value)) return(rep("", max_lines))
  wrapped <- strwrap(value, width = max(10L, suppressWarnings(as.integer(width)) %||% 22L))
  wrapped <- wrapped[seq_len(min(length(wrapped), max_lines))]
  if (length(wrapped) < max_lines) wrapped <- c(wrapped, rep("", max_lines - length(wrapped)))
  wrapped
}

article_lab_thumbnail_data_uri <- function(title, subtitle, label, variant_index = 1L) {
  variant_index <- suppressWarnings(as.integer(variant_index))
  if (is.na(variant_index) || variant_index < 1L) variant_index <- 1L
  palettes <- list(
    list(bg1 = "#f3efe3", bg2 = "#e6dcc0", accent = "#1d5c4d", accent2 = "#183a36", text = "#1c1d21", chip = "#ffffff"),
    list(bg1 = "#eef4f7", bg2 = "#d6e7ee", accent = "#205b7a", accent2 = "#163b50", text = "#17202a", chip = "#ffffff"),
    list(bg1 = "#f6eee8", bg2 = "#eed7ca", accent = "#b24f30", accent2 = "#6f2f1e", text = "#211c19", chip = "#fffaf5"),
    list(bg1 = "#eef6ee", bg2 = "#d7ebd6", accent = "#2d6d47", accent2 = "#18402a", text = "#172117", chip = "#ffffff")
  )
  palette <- palettes[[((variant_index - 1L) %% length(palettes)) + 1L]]
  title_lines <- article_lab_thumbnail_text_lines(title, width = 19L, max_lines = 3L)
  subtitle_lines <- article_lab_thumbnail_text_lines(subtitle, width = 28L, max_lines = 2L)
  kicker <- article_lab_xml_escape(label %||% paste("Concept", variant_index))

  svg <- paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' width='1200' height='720' viewBox='0 0 1200 720'>",
    "<defs><linearGradient id='bg' x1='0%' y1='0%' x2='100%' y2='100%'>",
    "<stop offset='0%' stop-color='", palette$bg1, "'/>",
    "<stop offset='100%' stop-color='", palette$bg2, "'/></linearGradient></defs>",
    "<rect width='1200' height='720' rx='44' fill='url(#bg)'/>",
    "<circle cx='1010' cy='112' r='180' fill='", palette$accent, "' opacity='0.15'/>",
    "<rect x='70' y='78' width='160' height='40' rx='18' fill='", palette$chip, "' opacity='0.92'/>",
    "<text x='95' y='104' font-family='Georgia, serif' font-size='24' font-weight='700' fill='", palette$accent2, "'>Medium-style</text>",
    "<rect x='72' y='156' width='500' height='410' rx='38' fill='#ffffff' opacity='0.95'/>",
    "<rect x='72' y='156' width='500' height='14' fill='", palette$accent, "' opacity='0.92'/>",
    "<text x='112' y='248' font-family='Georgia, serif' font-size='56' font-weight='700' fill='", palette$text, "'>", article_lab_xml_escape(title_lines[[1]]), "</text>",
    "<text x='112' y='318' font-family='Georgia, serif' font-size='56' font-weight='700' fill='", palette$text, "'>", article_lab_xml_escape(title_lines[[2]]), "</text>",
    "<text x='112' y='388' font-family='Georgia, serif' font-size='56' font-weight='700' fill='", palette$text, "'>", article_lab_xml_escape(title_lines[[3]]), "</text>",
    "<text x='112' y='468' font-family='Helvetica, Arial, sans-serif' font-size='26' fill='", palette$accent2, "' opacity='0.88'>", article_lab_xml_escape(subtitle_lines[[1]]), "</text>",
    "<text x='112' y='505' font-family='Helvetica, Arial, sans-serif' font-size='26' fill='", palette$accent2, "' opacity='0.88'>", article_lab_xml_escape(subtitle_lines[[2]]), "</text>",
    "<rect x='640' y='108' width='490' height='504' rx='40' fill='", palette$accent, "'/>",
    "<rect x='684' y='156' width='402' height='122' rx='30' fill='", palette$chip, "' opacity='0.95'/>",
    "<text x='724' y='230' font-family='Helvetica, Arial, sans-serif' font-size='40' font-weight='700' fill='", palette$accent2, "'>", kicker, "</text>",
    "<rect x='700' y='324' width='338' height='30' rx='15' fill='#ffffff' opacity='0.92'/>",
    "<rect x='700' y='374' width='278' height='30' rx='15' fill='#ffffff' opacity='0.72'/>",
    "<rect x='700' y='424' width='360' height='30' rx='15' fill='#ffffff' opacity='0.5'/>",
    "<circle cx='976' cy='544' r='84' fill='", palette$accent2, "' opacity='0.2'/>",
    "<text x='698' y='540' font-family='Helvetica, Arial, sans-serif' font-size='28' fill='#ffffff'>Clear finance thumbnail concept</text>",
    "<text x='698' y='580' font-family='Helvetica, Arial, sans-serif' font-size='28' fill='#ffffff'>designed for title + subtitle pairing</text>",
    "</svg>"
  )

  paste0("data:image/svg+xml;charset=UTF-8,", utils::URLencode(svg, reserved = TRUE))
}

stub_thumbnail_candidates_for_package <- function(title, subtitle, prompt = NA_character_, count = article_lab_default_thumbnail_variants) {
  count <- suppressWarnings(as.integer(count))
  if (is.na(count) || count < 1L) count <- article_lab_default_thumbnail_variants
  count <- min(count, 4L)
  labels <- c(
    "Stat-led hero",
    "Calm editorial graphic",
    "Decision-path visual",
    "Human habit concept"
  )
  data.frame(
    thumbnail_label = labels[seq_len(count)],
    thumbnail_data_uri = vapply(seq_len(count), function(i) {
      article_lab_thumbnail_data_uri(title, subtitle, labels[[i]], variant_index = i)
    }, character(1)),
    created_at = rep(now_utc(), count),
    generation_mode = rep("stub", count),
    raw_json = rep(
      toJSON(
        list(
          prompt = article_lab_input_string(prompt) %||% article_lab_default_thumbnail_prompt,
          mode = "stub",
          title = article_lab_input_string(title),
          subtitle = article_lab_input_string(subtitle)
        ),
        auto_unbox = TRUE,
        null = "null"
      ),
      count
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

article_lab_thumbnail_api_request <- function(packages, variants_per_package = article_lab_default_thumbnail_variants, model = NA_character_, prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_thumbnails.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_thumbnails.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(packages) == 0) return(list(rows = data.frame(), model = article_lab_default_thumbnail_model, mode = "api", raw_json = NULL))

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_thumbnail_model,
    prompt = article_lab_input_string(prompt) %||% article_lab_default_thumbnail_prompt,
    variants_per_package = max(1L, min(4L, suppressWarnings(as.integer(variants_per_package)) %||% article_lab_default_thumbnail_variants)),
    packages = unname(lapply(seq_len(nrow(packages)), function(i) {
      list(
        subtitle_id = packages$subtitle_id[[i]],
        candidate_id = packages$candidate_id[[i]],
        batch_id = packages$batch_id[[i]],
        title = packages$title[[i]],
        subtitle = packages$subtitle[[i]]
      )
    }))
  )

  request_file <- tempfile(pattern = "article_lab_thumbnail_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_thumbnail_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_thumbnail_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2(
    "node",
    args = c(helper_path, request_file),
    stdout = stdout_file,
    stderr = stderr_file
  )
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Thumbnail generation helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Thumbnail generation helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  result_rows <- lapply(parsed$results %||% list(), function(entry) {
    thumbnails <- entry$thumbnails %||% list()
    if (length(thumbnails) == 0) return(NULL)
    rows <- lapply(thumbnails, function(thumbnail) {
      data.frame(
        subtitle_id = article_lab_input_string(entry$subtitle_id),
        candidate_id = article_lab_input_string(entry$candidate_id),
        batch_id = article_lab_input_string(entry$batch_id),
        title = article_lab_input_string(entry$title),
        subtitle = article_lab_input_string(entry$subtitle),
        thumbnail_label = article_lab_input_string(thumbnail$thumbnail_label) %||% "API concept",
        thumbnail_data_uri = article_lab_input_string(thumbnail$thumbnail_data_uri),
        created_at = article_lab_input_string(thumbnail$created_at) %||% now_utc(),
        model = article_lab_input_string(thumbnail$model) %||% article_lab_input_string(parsed$model) %||% request_payload$model,
        generation_mode = article_lab_input_string(thumbnail$generation_mode) %||% "api",
        raw_json = if (is.null(thumbnail$raw_json)) stdout_text else toJSON(thumbnail$raw_json, auto_unbox = TRUE, null = "null"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    do.call(rbind, rows)
  })
  result_rows <- Filter(Negate(is.null), result_rows)

  list(
    rows = if (length(result_rows) == 0) data.frame() else do.call(rbind, result_rows),
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    mode = article_lab_input_string(parsed$mode) %||% "api",
    raw_json = stdout_text
  )
}

generate_thumbnail_candidates <- function(packages, variants_per_package = article_lab_default_thumbnail_variants, model = NA_character_, prompt = NA_character_) {
  tryCatch(
    article_lab_thumbnail_api_request(packages, variants_per_package = variants_per_package, model = model, prompt = prompt),
    error = function(e) {
      rows <- lapply(seq_len(nrow(packages)), function(i) {
        variants <- stub_thumbnail_candidates_for_package(
          title = packages$title[[i]],
          subtitle = packages$subtitle[[i]],
          prompt = prompt,
          count = variants_per_package
        )
        if (nrow(variants) == 0) return(NULL)
        data.frame(
          subtitle_id = rep(packages$subtitle_id[[i]], nrow(variants)),
          candidate_id = rep(packages$candidate_id[[i]], nrow(variants)),
          batch_id = rep(packages$batch_id[[i]], nrow(variants)),
          title = rep(packages$title[[i]], nrow(variants)),
          subtitle = rep(packages$subtitle[[i]], nrow(variants)),
          thumbnail_label = variants$thumbnail_label,
          thumbnail_data_uri = variants$thumbnail_data_uri,
          created_at = variants$created_at,
          model = rep(article_lab_input_string(model) %||% article_lab_default_thumbnail_model, nrow(variants)),
          generation_mode = variants$generation_mode,
          raw_json = variants$raw_json,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      list(
        rows = if (length(rows) == 0) data.frame() else do.call(rbind, rows),
        model = article_lab_input_string(model) %||% article_lab_default_thumbnail_model,
        mode = "stub",
        raw_json = article_lab_input_string(prompt) %||% article_lab_default_thumbnail_prompt,
        fallback_reason = conditionMessage(e)
      )
    }
  )
}

stub_outline_for_package <- function(title, subtitle, thumbnail_label = NA_character_) {
  title <- article_lab_input_string(title) %||% "Working title"
  subtitle <- article_lab_input_string(subtitle) %||% "Working subtitle"
  thumbnail_label <- article_lab_input_string(thumbnail_label) %||% "approved thumbnail"
  paste(
    "# Outline",
    "",
    paste0("## Working title: ", title),
    paste0("Subtitle: ", subtitle),
    paste0("Thumbnail angle: ", thumbnail_label),
    "",
    "## Hook",
    "- Open with the reader problem or tension the title promises to resolve.",
    "- Make the stakes concrete without overstating the evidence.",
    "",
    "## Main sections",
    "1. Frame the core mistake or question.",
    "2. Explain the mechanism in plain language.",
    "3. Show the practical tradeoffs for an everyday investor.",
    "4. Give a simple decision framework or checklist.",
    "5. Address caveats, uncertainty, and cases where the advice may not apply.",
    "",
    "## Close",
    "- End with a measured takeaway and one practical next step.",
    sep = "\n"
  )
}

article_lab_outline_api_request <- function(packages, model = NA_character_, prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_outlines.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_outlines.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(packages) == 0) return(list(rows = data.frame(), model = article_lab_default_outline_model, mode = "api", raw_json = NULL))

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_outline_model,
    prompt = article_lab_input_multiline(prompt) %||% article_lab_default_outline_prompt,
    packages = unname(lapply(seq_len(nrow(packages)), function(i) {
      list(
        thumbnail_id = packages$thumbnail_id[[i]],
        subtitle_id = packages$subtitle_id[[i]],
        candidate_id = packages$candidate_id[[i]],
        batch_id = packages$batch_id[[i]],
        title = packages$title[[i]],
        subtitle = packages$subtitle[[i]],
        thumbnail_label = packages$thumbnail_label[[i]]
      )
    }))
  )

  request_file <- tempfile(pattern = "article_lab_outline_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_outline_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_outline_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2("node", args = c(helper_path, request_file), stdout = stdout_file, stderr = stderr_file)
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Outline generation helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Outline generation helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  result_rows <- lapply(parsed$results %||% list(), function(entry) {
    data.frame(
      thumbnail_id = article_lab_input_string(entry$thumbnail_id),
      subtitle_id = article_lab_input_string(entry$subtitle_id),
      candidate_id = article_lab_input_string(entry$candidate_id),
      batch_id = article_lab_input_string(entry$batch_id),
      outline_text = article_lab_input_multiline(entry$outline_text),
      created_at = now_utc(),
      model = article_lab_input_string(parsed$model) %||% request_payload$model,
      generation_mode = "api",
      raw_json = stdout_text,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  result_rows <- Filter(function(row) nrow(row) > 0 && !is.na(row$outline_text[[1]]), result_rows)
  list(
    rows = if (length(result_rows) == 0) data.frame() else do.call(rbind, result_rows),
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    mode = article_lab_input_string(parsed$mode) %||% "api",
    raw_json = stdout_text
  )
}

generate_outline_drafts <- function(packages, model = NA_character_, prompt = NA_character_) {
  tryCatch(
    article_lab_outline_api_request(packages, model = model, prompt = prompt),
    error = function(e) {
      rows <- lapply(seq_len(nrow(packages)), function(i) {
        data.frame(
          thumbnail_id = packages$thumbnail_id[[i]],
          subtitle_id = packages$subtitle_id[[i]],
          candidate_id = packages$candidate_id[[i]],
          batch_id = packages$batch_id[[i]],
          outline_text = stub_outline_for_package(packages$title[[i]], packages$subtitle[[i]], packages$thumbnail_label[[i]]),
          created_at = now_utc(),
          model = article_lab_input_string(model) %||% article_lab_default_outline_model,
          generation_mode = "stub",
          raw_json = toJSON(list(prompt = article_lab_input_multiline(prompt), fallback_reason = conditionMessage(e)), auto_unbox = TRUE, null = "null"),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })
      list(rows = do.call(rbind, rows), model = article_lab_input_string(model) %||% article_lab_default_outline_model, mode = "stub", fallback_reason = conditionMessage(e))
    }
  )
}

stub_subtitle_candidates_for_title <- function(title, count = 4L) {
  base_title <- article_lab_input_string(title) %||% "this article"
  count <- suppressWarnings(as.integer(count))
  if (is.na(count) || count < 1L) count <- 4L
  count <- min(count, 8L)

  lead_ins <- c(
    "A calmer look at what actually works",
    "A practical breakdown without hype",
    "What the evidence suggests for beginners",
    "A realistic guide for long-term investors",
    "Clear, credible takeaways you can use"
  )
  angles <- c(
    "before your next financial decision",
    "if you want progress without prediction",
    "for steadier investing habits",
    "without turning finance into a full-time job",
    "with fewer mistakes and less noise"
  )

  seed_key <- sum(utf8ToInt(base_title)) %% .Machine$integer.max
  set.seed(seed_key)
  subtitles <- vapply(seq_len(count), function(i) {
    if (i %% 2L == 1L) {
      paste(sample(lead_ins, 1), sample(angles, 1))
    } else {
      paste("For", sub(":.*$", "", base_title), sample(angles, 1))
    }
  }, character(1))
  article_lab_normalize_subtitle(subtitles)
}

article_lab_subtitle_api_request <- function(candidates, variants_per_title = 4L, model = NA_character_, prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_subtitles.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_subtitles.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(candidates) == 0) return(list(rows = data.frame(), model = article_lab_default_subtitle_model, mode = "api", raw_json = NULL))

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_subtitle_model,
    prompt = article_lab_input_string(prompt) %||% article_lab_default_subtitle_prompt,
    variants_per_title = max(1L, min(8L, suppressWarnings(as.integer(variants_per_title)) %||% 4L)),
    candidates = unname(lapply(seq_len(nrow(candidates)), function(i) {
      article_summary <- if ("article_summary" %in% names(candidates)) article_lab_input_multiline(candidates$article_summary[[i]]) else NA_character_
      list(
        candidate_id = candidates$candidate_id[[i]],
        batch_id = candidates$batch_id[[i]],
        title = candidates$title[[i]],
        article_summary = article_summary
      )
    }))
  )

  request_file <- tempfile(pattern = "article_lab_subtitle_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_subtitle_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_subtitle_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2(
    "node",
    args = c(helper_path, request_file),
    stdout = stdout_file,
    stderr = stderr_file
  )
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Subtitle generation helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Subtitle generation helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  result_rows <- lapply(parsed$results %||% list(), function(entry) {
    subtitles <- article_lab_normalize_subtitle(unname(unlist(entry$subtitles %||% list(), use.names = FALSE)))
    if (length(subtitles) == 0) return(NULL)
    data.frame(
      candidate_id = rep(article_lab_input_string(entry$candidate_id), length(subtitles)),
      batch_id = rep(article_lab_input_string(entry$batch_id), length(subtitles)),
      subtitle = subtitles,
      created_at = rep(article_lab_input_string(entry$created_at) %||% now_utc(), length(subtitles)),
      model = rep(article_lab_input_string(entry$model) %||% request_payload$model, length(subtitles)),
      generation_mode = rep(article_lab_input_string(entry$generation_mode) %||% "api", length(subtitles)),
      raw_json = rep(if (is.null(entry$raw_json)) stdout_text else toJSON(entry$raw_json, auto_unbox = TRUE, null = "null"), length(subtitles)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  result_rows <- Filter(Negate(is.null), result_rows)

  list(
    rows = if (length(result_rows) == 0) data.frame() else do.call(rbind, result_rows),
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    mode = article_lab_input_string(parsed$mode) %||% "api",
    raw_json = stdout_text
  )
}

generate_subtitle_candidates <- function(candidates, variants_per_title = 4L, model = NA_character_, prompt = NA_character_) {
  tryCatch(
    article_lab_subtitle_api_request(candidates, variants_per_title = variants_per_title, model = model, prompt = prompt),
    error = function(e) {
      rows <- lapply(seq_len(nrow(candidates)), function(i) {
        subtitles <- stub_subtitle_candidates_for_title(candidates$title[[i]], count = variants_per_title)
        if (length(subtitles) == 0) return(NULL)
        data.frame(
          candidate_id = rep(candidates$candidate_id[[i]], length(subtitles)),
          batch_id = rep(candidates$batch_id[[i]], length(subtitles)),
          subtitle = subtitles,
          created_at = rep(now_utc(), length(subtitles)),
          model = rep(article_lab_input_string(model) %||% article_lab_default_subtitle_model, length(subtitles)),
          generation_mode = rep("stub", length(subtitles)),
          raw_json = rep(
            toJSON(
              list(
                prompt = article_lab_input_string(prompt) %||% article_lab_default_subtitle_prompt,
                mode = "stub"
              ),
              auto_unbox = TRUE,
              null = "null"
            ),
            length(subtitles)
          ),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      list(
        rows = if (length(rows) == 0) data.frame() else do.call(rbind, rows),
        model = article_lab_input_string(model) %||% article_lab_default_subtitle_model,
        mode = "stub",
        raw_json = article_lab_input_string(prompt) %||% article_lab_default_subtitle_prompt,
        fallback_reason = conditionMessage(e)
      )
    }
  )
}

load_article_lab_subtitle_targets <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      COALESCE(s.generated_n, 0) AS generated_subtitle_n,
      COALESCE(s.approved_n, 0) AS approved_subtitle_n,
      COALESCE(s.rejected_n, 0) AS rejected_subtitle_n
    FROM article_lab_title_candidates c
    LEFT JOIN (
      SELECT
        candidate_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_subtitle_candidates
      GROUP BY candidate_id
    ) s
      ON s.candidate_id = c.candidate_id
    WHERE c.archived = 0
    ORDER BY c.created_at DESC, c.candidate_id DESC
    " else "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      COALESCE(s.generated_n, 0) AS generated_subtitle_n,
      COALESCE(s.approved_n, 0) AS approved_subtitle_n,
      COALESCE(s.rejected_n, 0) AS rejected_subtitle_n
    FROM article_lab_title_candidates c
    LEFT JOIN (
      SELECT
        candidate_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_subtitle_candidates
      GROUP BY candidate_id
    ) s
      ON s.candidate_id = c.candidate_id
    WHERE c.batch_id = ?
      AND c.archived = 0
    ORDER BY c.created_at DESC, c.candidate_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

load_article_lab_subtitle_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_subtitle_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at,
      s.subtitle,
      s.status AS subtitle_status,
      s.notes,
      s.model,
      s.generation_mode,
      s.approved_at,
      s.rejected_at,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    " else "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at,
      s.subtitle,
      s.status AS subtitle_status,
      s.notes,
      s.model,
      s.generation_mode,
      s.approved_at,
      s.rejected_at,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    WHERE s.batch_id = ?
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

load_article_lab_ready_for_thumbnail_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_subtitle_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.title,
      c.status,
      c.notes,
      GROUP_CONCAT(s.subtitle, '\n') AS approved_subtitles,
      COUNT(*) AS approved_subtitle_n
    FROM article_lab_title_candidates c
    INNER JOIN article_lab_subtitle_candidates s
      ON s.candidate_id = c.candidate_id
     AND s.status = 'approved'
    WHERE c.archived = 0
      AND c.status = 'ready_for_thumbnail'
    GROUP BY c.candidate_id, c.batch_id, c.title, c.status, c.notes
    ORDER BY c.batch_id DESC, c.candidate_id DESC
    " else "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.title,
      c.status,
      c.notes,
      GROUP_CONCAT(s.subtitle, '\n') AS approved_subtitles,
      COUNT(*) AS approved_subtitle_n
    FROM article_lab_title_candidates c
    INNER JOIN article_lab_subtitle_candidates s
      ON s.candidate_id = c.candidate_id
     AND s.status = 'approved'
    WHERE c.archived = 0
      AND c.status = 'ready_for_thumbnail'
      AND c.batch_id = ?
    GROUP BY c.candidate_id, c.batch_id, c.title, c.status, c.notes
    ORDER BY c.batch_id DESC, c.candidate_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

load_article_lab_thumbnail_packages <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_subtitle_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at AS subtitle_created_at,
      s.subtitle,
      s.notes,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      COALESCE(t.generated_n, 0) AS generated_thumbnail_n,
      COALESCE(t.approved_n, 0) AS approved_thumbnail_n,
      COALESCE(t.rejected_n, 0) AS rejected_thumbnail_n
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    LEFT JOIN (
      SELECT
        subtitle_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_thumbnail_candidates
      GROUP BY subtitle_id
    ) t
      ON t.subtitle_id = s.subtitle_id
    WHERE s.status = 'approved'
      AND c.archived = 0
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    " else "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at AS subtitle_created_at,
      s.subtitle,
      s.notes,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      COALESCE(t.generated_n, 0) AS generated_thumbnail_n,
      COALESCE(t.approved_n, 0) AS approved_thumbnail_n,
      COALESCE(t.rejected_n, 0) AS rejected_thumbnail_n
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    LEFT JOIN (
      SELECT
        subtitle_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_thumbnail_candidates
      GROUP BY subtitle_id
    ) t
      ON t.subtitle_id = s.subtitle_id
    WHERE s.status = 'approved'
      AND c.archived = 0
      AND s.batch_id = ?
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    "
  rows <- if (all_batches) dbGetQuery(con, query) else dbGetQuery(con, query, params = list(batch_id))
  if (nrow(rows) == 0) return(rows)
  rows <- article_lab_normalize_candidate_rows(rows)
  rows[rows$approved_thumbnail_n <= 0 & rows$generated_thumbnail_n <= 0, , drop = FALSE]
}

load_article_lab_thumbnail_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_thumbnail_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.status AS thumbnail_status,
      t.notes,
      t.model,
      t.generation_mode,
      t.approved_at,
      t.rejected_at,
      s.subtitle,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    " else "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.status AS thumbnail_status,
      t.notes,
      t.model,
      t.generation_mode,
      t.approved_at,
      t.rejected_at,
      s.subtitle,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    WHERE t.batch_id = ?
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    "
  rows <- if (all_batches) dbGetQuery(con, query) else dbGetQuery(con, query, params = list(batch_id))
  if (nrow(rows) == 0) return(rows)
  approved_packages <- unique(rows$subtitle_id[rows$thumbnail_status == "approved"])
  rows[rows$thumbnail_status == "generated" & !(rows$subtitle_id %in% approved_packages), , drop = FALSE]
}

load_article_lab_ready_for_outline_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_thumbnail_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.notes,
      s.subtitle,
      c.title,
      c.status,
      o.outline_id,
      o.outline_text,
      o.status AS outline_status,
      o.notes AS outline_notes,
      o.model AS outline_model,
      o.generation_mode AS outline_generation_mode,
      o.updated_at AS outline_updated_at,
      o.approved_at AS outline_approved_at
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    LEFT JOIN article_lab_outlines o
      ON o.thumbnail_id = t.thumbnail_id
     AND o.status IN ('draft', 'approved')
    WHERE t.status = 'approved'
      AND c.archived = 0
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    " else "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.notes,
      s.subtitle,
      c.title,
      c.status,
      o.outline_id,
      o.outline_text,
      o.status AS outline_status,
      o.notes AS outline_notes,
      o.model AS outline_model,
      o.generation_mode AS outline_generation_mode,
      o.updated_at AS outline_updated_at,
      o.approved_at AS outline_approved_at
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    LEFT JOIN article_lab_outlines o
      ON o.thumbnail_id = t.thumbnail_id
     AND o.status IN ('draft', 'approved')
    WHERE t.status = 'approved'
      AND c.archived = 0
      AND t.batch_id = ?
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

article_lab_update_subtitle_notes <- function(con, notes_updates) {
  if (length(notes_updates) == 0) return(0L)
  updated_n <- 0L
  dbBegin(con)
  tryCatch({
    for (entry in notes_updates) {
      subtitle_id <- clean_text(entry$subtitle_id)
      if (length(subtitle_id) == 0 || is.na(subtitle_id[[1]])) next
      dbExecute(
        con,
        "UPDATE article_lab_subtitle_candidates SET notes = ? WHERE subtitle_id = ?",
        params = list(clean_text(entry$notes), subtitle_id[[1]])
      )
      updated_n <- updated_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  updated_n
}

article_lab_update_thumbnail_notes <- function(con, notes_updates) {
  if (length(notes_updates) == 0) return(0L)
  updated_n <- 0L
  dbBegin(con)
  tryCatch({
    for (entry in notes_updates) {
      thumbnail_id <- clean_text(entry$thumbnail_id)
      if (length(thumbnail_id) == 0 || is.na(thumbnail_id[[1]])) next
      dbExecute(
        con,
        "UPDATE article_lab_thumbnail_candidates SET notes = ? WHERE thumbnail_id = ?",
        params = list(clean_text(entry$notes), thumbnail_id[[1]])
      )
      updated_n <- updated_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  updated_n
}

article_lab_sync_title_subtitle_stage <- function(con, candidate_ids) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) return(invisible(NULL))

  for (candidate_id in candidate_ids) {
    approved_n <- dbGetQuery(
      con,
      "SELECT COUNT(*) AS approved_n FROM article_lab_subtitle_candidates WHERE candidate_id = ? AND status = 'approved'",
      params = list(candidate_id)
    )$approved_n[[1]] %||% 0L
    if (approved_n > 0) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = 'ready_for_thumbnail', promoted = 0, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?",
        params = list(candidate_id)
      )
    } else {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = 'approved_for_subtitle', promoted = 1, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?",
        params = list(candidate_id)
      )
    }
  }
  invisible(NULL)
}

article_lab_sync_title_thumbnail_stage <- function(con, candidate_ids) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) return(invisible(NULL))

  for (candidate_id in candidate_ids) {
    approved_thumbnail_n <- if (dbExistsTable(con, "article_lab_thumbnail_candidates")) {
      dbGetQuery(
        con,
        "SELECT COUNT(*) AS approved_n FROM article_lab_thumbnail_candidates WHERE candidate_id = ? AND status = 'approved'",
        params = list(candidate_id)
      )$approved_n[[1]] %||% 0L
    } else {
      0L
    }
    approved_subtitle_n <- dbGetQuery(
      con,
      "SELECT COUNT(*) AS approved_n FROM article_lab_subtitle_candidates WHERE candidate_id = ? AND status = 'approved'",
      params = list(candidate_id)
    )$approved_n[[1]] %||% 0L

    if (approved_thumbnail_n > 0) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = 'ready_for_outline', promoted = 0, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?",
        params = list(candidate_id)
      )
    } else if (approved_subtitle_n > 0) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = 'ready_for_thumbnail', promoted = 0, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?",
        params = list(candidate_id)
      )
    } else {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = 'approved_for_subtitle', promoted = 1, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?",
        params = list(candidate_id)
      )
    }
  }
  invisible(NULL)
}

article_lab_insert_outline_drafts <- function(con, outline_rows) {
  if (nrow(outline_rows) == 0) return(0L)
  inserted_n <- 0L
  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(outline_rows))) {
      existing <- dbGetQuery(
        con,
        "SELECT outline_id FROM article_lab_outlines WHERE thumbnail_id = ? AND status IN ('draft', 'approved') LIMIT 1",
        params = list(outline_rows$thumbnail_id[[i]])
      )
      if (nrow(existing) > 0) next
      timestamp <- outline_rows$created_at[[i]] %||% now_utc()
      dbExecute(
        con,
        "INSERT INTO article_lab_outlines
         (outline_id, thumbnail_id, subtitle_id, candidate_id, batch_id, created_at, updated_at, outline_text, status, notes, model, generation_mode, raw_json, approved_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'draft', NULL, ?, ?, ?, NULL)",
        params = list(
          article_lab_outline_id(outline_rows$thumbnail_id[[i]]),
          outline_rows$thumbnail_id[[i]], outline_rows$subtitle_id[[i]], outline_rows$candidate_id[[i]], outline_rows$batch_id[[i]],
          timestamp, timestamp, outline_rows$outline_text[[i]], outline_rows$model[[i]], outline_rows$generation_mode[[i]], outline_rows$raw_json[[i]]
        )
      )
      inserted_n <- inserted_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  inserted_n
}

article_lab_update_outlines <- function(con, outline_updates) {
  if (length(outline_updates) == 0) return(0L)
  updated_n <- 0L
  timestamp <- now_utc()
  dbBegin(con)
  tryCatch({
    for (entry in outline_updates) {
      outline_id <- clean_text(entry$outline_id)
      if (length(outline_id) == 0 || is.na(outline_id[[1]])) next
      outline_text <- article_lab_input_multiline(entry$outline_text)
      if (is.na(outline_text)) next
      dbExecute(
        con,
        "UPDATE article_lab_outlines SET outline_text = ?, notes = ?, updated_at = ? WHERE outline_id = ? AND status = 'draft'",
        params = list(outline_text, clean_text(entry$notes), timestamp, outline_id[[1]])
      )
      updated_n <- updated_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  updated_n
}

article_lab_approve_outlines <- function(con, outline_ids) {
  outline_ids <- clean_text(outline_ids)
  outline_ids <- unique(outline_ids[!is.na(outline_ids)])
  if (length(outline_ids) == 0) return(list(approved_n = 0L, candidate_ids = character()))
  placeholders <- paste(rep("?", length(outline_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT outline_id, candidate_id, batch_id FROM article_lab_outlines WHERE outline_id IN (%s) AND status = 'draft'", placeholders),
    params = as.list(outline_ids)
  )
  if (nrow(rows) == 0) return(list(approved_n = 0L, candidate_ids = character()))
  timestamp <- now_utc()
  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      sprintf("UPDATE article_lab_outlines SET status = 'approved', approved_at = ?, updated_at = ? WHERE outline_id IN (%s)", paste(rep("?", nrow(rows)), collapse = ", ")),
      params = c(list(timestamp, timestamp), as.list(rows$outline_id))
    )
    dbExecute(
      con,
      sprintf("UPDATE article_lab_title_candidates SET status = 'ready_for_draft', promoted = 0, ready_for_human_rating = 0, archived = 0 WHERE candidate_id IN (%s)", paste(rep("?", length(unique(rows$candidate_id))), collapse = ", ")),
      params = as.list(unique(rows$candidate_id))
    )
    for (batch_id in unique(rows$batch_id)) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  list(approved_n = nrow(rows), candidate_ids = unique(rows$candidate_id))
}

article_lab_generate_subtitles_for_titles <- function(con, candidate_ids, model = NA_character_, prompt = NA_character_, variants_per_title = 4L) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) {
    return(list(generated_n = 0L, title_n = 0L, skipped_n = 0L, batch_ids = character(), mode = "none", model = article_lab_default_subtitle_model))
  }

  placeholders <- paste(rep("?", length(candidate_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT c.candidate_id, c.batch_id, c.title, c.status, c.ready_for_human_rating, c.promoted, c.archived,
              COALESCE(s.generated_n, 0) AS generated_subtitle_n, COALESCE(s.approved_n, 0) AS approved_subtitle_n
       FROM article_lab_title_candidates c
       LEFT JOIN (
         SELECT candidate_id,
                COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
                COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n
         FROM article_lab_subtitle_candidates
         GROUP BY candidate_id
       ) s
         ON s.candidate_id = c.candidate_id
       WHERE c.candidate_id IN (%s)",
      placeholders
    ),
    params = as.list(candidate_ids)
  )
  if (nrow(rows) == 0) {
    return(list(generated_n = 0L, title_n = 0L, skipped_n = length(candidate_ids), batch_ids = character(), mode = "none", model = article_lab_default_subtitle_model))
  }
  rows <- article_lab_normalize_candidate_rows(rows)
  eligible <- rows[
    rows$normalized_status == "approved_for_subtitle" &
      rows$generated_subtitle_n <= 0 &
      rows$approved_subtitle_n <= 0,
    c("candidate_id", "batch_id", "title"),
    drop = FALSE
  ]
  skipped_n <- length(candidate_ids) - nrow(eligible)
  if (nrow(eligible) == 0) {
    return(list(generated_n = 0L, title_n = 0L, skipped_n = skipped_n, batch_ids = unique(rows$batch_id), mode = "none", model = article_lab_default_subtitle_model))
  }

  summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(eligible$batch_id))
  eligible$article_summary <- NA_character_
  if (nrow(summary_contexts) > 0) {
    matched_summary <- summary_contexts$article_summary[match(eligible$batch_id, summary_contexts$batch_id)]
    eligible$article_summary <- matched_summary
  }

  existing_rows <- dbGetQuery(
    con,
    sprintf("SELECT candidate_id, subtitle FROM article_lab_subtitle_candidates WHERE candidate_id IN (%s)", paste(rep("?", nrow(eligible)), collapse = ", ")),
    params = as.list(eligible$candidate_id)
  )
  generated <- generate_subtitle_candidates(eligible, variants_per_title = variants_per_title, model = model, prompt = prompt)
  subtitle_rows <- generated$rows
  if (nrow(subtitle_rows) == 0) {
    return(list(generated_n = 0L, title_n = 0L, skipped_n = skipped_n + nrow(eligible), batch_ids = unique(rows$batch_id), mode = generated$mode %||% "none", model = generated$model %||% article_lab_default_subtitle_model, fallback_reason = generated$fallback_reason %||% NULL))
  }

  if (nrow(existing_rows) > 0) {
    existing_keys <- paste(existing_rows$candidate_id, tolower(existing_rows$subtitle))
    subtitle_rows <- subtitle_rows[!(paste(subtitle_rows$candidate_id, tolower(subtitle_rows$subtitle)) %in% existing_keys), , drop = FALSE]
  }
  if (nrow(subtitle_rows) == 0) {
    return(list(generated_n = 0L, title_n = 0L, skipped_n = skipped_n + nrow(eligible), batch_ids = unique(rows$batch_id), mode = generated$mode %||% "none", model = generated$model %||% article_lab_default_subtitle_model, fallback_reason = generated$fallback_reason %||% NULL))
  }

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(subtitle_rows))) {
      row <- subtitle_rows[i, , drop = FALSE]
      dbExecute(
        con,
        "
        INSERT INTO article_lab_subtitle_candidates
        (subtitle_id, candidate_id, batch_id, created_at, subtitle, status, notes, model, generation_mode, raw_json, approved_at, rejected_at)
        VALUES (?, ?, ?, ?, ?, 'generated', NULL, ?, ?, ?, NULL, NULL)
        ",
        params = list(
          article_lab_subtitle_id(row$candidate_id[[1]], i),
          row$candidate_id[[1]],
          row$batch_id[[1]],
          row$created_at[[1]] %||% now_utc(),
          row$subtitle[[1]],
          row$model[[1]] %||% article_lab_default_subtitle_model,
          row$generation_mode[[1]] %||% generated$mode %||% "generated",
          row$raw_json[[1]]
        )
      )
    }
    for (batch_id in unique(subtitle_rows$batch_id)) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(
    generated_n = nrow(subtitle_rows),
    title_n = length(unique(subtitle_rows$candidate_id)),
    skipped_n = skipped_n,
    batch_ids = unique(subtitle_rows$batch_id),
    mode = generated$mode %||% "generated",
    model = generated$model %||% article_lab_default_subtitle_model,
    fallback_reason = generated$fallback_reason %||% NULL
  )
}

article_lab_add_manual_subtitles <- function(con, candidate_id, subtitles) {
  candidate_id <- article_lab_input_string(candidate_id)
  subtitles <- article_lab_normalize_subtitle(unlist(strsplit(paste(subtitles, collapse = "\n"), "\n", fixed = TRUE)))
  if (is.na(candidate_id) || !nzchar(candidate_id) || length(subtitles) == 0) {
    return(list(added_n = 0L, skipped_n = 0L, duplicate_n = 0L, batch_id = NA_character_, title = NA_character_))
  }

  candidate_row <- dbGetQuery(
    con,
    "SELECT candidate_id, batch_id, title, status, ready_for_human_rating, promoted, archived
     FROM article_lab_title_candidates
     WHERE candidate_id = ?",
    params = list(candidate_id)
  )
  if (nrow(candidate_row) == 0) {
    return(list(added_n = 0L, skipped_n = length(subtitles), duplicate_n = 0L, batch_id = NA_character_, title = NA_character_))
  }
  candidate_row <- article_lab_normalize_candidate_rows(candidate_row)
  if (!(candidate_row$normalized_status[[1]] %in% c("approved_for_subtitle", "ready_for_thumbnail")) || isTRUE(candidate_row$archived[[1]] == 1)) {
    return(list(
      added_n = 0L,
      skipped_n = length(subtitles),
      duplicate_n = 0L,
      batch_id = candidate_row$batch_id[[1]] %||% NA_character_,
      title = candidate_row$title[[1]] %||% NA_character_
    ))
  }

  existing_rows <- dbGetQuery(
    con,
    "SELECT subtitle FROM article_lab_subtitle_candidates WHERE candidate_id = ?",
    params = list(candidate_id)
  )
  existing_keys <- if (nrow(existing_rows) > 0) tolower(clean_text(existing_rows$subtitle)) else character()
  subtitle_keys <- tolower(subtitles)
  keep_idx <- !(subtitle_keys %in% existing_keys)
  duplicate_n <- sum(!keep_idx)
  subtitles <- subtitles[keep_idx]
  if (length(subtitles) == 0) {
    return(list(
      added_n = 0L,
      skipped_n = 0L,
      duplicate_n = duplicate_n,
      batch_id = candidate_row$batch_id[[1]] %||% NA_character_,
      title = candidate_row$title[[1]] %||% NA_character_
    ))
  }

  created_at <- now_utc()
  dbBegin(con)
  tryCatch({
    for (i in seq_along(subtitles)) {
      dbExecute(
        con,
        "
        INSERT INTO article_lab_subtitle_candidates
        (subtitle_id, candidate_id, batch_id, created_at, subtitle, status, notes, model, generation_mode, raw_json, approved_at, rejected_at)
        VALUES (?, ?, ?, ?, ?, 'generated', NULL, NULL, 'manual', NULL, NULL, NULL)
        ",
        params = list(
          article_lab_subtitle_id(candidate_id, i),
          candidate_id,
          candidate_row$batch_id[[1]],
          created_at,
          subtitles[[i]]
        )
      )
    }
    article_lab_update_batch_status(con, candidate_row$batch_id[[1]])
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(
    added_n = length(subtitles),
    skipped_n = 0L,
    duplicate_n = duplicate_n,
    batch_id = candidate_row$batch_id[[1]] %||% NA_character_,
    title = candidate_row$title[[1]] %||% NA_character_
  )
}

article_lab_approve_subtitles <- function(con, subtitle_ids) {
  subtitle_ids <- clean_text(subtitle_ids)
  subtitle_ids <- unique(subtitle_ids[!is.na(subtitle_ids)])
  if (length(subtitle_ids) == 0) return(list(approved_n = 0L, skipped_n = 0L, candidate_ids = character(), batch_ids = character()))

  placeholders <- paste(rep("?", length(subtitle_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT subtitle_id, candidate_id, batch_id, status FROM article_lab_subtitle_candidates WHERE subtitle_id IN (%s)", placeholders),
    params = as.list(subtitle_ids)
  )
  if (nrow(rows) == 0) return(list(approved_n = 0L, skipped_n = length(subtitle_ids), candidate_ids = character(), batch_ids = character()))
  eligible_ids <- rows$subtitle_id[rows$status == "generated"]
  skipped_n <- length(subtitle_ids) - length(eligible_ids)
  candidate_ids <- unique(rows$candidate_id[rows$subtitle_id %in% eligible_ids])
  batch_ids <- unique(rows$batch_id[rows$subtitle_id %in% eligible_ids])

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf("UPDATE article_lab_subtitle_candidates SET status = 'approved', approved_at = ?, rejected_at = NULL WHERE subtitle_id IN (%s)", paste(rep("?", length(eligible_ids)), collapse = ", ")),
        params = c(list(now_utc()), as.list(eligible_ids))
      )
      article_lab_sync_title_subtitle_stage(con, candidate_ids)
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(approved_n = length(eligible_ids), skipped_n = skipped_n, candidate_ids = candidate_ids, batch_ids = batch_ids)
}

article_lab_reject_subtitles <- function(con, subtitle_ids) {
  subtitle_ids <- clean_text(subtitle_ids)
  subtitle_ids <- unique(subtitle_ids[!is.na(subtitle_ids)])
  if (length(subtitle_ids) == 0) return(list(rejected_n = 0L, skipped_n = 0L, candidate_ids = character(), batch_ids = character()))

  placeholders <- paste(rep("?", length(subtitle_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT subtitle_id, candidate_id, batch_id, status FROM article_lab_subtitle_candidates WHERE subtitle_id IN (%s)", placeholders),
    params = as.list(subtitle_ids)
  )
  if (nrow(rows) == 0) return(list(rejected_n = 0L, skipped_n = length(subtitle_ids), candidate_ids = character(), batch_ids = character()))
  eligible_ids <- rows$subtitle_id[rows$status == "generated"]
  skipped_n <- length(subtitle_ids) - length(eligible_ids)
  candidate_ids <- unique(rows$candidate_id[rows$subtitle_id %in% eligible_ids])
  batch_ids <- unique(rows$batch_id[rows$subtitle_id %in% eligible_ids])

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf("UPDATE article_lab_subtitle_candidates SET status = 'rejected', rejected_at = ? WHERE subtitle_id IN (%s)", paste(rep("?", length(eligible_ids)), collapse = ", ")),
        params = c(list(now_utc()), as.list(eligible_ids))
      )
      article_lab_sync_title_subtitle_stage(con, candidate_ids)
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(rejected_n = length(eligible_ids), skipped_n = skipped_n, candidate_ids = candidate_ids, batch_ids = batch_ids)
}

article_lab_dismiss_thumbnail_packages <- function(con, subtitle_ids) {
  subtitle_ids <- clean_text(subtitle_ids)
  subtitle_ids <- unique(subtitle_ids[!is.na(subtitle_ids)])
  if (length(subtitle_ids) == 0) return(list(dismissed_n = 0L, skipped_n = 0L, candidate_ids = character(), batch_ids = character()))

  placeholders <- paste(rep("?", length(subtitle_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT
         s.subtitle_id,
         s.candidate_id,
         s.batch_id,
         s.status,
         COALESCE(t.generated_n, 0) AS generated_thumbnail_n,
         COALESCE(t.approved_n, 0) AS approved_thumbnail_n
       FROM article_lab_subtitle_candidates s
       LEFT JOIN (
         SELECT
           subtitle_id,
           COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
           COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n
         FROM article_lab_thumbnail_candidates
         GROUP BY subtitle_id
       ) t
         ON t.subtitle_id = s.subtitle_id
       WHERE s.subtitle_id IN (%s)",
      placeholders
    ),
    params = as.list(subtitle_ids)
  )
  if (nrow(rows) == 0) return(list(dismissed_n = 0L, skipped_n = length(subtitle_ids), candidate_ids = character(), batch_ids = character()))

  eligible_rows <- rows[
    rows$status == "approved" &
      rows$generated_thumbnail_n <= 0 &
      rows$approved_thumbnail_n <= 0,
    ,
    drop = FALSE
  ]
  skipped_n <- length(subtitle_ids) - nrow(eligible_rows)
  candidate_ids <- unique(eligible_rows$candidate_id)
  batch_ids <- unique(eligible_rows$batch_id)

  dbBegin(con)
  tryCatch({
    if (nrow(eligible_rows) > 0) {
      dbExecute(
        con,
        sprintf("UPDATE article_lab_subtitle_candidates SET status = 'rejected', rejected_at = ?, approved_at = NULL WHERE subtitle_id IN (%s)", paste(rep("?", nrow(eligible_rows)), collapse = ", ")),
        params = c(list(now_utc()), as.list(eligible_rows$subtitle_id))
      )
      article_lab_sync_title_subtitle_stage(con, candidate_ids)
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(dismissed_n = nrow(eligible_rows), skipped_n = skipped_n, candidate_ids = candidate_ids, batch_ids = batch_ids)
}

article_lab_generate_thumbnails_for_packages <- function(con, subtitle_ids, model = NA_character_, prompt = NA_character_, variants_per_package = article_lab_default_thumbnail_variants) {
  subtitle_ids <- clean_text(subtitle_ids)
  subtitle_ids <- unique(subtitle_ids[!is.na(subtitle_ids)])
  if (length(subtitle_ids) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = 0L, batch_ids = character(), mode = "none", model = article_lab_default_thumbnail_model))
  }

  placeholders <- paste(rep("?", length(subtitle_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT
         s.subtitle_id,
         s.candidate_id,
         s.batch_id,
         s.subtitle,
         s.status AS subtitle_status,
         c.title,
         c.status,
         c.ready_for_human_rating,
         c.promoted,
         c.archived,
         COALESCE(t.generated_n, 0) AS generated_thumbnail_n,
         COALESCE(t.approved_n, 0) AS approved_thumbnail_n
       FROM article_lab_subtitle_candidates s
       INNER JOIN article_lab_title_candidates c
         ON c.candidate_id = s.candidate_id
       LEFT JOIN (
         SELECT
           subtitle_id,
           COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
           COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n
         FROM article_lab_thumbnail_candidates
         GROUP BY subtitle_id
       ) t
         ON t.subtitle_id = s.subtitle_id
       WHERE s.subtitle_id IN (%s)",
      placeholders
    ),
    params = as.list(subtitle_ids)
  )
  if (nrow(rows) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = length(subtitle_ids), batch_ids = character(), mode = "none", model = article_lab_default_thumbnail_model))
  }
  rows <- article_lab_normalize_candidate_rows(rows)
  eligible <- rows[
    rows$subtitle_status == "approved" &
      rows$generated_thumbnail_n <= 0 &
      rows$approved_thumbnail_n <= 0,
    c("subtitle_id", "candidate_id", "batch_id", "title", "subtitle"),
    drop = FALSE
  ]
  skipped_n <- length(subtitle_ids) - nrow(eligible)
  if (nrow(eligible) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = skipped_n, batch_ids = unique(rows$batch_id), mode = "none", model = article_lab_default_thumbnail_model))
  }

  existing_rows <- if (dbExistsTable(con, "article_lab_thumbnail_candidates")) {
    dbGetQuery(
      con,
      sprintf("SELECT subtitle_id, thumbnail_label FROM article_lab_thumbnail_candidates WHERE subtitle_id IN (%s)", paste(rep("?", nrow(eligible)), collapse = ", ")),
      params = as.list(eligible$subtitle_id)
    )
  } else {
    data.frame()
  }

  generated <- generate_thumbnail_candidates(eligible, variants_per_package = variants_per_package, model = model, prompt = prompt)
  thumbnail_rows <- generated$rows
  if (nrow(thumbnail_rows) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = skipped_n + nrow(eligible), batch_ids = unique(rows$batch_id), mode = generated$mode %||% "none", model = generated$model %||% article_lab_default_thumbnail_model))
  }

  if (nrow(existing_rows) > 0) {
    existing_keys <- paste(existing_rows$subtitle_id, tolower(existing_rows$thumbnail_label))
    thumbnail_rows <- thumbnail_rows[!(paste(thumbnail_rows$subtitle_id, tolower(thumbnail_rows$thumbnail_label)) %in% existing_keys), , drop = FALSE]
  }
  if (nrow(thumbnail_rows) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = skipped_n + nrow(eligible), batch_ids = unique(rows$batch_id), mode = generated$mode %||% "none", model = generated$model %||% article_lab_default_thumbnail_model))
  }

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(thumbnail_rows))) {
      row <- thumbnail_rows[i, , drop = FALSE]
      dbExecute(
        con,
        "
        INSERT INTO article_lab_thumbnail_candidates
        (thumbnail_id, subtitle_id, candidate_id, batch_id, created_at, thumbnail_label, thumbnail_data_uri, status, notes, model, generation_mode, raw_json, approved_at, rejected_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, 'generated', NULL, ?, ?, ?, NULL, NULL)
        ",
        params = list(
          article_lab_thumbnail_id(row$subtitle_id[[1]], i),
          row$subtitle_id[[1]],
          row$candidate_id[[1]],
          row$batch_id[[1]],
          row$created_at[[1]] %||% now_utc(),
          row$thumbnail_label[[1]],
          row$thumbnail_data_uri[[1]],
          row$model[[1]] %||% article_lab_default_thumbnail_model,
          row$generation_mode[[1]] %||% generated$mode %||% "generated",
          row$raw_json[[1]]
        )
      )
    }
    for (batch_id in unique(thumbnail_rows$batch_id)) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(
    generated_n = nrow(thumbnail_rows),
    package_n = length(unique(thumbnail_rows$subtitle_id)),
    skipped_n = skipped_n,
    batch_ids = unique(thumbnail_rows$batch_id),
    mode = generated$mode %||% "generated",
    model = generated$model %||% article_lab_default_thumbnail_model,
    fallback_reason = generated$fallback_reason %||% NULL
  )
}

article_lab_approve_thumbnails <- function(con, thumbnail_ids) {
  thumbnail_ids <- clean_text(thumbnail_ids)
  thumbnail_ids <- unique(thumbnail_ids[!is.na(thumbnail_ids)])
  if (length(thumbnail_ids) == 0) return(list(approved_n = 0L, skipped_n = 0L, candidate_ids = character(), batch_ids = character(), subtitle_ids = character(), duplicate_subtitle_ids = character(), message = NULL))

  placeholders <- paste(rep("?", length(thumbnail_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT thumbnail_id, subtitle_id, candidate_id, batch_id, status FROM article_lab_thumbnail_candidates WHERE thumbnail_id IN (%s)", placeholders),
    params = as.list(thumbnail_ids)
  )
  if (nrow(rows) == 0) return(list(approved_n = 0L, skipped_n = length(thumbnail_ids), candidate_ids = character(), batch_ids = character(), subtitle_ids = character(), duplicate_subtitle_ids = character(), message = NULL))
  eligible_rows <- rows[rows$status == "generated", , drop = FALSE]
  skipped_n <- length(thumbnail_ids) - nrow(eligible_rows)
  duplicate_counts <- table(eligible_rows$subtitle_id)
  duplicate_subtitle_ids <- names(duplicate_counts[duplicate_counts > 1L])
  if (length(duplicate_subtitle_ids) > 0) {
    return(list(
      approved_n = 0L,
      skipped_n = skipped_n,
      candidate_ids = character(),
      batch_ids = character(),
      subtitle_ids = character(),
      duplicate_subtitle_ids = duplicate_subtitle_ids,
      message = "Select only one thumbnail candidate per title/subtitle package before approving."
    ))
  }

  approved_packages <- if (nrow(eligible_rows) > 0) {
    dbGetQuery(
      con,
      sprintf("SELECT DISTINCT subtitle_id FROM article_lab_thumbnail_candidates WHERE status = 'approved' AND subtitle_id IN (%s)", paste(rep("?", nrow(eligible_rows)), collapse = ", ")),
      params = as.list(eligible_rows$subtitle_id)
    )
  } else {
    data.frame()
  }
  already_approved_ids <- clean_text(approved_packages$subtitle_id)
  if (length(already_approved_ids) > 0) {
    eligible_rows <- eligible_rows[!(eligible_rows$subtitle_id %in% already_approved_ids), , drop = FALSE]
    skipped_n <- length(thumbnail_ids) - nrow(eligible_rows)
  }
  if (nrow(eligible_rows) == 0) {
    return(list(approved_n = 0L, skipped_n = skipped_n, candidate_ids = character(), batch_ids = character(), subtitle_ids = character(), duplicate_subtitle_ids = character(), message = "No selected thumbnails were eligible for approval."))
  }

  eligible_ids <- eligible_rows$thumbnail_id
  candidate_ids <- unique(eligible_rows$candidate_id)
  batch_ids <- unique(eligible_rows$batch_id)
  subtitle_ids <- unique(eligible_rows$subtitle_id)

  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      sprintf("UPDATE article_lab_thumbnail_candidates SET status = 'approved', approved_at = ?, rejected_at = NULL WHERE thumbnail_id IN (%s)", paste(rep("?", length(eligible_ids)), collapse = ", ")),
      params = c(list(now_utc()), as.list(eligible_ids))
    )
    article_lab_sync_title_thumbnail_stage(con, candidate_ids)
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(approved_n = length(eligible_ids), skipped_n = skipped_n, candidate_ids = candidate_ids, batch_ids = batch_ids, subtitle_ids = subtitle_ids, duplicate_subtitle_ids = character(), message = NULL)
}

article_lab_reject_thumbnails <- function(con, thumbnail_ids) {
  thumbnail_ids <- clean_text(thumbnail_ids)
  thumbnail_ids <- unique(thumbnail_ids[!is.na(thumbnail_ids)])
  if (length(thumbnail_ids) == 0) return(list(rejected_n = 0L, skipped_n = 0L, candidate_ids = character(), batch_ids = character()))

  placeholders <- paste(rep("?", length(thumbnail_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT thumbnail_id, subtitle_id, candidate_id, batch_id, status FROM article_lab_thumbnail_candidates WHERE thumbnail_id IN (%s)", placeholders),
    params = as.list(thumbnail_ids)
  )
  if (nrow(rows) == 0) return(list(rejected_n = 0L, skipped_n = length(thumbnail_ids), candidate_ids = character(), batch_ids = character()))
  eligible_ids <- rows$thumbnail_id[rows$status == "generated"]
  skipped_n <- length(thumbnail_ids) - length(eligible_ids)
  candidate_ids <- unique(rows$candidate_id[rows$thumbnail_id %in% eligible_ids])
  batch_ids <- unique(rows$batch_id[rows$thumbnail_id %in% eligible_ids])

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf("UPDATE article_lab_thumbnail_candidates SET status = 'rejected', rejected_at = ? WHERE thumbnail_id IN (%s)", paste(rep("?", length(eligible_ids)), collapse = ", ")),
        params = c(list(now_utc()), as.list(eligible_ids))
      )
      article_lab_sync_title_thumbnail_stage(con, candidate_ids)
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(rejected_n = length(eligible_ids), skipped_n = skipped_n, candidate_ids = candidate_ids, batch_ids = batch_ids)
}

article_lab_unscored_candidates <- function(con, batch_id, model, prompt_version, scope) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  dbGetQuery(
    con,
    if (all_batches) "
    SELECT c.candidate_id, c.batch_id, c.title, c.status, c.title_char_count, c.title_length_flag
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    WHERE c.archived = 0
      AND c.promoted = 0
      AND c.status = 'ready_for_api_scoring'
      AND s.candidate_id IS NULL
    ORDER BY c.created_at, c.candidate_id
    " else "
    SELECT c.candidate_id, c.batch_id, c.title, c.status, c.title_char_count, c.title_length_flag
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    WHERE c.batch_id = ?
      AND c.archived = 0
      AND c.promoted = 0
      AND c.status = 'ready_for_api_scoring'
      AND s.candidate_id IS NULL
    ORDER BY c.created_at, c.candidate_id
    ",
    params = if (all_batches) list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope
    ) else list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope,
      batch_id
    )
  )
}

article_lab_update_batch_status <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id)) return(invisible(NULL))
  status_rows <- dbGetQuery(
    con,
    "
    SELECT
      COALESCE(SUM(CASE WHEN status = 'ready_for_api_scoring' THEN 1 ELSE 0 END), 0) AS ready_n,
      COALESCE(SUM(CASE WHEN status = 'api_scored' THEN 1 ELSE 0 END), 0) AS scored_n,
      COALESCE(SUM(CASE WHEN status = 'approved_for_subtitle' OR promoted = 1 THEN 1 ELSE 0 END), 0) AS approved_n,
      COALESCE(SUM(CASE WHEN status = 'ready_for_thumbnail' THEN 1 ELSE 0 END), 0) AS subtitle_ready_n,
      COALESCE(SUM(CASE WHEN status = 'ready_for_outline' THEN 1 ELSE 0 END), 0) AS outline_ready_n,
      COALESCE(SUM(CASE WHEN status = 'ready_for_draft' THEN 1 ELSE 0 END), 0) AS draft_ready_n,
      COALESCE(SUM(CASE WHEN archived = 1 THEN 1 ELSE 0 END), 0) AS archived_n,
      COUNT(*) AS total_n
    FROM article_lab_title_candidates
    WHERE batch_id = ?
    ",
    params = list(batch_id)
  )
  if (nrow(status_rows) == 0) return(invisible(NULL))
  row <- status_rows[1, , drop = FALSE]
  batch_status <- if (row$draft_ready_n[[1]] > 0) {
    "ready_for_draft"
  } else if (row$outline_ready_n[[1]] > 0) {
    "ready_for_outline"
  } else if (row$subtitle_ready_n[[1]] > 0) {
    "ready_for_thumbnail"
  } else if (row$approved_n[[1]] > 0) {
    "approved_for_subtitle"
  } else if (row$ready_n[[1]] > 0) {
    "ready_for_api_scoring"
  } else if (row$scored_n[[1]] > 0) {
    "api_scored"
  } else if (row$archived_n[[1]] >= row$total_n[[1]] && row$total_n[[1]] > 0) {
    "archived"
  } else {
    "generated"
  }
  dbExecute(
    con,
    "UPDATE article_lab_title_batches SET status = ? WHERE batch_id = ?",
    params = list(batch_status, batch_id)
  )
  invisible(batch_status)
}

article_lab_recover_api_pending_candidates <- function(con, batch_id = NULL) {
  batch_filter <- article_lab_input_string(batch_id)
  where_sql <- "WHERE c.status = 'api_pending'"
  params <- list()
  if (!is.null(batch_filter) && !identical(batch_filter, article_lab_all_batches_value)) {
    where_sql <- paste(where_sql, "AND c.batch_id = ?")
    params <- list(batch_filter)
  }

  pending_query <- sprintf(
    "
    SELECT
      c.candidate_id,
      c.batch_id,
      CASE
        WHEN EXISTS (
          SELECT 1
          FROM article_lab_title_api_scores s
          WHERE s.candidate_id = c.candidate_id
          LIMIT 1
        ) THEN 'api_scored'
        ELSE 'ready_for_api_scoring'
      END AS recovered_status
    FROM article_lab_title_candidates c
    %s
    ",
    where_sql
  )
  pending_rows <- if (length(params) > 0) {
    dbGetQuery(con, pending_query, params = params)
  } else {
    dbGetQuery(con, pending_query)
  }
  if (nrow(pending_rows) == 0) return(invisible(0L))

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(pending_rows))) {
      row <- pending_rows[i, , drop = FALSE]
      dbExecute(
        con,
        "
        UPDATE article_lab_title_candidates
        SET status = ?,
            ready_for_human_rating = 0,
            promoted = CASE WHEN ? = 'api_scored' THEN promoted ELSE 0 END,
            archived = 0
        WHERE candidate_id = ?
        ",
        params = list(
          row$recovered_status[[1]],
          row$recovered_status[[1]],
          row$candidate_id[[1]]
        )
      )
    }
    for (current_batch_id in unique(pending_rows$batch_id)) {
      article_lab_update_batch_status(con, current_batch_id)
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  invisible(nrow(pending_rows))
}

article_lab_upsert_score <- function(con, score_row) {
  combined_score <- article_lab_combined_title_score(
    curiosity = score_row$curiosity[[1]],
    emotional_pull = score_row$emotional_pull[[1]],
    medium_comment_potential = score_row$medium_comment_potential[[1]],
    overall_article_potential = score_row$overall_article_potential[[1]],
    trust_risk = score_row$trust_risk[[1]],
    title_char_count = score_row$title_char_count[[1]]
  )
  dbExecute(
    con,
    "
    INSERT OR REPLACE INTO article_lab_title_api_scores
    (score_id, candidate_id, batch_id, scored_at, model, prompt_version, scope,
     clarity, curiosity, specificity, beginner_appeal, credibility, emotional_pull,
     promise_strength, trust_risk, medium_clap_potential, medium_comment_potential,
     overall_article_potential, combined_title_score, predicted_success_bucket,
     short_reason, raw_json, error)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      article_lab_score_id(score_row$candidate_id[[1]]),
      score_row$candidate_id[[1]],
      score_row$batch_id[[1]],
      score_row$scored_at[[1]] %||% now_utc(),
      score_row$model[[1]] %||% article_lab_default_score_model,
      score_row$prompt_version[[1]] %||% article_lab_default_score_prompt_version,
      score_row$scope[[1]] %||% article_lab_default_score_scope,
      score_row$clarity[[1]],
      score_row$curiosity[[1]],
      score_row$specificity[[1]],
      score_row$beginner_appeal[[1]],
      score_row$credibility[[1]],
      score_row$emotional_pull[[1]],
      score_row$promise_strength[[1]],
      score_row$trust_risk[[1]],
      score_row$medium_clap_potential[[1]],
      score_row$medium_comment_potential[[1]],
      score_row$overall_article_potential[[1]],
      combined_score,
      score_row$predicted_success_bucket[[1]],
      score_row$short_reason[[1]],
      score_row$raw_json[[1]],
      NA_character_
    )
  )
  combined_score
}

article_lab_score_batch <- function(con, batch_id, model, prompt_version, scope, candidate_ids = NULL) {
  article_lab_recover_api_pending_candidates(con, batch_id = batch_id)
  batch_label <- if (identical(batch_id, article_lab_all_batches_value)) "all titles" else paste("batch", batch_id)
  selected_ids <- clean_text(candidate_ids)
  selected_ids <- unique(selected_ids[!is.na(selected_ids)])
  if (length(selected_ids) == 0) {
    return(list(
      scored_n = 0L,
      used_existing_n = 0L,
      failed_n = 0L,
      failed_ids = character(),
      batch_label = batch_label,
      message = "Select at least one API-queue title to score."
    ))
  }

  candidates <- load_article_lab_scoring_rows(con, batch_id, model, prompt_version, scope)
  if (nrow(candidates) == 0) {
    return(list(
      scored_n = 0L,
      used_existing_n = 0L,
      failed_n = 0L,
      failed_ids = character(),
      batch_label = batch_label,
      message = sprintf("No titles in %s are available for API scoring.", batch_label)
    ))
  }
  candidates <- article_lab_normalize_candidate_rows(candidates)
  candidates <- candidates[candidates$candidate_id %in% selected_ids, , drop = FALSE]
  if (nrow(candidates) == 0) {
    return(list(
      scored_n = 0L,
      used_existing_n = 0L,
      failed_n = 0L,
      failed_ids = character(),
      batch_label = batch_label,
      message = "None of the selected titles were found in the current API queue selection."
    ))
  }

  eligible <- candidates[candidates$normalized_status == "ready_for_api_scoring", , drop = FALSE]
  skipped_n <- length(selected_ids) - nrow(eligible)
  if (nrow(eligible) == 0) {
    return(list(
      scored_n = 0L,
      used_existing_n = 0L,
      failed_n = 0L,
      failed_ids = character(),
      skipped_n = skipped_n,
      batch_label = batch_label,
      message = "Only titles in API queue can be scored."
    ))
  }

  cached_rows <- eligible[!is.na(eligible$score_id), , drop = FALSE]
  api_rows <- eligible[is.na(eligible$score_id), c("candidate_id", "batch_id", "title", "status", "title_char_count", "title_length_flag"), drop = FALSE]
  previous_status <- setNames(api_rows$status, api_rows$candidate_id)

  result <- list(
    scores = data.frame(),
    errors = list(),
    model = article_lab_input_string(model) %||% article_lab_default_score_model,
    prompt_version = article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
    scope = article_lab_input_string(scope) %||% article_lab_default_score_scope
  )

  if (nrow(api_rows) > 0) {
    placeholders <- paste(rep("?", nrow(api_rows)), collapse = ", ")
    dbExecute(
      con,
      sprintf("UPDATE article_lab_title_candidates SET status = 'api_pending' WHERE candidate_id IN (%s)", placeholders),
      params = as.list(api_rows$candidate_id)
    )

    result <- tryCatch(
      article_lab_score_api_request(api_rows, model = model, prompt_version = prompt_version, scope = scope),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      for (candidate_id in names(previous_status)) {
        dbExecute(
          con,
          "UPDATE article_lab_title_candidates SET status = ? WHERE candidate_id = ?",
          params = list(previous_status[[candidate_id]], candidate_id)
        )
      }
      stop(result)
    }
  }

  scored_ids <- character()
  failed_ids <- character()
  cached_ids <- cached_rows$candidate_id
  batch_ids_to_update <- unique(eligible$batch_id)

  dbBegin(con)
  tryCatch({
    if (length(cached_ids) > 0) {
      dbExecute(
        con,
        sprintf(
          "UPDATE article_lab_title_candidates
           SET status = 'api_scored', ready_for_human_rating = 0, promoted = 0, archived = 0
           WHERE candidate_id IN (%s)",
          paste(rep("?", length(cached_ids)), collapse = ", ")
        ),
        params = as.list(cached_ids)
      )
      scored_ids <- c(scored_ids, cached_ids)
    }

    if (nrow(result$scores) > 0) {
      for (i in seq_len(nrow(result$scores))) {
        score_row <- result$scores[i, , drop = FALSE]
        match_index <- match(score_row$candidate_id[[1]], eligible$candidate_id)
        score_row$title_char_count <- eligible$title_char_count[[match_index]]
        article_lab_upsert_score(con, score_row)
        dbExecute(
          con,
          "
          UPDATE article_lab_title_candidates
          SET status = CASE
            WHEN status = 'archived' OR archived = 1 THEN 'archived'
            WHEN status = 'approved_for_subtitle' OR promoted = 1 THEN 'approved_for_subtitle'
            ELSE 'api_scored'
          END,
              ready_for_human_rating = 0,
              archived = 0
          WHERE candidate_id = ?
          ",
          params = list(score_row$candidate_id[[1]])
        )
        scored_ids <- c(scored_ids, score_row$candidate_id[[1]])
      }
    }

    if (length(result$errors) > 0) {
      failed_ids <- vapply(result$errors, function(entry) article_lab_input_string(entry$candidate_id) %||% NA_character_, character(1))
      failed_ids <- failed_ids[!is.na(failed_ids)]
    }
    untouched_ids <- setdiff(api_rows$candidate_id, union(scored_ids, failed_ids))
    failed_ids <- unique(c(failed_ids, untouched_ids))

    for (candidate_id in failed_ids) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = ? WHERE candidate_id = ?",
        params = list(previous_status[[candidate_id]] %||% "generated", candidate_id)
      )
    }

    for (batch_id_value in batch_ids_to_update) {
      article_lab_update_batch_status(con, batch_id_value)
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(
    scored_n = length(scored_ids),
    used_existing_n = length(cached_ids),
    failed_n = length(failed_ids),
    failed_ids = failed_ids,
    skipped_n = skipped_n,
    model = result$model,
    prompt_version = result$prompt_version,
    scope = result$scope,
    batch_label = batch_label,
    message = if (length(scored_ids) == 0 && length(failed_ids) > 0) {
      sprintf("Scoring failed for %s title%s. No titles were updated.", length(failed_ids), ifelse(length(failed_ids) == 1, "", "s"))
    } else {
      NULL
    }
  )
}

article_lab_save_generate_triage <- function(con, updates) {
  if (length(updates) == 0) return(character())
  batch_ids <- character()
  dbBegin(con)
  tryCatch({
    for (update in updates) {
      status_value <- article_lab_normalize_candidate_status(update$status)
      if (!(status_value %in% c("generated", "disqualified"))) status_value <- "generated"
      dbExecute(
        con,
        "
        UPDATE article_lab_title_candidates
        SET status = ?,
            notes = ?,
            ready_for_human_rating = 0,
            promoted = 0,
            archived = 0
        WHERE candidate_id = ?
        ",
        params = list(status_value, clean_text(update$notes), update$candidate_id)
      )
      batch_row <- dbGetQuery(
        con,
        "SELECT batch_id FROM article_lab_title_candidates WHERE candidate_id = ? LIMIT 1",
        params = list(update$candidate_id)
      )
      if (nrow(batch_row) > 0) batch_ids <- c(batch_ids, batch_row$batch_id[[1]])
    }
    for (batch_id in unique(batch_ids)) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  unique(batch_ids)
}

article_lab_move_candidates_to_api_queue <- function(con, candidate_ids) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) return(list(moved_n = 0L, skipped_n = 0L, batch_ids = character()))

  placeholders <- paste(rep("?", length(candidate_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT candidate_id, batch_id, status, ready_for_human_rating, promoted, archived FROM article_lab_title_candidates WHERE candidate_id IN (%s)",
      placeholders
    ),
    params = as.list(candidate_ids)
  )
  if (nrow(rows) == 0) return(list(moved_n = 0L, skipped_n = length(candidate_ids), batch_ids = character()))
  rows <- article_lab_normalize_candidate_rows(rows)
  eligible_ids <- rows$candidate_id[rows$normalized_status == "generated"]
  skipped_n <- length(candidate_ids) - length(eligible_ids)
  batch_ids <- unique(rows$batch_id)

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf(
          "UPDATE article_lab_title_candidates SET status = 'ready_for_api_scoring', ready_for_human_rating = 0, promoted = 0, archived = 0 WHERE candidate_id IN (%s)",
          paste(rep("?", length(eligible_ids)), collapse = ", ")
        ),
        params = as.list(eligible_ids)
      )
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(moved_n = length(eligible_ids), skipped_n = skipped_n, batch_ids = batch_ids)
}

article_lab_approve_candidates_for_subtitle <- function(con, candidate_ids) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) return(list(approved_n = 0L, skipped_n = 0L, batch_ids = character()))

  placeholders <- paste(rep("?", length(candidate_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT candidate_id, batch_id, status, ready_for_human_rating, promoted, archived FROM article_lab_title_candidates WHERE candidate_id IN (%s)",
      placeholders
    ),
    params = as.list(candidate_ids)
  )
  if (nrow(rows) == 0) return(list(approved_n = 0L, skipped_n = length(candidate_ids), batch_ids = character()))
  rows <- article_lab_normalize_candidate_rows(rows)
  eligible_ids <- rows$candidate_id[rows$normalized_status == "api_scored"]
  skipped_n <- length(candidate_ids) - length(eligible_ids)
  batch_ids <- unique(rows$batch_id)

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf(
          "UPDATE article_lab_title_candidates SET status = 'approved_for_subtitle', promoted = 1, ready_for_human_rating = 0, archived = 0 WHERE candidate_id IN (%s)",
          paste(rep("?", length(eligible_ids)), collapse = ", ")
        ),
        params = as.list(eligible_ids)
      )
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(approved_n = length(eligible_ids), skipped_n = skipped_n, batch_ids = batch_ids)
}

article_lab_archive_api_scored_candidates <- function(con, candidate_ids) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) return(list(archived_n = 0L, skipped_n = 0L, batch_ids = character()))

  placeholders <- paste(rep("?", length(candidate_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT candidate_id, batch_id, status, ready_for_human_rating, promoted, archived FROM article_lab_title_candidates WHERE candidate_id IN (%s)",
      placeholders
    ),
    params = as.list(candidate_ids)
  )
  if (nrow(rows) == 0) return(list(archived_n = 0L, skipped_n = length(candidate_ids), batch_ids = character()))
  rows <- article_lab_normalize_candidate_rows(rows)
  eligible_ids <- rows$candidate_id[rows$normalized_status %in% c("ready_for_api_scoring", "api_scored", "approved_for_subtitle")]
  skipped_n <- length(candidate_ids) - length(eligible_ids)
  batch_ids <- unique(rows$batch_id)

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf(
          "UPDATE article_lab_title_candidates SET status = 'archived', promoted = 0, ready_for_human_rating = 0, archived = 1 WHERE candidate_id IN (%s)",
          paste(rep("?", length(eligible_ids)), collapse = ", ")
        ),
        params = as.list(eligible_ids)
      )
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(archived_n = length(eligible_ids), skipped_n = skipped_n, batch_ids = batch_ids)
}

save_article_lab_batch <- function(con, prompt, seed_topic, inspiration_source, requested_batch_size, model, titles, raw_json = NA_character_, generation_mode = "generated", enforce_max_chars = TRUE, notes_extra = NULL) {
  if (length(titles) == 0) return(invisible(NULL))
  validated <- article_lab_validate_titles(titles, max_chars = article_lab_title_max_chars)
  title_values <- validated$titles
  if (length(title_values) == 0) {
    stop(sprintf("No titles met the %s-character maximum.", article_lab_title_max_chars), call. = FALSE)
  }
  batch_id <- article_lab_batch_id()
  created_at <- now_utc()
  prompt_value <- clean_text(prompt)
  if (length(prompt_value) == 0 || is.na(prompt_value[[1]])) prompt_value <- article_lab_default_prompt
  seed_topic_value <- clean_text(seed_topic)
  inspiration_value <- clean_text(inspiration_source)
  model_value <- clean_text(model)
  if (length(model_value) == 0 || is.na(model_value[[1]])) model_value <- article_lab_default_model
  requested_size <- suppressWarnings(as.integer(requested_batch_size))
  if (is.na(requested_size) || requested_size < 1L) requested_size <- length(title_values)

  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      "INSERT INTO article_lab_title_batches
       (batch_id, created_at, prompt, seed_topic, inspiration_source, requested_batch_size, model, status, notes)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        batch_id,
        created_at,
        prompt_value[[1]],
        if (length(seed_topic_value) == 0) NA_character_ else seed_topic_value[[1]],
        if (length(inspiration_value) == 0) NA_character_ else inspiration_value[[1]],
        requested_size,
        model_value[[1]],
        "generated",
        paste(
          sprintf("Generation mode: %s.", generation_mode),
          "Article Lab candidates stay generated until manual triage moves selected titles into ready_for_api_scoring.",
          notes_extra %||% ""
        )
      )
    )

    for (i in seq_along(title_values)) {
      title_value <- clean_text(title_values[[i]])
      if (length(title_value) == 0 || is.na(title_value[[1]])) next
      title_char_count <- article_lab_title_length(title_value[[1]])
      title_length_flag <- article_lab_title_length_flag(title_char_count)
      dbExecute(
        con,
        "INSERT INTO article_lab_title_candidates
         (candidate_id, batch_id, created_at, title, title_char_count, title_length_flag, status, source, ready_for_human_rating, promoted, archived, notes, raw_json)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          article_lab_candidate_id(batch_id, i),
          batch_id,
          created_at,
          title_value[[1]],
          title_char_count,
          title_length_flag,
          "generated",
          "article_lab",
          0L,
          0L,
          0L,
          NA_character_,
          raw_json
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  batch_id
}

load_latest_article_lab_batch <- function(con) {
  if (!dbExistsTable(con, "article_lab_title_batches")) return(NULL)
  batch <- dbGetQuery(con, "
    SELECT batch_id, created_at, prompt, seed_topic, inspiration_source,
      requested_batch_size, model, status, notes
    FROM article_lab_title_batches
    ORDER BY created_at DESC, batch_id DESC
    LIMIT 1
  ")
  if (nrow(batch) == 0) NULL else batch[1, , drop = FALSE]
}

load_article_lab_candidates_for_batch <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame())
  }
  if (identical(batch_id, article_lab_all_batches_value)) {
    return(dbGetQuery(
      con,
      "SELECT candidate_id, title, title_char_count, title_length_flag, status, created_at, batch_id,
         ready_for_human_rating, archived, promoted, notes
       FROM article_lab_title_candidates
       ORDER BY created_at DESC, candidate_id DESC"
    ))
  }
  dbGetQuery(
    con,
    "SELECT candidate_id, title, title_char_count, title_length_flag, status, created_at, batch_id,
       ready_for_human_rating, archived, promoted, notes
     FROM article_lab_title_candidates
     WHERE batch_id = ?
     ORDER BY candidate_id",
    params = list(batch_id)
  )
}

load_article_lab_batches <- function(con) {
  if (!dbExistsTable(con, "article_lab_title_batches")) return(data.frame())
  dbGetQuery(con, "
    SELECT batch_id, created_at, requested_batch_size, model, status, seed_topic, inspiration_source
    FROM article_lab_title_batches
    ORDER BY created_at DESC, batch_id DESC
  ")
}

load_article_lab_scoring_rows <- function(con, batch_id, model, prompt_version, scope) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  rows <- dbGetQuery(
    con,
    if (all_batches) "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.title_char_count,
      c.title_length_flag,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      s.score_id,
      s.scored_at,
      s.model,
      s.prompt_version,
      s.scope,
      s.clarity,
      s.curiosity,
      s.specificity,
      s.beginner_appeal,
      s.credibility,
      s.emotional_pull,
      s.promise_strength,
      s.trust_risk,
      s.medium_clap_potential,
      s.medium_comment_potential,
      s.overall_article_potential,
      s.combined_title_score,
      s.predicted_success_bucket,
      s.short_reason,
      s.error
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    ORDER BY c.created_at DESC, c.candidate_id DESC
    " else "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.title_char_count,
      c.title_length_flag,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      s.score_id,
      s.scored_at,
      s.model,
      s.prompt_version,
      s.scope,
      s.clarity,
      s.curiosity,
      s.specificity,
      s.beginner_appeal,
      s.credibility,
      s.emotional_pull,
      s.promise_strength,
      s.trust_risk,
      s.medium_clap_potential,
      s.medium_comment_potential,
      s.overall_article_potential,
      s.combined_title_score,
      s.predicted_success_bucket,
      s.short_reason,
      s.error
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    WHERE c.batch_id = ?
    ORDER BY c.created_at DESC, c.candidate_id DESC
    ",
    params = if (all_batches) list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope
    ) else list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope,
      batch_id
    )
  )
  if (nrow(rows) == 0) return(rows)
  rows$title_char_count <- ifelse(
    is.na(rows$title_char_count),
    article_lab_title_length(rows$title),
    suppressWarnings(as.integer(rows$title_char_count))
  )
  rows$title_length_flag <- ifelse(
    is.na(rows$title_length_flag) |
      rows$title_length_flag == "risky" |
      (rows$title_length_flag == "too_long" & rows$title_char_count <= article_lab_title_max_chars),
    article_lab_title_length_flag(rows$title_char_count),
    rows$title_length_flag
  )
  if (!("combined_title_score" %in% names(rows))) rows$combined_title_score <- NA_real_
  missing_combined <- is.na(rows$combined_title_score) & !is.na(rows$curiosity) & !is.na(rows$emotional_pull) &
    !is.na(rows$medium_comment_potential) & !is.na(rows$overall_article_potential)
  rows$combined_title_score[missing_combined] <- article_lab_combined_title_score(
    curiosity = rows$curiosity[missing_combined],
    emotional_pull = rows$emotional_pull[missing_combined],
    medium_comment_potential = rows$medium_comment_potential[missing_combined],
    overall_article_potential = rows$overall_article_potential[missing_combined],
    trust_risk = rows$trust_risk[missing_combined],
    title_char_count = rows$title_char_count[missing_combined]
  )
  rows
}

article_lab_overview <- function(con) {
  if (!dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame(
      saved_batches = 0L,
      saved_candidates = 0L,
      generated = 0L,
      disqualified = 0L,
      ready_for_api_scoring = 0L,
      api_scored = 0L,
      approved_for_subtitle = 0L,
      ready_for_thumbnail = 0L,
      ready_for_outline = 0L,
      ready_for_draft = 0L,
      rejected = 0L,
      archived = 0L
    ))
  }

  dbGetQuery(con, "
    SELECT
      (SELECT COUNT(*) FROM article_lab_title_batches) AS saved_batches,
      COUNT(*) AS saved_candidates,
      COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated,
      COALESCE(SUM(CASE WHEN status = 'disqualified' THEN 1 ELSE 0 END), 0) AS disqualified,
      COALESCE(SUM(CASE WHEN status = 'ready_for_api_scoring' THEN 1 ELSE 0 END), 0) AS ready_for_api_scoring,
      COALESCE(SUM(CASE WHEN status = 'api_scored' THEN 1 ELSE 0 END), 0) AS api_scored,
      COALESCE(SUM(CASE WHEN status = 'approved_for_subtitle' OR promoted = 1 THEN 1 ELSE 0 END), 0) AS approved_for_subtitle,
      COALESCE(SUM(CASE WHEN status = 'ready_for_thumbnail' THEN 1 ELSE 0 END), 0) AS ready_for_thumbnail,
      COALESCE(SUM(CASE WHEN status = 'ready_for_outline' THEN 1 ELSE 0 END), 0) AS ready_for_outline,
      COALESCE(SUM(CASE WHEN status = 'ready_for_draft' THEN 1 ELSE 0 END), 0) AS ready_for_draft,
      COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected,
      COALESCE(SUM(CASE WHEN archived = 1 THEN 1 ELSE 0 END), 0) AS archived
    FROM article_lab_title_candidates
  ")
}

article_lab_update_candidate_notes <- function(con, notes_updates) {
  if (length(notes_updates) == 0) return(0L)
  updated_n <- 0L
  dbBegin(con)
  tryCatch({
    for (entry in notes_updates) {
      candidate_id <- clean_text(entry$candidate_id)
      if (length(candidate_id) == 0 || is.na(candidate_id[[1]])) next
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET notes = ? WHERE candidate_id = ?",
        params = list(clean_text(entry$notes), candidate_id[[1]])
      )
      updated_n <- updated_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  updated_n
}

article_lab_count_badge <- function(count, label = "titles") {
  tags$span(class = "lab-count-badge", sprintf("%s %s", count, ifelse(identical(count, 1L), sub("s$", "", label), label)))
}

article_lab_signal_chip <- function(label, value, class_name = "default") {
  tags$span(
    class = paste("lab-chip", class_name),
    sprintf("%s %s", label, ifelse(is.na(value), "\u2014", format(round(as.numeric(value), 1), nsmall = 1, trim = TRUE)))
  )
}

article_lab_table_footer <- function(n, label = "titles") {
  if (n < 1) return(NULL)
  div(
    class = "lab-table-footer",
    sprintf("Showing 1 to %s of %s %s", n, n, ifelse(identical(n, 1L), sub("s$", "", label), label))
  )
}

article_lab_section_card <- function(title, description, body, count = NULL, footer = NULL) {
  div(
    class = "lab-card lab-section-card",
    div(
      class = "lab-section-header",
      div(
        h3(title),
        p(class = "lab-section-copy", description)
      ),
      if (is.null(count)) NULL else article_lab_count_badge(as.integer(count))
    ),
    body,
    footer
  )
}

article_lab_generate_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No current titles need triage. Generate and save a new batch, or show disqualified titles to review earlier skips."))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  headers <- c("Select", "Title", "Status", "Notes")
  tagList(
    tags$table(
      class = "lab-table",
      tags$thead(tags$tr(lapply(headers, tags$th))),
      tags$tbody(
        lapply(seq_len(nrow(rows)), function(i) {
          row <- rows[i, , drop = FALSE]
          candidate_id <- row$candidate_id[[1]]
          is_draft <- identical(row$normalized_status[[1]], "draft") || identical(row$batch_id[[1]], "(draft)")
          select_id <- article_lab_row_input_id("article_lab_generate_select", candidate_id)
          status_id <- article_lab_row_input_id("article_lab_generate_status", candidate_id)
          notes_id <- article_lab_row_input_id("article_lab_generate_notes", candidate_id)
          tags$tr(
            `data-selection-group` = "article_lab_generate",
            `data-candidate-id` = candidate_id,
            tags$td(
              class = "select-cell",
              checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
            ),
            tags$td(row$title[[1]]),
            tags$td(
              if (is_draft) {
                article_lab_badge("draft")
              } else {
                selectInput(
                  status_id,
                  label = NULL,
                  choices = article_lab_status_choices(c("generated", "disqualified")),
                  selected = if (row$normalized_status[[1]] %in% c("generated", "disqualified")) row$normalized_status[[1]] else "generated",
                  width = "100%"
                )
              }
            ),
            tags$td(
              if (is_draft) {
                span(class = "lab-status-copy", "Save the batch to start triage.")
              } else {
                textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note")
              }
            )
          )
        })
      )
    )
  )
}

article_lab_badge <- function(value) {
  label <- clean_text(value)
  if (length(label) == 0 || is.na(label[[1]])) label <- "n/a"
  status_key <- tolower(label[[1]])
  class_name <- gsub("[^A-Za-z0-9_]+", "_", status_key)
  tags$span(class = paste("lab-badge", class_name), article_lab_status_label(status_key))
}

article_lab_subtitle_badge <- function(value) {
  label <- clean_text(value)
  if (length(label) == 0 || is.na(label[[1]])) label <- "n/a"
  status_key <- tolower(label[[1]])
  class_name <- paste("subtitle", gsub("[^A-Za-z0-9_]+", "_", status_key), sep = "_")
  tags$span(class = paste("lab-badge", class_name), article_lab_subtitle_status_label(status_key))
}

article_lab_thumbnail_badge <- function(value) {
  label <- clean_text(value)
  if (length(label) == 0 || is.na(label[[1]])) label <- "n/a"
  status_key <- tolower(label[[1]])
  class_name <- paste("thumbnail", gsub("[^A-Za-z0-9_]+", "_", status_key), sep = "_")
  tags$span(class = paste("lab-badge", class_name), article_lab_thumbnail_status_label(status_key))
}

article_lab_score_queue_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No titles are currently waiting in the API queue for this selection."))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_queue_select", row$candidate_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_queue_notes", row$candidate_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_queue",
              `data-candidate-id` = row$candidate_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows))
  )
}

article_lab_score_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No API-scored titles are currently waiting for approval in this selection."))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  score_value <- function(x) {
    value <- suppressWarnings(as.numeric(x))
    ifelse(is.na(value), "\u2014", format(round(value, 1), nsmall = 1, trim = TRUE))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table scored-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "score-col", "Combined Score"),
          tags$th(class = "signals-col", "Main Signals"),
          tags$th(class = "trust-col", "Trust Risk"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_scored_select", row$candidate_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_scored_notes", row$candidate_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_scored",
              `data-candidate-id` = row$candidate_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "score-cell score-strong", score_value(row$combined_title_score[[1]])),
              tags$td(
                class = "signals-cell",
                div(
                  class = "lab-chip-row",
                  article_lab_signal_chip("Curiosity", row$curiosity[[1]], "blue"),
                  article_lab_signal_chip("Emotional", row$emotional_pull[[1]], "purple"),
                  article_lab_signal_chip("Comment", row$medium_comment_potential[[1]], "orange"),
                  article_lab_signal_chip("Overall", row$overall_article_potential[[1]], "green")
                )
              ),
              tags$td(class = "score-cell trust-cell", score_value(row$trust_risk[[1]])),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows))
  )
}

article_lab_subtitle_target_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No approved titles currently need subtitle candidates in this selection."))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_subtitle_title_select", row$candidate_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_subtitle_title_notes", row$candidate_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_subtitle_titles",
              `data-candidate-id` = row$candidate_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows))
  )
}

article_lab_subtitle_candidate_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No subtitle candidates are currently waiting for approval in this selection."))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "subtitle-col", "Subtitle"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_subtitle_candidate_select", row$subtitle_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_subtitle_candidate_notes", row$subtitle_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_subtitle_candidates",
              `data-candidate-id` = row$subtitle_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "subtitle-cell", row$subtitle[[1]]),
              tags$td(class = "status-cell", article_lab_subtitle_badge(row$subtitle_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows), label = "subtitle candidates")
  )
}

article_lab_thumbnail_package_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No title/subtitle packages currently need thumbnail candidates in this selection."))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "subtitle-col", "Subtitle"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_thumbnail_package_select", row$subtitle_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_thumbnail_package_notes", row$subtitle_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_thumbnail_packages",
              `data-candidate-id` = row$subtitle_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "subtitle-cell", row$subtitle[[1]]),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows), label = "packages")
  )
}

article_lab_thumbnail_candidate_grid_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No thumbnail preview cards are currently waiting for approval in this selection."))
  }

  div(
    class = "thumbnail-preview-grid",
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      select_id <- article_lab_row_input_id("article_lab_thumbnail_candidate_select", row$thumbnail_id[[1]])
      notes_id <- article_lab_row_input_id("article_lab_thumbnail_candidate_notes", row$thumbnail_id[[1]])

      div(
        class = "thumbnail-preview-card",
        `data-selection-group` = "article_lab_thumbnail_candidates",
        `data-candidate-id` = row$thumbnail_id[[1]],
        div(
          class = "thumbnail-preview-topbar",
          checkboxInput(select_id, label = NULL, value = FALSE, width = NULL),
          article_lab_thumbnail_badge(row$thumbnail_status[[1]])
        ),
        div(
          class = "thumbnail-preview-shell",
          div(
            class = "thumbnail-preview-meta medium-preview-card",
            div(class = "preview-kicker", row$thumbnail_label[[1]] %||% "Thumbnail candidate"),
            div(class = "preview-title", row$title[[1]]),
            div(class = "preview-subtitle", row$subtitle[[1]])
          ),
          div(
            class = "thumbnail-preview-image-wrap",
            tags$img(
              class = "thumbnail-preview-image",
              src = row$thumbnail_data_uri[[1]],
              alt = paste("Thumbnail candidate for", row$title[[1]])
            )
          )
        ),
        textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note")
      )
    })
  )
}

article_lab_ready_for_outline_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No title/subtitle/thumbnail packages are ready for Outline yet in this selection."))
  }

  div(
    class = "thumbnail-preview-grid",
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      has_outline <- !is.na(row$outline_id[[1]]) && nzchar(row$outline_id[[1]])
      outline_status <- clean_text(row$outline_status[[1]]) %||% "none"
      div(
        class = paste("thumbnail-preview-card approved", if (has_outline) paste0("outline-", outline_status) else "outline-missing"),
        div(
          class = "thumbnail-preview-topbar",
          article_lab_thumbnail_badge("approved"),
          if (!has_outline) checkboxInput(article_lab_row_input_id("article_lab_outline_packages", row$thumbnail_id[[1]]), "Generate outline", value = FALSE),
          if (has_outline && identical(outline_status, "draft")) checkboxInput(article_lab_row_input_id("article_lab_outline_candidates", row$outline_id[[1]]), "Approve outline", value = FALSE)
        ),
        div(
          class = "thumbnail-preview-shell",
          div(
            class = "thumbnail-preview-meta medium-preview-card",
            div(class = "preview-kicker", row$thumbnail_label[[1]] %||% "Approved thumbnail"),
            div(class = "preview-title", row$title[[1]]),
            div(class = "preview-subtitle", row$subtitle[[1]])
          ),
          div(
            class = "thumbnail-preview-image-wrap",
            tags$img(
              class = "thumbnail-preview-image",
              src = row$thumbnail_data_uri[[1]],
              alt = paste("Approved thumbnail for", row$title[[1]])
            )
          )
        ),
        if (has_outline) {
          div(
            class = "lab-outline-editor",
            div(class = "lab-status-copy", sprintf("Outline status: %s", article_lab_status_label(outline_status))),
            textAreaInput(
              article_lab_row_input_id("article_lab_outline_text", row$outline_id[[1]]),
              "Outline draft",
              value = row$outline_text[[1]] %||% "",
              width = "100%",
              height = "320px"
            ),
            textInput(
              article_lab_row_input_id("article_lab_outline_notes", row$outline_id[[1]]),
              "Review notes",
              value = row$outline_notes[[1]] %||% "",
              width = "100%"
            )
          )
        } else {
          div(class = "lab-status-copy", "No outline draft yet. Select this package and generate an outline.")
        }
      )
    })
  )
}

article_lab_ready_for_thumbnail_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No title packages are ready for Thumbnails yet in this selection."))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th(class = "title-col", "Title"),
          tags$th(class = "subtitle-col", "Approved subtitles"),
          tags$th(class = "status-col", "Status")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            subtitle_lines <- clean_text(strsplit(row$approved_subtitles[[1]] %||% "", "\n", fixed = TRUE)[[1]])
            tags$tr(
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(
                class = "subtitle-cell",
                div(
                  class = "approved-subtitle-list",
                  lapply(subtitle_lines[!is.na(subtitle_lines)], function(entry) div(class = "approved-subtitle-item", entry))
                )
              ),
              tags$td(class = "status-cell", article_lab_badge(row$status[[1]]))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows), label = "title packages")
  )
}

read_thumbnail_queue <- function() {
  if (!file.exists(thumbnail_queue_path)) return(data.frame())
  queue <- read.csv(thumbnail_queue_path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("local_image_path", "article_ids", "medium_post_ids")
  for (column in setdiff(required, names(queue))) queue[[column]] <- NA_character_
  for (column in c("normalized_image_url", "primary_image_url_for_download", "image_file_stem")) {
    if (!(column %in% names(queue))) queue[[column]] <- NA_character_
  }
  queue$local_image_path <- clean_text(queue$local_image_path)
  queue$local_image_path_abs <- vapply(queue$local_image_path, function(path) {
    if (is.na(path)) return(NA_character_)
    if (grepl("^/", path)) path else file.path(project_root, path)
  }, character(1), USE.NAMES = FALSE)
  queue$local_path_stem <- tools::file_path_sans_ext(basename(queue$local_image_path_abs))
  queue$local_path_matches_stem <- is.na(clean_text(queue$image_file_stem)) |
    (!is.na(queue$local_path_stem) & startsWith(queue$local_path_stem, clean_text(queue$image_file_stem)))
  queue$local_path_matches_stem[is.na(queue$local_path_matches_stem)] <- FALSE
  queue$local_exists <- !is.na(queue$local_image_path_abs) &
    file.exists(queue$local_image_path_abs) &
    queue$local_path_matches_stem
  queue
}

build_thumbnail_lookup <- function(queue = read_thumbnail_queue()) {
  if (nrow(queue) == 0) {
    return(list(article_ids = character(), post_ids = character(), urls = character()))
  }

  queue <- queue[queue$local_exists, , drop = FALSE]
  if (nrow(queue) == 0) {
    return(list(article_ids = character(), post_ids = character(), urls = character()))
  }

  article_ids <- character()
  article_paths <- character()
  post_ids <- character()
  post_paths <- character()
  urls <- character()
  url_paths <- character()

  for (i in seq_len(nrow(queue))) {
    path <- queue$local_image_path_abs[[i]]
    row_article_ids <- split_keys(queue$article_ids[[i]])
    row_post_ids <- split_keys(queue$medium_post_ids[[i]])
    row_urls <- clean_text(c(queue$normalized_image_url[[i]], queue$primary_image_url_for_download[[i]]))
    row_urls <- row_urls[!is.na(row_urls)]

    if (length(row_article_ids) > 0) {
      article_ids <- c(article_ids, row_article_ids)
      article_paths <- c(article_paths, rep(path, length(row_article_ids)))
    }
    if (length(row_post_ids) > 0) {
      post_ids <- c(post_ids, row_post_ids)
      post_paths <- c(post_paths, rep(path, length(row_post_ids)))
    }
    if (length(row_urls) > 0) {
      urls <- c(urls, row_urls)
      url_paths <- c(url_paths, rep(path, length(row_urls)))
    }
  }

  article_paths <- article_paths[!duplicated(article_ids)]
  names(article_paths) <- article_ids[!duplicated(article_ids)]
  post_paths <- post_paths[!duplicated(post_ids)]
  names(post_paths) <- post_ids[!duplicated(post_ids)]
  url_paths <- url_paths[!duplicated(urls)]
  names(url_paths) <- urls[!duplicated(urls)]

  list(article_ids = article_paths, post_ids = post_paths, urls = url_paths)
}

lookup_map_value <- function(map, key) {
  key <- clean_text(key)
  if (length(key) == 0 || is.na(key) || !(key %in% names(map))) return(NA_character_)
  unname(map[[key]])
}

lookup_local_thumbnail <- function(article_id, medium_post_id, thumbnail_url, lookup) {
  if (length(lookup$article_ids) == 0 && length(lookup$post_ids) == 0 && length(lookup$urls) == 0) {
    return(NA_character_)
  }

  article_key <- clean_text(article_id)
  post_key <- clean_text(medium_post_id)
  thumb_key <- normalize_image_url(thumbnail_url)

  path <- lookup_map_value(lookup$urls, thumb_key)
  if (!is.na(path)) return(path)

  path <- lookup_map_value(lookup$article_ids, article_key)
  if (!is.na(path)) return(path)

  path <- lookup_map_value(lookup$post_ids, post_key)
  if (!is.na(path)) return(path)

  NA_character_
}

get_rated_keys <- function(con) {
  if (!dbExistsTable(con, "human_preview_ratings")) {
    return(list(article_ids = character(), post_ids = character(), article_lab_candidate_ids = character()))
  }

  rated <- dbGetQuery(con, "
    SELECT DISTINCT article_id, medium_post_id, article_lab_candidate_id
    FROM human_preview_ratings
  ")

  article_ids <- clean_text(rated$article_id)
  post_ids <- clean_text(rated$medium_post_id)
  article_lab_candidate_ids <- clean_text(rated$article_lab_candidate_id)
  list(
    article_ids = unique(article_ids[!is.na(article_ids)]),
    post_ids = unique(post_ids[!is.na(post_ids)]),
    article_lab_candidate_ids = unique(article_lab_candidate_ids[!is.na(article_lab_candidate_ids)])
  )
}

mark_duplicate_pending_queue_items <- function(con) {
  rated_keys <- get_rated_keys(con)
  if (length(rated_keys$article_ids) == 0 && length(rated_keys$post_ids) == 0 && length(rated_keys$article_lab_candidate_ids) == 0) return(0L)

  pending <- dbGetQuery(con, "
    SELECT rating_session_id, queue_position, article_id, medium_post_id, source_type, article_lab_candidate_id
    FROM human_preview_rating_queue
    WHERE status = 'pending'
  ")
  if (nrow(pending) == 0) return(0L)

  article_keys <- clean_text(pending$article_id)
  post_keys <- clean_text(pending$medium_post_id)
  article_lab_keys <- clean_text(pending$article_lab_candidate_id)
  duplicate <- (!is.na(article_keys) & article_keys %in% rated_keys$article_ids) |
    (!is.na(post_keys) & post_keys %in% rated_keys$post_ids) |
    (!is.na(article_lab_keys) & article_lab_keys %in% rated_keys$article_lab_candidate_ids)
  duplicate[is.na(duplicate)] <- FALSE
  duplicate_rows <- pending[duplicate, , drop = FALSE]
  if (nrow(duplicate_rows) == 0) return(0L)

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(duplicate_rows))) {
      dbExecute(
        con,
        "UPDATE human_preview_rating_queue
         SET status = 'ignored_duplicate', completed_at = ?
         WHERE rating_session_id = ? AND queue_position = ? AND status = 'pending'",
        params = list(
          now_utc(),
          duplicate_rows$rating_session_id[[i]],
          duplicate_rows$queue_position[[i]]
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  nrow(duplicate_rows)
}

load_candidates <- function(con, exclude_rated = TRUE) {
  objects <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type IN ('table', 'view')")
  if (!("v_medium_title_prediction_dataset_v2" %in% objects$name)) {
    stop("Missing v_medium_title_prediction_dataset_v2. Run the Medium Analysis V2 schema setup first.", call. = FALSE)
  }

  rows <- dbGetQuery(con, "
    SELECT
      canonical_article_key,
      article_id,
      medium_post_id,
      url,
      title,
      subtitle,
      thumbnail_url
    FROM v_medium_title_prediction_dataset_v2
    WHERE NULLIF(TRIM(title), '') IS NOT NULL
  ")
  rows$title <- clean_text(rows$title)
  rows$subtitle <- clean_text(rows$subtitle)

  lookup <- build_thumbnail_lookup()
  rows$local_thumbnail_path <- vapply(seq_len(nrow(rows)), function(i) {
    lookup_local_thumbnail(rows$article_id[[i]], rows$medium_post_id[[i]], rows$thumbnail_url[[i]], lookup)
  }, character(1))
  rows$has_local_thumbnail <- !is.na(rows$local_thumbnail_path) & file.exists(rows$local_thumbnail_path)
  rows <- rows[rows$has_local_thumbnail, , drop = FALSE]
  rows$source_type <- "dataset"
  rows$article_lab_candidate_id <- NA_character_
  rows$candidate_created_at <- NA_character_

  rated_keys <- get_rated_keys(con)
  article_keys <- clean_text(rows$article_id)
  post_keys <- clean_text(rows$medium_post_id)
  rows$already_rated <- (!is.na(article_keys) & article_keys %in% rated_keys$article_ids) |
    (!is.na(post_keys) & post_keys %in% rated_keys$post_ids)
  rows$already_rated[is.na(rows$already_rated)] <- FALSE

  article_lab_rows <- if (dbExistsTable(con, "article_lab_title_candidates")) {
    lab <- dbGetQuery(con, "
      SELECT c.candidate_id AS article_lab_candidate_id,
        c.batch_id,
        c.created_at AS candidate_created_at,
        c.title,
        c.status,
        c.ready_for_human_rating
      FROM article_lab_title_candidates c
      WHERE c.archived = 0
        AND c.promoted = 0
        AND c.ready_for_human_rating = 1
        AND c.status = 'ready_for_human_rating'
      ORDER BY c.created_at DESC, c.candidate_id
    ")
    if (nrow(lab) > 0) {
      lab$title <- clean_text(lab$title)
      lab$already_rated <- clean_text(lab$article_lab_candidate_id) %in% rated_keys$article_lab_candidate_ids
      lab$already_rated[is.na(lab$already_rated)] <- FALSE
      data.frame(
        canonical_article_key = NA_character_,
        article_id = NA_integer_,
        medium_post_id = NA_character_,
        url = NA_character_,
        title = lab$title,
        subtitle = NA_character_,
        thumbnail_url = NA_character_,
        local_thumbnail_path = NA_character_,
        has_local_thumbnail = FALSE,
        source_type = "article_lab_generated",
        article_lab_candidate_id = lab$article_lab_candidate_id,
        candidate_created_at = lab$candidate_created_at,
        already_rated = lab$already_rated,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      data.frame()
    }
  } else {
    data.frame()
  }

  if (isTRUE(exclude_rated)) {
    rows <- rows[!rows$already_rated, , drop = FALSE]
    if (nrow(article_lab_rows) > 0) {
      article_lab_rows <- article_lab_rows[!article_lab_rows$already_rated, , drop = FALSE]
    }
  }

  if (nrow(article_lab_rows) == 0) return(rows)
  if (nrow(rows) == 0) return(article_lab_rows)

  combined <- rbind(article_lab_rows[, names(rows)], rows)
  combined
}

candidate_counts <- function(con) {
  candidates <- load_candidates(con, exclude_rated = FALSE)
  data.frame(
    total_thumbnail_candidates = nrow(candidates),
    already_rated = sum(candidates$already_rated, na.rm = TRUE),
    remaining_unrated = sum(!candidates$already_rated, na.rm = TRUE)
  )
}

append_article_lab_candidates_to_session <- function(con, session_id) {
  candidates <- load_candidates(con, exclude_rated = TRUE)
  candidates <- candidates[candidates$source_type == "article_lab_generated", , drop = FALSE]
  if (nrow(candidates) == 0) return(0L)

  existing <- dbGetQuery(con, "
    SELECT article_lab_candidate_id, queue_position
    FROM human_preview_rating_queue
    WHERE rating_session_id = ?
      AND COALESCE(status, 'pending') != 'removed_article_lab'
  ", params = list(session_id))
  existing_ids <- clean_text(existing$article_lab_candidate_id)
  keep <- !(clean_text(candidates$article_lab_candidate_id) %in% existing_ids)
  keep[is.na(keep)] <- TRUE
  additions <- candidates[keep, , drop = FALSE]
  if (nrow(additions) == 0) return(0L)

  existing_positions <- dbGetQuery(con, "
    SELECT MIN(queue_position) AS min_position
    FROM human_preview_rating_queue
    WHERE rating_session_id = ?
  ", params = list(session_id))
  min_position <- if (nrow(existing_positions) == 0 || is.na(existing_positions$min_position[[1]])) 1L else as.integer(existing_positions$min_position[[1]])
  start_position <- min_position - nrow(additions)

  additions <- additions[order(additions$candidate_created_at, additions$article_lab_candidate_id, decreasing = FALSE), , drop = FALSE]

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(additions))) {
      dbExecute(
        con,
        "INSERT INTO human_preview_rating_queue
         (rating_session_id, queue_position, article_id, medium_post_id, status, source_type, article_lab_candidate_id)
         VALUES (?, ?, ?, ?, 'pending', ?, ?)",
        params = list(
          session_id,
          start_position + i - 1L,
          NA_integer_,
          NA_character_,
          "article_lab_generated",
          additions$article_lab_candidate_id[[i]]
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  nrow(additions)
}

prune_article_lab_candidates_from_session <- function(con, session_id) {
  if (is.null(session_id) || is.na(session_id) || !nzchar(session_id)) return(0L)
  rows <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM human_preview_rating_queue q
    WHERE q.rating_session_id = ?
      AND q.source_type = 'article_lab_generated'
      AND q.status = 'pending'
      AND NOT EXISTS (
        SELECT 1
        FROM article_lab_title_candidates c
        WHERE c.candidate_id = q.article_lab_candidate_id
          AND c.ready_for_human_rating = 1
          AND c.archived = 0
          AND c.promoted = 0
          AND c.status = 'ready_for_human_rating'
      )
    ",
    params = list(session_id)
  )
  removed_n <- if (nrow(rows) == 0 || is.na(rows$n[[1]])) 0L else as.integer(rows$n[[1]])
  if (removed_n < 1L) return(0L)
  dbExecute(
    con,
    "
    UPDATE human_preview_rating_queue
    SET status = 'removed_article_lab', completed_at = COALESCE(completed_at, ?)
    WHERE rating_session_id = ?
      AND source_type = 'article_lab_generated'
      AND status = 'pending'
      AND NOT EXISTS (
        SELECT 1
        FROM article_lab_title_candidates c
        WHERE c.candidate_id = human_preview_rating_queue.article_lab_candidate_id
          AND c.ready_for_human_rating = 1
          AND c.archived = 0
          AND c.promoted = 0
          AND c.status = 'ready_for_human_rating'
      )
    ",
    params = list(now_utc(), session_id)
  )
  removed_n
}

get_dimension_rated_keys <- function(con) {
  if (!dbExistsTable(con, dimension_rating_table)) {
    return(list(canonical = character(), article_ids = character(), post_ids = character()))
  }

  manifest_filter <- if (is_dimension_v2_mode) "AND manifest_version = ?" else ""
  rated <- dbGetQuery(con, sprintf("
    SELECT DISTINCT canonical_article_key, article_id, medium_post_id
    FROM %s
    WHERE rating_mode = ?
      %s
  ", dbQuoteIdentifier(con, dimension_rating_table), manifest_filter), params = if (is_dimension_v2_mode) list(rating_mode, manifest_version) else list(rating_mode))

  canonical <- clean_text(rated$canonical_article_key)
  article_ids <- clean_text(rated$article_id)
  post_ids <- clean_text(rated$medium_post_id)
  list(
    canonical = unique(canonical[!is.na(canonical)]),
    article_ids = unique(article_ids[!is.na(article_ids)]),
    post_ids = unique(post_ids[!is.na(post_ids)])
  )
}

read_dimension_cohort <- function() {
  if (!file.exists(dimension_cohort_path)) return(data.frame())
  cohort <- read.csv(dimension_cohort_path, stringsAsFactors = FALSE, check.names = FALSE)
  for (column in c("canonical_article_key", "article_id", "medium_post_id", "title", "subtitle", "thumbnail_url", "local_image_path", "image_sha256", "thumbnail_status")) {
    if (!(column %in% names(cohort))) cohort[[column]] <- NA_character_
  }
  cohort$canonical_article_key <- clean_text(cohort$canonical_article_key)
  cohort$article_id <- clean_text(cohort$article_id)
  cohort$medium_post_id <- clean_text(cohort$medium_post_id)
  cohort$title <- clean_text(cohort$title)
  cohort$subtitle <- clean_text(cohort$subtitle)
  cohort$thumbnail_url <- clean_text(cohort$thumbnail_url)
  cohort$local_image_path <- clean_text(cohort$local_image_path)
  cohort$image_sha256 <- clean_text(cohort$image_sha256)
  cohort$thumbnail_status <- clean_text(cohort$thumbnail_status)
  cohort
}

load_dimension_candidates <- function(con, exclude_rated = TRUE) {
  if (is_dimension_v2_mode) {
    cohort <- read_dimension_cohort()
    if (nrow(cohort) == 0) return(data.frame())
    total_cohort_rows <- nrow(cohort)
    rows <- cohort[cohort$thumbnail_status == "valid", , drop = FALSE]
    if (nrow(rows) == 0) return(rows)

    rows$local_thumbnail_path <- rows$local_image_path
    rows$local_thumbnail_path_abs <- as_abs_path(rows$local_thumbnail_path)
    rows$current_image_sha256 <- vapply(rows$local_thumbnail_path_abs, file_sha256, character(1))
    rows$hash_matches_manifest <- !is.na(rows$current_image_sha256) &
      !is.na(rows$image_sha256) &
      rows$current_image_sha256 == rows$image_sha256
    rows$hash_matches_manifest[is.na(rows$hash_matches_manifest)] <- FALSE
    rows$has_local_thumbnail <- !is.na(rows$local_thumbnail_path_abs) &
      file.exists(rows$local_thumbnail_path_abs) &
      rows$hash_matches_manifest
    rows$thumbnail_status <- ifelse(rows$has_local_thumbnail, "valid", "stale_or_invalid")
    rows <- rows[rows$has_local_thumbnail, , drop = FALSE]
    if (nrow(rows) == 0) return(rows)

    rated_keys <- get_dimension_rated_keys(con)
    rows$already_dimension_rated <- (!is.na(rows$canonical_article_key) & rows$canonical_article_key %in% rated_keys$canonical) |
      (!is.na(rows$article_id) & rows$article_id %in% rated_keys$article_ids) |
      (!is.na(rows$medium_post_id) & rows$medium_post_id %in% rated_keys$post_ids)
    rows$already_dimension_rated[is.na(rows$already_dimension_rated)] <- FALSE
    rows$cohort_source <- "validated_manifest_v2"
    rows$total_cohort_rows <- total_cohort_rows

    if (isTRUE(exclude_rated)) {
      rows <- rows[!rows$already_dimension_rated, , drop = FALSE]
    }

    return(rows)
  }

  objects <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type IN ('table', 'view')")
  if (!("v_medium_title_prediction_dataset_v2" %in% objects$name)) {
    stop("Missing v_medium_title_prediction_dataset_v2. Run the Medium Analysis V2 schema setup first.", call. = FALSE)
  }

  rows <- dbGetQuery(con, "
    SELECT
      canonical_article_key,
      article_id,
      medium_post_id,
      url,
      title,
      subtitle,
      thumbnail_url
    FROM v_medium_title_prediction_dataset_v2
    WHERE NULLIF(TRIM(title), '') IS NOT NULL
  ")
  rows$canonical_article_key <- clean_text(rows$canonical_article_key)
  rows$article_id_text <- clean_text(rows$article_id)
  rows$medium_post_id <- clean_text(rows$medium_post_id)
  rows$title <- clean_text(rows$title)
  rows$subtitle <- clean_text(rows$subtitle)

  cohort <- read_dimension_cohort()
  cohort_source <- if (nrow(cohort) > 0) "all_cohort_csv" else "human_preview_ratings_fallback"
  total_cohort_rows <- nrow(cohort)

  if (nrow(cohort) > 0) {
    keep <- (!is.na(rows$canonical_article_key) & rows$canonical_article_key %in% cohort$canonical_article_key) |
      (!is.na(rows$article_id_text) & rows$article_id_text %in% cohort$article_id) |
      (!is.na(rows$medium_post_id) & rows$medium_post_id %in% cohort$medium_post_id)
    keep[is.na(keep)] <- FALSE
    rows <- rows[keep, , drop = FALSE]
  } else {
    if (!dbExistsTable(con, "human_preview_ratings")) {
      rows <- rows[FALSE, , drop = FALSE]
    } else {
      fallback <- dbGetQuery(con, "
        SELECT DISTINCT article_id, medium_post_id
        FROM human_preview_ratings
      ")
      fallback_article_ids <- clean_text(fallback$article_id)
      fallback_post_ids <- clean_text(fallback$medium_post_id)
      total_cohort_rows <- nrow(fallback)
      keep <- (!is.na(rows$article_id_text) & rows$article_id_text %in% fallback_article_ids) |
        (!is.na(rows$medium_post_id) & rows$medium_post_id %in% fallback_post_ids)
      keep[is.na(keep)] <- FALSE
      rows <- rows[keep, , drop = FALSE]
    }
  }

  lookup <- build_thumbnail_lookup()
  rows$local_thumbnail_path <- vapply(seq_len(nrow(rows)), function(i) {
    lookup_local_thumbnail(rows$article_id[[i]], rows$medium_post_id[[i]], rows$thumbnail_url[[i]], lookup)
  }, character(1))
  rows$has_local_thumbnail <- !is.na(rows$local_thumbnail_path) & file.exists(rows$local_thumbnail_path)
  rows$thumbnail_status <- ifelse(rows$has_local_thumbnail, "valid", "stale_or_invalid")
  rows <- rows[rows$has_local_thumbnail, , drop = FALSE]

  rated_keys <- get_dimension_rated_keys(con)
  rows$already_dimension_rated <- (!is.na(rows$canonical_article_key) & rows$canonical_article_key %in% rated_keys$canonical) |
    (!is.na(rows$article_id_text) & rows$article_id_text %in% rated_keys$article_ids) |
    (!is.na(rows$medium_post_id) & rows$medium_post_id %in% rated_keys$post_ids)
  rows$already_dimension_rated[is.na(rows$already_dimension_rated)] <- FALSE
  rows$cohort_source <- cohort_source
  rows$total_cohort_rows <- total_cohort_rows

  if (isTRUE(exclude_rated)) {
    rows <- rows[!rows$already_dimension_rated, , drop = FALSE]
  }

  rows
}

dimension_row_key <- function(canonical_article_key, article_id, medium_post_id) {
  canonical_key <- clean_text(canonical_article_key)
  article_key <- clean_text(article_id)
  post_key <- clean_text(medium_post_id)
  if (!is.na(canonical_key)) return(paste0("canonical:", canonical_key))
  if (!is.na(article_key)) return(paste0("article:", article_key))
  if (!is.na(post_key)) return(paste0("post:", post_key))
  NA_character_
}

dimension_row_keys <- function(rows) {
  vapply(seq_len(nrow(rows)), function(i) {
    dimension_row_key(rows$canonical_article_key[[i]], rows$article_id[[i]], rows$medium_post_id[[i]])
  }, character(1))
}

mark_invalid_dimension_pass_queue_items <- function(con, active_dimension, candidates) {
  valid_keys <- dimension_row_keys(candidates)
  valid_keys <- valid_keys[!is.na(valid_keys)]

  pending <- dbGetQuery(con, "
    SELECT queue_position, article_id, medium_post_id, canonical_article_key
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ?
      AND active_dimension = ?
      AND status = 'pending'
  ", params = list(rating_mode, active_dimension))
  if (nrow(pending) == 0) return(0L)

  pending_keys <- dimension_row_keys(pending)
  invalid <- is.na(pending_keys) | !(pending_keys %in% valid_keys)
  invalid[is.na(invalid)] <- TRUE
  invalid_rows <- pending[invalid, , drop = FALSE]
  if (nrow(invalid_rows) == 0) return(0L)

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(invalid_rows))) {
      dbExecute(
        con,
        "UPDATE human_preview_dimension_pass_queue
         SET status = 'ignored_invalid_thumbnail', completed_at = ?
         WHERE rating_mode = ?
           AND active_dimension = ?
           AND queue_position = ?
           AND status = 'pending'",
        params = list(now_utc(), rating_mode, active_dimension, invalid_rows$queue_position[[i]])
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  nrow(invalid_rows)
}

top_up_dimension_pass_queue <- function(con, active_dimension, candidates, target_n = Inf) {
  if (nrow(candidates) == 0) return(0L)

  existing <- dbGetQuery(con, "
    SELECT queue_position, article_id, medium_post_id, canonical_article_key, status
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ?
      AND active_dimension = ?
  ", params = list(rating_mode, active_dimension))

  desired_n <- if (is.infinite(target_n)) nrow(candidates) else min(as.integer(target_n), nrow(candidates))
  active_status <- existing$status %in% c("pending", "rated", "skipped")
  active_status[is.na(active_status)] <- FALSE
  needed <- desired_n - sum(active_status)
  if (needed <= 0) return(0L)

  existing_keys <- dimension_row_keys(existing)
  existing_keys <- existing_keys[!is.na(existing_keys)]
  candidate_keys <- dimension_row_keys(candidates)
  available <- !is.na(candidate_keys) & !(candidate_keys %in% existing_keys)
  available[is.na(available)] <- FALSE
  if (!any(available)) return(0L)

  seed <- sum(utf8ToInt(active_dimension)) + 1009L
  set.seed(seed)
  additions <- candidates[available, , drop = FALSE]
  additions <- additions[sample.int(nrow(additions)), , drop = FALSE]
  additions <- head(additions, min(needed, nrow(additions)))

  max_position <- if (nrow(existing) == 0 || all(is.na(existing$queue_position))) 0L else max(existing$queue_position, na.rm = TRUE)

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(additions))) {
      completed <- dimension_has_value(
        con,
        additions$article_id[[i]],
        additions$medium_post_id[[i]],
        additions$canonical_article_key[[i]],
        active_dimension
      )
      dbExecute(
        con,
        "INSERT OR IGNORE INTO human_preview_dimension_pass_queue
         (rating_mode, active_dimension, queue_position, article_id, medium_post_id,
          canonical_article_key, status, completed_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          rating_mode,
          active_dimension,
          max_position + i,
          additions$article_id[[i]],
          additions$medium_post_id[[i]],
          additions$canonical_article_key[[i]],
          if (completed) "rated" else "pending",
          if (completed) now_utc() else NA_character_
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  nrow(additions)
}

dimension_candidate_counts <- function(con) {
  candidates <- load_dimension_candidates(con, exclude_rated = FALSE)
  total_cohort_rows <- if (nrow(candidates) > 0) candidates$total_cohort_rows[[1]] else nrow(read_dimension_cohort())
  if (total_cohort_rows == 0 && dbExistsTable(con, "human_preview_ratings")) {
    total_cohort_rows <- dbGetQuery(con, "SELECT COUNT(DISTINCT COALESCE(CAST(article_id AS TEXT), medium_post_id)) AS n FROM human_preview_ratings")$n[[1]]
  }
  status <- if (dbExistsTable(con, "human_preview_dimension_pass_queue")) {
    dbGetQuery(con, "
      SELECT
        active_dimension,
        COUNT(*) AS total,
        SUM(CASE WHEN status IN ('rated', 'skipped') THEN 1 ELSE 0 END) AS completed,
        SUM(CASE WHEN status = 'pending' THEN 1 ELSE 0 END) AS pending
      FROM human_preview_dimension_pass_queue
      WHERE rating_mode = ?
      GROUP BY active_dimension
    ", params = list(rating_mode))
  } else {
    data.frame(active_dimension = character(), total = integer(), completed = integer(), pending = integer())
  }
  completed_dimensions <- sum(vapply(active_dimension_fields, function(field) {
    row <- status[status$active_dimension == field, , drop = FALSE]
    nrow(row) > 0 && !is.na(row$pending[[1]]) && row$pending[[1]] == 0 && row$total[[1]] > 0
  }, logical(1)))
  data.frame(
    total_cohort_rows = total_cohort_rows,
    usable_local_thumbnails = nrow(candidates),
    completed_dimensions = completed_dimensions,
    total_dimensions = length(active_dimension_fields),
    cohort_source = if (nrow(candidates) > 0) candidates$cohort_source[[1]] else if (file.exists(dimension_cohort_path)) "all_cohort_csv" else "human_preview_ratings_fallback"
  )
}

create_new_session <- function(con, target_n = Inf) {
  seed <- sample.int(.Machine$integer.max, 1)
  session_id <- paste0("preview_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", seed)
  candidates <- load_candidates(con, exclude_rated = TRUE)
  if (nrow(candidates) == 0) stop("No unrated candidate articles with local thumbnails were found.", call. = FALSE)

  set.seed(seed)
  article_lab_candidates <- candidates[candidates$source_type == "article_lab_generated", , drop = FALSE]
  dataset_candidates <- candidates[candidates$source_type != "article_lab_generated", , drop = FALSE]
  if (nrow(article_lab_candidates) > 0) {
    article_lab_candidates <- article_lab_candidates[order(article_lab_candidates$candidate_created_at, article_lab_candidates$article_lab_candidate_id, decreasing = FALSE), , drop = FALSE]
  }
  shuffled_dataset <- if (nrow(dataset_candidates) > 0) dataset_candidates[sample.int(nrow(dataset_candidates)), , drop = FALSE] else dataset_candidates
  shuffled <- if (nrow(article_lab_candidates) > 0) rbind(article_lab_candidates, shuffled_dataset) else shuffled_dataset
  selected_n <- min(target_n, nrow(shuffled))
  selected_n <- as.integer(selected_n)
  selected <- head(shuffled, selected_n)

  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      "INSERT INTO human_rating_sessions
       (rating_session_id, created_at, interface_version, rating_mode, queue_seed, target_n, notes)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(session_id, now_utc(), interface_version, rating_mode, seed, selected_n, "Mode: unrated thumbnails only")
    )

    for (i in seq_len(nrow(selected))) {
      dbExecute(
        con,
        "INSERT INTO human_preview_rating_queue
         (rating_session_id, queue_position, article_id, medium_post_id, status, source_type, article_lab_candidate_id)
         VALUES (?, ?, ?, ?, 'pending', ?, ?)",
        params = list(
          session_id,
          i,
          selected$article_id[[i]],
          selected$medium_post_id[[i]],
          selected$source_type[[i]],
          selected$article_lab_candidate_id[[i]]
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  session_id
}

resume_or_create_session <- function(con, target_n = Inf) {
  mark_duplicate_pending_queue_items(con)

  existing <- dbGetQuery(con, "
    SELECT s.rating_session_id
    FROM human_rating_sessions s
    WHERE s.interface_version = ?
      AND s.rating_mode = ?
      AND EXISTS (
        SELECT 1
        FROM human_preview_rating_queue q
        WHERE q.rating_session_id = s.rating_session_id
          AND q.status = 'pending'
      )
    ORDER BY s.created_at DESC
    LIMIT 1
  ", params = list(interface_version, rating_mode))

  if (nrow(existing) > 0) {
    session_id <- existing$rating_session_id[[1]]
    prune_article_lab_candidates_from_session(con, session_id)
    append_article_lab_candidates_to_session(con, session_id)
    session_id
  } else {
    create_new_session(con, target_n)
  }
}

load_current_item <- function(con, session_id) {
  item <- dbGetQuery(con, "
    SELECT rating_session_id, queue_position, article_id, medium_post_id, status, shown_at, completed_at, source_type, article_lab_candidate_id
    FROM human_preview_rating_queue
    WHERE rating_session_id = ?
      AND status = 'pending'
    ORDER BY queue_position
    LIMIT 1
  ", params = list(session_id))
  if (nrow(item) == 0) return(NULL)

  if (is.na(item$shown_at[[1]]) || !nzchar(item$shown_at[[1]])) {
    item$shown_at[[1]] <- now_utc()
    dbExecute(
      con,
      "UPDATE human_preview_rating_queue
       SET shown_at = ?
       WHERE rating_session_id = ? AND queue_position = ?",
      params = list(item$shown_at[[1]], session_id, item$queue_position[[1]])
    )
  }

  source_type <- first_value(item, "source_type", "dataset")
  if (identical(source_type, "article_lab_generated")) {
    details <- dbGetQuery(con, "
      SELECT candidate_id AS article_lab_candidate_id, batch_id, created_at, title, status
      FROM article_lab_title_candidates
      WHERE candidate_id = ?
      LIMIT 1
    ", params = list(item$article_lab_candidate_id[[1]]))
    if (nrow(details) == 0) return(NULL)

    details$title <- clean_text(details$title)
    details$subtitle <- NA_character_
    details$thumbnail_url <- NA_character_
    details$local_thumbnail_path <- NA_character_
    details$url <- NA_character_
    details$canonical_article_key <- NA_character_
    details$article_id <- NA_integer_
    details$medium_post_id <- NA_character_
    details$thumbnail_status <- "article_lab_title_only"
    return(cbind(item, details[1, , drop = FALSE]))
  }

  details <- dbGetQuery(con, "
    SELECT
      canonical_article_key,
      article_id,
      medium_post_id,
      url,
      title,
      subtitle,
      thumbnail_url
    FROM v_medium_title_prediction_dataset_v2
    WHERE article_id = ?
       OR (medium_post_id IS NOT NULL AND medium_post_id = ?)
    LIMIT 1
  ", params = list(item$article_id[[1]], item$medium_post_id[[1]]))

  if (nrow(details) == 0) {
    details <- dbGetQuery(con, "
      SELECT
        NULL AS canonical_article_key,
        id AS article_id,
        medium_post_id,
        url,
        title,
        subtitle,
        image_url AS thumbnail_url
      FROM medium_articles
      WHERE id = ?
      LIMIT 1
    ", params = list(item$article_id[[1]]))
  }

  if (nrow(details) == 0) return(NULL)

  details$title <- clean_text(details$title)
  details$subtitle <- clean_text(details$subtitle)
  lookup <- build_thumbnail_lookup()
  details$local_thumbnail_path <- lookup_local_thumbnail(
    details$article_id[[1]],
    details$medium_post_id[[1]],
    details$thumbnail_url[[1]],
    lookup
  )

  cbind(item, details[1, , drop = FALSE])
}

queue_counts <- function(con, session_id) {
  dbGetQuery(con, "
    SELECT
      COUNT(*) AS total,
      SUM(CASE WHEN status IN ('rated', 'skipped') THEN 1 ELSE 0 END) AS completed,
      SUM(CASE WHEN status = 'pending' THEN 1 ELSE 0 END) AS pending,
      SUM(CASE WHEN status = 'ignored_duplicate' THEN 1 ELSE 0 END) AS ignored_duplicate
    FROM human_preview_rating_queue
    WHERE rating_session_id = ?
  ", params = list(session_id))
}

save_current_rating <- function(con, item, score = NULL, note = "", skipped = FALSE, shown_started_at = Sys.time()) {
  if (is.null(item) || nrow(item) == 0) return(invisible(FALSE))
  rated_at <- now_utc()
  seconds_spent <- as.numeric(difftime(Sys.time(), shown_started_at, units = "secs"))
  seconds_spent <- max(0, seconds_spent)
  score_value <- if (is.null(score)) NA_integer_ else as.integer(score)
  note_value <- clean_text(note)
  if (length(note_value) == 0 || is.na(note_value[[1]])) note_value <- NA_character_

  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      "INSERT INTO human_preview_ratings
       (rating_session_id, queue_position, article_id, medium_post_id, interface_version,
        rating_prompt, shown_title, shown_subtitle, shown_thumbnail_path,
        human_feed_success_potential, human_feed_success_note, skipped, source_type, article_lab_candidate_id,
        shown_at, rated_at, seconds_spent)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        item$rating_session_id[[1]],
        item$queue_position[[1]],
        item$article_id[[1]],
        item$medium_post_id[[1]],
        interface_version,
        rating_prompt,
        item$title[[1]],
        item$subtitle[[1]],
        item$local_thumbnail_path[[1]],
        score_value,
        note_value[[1]],
        if (isTRUE(skipped)) 1L else 0L,
        first_value(item, "source_type", "dataset"),
        first_value(item, "article_lab_candidate_id"),
        item$shown_at[[1]],
        rated_at,
        seconds_spent
      )
    )

    dbExecute(
      con,
      "UPDATE human_preview_rating_queue
       SET status = ?, completed_at = ?
       WHERE rating_session_id = ? AND queue_position = ?",
      params = list(
        if (isTRUE(skipped)) "skipped" else "rated",
        rated_at,
        item$rating_session_id[[1]],
        item$queue_position[[1]]
      )
    )

    if (identical(first_value(item, "source_type", "dataset"), "article_lab_generated")) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates
         SET status = ?, ready_for_human_rating = 0
         WHERE candidate_id = ?",
        params = list(
          if (isTRUE(skipped)) "human_skipped" else "human_rated",
          first_value(item, "article_lab_candidate_id")
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  invisible(TRUE)
}

undo_previous_rating <- function(con, session_id) {
  previous <- dbGetQuery(con, "
    SELECT id, queue_position, source_type, article_lab_candidate_id
    FROM human_preview_ratings
    WHERE rating_session_id = ?
    ORDER BY rated_at DESC, id DESC
    LIMIT 1
  ", params = list(session_id))
  if (nrow(previous) == 0) return(FALSE)

  dbBegin(con)
  tryCatch({
    dbExecute(con, "DELETE FROM human_preview_ratings WHERE id = ?", params = list(previous$id[[1]]))
    dbExecute(
      con,
      "UPDATE human_preview_rating_queue
       SET status = 'pending', shown_at = NULL, completed_at = NULL
       WHERE rating_session_id = ? AND queue_position = ?",
      params = list(session_id, previous$queue_position[[1]])
    )
    if (identical(first_value(previous, "source_type", "dataset"), "article_lab_generated")) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates
         SET status = 'ready_for_human_rating', ready_for_human_rating = 1
         WHERE candidate_id = ?",
        params = list(first_value(previous, "article_lab_candidate_id"))
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  TRUE
}

dimension_has_value <- function(con, article_id, medium_post_id, canonical_article_key, active_dimension) {
  if (!(active_dimension %in% dimension_fields)) return(FALSE)
  manifest_filter <- if (is_dimension_v2_mode) "AND manifest_version = ?" else ""
  rows <- dbGetQuery(
    con,
    sprintf("
      SELECT %s AS value
      FROM %s
      WHERE rating_mode = ?
        %s
        AND (
          (canonical_article_key IS NOT NULL AND canonical_article_key = ?)
          OR (canonical_article_key IS NULL AND article_id IS NOT NULL AND article_id = ?)
          OR (canonical_article_key IS NULL AND article_id IS NULL AND medium_post_id IS NOT NULL AND medium_post_id = ?)
        )
      ORDER BY updated_at DESC, rated_at DESC, id DESC
      LIMIT 1
    ", dbQuoteIdentifier(con, active_dimension), dbQuoteIdentifier(con, dimension_rating_table), manifest_filter),
    params = if (is_dimension_v2_mode) {
      list(rating_mode, manifest_version, canonical_article_key, article_id, medium_post_id)
    } else {
      list(rating_mode, canonical_article_key, article_id, medium_post_id)
    }
  )
  nrow(rows) > 0 && !is.na(rows$value[[1]]) && nzchar(as.character(rows$value[[1]]))
}

ensure_dimension_pass_queue <- function(con, active_dimension, target_n = Inf) {
  if (!(active_dimension %in% dimension_fields)) stop("Unknown dimension: ", active_dimension, call. = FALSE)
  candidates <- load_dimension_candidates(con, exclude_rated = FALSE)
  if (nrow(candidates) == 0) stop("No dimension-rating candidate articles with valid local thumbnails were found.", call. = FALSE)

  existing <- dbGetQuery(con, "
    SELECT COUNT(*) AS n
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ? AND active_dimension = ?
  ", params = list(rating_mode, active_dimension))
  if (existing$n[[1]] > 0) {
    mark_invalid_dimension_pass_queue_items(con, active_dimension, candidates)
    top_up_dimension_pass_queue(con, active_dimension, candidates, target_n = target_n)
    return(invisible(FALSE))
  }

  seed <- sum(utf8ToInt(active_dimension)) + 1009L
  set.seed(seed)
  shuffled <- candidates[sample.int(nrow(candidates)), , drop = FALSE]
  selected_n <- min(target_n, nrow(shuffled))
  selected <- head(shuffled, as.integer(selected_n))

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(selected))) {
      completed <- dimension_has_value(
        con,
        selected$article_id[[i]],
        selected$medium_post_id[[i]],
        selected$canonical_article_key[[i]],
        active_dimension
      )
      dbExecute(
        con,
        "INSERT OR IGNORE INTO human_preview_dimension_pass_queue
         (rating_mode, active_dimension, queue_position, article_id, medium_post_id,
          canonical_article_key, status, completed_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          rating_mode,
          active_dimension,
          i,
          selected$article_id[[i]],
          selected$medium_post_id[[i]],
          selected$canonical_article_key[[i]],
          if (completed) "rated" else "pending",
          if (completed) now_utc() else NA_character_
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  invisible(TRUE)
}

ensure_dimension_pass_queues <- function(con, target_n = Inf) {
  for (field in active_dimension_fields) ensure_dimension_pass_queue(con, field, target_n = target_n)
  invisible(TRUE)
}

dimension_queue_counts <- function(con, active_dimension) {
  ensure_dimension_pass_queue(con, active_dimension, target_n = default_target_n)
  dbGetQuery(con, "
    SELECT
      COUNT(*) AS total,
      SUM(CASE WHEN status IN ('rated', 'skipped') THEN 1 ELSE 0 END) AS completed,
      SUM(CASE WHEN status = 'pending' THEN 1 ELSE 0 END) AS pending,
      SUM(CASE WHEN status = 'skipped' THEN 1 ELSE 0 END) AS skipped
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ? AND active_dimension = ?
  ", params = list(rating_mode, active_dimension))
}

dimension_pass_status <- function(con) {
  ensure_dimension_pass_queues(con, target_n = default_target_n)
  counts <- do.call(rbind, lapply(active_dimension_fields, function(field) {
    c <- dimension_queue_counts(con, field)
    data.frame(
      active_dimension = field,
      total = ifelse(is.na(c$total[[1]]), 0L, c$total[[1]]),
      completed = ifelse(is.na(c$completed[[1]]), 0L, c$completed[[1]]),
      pending = ifelse(is.na(c$pending[[1]]), 0L, c$pending[[1]]),
      skipped = ifelse(is.na(c$skipped[[1]]), 0L, c$skipped[[1]])
    )
  }))
  counts$dimension_index <- match(counts$active_dimension, active_dimension_fields)
  counts
}

first_incomplete_dimension <- function(con) {
  status <- dimension_pass_status(con)
  incomplete <- status[status$pending > 0, , drop = FALSE]
  if (nrow(incomplete) == 0) return(NA_character_)
  incomplete$active_dimension[[which.min(incomplete$dimension_index)]]
}

next_incomplete_dimension_after <- function(con, active_dimension) {
  status <- dimension_pass_status(con)
  current_index <- match(active_dimension, active_dimension_fields)
  incomplete <- status[status$pending > 0 & status$dimension_index > current_index, , drop = FALSE]
  if (nrow(incomplete) == 0) return(NA_character_)
  incomplete$active_dimension[[which.min(incomplete$dimension_index)]]
}

initialize_app_database <- local({
  initialized <- FALSE

  function() {
    if (isTRUE(initialized)) return(invisible(TRUE))
    con <- connect_db()
    on.exit(dbDisconnect(con), add = TRUE)

    ensure_rating_schema(con)
    ensure_article_lab_schema(con)
    ensure_research_workflow_schema(con)
    article_lab_recover_api_pending_candidates(con)
    if (is_dimension_mode) ensure_dimension_pass_queues(con, target_n = default_target_n)

    initialized <<- TRUE
    invisible(TRUE)
  }
})

initialize_app_database()

load_current_dimension_item <- function(con, active_dimension) {
  ensure_dimension_pass_queue(con, active_dimension, target_n = default_target_n)
  item <- dbGetQuery(con, "
    SELECT rating_mode, active_dimension, queue_position, article_id, medium_post_id,
      canonical_article_key, status, shown_at, completed_at, seconds_spent
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ?
      AND active_dimension = ?
      AND status = 'pending'
    ORDER BY queue_position
    LIMIT 1
  ", params = list(rating_mode, active_dimension))
  if (nrow(item) == 0) return(NULL)

  if (is.na(item$shown_at[[1]]) || !nzchar(item$shown_at[[1]])) {
    item$shown_at[[1]] <- now_utc()
    dbExecute(
      con,
      "UPDATE human_preview_dimension_pass_queue
       SET shown_at = ?
       WHERE rating_mode = ? AND active_dimension = ? AND queue_position = ?",
      params = list(item$shown_at[[1]], rating_mode, active_dimension, item$queue_position[[1]])
    )
  }

  if (is_dimension_v2_mode) {
    candidates <- load_dimension_candidates(con, exclude_rated = FALSE)
    if (nrow(candidates) == 0) return(NULL)
    candidate_keys <- dimension_row_keys(candidates)
    item_key <- dimension_row_key(item$canonical_article_key[[1]], item$article_id[[1]], item$medium_post_id[[1]])
    match_index <- match(item_key, candidate_keys)
    if (is.na(match_index)) return(NULL)
    candidate <- candidates[match_index, , drop = FALSE]
    current_item <- data.frame(
      rating_mode = item$rating_mode[[1]],
      active_dimension = item$active_dimension[[1]],
      queue_position = item$queue_position[[1]],
      article_id = item$article_id[[1]],
      medium_post_id = item$medium_post_id[[1]],
      canonical_article_key = item$canonical_article_key[[1]],
      status = item$status[[1]],
      shown_at = item$shown_at[[1]],
      completed_at = item$completed_at[[1]],
      seconds_spent = item$seconds_spent[[1]],
      title = candidate$title[[1]],
      subtitle = candidate$subtitle[[1]],
      thumbnail_url = candidate$thumbnail_url[[1]],
      local_image_path = candidate$local_image_path[[1]],
      local_thumbnail_path = candidate$local_thumbnail_path[[1]],
      local_thumbnail_path_abs = candidate$local_thumbnail_path_abs[[1]],
      image_sha256 = candidate$image_sha256[[1]],
      current_image_sha256 = candidate$current_image_sha256[[1]],
      hash_matches_manifest = candidate$hash_matches_manifest[[1]],
      thumbnail_status = candidate$thumbnail_status[[1]],
      cohort_source = if ("cohort_source" %in% names(candidate)) candidate$cohort_source[[1]] else NA_character_,
      render_source = "validated_manifest_v2",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    info <- v2_render_info(current_item)
    message(sprintf(
      "dimensions_v2 render audit | queue_position=%s | article_id=%s | medium_post_id=%s | canonical_article_key=%s | local_thumbnail_path=%s | manifest image_sha256=%s | rendered file path=%s | rendered file sha256=%s | hashes_match=%s",
      first_value(current_item, "queue_position"),
      first_value(current_item, "article_id"),
      first_value(current_item, "medium_post_id"),
      first_value(current_item, "canonical_article_key"),
      info$path,
      info$manifest_hash,
      info$path_abs,
      info$rendered_hash,
      isTRUE(info$valid)
    ))
    return(current_item)
  }

  details <- dbGetQuery(con, "
    SELECT
      canonical_article_key,
      article_id,
      medium_post_id,
      url,
      title,
      subtitle,
      thumbnail_url
    FROM v_medium_title_prediction_dataset_v2
    WHERE canonical_article_key = ?
       OR article_id = ?
       OR (medium_post_id IS NOT NULL AND medium_post_id = ?)
    LIMIT 1
  ", params = list(item$canonical_article_key[[1]], item$article_id[[1]], item$medium_post_id[[1]]))

  if (nrow(details) == 0) return(NULL)

  details$title <- clean_text(details$title)
  details$subtitle <- clean_text(details$subtitle)
  lookup <- build_thumbnail_lookup()
  details$local_thumbnail_path <- lookup_local_thumbnail(
    details$article_id[[1]],
    details$medium_post_id[[1]],
    details$thumbnail_url[[1]],
    lookup
  )
  details$thumbnail_status <- if (
    !is.na(details$local_thumbnail_path[[1]]) &&
      file.exists(details$local_thumbnail_path[[1]])
  ) "valid" else "stale_or_invalid"

  cbind(item, details[1, , drop = FALSE])
}

find_dimension_rating_id <- function(con, item) {
  manifest_filter <- if (is_dimension_v2_mode) "AND manifest_version = ?" else ""
  rows <- dbGetQuery(con, sprintf("
    SELECT id, human_dimension_note
    FROM %s
    WHERE rating_mode = ?
      %s
      AND (
        (canonical_article_key IS NOT NULL AND canonical_article_key = ?)
        OR (canonical_article_key IS NULL AND article_id IS NOT NULL AND article_id = ?)
        OR (canonical_article_key IS NULL AND article_id IS NULL AND medium_post_id IS NOT NULL AND medium_post_id = ?)
      )
    ORDER BY updated_at DESC, rated_at DESC, id DESC
    LIMIT 1
  ", dbQuoteIdentifier(con, dimension_rating_table), manifest_filter), params = if (is_dimension_v2_mode) {
    list(rating_mode, manifest_version, item$canonical_article_key[[1]], item$article_id[[1]], item$medium_post_id[[1]])
  } else {
    list(rating_mode, item$canonical_article_key[[1]], item$article_id[[1]], item$medium_post_id[[1]])
  })
  if (nrow(rows) == 0) NULL else rows[1, , drop = FALSE]
}

update_dimension_note <- function(existing_note, active_dimension, note) {
  note_value <- clean_text(note)
  if (length(note_value) == 0 || is.na(note_value[[1]])) return(existing_note)
  existing <- clean_text(existing_note)
  lines <- if (length(existing) == 0 || is.na(existing[[1]])) character() else strsplit(existing[[1]], "\n", fixed = TRUE)[[1]]
  prefix <- paste0("[", active_dimension, "]")
  lines <- lines[!startsWith(lines, prefix)]
  paste(c(lines, paste(prefix, note_value[[1]])), collapse = "\n")
}

save_current_dimension_rating <- function(con, item, active_dimension, value = NULL, note = "", skipped = FALSE, shown_started_at = Sys.time()) {
  if (is.null(item) || nrow(item) == 0) return(invisible(FALSE))
  if (!(active_dimension %in% dimension_fields)) return(invisible(FALSE))
  rated_at <- now_utc()
  seconds_spent <- as.numeric(difftime(Sys.time(), shown_started_at, units = "secs"))
  seconds_spent <- max(0, seconds_spent)
  rating_value <- if (isTRUE(skipped)) NA else value
  if (!isTRUE(skipped) && active_dimension %in% dimension_numeric_fields) {
    rating_value <- suppressWarnings(as.integer(rating_value))
    if (is.na(rating_value) || rating_value < 1L || rating_value > 5L) return(invisible(FALSE))
  }
  if (!isTRUE(skipped) && active_dimension == "ai_low_effort_flag") {
    if (!(rating_value %in% c("yes", "unsure", "no"))) return(invisible(FALSE))
  }
  shown_subtitle <- displayed_subtitle_for_field(item, active_dimension)
  shown_thumbnail_path <- displayed_thumbnail_path_for_field(item, active_dimension)
  shown_image_sha256 <- if (is_dimension_v2_mode) {
    if (active_dimension %in% text_only_dimension_fields) {
      NA_character_
    } else {
    info <- v2_render_info(item)
    if (!isTRUE(info$valid)) return(invisible(FALSE))
    info$rendered_hash
    }
  } else {
    NA_character_
  }

  dbBegin(con)
  tryCatch({
    existing <- find_dimension_rating_id(con, item)
    existing_note <- if (is.null(existing)) NA_character_ else existing$human_dimension_note[[1]]
    note_value <- update_dimension_note(existing_note, active_dimension, note)
    if (length(note_value) == 0 || is.na(note_value[[1]])) note_value <- NA_character_

    if (is.null(existing)) {
      if (is_dimension_v2_mode) {
        dbExecute(
          con,
          sprintf(
            "INSERT INTO human_preview_dimension_ratings_v2
             (rating_session_id, queue_position, article_id, medium_post_id, canonical_article_key,
              interface_version, rating_mode, manifest_version, shown_title, shown_subtitle,
              shown_thumbnail_path, shown_image_sha256, %s, human_dimension_note, skipped,
              shown_at, rated_at, seconds_spent, created_at, updated_at)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            dbQuoteIdentifier(con, active_dimension)
          ),
          params = list(
            NA_character_,
            item$queue_position[[1]],
            item$article_id[[1]],
            item$medium_post_id[[1]],
            item$canonical_article_key[[1]],
            interface_version,
            rating_mode,
            manifest_version,
            item$title[[1]],
            shown_subtitle,
            shown_thumbnail_path,
            shown_image_sha256,
            rating_value,
            note_value[[1]],
            if (isTRUE(skipped)) 1L else 0L,
            item$shown_at[[1]],
            rated_at,
            seconds_spent,
            rated_at,
            rated_at
          )
        )
      } else {
        dbExecute(
          con,
          sprintf(
            "INSERT INTO human_preview_dimension_ratings
             (rating_session_id, queue_position, article_id, medium_post_id, canonical_article_key,
              interface_version, rating_mode, shown_title, shown_subtitle, shown_thumbnail_path,
              %s, human_dimension_note, skipped, shown_at, rated_at, seconds_spent, created_at, updated_at)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            dbQuoteIdentifier(con, active_dimension)
          ),
          params = list(
            NA_character_,
            item$queue_position[[1]],
            item$article_id[[1]],
            item$medium_post_id[[1]],
            item$canonical_article_key[[1]],
            interface_version,
            rating_mode,
            item$title[[1]],
            shown_subtitle,
            shown_thumbnail_path,
            rating_value,
            note_value[[1]],
            if (isTRUE(skipped)) 1L else 0L,
            item$shown_at[[1]],
            rated_at,
            seconds_spent,
            rated_at,
            rated_at
          )
        )
      }
    } else {
      if (is_dimension_v2_mode) {
        dbExecute(
          con,
          sprintf(
            "UPDATE human_preview_dimension_ratings_v2
             SET queue_position = ?, manifest_version = ?, shown_title = ?, shown_subtitle = ?,
               shown_thumbnail_path = ?, shown_image_sha256 = ?, %s = ?,
               human_dimension_note = ?, skipped = ?, shown_at = ?, rated_at = ?,
               seconds_spent = ?, updated_at = ?
             WHERE id = ?",
            dbQuoteIdentifier(con, active_dimension)
          ),
          params = list(
            item$queue_position[[1]],
            manifest_version,
            item$title[[1]],
            shown_subtitle,
            shown_thumbnail_path,
            shown_image_sha256,
            rating_value,
            note_value[[1]],
            if (isTRUE(skipped)) 1L else 0L,
            item$shown_at[[1]],
            rated_at,
            seconds_spent,
            rated_at,
            existing$id[[1]]
          )
        )
      } else {
        dbExecute(
          con,
          sprintf(
            "UPDATE human_preview_dimension_ratings
             SET queue_position = ?, shown_title = ?, shown_subtitle = ?, shown_thumbnail_path = ?,
               %s = ?, human_dimension_note = ?, skipped = ?,
               shown_at = ?, rated_at = ?, seconds_spent = ?, updated_at = ?
             WHERE id = ?",
            dbQuoteIdentifier(con, active_dimension)
          ),
          params = list(
            item$queue_position[[1]],
            item$title[[1]],
            shown_subtitle,
            shown_thumbnail_path,
            rating_value,
            note_value[[1]],
            if (isTRUE(skipped)) 1L else 0L,
            item$shown_at[[1]],
            rated_at,
            seconds_spent,
            rated_at,
            existing$id[[1]]
          )
        )
      }
    }

    dbExecute(
      con,
      "UPDATE human_preview_dimension_pass_queue
       SET status = ?, completed_at = ?, seconds_spent = ?
       WHERE rating_mode = ? AND active_dimension = ? AND queue_position = ?",
      params = list(
        if (isTRUE(skipped)) "skipped" else "rated",
        rated_at,
        seconds_spent,
        rating_mode,
        active_dimension,
        item$queue_position[[1]]
      )
    )
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  invisible(TRUE)
}

undo_previous_dimension_rating <- function(con, active_dimension) {
  if (!(active_dimension %in% dimension_fields)) return(FALSE)
  previous <- dbGetQuery(con, "
    SELECT queue_position, article_id, medium_post_id, canonical_article_key
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ?
      AND active_dimension = ?
      AND status IN ('rated', 'skipped')
    ORDER BY completed_at DESC, queue_position DESC
    LIMIT 1
  ", params = list(rating_mode, active_dimension))
  if (nrow(previous) == 0) return(FALSE)

  dbBegin(con)
  tryCatch({
    pseudo_item <- previous
    rating_row <- find_dimension_rating_id(con, pseudo_item)
    if (!is.null(rating_row)) {
      dbExecute(
        con,
        sprintf(
          "UPDATE %s
           SET %s = NULL, updated_at = ?
           WHERE id = ?",
          dbQuoteIdentifier(con, dimension_rating_table),
          dbQuoteIdentifier(con, active_dimension)
        ),
        params = list(now_utc(), rating_row$id[[1]])
      )
      dbExecute(
        con,
        sprintf("DELETE FROM %s
         WHERE id = ?
           AND personal_click_appeal IS NULL
           AND title_hook_strength IS NULL
           AND visual_hook IS NULL
           AND emotional_pull_preview IS NULL
           AND ai_low_effort_flag IS NULL
           AND NULLIF(TRIM(COALESCE(human_dimension_note, '')), '') IS NULL", dbQuoteIdentifier(con, dimension_rating_table)),
        params = list(rating_row$id[[1]])
      )
    }
    dbExecute(
      con,
      "UPDATE human_preview_dimension_pass_queue
       SET status = 'pending', shown_at = NULL, completed_at = NULL, seconds_spent = NULL
       WHERE rating_mode = ? AND active_dimension = ? AND queue_position = ?",
      params = list(rating_mode, active_dimension, previous$queue_position[[1]])
    )
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  TRUE
}

ui <- fluidPage(
  tags$head(
    tags$title("Medium Preview Rating"),
    tags$style(HTML("
      :root {
        --green: #1a8917;
        --green-soft: #eef7f0;
        --ink: #191919;
        --muted: #6b6b6b;
        --line: #e6e6e6;
        --panel: #f8f8f8;
      }
      body {
        margin: 0;
        color: var(--ink);
        background: #fff;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      }
      .container-fluid { padding: 0; }
      .topbar {
        height: 50px;
        border-bottom: 1px solid var(--line);
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 0 22px;
      }
      .brand { display: flex; align-items: center; gap: 14px; font-size: 20px; font-weight: 700; }
      .brand-mark {
        width: 32px; height: 32px; border-radius: 6px; background: #197b30; color: #fff;
        display: grid; place-items: center; font-family: Georgia, serif; font-size: 23px; font-weight: 700;
      }
      .top-actions { color: var(--muted); display: flex; gap: 22px; align-items: center; font-size: 15px; }
      .app-shell { display: grid; grid-template-columns: 250px minmax(560px, 1fr) 300px; min-height: calc(100vh - 50px); }
      .sidebar, .guide { border-right: 1px solid var(--line); padding: 22px 20px; position: relative; }
      .guide { border-right: 0; border-left: 1px solid var(--line); }
      .sidebar-nav-group { margin-bottom: 18px; }
      .sidebar-nav-label {
        margin: 0 0 10px;
        color: #8a8a8a;
        font-size: 11px;
        font-weight: 750;
        letter-spacing: .08em;
        text-transform: uppercase;
      }
      .nav-item {
        min-height: 58px;
        display: grid;
        grid-template-columns: 18px minmax(0, 1fr);
        gap: 14px;
        align-items: start;
        padding: 12px 14px;
        border-radius: 10px;
        color: var(--ink);
        font-size: 15px;
        margin-bottom: 8px;
      }
      button.nav-item {
        width: 100%;
        border: 0;
        background: transparent;
        text-align: left;
      }
      button.nav-item:hover { background: #f5f5f5; }
      button.nav-item:focus { outline: none; box-shadow: none; }
      button.nav-item:disabled { opacity: .78; cursor: default; }
      .nav-item.active { background: var(--green-soft); }
      .nav-icon {
        font-size: 16px;
        line-height: 1;
        margin-top: 2px;
      }
      .nav-copy {
        display: grid;
        gap: 4px;
        min-width: 0;
      }
      .nav-title {
        font-size: 15px;
        font-weight: 700;
        line-height: 1.15;
      }
      .nav-subtitle {
        color: var(--muted);
        font-size: 12px;
        line-height: 1.3;
      }
      .daily-goal {
        border: 1px solid var(--line); border-radius: 8px; padding: 14px 18px;
        max-width: none;
        position: static;
        margin-top: 18px;
      }
      .daily-goal.static-card {
        position: static;
        max-width: none;
        margin-top: 18px;
      }
      .article-lab-helper {
        display: grid;
        gap: 8px;
      }
      .daily-goal strong { display: block; margin-bottom: 8px; }
      .daily-goal .num { color: var(--green); font-weight: 700; }
      .progress-track { height: 7px; background: #e9e9e9; border-radius: 99px; overflow: hidden; margin: 12px 0 8px; }
      .progress-fill { height: 100%; background: var(--green); border-radius: 99px; width: 0%; }
      .main { padding: 22px 30px 18px; max-width: 920px; width: 100%; margin: 0 auto; }
      .app-shell.workflow-wide-layout { grid-template-columns: 250px minmax(860px, 1fr); }
      .app-shell.workflow-wide-layout .guide,
      .guide.guide-hidden { display: none; }
      .main.workflow-wide-main {
        max-width: 1280px;
        padding-right: 34px;
      }
      .main.workflow-wide-main .page-subtitle,
      .main.workflow-wide-main .lab-card,
      .main.workflow-wide-main .empty-state {
        max-width: none;
      }
      h1 { margin: 0; font-size: 26px; line-height: 1.05; font-weight: 750; letter-spacing: 0; }
      .progress-line { margin-top: 5px; color: var(--muted); font-size: 16px; }
      .progress-line .current { color: var(--green); font-weight: 750; }
      .mode-line { margin-top: 5px; color: var(--muted); font-size: 12px; }
      .mode-line strong { color: var(--green); font-weight: 650; }
      .v2-debug-banner {
        max-width: 760px;
        margin-top: 8px;
        padding: 8px 10px;
        border: 1px solid #b8d8ba;
        border-radius: 6px;
        background: #f4fbf4;
        color: #3f6f42;
        font-size: 11px;
        line-height: 1.35;
        overflow-wrap: anywhere;
      }
      .v2-debug-banner.error {
        border-color: #d95f5f;
        background: #fff5f5;
        color: #a83232;
      }
      .v2-debug-banner strong { color: inherit; font-weight: 700; }
      .v2-paused-warning {
        max-width: 760px;
        margin-top: 8px;
        padding: 9px 11px;
        border: 1px solid #e0c36f;
        border-radius: 6px;
        background: #fff9e8;
        color: #6f5600;
        font-size: 12px;
        line-height: 1.35;
      }
      .tabs { display: flex; gap: 38px; border-bottom: 1px solid var(--line); margin-top: 14px; max-width: 760px; box-sizing: border-box; }
      .tab { padding-bottom: 8px; color: var(--muted); font-size: 15px; }
      .tab.active { color: var(--ink); font-weight: 650; border-bottom: 2px solid var(--ink); }
      .article-card {
        display: grid;
        grid-template-columns: minmax(0, 1fr) 170px;
        gap: 32px;
        align-items: center;
        box-sizing: border-box;
        width: 100%;
        max-width: 760px;
        margin-top: 0;
        padding: 28px 0 30px;
        border-bottom: 1px solid #f2f2f2;
        background: #fff;
      }
      .article-title {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        font-size: 26px; line-height: 1.15; font-weight: 760; letter-spacing: 0; margin: 0 0 8px;
      }
      .article-subtitle { font-size: 18px; line-height: 1.35; color: var(--muted); margin: 0; }
      .thumbnail-wrap { display: flex; justify-content: flex-end; align-items: center; }
      .thumbnail-wrap .shiny-image-output {
        width: 170px !important;
        height: 113px !important;
      }
      .thumbnail-wrap img {
        width: 170px; height: 113px; object-fit: cover; border-radius: 1px; display: block;
      }
      .thumbnail-placeholder {
        width: 170px; height: 113px; border-radius: 1px; background: #fff; border: 1px dashed #cfcfcf;
        box-sizing: border-box; color: #777; font-size: 11px; display: grid; place-items: center; text-align: center;
        padding: 8px;
        white-space: pre-line;
      }
      .thumbnail-placeholder.error {
        border-color: #d95f5f;
        color: #a83232;
        background: #fff5f5;
      }
      .article-card.thumbnail-only {
        grid-template-columns: 170px;
        justify-content: center;
        align-items: center;
        max-width: 760px;
        padding-top: 18px;
      }
      .article-card.thumbnail-only .thumbnail-wrap { justify-content: center; }
      .article-card.thumbnail-only .thumbnail-wrap .shiny-image-output,
      .article-card.thumbnail-only .thumbnail-wrap img,
      .article-card.thumbnail-only .thumbnail-placeholder {
        width: 170px !important;
        height: 113px !important;
      }
      .article-card.text-only {
        grid-template-columns: minmax(0, 1fr);
        max-width: 760px;
      }
      .article-card.text-only .article-title {
        font-size: 30px;
        line-height: 1.12;
        margin-bottom: 10px;
      }
      .article-card.text-only .article-subtitle {
        font-size: 20px;
        line-height: 1.35;
      }
      .dimension-verification-title {
        color: #9a9a9a;
        font-size: 12px;
        line-height: 1.25;
        font-weight: 400;
        max-width: 360px;
        cursor: copy;
        user-select: text;
      }
      .dimension-verification-title.copied { color: var(--green); }
      .thumbnail-invalid-label { max-width: 130px; }
      .rating-panel {
        border: 1px solid var(--line);
        border-radius: 8px;
        box-sizing: border-box;
        background: #fff;
        margin-top: 12px;
        max-width: 760px;
        padding: 14px 20px;
      }
      .prompt { font-size: 15px; line-height: 1.22; font-weight: 680; margin-bottom: 10px; }
      .note-row label { color: var(--muted); font-weight: 500; margin-bottom: 4px; font-size: 13px; }
      .note-row input.form-control, .note-row textarea.form-control {
        height: 40px; border: 1px solid #d9d9d9; border-radius: 8px;
        box-shadow: none; font-size: 14px; padding: 8px 12px;
      }
      .note-row textarea.form-control { min-height: 54px; resize: vertical; }
      .note-row input.form-control:focus, .note-row textarea.form-control:focus { border-color: var(--green); box-shadow: 0 0 0 3px rgba(26, 137, 23, .12); }
      .scale-labels { display: flex; justify-content: space-between; color: var(--muted); margin: 9px 4px 5px; font-size: 13px; }
      .rating-buttons { display: grid; grid-template-columns: repeat(5, minmax(64px, 1fr)); gap: 10px; }
      .rating-buttons .btn {
        height: 40px; border: 1px solid #d8d8d8; border-radius: 7px; background: #fff;
        color: var(--ink); font-size: 19px; font-weight: 650; box-shadow: none;
      }
      .rating-buttons .btn:hover {
        border-color: var(--green); color: var(--green); background: #f6fbf6;
      }
      .rating-buttons .btn.rating-confirm {
        border-color: var(--green); color: #fff; background: var(--green);
      }
      .rating-buttons .btn:focus {
        border-color: #d8d8d8; color: var(--ink); background: #fff; outline: none; box-shadow: none;
      }
      .rating-actions { display: flex; gap: 12px; justify-content: space-between; align-items: center; margin-top: 10px; }
      .rating-actions .btn {
        min-width: 136px; height: 34px; border-radius: 7px; font-size: 13px; font-weight: 650; box-shadow: none;
      }
      .dimension-rubric { display: grid; gap: 8px; margin-top: 10px; }
      .dimension-row {
        border: 1px solid var(--line);
        border-radius: 8px;
        padding: 9px 10px;
        display: grid;
        grid-template-columns: minmax(0, 1fr) auto;
        gap: 12px;
        align-items: center;
      }
      .dimension-row.active { border-color: var(--green); background: var(--green-soft); }
      .dimension-name { font-size: 13px; font-weight: 720; margin-bottom: 2px; }
      .dimension-question { color: var(--muted); font-size: 12px; line-height: 1.25; }
      .dimension-buttons { display: flex; gap: 6px; flex-wrap: nowrap; }
      .dimension-choice.btn {
        min-width: 42px; min-height: 32px; padding: 4px 8px; border-radius: 7px;
        border: 1px solid #d8d8d8; background: #fff; color: var(--ink);
        font-weight: 650; box-shadow: none;
      }
      .dimension-choice.flag-choice { min-width: 64px; display: grid; gap: 1px; justify-items: center; line-height: 1.05; }
      .dimension-choice-label { display: block; }
      .dimension-choice-shortcut { display: block; color: #a0a0a0; font-size: 10px; font-weight: 500; }
      .dimension-choice.selected,
      .dimension-choice.dimension-confirm { border-color: var(--green); background: var(--green); color: #fff; }
      .dimension-choice.selected .dimension-choice-shortcut,
      .dimension-choice.dimension-confirm .dimension-choice-shortcut { color: rgba(255,255,255,.72); }
      .dimension-pass-header { display: grid; gap: 6px; margin-bottom: 12px; }
      .dimension-pass-kicker { color: var(--green); font-size: 12px; font-weight: 750; text-transform: uppercase; letter-spacing: 0; }
      .dimension-pass-name { font-size: 22px; line-height: 1.1; font-weight: 760; }
      .dimension-pass-question { font-size: 15px; line-height: 1.28; font-weight: 650; }
      .dimension-pass-focus { color: var(--muted); font-size: 13px; }
      .dimension-scale-list { display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: 6px; margin: 8px 0 10px; }
      .dimension-scale-item {
        border: 1px solid var(--line); border-radius: 7px; padding: 7px 8px; min-height: 46px;
        font-size: 12px; line-height: 1.2; text-align: left; box-shadow: none;
      }
      .dimension-scale-item strong { display: block; color: var(--green); font-size: 13px; margin-bottom: 2px; }
      .dimension-scale-shortcut { display: block; color: #a0a0a0; font-size: 10px; font-weight: 500; margin-top: 4px; }
      .dimension-scale-choice { width: 100%; background: #fff; cursor: pointer; transition: background-color .12s ease, border-color .12s ease, color .12s ease; }
      .dimension-scale-choice:hover:not(:disabled) { border-color: #bdbdbd; background: #fafafa; }
      .dimension-scale-choice:disabled { cursor: not-allowed; opacity: .55; }
      .dimension-scale-choice.dimension-confirm,
      .dimension-scale-choice.selected,
      .dimension-scale-choice.dimension-confirm strong,
      .dimension-scale-choice.selected strong { border-color: var(--green); color: #fff; }
      .dimension-scale-choice.dimension-confirm .dimension-scale-shortcut,
      .dimension-scale-choice.selected .dimension-scale-shortcut { color: rgba(255,255,255,.72); }
      .dimension-scale-choice.dimension-confirm,
      .dimension-scale-choice.selected { background: var(--green); }
      .dimension-flag-scale { grid-template-columns: repeat(3, minmax(0, 1fr)); }
      .dimension-flag-scale .dimension-scale-item strong { color: var(--ink); }
      .btn-default { border-color: #d8d8d8; background: #fff; color: var(--ink); }
      .btn-default:hover { border-color: #bdbdbd; background: #fafafa; }
      .next-dimension-cta {
        margin-top: 12px;
        padding: 14px 16px;
        border: 1px solid #cfe4d2;
        border-radius: 8px;
        background: #f5fbf6;
        display: grid;
        gap: 10px;
      }
      .next-dimension-copy {
        color: var(--muted);
        font-size: 13px;
        line-height: 1.35;
      }
      .next-dimension-cta .btn {
        width: 100%;
        min-height: 46px;
        border-radius: 8px;
        border: 1px solid var(--green);
        background: var(--green);
        color: #fff;
        font-size: 15px;
        font-weight: 700;
        box-shadow: none;
      }
      .next-dimension-cta .btn:hover {
        border-color: #166f14;
        background: #166f14;
        color: #fff;
      }
      .next-dimension-cta .btn:focus {
        border-color: #166f14;
        background: #166f14;
        color: #fff;
        outline: none;
        box-shadow: 0 0 0 3px rgba(26, 137, 23, .14);
      }
      .shortcut-copy { color: var(--muted); font-size: 13px; }
      .page-subtitle {
        margin-top: 8px;
        color: var(--muted);
        font-size: 16px;
        line-height: 1.4;
        max-width: 760px;
      }
      .lab-card,
      .status-card,
      .empty-state {
        border: 1px solid var(--line);
        border-radius: 8px;
        background: #fff;
      }
      .lab-card {
        max-width: 760px;
        margin-top: 18px;
        padding: 18px 20px 20px;
      }
      .lab-section-card { padding-bottom: 16px; }
      .lab-card h2 {
        margin: 0 0 12px;
        font-size: 19px;
        font-weight: 760;
      }
      .lab-card h3 {
        margin: 0 0 10px;
        font-size: 17px;
        font-weight: 730;
      }
      .lab-section-header {
        display: flex;
        justify-content: space-between;
        gap: 14px;
        align-items: flex-start;
        margin-bottom: 14px;
      }
      .lab-section-copy {
        margin: 0;
        color: var(--muted);
        font-size: 14px;
        line-height: 1.4;
      }
      .lab-count-badge {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        padding: 4px 10px;
        border-radius: 999px;
        background: #eef4ff;
        color: #2757a3;
        font-size: 12px;
        font-weight: 700;
        white-space: nowrap;
      }
      .lab-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 12px 14px;
        margin-top: 14px;
      }
      .lab-field label {
        display: block;
        color: var(--muted);
        font-size: 13px;
        font-weight: 600;
        margin-bottom: 6px;
      }
      .lab-field .form-control {
        border-radius: 8px;
        border: 1px solid #d9d9d9;
        box-shadow: none;
      }
      .lab-field .form-control:focus {
        border-color: var(--green);
        box-shadow: 0 0 0 3px rgba(26, 137, 23, .12);
      }
      .lab-editor-textarea textarea.form-control {
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, 'Liberation Mono', monospace;
        font-size: 13px;
        line-height: 1.55;
        white-space: pre-wrap;
        tab-size: 2;
      }
      .lab-actions {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        margin-top: 14px;
      }
      .lab-actions .btn {
        min-width: 138px;
        height: 38px;
        border-radius: 8px;
        box-shadow: none;
        font-weight: 650;
      }
      .lab-actions .btn.lab-primary {
        border-color: var(--green);
        background: var(--green);
        color: #fff;
      }
      .lab-actions .btn.lab-primary:hover,
      .lab-actions .btn.lab-primary:focus {
        border-color: #166f14;
        background: #166f14;
        color: #fff;
      }
      .lab-actions .btn.lab-secondary {
        border-color: #d8d8d8;
        background: #fff;
        color: var(--ink);
      }
      .lab-actions .btn.lab-secondary:hover,
      .lab-actions .btn.lab-secondary:focus {
        border-color: #bdbdbd;
        background: #fafafa;
        color: var(--ink);
      }
      .lab-actions .btn.lab-primary.loading,
      .lab-actions .btn.lab-primary.loading:hover,
      .lab-actions .btn.lab-primary.loading:focus {
        border-color: #166f14;
        background: #166f14;
        color: #fff;
        cursor: wait;
      }
      .lab-actions .btn.lab-primary.loading[disabled] {
        opacity: 1;
      }
      .button-spinner {
        display: inline-block;
        width: 14px;
        height: 14px;
        margin-right: 8px;
        border: 2px solid rgba(255,255,255,.34);
        border-top-color: #fff;
        border-radius: 50%;
        vertical-align: -2px;
        animation: lab-spin .75s linear infinite;
      }
      @keyframes lab-spin {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
      }
      .lab-table {
        width: 100%;
        border-collapse: collapse;
        margin-top: 6px;
      }
      .lab-table-wrap {
        width: 100%;
        overflow-x: auto;
      }
      .lab-table th,
      .lab-table td {
        padding: 10px 8px;
        border-bottom: 1px solid #f0f0f0;
        text-align: left;
        vertical-align: top;
        font-size: 13px;
        line-height: 1.35;
      }
      .lab-table th {
        color: var(--muted);
        font-size: 12px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: .01em;
      }
      .lab-table td.score-cell {
        font-variant-numeric: tabular-nums;
        white-space: nowrap;
      }
      .lab-table td.score-strong {
        font-size: 18px;
        font-weight: 760;
      }
      .lab-table td.select-cell {
        width: 56px;
      }
      .lab-table th.title-col,
      .lab-table td.title-cell {
        width: 34%;
      }
      .lab-table th.notes-col,
      .lab-table td.notes-cell {
        width: 24%;
      }
      .lab-table th.subtitle-col,
      .lab-table td.subtitle-cell {
        width: 34%;
      }
      .lab-table th.signals-col,
      .lab-table td.signals-cell {
        width: 24%;
      }
      .lab-table th.score-col,
      .lab-table td.score-cell,
      .lab-table th.trust-col,
      .lab-table td.trust-cell,
      .lab-table th.status-col,
      .lab-table td.status-cell {
        white-space: nowrap;
      }
      .scored-table .notes-cell .form-control {
        min-width: 220px;
      }
      .lab-table .shiny-input-container {
        width: 100%;
        margin-bottom: 0;
      }
      .lab-table .checkbox {
        margin: 0;
      }
      .lab-table .form-control {
        min-height: 34px;
      }
      table.dataTable.research-source-table {
        width: 100% !important;
        table-layout: fixed;
      }
      table.dataTable.research-source-table tbody td {
        vertical-align: middle;
      }
      table.dataTable.research-source-table tbody tr.selected,
      table.dataTable.research-source-table tbody tr.selected > * {
        background-color: #edf8ef !important;
        color: var(--ink) !important;
      }
      table.dataTable.research-source-table .research-source-title,
      table.dataTable.research-source-table .research-source-main {
        display: block;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      table.dataTable.research-source-table .research-source-title,
      table.dataTable.research-source-table .research-source-main { max-width: 100%; }
      table.dataTable.research-source-table .research-source-links {
        white-space: nowrap;
      }
      .lab-table-footer {
        margin-top: 10px;
        color: var(--muted);
        font-size: 13px;
      }
      .lab-badge {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 4px 8px;
        border-radius: 999px;
        font-size: 12px;
        font-weight: 650;
        line-height: 1;
        background: #f4f6f4;
        color: #35523b;
      }
      .lab-badge.mobile_safe,
      .lab-badge.good { background: #edf8ef; color: #1a6d27; }
      .lab-badge.long_but_allowed { background: #fff6e6; color: #8a5a00; }
      .lab-badge.very_long_but_allowed { background: #fff1df; color: #9a4f00; }
      .lab-badge.too_long { background: #fdecec; color: #9b2727; }
      .lab-badge.ready_for_api_scoring { background: #edf8ef; color: #1a6d27; }
      .lab-badge.api_scored { background: #eef4ff; color: #2757a3; }
      .lab-badge.approved_for_subtitle { background: #f0ebff; color: #5a33a2; }
      .lab-badge.ready_for_thumbnail { background: #e9f7f4; color: #166f62; }
      .lab-badge.ready_for_outline { background: #e8f4ea; color: #1f6c2c; }
      .lab-badge.subtitle_generated { background: #f5f5f5; color: #666; }
      .lab-badge.subtitle_approved { background: #e9f7f4; color: #166f62; }
      .lab-badge.subtitle_rejected { background: #fff3e6; color: #8a5200; }
      .lab-badge.thumbnail_generated { background: #f5f5f5; color: #666; }
      .lab-badge.thumbnail_approved { background: #e9f7f4; color: #166f62; }
      .lab-badge.thumbnail_rejected { background: #fff3e6; color: #8a5200; }
      .lab-badge.disqualified,
      .lab-badge.rejected { background: #fff3e6; color: #8a5200; }
      .lab-badge.generated,
      .lab-badge.api_pending,
      .lab-badge.draft { background: #f5f5f5; color: #666; }
      .lab-badge.archived { background: #f3f3f3; color: #777; }
      .lab-chip-row {
        display: flex;
        flex-wrap: wrap;
        gap: 6px;
      }
      .lab-chip {
        display: inline-flex;
        align-items: center;
        padding: 4px 9px;
        border-radius: 999px;
        font-size: 12px;
        font-weight: 650;
        line-height: 1;
        white-space: nowrap;
      }
      .lab-chip.blue { background: #eaf1ff; color: #2e63b7; }
      .lab-chip.purple { background: #f1ebff; color: #6a44b8; }
      .lab-chip.orange { background: #fff0df; color: #b46406; }
      .lab-chip.green { background: #eaf7ea; color: #2f7a34; }
      .lab-chip.default { background: #f3f3f3; color: #555; }
      .lab-status-copy {
        margin-top: 12px;
        color: var(--muted);
        font-size: 13px;
      }
      .approved-subtitle-list {
        display: grid;
        gap: 8px;
      }
      .approved-subtitle-item {
        padding: 7px 10px;
        border-radius: 8px;
        background: #f7f7f7;
        line-height: 1.35;
      }
      .thumbnail-preview-grid {
        display: grid;
        gap: 18px;
        margin-top: 8px;
        max-width: 760px;
      }
      .thumbnail-preview-card {
        border: 1px solid var(--line);
        border-radius: 8px;
        background: #fff;
        padding: 14px 18px 18px;
        box-shadow: none;
        max-width: 760px;
      }
      .thumbnail-preview-card.approved {
        border-color: #dceedd;
        background: #fbfefb;
      }
      .thumbnail-preview-topbar {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 10px;
        margin-bottom: 12px;
      }
      .thumbnail-preview-topbar .checkbox {
        margin: 0;
      }
      .thumbnail-preview-shell {
        display: grid;
        grid-template-columns: minmax(0, 1fr) 170px;
        gap: 32px;
        align-items: center;
      }
      .thumbnail-preview-image-wrap {
        border-radius: 1px;
        overflow: hidden;
        background: #fff;
        display: flex;
        justify-content: flex-end;
        align-items: center;
      }
      .thumbnail-preview-image {
        width: 170px;
        height: 113px;
        object-fit: cover;
        display: block;
        border: 0;
      }
      .medium-preview-card {
        border: 0;
        border-radius: 0;
        padding: 0;
        background: transparent;
        min-width: 0;
      }
      .preview-kicker {
        font-size: 11px;
        font-weight: 700;
        letter-spacing: .02em;
        text-transform: uppercase;
        color: var(--green);
        margin-bottom: 8px;
      }
      .preview-title {
        font-size: 26px;
        line-height: 1.15;
        font-weight: 760;
        color: var(--ink);
        margin-bottom: 8px;
      }
      .preview-subtitle {
        color: var(--muted);
        font-size: 18px;
        line-height: 1.35;
      }
      .thumbnail-preview-card .shiny-input-container {
        width: 100%;
        margin-top: 12px;
        margin-bottom: 0;
      }
      .status-card {
        padding: 14px 16px;
        margin-bottom: 12px;
      }
      .status-card h3 {
        margin: 0 0 8px;
        font-size: 16px;
        font-weight: 760;
      }
      .status-card p,
      .status-card li {
        margin: 0;
        color: #333;
        line-height: 1.4;
        font-size: 13px;
      }
      .status-card .status-metric {
        color: var(--green);
        font-size: 24px;
        font-weight: 760;
        line-height: 1;
        margin-bottom: 8px;
      }
      .overview-metric-list {
        display: grid;
        gap: 14px;
      }
      .overview-metric {
        display: grid;
        grid-template-columns: 34px minmax(0, 1fr);
        gap: 10px;
        align-items: center;
      }
      .overview-metric-value {
        color: #2e63b7;
        font-size: 24px;
        font-weight: 760;
        line-height: 1;
      }
      .overview-metric-value.green { color: var(--green); }
      .empty-state {
        max-width: 760px;
        margin-top: 12px;
        padding: 18px 20px;
        color: var(--muted);
        font-size: 14px;
      }
      .step-placeholder {
        display: grid;
        gap: 12px;
      }
      .guide-section { border-bottom: 1px solid var(--line); padding: 10px 0 16px; }
      .guide-section:first-child { padding-top: 0; }
      .guide-section:last-child { border-bottom: 0; }
      .guide h3 { font-size: 16px; margin: 0 0 10px; font-weight: 750; }
      .guide p, .guide li { color: #333; line-height: 1.34; font-size: 13px; }
      .guide ul { padding-left: 18px; margin: 0; }
      .guide li { margin-bottom: 5px; }
      .guide .tip {
        background: var(--green-soft); border-radius: 8px; padding: 14px;
        position: absolute;
        left: 20px;
        right: 20px;
      }
      .done-state {
        border: 1px solid var(--line); border-radius: 8px; padding: 42px; margin-top: 28px; text-align: center;
      }
      @media (max-width: 1180px) {
        .app-shell { grid-template-columns: 86px minmax(520px, 1fr); }
        .guide { display: none; }
        .sidebar { padding: 24px 14px; }
        .nav-copy, .daily-goal { display: none; }
        .nav-item { grid-template-columns: 1fr; justify-items: center; padding: 12px 10px; }
      }
      @media (max-width: 820px) {
        .app-shell { display: block; }
        .sidebar { display: none; }
        .main { padding: 28px 18px; }
        .lab-grid { grid-template-columns: 1fr; }
        .article-card { grid-template-columns: 1fr; gap: 18px; padding: 24px 0 26px; }
        .article-card.thumbnail-only { grid-template-columns: 170px; }
        .article-card.text-only { grid-template-columns: 1fr; }
        .thumbnail-preview-shell { grid-template-columns: 1fr; gap: 18px; }
        .thumbnail-preview-grid { max-width: none; }
        .thumbnail-preview-card { padding: 14px 14px 16px; }
        .thumbnail-wrap { justify-content: flex-start; }
        .thumbnail-wrap .shiny-image-output,
        .thumbnail-wrap img,
        .thumbnail-placeholder,
        .thumbnail-preview-image { width: 100% !important; height: auto !important; aspect-ratio: 1.5; }
        .rating-buttons { gap: 8px; }
        .rating-buttons .btn { height: 52px; font-size: 21px; }
        .dimension-row { grid-template-columns: 1fr; }
        .dimension-buttons { justify-content: stretch; }
        .dimension-choice.btn { flex: 1; }
      }
    ")),
    tags$script(HTML(paste0(
      "window.__dimensionMode = ", if (is_dimension_mode) "true" else "false", ";\n",
      "
      function flashAndSubmitRating(score) {
        const button = document.getElementById('score_' + score);
        if (button) {
          document.querySelectorAll('.rating-buttons .btn').forEach(function(oneButton) {
            oneButton.classList.remove('rating-confirm');
          });
          button.classList.add('rating-confirm');
        }
        window.setTimeout(function() {
          Shiny.setInputValue('score_key', {score: Number(score), nonce: Date.now()}, {priority: 'event'});
        }, 70);
      }

      function flashAndSubmitDimension(field, value) {
        const selector = '.dimension-choice[data-field=\"' + field + '\"][data-value=\"' + value + '\"]';
        const button = document.querySelector(selector);
        if (button) {
          document.querySelectorAll('.dimension-choice').forEach(function(oneButton) {
            oneButton.classList.remove('dimension-confirm');
          });
          button.classList.add('dimension-confirm');
        }
        window.setTimeout(function() {
          Shiny.setInputValue('dimension_select', {
            field: field,
            value: value,
            nonce: Date.now()
          }, {priority: 'event'});
        }, 23);
      }

      function copyVerificationTitle(element) {
        if (!element) return;
        const title = element.getAttribute('data-copy-title') || element.textContent || '';
        const done = function() {
          element.classList.add('copied');
          const original = element.getAttribute('data-original-title') || title;
          element.setAttribute('data-original-title', original);
          element.textContent = 'Copied title';
          window.setTimeout(function() {
            element.classList.remove('copied');
            element.textContent = original;
          }, 850);
        };
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(title).then(done).catch(function() {
            window.prompt('Copy title', title);
          });
        } else {
          window.prompt('Copy title', title);
        }
      }

      function alignSideCardsToRatingPanel() {
        const ratingPanel = document.querySelector('.rating-panel');
        const sidebar = document.querySelector('.sidebar');
        const guide = document.querySelector('.guide');
        const dailyGoal = document.querySelector('.daily-goal');
        const tip = document.querySelector('.guide .tip');
        if (!ratingPanel || !sidebar || !guide || !dailyGoal || !tip) return;

        const ratingBottom = ratingPanel.getBoundingClientRect().bottom;
        const sidebarTop = sidebar.getBoundingClientRect().top;
        const guideTop = guide.getBoundingClientRect().top;
        const tipTop = Math.max(22, ratingBottom - guideTop - tip.offsetHeight);
        if (window.getComputedStyle(dailyGoal).position === 'absolute') {
          const dailyTop = Math.max(22, ratingBottom - sidebarTop - dailyGoal.offsetHeight);
          dailyGoal.style.top = dailyTop + 'px';
        }
        tip.style.top = tipTop + 'px';
      }

      function articleLabSyncSelections(groupName) {
        if (!groupName || typeof Shiny === 'undefined' || typeof Shiny.setInputValue !== 'function') return;
        const rows = Array.from(document.querySelectorAll('[data-selection-group=\"' + groupName + '\"]'));
        const selectedIds = rows
          .filter(function(row) {
            const checkbox = row.querySelector('input[type=\"checkbox\"]');
            return !!(checkbox && checkbox.checked);
          })
          .map(function(row) {
            return row.getAttribute('data-candidate-id');
          })
          .filter(function(value) {
            return !!value;
          });
        Shiny.setInputValue(groupName + '_selected_snapshot', selectedIds, { priority: 'event' });
      }

      window.articleLabSyncSelections = articleLabSyncSelections;

      let articleLabThumbnailTimer = null;

      function articleLabFormatDuration(totalSeconds) {
        totalSeconds = Math.max(0, Math.round(Number(totalSeconds) || 0));
        const minutes = Math.floor(totalSeconds / 60);
        const seconds = totalSeconds % 60;
        if (minutes <= 0) return seconds + ' sec';
        if (seconds === 0) return minutes + ' min';
        return minutes + ' min ' + seconds + ' sec';
      }

      function articleLabFormatClock(timestamp) {
        const date = new Date(timestamp);
        if (Number.isNaN(date.getTime())) return '';
        return date.toLocaleTimeString([], { hour: 'numeric', minute: '2-digit' });
      }

      function articleLabSetThumbnailStatus(copy) {
        const target = document.getElementById('article_lab_thumbnail_timer');
        if (target) target.textContent = copy || '';
      }

      function articleLabStartThumbnailTimer(payload) {
        if (articleLabThumbnailTimer) window.clearInterval(articleLabThumbnailTimer);
        const startedAt = payload && payload.started_at ? new Date(payload.started_at) : new Date();
        const totalExpected = Math.max(1, Number(payload && payload.total_expected) || 1);
        const estimateLabel = payload && payload.estimate_label ? payload.estimate_label : '';
        const upperSeconds = Math.max(1, Number(payload && payload.upper_seconds) || 1);
        const startedLabel = articleLabFormatClock(startedAt);
        const render = function() {
          const elapsedSeconds = Math.max(0, (Date.now() - startedAt.getTime()) / 1000);
          let copy = 'Generating thumbnails: requested ' + totalExpected + ' thumbnail' + (totalExpected === 1 ? '' : 's') + '.';
          if (startedLabel) copy += ' Started ' + startedLabel + '.';
          if (estimateLabel) copy += ' Initial estimate: ' + estimateLabel + '.';
          copy += ' Elapsed: ' + articleLabFormatDuration(elapsedSeconds) + '.';
          if (elapsedSeconds > upperSeconds) {
            copy += ' Still running, slower than the initial estimate. True completed/remaining progress is not available until this blocking OpenAI call finishes.';
          } else {
            copy += ' Waiting for OpenAI; true completed/remaining progress is not available during this blocking call.';
          }
          articleLabSetThumbnailStatus(copy);
        };
        render();
        articleLabThumbnailTimer = window.setInterval(render, 1000);
      }

      function articleLabStopThumbnailTimer(payload) {
        if (articleLabThumbnailTimer) {
          window.clearInterval(articleLabThumbnailTimer);
          articleLabThumbnailTimer = null;
        }
        articleLabSetThumbnailStatus(payload && payload.message ? payload.message : '');
      }

      function setWorkflowLayout(layoutName) {
        const appShell = document.querySelector('.app-shell');
        const main = document.querySelector('.main');
        const guide = document.querySelector('.guide');
        const wideSections = ['research_inbox', 'api_scoring', 'subtitle_generation', 'thumbnails'];
        const useWideLayout = wideSections.indexOf(layoutName) >= 0;
        if (appShell) appShell.classList.toggle('workflow-wide-layout', useWideLayout);
        if (main) main.classList.toggle('workflow-wide-main', useWideLayout);
        if (guide) guide.classList.toggle('guide-hidden', useWideLayout);
      }

      window.addEventListener('resize', function() {
        window.requestAnimationFrame(alignSideCardsToRatingPanel);
      });
      document.addEventListener('DOMContentLoaded', function() {
        window.setTimeout(alignSideCardsToRatingPanel, 250);
      });
      window.setInterval(alignSideCardsToRatingPanel, 300);

      document.addEventListener('click', function(event) {
        const verificationTitle = event.target && event.target.closest ? event.target.closest('.dimension-verification-title') : null;
        if (verificationTitle) {
          event.preventDefault();
          event.stopPropagation();
          event.stopImmediatePropagation();
          copyVerificationTitle(verificationTitle);
          return;
        }
        const dimensionButton = event.target && event.target.closest ? event.target.closest('.dimension-choice') : null;
        if (dimensionButton) {
          event.preventDefault();
          event.stopPropagation();
          event.stopImmediatePropagation();
          flashAndSubmitDimension(
            dimensionButton.getAttribute('data-field'),
            dimensionButton.getAttribute('data-value')
          );
          return;
        }
        const button = event.target && event.target.closest ? event.target.closest('.rating-buttons .btn') : null;
        if (!button || !button.id || !button.id.match(/^score_[1-5]$/)) return;
        event.preventDefault();
        event.stopPropagation();
        event.stopImmediatePropagation();
        flashAndSubmitRating(button.id.replace('score_', ''));
      }, true);

      function isTextEntryTarget(target) {
        if (!target) return false;
        const tag = (target.tagName || '').toLowerCase();
        return tag === 'textarea' ||
          tag === 'input' ||
          tag === 'select' ||
          target.isContentEditable === true;
      }

      document.addEventListener('keydown', function(event) {
        if (event.metaKey || event.ctrlKey || event.altKey) return;
        const inText = isTextEntryTarget(event.target);
        const rawKey = event.key || '';
        const rawCode = event.code || '';
        const digitMatch = rawKey.match(/^[1-5]$/) || rawCode.match(/^(Digit|Numpad)([1-5])$/);
        const digit = digitMatch ? Number(digitMatch[2] || digitMatch[0]) : null;
        const letter = rawKey.length === 1 ? rawKey.toLowerCase() : rawCode.replace(/^Key/, '').toLowerCase();
        const homeRowRatings = {a: 1, s: 2, d: 3, f: 4, j: 5};

        if (inText) {
          const note = document.getElementById('note');
          const noteFocused = note && document.activeElement === note;
          if (noteFocused && (rawKey === 'Enter' || rawCode === 'Enter' || rawKey === 'Escape' || rawCode === 'Escape')) {
            event.preventDefault();
            note.dispatchEvent(new Event('change', { bubbles: true }));
            note.blur();
          }
          return;
        }

        if (window.__dimensionMode) {
          if (digit !== null) {
            event.preventDefault();
            const keyField = document.querySelector('.dimension-choice')?.getAttribute('data-field');
            const flagValueMap = {'1': 'yes', '2': 'unsure', '3': 'no'};
            const numericButton = document.querySelector('.dimension-choice[data-value=\"' + digit + '\"]');
            const flagButton = document.querySelector('.dimension-choice[data-value=\"' + flagValueMap[String(digit)] + '\"]');
            const targetButton = numericButton || flagButton;
            if (keyField && targetButton) targetButton.classList.add('dimension-confirm');
            window.setTimeout(function() {
              Shiny.setInputValue('dimension_key', {key: String(digit), nonce: Date.now()}, {priority: 'event'});
            }, 23);
          } else if (['a', 's', 'd', 'f', 'j'].indexOf(letter) >= 0) {
            event.preventDefault();
            const keyField = document.querySelector('.dimension-choice')?.getAttribute('data-field');
            const numericValueMap = {a: '1', s: '2', d: '3', f: '4', j: '5'};
            const flagValueMap = {s: 'yes', d: 'unsure', j: 'no'};
            const numericButton = document.querySelector('.dimension-choice[data-value=\"' + numericValueMap[letter] + '\"]');
            const flagButton = document.querySelector('.dimension-choice[data-value=\"' + flagValueMap[letter] + '\"]');
            const targetButton = numericButton || flagButton;
            if (keyField && targetButton) targetButton.classList.add('dimension-confirm');
            window.setTimeout(function() {
              Shiny.setInputValue('dimension_key', {key: letter, nonce: Date.now()}, {priority: 'event'});
            }, 23);
          } else if (rawKey === ' ' || rawCode === 'Space') {
            event.preventDefault();
            Shiny.setInputValue('skip_key', Date.now(), {priority: 'event'});
          } else if (letter === 'u') {
            event.preventDefault();
            Shiny.setInputValue('undo_key', Date.now(), {priority: 'event'});
          } else if (letter === 'n') {
            event.preventDefault();
            const note = document.getElementById('note');
            if (note) note.focus();
          } else if (letter === 'r') {
            event.preventDefault();
            Shiny.setInputValue('dimension_reset_key', Date.now(), {priority: 'event'});
          } else if (letter === 'b' || rawKey === 'Backspace' || rawCode === 'Backspace') {
            event.preventDefault();
            Shiny.setInputValue('dimension_back_key', Date.now(), {priority: 'event'});
          }
          return;
        }

        if (digit !== null) {
          event.preventDefault();
          flashAndSubmitRating(digit);
        } else if (Object.prototype.hasOwnProperty.call(homeRowRatings, letter)) {
          event.preventDefault();
          flashAndSubmitRating(homeRowRatings[letter]);
        } else if (rawKey === ' ' || rawCode === 'Space') {
          event.preventDefault();
          Shiny.setInputValue('skip_key', Date.now(), {priority: 'event'});
        } else if (letter === 'u') {
          event.preventDefault();
          Shiny.setInputValue('undo_key', Date.now(), {priority: 'event'});
        } else if (letter === 'n') {
          event.preventDefault();
          const note = document.getElementById('note');
          if (note) note.focus();
        }
      });
      function handleClearRatingFocus(_) {
        document.querySelectorAll('.rating-buttons .btn').forEach(function(oneButton) {
          oneButton.classList.remove('rating-confirm');
        });
        if (document.activeElement && document.activeElement.blur) {
          document.activeElement.blur();
        }
        window.setTimeout(alignSideCardsToRatingPanel, 80);
        window.setTimeout(alignSideCardsToRatingPanel, 260);
      }
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler('clearRatingFocus', handleClearRatingFocus);
        Shiny.addCustomMessageHandler('setWorkflowLayout', setWorkflowLayout);
        Shiny.addCustomMessageHandler('articleLabStartThumbnailTimer', articleLabStartThumbnailTimer);
        Shiny.addCustomMessageHandler('articleLabStopThumbnailTimer', articleLabStopThumbnailTimer);
      } else {
        document.addEventListener('shiny:connected', function() {
          Shiny.addCustomMessageHandler('clearRatingFocus', handleClearRatingFocus);
          Shiny.addCustomMessageHandler('setWorkflowLayout', setWorkflowLayout);
          Shiny.addCustomMessageHandler('articleLabStartThumbnailTimer', articleLabStartThumbnailTimer);
          Shiny.addCustomMessageHandler('articleLabStopThumbnailTimer', articleLabStopThumbnailTimer);
        }, { once: true });
      }
    ")))
  ),
  div(
    class = "topbar",
    div(class = "brand", div(class = "brand-mark", "M"), div("Medium Preview Rating")),
    div(class = "top-actions", span("Focus mode"), span("Local SQLite"))
  ),
  div(
    class = "app-shell",
    tags$aside(
      class = "sidebar",
      uiOutput("sidebar_nav"),
      uiOutput("sidebar_status_card")
    ),
    tags$main(
      class = "main",
      uiOutput("main_panel")
    ),
    tags$aside(
      class = "guide",
      uiOutput("guide_content")
    )
  )
)

server <- function(input, output, session) {
  con <- connect_db()
  onStop(function() dbDisconnect(con))
  rating_session_id <- if (is_dimension_mode) NULL else resume_or_create_session(con, target_n = default_target_n)
  active_section <- reactiveVal("home")
  active_dimension <- reactiveVal(if (is_dimension_mode) first_incomplete_dimension(con) else NA_character_)
  current <- reactiveVal(NULL)
  shown_started_at <- reactiveVal(Sys.time())
  saved_article_lab_prompt <- reactiveVal(load_article_lab_prompt(con))
  article_lab_state <- reactiveValues(
    draft = NULL,
    draft_created_at = NULL,
    draft_meta = NULL,
    is_generating = FALSE,
    is_scoring = FALSE,
    is_generating_subtitles = FALSE,
    is_generating_thumbnails = FALSE,
    thumbnail_generation_started_at = NULL,
    thumbnail_generation_estimate = NULL,
    notice = NULL
  )
  article_lab_refresh <- reactiveVal(0L)

  observeEvent(input$research_summary_prompt_version, {
    updateTextAreaInput(
      session,
      "research_summary_api_prompt",
      value = load_research_summary_prompt(con, input$research_summary_prompt_version)
    )
  }, ignoreInit = FALSE)

  output$article_lab_prompt_save_button <- renderUI({
    current_prompt <- article_lab_input_multiline(input$article_lab_prompt) %||% article_lab_default_prompt
    saved_prompt <- article_lab_input_multiline(saved_article_lab_prompt()) %||% article_lab_default_prompt
    has_changes <- !identical(current_prompt, saved_prompt)
    actionButton(
      "article_lab_save_prompt",
      if (has_changes) "Save prompt" else "Prompt saved",
      class = if (has_changes) "lab-primary" else "lab-secondary",
      disabled = if (has_changes) NULL else "disabled"
    )
  })

  observeEvent(input$sidebar_nav, {
    valid_sections <- c("home", article_lab_workflow_sections, "settings")
    if (is.character(input$sidebar_nav) && input$sidebar_nav %in% valid_sections) {
      active_section(input$sidebar_nav)
      if (identical(input$sidebar_nav, "home")) refresh_current()
    }
  }, ignoreInit = TRUE)

  observe({
    session$sendCustomMessage("setWorkflowLayout", active_section())
  })

  refresh_current <- function() {
    item <- if (is_dimension_mode) {
      field <- isolate(active_dimension())
      if (is.na(field)) {
        NULL
      } else {
        loaded_item <- load_current_dimension_item(con, field)
        if (is.null(loaded_item)) {
          next_field <- next_incomplete_dimension_after(con, field)
          if (!is.na(next_field)) {
            active_dimension(next_field)
            loaded_item <- load_current_dimension_item(con, next_field)
          }
        }
        loaded_item
      }
    } else {
      prune_article_lab_candidates_from_session(con, rating_session_id)
      append_article_lab_candidates_to_session(con, rating_session_id)
      load_current_item(con, rating_session_id)
    }
    current(item)
    shown_started_at(Sys.time())
    if (is_dimension_mode) {
      updateTextAreaInput(session, "note", value = "")
    } else {
      updateTextInput(session, "note", value = "")
    }
    session$sendCustomMessage("clearRatingFocus", list())
  }

  refresh_current()

  counts <- reactive({
    invalidateLater(1000, session)
    if (is_dimension_mode) {
      field <- active_dimension()
      if (is.na(field)) {
        data.frame(total = 0L, completed = 0L, pending = 0L, skipped = 0L)
      } else {
        dimension_queue_counts(con, field)
      }
    } else {
      queue_counts(con, rating_session_id)
    }
  })

  candidate_stats <- reactive({
    invalidateLater(5000, session)
    if (is_dimension_mode) dimension_candidate_counts(con) else candidate_counts(con)
  })

  output$sidebar_nav <- renderUI({
    current_section <- active_section()
    nav_button <- function(section, icon, label, subtitle, enabled = TRUE) {
      tags$button(
        type = "button",
        class = paste("nav-item", if (identical(current_section, section)) "active" else ""),
        onclick = if (enabled) sprintf("Shiny.setInputValue('sidebar_nav', '%s', {priority: 'event'})", section) else NULL,
        disabled = if (!enabled) "disabled" else NULL,
        span(class = "nav-icon", icon),
        div(
          class = "nav-copy",
          div(class = "nav-title", label),
          div(class = "nav-subtitle", subtitle)
        )
      )
    }

    tagList(
      div(
        class = "sidebar-nav-group",
        nav_button("home", "\u2302", "Home", "Current rating workflow")
      ),
      div(
        class = "sidebar-nav-group",
        div(class = "sidebar-nav-label", "Article Lab"),
        nav_button("research_inbox", "R", "Research Inbox", "Track papers and article angles"),
        nav_button("summary", "S", "Summary", "Check paper summary"),
        nav_button("generate", "\u21bb", "Generate", "Generate & triage titles"),
        nav_button("api_scoring", "\u2699", "API Scoring", "Score with API & approve"),
        nav_button("subtitle_generation", "\u270d", "Subtitle Generation", "Generate subtitles"),
        nav_button("thumbnails", "\u25a7", "Thumbnails", "Generate thumbnails"),
        nav_button("outline", "\u2263", "Outline", "Create article outline"),
        nav_button("full_text", "\u270e", "Full Text", "Write full article"),
        nav_button("review_publish", "\u2611", "Review & Publish", "Review and publish")
      ),
      div(
        class = "sidebar-nav-group",
        nav_button("settings", "\u2699", "Settings", "App settings")
      )
    )
  })

  output$sidebar_status_card <- renderUI({
    if (article_lab_is_workflow_section(active_section()) || identical(active_section(), "settings")) {
      return(div(
        class = "daily-goal static-card",
        div(
          class = "article-lab-helper",
          strong("Article Lab helper"),
          p("Follow each step in order."),
          p(class = "shortcut-copy", "Manually approve at key stages.")
        )
      ))
    }

    div(
      class = "daily-goal",
      strong("Daily goal"),
      htmlOutput("sidebar_progress"),
      uiOutput("progress_bar"),
      uiOutput("sidebar_shortcuts")
    )
  })

  output$main_panel <- renderUI({
    current_section <- active_section()
    if (article_lab_is_workflow_section(current_section) || identical(current_section, "settings")) {
      page_meta <- article_lab_nav_meta(current_section)
      generate_panel <- tagList(
        div(
          class = "lab-card",
          h2("Generation prompt"),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_prompt",
              label = "Manual/default prompt",
              value = saved_article_lab_prompt(),
              width = "100%",
              height = "230px"
            )
          ),
          div(class = "lab-actions", uiOutput("article_lab_prompt_save_button")),
          div(
            class = "lab-grid",
            div(
              class = "lab-field",
              uiOutput("article_lab_research_summary_selector")
            ),
            div(
              class = "lab-field",
              numericInput("article_lab_batch_size", "Batch size", value = 12L, min = 1L, max = 25L, width = "100%")
            ),
            div(
              class = "lab-field",
              selectInput("article_lab_model", "Model", choices = article_lab_title_generation_model_choices, selected = article_lab_default_model, width = "100%")
            ),
            div(
              class = "lab-field",
              textInput("article_lab_seed_topic", "Optional seed/topic (manual mode)", value = "", width = "100%", placeholder = "Optional article idea or angle")
            ),
            div(
              class = "lab-field",
              selectInput(
                "article_lab_inspiration_source",
                "Optional inspiration source (manual mode)",
                choices = c("", "manual prompt", "top performing titles", "custom"),
                selected = "",
                width = "100%"
              )
            )
          ),
          uiOutput("article_lab_effective_prompt"),
          div(
            class = "lab-actions",
            uiOutput("article_lab_generate_button"),
            actionButton("article_lab_save", "Save batch", class = "lab-secondary"),
            actionButton("article_lab_clear", "Clear draft", class = "lab-secondary")
          ),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_manual_titles",
              "Add title ideas manually",
              value = "",
              width = "100%",
              height = "120px",
              placeholder = "Enter one title idea per line"
            )
          ),
          div(
            class = "lab-actions",
            actionButton("article_lab_add_manual_titles", "Add manual titles", class = "lab-secondary")
          ),
          uiOutput("article_lab_notice")
        ),
        div(
          class = "lab-card",
          h3("Current batch triage"),
          div(
            class = "lab-actions",
            checkboxInput("article_lab_generate_select_all", "Select all", value = FALSE),
            checkboxInput("article_lab_show_disqualified", "Show disqualified titles", value = FALSE),
            actionButton("article_lab_save_triage", "Save triage changes", class = "lab-secondary"),
            actionButton("article_lab_move_to_api_queue", "Move selected to API queue", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_generate');")
          ),
          uiOutput("article_lab_latest_titles")
        )
      )

      api_score_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(class = "lab-grid", uiOutput("article_lab_batch_selector"), div(class = "lab-field", selectInput("article_lab_score_model", "Model", choices = article_lab_score_model_choices, selected = article_lab_default_score_model, width = "100%")), div(class = "lab-field", textInput("article_lab_score_prompt_version", "Prompt version", value = article_lab_default_score_prompt_version, width = "100%")), div(class = "lab-field", textInput("article_lab_score_scope", "Scope", value = article_lab_default_score_scope, width = "100%"))),
          div(
            class = "lab-actions",
            uiOutput("article_lab_score_button"),
            actionButton("article_lab_refresh_scores", "Refresh", class = "lab-secondary")
          ),
          uiOutput("article_lab_score_effective_prompt"),
          div(class = "lab-status-copy", "Only titles in the API queue are scored."),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_score_sections")
      )

      subtitle_generation_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_subtitle_prompt",
              "Prompt",
              value = article_lab_default_subtitle_prompt,
              width = "100%",
              height = "190px"
            )
          ),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_subtitle_model", "Model", choices = article_lab_subtitle_model_choices, selected = article_lab_default_subtitle_model, width = "100%")),
            div(class = "lab-field", numericInput("article_lab_subtitle_variants_per_title", "Subtitle candidates per title", value = 4L, min = 1L, max = 8L, width = "100%"))
          ),
          div(
            class = "lab-actions",
            uiOutput("article_lab_subtitle_generate_button"),
            actionButton("article_lab_refresh_subtitles", "Refresh", class = "lab-secondary")
          ),
          uiOutput("article_lab_subtitle_effective_prompt"),
          tags$hr(class = "lab-divider"),
          div(
            class = "lab-grid",
            div(class = "lab-field", selectizeInput("article_lab_manual_subtitle_candidate_id", "Add manual subtitle for title", choices = character(), selected = NULL, width = "100%")),
            div(
              class = "lab-field",
              textAreaInput(
                "article_lab_manual_subtitle_text",
                "Manual subtitle idea(s)",
                value = "",
                width = "100%",
                height = "110px",
                placeholder = "Enter one subtitle idea per line"
              )
            )
          ),
          div(
            class = "lab-actions",
            actionButton("article_lab_add_manual_subtitles", "Add manual subtitle idea(s)", class = "lab-secondary")
          ),
          div(class = "lab-status-copy", "Generate subtitle variants for approved titles, then approve or reject candidates manually."),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_subtitle_sections")
      )

      thumbnail_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_thumbnail_prompt",
              "Prompt",
              value = article_lab_default_thumbnail_prompt,
              width = "100%",
              height = "170px"
            )
          ),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_thumbnail_model", "Responses generation model", choices = article_lab_thumbnail_model_choices, selected = article_lab_default_thumbnail_model, width = "100%")),
            div(class = "lab-field", numericInput("article_lab_thumbnail_variants_per_package", "Thumbnail candidates per package", value = article_lab_default_thumbnail_variants, min = 1L, max = 4L, width = "100%"))
          ),
          div(
            class = "lab-actions",
            uiOutput("article_lab_thumbnail_generate_button"),
            actionButton("article_lab_refresh_thumbnails", "Refresh", class = "lab-secondary")
          ),
          uiOutput("article_lab_thumbnail_effective_prompt"),
          div(id = "article_lab_thumbnail_timer", class = "lab-status-copy"),
          div(class = "lab-status-copy", "Generate thumbnail candidates for approved title/subtitle packages, then approve one preview card per package."),
          uiOutput("article_lab_notice")
        )
        ,
        uiOutput("article_lab_thumbnail_sections")
      )

      outline_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-field",
            textAreaInput("article_lab_outline_prompt", "Prompt", value = article_lab_default_outline_prompt, width = "100%", height = "150px")
          ),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_outline_model", "Model", choices = article_lab_outline_model_choices, selected = article_lab_default_outline_model, width = "100%"))
          ),
          div(
            class = "lab-actions",
            actionButton("article_lab_generate_outlines", "Generate selected outline(s)", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_outline_packages');"),
            actionButton("article_lab_save_outlines", "Save outline edits", class = "lab-secondary"),
            actionButton("article_lab_approve_outlines", "Approve selected outline(s)", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_outline_candidates');"),
            actionButton("article_lab_refresh_outlines", "Refresh", class = "lab-secondary")
          ),
          div(class = "lab-status-copy", "Generate an outline from approved packages, edit/review it here, then approve it to move the package to draft-ready."),
          uiOutput("article_lab_notice")
        ),
        article_lab_section_card(
          "Ready for Outline",
          "Approved title/subtitle/thumbnail packages are available here for the next drafting step.",
          article_lab_ready_for_outline_table_ui(article_lab_ready_for_outline_rows()),
          count = nrow(article_lab_ready_for_outline_rows())
        )
      )

      placeholder_panel <- function(copy) {
        div(
          class = "lab-card step-placeholder",
          p(copy),
          p(class = "shortcut-copy", "This step is present in the workflow navigation, but its deeper implementation is intentionally left untouched in this pass.")
        )
      }

      research_inbox_panel <- tagList(
        div(
          class = "lab-card",
          h2("Ranked Queue"),
          div(class = "lab-status-copy", "Ranked sources have a manual sort order. Use the buttons to move the selected ranked source."),
          div(class = "lab-grid", div(class = "lab-field", selectInput("research_source_status_filter", "Filter by status", choices = c("All" = "__all__", "new", "reading", "angle_ready", "used", "archived"), selected = "__all__", width = "100%"))),
          div(class = "lab-actions", actionButton("research_refresh", "Refresh", class = "lab-secondary"), actionButton("research_ranked_move_up", "Move selected up", class = "lab-secondary"), actionButton("research_ranked_move_down", "Move selected down", class = "lab-secondary"), actionButton("research_remove_from_ranked", "Remove selected from ranked queue", class = "lab-secondary")),
          DT::DTOutput("research_ranked_sources_table")
        ),
        div(
          class = "lab-card",
          h2("Selected Source / Angle Workspace"),
          uiOutput("research_selected_source_summary"),
          uiOutput("research_angle_workspace")
        ),
        div(
          class = "lab-card",
          h2("Unranked Sources"),
          div(class = "lab-status-copy", "Unranked sources have no manual sort order."),
          div(class = "lab-actions", actionButton("research_add_to_ranked", "Add selected to ranked queue", class = "lab-primary")),
          DT::DTOutput("research_unranked_sources_table")
        ),
        div(
          class = "lab-card",
          h3("New source"),
          div(class = "lab-grid", div(class = "lab-field", textInput("research_new_source_title", "Source title", width = "100%")), div(class = "lab-field", textInput("research_new_source_url", "Source URL", width = "100%")), div(class = "lab-field", textInput("research_new_pdf_url", "PDF URL", width = "100%")), div(class = "lab-field", numericInput("research_new_source_sort", "Sort order", value = NULL, width = "100%"))),
          div(class = "lab-field", textAreaInput("research_new_source_main_idea", "Main idea", width = "100%", height = "90px")),
          div(class = "lab-field", textAreaInput("research_new_source_abstract", "Abstract", width = "100%", height = "90px")),
          div(class = "lab-grid", div(class = "lab-field", textInput("research_new_source_status", "Status", value = "new", width = "100%")), div(class = "lab-field", textInput("research_new_source_name", "Source name", value = "", width = "100%"))),
          div(class = "lab-field", textAreaInput("research_new_source_notes", "Notes", width = "100%", height = "80px")),
          div(class = "lab-actions", actionButton("research_add_source", "Add source", class = "lab-primary"))
        ),
        uiOutput("article_lab_notice")
      )

      summary_panel <- tagList(
        div(
          class = "lab-card",
          h2("Research Summary"),
          div(class = "lab-field", uiOutput("research_summary_source_selector")),
          uiOutput("research_summary_selected_source"),
          uiOutput("research_summary_pdf_status"),
          div(
            class = "lab-actions",
            actionButton("research_download_pdf", "Download PDF", class = "lab-secondary"),
            actionButton("research_clear_pdf", "Clear/replace PDF", class = "lab-secondary")
          ),
          div(class = "lab-field", fileInput("research_pdf_upload", "Upload PDF manually", accept = c(".pdf", "application/pdf"), width = "100%")),
          uiOutput("research_summary_pdf_gate"),
          div(
            class = "lab-card",
            h3("API summary generation"),
            div(
              class = "lab-grid",
              div(class = "lab-field", selectInput("research_summary_model", "Model", choices = article_lab_research_summary_model_choices, selected = article_lab_default_research_summary_model, width = "100%")),
              div(class = "lab-field", selectInput("research_summary_prompt_version", "Prompt version", choices = article_lab_research_summary_prompt_version_choices, selected = article_lab_default_research_summary_prompt_version, width = "100%"))
            ),
            div(class = "lab-field lab-editor-textarea", textAreaInput("research_summary_api_prompt", "API prompt", value = article_lab_default_research_summary_prompt, width = "100%", height = "260px")),
            uiOutput("research_summary_effective_prompt"),
            div(class = "lab-actions", actionButton("research_generate_summary_draft", "Generate summary draft", class = "lab-primary"))
          ),
          div(
            class = "lab-field lab-editor-textarea",
            textAreaInput("research_summary_text", "Summary text", value = research_summary_template, width = "100%", height = "620px")
          ),
          div(
            class = "lab-actions",
            actionButton("research_save_summary_draft", "Save summary draft", class = "lab-secondary"),
            actionButton("research_confirm_summary", "Mark summary confirmed", class = "lab-primary"),
            actionButton("research_send_summary_to_generate", "Send confirmed summary to Generate", class = "lab-secondary")
          ),
          uiOutput("article_lab_notice")
        )
      )

      page_body <- switch(
        current_section,
        research_inbox = research_inbox_panel,
        summary = summary_panel,
        generate = generate_panel,
        api_scoring = api_score_panel,
        subtitle_generation = subtitle_generation_panel,
        thumbnails = thumbnail_panel,
        outline = outline_panel,
        full_text = placeholder_panel("Full article drafting workflow routing is now exposed here."),
        review_publish = placeholder_panel("Final review and publish workflow routing is now exposed here."),
        settings = placeholder_panel("Settings remain available from the sidebar."),
        generate_panel
      )

      return(tagList(
        h1(page_meta$title %||% page_meta$nav_title),
        div(class = "page-subtitle", page_meta$subtitle %||% page_meta$nav_subtitle),
        page_body
      ))
    }

    tagList(
      h1("Medium Preview Rating"),
      htmlOutput("progress_line"),
      htmlOutput("mode_line"),
      uiOutput("v2_paused_warning"),
      uiOutput("v2_debug_banner"),
      div(class = "tabs", div(class = "tab active", "For you"), div(class = "tab", "Featured")),
      uiOutput("article_area"),
      uiOutput("rating_panel")
    )
  })

  output$progress_line <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    if (is_dimension_mode) {
      total <- ifelse(is.na(c$total[[1]]), 0, c$total[[1]])
      field <- active_dimension()
      if (is.na(field)) {
        HTML(sprintf("All <span class='current'>%s</span> active dimension passes complete", length(active_dimension_fields)))
      } else if (completed >= total && total > 0) {
        HTML(sprintf("Dimension complete: <span class='current'>%s</span> · %s / %s", dimension_labels[[field]], completed, total))
      } else {
        HTML(sprintf("Dimension progress: <span class='current'>%s</span> / %s", completed + 1L, total))
      }
    } else {
      remaining <- stats$remaining_unrated[[1]]
      total <- completed + remaining
      HTML(sprintf("Article <span class='current'>%s</span> / %s", completed + 1L, total))
    }
  })

  output$mode_line <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    pending <- ifelse(is.na(c$pending[[1]]), 0, c$pending[[1]])
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    if (is_dimension_mode) {
      field <- active_dimension()
      active_label <- if (is.na(field)) "all complete" else field
      if (is_dimension_v2_mode) {
        return(HTML(sprintf(
          "<div class='mode-line'><strong>Mode:</strong> %s · active dimension %s · cohort rows %s · active dimensions %s · dimension progress %s done / %s pending · overall manual ratings %s / %s complete</div>",
          rating_mode,
          active_label,
          stats$total_cohort_rows[[1]],
          stats$total_dimensions[[1]],
          completed,
          pending,
          stats$completed_dimensions[[1]],
          stats$total_dimensions[[1]]
        )))
      }
      HTML(sprintf(
        "<div class='mode-line'><strong>Mode:</strong> %s · active dimension %s · cohort rows %s · usable local thumbnails %s · dimension progress %s done / %s pending · overall %s / %s dimensions complete</div>",
        rating_mode,
        active_label,
        stats$total_cohort_rows[[1]],
        stats$usable_local_thumbnails[[1]],
        completed,
        pending,
        stats$completed_dimensions[[1]],
        stats$total_dimensions[[1]]
      ))
    } else {
      HTML(sprintf(
        "<div class='mode-line'><strong>Mode:</strong> unrated thumbnails only · thumbnail candidates %s · already rated %s · remaining unrated %s · session %s done / %s pending</div>",
        stats$total_thumbnail_candidates[[1]],
        stats$already_rated[[1]],
        stats$remaining_unrated[[1]],
        completed,
        pending
      ))
    }
  })

  output$v2_paused_warning <- renderUI({
    if (!is_dimension_v2_mode) return(NULL)
    NULL
  })

  output$v2_debug_banner <- renderUI({
    if (!is_dimension_v2_mode) return(NULL)
    item <- current()
    field <- active_dimension()
    c <- counts()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- ifelse(is.na(c$total[[1]]), 0, c$total[[1]])
    if (is.na(field) || (total > 0 && completed >= total)) return(NULL)
    if (is.null(item)) {
      return(div(
        class = "v2-debug-banner error",
        strong("dimensions_v2 manifest render debug: "),
        "no current manifest item"
      ))
    }
    info <- v2_render_info(item)
    local_basename <- basename(first_value(item, "local_thumbnail_path_abs", first_value(item, "local_thumbnail_path")))
    short_hash <- function(x) {
      value <- clean_text(x)
      if (length(value) == 0 || is.na(value[[1]])) return("NA")
      substr(value[[1]], 1, 12)
    }
    div(
      class = paste("v2-debug-banner", if (isTRUE(info$valid)) "" else "error"),
      strong("dimensions_v2 manifest render debug: "),
      paste0(
        "queue_position=", first_value(item, "queue_position"),
        " | article_id=", first_value(item, "article_id"),
        " | medium_post_id=", first_value(item, "medium_post_id"),
        " | canonical_article_key=", first_value(item, "canonical_article_key"),
        " | image=", local_basename,
        " | thumbnail_status=", first_value(item, "thumbnail_status"),
        " | hash_matches_manifest=", first_value(item, "hash_matches_manifest"),
        " | image_sha256=", short_hash(first_value(item, "image_sha256")),
        " | current_image_sha256=", short_hash(first_value(item, "current_image_sha256")),
        " | render_valid=", isTRUE(info$valid),
        " | render_reason=", info$reason,
        " | active_dimension=", field
      )
    )
  })

  output$sidebar_progress <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- if (is_dimension_mode) {
      ifelse(is.na(c$total[[1]]), 0, c$total[[1]])
    } else {
      completed + stats$remaining_unrated[[1]]
    }
    HTML(sprintf("<span class='num'>%s</span> / %s", completed, total))
  })

  output$progress_bar <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- if (is_dimension_mode) {
      max(1, ifelse(is.na(c$total[[1]]), 0, c$total[[1]]))
    } else {
      max(1, completed + stats$remaining_unrated[[1]])
    }
    div(
      class = "progress-track",
      div(class = "progress-fill", style = sprintf("width: %.1f%%;", 100 * completed / total))
    )
  })

  output$sidebar_shortcuts <- renderUI({
    if (is_dimension_mode) {
      field <- active_dimension()
      text <- if (!is.na(field) && field == "ai_low_effort_flag") {
        "A/S/J flag, Space skip, U undo"
      } else {
        "A/S/D/F/J rate, Space skip, U undo"
      }
      div(class = "shortcut-copy", text)
    } else {
      div(class = "shortcut-copy", "A/S/D/F/J rate, Space skip, U undo")
    }
  })

  article_lab_saved_batch <- reactive({
    article_lab_refresh()
    load_latest_article_lab_batch(con)
  })

  article_lab_batches <- reactive({
    article_lab_refresh()
    load_article_lab_batches(con)
  })

  observe({
    batches <- article_lab_batches()
    batch_choices <- if (nrow(batches) == 0) character() else setNames(
      batches$batch_id,
      sprintf("%s · %s", batches$batch_id, batches$created_at)
    )
    choices <- c("All batches" = article_lab_all_batches_value, batch_choices)
    selected <- isolate(input$article_lab_selected_batch)
    valid_values <- c(article_lab_all_batches_value, batches$batch_id)
    if (is.null(selected) || !nzchar(selected) || !(selected %in% valid_values)) {
      selected <- article_lab_all_batches_value
    }
    updateSelectInput(session, "article_lab_selected_batch", choices = choices, selected = selected)
  })

  article_lab_selected_batch_id <- reactive({
    selected <- clean_text(input$article_lab_selected_batch)
    if (length(selected) > 0 && !is.na(selected[[1]])) return(selected[[1]])
    batch <- article_lab_saved_batch()
    if (is.null(batch)) return(NA_character_)
    batch$batch_id[[1]]
  })

  article_lab_selected_batch_candidates <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_candidates_for_batch(con, batch_id)
    article_lab_normalize_candidate_rows(rows)
  })

  article_lab_generate_candidates <- reactive({
    article_lab_refresh()
    batch <- article_lab_saved_batch()
    if (is.null(batch) || nrow(batch) == 0) return(data.frame())
    rows <- load_article_lab_candidates_for_batch(con, batch$batch_id[[1]])
    rows <- article_lab_normalize_candidate_rows(rows)
    show_disqualified <- isTRUE(input$article_lab_show_disqualified %||% FALSE)
    keep_statuses <- if (show_disqualified) c("generated", "disqualified") else "generated"
    rows <- rows[rows$normalized_status %in% keep_statuses, , drop = FALSE]
    rows[order(rows$created_at, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_overview_stats <- reactive({
    article_lab_refresh()
    article_lab_overview(con)
  })

  article_lab_scoring_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_scoring_rows(
      con,
      batch_id = batch_id,
      model = input$article_lab_score_model %||% article_lab_default_score_model,
      prompt_version = input$article_lab_score_prompt_version %||% article_lab_default_score_prompt_version,
      scope = input$article_lab_score_scope %||% article_lab_default_score_scope
    )
    rows <- article_lab_normalize_candidate_rows(rows)
    rows <- rows[rows$normalized_status %in% c("ready_for_api_scoring", "api_pending", "api_scored"), , drop = FALSE]
    rows[order(rows$created_at, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_queue_rows <- reactive({
    rows <- article_lab_scoring_rows()
    rows[rows$normalized_status %in% c("ready_for_api_scoring", "api_pending"), , drop = FALSE]
  })

  article_lab_scored_rows <- reactive({
    rows <- article_lab_scoring_rows()
    rows <- rows[rows$normalized_status == "api_scored", , drop = FALSE]
    if (nrow(rows) == 0) return(rows)
    combined_scores <- suppressWarnings(as.numeric(rows$combined_title_score))
    combined_scores[is.na(combined_scores)] <- -Inf
    rows[order(combined_scores, rows$created_at, rows$candidate_id, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  })

  article_lab_subtitle_target_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_subtitle_targets(con, batch_id)
    rows <- article_lab_normalize_candidate_rows(rows)
    rows <- rows[
      rows$normalized_status == "approved_for_subtitle" &
        suppressWarnings(as.integer(rows$generated_subtitle_n)) <= 0 &
        suppressWarnings(as.integer(rows$approved_subtitle_n)) <= 0,
      ,
      drop = FALSE
    ]
    rows[order(rows$created_at, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_pending_subtitle_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_subtitle_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows <- article_lab_normalize_candidate_rows(
      within(rows, {
        status <- parent_status
      })
    )
    rows <- rows[
      rows$subtitle_status == "generated" &
        rows$normalized_status %in% c("approved_for_subtitle", "ready_for_thumbnail"),
      ,
      drop = FALSE
    ]
    title_sort <- tolower(ifelse(is.na(rows$title), "", rows$title))
    created_sort <- xtfrm(rows$created_at)
    subtitle_sort <- xtfrm(rows$subtitle_id)
    rows[order(title_sort, -created_sort, -subtitle_sort), , drop = FALSE]
  })

  article_lab_thumbnail_package_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_thumbnail_packages(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    title_sort <- tolower(ifelse(is.na(rows$title), "", rows$title))
    subtitle_sort <- tolower(ifelse(is.na(rows$subtitle), "", rows$subtitle))
    rows[order(title_sort, subtitle_sort, decreasing = FALSE), , drop = FALSE]
  })

  research_refresh <- reactiveVal(0L)
  selected_research_source_id <- reactiveVal(NA_integer_)

  research_ranked_sources <- reactive({
    research_refresh()
    load_research_sources(con, input$research_source_status_filter %||% "__all__", ranked = TRUE)
  })

  research_unranked_sources <- reactive({
    research_refresh()
    load_research_sources(con, input$research_source_status_filter %||% "__all__", ranked = FALSE)
  })

  research_summary_sources <- reactive({
    research_refresh()
    load_research_sources(con, "__all__", ranked = NULL)
  })

  confirmed_research_summaries <- reactive({
    research_refresh()
    load_confirmed_research_summaries(con)
  })

  selected_generate_summary <- reactive({
    selected_summary_id <- research_input_integer(input$article_lab_research_summary_id)
    rows <- confirmed_research_summaries()
    if (is.na(selected_summary_id) || nrow(rows) == 0 || !(selected_summary_id %in% rows$summary_id)) return(data.frame())
    rows[match(selected_summary_id, rows$summary_id), , drop = FALSE]
  })

  article_lab_effective_generation_inputs <- reactive({
    selected_summary <- selected_generate_summary()
    if (nrow(selected_summary) > 0) {
      return(list(
        mode = "research_summary",
        prompt = research_summary_prompt(selected_summary),
        manual_prompt = input$article_lab_prompt %||% article_lab_default_prompt,
        seed_topic = selected_summary$source_title[[1]],
        inspiration_source = paste0("research_summary:", selected_summary$summary_id[[1]]),
        summary_id = selected_summary$summary_id[[1]],
        source_title = selected_summary$source_title[[1]] %||% ""
      ))
    }
    list(
      mode = "manual",
      prompt = input$article_lab_prompt %||% article_lab_default_prompt,
      manual_prompt = "",
      seed_topic = input$article_lab_seed_topic %||% "",
      inspiration_source = input$article_lab_inspiration_source %||% "",
      summary_id = NA_integer_,
      source_title = ""
    )
  })

  selected_research_source <- reactive({
    research_refresh()
    load_research_source_by_id(con, selected_research_source_id())
  })

  selected_research_source_summary <- reactive({
    research_refresh()
    load_research_source_summary(con, selected_research_source_id())
  })

  selected_research_pdf_asset <- reactive({
    research_refresh()
    load_research_pdf_asset(con, selected_research_source_id())
  })

  research_angles <- reactive({
    research_refresh()
    source <- selected_research_source()
    if (nrow(source) == 0) return(data.frame())
    load_research_angles(con, source$research_source_id[[1]])
  })

  selected_research_angle <- reactive({
    rows <- research_angles()
    selected <- input$research_angles_table_rows_selected
    if (nrow(rows) == 0 || length(selected) == 0) return(data.frame())
    rows[selected[[1]], , drop = FALSE]
  })

  article_lab_pending_thumbnail_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_thumbnail_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows <- article_lab_normalize_candidate_rows(
      within(rows, {
        status <- parent_status
      })
    )
    title_sort <- tolower(ifelse(is.na(rows$title), "", rows$title))
    subtitle_sort <- tolower(ifelse(is.na(rows$subtitle), "", rows$subtitle))
    created_sort <- xtfrm(rows$created_at)
    rows[order(title_sort, subtitle_sort, -created_sort), , drop = FALSE]
  })

  article_lab_ready_for_thumbnail_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_ready_for_thumbnail_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$batch_id, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_ready_for_outline_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_ready_for_outline_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$created_at, rows$thumbnail_id, decreasing = TRUE), , drop = FALSE]
  })

  collect_generate_triage_updates <- function(rows) {
    if (nrow(rows) == 0) return(list(updates = list(), selected_ids = character()))
    updates <- lapply(seq_len(nrow(rows)), function(i) {
      candidate_id <- rows$candidate_id[[i]]
      list(
        candidate_id = candidate_id,
        status = input[[article_lab_row_input_id("article_lab_generate_status", candidate_id)]] %||% rows$normalized_status[[i]],
        notes = input[[article_lab_row_input_id("article_lab_generate_notes", candidate_id)]] %||% rows$notes[[i]],
        selected = isTRUE(input[[article_lab_row_input_id("article_lab_generate_select", candidate_id)]])
      )
    })
    list(
      updates = updates,
      selected_ids = vapply(updates[vapply(updates, function(x) isTRUE(x$selected), logical(1))], `[[`, character(1), "candidate_id")
    )
  }

  collect_candidate_note_updates <- function(rows, prefix) {
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      candidate_id <- rows$candidate_id[[i]]
      list(
        candidate_id = candidate_id,
        notes = input[[article_lab_row_input_id(prefix, candidate_id)]] %||% rows$notes[[i]]
      )
    })
  }

  collect_subtitle_note_updates <- function(rows, prefix) {
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      subtitle_id <- rows$subtitle_id[[i]]
      list(
        subtitle_id = subtitle_id,
        notes = input[[article_lab_row_input_id(prefix, subtitle_id)]] %||% rows$notes[[i]]
      )
    })
  }

  collect_thumbnail_note_updates <- function(rows, prefix) {
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      thumbnail_id <- rows$thumbnail_id[[i]]
      list(
        thumbnail_id = thumbnail_id,
        notes = input[[article_lab_row_input_id(prefix, thumbnail_id)]] %||% rows$notes[[i]]
      )
    })
  }

  collect_outline_updates <- function(rows) {
    if (nrow(rows) == 0 || !("outline_id" %in% names(rows))) return(list())
    rows <- rows[!is.na(rows$outline_id) & nzchar(rows$outline_id) & rows$outline_status == "draft", , drop = FALSE]
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      outline_id <- rows$outline_id[[i]]
      list(
        outline_id = outline_id,
        outline_text = input[[article_lab_row_input_id("article_lab_outline_text", outline_id)]] %||% rows$outline_text[[i]],
        notes = input[[article_lab_row_input_id("article_lab_outline_notes", outline_id)]] %||% rows$outline_notes[[i]]
      )
    })
  }

  collect_selected_ids <- function(rows, prefix, snapshot_ids = NULL, key_col = "candidate_id") {
    if (nrow(rows) == 0) return(character())
    snapshot_ids <- clean_text(snapshot_ids)
    snapshot_ids <- unique(snapshot_ids[!is.na(snapshot_ids)])
    if (length(snapshot_ids) > 0) {
      return(rows[[key_col]][rows[[key_col]] %in% snapshot_ids])
    }
    selected <- vapply(seq_len(nrow(rows)), function(i) {
      row_id <- rows[[key_col]][[i]]
      isTRUE(input[[article_lab_row_input_id(prefix, row_id)]])
    }, logical(1))
    rows[[key_col]][selected]
  }

  article_lab_apply_select_all <- function(rows, prefix, value, key_col = "candidate_id") {
    for (cid in rows[[key_col]]) {
      updateCheckboxInput(
        session,
        inputId = article_lab_row_input_id(prefix, cid),
        value = value
      )
    }
  }

  observe({
    article_lab_generate_candidates()
    updateCheckboxInput(session, inputId = "article_lab_generate_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_generate_select_all, {
    rows <- article_lab_generate_candidates()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_generate_select", isTRUE(input$article_lab_generate_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_queue_rows()
    updateCheckboxInput(session, inputId = "article_lab_queue_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_queue_select_all, {
    rows <- article_lab_queue_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_queue_select", isTRUE(input$article_lab_queue_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_scored_rows()
    updateCheckboxInput(session, inputId = "article_lab_scored_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_scored_select_all, {
    rows <- article_lab_scored_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_scored_select", isTRUE(input$article_lab_scored_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_subtitle_target_rows()
    updateCheckboxInput(session, inputId = "article_lab_subtitle_title_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_subtitle_title_select_all, {
    rows <- article_lab_subtitle_target_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_subtitle_title_select", isTRUE(input$article_lab_subtitle_title_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_pending_subtitle_rows()
    updateCheckboxInput(session, inputId = "article_lab_subtitle_candidate_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_subtitle_candidate_select_all, {
    rows <- article_lab_pending_subtitle_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_subtitle_candidate_select", isTRUE(input$article_lab_subtitle_candidate_select_all), key_col = "subtitle_id")
  }, ignoreInit = TRUE)

  observeEvent(article_lab_selected_batch_id(), {
    choices <- article_lab_manual_subtitle_choice_map(
      article_lab_subtitle_target_rows(),
      article_lab_pending_subtitle_rows()
    )
    current_value <- article_lab_input_string(input$article_lab_manual_subtitle_candidate_id)
    selected_value <- if (length(choices) > 0L && length(current_value) == 1L && !is.na(current_value) && current_value %in% unname(unlist(choices, use.names = FALSE))) current_value else NULL
    updateSelectizeInput(
      session,
      inputId = "article_lab_manual_subtitle_candidate_id",
      choices = choices,
      selected = selected_value,
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observe({
    article_lab_thumbnail_package_rows()
    updateCheckboxInput(session, inputId = "article_lab_thumbnail_package_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_thumbnail_package_select_all, {
    rows <- article_lab_thumbnail_package_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_thumbnail_package_select", isTRUE(input$article_lab_thumbnail_package_select_all), key_col = "subtitle_id")
  }, ignoreInit = TRUE)

  observe({
    article_lab_pending_thumbnail_rows()
    updateCheckboxInput(session, inputId = "article_lab_thumbnail_candidate_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_thumbnail_candidate_select_all, {
    rows <- article_lab_pending_thumbnail_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_thumbnail_candidate_select", isTRUE(input$article_lab_thumbnail_candidate_select_all), key_col = "thumbnail_id")
  }, ignoreInit = TRUE)

  observeEvent(input$research_refresh, {
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_refresh_selected_source, {
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_ranked_sources_table_rows_selected, {
    rows <- research_ranked_sources()
    selected <- input$research_ranked_sources_table_rows_selected
    if (nrow(rows) == 0 || length(selected) == 0) return()
    selected_research_source_id(rows$research_source_id[[selected[[1]]]])
    DT::selectRows(DT::dataTableProxy("research_unranked_sources_table"), NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$research_unranked_sources_table_rows_selected, {
    rows <- research_unranked_sources()
    selected <- input$research_unranked_sources_table_rows_selected
    if (nrow(rows) == 0 || length(selected) == 0) return()
    selected_research_source_id(rows$research_source_id[[selected[[1]]]])
    DT::selectRows(DT::dataTableProxy("research_ranked_sources_table"), NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$research_summary_source_id, {
    selected_research_source_id(research_input_integer(input$research_summary_source_id))
  }, ignoreInit = TRUE)

  observeEvent(selected_research_source_summary(), {
    summary <- selected_research_source_summary()
    value <- if (nrow(summary) == 0) research_summary_template else summary$summary_text[[1]] %||% research_summary_template
    updateTextAreaInput(session, "research_summary_text", value = value)
  }, ignoreInit = FALSE)

  normalize_research_ranked_queue <- function() {
    ids <- dbGetQuery(con, "SELECT research_source_id FROM research_sources WHERE manual_sort_order IS NOT NULL ORDER BY manual_sort_order ASC, updated_at DESC")
    if (nrow(ids) == 0) return(invisible(NULL))
    timestamp <- now_utc()
    for (i in seq_len(nrow(ids))) {
      dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = ? WHERE research_source_id = ?", params = list(timestamp, i, ids$research_source_id[[i]]))
    }
    invisible(NULL)
  }

  observeEvent(input$research_add_to_ranked, {
    id <- research_input_integer(selected_research_source_id())
    if (is.na(id)) {
      article_lab_state$notice <- "Select an unranked source before adding it to the ranked queue."
      return(invisible(NULL))
    }
    current <- dbGetQuery(con, "SELECT manual_sort_order FROM research_sources WHERE research_source_id = ? LIMIT 1", params = list(id))
    if (nrow(current) == 0 || !is.na(current$manual_sort_order[[1]])) {
      article_lab_state$notice <- "Select an unranked source before adding it to the ranked queue."
      return(invisible(NULL))
    }
    max_sort <- dbGetQuery(con, "SELECT COALESCE(MAX(manual_sort_order), 0) AS max_sort FROM research_sources WHERE manual_sort_order IS NOT NULL")
    dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = ? WHERE research_source_id = ?", params = list(now_utc(), as.integer(max_sort$max_sort[[1]]) + 1L, id))
    normalize_research_ranked_queue()
    article_lab_state$notice <- "Source added to ranked queue."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_remove_from_ranked, {
    id <- research_input_integer(selected_research_source_id())
    if (is.na(id)) {
      article_lab_state$notice <- "Select a ranked source before removing it from the ranked queue."
      return(invisible(NULL))
    }
    current <- dbGetQuery(con, "SELECT manual_sort_order FROM research_sources WHERE research_source_id = ? LIMIT 1", params = list(id))
    if (nrow(current) == 0 || is.na(current$manual_sort_order[[1]])) {
      article_lab_state$notice <- "Select a ranked source before removing it from the ranked queue."
      return(invisible(NULL))
    }
    dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = NULL WHERE research_source_id = ?", params = list(now_utc(), id))
    normalize_research_ranked_queue()
    article_lab_state$notice <- "Source removed from ranked queue."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  move_ranked_source <- function(direction) {
    id <- research_input_integer(selected_research_source_id())
    rows <- dbGetQuery(con, "SELECT research_source_id FROM research_sources WHERE manual_sort_order IS NOT NULL ORDER BY manual_sort_order ASC, updated_at DESC")
    if (is.na(id) || nrow(rows) < 2 || !(id %in% rows$research_source_id)) return(FALSE)
    index <- match(id, rows$research_source_id)
    swap_index <- index + direction
    if (is.na(swap_index) || swap_index < 1L || swap_index > nrow(rows)) return(FALSE)
    ids <- rows$research_source_id
    ids[c(index, swap_index)] <- ids[c(swap_index, index)]
    timestamp <- now_utc()
    for (i in seq_along(ids)) {
      dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = ? WHERE research_source_id = ?", params = list(timestamp, i, ids[[i]]))
    }
    TRUE
  }

  observeEvent(input$research_ranked_move_up, {
    if (move_ranked_source(-1L)) article_lab_state$notice <- "Ranked source moved up." else article_lab_state$notice <- "Select a ranked source that can move up."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_ranked_move_down, {
    if (move_ranked_source(1L)) article_lab_state$notice <- "Ranked source moved down." else article_lab_state$notice <- "Select a ranked source that can move down."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_add_source, {
    title <- research_input_value(input$research_new_source_title)
    if (is.na(title)) {
      article_lab_state$notice <- "Enter a source title before adding a research source."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    dbExecute(con, "
      INSERT INTO research_sources
        (created_at, updated_at, source_title, source_url, pdf_url, main_idea, abstract, source_type, source_name, manual_sort_order, status, notes)
      VALUES (?, ?, ?, ?, ?, ?, ?, 'paper', ?, ?, ?, ?)
    ", params = list(timestamp, timestamp, title, research_input_value(input$research_new_source_url), research_input_value(input$research_new_pdf_url), research_input_value(input$research_new_source_main_idea), research_input_value(input$research_new_source_abstract), research_input_value(input$research_new_source_name), research_input_integer(input$research_new_source_sort), research_input_default(input$research_new_source_status, "new"), research_input_value(input$research_new_source_notes)))
    article_lab_state$notice <- "Research source added."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_save_source, {
    rows <- selected_research_source()
    if (nrow(rows) == 0) {
      article_lab_state$notice <- "Select a source from the table to edit it."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    id <- rows$research_source_id[[1]]
    dbExecute(con, "
      UPDATE research_sources
      SET updated_at = ?, source_title = ?, source_url = ?, pdf_url = ?, main_idea = ?, abstract = ?, manual_sort_order = ?, status = ?, notes = ?
      WHERE research_source_id = ?
    ", params = list(timestamp, research_input_default(input$research_edit_source_title, rows$source_title[[1]]), research_input_value(input$research_edit_source_url), research_input_value(input$research_edit_pdf_url), research_input_value(input$research_edit_source_main), research_input_value(input$research_edit_source_abstract), research_input_integer(input$research_edit_source_sort), research_input_default(input$research_edit_source_status, "new"), research_input_value(input$research_edit_source_notes), id))
    article_lab_state$notice <- "Research source edits saved."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_add_angle, {
    source <- selected_research_source()
    source_id <- if (nrow(source) == 0) NA_integer_ else source$research_source_id[[1]]
    title <- research_input_value(input$research_new_angle_title)
    if (is.na(source_id) || is.na(title)) {
      article_lab_state$notice <- "Select a source and enter an angle title before creating an angle."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    dbExecute(con, "
      INSERT INTO research_article_angles
        (research_source_id, created_at, updated_at, angle_title, main_idea, manual_sort_order, status, notes)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(source_id, timestamp, timestamp, title, research_input_value(input$research_new_angle_main_idea), research_input_integer(input$research_new_angle_sort), research_input_default(input$research_new_angle_status, "idea"), research_input_value(input$research_new_angle_notes)))
    article_lab_state$notice <- "Research angle created."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_save_angle, {
    rows <- selected_research_angle()
    if (nrow(rows) == 0) return()
    timestamp <- now_utc()
    id <- rows$research_angle_id[[1]]
    dbExecute(con, "
      UPDATE research_article_angles
      SET updated_at = ?, angle_title = ?, main_idea = ?, manual_sort_order = ?, status = ?, notes = ?
      WHERE research_angle_id = ?
    ", params = list(timestamp, research_input_default(input$research_edit_angle_title, rows$angle_title[[1]]), research_input_value(input$research_edit_angle_main), research_input_integer(input$research_edit_angle_sort), research_input_default(input$research_edit_angle_status, "idea"), research_input_value(input$research_edit_angle_notes), id))
    article_lab_state$notice <- "Research angle edits saved."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  save_research_summary <- function(status) {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before saving a summary."
      return(NULL)
    }
    summary_text <- research_multiline_value(input$research_summary_text)
    if (is.na(summary_text)) {
      article_lab_state$notice <- "Enter summary text before saving."
      return(NULL)
    }
    timestamp <- now_utc()
    source_id <- source$research_source_id[[1]]
    if (identical(status, "draft")) {
      existing <- load_research_source_summary(con, source_id, status = "draft")
      if (nrow(existing) > 0) {
        dbExecute(con, "UPDATE research_source_summaries SET updated_at = ?, summary_text = ?, status = 'draft' WHERE summary_id = ?", params = list(timestamp, summary_text, existing$summary_id[[1]]))
        return(existing$summary_id[[1]])
      }
      dbExecute(con, "INSERT INTO research_source_summaries (research_source_id, created_at, updated_at, summary_text, status) VALUES (?, ?, ?, ?, 'draft')", params = list(source_id, timestamp, timestamp, summary_text))
      return(dbGetQuery(con, "SELECT last_insert_rowid() AS summary_id")$summary_id[[1]])
    }
    dbExecute(con, "INSERT INTO research_source_summaries (research_source_id, created_at, updated_at, summary_text, status, confirmed_at) VALUES (?, ?, ?, ?, 'confirmed', ?)", params = list(source_id, timestamp, timestamp, summary_text, timestamp))
    dbGetQuery(con, "SELECT last_insert_rowid() AS summary_id")$summary_id[[1]]
  }

  observeEvent(input$research_save_summary_draft, {
    summary_id <- save_research_summary("draft")
    if (!is.null(summary_id)) {
      article_lab_state$notice <- sprintf("Saved summary draft %s.", summary_id)
      research_refresh(research_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$research_confirm_summary, {
    summary_id <- save_research_summary("confirmed")
    if (!is.null(summary_id)) {
      article_lab_state$notice <- sprintf("Confirmed summary %s.", summary_id)
      research_refresh(research_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$research_download_pdf, {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before downloading a PDF."
      return(invisible(NULL))
    }
    source_id <- source$research_source_id[[1]]
    url <- research_pdf_source_url(source)
    if (is.na(url)) {
      save_research_pdf_asset(con, source_id, "failed", error = "No PDF URL found. Add a PDF URL or use manual upload.")
      article_lab_state$notice <- "No PDF URL found. Add a PDF URL or use manual upload."
      research_refresh(research_refresh() + 1L)
      return(invisible(NULL))
    }
    original_filename <- basename(strsplit(url, "[?#]", perl = TRUE)[[1]][[1]])
    if (!nzchar(original_filename) || identical(original_filename, "/")) original_filename <- NA_character_
    destination <- research_pdf_local_path(source_id, source$source_title[[1]], original_filename)
    temp_path <- tempfile(fileext = ".pdf")
    result <- tryCatch({
      utils::download.file(url, temp_path, mode = "wb", quiet = TRUE)
      if (!research_file_is_pdf(temp_path)) stop("Downloaded file is not a PDF.", call. = FALSE)
      if (!file.copy(temp_path, destination, overwrite = TRUE)) stop("Could not copy downloaded PDF into local folder.", call. = FALSE)
      sha <- research_pdf_sha256(destination)
      save_research_pdf_asset(con, source_id, "downloaded", source_url = url, local_path = destination, original_filename = original_filename, file_sha256 = sha, error = NA_character_)
      sprintf("Downloaded PDF to %s.", destination)
    }, error = function(e) {
      save_research_pdf_asset(con, source_id, "failed", source_url = url, local_path = NA_character_, original_filename = original_filename, file_sha256 = NA_character_, error = conditionMessage(e))
      sprintf("PDF download failed: %s", conditionMessage(e))
    }, finally = {
      if (file.exists(temp_path)) unlink(temp_path)
    })
    article_lab_state$notice <- result
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_pdf_upload, {
    source <- selected_research_source()
    upload <- input$research_pdf_upload
    if (nrow(source) == 0 || is.null(upload) || nrow(upload) == 0) return(invisible(NULL))
    source_id <- source$research_source_id[[1]]
    original_filename <- upload$name[[1]]
    destination <- research_pdf_local_path(source_id, source$source_title[[1]], original_filename)
    result <- tryCatch({
      if (!grepl("\\.pdf$", original_filename, ignore.case = TRUE) && !identical(upload$type[[1]], "application/pdf")) stop("Uploaded file is not a PDF.", call. = FALSE)
      if (!research_file_is_pdf(upload$datapath[[1]])) stop("Uploaded file content is not a PDF.", call. = FALSE)
      if (!file.copy(upload$datapath[[1]], destination, overwrite = TRUE)) stop("Could not copy uploaded PDF into local folder.", call. = FALSE)
      sha <- research_pdf_sha256(destination)
      save_research_pdf_asset(con, source_id, "uploaded", source_url = NA_character_, local_path = destination, original_filename = original_filename, file_sha256 = sha, error = NA_character_)
      sprintf("Uploaded PDF to %s.", destination)
    }, error = function(e) {
      save_research_pdf_asset(con, source_id, "failed", source_url = NA_character_, local_path = NA_character_, original_filename = original_filename, file_sha256 = NA_character_, error = conditionMessage(e))
      sprintf("PDF upload failed: %s", conditionMessage(e))
    })
    article_lab_state$notice <- result
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_clear_pdf, {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before clearing a PDF asset."
      return(invisible(NULL))
    }
    save_research_pdf_asset(con, source$research_source_id[[1]], "missing", source_url = NA_character_, local_path = NA_character_, original_filename = NA_character_, file_sha256 = NA_character_, error = NA_character_)
    article_lab_state$notice <- "PDF asset cleared. Download or upload a replacement PDF."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_generate_summary_draft, {
    source <- selected_research_source()
    asset <- selected_research_pdf_asset()
    prompt_version <- input$research_summary_prompt_version
    prompt_text <- article_lab_input_multiline(input$research_summary_api_prompt) %||% article_lab_default_research_summary_prompt
    save_research_summary_prompt(con, prompt_version, prompt_text)
    result <- tryCatch(
      research_summary_api_request(
        source = source,
        asset = asset,
        model = input$research_summary_model,
        prompt_version = prompt_version,
        prompt = prompt_text
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste("Summary generation failed:", conditionMessage(result))
      return(invisible(NULL))
    }

    updateTextAreaInput(session, "research_summary_text", value = result$summary_text)
    timestamp <- now_utc()
    source_id <- source$research_source_id[[1]]
    existing <- load_research_source_summary(con, source_id, status = "draft")
    if (nrow(existing) > 0) {
      dbExecute(con, "
        UPDATE research_source_summaries
        SET updated_at = ?, summary_text = ?, status = 'draft', model = ?, prompt_version = ?, raw_json = ?
        WHERE summary_id = ?
      ", params = list(timestamp, result$summary_text, result$model, result$prompt_version, result$raw_json, existing$summary_id[[1]]))
      summary_id <- existing$summary_id[[1]]
    } else {
      dbExecute(con, "
        INSERT INTO research_source_summaries
          (research_source_id, created_at, updated_at, summary_text, status, model, prompt_version, raw_json)
        VALUES (?, ?, ?, ?, 'draft', ?, ?, ?)
      ", params = list(source_id, timestamp, timestamp, result$summary_text, result$model, result$prompt_version, result$raw_json))
      summary_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS summary_id")$summary_id[[1]]
    }
    article_lab_state$notice <- sprintf("Generated and saved summary draft %s with model %s.", summary_id, result$model)
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  use_confirmed_summary_in_generate <- function(summary_id) {
    rows <- confirmed_research_summaries()
    summary_id_value <- research_input_integer(summary_id)
    if (is.na(summary_id_value) || nrow(rows) == 0 || !(summary_id_value %in% rows$summary_id)) return(FALSE)
    row <- rows[match(summary_id_value, rows$summary_id), , drop = FALSE]
    prompt <- research_summary_prompt(row)
    updateTextAreaInput(session, "article_lab_prompt", value = prompt)
    updateTextInput(session, "article_lab_seed_topic", value = row$source_title[[1]] %||% "")
    updateSelectInput(session, "article_lab_inspiration_source", selected = "")
    updateSelectizeInput(session, "article_lab_research_summary_id", selected = as.character(summary_id_value))
    active_section("generate")
    TRUE
  }

  observeEvent(input$research_send_summary_to_generate, {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before sending a summary to Generate."
      return(invisible(NULL))
    }
    confirmed <- load_research_source_summary(con, source$research_source_id[[1]], status = "confirmed")
    if (nrow(confirmed) == 0) {
      article_lab_state$notice <- "Confirm this source summary before sending it to Generate."
      return(invisible(NULL))
    }
    if (use_confirmed_summary_in_generate(confirmed$summary_id[[1]])) {
      article_lab_state$notice <- sprintf("Loaded confirmed summary %s into Generate.", confirmed$summary_id[[1]])
    }
  }, ignoreInit = TRUE)

  observeEvent(input$research_send_to_title_lab, {
    angle_id <- research_input_integer(input$research_send_to_title_lab)
    angle <- dbGetQuery(con, "SELECT * FROM research_article_angles WHERE research_angle_id = ? LIMIT 1", params = list(angle_id))
    if (nrow(angle) == 0 || is.na(angle$research_source_id[[1]])) return()
    source <- dbGetQuery(con, "SELECT * FROM research_sources WHERE research_source_id = ? LIMIT 1", params = list(angle$research_source_id[[1]]))
    if (nrow(source) == 0) return()
    prompt <- research_title_prompt(source, angle)
    inspiration <- paste0("research_angle:", angle_id)
    generated <- generate_title_candidates(con, prompt, batch_size = input$article_lab_batch_size %||% 12L, seed_topic = angle$angle_title[[1]], inspiration_source = inspiration, model = input$article_lab_model %||% article_lab_default_model)
    batch_id <- save_article_lab_batch(con, prompt, angle$angle_title[[1]], inspiration, input$article_lab_batch_size %||% 12L, generated$model %||% input$article_lab_model %||% article_lab_default_model, generated$titles$title, raw_json = generated$raw_json, generation_mode = generated$mode %||% "research_inbox")
    dbExecute(con, "UPDATE research_article_angles SET updated_at = ?, status = 'sent_to_title_lab', article_lab_batch_id = ? WHERE research_angle_id = ?", params = list(now_utc(), batch_id, angle_id))
    updateTextAreaInput(session, "article_lab_prompt", value = prompt)
    updateTextInput(session, "article_lab_seed_topic", value = angle$angle_title[[1]])
    updateSelectInput(session, "article_lab_inspiration_source", selected = "custom")
    active_section("generate")
    article_lab_state$notice <- sprintf("Sent research angle %s to Title Lab as batch %s.", angle_id, batch_id)
    article_lab_refresh(article_lab_refresh() + 1L)
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  output$article_lab_notice <- renderUI({
    notice <- article_lab_state$notice
    if (is.null(notice) || !nzchar(notice)) return(NULL)
    div(class = "lab-status-copy", notice)
  })

  output$research_summary_source_selector <- renderUI({
    rows <- research_summary_sources()
    choices <- if (nrow(rows) == 0) character() else setNames(
      rows$research_source_id,
      sprintf(
        "%s%s · %s",
        ifelse(is.na(rows$manual_sort_order), "", sprintf("#%s ", rows$manual_sort_order)),
        rows$source_title,
        rows$status
      )
    )
    selected <- selected_research_source_id()
    selectizeInput("research_summary_source_id", "Source", choices = choices, selected = selected, width = "100%")
  })

  output$research_summary_selected_source <- renderUI({
    source <- selected_research_source()
    if (nrow(source) == 0) return(div(class = "lab-status-copy", "Select a source to write or confirm a summary."))
    rank_copy <- if (is.na(source$manual_sort_order[[1]])) "Unranked" else sprintf("Rank #%s", source$manual_sort_order[[1]])
    main_idea <- research_input_value(source$main_idea[[1]])
    abstract <- research_input_value(source$abstract[[1]])
    div(
      class = "lab-status-copy",
      h3(source$source_title[[1]]),
      HTML(sprintf("<strong>%s</strong> · status: %s · %s", htmltools::htmlEscape(rank_copy), htmltools::htmlEscape(source$status[[1]] %||% ""), research_links(source$source_url[[1]], source$pdf_url[[1]]))),
      if (!is.na(main_idea)) p(strong("Main idea: "), main_idea),
      if (!is.na(abstract)) p(strong("Abstract: "), abstract)
    )
  })

  output$research_summary_pdf_status <- renderUI({
    source <- selected_research_source()
    if (nrow(source) == 0) return(NULL)
    asset <- selected_research_pdf_asset()
    status <- if (nrow(asset) == 0) "missing" else research_input_default(asset$status[[1]], "missing")
    status_label <- research_pdf_status_labels[[status]] %||% status
    local_path <- if (nrow(asset) == 0) NA_character_ else research_input_value(asset$local_path[[1]])
    source_url <- if (nrow(asset) == 0) research_pdf_source_url(source) else research_input_value(asset$source_url[[1]])
    error <- if (nrow(asset) == 0) NA_character_ else research_input_value(asset$error[[1]])
    div(
      class = "lab-card",
      h3("PDF"),
      p(strong("PDF status: "), status_label),
      if (!is.na(local_path)) p(strong("Local path: "), local_path),
      if (!is.na(source_url)) p(strong("Source URL used: "), source_url),
      if (identical(status, "failed") && !is.na(error)) p(strong("Error: "), error)
    )
  })

  output$research_summary_pdf_gate <- renderUI({
    asset <- selected_research_pdf_asset()
    ready <- nrow(asset) > 0 && asset$status[[1]] %in% c("downloaded", "uploaded") && !is.na(research_input_value(asset$local_path[[1]]))
    copy <- if (isTRUE(ready)) "PDF ready for summary generation." else "Download or upload a PDF before generating an API summary."
    div(class = "lab-status-copy", copy)
  })

  output$article_lab_research_summary_selector <- renderUI({
    rows <- confirmed_research_summaries()
    choices <- if (nrow(rows) == 0) character() else setNames(
      rows$summary_id,
      sprintf("%s · %s", rows$source_title, rows$confirmed_at %||% rows$updated_at)
    )
    empty_choice <- stats::setNames("", "")
    selectizeInput("article_lab_research_summary_id", "Research summary inspiration", choices = c(empty_choice, choices), selected = "", width = "100%")
  })

  output$article_lab_effective_prompt <- renderUI({
    effective <- article_lab_effective_generation_inputs()
    summary_mode <- identical(effective$mode, "research_summary")
    mode_copy <- if (summary_mode) {
      sprintf(
        "Research summary mode: Generate will use the manual/default prompt as title guidance, ignore the manual seed/topic and manual inspiration-source dropdown, and use confirmed summary %s (%s) as the article summary.",
        effective$summary_id,
        effective$source_title
      )
    } else {
      "Manual mode: Generate will use the manual/default prompt textarea, optional seed/topic, and optional inspiration-source dropdown below."
    }
    request_additions <- paste(
      sprintf("Batch size: %s", input$article_lab_batch_size %||% 12L),
      sprintf("Model: %s", input$article_lab_model %||% article_lab_default_model),
      sprintf("Seed topic: %s", article_lab_input_string(effective$seed_topic) %||% "(none)"),
      sprintf("Inspiration source: %s", article_lab_input_string(effective$inspiration_source) %||% "(none)"),
      sep = "\n"
    )
    example_titles <- if (identical(article_lab_input_string(effective$inspiration_source), "top performing titles")) {
      article_lab_top_title_examples(con, limit = 8L)
    } else {
      character()
    }
    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", mode_copy),
      tags$details(
        open = if (summary_mode) "open" else NULL,
        tags$summary("Show exact effective prompt"),
        h4("Title helper wrapper"),
        tags$pre(class = "lab-status-copy", paste(
          "You generate Medium-style article title candidates for personal finance and investing.",
          "Return valid JSON only in the shape {\"titles\": [\"...\", \"...\"]}.",
          sprintf("Return exactly %s titles.", input$article_lab_batch_size %||% 12L),
          sprintf("Every title must be at most %s characters, including spaces.", article_lab_title_max_chars),
          sprintf("Prefer %s-%s characters when possible. Do not make titles long unless the extra words clearly improve clarity or curiosity.", article_lab_title_preferred_min_chars, article_lab_title_preferred_max_chars),
          "Do not include explanations, numbering, markdown, or code fences.",
          "Do not copy any example title verbatim.",
          "Keep the titles credible, science-based, beginner-friendly, and not clickbait.",
          "If a title would exceed the limit, rewrite it shorter instead of truncating it.",
          sep = "\n"
        )),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        if (summary_mode && nzchar(trimws(effective$manual_prompt %||% ""))) tagList(
          h4("Manual/default prompt"),
          tags$pre(class = "lab-status-copy", effective$manual_prompt)
        ),
        if (length(example_titles) > 0) tagList(
          h4("Reference examples sent as inspiration"),
          tags$pre(class = "lab-status-copy", paste(sprintf("%s. %s", seq_along(example_titles), example_titles), collapse = "\n"))
        ),
        h4("Article summary"),
        tags$pre(class = "lab-status-copy", effective$prompt)
      )
    )
  })

  output$research_summary_effective_prompt <- renderUI({
    source <- selected_research_source()
    asset <- selected_research_pdf_asset()
    prompt_text <- article_lab_input_multiline(input$research_summary_api_prompt) %||% article_lab_default_research_summary_prompt
    pdf_status <- if (nrow(asset) == 0) "missing" else research_input_default(asset$status[[1]], "missing")
    local_pdf_path <- if (nrow(asset) == 0) NA_character_ else research_input_value(asset$local_path[[1]])
    resolved_pdf_path <- research_resolve_local_pdf_path(local_pdf_path)
    request_fields <- paste(
      sprintf("Model: %s", article_lab_input_string(input$research_summary_model) %||% article_lab_default_research_summary_model),
      sprintf("Prompt version: %s", article_lab_input_string(input$research_summary_prompt_version) %||% article_lab_default_research_summary_prompt_version),
      sprintf("PDF attachment status: %s", pdf_status),
      sprintf("PDF attachment filename/path: %s", resolved_pdf_path %||% "(none)"),
      sep = "\n"
    )
    metadata_text <- if (nrow(source) == 0) {
      "(No source selected. Select a source to see the exact source metadata sent with the PDF.)"
    } else {
      paste(
        "Source metadata:",
        sprintf("Research source ID: %s", source$research_source_id[[1]] %||% ""),
        sprintf("Source title: %s", article_lab_input_string(source$source_title[[1]]) %||% ""),
        sprintf("Source URL: %s", article_lab_input_string(source$source_url[[1]]) %||% ""),
        sprintf("PDF URL: %s", article_lab_input_string(source$pdf_url[[1]]) %||% ""),
        "Main idea:",
        article_lab_input_multiline(source$main_idea[[1]]) %||% "",
        "",
        "Abstract:",
        article_lab_input_multiline(source$abstract[[1]]) %||% "",
        "",
        "User prompt:",
        prompt_text,
        sep = "\n"
      )
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Summary generation sends the selected PDF as an input_file plus this text metadata/prompt payload."),
      tags$details(
        open = if (nrow(source) > 0) "open" else NULL,
        tags$summary("Show exact research summary API prompt"),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_fields),
        h4("Input text sent with PDF"),
        tags$pre(class = "lab-status-copy", metadata_text)
      )
    )
  })

  output$article_lab_score_effective_prompt <- renderUI({
    queue_rows <- article_lab_queue_rows()
    selected_ids <- collect_selected_ids(
      queue_rows,
      "article_lab_queue_select",
      snapshot_ids = input$article_lab_queue_selected_snapshot
    )
    selected_rows <- if (length(selected_ids) > 0 && nrow(queue_rows) > 0) {
      queue_rows[queue_rows$candidate_id %in% selected_ids, , drop = FALSE]
    } else {
      queue_rows[0, , drop = FALSE]
    }
    model <- article_lab_input_string(input$article_lab_score_model) %||% article_lab_default_score_model
    prompt_version <- article_lab_input_string(input$article_lab_score_prompt_version) %||% article_lab_default_score_prompt_version
    scope <- article_lab_input_string(input$article_lab_score_scope) %||% article_lab_default_score_scope
    request_fields <- paste(
      sprintf("Model: %s", model),
      sprintf("Prompt version: %s", prompt_version),
      sprintf("Scope: %s", scope),
      "Response format: strict JSON schema with curiosity, emotional_pull, medium_comment_potential, overall_article_potential, trust_risk, predicted_success_bucket, and short_reason.",
      sep = "\n"
    )
    user_prompts <- if (nrow(selected_rows) == 0) {
      "(No selected API-queue titles. Select title checkboxes to see the exact per-title user prompt that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_rows)), function(i) {
        paste(
          sprintf("candidate_id=%s | batch_id=%s", selected_rows$candidate_id[[i]], selected_rows$batch_id[[i]]),
          article_lab_score_user_prompt(prompt_version, scope, selected_rows$title[[i]]),
          sep = "\n\n"
        )
      }, character(1)), collapse = "\n\n---\n\n")
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "API scoring sends one request per selected title. Each request uses this system prompt plus the per-title user prompt below."),
      tags$details(
        open = if (nrow(selected_rows) > 0) "open" else NULL,
        tags$summary("Show exact title scoring API prompt"),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_fields),
        h4("System prompt"),
        tags$pre(class = "lab-status-copy", article_lab_score_system_prompt),
        h4("Per-title user prompt"),
        tags$pre(class = "lab-status-copy", user_prompts)
      )
    )
  })

  output$article_lab_subtitle_effective_prompt <- renderUI({
    targets <- article_lab_subtitle_target_rows()
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(targets$batch_id))
    has_summary <- nrow(summary_contexts) > 0
    variants_per_title <- max(1L, min(8L, suppressWarnings(as.integer(input$article_lab_subtitle_variants_per_title)) %||% 4L))
    base_prompt <- article_lab_input_multiline(input$article_lab_subtitle_prompt) %||% article_lab_default_subtitle_prompt
    request_additions <- paste(
      sprintf("Model: %s", article_lab_input_string(input$article_lab_subtitle_model) %||% article_lab_default_subtitle_model),
      sprintf("Subtitle candidates per title: %s", variants_per_title),
      sprintf("Max subtitle characters: %s", article_lab_subtitle_max_chars),
      sep = "\n"
    )
    title_list <- if (nrow(targets) == 0) {
      "(No eligible approved titles in the current batch filter.)"
    } else {
      paste(vapply(seq_len(nrow(targets)), function(i) {
        sprintf("%s. candidate_id=%s | batch_id=%s | title=%s", i, targets$candidate_id[[i]], targets$batch_id[[i]], targets$title[[i]])
      }, character(1)), collapse = "\n")
    }
    summary_copy <- if (has_summary) {
      "Subtitle generation will append the confirmed article summary attached to each title's source batch."
    } else {
      "No attached research summary was found for the eligible titles in the current batch filter. Subtitle generation will use the base prompt and titles only."
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", summary_copy),
      tags$details(
        open = if (has_summary) "open" else NULL,
        tags$summary("Show exact effective prompt"),
        h4("Subtitle prompt"),
        tags$pre(class = "lab-status-copy", base_prompt),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        h4("Titles"),
        tags$pre(class = "lab-status-copy", title_list),
        if (has_summary) tagList(
          h4("Attached article summaries"),
          tags$pre(class = "lab-status-copy", paste(vapply(seq_len(nrow(summary_contexts)), function(i) {
            paste(
              sprintf("Batch: %s", summary_contexts$batch_id[[i]]),
              sprintf("Summary ID: %s", summary_contexts$summary_id[[i]]),
              sprintf("Source title: %s", summary_contexts$source_title[[i]] %||% ""),
              summary_contexts$article_summary[[i]],
              sep = "\n"
            )
          }, character(1)), collapse = "\n\n---\n\n"))
        )
      )
    )
  })

  output$article_lab_thumbnail_effective_prompt <- renderUI({
    packages <- article_lab_thumbnail_package_rows()
    variants_per_package <- max(1L, min(4L, suppressWarnings(as.integer(input$article_lab_thumbnail_variants_per_package)) %||% article_lab_default_thumbnail_variants))
    base_prompt <- article_lab_input_multiline(input$article_lab_thumbnail_prompt) %||% article_lab_default_thumbnail_prompt
    selected_ids <- collect_selected_ids(
      packages,
      "article_lab_thumbnail_package_select",
      snapshot_ids = input$article_lab_thumbnail_packages_selected_snapshot,
      key_col = "subtitle_id"
    )
    selected_packages <- if (length(selected_ids) > 0 && nrow(packages) > 0) {
      packages[packages$subtitle_id %in% selected_ids, , drop = FALSE]
    } else {
      packages[0, , drop = FALSE]
    }
    request_additions <- paste(
      sprintf("Model: %s", article_lab_input_string(input$article_lab_thumbnail_model) %||% article_lab_default_thumbnail_model),
      "Image generation: Responses API built-in image_generation tool",
      sprintf("Thumbnail candidates per package: %s", variants_per_package),
      sep = "\n"
    )
    package_list <- if (nrow(selected_packages) == 0) {
      "(No selected eligible title/subtitle packages. Select package checkboxes to see the exact package context that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        sprintf(
          "%s. subtitle_id=%s | candidate_id=%s | batch_id=%s\nTitle: %s\nSubtitle: %s",
          i,
          selected_packages$subtitle_id[[i]],
          selected_packages$candidate_id[[i]],
          selected_packages$batch_id[[i]],
          selected_packages$title[[i]],
          selected_packages$subtitle[[i]]
        )
      }, character(1)), collapse = "\n\n")
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Thumbnail generation sends this prompt plus the selected title/subtitle package context to the selected Responses model, which calls the built-in image_generation tool."),
      tags$details(
        open = if (nrow(selected_packages) > 0) "open" else NULL,
        tags$summary("Show exact thumbnail API prompt"),
        h4("Thumbnail prompt"),
        tags$pre(class = "lab-status-copy", base_prompt),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        h4("Selected title/subtitle packages"),
        tags$pre(class = "lab-status-copy", package_list)
      )
    )
  })

  output$research_ranked_sources_table <- DT::renderDT({
    rows <- research_ranked_sources()
    display <- if (nrow(rows) == 0) {
      data.frame(research_source_id = integer(), Rank = integer(), Status = character(), Title = character(), Links = character(), Angles = integer(), check.names = FALSE)
    } else {
      source_title <- vapply(rows$source_title, research_input_default, character(1), default = "")
      data.frame(
        research_source_id = rows$research_source_id,
        Rank = seq_len(nrow(rows)),
        Status = rows$status,
        Title = sprintf('<span class="research-source-title" title="%s">%s</span>', htmltools::htmlEscape(source_title), htmltools::htmlEscape(vapply(source_title, research_truncate, character(1), max_chars = 240L))),
        Links = sprintf('<span class="research-source-links">%s</span>', mapply(research_links, rows$source_url, rows$pdf_url, USE.NAMES = FALSE)),
        Angles = rows$angles_count,
        check.names = FALSE
      )
    }
    DT::datatable(display, rownames = FALSE, escape = FALSE, class = "compact stripe hover research-source-table", selection = list(mode = "single", target = "row"), options = list(pageLength = 100, autoWidth = FALSE, order = list(), columnDefs = list(list(targets = 0, visible = FALSE), list(targets = 1, width = "6%"), list(targets = 2, width = "10%"), list(targets = 3, width = "72%"), list(targets = 4, width = "7%"), list(targets = 5, width = "5%"))))
  })

  output$research_unranked_sources_table <- DT::renderDT({
    rows <- research_unranked_sources()
    display <- if (nrow(rows) == 0) {
      data.frame(research_source_id = integer(), Status = character(), Title = character(), `Main idea` = character(), Links = character(), Angles = integer(), check.names = FALSE)
    } else {
      source_title <- vapply(rows$source_title, research_input_default, character(1), default = "")
      data.frame(
        research_source_id = rows$research_source_id,
        Status = rows$status,
        Title = sprintf('<span class="research-source-title" title="%s">%s</span>', htmltools::htmlEscape(source_title), htmltools::htmlEscape(vapply(source_title, research_truncate, character(1), max_chars = 220L))),
        `Main idea` = vapply(rows$main_idea, research_truncate, character(1), max_chars = 120L),
        Links = sprintf('<span class="research-source-links">%s</span>', mapply(research_links, rows$source_url, rows$pdf_url, USE.NAMES = FALSE)),
        Angles = rows$angles_count,
        check.names = FALSE
      )
    }
    DT::datatable(display, rownames = FALSE, escape = FALSE, class = "compact stripe hover research-source-table", selection = list(mode = "single", target = "row"), options = list(pageLength = 100, autoWidth = FALSE, order = list(), columnDefs = list(list(targets = 0, visible = FALSE), list(targets = 1, width = "10%"), list(targets = 2, width = "55%"), list(targets = 3, width = "25%"), list(targets = 4, width = "6%"), list(targets = 5, width = "4%"))))
  })

  output$research_selected_source_summary <- renderUI({
    row <- selected_research_source()
    if (nrow(row) == 0) return(div(class = "lab-status-copy", "Select a ranked or unranked source to edit details and create angles."))
    rank_label <- if (is.na(row$manual_sort_order[[1]])) "Unranked" else paste("Rank", row$manual_sort_order[[1]])
    main_idea <- research_truncate(row$main_idea[[1]], max_chars = 220L)
    div(
      class = "research-selected-summary",
      h3(row$source_title[[1]]),
      div(class = "research-source-links", HTML(research_links(row$source_url[[1]], row$pdf_url[[1]]))),
      div(class = "lab-status-copy", if (nzchar(main_idea)) main_idea else "No main idea saved yet."),
      div(class = "lab-status-copy", sprintf("Status: %s · %s", row$status[[1]], rank_label))
    )
  })

  output$research_angle_workspace <- renderUI({
    row <- selected_research_source()
    if (nrow(row) == 0) return(NULL)
    tagList(
      div(class = "lab-status-copy", "Lower angle sort number appears higher."),
      DT::DTOutput("research_angles_table"),
      h3("Create angle"),
      div(class = "lab-grid", div(class = "lab-field", textInput("research_new_angle_title", "Angle title", width = "100%")), div(class = "lab-field", numericInput("research_new_angle_sort", "Sort order", value = NULL, width = "100%")), div(class = "lab-field", textInput("research_new_angle_status", "Status", value = "idea", width = "100%"))),
      div(class = "lab-field", textAreaInput("research_new_angle_main_idea", "Angle main idea", width = "100%", height = "90px")),
      div(class = "lab-field", textAreaInput("research_new_angle_notes", "Notes", width = "100%", height = "80px")),
      div(class = "lab-actions", actionButton("research_add_angle", "Create angle from selected source", class = "lab-primary")),
      uiOutput("research_selected_angle_editor"),
      tags$details(
        class = "research-source-details",
        tags$summary("Edit source details"),
        uiOutput("research_selected_source_editor")
      )
    )
  })

  output$research_selected_source_editor <- renderUI({
    row <- selected_research_source()
    if (nrow(row) == 0) return(div(class = "lab-status-copy", "Select a ranked or unranked source to edit details and create angles."))
    div(
      div(class = "lab-status-copy", sprintf("Editing source %s", row$research_source_id[[1]])),
      div(class = "lab-grid", div(class = "lab-field", textInput("research_edit_source_title", "Source title", value = row$source_title[[1]], width = "100%")), div(class = "lab-field", textInput("research_edit_source_url", "Source URL", value = row$source_url[[1]] %||% "", width = "100%")), div(class = "lab-field", textInput("research_edit_pdf_url", "PDF URL", value = row$pdf_url[[1]] %||% "", width = "100%")), div(class = "lab-field", numericInput("research_edit_source_sort", "Sort order", value = research_numeric_default(row$manual_sort_order[[1]]), width = "100%")), div(class = "lab-field", textInput("research_edit_source_status", "Status", value = row$status[[1]], width = "100%"))),
      div(class = "lab-field", textAreaInput("research_edit_source_main", "Main idea", value = row$main_idea[[1]] %||% "", width = "100%", height = "90px")),
      div(class = "lab-field", textAreaInput("research_edit_source_abstract", "Abstract", value = row$abstract[[1]] %||% "", width = "100%", height = "90px")),
      div(class = "lab-field", textAreaInput("research_edit_source_notes", "Notes", value = row$notes[[1]] %||% "", width = "100%", height = "80px")),
      div(class = "lab-actions", actionButton("research_save_source", "Save selected source", class = "lab-primary"), actionButton("research_refresh_selected_source", "Refresh", class = "lab-secondary"))
    )
  })

  output$research_angles_table <- DT::renderDT({
    rows <- research_angles()
    display <- if (nrow(rows) == 0) {
      data.frame(research_angle_id = integer(), Sort = integer(), Status = character(), `Angle title` = character(), `Main idea` = character(), `Title Lab batch` = character(), Updated = character(), check.names = FALSE)
    } else {
      data.frame(
        research_angle_id = rows$research_angle_id,
        Sort = rows$manual_sort_order,
        Status = rows$status,
        `Angle title` = vapply(rows$angle_title, research_truncate, character(1), max_chars = 80L),
        `Main idea` = vapply(rows$main_idea, research_truncate, character(1), max_chars = 110L),
        `Title Lab batch` = rows$article_lab_batch_id,
        Updated = rows$updated_at,
        check.names = FALSE
      )
    }
    DT::datatable(display, rownames = FALSE, escape = TRUE, selection = list(mode = "single", target = "row"), options = list(pageLength = 8, scrollX = TRUE, order = list(), columnDefs = list(list(targets = 0, visible = FALSE))))
  })

  output$research_selected_angle_editor <- renderUI({
    source <- selected_research_source()
    if (nrow(source) == 0) return(div(class = "lab-status-copy", "Select a source to view and edit its angles."))
    row <- selected_research_angle()
    if (nrow(row) == 0) return(div(class = "lab-status-copy", "Select an angle from the table to edit it, or create a new angle below."))
    id <- row$research_angle_id[[1]]
    div(
      h3("Selected angle"),
      div(class = "lab-grid", div(class = "lab-field", textInput("research_edit_angle_title", "Angle title", value = row$angle_title[[1]], width = "100%")), div(class = "lab-field", numericInput("research_edit_angle_sort", "Sort order", value = research_numeric_default(row$manual_sort_order[[1]]), width = "100%")), div(class = "lab-field", textInput("research_edit_angle_status", "Status", value = row$status[[1]], width = "100%")), div(class = "lab-field", textInput("research_edit_angle_batch", "Article Lab batch", value = row$article_lab_batch_id[[1]] %||% "", width = "100%"))),
      div(class = "lab-field", textAreaInput("research_edit_angle_main", "Main idea", value = row$main_idea[[1]] %||% "", width = "100%", height = "80px")),
      div(class = "lab-field", textAreaInput("research_edit_angle_notes", "Notes", value = row$notes[[1]] %||% "", width = "100%", height = "70px")),
      div(class = "lab-actions", actionButton("research_save_angle", "Save angle edits", class = "lab-secondary"), tags$button(type = "button", class = "btn btn-default action-button lab-primary", onclick = sprintf("Shiny.setInputValue('research_send_to_title_lab', '%s', {priority: 'event'})", id), "Send to Title Lab"))
    )
  })

  output$article_lab_generate_button <- renderUI({
    if (isTRUE(article_lab_state$is_generating)) {
      tags$button(
        id = "article_lab_generate",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Generating..."
      )
    } else {
      actionButton("article_lab_generate", "Generate titles", class = "lab-primary")
    }
  })

  output$article_lab_batch_selector <- renderUI({
    batches <- article_lab_batches()
    batch_choices <- if (nrow(batches) == 0) character() else setNames(
      batches$batch_id,
      sprintf("%s · %s", batches$batch_id, batches$created_at)
    )
    choices <- c("All batches" = article_lab_all_batches_value, batch_choices)
    div(
      class = "lab-field",
      selectInput(
        "article_lab_selected_batch",
        "Batch selector",
        choices = choices,
        selected = article_lab_all_batches_value,
        width = "100%"
      )
    )
  })

  output$article_lab_score_button <- renderUI({
    if (isTRUE(article_lab_state$is_scoring)) {
      tags$button(
        id = "article_lab_score_titles",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Scoring..."
      )
    } else {
      actionButton("article_lab_score_titles", "Score selected API queue", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_queue');")
    }
  })

  output$article_lab_subtitle_generate_button <- renderUI({
    if (isTRUE(article_lab_state$is_generating_subtitles)) {
      tags$button(
        id = "article_lab_generate_subtitles",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Generating..."
      )
    } else {
      actionButton("article_lab_generate_subtitles", "Generate selected subtitle candidates", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_subtitle_titles');")
    }
  })

  output$article_lab_thumbnail_generate_button <- renderUI({
    if (isTRUE(article_lab_state$is_generating_thumbnails)) {
      tags$button(
        id = "article_lab_generate_thumbnails",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Generating..."
      )
    } else {
      actionButton("article_lab_generate_thumbnails", "Generate selected thumbnail candidates", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_packages');")
    }
  })

  output$article_lab_latest_titles <- renderUI({
    saved <- article_lab_generate_candidates()
    draft <- article_lab_state$draft
    if (!is.null(draft) && nrow(draft) > 0) {
      draft_rows <- data.frame(
        candidate_id = sprintf("draft_%02d", seq_len(nrow(draft))),
        title = draft$title,
        title_char_count = article_lab_title_length(draft$title),
        title_length_flag = article_lab_title_length_flag(article_lab_title_length(draft$title)),
        status = rep("draft", nrow(draft)),
        created_at = rep(article_lab_state$draft_created_at %||% now_utc(), nrow(draft)),
        batch_id = rep("(draft)", nrow(draft)),
        notes = rep(NA_character_, nrow(draft)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      rows <- draft_rows
    } else {
      rows <- saved
    }
    article_lab_generate_table_ui(rows)
  })

  output$article_lab_score_sections <- renderUI({
    queue_rows <- article_lab_queue_rows()
    scored_rows <- article_lab_scored_rows()

    tagList(
      article_lab_section_card(
        "1. API queue (waiting to be scored)",
        "These titles have not been scored yet.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_queue_select_all", "Select all", value = FALSE)
          ),
          article_lab_score_queue_table_ui(queue_rows),
          div(
            class = "lab-actions",
            actionButton("article_lab_archive_queue_titles", "Archive selected titles", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_queue');")
          )
        ),
        count = nrow(queue_rows)
      ),
      article_lab_section_card(
        "2. Scored titles awaiting approval",
        "These titles have been scored by the API. Select the ones you want to approve for subtitle generation.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_scored_select_all", "Select all", value = FALSE)
          ),
          article_lab_score_table_ui(scored_rows),
          div(
            class = "lab-actions",
            actionButton("article_lab_approve_for_subtitle", "Approve selected for subtitle generation", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_scored');"),
            actionButton("article_lab_archive_scored_titles", "Archive selected titles", onclick = "window.articleLabSyncSelections('article_lab_scored');")
          ),
          div(class = "lab-status-copy", "Approved titles will move to Subtitle Generation.")
        ),
        count = nrow(scored_rows)
      )
    )
  })

  output$article_lab_subtitle_sections <- renderUI({
    target_rows <- article_lab_subtitle_target_rows()
    subtitle_rows <- article_lab_pending_subtitle_rows()

    tagList(
      article_lab_section_card(
        "1. Titles awaiting subtitle generation",
        "These approved titles do not have active subtitle candidates yet.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_subtitle_title_select_all", "Select all", value = FALSE)
          ),
          article_lab_subtitle_target_table_ui(target_rows),
          div(
            class = "lab-actions",
            actionButton("article_lab_archive_subtitle_titles", "Archive selected titles", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_subtitle_titles');")
          )
        ),
        count = nrow(target_rows)
      ),
      article_lab_section_card(
        "2. Subtitle candidates awaiting approval",
        "Select subtitle candidates to approve for Thumbnails or reject without deleting them.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_subtitle_candidate_select_all", "Select all", value = FALSE)
          ),
          article_lab_subtitle_candidate_table_ui(subtitle_rows),
          div(
            class = "lab-actions",
            actionButton("article_lab_approve_subtitles", "Approve selected subtitle(s)", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_subtitle_candidates');"),
            actionButton("article_lab_reject_subtitles", "Reject selected", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_subtitle_candidates');")
          ),
          div(class = "lab-status-copy", "Approved subtitle candidates stay available as variants for the Thumbnails step.")
        ),
        count = nrow(subtitle_rows)
      )
    )
  })

  output$article_lab_thumbnail_sections <- renderUI({
    package_rows <- article_lab_thumbnail_package_rows()
    thumbnail_rows <- article_lab_pending_thumbnail_rows()

    tagList(
      article_lab_section_card(
        "1. Title/subtitle packages awaiting thumbnail generation",
        "These approved title/subtitle packages do not have active thumbnail candidates yet.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_thumbnail_package_select_all", "Select all", value = FALSE)
          ),
          article_lab_thumbnail_package_table_ui(package_rows),
          div(
            class = "lab-actions",
            actionButton("article_lab_dismiss_thumbnail_packages", "Dismiss selected packages", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_packages');")
          )
        ),
        count = nrow(package_rows)
      ),
      article_lab_section_card(
        "2. Thumbnail preview cards awaiting approval",
        "Select one preview card per title/subtitle package to approve for Outline, or reject candidates without deleting them.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_thumbnail_candidate_select_all", "Select all", value = FALSE)
          ),
          article_lab_thumbnail_candidate_grid_ui(thumbnail_rows),
          div(
            class = "lab-actions",
            actionButton("article_lab_approve_thumbnails", "Approve selected thumbnail", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_candidates');"),
            actionButton("article_lab_reject_thumbnails", "Reject selected", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_candidates');")
          ),
          div(class = "lab-status-copy", "Only one approved thumbnail is allowed per title/subtitle package. Approved packages move to Outline.")
        ),
        count = nrow(thumbnail_rows)
      )
    )
  })

  observeEvent(input$article_lab_generate, {
    article_lab_state$is_generating <- TRUE
    on.exit({
      article_lab_state$is_generating <- FALSE
    }, add = TRUE)

    selected_summary <- selected_generate_summary()
    effective_inputs <- article_lab_effective_generation_inputs()
    prompt_value <- effective_inputs$prompt
    manual_prompt_value <- effective_inputs$manual_prompt
    seed_topic_value <- effective_inputs$seed_topic
    inspiration_value <- effective_inputs$inspiration_source

    generated <- generate_title_candidates(
      con = con,
      prompt = prompt_value,
      batch_size = input$article_lab_batch_size,
      seed_topic = seed_topic_value,
      inspiration_source = inspiration_value,
      model = input$article_lab_model,
      manual_prompt = manual_prompt_value
    )
    article_lab_state$draft <- generated$titles
    article_lab_state$draft_created_at <- now_utc()
    article_lab_state$draft_meta <- modifyList(generated, list(
      prompt = prompt_value,
      manual_prompt = manual_prompt_value,
      seed_topic = seed_topic_value,
      inspiration_source = inspiration_value,
      notes_extra = if (nrow(selected_summary) > 0) paste(
        sprintf("Research summary: %s.", selected_summary$summary_id[[1]]),
        sprintf("Research source: %s.", selected_summary$research_source_id[[1]]),
        sprintf("Source title: %s.", selected_summary$source_title[[1]] %||% ""),
        sprintf("Source URL: %s.", selected_summary$source_url[[1]] %||% ""),
        sprintf("PDF URL: %s.", selected_summary$pdf_url[[1]] %||% "")
      ) else NULL
    ))
    if (identical(generated$mode, "api")) {
      example_copy <- if (isTRUE(generated$example_titles_used > 0)) {
        sprintf(" Used %s top-performing title examples as inspiration.", generated$example_titles_used)
      } else {
        ""
      }
      retry_copy <- if (isTRUE(generated$retry_used)) {
        " Strict mode triggered one automatic retry to shorten titles above the hard maximum."
      } else {
        ""
      }
      dropped_copy <- if (isTRUE(generated$dropped_n > 0)) {
        sprintf(" Dropped %s title%s over %s characters after strict validation.", generated$dropped_n, ifelse(generated$dropped_n == 1, "", "s"), article_lab_title_max_chars)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "Generated %s draft titles with the OpenAI API using model %s.%s%s%s Save the batch to persist it to SQLite.",
        nrow(generated$titles),
        generated$model %||% article_lab_default_model,
        example_copy,
        retry_copy,
        dropped_copy
      )
    } else {
      dropped_copy <- if (isTRUE(generated$dropped_n > 0)) {
        sprintf(" Dropped %s title%s over %s characters after strict validation.", generated$dropped_n, ifelse(generated$dropped_n == 1, "", "s"), article_lab_title_max_chars)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "API generation was unavailable, so the local stub helper generated %s draft titles instead.%s Reason: %s",
        nrow(generated$titles),
        dropped_copy,
        generated$fallback_reason %||% "unknown error"
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_add_manual_titles, {
    manual_titles <- article_lab_parse_manual_titles(input$article_lab_manual_titles)
    if (length(manual_titles) == 0) {
      article_lab_state$notice <- "Enter at least one manual title idea, with one title per line."
      return(invisible(NULL))
    }

    existing_titles <- if (!is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
      clean_text(article_lab_state$draft$title)
    } else {
      character()
    }
    new_manual_titles <- setdiff(manual_titles, existing_titles)
    if (length(new_manual_titles) == 0) {
      article_lab_state$notice <- "Those manual titles are already in the current draft."
      updateTextAreaInput(session, "article_lab_manual_titles", value = "")
      return(invisible(NULL))
    }
    combined_titles <- unique(c(existing_titles, manual_titles))
    normalized_titles <- article_lab_normalize_titles(combined_titles)
    if (length(normalized_titles) == 0) {
      article_lab_state$notice <- "No usable manual titles were provided."
      return(invisible(NULL))
    }

    article_lab_state$draft <- data.frame(
      row_number = seq_along(normalized_titles),
      title = normalized_titles,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    article_lab_state$draft_created_at <- article_lab_state$draft_created_at %||% now_utc()

    prior_mode <- article_lab_state$draft_meta$mode %||% NA_character_
    next_mode <- if (is.na(prior_mode) || !nzchar(prior_mode)) {
      "manual"
    } else if (identical(prior_mode, "manual")) {
      "manual"
    } else {
      "mixed"
    }
    article_lab_state$draft_meta <- modifyList(
      article_lab_state$draft_meta %||% list(),
      list(
        mode = next_mode,
        raw_json = article_lab_state$draft_meta$raw_json %||% NA_character_
      )
    )

    added_n <- sum(normalized_titles %in% new_manual_titles)
    over_limit_n <- sum(article_lab_title_length(new_manual_titles) > article_lab_title_mobile_safe_chars, na.rm = TRUE)
    length_copy <- if (over_limit_n > 0) {
      sprintf(" %s title%s exceed the %s-character mobile-safe length and were kept with their length flag.", over_limit_n, ifelse(over_limit_n == 1, "", "s"), article_lab_title_mobile_safe_chars)
    } else {
      ""
    }
    article_lab_state$notice <- sprintf(
      "Added %s manual title idea%s to the current draft.%s Save the batch to persist it to SQLite.",
      added_n,
      ifelse(added_n == 1, "", "s"),
      length_copy
    )
    updateTextAreaInput(session, "article_lab_manual_titles", value = "")
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_prompt, {
    prompt_text <- article_lab_input_multiline(input$article_lab_prompt)
    if (is.na(prompt_text)) {
      article_lab_state$notice <- "Enter a prompt before saving."
      return(invisible(NULL))
    }
    save_article_lab_prompt(con, prompt_text)
    saved_article_lab_prompt(prompt_text)
    article_lab_state$notice <- "Saved manual/default generation prompt."
  }, ignoreInit = TRUE)

  save_current_article_lab_draft <- function() {
    draft <- article_lab_state$draft
    draft_meta <- article_lab_state$draft_meta %||% list()
    if (is.null(draft) || nrow(draft) == 0) return(NULL)

    batch_id <- save_article_lab_batch(
      con,
      prompt = draft_meta$prompt %||% input$article_lab_prompt,
      seed_topic = draft_meta$seed_topic %||% input$article_lab_seed_topic,
      inspiration_source = draft_meta$inspiration_source %||% input$article_lab_inspiration_source,
      requested_batch_size = input$article_lab_batch_size,
      model = input$article_lab_model,
      titles = draft$title,
      raw_json = if (is.null(draft_meta$raw_json)) NA_character_ else draft_meta$raw_json,
      generation_mode = draft_meta$mode %||% "generated",
      enforce_max_chars = !((draft_meta$mode %||% "") %in% c("manual", "mixed")),
      notes_extra = draft_meta$notes_extra
    )
    saved_mode <- draft_meta$mode %||% "generated"
    article_lab_state$draft <- NULL
    article_lab_state$draft_created_at <- NULL
    article_lab_state$draft_meta <- NULL
    article_lab_refresh(article_lab_refresh() + 1L)

    list(batch_id = batch_id, mode = saved_mode, title_n = nrow(draft))
  }

  observeEvent(input$article_lab_save, {
    saved <- save_current_article_lab_draft()
    if (is.null(saved)) {
      article_lab_state$notice <- "Nothing to save yet. Generate a draft first."
      return(invisible(NULL))
    }

    article_lab_state$notice <- if (saved$mode %in% c("manual", "mixed")) {
      sprintf(
        "Saved batch %s. Candidates start as New and stay in Generate until you manually move selected titles to the API queue. Generation mode: %s. Overlength manual titles were preserved with their length flag.",
        saved$batch_id,
        saved$mode
      )
    } else {
      sprintf(
        "Saved batch %s. Candidates start as New and stay in Generate until you manually move selected titles to the API queue. Generation mode: %s. Max title length enforced: %s characters.",
        saved$batch_id,
        saved$mode,
        article_lab_title_max_chars
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_clear, {
    article_lab_state$draft <- NULL
    article_lab_state$draft_created_at <- NULL
    article_lab_state$draft_meta <- NULL
    article_lab_state$notice <- "Cleared the unsaved draft."
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_triage, {
    if (!is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
      saved <- save_current_article_lab_draft()
      article_lab_state$notice <- sprintf("Saved draft batch %s with %s title%s. You can now edit statuses or notes.", saved$batch_id, saved$title_n, ifelse(saved$title_n == 1, "", "s"))
      return(invisible(NULL))
    }
    rows <- article_lab_generate_candidates()
    payload <- collect_generate_triage_updates(rows)
    if (length(payload$updates) == 0) {
      article_lab_state$notice <- "No saved titles are visible in the current triage view."
      return(invisible(NULL))
    }
    article_lab_save_generate_triage(con, payload$updates)
    article_lab_state$notice <- sprintf("Saved triage updates for %s title%s.", length(payload$updates), ifelse(length(payload$updates) == 1, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_move_to_api_queue, {
    if (!is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
      draft <- article_lab_state$draft
      selected_indexes <- which(vapply(seq_len(nrow(draft)), function(i) {
        isTRUE(input[[article_lab_row_input_id("article_lab_generate_select", sprintf("draft_%02d", i))]])
      }, logical(1)))
      if (length(selected_indexes) == 0) {
        article_lab_state$notice <- "Select at least one draft title before moving it to the API queue."
        return(invisible(NULL))
      }
      saved <- save_current_article_lab_draft()
      selected_ids <- article_lab_candidate_id(saved$batch_id, selected_indexes)
      result <- article_lab_move_candidates_to_api_queue(con, selected_ids)
      article_lab_state$notice <- sprintf(
        "Saved draft batch %s and moved %s selected title%s to API queue. %s selected title%s were skipped because they were not eligible.",
        saved$batch_id,
        result$moved_n,
        ifelse(result$moved_n == 1, "", "s"),
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s")
      )
      article_lab_refresh(article_lab_refresh() + 1L)
      if (result$moved_n > 0) {
        updateSelectInput(session, "article_lab_selected_batch", selected = saved$batch_id)
        active_section("api_scoring")
      }
      return(invisible(NULL))
    }
    rows <- article_lab_generate_candidates()
    payload <- collect_generate_triage_updates(rows)
    article_lab_save_generate_triage(con, payload$updates)
    snapshot_selected_ids <- collect_selected_ids(
      rows,
      "article_lab_generate_select",
      snapshot_ids = input$article_lab_generate_selected_snapshot
    )
    selected_ids <- unique(c(payload$selected_ids, snapshot_selected_ids))
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one New title before moving it to the API queue."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_move_candidates_to_api_queue(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Moved %s selected title%s to API queue. %s selected title%s were skipped because they were disqualified or not eligible.",
      result$moved_n,
      ifelse(result$moved_n == 1, "", "s"),
      result$skipped_n,
      ifelse(result$skipped_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
    if (result$moved_n > 0) {
      if (length(result$batch_ids) == 1 && nzchar(result$batch_ids[[1]])) {
        updateSelectInput(session, "article_lab_selected_batch", selected = result$batch_ids[[1]])
      }
      active_section("api_scoring")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_score_titles, {
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) {
      article_lab_state$notice <- "Select a saved batch before scoring."
      return(invisible(NULL))
    }
    article_lab_state$is_scoring <- TRUE
    on.exit({
      article_lab_state$is_scoring <- FALSE
    }, add = TRUE)

    queue_rows <- article_lab_queue_rows()
    selected_ids <- collect_selected_ids(
      queue_rows,
      "article_lab_queue_select",
      snapshot_ids = input$article_lab_queue_selected_snapshot
    )
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(queue_rows, "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_scored_rows(), "article_lab_scored_notes"))
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-queue title before scoring."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    result <- tryCatch(
      article_lab_score_batch(
        con,
        batch_id = batch_id,
        model = input$article_lab_score_model,
        prompt_version = input$article_lab_score_prompt_version,
        scope = input$article_lab_score_scope,
        candidate_ids = selected_ids
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste("API scoring failed:", conditionMessage(result))
    } else {
      article_lab_state$notice <- if (result$scored_n > 0) {
        sprintf(
          "Scored %s selected API-queue title%s for %s using model %s, prompt %s, scope %s.%s%s",
          result$scored_n,
          ifelse(result$scored_n == 1, "", "s"),
          result$batch_label %||% paste("batch", batch_id),
          result$model %||% article_lab_default_score_model,
          result$prompt_version %||% article_lab_default_score_prompt_version,
          result$scope %||% article_lab_default_score_scope,
          if (result$used_existing_n > 0) sprintf(" %s used an existing saved API score.", result$used_existing_n) else "",
          if (result$failed_n > 0) sprintf(" %s failed and stayed in their previous status.", result$failed_n) else ""
        )
      } else {
        result$message %||% "No titles are currently waiting in the API queue for this selection."
      }
      article_lab_refresh(article_lab_refresh() + 1L)
      if (!is_dimension_mode && !is.null(rating_session_id) && !is.na(rating_session_id)) {
        prune_article_lab_candidates_from_session(con, rating_session_id)
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_for_subtitle, {
    scored_rows <- article_lab_scored_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_queue_rows(), "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(scored_rows, "article_lab_scored_notes"))
    selected_ids <- collect_selected_ids(
      scored_rows,
      "article_lab_scored_select",
      snapshot_ids = input$article_lab_scored_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-scored title before approving it for subtitle generation."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_candidates_for_subtitle(con, selected_ids)
    article_lab_state$notice <- if (result$approved_n > 0 || result$skipped_n == 0) {
      sprintf("Approved %s selected title%s for subtitle generation.", result$approved_n, ifelse(result$approved_n == 1, "", "s"))
    } else {
      sprintf(
        "Approved %s selected title%s for subtitle generation. %s selected title%s were skipped because they were not API scored.",
        result$approved_n,
        ifelse(result$approved_n == 1, "", "s"),
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s")
      )
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_queue_titles, {
    queue_rows <- article_lab_queue_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(queue_rows, "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_scored_rows(), "article_lab_scored_notes"))
    selected_ids <- collect_selected_ids(
      queue_rows,
      "article_lab_queue_select",
      snapshot_ids = input$article_lab_queue_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-queue title before archiving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_archive_api_scored_candidates(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Archived %s selected API-queue title%s. No rows were deleted.",
      result$archived_n,
      ifelse(result$archived_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_scored_titles, {
    scored_rows <- article_lab_scored_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_queue_rows(), "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(scored_rows, "article_lab_scored_notes"))
    selected_ids <- collect_selected_ids(
      scored_rows,
      "article_lab_scored_select",
      snapshot_ids = input$article_lab_scored_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-scored title before archiving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_archive_api_scored_candidates(con, selected_ids)
    article_lab_state$notice <- if (result$archived_n > 0 || result$skipped_n == 0) {
      sprintf("Archived %s selected title%s.", result$archived_n, ifelse(result$archived_n == 1, "", "s"))
    } else {
      sprintf(
        "Archived %s selected title%s. %s selected title%s were skipped because they were not API scored.",
        result$archived_n,
        ifelse(result$archived_n == 1, "", "s"),
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s")
      )
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_subtitle_titles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      target_rows,
      "article_lab_subtitle_title_select",
      snapshot_ids = input$article_lab_subtitle_titles_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one subtitle-stage title before archiving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_archive_api_scored_candidates(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Archived %s selected subtitle-stage title%s. No rows were deleted.",
      result$archived_n,
      ifelse(result$archived_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_subtitles, {
    article_lab_state$is_generating_subtitles <- TRUE
    on.exit({
      article_lab_state$is_generating_subtitles <- FALSE
    }, add = TRUE)

    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      target_rows,
      "article_lab_subtitle_title_select",
      snapshot_ids = input$article_lab_subtitle_titles_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one approved title before generating subtitle candidates."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    result <- tryCatch(
      article_lab_generate_subtitles_for_titles(
        con,
        candidate_ids = selected_ids,
        model = input$article_lab_subtitle_model,
        prompt = input$article_lab_subtitle_prompt,
        variants_per_title = input$article_lab_subtitle_variants_per_title
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste("Subtitle generation failed:", conditionMessage(result))
    } else {
      fallback_copy <- if (!is.null(result$fallback_reason) && nzchar(result$fallback_reason)) {
        sprintf(" Stub fallback was used because: %s", result$fallback_reason)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "Generated %s subtitle candidate%s for %s selected title%s using model %s.%s %s selected title%s were skipped because they were not eligible or already had active subtitle candidates.%s",
        result$generated_n,
        ifelse(result$generated_n == 1, "", "s"),
        result$title_n,
        ifelse(result$title_n == 1, "", "s"),
        result$model %||% article_lab_default_subtitle_model,
        if (identical(result$mode, "stub")) " The stub helper was used." else "",
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s"),
        fallback_copy
      )
      article_lab_refresh(article_lab_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_add_manual_subtitles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))

    candidate_id <- article_lab_input_string(input$article_lab_manual_subtitle_candidate_id)
    subtitle_text <- input$article_lab_manual_subtitle_text %||% ""
    proposed_subtitles <- article_lab_normalize_subtitle(unlist(strsplit(subtitle_text, "\n", fixed = TRUE)))
    if (is.na(candidate_id) || !nzchar(candidate_id)) {
      article_lab_state$notice <- "Choose a title before adding manual subtitle ideas."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    if (length(proposed_subtitles) == 0) {
      article_lab_state$notice <- sprintf("Enter at least one manual subtitle idea under %s characters.", article_lab_subtitle_max_chars)
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    result <- article_lab_add_manual_subtitles(con, candidate_id, proposed_subtitles)
    if (result$added_n > 0) {
      duplicate_copy <- if (isTRUE(result$duplicate_n > 0)) {
        sprintf(" %s duplicate idea%s were skipped.", result$duplicate_n, ifelse(result$duplicate_n == 1, "", "s"))
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "Added %s manual subtitle idea%s for \"%s\".%s",
        result$added_n,
        ifelse(result$added_n == 1, "", "s"),
        result$title %||% "the selected title",
        duplicate_copy
      )
      updateTextAreaInput(session, "article_lab_manual_subtitle_text", value = "")
    } else if (isTRUE(result$duplicate_n > 0)) {
      article_lab_state$notice <- sprintf(
        "All entered subtitle ideas for \"%s\" already exist in this title's subtitle list.",
        result$title %||% "the selected title"
      )
    } else {
      article_lab_state$notice <- "The selected title is not currently eligible for manual subtitle ideas in this stage."
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_subtitles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_subtitle_candidate_select",
      snapshot_ids = input$article_lab_subtitle_candidates_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one subtitle candidate before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_subtitles(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Approved %s selected subtitle candidate%s. %s title package%s are now ready for Thumbnails.",
      result$approved_n,
      ifelse(result$approved_n == 1, "", "s"),
      length(unique(result$candidate_ids)),
      ifelse(length(unique(result$candidate_ids)) == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_reject_subtitles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_subtitle_candidate_select",
      snapshot_ids = input$article_lab_subtitle_candidates_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one subtitle candidate before rejecting it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_reject_subtitles(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Rejected %s selected subtitle candidate%s. No rows were deleted.",
      result$rejected_n,
      ifelse(result$rejected_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_thumbnails, {
    article_lab_state$is_generating_thumbnails <- TRUE
    on.exit({
      article_lab_state$is_generating_thumbnails <- FALSE
      article_lab_state$thumbnail_generation_started_at <- NULL
      article_lab_state$thumbnail_generation_estimate <- NULL
    }, add = TRUE)

    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      package_rows,
      "article_lab_thumbnail_package_select",
      snapshot_ids = input$article_lab_thumbnail_packages_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one ready title/subtitle package before generating thumbnail candidates."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    variants_per_package <- max(1L, min(4L, suppressWarnings(as.integer(input$article_lab_thumbnail_variants_per_package)) %||% article_lab_default_thumbnail_variants))
    estimate <- article_lab_thumbnail_estimate(length(selected_ids) * variants_per_package)
    started_at <- Sys.time()
    article_lab_state$thumbnail_generation_started_at <- started_at
    article_lab_state$thumbnail_generation_estimate <- estimate
    article_lab_state$notice <- sprintf(
      "Generating thumbnails: requested %s thumbnail%s for %s selected package%s. Initial estimate: %s. Waiting for OpenAI; live completed/remaining progress is not available during this blocking call.",
      estimate$total_expected,
      ifelse(estimate$total_expected == 1L, "", "s"),
      length(selected_ids),
      ifelse(length(selected_ids) == 1L, "", "s"),
      estimate$label
    )
    session$sendCustomMessage(
      "articleLabStartThumbnailTimer",
      list(
        total_expected = estimate$total_expected,
        estimate_label = estimate$label,
        lower_seconds = estimate$lower_seconds,
        upper_seconds = estimate$upper_seconds,
        started_at = paste0(format(as.POSIXct(started_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%OS3", tz = "UTC"), "Z")
      )
    )
    if (is.function(session$flushReact)) session$flushReact()

    result <- tryCatch(
      article_lab_generate_thumbnails_for_packages(
        con,
        subtitle_ids = selected_ids,
        model = input$article_lab_thumbnail_model,
        prompt = input$article_lab_thumbnail_prompt,
        variants_per_package = variants_per_package
      ),
      error = function(e) e
    )
    actual_seconds <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
    comparison <- article_lab_estimate_comparison(actual_seconds, estimate$lower_seconds, estimate$upper_seconds)
    timing_copy <- sprintf(
      "Thumbnail generation finished in %s. Initial estimate was %s, so this run was %s.",
      article_lab_format_duration(actual_seconds),
      estimate$label,
      comparison
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste(timing_copy, "Thumbnail generation failed:", conditionMessage(result))
      session$sendCustomMessage("articleLabStopThumbnailTimer", list(message = article_lab_state$notice))
    } else {
      mode_label <- result$mode %||% "unknown"
      fallback_count <- if (identical(mode_label, "stub")) result$generated_n else 0L
      failure_count <- 0L
      fallback_copy <- if (identical(mode_label, "stub") && !is.null(result$fallback_reason) && nzchar(result$fallback_reason)) {
        sprintf(" Stub fallback reason: %s", result$fallback_reason)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "%s Generated %s thumbnail candidate%s for %s selected package%s using model %s in %s mode. Fallback count: %s. Failure count: %s. %s selected package%s were skipped because they were not eligible or already had active thumbnail candidates.%s",
        timing_copy,
        result$generated_n,
        ifelse(result$generated_n == 1, "", "s"),
        result$package_n,
        ifelse(result$package_n == 1, "", "s"),
        result$model %||% article_lab_default_thumbnail_model,
        mode_label,
        fallback_count,
        failure_count,
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s"),
        fallback_copy
      )
      session$sendCustomMessage("articleLabStopThumbnailTimer", list(message = article_lab_state$notice))
      article_lab_refresh(article_lab_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_dismiss_thumbnail_packages, {
    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      package_rows,
      "article_lab_thumbnail_package_select",
      snapshot_ids = input$article_lab_thumbnail_packages_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one ready title/subtitle package before dismissing it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_dismiss_thumbnail_packages(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Dismissed %s selected title/subtitle package%s. No rows were deleted.",
      result$dismissed_n,
      ifelse(result$dismissed_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_thumbnails, {
    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_thumbnail_candidate_select",
      snapshot_ids = input$article_lab_thumbnail_candidates_selected_snapshot,
      key_col = "thumbnail_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one thumbnail preview card before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    selected_rows <- pending_rows[pending_rows$thumbnail_id %in% selected_ids, , drop = FALSE]
    duplicate_subtitle_ids <- names(table(selected_rows$subtitle_id)[table(selected_rows$subtitle_id) > 1L])
    if (length(duplicate_subtitle_ids) > 0) {
      article_lab_state$notice <- "Select only one thumbnail candidate per title/subtitle package before approving."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_thumbnails(con, selected_ids)
    if (!is.null(result$message) && nzchar(result$message)) {
      article_lab_state$notice <- result$message
    } else {
      article_lab_state$notice <- sprintf(
        "Approved %s selected thumbnail%s. %s package%s are now ready for Outline.",
        result$approved_n,
        ifelse(result$approved_n == 1, "", "s"),
        length(unique(result$subtitle_ids)),
        ifelse(length(unique(result$subtitle_ids)) == 1, "", "s")
      )
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_reject_thumbnails, {
    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_thumbnail_candidate_select",
      snapshot_ids = input$article_lab_thumbnail_candidates_selected_snapshot,
      key_col = "thumbnail_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one thumbnail preview card before rejecting it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_reject_thumbnails(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Rejected %s selected thumbnail candidate%s. No rows were deleted.",
      result$rejected_n,
      ifelse(result$rejected_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_outlines, {
    outline_rows <- article_lab_ready_for_outline_rows()
    article_lab_update_outlines(con, collect_outline_updates(outline_rows))
    selected_ids <- collect_selected_ids(
      outline_rows,
      "article_lab_outline_packages",
      snapshot_ids = input$article_lab_outline_packages_selected_snapshot,
      key_col = "thumbnail_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one approved package without an outline before generating."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    selected_rows <- outline_rows[outline_rows$thumbnail_id %in% selected_ids & (is.na(outline_rows$outline_id) | !nzchar(outline_rows$outline_id)), , drop = FALSE]
    if (nrow(selected_rows) == 0) {
      article_lab_state$notice <- "Selected packages already have active outline drafts."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- generate_outline_drafts(selected_rows, model = input$article_lab_outline_model, prompt = input$article_lab_outline_prompt)
    inserted_n <- article_lab_insert_outline_drafts(con, result$rows)
    article_lab_state$notice <- sprintf(
      "Generated %s outline draft%s using model %s in %s mode.",
      inserted_n,
      ifelse(inserted_n == 1L, "", "s"),
      result$model %||% article_lab_default_outline_model,
      result$mode %||% "unknown"
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_outlines, {
    updated_n <- article_lab_update_outlines(con, collect_outline_updates(article_lab_ready_for_outline_rows()))
    article_lab_state$notice <- sprintf("Saved %s editable outline%s.", updated_n, ifelse(updated_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_outlines, {
    outline_rows <- article_lab_ready_for_outline_rows()
    article_lab_update_outlines(con, collect_outline_updates(outline_rows))
    draft_rows <- outline_rows[!is.na(outline_rows$outline_id) & nzchar(outline_rows$outline_id) & outline_rows$outline_status == "draft", , drop = FALSE]
    selected_ids <- collect_selected_ids(
      draft_rows,
      "article_lab_outline_candidates",
      snapshot_ids = input$article_lab_outline_candidates_selected_snapshot,
      key_col = "outline_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one outline draft before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_outlines(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Approved %s outline%s. %s package%s moved to draft-ready.",
      result$approved_n,
      ifelse(result$approved_n == 1L, "", "s"),
      length(result$candidate_ids),
      ifelse(length(result$candidate_ids) == 1L, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_outlines, {
    updated_n <- article_lab_update_outlines(con, collect_outline_updates(article_lab_ready_for_outline_rows()))
    article_lab_state$notice <- sprintf("Refreshed Outline and saved %s editable outline%s.", updated_n, ifelse(updated_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_scores, {
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_queue_rows(), "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_scored_rows(), "article_lab_scored_notes"))
    article_lab_state$notice <- "Refreshed API Scoring and saved visible notes."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_subtitles, {
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_subtitle_target_rows(), "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(article_lab_pending_subtitle_rows(), "article_lab_subtitle_candidate_notes"))
    article_lab_state$notice <- "Refreshed Subtitle Generation and saved visible notes."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_thumbnails, {
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(article_lab_thumbnail_package_rows(), "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(article_lab_pending_thumbnail_rows(), "article_lab_thumbnail_candidate_notes"))
    article_lab_state$notice <- "Refreshed Thumbnails and saved visible notes."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_open_docs, {
    showModal(modalDialog(
      title = "Article Lab workflow docs",
      p("Source-of-truth docs for this workflow:"),
      tags$ul(
        tags$li("data/analysis/article_lab/2026-05-23_title_lab_scoring_and_workflow_summary.md"),
        tags$li("data/analysis/article_lab/2026-05-23_human_score_and_api_human_combination_notes.md")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)

  output$guide_content <- renderUI({
    if (article_lab_is_workflow_section(active_section()) || identical(active_section(), "settings")) {
      current_section <- active_section()
      overview <- article_lab_overview_stats()
      batches <- article_lab_batches()
      latest_saved_batch <- article_lab_saved_batch()
      selected_batch_id <- article_lab_selected_batch_id()
      selected_candidates <- article_lab_selected_batch_candidates()
      selected_batch <- if (nrow(batches) > 0 && !is.na(selected_batch_id) && nzchar(selected_batch_id)) {
        batches[batches$batch_id == selected_batch_id, , drop = FALSE]
      } else {
        data.frame()
      }
      current_batch_label <- if (identical(current_section, "generate") && !is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
        sprintf("Unsaved draft with %s titles", nrow(article_lab_state$draft))
      } else if (identical(current_section, "generate") && !is.null(latest_saved_batch) && nrow(latest_saved_batch) > 0) {
        sprintf("Latest saved batch %s", latest_saved_batch$batch_id[[1]])
      } else if (identical(selected_batch_id, article_lab_all_batches_value)) {
        "All saved titles across batches"
      } else if (nrow(selected_batch) > 0) {
        sprintf("Saved batch %s", selected_batch$batch_id[[1]])
      } else {
        "No batch saved yet"
      }
      current_batch_meta <- if (identical(current_section, "generate") && !is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
        paste("Draft created at", article_lab_state$draft_created_at %||% now_utc())
      } else if (identical(current_section, "generate") && !is.null(latest_saved_batch) && nrow(latest_saved_batch) > 0) {
        paste("Created", latest_saved_batch$created_at[[1]], "\u00b7 model", first_value(latest_saved_batch, "model", article_lab_default_model))
      } else if (identical(selected_batch_id, article_lab_all_batches_value)) {
        "The current selection spans all saved batches."
      } else if (nrow(selected_batch) > 0) {
        paste("Created", selected_batch$created_at[[1]], "\u00b7 model", first_value(selected_batch, "model", article_lab_default_model))
      } else {
        "Generate first, then save to persist candidates."
      }
      ready_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "ready_for_api_scoring", na.rm = TRUE)
      scored_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "api_scored", na.rm = TRUE)
      approved_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "approved_for_subtitle", na.rm = TRUE)
      subtitle_ready_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "ready_for_thumbnail", na.rm = TRUE)

      if (current_section %in% c("research_inbox", "api_scoring", "subtitle_generation", "thumbnails")) {
        return(NULL)
      }

      return(tagList(
        div(
          class = "status-card",
          h3("Article Lab status"),
          div(class = "status-metric", overview$saved_candidates[[1]]),
          p(sprintf("%s saved candidates across %s batches.", overview$saved_candidates[[1]], overview$saved_batches[[1]])),
          p(class = "lab-status-copy", sprintf("%s remain New, %s are in API queue, %s are approved for subtitles, %s are ready for Thumbnails, and %s are ready for Outline.", overview$generated[[1]], overview$ready_for_api_scoring[[1]], overview$approved_for_subtitle[[1]], overview$ready_for_thumbnail[[1]], overview$ready_for_outline[[1]]))
        ),
        div(
          class = "status-card",
          h3("Current selection"),
          p(current_batch_label),
          p(class = "lab-status-copy", current_batch_meta)
        ),
        div(
          class = "status-card",
          h3("Reminder"),
          p("Home remains the separate rating workflow."),
          p(class = "lab-status-copy", sprintf("This pass now covers Generate, API Scoring, Subtitle Generation, and Thumbnails. %s title%s are ready for Thumbnails in the current selection.", subtitle_ready_n, ifelse(subtitle_ready_n == 1, "", "s")))
        )
      ))
    }

    if (is_dimension_mode) {
      field <- active_dimension()
      if (is.na(field)) {
        return(tagList(
          div(class = "guide-section", h3("Dimension pass"), p("All dimension passes are complete.")),
          div(class = "tip", h3("Reminder"), p("No outcome, API, or prior human score data is shown during rating."))
        ))
      }
      return(tagList(
        div(class = "guide-section", h3("Active dimension"), p(dimension_labels[[field]])),
        div(class = "guide-section", h3("Focus"), p(dimension_focus[[field]])),
        div(
          class = "guide-section",
          h3("Hotkeys"),
          if (field == "ai_low_effort_flag") {
            tags$ul(tags$li("A or 1 = yes"), tags$li("S or 2 = unsure"), tags$li("J or 3 = no"))
          } else {
            tags$ul(tags$li("A/S/D/F/J = 1/2/3/4/5"), tags$li("1 through 5 also work"))
          }
        ),
        div(class = "tip", h3("Reminder"), p("Score only the active dimension. Do not judge the other dimensions during this pass."))
      ))
    }

    current_item <- current()
    title_only_home <- !is.null(current_item) && identical(first_value(current_item, "source_type", "dataset"), "article_lab_generated")

    tagList(
      div(
        class = "guide-section",
        h3("How it works"),
        p(if (title_only_home) {
          "Rate this title-only candidate using only the visible headline."
        } else {
          "Rate each preview using only the visible headline, subtitle, and thumbnail."
        })
      ),
      div(
        class = "guide-section",
        h3("Focus on"),
        if (title_only_home) {
          tags$ul(
            tags$li("Headline clarity and hook"),
            tags$li("Specificity and credibility"),
            tags$li("Curiosity without clickbait"),
            tags$li("Your gut reaction to the title alone")
          )
        } else {
          tags$ul(
            tags$li("Headline clarity and hook"),
            tags$li("Topic relevance and appeal"),
            tags$li("Perceived value to readers"),
            tags$li("Your gut feeling")
          )
        }
      ),
      div(
        class = "guide-section",
        h3("Rating guide"),
        p("1 Very weak"),
        p("2 Weak"),
        p("3 Average / unclear"),
        p("4 Strong"),
        p("5 Very strong")
      ),
      div(class = "tip", h3("Tip"), p("There are no right or wrong answers. Consistency is the goal."))
    )
  })

  output$article_area <- renderUI({
    item <- current()
    if (is.null(item)) {
      if (is_dimension_mode) {
        field <- active_dimension()
        if (is.na(field)) {
          return(div(class = "done-state", h2("All dimensions complete"), p("Every dimension pass has been completed for the cohort.")))
        }
        return(div(class = "done-state", h2("Dimension complete"), p(paste("Completed pass:", dimension_labels[[field]]))))
      }
      return(div(class = "done-state", h2("Session complete"), p("All queued previews have been rated or skipped.")))
    }

    field <- if (is_dimension_mode) active_dimension() else NA_character_
    render_info <- if (is_dimension_v2_mode) v2_render_info(item) else NULL
    is_article_lab_title_only <- identical(first_value(item, "source_type", "dataset"), "article_lab_generated")
    thumbnail_path <- item$local_thumbnail_path[[1]]
    thumbnail_path_abs <- if (is_dimension_v2_mode) {
      render_info$path_abs
    } else if ("local_thumbnail_path_abs" %in% names(item)) {
      item$local_thumbnail_path_abs[[1]]
    } else {
      as_abs_path(thumbnail_path)[[1]]
    }
    thumbnail_status <- if ("thumbnail_status" %in% names(item)) item$thumbnail_status[[1]] else NA_character_
    has_thumbnail <- if (is_dimension_v2_mode) {
      isTRUE(render_info$valid)
    } else {
      identical(thumbnail_status, "valid") && !is.na(thumbnail_path_abs) && file.exists(thumbnail_path_abs)
    }
    isolate_title_field <- is_article_lab_title_only || (is_dimension_mode && !is.na(field) && field %in% title_isolation_dimension_fields)
    thumbnail_ui <- if (isolate_title_field) {
      div(class = "thumbnail-placeholder", div(class = "thumbnail-invalid-label", title_only_placeholder_thumbnail_label))
    } else if (has_thumbnail) {
      imageOutput("thumbnail", width = "170px", height = "113px")
    } else if (is_dimension_v2_mode) {
      missing_reason <- render_info$reason %in% c("missing_file", "missing_manifest_hash", "missing_rendered_hash")
      placeholder_label <- if (isTRUE(missing_reason)) {
        "Thumbnail missing: validated manifest image unavailable"
      } else {
        "Thumbnail blocked: manifest/hash mismatch"
      }
      div(class = "thumbnail-placeholder error", div(class = "thumbnail-invalid-label", placeholder_label))
    } else {
      div(class = "thumbnail-placeholder", div(class = "thumbnail-invalid-label", "Invalid or missing thumbnail"))
    }

    subtitle <- if (is_article_lab_title_only) title_only_placeholder_subtitle else displayed_subtitle_for_field(item, field)
    thumbnail_only <- is_dimension_mode && !is.na(field) && field %in% thumbnail_only_dimension_fields
    text_only <- is_article_lab_title_only || (is_dimension_mode && !is.na(field) && field %in% text_only_dimension_fields)

    if (text_only) {
      return(div(
        class = "article-card",
        div(
          h2(class = "article-title", item$title[[1]]),
          if (!is.na(subtitle)) p(class = "article-subtitle", subtitle)
        ),
        div(class = "thumbnail-wrap", thumbnail_ui)
      ))
    }

    if (thumbnail_only) {
      return(div(
        class = "article-card thumbnail-only",
        div(class = "thumbnail-wrap", thumbnail_ui)
      ))
    }

    div(
      class = "article-card",
      div(
        h2(class = "article-title", item$title[[1]]),
        if (!is.na(subtitle)) p(class = "article-subtitle", subtitle)
      ),
      div(class = "thumbnail-wrap", thumbnail_ui)
    )
  })

  output$thumbnail <- renderImage({
    item <- current()
    req(!is.null(item))
    if (is_dimension_v2_mode) {
      info <- v2_render_info(item)
      req(isTRUE(info$valid))
      path_abs <- info$path_abs
    } else {
      path <- item$local_thumbnail_path[[1]]
      path_abs <- if ("local_thumbnail_path_abs" %in% names(item)) {
      item$local_thumbnail_path_abs[[1]]
      } else {
        as_abs_path(path)[[1]]
      }
    }
    req(!is.na(path_abs), file.exists(path_abs))
    list(src = normalizePath(path_abs, mustWork = TRUE), alt = "", width = 170, height = 113)
  }, deleteFile = FALSE)

  output$rating_panel <- renderUI({
    if (!is_dimension_mode) {
      current_item <- current()
      prompt_text <- if (!is.null(current_item) && identical(first_value(current_item, "source_type", "dataset"), "article_lab_generated")) {
        "Based only on the title, how likely is this article to perform well on Medium?"
      } else {
        rating_prompt
      }
      return(div(
        class = "rating-panel",
        div(class = "prompt", prompt_text),
        div(
          class = "note-row",
          textInput(
            "note",
            "Optional note",
            value = "",
            width = "100%",
            placeholder = "Quick note, e.g. AI thumbnail, strong title, generic topic"
          )
        ),
        div(class = "scale-labels", span("Very weak"), span("Very strong")),
        div(
          class = "rating-buttons",
          actionButton("score_1", "1"),
          actionButton("score_2", "2"),
          actionButton("score_3", "3"),
          actionButton("score_4", "4"),
          actionButton("score_5", "5")
        ),
        div(
          class = "rating-actions",
          div(actionButton("skip", "Skip"), actionButton("undo", "Undo previous")),
          div(class = "shortcut-copy", "A=1, S=2, D=3, F=4, J=5 · 1-5 also work · Space=skip · U=undo · N=note · Enter/Esc exits note")
        )
      ))
    }

    field <- active_dimension()
    c <- counts()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- ifelse(is.na(c$total[[1]]), 0, c$total[[1]])

    if (is.na(field)) {
      return(div(
        class = "rating-panel",
        div(class = "dimension-pass-header",
            div(class = "dimension-pass-kicker", "Dimension pass complete"),
            div(class = "dimension-pass-name", "All dimensions complete"),
            div(
              class = "dimension-pass-focus",
              if (is_dimension_v2_mode) {
                sprintf(
                  "Overall manual rating progress: %s / %s ratings complete",
                  candidate_stats()$completed_dimensions[[1]],
                  candidate_stats()$total_dimensions[[1]]
                )
              } else {
                sprintf("Overall active dimension progress: %s / %s dimensions complete", length(active_dimension_fields), length(active_dimension_fields))
              }
            )
        )
      ))
    }

    if (total > 0 && completed >= total) {
      next_field <- next_incomplete_dimension_after(con, field)
      return(div(
        class = "rating-panel",
        div(class = "dimension-pass-header",
            div(class = "dimension-pass-kicker", "Dimension complete"),
            div(class = "dimension-pass-name", dimension_labels[[field]]),
            div(
              class = "dimension-pass-focus",
              if (is_dimension_v2_mode) {
                sprintf(
                  "Dimension progress: %s / %s · Overall manual rating progress: %s / %s ratings complete",
                  completed,
                  total,
                  candidate_stats()$completed_dimensions[[1]],
                  candidate_stats()$total_dimensions[[1]]
                )
              } else {
                sprintf("Dimension progress: %s / %s", completed, total)
              }
            )
        ),
        if (!is.na(next_field)) {
          div(
            class = "next-dimension-cta",
            div(class = "next-dimension-copy", sprintf("This pass is finished. Continue directly into the next dimension: %s.", dimension_labels[[next_field]])),
            actionButton("start_next_dimension", paste("Continue To", dimension_labels[[next_field]]))
          )
        } else {
          div(class = "shortcut-copy", "All dimension passes are complete.")
        }
      ))
    }

    item <- current()
    can_rate_current <- !is_dimension_v2_mode ||
      field %in% text_only_dimension_fields ||
      (!is.null(item) && isTRUE(v2_render_info(item)$valid))

    numeric_buttons <- function(field, enabled = TRUE) {
      div(
        class = "dimension-buttons",
        lapply(1:5, function(score) {
          tags$button(
            type = "button",
            class = "btn dimension-choice",
            `data-field` = field,
            `data-value` = as.character(score),
            disabled = if (isTRUE(enabled)) NULL else "disabled",
            as.character(score)
          )
        })
      )
    }
    flag_buttons <- function(field, enabled = TRUE) {
      choices <- c("yes", "unsure", "no")
      shortcuts <- c(yes = "D", unsure = "F", no = "J")
      div(
        class = "dimension-buttons",
        lapply(choices, function(choice) {
          tags$button(
            type = "button",
            class = "btn dimension-choice flag-choice",
            `data-field` = field,
            `data-value` = choice,
            disabled = if (isTRUE(enabled)) NULL else "disabled",
            span(class = "dimension-choice-label", choice),
            span(class = "dimension-choice-shortcut", shortcuts[[choice]])
          )
        })
      )
    }
    scale_ui <- function(field) {
      scale <- dimension_scale[[field]]
      scale_shortcuts <- c("1" = "A=1", "2" = "S=2", "3" = "D=3", "4" = "F=4", "5" = "J=5")
      flag_shortcuts <- c(yes = "S", unsure = "D", no = "J")
      div(
        class = paste("dimension-scale-list", if (field == "ai_low_effort_flag") "dimension-flag-scale" else ""),
        lapply(names(scale), function(name) {
          if (field == "ai_low_effort_flag") {
            tags$button(
              type = "button",
              class = "btn dimension-scale-item dimension-scale-choice dimension-choice",
              `data-field` = field,
              `data-value` = as.character(name),
              disabled = if (isTRUE(can_rate_current)) NULL else "disabled",
              strong(scale[[name]]),
              span(class = "dimension-scale-shortcut", flag_shortcuts[[as.character(name)]])
            )
          } else {
            tags$button(
              type = "button",
              class = "btn dimension-scale-item dimension-scale-choice dimension-choice",
              `data-field` = field,
              `data-value` = as.character(name),
              disabled = if (isTRUE(can_rate_current)) NULL else "disabled",
              strong(name),
              span(scale[[name]]),
              if (as.character(name) %in% names(scale_shortcuts)) {
                span(class = "dimension-scale-shortcut", scale_shortcuts[[as.character(name)]])
              }
            )
          }
        })
      )
    }

    verification_title <- if (
      !is.null(item) &&
        field %in% thumbnail_only_dimension_fields &&
        "title" %in% names(item) &&
        !is.na(item$title[[1]])
    ) {
      div(
        class = "dimension-verification-title",
        `data-copy-title` = item$title[[1]],
        title = "Click to copy title",
        item$title[[1]]
      )
    } else {
      NULL
    }

    div(
      class = "rating-panel",
      div(
        class = "dimension-pass-header",
        div(class = "dimension-pass-kicker", "Dimension pass"),
        div(class = "dimension-pass-name", paste("Active dimension:", dimension_labels[[field]])),
        div(class = "dimension-pass-focus", strong("Focus: "), dimension_focus[[field]]),
        div(class = "dimension-pass-question", strong("Question: "), dimension_questions[[field]]),
        div(
          class = "dimension-pass-focus",
          if (is_dimension_v2_mode) {
            sprintf(
              "Dimension progress: %s / %s · Overall manual rating progress: %s / %s ratings complete",
              completed,
              total,
              candidate_stats()$completed_dimensions[[1]],
              candidate_stats()$total_dimensions[[1]]
            )
          } else {
            sprintf(
              "Dimension progress: %s / %s · Overall active dimension progress: %s / %s dimensions complete",
              completed,
              total,
              candidate_stats()$completed_dimensions[[1]],
              candidate_stats()$total_dimensions[[1]]
            )
          }
        ),
        verification_title
      ),
      scale_ui(field),
      div(
        class = "note-row",
        textAreaInput(
          "note",
          "Optional note",
          value = "",
          width = "100%",
          height = "54px",
          placeholder = "Optional note"
        )
      ),
      div(
        class = "rating-actions",
        div(actionButton("skip", "Skip"), actionButton("undo", "Undo previous")),
        div(
          class = "shortcut-copy",
          if (field == "ai_low_effort_flag") {
            "S=yes, D=unsure, J=no · 1=yes, 2=unsure, 3=no · Space=skip, U=undo, N=note"
          } else {
            "A=1, S=2, D=3, F=4, J=5 · 1-5 also work · Space=skip, U=undo, N=note"
          }
        )
      )
    )
  })

  handle_score <- function(score) {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (is_dimension_mode) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    save_current_rating(con, item, score = score, note = input$note, skipped = FALSE, shown_started_at = shown_started_at())
    refresh_current()
  }

  observeEvent(input$score_1, handle_score(1L), ignoreInit = TRUE)
  observeEvent(input$score_2, handle_score(2L), ignoreInit = TRUE)
  observeEvent(input$score_3, handle_score(3L), ignoreInit = TRUE)
  observeEvent(input$score_4, handle_score(4L), ignoreInit = TRUE)
  observeEvent(input$score_5, handle_score(5L), ignoreInit = TRUE)
  observeEvent(input$score_key, {
    score_value <- input$score_key
    if (is.list(score_value) && !is.null(score_value$score)) {
      score_value <- score_value$score
    }
    handle_score(as.integer(score_value))
  }, ignoreInit = TRUE)

  observeEvent(input$skip, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    if (is_dimension_mode) {
      save_current_dimension_rating(con, item, active_dimension(), note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    } else {
      save_current_rating(con, item, score = NULL, note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    }
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$skip_key, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    if (is_dimension_mode) {
      save_current_dimension_rating(con, item, active_dimension(), note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    } else {
      save_current_rating(con, item, score = NULL, note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    }
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$undo, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (is_dimension_mode) undo_previous_dimension_rating(con, active_dimension()) else undo_previous_rating(con, rating_session_id)
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$undo_key, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (is_dimension_mode) undo_previous_dimension_rating(con, active_dimension()) else undo_previous_rating(con, rating_session_id)
    refresh_current()
  }, ignoreInit = TRUE)

  apply_dimension_value <- function(field, value) {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (!is_dimension_mode || !(field %in% dimension_fields)) return(invisible(NULL))
    if (!identical(field, active_dimension())) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    if (
      is_dimension_v2_mode &&
        !(field %in% text_only_dimension_fields) &&
        !isTRUE(v2_render_info(item)$valid)
    ) {
      return(invisible(NULL))
    }
    save_current_dimension_rating(
      con,
      item,
      field,
      value = value,
      note = input$note,
      skipped = FALSE,
      shown_started_at = shown_started_at()
    )
    refresh_current()
  }

  observeEvent(input$dimension_select, {
    value <- input$dimension_select
    if (!is.list(value)) return(invisible(NULL))
    apply_dimension_value(value$field, value$value)
  }, ignoreInit = TRUE)

  observeEvent(input$dimension_key, {
    value <- input$dimension_key
    key <- if (is.list(value)) value$key else value
    field <- active_dimension()
    if (is.na(field)) return(invisible(NULL))
    if (field %in% dimension_numeric_fields) {
      numeric_map <- c(a = 1L, s = 2L, d = 3L, f = 4L, j = 5L)
      score <- if (key %in% names(numeric_map)) numeric_map[[key]] else suppressWarnings(as.integer(key))
      apply_dimension_value(field, score)
    } else {
      flag_map <- c(s = "yes", d = "unsure", j = "no", `1` = "yes", `2` = "unsure", `3` = "no")
      if (key %in% names(flag_map)) apply_dimension_value(field, flag_map[[key]])
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dimension_back_key, {
    invisible(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$dimension_reset_key, {
    invisible(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$start_next_dimension, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (!is_dimension_mode) return(invisible(NULL))
    next_field <- next_incomplete_dimension_after(con, active_dimension())
    if (!is.na(next_field)) active_dimension(next_field)
    refresh_current()
  }, ignoreInit = TRUE)

}

shinyApp(ui, server)
