required_packages <- c("shiny", "DBI", "RSQLite", "jsonlite")
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
  dbConnect(SQLite(), db_path)
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

article_lab_default_model <- local({
  configured <- Sys.getenv("OPENAI_TITLE_GENERATION_MODEL", unset = "")
  if (!nzchar(configured)) configured <- Sys.getenv("OPENAI_HEADLINE_MODEL", unset = "")
  if (!nzchar(configured)) configured <- "gpt-5-mini"
  configured
})

ensure_article_lab_schema <- function(con) {
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
      status TEXT NOT NULL DEFAULT 'generated',
      source TEXT NOT NULL DEFAULT 'article_lab',
      ready_for_human_rating INTEGER NOT NULL DEFAULT 0,
      promoted INTEGER NOT NULL DEFAULT 0,
      archived INTEGER NOT NULL DEFAULT 0,
      raw_json TEXT,
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

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
}

article_lab_batch_id <- function() {
  paste0("alb_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(99999L, 1))
}

article_lab_candidate_id <- function(batch_id, index) {
  paste0("alc_", batch_id, "_", sprintf("%02d", as.integer(index)))
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

  data.frame(
    row_number = seq_len(n),
    title = titles[seq_len(n)],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

article_lab_input_string <- function(x) {
  value <- clean_text(x)
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

article_lab_api_request <- function(prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_, example_titles = character()) {
  helper_path <- file.path("scripts", "writing_api", "generate_titles.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_titles.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)

  request_payload <- list(
    prompt = article_lab_input_string(prompt) %||% article_lab_default_prompt,
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
    response_id = article_lab_input_string(parsed$response_id)
  )
}

generate_title_candidates <- function(con, prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_) {
  inspiration_value <- article_lab_input_string(inspiration_source)
  example_titles <- if (identical(inspiration_value, "top performing titles")) article_lab_top_title_examples(con, limit = 8L) else character()

  tryCatch({
    api_result <- article_lab_api_request(
      prompt = prompt,
      batch_size = batch_size,
      seed_topic = seed_topic,
      inspiration_source = inspiration_source,
      model = model,
      example_titles = example_titles
    )
    api_result$fallback_reason <- NULL
    api_result
  }, error = function(e) {
    list(
      titles = stub_title_candidates(
        prompt = prompt,
        batch_size = batch_size,
        seed_topic = seed_topic,
        inspiration_source = inspiration_source,
        model = model
      ),
      mode = "stub",
      model = article_lab_input_string(model) %||% article_lab_default_model,
      raw_json = NULL,
      example_titles_used = as.integer(length(example_titles)),
      response_id = NULL,
      fallback_reason = conditionMessage(e)
    )
  })
}

save_article_lab_batch <- function(con, prompt, seed_topic, inspiration_source, requested_batch_size, model, titles, raw_json = NA_character_, generation_mode = "generated") {
  if (length(titles) == 0) return(invisible(NULL))
  batch_id <- article_lab_batch_id()
  created_at <- now_utc()
  prompt_value <- clean_text(prompt)
  if (length(prompt_value) == 0 || is.na(prompt_value[[1]])) prompt_value <- article_lab_default_prompt
  seed_topic_value <- clean_text(seed_topic)
  inspiration_value <- clean_text(inspiration_source)
  model_value <- clean_text(model)
  if (length(model_value) == 0 || is.na(model_value[[1]])) model_value <- article_lab_default_model
  requested_size <- suppressWarnings(as.integer(requested_batch_size))
  if (is.na(requested_size) || requested_size < 1L) requested_size <- length(titles)

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
          "Queue integration TODO: stored as ready_for_human_rating for later title-only workflow ingestion."
        )
      )
    )

    for (i in seq_along(titles)) {
      title_value <- clean_text(titles[[i]])
      if (length(title_value) == 0 || is.na(title_value[[1]])) next
      dbExecute(
        con,
        "INSERT INTO article_lab_title_candidates
         (candidate_id, batch_id, created_at, title, status, source, ready_for_human_rating, promoted, archived, raw_json)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          article_lab_candidate_id(batch_id, i),
          batch_id,
          created_at,
          title_value[[1]],
          "ready_for_human_rating",
          "article_lab",
          1L,
          0L,
          0L,
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
  dbGetQuery(
    con,
    "SELECT candidate_id, title, status, created_at, batch_id
     FROM article_lab_title_candidates
     WHERE batch_id = ?
     ORDER BY candidate_id",
    params = list(batch_id)
  )
}

article_lab_overview <- function(con) {
  if (!dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame(
      saved_batches = 0L,
      saved_candidates = 0L,
      ready_for_human_rating = 0L,
      promoted = 0L,
      archived = 0L
    ))
  }

  dbGetQuery(con, "
    SELECT
      (SELECT COUNT(*) FROM article_lab_title_batches) AS saved_batches,
      COUNT(*) AS saved_candidates,
      COALESCE(SUM(CASE WHEN ready_for_human_rating = 1 THEN 1 ELSE 0 END), 0) AS ready_for_human_rating,
      COALESCE(SUM(CASE WHEN promoted = 1 THEN 1 ELSE 0 END), 0) AS promoted,
      COALESCE(SUM(CASE WHEN archived = 1 THEN 1 ELSE 0 END), 0) AS archived
    FROM article_lab_title_candidates
  ")
}

article_lab_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No generated title batch yet. Generate a draft or save a batch to populate this table."))
  }

  headers <- c("Candidate", "Title", "Status", "Created", "Batch")
  tagList(
    tags$table(
      class = "lab-table",
      tags$thead(tags$tr(lapply(headers, tags$th))),
      tags$tbody(
        lapply(seq_len(nrow(rows)), function(i) {
          tags$tr(
            tags$td(rows$candidate_id[[i]]),
            tags$td(rows$title[[i]]),
            tags$td(rows$status[[i]]),
            tags$td(rows$created_at[[i]]),
            tags$td(rows$batch_id[[i]])
          )
        })
      )
    )
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
    return(list(article_ids = character(), post_ids = character()))
  }

  rated <- dbGetQuery(con, "
    SELECT DISTINCT article_id, medium_post_id
    FROM human_preview_ratings
  ")

  article_ids <- clean_text(rated$article_id)
  post_ids <- clean_text(rated$medium_post_id)
  list(
    article_ids = unique(article_ids[!is.na(article_ids)]),
    post_ids = unique(post_ids[!is.na(post_ids)])
  )
}

mark_duplicate_pending_queue_items <- function(con) {
  rated_keys <- get_rated_keys(con)
  if (length(rated_keys$article_ids) == 0 && length(rated_keys$post_ids) == 0) return(0L)

  pending <- dbGetQuery(con, "
    SELECT rating_session_id, queue_position, article_id, medium_post_id
    FROM human_preview_rating_queue
    WHERE status = 'pending'
  ")
  if (nrow(pending) == 0) return(0L)

  article_keys <- clean_text(pending$article_id)
  post_keys <- clean_text(pending$medium_post_id)
  duplicate <- (!is.na(article_keys) & article_keys %in% rated_keys$article_ids) |
    (!is.na(post_keys) & post_keys %in% rated_keys$post_ids)
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

  rated_keys <- get_rated_keys(con)
  article_keys <- clean_text(rows$article_id)
  post_keys <- clean_text(rows$medium_post_id)
  rows$already_rated <- (!is.na(article_keys) & article_keys %in% rated_keys$article_ids) |
    (!is.na(post_keys) & post_keys %in% rated_keys$post_ids)
  rows$already_rated[is.na(rows$already_rated)] <- FALSE

  if (isTRUE(exclude_rated)) {
    rows <- rows[!rows$already_rated, , drop = FALSE]
  }

  rows
}

candidate_counts <- function(con) {
  candidates <- load_candidates(con, exclude_rated = FALSE)
  data.frame(
    total_thumbnail_candidates = nrow(candidates),
    already_rated = sum(candidates$already_rated, na.rm = TRUE),
    remaining_unrated = sum(!candidates$already_rated, na.rm = TRUE)
  )
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
  shuffled <- candidates[sample.int(nrow(candidates)), , drop = FALSE]
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
         (rating_session_id, queue_position, article_id, medium_post_id, status)
         VALUES (?, ?, ?, ?, 'pending')",
        params = list(
          session_id,
          i,
          selected$article_id[[i]],
          selected$medium_post_id[[i]]
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

  if (nrow(existing) > 0) existing$rating_session_id[[1]] else create_new_session(con, target_n)
}

load_current_item <- function(con, session_id) {
  item <- dbGetQuery(con, "
    SELECT rating_session_id, queue_position, article_id, medium_post_id, status, shown_at, completed_at
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
        human_feed_success_potential, human_feed_success_note, skipped,
        shown_at, rated_at, seconds_spent)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
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
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  invisible(TRUE)
}

undo_previous_rating <- function(con, session_id) {
  previous <- dbGetQuery(con, "
    SELECT id, queue_position
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
      .nav-item {
        height: 42px; display: flex; align-items: center; gap: 14px; padding: 0 18px;
        border-radius: 8px; color: var(--ink); font-size: 16px; margin-bottom: 10px;
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
      .nav-item.active { background: var(--green-soft); font-weight: 650; }
      .daily-goal {
        border: 1px solid var(--line); border-radius: 8px; padding: 14px 18px;
        max-width: 250px;
        position: absolute;
        left: 20px;
        right: 20px;
      }
      .daily-goal.static-card {
        position: static;
        max-width: none;
        margin-top: 18px;
      }
      .daily-goal strong { display: block; margin-bottom: 8px; }
      .daily-goal .num { color: var(--green); font-weight: 700; }
      .progress-track { height: 7px; background: #e9e9e9; border-radius: 99px; overflow: hidden; margin: 12px 0 8px; }
      .progress-fill { height: 100%; background: var(--green); border-radius: 99px; width: 0%; }
      .main { padding: 22px 30px 18px; max-width: 920px; width: 100%; margin: 0 auto; }
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
      .section-tabs {
        display: flex;
        gap: 24px;
        border-bottom: 1px solid var(--line);
        margin-top: 18px;
        max-width: 760px;
      }
      .section-tab {
        padding: 0 0 10px;
        border: 0;
        background: transparent;
        color: var(--muted);
        font-size: 15px;
        font-weight: 500;
      }
      .section-tab.active {
        color: var(--ink);
        font-weight: 700;
        border-bottom: 2px solid var(--green);
      }
      .section-tab.disabled { opacity: .66; cursor: default; }
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
      .lab-table {
        width: 100%;
        border-collapse: collapse;
        margin-top: 6px;
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
      .lab-status-copy {
        margin-top: 12px;
        color: var(--muted);
        font-size: 13px;
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
      .empty-state {
        max-width: 760px;
        margin-top: 12px;
        padding: 18px 20px;
        color: var(--muted);
        font-size: 14px;
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
        .nav-item span, .daily-goal { display: none; }
      }
      @media (max-width: 820px) {
        .app-shell { display: block; }
        .sidebar { display: none; }
        .main { padding: 28px 18px; }
        .lab-grid { grid-template-columns: 1fr; }
        .article-card { grid-template-columns: 1fr; gap: 18px; padding: 24px 0 26px; }
        .article-card.thumbnail-only { grid-template-columns: 170px; }
        .article-card.text-only { grid-template-columns: 1fr; }
        .thumbnail-wrap { justify-content: flex-start; }
        .thumbnail-wrap .shiny-image-output,
        .thumbnail-wrap img,
        .thumbnail-placeholder { width: 100% !important; height: auto !important; aspect-ratio: 1.5; }
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
        const dailyTop = Math.max(22, ratingBottom - sidebarTop - dailyGoal.offsetHeight);
        const tipTop = Math.max(22, ratingBottom - guideTop - tip.offsetHeight);

        dailyGoal.style.top = dailyTop + 'px';
        tip.style.top = tipTop + 'px';
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
      } else {
        document.addEventListener('shiny:connected', function() {
          Shiny.addCustomMessageHandler('clearRatingFocus', handleClearRatingFocus);
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
  ensure_rating_schema(con)
  ensure_article_lab_schema(con)

  if (is_dimension_mode) ensure_dimension_pass_queues(con, target_n = default_target_n)
  rating_session_id <- if (is_dimension_mode) NULL else resume_or_create_session(con, target_n = default_target_n)
  active_section <- reactiveVal("home")
  active_dimension <- reactiveVal(if (is_dimension_mode) first_incomplete_dimension(con) else NA_character_)
  current <- reactiveVal(NULL)
  shown_started_at <- reactiveVal(Sys.time())
  article_lab_state <- reactiveValues(
    draft = NULL,
    draft_created_at = NULL,
    draft_meta = NULL,
    notice = NULL
  )
  article_lab_refresh <- reactiveVal(0L)

  observeEvent(input$sidebar_nav, {
    if (is.character(input$sidebar_nav) && input$sidebar_nav %in% c("home", "article_lab")) {
      active_section(input$sidebar_nav)
    }
  }, ignoreInit = TRUE)

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
    nav_button <- function(section, icon, label, enabled = TRUE) {
      tags$button(
        type = "button",
        class = paste("nav-item", if (identical(current_section, section)) "active" else ""),
        onclick = if (enabled) sprintf("Shiny.setInputValue('sidebar_nav', '%s', {priority: 'event'})", section) else NULL,
        disabled = if (!enabled) "disabled" else NULL,
        span(icon),
        span(label)
      )
    }

    tagList(
      nav_button("home", "\u2302", "Home"),
      tags$button(type = "button", class = "nav-item", disabled = "disabled", span("\u25a4"), span("Queue")),
      tags$button(type = "button", class = "nav-item", disabled = "disabled", span("\u21ba"), span("History")),
      tags$button(type = "button", class = "nav-item", disabled = "disabled", span("\u2699"), span("Settings")),
      nav_button("article_lab", "\u270e", "Article Lab")
    )
  })

  output$sidebar_status_card <- renderUI({
    if (identical(active_section(), "article_lab")) {
      return(div(
        class = "daily-goal static-card",
        strong("Article Lab"),
        p("Generate tab MVP only."),
        p(class = "shortcut-copy", "Human rating stays in the normal workflow.")
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
    if (identical(active_section(), "article_lab")) {
      return(tagList(
        h1("Article Lab"),
        div(class = "page-subtitle", "Generate, save, and review new title candidates."),
        div(
          class = "section-tabs",
          div(class = "section-tab active", "Generate"),
          div(class = "section-tab disabled", "API score"),
          div(class = "section-tab disabled", "Compare"),
          div(class = "section-tab disabled", "Promote")
        ),
        div(
          class = "lab-card",
          h2("Generation prompt"),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_prompt",
              label = NULL,
              value = article_lab_default_prompt,
              width = "100%",
              height = "230px"
            )
          ),
          div(
            class = "lab-grid",
            div(
              class = "lab-field",
              numericInput("article_lab_batch_size", "Batch size", value = 12L, min = 1L, max = 25L, width = "100%")
            ),
            div(
              class = "lab-field",
              textInput("article_lab_model", "Model", value = article_lab_default_model, width = "100%")
            ),
            div(
              class = "lab-field",
              textInput("article_lab_seed_topic", "Optional seed/topic", value = "", width = "100%", placeholder = "Optional article idea or angle")
            ),
            div(
              class = "lab-field",
              selectInput(
                "article_lab_inspiration_source",
                "Optional inspiration source",
                choices = c("", "manual prompt", "top performing titles", "custom"),
                selected = "",
                width = "100%"
              )
            )
          ),
          div(
            class = "lab-actions",
            actionButton("article_lab_generate", "Generate titles", class = "lab-primary"),
            actionButton("article_lab_save", "Save batch", class = "lab-secondary"),
            actionButton("article_lab_clear", "Clear draft", class = "lab-secondary")
          ),
          uiOutput("article_lab_notice")
        ),
        div(
          class = "lab-card",
          h3("Latest generated titles"),
          uiOutput("article_lab_latest_titles")
        )
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

  article_lab_saved_candidates <- reactive({
    article_lab_refresh()
    batch <- article_lab_saved_batch()
    if (is.null(batch)) return(data.frame())
    load_article_lab_candidates_for_batch(con, batch$batch_id[[1]])
  })

  article_lab_overview_stats <- reactive({
    article_lab_refresh()
    article_lab_overview(con)
  })

  output$article_lab_notice <- renderUI({
    notice <- article_lab_state$notice
    if (is.null(notice) || !nzchar(notice)) return(NULL)
    div(class = "lab-status-copy", notice)
  })

  output$article_lab_latest_titles <- renderUI({
    saved <- article_lab_saved_candidates()
    draft <- article_lab_state$draft
    if (!is.null(draft) && nrow(draft) > 0) {
      draft_rows <- data.frame(
        candidate_id = sprintf("draft_%02d", seq_len(nrow(draft))),
        title = draft$title,
        status = rep("draft", nrow(draft)),
        created_at = rep(article_lab_state$draft_created_at %||% now_utc(), nrow(draft)),
        batch_id = rep("(draft)", nrow(draft)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      rows <- draft_rows
    } else {
      rows <- saved
    }
    article_lab_table_ui(rows)
  })

  observeEvent(input$article_lab_generate, {
    generated <- generate_title_candidates(
      con = con,
      prompt = input$article_lab_prompt,
      batch_size = input$article_lab_batch_size,
      seed_topic = input$article_lab_seed_topic,
      inspiration_source = input$article_lab_inspiration_source,
      model = input$article_lab_model
    )
    article_lab_state$draft <- generated$titles
    article_lab_state$draft_created_at <- now_utc()
    article_lab_state$draft_meta <- generated
    if (identical(generated$mode, "api")) {
      example_copy <- if (isTRUE(generated$example_titles_used > 0)) {
        sprintf(" Used %s top-performing title examples as inspiration.", generated$example_titles_used)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "Generated %s draft titles with the OpenAI API using model %s.%s Save the batch to persist it to SQLite.",
        nrow(generated$titles),
        generated$model %||% article_lab_default_model,
        example_copy
      )
    } else {
      article_lab_state$notice <- sprintf(
        "API generation was unavailable, so the local stub helper generated %s draft titles instead. Reason: %s",
        nrow(generated$titles),
        generated$fallback_reason %||% "unknown error"
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save, {
    draft <- article_lab_state$draft
    draft_meta <- article_lab_state$draft_meta
    if (is.null(draft) || nrow(draft) == 0) {
      article_lab_state$notice <- "Nothing to save yet. Generate a draft first."
      return(invisible(NULL))
    }

    batch_id <- save_article_lab_batch(
      con,
      prompt = input$article_lab_prompt,
      seed_topic = input$article_lab_seed_topic,
      inspiration_source = input$article_lab_inspiration_source,
      requested_batch_size = input$article_lab_batch_size,
      model = input$article_lab_model,
      titles = draft$title,
      raw_json = if (is.null(draft_meta$raw_json)) NA_character_ else draft_meta$raw_json,
      generation_mode = draft_meta$mode %||% "generated"
    )
    article_lab_state$draft <- NULL
    article_lab_state$draft_created_at <- NULL
    article_lab_state$draft_meta <- NULL
    article_lab_state$notice <- sprintf(
      "Saved batch %s. Candidates are stored with status ready_for_human_rating. Generation mode: %s.",
      batch_id,
      draft_meta$mode %||% "generated"
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_clear, {
    article_lab_state$draft <- NULL
    article_lab_state$draft_created_at <- NULL
    article_lab_state$draft_meta <- NULL
    article_lab_state$notice <- "Cleared the unsaved draft."
  }, ignoreInit = TRUE)

  output$guide_content <- renderUI({
    if (identical(active_section(), "article_lab")) {
      overview <- article_lab_overview_stats()
      saved_batch <- article_lab_saved_batch()
      draft <- article_lab_state$draft
      draft_n <- if (is.null(draft)) 0L else nrow(draft)
      current_batch_label <- if (draft_n > 0) {
        sprintf("Unsaved draft with %s titles", draft_n)
      } else if (!is.null(saved_batch)) {
        sprintf("Saved batch %s", saved_batch$batch_id[[1]])
      } else {
        "No batch saved yet"
      }
      current_batch_meta <- if (draft_n > 0) {
        paste("Draft created at", article_lab_state$draft_created_at %||% now_utc())
      } else if (!is.null(saved_batch)) {
        paste("Created", saved_batch$created_at[[1]], "\u00b7 model", first_value(saved_batch, "model", article_lab_default_model))
      } else {
        "Generate first, then save to persist candidates."
      }

      return(tagList(
        div(
          class = "status-card",
          h3("Article Lab status"),
          div(class = "status-metric", overview$ready_for_human_rating[[1]]),
          p(sprintf("%s saved candidates marked ready for later human rating.", overview$ready_for_human_rating[[1]])),
          p(class = "lab-status-copy", sprintf("%s saved batches total.", overview$saved_batches[[1]]))
        ),
        div(
          class = "status-card",
          h3("Current batch"),
          p(current_batch_label),
          p(class = "lab-status-copy", current_batch_meta)
        ),
        div(
          class = "status-card",
          h3("Reminder"),
          p("Human rating happens in the normal rating queue. API scores should stay hidden until human ratings are complete."),
          p(class = "lab-status-copy", "Queue integration TODO: stored titles are marked ready_for_human_rating only in this MVP.")
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

    tagList(
      div(class = "guide-section", h3("How it works"), p("Rate each preview using only the visible headline, subtitle, and thumbnail.")),
      div(
        class = "guide-section",
        h3("Focus on"),
        tags$ul(
          tags$li("Headline clarity and hook"),
          tags$li("Topic relevance and appeal"),
          tags$li("Perceived value to readers"),
          tags$li("Your gut feeling")
        )
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
    isolate_title_field <- is_dimension_mode && !is.na(field) && field %in% title_isolation_dimension_fields
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

    subtitle <- displayed_subtitle_for_field(item, field)
    thumbnail_only <- is_dimension_mode && !is.na(field) && field %in% thumbnail_only_dimension_fields
    text_only <- is_dimension_mode && !is.na(field) && field %in% text_only_dimension_fields

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
      return(div(
        class = "rating-panel",
        div(class = "prompt", rating_prompt),
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
