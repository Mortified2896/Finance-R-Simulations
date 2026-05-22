required_packages <- c("DBI", "RSQLite")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("DBI", "RSQLite"))',
    call. = FALSE
  )
}

library(DBI)
library(RSQLite)

parse_args <- function(args) {
  out <- list(
    db = file.path("data", "db", "medium_articles.sqlite"),
    prompt_version = "v2_2",
    model = "gpt-5-mini",
    output_mode = "all"
  )

  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--db") {
      i <- i + 1
      if (i > length(args)) stop("--db requires a path", call. = FALSE)
      out$db <- args[[i]]
    } else if (startsWith(arg, "--db=")) {
      out$db <- sub("^--db=", "", arg)
    } else if (arg == "--prompt-version") {
      i <- i + 1
      if (i > length(args)) stop("--prompt-version requires a value", call. = FALSE)
      out$prompt_version <- args[[i]]
    } else if (startsWith(arg, "--prompt-version=")) {
      out$prompt_version <- sub("^--prompt-version=", "", arg)
    } else if (arg == "--model") {
      i <- i + 1
      if (i > length(args)) stop("--model requires a value", call. = FALSE)
      out$model <- args[[i]]
    } else if (startsWith(arg, "--model=")) {
      out$model <- sub("^--model=", "", arg)
    } else if (arg == "--output-mode") {
      i <- i + 1
      if (i > length(args)) stop("--output-mode requires a value", call. = FALSE)
      out$output_mode <- args[[i]]
    } else if (startsWith(arg, "--output-mode=")) {
      out$output_mode <- sub("^--output-mode=", "", arg)
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1
  }

  if (!(out$output_mode %in% c("latest", "snapshot", "both", "all"))) {
    stop("--output-mode must be latest, snapshot, both, or all", call. = FALSE)
  }

  out
}

table_or_view_exists <- function(connection, name) {
  dbExistsTable(connection, name)
}

scalar <- function(connection, sql, params = list()) {
  value <- dbGetQuery(connection, sql, params = params)[1, 1]
  if (is.na(value)) 0 else value
}

sanitize_key_part <- function(x) {
  x <- ifelse(is.na(x) || !nzchar(x), "all", x)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nzchar(x), x, "all")
}

selected_output_dirs <- function(base_dir, output_mode, timestamp, key) {
  dirs <- character()
  if (output_mode %in% c("latest", "both", "all")) {
    dirs <- c(dirs, file.path(base_dir, "latest"))
  }
  if (output_mode %in% c("snapshot", "both", "all")) {
    dirs <- c(dirs, file.path(base_dir, "runs", paste(timestamp, key, sep = "_")))
  }
  unique(dirs)
}

write_run_metadata <- function(output_dir, timestamp, args, database_path, row_count, key) {
  metadata <- data.frame(
    field = c("timestamp", "prompt_version", "model", "db_path", "output_folder", "script_name", "row_count", "snapshot_key"),
    value = c(
      timestamp,
      args$prompt_version,
      args$model,
      database_path,
      output_dir,
      "scripts/export_medium_analysis_v2_chatgpt_snapshot.R",
      as.character(row_count),
      key
    ),
    stringsAsFactors = FALSE
  )
  write.csv(metadata, file.path(output_dir, "run_metadata.csv"), row.names = FALSE)
  writeLines(paste(metadata$field, metadata$value, sep = ": "), file.path(output_dir, "run_metadata.txt"))
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
database_path <- args$db

if (!file.exists(database_path)) {
  stop("Could not find database at: ", database_path, call. = FALSE)
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

required_objects <- c(
  "v_medium_title_prediction_dataset_v2",
  "medium_title_api_scores",
  "human_preview_dimension_ratings_v2",
  "medium_title_human_ratings"
)
missing_objects <- required_objects[!vapply(required_objects, table_or_view_exists, logical(1), connection = connection)]
if (length(missing_objects) > 0) {
  stop("Missing required V2 object(s): ", paste(missing_objects, collapse = ", "), call. = FALSE)
}

base_output_dir <- file.path("data", "analysis", "medium_analysis_v2", "chatgpt_snapshot")
run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
snapshot_key <- paste(
  sanitize_key_part(args$prompt_version),
  sanitize_key_part(args$model),
  "combined_snapshot",
  sep = "__"
)
output_dirs <- selected_output_dirs(base_output_dir, args$output_mode, run_timestamp, snapshot_key)
for (dir in output_dirs) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

dataset <- dbGetQuery(connection, "
  SELECT
    canonical_article_key,
    article_id,
    medium_post_id,
    url,
    title,
    subtitle,
    author,
    publication_name,
    source_tag,
    claps,
    responses,
    success_score,
    top_20_percent,
    thumbnail_url
  FROM v_medium_title_prediction_dataset_v2
  ORDER BY canonical_article_key
")

api_wide <- dbGetQuery(connection, "
  WITH latest_api AS (
    SELECT *
    FROM (
      SELECT
        s.*,
        ROW_NUMBER() OVER (
          PARTITION BY s.canonical_article_key, s.prompt_version, s.model, s.score_scope
          ORDER BY COALESCE(s.scored_at, '') DESC, s.id DESC
        ) AS rn
      FROM medium_title_api_scores s
      WHERE s.prompt_version = ?
        AND s.model = ?
    )
    WHERE rn = 1
  )
  SELECT
    canonical_article_key,
    MAX(CASE WHEN score_scope = 'title_only' THEN medium_clap_potential END) AS api_title_only_medium_clap_potential,
    MAX(CASE WHEN score_scope = 'title_only' THEN medium_comment_potential END) AS api_title_only_medium_comment_potential,
    MAX(CASE WHEN score_scope = 'title_only' THEN overall_article_potential END) AS api_title_only_overall_article_potential,
    MAX(CASE WHEN score_scope = 'title_only' THEN predicted_success_bucket END) AS api_title_only_predicted_success_bucket,
    MAX(CASE WHEN score_scope = 'subtitle_only' THEN medium_clap_potential END) AS api_subtitle_only_medium_clap_potential,
    MAX(CASE WHEN score_scope = 'subtitle_only' THEN medium_comment_potential END) AS api_subtitle_only_medium_comment_potential,
    MAX(CASE WHEN score_scope = 'subtitle_only' THEN overall_article_potential END) AS api_subtitle_only_overall_article_potential,
    MAX(CASE WHEN score_scope = 'subtitle_only' THEN predicted_success_bucket END) AS api_subtitle_only_predicted_success_bucket,
    MAX(CASE WHEN score_scope = 'title_subtitle' THEN medium_clap_potential END) AS api_title_subtitle_medium_clap_potential,
    MAX(CASE WHEN score_scope = 'title_subtitle' THEN medium_comment_potential END) AS api_title_subtitle_medium_comment_potential,
    MAX(CASE WHEN score_scope = 'title_subtitle' THEN overall_article_potential END) AS api_title_subtitle_overall_article_potential,
    MAX(CASE WHEN score_scope = 'title_subtitle' THEN predicted_success_bucket END) AS api_title_subtitle_predicted_success_bucket
  FROM latest_api
  GROUP BY canonical_article_key
", params = list(args$prompt_version, args$model))

api_counts <- dbGetQuery(connection, "
  WITH latest_api AS (
    SELECT *
    FROM (
      SELECT
        s.*,
        ROW_NUMBER() OVER (
          PARTITION BY s.canonical_article_key, s.prompt_version, s.model, s.score_scope
          ORDER BY COALESCE(s.scored_at, '') DESC, s.id DESC
        ) AS rn
      FROM medium_title_api_scores s
      WHERE s.prompt_version = ?
        AND s.model = ?
    )
    WHERE rn = 1
  )
  SELECT
    score_scope,
    COUNT(*) AS rows,
    COUNT(DISTINCT canonical_article_key) AS articles,
    SUM(CASE WHEN overall_article_potential IS NOT NULL THEN 1 ELSE 0 END) AS scored_rows
  FROM latest_api
  GROUP BY score_scope
  ORDER BY score_scope
", params = list(args$prompt_version, args$model))

human_preview <- dbGetQuery(connection, "
  SELECT
    canonical_article_key,
    rating_mode,
    manifest_version,
    personal_click_appeal,
    title_hook_strength,
    visual_hook,
    emotional_pull_preview,
    ai_low_effort_flag,
    human_dimension_note,
    rated_at,
    shown_thumbnail_path,
    shown_image_sha256
  FROM human_preview_dimension_ratings_v2
  WHERE rating_mode = 'human_preview_dimensions_v2'
    AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
  ORDER BY canonical_article_key
")

title_human <- dbGetQuery(connection, "
  WITH latest_title_human AS (
    SELECT *
    FROM (
      SELECT
        r.*,
        ROW_NUMBER() OVER (
          PARTITION BY r.canonical_article_key, r.rater, r.rating_version
          ORDER BY COALESCE(r.rated_at, '') DESC, r.id DESC
        ) AS rn
      FROM medium_title_human_ratings r
    )
    WHERE rn = 1
  )
  SELECT
    canonical_article_key,
    MAX(CASE WHEN skipped = 0 THEN general_rating END) AS title_general_rating,
    MAX(CASE WHEN skipped = 1 THEN 1 ELSE 0 END) AS title_rating_skipped,
    MAX(rater) AS title_rating_rater,
    MAX(rating_version) AS title_rating_version,
    MAX(rated_at) AS title_rating_rated_at
  FROM latest_title_human
  GROUP BY canonical_article_key
")

combined <- merge(dataset, api_wide, by = "canonical_article_key", all.x = TRUE)
combined <- merge(combined, human_preview, by = "canonical_article_key", all.x = TRUE)
combined <- merge(combined, title_human, by = "canonical_article_key", all.x = TRUE)

api_present <- (!is.na(combined$api_title_only_overall_article_potential) |
  !is.na(combined$api_subtitle_only_overall_article_potential) |
  !is.na(combined$api_title_subtitle_overall_article_potential))
api_present[is.na(api_present)] <- FALSE

preview_present <- (!is.na(combined$personal_click_appeal) |
  !is.na(combined$title_hook_strength) |
  !is.na(combined$visual_hook) |
  !is.na(combined$emotional_pull_preview) |
  !is.na(combined$ai_low_effort_flag))
preview_present[is.na(preview_present)] <- FALSE

title_human_present <- (!is.na(combined$title_general_rating) |
  (!is.na(combined$title_rating_skipped) & combined$title_rating_skipped == 1))
title_human_present[is.na(title_human_present)] <- FALSE

api_joined_articles <- sum(api_present)
human_preview_joined_articles <- sum(preview_present)
title_human_joined_articles <- sum(title_human_present)
both_api_and_preview_articles <- sum(api_present & preview_present)
combined_399 <- combined[preview_present, , drop = FALSE]

preview_total <- nrow(human_preview)
preview_coverage <- data.frame(
  field = c(
    "personal_click_appeal",
    "title_hook_strength",
    "visual_hook",
    "emotional_pull_preview",
    "ai_low_effort_flag"
  ),
  filled_rows = c(
    sum(!is.na(human_preview$personal_click_appeal)),
    sum(!is.na(human_preview$title_hook_strength)),
    sum(!is.na(human_preview$visual_hook)),
    sum(!is.na(human_preview$emotional_pull_preview)),
    sum(!is.na(human_preview$ai_low_effort_flag) & trimws(human_preview$ai_low_effort_flag) != "")
  ),
  total_rows = preview_total,
  stringsAsFactors = FALSE
)
preview_coverage$coverage_pct <- if (preview_total == 0) 0 else round(100 * preview_coverage$filled_rows / preview_total, 1)

summary_lines <- c(
  "Medium Analysis V2 ChatGPT Snapshot",
  "===================================",
  "",
  paste0("Timestamp: ", run_timestamp),
  paste0("DB path: ", database_path),
  paste0("Prompt version: ", args$prompt_version),
  paste0("Model: ", args$model),
  "",
  "Core counts",
  "-----------",
  paste0("Dataset rows: ", nrow(dataset)),
  paste0("API-joined articles: ", api_joined_articles),
  paste0("Human preview dimension articles: ", human_preview_joined_articles),
  paste0("Title-only human rating articles: ", title_human_joined_articles),
  paste0("Articles with both API and preview ratings: ", both_api_and_preview_articles),
  "",
  "API scope counts",
  "----------------",
  if (nrow(api_counts) == 0) "No API rows found for the selected prompt/model." else apply(api_counts, 1, function(row) {
    paste0(
      row[["score_scope"]], ": ",
      row[["articles"]], " articles, ",
      row[["scored_rows"]], " scored rows"
    )
  }),
  "",
  "Human preview dimension coverage",
  "-------------------------------",
  if (nrow(preview_coverage) == 0) "No preview dimension rows found." else apply(preview_coverage, 1, function(row) {
    paste0(
      row[["field"]], ": ",
      row[["filled_rows"]], " / ",
      row[["total_rows"]], " (",
      row[["coverage_pct"]], "%)"
    )
  }),
  "",
  "Files",
  "-----",
  "chatgpt_snapshot_combined.csv: all 1,794 dataset rows with joined API and human rating columns.",
  "chatgpt_snapshot_399_articles.csv: only the 399 clean articles with preview-dimension human ratings and matching API fields.",
  "chatgpt_snapshot_api_coverage.csv: API row counts by scope for the selected prompt/model.",
  "chatgpt_snapshot_human_preview_coverage.csv: preview-dimension field coverage counts.",
  "chatgpt_snapshot_summary.txt: this summary."
)

for (dir in output_dirs) {
  write.csv(combined, file.path(dir, "chatgpt_snapshot_combined.csv"), row.names = FALSE)
  write.csv(combined_399, file.path(dir, "chatgpt_snapshot_399_articles.csv"), row.names = FALSE)
  write.csv(api_counts, file.path(dir, "chatgpt_snapshot_api_coverage.csv"), row.names = FALSE)
  write.csv(preview_coverage, file.path(dir, "chatgpt_snapshot_human_preview_coverage.csv"), row.names = FALSE)
  writeLines(summary_lines, file.path(dir, "chatgpt_snapshot_summary.txt"))
  write_run_metadata(dir, run_timestamp, args, database_path, nrow(combined), snapshot_key)
}

cat("Medium Analysis V2 ChatGPT Snapshot\n")
cat("===================================\n")
cat("DB path: ", database_path, "\n", sep = "")
cat("Prompt version: ", args$prompt_version, "\n", sep = "")
cat("Model: ", args$model, "\n", sep = "")
cat("Output dirs: ", paste(output_dirs, collapse = "; "), "\n", sep = "")
cat("Combined rows: ", nrow(combined), "\n", sep = "")
