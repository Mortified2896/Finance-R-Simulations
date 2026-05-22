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
  out <- list(db = file.path("data", "db", "medium_articles.sqlite"))
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--db") {
      i <- i + 1
      if (i > length(args)) stop("--db requires a path", call. = FALSE)
      out$db <- args[[i]]
    } else if (startsWith(arg, "--db=")) {
      out$db <- sub("^--db=", "", arg)
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1
  }
  out
}

scalar <- function(connection, sql) {
  value <- dbGetQuery(connection, sql)[1, 1]
  if (is.na(value)) 0 else value
}

print_metric <- function(label, value) {
  cat(sprintf("%-46s %s\n", paste0(label, ":"), format(value, big.mark = ",")))
}

table_or_view_exists <- function(connection, name) {
  dbExistsTable(connection, name)
}

table_columns <- function(connection, table_name) {
  dbGetQuery(
    connection,
    paste0("PRAGMA table_info(", dbQuoteIdentifier(connection, table_name), ")")
  )$name
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
database_path <- args$db

cat("Medium Analysis V2 Validation\n")
cat("=============================\n")
cat("DB path: ", database_path, "\n\n", sep = "")

if (!file.exists(database_path)) {
  stop("Could not find database at: ", database_path, call. = FALSE)
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

required_objects <- c(
  "medium_articles",
  "medium_tag_page_observations",
  "v_medium_canonical_articles",
  "v_medium_title_prediction_dataset_v2",
  "medium_title_api_scores",
  "medium_title_human_ratings",
  "medium_article_image_assets"
)
missing_objects <- required_objects[!vapply(required_objects, table_or_view_exists, logical(1), connection = connection)]
if (length(missing_objects) > 0) {
  cat("Missing V2 object(s): ", paste(missing_objects, collapse = ", "), "\n", sep = "")
  cat("Run: Rscript scripts/apply_medium_analysis_v2_schema.R\n")
  quit(status = 1)
}

cat("Core counts\n")
cat("-----------\n")
print_metric("medium_articles rows", scalar(connection, "SELECT COUNT(*) FROM medium_articles"))
print_metric(
  "raw observed article_id count",
  scalar(connection, "SELECT COUNT(DISTINCT article_id) FROM medium_tag_page_observations WHERE article_id IS NOT NULL")
)
print_metric(
  "raw observed medium_post_id count",
  scalar(connection, "SELECT COUNT(DISTINCT medium_post_id) FROM medium_tag_page_observations WHERE NULLIF(TRIM(medium_post_id), '') IS NOT NULL")
)
print_metric(
  "raw observed normalized URL count",
  scalar(connection, "SELECT COUNT(DISTINCT article_url_normalized) FROM medium_tag_page_observations WHERE NULLIF(TRIM(article_url_normalized), '') IS NOT NULL")
)
print_metric("canonical article view rows", scalar(connection, "SELECT COUNT(*) FROM v_medium_canonical_articles"))
print_metric("title prediction dataset V2 rows", scalar(connection, "SELECT COUNT(*) FROM v_medium_title_prediction_dataset_v2"))
print_metric(
  "duplicate medium_post_id groups",
  scalar(connection, "
    SELECT COUNT(*)
    FROM (
      SELECT medium_post_id
      FROM medium_articles
      WHERE NULLIF(TRIM(medium_post_id), '') IS NOT NULL
      GROUP BY medium_post_id
      HAVING COUNT(*) > 1
    )
  ")
)

cat("\nCoverage in V2 dataset\n")
cat("----------------------\n")
dataset_count <- scalar(connection, "SELECT COUNT(*) FROM v_medium_title_prediction_dataset_v2")
coverage <- function(column, predicate) {
  covered <- scalar(connection, paste0("SELECT COUNT(*) FROM v_medium_title_prediction_dataset_v2 WHERE ", predicate))
  sprintf("%s / %s (%.1f%%)", format(covered, big.mark = ","), format(dataset_count, big.mark = ","), if (dataset_count == 0) 0 else 100 * covered / dataset_count)
}
print_metric("title coverage", coverage("title", "NULLIF(TRIM(title), '') IS NOT NULL"))
print_metric("subtitle coverage", coverage("subtitle", "NULLIF(TRIM(subtitle), '') IS NOT NULL"))
print_metric("clap coverage", coverage("claps", "claps IS NOT NULL"))
print_metric("response coverage", coverage("responses", "responses IS NOT NULL"))
print_metric("publication_name coverage", coverage("publication_name", "NULLIF(TRIM(publication_name), '') IS NOT NULL"))
print_metric("thumbnail_url coverage", coverage("thumbnail_url", "NULLIF(TRIM(thumbnail_url), '') IS NOT NULL"))

cat("\nAPI score counts\n")
cat("----------------\n")
api_score_scope_expr <- if ("score_scope" %in% table_columns(connection, "medium_title_api_scores")) {
  "COALESCE(NULLIF(TRIM(score_scope), ''), 'title_subtitle')"
} else {
  "'legacy_title_subtitle'"
}
api_counts <- dbGetQuery(connection, paste0("
  SELECT
    prompt_version,
    model,
    ", api_score_scope_expr, " AS score_scope,
    COUNT(*) AS n
  FROM medium_title_api_scores
  GROUP BY prompt_version, model, score_scope
  ORDER BY n DESC, prompt_version, model, score_scope
"))
if (nrow(api_counts) == 0) {
  cat("No API scores yet.\n")
} else {
  print(api_counts, row.names = FALSE)
}

print_metric(
  "API medium_clap_potential count",
  scalar(connection, "SELECT COUNT(*) FROM medium_title_api_scores WHERE medium_clap_potential IS NOT NULL")
)
print_metric(
  "API medium_comment_potential count",
  scalar(connection, "SELECT COUNT(*) FROM medium_title_api_scores WHERE medium_comment_potential IS NOT NULL")
)
print_metric(
  "API overall_article_potential count",
  scalar(connection, "SELECT COUNT(*) FROM medium_title_api_scores WHERE overall_article_potential IS NOT NULL")
)

cat("\nHuman rating counts\n")
cat("-------------------\n")
rating_counts <- dbGetQuery(connection, "
  SELECT rater, rating_version, COUNT(*) AS n
  FROM medium_title_human_ratings
  GROUP BY rater, rating_version
  ORDER BY n DESC, rater, rating_version
")
if (nrow(rating_counts) == 0) {
  cat("No human ratings yet.\n")
} else {
  print(rating_counts, row.names = FALSE)
}

print_metric(
  "human general_rating coverage",
  sprintf(
    "%s / %s (%.1f%%)",
    format(scalar(connection, "SELECT COUNT(*) FROM medium_title_human_ratings WHERE general_rating IS NOT NULL"), big.mark = ","),
    format(scalar(connection, "SELECT COUNT(*) FROM medium_title_human_ratings"), big.mark = ","),
    if (scalar(connection, "SELECT COUNT(*) FROM medium_title_human_ratings") == 0) 0 else 100 * scalar(connection, "SELECT COUNT(*) FROM medium_title_human_ratings WHERE general_rating IS NOT NULL") / scalar(connection, "SELECT COUNT(*) FROM medium_title_human_ratings")
  )
)

if (table_or_view_exists(connection, "human_preview_dimension_ratings_v2")) {
  cat("\nHuman preview dimension rating counts\n")
  cat("-----------------------------------\n")
  preview_counts <- dbGetQuery(connection, "
    SELECT
      rating_mode,
      manifest_version,
      COUNT(*) AS n,
      COUNT(DISTINCT canonical_article_key) AS articles
    FROM human_preview_dimension_ratings_v2
    GROUP BY rating_mode, manifest_version
    ORDER BY n DESC, rating_mode, manifest_version
  ")
  if (nrow(preview_counts) == 0) {
    cat("No preview dimension ratings yet.\n")
  } else {
    print(preview_counts, row.names = FALSE)
  }

  preview_total <- scalar(connection, "
    SELECT COUNT(*)
    FROM human_preview_dimension_ratings_v2
    WHERE rating_mode = 'human_preview_dimensions_v2'
      AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
  ")

  print_metric(
    "preview personal_click_appeal coverage",
    sprintf(
      "%s / %s (%.1f%%)",
      format(scalar(connection, "
        SELECT COUNT(*)
        FROM human_preview_dimension_ratings_v2
        WHERE rating_mode = 'human_preview_dimensions_v2'
          AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
          AND personal_click_appeal IS NOT NULL
      "), big.mark = ","),
      format(preview_total, big.mark = ","),
      if (preview_total == 0) 0 else 100 * scalar(connection, "
        SELECT COUNT(*)
        FROM human_preview_dimension_ratings_v2
        WHERE rating_mode = 'human_preview_dimensions_v2'
          AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
          AND personal_click_appeal IS NOT NULL
      ") / preview_total
    )
  )
  print_metric(
    "preview title_hook_strength coverage",
    sprintf(
      "%s / %s (%.1f%%)",
      format(scalar(connection, "
        SELECT COUNT(*)
        FROM human_preview_dimension_ratings_v2
        WHERE rating_mode = 'human_preview_dimensions_v2'
          AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
          AND title_hook_strength IS NOT NULL
      "), big.mark = ","),
      format(preview_total, big.mark = ","),
      if (preview_total == 0) 0 else 100 * scalar(connection, "
        SELECT COUNT(*)
        FROM human_preview_dimension_ratings_v2
        WHERE rating_mode = 'human_preview_dimensions_v2'
          AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
          AND title_hook_strength IS NOT NULL
      ") / preview_total
    )
  )
  print_metric(
    "preview visual_hook coverage",
    sprintf(
      "%s / %s (%.1f%%)",
      format(scalar(connection, "
        SELECT COUNT(*)
        FROM human_preview_dimension_ratings_v2
        WHERE rating_mode = 'human_preview_dimensions_v2'
          AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
          AND visual_hook IS NOT NULL
      "), big.mark = ","),
      format(preview_total, big.mark = ","),
      if (preview_total == 0) 0 else 100 * scalar(connection, "
        SELECT COUNT(*)
        FROM human_preview_dimension_ratings_v2
        WHERE rating_mode = 'human_preview_dimensions_v2'
          AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
          AND visual_hook IS NOT NULL
      ") / preview_total
    )
  )
  print_metric(
    "preview emotional_pull coverage",
    sprintf(
      "%s / %s (%.1f%%)",
      format(scalar(connection, "
        SELECT COUNT(*)
        FROM human_preview_dimension_ratings_v2
        WHERE rating_mode = 'human_preview_dimensions_v2'
          AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
          AND emotional_pull_preview IS NOT NULL
      "), big.mark = ","),
      format(preview_total, big.mark = ","),
      if (preview_total == 0) 0 else 100 * scalar(connection, "
        SELECT COUNT(*)
        FROM human_preview_dimension_ratings_v2
        WHERE rating_mode = 'human_preview_dimensions_v2'
          AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
          AND emotional_pull_preview IS NOT NULL
      ") / preview_total
    )
  )
  print_metric(
    "preview ai_low_effort_flag coverage",
    sprintf(
      "%s / %s (%.1f%%)",
      format(scalar(connection, "
        SELECT COUNT(*)
        FROM human_preview_dimension_ratings_v2
        WHERE rating_mode = 'human_preview_dimensions_v2'
          AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
          AND ai_low_effort_flag IS NOT NULL
          AND TRIM(ai_low_effort_flag) <> ''
      "), big.mark = ","),
      format(preview_total, big.mark = ","),
      if (preview_total == 0) 0 else 100 * scalar(connection, "
        SELECT COUNT(*)
        FROM human_preview_dimension_ratings_v2
        WHERE rating_mode = 'human_preview_dimensions_v2'
          AND manifest_version = 'human_rated_thumbnail_valid_cohort_v2'
          AND ai_low_effort_flag IS NOT NULL
          AND TRIM(ai_low_effort_flag) <> ''
      ") / preview_total
    )
  )
}

cat("\nWarnings\n")
cat("--------\n")
suspicious_publications <- dbGetQuery(connection, "
  SELECT publication, COUNT(*) AS n
  FROM medium_articles
  WHERE publication IN ('Search', 'Write')
  GROUP BY publication
  ORDER BY n DESC, publication
")
if (nrow(suspicious_publications) > 0) {
  cat("Suspicious medium_articles.publication values found. Use tag-page publication_name for analysis:\n")
  print(suspicious_publications, row.names = FALSE)
} else {
  cat("No suspicious Search/Write publication labels found in medium_articles.publication.\n")
}

raw_observed <- scalar(connection, "SELECT COUNT(DISTINCT article_id) FROM medium_tag_page_observations WHERE article_id IS NOT NULL")
canonical_dataset <- scalar(connection, "SELECT COUNT(*) FROM v_medium_title_prediction_dataset_v2")
if (canonical_dataset < raw_observed) {
  cat(sprintf(
    "Canonical dataset has %s fewer rows than raw observed article_id count; verify this is explained by duplicate Medium post IDs.\n",
    format(raw_observed - canonical_dataset, big.mark = ",")
  ))
}

cat("Leakage reminder: API scoring input must only include title for title_only scope, or title and subtitle for title_subtitle scope.\n")
cat("V2.1 excludes click_potential because competitor clicks/views/reads are unavailable.\n")
cat("\nValidation complete.\n")
