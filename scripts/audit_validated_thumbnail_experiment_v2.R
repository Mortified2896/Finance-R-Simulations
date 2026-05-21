required_packages <- c("DBI", "RSQLite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

library(DBI)
library(RSQLite)

manifest_version <- "human_rated_thumbnail_valid_cohort_v2"
manual_rating_mode <- "human_preview_dimensions_v2"
api_prompt_version <- "thumbnail_v1_validated"

clean_text <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
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

article_key <- function(canonical_article_key, article_id, medium_post_id) {
  canonical_article_key <- clean_text(canonical_article_key)
  article_id <- clean_text(article_id)
  medium_post_id <- clean_text(medium_post_id)
  if (!is.na(canonical_article_key)) return(paste0("canonical:", canonical_article_key))
  if (!is.na(article_id)) return(paste0("article:", article_id))
  if (!is.na(medium_post_id)) return(paste0("post:", medium_post_id))
  NA_character_
}

row_keys <- function(rows) {
  canonical <- if ("canonical_article_key" %in% names(rows)) rows$canonical_article_key else rep(NA_character_, nrow(rows))
  article_id <- if ("article_id" %in% names(rows)) rows$article_id else rep(NA_character_, nrow(rows))
  medium_post_id <- if ("medium_post_id" %in% names(rows)) rows$medium_post_id else rep(NA_character_, nrow(rows))
  vapply(seq_len(nrow(rows)), function(i) {
    article_key(canonical[[i]], article_id[[i]], medium_post_id[[i]])
  }, character(1))
}

table_columns <- function(con, table_name) {
  if (!dbExistsTable(con, table_name)) return(character())
  dbGetQuery(con, sprintf("PRAGMA table_info(%s)", dbQuoteIdentifier(con, table_name)))$name
}

project_root <- find_project_root()
db_path <- Sys.getenv("MEDIUM_RATING_DB", unset = file.path(project_root, "data", "db", "medium_articles.sqlite"))
manifest_path <- file.path(project_root, "data", "analysis", "medium_images", paste0(manifest_version, ".csv"))
audit_path <- file.path(project_root, "data", "analysis", "medium_images", "validated_thumbnail_experiment_v2_audit.csv")
summary_path <- file.path(project_root, "data", "analysis", "medium_images", "validated_thumbnail_experiment_v2_audit_summary.md")

if (!file.exists(manifest_path)) {
  stop("Missing validated manifest. Run scripts/build_validated_thumbnail_manifest_v2.R first: ", manifest_path, call. = FALSE)
}

manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
for (column in c("canonical_article_key", "article_id", "medium_post_id", "local_image_path", "image_sha256", "thumbnail_status")) {
  if (!(column %in% names(manifest))) manifest[[column]] <- NA_character_
  manifest[[column]] <- clean_text(manifest[[column]])
}
manifest <- manifest[manifest$thumbnail_status == "valid", , drop = FALSE]
manifest$experiment_key <- row_keys(manifest)
manifest <- manifest[!is.na(manifest$experiment_key), , drop = FALSE]

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

manual <- data.frame()
manual_columns <- table_columns(con, "human_preview_dimension_ratings_v2")
if (length(manual_columns) > 0) {
  required <- c("canonical_article_key", "article_id", "medium_post_id", "rating_mode", "manifest_version", "shown_thumbnail_path", "shown_image_sha256")
  if (all(required %in% manual_columns)) {
    manual <- dbGetQuery(con, "
      SELECT canonical_article_key, article_id, medium_post_id, rating_mode, manifest_version,
        shown_thumbnail_path, shown_image_sha256
      FROM human_preview_dimension_ratings_v2
      WHERE rating_mode = ? AND manifest_version = ?
    ", params = list(manual_rating_mode, manifest_version))
  }
}
if (nrow(manual) > 0) {
  manual$experiment_key <- row_keys(manual)
  manual <- manual[!duplicated(manual$experiment_key), , drop = FALSE]
}

api <- data.frame()
api_columns <- table_columns(con, "medium_thumbnail_api_scores")
if (all(c("canonical_article_key", "article_id", "medium_post_id", "prompt_version", "local_image_path", "image_hash") %in% api_columns)) {
  api <- dbGetQuery(con, "
    SELECT canonical_article_key, article_id, medium_post_id, score_scope, prompt_version,
      local_image_path, image_hash
    FROM medium_thumbnail_api_scores
    WHERE prompt_version = ?
  ", params = list(api_prompt_version))
}
if (nrow(api) > 0) {
  api$experiment_key <- row_keys(api)
}

manual_index <- if (nrow(manual) > 0) match(manifest$experiment_key, manual$experiment_key) else rep(NA_integer_, nrow(manifest))
manual_path <- if (nrow(manual) > 0) manual$shown_thumbnail_path[manual_index] else rep(NA_character_, nrow(manifest))
manual_hash <- if (nrow(manual) > 0) manual$shown_image_sha256[manual_index] else rep(NA_character_, nrow(manifest))
manual_present <- !is.na(manual_index)
manual_path_match <- manual_present & clean_text(manual_path) == clean_text(manifest$local_image_path)
manual_hash_match <- manual_present & clean_text(manual_hash) == clean_text(manifest$image_sha256)

api_rows <- integer(nrow(manifest))
api_matching_rows <- integer(nrow(manifest))
api_mismatched_rows <- integer(nrow(manifest))
if (nrow(api) > 0) {
  for (i in seq_len(nrow(manifest))) {
    subset <- api[api$experiment_key == manifest$experiment_key[[i]], , drop = FALSE]
    api_rows[[i]] <- nrow(subset)
    if (nrow(subset) > 0) {
      path_match <- clean_text(subset$local_image_path) == clean_text(manifest$local_image_path[[i]])
      hash_match <- clean_text(subset$image_hash) == clean_text(manifest$image_sha256[[i]])
      matches <- path_match & hash_match
      matches[is.na(matches)] <- FALSE
      api_matching_rows[[i]] <- sum(matches)
      api_mismatched_rows[[i]] <- sum(!matches)
    }
  }
}

audit <- data.frame(
  canonical_article_key = manifest$canonical_article_key,
  article_id = manifest$article_id,
  medium_post_id = manifest$medium_post_id,
  manifest_local_image_path = manifest$local_image_path,
  manifest_image_sha256 = manifest$image_sha256,
  manual_present = manual_present,
  manual_shown_thumbnail_path = manual_path,
  manual_shown_image_sha256 = manual_hash,
  manual_path_match = manual_path_match,
  manual_hash_match = manual_hash_match,
  api_rows = api_rows,
  api_matching_rows = api_matching_rows,
  api_mismatched_rows = api_mismatched_rows,
  stringsAsFactors = FALSE
)

summary_counts <- c(
  manifest_valid_rows = nrow(manifest),
  manual_rows_matching_manifest = sum(audit$manual_present & audit$manual_path_match & audit$manual_hash_match, na.rm = TRUE),
  manual_rows_mismatched = sum(audit$manual_present & !(audit$manual_path_match & audit$manual_hash_match), na.rm = TRUE),
  api_rows_matching_manifest = sum(audit$api_matching_rows, na.rm = TRUE),
  api_rows_mismatched = sum(audit$api_mismatched_rows, na.rm = TRUE),
  missing_manual_rows = sum(!audit$manual_present, na.rm = TRUE),
  missing_api_rows = sum(audit$api_rows == 0, na.rm = TRUE)
)

dir.create(dirname(audit_path), recursive = TRUE, showWarnings = FALSE)
write.csv(audit, audit_path, row.names = FALSE, na = "")
summary_lines <- c(
  "# Validated Thumbnail Experiment V2 Audit",
  "",
  paste0("- manifest_version: `", manifest_version, "`"),
  paste0("- manual_rating_mode: `", manual_rating_mode, "`"),
  paste0("- api_prompt_version: `", api_prompt_version, "`"),
  "",
  paste0("- ", names(summary_counts), ": ", as.integer(summary_counts))
)
writeLines(summary_lines, summary_path)

message("Saved validated thumbnail experiment audit to: ", audit_path)
message("Saved summary to: ", summary_path)
print(data.frame(metric = names(summary_counts), value = as.integer(summary_counts)), row.names = FALSE)
