required_packages <- c("DBI", "RSQLite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

library(DBI)
library(RSQLite)

rating_mode <- "human_preview_dimensions_v2"
manifest_version <- "human_rated_thumbnail_valid_cohort_v2"

clean_text <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

row_key <- function(canonical_article_key, article_id, medium_post_id) {
  canonical_key <- clean_text(canonical_article_key)
  article_key <- clean_text(article_id)
  post_key <- clean_text(medium_post_id)
  if (!is.na(canonical_key)) return(paste0("canonical:", canonical_key))
  if (!is.na(article_key)) return(paste0("article:", article_key))
  if (!is.na(post_key)) return(paste0("post:", post_key))
  NA_character_
}

row_keys <- function(rows) {
  if (nrow(rows) == 0) return(character())
  vapply(seq_len(nrow(rows)), function(i) {
    row_key(rows$canonical_article_key[[i]], rows$article_id[[i]], rows$medium_post_id[[i]])
  }, character(1))
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

as_abs_path <- function(path, project_root) {
  value <- clean_text(path)
  if (is.na(value)) return(NA_character_)
  if (grepl("^/", value)) value else file.path(project_root, value)
}

hash_file <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_character_)
  hash <- suppressWarnings(tools::sha256sum(path))
  if (length(hash) == 0 || is.na(hash[[1]])) return(NA_character_)
  unname(hash[[1]])
}

project_root <- find_project_root()
db_path <- Sys.getenv("MEDIUM_RATING_DB", unset = file.path(project_root, "data", "db", "medium_articles.sqlite"))
cohort_path <- file.path(project_root, "data", "analysis", "medium_images", paste0(manifest_version, ".csv"))

if (!file.exists(cohort_path)) {
  stop("Missing validated cohort CSV: ", cohort_path, call. = FALSE)
}
if (!file.exists(db_path)) {
  stop("Missing SQLite DB: ", db_path, call. = FALSE)
}

cohort <- read.csv(cohort_path, stringsAsFactors = FALSE, check.names = FALSE)
for (column in c("canonical_article_key", "article_id", "medium_post_id", "local_image_path", "image_sha256", "thumbnail_status")) {
  if (!(column %in% names(cohort))) cohort[[column]] <- NA_character_
  cohort[[column]] <- clean_text(cohort[[column]])
}
cohort <- cohort[cohort$thumbnail_status == "valid", , drop = FALSE]

cohort$cohort_key <- row_keys(cohort)
cohort$local_image_path_abs <- vapply(cohort$local_image_path, as_abs_path, character(1), project_root = project_root, USE.NAMES = FALSE)
cohort$current_image_sha256 <- vapply(cohort$local_image_path_abs, hash_file, character(1), USE.NAMES = FALSE)
cohort$file_exists <- !is.na(cohort$local_image_path_abs) & file.exists(cohort$local_image_path_abs)
cohort$hash_matches <- !is.na(cohort$current_image_sha256) &
  !is.na(cohort$image_sha256) &
  cohort$current_image_sha256 == cohort$image_sha256
cohort$hash_matches[is.na(cohort$hash_matches)] <- FALSE

cohort_key_counts <- table(cohort$cohort_key[!is.na(cohort$cohort_key)])
duplicate_cohort_keys <- names(cohort_key_counts[cohort_key_counts > 1])
cohort_invalid <- is.na(cohort$cohort_key) | !cohort$file_exists | !cohort$hash_matches | (cohort$cohort_key %in% duplicate_cohort_keys)
cohort_invalid[is.na(cohort_invalid)] <- TRUE

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

queue <- dbGetQuery(con, "
  SELECT queue_position, active_dimension, article_id, medium_post_id, canonical_article_key, status
  FROM human_preview_dimension_pass_queue
  WHERE rating_mode = ?
    AND status IN ('pending', 'rated')
  ORDER BY active_dimension, queue_position
", params = list(rating_mode))

if (nrow(queue) == 0) {
  stop("No pending/rated queue rows found for rating_mode=", rating_mode, call. = FALSE)
}

queue$queue_key <- row_keys(queue)
queue$cohort_match_count <- vapply(queue$queue_key, function(key) {
  if (is.na(key) || !(key %in% names(cohort_key_counts))) return(0L)
  as.integer(cohort_key_counts[[key]])
}, integer(1))
queue$maps_exactly_one <- queue$cohort_match_count == 1L
queue_invalid <- is.na(queue$queue_key) | !queue$maps_exactly_one
queue_invalid[is.na(queue_invalid)] <- TRUE

cat("validate_dimension_v2_thumbnail_queue_mapping\n")
cat("manifest_version:", manifest_version, "\n")
cat("rating_mode:", rating_mode, "\n\n")
cat("cohort_valid_rows:", nrow(cohort), "\n")
cat("cohort_invalid_rows:", sum(cohort_invalid), "\n")
cat("cohort_duplicate_keys:", length(duplicate_cohort_keys), "\n")
cat("queue_rows_checked:", nrow(queue), "\n")
cat("queue_invalid_rows:", sum(queue_invalid), "\n")
cat("queue_exact_one_matches:", sum(queue$maps_exactly_one, na.rm = TRUE), "\n\n")

if (any(cohort_invalid)) {
  cat("Example invalid cohort rows:\n")
  print(utils::head(cohort[cohort_invalid, c(
    "canonical_article_key", "article_id", "medium_post_id",
    "local_image_path", "local_image_path_abs", "image_sha256",
    "current_image_sha256", "file_exists", "hash_matches"
  )], 10), row.names = FALSE)
  cat("\n")
}

if (any(queue_invalid)) {
  cat("Example invalid queue mappings:\n")
  print(utils::head(queue[queue_invalid, c(
    "active_dimension", "queue_position", "status",
    "canonical_article_key", "article_id", "medium_post_id",
    "queue_key", "cohort_match_count"
  )], 10), row.names = FALSE)
  cat("\n")
}

if (any(cohort_invalid) || any(queue_invalid)) {
  stop("Validation failed: cohort and/or queue mapping mismatches detected.", call. = FALSE)
}

cat("Validation passed.\n")
