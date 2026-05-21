required_packages <- c("DBI", "RSQLite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

library(DBI)
library(RSQLite)

clean_text <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
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

file_sha256 <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_character_)
  hash <- suppressWarnings(tools::sha256sum(path))
  if (length(hash) == 0 || is.na(hash[[1]])) return(NA_character_)
  unname(hash[[1]])
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

parse_args <- function(args) {
  out <- list(
    article_id = NULL,
    medium_post_id = NULL,
    canonical_article_key = NULL,
    local_image_basename = NULL
  )
  i <- 1L
  while (i <= length(args)) {
    token <- args[[i]]
    if (startsWith(token, "--article-id=")) {
      out$article_id <- sub("^--article-id=", "", token)
    } else if (token == "--article-id" && i < length(args)) {
      i <- i + 1L
      out$article_id <- args[[i]]
    } else if (startsWith(token, "--medium-post-id=")) {
      out$medium_post_id <- sub("^--medium-post-id=", "", token)
    } else if (token == "--medium-post-id" && i < length(args)) {
      i <- i + 1L
      out$medium_post_id <- args[[i]]
    } else if (startsWith(token, "--canonical-article-key=")) {
      out$canonical_article_key <- sub("^--canonical-article-key=", "", token)
    } else if (token == "--canonical-article-key" && i < length(args)) {
      i <- i + 1L
      out$canonical_article_key <- args[[i]]
    } else if (startsWith(token, "--local-image-basename=")) {
      out$local_image_basename <- sub("^--local-image-basename=", "", token)
    } else if (token == "--local-image-basename" && i < length(args)) {
      i <- i + 1L
      out$local_image_basename <- args[[i]]
    }
    i <- i + 1L
  }
  out$article_id <- clean_text(out$article_id)
  out$medium_post_id <- clean_text(out$medium_post_id)
  out$canonical_article_key <- clean_text(out$canonical_article_key)
  out$local_image_basename <- clean_text(out$local_image_basename)
  out
}

matches_target <- function(df, target) {
  if (nrow(df) == 0) return(rep(FALSE, 0L))

  has <- function(name) name %in% names(df)
  canonical <- if (has("canonical_article_key")) clean_text(df$canonical_article_key) else rep(NA_character_, nrow(df))
  article_id <- if (has("article_id")) clean_text(df$article_id) else rep(NA_character_, nrow(df))
  medium_post_id <- if (has("medium_post_id")) clean_text(df$medium_post_id) else rep(NA_character_, nrow(df))
  local_path <- if (has("local_image_path")) clean_text(df$local_image_path) else rep(NA_character_, nrow(df))

  basename_match <- rep(FALSE, nrow(df))
  if (!is.na(target$local_image_basename)) {
    basename_match <- basename(local_path) == target$local_image_basename
    basename_match[is.na(basename_match)] <- FALSE
  }

  keep <- rep(FALSE, nrow(df))
  if (!is.na(target$canonical_article_key)) keep <- keep | (canonical == target$canonical_article_key)
  if (!is.na(target$article_id)) keep <- keep | (article_id == target$article_id)
  if (!is.na(target$medium_post_id)) keep <- keep | (medium_post_id == target$medium_post_id)
  keep <- keep | basename_match
  keep[is.na(keep)] <- FALSE
  keep
}

print_section <- function(title) {
  cat("\n")
  cat(title, "\n")
  cat(paste(rep("-", nchar(title)), collapse = ""), "\n", sep = "")
}

maybe_read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame())
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

target <- parse_args(commandArgs(trailingOnly = TRUE))
if (is.na(target$article_id)) target$article_id <- "61"
if (is.na(target$medium_post_id)) target$medium_post_id <- "7bbdee94fb13"
if (is.na(target$canonical_article_key)) target$canonical_article_key <- "post:7bbdee94fb13"
if (is.na(target$local_image_basename)) target$local_image_basename <- "00089_miro.medium.com.jpg"

project_root <- find_project_root()
db_path <- file.path(project_root, "data", "db", "medium_articles.sqlite")
cohort_path <- file.path(project_root, "data", "analysis", "medium_images", "human_rated_thumbnail_valid_cohort_v2.csv")
queue_path <- file.path(project_root, "data", "analysis", "medium_images", "medium_image_download_queue.csv")
source_cohort_path <- file.path(project_root, "data", "analysis", "title_api_score_samples", "human_rated_thumbnail_all_v1.csv")
valid_sample_path <- file.path(project_root, "data", "analysis", "title_api_score_samples", "human_rated_thumbnail_valid_v2.csv")

cat("audit_dimension_v2_thumbnail_provenance_row\n")
cat("project_root:", project_root, "\n")
cat("db_path:", db_path, "\n")
cat("target canonical_article_key:", target$canonical_article_key, "\n")
cat("target article_id:", target$article_id, "\n")
cat("target medium_post_id:", target$medium_post_id, "\n")
cat("target local_image_basename:", target$local_image_basename, "\n")

print_section("1) Cohort CSV Row")
cohort <- maybe_read_csv(cohort_path)
cohort_keep <- matches_target(cohort, target)
cohort_rows <- cohort[cohort_keep, , drop = FALSE]
cat("path:", cohort_path, "\n")
cat("matched_rows:", nrow(cohort_rows), "\n")
if (nrow(cohort_rows) > 0) {
  core <- intersect(c(
    "canonical_article_key", "article_id", "medium_post_id", "title", "subtitle",
    "thumbnail_url", "local_image_path", "image_sha256", "thumbnail_status",
    "normalized_image_url", "image_file_stem", "invalid_reason"
  ), names(cohort_rows))
  print(cohort_rows[, core, drop = FALSE], row.names = FALSE)
}

print_section("2) Source Cohort / Sample CSV Rows")
for (path in c(source_cohort_path, valid_sample_path)) {
  rows <- maybe_read_csv(path)
  keep <- matches_target(rows, target)
  out <- rows[keep, , drop = FALSE]
  cat("path:", path, "\n")
  cat("matched_rows:", nrow(out), "\n")
  if (nrow(out) > 0) print(out, row.names = FALSE)
  cat("\n")
}

print_section("3) Source DB Rows (medium_articles)")
con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

article_query <- dbGetQuery(
  con,
  "SELECT * FROM medium_articles WHERE id = ? OR medium_post_id = ? ORDER BY id",
  params = list(as.integer(target$article_id), target$medium_post_id)
)
cat("matched_rows:", nrow(article_query), "\n")
if (nrow(article_query) > 0) print(article_query, row.names = FALSE)

print_section("4) v_medium_title_prediction_dataset_v2 Rows")
view_query <- dbGetQuery(
  con,
  "SELECT * FROM v_medium_title_prediction_dataset_v2
   WHERE canonical_article_key = ? OR article_id = ? OR medium_post_id = ?",
  params = list(target$canonical_article_key, as.integer(target$article_id), target$medium_post_id)
)
cat("matched_rows:", nrow(view_query), "\n")
if (nrow(view_query) > 0) print(view_query, row.names = FALSE)

print_section("5) Queue CSV Rows")
queue <- maybe_read_csv(queue_path)
for (column in c(
  "normalized_image_url", "primary_image_url_for_download", "article_ids", "medium_post_ids",
  "local_image_path", "image_file_stem", "download_status", "status", "download_sha256",
  "content_sha256", "image_sha256", "notes"
)) {
  if (!(column %in% names(queue))) queue[[column]] <- NA_character_
}

article_pattern <- if (is.na(target$article_id)) NULL else paste0("(^|[,;|])\\s*", target$article_id, "\\s*($|[,;|])")
post_pattern <- if (is.na(target$medium_post_id)) NULL else paste0("(^|[,;|])\\s*", target$medium_post_id, "\\s*($|[,;|])")

queue_keep <- rep(FALSE, nrow(queue))
queue_keep <- queue_keep | (basename(clean_text(queue$local_image_path)) == target$local_image_basename)
if (!is.null(article_pattern)) queue_keep <- queue_keep | grepl(article_pattern, clean_text(queue$article_ids), perl = TRUE)
if (!is.null(post_pattern)) queue_keep <- queue_keep | grepl(post_pattern, clean_text(queue$medium_post_ids), perl = TRUE)

if (nrow(cohort_rows) > 0 && "thumbnail_url" %in% names(cohort_rows)) {
  cohort_thumb_norm <- normalize_image_url(cohort_rows$thumbnail_url)
  queue_keep <- queue_keep | clean_text(queue$normalized_image_url) %in% cohort_thumb_norm
  queue_keep <- queue_keep | normalize_image_url(queue$primary_image_url_for_download) %in% cohort_thumb_norm
}
queue_keep[is.na(queue_keep)] <- FALSE
queue_rows <- queue[queue_keep, , drop = FALSE]
cat("path:", queue_path, "\n")
cat("matched_rows:", nrow(queue_rows), "\n")
if (nrow(queue_rows) > 0) {
  cols <- intersect(c(
    "normalized_image_url", "primary_image_url_for_download",
    "article_ids", "medium_post_ids", "image_file_stem",
    "download_status", "status", "local_image_path",
    "download_sha256", "content_sha256", "image_sha256", "notes"
  ), names(queue_rows))
  print(queue_rows[, cols, drop = FALSE], row.names = FALSE)
}

print_section("6) Observation Tables (Upstream provenance)")
tag_obs <- dbGetQuery(
  con,
  "SELECT snapshot_id, article_id, medium_post_id, tag_slug, page_position, title, subtitle,
          thumbnail_url, thumbnail_source, thumbnail_confidence, thumbnail_status, observed_at
   FROM medium_tag_page_observations
   WHERE medium_post_id = ?
   ORDER BY observed_at",
  params = list(target$medium_post_id)
)
cat("medium_tag_page_observations matched_rows:", nrow(tag_obs), "\n")
if (nrow(tag_obs) > 0) {
  print(utils::head(tag_obs, 12), row.names = FALSE)
  if (nrow(tag_obs) > 12) cat("... (", nrow(tag_obs) - 12, " more rows)\n", sep = "")
  distinct_thumbs <- aggregate(observed_at ~ thumbnail_url, tag_obs, length)
  names(distinct_thumbs)[names(distinct_thumbs) == "observed_at"] <- "n"
  cat("distinct thumbnail_url counts for this post:\n")
  print(distinct_thumbs, row.names = FALSE)
}

sidebar_obs <- dbGetQuery(
  con,
  "SELECT id, article_id, medium_post_id, title, source_surface, source_url, source_file, observed_at
   FROM medium_search_sidebar_post_observations
   WHERE medium_post_id = ?
   ORDER BY observed_at",
  params = list(target$medium_post_id)
)
cat("medium_search_sidebar_post_observations matched_rows:", nrow(sidebar_obs), "\n")
if (nrow(sidebar_obs) > 0) print(sidebar_obs, row.names = FALSE)

text_obs <- dbGetQuery(
  con,
  "SELECT id, article_id, collected_at, source_type, thumbnail_url, thumbnail_source,
          thumbnail_confidence, thumbnail_status
   FROM medium_article_text_snapshots
   WHERE article_id = ?
   ORDER BY collected_at",
  params = list(as.integer(target$article_id))
)
cat("medium_article_text_snapshots (article_id) matched_rows:", nrow(text_obs), "\n")
if (nrow(text_obs) > 0) print(text_obs, row.names = FALSE)

print_section("7) Local Image File Identity")
image_candidates <- unique(clean_text(c(
  if ("local_image_path" %in% names(cohort_rows)) cohort_rows$local_image_path else NA_character_,
  if ("local_image_path" %in% names(queue_rows)) queue_rows$local_image_path else NA_character_
)))
image_candidates <- image_candidates[!is.na(image_candidates)]

if (length(image_candidates) == 0 && !is.na(target$local_image_basename)) {
  guess <- file.path("data", "analysis", "medium_images", "downloaded", target$local_image_basename)
  image_candidates <- guess
}

if (length(image_candidates) == 0) {
  cat("No local image candidates found.\n")
} else {
  for (rel_path in image_candidates) {
    abs_path <- as_abs_path(rel_path, project_root)
    exists <- !is.na(abs_path) && file.exists(abs_path)
    cat("relative_path:", rel_path, "\n")
    cat("absolute_path:", abs_path, "\n")
      cat("exists:", exists, "\n")
      if (exists) {
        cat("sha256:", file_sha256(abs_path), "\n")
        if (nzchar(Sys.which("sips"))) {
          dims_cmd <- sprintf("sips -g pixelWidth -g pixelHeight %s", shQuote(abs_path))
          dims <- tryCatch(system(dims_cmd, intern = TRUE, ignore.stderr = FALSE), error = function(e) character())
          if (length(dims) > 0) cat(paste(dims, collapse = "\n"), "\n")
        }
      }
    cat("\n")
  }
}

print_section("8) Script provenance reference")
build_script <- file.path(project_root, "scripts", "build_validated_thumbnail_manifest_v2.R")
cat("build_script:", build_script, "\n")
if (file.exists(build_script)) {
  cat("The validated cohort is generated by this script.\n")
} else {
  cat("Build script missing.\n")
}
