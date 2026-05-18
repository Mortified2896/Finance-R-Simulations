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
    sample_file = file.path("data", "analysis", "title_api_score_samples", "thumbnail_100_v1.csv")
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
    } else if (arg == "--sample-file") {
      i <- i + 1
      if (i > length(args)) stop("--sample-file requires a path", call. = FALSE)
      out$sample_file <- args[[i]]
    } else if (startsWith(arg, "--sample-file=")) {
      out$sample_file <- sub("^--sample-file=", "", arg)
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1
  }
  out
}

clean_text <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

read_csv_if_exists <- function(path) {
  if (!file.exists(path)) return(data.frame())
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

split_multi <- function(x) {
  parts <- unlist(strsplit(paste(x, collapse = ","), "[,;|]", perl = TRUE), use.names = FALSE)
  unique(clean_text(parts))
}

queue_matches_sample <- function(sample_df, queue_df) {
  if (nrow(sample_df) == 0 || nrow(queue_df) == 0) return(rep(FALSE, nrow(sample_df)))
  sample_keys <- clean_text(sample_df$canonical_article_key)
  sample_article_ids <- clean_text(sample_df$article_id)
  sample_post_ids <- clean_text(sample_df$medium_post_id)
  sample_urls <- clean_text(sample_df$thumbnail_url)

  vapply(seq_len(nrow(sample_df)), function(i) {
    local <- FALSE
    if (!is.na(sample_urls[[i]]) && "primary_image_url_for_download" %in% names(queue_df)) {
      local <- local || any(clean_text(queue_df$primary_image_url_for_download) == sample_urls[[i]], na.rm = TRUE)
    }
    if (!is.na(sample_urls[[i]]) && "normalized_image_url" %in% names(queue_df)) {
      local <- local || any(clean_text(queue_df$normalized_image_url) == sample_urls[[i]], na.rm = TRUE)
    }
    if (!is.na(sample_article_ids[[i]]) && "article_ids" %in% names(queue_df)) {
      local <- local || any(vapply(queue_df$article_ids, function(x) sample_article_ids[[i]] %in% split_multi(x), logical(1)))
    }
    if (!is.na(sample_post_ids[[i]]) && "medium_post_ids" %in% names(queue_df)) {
      local <- local || any(vapply(queue_df$medium_post_ids, function(x) sample_post_ids[[i]] %in% split_multi(x), logical(1)))
    }
    local
  }, logical(1))
}

queue_local_paths <- function(sample_df, queue_df) {
  if (nrow(sample_df) == 0 || nrow(queue_df) == 0 || !("local_image_path" %in% names(queue_df))) {
    return(rep(NA_character_, nrow(sample_df)))
  }
  out <- rep(NA_character_, nrow(sample_df))
  for (i in seq_len(nrow(sample_df))) {
    sample_url <- clean_text(sample_df$thumbnail_url[[i]])
    article_id <- clean_text(sample_df$article_id[[i]])
    post_id <- clean_text(sample_df$medium_post_id[[i]])
    keep <- rep(FALSE, nrow(queue_df))
    if (!is.na(sample_url) && "primary_image_url_for_download" %in% names(queue_df)) {
      keep <- keep | clean_text(queue_df$primary_image_url_for_download) == sample_url
    }
    if (!is.na(sample_url) && "normalized_image_url" %in% names(queue_df)) {
      keep <- keep | clean_text(queue_df$normalized_image_url) == sample_url
    }
    if (!is.na(article_id) && "article_ids" %in% names(queue_df)) {
      keep <- keep | vapply(queue_df$article_ids, function(x) article_id %in% split_multi(x), logical(1))
    }
    if (!is.na(post_id) && "medium_post_ids" %in% names(queue_df)) {
      keep <- keep | vapply(queue_df$medium_post_ids, function(x) post_id %in% split_multi(x), logical(1))
    }
    paths <- clean_text(queue_df$local_image_path[keep])
    paths <- paths[!is.na(paths) & file.exists(paths)]
    if (length(paths) > 0) out[[i]] <- paths[[1]]
  }
  out
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

message("Medium Thumbnail Asset Audit V1")
message("===============================")
message("DB path: ", args$db)
message("Sample file: ", args$sample_file)
message("This script opens SQLite read-only and does not modify the database.")

if (!file.exists(args$db)) stop("Could not find database at: ", args$db, call. = FALSE)
if (!file.exists(args$sample_file)) stop("Could not find sample file at: ", args$sample_file, call. = FALSE)

connection <- dbConnect(SQLite(), args$db, flags = SQLITE_RO)
on.exit(dbDisconnect(connection), add = TRUE)

objects <- dbGetQuery(connection, "SELECT name, type FROM sqlite_master WHERE type IN ('table', 'view')")
required <- c("v_medium_title_prediction_dataset_v2")
missing <- setdiff(required, objects$name)
if (length(missing) > 0) stop("Missing required object(s): ", paste(missing, collapse = ", "), call. = FALSE)

dataset_counts <- dbGetQuery(connection, "
  SELECT
    COUNT(*) AS rows,
    SUM(CASE WHEN NULLIF(TRIM(thumbnail_url), '') IS NOT NULL THEN 1 ELSE 0 END) AS rows_with_thumbnail_url,
    SUM(CASE WHEN COALESCE(has_thumbnail_url, 0) = 1 THEN 1 ELSE 0 END) AS rows_with_has_thumbnail_url
  FROM v_medium_title_prediction_dataset_v2
")

sample_df <- read.csv(args$sample_file, stringsAsFactors = FALSE, check.names = FALSE)
if (!("canonical_article_key" %in% names(sample_df))) {
  stop("Sample file must contain canonical_article_key", call. = FALSE)
}

sample_keys <- clean_text(sample_df$canonical_article_key)
sample_sql <- paste(sprintf("'%s'", gsub("'", "''", sample_keys[!is.na(sample_keys)])), collapse = ",")
cohort <- if (nzchar(sample_sql)) {
  dbGetQuery(connection, paste0("
    SELECT canonical_article_key, article_id, medium_post_id, thumbnail_url, has_thumbnail_url
    FROM v_medium_title_prediction_dataset_v2
    WHERE canonical_article_key IN (", sample_sql, ")
  "))
} else {
  data.frame()
}

image_assets_exists <- "medium_article_image_assets" %in% objects$name
asset_count <- if (image_assets_exists) dbGetQuery(connection, "SELECT COUNT(*) AS n FROM medium_article_image_assets")$n[[1]] else NA_integer_
asset_useful <- FALSE
asset_local_matches <- 0L
if (image_assets_exists && asset_count > 0 && nrow(cohort) > 0) {
  asset_local_matches <- dbGetQuery(connection, paste0("
    SELECT COUNT(DISTINCT d.canonical_article_key) AS n
    FROM v_medium_title_prediction_dataset_v2 d
    JOIN medium_article_image_assets a
      ON (
        a.canonical_article_key = d.canonical_article_key
        OR (a.article_id IS NOT NULL AND a.article_id = d.article_id)
        OR (NULLIF(TRIM(a.medium_post_id), '') IS NOT NULL AND a.medium_post_id = d.medium_post_id)
        OR a.image_url = d.thumbnail_url
      )
    WHERE d.canonical_article_key IN (", sample_sql, ")
      AND NULLIF(TRIM(a.local_path), '') IS NOT NULL
  "))$n[[1]]
  asset_useful <- asset_local_matches > 0
}

thumbnail_queue <- read_csv_if_exists(file.path("data", "analysis", "medium_images", "medium_image_download_queue.csv"))
body_queue <- read_csv_if_exists(file.path("data", "analysis", "medium_body_images", "medium_body_image_download_queue.csv"))
cohort_local_paths <- queue_local_paths(sample_df, thumbnail_queue)

likely_folders <- c(
  file.path("data", "analysis", "medium_images", "downloaded"),
  file.path("data", "analysis", "medium_body_images", "downloaded")
)

folder_counts <- data.frame(
  folder = likely_folders,
  exists = file.exists(likely_folders),
  file_count = vapply(likely_folders, function(path) {
    if (!dir.exists(path)) return(0L)
    length(list.files(path, recursive = TRUE, full.names = TRUE))
  }, integer(1)),
  stringsAsFactors = FALSE
)

thumbnail_queue_matches <- queue_matches_sample(sample_df, thumbnail_queue)
body_queue_matches <- queue_matches_sample(sample_df, body_queue)

message("")
message("Dataset:")
print(dataset_counts, row.names = FALSE)

message("")
message("Fixed thumbnail cohort:")
message("- sample CSV rows: ", nrow(sample_df))
message("- rows matched to V2 dataset: ", nrow(cohort))
message("- cohort rows with thumbnail_url in sample CSV: ", sum(!is.na(clean_text(sample_df$thumbnail_url))))
message("- cohort rows with local downloaded thumbnail path from thumbnail queue: ", sum(!is.na(cohort_local_paths)))
message("- cohort rows missing local downloaded thumbnail file: ", nrow(sample_df) - sum(!is.na(cohort_local_paths)))
message("- cohort rows joinable to thumbnail queue by URL/article_id/medium_post_id: ", sum(thumbnail_queue_matches))
message("- cohort rows joinable to body-image queue by URL/article_id/medium_post_id: ", sum(body_queue_matches))

message("")
message("medium_article_image_assets:")
message("- exists: ", image_assets_exists)
message("- rows: ", ifelse(is.na(asset_count), "NA", asset_count))
message("- local-path matches for cohort: ", asset_local_matches)
message("- useful for this cohort now: ", asset_useful)

message("")
message("Likely local image folders:")
print(folder_counts, row.names = FALSE)

message("")
message("Joinability notes:")
message("- Best current thumbnail mapping source: data/analysis/medium_images/medium_image_download_queue.csv")
message("- Local thumbnail files can be joined to the cohort through thumbnail_url, article_id, or medium_post_id when queue rows have local_image_path.")
message("- canonical_article_key is available in the sample and V2 dataset, but not in the image queue; use article_id/medium_post_id/url fallbacks.")
message("- data/analysis/medium_body_images/downloaded/ appears to contain in-article images, not necessarily feed thumbnails.")
