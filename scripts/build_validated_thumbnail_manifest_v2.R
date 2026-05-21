required_packages <- c("DBI", "RSQLite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

library(DBI)
library(RSQLite)

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

split_keys <- function(x) {
  value <- clean_text(x)
  if (length(value) == 0 || is.na(value)) return(character())
  parts <- unlist(strsplit(value, "[,;|]", perl = TRUE), use.names = FALSE)
  parts <- clean_text(parts)
  unique(parts[!is.na(parts)])
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
  path <- clean_text(path)
  vapply(path, function(one_path) {
    if (is.na(one_path)) return(NA_character_)
    if (grepl("^/", one_path)) one_path else file.path(project_root, one_path)
  }, character(1), USE.NAMES = FALSE)
}

path_stem <- function(path) {
  path <- clean_text(path)
  vapply(path, function(one_path) {
    if (is.na(one_path)) return(NA_character_)
    tools::file_path_sans_ext(basename(one_path))
  }, character(1), USE.NAMES = FALSE)
}

file_sha256 <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_character_)
  hash <- suppressWarnings(tools::sha256sum(path))
  if (length(hash) == 0 || is.na(hash[[1]])) return(NA_character_)
  unname(hash[[1]])
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

queue_lookup_maps <- function(queue) {
  url_map <- new.env(parent = emptyenv())
  article_map <- new.env(parent = emptyenv())
  post_map <- new.env(parent = emptyenv())
  put_first <- function(env, key, value) {
    key <- clean_text(key)
    if (length(key) == 0 || is.na(key)) return(invisible(NULL))
    if (!exists(key, envir = env, inherits = FALSE)) assign(key, value, envir = env)
    invisible(NULL)
  }

  for (i in seq_len(nrow(queue))) {
    put_first(url_map, queue$normalized_image_url[[i]], i)
    put_first(url_map, queue$primary_image_url_for_download_normalized[[i]], i)
    for (key in split_keys(queue$article_ids[[i]])) put_first(article_map, key, i)
    for (key in split_keys(queue$medium_post_ids[[i]])) put_first(post_map, key, i)
  }

  list(urls = url_map, article_ids = article_map, post_ids = post_map)
}

lookup_queue_index <- function(article_id, medium_post_id, thumbnail_url, maps) {
  get_first <- function(env, key) {
    key <- clean_text(key)
    if (length(key) == 0 || is.na(key) || !exists(key, envir = env, inherits = FALSE)) return(NA_integer_)
    get(key, envir = env, inherits = FALSE)
  }

  url_index <- get_first(maps$urls, normalize_image_url(thumbnail_url))
  if (!is.na(url_index)) return(url_index)

  article_index <- get_first(maps$article_ids, article_id)
  if (!is.na(article_index)) return(article_index)

  post_index <- get_first(maps$post_ids, medium_post_id)
  if (!is.na(post_index)) return(post_index)

  NA_integer_
}

first_non_missing_reason <- function(...) {
  parts <- list(...)
  out <- rep(NA_character_, length(parts[[1]]))
  for (part in parts) {
    use <- is.na(out) & !is.na(part)
    out[use] <- part[use]
  }
  out[is.na(out)] <- ""
  out
}

project_root <- find_project_root()
db_path <- Sys.getenv("MEDIUM_RATING_DB", unset = file.path(project_root, "data", "db", "medium_articles.sqlite"))
source_cohort_path <- file.path(project_root, "data", "analysis", "title_api_score_samples", "human_rated_thumbnail_all_v1.csv")
queue_path <- file.path(project_root, "data", "analysis", "medium_images", "medium_image_download_queue.csv")
manifest_path <- file.path(project_root, "data", "analysis", "medium_images", paste0(manifest_version, ".csv"))
invalid_audit_path <- file.path(project_root, "data", "analysis", "medium_images", paste0(manifest_version, "_invalid_audit.csv"))
sample_path <- file.path(project_root, "data", "analysis", "title_api_score_samples", "human_rated_thumbnail_valid_v2.csv")

if (!file.exists(queue_path)) stop("Missing thumbnail queue: ", queue_path, call. = FALSE)

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

objects <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type IN ('table', 'view')")
if (!("v_medium_title_prediction_dataset_v2" %in% objects$name)) {
  stop("Missing v_medium_title_prediction_dataset_v2. Run scripts/apply_medium_analysis_v2_schema.R first.", call. = FALSE)
}

dataset <- dbGetQuery(con, "
  SELECT canonical_article_key, article_id, medium_post_id, title, subtitle, thumbnail_url
  FROM v_medium_title_prediction_dataset_v2
  WHERE NULLIF(TRIM(title), '') IS NOT NULL
")
dataset$canonical_article_key <- clean_text(dataset$canonical_article_key)
dataset$article_id <- clean_text(dataset$article_id)
dataset$medium_post_id <- clean_text(dataset$medium_post_id)
dataset$title <- clean_text(dataset$title)
dataset$subtitle <- clean_text(dataset$subtitle)
dataset$thumbnail_url <- clean_text(dataset$thumbnail_url)

if (file.exists(source_cohort_path)) {
  source_cohort <- read.csv(source_cohort_path, stringsAsFactors = FALSE, check.names = FALSE)
  source_name <- source_cohort_path
} else if (dbExistsTable(con, "human_preview_ratings")) {
  source_cohort <- dbGetQuery(con, "
    SELECT DISTINCT article_id, medium_post_id
    FROM human_preview_ratings
    WHERE article_id IS NOT NULL OR NULLIF(TRIM(medium_post_id), '') IS NOT NULL
  ")
  source_name <- "human_preview_ratings fallback"
} else {
  stop("Missing source cohort and no human_preview_ratings fallback: ", source_cohort_path, call. = FALSE)
}

for (column in c("canonical_article_key", "article_id", "medium_post_id")) {
  if (!(column %in% names(source_cohort))) source_cohort[[column]] <- NA_character_
  source_cohort[[column]] <- clean_text(source_cohort[[column]])
}

dataset_key <- row_keys(dataset)
source_key <- row_keys(source_cohort)
by_canonical <- match(source_cohort$canonical_article_key, dataset$canonical_article_key)
by_article <- match(source_cohort$article_id, dataset$article_id)
by_post <- match(source_cohort$medium_post_id, dataset$medium_post_id)
match_index <- ifelse(!is.na(by_canonical), by_canonical, ifelse(!is.na(by_article), by_article, by_post))

matched <- dataset[match_index, , drop = FALSE]
matched[is.na(match_index), names(matched)] <- NA

queue <- read.csv(queue_path, stringsAsFactors = FALSE, check.names = FALSE)
for (column in c("normalized_image_url", "primary_image_url_for_download", "article_ids", "medium_post_ids", "image_file_stem", "local_image_path")) {
  if (!(column %in% names(queue))) queue[[column]] <- NA_character_
  queue[[column]] <- clean_text(queue[[column]])
}
queue$primary_image_url_for_download_normalized <- normalize_image_url(queue$primary_image_url_for_download)
queue$local_image_path_abs <- as_abs_path(queue$local_image_path, project_root)

lookup_maps <- queue_lookup_maps(queue)
queue_index <- vapply(seq_len(nrow(matched)), function(i) {
  if (is.na(match_index[[i]])) return(NA_integer_)
  lookup_queue_index(matched$article_id[[i]], matched$medium_post_id[[i]], matched$thumbnail_url[[i]], lookup_maps)
}, integer(1))

local_image_path <- queue$local_image_path[queue_index]
local_image_path_abs <- queue$local_image_path_abs[queue_index]
normalized_image_url <- queue$normalized_image_url[queue_index]
image_file_stem <- queue$image_file_stem[queue_index]
actual_stem <- path_stem(local_image_path_abs)
file_exists <- !is.na(local_image_path_abs) & file.exists(local_image_path_abs)
stem_matches <- !is.na(actual_stem) & !is.na(image_file_stem) & startsWith(actual_stem, image_file_stem)
stem_matches[is.na(stem_matches)] <- FALSE
image_sha256 <- vapply(local_image_path_abs, file_sha256, character(1))

article_identity <- row_keys(matched)
path_identity <- paste(normalized_image_url, article_identity, sep = "|")
path_groups <- split(path_identity[!is.na(local_image_path)], local_image_path[!is.na(local_image_path)])
duplicate_paths <- names(path_groups)[vapply(path_groups, function(values) {
  length(unique(clean_text(values)[!is.na(clean_text(values))])) > 1
}, logical(1))]
duplicate_local_path <- !is.na(local_image_path) & local_image_path %in% duplicate_paths

no_dataset_match <- is.na(match_index)
no_thumbnail_url <- !no_dataset_match & is.na(clean_text(matched$thumbnail_url))
no_queue_match <- !no_dataset_match & !no_thumbnail_url & is.na(queue_index)
missing_local_path <- !no_dataset_match & !no_thumbnail_url & !is.na(queue_index) & is.na(local_image_path_abs)
missing_local_file <- !no_dataset_match & !no_thumbnail_url & !is.na(queue_index) & !missing_local_path & !file_exists
stem_mismatch <- !no_dataset_match & !no_thumbnail_url & !is.na(queue_index) & file_exists & !stem_matches
duplicate_path_reason <- !no_dataset_match & !no_thumbnail_url & !is.na(queue_index) & file_exists & stem_matches & duplicate_local_path
hash_missing <- !no_dataset_match & !no_thumbnail_url & !is.na(queue_index) & file_exists & stem_matches & !duplicate_local_path & is.na(image_sha256)

invalid_reason <- first_non_missing_reason(
  ifelse(no_dataset_match, "source_row_not_found_in_v_medium_title_prediction_dataset_v2", NA_character_),
  ifelse(no_thumbnail_url, "missing_thumbnail_url", NA_character_),
  ifelse(no_queue_match, "missing_queue_match", NA_character_),
  ifelse(missing_local_path, "missing_local_image_path", NA_character_),
  ifelse(missing_local_file, "local_image_path_missing_file", NA_character_),
  ifelse(stem_mismatch, "filename_stem_mismatch", NA_character_),
  ifelse(duplicate_path_reason, "duplicate_local_image_path_in_cohort", NA_character_),
  ifelse(hash_missing, "image_sha256_unavailable", NA_character_)
)

valid <- invalid_reason == ""
thumbnail_status <- ifelse(valid, "valid", "invalid")

manifest <- data.frame(
  canonical_article_key = matched$canonical_article_key,
  article_id = matched$article_id,
  medium_post_id = matched$medium_post_id,
  title = matched$title,
  subtitle = matched$subtitle,
  thumbnail_url = matched$thumbnail_url,
  normalized_image_url = normalized_image_url,
  local_image_path = local_image_path,
  image_file_stem = image_file_stem,
  image_sha256 = image_sha256,
  thumbnail_status = thumbnail_status,
  invalid_reason = invalid_reason,
  stringsAsFactors = FALSE
)

valid_sample <- manifest[manifest$thumbnail_status == "valid", c(
  "canonical_article_key",
  "article_id",
  "medium_post_id",
  "title",
  "subtitle",
  "thumbnail_url",
  "local_image_path",
  "image_sha256"
), drop = FALSE]

dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(sample_path), recursive = TRUE, showWarnings = FALSE)
write.csv(manifest, manifest_path, row.names = FALSE, na = "")
write.csv(manifest[manifest$thumbnail_status != "valid", , drop = FALSE], invalid_audit_path, row.names = FALSE, na = "")
write.csv(valid_sample, sample_path, row.names = FALSE, na = "")

message("Validated thumbnail manifest v2")
message("Source cohort: ", source_name)
message("Manifest: ", manifest_path)
message("Invalid audit: ", invalid_audit_path)
message("Valid cohort sample: ", sample_path)
message("Status counts:")
print(as.data.frame(table(manifest$thumbnail_status), stringsAsFactors = FALSE), row.names = FALSE)
message("Invalid reason counts:")
print(as.data.frame(table(manifest$invalid_reason[manifest$thumbnail_status != "valid"]), stringsAsFactors = FALSE), row.names = FALSE)
