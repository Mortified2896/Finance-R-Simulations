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
    if (length(kept_params) == 0) return(base_url)
    paste0(base_url, "?", paste(kept_params, collapse = "&"))
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
  candidates <- unique(c(env_root, getwd(), normalizePath(file.path(getwd(), ".."), mustWork = FALSE)))
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

path_matches_stem <- function(path, expected_stem) {
  stem <- path_stem(path)
  expected_stem <- clean_text(expected_stem)
  ok <- !is.na(stem) & !is.na(expected_stem) & startsWith(stem, expected_stem)
  ok[is.na(ok)] <- FALSE
  ok
}

file_sha256 <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_character_)
  hash <- suppressWarnings(tools::sha256sum(path))
  if (length(hash) == 0 || is.na(hash[[1]])) return(NA_character_)
  unname(hash[[1]])
}

match_queue_index <- function(article_id, medium_post_id, thumbnail_url, queue) {
  if (nrow(queue) == 0) return(NA_integer_)
  article_id <- clean_text(article_id)
  medium_post_id <- clean_text(medium_post_id)
  thumbnail_url <- normalize_image_url(thumbnail_url)

  if (!is.na(thumbnail_url)) {
    url_match <- which(queue$normalized_image_url == thumbnail_url |
      queue$primary_image_url_for_download_normalized == thumbnail_url)
    if (length(url_match) > 0) return(url_match[[1]])
  }

  if (!is.na(article_id)) {
    article_match <- which(vapply(queue$article_ids, function(x) article_id %in% split_keys(x), logical(1)))
    if (length(article_match) > 0) return(article_match[[1]])
  }

  if (!is.na(medium_post_id)) {
    post_match <- which(vapply(queue$medium_post_ids, function(x) medium_post_id %in% split_keys(x), logical(1)))
    if (length(post_match) > 0) return(post_match[[1]])
  }

  NA_integer_
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
  vapply(seq_len(nrow(rows)), function(i) {
    article_key(canonical[[i]], rows$article_id[[i]], rows$medium_post_id[[i]])
  }, character(1))
}

audit_stored_paths <- function(con, table_name, audit) {
  if (!dbExistsTable(con, table_name)) {
    return(data.frame(table = table_name, category = "table_missing", n = 1L))
  }
  columns <- dbGetQuery(con, sprintf("PRAGMA table_info(%s)", dbQuoteIdentifier(con, table_name)))$name
  needed <- c("article_id", "medium_post_id", "shown_thumbnail_path")
  if (table_name == "medium_thumbnail_api_scores") needed <- c("article_id", "medium_post_id", "local_image_path", "image_hash")
  if (!all(needed %in% columns)) {
    return(data.frame(table = table_name, category = "missing_columns", n = 1L))
  }

  canonical_expr <- if ("canonical_article_key" %in% columns) "canonical_article_key" else "NULL AS canonical_article_key"
  path_expr <- if (table_name == "medium_thumbnail_api_scores") {
    "local_image_path AS stored_path, image_hash AS stored_hash"
  } else {
    "shown_thumbnail_path AS stored_path, NULL AS stored_hash"
  }
  rows <- dbGetQuery(con, sprintf(
    "SELECT %s, article_id, medium_post_id, %s FROM %s",
    canonical_expr,
    path_expr,
    dbQuoteIdentifier(con, table_name)
  ))
  if (nrow(rows) == 0) {
    return(data.frame(table = table_name, category = "no_rows", n = 0L))
  }

  current_by_key <- audit[!is.na(audit$article_key), , drop = FALSE]
  current_by_key <- current_by_key[!duplicated(current_by_key$article_key), , drop = FALSE]
  match_index <- match(row_keys(rows), current_by_key$article_key)
  current_path <- current_by_key$resolved_local_image_path[match_index]
  current_status <- current_by_key$thumbnail_status[match_index]
  expected_stem <- current_by_key$expected_image_file_stem[match_index]
  current_hash <- vapply(current_path, file_sha256, character(1))

  stored_path <- as_abs_path(rows$stored_path, project_root)
  stored_exists <- !is.na(stored_path) & file.exists(stored_path)
  stored_stem_matches <- path_matches_stem(stored_path, expected_stem)
  same_path <- !is.na(current_path) & !is.na(stored_path) & normalizePath(current_path, mustWork = FALSE) == normalizePath(stored_path, mustWork = FALSE)
  stored_hash <- clean_text(rows$stored_hash)
  hash_differs <- table_name == "medium_thumbnail_api_scores" &
    same_path &
    !is.na(stored_hash) &
    !is.na(current_hash) &
    stored_hash != current_hash

  category <- ifelse(
    is.na(current_status) | current_status != "valid",
    "no_current_valid_thumbnail",
    ifelse(
      !stored_exists,
      "stored_path_missing_now",
      ifelse(
        !stored_stem_matches,
        "stored_path_stem_mismatch",
        ifelse(
          !same_path,
          "stored_path_differs_from_current_valid",
          ifelse(hash_differs, "stored_hash_differs_from_current_valid", "valid_same_thumbnail")
        )
      )
    )
  )

  counts <- as.data.frame(table(category), stringsAsFactors = FALSE)
  names(counts) <- c("category", "n")
  counts$table <- table_name
  counts[, c("table", "category", "n")]
}

project_root <- find_project_root()
db_path <- Sys.getenv("MEDIUM_RATING_DB", unset = file.path(project_root, "data", "db", "medium_articles.sqlite"))
queue_path <- file.path(project_root, "data", "analysis", "medium_images", "medium_image_download_queue.csv")
audit_path <- file.path(project_root, "data", "analysis", "medium_images", "thumbnail_mapping_audit.csv")

if (!file.exists(queue_path)) stop("Missing thumbnail queue: ", queue_path, call. = FALSE)

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

dataset <- dbGetQuery(con, "
  SELECT canonical_article_key, article_id, medium_post_id, title, subtitle, thumbnail_url
  FROM v_medium_title_prediction_dataset_v2
")
dataset$canonical_article_key <- clean_text(dataset$canonical_article_key)
dataset$article_id <- clean_text(dataset$article_id)
dataset$medium_post_id <- clean_text(dataset$medium_post_id)
dataset$title <- clean_text(dataset$title)
dataset$thumbnail_url <- clean_text(dataset$thumbnail_url)

queue <- read.csv(queue_path, stringsAsFactors = FALSE, check.names = FALSE)
for (column in c("normalized_image_url", "primary_image_url_for_download", "article_ids", "medium_post_ids", "image_file_stem", "local_image_path")) {
  if (!(column %in% names(queue))) queue[[column]] <- NA_character_
  queue[[column]] <- clean_text(queue[[column]])
}
queue$primary_image_url_for_download_normalized <- normalize_image_url(queue$primary_image_url_for_download)
queue$local_image_path_abs <- as_abs_path(queue$local_image_path, project_root)
lookup_maps <- queue_lookup_maps(queue)

match_index <- vapply(seq_len(nrow(dataset)), function(i) {
  lookup_queue_index(dataset$article_id[[i]], dataset$medium_post_id[[i]], dataset$thumbnail_url[[i]], lookup_maps)
}, integer(1))

resolved_path <- queue$local_image_path_abs[match_index]
expected_stem <- queue$image_file_stem[match_index]
actual_stem <- path_stem(resolved_path)
file_exists <- !is.na(resolved_path) & file.exists(resolved_path)
stem_matches <- path_matches_stem(resolved_path, expected_stem)

path_stems <- split(expected_stem[!is.na(resolved_path)], resolved_path[!is.na(resolved_path)])
duplicated_paths <- names(path_stems)[vapply(path_stems, function(x) length(unique(clean_text(x)[!is.na(clean_text(x))])) > 1, logical(1))]
duplicate_local_path <- !is.na(resolved_path) & resolved_path %in% duplicated_paths

thumbnail_status <- ifelse(
  is.na(clean_text(dataset$thumbnail_url)),
  "no_thumbnail_url",
  ifelse(
    is.na(match_index) | is.na(resolved_path),
    "missing_local_file",
    ifelse(
      !file_exists,
      "missing_local_file",
      ifelse(!stem_matches, "stem_mismatch", ifelse(duplicate_local_path, "duplicate_local_path", "valid"))
    )
  )
)
thumbnail_status[thumbnail_status != "valid" & !thumbnail_status %in% c("no_thumbnail_url", "missing_local_file", "stem_mismatch", "duplicate_local_path")] <- "stale_or_invalid"

audit <- data.frame(
  canonical_article_key = dataset$canonical_article_key,
  article_id = dataset$article_id,
  medium_post_id = dataset$medium_post_id,
  title = dataset$title,
  thumbnail_url = dataset$thumbnail_url,
  resolved_local_image_path = resolved_path,
  expected_image_file_stem = expected_stem,
  actual_file_stem = actual_stem,
  file_exists = file_exists,
  stem_matches = stem_matches,
  duplicate_local_path = duplicate_local_path,
  thumbnail_status = thumbnail_status,
  stringsAsFactors = FALSE
)
audit$article_key <- row_keys(audit)

dir.create(dirname(audit_path), recursive = TRUE, showWarnings = FALSE)
write.csv(audit[, setdiff(names(audit), "article_key")], audit_path, row.names = FALSE, na = "")

message("Saved thumbnail mapping audit to: ", audit_path)
message("Thumbnail mapping status counts:")
print(as.data.frame(table(audit$thumbnail_status), stringsAsFactors = FALSE), row.names = FALSE)

bad <- audit[audit$title == "10 Best Italian Cities to Retire to in 2026", , drop = FALSE]
if (nrow(bad) > 0) {
  message("\nSpecific article audit:")
  print(bad[, c(
    "canonical_article_key", "article_id", "medium_post_id", "thumbnail_url",
    "expected_image_file_stem", "resolved_local_image_path", "file_exists",
    "stem_matches", "duplicate_local_path", "thumbnail_status"
  )], row.names = FALSE)
}

message("\nStored thumbnail path audit:")
stored_counts <- do.call(rbind, list(
  audit_stored_paths(con, "human_preview_ratings", audit),
  audit_stored_paths(con, "human_preview_dimension_ratings", audit),
  audit_stored_paths(con, "medium_thumbnail_api_scores", audit)
))
print(stored_counts, row.names = FALSE)
