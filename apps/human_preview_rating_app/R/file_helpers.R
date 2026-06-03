article_lab_debug_log <- function(event, details = list()) {
  log_dir <- file.path(project_root, ".local_gitignored")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  detail_text <- tryCatch(
    toJSON(details, auto_unbox = TRUE, null = "null"),
    error = function(e) paste0("{\"log_error\":", toJSON(conditionMessage(e), auto_unbox = TRUE), "}")
  )
  line <- paste(now_utc(), event, detail_text, sep = "\t")
  cat(line, "\n", file = file.path(log_dir, "article_lab_debug.log"), append = TRUE)
  message("Article Lab debug: ", event, " ", detail_text)
  invisible(NULL)
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
