input_path <- file.path("data", "analysis", "medium_analysis_v1", "medium_title_prediction_dataset.csv")
output_dir <- file.path("data", "analysis", "medium_images")
output_path <- file.path(output_dir, "medium_image_download_queue.csv")
download_dir <- file.path(output_dir, "downloaded")

message("Medium Image Download Queue Export")
message("==================================")

if (!file.exists(input_path)) {
  stop("Could not find title prediction dataset at: ", input_path, call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

clean_text_vector <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

collapse_unique <- function(x) {
  value <- sort(unique(clean_text_vector(x)))
  value <- value[!is.na(value)]
  if (length(value) == 0) {
    return(NA_character_)
  }
  paste(value, collapse = "; ")
}

normalize_image_url <- function(url) {
  value <- clean_text_vector(url)
  if (length(value) == 0) {
    return(character())
  }

  vapply(value, function(one_url) {
    if (is.na(one_url)) {
      return(NA_character_)
    }

    without_fragment <- sub("#.*$", "", one_url)
    split_url <- strsplit(without_fragment, "\\?", fixed = FALSE)[[1]]
    base_url <- split_url[1]

    if (length(split_url) == 1 || split_url[2] == "") {
      return(base_url)
    }

    query_params <- unlist(strsplit(split_url[2], "&", fixed = TRUE), use.names = FALSE)
    parameter_names <- sub("=.*$", "", query_params)
    tracking_param <- grepl("^utm_", parameter_names, ignore.case = TRUE) |
      tolower(parameter_names) %in% c("fbclid", "gclid")
    kept_params <- query_params[!tracking_param & query_params != ""]

    if (length(kept_params) == 0) {
      base_url
    } else {
      paste0(base_url, "?", paste(kept_params, collapse = "&"))
    }
  }, character(1), USE.NAMES = FALSE)
}

extract_domain <- function(url) {
  value <- clean_text_vector(url)
  if (length(value) == 0) {
    return(character())
  }

  vapply(value, function(one_url) {
    if (is.na(one_url) || !grepl("^[a-z][a-z0-9+.-]*://", one_url, ignore.case = TRUE)) {
      return(NA_character_)
    }
    sub("^[a-z][a-z0-9+.-]*://([^/?#]+).*$", "\\1", one_url, ignore.case = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

make_file_stem <- function(url, index) {
  domain <- extract_domain(url)
  domain[is.na(domain)] <- "image"
  stem_base <- paste0(sprintf("%05d", index), "_", domain)
  stem_base <- gsub("[^A-Za-z0-9._-]+", "_", stem_base)
  stem_base <- gsub("_+", "_", stem_base)
  stem_base <- sub("_$", "", stem_base)
  stem_base
}

first_existing_download_path <- function(stem) {
  clean_stem <- clean_text_vector(stem)
  if (length(clean_stem) == 0 || is.na(clean_stem) || !dir.exists(download_dir)) {
    return(NA_character_)
  }

  matches <- Sys.glob(file.path(download_dir, paste0(clean_stem, ".*")))
  if (length(matches) == 0) {
    return(NA_character_)
  }

  matches[1]
}

dataset <- read.csv(input_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
existing_queue <- if (file.exists(output_path)) {
  read.csv(output_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
} else {
  data.frame()
}

required_columns <- c(
  "primary_image_url_for_download",
  "primary_image_url_source"
)
missing_required_columns <- setdiff(required_columns, names(dataset))
if (length(missing_required_columns) > 0) {
  stop(
    "Title prediction dataset is missing required image column(s): ",
    paste(missing_required_columns, collapse = ", "),
    ". Run scripts/build_medium_title_prediction_dataset.R first.",
    call. = FALSE
  )
}

optional_columns <- c(
  "article_id",
  "medium_post_id",
  "title",
  "url",
  "canonical_url",
  "latest_tag_thumbnail_alt",
  "primary_image_url_provenance",
  "primary_image_url_confidence",
  "primary_image_url_status",
  "author",
  "publication"
)
for (column_name in setdiff(optional_columns, names(dataset))) {
  dataset[[column_name]] <- NA_character_
  warning("Dataset is missing optional column: ", column_name, "; queue field will be NA.", call. = FALSE)
}

dataset$primary_image_url_for_download <- clean_text_vector(dataset$primary_image_url_for_download)
dataset$primary_image_url_source <- clean_text_vector(dataset$primary_image_url_source)
dataset$normalized_image_url <- normalize_image_url(dataset$primary_image_url_for_download)

rows_with_image <- !is.na(dataset$primary_image_url_for_download) & !is.na(dataset$normalized_image_url)
image_rows <- dataset[rows_with_image, , drop = FALSE]

if (nrow(image_rows) > 0) {
  image_groups <- split(image_rows, image_rows$normalized_image_url, drop = TRUE)
  queue <- do.call(rbind, lapply(seq_along(image_groups), function(index) {
    group <- image_groups[[index]]
    normalized_url <- names(image_groups)[index]
    data.frame(
      normalized_image_url = normalized_url,
      primary_image_url_for_download = collapse_unique(group$primary_image_url_for_download),
      primary_image_url_source = collapse_unique(group$primary_image_url_source),
      primary_image_url_provenance = collapse_unique(group$primary_image_url_provenance),
      primary_image_url_confidence = collapse_unique(group$primary_image_url_confidence),
      primary_image_url_status = collapse_unique(group$primary_image_url_status),
      image_url_domain = extract_domain(normalized_url),
      article_ids = collapse_unique(group$article_id),
      medium_post_ids = collapse_unique(group$medium_post_id),
      titles = collapse_unique(group$title),
      urls = collapse_unique(group$url),
      canonical_urls = collapse_unique(group$canonical_url),
      thumbnail_alt = collapse_unique(group$latest_tag_thumbnail_alt),
      authors = collapse_unique(group$author),
      publications = collapse_unique(group$publication),
      n_articles_using_image = length(unique(clean_text_vector(group$article_id))),
      image_file_stem = make_file_stem(normalized_url, index),
      download_status = "",
      local_image_path = "",
      notes = "",
      stringsAsFactors = FALSE
    )
  }))
  queue <- queue[order(queue$normalized_image_url), , drop = FALSE]
} else {
  queue <- data.frame(
    normalized_image_url = character(),
    primary_image_url_for_download = character(),
    primary_image_url_source = character(),
    primary_image_url_provenance = character(),
    primary_image_url_confidence = character(),
    primary_image_url_status = character(),
    image_url_domain = character(),
    article_ids = character(),
    medium_post_ids = character(),
    titles = character(),
    urls = character(),
    canonical_urls = character(),
    thumbnail_alt = character(),
    authors = character(),
    publications = character(),
    n_articles_using_image = integer(),
    image_file_stem = character(),
    download_status = character(),
    local_image_path = character(),
    notes = character(),
    stringsAsFactors = FALSE
  )
}

if (nrow(queue) > 0) {
  for (column_name in c("download_status", "local_image_path", "notes")) {
    if (!(column_name %in% names(queue))) {
      queue[[column_name]] <- ""
    }
  }

  if (
    nrow(existing_queue) > 0 &&
    all(c("normalized_image_url", "download_status", "local_image_path", "notes") %in% names(existing_queue))
  ) {
    match_index <- match(queue$normalized_image_url, existing_queue$normalized_image_url)
    matched <- !is.na(match_index)

    for (column_name in c("download_status", "local_image_path", "notes")) {
      replacement <- clean_text_vector(existing_queue[[column_name]][match_index])
      fill <- matched & is.na(clean_text_vector(queue[[column_name]])) & !is.na(replacement)
      queue[[column_name]][fill] <- replacement[fill]
    }
  }

  missing_status <- is.na(clean_text_vector(queue$download_status))
  if (any(missing_status)) {
    recovered_paths <- vapply(queue$image_file_stem[missing_status], first_existing_download_path, character(1))
    recovered <- !is.na(recovered_paths)
    missing_indices <- which(missing_status)
    recovered_indices <- missing_indices[recovered]
    queue$download_status[recovered_indices] <- "downloaded"
    queue$local_image_path[recovered_indices] <- recovered_paths[recovered]
    queue$notes[recovered_indices] <- "Recovered existing local file during queue export"
  }
}

write.csv(queue, output_path, row.names = FALSE, na = "")

source_counts <- sort(table(dataset$primary_image_url_source), decreasing = TRUE)
source_lines <- if (length(source_counts) == 0) {
  "  (none)"
} else {
  paste0("  ", names(source_counts), ": ", as.integer(source_counts))
}

message("Rows in title prediction dataset: ", nrow(dataset))
message("Rows with primary image URL: ", sum(rows_with_image))
message("Rows skipped because image URL missing: ", sum(!rows_with_image))
message("Unique normalized image URLs exported: ", nrow(queue))
message("Top image URL sources by count:")
message(paste(source_lines, collapse = "\n"))
message("\nSaved image download queue to: ", output_path)
