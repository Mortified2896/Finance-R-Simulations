required_packages <- c("DBI", "RSQLite", "jsonlite")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("DBI", "RSQLite", "jsonlite"))',
    call. = FALSE
  )
}

library(DBI)
library(RSQLite)
library(jsonlite)

database_path <- Sys.getenv("MEDIUM_DB_PATH", file.path("data", "db", "medium_articles.sqlite"))
output_dir <- file.path("data", "analysis", "medium_body_images")
output_path <- file.path(output_dir, "medium_body_image_download_queue.csv")
download_dir <- file.path(output_dir, "downloaded")

message("Medium Body Image Download Queue Export")
message("=======================================")

if (!file.exists(database_path)) {
  stop("Could not find database at: ", database_path, call. = FALSE)
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
  stem_base <- paste0(sprintf("%05d", index), "_body_", domain)
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

is_probable_avatar_or_icon <- function(src, alt, width, height) {
  source_text <- paste(clean_text_vector(src), clean_text_vector(alt), collapse = " ")
  small_square <- !is.na(width) && !is.na(height) &&
    width <= 96 && height <= 96 && abs(width - height) <= 8
  grepl("resize:fill:64:64|avatar|profile picture|A clap icon|A response icon|Medium Logo", source_text, ignore.case = TRUE) ||
    small_square
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

if (!dbExistsTable(connection, "medium_article_text_snapshots")) {
  stop("medium_article_text_snapshots table does not exist in: ", database_path, call. = FALSE)
}

snapshots <- dbGetQuery(connection, "
  SELECT
    s.id AS snapshot_id,
    s.article_id,
    s.article_url_normalized,
    s.collected_at,
    s.images_json,
    a.medium_post_id,
    a.title,
    a.url,
    a.canonical_url,
    a.author,
    a.publication
  FROM medium_article_text_snapshots s
  LEFT JOIN medium_articles a
    ON s.article_id = a.id
  WHERE s.images_json IS NOT NULL
    AND trim(s.images_json) <> ''
    AND trim(s.images_json) <> '[]'
")

image_rows <- data.frame()

for (row_index in seq_len(nrow(snapshots))) {
  row <- snapshots[row_index, , drop = FALSE]
  parsed_images <- tryCatch(
    jsonlite::fromJSON(row$images_json[1], simplifyDataFrame = TRUE),
    error = function(error) data.frame()
  )

  if (!is.data.frame(parsed_images) || nrow(parsed_images) == 0 || !("src" %in% names(parsed_images))) {
    next
  }

  for (image_index in seq_len(nrow(parsed_images))) {
    src <- clean_text_vector(parsed_images$src[image_index])
    if (length(src) == 0 || is.na(src) || grepl("^data:|^blob:", src, ignore.case = TRUE)) {
      next
    }

    alt <- if ("alt" %in% names(parsed_images)) clean_text_vector(parsed_images$alt[image_index]) else NA_character_
    caption <- if ("caption" %in% names(parsed_images)) clean_text_vector(parsed_images$caption[image_index]) else NA_character_
    width <- if ("width" %in% names(parsed_images)) suppressWarnings(as.numeric(parsed_images$width[image_index])) else NA_real_
    height <- if ("height" %in% names(parsed_images)) suppressWarnings(as.numeric(parsed_images$height[image_index])) else NA_real_
    position <- if ("position" %in% names(parsed_images)) suppressWarnings(as.integer(parsed_images$position[image_index])) else image_index

    if (isTRUE(is_probable_avatar_or_icon(src, alt, width, height))) {
      next
    }

    image_rows <- rbind(
      image_rows,
      data.frame(
        body_image_url = src,
        body_image_position = position,
        body_image_alt = alt,
        body_image_caption = caption,
        body_image_width = width,
        body_image_height = height,
        snapshot_id = row$snapshot_id[1],
        article_id = row$article_id[1],
        medium_post_id = row$medium_post_id[1],
        title = row$title[1],
        url = row$url[1],
        canonical_url = row$canonical_url[1],
        article_url_normalized = row$article_url_normalized[1],
        author = row$author[1],
        publication = row$publication[1],
        collected_at = row$collected_at[1],
        stringsAsFactors = FALSE
      )
    )
  }
}

existing_queue <- if (file.exists(output_path)) {
  read.csv(output_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
} else {
  data.frame()
}

if (nrow(image_rows) > 0) {
  image_rows$normalized_image_url <- normalize_image_url(image_rows$body_image_url)
  image_rows <- image_rows[!is.na(image_rows$normalized_image_url), , drop = FALSE]
  image_groups <- split(image_rows, image_rows$normalized_image_url, drop = TRUE)

  queue <- do.call(rbind, lapply(seq_along(image_groups), function(index) {
    group <- image_groups[[index]]
    normalized_url <- names(image_groups)[index]
    data.frame(
      normalized_image_url = normalized_url,
      body_image_url = collapse_unique(group$body_image_url),
      image_kind = "body_image",
      image_url_domain = extract_domain(normalized_url),
      article_ids = collapse_unique(group$article_id),
      medium_post_ids = collapse_unique(group$medium_post_id),
      titles = collapse_unique(group$title),
      urls = collapse_unique(group$url),
      canonical_urls = collapse_unique(group$canonical_url),
      authors = collapse_unique(group$author),
      publications = collapse_unique(group$publication),
      snapshot_ids = collapse_unique(group$snapshot_id),
      body_image_positions = collapse_unique(group$body_image_position),
      body_image_alt = collapse_unique(group$body_image_alt),
      body_image_caption = collapse_unique(group$body_image_caption),
      body_image_widths = collapse_unique(group$body_image_width),
      body_image_heights = collapse_unique(group$body_image_height),
      first_collected_at = suppressWarnings(min(clean_text_vector(group$collected_at), na.rm = TRUE)),
      latest_collected_at = suppressWarnings(max(clean_text_vector(group$collected_at), na.rm = TRUE)),
      n_articles_using_image = length(unique(clean_text_vector(group$article_id))),
      n_snapshots_using_image = length(unique(clean_text_vector(group$snapshot_id))),
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
    body_image_url = character(),
    image_kind = character(),
    image_url_domain = character(),
    article_ids = character(),
    medium_post_ids = character(),
    titles = character(),
    urls = character(),
    canonical_urls = character(),
    authors = character(),
    publications = character(),
    snapshot_ids = character(),
    body_image_positions = character(),
    body_image_alt = character(),
    body_image_caption = character(),
    body_image_widths = character(),
    body_image_heights = character(),
    first_collected_at = character(),
    latest_collected_at = character(),
    n_articles_using_image = integer(),
    n_snapshots_using_image = integer(),
    image_file_stem = character(),
    download_status = character(),
    local_image_path = character(),
    notes = character(),
    stringsAsFactors = FALSE
  )
}

if (nrow(queue) > 0) {
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

message("Article text snapshots with images_json: ", nrow(snapshots))
message("Body image rows after filtering avatars/icons: ", nrow(image_rows))
message("Unique normalized body image URLs exported: ", nrow(queue))
message("Rows skipped because no body image URL after filtering: ", max(0, nrow(snapshots) - length(unique(image_rows$snapshot_id))))
message("\nSaved body image download queue to: ", output_path)
