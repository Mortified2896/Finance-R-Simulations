clean_text <- function(x) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) {
    return(NA_character_)
  }

  value <- trimws(as.character(x)[1])
  value <- gsub("\u00a0", " ", value, fixed = TRUE)
  value <- gsub("\\s+", " ", value)

  if (identical(value, "")) {
    return(NA_character_)
  }

  value
}

is_missing_text <- function(x) {
  length(x) == 0 || is.null(x) || all(is.na(x)) || identical(trimws(as.character(x)[1]), "")
}

current_timestamp <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

infer_page_variant_from_url <- function(tag_url) {
  normalized_url <- normalize_medium_url(tag_url)

  if (is.na(normalized_url)) {
    return("unknown")
  }

  if (grepl("/tag/[^/]+/recommended$", normalized_url, ignore.case = TRUE)) {
    return("tag_recommended")
  }

  if (grepl("/tag/[^/]+$", normalized_url, ignore.case = TRUE)) {
    return("tag_landing")
  }

  "unknown"
}

scalar_from_json <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  if (is.logical(x)) {
    return(x[1])
  }

  clean_text(x[1])
}

integer_from_json <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_integer_)
  }

  suppressWarnings(as.integer(x[1]))
}

real_from_json <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(x[1]))
}

logical_flag_from_json <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_integer_)
  }

  if (isTRUE(x[1])) {
    return(1L)
  }

  if (identical(x[1], FALSE)) {
    return(0L)
  }

  NA_integer_
}

normalize_medium_url <- function(x) {
  value <- clean_text(x)

  if (is.na(value)) {
    return(NA_character_)
  }

  if (grepl("^//", value)) {
    value <- paste0("https:", value)
  } else if (grepl("^/", value)) {
    value <- paste0("https://medium.com", value)
  }

  value <- sub("\\?.*$", "", value)
  value <- sub("#.*$", "", value)
  value <- sub("/+$", "", value)

  if (grepl("^https?://[^/]+$", value, ignore.case = TRUE)) {
    return(value)
  }

  value
}

extract_medium_post_id <- function(url) {
  normalized_url <- normalize_medium_url(url)

  if (is.na(normalized_url)) {
    return(NA_character_)
  }

  match <- regmatches(
    normalized_url,
    regexpr("([A-Fa-f0-9]{12})(?:/?$)", normalized_url, perl = TRUE)
  )

  if (length(match) == 0 || identical(match, character(0))) {
    return(NA_character_)
  }

  tolower(gsub("[^A-Fa-f0-9]", "", match[1]))
}

compute_source_file_hash <- function(file_path) {
  hash <- tryCatch(
    unname(tools::md5sum(file_path)[1]),
    error = function(error) NA_character_
  )

  clean_text(hash)
}

table_columns <- function(connection, table_name) {
  if (!dbExistsTable(connection, table_name)) {
    return(character())
  }

  dbGetQuery(connection, paste0("PRAGMA table_info(", table_name, ")"))$name
}

add_column_if_missing <- function(connection, table_name, column_name, column_definition, warn_only = FALSE) {
  existing_columns <- table_columns(connection, table_name)

  if (column_name %in% existing_columns) {
    return(TRUE)
  }

  result <- tryCatch(
    {
      dbExecute(connection, paste("ALTER TABLE", table_name, "ADD COLUMN", column_name, column_definition))
      TRUE
    },
    error = function(error) {
      if (isTRUE(warn_only)) {
        warning(
          "Could not add column ",
          table_name,
          ".",
          column_name,
          ": ",
          conditionMessage(error),
          call. = FALSE
        )
        return(FALSE)
      }

      stop(
        "Could not add column ",
        table_name,
        ".",
        column_name,
        ": ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  result
}

inspect_medium_articles_schema <- function(connection) {
  columns <- table_columns(connection, "medium_articles")

  list(
    columns = columns,
    has_id = "id" %in% columns,
    has_url = "url" %in% columns,
    has_canonical_url = "canonical_url" %in% columns,
    has_raw_url = "raw_url" %in% columns,
    has_medium_post_id = "medium_post_id" %in% columns,
    has_snippet = "snippet" %in% columns,
    has_subtitle = "subtitle" %in% columns,
    has_description_html = "description_html" %in% columns,
    has_author = "author" %in% columns,
    has_publication = "publication" %in% columns,
    has_publication_status = "publication_status" %in% columns,
    has_image_url = "image_url" %in% columns,
    has_image_url_manual = "image_url_manual" %in% columns,
    has_image_url_source = "image_url_source" %in% columns,
    has_image_url_confidence = "image_url_confidence" %in% columns,
    has_image_url_status = "image_url_status" %in% columns,
    has_content_text = "content_text" %in% columns,
    has_content_encoded_html = "content_encoded_html" %in% columns,
    has_visible_article_text = "visible_article_text" %in% columns,
    has_last_seen_at = "last_seen_at" %in% columns,
    full_content_columns = intersect(c("content_text", "content_encoded_html"), columns)
  )
}

ensure_medium_articles_schema <- function(connection) {
  invisible(dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_articles (
      id INTEGER PRIMARY KEY,
      source_tag TEXT NOT NULL,
      title TEXT NOT NULL,
      url TEXT UNIQUE NOT NULL,
      raw_url TEXT,
      guid TEXT,
      author TEXT,
      published_at TEXT,
      updated_at TEXT,
      fetched_at TEXT NOT NULL,
      categories TEXT,
      snippet TEXT,
      image_url TEXT,
      description_html TEXT,
      content_encoded_html TEXT,
      content_text TEXT,
      canonical_url TEXT,
      subtitle TEXT,
      publication TEXT,
      published_date_manual TEXT,
      modified_date_manual TEXT,
      read_time TEXT,
      is_member_only INTEGER,
      author_followers_raw TEXT,
      publication_followers_raw TEXT,
      medium_post_id TEXT,
      image_url_manual TEXT,
      image_url_source TEXT,
      image_url_confidence TEXT,
      image_url_status TEXT,
      visible_article_text TEXT,
      visible_text_word_count INTEGER,
      visible_article_text_truncated INTEGER,
      visible_article_text_max_chars INTEGER,
      visible_article_collected_at TEXT,
      manual_relevance_status TEXT,
      manual_relevance_checked_at TEXT,
      manual_relevance_note TEXT,
      publication_status TEXT,
      is_own_article INTEGER DEFAULT 0,
      own_article_source TEXT,
      own_article_detected_at TEXT,
      last_seen_at TEXT
    )
  "))

  add_column_if_missing(connection, "medium_articles", "raw_url", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "guid", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "author", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "published_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "updated_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "categories", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "snippet", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "image_url", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "description_html", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "content_encoded_html", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "content_text", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "canonical_url", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "subtitle", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "publication", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "published_date_manual", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "modified_date_manual", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "read_time", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "is_member_only", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "author_followers_raw", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "publication_followers_raw", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "medium_post_id", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "image_url_manual", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "image_url_source", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "image_url_confidence", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "image_url_status", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "visible_article_text", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "visible_text_word_count", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "visible_article_text_truncated", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "visible_article_text_max_chars", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "visible_article_collected_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "manual_relevance_status", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "manual_relevance_checked_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "manual_relevance_note", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "publication_status", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "is_own_article", "INTEGER DEFAULT 0", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "own_article_source", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "own_article_detected_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_articles", "last_seen_at", "TEXT", warn_only = TRUE)

  create_index_if_possible(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_articles_url
    ON medium_articles(url)
  ", "Could not create medium_articles url unique index")

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_articles_medium_post_id
    ON medium_articles(medium_post_id)
  ", "Could not create medium_articles post id index")

  ensure_medium_article_text_snapshots_schema(connection)
}

ensure_medium_article_text_snapshots_schema <- function(connection) {
  invisible(dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_article_text_snapshots (
      id INTEGER PRIMARY KEY,
      article_id INTEGER NOT NULL,
      article_url_normalized TEXT NOT NULL,
      collected_at TEXT NOT NULL,
      source_type TEXT,
      extraction_method TEXT,
      text_hash TEXT NOT NULL,
      visible_text TEXT,
      word_count INTEGER,
      text_truncated INTEGER,
      max_chars INTEGER,
      text_blocks_json TEXT,
      article_tags_json TEXT,
      highlighted_text_json TEXT,
      images_json TEXT,
      thumbnail_url TEXT,
      thumbnail_alt TEXT,
      thumbnail_source TEXT,
      thumbnail_confidence TEXT,
      thumbnail_status TEXT,
      author_name TEXT,
      author_url TEXT,
      publication_name TEXT,
      publication_url TEXT,
      published_label TEXT,
      capture_completeness TEXT,
      readable_status TEXT,
      error_message TEXT,
      FOREIGN KEY(article_id) REFERENCES medium_articles(id),
      UNIQUE(article_id, text_hash)
    )
  "))

  add_column_if_missing(connection, "medium_article_text_snapshots", "article_id", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "article_url_normalized", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "collected_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "source_type", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "extraction_method", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "text_hash", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "visible_text", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "word_count", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "text_truncated", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "max_chars", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "text_blocks_json", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "article_tags_json", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "highlighted_text_json", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "images_json", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "thumbnail_url", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "thumbnail_alt", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "thumbnail_source", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "thumbnail_confidence", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "thumbnail_status", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "author_name", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "author_url", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "publication_name", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "publication_url", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "published_label", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "capture_completeness", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "readable_status", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_text_snapshots", "error_message", "TEXT", warn_only = TRUE)

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_article_text_snapshots_article_id
    ON medium_article_text_snapshots(article_id)
  ", "Could not create text snapshots article_id index")

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_article_text_snapshots_collected_at
    ON medium_article_text_snapshots(collected_at)
  ", "Could not create text snapshots collected_at index")

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_article_text_snapshots_article_url
    ON medium_article_text_snapshots(article_url_normalized)
  ", "Could not create text snapshots article URL index")
}

create_index_if_possible <- function(connection, sql_text, warning_prefix) {
  invisible(tryCatch(
    dbExecute(connection, sql_text),
    error = function(error) {
      warning(
        warning_prefix,
        ": ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  ))
}

quote_identifier <- function(x) {
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}

fetch_medium_article_by_id <- function(connection, article_id) {
  dbGetQuery(
    connection,
    "SELECT * FROM medium_articles WHERE id = ? LIMIT 1",
    params = list(article_id)
  )
}

find_article_by_post_id <- function(connection, medium_post_id, schema_info) {
  normalized_post_id <- clean_text(medium_post_id)

  if (is.na(normalized_post_id) || !isTRUE(schema_info$has_medium_post_id)) {
    return(data.frame())
  }

  dbGetQuery(
    connection,
    "
      SELECT *
      FROM medium_articles
      WHERE LOWER(TRIM(medium_post_id)) = LOWER(TRIM(?))
      ORDER BY id ASC
      LIMIT 1
    ",
    params = list(normalized_post_id)
  )
}

find_article_by_url <- function(connection, article_url, schema_info) {
  normalized_url <- normalize_medium_url(article_url)

  if (is.na(normalized_url)) {
    return(data.frame())
  }

  where_parts <- character()
  params <- list()

  for (column_name in c("url", "canonical_url", "raw_url")) {
    if (column_name %in% schema_info$columns) {
      where_parts <- c(where_parts, paste0(quote_identifier(column_name), " = ?"))
      params <- c(params, list(normalized_url))
    }
  }

  if (length(where_parts) == 0) {
    return(data.frame())
  }

  query <- paste0(
    "SELECT * FROM medium_articles WHERE ",
    paste(where_parts, collapse = " OR "),
    " ORDER BY id ASC LIMIT 1"
  )

  dbGetQuery(connection, query, params = params)
}

choose_article_match <- function(post_match, url_match, article_url, medium_post_id) {
  if (nrow(post_match) > 0 && nrow(url_match) > 0) {
    if (identical(post_match$id[1], url_match$id[1])) {
      return(list(
        row = post_match,
        warning_message = NULL
      ))
    }

    return(list(
      row = post_match,
      warning_message = paste0(
        "Conflict while matching article. medium_post_id '",
        clean_text(medium_post_id),
        "' matched row ",
        post_match$id[1],
        " (",
        post_match$url[1],
        ") but normalized URL '",
        normalize_medium_url(article_url),
        "' matched row ",
        url_match$id[1],
        " (",
        url_match$url[1],
        "). Using the medium_post_id match conservatively."
      )
    ))
  }

  if (nrow(post_match) > 0) {
    return(list(row = post_match, warning_message = NULL))
  }

  if (nrow(url_match) > 0) {
    return(list(row = url_match, warning_message = NULL))
  }

  list(row = data.frame(), warning_message = NULL)
}

prefer_longer_text <- function(existing_value, new_value) {
  if (is_missing_text(new_value)) {
    return(existing_value)
  }

  if (is_missing_text(existing_value)) {
    return(clean_text(new_value))
  }

  existing_value <- clean_text(existing_value)
  new_value <- clean_text(new_value)

  if (is.na(existing_value) || (!is.na(new_value) && nchar(new_value) > nchar(existing_value))) {
    return(new_value)
  }

  existing_value
}

prefer_fill_missing <- function(existing_value, new_value) {
  if (is_missing_text(existing_value) && !is_missing_text(new_value)) {
    return(clean_text(new_value))
  }

  existing_value
}

prepare_medium_article_insert <- function(card, schema_info, observed_at) {
  values <- list(
    source_tag = "medium-tag-page",
    title = if (is_missing_text(card$title)) "(untitled Medium article)" else clean_text(card$title),
    url = normalize_medium_url(card$article_url),
    fetched_at = clean_text(observed_at)
  )

  optional_values <- list(
    raw_url = normalize_medium_url(card$article_url),
    canonical_url = normalize_medium_url(card$article_url),
    medium_post_id = clean_text(card$medium_post_id),
    author = clean_text(card$author_name),
    subtitle = clean_text(card$subtitle),
    snippet = clean_text(card$subtitle),
    publication = clean_text(card$publication_name),
    publication_status = clean_text(card$publication_status),
    published_at = if (!is_missing_text(card$published_at)) {
      clean_text(card$published_at)
    } else if (!is_missing_text(card$published_at_inferred)) {
      clean_text(card$published_at_inferred)
    } else {
      clean_text(card$published_date_inferred)
    },
    updated_at = clean_text(card$updated_at),
    read_time = if (!is.na(card$read_time_minutes)) paste0(card$read_time_minutes, " min read") else NA_character_,
    categories = clean_text(card$article_tags_json),
    image_url = clean_text(card$thumbnail_url),
    image_url_manual = clean_text(card$thumbnail_url),
    image_url_source = if (!is_missing_text(card$thumbnail_url)) "tag_card_thumbnail" else NA_character_,
    image_url_confidence = if (!is_missing_text(card$thumbnail_url)) "high" else NA_character_,
    image_url_status = if (!is_missing_text(card$thumbnail_url)) "found_confirmed_card" else NA_character_,
    is_member_only = card$is_member_only
  )

  allowed_names <- names(values)[names(values) %in% schema_info$columns]
  insert_columns <- allowed_names
  insert_values <- values[allowed_names]

  for (column_name in names(optional_values)) {
    if (column_name %in% schema_info$columns) {
      insert_columns <- c(insert_columns, column_name)
      insert_values[[column_name]] <- optional_values[[column_name]]
    }
  }

  list(columns = insert_columns, values = insert_values)
}

update_medium_article_metadata <- function(connection, existing_row, card, schema_info, observed_at = NA_character_) {
  row <- existing_row
  updates <- list()

  maybe_update <- function(column_name, new_value) {
    if (!(column_name %in% names(row))) {
      return(invisible(NULL))
    }

    existing_value <- row[[column_name]][1]
    same_value <- if (is.na(existing_value) && is.na(new_value)) {
      TRUE
    } else {
      identical(as.character(existing_value), as.character(new_value))
    }

    if (!same_value) {
      updates[[column_name]] <<- new_value
      row[[column_name]][1] <<- new_value
    }
  }

  if ("title" %in% names(row)) {
    maybe_update("title", prefer_longer_text(row$title[1], card$title))
  }

  if ("author" %in% names(row)) {
    maybe_update("author", prefer_fill_missing(row$author[1], card$author_name))
  }

  if ("canonical_url" %in% names(row)) {
    maybe_update("canonical_url", prefer_fill_missing(row$canonical_url[1], normalize_medium_url(card$article_url)))
  }

  if ("raw_url" %in% names(row)) {
    maybe_update("raw_url", prefer_fill_missing(row$raw_url[1], normalize_medium_url(card$article_url)))
  }

  if ("medium_post_id" %in% names(row)) {
    maybe_update("medium_post_id", prefer_fill_missing(row$medium_post_id[1], card$medium_post_id))
  }

  if ("subtitle" %in% names(row)) {
    maybe_update("subtitle", prefer_longer_text(row$subtitle[1], card$subtitle))
  }

  if ("snippet" %in% names(row)) {
    maybe_update("snippet", prefer_longer_text(row$snippet[1], card$subtitle))
  }

  if ("publication" %in% names(row)) {
    maybe_update("publication", prefer_fill_missing(row$publication[1], card$publication_name))
  }

  if ("publication_status" %in% names(row) && is_missing_text(row$publication_status[1]) && !is_missing_text(card$publication_status)) {
    maybe_update("publication_status", card$publication_status)
  }

  if ("published_at" %in% names(row)) {
    maybe_update("published_at", prefer_fill_missing(
      row$published_at[1],
      if (!is_missing_text(card$published_at)) {
        card$published_at
      } else if (!is_missing_text(card$published_at_inferred)) {
        card$published_at_inferred
      } else {
        card$published_date_inferred
      }
    ))
  }

  if ("updated_at" %in% names(row)) {
    maybe_update("updated_at", prefer_fill_missing(row$updated_at[1], card$updated_at))
  }

  if ("read_time" %in% names(row) && !is.na(card$read_time_minutes)) {
    maybe_update("read_time", prefer_fill_missing(row$read_time[1], paste0(card$read_time_minutes, " min read")))
  }

  if ("categories" %in% names(row)) {
    maybe_update("categories", prefer_fill_missing(row$categories[1], card$article_tags_json))
  }

  for (image_column in c("image_url_manual", "image_url")) {
    if (image_column %in% names(row)) {
      maybe_update(image_column, prefer_fill_missing(row[[image_column]][1], card$thumbnail_url))
    }
  }

  if (!is_missing_text(card$thumbnail_url)) {
    if ("image_url_source" %in% names(row)) {
      maybe_update("image_url_source", prefer_fill_missing(row$image_url_source[1], "tag_card_thumbnail"))
    }
    if ("image_url_confidence" %in% names(row)) {
      maybe_update("image_url_confidence", prefer_fill_missing(row$image_url_confidence[1], "high"))
    }
    if ("image_url_status" %in% names(row)) {
      maybe_update("image_url_status", prefer_fill_missing(row$image_url_status[1], "found_confirmed_card"))
    }
  }

  if ("is_member_only" %in% names(row) && is.na(row$is_member_only[1]) && !is.na(card$is_member_only)) {
    maybe_update("is_member_only", card$is_member_only)
  }

  if ("last_seen_at" %in% names(row) && !is_missing_text(observed_at)) {
    existing_seen <- clean_text(row$last_seen_at[1])
    if (is.na(existing_seen) || clean_text(observed_at) > existing_seen) {
      maybe_update("last_seen_at", clean_text(observed_at))
    }
  }

  if (length(updates) == 0) {
    return(FALSE)
  }

  assignments <- paste0(vapply(names(updates), quote_identifier, character(1)), " = ?")
  query <- paste0(
    "UPDATE medium_articles SET ",
    paste(assignments, collapse = ", "),
    " WHERE id = ?"
  )

  params <- c(unname(updates), list(existing_row$id[1]))
  dbExecute(connection, query, params = params)
  TRUE
}

find_or_create_medium_article <- function(connection, card, schema_info, observed_at) {
  post_match <- find_article_by_post_id(connection, card$medium_post_id, schema_info)
  url_match <- find_article_by_url(connection, card$article_url, schema_info)
  chosen_match <- choose_article_match(post_match, url_match, card$article_url, card$medium_post_id)

  if (nrow(chosen_match$row) == 0) {
    insert_payload <- prepare_medium_article_insert(card, schema_info, observed_at)
    insert_columns <- paste(vapply(insert_payload$columns, quote_identifier, character(1)), collapse = ", ")
    placeholders <- paste(rep("?", length(insert_payload$columns)), collapse = ", ")
    insert_query <- paste0(
      "INSERT INTO medium_articles (",
      insert_columns,
      ") VALUES (",
      placeholders,
      ")"
    )

    dbExecute(connection, insert_query, params = unname(insert_payload$values))
    article_id <- dbGetQuery(connection, "SELECT last_insert_rowid() AS id")$id[1]

    return(list(
      article_id = article_id,
      status = "created",
      warning_message = chosen_match$warning_message
    ))
  }

  was_updated <- update_medium_article_metadata(connection, chosen_match$row, card, schema_info, observed_at)

  list(
    article_id = chosen_match$row$id[1],
    status = if (isTRUE(was_updated)) "updated" else "reused",
    warning_message = chosen_match$warning_message
  )
}

article_has_full_content <- function(article_row, schema_info) {
  if (!is.data.frame(article_row) || nrow(article_row) == 0) {
    return(list(
      has_full_content = FALSE,
      warning_message = "Could not determine full-content status because no article row was available."
    ))
  }

  if (length(schema_info$full_content_columns) == 0) {
    return(list(
      has_full_content = FALSE,
      warning_message = "No full-content columns were found in medium_articles. Treating articles as missing full content."
    ))
  }

  for (column_name in schema_info$full_content_columns) {
    if (column_name %in% names(article_row) && !is_missing_text(article_row[[column_name]][1])) {
      return(list(has_full_content = TRUE, warning_message = NULL))
    }
  }

  list(has_full_content = FALSE, warning_message = NULL)
}

ensure_medium_tag_page_schema <- function(connection, schema_info) {
  invisible(dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_tag_page_snapshots (
      id INTEGER PRIMARY KEY,
      tag_slug TEXT NOT NULL,
      page_variant TEXT,
      tag_url TEXT,
      page_title TEXT,
      captured_at TEXT,
      imported_at TEXT DEFAULT CURRENT_TIMESTAMP,
      source_json_path TEXT,
      source_file_hash TEXT,
      source_type TEXT,
      schema_version INTEGER,
      cards_found INTEGER
    )
  "))

  invisible(dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_tag_page_observations (
      id INTEGER PRIMARY KEY,
      snapshot_id INTEGER NOT NULL,
      article_id INTEGER,
      article_url_normalized TEXT NOT NULL,
      medium_post_id TEXT,
      tag_slug TEXT NOT NULL,
      page_position INTEGER,
      section_name TEXT,
      title TEXT,
      subtitle TEXT,
      author_name TEXT,
      author_url TEXT,
      author_username TEXT,
      author_medium_user_id TEXT,
      publication_name TEXT,
      publication_url TEXT,
      publication_id TEXT,
      publication_slug TEXT,
      publication_domain TEXT,
      publication_subscriber_count INTEGER,
      publication_status TEXT,
      published_label TEXT,
      published_at TEXT,
      published_date_inferred TEXT,
      published_date_inferred_from TEXT,
      published_at_inferred TEXT,
      published_at_inferred_precision TEXT,
      updated_at TEXT,
      read_time_minutes REAL,
      article_tags_json TEXT,
      claps INTEGER,
      responses INTEGER,
      is_member_only INTEGER,
      thumbnail_url TEXT,
      thumbnail_alt TEXT,
      thumbnail_source TEXT,
      thumbnail_confidence TEXT,
      thumbnail_status TEXT,
      recommendation_source TEXT,
      recommendation_surface TEXT,
      recommendation_tag_slug TEXT,
      recommendation_position INTEGER,
      recommendation_result_set_size INTEGER,
      observed_at TEXT,
      imported_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(snapshot_id) REFERENCES medium_tag_page_snapshots(id),
      FOREIGN KEY(article_id) REFERENCES medium_articles(id)
    )
  "))

  invisible(dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_article_import_queue (
      id INTEGER PRIMARY KEY,
      article_id INTEGER,
      article_url_normalized TEXT NOT NULL,
      medium_post_id TEXT,
      first_seen_at TEXT,
      last_seen_at TEXT,
      source_type TEXT,
      source_context TEXT,
      tag_slug TEXT,
      status TEXT DEFAULT 'pending',
      priority INTEGER DEFAULT 0,
      imported_at TEXT,
      notes TEXT,
      FOREIGN KEY(article_id) REFERENCES medium_articles(id)
    )
  "))

  add_column_if_missing(connection, "medium_tag_page_snapshots", "source_file_hash", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_snapshots", "page_variant", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "article_id", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "observed_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "author_username", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "author_medium_user_id", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "publication_id", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "publication_slug", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "publication_domain", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "publication_subscriber_count", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "publication_status", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "published_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "published_date_inferred", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "published_date_inferred_from", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "published_at_inferred", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "published_at_inferred_precision", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "updated_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "read_time_minutes", "REAL", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "article_tags_json", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "thumbnail_source", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "thumbnail_confidence", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "thumbnail_status", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "recommendation_source", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "recommendation_surface", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "recommendation_tag_slug", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "recommendation_position", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_page_observations", "recommendation_result_set_size", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "article_id", "INTEGER", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "medium_post_id", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "first_seen_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "last_seen_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "source_type", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "source_context", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "tag_slug", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "status", "TEXT DEFAULT 'pending'", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "priority", "INTEGER DEFAULT 0", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "imported_at", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_article_import_queue", "notes", "TEXT", warn_only = TRUE)

  create_index_if_possible(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_tag_page_snapshots_source_file_hash
    ON medium_tag_page_snapshots(source_file_hash)
  ", "Could not create source_file_hash unique index")

  create_index_if_possible(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_tag_page_observations_snapshot_article_position
    ON medium_tag_page_observations(snapshot_id, article_url_normalized, page_position)
  ", "Could not create snapshot/article/position unique index")

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_tag_page_observations_article_id
    ON medium_tag_page_observations(article_id)
  ", "Could not create article_id index")

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_tag_page_observations_tag_slug
    ON medium_tag_page_observations(tag_slug)
  ", "Could not create tag_slug index")

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_tag_page_observations_article_url
    ON medium_tag_page_observations(article_url_normalized)
  ", "Could not create article_url index")

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_tag_page_observations_medium_post_id
    ON medium_tag_page_observations(medium_post_id)
  ", "Could not create medium_post_id index")

  create_index_if_possible(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_article_import_queue_article_url
    ON medium_article_import_queue(article_url_normalized)
  ", "Could not create article import queue unique index")
}

ensure_medium_search_tags_schema <- function(connection) {
  invisible(dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_candidate_tags (
      id INTEGER PRIMARY KEY,
      tag_slug TEXT NOT NULL UNIQUE,
      display_title TEXT,
      tag_url TEXT,
      first_seen_at TEXT,
      last_seen_at TEXT,
      first_seen_search_term TEXT,
      last_seen_search_term TEXT,
      status TEXT,
      notes TEXT
    )
  "))

  invisible(dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_tag_discovery_observations (
      id INTEGER PRIMARY KEY,
      observed_at TEXT NOT NULL,
      search_term TEXT NOT NULL,
      tag_slug TEXT NOT NULL,
      display_title TEXT,
      result_rank INTEGER,
      tracking_context TEXT,
      source_url TEXT,
      source_file TEXT,
      imported_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  "))

  invisible(dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_search_sidebar_post_observations (
      id INTEGER PRIMARY KEY,
      observed_at TEXT NOT NULL,
      search_term TEXT NOT NULL,
      source_surface TEXT NOT NULL,
      article_id INTEGER,
      article_url_normalized TEXT,
      medium_post_id TEXT,
      title TEXT,
      author_name TEXT,
      author_url TEXT,
      author_username TEXT,
      result_rank INTEGER,
      tracking_context TEXT,
      source_url TEXT,
      source_file TEXT,
      imported_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(article_id) REFERENCES medium_articles(id)
    )
  "))

  invisible(dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_search_people_observations (
      id INTEGER PRIMARY KEY,
      observed_at TEXT NOT NULL,
      search_term TEXT NOT NULL,
      source_surface TEXT NOT NULL,
      profile_url TEXT,
      username TEXT,
      display_name TEXT,
      bio_snippet TEXT,
      result_rank INTEGER,
      tracking_context TEXT,
      source_url TEXT,
      source_file TEXT,
      imported_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  "))

  add_column_if_missing(connection, "medium_candidate_tags", "tag_url", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_candidate_tags", "first_seen_search_term", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_candidate_tags", "last_seen_search_term", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_candidate_tags", "status", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_candidate_tags", "notes", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_discovery_observations", "tracking_context", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_tag_discovery_observations", "source_file", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_search_sidebar_post_observations", "tracking_context", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_search_sidebar_post_observations", "source_file", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_search_people_observations", "tracking_context", "TEXT", warn_only = TRUE)
  add_column_if_missing(connection, "medium_search_people_observations", "source_file", "TEXT", warn_only = TRUE)

  create_index_if_possible(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_candidate_tags_slug
    ON medium_candidate_tags(tag_slug)
  ", "Could not create candidate tag slug unique index")

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_tag_discovery_search_tag_context_observed
    ON medium_tag_discovery_observations(search_term, tag_slug, tracking_context, observed_at)
  ", "Could not create tag discovery cooldown index")

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_search_sidebar_posts_article_window
    ON medium_search_sidebar_post_observations(search_term, source_surface, medium_post_id, article_url_normalized, tracking_context, observed_at)
  ", "Could not create search sidebar post duplicate-window index")

  create_index_if_possible(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_search_people_window
    ON medium_search_people_observations(search_term, username, profile_url, tracking_context, observed_at)
  ", "Could not create search people cooldown index")
}

find_existing_queue_row <- function(connection, normalized_url) {
  if (!dbExistsTable(connection, "medium_article_import_queue")) {
    return(data.frame())
  }

  dbGetQuery(
    connection,
    "
      SELECT *
      FROM medium_article_import_queue
      WHERE article_url_normalized = ?
      LIMIT 1
    ",
    params = list(normalized_url)
  )
}

insert_or_update_queue_row <- function(connection, article_id, normalized_url, medium_post_id, tag_slug, observed_at, has_full_content) {
  queue_columns <- table_columns(connection, "medium_article_import_queue")
  existing_row <- find_existing_queue_row(connection, normalized_url)
  has_article_id_column <- "article_id" %in% queue_columns
  safe_article_id <- if (has_article_id_column) article_id else NA_integer_
  existing_status <- if (nrow(existing_row) > 0 && "status" %in% names(existing_row)) clean_text(existing_row$status[1]) else NA_character_

  if (isTRUE(has_full_content)) {
    if (nrow(existing_row) == 0) {
      return("already_had_full_content")
    }

    assignments <- c(
      if (has_article_id_column) "article_id = COALESCE(article_id, ?)" else NULL,
      if ("medium_post_id" %in% queue_columns) "medium_post_id = COALESCE(NULLIF(medium_post_id, ''), ?)" else NULL,
      if ("last_seen_at" %in% queue_columns) "last_seen_at = COALESCE(?, last_seen_at)" else NULL,
      if ("tag_slug" %in% queue_columns) "tag_slug = COALESCE(NULLIF(tag_slug, ''), ?)" else NULL,
      if ("status" %in% queue_columns) "status = CASE WHEN status IS NULL OR status = '' OR LOWER(status) = 'pending' THEN 'imported' ELSE status END" else NULL,
      if ("imported_at" %in% queue_columns) "imported_at = CASE WHEN imported_at IS NULL OR imported_at = '' THEN ? ELSE imported_at END" else NULL
    )

    params <- c(
      if (has_article_id_column) list(safe_article_id) else NULL,
      if ("medium_post_id" %in% queue_columns) list(medium_post_id) else NULL,
      if ("last_seen_at" %in% queue_columns) list(observed_at) else NULL,
      if ("tag_slug" %in% queue_columns) list(tag_slug) else NULL,
      if ("imported_at" %in% queue_columns) list(observed_at) else NULL,
      list(existing_row$id[1])
    )

    dbExecute(
      connection,
      paste0(
        "UPDATE medium_article_import_queue SET ",
        paste(assignments, collapse = ", "),
        " WHERE id = ?"
      ),
      params = params
    )

    return("already_queued_or_imported")
  }

  if (nrow(existing_row) == 0) {
    insert_columns <- c(
      if (has_article_id_column) "article_id" else NULL,
      "article_url_normalized",
      if ("medium_post_id" %in% queue_columns) "medium_post_id" else NULL,
      if ("first_seen_at" %in% queue_columns) "first_seen_at" else NULL,
      if ("last_seen_at" %in% queue_columns) "last_seen_at" else NULL,
      if ("source_type" %in% queue_columns) "source_type" else NULL,
      if ("source_context" %in% queue_columns) "source_context" else NULL,
      if ("tag_slug" %in% queue_columns) "tag_slug" else NULL,
      if ("status" %in% queue_columns) "status" else NULL,
      if ("priority" %in% queue_columns) "priority" else NULL,
      if ("imported_at" %in% queue_columns) "imported_at" else NULL,
      if ("notes" %in% queue_columns) "notes" else NULL
    )

    insert_values <- c(
      if (has_article_id_column) list(safe_article_id) else NULL,
      list(normalized_url),
      if ("medium_post_id" %in% queue_columns) list(medium_post_id) else NULL,
      if ("first_seen_at" %in% queue_columns) list(observed_at) else NULL,
      if ("last_seen_at" %in% queue_columns) list(observed_at) else NULL,
      if ("source_type" %in% queue_columns) list("medium_tag_page_bookmarklet") else NULL,
      if ("source_context" %in% queue_columns) list(paste0("tag:", tag_slug)) else NULL,
      if ("tag_slug" %in% queue_columns) list(tag_slug) else NULL,
      if ("status" %in% queue_columns) list("pending") else NULL,
      if ("priority" %in% queue_columns) list(0L) else NULL,
      if ("imported_at" %in% queue_columns) list(NA_character_) else NULL,
      if ("notes" %in% queue_columns) list(NA_character_) else NULL
    )

    dbExecute(
      connection,
      paste0(
        "INSERT INTO medium_article_import_queue (",
        paste(insert_columns, collapse = ", "),
        ") VALUES (",
        paste(rep("?", length(insert_columns)), collapse = ", "),
        ")"
      ),
      params = insert_values
    )

    return("queued")
  }

  if (!is.na(existing_status) && tolower(existing_status) %in% c("imported", "done")) {
    assignments <- c(
      if (has_article_id_column) "article_id = COALESCE(article_id, ?)" else NULL,
      if ("medium_post_id" %in% queue_columns) "medium_post_id = COALESCE(NULLIF(medium_post_id, ''), ?)" else NULL,
      if ("last_seen_at" %in% queue_columns) "last_seen_at = COALESCE(?, last_seen_at)" else NULL,
      if ("tag_slug" %in% queue_columns) "tag_slug = COALESCE(NULLIF(tag_slug, ''), ?)" else NULL
    )

    params <- c(
      if (has_article_id_column) list(safe_article_id) else NULL,
      if ("medium_post_id" %in% queue_columns) list(medium_post_id) else NULL,
      if ("last_seen_at" %in% queue_columns) list(observed_at) else NULL,
      if ("tag_slug" %in% queue_columns) list(tag_slug) else NULL,
      list(existing_row$id[1])
    )

    dbExecute(
      connection,
      paste0(
        "UPDATE medium_article_import_queue SET ",
        paste(assignments, collapse = ", "),
        " WHERE id = ?"
      ),
      params = params
    )

    return("already_queued_or_imported")
  }

  assignments <- c(
    if (has_article_id_column) "article_id = COALESCE(article_id, ?)" else NULL,
    if ("medium_post_id" %in% queue_columns) "medium_post_id = COALESCE(NULLIF(medium_post_id, ''), ?)" else NULL,
    if ("last_seen_at" %in% queue_columns) "last_seen_at = COALESCE(?, last_seen_at)" else NULL,
    if ("source_type" %in% queue_columns) "source_type = COALESCE(NULLIF(source_type, ''), 'medium_tag_page_bookmarklet')" else NULL,
    if ("source_context" %in% queue_columns) "source_context = COALESCE(NULLIF(source_context, ''), ?)" else NULL,
    if ("tag_slug" %in% queue_columns) "tag_slug = COALESCE(NULLIF(tag_slug, ''), ?)" else NULL,
    if ("status" %in% queue_columns) "status = CASE WHEN status IS NULL OR status = '' THEN 'pending' WHEN LOWER(status) IN ('imported', 'done') THEN status ELSE 'pending' END" else NULL
  )

  params <- c(
    if (has_article_id_column) list(safe_article_id) else NULL,
    if ("medium_post_id" %in% queue_columns) list(medium_post_id) else NULL,
    if ("last_seen_at" %in% queue_columns) list(observed_at) else NULL,
    if ("source_context" %in% queue_columns) list(paste0("tag:", tag_slug)) else NULL,
    if ("tag_slug" %in% queue_columns) list(tag_slug) else NULL,
    list(existing_row$id[1])
  )

  dbExecute(
    connection,
    paste0(
      "UPDATE medium_article_import_queue SET ",
      paste(assignments, collapse = ", "),
      " WHERE id = ?"
    ),
    params = params
  )

  "already_queued_or_imported"
}
