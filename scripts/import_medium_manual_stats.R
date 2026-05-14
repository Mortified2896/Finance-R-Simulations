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

clean_text <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x)) {
    return(NA_character_)
  }

  x <- trimws(as.character(x))
  x <- gsub("\\s+", " ", x)

  if (identical(x, "")) {
    return(NA_character_)
  }

  x
}

clean_url <- function(x) {
  x <- clean_text(x)

  if (is.na(x)) {
    return(NA_character_)
  }

  x <- sub("\\?.*$", "", x)
  x <- sub("#.*$", "", x)
  x
}

parse_compact_number <- function(x) {
  x <- clean_text(x)

  if (is.na(x)) {
    return(NA_integer_)
  }

  compact <- toupper(gsub("\\s+", "", gsub(",", "", x)))
  number_part <- suppressWarnings(as.numeric(sub("^([0-9]+\\.?[0-9]*).*$", "\\1", compact)))

  if (is.na(number_part)) {
    return(NA_integer_)
  }

  multiplier <- 1

  if (grepl("K$", compact)) {
    multiplier <- 1000
  } else if (grepl("M$", compact)) {
    multiplier <- 1000000
  }

  as.integer(round(number_part * multiplier))
}

create_public_stats_table <- function(connection) {
  invisible(dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_article_public_stats (
      id INTEGER PRIMARY KEY,
      article_url TEXT NOT NULL,
      observed_at TEXT NOT NULL,
      observed_date TEXT NOT NULL,
      claps_count INTEGER,
      responses_count INTEGER,
      claps_raw TEXT,
      responses_raw TEXT,
      parse_status TEXT NOT NULL,
      parse_method TEXT,
      error_message TEXT,
      publication_snapshot TEXT,
      publication_status_snapshot TEXT,
      UNIQUE(article_url, observed_at)
    )
  "))
}

ensure_public_stats_schema <- function(connection) {
  if (!dbExistsTable(connection, "medium_article_public_stats")) {
    create_public_stats_table(connection)
    return(invisible())
  }

  table_sql <- dbGetQuery(
    connection,
    "
      SELECT sql
      FROM sqlite_master
      WHERE type = 'table'
        AND name = 'medium_article_public_stats'
    "
  )$sql

  has_old_daily_unique <- length(table_sql) > 0 &&
    grepl("UNIQUE\\s*\\(\\s*article_url\\s*,\\s*observed_date\\s*\\)", table_sql, ignore.case = TRUE)

  if (has_old_daily_unique) {
    message("Migrating medium_article_public_stats to keep every observation timestamp...")

    backup_table <- paste0("medium_article_public_stats_backup_", format(Sys.time(), "%Y%m%d%H%M%S"))

    dbBegin(connection)
    tryCatch(
      {
        dbExecute(connection, paste("ALTER TABLE medium_article_public_stats RENAME TO", backup_table))
        create_public_stats_table(connection)
        dbExecute(
          connection,
          paste0(
            "
              INSERT OR IGNORE INTO medium_article_public_stats (
                id,
                article_url,
                observed_at,
                observed_date,
                claps_count,
                responses_count,
                claps_raw,
                responses_raw,
                parse_status,
                parse_method,
                error_message
              )
              SELECT
                id,
                article_url,
                observed_at,
                observed_date,
                claps_count,
                responses_count,
                claps_raw,
                responses_raw,
                parse_status,
                parse_method,
                error_message
              FROM ", backup_table, "
              ORDER BY id
            "
          )
        )
        dbExecute(connection, paste("DROP TABLE", backup_table))
        dbCommit(connection)
        message("Migration complete: stats now allow multiple observations per article per day.")
      },
      error = function(error) {
        dbRollback(connection)
        stop("Stats table migration failed: ", conditionMessage(error), call. = FALSE)
      }
    )
  }

  invisible(tryCatch(
    dbExecute(
      connection,
      "
        CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_article_public_stats_article_url_observed_at
        ON medium_article_public_stats(article_url, observed_at)
      "
    ),
    error = function(error) {
      warning(
        "Could not create the exact-observation unique index. The importer will still check duplicates in code. ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  ))
}

add_column_if_missing <- function(connection, table_name, column_name, column_definition) {
  table_info <- dbGetQuery(connection, paste0("PRAGMA table_info(", table_name, ")"))

  if (!column_name %in% table_info$name) {
    dbExecute(connection, paste("ALTER TABLE", table_name, "ADD COLUMN", column_name, column_definition))
    message("Added column: ", table_name, ".", column_name)
  }
}

ensure_public_stats_columns <- function(connection) {
  add_column_if_missing(connection, "medium_article_public_stats", "publication_snapshot", "TEXT")
  add_column_if_missing(connection, "medium_article_public_stats", "publication_status_snapshot", "TEXT")
}

ensure_medium_articles_columns <- function(connection) {
  add_column_if_missing(connection, "medium_articles", "canonical_url", "TEXT")
  add_column_if_missing(connection, "medium_articles", "subtitle", "TEXT")
  add_column_if_missing(connection, "medium_articles", "publication", "TEXT")
  add_column_if_missing(connection, "medium_articles", "publication_status", "TEXT")
  add_column_if_missing(connection, "medium_articles", "published_date_manual", "TEXT")
  add_column_if_missing(connection, "medium_articles", "modified_date_manual", "TEXT")
  add_column_if_missing(connection, "medium_articles", "read_time", "TEXT")
  add_column_if_missing(connection, "medium_articles", "is_member_only", "INTEGER")
  add_column_if_missing(connection, "medium_articles", "author_followers_raw", "TEXT")
  add_column_if_missing(connection, "medium_articles", "publication_followers_raw", "TEXT")
  add_column_if_missing(connection, "medium_articles", "medium_post_id", "TEXT")
  add_column_if_missing(connection, "medium_articles", "image_url_manual", "TEXT")
  add_column_if_missing(connection, "medium_articles", "visible_article_text", "TEXT")
  add_column_if_missing(connection, "medium_articles", "visible_text_word_count", "INTEGER")
  add_column_if_missing(connection, "medium_articles", "visible_article_text_truncated", "INTEGER")
  add_column_if_missing(connection, "medium_articles", "visible_article_text_max_chars", "INTEGER")
  add_column_if_missing(connection, "medium_articles", "visible_article_collected_at", "TEXT")
  add_column_if_missing(connection, "medium_articles", "manual_relevance_status", "TEXT")
  add_column_if_missing(connection, "medium_articles", "manual_relevance_checked_at", "TEXT")
  add_column_if_missing(connection, "medium_articles", "manual_relevance_note", "TEXT")
  add_column_if_missing(connection, "medium_articles", "last_seen_at", "TEXT")
  add_column_if_missing(connection, "medium_articles", "is_own_article", "INTEGER DEFAULT 0")
  add_column_if_missing(connection, "medium_articles", "own_article_source", "TEXT")
  add_column_if_missing(connection, "medium_articles", "own_article_detected_at", "TEXT")
}

publication_status_rank <- function(status) {
  status <- clean_text(status)

  if (is.na(status)) {
    return(0L)
  }

  switch(
    status,
    publication = 3L,
    none = 2L,
    unknown = 1L,
    0L
  )
}

normalize_publication_status <- function(status, publication) {
  status <- clean_text(status)
  publication <- clean_text(publication)

  if (!is.na(publication)) {
    return("publication")
  }

  if (is.na(status)) {
    return(NA_character_)
  }

  status <- tolower(status)

  if (status %in% c("publication", "none", "unknown")) {
    return(status)
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

parse_bookmarklet_input <- function(input_text) {
  input_text <- trimws(input_text)

  if (grepl("^\\{", input_text)) {
    parsed <- fromJSON(input_text, simplifyVector = TRUE)
    return(list(
      observed_at = clean_text(parsed$observed_at),
      url = clean_url(parsed$url),
      canonical_url = clean_url(parsed$canonical_url),
      title = clean_text(parsed$title),
      subtitle = clean_text(parsed$subtitle),
      author = clean_text(parsed$author),
      publication = clean_text(parsed$publication),
      publication_status = normalize_publication_status(parsed$publication_status, parsed$publication),
      published_date = clean_text(parsed$published_date),
      modified_date = clean_text(parsed$modified_date),
      read_time = clean_text(parsed$read_time),
      is_member_only = if (isTRUE(parsed$is_member_only)) 1L else if (identical(parsed$is_member_only, FALSE)) 0L else NA_integer_,
      claps_raw = clean_text(parsed$claps_raw),
      responses_raw = clean_text(parsed$responses_raw),
      author_followers_raw = clean_text(parsed$author_followers_raw),
      publication_followers_raw = clean_text(parsed$publication_followers_raw),
      medium_post_id = clean_text(parsed$medium_post_id),
      image_url = clean_text(parsed$image_url),
      visible_article_text = clean_text(parsed$visible_article_text),
      visible_text_word_count = integer_from_json(parsed$visible_text_word_count),
      visible_article_text_truncated = if (isTRUE(parsed$visible_article_text_truncated)) {
        1L
      } else if (identical(parsed$visible_article_text_truncated, FALSE)) {
        0L
      } else {
        NA_integer_
      },
      visible_article_text_max_chars = integer_from_json(parsed$visible_article_text_max_chars),
      source = clean_text(parsed$source)
    ))
  }

  parts <- strsplit(input_text, "\t", fixed = TRUE)[[1]]

  if (length(parts) != 6) {
    stop(
      "The pasted value must be either bookmarklet JSON or the old 6-column TSV:\n",
      "observed_at, url, title, claps_raw, responses_raw, source",
      call. = FALSE
    )
  }

  list(
    observed_at = clean_text(parts[1]),
    url = clean_url(parts[2]),
    canonical_url = NA_character_,
    title = clean_text(parts[3]),
    subtitle = NA_character_,
    author = NA_character_,
    publication = NA_character_,
    publication_status = NA_character_,
    published_date = NA_character_,
    modified_date = NA_character_,
    read_time = NA_character_,
    is_member_only = NA_integer_,
    claps_raw = clean_text(parts[4]),
    responses_raw = clean_text(parts[5]),
    author_followers_raw = NA_character_,
    publication_followers_raw = NA_character_,
    medium_post_id = NA_character_,
    image_url = NA_character_,
    visible_article_text = NA_character_,
    visible_text_word_count = NA_integer_,
    visible_article_text_truncated = NA_integer_,
    visible_article_text_max_chars = NA_integer_,
    source = clean_text(parts[6])
  )
}

find_existing_article_url <- function(connection, article) {
  candidate_urls <- unique(na.omit(c(article$url, article$canonical_url)))

  for (candidate_url in candidate_urls) {
    found <- dbGetQuery(
      connection,
      "SELECT url FROM medium_articles WHERE url = ? OR canonical_url = ? LIMIT 1",
      params = list(candidate_url, candidate_url)
    )

    if (nrow(found) > 0) {
      return(found$url[1])
    }
  }

  if (!is_missing_text(article$medium_post_id) && "medium_post_id" %in% dbListFields(connection, "medium_articles")) {
    found <- dbGetQuery(
      connection,
      "SELECT url FROM medium_articles WHERE LOWER(TRIM(medium_post_id)) = LOWER(TRIM(?)) LIMIT 1",
      params = list(article$medium_post_id)
    )

    if (nrow(found) > 0) {
      return(found$url[1])
    }
  }

  article$url
}

is_missing_text <- function(x) {
  length(x) == 0 || is.null(x) || is.na(x) || trimws(as.character(x)) == ""
}

would_fill_missing <- function(existing_row, column_name, new_value) {
  !is_missing_text(new_value) && column_name %in% names(existing_row) && is_missing_text(existing_row[[column_name]][1])
}

would_update_longer_text <- function(existing_row, column_name, new_value) {
  !is_missing_text(new_value) &&
    column_name %in% names(existing_row) &&
    (is_missing_text(existing_row[[column_name]][1]) || nchar(new_value) > nchar(as.character(existing_row[[column_name]][1])))
}

would_update_larger_integer <- function(existing_row, column_name, new_value) {
  !is.na(new_value) &&
    column_name %in% names(existing_row) &&
    (is.na(existing_row[[column_name]][1]) || new_value > existing_row[[column_name]][1])
}

upsert_medium_article <- function(connection, article, article_url) {
  existing <- dbGetQuery(
    connection,
    "SELECT * FROM medium_articles WHERE url = ? LIMIT 1",
    params = list(article_url)
  )

  if (nrow(existing) == 0) {
    dbExecute(
      connection,
      "
        INSERT INTO medium_articles (
          source_tag,
          title,
          url,
          raw_url,
          author,
          fetched_at,
          canonical_url,
          subtitle,
          publication,
          publication_status,
          published_date_manual,
          modified_date_manual,
          read_time,
          is_member_only,
          author_followers_raw,
          publication_followers_raw,
          medium_post_id,
          image_url_manual,
          visible_article_text,
          visible_text_word_count,
          visible_article_text_truncated,
          visible_article_text_max_chars,
          visible_article_collected_at,
          last_seen_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ",
      params = list(
        "manual-browser",
        ifelse(is.na(article$title), "(untitled Medium article)", article$title),
        article_url,
        article$url,
        article$author,
        article$observed_at,
        article$canonical_url,
        article$subtitle,
        article$publication,
        article$publication_status,
        article$published_date,
        article$modified_date,
        article$read_time,
        article$is_member_only,
        article$author_followers_raw,
        article$publication_followers_raw,
        article$medium_post_id,
        article$image_url,
        article$visible_article_text,
        article$visible_text_word_count,
        article$visible_article_text_truncated,
        article$visible_article_text_max_chars,
        article$observed_at,
        article$observed_at
      )
    )
    return("created")
  }

  metadata_will_update <- any(c(
    would_fill_missing(existing, "title", article$title),
    would_fill_missing(existing, "author", article$author),
    would_fill_missing(existing, "canonical_url", article$canonical_url),
    would_fill_missing(existing, "subtitle", article$subtitle),
    would_fill_missing(existing, "publication", article$publication),
    "publication_status" %in% names(existing) && publication_status_rank(article$publication_status) > publication_status_rank(existing$publication_status[1]),
    would_fill_missing(existing, "published_date_manual", article$published_date),
    would_fill_missing(existing, "modified_date_manual", article$modified_date),
    would_fill_missing(existing, "read_time", article$read_time),
    "is_member_only" %in% names(existing) && is.na(existing$is_member_only[1]) && !is.na(article$is_member_only),
    would_fill_missing(existing, "author_followers_raw", article$author_followers_raw),
    would_fill_missing(existing, "publication_followers_raw", article$publication_followers_raw),
    would_fill_missing(existing, "medium_post_id", article$medium_post_id),
    would_fill_missing(existing, "image_url_manual", article$image_url),
    would_update_longer_text(existing, "visible_article_text", article$visible_article_text),
    would_update_larger_integer(existing, "visible_text_word_count", article$visible_text_word_count),
    "visible_article_text_truncated" %in% names(existing) && is.na(existing$visible_article_text_truncated[1]) && !is.na(article$visible_article_text_truncated),
    "visible_article_text_max_chars" %in% names(existing) && is.na(existing$visible_article_text_max_chars[1]) && !is.na(article$visible_article_text_max_chars)
  ))

  dbExecute(
    connection,
    "
      UPDATE medium_articles
      SET
        title = COALESCE(NULLIF(title, ''), ?),
        author = COALESCE(NULLIF(author, ''), ?),
        canonical_url = COALESCE(NULLIF(canonical_url, ''), ?),
        subtitle = COALESCE(NULLIF(subtitle, ''), ?),
        publication = COALESCE(NULLIF(publication, ''), ?),
        publication_status = CASE
          WHEN ? IS NOT NULL
            AND (
              publication_status IS NULL
              OR publication_status = ''
              OR (
                CASE ?
                  WHEN 'publication' THEN 3
                  WHEN 'none' THEN 2
                  WHEN 'unknown' THEN 1
                  ELSE 0
                END
              ) > (
                CASE publication_status
                  WHEN 'publication' THEN 3
                  WHEN 'none' THEN 2
                  WHEN 'unknown' THEN 1
                  ELSE 0
                END
              )
            )
          THEN ?
          ELSE publication_status
        END,
        published_date_manual = COALESCE(NULLIF(published_date_manual, ''), ?),
        modified_date_manual = COALESCE(NULLIF(modified_date_manual, ''), ?),
        read_time = COALESCE(NULLIF(read_time, ''), ?),
        is_member_only = COALESCE(is_member_only, ?),
        author_followers_raw = COALESCE(NULLIF(author_followers_raw, ''), ?),
        publication_followers_raw = COALESCE(NULLIF(publication_followers_raw, ''), ?),
        medium_post_id = COALESCE(NULLIF(medium_post_id, ''), ?),
        image_url_manual = COALESCE(NULLIF(image_url_manual, ''), ?),
        visible_article_text = CASE
          WHEN ? IS NOT NULL
            AND (
              visible_article_text IS NULL
              OR visible_article_text = ''
              OR length(?) > length(visible_article_text)
            )
          THEN ?
          ELSE visible_article_text
        END,
        visible_text_word_count = CASE
          WHEN ? IS NOT NULL
            AND (
              visible_text_word_count IS NULL
              OR ? > visible_text_word_count
            )
          THEN ?
          ELSE visible_text_word_count
        END,
        visible_article_text_truncated = COALESCE(visible_article_text_truncated, ?),
        visible_article_text_max_chars = COALESCE(visible_article_text_max_chars, ?),
        visible_article_collected_at = CASE
          WHEN ? IS NOT NULL
            AND (
              visible_article_text IS NULL
              OR visible_article_text = ''
              OR length(?) > length(visible_article_text)
            )
          THEN ?
          ELSE visible_article_collected_at
        END,
        last_seen_at = CASE
          WHEN ? IS NOT NULL
            AND (
              last_seen_at IS NULL
              OR last_seen_at = ''
              OR ? > last_seen_at
            )
          THEN ?
          ELSE last_seen_at
        END
      WHERE url = ?
    ",
    params = list(
      article$title,
      article$author,
      article$canonical_url,
      article$subtitle,
      article$publication,
      article$publication_status,
      article$publication_status,
      article$publication_status,
      article$published_date,
      article$modified_date,
      article$read_time,
      article$is_member_only,
      article$author_followers_raw,
      article$publication_followers_raw,
      article$medium_post_id,
      article$image_url,
      article$visible_article_text,
      article$visible_article_text,
      article$visible_article_text,
      article$visible_text_word_count,
      article$visible_text_word_count,
      article$visible_text_word_count,
      article$visible_article_text_truncated,
      article$visible_article_text_max_chars,
      article$observed_at,
      article$visible_article_text,
      article$observed_at,
      article$observed_at,
      article$observed_at,
      article$observed_at,
      article_url
    )
  )

  if (metadata_will_update) {
    "updated"
  } else {
    "already existed"
  }
}

message("Medium Manual Stats Importer")
message("============================")

import_bookmarklet_text <- function(connection, input_text) {
  if (length(input_text) == 0 || trimws(input_text) == "") {
    stop("No bookmarklet data was provided.", call. = FALSE)
  }

  article <- parse_bookmarklet_input(input_text)

  if (is.na(article$observed_at) || is.na(article$url)) {
    stop("The bookmarklet data must include observed_at and url.", call. = FALSE)
  }

  if (!identical(article$source, "bookmarklet_manual_browser")) {
    warning("Unexpected source value: ", article$source, call. = FALSE)
  }

  observed_date <- substr(article$observed_at, 1, 10)
  claps_count <- parse_compact_number(article$claps_raw)
  responses_count <- parse_compact_number(article$responses_raw)

  article_url <- find_existing_article_url(connection, article)
  existing_article <- dbGetQuery(
    connection,
    "SELECT image_url_manual FROM medium_articles WHERE url = ? LIMIT 1",
    params = list(article_url)
  )
  article_status <- upsert_medium_article(connection, article, article_url)

  image_source_status <- if (is_missing_text(article$image_url)) {
    "missing from source"
  } else if (nrow(existing_article) == 0 || is_missing_text(existing_article$image_url_manual[1])) {
    "found and written"
  } else {
    "skipped because already present"
  }

  duplicate_observation <- dbGetQuery(
    connection,
    "
      SELECT COUNT(*) AS n
      FROM medium_article_public_stats
      WHERE article_url = ?
        AND observed_at = ?
    ",
    params = list(article_url, article$observed_at)
  )$n

  observation_status <- "inserted"

  if (duplicate_observation > 0) {
    observation_status <- "duplicate"
  } else {
    invisible(dbExecute(
      connection,
      "
        INSERT INTO medium_article_public_stats (
          article_url,
          observed_at,
          observed_date,
          claps_count,
          responses_count,
          claps_raw,
          responses_raw,
          parse_status,
          parse_method,
          error_message,
          publication_snapshot,
          publication_status_snapshot
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ",
      params = list(
        article_url,
        article$observed_at,
        observed_date,
        claps_count,
        responses_count,
        article$claps_raw,
        article$responses_raw,
        "ok",
        article$source,
        NA_character_,
        article$publication,
        article$publication_status
      )
    ))
  }

  if (identical(observation_status, "duplicate")) {
    message("\nSkipped duplicate manual Medium stats")
    message("-------------------------------------")
  } else {
    message("\nSaved new manual Medium stats")
    message("-----------------------------")
  }
  message("Article metadata: ", article_status)
  if (identical(observation_status, "duplicate")) {
    message("Observation status: Duplicate observation already exists for this article/timestamp; skipped.")
  } else {
    message("Observation status: Saved new observation for this article/timestamp.")
  }
  message("URL: ", article_url)
  message("Title: ", ifelse(is.na(article$title), "(blank)", article$title))
  message("Claps: ", ifelse(is.na(article$claps_raw), "(blank)", article$claps_raw), " / ", ifelse(is.na(claps_count), "NA", claps_count))
  message("Responses: ", ifelse(is.na(article$responses_raw), "(blank)", article$responses_raw), " / ", ifelse(is.na(responses_count), "NA", responses_count))
  message("Publication: ", ifelse(is.na(article$publication), "(blank)", article$publication))
  message("Publication status: ", ifelse(is.na(article$publication_status), "(blank)", article$publication_status))
  message("Read time: ", ifelse(is.na(article$read_time), "(blank)", article$read_time))
  message("Member only: ", ifelse(is.na(article$is_member_only), "NA", article$is_member_only))
  message("Image URL: ", ifelse(is.na(article$image_url), "(blank)", article$image_url))
  message("Image URL status: ", image_source_status)
  message("Visible word count: ", ifelse(is.na(article$visible_text_word_count), "NA", article$visible_text_word_count))
  message("Visible text truncated: ", ifelse(is.na(article$visible_article_text_truncated), "NA", article$visible_article_text_truncated))
  message("Observed at: ", article$observed_at)

  invisible(list(
    article_url = article_url,
    observed_at = article$observed_at,
    observation_status = observation_status
  ))
}

read_clipboard_text <- function() {
  if (Sys.which("pbpaste") == "") {
    message("Clipboard import requires macOS pbpaste. Use file-based import instead.")
    return(NA_character_)
  }

  clipboard_lines <- system2("pbpaste", stdout = TRUE, stderr = FALSE)
  paste(clipboard_lines, collapse = "\n")
}

write_clipboard_text <- function(text) {
  if (Sys.which("pbcopy") == "") {
    message("URL clipboard copy requires macOS pbcopy. Copy this URL manually:")
    message(text)
    return(FALSE)
  }

  copied <- tryCatch(
    {
      clipboard_connection <- pipe("pbcopy", open = "w")
      on.exit(close(clipboard_connection), add = TRUE)
      writeLines(text, clipboard_connection, useBytes = TRUE)
      TRUE
    },
    error = function(error) FALSE
  )

  if (!copied) {
    message("Could not copy the URL to the clipboard. Copy this URL manually:")
    message(text)
    return(FALSE)
  }

  TRUE
}

current_timestamp <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

read_input_file_text <- function(input_path) {
  paste(readLines(input_path, warn = FALSE), collapse = "\n")
}

detect_json_source_type <- function(input_text) {
  trimmed <- trimws(input_text)

  if (!grepl("^\\{", trimmed)) {
    return(NA_character_)
  }

  parsed <- tryCatch(
    fromJSON(trimmed, simplifyVector = FALSE),
    error = function(error) NULL
  )

  if (is.null(parsed)) {
    return(NA_character_)
  }

  clean_text(parsed$source_type)
}

read_arbitrary_import_text <- function() {
  message("\nAdd/import arbitrary Medium article")
  message("----------------------------------")
  message("Paste raw bookmarklet JSON / old TSV, or enter a local path to a bookmarklet JSON file.")
  message("Press Enter to cancel. Type q / quit / exit to cancel.")

  input_value <- readLines("stdin", n = 1, warn = FALSE)

  if (length(input_value) == 0) {
    return(list(status = "cancel"))
  }

  raw_value <- input_value[1]
  trimmed_value <- trimws(raw_value)

  if (trimmed_value %in% c("", "q", "quit", "exit")) {
    return(list(status = "cancel"))
  }

  normalized_path <- path.expand(trimmed_value)

  if (file.exists(normalized_path)) {
    return(list(
      status = "ok",
      input_text = read_input_file_text(normalized_path),
      source_label = paste0("file: ", normalized_path)
    ))
  }

  if (grepl("^\\{", trimmed_value) || grepl("\t", raw_value, fixed = TRUE)) {
    return(list(
      status = "ok",
      input_text = raw_value,
      source_label = "pasted terminal input"
    ))
  }

  if (grepl("\\.json$", trimmed_value, ignore.case = TRUE)) {
    message("The file path does not exist:")
    message(normalized_path)
    return(list(status = "error"))
  }

  message("Input was neither a valid existing file path nor bookmarklet JSON/old TSV.")
  return(list(status = "error"))
}

import_arbitrary_article <- function(connection) {
  repeat {
    input_result <- read_arbitrary_import_text()

    if (identical(input_result$status, "cancel")) {
      message("Arbitrary article import cancelled.")
      return("cancel")
    }

    if (identical(input_result$status, "error")) {
      next
    }

    if (identical(detect_json_source_type(input_result$input_text), "medium_tag_page_bookmarklet")) {
      message("\nThis file is Medium tag-page bookmarklet JSON.")
      message("Use the shared file-drop launcher or run:")
      message("Rscript scripts/import_medium_tag_page_bookmarklet.R <path-to-json>")
      next
    }

    parsed_article <- tryCatch(
      parse_bookmarklet_input(input_result$input_text),
      error = function(error) {
        message("\nImport failed:")
        message("The provided input is not valid bookmarklet JSON or old TSV data.")
        message(conditionMessage(error))
        NULL
      }
    )

    if (is.null(parsed_article)) {
      next
    }

    if (is.na(parsed_article$url) && is.na(parsed_article$canonical_url)) {
      message("\nImport failed:")
      message("The bookmarklet data must include url or canonical_url.")
      next
    }

    if (is.na(parsed_article$canonical_url)) {
      message("Note: canonical_url is blank in the provided bookmarklet data; using url for matching/import.")
    }

    message("Import source: ", input_result$source_label)

    tryCatch(
      {
        import_bookmarklet_text(connection, input_result$input_text)
        return("imported")
      },
      error = function(error) {
        message("\nImport failed:")
        message(conditionMessage(error))
      }
    )
  }
}

candidate_count <- function(connection) {
  dbGetQuery(
    connection,
    "
      SELECT COUNT(*) AS n
      FROM medium_articles a
      WHERE a.manual_relevance_status IS NULL
        AND NOT EXISTS (
          SELECT 1
          FROM medium_article_public_stats s
          WHERE s.article_url = a.url
            AND s.parse_status = 'ok'
        )
    "
  )$n
}

next_candidate <- function(connection) {
  own_stats_join <- if (dbExistsTable(connection, "medium_own_story_stats")) {
    "
      LEFT JOIN (
        SELECT story_url, observed_at, views_count, reads_count, earnings_usd
        FROM medium_own_story_stats os
        WHERE observed_at = (
          SELECT MAX(observed_at)
          FROM medium_own_story_stats
          WHERE story_url = os.story_url
        )
      ) latest_own_stats
        ON latest_own_stats.story_url = a.url
    "
  } else {
    "
      LEFT JOIN (
        SELECT
          NULL AS story_url,
          NULL AS observed_at,
          NULL AS views_count,
          NULL AS reads_count,
          NULL AS earnings_usd
        WHERE 0
      ) latest_own_stats
        ON latest_own_stats.story_url = a.url
    "
  }

  dbGetQuery(
    connection,
    paste0("
      SELECT
        a.id,
        a.source_tag,
        a.title,
        a.url,
        a.author,
        a.published_at,
        a.snippet,
        a.description_html,
        CASE WHEN COALESCE(a.is_own_article, 0) = 1 THEN 'own article' ELSE 'rss random' END AS queue_priority,
        latest_own_stats.observed_at AS latest_private_observed_at,
        latest_own_stats.views_count AS latest_private_views,
        latest_own_stats.reads_count AS latest_private_reads,
        latest_own_stats.earnings_usd AS latest_private_earnings
      FROM medium_articles a
      ", own_stats_join, "
      WHERE a.manual_relevance_status IS NULL
        AND NOT EXISTS (
          SELECT 1
          FROM medium_article_public_stats s
          WHERE s.article_url = a.url
            AND s.parse_status = 'ok'
        )
      ORDER BY
        CASE WHEN COALESCE(a.is_own_article, 0) = 1 THEN 0 ELSE 1 END,
        RANDOM()
      LIMIT 1
    ")
  )
}

show_candidate <- function(connection, candidate) {
  remaining <- candidate_count(connection)
  snippet <- clean_text(candidate$snippet[1])
  description <- clean_text(candidate$description_html[1])
  preview <- if (!is.na(snippet)) snippet else description

  message("\nManual Medium Review Queue")
  message("==========================")
  message("Remaining unreviewed/unimported candidates: ", remaining)
  message("\nTitle: ", ifelse(is.na(candidate$title[1]), "(blank)", candidate$title[1]))
  if ("queue_priority" %in% names(candidate) && identical(candidate$queue_priority[1], "own article")) {
    message("Priority: own article")

    if ("latest_private_observed_at" %in% names(candidate) && !is.na(candidate$latest_private_observed_at[1])) {
      message(
        "Latest private stats: ",
        "views=", ifelse(is.na(candidate$latest_private_views[1]), "NA", candidate$latest_private_views[1]),
        ", reads=", ifelse(is.na(candidate$latest_private_reads[1]), "NA", candidate$latest_private_reads[1]),
        ", earnings=", ifelse(is.na(candidate$latest_private_earnings[1]), "NA", sprintf("$%.2f", candidate$latest_private_earnings[1])),
        " (", candidate$latest_private_observed_at[1], ")"
      )
    }
  }
  message("Source tag: ", ifelse(is.na(candidate$source_tag[1]), "(blank)", candidate$source_tag[1]))
  message("Author: ", ifelse(is.na(candidate$author[1]), "(blank)", candidate$author[1]))
  message("Published at: ", ifelse(is.na(candidate$published_at[1]), "(blank)", candidate$published_at[1]))

  if (!is.na(preview)) {
    message("Snippet: ", substr(preview, 1, 500))
  } else {
    message("Snippet: (blank)")
  }

  message("URL: ", candidate$url[1])
}

mark_candidate_relevance <- function(connection, article_url, status) {
  dbExecute(
    connection,
    "
      UPDATE medium_articles
      SET
        manual_relevance_status = ?,
        manual_relevance_checked_at = ?
      WHERE url = ?
    ",
    params = list(status, current_timestamp(), article_url)
  )
}

choose_review_action <- function() {
  repeat {
    message("\nChoose:")
    message("r / relevant     = mark relevant, copy URL, then wait for bookmarklet import")
    message("m / maybe        = mark maybe, copy URL, then wait for bookmarklet import")
    message("n / not relevant = mark not relevant")
    message("s / skip         = leave unreviewed and show another candidate")
    message("q / quit / exit  = exit")

    choice <- readLines("stdin", n = 1, warn = FALSE)

    if (length(choice) == 0) {
      return("quit")
    }

    choice <- tolower(trimws(choice))

    if (choice %in% c("r", "relevant")) {
      return("relevant")
    }

    if (choice %in% c("m", "maybe")) {
      return("maybe")
    }

    if (choice %in% c("n", "not relevant", "not_relevant", "not-relevant")) {
      return("not_relevant")
    }

    if (choice %in% c("s", "skip", "")) {
      return("skip")
    }

    if (choice %in% c("q", "quit", "exit")) {
      return("quit")
    }

    message("I did not understand that. Type r, m, n, s, or q.")
  }
}

choose_await_import_action <- function(suggested_url) {
  repeat {
    message("\nAwaiting bookmarklet import")
    message("---------------------------")
    message("Suggested article URL: ", suggested_url)
    message("Press Enter / i / import to import current bookmarklet JSON from clipboard.")
    message("s / skip to leave import for later and show another candidate.")
    message("q / quit / exit to exit.")

    choice <- readLines("stdin", n = 1, warn = FALSE)

    if (length(choice) == 0) {
      return("quit")
    }

    choice <- tolower(trimws(choice))

    if (choice %in% c("", "i", "import")) {
      return("import")
    }

    if (choice %in% c("s", "skip")) {
      return("skip")
    }

    if (choice %in% c("q", "quit", "exit")) {
      return("quit")
    }

    message("I did not understand that. Press Enter to import, or type s or q.")
  }
}

await_import_for_candidate <- function(connection, suggested_url) {
  repeat {
    action <- choose_await_import_action(suggested_url)

    if (identical(action, "skip")) {
      message("Import left for later.")
      return("next")
    }

    if (identical(action, "quit")) {
      return("quit")
    }

    input_text <- read_clipboard_text()

    if (is.na(input_text) || trimws(input_text) == "") {
      message("The clipboard is empty. Click the bookmarklet first, then try import again.")
      next
    }

    imported_article <- tryCatch(
      parse_bookmarklet_input(input_text),
      error = function(error) {
        message("\nImport failed:")
        message("The clipboard does not contain valid bookmarklet JSON or old TSV data.")
        message(conditionMessage(error))
        NULL
      }
    )

    if (is.null(imported_article)) {
      next
    }

    imported_url <- imported_article$url

    if (!is.na(imported_url) && !identical(imported_url, suggested_url)) {
      message("Warning: imported URL differs from suggested URL.")
      message("Suggested URL: ", suggested_url)
      message("Imported URL: ", imported_url)
    }

    tryCatch(
      {
        import_bookmarklet_text(connection, input_text)
        return("next")
      },
      error = function(error) {
        message("\nImport failed:")
        message(conditionMessage(error))
        message("Click the bookmarklet again, then try import again.")
      }
    )
  }
}

run_review_queue <- function(connection) {
  message("\nRecommended workflow")
  message("--------------------")
  message("1. Review the suggested RSS article candidate.")
  message("2. Mark relevant or maybe to copy its URL to your clipboard.")
  message("3. Open the article, click the Medium bookmarklet, then return here.")
  message("4. Import the current clipboard when prompted.")

  repeat {
    remaining <- candidate_count(connection)

    if (remaining == 0) {
      message("\nNo unreviewed RSS candidates without manual stats remain.")
      message("")
      message("Options:")
      message("[r] refresh/check RSS candidates again")
      message("[a] add/import arbitrary Medium article from bookmarklet JSON or JSON file path")
      message("[q] quit")
      choice <- readLines("stdin", n = 1, warn = FALSE)

      if (length(choice) == 0) {
        next
      }

      choice <- tolower(trimws(choice))

      if (choice %in% c("", "r", "refresh")) {
        next
      }

      if (choice %in% c("a", "add", "import")) {
        import_arbitrary_article(connection)
        next
      }

      if (choice %in% c("q", "quit", "exit")) {
        message("Exiting.")
        break
      }

      message("I did not understand that.")
      next
    }

    candidate <- next_candidate(connection)

    if (nrow(candidate) == 0) {
      next
    }

    show_candidate(connection, candidate)
    action <- choose_review_action()
    article_url <- candidate$url[1]

    if (identical(action, "quit")) {
      message("Exiting.")
      break
    }

    if (identical(action, "skip")) {
      next
    }

    if (identical(action, "not_relevant")) {
      mark_candidate_relevance(connection, article_url, "not_relevant")
      message("Marked not relevant.")
      next
    }

    if (action %in% c("relevant", "maybe")) {
      mark_candidate_relevance(connection, article_url, action)
      copied <- write_clipboard_text(article_url)
      message("Marked ", action, ".")
      if (copied) {
        message("URL copied to clipboard.")
      }
      message("Article URL: ", article_url)
      message("Open the article, click the Medium bookmarklet, then return here.")

      result <- await_import_for_candidate(connection, article_url)
      if (identical(result, "quit")) {
        message("Exiting.")
        break
      }
    }
  }
}

if (!file.exists(database_path)) {
  stop(
    "The database does not exist yet.\n\n",
    "Create it first by running:\n\n",
    "Rscript scripts/collect_medium_rss.R",
    call. = FALSE
  )
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

if (!dbExistsTable(connection, "medium_articles")) {
  stop("The medium_articles table does not exist yet. Run the RSS collector first.", call. = FALSE)
}

ensure_public_stats_schema(connection)
ensure_public_stats_columns(connection)
ensure_medium_articles_columns(connection)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 1) {
  stop(
    "Please provide at most one input file path.\n\n",
    "Example:\n",
    "Rscript scripts/import_medium_manual_stats.R debug_samples/latest_medium_bookmarklet.json",
    call. = FALSE
  )
}

if (length(args) == 1) {
  input_path <- args[1]

  if (!file.exists(input_path)) {
    stop(
      "The input file does not exist:\n\n",
      input_path,
      "\n\nSave the bookmarklet JSON to a file first, for example:\n",
      "pbpaste > debug_samples/latest_medium_bookmarklet.json",
      call. = FALSE
    )
  }

  message("Reading bookmarklet data from file:")
  message(input_path)
  input_text <- read_input_file_text(input_path)

  if (identical(detect_json_source_type(input_text), "medium_tag_page_bookmarklet")) {
    stop(
      "This file is Medium tag-page bookmarklet JSON.\n\n",
      "Use the shared file-drop launcher or run:\n\n",
      "Rscript scripts/import_medium_tag_page_bookmarklet.R ",
      input_path,
      call. = FALSE
    )
  }

  import_bookmarklet_text(connection, input_text)
} else {
  run_review_queue(connection)
}
