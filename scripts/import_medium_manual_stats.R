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

database_path <- file.path("data", "medium_articles.sqlite")

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

ensure_medium_articles_columns <- function(connection) {
  add_column_if_missing(connection, "medium_articles", "canonical_url", "TEXT")
  add_column_if_missing(connection, "medium_articles", "subtitle", "TEXT")
  add_column_if_missing(connection, "medium_articles", "publication", "TEXT")
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
      visible_text_word_count = suppressWarnings(as.integer(parsed$visible_text_word_count)),
      visible_article_text_truncated = if (isTRUE(parsed$visible_article_text_truncated)) {
        1L
      } else if (identical(parsed$visible_article_text_truncated, FALSE)) {
        0L
      } else {
        NA_integer_
      },
      visible_article_text_max_chars = suppressWarnings(as.integer(parsed$visible_article_text_max_chars)),
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
          visible_article_collected_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
        END
      WHERE url = ?
    ",
    params = list(
      article$title,
      article$author,
      article$canonical_url,
      article$subtitle,
      article$publication,
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
  article_status <- upsert_medium_article(connection, article, article_url)

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
          error_message
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
        NA_character_
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
  message("Read time: ", ifelse(is.na(article$read_time), "(blank)", article$read_time))
  message("Member only: ", ifelse(is.na(article$is_member_only), "NA", article$is_member_only))
  message("Visible word count: ", ifelse(is.na(article$visible_text_word_count), "NA", article$visible_text_word_count))
  message("Visible text truncated: ", ifelse(is.na(article$visible_article_text_truncated), "NA", article$visible_article_text_truncated))
  message("Observed at: ", article$observed_at)
}

read_clipboard_text <- function() {
  if (Sys.which("pbpaste") == "") {
    message("Clipboard import requires macOS pbpaste. Use file-based import instead.")
    return(NA_character_)
  }

  clipboard_lines <- system2("pbpaste", stdout = TRUE, stderr = FALSE)
  paste(clipboard_lines, collapse = "\n")
}

choose_menu_option <- function() {
  repeat {
    message("\nMedium Manual Stats Importer")
    message("----------------------------")
    message("Press Enter, 1, i, or import to import the current clipboard.")
    message("Type q, quit, exit, or 2 to exit.")
    choice <- tolower(trimws(readLines("stdin", n = 1, warn = FALSE)))

    if (length(choice) == 0 || identical(choice, "")) {
      return(1L)
    }

    if (choice %in% c("1", "i", "import")) {
      return(1L)
    }

    if (choice %in% c("q", "quit", "exit", "2")) {
      return(2L)
    }

    message("I did not understand that. Press Enter to import, or type q to exit.")
  }
}

run_clipboard_menu <- function(connection) {
  message("\nRecommended workflow")
  message("--------------------")
  message("1. Open a Medium article in your browser.")
  message("2. Click the Medium bookmarklet so it copies JSON.")
  message("3. Return here and import the current clipboard.")
  message("4. Quit when finished.")

  repeat {
    choice <- choose_menu_option()

    if (identical(choice, 1L)) {
      input_text <- read_clipboard_text()

      if (is.na(input_text)) {
        next
      }

      if (trimws(input_text) == "") {
        message("The clipboard is empty. Click the bookmarklet first, then choose Import current clipboard again.")
        next
      }

      tryCatch(
        import_bookmarklet_text(connection, input_text),
        error = function(error) {
          message("\nImport failed:")
          message(conditionMessage(error))
          message("Copy fresh bookmarklet JSON and choose Import current clipboard again.")
        }
      )
      next
    }

    if (identical(choice, 2L)) {
      message("Exiting.")
      break
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
  input_text <- paste(readLines(input_path, warn = FALSE), collapse = "\n")
  import_bookmarklet_text(connection, input_text)
} else {
  run_clipboard_menu(connection)
}
