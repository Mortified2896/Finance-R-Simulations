required_packages <- c("xml2", "rvest", "DBI", "RSQLite")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("xml2", "rvest", "DBI", "RSQLite"))',
    call. = FALSE
  )
}

library(xml2)
library(rvest)
library(DBI)
library(RSQLite)

database_path <- Sys.getenv("MEDIUM_DB_PATH", file.path("data", "db", "medium_articles.sqlite"))
default_stats_dir <- file.path("debug_samples", "Stats Page")

clean_text <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x)) {
    return(NA_character_)
  }

  x <- trimws(as.character(x)[1])
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)

  if (identical(x, "")) {
    return(NA_character_)
  }

  x
}

is_missing_text <- function(x) {
  length(x) == 0 || is.null(x) || is.na(x) || trimws(as.character(x)[1]) == ""
}

current_timestamp <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

blank_if_missing <- function(x) {
  if (is_missing_text(x)) {
    return(NA_character_)
  }

  as.character(x)[1]
}

clean_url <- function(x) {
  x <- clean_text(x)

  if (is.na(x)) {
    return(NA_character_)
  }

  if (grepl("^/", x)) {
    x <- paste0("https://medium.com", x)
  }

  x <- sub("\\?.*$", "", x)
  x <- sub("#.*$", "", x)
  x
}

extract_medium_post_id <- function(url) {
  url <- clean_url(url)

  if (is.na(url)) {
    return(NA_character_)
  }

  match <- regmatches(url, regexpr("[A-Fa-f0-9]{12,}$", url))
  if (length(match) == 0 || identical(match, character(0))) {
    return(NA_character_)
  }

  tolower(match[1])
}

parse_compact_number <- function(x) {
  x <- clean_text(x)

  if (is.na(x) || x %in% c("-", "\u2014")) {
    return(NA_integer_)
  }

  compact <- toupper(gsub(",", "", gsub("\\s+", "", x)))
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

parse_earnings_usd <- function(x) {
  x <- clean_text(x)

  if (is.na(x) || x %in% c("-", "\u2014")) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", x)))
}

parse_stats_page_date <- function(x) {
  x <- clean_text(x)

  if (is.na(x)) {
    return(NA_character_)
  }

  parsed <- suppressWarnings(as.Date(x, format = "%b %d, %Y"))

  if (is.na(parsed)) {
    return(NA_character_)
  }

  format(parsed, "%Y-%m-%d")
}

extract_iso_day <- function(x) {
  x <- blank_if_missing(x)

  if (is.na(x)) {
    return(NA_character_)
  }

  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    return(x)
  }

  if (grepl("^\\d{4}-\\d{2}-\\d{2}T", x)) {
    return(substr(x, 1, 10))
  }

  NA_character_
}

date_update_counters <- function() {
  list(
    filled = 0L,
    normalized = 0L,
    skipped_equivalent = 0L,
    skipped_preserved = 0L,
    failed_parse = 0L,
    conflicts = 0L
  )
}

increment_counter <- function(counters, name) {
  counters[[name]] <- counters[[name]] + 1L
  counters
}

apply_article_publication_date <- function(connection, article_id, existing_published_at, raw_stats_date, log_prefix = NULL) {
  normalized_stats_date <- parse_stats_page_date(raw_stats_date)

  if (is.na(normalized_stats_date)) {
    return(list(
      status = "failed_parse",
      counters = increment_counter(date_update_counters(), "failed_parse"),
      normalized_value = NA_character_,
      message = if (!is_missing_text(raw_stats_date)) {
        paste0(if (is_missing_text(log_prefix)) "" else log_prefix, "Could not parse stats publication date: ", raw_stats_date)
      } else {
        NULL
      }
    ))
  }

  existing_published_at <- blank_if_missing(existing_published_at)

  if (is.na(existing_published_at)) {
    dbExecute(
      connection,
      "UPDATE medium_articles SET published_at = ? WHERE id = ?",
      params = list(normalized_stats_date, article_id)
    )

    return(list(
      status = "filled",
      counters = increment_counter(date_update_counters(), "filled"),
      normalized_value = normalized_stats_date,
      message = if (!is_missing_text(log_prefix)) paste0(log_prefix, "Filled blank article published_at with ", normalized_stats_date) else NULL
    ))
  }

  existing_iso_day <- extract_iso_day(existing_published_at)

  if (!is.na(existing_iso_day) && identical(existing_iso_day, normalized_stats_date)) {
    return(list(
      status = "skipped_equivalent",
      counters = increment_counter(date_update_counters(), "skipped_equivalent"),
      normalized_value = normalized_stats_date,
      message = NULL
    ))
  }

  legacy_normalized <- parse_stats_page_date(existing_published_at)

  if (!is.na(legacy_normalized) && identical(legacy_normalized, normalized_stats_date) && !identical(existing_published_at, normalized_stats_date)) {
    dbExecute(
      connection,
      "UPDATE medium_articles SET published_at = ? WHERE id = ?",
      params = list(normalized_stats_date, article_id)
    )

    return(list(
      status = "normalized",
      counters = increment_counter(date_update_counters(), "normalized"),
      normalized_value = normalized_stats_date,
      message = if (!is_missing_text(log_prefix)) paste0(log_prefix, "Normalized legacy article published_at from ", existing_published_at, " to ", normalized_stats_date) else NULL
    ))
  }

  counters <- date_update_counters()
  counters <- increment_counter(counters, "skipped_preserved")
  counters <- increment_counter(counters, "conflicts")

  list(
    status = "conflict",
    counters = counters,
    normalized_value = normalized_stats_date,
    message = if (!is_missing_text(log_prefix)) {
      paste0(
        log_prefix,
        "Preserved existing article published_at ",
        existing_published_at,
        " instead of overwriting with ",
        normalized_stats_date
      )
    } else {
      NULL
    }
  )
}

merge_counters <- function(total, delta) {
  for (name in names(total)) {
    total[[name]] <- total[[name]] + delta[[name]]
  }

  total
}

add_column_if_missing <- function(connection, table_name, column_name, column_definition) {
  table_info <- dbGetQuery(connection, paste0("PRAGMA table_info(", table_name, ")"))

  if (!column_name %in% table_info$name) {
    dbExecute(connection, paste("ALTER TABLE", table_name, "ADD COLUMN", column_name, column_definition))
    message("Added column: ", table_name, ".", column_name)
  }
}

ensure_medium_articles_columns <- function(connection) {
  add_column_if_missing(connection, "medium_articles", "is_own_article", "INTEGER DEFAULT 0")
  add_column_if_missing(connection, "medium_articles", "own_article_source", "TEXT")
  add_column_if_missing(connection, "medium_articles", "own_article_detected_at", "TEXT")
  add_column_if_missing(connection, "medium_articles", "read_time", "TEXT")
  add_column_if_missing(connection, "medium_articles", "is_member_only", "INTEGER")
  add_column_if_missing(connection, "medium_articles", "medium_post_id", "TEXT")
}

ensure_own_stats_table <- function(connection) {
  dbExecute(connection, "
    CREATE TABLE IF NOT EXISTS medium_own_story_stats (
      id INTEGER PRIMARY KEY,
      observed_at TEXT NOT NULL,
      stats_period_type TEXT,
      stats_period_label TEXT,
      stats_date_range_label TEXT,
      story_url TEXT NOT NULL,
      medium_post_id TEXT,
      title_snapshot TEXT,
      is_member_only_snapshot INTEGER,
      read_time_snapshot TEXT,
      published_date_snapshot TEXT,
      presentations_raw TEXT,
      presentations_count INTEGER,
      views_raw TEXT,
      views_count INTEGER,
      reads_raw TEXT,
      reads_count INTEGER,
      earnings_raw TEXT,
      earnings_usd REAL,
      source_file TEXT,
      imported_at TEXT NOT NULL,
      UNIQUE(story_url, observed_at)
    )
  ")

  invisible(tryCatch(
    dbExecute(
      connection,
      "
        CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_own_story_stats_story_url_observed_at
        ON medium_own_story_stats(story_url, observed_at)
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

find_default_stats_html <- function() {
  if (!dir.exists(default_stats_dir)) {
    stop(
      "No input path was provided, and the default stats fixture directory does not exist:\n\n",
      default_stats_dir,
      call. = FALSE
    )
  }

  candidates <- list.files(default_stats_dir, pattern = "\\.html?$", full.names = TRUE, ignore.case = TRUE)
  candidates <- candidates[!grepl("_files", candidates, fixed = TRUE)]

  if (length(candidates) == 0) {
    stop(
      "No input path was provided, and no main HTML file was found in:\n\n",
      default_stats_dir,
      "\n\nPass the saved Medium stats HTML path explicitly, for example:\n",
      'Rscript scripts/import_medium_own_stats_from_html.R "debug_samples/Stats Page/Medium Stats Page.html"',
      call. = FALSE
    )
  }

  preferred <- candidates[grepl("stats", basename(candidates), ignore.case = TRUE)]
  if (length(preferred) > 0) {
    return(preferred[1])
  }

  candidates[1]
}

extract_period_metadata <- function(doc) {
  body_text <- html_text2(html_element(doc, "body"))
  body_text <- gsub("\u00a0", " ", body_text, fixed = TRUE)
  lines <- trimws(strsplit(body_text, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]

  date_range_lines <- grep(
    "^[A-Z][a-z]+ [0-9]{1,2}, [0-9]{4} .+ Today \\(UTC\\).+Updated",
    lines,
    value = TRUE
  )

  stats_date_range_label <- if (length(date_range_lines) > 0) clean_text(date_range_lines[length(date_range_lines)]) else NA_character_
  stats_period_type <- if (any(lines == "Lifetime")) "lifetime" else if (any(lines == "Monthly")) "monthly" else NA_character_
  stats_period_label <- if (any(lines == "Latest")) "Latest" else if (!is.na(stats_period_type)) tools::toTitleCase(stats_period_type) else NA_character_

  list(
    stats_period_type = stats_period_type,
    stats_period_label = stats_period_label,
    stats_date_range_label = stats_date_range_label
  )
}

metric_column_index <- function(headers, name, fallback) {
  found <- which(tolower(headers) == tolower(name))
  if (length(found) > 0) {
    return(found[1])
  }
  fallback
}

extract_story_url <- function(row) {
  links <- html_elements(row, "a")
  link_text <- vapply(html_text2(links), clean_text, character(1))
  hrefs <- html_attr(links, "href")

  view_story <- which(link_text == "View story")
  if (length(view_story) > 0) {
    return(clean_url(hrefs[view_story[1]]))
  }

  public_links <- hrefs[
    grepl("^https://medium\\.com/", hrefs) &
      !grepl("/me/stats", hrefs, fixed = TRUE) &
      grepl("[A-Fa-f0-9]{12}", hrefs)
  ]

  if (length(public_links) > 0) {
    return(clean_url(public_links[1]))
  }

  NA_character_
}

parse_story_metadata <- function(row) {
  story_cell <- html_element(row, "td")
  title <- clean_text(html_text2(html_element(story_cell, "h2")))
  story_text <- html_text2(story_cell)
  story_url <- extract_story_url(row)

  read_time <- regmatches(story_text, regexpr("[0-9]+\\s+min read", story_text, ignore.case = TRUE))
  read_time <- if (length(read_time) == 0 || identical(read_time, character(0))) NA_character_ else clean_text(read_time[1])

  published_date <- regmatches(
    story_text,
    regexpr("[A-Z][a-z]{2}\\s+[0-9]{1,2},\\s+[0-9]{4}", story_text)
  )
  published_date <- if (length(published_date) == 0 || identical(published_date, character(0))) NA_character_ else clean_text(published_date[1])

  list(
    title = title,
    story_url = story_url,
    medium_post_id = extract_medium_post_id(story_url),
    is_member_only = if (length(html_elements(story_cell, 'button[aria-label="Member-only story"]')) > 0) 1L else 0L,
    read_time = read_time,
    published_date = published_date
  )
}

parse_stats_html <- function(input_path, observed_at = current_timestamp()) {
  doc <- read_html(input_path)
  table <- html_element(doc, "table")

  if (is.na(xml_name(table))) {
    stop("Could not find a stats table in the HTML file: ", input_path, call. = FALSE)
  }

  headers <- html_text2(html_elements(table, "thead th"))
  headers <- vapply(headers, clean_text, character(1))

  if (length(headers) == 0) {
    headers <- c("Story", "Presentations", "Views", "Reads", "Earnings")
  }

  rows <- html_elements(table, "tbody tr")

  period <- extract_period_metadata(doc)
  presentations_index <- metric_column_index(headers, "Presentations", 2L)
  views_index <- metric_column_index(headers, "Views", 3L)
  reads_index <- metric_column_index(headers, "Reads", 4L)
  earnings_index <- metric_column_index(headers, "Earnings", 5L)

  parsed <- lapply(rows, function(row) {
    cells <- html_elements(row, "td")
    cell_text <- vapply(html_text2(cells), clean_text, character(1))
    metadata <- parse_story_metadata(row)

    if (length(cells) < 2 || is.na(metadata$story_url) || is.na(metadata$title)) {
      return(NULL)
    }

    presentations_raw <- if (presentations_index <= length(cell_text)) cell_text[presentations_index] else NA_character_
    views_raw <- if (views_index <= length(cell_text)) cell_text[views_index] else NA_character_
    reads_raw <- if (reads_index <= length(cell_text)) cell_text[reads_index] else NA_character_
    earnings_raw <- if (earnings_index <= length(cell_text)) cell_text[earnings_index] else NA_character_

    data.frame(
      observed_at = observed_at,
      stats_period_type = period$stats_period_type,
      stats_period_label = period$stats_period_label,
      stats_date_range_label = period$stats_date_range_label,
      story_url = metadata$story_url,
      medium_post_id = metadata$medium_post_id,
      title_snapshot = metadata$title,
      is_member_only_snapshot = metadata$is_member_only,
      read_time_snapshot = metadata$read_time,
      published_date_snapshot = metadata$published_date,
      presentations_raw = presentations_raw,
      presentations_count = parse_compact_number(presentations_raw),
      views_raw = views_raw,
      views_count = parse_compact_number(views_raw),
      reads_raw = reads_raw,
      reads_count = parse_compact_number(reads_raw),
      earnings_raw = earnings_raw,
      earnings_usd = parse_earnings_usd(earnings_raw),
      stringsAsFactors = FALSE
    )
  })

  parsed <- parsed[!vapply(parsed, is.null, logical(1))]

  if (length(parsed) == 0) {
    return(data.frame())
  }

  do.call(rbind, parsed)
}

find_existing_article <- function(connection, story_url, medium_post_id) {
  if (!is.na(medium_post_id)) {
    found <- dbGetQuery(
      connection,
      "
        SELECT *
        FROM medium_articles
        WHERE medium_post_id = ?
           OR url LIKE ?
           OR raw_url LIKE ?
        LIMIT 1
      ",
      params = list(medium_post_id, paste0("%", medium_post_id), paste0("%", medium_post_id, "%"))
    )

    if (nrow(found) > 0) {
      return(found)
    }
  }

  dbGetQuery(
    connection,
    "
      SELECT *
      FROM medium_articles
      WHERE url = ?
         OR raw_url = ?
      LIMIT 1
    ",
    params = list(story_url, story_url)
  )
}

upsert_medium_article <- function(connection, row, imported_at) {
  publication_date_counters <- date_update_counters()
  publication_date_messages <- character()
  existing <- find_existing_article(connection, row$story_url, row$medium_post_id)
  normalized_published_at <- parse_stats_page_date(row$published_date_snapshot)

  if (nrow(existing) == 0) {
    dbExecute(
      connection,
      "
        INSERT INTO medium_articles (
          source_tag,
          title,
          url,
          raw_url,
          fetched_at,
          published_at,
          read_time,
          is_member_only,
          medium_post_id,
          is_own_article,
          own_article_source,
          own_article_detected_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?, ?)
      ",
      params = list(
        "medium-stats-page",
        row$title_snapshot,
        row$story_url,
        row$story_url,
        imported_at,
        normalized_published_at,
        row$read_time_snapshot,
        row$is_member_only_snapshot,
        row$medium_post_id,
        "medium_stats_page",
        imported_at
      )
    )
    if (is.na(normalized_published_at) && !is_missing_text(row$published_date_snapshot)) {
      publication_date_counters <- increment_counter(publication_date_counters, "failed_parse")
      publication_date_messages <- c(
        publication_date_messages,
        paste0("Article create for ", row$story_url, ": Could not parse stats publication date: ", row$published_date_snapshot)
      )
    } else if (!is.na(normalized_published_at)) {
      publication_date_counters <- increment_counter(publication_date_counters, "filled")
    }

    return(list(
      article_status = "created",
      publication_date_counters = publication_date_counters,
      publication_date_messages = publication_date_messages
    ))
  }

  dbExecute(
    connection,
    "
      UPDATE medium_articles
      SET
        is_own_article = 1,
        own_article_source = COALESCE(NULLIF(own_article_source, ''), 'medium_stats_page'),
        own_article_detected_at = COALESCE(NULLIF(own_article_detected_at, ''), ?),
        title = COALESCE(NULLIF(title, ''), ?),
        raw_url = COALESCE(NULLIF(raw_url, ''), ?),
        published_at = COALESCE(NULLIF(published_at, ''), ?),
        read_time = COALESCE(NULLIF(read_time, ''), ?),
        is_member_only = COALESCE(is_member_only, ?),
        medium_post_id = COALESCE(NULLIF(medium_post_id, ''), ?)
      WHERE id = ?
    ",
    params = list(
      imported_at,
      row$title_snapshot,
      row$story_url,
      row$published_date_snapshot,
      row$read_time_snapshot,
      row$is_member_only_snapshot,
      row$medium_post_id,
      existing$id[1]
    )
  )

  publication_update <- apply_article_publication_date(
    connection = connection,
    article_id = existing$id[1],
    existing_published_at = existing$published_at[1],
    raw_stats_date = row$published_date_snapshot,
    log_prefix = paste0("Article update for ", row$story_url, ": ")
  )

  if (!is.null(publication_update$message)) {
    publication_date_messages <- c(publication_date_messages, publication_update$message)
  }

  list(
    article_status = "updated",
    publication_date_counters = merge_counters(publication_date_counters, publication_update$counters),
    publication_date_messages = publication_date_messages
  )
}

insert_own_stats_observation <- function(connection, row, source_file, imported_at) {
  duplicate <- dbGetQuery(
    connection,
    "
      SELECT COUNT(*) AS n
      FROM medium_own_story_stats
      WHERE story_url = ?
        AND observed_at = ?
    ",
    params = list(row$story_url, row$observed_at)
  )$n

  if (duplicate > 0) {
    return("duplicate")
  }

  dbExecute(
    connection,
    "
      INSERT INTO medium_own_story_stats (
        observed_at,
        stats_period_type,
        stats_period_label,
        stats_date_range_label,
        story_url,
        medium_post_id,
        title_snapshot,
        is_member_only_snapshot,
        read_time_snapshot,
        published_date_snapshot,
        presentations_raw,
        presentations_count,
        views_raw,
        views_count,
        reads_raw,
        reads_count,
        earnings_raw,
        earnings_usd,
        source_file,
        imported_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      row$observed_at,
      row$stats_period_type,
      row$stats_period_label,
      row$stats_date_range_label,
      row$story_url,
      row$medium_post_id,
      row$title_snapshot,
      row$is_member_only_snapshot,
      row$read_time_snapshot,
      row$published_date_snapshot,
      row$presentations_raw,
      row$presentations_count,
      row$views_raw,
      row$views_count,
      row$reads_raw,
      row$reads_count,
      row$earnings_raw,
      row$earnings_usd,
      source_file,
      imported_at
    )
  )

  "inserted"
}

backfill_article_publication_dates <- function(connection) {
  counters <- date_update_counters()
  messages <- character()

  candidates <- dbGetQuery(
    connection,
    "
      SELECT
        a.id AS article_id,
        a.url AS article_url,
        a.published_at AS article_published_at,
        latest_stats.published_date_snapshot
      FROM medium_articles a
      JOIN (
        SELECT s1.story_url, s1.medium_post_id, s1.published_date_snapshot
        FROM medium_own_story_stats s1
        JOIN (
          SELECT
            COALESCE(NULLIF(medium_post_id, ''), story_url) AS story_key,
            MAX(observed_at) AS max_observed_at
          FROM medium_own_story_stats
          WHERE published_date_snapshot IS NOT NULL
            AND TRIM(published_date_snapshot) <> ''
          GROUP BY COALESCE(NULLIF(medium_post_id, ''), story_url)
        ) latest
          ON latest.max_observed_at = s1.observed_at
         AND latest.story_key = COALESCE(NULLIF(s1.medium_post_id, ''), s1.story_url)
      ) latest_stats
        ON latest_stats.story_url = a.url
        OR (
          latest_stats.medium_post_id IS NOT NULL
          AND latest_stats.medium_post_id <> ''
          AND latest_stats.medium_post_id = a.medium_post_id
        )
    "
  )

  if (nrow(candidates) == 0) {
    return(list(counters = counters, messages = messages))
  }

  candidates <- candidates[!duplicated(candidates$article_id), ]

  for (i in seq_len(nrow(candidates))) {
    result <- apply_article_publication_date(
      connection = connection,
      article_id = candidates$article_id[i],
      existing_published_at = candidates$article_published_at[i],
      raw_stats_date = candidates$published_date_snapshot[i],
      log_prefix = paste0("Backfill for ", candidates$article_url[i], ": ")
    )

    counters <- merge_counters(counters, result$counters)
    if (!is.null(result$message)) {
      messages <- c(messages, result$message)
    }
  }

  list(counters = counters, messages = messages)
}

post_import_audit <- function(connection) {
  article_count <- dbGetQuery(connection, "
    SELECT COUNT(*) AS n
    FROM medium_articles
  ")$n

  own_stats_row_count <- dbGetQuery(connection, "
    SELECT COUNT(*) AS n
    FROM medium_own_story_stats
  ")$n

  articles_with_published_at <- dbGetQuery(connection, "
    SELECT COUNT(*) AS n
    FROM medium_articles
    WHERE published_at IS NOT NULL
      AND TRIM(published_at) <> ''
  ")$n

  articles_with_iso_published_at <- dbGetQuery(connection, "
    SELECT COUNT(*) AS n
    FROM medium_articles
    WHERE TRIM(COALESCE(published_at, '')) GLOB '????-??-??*'
  ")$n

  articles_with_raw_month_name_published_at <- dbGetQuery(connection, "
    SELECT COUNT(*) AS n
    FROM medium_articles
    WHERE TRIM(COALESCE(published_at, '')) GLOB '[A-Z][a-z][a-z] *'
  ")$n

  stats_without_article_date <- dbGetQuery(connection, "
    SELECT
      a.url,
      a.title,
      a.published_at,
      latest_stats.published_date_snapshot
    FROM medium_articles a
    JOIN (
      SELECT s1.story_url, s1.medium_post_id, s1.published_date_snapshot
      FROM medium_own_story_stats s1
      JOIN (
        SELECT
          COALESCE(NULLIF(medium_post_id, ''), story_url) AS story_key,
          MAX(observed_at) AS max_observed_at
        FROM medium_own_story_stats
        WHERE published_date_snapshot IS NOT NULL
          AND TRIM(published_date_snapshot) <> ''
        GROUP BY COALESCE(NULLIF(medium_post_id, ''), story_url)
      ) latest
        ON latest.max_observed_at = s1.observed_at
       AND latest.story_key = COALESCE(NULLIF(s1.medium_post_id, ''), s1.story_url)
    ) latest_stats
      ON latest_stats.story_url = a.url
      OR (
        latest_stats.medium_post_id IS NOT NULL
        AND latest_stats.medium_post_id <> ''
        AND latest_stats.medium_post_id = a.medium_post_id
      )
    WHERE a.published_at IS NULL
       OR TRIM(a.published_at) = ''
    ORDER BY a.url
  ")

  own_article_duplicate_groups <- dbGetQuery(connection, "
    SELECT COUNT(*) AS n
    FROM (
      SELECT url
      FROM medium_articles
      WHERE COALESCE(is_own_article, 0) = 1
      GROUP BY url
      HAVING COUNT(*) > 1
    ) duplicate_urls
  ")$n

  duplicate_medium_post_id_groups <- dbGetQuery(connection, "
    SELECT COUNT(*) AS n
    FROM (
      SELECT LOWER(TRIM(medium_post_id)) AS medium_post_id_key
      FROM medium_articles
      WHERE TRIM(COALESCE(medium_post_id, '')) <> ''
      GROUP BY LOWER(TRIM(medium_post_id))
      HAVING COUNT(*) > 1
    ) duplicate_post_ids
  ")$n

  list(
    article_count = article_count,
    own_stats_row_count = own_stats_row_count,
    articles_with_published_at = articles_with_published_at,
    articles_with_iso_published_at = articles_with_iso_published_at,
    articles_with_raw_month_name_published_at = articles_with_raw_month_name_published_at,
    stats_without_article_date = stats_without_article_date,
    own_article_duplicate_groups = own_article_duplicate_groups,
    duplicate_medium_post_id_groups = duplicate_medium_post_id_groups
  )
}

print_summary <- function(source_file, rows, article_statuses, observation_statuses, publication_date_counters, publication_date_messages, audit) {
  message("\nMedium Own Stats Import Summary")
  message("===============================")
  message("Source HTML file: ", source_file)
  message("Observed at: ", if (nrow(rows) > 0) rows$observed_at[1] else "(none)")
  message("Stats period label: ", if (nrow(rows) > 0 && !is.na(rows$stats_period_label[1])) rows$stats_period_label[1] else "(not found)")
  message("Stats date range: ", if (nrow(rows) > 0 && !is.na(rows$stats_date_range_label[1])) rows$stats_date_range_label[1] else "(not found)")
  message("Story rows found: ", nrow(rows))
  message("medium_articles rows created or updated: ", sum(article_statuses %in% c("created", "updated")))
  message("Own stats observations inserted: ", sum(observation_statuses == "inserted"))
  message("Skipped exact duplicates: ", sum(observation_statuses == "duplicate"))
  message("Article publication dates filled: ", publication_date_counters$filled)
  message("Article publication dates normalized: ", publication_date_counters$normalized)
  message("Article publication dates skipped as equivalent: ", publication_date_counters$skipped_equivalent)
  message("Article publication dates skipped/preserved: ", publication_date_counters$skipped_preserved)
  message("Article publication dates failed to parse: ", publication_date_counters$failed_parse)
  message("Article publication date conflicts: ", publication_date_counters$conflicts)

  message("\nParsed row preview")
  message("------------------")
  if (nrow(rows) == 0) {
    message("No parsed rows.")
  } else {
    preview <- rows[
      seq_len(min(10, nrow(rows))),
      c("title_snapshot", "presentations_raw", "views_raw", "reads_raw", "earnings_raw", "story_url")
    ]
    names(preview) <- c("title", "presentations", "views", "reads", "earnings", "url")
    print(preview, row.names = FALSE)
  }

  if (length(publication_date_messages) > 0) {
    message("\nPublication date updates")
    message("------------------------")
    unique_messages <- unique(publication_date_messages)
    for (message_text in unique_messages) {
      message(message_text)
    }
  }

  message("\nPost-import audit")
  message("-----------------")
  message("Total article count: ", audit$article_count)
  message("Total own stats row count: ", audit$own_stats_row_count)
  message("Articles with non-blank published_at: ", audit$articles_with_published_at)
  message("Articles with normalized ISO published_at: ", audit$articles_with_iso_published_at)
  message("Articles still showing raw month-name published_at: ", audit$articles_with_raw_month_name_published_at)
  message("Own-article duplicate URL groups: ", audit$own_article_duplicate_groups)
  message("Duplicate medium_post_id groups: ", audit$duplicate_medium_post_id_groups)
  message("Rows where stats has published_date_snapshot but article published_at is blank: ", nrow(audit$stats_without_article_date))

  if (nrow(audit$stats_without_article_date) > 0) {
    print(audit$stats_without_article_date, row.names = FALSE)
  }

  invisible(audit)
}

print_startup_diagnostics <- function(connection, database_path) {
  absolute_database_path <- normalizePath(database_path, winslash = "/", mustWork = FALSE)
  file_exists <- file.exists(database_path)
  article_count <- if (dbExistsTable(connection, "medium_articles")) {
    dbGetQuery(connection, "SELECT COUNT(*) AS n FROM medium_articles")$n
  } else {
    NA_integer_
  }
  own_stats_row_count <- if (dbExistsTable(connection, "medium_own_story_stats")) {
    dbGetQuery(connection, "SELECT COUNT(*) AS n FROM medium_own_story_stats")$n
  } else {
    0L
  }

  message("\nDatabase startup")
  message("----------------")
  message("Database path: ", absolute_database_path)
  message("Database exists: ", if (file_exists) "yes" else "no")
  message("Current medium_articles row count: ", ifelse(is.na(article_count), "table missing", article_count))
  message("Current medium_own_story_stats row count: ", own_stats_row_count)
}

message("Medium Own Stats HTML Importer")
message("==============================")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 1) {
  stop(
    "Please provide at most one Medium stats HTML path.\n\n",
    'Example: Rscript scripts/import_medium_own_stats_from_html.R "debug_samples/Stats Page/Medium Stats Page.html"',
    call. = FALSE
  )
}

input_path <- if (length(args) == 1) args[1] else find_default_stats_html()

if (!file.exists(input_path)) {
  stop("The input HTML file does not exist:\n\n", input_path, call. = FALSE)
}

if (!file.exists(database_path)) {
  stop(
    "The database does not exist yet:\n\n",
    database_path,
    "\n\nCreate it first by running:\n\n",
    "Rscript scripts/collect_medium_rss.R",
    call. = FALSE
  )
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

if (!dbExistsTable(connection, "medium_articles")) {
  stop("The medium_articles table does not exist yet. Run the RSS collector first.", call. = FALSE)
}

ensure_medium_articles_columns(connection)
ensure_own_stats_table(connection)
print_startup_diagnostics(connection, database_path)

observed_at <- Sys.getenv("MEDIUM_OBSERVED_AT", current_timestamp())
imported_at <- current_timestamp()
rows <- parse_stats_html(input_path, observed_at = observed_at)
publication_date_counters <- date_update_counters()
publication_date_messages <- character()

if (nrow(rows) == 0) {
  backfill_result <- backfill_article_publication_dates(connection)
  publication_date_counters <- merge_counters(publication_date_counters, backfill_result$counters)
  publication_date_messages <- c(publication_date_messages, backfill_result$messages)
  audit <- post_import_audit(connection)
  print_summary(input_path, rows, character(), character(), publication_date_counters, publication_date_messages, audit)
  quit(status = 0)
}

article_statuses <- character(nrow(rows))
observation_statuses <- character(nrow(rows))

dbBegin(connection)
tryCatch(
  {
    for (i in seq_len(nrow(rows))) {
      article_result <- upsert_medium_article(connection, rows[i, ], imported_at)
      article_statuses[i] <- article_result$article_status
      publication_date_counters <- merge_counters(publication_date_counters, article_result$publication_date_counters)
      publication_date_messages <- c(publication_date_messages, article_result$publication_date_messages)
      observation_statuses[i] <- insert_own_stats_observation(connection, rows[i, ], input_path, imported_at)
    }

    backfill_result <- backfill_article_publication_dates(connection)
    publication_date_counters <- merge_counters(publication_date_counters, backfill_result$counters)
    publication_date_messages <- c(publication_date_messages, backfill_result$messages)
    dbCommit(connection)
  },
  error = function(error) {
    dbRollback(connection)
    stop("Own stats import failed: ", conditionMessage(error), call. = FALSE)
  }
)

audit <- post_import_audit(connection)
print_summary(
  input_path,
  rows,
  article_statuses,
  observation_statuses,
  publication_date_counters,
  publication_date_messages,
  audit
)
