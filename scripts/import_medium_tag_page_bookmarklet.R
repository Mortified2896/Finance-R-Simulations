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

source(file.path("scripts", "medium_tag_import_helpers.R"))

database_path <- Sys.getenv("MEDIUM_DB_PATH", file.path("data", "db", "medium_articles.sqlite"))
STATS_OBSERVATION_MIN_GAP_HOURS <- 12

first_non_missing <- function(primary_value, fallback_value) {
  if (!is_missing_text(primary_value)) {
    return(clean_text(primary_value))
  }

  clean_text(fallback_value)
}

clean_card <- function(card, fallback_position = NA_integer_) {
  normalized_url <- normalize_medium_url(scalar_from_json(card$article_url))
  article_tags <- card$article_tags
  article_tags_json <- if (is.null(article_tags) || length(article_tags) == 0) {
    NA_character_
  } else {
    jsonlite::toJSON(unlist(article_tags), auto_unbox = TRUE)
  }

  list(
    position = if (!is.na(integer_from_json(card$position))) integer_from_json(card$position) else fallback_position,
    section = clean_text(card$section),
    article_url = normalized_url,
    medium_post_id = {
      post_id <- clean_text(card$medium_post_id)
      if (is.na(post_id)) extract_medium_post_id(normalized_url) else tolower(post_id)
    },
    title = clean_text(card$title),
    subtitle = clean_text(card$subtitle),
    author_name = clean_text(card$author_name),
    author_url = normalize_medium_url(scalar_from_json(card$author_url)),
    author_username = clean_text(card$author_username),
    author_medium_user_id = clean_text(card$author_medium_user_id),
    publication_name = clean_text(card$publication_name),
    publication_url = normalize_medium_url(scalar_from_json(card$publication_url)),
    publication_id = clean_text(card$publication_id),
    publication_slug = clean_text(card$publication_slug),
    publication_domain = clean_text(card$publication_domain),
    publication_subscriber_count = integer_from_json(card$publication_subscriber_count),
    publication_status = clean_text(card$publication_status),
    published_label = clean_text(card$published_label),
    published_at = clean_text(card$published_at),
    published_date_inferred = clean_text(card$published_date_inferred),
    published_date_inferred_from = clean_text(card$published_date_inferred_from),
    published_at_inferred = clean_text(card$published_at_inferred),
    published_at_inferred_precision = clean_text(card$published_at_inferred_precision),
    updated_at = clean_text(card$updated_at),
    read_time_minutes = real_from_json(card$read_time_minutes),
    article_tags_json = clean_text(article_tags_json),
    claps = integer_from_json(card$claps),
    responses = integer_from_json(card$responses),
    is_member_only = logical_flag_from_json(card$is_member_only),
    thumbnail_url = clean_text(card$thumbnail_url),
    thumbnail_alt = clean_text(card$thumbnail_alt),
    thumbnail_source = if (!is_missing_text(card$thumbnail_url)) "tag_card_thumbnail" else NA_character_,
    thumbnail_confidence = if (!is_missing_text(card$thumbnail_url)) "high" else "missing",
    thumbnail_status = if (!is_missing_text(card$thumbnail_url)) "found_confirmed_card" else "not_found",
    recommendation_source = clean_text(card$recommendation_source),
    recommendation_surface = clean_text(card$recommendation_surface),
    recommendation_tag_slug = clean_text(card$recommendation_tag_slug),
    recommendation_position = integer_from_json(card$recommendation_position),
    recommendation_result_set_size = integer_from_json(card$recommendation_result_set_size)
  )
}

clean_cards <- function(cards) {
  cleaned_cards <- list()

  for (index in seq_along(cards)) {
    cleaned <- clean_card(cards[[index]], fallback_position = index)

    if (is.na(cleaned$article_url)) {
      next
    }

    cleaned_cards[[length(cleaned_cards) + 1]] <- cleaned
  }

  cleaned_cards
}

read_tag_page_payload <- function(input_path) {
  parsed <- fromJSON(input_path, simplifyVector = FALSE)
  source_type <- clean_text(parsed$source_type)

  if (!identical(source_type, "medium_tag_page_bookmarklet")) {
    stop(
      "The JSON file does not contain source_type = 'medium_tag_page_bookmarklet'.",
      call. = FALSE
    )
  }

  cards <- parsed$cards

  if (is.null(cards) || !length(cards)) {
    cards <- list()
  }

  list(
    source_type = source_type,
    schema_version = integer_from_json(parsed$schema_version),
    tag_slug = clean_text(parsed$tag_slug),
    tag_url = normalize_medium_url(scalar_from_json(parsed$tag_url)),
    page_variant = {
      parsed_variant <- clean_text(parsed$page_variant)
      inferred_variant <- infer_page_variant_from_url(scalar_from_json(parsed$tag_url))
      if (!is_missing_text(parsed_variant)) parsed_variant else inferred_variant
    },
    captured_at = clean_text(parsed$captured_at),
    page_title = clean_text(parsed$page_title),
    cards = clean_cards(cards)
  )
}

find_existing_snapshot_by_hash <- function(connection, source_file_hash) {
  if (!dbExistsTable(connection, "medium_tag_page_snapshots") || is.na(source_file_hash)) {
    return(data.frame())
  }

  dbGetQuery(
    connection,
    "
      SELECT *
      FROM medium_tag_page_snapshots
      WHERE source_file_hash = ?
      LIMIT 1
    ",
    params = list(source_file_hash)
  )
}

insert_snapshot <- function(connection, payload, input_path, source_file_hash) {
  snapshot_columns <- table_columns(connection, "medium_tag_page_snapshots")
  insert_values <- list(
    tag_slug = payload$tag_slug,
    page_variant = payload$page_variant,
    tag_url = payload$tag_url,
    page_title = payload$page_title,
    captured_at = payload$captured_at,
    imported_at = current_timestamp(),
    source_json_path = normalizePath(input_path, winslash = "/", mustWork = FALSE),
    source_file_hash = source_file_hash,
    source_type = payload$source_type,
    schema_version = payload$schema_version,
    cards_found = length(payload$cards)
  )

  insert_names <- names(insert_values)[names(insert_values) %in% snapshot_columns]

  dbExecute(
    connection,
    paste0(
      "INSERT INTO medium_tag_page_snapshots (",
      paste(insert_names, collapse = ", "),
      ") VALUES (",
      paste(rep("?", length(insert_names)), collapse = ", "),
      ")"
    ),
    params = unname(insert_values[insert_names])
  )

  dbGetQuery(connection, "SELECT last_insert_rowid() AS id")$id[1]
}

insert_observation <- function(connection, snapshot_id, article_id, card, payload, imported_at) {
  observed_at <- first_non_missing(payload$captured_at, imported_at)

  dbExecute(
    connection,
    "
      INSERT OR IGNORE INTO medium_tag_page_observations (
        snapshot_id,
        article_id,
        article_url_normalized,
        medium_post_id,
        tag_slug,
        page_position,
        section_name,
        title,
        subtitle,
        author_name,
        author_url,
        author_username,
        author_medium_user_id,
        publication_name,
        publication_url,
        publication_id,
        publication_slug,
        publication_domain,
        publication_subscriber_count,
        publication_status,
        published_label,
        published_at,
        published_date_inferred,
        published_date_inferred_from,
        published_at_inferred,
        published_at_inferred_precision,
        updated_at,
        read_time_minutes,
        article_tags_json,
        claps,
        responses,
        is_member_only,
        thumbnail_url,
        thumbnail_alt,
        thumbnail_source,
        thumbnail_confidence,
        thumbnail_status,
        recommendation_source,
        recommendation_surface,
        recommendation_tag_slug,
        recommendation_position,
        recommendation_result_set_size,
        observed_at,
        imported_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      snapshot_id,
      article_id,
      card$article_url,
      card$medium_post_id,
      payload$tag_slug,
      card$position,
      card$section,
      card$title,
      card$subtitle,
      card$author_name,
      card$author_url,
      card$author_username,
      card$author_medium_user_id,
      card$publication_name,
      card$publication_url,
      card$publication_id,
      card$publication_slug,
      card$publication_domain,
      card$publication_subscriber_count,
      card$publication_status,
      card$published_label,
      card$published_at,
      card$published_date_inferred,
      card$published_date_inferred_from,
      card$published_at_inferred,
      card$published_at_inferred_precision,
      card$updated_at,
      card$read_time_minutes,
      card$article_tags_json,
      card$claps,
      card$responses,
      card$is_member_only,
      card$thumbnail_url,
      card$thumbnail_alt,
      card$thumbnail_source,
      card$thumbnail_confidence,
      card$thumbnail_status,
      card$recommendation_source,
      card$recommendation_surface,
      card$recommendation_tag_slug,
      card$recommendation_position,
      card$recommendation_result_set_size,
      observed_at,
      imported_at
    )
  )
}

null_to_key <- function(value) {
  value <- clean_text(value)
  if (is.na(value)) "" else value
}

appearance_dedupe_key <- function(article_id, card, payload) {
  paste(
    article_id,
    null_to_key(payload$source_type),
    null_to_key(payload$page_variant),
    null_to_key(payload$tag_slug),
    null_to_key(payload$tag_url),
    null_to_key(card$recommendation_surface),
    null_to_key(card$recommendation_tag_slug),
    null_to_key(card$position),
    null_to_key(card$recommendation_position),
    sep = "\r"
  )
}

ensure_medium_article_public_stats_schema <- function(connection) {
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

  invisible(tryCatch(
    dbExecute(connection, "
      CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_article_public_stats_article_url_observed_at
      ON medium_article_public_stats(article_url, observed_at)
    "),
    error = function(error) {
      warning(
        "Could not create the exact public-stats observation index. The importer will still throttle in code. ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  ))
}

latest_article_observation <- function(connection, article_id) {
  if (!dbExistsTable(connection, "medium_article_public_stats")) {
    return(data.frame())
  }

  dbGetQuery(
    connection,
    "
      SELECT
        s.*
      FROM medium_article_public_stats s
      INNER JOIN medium_articles a
        ON s.article_url = a.url
      WHERE a.id = ?
      ORDER BY s.observed_at DESC, s.id DESC
      LIMIT 1
    ",
    params = list(article_id)
  )
}

parse_observed_at <- function(value) {
  value <- clean_text(value)

  if (is.na(value)) {
    return(as.POSIXct(NA))
  }

  parsed <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  if (is.na(parsed)) {
    parsed <- as.POSIXct(value, tz = "UTC")
  }

  parsed
}

same_count <- function(left, right) {
  if (is.na(left) && is.na(right)) {
    return(TRUE)
  }

  identical(as.integer(left), as.integer(right))
}

should_insert_article_observation <- function(
  conn,
  article_id,
  claps,
  responses,
  observed_at,
  min_gap_hours = STATS_OBSERVATION_MIN_GAP_HOURS
) {
  latest <- latest_article_observation(conn, article_id)

  if (nrow(latest) == 0) {
    return(list(insert = TRUE, reason = "first_observation"))
  }

  claps_changed <- !same_count(claps, latest$claps_count[1])
  responses_changed <- !same_count(responses, latest$responses_count[1])

  if (isTRUE(claps_changed) || isTRUE(responses_changed)) {
    return(list(insert = TRUE, reason = "stats_changed"))
  }

  latest_time <- parse_observed_at(latest$observed_at[1])
  observed_time <- parse_observed_at(observed_at)

  if (is.na(latest_time) || is.na(observed_time)) {
    return(list(insert = TRUE, reason = "min_gap_passed"))
  }

  elapsed_hours <- as.numeric(difftime(observed_time, latest_time, units = "hours"))

  if (!is.na(elapsed_hours) && elapsed_hours >= min_gap_hours) {
    return(list(insert = TRUE, reason = "min_gap_passed"))
  }

  list(insert = FALSE, reason = "unchanged_too_recent")
}

insert_article_stats_observation <- function(connection, article_url, card, observed_at, reason) {
  observed_date <- substr(observed_at, 1, 10)
  parse_status <- if (!is.na(card$claps) || !is.na(card$responses)) "ok" else "not_found"

  dbExecute(
    connection,
    "
      INSERT OR IGNORE INTO medium_article_public_stats (
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
      observed_at,
      observed_date,
      card$claps,
      card$responses,
      if (!is.na(card$claps)) as.character(card$claps) else NA_character_,
      if (!is.na(card$responses)) as.character(card$responses) else NA_character_,
      parse_status,
      paste0("medium_tag_page_", reason),
      NA_character_
    )
  )
}

print_new_import_summary <- function(summary, payload, source_file_hash, snapshot_id, database_path) {
  context_label <- if (!is_missing_text(payload$page_variant) && grepl("^publication_", payload$page_variant, ignore.case = TRUE)) "Publication" else "Tag"
  message("Imported Medium card page")
  message("-------------------------")
  message(context_label, ": ", payload$tag_slug)
  message("Page variant: ", payload$page_variant)
  message("Articles parsed: ", summary$cards_found)
  message("Articles inserted: ", summary$new_articles)
  message("Articles reused/updated: ", summary$existing_articles)
  message("Appearances inserted: ", summary$appearances_inserted)
  message("Appearances skipped as duplicates: ", summary$appearances_skipped_duplicate)
  message("Stats observations inserted: ", summary$stats_observations_inserted)
  message("Stats observations skipped unchanged/too recent: ", summary$stats_skipped_unchanged_too_recent)
  message("Stats observations inserted because first seen: ", summary$stats_inserted_first_observation)
  message("Stats observations inserted because claps/responses changed: ", summary$stats_inserted_stats_changed)
  message("Stats observations inserted because min gap passed: ", summary$stats_inserted_min_gap_passed)
  message("Queued for article import: ", summary$queued_for_import)
  message("Already queued/imported: ", summary$already_queued_or_imported)
  message("Already had full text: ", summary$already_had_full_text)
  message("Thumbnail URLs saved: ", summary$thumbnail_urls_saved)
  if (length(summary$warnings) > 0) {
    message("Warnings: ", length(summary$warnings))
    for (warning_message in unique(summary$warnings)) {
      message("- ", warning_message)
    }
  }
  message("source_file_hash: ", source_file_hash)
  message("snapshot_id: ", snapshot_id)
  message("DB: ", database_path)
}

print_duplicate_summary <- function(snapshot_row, source_file_hash, database_path) {
  message("Already imported")
  message("----------------")
  message("source_file_hash: ", source_file_hash)
  if ("id" %in% names(snapshot_row)) {
    message("snapshot_id: ", snapshot_row$id[1])
  }
  if ("tag_slug" %in% names(snapshot_row)) {
    context_label <- if ("page_variant" %in% names(snapshot_row) && !is_missing_text(snapshot_row$page_variant[1]) && grepl("^publication_", snapshot_row$page_variant[1], ignore.case = TRUE)) "Publication" else "Tag"
    message(context_label, ": ", snapshot_row$tag_slug[1])
  }
  if ("page_variant" %in% names(snapshot_row) && !is_missing_text(snapshot_row$page_variant[1])) {
    message("Page variant: ", snapshot_row$page_variant[1])
  }
  if ("captured_at" %in% names(snapshot_row) && !is_missing_text(snapshot_row$captured_at[1])) {
    message("Captured at: ", snapshot_row$captured_at[1])
  }
  if ("imported_at" %in% names(snapshot_row) && !is_missing_text(snapshot_row$imported_at[1])) {
    message("Originally imported at: ", snapshot_row$imported_at[1])
  }
  message("No new observations were inserted.")
  message("DB: ", database_path)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop(
    "Please provide exactly one Medium tag-page bookmarklet JSON file path.\n\n",
    "Example:\n",
    "Rscript scripts/import_medium_tag_page_bookmarklet.R debug_samples/medium_tag_page_fixture_small.json",
    call. = FALSE
  )
}

input_path <- args[1]

if (!file.exists(input_path)) {
  stop("The input file does not exist:\n\n", input_path, call. = FALSE)
}

dir.create(dirname(database_path), showWarnings = FALSE, recursive = TRUE)

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

ensure_medium_articles_schema(connection)
schema_info <- inspect_medium_articles_schema(connection)
ensure_medium_tag_page_schema(connection, schema_info)
ensure_medium_article_public_stats_schema(connection)

payload <- read_tag_page_payload(input_path)
source_file_hash <- compute_source_file_hash(input_path)
existing_snapshot <- find_existing_snapshot_by_hash(connection, source_file_hash)

if (nrow(existing_snapshot) > 0) {
  print_duplicate_summary(existing_snapshot, source_file_hash, database_path)
  quit(status = 0)
}

imported_at <- current_timestamp()
summary <- list(
  cards_found = length(payload$cards),
  new_articles = 0L,
  existing_articles = 0L,
  appearances_inserted = 0L,
  appearances_skipped_duplicate = 0L,
  stats_observations_inserted = 0L,
  stats_skipped_unchanged_too_recent = 0L,
  stats_inserted_first_observation = 0L,
  stats_inserted_stats_changed = 0L,
  stats_inserted_min_gap_passed = 0L,
  queued_for_import = 0L,
  already_queued_or_imported = 0L,
  already_had_full_text = 0L,
  thumbnail_urls_saved = 0L,
  warnings = character()
)

if (!"page_variant" %in% table_columns(connection, "medium_tag_page_snapshots")) {
  summary$warnings <- c(
    summary$warnings,
    "medium_tag_page_snapshots.page_variant is not available. The importer will continue without storing page_variant."
  )
}

dbBegin(connection)

transaction_ok <- FALSE
snapshot_id <- NA_integer_
seen_appearance_keys <- character()

tryCatch(
  {
    snapshot_id <- insert_snapshot(connection, payload, input_path, source_file_hash)

    for (card in payload$cards) {
      observed_at_for_row <- first_non_missing(payload$captured_at, imported_at)
      article_result <- find_or_create_medium_article(connection, card, schema_info, observed_at_for_row)

      if (!is.null(article_result$warning_message)) {
        summary$warnings <- c(summary$warnings, article_result$warning_message)
      }

      if (identical(article_result$status, "created")) {
        summary$new_articles <- summary$new_articles + 1L
      } else {
        summary$existing_articles <- summary$existing_articles + 1L
      }

      article_row <- fetch_medium_article_by_id(connection, article_result$article_id)
      full_content_result <- article_has_full_content(article_row, schema_info)

      if (!is.null(full_content_result$warning_message)) {
        summary$warnings <- c(summary$warnings, full_content_result$warning_message)
      }

      appearance_key <- appearance_dedupe_key(article_result$article_id, card, payload)

      if (appearance_key %in% seen_appearance_keys) {
        summary$appearances_skipped_duplicate <- summary$appearances_skipped_duplicate + 1L
      } else {
        seen_appearance_keys <- c(seen_appearance_keys, appearance_key)
        rows_inserted <- insert_observation(connection, snapshot_id, article_result$article_id, card, payload, imported_at)

        if (rows_inserted > 0) {
          summary$appearances_inserted <- summary$appearances_inserted + 1L
        } else {
          summary$appearances_skipped_duplicate <- summary$appearances_skipped_duplicate + 1L
        }
      }

      stats_decision <- should_insert_article_observation(
        conn = connection,
        article_id = article_result$article_id,
        claps = card$claps,
        responses = card$responses,
        observed_at = observed_at_for_row
      )

      if (isTRUE(stats_decision$insert)) {
        stats_rows_inserted <- insert_article_stats_observation(
          connection,
          card$article_url,
          card,
          observed_at_for_row,
          stats_decision$reason
        )

        if (stats_rows_inserted > 0) {
          summary$stats_observations_inserted <- summary$stats_observations_inserted + 1L
          summary[[paste0("stats_inserted_", stats_decision$reason)]] <-
            summary[[paste0("stats_inserted_", stats_decision$reason)]] + 1L
        } else {
          summary$stats_skipped_unchanged_too_recent <- summary$stats_skipped_unchanged_too_recent + 1L
        }
      } else if (identical(stats_decision$reason, "unchanged_too_recent")) {
        summary$stats_skipped_unchanged_too_recent <- summary$stats_skipped_unchanged_too_recent + 1L
      }

      if (!is.na(card$thumbnail_url)) {
        summary$thumbnail_urls_saved <- summary$thumbnail_urls_saved + 1L
      }

      queue_result <- insert_or_update_queue_row(
        connection = connection,
        article_id = article_result$article_id,
        normalized_url = card$article_url,
        medium_post_id = card$medium_post_id,
        tag_slug = payload$tag_slug,
        observed_at = observed_at_for_row,
        has_full_content = isTRUE(full_content_result$has_full_content),
        page_variant = payload$page_variant
      )

      if (identical(queue_result, "queued")) {
        summary$queued_for_import <- summary$queued_for_import + 1L
      } else if (identical(queue_result, "already_queued_or_imported")) {
        summary$already_queued_or_imported <- summary$already_queued_or_imported + 1L
      } else if (identical(queue_result, "already_had_full_content")) {
        summary$already_had_full_text <- summary$already_had_full_text + 1L
      }
    }

    dbCommit(connection)
    transaction_ok <- TRUE
  },
  error = function(error) {
    try(dbRollback(connection), silent = TRUE)
    stop("Tag-page import failed: ", conditionMessage(error), call. = FALSE)
  }
)

if (!isTRUE(transaction_ok)) {
  stop("Tag-page import failed before commit.", call. = FALSE)
}

print_new_import_summary(summary, payload, source_file_hash, snapshot_id, database_path)
