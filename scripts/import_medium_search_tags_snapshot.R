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
TAG_DISCOVERY_COOLDOWN_DAYS <- 180
SIDEBAR_POST_MIN_GAP_HOURS <- 12
PEOPLE_COOLDOWN_DAYS <- 180

first_non_missing <- function(primary_value, fallback_value) {
  if (!is_missing_text(primary_value)) {
    return(clean_text(primary_value))
  }

  clean_text(fallback_value)
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

cooldown_start <- function(observed_at, days = 180) {
  observed_time <- parse_observed_at(observed_at)

  if (is.na(observed_time)) {
    observed_time <- Sys.time()
  }

  format(observed_time - as.difftime(days, units = "days"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

gap_start <- function(observed_at, hours = 12) {
  observed_time <- parse_observed_at(observed_at)

  if (is.na(observed_time)) {
    observed_time <- Sys.time()
  }

  format(observed_time - as.difftime(hours, units = "hours"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

normalize_search_term <- function(value) {
  value <- clean_text(value)

  if (is.na(value)) {
    return(NA_character_)
  }

  tolower(value)
}

normalize_tracking_context <- function(value) {
  value <- clean_text(value)

  if (is.na(value)) {
    return("unknown_context")
  }

  value
}

normalize_source_url <- function(x) {
  value <- clean_text(x)

  if (is.na(value)) {
    return(NA_character_)
  }

  if (grepl("^//", value)) {
    value <- paste0("https:", value)
  } else if (grepl("^/", value)) {
    value <- paste0("https://medium.com", value)
  }

  sub("#.*$", "", value)
}

clean_tag <- function(tag, fallback_rank = NA_integer_) {
  tag_slug <- clean_text(tag$tag_slug)

  if (is.na(tag_slug)) {
    return(NULL)
  }

  list(
    search_term = normalize_search_term(tag$search_term),
    tag_slug = tolower(tag_slug),
    display_title = clean_text(tag$display_title),
    result_rank = if (!is.na(integer_from_json(tag$result_rank))) integer_from_json(tag$result_rank) else fallback_rank,
    tag_url = normalize_medium_url(scalar_from_json(tag$tag_url)),
    source_url = normalize_source_url(scalar_from_json(tag$source_url))
  )
}

clean_sidebar_post <- function(post, fallback_rank = NA_integer_) {
  normalized_url <- normalize_medium_url(scalar_from_json(post$article_url))
  post_id <- clean_text(post$medium_post_id)

  if (is.na(post_id)) {
    post_id <- extract_medium_post_id(normalized_url)
  } else {
    post_id <- tolower(post_id)
  }

  if (is.na(normalized_url) && is.na(post_id)) {
    return(NULL)
  }

  list(
    search_term = normalize_search_term(post$search_term),
    source_surface = first_non_missing(post$source_surface, "search_sidebar_posts"),
    result_rank = if (!is.na(integer_from_json(post$result_rank))) integer_from_json(post$result_rank) else fallback_rank,
    article_url = normalized_url,
    medium_post_id = post_id,
    title = clean_text(post$title),
    author_name = clean_text(post$author_name),
    author_url = normalize_medium_url(scalar_from_json(post$author_url)),
    author_username = clean_text(post$author_username),
    published_at = clean_text(post$published_at),
    updated_at = clean_text(post$updated_at),
    is_member_only = logical_flag_from_json(post$is_member_only)
  )
}

clean_person <- function(person, fallback_rank = NA_integer_) {
  profile_url <- normalize_medium_url(scalar_from_json(person$profile_url))
  username <- clean_text(person$username)

  if (is.na(profile_url) && is.na(username)) {
    return(NULL)
  }

  list(
    search_term = normalize_search_term(person$search_term),
    source_surface = first_non_missing(person$source_surface, "search_sidebar_people"),
    result_rank = if (!is.na(integer_from_json(person$result_rank))) integer_from_json(person$result_rank) else fallback_rank,
    profile_url = profile_url,
    username = username,
    display_name = clean_text(person$display_name),
    bio_snippet = clean_text(person$bio_snippet)
  )
}

clean_list <- function(items, cleaner) {
  if (is.null(items) || length(items) == 0) {
    return(list())
  }

  cleaned <- list()
  for (index in seq_along(items)) {
    row <- cleaner(items[[index]], index)
    if (!is.null(row)) {
      cleaned[[length(cleaned) + 1]] <- row
    }
  }

  cleaned
}

read_payload <- function(input_path) {
  parsed <- fromJSON(input_path, simplifyVector = FALSE)
  source_type <- clean_text(parsed$source_type)

  if (!identical(source_type, "medium_search_tags_page")) {
    stop("The JSON file does not contain source_type = 'medium_search_tags_page'.", call. = FALSE)
  }

  search_term <- normalize_search_term(parsed$search_term)

  list(
    source_type = source_type,
    schema_version = integer_from_json(parsed$schema_version),
    page_type = clean_text(parsed$page_type),
    search_term = search_term,
    source_url = normalize_source_url(scalar_from_json(parsed$source_url)),
    captured_at = clean_text(parsed$captured_at),
    page_title = clean_text(parsed$page_title),
    tracking_context = normalize_tracking_context(parsed$tracking_context),
    tags = clean_list(parsed$tags, clean_tag),
    sidebar_posts = clean_list(parsed$sidebar_posts, clean_sidebar_post),
    sidebar_people = clean_list(parsed$sidebar_people, clean_person)
  )
}

upsert_candidate_tag <- function(connection, tag, observed_at) {
  existing <- dbGetQuery(
    connection,
    "SELECT id FROM medium_candidate_tags WHERE tag_slug = ? LIMIT 1",
    params = list(tag$tag_slug)
  )

  if (nrow(existing) == 0) {
    dbExecute(
      connection,
      "
        INSERT INTO medium_candidate_tags (
          tag_slug, display_title, tag_url, first_seen_at, last_seen_at,
          first_seen_search_term, last_seen_search_term
        ) VALUES (?, ?, ?, ?, ?, ?, ?)
      ",
      params = list(
        tag$tag_slug,
        tag$display_title,
        tag$tag_url,
        observed_at,
        observed_at,
        tag$search_term,
        tag$search_term
      )
    )
    return("created")
  }

  dbExecute(
    connection,
    "
      UPDATE medium_candidate_tags
      SET
        display_title = COALESCE(NULLIF(?, ''), display_title),
        tag_url = COALESCE(NULLIF(?, ''), tag_url),
        last_seen_at = ?,
        last_seen_search_term = ?
      WHERE tag_slug = ?
    ",
    params = list(tag$display_title, tag$tag_url, observed_at, tag$search_term, tag$tag_slug)
  )

  "updated"
}

recent_tag_discovery_exists <- function(connection, tag, tracking_context, observed_at) {
  recent_since <- cooldown_start(observed_at, TAG_DISCOVERY_COOLDOWN_DAYS)
  dbGetQuery(
    connection,
    "
      SELECT COUNT(*) AS n
      FROM medium_tag_discovery_observations
      WHERE search_term = ?
        AND tag_slug = ?
        AND COALESCE(tracking_context, '') = COALESCE(?, '')
        AND observed_at >= ?
    ",
    params = list(tag$search_term, tag$tag_slug, tracking_context, recent_since)
  )$n[1] > 0
}

insert_tag_discovery <- function(connection, tag, payload, observed_at, source_file) {
  dbExecute(
    connection,
    "
      INSERT INTO medium_tag_discovery_observations (
        observed_at, search_term, tag_slug, display_title, result_rank,
        tracking_context, source_url, source_file
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      observed_at,
      tag$search_term,
      tag$tag_slug,
      tag$display_title,
      tag$result_rank,
      payload$tracking_context,
      first_non_missing(tag$source_url, payload$source_url),
      source_file
    )
  )
}

article_card_from_sidebar_post <- function(post) {
  list(
    article_url = post$article_url,
    medium_post_id = post$medium_post_id,
    title = post$title,
    subtitle = NA_character_,
    author_name = post$author_name,
    author_url = post$author_url,
    author_username = post$author_username,
    author_medium_user_id = NA_character_,
    publication_name = NA_character_,
    publication_url = NA_character_,
    publication_id = NA_character_,
    publication_slug = NA_character_,
    publication_domain = NA_character_,
    publication_subscriber_count = NA_integer_,
    publication_status = NA_character_,
    published_at = post$published_at,
    published_date_inferred = NA_character_,
    published_at_inferred = NA_character_,
    updated_at = post$updated_at,
    read_time_minutes = NA_real_,
    article_tags_json = NA_character_,
    thumbnail_url = NA_character_,
    is_member_only = post$is_member_only
  )
}

recent_sidebar_post_exists <- function(connection, post, tracking_context, observed_at) {
  recent_since <- gap_start(observed_at, SIDEBAR_POST_MIN_GAP_HOURS)
  dbGetQuery(
    connection,
    "
      SELECT COUNT(*) AS n
      FROM medium_search_sidebar_post_observations
      WHERE search_term = ?
        AND source_surface = ?
        AND COALESCE(medium_post_id, '') = COALESCE(?, '')
        AND COALESCE(article_url_normalized, '') = COALESCE(?, '')
        AND COALESCE(tracking_context, '') = COALESCE(?, '')
        AND observed_at >= ?
    ",
    params = list(
      post$search_term,
      post$source_surface,
      post$medium_post_id,
      post$article_url,
      tracking_context,
      recent_since
    )
  )$n[1] > 0
}

insert_sidebar_post_observation <- function(connection, post, article_id, payload, observed_at, source_file) {
  dbExecute(
    connection,
    "
      INSERT INTO medium_search_sidebar_post_observations (
        observed_at, search_term, source_surface, article_id, article_url_normalized,
        medium_post_id, title, author_name, author_url, author_username, result_rank,
        tracking_context, source_url, source_file
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      observed_at,
      post$search_term,
      post$source_surface,
      article_id,
      post$article_url,
      post$medium_post_id,
      post$title,
      post$author_name,
      post$author_url,
      post$author_username,
      post$result_rank,
      payload$tracking_context,
      payload$source_url,
      source_file
    )
  )
}

recent_person_exists <- function(connection, person, tracking_context, observed_at) {
  recent_since <- cooldown_start(observed_at, PEOPLE_COOLDOWN_DAYS)
  dbGetQuery(
    connection,
    "
      SELECT COUNT(*) AS n
      FROM medium_search_people_observations
      WHERE search_term = ?
        AND COALESCE(username, '') = COALESCE(?, '')
        AND COALESCE(profile_url, '') = COALESCE(?, '')
        AND COALESCE(tracking_context, '') = COALESCE(?, '')
        AND observed_at >= ?
    ",
    params = list(person$search_term, person$username, person$profile_url, tracking_context, recent_since)
  )$n[1] > 0
}

insert_person_observation <- function(connection, person, payload, observed_at, source_file) {
  dbExecute(
    connection,
    "
      INSERT INTO medium_search_people_observations (
        observed_at, search_term, source_surface, profile_url, username,
        display_name, bio_snippet, result_rank, tracking_context, source_url, source_file
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      observed_at,
      person$search_term,
      person$source_surface,
      person$profile_url,
      person$username,
      person$display_name,
      person$bio_snippet,
      person$result_rank,
      payload$tracking_context,
      payload$source_url,
      source_file
    )
  )
}

print_summary <- function(summary, payload, database_path) {
  message("Medium search-tags page detected")
  message("--------------------------------")
  message("Search term: ", payload$search_term)
  message("Tracking context: ", if (is.na(payload$tracking_context)) "" else payload$tracking_context)
  message("")
  message("Tag discovery:")
  message("Tags found: ", summary$tags_found)
  message("New tag observations saved: ", summary$tag_observations_saved)
  message("Skipped by 180-day cooldown: ", summary$tag_observations_skipped_cooldown)
  message("")
  message("Top tags:")
  for (tag in head(payload$tags, 5)) {
    message(tag$result_rank, ". ", tag$display_title, " -> ", tag$tag_slug)
  }
  message("")
  message("Sidebar posts:")
  message("Posts found: ", summary$posts_found)
  message("New article observations saved: ", summary$post_observations_saved)
  message("Skipped by duplicate window: ", summary$post_observations_skipped_duplicate)
  message("")
  message("Sidebar people:")
  message("People found: ", summary$people_found)
  message("New people observations saved: ", summary$people_observations_saved)
  message("Skipped by 180-day cooldown: ", summary$people_observations_skipped_cooldown)
  message("")
  message("DB: ", database_path)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop(
    "Please provide exactly one Medium search-tags JSON file path.\n\n",
    "Example:\n",
    "Rscript scripts/import_medium_search_tags_snapshot.R data/medium_tag_watcher_snapshots/medium_search_tags_watch_finance.json",
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
ensure_medium_search_tags_schema(connection)

payload <- read_payload(input_path)
imported_at <- current_timestamp()
observed_at <- first_non_missing(payload$captured_at, imported_at)
source_file <- normalizePath(input_path, winslash = "/", mustWork = FALSE)

summary <- list(
  tags_found = length(payload$tags),
  tag_observations_saved = 0L,
  tag_observations_skipped_cooldown = 0L,
  candidate_tags_created = 0L,
  candidate_tags_updated = 0L,
  posts_found = length(payload$sidebar_posts),
  post_observations_saved = 0L,
  post_observations_skipped_duplicate = 0L,
  people_found = length(payload$sidebar_people),
  people_observations_saved = 0L,
  people_observations_skipped_cooldown = 0L
)

dbBegin(connection)
transaction_ok <- FALSE

tryCatch(
  {
    for (tag in payload$tags) {
      tag$search_term <- first_non_missing(tag$search_term, payload$search_term)
      candidate_status <- upsert_candidate_tag(connection, tag, observed_at)
      if (identical(candidate_status, "created")) {
        summary$candidate_tags_created <- summary$candidate_tags_created + 1L
      } else {
        summary$candidate_tags_updated <- summary$candidate_tags_updated + 1L
      }

      if (recent_tag_discovery_exists(connection, tag, payload$tracking_context, observed_at)) {
        summary$tag_observations_skipped_cooldown <- summary$tag_observations_skipped_cooldown + 1L
      } else {
        rows_inserted <- insert_tag_discovery(connection, tag, payload, observed_at, source_file)
        summary$tag_observations_saved <- summary$tag_observations_saved + rows_inserted
      }
    }

    for (post in payload$sidebar_posts) {
      post$search_term <- first_non_missing(post$search_term, payload$search_term)

      if (recent_sidebar_post_exists(connection, post, payload$tracking_context, observed_at)) {
        summary$post_observations_skipped_duplicate <- summary$post_observations_skipped_duplicate + 1L
        next
      }

      card <- article_card_from_sidebar_post(post)
      article_result <- find_or_create_medium_article(connection, card, schema_info, observed_at)
      rows_inserted <- insert_sidebar_post_observation(connection, post, article_result$article_id, payload, observed_at, source_file)
      summary$post_observations_saved <- summary$post_observations_saved + rows_inserted
    }

    for (person in payload$sidebar_people) {
      person$search_term <- first_non_missing(person$search_term, payload$search_term)

      if (recent_person_exists(connection, person, payload$tracking_context, observed_at)) {
        summary$people_observations_skipped_cooldown <- summary$people_observations_skipped_cooldown + 1L
      } else {
        rows_inserted <- insert_person_observation(connection, person, payload, observed_at, source_file)
        summary$people_observations_saved <- summary$people_observations_saved + rows_inserted
      }
    }

    dbCommit(connection)
    transaction_ok <- TRUE
  },
  error = function(error) {
    try(dbRollback(connection), silent = TRUE)
    stop("Search-tags import failed: ", conditionMessage(error), call. = FALSE)
  }
)

if (!isTRUE(transaction_ok)) {
  stop("Search-tags import failed before commit.", call. = FALSE)
}

print_summary(summary, payload, database_path)
