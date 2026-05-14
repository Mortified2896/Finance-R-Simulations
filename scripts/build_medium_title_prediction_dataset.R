required_packages <- c("DBI", "RSQLite")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("DBI", "RSQLite"))',
    call. = FALSE
  )
}

library(DBI)
library(RSQLite)

database_path <- file.path("data", "db", "medium_articles.sqlite")
output_dir <- file.path("data", "analysis")
output_path <- file.path(output_dir, "medium_title_prediction_dataset.csv")

message("Medium Title Prediction Dataset Builder")
message("=======================================")

if (!file.exists(database_path)) {
  stop("Could not find database at: ", database_path, call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

table_exists <- function(table_name) {
  dbExistsTable(connection, table_name)
}

table_columns <- function(table_name) {
  if (!table_exists(table_name)) {
    return(character())
  }
  dbGetQuery(connection, paste0("PRAGMA table_info(", dbQuoteIdentifier(connection, table_name), ")"))$name
}

has_column <- function(table_name, column_name) {
  column_name %in% table_columns(table_name)
}

read_table_or_empty <- function(table_name) {
  if (!table_exists(table_name)) {
    return(data.frame())
  }
  dbReadTable(connection, table_name)
}

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

normalize_url_vector <- function(x) {
  y <- clean_text_vector(x)
  y <- ifelse(!is.na(y) & grepl("^//", y), paste0("https:", y), y)
  y <- ifelse(!is.na(y) & grepl("^/", y), paste0("https://medium.com", y), y)
  y <- sub("\\?.*$", "", y)
  y <- sub("#.*$", "", y)
  y <- sub("/+$", "", y)
  y[y == ""] <- NA_character_
  y
}

first_present_column <- function(data, candidates) {
  found <- candidates[candidates %in% names(data)]
  if (length(found) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  value <- clean_text_vector(data[[found[1]]])
  if (length(found) > 1) {
    for (column_name in found[-1]) {
      replacement <- clean_text_vector(data[[column_name]])
      value[is.na(value) & !is.na(replacement)] <- replacement[is.na(value) & !is.na(replacement)]
    }
  }
  value
}

parse_datetime <- function(x) {
  value <- clean_text_vector(x)
  value <- sub("Z$", "", value)
  value <- sub("T", " ", value, fixed = TRUE)

  parsed <- suppressWarnings(as.POSIXct(value, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
  missing <- is.na(parsed)
  if (any(missing)) {
    parsed[missing] <- suppressWarnings(as.POSIXct(value[missing], tz = "UTC", format = "%Y-%m-%d"))
  }
  parsed
}

parse_date <- function(x) {
  parsed_datetime <- parse_datetime(x)
  as.Date(parsed_datetime)
}

collapse_unique <- function(x) {
  value <- sort(unique(clean_text_vector(x)))
  value <- value[!is.na(value)]
  if (length(value) == 0) {
    return(NA_character_)
  }
  paste(value, collapse = "; ")
}

coalesce_value <- function(primary, fallback) {
  primary[is.na(primary) & !is.na(fallback)] <- fallback[is.na(primary) & !is.na(fallback)]
  primary
}

warn_missing_columns <- function(table_name, available_columns, expected_columns) {
  missing_columns <- setdiff(expected_columns, available_columns)
  if (length(missing_columns) > 0) {
    warning(
      table_name,
      " is missing expected column(s): ",
      paste(missing_columns, collapse = ", "),
      "; related output fields will be NA.",
      call. = FALSE
    )
  }
}

latest_non_empty_value <- function(group, value_column, order_column = "observed_datetime") {
  if (!(value_column %in% names(group))) {
    return(NA_character_)
  }

  value <- clean_text_vector(group[[value_column]])
  valid_index <- which(!is.na(value))
  if (length(valid_index) == 0) {
    return(NA_character_)
  }

  if (order_column %in% names(group)) {
    order_value <- group[[order_column]][valid_index]
    ordered_valid_index <- valid_index[order(order_value, decreasing = TRUE, na.last = TRUE)]
    return(value[ordered_valid_index[1]])
  }

  value[valid_index[length(valid_index)]]
}

if (!table_exists("medium_articles")) {
  stop("The required table medium_articles does not exist in ", database_path, call. = FALSE)
}

articles <- read_table_or_empty("medium_articles")
article_columns <- names(articles)
warn_missing_columns("medium_articles", article_columns, c("image_url", "image_url_manual", "image_url_source", "image_url_confidence", "image_url_status"))

if (!("id" %in% article_columns)) {
  articles$id <- seq_len(nrow(articles))
  warning("medium_articles.id is missing; using row numbers as article_id in the output.", call. = FALSE)
}

dataset <- data.frame(
  article_id = articles$id,
  medium_post_id = first_present_column(articles, c("medium_post_id")),
  url = first_present_column(articles, c("url")),
  canonical_url = first_present_column(articles, c("canonical_url")),
  title = first_present_column(articles, c("title")),
  subtitle = first_present_column(articles, c("subtitle")),
  description = first_present_column(articles, c("snippet", "description_html")),
  author = first_present_column(articles, c("author")),
  publication = first_present_column(articles, c("publication")),
  image_url = first_present_column(articles, c("image_url")),
  image_url_manual = first_present_column(articles, c("image_url_manual")),
  image_url_source = first_present_column(articles, c("image_url_source")),
  image_url_confidence = first_present_column(articles, c("image_url_confidence")),
  image_url_status = first_present_column(articles, c("image_url_status")),
  stringsAsFactors = FALSE
)

dataset$url_normalized <- normalize_url_vector(dataset$url)
dataset$canonical_url_normalized <- normalize_url_vector(dataset$canonical_url)

message("Loaded ", nrow(dataset), " rows from medium_articles.")

tag_summary <- data.frame()
if (table_exists("medium_tag_page_observations")) {
  tag_observations <- read_table_or_empty("medium_tag_page_observations")
  tag_columns <- names(tag_observations)

  if (nrow(tag_observations) > 0) {
    if (!("article_id" %in% tag_columns)) {
      tag_observations$article_id <- NA_integer_
    }
    if (!("article_url_normalized" %in% tag_columns)) {
      tag_observations$article_url_normalized <- NA_character_
    }
    if (!("observed_at" %in% tag_columns)) {
      tag_observations$observed_at <- NA_character_
    }
    if (!("page_position" %in% tag_columns)) {
      tag_observations$page_position <- NA_real_
    }
    if (!("tag_slug" %in% tag_columns)) {
      tag_observations$tag_slug <- NA_character_
    }
    if (!("thumbnail_url" %in% tag_columns)) {
      tag_observations$thumbnail_url <- NA_character_
      warning("medium_tag_page_observations.thumbnail_url is missing; latest_tag_thumbnail_url will be NA.", call. = FALSE)
    }
    if (!("thumbnail_alt" %in% tag_columns)) {
      tag_observations$thumbnail_alt <- NA_character_
      warning("medium_tag_page_observations.thumbnail_alt is missing; latest_tag_thumbnail_alt will be NA.", call. = FALSE)
    }
    if (!("thumbnail_source" %in% tag_columns)) {
      tag_observations$thumbnail_source <- NA_character_
    }
    if (!("thumbnail_confidence" %in% tag_columns)) {
      tag_observations$thumbnail_confidence <- NA_character_
    }
    if (!("thumbnail_status" %in% tag_columns)) {
      tag_observations$thumbnail_status <- NA_character_
    }

    if (table_exists("medium_tag_page_snapshots") && "snapshot_id" %in% tag_columns) {
      snapshots <- read_table_or_empty("medium_tag_page_snapshots")
      if (all(c("id", "page_variant") %in% names(snapshots))) {
        tag_observations <- merge(
          tag_observations,
          snapshots[, c("id", "page_variant")],
          by.x = "snapshot_id",
          by.y = "id",
          all.x = TRUE
        )
      }
    }
    if (!("page_variant" %in% names(tag_observations))) {
      tag_observations$page_variant <- NA_character_
    }

    tag_observations$observed_datetime <- parse_datetime(tag_observations$observed_at)
    tag_observations$page_position_numeric <- suppressWarnings(as.numeric(tag_observations$page_position))

    split_key <- if (sum(!is.na(tag_observations$article_id)) > 0) {
      as.character(tag_observations$article_id)
    } else {
      normalize_url_vector(tag_observations$article_url_normalized)
    }

    tag_groups <- split(tag_observations, split_key, drop = TRUE)
    tag_summary <- do.call(rbind, lapply(names(tag_groups), function(key) {
      group <- tag_groups[[key]]
      latest_thumbnail_url <- latest_non_empty_value(group, "thumbnail_url")
      latest_thumbnail_alt <- latest_non_empty_value(group, "thumbnail_alt")
      latest_thumbnail_source <- latest_non_empty_value(group, "thumbnail_source")
      latest_thumbnail_confidence <- latest_non_empty_value(group, "thumbnail_confidence")
      latest_thumbnail_status <- latest_non_empty_value(group, "thumbnail_status")
      data.frame(
        article_id = suppressWarnings(as.integer(key)),
        article_url_normalized = if ("article_url_normalized" %in% names(group)) collapse_unique(group$article_url_normalized) else NA_character_,
        first_observed_at = if (all(is.na(group$observed_datetime))) NA_character_ else format(min(group$observed_datetime, na.rm = TRUE), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        latest_observed_at = if (all(is.na(group$observed_datetime))) NA_character_ else format(max(group$observed_datetime, na.rm = TRUE), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        times_seen = nrow(group),
        best_rank = if (all(is.na(group$page_position_numeric))) NA_real_ else min(group$page_position_numeric, na.rm = TRUE),
        average_rank = if (all(is.na(group$page_position_numeric))) NA_real_ else mean(group$page_position_numeric, na.rm = TRUE),
        observed_tag_slugs = collapse_unique(group$tag_slug),
        observed_page_variants = collapse_unique(group$page_variant),
        tag_observation_claps_max = if ("claps" %in% names(group) && !all(is.na(group$claps))) max(suppressWarnings(as.numeric(group$claps)), na.rm = TRUE) else NA_real_,
        tag_observation_responses_max = if ("responses" %in% names(group) && !all(is.na(group$responses))) max(suppressWarnings(as.numeric(group$responses)), na.rm = TRUE) else NA_real_,
        latest_tag_thumbnail_url = latest_thumbnail_url,
        latest_tag_thumbnail_alt = latest_thumbnail_alt,
        latest_tag_thumbnail_source = latest_thumbnail_source,
        latest_tag_thumbnail_confidence = latest_thumbnail_confidence,
        latest_tag_thumbnail_status = latest_thumbnail_status,
        stringsAsFactors = FALSE
      )
    }))

    message("Summarized ", nrow(tag_summary), " articles from medium_tag_page_observations.")
  }
} else {
  warning("medium_tag_page_observations table is missing; observation features will be NA.", call. = FALSE)
}

if (nrow(tag_summary) > 0) {
  dataset <- merge(dataset, tag_summary, by = "article_id", all.x = TRUE, sort = FALSE)
} else {
  dataset$first_observed_at <- NA_character_
  dataset$latest_observed_at <- NA_character_
  dataset$times_seen <- NA_integer_
  dataset$best_rank <- NA_real_
  dataset$average_rank <- NA_real_
  dataset$observed_tag_slugs <- NA_character_
  dataset$observed_page_variants <- NA_character_
  dataset$tag_observation_claps_max <- NA_real_
  dataset$tag_observation_responses_max <- NA_real_
  dataset$latest_tag_thumbnail_url <- NA_character_
  dataset$latest_tag_thumbnail_alt <- NA_character_
  dataset$latest_tag_thumbnail_source <- NA_character_
  dataset$latest_tag_thumbnail_confidence <- NA_character_
  dataset$latest_tag_thumbnail_status <- NA_character_
}

if (table_exists("medium_article_import_queue")) {
  queue <- read_table_or_empty("medium_article_import_queue")
  if (nrow(queue) > 0 && "article_id" %in% names(queue)) {
    queue$first_seen_datetime <- if ("first_seen_at" %in% names(queue)) parse_datetime(queue$first_seen_at) else as.POSIXct(NA)
    queue$last_seen_datetime <- if ("last_seen_at" %in% names(queue)) parse_datetime(queue$last_seen_at) else as.POSIXct(NA)

    queue_groups <- split(queue, as.character(queue$article_id), drop = TRUE)
    queue_summary <- do.call(rbind, lapply(names(queue_groups), function(key) {
      group <- queue_groups[[key]]
      data.frame(
        article_id = suppressWarnings(as.integer(key)),
        queue_first_seen_at = if (all(is.na(group$first_seen_datetime))) NA_character_ else format(min(group$first_seen_datetime, na.rm = TRUE), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        queue_last_seen_at = if (all(is.na(group$last_seen_datetime))) NA_character_ else format(max(group$last_seen_datetime, na.rm = TRUE), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        queue_tag_slugs = if ("tag_slug" %in% names(group)) collapse_unique(group$tag_slug) else NA_character_,
        stringsAsFactors = FALSE
      )
    }))

    dataset <- merge(dataset, queue_summary, by = "article_id", all.x = TRUE, sort = FALSE)
    dataset$first_observed_at <- coalesce_value(dataset$first_observed_at, dataset$queue_first_seen_at)
    dataset$latest_observed_at <- coalesce_value(dataset$latest_observed_at, dataset$queue_last_seen_at)
    dataset$observed_tag_slugs <- coalesce_value(dataset$observed_tag_slugs, dataset$queue_tag_slugs)
  }
}

stats_summary <- data.frame()
if (table_exists("medium_article_public_stats")) {
  public_stats <- read_table_or_empty("medium_article_public_stats")

  if (nrow(public_stats) > 0) {
    if (!("article_url" %in% names(public_stats))) {
      public_stats$article_url <- NA_character_
    }
    if (!("observed_at" %in% names(public_stats))) {
      public_stats$observed_at <- NA_character_
    }
    if (!("claps_count" %in% names(public_stats))) {
      public_stats$claps_count <- NA_real_
    }
    if (!("responses_count" %in% names(public_stats))) {
      public_stats$responses_count <- NA_real_
    }
    if (!("parse_status" %in% names(public_stats))) {
      public_stats$parse_status <- "ok"
    }

    public_stats <- public_stats[is.na(public_stats$parse_status) | public_stats$parse_status == "ok", , drop = FALSE]
    public_stats$article_url_normalized <- normalize_url_vector(public_stats$article_url)
    public_stats$observed_datetime <- parse_datetime(public_stats$observed_at)
    public_stats$claps_count <- suppressWarnings(as.numeric(public_stats$claps_count))
    public_stats$responses_count <- suppressWarnings(as.numeric(public_stats$responses_count))

    stats_groups <- split(public_stats, public_stats$article_url_normalized, drop = TRUE)
    stats_summary <- do.call(rbind, lapply(names(stats_groups), function(key) {
      group <- stats_groups[[key]]
      order_index <- order(group$observed_datetime, decreasing = TRUE, na.last = TRUE)
      latest <- group[order_index[1], , drop = FALSE]
      data.frame(
        stats_url_normalized = key,
        latest_claps = latest$claps_count[1],
        latest_responses = latest$responses_count[1],
        latest_stats_observed_at = if (is.na(latest$observed_datetime[1])) NA_character_ else format(latest$observed_datetime[1], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        max_claps = if (all(is.na(group$claps_count))) NA_real_ else max(group$claps_count, na.rm = TRUE),
        max_responses = if (all(is.na(group$responses_count))) NA_real_ else max(group$responses_count, na.rm = TRUE),
        public_stats_observation_count = nrow(group),
        stringsAsFactors = FALSE
      )
    }))

    message("Summarized public stats for ", nrow(stats_summary), " article URLs.")
  }
} else {
  warning("medium_article_public_stats table is missing; public performance fields will be NA.", call. = FALSE)
}

if (nrow(stats_summary) > 0) {
  dataset <- merge(dataset, stats_summary, by.x = "url_normalized", by.y = "stats_url_normalized", all.x = TRUE, sort = FALSE)

  missing_stats <- is.na(dataset$latest_claps) & !is.na(dataset$canonical_url_normalized)
  if (any(missing_stats)) {
    canonical_matches <- merge(
      dataset[missing_stats, c("article_id", "canonical_url_normalized")],
      stats_summary,
      by.x = "canonical_url_normalized",
      by.y = "stats_url_normalized",
      all.x = TRUE,
      sort = FALSE
    )
    match_index <- match(canonical_matches$article_id, dataset$article_id)
    for (column_name in setdiff(names(stats_summary), "stats_url_normalized")) {
      replacement <- canonical_matches[[column_name]]
      fill <- is.na(dataset[[column_name]][match_index]) & !is.na(replacement)
      dataset[[column_name]][match_index[fill]] <- replacement[fill]
    }
  }
} else {
  dataset$latest_claps <- NA_real_
  dataset$latest_responses <- NA_real_
  dataset$latest_stats_observed_at <- NA_character_
  dataset$max_claps <- NA_real_
  dataset$max_responses <- NA_real_
  dataset$public_stats_observation_count <- NA_integer_
}

dataset$latest_claps <- suppressWarnings(as.numeric(dataset$latest_claps))
dataset$latest_responses <- suppressWarnings(as.numeric(dataset$latest_responses))
dataset$max_claps <- suppressWarnings(as.numeric(dataset$max_claps))
dataset$max_responses <- suppressWarnings(as.numeric(dataset$max_responses))

valid_success <- !is.na(dataset$latest_claps) | !is.na(dataset$latest_responses)
dataset$success_score <- NA_real_
dataset$success_score[valid_success] <-
  log1p(ifelse(is.na(dataset$latest_claps[valid_success]), 0, dataset$latest_claps[valid_success])) +
  2 * log1p(ifelse(is.na(dataset$latest_responses[valid_success]), 0, dataset$latest_responses[valid_success]))

dataset$high_performer_top20 <- NA
dataset$high_performer_top10 <- NA
if (sum(!is.na(dataset$success_score)) > 0) {
  cutoff20 <- as.numeric(stats::quantile(dataset$success_score, probs = 0.80, na.rm = TRUE, type = 7))
  cutoff10 <- as.numeric(stats::quantile(dataset$success_score, probs = 0.90, na.rm = TRUE, type = 7))
  dataset$high_performer_top20[!is.na(dataset$success_score)] <- dataset$success_score[!is.na(dataset$success_score)] >= cutoff20
  dataset$high_performer_top10[!is.na(dataset$success_score)] <- dataset$success_score[!is.na(dataset$success_score)] >= cutoff10
}

article_published_manual <- first_present_column(articles, c("published_date_manual"))
article_published_from_feed <- first_present_column(articles, c("published_at"))
article_published <- article_published_manual
article_published[is.na(article_published)] <- article_published_from_feed[is.na(article_published)]
published_date <- parse_date(article_published)
dataset$published_date <- published_date[match(dataset$article_id, articles$id)]
matched_article_index <- match(dataset$article_id, articles$id)
dataset$published_date_source <- ifelse(
  !is.na(dataset$published_date) & !is.na(article_published_manual[matched_article_index]),
  "medium_articles.published_date_manual",
  ifelse(!is.na(dataset$published_date), "medium_articles.published_at", NA_character_)
)

rows_with_published_date <- sum(!is.na(dataset$published_date))
if (rows_with_published_date == 0) {
  warning("No reliable published dates found; article age variables will be NA.", call. = FALSE)
} else if (rows_with_published_date < nrow(dataset)) {
  warning(
    "Reliable published dates found for ",
    rows_with_published_date,
    " of ",
    nrow(dataset),
    " articles; article age variables are NA for the rest.",
    call. = FALSE
  )
}

latest_observation_date <- parse_date(dataset$latest_observed_at)
latest_stats_date <- parse_date(dataset$latest_stats_observed_at)

dataset$article_age_days_at_latest_observation <- as.numeric(latest_observation_date - dataset$published_date)
dataset$article_age_days_at_latest_stats <- as.numeric(latest_stats_date - dataset$published_date)
dataset$article_age_days_at_latest_observation[dataset$article_age_days_at_latest_observation < 0] <- NA_real_
dataset$article_age_days_at_latest_stats[dataset$article_age_days_at_latest_stats < 0] <- NA_real_

dataset$image_url <- clean_text_vector(dataset$image_url)
dataset$image_url_manual <- clean_text_vector(dataset$image_url_manual)
dataset$image_url_source <- clean_text_vector(dataset$image_url_source)
dataset$image_url_confidence <- clean_text_vector(dataset$image_url_confidence)
dataset$image_url_status <- clean_text_vector(dataset$image_url_status)
dataset$latest_tag_thumbnail_url <- clean_text_vector(dataset$latest_tag_thumbnail_url)
dataset$latest_tag_thumbnail_alt <- clean_text_vector(dataset$latest_tag_thumbnail_alt)
dataset$latest_tag_thumbnail_source <- clean_text_vector(dataset$latest_tag_thumbnail_source)
dataset$latest_tag_thumbnail_confidence <- clean_text_vector(dataset$latest_tag_thumbnail_confidence)
dataset$latest_tag_thumbnail_status <- clean_text_vector(dataset$latest_tag_thumbnail_status)

dataset$primary_image_url_for_download <- dataset$image_url_manual
dataset$primary_image_url_source <- ifelse(!is.na(dataset$primary_image_url_for_download), "image_url_manual", "missing")
dataset$primary_image_url_provenance <- ifelse(!is.na(dataset$primary_image_url_for_download), dataset$image_url_source, NA_character_)
dataset$primary_image_url_confidence <- ifelse(!is.na(dataset$primary_image_url_for_download), dataset$image_url_confidence, NA_character_)
dataset$primary_image_url_status <- ifelse(!is.na(dataset$primary_image_url_for_download), dataset$image_url_status, "missing")

use_image_url <- is.na(dataset$primary_image_url_for_download) & !is.na(dataset$image_url)
dataset$primary_image_url_for_download[use_image_url] <- dataset$image_url[use_image_url]
dataset$primary_image_url_source[use_image_url] <- "image_url"
dataset$primary_image_url_provenance[use_image_url] <- dataset$image_url_source[use_image_url]
dataset$primary_image_url_confidence[use_image_url] <- dataset$image_url_confidence[use_image_url]
dataset$primary_image_url_status[use_image_url] <- dataset$image_url_status[use_image_url]

use_tag_thumbnail <- is.na(dataset$primary_image_url_for_download) & !is.na(dataset$latest_tag_thumbnail_url)
dataset$primary_image_url_for_download[use_tag_thumbnail] <- dataset$latest_tag_thumbnail_url[use_tag_thumbnail]
dataset$primary_image_url_source[use_tag_thumbnail] <- "tag_thumbnail_url"
dataset$primary_image_url_provenance[use_tag_thumbnail] <- dataset$latest_tag_thumbnail_source[use_tag_thumbnail]
dataset$primary_image_url_confidence[use_tag_thumbnail] <- dataset$latest_tag_thumbnail_confidence[use_tag_thumbnail]
dataset$primary_image_url_status[use_tag_thumbnail] <- dataset$latest_tag_thumbnail_status[use_tag_thumbnail]

dataset$primary_image_url_provenance[is.na(dataset$primary_image_url_provenance) & !is.na(dataset$primary_image_url_for_download)] <- "unknown"
dataset$primary_image_url_confidence[is.na(dataset$primary_image_url_confidence) & !is.na(dataset$primary_image_url_for_download)] <- "unknown"
dataset$primary_image_url_status[is.na(dataset$primary_image_url_status) & !is.na(dataset$primary_image_url_for_download)] <- "found_unknown"

summary_lines <- c(
  "Data quality summary",
  "====================",
  paste("Total articles:", nrow(dataset)),
  paste("Rows with title:", sum(!is.na(clean_text_vector(dataset$title)))),
  paste("Rows with claps:", sum(!is.na(dataset$latest_claps))),
  paste("Rows with responses:", sum(!is.na(dataset$latest_responses))),
  paste("Rows with usable success_score:", sum(!is.na(dataset$success_score))),
  paste("Rows with publication:", sum(!is.na(clean_text_vector(dataset$publication)))),
  paste(
    "Observation date range:",
    if (all(is.na(latest_observation_date))) {
      "(none)"
    } else {
      paste(min(latest_observation_date, na.rm = TRUE), "to", max(latest_observation_date, na.rm = TRUE))
    }
  )
)

message("\n", paste(summary_lines, collapse = "\n"))

output_columns <- c(
  "article_id",
  "medium_post_id",
  "url",
  "canonical_url",
  "title",
  "subtitle",
  "description",
  "author",
  "publication",
  "image_url",
  "image_url_manual",
  "image_url_source",
  "image_url_confidence",
  "image_url_status",
  "latest_tag_thumbnail_url",
  "latest_tag_thumbnail_alt",
  "latest_tag_thumbnail_source",
  "latest_tag_thumbnail_confidence",
  "latest_tag_thumbnail_status",
  "primary_image_url_for_download",
  "primary_image_url_source",
  "primary_image_url_provenance",
  "primary_image_url_confidence",
  "primary_image_url_status",
  "first_observed_at",
  "latest_observed_at",
  "times_seen",
  "best_rank",
  "average_rank",
  "observed_tag_slugs",
  "observed_page_variants",
  "latest_claps",
  "latest_responses",
  "latest_stats_observed_at",
  "max_claps",
  "max_responses",
  "public_stats_observation_count",
  "success_score",
  "high_performer_top20",
  "high_performer_top10",
  "published_date",
  "published_date_source",
  "article_age_days_at_latest_observation",
  "article_age_days_at_latest_stats",
  "tag_observation_claps_max",
  "tag_observation_responses_max"
)

missing_output_columns <- setdiff(output_columns, names(dataset))
for (column_name in missing_output_columns) {
  dataset[[column_name]] <- NA
}

dataset <- dataset[order(dataset$article_id), output_columns]
write.csv(dataset, output_path, row.names = FALSE, na = "")

message("\nSaved dataset to: ", output_path)
