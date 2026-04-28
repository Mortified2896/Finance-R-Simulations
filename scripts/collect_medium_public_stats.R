required_packages <- c("xml2", "rvest", "DBI", "RSQLite", "curl")

# Stop early with a clear message if a package is missing.
# This script does not install packages automatically.
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("xml2", "rvest", "DBI", "RSQLite", "curl"))',
    call. = FALSE
  )
}

library(xml2)
library(rvest)
library(DBI)
library(RSQLite)
library(curl)

database_path <- file.path("data", "medium_articles.sqlite")
max_articles <- 10
delay_seconds <- 3

clean_text <- function(x) {
  if (length(x) == 0) {
    return(NA_character_)
  }

  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x[is.na(x) | x == ""] <- NA_character_

  x
}

parse_compact_number <- function(x) {
  x <- clean_text(x)

  if (is.na(x)) {
    return(NA_integer_)
  }

  x <- gsub(",", "", x)
  x <- gsub("\\s+", "", x)
  x <- toupper(x)

  number_part <- suppressWarnings(as.numeric(sub("^([0-9]+\\.?[0-9]*).*$", "\\1", x)))

  if (is.na(number_part)) {
    return(NA_integer_)
  }

  multiplier <- 1

  if (grepl("K$", x)) {
    multiplier <- 1000
  } else if (grepl("M$", x)) {
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
      UNIQUE(article_url, observed_date)
    )
  "))
}

select_articles_for_batch <- function(connection, max_articles, observed_date) {
  dbGetQuery(
    connection,
    "
      SELECT title, url
      FROM medium_articles
      WHERE url NOT IN (
        SELECT article_url
        FROM medium_article_public_stats
        WHERE observed_date = ?
      )
      ORDER BY COALESCE(published_at, updated_at, fetched_at) DESC, id DESC
      LIMIT ?
    ",
    params = list(observed_date, max_articles)
  )
}

attribute_values <- function(html_doc, css, attribute_name) {
  values <- html_attr(html_elements(html_doc, css), attribute_name)
  values <- clean_text(values)
  values[!is.na(values)]
}

page_public_text_candidates <- function(html_doc) {
  page_text <- clean_text(html_text2(html_doc))

  candidates <- c(
    page_text,
    html_text2(html_elements(html_doc, "button, a")),
    attribute_values(html_doc, "[aria-label]", "aria-label"),
    attribute_values(html_doc, "[title]", "title"),
    attribute_values(html_doc, "[data-tooltip]", "data-tooltip"),
    attribute_values(html_doc, "[data-testid]", "data-testid")
  )

  candidates <- clean_text(candidates)
  unique(candidates[!is.na(candidates)])
}

find_engagement_raw <- function(candidates, patterns) {
  for (candidate in candidates) {
    for (pattern in patterns) {
      match_result <- regexec(pattern, candidate, ignore.case = TRUE, perl = TRUE)
      match_values <- regmatches(candidate, match_result)[[1]]

      if (length(match_values) >= 2) {
        return(clean_text(match_values[2]))
      }
    }
  }

  NA_character_
}

parse_public_stats_from_page <- function(html_doc) {
  candidates <- page_public_text_candidates(html_doc)
  compact_number <- "([0-9][0-9,.]*\\s*[KkMm]?)"

  # These patterns intentionally stay simple. They only use public page text
  # and accessible labels returned by a normal page request.
  claps_patterns <- c(
    paste0(compact_number, "\\s+(?:claps?|applause|recommends?|recommendations?)\\b"),
    paste0("\\b(?:claps?|applause|recommends?|recommendations?)\\b\\D{0,40}", compact_number)
  )

  responses_patterns <- c(
    paste0(compact_number, "\\s+(?:responses?|comments?)\\b"),
    paste0("\\b(?:responses?|comments?)\\b\\D{0,40}", compact_number)
  )

  claps_raw <- find_engagement_raw(candidates, claps_patterns)
  responses_raw <- find_engagement_raw(candidates, responses_patterns)

  list(
    claps_raw = claps_raw,
    responses_raw = responses_raw,
    claps_count = parse_compact_number(claps_raw),
    responses_count = parse_compact_number(responses_raw)
  )
}

make_public_request_sessions <- function() {
  safari_user_agent <- paste(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
    "AppleWebKit/605.1.15 (KHTML, like Gecko)",
    "Version/17.4 Safari/605.1.15"
  )

  chrome_user_agent <- paste(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
    "AppleWebKit/537.36 (KHTML, like Gecko)",
    "Chrome/124.0.0.0 Safari/537.36"
  )

  request_attempts <- list(
    list(
      name = "safari_headers_medium_referer",
      user_agent = safari_user_agent,
      referer = "https://medium.com/"
    ),
    list(
      name = "chrome_headers_medium_referer",
      user_agent = chrome_user_agent,
      referer = "https://medium.com/"
    ),
    list(
      name = "chrome_headers_tag_referer",
      user_agent = chrome_user_agent,
      referer = "https://medium.com/tag/finance"
    )
  )

  lapply(request_attempts, function(request_attempt) {
    request_handle <- new_handle()

    handle_setopt(
      request_handle,
      followlocation = TRUE,
      timeout = 30,
      cookiefile = ""
    )

    handle_setheaders(
      request_handle,
      "User-Agent" = request_attempt$user_agent,
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language" = "en-US,en;q=0.9",
      "Referer" = "https://medium.com/"
    )

    list(
      name = request_attempt$name,
      referer = request_attempt$referer,
      handle = request_handle,
      warmup_status_code = NA_integer_,
      warmup_error = NA_character_
    )
  })
}

warm_up_public_sessions <- function(request_sessions) {
  message("Warm-up request: https://medium.com/")

  for (session_index in seq_along(request_sessions)) {
    warmup_response <- tryCatch(
      curl_fetch_memory("https://medium.com/", handle = request_sessions[[session_index]]$handle),
      error = function(e) {
        list(error = conditionMessage(e))
      }
    )

    if (is.list(warmup_response) && !is.null(warmup_response$error)) {
      request_sessions[[session_index]]$warmup_error <- warmup_response$error
      message("Warm-up ", request_sessions[[session_index]]$name, ": failed - ", warmup_response$error)
      next
    }

    request_sessions[[session_index]]$warmup_status_code <- warmup_response$status_code
    message("Warm-up ", request_sessions[[session_index]]$name, ": HTTP ", warmup_response$status_code)
  }

  request_sessions
}

fetch_public_page <- function(article_url, request_sessions) {
  fetch_errors <- character(0)
  last_status_code <- NA_integer_
  last_method <- NA_character_

  for (request_attempt in request_sessions) {
    page_response <- tryCatch(
      {
        handle_setheaders(
          request_attempt$handle,
          "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "Accept-Language" = "en-US,en;q=0.9",
          "Referer" = request_attempt$referer
        )

        curl_fetch_memory(article_url, handle = request_attempt$handle)
      },
      error = function(e) {
        list(error = conditionMessage(e))
      }
    )

    last_method <- request_attempt$name

    if (is.list(page_response) && !is.null(page_response$error)) {
      fetch_errors <- c(fetch_errors, paste0(request_attempt$name, ": ", page_response$error))
      next
    }

    last_status_code <- page_response$status_code

    if (last_status_code < 200 || last_status_code >= 300) {
      fetch_errors <- c(fetch_errors, paste0(request_attempt$name, ": HTTP status ", last_status_code))
      next
    }

    if (length(page_response$content) == 0) {
      fetch_errors <- c(fetch_errors, paste0(request_attempt$name, ": page request returned no HTML"))
      next
    }

    html_text <- rawToChar(page_response$content)

    return(tryCatch(
      {
        parsed_page <- read_html(html_text)
        attr(parsed_page, "fetch_method") <- request_attempt$name
        parsed_page
      },
      error = function(e) {
        list(
          error = conditionMessage(e),
          status_code = last_status_code,
          method = request_attempt$name
        )
      }
    ))
  }

  list(
    error = paste(fetch_errors, collapse = " | "),
    status_code = last_status_code,
    method = last_method
  )
}

save_public_stats <- function(connection, observation) {
  dbExecute(
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
      ON CONFLICT(article_url, observed_date) DO UPDATE SET
        observed_at = excluded.observed_at,
        claps_count = excluded.claps_count,
        responses_count = excluded.responses_count,
        claps_raw = excluded.claps_raw,
        responses_raw = excluded.responses_raw,
        parse_status = excluded.parse_status,
        parse_method = excluded.parse_method,
        error_message = excluded.error_message
    ",
    params = unname(observation)
  )
}

collect_one_article <- function(connection, article, batch_number, attempted_articles, request_sessions) {
  observed_time <- Sys.time()
  observed_at <- format(observed_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  observed_date <- format(observed_time, "%Y-%m-%d", tz = "UTC")

  message("\nArticle ", batch_number, " of ", attempted_articles)
  message("Title: ", article$title)
  message("URL: ", article$url)

  html_doc <- fetch_public_page(article$url, request_sessions)

  if (is.list(html_doc) && !is.null(html_doc$error)) {
    status_text <- if (is.na(html_doc$status_code)) {
      "unknown"
    } else {
      as.character(html_doc$status_code)
    }

    observation <- list(
      article$url,
      observed_at,
      observed_date,
      NA_integer_,
      NA_integer_,
      NA_character_,
      NA_character_,
      "fetch_failed",
      paste0("public_html_", html_doc$method),
      html_doc$error
    )

    save_public_stats(connection, observation)

    message("Claps: not found")
    message("Responses: not found")
    message("HTTP status: ", status_text)
    message("Parse status: fetch_failed")

    return("fetch_failed")
  }

  parsed_stats <- parse_public_stats_from_page(html_doc)

  parse_status <- if (!is.na(parsed_stats$claps_count) || !is.na(parsed_stats$responses_count)) {
    "ok"
  } else {
    "not_found"
  }

  observation <- list(
    article$url,
    observed_at,
    observed_date,
    parsed_stats$claps_count,
    parsed_stats$responses_count,
    parsed_stats$claps_raw,
    parsed_stats$responses_raw,
    parse_status,
    paste0("public_html_text_and_attributes_", attr(html_doc, "fetch_method")),
    NA_character_
  )

  save_public_stats(connection, observation)

  claps_message <- if (is.na(parsed_stats$claps_count)) {
    "not found"
  } else {
    paste0(parsed_stats$claps_raw, " -> ", parsed_stats$claps_count)
  }

  responses_message <- if (is.na(parsed_stats$responses_count)) {
    "not found"
  } else {
    paste0(parsed_stats$responses_raw, " -> ", parsed_stats$responses_count)
  }

  message("Claps: ", claps_message)
  message("Responses: ", responses_message)
  message("Parse status: ", parse_status)

  parse_status
}

message("Medium Public Stats Collector")
message("=============================")

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

create_public_stats_table(connection)

request_sessions <- warm_up_public_sessions(make_public_request_sessions())

today <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")
articles <- select_articles_for_batch(connection, max_articles, today)
attempted_articles <- nrow(articles)

if (attempted_articles == 0) {
  message("No articles need a public stats observation for today.")
  message("Database path: ", database_path)
  quit(status = 0)
}

statuses <- character(0)

for (article_index in seq_len(attempted_articles)) {
  status <- tryCatch(
    collect_one_article(connection, articles[article_index, ], article_index, attempted_articles, request_sessions),
    error = function(e) {
      observed_time <- Sys.time()
      observed_at <- format(observed_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      observed_date <- format(observed_time, "%Y-%m-%d", tz = "UTC")

      observation <- list(
        articles$url[article_index],
        observed_at,
        observed_date,
        NA_integer_,
        NA_integer_,
        NA_character_,
        NA_character_,
        "error",
        "public_html_text_and_attributes",
        conditionMessage(e)
      )

      save_public_stats(connection, observation)

      message("Claps: not found")
      message("Responses: not found")
      message("Parse status: error")
      warning("Could not parse article '", articles$title[article_index], "': ", conditionMessage(e), call. = FALSE)

      "error"
    }
  )

  statuses <- c(statuses, status)

  if (article_index < attempted_articles) {
    Sys.sleep(delay_seconds)
  }
}

status_count <- function(status_name) {
  sum(statuses == status_name)
}

message("\nSummary")
message("=======")
message("Attempted articles: ", attempted_articles)
message("OK count: ", status_count("ok"))
message("Not found count: ", status_count("not_found"))
message("Fetch failed count: ", status_count("fetch_failed"))
message("Error count: ", status_count("error"))
message("Database path: ", database_path)
