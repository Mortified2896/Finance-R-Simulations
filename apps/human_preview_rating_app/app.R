required_packages <- c("shiny", "DBI", "RSQLite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ", paste(missing_packages, collapse = ", "), "\n\n",
    "Install them in R with:\n",
    'install.packages(c("', paste(missing_packages, collapse = '", "'), '"))',
    call. = FALSE
  )
}

library(shiny)
library(DBI)
library(RSQLite)

interface_version <- "human_preview_rating_app_v1"
rating_mode <- "feed_preview_1_5"
rating_prompt <- "Based only on the title, subtitle, and thumbnail, how likely is this article to perform well on Medium?"

clean_text <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

now_utc <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

find_project_root <- function() {
  env_root <- Sys.getenv("MEDIUM_PROJECT_ROOT", unset = "")
  candidates <- unique(c(
    env_root,
    getwd(),
    normalizePath(file.path(getwd(), ".."), mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE)
  ))
  candidates <- candidates[nzchar(candidates)]
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "data", "db", "medium_articles.sqlite"))) {
      return(normalizePath(candidate, mustWork = TRUE))
    }
  }
  stop("Could not find project root with data/db/medium_articles.sqlite.", call. = FALSE)
}

project_root <- find_project_root()
db_path <- Sys.getenv(
  "MEDIUM_RATING_DB",
  unset = file.path(project_root, "data", "db", "medium_articles.sqlite")
)
thumbnail_queue_path <- file.path(
  project_root,
  "data",
  "analysis",
  "medium_images",
  "medium_image_download_queue.csv"
)

split_keys <- function(x) {
  value <- clean_text(x)
  if (length(value) == 0 || is.na(value)) return(character())
  parts <- unlist(strsplit(value, "[,;|]", perl = TRUE), use.names = FALSE)
  parts <- clean_text(parts)
  unique(parts[!is.na(parts)])
}

normalize_image_url <- function(url) {
  value <- clean_text(url)
  vapply(value, function(one_url) {
    if (is.na(one_url)) return(NA_character_)
    without_fragment <- sub("#.*$", "", one_url)
    split_url <- strsplit(without_fragment, "\\?", fixed = FALSE)[[1]]
    base_url <- split_url[[1]]
    if (length(split_url) == 1 || split_url[[2]] == "") return(base_url)
    query_params <- unlist(strsplit(split_url[[2]], "&", fixed = TRUE), use.names = FALSE)
    parameter_names <- sub("=.*$", "", query_params)
    tracking_param <- grepl("^utm_", parameter_names, ignore.case = TRUE) |
      tolower(parameter_names) %in% c("fbclid", "gclid")
    kept_params <- query_params[!tracking_param & query_params != ""]
    if (length(kept_params) == 0) base_url else paste0(base_url, "?", paste(kept_params, collapse = "&"))
  }, character(1), USE.NAMES = FALSE)
}

connect_db <- function() {
  dbConnect(SQLite(), db_path)
}

ensure_rating_schema <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_rating_sessions (
      rating_session_id TEXT PRIMARY KEY,
      created_at TEXT,
      interface_version TEXT,
      rating_mode TEXT,
      queue_seed INTEGER,
      target_n INTEGER,
      notes TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_rating_queue (
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      status TEXT DEFAULT 'pending',
      shown_at TEXT,
      completed_at TEXT,
      PRIMARY KEY (rating_session_id, queue_position)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_ratings (
      id INTEGER PRIMARY KEY,
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      interface_version TEXT,
      rating_prompt TEXT,
      shown_title TEXT,
      shown_subtitle TEXT,
      shown_thumbnail_path TEXT,
      human_feed_success_potential INTEGER,
      human_feed_success_note TEXT,
      skipped INTEGER DEFAULT 0,
      shown_at TEXT,
      rated_at TEXT,
      seconds_spent REAL
    )
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_queue_status
    ON human_preview_rating_queue (rating_session_id, status, queue_position)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_ratings_session
    ON human_preview_ratings (rating_session_id, rated_at, id)
  ")
}

read_thumbnail_queue <- function() {
  if (!file.exists(thumbnail_queue_path)) return(data.frame())
  queue <- read.csv(thumbnail_queue_path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("local_image_path", "article_ids", "medium_post_ids")
  for (column in setdiff(required, names(queue))) queue[[column]] <- NA_character_
  for (column in c("normalized_image_url", "primary_image_url_for_download")) {
    if (!(column %in% names(queue))) queue[[column]] <- NA_character_
  }
  queue$local_image_path <- clean_text(queue$local_image_path)
  queue$local_image_path_abs <- vapply(queue$local_image_path, function(path) {
    if (is.na(path)) return(NA_character_)
    if (grepl("^/", path)) path else file.path(project_root, path)
  }, character(1), USE.NAMES = FALSE)
  queue$local_exists <- !is.na(queue$local_image_path_abs) & file.exists(queue$local_image_path_abs)
  queue
}

build_thumbnail_lookup <- function(queue = read_thumbnail_queue()) {
  if (nrow(queue) == 0) {
    return(list(article_ids = character(), post_ids = character(), urls = character()))
  }

  queue <- queue[queue$local_exists, , drop = FALSE]
  if (nrow(queue) == 0) {
    return(list(article_ids = character(), post_ids = character(), urls = character()))
  }

  article_ids <- character()
  article_paths <- character()
  post_ids <- character()
  post_paths <- character()
  urls <- character()
  url_paths <- character()

  for (i in seq_len(nrow(queue))) {
    path <- queue$local_image_path_abs[[i]]
    row_article_ids <- split_keys(queue$article_ids[[i]])
    row_post_ids <- split_keys(queue$medium_post_ids[[i]])
    row_urls <- clean_text(c(queue$normalized_image_url[[i]], queue$primary_image_url_for_download[[i]]))
    row_urls <- row_urls[!is.na(row_urls)]

    if (length(row_article_ids) > 0) {
      article_ids <- c(article_ids, row_article_ids)
      article_paths <- c(article_paths, rep(path, length(row_article_ids)))
    }
    if (length(row_post_ids) > 0) {
      post_ids <- c(post_ids, row_post_ids)
      post_paths <- c(post_paths, rep(path, length(row_post_ids)))
    }
    if (length(row_urls) > 0) {
      urls <- c(urls, row_urls)
      url_paths <- c(url_paths, rep(path, length(row_urls)))
    }
  }

  article_paths <- article_paths[!duplicated(article_ids)]
  names(article_paths) <- article_ids[!duplicated(article_ids)]
  post_paths <- post_paths[!duplicated(post_ids)]
  names(post_paths) <- post_ids[!duplicated(post_ids)]
  url_paths <- url_paths[!duplicated(urls)]
  names(url_paths) <- urls[!duplicated(urls)]

  list(article_ids = article_paths, post_ids = post_paths, urls = url_paths)
}

lookup_map_value <- function(map, key) {
  key <- clean_text(key)
  if (length(key) == 0 || is.na(key) || !(key %in% names(map))) return(NA_character_)
  unname(map[[key]])
}

lookup_local_thumbnail <- function(article_id, medium_post_id, thumbnail_url, lookup) {
  if (length(lookup$article_ids) == 0 && length(lookup$post_ids) == 0 && length(lookup$urls) == 0) {
    return(NA_character_)
  }

  article_key <- clean_text(article_id)
  post_key <- clean_text(medium_post_id)
  thumb_key <- normalize_image_url(thumbnail_url)

  path <- lookup_map_value(lookup$article_ids, article_key)
  if (!is.na(path)) return(path)

  path <- lookup_map_value(lookup$post_ids, post_key)
  if (!is.na(path)) return(path)

  path <- lookup_map_value(lookup$urls, thumb_key)
  if (!is.na(path)) return(path)

  NA_character_
}

load_candidates <- function(con, target_n) {
  objects <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type IN ('table', 'view')")
  if (!("v_medium_title_prediction_dataset_v2" %in% objects$name)) {
    stop("Missing v_medium_title_prediction_dataset_v2. Run the Medium Analysis V2 schema setup first.", call. = FALSE)
  }

  rows <- dbGetQuery(con, "
    SELECT
      canonical_article_key,
      article_id,
      medium_post_id,
      url,
      title,
      subtitle,
      thumbnail_url
    FROM v_medium_title_prediction_dataset_v2
    WHERE NULLIF(TRIM(title), '') IS NOT NULL
  ")
  rows$title <- clean_text(rows$title)
  rows$subtitle <- clean_text(rows$subtitle)

  lookup <- build_thumbnail_lookup()
  rows$local_thumbnail_path <- vapply(seq_len(nrow(rows)), function(i) {
    lookup_local_thumbnail(rows$article_id[[i]], rows$medium_post_id[[i]], rows$thumbnail_url[[i]], lookup)
  }, character(1))
  rows$has_local_thumbnail <- !is.na(rows$local_thumbnail_path) & file.exists(rows$local_thumbnail_path)

  local_rows <- rows[rows$has_local_thumbnail, , drop = FALSE]
  fallback_rows <- rows[!rows$has_local_thumbnail, , drop = FALSE]
  if (nrow(local_rows) >= target_n) local_rows else rbind(local_rows, fallback_rows)
}

create_new_session <- function(con, target_n = 100) {
  seed <- sample.int(.Machine$integer.max, 1)
  session_id <- paste0("preview_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", seed)
  candidates <- load_candidates(con, target_n)
  if (nrow(candidates) == 0) stop("No candidate articles with titles were found.", call. = FALSE)

  set.seed(seed)
  local_first <- candidates[order(!candidates$has_local_thumbnail), , drop = FALSE]
  shuffled <- local_first[sample.int(nrow(local_first)), , drop = FALSE]
  selected <- head(shuffled, target_n)

  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      "INSERT INTO human_rating_sessions
       (rating_session_id, created_at, interface_version, rating_mode, queue_seed, target_n, notes)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(session_id, now_utc(), interface_version, rating_mode, seed, target_n, NA_character_)
    )

    for (i in seq_len(nrow(selected))) {
      dbExecute(
        con,
        "INSERT INTO human_preview_rating_queue
         (rating_session_id, queue_position, article_id, medium_post_id, status)
         VALUES (?, ?, ?, ?, 'pending')",
        params = list(
          session_id,
          i,
          selected$article_id[[i]],
          selected$medium_post_id[[i]]
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  session_id
}

resume_or_create_session <- function(con, target_n = 100) {
  existing <- dbGetQuery(con, "
    SELECT s.rating_session_id
    FROM human_rating_sessions s
    WHERE s.interface_version = ?
      AND s.rating_mode = ?
      AND EXISTS (
        SELECT 1
        FROM human_preview_rating_queue q
        WHERE q.rating_session_id = s.rating_session_id
          AND q.status = 'pending'
      )
    ORDER BY s.created_at DESC
    LIMIT 1
  ", params = list(interface_version, rating_mode))

  if (nrow(existing) > 0) existing$rating_session_id[[1]] else create_new_session(con, target_n)
}

load_current_item <- function(con, session_id) {
  item <- dbGetQuery(con, "
    SELECT rating_session_id, queue_position, article_id, medium_post_id, status, shown_at, completed_at
    FROM human_preview_rating_queue
    WHERE rating_session_id = ?
      AND status = 'pending'
    ORDER BY queue_position
    LIMIT 1
  ", params = list(session_id))
  if (nrow(item) == 0) return(NULL)

  if (is.na(item$shown_at[[1]]) || !nzchar(item$shown_at[[1]])) {
    item$shown_at[[1]] <- now_utc()
    dbExecute(
      con,
      "UPDATE human_preview_rating_queue
       SET shown_at = ?
       WHERE rating_session_id = ? AND queue_position = ?",
      params = list(item$shown_at[[1]], session_id, item$queue_position[[1]])
    )
  }

  details <- dbGetQuery(con, "
    SELECT
      canonical_article_key,
      article_id,
      medium_post_id,
      url,
      title,
      subtitle,
      thumbnail_url
    FROM v_medium_title_prediction_dataset_v2
    WHERE article_id = ?
       OR (medium_post_id IS NOT NULL AND medium_post_id = ?)
    LIMIT 1
  ", params = list(item$article_id[[1]], item$medium_post_id[[1]]))

  if (nrow(details) == 0) {
    details <- dbGetQuery(con, "
      SELECT
        NULL AS canonical_article_key,
        id AS article_id,
        medium_post_id,
        url,
        title,
        subtitle,
        image_url AS thumbnail_url
      FROM medium_articles
      WHERE id = ?
      LIMIT 1
    ", params = list(item$article_id[[1]]))
  }

  if (nrow(details) == 0) return(NULL)

  details$title <- clean_text(details$title)
  details$subtitle <- clean_text(details$subtitle)
  lookup <- build_thumbnail_lookup()
  details$local_thumbnail_path <- lookup_local_thumbnail(
    details$article_id[[1]],
    details$medium_post_id[[1]],
    details$thumbnail_url[[1]],
    lookup
  )

  cbind(item, details[1, , drop = FALSE])
}

queue_counts <- function(con, session_id) {
  dbGetQuery(con, "
    SELECT
      COUNT(*) AS total,
      SUM(CASE WHEN status IN ('rated', 'skipped') THEN 1 ELSE 0 END) AS completed,
      SUM(CASE WHEN status = 'pending' THEN 1 ELSE 0 END) AS pending
    FROM human_preview_rating_queue
    WHERE rating_session_id = ?
  ", params = list(session_id))
}

save_current_rating <- function(con, item, score = NULL, note = "", skipped = FALSE, shown_started_at = Sys.time()) {
  if (is.null(item) || nrow(item) == 0) return(invisible(FALSE))
  rated_at <- now_utc()
  seconds_spent <- as.numeric(difftime(Sys.time(), shown_started_at, units = "secs"))
  seconds_spent <- max(0, seconds_spent)
  score_value <- if (is.null(score)) NA_integer_ else as.integer(score)
  note_value <- clean_text(note)
  if (length(note_value) == 0 || is.na(note_value[[1]])) note_value <- NA_character_

  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      "INSERT INTO human_preview_ratings
       (rating_session_id, queue_position, article_id, medium_post_id, interface_version,
        rating_prompt, shown_title, shown_subtitle, shown_thumbnail_path,
        human_feed_success_potential, human_feed_success_note, skipped,
        shown_at, rated_at, seconds_spent)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        item$rating_session_id[[1]],
        item$queue_position[[1]],
        item$article_id[[1]],
        item$medium_post_id[[1]],
        interface_version,
        rating_prompt,
        item$title[[1]],
        item$subtitle[[1]],
        item$local_thumbnail_path[[1]],
        score_value,
        note_value[[1]],
        if (isTRUE(skipped)) 1L else 0L,
        item$shown_at[[1]],
        rated_at,
        seconds_spent
      )
    )

    dbExecute(
      con,
      "UPDATE human_preview_rating_queue
       SET status = ?, completed_at = ?
       WHERE rating_session_id = ? AND queue_position = ?",
      params = list(
        if (isTRUE(skipped)) "skipped" else "rated",
        rated_at,
        item$rating_session_id[[1]],
        item$queue_position[[1]]
      )
    )
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  invisible(TRUE)
}

undo_previous_rating <- function(con, session_id) {
  previous <- dbGetQuery(con, "
    SELECT id, queue_position
    FROM human_preview_ratings
    WHERE rating_session_id = ?
    ORDER BY rated_at DESC, id DESC
    LIMIT 1
  ", params = list(session_id))
  if (nrow(previous) == 0) return(FALSE)

  dbBegin(con)
  tryCatch({
    dbExecute(con, "DELETE FROM human_preview_ratings WHERE id = ?", params = list(previous$id[[1]]))
    dbExecute(
      con,
      "UPDATE human_preview_rating_queue
       SET status = 'pending', shown_at = NULL, completed_at = NULL
       WHERE rating_session_id = ? AND queue_position = ?",
      params = list(session_id, previous$queue_position[[1]])
    )
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  TRUE
}

ui <- fluidPage(
  tags$head(
    tags$title("Medium Preview Rating"),
    tags$style(HTML("
      :root {
        --green: #1a8917;
        --green-soft: #eef7f0;
        --ink: #191919;
        --muted: #6b6b6b;
        --line: #e6e6e6;
        --panel: #f8f8f8;
      }
      body {
        margin: 0;
        color: var(--ink);
        background: #fff;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      }
      .container-fluid { padding: 0; }
      .topbar {
        height: 50px;
        border-bottom: 1px solid var(--line);
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 0 22px;
      }
      .brand { display: flex; align-items: center; gap: 14px; font-size: 20px; font-weight: 700; }
      .brand-mark {
        width: 32px; height: 32px; border-radius: 6px; background: #197b30; color: #fff;
        display: grid; place-items: center; font-family: Georgia, serif; font-size: 23px; font-weight: 700;
      }
      .top-actions { color: var(--muted); display: flex; gap: 22px; align-items: center; font-size: 15px; }
      .app-shell { display: grid; grid-template-columns: 250px minmax(560px, 1fr) 300px; min-height: calc(100vh - 50px); }
      .sidebar, .guide { border-right: 1px solid var(--line); padding: 22px 20px; position: relative; }
      .guide { border-right: 0; border-left: 1px solid var(--line); }
      .nav-item {
        height: 42px; display: flex; align-items: center; gap: 14px; padding: 0 18px;
        border-radius: 8px; color: var(--ink); font-size: 16px; margin-bottom: 10px;
      }
      .nav-item.active { background: var(--green-soft); font-weight: 650; }
      .daily-goal {
        border: 1px solid var(--line); border-radius: 8px; padding: 14px 18px;
        max-width: 250px;
        position: absolute;
        left: 20px;
        right: 20px;
      }
      .daily-goal strong { display: block; margin-bottom: 8px; }
      .daily-goal .num { color: var(--green); font-weight: 700; }
      .progress-track { height: 7px; background: #e9e9e9; border-radius: 99px; overflow: hidden; margin: 12px 0 8px; }
      .progress-fill { height: 100%; background: var(--green); border-radius: 99px; width: 0%; }
      .main { padding: 22px 30px 18px; max-width: 920px; width: 100%; margin: 0 auto; }
      h1 { margin: 0; font-size: 26px; line-height: 1.05; font-weight: 750; letter-spacing: 0; }
      .progress-line { margin-top: 5px; color: var(--muted); font-size: 16px; }
      .progress-line .current { color: var(--green); font-weight: 750; }
      .tabs { display: flex; gap: 38px; border-bottom: 1px solid var(--line); margin-top: 14px; max-width: 760px; box-sizing: border-box; }
      .tab { padding-bottom: 8px; color: var(--muted); font-size: 15px; }
      .tab.active { color: var(--ink); font-weight: 650; border-bottom: 2px solid var(--ink); }
      .article-card {
        display: grid;
        grid-template-columns: minmax(0, 1fr) 170px;
        gap: 32px;
        align-items: center;
        box-sizing: border-box;
        width: 100%;
        max-width: 760px;
        margin-top: 0;
        padding: 28px 0 30px;
        border-bottom: 1px solid #f2f2f2;
        background: #fff;
      }
      .article-title {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        font-size: 26px; line-height: 1.15; font-weight: 760; letter-spacing: 0; margin: 0 0 8px;
      }
      .article-subtitle { font-size: 18px; line-height: 1.35; color: var(--muted); margin: 0; }
      .thumbnail-wrap { display: flex; justify-content: flex-end; align-items: center; }
      .thumbnail-wrap .shiny-image-output {
        width: 170px !important;
        height: 113px !important;
      }
      .thumbnail-wrap img {
        width: 170px; height: 113px; object-fit: cover; border-radius: 1px; display: block;
      }
      .thumbnail-placeholder {
        width: 170px; height: 113px; border-radius: 1px; background: #f2f2f2; border: 1px solid var(--line);
      }
      .rating-panel {
        border: 1px solid var(--line);
        border-radius: 8px;
        box-sizing: border-box;
        background: #fff;
        margin-top: 12px;
        max-width: 760px;
        padding: 14px 20px;
      }
      .prompt { font-size: 15px; line-height: 1.22; font-weight: 680; margin-bottom: 10px; }
      .note-row label { color: var(--muted); font-weight: 500; margin-bottom: 4px; font-size: 13px; }
      textarea.form-control {
        min-height: 44px; resize: vertical; border: 1px solid #d9d9d9; border-radius: 8px;
        box-shadow: none; font-size: 14px; padding: 10px 12px;
      }
      textarea.form-control:focus { border-color: var(--green); box-shadow: 0 0 0 3px rgba(26, 137, 23, .12); }
      .scale-labels { display: flex; justify-content: space-between; color: var(--muted); margin: 9px 4px 5px; font-size: 13px; }
      .rating-buttons { display: grid; grid-template-columns: repeat(5, minmax(64px, 1fr)); gap: 10px; }
      .rating-buttons .btn {
        height: 40px; border: 1px solid #d8d8d8; border-radius: 7px; background: #fff;
        color: var(--ink); font-size: 19px; font-weight: 650; box-shadow: none;
      }
      .rating-buttons .btn:hover {
        border-color: var(--green); color: var(--green); background: #f6fbf6;
      }
      .rating-buttons .btn.rating-confirm {
        border-color: var(--green); color: #fff; background: var(--green);
      }
      .rating-buttons .btn:focus {
        border-color: #d8d8d8; color: var(--ink); background: #fff; outline: none; box-shadow: none;
      }
      .rating-actions { display: flex; gap: 12px; justify-content: space-between; align-items: center; margin-top: 10px; }
      .rating-actions .btn {
        min-width: 136px; height: 34px; border-radius: 7px; font-size: 13px; font-weight: 650; box-shadow: none;
      }
      .btn-default { border-color: #d8d8d8; background: #fff; color: var(--ink); }
      .btn-default:hover { border-color: #bdbdbd; background: #fafafa; }
      .shortcut-copy { color: var(--muted); font-size: 13px; }
      .guide-section { border-bottom: 1px solid var(--line); padding: 10px 0 16px; }
      .guide-section:first-child { padding-top: 0; }
      .guide-section:last-child { border-bottom: 0; }
      .guide h3 { font-size: 16px; margin: 0 0 10px; font-weight: 750; }
      .guide p, .guide li { color: #333; line-height: 1.34; font-size: 13px; }
      .guide ul { padding-left: 18px; margin: 0; }
      .guide li { margin-bottom: 5px; }
      .guide .tip {
        background: var(--green-soft); border-radius: 8px; padding: 14px;
        position: absolute;
        left: 20px;
        right: 20px;
      }
      .done-state {
        border: 1px solid var(--line); border-radius: 8px; padding: 42px; margin-top: 28px; text-align: center;
      }
      @media (max-width: 1180px) {
        .app-shell { grid-template-columns: 86px minmax(520px, 1fr); }
        .guide { display: none; }
        .sidebar { padding: 24px 14px; }
        .nav-item span, .daily-goal { display: none; }
      }
      @media (max-width: 820px) {
        .app-shell { display: block; }
        .sidebar { display: none; }
        .main { padding: 28px 18px; }
        .article-card { grid-template-columns: 1fr; gap: 18px; padding: 24px 0 26px; }
        .thumbnail-wrap { justify-content: flex-start; }
        .thumbnail-wrap .shiny-image-output,
        .thumbnail-wrap img,
        .thumbnail-placeholder { width: 100% !important; height: auto !important; aspect-ratio: 1.5; }
        .rating-buttons { gap: 8px; }
        .rating-buttons .btn { height: 52px; font-size: 21px; }
      }
    ")),
    tags$script(HTML("
      function flashAndSubmitRating(score) {
        const button = document.getElementById('score_' + score);
        if (button) {
          document.querySelectorAll('.rating-buttons .btn').forEach(function(oneButton) {
            oneButton.classList.remove('rating-confirm');
          });
          button.classList.add('rating-confirm');
        }
        window.setTimeout(function() {
          Shiny.setInputValue('score_key', {score: Number(score), nonce: Date.now()}, {priority: 'event'});
        }, 140);
      }

      function alignSideCardsToRatingPanel() {
        const ratingPanel = document.querySelector('.rating-panel');
        const sidebar = document.querySelector('.sidebar');
        const guide = document.querySelector('.guide');
        const dailyGoal = document.querySelector('.daily-goal');
        const tip = document.querySelector('.guide .tip');
        if (!ratingPanel || !sidebar || !guide || !dailyGoal || !tip) return;

        const ratingBottom = ratingPanel.getBoundingClientRect().bottom;
        const sidebarTop = sidebar.getBoundingClientRect().top;
        const guideTop = guide.getBoundingClientRect().top;
        const dailyTop = Math.max(22, ratingBottom - sidebarTop - dailyGoal.offsetHeight);
        const tipTop = Math.max(22, ratingBottom - guideTop - tip.offsetHeight);

        dailyGoal.style.top = dailyTop + 'px';
        tip.style.top = tipTop + 'px';
      }

      window.addEventListener('resize', function() {
        window.requestAnimationFrame(alignSideCardsToRatingPanel);
      });
      document.addEventListener('DOMContentLoaded', function() {
        window.setTimeout(alignSideCardsToRatingPanel, 250);
      });
      window.setInterval(alignSideCardsToRatingPanel, 300);

      document.addEventListener('click', function(event) {
        const button = event.target && event.target.closest ? event.target.closest('.rating-buttons .btn') : null;
        if (!button || !button.id || !button.id.match(/^score_[1-5]$/)) return;
        event.preventDefault();
        event.stopPropagation();
        event.stopImmediatePropagation();
        flashAndSubmitRating(button.id.replace('score_', ''));
      }, true);

      document.addEventListener('keydown', function(event) {
        if (event.metaKey || event.ctrlKey || event.altKey) return;
        const tag = (event.target && event.target.tagName || '').toLowerCase();
        const inText = tag === 'textarea' || tag === 'input';
        const rawKey = event.key || '';
        const rawCode = event.code || '';
        const digitMatch = rawKey.match(/^[1-5]$/) || rawCode.match(/^(Digit|Numpad)([1-5])$/);
        const digit = digitMatch ? Number(digitMatch[2] || digitMatch[0]) : null;
        const letter = rawKey.length === 1 ? rawKey.toLowerCase() : rawCode.replace(/^Key/, '').toLowerCase();

        if (rawKey === 'Escape' || rawCode === 'Escape') {
          const note = document.getElementById('note');
          if (note) {
            note.value = '';
            note.dispatchEvent(new Event('input', { bubbles: true }));
          }
          Shiny.setInputValue('clear_note_shortcut', Date.now(), {priority: 'event'});
          return;
        }

        if (inText) return;

        if (digit !== null) {
          event.preventDefault();
          flashAndSubmitRating(digit);
        } else if (letter === 's') {
          event.preventDefault();
          Shiny.setInputValue('skip_key', Date.now(), {priority: 'event'});
        } else if (letter === 'u') {
          event.preventDefault();
          Shiny.setInputValue('undo_key', Date.now(), {priority: 'event'});
        } else if (letter === 'n') {
          event.preventDefault();
          const note = document.getElementById('note');
          if (note) note.focus();
        }
      });
      function handleClearRatingFocus(_) {
        document.querySelectorAll('.rating-buttons .btn').forEach(function(oneButton) {
          oneButton.classList.remove('rating-confirm');
        });
        if (document.activeElement && document.activeElement.blur) {
          document.activeElement.blur();
        }
        window.setTimeout(alignSideCardsToRatingPanel, 80);
        window.setTimeout(alignSideCardsToRatingPanel, 260);
      }
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler('clearRatingFocus', handleClearRatingFocus);
      } else {
        document.addEventListener('shiny:connected', function() {
          Shiny.addCustomMessageHandler('clearRatingFocus', handleClearRatingFocus);
        }, { once: true });
      }
    "))
  ),
  div(
    class = "topbar",
    div(class = "brand", div(class = "brand-mark", "M"), div("Medium Preview Rating")),
    div(class = "top-actions", span("Focus mode"), span("Local SQLite"))
  ),
  div(
    class = "app-shell",
    tags$aside(
      class = "sidebar",
      div(class = "nav-item active", span("\u2302"), span("Home")),
      div(class = "nav-item", span("\u25a4"), span("Queue")),
      div(class = "nav-item", span("\u21ba"), span("History")),
      div(class = "nav-item", span("\u2699"), span("Settings")),
      div(
        class = "daily-goal",
        strong("Daily goal"),
        htmlOutput("sidebar_progress"),
        uiOutput("progress_bar"),
        div(class = "shortcut-copy", "1-5 rate, S skip, U undo")
      )
    ),
    tags$main(
      class = "main",
      h1("Medium Preview Rating"),
      htmlOutput("progress_line"),
      div(class = "tabs", div(class = "tab active", "For you"), div(class = "tab", "Featured")),
      uiOutput("article_area"),
      div(
        class = "rating-panel",
        div(class = "prompt", rating_prompt),
        div(
          class = "note-row",
          textAreaInput(
            "note",
            "Optional note",
            value = "",
            width = "100%",
            placeholder = "Share any quick thoughts on this headline, angle, or topic..."
          )
        ),
        div(class = "scale-labels", span("Very weak"), span("Very strong")),
        div(
          class = "rating-buttons",
          actionButton("score_1", "1"),
          actionButton("score_2", "2"),
          actionButton("score_3", "3"),
          actionButton("score_4", "4"),
          actionButton("score_5", "5")
        ),
        div(
          class = "rating-actions",
          div(actionButton("skip", "Skip"), actionButton("undo", "Undo previous")),
          div(class = "shortcut-copy", "N focuses note. Esc clears note.")
        )
      )
    ),
    tags$aside(
      class = "guide",
      div(class = "guide-section", h3("How it works"), p("Rate each preview using only the visible headline, subtitle, and thumbnail.")),
      div(
        class = "guide-section",
        h3("Focus on"),
        tags$ul(
          tags$li("Headline clarity and hook"),
          tags$li("Topic relevance and appeal"),
          tags$li("Perceived value to readers"),
          tags$li("Your gut feeling")
        )
      ),
      div(
        class = "guide-section",
        h3("Rating guide"),
        p("1 Very weak"),
        p("2 Weak"),
        p("3 Average / unclear"),
        p("4 Strong"),
        p("5 Very strong")
      ),
      div(class = "tip", h3("Tip"), p("There are no right or wrong answers. Consistency is the goal."))
    )
  )
)

server <- function(input, output, session) {
  con <- connect_db()
  onStop(function() dbDisconnect(con))
  ensure_rating_schema(con)

  rating_session_id <- resume_or_create_session(con, target_n = 100)
  current <- reactiveVal(NULL)
  shown_started_at <- reactiveVal(Sys.time())

  refresh_current <- function() {
    item <- load_current_item(con, rating_session_id)
    current(item)
    shown_started_at(Sys.time())
    updateTextAreaInput(session, "note", value = "")
    session$sendCustomMessage("clearRatingFocus", list())
  }

  refresh_current()

  counts <- reactive({
    invalidateLater(1000, session)
    queue_counts(con, rating_session_id)
  })

  output$progress_line <- renderUI({
    c <- counts()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- c$total[[1]]
    HTML(sprintf("Article <span class='current'>%s</span> / %s", completed + 1L, total))
  })

  output$sidebar_progress <- renderUI({
    c <- counts()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- c$total[[1]]
    HTML(sprintf("<span class='num'>%s</span> / %s", completed, total))
  })

  output$progress_bar <- renderUI({
    c <- counts()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- max(1, c$total[[1]])
    div(
      class = "progress-track",
      div(class = "progress-fill", style = sprintf("width: %.1f%%;", 100 * completed / total))
    )
  })

  output$article_area <- renderUI({
    item <- current()
    if (is.null(item)) {
      return(div(class = "done-state", h2("Session complete"), p("All queued previews have been rated or skipped.")))
    }

    thumbnail_path <- item$local_thumbnail_path[[1]]
    has_thumbnail <- !is.na(thumbnail_path) && file.exists(thumbnail_path)
    thumbnail_ui <- if (has_thumbnail) {
      imageOutput("thumbnail", width = "170px", height = "113px")
    } else {
      div(class = "thumbnail-placeholder")
    }

    subtitle <- item$subtitle[[1]]
    div(
      class = "article-card",
      div(
        h2(class = "article-title", item$title[[1]]),
        if (!is.na(subtitle)) p(class = "article-subtitle", subtitle)
      ),
      div(class = "thumbnail-wrap", thumbnail_ui)
    )
  })

  output$thumbnail <- renderImage({
    item <- current()
    req(!is.null(item))
    path <- item$local_thumbnail_path[[1]]
    req(!is.na(path), file.exists(path))
    list(src = normalizePath(path, mustWork = TRUE), alt = "", width = 170, height = 113)
  }, deleteFile = FALSE)

  handle_score <- function(score) {
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    save_current_rating(con, item, score = score, note = input$note, skipped = FALSE, shown_started_at = shown_started_at())
    refresh_current()
  }

  observeEvent(input$score_1, handle_score(1L), ignoreInit = TRUE)
  observeEvent(input$score_2, handle_score(2L), ignoreInit = TRUE)
  observeEvent(input$score_3, handle_score(3L), ignoreInit = TRUE)
  observeEvent(input$score_4, handle_score(4L), ignoreInit = TRUE)
  observeEvent(input$score_5, handle_score(5L), ignoreInit = TRUE)
  observeEvent(input$score_key, {
    score_value <- input$score_key
    if (is.list(score_value) && !is.null(score_value$score)) {
      score_value <- score_value$score
    }
    handle_score(as.integer(score_value))
  }, ignoreInit = TRUE)

  observeEvent(input$skip, {
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    save_current_rating(con, item, score = NULL, note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$skip_key, {
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    save_current_rating(con, item, score = NULL, note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$undo, {
    undo_previous_rating(con, rating_session_id)
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$undo_key, {
    undo_previous_rating(con, rating_session_id)
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$clear_note_shortcut, {
    updateTextAreaInput(session, "note", value = "")
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
