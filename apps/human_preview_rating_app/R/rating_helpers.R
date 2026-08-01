read_thumbnail_queue <- function() {
  if (!file.exists(thumbnail_queue_path)) return(data.frame())
  queue <- read.csv(thumbnail_queue_path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("local_image_path", "article_ids", "medium_post_ids")
  for (column in setdiff(required, names(queue))) queue[[column]] <- NA_character_
  for (column in c("normalized_image_url", "primary_image_url_for_download", "image_file_stem")) {
    if (!(column %in% names(queue))) queue[[column]] <- NA_character_
  }
  queue$local_image_path <- clean_text(queue$local_image_path)
  queue$local_image_path_abs <- vapply(queue$local_image_path, function(path) {
    if (is.na(path)) return(NA_character_)
    if (grepl("^/", path)) path else file.path(project_root, path)
  }, character(1), USE.NAMES = FALSE)
  queue$local_path_stem <- tools::file_path_sans_ext(basename(queue$local_image_path_abs))
  queue$local_path_matches_stem <- is.na(clean_text(queue$image_file_stem)) |
    (!is.na(queue$local_path_stem) & startsWith(queue$local_path_stem, clean_text(queue$image_file_stem)))
  queue$local_path_matches_stem[is.na(queue$local_path_matches_stem)] <- FALSE
  queue$local_exists <- !is.na(queue$local_image_path_abs) &
    file.exists(queue$local_image_path_abs) &
    queue$local_path_matches_stem
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

  path <- lookup_map_value(lookup$urls, thumb_key)
  if (!is.na(path)) return(path)

  path <- lookup_map_value(lookup$article_ids, article_key)
  if (!is.na(path)) return(path)

  path <- lookup_map_value(lookup$post_ids, post_key)
  if (!is.na(path)) return(path)

  NA_character_
}

get_rated_keys <- function(con) {
  if (!dbExistsTable(con, "human_preview_ratings")) {
    return(list(article_ids = character(), post_ids = character(), article_lab_candidate_ids = character()))
  }

  rated <- dbGetQuery(con, "
    SELECT DISTINCT article_id, medium_post_id, article_lab_candidate_id
    FROM human_preview_ratings
  ")

  article_ids <- clean_text(rated$article_id)
  post_ids <- clean_text(rated$medium_post_id)
  article_lab_candidate_ids <- clean_text(rated$article_lab_candidate_id)
  list(
    article_ids = unique(article_ids[!is.na(article_ids)]),
    post_ids = unique(post_ids[!is.na(post_ids)]),
    article_lab_candidate_ids = unique(article_lab_candidate_ids[!is.na(article_lab_candidate_ids)])
  )
}

mark_duplicate_pending_queue_items <- function(con) {
  rated_keys <- get_rated_keys(con)
  if (length(rated_keys$article_ids) == 0 && length(rated_keys$post_ids) == 0 && length(rated_keys$article_lab_candidate_ids) == 0) return(0L)

  pending <- dbGetQuery(con, "
    SELECT rating_session_id, queue_position, article_id, medium_post_id, source_type, article_lab_candidate_id
    FROM human_preview_rating_queue
    WHERE status = 'pending'
  ")
  if (nrow(pending) == 0) return(0L)

  article_keys <- clean_text(pending$article_id)
  post_keys <- clean_text(pending$medium_post_id)
  article_lab_keys <- clean_text(pending$article_lab_candidate_id)
  duplicate <- (!is.na(article_keys) & article_keys %in% rated_keys$article_ids) |
    (!is.na(post_keys) & post_keys %in% rated_keys$post_ids) |
    (!is.na(article_lab_keys) & article_lab_keys %in% rated_keys$article_lab_candidate_ids)
  duplicate[is.na(duplicate)] <- FALSE
  duplicate_rows <- pending[duplicate, , drop = FALSE]
  if (nrow(duplicate_rows) == 0) return(0L)

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(duplicate_rows))) {
      dbExecute(
        con,
        "UPDATE human_preview_rating_queue
         SET status = 'ignored_duplicate', completed_at = ?
         WHERE rating_session_id = ? AND queue_position = ? AND status = 'pending'",
        params = list(
          now_utc(),
          duplicate_rows$rating_session_id[[i]],
          duplicate_rows$queue_position[[i]]
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  nrow(duplicate_rows)
}

load_candidates <- function(con, exclude_rated = TRUE) {
  objects <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type IN ('table', 'view')")
  rows <- if ("v_medium_title_prediction_dataset_v2" %in% objects$name) dbGetQuery(con, "
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
  ") else data.frame(
    canonical_article_key = character(), article_id = integer(), medium_post_id = character(),
    url = character(), title = character(), subtitle = character(), thumbnail_url = character(),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  rows$title <- clean_text(rows$title)
  rows$subtitle <- clean_text(rows$subtitle)

  lookup <- build_thumbnail_lookup()
  rows$local_thumbnail_path <- vapply(seq_len(nrow(rows)), function(i) {
    lookup_local_thumbnail(rows$article_id[[i]], rows$medium_post_id[[i]], rows$thumbnail_url[[i]], lookup)
  }, character(1))
  rows$has_local_thumbnail <- !is.na(rows$local_thumbnail_path) & file.exists(rows$local_thumbnail_path)
  rows <- rows[rows$has_local_thumbnail, , drop = FALSE]
  row_n <- nrow(rows)
  rows$source_type <- rep("dataset", row_n)
  rows$article_lab_candidate_id <- rep(NA_character_, row_n)
  rows$candidate_created_at <- rep(NA_character_, row_n)

  rated_keys <- get_rated_keys(con)
  article_keys <- clean_text(rows$article_id)
  post_keys <- clean_text(rows$medium_post_id)
  rows$already_rated <- (!is.na(article_keys) & article_keys %in% rated_keys$article_ids) |
    (!is.na(post_keys) & post_keys %in% rated_keys$post_ids)
  rows$already_rated[is.na(rows$already_rated)] <- FALSE

  article_lab_rows <- if (dbExistsTable(con, "article_lab_title_candidates")) {
    lab <- dbGetQuery(con, "
      SELECT c.candidate_id AS article_lab_candidate_id,
        c.batch_id,
        c.created_at AS candidate_created_at,
        c.title,
        c.status,
        c.ready_for_human_rating
      FROM article_lab_title_candidates c
      WHERE c.archived = 0
        AND c.promoted = 0
        AND c.ready_for_human_rating = 1
        AND c.status = 'ready_for_human_rating'
      ORDER BY c.created_at DESC, c.candidate_id
    ")
    if (nrow(lab) > 0) {
      lab$title <- clean_text(lab$title)
      lab$already_rated <- clean_text(lab$article_lab_candidate_id) %in% rated_keys$article_lab_candidate_ids
      lab$already_rated[is.na(lab$already_rated)] <- FALSE
      data.frame(
        canonical_article_key = NA_character_,
        article_id = NA_integer_,
        medium_post_id = NA_character_,
        url = NA_character_,
        title = lab$title,
        subtitle = NA_character_,
        thumbnail_url = NA_character_,
        local_thumbnail_path = NA_character_,
        has_local_thumbnail = FALSE,
        source_type = "article_lab_generated",
        article_lab_candidate_id = lab$article_lab_candidate_id,
        candidate_created_at = lab$candidate_created_at,
        already_rated = lab$already_rated,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      data.frame()
    }
  } else {
    data.frame()
  }

  if (isTRUE(exclude_rated)) {
    rows <- rows[!rows$already_rated, , drop = FALSE]
    if (nrow(article_lab_rows) > 0) {
      article_lab_rows <- article_lab_rows[!article_lab_rows$already_rated, , drop = FALSE]
    }
  }

  if (nrow(article_lab_rows) == 0) return(rows)
  if (nrow(rows) == 0) return(article_lab_rows)

  combined <- rbind(article_lab_rows[, names(rows)], rows)
  combined
}

candidate_counts <- function(con) {
  candidates <- load_candidates(con, exclude_rated = FALSE)
  data.frame(
    total_thumbnail_candidates = nrow(candidates),
    already_rated = sum(candidates$already_rated, na.rm = TRUE),
    remaining_unrated = sum(!candidates$already_rated, na.rm = TRUE)
  )
}

append_article_lab_candidates_to_session <- function(con, session_id) {
  candidates <- load_candidates(con, exclude_rated = TRUE)
  candidates <- candidates[candidates$source_type == "article_lab_generated", , drop = FALSE]
  if (nrow(candidates) == 0) return(0L)

  existing <- dbGetQuery(con, "
    SELECT article_lab_candidate_id, queue_position
    FROM human_preview_rating_queue
    WHERE rating_session_id = ?
      AND COALESCE(status, 'pending') != 'removed_article_lab'
  ", params = list(session_id))
  existing_ids <- clean_text(existing$article_lab_candidate_id)
  keep <- !(clean_text(candidates$article_lab_candidate_id) %in% existing_ids)
  keep[is.na(keep)] <- TRUE
  additions <- candidates[keep, , drop = FALSE]
  if (nrow(additions) == 0) return(0L)

  existing_positions <- dbGetQuery(con, "
    SELECT MIN(queue_position) AS min_position
    FROM human_preview_rating_queue
    WHERE rating_session_id = ?
  ", params = list(session_id))
  min_position <- if (nrow(existing_positions) == 0 || is.na(existing_positions$min_position[[1]])) 1L else as.integer(existing_positions$min_position[[1]])
  start_position <- min_position - nrow(additions)

  additions <- additions[order(additions$candidate_created_at, additions$article_lab_candidate_id, decreasing = FALSE), , drop = FALSE]

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(additions))) {
      dbExecute(
        con,
        "INSERT INTO human_preview_rating_queue
         (rating_session_id, queue_position, article_id, medium_post_id, status, source_type, article_lab_candidate_id)
         VALUES (?, ?, ?, ?, 'pending', ?, ?)",
        params = list(
          session_id,
          start_position + i - 1L,
          NA_integer_,
          NA_character_,
          "article_lab_generated",
          additions$article_lab_candidate_id[[i]]
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  nrow(additions)
}

prune_article_lab_candidates_from_session <- function(con, session_id) {
  if (is.null(session_id) || is.na(session_id) || !nzchar(session_id)) return(0L)
  rows <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM human_preview_rating_queue q
    WHERE q.rating_session_id = ?
      AND q.source_type = 'article_lab_generated'
      AND q.status = 'pending'
      AND NOT EXISTS (
        SELECT 1
        FROM article_lab_title_candidates c
        WHERE c.candidate_id = q.article_lab_candidate_id
          AND c.ready_for_human_rating = 1
          AND c.archived = 0
          AND c.promoted = 0
          AND c.status = 'ready_for_human_rating'
      )
    ",
    params = list(session_id)
  )
  removed_n <- if (nrow(rows) == 0 || is.na(rows$n[[1]])) 0L else as.integer(rows$n[[1]])
  if (removed_n < 1L) return(0L)
  dbExecute(
    con,
    "
    UPDATE human_preview_rating_queue
    SET status = 'removed_article_lab', completed_at = COALESCE(completed_at, ?)
    WHERE rating_session_id = ?
      AND source_type = 'article_lab_generated'
      AND status = 'pending'
      AND NOT EXISTS (
        SELECT 1
        FROM article_lab_title_candidates c
        WHERE c.candidate_id = human_preview_rating_queue.article_lab_candidate_id
          AND c.ready_for_human_rating = 1
          AND c.archived = 0
          AND c.promoted = 0
          AND c.status = 'ready_for_human_rating'
      )
    ",
    params = list(now_utc(), session_id)
  )
  removed_n
}

get_dimension_rated_keys <- function(con) {
  if (!dbExistsTable(con, dimension_rating_table)) {
    return(list(canonical = character(), article_ids = character(), post_ids = character()))
  }

  manifest_filter <- if (is_dimension_v2_mode) "AND manifest_version = ?" else ""
  rated <- dbGetQuery(con, sprintf("
    SELECT DISTINCT canonical_article_key, article_id, medium_post_id
    FROM %s
    WHERE rating_mode = ?
      %s
  ", dbQuoteIdentifier(con, dimension_rating_table), manifest_filter), params = if (is_dimension_v2_mode) list(rating_mode, manifest_version) else list(rating_mode))

  canonical <- clean_text(rated$canonical_article_key)
  article_ids <- clean_text(rated$article_id)
  post_ids <- clean_text(rated$medium_post_id)
  list(
    canonical = unique(canonical[!is.na(canonical)]),
    article_ids = unique(article_ids[!is.na(article_ids)]),
    post_ids = unique(post_ids[!is.na(post_ids)])
  )
}

read_dimension_cohort <- function() {
  if (!file.exists(dimension_cohort_path)) return(data.frame())
  cohort <- read.csv(dimension_cohort_path, stringsAsFactors = FALSE, check.names = FALSE)
  for (column in c("canonical_article_key", "article_id", "medium_post_id", "title", "subtitle", "thumbnail_url", "local_image_path", "image_sha256", "thumbnail_status")) {
    if (!(column %in% names(cohort))) cohort[[column]] <- NA_character_
  }
  cohort$canonical_article_key <- clean_text(cohort$canonical_article_key)
  cohort$article_id <- clean_text(cohort$article_id)
  cohort$medium_post_id <- clean_text(cohort$medium_post_id)
  cohort$title <- clean_text(cohort$title)
  cohort$subtitle <- clean_text(cohort$subtitle)
  cohort$thumbnail_url <- clean_text(cohort$thumbnail_url)
  cohort$local_image_path <- clean_text(cohort$local_image_path)
  cohort$image_sha256 <- clean_text(cohort$image_sha256)
  cohort$thumbnail_status <- clean_text(cohort$thumbnail_status)
  cohort
}

load_dimension_candidates <- function(con, exclude_rated = TRUE) {
  if (is_dimension_v2_mode) {
    cohort <- read_dimension_cohort()
    if (nrow(cohort) == 0) return(data.frame())
    total_cohort_rows <- nrow(cohort)
    rows <- cohort[cohort$thumbnail_status == "valid", , drop = FALSE]
    if (nrow(rows) == 0) return(rows)

    rows$local_thumbnail_path <- rows$local_image_path
    rows$local_thumbnail_path_abs <- as_abs_path(rows$local_thumbnail_path)
    rows$current_image_sha256 <- vapply(rows$local_thumbnail_path_abs, file_sha256, character(1))
    rows$hash_matches_manifest <- !is.na(rows$current_image_sha256) &
      !is.na(rows$image_sha256) &
      rows$current_image_sha256 == rows$image_sha256
    rows$hash_matches_manifest[is.na(rows$hash_matches_manifest)] <- FALSE
    rows$has_local_thumbnail <- !is.na(rows$local_thumbnail_path_abs) &
      file.exists(rows$local_thumbnail_path_abs) &
      rows$hash_matches_manifest
    rows$thumbnail_status <- ifelse(rows$has_local_thumbnail, "valid", "stale_or_invalid")
    rows <- rows[rows$has_local_thumbnail, , drop = FALSE]
    if (nrow(rows) == 0) return(rows)

    rated_keys <- get_dimension_rated_keys(con)
    rows$already_dimension_rated <- (!is.na(rows$canonical_article_key) & rows$canonical_article_key %in% rated_keys$canonical) |
      (!is.na(rows$article_id) & rows$article_id %in% rated_keys$article_ids) |
      (!is.na(rows$medium_post_id) & rows$medium_post_id %in% rated_keys$post_ids)
    rows$already_dimension_rated[is.na(rows$already_dimension_rated)] <- FALSE
    rows$cohort_source <- "validated_manifest_v2"
    rows$total_cohort_rows <- total_cohort_rows

    if (isTRUE(exclude_rated)) {
      rows <- rows[!rows$already_dimension_rated, , drop = FALSE]
    }

    return(rows)
  }

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
  rows$canonical_article_key <- clean_text(rows$canonical_article_key)
  rows$article_id_text <- clean_text(rows$article_id)
  rows$medium_post_id <- clean_text(rows$medium_post_id)
  rows$title <- clean_text(rows$title)
  rows$subtitle <- clean_text(rows$subtitle)

  cohort <- read_dimension_cohort()
  cohort_source <- if (nrow(cohort) > 0) "all_cohort_csv" else "human_preview_ratings_fallback"
  total_cohort_rows <- nrow(cohort)

  if (nrow(cohort) > 0) {
    keep <- (!is.na(rows$canonical_article_key) & rows$canonical_article_key %in% cohort$canonical_article_key) |
      (!is.na(rows$article_id_text) & rows$article_id_text %in% cohort$article_id) |
      (!is.na(rows$medium_post_id) & rows$medium_post_id %in% cohort$medium_post_id)
    keep[is.na(keep)] <- FALSE
    rows <- rows[keep, , drop = FALSE]
  } else {
    if (!dbExistsTable(con, "human_preview_ratings")) {
      rows <- rows[FALSE, , drop = FALSE]
    } else {
      fallback <- dbGetQuery(con, "
        SELECT DISTINCT article_id, medium_post_id
        FROM human_preview_ratings
      ")
      fallback_article_ids <- clean_text(fallback$article_id)
      fallback_post_ids <- clean_text(fallback$medium_post_id)
      total_cohort_rows <- nrow(fallback)
      keep <- (!is.na(rows$article_id_text) & rows$article_id_text %in% fallback_article_ids) |
        (!is.na(rows$medium_post_id) & rows$medium_post_id %in% fallback_post_ids)
      keep[is.na(keep)] <- FALSE
      rows <- rows[keep, , drop = FALSE]
    }
  }

  lookup <- build_thumbnail_lookup()
  rows$local_thumbnail_path <- vapply(seq_len(nrow(rows)), function(i) {
    lookup_local_thumbnail(rows$article_id[[i]], rows$medium_post_id[[i]], rows$thumbnail_url[[i]], lookup)
  }, character(1))
  rows$has_local_thumbnail <- !is.na(rows$local_thumbnail_path) & file.exists(rows$local_thumbnail_path)
  rows$thumbnail_status <- ifelse(rows$has_local_thumbnail, "valid", "stale_or_invalid")
  rows <- rows[rows$has_local_thumbnail, , drop = FALSE]

  rated_keys <- get_dimension_rated_keys(con)
  rows$already_dimension_rated <- (!is.na(rows$canonical_article_key) & rows$canonical_article_key %in% rated_keys$canonical) |
    (!is.na(rows$article_id_text) & rows$article_id_text %in% rated_keys$article_ids) |
    (!is.na(rows$medium_post_id) & rows$medium_post_id %in% rated_keys$post_ids)
  rows$already_dimension_rated[is.na(rows$already_dimension_rated)] <- FALSE
  rows$cohort_source <- cohort_source
  rows$total_cohort_rows <- total_cohort_rows

  if (isTRUE(exclude_rated)) {
    rows <- rows[!rows$already_dimension_rated, , drop = FALSE]
  }

  rows
}

dimension_row_key <- function(canonical_article_key, article_id, medium_post_id) {
  canonical_key <- clean_text(canonical_article_key)
  article_key <- clean_text(article_id)
  post_key <- clean_text(medium_post_id)
  if (!is.na(canonical_key)) return(paste0("canonical:", canonical_key))
  if (!is.na(article_key)) return(paste0("article:", article_key))
  if (!is.na(post_key)) return(paste0("post:", post_key))
  NA_character_
}

dimension_row_keys <- function(rows) {
  vapply(seq_len(nrow(rows)), function(i) {
    dimension_row_key(rows$canonical_article_key[[i]], rows$article_id[[i]], rows$medium_post_id[[i]])
  }, character(1))
}

mark_invalid_dimension_pass_queue_items <- function(con, active_dimension, candidates) {
  valid_keys <- dimension_row_keys(candidates)
  valid_keys <- valid_keys[!is.na(valid_keys)]

  pending <- dbGetQuery(con, "
    SELECT queue_position, article_id, medium_post_id, canonical_article_key
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ?
      AND active_dimension = ?
      AND status = 'pending'
  ", params = list(rating_mode, active_dimension))
  if (nrow(pending) == 0) return(0L)

  pending_keys <- dimension_row_keys(pending)
  invalid <- is.na(pending_keys) | !(pending_keys %in% valid_keys)
  invalid[is.na(invalid)] <- TRUE
  invalid_rows <- pending[invalid, , drop = FALSE]
  if (nrow(invalid_rows) == 0) return(0L)

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(invalid_rows))) {
      dbExecute(
        con,
        "UPDATE human_preview_dimension_pass_queue
         SET status = 'ignored_invalid_thumbnail', completed_at = ?
         WHERE rating_mode = ?
           AND active_dimension = ?
           AND queue_position = ?
           AND status = 'pending'",
        params = list(now_utc(), rating_mode, active_dimension, invalid_rows$queue_position[[i]])
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  nrow(invalid_rows)
}

top_up_dimension_pass_queue <- function(con, active_dimension, candidates, target_n = Inf) {
  if (nrow(candidates) == 0) return(0L)

  existing <- dbGetQuery(con, "
    SELECT queue_position, article_id, medium_post_id, canonical_article_key, status
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ?
      AND active_dimension = ?
  ", params = list(rating_mode, active_dimension))

  desired_n <- if (is.infinite(target_n)) nrow(candidates) else min(as.integer(target_n), nrow(candidates))
  active_status <- existing$status %in% c("pending", "rated", "skipped")
  active_status[is.na(active_status)] <- FALSE
  needed <- desired_n - sum(active_status)
  if (needed <= 0) return(0L)

  existing_keys <- dimension_row_keys(existing)
  existing_keys <- existing_keys[!is.na(existing_keys)]
  candidate_keys <- dimension_row_keys(candidates)
  available <- !is.na(candidate_keys) & !(candidate_keys %in% existing_keys)
  available[is.na(available)] <- FALSE
  if (!any(available)) return(0L)

  seed <- sum(utf8ToInt(active_dimension)) + 1009L
  set.seed(seed)
  additions <- candidates[available, , drop = FALSE]
  additions <- additions[sample.int(nrow(additions)), , drop = FALSE]
  additions <- head(additions, min(needed, nrow(additions)))

  max_position <- if (nrow(existing) == 0 || all(is.na(existing$queue_position))) 0L else max(existing$queue_position, na.rm = TRUE)

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(additions))) {
      completed <- dimension_has_value(
        con,
        additions$article_id[[i]],
        additions$medium_post_id[[i]],
        additions$canonical_article_key[[i]],
        active_dimension
      )
      dbExecute(
        con,
        "INSERT OR IGNORE INTO human_preview_dimension_pass_queue
         (rating_mode, active_dimension, queue_position, article_id, medium_post_id,
          canonical_article_key, status, completed_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          rating_mode,
          active_dimension,
          max_position + i,
          additions$article_id[[i]],
          additions$medium_post_id[[i]],
          additions$canonical_article_key[[i]],
          if (completed) "rated" else "pending",
          if (completed) now_utc() else NA_character_
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  nrow(additions)
}

dimension_candidate_counts <- function(con) {
  candidates <- load_dimension_candidates(con, exclude_rated = FALSE)
  total_cohort_rows <- if (nrow(candidates) > 0) candidates$total_cohort_rows[[1]] else nrow(read_dimension_cohort())
  if (total_cohort_rows == 0 && dbExistsTable(con, "human_preview_ratings")) {
    total_cohort_rows <- dbGetQuery(con, "SELECT COUNT(DISTINCT COALESCE(CAST(article_id AS TEXT), medium_post_id)) AS n FROM human_preview_ratings")$n[[1]]
  }
  status <- if (dbExistsTable(con, "human_preview_dimension_pass_queue")) {
    dbGetQuery(con, "
      SELECT
        active_dimension,
        COUNT(*) AS total,
        SUM(CASE WHEN status IN ('rated', 'skipped') THEN 1 ELSE 0 END) AS completed,
        SUM(CASE WHEN status = 'pending' THEN 1 ELSE 0 END) AS pending
      FROM human_preview_dimension_pass_queue
      WHERE rating_mode = ?
      GROUP BY active_dimension
    ", params = list(rating_mode))
  } else {
    data.frame(active_dimension = character(), total = integer(), completed = integer(), pending = integer())
  }
  completed_dimensions <- sum(vapply(active_dimension_fields, function(field) {
    row <- status[status$active_dimension == field, , drop = FALSE]
    nrow(row) > 0 && !is.na(row$pending[[1]]) && row$pending[[1]] == 0 && row$total[[1]] > 0
  }, logical(1)))
  data.frame(
    total_cohort_rows = total_cohort_rows,
    usable_local_thumbnails = nrow(candidates),
    completed_dimensions = completed_dimensions,
    total_dimensions = length(active_dimension_fields),
    cohort_source = if (nrow(candidates) > 0) candidates$cohort_source[[1]] else if (file.exists(dimension_cohort_path)) "all_cohort_csv" else "human_preview_ratings_fallback"
  )
}

create_new_session <- function(con, target_n = Inf) {
  seed <- sample.int(.Machine$integer.max, 1)
  session_id <- paste0("preview_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", seed)
  candidates <- load_candidates(con, exclude_rated = TRUE)
  if (nrow(candidates) == 0) stop("No unrated candidate articles with local thumbnails were found.", call. = FALSE)

  set.seed(seed)
  article_lab_candidates <- candidates[candidates$source_type == "article_lab_generated", , drop = FALSE]
  dataset_candidates <- candidates[candidates$source_type != "article_lab_generated", , drop = FALSE]
  if (nrow(article_lab_candidates) > 0) {
    article_lab_candidates <- article_lab_candidates[order(article_lab_candidates$candidate_created_at, article_lab_candidates$article_lab_candidate_id, decreasing = FALSE), , drop = FALSE]
  }
  shuffled_dataset <- if (nrow(dataset_candidates) > 0) dataset_candidates[sample.int(nrow(dataset_candidates)), , drop = FALSE] else dataset_candidates
  shuffled <- if (nrow(article_lab_candidates) > 0) rbind(article_lab_candidates, shuffled_dataset) else shuffled_dataset
  selected_n <- min(target_n, nrow(shuffled))
  selected_n <- as.integer(selected_n)
  selected <- head(shuffled, selected_n)

  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      "INSERT INTO human_rating_sessions
       (rating_session_id, created_at, interface_version, rating_mode, queue_seed, target_n, notes)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(session_id, now_utc(), interface_version, rating_mode, seed, selected_n, "Mode: unrated thumbnails only")
    )

    for (i in seq_len(nrow(selected))) {
      dbExecute(
        con,
        "INSERT INTO human_preview_rating_queue
         (rating_session_id, queue_position, article_id, medium_post_id, status, source_type, article_lab_candidate_id)
         VALUES (?, ?, ?, ?, 'pending', ?, ?)",
        params = list(
          session_id,
          i,
          selected$article_id[[i]],
          selected$medium_post_id[[i]],
          selected$source_type[[i]],
          selected$article_lab_candidate_id[[i]]
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

resume_or_create_session <- function(con, target_n = Inf) {
  mark_duplicate_pending_queue_items(con)

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

  if (nrow(existing) > 0) {
    session_id <- existing$rating_session_id[[1]]
    prune_article_lab_candidates_from_session(con, session_id)
    append_article_lab_candidates_to_session(con, session_id)
    session_id
  } else {
    create_new_session(con, target_n)
  }
}

load_current_item <- function(con, session_id) {
  item <- dbGetQuery(con, "
    SELECT rating_session_id, queue_position, article_id, medium_post_id, status, shown_at, completed_at, source_type, article_lab_candidate_id
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

  source_type <- first_value(item, "source_type", "dataset")
  if (identical(source_type, "article_lab_generated")) {
    details <- dbGetQuery(con, "
      SELECT candidate_id AS article_lab_candidate_id, batch_id, created_at, title, status
      FROM article_lab_title_candidates
      WHERE candidate_id = ?
      LIMIT 1
    ", params = list(item$article_lab_candidate_id[[1]]))
    if (nrow(details) == 0) return(NULL)

    details$title <- clean_text(details$title)
    details$subtitle <- NA_character_
    details$thumbnail_url <- NA_character_
    details$local_thumbnail_path <- NA_character_
    details$url <- NA_character_
    details$canonical_article_key <- NA_character_
    details$article_id <- NA_integer_
    details$medium_post_id <- NA_character_
    details$thumbnail_status <- "article_lab_title_only"
    return(cbind(item, details[1, , drop = FALSE]))
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
      SUM(CASE WHEN status = 'pending' THEN 1 ELSE 0 END) AS pending,
      SUM(CASE WHEN status = 'ignored_duplicate' THEN 1 ELSE 0 END) AS ignored_duplicate
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
        human_feed_success_potential, human_feed_success_note, skipped, source_type, article_lab_candidate_id,
        shown_at, rated_at, seconds_spent)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
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
        first_value(item, "source_type", "dataset"),
        first_value(item, "article_lab_candidate_id"),
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

    if (identical(first_value(item, "source_type", "dataset"), "article_lab_generated")) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates
         SET status = ?, ready_for_human_rating = 0
         WHERE candidate_id = ?",
        params = list(
          if (isTRUE(skipped)) "human_skipped" else "human_rated",
          first_value(item, "article_lab_candidate_id")
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  invisible(TRUE)
}

undo_previous_rating <- function(con, session_id) {
  previous <- dbGetQuery(con, "
    SELECT id, queue_position, source_type, article_lab_candidate_id
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
    if (identical(first_value(previous, "source_type", "dataset"), "article_lab_generated")) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates
         SET status = 'ready_for_human_rating', ready_for_human_rating = 1
         WHERE candidate_id = ?",
        params = list(first_value(previous, "article_lab_candidate_id"))
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  TRUE
}

dimension_has_value <- function(con, article_id, medium_post_id, canonical_article_key, active_dimension) {
  if (!(active_dimension %in% dimension_fields)) return(FALSE)
  manifest_filter <- if (is_dimension_v2_mode) "AND manifest_version = ?" else ""
  rows <- dbGetQuery(
    con,
    sprintf("
      SELECT %s AS value
      FROM %s
      WHERE rating_mode = ?
        %s
        AND (
          (canonical_article_key IS NOT NULL AND canonical_article_key = ?)
          OR (canonical_article_key IS NULL AND article_id IS NOT NULL AND article_id = ?)
          OR (canonical_article_key IS NULL AND article_id IS NULL AND medium_post_id IS NOT NULL AND medium_post_id = ?)
        )
      ORDER BY updated_at DESC, rated_at DESC, id DESC
      LIMIT 1
    ", dbQuoteIdentifier(con, active_dimension), dbQuoteIdentifier(con, dimension_rating_table), manifest_filter),
    params = if (is_dimension_v2_mode) {
      list(rating_mode, manifest_version, canonical_article_key, article_id, medium_post_id)
    } else {
      list(rating_mode, canonical_article_key, article_id, medium_post_id)
    }
  )
  nrow(rows) > 0 && !is.na(rows$value[[1]]) && nzchar(as.character(rows$value[[1]]))
}

ensure_dimension_pass_queue <- function(con, active_dimension, target_n = Inf) {
  if (!(active_dimension %in% dimension_fields)) stop("Unknown dimension: ", active_dimension, call. = FALSE)
  candidates <- load_dimension_candidates(con, exclude_rated = FALSE)
  if (nrow(candidates) == 0) stop("No dimension-rating candidate articles with valid local thumbnails were found.", call. = FALSE)

  existing <- dbGetQuery(con, "
    SELECT COUNT(*) AS n
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ? AND active_dimension = ?
  ", params = list(rating_mode, active_dimension))
  if (existing$n[[1]] > 0) {
    mark_invalid_dimension_pass_queue_items(con, active_dimension, candidates)
    top_up_dimension_pass_queue(con, active_dimension, candidates, target_n = target_n)
    return(invisible(FALSE))
  }

  seed <- sum(utf8ToInt(active_dimension)) + 1009L
  set.seed(seed)
  shuffled <- candidates[sample.int(nrow(candidates)), , drop = FALSE]
  selected_n <- min(target_n, nrow(shuffled))
  selected <- head(shuffled, as.integer(selected_n))

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(selected))) {
      completed <- dimension_has_value(
        con,
        selected$article_id[[i]],
        selected$medium_post_id[[i]],
        selected$canonical_article_key[[i]],
        active_dimension
      )
      dbExecute(
        con,
        "INSERT OR IGNORE INTO human_preview_dimension_pass_queue
         (rating_mode, active_dimension, queue_position, article_id, medium_post_id,
          canonical_article_key, status, completed_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          rating_mode,
          active_dimension,
          i,
          selected$article_id[[i]],
          selected$medium_post_id[[i]],
          selected$canonical_article_key[[i]],
          if (completed) "rated" else "pending",
          if (completed) now_utc() else NA_character_
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  invisible(TRUE)
}

ensure_dimension_pass_queues <- function(con, target_n = Inf) {
  for (field in active_dimension_fields) ensure_dimension_pass_queue(con, field, target_n = target_n)
  invisible(TRUE)
}

dimension_queue_counts <- function(con, active_dimension) {
  ensure_dimension_pass_queue(con, active_dimension, target_n = default_target_n)
  dbGetQuery(con, "
    SELECT
      COUNT(*) AS total,
      SUM(CASE WHEN status IN ('rated', 'skipped') THEN 1 ELSE 0 END) AS completed,
      SUM(CASE WHEN status = 'pending' THEN 1 ELSE 0 END) AS pending,
      SUM(CASE WHEN status = 'skipped' THEN 1 ELSE 0 END) AS skipped
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ? AND active_dimension = ?
  ", params = list(rating_mode, active_dimension))
}

dimension_pass_status <- function(con) {
  ensure_dimension_pass_queues(con, target_n = default_target_n)
  counts <- do.call(rbind, lapply(active_dimension_fields, function(field) {
    c <- dimension_queue_counts(con, field)
    data.frame(
      active_dimension = field,
      total = ifelse(is.na(c$total[[1]]), 0L, c$total[[1]]),
      completed = ifelse(is.na(c$completed[[1]]), 0L, c$completed[[1]]),
      pending = ifelse(is.na(c$pending[[1]]), 0L, c$pending[[1]]),
      skipped = ifelse(is.na(c$skipped[[1]]), 0L, c$skipped[[1]])
    )
  }))
  counts$dimension_index <- match(counts$active_dimension, active_dimension_fields)
  counts
}

first_incomplete_dimension <- function(con) {
  status <- dimension_pass_status(con)
  incomplete <- status[status$pending > 0, , drop = FALSE]
  if (nrow(incomplete) == 0) return(NA_character_)
  incomplete$active_dimension[[which.min(incomplete$dimension_index)]]
}

next_incomplete_dimension_after <- function(con, active_dimension) {
  status <- dimension_pass_status(con)
  current_index <- match(active_dimension, active_dimension_fields)
  incomplete <- status[status$pending > 0 & status$dimension_index > current_index, , drop = FALSE]
  if (nrow(incomplete) == 0) return(NA_character_)
  incomplete$active_dimension[[which.min(incomplete$dimension_index)]]
}


load_current_dimension_item <- function(con, active_dimension) {
  ensure_dimension_pass_queue(con, active_dimension, target_n = default_target_n)
  item <- dbGetQuery(con, "
    SELECT rating_mode, active_dimension, queue_position, article_id, medium_post_id,
      canonical_article_key, status, shown_at, completed_at, seconds_spent
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ?
      AND active_dimension = ?
      AND status = 'pending'
    ORDER BY queue_position
    LIMIT 1
  ", params = list(rating_mode, active_dimension))
  if (nrow(item) == 0) return(NULL)

  if (is.na(item$shown_at[[1]]) || !nzchar(item$shown_at[[1]])) {
    item$shown_at[[1]] <- now_utc()
    dbExecute(
      con,
      "UPDATE human_preview_dimension_pass_queue
       SET shown_at = ?
       WHERE rating_mode = ? AND active_dimension = ? AND queue_position = ?",
      params = list(item$shown_at[[1]], rating_mode, active_dimension, item$queue_position[[1]])
    )
  }

  if (is_dimension_v2_mode) {
    candidates <- load_dimension_candidates(con, exclude_rated = FALSE)
    if (nrow(candidates) == 0) return(NULL)
    candidate_keys <- dimension_row_keys(candidates)
    item_key <- dimension_row_key(item$canonical_article_key[[1]], item$article_id[[1]], item$medium_post_id[[1]])
    match_index <- match(item_key, candidate_keys)
    if (is.na(match_index)) return(NULL)
    candidate <- candidates[match_index, , drop = FALSE]
    current_item <- data.frame(
      rating_mode = item$rating_mode[[1]],
      active_dimension = item$active_dimension[[1]],
      queue_position = item$queue_position[[1]],
      article_id = item$article_id[[1]],
      medium_post_id = item$medium_post_id[[1]],
      canonical_article_key = item$canonical_article_key[[1]],
      status = item$status[[1]],
      shown_at = item$shown_at[[1]],
      completed_at = item$completed_at[[1]],
      seconds_spent = item$seconds_spent[[1]],
      title = candidate$title[[1]],
      subtitle = candidate$subtitle[[1]],
      thumbnail_url = candidate$thumbnail_url[[1]],
      local_image_path = candidate$local_image_path[[1]],
      local_thumbnail_path = candidate$local_thumbnail_path[[1]],
      local_thumbnail_path_abs = candidate$local_thumbnail_path_abs[[1]],
      image_sha256 = candidate$image_sha256[[1]],
      current_image_sha256 = candidate$current_image_sha256[[1]],
      hash_matches_manifest = candidate$hash_matches_manifest[[1]],
      thumbnail_status = candidate$thumbnail_status[[1]],
      cohort_source = if ("cohort_source" %in% names(candidate)) candidate$cohort_source[[1]] else NA_character_,
      render_source = "validated_manifest_v2",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    info <- v2_render_info(current_item)
    message(sprintf(
      "dimensions_v2 render audit | queue_position=%s | article_id=%s | medium_post_id=%s | canonical_article_key=%s | local_thumbnail_path=%s | manifest image_sha256=%s | rendered file path=%s | rendered file sha256=%s | hashes_match=%s",
      first_value(current_item, "queue_position"),
      first_value(current_item, "article_id"),
      first_value(current_item, "medium_post_id"),
      first_value(current_item, "canonical_article_key"),
      info$path,
      info$manifest_hash,
      info$path_abs,
      info$rendered_hash,
      isTRUE(info$valid)
    ))
    return(current_item)
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
    WHERE canonical_article_key = ?
       OR article_id = ?
       OR (medium_post_id IS NOT NULL AND medium_post_id = ?)
    LIMIT 1
  ", params = list(item$canonical_article_key[[1]], item$article_id[[1]], item$medium_post_id[[1]]))

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
  details$thumbnail_status <- if (
    !is.na(details$local_thumbnail_path[[1]]) &&
      file.exists(details$local_thumbnail_path[[1]])
  ) "valid" else "stale_or_invalid"

  cbind(item, details[1, , drop = FALSE])
}

find_dimension_rating_id <- function(con, item) {
  manifest_filter <- if (is_dimension_v2_mode) "AND manifest_version = ?" else ""
  rows <- dbGetQuery(con, sprintf("
    SELECT id, human_dimension_note
    FROM %s
    WHERE rating_mode = ?
      %s
      AND (
        (canonical_article_key IS NOT NULL AND canonical_article_key = ?)
        OR (canonical_article_key IS NULL AND article_id IS NOT NULL AND article_id = ?)
        OR (canonical_article_key IS NULL AND article_id IS NULL AND medium_post_id IS NOT NULL AND medium_post_id = ?)
      )
    ORDER BY updated_at DESC, rated_at DESC, id DESC
    LIMIT 1
  ", dbQuoteIdentifier(con, dimension_rating_table), manifest_filter), params = if (is_dimension_v2_mode) {
    list(rating_mode, manifest_version, item$canonical_article_key[[1]], item$article_id[[1]], item$medium_post_id[[1]])
  } else {
    list(rating_mode, item$canonical_article_key[[1]], item$article_id[[1]], item$medium_post_id[[1]])
  })
  if (nrow(rows) == 0) NULL else rows[1, , drop = FALSE]
}

update_dimension_note <- function(existing_note, active_dimension, note) {
  note_value <- clean_text(note)
  if (length(note_value) == 0 || is.na(note_value[[1]])) return(existing_note)
  existing <- clean_text(existing_note)
  lines <- if (length(existing) == 0 || is.na(existing[[1]])) character() else strsplit(existing[[1]], "\n", fixed = TRUE)[[1]]
  prefix <- paste0("[", active_dimension, "]")
  lines <- lines[!startsWith(lines, prefix)]
  paste(c(lines, paste(prefix, note_value[[1]])), collapse = "\n")
}

save_current_dimension_rating <- function(con, item, active_dimension, value = NULL, note = "", skipped = FALSE, shown_started_at = Sys.time()) {
  if (is.null(item) || nrow(item) == 0) return(invisible(FALSE))
  if (!(active_dimension %in% dimension_fields)) return(invisible(FALSE))
  rated_at <- now_utc()
  seconds_spent <- as.numeric(difftime(Sys.time(), shown_started_at, units = "secs"))
  seconds_spent <- max(0, seconds_spent)
  rating_value <- if (isTRUE(skipped)) NA else value
  if (!isTRUE(skipped) && active_dimension %in% dimension_numeric_fields) {
    rating_value <- suppressWarnings(as.integer(rating_value))
    if (is.na(rating_value) || rating_value < 1L || rating_value > 5L) return(invisible(FALSE))
  }
  if (!isTRUE(skipped) && active_dimension == "ai_low_effort_flag") {
    if (!(rating_value %in% c("yes", "unsure", "no"))) return(invisible(FALSE))
  }
  shown_subtitle <- displayed_subtitle_for_field(item, active_dimension)
  shown_thumbnail_path <- displayed_thumbnail_path_for_field(item, active_dimension)
  shown_image_sha256 <- if (is_dimension_v2_mode) {
    if (active_dimension %in% text_only_dimension_fields) {
      NA_character_
    } else {
    info <- v2_render_info(item)
    if (!isTRUE(info$valid)) return(invisible(FALSE))
    info$rendered_hash
    }
  } else {
    NA_character_
  }

  dbBegin(con)
  tryCatch({
    existing <- find_dimension_rating_id(con, item)
    existing_note <- if (is.null(existing)) NA_character_ else existing$human_dimension_note[[1]]
    note_value <- update_dimension_note(existing_note, active_dimension, note)
    if (length(note_value) == 0 || is.na(note_value[[1]])) note_value <- NA_character_

    if (is.null(existing)) {
      if (is_dimension_v2_mode) {
        dbExecute(
          con,
          sprintf(
            "INSERT INTO human_preview_dimension_ratings_v2
             (rating_session_id, queue_position, article_id, medium_post_id, canonical_article_key,
              interface_version, rating_mode, manifest_version, shown_title, shown_subtitle,
              shown_thumbnail_path, shown_image_sha256, %s, human_dimension_note, skipped,
              shown_at, rated_at, seconds_spent, created_at, updated_at)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            dbQuoteIdentifier(con, active_dimension)
          ),
          params = list(
            NA_character_,
            item$queue_position[[1]],
            item$article_id[[1]],
            item$medium_post_id[[1]],
            item$canonical_article_key[[1]],
            interface_version,
            rating_mode,
            manifest_version,
            item$title[[1]],
            shown_subtitle,
            shown_thumbnail_path,
            shown_image_sha256,
            rating_value,
            note_value[[1]],
            if (isTRUE(skipped)) 1L else 0L,
            item$shown_at[[1]],
            rated_at,
            seconds_spent,
            rated_at,
            rated_at
          )
        )
      } else {
        dbExecute(
          con,
          sprintf(
            "INSERT INTO human_preview_dimension_ratings
             (rating_session_id, queue_position, article_id, medium_post_id, canonical_article_key,
              interface_version, rating_mode, shown_title, shown_subtitle, shown_thumbnail_path,
              %s, human_dimension_note, skipped, shown_at, rated_at, seconds_spent, created_at, updated_at)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            dbQuoteIdentifier(con, active_dimension)
          ),
          params = list(
            NA_character_,
            item$queue_position[[1]],
            item$article_id[[1]],
            item$medium_post_id[[1]],
            item$canonical_article_key[[1]],
            interface_version,
            rating_mode,
            item$title[[1]],
            shown_subtitle,
            shown_thumbnail_path,
            rating_value,
            note_value[[1]],
            if (isTRUE(skipped)) 1L else 0L,
            item$shown_at[[1]],
            rated_at,
            seconds_spent,
            rated_at,
            rated_at
          )
        )
      }
    } else {
      if (is_dimension_v2_mode) {
        dbExecute(
          con,
          sprintf(
            "UPDATE human_preview_dimension_ratings_v2
             SET queue_position = ?, manifest_version = ?, shown_title = ?, shown_subtitle = ?,
               shown_thumbnail_path = ?, shown_image_sha256 = ?, %s = ?,
               human_dimension_note = ?, skipped = ?, shown_at = ?, rated_at = ?,
               seconds_spent = ?, updated_at = ?
             WHERE id = ?",
            dbQuoteIdentifier(con, active_dimension)
          ),
          params = list(
            item$queue_position[[1]],
            manifest_version,
            item$title[[1]],
            shown_subtitle,
            shown_thumbnail_path,
            shown_image_sha256,
            rating_value,
            note_value[[1]],
            if (isTRUE(skipped)) 1L else 0L,
            item$shown_at[[1]],
            rated_at,
            seconds_spent,
            rated_at,
            existing$id[[1]]
          )
        )
      } else {
        dbExecute(
          con,
          sprintf(
            "UPDATE human_preview_dimension_ratings
             SET queue_position = ?, shown_title = ?, shown_subtitle = ?, shown_thumbnail_path = ?,
               %s = ?, human_dimension_note = ?, skipped = ?,
               shown_at = ?, rated_at = ?, seconds_spent = ?, updated_at = ?
             WHERE id = ?",
            dbQuoteIdentifier(con, active_dimension)
          ),
          params = list(
            item$queue_position[[1]],
            item$title[[1]],
            shown_subtitle,
            shown_thumbnail_path,
            rating_value,
            note_value[[1]],
            if (isTRUE(skipped)) 1L else 0L,
            item$shown_at[[1]],
            rated_at,
            seconds_spent,
            rated_at,
            existing$id[[1]]
          )
        )
      }
    }

    dbExecute(
      con,
      "UPDATE human_preview_dimension_pass_queue
       SET status = ?, completed_at = ?, seconds_spent = ?
       WHERE rating_mode = ? AND active_dimension = ? AND queue_position = ?",
      params = list(
        if (isTRUE(skipped)) "skipped" else "rated",
        rated_at,
        seconds_spent,
        rating_mode,
        active_dimension,
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

undo_previous_dimension_rating <- function(con, active_dimension) {
  if (!(active_dimension %in% dimension_fields)) return(FALSE)
  previous <- dbGetQuery(con, "
    SELECT queue_position, article_id, medium_post_id, canonical_article_key
    FROM human_preview_dimension_pass_queue
    WHERE rating_mode = ?
      AND active_dimension = ?
      AND status IN ('rated', 'skipped')
    ORDER BY completed_at DESC, queue_position DESC
    LIMIT 1
  ", params = list(rating_mode, active_dimension))
  if (nrow(previous) == 0) return(FALSE)

  dbBegin(con)
  tryCatch({
    pseudo_item <- previous
    rating_row <- find_dimension_rating_id(con, pseudo_item)
    if (!is.null(rating_row)) {
      dbExecute(
        con,
        sprintf(
          "UPDATE %s
           SET %s = NULL, updated_at = ?
           WHERE id = ?",
          dbQuoteIdentifier(con, dimension_rating_table),
          dbQuoteIdentifier(con, active_dimension)
        ),
        params = list(now_utc(), rating_row$id[[1]])
      )
      dbExecute(
        con,
        sprintf("DELETE FROM %s
         WHERE id = ?
           AND personal_click_appeal IS NULL
           AND title_hook_strength IS NULL
           AND visual_hook IS NULL
           AND emotional_pull_preview IS NULL
           AND ai_low_effort_flag IS NULL
           AND NULLIF(TRIM(COALESCE(human_dimension_note, '')), '') IS NULL", dbQuoteIdentifier(con, dimension_rating_table)),
        params = list(rating_row$id[[1]])
      )
    }
    dbExecute(
      con,
      "UPDATE human_preview_dimension_pass_queue
       SET status = 'pending', shown_at = NULL, completed_at = NULL, seconds_spent = NULL
       WHERE rating_mode = ? AND active_dimension = ? AND queue_position = ?",
      params = list(rating_mode, active_dimension, previous$queue_position[[1]])
    )
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  TRUE
}
