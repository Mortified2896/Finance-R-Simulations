article_lab_update_subtitle_notes <- function(con, notes_updates) {
  if (length(notes_updates) == 0) return(0L)
  updated_n <- 0L
  dbBegin(con)
  tryCatch({
    for (entry in notes_updates) {
      subtitle_id <- clean_text(entry$subtitle_id)
      if (length(subtitle_id) == 0 || is.na(subtitle_id[[1]])) next
      dbExecute(
        con,
        "UPDATE article_lab_subtitle_candidates SET notes = ? WHERE subtitle_id = ?",
        params = list(clean_text(entry$notes), subtitle_id[[1]])
      )
      updated_n <- updated_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  updated_n
}

article_lab_update_thumbnail_notes <- function(con, notes_updates) {
  if (length(notes_updates) == 0) return(0L)
  updated_n <- 0L
  dbBegin(con)
  tryCatch({
    for (entry in notes_updates) {
      thumbnail_id <- clean_text(entry$thumbnail_id)
      if (length(thumbnail_id) == 0 || is.na(thumbnail_id[[1]])) next
      dbExecute(
        con,
        "UPDATE article_lab_thumbnail_candidates SET notes = ? WHERE thumbnail_id = ?",
        params = list(clean_text(entry$notes), thumbnail_id[[1]])
      )
      updated_n <- updated_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  updated_n
}

article_lab_sync_title_subtitle_stage <- function(con, candidate_ids) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) return(invisible(NULL))

  for (candidate_id in candidate_ids) {
    approved_n <- dbGetQuery(
      con,
      "SELECT COUNT(*) AS approved_n FROM article_lab_subtitle_candidates WHERE candidate_id = ? AND status = 'approved'",
      params = list(candidate_id)
    )$approved_n[[1]] %||% 0L
    if (approved_n > 0) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = 'ready_for_thumbnail', promoted = 0, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?",
        params = list(candidate_id)
      )
    } else {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = 'approved_for_subtitle', promoted = 1, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?",
        params = list(candidate_id)
      )
    }
  }
  invisible(NULL)
}

article_lab_sync_title_thumbnail_stage <- function(con, candidate_ids) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) return(invisible(NULL))

  for (candidate_id in candidate_ids) {
    approved_thumbnail_n <- if (dbExistsTable(con, "article_lab_thumbnail_candidates")) {
      dbGetQuery(
        con,
        "SELECT COUNT(*) AS approved_n FROM article_lab_thumbnail_candidates WHERE candidate_id = ? AND status = 'approved'",
        params = list(candidate_id)
      )$approved_n[[1]] %||% 0L
    } else {
      0L
    }
    approved_subtitle_n <- dbGetQuery(
      con,
      "SELECT COUNT(*) AS approved_n FROM article_lab_subtitle_candidates WHERE candidate_id = ? AND status = 'approved'",
      params = list(candidate_id)
    )$approved_n[[1]] %||% 0L

    if (approved_thumbnail_n > 0) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = 'ready_for_outline', promoted = 0, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?",
        params = list(candidate_id)
      )
    } else if (approved_subtitle_n > 0) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = 'ready_for_thumbnail', promoted = 0, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?",
        params = list(candidate_id)
      )
    } else {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = 'approved_for_subtitle', promoted = 1, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?",
        params = list(candidate_id)
      )
    }
  }
  invisible(NULL)
}

article_lab_insert_outline_drafts <- function(con, outline_rows) {
  if (nrow(outline_rows) == 0) return(0L)
  inserted_n <- 0L
  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(outline_rows))) {
      existing <- dbGetQuery(
        con,
        "SELECT outline_id FROM article_lab_outlines WHERE thumbnail_id = ? AND status IN ('draft', 'approved') LIMIT 1",
        params = list(outline_rows$thumbnail_id[[i]])
      )
      timestamp <- outline_rows$created_at[[i]] %||% now_utc()
      if (nrow(existing) > 0) {
        dbExecute(
          con,
          "UPDATE article_lab_outlines
           SET updated_at = ?, outline_text = ?, status = 'draft', notes = NULL, model = ?, reasoning_effort = ?, reasoning_mode = ?, generation_mode = ?, raw_json = ?, approved_at = NULL
           WHERE outline_id = ?",
          params = list(
            timestamp, outline_rows$outline_text[[i]], outline_rows$model[[i]], outline_rows$reasoning_effort[[i]], outline_rows$reasoning_mode[[i]], outline_rows$generation_mode[[i]], outline_rows$raw_json[[i]], existing$outline_id[[1]]
          )
        )
      } else {
        dbExecute(
          con,
          "INSERT INTO article_lab_outlines
           (outline_id, thumbnail_id, subtitle_id, candidate_id, batch_id, created_at, updated_at, outline_text, status, notes, model, reasoning_effort, reasoning_mode, generation_mode, raw_json, approved_at)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'draft', NULL, ?, ?, ?, ?, ?, NULL)",
          params = list(
            article_lab_outline_id(outline_rows$thumbnail_id[[i]]),
            outline_rows$thumbnail_id[[i]], outline_rows$subtitle_id[[i]], outline_rows$candidate_id[[i]], outline_rows$batch_id[[i]],
            timestamp, timestamp, outline_rows$outline_text[[i]], outline_rows$model[[i]], outline_rows$reasoning_effort[[i]], outline_rows$reasoning_mode[[i]], outline_rows$generation_mode[[i]], outline_rows$raw_json[[i]]
          )
        )
      }
      inserted_n <- inserted_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  inserted_n
}

article_lab_update_outlines <- function(con, outline_updates) {
  if (length(outline_updates) == 0) return(0L)
  updated_n <- 0L
  timestamp <- now_utc()
  dbBegin(con)
  tryCatch({
    for (entry in outline_updates) {
      outline_id <- clean_text(entry$outline_id)
      if (length(outline_id) == 0 || is.na(outline_id[[1]])) next
      outline_text <- article_lab_input_multiline(entry$outline_text)
      if (length(outline_text) == 0 || is.na(outline_text[[1]])) next
      dbExecute(
        con,
        "UPDATE article_lab_outlines SET outline_text = ?, notes = ?, updated_at = ? WHERE outline_id = ? AND status IN ('draft', 'approved')",
        params = list(outline_text[[1]], article_lab_input_string(entry$notes) %||% NA_character_, timestamp, outline_id[[1]])
      )
      updated_n <- updated_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  updated_n
}

article_lab_approve_outlines <- function(con, outline_ids) {
  outline_ids <- clean_text(outline_ids)
  outline_ids <- unique(outline_ids[!is.na(outline_ids)])
  if (length(outline_ids) == 0) return(list(approved_n = 0L, candidate_ids = character()))
  placeholders <- paste(rep("?", length(outline_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT outline_id, candidate_id, batch_id FROM article_lab_outlines WHERE outline_id IN (%s) AND status = 'draft'", placeholders),
    params = as.list(outline_ids)
  )
  if (nrow(rows) == 0) return(list(approved_n = 0L, candidate_ids = character()))
  timestamp <- now_utc()
  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      sprintf("UPDATE article_lab_outlines SET status = 'approved', approved_at = ?, updated_at = ? WHERE outline_id IN (%s)", paste(rep("?", nrow(rows)), collapse = ", ")),
      params = c(list(timestamp, timestamp), as.list(rows$outline_id))
    )
    dbExecute(
      con,
      sprintf("UPDATE article_lab_title_candidates SET status = 'ready_for_draft', promoted = 0, ready_for_human_rating = 0, archived = 0 WHERE candidate_id IN (%s)", paste(rep("?", length(unique(rows$candidate_id))), collapse = ", ")),
      params = as.list(unique(rows$candidate_id))
    )
    for (batch_id in unique(rows$batch_id)) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  list(approved_n = nrow(rows), candidate_ids = unique(rows$candidate_id))
}

article_lab_save_outline_context_notes <- function(con, thumbnail_id, context_notes) {
  if (is.null(thumbnail_id) || is.na(thumbnail_id) || !nzchar(thumbnail_id)) return(invisible(NULL))
  notes_value <- article_lab_input_multiline(context_notes)
  if (is.null(notes_value) || is.na(notes_value)) notes_value <- ""
  dbExecute(
    con,
    "UPDATE article_lab_thumbnail_candidates SET outline_context_notes = ? WHERE thumbnail_id = ?",
    params = list(notes_value, thumbnail_id)
  )
  invisible(NULL)
}

article_lab_load_outline_context_notes <- function(con, thumbnail_id) {
  if (is.null(thumbnail_id) || is.na(thumbnail_id) || !nzchar(thumbnail_id)) return(NA_character_)
  result <- dbGetQuery(
    con,
    "SELECT outline_context_notes FROM article_lab_thumbnail_candidates WHERE thumbnail_id = ? AND outline_context_notes IS NOT NULL AND outline_context_notes != '' LIMIT 1",
    params = list(thumbnail_id)
  )
  if (nrow(result) > 0) result$outline_context_notes[[1]] else NA_character_
}


article_lab_archive_outlines <- function(con, outline_ids) {
  outline_ids <- clean_text(outline_ids)
  outline_ids <- unique(outline_ids[!is.na(outline_ids)])
  if (length(outline_ids) == 0) return(list(archived_n = 0L, batch_ids = character()))
  placeholders <- paste(rep("?", length(outline_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT outline_id, candidate_id, batch_id FROM article_lab_outlines WHERE outline_id IN (%s)", placeholders),
    params = as.list(outline_ids)
  )
  if (nrow(rows) == 0) return(list(archived_n = 0L, batch_ids = character()))
  candidate_ids <- unique(rows$candidate_id)
  batch_ids <- unique(rows$batch_id)
  timestamp <- now_utc()
  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      sprintf("UPDATE article_lab_outlines SET archived = 1, archived_at = ?, updated_at = ? WHERE outline_id IN (%s)", paste(rep("?", nrow(rows)), collapse = ", ")),
      params = c(list(timestamp, timestamp), as.list(rows$outline_id))
    )
    dbExecute(
      con,
      sprintf("UPDATE article_lab_title_candidates SET status = 'archived', promoted = 0, ready_for_human_rating = 0, archived = 1 WHERE candidate_id IN (%s)", paste(rep("?", length(candidate_ids)), collapse = ", ")),
      params = as.list(candidate_ids)
    )
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  list(archived_n = nrow(rows), batch_ids = batch_ids)
}


article_lab_full_text_api_request <- function(con, packages, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_, prompt_key = NA_character_, include_context = TRUE) {
  helper_path <- file.path("scripts", "writing_api", "generate_full_text.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_full_text.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(packages) == 0) return(list(rows = data.frame(), model = article_lab_default_full_text_model, mode = "api", raw_json = NULL, warnings = character()))

  settings <- article_lab_validate_generation_settings(article_lab_input_string(model) %||% article_lab_default_full_text_model, reasoning_effort, reasoning_mode)
  request_payload <- list(
    model = settings$model,
    reasoning_effort = settings$reasoning_effort,
    reasoning_mode = settings$reasoning_mode,
    prompt = article_lab_input_multiline(prompt) %||% article_lab_default_full_text_prompt,
    prompt_key = article_lab_input_string(prompt_key) %||% article_lab_full_text_prompt_key,
    packages = unname(lapply(seq_len(nrow(packages)), function(i) {
      summary_id <- research_input_integer(packages$summary_id[[i]])
      checked_evidence <- if (isTRUE(include_context) && !is.na(summary_id)) build_checked_summary_evidence(con, summary_id) else list()
      pdf_path <- if ("pdf_local_path" %in% names(packages) && isTRUE(include_context)) research_resolve_local_pdf_path(packages$pdf_local_path[[i]]) else NA_character_
      has_pdf <- !is.na(pdf_path) && file.exists(pdf_path)
      source_mode <- if (!isTRUE(include_context)) "none" else if (has_pdf) "pdf_attachment" else if (length(checked_evidence) > 0) "checked_summary_evidence" else "none"
      list(
        outline_id = packages$outline_id[[i]],
        thumbnail_id = packages$thumbnail_id[[i]],
        subtitle_id = packages$subtitle_id[[i]],
        candidate_id = packages$candidate_id[[i]],
        batch_id = packages$batch_id[[i]],
        title = packages$title[[i]],
        subtitle = packages$subtitle[[i]],
        thumbnail_label = packages$thumbnail_label[[i]],
        outline_text = packages$outline_text[[i]],
        source_context_mode = source_mode,
        article_summary = NULL,
        checked_evidence = checked_evidence,
        pdf_path = if (has_pdf) pdf_path else NULL
      )
    }))
  )

  request_file <- tempfile(pattern = "article_lab_full_text_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_full_text_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_full_text_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2("node", args = c(helper_path, request_file), stdout = stdout_file, stderr = stderr_file, timeout = article_lab_full_text_helper_timeout_seconds)
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (identical(status, 124L)) stop(sprintf("Full article generation helper timed out after %s seconds.", article_lab_full_text_helper_timeout_seconds), call. = FALSE)
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Full article generation helper failed.", call. = FALSE)
  if (!nzchar(trimws(stdout_text))) stop("Full article generation helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  result_rows <- lapply(parsed$results %||% list(), function(entry) {
    citation_map <- entry$citation_map %||% list()
    citation_map_json <- if (length(citation_map) == 0) NA_character_ else toJSON(citation_map, auto_unbox = TRUE, null = "null")
    data.frame(
      outline_id = article_lab_input_string(entry$outline_id),
      thumbnail_id = article_lab_input_string(entry$thumbnail_id),
      subtitle_id = article_lab_input_string(entry$subtitle_id),
      candidate_id = article_lab_input_string(entry$candidate_id),
      batch_id = article_lab_input_string(entry$batch_id),
      full_text = article_lab_input_multiline(entry$full_text),
      source_context_mode = article_lab_input_string(entry$source_context_mode) %||% "none",
      citation_map_json = citation_map_json,
      created_at = now_utc(),
      model = article_lab_input_string(parsed$model) %||% request_payload$model,
      reasoning_effort = article_lab_input_string(parsed$reasoning_effort) %||% settings$reasoning_effort,
      reasoning_mode = article_lab_input_string(parsed$reasoning_mode) %||% settings$reasoning_mode,
      generation_mode = "api",
      raw_json = stdout_text,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  result_rows <- Filter(function(row) nrow(row) > 0 && !is.na(row$full_text[[1]]), result_rows)
  list(
    rows = if (length(result_rows) == 0) data.frame() else do.call(rbind, result_rows),
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    mode = article_lab_input_string(parsed$mode) %||% "api",
    raw_json = stdout_text,
    warnings = parsed$warnings %||% list()
  )
}

generate_full_text_drafts <- function(con, packages, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_, prompt_key = NA_character_, include_context = TRUE) {
  tryCatch(
    article_lab_full_text_api_request(con, packages, model = model, reasoning_effort = reasoning_effort, reasoning_mode = reasoning_mode, prompt = prompt, prompt_key = prompt_key, include_context = include_context),
    error = function(e) list(rows = data.frame(), model = article_lab_input_string(model) %||% article_lab_default_full_text_model, mode = "failed", fallback_reason = conditionMessage(e))
  )
}

article_lab_insert_full_text_drafts <- function(con, draft_rows, prompt_key = NA_character_, prompt_version = NA_character_) {
  if (nrow(draft_rows) == 0) return(0L)
  inserted_n <- 0L
  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(draft_rows))) {
      timestamp <- draft_rows$created_at[[i]] %||% now_utc()
      citation_map_json <- if ("citation_map_json" %in% names(draft_rows)) draft_rows$citation_map_json[[i]] else NA_character_
      dbExecute(
        con,
        "INSERT INTO article_lab_full_text_drafts
         (full_text_draft_id, outline_id, thumbnail_id, subtitle_id, candidate_id, batch_id,
          original_generated_text, current_draft_text, status, is_approved, model, reasoning_effort, reasoning_mode, prompt_key, prompt_version,
          generation_mode, source_context_mode, citation_map_json, raw_json, notes, created_at, updated_at, approved_at, rejected_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'draft', 0, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, ?, ?, NULL, NULL)",
        params = list(
          article_lab_full_text_draft_id(draft_rows$outline_id[[i]]),
          draft_rows$outline_id[[i]], draft_rows$thumbnail_id[[i]], draft_rows$subtitle_id[[i]], draft_rows$candidate_id[[i]], draft_rows$batch_id[[i]],
          draft_rows$full_text[[i]], draft_rows$full_text[[i]], draft_rows$model[[i]] %||% article_lab_default_full_text_model, draft_rows$reasoning_effort[[i]], draft_rows$reasoning_mode[[i]],
          article_lab_input_string(prompt_key) %||% article_lab_full_text_prompt_key,
          article_lab_input_string(prompt_version) %||% article_lab_input_string(prompt_key) %||% article_lab_full_text_prompt_key,
          draft_rows$generation_mode[[i]] %||% "api", draft_rows$source_context_mode[[i]] %||% "none", citation_map_json, draft_rows$raw_json[[i]], timestamp, timestamp
        )
      )
      inserted_n <- inserted_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  inserted_n
}

article_lab_update_full_text_drafts <- function(con, draft_updates, edit_source = "manual_save") {
  if (length(draft_updates) == 0) return(0L)
  updated_n <- 0L
  timestamp <- now_utc()
  dbBegin(con)
  tryCatch({
    for (entry in draft_updates) {
      draft_id <- clean_text(entry$full_text_draft_id)
      if (length(draft_id) == 0 || is.na(draft_id[[1]])) next
      new_text <- article_lab_input_multiline(entry$current_draft_text)
      if (length(new_text) == 0 || is.na(new_text[[1]])) next
      new_notes <- article_lab_input_string(entry$notes) %||% NA_character_
      existing <- dbGetQuery(con, "SELECT current_draft_text, notes FROM article_lab_full_text_drafts WHERE full_text_draft_id = ?", params = list(draft_id[[1]]))
      if (nrow(existing) == 0) next
      previous_text <- existing$current_draft_text[[1]]
      previous_notes <- article_lab_input_string(existing$notes[[1]]) %||% NA_character_
      if (identical(previous_text, new_text[[1]]) && identical(previous_notes, new_notes)) next
      if (!identical(previous_text, new_text[[1]])) {
        dbExecute(
          con,
          "INSERT INTO article_lab_full_text_draft_revisions (revision_id, full_text_draft_id, previous_text, new_text, edit_source, edit_note, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)",
          params = list(article_lab_full_text_revision_id(draft_id[[1]]), draft_id[[1]], previous_text, new_text[[1]], edit_source, new_notes, timestamp)
        )
      }
      dbExecute(con, "UPDATE article_lab_full_text_drafts SET current_draft_text = ?, notes = ?, updated_at = ? WHERE full_text_draft_id = ?", params = list(new_text[[1]], new_notes, timestamp, draft_id[[1]]))
      updated_n <- updated_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  updated_n
}

article_lab_approve_full_text_draft <- function(con, full_text_draft_id) {
  draft_id <- article_lab_input_string(full_text_draft_id)
  if (is.na(draft_id) || !nzchar(draft_id)) return(list(approved_n = 0L, candidate_ids = character()))
  row <- dbGetQuery(con, "SELECT full_text_draft_id, outline_id, candidate_id, batch_id FROM article_lab_full_text_drafts WHERE full_text_draft_id = ? AND status = 'draft'", params = list(draft_id))
  if (nrow(row) == 0) return(list(approved_n = 0L, candidate_ids = character()))
  timestamp <- now_utc()
  dbBegin(con)
  tryCatch({
    dbExecute(con, "UPDATE article_lab_full_text_drafts SET is_approved = 0, status = CASE WHEN status = 'approved' THEN 'draft' ELSE status END, approved_at = NULL, updated_at = ? WHERE outline_id = ?", params = list(timestamp, row$outline_id[[1]]))
    dbExecute(con, "UPDATE article_lab_full_text_drafts SET status = 'approved', is_approved = 1, approved_at = ?, updated_at = ? WHERE full_text_draft_id = ?", params = list(timestamp, timestamp, draft_id))
    dbExecute(con, "UPDATE article_lab_title_candidates SET status = 'ready_for_review_publish', promoted = 0, ready_for_human_rating = 0, archived = 0 WHERE candidate_id = ?", params = list(row$candidate_id[[1]]))
    article_lab_update_batch_status(con, row$batch_id[[1]])
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  list(approved_n = 1L, candidate_ids = row$candidate_id)
}

article_lab_reject_full_text_draft <- function(con, full_text_draft_id) {
  draft_id <- article_lab_input_string(full_text_draft_id)
  if (is.na(draft_id) || !nzchar(draft_id)) return(0L)
  dbExecute(con, "UPDATE article_lab_full_text_drafts SET status = 'rejected', is_approved = 0, rejected_at = ?, approved_at = NULL, updated_at = ? WHERE full_text_draft_id = ? AND status != 'approved'", params = list(now_utc(), now_utc(), draft_id))
}

article_lab_parse_medium_tags <- function(value) {
  raw <- article_lab_input_multiline(value) %||% ""
  tags <- clean_text(unlist(strsplit(raw, "[,\n]", perl = TRUE), use.names = FALSE))
  tags <- unique(tags[!is.na(tags)])
  head(tags, 5L)
}

article_lab_tags_display <- function(tags_json) {
  text <- article_lab_input_string(tags_json)
  if (is.null(text)) return("")
  parsed <- tryCatch(fromJSON(text), error = function(e) character())
  paste(clean_text(parsed), collapse = ", ")
}


article_lab_save_publication <- function(con, publication_name, platform = "Medium") {
  name <- article_lab_input_string(publication_name)
  if (is.null(name) || is.na(name) || !nzchar(name)) return(NA_character_)
  platform <- article_lab_input_string(platform) %||% "Medium"
  existing <- dbGetQuery(con, "SELECT publication_id FROM article_lab_publications WHERE publication_name = ? AND platform = ? LIMIT 1", params = list(name, platform))
  timestamp <- now_utc()
  if (nrow(existing) > 0) {
    dbExecute(con, "UPDATE article_lab_publications SET is_active = 1, updated_at = ? WHERE publication_id = ?", params = list(timestamp, existing$publication_id[[1]]))
    return(existing$publication_id[[1]])
  }
  publication_id <- article_lab_publication_id(name)
  dbExecute(
    con,
    "INSERT INTO article_lab_publications (publication_id, publication_name, platform, submission_notes, submission_url, is_active, created_at, updated_at) VALUES (?, ?, ?, NULL, NULL, 1, ?, ?)",
    params = list(publication_id, name, platform, timestamp, timestamp)
  )
  publication_id
}


article_lab_medium_tags_api_request <- function(row, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_medium_tags.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_medium_tags.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(row) == 0) stop("Select an approved full article draft before generating Medium tags.", call. = FALSE)

  settings <- article_lab_validate_generation_settings(article_lab_input_string(model) %||% article_lab_default_medium_tags_model, reasoning_effort, reasoning_mode)
  request_payload <- list(
    model = settings$model,
    reasoning_effort = settings$reasoning_effort,
    reasoning_mode = settings$reasoning_mode,
    prompt = article_lab_input_multiline(prompt) %||% article_lab_default_medium_tags_prompt,
    article = list(
      full_text_draft_id = article_lab_row_value(row, "full_text_draft_id"),
      candidate_id = article_lab_row_value(row, "candidate_id"),
      batch_id = article_lab_row_value(row, "batch_id"),
      title = article_lab_row_value(row, "title"),
      subtitle = article_lab_row_value(row, "subtitle"),
      body = article_lab_row_value(row, "current_draft_text")
    )
  )

  request_file <- tempfile(pattern = "article_lab_medium_tags_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_medium_tags_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_medium_tags_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2("node", args = c(helper_path, request_file), stdout = stdout_file, stderr = stderr_file)
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Medium tag generation helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Medium tag generation helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  tags <- article_lab_parse_medium_tags(paste(unlist(parsed$tags %||% list(), use.names = FALSE), collapse = ", "))
  if (length(tags) == 0) stop("API helper returned no usable Medium tags.", call. = FALSE)
  list(tags = tags, model = article_lab_input_string(parsed$model) %||% request_payload$model, mode = article_lab_input_string(parsed$mode) %||% "api", raw_json = stdout_text, response_id = article_lab_input_string(parsed$response_id))
}

article_lab_save_publish_settings <- function(con, row, values) {
  if (nrow(row) == 0) return(0L)
  draft_id <- article_lab_row_value(row, "full_text_draft_id")
  if (is.na(draft_id) || !nzchar(draft_id)) return(0L)
  tags <- article_lab_parse_medium_tags(values$medium_tags %||% "")
  target <- article_lab_input_string(values$publishing_target) %||% "Do not publish yet"
  if (!(target %in% article_lab_publish_target_choices)) target <- "Do not publish yet"
  monetization <- article_lab_input_string(values$monetization) %||% "Undecided"
  if (!(monetization %in% article_lab_monetization_choices)) monetization <- "Undecided"
  status <- article_lab_input_string(values$publish_status) %||% "ready_for_review_publish"
  if (!(status %in% article_lab_publish_status_values)) status <- "ready_for_review_publish"
  publication_id <- article_lab_input_string(values$publication_id) %||% NA_character_
  new_publication_name <- article_lab_input_string(values$new_publication_name) %||% NA_character_
  if (identical(target, "Submit to Medium publication") && !is.na(new_publication_name) && nzchar(new_publication_name)) {
    publication_id <- article_lab_save_publication(con, new_publication_name)
  }
  publication_name <- NA_character_
  if (!is.na(publication_id) && nzchar(publication_id)) {
    publication <- dbGetQuery(con, "SELECT publication_name FROM article_lab_publications WHERE publication_id = ? LIMIT 1", params = list(publication_id))
    if (nrow(publication) > 0) publication_name <- publication$publication_name[[1]]
  }
  timestamp <- now_utc()
  existing <- dbGetQuery(con, "SELECT publish_settings_id, publish_status, submitted_at, published_at FROM article_lab_publish_settings WHERE full_text_draft_id = ? LIMIT 1", params = list(draft_id))
  submitted_at <- if (nrow(existing) > 0) existing$submitted_at[[1]] else NA_character_
  published_at <- if (nrow(existing) > 0) existing$published_at[[1]] else NA_character_
  if (identical(status, "submitted") && (is.na(submitted_at) || !nzchar(submitted_at))) submitted_at <- timestamp
  if (identical(status, "published") && (is.na(published_at) || !nzchar(published_at))) published_at <- timestamp
  params <- list(
    draft_id,
    article_lab_row_value(row, "thumbnail_id"), article_lab_row_value(row, "subtitle_id"), article_lab_row_value(row, "candidate_id"), article_lab_row_value(row, "batch_id"),
    toJSON(tags, auto_unbox = TRUE), target, publication_id, publication_name, monetization,
    article_lab_input_string(values$canonical_url) %||% NA_character_,
    article_lab_input_string(values$featured_image_alt_text) %||% NA_character_,
    article_lab_input_string(values$image_credit_source) %||% NA_character_,
    article_lab_input_string(values$published_url) %||% NA_character_,
    status, article_lab_input_multiline(values$notes) %||% NA_character_, submitted_at, published_at, timestamp
  )
  if (nrow(existing) > 0) {
    dbExecute(
      con,
      "UPDATE article_lab_publish_settings
       SET full_text_draft_id = ?, thumbnail_id = ?, subtitle_id = ?, candidate_id = ?, batch_id = ?,
           medium_tags_json = ?, publishing_target = ?, publication_id = ?, publication_name_snapshot = ?, monetization = ?,
           canonical_url = ?, featured_image_alt_text = ?, image_credit_source = ?, published_url = ?, publish_status = ?,
           notes = ?, submitted_at = ?, published_at = ?, updated_at = ?
       WHERE publish_settings_id = ?",
      params = c(params, list(existing$publish_settings_id[[1]]))
    )
  } else {
    dbExecute(
      con,
      "INSERT INTO article_lab_publish_settings
       (publish_settings_id, full_text_draft_id, thumbnail_id, subtitle_id, candidate_id, batch_id,
        medium_tags_json, publishing_target, publication_id, publication_name_snapshot, monetization,
        canonical_url, featured_image_alt_text, image_credit_source, published_url, publish_status, notes,
        submitted_at, published_at, created_at, updated_at)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = c(list(article_lab_publish_settings_id(draft_id)), params[1:18], list(timestamp, timestamp))
    )
  }
  1L
}

article_lab_generate_subtitles_for_titles <- function(con, candidate_ids, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_, variants_per_title = 4L) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) {
    return(list(generated_n = 0L, title_n = 0L, skipped_n = 0L, batch_ids = character(), mode = "none", model = article_lab_default_subtitle_model))
  }

  placeholders <- paste(rep("?", length(candidate_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT c.candidate_id, c.batch_id, c.title, c.status, c.ready_for_human_rating, c.promoted, c.archived,
              COALESCE(s.generated_n, 0) AS generated_subtitle_n, COALESCE(s.approved_n, 0) AS approved_subtitle_n
       FROM article_lab_title_candidates c
       LEFT JOIN (
         SELECT candidate_id,
                COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
                COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n
         FROM article_lab_subtitle_candidates
         GROUP BY candidate_id
       ) s
         ON s.candidate_id = c.candidate_id
       WHERE c.candidate_id IN (%s)",
      placeholders
    ),
    params = as.list(candidate_ids)
  )
  if (nrow(rows) == 0) {
    return(list(generated_n = 0L, title_n = 0L, skipped_n = length(candidate_ids), batch_ids = character(), mode = "none", model = article_lab_default_subtitle_model))
  }
  rows <- article_lab_normalize_candidate_rows(rows)
  eligible <- rows[
    rows$normalized_status == "approved_for_subtitle" &
      rows$generated_subtitle_n <= 0 &
      rows$approved_subtitle_n <= 0,
    c("candidate_id", "batch_id", "title"),
    drop = FALSE
  ]
  skipped_n <- length(candidate_ids) - nrow(eligible)
  if (nrow(eligible) == 0) {
    return(list(generated_n = 0L, title_n = 0L, skipped_n = skipped_n, batch_ids = unique(rows$batch_id), mode = "none", model = article_lab_default_subtitle_model))
  }

  summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(eligible$batch_id))
  eligible$article_summary <- NA_character_
  if (nrow(summary_contexts) > 0) {
    matched_summary <- summary_contexts$article_summary[match(eligible$batch_id, summary_contexts$batch_id)]
    eligible$article_summary <- matched_summary
  }

  existing_rows <- dbGetQuery(
    con,
    sprintf("SELECT candidate_id, subtitle FROM article_lab_subtitle_candidates WHERE candidate_id IN (%s)", paste(rep("?", nrow(eligible)), collapse = ", ")),
    params = as.list(eligible$candidate_id)
  )
  generated <- generate_subtitle_candidates(eligible, variants_per_title = variants_per_title, model = model, reasoning_effort = reasoning_effort, reasoning_mode = reasoning_mode, prompt = prompt)
  subtitle_rows <- generated$rows
  if (nrow(subtitle_rows) == 0) {
    return(list(generated_n = 0L, title_n = 0L, skipped_n = skipped_n + nrow(eligible), batch_ids = unique(rows$batch_id), mode = generated$mode %||% "none", model = generated$model %||% article_lab_default_subtitle_model, fallback_reason = generated$fallback_reason %||% NULL))
  }

  if (nrow(existing_rows) > 0) {
    existing_keys <- paste(existing_rows$candidate_id, tolower(existing_rows$subtitle))
    subtitle_rows <- subtitle_rows[!(paste(subtitle_rows$candidate_id, tolower(subtitle_rows$subtitle)) %in% existing_keys), , drop = FALSE]
  }
  if (nrow(subtitle_rows) == 0) {
    return(list(generated_n = 0L, title_n = 0L, skipped_n = skipped_n + nrow(eligible), batch_ids = unique(rows$batch_id), mode = generated$mode %||% "none", model = generated$model %||% article_lab_default_subtitle_model, fallback_reason = generated$fallback_reason %||% NULL))
  }

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(subtitle_rows))) {
      row <- subtitle_rows[i, , drop = FALSE]
      dbExecute(
        con,
        "
        INSERT INTO article_lab_subtitle_candidates
        (subtitle_id, candidate_id, batch_id, created_at, subtitle, status, notes, model, reasoning_effort, reasoning_mode, generation_mode, raw_json, approved_at, rejected_at)
        VALUES (?, ?, ?, ?, ?, 'generated', NULL, ?, ?, ?, ?, ?, NULL, NULL)
        ",
        params = list(
          article_lab_subtitle_id(row$candidate_id[[1]], i),
          row$candidate_id[[1]],
          row$batch_id[[1]],
          row$created_at[[1]] %||% now_utc(),
          row$subtitle[[1]],
          row$model[[1]] %||% article_lab_default_subtitle_model,
          row$reasoning_effort[[1]],
          row$reasoning_mode[[1]],
          row$generation_mode[[1]] %||% generated$mode %||% "generated",
          row$raw_json[[1]]
        )
      )
    }
    for (batch_id in unique(subtitle_rows$batch_id)) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(
    generated_n = nrow(subtitle_rows),
    title_n = length(unique(subtitle_rows$candidate_id)),
    skipped_n = skipped_n,
    batch_ids = unique(subtitle_rows$batch_id),
    mode = generated$mode %||% "generated",
    model = generated$model %||% article_lab_default_subtitle_model,
    fallback_reason = generated$fallback_reason %||% NULL
  )
}

article_lab_add_manual_subtitles <- function(con, candidate_id, subtitles) {
  candidate_id <- article_lab_input_string(candidate_id)
  subtitles <- article_lab_normalize_subtitle(unlist(strsplit(paste(subtitles, collapse = "\n"), "\n", fixed = TRUE)))
  if (is.na(candidate_id) || !nzchar(candidate_id) || length(subtitles) == 0) {
    return(list(added_n = 0L, skipped_n = 0L, duplicate_n = 0L, batch_id = NA_character_, title = NA_character_))
  }

  candidate_row <- dbGetQuery(
    con,
    "SELECT candidate_id, batch_id, title, status, ready_for_human_rating, promoted, archived
     FROM article_lab_title_candidates
     WHERE candidate_id = ?",
    params = list(candidate_id)
  )
  if (nrow(candidate_row) == 0) {
    return(list(added_n = 0L, skipped_n = length(subtitles), duplicate_n = 0L, batch_id = NA_character_, title = NA_character_))
  }
  candidate_row <- article_lab_normalize_candidate_rows(candidate_row)
  if (!(candidate_row$normalized_status[[1]] %in% c("approved_for_subtitle", "ready_for_thumbnail")) || isTRUE(candidate_row$archived[[1]] == 1)) {
    return(list(
      added_n = 0L,
      skipped_n = length(subtitles),
      duplicate_n = 0L,
      batch_id = candidate_row$batch_id[[1]] %||% NA_character_,
      title = candidate_row$title[[1]] %||% NA_character_
    ))
  }

  existing_rows <- dbGetQuery(
    con,
    "SELECT subtitle FROM article_lab_subtitle_candidates WHERE candidate_id = ?",
    params = list(candidate_id)
  )
  existing_keys <- if (nrow(existing_rows) > 0) tolower(clean_text(existing_rows$subtitle)) else character()
  subtitle_keys <- tolower(subtitles)
  keep_idx <- !(subtitle_keys %in% existing_keys)
  duplicate_n <- sum(!keep_idx)
  subtitles <- subtitles[keep_idx]
  if (length(subtitles) == 0) {
    return(list(
      added_n = 0L,
      skipped_n = 0L,
      duplicate_n = duplicate_n,
      batch_id = candidate_row$batch_id[[1]] %||% NA_character_,
      title = candidate_row$title[[1]] %||% NA_character_
    ))
  }

  created_at <- now_utc()
  dbBegin(con)
  tryCatch({
    for (i in seq_along(subtitles)) {
      dbExecute(
        con,
        "
        INSERT INTO article_lab_subtitle_candidates
        (subtitle_id, candidate_id, batch_id, created_at, subtitle, status, notes, model, generation_mode, raw_json, approved_at, rejected_at)
        VALUES (?, ?, ?, ?, ?, 'generated', NULL, NULL, 'manual', NULL, NULL, NULL)
        ",
        params = list(
          article_lab_subtitle_id(candidate_id, i),
          candidate_id,
          candidate_row$batch_id[[1]],
          created_at,
          subtitles[[i]]
        )
      )
    }
    article_lab_update_batch_status(con, candidate_row$batch_id[[1]])
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(
    added_n = length(subtitles),
    skipped_n = 0L,
    duplicate_n = duplicate_n,
    batch_id = candidate_row$batch_id[[1]] %||% NA_character_,
    title = candidate_row$title[[1]] %||% NA_character_
  )
}

article_lab_approve_subtitles <- function(con, subtitle_ids) {
  subtitle_ids <- clean_text(subtitle_ids)
  subtitle_ids <- unique(subtitle_ids[!is.na(subtitle_ids)])
  if (length(subtitle_ids) == 0) return(list(approved_n = 0L, skipped_n = 0L, candidate_ids = character(), batch_ids = character()))

  placeholders <- paste(rep("?", length(subtitle_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT subtitle_id, candidate_id, batch_id, status FROM article_lab_subtitle_candidates WHERE subtitle_id IN (%s)", placeholders),
    params = as.list(subtitle_ids)
  )
  if (nrow(rows) == 0) return(list(approved_n = 0L, skipped_n = length(subtitle_ids), candidate_ids = character(), batch_ids = character()))
  eligible_ids <- rows$subtitle_id[rows$status == "generated"]
  skipped_n <- length(subtitle_ids) - length(eligible_ids)
  candidate_ids <- unique(rows$candidate_id[rows$subtitle_id %in% eligible_ids])
  batch_ids <- unique(rows$batch_id[rows$subtitle_id %in% eligible_ids])

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf("UPDATE article_lab_subtitle_candidates SET status = 'approved', approved_at = ?, rejected_at = NULL WHERE subtitle_id IN (%s)", paste(rep("?", length(eligible_ids)), collapse = ", ")),
        params = c(list(now_utc()), as.list(eligible_ids))
      )
      article_lab_sync_title_subtitle_stage(con, candidate_ids)
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(approved_n = length(eligible_ids), skipped_n = skipped_n, candidate_ids = candidate_ids, batch_ids = batch_ids)
}

article_lab_reject_subtitles <- function(con, subtitle_ids) {
  subtitle_ids <- clean_text(subtitle_ids)
  subtitle_ids <- unique(subtitle_ids[!is.na(subtitle_ids)])
  if (length(subtitle_ids) == 0) return(list(rejected_n = 0L, skipped_n = 0L, candidate_ids = character(), batch_ids = character()))

  placeholders <- paste(rep("?", length(subtitle_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT subtitle_id, candidate_id, batch_id, status FROM article_lab_subtitle_candidates WHERE subtitle_id IN (%s)", placeholders),
    params = as.list(subtitle_ids)
  )
  if (nrow(rows) == 0) return(list(rejected_n = 0L, skipped_n = length(subtitle_ids), candidate_ids = character(), batch_ids = character()))
  eligible_ids <- rows$subtitle_id[rows$status == "generated"]
  skipped_n <- length(subtitle_ids) - length(eligible_ids)
  candidate_ids <- unique(rows$candidate_id[rows$subtitle_id %in% eligible_ids])
  batch_ids <- unique(rows$batch_id[rows$subtitle_id %in% eligible_ids])

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf("UPDATE article_lab_subtitle_candidates SET status = 'rejected', rejected_at = ? WHERE subtitle_id IN (%s)", paste(rep("?", length(eligible_ids)), collapse = ", ")),
        params = c(list(now_utc()), as.list(eligible_ids))
      )
      article_lab_sync_title_subtitle_stage(con, candidate_ids)
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(rejected_n = length(eligible_ids), skipped_n = skipped_n, candidate_ids = candidate_ids, batch_ids = batch_ids)
}

article_lab_dismiss_thumbnail_packages <- function(con, subtitle_ids) {
  subtitle_ids <- clean_text(subtitle_ids)
  subtitle_ids <- unique(subtitle_ids[!is.na(subtitle_ids)])
  if (length(subtitle_ids) == 0) return(list(dismissed_n = 0L, skipped_n = 0L, candidate_ids = character(), batch_ids = character()))

  placeholders <- paste(rep("?", length(subtitle_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT
         s.subtitle_id,
         s.candidate_id,
         s.batch_id,
         s.status,
         COALESCE(t.generated_n, 0) AS generated_thumbnail_n,
         COALESCE(t.approved_n, 0) AS approved_thumbnail_n
       FROM article_lab_subtitle_candidates s
       LEFT JOIN (
         SELECT
           subtitle_id,
           COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
           COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n
         FROM article_lab_thumbnail_candidates
         GROUP BY subtitle_id
       ) t
         ON t.subtitle_id = s.subtitle_id
       WHERE s.subtitle_id IN (%s)",
      placeholders
    ),
    params = as.list(subtitle_ids)
  )
  if (nrow(rows) == 0) return(list(dismissed_n = 0L, skipped_n = length(subtitle_ids), candidate_ids = character(), batch_ids = character()))

  eligible_rows <- rows[
    rows$status == "approved" &
      rows$generated_thumbnail_n <= 0 &
      rows$approved_thumbnail_n <= 0,
    ,
    drop = FALSE
  ]
  skipped_n <- length(subtitle_ids) - nrow(eligible_rows)
  candidate_ids <- unique(eligible_rows$candidate_id)
  batch_ids <- unique(eligible_rows$batch_id)

  dbBegin(con)
  tryCatch({
    if (nrow(eligible_rows) > 0) {
      dbExecute(
        con,
        sprintf("UPDATE article_lab_subtitle_candidates SET status = 'rejected', rejected_at = ?, approved_at = NULL WHERE subtitle_id IN (%s)", paste(rep("?", nrow(eligible_rows)), collapse = ", ")),
        params = c(list(now_utc()), as.list(eligible_rows$subtitle_id))
      )
      article_lab_sync_title_subtitle_stage(con, candidate_ids)
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(dismissed_n = nrow(eligible_rows), skipped_n = skipped_n, candidate_ids = candidate_ids, batch_ids = batch_ids)
}

article_lab_generate_thumbnails_for_packages <- function(con, subtitle_ids, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_, variants_per_package = article_lab_default_thumbnail_variants, size = article_lab_default_thumbnail_size, quality = article_lab_default_thumbnail_quality, output_format = article_lab_default_thumbnail_output_format, output_compression = NULL, background = article_lab_default_thumbnail_background) {
  subtitle_ids <- clean_text(subtitle_ids)
  subtitle_ids <- unique(subtitle_ids[!is.na(subtitle_ids)])
  if (length(subtitle_ids) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = 0L, batch_ids = character(), mode = "none", model = article_lab_default_thumbnail_model))
  }

  placeholders <- paste(rep("?", length(subtitle_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT
         s.subtitle_id,
         s.candidate_id,
         s.batch_id,
         s.subtitle,
         s.status AS subtitle_status,
         c.title,
         c.status,
         c.ready_for_human_rating,
         c.promoted,
         c.archived,
         COALESCE(t.generated_n, 0) AS generated_thumbnail_n,
         COALESCE(t.approved_n, 0) AS approved_thumbnail_n
       FROM article_lab_subtitle_candidates s
       INNER JOIN article_lab_title_candidates c
         ON c.candidate_id = s.candidate_id
       LEFT JOIN (
         SELECT
           subtitle_id,
           COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
           COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n
         FROM article_lab_thumbnail_candidates
         GROUP BY subtitle_id
       ) t
         ON t.subtitle_id = s.subtitle_id
       WHERE s.subtitle_id IN (%s)",
      placeholders
    ),
    params = as.list(subtitle_ids)
  )
  if (nrow(rows) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = length(subtitle_ids), batch_ids = character(), mode = "none", model = article_lab_default_thumbnail_model))
  }
  rows <- article_lab_normalize_candidate_rows(rows)
  eligible <- rows[
    rows$subtitle_status == "approved" &
      rows$approved_thumbnail_n <= 0,
    c("subtitle_id", "candidate_id", "batch_id", "title", "subtitle"),
    drop = FALSE
  ]
  skipped_n <- length(subtitle_ids) - nrow(eligible)
  if (nrow(eligible) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = skipped_n, batch_ids = unique(rows$batch_id), mode = "none", model = article_lab_default_thumbnail_model))
  }

  generated <- generate_thumbnail_candidates(eligible, variants_per_package = variants_per_package, model = model, reasoning_effort = reasoning_effort, reasoning_mode = reasoning_mode, prompt = prompt, size = size, quality = quality, output_format = output_format, output_compression = output_compression, background = background)
  thumbnail_rows <- generated$rows
  if (nrow(thumbnail_rows) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = skipped_n + nrow(eligible), batch_ids = unique(rows$batch_id), mode = generated$mode %||% "none", model = generated$model %||% article_lab_default_thumbnail_model, fallback_reason = generated$fallback_reason %||% NULL))
  }

  asset_dir <- file.path(project_root, ".local_gitignored", "article_lab_thumbnails")
  if (!dir.exists(asset_dir)) dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)
  thumbnail_rows$local_asset_path <- vapply(seq_len(nrow(thumbnail_rows)), function(i) {
    data_uri <- thumbnail_rows$thumbnail_data_uri[[i]]
    matched <- regexec("^data:image/(png|webp|jpeg);base64,(.+)$", data_uri, perl = TRUE)
    parts <- regmatches(data_uri, matched)[[1]]
    if (length(parts) != 3L) stop("Generated thumbnail did not contain a supported image data URI.", call. = FALSE)
    extension <- if (identical(parts[[2]], "jpeg")) "jpg" else parts[[2]]
    response_suffix <- substr(gsub("[^A-Za-z0-9]", "", thumbnail_rows$response_id[[i]] %||% "response"), 1L, 20L)
    asset_name <- sprintf("%s_variant_%s_%s.%s", gsub("[^A-Za-z0-9_-]", "_", thumbnail_rows$subtitle_id[[i]]), thumbnail_rows$variant_index[[i]] %||% i, response_suffix, extension)
    absolute_path <- file.path(asset_dir, asset_name)
    writeBin(jsonlite::base64_dec(parts[[3]]), absolute_path)
    file.path(".local_gitignored", "article_lab_thumbnails", asset_name)
  }, character(1))

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(thumbnail_rows))) {
      row <- thumbnail_rows[i, , drop = FALSE]
      dbExecute(
        con,
        "
        INSERT INTO article_lab_thumbnail_candidates
        (thumbnail_id, subtitle_id, candidate_id, batch_id, created_at, thumbnail_label, thumbnail_data_uri, status, notes, model, reasoning_effort, reasoning_mode, generation_mode, raw_json, approved_at, rejected_at, submitted_prompt, revised_prompt, response_id, image_generation_call_id, variant_index, image_settings_json, local_asset_path, generation_run_id)
        VALUES (?, ?, ?, ?, ?, ?, ?, 'generated', NULL, ?, ?, ?, ?, ?, NULL, NULL, ?, ?, ?, ?, ?, ?, ?, ?)
        ",
        params = list(
          article_lab_thumbnail_id(row$subtitle_id[[1]], i),
          row$subtitle_id[[1]],
          row$candidate_id[[1]],
          row$batch_id[[1]],
          row$created_at[[1]] %||% now_utc(),
          row$thumbnail_label[[1]],
          row$thumbnail_data_uri[[1]],
          row$model[[1]] %||% article_lab_default_thumbnail_model,
          row$reasoning_effort[[1]],
          row$reasoning_mode[[1]],
          row$generation_mode[[1]] %||% generated$mode %||% "generated",
          row$raw_json[[1]],
          row$submitted_prompt[[1]],
          row$revised_prompt[[1]],
          row$response_id[[1]],
          row$image_generation_call_id[[1]],
          row$variant_index[[1]],
          row$image_settings_json[[1]],
          row$local_asset_path[[1]],
          row$generation_run_id[[1]]
        )
      )
    }
    for (batch_id in unique(thumbnail_rows$batch_id)) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(
    generated_n = nrow(thumbnail_rows),
    package_n = length(unique(thumbnail_rows$subtitle_id)),
    skipped_n = skipped_n,
    batch_ids = unique(thumbnail_rows$batch_id),
    mode = generated$mode %||% "generated",
    model = generated$model %||% article_lab_default_thumbnail_model,
    fallback_reason = generated$fallback_reason %||% NULL
  )
}

article_lab_approve_thumbnails <- function(con, thumbnail_ids) {
  thumbnail_ids <- clean_text(thumbnail_ids)
  thumbnail_ids <- unique(thumbnail_ids[!is.na(thumbnail_ids)])
  if (length(thumbnail_ids) == 0) return(list(approved_n = 0L, skipped_n = 0L, candidate_ids = character(), batch_ids = character(), subtitle_ids = character(), duplicate_subtitle_ids = character(), message = NULL))

  placeholders <- paste(rep("?", length(thumbnail_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT thumbnail_id, subtitle_id, candidate_id, batch_id, status FROM article_lab_thumbnail_candidates WHERE thumbnail_id IN (%s)", placeholders),
    params = as.list(thumbnail_ids)
  )
  if (nrow(rows) == 0) return(list(approved_n = 0L, skipped_n = length(thumbnail_ids), candidate_ids = character(), batch_ids = character(), subtitle_ids = character(), duplicate_subtitle_ids = character(), message = NULL))
  eligible_rows <- rows[rows$status == "generated", , drop = FALSE]
  skipped_n <- length(thumbnail_ids) - nrow(eligible_rows)
  duplicate_counts <- table(eligible_rows$subtitle_id)
  duplicate_subtitle_ids <- names(duplicate_counts[duplicate_counts > 1L])
  if (length(duplicate_subtitle_ids) > 0) {
    return(list(
      approved_n = 0L,
      skipped_n = skipped_n,
      candidate_ids = character(),
      batch_ids = character(),
      subtitle_ids = character(),
      duplicate_subtitle_ids = duplicate_subtitle_ids,
      message = "Select only one thumbnail candidate per title/subtitle package before approving."
    ))
  }

  approved_packages <- if (nrow(eligible_rows) > 0) {
    dbGetQuery(
      con,
      sprintf("SELECT DISTINCT subtitle_id FROM article_lab_thumbnail_candidates WHERE status = 'approved' AND subtitle_id IN (%s)", paste(rep("?", nrow(eligible_rows)), collapse = ", ")),
      params = as.list(eligible_rows$subtitle_id)
    )
  } else {
    data.frame()
  }
  already_approved_ids <- clean_text(approved_packages$subtitle_id)
  if (length(already_approved_ids) > 0) {
    eligible_rows <- eligible_rows[!(eligible_rows$subtitle_id %in% already_approved_ids), , drop = FALSE]
    skipped_n <- length(thumbnail_ids) - nrow(eligible_rows)
  }
  if (nrow(eligible_rows) == 0) {
    return(list(approved_n = 0L, skipped_n = skipped_n, candidate_ids = character(), batch_ids = character(), subtitle_ids = character(), duplicate_subtitle_ids = character(), message = "No selected thumbnails were eligible for approval."))
  }

  eligible_ids <- eligible_rows$thumbnail_id
  candidate_ids <- unique(eligible_rows$candidate_id)
  batch_ids <- unique(eligible_rows$batch_id)
  subtitle_ids <- unique(eligible_rows$subtitle_id)

  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      sprintf("UPDATE article_lab_thumbnail_candidates SET status = 'approved', approved_at = ?, rejected_at = NULL WHERE thumbnail_id IN (%s)", paste(rep("?", length(eligible_ids)), collapse = ", ")),
      params = c(list(now_utc()), as.list(eligible_ids))
    )
    article_lab_sync_title_thumbnail_stage(con, candidate_ids)
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(approved_n = length(eligible_ids), skipped_n = skipped_n, candidate_ids = candidate_ids, batch_ids = batch_ids, subtitle_ids = subtitle_ids, duplicate_subtitle_ids = character(), message = NULL)
}

article_lab_reject_thumbnails <- function(con, thumbnail_ids) {
  thumbnail_ids <- clean_text(thumbnail_ids)
  thumbnail_ids <- unique(thumbnail_ids[!is.na(thumbnail_ids)])
  if (length(thumbnail_ids) == 0) return(list(rejected_n = 0L, skipped_n = 0L, candidate_ids = character(), batch_ids = character()))

  placeholders <- paste(rep("?", length(thumbnail_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf("SELECT thumbnail_id, subtitle_id, candidate_id, batch_id, status FROM article_lab_thumbnail_candidates WHERE thumbnail_id IN (%s)", placeholders),
    params = as.list(thumbnail_ids)
  )
  if (nrow(rows) == 0) return(list(rejected_n = 0L, skipped_n = length(thumbnail_ids), candidate_ids = character(), batch_ids = character()))
  eligible_ids <- rows$thumbnail_id[rows$status == "generated"]
  skipped_n <- length(thumbnail_ids) - length(eligible_ids)
  candidate_ids <- unique(rows$candidate_id[rows$thumbnail_id %in% eligible_ids])
  batch_ids <- unique(rows$batch_id[rows$thumbnail_id %in% eligible_ids])

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf("UPDATE article_lab_thumbnail_candidates SET status = 'rejected', rejected_at = ? WHERE thumbnail_id IN (%s)", paste(rep("?", length(eligible_ids)), collapse = ", ")),
        params = c(list(now_utc()), as.list(eligible_ids))
      )
      article_lab_sync_title_thumbnail_stage(con, candidate_ids)
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(rejected_n = length(eligible_ids), skipped_n = skipped_n, candidate_ids = candidate_ids, batch_ids = batch_ids)
}


article_lab_update_batch_status <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id)) return(invisible(NULL))
  status_rows <- dbGetQuery(
    con,
    "
    SELECT
      COALESCE(SUM(CASE WHEN status = 'ready_for_api_scoring' THEN 1 ELSE 0 END), 0) AS ready_n,
      COALESCE(SUM(CASE WHEN status = 'api_scored' THEN 1 ELSE 0 END), 0) AS scored_n,
      COALESCE(SUM(CASE WHEN status = 'approved_for_subtitle' OR promoted = 1 THEN 1 ELSE 0 END), 0) AS approved_n,
      COALESCE(SUM(CASE WHEN status = 'ready_for_thumbnail' THEN 1 ELSE 0 END), 0) AS subtitle_ready_n,
      COALESCE(SUM(CASE WHEN status = 'ready_for_outline' THEN 1 ELSE 0 END), 0) AS outline_ready_n,
      COALESCE(SUM(CASE WHEN status = 'ready_for_draft' THEN 1 ELSE 0 END), 0) AS draft_ready_n,
      COALESCE(SUM(CASE WHEN archived = 1 THEN 1 ELSE 0 END), 0) AS archived_n,
      COUNT(*) AS total_n
    FROM article_lab_title_candidates
    WHERE batch_id = ?
    ",
    params = list(batch_id)
  )
  if (nrow(status_rows) == 0) return(invisible(NULL))
  row <- status_rows[1, , drop = FALSE]
  batch_status <- if (row$draft_ready_n[[1]] > 0) {
    "ready_for_draft"
  } else if (row$outline_ready_n[[1]] > 0) {
    "ready_for_outline"
  } else if (row$subtitle_ready_n[[1]] > 0) {
    "ready_for_thumbnail"
  } else if (row$approved_n[[1]] > 0) {
    "approved_for_subtitle"
  } else if (row$ready_n[[1]] > 0) {
    "ready_for_api_scoring"
  } else if (row$scored_n[[1]] > 0) {
    "api_scored"
  } else if (row$archived_n[[1]] >= row$total_n[[1]] && row$total_n[[1]] > 0) {
    "archived"
  } else {
    "generated"
  }
  dbExecute(
    con,
    "UPDATE article_lab_title_batches SET status = ? WHERE batch_id = ?",
    params = list(batch_status, batch_id)
  )
  invisible(batch_status)
}

article_lab_recover_api_pending_candidates <- function(con, batch_id = NULL) {
  batch_filter <- article_lab_input_string(batch_id)
  where_sql <- "WHERE c.status = 'api_pending'"
  params <- list()
  if (!is.null(batch_filter) && !identical(batch_filter, article_lab_all_batches_value)) {
    where_sql <- paste(where_sql, "AND c.batch_id = ?")
    params <- list(batch_filter)
  }

  pending_query <- sprintf(
    "
    SELECT
      c.candidate_id,
      c.batch_id,
      CASE
        WHEN EXISTS (
          SELECT 1
          FROM article_lab_title_api_scores s
          WHERE s.candidate_id = c.candidate_id
          LIMIT 1
        ) THEN 'api_scored'
        ELSE 'ready_for_api_scoring'
      END AS recovered_status
    FROM article_lab_title_candidates c
    %s
    ",
    where_sql
  )
  pending_rows <- if (length(params) > 0) {
    dbGetQuery(con, pending_query, params = params)
  } else {
    dbGetQuery(con, pending_query)
  }
  if (nrow(pending_rows) == 0) return(invisible(0L))

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(pending_rows))) {
      row <- pending_rows[i, , drop = FALSE]
      dbExecute(
        con,
        "
        UPDATE article_lab_title_candidates
        SET status = ?,
            ready_for_human_rating = 0,
            promoted = CASE WHEN ? = 'api_scored' THEN promoted ELSE 0 END,
            archived = 0
        WHERE candidate_id = ?
        ",
        params = list(
          row$recovered_status[[1]],
          row$recovered_status[[1]],
          row$candidate_id[[1]]
        )
      )
    }
    for (current_batch_id in unique(pending_rows$batch_id)) {
      article_lab_update_batch_status(con, current_batch_id)
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  invisible(nrow(pending_rows))
}

article_lab_upsert_score <- function(con, score_row) {
  combined_score <- article_lab_combined_title_score(
    curiosity = score_row$curiosity[[1]],
    emotional_pull = score_row$emotional_pull[[1]],
    medium_comment_potential = score_row$medium_comment_potential[[1]],
    overall_article_potential = score_row$overall_article_potential[[1]],
    trust_risk = score_row$trust_risk[[1]],
    title_char_count = score_row$title_char_count[[1]]
  )
  dbExecute(
    con,
    "
    INSERT OR REPLACE INTO article_lab_title_api_scores
    (score_id, candidate_id, batch_id, scored_at, model, reasoning_effort, reasoning_mode, prompt_version, scope,
     clarity, curiosity, specificity, beginner_appeal, credibility, emotional_pull,
     promise_strength, trust_risk, medium_clap_potential, medium_comment_potential,
     overall_article_potential, combined_title_score, predicted_success_bucket,
     short_reason, raw_json, error)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      article_lab_score_id(score_row$candidate_id[[1]]),
      score_row$candidate_id[[1]],
      score_row$batch_id[[1]],
      score_row$scored_at[[1]] %||% now_utc(),
      score_row$model[[1]] %||% article_lab_default_score_model,
      score_row$reasoning_effort[[1]] %||% NA_character_,
      score_row$reasoning_mode[[1]] %||% "standard",
      score_row$prompt_version[[1]] %||% article_lab_default_score_prompt_version,
      score_row$scope[[1]] %||% article_lab_default_score_scope,
      score_row$clarity[[1]],
      score_row$curiosity[[1]],
      score_row$specificity[[1]],
      score_row$beginner_appeal[[1]],
      score_row$credibility[[1]],
      score_row$emotional_pull[[1]],
      score_row$promise_strength[[1]],
      score_row$trust_risk[[1]],
      score_row$medium_clap_potential[[1]],
      score_row$medium_comment_potential[[1]],
      score_row$overall_article_potential[[1]],
      combined_score,
      score_row$predicted_success_bucket[[1]],
      score_row$short_reason[[1]],
      score_row$raw_json[[1]],
      NA_character_
    )
  )
  combined_score
}

article_lab_score_batch <- function(con, batch_id, model, prompt_version, scope, reasoning_effort = NA_character_, reasoning_mode = "standard", candidate_ids = NULL) {
  article_lab_recover_api_pending_candidates(con, batch_id = batch_id)
  batch_label <- if (identical(batch_id, article_lab_all_batches_value)) "all titles" else paste("batch", batch_id)
  selected_ids <- clean_text(candidate_ids)
  selected_ids <- unique(selected_ids[!is.na(selected_ids)])
  if (length(selected_ids) == 0) {
    return(list(
      scored_n = 0L,
      used_existing_n = 0L,
      failed_n = 0L,
      failed_ids = character(),
      batch_label = batch_label,
      message = "Select at least one API-queue title to score."
    ))
  }

  candidates <- load_article_lab_scoring_rows(con, batch_id, model, prompt_version, scope)
  if (nrow(candidates) == 0) {
    return(list(
      scored_n = 0L,
      used_existing_n = 0L,
      failed_n = 0L,
      failed_ids = character(),
      batch_label = batch_label,
      message = sprintf("No titles in %s are available for API scoring.", batch_label)
    ))
  }
  candidates <- article_lab_normalize_candidate_rows(candidates)
  candidates <- candidates[candidates$candidate_id %in% selected_ids, , drop = FALSE]
  if (nrow(candidates) == 0) {
    return(list(
      scored_n = 0L,
      used_existing_n = 0L,
      failed_n = 0L,
      failed_ids = character(),
      batch_label = batch_label,
      message = "None of the selected titles were found in the current API queue selection."
    ))
  }

  eligible <- candidates[candidates$normalized_status == "ready_for_api_scoring", , drop = FALSE]
  skipped_n <- length(selected_ids) - nrow(eligible)
  if (nrow(eligible) == 0) {
    return(list(
      scored_n = 0L,
      used_existing_n = 0L,
      failed_n = 0L,
      failed_ids = character(),
      skipped_n = skipped_n,
      batch_label = batch_label,
      message = "Only titles in API queue can be scored."
    ))
  }

  cached_rows <- eligible[!is.na(eligible$score_id), , drop = FALSE]
  api_rows <- eligible[is.na(eligible$score_id), c("candidate_id", "batch_id", "title", "status", "title_char_count", "title_length_flag"), drop = FALSE]
  previous_status <- setNames(api_rows$status, api_rows$candidate_id)

  result <- list(
    scores = data.frame(),
    errors = list(),
    model = article_lab_input_string(model) %||% article_lab_default_score_model,
    prompt_version = article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
    scope = article_lab_input_string(scope) %||% article_lab_default_score_scope
  )

  if (nrow(api_rows) > 0) {
    placeholders <- paste(rep("?", nrow(api_rows)), collapse = ", ")
    dbExecute(
      con,
      sprintf("UPDATE article_lab_title_candidates SET status = 'api_pending' WHERE candidate_id IN (%s)", placeholders),
      params = as.list(api_rows$candidate_id)
    )

    result <- tryCatch(
      article_lab_score_api_request(api_rows, model = model, reasoning_effort = reasoning_effort, reasoning_mode = reasoning_mode, prompt_version = prompt_version, scope = scope),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      for (candidate_id in names(previous_status)) {
        dbExecute(
          con,
          "UPDATE article_lab_title_candidates SET status = ? WHERE candidate_id = ?",
          params = list(previous_status[[candidate_id]], candidate_id)
        )
      }
      stop(result)
    }
  }

  scored_ids <- character()
  failed_ids <- character()
  cached_ids <- cached_rows$candidate_id
  batch_ids_to_update <- unique(eligible$batch_id)

  dbBegin(con)
  tryCatch({
    if (length(cached_ids) > 0) {
      dbExecute(
        con,
        sprintf(
          "UPDATE article_lab_title_candidates
           SET status = 'api_scored', ready_for_human_rating = 0, promoted = 0, archived = 0
           WHERE candidate_id IN (%s)",
          paste(rep("?", length(cached_ids)), collapse = ", ")
        ),
        params = as.list(cached_ids)
      )
      scored_ids <- c(scored_ids, cached_ids)
    }

    if (nrow(result$scores) > 0) {
      for (i in seq_len(nrow(result$scores))) {
        score_row <- result$scores[i, , drop = FALSE]
        match_index <- match(score_row$candidate_id[[1]], eligible$candidate_id)
        score_row$title_char_count <- eligible$title_char_count[[match_index]]
        article_lab_upsert_score(con, score_row)
        dbExecute(
          con,
          "
          UPDATE article_lab_title_candidates
          SET status = CASE
            WHEN status = 'archived' OR archived = 1 THEN 'archived'
            WHEN status = 'approved_for_subtitle' OR promoted = 1 THEN 'approved_for_subtitle'
            ELSE 'api_scored'
          END,
              ready_for_human_rating = 0,
              archived = 0
          WHERE candidate_id = ?
          ",
          params = list(score_row$candidate_id[[1]])
        )
        scored_ids <- c(scored_ids, score_row$candidate_id[[1]])
      }
    }

    if (length(result$errors) > 0) {
      failed_ids <- vapply(result$errors, function(entry) article_lab_input_string(entry$candidate_id) %||% NA_character_, character(1))
      failed_ids <- failed_ids[!is.na(failed_ids)]
    }
    untouched_ids <- setdiff(api_rows$candidate_id, union(scored_ids, failed_ids))
    failed_ids <- unique(c(failed_ids, untouched_ids))

    for (candidate_id in failed_ids) {
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET status = ? WHERE candidate_id = ?",
        params = list(previous_status[[candidate_id]] %||% "generated", candidate_id)
      )
    }

    for (batch_id_value in batch_ids_to_update) {
      article_lab_update_batch_status(con, batch_id_value)
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(
    scored_n = length(scored_ids),
    used_existing_n = length(cached_ids),
    failed_n = length(failed_ids),
    failed_ids = failed_ids,
    skipped_n = skipped_n,
    model = result$model,
    prompt_version = result$prompt_version,
    scope = result$scope,
    batch_label = batch_label,
    message = if (length(scored_ids) == 0 && length(failed_ids) > 0) {
      sprintf("Scoring failed for %s title%s. No titles were updated.", length(failed_ids), ifelse(length(failed_ids) == 1, "", "s"))
    } else {
      NULL
    }
  )
}

article_lab_save_generate_triage <- function(con, updates) {
  if (length(updates) == 0) return(character())
  batch_ids <- character()
  dbBegin(con)
  tryCatch({
    for (update in updates) {
      status_value <- article_lab_normalize_candidate_status(update$status)
      if (!(status_value %in% c("generated", "disqualified"))) status_value <- "generated"
      dbExecute(
        con,
        "
        UPDATE article_lab_title_candidates
        SET status = ?,
            notes = ?,
            ready_for_human_rating = 0,
            promoted = 0,
            archived = 0
        WHERE candidate_id = ?
        ",
        params = list(status_value, clean_text(update$notes), update$candidate_id)
      )
      batch_row <- dbGetQuery(
        con,
        "SELECT batch_id FROM article_lab_title_candidates WHERE candidate_id = ? LIMIT 1",
        params = list(update$candidate_id)
      )
      if (nrow(batch_row) > 0) batch_ids <- c(batch_ids, batch_row$batch_id[[1]])
    }
    for (batch_id in unique(batch_ids)) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  unique(batch_ids)
}

article_lab_move_candidates_to_api_queue <- function(con, candidate_ids) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) return(list(moved_n = 0L, skipped_n = 0L, batch_ids = character()))

  placeholders <- paste(rep("?", length(candidate_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT candidate_id, batch_id, status, ready_for_human_rating, promoted, archived FROM article_lab_title_candidates WHERE candidate_id IN (%s)",
      placeholders
    ),
    params = as.list(candidate_ids)
  )
  if (nrow(rows) == 0) return(list(moved_n = 0L, skipped_n = length(candidate_ids), batch_ids = character()))
  rows <- article_lab_normalize_candidate_rows(rows)
  eligible_ids <- rows$candidate_id[rows$normalized_status == "generated"]
  skipped_n <- length(candidate_ids) - length(eligible_ids)
  batch_ids <- unique(rows$batch_id)

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf(
          "UPDATE article_lab_title_candidates SET status = 'ready_for_api_scoring', ready_for_human_rating = 0, promoted = 0, archived = 0 WHERE candidate_id IN (%s)",
          paste(rep("?", length(eligible_ids)), collapse = ", ")
        ),
        params = as.list(eligible_ids)
      )
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(moved_n = length(eligible_ids), skipped_n = skipped_n, batch_ids = batch_ids)
}

article_lab_approve_candidates_for_subtitle <- function(con, candidate_ids) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) return(list(approved_n = 0L, skipped_n = 0L, batch_ids = character()))

  placeholders <- paste(rep("?", length(candidate_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT candidate_id, batch_id, status, ready_for_human_rating, promoted, archived FROM article_lab_title_candidates WHERE candidate_id IN (%s)",
      placeholders
    ),
    params = as.list(candidate_ids)
  )
  if (nrow(rows) == 0) return(list(approved_n = 0L, skipped_n = length(candidate_ids), batch_ids = character()))
  rows <- article_lab_normalize_candidate_rows(rows)
  eligible_ids <- rows$candidate_id[rows$normalized_status == "api_scored"]
  skipped_n <- length(candidate_ids) - length(eligible_ids)
  batch_ids <- unique(rows$batch_id)

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf(
          "UPDATE article_lab_title_candidates SET status = 'approved_for_subtitle', promoted = 1, ready_for_human_rating = 0, archived = 0 WHERE candidate_id IN (%s)",
          paste(rep("?", length(eligible_ids)), collapse = ", ")
        ),
        params = as.list(eligible_ids)
      )
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(approved_n = length(eligible_ids), skipped_n = skipped_n, batch_ids = batch_ids)
}

article_lab_archive_api_scored_candidates <- function(con, candidate_ids) {
  candidate_ids <- clean_text(candidate_ids)
  candidate_ids <- unique(candidate_ids[!is.na(candidate_ids)])
  if (length(candidate_ids) == 0) return(list(archived_n = 0L, skipped_n = 0L, batch_ids = character()))

  placeholders <- paste(rep("?", length(candidate_ids)), collapse = ", ")
  rows <- dbGetQuery(
    con,
    sprintf(
      "SELECT candidate_id, batch_id, status, ready_for_human_rating, promoted, archived FROM article_lab_title_candidates WHERE candidate_id IN (%s)",
      placeholders
    ),
    params = as.list(candidate_ids)
  )
  if (nrow(rows) == 0) return(list(archived_n = 0L, skipped_n = length(candidate_ids), batch_ids = character()))
  rows <- article_lab_normalize_candidate_rows(rows)
  eligible_ids <- rows$candidate_id[rows$normalized_status %in% c("ready_for_api_scoring", "api_scored", "approved_for_subtitle", "ready_for_outline", "ready_for_draft")]
  skipped_n <- length(candidate_ids) - length(eligible_ids)
  batch_ids <- unique(rows$batch_id)

  dbBegin(con)
  tryCatch({
    if (length(eligible_ids) > 0) {
      dbExecute(
        con,
        sprintf(
          "UPDATE article_lab_title_candidates SET status = 'archived', promoted = 0, ready_for_human_rating = 0, archived = 1 WHERE candidate_id IN (%s)",
          paste(rep("?", length(eligible_ids)), collapse = ", ")
        ),
        params = as.list(eligible_ids)
      )
    }
    for (batch_id in batch_ids) article_lab_update_batch_status(con, batch_id)
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  list(archived_n = length(eligible_ids), skipped_n = skipped_n, batch_ids = batch_ids)
}

save_article_lab_batch <- function(con, prompt, seed_topic, inspiration_source, requested_batch_size, model, titles, reasoning_effort = NA_character_, reasoning_mode = "standard", raw_json = NA_character_, generation_mode = "generated", enforce_max_chars = TRUE, notes_extra = NULL, article_context_notes = NULL, article_project_id = NULL) {
  if (length(titles) == 0) return(invisible(NULL))
  validated <- article_lab_validate_titles(titles, max_chars = article_lab_title_max_chars)
  title_values <- validated$titles
  if (length(title_values) == 0) {
    stop(sprintf("No titles met the %s-character maximum.", article_lab_title_max_chars), call. = FALSE)
  }
  batch_id <- article_lab_batch_id()
  created_at <- now_utc()
  prompt_value <- clean_text(prompt)
  if (length(prompt_value) == 0 || is.na(prompt_value[[1]])) prompt_value <- article_lab_default_prompt
  seed_topic_value <- clean_text(seed_topic)
  inspiration_value <- clean_text(inspiration_source)
  model_value <- clean_text(model)
  if (length(model_value) == 0 || is.na(model_value[[1]])) model_value <- article_lab_default_model
  requested_size <- suppressWarnings(as.integer(requested_batch_size))
  if (is.na(requested_size) || requested_size < 1L) requested_size <- length(title_values)
  project_id <- article_lab_input_string(article_project_id)
  if (is.null(project_id) || !nzchar(project_id)) stop("Select an article project before saving a production title batch.", call. = FALSE)
  if (!dbExistsTable(con, "article_projects") || dbGetQuery(con, "SELECT COUNT(*) AS n FROM article_projects WHERE article_project_id = ?", params = list(project_id))$n[[1]] != 1L) {
    stop("The selected article project no longer exists. No title batch was saved.", call. = FALSE)
  }

  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      "INSERT INTO article_lab_title_batches
       (batch_id, article_project_id, created_at, prompt, seed_topic, inspiration_source, requested_batch_size, model, reasoning_effort, reasoning_mode, status, notes, article_context_notes)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        batch_id,
        project_id,
        created_at,
        prompt_value[[1]],
        if (length(seed_topic_value) == 0) NA_character_ else seed_topic_value[[1]],
        if (length(inspiration_value) == 0) NA_character_ else inspiration_value[[1]],
        requested_size,
        model_value[[1]],
        article_lab_input_string(reasoning_effort) %||% NA_character_,
        article_lab_input_string(reasoning_mode) %||% "standard",
        "generated",
        paste(
          sprintf("Generation mode: %s.", generation_mode),
          "Article Lab candidates stay generated until manual triage moves selected titles into ready_for_api_scoring.",
          notes_extra %||% ""
        ),
        article_lab_input_multiline(article_context_notes) %||% NA_character_
      )
    )

    for (i in seq_along(title_values)) {
      title_value <- clean_text(title_values[[i]])
      if (length(title_value) == 0 || is.na(title_value[[1]])) next
      title_char_count <- article_lab_title_length(title_value[[1]])
      title_length_flag <- article_lab_title_length_flag(title_char_count)
      dbExecute(
        con,
        "INSERT INTO article_lab_title_candidates
         (candidate_id, batch_id, created_at, title, title_char_count, title_length_flag, status, source, ready_for_human_rating, promoted, archived, notes, raw_json)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          article_lab_candidate_id(batch_id, i),
          batch_id,
          created_at,
          title_value[[1]],
          title_char_count,
          title_length_flag,
          "generated",
          "article_lab",
          0L,
          0L,
          0L,
          NA_character_,
          raw_json
        )
      )
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })

  batch_id
}


article_lab_update_candidate_notes <- function(con, notes_updates) {
  if (length(notes_updates) == 0) return(0L)
  updated_n <- 0L
  dbBegin(con)
  tryCatch({
    for (entry in notes_updates) {
      candidate_id <- clean_text(entry$candidate_id)
      if (length(candidate_id) == 0 || is.na(candidate_id[[1]])) next
      dbExecute(
        con,
        "UPDATE article_lab_title_candidates SET notes = ? WHERE candidate_id = ?",
        params = list(clean_text(entry$notes), candidate_id[[1]])
      )
      updated_n <- updated_n + 1L
    }
    dbCommit(con)
  }, error = function(e) {
    dbRollback(con)
    stop(e)
  })
  updated_n
}
