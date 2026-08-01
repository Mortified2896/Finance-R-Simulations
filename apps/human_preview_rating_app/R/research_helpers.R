

research_workflow_sort_sql <- "CASE WHEN manual_sort_order IS NULL THEN 1 ELSE 0 END, manual_sort_order ASC, updated_at DESC"
research_source_sort_sql <- "CASE WHEN s.manual_sort_order IS NULL THEN 1 ELSE 0 END, s.manual_sort_order ASC, s.updated_at DESC"
research_ranked_source_sort_sql <- "s.manual_sort_order ASC, s.updated_at DESC"
research_unranked_source_sort_sql <- "s.updated_at DESC"
research_angle_sort_sql <- "CASE WHEN a.manual_sort_order IS NULL THEN 1 ELSE 0 END, a.manual_sort_order ASC, a.updated_at DESC"

load_research_sources <- function(con, status = "__all__", ranked = NULL) {
  if (!dbExistsTable(con, "research_sources")) return(data.frame())
  status_value <- research_input_value(status)
  where <- character()
  params <- list()
  if (!is.na(status_value) && !identical(status_value, "__all__")) {
    where <- c(where, "s.status = ?")
    params <- c(params, list(status_value))
  } else if (!is.null(ranked)) {
    where <- c(where, "s.status NOT IN ('used', 'archived')")
  }
  if (isTRUE(ranked)) {
    where <- c(where, "s.manual_sort_order IS NOT NULL")
    order_sql <- research_ranked_source_sort_sql
  } else if (identical(ranked, FALSE)) {
    where <- c(where, "s.manual_sort_order IS NULL")
    order_sql <- research_unranked_source_sort_sql
  } else {
    order_sql <- research_source_sort_sql
  }
  source_query <- "
    SELECT s.*, COUNT(a.research_angle_id) AS angles_count
    FROM research_sources s
    LEFT JOIN research_article_angles a ON a.research_source_id = s.research_source_id
  "
  where_sql <- if (length(where) > 0) paste0(" WHERE ", paste(where, collapse = " AND ")) else ""
  query <- paste0(source_query, where_sql, " GROUP BY s.research_source_id ORDER BY ", order_sql)
  if (length(params) > 0) dbGetQuery(con, query, params = params) else dbGetQuery(con, query)
}

load_research_source_by_id <- function(con, source_id) {
  source_id_value <- research_input_integer(source_id)
  if (is.na(source_id_value) || !dbExistsTable(con, "research_sources")) return(data.frame())
  dbGetQuery(con, "
    SELECT s.*, COUNT(a.research_angle_id) AS angles_count
    FROM research_sources s
    LEFT JOIN research_article_angles a ON a.research_source_id = s.research_source_id
    WHERE s.research_source_id = ?
    GROUP BY s.research_source_id
    LIMIT 1
  ", params = list(source_id_value))
}

load_research_angles <- function(con, source_id = NULL, include_completed = FALSE) {
  if (!dbExistsTable(con, "research_article_angles")) return(data.frame())
  source_id_value <- research_input_integer(source_id)
  angle_query <- paste0("
    SELECT a.*, s.source_title, s.source_url, s.pdf_url, s.main_idea AS source_main_idea, s.abstract AS source_abstract
    FROM research_article_angles a
    LEFT JOIN research_sources s ON s.research_source_id = a.research_source_id
  ")
  active_filter <- if (isTRUE(include_completed)) "" else "a.status NOT IN ('sent_to_title_lab', 'archived')"
  if (!is.na(source_id_value)) {
    where_sql <- paste(c("a.research_source_id = ?", active_filter[nzchar(active_filter)]), collapse = " AND ")
    return(dbGetQuery(con, paste0(angle_query, " WHERE ", where_sql, " ORDER BY ", research_angle_sort_sql), params = list(source_id_value)))
  }
  where_sql <- if (nzchar(active_filter)) paste0(" WHERE ", active_filter) else ""
  dbGetQuery(con, paste0(angle_query, where_sql, " ORDER BY ", research_angle_sort_sql))
}

research_truncate <- function(value, max_chars = 90L) {
  value <- research_input_value(value)
  if (is.na(value)) return("")
  if (nchar(value, type = "chars") <= max_chars) return(value)
  paste0(substr(value, 1L, max_chars - 3L), "...")
}

research_link <- function(url, label) {
  value <- research_input_value(url)
  if (is.na(value)) return("")
  sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">%s</a>', htmltools::htmlEscape(value), htmltools::htmlEscape(label))
}

research_links <- function(source_url, pdf_url) {
  links <- c(research_link(source_url, "Open"), research_link(pdf_url, "PDF"))
  links <- links[nzchar(links)]
  paste(links, collapse = " &middot; ")
}

research_summary_template <- paste(
  "Short summary:",
  "",
  "Main findings:",
  "",
  "Why it matters for investors:",
  "",
  "Interesting details:",
  "",
  "Caveats / limitations:",
  "",
  "What not to overclaim:",
  "",
  "Possible article directions:",
  sep = "\n"
)

load_research_source_summary <- function(con, source_id, status = NULL) {
  source_id_value <- research_input_integer(source_id)
  if (is.na(source_id_value) || !dbExistsTable(con, "research_source_summaries")) return(data.frame())
  status_value <- research_input_value(status)
  if (!is.na(status_value)) {
    return(dbGetQuery(con, "
      SELECT *
      FROM research_source_summaries
      WHERE research_source_id = ? AND status = ?
      ORDER BY updated_at DESC, summary_id DESC
      LIMIT 1
    ", params = list(source_id_value, status_value)))
  }
  dbGetQuery(con, "
    SELECT *
    FROM research_source_summaries
    WHERE research_source_id = ?
    ORDER BY CASE status WHEN 'draft' THEN 0 WHEN 'confirmed' THEN 1 ELSE 2 END, updated_at DESC, summary_id DESC
    LIMIT 1
  ", params = list(source_id_value))
}

load_confirmed_research_summaries <- function(con) {
  if (!dbExistsTable(con, "research_source_summaries") || !dbExistsTable(con, "research_sources")) return(data.frame())
  dbGetQuery(con, "
    SELECT ss.*, s.source_title, s.source_url, s.pdf_url, s.main_idea, s.abstract,
      s.status AS source_status, s.manual_sort_order
    FROM research_source_summaries ss
    JOIN research_sources s ON s.research_source_id = ss.research_source_id
    WHERE ss.status = 'confirmed'
    ORDER BY CASE WHEN s.manual_sort_order IS NULL THEN 1 ELSE 0 END,
      s.manual_sort_order ASC, ss.confirmed_at DESC, ss.updated_at DESC, ss.summary_id DESC
  ")
}

research_summary_prompt <- function(summary_row) {
  paste(
    paste("Source title:", summary_row$source_title[[1]] %||% ""),
    paste("Source link:", summary_row$source_url[[1]] %||% ""),
    paste("PDF link:", summary_row$pdf_url[[1]] %||% ""),
    paste("Confirmed summary:", summary_row$summary_text[[1]] %||% ""),
    sep = "\n\n"
  )
}

article_lab_research_summary_id_from_source <- function(inspiration_source) {
  value <- article_lab_input_string(inspiration_source)
  if (length(value) != 1L || is.na(value) || !isTRUE(grepl("^research_summary:[0-9]+$", value))) return(NA_integer_)
  suppressWarnings(as.integer(sub("^research_summary:", "", value)))
}

load_article_lab_batch_summary_contexts <- function(con, batch_ids) {
  batch_ids <- clean_text(batch_ids)
  batch_ids <- unique(batch_ids[!is.na(batch_ids)])
  if (length(batch_ids) == 0 || !dbExistsTable(con, "article_lab_title_batches")) return(data.frame())
  placeholders <- paste(rep("?", length(batch_ids)), collapse = ", ")
  batches <- dbGetQuery(
    con,
    sprintf("SELECT batch_id, inspiration_source FROM article_lab_title_batches WHERE batch_id IN (%s)", placeholders),
    params = as.list(batch_ids)
  )
  if (nrow(batches) == 0) return(data.frame())
  batches$summary_id <- vapply(batches$inspiration_source, article_lab_research_summary_id_from_source, integer(1))

  research_angle_id_from_source <- function(src) {
    value <- article_lab_input_string(src)
    if (length(value) != 1L || is.na(value) || !isTRUE(grepl("^research_angle:[0-9]+$", value))) return(NA_integer_)
    suppressWarnings(as.integer(sub("^research_angle:", "", value)))
  }
  angle_ids <- vapply(batches$inspiration_source, research_angle_id_from_source, integer(1))
  if (any(!is.na(angle_ids)) && dbExistsTable(con, "research_article_angles") && dbExistsTable(con, "research_source_summaries")) {
    angle_id_list <- unique(angle_ids[!is.na(angle_ids)])
    ap <- paste(rep("?", length(angle_id_list)), collapse = ", ")
    angles <- dbGetQuery(
      con,
      sprintf("SELECT research_angle_id, research_source_id FROM research_article_angles WHERE research_angle_id IN (%s)", ap),
      params = as.list(angle_id_list)
    )
    if (nrow(angles) > 0) {
      source_ids <- unique(angles$research_source_id)
      sp <- paste(rep("?", length(source_ids)), collapse = ", ")
      angle_summaries <- dbGetQuery(
        con,
        sprintf("SELECT summary_id, research_source_id FROM research_source_summaries WHERE research_source_id IN (%s)", sp),
        params = as.list(source_ids)
      )
      if (nrow(angle_summaries) > 0) {
        angle_summaries <- angle_summaries[!duplicated(angle_summaries$research_source_id), , drop = FALSE]
        src_map <- angles$research_source_id[match(angle_ids, angles$research_angle_id)]
        smry_map <- angle_summaries$summary_id[match(src_map, angle_summaries$research_source_id)]
        batches$summary_id[!is.na(angle_ids) & is.na(batches$summary_id)] <- smry_map[!is.na(angle_ids) & is.na(batches$summary_id)]
      }
    }
  }

  batches <- batches[!is.na(batches$summary_id), , drop = FALSE]
  if (nrow(batches) == 0 || !dbExistsTable(con, "research_source_summaries") || !dbExistsTable(con, "research_sources")) return(data.frame())

  summary_ids <- unique(batches$summary_id)
  summary_placeholders <- paste(rep("?", length(summary_ids)), collapse = ", ")
  summaries <- dbGetQuery(
    con,
    sprintf(
      "SELECT ss.summary_id, ss.summary_text, ss.research_source_id, s.source_title, s.source_url, s.pdf_url
       FROM research_source_summaries ss
       JOIN research_sources s ON s.research_source_id = ss.research_source_id
       WHERE ss.summary_id IN (%s)",
      summary_placeholders
    ),
    params = as.list(summary_ids)
  )
  if (nrow(summaries) == 0) return(data.frame())

  contexts <- merge(batches[, c("batch_id", "summary_id"), drop = FALSE], summaries, by = "summary_id", all.x = FALSE, all.y = FALSE)
  contexts$pdf_status <- NA_character_
  contexts$pdf_local_path <- NA_character_
  if (dbExistsTable(con, "research_source_assets")) {
    source_ids <- unique(contexts$research_source_id)
    source_placeholders <- paste(rep("?", length(source_ids)), collapse = ", ")
    assets <- dbGetQuery(
      con,
      sprintf(
        "SELECT research_source_id, status, local_path
         FROM research_source_assets
         WHERE asset_type = 'pdf' AND research_source_id IN (%s)
         ORDER BY CASE WHEN status IN ('downloaded', 'uploaded') THEN 0 ELSE 1 END, updated_at DESC",
        source_placeholders
      ),
      params = as.list(source_ids)
    )
    if (nrow(assets) > 0) {
      assets <- assets[!duplicated(assets$research_source_id), , drop = FALSE]
      matched_assets <- match(contexts$research_source_id, assets$research_source_id)
      contexts$pdf_status <- assets$status[matched_assets]
      contexts$pdf_local_path <- assets$local_path[matched_assets]
    }
  }
  contexts$article_summary <- vapply(seq_len(nrow(contexts)), function(i) research_summary_prompt(contexts[i, , drop = FALSE]), character(1))
  contexts[, c("batch_id", "summary_id", "source_title", "source_url", "pdf_url", "article_summary", "pdf_status", "pdf_local_path"), drop = FALSE]
}

research_pdf_dir <- file.path(project_root, "data", "research_pdfs")

research_pdf_status_labels <- c(
  missing = "Missing",
  downloaded = "Downloaded",
  uploaded = "Uploaded manually",
  failed = "Download failed"
)

load_research_summary_prompt <- function(con, prompt_version) {
  version <- article_lab_input_string(prompt_version) %||% article_lab_default_research_summary_prompt_version
  if (!dbExistsTable(con, "research_summary_prompts")) return(article_lab_default_research_summary_prompt)
  rows <- dbGetQuery(con, "
    SELECT prompt_text
    FROM research_summary_prompts
    WHERE prompt_version = ?
    LIMIT 1
  ", params = list(version))
  if (nrow(rows) == 0) return(article_lab_default_research_summary_prompt)
  stored <- article_lab_input_multiline(rows$prompt_text[[1]]) %||% article_lab_default_research_summary_prompt
  if (identical(stored, article_lab_legacy_default_research_summary_prompt)) article_lab_default_research_summary_prompt else stored
}

save_research_summary_prompt <- function(con, prompt_version, prompt_text) {
  version <- article_lab_input_string(prompt_version) %||% article_lab_default_research_summary_prompt_version
  text <- article_lab_input_multiline(prompt_text) %||% article_lab_default_research_summary_prompt
  timestamp <- now_utc()
  rows <- dbGetQuery(con, "SELECT prompt_version FROM research_summary_prompts WHERE prompt_version = ? LIMIT 1", params = list(version))
  if (nrow(rows) > 0) {
    dbExecute(con, "
      UPDATE research_summary_prompts
      SET updated_at = ?, prompt_text = ?
      WHERE prompt_version = ?
    ", params = list(timestamp, text, version))
    return(invisible(version))
  }
  dbExecute(con, "
    INSERT INTO research_summary_prompts (prompt_version, created_at, updated_at, prompt_text)
    VALUES (?, ?, ?, ?)
  ", params = list(version, timestamp, timestamp, text))
  invisible(version)
}

research_safe_file_slug <- function(value) {
  value <- research_input_default(value, "research-source")
  value <- iconv(value, to = "ASCII//TRANSLIT", sub = "")
  value <- tolower(gsub("[^a-z0-9]+", "-", value))
  value <- gsub("(^-+|-+$)", "", value)
  if (!nzchar(value)) "research-source" else substr(value, 1L, 80L)
}

research_pdf_local_path <- function(source_id, title, original_filename = NULL) {
  dir.create(research_pdf_dir, recursive = TRUE, showWarnings = FALSE)
  extension <- tolower(tools::file_ext(original_filename %||% ""))
  if (!identical(extension, "pdf")) extension <- "pdf"
  file.path(research_pdf_dir, sprintf("research_source_%s_%s.%s", as.integer(source_id), research_safe_file_slug(title), extension))
}

research_resolve_local_pdf_path <- function(path) {
  value <- research_input_value(path)
  if (is.na(value)) return(NA_character_)
  candidates <- if (grepl("^(/|[A-Za-z]:[/\\\\])", value)) {
    value
  } else {
    c(value, file.path(project_root, value))
  }
  for (candidate in candidates) {
    if (file.exists(candidate)) return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }
  value
}

research_pdf_sha256 <- function(path) {
  value <- tools::sha256sum(path)
  unname(as.character(value[[1]]))
}

research_file_is_pdf <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 5) return(FALSE)
  header <- readBin(path, what = "raw", n = 5L)
  identical(rawToChar(header), "%PDF-")
}

research_pdf_source_url <- function(source) {
  pdf_url <- research_input_value(source$pdf_url[[1]])
  if (!is.na(pdf_url)) return(pdf_url)
  source_url <- research_input_value(source$source_url[[1]])
  if (!is.na(source_url) && grepl("\\.pdf($|[?#])", source_url, ignore.case = TRUE)) return(source_url)
  NA_character_
}

load_research_pdf_asset <- function(con, source_id) {
  source_id_value <- research_input_integer(source_id)
  if (is.na(source_id_value) || !dbExistsTable(con, "research_source_assets")) return(data.frame())
  dbGetQuery(con, "
    SELECT *
    FROM research_source_assets
    WHERE research_source_id = ? AND asset_type = 'pdf'
    ORDER BY updated_at DESC, asset_id DESC
    LIMIT 1
  ", params = list(source_id_value))
}

save_research_pdf_asset <- function(con, source_id, status, source_url = NA_character_, local_path = NA_character_, original_filename = NA_character_, file_sha256 = NA_character_, error = NA_character_) {
  timestamp <- now_utc()
  existing <- load_research_pdf_asset(con, source_id)
  local_path <- research_resolve_local_pdf_path(local_path)
  values <- list(timestamp, source_url, local_path, original_filename, file_sha256, status, error)
  if (nrow(existing) > 0) {
    dbExecute(con, "
      UPDATE research_source_assets
      SET updated_at = ?, source_url = ?, local_path = ?, original_filename = ?, file_sha256 = ?, status = ?, error = ?
      WHERE asset_id = ?
    ", params = c(values, list(existing$asset_id[[1]])))
    return(existing$asset_id[[1]])
  }
  dbExecute(con, "
    INSERT INTO research_source_assets
      (research_source_id, created_at, updated_at, asset_type, source_url, local_path, original_filename, file_sha256, status, error)
    VALUES (?, ?, ?, 'pdf', ?, ?, ?, ?, ?, ?)
  ", params = list(source_id, timestamp, timestamp, source_url, local_path, original_filename, file_sha256, status, error))
  dbGetQuery(con, "SELECT last_insert_rowid() AS asset_id")$asset_id[[1]]
}

research_summary_api_request <- function(source, asset, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt_version = NA_character_, prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "summarize_research_pdf.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/summarize_research_pdf.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(source) == 0) stop("Select a source before generating a summary.", call. = FALSE)
  if (nrow(asset) == 0 || !(asset$status[[1]] %in% c("downloaded", "uploaded"))) stop("Download or upload a PDF before generating an API summary.", call. = FALSE)
  local_pdf_path <- research_resolve_local_pdf_path(asset$local_path[[1]])
  if (is.na(local_pdf_path) || !file.exists(local_pdf_path)) stop("The selected PDF asset does not exist on disk.", call. = FALSE)

  settings <- article_lab_validate_generation_settings(article_lab_input_string(model) %||% article_lab_default_research_summary_model, reasoning_effort, reasoning_mode)
  request_payload <- list(
    model = settings$model,
    reasoning_effort = settings$reasoning_effort,
    reasoning_mode = settings$reasoning_mode,
    prompt_version = article_lab_input_string(prompt_version) %||% article_lab_default_research_summary_prompt_version,
    prompt = article_lab_input_multiline(prompt) %||% article_lab_default_research_summary_prompt,
    research_source_id = source$research_source_id[[1]],
    source_title = article_lab_input_string(source$source_title[[1]]),
    source_url = article_lab_input_string(source$source_url[[1]]),
    pdf_url = article_lab_input_string(source$pdf_url[[1]]),
    main_idea = article_lab_input_multiline(source$main_idea[[1]]),
    abstract = article_lab_input_multiline(source$abstract[[1]]),
    local_pdf_path = local_pdf_path
  )

  request_file <- tempfile(pattern = "research_summary_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "research_summary_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "research_summary_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2("node", args = c(helper_path, request_file), stdout = stdout_file, stderr = stderr_file)
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Research summary helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Research summary helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  summary_text <- article_lab_input_multiline(parsed$summary_text)
  if (is.null(summary_text) || is.na(summary_text)) stop("Research summary helper returned no summary_text.", call. = FALSE)
  list(
    summary_text = summary_text,
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    reasoning_effort = article_lab_input_string(parsed$reasoning_effort) %||% settings$reasoning_effort,
    reasoning_mode = article_lab_input_string(parsed$reasoning_mode) %||% settings$reasoning_mode,
    prompt_version = article_lab_input_string(parsed$prompt_version) %||% request_payload$prompt_version,
    raw_json = stdout_text,
    response_id = article_lab_input_string(parsed$response_id)
  )
}

research_paperqa_resolve_python <- function() {
  env_candidates <- clean_text(c(
    Sys.getenv("ARTICLE_LAB_PAPERQA_PYTHON", unset = ""),
    Sys.getenv("ARTICLE_LAB_PYTHON", unset = ""),
    Sys.getenv("WRITING_API_PYTHON", unset = "")
  ))
  path_candidates <- clean_text(c(Sys.which("python3"), Sys.which("python")))
  candidates <- unique(c(
    env_candidates[!is.na(env_candidates)],
    path_candidates[!is.na(path_candidates)]
  ))
  if (length(candidates) == 0) {
    return(list(
      ok = FALSE,
      error = "No Python interpreter found for PaperQA2 chunk retrieval. Set ARTICLE_LAB_PAPERQA_PYTHON.",
      python_bin = NA_character_
    ))
  }
  for (candidate in candidates) {
    if (!file.exists(candidate)) next
    stdout_file <- tempfile(pattern = "paperqa_check_stdout_", fileext = ".log")
    stderr_file <- tempfile(pattern = "paperqa_check_stderr_", fileext = ".log")
    on.exit(unlink(c(stdout_file, stderr_file), force = TRUE), add = TRUE)
    check_code <- "import sys; print(sys.version)"
    status <- suppressWarnings(system2(
      candidate,
      args = c("-c", shQuote(check_code)),
      stdout = stdout_file,
      stderr = stderr_file
    ))
    if (is.numeric(status) && length(status) == 1 && !is.na(status) && status == 0) {
      return(list(ok = TRUE, python_bin = candidate))
    }
  }
  list(
    ok = FALSE,
    error = "No working Python interpreter found. Set ARTICLE_LAB_PAPERQA_PYTHON to a valid Python 3 binary.",
    python_bin = candidates[[1]] %||% NA_character_
  )
}

research_paperqa_chunks_request <- function(source, asset, query = NULL, chunk_chars = 1500L, chunk_overlap = 100L) {
  helper_path <- file.path("scripts", "writing_api", "paperqa_chunks.py")
  if (!file.exists(file.path(project_root, helper_path))) {
    stop("Missing helper script: scripts/writing_api/paperqa_chunks.py", call. = FALSE)
  }
  if (nrow(source) == 0) stop("Select a source before retrieving PaperQA2 chunks.", call. = FALSE)
  if (nrow(asset) == 0 || !(asset$status[[1]] %in% c("downloaded", "uploaded"))) {
    stop("Download or upload a PDF before retrieving PaperQA2 chunks.", call. = FALSE)
  }
  local_pdf_path <- research_resolve_local_pdf_path(asset$local_path[[1]])
  if (is.na(local_pdf_path) || !file.exists(local_pdf_path)) {
    stop("The selected PDF asset does not exist on disk.", call. = FALSE)
  }

  python_resolved <- research_paperqa_resolve_python()
  if (!isTRUE(python_resolved$ok)) {
    stop(python_resolved$error, call. = FALSE)
  }
  python_bin <- python_resolved$python_bin

  output_dir <- file.path(project_root, "data", "research_paperqa_chunks")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  query_text <- if (is.null(query) || !nzchar(trimws(query))) NULL else query

  request_payload <- list(
    local_pdf_path = local_pdf_path,
    research_source_id = source$research_source_id[[1]],
    source_title = article_lab_input_string(source$source_title[[1]]) %||% "untitled",
    query = query_text,
    output_dir = output_dir,
    chunk_chars = as.integer(chunk_chars),
    chunk_overlap = as.integer(chunk_overlap)
  )

  request_file <- tempfile(pattern = "paperqa_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "paperqa_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "paperqa_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2(python_bin, args = c(helper_path, request_file), stdout = stdout_file, stderr = stderr_file)
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""

  if (!nzchar(trimws(stdout_text))) {
    clean_msg <- clean_text(stderr_text) %||% "PaperQA2 helper returned no output."
    stop(clean_msg, call. = FALSE)
  }

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)

  if (identical(parsed$mode, "paperqa_missing")) {
    detail <- parsed$detail %||% "unknown"
    hint <- parsed$hint %||% "pip install paper-qa (requires Python >= 3.11)"
    warning_msg <- sprintf(
      "PaperQA2 not available (%s). %s. Set ARTICLE_LAB_PAPERQA_PYTHON to a Python 3.11+ environment with paper-qa installed.",
      detail, hint
    )
    warning(warning_msg)
    return(list(
      mode = "paperqa_missing",
      query = parsed$query %||% NULL,
      contexts = parsed$contexts %||% list(),
      chunks = parsed$chunks %||% list(),
      chunk_count = parsed$chunk_count %||% 0L,
      diagnostics = list(
        detail = detail,
        python_version = parsed$python_version %||% "unknown",
        python_executable = parsed$python_executable %||% "unknown",
        hint = hint
      ),
      warning = warning_msg,
      raw_json = stdout_text
    ))
  }

  if (!(identical(parsed$mode, "paperqa") || identical(parsed$mode, "paperqa_query")) || !is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    err_msg <- clean_text(parsed$error) %||% clean_text(stderr_text) %||% clean_text(stdout_text) %||% "PaperQA2 helper failed."
    stop(err_msg, call. = FALSE)
  }

  is_query_mode <- identical(parsed$mode, "paperqa_query")
  if (is_query_mode) {
    contexts <- parsed$contexts %||% list()
    if (!is.list(contexts)) contexts <- list()
    list(
      mode = parsed$mode,
      query = parsed$query,
      answer = parsed$answer %||% NULL,
      contexts = contexts,
      context_count = parsed$context_count %||% length(contexts),
      chunks_dir = parsed$chunks_dir %||% paste0(output_dir, "/"),
      chunks_file = parsed$chunks_file %||% NA_character_,
      paperqa_version = parsed$paperqa_version %||% "unknown",
      diagnostics = parsed$diagnostics %||% list(),
      raw_json = stdout_text
    )
  } else {
    chunks <- parsed$chunks %||% list()
    if (!is.list(chunks)) chunks <- list()
    list(
      mode = parsed$mode %||% "paperqa",
      chunks = chunks,
      chunk_count = parsed$chunk_count %||% length(chunks),
      chunks_dir = parsed$chunks_dir %||% paste0(output_dir, "/"),
      chunks_file = parsed$chunks_file %||% NA_character_,
      paperqa_version = parsed$paperqa_version %||% "unknown",
      diagnostics = parsed$diagnostics %||% list(),
      raw_json = stdout_text
    )
  }
}

research_evidence_render_template <- function(template, variables) {
  out <- article_lab_input_multiline(template) %||% ""
  for (name in names(variables)) {
    value <- variables[[name]]
    if (is.null(value) || length(value) == 0 || is.na(value[[1]])) value <- ""
    out <- gsub(paste0("\\{\\{", name, "\\}\\}"), as.character(value[[1]]), out, fixed = FALSE)
  }
  out
}

research_evidence_api_request <- function(step, resolved_prompt, model, reasoning_effort, summary_id, research_source_id, reasoning_mode = "standard") {
  helper_path <- file.path("scripts", "writing_api", "select_summary_evidence.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/select_summary_evidence.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  settings <- article_lab_validate_generation_settings(article_lab_input_string(model) %||% article_lab_default_evidence_selection_model, reasoning_effort, reasoning_mode)
  request_payload <- list(
    step = step,
    model = settings$model,
    reasoning_effort = settings$reasoning_effort,
    reasoning_mode = settings$reasoning_mode,
    resolved_prompt = article_lab_input_multiline(resolved_prompt),
    summary_id = summary_id,
    research_source_id = research_source_id
  )
  request_file <- tempfile(pattern = paste0(step, "_request_"), fileext = ".json")
  stdout_file <- tempfile(pattern = paste0(step, "_stdout_"), fileext = ".json")
  stderr_file <- tempfile(pattern = paste0(step, "_stderr_"), fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)
  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2("node", args = c(helper_path, request_file), stdout = stdout_file, stderr = stderr_file, timeout = 120L)
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Evidence helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Evidence helper returned no output.", call. = FALSE)
  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  parsed$raw_json <- stdout_text
  parsed
}

research_pdf_split_sentences <- function(text) {
  text <- gsub("\r\n?", "\n", text %||% "")
  text <- gsub("-\\s*\n\\s*", "", text)
  text <- gsub("\\s*\n\\s*", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  if (!nzchar(text)) return(character())
  parts <- unlist(strsplit(text, "(?<=[.!?])\\s+(?=[A-Z0-9(\\[])", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

research_summary_split_sentences <- function(text) {
  lines <- unlist(strsplit(article_lab_input_multiline(text) %||% "", "\n+", perl = TRUE), use.names = FALSE)
  out <- character()
  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line)) next
    if (grepl(":\\s*$", line)) {
      out <- c(out, line)
    } else {
      out <- c(out, research_pdf_split_sentences(line))
    }
  }
  out[nzchar(out)]
}

research_summary_sentence_payload <- function(summary_text) {
  sentences <- research_summary_split_sentences(summary_text)
  lapply(seq_along(sentences), function(i) list(sentence_index = i, sentence_text = sentences[[i]]))
}

research_claim_marker_id <- function(claim_id) paste0("research_claim_marker_", as.integer(claim_id))

research_summary_line_blocks <- function(text) {
  lines <- unlist(strsplit(article_lab_input_multiline(text) %||% "", "\n", fixed = TRUE), use.names = FALSE)
  blocks <- list()
  sentence_index <- 0L
  for (line in lines) {
    raw_line <- line
    trimmed <- trimws(line)
    if (!nzchar(trimmed)) {
      blocks[[length(blocks) + 1L]] <- list(type = "blank", prefix = "", units = list())
      next
    }
    prefix <- ""
    body <- trimmed
    type <- "paragraph"
    bullet_match <- regexpr("^([-*]|[0-9]+[.)])\\s+", body, perl = TRUE)
    if (bullet_match[[1]] == 1L) {
      prefix <- regmatches(body, bullet_match)
      body <- trimws(sub("^([-*]|[0-9]+[.)])\\s+", "", body, perl = TRUE))
      type <- "list_item"
    } else if (grepl(":\\s*$", body)) {
      type <- "heading"
    }
    parts <- if (identical(type, "heading")) body else research_pdf_split_sentences(body)
    units <- lapply(parts, function(part) {
      sentence_index <<- sentence_index + 1L
      list(sentence_index = sentence_index, text = part)
    })
    blocks[[length(blocks) + 1L]] <- list(type = type, prefix = prefix, units = units, raw = raw_line)
  }
  blocks
}

research_marker_status <- function(row) {
  if (is.null(row) || nrow(row) == 0 || is.na(row$evidence_id[[1]])) return("evidence_not_fetched")
  status <- row$selection_status[[1]] %||% "suggested"
  if (is.na(status) || !nzchar(status)) "suggested" else status
}

research_support_status <- function(value, sentence_ids = integer(), confidence = NULL) {
  status <- article_lab_input_string(value) %||% ""
  allowed <- c("supports", "partially_supports", "generally_supported_no_direct_quote", "weak_support", "contradicts", "no_match", "suggested", "verified", "rejected", "failed")
  if (!nzchar(status) || !(status %in% allowed)) {
    status <- if (length(sentence_ids) > 0 && !identical(confidence, "none")) "supports" else "no_match"
  }
  status
}

research_status_label <- function(status) {
  labels <- c(
    supports = "supports",
    partially_supports = "partially supports",
    generally_supported_no_direct_quote = "generally supported, no direct quote",
    weak_support = "weak support",
    contradicts = "contradicts",
    no_match = "no match",
    suggested = "suggested",
    verified = "verified",
    rejected = "rejected",
    failed = "failed",
    evidence_not_fetched = "evidence not fetched yet"
  )
  labels[[status]] %||% status
}

research_normalize_marker_text <- function(value) {
  text <- article_lab_input_string(value) %||% ""
  text <- tolower(gsub("\\s+", " ", trimws(text), perl = TRUE))
  if (!nzchar(text)) NA_character_ else text
}

research_marker_label <- function(indices) {
  values <- sort(unique(suppressWarnings(as.integer(indices))))
  values <- values[!is.na(values)]
  if (length(values) == 0) return("[]")
  if (length(values) > 1L && all(diff(values) == 1L)) {
    return(sprintf("[%s-%s]", values[[1]], values[[length(values)]]))
  }
  sprintf("[%s]", paste(values, collapse = ","))
}

research_group_marker_status <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) return("evidence_not_fetched")
  statuses <- vapply(seq_len(nrow(rows)), function(i) research_marker_status(rows[i, , drop = FALSE]), character(1))
  if (all(statuses == "evidence_not_fetched")) return("evidence_not_fetched")
  if (all(statuses == "verified")) return("verified")
  priority <- c("rejected", "contradicts", "no_match", "weak_support", "generally_supported_no_direct_quote", "partially_supports", "supports", "suggested", "verified")
  for (status in priority) {
    if (any(statuses == status)) return(status)
  }
  statuses[[1]]
}

research_group_status_summary <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) return("No claims")
  statuses <- vapply(seq_len(nrow(rows)), function(i) research_marker_status(rows[i, , drop = FALSE]), character(1))
  pieces <- c(sprintf("%s claim%s", nrow(rows), ifelse(nrow(rows) == 1L, "", "s")))
  for (status in unique(statuses)) {
    count <- sum(statuses == status)
    pieces <- c(pieces, sprintf("%s %s", count, research_status_label(status)))
  }
  paste(pieces, collapse = " · ")
}

research_prepare_evidence_marker_rows <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) return(rows)
  rows$display_index <- seq_len(nrow(rows))
  normalized_original <- vapply(rows$original_text, research_normalize_marker_text, character(1))
  rows$marker_group_key <- ifelse(
    !is.na(normalized_original) & nzchar(normalized_original),
    paste0("original:", normalized_original),
    paste0("sentence:", rows$claim_index)
  )
  rows
}

research_quote_rows_for_evidence <- function(con, evidence_id, legacy_sentence_id = NA_integer_) {
  evidence_id_value <- research_input_integer(evidence_id)
  if (!is.na(evidence_id_value) && dbExistsTable(con, "research_summary_claim_evidence_sentences")) {
    rows <- dbGetQuery(con, "
      SELECT es.sentence_id, es.quote_rank, COALESCE(es.page_number, s.page_number) AS page_number, s.sentence_text
      FROM research_summary_claim_evidence_sentences es
      JOIN research_pdf_sentences s ON s.sentence_id = es.sentence_id
      WHERE es.evidence_id = ?
      ORDER BY es.quote_rank ASC, es.evidence_sentence_id ASC
    ", params = list(evidence_id_value))
    if (nrow(rows) > 0) return(rows)
  }
  legacy_id <- research_input_integer(legacy_sentence_id)
  if (is.na(legacy_id)) return(data.frame())
  dbGetQuery(con, "
    SELECT sentence_id, 1 AS quote_rank, page_number, sentence_text
    FROM research_pdf_sentences
    WHERE sentence_id = ?
  ", params = list(legacy_id))
}

research_evidence_sentence_ids <- function(result) {
  values <- result$sentence_ids %||% result$sentence_id %||% list()
  if (is.null(values)) return(integer())
  if (!is.list(values)) values <- as.list(values)
  ids <- vapply(values, research_input_integer, integer(1))
  unique(ids[!is.na(ids)])
}

research_pdf_extract_pages <- function(pdf_path) {
  if (requireNamespace("pdftools", quietly = TRUE)) {
    pages <- pdftools::pdf_text(pdf_path)
    return(data.frame(page_number = seq_along(pages), text = pages, stringsAsFactors = FALSE))
  }
  helper_path <- file.path("scripts", "writing_api", "extract_pdf_text.mjs")
  if (!file.exists(file.path(project_root, helper_path))) {
    stop("No PDF text extractor is available. Install pdftools or restore scripts/writing_api/extract_pdf_text.mjs.", call. = FALSE)
  }
  stdout_file <- tempfile(pattern = "pdf_extract_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "pdf_extract_stderr_", fileext = ".log")
  on.exit(unlink(c(stdout_file, stderr_file), force = TRUE), add = TRUE)
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2("node", args = c(helper_path, pdf_path), stdout = stdout_file, stderr = stderr_file, timeout = 120L)
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "PDF text extraction helper failed.", call. = FALSE)
  }
  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  pages <- parsed$pages %||% list()
  data.frame(
    page_number = vapply(pages, function(page) {
      value <- suppressWarnings(as.integer(page$page_number))
      if (length(value) == 0 || is.na(value[[1]])) NA_integer_ else value[[1]]
    }, integer(1)),
    text = vapply(pages, function(page) article_lab_input_multiline(page$text) %||% "", character(1)),
    stringsAsFactors = FALSE
  )
}

research_extract_pdf_sentences <- function(con, source, asset) {
  if (nrow(source) == 0 || nrow(asset) == 0) stop("Select a source with a PDF before extracting sentences.", call. = FALSE)
  pdf_path <- research_resolve_local_pdf_path(asset$local_path[[1]])
  if (is.na(pdf_path) || !file.exists(pdf_path)) stop("The selected PDF asset does not exist on disk.", call. = FALSE)
  sha <- research_pdf_sha256(pdf_path)
  existing_n <- dbGetQuery(con, "
    SELECT COUNT(*) AS n
    FROM research_pdf_sentences
    WHERE research_source_id = ? AND file_sha256 = ? AND NULLIF(TRIM(sentence_text), '') IS NOT NULL
  ", params = list(source$research_source_id[[1]], sha))$n[[1]]
  if (!is.na(existing_n) && existing_n > 0) return(existing_n)

  pages <- research_pdf_extract_pages(pdf_path)
  timestamp <- now_utc()
  dbExecute(con, "DELETE FROM research_pdf_sentences WHERE research_source_id = ? AND asset_id = ?", params = list(source$research_source_id[[1]], asset$asset_id[[1]]))
  inserted <- 0L
  for (page_index in seq_len(nrow(pages))) {
    sentences <- research_pdf_split_sentences(pages$text[[page_index]])
    if (length(sentences) == 0) next
    keep <- nchar(sentences, type = "chars") >= 30L & nchar(sentences, type = "chars") <= 1200L
    sentences <- unique(sentences[keep])
    for (sentence_index in seq_along(sentences)) {
      dbExecute(con, "
        INSERT INTO research_pdf_sentences
          (asset_id, research_source_id, file_sha256, page_number, sentence_index, sentence_text, char_count, extracted_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(asset$asset_id[[1]], source$research_source_id[[1]], sha, pages$page_number[[page_index]], sentence_index, sentences[[sentence_index]], nchar(sentences[[sentence_index]], type = "chars"), timestamp))
      inserted <- inserted + 1L
    }
  }
  inserted
}

research_claim_terms <- function(claim_text) {
  words <- tolower(unlist(strsplit(gsub("[^A-Za-z0-9 ]+", " ", claim_text %||% ""), "\\s+"), use.names = FALSE))
  words <- words[nchar(words) >= 4L]
  stop_words <- c("this", "that", "with", "from", "they", "their", "there", "where", "which", "about", "would", "could", "should", "investor", "investors", "paper", "study", "shows", "finds")
  kept <- unique(words[!(words %in% stop_words)])
  if (length(kept) == 0) return(character())
  kept[seq_len(min(8L, length(kept)))]
}

research_candidate_sentences_for_claim <- function(con, source_id, claim_text, limit = 8L) {
  terms <- research_claim_terms(claim_text)
  if (length(terms) == 0) return(data.frame())
  rows <- dbGetQuery(con, "
    SELECT sentence_id, page_number, sentence_text, char_count
    FROM research_pdf_sentences
    WHERE research_source_id = ?
      AND char_count BETWEEN 40 AND 450
    ORDER BY page_number ASC, sentence_index ASC
  ", params = list(source_id))
  if (nrow(rows) == 0) return(rows)
  lower_sentences <- tolower(rows$sentence_text)
  scores <- vapply(lower_sentences, function(sentence) sum(vapply(terms, function(term) grepl(term, sentence, fixed = TRUE), logical(1))), numeric(1))
  rows$score <- scores
  rows <- rows[rows$score > 0, , drop = FALSE]
  if (nrow(rows) == 0) return(rows)
  rows <- rows[order(-rows$score, rows$char_count), , drop = FALSE]
  rows[seq_len(min(limit, nrow(rows))), , drop = FALSE]
}

research_current_summary_for_evidence <- function(con, source_id, summary_text) {
  source_id_value <- research_input_integer(source_id)
  if (is.na(source_id_value)) return(data.frame())
  text_value <- article_lab_input_multiline(summary_text)
  existing <- load_research_source_summary(con, source_id_value)
  if (nrow(existing) > 0 && identical(article_lab_input_multiline(existing$summary_text[[1]]), text_value)) return(existing)
  if (nrow(existing) > 0) return(existing)
  data.frame()
}

load_research_summary_evidence_rows <- function(con, summary_id) {
  summary_id_value <- research_input_integer(summary_id)
  if (is.na(summary_id_value) || !dbExistsTable(con, "research_summary_claims")) return(data.frame())
  dbGetQuery(con, "
    SELECT c.claim_id, c.claim_index, c.claim_text, c.original_text, c.placement_hint, c.importance, c.status AS claim_status,
      c.model AS marker_model, c.reasoning_effort AS marker_reasoning_effort,
      c.prompt_template AS marker_prompt_template, c.prompt_payload_json AS marker_prompt_payload_json,
      e.evidence_id, e.selection_status, e.confidence, e.selector_reason, e.model, e.reasoning_effort,
      e.prompt_template, e.prompt_payload_json, e.error_message, e.verified_at, e.notes,
      s.sentence_id, s.page_number, s.sentence_text
    FROM research_summary_claims c
    LEFT JOIN research_summary_claim_evidence e ON e.claim_id = c.claim_id
    LEFT JOIN research_pdf_sentences s ON s.sentence_id = e.sentence_id
    WHERE c.summary_id = ?
    ORDER BY c.claim_index ASC, c.claim_id ASC, e.updated_at DESC
  ", params = list(summary_id_value))
}

build_checked_summary_evidence <- function(con, summary_id) {
  if (is.na(research_input_integer(summary_id))) return(list())
  rows <- research_latest_evidence_by_claim(load_research_summary_evidence_rows(con, summary_id))
  if (nrow(rows) == 0) return(list())
  checked_statuses <- c("supports", "partially_supports", "verified")
  out <- list()
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    selection_status <- article_lab_input_string(row$selection_status[[1]]) %||% "suggested"
    quote_rows <- research_quote_rows_for_evidence(con, row$evidence_id[[1]], row$sentence_id[[1]])
    if (nrow(quote_rows) == 0) next
    sentence_ids <- vapply(quote_rows$sentence_id, function(value) {
      num <- suppressWarnings(as.integer(value))
      if (is.na(num)) "" else as.character(num)
    }, character(1))
    sentence_ids <- sentence_ids[nzchar(sentence_ids)]
    if (length(sentence_ids) == 0) next
    page_value <- quote_rows$page_number[[1]]
    page_text <- if (is.na(page_value)) NA_character_ else as.character(as.integer(page_value))
    supporting_quote <- quote_rows$sentence_text[[1]] %||% NA_character_
    if (is.na(supporting_quote) || !nzchar(supporting_quote)) next
    out[[length(out) + 1L]] <- list(
      claim_id = row$claim_id[[1]],
      claim_text = article_lab_input_string(row$claim_text[[1]]) %||% "",
      supporting_quote = supporting_quote,
      page = page_text,
      sentence_ids = sentence_ids,
      selection_status = selection_status,
      confidence = article_lab_input_string(row$confidence[[1]]) %||% NA_character_,
      evidence_status = if (selection_status %in% checked_statuses) "checked" else "unchecked"
    )
  }
  out
}

research_latest_evidence_by_claim <- function(rows) {
  if (nrow(rows) == 0) return(rows)
  rows[!duplicated(rows$claim_id), , drop = FALSE]
}

research_title_prompt <- function(source, angle) {
  source_context <- research_input_default(source$main_idea[[1]], research_input_default(source$abstract[[1]], ""))
  paste(
    "Generate reader-facing Medium titles, not academic paper summary titles.",
    "Stay credible, beginner-friendly, science-based, and do not overclaim what the source proves.",
    paste("Source title:", source$source_title[[1]] %||% ""),
    paste("Source link:", source$source_url[[1]] %||% ""),
    paste("PDF link:", source$pdf_url[[1]] %||% ""),
    paste("Main idea or abstract:", source_context),
    paste("Article angle title:", angle$angle_title[[1]] %||% ""),
    paste("Angle main idea:", angle$main_idea[[1]] %||% ""),
    sep = "\n\n"
  )
}
