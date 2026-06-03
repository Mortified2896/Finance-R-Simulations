required_packages <- c("shiny", "DBI", "RSQLite", "jsonlite", "DT")
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
library(jsonlite)
library(DT)

source("R/text_helpers.R", local = TRUE)
source("R/input_helpers.R", local = TRUE)
source("R/file_helpers.R", local = TRUE)
source("R/app_config.R", local = TRUE)
source("R/db_helpers.R", local = TRUE)
source("R/ui_helpers.R", local = TRUE)
source("R/ui_assets.R", local = TRUE)
source("R/status_helpers.R", local = TRUE)
source("R/workflow_helpers.R", local = TRUE)
source("R/scoring_helpers.R", local = TRUE)
source("R/title_subtitle_helpers.R", local = TRUE)
source("R/id_helpers.R", local = TRUE)
source("R/article_lab_config.R", local = TRUE)
source("R/schema_rating.R", local = TRUE)
source("R/schema_article_lab.R", local = TRUE)
source("R/schema_research.R", local = TRUE)



list_article_lab_prompt_keys <- function(con, default_key = article_lab_manual_prompt_key) {
  if (!dbExistsTable(con, "article_lab_prompts")) return(default_key)
  rows <- dbGetQuery(con, "
    SELECT prompt_key
    FROM article_lab_prompts
    WHERE prompt_key IS NOT NULL AND TRIM(prompt_key) <> ''
    ORDER BY updated_at DESC, prompt_key ASC
  ")
  keys <- unique(c(default_key, rows$prompt_key %||% character()))
  keys[nzchar(keys)]
}

load_article_lab_prompt <- function(con, prompt_key = article_lab_manual_prompt_key, default_prompt = article_lab_default_prompt) {
  key <- article_lab_input_string(prompt_key) %||% article_lab_manual_prompt_key
  fallback <- article_lab_input_multiline(default_prompt) %||% article_lab_default_prompt
  if (!dbExistsTable(con, "article_lab_prompts")) return(fallback)
  rows <- dbGetQuery(con, "
    SELECT prompt_text
    FROM article_lab_prompts
    WHERE prompt_key = ?
    LIMIT 1
  ", params = list(key))
  if (nrow(rows) == 0) return(fallback)
  article_lab_input_multiline(rows$prompt_text[[1]]) %||% fallback
}

save_article_lab_prompt <- function(con, prompt_text, prompt_key = article_lab_manual_prompt_key, default_prompt = article_lab_default_prompt) {
  key <- article_lab_input_string(prompt_key) %||% article_lab_manual_prompt_key
  text <- article_lab_input_multiline(prompt_text) %||% (article_lab_input_multiline(default_prompt) %||% article_lab_default_prompt)
  timestamp <- now_utc()
  rows <- dbGetQuery(con, "SELECT prompt_key FROM article_lab_prompts WHERE prompt_key = ? LIMIT 1", params = list(key))
  if (nrow(rows) > 0) {
    dbExecute(con, "
      UPDATE article_lab_prompts
      SET updated_at = ?, prompt_text = ?
      WHERE prompt_key = ?
    ", params = list(timestamp, text, key))
    return(invisible(key))
  }
  dbExecute(con, "
    INSERT INTO article_lab_prompts (prompt_key, created_at, updated_at, prompt_text)
    VALUES (?, ?, ?, ?)
  ", params = list(key, timestamp, timestamp, text))
  invisible(key)
}

article_lab_normalize_candidate_rows <- function(rows) {
  if (nrow(rows) == 0) return(rows)
  ready_column <- if ("ready_for_human_rating" %in% names(rows)) rows$ready_for_human_rating else rep(0L, nrow(rows))
  promoted_column <- if ("promoted" %in% names(rows)) rows$promoted else rep(0L, nrow(rows))
  archived_column <- if ("archived" %in% names(rows)) rows$archived else rep(0L, nrow(rows))
  rows$normalized_status <- vapply(seq_len(nrow(rows)), function(i) {
    article_lab_normalize_candidate_status(
      status = rows$status[[i]],
      ready_for_human_rating = ready_column[[i]],
      promoted = promoted_column[[i]],
      archived = archived_column[[i]]
    )
  }, character(1))
  rows$status_label <- vapply(rows$normalized_status, article_lab_status_label, character(1))
  rows
}



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

load_research_angles <- function(con, source_id = NULL) {
  if (!dbExistsTable(con, "research_article_angles")) return(data.frame())
  source_id_value <- research_input_integer(source_id)
  angle_query <- paste0("
    SELECT a.*, s.source_title, s.source_url, s.pdf_url, s.main_idea AS source_main_idea, s.abstract AS source_abstract
    FROM research_article_angles a
    LEFT JOIN research_sources s ON s.research_source_id = a.research_source_id
  ")
  if (!is.na(source_id_value)) {
    return(dbGetQuery(con, paste0(angle_query, " WHERE a.research_source_id = ? ORDER BY ", research_angle_sort_sql), params = list(source_id_value)))
  }
  dbGetQuery(con, paste0(angle_query, " ORDER BY ", research_angle_sort_sql))
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
  article_lab_input_multiline(rows$prompt_text[[1]]) %||% article_lab_default_research_summary_prompt
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

research_summary_api_request <- function(source, asset, model = NA_character_, prompt_version = NA_character_, prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "summarize_research_pdf.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/summarize_research_pdf.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(source) == 0) stop("Select a source before generating a summary.", call. = FALSE)
  if (nrow(asset) == 0 || !(asset$status[[1]] %in% c("downloaded", "uploaded"))) stop("Download or upload a PDF before generating an API summary.", call. = FALSE)
  local_pdf_path <- research_resolve_local_pdf_path(asset$local_path[[1]])
  if (is.na(local_pdf_path) || !file.exists(local_pdf_path)) stop("The selected PDF asset does not exist on disk.", call. = FALSE)

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_research_summary_model,
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
    prompt_version = article_lab_input_string(parsed$prompt_version) %||% request_payload$prompt_version,
    raw_json = stdout_text,
    response_id = article_lab_input_string(parsed$response_id)
  )
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

stub_title_candidates <- function(prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_) {
  prompt_value <- clean_text(prompt)
  if (length(prompt_value) == 0 || is.na(prompt_value[[1]])) prompt_value <- article_lab_default_prompt
  topic_value <- clean_text(seed_topic)
  source_value <- clean_text(inspiration_source)
  model_value <- clean_text(model)
  n <- suppressWarnings(as.integer(batch_size))
  if (is.na(n) || n < 1L) n <- 10L
  n <- min(n, 25L)

  topic_phrase <- if (length(topic_value) > 0 && !is.na(topic_value[[1]])) {
    topic_value[[1]]
  } else {
    "building wealth without getting lost in noise"
  }

  if (length(source_value) == 0 || is.na(source_value[[1]])) source_value <- "manual prompt"
  if (length(model_value) == 0 || is.na(model_value[[1]])) model_value <- article_lab_default_model

  seed_key <- paste(prompt_value[[1]], topic_phrase, source_value[[1]], model_value[[1]], sep = "|")
  set.seed(sum(utf8ToInt(seed_key)) %% .Machine$integer.max)

  opening <- c(
    "What Most Beginners Miss About",
    "The Quiet Truth About",
    "Why Smart People Still Struggle With",
    "A Better Way To Think About",
    "The Science-Backed Case For",
    "The Hidden Emotional Cost Of",
    "What Finally Helped Me Understand",
    "The Beginner-Friendly Guide To",
    "Why So Many People Overcomplicate",
    "The Calm, Credible Take On"
  )
  topic_suffix <- c(
    "index fund investing",
    "retirement planning",
    "financial independence",
    "building wealth slowly",
    "market volatility",
    "saving without burnout",
    "long-term investing",
    "money habits that actually stick",
    "avoiding expensive investing mistakes",
    "staying rational when headlines get loud"
  )
  payoff <- c(
    "Before Your Next Money Decision",
    "If You Want Progress Without Hype",
    "When You Want Less Stress And Better Odds",
    "Without Pretending The Future Is Predictable",
    "If You Are Tired Of Generic Advice",
    "For People Who Want A Realistic Plan",
    "Without Turning Finance Into A Full-Time Job",
    "If You Want Confidence, Not False Certainty",
    "For Beginners Who Value Evidence",
    "Without Falling For Clickbait"
  )

  titles <- character()
  attempts <- 0L
  while (length(titles) < n && attempts < n * 12L) {
    attempts <- attempts + 1L
    candidate <- paste(
      sample(opening, 1),
      if (!is.na(topic_phrase) && nzchar(topic_phrase) && runif(1) < 0.65) topic_phrase else sample(topic_suffix, 1)
    )
    if (runif(1) < 0.78) {
      candidate <- paste(candidate, sample(payoff, 1), sep = ": ")
    }
    titles <- unique(c(titles, candidate))
  }

  if (length(titles) < n) {
    filler <- vapply(seq_len(n - length(titles)), function(i) {
      paste("A Smarter Beginner's Way To Approach", topic_phrase, sprintf("(%s)", i))
    }, character(1))
    titles <- c(titles, filler)
  }

  validated <- article_lab_validate_titles(titles[seq_len(n)], max_chars = article_lab_title_max_chars)
  data.frame(
    row_number = seq_along(validated$titles),
    title = validated$titles,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

article_lab_has_api_key <- function() {
  env_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (nzchar(trimws(env_key))) return(TRUE)
  env_path <- file.path(project_root, ".env")
  if (!file.exists(env_path)) return(FALSE)
  lines <- tryCatch(readLines(env_path, warn = FALSE), error = function(e) character())
  any(grepl("^\\s*OPENAI_API_KEY\\s*=\\s*.+", lines))
}

article_lab_python_candidates <- function() {
  env_candidates <- clean_text(c(
    Sys.getenv("ARTICLE_LAB_PYTHON", unset = ""),
    Sys.getenv("WRITING_API_PYTHON", unset = "")
  ))
  project_candidates <- clean_text(c(
    file.path(project_root, ".local_gitignored", "article_lab_venv", "bin", "python"),
    file.path(project_root, ".venv", "bin", "python")
  ))
  project_candidates <- project_candidates[file.exists(project_candidates)]
  path_candidates <- clean_text(c(Sys.which("python3"), Sys.which("python")))
  unique(c(env_candidates[!is.na(env_candidates)], project_candidates[!is.na(project_candidates)], path_candidates[!is.na(path_candidates)]))
}

article_lab_resolve_python <- function() {
  candidates <- article_lab_python_candidates()
  if (length(candidates) == 0) {
    stop(
      "No Python interpreter found for Article Lab API scoring. ",
      "Set ARTICLE_LAB_PYTHON to the Python executable that has the OpenAI package installed.",
      call. = FALSE
    )
  }
  checks <- lapply(candidates, function(candidate) {
    check <- article_lab_python_package_check(candidate)
    check$python_bin <- candidate
    check
  })
  for (check in checks) {
    if (isTRUE(check$ok)) {
      message("Article Lab API scoring using Python: ", check$python_bin)
      return(check$python_bin)
    }
  }

  details <- vapply(checks, function(check) {
    detail <- clean_text(check$stderr) %||% clean_text(check$stdout) %||% "package import check failed"
    paste0(shQuote(check$python_bin), ": ", detail)
  }, character(1))
  stop(
    paste0(
      "No Python interpreter available to Article Lab API scoring can import the required package(s). ",
      article_lab_python_setup_message(candidates[[1]]),
      " Tried: ", paste(details, collapse = " | ")
    ),
    call. = FALSE
  )
}

article_lab_python_package_check <- function(python_bin) {
  stdout_file <- tempfile(pattern = "article_lab_python_check_stdout_", fileext = ".log")
  stderr_file <- tempfile(pattern = "article_lab_python_check_stderr_", fileext = ".log")
  on.exit(unlink(c(stdout_file, stderr_file), force = TRUE), add = TRUE)
  check_code <- paste(
    "import os",
    "import openai",
    "tracing = all((os.environ.get(name) or '').strip() for name in ('LANGFUSE_PUBLIC_KEY', 'LANGFUSE_SECRET_KEY')) and ((os.environ.get('LANGFUSE_BASE_URL') or os.environ.get('LANGFUSE_HOST') or '').strip())",
    "if tracing:",
    "    import langfuse",
    "    import langfuse.openai",
    sep = "\n"
  )

  # system2() does not preserve spaces inside -c code unless the argument is quoted explicitly.
  status <- suppressWarnings(system2(
    python_bin,
    args = c("-c", shQuote(check_code)),
    stdout = stdout_file,
    stderr = stderr_file
  ))
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  list(
    ok = is.numeric(status) && length(status) == 1 && !is.na(status) && status == 0,
    status = status,
    stdout = stdout_text,
    stderr = stderr_text
  )
}

article_lab_python_setup_message <- function(python_bin) {
  python_label <- shQuote(python_bin)
  paste0(
    "Article Lab API scoring is using Python interpreter ", python_label, ". ",
    "Install the required package(s) into that interpreter with: ",
    python_label, " -m pip install openai",
    ". If you want the app to use a different interpreter or virtualenv, set ARTICLE_LAB_PYTHON before starting the Shiny app."
  )
}

article_lab_top_title_examples <- function(con, limit = 8L) {
  objects <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type IN ('table', 'view')")
  if (!("v_medium_title_prediction_dataset_v2" %in% objects$name)) return(character())

  query <- sprintf("
    SELECT title
    FROM v_medium_title_prediction_dataset_v2
    WHERE NULLIF(TRIM(title), '') IS NOT NULL
      AND success_score IS NOT NULL
    ORDER BY COALESCE(CAST(top_20_percent AS INTEGER), 0) DESC, success_score DESC
    LIMIT %s
  ", as.integer(limit))
  rows <- dbGetQuery(con, query)
  titles <- clean_text(rows$title)
  unique(titles[!is.na(titles)])
}

article_lab_api_request <- function(prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_, example_titles = character(), manual_prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_titles.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_titles.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)

  request_payload <- list(
    prompt = article_lab_input_string(prompt) %||% article_lab_default_prompt,
    manual_prompt = article_lab_input_multiline(manual_prompt),
    batch_size = as.integer(batch_size),
    seed_topic = article_lab_input_string(seed_topic),
    inspiration_source = article_lab_input_string(inspiration_source),
    model = article_lab_input_string(model) %||% article_lab_default_model,
    example_titles = unname(example_titles)
  )

  request_file <- tempfile(pattern = "article_lab_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2(
    "node",
    args = c(helper_path, request_file),
    stdout = stdout_file,
    stderr = stderr_file
  )
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Title generation helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Title generation helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  titles <- unlist(parsed$titles %||% list(), use.names = FALSE)
  titles <- clean_text(titles)
  titles <- unique(titles[!is.na(titles)])
  if (length(titles) == 0) stop("API helper returned no usable titles.", call. = FALSE)

  list(
    titles = data.frame(
      row_number = seq_along(titles),
      title = titles,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    mode = article_lab_input_string(parsed$mode) %||% "api",
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    raw_json = stdout_text,
    example_titles_used = as.integer(length(example_titles)),
    response_id = article_lab_input_string(parsed$response_id),
    retry_used = isTRUE(parsed$retry_used),
    dropped_n = as.integer(parsed$dropped_count %||% 0L),
    dropped_titles = unname(unlist(parsed$dropped_titles %||% list(), use.names = FALSE))
  )
}

generate_title_candidates <- function(con, prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_, manual_prompt = NA_character_) {
  inspiration_value <- article_lab_input_string(inspiration_source)
  example_titles <- if (identical(inspiration_value, "top performing titles")) article_lab_top_title_examples(con, limit = 8L) else character()

  tryCatch({
    api_result <- article_lab_api_request(
      prompt = prompt,
      batch_size = batch_size,
      seed_topic = seed_topic,
      inspiration_source = inspiration_source,
      model = model,
      example_titles = example_titles,
      manual_prompt = manual_prompt
    )
    api_result$fallback_reason <- NULL
    api_result$validated <- article_lab_validate_titles(api_result$titles$title, max_chars = article_lab_title_max_chars)
    api_result$titles <- data.frame(
      row_number = seq_along(api_result$validated$titles),
      title = api_result$validated$titles,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (is.null(api_result$dropped_n) || is.na(api_result$dropped_n)) api_result$dropped_n <- api_result$validated$dropped_n
    api_result
  }, error = function(e) {
    stub_rows <- stub_title_candidates(
      prompt = prompt,
      batch_size = batch_size,
      seed_topic = seed_topic,
      inspiration_source = inspiration_source,
      model = model
    )
    list(
      titles = stub_rows,
      mode = "stub",
      model = article_lab_input_string(model) %||% article_lab_default_model,
      raw_json = NULL,
      example_titles_used = as.integer(length(example_titles)),
      response_id = NULL,
      fallback_reason = conditionMessage(e),
      dropped_n = 0L
    )
  })
}

article_lab_score_system_prompt <- paste(
  "You score the reader-facing pre-click appeal of Medium finance titles.",
  "Use only the supplied title. Do not infer or use claps, responses, rank, age, publication performance, or observation history.",
  "Do not estimate click potential. Return calibrated JSON scores from 1 to 5."
)

article_lab_score_user_prompt <- function(prompt_version, scope, title) {
  prompt_version <- article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version
  scope <- article_lab_input_string(scope) %||% article_lab_default_score_scope
  title <- article_lab_input_string(title) %||% ""
  title_json <- toJSON(list(title = title), auto_unbox = TRUE, pretty = TRUE)

  if (identical(prompt_version, "v2_3")) {
    return(paste0(
      "Prompt version: ", prompt_version, "\n\n",
      "Score scope: ", scope, "\n",
      "You are scoring only the title. Do not infer a subtitle. Treat missing context as missing.\n\n",
      "Important measurement note:\n",
      "Do not estimate click potential. For competitor Medium articles, we do not have impressions, views, reads, or click-through data, so click potential is not directly testable in this dataset.\n\n",
      "Focus instead on outcomes that can be compared against observed public metrics:\n",
      "- medium_comment_potential: how likely this article is to receive written responses/comments, especially because it invites disagreement, personal stories, debate, strong opinions, corrections, or follow-up questions.\n",
      "- overall_article_potential: overall expected Medium performance based on the title only, considering likely reader interest, topic strength, emotional pull, trust, and engagement potential.\n\n",
      "Calibrate scores relative to typical Medium personal finance articles, not in isolation.\n\n",
      "Use the full 1-5 scale aggressively:\n",
      "1 = very weak, likely below average\n",
      "2 = below average or generic\n",
      "3 = average / okay for Medium finance\n",
      "4 = clearly above average, likely stronger than most articles\n",
      "5 = exceptional, rare, top-tier potential\n\n",
      "Most normal articles should receive 2 or 3.\n",
      "Do not give 4 unless the title has a clearly strong hook, strong topic demand, meaningful emotional or discussion pull, and a clear reader payoff.\n",
      "Do not give 5 unless the title looks unusually compelling and would plausibly belong among the strongest articles in the dataset.\n",
      "Avoid defaulting to 4 for merely competent, useful, or credible articles.\n\n",
      "Input fields, and no other article data:\n",
      title_json, "\n\n",
      "Rubric:\n",
      "- curiosity: How much the title creates a genuine desire to know more.\n",
      "- emotional_pull: How much the title creates emotional interest, concern, excitement, surprise, or urgency.\n",
      "- medium_comment_potential: Estimate how likely the article is to generate written Medium responses/comments. Higher scores should go to title wording that invites disagreement, debate, personal experiences, corrections, strong opinions, or nuanced discussion. A useful but straightforward article can have high clap potential but low comment potential. Use the full scale.\n",
      "- overall_article_potential: Estimate overall Medium performance potential from the title only. This should be a relative ranking judgment, not a quality compliment. Consider topic demand, emotional stakes, trust, likely engagement, and whether the title feels meaningfully differentiated from generic finance content. Use 5 sparingly for likely top-decile potential.\n",
      "- trust_risk: Risk that the title feels exaggerated, misleading, too clickbaity, or credibility-damaging. Higher means more risk. A title can create curiosity or emotion while still carrying trust risk.\n\n",
      "predicted_success_bucket:\n",
      "- low = likely below median or weak relative to typical Medium finance articles.\n",
      "- medium = around median to moderately above average.\n",
      "- high = likely top 20 percent potential. Use high sparingly. Do not classify most articles as high.\n\n",
      "Return JSON matching the schema exactly. short_reason must be one short sentence."
    ))
  }

  paste0(
    "Prompt version: ", prompt_version, "\n\n",
    "Score scope: ", scope, "\n",
    "You are scoring only the title. Do not infer a subtitle. Treat missing context as missing.\n\n",
    "Input fields, and no other article data:\n",
    title_json, "\n\n",
    "Return JSON matching the schema exactly."
  )
}

article_lab_score_api_request <- function(candidates, model = NA_character_, prompt_version = NA_character_, scope = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "score_article_lab_titles.py")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/score_article_lab_titles.py", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(candidates) == 0) return(list(scores = data.frame(), errors = list()))
  python_bin <- article_lab_resolve_python()

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_score_model,
    prompt_version = article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
    scope = article_lab_input_string(scope) %||% article_lab_default_score_scope,
    candidates = unname(lapply(seq_len(nrow(candidates)), function(i) {
      list(
        candidate_id = candidates$candidate_id[[i]],
        batch_id = candidates$batch_id[[i]],
        title = candidates$title[[i]],
        title_char_count = suppressWarnings(as.integer(candidates$title_char_count[[i]])),
        title_length_flag = candidates$title_length_flag[[i]]
      )
    }))
  )

  request_file <- tempfile(pattern = "article_lab_score_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_score_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_score_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2(
    python_bin,
    args = c(helper_path, request_file),
    stdout = stdout_file,
    stderr = stderr_file
  )
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    failure_text <- clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Article Lab scoring helper failed."
    if (grepl("Missing Python package", failure_text, fixed = TRUE) || grepl("No module named 'openai'", failure_text, fixed = TRUE)) {
      failure_text <- paste(failure_text, article_lab_python_setup_message(python_bin))
    }
    stop(failure_text, call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Article Lab scoring helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  raw_scores <- parsed$scores %||% list()
  raw_errors <- parsed$errors %||% list()
  if (!is.list(raw_scores)) raw_scores <- list()
  if (!is.list(raw_errors)) raw_errors <- list()

  score_rows <- lapply(raw_scores, function(entry) {
    row <- data.frame(
      candidate_id = article_lab_input_string(entry$candidate_id),
      batch_id = article_lab_input_string(entry$batch_id),
      scored_at = article_lab_input_string(entry$scored_at),
      model = article_lab_input_string(entry$model) %||% request_payload$model,
      prompt_version = article_lab_input_string(entry$prompt_version) %||% request_payload$prompt_version,
      scope = article_lab_input_string(entry$scope) %||% request_payload$scope,
      predicted_success_bucket = article_lab_input_string(entry$predicted_success_bucket),
      short_reason = article_lab_input_string(entry$short_reason),
      raw_json = if (is.null(entry$raw_json)) NA_character_ else toJSON(entry$raw_json, auto_unbox = TRUE, null = "null"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    for (field in article_lab_score_fields) {
      field_value <- entry[[field]]
      row[[field]] <- if (is.null(field_value) || length(field_value) == 0) {
        NA_real_
      } else {
        suppressWarnings(as.numeric(field_value[[1]]))
      }
    }
    row
  })
  score_frame <- if (length(score_rows) == 0) data.frame() else do.call(rbind, score_rows)
  if (nrow(score_frame) > 0) {
    for (field in article_lab_score_fields) score_frame[[field]] <- suppressWarnings(as.numeric(score_frame[[field]]))
  }

  list(
    scores = score_frame,
    errors = raw_errors,
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    prompt_version = article_lab_input_string(parsed$prompt_version) %||% request_payload$prompt_version,
    scope = article_lab_input_string(parsed$scope) %||% request_payload$scope,
    raw_json = stdout_text
  )
}

article_lab_manual_subtitle_choice_map <- function(target_rows, pending_rows) {
  target_rows <- if (is.null(target_rows)) data.frame() else target_rows
  pending_rows <- if (is.null(pending_rows)) data.frame() else pending_rows

  target_titles <- if (nrow(target_rows) > 0) {
    target_rows[, intersect(c("candidate_id", "batch_id", "title", "created_at"), names(target_rows)), drop = FALSE]
  } else {
    data.frame()
  }
  pending_titles <- if (nrow(pending_rows) > 0) {
    pending_rows[, intersect(c("candidate_id", "batch_id", "title", "created_at"), names(pending_rows)), drop = FALSE]
  } else {
    data.frame()
  }
  rows <- unique(rbind(target_titles, pending_titles))
  if (nrow(rows) == 0) return(character())

  rows$title <- clean_text(rows$title)
  rows$candidate_id <- clean_text(rows$candidate_id)
  rows$batch_id <- clean_text(rows$batch_id)
  rows$created_at <- clean_text(rows$created_at)
  rows <- rows[!is.na(rows$candidate_id) & nzchar(rows$candidate_id) & !is.na(rows$title) & nzchar(rows$title), , drop = FALSE]
  if (nrow(rows) == 0) return(character())

  duplicate_title <- ave(rows$title, rows$title, FUN = length) > 1L
  labels <- rows$title
  if (any(duplicate_title)) {
    labels[duplicate_title] <- paste0(
      rows$title[duplicate_title],
      " (",
      substr(rows$candidate_id[duplicate_title], 1L, 12L),
      ")"
    )
  }
  choices <- as.list(rows$candidate_id)
  names(choices) <- labels
  choices
}

article_lab_xml_escape <- function(text) {
  value <- enc2utf8(as.character(text %||% ""))
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub("\"", "&quot;", value, fixed = TRUE)
  value <- gsub("'", "&apos;", value, fixed = TRUE)
  value
}

article_lab_thumbnail_text_lines <- function(text, width = 22L, max_lines = 3L) {
  value <- article_lab_input_string(text) %||% ""
  if (!nzchar(value)) return(rep("", max_lines))
  wrapped <- strwrap(value, width = max(10L, suppressWarnings(as.integer(width)) %||% 22L))
  wrapped <- wrapped[seq_len(min(length(wrapped), max_lines))]
  if (length(wrapped) < max_lines) wrapped <- c(wrapped, rep("", max_lines - length(wrapped)))
  wrapped
}

article_lab_thumbnail_data_uri <- function(title, subtitle, label, variant_index = 1L) {
  variant_index <- suppressWarnings(as.integer(variant_index))
  if (is.na(variant_index) || variant_index < 1L) variant_index <- 1L
  palettes <- list(
    list(bg1 = "#f3efe3", bg2 = "#e6dcc0", accent = "#1d5c4d", accent2 = "#183a36", text = "#1c1d21", chip = "#ffffff"),
    list(bg1 = "#eef4f7", bg2 = "#d6e7ee", accent = "#205b7a", accent2 = "#163b50", text = "#17202a", chip = "#ffffff"),
    list(bg1 = "#f6eee8", bg2 = "#eed7ca", accent = "#b24f30", accent2 = "#6f2f1e", text = "#211c19", chip = "#fffaf5"),
    list(bg1 = "#eef6ee", bg2 = "#d7ebd6", accent = "#2d6d47", accent2 = "#18402a", text = "#172117", chip = "#ffffff")
  )
  palette <- palettes[[((variant_index - 1L) %% length(palettes)) + 1L]]
  title_lines <- article_lab_thumbnail_text_lines(title, width = 19L, max_lines = 3L)
  subtitle_lines <- article_lab_thumbnail_text_lines(subtitle, width = 28L, max_lines = 2L)
  kicker <- article_lab_xml_escape(label %||% paste("Concept", variant_index))

  svg <- paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' width='1200' height='720' viewBox='0 0 1200 720'>",
    "<defs><linearGradient id='bg' x1='0%' y1='0%' x2='100%' y2='100%'>",
    "<stop offset='0%' stop-color='", palette$bg1, "'/>",
    "<stop offset='100%' stop-color='", palette$bg2, "'/></linearGradient></defs>",
    "<rect width='1200' height='720' rx='44' fill='url(#bg)'/>",
    "<circle cx='1010' cy='112' r='180' fill='", palette$accent, "' opacity='0.15'/>",
    "<rect x='70' y='78' width='160' height='40' rx='18' fill='", palette$chip, "' opacity='0.92'/>",
    "<text x='95' y='104' font-family='Georgia, serif' font-size='24' font-weight='700' fill='", palette$accent2, "'>Medium-style</text>",
    "<rect x='72' y='156' width='500' height='410' rx='38' fill='#ffffff' opacity='0.95'/>",
    "<rect x='72' y='156' width='500' height='14' fill='", palette$accent, "' opacity='0.92'/>",
    "<text x='112' y='248' font-family='Georgia, serif' font-size='56' font-weight='700' fill='", palette$text, "'>", article_lab_xml_escape(title_lines[[1]]), "</text>",
    "<text x='112' y='318' font-family='Georgia, serif' font-size='56' font-weight='700' fill='", palette$text, "'>", article_lab_xml_escape(title_lines[[2]]), "</text>",
    "<text x='112' y='388' font-family='Georgia, serif' font-size='56' font-weight='700' fill='", palette$text, "'>", article_lab_xml_escape(title_lines[[3]]), "</text>",
    "<text x='112' y='468' font-family='Helvetica, Arial, sans-serif' font-size='26' fill='", palette$accent2, "' opacity='0.88'>", article_lab_xml_escape(subtitle_lines[[1]]), "</text>",
    "<text x='112' y='505' font-family='Helvetica, Arial, sans-serif' font-size='26' fill='", palette$accent2, "' opacity='0.88'>", article_lab_xml_escape(subtitle_lines[[2]]), "</text>",
    "<rect x='640' y='108' width='490' height='504' rx='40' fill='", palette$accent, "'/>",
    "<rect x='684' y='156' width='402' height='122' rx='30' fill='", palette$chip, "' opacity='0.95'/>",
    "<text x='724' y='230' font-family='Helvetica, Arial, sans-serif' font-size='40' font-weight='700' fill='", palette$accent2, "'>", kicker, "</text>",
    "<rect x='700' y='324' width='338' height='30' rx='15' fill='#ffffff' opacity='0.92'/>",
    "<rect x='700' y='374' width='278' height='30' rx='15' fill='#ffffff' opacity='0.72'/>",
    "<rect x='700' y='424' width='360' height='30' rx='15' fill='#ffffff' opacity='0.5'/>",
    "<circle cx='976' cy='544' r='84' fill='", palette$accent2, "' opacity='0.2'/>",
    "<text x='698' y='540' font-family='Helvetica, Arial, sans-serif' font-size='28' fill='#ffffff'>Clear finance thumbnail concept</text>",
    "<text x='698' y='580' font-family='Helvetica, Arial, sans-serif' font-size='28' fill='#ffffff'>designed for title + subtitle pairing</text>",
    "</svg>"
  )

  paste0("data:image/svg+xml;charset=UTF-8,", utils::URLencode(svg, reserved = TRUE))
}

stub_thumbnail_candidates_for_package <- function(title, subtitle, prompt = NA_character_, count = article_lab_default_thumbnail_variants) {
  count <- suppressWarnings(as.integer(count))
  if (is.na(count) || count < 1L) count <- article_lab_default_thumbnail_variants
  count <- min(count, 4L)
  labels <- c(
    "Stat-led hero",
    "Calm editorial graphic",
    "Decision-path visual",
    "Human habit concept"
  )
  data.frame(
    thumbnail_label = labels[seq_len(count)],
    thumbnail_data_uri = vapply(seq_len(count), function(i) {
      article_lab_thumbnail_data_uri(title, subtitle, labels[[i]], variant_index = i)
    }, character(1)),
    created_at = rep(now_utc(), count),
    generation_mode = rep("stub", count),
    raw_json = rep(
      toJSON(
        list(
          prompt = article_lab_input_string(prompt) %||% article_lab_default_thumbnail_prompt,
          mode = "stub",
          title = article_lab_input_string(title),
          subtitle = article_lab_input_string(subtitle)
        ),
        auto_unbox = TRUE,
        null = "null"
      ),
      count
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

article_lab_thumbnail_api_request <- function(packages, variants_per_package = article_lab_default_thumbnail_variants, model = NA_character_, prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_thumbnails.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_thumbnails.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(packages) == 0) return(list(rows = data.frame(), model = article_lab_default_thumbnail_model, mode = "api", raw_json = NULL))

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_thumbnail_model,
    prompt = article_lab_input_string(prompt) %||% article_lab_default_thumbnail_prompt,
    variants_per_package = max(1L, min(4L, suppressWarnings(as.integer(variants_per_package)) %||% article_lab_default_thumbnail_variants)),
    packages = unname(lapply(seq_len(nrow(packages)), function(i) {
      list(
        subtitle_id = packages$subtitle_id[[i]],
        candidate_id = packages$candidate_id[[i]],
        batch_id = packages$batch_id[[i]],
        title = packages$title[[i]],
        subtitle = packages$subtitle[[i]]
      )
    }))
  )

  request_file <- tempfile(pattern = "article_lab_thumbnail_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_thumbnail_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_thumbnail_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2(
    "node",
    args = c(helper_path, request_file),
    stdout = stdout_file,
    stderr = stderr_file
  )
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Thumbnail generation helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Thumbnail generation helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  result_rows <- lapply(parsed$results %||% list(), function(entry) {
    thumbnails <- entry$thumbnails %||% list()
    if (length(thumbnails) == 0) return(NULL)
    rows <- lapply(thumbnails, function(thumbnail) {
      data.frame(
        subtitle_id = article_lab_input_string(entry$subtitle_id),
        candidate_id = article_lab_input_string(entry$candidate_id),
        batch_id = article_lab_input_string(entry$batch_id),
        title = article_lab_input_string(entry$title),
        subtitle = article_lab_input_string(entry$subtitle),
        thumbnail_label = article_lab_input_string(thumbnail$thumbnail_label) %||% "API concept",
        thumbnail_data_uri = article_lab_input_string(thumbnail$thumbnail_data_uri),
        created_at = article_lab_input_string(thumbnail$created_at) %||% now_utc(),
        model = article_lab_input_string(thumbnail$model) %||% article_lab_input_string(parsed$model) %||% request_payload$model,
        generation_mode = article_lab_input_string(thumbnail$generation_mode) %||% "api",
        raw_json = if (is.null(thumbnail$raw_json)) stdout_text else toJSON(thumbnail$raw_json, auto_unbox = TRUE, null = "null"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    do.call(rbind, rows)
  })
  result_rows <- Filter(Negate(is.null), result_rows)

  list(
    rows = if (length(result_rows) == 0) data.frame() else do.call(rbind, result_rows),
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    mode = article_lab_input_string(parsed$mode) %||% "api",
    raw_json = stdout_text
  )
}

generate_thumbnail_candidates <- function(packages, variants_per_package = article_lab_default_thumbnail_variants, model = NA_character_, prompt = NA_character_) {
  tryCatch(
    article_lab_thumbnail_api_request(packages, variants_per_package = variants_per_package, model = model, prompt = prompt),
    error = function(e) {
      rows <- lapply(seq_len(nrow(packages)), function(i) {
        variants <- stub_thumbnail_candidates_for_package(
          title = packages$title[[i]],
          subtitle = packages$subtitle[[i]],
          prompt = prompt,
          count = variants_per_package
        )
        if (nrow(variants) == 0) return(NULL)
        data.frame(
          subtitle_id = rep(packages$subtitle_id[[i]], nrow(variants)),
          candidate_id = rep(packages$candidate_id[[i]], nrow(variants)),
          batch_id = rep(packages$batch_id[[i]], nrow(variants)),
          title = rep(packages$title[[i]], nrow(variants)),
          subtitle = rep(packages$subtitle[[i]], nrow(variants)),
          thumbnail_label = variants$thumbnail_label,
          thumbnail_data_uri = variants$thumbnail_data_uri,
          created_at = variants$created_at,
          model = rep(article_lab_input_string(model) %||% article_lab_default_thumbnail_model, nrow(variants)),
          generation_mode = variants$generation_mode,
          raw_json = variants$raw_json,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      list(
        rows = if (length(rows) == 0) data.frame() else do.call(rbind, rows),
        model = article_lab_input_string(model) %||% article_lab_default_thumbnail_model,
        mode = "stub",
        raw_json = article_lab_input_string(prompt) %||% article_lab_default_thumbnail_prompt,
        fallback_reason = conditionMessage(e)
      )
    }
  )
}

stub_outline_for_package <- function(title, subtitle, thumbnail_label = NA_character_) {
  title <- article_lab_input_string(title) %||% "Working title"
  subtitle <- article_lab_input_string(subtitle) %||% "Working subtitle"
  thumbnail_label <- article_lab_input_string(thumbnail_label) %||% "approved thumbnail"
  paste(
    "# Outline",
    "",
    paste0("## Working title: ", title),
    paste0("Subtitle: ", subtitle),
    paste0("Thumbnail angle: ", thumbnail_label),
    "",
    "## Hook",
    "- Open with the reader problem or tension the title promises to resolve.",
    "- Make the stakes concrete without overstating the evidence.",
    "",
    "## Main sections",
    "1. Frame the core mistake or question.",
    "2. Explain the mechanism in plain language.",
    "3. Show the practical tradeoffs for an everyday investor.",
    "4. Give a simple decision framework or checklist.",
    "5. Address caveats, uncertainty, and cases where the advice may not apply.",
    "",
    "## Close",
    "- End with a measured takeaway and one practical next step.",
    sep = "\n"
  )
}

article_lab_outline_api_request <- function(packages, model = NA_character_, prompt = NA_character_, include_context = TRUE) {
  helper_path <- file.path("scripts", "writing_api", "generate_outlines.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_outlines.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(packages) == 0) return(list(rows = data.frame(), model = article_lab_default_outline_model, mode = "api", raw_json = NULL))

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_outline_model,
    prompt = article_lab_input_multiline(prompt) %||% article_lab_default_outline_prompt,
    packages = unname(lapply(seq_len(nrow(packages)), function(i) {
      pdf_path <- if ("pdf_local_path" %in% names(packages) && isTRUE(include_context)) research_resolve_local_pdf_path(packages$pdf_local_path[[i]]) else NA_character_
      list(
        thumbnail_id = packages$thumbnail_id[[i]],
        subtitle_id = packages$subtitle_id[[i]],
        candidate_id = packages$candidate_id[[i]],
        batch_id = packages$batch_id[[i]],
        title = packages$title[[i]],
        subtitle = packages$subtitle[[i]],
        thumbnail_label = packages$thumbnail_label[[i]],
        article_summary = if ("article_summary" %in% names(packages) && isTRUE(include_context) && is.na(pdf_path)) packages$article_summary[[i]] else NULL,
        pdf_path = pdf_path
      )
    }))
  )

  request_file <- tempfile(pattern = "article_lab_outline_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_outline_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_outline_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2("node", args = c(helper_path, request_file), stdout = stdout_file, stderr = stderr_file, timeout = article_lab_outline_helper_timeout_seconds)
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (identical(status, 124L)) {
    stop(sprintf("Outline generation helper timed out after %s seconds. Check internet connectivity and try again.", article_lab_outline_helper_timeout_seconds), call. = FALSE)
  }
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Outline generation helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Outline generation helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  result_rows <- lapply(parsed$results %||% list(), function(entry) {
    data.frame(
      thumbnail_id = article_lab_input_string(entry$thumbnail_id),
      subtitle_id = article_lab_input_string(entry$subtitle_id),
      candidate_id = article_lab_input_string(entry$candidate_id),
      batch_id = article_lab_input_string(entry$batch_id),
      outline_text = article_lab_input_multiline(entry$outline_text),
      created_at = now_utc(),
      model = article_lab_input_string(parsed$model) %||% request_payload$model,
      generation_mode = "api",
      raw_json = stdout_text,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  result_rows <- Filter(function(row) nrow(row) > 0 && !is.na(row$outline_text[[1]]), result_rows)
  list(
    rows = if (length(result_rows) == 0) data.frame() else do.call(rbind, result_rows),
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    mode = article_lab_input_string(parsed$mode) %||% "api",
    raw_json = stdout_text
  )
}

generate_outline_drafts <- function(packages, model = NA_character_, prompt = NA_character_, include_context = TRUE) {
  tryCatch(
    article_lab_outline_api_request(packages, model = model, prompt = prompt, include_context = include_context),
    error = function(e) {
      list(
        rows = data.frame(),
        model = article_lab_input_string(model) %||% article_lab_default_outline_model,
        mode = "failed",
        fallback_reason = conditionMessage(e)
      )
    }
  )
}

stub_subtitle_candidates_for_title <- function(title, count = 4L) {
  base_title <- article_lab_input_string(title) %||% "this article"
  count <- suppressWarnings(as.integer(count))
  if (is.na(count) || count < 1L) count <- 4L
  count <- min(count, 8L)

  lead_ins <- c(
    "A calmer look at what actually works",
    "A practical breakdown without hype",
    "What the evidence suggests for beginners",
    "A realistic guide for long-term investors",
    "Clear, credible takeaways you can use"
  )
  angles <- c(
    "before your next financial decision",
    "if you want progress without prediction",
    "for steadier investing habits",
    "without turning finance into a full-time job",
    "with fewer mistakes and less noise"
  )

  seed_key <- sum(utf8ToInt(base_title)) %% .Machine$integer.max
  set.seed(seed_key)
  subtitles <- vapply(seq_len(count), function(i) {
    if (i %% 2L == 1L) {
      paste(sample(lead_ins, 1), sample(angles, 1))
    } else {
      paste("For", sub(":.*$", "", base_title), sample(angles, 1))
    }
  }, character(1))
  article_lab_normalize_subtitle(subtitles)
}

article_lab_subtitle_api_request <- function(candidates, variants_per_title = 4L, model = NA_character_, prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_subtitles.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_subtitles.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(candidates) == 0) return(list(rows = data.frame(), model = article_lab_default_subtitle_model, mode = "api", raw_json = NULL))

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_subtitle_model,
    prompt = article_lab_input_string(prompt) %||% article_lab_default_subtitle_prompt,
    variants_per_title = max(1L, min(8L, suppressWarnings(as.integer(variants_per_title)) %||% 4L)),
    candidates = unname(lapply(seq_len(nrow(candidates)), function(i) {
      article_summary <- if ("article_summary" %in% names(candidates)) article_lab_input_multiline(candidates$article_summary[[i]]) else NA_character_
      list(
        candidate_id = candidates$candidate_id[[i]],
        batch_id = candidates$batch_id[[i]],
        title = candidates$title[[i]],
        article_summary = article_summary
      )
    }))
  )

  request_file <- tempfile(pattern = "article_lab_subtitle_request_", fileext = ".json")
  stdout_file <- tempfile(pattern = "article_lab_subtitle_stdout_", fileext = ".json")
  stderr_file <- tempfile(pattern = "article_lab_subtitle_stderr_", fileext = ".log")
  on.exit(unlink(c(request_file, stdout_file, stderr_file), force = TRUE), add = TRUE)

  write_json(request_payload, request_file, auto_unbox = TRUE, pretty = FALSE, null = "null")
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(project_root)
  status <- system2(
    "node",
    args = c(helper_path, request_file),
    stdout = stdout_file,
    stderr = stderr_file
  )
  stdout_text <- if (file.exists(stdout_file)) paste(readLines(stdout_file, warn = FALSE), collapse = "\n") else ""
  stderr_text <- if (file.exists(stderr_file)) paste(readLines(stderr_file, warn = FALSE), collapse = "\n") else ""
  if (!is.numeric(status) || length(status) != 1 || is.na(status) || status != 0) {
    stop(clean_text(stderr_text) %||% clean_text(stdout_text) %||% "Subtitle generation helper failed.", call. = FALSE)
  }
  if (!nzchar(trimws(stdout_text))) stop("Subtitle generation helper returned no output.", call. = FALSE)

  parsed <- fromJSON(stdout_text, simplifyVector = FALSE)
  result_rows <- lapply(parsed$results %||% list(), function(entry) {
    subtitles <- article_lab_normalize_subtitle(unname(unlist(entry$subtitles %||% list(), use.names = FALSE)))
    if (length(subtitles) == 0) return(NULL)
    data.frame(
      candidate_id = rep(article_lab_input_string(entry$candidate_id), length(subtitles)),
      batch_id = rep(article_lab_input_string(entry$batch_id), length(subtitles)),
      subtitle = subtitles,
      created_at = rep(article_lab_input_string(entry$created_at) %||% now_utc(), length(subtitles)),
      model = rep(article_lab_input_string(entry$model) %||% request_payload$model, length(subtitles)),
      generation_mode = rep(article_lab_input_string(entry$generation_mode) %||% "api", length(subtitles)),
      raw_json = rep(if (is.null(entry$raw_json)) stdout_text else toJSON(entry$raw_json, auto_unbox = TRUE, null = "null"), length(subtitles)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  result_rows <- Filter(Negate(is.null), result_rows)

  list(
    rows = if (length(result_rows) == 0) data.frame() else do.call(rbind, result_rows),
    model = article_lab_input_string(parsed$model) %||% request_payload$model,
    mode = article_lab_input_string(parsed$mode) %||% "api",
    raw_json = stdout_text
  )
}

generate_subtitle_candidates <- function(candidates, variants_per_title = 4L, model = NA_character_, prompt = NA_character_) {
  tryCatch(
    article_lab_subtitle_api_request(candidates, variants_per_title = variants_per_title, model = model, prompt = prompt),
    error = function(e) {
      rows <- lapply(seq_len(nrow(candidates)), function(i) {
        subtitles <- stub_subtitle_candidates_for_title(candidates$title[[i]], count = variants_per_title)
        if (length(subtitles) == 0) return(NULL)
        data.frame(
          candidate_id = rep(candidates$candidate_id[[i]], length(subtitles)),
          batch_id = rep(candidates$batch_id[[i]], length(subtitles)),
          subtitle = subtitles,
          created_at = rep(now_utc(), length(subtitles)),
          model = rep(article_lab_input_string(model) %||% article_lab_default_subtitle_model, length(subtitles)),
          generation_mode = rep("stub", length(subtitles)),
          raw_json = rep(
            toJSON(
              list(
                prompt = article_lab_input_string(prompt) %||% article_lab_default_subtitle_prompt,
                mode = "stub"
              ),
              auto_unbox = TRUE,
              null = "null"
            ),
            length(subtitles)
          ),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      list(
        rows = if (length(rows) == 0) data.frame() else do.call(rbind, rows),
        model = article_lab_input_string(model) %||% article_lab_default_subtitle_model,
        mode = "stub",
        raw_json = article_lab_input_string(prompt) %||% article_lab_default_subtitle_prompt,
        fallback_reason = conditionMessage(e)
      )
    }
  )
}

load_article_lab_subtitle_targets <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      COALESCE(s.generated_n, 0) AS generated_subtitle_n,
      COALESCE(s.approved_n, 0) AS approved_subtitle_n,
      COALESCE(s.rejected_n, 0) AS rejected_subtitle_n
    FROM article_lab_title_candidates c
    LEFT JOIN (
      SELECT
        candidate_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_subtitle_candidates
      GROUP BY candidate_id
    ) s
      ON s.candidate_id = c.candidate_id
    WHERE c.archived = 0
    ORDER BY c.created_at DESC, c.candidate_id DESC
    " else "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      COALESCE(s.generated_n, 0) AS generated_subtitle_n,
      COALESCE(s.approved_n, 0) AS approved_subtitle_n,
      COALESCE(s.rejected_n, 0) AS rejected_subtitle_n
    FROM article_lab_title_candidates c
    LEFT JOIN (
      SELECT
        candidate_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_subtitle_candidates
      GROUP BY candidate_id
    ) s
      ON s.candidate_id = c.candidate_id
    WHERE c.batch_id = ?
      AND c.archived = 0
    ORDER BY c.created_at DESC, c.candidate_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

load_article_lab_subtitle_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_subtitle_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at,
      s.subtitle,
      s.status AS subtitle_status,
      s.notes,
      s.model,
      s.generation_mode,
      s.approved_at,
      s.rejected_at,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    " else "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at,
      s.subtitle,
      s.status AS subtitle_status,
      s.notes,
      s.model,
      s.generation_mode,
      s.approved_at,
      s.rejected_at,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    WHERE s.batch_id = ?
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

load_article_lab_ready_for_thumbnail_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_subtitle_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.title,
      c.status,
      c.notes,
      GROUP_CONCAT(s.subtitle, '\n') AS approved_subtitles,
      COUNT(*) AS approved_subtitle_n
    FROM article_lab_title_candidates c
    INNER JOIN article_lab_subtitle_candidates s
      ON s.candidate_id = c.candidate_id
     AND s.status = 'approved'
    WHERE c.archived = 0
      AND c.status = 'ready_for_thumbnail'
    GROUP BY c.candidate_id, c.batch_id, c.title, c.status, c.notes
    ORDER BY c.batch_id DESC, c.candidate_id DESC
    " else "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.title,
      c.status,
      c.notes,
      GROUP_CONCAT(s.subtitle, '\n') AS approved_subtitles,
      COUNT(*) AS approved_subtitle_n
    FROM article_lab_title_candidates c
    INNER JOIN article_lab_subtitle_candidates s
      ON s.candidate_id = c.candidate_id
     AND s.status = 'approved'
    WHERE c.archived = 0
      AND c.status = 'ready_for_thumbnail'
      AND c.batch_id = ?
    GROUP BY c.candidate_id, c.batch_id, c.title, c.status, c.notes
    ORDER BY c.batch_id DESC, c.candidate_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

load_article_lab_thumbnail_packages <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_subtitle_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at AS subtitle_created_at,
      s.subtitle,
      s.notes,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      COALESCE(t.generated_n, 0) AS generated_thumbnail_n,
      COALESCE(t.approved_n, 0) AS approved_thumbnail_n,
      COALESCE(t.rejected_n, 0) AS rejected_thumbnail_n
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    LEFT JOIN (
      SELECT
        subtitle_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_thumbnail_candidates
      GROUP BY subtitle_id
    ) t
      ON t.subtitle_id = s.subtitle_id
    WHERE s.status = 'approved'
      AND c.archived = 0
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    " else "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at AS subtitle_created_at,
      s.subtitle,
      s.notes,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      COALESCE(t.generated_n, 0) AS generated_thumbnail_n,
      COALESCE(t.approved_n, 0) AS approved_thumbnail_n,
      COALESCE(t.rejected_n, 0) AS rejected_thumbnail_n
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    LEFT JOIN (
      SELECT
        subtitle_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_thumbnail_candidates
      GROUP BY subtitle_id
    ) t
      ON t.subtitle_id = s.subtitle_id
    WHERE s.status = 'approved'
      AND c.archived = 0
      AND s.batch_id = ?
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    "
  rows <- if (all_batches) dbGetQuery(con, query) else dbGetQuery(con, query, params = list(batch_id))
  if (nrow(rows) == 0) return(rows)
  rows <- article_lab_normalize_candidate_rows(rows)
  rows[rows$approved_thumbnail_n <= 0 & rows$generated_thumbnail_n <= 0, , drop = FALSE]
}

load_article_lab_thumbnail_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_thumbnail_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.status AS thumbnail_status,
      t.notes,
      t.model,
      t.generation_mode,
      t.approved_at,
      t.rejected_at,
      s.subtitle,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    " else "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.status AS thumbnail_status,
      t.notes,
      t.model,
      t.generation_mode,
      t.approved_at,
      t.rejected_at,
      s.subtitle,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    WHERE t.batch_id = ?
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    "
  rows <- if (all_batches) dbGetQuery(con, query) else dbGetQuery(con, query, params = list(batch_id))
  if (nrow(rows) == 0) return(rows)
  approved_packages <- unique(rows$subtitle_id[rows$thumbnail_status == "approved"])
  rows[rows$thumbnail_status == "generated" & !(rows$subtitle_id %in% approved_packages), , drop = FALSE]
}

load_article_lab_ready_for_outline_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_thumbnail_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.notes,
      s.subtitle,
      c.title,
      c.status,
      o.outline_id,
      o.outline_text,
      o.status AS outline_status,
      o.notes AS outline_notes,
      o.model AS outline_model,
      o.generation_mode AS outline_generation_mode,
      o.updated_at AS outline_updated_at,
      o.approved_at AS outline_approved_at
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    LEFT JOIN article_lab_outlines o
      ON o.thumbnail_id = t.thumbnail_id
     AND o.status IN ('draft', 'approved')
    WHERE t.status = 'approved'
      AND c.archived = 0
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    " else "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.notes,
      s.subtitle,
      c.title,
      c.status,
      o.outline_id,
      o.outline_text,
      o.status AS outline_status,
      o.notes AS outline_notes,
      o.model AS outline_model,
      o.generation_mode AS outline_generation_mode,
      o.updated_at AS outline_updated_at,
      o.approved_at AS outline_approved_at
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    LEFT JOIN article_lab_outlines o
      ON o.thumbnail_id = t.thumbnail_id
     AND o.status IN ('draft', 'approved')
    WHERE t.status = 'approved'
      AND c.archived = 0
      AND t.batch_id = ?
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

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
           SET updated_at = ?, outline_text = ?, status = 'draft', notes = NULL, model = ?, generation_mode = ?, raw_json = ?, approved_at = NULL
           WHERE outline_id = ?",
          params = list(
            timestamp, outline_rows$outline_text[[i]], outline_rows$model[[i]], outline_rows$generation_mode[[i]], outline_rows$raw_json[[i]], existing$outline_id[[1]]
          )
        )
      } else {
        dbExecute(
          con,
          "INSERT INTO article_lab_outlines
           (outline_id, thumbnail_id, subtitle_id, candidate_id, batch_id, created_at, updated_at, outline_text, status, notes, model, generation_mode, raw_json, approved_at)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'draft', NULL, ?, ?, ?, NULL)",
          params = list(
            article_lab_outline_id(outline_rows$thumbnail_id[[i]]),
            outline_rows$thumbnail_id[[i]], outline_rows$subtitle_id[[i]], outline_rows$candidate_id[[i]], outline_rows$batch_id[[i]],
            timestamp, timestamp, outline_rows$outline_text[[i]], outline_rows$model[[i]], outline_rows$generation_mode[[i]], outline_rows$raw_json[[i]]
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
        "UPDATE article_lab_outlines SET outline_text = ?, notes = ?, updated_at = ? WHERE outline_id = ? AND status = 'draft'",
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

load_article_lab_full_text_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_outlines")) return(data.frame())
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      o.outline_id, o.thumbnail_id, o.subtitle_id, o.candidate_id, o.batch_id,
      o.outline_text, o.updated_at AS outline_updated_at,
      t.thumbnail_label, t.thumbnail_data_uri,
      s.subtitle,
      c.title, c.status,
      d.full_text_draft_id, d.original_generated_text, d.current_draft_text,
      d.status AS draft_status, d.is_approved, d.model AS draft_model,
      d.prompt_key, d.prompt_version, d.generation_mode AS draft_generation_mode,
      d.source_context_mode, d.notes AS draft_notes, d.created_at AS draft_created_at,
      d.updated_at AS draft_updated_at, d.approved_at AS draft_approved_at, d.rejected_at AS draft_rejected_at
    FROM article_lab_outlines o
    INNER JOIN article_lab_thumbnail_candidates t ON t.thumbnail_id = o.thumbnail_id
    INNER JOIN article_lab_subtitle_candidates s ON s.subtitle_id = o.subtitle_id
    INNER JOIN article_lab_title_candidates c ON c.candidate_id = o.candidate_id
    LEFT JOIN article_lab_full_text_drafts d ON d.outline_id = o.outline_id AND d.status != 'rejected'
    WHERE o.status = 'approved' AND c.archived = 0
    ORDER BY o.updated_at DESC, d.updated_at DESC, o.outline_id DESC
    " else "
    SELECT
      o.outline_id, o.thumbnail_id, o.subtitle_id, o.candidate_id, o.batch_id,
      o.outline_text, o.updated_at AS outline_updated_at,
      t.thumbnail_label, t.thumbnail_data_uri,
      s.subtitle,
      c.title, c.status,
      d.full_text_draft_id, d.original_generated_text, d.current_draft_text,
      d.status AS draft_status, d.is_approved, d.model AS draft_model,
      d.prompt_key, d.prompt_version, d.generation_mode AS draft_generation_mode,
      d.source_context_mode, d.notes AS draft_notes, d.created_at AS draft_created_at,
      d.updated_at AS draft_updated_at, d.approved_at AS draft_approved_at, d.rejected_at AS draft_rejected_at
    FROM article_lab_outlines o
    INNER JOIN article_lab_thumbnail_candidates t ON t.thumbnail_id = o.thumbnail_id
    INNER JOIN article_lab_subtitle_candidates s ON s.subtitle_id = o.subtitle_id
    INNER JOIN article_lab_title_candidates c ON c.candidate_id = o.candidate_id
    LEFT JOIN article_lab_full_text_drafts d ON d.outline_id = o.outline_id AND d.status != 'rejected'
    WHERE o.status = 'approved' AND c.archived = 0 AND o.batch_id = ?
    ORDER BY o.updated_at DESC, d.updated_at DESC, o.outline_id DESC
    "
  if (all_batches) dbGetQuery(con, query) else dbGetQuery(con, query, params = list(batch_id))
}

article_lab_full_text_package_rows <- function(rows) {
  if (nrow(rows) == 0) return(rows)
  rows[!duplicated(rows$outline_id), c("outline_id", "thumbnail_id", "subtitle_id", "candidate_id", "batch_id", "outline_text", "thumbnail_label", "thumbnail_data_uri", "subtitle", "title", "status"), drop = FALSE]
}

article_lab_full_text_api_request <- function(packages, model = NA_character_, prompt = NA_character_, prompt_key = NA_character_, include_context = TRUE) {
  helper_path <- file.path("scripts", "writing_api", "generate_full_text.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_full_text.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(packages) == 0) return(list(rows = data.frame(), model = article_lab_default_full_text_model, mode = "api", raw_json = NULL))

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_full_text_model,
    prompt = article_lab_input_multiline(prompt) %||% article_lab_default_full_text_prompt,
    prompt_key = article_lab_input_string(prompt_key) %||% article_lab_full_text_prompt_key,
    packages = unname(lapply(seq_len(nrow(packages)), function(i) {
      pdf_path <- if ("pdf_local_path" %in% names(packages) && isTRUE(include_context)) research_resolve_local_pdf_path(packages$pdf_local_path[[i]]) else NA_character_
      source_mode <- if (!isTRUE(include_context)) "none" else if (!is.na(pdf_path) && file.exists(pdf_path)) "pdf_attachment" else if ("article_summary" %in% names(packages) && !is.na(packages$article_summary[[i]]) && nzchar(packages$article_summary[[i]])) "summary_fallback" else "none"
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
        article_summary = if (identical(source_mode, "summary_fallback")) packages$article_summary[[i]] else NULL,
        pdf_path = if (identical(source_mode, "pdf_attachment")) pdf_path else NULL
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
    data.frame(
      outline_id = article_lab_input_string(entry$outline_id),
      thumbnail_id = article_lab_input_string(entry$thumbnail_id),
      subtitle_id = article_lab_input_string(entry$subtitle_id),
      candidate_id = article_lab_input_string(entry$candidate_id),
      batch_id = article_lab_input_string(entry$batch_id),
      full_text = article_lab_input_multiline(entry$full_text),
      source_context_mode = article_lab_input_string(entry$source_context_mode) %||% "none",
      created_at = now_utc(),
      model = article_lab_input_string(parsed$model) %||% request_payload$model,
      generation_mode = "api",
      raw_json = stdout_text,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  result_rows <- Filter(function(row) nrow(row) > 0 && !is.na(row$full_text[[1]]), result_rows)
  list(rows = if (length(result_rows) == 0) data.frame() else do.call(rbind, result_rows), model = article_lab_input_string(parsed$model) %||% request_payload$model, mode = article_lab_input_string(parsed$mode) %||% "api", raw_json = stdout_text)
}

generate_full_text_drafts <- function(packages, model = NA_character_, prompt = NA_character_, prompt_key = NA_character_, include_context = TRUE) {
  tryCatch(
    article_lab_full_text_api_request(packages, model = model, prompt = prompt, prompt_key = prompt_key, include_context = include_context),
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
      dbExecute(
        con,
        "INSERT INTO article_lab_full_text_drafts
         (full_text_draft_id, outline_id, thumbnail_id, subtitle_id, candidate_id, batch_id,
          original_generated_text, current_draft_text, status, is_approved, model, prompt_key, prompt_version,
          generation_mode, source_context_mode, raw_json, notes, created_at, updated_at, approved_at, rejected_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'draft', 0, ?, ?, ?, ?, ?, ?, NULL, ?, ?, NULL, NULL)",
        params = list(
          article_lab_full_text_draft_id(draft_rows$outline_id[[i]]),
          draft_rows$outline_id[[i]], draft_rows$thumbnail_id[[i]], draft_rows$subtitle_id[[i]], draft_rows$candidate_id[[i]], draft_rows$batch_id[[i]],
          draft_rows$full_text[[i]], draft_rows$full_text[[i]], draft_rows$model[[i]] %||% article_lab_default_full_text_model,
          article_lab_input_string(prompt_key) %||% article_lab_full_text_prompt_key,
          article_lab_input_string(prompt_version) %||% article_lab_input_string(prompt_key) %||% article_lab_full_text_prompt_key,
          draft_rows$generation_mode[[i]] %||% "api", draft_rows$source_context_mode[[i]] %||% "none", draft_rows$raw_json[[i]], timestamp, timestamp
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

load_article_lab_publications <- function(con, active_only = TRUE) {
  if (!dbExistsTable(con, "article_lab_publications")) return(data.frame())
  query <- "SELECT publication_id, publication_name, platform, submission_notes, submission_url, is_active, created_at, updated_at FROM article_lab_publications"
  if (isTRUE(active_only)) query <- paste(query, "WHERE is_active = 1")
  query <- paste(query, "ORDER BY publication_name COLLATE NOCASE ASC")
  dbGetQuery(con, query)
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

load_article_lab_review_publish_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_full_text_drafts")) return(data.frame())
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- "
    SELECT
      d.full_text_draft_id, d.outline_id, d.thumbnail_id, d.subtitle_id, d.candidate_id, d.batch_id,
      d.current_draft_text, d.status AS draft_status, d.is_approved, d.approved_at, d.updated_at AS draft_updated_at,
      c.title, c.status AS candidate_status, c.archived,
      s.subtitle,
      t.thumbnail_label, t.thumbnail_data_uri,
      ps.publish_settings_id, ps.medium_tags_json, ps.publishing_target, ps.publication_id,
      ps.publication_name_snapshot, ps.monetization, ps.canonical_url, ps.featured_image_alt_text,
      ps.image_credit_source, ps.published_url, ps.publish_status, ps.notes AS publish_notes,
      ps.submitted_at, ps.published_at, ps.updated_at AS publish_updated_at
    FROM article_lab_full_text_drafts d
    INNER JOIN article_lab_title_candidates c ON c.candidate_id = d.candidate_id
    LEFT JOIN article_lab_subtitle_candidates s ON s.subtitle_id = d.subtitle_id
    LEFT JOIN article_lab_thumbnail_candidates t ON t.thumbnail_id = d.thumbnail_id
    LEFT JOIN article_lab_publish_settings ps ON ps.full_text_draft_id = d.full_text_draft_id
    WHERE (d.status = 'approved' OR d.is_approved = 1)
      AND COALESCE(c.archived, 0) = 0
  "
  params <- list()
  if (!all_batches) {
    query <- paste(query, "AND d.batch_id = ?")
    params <- list(batch_id)
  }
  query <- paste(query, "ORDER BY d.approved_at DESC, d.updated_at DESC, d.full_text_draft_id DESC")
  if (length(params) == 0) dbGetQuery(con, query) else dbGetQuery(con, query, params = params)
}

article_lab_medium_ready_markdown <- function(row, settings = row) {
  if (nrow(row) == 0) return("")
  title <- article_lab_row_value(row, "title", "Untitled")
  subtitle <- article_lab_row_value(row, "subtitle", "")
  body <- article_lab_row_value(row, "current_draft_text", "")
  alt_text <- article_lab_row_value(settings, "featured_image_alt_text", "")
  credit <- article_lab_row_value(settings, "image_credit_source", "")
  parts <- c(paste0("# ", title))
  if (!is.na(subtitle) && nzchar(subtitle)) parts <- c(parts, paste0("_", subtitle, "_"))
  if (!is.na(alt_text) && nzchar(alt_text)) parts <- c(parts, paste0("![", alt_text, "]()"))
  if (!is.na(credit) && nzchar(credit)) parts <- c(parts, paste0("Image credit/source: ", credit))
  parts <- c(parts, body)
  paste(parts, collapse = "\n\n")
}

article_lab_medium_tags_effective_prompt <- function(row, prompt = NA_character_) {
  if (nrow(row) == 0) return("")
  base_prompt <- article_lab_input_multiline(prompt) %||% article_lab_default_medium_tags_prompt
  paste(
    base_prompt,
    "Article package:",
    sprintf("full_text_draft_id=%s | candidate_id=%s | batch_id=%s", article_lab_row_value(row, "full_text_draft_id", ""), article_lab_row_value(row, "candidate_id", ""), article_lab_row_value(row, "batch_id", "")),
    sprintf("Title: %s", article_lab_row_value(row, "title", "")),
    sprintf("Subtitle: %s", article_lab_row_value(row, "subtitle", "")),
    "Article body:",
    article_lab_row_value(row, "current_draft_text", ""),
    sep = "\n\n"
  )
}

article_lab_medium_tags_api_request <- function(row, model = NA_character_, prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_medium_tags.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_medium_tags.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(row) == 0) stop("Select an approved full article draft before generating Medium tags.", call. = FALSE)

  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_medium_tags_model,
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

article_lab_generate_subtitles_for_titles <- function(con, candidate_ids, model = NA_character_, prompt = NA_character_, variants_per_title = 4L) {
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
  generated <- generate_subtitle_candidates(eligible, variants_per_title = variants_per_title, model = model, prompt = prompt)
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
        (subtitle_id, candidate_id, batch_id, created_at, subtitle, status, notes, model, generation_mode, raw_json, approved_at, rejected_at)
        VALUES (?, ?, ?, ?, ?, 'generated', NULL, ?, ?, ?, NULL, NULL)
        ",
        params = list(
          article_lab_subtitle_id(row$candidate_id[[1]], i),
          row$candidate_id[[1]],
          row$batch_id[[1]],
          row$created_at[[1]] %||% now_utc(),
          row$subtitle[[1]],
          row$model[[1]] %||% article_lab_default_subtitle_model,
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

article_lab_generate_thumbnails_for_packages <- function(con, subtitle_ids, model = NA_character_, prompt = NA_character_, variants_per_package = article_lab_default_thumbnail_variants) {
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
      rows$generated_thumbnail_n <= 0 &
      rows$approved_thumbnail_n <= 0,
    c("subtitle_id", "candidate_id", "batch_id", "title", "subtitle"),
    drop = FALSE
  ]
  skipped_n <- length(subtitle_ids) - nrow(eligible)
  if (nrow(eligible) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = skipped_n, batch_ids = unique(rows$batch_id), mode = "none", model = article_lab_default_thumbnail_model))
  }

  existing_rows <- if (dbExistsTable(con, "article_lab_thumbnail_candidates")) {
    dbGetQuery(
      con,
      sprintf("SELECT subtitle_id, thumbnail_label FROM article_lab_thumbnail_candidates WHERE subtitle_id IN (%s)", paste(rep("?", nrow(eligible)), collapse = ", ")),
      params = as.list(eligible$subtitle_id)
    )
  } else {
    data.frame()
  }

  generated <- generate_thumbnail_candidates(eligible, variants_per_package = variants_per_package, model = model, prompt = prompt)
  thumbnail_rows <- generated$rows
  if (nrow(thumbnail_rows) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = skipped_n + nrow(eligible), batch_ids = unique(rows$batch_id), mode = generated$mode %||% "none", model = generated$model %||% article_lab_default_thumbnail_model))
  }

  if (nrow(existing_rows) > 0) {
    existing_keys <- paste(existing_rows$subtitle_id, tolower(existing_rows$thumbnail_label))
    thumbnail_rows <- thumbnail_rows[!(paste(thumbnail_rows$subtitle_id, tolower(thumbnail_rows$thumbnail_label)) %in% existing_keys), , drop = FALSE]
  }
  if (nrow(thumbnail_rows) == 0) {
    return(list(generated_n = 0L, package_n = 0L, skipped_n = skipped_n + nrow(eligible), batch_ids = unique(rows$batch_id), mode = generated$mode %||% "none", model = generated$model %||% article_lab_default_thumbnail_model))
  }

  dbBegin(con)
  tryCatch({
    for (i in seq_len(nrow(thumbnail_rows))) {
      row <- thumbnail_rows[i, , drop = FALSE]
      dbExecute(
        con,
        "
        INSERT INTO article_lab_thumbnail_candidates
        (thumbnail_id, subtitle_id, candidate_id, batch_id, created_at, thumbnail_label, thumbnail_data_uri, status, notes, model, generation_mode, raw_json, approved_at, rejected_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, 'generated', NULL, ?, ?, ?, NULL, NULL)
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
          row$generation_mode[[1]] %||% generated$mode %||% "generated",
          row$raw_json[[1]]
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

article_lab_unscored_candidates <- function(con, batch_id, model, prompt_version, scope) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  dbGetQuery(
    con,
    if (all_batches) "
    SELECT c.candidate_id, c.batch_id, c.title, c.status, c.title_char_count, c.title_length_flag
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    WHERE c.archived = 0
      AND c.promoted = 0
      AND c.status = 'ready_for_api_scoring'
      AND s.candidate_id IS NULL
    ORDER BY c.created_at, c.candidate_id
    " else "
    SELECT c.candidate_id, c.batch_id, c.title, c.status, c.title_char_count, c.title_length_flag
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    WHERE c.batch_id = ?
      AND c.archived = 0
      AND c.promoted = 0
      AND c.status = 'ready_for_api_scoring'
      AND s.candidate_id IS NULL
    ORDER BY c.created_at, c.candidate_id
    ",
    params = if (all_batches) list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope
    ) else list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope,
      batch_id
    )
  )
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
    (score_id, candidate_id, batch_id, scored_at, model, prompt_version, scope,
     clarity, curiosity, specificity, beginner_appeal, credibility, emotional_pull,
     promise_strength, trust_risk, medium_clap_potential, medium_comment_potential,
     overall_article_potential, combined_title_score, predicted_success_bucket,
     short_reason, raw_json, error)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      article_lab_score_id(score_row$candidate_id[[1]]),
      score_row$candidate_id[[1]],
      score_row$batch_id[[1]],
      score_row$scored_at[[1]] %||% now_utc(),
      score_row$model[[1]] %||% article_lab_default_score_model,
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

article_lab_score_batch <- function(con, batch_id, model, prompt_version, scope, candidate_ids = NULL) {
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
      article_lab_score_api_request(api_rows, model = model, prompt_version = prompt_version, scope = scope),
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
  eligible_ids <- rows$candidate_id[rows$normalized_status %in% c("ready_for_api_scoring", "api_scored", "approved_for_subtitle")]
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

save_article_lab_batch <- function(con, prompt, seed_topic, inspiration_source, requested_batch_size, model, titles, raw_json = NA_character_, generation_mode = "generated", enforce_max_chars = TRUE, notes_extra = NULL) {
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

  dbBegin(con)
  tryCatch({
    dbExecute(
      con,
      "INSERT INTO article_lab_title_batches
       (batch_id, created_at, prompt, seed_topic, inspiration_source, requested_batch_size, model, status, notes)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        batch_id,
        created_at,
        prompt_value[[1]],
        if (length(seed_topic_value) == 0) NA_character_ else seed_topic_value[[1]],
        if (length(inspiration_value) == 0) NA_character_ else inspiration_value[[1]],
        requested_size,
        model_value[[1]],
        "generated",
        paste(
          sprintf("Generation mode: %s.", generation_mode),
          "Article Lab candidates stay generated until manual triage moves selected titles into ready_for_api_scoring.",
          notes_extra %||% ""
        )
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

load_latest_article_lab_batch <- function(con) {
  if (!dbExistsTable(con, "article_lab_title_batches")) return(NULL)
  batch <- dbGetQuery(con, "
    SELECT batch_id, created_at, prompt, seed_topic, inspiration_source,
      requested_batch_size, model, status, notes
    FROM article_lab_title_batches
    ORDER BY created_at DESC, batch_id DESC
    LIMIT 1
  ")
  if (nrow(batch) == 0) NULL else batch[1, , drop = FALSE]
}

load_article_lab_candidates_for_batch <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame())
  }
  if (identical(batch_id, article_lab_all_batches_value)) {
    return(dbGetQuery(
      con,
      "SELECT candidate_id, title, title_char_count, title_length_flag, status, created_at, batch_id,
         ready_for_human_rating, archived, promoted, notes
       FROM article_lab_title_candidates
       ORDER BY created_at DESC, candidate_id DESC"
    ))
  }
  dbGetQuery(
    con,
    "SELECT candidate_id, title, title_char_count, title_length_flag, status, created_at, batch_id,
       ready_for_human_rating, archived, promoted, notes
     FROM article_lab_title_candidates
     WHERE batch_id = ?
     ORDER BY candidate_id",
    params = list(batch_id)
  )
}

load_article_lab_batches <- function(con) {
  if (!dbExistsTable(con, "article_lab_title_batches")) return(data.frame())
  dbGetQuery(con, "
    SELECT batch_id, created_at, requested_batch_size, model, status, seed_topic, inspiration_source
    FROM article_lab_title_batches
    ORDER BY created_at DESC, batch_id DESC
  ")
}

load_article_lab_scoring_rows <- function(con, batch_id, model, prompt_version, scope) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  rows <- dbGetQuery(
    con,
    if (all_batches) "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.title_char_count,
      c.title_length_flag,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      s.score_id,
      s.scored_at,
      s.model,
      s.prompt_version,
      s.scope,
      s.clarity,
      s.curiosity,
      s.specificity,
      s.beginner_appeal,
      s.credibility,
      s.emotional_pull,
      s.promise_strength,
      s.trust_risk,
      s.medium_clap_potential,
      s.medium_comment_potential,
      s.overall_article_potential,
      s.combined_title_score,
      s.predicted_success_bucket,
      s.short_reason,
      s.error
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    ORDER BY c.created_at DESC, c.candidate_id DESC
    " else "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.title_char_count,
      c.title_length_flag,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      s.score_id,
      s.scored_at,
      s.model,
      s.prompt_version,
      s.scope,
      s.clarity,
      s.curiosity,
      s.specificity,
      s.beginner_appeal,
      s.credibility,
      s.emotional_pull,
      s.promise_strength,
      s.trust_risk,
      s.medium_clap_potential,
      s.medium_comment_potential,
      s.overall_article_potential,
      s.combined_title_score,
      s.predicted_success_bucket,
      s.short_reason,
      s.error
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    WHERE c.batch_id = ?
    ORDER BY c.created_at DESC, c.candidate_id DESC
    ",
    params = if (all_batches) list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope
    ) else list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope,
      batch_id
    )
  )
  if (nrow(rows) == 0) return(rows)
  rows$title_char_count <- ifelse(
    is.na(rows$title_char_count),
    article_lab_title_length(rows$title),
    suppressWarnings(as.integer(rows$title_char_count))
  )
  rows$title_length_flag <- ifelse(
    is.na(rows$title_length_flag) |
      rows$title_length_flag == "risky" |
      (rows$title_length_flag == "too_long" & rows$title_char_count <= article_lab_title_max_chars),
    article_lab_title_length_flag(rows$title_char_count),
    rows$title_length_flag
  )
  if (!("combined_title_score" %in% names(rows))) rows$combined_title_score <- NA_real_
  missing_combined <- is.na(rows$combined_title_score) & !is.na(rows$curiosity) & !is.na(rows$emotional_pull) &
    !is.na(rows$medium_comment_potential) & !is.na(rows$overall_article_potential)
  rows$combined_title_score[missing_combined] <- article_lab_combined_title_score(
    curiosity = rows$curiosity[missing_combined],
    emotional_pull = rows$emotional_pull[missing_combined],
    medium_comment_potential = rows$medium_comment_potential[missing_combined],
    overall_article_potential = rows$overall_article_potential[missing_combined],
    trust_risk = rows$trust_risk[missing_combined],
    title_char_count = rows$title_char_count[missing_combined]
  )
  rows
}

article_lab_overview <- function(con) {
  if (!dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame(
      saved_batches = 0L,
      saved_candidates = 0L,
      generated = 0L,
      disqualified = 0L,
      ready_for_api_scoring = 0L,
      api_scored = 0L,
      approved_for_subtitle = 0L,
      ready_for_thumbnail = 0L,
      ready_for_outline = 0L,
      ready_for_draft = 0L,
      rejected = 0L,
      archived = 0L
    ))
  }

  dbGetQuery(con, "
    SELECT
      (SELECT COUNT(*) FROM article_lab_title_batches) AS saved_batches,
      COUNT(*) AS saved_candidates,
      COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated,
      COALESCE(SUM(CASE WHEN status = 'disqualified' THEN 1 ELSE 0 END), 0) AS disqualified,
      COALESCE(SUM(CASE WHEN status = 'ready_for_api_scoring' THEN 1 ELSE 0 END), 0) AS ready_for_api_scoring,
      COALESCE(SUM(CASE WHEN status = 'api_scored' THEN 1 ELSE 0 END), 0) AS api_scored,
      COALESCE(SUM(CASE WHEN status = 'approved_for_subtitle' OR promoted = 1 THEN 1 ELSE 0 END), 0) AS approved_for_subtitle,
      COALESCE(SUM(CASE WHEN status = 'ready_for_thumbnail' THEN 1 ELSE 0 END), 0) AS ready_for_thumbnail,
      COALESCE(SUM(CASE WHEN status = 'ready_for_outline' THEN 1 ELSE 0 END), 0) AS ready_for_outline,
      COALESCE(SUM(CASE WHEN status = 'ready_for_draft' THEN 1 ELSE 0 END), 0) AS ready_for_draft,
      COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected,
      COALESCE(SUM(CASE WHEN archived = 1 THEN 1 ELSE 0 END), 0) AS archived
    FROM article_lab_title_candidates
  ")
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

article_lab_generate_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No titles need triage",
      "Generate and save a new batch, or show disqualified titles to review earlier skips.",
      "Next step: create title candidates above, then move selected titles to the API queue."
    ))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  headers <- c("Select", "Title", "Status", "Notes")
  tagList(
    tags$table(
      class = "lab-table",
      tags$thead(tags$tr(lapply(headers, tags$th))),
      tags$tbody(
        lapply(seq_len(nrow(rows)), function(i) {
          row <- rows[i, , drop = FALSE]
          candidate_id <- row$candidate_id[[1]]
          is_draft <- identical(row$normalized_status[[1]], "draft") || identical(row$batch_id[[1]], "(draft)")
          select_id <- article_lab_row_input_id("article_lab_generate_select", candidate_id)
          status_id <- article_lab_row_input_id("article_lab_generate_status", candidate_id)
          notes_id <- article_lab_row_input_id("article_lab_generate_notes", candidate_id)
          tags$tr(
            `data-selection-group` = "article_lab_generate",
            `data-candidate-id` = candidate_id,
            tags$td(
              class = "select-cell",
              checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
            ),
            tags$td(row$title[[1]]),
            tags$td(
              if (is_draft) {
                article_lab_badge("draft")
              } else {
                selectInput(
                  status_id,
                  label = NULL,
                  choices = article_lab_status_choices(c("generated", "disqualified")),
                  selected = if (row$normalized_status[[1]] %in% c("generated", "disqualified")) row$normalized_status[[1]] else "generated",
                  width = "100%"
                )
              }
            ),
            tags$td(
              if (is_draft) {
                span(class = "lab-status-copy", "Save the batch to start triage.")
              } else {
                textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note")
              }
            )
          )
        })
      )
    )
  )
}

article_lab_badge <- function(value) {
  label <- clean_text(value)
  if (length(label) == 0 || is.na(label[[1]])) label <- "n/a"
  status_key <- tolower(label[[1]])
  class_name <- gsub("[^A-Za-z0-9_]+", "_", status_key)
  tags$span(class = paste("lab-badge", class_name), article_lab_status_label(status_key))
}

article_lab_subtitle_badge <- function(value) {
  label <- clean_text(value)
  if (length(label) == 0 || is.na(label[[1]])) label <- "n/a"
  status_key <- tolower(label[[1]])
  class_name <- paste("subtitle", gsub("[^A-Za-z0-9_]+", "_", status_key), sep = "_")
  tags$span(class = paste("lab-badge", class_name), article_lab_subtitle_status_label(status_key))
}

article_lab_thumbnail_badge <- function(value) {
  label <- clean_text(value)
  if (length(label) == 0 || is.na(label[[1]])) label <- "n/a"
  status_key <- tolower(label[[1]])
  class_name <- paste("thumbnail", gsub("[^A-Za-z0-9_]+", "_", status_key), sep = "_")
  tags$span(class = paste("lab-badge", class_name), article_lab_thumbnail_status_label(status_key))
}

article_lab_score_queue_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No queued titles",
      "No titles are currently waiting in the API queue for this selection.",
      "Next step: move generated titles into the API queue from the Generate tab."
    ))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_queue_select", row$candidate_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_queue_notes", row$candidate_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_queue",
              `data-candidate-id` = row$candidate_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows))
  )
}

article_lab_score_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No scored titles to review",
      "No API-scored titles are currently waiting for approval in this selection.",
      "Next step: score queued titles, then approve the strongest titles for subtitle generation."
    ))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  score_value <- function(x) {
    value <- suppressWarnings(as.numeric(x))
    ifelse(is.na(value), "\u2014", format(round(value, 1), nsmall = 1, trim = TRUE))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table scored-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "score-col", "Combined Score"),
          tags$th(class = "signals-col", "Main Signals"),
          tags$th(class = "trust-col", "Trust Risk"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_scored_select", row$candidate_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_scored_notes", row$candidate_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_scored",
              `data-candidate-id` = row$candidate_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "score-cell score-strong", score_value(row$combined_title_score[[1]])),
              tags$td(
                class = "signals-cell",
                div(
                  class = "lab-chip-row",
                  article_lab_signal_chip("Curiosity", row$curiosity[[1]], "blue"),
                  article_lab_signal_chip("Emotional", row$emotional_pull[[1]], "purple"),
                  article_lab_signal_chip("Comment", row$medium_comment_potential[[1]], "orange"),
                  article_lab_signal_chip("Overall", row$overall_article_potential[[1]], "green")
                )
              ),
              tags$td(class = "score-cell trust-cell", score_value(row$trust_risk[[1]])),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows))
  )
}

article_lab_subtitle_target_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No titles need subtitles",
      "No approved titles currently need subtitle candidates in this selection.",
      "Next step: approve scored titles from API Scoring."
    ))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_subtitle_title_select", row$candidate_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_subtitle_title_notes", row$candidate_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_subtitle_titles",
              `data-candidate-id` = row$candidate_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows))
  )
}

article_lab_subtitle_candidate_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No subtitle candidates",
      "No subtitle candidates are currently waiting for approval in this selection.",
      "Next step: generate subtitle candidates or add manual subtitle ideas above."
    ))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "subtitle-col", "Subtitle"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_subtitle_candidate_select", row$subtitle_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_subtitle_candidate_notes", row$subtitle_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_subtitle_candidates",
              `data-candidate-id` = row$subtitle_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "subtitle-cell", row$subtitle[[1]]),
              tags$td(class = "status-cell", article_lab_subtitle_badge(row$subtitle_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows), label = "subtitle candidates")
  )
}

article_lab_thumbnail_package_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No packages need thumbnails",
      "No title/subtitle packages currently need thumbnail candidates in this selection.",
      "Next step: approve subtitle candidates from Subtitle Generation."
    ))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "subtitle-col", "Subtitle"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_thumbnail_package_select", row$subtitle_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_thumbnail_package_notes", row$subtitle_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_thumbnail_packages",
              `data-candidate-id` = row$subtitle_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "subtitle-cell", row$subtitle[[1]]),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows), label = "packages")
  )
}

article_lab_thumbnail_candidate_grid_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No thumbnail preview cards",
      "No thumbnail preview cards are currently waiting for approval in this selection.",
      "Next step: select title/subtitle packages above and generate thumbnail candidates."
    ))
  }

  div(
    class = "thumbnail-preview-grid",
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      select_id <- article_lab_row_input_id("article_lab_thumbnail_candidate_select", row$thumbnail_id[[1]])
      notes_id <- article_lab_row_input_id("article_lab_thumbnail_candidate_notes", row$thumbnail_id[[1]])

      div(
        class = "thumbnail-preview-card",
        `data-selection-group` = "article_lab_thumbnail_candidates",
        `data-candidate-id` = row$thumbnail_id[[1]],
        div(
          class = "thumbnail-preview-topbar",
          checkboxInput(select_id, label = NULL, value = FALSE, width = NULL),
          article_lab_thumbnail_badge(row$thumbnail_status[[1]])
        ),
        div(
          class = "thumbnail-preview-shell",
          div(
            class = "thumbnail-preview-meta medium-preview-card",
            div(class = "preview-kicker", row$thumbnail_label[[1]] %||% "Thumbnail candidate"),
            div(class = "preview-title", row$title[[1]]),
            div(class = "preview-subtitle", row$subtitle[[1]])
          ),
          div(
            class = "thumbnail-preview-image-wrap",
            tags$img(
              class = "thumbnail-preview-image",
              src = row$thumbnail_data_uri[[1]],
              alt = paste("Thumbnail candidate for", row$title[[1]])
            )
          )
        ),
        textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note")
      )
    })
  )
}

article_lab_ready_for_outline_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No packages ready for Outline",
      "No title/subtitle/thumbnail packages are ready for Outline yet in this selection.",
      "Next step: approve one thumbnail candidate per package."
    ))
  }

  div(
    class = "thumbnail-preview-grid",
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      outline_id <- article_lab_row_value(row, "outline_id")
      thumbnail_id <- article_lab_row_value(row, "thumbnail_id")
      has_outline <- !is.na(outline_id) && nzchar(outline_id)
      outline_status <- article_lab_input_string(article_lab_row_value(row, "outline_status")) %||% "none"
      div(
        class = paste("thumbnail-preview-card approved", if (has_outline) paste0("outline-", outline_status) else "outline-missing"),
        `data-selection-group` = if (has_outline && identical(outline_status, "draft")) "article_lab_outline_candidates" else "article_lab_outline_packages",
        `data-candidate-id` = if (has_outline && identical(outline_status, "draft")) outline_id else thumbnail_id,
        div(
          class = "thumbnail-preview-topbar",
          article_lab_thumbnail_badge("approved"),
          checkboxInput(article_lab_row_input_id("article_lab_outline_packages", thumbnail_id), if (has_outline) "Regenerate outline" else "Generate outline", value = FALSE),
          if (has_outline && identical(outline_status, "draft")) checkboxInput(article_lab_row_input_id("article_lab_outline_candidates", outline_id), "Approve outline", value = FALSE)
        ),
        div(
          class = "thumbnail-preview-shell",
          div(
            class = "thumbnail-preview-meta medium-preview-card",
            div(class = "preview-kicker", article_lab_row_value(row, "thumbnail_label", "Approved thumbnail")),
            div(class = "preview-title", article_lab_row_value(row, "title", "Untitled")),
            div(class = "preview-subtitle", article_lab_row_value(row, "subtitle", ""))
          ),
          div(
            class = "thumbnail-preview-image-wrap",
            tags$img(
              class = "thumbnail-preview-image",
              src = article_lab_row_value(row, "thumbnail_data_uri", ""),
              alt = paste("Approved thumbnail for", article_lab_row_value(row, "title", "untitled article"))
            )
          )
        ),
        if (has_outline) {
          div(
            class = "lab-outline-editor",
            div(class = "lab-status-copy", sprintf("Outline status: %s", article_lab_status_label(outline_status))),
            textAreaInput(
              article_lab_row_input_id("article_lab_outline_text", outline_id),
              "Outline draft",
              value = article_lab_row_value(row, "outline_text", ""),
              width = "100%",
              height = "320px"
            ),
            textInput(
              article_lab_row_input_id("article_lab_outline_notes", outline_id),
              "Review notes",
              value = article_lab_row_value(row, "outline_notes", ""),
              width = "100%"
            )
          )
        } else {
          div(class = "lab-status-copy", "No outline draft yet. Select this package and generate an outline.")
        }
      )
    })
  )
}

article_lab_full_text_source_badge <- function(row, summary_contexts, include_context = TRUE) {
  context <- if (nrow(summary_contexts) == 0) data.frame() else summary_contexts[summary_contexts$batch_id == row$batch_id[[1]], , drop = FALSE]
  pdf_path <- if (nrow(context) > 0) research_resolve_local_pdf_path(context$pdf_local_path[[1]]) else NA_character_
  has_pdf <- !is.na(pdf_path) && file.exists(pdf_path)
  has_summary <- nrow(context) > 0 && !is.na(context$article_summary[[1]]) && nzchar(context$article_summary[[1]])
  if (!isTRUE(include_context)) return(tags$span(class = "lab-chip default", "Source off"))
  if (has_pdf) return(tags$span(class = "lab-chip blue", "PDF available"))
  if (has_summary) return(tags$span(class = "lab-chip green", "Summary fallback"))
  tags$span(class = "lab-chip orange", "No source context")
}

article_lab_full_text_table_ui <- function(rows, packages, summary_contexts, include_context = TRUE) {
  if (nrow(packages) == 0) return(article_lab_empty_state(
    "No outlines ready for Full Text",
    "No approved outlines are ready for Full Article yet in this selection.",
    "Next step: approve an outline from the Outline tab."
  ))
  draft_rows <- rows[!is.na(rows$full_text_draft_id) & nzchar(rows$full_text_draft_id), , drop = FALSE]
  tagList(
    div(
      class = "thumbnail-preview-grid",
      lapply(seq_len(nrow(packages)), function(i) {
        row <- packages[i, , drop = FALSE]
        outline_id <- row$outline_id[[1]]
        package_drafts <- draft_rows[draft_rows$outline_id == outline_id, , drop = FALSE]
        div(
          class = "thumbnail-preview-card approved",
          `data-selection-group` = "article_lab_full_text_packages",
          `data-candidate-id` = outline_id,
          div(
            class = "thumbnail-preview-topbar",
            div(class = "lab-chip-row", article_lab_badge("ready_for_draft"), article_lab_full_text_source_badge(row, summary_contexts, include_context)),
            checkboxInput(article_lab_row_input_id("article_lab_full_text_packages", outline_id), "Generate draft for this outline", value = FALSE)
          ),
          div(
            class = "thumbnail-preview-shell",
            div(
              class = "thumbnail-preview-meta medium-preview-card",
              div(class = "preview-kicker", row$thumbnail_label[[1]] %||% "Approved thumbnail"),
              div(class = "preview-title", row$title[[1]] %||% "Untitled"),
              div(class = "preview-subtitle", row$subtitle[[1]] %||% "")
            ),
            div(
              class = "thumbnail-preview-image-wrap",
              tags$img(class = "thumbnail-preview-image", src = row$thumbnail_data_uri[[1]] %||% "", alt = paste("Approved thumbnail for", row$title[[1]] %||% "untitled article"))
            )
          ),
          div(class = "lab-status-copy", sprintf("Outline ID: %s", outline_id)),
          tags$details(
            tags$summary("Show approved outline"),
            tags$pre(class = "lab-status-copy", row$outline_text[[1]] %||% "")
          ),
          if (nrow(package_drafts) == 0) {
            div(class = "lab-status-copy", "No full article draft yet. Select this outline and generate a draft.")
          } else {
            tagList(lapply(seq_len(nrow(package_drafts)), function(j) {
              draft <- package_drafts[j, , drop = FALSE]
              draft_id <- draft$full_text_draft_id[[1]]
              draft_text_id <- article_lab_row_input_id("article_lab_full_text_draft_text", draft_id)
              editor <- div(
                class = "lab-outline-editor",
                `data-selection-group` = "article_lab_full_text_drafts",
                `data-candidate-id` = draft_id,
                div(
                  class = "thumbnail-preview-topbar",
                  div(class = "lab-chip-row", article_lab_badge(draft$draft_status[[1]] %||% "draft"), tags$span(class = "lab-chip default", draft$source_context_mode[[1]] %||% "none"), tags$span(class = "lab-chip default", draft$draft_model[[1]] %||% "model unknown")),
                  checkboxInput(article_lab_row_input_id("article_lab_full_text_drafts", draft_id), "Select draft", value = FALSE)
                ),
                div(
                  class = "lab-actions",
                  tags$button(type = "button", class = "btn lab-secondary", onclick = sprintf("window.articleLabCopyValueFromElement('%s', this, 'Copied draft');", draft_text_id), "Copy full article draft")
                ),
                textAreaInput(draft_text_id, "Full article draft", value = draft$current_draft_text[[1]] %||% "", width = "100%", height = "720px"),
                textInput(article_lab_row_input_id("article_lab_full_text_draft_notes", draft_id), "Draft notes", value = draft$draft_notes[[1]] %||% "", width = "100%")
              )
              if (j == 1L) editor else tags$details(tags$summary(sprintf("Show older draft variant %s", j)), editor)
            }))
          }
        )
      })
    )
  )
}

article_lab_review_publish_selector_ui <- function(rows, selected_id = NULL) {
  if (nrow(rows) == 0) return(article_lab_empty_state(
    "No approved drafts ready",
    "No approved full article drafts are ready for Review & Publish.",
    "Next step: approve one full article draft from the Full Text tab."
  ))
  labels <- vapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    status <- article_lab_publish_status_label(article_lab_row_value(row, "publish_status", "ready_for_review_publish"))
    sprintf("%s [%s]", article_lab_row_value(row, "title", "Untitled"), status)
  }, character(1))
  choices <- setNames(rows$full_text_draft_id, labels)
  if (is.null(selected_id) || is.na(selected_id) || !(selected_id %in% rows$full_text_draft_id)) selected_id <- rows$full_text_draft_id[[1]]
  selectizeInput("article_lab_review_publish_draft_id", "Approved article", choices = choices, selected = selected_id, width = "100%")
}

article_lab_review_publish_workspace_ui <- function(row, publications) {
  if (nrow(row) == 0) return(article_lab_empty_state("Select an approved draft", "Select an approved full article draft to manage publishing metadata."))
  target <- article_lab_row_value(row, "publishing_target", "Do not publish yet")
  if (is.na(target) || !(target %in% article_lab_publish_target_choices)) target <- "Do not publish yet"
  monetization <- article_lab_row_value(row, "monetization", "Undecided")
  if (is.na(monetization) || !(monetization %in% article_lab_monetization_choices)) monetization <- "Undecided"
  publish_status <- article_lab_row_value(row, "publish_status", "ready_for_review_publish")
  if (is.na(publish_status) || !(publish_status %in% article_lab_publish_status_values)) publish_status <- "ready_for_review_publish"
  publication_choices <- c("No saved publication selected" = "")
  if (nrow(publications) > 0) publication_choices <- c(publication_choices, setNames(publications$publication_id, publications$publication_name))
  selected_publication <- article_lab_row_value(row, "publication_id", "")
  if (is.na(selected_publication) || !(selected_publication %in% unname(publication_choices))) selected_publication <- ""
  markdown_id <- "article_lab_medium_ready_markdown_text"

  tagList(
    div(class = "lab-publish-status-row", article_lab_badge("ready_for_review_publish"), tags$span(class = "lab-chip default", article_lab_publish_status_label(publish_status))),
    div(
      class = "lab-publish-workspace",
      div(
        class = "lab-card lab-publish-preview",
        h2("Approved article package"),
        h3(article_lab_row_value(row, "title", "Untitled")),
        div(class = "page-subtitle", article_lab_row_value(row, "subtitle", "")),
        if (!is.na(article_lab_row_value(row, "thumbnail_data_uri", NA_character_)) && nzchar(article_lab_row_value(row, "thumbnail_data_uri", ""))) {
          div(
            class = "thumbnail-preview-image-wrap",
            tags$img(class = "thumbnail-preview-image", src = article_lab_row_value(row, "thumbnail_data_uri", ""), alt = paste("Featured image for", article_lab_row_value(row, "title", "untitled article")))
          )
        },
        tags$details(
          tags$summary("Read-only approved article preview"),
          tags$pre(class = "lab-status-copy lab-readonly-preview", article_lab_row_value(row, "current_draft_text", ""))
        )
      ),
      div(
        class = "lab-card lab-publish-metadata",
        h2("Publishing metadata"),
        div(
          class = "lab-grid",
          div(class = "lab-field", textInput("article_lab_publish_medium_tags", "Medium tags (max 5, comma or line separated)", value = article_lab_tags_display(article_lab_row_value(row, "medium_tags_json", "")), width = "100%")),
          div(class = "lab-field", selectInput("article_lab_medium_tags_model", "Medium tags model", choices = article_lab_medium_tags_model_choices, selected = article_lab_default_medium_tags_model, width = "100%")),
          div(class = "lab-field", selectInput("article_lab_publishing_target", "Publishing target", choices = article_lab_publish_target_choices, selected = target, width = "100%")),
          div(class = "lab-field", selectInput("article_lab_publish_status", "Publish status", choices = setNames(article_lab_publish_status_values, vapply(article_lab_publish_status_values, article_lab_publish_status_label, character(1))), selected = publish_status, width = "100%")),
          div(class = "lab-field", selectInput("article_lab_monetization", "Monetization", choices = article_lab_monetization_choices, selected = monetization, width = "100%"))
        ),
        conditionalPanel(
          condition = "input.article_lab_publishing_target == 'Submit to Medium publication'",
          div(
            class = "lab-grid",
            div(class = "lab-field", selectInput("article_lab_publication_id", "Medium publication", choices = publication_choices, selected = selected_publication, width = "100%")),
            div(class = "lab-field", textInput("article_lab_new_publication_name", "Add publication name", value = "", width = "100%", placeholder = "Use when missing from the saved list"))
          )
        ),
        div(
          class = "lab-grid",
          div(class = "lab-field", textInput("article_lab_canonical_url", "Canonical URL", value = article_lab_row_value(row, "canonical_url", ""), width = "100%")),
          div(class = "lab-field", textInput("article_lab_published_url", "Published URL", value = article_lab_row_value(row, "published_url", ""), width = "100%")),
          div(class = "lab-field", textInput("article_lab_featured_image_alt_text", "Featured image alt text", value = article_lab_row_value(row, "featured_image_alt_text", ""), width = "100%")),
          div(class = "lab-field", textInput("article_lab_image_credit_source", "Image credit/source", value = article_lab_row_value(row, "image_credit_source", ""), width = "100%"))
        ),
        div(class = "lab-field", textAreaInput("article_lab_publish_notes", "Notes", value = article_lab_row_value(row, "publish_notes", ""), width = "100%", height = "90px")),
        tags$details(
          class = "lab-secondary-details",
          tags$summary("Medium tag generation prompt"),
          div(class = "lab-field", textAreaInput("article_lab_medium_tags_prompt", "Medium tags API prompt", value = article_lab_default_medium_tags_prompt, width = "100%", height = "130px")),
          uiOutput("article_lab_medium_tags_effective_prompt")
        ),
        article_lab_action_bar(
          actionButton("article_lab_save_publish_settings", "Save publish settings", class = "lab-primary"),
          actionButton("article_lab_generate_medium_tags", "Generate Medium tags", class = "lab-secondary"),
          tags$button(type = "button", class = "btn lab-secondary", onclick = sprintf("window.articleLabCopyTextFromElement('%s', this, 'Copied article');", markdown_id), "Copy Medium-ready article"),
          downloadButton("article_lab_export_markdown", "Export Markdown", class = "lab-secondary"),
          actionButton("article_lab_refresh_publish", "Refresh", class = "lab-secondary")
        ),
        tags$details(
          class = "lab-secondary-details",
          tags$summary("Medium-ready Markdown preview"),
          tags$pre(id = markdown_id, class = "lab-status-copy lab-readonly-preview", article_lab_medium_ready_markdown(row, row))
        )
      )
    )
  )
}

article_lab_ready_for_thumbnail_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No title packages are ready for Thumbnails yet in this selection."))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th(class = "title-col", "Title"),
          tags$th(class = "subtitle-col", "Approved subtitles"),
          tags$th(class = "status-col", "Status")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            subtitle_lines <- clean_text(strsplit(row$approved_subtitles[[1]] %||% "", "\n", fixed = TRUE)[[1]])
            tags$tr(
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(
                class = "subtitle-cell",
                div(
                  class = "approved-subtitle-list",
                  lapply(subtitle_lines[!is.na(subtitle_lines)], function(entry) div(class = "approved-subtitle-item", entry))
                )
              ),
              tags$td(class = "status-cell", article_lab_badge(row$status[[1]]))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows), label = "title packages")
  )
}

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
  rows <- rows[rows$has_local_thumbnail, , drop = FALSE]
  rows$source_type <- "dataset"
  rows$article_lab_candidate_id <- NA_character_
  rows$candidate_created_at <- NA_character_

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

initialize_app_database <- local({
  initialized <- FALSE

  function() {
    if (isTRUE(initialized)) return(invisible(TRUE))
    con <- connect_db()
    on.exit(dbDisconnect(con), add = TRUE)

    ensure_rating_schema(con)
    ensure_article_lab_schema(con)
    ensure_research_workflow_schema(con)
    article_lab_recover_api_pending_candidates(con)
    if (is_dimension_mode) ensure_dimension_pass_queues(con, target_n = default_target_n)

    initialized <<- TRUE
    invisible(TRUE)
  }
})

initialize_app_database()

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

ui <- fluidPage(
  tags$head(
    tags$title("Medium Preview Rating"),
    tags$style(article_lab_css()),
    tags$script(article_lab_js())
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
      uiOutput("sidebar_nav"),
      uiOutput("sidebar_status_card")
    ),
    tags$main(
      class = "main",
      uiOutput("main_panel")
    ),
    tags$aside(
      class = "guide",
      uiOutput("guide_content")
    )
  )
)

server <- function(input, output, session) {
  con <- connect_db()
  onStop(function() dbDisconnect(con))
  rating_session_id <- if (is_dimension_mode) NULL else resume_or_create_session(con, target_n = default_target_n)
  active_section <- reactiveVal("home")
  active_dimension <- reactiveVal(if (is_dimension_mode) first_incomplete_dimension(con) else NA_character_)
  current <- reactiveVal(NULL)
  shown_started_at <- reactiveVal(Sys.time())
  saved_article_lab_prompt_key <- reactiveVal(article_lab_manual_prompt_key)
  saved_article_lab_prompt <- reactiveVal(load_article_lab_prompt(con, article_lab_manual_prompt_key))
  saved_article_lab_outline_prompt_key <- reactiveVal(article_lab_outline_prompt_key)
  saved_article_lab_outline_prompt <- reactiveVal(load_article_lab_prompt(con, article_lab_outline_prompt_key, article_lab_default_outline_prompt))
  saved_article_lab_full_text_prompt_key <- reactiveVal(article_lab_full_text_prompt_key)
  saved_article_lab_full_text_prompt <- reactiveVal(load_article_lab_prompt(con, article_lab_full_text_prompt_key, article_lab_default_full_text_prompt))
  article_lab_state <- reactiveValues(
    draft = NULL,
    draft_created_at = NULL,
    draft_meta = NULL,
    is_generating = FALSE,
    is_scoring = FALSE,
    is_generating_subtitles = FALSE,
    is_generating_thumbnails = FALSE,
    thumbnail_generation_started_at = NULL,
    thumbnail_generation_estimate = NULL,
    notice = NULL
  )
  article_lab_refresh <- reactiveVal(0L)

  observeEvent(input$research_summary_prompt_version, {
    updateTextAreaInput(
      session,
      "research_summary_api_prompt",
      value = load_research_summary_prompt(con, input$research_summary_prompt_version)
    )
  }, ignoreInit = FALSE)

  output$article_lab_prompt_save_button <- renderUI({
    current_prompt <- article_lab_input_multiline(input$article_lab_prompt) %||% article_lab_default_prompt
    saved_prompt <- article_lab_input_multiline(saved_article_lab_prompt()) %||% article_lab_default_prompt
    current_key <- article_lab_input_string(input$article_lab_prompt_key) %||% saved_article_lab_prompt_key()
    saved_key <- saved_article_lab_prompt_key()
    has_changes <- !identical(current_prompt, saved_prompt)
    has_key_changes <- !identical(current_key, saved_key)
    actionButton(
      "article_lab_save_prompt",
      if (has_changes || has_key_changes) "Save prompt" else "Prompt saved",
      class = if (has_changes || has_key_changes) "lab-primary" else "lab-secondary",
      disabled = if (has_changes || has_key_changes) NULL else "disabled"
    )
  })

  output$article_lab_prompt_selector <- renderUI({
    keys <- list_article_lab_prompt_keys(con)
    selected <- saved_article_lab_prompt_key()
    selectInput("article_lab_prompt_key_select", "Saved prompt", choices = keys, selected = selected, width = "100%")
  })

  observeEvent(input$article_lab_prompt_key_select, {
    key <- article_lab_input_string(input$article_lab_prompt_key_select) %||% article_lab_manual_prompt_key
    prompt_text <- load_article_lab_prompt(con, key)
    saved_article_lab_prompt_key(key)
    saved_article_lab_prompt(prompt_text)
    updateTextInput(session, "article_lab_prompt_key", value = key)
    updateTextAreaInput(session, "article_lab_prompt", value = prompt_text)
  }, ignoreInit = TRUE)

  output$article_lab_outline_prompt_selector <- renderUI({
    keys <- list_article_lab_prompt_keys(con, article_lab_outline_prompt_key)
    selected <- saved_article_lab_outline_prompt_key()
    selectInput("article_lab_outline_prompt_key_select", "Saved prompt", choices = keys, selected = selected, width = "100%")
  })

  output$article_lab_outline_prompt_save_button <- renderUI({
    current_prompt <- article_lab_input_multiline(input$article_lab_outline_prompt) %||% article_lab_default_outline_prompt
    saved_prompt <- article_lab_input_multiline(saved_article_lab_outline_prompt()) %||% article_lab_default_outline_prompt
    current_key <- article_lab_input_string(input$article_lab_outline_prompt_key) %||% saved_article_lab_outline_prompt_key()
    saved_key <- saved_article_lab_outline_prompt_key()
    has_changes <- !identical(current_prompt, saved_prompt) || !identical(current_key, saved_key)
    actionButton(
      "article_lab_save_outline_prompt",
      if (has_changes) "Save prompt" else "Prompt saved",
      class = if (has_changes) "lab-primary" else "lab-secondary",
      disabled = if (has_changes) NULL else "disabled"
    )
  })

  observeEvent(input$article_lab_outline_prompt_key_select, {
    key <- article_lab_input_string(input$article_lab_outline_prompt_key_select) %||% article_lab_outline_prompt_key
    prompt_text <- load_article_lab_prompt(con, key, article_lab_default_outline_prompt)
    saved_article_lab_outline_prompt_key(key)
    saved_article_lab_outline_prompt(prompt_text)
    updateTextInput(session, "article_lab_outline_prompt_key", value = key)
    updateTextAreaInput(session, "article_lab_outline_prompt", value = prompt_text)
  }, ignoreInit = TRUE)

  output$article_lab_full_text_prompt_selector <- renderUI({
    keys <- list_article_lab_prompt_keys(con, article_lab_full_text_prompt_key)
    selected <- saved_article_lab_full_text_prompt_key()
    selectInput("article_lab_full_text_prompt_key_select", "Saved prompt", choices = keys, selected = selected, width = "100%")
  })

  output$article_lab_full_text_prompt_save_button <- renderUI({
    current_prompt <- article_lab_input_multiline(input$article_lab_full_text_prompt) %||% article_lab_default_full_text_prompt
    saved_prompt <- article_lab_input_multiline(saved_article_lab_full_text_prompt()) %||% article_lab_default_full_text_prompt
    current_key <- article_lab_input_string(input$article_lab_full_text_prompt_key) %||% saved_article_lab_full_text_prompt_key()
    saved_key <- saved_article_lab_full_text_prompt_key()
    has_changes <- !identical(current_prompt, saved_prompt) || !identical(current_key, saved_key)
    actionButton("article_lab_save_full_text_prompt", if (has_changes) "Save prompt" else "Prompt saved", class = if (has_changes) "lab-primary" else "lab-secondary", disabled = if (has_changes) NULL else "disabled")
  })

  observeEvent(input$article_lab_full_text_prompt_key_select, {
    key <- article_lab_input_string(input$article_lab_full_text_prompt_key_select) %||% article_lab_full_text_prompt_key
    prompt_text <- load_article_lab_prompt(con, key, article_lab_default_full_text_prompt)
    saved_article_lab_full_text_prompt_key(key)
    saved_article_lab_full_text_prompt(prompt_text)
    updateTextInput(session, "article_lab_full_text_prompt_key", value = key)
    updateTextAreaInput(session, "article_lab_full_text_prompt", value = prompt_text)
  }, ignoreInit = TRUE)

  observeEvent(input$sidebar_nav, {
    valid_sections <- c("home", article_lab_workflow_sections, "settings")
    if (is.character(input$sidebar_nav) && input$sidebar_nav %in% valid_sections) {
      active_section(input$sidebar_nav)
      if (identical(input$sidebar_nav, "home")) refresh_current()
    }
  }, ignoreInit = TRUE)

  observe({
    session$sendCustomMessage("setWorkflowLayout", active_section())
  })

  refresh_current <- function() {
    item <- if (is_dimension_mode) {
      field <- isolate(active_dimension())
      if (is.na(field)) {
        NULL
      } else {
        loaded_item <- load_current_dimension_item(con, field)
        if (is.null(loaded_item)) {
          next_field <- next_incomplete_dimension_after(con, field)
          if (!is.na(next_field)) {
            active_dimension(next_field)
            loaded_item <- load_current_dimension_item(con, next_field)
          }
        }
        loaded_item
      }
    } else {
      prune_article_lab_candidates_from_session(con, rating_session_id)
      append_article_lab_candidates_to_session(con, rating_session_id)
      load_current_item(con, rating_session_id)
    }
    current(item)
    shown_started_at(Sys.time())
    if (is_dimension_mode) {
      updateTextAreaInput(session, "note", value = "")
    } else {
      updateTextInput(session, "note", value = "")
    }
    session$sendCustomMessage("clearRatingFocus", list())
  }

  refresh_current()

  counts <- reactive({
    invalidateLater(1000, session)
    if (is_dimension_mode) {
      field <- active_dimension()
      if (is.na(field)) {
        data.frame(total = 0L, completed = 0L, pending = 0L, skipped = 0L)
      } else {
        dimension_queue_counts(con, field)
      }
    } else {
      queue_counts(con, rating_session_id)
    }
  })

  candidate_stats <- reactive({
    invalidateLater(5000, session)
    if (is_dimension_mode) dimension_candidate_counts(con) else candidate_counts(con)
  })

  output$sidebar_nav <- renderUI({
    current_section <- active_section()
    nav_button <- function(section, icon, label, subtitle, enabled = TRUE) {
      tags$button(
        type = "button",
        class = paste("nav-item", if (identical(current_section, section)) "active" else ""),
        onclick = if (enabled) sprintf("Shiny.setInputValue('sidebar_nav', '%s', {priority: 'event'})", section) else NULL,
        disabled = if (!enabled) "disabled" else NULL,
        span(class = "nav-icon", icon),
        div(
          class = "nav-copy",
          div(class = "nav-title", label),
          div(class = "nav-subtitle", subtitle)
        )
      )
    }

    tagList(
      div(
        class = "sidebar-nav-group",
        nav_button("home", "\u2302", "Home", "Current rating workflow")
      ),
      div(
        class = "sidebar-nav-group",
        div(class = "sidebar-nav-label", "Article Lab"),
        nav_button("research_inbox", "R", "Research Inbox", "Track papers and article angles"),
        nav_button("summary", "S", "Summary", "Check paper summary"),
        nav_button("generate", "\u21bb", "Generate", "Generate & triage titles"),
        nav_button("api_scoring", "\u2699", "API Scoring", "Score with API & approve"),
        nav_button("subtitle_generation", "\u270d", "Subtitle Generation", "Generate subtitles"),
        nav_button("thumbnails", "\u25a7", "Thumbnails", "Generate thumbnails"),
        nav_button("outline", "\u2263", "Outline", "Create article outline"),
        nav_button("full_text", "\u270e", "Full Text", "Write full article"),
        nav_button("review_publish", "\u2611", "Review & Publish", "Review and publish")
      ),
      div(
        class = "sidebar-nav-group",
        nav_button("settings", "\u2699", "Settings", "App settings")
      )
    )
  })

  output$sidebar_status_card <- renderUI({
    if (article_lab_is_workflow_section(active_section()) || identical(active_section(), "settings")) {
      return(div(
        class = "daily-goal static-card",
        div(
          class = "article-lab-helper",
          strong("Article Lab helper"),
          p("Follow each step in order."),
          p(class = "shortcut-copy", "Manually approve at key stages.")
        )
      ))
    }

    div(
      class = "daily-goal",
      strong("Daily goal"),
      htmlOutput("sidebar_progress"),
      uiOutput("progress_bar"),
      uiOutput("sidebar_shortcuts")
    )
  })

  output$main_panel <- renderUI({
    current_section <- active_section()
    if (article_lab_is_workflow_section(current_section) || identical(current_section, "settings")) {
      page_meta <- article_lab_nav_meta(current_section)
      generate_has_rows <- {
        saved_rows <- article_lab_generate_candidates()
        draft_rows <- article_lab_state$draft
        nrow(saved_rows) > 0 || (!is.null(draft_rows) && nrow(draft_rows) > 0)
      }
      generate_panel <- tagList(
        div(
          class = "lab-card",
          h2("Generation prompt"),
          div(
            class = "lab-grid",
            div(class = "lab-field", uiOutput("article_lab_prompt_selector")),
            div(class = "lab-field", textInput("article_lab_prompt_key", "Prompt key", value = article_lab_manual_prompt_key, width = "100%"))
          ),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_prompt",
              label = "Manual/default prompt",
              value = saved_article_lab_prompt(),
              width = "100%",
              height = "230px"
            )
          ),
          div(class = "lab-actions", uiOutput("article_lab_prompt_save_button")),
          div(
            class = "lab-grid",
            div(
              class = "lab-field",
              uiOutput("article_lab_research_summary_selector")
            ),
            div(
              class = "lab-field",
              numericInput("article_lab_batch_size", "Batch size", value = 12L, min = 1L, max = 25L, width = "100%")
            ),
            div(
              class = "lab-field",
              selectInput("article_lab_model", "Model", choices = article_lab_title_generation_model_choices, selected = article_lab_default_model, width = "100%")
            ),
            div(
              class = "lab-field",
              textInput("article_lab_seed_topic", "Optional seed/topic (manual mode)", value = "", width = "100%", placeholder = "Optional article idea or angle")
            ),
            div(
              class = "lab-field",
              selectInput(
                "article_lab_inspiration_source",
                "Optional inspiration source (manual mode)",
                choices = c("", "manual prompt", "top performing titles", "custom"),
                selected = "",
                width = "100%"
              )
            )
          ),
          uiOutput("article_lab_effective_prompt"),
          article_lab_action_bar(
            uiOutput("article_lab_generate_button"),
            actionButton("article_lab_save", "Save batch", class = "lab-secondary"),
            actionButton("article_lab_clear", "Clear draft", class = "lab-secondary")
          ),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_manual_titles",
              "Add title ideas manually",
              value = "",
              width = "100%",
              height = "120px",
              placeholder = "Enter one title idea per line"
            )
          ),
          article_lab_action_bar(
            actionButton("article_lab_add_manual_titles", "Add manual titles", class = "lab-secondary")
          ),
          uiOutput("article_lab_notice")
        ),
        div(
          class = "lab-card",
          h3("Current batch triage"),
          article_lab_action_bar(
            checkboxInput("article_lab_generate_select_all", "Select all", value = FALSE),
            checkboxInput("article_lab_show_disqualified", "Show disqualified titles", value = FALSE),
            article_lab_button("article_lab_save_triage", "Save triage changes", class = "lab-secondary", disabled = !generate_has_rows),
            article_lab_button("article_lab_move_to_api_queue", "Move selected to API queue", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_generate');", disabled = !generate_has_rows)
          ),
          uiOutput("article_lab_latest_titles")
        )
      )

      api_score_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(class = "lab-grid", uiOutput("article_lab_batch_selector"), div(class = "lab-field", selectInput("article_lab_score_model", "Model", choices = article_lab_score_model_choices, selected = article_lab_default_score_model, width = "100%")), div(class = "lab-field", textInput("article_lab_score_prompt_version", "Prompt version", value = article_lab_default_score_prompt_version, width = "100%")), div(class = "lab-field", textInput("article_lab_score_scope", "Scope", value = article_lab_default_score_scope, width = "100%"))),
          article_lab_action_bar(
            uiOutput("article_lab_score_button"),
            actionButton("article_lab_refresh_scores", "Refresh", class = "lab-secondary")
          ),
          uiOutput("article_lab_score_effective_prompt"),
          div(class = "lab-status-copy", "Only titles in the API queue are scored."),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_score_sections")
      )

      subtitle_generation_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_subtitle_prompt",
              "Prompt",
              value = article_lab_default_subtitle_prompt,
              width = "100%",
              height = "190px"
            )
          ),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_subtitle_model", "Model", choices = article_lab_subtitle_model_choices, selected = article_lab_default_subtitle_model, width = "100%")),
            div(class = "lab-field", numericInput("article_lab_subtitle_variants_per_title", "Subtitle candidates per title", value = 4L, min = 1L, max = 8L, width = "100%"))
          ),
          article_lab_action_bar(
            uiOutput("article_lab_subtitle_generate_button"),
            actionButton("article_lab_refresh_subtitles", "Refresh", class = "lab-secondary")
          ),
          uiOutput("article_lab_subtitle_effective_prompt"),
          tags$hr(class = "lab-divider"),
          div(
            class = "lab-grid",
            div(class = "lab-field", selectizeInput("article_lab_manual_subtitle_candidate_id", "Add manual subtitle for title", choices = character(), selected = NULL, width = "100%")),
            div(
              class = "lab-field",
              textAreaInput(
                "article_lab_manual_subtitle_text",
                "Manual subtitle idea(s)",
                value = "",
                width = "100%",
                height = "110px",
                placeholder = "Enter one subtitle idea per line"
              )
            )
          ),
          article_lab_action_bar(
            actionButton("article_lab_add_manual_subtitles", "Add manual subtitle idea(s)", class = "lab-secondary")
          ),
          div(class = "lab-status-copy", "Generate subtitle variants for approved titles, then approve or reject candidates manually."),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_subtitle_sections")
      )

      thumbnail_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_thumbnail_prompt",
              "Prompt",
              value = article_lab_default_thumbnail_prompt,
              width = "100%",
              height = "170px"
            )
          ),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_thumbnail_model", "Responses generation model", choices = article_lab_thumbnail_model_choices, selected = article_lab_default_thumbnail_model, width = "100%")),
            div(class = "lab-field", numericInput("article_lab_thumbnail_variants_per_package", "Thumbnail candidates per package", value = article_lab_default_thumbnail_variants, min = 1L, max = 4L, width = "100%"))
          ),
          article_lab_action_bar(
            uiOutput("article_lab_thumbnail_generate_button"),
            actionButton("article_lab_refresh_thumbnails", "Refresh", class = "lab-secondary")
          ),
          uiOutput("article_lab_thumbnail_effective_prompt"),
          div(id = "article_lab_thumbnail_timer", class = "lab-status-copy"),
          div(class = "lab-status-copy", "Generate thumbnail candidates for approved title/subtitle packages, then approve one preview card per package."),
          uiOutput("article_lab_notice")
        )
        ,
        uiOutput("article_lab_thumbnail_sections")
      )

      outline_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-grid",
            div(class = "lab-field", uiOutput("article_lab_outline_prompt_selector")),
            div(class = "lab-field", textInput("article_lab_outline_prompt_key", "Prompt key", value = article_lab_outline_prompt_key, width = "100%"))
          ),
          div(
            class = "lab-field",
            textAreaInput("article_lab_outline_prompt", "Prompt", value = saved_article_lab_outline_prompt(), width = "100%", height = "150px")
          ),
          div(class = "lab-actions", uiOutput("article_lab_outline_prompt_save_button")),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_outline_model", "Model", choices = article_lab_outline_model_choices, selected = article_lab_default_outline_model, width = "100%")),
            uiOutput("article_lab_outline_context_toggle")
          ),
          article_lab_action_bar(
            actionButton("article_lab_generate_outlines", "Generate selected outline(s)", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_outline_packages');"),
            actionButton("article_lab_save_outlines", "Save outline edits", class = "lab-secondary"),
            actionButton("article_lab_approve_outlines", "Approve selected outline(s)", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_outline_candidates');"),
            actionButton("article_lab_refresh_outlines", "Refresh", class = "lab-secondary")
          ),
          div(class = "lab-status-copy", "Generate an outline from approved packages, edit/review it here, then approve it to move the package to draft-ready."),
          uiOutput("article_lab_outline_effective_prompt"),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_outline_sections")
      )

      full_text_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-grid",
            div(class = "lab-field", uiOutput("article_lab_full_text_prompt_selector")),
            div(class = "lab-field", textInput("article_lab_full_text_prompt_key", "Prompt key", value = article_lab_full_text_prompt_key, width = "100%"))
          ),
          div(
            class = "lab-field",
            textAreaInput("article_lab_full_text_prompt", "Prompt", value = saved_article_lab_full_text_prompt(), width = "100%", height = "170px")
          ),
          div(class = "lab-actions", uiOutput("article_lab_full_text_prompt_save_button")),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_full_text_model", "Model", choices = article_lab_full_text_model_choices, selected = article_lab_default_full_text_model, width = "100%")),
            div(class = "lab-field", checkboxInput("article_lab_full_text_include_context", "Include available source context (PDF preferred, summary fallback)", value = TRUE, width = "100%"))
          ),
          article_lab_action_bar(
            actionButton("article_lab_generate_full_text", "Generate full article draft", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_full_text_packages');"),
            actionButton("article_lab_generate_full_text_variant", "Generate another variant", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_full_text_packages');"),
            actionButton("article_lab_regenerate_full_text_draft", "Regenerate selected draft", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_full_text_drafts');"),
            actionButton("article_lab_save_full_text_drafts", "Save draft edits", class = "lab-secondary"),
            actionButton("article_lab_approve_full_text_draft", "Approve selected draft", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_full_text_drafts');"),
            actionButton("article_lab_reject_full_text_draft", "Reject selected draft", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_full_text_drafts');"),
            actionButton("article_lab_refresh_full_text", "Refresh", class = "lab-secondary")
          ),
          div(class = "lab-status-copy", "Generate drafts from approved outlines, edit the selected draft directly, save revisions, then approve one draft for Review & Publish."),
          uiOutput("article_lab_full_text_effective_prompt"),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_full_text_sections")
      )

      review_publish_panel <- tagList(
        div(
          class = "lab-card",
          h2("Review & Publish"),
          div(class = "lab-status-copy", "Approved full article drafts appear here for local publishing metadata, copy/export, and manual status tracking. The article text is read-only in this tab."),
          uiOutput("article_lab_review_publish_selector"),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_review_publish_workspace")
      )

      placeholder_panel <- function(copy) {
        div(
          class = "lab-card step-placeholder",
          p(copy),
          p(class = "shortcut-copy", "This step is present in the workflow navigation, but its deeper implementation is intentionally left untouched in this pass.")
        )
      }

      research_inbox_panel <- tagList(
        div(
          class = "lab-card",
          h2("Ranked Queue"),
          div(class = "lab-status-copy", "Ranked sources have a manual sort order. Finished and archived sources are hidden unless that status is selected."),
          div(class = "lab-grid", div(class = "lab-field", selectInput("research_source_status_filter", "Filter by status", choices = c("All" = "__all__", "new", "reading", "angle_ready", "used", "archived"), selected = "__all__", width = "100%"))),
          div(class = "lab-actions", actionButton("research_refresh", "Refresh", class = "lab-secondary"), actionButton("research_ranked_move_up", "Move selected up", class = "lab-secondary"), actionButton("research_ranked_move_down", "Move selected down", class = "lab-secondary"), actionButton("research_remove_from_ranked", "Remove selected from ranked queue", class = "lab-secondary")),
          DT::DTOutput("research_ranked_sources_table")
        ),
        div(
          class = "lab-card",
          h2("Selected Source / Angle Workspace"),
          uiOutput("research_selected_source_summary"),
          uiOutput("research_angle_workspace")
        ),
        div(
          class = "lab-card",
          h2("Unranked Sources"),
          div(class = "lab-status-copy", "Unranked sources have no manual sort order."),
          div(class = "lab-actions", actionButton("research_add_to_ranked", "Add selected to ranked queue", class = "lab-primary")),
          DT::DTOutput("research_unranked_sources_table")
        ),
        div(
          class = "lab-card",
          h3("New source"),
          div(class = "lab-grid", div(class = "lab-field", textInput("research_new_source_title", "Source title", width = "100%")), div(class = "lab-field", textInput("research_new_source_url", "Source URL", width = "100%")), div(class = "lab-field", textInput("research_new_pdf_url", "PDF URL", width = "100%")), div(class = "lab-field", numericInput("research_new_source_sort", "Sort order", value = NULL, width = "100%"))),
          div(class = "lab-field", textAreaInput("research_new_source_main_idea", "Main idea", width = "100%", height = "90px")),
          div(class = "lab-field", textAreaInput("research_new_source_abstract", "Abstract", width = "100%", height = "90px")),
          div(class = "lab-grid", div(class = "lab-field", textInput("research_new_source_status", "Status", value = "new", width = "100%")), div(class = "lab-field", textInput("research_new_source_name", "Source name", value = "", width = "100%"))),
          div(class = "lab-field", textAreaInput("research_new_source_notes", "Notes", width = "100%", height = "80px")),
          div(class = "lab-actions", actionButton("research_add_source", "Add source", class = "lab-primary"))
        ),
        uiOutput("article_lab_notice")
      )

      summary_panel <- tagList(
        div(
          class = "lab-card",
          h2("Research Summary"),
          div(class = "lab-field", uiOutput("research_summary_source_selector")),
          uiOutput("research_summary_selected_source"),
          uiOutput("research_summary_pdf_status"),
          div(
            class = "lab-actions",
            actionButton("research_download_pdf", "Download PDF", class = "lab-secondary"),
            actionButton("research_clear_pdf", "Clear/replace PDF", class = "lab-secondary")
          ),
          div(class = "lab-field", fileInput("research_pdf_upload", "Upload PDF manually", accept = c(".pdf", "application/pdf"), width = "100%")),
          uiOutput("research_summary_pdf_gate"),
          div(
            class = "lab-card",
            h3("API summary generation"),
            div(
              class = "lab-grid",
              div(class = "lab-field", selectInput("research_summary_model", "Model", choices = article_lab_research_summary_model_choices, selected = article_lab_default_research_summary_model, width = "100%")),
              div(class = "lab-field", selectInput("research_summary_prompt_version", "Prompt version", choices = article_lab_research_summary_prompt_version_choices, selected = article_lab_default_research_summary_prompt_version, width = "100%"))
            ),
            div(class = "lab-field lab-editor-textarea", textAreaInput("research_summary_api_prompt", "API prompt", value = article_lab_default_research_summary_prompt, width = "100%", height = "260px")),
            uiOutput("research_summary_effective_prompt"),
            div(class = "lab-actions", actionButton("research_generate_summary_draft", "Generate summary draft", class = "lab-primary"))
          ),
          div(
            class = "lab-field lab-editor-textarea",
            textAreaInput("research_summary_text", "Summary text", value = research_summary_template, width = "100%", height = "620px")
          ),
          div(
            class = "lab-actions",
            actionButton("research_save_summary_draft", "Save summary draft", class = "lab-secondary"),
            actionButton("research_confirm_summary", "Mark summary confirmed", class = "lab-primary"),
            actionButton("research_send_summary_to_generate", "Send confirmed summary to Generate", class = "lab-secondary")
          ),
          uiOutput("article_lab_notice")
        )
      )

      page_body <- switch(
        current_section,
        research_inbox = research_inbox_panel,
        summary = summary_panel,
        generate = generate_panel,
        api_scoring = api_score_panel,
        subtitle_generation = subtitle_generation_panel,
        thumbnails = thumbnail_panel,
        outline = outline_panel,
        full_text = full_text_panel,
        review_publish = review_publish_panel,
        settings = placeholder_panel("Settings remain available from the sidebar."),
        generate_panel
      )

      return(tagList(
        h1(page_meta$title %||% page_meta$nav_title),
        div(class = "page-subtitle", page_meta$subtitle %||% page_meta$nav_subtitle),
        page_body
      ))
    }

    tagList(
      h1("Medium Preview Rating"),
      htmlOutput("progress_line"),
      htmlOutput("mode_line"),
      uiOutput("v2_paused_warning"),
      uiOutput("v2_debug_banner"),
      div(class = "tabs", div(class = "tab active", "For you"), div(class = "tab", "Featured")),
      uiOutput("article_area"),
      uiOutput("rating_panel")
    )
  })

  output$progress_line <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    if (is_dimension_mode) {
      total <- ifelse(is.na(c$total[[1]]), 0, c$total[[1]])
      field <- active_dimension()
      if (is.na(field)) {
        HTML(sprintf("All <span class='current'>%s</span> active dimension passes complete", length(active_dimension_fields)))
      } else if (completed >= total && total > 0) {
        HTML(sprintf("Dimension complete: <span class='current'>%s</span> · %s / %s", dimension_labels[[field]], completed, total))
      } else {
        HTML(sprintf("Dimension progress: <span class='current'>%s</span> / %s", completed + 1L, total))
      }
    } else {
      remaining <- stats$remaining_unrated[[1]]
      total <- completed + remaining
      HTML(sprintf("Article <span class='current'>%s</span> / %s", completed + 1L, total))
    }
  })

  output$mode_line <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    pending <- ifelse(is.na(c$pending[[1]]), 0, c$pending[[1]])
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    if (is_dimension_mode) {
      field <- active_dimension()
      active_label <- if (is.na(field)) "all complete" else field
      if (is_dimension_v2_mode) {
        return(HTML(sprintf(
          "<div class='mode-line'><strong>Mode:</strong> %s · active dimension %s · cohort rows %s · active dimensions %s · dimension progress %s done / %s pending · overall manual ratings %s / %s complete</div>",
          rating_mode,
          active_label,
          stats$total_cohort_rows[[1]],
          stats$total_dimensions[[1]],
          completed,
          pending,
          stats$completed_dimensions[[1]],
          stats$total_dimensions[[1]]
        )))
      }
      HTML(sprintf(
        "<div class='mode-line'><strong>Mode:</strong> %s · active dimension %s · cohort rows %s · usable local thumbnails %s · dimension progress %s done / %s pending · overall %s / %s dimensions complete</div>",
        rating_mode,
        active_label,
        stats$total_cohort_rows[[1]],
        stats$usable_local_thumbnails[[1]],
        completed,
        pending,
        stats$completed_dimensions[[1]],
        stats$total_dimensions[[1]]
      ))
    } else {
      HTML(sprintf(
        "<div class='mode-line'><strong>Mode:</strong> unrated thumbnails only · thumbnail candidates %s · already rated %s · remaining unrated %s · session %s done / %s pending</div>",
        stats$total_thumbnail_candidates[[1]],
        stats$already_rated[[1]],
        stats$remaining_unrated[[1]],
        completed,
        pending
      ))
    }
  })

  output$v2_paused_warning <- renderUI({
    if (!is_dimension_v2_mode) return(NULL)
    NULL
  })

  output$v2_debug_banner <- renderUI({
    if (!is_dimension_v2_mode) return(NULL)
    item <- current()
    field <- active_dimension()
    c <- counts()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- ifelse(is.na(c$total[[1]]), 0, c$total[[1]])
    if (is.na(field) || (total > 0 && completed >= total)) return(NULL)
    if (is.null(item)) {
      return(div(
        class = "v2-debug-banner error",
        strong("dimensions_v2 manifest render debug: "),
        "no current manifest item"
      ))
    }
    info <- v2_render_info(item)
    local_basename <- basename(first_value(item, "local_thumbnail_path_abs", first_value(item, "local_thumbnail_path")))
    short_hash <- function(x) {
      value <- clean_text(x)
      if (length(value) == 0 || is.na(value[[1]])) return("NA")
      substr(value[[1]], 1, 12)
    }
    div(
      class = paste("v2-debug-banner", if (isTRUE(info$valid)) "" else "error"),
      strong("dimensions_v2 manifest render debug: "),
      paste0(
        "queue_position=", first_value(item, "queue_position"),
        " | article_id=", first_value(item, "article_id"),
        " | medium_post_id=", first_value(item, "medium_post_id"),
        " | canonical_article_key=", first_value(item, "canonical_article_key"),
        " | image=", local_basename,
        " | thumbnail_status=", first_value(item, "thumbnail_status"),
        " | hash_matches_manifest=", first_value(item, "hash_matches_manifest"),
        " | image_sha256=", short_hash(first_value(item, "image_sha256")),
        " | current_image_sha256=", short_hash(first_value(item, "current_image_sha256")),
        " | render_valid=", isTRUE(info$valid),
        " | render_reason=", info$reason,
        " | active_dimension=", field
      )
    )
  })

  output$sidebar_progress <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- if (is_dimension_mode) {
      ifelse(is.na(c$total[[1]]), 0, c$total[[1]])
    } else {
      completed + stats$remaining_unrated[[1]]
    }
    HTML(sprintf("<span class='num'>%s</span> / %s", completed, total))
  })

  output$progress_bar <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- if (is_dimension_mode) {
      max(1, ifelse(is.na(c$total[[1]]), 0, c$total[[1]]))
    } else {
      max(1, completed + stats$remaining_unrated[[1]])
    }
    div(
      class = "progress-track",
      div(class = "progress-fill", style = sprintf("width: %.1f%%;", 100 * completed / total))
    )
  })

  output$sidebar_shortcuts <- renderUI({
    if (is_dimension_mode) {
      field <- active_dimension()
      text <- if (!is.na(field) && field == "ai_low_effort_flag") {
        "A/S/J flag, Space skip, U undo"
      } else {
        "A/S/D/F/J rate, Space skip, U undo"
      }
      div(class = "shortcut-copy", text)
    } else {
      div(class = "shortcut-copy", "A/S/D/F/J rate, Space skip, U undo")
    }
  })

  article_lab_saved_batch <- reactive({
    article_lab_refresh()
    load_latest_article_lab_batch(con)
  })

  article_lab_batches <- reactive({
    article_lab_refresh()
    load_article_lab_batches(con)
  })

  observe({
    batches <- article_lab_batches()
    batch_choices <- if (nrow(batches) == 0) character() else setNames(
      batches$batch_id,
      sprintf("%s · %s", batches$batch_id, batches$created_at)
    )
    choices <- c("All batches" = article_lab_all_batches_value, batch_choices)
    selected <- isolate(input$article_lab_selected_batch)
    valid_values <- c(article_lab_all_batches_value, batches$batch_id)
    if (is.null(selected) || !nzchar(selected) || !(selected %in% valid_values)) {
      selected <- article_lab_all_batches_value
    }
    updateSelectInput(session, "article_lab_selected_batch", choices = choices, selected = selected)
  })

  article_lab_selected_batch_id <- reactive({
    selected <- clean_text(input$article_lab_selected_batch)
    if (length(selected) > 0 && !is.na(selected[[1]])) return(selected[[1]])
    batch <- article_lab_saved_batch()
    if (is.null(batch)) return(NA_character_)
    batch$batch_id[[1]]
  })

  article_lab_selected_batch_candidates <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_candidates_for_batch(con, batch_id)
    article_lab_normalize_candidate_rows(rows)
  })

  article_lab_generate_candidates <- reactive({
    article_lab_refresh()
    batch <- article_lab_saved_batch()
    if (is.null(batch) || nrow(batch) == 0) return(data.frame())
    rows <- load_article_lab_candidates_for_batch(con, batch$batch_id[[1]])
    rows <- article_lab_normalize_candidate_rows(rows)
    show_disqualified <- isTRUE(input$article_lab_show_disqualified %||% FALSE)
    keep_statuses <- if (show_disqualified) c("generated", "disqualified") else "generated"
    rows <- rows[rows$normalized_status %in% keep_statuses, , drop = FALSE]
    rows[order(rows$created_at, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_overview_stats <- reactive({
    article_lab_refresh()
    article_lab_overview(con)
  })

  article_lab_scoring_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_scoring_rows(
      con,
      batch_id = batch_id,
      model = input$article_lab_score_model %||% article_lab_default_score_model,
      prompt_version = input$article_lab_score_prompt_version %||% article_lab_default_score_prompt_version,
      scope = input$article_lab_score_scope %||% article_lab_default_score_scope
    )
    rows <- article_lab_normalize_candidate_rows(rows)
    rows <- rows[rows$normalized_status %in% c("ready_for_api_scoring", "api_pending", "api_scored"), , drop = FALSE]
    rows[order(rows$created_at, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_queue_rows <- reactive({
    rows <- article_lab_scoring_rows()
    rows[rows$normalized_status %in% c("ready_for_api_scoring", "api_pending"), , drop = FALSE]
  })

  article_lab_scored_rows <- reactive({
    rows <- article_lab_scoring_rows()
    rows <- rows[rows$normalized_status == "api_scored", , drop = FALSE]
    if (nrow(rows) == 0) return(rows)
    combined_scores <- suppressWarnings(as.numeric(rows$combined_title_score))
    combined_scores[is.na(combined_scores)] <- -Inf
    rows[order(combined_scores, rows$created_at, rows$candidate_id, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  })

  article_lab_subtitle_target_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_subtitle_targets(con, batch_id)
    rows <- article_lab_normalize_candidate_rows(rows)
    rows <- rows[
      rows$normalized_status == "approved_for_subtitle" &
        suppressWarnings(as.integer(rows$generated_subtitle_n)) <= 0 &
        suppressWarnings(as.integer(rows$approved_subtitle_n)) <= 0,
      ,
      drop = FALSE
    ]
    rows[order(rows$created_at, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_pending_subtitle_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_subtitle_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows <- article_lab_normalize_candidate_rows(
      within(rows, {
        status <- parent_status
      })
    )
    rows <- rows[
      rows$subtitle_status == "generated" &
        rows$normalized_status %in% c("approved_for_subtitle", "ready_for_thumbnail"),
      ,
      drop = FALSE
    ]
    title_sort <- tolower(ifelse(is.na(rows$title), "", rows$title))
    created_sort <- xtfrm(rows$created_at)
    subtitle_sort <- xtfrm(rows$subtitle_id)
    rows[order(title_sort, -created_sort, -subtitle_sort), , drop = FALSE]
  })

  article_lab_thumbnail_package_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_thumbnail_packages(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    title_sort <- tolower(ifelse(is.na(rows$title), "", rows$title))
    subtitle_sort <- tolower(ifelse(is.na(rows$subtitle), "", rows$subtitle))
    rows[order(title_sort, subtitle_sort, decreasing = FALSE), , drop = FALSE]
  })

  research_refresh <- reactiveVal(0L)
  selected_research_source_id <- reactiveVal(NA_integer_)

  research_ranked_sources <- reactive({
    research_refresh()
    load_research_sources(con, input$research_source_status_filter %||% "__all__", ranked = TRUE)
  })

  research_unranked_sources <- reactive({
    research_refresh()
    load_research_sources(con, input$research_source_status_filter %||% "__all__", ranked = FALSE)
  })

  research_summary_sources <- reactive({
    research_refresh()
    load_research_sources(con, "__all__", ranked = NULL)
  })

  confirmed_research_summaries <- reactive({
    research_refresh()
    load_confirmed_research_summaries(con)
  })

  selected_generate_summary <- reactive({
    selected_summary_id <- research_input_integer(input$article_lab_research_summary_id)
    rows <- confirmed_research_summaries()
    if (is.na(selected_summary_id) || nrow(rows) == 0 || !(selected_summary_id %in% rows$summary_id)) return(data.frame())
    rows[match(selected_summary_id, rows$summary_id), , drop = FALSE]
  })

  article_lab_effective_generation_inputs <- reactive({
    selected_summary <- selected_generate_summary()
    if (nrow(selected_summary) > 0) {
      return(list(
        mode = "research_summary",
        prompt = research_summary_prompt(selected_summary),
        manual_prompt = input$article_lab_prompt %||% article_lab_default_prompt,
        seed_topic = selected_summary$source_title[[1]],
        inspiration_source = paste0("research_summary:", selected_summary$summary_id[[1]]),
        summary_id = selected_summary$summary_id[[1]],
        source_title = selected_summary$source_title[[1]] %||% ""
      ))
    }
    list(
      mode = "manual",
      prompt = input$article_lab_prompt %||% article_lab_default_prompt,
      manual_prompt = "",
      seed_topic = input$article_lab_seed_topic %||% "",
      inspiration_source = input$article_lab_inspiration_source %||% "",
      summary_id = NA_integer_,
      source_title = ""
    )
  })

  selected_research_source <- reactive({
    research_refresh()
    load_research_source_by_id(con, selected_research_source_id())
  })

  selected_research_source_summary <- reactive({
    research_refresh()
    load_research_source_summary(con, selected_research_source_id())
  })

  selected_research_pdf_asset <- reactive({
    research_refresh()
    load_research_pdf_asset(con, selected_research_source_id())
  })

  research_angles <- reactive({
    research_refresh()
    source <- selected_research_source()
    if (nrow(source) == 0) return(data.frame())
    load_research_angles(con, source$research_source_id[[1]])
  })

  selected_research_angle <- reactive({
    rows <- research_angles()
    selected <- input$research_angles_table_rows_selected
    if (nrow(rows) == 0 || length(selected) == 0) return(data.frame())
    rows[selected[[1]], , drop = FALSE]
  })

  article_lab_pending_thumbnail_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_thumbnail_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows <- article_lab_normalize_candidate_rows(
      within(rows, {
        status <- parent_status
      })
    )
    title_sort <- tolower(ifelse(is.na(rows$title), "", rows$title))
    subtitle_sort <- tolower(ifelse(is.na(rows$subtitle), "", rows$subtitle))
    created_sort <- xtfrm(rows$created_at)
    rows[order(title_sort, subtitle_sort, -created_sort), , drop = FALSE]
  })

  article_lab_ready_for_thumbnail_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_ready_for_thumbnail_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$batch_id, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_ready_for_outline_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_ready_for_outline_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$created_at, rows$thumbnail_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_full_text_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_full_text_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$outline_updated_at, rows$draft_updated_at, rows$outline_id, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  })

  article_lab_full_text_package_rows_reactive <- reactive({
    article_lab_full_text_package_rows(article_lab_full_text_rows())
  })

  article_lab_review_publish_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_review_publish_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$approved_at, rows$draft_updated_at, rows$full_text_draft_id, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  })

  article_lab_publication_rows <- reactive({
    article_lab_refresh()
    load_article_lab_publications(con, active_only = TRUE)
  })

  article_lab_selected_review_publish_row <- reactive({
    rows <- article_lab_review_publish_rows()
    if (nrow(rows) == 0) return(data.frame())
    selected_id <- article_lab_input_string(input$article_lab_review_publish_draft_id) %||% rows$full_text_draft_id[[1]]
    if (!(selected_id %in% rows$full_text_draft_id)) selected_id <- rows$full_text_draft_id[[1]]
    rows[match(selected_id, rows$full_text_draft_id), , drop = FALSE]
  })

  collect_generate_triage_updates <- function(rows) {
    if (nrow(rows) == 0) return(list(updates = list(), selected_ids = character()))
    updates <- lapply(seq_len(nrow(rows)), function(i) {
      candidate_id <- rows$candidate_id[[i]]
      list(
        candidate_id = candidate_id,
        status = input[[article_lab_row_input_id("article_lab_generate_status", candidate_id)]] %||% rows$normalized_status[[i]],
        notes = input[[article_lab_row_input_id("article_lab_generate_notes", candidate_id)]] %||% rows$notes[[i]],
        selected = isTRUE(input[[article_lab_row_input_id("article_lab_generate_select", candidate_id)]])
      )
    })
    list(
      updates = updates,
      selected_ids = vapply(updates[vapply(updates, function(x) isTRUE(x$selected), logical(1))], `[[`, character(1), "candidate_id")
    )
  }

  collect_candidate_note_updates <- function(rows, prefix) {
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      candidate_id <- rows$candidate_id[[i]]
      list(
        candidate_id = candidate_id,
        notes = input[[article_lab_row_input_id(prefix, candidate_id)]] %||% rows$notes[[i]]
      )
    })
  }

  collect_subtitle_note_updates <- function(rows, prefix) {
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      subtitle_id <- rows$subtitle_id[[i]]
      list(
        subtitle_id = subtitle_id,
        notes = input[[article_lab_row_input_id(prefix, subtitle_id)]] %||% rows$notes[[i]]
      )
    })
  }

  collect_thumbnail_note_updates <- function(rows, prefix) {
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      thumbnail_id <- rows$thumbnail_id[[i]]
      list(
        thumbnail_id = thumbnail_id,
        notes = input[[article_lab_row_input_id(prefix, thumbnail_id)]] %||% rows$notes[[i]]
      )
    })
  }

  collect_outline_updates <- function(rows) {
    if (nrow(rows) == 0 || !("outline_id" %in% names(rows))) return(list())
    rows <- rows[!is.na(rows$outline_id) & nzchar(rows$outline_id) & rows$outline_status == "draft", , drop = FALSE]
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      outline_id <- rows$outline_id[[i]]
      list(
        outline_id = outline_id,
        outline_text = input[[article_lab_row_input_id("article_lab_outline_text", outline_id)]] %||% rows$outline_text[[i]],
        notes = input[[article_lab_row_input_id("article_lab_outline_notes", outline_id)]] %||% rows$outline_notes[[i]]
      )
    })
  }

  collect_full_text_updates <- function(rows) {
    if (nrow(rows) == 0 || !("full_text_draft_id" %in% names(rows))) return(list())
    rows <- rows[!is.na(rows$full_text_draft_id) & nzchar(rows$full_text_draft_id) & rows$draft_status == "draft", , drop = FALSE]
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      draft_id <- rows$full_text_draft_id[[i]]
      list(
        full_text_draft_id = draft_id,
        current_draft_text = input[[article_lab_row_input_id("article_lab_full_text_draft_text", draft_id)]] %||% rows$current_draft_text[[i]],
        notes = input[[article_lab_row_input_id("article_lab_full_text_draft_notes", draft_id)]] %||% rows$draft_notes[[i]]
      )
    })
  }

  collect_selected_ids <- function(rows, prefix, snapshot_ids = NULL, key_col = "candidate_id") {
    if (nrow(rows) == 0) return(character())
    snapshot_ids <- clean_text(snapshot_ids)
    snapshot_ids <- unique(snapshot_ids[!is.na(snapshot_ids)])
    if (length(snapshot_ids) > 0) {
      return(rows[[key_col]][rows[[key_col]] %in% snapshot_ids])
    }
    selected <- vapply(seq_len(nrow(rows)), function(i) {
      row_id <- rows[[key_col]][[i]]
      isTRUE(input[[article_lab_row_input_id(prefix, row_id)]])
    }, logical(1))
    rows[[key_col]][selected]
  }

  article_lab_apply_select_all <- function(rows, prefix, value, key_col = "candidate_id") {
    for (cid in rows[[key_col]]) {
      updateCheckboxInput(
        session,
        inputId = article_lab_row_input_id(prefix, cid),
        value = value
      )
    }
  }

  observe({
    article_lab_generate_candidates()
    updateCheckboxInput(session, inputId = "article_lab_generate_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_generate_select_all, {
    rows <- article_lab_generate_candidates()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_generate_select", isTRUE(input$article_lab_generate_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_queue_rows()
    updateCheckboxInput(session, inputId = "article_lab_queue_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_queue_select_all, {
    rows <- article_lab_queue_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_queue_select", isTRUE(input$article_lab_queue_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_scored_rows()
    updateCheckboxInput(session, inputId = "article_lab_scored_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_scored_select_all, {
    rows <- article_lab_scored_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_scored_select", isTRUE(input$article_lab_scored_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_subtitle_target_rows()
    updateCheckboxInput(session, inputId = "article_lab_subtitle_title_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_subtitle_title_select_all, {
    rows <- article_lab_subtitle_target_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_subtitle_title_select", isTRUE(input$article_lab_subtitle_title_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_pending_subtitle_rows()
    updateCheckboxInput(session, inputId = "article_lab_subtitle_candidate_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_subtitle_candidate_select_all, {
    rows <- article_lab_pending_subtitle_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_subtitle_candidate_select", isTRUE(input$article_lab_subtitle_candidate_select_all), key_col = "subtitle_id")
  }, ignoreInit = TRUE)

  observeEvent(article_lab_selected_batch_id(), {
    choices <- article_lab_manual_subtitle_choice_map(
      article_lab_subtitle_target_rows(),
      article_lab_pending_subtitle_rows()
    )
    current_value <- article_lab_input_string(input$article_lab_manual_subtitle_candidate_id)
    selected_value <- if (length(choices) > 0L && length(current_value) == 1L && !is.na(current_value) && current_value %in% unname(unlist(choices, use.names = FALSE))) current_value else NULL
    updateSelectizeInput(
      session,
      inputId = "article_lab_manual_subtitle_candidate_id",
      choices = choices,
      selected = selected_value,
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observe({
    article_lab_thumbnail_package_rows()
    updateCheckboxInput(session, inputId = "article_lab_thumbnail_package_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_thumbnail_package_select_all, {
    rows <- article_lab_thumbnail_package_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_thumbnail_package_select", isTRUE(input$article_lab_thumbnail_package_select_all), key_col = "subtitle_id")
  }, ignoreInit = TRUE)

  observe({
    article_lab_pending_thumbnail_rows()
    updateCheckboxInput(session, inputId = "article_lab_thumbnail_candidate_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_thumbnail_candidate_select_all, {
    rows <- article_lab_pending_thumbnail_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_thumbnail_candidate_select", isTRUE(input$article_lab_thumbnail_candidate_select_all), key_col = "thumbnail_id")
  }, ignoreInit = TRUE)

  observeEvent(input$research_refresh, {
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_refresh_selected_source, {
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_ranked_sources_table_rows_selected, {
    rows <- research_ranked_sources()
    selected <- input$research_ranked_sources_table_rows_selected
    if (nrow(rows) == 0 || length(selected) == 0) return()
    selected_research_source_id(rows$research_source_id[[selected[[1]]]])
    DT::selectRows(DT::dataTableProxy("research_unranked_sources_table"), NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$research_unranked_sources_table_rows_selected, {
    rows <- research_unranked_sources()
    selected <- input$research_unranked_sources_table_rows_selected
    if (nrow(rows) == 0 || length(selected) == 0) return()
    selected_research_source_id(rows$research_source_id[[selected[[1]]]])
    DT::selectRows(DT::dataTableProxy("research_ranked_sources_table"), NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$research_summary_source_id, {
    selected_research_source_id(research_input_integer(input$research_summary_source_id))
  }, ignoreInit = TRUE)

  observeEvent(selected_research_source_summary(), {
    summary <- selected_research_source_summary()
    value <- if (nrow(summary) == 0) research_summary_template else summary$summary_text[[1]] %||% research_summary_template
    updateTextAreaInput(session, "research_summary_text", value = value)
  }, ignoreInit = FALSE)

  normalize_research_ranked_queue <- function() {
    ids <- dbGetQuery(con, "SELECT research_source_id FROM research_sources WHERE manual_sort_order IS NOT NULL ORDER BY manual_sort_order ASC, updated_at DESC")
    if (nrow(ids) == 0) return(invisible(NULL))
    timestamp <- now_utc()
    for (i in seq_len(nrow(ids))) {
      dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = ? WHERE research_source_id = ?", params = list(timestamp, i, ids$research_source_id[[i]]))
    }
    invisible(NULL)
  }

  observeEvent(input$research_add_to_ranked, {
    id <- research_input_integer(selected_research_source_id())
    if (is.na(id)) {
      article_lab_state$notice <- "Select an unranked source before adding it to the ranked queue."
      return(invisible(NULL))
    }
    current <- dbGetQuery(con, "SELECT manual_sort_order FROM research_sources WHERE research_source_id = ? LIMIT 1", params = list(id))
    if (nrow(current) == 0 || !is.na(current$manual_sort_order[[1]])) {
      article_lab_state$notice <- "Select an unranked source before adding it to the ranked queue."
      return(invisible(NULL))
    }
    max_sort <- dbGetQuery(con, "SELECT COALESCE(MAX(manual_sort_order), 0) AS max_sort FROM research_sources WHERE manual_sort_order IS NOT NULL")
    dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = ? WHERE research_source_id = ?", params = list(now_utc(), as.integer(max_sort$max_sort[[1]]) + 1L, id))
    normalize_research_ranked_queue()
    article_lab_state$notice <- "Source added to ranked queue."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_remove_from_ranked, {
    id <- research_input_integer(selected_research_source_id())
    if (is.na(id)) {
      article_lab_state$notice <- "Select a ranked source before removing it from the ranked queue."
      return(invisible(NULL))
    }
    current <- dbGetQuery(con, "SELECT manual_sort_order FROM research_sources WHERE research_source_id = ? LIMIT 1", params = list(id))
    if (nrow(current) == 0 || is.na(current$manual_sort_order[[1]])) {
      article_lab_state$notice <- "Select a ranked source before removing it from the ranked queue."
      return(invisible(NULL))
    }
    dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = NULL WHERE research_source_id = ?", params = list(now_utc(), id))
    normalize_research_ranked_queue()
    article_lab_state$notice <- "Source removed from ranked queue."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_mark_finished, {
    rows <- selected_research_source()
    if (nrow(rows) == 0) {
      article_lab_state$notice <- "Select a source before marking it finished."
      return(invisible(NULL))
    }
    used_articles <- research_multiline_value(input$research_used_articles)
    if (is.na(used_articles)) {
      article_lab_state$notice <- "Add the article title or URL you wrote from this source before marking it finished."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    dbExecute(con, "
      UPDATE research_sources
      SET updated_at = ?, status = 'used', manual_sort_order = NULL, used_articles = ?, finished_at = ?
      WHERE research_source_id = ?
    ", params = list(timestamp, used_articles, timestamp, rows$research_source_id[[1]]))
    normalize_research_ranked_queue()
    article_lab_state$notice <- "Source marked finished and archived as used."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  move_ranked_source <- function(direction) {
    id <- research_input_integer(selected_research_source_id())
    rows <- dbGetQuery(con, "SELECT research_source_id FROM research_sources WHERE manual_sort_order IS NOT NULL ORDER BY manual_sort_order ASC, updated_at DESC")
    if (is.na(id) || nrow(rows) < 2 || !(id %in% rows$research_source_id)) return(FALSE)
    index <- match(id, rows$research_source_id)
    swap_index <- index + direction
    if (is.na(swap_index) || swap_index < 1L || swap_index > nrow(rows)) return(FALSE)
    ids <- rows$research_source_id
    ids[c(index, swap_index)] <- ids[c(swap_index, index)]
    timestamp <- now_utc()
    for (i in seq_along(ids)) {
      dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = ? WHERE research_source_id = ?", params = list(timestamp, i, ids[[i]]))
    }
    TRUE
  }

  observeEvent(input$research_ranked_move_up, {
    if (move_ranked_source(-1L)) article_lab_state$notice <- "Ranked source moved up." else article_lab_state$notice <- "Select a ranked source that can move up."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_ranked_move_down, {
    if (move_ranked_source(1L)) article_lab_state$notice <- "Ranked source moved down." else article_lab_state$notice <- "Select a ranked source that can move down."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_add_source, {
    title <- research_input_value(input$research_new_source_title)
    if (is.na(title)) {
      article_lab_state$notice <- "Enter a source title before adding a research source."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    dbExecute(con, "
      INSERT INTO research_sources
        (created_at, updated_at, source_title, source_url, pdf_url, main_idea, abstract, source_type, source_name, manual_sort_order, status, notes)
      VALUES (?, ?, ?, ?, ?, ?, ?, 'paper', ?, ?, ?, ?)
    ", params = list(timestamp, timestamp, title, research_input_value(input$research_new_source_url), research_input_value(input$research_new_pdf_url), research_input_value(input$research_new_source_main_idea), research_input_value(input$research_new_source_abstract), research_input_value(input$research_new_source_name), research_input_integer(input$research_new_source_sort), research_input_default(input$research_new_source_status, "new"), research_input_value(input$research_new_source_notes)))
    article_lab_state$notice <- "Research source added."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_save_source, {
    rows <- selected_research_source()
    if (nrow(rows) == 0) {
      article_lab_state$notice <- "Select a source from the table to edit it."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    id <- rows$research_source_id[[1]]
    dbExecute(con, "
      UPDATE research_sources
      SET updated_at = ?, source_title = ?, source_url = ?, pdf_url = ?, main_idea = ?, abstract = ?, manual_sort_order = ?, status = ?, used_articles = ?, notes = ?
      WHERE research_source_id = ?
    ", params = list(timestamp, research_input_default(input$research_edit_source_title, rows$source_title[[1]]), research_input_value(input$research_edit_source_url), research_input_value(input$research_edit_pdf_url), research_input_value(input$research_edit_source_main), research_input_value(input$research_edit_source_abstract), research_input_integer(input$research_edit_source_sort), research_input_default(input$research_edit_source_status, "new"), research_multiline_value(input$research_edit_source_used_articles), research_input_value(input$research_edit_source_notes), id))
    article_lab_state$notice <- "Research source edits saved."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_add_angle, {
    source <- selected_research_source()
    source_id <- if (nrow(source) == 0) NA_integer_ else source$research_source_id[[1]]
    title <- research_input_value(input$research_new_angle_title)
    if (is.na(source_id) || is.na(title)) {
      article_lab_state$notice <- "Select a source and enter an angle title before creating an angle."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    dbExecute(con, "
      INSERT INTO research_article_angles
        (research_source_id, created_at, updated_at, angle_title, main_idea, manual_sort_order, status, notes)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(source_id, timestamp, timestamp, title, research_input_value(input$research_new_angle_main_idea), research_input_integer(input$research_new_angle_sort), research_input_default(input$research_new_angle_status, "idea"), research_input_value(input$research_new_angle_notes)))
    article_lab_state$notice <- "Research angle created."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_save_angle, {
    rows <- selected_research_angle()
    if (nrow(rows) == 0) return()
    timestamp <- now_utc()
    id <- rows$research_angle_id[[1]]
    dbExecute(con, "
      UPDATE research_article_angles
      SET updated_at = ?, angle_title = ?, main_idea = ?, manual_sort_order = ?, status = ?, notes = ?
      WHERE research_angle_id = ?
    ", params = list(timestamp, research_input_default(input$research_edit_angle_title, rows$angle_title[[1]]), research_input_value(input$research_edit_angle_main), research_input_integer(input$research_edit_angle_sort), research_input_default(input$research_edit_angle_status, "idea"), research_input_value(input$research_edit_angle_notes), id))
    article_lab_state$notice <- "Research angle edits saved."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  save_research_summary <- function(status) {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before saving a summary."
      return(NULL)
    }
    summary_text <- research_multiline_value(input$research_summary_text)
    if (is.na(summary_text)) {
      article_lab_state$notice <- "Enter summary text before saving."
      return(NULL)
    }
    timestamp <- now_utc()
    source_id <- source$research_source_id[[1]]
    if (identical(status, "draft")) {
      existing <- load_research_source_summary(con, source_id, status = "draft")
      if (nrow(existing) > 0) {
        dbExecute(con, "UPDATE research_source_summaries SET updated_at = ?, summary_text = ?, status = 'draft' WHERE summary_id = ?", params = list(timestamp, summary_text, existing$summary_id[[1]]))
        return(existing$summary_id[[1]])
      }
      dbExecute(con, "INSERT INTO research_source_summaries (research_source_id, created_at, updated_at, summary_text, status) VALUES (?, ?, ?, ?, 'draft')", params = list(source_id, timestamp, timestamp, summary_text))
      return(dbGetQuery(con, "SELECT last_insert_rowid() AS summary_id")$summary_id[[1]])
    }
    dbExecute(con, "INSERT INTO research_source_summaries (research_source_id, created_at, updated_at, summary_text, status, confirmed_at) VALUES (?, ?, ?, ?, 'confirmed', ?)", params = list(source_id, timestamp, timestamp, summary_text, timestamp))
    dbGetQuery(con, "SELECT last_insert_rowid() AS summary_id")$summary_id[[1]]
  }

  observeEvent(input$research_save_summary_draft, {
    summary_id <- save_research_summary("draft")
    if (!is.null(summary_id)) {
      article_lab_state$notice <- sprintf("Saved summary draft %s.", summary_id)
      research_refresh(research_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$research_confirm_summary, {
    summary_id <- save_research_summary("confirmed")
    if (!is.null(summary_id)) {
      article_lab_state$notice <- sprintf("Confirmed summary %s.", summary_id)
      research_refresh(research_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$research_download_pdf, {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before downloading a PDF."
      return(invisible(NULL))
    }
    source_id <- source$research_source_id[[1]]
    url <- research_pdf_source_url(source)
    if (is.na(url)) {
      save_research_pdf_asset(con, source_id, "failed", error = "No PDF URL found. Add a PDF URL or use manual upload.")
      article_lab_state$notice <- "No PDF URL found. Add a PDF URL or use manual upload."
      research_refresh(research_refresh() + 1L)
      return(invisible(NULL))
    }
    original_filename <- basename(strsplit(url, "[?#]", perl = TRUE)[[1]][[1]])
    if (!nzchar(original_filename) || identical(original_filename, "/")) original_filename <- NA_character_
    destination <- research_pdf_local_path(source_id, source$source_title[[1]], original_filename)
    temp_path <- tempfile(fileext = ".pdf")
    result <- tryCatch({
      utils::download.file(url, temp_path, mode = "wb", quiet = TRUE)
      if (!research_file_is_pdf(temp_path)) stop("Downloaded file is not a PDF.", call. = FALSE)
      if (!file.copy(temp_path, destination, overwrite = TRUE)) stop("Could not copy downloaded PDF into local folder.", call. = FALSE)
      sha <- research_pdf_sha256(destination)
      save_research_pdf_asset(con, source_id, "downloaded", source_url = url, local_path = destination, original_filename = original_filename, file_sha256 = sha, error = NA_character_)
      sprintf("Downloaded PDF to %s.", destination)
    }, error = function(e) {
      save_research_pdf_asset(con, source_id, "failed", source_url = url, local_path = NA_character_, original_filename = original_filename, file_sha256 = NA_character_, error = conditionMessage(e))
      sprintf("PDF download failed: %s", conditionMessage(e))
    }, finally = {
      if (file.exists(temp_path)) unlink(temp_path)
    })
    article_lab_state$notice <- result
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_pdf_upload, {
    source <- selected_research_source()
    upload <- input$research_pdf_upload
    if (nrow(source) == 0 || is.null(upload) || nrow(upload) == 0) return(invisible(NULL))
    source_id <- source$research_source_id[[1]]
    original_filename <- upload$name[[1]]
    destination <- research_pdf_local_path(source_id, source$source_title[[1]], original_filename)
    result <- tryCatch({
      if (!grepl("\\.pdf$", original_filename, ignore.case = TRUE) && !identical(upload$type[[1]], "application/pdf")) stop("Uploaded file is not a PDF.", call. = FALSE)
      if (!research_file_is_pdf(upload$datapath[[1]])) stop("Uploaded file content is not a PDF.", call. = FALSE)
      if (!file.copy(upload$datapath[[1]], destination, overwrite = TRUE)) stop("Could not copy uploaded PDF into local folder.", call. = FALSE)
      sha <- research_pdf_sha256(destination)
      save_research_pdf_asset(con, source_id, "uploaded", source_url = NA_character_, local_path = destination, original_filename = original_filename, file_sha256 = sha, error = NA_character_)
      sprintf("Uploaded PDF to %s.", destination)
    }, error = function(e) {
      save_research_pdf_asset(con, source_id, "failed", source_url = NA_character_, local_path = NA_character_, original_filename = original_filename, file_sha256 = NA_character_, error = conditionMessage(e))
      sprintf("PDF upload failed: %s", conditionMessage(e))
    })
    article_lab_state$notice <- result
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_clear_pdf, {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before clearing a PDF asset."
      return(invisible(NULL))
    }
    save_research_pdf_asset(con, source$research_source_id[[1]], "missing", source_url = NA_character_, local_path = NA_character_, original_filename = NA_character_, file_sha256 = NA_character_, error = NA_character_)
    article_lab_state$notice <- "PDF asset cleared. Download or upload a replacement PDF."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_generate_summary_draft, {
    source <- selected_research_source()
    asset <- selected_research_pdf_asset()
    prompt_version <- input$research_summary_prompt_version
    prompt_text <- article_lab_input_multiline(input$research_summary_api_prompt) %||% article_lab_default_research_summary_prompt
    save_research_summary_prompt(con, prompt_version, prompt_text)
    result <- tryCatch(
      research_summary_api_request(
        source = source,
        asset = asset,
        model = input$research_summary_model,
        prompt_version = prompt_version,
        prompt = prompt_text
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste("Summary generation failed:", conditionMessage(result))
      return(invisible(NULL))
    }

    updateTextAreaInput(session, "research_summary_text", value = result$summary_text)
    timestamp <- now_utc()
    source_id <- source$research_source_id[[1]]
    existing <- load_research_source_summary(con, source_id, status = "draft")
    if (nrow(existing) > 0) {
      dbExecute(con, "
        UPDATE research_source_summaries
        SET updated_at = ?, summary_text = ?, status = 'draft', model = ?, prompt_version = ?, raw_json = ?
        WHERE summary_id = ?
      ", params = list(timestamp, result$summary_text, result$model, result$prompt_version, result$raw_json, existing$summary_id[[1]]))
      summary_id <- existing$summary_id[[1]]
    } else {
      dbExecute(con, "
        INSERT INTO research_source_summaries
          (research_source_id, created_at, updated_at, summary_text, status, model, prompt_version, raw_json)
        VALUES (?, ?, ?, ?, 'draft', ?, ?, ?)
      ", params = list(source_id, timestamp, timestamp, result$summary_text, result$model, result$prompt_version, result$raw_json))
      summary_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS summary_id")$summary_id[[1]]
    }
    article_lab_state$notice <- sprintf("Generated and saved summary draft %s with model %s.", summary_id, result$model)
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  use_confirmed_summary_in_generate <- function(summary_id) {
    rows <- confirmed_research_summaries()
    summary_id_value <- research_input_integer(summary_id)
    if (is.na(summary_id_value) || nrow(rows) == 0 || !(summary_id_value %in% rows$summary_id)) return(FALSE)
    row <- rows[match(summary_id_value, rows$summary_id), , drop = FALSE]
    prompt <- research_summary_prompt(row)
    updateTextAreaInput(session, "article_lab_prompt", value = prompt)
    updateTextInput(session, "article_lab_seed_topic", value = row$source_title[[1]] %||% "")
    updateSelectInput(session, "article_lab_inspiration_source", selected = "")
    updateSelectizeInput(session, "article_lab_research_summary_id", selected = as.character(summary_id_value))
    active_section("generate")
    TRUE
  }

  observeEvent(input$research_send_summary_to_generate, {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before sending a summary to Generate."
      return(invisible(NULL))
    }
    confirmed <- load_research_source_summary(con, source$research_source_id[[1]], status = "confirmed")
    if (nrow(confirmed) == 0) {
      article_lab_state$notice <- "Confirm this source summary before sending it to Generate."
      return(invisible(NULL))
    }
    if (use_confirmed_summary_in_generate(confirmed$summary_id[[1]])) {
      article_lab_state$notice <- sprintf("Loaded confirmed summary %s into Generate.", confirmed$summary_id[[1]])
    }
  }, ignoreInit = TRUE)

  observeEvent(input$research_send_to_title_lab, {
    angle_id <- research_input_integer(input$research_send_to_title_lab)
    angle <- dbGetQuery(con, "SELECT * FROM research_article_angles WHERE research_angle_id = ? LIMIT 1", params = list(angle_id))
    if (nrow(angle) == 0 || is.na(angle$research_source_id[[1]])) return()
    source <- dbGetQuery(con, "SELECT * FROM research_sources WHERE research_source_id = ? LIMIT 1", params = list(angle$research_source_id[[1]]))
    if (nrow(source) == 0) return()
    prompt <- research_title_prompt(source, angle)
    inspiration <- paste0("research_angle:", angle_id)
    generated <- generate_title_candidates(con, prompt, batch_size = input$article_lab_batch_size %||% 12L, seed_topic = angle$angle_title[[1]], inspiration_source = inspiration, model = input$article_lab_model %||% article_lab_default_model)
    batch_id <- save_article_lab_batch(con, prompt, angle$angle_title[[1]], inspiration, input$article_lab_batch_size %||% 12L, generated$model %||% input$article_lab_model %||% article_lab_default_model, generated$titles$title, raw_json = generated$raw_json, generation_mode = generated$mode %||% "research_inbox")
    dbExecute(con, "UPDATE research_article_angles SET updated_at = ?, status = 'sent_to_title_lab', article_lab_batch_id = ? WHERE research_angle_id = ?", params = list(now_utc(), batch_id, angle_id))
    updateTextAreaInput(session, "article_lab_prompt", value = prompt)
    updateTextInput(session, "article_lab_seed_topic", value = angle$angle_title[[1]])
    updateSelectInput(session, "article_lab_inspiration_source", selected = "custom")
    active_section("generate")
    article_lab_state$notice <- sprintf("Sent research angle %s to Title Lab as batch %s.", angle_id, batch_id)
    article_lab_refresh(article_lab_refresh() + 1L)
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  output$article_lab_notice <- renderUI({
    notice <- article_lab_state$notice
    if (is.null(notice) || !nzchar(notice)) return(NULL)
    div(class = "lab-status-copy", notice)
  })

  output$research_summary_source_selector <- renderUI({
    rows <- research_summary_sources()
    choices <- if (nrow(rows) == 0) character() else setNames(
      rows$research_source_id,
      sprintf(
        "%s%s · %s",
        ifelse(is.na(rows$manual_sort_order), "", sprintf("#%s ", rows$manual_sort_order)),
        rows$source_title,
        rows$status
      )
    )
    selected <- selected_research_source_id()
    selectizeInput("research_summary_source_id", "Source", choices = choices, selected = selected, width = "100%")
  })

  output$research_summary_selected_source <- renderUI({
    source <- selected_research_source()
    if (nrow(source) == 0) return(div(class = "lab-status-copy", "Select a source to write or confirm a summary."))
    rank_copy <- if (is.na(source$manual_sort_order[[1]])) "Unranked" else sprintf("Rank #%s", source$manual_sort_order[[1]])
    main_idea <- research_input_value(source$main_idea[[1]])
    abstract <- research_input_value(source$abstract[[1]])
    div(
      class = "lab-status-copy",
      h3(source$source_title[[1]]),
      HTML(sprintf("<strong>%s</strong> · status: %s · %s", htmltools::htmlEscape(rank_copy), htmltools::htmlEscape(source$status[[1]] %||% ""), research_links(source$source_url[[1]], source$pdf_url[[1]]))),
      if (!is.na(main_idea)) p(strong("Main idea: "), main_idea),
      if (!is.na(abstract)) p(strong("Abstract: "), abstract)
    )
  })

  output$research_summary_pdf_status <- renderUI({
    source <- selected_research_source()
    if (nrow(source) == 0) return(NULL)
    asset <- selected_research_pdf_asset()
    status <- if (nrow(asset) == 0) "missing" else research_input_default(asset$status[[1]], "missing")
    status_label <- research_pdf_status_labels[[status]] %||% status
    local_path <- if (nrow(asset) == 0) NA_character_ else research_input_value(asset$local_path[[1]])
    source_url <- if (nrow(asset) == 0) research_pdf_source_url(source) else research_input_value(asset$source_url[[1]])
    error <- if (nrow(asset) == 0) NA_character_ else research_input_value(asset$error[[1]])
    div(
      class = "lab-card",
      h3("PDF"),
      p(strong("PDF status: "), status_label),
      if (!is.na(local_path)) p(strong("Local path: "), local_path),
      if (!is.na(source_url)) p(strong("Source URL used: "), source_url),
      if (identical(status, "failed") && !is.na(error)) p(strong("Error: "), error)
    )
  })

  output$research_summary_pdf_gate <- renderUI({
    asset <- selected_research_pdf_asset()
    ready <- nrow(asset) > 0 && asset$status[[1]] %in% c("downloaded", "uploaded") && !is.na(research_input_value(asset$local_path[[1]]))
    copy <- if (isTRUE(ready)) "PDF ready for summary generation." else "Download or upload a PDF before generating an API summary."
    div(class = "lab-status-copy", copy)
  })

  output$article_lab_research_summary_selector <- renderUI({
    rows <- confirmed_research_summaries()
    choices <- if (nrow(rows) == 0) character() else setNames(
      rows$summary_id,
      sprintf("%s · %s", rows$source_title, rows$confirmed_at %||% rows$updated_at)
    )
    empty_choice <- stats::setNames("", "")
    selectizeInput("article_lab_research_summary_id", "Research summary inspiration", choices = c(empty_choice, choices), selected = "", width = "100%")
  })

  output$article_lab_effective_prompt <- renderUI({
    effective <- article_lab_effective_generation_inputs()
    summary_mode <- identical(effective$mode, "research_summary")
    mode_copy <- if (summary_mode) {
      sprintf(
        "Research summary mode: Generate will use the manual/default prompt as title guidance, ignore the manual seed/topic and manual inspiration-source dropdown, and use confirmed summary %s (%s) as the article summary.",
        effective$summary_id,
        effective$source_title
      )
    } else {
      "Manual mode: Generate will use the manual/default prompt textarea, optional seed/topic, and optional inspiration-source dropdown below."
    }
    request_additions <- paste(
      sprintf("Batch size: %s", input$article_lab_batch_size %||% 12L),
      sprintf("Model: %s", input$article_lab_model %||% article_lab_default_model),
      sprintf("Seed topic: %s", article_lab_input_string(effective$seed_topic) %||% "(none)"),
      sprintf("Inspiration source: %s", article_lab_input_string(effective$inspiration_source) %||% "(none)"),
      sep = "\n"
    )
    example_titles <- if (identical(article_lab_input_string(effective$inspiration_source), "top performing titles")) {
      article_lab_top_title_examples(con, limit = 8L)
    } else {
      character()
    }
    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", mode_copy),
      tags$details(
        open = if (summary_mode) "open" else NULL,
        tags$summary("Show exact effective prompt"),
        h4("Title helper wrapper"),
        tags$pre(class = "lab-status-copy", paste(
          "You generate Medium-style article title candidates for personal finance and investing.",
          "Return valid JSON only in the shape {\"titles\": [\"...\", \"...\"]}.",
          sprintf("Return exactly %s titles.", input$article_lab_batch_size %||% 12L),
          sprintf("Every title must be at most %s characters, including spaces.", article_lab_title_max_chars),
          sprintf("Prefer %s-%s characters when possible. Do not make titles long unless the extra words clearly improve clarity or curiosity.", article_lab_title_preferred_min_chars, article_lab_title_preferred_max_chars),
          "Do not include explanations, numbering, markdown, or code fences.",
          "Do not copy any example title verbatim.",
          "Keep the titles credible, science-based, beginner-friendly, and not clickbait.",
          "If a title would exceed the limit, rewrite it shorter instead of truncating it.",
          sep = "\n"
        )),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        if (summary_mode && nzchar(trimws(effective$manual_prompt %||% ""))) tagList(
          h4("Manual/default prompt"),
          tags$pre(class = "lab-status-copy", effective$manual_prompt)
        ),
        if (length(example_titles) > 0) tagList(
          h4("Reference examples sent as inspiration"),
          tags$pre(class = "lab-status-copy", paste(sprintf("%s. %s", seq_along(example_titles), example_titles), collapse = "\n"))
        ),
        h4("Article summary"),
        tags$pre(class = "lab-status-copy", effective$prompt)
      )
    )
  })

  output$research_summary_effective_prompt <- renderUI({
    source <- selected_research_source()
    asset <- selected_research_pdf_asset()
    prompt_text <- article_lab_input_multiline(input$research_summary_api_prompt) %||% article_lab_default_research_summary_prompt
    pdf_status <- if (nrow(asset) == 0) "missing" else research_input_default(asset$status[[1]], "missing")
    local_pdf_path <- if (nrow(asset) == 0) NA_character_ else research_input_value(asset$local_path[[1]])
    resolved_pdf_path <- research_resolve_local_pdf_path(local_pdf_path)
    request_fields <- paste(
      sprintf("Model: %s", article_lab_input_string(input$research_summary_model) %||% article_lab_default_research_summary_model),
      sprintf("Prompt version: %s", article_lab_input_string(input$research_summary_prompt_version) %||% article_lab_default_research_summary_prompt_version),
      sprintf("PDF attachment status: %s", pdf_status),
      sprintf("PDF attachment filename/path: %s", resolved_pdf_path %||% "(none)"),
      sep = "\n"
    )
    metadata_text <- if (nrow(source) == 0) {
      "(No source selected. Select a source to see the exact source metadata sent with the PDF.)"
    } else {
      paste(
        "Source metadata:",
        sprintf("Research source ID: %s", source$research_source_id[[1]] %||% ""),
        sprintf("Source title: %s", article_lab_input_string(source$source_title[[1]]) %||% ""),
        sprintf("Source URL: %s", article_lab_input_string(source$source_url[[1]]) %||% ""),
        sprintf("PDF URL: %s", article_lab_input_string(source$pdf_url[[1]]) %||% ""),
        "Main idea:",
        article_lab_input_multiline(source$main_idea[[1]]) %||% "",
        "",
        "Abstract:",
        article_lab_input_multiline(source$abstract[[1]]) %||% "",
        "",
        "User prompt:",
        prompt_text,
        sep = "\n"
      )
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Summary generation sends the selected PDF as an input_file plus this text metadata/prompt payload."),
      tags$details(
        open = if (nrow(source) > 0) "open" else NULL,
        tags$summary("Show exact research summary API prompt"),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_fields),
        h4("Input text sent with PDF"),
        tags$pre(class = "lab-status-copy", metadata_text)
      )
    )
  })

  output$article_lab_score_effective_prompt <- renderUI({
    queue_rows <- article_lab_queue_rows()
    selected_ids <- collect_selected_ids(
      queue_rows,
      "article_lab_queue_select",
      snapshot_ids = input$article_lab_queue_selected_snapshot
    )
    selected_rows <- if (length(selected_ids) > 0 && nrow(queue_rows) > 0) {
      queue_rows[queue_rows$candidate_id %in% selected_ids, , drop = FALSE]
    } else {
      queue_rows[0, , drop = FALSE]
    }
    model <- article_lab_input_string(input$article_lab_score_model) %||% article_lab_default_score_model
    prompt_version <- article_lab_input_string(input$article_lab_score_prompt_version) %||% article_lab_default_score_prompt_version
    scope <- article_lab_input_string(input$article_lab_score_scope) %||% article_lab_default_score_scope
    request_fields <- paste(
      sprintf("Model: %s", model),
      sprintf("Prompt version: %s", prompt_version),
      sprintf("Scope: %s", scope),
      "Response format: strict JSON schema with curiosity, emotional_pull, medium_comment_potential, overall_article_potential, trust_risk, predicted_success_bucket, and short_reason.",
      sep = "\n"
    )
    user_prompts <- if (nrow(selected_rows) == 0) {
      "(No selected API-queue titles. Select title checkboxes to see the exact per-title user prompt that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_rows)), function(i) {
        paste(
          sprintf("candidate_id=%s | batch_id=%s", selected_rows$candidate_id[[i]], selected_rows$batch_id[[i]]),
          article_lab_score_user_prompt(prompt_version, scope, selected_rows$title[[i]]),
          sep = "\n\n"
        )
      }, character(1)), collapse = "\n\n---\n\n")
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "API scoring sends one request per selected title. Each request uses this system prompt plus the per-title user prompt below."),
      tags$details(
        open = if (nrow(selected_rows) > 0) "open" else NULL,
        tags$summary("Show exact title scoring API prompt"),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_fields),
        h4("System prompt"),
        tags$pre(class = "lab-status-copy", article_lab_score_system_prompt),
        h4("Per-title user prompt"),
        tags$pre(class = "lab-status-copy", user_prompts)
      )
    )
  })

  output$article_lab_subtitle_effective_prompt <- renderUI({
    targets <- article_lab_subtitle_target_rows()
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(targets$batch_id))
    has_summary <- nrow(summary_contexts) > 0
    variants_per_title <- max(1L, min(8L, suppressWarnings(as.integer(input$article_lab_subtitle_variants_per_title)) %||% 4L))
    base_prompt <- article_lab_input_multiline(input$article_lab_subtitle_prompt) %||% article_lab_default_subtitle_prompt
    request_additions <- paste(
      sprintf("Model: %s", article_lab_input_string(input$article_lab_subtitle_model) %||% article_lab_default_subtitle_model),
      sprintf("Subtitle candidates per title: %s", variants_per_title),
      sprintf("Max subtitle characters: %s", article_lab_subtitle_max_chars),
      sep = "\n"
    )
    title_list <- if (nrow(targets) == 0) {
      "(No eligible approved titles in the current batch filter.)"
    } else {
      paste(vapply(seq_len(nrow(targets)), function(i) {
        sprintf("%s. candidate_id=%s | batch_id=%s | title=%s", i, targets$candidate_id[[i]], targets$batch_id[[i]], targets$title[[i]])
      }, character(1)), collapse = "\n")
    }
    summary_copy <- if (has_summary) {
      "Subtitle generation will append the confirmed article summary attached to each title's source batch."
    } else {
      "No attached research summary was found for the eligible titles in the current batch filter. Subtitle generation will use the base prompt and titles only."
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", summary_copy),
      tags$details(
        open = if (has_summary) "open" else NULL,
        tags$summary("Show exact effective prompt"),
        h4("Subtitle prompt"),
        tags$pre(class = "lab-status-copy", base_prompt),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        h4("Titles"),
        tags$pre(class = "lab-status-copy", title_list),
        if (has_summary) tagList(
          h4("Attached article summaries"),
          tags$pre(class = "lab-status-copy", paste(vapply(seq_len(nrow(summary_contexts)), function(i) {
            paste(
              sprintf("Batch: %s", summary_contexts$batch_id[[i]]),
              sprintf("Summary ID: %s", summary_contexts$summary_id[[i]]),
              sprintf("Source title: %s", summary_contexts$source_title[[i]] %||% ""),
              summary_contexts$article_summary[[i]],
              sep = "\n"
            )
          }, character(1)), collapse = "\n\n---\n\n"))
        )
      )
    )
  })

  output$article_lab_thumbnail_effective_prompt <- renderUI({
    packages <- article_lab_thumbnail_package_rows()
    variants_per_package <- max(1L, min(4L, suppressWarnings(as.integer(input$article_lab_thumbnail_variants_per_package)) %||% article_lab_default_thumbnail_variants))
    base_prompt <- article_lab_input_multiline(input$article_lab_thumbnail_prompt) %||% article_lab_default_thumbnail_prompt
    selected_ids <- collect_selected_ids(
      packages,
      "article_lab_thumbnail_package_select",
      snapshot_ids = input$article_lab_thumbnail_packages_selected_snapshot,
      key_col = "subtitle_id"
    )
    selected_packages <- if (length(selected_ids) > 0 && nrow(packages) > 0) {
      packages[packages$subtitle_id %in% selected_ids, , drop = FALSE]
    } else {
      packages[0, , drop = FALSE]
    }
    request_additions <- paste(
      sprintf("Model: %s", article_lab_input_string(input$article_lab_thumbnail_model) %||% article_lab_default_thumbnail_model),
      "Image generation: Responses API built-in image_generation tool",
      sprintf("Thumbnail candidates per package: %s", variants_per_package),
      sep = "\n"
    )
    package_list <- if (nrow(selected_packages) == 0) {
      "(No selected eligible title/subtitle packages. Select package checkboxes to see the exact package context that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        sprintf(
          "%s. subtitle_id=%s | candidate_id=%s | batch_id=%s\nTitle: %s\nSubtitle: %s",
          i,
          selected_packages$subtitle_id[[i]],
          selected_packages$candidate_id[[i]],
          selected_packages$batch_id[[i]],
          selected_packages$title[[i]],
          selected_packages$subtitle[[i]]
        )
      }, character(1)), collapse = "\n\n")
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Thumbnail generation sends this prompt plus the selected title/subtitle package context to the selected Responses model, which calls the built-in image_generation tool."),
      tags$details(
        open = if (nrow(selected_packages) > 0) "open" else NULL,
        tags$summary("Show exact thumbnail API prompt"),
        h4("Thumbnail prompt"),
        tags$pre(class = "lab-status-copy", base_prompt),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        h4("Selected title/subtitle packages"),
        tags$pre(class = "lab-status-copy", package_list)
      )
    )
  })

  output$article_lab_outline_context_toggle <- renderUI({
    packages <- article_lab_ready_for_outline_rows()
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(packages$batch_id))
    pdf_href <- NA_character_
    if (nrow(summary_contexts) > 0) {
      pdf_urls <- clean_text(summary_contexts$pdf_url)
      pdf_urls <- pdf_urls[!is.na(pdf_urls)]
      if (length(pdf_urls) > 0) pdf_href <- pdf_urls[[1]]
      if (is.na(pdf_href)) {
        local_paths <- vapply(summary_contexts$pdf_local_path, research_resolve_local_pdf_path, character(1))
        local_paths <- local_paths[!is.na(local_paths) & file.exists(local_paths)]
        if (length(local_paths) > 0) {
          pdf_href <- paste0("file://", URLencode(normalizePath(local_paths[[1]], mustWork = TRUE), reserved = TRUE))
        }
      }
    }
    pdf_label <- if (is.na(pdf_href)) {
      '<span style="color:#1a73e8;font-size:0.85em;font-weight:700;letter-spacing:0.03em;">PDF</span>'
    } else {
      sprintf(
        '<a href="%s" target="_blank" rel="noopener noreferrer" style="color:#1a73e8;font-size:0.85em;font-weight:700;letter-spacing:0.03em;text-decoration:underline;">PDF</a>',
        htmltools::htmlEscape(pdf_href)
      )
    }
    div(
      class = "lab-field",
      checkboxInput(
        "article_lab_outline_include_context",
        HTML(paste0("Include available research context ", pdf_label, " preferred, text fallback")),
        value = TRUE,
        width = "100%"
      )
    )
  })

  output$article_lab_outline_effective_prompt <- renderUI({
    packages <- article_lab_ready_for_outline_rows()
    selected_ids <- collect_selected_ids(
      packages,
      "article_lab_outline_packages",
      snapshot_ids = input$article_lab_outline_packages_selected_snapshot,
      key_col = "thumbnail_id"
    )
    selected_packages <- if (length(selected_ids) > 0 && nrow(packages) > 0) {
      packages[packages$thumbnail_id %in% selected_ids, , drop = FALSE]
    } else {
      packages[0, , drop = FALSE]
    }
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(packages$batch_id))
    include_context <- isTRUE(input$article_lab_outline_include_context)
    base_prompt <- article_lab_input_multiline(input$article_lab_outline_prompt) %||% article_lab_default_outline_prompt
    request_additions <- paste(
      sprintf("Model: %s", article_lab_input_string(input$article_lab_outline_model) %||% article_lab_default_outline_model),
      sprintf("Include available research context: %s", if (include_context) "yes" else "no"),
      "Response format: JSON with one Markdown outline_text per selected package.",
      sep = "\n"
    )
    context_payload <- if (!include_context) {
      "(Research context is available only if listed above, but the include-context toggle is off.)"
    } else if (nrow(summary_contexts) == 0) {
      "(No research context will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(summary_contexts)), function(i) {
        pdf_path <- research_resolve_local_pdf_path(summary_contexts$pdf_local_path[[i]])
        has_pdf <- !is.na(pdf_path) && file.exists(pdf_path)
        if (has_pdf) {
          paste(
            sprintf("Batch: %s", summary_contexts$batch_id[[i]]),
            "Context sent: PDF file attachment",
            "Text summary sent: no, because the PDF itself is attached",
            sep = "\n"
          )
        } else {
          paste(
            sprintf("Batch: %s", summary_contexts$batch_id[[i]]),
            "Context sent: text summary fallback",
            sprintf("Summary ID: %s", summary_contexts$summary_id[[i]]),
            sprintf("Source title: %s", summary_contexts$source_title[[i]] %||% ""),
            "Exact text sent to API:",
            summary_contexts$article_summary[[i]],
            sep = "\n"
          )
        }
      }, character(1)), collapse = "\n\n---\n\n")
    }
    package_list <- if (nrow(selected_packages) == 0) {
      "(No selected title/subtitle/thumbnail packages. Select Generate outline or Regenerate outline checkboxes to see the exact package context that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        sprintf(
          "%s. thumbnail_id=%s | subtitle_id=%s | candidate_id=%s | batch_id=%s\nTitle: %s\nSubtitle: %s\nThumbnail label: %s",
          i,
          selected_packages$thumbnail_id[[i]],
          selected_packages$subtitle_id[[i]],
          selected_packages$candidate_id[[i]],
          selected_packages$batch_id[[i]],
          selected_packages$title[[i]],
          selected_packages$subtitle[[i]],
          selected_packages$thumbnail_label[[i]] %||% "approved thumbnail"
        )
      }, character(1)), collapse = "\n\n")
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Outline generation sends this prompt plus selected title/subtitle/thumbnail package context. When enabled, a local PDF is attached as a file; summary text is sent only when no local PDF is available."),
      tags$details(
        open = if (nrow(selected_packages) > 0 || nrow(summary_contexts) > 0) "open" else NULL,
        tags$summary("Show exact outline API prompt"),
        h4("Outline prompt"),
        tags$pre(class = "lab-status-copy", base_prompt),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        h4("Selected packages"),
        tags$pre(class = "lab-status-copy", package_list),
        h4("Research context sent"),
        tags$pre(class = "lab-status-copy", context_payload)
      )
    )
  })

  output$article_lab_full_text_effective_prompt <- renderUI({
    rows <- article_lab_full_text_rows()
    packages <- article_lab_full_text_package_rows(rows)
    selected_ids <- collect_selected_ids(packages, "article_lab_full_text_packages", snapshot_ids = input$article_lab_full_text_packages_selected_snapshot, key_col = "outline_id")
    if (length(selected_ids) > 1) selected_ids <- selected_ids[[1]]
    selected_packages <- if (length(selected_ids) > 0 && nrow(packages) > 0) packages[packages$outline_id %in% selected_ids, , drop = FALSE] else packages[0, , drop = FALSE]
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(packages$batch_id))
    include_context <- isTRUE(input$article_lab_full_text_include_context)
    base_prompt <- article_lab_input_multiline(input$article_lab_full_text_prompt) %||% article_lab_default_full_text_prompt
    prompt_key <- article_lab_input_string(input$article_lab_full_text_prompt_key) %||% article_lab_full_text_prompt_key
    request_additions <- paste(
      sprintf("Model: %s", article_lab_input_string(input$article_lab_full_text_model) %||% article_lab_default_full_text_model),
      sprintf("Prompt key/version: %s", prompt_key),
      sprintf("Include available source context: %s", if (include_context) "yes" else "no"),
      "Response format: JSON with one complete Markdown full_text for the selected package; if a single-package response is plain Markdown, the helper accepts it as that package's draft.",
      sep = "\n"
    )
    package_list <- if (nrow(selected_packages) == 0) {
      "(No selected approved outline. Select one outline checkbox to see the exact package context that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        sprintf(
          "%s. outline_id=%s | thumbnail_id=%s | subtitle_id=%s | candidate_id=%s | batch_id=%s\nTitle: %s\nSubtitle: %s\nThumbnail concept: %s\nApproved outline:\n%s",
          i,
          selected_packages$outline_id[[i]], selected_packages$thumbnail_id[[i]], selected_packages$subtitle_id[[i]], selected_packages$candidate_id[[i]], selected_packages$batch_id[[i]],
          selected_packages$title[[i]], selected_packages$subtitle[[i]], selected_packages$thumbnail_label[[i]] %||% "approved thumbnail", selected_packages$outline_text[[i]]
        )
      }, character(1)), collapse = "\n\n---\n\n")
    }
    exact_package_list <- if (nrow(selected_packages) == 0) {
      "(No selected approved outline. Select one outline checkbox to see the exact package context that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        context <- if (nrow(summary_contexts) == 0) data.frame() else summary_contexts[summary_contexts$batch_id == selected_packages$batch_id[[i]], , drop = FALSE]
        pdf_path <- if (nrow(context) > 0 && isTRUE(include_context)) research_resolve_local_pdf_path(context$pdf_local_path[[1]]) else NA_character_
        has_pdf <- !is.na(pdf_path) && file.exists(pdf_path)
        has_summary <- isTRUE(include_context) && nrow(context) > 0 && !is.na(context$article_summary[[1]]) && nzchar(context$article_summary[[1]])
        source_mode <- if (!isTRUE(include_context)) "none" else if (has_pdf) "pdf_attachment" else if (has_summary) "summary_fallback" else "none"
        lines <- c(
          sprintf(
            "%s. outline_id=%s | thumbnail_id=%s | subtitle_id=%s | candidate_id=%s | batch_id=%s",
            i,
            selected_packages$outline_id[[i]], selected_packages$thumbnail_id[[i]], selected_packages$subtitle_id[[i]], selected_packages$candidate_id[[i]], selected_packages$batch_id[[i]]
          ),
          sprintf("Title: %s", selected_packages$title[[i]]),
          sprintf("Subtitle: %s", selected_packages$subtitle[[i]]),
          sprintf("Thumbnail concept: %s", selected_packages$thumbnail_label[[i]] %||% "approved thumbnail"),
          "Approved outline:",
          selected_packages$outline_text[[i]],
          sprintf("Source context mode: %s", source_mode)
        )
        if (identical(source_mode, "summary_fallback")) lines <- c(lines, "Research summary/full text fallback:", context$article_summary[[1]])
        if (identical(source_mode, "pdf_attachment")) lines <- c(lines, "Research PDF: attached as input_file")
        paste(lines, collapse = "\n")
      }, character(1)), collapse = "\n\n")
    }
    exact_api_prompt <- paste(
      base_prompt,
      "Return valid JSON only.",
      "Return JSON only in this shape: {\"results\":[{\"outline_id\":string,\"thumbnail_id\":string,\"subtitle_id\":string,\"candidate_id\":string,\"batch_id\":string,\"source_context_mode\":\"pdf_attachment\"|\"summary_fallback\"|\"none\",\"full_text\":string}]}",
      "Copy ids exactly from the package. The full_text value must be the complete Markdown article draft, not a schema example, MARKDOWN_ARTICLE_HERE, placeholder, excerpt, note, or explanation.",
      "Ignore any earlier placeholder value such as MARKDOWN_ARTICLE_HERE; replace it with the actual full Markdown article.",
      "Return one full article draft per package, preserving all ids exactly.",
      "Packages:",
      exact_package_list,
      sep = "\n\n"
    )
    context_payload <- if (!include_context) {
      "(Source context toggle is off. No PDF or summary text will be sent.)"
    } else if (nrow(selected_packages) == 0) {
      "(Select approved outlines to see source context for those packages.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        context <- if (nrow(summary_contexts) == 0) data.frame() else summary_contexts[summary_contexts$batch_id == selected_packages$batch_id[[i]], , drop = FALSE]
        pdf_path <- if (nrow(context) > 0) research_resolve_local_pdf_path(context$pdf_local_path[[1]]) else NA_character_
        has_pdf <- !is.na(pdf_path) && file.exists(pdf_path)
        has_summary <- nrow(context) > 0 && !is.na(context$article_summary[[1]]) && nzchar(context$article_summary[[1]])
        if (has_pdf) {
          paste(sprintf("Outline: %s", selected_packages$outline_id[[i]]), "Context sent: PDF file attachment", sprintf("Attached local file/path: %s", pdf_path), "Text summary sent: no, because the PDF itself is attached", sep = "\n")
        } else if (has_summary) {
          paste(sprintf("Outline: %s", selected_packages$outline_id[[i]]), "Context sent: text summary/full text fallback", sprintf("Summary ID: %s", context$summary_id[[1]]), sprintf("Source title: %s", context$source_title[[1]] %||% ""), "Exact text sent to API:", context$article_summary[[1]], sep = "\n")
        } else {
          paste(sprintf("Outline: %s", selected_packages$outline_id[[i]]), "Context sent: none", sep = "\n")
        }
      }, character(1)), collapse = "\n\n---\n\n")
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Full article generation sends this prompt plus the selected title/subtitle/thumbnail/outline context. When enabled, a local PDF is attached first; summary text is sent only when no local PDF is available."),
      tags$details(
        open = if (nrow(selected_packages) > 0) "open" else NULL,
        tags$summary("Show exact full article API prompt"),
        div(
          class = "lab-actions",
          tags$button(type = "button", class = "btn lab-secondary", onclick = "window.articleLabCopyTextFromElement('article_lab_full_text_exact_api_prompt', this);", "Copy full API prompt")
        ),
        h4("Exact input_text sent to the API"),
        tags$pre(id = "article_lab_full_text_exact_api_prompt", class = "lab-status-copy", exact_api_prompt),
        h4("Full article prompt"),
        tags$pre(class = "lab-status-copy", base_prompt),
        h4("Full article helper wrapper"),
        tags$pre(class = "lab-status-copy", paste(
          "Return one full article draft for the selected package, preserving all ids exactly.",
          "Required response shape: {\"results\":[{\"outline_id\":string,\"thumbnail_id\":string,\"subtitle_id\":string,\"candidate_id\":string,\"batch_id\":string,\"source_context_mode\":\"pdf_attachment\"|\"summary_fallback\"|\"none\",\"full_text\":string}]}",
          "The full_text value must be the complete Markdown article draft, not MARKDOWN_ARTICLE_HERE, a placeholder, schema example, excerpt, note, or explanation. For a single selected package, a plain Markdown response or single-object {\"full_text\":...} response is accepted as that package's draft.",
          sep = "\n"
        )),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        h4("Selected approved outlines"),
        tags$pre(class = "lab-status-copy", package_list),
        h4("Source context sent"),
        tags$pre(class = "lab-status-copy", context_payload)
      )
    )
  })

  output$research_ranked_sources_table <- DT::renderDT({
    rows <- research_ranked_sources()
    display <- if (nrow(rows) == 0) {
      data.frame(research_source_id = integer(), Rank = integer(), Status = character(), Title = character(), Used = character(), Links = character(), Angles = integer(), check.names = FALSE)
    } else {
      source_title <- vapply(rows$source_title, research_input_default, character(1), default = "")
      data.frame(
        research_source_id = rows$research_source_id,
        Rank = seq_len(nrow(rows)),
        Status = rows$status,
        Title = sprintf('<span class="research-source-title" title="%s">%s</span>', htmltools::htmlEscape(source_title), htmltools::htmlEscape(vapply(source_title, research_truncate, character(1), max_chars = 240L))),
        Used = vapply(rows$used_articles, research_truncate, character(1), max_chars = 80L),
        Links = sprintf('<span class="research-source-links">%s</span>', mapply(research_links, rows$source_url, rows$pdf_url, USE.NAMES = FALSE)),
        Angles = rows$angles_count,
        check.names = FALSE
      )
    }
    DT::datatable(display, rownames = FALSE, escape = FALSE, class = "compact stripe hover research-source-table", selection = list(mode = "single", target = "row"), options = list(pageLength = 100, autoWidth = FALSE, order = list(), columnDefs = list(list(targets = 0, visible = FALSE), list(targets = 1, width = "6%"), list(targets = 2, width = "10%"), list(targets = 3, width = "60%"), list(targets = 4, width = "12%"), list(targets = 5, width = "7%"), list(targets = 6, width = "5%"))))
  })

  output$research_unranked_sources_table <- DT::renderDT({
    rows <- research_unranked_sources()
    display <- if (nrow(rows) == 0) {
      data.frame(research_source_id = integer(), Status = character(), Title = character(), `Main idea` = character(), Used = character(), Links = character(), Angles = integer(), check.names = FALSE)
    } else {
      source_title <- vapply(rows$source_title, research_input_default, character(1), default = "")
      data.frame(
        research_source_id = rows$research_source_id,
        Status = rows$status,
        Title = sprintf('<span class="research-source-title" title="%s">%s</span>', htmltools::htmlEscape(source_title), htmltools::htmlEscape(vapply(source_title, research_truncate, character(1), max_chars = 220L))),
        `Main idea` = vapply(rows$main_idea, research_truncate, character(1), max_chars = 120L),
        Used = vapply(rows$used_articles, research_truncate, character(1), max_chars = 70L),
        Links = sprintf('<span class="research-source-links">%s</span>', mapply(research_links, rows$source_url, rows$pdf_url, USE.NAMES = FALSE)),
        Angles = rows$angles_count,
        check.names = FALSE
      )
    }
    DT::datatable(display, rownames = FALSE, escape = FALSE, class = "compact stripe hover research-source-table", selection = list(mode = "single", target = "row"), options = list(pageLength = 100, autoWidth = FALSE, order = list(), columnDefs = list(list(targets = 0, visible = FALSE), list(targets = 1, width = "10%"), list(targets = 2, width = "45%"), list(targets = 3, width = "23%"), list(targets = 4, width = "12%"), list(targets = 5, width = "6%"), list(targets = 6, width = "4%"))))
  })

  output$research_selected_source_summary <- renderUI({
    row <- selected_research_source()
    if (nrow(row) == 0) return(div(class = "lab-status-copy", "Select a ranked or unranked source to edit details and create angles."))
    rank_label <- if (is.na(row$manual_sort_order[[1]])) "Unranked" else paste("Rank", row$manual_sort_order[[1]])
    main_idea <- research_truncate(row$main_idea[[1]], max_chars = 220L)
    div(
      class = "research-selected-summary",
      h3(row$source_title[[1]]),
      div(class = "research-source-links", HTML(research_links(row$source_url[[1]], row$pdf_url[[1]]))),
      div(class = "lab-status-copy", if (nzchar(main_idea)) main_idea else "No main idea saved yet."),
      div(class = "lab-status-copy", sprintf("Status: %s · %s%s", row$status[[1]], rank_label, if (!is.na(row$finished_at[[1]]) && nzchar(row$finished_at[[1]])) paste0(" · Finished ", row$finished_at[[1]]) else "")),
      if (!is.na(row$used_articles[[1]]) && nzchar(row$used_articles[[1]])) div(class = "lab-status-copy", strong("Used for: "), row$used_articles[[1]]) else NULL
    )
  })

  output$research_angle_workspace <- renderUI({
    row <- selected_research_source()
    if (nrow(row) == 0) return(NULL)
    tagList(
      div(class = "lab-status-copy", "Lower angle sort number appears higher."),
      DT::DTOutput("research_angles_table"),
      h3("Finish source"),
      div(class = "lab-field", textAreaInput("research_used_articles", "Article(s) written from this source", value = row$used_articles[[1]] %||% "", width = "100%", height = "70px", placeholder = "One title or URL per line")),
      div(class = "lab-actions", actionButton("research_mark_finished", "Mark selected source finished", class = "lab-secondary")),
      h3("Create angle"),
      div(class = "lab-grid", div(class = "lab-field", textInput("research_new_angle_title", "Angle title", width = "100%")), div(class = "lab-field", numericInput("research_new_angle_sort", "Sort order", value = NULL, width = "100%")), div(class = "lab-field", textInput("research_new_angle_status", "Status", value = "idea", width = "100%"))),
      div(class = "lab-field", textAreaInput("research_new_angle_main_idea", "Angle main idea", width = "100%", height = "90px")),
      div(class = "lab-field", textAreaInput("research_new_angle_notes", "Notes", width = "100%", height = "80px")),
      div(class = "lab-actions", actionButton("research_add_angle", "Create angle from selected source", class = "lab-primary")),
      uiOutput("research_selected_angle_editor"),
      tags$details(
        class = "research-source-details",
        tags$summary("Edit source details"),
        uiOutput("research_selected_source_editor")
      )
    )
  })

  output$research_selected_source_editor <- renderUI({
    row <- selected_research_source()
    if (nrow(row) == 0) return(div(class = "lab-status-copy", "Select a ranked or unranked source to edit details and create angles."))
    div(
      div(class = "lab-status-copy", sprintf("Editing source %s", row$research_source_id[[1]])),
      div(class = "lab-grid", div(class = "lab-field", textInput("research_edit_source_title", "Source title", value = row$source_title[[1]], width = "100%")), div(class = "lab-field", textInput("research_edit_source_url", "Source URL", value = row$source_url[[1]] %||% "", width = "100%")), div(class = "lab-field", textInput("research_edit_pdf_url", "PDF URL", value = row$pdf_url[[1]] %||% "", width = "100%")), div(class = "lab-field", numericInput("research_edit_source_sort", "Sort order", value = research_numeric_default(row$manual_sort_order[[1]]), width = "100%")), div(class = "lab-field", textInput("research_edit_source_status", "Status", value = row$status[[1]], width = "100%"))),
      div(class = "lab-field", textAreaInput("research_edit_source_main", "Main idea", value = row$main_idea[[1]] %||% "", width = "100%", height = "90px")),
      div(class = "lab-field", textAreaInput("research_edit_source_abstract", "Abstract", value = row$abstract[[1]] %||% "", width = "100%", height = "90px")),
      div(class = "lab-field", textAreaInput("research_edit_source_used_articles", "Article(s) written from this source", value = row$used_articles[[1]] %||% "", width = "100%", height = "70px")),
      div(class = "lab-field", textAreaInput("research_edit_source_notes", "Notes", value = row$notes[[1]] %||% "", width = "100%", height = "80px")),
      div(class = "lab-actions", actionButton("research_save_source", "Save selected source", class = "lab-primary"), actionButton("research_refresh_selected_source", "Refresh", class = "lab-secondary"))
    )
  })

  output$research_angles_table <- DT::renderDT({
    rows <- research_angles()
    display <- if (nrow(rows) == 0) {
      data.frame(research_angle_id = integer(), Sort = integer(), Status = character(), `Angle title` = character(), `Main idea` = character(), `Title Lab batch` = character(), Updated = character(), check.names = FALSE)
    } else {
      data.frame(
        research_angle_id = rows$research_angle_id,
        Sort = rows$manual_sort_order,
        Status = rows$status,
        `Angle title` = vapply(rows$angle_title, research_truncate, character(1), max_chars = 80L),
        `Main idea` = vapply(rows$main_idea, research_truncate, character(1), max_chars = 110L),
        `Title Lab batch` = rows$article_lab_batch_id,
        Updated = rows$updated_at,
        check.names = FALSE
      )
    }
    DT::datatable(display, rownames = FALSE, escape = TRUE, selection = list(mode = "single", target = "row"), options = list(pageLength = 8, scrollX = TRUE, order = list(), columnDefs = list(list(targets = 0, visible = FALSE))))
  })

  output$research_selected_angle_editor <- renderUI({
    source <- selected_research_source()
    if (nrow(source) == 0) return(div(class = "lab-status-copy", "Select a source to view and edit its angles."))
    row <- selected_research_angle()
    if (nrow(row) == 0) return(div(class = "lab-status-copy", "Select an angle from the table to edit it, or create a new angle below."))
    id <- row$research_angle_id[[1]]
    div(
      h3("Selected angle"),
      div(class = "lab-grid", div(class = "lab-field", textInput("research_edit_angle_title", "Angle title", value = row$angle_title[[1]], width = "100%")), div(class = "lab-field", numericInput("research_edit_angle_sort", "Sort order", value = research_numeric_default(row$manual_sort_order[[1]]), width = "100%")), div(class = "lab-field", textInput("research_edit_angle_status", "Status", value = row$status[[1]], width = "100%")), div(class = "lab-field", textInput("research_edit_angle_batch", "Article Lab batch", value = row$article_lab_batch_id[[1]] %||% "", width = "100%"))),
      div(class = "lab-field", textAreaInput("research_edit_angle_main", "Main idea", value = row$main_idea[[1]] %||% "", width = "100%", height = "80px")),
      div(class = "lab-field", textAreaInput("research_edit_angle_notes", "Notes", value = row$notes[[1]] %||% "", width = "100%", height = "70px")),
      div(class = "lab-actions", actionButton("research_save_angle", "Save angle edits", class = "lab-secondary"), tags$button(type = "button", class = "btn btn-default action-button lab-primary", onclick = sprintf("Shiny.setInputValue('research_send_to_title_lab', '%s', {priority: 'event'})", id), "Send to Title Lab"))
    )
  })

  output$article_lab_generate_button <- renderUI({
    if (isTRUE(article_lab_state$is_generating)) {
      tags$button(
        id = "article_lab_generate",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Generating..."
      )
    } else {
      actionButton("article_lab_generate", "Generate titles", class = "lab-primary")
    }
  })

  output$article_lab_batch_selector <- renderUI({
    batches <- article_lab_batches()
    batch_choices <- if (nrow(batches) == 0) character() else setNames(
      batches$batch_id,
      sprintf("%s · %s", batches$batch_id, batches$created_at)
    )
    choices <- c("All batches" = article_lab_all_batches_value, batch_choices)
    div(
      class = "lab-field",
      selectInput(
        "article_lab_selected_batch",
        "Batch selector",
        choices = choices,
        selected = article_lab_all_batches_value,
        width = "100%"
      )
    )
  })

  output$article_lab_score_button <- renderUI({
    if (isTRUE(article_lab_state$is_scoring)) {
      tags$button(
        id = "article_lab_score_titles",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Scoring..."
      )
    } else {
      actionButton("article_lab_score_titles", "Score selected API queue", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_queue');")
    }
  })

  output$article_lab_subtitle_generate_button <- renderUI({
    if (isTRUE(article_lab_state$is_generating_subtitles)) {
      tags$button(
        id = "article_lab_generate_subtitles",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Generating..."
      )
    } else {
      actionButton("article_lab_generate_subtitles", "Generate selected subtitle candidates", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_subtitle_titles');")
    }
  })

  output$article_lab_thumbnail_generate_button <- renderUI({
    if (isTRUE(article_lab_state$is_generating_thumbnails)) {
      tags$button(
        id = "article_lab_generate_thumbnails",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Generating..."
      )
    } else {
      actionButton("article_lab_generate_thumbnails", "Generate selected thumbnail candidates", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_packages');")
    }
  })

  output$article_lab_latest_titles <- renderUI({
    saved <- article_lab_generate_candidates()
    draft <- article_lab_state$draft
    if (!is.null(draft) && nrow(draft) > 0) {
      draft_rows <- data.frame(
        candidate_id = sprintf("draft_%02d", seq_len(nrow(draft))),
        title = draft$title,
        title_char_count = article_lab_title_length(draft$title),
        title_length_flag = article_lab_title_length_flag(article_lab_title_length(draft$title)),
        status = rep("draft", nrow(draft)),
        created_at = rep(article_lab_state$draft_created_at %||% now_utc(), nrow(draft)),
        batch_id = rep("(draft)", nrow(draft)),
        notes = rep(NA_character_, nrow(draft)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      rows <- draft_rows
    } else {
      rows <- saved
    }
    article_lab_generate_table_ui(rows)
  })

  output$article_lab_score_sections <- renderUI({
    queue_rows <- article_lab_queue_rows()
    scored_rows <- article_lab_scored_rows()

    tagList(
      article_lab_section_card(
        "1. API queue (waiting to be scored)",
        "These titles have not been scored yet.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_queue_select_all", "Select all", value = FALSE)
          ),
          article_lab_score_queue_table_ui(queue_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_archive_queue_titles", "Archive selected titles", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_queue');", disabled = nrow(queue_rows) == 0)
          )
        ),
        count = nrow(queue_rows)
      ),
      article_lab_section_card(
        "2. Scored titles awaiting approval",
        "These titles have been scored by the API. Select the ones you want to approve for subtitle generation.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_scored_select_all", "Select all", value = FALSE)
          ),
          article_lab_score_table_ui(scored_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_approve_for_subtitle", "Approve selected for subtitles", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_scored');", disabled = nrow(scored_rows) == 0),
            article_lab_button("article_lab_archive_scored_titles", "Archive selected titles", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_scored');", disabled = nrow(scored_rows) == 0)
          ),
          div(class = "lab-status-copy", "Approved titles will move to Subtitle Generation.")
        ),
        count = nrow(scored_rows)
      )
    )
  })

  output$article_lab_subtitle_sections <- renderUI({
    target_rows <- article_lab_subtitle_target_rows()
    subtitle_rows <- article_lab_pending_subtitle_rows()

    tagList(
      article_lab_section_card(
        "1. Titles awaiting subtitle generation",
        "These approved titles do not have active subtitle candidates yet.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_subtitle_title_select_all", "Select all", value = FALSE)
          ),
          article_lab_subtitle_target_table_ui(target_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_archive_subtitle_titles", "Archive selected titles", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_subtitle_titles');", disabled = nrow(target_rows) == 0)
          )
        ),
        count = nrow(target_rows)
      ),
      article_lab_section_card(
        "2. Subtitle candidates awaiting approval",
        "Select subtitle candidates to approve for Thumbnails or reject without deleting them.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_subtitle_candidate_select_all", "Select all", value = FALSE)
          ),
          article_lab_subtitle_candidate_table_ui(subtitle_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_approve_subtitles", "Approve selected subtitles", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_subtitle_candidates');", disabled = nrow(subtitle_rows) == 0),
            article_lab_button("article_lab_reject_subtitles", "Reject selected", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_subtitle_candidates');", disabled = nrow(subtitle_rows) == 0)
          ),
          div(class = "lab-status-copy", "Approved subtitle candidates stay available as variants for the Thumbnails step.")
        ),
        count = nrow(subtitle_rows)
      )
    )
  })

  output$article_lab_thumbnail_sections <- renderUI({
    package_rows <- article_lab_thumbnail_package_rows()
    thumbnail_rows <- article_lab_pending_thumbnail_rows()

    tagList(
      article_lab_section_card(
        "1. Title/subtitle packages awaiting thumbnail generation",
        "These approved title/subtitle packages do not have active thumbnail candidates yet.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_thumbnail_package_select_all", "Select all", value = FALSE)
          ),
          article_lab_thumbnail_package_table_ui(package_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_dismiss_thumbnail_packages", "Dismiss selected packages", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_packages');", disabled = nrow(package_rows) == 0)
          )
        ),
        count = nrow(package_rows)
      ),
      article_lab_section_card(
        "2. Thumbnail preview cards awaiting approval",
        "Select one preview card per title/subtitle package to approve for Outline, or reject candidates without deleting them.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_thumbnail_candidate_select_all", "Select all", value = FALSE)
          ),
          article_lab_thumbnail_candidate_grid_ui(thumbnail_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_approve_thumbnails", "Approve selected thumbnail", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_candidates');", disabled = nrow(thumbnail_rows) == 0),
            article_lab_button("article_lab_reject_thumbnails", "Reject selected", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_candidates');", disabled = nrow(thumbnail_rows) == 0)
          ),
          div(class = "lab-status-copy", "Only one approved thumbnail is allowed per title/subtitle package. Approved packages move to Outline.")
        ),
        count = nrow(thumbnail_rows)
      )
    )
  })

  output$article_lab_outline_sections <- renderUI({
    outline_rows <- article_lab_ready_for_outline_rows()

    article_lab_section_card(
      "Ready for Outline",
      "Approved title/subtitle/thumbnail packages are available here for the next drafting step.",
      article_lab_ready_for_outline_table_ui(outline_rows),
      count = nrow(outline_rows)
    )
  })

  output$article_lab_full_text_sections <- renderUI({
    rows <- article_lab_full_text_rows()
    packages <- article_lab_full_text_package_rows(rows)
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(packages$batch_id))
    article_lab_section_card(
      "Approved outlines ready for Full Article",
      "Generate one or more full article variants from each approved outline, edit drafts in place, and approve one for Review & Publish.",
      article_lab_full_text_table_ui(rows, packages, summary_contexts, include_context = isTRUE(input$article_lab_full_text_include_context)),
      count = nrow(packages)
    )
  })

  output$article_lab_review_publish_selector <- renderUI({
    rows <- article_lab_review_publish_rows()
    article_lab_review_publish_selector_ui(rows, input$article_lab_review_publish_draft_id)
  })

  output$article_lab_review_publish_workspace <- renderUI({
    article_lab_review_publish_workspace_ui(article_lab_selected_review_publish_row(), article_lab_publication_rows())
  })

  output$article_lab_medium_tags_effective_prompt <- renderUI({
    row <- article_lab_selected_review_publish_row()
    model <- article_lab_input_string(input$article_lab_medium_tags_model) %||% article_lab_default_medium_tags_model
    prompt <- article_lab_input_multiline(input$article_lab_medium_tags_prompt) %||% article_lab_default_medium_tags_prompt
    exact_prompt <- article_lab_medium_tags_effective_prompt(row, prompt)
    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Medium tag generation sends the selected approved article package and asks for JSON tags only."),
      tags$details(
        open = if (nrow(row) > 0) "open" else NULL,
        tags$summary("Show exact Medium tags API prompt"),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", paste(sprintf("Model: %s", model), "Response format: JSON with a tags array, capped to 5 tags on save.", sep = "\n")),
        h4("Exact prompt"),
        tags$pre(class = "lab-status-copy", if (nzchar(exact_prompt)) exact_prompt else "(No approved article selected.)")
      )
    )
  })

  observeEvent(input$article_lab_generate, {
    article_lab_state$is_generating <- TRUE
    on.exit({
      article_lab_state$is_generating <- FALSE
    }, add = TRUE)

    selected_summary <- selected_generate_summary()
    effective_inputs <- article_lab_effective_generation_inputs()
    prompt_value <- effective_inputs$prompt
    manual_prompt_value <- effective_inputs$manual_prompt
    seed_topic_value <- effective_inputs$seed_topic
    inspiration_value <- effective_inputs$inspiration_source

    generated <- generate_title_candidates(
      con = con,
      prompt = prompt_value,
      batch_size = input$article_lab_batch_size,
      seed_topic = seed_topic_value,
      inspiration_source = inspiration_value,
      model = input$article_lab_model,
      manual_prompt = manual_prompt_value
    )
    article_lab_state$draft <- generated$titles
    article_lab_state$draft_created_at <- now_utc()
    article_lab_state$draft_meta <- modifyList(generated, list(
      prompt = prompt_value,
      manual_prompt = manual_prompt_value,
      seed_topic = seed_topic_value,
      inspiration_source = inspiration_value,
      notes_extra = if (nrow(selected_summary) > 0) paste(
        sprintf("Research summary: %s.", selected_summary$summary_id[[1]]),
        sprintf("Research source: %s.", selected_summary$research_source_id[[1]]),
        sprintf("Source title: %s.", selected_summary$source_title[[1]] %||% ""),
        sprintf("Source URL: %s.", selected_summary$source_url[[1]] %||% ""),
        sprintf("PDF URL: %s.", selected_summary$pdf_url[[1]] %||% "")
      ) else NULL
    ))
    if (identical(generated$mode, "api")) {
      example_copy <- if (isTRUE(generated$example_titles_used > 0)) {
        sprintf(" Used %s top-performing title examples as inspiration.", generated$example_titles_used)
      } else {
        ""
      }
      retry_copy <- if (isTRUE(generated$retry_used)) {
        " Strict mode triggered one automatic retry to shorten titles above the hard maximum."
      } else {
        ""
      }
      dropped_copy <- if (isTRUE(generated$dropped_n > 0)) {
        sprintf(" Dropped %s title%s over %s characters after strict validation.", generated$dropped_n, ifelse(generated$dropped_n == 1, "", "s"), article_lab_title_max_chars)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "Generated %s draft titles with the OpenAI API using model %s.%s%s%s Save the batch to persist it to SQLite.",
        nrow(generated$titles),
        generated$model %||% article_lab_default_model,
        example_copy,
        retry_copy,
        dropped_copy
      )
    } else {
      dropped_copy <- if (isTRUE(generated$dropped_n > 0)) {
        sprintf(" Dropped %s title%s over %s characters after strict validation.", generated$dropped_n, ifelse(generated$dropped_n == 1, "", "s"), article_lab_title_max_chars)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "API generation was unavailable, so the local stub helper generated %s draft titles instead.%s Reason: %s",
        nrow(generated$titles),
        dropped_copy,
        generated$fallback_reason %||% "unknown error"
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_add_manual_titles, {
    manual_titles <- article_lab_parse_manual_titles(input$article_lab_manual_titles)
    if (length(manual_titles) == 0) {
      article_lab_state$notice <- "Enter at least one manual title idea, with one title per line."
      return(invisible(NULL))
    }

    existing_titles <- if (!is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
      clean_text(article_lab_state$draft$title)
    } else {
      character()
    }
    new_manual_titles <- setdiff(manual_titles, existing_titles)
    if (length(new_manual_titles) == 0) {
      article_lab_state$notice <- "Those manual titles are already in the current draft."
      updateTextAreaInput(session, "article_lab_manual_titles", value = "")
      return(invisible(NULL))
    }
    combined_titles <- unique(c(existing_titles, manual_titles))
    normalized_titles <- article_lab_normalize_titles(combined_titles)
    if (length(normalized_titles) == 0) {
      article_lab_state$notice <- "No usable manual titles were provided."
      return(invisible(NULL))
    }

    article_lab_state$draft <- data.frame(
      row_number = seq_along(normalized_titles),
      title = normalized_titles,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    article_lab_state$draft_created_at <- article_lab_state$draft_created_at %||% now_utc()

    prior_mode <- article_lab_state$draft_meta$mode %||% NA_character_
    next_mode <- if (is.na(prior_mode) || !nzchar(prior_mode)) {
      "manual"
    } else if (identical(prior_mode, "manual")) {
      "manual"
    } else {
      "mixed"
    }
    article_lab_state$draft_meta <- modifyList(
      article_lab_state$draft_meta %||% list(),
      list(
        mode = next_mode,
        raw_json = article_lab_state$draft_meta$raw_json %||% NA_character_
      )
    )

    added_n <- sum(normalized_titles %in% new_manual_titles)
    over_limit_n <- sum(article_lab_title_length(new_manual_titles) > article_lab_title_mobile_safe_chars, na.rm = TRUE)
    length_copy <- if (over_limit_n > 0) {
      sprintf(" %s title%s exceed the %s-character mobile-safe length and were kept with their length flag.", over_limit_n, ifelse(over_limit_n == 1, "", "s"), article_lab_title_mobile_safe_chars)
    } else {
      ""
    }
    article_lab_state$notice <- sprintf(
      "Added %s manual title idea%s to the current draft.%s Save the batch to persist it to SQLite.",
      added_n,
      ifelse(added_n == 1, "", "s"),
      length_copy
    )
    updateTextAreaInput(session, "article_lab_manual_titles", value = "")
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_prompt, {
    prompt_text <- article_lab_input_multiline(input$article_lab_prompt)
    prompt_key <- article_lab_input_string(input$article_lab_prompt_key) %||% article_lab_manual_prompt_key
    if (is.na(prompt_text)) {
      article_lab_state$notice <- "Enter a prompt before saving."
      return(invisible(NULL))
    }
    save_article_lab_prompt(con, prompt_text, prompt_key)
    saved_article_lab_prompt_key(prompt_key)
    saved_article_lab_prompt(prompt_text)
    updateSelectInput(session, "article_lab_prompt_key_select", choices = list_article_lab_prompt_keys(con), selected = prompt_key)
    updateTextInput(session, "article_lab_prompt_key", value = prompt_key)
    article_lab_state$notice <- sprintf("Saved generation prompt '%s'.", prompt_key)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_outline_prompt, {
    prompt_text <- article_lab_input_multiline(input$article_lab_outline_prompt)
    prompt_key <- article_lab_input_string(input$article_lab_outline_prompt_key) %||% article_lab_outline_prompt_key
    if (is.na(prompt_text)) {
      article_lab_state$notice <- "Enter an outline prompt before saving."
      return(invisible(NULL))
    }
    save_article_lab_prompt(con, prompt_text, prompt_key, article_lab_default_outline_prompt)
    saved_article_lab_outline_prompt_key(prompt_key)
    saved_article_lab_outline_prompt(prompt_text)
    updateSelectInput(session, "article_lab_outline_prompt_key_select", choices = list_article_lab_prompt_keys(con, article_lab_outline_prompt_key), selected = prompt_key)
    updateTextInput(session, "article_lab_outline_prompt_key", value = prompt_key)
    article_lab_state$notice <- sprintf("Saved outline prompt '%s'.", prompt_key)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_full_text_prompt, {
    prompt_text <- article_lab_input_multiline(input$article_lab_full_text_prompt)
    prompt_key <- article_lab_input_string(input$article_lab_full_text_prompt_key) %||% article_lab_full_text_prompt_key
    if (is.na(prompt_text)) {
      article_lab_state$notice <- "Enter a full article prompt before saving."
      return(invisible(NULL))
    }
    save_article_lab_prompt(con, prompt_text, prompt_key, article_lab_default_full_text_prompt)
    saved_article_lab_full_text_prompt_key(prompt_key)
    saved_article_lab_full_text_prompt(prompt_text)
    updateSelectInput(session, "article_lab_full_text_prompt_key_select", choices = list_article_lab_prompt_keys(con, article_lab_full_text_prompt_key), selected = prompt_key)
    updateTextInput(session, "article_lab_full_text_prompt_key", value = prompt_key)
    article_lab_state$notice <- sprintf("Saved full article prompt '%s'.", prompt_key)
  }, ignoreInit = TRUE)

  save_current_article_lab_draft <- function() {
    draft <- article_lab_state$draft
    draft_meta <- article_lab_state$draft_meta %||% list()
    if (is.null(draft) || nrow(draft) == 0) return(NULL)

    batch_id <- save_article_lab_batch(
      con,
      prompt = draft_meta$prompt %||% input$article_lab_prompt,
      seed_topic = draft_meta$seed_topic %||% input$article_lab_seed_topic,
      inspiration_source = draft_meta$inspiration_source %||% input$article_lab_inspiration_source,
      requested_batch_size = input$article_lab_batch_size,
      model = input$article_lab_model,
      titles = draft$title,
      raw_json = if (is.null(draft_meta$raw_json)) NA_character_ else draft_meta$raw_json,
      generation_mode = draft_meta$mode %||% "generated",
      enforce_max_chars = !((draft_meta$mode %||% "") %in% c("manual", "mixed")),
      notes_extra = draft_meta$notes_extra
    )
    saved_mode <- draft_meta$mode %||% "generated"
    article_lab_state$draft <- NULL
    article_lab_state$draft_created_at <- NULL
    article_lab_state$draft_meta <- NULL
    article_lab_refresh(article_lab_refresh() + 1L)

    list(batch_id = batch_id, mode = saved_mode, title_n = nrow(draft))
  }

  observeEvent(input$article_lab_save, {
    saved <- save_current_article_lab_draft()
    if (is.null(saved)) {
      article_lab_state$notice <- "Nothing to save yet. Generate a draft first."
      return(invisible(NULL))
    }

    article_lab_state$notice <- if (saved$mode %in% c("manual", "mixed")) {
      sprintf(
        "Saved batch %s. Candidates start as New and stay in Generate until you manually move selected titles to the API queue. Generation mode: %s. Overlength manual titles were preserved with their length flag.",
        saved$batch_id,
        saved$mode
      )
    } else {
      sprintf(
        "Saved batch %s. Candidates start as New and stay in Generate until you manually move selected titles to the API queue. Generation mode: %s. Max title length enforced: %s characters.",
        saved$batch_id,
        saved$mode,
        article_lab_title_max_chars
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_clear, {
    article_lab_state$draft <- NULL
    article_lab_state$draft_created_at <- NULL
    article_lab_state$draft_meta <- NULL
    article_lab_state$notice <- "Cleared the unsaved draft."
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_triage, {
    if (!is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
      saved <- save_current_article_lab_draft()
      article_lab_state$notice <- sprintf("Saved draft batch %s with %s title%s. You can now edit statuses or notes.", saved$batch_id, saved$title_n, ifelse(saved$title_n == 1, "", "s"))
      return(invisible(NULL))
    }
    rows <- article_lab_generate_candidates()
    payload <- collect_generate_triage_updates(rows)
    if (length(payload$updates) == 0) {
      article_lab_state$notice <- "No saved titles are visible in the current triage view."
      return(invisible(NULL))
    }
    article_lab_save_generate_triage(con, payload$updates)
    article_lab_state$notice <- sprintf("Saved triage updates for %s title%s.", length(payload$updates), ifelse(length(payload$updates) == 1, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_move_to_api_queue, {
    if (!is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
      draft <- article_lab_state$draft
      selected_indexes <- which(vapply(seq_len(nrow(draft)), function(i) {
        isTRUE(input[[article_lab_row_input_id("article_lab_generate_select", sprintf("draft_%02d", i))]])
      }, logical(1)))
      if (length(selected_indexes) == 0) {
        article_lab_state$notice <- "Select at least one draft title before moving it to the API queue."
        return(invisible(NULL))
      }
      saved <- save_current_article_lab_draft()
      selected_ids <- article_lab_candidate_id(saved$batch_id, selected_indexes)
      result <- article_lab_move_candidates_to_api_queue(con, selected_ids)
      article_lab_state$notice <- sprintf(
        "Saved draft batch %s and moved %s selected title%s to API queue. %s selected title%s were skipped because they were not eligible.",
        saved$batch_id,
        result$moved_n,
        ifelse(result$moved_n == 1, "", "s"),
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s")
      )
      article_lab_refresh(article_lab_refresh() + 1L)
      if (result$moved_n > 0) {
        updateSelectInput(session, "article_lab_selected_batch", selected = saved$batch_id)
        active_section("api_scoring")
      }
      return(invisible(NULL))
    }
    rows <- article_lab_generate_candidates()
    payload <- collect_generate_triage_updates(rows)
    article_lab_save_generate_triage(con, payload$updates)
    snapshot_selected_ids <- collect_selected_ids(
      rows,
      "article_lab_generate_select",
      snapshot_ids = input$article_lab_generate_selected_snapshot
    )
    selected_ids <- unique(c(payload$selected_ids, snapshot_selected_ids))
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one New title before moving it to the API queue."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_move_candidates_to_api_queue(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Moved %s selected title%s to API queue. %s selected title%s were skipped because they were disqualified or not eligible.",
      result$moved_n,
      ifelse(result$moved_n == 1, "", "s"),
      result$skipped_n,
      ifelse(result$skipped_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
    if (result$moved_n > 0) {
      if (length(result$batch_ids) == 1 && nzchar(result$batch_ids[[1]])) {
        updateSelectInput(session, "article_lab_selected_batch", selected = result$batch_ids[[1]])
      }
      active_section("api_scoring")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_score_titles, {
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) {
      article_lab_state$notice <- "Select a saved batch before scoring."
      return(invisible(NULL))
    }
    article_lab_state$is_scoring <- TRUE
    on.exit({
      article_lab_state$is_scoring <- FALSE
    }, add = TRUE)

    queue_rows <- article_lab_queue_rows()
    selected_ids <- collect_selected_ids(
      queue_rows,
      "article_lab_queue_select",
      snapshot_ids = input$article_lab_queue_selected_snapshot
    )
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(queue_rows, "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_scored_rows(), "article_lab_scored_notes"))
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-queue title before scoring."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    result <- tryCatch(
      article_lab_score_batch(
        con,
        batch_id = batch_id,
        model = input$article_lab_score_model,
        prompt_version = input$article_lab_score_prompt_version,
        scope = input$article_lab_score_scope,
        candidate_ids = selected_ids
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste("API scoring failed:", conditionMessage(result))
    } else {
      article_lab_state$notice <- if (result$scored_n > 0) {
        sprintf(
          "Scored %s selected API-queue title%s for %s using model %s, prompt %s, scope %s.%s%s",
          result$scored_n,
          ifelse(result$scored_n == 1, "", "s"),
          result$batch_label %||% paste("batch", batch_id),
          result$model %||% article_lab_default_score_model,
          result$prompt_version %||% article_lab_default_score_prompt_version,
          result$scope %||% article_lab_default_score_scope,
          if (result$used_existing_n > 0) sprintf(" %s used an existing saved API score.", result$used_existing_n) else "",
          if (result$failed_n > 0) sprintf(" %s failed and stayed in their previous status.", result$failed_n) else ""
        )
      } else {
        result$message %||% "No titles are currently waiting in the API queue for this selection."
      }
      article_lab_refresh(article_lab_refresh() + 1L)
      if (!is_dimension_mode && !is.null(rating_session_id) && !is.na(rating_session_id)) {
        prune_article_lab_candidates_from_session(con, rating_session_id)
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_for_subtitle, {
    scored_rows <- article_lab_scored_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_queue_rows(), "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(scored_rows, "article_lab_scored_notes"))
    selected_ids <- collect_selected_ids(
      scored_rows,
      "article_lab_scored_select",
      snapshot_ids = input$article_lab_scored_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-scored title before approving it for subtitle generation."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_candidates_for_subtitle(con, selected_ids)
    article_lab_state$notice <- if (result$approved_n > 0 || result$skipped_n == 0) {
      sprintf("Approved %s selected title%s for subtitle generation.", result$approved_n, ifelse(result$approved_n == 1, "", "s"))
    } else {
      sprintf(
        "Approved %s selected title%s for subtitle generation. %s selected title%s were skipped because they were not API scored.",
        result$approved_n,
        ifelse(result$approved_n == 1, "", "s"),
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s")
      )
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_queue_titles, {
    queue_rows <- article_lab_queue_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(queue_rows, "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_scored_rows(), "article_lab_scored_notes"))
    selected_ids <- collect_selected_ids(
      queue_rows,
      "article_lab_queue_select",
      snapshot_ids = input$article_lab_queue_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-queue title before archiving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_archive_api_scored_candidates(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Archived %s selected API-queue title%s. No rows were deleted.",
      result$archived_n,
      ifelse(result$archived_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_scored_titles, {
    scored_rows <- article_lab_scored_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_queue_rows(), "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(scored_rows, "article_lab_scored_notes"))
    selected_ids <- collect_selected_ids(
      scored_rows,
      "article_lab_scored_select",
      snapshot_ids = input$article_lab_scored_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-scored title before archiving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_archive_api_scored_candidates(con, selected_ids)
    article_lab_state$notice <- if (result$archived_n > 0 || result$skipped_n == 0) {
      sprintf("Archived %s selected title%s.", result$archived_n, ifelse(result$archived_n == 1, "", "s"))
    } else {
      sprintf(
        "Archived %s selected title%s. %s selected title%s were skipped because they were not API scored.",
        result$archived_n,
        ifelse(result$archived_n == 1, "", "s"),
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s")
      )
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_subtitle_titles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      target_rows,
      "article_lab_subtitle_title_select",
      snapshot_ids = input$article_lab_subtitle_titles_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one subtitle-stage title before archiving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_archive_api_scored_candidates(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Archived %s selected subtitle-stage title%s. No rows were deleted.",
      result$archived_n,
      ifelse(result$archived_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_subtitles, {
    article_lab_state$is_generating_subtitles <- TRUE
    on.exit({
      article_lab_state$is_generating_subtitles <- FALSE
    }, add = TRUE)

    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      target_rows,
      "article_lab_subtitle_title_select",
      snapshot_ids = input$article_lab_subtitle_titles_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one approved title before generating subtitle candidates."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    result <- tryCatch(
      article_lab_generate_subtitles_for_titles(
        con,
        candidate_ids = selected_ids,
        model = input$article_lab_subtitle_model,
        prompt = input$article_lab_subtitle_prompt,
        variants_per_title = input$article_lab_subtitle_variants_per_title
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste("Subtitle generation failed:", conditionMessage(result))
    } else {
      fallback_copy <- if (!is.null(result$fallback_reason) && nzchar(result$fallback_reason)) {
        sprintf(" Stub fallback was used because: %s", result$fallback_reason)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "Generated %s subtitle candidate%s for %s selected title%s using model %s.%s %s selected title%s were skipped because they were not eligible or already had active subtitle candidates.%s",
        result$generated_n,
        ifelse(result$generated_n == 1, "", "s"),
        result$title_n,
        ifelse(result$title_n == 1, "", "s"),
        result$model %||% article_lab_default_subtitle_model,
        if (identical(result$mode, "stub")) " The stub helper was used." else "",
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s"),
        fallback_copy
      )
      article_lab_refresh(article_lab_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_add_manual_subtitles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))

    candidate_id <- article_lab_input_string(input$article_lab_manual_subtitle_candidate_id)
    subtitle_text <- input$article_lab_manual_subtitle_text %||% ""
    proposed_subtitles <- article_lab_normalize_subtitle(unlist(strsplit(subtitle_text, "\n", fixed = TRUE)))
    if (is.na(candidate_id) || !nzchar(candidate_id)) {
      article_lab_state$notice <- "Choose a title before adding manual subtitle ideas."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    if (length(proposed_subtitles) == 0) {
      article_lab_state$notice <- sprintf("Enter at least one manual subtitle idea under %s characters.", article_lab_subtitle_max_chars)
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    result <- article_lab_add_manual_subtitles(con, candidate_id, proposed_subtitles)
    if (result$added_n > 0) {
      duplicate_copy <- if (isTRUE(result$duplicate_n > 0)) {
        sprintf(" %s duplicate idea%s were skipped.", result$duplicate_n, ifelse(result$duplicate_n == 1, "", "s"))
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "Added %s manual subtitle idea%s for \"%s\".%s",
        result$added_n,
        ifelse(result$added_n == 1, "", "s"),
        result$title %||% "the selected title",
        duplicate_copy
      )
      updateTextAreaInput(session, "article_lab_manual_subtitle_text", value = "")
    } else if (isTRUE(result$duplicate_n > 0)) {
      article_lab_state$notice <- sprintf(
        "All entered subtitle ideas for \"%s\" already exist in this title's subtitle list.",
        result$title %||% "the selected title"
      )
    } else {
      article_lab_state$notice <- "The selected title is not currently eligible for manual subtitle ideas in this stage."
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_subtitles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_subtitle_candidate_select",
      snapshot_ids = input$article_lab_subtitle_candidates_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one subtitle candidate before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_subtitles(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Approved %s selected subtitle candidate%s. %s title package%s are now ready for Thumbnails.",
      result$approved_n,
      ifelse(result$approved_n == 1, "", "s"),
      length(unique(result$candidate_ids)),
      ifelse(length(unique(result$candidate_ids)) == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_reject_subtitles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_subtitle_candidate_select",
      snapshot_ids = input$article_lab_subtitle_candidates_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one subtitle candidate before rejecting it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_reject_subtitles(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Rejected %s selected subtitle candidate%s. No rows were deleted.",
      result$rejected_n,
      ifelse(result$rejected_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_thumbnails, {
    article_lab_state$is_generating_thumbnails <- TRUE
    on.exit({
      article_lab_state$is_generating_thumbnails <- FALSE
      article_lab_state$thumbnail_generation_started_at <- NULL
      article_lab_state$thumbnail_generation_estimate <- NULL
    }, add = TRUE)

    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      package_rows,
      "article_lab_thumbnail_package_select",
      snapshot_ids = input$article_lab_thumbnail_packages_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one ready title/subtitle package before generating thumbnail candidates."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    variants_per_package <- max(1L, min(4L, suppressWarnings(as.integer(input$article_lab_thumbnail_variants_per_package)) %||% article_lab_default_thumbnail_variants))
    estimate <- article_lab_thumbnail_estimate(length(selected_ids) * variants_per_package)
    started_at <- Sys.time()
    article_lab_state$thumbnail_generation_started_at <- started_at
    article_lab_state$thumbnail_generation_estimate <- estimate
    article_lab_state$notice <- sprintf(
      "Generating thumbnails: requested %s thumbnail%s for %s selected package%s. Initial estimate: %s. Waiting for OpenAI; live completed/remaining progress is not available during this blocking call.",
      estimate$total_expected,
      ifelse(estimate$total_expected == 1L, "", "s"),
      length(selected_ids),
      ifelse(length(selected_ids) == 1L, "", "s"),
      estimate$label
    )
    session$sendCustomMessage(
      "articleLabStartThumbnailTimer",
      list(
        total_expected = estimate$total_expected,
        estimate_label = estimate$label,
        lower_seconds = estimate$lower_seconds,
        upper_seconds = estimate$upper_seconds,
        started_at = paste0(format(as.POSIXct(started_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%OS3", tz = "UTC"), "Z")
      )
    )
    if (is.function(session$flushReact)) session$flushReact()

    result <- tryCatch(
      article_lab_generate_thumbnails_for_packages(
        con,
        subtitle_ids = selected_ids,
        model = input$article_lab_thumbnail_model,
        prompt = input$article_lab_thumbnail_prompt,
        variants_per_package = variants_per_package
      ),
      error = function(e) e
    )
    actual_seconds <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
    comparison <- article_lab_estimate_comparison(actual_seconds, estimate$lower_seconds, estimate$upper_seconds)
    timing_copy <- sprintf(
      "Thumbnail generation finished in %s. Initial estimate was %s, so this run was %s.",
      article_lab_format_duration(actual_seconds),
      estimate$label,
      comparison
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste(timing_copy, "Thumbnail generation failed:", conditionMessage(result))
      session$sendCustomMessage("articleLabStopThumbnailTimer", list(message = article_lab_state$notice))
    } else {
      mode_label <- result$mode %||% "unknown"
      fallback_count <- if (identical(mode_label, "stub")) result$generated_n else 0L
      failure_count <- 0L
      fallback_copy <- if (identical(mode_label, "stub") && !is.null(result$fallback_reason) && nzchar(result$fallback_reason)) {
        sprintf(" Stub fallback reason: %s", result$fallback_reason)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "%s Generated %s thumbnail candidate%s for %s selected package%s using model %s in %s mode. Fallback count: %s. Failure count: %s. %s selected package%s were skipped because they were not eligible or already had active thumbnail candidates.%s",
        timing_copy,
        result$generated_n,
        ifelse(result$generated_n == 1, "", "s"),
        result$package_n,
        ifelse(result$package_n == 1, "", "s"),
        result$model %||% article_lab_default_thumbnail_model,
        mode_label,
        fallback_count,
        failure_count,
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s"),
        fallback_copy
      )
      session$sendCustomMessage("articleLabStopThumbnailTimer", list(message = article_lab_state$notice))
      article_lab_refresh(article_lab_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_dismiss_thumbnail_packages, {
    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      package_rows,
      "article_lab_thumbnail_package_select",
      snapshot_ids = input$article_lab_thumbnail_packages_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one ready title/subtitle package before dismissing it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_dismiss_thumbnail_packages(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Dismissed %s selected title/subtitle package%s. No rows were deleted.",
      result$dismissed_n,
      ifelse(result$dismissed_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_thumbnails, {
    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_thumbnail_candidate_select",
      snapshot_ids = input$article_lab_thumbnail_candidates_selected_snapshot,
      key_col = "thumbnail_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one thumbnail preview card before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    selected_rows <- pending_rows[pending_rows$thumbnail_id %in% selected_ids, , drop = FALSE]
    duplicate_subtitle_ids <- names(table(selected_rows$subtitle_id)[table(selected_rows$subtitle_id) > 1L])
    if (length(duplicate_subtitle_ids) > 0) {
      article_lab_state$notice <- "Select only one thumbnail candidate per title/subtitle package before approving."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_thumbnails(con, selected_ids)
    if (!is.null(result$message) && nzchar(result$message)) {
      article_lab_state$notice <- result$message
    } else {
      article_lab_state$notice <- sprintf(
        "Approved %s selected thumbnail%s. %s package%s are now ready for Outline.",
        result$approved_n,
        ifelse(result$approved_n == 1, "", "s"),
        length(unique(result$subtitle_ids)),
        ifelse(length(unique(result$subtitle_ids)) == 1, "", "s")
      )
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_reject_thumbnails, {
    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_thumbnail_candidate_select",
      snapshot_ids = input$article_lab_thumbnail_candidates_selected_snapshot,
      key_col = "thumbnail_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one thumbnail preview card before rejecting it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_reject_thumbnails(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Rejected %s selected thumbnail candidate%s. No rows were deleted.",
      result$rejected_n,
      ifelse(result$rejected_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_outlines, {
    started_at <- Sys.time()
    article_lab_debug_log("outline_generate_clicked", list(model = input$article_lab_outline_model, include_context = isTRUE(input$article_lab_outline_include_context)))
    article_lab_state$notice <- "Generating selected outline draft(s). Waiting for OpenAI; this can take a while."
    article_lab_refresh(article_lab_refresh() + 1L)
    if (is.function(session$flushReact)) session$flushReact()

    result <- tryCatch({
      outline_rows <- article_lab_ready_for_outline_rows()
      article_lab_debug_log("outline_generate_rows_loaded", list(ready_rows = nrow(outline_rows)))
      saved_edits_n <- article_lab_update_outlines(con, collect_outline_updates(outline_rows))
      selected_ids <- collect_selected_ids(
        outline_rows,
        "article_lab_outline_packages",
        snapshot_ids = input$article_lab_outline_packages_selected_snapshot,
        key_col = "thumbnail_id"
      )
      article_lab_debug_log("outline_generate_selection", list(saved_edits_n = saved_edits_n, selected_n = length(selected_ids), selected_ids = selected_ids))
      if (length(selected_ids) == 0) {
        list(ok = FALSE, notice = "Select at least one package before generating or regenerating an outline.")
      } else {
        selected_rows <- outline_rows[outline_rows$thumbnail_id %in% selected_ids, , drop = FALSE]
        if (nrow(selected_rows) == 0) {
          list(ok = FALSE, notice = "Selected packages are no longer available for outline generation.")
        } else {
          summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(selected_rows$batch_id))
          selected_rows$article_summary <- NA_character_
          selected_rows$pdf_local_path <- NA_character_
          if (nrow(summary_contexts) > 0) {
            matched_summary <- match(selected_rows$batch_id, summary_contexts$batch_id)
            selected_rows$article_summary <- summary_contexts$article_summary[matched_summary]
            selected_rows$pdf_local_path <- summary_contexts$pdf_local_path[matched_summary]
          }
          article_lab_debug_log("outline_generate_context_loaded", list(
            selected_rows = nrow(selected_rows),
            summary_context_rows = nrow(summary_contexts),
            pdf_context_n = sum(!is.na(selected_rows$pdf_local_path) & nzchar(selected_rows$pdf_local_path)),
            summary_context_n = sum(!is.na(selected_rows$article_summary) & nzchar(selected_rows$article_summary))
          ))
          generated <- generate_outline_drafts(
            selected_rows,
            model = input$article_lab_outline_model,
            prompt = input$article_lab_outline_prompt,
            include_context = isTRUE(input$article_lab_outline_include_context)
          )
          article_lab_debug_log("outline_generate_drafts_returned", list(
            mode = generated$mode %||% "unknown",
            model = generated$model %||% article_lab_default_outline_model,
            generated_rows = nrow(generated$rows),
            fallback_reason = generated$fallback_reason %||% NA_character_
          ))
          if (identical(generated$mode, "failed")) {
            list(
              ok = FALSE,
              notice = paste("Outline API call failed. No generic stub outline was saved.", generated$fallback_reason %||% "See .local_gitignored/article_lab_debug.log for details.")
            )
          } else if (nrow(generated$rows) == 0) {
            list(
              ok = FALSE,
              notice = "Outline API call returned no usable outline rows. No generic stub outline was saved. See .local_gitignored/article_lab_debug.log for details."
            )
          } else {
          inserted_n <- article_lab_insert_outline_drafts(con, generated$rows)
          article_lab_debug_log("outline_generate_inserted", list(inserted_n = inserted_n))
          list(
            ok = TRUE,
            notice = sprintf(
              "Generated %s outline draft%s using model %s in %s mode.",
              inserted_n,
              ifelse(inserted_n == 1L, "", "s"),
              generated$model %||% article_lab_default_outline_model,
              generated$mode %||% "unknown"
            )
          )
          }
        }
      }
    }, error = function(e) {
      article_lab_debug_log("outline_generate_error", list(
        message = conditionMessage(e),
        call = paste(deparse(conditionCall(e)), collapse = " "),
        elapsed_seconds = as.numeric(difftime(Sys.time(), started_at, units = "secs"))
      ))
      list(ok = FALSE, notice = paste("Outline generation failed:", conditionMessage(e), "Debug log: .local_gitignored/article_lab_debug.log"))
    })

    article_lab_state$notice <- result$notice
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_outlines, {
    updated_n <- article_lab_update_outlines(con, collect_outline_updates(article_lab_ready_for_outline_rows()))
    article_lab_state$notice <- sprintf("Saved %s editable outline%s.", updated_n, ifelse(updated_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_outlines, {
    outline_rows <- article_lab_ready_for_outline_rows()
    article_lab_update_outlines(con, collect_outline_updates(outline_rows))
    draft_rows <- outline_rows[!is.na(outline_rows$outline_id) & nzchar(outline_rows$outline_id) & outline_rows$outline_status == "draft", , drop = FALSE]
    selected_ids <- collect_selected_ids(
      draft_rows,
      "article_lab_outline_candidates",
      snapshot_ids = input$article_lab_outline_candidates_selected_snapshot,
      key_col = "outline_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one outline draft before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_outlines(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Approved %s outline%s. %s package%s moved to draft-ready.",
      result$approved_n,
      ifelse(result$approved_n == 1L, "", "s"),
      length(result$candidate_ids),
      ifelse(length(result$candidate_ids) == 1L, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_outlines, {
    updated_n <- article_lab_update_outlines(con, collect_outline_updates(article_lab_ready_for_outline_rows()))
    article_lab_state$notice <- sprintf("Refreshed Outline and saved %s editable outline%s.", updated_n, ifelse(updated_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  generate_selected_full_text <- function(variant = FALSE, selected_rows = NULL) {
    started_at <- Sys.time()
    if (is.null(selected_rows)) {
      rows <- article_lab_full_text_rows()
      packages <- article_lab_full_text_package_rows(rows)
      selected_ids <- collect_selected_ids(packages, "article_lab_full_text_packages", snapshot_ids = input$article_lab_full_text_packages_selected_snapshot, key_col = "outline_id")
      if (length(selected_ids) == 0) {
        article_lab_state$notice <- "Select one approved outline before generating a full article draft."
        article_lab_refresh(article_lab_refresh() + 1L)
        return(invisible(NULL))
      }
      if (length(selected_ids) > 1) selected_ids <- selected_ids[[1]]
      selected_rows <- packages[packages$outline_id %in% selected_ids, , drop = FALSE]
      if (nrow(selected_rows) == 0) {
        article_lab_state$notice <- "Selected outlines are no longer available for full article generation."
        article_lab_refresh(article_lab_refresh() + 1L)
        return(invisible(NULL))
      }
    }
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(selected_rows$batch_id))
    selected_rows$article_summary <- NA_character_
    selected_rows$pdf_local_path <- NA_character_
    if (nrow(summary_contexts) > 0) {
      matched_summary <- match(selected_rows$batch_id, summary_contexts$batch_id)
      selected_rows$article_summary <- summary_contexts$article_summary[matched_summary]
      selected_rows$pdf_local_path <- summary_contexts$pdf_local_path[matched_summary]
    }
    article_lab_state$notice <- if (variant) "Generating another full article variant. Waiting for OpenAI." else "Generating full article draft. Waiting for OpenAI."
    article_lab_refresh(article_lab_refresh() + 1L)
    if (is.function(session$flushReact)) session$flushReact()
    generated <- generate_full_text_drafts(
      selected_rows,
      model = input$article_lab_full_text_model,
      prompt = input$article_lab_full_text_prompt,
      prompt_key = input$article_lab_full_text_prompt_key,
      include_context = isTRUE(input$article_lab_full_text_include_context)
    )
    if (identical(generated$mode, "failed")) {
      article_lab_state$notice <- paste("Full article API call failed. No draft was saved.", generated$fallback_reason %||% "See .local_gitignored/article_lab_debug.log for details.")
    } else if (nrow(generated$rows) == 0) {
      article_lab_state$notice <- "Full article API call returned no usable draft rows. No draft was saved."
    } else {
      inserted_n <- article_lab_insert_full_text_drafts(con, generated$rows, prompt_key = input$article_lab_full_text_prompt_key, prompt_version = input$article_lab_full_text_prompt_key)
      article_lab_state$notice <- sprintf(
        "Generated %s full article draft%s using model %s in %s mode in %s.",
        inserted_n,
        ifelse(inserted_n == 1L, "", "s"),
        generated$model %||% article_lab_default_full_text_model,
        generated$mode %||% "unknown",
        article_lab_format_duration(as.numeric(difftime(Sys.time(), started_at, units = "secs")))
      )
    }
    article_lab_refresh(article_lab_refresh() + 1L)
    invisible(NULL)
  }

  observeEvent(input$article_lab_generate_full_text, {
    generate_selected_full_text(variant = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_full_text_variant, {
    generate_selected_full_text(variant = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_regenerate_full_text_draft, {
    rows <- article_lab_full_text_rows()
    draft_rows <- rows[!is.na(rows$full_text_draft_id) & nzchar(rows$full_text_draft_id) & rows$draft_status == "draft", , drop = FALSE]
    selected_ids <- collect_selected_ids(draft_rows, "article_lab_full_text_drafts", snapshot_ids = input$article_lab_full_text_drafts_selected_snapshot, key_col = "full_text_draft_id")
    if (length(selected_ids) != 1L) {
      article_lab_state$notice <- "Select exactly one unapproved full article draft before regenerating it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    selected_draft <- draft_rows[draft_rows$full_text_draft_id %in% selected_ids[[1]], , drop = FALSE]
    package_rows <- article_lab_full_text_package_rows(rows)
    selected_package <- package_rows[package_rows$outline_id %in% selected_draft$outline_id[[1]], , drop = FALSE]
    if (nrow(selected_package) == 0) {
      article_lab_state$notice <- "The selected draft's outline is no longer available for regeneration."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    generate_selected_full_text(variant = TRUE, selected_rows = selected_package)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_full_text_drafts, {
    updated_n <- article_lab_update_full_text_drafts(con, collect_full_text_updates(article_lab_full_text_rows()))
    article_lab_state$notice <- sprintf("Saved %s full article draft%s and recorded revision rows for changed text.", updated_n, ifelse(updated_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_full_text_draft, {
    rows <- article_lab_full_text_rows()
    article_lab_update_full_text_drafts(con, collect_full_text_updates(rows))
    draft_rows <- rows[!is.na(rows$full_text_draft_id) & nzchar(rows$full_text_draft_id) & rows$draft_status == "draft", , drop = FALSE]
    selected_ids <- collect_selected_ids(draft_rows, "article_lab_full_text_drafts", snapshot_ids = input$article_lab_full_text_drafts_selected_snapshot, key_col = "full_text_draft_id")
    if (length(selected_ids) != 1L) {
      article_lab_state$notice <- "Select exactly one draft before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_full_text_draft(con, selected_ids[[1]])
    article_lab_state$notice <- sprintf("Approved %s full article draft. Package moved to Review & Publish.", result$approved_n)
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_reject_full_text_draft, {
    rows <- article_lab_full_text_rows()
    draft_rows <- rows[!is.na(rows$full_text_draft_id) & nzchar(rows$full_text_draft_id) & rows$draft_status == "draft", , drop = FALSE]
    selected_ids <- collect_selected_ids(draft_rows, "article_lab_full_text_drafts", snapshot_ids = input$article_lab_full_text_drafts_selected_snapshot, key_col = "full_text_draft_id")
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one unapproved draft before rejecting it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    rejected_n <- sum(vapply(selected_ids, function(id) article_lab_reject_full_text_draft(con, id), numeric(1)), na.rm = TRUE)
    article_lab_state$notice <- sprintf("Rejected %s full article draft%s. No rows were deleted.", rejected_n, ifelse(rejected_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_full_text, {
    updated_n <- article_lab_update_full_text_drafts(con, collect_full_text_updates(article_lab_full_text_rows()))
    article_lab_state$notice <- sprintf("Refreshed Full Article and saved %s editable draft%s.", updated_n, ifelse(updated_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  article_lab_current_publish_values <- reactive({
    list(
      medium_tags = input$article_lab_publish_medium_tags %||% "",
      publishing_target = input$article_lab_publishing_target %||% "Do not publish yet",
      publication_id = input$article_lab_publication_id %||% "",
      new_publication_name = input$article_lab_new_publication_name %||% "",
      monetization = input$article_lab_monetization %||% "Undecided",
      canonical_url = input$article_lab_canonical_url %||% "",
      featured_image_alt_text = input$article_lab_featured_image_alt_text %||% "",
      image_credit_source = input$article_lab_image_credit_source %||% "",
      published_url = input$article_lab_published_url %||% "",
      publish_status = input$article_lab_publish_status %||% "ready_for_review_publish",
      notes = input$article_lab_publish_notes %||% ""
    )
  })

  observeEvent(input$article_lab_save_publish_settings, {
    row <- article_lab_selected_review_publish_row()
    if (nrow(row) == 0) {
      article_lab_state$notice <- "Select an approved full article draft before saving publish settings."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    tags <- article_lab_parse_medium_tags(input$article_lab_publish_medium_tags %||% "")
    saved_n <- article_lab_save_publish_settings(con, row, article_lab_current_publish_values())
    tag_note <- if (length(tags) >= 5L) " Medium tags were capped at 5." else ""
    article_lab_state$notice <- sprintf("Saved publish settings for %s approved draft.%s", saved_n, tag_note)
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_medium_tags, {
    row <- article_lab_selected_review_publish_row()
    if (nrow(row) == 0) {
      article_lab_state$notice <- "Select an approved full article draft before generating Medium tags."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- tryCatch(
      article_lab_medium_tags_api_request(
        row,
        model = input$article_lab_medium_tags_model %||% article_lab_default_medium_tags_model,
        prompt = input$article_lab_medium_tags_prompt %||% article_lab_default_medium_tags_prompt
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(result$error)) {
      article_lab_state$notice <- paste("Medium tag generation failed:", result$error)
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    updateTextInput(session, "article_lab_publish_medium_tags", value = paste(result$tags, collapse = ", "))
    article_lab_state$notice <- sprintf("Generated %s Medium tag%s with %s. Review and save publish settings to persist them.", length(result$tags), ifelse(length(result$tags) == 1L, "", "s"), result$model %||% article_lab_default_medium_tags_model)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_publish, {
    article_lab_state$notice <- "Refreshed Review & Publish."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  output$article_lab_export_markdown <- downloadHandler(
    filename = function() {
      row <- article_lab_selected_review_publish_row()
      title <- if (nrow(row) > 0) article_lab_row_value(row, "title", "approved_article") else "approved_article"
      slug <- tolower(gsub("[^A-Za-z0-9]+", "-", title))
      slug <- gsub("(^-+|-+$)", "", slug)
      if (!nzchar(slug)) slug <- "approved-article"
      paste0(slug, "-", format(Sys.Date(), "%Y-%m-%d"), ".md")
    },
    content = function(file) {
      row <- article_lab_selected_review_publish_row()
      text <- article_lab_medium_ready_markdown(row, row)
      writeLines(text, file, useBytes = TRUE)
    }
  )

  observeEvent(input$article_lab_refresh_scores, {
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_queue_rows(), "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_scored_rows(), "article_lab_scored_notes"))
    article_lab_state$notice <- "Refreshed API Scoring and saved visible notes."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_subtitles, {
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_subtitle_target_rows(), "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(article_lab_pending_subtitle_rows(), "article_lab_subtitle_candidate_notes"))
    article_lab_state$notice <- "Refreshed Subtitle Generation and saved visible notes."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_thumbnails, {
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(article_lab_thumbnail_package_rows(), "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(article_lab_pending_thumbnail_rows(), "article_lab_thumbnail_candidate_notes"))
    article_lab_state$notice <- "Refreshed Thumbnails and saved visible notes."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_open_docs, {
    showModal(modalDialog(
      title = "Article Lab workflow docs",
      p("Source-of-truth docs for this workflow:"),
      tags$ul(
        tags$li("data/analysis/article_lab/2026-05-23_title_lab_scoring_and_workflow_summary.md"),
        tags$li("data/analysis/article_lab/2026-05-23_human_score_and_api_human_combination_notes.md")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)

  output$guide_content <- renderUI({
    if (article_lab_is_workflow_section(active_section()) || identical(active_section(), "settings")) {
      current_section <- active_section()
      overview <- article_lab_overview_stats()
      batches <- article_lab_batches()
      latest_saved_batch <- article_lab_saved_batch()
      selected_batch_id <- article_lab_selected_batch_id()
      selected_candidates <- article_lab_selected_batch_candidates()
      selected_batch <- if (nrow(batches) > 0 && !is.na(selected_batch_id) && nzchar(selected_batch_id)) {
        batches[batches$batch_id == selected_batch_id, , drop = FALSE]
      } else {
        data.frame()
      }
      current_batch_label <- if (identical(current_section, "generate") && !is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
        sprintf("Unsaved draft with %s titles", nrow(article_lab_state$draft))
      } else if (identical(current_section, "generate") && !is.null(latest_saved_batch) && nrow(latest_saved_batch) > 0) {
        sprintf("Latest saved batch %s", latest_saved_batch$batch_id[[1]])
      } else if (identical(selected_batch_id, article_lab_all_batches_value)) {
        "All saved titles across batches"
      } else if (nrow(selected_batch) > 0) {
        sprintf("Saved batch %s", selected_batch$batch_id[[1]])
      } else {
        "No batch saved yet"
      }
      current_batch_meta <- if (identical(current_section, "generate") && !is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
        paste("Draft created at", article_lab_state$draft_created_at %||% now_utc())
      } else if (identical(current_section, "generate") && !is.null(latest_saved_batch) && nrow(latest_saved_batch) > 0) {
        paste("Created", latest_saved_batch$created_at[[1]], "\u00b7 model", first_value(latest_saved_batch, "model", article_lab_default_model))
      } else if (identical(selected_batch_id, article_lab_all_batches_value)) {
        "The current selection spans all saved batches."
      } else if (nrow(selected_batch) > 0) {
        paste("Created", selected_batch$created_at[[1]], "\u00b7 model", first_value(selected_batch, "model", article_lab_default_model))
      } else {
        "Generate first, then save to persist candidates."
      }
      ready_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "ready_for_api_scoring", na.rm = TRUE)
      scored_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "api_scored", na.rm = TRUE)
      approved_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "approved_for_subtitle", na.rm = TRUE)
      subtitle_ready_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "ready_for_thumbnail", na.rm = TRUE)

      if (current_section %in% c("research_inbox", "api_scoring", "subtitle_generation", "thumbnails")) {
        return(NULL)
      }

      return(tagList(
        div(
          class = "status-card",
          h3("Article Lab status"),
          div(class = "status-metric", overview$saved_candidates[[1]]),
          p(sprintf("%s saved candidates across %s batches.", overview$saved_candidates[[1]], overview$saved_batches[[1]])),
          p(class = "lab-status-copy", sprintf("%s remain New, %s are in API queue, %s are approved for subtitles, %s are ready for Thumbnails, and %s are ready for Outline.", overview$generated[[1]], overview$ready_for_api_scoring[[1]], overview$approved_for_subtitle[[1]], overview$ready_for_thumbnail[[1]], overview$ready_for_outline[[1]]))
        ),
        div(
          class = "status-card",
          h3("Current selection"),
          p(current_batch_label),
          p(class = "lab-status-copy", current_batch_meta)
        ),
        div(
          class = "status-card",
          h3("Reminder"),
          p("Home remains the separate rating workflow."),
          p(class = "lab-status-copy", sprintf("This pass now covers Generate, API Scoring, Subtitle Generation, and Thumbnails. %s title%s are ready for Thumbnails in the current selection.", subtitle_ready_n, ifelse(subtitle_ready_n == 1, "", "s")))
        )
      ))
    }

    if (is_dimension_mode) {
      field <- active_dimension()
      if (is.na(field)) {
        return(tagList(
          div(class = "guide-section", h3("Dimension pass"), p("All dimension passes are complete.")),
          div(class = "tip", h3("Reminder"), p("No outcome, API, or prior human score data is shown during rating."))
        ))
      }
      return(tagList(
        div(class = "guide-section", h3("Active dimension"), p(dimension_labels[[field]])),
        div(class = "guide-section", h3("Focus"), p(dimension_focus[[field]])),
        div(
          class = "guide-section",
          h3("Hotkeys"),
          if (field == "ai_low_effort_flag") {
            tags$ul(tags$li("A or 1 = yes"), tags$li("S or 2 = unsure"), tags$li("J or 3 = no"))
          } else {
            tags$ul(tags$li("A/S/D/F/J = 1/2/3/4/5"), tags$li("1 through 5 also work"))
          }
        ),
        div(class = "tip", h3("Reminder"), p("Score only the active dimension. Do not judge the other dimensions during this pass."))
      ))
    }

    current_item <- current()
    title_only_home <- !is.null(current_item) && identical(first_value(current_item, "source_type", "dataset"), "article_lab_generated")

    tagList(
      div(
        class = "guide-section",
        h3("How it works"),
        p(if (title_only_home) {
          "Rate this title-only candidate using only the visible headline."
        } else {
          "Rate each preview using only the visible headline, subtitle, and thumbnail."
        })
      ),
      div(
        class = "guide-section",
        h3("Focus on"),
        if (title_only_home) {
          tags$ul(
            tags$li("Headline clarity and hook"),
            tags$li("Specificity and credibility"),
            tags$li("Curiosity without clickbait"),
            tags$li("Your gut reaction to the title alone")
          )
        } else {
          tags$ul(
            tags$li("Headline clarity and hook"),
            tags$li("Topic relevance and appeal"),
            tags$li("Perceived value to readers"),
            tags$li("Your gut feeling")
          )
        }
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
  })

  output$article_area <- renderUI({
    item <- current()
    if (is.null(item)) {
      if (is_dimension_mode) {
        field <- active_dimension()
        if (is.na(field)) {
          return(div(class = "done-state", h2("All dimensions complete"), p("Every dimension pass has been completed for the cohort.")))
        }
        return(div(class = "done-state", h2("Dimension complete"), p(paste("Completed pass:", dimension_labels[[field]]))))
      }
      return(div(class = "done-state", h2("Session complete"), p("All queued previews have been rated or skipped.")))
    }

    field <- if (is_dimension_mode) active_dimension() else NA_character_
    render_info <- if (is_dimension_v2_mode) v2_render_info(item) else NULL
    is_article_lab_title_only <- identical(first_value(item, "source_type", "dataset"), "article_lab_generated")
    thumbnail_path <- item$local_thumbnail_path[[1]]
    thumbnail_path_abs <- if (is_dimension_v2_mode) {
      render_info$path_abs
    } else if ("local_thumbnail_path_abs" %in% names(item)) {
      item$local_thumbnail_path_abs[[1]]
    } else {
      as_abs_path(thumbnail_path)[[1]]
    }
    thumbnail_status <- if ("thumbnail_status" %in% names(item)) item$thumbnail_status[[1]] else NA_character_
    has_thumbnail <- if (is_dimension_v2_mode) {
      isTRUE(render_info$valid)
    } else {
      identical(thumbnail_status, "valid") && !is.na(thumbnail_path_abs) && file.exists(thumbnail_path_abs)
    }
    isolate_title_field <- is_article_lab_title_only || (is_dimension_mode && !is.na(field) && field %in% title_isolation_dimension_fields)
    thumbnail_ui <- if (isolate_title_field) {
      div(class = "thumbnail-placeholder", div(class = "thumbnail-invalid-label", title_only_placeholder_thumbnail_label))
    } else if (has_thumbnail) {
      imageOutput("thumbnail", width = "170px", height = "113px")
    } else if (is_dimension_v2_mode) {
      missing_reason <- render_info$reason %in% c("missing_file", "missing_manifest_hash", "missing_rendered_hash")
      placeholder_label <- if (isTRUE(missing_reason)) {
        "Thumbnail missing: validated manifest image unavailable"
      } else {
        "Thumbnail blocked: manifest/hash mismatch"
      }
      div(class = "thumbnail-placeholder error", div(class = "thumbnail-invalid-label", placeholder_label))
    } else {
      div(class = "thumbnail-placeholder", div(class = "thumbnail-invalid-label", "Invalid or missing thumbnail"))
    }

    subtitle <- if (is_article_lab_title_only) title_only_placeholder_subtitle else displayed_subtitle_for_field(item, field)
    thumbnail_only <- is_dimension_mode && !is.na(field) && field %in% thumbnail_only_dimension_fields
    text_only <- is_article_lab_title_only || (is_dimension_mode && !is.na(field) && field %in% text_only_dimension_fields)

    if (text_only) {
      return(div(
        class = "article-card",
        div(
          h2(class = "article-title", item$title[[1]]),
          if (!is.na(subtitle)) p(class = "article-subtitle", subtitle)
        ),
        div(class = "thumbnail-wrap", thumbnail_ui)
      ))
    }

    if (thumbnail_only) {
      return(div(
        class = "article-card thumbnail-only",
        div(class = "thumbnail-wrap", thumbnail_ui)
      ))
    }

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
    if (is_dimension_v2_mode) {
      info <- v2_render_info(item)
      req(isTRUE(info$valid))
      path_abs <- info$path_abs
    } else {
      path <- item$local_thumbnail_path[[1]]
      path_abs <- if ("local_thumbnail_path_abs" %in% names(item)) {
      item$local_thumbnail_path_abs[[1]]
      } else {
        as_abs_path(path)[[1]]
      }
    }
    req(!is.na(path_abs), file.exists(path_abs))
    list(src = normalizePath(path_abs, mustWork = TRUE), alt = "", width = 170, height = 113)
  }, deleteFile = FALSE)

  output$rating_panel <- renderUI({
    if (!is_dimension_mode) {
      current_item <- current()
      prompt_text <- if (!is.null(current_item) && identical(first_value(current_item, "source_type", "dataset"), "article_lab_generated")) {
        "Based only on the title, how likely is this article to perform well on Medium?"
      } else {
        rating_prompt
      }
      return(div(
        class = "rating-panel",
        div(class = "prompt", prompt_text),
        div(
          class = "note-row",
          textInput(
            "note",
            "Optional note",
            value = "",
            width = "100%",
            placeholder = "Quick note, e.g. AI thumbnail, strong title, generic topic"
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
          div(class = "shortcut-copy", "A=1, S=2, D=3, F=4, J=5 · 1-5 also work · Space=skip · U=undo · N=note · Enter/Esc exits note")
        )
      ))
    }

    field <- active_dimension()
    c <- counts()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- ifelse(is.na(c$total[[1]]), 0, c$total[[1]])

    if (is.na(field)) {
      return(div(
        class = "rating-panel",
        div(class = "dimension-pass-header",
            div(class = "dimension-pass-kicker", "Dimension pass complete"),
            div(class = "dimension-pass-name", "All dimensions complete"),
            div(
              class = "dimension-pass-focus",
              if (is_dimension_v2_mode) {
                sprintf(
                  "Overall manual rating progress: %s / %s ratings complete",
                  candidate_stats()$completed_dimensions[[1]],
                  candidate_stats()$total_dimensions[[1]]
                )
              } else {
                sprintf("Overall active dimension progress: %s / %s dimensions complete", length(active_dimension_fields), length(active_dimension_fields))
              }
            )
        )
      ))
    }

    if (total > 0 && completed >= total) {
      next_field <- next_incomplete_dimension_after(con, field)
      return(div(
        class = "rating-panel",
        div(class = "dimension-pass-header",
            div(class = "dimension-pass-kicker", "Dimension complete"),
            div(class = "dimension-pass-name", dimension_labels[[field]]),
            div(
              class = "dimension-pass-focus",
              if (is_dimension_v2_mode) {
                sprintf(
                  "Dimension progress: %s / %s · Overall manual rating progress: %s / %s ratings complete",
                  completed,
                  total,
                  candidate_stats()$completed_dimensions[[1]],
                  candidate_stats()$total_dimensions[[1]]
                )
              } else {
                sprintf("Dimension progress: %s / %s", completed, total)
              }
            )
        ),
        if (!is.na(next_field)) {
          div(
            class = "next-dimension-cta",
            div(class = "next-dimension-copy", sprintf("This pass is finished. Continue directly into the next dimension: %s.", dimension_labels[[next_field]])),
            actionButton("start_next_dimension", paste("Continue To", dimension_labels[[next_field]]))
          )
        } else {
          div(class = "shortcut-copy", "All dimension passes are complete.")
        }
      ))
    }

    item <- current()
    can_rate_current <- !is_dimension_v2_mode ||
      field %in% text_only_dimension_fields ||
      (!is.null(item) && isTRUE(v2_render_info(item)$valid))

    numeric_buttons <- function(field, enabled = TRUE) {
      div(
        class = "dimension-buttons",
        lapply(1:5, function(score) {
          tags$button(
            type = "button",
            class = "btn dimension-choice",
            `data-field` = field,
            `data-value` = as.character(score),
            disabled = if (isTRUE(enabled)) NULL else "disabled",
            as.character(score)
          )
        })
      )
    }
    flag_buttons <- function(field, enabled = TRUE) {
      choices <- c("yes", "unsure", "no")
      shortcuts <- c(yes = "D", unsure = "F", no = "J")
      div(
        class = "dimension-buttons",
        lapply(choices, function(choice) {
          tags$button(
            type = "button",
            class = "btn dimension-choice flag-choice",
            `data-field` = field,
            `data-value` = choice,
            disabled = if (isTRUE(enabled)) NULL else "disabled",
            span(class = "dimension-choice-label", choice),
            span(class = "dimension-choice-shortcut", shortcuts[[choice]])
          )
        })
      )
    }
    scale_ui <- function(field) {
      scale <- dimension_scale[[field]]
      scale_shortcuts <- c("1" = "A=1", "2" = "S=2", "3" = "D=3", "4" = "F=4", "5" = "J=5")
      flag_shortcuts <- c(yes = "S", unsure = "D", no = "J")
      div(
        class = paste("dimension-scale-list", if (field == "ai_low_effort_flag") "dimension-flag-scale" else ""),
        lapply(names(scale), function(name) {
          if (field == "ai_low_effort_flag") {
            tags$button(
              type = "button",
              class = "btn dimension-scale-item dimension-scale-choice dimension-choice",
              `data-field` = field,
              `data-value` = as.character(name),
              disabled = if (isTRUE(can_rate_current)) NULL else "disabled",
              strong(scale[[name]]),
              span(class = "dimension-scale-shortcut", flag_shortcuts[[as.character(name)]])
            )
          } else {
            tags$button(
              type = "button",
              class = "btn dimension-scale-item dimension-scale-choice dimension-choice",
              `data-field` = field,
              `data-value` = as.character(name),
              disabled = if (isTRUE(can_rate_current)) NULL else "disabled",
              strong(name),
              span(scale[[name]]),
              if (as.character(name) %in% names(scale_shortcuts)) {
                span(class = "dimension-scale-shortcut", scale_shortcuts[[as.character(name)]])
              }
            )
          }
        })
      )
    }

    verification_title <- if (
      !is.null(item) &&
        field %in% thumbnail_only_dimension_fields &&
        "title" %in% names(item) &&
        !is.na(item$title[[1]])
    ) {
      div(
        class = "dimension-verification-title",
        `data-copy-title` = item$title[[1]],
        title = "Click to copy title",
        item$title[[1]]
      )
    } else {
      NULL
    }

    div(
      class = "rating-panel",
      div(
        class = "dimension-pass-header",
        div(class = "dimension-pass-kicker", "Dimension pass"),
        div(class = "dimension-pass-name", paste("Active dimension:", dimension_labels[[field]])),
        div(class = "dimension-pass-focus", strong("Focus: "), dimension_focus[[field]]),
        div(class = "dimension-pass-question", strong("Question: "), dimension_questions[[field]]),
        div(
          class = "dimension-pass-focus",
          if (is_dimension_v2_mode) {
            sprintf(
              "Dimension progress: %s / %s · Overall manual rating progress: %s / %s ratings complete",
              completed,
              total,
              candidate_stats()$completed_dimensions[[1]],
              candidate_stats()$total_dimensions[[1]]
            )
          } else {
            sprintf(
              "Dimension progress: %s / %s · Overall active dimension progress: %s / %s dimensions complete",
              completed,
              total,
              candidate_stats()$completed_dimensions[[1]],
              candidate_stats()$total_dimensions[[1]]
            )
          }
        ),
        verification_title
      ),
      scale_ui(field),
      div(
        class = "note-row",
        textAreaInput(
          "note",
          "Optional note",
          value = "",
          width = "100%",
          height = "54px",
          placeholder = "Optional note"
        )
      ),
      div(
        class = "rating-actions",
        div(actionButton("skip", "Skip"), actionButton("undo", "Undo previous")),
        div(
          class = "shortcut-copy",
          if (field == "ai_low_effort_flag") {
            "S=yes, D=unsure, J=no · 1=yes, 2=unsure, 3=no · Space=skip, U=undo, N=note"
          } else {
            "A=1, S=2, D=3, F=4, J=5 · 1-5 also work · Space=skip, U=undo, N=note"
          }
        )
      )
    )
  })

  handle_score <- function(score) {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (is_dimension_mode) return(invisible(NULL))
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
    if (!identical(active_section(), "home")) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    if (is_dimension_mode) {
      save_current_dimension_rating(con, item, active_dimension(), note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    } else {
      save_current_rating(con, item, score = NULL, note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    }
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$skip_key, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    if (is_dimension_mode) {
      save_current_dimension_rating(con, item, active_dimension(), note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    } else {
      save_current_rating(con, item, score = NULL, note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    }
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$undo, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (is_dimension_mode) undo_previous_dimension_rating(con, active_dimension()) else undo_previous_rating(con, rating_session_id)
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$undo_key, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (is_dimension_mode) undo_previous_dimension_rating(con, active_dimension()) else undo_previous_rating(con, rating_session_id)
    refresh_current()
  }, ignoreInit = TRUE)

  apply_dimension_value <- function(field, value) {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (!is_dimension_mode || !(field %in% dimension_fields)) return(invisible(NULL))
    if (!identical(field, active_dimension())) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    if (
      is_dimension_v2_mode &&
        !(field %in% text_only_dimension_fields) &&
        !isTRUE(v2_render_info(item)$valid)
    ) {
      return(invisible(NULL))
    }
    save_current_dimension_rating(
      con,
      item,
      field,
      value = value,
      note = input$note,
      skipped = FALSE,
      shown_started_at = shown_started_at()
    )
    refresh_current()
  }

  observeEvent(input$dimension_select, {
    value <- input$dimension_select
    if (!is.list(value)) return(invisible(NULL))
    apply_dimension_value(value$field, value$value)
  }, ignoreInit = TRUE)

  observeEvent(input$dimension_key, {
    value <- input$dimension_key
    key <- if (is.list(value)) value$key else value
    field <- active_dimension()
    if (is.na(field)) return(invisible(NULL))
    if (field %in% dimension_numeric_fields) {
      numeric_map <- c(a = 1L, s = 2L, d = 3L, f = 4L, j = 5L)
      score <- if (key %in% names(numeric_map)) numeric_map[[key]] else suppressWarnings(as.integer(key))
      apply_dimension_value(field, score)
    } else {
      flag_map <- c(s = "yes", d = "unsure", j = "no", `1` = "yes", `2` = "unsure", `3` = "no")
      if (key %in% names(flag_map)) apply_dimension_value(field, flag_map[[key]])
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dimension_back_key, {
    invisible(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$dimension_reset_key, {
    invisible(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$start_next_dimension, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (!is_dimension_mode) return(invisible(NULL))
    next_field <- next_incomplete_dimension_after(con, active_dimension())
    if (!is.na(next_field)) active_dimension(next_field)
    refresh_current()
  }, ignoreInit = TRUE)

}

shinyApp(ui, server)
