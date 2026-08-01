article_lab_has_api_key <- function() {
  env_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (nzchar(trimws(env_key))) return(TRUE)
  env_path <- file.path(project_root, ".env")
  if (!file.exists(env_path)) return(FALSE)
  lines <- tryCatch(readLines(env_path, warn = FALSE), error = function(e) character())
  any(grepl("^\\s*OPENAI_API_KEY\\s*=\\s*.+", lines))
}

article_lab_validate_generation_settings <- function(model, reasoning_effort = NA_character_, reasoning_mode = "standard") {
  model <- article_lab_input_string(model)
  if (is.na(model)) stop("A generation model is required.", call. = FALSE)
  supported <- article_lab_reasoning_capabilities(model)
  effort <- article_lab_input_string(reasoning_effort)
  mode <- article_lab_input_string(reasoning_mode) %||% "standard"
  if (!is.null(effort) && !effort %in% supported) stop("Reasoning level '", effort, "' is not supported by model ", model, ".", call. = FALSE)
  if (!mode %in% c("standard", "pro")) stop("Unsupported execution mode: ", mode, call. = FALSE)
  if (identical(mode, "pro") && !article_lab_supports_pro_mode(model)) stop("Pro mode is not supported by model ", model, ".", call. = FALSE)
  list(model = model, reasoning_effort = if (length(supported) == 0 || is.null(effort)) NA_character_ else effort, reasoning_mode = if (article_lab_supports_pro_mode(model)) mode else "standard")
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

article_lab_effective_title_prompt_text <- function(prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, example_titles = character(), manual_prompt = NA_character_, context_notes = NA_character_) {
  requested_n <- suppressWarnings(as.integer(batch_size))
  if (length(requested_n) == 0L || is.na(requested_n)) requested_n <- 12L
  requested_n <- max(1L, min(25L, requested_n))
  sections <- c(
    "You generate Medium-style article title candidates for personal finance and investing.",
    "Return valid JSON only in the shape {\"titles\": [\"...\", \"...\"]}.",
    sprintf("Return exactly %s titles.", requested_n),
    sprintf("Every title must be at most %s characters, including spaces.", article_lab_title_max_chars),
    sprintf("Prefer %s-%s characters when possible. Do not make titles long unless the extra words clearly improve clarity or curiosity.", article_lab_title_preferred_min_chars, article_lab_title_preferred_max_chars),
    "Do not include explanations, numbering, markdown, or code fences.",
    "Do not copy any example title verbatim.",
    "Keep the titles credible, science-based, beginner-friendly, and not clickbait.",
    "If a title would exceed the limit, rewrite it shorter instead of truncating it."
  )
  optional_scalar <- function(value, multiline = FALSE) {
    cleaned <- if (multiline) article_lab_input_multiline(value) else article_lab_input_string(value)
    if (length(cleaned) == 0L || is.na(cleaned[[1]])) NA_character_ else cleaned[[1]]
  }
  seed <- optional_scalar(seed_topic)
  inspiration <- optional_scalar(inspiration_source)
  manual <- optional_scalar(manual_prompt, multiline = TRUE)
  context <- optional_scalar(context_notes, multiline = TRUE)
  article_summary <- optional_scalar(prompt, multiline = TRUE)
  if (is.na(article_summary)) article_summary <- article_lab_default_prompt
  examples <- clean_text(example_titles)
  examples <- unique(examples[!is.na(examples)])

  template <- if (!is.na(manual) && grepl("\\{\\{[a-z_]+\\}\\}", manual)) manual else if (grepl("\\{\\{[a-z_]+\\}\\}", article_summary)) article_summary else NA_character_
  if (!is.na(template)) {
    summary_value <- if (identical(template, manual)) article_summary else ""
    values <- c(
      idea_context = if (is.na(context)) "" else context,
      article_summary = if (is.na(summary_value)) "" else summary_value,
      batch_size = as.character(requested_n),
      seed_topic = if (is.na(seed)) "" else seed,
      inspiration_source = if (is.na(inspiration)) "" else inspiration,
      example_titles = if (length(examples) == 0L) "" else paste(sprintf("%s. %s", seq_along(examples), examples), collapse = "\n"),
      max_title_chars = as.character(article_lab_title_max_chars),
      preferred_title_length = sprintf("%s-%s", article_lab_title_preferred_min_chars, article_lab_title_preferred_max_chars)
    )
    rendered <- template
    for (key in names(values)) {
      if (!nzchar(values[[key]])) rendered <- gsub(sprintf("(?m)^[^\\n]*\\{\\{%s\\}\\}[^\\n]*\\n?", key), "", rendered, perl = TRUE)
      rendered <- gsub(sprintf("{{%s}}", key), values[[key]], rendered, fixed = TRUE)
    }
    unresolved <- unique(regmatches(rendered, gregexpr("\\{\\{[a-z_]+\\}\\}", rendered, perl = TRUE))[[1]])
    unresolved <- unresolved[nzchar(unresolved) & unresolved != "-1"]
    if (length(unresolved) > 0L) stop(sprintf("Unknown title-prompt variable%s: %s", ifelse(length(unresolved) == 1L, "", "s"), paste(unresolved, collapse = ", ")), call. = FALSE)
    rendered <- gsub("\\n{3,}", "\n\n", rendered, perl = TRUE)
    return(trimws(rendered))
  }

  if (!is.na(seed)) sections <- c(sections, sprintf("Seed topic: %s", seed))
  if (!is.na(inspiration)) sections <- c(sections, sprintf("Inspiration source: %s", inspiration))
  if (length(examples) > 0L) sections <- c(
    sections,
    "Reference examples from top-performing historical titles. Use them only as inspiration for tone/patterns:",
    paste(sprintf("%s. %s", seq_along(examples), examples), collapse = "\n")
  )
  if (!is.na(manual)) sections <- c(sections, "Manual/default prompt:", manual)
  sections <- c(sections, "Article summary:", article_summary)
  base_prompt <- paste(sections, collapse = "\n\n")
  if (!is.na(context)) sprintf("Article context:\n%s\n\n%s", context, base_prompt) else base_prompt
}

article_lab_api_request <- function(prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", example_titles = character(), manual_prompt = NA_character_, context_notes = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_titles.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_titles.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)

  settings <- article_lab_validate_generation_settings(article_lab_input_string(model) %||% article_lab_default_model, reasoning_effort, reasoning_mode)
  request_payload <- list(
    prompt = article_lab_input_string(prompt) %||% article_lab_default_prompt,
    manual_prompt = article_lab_input_multiline(manual_prompt),
    batch_size = as.integer(batch_size),
    seed_topic = article_lab_input_string(seed_topic),
    inspiration_source = article_lab_input_string(inspiration_source),
    model = settings$model,
    reasoning_effort = settings$reasoning_effort,
    reasoning_mode = settings$reasoning_mode,
    example_titles = unname(example_titles),
    context_notes = article_lab_input_multiline(context_notes)
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
    reasoning_effort = article_lab_input_string(parsed$reasoning_effort) %||% settings$reasoning_effort,
    reasoning_mode = article_lab_input_string(parsed$reasoning_mode) %||% settings$reasoning_mode,
    raw_json = stdout_text,
    example_titles_used = as.integer(length(example_titles)),
    response_id = article_lab_input_string(parsed$response_id),
    retry_used = isTRUE(parsed$retry_used),
    dropped_n = as.integer(parsed$dropped_count %||% 0L),
    dropped_titles = unname(unlist(parsed$dropped_titles %||% list(), use.names = FALSE))
  )
}

generate_title_candidates <- function(con, prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", manual_prompt = NA_character_, context_notes = NA_character_) {
  inspiration_value <- article_lab_input_string(inspiration_source)
  example_titles <- if (identical(inspiration_value, "top performing titles")) article_lab_top_title_examples(con, limit = 8L) else character()

  tryCatch({
    api_result <- article_lab_api_request(
      prompt = prompt,
      batch_size = batch_size,
      seed_topic = seed_topic,
      inspiration_source = inspiration_source,
      model = model,
      reasoning_effort = reasoning_effort,
      reasoning_mode = reasoning_mode,
      example_titles = example_titles,
      manual_prompt = manual_prompt,
      context_notes = context_notes
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
    list(
      titles = data.frame(row_number = integer(), title = character(), stringsAsFactors = FALSE),
      mode = "failed",
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

article_lab_score_api_request <- function(candidates, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt_version = NA_character_, scope = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "score_article_lab_titles.py")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/score_article_lab_titles.py", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(candidates) == 0) return(list(scores = data.frame(), errors = list()))
  python_bin <- article_lab_resolve_python()

  settings <- article_lab_validate_generation_settings(article_lab_input_string(model) %||% article_lab_default_score_model, reasoning_effort, reasoning_mode)
  request_payload <- list(
    model = settings$model,
    reasoning_effort = settings$reasoning_effort,
    reasoning_mode = settings$reasoning_mode,
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

article_lab_thumbnail_api_request <- function(packages, variants_per_package = article_lab_default_thumbnail_variants, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_) {
  requested_model <- article_lab_input_string(model) %||% article_lab_default_thumbnail_model
  article_lab_validate_image_generation_model(requested_model, "Selected Images-tab model")
  helper_path <- file.path("scripts", "writing_api", "generate_thumbnails.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_thumbnails.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(packages) == 0) return(list(rows = data.frame(), model = article_lab_default_thumbnail_model, mode = "api", raw_json = NULL))

  settings <- article_lab_validate_generation_settings(requested_model, reasoning_effort, reasoning_mode)
  request_payload <- list(
    model = settings$model,
    reasoning_effort = settings$reasoning_effort,
    reasoning_mode = settings$reasoning_mode,
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
        reasoning_effort = article_lab_input_string(thumbnail$reasoning_effort) %||% settings$reasoning_effort,
        reasoning_mode = article_lab_input_string(thumbnail$reasoning_mode) %||% settings$reasoning_mode,
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

generate_thumbnail_candidates <- function(packages, variants_per_package = article_lab_default_thumbnail_variants, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_) {
  tryCatch(
    article_lab_thumbnail_api_request(packages, variants_per_package = variants_per_package, model = model, reasoning_effort = reasoning_effort, reasoning_mode = reasoning_mode, prompt = prompt),
    error = function(e) {
      list(
        rows = data.frame(),
        model = article_lab_input_string(model) %||% article_lab_default_thumbnail_model,
        mode = "failed",
        raw_json = article_lab_input_string(prompt) %||% article_lab_default_thumbnail_prompt,
        fallback_reason = conditionMessage(e)
      )
    }
  )
}

article_lab_outline_api_request <- function(packages, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_, include_context = TRUE, context_notes = NULL) {
  helper_path <- file.path("scripts", "writing_api", "generate_outlines.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_outlines.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(packages) == 0) return(list(rows = data.frame(), model = article_lab_default_outline_model, mode = "api", raw_json = NULL))

  context_notes_clean <- article_lab_input_multiline(context_notes)
  settings <- article_lab_validate_generation_settings(article_lab_input_string(model) %||% article_lab_default_outline_model, reasoning_effort, reasoning_mode)
  request_payload <- list(
    model = settings$model,
    reasoning_effort = settings$reasoning_effort,
    reasoning_mode = settings$reasoning_mode,
    prompt = article_lab_input_multiline(prompt) %||% article_lab_default_outline_prompt,
    context_notes = if (is.null(context_notes_clean) || is.na(context_notes_clean) || !nzchar(context_notes_clean)) NULL else context_notes_clean,
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
      reasoning_effort = article_lab_input_string(parsed$reasoning_effort) %||% settings$reasoning_effort,
      reasoning_mode = article_lab_input_string(parsed$reasoning_mode) %||% settings$reasoning_mode,
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

generate_outline_drafts <- function(packages, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_, include_context = TRUE, context_notes = NULL) {
  tryCatch(
    article_lab_outline_api_request(packages, model = model, reasoning_effort = reasoning_effort, reasoning_mode = reasoning_mode, prompt = prompt, include_context = include_context, context_notes = context_notes),
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

article_lab_subtitle_api_request <- function(candidates, variants_per_title = 4L, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_) {
  helper_path <- file.path("scripts", "writing_api", "generate_subtitles.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_subtitles.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(candidates) == 0) return(list(rows = data.frame(), model = article_lab_default_subtitle_model, mode = "api", raw_json = NULL))

  settings <- article_lab_validate_generation_settings(article_lab_input_string(model) %||% article_lab_default_subtitle_model, reasoning_effort, reasoning_mode)
  request_payload <- list(
    model = settings$model,
    reasoning_effort = settings$reasoning_effort,
    reasoning_mode = settings$reasoning_mode,
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
      reasoning_effort = rep(article_lab_input_string(parsed$reasoning_effort) %||% settings$reasoning_effort, length(subtitles)),
      reasoning_mode = rep(article_lab_input_string(parsed$reasoning_mode) %||% settings$reasoning_mode, length(subtitles)),
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

generate_subtitle_candidates <- function(candidates, variants_per_title = 4L, model = NA_character_, reasoning_effort = NA_character_, reasoning_mode = "standard", prompt = NA_character_) {
  tryCatch(
    article_lab_subtitle_api_request(candidates, variants_per_title = variants_per_title, model = model, reasoning_effort = reasoning_effort, reasoning_mode = reasoning_mode, prompt = prompt),
    error = function(e) {
      list(
        rows = data.frame(),
        model = article_lab_input_string(model) %||% article_lab_default_subtitle_model,
        mode = "failed",
        raw_json = article_lab_input_string(prompt) %||% article_lab_default_subtitle_prompt,
        fallback_reason = conditionMessage(e)
      )
    }
  )
}
