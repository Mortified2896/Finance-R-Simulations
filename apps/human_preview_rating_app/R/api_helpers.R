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

article_lab_api_request <- function(prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_, example_titles = character(), manual_prompt = NA_character_, context_notes = NA_character_) {
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
    raw_json = stdout_text,
    example_titles_used = as.integer(length(example_titles)),
    response_id = article_lab_input_string(parsed$response_id),
    retry_used = isTRUE(parsed$retry_used),
    dropped_n = as.integer(parsed$dropped_count %||% 0L),
    dropped_titles = unname(unlist(parsed$dropped_titles %||% list(), use.names = FALSE))
  )
}

generate_title_candidates <- function(con, prompt, batch_size, seed_topic = NA_character_, inspiration_source = NA_character_, model = NA_character_, manual_prompt = NA_character_, context_notes = NA_character_) {
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

article_lab_outline_api_request <- function(packages, model = NA_character_, prompt = NA_character_, include_context = TRUE, context_notes = NULL) {
  helper_path <- file.path("scripts", "writing_api", "generate_outlines.mjs")
  if (!file.exists(file.path(project_root, helper_path))) stop("Missing helper script: scripts/writing_api/generate_outlines.mjs", call. = FALSE)
  if (!article_lab_has_api_key()) stop("OPENAI_API_KEY is not configured in the environment or local .env file.", call. = FALSE)
  if (nrow(packages) == 0) return(list(rows = data.frame(), model = article_lab_default_outline_model, mode = "api", raw_json = NULL))

  context_notes_clean <- article_lab_input_multiline(context_notes)
  request_payload <- list(
    model = article_lab_input_string(model) %||% article_lab_default_outline_model,
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

generate_outline_drafts <- function(packages, model = NA_character_, prompt = NA_character_, include_context = TRUE, context_notes = NULL) {
  tryCatch(
    article_lab_outline_api_request(packages, model = model, prompt = prompt, include_context = include_context, context_notes = context_notes),
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

