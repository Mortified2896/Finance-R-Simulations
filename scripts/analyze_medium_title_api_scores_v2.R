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

parse_args <- function(args) {
  out <- list(
    db = file.path("data", "db", "medium_articles.sqlite"),
    prompt_version = "v2_1",
    model = NA_character_,
    scope = "all",
    sample_file = NA_character_,
    output_mode = "all",
    min_rows = 30L
  )

  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--db") {
      i <- i + 1
      if (i > length(args)) stop("--db requires a path", call. = FALSE)
      out$db <- args[[i]]
    } else if (startsWith(arg, "--db=")) {
      out$db <- sub("^--db=", "", arg)
    } else if (arg == "--prompt-version") {
      i <- i + 1
      if (i > length(args)) stop("--prompt-version requires a value", call. = FALSE)
      out$prompt_version <- args[[i]]
    } else if (startsWith(arg, "--prompt-version=")) {
      out$prompt_version <- sub("^--prompt-version=", "", arg)
    } else if (arg == "--model") {
      i <- i + 1
      if (i > length(args)) stop("--model requires a value", call. = FALSE)
      out$model <- args[[i]]
    } else if (startsWith(arg, "--model=")) {
      out$model <- sub("^--model=", "", arg)
    } else if (arg == "--scope") {
      i <- i + 1
      if (i > length(args)) stop("--scope requires a value", call. = FALSE)
      out$scope <- args[[i]]
    } else if (startsWith(arg, "--scope=")) {
      out$scope <- sub("^--scope=", "", arg)
    } else if (arg == "--sample-file") {
      i <- i + 1
      if (i > length(args)) stop("--sample-file requires a path", call. = FALSE)
      out$sample_file <- args[[i]]
    } else if (startsWith(arg, "--sample-file=")) {
      out$sample_file <- sub("^--sample-file=", "", arg)
    } else if (arg == "--output-mode") {
      i <- i + 1
      if (i > length(args)) stop("--output-mode requires a value", call. = FALSE)
      out$output_mode <- args[[i]]
    } else if (startsWith(arg, "--output-mode=")) {
      out$output_mode <- sub("^--output-mode=", "", arg)
    } else if (arg == "--min-rows") {
      i <- i + 1
      if (i > length(args)) stop("--min-rows requires a value", call. = FALSE)
      out$min_rows <- as.integer(args[[i]])
    } else if (startsWith(arg, "--min-rows=")) {
      out$min_rows <- as.integer(sub("^--min-rows=", "", arg))
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1
  }

  if (is.na(out$min_rows) || out$min_rows < 1) {
    stop("--min-rows must be a positive integer", call. = FALSE)
  }
  if (!(out$scope %in% c("title_only", "title_subtitle", "all", "*"))) {
    stop("--scope must be title_only, title_subtitle, or all", call. = FALSE)
  }
  if (!(out$output_mode %in% c("latest", "by_method", "snapshot", "both", "all"))) {
    stop("--output-mode must be latest, by_method, snapshot, both, or all", call. = FALSE)
  }

  out
}

	clean_text_vector <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
	}

	table_columns <- function(connection, table_name) {
	  dbGetQuery(
	    connection,
	    paste0("PRAGMA table_info(", dbQuoteIdentifier(connection, table_name), ")")
	  )$name
	}

as_logical_clean <- function(x) {
  if (is.logical(x)) return(x)
  value <- toupper(clean_text_vector(x))
  out <- rep(NA, length(value))
  out[value %in% c("TRUE", "T", "1", "YES")] <- TRUE
  out[value %in% c("FALSE", "F", "0", "NO")] <- FALSE
  out
}

as_numeric_clean <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_cor <- function(x, y, method) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3 || stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = method))
}

auc_rank <- function(actual, score) {
  actual <- as_logical_clean(actual)
  score <- as.numeric(score)
  ok <- !is.na(actual) & is.finite(score)
  actual <- actual[ok]
  score <- score[ok]
  positives <- sum(actual)
  negatives <- sum(!actual)
  if (length(actual) < 2 || positives == 0 || negatives == 0) return(NA_real_)
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[actual]) - positives * (positives + 1) / 2) / (positives * negatives)
}

precision_recall_at_top_fraction <- function(actual, score, fraction = 0.20) {
  actual <- as_logical_clean(actual)
  score <- as.numeric(score)
  ok <- !is.na(actual) & is.finite(score)
  actual <- actual[ok]
  score <- score[ok]
  if (length(actual) < 2 || sum(actual) == 0) {
    return(c(n = length(actual), predicted_n = NA_real_, precision = NA_real_, recall = NA_real_))
  }
  predicted_n <- max(1L, ceiling(length(actual) * fraction))
  chosen <- order(score, decreasing = TRUE, na.last = NA)[seq_len(predicted_n)]
  tp <- sum(actual[chosen])
  c(
    n = length(actual),
    predicted_n = predicted_n,
    precision = tp / predicted_n,
    recall = tp / sum(actual)
  )
}

write_empty_csv <- function(path, columns) {
  write.csv(as.data.frame(setNames(replicate(length(columns), logical(0), simplify = FALSE), columns)), path, row.names = FALSE)
}

empty_outputs <- function(output_dir, reason) {
  write_empty_csv(file.path(output_dir, "api_score_distribution.csv"), c("prompt_version", "model", "score_scope", "field", "n", "mean", "sd", "min", "max", "score_1", "score_2", "score_3", "score_4", "score_5"))
  write_empty_csv(file.path(output_dir, "api_score_correlations.csv"), c("prompt_version", "model", "score_scope", "field", "outcome", "n", "pearson", "spearman"))
  write_empty_csv(file.path(output_dir, "api_score_auc.csv"), c("prompt_version", "model", "score_scope", "field", "label", "n", "auc", "precision_at_top20_predicted", "recall_at_top20_predicted"))
  write_empty_csv(file.path(output_dir, "api_score_bucket_diagnostics.csv"), c("prompt_version", "model", "score_scope", "field", "score_value", "n", "mean_success_score", "median_success_score", "top_20_percent_rate", "mean_log_claps", "mean_log_responses", "median_claps", "median_responses"))
  write_empty_csv(file.path(output_dir, "api_predicted_success_bucket_diagnostics.csv"), c("prompt_version", "model", "score_scope", "predicted_success_bucket", "n", "mean_success_score", "median_success_score", "top_20_percent_rate", "median_claps", "median_responses", "mean_log_claps", "mean_log_responses"))
  example_cols <- c("prompt_version", "model", "score_scope", "canonical_article_key", "article_id", "medium_post_id", "title", "subtitle", "claps", "responses", "log_claps", "log_responses", "success_score", "top_20_percent", "medium_clap_potential", "medium_comment_potential", "overall_article_potential", "predicted_success_bucket", "short_reason")
  for (name in c("api_false_positives_overall.csv", "api_false_negatives_overall.csv", "api_comment_false_positives.csv", "api_comment_false_negatives.csv", "api_clap_false_positives.csv", "api_clap_false_negatives.csv")) {
    write_empty_csv(file.path(output_dir, name), example_cols)
  }
  writeLines(c(
    "Medium Title API Scores V2 Evaluation",
    "=====================================",
    "",
    reason,
    "",
    "No OpenAI API calls are made by this script.",
    "SQLite is opened read-only and is not modified."
  ), file.path(output_dir, "api_eval_summary.txt"))
}

sanitize_key_part <- function(x) {
  x <- ifelse(is.na(x) || !nzchar(x), "all", x)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  ifelse(nzchar(x), x, "all")
}

sample_label_from_path <- function(path) {
  if (is.na(path) || !nzchar(path)) return("all_articles")
  sanitize_key_part(tools::file_path_sans_ext(basename(path)))
}

method_key <- function(prompt_version_filter, model_filter, scope_filter, sample_file) {
  paste(
    sanitize_key_part(ifelse(is.na(prompt_version_filter), "all_prompts", prompt_version_filter)),
    sanitize_key_part(ifelse(is.na(model_filter), "all_models", model_filter)),
    sanitize_key_part(ifelse(is.na(scope_filter), "all_scopes", scope_filter)),
    paste0("sample_", sample_label_from_path(sample_file)),
    sep = "__"
  )
}

selected_output_dirs <- function(base_dir, output_mode, key, timestamp) {
  dirs <- character()
  if (output_mode %in% c("latest", "both", "all")) {
    dirs <- c(dirs, file.path(base_dir, "latest"))
  }
  if (output_mode %in% c("by_method", "both", "all")) {
    dirs <- c(dirs, file.path(base_dir, "by_method", key))
  }
  if (output_mode %in% c("snapshot", "all")) {
    dirs <- c(dirs, file.path(base_dir, "runs", paste(timestamp, key, sep = "_")))
  }
  unique(dirs)
}

sample_keys_from_file <- function(path) {
  if (is.na(path) || !nzchar(path)) return(character())
  if (!file.exists(path)) stop("Sample file not found: ", path, call. = FALSE)
  sample_df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!("canonical_article_key" %in% names(sample_df))) {
    stop("Sample file must include canonical_article_key: ", path, call. = FALSE)
  }
  keys <- clean_text_vector(sample_df$canonical_article_key)
  unique(keys[!is.na(keys)])
}

write_run_metadata <- function(output_dir, timestamp, args, prompt_version_filter, scope_filter, row_count, database_path, key) {
  metadata <- data.frame(
    field = c("timestamp", "prompt_version_filter", "model_filter", "scope_filter", "sample_file", "row_count", "db_path", "output_folder", "script_name", "method_key"),
    value = c(
      timestamp,
      ifelse(is.na(prompt_version_filter), "all", prompt_version_filter),
      ifelse(is.na(args$model), "all", args$model),
      ifelse(is.na(scope_filter), "all", scope_filter),
      ifelse(is.na(args$sample_file), "", args$sample_file),
      as.character(row_count),
      database_path,
      output_dir,
      "scripts/analyze_medium_title_api_scores_v2.R",
      key
    ),
    stringsAsFactors = FALSE
  )
  write.csv(metadata, file.path(output_dir, "run_metadata.csv"), row.names = FALSE)
  writeLines(paste(metadata$field, metadata$value, sep = ": "), file.path(output_dir, "run_metadata.txt"))
}

write_empty_outputs_all <- function(output_dirs, reason, timestamp, args, prompt_version_filter, scope_filter, database_path, key) {
  for (dir in output_dirs) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    empty_outputs(dir, reason)
    write_run_metadata(dir, timestamp, args, prompt_version_filter, scope_filter, 0L, database_path, key)
  }
}

write_report_outputs <- function(output_dirs, files, summary_lines, timestamp, args, prompt_version_filter, scope_filter, row_count, database_path, key) {
  for (dir in output_dirs) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    for (name in names(files)) {
      write.csv(files[[name]], file.path(dir, name), row.names = FALSE)
    }
    writeLines(summary_lines, file.path(dir, "api_eval_summary.txt"))
    write_run_metadata(dir, timestamp, args, prompt_version_filter, scope_filter, row_count, database_path, key)
  }
}

ensure_outcomes <- function(df) {
  df$claps <- as_numeric_clean(df$claps)
  df$responses <- as_numeric_clean(df$responses)

  if (!("log_claps" %in% names(df)) || all(is.na(df$log_claps))) {
    df$log_claps <- log1p(df$claps)
  } else {
    df$log_claps <- as_numeric_clean(df$log_claps)
    missing <- is.na(df$log_claps) & !is.na(df$claps)
    df$log_claps[missing] <- log1p(df$claps[missing])
  }

  if (!("log_responses" %in% names(df)) || all(is.na(df$log_responses))) {
    df$log_responses <- log1p(df$responses)
  } else {
    df$log_responses <- as_numeric_clean(df$log_responses)
    missing <- is.na(df$log_responses) & !is.na(df$responses)
    df$log_responses[missing] <- log1p(df$responses[missing])
  }

  if (!("success_score" %in% names(df)) || all(is.na(df$success_score))) {
    df$success_score <- df$log_claps + 2 * df$log_responses
  } else {
    df$success_score <- as_numeric_clean(df$success_score)
    missing <- is.na(df$success_score) & !is.na(df$log_claps) & !is.na(df$log_responses)
    df$success_score[missing] <- df$log_claps[missing] + 2 * df$log_responses[missing]
  }

  add_top_label <- function(column, fraction, value) {
    current <- if (column %in% names(df)) as_logical_clean(df[[column]]) else rep(NA, nrow(df))
    if (all(is.na(current)) && sum(is.finite(df$success_score)) > 0) {
      threshold <- as.numeric(stats::quantile(df$success_score, probs = 1 - fraction, na.rm = TRUE, type = 7))
      current <- df$success_score >= threshold
    }
    df[[column]] <<- current
  }

  add_top_label("top_20_percent", 0.20, TRUE)
  add_top_label("top_10_percent", 0.10, TRUE)
  add_top_label("top_5_percent", 0.05, TRUE)

  if (!("over_50_claps" %in% names(df))) df$over_50_claps <- df$claps >= 50
  if (!("over_100_claps" %in% names(df))) df$over_100_claps <- df$claps >= 100
  if (!("over_200_claps" %in% names(df))) df$over_200_claps <- df$claps >= 200
  df$over_50_claps <- as_logical_clean(df$over_50_claps)
  df$over_100_claps <- as_logical_clean(df$over_100_claps)
  df$over_200_claps <- as_logical_clean(df$over_200_claps)

  df$response_top20 <- rep(NA, nrow(df))
  if (sum(is.finite(df$log_responses)) > 0) {
    threshold <- as.numeric(stats::quantile(df$log_responses, probs = 0.80, na.rm = TRUE, type = 7))
    df$response_top20 <- df$log_responses >= threshold
  }

  df$clap_top20 <- rep(NA, nrow(df))
  if (sum(is.finite(df$log_claps)) > 0) {
    threshold <- as.numeric(stats::quantile(df$log_claps, probs = 0.80, na.rm = TRUE, type = 7))
    df$clap_top20 <- df$log_claps >= threshold
  }

  df
}

distribution_rows <- function(df, numeric_fields) {
  rows <- lapply(numeric_fields, function(field) {
    values <- as_numeric_clean(df[[field]])
    counts <- vapply(1:5, function(score) sum(values == score, na.rm = TRUE), integer(1))
	    data.frame(
	      prompt_version = unique(df$prompt_version)[1],
	      model = unique(df$model)[1],
	      score_scope = unique(df$score_scope)[1],
	      field = field,
      n = sum(!is.na(values)),
      mean = mean(values, na.rm = TRUE),
      sd = stats::sd(values, na.rm = TRUE),
      min = suppressWarnings(min(values, na.rm = TRUE)),
      max = suppressWarnings(max(values, na.rm = TRUE)),
      score_1 = counts[[1]],
      score_2 = counts[[2]],
      score_3 = counts[[3]],
      score_4 = counts[[4]],
      score_5 = counts[[5]],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[!is.finite(out$min), "min"] <- NA_real_
  out[!is.finite(out$max), "max"] <- NA_real_
  out
}

correlation_rows <- function(df, numeric_fields, outcomes) {
  rows <- list()
  index <- 1L
  for (field in numeric_fields) {
    for (outcome in outcomes) {
      x <- as_numeric_clean(df[[field]])
      y <- as_numeric_clean(df[[outcome]])
      ok <- is.finite(x) & is.finite(y)
	      rows[[index]] <- data.frame(
	        prompt_version = unique(df$prompt_version)[1],
	        model = unique(df$model)[1],
	        score_scope = unique(df$score_scope)[1],
	        field = field,
        outcome = outcome,
        n = sum(ok),
        pearson = safe_cor(x, y, "pearson"),
        spearman = safe_cor(x, y, "spearman"),
        stringsAsFactors = FALSE
      )
      index <- index + 1L
    }
  }
  do.call(rbind, rows)
}

bucket_rows <- function(df, numeric_fields) {
  rows <- list()
  index <- 1L
  for (field in numeric_fields) {
    values <- as_numeric_clean(df[[field]])
    for (score in 1:5) {
      keep <- values == score & !is.na(values)
      top20 <- as_logical_clean(df$top_20_percent)
	      rows[[index]] <- data.frame(
	        prompt_version = unique(df$prompt_version)[1],
	        model = unique(df$model)[1],
	        score_scope = unique(df$score_scope)[1],
	        field = field,
        score_value = score,
        n = sum(keep),
        mean_success_score = mean(df$success_score[keep], na.rm = TRUE),
        median_success_score = median(df$success_score[keep], na.rm = TRUE),
        top_20_percent_rate = mean(top20[keep], na.rm = TRUE),
        mean_log_claps = mean(df$log_claps[keep], na.rm = TRUE),
        mean_log_responses = mean(df$log_responses[keep], na.rm = TRUE),
        median_claps = median(df$claps[keep], na.rm = TRUE),
        median_responses = median(df$responses[keep], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      index <- index + 1L
    }
  }
  out <- do.call(rbind, rows)
	  numeric_cols <- setdiff(names(out), c("prompt_version", "model", "score_scope", "field"))
  for (col in numeric_cols) out[[col]][!is.finite(out[[col]])] <- NA_real_
  out
}

auc_rows <- function(df, numeric_fields, labels) {
  rows <- list()
  index <- 1L
  for (field in numeric_fields) {
    for (label in labels) {
      score <- as_numeric_clean(df[[field]])
      actual <- as_logical_clean(df[[label]])
      pr <- precision_recall_at_top_fraction(actual, score, 0.20)
	      rows[[index]] <- data.frame(
	        prompt_version = unique(df$prompt_version)[1],
	        model = unique(df$model)[1],
	        score_scope = unique(df$score_scope)[1],
	        field = field,
        label = label,
        n = pr[["n"]],
        auc = auc_rank(actual, score),
        precision_at_top20_predicted = pr[["precision"]],
        recall_at_top20_predicted = pr[["recall"]],
        stringsAsFactors = FALSE
      )
      index <- index + 1L
    }
  }
  do.call(rbind, rows)
}

predicted_bucket_rows <- function(df) {
  bucket <- clean_text_vector(df$predicted_success_bucket)
  levels <- c("low", "medium", "high")
  rows <- lapply(levels, function(level) {
    keep <- !is.na(bucket) & tolower(bucket) == level
    top20 <- as_logical_clean(df$top_20_percent)
	    data.frame(
	      prompt_version = unique(df$prompt_version)[1],
	      model = unique(df$model)[1],
	      score_scope = unique(df$score_scope)[1],
	      predicted_success_bucket = level,
      n = sum(keep),
      mean_success_score = mean(df$success_score[keep], na.rm = TRUE),
      median_success_score = median(df$success_score[keep], na.rm = TRUE),
      top_20_percent_rate = mean(top20[keep], na.rm = TRUE),
      median_claps = median(df$claps[keep], na.rm = TRUE),
      median_responses = median(df$responses[keep], na.rm = TRUE),
      mean_log_claps = mean(df$log_claps[keep], na.rm = TRUE),
      mean_log_responses = mean(df$log_responses[keep], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
	  for (col in setdiff(names(out), c("prompt_version", "model", "score_scope", "predicted_success_bucket"))) {
    out[[col]][!is.finite(out[[col]])] <- NA_real_
  }
  out
}

example_rows <- function(df, score_field, actual_field, score_direction, actual_direction, limit = 50) {
  score <- as_numeric_clean(df[[score_field]])
  actual <- as_numeric_clean(df[[actual_field]])
  if (score_direction == "high") {
    score_keep <- score >= 4
    score_order <- -score
  } else {
    score_keep <- score <= 2
    score_order <- score
  }
  if (actual_direction == "high") {
    actual_threshold <- as.numeric(stats::quantile(actual, probs = 0.80, na.rm = TRUE, type = 7))
    actual_keep <- actual >= actual_threshold
    actual_order <- -actual
  } else {
    actual_threshold <- as.numeric(stats::quantile(actual, probs = 0.50, na.rm = TRUE, type = 7))
    actual_keep <- actual <= actual_threshold
    actual_order <- actual
  }

  keep <- score_keep & actual_keep & !is.na(score_keep) & !is.na(actual_keep)
  out <- df[keep, example_columns(), drop = FALSE]
  if (nrow(out) == 0) return(out)
  order_index <- order(score_order[keep], actual_order[keep], na.last = TRUE)
  head(out[order_index, , drop = FALSE], limit)
}

example_rows_by_group <- function(groups, score_field, actual_field, score_direction, actual_direction, limit = 50) {
  rows <- lapply(groups, example_rows, score_field = score_field, actual_field = actual_field, score_direction = score_direction, actual_direction = actual_direction, limit = limit)
  rows <- rows[vapply(rows, nrow, integer(1)) > 0]
  if (length(rows) == 0) {
    return(as.data.frame(setNames(replicate(length(example_columns()), logical(0), simplify = FALSE), example_columns())))
  }
  do.call(rbind, rows)
}

example_columns <- function() {
  c(
    "prompt_version", "model", "score_scope", "canonical_article_key", "article_id", "medium_post_id",
    "title", "subtitle", "claps", "responses", "log_claps", "log_responses",
    "success_score", "top_20_percent", "medium_clap_potential",
    "medium_comment_potential", "overall_article_potential",
    "predicted_success_bucket", "short_reason"
  )
}

format_number <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("NA")
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

top_cor_line <- function(correlations, outcome) {
  rows <- correlations[correlations$outcome == outcome & !is.na(correlations$spearman), , drop = FALSE]
  if (nrow(rows) == 0) return(paste0(outcome, ": not enough variation to calculate."))
  rows <- rows[order(-abs(rows$spearman), rows$field), , drop = FALSE]
  paste0(outcome, ": ", rows$field[1], " Spearman ", format_number(rows$spearman[1]), " (Pearson ", format_number(rows$pearson[1]), ").")
}

read_baseline_context <- function() {
  candidates <- c(
    file.path("data", "analysis", "title_baseline_v2", "model_metrics.txt"),
    file.path("data", "analysis", "title_followup_v2", "model_comparison_with_context.txt"),
    file.path("data", "analysis", "subtitle_analysis_v2", "model_comparison_title_vs_subtitle.txt"),
    file.path("data", "analysis", "medium_analysis_v2", "title_baseline", "model_metrics.txt"),
    file.path("data", "analysis", "medium_analysis_v2", "title_followup", "model_comparison_with_context.txt"),
    file.path("data", "analysis", "medium_analysis_v2", "subtitle_analysis", "model_comparison_title_vs_subtitle.txt")
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    return("No V2 text-baseline summary files were found. Compare manually against title-only term-family AUC around 0.658 and title+subtitle term-family AUC around 0.696 if those are still current.")
  }
  paste("Found text-baseline context files:", paste(found, collapse = "; "))
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
database_path <- args$db
base_output_dir <- file.path("data", "analysis", "title_api_scores_v2")
prompt_version_filter <- if (tolower(args$prompt_version) %in% c("all", "*")) NA_character_ else args$prompt_version
scope_filter <- if (tolower(args$scope) %in% c("all", "*")) NA_character_ else args$scope
run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_method_key <- method_key(prompt_version_filter, args$model, scope_filter, args$sample_file)
output_dirs <- selected_output_dirs(base_output_dir, args$output_mode, output_method_key, run_timestamp)

message("Medium Title API Scores V2 Evaluation")
message("=====================================")
message("DB path: ", database_path)
message("Output mode: ", args$output_mode)
message("Method key: ", output_method_key)
message("Output dirs: ", paste(output_dirs, collapse = "; "))
message("Prompt version: ", ifelse(is.na(prompt_version_filter), "all", prompt_version_filter))
message("Model: ", ifelse(is.na(args$model), "all", args$model))
message("Scope: ", ifelse(is.na(scope_filter), "all", scope_filter))
message("Sample file: ", ifelse(is.na(args$sample_file), "none", args$sample_file))

if (!file.exists(database_path)) {
  stop("Could not find database at: ", database_path, call. = FALSE)
}

for (dir in output_dirs) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
sample_keys <- sample_keys_from_file(args$sample_file)

connection <- dbConnect(SQLite(), database_path, flags = SQLITE_RO)
on.exit(dbDisconnect(connection), add = TRUE)

required_objects <- c("medium_title_api_scores", "v_medium_title_prediction_dataset_v2")
missing_objects <- required_objects[!vapply(required_objects, dbExistsTable, logical(1), conn = connection)]
if (length(missing_objects) > 0) {
  write_empty_outputs_all(
    output_dirs,
    paste("Missing required DB object(s):", paste(missing_objects, collapse = ", ")),
    run_timestamp,
    args,
    prompt_version_filter,
    scope_filter,
    database_path,
    output_method_key
  )
  stop("Missing required DB object(s): ", paste(missing_objects, collapse = ", "), call. = FALSE)
}

score_columns <- table_columns(connection, "medium_title_api_scores")
scope_expr <- if ("score_scope" %in% score_columns) "COALESCE(NULLIF(TRIM(score_scope), ''), 'title_subtitle')" else "'legacy_title_subtitle'"
scope_where_expr <- if ("score_scope" %in% score_columns) "COALESCE(NULLIF(TRIM(s.score_scope), ''), 'title_subtitle')" else "'legacy_title_subtitle'"

available_counts <- dbGetQuery(connection, paste0("
  SELECT prompt_version, model, ", scope_expr, " AS score_scope, COUNT(*) AS n
  FROM medium_title_api_scores
  GROUP BY prompt_version, model, score_scope
  ORDER BY prompt_version, model, score_scope
"))
dataset_rows <- dbGetQuery(connection, "SELECT COUNT(*) AS n FROM v_medium_title_prediction_dataset_v2")$n[[1]]

sql <- paste0("
WITH latest_scores AS (
  SELECT *
  FROM (
    SELECT
      s.*,
      ", scope_where_expr, " AS score_scope_normalized,
      ROW_NUMBER() OVER (
        PARTITION BY s.canonical_article_key, s.prompt_version, s.model, ", scope_where_expr, "
        ORDER BY s.scored_at DESC, s.id DESC
      ) AS rn
    FROM medium_title_api_scores s
    WHERE (? IS NULL OR s.prompt_version = ?)
      AND (? IS NULL OR s.model = ?)
      AND (? IS NULL OR ", scope_where_expr, " = ?)
      AND (s.error IS NULL OR TRIM(s.error) = '')
  )
  WHERE rn = 1
)
SELECT
  s.prompt_version,
  s.model,
  s.score_scope_normalized AS score_scope,
  s.scored_at,
  s.canonical_article_key,
  d.article_id,
  COALESCE(s.medium_post_id, d.medium_post_id) AS medium_post_id,
  d.title,
  d.subtitle,
  d.claps,
  d.responses,
  d.log_claps,
  d.log_responses,
  d.success_score,
  d.top_20_percent,
  d.top_10_percent,
  d.top_5_percent,
  d.over_50_claps,
  d.over_100_claps,
  d.over_200_claps,
  s.clarity,
  s.curiosity,
  s.specificity,
  s.beginner_appeal,
  s.credibility,
  s.emotional_pull,
  s.promise_strength,
  s.medium_clap_potential,
  s.medium_comment_potential,
  s.overall_article_potential,
  s.trust_risk,
  s.predicted_success_bucket,
  s.short_reason
FROM latest_scores s
JOIN v_medium_title_prediction_dataset_v2 d
  ON d.canonical_article_key = s.canonical_article_key
ORDER BY s.prompt_version, s.model, s.score_scope_normalized, s.canonical_article_key
")

scored <- dbGetQuery(connection, sql, params = list(prompt_version_filter, prompt_version_filter, args$model, args$model, scope_filter, scope_filter))
if (length(sample_keys) > 0) {
  before_sample_filter <- nrow(scored)
  scored <- scored[scored$canonical_article_key %in% sample_keys, , drop = FALSE]
  message("Sample-file matched rows: ", nrow(scored), " of ", before_sample_filter, " joined scored rows.")
}
message("Usable joined scored rows: ", nrow(scored))

if (nrow(scored) == 0) {
	reason <- paste0(
	  "No usable joined API score rows found for prompt_version=", ifelse(is.na(prompt_version_filter), "all", prompt_version_filter),
	    ifelse(is.na(args$model), " and all models", paste0(" and model=", args$model)),
	    ifelse(is.na(scope_filter), " and all scopes", paste0(" and scope=", scope_filter)),
	    ifelse(length(sample_keys) > 0, paste0(" within sample_file=", args$sample_file, "."), ".")
	  )
  write_empty_outputs_all(output_dirs, reason, run_timestamp, args, prompt_version_filter, scope_filter, database_path, output_method_key)
  quit(status = 0)
}

numeric_fields <- c(
  "clarity", "curiosity", "specificity", "beginner_appeal", "credibility",
  "emotional_pull", "promise_strength", "medium_clap_potential",
  "medium_comment_potential", "overall_article_potential", "trust_risk"
)
outcomes <- c("log_claps", "log_responses", "success_score", "claps", "responses")
labels <- c("top_20_percent", "top_10_percent", "top_5_percent", "over_50_claps", "over_100_claps", "over_200_claps")

for (field in numeric_fields) scored[[field]] <- as_numeric_clean(scored[[field]])
scored <- ensure_outcomes(scored)

groups <- split(scored, paste(scored$prompt_version, scored$model, scored$score_scope, sep = " / "), drop = TRUE)

distribution <- do.call(rbind, lapply(groups, distribution_rows, numeric_fields = numeric_fields))
correlations <- do.call(rbind, lapply(groups, correlation_rows, numeric_fields = numeric_fields, outcomes = outcomes))
bucket_diagnostics <- do.call(rbind, lapply(groups, bucket_rows, numeric_fields = numeric_fields))
auc_table <- do.call(rbind, lapply(groups, auc_rows, numeric_fields = numeric_fields, labels = labels))
predicted_bucket_diagnostics <- do.call(rbind, lapply(groups, predicted_bucket_rows))

for (col in example_columns()) {
  if (!(col %in% names(scored))) scored[[col]] <- NA
}

output_files <- list(
  "api_score_distribution.csv" = distribution,
  "api_score_correlations.csv" = correlations,
  "api_score_auc.csv" = auc_table,
  "api_score_bucket_diagnostics.csv" = bucket_diagnostics,
  "api_predicted_success_bucket_diagnostics.csv" = predicted_bucket_diagnostics,
  "api_false_positives_overall.csv" = example_rows_by_group(groups, "overall_article_potential", "success_score", "high", "low"),
  "api_false_negatives_overall.csv" = example_rows_by_group(groups, "overall_article_potential", "success_score", "low", "high"),
  "api_comment_false_positives.csv" = example_rows_by_group(groups, "medium_comment_potential", "log_responses", "high", "low"),
  "api_comment_false_negatives.csv" = example_rows_by_group(groups, "medium_comment_potential", "log_responses", "low", "high"),
  "api_clap_false_positives.csv" = example_rows_by_group(groups, "medium_clap_potential", "log_claps", "high", "low"),
  "api_clap_false_negatives.csv" = example_rows_by_group(groups, "medium_clap_potential", "log_claps", "low", "high")
)

group_counts <- aggregate(canonical_article_key ~ prompt_version + model + score_scope, scored, length)
names(group_counts)[names(group_counts) == "canonical_article_key"] <- "usable_rows"

sample_comparison_lines <- character()
if (length(groups) > 1) {
  group_keys <- lapply(groups, function(df) unique(df$canonical_article_key))
  pair_lines <- character()
  group_names <- names(group_keys)
  for (i in seq_len(length(group_names) - 1)) {
    for (j in seq.int(i + 1, length(group_names))) {
      left <- group_names[[i]]
      right <- group_names[[j]]
      overlap <- length(intersect(group_keys[[left]], group_keys[[right]]))
      left_n <- length(group_keys[[left]])
      right_n <- length(group_keys[[right]])
      pair_lines <- c(
        pair_lines,
        sprintf(
          "%s vs %s: overlap %s of %s/%s rows.",
          left,
          right,
          format(overlap, big.mark = ","),
          format(left_n, big.mark = ","),
          format(right_n, big.mark = ",")
        )
      )
    }
  }
  sample_comparison_lines <- c(
    "Prompt/model/scope sample comparison:",
    pair_lines,
    "WARNING: prompt/model/scope comparisons are fairest only when scored on the same canonical articles."
  )
}

key_auc <- auc_table[auc_table$field %in% c("medium_clap_potential", "medium_comment_potential", "overall_article_potential") & auc_table$label %in% c("top_20_percent", "top_10_percent", "top_5_percent"), , drop = FALSE]
key_auc <- key_auc[order(key_auc$prompt_version, key_auc$model, key_auc$score_scope, key_auc$label, -key_auc$auc), , drop = FALSE]

summary_lines <- c(
  "Medium Title API Scores V2 Evaluation",
  "=====================================",
  "",
  paste("DB path:", database_path),
  paste("Output mode:", args$output_mode),
  paste("Output method key:", output_method_key),
  paste("Output dirs:", paste(output_dirs, collapse = "; ")),
	  paste("Prompt version filter:", ifelse(is.na(prompt_version_filter), "all", prompt_version_filter)),
	  paste("Model filter:", ifelse(is.na(args$model), "all", args$model)),
	  paste("Scope filter:", ifelse(is.na(scope_filter), "all", scope_filter)),
  paste("Sample file:", ifelse(is.na(args$sample_file), "none", args$sample_file)),
  paste("Sample file canonical keys:", ifelse(length(sample_keys) == 0, "none", format(length(sample_keys), big.mark = ","))),
  paste("V2 title prediction dataset rows:", format(dataset_rows, big.mark = ",")),
  "",
  "Available API score counts in DB:",
  if (nrow(available_counts) == 0) "  No API scores found." else paste(capture.output(print(available_counts, row.names = FALSE)), collapse = "\n"),
  "",
  "Usable joined rows analyzed:",
  paste(capture.output(print(group_counts, row.names = FALSE)), collapse = "\n"),
  "",
  sample_comparison_lines,
  ""
)

total_rows <- nrow(scored)
if (total_rows < args$min_rows) {
  summary_lines <- c(summary_lines, paste0("WARNING: sample is below --min-rows=", args$min_rows, ". Treat all diagnostics as smoke-test output."))
} else if (total_rows < 100) {
  summary_lines <- c(summary_lines, "WARNING: fewer than 100 rows. Directional signal only; avoid firm conclusions.")
} else if (total_rows < 200) {
  summary_lines <- c(summary_lines, "WARNING: fewer than 200 rows. Useful for early diagnostics, still not a stable benchmark.")
} else {
  summary_lines <- c(summary_lines, "Sample size is large enough for a first diagnostic read, but still correlational.")
}

summary_lines <- c(summary_lines, "")

for (group_name in names(groups)) {
  group_df <- groups[[group_name]]
  group_cor <- correlations[paste(correlations$prompt_version, correlations$model, correlations$score_scope, sep = " / ") == group_name, , drop = FALSE]
  group_dist <- distribution[paste(distribution$prompt_version, distribution$model, distribution$score_scope, sep = " / ") == group_name, , drop = FALSE]
  group_bucket <- predicted_bucket_diagnostics[paste(predicted_bucket_diagnostics$prompt_version, predicted_bucket_diagnostics$model, predicted_bucket_diagnostics$score_scope, sep = " / ") == group_name, , drop = FALSE]
  low_variance <- group_dist$field[!is.na(group_dist$sd) & group_dist$sd < 0.50]

  summary_lines <- c(
    summary_lines,
    paste("Group:", group_name),
    paste("Rows:", nrow(group_df)),
    top_cor_line(group_cor, "log_claps"),
    top_cor_line(group_cor, "log_responses"),
    top_cor_line(group_cor, "success_score")
  )

  clap_cor <- group_cor[group_cor$field == "medium_clap_potential" & group_cor$outcome == "log_claps", "spearman"]
  comment_cor <- group_cor[group_cor$field == "medium_comment_potential" & group_cor$outcome == "log_responses", "spearman"]
  summary_lines <- c(
    summary_lines,
    paste0("medium_clap_potential vs log_claps Spearman: ", format_number(clap_cor)),
    paste0("medium_comment_potential vs log_responses Spearman: ", format_number(comment_cor))
  )

  if (nrow(group_bucket) > 0 && any(group_bucket$n > 0)) {
    bucket_text <- paste(
      apply(group_bucket[group_bucket$n > 0, c("predicted_success_bucket", "n", "mean_success_score", "top_20_percent_rate"), drop = FALSE], 1, function(row) {
        paste0(row[["predicted_success_bucket"]], ": n=", row[["n"]], ", mean_success=", format_number(as.numeric(row[["mean_success_score"]])), ", top20_rate=", format_number(as.numeric(row[["top_20_percent_rate"]])))
      }),
      collapse = "; "
    )
    summary_lines <- c(summary_lines, paste("predicted_success_bucket separation:", bucket_text))
  } else {
    summary_lines <- c(summary_lines, "predicted_success_bucket separation: not enough rows.")
  }

  if (length(low_variance) > 0) {
    summary_lines <- c(summary_lines, paste("Low-variance score fields to inspect:", paste(low_variance, collapse = ", ")))
  }
  summary_lines <- c(summary_lines, "")
}

summary_lines <- c(
  summary_lines,
  "Key AUC rows:",
  if (nrow(key_auc) == 0) "  Not enough scored rows/classes for AUC." else paste(capture.output(print(key_auc, row.names = FALSE)), collapse = "\n"),
  "",
  "Text baseline context:",
  read_baseline_context(),
  "",
  "Manual review next:",
  "- Inspect false positive/false negative CSVs for whether API scores are rewarding the wrong title patterns.",
  "- Compare title_only and title_subtitle scopes on overlapping articles before deciding whether subtitles add signal.",
  "- Compare overall_article_potential AUC against the matching V2 text baseline before treating API scoring as additive.",
  "- Check whether medium_clap_potential and medium_comment_potential separate different behavior instead of duplicating one generic score.",
  "",
  "Reminder: this evaluation is correlational and uses public claps/responses, not direct views, reads, impressions, or clicks.",
  "No OpenAI API calls are made by this script. SQLite is opened read-only and is not modified."
)

write_report_outputs(output_dirs, output_files, summary_lines, run_timestamp, args, prompt_version_filter, scope_filter, nrow(scored), database_path, output_method_key)

message("Wrote API evaluation outputs:")
for (dir in output_dirs) {
  message("  ", file.path(dir, "api_eval_summary.txt"))
}
