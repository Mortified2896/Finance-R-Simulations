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
    prompt_version = "thumbnail_v1",
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
  if (!(out$scope %in% c("thumbnail_only", "title_thumbnail", "title_subtitle_thumbnail", "all", "*"))) {
    stop("--scope must be thumbnail_only, title_thumbnail, title_subtitle_thumbnail, or all", call. = FALSE)
  }
  if (!(out$output_mode %in% c("latest", "by_method", "snapshot", "both", "all"))) {
    stop("--output-mode must be latest, by_method, snapshot, both, or all", call. = FALSE)
  }
  out
}

clean_text <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

as_numeric_clean <- function(x) suppressWarnings(as.numeric(x))

as_logical_clean <- function(x) {
  if (is.logical(x)) return(x)
  value <- toupper(clean_text(x))
  out <- rep(NA, length(value))
  out[value %in% c("TRUE", "T", "1", "YES")] <- TRUE
  out[value %in% c("FALSE", "F", "0", "NO")] <- FALSE
  out
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
  c(n = length(actual), predicted_n = predicted_n, precision = tp / predicted_n, recall = tp / sum(actual))
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
  if (output_mode %in% c("latest", "both", "all")) dirs <- c(dirs, file.path(base_dir, "latest"))
  if (output_mode %in% c("by_method", "both", "all")) dirs <- c(dirs, file.path(base_dir, "by_method", key))
  if (output_mode %in% c("snapshot", "all")) dirs <- c(dirs, file.path(base_dir, "runs", paste(timestamp, key, sep = "_")))
  unique(dirs)
}

sample_keys_from_file <- function(path) {
  if (is.na(path) || !nzchar(path)) return(character())
  if (!file.exists(path)) stop("Sample file not found: ", path, call. = FALSE)
  sample_df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!("canonical_article_key" %in% names(sample_df))) stop("Sample file must include canonical_article_key", call. = FALSE)
  keys <- clean_text(sample_df$canonical_article_key)
  unique(keys[!is.na(keys)])
}

write_empty_csv <- function(path, columns) {
  write.csv(as.data.frame(setNames(replicate(length(columns), logical(0), simplify = FALSE), columns)), path, row.names = FALSE)
}

write_run_metadata <- function(output_dir, timestamp, args, prompt_version_filter, scope_filter, row_count, database_path, key, image_input_modes) {
  metadata <- data.frame(
    field = c("timestamp", "prompt_version_filter", "model_filter", "scope_filter", "sample_file", "row_count", "image_input_modes", "db_path", "output_folder", "script_name", "method_key"),
    value = c(timestamp, ifelse(is.na(prompt_version_filter), "all", prompt_version_filter), ifelse(is.na(args$model), "all", args$model), ifelse(is.na(scope_filter), "all", scope_filter), ifelse(is.na(args$sample_file), "", args$sample_file), as.character(row_count), paste(image_input_modes, collapse = ","), database_path, output_dir, "scripts/analyze_medium_thumbnail_api_scores_v1.R", key),
    stringsAsFactors = FALSE
  )
  write.csv(metadata, file.path(output_dir, "run_metadata.csv"), row.names = FALSE)
  writeLines(paste(metadata$field, metadata$value, sep = ": "), file.path(output_dir, "run_metadata.txt"))
}

empty_outputs <- function(output_dir, reason) {
  write_empty_csv(file.path(output_dir, "thumbnail_score_distribution.csv"), c("prompt_version", "model", "score_scope", "image_input_mode", "field", "n", "mean", "sd", "min", "max", "score_1", "score_2", "score_3", "score_4", "score_5"))
  write_empty_csv(file.path(output_dir, "thumbnail_score_correlations.csv"), c("prompt_version", "model", "score_scope", "image_input_mode", "field", "outcome", "n", "pearson", "spearman"))
  write_empty_csv(file.path(output_dir, "thumbnail_score_auc.csv"), c("prompt_version", "model", "score_scope", "image_input_mode", "field", "label", "n", "auc", "precision_at_top20_predicted", "recall_at_top20_predicted"))
  write_empty_csv(file.path(output_dir, "thumbnail_scope_comparison.csv"), c("prompt_version", "model", "score_scope", "image_input_mode", "n", "best_field_success_score", "best_success_score_spearman", "best_success_score_pearson", "best_field_log_claps", "best_log_claps_spearman", "best_log_claps_pearson", "best_field_log_responses", "best_log_responses_spearman", "best_log_responses_pearson", "best_top20_auc_field", "best_top20_auc", "best_top20_precision_at_top20", "overall_thumbnail_potential_success_spearman", "overall_thumbnail_potential_log_claps_spearman", "overall_thumbnail_potential_log_responses_spearman", "overall_thumbnail_potential_top20_auc", "predicted_bucket_top20_spread"))
  write_empty_csv(file.path(output_dir, "thumbnail_field_leaderboard.csv"), c("prompt_version", "model", "score_scope", "image_input_mode", "field", "n", "success_score_spearman", "success_score_pearson", "log_claps_spearman", "log_claps_pearson", "log_responses_spearman", "log_responses_pearson", "top20_auc", "top10_auc", "top5_auc", "precision_at_top20_predicted", "recall_at_top20_predicted"))
  write_empty_csv(file.path(output_dir, "thumbnail_field_leaderboard_overall_sorted.csv"), c("prompt_version", "model", "score_scope", "image_input_mode", "field", "n", "success_score_spearman", "success_score_pearson", "log_claps_spearman", "log_claps_pearson", "log_responses_spearman", "log_responses_pearson", "top20_auc", "top10_auc", "top5_auc", "precision_at_top20_predicted", "recall_at_top20_predicted"))
  write_empty_csv(file.path(output_dir, "thumbnail_vs_title_context.csv"), c("method_type", "scope", "prompt_version", "model", "n", "best_success_score_spearman", "best_log_claps_spearman", "best_log_responses_spearman", "best_top20_auc"))
  write_empty_csv(file.path(output_dir, "thumbnail_score_bucket_diagnostics.csv"), c("prompt_version", "model", "score_scope", "image_input_mode", "field", "score_value", "n", "mean_success_score", "median_success_score", "top_20_percent_rate", "mean_log_claps", "mean_log_responses", "median_claps", "median_responses"))
  write_empty_csv(file.path(output_dir, "thumbnail_predicted_success_bucket_diagnostics.csv"), c("prompt_version", "model", "score_scope", "image_input_mode", "predicted_success_bucket", "n", "mean_success_score", "median_success_score", "top_20_percent_rate", "median_claps", "median_responses", "mean_log_claps", "mean_log_responses"))
  example_cols <- c("prompt_version", "model", "score_scope", "image_input_mode", "canonical_article_key", "article_id", "medium_post_id", "title", "subtitle", "claps", "responses", "log_claps", "log_responses", "success_score", "top_20_percent", "overall_thumbnail_potential", "predicted_success_bucket", "short_reason")
  write_empty_csv(file.path(output_dir, "thumbnail_false_positives_overall.csv"), example_cols)
  write_empty_csv(file.path(output_dir, "thumbnail_false_negatives_overall.csv"), example_cols)
  writeLines(c(
    "Medium Thumbnail API Scores V1 Evaluation",
    "=========================================",
    "",
    reason,
    "",
    "WARNING: 0 usable rows means this is setup output only.",
    "No OpenAI API calls are made by this script. SQLite is opened read-only and is not modified.",
    "Reminder: this evaluation is correlational and uses public claps/responses, not views/clicks."
  ), file.path(output_dir, "thumbnail_eval_summary.txt"))
}

write_empty_outputs_all <- function(output_dirs, reason, timestamp, args, prompt_version_filter, scope_filter, database_path, key) {
  for (dir in output_dirs) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    empty_outputs(dir, reason)
    write_run_metadata(dir, timestamp, args, prompt_version_filter, scope_filter, 0L, database_path, key, character())
  }
}

ensure_outcomes <- function(df) {
  df$claps <- as_numeric_clean(df$claps)
  df$responses <- as_numeric_clean(df$responses)
  df$log_claps <- if (!("log_claps" %in% names(df)) || all(is.na(df$log_claps))) log1p(df$claps) else as_numeric_clean(df$log_claps)
  df$log_responses <- if (!("log_responses" %in% names(df)) || all(is.na(df$log_responses))) log1p(df$responses) else as_numeric_clean(df$log_responses)
  df$success_score <- if (!("success_score" %in% names(df)) || all(is.na(df$success_score))) df$log_claps + 2 * df$log_responses else as_numeric_clean(df$success_score)
  for (label in c("top_20_percent", "top_10_percent", "top_5_percent")) df[[label]] <- as_logical_clean(df[[label]])
  for (label in c("over_50_claps", "over_100_claps", "over_200_claps")) df[[label]] <- as_logical_clean(df[[label]])
  df
}

group_id <- function(df) paste(df$prompt_version, df$model, df$score_scope, df$image_input_mode, sep = " / ")

distribution_rows <- function(df, numeric_fields) {
  do.call(rbind, lapply(numeric_fields, function(field) {
    values <- as_numeric_clean(df[[field]])
    counts <- vapply(1:5, function(score) sum(values == score, na.rm = TRUE), integer(1))
    data.frame(prompt_version = unique(df$prompt_version)[1], model = unique(df$model)[1], score_scope = unique(df$score_scope)[1], image_input_mode = unique(df$image_input_mode)[1], field = field, n = sum(!is.na(values)), mean = mean(values, na.rm = TRUE), sd = stats::sd(values, na.rm = TRUE), min = suppressWarnings(min(values, na.rm = TRUE)), max = suppressWarnings(max(values, na.rm = TRUE)), score_1 = counts[[1]], score_2 = counts[[2]], score_3 = counts[[3]], score_4 = counts[[4]], score_5 = counts[[5]], stringsAsFactors = FALSE)
  }))
}

correlation_rows <- function(df, numeric_fields, outcomes) {
  rows <- list()
  index <- 1L
  for (field in numeric_fields) {
    for (outcome in outcomes) {
      x <- as_numeric_clean(df[[field]])
      y <- as_numeric_clean(df[[outcome]])
      ok <- is.finite(x) & is.finite(y)
      rows[[index]] <- data.frame(prompt_version = unique(df$prompt_version)[1], model = unique(df$model)[1], score_scope = unique(df$score_scope)[1], image_input_mode = unique(df$image_input_mode)[1], field = field, outcome = outcome, n = sum(ok), pearson = safe_cor(x, y, "pearson"), spearman = safe_cor(x, y, "spearman"), stringsAsFactors = FALSE)
      index <- index + 1L
    }
  }
  do.call(rbind, rows)
}

auc_rows <- function(df, numeric_fields, labels) {
  rows <- list()
  index <- 1L
  for (field in numeric_fields) {
    for (label in labels) {
      score <- as_numeric_clean(df[[field]])
      actual <- as_logical_clean(df[[label]])
      pr <- precision_recall_at_top_fraction(actual, score, 0.20)
      rows[[index]] <- data.frame(prompt_version = unique(df$prompt_version)[1], model = unique(df$model)[1], score_scope = unique(df$score_scope)[1], image_input_mode = unique(df$image_input_mode)[1], field = field, label = label, n = pr[["n"]], auc = auc_rank(actual, score), precision_at_top20_predicted = pr[["precision"]], recall_at_top20_predicted = pr[["recall"]], stringsAsFactors = FALSE)
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
      rows[[index]] <- data.frame(prompt_version = unique(df$prompt_version)[1], model = unique(df$model)[1], score_scope = unique(df$score_scope)[1], image_input_mode = unique(df$image_input_mode)[1], field = field, score_value = score, n = sum(keep), mean_success_score = mean(df$success_score[keep], na.rm = TRUE), median_success_score = median(df$success_score[keep], na.rm = TRUE), top_20_percent_rate = mean(top20[keep], na.rm = TRUE), mean_log_claps = mean(df$log_claps[keep], na.rm = TRUE), mean_log_responses = mean(df$log_responses[keep], na.rm = TRUE), median_claps = median(df$claps[keep], na.rm = TRUE), median_responses = median(df$responses[keep], na.rm = TRUE), stringsAsFactors = FALSE)
      index <- index + 1L
    }
  }
  out <- do.call(rbind, rows)
  for (col in setdiff(names(out), c("prompt_version", "model", "score_scope", "image_input_mode", "field"))) out[[col]][!is.finite(out[[col]])] <- NA_real_
  out
}

predicted_bucket_rows <- function(df) {
  bucket <- tolower(clean_text(df$predicted_success_bucket))
  rows <- lapply(c("low", "medium", "high"), function(level) {
    keep <- !is.na(bucket) & bucket == level
    top20 <- as_logical_clean(df$top_20_percent)
    data.frame(prompt_version = unique(df$prompt_version)[1], model = unique(df$model)[1], score_scope = unique(df$score_scope)[1], image_input_mode = unique(df$image_input_mode)[1], predicted_success_bucket = level, n = sum(keep), mean_success_score = mean(df$success_score[keep], na.rm = TRUE), median_success_score = median(df$success_score[keep], na.rm = TRUE), top_20_percent_rate = mean(top20[keep], na.rm = TRUE), median_claps = median(df$claps[keep], na.rm = TRUE), median_responses = median(df$responses[keep], na.rm = TRUE), mean_log_claps = mean(df$log_claps[keep], na.rm = TRUE), mean_log_responses = mean(df$log_responses[keep], na.rm = TRUE), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  for (col in setdiff(names(out), c("prompt_version", "model", "score_scope", "image_input_mode", "predicted_success_bucket"))) out[[col]][!is.finite(out[[col]])] <- NA_real_
  out
}

example_columns <- function() c("prompt_version", "model", "score_scope", "image_input_mode", "canonical_article_key", "article_id", "medium_post_id", "title", "subtitle", "claps", "responses", "log_claps", "log_responses", "success_score", "top_20_percent", "overall_thumbnail_potential", "predicted_success_bucket", "short_reason")

example_rows <- function(df, score_direction, actual_direction, limit = 50) {
  score <- as_numeric_clean(df$overall_thumbnail_potential)
  actual <- as_numeric_clean(df$success_score)
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
  head(out[order(score_order[keep], actual_order[keep], na.last = TRUE), , drop = FALSE], limit)
}

example_rows_by_group <- function(groups, score_direction, actual_direction) {
  rows <- lapply(groups, example_rows, score_direction = score_direction, actual_direction = actual_direction)
  rows <- rows[vapply(rows, nrow, integer(1)) > 0]
  if (length(rows) == 0) return(as.data.frame(setNames(replicate(length(example_columns()), logical(0), simplify = FALSE), example_columns())))
  do.call(rbind, rows)
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

top_positive_cor_row <- function(correlations, outcome) {
  rows <- correlations[correlations$outcome == outcome & !is.na(correlations$spearman), , drop = FALSE]
  if (nrow(rows) == 0) return(rows)
  rows[order(-rows$spearman, rows$field), , drop = FALSE][1, , drop = FALSE]
}

top_positive_auc_row <- function(auc_table, label) {
  rows <- auc_table[auc_table$label == label & !is.na(auc_table$auc), , drop = FALSE]
  if (nrow(rows) == 0) return(rows)
  rows[order(-rows$auc, rows$field), , drop = FALSE][1, , drop = FALSE]
}

metric_value <- function(table, field, outcome_or_label, metric, outcome_col) {
  rows <- table[table$field == field & table[[outcome_col]] == outcome_or_label, , drop = FALSE]
  if (nrow(rows) == 0) return(NA_real_)
  rows[[metric]][1]
}

field_leaderboard_rows <- function(group_df, group_cor, group_auc, numeric_fields) {
  rows <- lapply(numeric_fields, function(field) {
    data.frame(
      prompt_version = unique(group_df$prompt_version)[1],
      model = unique(group_df$model)[1],
      score_scope = unique(group_df$score_scope)[1],
      image_input_mode = unique(group_df$image_input_mode)[1],
      field = field,
      n = sum(is.finite(as_numeric_clean(group_df[[field]]))),
      success_score_spearman = metric_value(group_cor, field, "success_score", "spearman", "outcome"),
      success_score_pearson = metric_value(group_cor, field, "success_score", "pearson", "outcome"),
      log_claps_spearman = metric_value(group_cor, field, "log_claps", "spearman", "outcome"),
      log_claps_pearson = metric_value(group_cor, field, "log_claps", "pearson", "outcome"),
      log_responses_spearman = metric_value(group_cor, field, "log_responses", "spearman", "outcome"),
      log_responses_pearson = metric_value(group_cor, field, "log_responses", "pearson", "outcome"),
      top20_auc = metric_value(group_auc, field, "top_20_percent", "auc", "label"),
      top10_auc = metric_value(group_auc, field, "top_10_percent", "auc", "label"),
      top5_auc = metric_value(group_auc, field, "top_5_percent", "auc", "label"),
      precision_at_top20_predicted = metric_value(group_auc, field, "top_20_percent", "precision_at_top20_predicted", "label"),
      recall_at_top20_predicted = metric_value(group_auc, field, "top_20_percent", "recall_at_top20_predicted", "label"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

predicted_bucket_spread <- function(group_bucket) {
  low <- group_bucket[group_bucket$predicted_success_bucket == "low", "top_20_percent_rate"]
  high <- group_bucket[group_bucket$predicted_success_bucket == "high", "top_20_percent_rate"]
  if (length(low) == 0 || length(high) == 0 || !is.finite(low[1]) || !is.finite(high[1])) return(NA_real_)
  high[1] - low[1]
}

scope_comparison_rows <- function(group_df, group_cor, group_auc, group_bucket) {
  best_success <- top_positive_cor_row(group_cor, "success_score")
  best_claps <- top_positive_cor_row(group_cor, "log_claps")
  best_responses <- top_positive_cor_row(group_cor, "log_responses")
  best_top20 <- top_positive_auc_row(group_auc, "top_20_percent")
  data.frame(
    prompt_version = unique(group_df$prompt_version)[1],
    model = unique(group_df$model)[1],
    score_scope = unique(group_df$score_scope)[1],
    image_input_mode = unique(group_df$image_input_mode)[1],
    n = length(unique(group_df$canonical_article_key)),
    best_field_success_score = ifelse(nrow(best_success) == 0, NA_character_, best_success$field[1]),
    best_success_score_spearman = ifelse(nrow(best_success) == 0, NA_real_, best_success$spearman[1]),
    best_success_score_pearson = ifelse(nrow(best_success) == 0, NA_real_, best_success$pearson[1]),
    best_field_log_claps = ifelse(nrow(best_claps) == 0, NA_character_, best_claps$field[1]),
    best_log_claps_spearman = ifelse(nrow(best_claps) == 0, NA_real_, best_claps$spearman[1]),
    best_log_claps_pearson = ifelse(nrow(best_claps) == 0, NA_real_, best_claps$pearson[1]),
    best_field_log_responses = ifelse(nrow(best_responses) == 0, NA_character_, best_responses$field[1]),
    best_log_responses_spearman = ifelse(nrow(best_responses) == 0, NA_real_, best_responses$spearman[1]),
    best_log_responses_pearson = ifelse(nrow(best_responses) == 0, NA_real_, best_responses$pearson[1]),
    best_top20_auc_field = ifelse(nrow(best_top20) == 0, NA_character_, best_top20$field[1]),
    best_top20_auc = ifelse(nrow(best_top20) == 0, NA_real_, best_top20$auc[1]),
    best_top20_precision_at_top20 = ifelse(nrow(best_top20) == 0, NA_real_, best_top20$precision_at_top20_predicted[1]),
    overall_thumbnail_potential_success_spearman = metric_value(group_cor, "overall_thumbnail_potential", "success_score", "spearman", "outcome"),
    overall_thumbnail_potential_log_claps_spearman = metric_value(group_cor, "overall_thumbnail_potential", "log_claps", "spearman", "outcome"),
    overall_thumbnail_potential_log_responses_spearman = metric_value(group_cor, "overall_thumbnail_potential", "log_responses", "spearman", "outcome"),
    overall_thumbnail_potential_top20_auc = metric_value(group_auc, "overall_thumbnail_potential", "top_20_percent", "auc", "label"),
    predicted_bucket_top20_spread = predicted_bucket_spread(group_bucket),
    stringsAsFactors = FALSE
  )
}

best_scope_sentence <- function(scope_comparison, metric, label) {
  rows <- scope_comparison[!is.na(scope_comparison[[metric]]), , drop = FALSE]
  if (nrow(rows) == 0) return(paste0(label, ": not enough variation to calculate."))
  rows <- rows[order(-rows[[metric]], rows$score_scope), , drop = FALSE]
  paste0(label, ": ", rows$score_scope[1], " with ", metric, " ", format_number(rows[[metric]][1]), ".")
}

scope_metric <- function(scope_comparison, scope, metric) {
  rows <- scope_comparison[scope_comparison$score_scope == scope & !is.na(scope_comparison[[metric]]), , drop = FALSE]
  if (nrow(rows) == 0) return(NA_real_)
  rows[[metric]][1]
}

beat_sentence <- function(scope_comparison, lhs, rhs, metric, label) {
  lhs_value <- scope_metric(scope_comparison, lhs, metric)
  rhs_value <- scope_metric(scope_comparison, rhs, metric)
  if (!is.finite(lhs_value) || !is.finite(rhs_value)) return(paste0(label, ": not enough comparable variation to tell."))
  if (lhs_value > rhs_value) {
    paste0(label, ": yes, ", lhs, " appears stronger than ", rhs, " on ", metric, " (", format_number(lhs_value), " vs ", format_number(rhs_value), ").")
  } else if (lhs_value < rhs_value) {
    paste0(label, ": no, ", rhs, " appears stronger than ", lhs, " on ", metric, " (", format_number(rhs_value), " vs ", format_number(lhs_value), ").")
  } else {
    paste0(label, ": tied on ", metric, " (", format_number(lhs_value), ").")
  }
}

title_score_context <- function(connection, sample_keys) {
  if (!dbExistsTable(connection, "medium_title_api_scores")) return("medium_title_api_scores table not found.")
  sample_filter <- ""
  if (length(sample_keys) > 0) {
    sample_sql <- paste(sprintf("'%s'", gsub("'", "''", sample_keys)), collapse = ",")
    sample_filter <- paste0(" AND canonical_article_key IN (", sample_sql, ")")
  }
  rows <- dbGetQuery(connection, paste0("
    SELECT prompt_version, model, COALESCE(NULLIF(TRIM(score_scope), ''), 'title_subtitle') AS score_scope, COUNT(DISTINCT canonical_article_key) AS n
    FROM medium_title_api_scores
    WHERE prompt_version = 'v2_2'
      AND COALESCE(NULLIF(TRIM(score_scope), ''), 'title_subtitle') IN ('title_only', 'title_subtitle')
      AND (error IS NULL OR TRIM(error) = '')
      ", sample_filter, "
    GROUP BY prompt_version, model, score_scope
    ORDER BY score_scope, model
  "))
  if (nrow(rows) == 0) return("No v2_2 title_only/title_subtitle API scores found on this sample for direct comparison.")
  paste(c("Existing title API scores available on this sample:", capture.output(print(rows, row.names = FALSE))), collapse = "\n")
}

title_context_table <- function(connection, sample_keys) {
  columns <- c("method_type", "scope", "prompt_version", "model", "n", "best_success_score_spearman", "best_log_claps_spearman", "best_log_responses_spearman", "best_top20_auc")
  empty <- as.data.frame(setNames(replicate(length(columns), logical(0), simplify = FALSE), columns))
  if (!dbExistsTable(connection, "medium_title_api_scores")) return(empty)
  sample_filter <- ""
  if (length(sample_keys) > 0) {
    sample_sql <- paste(sprintf("'%s'", gsub("'", "''", sample_keys)), collapse = ",")
    sample_filter <- paste0(" AND t.canonical_article_key IN (", sample_sql, ")")
  }
  rows <- dbGetQuery(connection, paste0("
    WITH latest_scores AS (
      SELECT *
      FROM (
        SELECT
          t.*,
          ROW_NUMBER() OVER (
            PARTITION BY t.canonical_article_key, t.prompt_version, t.model, COALESCE(NULLIF(TRIM(t.score_scope), ''), 'title_subtitle'), COALESCE(t.title_hash, ''), COALESCE(t.subtitle_hash, '')
            ORDER BY t.scored_at DESC, t.id DESC
          ) AS rn
        FROM medium_title_api_scores t
        WHERE t.prompt_version = 'v2_2'
          AND COALESCE(NULLIF(TRIM(t.score_scope), ''), 'title_subtitle') IN ('title_only', 'title_subtitle')
          AND (t.error IS NULL OR TRIM(t.error) = '')
          ", sample_filter, "
      )
      WHERE rn = 1
    )
    SELECT
      t.prompt_version,
      t.model,
      COALESCE(NULLIF(TRIM(t.score_scope), ''), 'title_subtitle') AS scope,
      t.canonical_article_key,
      d.success_score,
      d.log_claps,
      d.log_responses,
      d.top_20_percent,
      t.overall_article_potential,
      t.medium_clap_potential,
      t.medium_comment_potential,
      t.click_potential,
      t.curiosity,
      t.specificity,
      t.clarity,
      t.emotional_pull,
      t.credibility,
      t.beginner_appeal,
      t.promise_strength,
      t.trust_risk
    FROM latest_scores t
    JOIN v_medium_title_prediction_dataset_v2 d
      ON d.canonical_article_key = t.canonical_article_key
  "))
  if (nrow(rows) == 0) return(empty)
  title_fields <- intersect(c("overall_article_potential", "medium_clap_potential", "medium_comment_potential", "click_potential", "curiosity", "specificity", "clarity", "emotional_pull", "credibility", "beginner_appeal", "promise_strength", "trust_risk"), names(rows))
  title_groups <- split(rows, paste(rows$prompt_version, rows$model, rows$scope, sep = " / "), drop = TRUE)
  out <- lapply(title_groups, function(group_df) {
    scores <- lapply(title_fields, function(field) {
      x <- as_numeric_clean(group_df[[field]])
      data.frame(
        field = field,
        success_score_spearman = safe_cor(x, as_numeric_clean(group_df$success_score), "spearman"),
        log_claps_spearman = safe_cor(x, as_numeric_clean(group_df$log_claps), "spearman"),
        log_responses_spearman = safe_cor(x, as_numeric_clean(group_df$log_responses), "spearman"),
        top20_auc = auc_rank(group_df$top_20_percent, x),
        stringsAsFactors = FALSE
      )
    })
    score_df <- do.call(rbind, scores)
    data.frame(
      method_type = "title_api",
      scope = unique(group_df$scope)[1],
      prompt_version = unique(group_df$prompt_version)[1],
      model = unique(group_df$model)[1],
      n = length(unique(group_df$canonical_article_key)),
      best_success_score_spearman = suppressWarnings(max(score_df$success_score_spearman, na.rm = TRUE)),
      best_log_claps_spearman = suppressWarnings(max(score_df$log_claps_spearman, na.rm = TRUE)),
      best_log_responses_spearman = suppressWarnings(max(score_df$log_responses_spearman, na.rm = TRUE)),
      best_top20_auc = suppressWarnings(max(score_df$top20_auc, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  for (col in c("best_success_score_spearman", "best_log_claps_spearman", "best_log_responses_spearman", "best_top20_auc")) out[[col]][!is.finite(out[[col]])] <- NA_real_
  out
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
database_path <- args$db
base_output_dir <- file.path("data", "analysis", "thumbnail_api_scores_v1")
prompt_version_filter <- if (tolower(args$prompt_version) %in% c("all", "*")) NA_character_ else args$prompt_version
scope_filter <- if (tolower(args$scope) %in% c("all", "*")) NA_character_ else args$scope
run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_method_key <- method_key(prompt_version_filter, args$model, scope_filter, args$sample_file)
output_dirs <- selected_output_dirs(base_output_dir, args$output_mode, output_method_key, run_timestamp)

message("Medium Thumbnail API Scores V1 Evaluation")
message("=========================================")
message("DB path: ", database_path)
message("Output mode: ", args$output_mode)
message("Method key: ", output_method_key)
message("Output dirs: ", paste(output_dirs, collapse = "; "))
message("Prompt version: ", ifelse(is.na(prompt_version_filter), "all", prompt_version_filter))
message("Model: ", ifelse(is.na(args$model), "all", args$model))
message("Scope: ", ifelse(is.na(scope_filter), "all", scope_filter))
message("Sample file: ", ifelse(is.na(args$sample_file), "none", args$sample_file))

if (!file.exists(database_path)) stop("Could not find database at: ", database_path, call. = FALSE)
for (dir in output_dirs) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
sample_keys <- sample_keys_from_file(args$sample_file)

connection <- dbConnect(SQLite(), database_path, flags = SQLITE_RO)
on.exit(dbDisconnect(connection), add = TRUE)

required_objects <- c("medium_thumbnail_api_scores", "v_medium_title_prediction_dataset_v2")
missing_objects <- required_objects[!vapply(required_objects, dbExistsTable, logical(1), conn = connection)]
if (length(missing_objects) > 0) {
  write_empty_outputs_all(output_dirs, paste("Missing required DB object(s):", paste(missing_objects, collapse = ", ")), run_timestamp, args, prompt_version_filter, scope_filter, database_path, output_method_key)
  stop("Missing required DB object(s): ", paste(missing_objects, collapse = ", "), call. = FALSE)
}

available_counts <- dbGetQuery(connection, "
  SELECT prompt_version, model, score_scope, image_input_mode, COUNT(*) AS n
  FROM medium_thumbnail_api_scores
  GROUP BY prompt_version, model, score_scope, image_input_mode
  ORDER BY prompt_version, model, score_scope, image_input_mode
")
dataset_rows <- dbGetQuery(connection, "SELECT COUNT(*) AS n FROM v_medium_title_prediction_dataset_v2")$n[[1]]

sql <- "
WITH latest_scores AS (
  SELECT *
  FROM (
    SELECT
      s.*,
      ROW_NUMBER() OVER (
        PARTITION BY s.canonical_article_key, s.prompt_version, s.model, s.score_scope, COALESCE(s.image_hash, ''), COALESCE(s.image_url, ''), COALESCE(s.title_hash, ''), COALESCE(s.subtitle_hash, '')
        ORDER BY s.scored_at DESC, s.id DESC
      ) AS rn
    FROM medium_thumbnail_api_scores s
    WHERE (? IS NULL OR s.prompt_version = ?)
      AND (? IS NULL OR s.model = ?)
      AND (? IS NULL OR s.score_scope = ?)
      AND (s.error IS NULL OR TRIM(s.error) = '')
  )
  WHERE rn = 1
)
SELECT
  s.prompt_version,
  s.model,
  s.score_scope,
  s.image_input_mode,
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
  s.visual_clarity,
  s.visual_hook,
  s.visual_relevance,
  s.visual_distinctiveness,
  s.professional_credibility,
  s.emotional_pull_visual,
  s.finance_topic_fit,
  s.generic_stock_photo_risk,
  s.ai_or_low_quality_risk,
  s.text_readability,
  s.overall_thumbnail_potential,
  s.predicted_success_bucket,
  s.short_reason
FROM latest_scores s
JOIN v_medium_title_prediction_dataset_v2 d
  ON d.canonical_article_key = s.canonical_article_key
ORDER BY s.prompt_version, s.model, s.score_scope, s.image_input_mode, s.canonical_article_key
"

scored <- dbGetQuery(connection, sql, params = list(prompt_version_filter, prompt_version_filter, args$model, args$model, scope_filter, scope_filter))
if (length(sample_keys) > 0) {
  before_sample_filter <- nrow(scored)
  scored <- scored[scored$canonical_article_key %in% sample_keys, , drop = FALSE]
  message("Sample-file matched rows: ", nrow(scored), " of ", before_sample_filter, " joined scored rows.")
}
message("Usable joined scored rows: ", nrow(scored))

if (nrow(scored) == 0) {
  reason <- paste0("No usable joined thumbnail API score rows found for prompt_version=", ifelse(is.na(prompt_version_filter), "all", prompt_version_filter), ifelse(is.na(args$model), " and all models", paste0(" and model=", args$model)), ifelse(is.na(scope_filter), " and all scopes", paste0(" and scope=", scope_filter)), ifelse(length(sample_keys) > 0, paste0(" within sample_file=", args$sample_file, "."), "."))
  write_empty_outputs_all(output_dirs, reason, run_timestamp, args, prompt_version_filter, scope_filter, database_path, output_method_key)
  quit(status = 0)
}

numeric_fields <- c("visual_clarity", "visual_hook", "visual_relevance", "visual_distinctiveness", "professional_credibility", "emotional_pull_visual", "finance_topic_fit", "generic_stock_photo_risk", "ai_or_low_quality_risk", "text_readability", "overall_thumbnail_potential")
outcomes <- c("claps", "responses", "log_claps", "log_responses", "success_score")
labels <- c("top_20_percent", "top_10_percent", "top_5_percent", "over_50_claps", "over_100_claps", "over_200_claps")

for (field in numeric_fields) scored[[field]] <- as_numeric_clean(scored[[field]])
scored <- ensure_outcomes(scored)
groups <- split(scored, group_id(scored), drop = TRUE)

distribution <- do.call(rbind, lapply(groups, distribution_rows, numeric_fields = numeric_fields))
for (col in c("min", "max")) distribution[[col]][!is.finite(distribution[[col]])] <- NA_real_
correlations <- do.call(rbind, lapply(groups, correlation_rows, numeric_fields = numeric_fields, outcomes = outcomes))
auc_table <- do.call(rbind, lapply(groups, auc_rows, numeric_fields = numeric_fields, labels = labels))
bucket_diagnostics <- do.call(rbind, lapply(groups, bucket_rows, numeric_fields = numeric_fields))
predicted_bucket_diagnostics <- do.call(rbind, lapply(groups, predicted_bucket_rows))

field_leaderboard <- do.call(rbind, lapply(names(groups), function(group_name) {
  group_df <- groups[[group_name]]
  group_cor <- correlations[group_id(correlations) == group_name, , drop = FALSE]
  group_auc <- auc_table[group_id(auc_table) == group_name, , drop = FALSE]
  field_leaderboard_rows(group_df, group_cor, group_auc, numeric_fields)
}))
field_leaderboard <- field_leaderboard[order(field_leaderboard$score_scope, -field_leaderboard$success_score_spearman, field_leaderboard$field, na.last = TRUE), , drop = FALSE]
field_leaderboard_overall_sorted <- field_leaderboard[order(-field_leaderboard$success_score_spearman, -field_leaderboard$log_claps_spearman, -field_leaderboard$log_responses_spearman, field_leaderboard$score_scope, field_leaderboard$field, na.last = TRUE), , drop = FALSE]

scope_comparison <- do.call(rbind, lapply(names(groups), function(group_name) {
  group_df <- groups[[group_name]]
  group_cor <- correlations[group_id(correlations) == group_name, , drop = FALSE]
  group_auc <- auc_table[group_id(auc_table) == group_name, , drop = FALSE]
  group_bucket <- predicted_bucket_diagnostics[group_id(predicted_bucket_diagnostics) == group_name, , drop = FALSE]
  scope_comparison_rows(group_df, group_cor, group_auc, group_bucket)
}))
scope_comparison <- scope_comparison[order(match(scope_comparison$score_scope, c("thumbnail_only", "title_thumbnail", "title_subtitle_thumbnail")), scope_comparison$model, scope_comparison$image_input_mode, na.last = TRUE), , drop = FALSE]

thumbnail_context <- data.frame(
  method_type = "thumbnail_api",
  scope = scope_comparison$score_scope,
  prompt_version = scope_comparison$prompt_version,
  model = scope_comparison$model,
  n = scope_comparison$n,
  best_success_score_spearman = scope_comparison$best_success_score_spearman,
  best_log_claps_spearman = scope_comparison$best_log_claps_spearman,
  best_log_responses_spearman = scope_comparison$best_log_responses_spearman,
  best_top20_auc = scope_comparison$best_top20_auc,
  stringsAsFactors = FALSE
)
title_context <- title_context_table(connection, sample_keys)
thumbnail_vs_title_context <- rbind(thumbnail_context, title_context)

for (col in example_columns()) if (!(col %in% names(scored))) scored[[col]] <- NA

output_files <- list(
  "thumbnail_score_distribution.csv" = distribution,
  "thumbnail_score_correlations.csv" = correlations,
  "thumbnail_score_auc.csv" = auc_table,
  "thumbnail_scope_comparison.csv" = scope_comparison,
  "thumbnail_field_leaderboard.csv" = field_leaderboard,
  "thumbnail_field_leaderboard_overall_sorted.csv" = field_leaderboard_overall_sorted,
  "thumbnail_vs_title_context.csv" = thumbnail_vs_title_context,
  "thumbnail_score_bucket_diagnostics.csv" = bucket_diagnostics,
  "thumbnail_predicted_success_bucket_diagnostics.csv" = predicted_bucket_diagnostics,
  "thumbnail_false_positives_overall.csv" = example_rows_by_group(groups, "high", "low"),
  "thumbnail_false_negatives_overall.csv" = example_rows_by_group(groups, "low", "high")
)

group_counts <- aggregate(canonical_article_key ~ prompt_version + model + score_scope + image_input_mode, scored, length)
names(group_counts)[names(group_counts) == "canonical_article_key"] <- "usable_rows"

summary_lines <- c(
  "Medium Thumbnail API Scores V1 Evaluation",
  "=========================================",
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
  "Available thumbnail API score counts in DB:",
  if (nrow(available_counts) == 0) "  No thumbnail API scores found." else paste(capture.output(print(available_counts, row.names = FALSE)), collapse = "\n"),
  "",
  "Usable joined rows analyzed:",
  paste(capture.output(print(group_counts, row.names = FALSE)), collapse = "\n"),
  ""
)

total_rows <- nrow(scored)
if (total_rows < args$min_rows) {
  summary_lines <- c(summary_lines, paste0("WARNING: sample is below --min-rows=", args$min_rows, ". Treat all diagnostics as smoke-test output."))
} else if (total_rows < 100) {
  summary_lines <- c(summary_lines, "WARNING: fewer than 100 rows. Directional signal only; avoid firm conclusions.")
} else {
  summary_lines <- c(summary_lines, "Sample size is usable for a first diagnostic read, but still correlational.")
}
summary_lines <- c(summary_lines, "")

is_all_scope_run <- is.na(scope_filter)
if (is_all_scope_run && length(unique(scored$score_scope)) > 1) {
  scope_keys <- split(scored$canonical_article_key, scored$score_scope, drop = TRUE)
  scope_sets <- lapply(scope_keys, unique)
  common_keys <- Reduce(intersect, scope_sets)
  union_keys <- unique(unlist(scope_sets, use.names = FALSE))
  best_success_scope <- scope_comparison[order(-scope_comparison$best_success_score_spearman, scope_comparison$score_scope, na.last = TRUE), , drop = FALSE]
  best_claps_scope <- scope_comparison[order(-scope_comparison$best_log_claps_spearman, scope_comparison$score_scope, na.last = TRUE), , drop = FALSE]
  best_responses_scope <- scope_comparison[order(-scope_comparison$best_log_responses_spearman, scope_comparison$score_scope, na.last = TRUE), , drop = FALSE]
  strongest_field <- field_leaderboard_overall_sorted[!is.na(field_leaderboard_overall_sorted$success_score_spearman), , drop = FALSE]
  subtitle_delta <- scope_metric(scope_comparison, "title_subtitle_thumbnail", "best_success_score_spearman") - scope_metric(scope_comparison, "title_thumbnail", "best_success_score_spearman")
  subtitle_read <- if (!is.finite(subtitle_delta)) {
    "Adding subtitle: not enough comparable variation to tell in this run."
  } else if (subtitle_delta > 0) {
    paste0("Adding subtitle appears to help in this 100-row cohort on success_score Spearman by ", format_number(subtitle_delta), ", but this is correlational.")
  } else if (subtitle_delta < 0) {
    paste0("Adding subtitle appears to hurt in this 100-row cohort on success_score Spearman by ", format_number(abs(subtitle_delta)), ", but this is correlational.")
  } else {
    "Adding subtitle is tied with title_thumbnail on success_score Spearman in this run."
  }
  summary_lines <- c(
    summary_lines,
    "Scope comparison on the fixed cohort",
    "------------------------------------",
    paste("Canonical articles in sample:", ifelse(length(sample_keys) > 0, length(sample_keys), length(union_keys))),
    paste("Canonical articles with thumbnail scores in any compared scope:", length(union_keys)),
    paste("Scopes compared:", length(unique(scored$score_scope)), paste(sort(unique(scored$score_scope)), collapse = ", ")),
    paste("Scope overlap:", length(common_keys), "canonical articles are present in every compared scope."),
    best_scope_sentence(scope_comparison, "best_success_score_spearman", "Best scope for success_score"),
    best_scope_sentence(scope_comparison, "best_log_claps_spearman", "Best scope for log_claps"),
    best_scope_sentence(scope_comparison, "best_log_responses_spearman", "Best scope for log_responses"),
    if (nrow(strongest_field) == 0) {
      "Strongest field overall: not enough variation to calculate."
    } else {
      paste0("Strongest field overall: ", strongest_field$score_scope[1], " / ", strongest_field$field[1], " with success_score Spearman ", format_number(strongest_field$success_score_spearman[1]), ".")
    },
    beat_sentence(scope_comparison, "title_thumbnail", "thumbnail_only", "best_success_score_spearman", "Did title_thumbnail beat thumbnail_only?"),
    beat_sentence(scope_comparison, "title_subtitle_thumbnail", "title_thumbnail", "best_success_score_spearman", "Did title_subtitle_thumbnail beat title_thumbnail?"),
    subtitle_read,
    paste0("Scope-level additive signal cannot be proven from this report alone, but scope comparison on the same ", length(common_keys), " overlapping articles suggests ", best_success_scope$score_scope[1], " is currently strongest for success_score."),
    "Existing title API scores are available on the same sample when listed below, but this report does not yet fit a combined model. Treat title-vs-thumbnail comparisons as descriptive unless a combined/additive model is added.",
    "This is correlational and uses public claps/responses, not views/clicks; it is not proof of causality.",
    "",
    "Scope comparison table:",
    paste(capture.output(print(scope_comparison, row.names = FALSE)), collapse = "\n"),
    ""
  )
}

for (group_name in names(groups)) {
  group_df <- groups[[group_name]]
  group_cor <- correlations[group_id(correlations) == group_name, , drop = FALSE]
  group_dist <- distribution[group_id(distribution) == group_name, , drop = FALSE]
  group_bucket <- predicted_bucket_diagnostics[group_id(predicted_bucket_diagnostics) == group_name, , drop = FALSE]
  low_variance <- group_dist$field[!is.na(group_dist$sd) & group_dist$sd < 0.50]
  risk_rows <- group_cor[group_cor$field %in% c("generic_stock_photo_risk", "ai_or_low_quality_risk") & group_cor$outcome == "success_score", , drop = FALSE]
  summary_lines <- c(
    summary_lines,
    paste("Group:", group_name),
    paste("Rows:", nrow(group_df)),
    top_cor_line(group_cor, "success_score"),
    top_cor_line(group_cor, "log_claps"),
    top_cor_line(group_cor, "log_responses"),
    {
      best_group_success <- top_positive_cor_row(group_cor, "success_score")
      if (nrow(best_group_success) == 0) {
        "Within this scope, the strongest field for success_score could not be calculated because there were too few rows or too little variation."
      } else {
        paste0("Within this scope, the strongest field for success_score was ", best_group_success$field[1], " with Spearman ", format_number(best_group_success$spearman[1]), ".")
      }
    }
  )
  if (nrow(risk_rows) > 0) {
    risk_text <- paste(paste0(risk_rows$field, " Spearman ", vapply(risk_rows$spearman, format_number, character(1))), collapse = "; ")
    summary_lines <- c(summary_lines, paste("Risk fields vs success_score:", risk_text))
  }
  if (nrow(group_bucket) > 0 && any(group_bucket$n > 0)) {
    bucket_text <- paste(apply(group_bucket[group_bucket$n > 0, c("predicted_success_bucket", "n", "mean_success_score", "top_20_percent_rate"), drop = FALSE], 1, function(row) paste0(row[["predicted_success_bucket"]], ": n=", row[["n"]], ", mean_success=", format_number(as.numeric(row[["mean_success_score"]])), ", top20_rate=", format_number(as.numeric(row[["top_20_percent_rate"]])))), collapse = "; ")
    summary_lines <- c(summary_lines, paste("predicted_success_bucket separation:", bucket_text))
  } else {
    summary_lines <- c(summary_lines, "predicted_success_bucket separation: not enough rows.")
  }
  if (length(low_variance) > 0) summary_lines <- c(summary_lines, paste("Low-variance score fields to inspect:", paste(low_variance, collapse = ", ")))
  summary_lines <- c(summary_lines, "")
}

summary_lines <- c(
  summary_lines,
  "Existing title API comparison context:",
  title_score_context(connection, sample_keys),
  "",
  "Interpretation reminders:",
  "- thumbnail_only tests the image alone.",
  "- title_thumbnail and title_subtitle_thumbnail are feed-package scopes and should be compared on overlapping canonical articles.",
  "- This evaluation is correlational and uses public claps/responses, not views/clicks.",
  "- Small samples and one-row smoke tests cannot support conclusions about additive signal.",
  "",
  "Recommended next analysis:",
  "Fit simple combined models on the fixed cohort:",
  "- title API fields only",
  "- thumbnail API fields only",
  "- title API + thumbnail API",
  "- text lexical baseline if available on the same cohort",
  "",
  "This is needed to test whether thumbnail scores add signal beyond title scores.",
  "",
  "No OpenAI API calls are made by this script. SQLite is opened read-only and is not modified."
)

for (dir in output_dirs) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (name in names(output_files)) write.csv(output_files[[name]], file.path(dir, name), row.names = FALSE)
  writeLines(summary_lines, file.path(dir, "thumbnail_eval_summary.txt"))
  write_run_metadata(dir, run_timestamp, args, prompt_version_filter, scope_filter, nrow(scored), database_path, output_method_key, unique(scored$image_input_mode))
}

message("Wrote thumbnail API evaluation outputs:")
for (dir in output_dirs) message("  ", file.path(dir, "thumbnail_eval_summary.txt"))
