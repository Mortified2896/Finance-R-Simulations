input_path <- file.path("data", "analysis", "medium_title_prediction_dataset.csv")
audit_path <- file.path("data", "analysis", "subtitle_analysis", "subtitle_quality_audit.csv")
output_dir <- file.path("data", "analysis", "openai_headline_scoring")
score_file_candidates <- c(
  headline_v2 = file.path(output_dir, "openai_headline_scores_v2.csv"),
  headline_v1 = file.path(output_dir, "openai_headline_scores.csv")
)

args <- commandArgs(trailingOnly = TRUE)
requested_rubric_version <- NA_character_
for (arg in args) {
  if (grepl("^--rubric-version=", arg)) {
    requested_rubric_version <- sub("^--rubric-version=", "", arg)
  }
}

select_scores_path <- function(candidates, requested_version = NA_character_) {
  if (!is.na(requested_version) && requested_version != "") {
    if (!(requested_version %in% names(candidates))) {
      stop("Unsupported rubric version requested: ", requested_version, call. = FALSE)
    }
    return(candidates[[requested_version]])
  }
  for (version in names(candidates)) {
    path <- candidates[[version]]
    if (file.exists(path)) {
      return(path)
    }
  }
  candidates[[length(candidates)]]
}

scores_path <- select_scores_path(score_file_candidates, requested_rubric_version)

message("OpenAI Headline Scoring Analysis")
message("================================")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

clean_text_vector <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

as_logical_clean <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  value <- toupper(clean_text_vector(x))
  out <- rep(NA, length(value))
  out[value %in% c("TRUE", "T", "1", "YES")] <- TRUE
  out[value %in% c("FALSE", "F", "0", "NO")] <- FALSE
  out
}

normalize_text <- function(x) {
  value <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = " ")
  value <- tolower(value)
  value <- gsub("[^a-z0-9]+", " ", value)
  value <- gsub("\\s+", " ", value)
  trimws(value)
}

stopwords <- unique(c(
  "a", "about", "above", "after", "again", "against", "all", "am", "an", "and",
  "any", "are", "as", "at", "be", "because", "been", "before", "being", "below",
  "between", "both", "but", "by", "can", "could", "did", "do", "does", "doing",
  "down", "during", "each", "few", "for", "from", "further", "had", "has", "have",
  "having", "he", "her", "here", "hers", "herself", "him", "himself", "his", "how",
  "i", "if", "in", "into", "is", "it", "its", "itself", "just", "me", "more",
  "most", "my", "myself", "no", "nor", "not", "now", "of", "off", "on", "once",
  "only", "or", "other", "our", "ours", "ourselves", "out", "over", "own", "same",
  "she", "should", "so", "some", "such", "than", "that", "the", "their", "theirs",
  "them", "themselves", "then", "there", "these", "they", "this", "those", "through",
  "to", "too", "under", "until", "up", "very", "was", "we", "were", "what", "when",
  "where", "which", "while", "who", "whom", "why", "will", "with", "you", "your",
  "yours", "yourself", "yourselves", "medium", "story", "stories",
  "d", "ll", "m", "re", "s", "t", "ve"
))

split_words <- function(text) {
  normalized <- normalize_text(text)
  if (is.na(normalized) || normalized == "") {
    return(character())
  }
  words <- unlist(strsplit(normalized, " ", fixed = TRUE), use.names = FALSE)
  words <- words[nchar(words) > 1]
  words[!(words %in% stopwords)]
}

make_ngrams <- function(words, n) {
  if (length(words) < n) {
    return(character())
  }
  vapply(seq_len(length(words) - n + 1), function(i) paste(words[i:(i + n - 1)], collapse = " "), character(1))
}

term_sets_for_docs <- function(texts) {
  lapply(texts, function(text) {
    words <- split_words(text)
    unique(c(make_ngrams(words, 1), make_ngrams(words, 2), make_ngrams(words, 3)))
  })
}

contains_family <- function(term_sets, terms) {
  vapply(term_sets, function(article_terms) any(terms %in% article_terms), logical(1))
}

fill_numeric <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
}

make_basic_numeric_features <- function(text, prefix) {
  normalized <- normalize_text(text)
  split_text <- strsplit(ifelse(is.na(normalized), "", normalized), " ")
  word_count <- vapply(split_text, function(words) sum(words != ""), integer(1))
  data.frame(
    setNames(
      list(
        nchar(ifelse(is.na(text), "", text)),
        word_count,
        as.integer(grepl("[0-9]", ifelse(is.na(text), "", text))),
        as.integer(grepl("\\?", ifelse(is.na(text), "", text))),
        as.integer(grepl(":", ifelse(is.na(text), "", text))),
        as.integer(grepl("\\$", ifelse(is.na(text), "", text)))
      ),
      paste0(prefix, c("_char_count", "_word_count", "_has_number", "_has_question_mark", "_has_colon", "_has_dollar"))
    ),
    stringsAsFactors = FALSE
  )
}

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2, na.rm = TRUE))
mae <- function(actual, predicted) mean(abs(actual - predicted), na.rm = TRUE)

r_squared <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) < 2) {
    return(NA_real_)
  }
  suppressWarnings(cor(actual[ok], predicted[ok])^2)
}

auc_rank <- function(actual, score) {
  ok <- !is.na(actual) & !is.na(score)
  actual <- actual[ok]
  score <- score[ok]
  positives <- sum(actual)
  negatives <- sum(!actual)
  if (positives == 0 || negatives == 0) {
    return(NA_real_)
  }
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[actual]) - positives * (positives + 1) / 2) / (positives * negatives)
}

classification_metrics <- function(actual, probability, target_rate = 0.20) {
  ok <- !is.na(actual) & !is.na(probability)
  actual <- actual[ok]
  probability <- probability[ok]
  if (length(actual) == 0 || length(unique(actual)) < 2) {
    return(c(accuracy = NA_real_, precision = NA_real_, recall = NA_real_, auc = NA_real_))
  }
  cutoff <- as.numeric(stats::quantile(probability, probs = 1 - target_rate, na.rm = TRUE, type = 7))
  predicted <- probability >= cutoff
  true_positive <- sum(predicted & actual)
  false_positive <- sum(predicted & !actual)
  false_negative <- sum(!predicted & actual)
  c(
    accuracy = mean(predicted == actual),
    precision = ifelse(true_positive + false_positive > 0, true_positive / (true_positive + false_positive), NA_real_),
    recall = ifelse(true_positive + false_negative > 0, true_positive / (true_positive + false_negative), NA_real_),
    auc = auc_rank(actual, probability)
  )
}

fit_lm_safely <- function(x_train, y_train) {
  train <- data.frame(target = y_train, x_train, check.names = FALSE)
  tryCatch(stats::lm(target ~ ., data = train), error = function(error) NULL)
}

fit_glm_safely <- function(x_train, y_train) {
  if (length(unique(y_train)) < 2) {
    return(NULL)
  }
  train <- data.frame(top20 = y_train, x_train, check.names = FALSE)
  tryCatch(suppressWarnings(stats::glm(top20 ~ ., data = train, family = stats::binomial())), error = function(error) NULL)
}

predict_safely <- function(model, newdata, type = "response") {
  if (is.null(model)) {
    return(rep(NA_real_, nrow(newdata)))
  }
  tryCatch(
    suppressWarnings(as.numeric(stats::predict(model, newdata = newdata, type = type))),
    error = function(error) rep(NA_real_, nrow(newdata))
  )
}

evaluate_model <- function(target_name, model_name, model_group, x, target_value, top20_label, base_seed = 20260513) {
  complete <- stats::complete.cases(x) & !is.na(target_value) & !is.na(top20_label)
  row_index <- which(complete)
  if (length(row_index) < 30 || sum(top20_label[row_index]) < 5 || sum(!top20_label[row_index]) < 5) {
    return(list(
      metrics = data.frame(
        target = target_name,
        model = model_name,
        model_group = model_group,
        feature_count = ncol(x),
        usable_rows = length(row_index),
        train_rows = NA_integer_,
        test_rows = NA_integer_,
        regression_rmse = NA_real_,
        regression_mae = NA_real_,
        regression_r_squared = NA_real_,
        classification_accuracy = NA_real_,
        classification_precision_at_top20_predicted = NA_real_,
        classification_recall_at_top20_predicted = NA_real_,
        classification_auc = NA_real_,
        note = "insufficient scored rows or target classes for split evaluation",
        stringsAsFactors = FALSE
      ),
      pred_reg_all = rep(NA_real_, nrow(x)),
      pred_cls_all = rep(NA_real_, nrow(x))
    ))
  }

  set.seed(base_seed)
  train_index <- sort(sample(row_index, size = floor(0.80 * length(row_index))))
  test_index <- setdiff(row_index, train_index)
  if (length(test_index) < 5 || sum(top20_label[train_index]) < 3 || sum(!top20_label[train_index]) < 3) {
    return(list(
      metrics = data.frame(
        target = target_name,
        model = model_name,
        model_group = model_group,
        feature_count = ncol(x),
        usable_rows = length(row_index),
        train_rows = length(train_index),
        test_rows = length(test_index),
        regression_rmse = NA_real_,
        regression_mae = NA_real_,
        regression_r_squared = NA_real_,
        classification_accuracy = NA_real_,
        classification_precision_at_top20_predicted = NA_real_,
        classification_recall_at_top20_predicted = NA_real_,
        classification_auc = NA_real_,
        note = "insufficient split balance for evaluation",
        stringsAsFactors = FALSE
      ),
      pred_reg_all = rep(NA_real_, nrow(x)),
      pred_cls_all = rep(NA_real_, nrow(x))
    ))
  }

  x_train <- x[train_index, , drop = FALSE]
  x_test <- x[test_index, , drop = FALSE]
  lm_model <- fit_lm_safely(x_train, target_value[train_index])
  glm_model <- fit_glm_safely(x_train, top20_label[train_index])
  pred_reg_test <- predict_safely(lm_model, x_test, type = "response")
  pred_cls_test <- predict_safely(glm_model, x_test, type = "response")
  pred_reg_all <- predict_safely(lm_model, x, type = "response")
  pred_cls_all <- predict_safely(glm_model, x, type = "response")
  cls <- classification_metrics(top20_label[test_index], pred_cls_test)

  list(
    metrics = data.frame(
      target = target_name,
      model = model_name,
      model_group = model_group,
      feature_count = ncol(x),
      usable_rows = length(row_index),
      train_rows = length(train_index),
      test_rows = length(test_index),
      regression_rmse = rmse(target_value[test_index], pred_reg_test),
      regression_mae = mae(target_value[test_index], pred_reg_test),
      regression_r_squared = r_squared(target_value[test_index], pred_reg_test),
      classification_accuracy = cls["accuracy"],
      classification_precision_at_top20_predicted = cls["precision"],
      classification_recall_at_top20_predicted = cls["recall"],
      classification_auc = cls["auc"],
      note = "",
      stringsAsFactors = FALSE
    ),
    pred_reg_all = pred_reg_all,
    pred_cls_all = pred_cls_all
  )
}

empty_outputs <- function(reason) {
  write.csv(data.frame(), file.path(output_dir, "openai_score_correlations.csv"), row.names = FALSE)
  write.csv(data.frame(), file.path(output_dir, "openai_dimension_summary.csv"), row.names = FALSE)
  write.csv(data.frame(), file.path(output_dir, "openai_scored_articles_with_predictions.csv"), row.names = FALSE)
  writeLines(c(
    "OpenAI headline model comparison",
    "================================",
    reason
  ), file.path(output_dir, "openai_model_comparison.txt"))
  writeLines(c(
    "OpenAI headline scoring summary",
    "===============================",
    reason,
    "",
    "Run the scorer first, for example:",
    "python3 scripts/score_medium_headlines_openai.py --dry-run",
    "python3 scripts/score_medium_headlines_openai.py --sample 5 --limit 10"
  ), file.path(output_dir, "openai_headline_scoring_summary.txt"))
}

if (!file.exists(input_path)) {
  stop("Could not find analysis dataset at: ", input_path, call. = FALSE)
}
if (!file.exists(scores_path)) {
  empty_outputs("No OpenAI score CSV found yet.")
  quit(status = 0)
}

articles <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
scores <- read.csv(scores_path, stringsAsFactors = FALSE, check.names = FALSE)
if (nrow(scores) == 0) {
  empty_outputs("OpenAI score CSV exists but has no rows.")
  quit(status = 0)
}
if ("rubric_version" %in% names(scores)) {
  available_rubric_versions <- sort(unique(clean_text_vector(scores$rubric_version)))
  available_rubric_versions <- available_rubric_versions[!is.na(available_rubric_versions)]
  if (is.na(requested_rubric_version) || requested_rubric_version == "") {
    selected_rubric_version <- tail(available_rubric_versions, 1)
  } else {
    selected_rubric_version <- requested_rubric_version
  }
  scores <- scores[scores$rubric_version == selected_rubric_version, , drop = FALSE]
  if (nrow(scores) == 0) {
    empty_outputs(paste("No OpenAI score rows found for rubric_version:", selected_rubric_version))
    quit(status = 0)
  }
} else {
  selected_rubric_version <- "unknown"
  available_rubric_versions <- "unknown"
}

articles$article_id <- as.character(articles$article_id)
scores$article_id <- as.character(scores$article_id)
articles$title <- clean_text_vector(articles$title)
articles$subtitle_deck_text <- if ("subtitle" %in% names(articles)) clean_text_vector(articles$subtitle) else NA_character_
articles$latest_claps <- suppressWarnings(as.numeric(articles$latest_claps))
articles$latest_responses <- suppressWarnings(as.numeric(articles$latest_responses))
articles$log_claps <- ifelse(!is.na(articles$latest_claps), log1p(articles$latest_claps), NA_real_)
articles$log_responses <- ifelse(!is.na(articles$latest_responses), log1p(articles$latest_responses), NA_real_)
articles$success_score_current <- if ("success_score" %in% names(articles)) suppressWarnings(as.numeric(articles$success_score)) else NA_real_
articles$success_score_equal <- ifelse(
  !is.na(articles$log_claps) | !is.na(articles$log_responses),
  ifelse(is.na(articles$log_claps), 0, articles$log_claps) + ifelse(is.na(articles$log_responses), 0, articles$log_responses),
  NA_real_
)

term_families <- list(
  retirement_family = c("retire", "retires", "retired", "retiring", "retirement", "retiree", "retirees"),
  etf_family = c("etf", "etfs"),
  index_family = c("index", "indexes", "indexing", "index fund", "index funds"),
  portfolio_family = c("portfolio", "portfolios"),
  investing_family = c("invest", "invests", "invested", "investing", "investment", "investments", "investor", "investors"),
  crypto_family = c("crypto", "bitcoin", "ethereum", "blockchain", "web3"),
  ai_family = c("ai", "artificial intelligence", "chatgpt"),
  money_family = c("money", "wealth", "rich", "millionaire", "millionaires", "income"),
  mistake_family = c("mistake", "mistakes", "wrong", "avoid", "problem", "problems"),
  beginner_family = c("beginner", "beginners", "starting", "start"),
  retirement_fire_family = c("retirement", "retire", "retires", "retired", "retiring", "fire", "financial independence")
)

usable <- articles[!is.na(articles$title), , drop = FALSE]
combined_text <- paste(usable$title, ifelse(is.na(usable$subtitle_deck_text), "", usable$subtitle_deck_text))
combined_term_sets <- term_sets_for_docs(combined_text)
family_matrix <- data.frame(row.names = seq_len(nrow(usable)))
for (family_name in names(term_families)) {
  family_matrix[[paste0("combined_", family_name)]] <- as.integer(contains_family(combined_term_sets, term_families[[family_name]]))
}
baseline_features <- data.frame(
  make_basic_numeric_features(usable$title, "title"),
  make_basic_numeric_features(usable$subtitle_deck_text, "subtitle"),
  family_matrix,
  check.names = FALSE
)

score_columns <- grep("_score$", names(scores), value = TRUE)
dimension_names <- sub("_score$", "", score_columns)

make_scope_wide <- function(scores_data, scope, prefix) {
  scoped <- scores_data[scores_data$score_scope == scope, , drop = FALSE]
  if (nrow(scoped) == 0) {
    return(data.frame(article_id = character(), stringsAsFactors = FALSE))
  }
  scoped <- scoped[order(scoped$article_id, scoped$timestamp_utc), , drop = FALSE]
  scoped <- scoped[!duplicated(scoped$article_id, fromLast = TRUE), , drop = FALSE]
  out <- data.frame(article_id = scoped$article_id, stringsAsFactors = FALSE)
  for (col in score_columns) {
    out[[paste0(prefix, "_", col)]] <- suppressWarnings(as.numeric(scoped[[col]]))
  }
  out
}

pair_scores <- make_scope_wide(scores, "title_subtitle_pair", "openai_pair")
title_scores <- make_scope_wide(scores, "title_only", "openai_title")
scored <- merge(usable, pair_scores, by = "article_id", all.x = TRUE)
scored <- merge(scored, title_scores, by = "article_id", all.x = TRUE)
scored$openai_scored_any <- scored$article_id %in% unique(scores$article_id)

if (file.exists(audit_path)) {
  audit <- read.csv(audit_path, stringsAsFactors = FALSE, check.names = FALSE)
  audit$article_id <- as.character(audit$article_id)
  keep_audit <- intersect(
    c("article_id", "subtitle_missing", "subtitle_too_short", "subtitle_very_short", "subtitle_truncated", "subtitle_low_information"),
    names(audit)
  )
  scored <- merge(scored, audit[, keep_audit, drop = FALSE], by = "article_id", all.x = TRUE)
}

baseline_features <- baseline_features[match(scored$article_id, usable$article_id), , drop = FALSE]
row.names(baseline_features) <- NULL
baseline_features_scored_rows <- baseline_features
baseline_features_scored_rows[!scored$openai_scored_any, ] <- NA
pair_feature_cols <- grep("^openai_pair_.*_score$", names(scored), value = TRUE)
title_feature_cols <- grep("^openai_title_.*_score$", names(scored), value = TRUE)
all_openai_feature_cols <- c(pair_feature_cols, title_feature_cols)

feature_sets <- list()
feature_sets$current_prepublication_baseline = list(
  group = "A_baseline_without_openai",
  features = baseline_features_scored_rows
)
if (length(all_openai_feature_cols) > 0) {
  feature_sets$openai_scores_only = list(
    group = "B_openai_scores_only",
    features = scored[, all_openai_feature_cols, drop = FALSE]
  )
  feature_sets$baseline_plus_openai_scores = list(
    group = "C_baseline_plus_openai",
    features = data.frame(baseline_features_scored_rows, scored[, all_openai_feature_cols, drop = FALSE], check.names = FALSE)
  )
}
if (length(title_feature_cols) > 0) {
  feature_sets$title_only_openai_scores_only = list(
    group = "D_title_only_openai_only",
    features = scored[, title_feature_cols, drop = FALSE]
  )
}
if (length(pair_feature_cols) > 0) {
  feature_sets$title_subtitle_openai_scores_only = list(
    group = "E_title_subtitle_openai_only",
    features = scored[, pair_feature_cols, drop = FALSE]
  )
  feature_sets$baseline_plus_title_subtitle_openai_scores = list(
    group = "F_baseline_plus_title_subtitle_openai",
    features = data.frame(baseline_features_scored_rows, scored[, pair_feature_cols, drop = FALSE], check.names = FALSE)
  )
}

targets <- list(
  success_score = scored$success_score_current,
  success_score_equal = scored$success_score_equal,
  log_claps = scored$log_claps,
  log_responses = scored$log_responses
)

metrics_table <- data.frame()
for (target_name in names(targets)) {
  target_value <- suppressWarnings(as.numeric(targets[[target_name]]))
  valid <- !is.na(target_value)
  top20_label <- rep(NA, length(target_value))
  if (sum(valid) > 0) {
    cutoff <- as.numeric(stats::quantile(target_value[valid], probs = 0.80, na.rm = TRUE, type = 7))
    top20_label[valid] <- target_value[valid] >= cutoff
  }
  for (model_name in names(feature_sets)) {
    result <- evaluate_model(
      target_name,
      model_name,
      feature_sets[[model_name]]$group,
      feature_sets[[model_name]]$features,
      target_value,
      top20_label
    )
    metrics_table <- rbind(metrics_table, result$metrics)
    if (target_name == "success_score") {
      scored[[paste0("pred_success_", model_name)]] <- result$pred_reg_all
      scored[[paste0("pred_high_top20_", model_name)]] <- result$pred_cls_all
    }
  }
}

metrics_table <- metrics_table[order(metrics_table$target, metrics_table$model_group), , drop = FALSE]
writeLines(c(
  "OpenAI headline model comparison",
  "================================",
  "All models use only rows with available OpenAI scores required by that feature set.",
  "The baseline is title+subtitle/deck numeric features plus term-family indicators.",
  "",
  paste("Scored CSV rows:", nrow(scores)),
  paste("Scores file:", scores_path),
  paste("Rubric version analyzed:", selected_rubric_version),
  paste("Unique scored articles:", length(unique(scores$article_id))),
  paste("Title+subtitle scored articles:", nrow(pair_scores)),
  paste("Title-only scored articles:", nrow(title_scores)),
  "",
  capture.output(print(metrics_table, row.names = FALSE))
), file.path(output_dir, "openai_model_comparison.txt"))

cor_rows <- list()
for (scope_prefix in c("openai_pair", "openai_title")) {
  scope_cols <- grep(paste0("^", scope_prefix, "_.*_score$"), names(scored), value = TRUE)
  if (length(scope_cols) > 1) {
    for (i in seq_along(scope_cols)) {
      for (j in seq_along(scope_cols)) {
        if (j <= i) next
        x <- suppressWarnings(as.numeric(scored[[scope_cols[i]]]))
        y <- suppressWarnings(as.numeric(scored[[scope_cols[j]]]))
        ok <- !is.na(x) & !is.na(y)
        cor_rows[[length(cor_rows) + 1]] <- data.frame(
          correlation_type = "dimension_to_dimension",
          scope = sub("^openai_", "", scope_prefix),
          dimension_a = sub(paste0("^", scope_prefix, "_"), "", sub("_score$", "", scope_cols[i])),
          dimension_b = sub(paste0("^", scope_prefix, "_"), "", sub("_score$", "", scope_cols[j])),
          target = NA_character_,
          n = sum(ok),
          correlation = ifelse(sum(ok) >= 3, suppressWarnings(cor(x[ok], y[ok])), NA_real_),
          stringsAsFactors = FALSE
        )
      }
    }
    for (col in scope_cols) {
      x <- suppressWarnings(as.numeric(scored[[col]]))
      for (target_name in names(targets)) {
        y <- suppressWarnings(as.numeric(targets[[target_name]]))
        ok <- !is.na(x) & !is.na(y)
        cor_rows[[length(cor_rows) + 1]] <- data.frame(
          correlation_type = "dimension_to_target",
          scope = sub("^openai_", "", scope_prefix),
          dimension_a = sub(paste0("^", scope_prefix, "_"), "", sub("_score$", "", col)),
          dimension_b = NA_character_,
          target = target_name,
          n = sum(ok),
          correlation = ifelse(sum(ok) >= 3, suppressWarnings(cor(x[ok], y[ok])), NA_real_),
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
correlations <- if (length(cor_rows) > 0) do.call(rbind, cor_rows) else data.frame()
write.csv(correlations, file.path(output_dir, "openai_score_correlations.csv"), row.names = FALSE)

summary_rows <- list()
for (scope_prefix in c("openai_pair", "openai_title")) {
  scope_cols <- grep(paste0("^", scope_prefix, "_.*_score$"), names(scored), value = TRUE)
  for (col in scope_cols) {
    x <- suppressWarnings(as.numeric(scored[[col]]))
    dimension <- sub(paste0("^", scope_prefix, "_"), "", sub("_score$", "", col))
    success_ok <- !is.na(x) & !is.na(scored$success_score_current)
    response_ok <- !is.na(x) & !is.na(scored$log_responses)
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      scope = sub("^openai_", "", scope_prefix),
      dimension = dimension,
      n_scored = sum(!is.na(x)),
      mean_score = mean(x, na.rm = TRUE),
      sd_score = stats::sd(x, na.rm = TRUE),
      min_score = suppressWarnings(min(x, na.rm = TRUE)),
      max_score = suppressWarnings(max(x, na.rm = TRUE)),
      cor_success_score = ifelse(sum(success_ok) >= 3, suppressWarnings(cor(x[success_ok], scored$success_score_current[success_ok])), NA_real_),
      cor_log_responses = ifelse(sum(response_ok) >= 3, suppressWarnings(cor(x[response_ok], scored$log_responses[response_ok])), NA_real_),
      stringsAsFactors = FALSE
    )
  }
}
dimension_summary <- if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame()
write.csv(dimension_summary, file.path(output_dir, "openai_dimension_summary.csv"), row.names = FALSE)

write.csv(scored, file.path(output_dir, "openai_scored_articles_with_predictions.csv"), row.names = FALSE)

metric_value <- function(model, target, metric) {
  row <- metrics_table[metrics_table$model == model & metrics_table$target == target, , drop = FALSE]
  if (nrow(row) == 0 || !(metric %in% names(row))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(row[[metric]][1]))
}

best_dims <- dimension_summary[dimension_summary$scope == "pair" & !is.na(dimension_summary$cor_success_score), , drop = FALSE]
if (nrow(best_dims) > 0) {
  best_dims <- best_dims[order(-abs(best_dims$cor_success_score)), , drop = FALSE]
}
redundant_pairs <- correlations[
  correlations$correlation_type == "dimension_to_dimension" &
    !is.na(correlations$correlation) &
    abs(correlations$correlation) >= 0.80,
  ,
  drop = FALSE
]

baseline_auc <- metric_value("current_prepublication_baseline", "success_score", "classification_auc")
openai_auc <- metric_value("openai_scores_only", "success_score", "classification_auc")
combined_auc <- metric_value("baseline_plus_openai_scores", "success_score", "classification_auc")
title_auc <- metric_value("title_only_openai_scores_only", "success_score", "classification_auc")
pair_auc <- metric_value("title_subtitle_openai_scores_only", "success_score", "classification_auc")

summary_lines <- c(
  "OpenAI headline scoring summary",
  "===============================",
  paste("Scored CSV rows:", nrow(scores)),
  paste("Scores file:", scores_path),
  paste("Rubric version analyzed:", selected_rubric_version),
  paste("Unique scored articles:", length(unique(scores$article_id))),
  paste("Title+subtitle scored articles:", nrow(pair_scores)),
  paste("Title-only scored articles:", nrow(title_scores)),
  "",
  "Do OpenAI scores alone predict performance better than the existing text baseline?",
  paste("Baseline success-score AUC:", ifelse(is.na(baseline_auc), "NA", round(baseline_auc, 3))),
  paste("OpenAI-scores-only success-score AUC:", ifelse(is.na(openai_auc), "NA", round(openai_auc, 3))),
  "Treat this as inconclusive when the scored sample is small or split metrics are NA.",
  "",
  "Do OpenAI scores add incremental lift over title+subtitle term-family features?",
  paste("Baseline + OpenAI success-score AUC:", ifelse(is.na(combined_auc), "NA", round(combined_auc, 3))),
  "Compare against the baseline on the same scored-row pilot before scaling.",
  "",
  "Which dimensions appear useful?",
  if (nrow(best_dims) == 0) {
    "Not enough scored rows yet to estimate dimension usefulness."
  } else {
    paste(head(paste0(best_dims$dimension, " (cor=", round(best_dims$cor_success_score, 3), ")"), 6), collapse = "; ")
  },
  "",
  "Which dimensions look redundant or noisy?",
  if (nrow(redundant_pairs) == 0) {
    "No dimension pairs crossed |correlation| >= 0.80 in the current scored sample, or the sample is too small."
  } else {
    paste(head(paste0(redundant_pairs$dimension_a, " / ", redundant_pairs$dimension_b, " (cor=", round(redundant_pairs$correlation, 3), ")"), 8), collapse = "; ")
  },
  "",
  "Does title+subtitle scoring beat title-only scoring?",
  paste("Title-only OpenAI AUC:", ifelse(is.na(title_auc), "NA", round(title_auc, 3))),
  paste("Title+subtitle OpenAI AUC:", ifelse(is.na(pair_auc), "NA", round(pair_auc, 3))),
  "",
  "Should we scale from pilot sample to all articles?",
  ifelse(
    nrow(pair_scores) < 100 || is.na(pair_auc),
    "Not yet. First score a larger pilot sample, roughly 200-300 articles across both scopes, and inspect stability.",
    "Maybe. Scale only if lift is stable against the baseline and the useful dimensions are interpretable."
  ),
  "",
  "Which rubric dimensions should be kept, merged, or dropped in v2?",
  "Keep/merge/drop decisions should wait for the pilot to reach enough rows for stable correlations. Strongly redundant pairs are listed above as merge candidates; low-variance or near-zero target-correlation dimensions are drop candidates."
)
writeLines(summary_lines, file.path(output_dir, "openai_headline_scoring_summary.txt"))

message("Saved OpenAI headline scoring analysis outputs to: ", output_dir)
