input_path <- file.path("data", "analysis", "medium_analysis_v1", "medium_title_prediction_dataset.csv")
output_dir <- file.path("data", "analysis", "medium_analysis_v1", "openai_headline_scoring")
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

message("OpenAI Headline Scoring Regularized/Repeated Analysis")
message("====================================================")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

comparison_csv <- file.path(output_dir, "openai_regularized_model_comparison.csv")
comparison_txt <- file.path(output_dir, "openai_regularized_model_comparison.txt")
importance_csv <- file.path(output_dir, "openai_regularized_feature_importance.csv")
summary_txt <- file.path(output_dir, "openai_regularized_summary.txt")

repeat_count <- 20
train_fraction <- 0.80
base_seed <- 20260513

clean_text_vector <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
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

prepare_matrix <- function(features) {
  features <- as.data.frame(features, check.names = FALSE)
  for (col in names(features)) {
    features[[col]] <- suppressWarnings(as.numeric(features[[col]]))
  }
  features
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
  tryCatch(
    suppressWarnings(stats::glm(top20 ~ ., data = train, family = stats::binomial())),
    error = function(error) NULL
  )
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

make_split <- function(row_index, top20_label, repeat_id) {
  set.seed(base_seed + repeat_id)
  positives <- row_index[top20_label[row_index]]
  negatives <- row_index[!top20_label[row_index]]
  train_pos <- sample(positives, size = max(1, floor(train_fraction * length(positives))))
  train_neg <- sample(negatives, size = max(1, floor(train_fraction * length(negatives))))
  train_index <- sort(c(train_pos, train_neg))
  test_index <- setdiff(row_index, train_index)
  list(train = train_index, test = test_index)
}

metric_row <- function(target_name, feature_set_name, model_group, method, alpha, repeat_id, feature_count, usable_rows, train_rows, test_rows, y_test, top20_test, pred_reg, pred_cls, note = "") {
  cls <- classification_metrics(top20_test, pred_cls)
  data.frame(
    target = target_name,
    feature_set = feature_set_name,
    model_group = model_group,
    method = method,
    alpha = alpha,
    repeat_id = repeat_id,
    feature_count = feature_count,
    usable_rows = usable_rows,
    train_rows = train_rows,
    test_rows = test_rows,
    auc = cls["auc"],
    precision_at_predicted_top20 = cls["precision"],
    recall_at_predicted_top20 = cls["recall"],
    accuracy = cls["accuracy"],
    rmse = rmse(y_test, pred_reg),
    mae = mae(y_test, pred_reg),
    r_squared = r_squared(y_test, pred_reg),
    note = note,
    stringsAsFactors = FALSE
  )
}

evaluate_repeated_simple <- function(target_name, feature_set_name, model_group, features, target_value, top20_label) {
  x <- prepare_matrix(features)
  complete <- stats::complete.cases(x) & !is.na(target_value) & !is.na(top20_label)
  row_index <- which(complete)
  if (length(row_index) < 40 || sum(top20_label[row_index]) < 8 || sum(!top20_label[row_index]) < 8) {
    return(data.frame())
  }

  rows <- list()
  for (repeat_id in seq_len(repeat_count)) {
    split <- make_split(row_index, top20_label, repeat_id)
    train_index <- split$train
    test_index <- split$test
    if (length(test_index) < 5 || length(unique(top20_label[test_index])) < 2) {
      next
    }
    lm_model <- fit_lm_safely(x[train_index, , drop = FALSE], target_value[train_index])
    glm_model <- fit_glm_safely(x[train_index, , drop = FALSE], top20_label[train_index])
    pred_reg <- predict_safely(lm_model, x[test_index, , drop = FALSE], type = "response")
    pred_cls <- predict_safely(glm_model, x[test_index, , drop = FALSE], type = "response")
    rows[[length(rows) + 1]] <- metric_row(
      target_name, feature_set_name, model_group, "simple_repeated_split", NA_real_, repeat_id,
      ncol(x), length(row_index), length(train_index), length(test_index),
      target_value[test_index], top20_label[test_index], pred_reg, pred_cls
    )
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

evaluate_repeated_glmnet <- function(target_name, feature_set_name, model_group, features, target_value, top20_label, alpha_value) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    return(data.frame())
  }
  x <- as.matrix(prepare_matrix(features))
  complete <- stats::complete.cases(x) & !is.na(target_value) & !is.na(top20_label)
  row_index <- which(complete)
  if (length(row_index) < 40 || sum(top20_label[row_index]) < 8 || sum(!top20_label[row_index]) < 8) {
    return(data.frame())
  }

  rows <- list()
  for (repeat_id in seq_len(repeat_count)) {
    split <- make_split(row_index, top20_label, repeat_id)
    train_index <- split$train
    test_index <- split$test
    if (length(test_index) < 5 || length(unique(top20_label[test_index])) < 2) {
      next
    }
    set.seed(base_seed + 1000 + repeat_id)
    reg_model <- tryCatch(
      glmnet::cv.glmnet(x[train_index, , drop = FALSE], target_value[train_index], alpha = alpha_value, family = "gaussian", nfolds = 5),
      error = function(error) NULL
    )
    cls_model <- tryCatch(
      glmnet::cv.glmnet(x[train_index, , drop = FALSE], as.integer(top20_label[train_index]), alpha = alpha_value, family = "binomial", type.measure = "auc", nfolds = 5),
      error = function(error) NULL
    )
    pred_reg <- if (is.null(reg_model)) rep(NA_real_, length(test_index)) else as.numeric(stats::predict(reg_model, newx = x[test_index, , drop = FALSE], s = "lambda.min"))
    pred_cls <- if (is.null(cls_model)) rep(NA_real_, length(test_index)) else as.numeric(stats::predict(cls_model, newx = x[test_index, , drop = FALSE], s = "lambda.min", type = "response"))
    rows[[length(rows) + 1]] <- metric_row(
      target_name, feature_set_name, model_group, "regularized_cv_glmnet", alpha_value, repeat_id,
      ncol(x), length(row_index), length(train_index), length(test_index),
      target_value[test_index], top20_label[test_index], pred_reg, pred_cls
    )
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

summarize_metrics <- function(rows) {
  if (nrow(rows) == 0) {
    return(data.frame())
  }
  split_rows <- split(rows, paste(rows$target, rows$feature_set, rows$model_group, rows$method, rows$alpha, sep = "\r"), drop = TRUE)
  summaries <- lapply(split_rows, function(df) {
    data.frame(
      target = df$target[1],
      feature_set = df$feature_set[1],
      model_group = df$model_group[1],
      method = df$method[1],
      alpha = df$alpha[1],
      repeats = nrow(df),
      feature_count = df$feature_count[1],
      usable_rows = df$usable_rows[1],
      mean_auc = mean(df$auc, na.rm = TRUE),
      sd_auc = stats::sd(df$auc, na.rm = TRUE),
      mean_precision_at_predicted_top20 = mean(df$precision_at_predicted_top20, na.rm = TRUE),
      sd_precision_at_predicted_top20 = stats::sd(df$precision_at_predicted_top20, na.rm = TRUE),
      mean_recall_at_predicted_top20 = mean(df$recall_at_predicted_top20, na.rm = TRUE),
      sd_recall_at_predicted_top20 = stats::sd(df$recall_at_predicted_top20, na.rm = TRUE),
      mean_rmse = mean(df$rmse, na.rm = TRUE),
      sd_rmse = stats::sd(df$rmse, na.rm = TRUE),
      mean_mae = mean(df$mae, na.rm = TRUE),
      sd_mae = stats::sd(df$mae, na.rm = TRUE),
      mean_r_squared = mean(df$r_squared, na.rm = TRUE),
      sd_r_squared = stats::sd(df$r_squared, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, summaries)
  out[order(out$target, out$method, out$alpha, out$model_group), , drop = FALSE]
}

feature_importance_glmnet <- function(targets, feature_sets, scored) {
  columns <- c("target", "outcome_model", "feature_set", "model_group", "alpha", "lambda", "feature_name", "coefficient", "selected_nonzero", "note")
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    return(data.frame(
      target = NA_character_,
      outcome_model = NA_character_,
      feature_set = NA_character_,
      model_group = NA_character_,
      alpha = NA_real_,
      lambda = NA_real_,
      feature_name = NA_character_,
      coefficient = NA_real_,
      selected_nonzero = NA,
      note = "glmnet is not installed; run install.packages(\"glmnet\") to enable regularized feature importance",
      stringsAsFactors = FALSE
    )[, columns])
  }

  rows <- list()
  for (target_name in names(targets)) {
    target_value <- suppressWarnings(as.numeric(targets[[target_name]]))
    valid <- !is.na(target_value)
    top20_label <- rep(NA, length(target_value))
    cutoff <- as.numeric(stats::quantile(target_value[valid], probs = 0.80, na.rm = TRUE, type = 7))
    top20_label[valid] <- target_value[valid] >= cutoff
    for (feature_set_name in names(feature_sets)) {
      x <- as.matrix(prepare_matrix(feature_sets[[feature_set_name]]$features))
      complete <- stats::complete.cases(x) & !is.na(target_value) & !is.na(top20_label)
      if (sum(complete) < 40 || sum(top20_label[complete]) < 8 || sum(!top20_label[complete]) < 8) {
        next
      }
      for (alpha_value in c(0, 0.5, 1)) {
        for (outcome_model in c("linear", "logistic_top20")) {
          set.seed(base_seed)
          fit <- tryCatch({
            if (outcome_model == "linear") {
              glmnet::cv.glmnet(x[complete, , drop = FALSE], target_value[complete], alpha = alpha_value, family = "gaussian", nfolds = 5)
            } else {
              glmnet::cv.glmnet(x[complete, , drop = FALSE], as.integer(top20_label[complete]), alpha = alpha_value, family = "binomial", type.measure = "auc", nfolds = 5)
            }
          }, error = function(error) NULL)
          if (is.null(fit)) {
            next
          }
          coefs <- as.matrix(stats::coef(fit, s = "lambda.min"))
          coefs <- coefs[rownames(coefs) != "(Intercept)", , drop = FALSE]
          rows[[length(rows) + 1]] <- data.frame(
            target = target_name,
            outcome_model = outcome_model,
            feature_set = feature_set_name,
            model_group = feature_sets[[feature_set_name]]$group,
            alpha = alpha_value,
            lambda = fit$lambda.min,
            feature_name = rownames(coefs),
            coefficient = as.numeric(coefs[, 1]),
            selected_nonzero = as.numeric(coefs[, 1]) != 0,
            note = "final exploratory glmnet model fit on all scored rows",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (length(rows) == 0) {
    return(as.data.frame(setNames(replicate(length(columns), logical(0), simplify = FALSE), columns)))
  }
  do.call(rbind, rows)
}

empty_outputs <- function(reason) {
  write.csv(data.frame(), comparison_csv, row.names = FALSE)
  write.csv(data.frame(
    target = NA_character_,
    outcome_model = NA_character_,
    feature_set = NA_character_,
    model_group = NA_character_,
    alpha = NA_real_,
    lambda = NA_real_,
    feature_name = NA_character_,
    coefficient = NA_real_,
    selected_nonzero = NA,
    note = reason
  ), importance_csv, row.names = FALSE)
  writeLines(c("OpenAI regularized model comparison", "===================================", reason), comparison_txt)
  writeLines(c("OpenAI regularized scoring summary", "==================================", reason), summary_txt)
}

if (!file.exists(input_path)) {
  stop("Could not find input dataset: ", input_path, call. = FALSE)
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
}

articles$article_id <- as.character(articles$article_id)
scores$article_id <- as.character(scores$article_id)
articles$title <- clean_text_vector(articles$title)
articles$subtitle_deck_text <- if ("subtitle" %in% names(articles)) clean_text_vector(articles$subtitle) else NA_character_
articles$latest_claps <- suppressWarnings(as.numeric(articles$latest_claps))
articles$latest_responses <- suppressWarnings(as.numeric(articles$latest_responses))
articles$log_claps <- ifelse(!is.na(articles$latest_claps), log1p(articles$latest_claps), NA_real_)
articles$log_responses <- ifelse(!is.na(articles$latest_responses), log1p(articles$latest_responses), NA_real_)
articles$success_score <- if ("success_score" %in% names(articles)) suppressWarnings(as.numeric(articles$success_score)) else articles$log_claps + 2 * articles$log_responses
articles$success_score_equal <- ifelse(
  !is.na(articles$log_claps) | !is.na(articles$log_responses),
  ifelse(is.na(articles$log_claps), 0, articles$log_claps) + ifelse(is.na(articles$log_responses), 0, articles$log_responses),
  NA_real_
)

score_columns <- grep("_score$", names(scores), value = TRUE)
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

scored_ids <- Reduce(intersect, list(articles$article_id, pair_scores$article_id, title_scores$article_id))
scored <- articles[articles$article_id %in% scored_ids & !is.na(articles$title), , drop = FALSE]
scored <- merge(scored, pair_scores, by = "article_id", all.x = TRUE)
scored <- merge(scored, title_scores, by = "article_id", all.x = TRUE)
scored <- scored[order(as.numeric(scored$article_id)), , drop = FALSE]

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

combined_text <- paste(scored$title, ifelse(is.na(scored$subtitle_deck_text), "", scored$subtitle_deck_text))
combined_term_sets <- term_sets_for_docs(combined_text)
family_matrix <- data.frame(row.names = seq_len(nrow(scored)))
for (family_name in names(term_families)) {
  family_matrix[[paste0("combined_", family_name)]] <- as.integer(contains_family(combined_term_sets, term_families[[family_name]]))
}

baseline_features <- data.frame(
  make_basic_numeric_features(scored$title, "title"),
  make_basic_numeric_features(scored$subtitle_deck_text, "subtitle"),
  family_matrix,
  check.names = FALSE
)

pair_feature_cols <- grep("^openai_pair_.*_score$", names(scored), value = TRUE)
title_feature_cols <- grep("^openai_title_.*_score$", names(scored), value = TRUE)
all_openai_feature_cols <- c(pair_feature_cols, title_feature_cols)

feature_sets <- list(
  baseline_without_openai = list(
    group = "A_baseline_without_openai",
    features = baseline_features
  ),
  openai_scores_only = list(
    group = "B_openai_scores_only",
    features = scored[, all_openai_feature_cols, drop = FALSE]
  ),
  baseline_plus_openai_scores = list(
    group = "C_baseline_plus_openai_scores",
    features = data.frame(baseline_features, scored[, all_openai_feature_cols, drop = FALSE], check.names = FALSE)
  ),
  title_only_openai_scores_only = list(
    group = "D_title_only_openai_scores_only",
    features = scored[, title_feature_cols, drop = FALSE]
  ),
  title_subtitle_openai_scores_only = list(
    group = "E_title_subtitle_openai_scores_only",
    features = scored[, pair_feature_cols, drop = FALSE]
  ),
  baseline_plus_title_subtitle_openai_scores = list(
    group = "F_baseline_plus_title_subtitle_openai_scores",
    features = data.frame(baseline_features, scored[, pair_feature_cols, drop = FALSE], check.names = FALSE)
  )
)

targets <- list(
  log_claps = scored$log_claps,
  log_responses = scored$log_responses,
  success_score = scored$success_score,
  success_score_equal = scored$success_score_equal
)

glmnet_available <- requireNamespace("glmnet", quietly = TRUE)
if (!glmnet_available) {
  message("glmnet is not installed. Regularized glmnet models will be skipped. Install with install.packages(\"glmnet\").")
}

metric_rows <- list()
for (target_name in names(targets)) {
  target_value <- suppressWarnings(as.numeric(targets[[target_name]]))
  valid <- !is.na(target_value)
  top20_label <- rep(NA, length(target_value))
  cutoff <- as.numeric(stats::quantile(target_value[valid], probs = 0.80, na.rm = TRUE, type = 7))
  top20_label[valid] <- target_value[valid] >= cutoff

  for (feature_set_name in names(feature_sets)) {
    feature_set <- feature_sets[[feature_set_name]]
    metric_rows[[length(metric_rows) + 1]] <- evaluate_repeated_simple(
      target_name,
      feature_set_name,
      feature_set$group,
      feature_set$features,
      target_value,
      top20_label
    )
    if (glmnet_available) {
      for (alpha_value in c(0, 0.5, 1)) {
        metric_rows[[length(metric_rows) + 1]] <- evaluate_repeated_glmnet(
          target_name,
          feature_set_name,
          feature_set$group,
          feature_set$features,
          target_value,
          top20_label,
          alpha_value
        )
      }
    }
  }
}

metric_rows <- metric_rows[vapply(metric_rows, nrow, integer(1)) > 0]
all_metrics <- if (length(metric_rows) == 0) data.frame() else do.call(rbind, metric_rows)
comparison <- summarize_metrics(all_metrics)
write.csv(comparison, comparison_csv, row.names = FALSE)

feature_importance <- feature_importance_glmnet(targets, feature_sets, scored)
write.csv(feature_importance, importance_csv, row.names = FALSE)

best_for <- function(target_name, method_name = "simple_repeated_split") {
  rows <- comparison[comparison$target == target_name & comparison$method == method_name, , drop = FALSE]
  rows <- rows[!is.na(rows$mean_auc), , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NULL)
  }
  rows[which.max(rows$mean_auc), , drop = FALSE]
}

value_for <- function(target_name, feature_set_name, metric = "mean_auc", method_name = "simple_repeated_split", alpha_value = NA_real_) {
  rows <- comparison[comparison$target == target_name & comparison$feature_set == feature_set_name & comparison$method == method_name, , drop = FALSE]
  if (is.na(alpha_value)) {
    rows <- rows[is.na(rows$alpha), , drop = FALSE]
  } else {
    rows <- rows[!is.na(rows$alpha) & rows$alpha == alpha_value, , drop = FALSE]
  }
  if (nrow(rows) == 0 || !(metric %in% names(rows))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(rows[[metric]][1]))
}

best_value_for <- function(target_name, feature_set_name, metric = "mean_auc", method_name = "regularized_cv_glmnet") {
  rows <- comparison[comparison$target == target_name & comparison$feature_set == feature_set_name & comparison$method == method_name, , drop = FALSE]
  rows <- rows[!is.na(rows[[metric]]), , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NA_real_)
  }
  values <- suppressWarnings(as.numeric(rows[[metric]]))
  if (metric %in% c("mean_rmse", "mean_mae")) {
    return(min(values, na.rm = TRUE))
  }
  max(values, na.rm = TRUE)
}

best_alpha_for <- function(target_name, feature_set_name, metric = "mean_auc", method_name = "regularized_cv_glmnet") {
  rows <- comparison[comparison$target == target_name & comparison$feature_set == feature_set_name & comparison$method == method_name, , drop = FALSE]
  rows <- rows[!is.na(rows[[metric]]), , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NA_real_)
  }
  values <- suppressWarnings(as.numeric(rows[[metric]]))
  if (metric %in% c("mean_rmse", "mean_mae")) {
    return(rows$alpha[which.min(values)])
  }
  rows$alpha[which.max(values)]
}

openai_beats_baseline <- vapply(names(targets), function(target_name) {
  value_for(target_name, "openai_scores_only", "mean_auc") > value_for(target_name, "baseline_without_openai", "mean_auc")
}, logical(1))
pair_beats_title <- vapply(names(targets), function(target_name) {
  value_for(target_name, "title_subtitle_openai_scores_only", "mean_auc") > value_for(target_name, "title_only_openai_scores_only", "mean_auc")
}, logical(1))

target_lifts <- data.frame(
  target = names(targets),
  openai_minus_baseline_auc = vapply(names(targets), function(target_name) {
    value_for(target_name, "openai_scores_only", "mean_auc") - value_for(target_name, "baseline_without_openai", "mean_auc")
  }, numeric(1)),
  combined_minus_baseline_auc = vapply(names(targets), function(target_name) {
    value_for(target_name, "baseline_plus_openai_scores", "mean_auc") - value_for(target_name, "baseline_without_openai", "mean_auc")
  }, numeric(1)),
  stringsAsFactors = FALSE
)
target_lifts <- target_lifts[order(-target_lifts$openai_minus_baseline_auc), , drop = FALSE]

regularized_lifts <- data.frame(
  target = names(targets),
  best_baseline_auc = vapply(names(targets), function(target_name) best_value_for(target_name, "baseline_without_openai"), numeric(1)),
  best_openai_only_auc = vapply(names(targets), function(target_name) best_value_for(target_name, "openai_scores_only"), numeric(1)),
  best_baseline_plus_openai_auc = vapply(names(targets), function(target_name) best_value_for(target_name, "baseline_plus_openai_scores"), numeric(1)),
  best_title_only_openai_auc = vapply(names(targets), function(target_name) best_value_for(target_name, "title_only_openai_scores_only"), numeric(1)),
  best_title_subtitle_openai_auc = vapply(names(targets), function(target_name) best_value_for(target_name, "title_subtitle_openai_scores_only"), numeric(1)),
  best_baseline_plus_title_subtitle_auc = vapply(names(targets), function(target_name) best_value_for(target_name, "baseline_plus_title_subtitle_openai_scores"), numeric(1)),
  baseline_plus_openai_alpha = vapply(names(targets), function(target_name) best_alpha_for(target_name, "baseline_plus_openai_scores"), numeric(1)),
  stringsAsFactors = FALSE
)
regularized_lifts$openai_only_minus_baseline_auc <- regularized_lifts$best_openai_only_auc - regularized_lifts$best_baseline_auc
regularized_lifts$baseline_plus_openai_minus_baseline_auc <- regularized_lifts$best_baseline_plus_openai_auc - regularized_lifts$best_baseline_auc
regularized_lifts$pair_minus_title_openai_auc <- regularized_lifts$best_title_subtitle_openai_auc - regularized_lifts$best_title_only_openai_auc

regularized_lines <- if (glmnet_available) {
  c(
    "glmnet regularized models were run with alpha = 0, 0.5, and 1.",
    "Feature importance is from final exploratory glmnet models fit on all scored rows.",
    "Regularized lift below uses the best alpha by mean AUC for each target/feature set."
  )
} else {
  c(
    "glmnet is not installed, so ridge/elastic-net/lasso models were skipped.",
    "Install with install.packages(\"glmnet\") and rerun this script to enable regularized models and coefficient exports.",
    "Repeated-split simple models still ran as a stability check."
  )
}

scale_recommendation <- if (
  glmnet_available &&
    !is.na(regularized_lifts$baseline_plus_openai_minus_baseline_auc[regularized_lifts$target == "success_score"]) &&
    regularized_lifts$baseline_plus_openai_minus_baseline_auc[regularized_lifts$target == "success_score"] >= 0.05 &&
    regularized_lifts$baseline_plus_openai_minus_baseline_auc[regularized_lifts$target == "log_responses"] >= 0.05
) {
  "The regularized pilot is promising enough to justify either a larger scored pilot or scoring the remaining articles, but review the high AUC variance before treating this as durable."
} else if (
  all(is.na(target_lifts$openai_minus_baseline_auc)) ||
    sum(openai_beats_baseline, na.rm = TRUE) < 2 ||
    value_for("success_score", "openai_scores_only", "mean_auc") - value_for("success_score", "baseline_without_openai", "mean_auc") < 0.03
) {
  "Do not score all remaining articles yet. The 200-article pilot needs either stronger repeated-split lift or lower-variance regularized results before scaling."
} else {
  "Consider scoring the remaining articles only if the same lift remains visible after reviewing target-specific repeated-split variance and, ideally, rerunning with glmnet installed."
}

summary_lines <- c(
  "OpenAI regularized/repeated scoring summary",
  "==========================================",
  paste("Scored articles used:", nrow(scored)),
  paste("Scores file:", scores_path),
  paste("Rubric version analyzed:", selected_rubric_version),
  paste("Scored rows in OpenAI CSV:", nrow(scores)),
  paste("Repeated train/test splits:", repeat_count),
  paste("Train fraction:", train_fraction),
  paste("glmnet available:", glmnet_available),
  "",
  regularized_lines,
  "",
  "Does OpenAI-only still beat the baseline under repeated splits?",
  paste(names(openai_beats_baseline), ifelse(openai_beats_baseline, "yes", "no/unclear"), collapse = "; "),
  "",
  "Does baseline + OpenAI improve once regularization is used?",
  if (glmnet_available) {
    paste(
      regularized_lifts$target,
      paste0(
        "baseline+OpenAI lift=",
        round(regularized_lifts$baseline_plus_openai_minus_baseline_auc, 3),
        " (best alpha=",
        regularized_lifts$baseline_plus_openai_alpha,
        ")"
      ),
      collapse = "; "
    )
  } else {
    "Cannot answer regularized lift in this environment because glmnet is not installed."
  },
  "",
  "Is title-only OpenAI or title+subtitle OpenAI more stable?",
  paste(names(pair_beats_title), ifelse(pair_beats_title, "title+subtitle higher mean AUC", "title-only higher/equal mean AUC"), collapse = "; "),
  "",
  "Which target benefits most from OpenAI scoring?",
  if (glmnet_available && any(!is.na(regularized_lifts$baseline_plus_openai_minus_baseline_auc))) {
    best_reg <- regularized_lifts[which.max(regularized_lifts$baseline_plus_openai_minus_baseline_auc), , drop = FALSE]
    paste0(best_reg$target, " has the largest regularized baseline+OpenAI minus baseline mean AUC lift: ", round(best_reg$baseline_plus_openai_minus_baseline_auc, 3))
  } else if (nrow(target_lifts) == 0) {
    "No repeated-split target lift estimates were available."
  } else {
    paste0(target_lifts$target[1], " has the largest OpenAI-only minus baseline mean AUC lift: ", round(target_lifts$openai_minus_baseline_auc[1], 3))
  },
  "",
  "Recommendation on scoring all remaining articles",
  scale_recommendation,
  "",
  "Interpretation guardrails",
  "- Sample size is still only 200 articles.",
  "- This is not causal evidence.",
  "- Performance proxies are claps/responses, not earnings.",
  "- Context-free headline scoring is only one part of Medium performance.",
  "- Do not scale to all articles unless lift is stable under repeated splits or regularization."
)
writeLines(summary_lines, summary_txt)

comparison_lines <- c(
  "OpenAI regularized/repeated model comparison",
  "===========================================",
  paste("Scored articles used:", nrow(scored)),
  paste("Scores file:", scores_path),
  paste("Rubric version analyzed:", selected_rubric_version),
  paste("Repeated train/test splits:", repeat_count),
  paste("glmnet available:", glmnet_available),
  "",
  "Metric summary",
  "--------------",
  capture.output(print(comparison, row.names = FALSE))
)
writeLines(comparison_lines, comparison_txt)

message("Saved regularized/repeated OpenAI headline outputs to: ", output_dir)
