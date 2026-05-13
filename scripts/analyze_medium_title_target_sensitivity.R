input_path <- file.path("data", "analysis", "medium_title_prediction_dataset.csv")
output_dir <- file.path("data", "analysis", "title_target_sensitivity")

message("Medium Title Target Sensitivity Analysis")
message("========================================")

if (!file.exists(input_path)) {
  stop(
    "Could not find analysis dataset at: ",
    input_path,
    "\nRun this first:\nRscript scripts/build_medium_title_prediction_dataset.R",
    call. = FALSE
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

articles <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)

clean_text_vector <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

normalize_title <- function(x) {
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

split_words <- function(title) {
  normalized <- normalize_title(title)
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

term_sets_for_docs <- function(titles) {
  lapply(titles, function(title) {
    words <- split_words(title)
    unique(c(make_ngrams(words, 1), make_ngrams(words, 2), make_ngrams(words, 3)))
  })
}

contains_family <- function(term_sets, terms) {
  vapply(term_sets, function(article_terms) any(terms %in% article_terms), logical(1))
}

safe_numeric <- function(data, column_name) {
  if (!(column_name %in% names(data))) {
    return(rep(NA_real_, nrow(data)))
  }
  suppressWarnings(as.numeric(data[[column_name]]))
}

fill_numeric <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
}

top_level_factor <- function(x, train_index, top_n = 20, min_n = 8) {
  value <- clean_text_vector(x)
  train_values <- value[train_index]
  counts <- sort(table(train_values[!is.na(train_values)]), decreasing = TRUE)
  keep <- names(head(counts[counts >= min_n], top_n))
  out <- ifelse(!is.na(value) & value %in% keep, value, "other_or_missing")
  factor(out, levels = c(sort(keep), "other_or_missing"))
}

expand_multi_value_factor <- function(x, prefix, train_index, min_n = 10, max_levels = 30) {
  value <- clean_text_vector(x)
  split_values <- strsplit(ifelse(is.na(value), "", value), "\\s*;\\s*")
  train_values <- unlist(split_values[train_index], use.names = FALSE)
  train_values <- train_values[train_values != ""]
  counts <- sort(table(train_values), decreasing = TRUE)
  keep <- names(head(counts[counts >= min_n], max_levels))

  if (length(keep) == 0) {
    return(data.frame(row.names = seq_along(value)))
  }

  out <- data.frame(row.names = seq_along(value))
  for (level in keep) {
    out[[paste0(prefix, "_", make.names(level))]] <- as.integer(vapply(split_values, function(parts) level %in% parts, logical(1)))
  }
  out
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
  tryCatch(
    stats::lm(target ~ ., data = train),
    error = function(error) {
      warning("Regression model failed: ", conditionMessage(error), call. = FALSE)
      NULL
    }
  )
}

fit_glm_safely <- function(x_train, y_train) {
  if (length(unique(y_train)) < 2) {
    return(NULL)
  }
  train <- data.frame(top20 = y_train, x_train, check.names = FALSE)
  tryCatch(
    suppressWarnings(stats::glm(top20 ~ ., data = train, family = stats::binomial())),
    error = function(error) {
      warning("Classification model failed: ", conditionMessage(error), call. = FALSE)
      NULL
    }
  )
}

predict_safely <- function(model, newdata, type = "response") {
  if (is.null(model)) {
    return(rep(NA_real_, nrow(newdata)))
  }
  tryCatch(
    as.numeric(stats::predict(model, newdata = newdata, type = type)),
    warning = function(warning) suppressWarnings(as.numeric(stats::predict(model, newdata = newdata, type = type))),
    error = function(error) {
      warning("Prediction failed: ", conditionMessage(error), call. = FALSE)
      rep(NA_real_, nrow(newdata))
    }
  )
}

evaluate_model <- function(target_name, model_name, model_group, x, train_index, test_index, y_reg, y_cls) {
  x_train <- x[train_index, , drop = FALSE]
  x_test <- x[test_index, , drop = FALSE]
  lm_model <- fit_lm_safely(x_train, y_reg[train_index])
  glm_model <- fit_glm_safely(x_train, y_cls[train_index])

  pred_reg_test <- predict_safely(lm_model, x_test, type = "response")
  pred_cls_test <- predict_safely(glm_model, x_test, type = "response")
  cls <- classification_metrics(y_cls[test_index], pred_cls_test)

  data.frame(
    target = target_name,
    model = model_name,
    model_group = model_group,
    feature_count = ncol(x),
    train_rows = length(train_index),
    test_rows = length(test_index),
    regression_rmse = rmse(y_reg[test_index], pred_reg_test),
    regression_mae = mae(y_reg[test_index], pred_reg_test),
    regression_r_squared = r_squared(y_reg[test_index], pred_reg_test),
    classification_accuracy = cls["accuracy"],
    classification_precision_at_top20_predicted = cls["precision"],
    classification_recall_at_top20_predicted = cls["recall"],
    classification_auc = cls["auc"],
    stringsAsFactors = FALSE
  )
}

family_metric_summary <- function(target_name, family_name, matched_terms, present, target_value, top20_label) {
  absent <- !present
  present_target <- target_value[present]
  absent_target <- target_value[absent]
  present_top20 <- top20_label[present]
  absent_top20 <- top20_label[absent]

  mean_present <- mean(present_target, na.rm = TRUE)
  mean_absent <- mean(absent_target, na.rm = TRUE)
  median_present <- median(present_target, na.rm = TRUE)
  median_absent <- median(absent_target, na.rm = TRUE)
  rate_present <- mean(present_top20, na.rm = TRUE)
  rate_absent <- mean(absent_top20, na.rm = TRUE)
  mean_lift <- mean_present - mean_absent
  median_lift <- median_present - median_absent
  rate_lift <- rate_present - rate_absent

  data.frame(
    target = target_name,
    family = family_name,
    matched_terms = paste(matched_terms, collapse = "; "),
    n_titles_present = sum(present, na.rm = TRUE),
    n_titles_absent = sum(absent, na.rm = TRUE),
    mean_target_present = mean_present,
    mean_target_absent = mean_absent,
    mean_target_lift = mean_lift,
    median_target_present = median_present,
    median_target_absent = median_absent,
    median_target_lift = median_lift,
    high_performer_top20_rate_present = rate_present,
    high_performer_top20_rate_absent = rate_absent,
    high_performer_top20_rate_lift = rate_lift,
    mean_lift_direction = ifelse(mean_lift > 0, "positive", ifelse(mean_lift < 0, "negative", "zero")),
    high_rate_lift_direction = ifelse(rate_lift > 0, "positive", ifelse(rate_lift < 0, "negative", "zero")),
    min_n_10 = sum(present, na.rm = TRUE) >= 10,
    min_n_20 = sum(present, na.rm = TRUE) >= 20,
    stringsAsFactors = FALSE
  )
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits, trim = TRUE))
}

fmt_pct <- function(x) {
  ifelse(is.na(x), "NA", paste0(round(100 * x, 1), "%"))
}

required_columns <- c("title", "latest_claps", "latest_responses")
missing_columns <- setdiff(required_columns, names(articles))
if (length(missing_columns) > 0) {
  stop("Dataset is missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

articles$title <- clean_text_vector(articles$title)
articles$latest_claps <- suppressWarnings(as.numeric(articles$latest_claps))
articles$latest_responses <- suppressWarnings(as.numeric(articles$latest_responses))

usable <- articles[!is.na(articles$title), , drop = FALSE]
message("Loaded ", nrow(articles), " rows.")
message("Using ", nrow(usable), " rows with titles.")

usable$log_claps <- ifelse(!is.na(usable$latest_claps), log1p(usable$latest_claps), NA_real_)
usable$log_responses <- ifelse(!is.na(usable$latest_responses), log1p(usable$latest_responses), NA_real_)
usable$success_score_current <- ifelse(
  !is.na(usable$log_claps) | !is.na(usable$log_responses),
  ifelse(is.na(usable$log_claps), 0, usable$log_claps) + 2 * ifelse(is.na(usable$log_responses), 0, usable$log_responses),
  NA_real_
)
usable$success_score_equal <- ifelse(
  !is.na(usable$log_claps) | !is.na(usable$log_responses),
  ifelse(is.na(usable$log_claps), 0, usable$log_claps) + ifelse(is.na(usable$log_responses), 0, usable$log_responses),
  NA_real_
)
usable$success_score_clap_weighted <- ifelse(
  !is.na(usable$log_claps) | !is.na(usable$log_responses),
  ifelse(is.na(usable$log_claps), 0, usable$log_claps) + 0.5 * ifelse(is.na(usable$log_responses), 0, usable$log_responses),
  NA_real_
)

targets <- c(
  "log_claps",
  "log_responses",
  "success_score_current",
  "success_score_equal",
  "success_score_clap_weighted"
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
  mistake_family = c("mistake", "mistakes", "wrong", "avoid"),
  beginner_family = c("beginner", "beginners", "starting", "start"),
  retirement_fire_family = c("retirement", "retire", "retires", "retired", "retiring", "fire", "financial independence")
)

term_sets <- term_sets_for_docs(usable$title)
family_matrix <- data.frame(row.names = seq_len(nrow(usable)))
for (family_name in names(term_families)) {
  family_matrix[[family_name]] <- as.integer(contains_family(term_sets, term_families[[family_name]]))
}

normalized_titles <- normalize_title(usable$title)
split_normalized_titles <- strsplit(normalized_titles, " ")
numeric_title_features <- data.frame(
  title_char_count = nchar(usable$title),
  title_word_count = vapply(split_normalized_titles, function(words) sum(words != ""), integer(1)),
  title_has_number = as.integer(grepl("[0-9]", usable$title)),
  title_has_question_mark = as.integer(grepl("\\?", usable$title)),
  title_has_colon = as.integer(grepl(":", usable$title)),
  title_has_dollar = as.integer(grepl("\\$", usable$title)),
  stringsAsFactors = FALSE
)

set.seed(20260513)
base_train_index <- sort(sample(seq_len(nrow(usable)), size = floor(0.80 * nrow(usable))))
base_test_index <- setdiff(seq_len(nrow(usable)), base_train_index)

context_numeric <- data.frame(
  times_seen = fill_numeric(safe_numeric(usable, "times_seen")),
  best_rank = fill_numeric(safe_numeric(usable, "best_rank")),
  average_rank = fill_numeric(safe_numeric(usable, "average_rank")),
  article_age_days_at_latest_observation = fill_numeric(safe_numeric(usable, "article_age_days_at_latest_observation")),
  article_age_days_at_latest_stats = fill_numeric(safe_numeric(usable, "article_age_days_at_latest_stats")),
  public_stats_observation_count = fill_numeric(safe_numeric(usable, "public_stats_observation_count")),
  stringsAsFactors = FALSE
)

context_categorical <- data.frame(row.names = seq_len(nrow(usable)))
if ("publication" %in% names(usable)) {
  context_categorical$publication_group <- top_level_factor(usable$publication, base_train_index, top_n = 20, min_n = 8)
}
if ("author" %in% names(usable)) {
  context_categorical$author_group <- top_level_factor(usable$author, base_train_index, top_n = 20, min_n = 8)
}
if ("observed_page_variants" %in% names(usable)) {
  context_categorical$observed_page_variants_group <- top_level_factor(usable$observed_page_variants, base_train_index, top_n = 10, min_n = 5)
}
tag_features <- if ("observed_tag_slugs" %in% names(usable)) {
  expand_multi_value_factor(usable$observed_tag_slugs, "tag", base_train_index, min_n = 10, max_levels = 30)
} else {
  data.frame(row.names = seq_len(nrow(usable)))
}
context_features <- data.frame(context_numeric, context_categorical, tag_features, check.names = FALSE)

model_feature_sets <- list(
  title_only_numeric = list(group = "title_only_pre_publication", features = numeric_title_features),
  title_only_numeric_plus_term_families = list(group = "title_only_pre_publication", features = data.frame(numeric_title_features, family_matrix, check.names = FALSE)),
  context_aware_numeric_plus_context = list(group = "context_aware_post_observation", features = data.frame(numeric_title_features, context_features, check.names = FALSE)),
  context_aware_context_plus_term_families = list(group = "context_aware_post_observation", features = data.frame(numeric_title_features, context_features, family_matrix, check.names = FALSE))
)

target_summary_rows <- list()
term_sensitivity_rows <- list()
model_metric_rows <- list()

for (target_name in targets) {
  target_value <- suppressWarnings(as.numeric(usable[[target_name]]))
  valid <- !is.na(target_value)
  if (sum(valid) == 0) {
    warning("Skipping target with no valid rows: ", target_name, call. = FALSE)
    next
  }

  cutoff <- as.numeric(stats::quantile(target_value[valid], probs = 0.80, na.rm = TRUE, type = 7))
  top20_label <- rep(NA, length(target_value))
  top20_label[valid] <- target_value[valid] >= cutoff

  target_summary_rows[[target_name]] <- data.frame(
    target = target_name,
    valid_rows = sum(valid),
    target_mean = mean(target_value[valid], na.rm = TRUE),
    target_median = median(target_value[valid], na.rm = TRUE),
    target_min = min(target_value[valid], na.rm = TRUE),
    target_max = max(target_value[valid], na.rm = TRUE),
    top20_cutoff = cutoff,
    high_performer_count = sum(top20_label, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  for (family_name in names(term_families)) {
    present <- family_matrix[[family_name]] == 1
    present[!valid] <- FALSE
    family_valid <- valid
    row <- family_metric_summary(
      target_name,
      family_name,
      term_families[[family_name]],
      present[family_valid],
      target_value[family_valid],
      top20_label[family_valid]
    )
    term_sensitivity_rows[[paste(target_name, family_name, sep = "__")]] <- row
  }

  valid_indices <- which(valid)
  train_index <- intersect(base_train_index, valid_indices)
  test_index <- intersect(base_test_index, valid_indices)

  enough_rows <- length(train_index) >= 80 &&
    length(test_index) >= 20 &&
    sum(top20_label[train_index], na.rm = TRUE) >= 10 &&
    sum(!top20_label[train_index], na.rm = TRUE) >= 10 &&
    sum(top20_label[test_index], na.rm = TRUE) >= 5 &&
    sum(!top20_label[test_index], na.rm = TRUE) >= 5

  if (!enough_rows) {
    warning("Skipping model comparison for target with insufficient split examples: ", target_name, call. = FALSE)
    next
  }

  for (model_name in names(model_feature_sets)) {
    result <- evaluate_model(
      target_name,
      model_name,
      model_feature_sets[[model_name]]$group,
      model_feature_sets[[model_name]]$features,
      train_index,
      test_index,
      target_value,
      top20_label
    )
    model_metric_rows[[paste(target_name, model_name, sep = "__")]] <- result
  }
}

target_summary <- do.call(rbind, target_summary_rows)
write.csv(target_summary, file.path(output_dir, "target_summary.csv"), row.names = FALSE)

term_family_target_sensitivity <- do.call(rbind, term_sensitivity_rows)
current_rows <- term_family_target_sensitivity[term_family_target_sensitivity$target == "success_score_current", c("family", "mean_lift_direction", "high_rate_lift_direction")]
names(current_rows) <- c("family", "current_mean_lift_direction", "current_high_rate_lift_direction")
term_family_target_sensitivity <- merge(term_family_target_sensitivity, current_rows, by = "family", all.x = TRUE, sort = FALSE)
term_family_target_sensitivity$mean_direction_matches_current <- term_family_target_sensitivity$mean_lift_direction == term_family_target_sensitivity$current_mean_lift_direction
term_family_target_sensitivity$high_rate_direction_matches_current <- term_family_target_sensitivity$high_rate_lift_direction == term_family_target_sensitivity$current_high_rate_lift_direction
term_family_target_sensitivity <- term_family_target_sensitivity[order(term_family_target_sensitivity$family, term_family_target_sensitivity$target), ]
write.csv(term_family_target_sensitivity, file.path(output_dir, "term_family_target_sensitivity.csv"), row.names = FALSE)

model_metrics_by_target <- if (length(model_metric_rows) > 0) do.call(rbind, model_metric_rows) else data.frame()
if (nrow(model_metrics_by_target) > 0) {
  model_metrics_by_target <- model_metrics_by_target[order(model_metrics_by_target$target, model_metrics_by_target$model_group, -model_metrics_by_target$classification_auc), ]
}
write.csv(model_metrics_by_target, file.path(output_dir, "model_metrics_by_target.csv"), row.names = FALSE)

family_target <- function(family_name, target_name) {
  row <- term_family_target_sensitivity[
    term_family_target_sensitivity$family == family_name & term_family_target_sensitivity$target == target_name,
    ,
    drop = FALSE
  ]
  if (nrow(row) == 0) {
    return(NULL)
  }
  row
}

family_direction_text <- function(family_name) {
  rows <- term_family_target_sensitivity[term_family_target_sensitivity$family == family_name, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(paste(family_name, "has no rows."))
  }
  paste0(
    family_name,
    ": mean-lift directions by target = ",
    paste(paste(rows$target, rows$mean_lift_direction, sep = ":"), collapse = ", "),
    "; top20-rate directions = ",
    paste(paste(rows$target, rows$high_rate_lift_direction, sep = ":"), collapse = ", "),
    "."
  )
}

best_title_by_target <- data.frame()
if (nrow(model_metrics_by_target) > 0) {
  for (target_name in unique(model_metrics_by_target$target)) {
    subset <- model_metrics_by_target[
      model_metrics_by_target$target == target_name & model_metrics_by_target$model_group == "title_only_pre_publication",
      ,
      drop = FALSE
    ]
    if (nrow(subset) > 0) {
      best_title_by_target <- rbind(
        best_title_by_target,
        subset[which.max(ifelse(is.na(subset$classification_auc), -Inf, subset$classification_auc)), , drop = FALSE]
      )
    }
  }
}

ret_claps <- family_target("retirement_family", "log_claps")
ret_responses <- family_target("retirement_family", "log_responses")
ret_current <- family_target("retirement_family", "success_score_current")
mistake_responses <- family_target("mistake_family", "log_responses")

weak_family_lines <- c(
  family_direction_text("etf_family"),
  family_direction_text("index_family"),
  family_direction_text("portfolio_family")
)

comment_family_rows <- term_family_target_sensitivity[term_family_target_sensitivity$target == "log_responses", , drop = FALSE]
comment_family_rows <- comment_family_rows[order(-comment_family_rows$mean_target_lift), ]
top_response_family <- comment_family_rows[1, , drop = FALSE]

best_title_lines <- if (nrow(best_title_by_target) > 0) {
  apply(best_title_by_target, 1, function(row) {
    paste0(
      row[["target"]],
      ": ",
      row[["model"]],
      " (AUC ",
      fmt_num(as.numeric(row[["classification_auc"]])),
      ", RMSE ",
      fmt_num(as.numeric(row[["regression_rmse"]]),
      3),
      ")"
    )
  })
} else {
  "Model comparison was not available."
}

summary_lines <- c(
  "Medium title target-sensitivity summary",
  "=======================================",
  "",
  "Scope",
  "-----",
  paste("Rows analyzed:", nrow(usable)),
  "This analysis compares five log-transformed targets built from latest public claps and responses.",
  "Log targets reduce outlier dominance and make the models focus more on relative performance than raw viral extremes.",
  "Title-only / pre-publication models use only title length, punctuation, and term families.",
  "Context-aware / post-observation models additionally use observed publication/author/tag/rank/age controls when available.",
  "No SQLite writes and no OpenAI/API scoring are used.",
  "",
  "Do retirement-related titles look strong for claps, responses, and combined metrics?",
  "----------------------------------------------------------------------------------"
)

if (!is.null(ret_claps) && !is.null(ret_responses) && !is.null(ret_current)) {
  summary_lines <- c(
    summary_lines,
    paste0("Retirement family log-claps mean lift: ", fmt_num(ret_claps$mean_target_lift), "; top20-rate lift: ", fmt_pct(ret_claps$high_performer_top20_rate_lift), "."),
    paste0("Retirement family log-responses mean lift: ", fmt_num(ret_responses$mean_target_lift), "; top20-rate lift: ", fmt_pct(ret_responses$high_performer_top20_rate_lift), "."),
    paste0("Retirement family current combined-score mean lift: ", fmt_num(ret_current$mean_target_lift), "; top20-rate lift: ", fmt_pct(ret_current$high_performer_top20_rate_lift), "."),
    if (all(c(ret_claps$mean_target_lift, ret_responses$mean_target_lift, ret_current$mean_target_lift) > 0)) {
      "In this dataset, retirement-related titles remain positive across claps, responses, and the current combined score."
    } else {
      "Retirement-related titles do not stay positive across every target, so the finding is target-sensitive."
    }
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "Are ETF/index/portfolio weak across all targets?",
  "-----------------------------------------------",
  weak_family_lines,
  "These families should still be treated as potentially confounded by collection surface, tag mix, and formulaic article framing.",
  "",
  "Do some families predict comments/responses better than claps?",
  "-------------------------------------------------------------",
  paste0(
    "The strongest family by log-responses mean lift is ",
    top_response_family$family,
    " with lift ",
    fmt_num(top_response_family$mean_target_lift),
    " and top20-rate lift ",
    fmt_pct(top_response_family$high_performer_top20_rate_lift),
    "."
  )
)

if (!is.null(mistake_responses)) {
  summary_lines <- c(
    summary_lines,
    paste0(
      "Mistake/problem-framing terms have log-responses lift ",
      fmt_num(mistake_responses$mean_target_lift),
      " and top20-rate lift ",
      fmt_pct(mistake_responses$high_performer_top20_rate_lift),
      ", which is useful to compare with their clap lift in the CSV."
    )
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "Does the best title-only model change depending on target?",
  "---------------------------------------------------------",
  best_title_lines,
  "",
  "Which target seems most useful for near-term Medium strategy?",
  "------------------------------------------------------------",
  "Use multiple targets for now. The current score is useful when comments/responses are strategically important, but log_claps is cleaner for broad reach and log_responses is cleaner for discussion/engagement.",
  "For near-term title strategy, success_score_equal is a useful middle ground because it rewards both reach and response without making comments dominate as strongly as the current 2x response formula.",
  "",
  "Before adding OpenAI/ChatGPT API title scoring",
  "----------------------------------------------",
  "- Review family examples under each target, especially where direction changes.",
  "- Add subtitle/deck analysis next, because Medium performance often depends on the title plus deck together.",
  "- Separate observed tag/page effects from wording effects before treating title wording as causal.",
  "- Consider time-aware validation once observations span more collection dates.",
  "- Benchmark any future LLM title score against these title-only target-specific baselines."
)

writeLines(summary_lines, file.path(output_dir, "target_sensitivity_summary.txt"))

message("Saved target sensitivity outputs to: ", output_dir)
message("Done.")
