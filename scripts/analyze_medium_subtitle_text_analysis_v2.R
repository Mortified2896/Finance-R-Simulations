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

database_path <- file.path("data", "db", "medium_articles.sqlite")
output_dir <- file.path("data", "analysis", "medium_analysis_v2", "subtitle_analysis")

message("Medium Subtitle/Deck Text Analysis V2")
message("=====================================")

if (!file.exists(database_path)) {
  stop(
    "Could not find database at: ",
    database_path,
    call. = FALSE
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

connection <- dbConnect(SQLite(), database_path, flags = SQLITE_RO)
on.exit(dbDisconnect(connection), add = TRUE)

if (!dbExistsTable(connection, "v_medium_title_prediction_dataset_v2")) {
  stop("Missing view v_medium_title_prediction_dataset_v2. Run scripts/apply_medium_analysis_v2_schema.R first.", call. = FALSE)
}

articles <- dbGetQuery(connection, "
  SELECT
    canonical_article_key AS article_id,
    article_id AS source_article_id,
    medium_post_id,
    url,
    title,
    subtitle,
    author,
    publication_name AS publication,
    tags_seen AS observed_tag_slugs,
    source_tag,
    times_seen,
    best_page_position AS best_rank,
    best_page_position AS average_rank,
    age_days_at_observation AS article_age_days_at_latest_observation,
    age_days_at_observation AS article_age_days_at_latest_stats,
    claps,
    responses,
    claps AS latest_claps,
    responses AS latest_responses,
    log_claps,
    log_responses,
    success_score,
    top_20_percent AS high_performer_top20,
    top_10_percent AS high_performer_top10,
    top_5_percent AS high_performer_top5,
    top_20_percent,
    top_10_percent,
    top_5_percent
  FROM v_medium_title_prediction_dataset_v2
")

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

build_term_rows <- function(texts, n, term_type) {
  rows <- lapply(seq_along(texts), function(i) {
    words <- split_words(texts[i])
    terms <- unique(make_ngrams(words, n))
    terms <- terms[terms != ""]
    if (length(terms) == 0) {
      return(NULL)
    }
    data.frame(row_index = i, term = terms, term_type = term_type, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame(row_index = integer(), term = character(), term_type = character()))
  }
  do.call(rbind, rows)
}

contains_family <- function(term_sets, terms) {
  vapply(term_sets, function(article_terms) any(terms %in% article_terms), logical(1))
}

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

se_rate <- function(rate, n) {
  if (is.na(rate) || n <= 0) {
    return(NA_real_)
  }
  sqrt(rate * (1 - rate) / n)
}

summarize_term_rows <- function(term_rows, success_score, high_performer, min_n = 10) {
  if (nrow(term_rows) == 0) {
    return(data.frame())
  }
  groups <- split(term_rows$row_index, term_rows$term, drop = TRUE)
  all_index <- seq_along(success_score)
  rows <- lapply(names(groups), function(term) {
    present_index <- sort(unique(groups[[term]]))
    n_present <- length(present_index)
    if (n_present < min_n) {
      return(NULL)
    }

    absent_index <- setdiff(all_index, present_index)
    present_success <- success_score[present_index]
    absent_success <- success_score[absent_index]
    present_high <- high_performer[present_index]
    absent_high <- high_performer[absent_index]
    present_rate <- mean(present_high, na.rm = TRUE)
    absent_rate <- mean(absent_high, na.rm = TRUE)
    rate_lift <- present_rate - absent_rate
    rate_lift_se <- sqrt(se_rate(present_rate, n_present)^2 + se_rate(absent_rate, length(absent_index))^2)
    mean_present <- mean(present_success, na.rm = TRUE)
    mean_absent <- mean(absent_success, na.rm = TRUE)

    data.frame(
      term = term,
      n_rows_present = n_present,
      median_success_present = median(present_success, na.rm = TRUE),
      median_success_absent = median(absent_success, na.rm = TRUE),
      mean_success_present = mean_present,
      mean_success_absent = mean_absent,
      mean_success_lift = mean_present - mean_absent,
      high_performer_top20_rate_present = present_rate,
      high_performer_top20_rate_absent = absent_rate,
      high_performer_top20_rate_lift = rate_lift,
      high_performer_top20_rate_lift_low_approx = rate_lift - 1.96 * rate_lift_se,
      high_performer_top20_rate_lift_high_approx = rate_lift + 1.96 * rate_lift_se,
      min_n_10 = n_present >= 10,
      min_n_20 = n_present >= 20,
      small_sample_flag = n_present < 20,
      unstable_direction_flag = !is.na(rate_lift_se) && ((rate_lift - 1.96 * rate_lift_se) <= 0 && (rate_lift + 1.96 * rate_lift_se) >= 0),
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

write_empty_csv <- function(path, columns) {
  write.csv(as.data.frame(setNames(replicate(length(columns), logical(0), simplify = FALSE), columns)), path, row.names = FALSE)
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

make_term_matrix <- function(term_sets, vocabulary, prefix) {
  if (length(vocabulary) == 0) {
    return(matrix(nrow = length(term_sets), ncol = 0))
  }
  mat <- matrix(0L, nrow = length(term_sets), ncol = length(vocabulary))
  colnames(mat) <- paste0(prefix, "_", make.names(vocabulary, unique = TRUE))
  names(vocabulary) <- colnames(mat)
  lookup <- setNames(seq_along(vocabulary), vocabulary)
  for (i in seq_along(term_sets)) {
    present <- intersect(term_sets[[i]], vocabulary)
    if (length(present) > 0) {
      mat[i, lookup[present]] <- 1L
    }
  }
  mat
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

fit_lm_safely <- function(x_train, y_train) {
  train <- data.frame(success_score = y_train, x_train, check.names = FALSE)
  tryCatch(
    stats::lm(success_score ~ ., data = train),
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
  train <- data.frame(high_performer_top20 = y_train, x_train, check.names = FALSE)
  tryCatch(
    suppressWarnings(stats::glm(high_performer_top20 ~ ., data = train, family = stats::binomial())),
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

evaluate_model <- function(model_name, model_group, x, train_index, test_index, y_reg, y_cls) {
  x_train <- x[train_index, , drop = FALSE]
  x_test <- x[test_index, , drop = FALSE]
  lm_model <- fit_lm_safely(x_train, y_reg[train_index])
  glm_model <- fit_glm_safely(x_train, y_cls[train_index])
  pred_reg_test <- predict_safely(lm_model, x_test, type = "response")
  pred_cls_test <- predict_safely(glm_model, x_test, type = "response")
  pred_reg_all <- predict_safely(lm_model, x, type = "response")
  pred_cls_all <- predict_safely(glm_model, x, type = "response")
  cls <- classification_metrics(y_cls[test_index], pred_cls_test)
  list(
    metrics = data.frame(
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
    ),
    pred_reg_all = pred_reg_all,
    pred_cls_all = pred_cls_all
  )
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

required_columns <- c("title", "latest_claps", "latest_responses")
missing_columns <- setdiff(required_columns, names(articles))
if (length(missing_columns) > 0) {
  stop("Dataset is missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

articles$title <- clean_text_vector(articles$title)
articles$latest_claps <- suppressWarnings(as.numeric(articles$latest_claps))
articles$latest_responses <- suppressWarnings(as.numeric(articles$latest_responses))
articles$success_score_main <- ifelse(
  !is.na(articles$latest_claps) | !is.na(articles$latest_responses),
  log1p(ifelse(is.na(articles$latest_claps), 0, articles$latest_claps)) +
    2 * log1p(ifelse(is.na(articles$latest_responses), 0, articles$latest_responses)),
  NA_real_
)

valid_success <- !is.na(articles$success_score_main)
cutoff20 <- as.numeric(stats::quantile(articles$success_score_main[valid_success], probs = 0.80, na.rm = TRUE, type = 7))
articles$high_performer_top20_main <- NA
articles$high_performer_top20_main[valid_success] <- articles$success_score_main[valid_success] >= cutoff20

subtitle_candidates <- intersect(c("subtitle", "deck", "description", "snippet"), names(articles))
subtitle_source_column <- NA_character_
subtitle_text <- rep(NA_character_, nrow(articles))
if (length(subtitle_candidates) > 0) {
  counts <- vapply(subtitle_candidates, function(col) sum(!is.na(clean_text_vector(articles[[col]]))), integer(1))
  subtitle_source_column <- names(sort(counts, decreasing = TRUE))[1]
  subtitle_text <- clean_text_vector(articles[[subtitle_source_column]])
  for (col in subtitle_candidates) {
    replacement <- clean_text_vector(articles[[col]])
    subtitle_text[is.na(subtitle_text) & !is.na(replacement)] <- replacement[is.na(subtitle_text) & !is.na(replacement)]
  }
}
articles$subtitle_deck_text <- subtitle_text

rows_with_title <- sum(!is.na(articles$title))
rows_with_subtitle <- sum(!is.na(articles$subtitle_deck_text))
rows_with_both <- sum(!is.na(articles$title) & !is.na(articles$subtitle_deck_text))
subtitle_lengths <- nchar(articles$subtitle_deck_text[!is.na(articles$subtitle_deck_text)])
subtitle_examples <- head(articles$subtitle_deck_text[!is.na(articles$subtitle_deck_text)], 12)

quality_lines <- c(
  "Subtitle/deck data quality",
  "==========================",
  "Source: v_medium_title_prediction_dataset_v2",
  paste("V2 view rows loaded:", nrow(articles)),
  paste("Total rows:", nrow(articles)),
  paste("Rows with title:", rows_with_title),
  paste("Rows with usable subtitle/deck:", rows_with_subtitle),
  paste("Rows with both title and subtitle/deck:", rows_with_both),
  paste("Subtitle source column selected:", ifelse(is.na(subtitle_source_column), "(none)", subtitle_source_column)),
  paste("Candidate subtitle-like columns:", ifelse(length(subtitle_candidates) == 0, "(none)", paste(subtitle_candidates, collapse = ", "))),
  paste("Median subtitle length:", ifelse(length(subtitle_lengths) == 0, "NA", median(subtitle_lengths))),
  paste("Subtitle missing rate:", round(1 - rows_with_subtitle / nrow(articles), 4)),
  "",
  "Examples",
  "--------",
  if (length(subtitle_examples) == 0) "(no usable subtitle/deck examples)" else paste0(seq_along(subtitle_examples), ". ", subtitle_examples)
)
writeLines(quality_lines, file.path(output_dir, "subtitle_data_quality.txt"))

if (rows_with_subtitle < 50) {
  warning("Subtitle/deck data is too sparse for reliable analysis; writing diagnostic outputs only.", call. = FALSE)
  write_empty_csv(file.path(output_dir, "subtitle_terms_by_lift.csv"), c("term", "n_rows_present"))
  write_empty_csv(file.path(output_dir, "subtitle_terms_by_high_performer_rate.csv"), c("term", "n_rows_present"))
  write_empty_csv(file.path(output_dir, "scored_articles_subtitle_models.csv"), c("article_id", "title", "subtitle_deck_text"))
  write_empty_csv(file.path(output_dir, "model_error_examples_subtitle.csv"), c("error_type", "article_id", "title", "subtitle_deck_text"))
  writeLines(c(
    "Subtitle analysis summary",
    "=========================",
    "Subtitle/deck analysis cannot yet be done reliably because there are too few usable subtitle/deck rows.",
    paste("Rows with usable subtitle/deck:", rows_with_subtitle)
  ), file.path(output_dir, "subtitle_analysis_summary.txt"))
  writeLines(c(
    "Model comparison title vs subtitle",
    "==================================",
    "Modeling skipped because subtitle/deck data is too sparse."
  ), file.path(output_dir, "model_comparison_title_vs_subtitle.txt"))
  quit(status = 0)
}

usable <- articles[!is.na(articles$title) & !is.na(articles$subtitle_deck_text) & !is.na(articles$success_score_main), , drop = FALSE]
message("Using ", nrow(usable), " rows with title, subtitle/deck, and success target.")

subtitle_term_sets <- term_sets_for_docs(usable$subtitle_deck_text)
title_term_sets <- term_sets_for_docs(usable$title)
combined_text <- paste(usable$title, usable$subtitle_deck_text)
combined_term_sets <- term_sets_for_docs(combined_text)

subtitle_rows <- rbind(
  build_term_rows(usable$subtitle_deck_text, 1, "unigram"),
  build_term_rows(usable$subtitle_deck_text, 2, "bigram"),
  build_term_rows(usable$subtitle_deck_text, 3, "trigram")
)

term_summary <- summarize_term_rows(subtitle_rows, usable$success_score_main, usable$high_performer_top20_main, min_n = 10)
term_columns <- c(
  "term", "n_rows_present", "median_success_present", "median_success_absent",
  "mean_success_present", "mean_success_absent", "mean_success_lift",
  "high_performer_top20_rate_present", "high_performer_top20_rate_absent",
  "high_performer_top20_rate_lift", "high_performer_top20_rate_lift_low_approx",
  "high_performer_top20_rate_lift_high_approx", "min_n_10", "min_n_20",
  "small_sample_flag", "unstable_direction_flag"
)

if (nrow(term_summary) > 0) {
  by_lift <- term_summary[order(-term_summary$mean_success_lift, -term_summary$n_rows_present, term_summary$term), ]
  by_rate <- term_summary[order(-term_summary$high_performer_top20_rate_lift, -term_summary$n_rows_present, term_summary$term), ]
  write.csv(by_lift, file.path(output_dir, "subtitle_terms_by_lift.csv"), row.names = FALSE)
  write.csv(by_rate, file.path(output_dir, "subtitle_terms_by_high_performer_rate.csv"), row.names = FALSE)
} else {
  write_empty_csv(file.path(output_dir, "subtitle_terms_by_lift.csv"), term_columns)
  write_empty_csv(file.path(output_dir, "subtitle_terms_by_high_performer_rate.csv"), term_columns)
}

set.seed(20260513)
train_index <- sort(sample(seq_len(nrow(usable)), size = floor(0.80 * nrow(usable))))
test_index <- setdiff(seq_len(nrow(usable)), train_index)

make_family_matrix <- function(term_sets, prefix) {
  out <- data.frame(row.names = seq_along(term_sets))
  for (family_name in names(term_families)) {
    out[[paste0(prefix, "_", family_name)]] <- as.integer(contains_family(term_sets, term_families[[family_name]]))
  }
  out
}

title_numeric <- make_basic_numeric_features(usable$title, "title")
subtitle_numeric <- make_basic_numeric_features(usable$subtitle_deck_text, "subtitle")
combined_numeric <- data.frame(title_numeric, subtitle_numeric, check.names = FALSE)

title_family_matrix <- make_family_matrix(title_term_sets, "title")
combined_family_matrix <- make_family_matrix(combined_term_sets, "combined")

subtitle_train_terms <- unlist(subtitle_term_sets[train_index], use.names = FALSE)
subtitle_counts <- sort(table(subtitle_train_terms), decreasing = TRUE)
subtitle_vocab <- names(head(subtitle_counts[subtitle_counts >= 10 & names(subtitle_counts) != ""], 150))
subtitle_text_features <- data.frame(make_term_matrix(subtitle_term_sets, subtitle_vocab, "subtitle_text"), check.names = FALSE)

combined_train_terms <- unlist(combined_term_sets[train_index], use.names = FALSE)
combined_counts <- sort(table(combined_train_terms), decreasing = TRUE)
combined_vocab <- names(head(combined_counts[combined_counts >= 10 & names(combined_counts) != ""], 200))
combined_text_features <- data.frame(make_term_matrix(combined_term_sets, combined_vocab, "combined_text"), check.names = FALSE)

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
  context_categorical$publication_group <- top_level_factor(usable$publication, train_index, top_n = 20, min_n = 8)
}
if ("author" %in% names(usable)) {
  context_categorical$author_group <- top_level_factor(usable$author, train_index, top_n = 20, min_n = 8)
}
if ("observed_page_variants" %in% names(usable)) {
  context_categorical$observed_page_variants_group <- top_level_factor(usable$observed_page_variants, train_index, top_n = 10, min_n = 5)
}
tag_features <- if ("observed_tag_slugs" %in% names(usable)) {
  expand_multi_value_factor(usable$observed_tag_slugs, "tag", train_index, min_n = 10, max_levels = 30)
} else {
  data.frame(row.names = seq_len(nrow(usable)))
}
context_features <- data.frame(context_numeric, context_categorical, tag_features, check.names = FALSE)

model_feature_sets <- list(
  title_only_numeric_plus_term_families = list(
    group = "pre_publication_drafting_time",
    features = data.frame(title_numeric, title_family_matrix, check.names = FALSE)
  ),
  subtitle_only_numeric_plus_text_features = list(
    group = "pre_publication_drafting_time",
    features = data.frame(subtitle_numeric, subtitle_text_features, check.names = FALSE)
  ),
  title_plus_subtitle_numeric_plus_text_features = list(
    group = "pre_publication_drafting_time",
    features = data.frame(combined_numeric, combined_text_features, check.names = FALSE)
  ),
  title_plus_subtitle_numeric_plus_term_families = list(
    group = "pre_publication_drafting_time",
    features = data.frame(combined_numeric, combined_family_matrix, check.names = FALSE)
  ),
  title_plus_subtitle_plus_context = list(
    group = "context_aware_post_observation",
    features = data.frame(combined_numeric, combined_text_features, context_features, check.names = FALSE)
  )
)

model_results <- list()
metrics_table <- data.frame()
scored_articles <- usable
scored_articles$model_split <- NA_character_
scored_articles$model_split[train_index] <- "train"
scored_articles$model_split[test_index] <- "test"

for (model_name in names(model_feature_sets)) {
  result <- evaluate_model(
    model_name,
    model_feature_sets[[model_name]]$group,
    model_feature_sets[[model_name]]$features,
    train_index,
    test_index,
    usable$success_score_main,
    usable$high_performer_top20_main
  )
  model_results[[model_name]] <- result
  metrics_table <- rbind(metrics_table, result$metrics)
  scored_articles[[paste0("pred_success_", model_name)]] <- result$pred_reg_all
  scored_articles[[paste0("pred_high_top20_", model_name)]] <- result$pred_cls_all
}

metrics_table <- metrics_table[order(metrics_table$model_group, -metrics_table$classification_auc, metrics_table$regression_rmse), ]
model_lines <- c(
  "Title vs subtitle/deck model comparison",
  "=======================================",
  "Pre-publication / drafting-time models use only title and/or subtitle/deck text features.",
  "The optional context-aware model includes post-observation controls and should be interpreted as explanatory, not draft-time predictive.",
  "",
  paste("Rows used:", nrow(usable)),
  paste("Train rows:", length(train_index)),
  paste("Test rows:", length(test_index)),
  paste("Subtitle text vocabulary terms used:", length(subtitle_vocab)),
  paste("Title+subtitle text vocabulary terms used:", length(combined_vocab)),
  "",
  capture.output(print(metrics_table, row.names = FALSE))
)
writeLines(model_lines, file.path(output_dir, "model_comparison_title_vs_subtitle.txt"))

best_pre <- metrics_table[metrics_table$model_group == "pre_publication_drafting_time", , drop = FALSE]
best_pre <- best_pre[which.max(ifelse(is.na(best_pre$classification_auc), -Inf, best_pre$classification_auc)), , drop = FALSE]
best_model_name <- best_pre$model[1]
best_pred <- model_results[[best_model_name]]$pred_cls_all
test_probability <- best_pred[test_index]
test_actual <- usable$high_performer_top20_main[test_index]
pred_cutoff <- as.numeric(stats::quantile(test_probability, probs = 0.80, na.rm = TRUE, type = 7))
test_predicted <- test_probability >= pred_cutoff
error_test <- usable[test_index, , drop = FALSE]
error_test$best_pre_publication_model <- best_model_name
error_test$predicted_high_probability <- test_probability
error_test$predicted_high_top20 <- test_predicted
error_test$error_type <- ifelse(
  test_predicted & test_actual,
  "true_positive",
  ifelse(test_predicted & !test_actual, "false_positive", ifelse(!test_predicted & test_actual, "false_negative", "true_negative"))
)

sample_error_type <- function(data, type, n = 25) {
  subset <- data[data$error_type == type, , drop = FALSE]
  if (nrow(subset) == 0) {
    return(subset)
  }
  if (type %in% c("false_positive", "true_positive")) {
    subset <- subset[order(-subset$predicted_high_probability), , drop = FALSE]
  } else if (type == "false_negative") {
    subset <- subset[order(subset$predicted_high_probability), , drop = FALSE]
  } else {
    subset <- subset[order(abs(subset$predicted_high_probability - pred_cutoff)), , drop = FALSE]
  }
  head(subset, n)
}

error_examples <- rbind(
  sample_error_type(error_test, "false_positive"),
  sample_error_type(error_test, "false_negative"),
  sample_error_type(error_test, "true_positive"),
  sample_error_type(error_test, "true_negative")
)
error_columns <- intersect(
  c(
    "error_type", "best_pre_publication_model", "predicted_high_probability", "predicted_high_top20",
    "article_id", "title", "subtitle_deck_text", "publication", "author", "observed_tag_slugs",
    "latest_claps", "latest_responses", "success_score_main", "high_performer_top20_main", "url"
  ),
  names(error_examples)
)
write.csv(error_examples[, error_columns, drop = FALSE], file.path(output_dir, "model_error_examples_subtitle.csv"), row.names = FALSE)

scored_columns <- intersect(
  c(
    "article_id", "medium_post_id", "title", "subtitle_deck_text", "author", "publication",
    "observed_tag_slugs", "latest_claps", "latest_responses", "success_score_main",
    "high_performer_top20_main", "model_split",
    grep("^pred_success_|^pred_high_top20_", names(scored_articles), value = TRUE),
    "url"
  ),
  names(scored_articles)
)
write.csv(scored_articles[, scored_columns, drop = FALSE], file.path(output_dir, "scored_articles_subtitle_models.csv"), row.names = FALSE)

top_terms_lift <- if (nrow(term_summary) > 0) {
  head(term_summary[order(-term_summary$mean_success_lift), c("term", "n_rows_present", "mean_success_lift", "high_performer_top20_rate_lift")], 10)
} else {
  data.frame()
}
top_terms_rate <- if (nrow(term_summary) > 0) {
  head(term_summary[order(-term_summary$high_performer_top20_rate_lift), c("term", "n_rows_present", "mean_success_lift", "high_performer_top20_rate_lift")], 10)
} else {
  data.frame()
}

metric_for <- function(model_name, column_name) {
  row <- metrics_table[metrics_table$model == model_name, , drop = FALSE]
  if (nrow(row) == 0) {
    return(NA_real_)
  }
  row[[column_name]][1]
}

etf_related_terms <- if (nrow(term_summary) > 0) {
  term_summary[grepl("\\b(etf|index|portfolio|fund|funds)\\b", term_summary$term), , drop = FALSE]
} else {
  data.frame()
}
etf_note <- if (nrow(etf_related_terms) > 0) {
  etf_related_terms <- etf_related_terms[order(-etf_related_terms$mean_success_lift), ]
  paste0(
    "ETF/index/portfolio-related subtitle terms are present in the term table. Best mean-lift example: '",
    etf_related_terms$term[1],
    "' with lift ",
    round(etf_related_terms$mean_success_lift[1], 3),
    " across ",
    etf_related_terms$n_rows_present[1],
    " rows."
  )
} else {
  "No ETF/index/portfolio-related subtitle terms passed the minimum frequency threshold."
}

summary_lines <- c(
  "Subtitle/deck analysis summary",
  "==============================",
  "",
  "Scope",
  "-----",
  "Source: v_medium_title_prediction_dataset_v2",
  paste("V2 view rows loaded:", nrow(articles)),
  paste("Rows analyzed:", nrow(usable)),
  paste("Subtitle/deck source column:", subtitle_source_column),
  "This script uses title and subtitle/deck text only for the main pre-publication comparisons.",
  "No OpenAI/API scoring and no SQLite DB writes were performed.",
  "",
  "How many articles have usable subtitles/decks?",
  "---------------------------------------------",
  paste("Usable subtitle/deck rows:", rows_with_subtitle, "of", nrow(articles)),
  paste("Rows with both title and subtitle/deck:", rows_with_both),
  paste("Median subtitle/deck length:", median(subtitle_lengths)),
  "",
  "Does subtitle-only text predict performance at all?",
  "--------------------------------------------------",
  paste0(
    "Subtitle-only model AUC: ",
    round(metric_for("subtitle_only_numeric_plus_text_features", "classification_auc"), 3),
    ", precision@predicted_top20: ",
    round(metric_for("subtitle_only_numeric_plus_text_features", "classification_precision_at_top20_predicted"), 3),
    ", RMSE: ",
    round(metric_for("subtitle_only_numeric_plus_text_features", "regression_rmse"), 3),
    "."
  ),
  "This is exploratory and should be judged against title-only and title+subtitle baselines, not in isolation.",
  "",
  "Does title+subtitle improve over title-only?",
  "--------------------------------------------",
  paste0(
    "Title-only AUC: ",
    round(metric_for("title_only_numeric_plus_term_families", "classification_auc"), 3),
    ", RMSE: ",
    round(metric_for("title_only_numeric_plus_term_families", "regression_rmse"), 3),
    "."
  ),
  paste0(
    "Title+subtitle text AUC: ",
    round(metric_for("title_plus_subtitle_numeric_plus_text_features", "classification_auc"), 3),
    ", RMSE: ",
    round(metric_for("title_plus_subtitle_numeric_plus_text_features", "regression_rmse"), 3),
    "."
  ),
  paste0(
    "Title+subtitle term-family AUC: ",
    round(metric_for("title_plus_subtitle_numeric_plus_term_families", "classification_auc"), 3),
    ", RMSE: ",
    round(metric_for("title_plus_subtitle_numeric_plus_term_families", "regression_rmse"), 3),
    "."
  ),
  "",
  "Which subtitle terms or phrases look promising?",
  "-----------------------------------------------",
  if (nrow(top_terms_lift) == 0) {
    "No subtitle terms passed the minimum frequency threshold."
  } else {
    paste(capture.output(print(top_terms_lift, row.names = FALSE)), collapse = "\n")
  },
  "",
  "Do subtitles help explain technical ETF/index/portfolio titles?",
  "---------------------------------------------------------------",
  etf_note,
  "Use the subtitle term tables and error examples to inspect whether technical titles fail because the deck is generic, too narrow, or collection-surface specific.",
  "",
  "Should we include subtitle/deck text before OpenAI/ChatGPT API title scoring?",
  "----------------------------------------------------------------------------",
  "Yes. Subtitle/deck text is available for most rows and is part of the reader-facing framing. It should be included before adding LLM scoring so future API features are benchmarked against a stronger text-only baseline.",
  "",
  "What should be analyzed next?",
  "-----------------------------",
  "- Add target sensitivity for subtitle models if we want clap-vs-response-specific subtitle effects.",
  "- Review false positives and false negatives from the best pre-publication model.",
  "- Inspect whether subtitle/deck terms reduce the apparent weakness of ETF/index/portfolio title families.",
  "- Only after that, add OpenAI/ChatGPT scoring as an extra benchmark feature."
)

writeLines(summary_lines, file.path(output_dir, "subtitle_analysis_summary.txt"))

message("Saved subtitle/deck analysis outputs to: ", output_dir)
message("Done.")
