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
output_dir <- file.path("data", "analysis", "medium_analysis_v2", "title_baseline")

message("Medium Title Text Baseline Analysis V2")
message("======================================")

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
  words <- words[!(words %in% stopwords)]
  words
}

make_ngrams <- function(words, n) {
  if (length(words) < n) {
    return(character())
  }
  vapply(seq_len(length(words) - n + 1), function(i) paste(words[i:(i + n - 1)], collapse = " "), character(1))
}

build_term_rows <- function(titles, n, term_type) {
  rows <- lapply(seq_along(titles), function(i) {
    words <- split_words(titles[i])
    terms <- unique(make_ngrams(words, n))
    terms <- terms[terms != ""]
    if (length(terms) == 0) {
      return(NULL)
    }
    data.frame(article_index = i, term = terms, term_type = term_type, stringsAsFactors = FALSE)
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame(article_index = integer(), term = character(), term_type = character()))
  }
  do.call(rbind, rows)
}

term_summary <- function(term_rows, success_score, high_performer, min_n = 10) {
  if (nrow(term_rows) == 0) {
    return(data.frame())
  }

  groups <- split(term_rows$article_index, term_rows$term, drop = TRUE)
  all_index <- seq_along(success_score)
  rows <- lapply(names(groups), function(term) {
    present <- sort(unique(groups[[term]]))
    absent <- setdiff(all_index, present)
    n_present <- length(present)

    if (n_present < min_n) {
      return(NULL)
    }

    present_success <- success_score[present]
    absent_success <- success_score[absent]
    present_high <- high_performer[present]
    absent_high <- high_performer[absent]

    present_mean <- mean(present_success, na.rm = TRUE)
    absent_mean <- mean(absent_success, na.rm = TRUE)
    present_rate <- mean(present_high, na.rm = TRUE)
    absent_rate <- mean(absent_high, na.rm = TRUE)

    data.frame(
      term = term,
      n_titles = n_present,
      median_success_present = median(present_success, na.rm = TRUE),
      mean_success_present = present_mean,
      mean_success_absent = absent_mean,
      mean_success_lift = present_mean - absent_mean,
      mean_success_ratio = ifelse(isTRUE(absent_mean > 0), present_mean / absent_mean, NA_real_),
      high_performer_top20_rate_present = present_rate,
      high_performer_top20_rate_absent = absent_rate,
      high_performer_top20_rate_lift = present_rate - absent_rate,
      stringsAsFactors = FALSE
    )
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.frame())
  }

  out <- do.call(rbind, rows)
  out[order(-out$mean_success_lift, -out$n_titles, out$term), ]
}

write_empty_csv <- function(path, columns) {
  write.csv(as.data.frame(setNames(replicate(length(columns), logical(0), simplify = FALSE), columns)), path, row.names = FALSE)
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

r_squared <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) < 2) {
    return(NA_real_)
  }
  value <- suppressWarnings(cor(actual[ok], predicted[ok]))
  value^2
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

term_sets_for_docs <- function(titles) {
  lapply(titles, function(title) {
    words <- split_words(title)
    unique(c(make_ngrams(words, 1), make_ngrams(words, 2)))
  })
}

make_term_matrix <- function(term_sets, vocabulary) {
  if (length(vocabulary) == 0) {
    return(matrix(nrow = length(term_sets), ncol = 0))
  }

  mat <- matrix(0L, nrow = length(term_sets), ncol = length(vocabulary))
  colnames(mat) <- paste0("term_", make.names(vocabulary, unique = TRUE))
  names(vocabulary) <- colnames(mat)

  vocabulary_lookup <- setNames(seq_along(vocabulary), vocabulary)
  for (i in seq_along(term_sets)) {
    present <- intersect(term_sets[[i]], vocabulary)
    if (length(present) > 0) {
      mat[i, vocabulary_lookup[present]] <- 1L
    }
  }
  mat
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
    warning = function(warning) {
      suppressWarnings(as.numeric(stats::predict(model, newdata = newdata, type = type)))
    },
    error = function(error) {
      warning("Prediction failed: ", conditionMessage(error), call. = FALSE)
      rep(NA_real_, nrow(newdata))
    }
  )
}

needed_columns <- c("title", "success_score", "high_performer_top20")
missing_columns <- setdiff(needed_columns, names(articles))
if (length(missing_columns) > 0) {
  stop("Dataset is missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

articles$title <- clean_text_vector(articles$title)
articles$success_score <- suppressWarnings(as.numeric(articles$success_score))
articles$latest_claps <- if ("latest_claps" %in% names(articles)) suppressWarnings(as.numeric(articles$latest_claps)) else NA_real_
articles$latest_responses <- if ("latest_responses" %in% names(articles)) suppressWarnings(as.numeric(articles$latest_responses)) else NA_real_
articles$high_performer_top20 <- as_logical_clean(articles$high_performer_top20)

usable <- articles[!is.na(articles$title) & !is.na(articles$success_score), , drop = FALSE]

message("Loaded ", nrow(articles), " article rows.")
message("Using ", nrow(usable), " rows with title and success_score.")

quality_lines <- c(
  "Title baseline data quality summary",
  "===================================",
  "Source: v_medium_title_prediction_dataset_v2",
  paste("V2 view rows loaded:", nrow(articles)),
  "No OpenAI/API scoring and no SQLite DB writes were performed.",
  paste("Rows in dataset:", nrow(articles)),
  paste("Rows with title:", sum(!is.na(articles$title))),
  paste("Rows with success_score:", sum(!is.na(articles$success_score))),
  paste("Rows used for title analysis:", nrow(usable)),
  paste("Rows marked high_performer_top20:", sum(usable$high_performer_top20, na.rm = TRUE)),
  paste("Median success_score:", round(median(usable$success_score, na.rm = TRUE), 4)),
  paste("Mean success_score:", round(mean(usable$success_score, na.rm = TRUE), 4))
)

unigram_rows <- build_term_rows(usable$title, 1, "unigram")
bigram_rows <- build_term_rows(usable$title, 2, "bigram")
trigram_rows <- build_term_rows(usable$title, 3, "trigram")

unigram_summary <- term_summary(unigram_rows, usable$success_score, usable$high_performer_top20, min_n = 10)
bigram_summary <- term_summary(bigram_rows, usable$success_score, usable$high_performer_top20, min_n = 10)
trigram_summary <- term_summary(trigram_rows, usable$success_score, usable$high_performer_top20, min_n = 10)

unigram_summary_min20 <- term_summary(unigram_rows, usable$success_score, usable$high_performer_top20, min_n = 20)
bigram_summary_min20 <- term_summary(bigram_rows, usable$success_score, usable$high_performer_top20, min_n = 20)

summary_columns <- c(
  "term",
  "n_titles",
  "median_success_present",
  "mean_success_present",
  "mean_success_absent",
  "mean_success_lift",
  "mean_success_ratio",
  "high_performer_top20_rate_present",
  "high_performer_top20_rate_absent",
  "high_performer_top20_rate_lift"
)

if (nrow(unigram_summary) > 0) {
  write.csv(unigram_summary, file.path(output_dir, "top_unigrams_by_lift.csv"), row.names = FALSE)
} else {
  write_empty_csv(file.path(output_dir, "top_unigrams_by_lift.csv"), summary_columns)
}

if (nrow(bigram_summary) > 0) {
  write.csv(bigram_summary, file.path(output_dir, "top_bigrams_by_lift.csv"), row.names = FALSE)
} else {
  write_empty_csv(file.path(output_dir, "top_bigrams_by_lift.csv"), summary_columns)
}

if (nrow(trigram_summary) > 0) {
  write.csv(trigram_summary, file.path(output_dir, "top_trigrams_by_lift.csv"), row.names = FALSE)
} else {
  write_empty_csv(file.path(output_dir, "top_trigrams_by_lift.csv"), summary_columns)
}

if (nrow(unigram_summary_min20) > 0) {
  write.csv(unigram_summary_min20, file.path(output_dir, "top_unigrams_by_lift_min20.csv"), row.names = FALSE)
} else {
  write_empty_csv(file.path(output_dir, "top_unigrams_by_lift_min20.csv"), summary_columns)
}
if (nrow(bigram_summary_min20) > 0) {
  write.csv(bigram_summary_min20, file.path(output_dir, "top_bigrams_by_lift_min20.csv"), row.names = FALSE)
} else {
  write_empty_csv(file.path(output_dir, "top_bigrams_by_lift_min20.csv"), summary_columns)
}

all_terms <- rbind(
  data.frame(term_type = "unigram", unigram_summary, stringsAsFactors = FALSE),
  data.frame(term_type = "bigram", bigram_summary, stringsAsFactors = FALSE),
  data.frame(term_type = "trigram", trigram_summary, stringsAsFactors = FALSE)
)

if (nrow(all_terms) > 0) {
  all_terms <- all_terms[order(-all_terms$high_performer_top20_rate_lift, -all_terms$n_titles, all_terms$term), ]
  write.csv(all_terms, file.path(output_dir, "top_terms_by_high_performer_rate.csv"), row.names = FALSE)
} else {
  write_empty_csv(file.path(output_dir, "top_terms_by_high_performer_rate.csv"), c("term_type", summary_columns))
}

article_columns <- intersect(
  c(
    "article_id",
    "medium_post_id",
    "title",
    "subtitle",
    "author",
    "publication",
    "url",
    "latest_claps",
    "latest_responses",
    "success_score",
    "high_performer_top20",
    "high_performer_top10",
    "observed_tag_slugs"
  ),
  names(usable)
)
top_articles <- usable[order(-usable$success_score), article_columns, drop = FALSE]
write.csv(head(top_articles, 100), file.path(output_dir, "top_articles_by_success.csv"), row.names = FALSE)

quality_lines <- c(
  quality_lines,
  "",
  "Term counts after minimum frequency filtering",
  "--------------------------------------------",
  paste("Unigrams with n >= 10:", nrow(unigram_summary)),
  paste("Bigrams with n >= 10:", nrow(bigram_summary)),
  paste("Trigrams with n >= 10:", nrow(trigram_summary)),
  paste("Unigrams with n >= 20:", nrow(unigram_summary_min20)),
  paste("Bigrams with n >= 20:", nrow(bigram_summary_min20))
)

writeLines(quality_lines, file.path(output_dir, "data_quality_summary.txt"))

message("Saved term summaries and top article outputs to: ", output_dir)

model_lines <- c(
  "Baseline model metrics",
  "======================",
  "These models are exploratory correlations from observed titles and public stats.",
  "They are not evidence that title words causally improve performance.",
  ""
)

scored_articles <- usable
scored_articles$model_split <- NA_character_
scored_articles$pred_success_numeric <- NA_real_
scored_articles$pred_success_text <- NA_real_
scored_articles$pred_high_top20_numeric <- NA_real_
scored_articles$pred_high_top20_text <- NA_real_

enough_rows <- nrow(usable) >= 100 && sum(usable$high_performer_top20, na.rm = TRUE) >= 20 && sum(!usable$high_performer_top20, na.rm = TRUE) >= 20

if (!enough_rows) {
  warning("Not enough usable rows or high performer examples for baseline modeling; writing placeholder model outputs.", call. = FALSE)
  model_lines <- c(
    model_lines,
    paste("Modeling skipped. Usable rows:", nrow(usable)),
    paste("High performer rows:", sum(usable$high_performer_top20, na.rm = TRUE))
  )
} else {
  set.seed(20260513)
  train_index <- sort(sample(seq_len(nrow(usable)), size = floor(0.80 * nrow(usable))))
  test_index <- setdiff(seq_len(nrow(usable)), train_index)

  scored_articles$model_split[train_index] <- "train"
  scored_articles$model_split[test_index] <- "test"

  numeric_features <- data.frame(
    title_char_count = nchar(usable$title),
    title_word_count = vapply(strsplit(normalize_title(usable$title), " "), length, integer(1)),
    title_has_number = as.integer(grepl("[0-9]", usable$title)),
    title_has_question_mark = as.integer(grepl("\\?", usable$title)),
    title_has_colon = as.integer(grepl(":", usable$title)),
    title_has_dollar = as.integer(grepl("\\$", usable$title)),
    stringsAsFactors = FALSE
  )

  term_sets <- term_sets_for_docs(usable$title)
  train_terms <- unlist(term_sets[train_index], use.names = FALSE)
  term_counts <- sort(table(train_terms), decreasing = TRUE)
  term_counts <- term_counts[term_counts >= 10]
  term_counts <- term_counts[names(term_counts) != ""]
  vocabulary <- names(head(term_counts, 150))

  text_matrix <- make_term_matrix(term_sets, vocabulary)
  text_features <- data.frame(numeric_features, text_matrix, check.names = FALSE)

  x_train_numeric <- numeric_features[train_index, , drop = FALSE]
  x_test_numeric <- numeric_features[test_index, , drop = FALSE]
  x_all_numeric <- numeric_features

  x_train_text <- text_features[train_index, , drop = FALSE]
  x_test_text <- text_features[test_index, , drop = FALSE]
  x_all_text <- text_features

  y_train_reg <- usable$success_score[train_index]
  y_test_reg <- usable$success_score[test_index]
  y_train_cls <- usable$high_performer_top20[train_index]
  y_test_cls <- usable$high_performer_top20[test_index]

  lm_numeric <- fit_lm_safely(x_train_numeric, y_train_reg)
  lm_text <- fit_lm_safely(x_train_text, y_train_reg)
  glm_numeric <- fit_glm_safely(x_train_numeric, y_train_cls)
  glm_text <- fit_glm_safely(x_train_text, y_train_cls)

  pred_test_lm_numeric <- predict_safely(lm_numeric, x_test_numeric, type = "response")
  pred_test_lm_text <- predict_safely(lm_text, x_test_text, type = "response")
  pred_test_glm_numeric <- predict_safely(glm_numeric, x_test_numeric, type = "response")
  pred_test_glm_text <- predict_safely(glm_text, x_test_text, type = "response")

  scored_articles$pred_success_numeric <- predict_safely(lm_numeric, x_all_numeric, type = "response")
  scored_articles$pred_success_text <- predict_safely(lm_text, x_all_text, type = "response")
  scored_articles$pred_high_top20_numeric <- predict_safely(glm_numeric, x_all_numeric, type = "response")
  scored_articles$pred_high_top20_text <- predict_safely(glm_text, x_all_text, type = "response")

  numeric_cls <- classification_metrics(y_test_cls, pred_test_glm_numeric)
  text_cls <- classification_metrics(y_test_cls, pred_test_glm_text)

  metric_table <- data.frame(
    model = c("numeric_features_only", "title_text_features"),
    train_rows = length(train_index),
    test_rows = length(test_index),
    text_vocabulary_terms = c(0, length(vocabulary)),
    regression_rmse = c(rmse(y_test_reg, pred_test_lm_numeric), rmse(y_test_reg, pred_test_lm_text)),
    regression_mae = c(mae(y_test_reg, pred_test_lm_numeric), mae(y_test_reg, pred_test_lm_text)),
    regression_r_squared = c(r_squared(y_test_reg, pred_test_lm_numeric), r_squared(y_test_reg, pred_test_lm_text)),
    classification_accuracy = c(numeric_cls["accuracy"], text_cls["accuracy"]),
    classification_precision_at_top20_predicted = c(numeric_cls["precision"], text_cls["precision"]),
    classification_recall_at_top20_predicted = c(numeric_cls["recall"], text_cls["recall"]),
    classification_auc = c(numeric_cls["auc"], text_cls["auc"]),
    stringsAsFactors = FALSE
  )

  model_lines <- c(
    model_lines,
    paste("Train rows:", length(train_index)),
    paste("Test rows:", length(test_index)),
    paste("High performer rate in full usable data:", round(mean(usable$high_performer_top20, na.rm = TRUE), 4)),
    paste("Title text vocabulary terms used:", length(vocabulary)),
    "",
    capture.output(print(metric_table, row.names = FALSE)),
    "",
    "Model definitions:",
    "- numeric_features_only: title length, word count, and simple punctuation/number flags.",
    "- title_text_features: numeric features plus binary title unigram/bigram indicators seen in at least 10 training titles, capped at 150 terms.",
    "",
    "Classification threshold:",
    "- Articles are classified as predicted high performers by taking the top 20% of predicted probabilities in the test set."
  )
}

writeLines(model_lines, file.path(output_dir, "model_metrics.txt"))

scored_columns <- intersect(
  c(
    "article_id",
    "medium_post_id",
    "title",
    "author",
    "publication",
    "url",
    "latest_claps",
    "latest_responses",
    "success_score",
    "high_performer_top20",
    "model_split",
    "pred_success_numeric",
    "pred_success_text",
    "pred_high_top20_numeric",
    "pred_high_top20_text"
  ),
  names(scored_articles)
)

write.csv(scored_articles[, scored_columns, drop = FALSE], file.path(output_dir, "scored_articles_baseline.csv"), row.names = FALSE)

message("Saved model outputs to: ", output_dir)
message("Done.")
