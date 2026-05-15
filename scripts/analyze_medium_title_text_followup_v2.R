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
output_dir <- file.path("data", "analysis", "medium_analysis_v2", "title_followup")

message("Medium Title Text Follow-up Analysis V2")
message("=======================================")

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

term_sets_for_docs <- function(titles) {
  lapply(titles, function(title) {
    words <- split_words(title)
    unique(c(make_ngrams(words, 1), make_ngrams(words, 2), make_ngrams(words, 3)))
  })
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

contains_family <- function(term_sets, terms) {
  vapply(term_sets, function(article_terms) any(terms %in% article_terms), logical(1))
}

se_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) {
    return(NA_real_)
  }
  stats::sd(x) / sqrt(length(x))
}

se_rate <- function(rate, n) {
  if (is.na(rate) || n <= 0) {
    return(NA_real_)
  }
  sqrt(rate * (1 - rate) / n)
}

metric_summary <- function(present, success_score, high_performer) {
  absent <- !present
  present_success <- success_score[present]
  absent_success <- success_score[absent]
  present_high <- high_performer[present]
  absent_high <- high_performer[absent]

  n_present <- sum(present, na.rm = TRUE)
  n_absent <- sum(absent, na.rm = TRUE)
  mean_present <- mean(present_success, na.rm = TRUE)
  mean_absent <- mean(absent_success, na.rm = TRUE)
  rate_present <- mean(present_high, na.rm = TRUE)
  rate_absent <- mean(absent_high, na.rm = TRUE)

  mean_diff <- mean_present - mean_absent
  mean_present_se <- se_mean(present_success)
  mean_diff_se <- sqrt(se_mean(present_success)^2 + se_mean(absent_success)^2)
  rate_diff <- rate_present - rate_absent
  rate_present_se <- se_rate(rate_present, n_present)
  rate_diff_se <- sqrt(se_rate(rate_present, n_present)^2 + se_rate(rate_absent, n_absent)^2)

  data.frame(
    n_titles_present = n_present,
    n_titles_absent = n_absent,
    median_success_present = median(present_success, na.rm = TRUE),
    mean_success_present = mean_present,
    mean_success_present_se = mean_present_se,
    mean_success_absent = mean_absent,
    mean_success_lift = mean_diff,
    mean_success_lift_se = mean_diff_se,
    mean_success_lift_low_approx = mean_diff - 1.96 * mean_diff_se,
    mean_success_lift_high_approx = mean_diff + 1.96 * mean_diff_se,
    high_performer_top20_rate_present = rate_present,
    high_performer_top20_rate_present_se = rate_present_se,
    high_performer_top20_rate_present_low_approx = pmax(0, rate_present - 1.96 * rate_present_se),
    high_performer_top20_rate_present_high_approx = pmin(1, rate_present + 1.96 * rate_present_se),
    high_performer_top20_rate_absent = rate_absent,
    high_performer_top20_rate_lift = rate_diff,
    high_performer_top20_rate_lift_se = rate_diff_se,
    high_performer_top20_rate_lift_low_approx = rate_diff - 1.96 * rate_diff_se,
    high_performer_top20_rate_lift_high_approx = rate_diff + 1.96 * rate_diff_se,
    min_n_10 = n_present >= 10,
    min_n_20 = n_present >= 20,
    small_sample_flag = n_present < 20,
    wide_interval_flag = !is.na(rate_diff_se) && (1.96 * rate_diff_se > 0.15),
    unstable_direction_flag = !is.na(rate_diff_se) && ((rate_diff - 1.96 * rate_diff_se) <= 0 && (rate_diff + 1.96 * rate_diff_se) >= 0),
    stringsAsFactors = FALSE
  )
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

make_term_matrix <- function(term_sets, vocabulary) {
  if (length(vocabulary) == 0) {
    return(matrix(nrow = length(term_sets), ncol = 0))
  }

  mat <- matrix(0L, nrow = length(term_sets), ncol = length(vocabulary))
  colnames(mat) <- paste0("raw_text_", make.names(vocabulary, unique = TRUE))
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
  replacement <- median(x, na.rm = TRUE)
  x[is.na(x)] <- replacement
  x
}

top_level_factor <- function(x, train_index, top_n = 20, min_n = 8) {
  value <- clean_text_vector(x)
  train_values <- value[train_index]
  counts <- sort(table(train_values[!is.na(train_values)]), decreasing = TRUE)
  keep <- names(counts[counts >= min_n])
  keep <- head(keep, top_n)
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
    col_name <- paste0(prefix, "_", make.names(level))
    out[[col_name]] <- as.integer(vapply(split_values, function(parts) level %in% parts, logical(1)))
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
    warning = function(warning) {
      suppressWarnings(as.numeric(stats::predict(model, newdata = newdata, type = type)))
    },
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

write_empty_csv <- function(path, columns) {
  write.csv(as.data.frame(setNames(replicate(length(columns), logical(0), simplify = FALSE), columns)), path, row.names = FALSE)
}

required_columns <- c("title", "success_score", "high_performer_top20")
missing_columns <- setdiff(required_columns, names(articles))
if (length(missing_columns) > 0) {
  stop("Dataset is missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

articles$title <- clean_text_vector(articles$title)
articles$success_score <- suppressWarnings(as.numeric(articles$success_score))
articles$high_performer_top20 <- as_logical_clean(articles$high_performer_top20)
articles$latest_claps <- if ("latest_claps" %in% names(articles)) suppressWarnings(as.numeric(articles$latest_claps)) else NA_real_
articles$latest_responses <- if ("latest_responses" %in% names(articles)) suppressWarnings(as.numeric(articles$latest_responses)) else NA_real_

usable <- articles[!is.na(articles$title) & !is.na(articles$success_score) & !is.na(articles$high_performer_top20), , drop = FALSE]

message("Loaded ", nrow(articles), " rows.")
message("Using ", nrow(usable), " rows with title, success_score, and high_performer_top20.")

term_sets <- term_sets_for_docs(usable$title)

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

family_matrix <- data.frame(row.names = seq_len(nrow(usable)))
for (family_name in names(term_families)) {
  family_matrix[[family_name]] <- as.integer(contains_family(term_sets, term_families[[family_name]]))
}

family_summary_rows <- lapply(names(term_families), function(family_name) {
  present <- family_matrix[[family_name]] == 1
  data.frame(
    family = family_name,
    matched_terms = paste(term_families[[family_name]], collapse = "; "),
    metric_summary(present, usable$success_score, usable$high_performer_top20),
    stringsAsFactors = FALSE
  )
})
family_summary <- do.call(rbind, family_summary_rows)
family_summary <- family_summary[order(-family_summary$mean_success_lift, -family_summary$n_titles_present), ]
write.csv(family_summary, file.path(output_dir, "term_family_summary.csv"), row.names = FALSE)

example_rows <- list()
for (family_name in names(term_families)) {
  present_index <- which(family_matrix[[family_name]] == 1)
  if (length(present_index) == 0) {
    next
  }
  top_index <- head(present_index[order(usable$success_score[present_index], decreasing = TRUE)], 5)
  low_index <- head(present_index[order(usable$success_score[present_index], decreasing = FALSE)], 5)
  selected <- c(top_index, low_index)
  example_rows[[family_name]] <- data.frame(
    family = family_name,
    example_type = c(rep("top_performer_in_family", length(top_index)), rep("low_performer_in_family", length(low_index))),
    article_id = usable$article_id[selected],
    title = usable$title[selected],
    publication = if ("publication" %in% names(usable)) usable$publication[selected] else NA_character_,
    observed_tag_slugs = if ("observed_tag_slugs" %in% names(usable)) usable$observed_tag_slugs[selected] else NA_character_,
    latest_claps = usable$latest_claps[selected],
    latest_responses = usable$latest_responses[selected],
    success_score = usable$success_score[selected],
    high_performer_top20 = usable$high_performer_top20[selected],
    stringsAsFactors = FALSE
  )
}
family_examples <- if (length(example_rows) > 0) do.call(rbind, example_rows) else data.frame()
if (nrow(family_examples) > 0) {
  write.csv(family_examples, file.path(output_dir, "term_family_examples.csv"), row.names = FALSE)
} else {
  write_empty_csv(file.path(output_dir, "term_family_examples.csv"), c("family", "example_type", "article_id", "title"))
}

unigram_rows <- build_term_rows(usable$title, 1, "unigram")
unigram_terms <- sort(unique(unigram_rows$term))
unigram_groups <- split(unigram_rows$article_index, unigram_rows$term, drop = TRUE)

stability_rows <- lapply(unigram_terms, function(term) {
  present <- rep(FALSE, nrow(usable))
  present[unique(unigram_groups[[term]])] <- TRUE
  data.frame(
    item_type = "unigram",
    item = term,
    matched_terms = term,
    metric_summary(present, usable$success_score, usable$high_performer_top20),
    stringsAsFactors = FALSE
  )
})

family_stability_rows <- lapply(names(term_families), function(family_name) {
  present <- family_matrix[[family_name]] == 1
  data.frame(
    item_type = "term_family",
    item = family_name,
    matched_terms = paste(term_families[[family_name]], collapse = "; "),
    metric_summary(present, usable$success_score, usable$high_performer_top20),
    stringsAsFactors = FALSE
  )
})

term_lift_stability <- do.call(rbind, c(stability_rows, family_stability_rows))
term_lift_stability <- term_lift_stability[term_lift_stability$n_titles_present >= 10, ]
term_lift_stability <- term_lift_stability[order(-term_lift_stability$mean_success_lift, -term_lift_stability$n_titles_present), ]
write.csv(term_lift_stability, file.path(output_dir, "term_lift_stability.csv"), row.names = FALSE)

enough_rows <- nrow(usable) >= 100 &&
  sum(usable$high_performer_top20, na.rm = TRUE) >= 20 &&
  sum(!usable$high_performer_top20, na.rm = TRUE) >= 20

model_lines <- c(
  "Model comparison with title-only and context-aware features",
  "==========================================================",
  "Title-only / pre-publication models use only title length, punctuation, term families, and raw title text.",
  "Context-aware / post-observation models may additionally use observed publication/author/tag/rank/age variables.",
  "Context-aware variables are useful explanatory controls but are not available when drafting a new article title.",
  ""
)

scored_articles <- usable
scored_articles$model_split <- NA_character_

model_results <- list()
metrics_table <- data.frame()
best_model_name <- NA_character_
best_model_group <- NA_character_
best_pred_cls <- rep(NA_real_, nrow(usable))

if (!enough_rows) {
  warning("Not enough usable rows or high performer examples for model comparison.", call. = FALSE)
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

  normalized_titles <- normalize_title(usable$title)
  split_normalized_titles <- strsplit(normalized_titles, " ")
  title_word_count <- vapply(split_normalized_titles, function(words) sum(words != ""), integer(1))

  numeric_title_features <- data.frame(
    title_char_count = nchar(usable$title),
    title_word_count = title_word_count,
    title_has_number = as.integer(grepl("[0-9]", usable$title)),
    title_has_question_mark = as.integer(grepl("\\?", usable$title)),
    title_has_colon = as.integer(grepl(":", usable$title)),
    title_has_dollar = as.integer(grepl("\\$", usable$title)),
    stringsAsFactors = FALSE
  )

  train_terms <- unlist(term_sets[train_index], use.names = FALSE)
  term_counts <- sort(table(train_terms), decreasing = TRUE)
  vocabulary <- names(head(term_counts[term_counts >= 10 & names(term_counts) != ""], 150))
  raw_text_matrix <- make_term_matrix(term_sets, vocabulary)
  raw_text_features <- data.frame(raw_text_matrix, check.names = FALSE)

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
    title_only_numeric = list(group = "title_only_pre_publication", features = numeric_title_features),
    title_only_numeric_plus_term_families = list(group = "title_only_pre_publication", features = data.frame(numeric_title_features, family_matrix, check.names = FALSE)),
    title_only_numeric_plus_raw_text = list(group = "title_only_pre_publication", features = data.frame(numeric_title_features, raw_text_features, check.names = FALSE)),
    context_aware_numeric_plus_context = list(group = "context_aware_post_observation", features = data.frame(numeric_title_features, context_features, check.names = FALSE)),
    context_aware_context_plus_term_families = list(group = "context_aware_post_observation", features = data.frame(numeric_title_features, context_features, family_matrix, check.names = FALSE))
  )

  for (model_name in names(model_feature_sets)) {
    model_group <- model_feature_sets[[model_name]]$group
    features <- model_feature_sets[[model_name]]$features
    result <- evaluate_model(
      model_name,
      model_group,
      features,
      train_index,
      test_index,
      usable$success_score,
      usable$high_performer_top20
    )
    model_results[[model_name]] <- result
    metrics_table <- rbind(metrics_table, result$metrics)
    scored_articles[[paste0("pred_success_", model_name)]] <- result$pred_reg_all
    scored_articles[[paste0("pred_high_top20_", model_name)]] <- result$pred_cls_all
  }

  metrics_table <- metrics_table[order(metrics_table$model_group, -metrics_table$classification_auc, metrics_table$regression_rmse), ]
  best_row <- metrics_table[which.max(ifelse(is.na(metrics_table$classification_auc), -Inf, metrics_table$classification_auc)), , drop = FALSE]
  best_model_name <- best_row$model[1]
  best_model_group <- best_row$model_group[1]
  best_pred_cls <- model_results[[best_model_name]]$pred_cls_all

  model_lines <- c(
    model_lines,
    paste("Rows used:", nrow(usable)),
    paste("Train rows:", length(train_index)),
    paste("Test rows:", length(test_index)),
    paste("High performer rate:", round(mean(usable$high_performer_top20), 4)),
    paste("Raw title vocabulary terms used:", length(vocabulary)),
    paste("Context feature columns used:", ncol(context_features)),
    "",
    capture.output(print(metrics_table, row.names = FALSE)),
    "",
    "Interpretation guardrails:",
    "- Title-only models are the fair comparison for drafting a new article title.",
    "- Context-aware models can explain observed outcomes but include variables collected after publication or discovery.",
    "- Any improvement from context-aware models should not be interpreted as title wording alone."
  )

  test_actual <- usable$high_performer_top20[test_index]
  test_probability <- best_pred_cls[test_index]
  cutoff <- as.numeric(stats::quantile(test_probability, probs = 0.80, na.rm = TRUE, type = 7))
  test_predicted <- test_probability >= cutoff

  error_test <- usable[test_index, , drop = FALSE]
  error_test$best_model <- best_model_name
  error_test$best_model_group <- best_model_group
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
      subset <- subset[order(abs(subset$predicted_high_probability - cutoff)), , drop = FALSE]
    }
    head(subset, n)
  }

  model_error_examples <- rbind(
    sample_error_type(error_test, "false_positive"),
    sample_error_type(error_test, "false_negative"),
    sample_error_type(error_test, "true_positive"),
    sample_error_type(error_test, "true_negative")
  )

  error_columns <- intersect(
    c(
      "error_type", "best_model", "best_model_group", "predicted_high_probability", "predicted_high_top20",
      "article_id", "title", "publication", "author", "observed_tag_slugs", "times_seen",
      "best_rank", "average_rank", "latest_claps", "latest_responses", "success_score", "high_performer_top20", "url"
    ),
    names(model_error_examples)
  )
  write.csv(model_error_examples[, error_columns, drop = FALSE], file.path(output_dir, "model_error_examples.csv"), row.names = FALSE)
}

writeLines(model_lines, file.path(output_dir, "model_comparison_with_context.txt"))

scored_columns <- intersect(
  c(
    "article_id", "medium_post_id", "title", "author", "publication", "observed_tag_slugs",
    "observed_page_variants", "times_seen", "best_rank", "average_rank",
    "latest_claps", "latest_responses", "success_score", "high_performer_top20",
    "model_split",
    grep("^pred_success_|^pred_high_top20_", names(scored_articles), value = TRUE),
    "url"
  ),
  names(scored_articles)
)
write.csv(scored_articles[, scored_columns, drop = FALSE], file.path(output_dir, "scored_articles_followup.csv"), row.names = FALSE)

if (!file.exists(file.path(output_dir, "model_error_examples.csv"))) {
  write_empty_csv(file.path(output_dir, "model_error_examples.csv"), c("error_type", "best_model", "article_id", "title"))
}

get_family_row <- function(family_name) {
  row <- family_summary[family_summary$family == family_name, , drop = FALSE]
  if (nrow(row) == 0) {
    return(NULL)
  }
  row
}

fmt_rate <- function(x) {
  ifelse(is.na(x), "NA", paste0(round(100 * x, 1), "%"))
}

retirement_row <- get_family_row("retirement_family")
etf_row <- get_family_row("etf_family")
index_row <- get_family_row("index_family")
portfolio_row <- get_family_row("portfolio_family")

title_only_metrics <- if (nrow(metrics_table) > 0) metrics_table[metrics_table$model_group == "title_only_pre_publication", , drop = FALSE] else data.frame()
context_metrics <- if (nrow(metrics_table) > 0) metrics_table[metrics_table$model_group == "context_aware_post_observation", , drop = FALSE] else data.frame()

best_title_only <- if (nrow(title_only_metrics) > 0) {
  title_only_metrics[which.max(ifelse(is.na(title_only_metrics$classification_auc), -Inf, title_only_metrics$classification_auc)), , drop = FALSE]
} else {
  data.frame()
}
best_context <- if (nrow(context_metrics) > 0) {
  context_metrics[which.max(ifelse(is.na(context_metrics$classification_auc), -Inf, context_metrics$classification_auc)), , drop = FALSE]
} else {
  data.frame()
}

summary_lines <- c(
  "Medium title follow-up summary",
  "==============================",
  "",
  "Scope",
  "-----",
  "Source: v_medium_title_prediction_dataset_v2",
  paste("V2 view rows loaded:", nrow(articles)),
  paste("Rows analyzed:", nrow(usable)),
  "This follow-up uses title text only for title-only / pre-publication models.",
  "Context-aware / post-observation models add publication, author, observed tags/page variants, rank, times_seen, and article age when available.",
  "No subtitle/deck text and no OpenAI/API title scoring are used here.",
  "No SQLite DB writes were performed.",
  "",
  "Are retirement-related findings robust?",
  "----------------------------------------"
)

if (!is.null(retirement_row)) {
  summary_lines <- c(
    summary_lines,
    paste0(
      "Retirement family appears in ", retirement_row$n_titles_present,
      " titles. Its high-performer rate is ", fmt_rate(retirement_row$high_performer_top20_rate_present),
      " versus ", fmt_rate(retirement_row$high_performer_top20_rate_absent),
      " when absent, with mean success lift ",
      round(retirement_row$mean_success_lift, 3), "."
    ),
    paste0(
      "Approximate rate-lift interval: ",
      round(retirement_row$high_performer_top20_rate_lift_low_approx, 3),
      " to ",
      round(retirement_row$high_performer_top20_rate_lift_high_approx, 3),
      "."
    ),
    if (isTRUE(retirement_row$small_sample_flag)) {
      "Caution: retirement sample is small."
    } else if (isTRUE(retirement_row$unstable_direction_flag)) {
      "Caution: the interval crosses zero, so direction is not stable by this simple check."
    } else {
      "By the simple count and interval checks, this is one of the more stable title-family signals."
    }
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "Are ETF/index/portfolio terms really weak?",
  "------------------------------------------"
)

weak_rows <- rbind(etf_row, index_row, portfolio_row)
if (!is.null(weak_rows) && nrow(weak_rows) > 0) {
  for (i in seq_len(nrow(weak_rows))) {
    summary_lines <- c(
      summary_lines,
      paste0(
        weak_rows$family[i], ": n=", weak_rows$n_titles_present[i],
        ", high-performer rate=", fmt_rate(weak_rows$high_performer_top20_rate_present[i]),
        ", absent rate=", fmt_rate(weak_rows$high_performer_top20_rate_absent[i]),
        ", mean success lift=", round(weak_rows$mean_success_lift[i], 3),
        "."
      )
    )
  }
  summary_lines <- c(
    summary_lines,
    "Treat these as potentially confounded. ETF/index/portfolio terms are closely tied to observed tags, page surfaces, and article framing, so weak raw lifts may reflect where those articles were collected or how formulaic the titles are rather than the terms themselves."
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "Do term families improve prediction over numeric title features only?",
  "--------------------------------------------------------------------"
)

if (nrow(title_only_metrics) > 0) {
  numeric_row <- title_only_metrics[title_only_metrics$model == "title_only_numeric", , drop = FALSE]
  family_row <- title_only_metrics[title_only_metrics$model == "title_only_numeric_plus_term_families", , drop = FALSE]
  raw_row <- title_only_metrics[title_only_metrics$model == "title_only_numeric_plus_raw_text", , drop = FALSE]
  summary_lines <- c(
    summary_lines,
    paste0("Numeric title-only AUC: ", round(numeric_row$classification_auc, 3), ", RMSE: ", round(numeric_row$regression_rmse, 3), "."),
    paste0("Numeric + term families AUC: ", round(family_row$classification_auc, 3), ", RMSE: ", round(family_row$regression_rmse, 3), "."),
    paste0("Numeric + raw text AUC: ", round(raw_row$classification_auc, 3), ", RMSE: ", round(raw_row$regression_rmse, 3), "."),
    "Use the title-only group as the fair drafting-time comparison."
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "Do context controls explain more than title wording?",
  "---------------------------------------------------"
)

if (nrow(best_title_only) > 0 && nrow(best_context) > 0) {
  summary_lines <- c(
    summary_lines,
    paste0("Best title-only model: ", best_title_only$model, " with AUC ", round(best_title_only$classification_auc, 3), " and RMSE ", round(best_title_only$regression_rmse, 3), "."),
    paste0("Best context-aware model: ", best_context$model, " with AUC ", round(best_context$classification_auc, 3), " and RMSE ", round(best_context$regression_rmse, 3), "."),
    "If context-aware metrics are better, interpret that as explanation of observed outcomes, not as a deployable title-quality model for drafts."
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "Before adding OpenAI/ChatGPT title scoring",
  "------------------------------------------",
  "- Add subtitle/deck analysis next; many Medium titles depend on the subtitle for framing.",
  "- Separate collection surface effects from wording effects, especially for tag-heavy groups like retirement, ETF, and index funds.",
  "- Consider time-aware validation once there are observations across more dates.",
  "- Review false positives and false negatives before adding more model complexity.",
  "- Keep future LLM scoring as an additional feature to benchmark against these title-only baselines, not as a replacement for them."
)

writeLines(summary_lines, file.path(output_dir, "followup_summary.txt"))

message("Saved follow-up outputs to: ", output_dir)
message("Done.")
