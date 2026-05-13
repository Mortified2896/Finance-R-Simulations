input_path <- file.path("data", "analysis", "medium_title_prediction_dataset.csv")
output_dir <- file.path("data", "analysis", "subtitle_analysis")

message("Medium Subtitle/Deck Quality Audit")
message("==================================")

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

normalize_text <- function(x) {
  value <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = " ")
  value <- tolower(value)
  value <- gsub("[^a-z0-9]+", " ", value)
  value <- gsub("\\s+", " ", value)
  trimws(value)
}

split_words <- function(text) {
  normalized <- normalize_text(text)
  if (is.na(normalized) || normalized == "") {
    return(character())
  }
  words <- unlist(strsplit(normalized, " ", fixed = TRUE), use.names = FALSE)
  words[words != ""]
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

finance_article_words <- c(
  "ai", "article", "bank", "banks", "bitcoin", "bond", "bonds", "business", "capital",
  "cash", "credit", "crypto", "debt", "dividend", "economy", "etf", "etfs", "finance",
  "financial", "fund", "funds", "income", "index", "invest", "investing", "investment",
  "investor", "investors", "market", "markets", "money", "portfolio", "retire",
  "retired", "retirement", "risk", "saving", "savings", "stock", "stocks", "tax",
  "trading", "wealth"
)

category_like_words <- c(
  finance_article_words,
  "case", "study", "system", "systems", "truth", "experience", "needed", "fallout",
  "option", "options", "introduction", "trading"
)

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

evaluate_model <- function(model_name, scenario, x, train_index, test_index, y_reg, y_cls) {
  x_train <- x[train_index, , drop = FALSE]
  x_test <- x[test_index, , drop = FALSE]
  lm_model <- fit_lm_safely(x_train, y_reg[train_index])
  glm_model <- fit_glm_safely(x_train, y_cls[train_index])
  pred_reg_test <- predict_safely(lm_model, x_test, type = "response")
  pred_cls_test <- predict_safely(glm_model, x_test, type = "response")
  cls <- classification_metrics(y_cls[test_index], pred_cls_test)
  data.frame(
    scenario = scenario,
    model = model_name,
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

write_empty_csv <- function(path, columns) {
  write.csv(as.data.frame(setNames(replicate(length(columns), logical(0), simplify = FALSE), columns)), path, row.names = FALSE)
}

select_subtitle_text <- function(data) {
  candidates <- intersect(c("subtitle", "deck", "description", "snippet"), names(data))
  if (length(candidates) == 0) {
    return(list(text = rep(NA_character_, nrow(data)), source = NA_character_, candidates = character()))
  }
  counts <- vapply(candidates, function(col) sum(!is.na(clean_text_vector(data[[col]]))), integer(1))
  source <- names(sort(counts, decreasing = TRUE))[1]
  text <- clean_text_vector(data[[source]])
  for (col in candidates) {
    replacement <- clean_text_vector(data[[col]])
    text[is.na(text) & !is.na(replacement)] <- replacement[is.na(text) & !is.na(replacement)]
  }
  list(text = text, source = source, candidates = candidates)
}

looks_like_author_name <- function(text) {
  if (is.na(text) || text == "") {
    return(FALSE)
  }
  value <- trimws(text)
  if (grepl("^by\\s+[A-Z]", value, ignore.case = FALSE)) {
    return(TRUE)
  }
  if (grepl("[[:punct:][:digit:]]", value)) {
    return(FALSE)
  }
  words <- unlist(strsplit(value, "\\s+"), use.names = FALSE)
  if (length(words) < 2 || length(words) > 4) {
    return(FALSE)
  }
  capitalized <- grepl("^[A-Z][A-Za-z'’-]*$", words)
  normalized_words <- split_words(value)
  no_category_words <- !any(normalized_words %in% category_like_words)
  all(capitalized) && no_category_words
}

near_duplicate <- function(a, b) {
  na <- normalize_text(a)
  nb <- normalize_text(b)
  if (is.na(na) || is.na(nb) || na == "" || nb == "") {
    return(FALSE)
  }
  if (identical(na, nb)) {
    return(TRUE)
  }
  distance <- utils::adist(na, nb)[1]
  max_len <- max(nchar(na), nchar(nb))
  max_len > 0 && (distance / max_len) <= 0.10
}

required_columns <- c("title", "latest_claps", "latest_responses")
missing_columns <- setdiff(required_columns, names(articles))
if (length(missing_columns) > 0) {
  stop("Dataset is missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

articles$title <- clean_text_vector(articles$title)
articles$latest_claps <- suppressWarnings(as.numeric(articles$latest_claps))
articles$latest_responses <- suppressWarnings(as.numeric(articles$latest_responses))
articles$success_score <- ifelse(
  !is.na(articles$latest_claps) | !is.na(articles$latest_responses),
  log1p(ifelse(is.na(articles$latest_claps), 0, articles$latest_claps)) +
    2 * log1p(ifelse(is.na(articles$latest_responses), 0, articles$latest_responses)),
  NA_real_
)
valid_success <- !is.na(articles$success_score)
cutoff20 <- as.numeric(stats::quantile(articles$success_score[valid_success], probs = 0.80, na.rm = TRUE, type = 7))
articles$high_performer_top20 <- NA
articles$high_performer_top20[valid_success] <- articles$success_score[valid_success] >= cutoff20

subtitle_selection <- select_subtitle_text(articles)
articles$subtitle_text_for_analysis <- subtitle_selection$text
subtitle_length <- nchar(ifelse(is.na(articles$subtitle_text_for_analysis), "", articles$subtitle_text_for_analysis))
subtitle_words <- lapply(articles$subtitle_text_for_analysis, split_words)

subtitle_missing <- is.na(articles$subtitle_text_for_analysis)
subtitle_too_short <- !subtitle_missing & subtitle_length < 20
subtitle_very_short <- !subtitle_missing & subtitle_length < 10
subtitle_looks_like_author_name <- vapply(articles$subtitle_text_for_analysis, looks_like_author_name, logical(1))
subtitle_truncated <- !subtitle_missing & grepl("…|\\.\\.\\.$", articles$subtitle_text_for_analysis)
subtitle_duplicate_title <- vapply(seq_len(nrow(articles)), function(i) near_duplicate(articles$title[i], articles$subtitle_text_for_analysis[i]), logical(1))
subtitle_contains_url <- !subtitle_missing & grepl("https?://|www\\.", articles$subtitle_text_for_analysis, ignore.case = TRUE)
subtitle_url_only <- subtitle_contains_url & lengths(subtitle_words) <= 3
subtitle_low_information <- subtitle_missing |
  subtitle_very_short |
  subtitle_looks_like_author_name |
  subtitle_duplicate_title |
  subtitle_url_only

audit <- data.frame(
  article_id = if ("article_id" %in% names(articles)) articles$article_id else seq_len(nrow(articles)),
  medium_post_id = if ("medium_post_id" %in% names(articles)) articles$medium_post_id else NA_character_,
  url = if ("url" %in% names(articles)) articles$url else NA_character_,
  title = articles$title,
  subtitle_source_column = subtitle_selection$source,
  subtitle_text_for_analysis = articles$subtitle_text_for_analysis,
  subtitle_length = subtitle_length,
  subtitle_missing = subtitle_missing,
  subtitle_too_short = subtitle_too_short,
  subtitle_very_short = subtitle_very_short,
  subtitle_looks_like_author_name = subtitle_looks_like_author_name,
  subtitle_truncated = subtitle_truncated,
  subtitle_duplicate_title = subtitle_duplicate_title,
  subtitle_contains_url = subtitle_contains_url,
  subtitle_low_information = subtitle_low_information,
  success_score = articles$success_score,
  high_performer_top20 = articles$high_performer_top20,
  publication = if ("publication" %in% names(articles)) articles$publication else NA_character_,
  author = if ("author" %in% names(articles)) articles$author else NA_character_,
  stringsAsFactors = FALSE
)
write.csv(audit, file.path(output_dir, "subtitle_quality_audit.csv"), row.names = FALSE)

flag_names <- c(
  "subtitle_missing",
  "subtitle_too_short",
  "subtitle_very_short",
  "subtitle_looks_like_author_name",
  "subtitle_truncated",
  "subtitle_duplicate_title",
  "subtitle_contains_url",
  "subtitle_low_information"
)

flag_summary <- do.call(rbind, lapply(flag_names, function(flag_name) {
  flagged <- audit[[flag_name]]
  data.frame(
    flag_name = flag_name,
    n_rows_flagged = sum(flagged, na.rm = TRUE),
    percent_rows_flagged = mean(flagged, na.rm = TRUE),
    mean_success_score_flagged = mean(audit$success_score[flagged], na.rm = TRUE),
    mean_success_score_unflagged = mean(audit$success_score[!flagged], na.rm = TRUE),
    high_performer_top20_rate_flagged = mean(audit$high_performer_top20[flagged], na.rm = TRUE),
    high_performer_top20_rate_unflagged = mean(audit$high_performer_top20[!flagged], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
write.csv(flag_summary, file.path(output_dir, "subtitle_quality_flag_summary.csv"), row.names = FALSE)

examples_for_flag <- function(flag_name, n = 8) {
  rows <- audit[audit[[flag_name]], c("article_id", "title", "subtitle_text_for_analysis", "publication", "author", "success_score", "high_performer_top20"), drop = FALSE]
  if (nrow(rows) == 0) {
    return(paste0(flag_name, ": no examples"))
  }
  rows <- head(rows, n)
  c(
    paste0(flag_name, " examples:"),
    paste0(
      "- [", rows$article_id, "] ",
      substr(rows$subtitle_text_for_analysis, 1, 140),
      " | title: ",
      substr(rows$title, 1, 100)
    )
  )
}

run_model_comparison <- function(data, scenario) {
  model_data <- data[!is.na(data$title) & !is.na(data$subtitle_text_for_analysis) & !is.na(data$success_score), , drop = FALSE]
  if (nrow(model_data) < 100 || sum(model_data$high_performer_top20, na.rm = TRUE) < 20) {
    return(data.frame(
      scenario = scenario,
      model = "modeling_skipped",
      feature_count = NA_integer_,
      train_rows = nrow(model_data),
      test_rows = NA_integer_,
      regression_rmse = NA_real_,
      regression_mae = NA_real_,
      regression_r_squared = NA_real_,
      classification_accuracy = NA_real_,
      classification_precision_at_top20_predicted = NA_real_,
      classification_recall_at_top20_predicted = NA_real_,
      classification_auc = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  set.seed(20260513)
  train_index <- sort(sample(seq_len(nrow(model_data)), size = floor(0.80 * nrow(model_data))))
  test_index <- setdiff(seq_len(nrow(model_data)), train_index)

  title_terms <- term_sets_for_docs(model_data$title)
  subtitle_terms <- term_sets_for_docs(model_data$subtitle_text_for_analysis)
  combined_terms <- term_sets_for_docs(paste(model_data$title, model_data$subtitle_text_for_analysis))

  family_matrix <- function(term_sets, prefix) {
    out <- data.frame(row.names = seq_along(term_sets))
    for (family_name in names(term_families)) {
      out[[paste0(prefix, "_", family_name)]] <- as.integer(contains_family(term_sets, term_families[[family_name]]))
    }
    out
  }

  title_numeric <- make_basic_numeric_features(model_data$title, "title")
  subtitle_numeric <- make_basic_numeric_features(model_data$subtitle_text_for_analysis, "subtitle")
  combined_numeric <- data.frame(title_numeric, subtitle_numeric, check.names = FALSE)

  subtitle_train_terms <- unlist(subtitle_terms[train_index], use.names = FALSE)
  subtitle_counts <- sort(table(subtitle_train_terms), decreasing = TRUE)
  subtitle_vocab <- names(head(subtitle_counts[subtitle_counts >= 10 & names(subtitle_counts) != ""], 150))
  subtitle_text_features <- data.frame(make_term_matrix(subtitle_terms, subtitle_vocab, "subtitle_text"), check.names = FALSE)

  combined_train_terms <- unlist(combined_terms[train_index], use.names = FALSE)
  combined_counts <- sort(table(combined_train_terms), decreasing = TRUE)
  combined_vocab <- names(head(combined_counts[combined_counts >= 10 & names(combined_counts) != ""], 200))
  combined_text_features <- data.frame(make_term_matrix(combined_terms, combined_vocab, "combined_text"), check.names = FALSE)

  feature_sets <- list(
    title_only = data.frame(title_numeric, family_matrix(title_terms, "title"), check.names = FALSE),
    subtitle_only = data.frame(subtitle_numeric, subtitle_text_features, check.names = FALSE),
    title_plus_subtitle = data.frame(combined_numeric, combined_text_features, check.names = FALSE),
    title_plus_subtitle_numeric_plus_term_families = data.frame(combined_numeric, family_matrix(combined_terms, "combined"), check.names = FALSE)
  )

  do.call(rbind, lapply(names(feature_sets), function(model_name) {
    evaluate_model(
      model_name = model_name,
      scenario = scenario,
      x = feature_sets[[model_name]],
      train_index = train_index,
      test_index = test_index,
      y_reg = model_data$success_score,
      y_cls = model_data$high_performer_top20
    )
  }))
}

cleaned_rows <- !audit$subtitle_low_information
all_model_metrics <- run_model_comparison(audit, "all_rows")
cleaned_model_metrics <- run_model_comparison(audit[cleaned_rows, , drop = FALSE], "cleaned_excluding_low_information")
quality_model_metrics <- rbind(all_model_metrics, cleaned_model_metrics)

model_lines <- c(
  "Subtitle quality-filtered model comparison",
  "==========================================",
  "Models are pre-publication / drafting-time only: title and subtitle/deck text features.",
  "Cleaned rows exclude subtitle_low_information: missing, very short, author-like, duplicate-title, or URL-only subtitles.",
  "",
  capture.output(print(quality_model_metrics, row.names = FALSE))
)
writeLines(model_lines, file.path(output_dir, "model_comparison_subtitle_quality_filtered.txt"))

total_rows <- nrow(audit)
usable_subtitle_rows <- sum(!audit$subtitle_missing)
low_info_rows <- sum(audit$subtitle_low_information)
low_info_rate <- low_info_rows / total_rows

metric_lookup <- function(metrics, scenario, model, column) {
  row <- metrics[metrics$scenario == scenario & metrics$model == model, , drop = FALSE]
  if (nrow(row) == 0) {
    return(NA_real_)
  }
  row[[column]][1]
}

all_title_sub_auc <- metric_lookup(quality_model_metrics, "all_rows", "title_plus_subtitle_numeric_plus_term_families", "classification_auc")
clean_title_sub_auc <- metric_lookup(quality_model_metrics, "cleaned_excluding_low_information", "title_plus_subtitle_numeric_plus_term_families", "classification_auc")
all_title_auc <- metric_lookup(quality_model_metrics, "all_rows", "title_only", "classification_auc")
clean_title_auc <- metric_lookup(quality_model_metrics, "cleaned_excluding_low_information", "title_only", "classification_auc")

recommendation <- if (low_info_rate < 0.05) {
  "Subtitle quality looks good enough for exploratory modeling, with a small low-information subset worth excluding in sensitivity checks."
} else if (low_info_rate < 0.15) {
  "Subtitle quality is usable but mixed; use cleaned-row sensitivity checks before trusting subtitle effects."
} else {
  "Subtitle quality needs improvement before subtitle-heavy modeling should be trusted."
}

summary_lines <- c(
  "Subtitle/deck quality summary",
  "=============================",
  "",
  paste("Total rows:", total_rows),
  paste("Selected subtitle source column:", ifelse(is.na(subtitle_selection$source), "(none)", subtitle_selection$source)),
  paste("Candidate subtitle-like columns:", ifelse(length(subtitle_selection$candidates) == 0, "(none)", paste(subtitle_selection$candidates, collapse = ", "))),
  paste("Usable subtitle rows:", usable_subtitle_rows),
  "",
  "Quality flag counts",
  "-------------------",
  paste0(flag_summary$flag_name, ": ", flag_summary$n_rows_flagged, " (", round(100 * flag_summary$percent_rows_flagged, 1), "%)"),
  "",
  "Examples by suspicious category",
  "-------------------------------",
  unlist(lapply(flag_names, examples_for_flag), use.names = FALSE),
  "",
  "Recommendation",
  "--------------",
  recommendation,
  "",
  "Cleaned model comparison",
  "------------------------",
  paste("Low-information subtitle rows excluded:", low_info_rows, "of", total_rows, paste0("(", round(100 * low_info_rate, 1), "%)")),
  paste("All rows title-only AUC:", round(all_title_auc, 3)),
  paste("All rows title+subtitle term-family AUC:", round(all_title_sub_auc, 3)),
  paste("Cleaned rows title-only AUC:", round(clean_title_auc, 3)),
  paste("Cleaned rows title+subtitle term-family AUC:", round(clean_title_sub_auc, 3)),
  "",
  "Answers",
  "-------",
  if (low_info_rate < 0.05) {
    "Suspicious subtitles are rare."
  } else {
    "Suspicious subtitles are common enough to keep a cleaned-row comparison in the workflow."
  },
  if (!is.na(clean_title_sub_auc) && !is.na(all_title_sub_auc) && abs(clean_title_sub_auc - all_title_sub_auc) <= 0.03) {
    "Excluding suspicious subtitles does not materially change the title+subtitle signal by AUC."
  } else {
    "Excluding suspicious subtitles changes the title+subtitle signal enough to inspect before relying on it."
  },
  "Title+subtitle findings are usable as an exploratory baseline before OpenAI/ChatGPT API scoring, but should be interpreted with the quality flags attached.",
  if (low_info_rate >= 0.05) {
    "Improve subtitle extraction/import quality before doing more subtitle-heavy analysis."
  } else {
    "Subtitle extraction/import quality is adequate for now, but author-like and very short decks should stay flagged."
  }
)

writeLines(summary_lines, file.path(output_dir, "subtitle_quality_summary.txt"))

message("Saved subtitle quality audit outputs to: ", output_dir)
message("Done.")
