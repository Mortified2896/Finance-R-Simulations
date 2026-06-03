article_lab_subtitle_max_chars <- 90L

article_lab_title_length <- function(x) {
  nchar(enc2utf8(as.character(x)), type = "chars", allowNA = TRUE, keepNA = TRUE)
}

article_lab_validate_titles <- function(titles, max_chars = article_lab_title_max_chars) {
  title_values <- clean_text(titles)
  lengths <- article_lab_title_length(title_values)
  valid <- !is.na(title_values) & !is.na(lengths) & lengths <= max_chars
  valid[is.na(valid)] <- FALSE
  list(
    titles = title_values[valid],
    dropped_titles = title_values[!valid & !is.na(title_values)],
    kept_n = sum(valid, na.rm = TRUE),
    dropped_n = sum(!valid & !is.na(title_values), na.rm = TRUE)
  )
}

article_lab_parse_manual_titles <- function(value) {
  text_value <- as.character(value %||% "")
  if (length(text_value) == 0 || is.na(text_value[[1]]) || !nzchar(text_value[[1]])) return(character())
  pieces <- unlist(strsplit(text_value[[1]], "\n", fixed = TRUE), use.names = FALSE)
  pieces <- clean_text(pieces)
  pieces <- pieces[!is.na(pieces)]
  unique(pieces[nzchar(pieces)])
}

article_lab_normalize_titles <- function(titles) {
  title_values <- clean_text(titles)
  unique(title_values[!is.na(title_values)])
}

article_lab_normalize_subtitle <- function(values, max_chars = article_lab_subtitle_max_chars) {
  unique_values <- unique(clean_text(values))
  unique_values <- unique_values[!is.na(unique_values)]
  char_counts <- nchar(enc2utf8(unique_values), type = "chars", allowNA = TRUE, keepNA = TRUE)
  unique_values[!is.na(char_counts) & char_counts <= max_chars]
}
