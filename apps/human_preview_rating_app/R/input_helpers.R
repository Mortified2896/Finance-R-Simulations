article_lab_input_string <- function(x) {
  value <- clean_text(x)
  if (length(value) == 0 || is.na(value[[1]])) NULL else value[[1]]
}

article_lab_input_multiline <- function(x) {
  value <- clean_multiline_text(x)
  if (length(value) == 0 || is.na(value[[1]])) NULL else value[[1]]
}

research_input_value <- function(value) {
  cleaned <- clean_text(value)
  if (length(cleaned) == 0 || is.na(cleaned[[1]])) NA_character_ else cleaned[[1]]
}

research_multiline_value <- function(value) {
  cleaned <- clean_multiline_text(value)
  if (length(cleaned) == 0 || is.na(cleaned[[1]])) NA_character_ else cleaned[[1]]
}

research_input_default <- function(value, default) {
  cleaned <- research_input_value(value)
  if (is.na(cleaned)) default else cleaned
}

research_input_integer <- function(value) {
  cleaned <- research_input_value(value)
  number <- suppressWarnings(as.integer(cleaned))
  if (is.na(number)) NA_integer_ else number
}

research_numeric_default <- function(value) {
  number <- suppressWarnings(as.integer(value))
  if (length(number) == 0 || is.na(number[[1]])) NULL else number[[1]]
}
