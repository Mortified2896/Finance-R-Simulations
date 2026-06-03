clean_text <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("\\s+", " ", y)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

clean_multiline_text <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- NA_character_
  y <- gsub("\r\n?", "\n", y)
  y <- gsub("\u00a0", " ", y, fixed = TRUE)
  y <- gsub("[ \t]+$", "", y, perl = TRUE)
  y <- trimws(y)
  y[y == ""] <- NA_character_
  y
}

now_utc <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

first_value <- function(row, column, default = NA_character_) {
  if (is.null(row) || nrow(row) == 0 || !(column %in% names(row))) return(default)
  value <- row[[column]][[1]]
  if (length(value) == 0) default else value
}

article_lab_row_value <- function(row, column, default = NA_character_) {
  value <- first_value(row, column, default)
  value %||% default
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

article_lab_format_duration <- function(seconds) {
  seconds <- suppressWarnings(as.numeric(seconds))
  if (is.na(seconds) || seconds < 0) seconds <- 0
  seconds <- as.integer(round(seconds))
  minutes <- seconds %/% 60L
  remaining_seconds <- seconds %% 60L
  if (minutes <= 0L) return(sprintf("%s sec", remaining_seconds))
  if (remaining_seconds == 0L) return(sprintf("%s min", minutes))
  sprintf("%s min %s sec", minutes, remaining_seconds)
}

article_lab_thumbnail_estimate <- function(total_expected) {
  total_expected <- suppressWarnings(as.integer(total_expected))
  if (is.na(total_expected) || total_expected < 1L) total_expected <- 1L
  list(
    total_expected = total_expected,
    lower_seconds = total_expected * 45,
    upper_seconds = total_expected * 90,
    label = sprintf(
      "%s-%s",
      article_lab_format_duration(total_expected * 45),
      article_lab_format_duration(total_expected * 90)
    )
  )
}

article_lab_estimate_comparison <- function(actual_seconds, lower_seconds, upper_seconds) {
  actual_seconds <- suppressWarnings(as.numeric(actual_seconds))
  lower_seconds <- suppressWarnings(as.numeric(lower_seconds))
  upper_seconds <- suppressWarnings(as.numeric(upper_seconds))
  if (is.na(actual_seconds) || is.na(lower_seconds) || is.na(upper_seconds)) return("within estimate")
  if (actual_seconds < lower_seconds) "faster than expected" else if (actual_seconds > upper_seconds) "slower than expected" else "within estimate"
}
