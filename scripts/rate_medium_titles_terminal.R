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

parse_args <- function(args) {
  out <- list(
    db = file.path("data", "db", "medium_articles.sqlite"),
    rater = Sys.getenv("USER", unset = "default"),
    limit = 100L,
    rating_version = "v2_general",
    general_rating = NA_integer_,
    note = NA_character_
  )
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--db", "--rater", "--limit", "--rating-version")) {
      i <- i + 1
      if (i > length(args)) stop(arg, " requires a value", call. = FALSE)
      key <- gsub("-", "_", sub("^--", "", arg))
      out[[key]] <- args[[i]]
    } else if (startsWith(arg, "--db=")) {
      out$db <- sub("^--db=", "", arg)
    } else if (startsWith(arg, "--rater=")) {
      out$rater <- sub("^--rater=", "", arg)
    } else if (startsWith(arg, "--limit=")) {
      out$limit <- sub("^--limit=", "", arg)
    } else if (startsWith(arg, "--rating-version=")) {
      out$rating_version <- sub("^--rating-version=", "", arg)
    } else if (arg %in% c("--general-rating", "--note")) {
      i <- i + 1
      if (i > length(args)) stop(arg, " requires a value", call. = FALSE)
      key <- gsub("-", "_", sub("^--", "", arg))
      out[[key]] <- args[[i]]
    } else if (startsWith(arg, "--general-rating=")) {
      out$general_rating <- sub("^--general-rating=", "", arg)
    } else if (startsWith(arg, "--note=")) {
      out$note <- sub("^--note=", "", arg)
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1
  }
  out$limit <- as.integer(out$limit)
  if (is.na(out$limit) || out$limit < 0) stop("--limit must be a non-negative integer", call. = FALSE)
  if (!is.na(out$general_rating)) {
    out$general_rating <- as.integer(out$general_rating)
    if (is.na(out$general_rating) || out$general_rating < 1L || out$general_rating > 5L) {
      stop("--general-rating must be an integer from 1 to 5", call. = FALSE)
    }
  }
  out
}

clean_text <- function(x) {
  if (length(x) == 0 || is.na(x) || is.null(x)) return("")
  trimws(gsub("\\s+", " ", as.character(x)))
}

simple_text_hash <- function(x) {
  x <- clean_text(x)
  ints <- utf8ToInt(x)
  if (length(ints) == 0) return("0")
  hash <- sum(as.numeric(ints) * seq_along(ints))
  sprintf("%.0f", hash %% 4294967291)
}

read_prompt_line <- function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  }
  cat(prompt)
  flush.console()
  input <- tryCatch(suppressWarnings(file("/dev/tty", open = "r")), error = function(e) NULL)
  close_input <- !is.null(input)
  if (is.null(input)) {
    input <- stdin()
  }
  on.exit(if (close_input) close(input), add = TRUE)
  answer <- readLines(input, n = 1, warn = FALSE)
  if (length(answer) == 0) {
    stop(
      "\nNo input was received from Terminal. ",
      "Run this script from an interactive Terminal, or pass --limit 0 for smoke tests.",
      call. = FALSE
    )
  }
  answer[[1]]
}

clear_terminal <- function() {
  cat("\033[2J\033[H")
  flush.console()
}

ask_general_rating <- function(prompt, min_value = 1L, max_value = 5L) {
  blank_answers <- 0L
  repeat {
    answer <- tolower(trimws(read_prompt_line(prompt)))
    if (!nzchar(answer)) {
      blank_answers <- blank_answers + 1L
      if (blank_answers >= 5L) {
        stop(
          "\nNo rating input was received. ",
          "Restart the rating workflow and enter 1-5, s, q, or b.",
          call. = FALSE
        )
      }
      next
    }
    blank_answers <- 0L
    if (answer %in% c("s", "q", "b")) return(answer)
    value <- suppressWarnings(as.integer(answer))
    if (!is.na(value) && value >= min_value && value <= max_value) return(value)
    cat("Enter ", min_value, "-", max_value, ", or s/q/b.\n", sep = "")
  }
}

count_unrated_with_thumbnail <- function(connection, args) {
  dbGetQuery(
    connection,
    "
      SELECT COUNT(*) AS n
      FROM v_medium_title_prediction_dataset_v2 d
      WHERE NULLIF(TRIM(d.title), '') IS NOT NULL
        AND (COALESCE(d.has_thumbnail_url, 0) = 1 OR NULLIF(TRIM(d.thumbnail_url), '') IS NOT NULL)
        AND NOT EXISTS (
          SELECT 1
          FROM medium_title_human_ratings r
          WHERE r.canonical_article_key = d.canonical_article_key
            AND r.rater = ?
            AND r.rating_version = ?
        )
    ",
    params = list(args$rater, args$rating_version)
  )$n[[1]]
}

count_rating_totals <- function(connection, args) {
  dbGetQuery(
    connection,
    "
      SELECT
        SUM(CASE WHEN skipped = 0 AND general_rating IS NOT NULL THEN 1 ELSE 0 END) AS rated,
        SUM(CASE WHEN skipped = 1 THEN 1 ELSE 0 END) AS skipped
      FROM medium_title_human_ratings
      WHERE rater = ?
        AND rating_version = ?
    ",
    params = list(args$rater, args$rating_version)
  )
}

insert_rating <- function(connection, row, args, general_rating, note, skipped) {
  rated_at <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  dbExecute(
    connection,
    "
      INSERT INTO medium_title_human_ratings (
        canonical_article_key,
        article_id,
        medium_post_id,
        rater,
        rating_version,
        rated_at,
        shown_title,
        shown_subtitle,
        title_hash,
        subtitle_hash,
        general_rating,
        click_rating,
        quality_rating,
        would_study,
        note,
        skipped
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      row$canonical_article_key,
      row$article_id,
      row$medium_post_id,
      args$rater,
      args$rating_version,
      rated_at,
      row$title,
      row$subtitle,
      simple_text_hash(row$title),
      simple_text_hash(row$subtitle),
      general_rating,
      NA_integer_,
      NA_integer_,
      NA_integer_,
      note,
      skipped
    )
  )
  dbGetQuery(connection, "SELECT last_insert_rowid() AS id")$id[[1]]
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

cat("Medium Title Human Rating\n")
cat("=========================\n")
cat("DB path: ", args$db, "\n", sep = "")
cat("Rater: ", args$rater, "\n", sep = "")
cat("Rating version: ", args$rating_version, "\n\n", sep = "")

stdin_is_tty <- tryCatch(isatty(stdin()), error = function(e) FALSE)
dev_tty_available <- tryCatch({
  input <- suppressWarnings(file("/dev/tty", open = "r"))
  close(input)
  TRUE
}, error = function(e) FALSE)
if (is.na(args$general_rating) && !(interactive() || stdin_is_tty || dev_tty_available) && args$limit > 0) {
  stop(
    "This rating workflow needs an interactive Terminal. ",
    "Use --limit 0 for smoke tests, or run it from a Terminal/.command launcher.",
    call. = FALSE
  )
}

if (!file.exists(args$db)) {
  stop("Could not find database at: ", args$db, call. = FALSE)
}

connection <- dbConnect(SQLite(), args$db)
on.exit(dbDisconnect(connection), add = TRUE)

required_objects <- c("v_medium_title_prediction_dataset_v2", "medium_title_human_ratings")
missing_objects <- required_objects[!vapply(required_objects, dbExistsTable, logical(1), conn = connection)]
if (length(missing_objects) > 0) {
  stop(
    "Missing required object(s): ",
    paste(missing_objects, collapse = ", "),
    ". Run scripts/apply_medium_analysis_v2_schema.R first.",
    call. = FALSE
  )
}

if (args$limit == 0) {
  cat("Limit is 0. Nothing to rate.\n")
  quit(status = 0)
}

unrated_thumbnail_count <- count_unrated_with_thumbnail(connection, args)
rating_totals <- count_rating_totals(connection, args)
already_rated_total <- if (is.na(rating_totals$rated[[1]])) 0L else as.integer(rating_totals$rated[[1]])
already_skipped_total <- if (is.na(rating_totals$skipped[[1]])) 0L else as.integer(rating_totals$skipped[[1]])

rows <- dbGetQuery(
  connection,
  "
    SELECT
      d.canonical_article_key,
      d.article_id,
      d.medium_post_id,
      d.title,
      d.subtitle,
      CASE
        WHEN COALESCE(d.has_thumbnail_url, 0) = 1 OR NULLIF(TRIM(d.thumbnail_url), '') IS NOT NULL THEN 1
        ELSE 0
      END AS has_thumbnail
    FROM v_medium_title_prediction_dataset_v2 d
    WHERE NULLIF(TRIM(d.title), '') IS NOT NULL
      AND NOT EXISTS (
        SELECT 1
        FROM medium_title_human_ratings r
        WHERE r.canonical_article_key = d.canonical_article_key
          AND r.rater = ?
          AND r.rating_version = ?
      )
    ORDER BY has_thumbnail DESC, RANDOM()
    LIMIT ?
  ",
  params = list(args$rater, args$rating_version, args$limit)
)

if (nrow(rows) == 0) {
  cat("No unrated articles found for this rater/version.\n")
  quit(status = 0)
}

cat("Controls: s = skip, q = save and quit, b = undo previous rating from this session.\n")
cat("Only title and subtitle are shown.\n\n")
cat("Unrated articles with thumbnails: ", unrated_thumbnail_count, "\n", sep = "")
cat("Already rated by you for this version: ", already_rated_total, "\n", sep = "")
if (already_skipped_total > 0L) {
  cat("Already skipped by you for this version: ", already_skipped_total, "\n", sep = "")
}

rated_count <- 0L
skipped_count <- 0L
last_insert <- NULL
last_row <- NULL
i <- 1L
while (i <= nrow(rows)) {
  clear_terminal()
  rating_totals <- count_rating_totals(connection, args)
  already_rated_total <- if (is.na(rating_totals$rated[[1]])) 0L else as.integer(rating_totals$rated[[1]])
  already_skipped_total <- if (is.na(rating_totals$skipped[[1]])) 0L else as.integer(rating_totals$skipped[[1]])
  unrated_thumbnail_count <- count_unrated_with_thumbnail(connection, args)
  row <- rows[i, ]
  cat("Medium Title Human Rating\n")
  cat("=========================\n")
  cat("Unrated articles with thumbnails: ", unrated_thumbnail_count, "\n", sep = "")
  cat("Already rated by you for this version: ", already_rated_total, "\n", sep = "")
  if (already_skipped_total > 0L) {
    cat("Already skipped by you for this version: ", already_skipped_total, "\n", sep = "")
  }
  cat("Controls: 1-5 = rate, s = skip, q = quit, b = undo previous rating from this session.\n")
  cat("\n----------------------------------------\n")
  cat("[", i, "/", nrow(rows), "]\n", sep = "")
  cat("Thumbnail: ", if (isTRUE(as.integer(row$has_thumbnail) == 1L)) "yes" else "no", "\n", sep = "")
  cat("Title: ", clean_text(row$title), "\n", sep = "")
  subtitle <- clean_text(row$subtitle)
  cat("Subtitle: ", if (nzchar(subtitle)) subtitle else "(none)", "\n\n", sep = "")

  general_rating <- if (!is.na(args$general_rating)) {
    args$general_rating
  } else {
    ask_general_rating("General rating 1-5: ")
  }
  if (general_rating == "q") break
  if (general_rating == "b") {
    clear_terminal()
    if (!is.null(last_insert)) {
      dbExecute(connection, "DELETE FROM medium_title_human_ratings WHERE id = ?", params = list(last_insert))
      cat("Undid previous rating from this session.\n")
      last_insert <- NULL
      if (!is.null(last_row)) {
        i <- max(1L, i - 1L)
      }
    } else {
      cat("No previous session rating to undo.\n")
    }
    next
  }
  if (general_rating == "s") {
    clear_terminal()
    last_insert <- insert_rating(connection, row, args, NA_integer_, "", 1L)
    last_row <- row
    skipped_count <- skipped_count + 1L
    i <- i + 1L
    next
  }
  clear_terminal()
  note <- if (!is.na(args$note)) args$note else ""
  last_insert <- insert_rating(connection, row, args, general_rating, note, 0L)
  last_row <- row
  rated_count <- rated_count + 1L
  i <- i + 1L
}

clear_terminal()
cat("\nSaved ratings: ", rated_count, "\n", sep = "")
cat("Saved skips: ", skipped_count, "\n", sep = "")
rating_totals <- count_rating_totals(connection, args)
already_rated_total <- if (is.na(rating_totals$rated[[1]])) 0L else as.integer(rating_totals$rated[[1]])
already_skipped_total <- if (is.na(rating_totals$skipped[[1]])) 0L else as.integer(rating_totals$skipped[[1]])
cat("Total rated by you for this version: ", already_rated_total, "\n", sep = "")
if (already_skipped_total > 0L) {
  cat("Total skipped by you for this version: ", already_skipped_total, "\n", sep = "")
}
cat("Unrated articles with thumbnails left: ", count_unrated_with_thumbnail(connection, args), "\n", sep = "")
cat("Done.\n")
quit(status = 0)
