required_packages <- c("DBI", "RSQLite", "jsonlite")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("DBI", "RSQLite", "jsonlite"))',
    call. = FALSE
  )
}

library(DBI)
library(RSQLite)
library(jsonlite)

source(file.path("scripts", "medium_tag_import_helpers.R"))

database_path <- Sys.getenv("MEDIUM_DB_PATH", file.path("data", "medium_articles.sqlite"))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop(
    "Please provide exactly one Medium article text snapshot JSON file path.\n\n",
    "Example:\n",
    "Rscript scripts/import_medium_article_text_snapshot.R data/medium_article_text_snapshots/example.json",
    call. = FALSE
  )
}

input_path <- args[1]

if (!file.exists(input_path)) {
  stop("The input file does not exist:\n\n", input_path, call. = FALSE)
}

payload <- fromJSON(input_path, simplifyVector = FALSE)

if (!identical(clean_text(payload$source_type), "medium_article_text_snapshot")) {
  stop("The JSON file does not contain source_type = 'medium_article_text_snapshot'.", call. = FALSE)
}

normalized_url <- normalize_medium_url(scalar_from_json(payload$article_url))
collected_at <- clean_text(payload$collected_at)
text_hash <- clean_text(payload$text_hash)
visible_text <- clean_text(payload$visible_text)
article_tags_json <- if (is.null(payload$article_tags) || length(payload$article_tags) == 0) {
  NA_character_
} else {
  clean_text(jsonlite::toJSON(payload$article_tags, auto_unbox = TRUE, null = "null"))
}
text_blocks_json <- if (is.null(payload$text_blocks) || length(payload$text_blocks) == 0) {
  NA_character_
} else {
  clean_text(jsonlite::toJSON(payload$text_blocks, auto_unbox = TRUE, null = "null"))
}
images_json <- if (is.null(payload$images) || length(payload$images) == 0) {
  NA_character_
} else {
  clean_text(jsonlite::toJSON(payload$images, auto_unbox = TRUE, null = "null"))
}

if (is.na(normalized_url)) {
  stop("Article text snapshot is missing article_url.", call. = FALSE)
}

if (is.na(collected_at)) {
  collected_at <- current_timestamp()
}

if (is.na(text_hash)) {
  stop("Article text snapshot is missing text_hash.", call. = FALSE)
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

ensure_medium_articles_schema(connection)
schema_info <- inspect_medium_articles_schema(connection)
ensure_medium_article_text_snapshots_schema(connection)

article_row <- find_article_by_url(connection, normalized_url, schema_info)

if (nrow(article_row) == 0) {
  dbExecute(
    connection,
    "
      INSERT INTO medium_articles (
        source_tag,
        title,
        url,
        raw_url,
        canonical_url,
        fetched_at,
        medium_post_id,
        author,
        publication,
        published_at,
        read_time,
        categories,
        visible_article_text,
        visible_text_word_count,
        visible_article_text_truncated,
        visible_article_text_max_chars,
        visible_article_collected_at,
        last_seen_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      "medium-article-text",
      if (!is_missing_text(payload$title)) clean_text(payload$title) else "(untitled Medium article)",
      normalized_url,
      normalized_url,
      normalized_url,
      collected_at,
      clean_text(payload$medium_post_id),
      clean_text(payload$author_name),
      clean_text(payload$publication_name),
      clean_text(payload$published_label),
      clean_text(payload$read_time),
      article_tags_json,
      visible_text,
      integer_from_json(payload$word_count),
      logical_flag_from_json(payload$text_truncated),
      integer_from_json(payload$max_chars),
      collected_at,
      collected_at
    )
  )
  article_id <- dbGetQuery(connection, "SELECT last_insert_rowid() AS id")$id[1]
  article_status <- "created"
} else {
  article_id <- article_row$id[1]
  dbExecute(
    connection,
    "
      UPDATE medium_articles
      SET
        title = COALESCE(NULLIF(title, ''), ?),
        medium_post_id = COALESCE(NULLIF(medium_post_id, ''), ?),
        author = COALESCE(NULLIF(author, ''), ?),
        publication = COALESCE(NULLIF(publication, ''), ?),
        published_at = COALESCE(NULLIF(published_at, ''), ?),
        read_time = COALESCE(NULLIF(read_time, ''), ?),
        categories = COALESCE(NULLIF(categories, ''), ?),
        visible_article_text = CASE
          WHEN ? IS NOT NULL
            AND (
              visible_article_text IS NULL
              OR visible_article_text = ''
              OR length(?) > length(visible_article_text)
            )
          THEN ?
          ELSE visible_article_text
        END,
        visible_text_word_count = CASE
          WHEN ? IS NOT NULL
            AND (
              visible_text_word_count IS NULL
              OR ? > visible_text_word_count
            )
          THEN ?
          ELSE visible_text_word_count
        END,
        visible_article_text_truncated = COALESCE(visible_article_text_truncated, ?),
        visible_article_text_max_chars = COALESCE(visible_article_text_max_chars, ?),
        visible_article_collected_at = CASE
          WHEN ? IS NOT NULL
            AND (
              visible_article_text IS NULL
              OR visible_article_text = ''
              OR length(?) > length(visible_article_text)
            )
          THEN ?
          ELSE visible_article_collected_at
        END,
        last_seen_at = CASE
          WHEN ? IS NOT NULL
            AND (
              last_seen_at IS NULL
              OR last_seen_at = ''
              OR ? > last_seen_at
            )
          THEN ?
          ELSE last_seen_at
        END
      WHERE id = ?
    ",
    params = list(
      clean_text(payload$title),
      clean_text(payload$medium_post_id),
      clean_text(payload$author_name),
      clean_text(payload$publication_name),
      clean_text(payload$published_label),
      clean_text(payload$read_time),
      article_tags_json,
      visible_text,
      visible_text,
      visible_text,
      integer_from_json(payload$word_count),
      integer_from_json(payload$word_count),
      integer_from_json(payload$word_count),
      logical_flag_from_json(payload$text_truncated),
      integer_from_json(payload$max_chars),
      collected_at,
      visible_text,
      collected_at,
      collected_at,
      collected_at,
      collected_at,
      article_id
    )
  )
  article_status <- "reused/updated"
}

rows_inserted <- dbExecute(
  connection,
  "
    INSERT OR IGNORE INTO medium_article_text_snapshots (
      article_id,
      article_url_normalized,
      collected_at,
      source_type,
      extraction_method,
      text_hash,
      visible_text,
      word_count,
      text_truncated,
      max_chars,
      text_blocks_json,
      article_tags_json,
      highlighted_text_json,
      images_json,
      author_name,
      author_url,
      publication_name,
      publication_url,
      published_label,
      capture_completeness,
      readable_status,
      error_message
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ",
  params = list(
    article_id,
    normalized_url,
    collected_at,
    clean_text(payload$source_type),
    clean_text(payload$extraction_method),
    text_hash,
    visible_text,
    integer_from_json(payload$word_count),
    logical_flag_from_json(payload$text_truncated),
    integer_from_json(payload$max_chars),
    text_blocks_json,
    article_tags_json,
    clean_text(jsonlite::toJSON(payload$highlighted_text, auto_unbox = TRUE, null = "null")),
    images_json,
    clean_text(payload$author_name),
    normalize_medium_url(scalar_from_json(payload$author_url)),
    clean_text(payload$publication_name),
    normalize_medium_url(scalar_from_json(payload$publication_url)),
    clean_text(payload$published_label),
    clean_text(payload$capture_completeness),
    clean_text(payload$readable_status),
    clean_text(payload$error_message)
  )
)

message("Imported Medium article text snapshot")
message("-------------------------------------")
message("Article metadata: ", article_status)
message("Text snapshot: ", if (rows_inserted > 0) "inserted" else "duplicate text hash; skipped")
message("URL: ", normalized_url)
message("Title: ", if (!is_missing_text(payload$title)) clean_text(payload$title) else "(blank)")
message("Author: ", if (!is_missing_text(payload$author_name)) clean_text(payload$author_name) else "(blank)")
message("Publication: ", if (!is_missing_text(payload$publication_name)) clean_text(payload$publication_name) else "(blank)")
message("Published label: ", if (!is_missing_text(payload$published_label)) clean_text(payload$published_label) else "(blank)")
message("Read time: ", if (!is_missing_text(payload$read_time)) clean_text(payload$read_time) else "(blank)")
message("Word count: ", ifelse(is.na(integer_from_json(payload$word_count)), "NA", integer_from_json(payload$word_count)))
message("Text blocks: ", if (is.null(payload$text_blocks)) 0L else length(payload$text_blocks))
message("Article tags: ", if (is.null(payload$article_tags)) 0L else length(payload$article_tags))
message("Highlighted passages: ", if (is.null(payload$highlighted_text)) 0L else length(payload$highlighted_text))
message("Images: ", if (is.null(payload$images)) 0L else length(payload$images))
message("Capture completeness: ", if (!is_missing_text(payload$capture_completeness)) clean_text(payload$capture_completeness) else "(blank)")
message("Readable status: ", if (!is_missing_text(payload$readable_status)) clean_text(payload$readable_status) else "(blank)")
message("text_hash: ", text_hash)
message("DB: ", database_path)
