required_packages <- c("xml2", "rvest", "DBI", "RSQLite", "lubridate")

# Stop early with a clear message if a package is missing.
# This script does not install packages automatically.
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("xml2", "rvest", "DBI", "RSQLite", "lubridate"))',
    call. = FALSE
  )
}

library(xml2)
library(rvest)
library(DBI)
library(RSQLite)
library(lubridate)

tags_to_collect <- c(
  "personal-finance",
  "investing",
  "finance",
  "stock-market",
  "etf",
  "financial-independence"
)

database_path <- file.path("data", "db", "medium_articles.sqlite")

article_columns <- c(
  "id",
  "source_tag",
  "title",
  "url",
  "raw_url",
  "guid",
  "author",
  "published_at",
  "updated_at",
  "fetched_at",
  "categories",
  "snippet",
  "image_url",
  "description_html",
  "content_encoded_html",
  "content_text"
)

# Make text easier to compare and store by removing extra whitespace.
clean_text <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return(NA_character_)
  }

  x <- trimws(x)
  x <- gsub("\\s+", " ", x)

  if (identical(x, "")) {
    return(NA_character_)
  }

  x
}

first_node_text <- function(node, xpath, namespaces = xml_ns(node)) {
  found_node <- tryCatch(
    xml_find_first(node, xpath, ns = namespaces),
    error = function(e) NULL
  )

  if (is.null(found_node) || inherits(found_node, "xml_missing")) {
    return(NA_character_)
  }

  clean_text(xml_text(found_node))
}

first_node_html <- function(node, xpath, namespaces = xml_ns(node)) {
  found_node <- tryCatch(
    xml_find_first(node, xpath, ns = namespaces),
    error = function(e) NULL
  )

  if (is.null(found_node) || inherits(found_node, "xml_missing")) {
    return(NA_character_)
  }

  value <- xml_text(found_node)

  if (length(value) == 0 || is.na(value) || trimws(value) == "") {
    return(NA_character_)
  }

  value
}

clean_url <- function(raw_url) {
  raw_url <- clean_text(raw_url)

  if (is.na(raw_url)) {
    return(NA_character_)
  }

  # Medium RSS links often include tracking query strings like ?source=...
  raw_url <- strsplit(raw_url, "[?#]")[[1]][1]
  raw_url
}

# Convert dates to a simple UTC timestamp when possible.
# If parsing fails, keep the original cleaned date text.
parse_date_text <- function(date_text) {
  date_text <- clean_text(date_text)

  if (is.na(date_text)) {
    return(NA_character_)
  }

  date_for_parsing <- sub("Z$", "+0000", date_text)
  date_for_parsing <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", date_for_parsing)

  date_formats <- c(
    "%a, %d %b %Y %H:%M:%S %z",
    "%a, %d %b %Y %H:%M:%S",
    "%Y-%m-%dT%H:%M:%S%z",
    "%Y-%m-%d %H:%M:%S %z",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S"
  )

  parsed_date <- NA

  for (date_format in date_formats) {
    parsed_date <- suppressWarnings(as.POSIXct(date_for_parsing, format = date_format, tz = "UTC"))

    if (!is.na(parsed_date)) {
      break
    }
  }

  if (is.na(parsed_date)) {
    return(date_text)
  }

  format(parsed_date, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

extract_html_text <- function(html_text) {
  if (length(html_text) == 0 || is.na(html_text) || trimws(html_text) == "") {
    return(NA_character_)
  }

  html_doc <- tryCatch(read_html(html_text), error = function(e) NULL)

  if (is.null(html_doc)) {
    return(clean_text(html_text))
  }

  clean_text(html_text2(html_doc))
}

extract_snippet <- function(description_html) {
  if (length(description_html) == 0 || is.na(description_html) || trimws(description_html) == "") {
    return(NA_character_)
  }

  html_doc <- tryCatch(read_html(description_html), error = function(e) NULL)

  if (is.null(html_doc)) {
    return(clean_text(description_html))
  }

  # Medium usually places the short RSS summary in this paragraph.
  snippet_node <- html_element(html_doc, "p.medium-feed-snippet")

  if (!inherits(snippet_node, "xml_missing") && length(snippet_node) > 0) {
    snippet_text <- clean_text(html_text2(snippet_node))

    if (!is.na(snippet_text)) {
      return(snippet_text)
    }
  }

  clean_text(html_text2(html_doc))
}

extract_image_url <- function(description_html) {
  if (length(description_html) == 0 || is.na(description_html) || trimws(description_html) == "") {
    return(NA_character_)
  }

  html_doc <- tryCatch(read_html(description_html), error = function(e) NULL)

  if (is.null(html_doc)) {
    return(NA_character_)
  }

  image_node <- html_element(html_doc, "img")

  if (inherits(image_node, "xml_missing") || length(image_node) == 0) {
    return(NA_character_)
  }

  clean_text(html_attr(image_node, "src"))
}

collapse_categories <- function(item_node) {
  category_nodes <- xml_find_all(item_node, "./*[local-name()='category']")
  categories <- trimws(xml_text(category_nodes))
  categories <- gsub("\\s+", " ", categories)
  categories <- categories[!is.na(categories)]
  categories <- categories[categories != ""]

  if (length(categories) == 0) {
    return(NA_character_)
  }

  paste(unique(categories), collapse = ", ")
}

article_from_item <- function(item_node, source_tag, fetched_at) {
  title <- first_node_text(item_node, "./*[local-name()='title']")
  raw_url <- first_node_text(item_node, "./*[local-name()='link']")
  url <- clean_url(raw_url)

  if (is.na(title) || is.na(url)) {
    return(NULL)
  }

  description_html <- first_node_html(item_node, "./*[local-name()='description']")
  content_encoded_html <- first_node_html(item_node, "./*[local-name()='encoded']")

  data.frame(
    source_tag = source_tag,
    title = title,
    url = url,
    raw_url = raw_url,
    guid = first_node_text(item_node, "./*[local-name()='guid']"),
    author = first_node_text(item_node, "./*[local-name()='creator']"),
    published_at = parse_date_text(first_node_text(item_node, "./*[local-name()='pubDate']")),
    updated_at = parse_date_text(first_node_text(item_node, "./*[local-name()='updated']")),
    fetched_at = fetched_at,
    categories = collapse_categories(item_node),
    snippet = extract_snippet(description_html),
    image_url = extract_image_url(description_html),
    description_html = description_html,
    content_encoded_html = content_encoded_html,
    content_text = extract_html_text(content_encoded_html),
    stringsAsFactors = FALSE
  )
}

articles_table_sql <- function(table_name = "medium_articles", if_not_exists = FALSE) {
  # The UNIQUE url column prevents duplicate rows when the script is run again.
  create_clause <- if (if_not_exists) {
    "CREATE TABLE IF NOT EXISTS"
  } else {
    "CREATE TABLE"
  }

  paste0("
    ", create_clause, " ", table_name, " (
      id INTEGER PRIMARY KEY,
      source_tag TEXT NOT NULL,
      title TEXT NOT NULL,
      url TEXT UNIQUE NOT NULL,
      raw_url TEXT,
      guid TEXT,
      author TEXT,
      published_at TEXT,
      updated_at TEXT,
      fetched_at TEXT NOT NULL,
      categories TEXT,
      snippet TEXT,
      image_url TEXT,
      description_html TEXT,
      content_encoded_html TEXT,
      content_text TEXT
    )
  ")
}

create_articles_table <- function(connection) {
  invisible(dbExecute(connection, articles_table_sql(if_not_exists = TRUE)))
}

table_uses_autoincrement <- function(connection) {
  table_info <- dbGetQuery(connection, "
    SELECT sql
    FROM sqlite_master
    WHERE type = 'table' AND name = 'medium_articles'
  ")

  if (nrow(table_info) == 0) {
    return(FALSE)
  }

  grepl("AUTOINCREMENT", table_info$sql[1], ignore.case = TRUE)
}

migrate_articles_table_if_needed <- function(connection, database_path) {
  if (!dbExistsTable(connection, "medium_articles")) {
    message("Schema check: medium_articles table does not exist yet. Creating it with id INTEGER PRIMARY KEY.")
    create_articles_table(connection)
    return(invisible(FALSE))
  }

  if (!table_uses_autoincrement(connection)) {
    message("Schema check: medium_articles already uses id INTEGER PRIMARY KEY. No migration needed.")
    return(invisible(FALSE))
  }

  backup_path <- file.path(
    dirname(database_path),
    paste0("medium_articles_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sqlite")
  )

  if (!file.copy(database_path, backup_path, overwrite = FALSE)) {
    stop("Could not create database backup before migration. Migration stopped.", call. = FALSE)
  }

  message("Backup created: ", backup_path)
  message("Schema migration: changing id from INTEGER PRIMARY KEY AUTOINCREMENT to INTEGER PRIMARY KEY.")

  columns_sql <- paste(article_columns, collapse = ", ")
  old_count <- dbGetQuery(connection, "SELECT COUNT(*) AS n FROM medium_articles")$n

  dbBegin(connection)

  tryCatch(
    {
      dbExecute(connection, "DROP TABLE IF EXISTS medium_articles_new")
      dbExecute(connection, articles_table_sql("medium_articles_new"))
      dbExecute(
        connection,
        paste0(
          "INSERT INTO medium_articles_new (", columns_sql, ") ",
          "SELECT ", columns_sql, " FROM medium_articles ORDER BY id"
        )
      )

      new_count <- dbGetQuery(connection, "SELECT COUNT(*) AS n FROM medium_articles_new")$n

      if (old_count != new_count) {
        stop("Row count changed during migration. Migration stopped before replacing the old table.")
      }

      dbExecute(connection, "DROP TABLE medium_articles")
      dbExecute(connection, "ALTER TABLE medium_articles_new RENAME TO medium_articles")
      dbCommit(connection)

      message("Schema migrated successfully. Existing rows preserved: ", new_count)
    },
    error = function(e) {
      dbRollback(connection)
      stop("Schema migration failed: ", conditionMessage(e), call. = FALSE)
    }
  )

  invisible(TRUE)
}

insert_article <- function(connection, article) {
  # Count before and after the insert so we can report duplicates clearly.
  before_count <- dbGetQuery(
    connection,
    "SELECT COUNT(*) AS n FROM medium_articles WHERE url = ?",
    params = list(article$url)
  )$n

  dbExecute(
    connection,
    "
      INSERT OR IGNORE INTO medium_articles (
        source_tag,
        title,
        url,
        raw_url,
        guid,
        author,
        published_at,
        updated_at,
        fetched_at,
        categories,
        snippet,
        image_url,
        description_html,
        content_encoded_html,
        content_text
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = unname(as.list(article[1, ]))
  )

  after_count <- dbGetQuery(
    connection,
    "SELECT COUNT(*) AS n FROM medium_articles WHERE url = ?",
    params = list(article$url)
  )$n

  after_count > before_count
}

collect_one_tag <- function(connection, source_tag) {
  feed_url <- paste0("https://medium.com/feed/tag/", source_tag)
  fetched_at <- format(with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")

  message("\nCollecting tag: ", source_tag)
  message("Feed: ", feed_url)

  # A single failed feed should not stop the whole collection run.
  feed <- tryCatch(
    read_xml(feed_url),
    error = function(e) {
      warning("Could not download or parse feed for tag '", source_tag, "': ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  if (is.null(feed)) {
    return(list(
      tag = source_tag,
      items_found = 0,
      new_rows = 0,
      duplicates = 0,
      invalid_items = 0,
      failed = TRUE
    ))
  }

  item_nodes <- xml_find_all(feed, ".//item")

  items_found <- length(item_nodes)
  new_rows <- 0
  duplicates <- 0
  invalid_items <- 0

  for (item_index in seq_along(item_nodes)) {
    article <- tryCatch(
      article_from_item(item_nodes[[item_index]], source_tag, fetched_at),
      error = function(e) {
        warning(
          "Skipping malformed item ",
          item_index,
          " in tag '",
          source_tag,
          "': ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )

    if (is.null(article)) {
      invalid_items <- invalid_items + 1
      warning("Skipping item ", item_index, " in tag '", source_tag, "' because it is missing title or URL.", call. = FALSE)
      next
    }

    inserted <- tryCatch(
      insert_article(connection, article),
      error = function(e) {
        warning(
          "Could not save item ",
          item_index,
          " in tag '",
          source_tag,
          "': ",
          conditionMessage(e),
          call. = FALSE
        )
        FALSE
      }
    )

    if (inserted) {
      new_rows <- new_rows + 1
    } else {
      duplicates <- duplicates + 1
    }
  }

  message("Items found: ", items_found)
  message("New rows inserted: ", new_rows)
  message("Duplicate rows skipped: ", duplicates)
  message("Invalid items skipped: ", invalid_items)

  list(
    tag = source_tag,
    items_found = items_found,
    new_rows = new_rows,
    duplicates = duplicates,
    invalid_items = invalid_items,
    failed = FALSE
  )
}

message("Medium RSS Collector")
message("====================")

dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("scripts", showWarnings = FALSE, recursive = TRUE)

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

migrate_articles_table_if_needed(connection, database_path)

results <- lapply(tags_to_collect, function(source_tag) {
  collect_one_tag(connection, source_tag)
})

failed_feeds <- vapply(results, function(x) isTRUE(x$failed), logical(1))
total_rows <- dbGetQuery(connection, "SELECT COUNT(*) AS n FROM medium_articles")$n

message("\nSummary")
message("=======")

for (result in results) {
  message(
    result$tag,
    ": found ",
    result$items_found,
    ", inserted ",
    result$new_rows,
    ", duplicates ",
    result$duplicates,
    ", invalid ",
    result$invalid_items
  )
}

if (any(failed_feeds)) {
  message("Failed feeds: ", paste(vapply(results[failed_feeds], `[[`, character(1), "tag"), collapse = ", "))
} else {
  message("Failed feeds: none")
}

message("Total rows now in database: ", total_rows)
message("\nDone. You can inspect the database with:")
message("Rscript scripts/inspect_medium_db.R")
