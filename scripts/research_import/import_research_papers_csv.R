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

csv_columns <- c(
  "source_name",
  "source_type",
  "source_collection",
  "source_page_url",
  "title",
  "authors",
  "topic",
  "published_date",
  "published_date_text",
  "publication_year",
  "summary",
  "link_url",
  "link_type",
  "pdf_url",
  "external_id",
  "doi",
  "research_status",
  "article_suitability",
  "manual_priority",
  "used_in_project",
  "notes"
)

metadata_columns <- c(
  "source_name",
  "source_type",
  "source_collection",
  "source_page_url",
  "title",
  "authors",
  "topic",
  "published_date",
  "published_date_text",
  "publication_year",
  "summary",
  "link_url",
  "link_type",
  "pdf_url",
  "external_id",
  "doi"
)

manual_columns <- c(
  "research_status",
  "article_suitability",
  "manual_priority",
  "used_in_project",
  "notes"
)

parse_args <- function(args) {
  out <- list(
    csv_path = NULL,
    db = file.path("data", "db", "medium_articles.sqlite")
  )
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--db") {
      i <- i + 1
      if (i > length(args)) stop("--db requires a path", call. = FALSE)
      out$db <- args[[i]]
    } else if (startsWith(arg, "--db=")) {
      out$db <- sub("^--db=", "", arg)
    } else if (startsWith(arg, "--")) {
      stop("Unknown argument: ", arg, call. = FALSE)
    } else if (is.null(out$csv_path)) {
      out$csv_path <- arg
    } else {
      stop("Unexpected extra argument: ", arg, call. = FALSE)
    }
    i <- i + 1
  }
  if (is.null(out$csv_path)) {
    stop("Usage: Rscript scripts/research_import/import_research_papers_csv.R path/to/research.csv [--db path/to/db.sqlite]", call. = FALSE)
  }
  out
}

clean_text <- function(value) {
  if (length(value) == 0 || is.na(value)) {
    return(NA_character_)
  }
  value <- trimws(as.character(value))
  if (identical(value, "")) {
    return(NA_character_)
  }
  value
}

clean_integer <- function(value) {
  value <- clean_text(value)
  if (is.na(value)) {
    return(NA_integer_)
  }
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed)) {
    return(NA_integer_)
  }
  parsed
}

first_or_default <- function(value, default) {
  value <- clean_text(value)
  if (is.na(value)) default else value
}

read_research_csv <- function(csv_path) {
  rows <- read.csv(csv_path, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
  names(rows) <- trimws(names(rows))

  if (!("title" %in% names(rows)) || !("link_url" %in% names(rows))) {
    stop("CSV must include at least title and link_url columns.", call. = FALSE)
  }

  for (column in csv_columns) {
    if (!(column %in% names(rows))) {
      rows[[column]] <- NA_character_
    }
  }

  rows
}

prepare_row <- function(row) {
  out <- list()
  for (column in csv_columns) {
    out[[column]] <- clean_text(row[[column]])
  }

  out$publication_year <- clean_integer(row$publication_year)
  out$manual_priority <- clean_integer(row$manual_priority)
  out$source_name <- first_or_default(out$source_name, "unknown")
  out$research_status <- first_or_default(out$research_status, "inbox")
  out$article_suitability <- first_or_default(out$article_suitability, "unknown")
  out
}

validate_database <- function(connection) {
  if (!dbExistsTable(connection, "research_papers")) {
    stop(
      "Table research_papers does not exist. Run:\n",
      "Rscript scripts/research_setup/apply_research_library_schema.R",
      call. = FALSE
    )
  }
}

get_existing_paper_id <- function(connection, link_url) {
  existing <- dbGetQuery(
    connection,
    "SELECT paper_id FROM research_papers WHERE link_url = ? LIMIT 1",
    params = list(link_url)
  )
  if (nrow(existing) == 0) {
    return(NA_integer_)
  }
  existing$paper_id[[1]]
}

insert_row <- function(connection, row, now) {
  params <- c(row[metadata_columns], row[manual_columns], list(imported_at = now, updated_at = now))
  dbExecute(
    connection,
    "
      INSERT INTO research_papers (
        source_name, source_type, source_collection, source_page_url, title, authors, topic,
        published_date, published_date_text, publication_year, summary, link_url, link_type,
        pdf_url, external_id, doi, research_status, article_suitability, manual_priority,
        used_in_project, notes, imported_at, updated_at
      ) VALUES (
        ?, ?, ?, ?, ?, ?, ?,
        ?, ?, ?, ?, ?, ?,
        ?, ?, ?, ?, ?, ?,
        ?, ?, ?, ?
      )
    ",
    params = unname(params)
  )
}

update_row <- function(connection, row, now) {
  params <- c(row[metadata_columns[metadata_columns != "link_url"]], list(updated_at = now, link_url = row$link_url))
  dbExecute(
    connection,
    "
      UPDATE research_papers
      SET source_name = ?,
          source_type = ?,
          source_collection = ?,
          source_page_url = ?,
          title = ?,
          authors = ?,
          topic = ?,
          published_date = ?,
          published_date_text = ?,
          publication_year = ?,
          summary = ?,
          link_type = ?,
          pdf_url = ?,
          external_id = ?,
          doi = ?,
          updated_at = ?
      WHERE link_url = ?
    ",
    params = unname(params)
  )
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
csv_path <- args$csv_path
database_path <- args$db

message("Research Library CSV Import")
message("===========================")
message("DB path: ", database_path)
message("CSV path: ", csv_path)

if (!file.exists(database_path)) {
  stop("Could not find database at: ", database_path, call. = FALSE)
}
if (!file.exists(csv_path)) {
  stop("Could not find CSV at: ", csv_path, call. = FALSE)
}

rows <- read_research_csv(csv_path)

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)
validate_database(connection)

inserted <- 0L
updated <- 0L
skipped <- 0L
imported_links <- character()
now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

invisible(dbWithTransaction(connection, {
  for (index in seq_len(nrow(rows))) {
    row <- prepare_row(rows[index, , drop = FALSE])

    if (is.na(row$title) || is.na(row$link_url)) {
      skipped <<- skipped + 1L
      next
    }

    existing_id <- get_existing_paper_id(connection, row$link_url)
    if (is.na(existing_id)) {
      insert_row(connection, row, now)
      inserted <<- inserted + 1L
    } else {
      update_row(connection, row, now)
      updated <<- updated + 1L
    }

    imported_links <<- c(imported_links, row$link_url)
  }
}))

message("\nImport summary")
message("--------------")
message("Inserted: ", inserted)
message("Updated: ", updated)
message("Skipped/invalid: ", skipped)
message("DB path: ", database_path)
message("CSV path: ", csv_path)

if (length(imported_links) > 0) {
  placeholders <- paste(rep("?", min(length(imported_links), 5L)), collapse = ", ")
  sample_rows <- dbGetQuery(
    connection,
    paste0(
      "SELECT paper_id, source_name, title, link_type, research_status, article_suitability, link_url ",
      "FROM research_papers WHERE link_url IN (", placeholders, ") ORDER BY paper_id LIMIT 5"
    ),
    params = as.list(imported_links[seq_len(min(length(imported_links), 5L))])
  )

  message("\nExample imported rows")
  message("---------------------")
  print(sample_rows, row.names = FALSE)
} else {
  message("\nNo valid rows imported.")
}
