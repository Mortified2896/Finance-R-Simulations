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
    skip_backup = FALSE
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
    } else if (arg == "--skip-backup") {
      out$skip_backup <- TRUE
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1
  }
  out
}

backup_database <- function(database_path) {
  backup_dir <- file.path(dirname(database_path), "BackupFolder")
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_path <- file.path(
    backup_dir,
    paste0(tools::file_path_sans_ext(basename(database_path)), "_research_library_backup_", timestamp, ".sqlite")
  )
  if (!file.copy(database_path, backup_path, overwrite = FALSE)) {
    stop("Could not create backup at: ", backup_path, call. = FALSE)
  }
  backup_path
}

db_execute <- function(connection, sql) {
  invisible(dbExecute(connection, sql))
}

table_exists <- function(connection, table_name) {
  isTRUE(dbExistsTable(connection, table_name))
}

index_exists <- function(connection, index_name) {
  query <- "SELECT 1 FROM sqlite_master WHERE type = 'index' AND name = ? LIMIT 1"
  nrow(dbGetQuery(connection, query, params = list(index_name))) > 0
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
database_path <- args$db

message("Research Library Schema Setup")
message("=============================")
message("DB path: ", database_path)

if (!file.exists(database_path)) {
  stop("Could not find database at: ", database_path, call. = FALSE)
}

if (!args$skip_backup) {
  backup_path <- backup_database(database_path)
  message("Backup created: ", backup_path)
} else {
  message("Backup skipped because --skip-backup was supplied.")
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

db_execute(connection, "PRAGMA foreign_keys = ON")

table_names <- c("research_papers")
index_names <- c(
  "idx_research_papers_link_url",
  "idx_research_papers_source_name",
  "idx_research_papers_topic",
  "idx_research_papers_link_type",
  "idx_research_papers_article_suitability",
  "idx_research_papers_published_date"
)

preexisting_tables <- table_names[vapply(table_names, table_exists, logical(1), connection = connection)]
preexisting_indexes <- index_names[vapply(index_names, index_exists, logical(1), connection = connection)]

invisible(dbWithTransaction(connection, {
  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS research_papers (
      paper_id INTEGER PRIMARY KEY,
      source_name TEXT NOT NULL,
      source_type TEXT,
      source_collection TEXT,
      source_page_url TEXT,
      title TEXT NOT NULL,
      authors TEXT,
      topic TEXT,
      published_date TEXT,
      published_date_text TEXT,
      publication_year INTEGER,
      summary TEXT,
      link_url TEXT NOT NULL,
      link_type TEXT,
      pdf_url TEXT,
      external_id TEXT,
      doi TEXT,
      research_status TEXT NOT NULL DEFAULT 'inbox',
      article_suitability TEXT NOT NULL DEFAULT 'unknown',
      manual_priority INTEGER,
      used_in_project TEXT,
      notes TEXT,
      imported_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")

  db_execute(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_research_papers_link_url
    ON research_papers (link_url)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_research_papers_source_name
    ON research_papers (source_name)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_research_papers_topic
    ON research_papers (topic)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_research_papers_link_type
    ON research_papers (link_type)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_research_papers_article_suitability
    ON research_papers (article_suitability)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_research_papers_published_date
    ON research_papers (published_date)
  ")
}))

verified_tables <- table_names[vapply(table_names, table_exists, logical(1), connection = connection)]
verified_indexes <- index_names[vapply(index_names, index_exists, logical(1), connection = connection)]

created_tables <- setdiff(verified_tables, preexisting_tables)
created_indexes <- setdiff(verified_indexes, preexisting_indexes)

message("Tables verified (", length(verified_tables), "): ", paste(verified_tables, collapse = ", "))
message("Tables newly created this run: ", if (length(created_tables) > 0) paste(created_tables, collapse = ", ") else "none")
message("Indexes verified (", length(verified_indexes), "): ", paste(verified_indexes, collapse = ", "))
message("Indexes newly created this run: ", if (length(created_indexes) > 0) paste(created_indexes, collapse = ", ") else "none")
message("Research Library schema ready.")
