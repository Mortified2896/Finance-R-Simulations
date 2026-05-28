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
  out <- list(db = file.path("data", "db", "medium_articles.sqlite"), skip_backup = FALSE)
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
    paste0(tools::file_path_sans_ext(basename(database_path)), "_research_workflow_backup_", timestamp, ".sqlite")
  )
  if (!file.copy(database_path, backup_path, overwrite = FALSE)) {
    stop("Could not create backup at: ", backup_path, call. = FALSE)
  }
  backup_path
}

db_execute <- function(connection, sql) invisible(dbExecute(connection, sql))

table_exists <- function(connection, table_name) isTRUE(dbExistsTable(connection, table_name))

index_exists <- function(connection, index_name) {
  query <- "SELECT 1 FROM sqlite_master WHERE type = 'index' AND name = ? LIMIT 1"
  nrow(dbGetQuery(connection, query, params = list(index_name))) > 0
}

add_column_if_missing <- function(connection, table_name, column_name, definition) {
  columns <- dbGetQuery(connection, sprintf("PRAGMA table_info(%s)", dbQuoteIdentifier(connection, table_name)))$name
  if (!(column_name %in% columns)) {
    db_execute(connection, sprintf(
      "ALTER TABLE %s ADD COLUMN %s %s",
      dbQuoteIdentifier(connection, table_name),
      dbQuoteIdentifier(connection, column_name),
      definition
    ))
  }
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
database_path <- args$db

message("Research Workflow Schema Setup")
message("==============================")
message("DB path: ", database_path)

if (!file.exists(database_path)) stop("Could not find database at: ", database_path, call. = FALSE)

if (!args$skip_backup) {
  backup_path <- backup_database(database_path)
  message("Backup created: ", backup_path)
} else {
  message("Backup skipped because --skip-backup was supplied.")
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

db_execute(connection, "PRAGMA foreign_keys = ON")

table_names <- c("research_sources", "research_article_angles", "research_source_summaries", "research_source_assets", "research_summary_prompts")
index_names <- c(
  "idx_research_sources_status_sort_updated",
  "idx_research_sources_name_type",
  "idx_research_article_angles_status_sort_updated",
  "idx_research_article_angles_source",
  "idx_research_source_summaries_source_status_updated",
  "idx_research_source_summaries_status_confirmed_updated",
  "idx_research_source_assets_source_type_status_updated",
  "idx_research_source_assets_file_sha256"
)

preexisting_tables <- table_names[vapply(table_names, table_exists, logical(1), connection = connection)]
preexisting_indexes <- index_names[vapply(index_names, index_exists, logical(1), connection = connection)]

invisible(dbWithTransaction(connection, {
  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS research_sources (
      research_source_id INTEGER PRIMARY KEY,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      source_title TEXT NOT NULL,
      source_url TEXT,
      pdf_url TEXT,
      main_idea TEXT,
      abstract TEXT,
      source_type TEXT DEFAULT 'paper',
      source_name TEXT,
      manual_sort_order INTEGER,
      status TEXT NOT NULL DEFAULT 'new',
      notes TEXT,
      imported_from_table TEXT,
      imported_from_id TEXT
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS research_article_angles (
      research_angle_id INTEGER PRIMARY KEY,
      research_source_id INTEGER,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      angle_title TEXT NOT NULL,
      main_idea TEXT,
      manual_sort_order INTEGER,
      status TEXT NOT NULL DEFAULT 'idea',
      notes TEXT,
      article_lab_batch_id TEXT,
      FOREIGN KEY(research_source_id) REFERENCES research_sources(research_source_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS research_source_summaries (
      summary_id INTEGER PRIMARY KEY,
      research_source_id INTEGER NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      summary_text TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'draft',
      confirmed_at TEXT,
      model TEXT,
      prompt_version TEXT,
      notes TEXT,
      raw_json TEXT,
      FOREIGN KEY(research_source_id) REFERENCES research_sources(research_source_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS research_source_assets (
      asset_id INTEGER PRIMARY KEY,
      research_source_id INTEGER NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      asset_type TEXT NOT NULL DEFAULT 'pdf',
      source_url TEXT,
      local_path TEXT,
      original_filename TEXT,
      file_sha256 TEXT,
      status TEXT NOT NULL DEFAULT 'missing',
      error TEXT,
      notes TEXT,
      FOREIGN KEY(research_source_id) REFERENCES research_sources(research_source_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS research_summary_prompts (
      prompt_version TEXT PRIMARY KEY,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      prompt_text TEXT NOT NULL
    )
  ")

  source_columns <- list(
    created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''",
    source_title = "TEXT NOT NULL DEFAULT ''",
    source_url = "TEXT",
    pdf_url = "TEXT",
    main_idea = "TEXT",
    abstract = "TEXT",
    source_type = "TEXT DEFAULT 'paper'",
    source_name = "TEXT",
    manual_sort_order = "INTEGER",
    status = "TEXT NOT NULL DEFAULT 'new'",
    notes = "TEXT",
    imported_from_table = "TEXT",
    imported_from_id = "TEXT"
  )
  for (column_name in names(source_columns)) add_column_if_missing(connection, "research_sources", column_name, source_columns[[column_name]])

  angle_columns <- list(
    research_source_id = "INTEGER",
    created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''",
    angle_title = "TEXT NOT NULL DEFAULT ''",
    main_idea = "TEXT",
    manual_sort_order = "INTEGER",
    status = "TEXT NOT NULL DEFAULT 'idea'",
    notes = "TEXT",
    article_lab_batch_id = "TEXT"
  )
  for (column_name in names(angle_columns)) add_column_if_missing(connection, "research_article_angles", column_name, angle_columns[[column_name]])

  summary_columns <- list(
    research_source_id = "INTEGER NOT NULL DEFAULT 0",
    created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''",
    summary_text = "TEXT NOT NULL DEFAULT ''",
    status = "TEXT NOT NULL DEFAULT 'draft'",
    confirmed_at = "TEXT",
    model = "TEXT",
    prompt_version = "TEXT",
    notes = "TEXT",
    raw_json = "TEXT"
  )
  for (column_name in names(summary_columns)) add_column_if_missing(connection, "research_source_summaries", column_name, summary_columns[[column_name]])

  asset_columns <- list(
    research_source_id = "INTEGER NOT NULL DEFAULT 0",
    created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''",
    asset_type = "TEXT NOT NULL DEFAULT 'pdf'",
    source_url = "TEXT",
    local_path = "TEXT",
    original_filename = "TEXT",
    file_sha256 = "TEXT",
    status = "TEXT NOT NULL DEFAULT 'missing'",
    error = "TEXT",
    notes = "TEXT"
  )
  for (column_name in names(asset_columns)) add_column_if_missing(connection, "research_source_assets", column_name, asset_columns[[column_name]])

  prompt_columns <- list(
    prompt_version = "TEXT NOT NULL DEFAULT ''",
    created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''",
    prompt_text = "TEXT NOT NULL DEFAULT ''"
  )
  for (column_name in names(prompt_columns)) add_column_if_missing(connection, "research_summary_prompts", column_name, prompt_columns[[column_name]])

  db_execute(connection, "CREATE INDEX IF NOT EXISTS idx_research_sources_status_sort_updated ON research_sources (status, manual_sort_order, updated_at)")
  db_execute(connection, "CREATE INDEX IF NOT EXISTS idx_research_sources_name_type ON research_sources (source_name, source_type)")
  db_execute(connection, "CREATE INDEX IF NOT EXISTS idx_research_article_angles_status_sort_updated ON research_article_angles (status, manual_sort_order, updated_at)")
  db_execute(connection, "CREATE INDEX IF NOT EXISTS idx_research_article_angles_source ON research_article_angles (research_source_id)")
  db_execute(connection, "CREATE INDEX IF NOT EXISTS idx_research_source_summaries_source_status_updated ON research_source_summaries (research_source_id, status, updated_at)")
  db_execute(connection, "CREATE INDEX IF NOT EXISTS idx_research_source_summaries_status_confirmed_updated ON research_source_summaries (status, confirmed_at, updated_at)")
  db_execute(connection, "CREATE INDEX IF NOT EXISTS idx_research_source_assets_source_type_status_updated ON research_source_assets (research_source_id, asset_type, status, updated_at)")
  db_execute(connection, "CREATE INDEX IF NOT EXISTS idx_research_source_assets_file_sha256 ON research_source_assets (file_sha256)")
}))

created_tables <- setdiff(table_names, preexisting_tables)
created_indexes <- setdiff(index_names, preexisting_indexes)

message("Created tables: ", if (length(created_tables) == 0) "none" else paste(created_tables, collapse = ", "))
message("Created indexes: ", if (length(created_indexes) == 0) "none" else paste(created_indexes, collapse = ", "))
message("Research workflow schema is ready.")
