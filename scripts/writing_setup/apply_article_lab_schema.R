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
    paste0(tools::file_path_sans_ext(basename(database_path)), "_article_lab_backup_", timestamp, ".sqlite")
  )
  if (!file.copy(database_path, backup_path, overwrite = FALSE)) {
    stop("Could not create backup at: ", backup_path, call. = FALSE)
  }
  backup_path
}

table_exists <- function(connection, table_name) {
  isTRUE(dbExistsTable(connection, table_name))
}

index_exists <- function(connection, index_name) {
  query <- "SELECT 1 FROM sqlite_master WHERE type = 'index' AND name = ? LIMIT 1"
  nrow(dbGetQuery(connection, query, params = list(index_name))) > 0
}

db_execute <- function(connection, sql) {
  invisible(dbExecute(connection, sql))
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
database_path <- args$db

message("Article Lab Schema Setup")
message("========================")
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

table_names <- c(
  "article_lab_title_batches",
  "article_lab_title_candidates",
  "article_lab_title_api_scores"
)

index_names <- c(
  "idx_article_lab_title_batches_created_at",
  "idx_article_lab_title_candidates_batch",
  "idx_article_lab_title_candidates_status",
  "idx_article_lab_title_api_scores_batch",
  "idx_article_lab_title_api_scores_cache",
  "idx_article_lab_title_api_scores_prompt"
)

preexisting_tables <- table_names[vapply(table_names, table_exists, logical(1), connection = connection)]
preexisting_indexes <- index_names[vapply(index_names, index_exists, logical(1), connection = connection)]

invisible(dbWithTransaction(connection, {
  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS article_lab_title_batches (
      batch_id TEXT PRIMARY KEY,
      created_at TEXT NOT NULL,
      prompt TEXT NOT NULL,
      seed_topic TEXT,
      inspiration_source TEXT,
      requested_batch_size INTEGER,
      model TEXT,
      status TEXT NOT NULL DEFAULT 'generated',
      notes TEXT
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS article_lab_title_candidates (
      candidate_id TEXT PRIMARY KEY,
      batch_id TEXT NOT NULL,
      created_at TEXT NOT NULL,
      title TEXT NOT NULL,
      title_char_count INTEGER,
      title_length_flag TEXT,
      status TEXT NOT NULL DEFAULT 'generated',
      source TEXT NOT NULL DEFAULT 'article_lab',
      ready_for_human_rating INTEGER NOT NULL DEFAULT 0,
      promoted INTEGER NOT NULL DEFAULT 0,
      archived INTEGER NOT NULL DEFAULT 0,
      notes TEXT,
      raw_json TEXT,
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_batches_created_at
    ON article_lab_title_batches (created_at, batch_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_candidates_batch
    ON article_lab_title_candidates (batch_id, created_at, candidate_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_candidates_status
    ON article_lab_title_candidates (status, ready_for_human_rating, archived, promoted)
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS article_lab_title_api_scores (
      score_id TEXT PRIMARY KEY,
      candidate_id TEXT NOT NULL,
      batch_id TEXT NOT NULL,
      scored_at TEXT NOT NULL,
      model TEXT NOT NULL,
      prompt_version TEXT NOT NULL,
      scope TEXT NOT NULL,
      clarity REAL,
      curiosity REAL,
      specificity REAL,
      beginner_appeal REAL,
      credibility REAL,
      emotional_pull REAL,
      promise_strength REAL,
      trust_risk REAL,
      medium_clap_potential REAL,
      medium_comment_potential REAL,
      overall_article_potential REAL,
      combined_title_score REAL,
      predicted_success_bucket TEXT,
      short_reason TEXT,
      raw_json TEXT,
      error TEXT,
      FOREIGN KEY(candidate_id) REFERENCES article_lab_title_candidates(candidate_id),
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_api_scores_batch
    ON article_lab_title_api_scores (batch_id, scored_at, candidate_id)
  ")

  db_execute(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_article_lab_title_api_scores_cache
    ON article_lab_title_api_scores (candidate_id, model, prompt_version, scope)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_api_scores_prompt
    ON article_lab_title_api_scores (prompt_version, model, scope)
  ")

  candidate_columns <- dbGetQuery(connection, "PRAGMA table_info(article_lab_title_candidates)")$name
  if (!("title_char_count" %in% candidate_columns)) {
    db_execute(connection, "ALTER TABLE article_lab_title_candidates ADD COLUMN title_char_count INTEGER")
  }
  if (!("title_length_flag" %in% candidate_columns)) {
    db_execute(connection, "ALTER TABLE article_lab_title_candidates ADD COLUMN title_length_flag TEXT")
  }
  if (!("notes" %in% candidate_columns)) {
    db_execute(connection, "ALTER TABLE article_lab_title_candidates ADD COLUMN notes TEXT")
  }

  db_execute(connection, "
    UPDATE article_lab_title_candidates
    SET title_char_count = COALESCE(title_char_count, LENGTH(COALESCE(title, ''))),
        title_length_flag = CASE
          WHEN LENGTH(COALESCE(title, '')) <= 45 THEN 'mobile_safe'
          WHEN LENGTH(COALESCE(title, '')) <= 60 THEN 'good'
          WHEN LENGTH(COALESCE(title, '')) <= 90 THEN 'long_but_allowed'
          WHEN LENGTH(COALESCE(title, '')) <= 140 THEN 'very_long_but_allowed'
          ELSE 'too_long'
        END
    WHERE title_char_count IS NULL
       OR title_length_flag IS NULL
       OR title_length_flag = 'risky'
       OR (title_length_flag = 'too_long' AND LENGTH(COALESCE(title, '')) <= 140)
       OR title_length_flag NOT IN ('mobile_safe', 'good', 'long_but_allowed', 'very_long_but_allowed', 'too_long')
  ")

  db_execute(connection, "
    UPDATE article_lab_title_candidates
    SET ready_for_human_rating = 0,
        status = CASE
          WHEN candidate_id IN (
            SELECT DISTINCT candidate_id
            FROM article_lab_title_api_scores
          ) THEN 'api_scored'
          ELSE 'generated'
        END
    WHERE ready_for_human_rating = 1
      AND status = 'ready_for_human_rating'
      AND archived = 0
  ")

  db_execute(connection, "
    UPDATE article_lab_title_candidates
    SET status = 'approved_for_subtitle',
        promoted = 1,
        ready_for_human_rating = 0,
        archived = 0
    WHERE promoted = 1
       OR status IN ('promoted', 'approved', 'approved_for_subtitle')
  ")

  db_execute(connection, "
    UPDATE article_lab_title_candidates
    SET status = 'archived',
        ready_for_human_rating = 0
    WHERE archived = 1
  ")
}))

created_tables <- setdiff(table_names, preexisting_tables)
created_indexes <- setdiff(index_names, preexisting_indexes)

if (length(created_tables) == 0 && length(created_indexes) == 0) {
  message("Schema already up to date. No new Article Lab objects created.")
} else {
  if (length(created_tables) > 0) message("Created tables: ", paste(created_tables, collapse = ", "))
  if (length(created_indexes) > 0) message("Created indexes: ", paste(created_indexes, collapse = ", "))
}

message("Article Lab schema ready.")
