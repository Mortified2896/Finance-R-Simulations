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
    paste0(tools::file_path_sans_ext(basename(database_path)), "_writing_lab_backup_", timestamp, ".sqlite")
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

message("Writing Lab Schema Setup")
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
  "writing_projects",
  "writing_title_ideas",
  "writing_title_variants",
  "writing_thumbnail_variants",
  "writing_subtitle_variants",
  "writing_structure_variants",
  "writing_body_drafts",
  "writing_api_scores",
  "writing_human_scores",
  "writing_decisions"
)

index_names <- c(
  "idx_writing_projects_project_slug",
  "idx_writing_title_ideas_project_id",
  "idx_writing_title_variants_project_id",
  "idx_writing_thumbnail_variants_project_id",
  "idx_writing_subtitle_variants_project_id",
  "idx_writing_structure_variants_project_id",
  "idx_writing_body_drafts_project_id",
  "idx_writing_api_scores_project_id",
  "idx_writing_api_scores_target",
  "idx_writing_human_scores_project_id",
  "idx_writing_human_scores_target",
  "idx_writing_decisions_project_id",
  "idx_writing_decisions_target"
)

preexisting_tables <- table_names[vapply(table_names, table_exists, logical(1), connection = connection)]
preexisting_indexes <- index_names[vapply(index_names, index_exists, logical(1), connection = connection)]

invisible(dbWithTransaction(connection, {
  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS writing_projects (
      project_id INTEGER PRIMARY KEY,
      project_slug TEXT NOT NULL UNIQUE,
      working_topic TEXT,
      status TEXT NOT NULL DEFAULT 'active',
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      comments TEXT
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS writing_title_ideas (
      title_idea_id INTEGER PRIMARY KEY,
      project_id INTEGER NOT NULL,
      title TEXT NOT NULL,
      comments TEXT,
      status TEXT NOT NULL DEFAULT 'idea',
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY (project_id) REFERENCES writing_projects(project_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS writing_title_variants (
      title_variant_id INTEGER PRIMARY KEY,
      project_id INTEGER NOT NULL,
      title TEXT NOT NULL,
      comments TEXT,
      status TEXT NOT NULL DEFAULT 'idea',
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY (project_id) REFERENCES writing_projects(project_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS writing_thumbnail_variants (
      thumbnail_variant_id INTEGER PRIMARY KEY,
      project_id INTEGER NOT NULL,
      title_variant_id INTEGER,
      thumbnail_file TEXT,
      thumbnail_concept TEXT,
      comments TEXT,
      status TEXT NOT NULL DEFAULT 'idea',
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY (project_id) REFERENCES writing_projects(project_id),
      FOREIGN KEY (title_variant_id) REFERENCES writing_title_variants(title_variant_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS writing_subtitle_variants (
      subtitle_variant_id INTEGER PRIMARY KEY,
      project_id INTEGER NOT NULL,
      title_variant_id INTEGER,
      thumbnail_variant_id INTEGER,
      subtitle TEXT NOT NULL,
      comments TEXT,
      status TEXT NOT NULL DEFAULT 'idea',
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY (project_id) REFERENCES writing_projects(project_id),
      FOREIGN KEY (title_variant_id) REFERENCES writing_title_variants(title_variant_id),
      FOREIGN KEY (thumbnail_variant_id) REFERENCES writing_thumbnail_variants(thumbnail_variant_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS writing_structure_variants (
      structure_variant_id INTEGER PRIMARY KEY,
      project_id INTEGER NOT NULL,
      title_variant_id INTEGER,
      thumbnail_variant_id INTEGER,
      subtitle_variant_id INTEGER,
      structure_markdown TEXT NOT NULL,
      comments TEXT,
      status TEXT NOT NULL DEFAULT 'idea',
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY (project_id) REFERENCES writing_projects(project_id),
      FOREIGN KEY (title_variant_id) REFERENCES writing_title_variants(title_variant_id),
      FOREIGN KEY (thumbnail_variant_id) REFERENCES writing_thumbnail_variants(thumbnail_variant_id),
      FOREIGN KEY (subtitle_variant_id) REFERENCES writing_subtitle_variants(subtitle_variant_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS writing_body_drafts (
      body_draft_id INTEGER PRIMARY KEY,
      project_id INTEGER NOT NULL,
      structure_variant_id INTEGER,
      body_markdown TEXT NOT NULL,
      comments TEXT,
      status TEXT NOT NULL DEFAULT 'draft',
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY (project_id) REFERENCES writing_projects(project_id),
      FOREIGN KEY (structure_variant_id) REFERENCES writing_structure_variants(structure_variant_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS writing_api_scores (
      api_score_id INTEGER PRIMARY KEY,
      project_id INTEGER NOT NULL,
      target_type TEXT NOT NULL,
      target_id INTEGER NOT NULL,
      score_scope TEXT NOT NULL,
      prompt_version TEXT NOT NULL,
      model TEXT NOT NULL,
      scored_at TEXT NOT NULL,
      raw_json TEXT NOT NULL,
      clarity INTEGER,
      curiosity INTEGER,
      specificity INTEGER,
      beginner_appeal INTEGER,
      credibility INTEGER,
      emotional_pull INTEGER,
      promise_strength INTEGER,
      medium_clap_potential INTEGER,
      medium_comment_potential INTEGER,
      overall_article_potential INTEGER,
      trust_risk INTEGER,
      predicted_success_bucket TEXT,
      short_reason TEXT,
      visual_clarity INTEGER,
      visual_hook INTEGER,
      visual_relevance INTEGER,
      visual_distinctiveness INTEGER,
      professional_credibility INTEGER,
      emotional_pull_visual INTEGER,
      finance_topic_fit INTEGER,
      generic_stock_photo_risk INTEGER,
      ai_or_low_quality_risk INTEGER,
      text_readability INTEGER,
      overall_thumbnail_potential INTEGER,
      FOREIGN KEY (project_id) REFERENCES writing_projects(project_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS writing_human_scores (
      human_score_id INTEGER PRIMARY KEY,
      project_id INTEGER NOT NULL,
      target_type TEXT NOT NULL,
      target_id INTEGER NOT NULL,
      rating_version TEXT NOT NULL DEFAULT 'human_preview_dimensions_v2',
      rated_at TEXT NOT NULL,
      personal_click_appeal INTEGER,
      title_hook_strength INTEGER,
      visual_hook INTEGER,
      emotional_pull_preview INTEGER,
      ai_low_effort_flag INTEGER,
      dimension_note TEXT,
      FOREIGN KEY (project_id) REFERENCES writing_projects(project_id)
    )
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS writing_decisions (
      decision_id INTEGER PRIMARY KEY,
      project_id INTEGER NOT NULL,
      target_type TEXT NOT NULL,
      target_id INTEGER NOT NULL,
      decision TEXT NOT NULL,
      reason TEXT,
      decided_at TEXT NOT NULL,
      FOREIGN KEY (project_id) REFERENCES writing_projects(project_id)
    )
  ")

  db_execute(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_writing_projects_project_slug
      ON writing_projects(project_slug)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_title_ideas_project_id
      ON writing_title_ideas(project_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_title_variants_project_id
      ON writing_title_variants(project_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_thumbnail_variants_project_id
      ON writing_thumbnail_variants(project_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_subtitle_variants_project_id
      ON writing_subtitle_variants(project_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_structure_variants_project_id
      ON writing_structure_variants(project_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_body_drafts_project_id
      ON writing_body_drafts(project_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_api_scores_project_id
      ON writing_api_scores(project_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_api_scores_target
      ON writing_api_scores(target_type, target_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_human_scores_project_id
      ON writing_human_scores(project_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_human_scores_target
      ON writing_human_scores(target_type, target_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_decisions_project_id
      ON writing_decisions(project_id)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_writing_decisions_target
      ON writing_decisions(target_type, target_id)
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
