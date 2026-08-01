article_candidate_statuses <- c("captured", "refining", "ready_for_evidence", "in_article_evidence", "archived")
article_candidate_origins <- c("quick_idea", "research_angle")

article_inbox_table_columns <- function(con, table_name) {
  if (!dbExistsTable(con, table_name)) return(character())
  dbGetQuery(con, sprintf("PRAGMA table_info(%s)", dbQuoteIdentifier(con, table_name)))$name
}

article_inbox_migrate_promoted_angles <- function(con) {
  if (!dbExistsTable(con, "research_article_angles")) return(invisible(TRUE))
  columns <- article_inbox_table_columns(con, "research_article_angles")
  if (!("article_lab_batch_id" %in% columns)) return(invisible(TRUE))
  rows <- dbGetQuery(con, "
    SELECT a.research_angle_id, a.research_source_id, a.created_at, a.updated_at,
      a.angle_title, a.main_idea, a.notes
    FROM research_article_angles a
    WHERE a.article_lab_batch_id IS NOT NULL AND TRIM(a.article_lab_batch_id) <> ''
  ")
  if (nrow(rows) == 0) return(invisible(TRUE))
  for (i in seq_len(nrow(rows))) {
    dbExecute(con, "
      INSERT OR IGNORE INTO article_candidates
        (candidate_id, working_title, core_idea, notes, origin_type, research_source_id, research_angle_id, status, created_at, updated_at)
      VALUES (?, ?, ?, ?, 'research_angle', ?, ?, 'captured', ?, ?)
    ", params = list(
      paste0("ac_ra_", rows$research_angle_id[[i]]), rows$angle_title[[i]], rows$main_idea[[i]], rows$notes[[i]],
      rows$research_source_id[[i]], rows$research_angle_id[[i]], rows$created_at[[i]], rows$updated_at[[i]]
    ))
  }
  invisible(TRUE)
}

ensure_article_inbox_schema <- function(con) {
  dbExecute(con, "PRAGMA foreign_keys = ON")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_candidates (
      candidate_id TEXT PRIMARY KEY,
      working_title TEXT NOT NULL,
      core_idea TEXT,
      notes TEXT,
      origin_type TEXT NOT NULL CHECK (origin_type IN ('quick_idea', 'research_angle')),
      research_source_id INTEGER,
      research_angle_id INTEGER,
      status TEXT NOT NULL DEFAULT 'captured' CHECK (status IN ('captured', 'refining', 'ready_for_evidence', 'in_article_evidence', 'archived')),
      status_before_archive TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      archived_at TEXT,
      FOREIGN KEY(research_source_id) REFERENCES research_sources(research_source_id),
      FOREIGN KEY(research_angle_id) REFERENCES research_article_angles(research_angle_id)
    )
  ")
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_projects (
      article_project_id TEXT PRIMARY KEY,
      article_candidate_id TEXT NOT NULL,
      working_title TEXT NOT NULL,
      core_idea TEXT,
      notes TEXT,
      origin_type TEXT NOT NULL,
      research_source_id INTEGER,
      research_angle_id INTEGER,
      source_summary_snapshot TEXT,
      provenance_json TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      archived_at TEXT,
      UNIQUE(article_candidate_id),
      FOREIGN KEY(article_candidate_id) REFERENCES article_candidates(candidate_id),
      FOREIGN KEY(research_source_id) REFERENCES research_sources(research_source_id),
      FOREIGN KEY(research_angle_id) REFERENCES research_article_angles(research_angle_id)
    )
  ")
  project_columns <- article_inbox_table_columns(con, "article_projects")
  if (!("archived_at" %in% project_columns)) {
    dbExecute(con, "ALTER TABLE article_projects ADD COLUMN archived_at TEXT")
  }
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_project_evidence_sources (
      article_project_evidence_source_id INTEGER PRIMARY KEY,
      article_project_id TEXT NOT NULL,
      research_source_id INTEGER NOT NULL,
      link_type TEXT NOT NULL DEFAULT 'origin',
      source_title_snapshot TEXT,
      source_summary_snapshot TEXT,
      created_at TEXT NOT NULL,
      UNIQUE(article_project_id, research_source_id),
      FOREIGN KEY(article_project_id) REFERENCES article_projects(article_project_id),
      FOREIGN KEY(research_source_id) REFERENCES research_sources(research_source_id)
    )
  ")
  dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_article_candidates_research_angle ON article_candidates (research_angle_id) WHERE research_angle_id IS NOT NULL")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_article_candidates_status_updated ON article_candidates (status, updated_at DESC)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_article_candidates_origin_updated ON article_candidates (origin_type, updated_at DESC)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_article_projects_candidate ON article_projects (article_candidate_id)")
  article_inbox_migrate_promoted_angles(con)
  invisible(TRUE)
}
