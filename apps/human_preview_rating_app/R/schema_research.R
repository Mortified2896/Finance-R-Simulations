ensure_research_workflow_schema <- function(con) {
  dbExecute(con, "
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
      used_articles TEXT,
      finished_at TEXT,
      notes TEXT,
      imported_from_table TEXT,
      imported_from_id TEXT
    )
  ")

  dbExecute(con, "
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

  dbExecute(con, "
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

  dbExecute(con, "
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

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS research_summary_prompts (
      prompt_version TEXT PRIMARY KEY,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      prompt_text TEXT NOT NULL
    )
  ")

  source_columns <- list(
    source_url = "TEXT", pdf_url = "TEXT", main_idea = "TEXT", abstract = "TEXT",
    source_type = "TEXT DEFAULT 'paper'", source_name = "TEXT", manual_sort_order = "INTEGER",
    used_articles = "TEXT", finished_at = "TEXT", notes = "TEXT", imported_from_table = "TEXT", imported_from_id = "TEXT"
  )
  for (column_name in names(source_columns)) db_add_column_if_missing(con, "research_sources", column_name, source_columns[[column_name]])

  angle_columns <- list(
    research_source_id = "INTEGER", main_idea = "TEXT", manual_sort_order = "INTEGER",
    notes = "TEXT", article_lab_batch_id = "TEXT"
  )
  for (column_name in names(angle_columns)) db_add_column_if_missing(con, "research_article_angles", column_name, angle_columns[[column_name]])

  summary_columns <- list(
    research_source_id = "INTEGER NOT NULL DEFAULT 0", created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''", summary_text = "TEXT NOT NULL DEFAULT ''",
    status = "TEXT NOT NULL DEFAULT 'draft'", confirmed_at = "TEXT", model = "TEXT",
    prompt_version = "TEXT", notes = "TEXT", raw_json = "TEXT"
  )
  for (column_name in names(summary_columns)) db_add_column_if_missing(con, "research_source_summaries", column_name, summary_columns[[column_name]])

  asset_columns <- list(
    research_source_id = "INTEGER NOT NULL DEFAULT 0", created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''", asset_type = "TEXT NOT NULL DEFAULT 'pdf'",
    source_url = "TEXT", local_path = "TEXT", original_filename = "TEXT", file_sha256 = "TEXT",
    status = "TEXT NOT NULL DEFAULT 'missing'", error = "TEXT", notes = "TEXT"
  )
  for (column_name in names(asset_columns)) db_add_column_if_missing(con, "research_source_assets", column_name, asset_columns[[column_name]])

  prompt_columns <- list(
    prompt_version = "TEXT NOT NULL DEFAULT ''", created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''", prompt_text = "TEXT NOT NULL DEFAULT ''"
  )
  for (column_name in names(prompt_columns)) db_add_column_if_missing(con, "research_summary_prompts", column_name, prompt_columns[[column_name]])

  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_sources_status_sort_updated ON research_sources (status, manual_sort_order, updated_at)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_sources_name_type ON research_sources (source_name, source_type)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_article_angles_status_sort_updated ON research_article_angles (status, manual_sort_order, updated_at)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_article_angles_source ON research_article_angles (research_source_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_source_summaries_source_status_updated ON research_source_summaries (research_source_id, status, updated_at)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_source_summaries_status_confirmed_updated ON research_source_summaries (status, confirmed_at, updated_at)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_source_assets_source_type_status_updated ON research_source_assets (research_source_id, asset_type, status, updated_at)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_research_source_assets_file_sha256 ON research_source_assets (file_sha256)")
}
