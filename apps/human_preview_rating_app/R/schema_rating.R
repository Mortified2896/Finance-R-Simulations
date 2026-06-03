ensure_rating_schema <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_rating_sessions (
      rating_session_id TEXT PRIMARY KEY,
      created_at TEXT,
      interface_version TEXT,
      rating_mode TEXT,
      queue_seed INTEGER,
      target_n INTEGER,
      notes TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_rating_queue (
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      status TEXT DEFAULT 'pending',
      shown_at TEXT,
      completed_at TEXT,
      PRIMARY KEY (rating_session_id, queue_position)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_ratings (
      id INTEGER PRIMARY KEY,
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      interface_version TEXT,
      rating_prompt TEXT,
      shown_title TEXT,
      shown_subtitle TEXT,
      shown_thumbnail_path TEXT,
      human_feed_success_potential INTEGER,
      human_feed_success_note TEXT,
      skipped INTEGER DEFAULT 0,
      shown_at TEXT,
      rated_at TEXT,
      seconds_spent REAL
    )
  ")

  queue_columns <- c(
    source_type = "TEXT",
    article_lab_candidate_id = "TEXT"
  )
  for (column_name in names(queue_columns)) {
    db_add_column_if_missing(con, "human_preview_rating_queue", column_name, queue_columns[[column_name]])
  }

  rating_columns <- c(
    source_type = "TEXT",
    article_lab_candidate_id = "TEXT"
  )
  for (column_name in names(rating_columns)) {
    db_add_column_if_missing(con, "human_preview_ratings", column_name, rating_columns[[column_name]])
  }

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_dimension_rating_queue (
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      canonical_article_key TEXT,
      status TEXT DEFAULT 'pending',
      shown_at TEXT,
      completed_at TEXT,
      PRIMARY KEY (rating_session_id, queue_position)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_dimension_ratings (
      id INTEGER PRIMARY KEY,
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      canonical_article_key TEXT,
      interface_version TEXT,
      rating_mode TEXT,
      shown_title TEXT,
      shown_subtitle TEXT,
      shown_thumbnail_path TEXT,
      personal_click_appeal INTEGER,
      title_hook_strength INTEGER,
      visual_hook INTEGER,
      emotional_pull_preview INTEGER,
      ai_low_effort_flag TEXT,
      human_dimension_note TEXT,
      skipped INTEGER DEFAULT 0,
      shown_at TEXT,
      rated_at TEXT,
      seconds_spent REAL
    )
  ")

  dimension_rating_columns <- c(
    rating_session_id = "TEXT",
    queue_position = "INTEGER",
    interface_version = "TEXT",
    skipped = "INTEGER DEFAULT 0",
    shown_at = "TEXT",
    rated_at = "TEXT",
    seconds_spent = "REAL",
    created_at = "TEXT",
    updated_at = "TEXT"
  )
  for (column_name in names(dimension_rating_columns)) {
    db_add_column_if_missing(con, "human_preview_dimension_ratings", column_name, dimension_rating_columns[[column_name]])
  }

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_dimension_ratings_v2 (
      id INTEGER PRIMARY KEY,
      rating_session_id TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      canonical_article_key TEXT,
      interface_version TEXT,
      rating_mode TEXT,
      manifest_version TEXT,
      shown_title TEXT,
      shown_subtitle TEXT,
      shown_thumbnail_path TEXT,
      shown_image_sha256 TEXT,
      personal_click_appeal INTEGER,
      title_hook_strength INTEGER,
      visual_hook INTEGER,
      emotional_pull_preview INTEGER,
      ai_low_effort_flag TEXT,
      human_dimension_note TEXT,
      skipped INTEGER DEFAULT 0,
      shown_at TEXT,
      rated_at TEXT,
      seconds_spent REAL,
      created_at TEXT,
      updated_at TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS human_preview_dimension_pass_queue (
      rating_mode TEXT,
      active_dimension TEXT,
      queue_position INTEGER,
      article_id INTEGER,
      medium_post_id TEXT,
      canonical_article_key TEXT,
      status TEXT DEFAULT 'pending',
      shown_at TEXT,
      completed_at TEXT,
      seconds_spent REAL,
      PRIMARY KEY (rating_mode, active_dimension, queue_position)
    )
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_queue_status
    ON human_preview_rating_queue (rating_session_id, status, queue_position)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_ratings_session
    ON human_preview_ratings (rating_session_id, rated_at, id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_queue_source
    ON human_preview_rating_queue (rating_session_id, source_type, article_lab_candidate_id, status)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_ratings_source
    ON human_preview_ratings (source_type, article_lab_candidate_id, rated_at, id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_dimension_queue_status
    ON human_preview_dimension_rating_queue (rating_session_id, status, queue_position)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_mode
    ON human_preview_dimension_ratings (rating_mode, rated_at, id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_v2_mode
    ON human_preview_dimension_ratings_v2 (rating_mode, manifest_version, updated_at, id)
  ")

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_canonical_unique
    ON human_preview_dimension_ratings (rating_mode, canonical_article_key)
    WHERE canonical_article_key IS NOT NULL
  "), silent = TRUE)

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_article_unique
    ON human_preview_dimension_ratings (rating_mode, article_id)
    WHERE canonical_article_key IS NULL AND article_id IS NOT NULL
  "), silent = TRUE)

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_post_unique
    ON human_preview_dimension_ratings (rating_mode, medium_post_id)
    WHERE canonical_article_key IS NULL AND article_id IS NULL AND medium_post_id IS NOT NULL
  "), silent = TRUE)

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_v2_canonical_unique
    ON human_preview_dimension_ratings_v2 (rating_mode, manifest_version, canonical_article_key)
    WHERE canonical_article_key IS NOT NULL
  "), silent = TRUE)

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_v2_article_unique
    ON human_preview_dimension_ratings_v2 (rating_mode, manifest_version, article_id)
    WHERE canonical_article_key IS NULL AND article_id IS NOT NULL
  "), silent = TRUE)

  try(dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_human_preview_dimension_ratings_v2_post_unique
    ON human_preview_dimension_ratings_v2 (rating_mode, manifest_version, medium_post_id)
    WHERE canonical_article_key IS NULL AND article_id IS NULL AND medium_post_id IS NOT NULL
  "), silent = TRUE)

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_human_preview_dimension_pass_queue_status
    ON human_preview_dimension_pass_queue (rating_mode, active_dimension, status, queue_position)
  ")
}
