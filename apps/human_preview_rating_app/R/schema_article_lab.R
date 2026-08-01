ensure_article_lab_schema <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_prompts (
      prompt_key TEXT PRIMARY KEY,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      prompt_text TEXT NOT NULL
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_title_batches (
      batch_id TEXT PRIMARY KEY,
      article_project_id TEXT,
      created_at TEXT NOT NULL,
      prompt TEXT NOT NULL,
      seed_topic TEXT,
      inspiration_source TEXT,
      requested_batch_size INTEGER,
      model TEXT,
      status TEXT NOT NULL DEFAULT 'generated',
      notes TEXT,
      FOREIGN KEY(article_project_id) REFERENCES article_projects(article_project_id)
    )
  ")

  db_add_column_if_missing(con, "article_lab_title_batches", "article_project_id", "TEXT")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_article_lab_title_batches_project ON article_lab_title_batches (article_project_id, created_at DESC)")

  dbExecute(con, "
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

  db_add_column_if_missing(con, "article_lab_title_batches", "article_context_notes", "TEXT")

  db_add_column_if_missing(con, "article_lab_title_candidates", "title_char_count", "INTEGER")
  db_add_column_if_missing(con, "article_lab_title_candidates", "title_length_flag", "TEXT")
  db_add_column_if_missing(con, "article_lab_title_candidates", "notes", "TEXT")

  prompt_columns <- list(
    prompt_key = "TEXT NOT NULL DEFAULT ''", created_at = "TEXT NOT NULL DEFAULT ''",
    updated_at = "TEXT NOT NULL DEFAULT ''", prompt_text = "TEXT NOT NULL DEFAULT ''"
  )
  for (column_name in names(prompt_columns)) db_add_column_if_missing(con, "article_lab_prompts", column_name, prompt_columns[[column_name]])

  dbExecute(con, "
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

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_subtitle_candidates (
      subtitle_id TEXT PRIMARY KEY,
      candidate_id TEXT NOT NULL,
      batch_id TEXT NOT NULL,
      created_at TEXT NOT NULL,
      subtitle TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'generated',
      notes TEXT,
      model TEXT,
      generation_mode TEXT NOT NULL DEFAULT 'generated',
      raw_json TEXT,
      approved_at TEXT,
      rejected_at TEXT,
      FOREIGN KEY(candidate_id) REFERENCES article_lab_title_candidates(candidate_id),
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "notes", "TEXT")
  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "model", "TEXT")
  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "generation_mode", "TEXT")
  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "raw_json", "TEXT")
  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "approved_at", "TEXT")
  db_add_column_if_missing(con, "article_lab_subtitle_candidates", "rejected_at", "TEXT")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_thumbnail_candidates (
      thumbnail_id TEXT PRIMARY KEY,
      subtitle_id TEXT NOT NULL,
      candidate_id TEXT NOT NULL,
      batch_id TEXT NOT NULL,
      created_at TEXT NOT NULL,
      thumbnail_label TEXT,
      thumbnail_data_uri TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'generated',
      notes TEXT,
      model TEXT,
      generation_mode TEXT NOT NULL DEFAULT 'generated',
      raw_json TEXT,
      approved_at TEXT,
      rejected_at TEXT,
      FOREIGN KEY(subtitle_id) REFERENCES article_lab_subtitle_candidates(subtitle_id),
      FOREIGN KEY(candidate_id) REFERENCES article_lab_title_candidates(candidate_id),
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "thumbnail_label", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "notes", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "model", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "generation_mode", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "raw_json", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "approved_at", "TEXT")
  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "rejected_at", "TEXT")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_outlines (
      outline_id TEXT PRIMARY KEY,
      thumbnail_id TEXT NOT NULL,
      subtitle_id TEXT NOT NULL,
      candidate_id TEXT NOT NULL,
      batch_id TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      outline_text TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'draft',
      notes TEXT,
      model TEXT,
      generation_mode TEXT NOT NULL DEFAULT 'generated',
      raw_json TEXT,
      approved_at TEXT,
      FOREIGN KEY(thumbnail_id) REFERENCES article_lab_thumbnail_candidates(thumbnail_id),
      FOREIGN KEY(subtitle_id) REFERENCES article_lab_subtitle_candidates(subtitle_id),
      FOREIGN KEY(candidate_id) REFERENCES article_lab_title_candidates(candidate_id),
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  db_add_column_if_missing(con, "article_lab_outlines", "updated_at", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "notes", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "model", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "generation_mode", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "raw_json", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "approved_at", "TEXT")
  db_add_column_if_missing(con, "article_lab_outlines", "archived", "INTEGER NOT NULL DEFAULT 0")
  db_add_column_if_missing(con, "article_lab_outlines", "archived_at", "TEXT")

  db_add_column_if_missing(con, "article_lab_thumbnail_candidates", "outline_context_notes", "TEXT")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_full_text_drafts (
      full_text_draft_id TEXT PRIMARY KEY,
      outline_id TEXT,
      thumbnail_id TEXT NOT NULL,
      subtitle_id TEXT NOT NULL,
      candidate_id TEXT NOT NULL,
      batch_id TEXT NOT NULL,
      original_generated_text TEXT NOT NULL,
      current_draft_text TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'draft',
      is_approved INTEGER NOT NULL DEFAULT 0,
      model TEXT,
      prompt_key TEXT,
      prompt_version TEXT,
      generation_mode TEXT NOT NULL DEFAULT 'generated',
      source_context_mode TEXT,
      raw_json TEXT,
      notes TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      approved_at TEXT,
      rejected_at TEXT,
      FOREIGN KEY(outline_id) REFERENCES article_lab_outlines(outline_id),
      FOREIGN KEY(thumbnail_id) REFERENCES article_lab_thumbnail_candidates(thumbnail_id),
      FOREIGN KEY(subtitle_id) REFERENCES article_lab_subtitle_candidates(subtitle_id),
      FOREIGN KEY(candidate_id) REFERENCES article_lab_title_candidates(candidate_id),
      FOREIGN KEY(batch_id) REFERENCES article_lab_title_batches(batch_id)
    )
  ")

  full_text_columns <- list(
    outline_id = "TEXT", thumbnail_id = "TEXT NOT NULL DEFAULT ''", subtitle_id = "TEXT NOT NULL DEFAULT ''",
    candidate_id = "TEXT NOT NULL DEFAULT ''", batch_id = "TEXT NOT NULL DEFAULT ''",
    original_generated_text = "TEXT NOT NULL DEFAULT ''", current_draft_text = "TEXT NOT NULL DEFAULT ''",
    status = "TEXT NOT NULL DEFAULT 'draft'", is_approved = "INTEGER NOT NULL DEFAULT 0",
    model = "TEXT", prompt_key = "TEXT", prompt_version = "TEXT", generation_mode = "TEXT NOT NULL DEFAULT 'generated'",
    source_context_mode = "TEXT", citation_map_json = "TEXT", raw_json = "TEXT", notes = "TEXT",
    created_at = "TEXT NOT NULL DEFAULT ''", updated_at = "TEXT NOT NULL DEFAULT ''",
    approved_at = "TEXT", rejected_at = "TEXT"
  )
  for (column_name in names(full_text_columns)) db_add_column_if_missing(con, "article_lab_full_text_drafts", column_name, full_text_columns[[column_name]])

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_full_text_draft_revisions (
      revision_id TEXT PRIMARY KEY,
      full_text_draft_id TEXT NOT NULL,
      previous_text TEXT,
      new_text TEXT,
      edit_source TEXT NOT NULL DEFAULT 'manual_save',
      edit_note TEXT,
      created_at TEXT NOT NULL,
      FOREIGN KEY(full_text_draft_id) REFERENCES article_lab_full_text_drafts(full_text_draft_id)
    )
  ")

  revision_columns <- list(
    full_text_draft_id = "TEXT NOT NULL DEFAULT ''", previous_text = "TEXT", new_text = "TEXT",
    edit_source = "TEXT NOT NULL DEFAULT 'manual_save'", edit_note = "TEXT", created_at = "TEXT NOT NULL DEFAULT ''"
  )
  for (column_name in names(revision_columns)) db_add_column_if_missing(con, "article_lab_full_text_draft_revisions", column_name, revision_columns[[column_name]])

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_publications (
      publication_id TEXT PRIMARY KEY,
      publication_name TEXT NOT NULL,
      platform TEXT NOT NULL DEFAULT 'Medium',
      submission_notes TEXT,
      submission_url TEXT,
      is_active INTEGER NOT NULL DEFAULT 1,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")

  publication_columns <- list(
    publication_name = "TEXT NOT NULL DEFAULT ''", platform = "TEXT NOT NULL DEFAULT 'Medium'",
    submission_notes = "TEXT", submission_url = "TEXT", is_active = "INTEGER NOT NULL DEFAULT 1",
    created_at = "TEXT NOT NULL DEFAULT ''", updated_at = "TEXT NOT NULL DEFAULT ''"
  )
  for (column_name in names(publication_columns)) db_add_column_if_missing(con, "article_lab_publications", column_name, publication_columns[[column_name]])

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS article_lab_publish_settings (
      publish_settings_id TEXT PRIMARY KEY,
      full_text_draft_id TEXT NOT NULL,
      thumbnail_id TEXT,
      subtitle_id TEXT,
      candidate_id TEXT,
      batch_id TEXT,
      medium_tags_json TEXT,
      publishing_target TEXT,
      publication_id TEXT,
      publication_name_snapshot TEXT,
      monetization TEXT,
      canonical_url TEXT,
      featured_image_alt_text TEXT,
      image_credit_source TEXT,
      published_url TEXT,
      publish_status TEXT NOT NULL DEFAULT 'ready_for_review_publish',
      notes TEXT,
      submitted_at TEXT,
      published_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY(full_text_draft_id) REFERENCES article_lab_full_text_drafts(full_text_draft_id),
      FOREIGN KEY(publication_id) REFERENCES article_lab_publications(publication_id)
    )
  ")

  publish_settings_columns <- list(
    full_text_draft_id = "TEXT NOT NULL DEFAULT ''", thumbnail_id = "TEXT", subtitle_id = "TEXT", candidate_id = "TEXT", batch_id = "TEXT",
    medium_tags_json = "TEXT", publishing_target = "TEXT", publication_id = "TEXT", publication_name_snapshot = "TEXT", monetization = "TEXT",
    canonical_url = "TEXT", featured_image_alt_text = "TEXT", image_credit_source = "TEXT", published_url = "TEXT",
    publish_status = "TEXT NOT NULL DEFAULT 'ready_for_review_publish'", notes = "TEXT", submitted_at = "TEXT", published_at = "TEXT",
    created_at = "TEXT NOT NULL DEFAULT ''", updated_at = "TEXT NOT NULL DEFAULT ''"
  )
  for (column_name in names(publish_settings_columns)) db_add_column_if_missing(con, "article_lab_publish_settings", column_name, publish_settings_columns[[column_name]])

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_batches_created_at
    ON article_lab_title_batches (created_at, batch_id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_candidates_batch
    ON article_lab_title_candidates (batch_id, created_at, candidate_id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_candidates_status
    ON article_lab_title_candidates (status, ready_for_human_rating, archived, promoted)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_api_scores_batch
    ON article_lab_title_api_scores (batch_id, scored_at, candidate_id)
  ")

  dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_article_lab_title_api_scores_cache
    ON article_lab_title_api_scores (candidate_id, model, prompt_version, scope)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_title_api_scores_prompt
    ON article_lab_title_api_scores (prompt_version, model, scope)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_subtitle_candidates_batch
    ON article_lab_subtitle_candidates (batch_id, candidate_id, created_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_subtitle_candidates_status
    ON article_lab_subtitle_candidates (status, candidate_id, created_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_thumbnail_candidates_batch
    ON article_lab_thumbnail_candidates (batch_id, subtitle_id, created_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_thumbnail_candidates_status
    ON article_lab_thumbnail_candidates (status, subtitle_id, created_at)
  ")

  dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_article_lab_thumbnail_candidates_one_approved_per_package
    ON article_lab_thumbnail_candidates (subtitle_id)
    WHERE status = 'approved'
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_outlines_batch
    ON article_lab_outlines (batch_id, thumbnail_id, updated_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_outlines_status
    ON article_lab_outlines (status, candidate_id, updated_at)
  ")

  dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_article_lab_outlines_one_active_per_thumbnail
    ON article_lab_outlines (thumbnail_id)
    WHERE status IN ('draft', 'approved')
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_full_text_drafts_outline
    ON article_lab_full_text_drafts (outline_id, updated_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_full_text_drafts_status
    ON article_lab_full_text_drafts (status, is_approved, candidate_id, updated_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_full_text_revisions_draft
    ON article_lab_full_text_draft_revisions (full_text_draft_id, created_at)
  ")

  dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_article_lab_publications_name_platform
    ON article_lab_publications (publication_name, platform)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_publications_active
    ON article_lab_publications (is_active, platform, publication_name)
  ")

  dbExecute(con, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_article_lab_publish_settings_draft
    ON article_lab_publish_settings (full_text_draft_id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_publish_settings_status
    ON article_lab_publish_settings (publish_status, updated_at)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_article_lab_publish_settings_publication
    ON article_lab_publish_settings (publication_id, updated_at)
  ")

  dbExecute(con, "
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

  dbExecute(con, "
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

  dbExecute(con, "
    UPDATE article_lab_title_candidates
    SET status = 'approved_for_subtitle',
        promoted = 1,
        ready_for_human_rating = 0,
        archived = 0
    WHERE promoted = 1
       OR status IN ('promoted', 'approved', 'approved_for_subtitle')
  ")

  dbExecute(con, "
    UPDATE article_lab_title_candidates
    SET status = 'ready_for_thumbnail',
        promoted = 0,
        ready_for_human_rating = 0,
        archived = 0
    WHERE candidate_id IN (
      SELECT DISTINCT candidate_id
      FROM article_lab_subtitle_candidates
      WHERE status = 'approved'
    )
  ")

  dbExecute(con, "
    UPDATE article_lab_title_candidates
    SET status = 'ready_for_outline',
        promoted = 0,
        ready_for_human_rating = 0,
        archived = 0
    WHERE candidate_id IN (
      SELECT DISTINCT candidate_id
      FROM article_lab_thumbnail_candidates
      WHERE status = 'approved'
    )
  ")

  dbExecute(con, "
    UPDATE article_lab_title_candidates
    SET status = 'archived',
        ready_for_human_rating = 0
    WHERE archived = 1
  ")
}
