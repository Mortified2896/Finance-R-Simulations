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
    paste0(tools::file_path_sans_ext(basename(database_path)), "_analysis_v2_backup_", timestamp, ".sqlite")
  )
  if (!file.copy(database_path, backup_path, overwrite = FALSE)) {
    stop("Could not create backup at: ", backup_path, call. = FALSE)
  }
  backup_path
}

db_execute <- function(connection, sql) {
  invisible(dbExecute(connection, sql))
}

table_columns <- function(connection, table_name) {
  if (!dbExistsTable(connection, table_name)) {
    return(character())
  }
  dbGetQuery(
    connection,
    paste0("PRAGMA table_info(", dbQuoteIdentifier(connection, table_name), ")")
  )$name
}

ensure_column <- function(connection, table_name, column_name, column_sql) {
  if (!(column_name %in% table_columns(connection, table_name))) {
    db_execute(
      connection,
      paste0(
        "ALTER TABLE ",
        dbQuoteIdentifier(connection, table_name),
        " ADD COLUMN ",
        column_name,
        " ",
        column_sql
      )
    )
  }
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
database_path <- args$db

message("Medium Analysis V2 Schema Setup")
message("================================")
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

required_tables <- c("medium_articles", "medium_tag_page_observations")
missing_tables <- required_tables[!vapply(required_tables, dbExistsTable, logical(1), conn = connection)]
if (length(missing_tables) > 0) {
  stop("Missing required table(s): ", paste(missing_tables, collapse = ", "), call. = FALSE)
}

invisible(dbWithTransaction(connection, {
  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS medium_title_api_scores (
      id INTEGER PRIMARY KEY,
      canonical_article_key TEXT NOT NULL,
      article_id INTEGER,
      medium_post_id TEXT,
      prompt_version TEXT NOT NULL,
      model TEXT NOT NULL,
      scored_at TEXT NOT NULL,
      title_hash TEXT NOT NULL,
      subtitle_hash TEXT,
      input_title TEXT NOT NULL,
      input_subtitle TEXT,
      raw_json TEXT NOT NULL,
      clarity INTEGER,
      curiosity INTEGER,
      specificity INTEGER,
      beginner_appeal INTEGER,
      credibility INTEGER,
      emotional_pull INTEGER,
      promise_strength INTEGER,
      click_potential INTEGER,
      medium_clap_potential INTEGER,
      medium_comment_potential INTEGER,
      overall_article_potential INTEGER,
      trust_risk INTEGER,
      predicted_success_bucket TEXT,
      short_reason TEXT,
      error TEXT
    )
  ")

  db_execute(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_title_api_scores_cache
      ON medium_title_api_scores (
        canonical_article_key,
        title_hash,
        subtitle_hash,
        prompt_version,
        model
      )
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_title_api_scores_prompt_model
      ON medium_title_api_scores(prompt_version, model)
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS medium_title_human_ratings (
      id INTEGER PRIMARY KEY,
      canonical_article_key TEXT NOT NULL,
      article_id INTEGER,
      medium_post_id TEXT,
      rater TEXT NOT NULL,
      rating_version TEXT NOT NULL,
      rated_at TEXT NOT NULL,
      shown_title TEXT NOT NULL,
      shown_subtitle TEXT,
      title_hash TEXT NOT NULL,
      subtitle_hash TEXT,
      general_rating INTEGER,
      click_rating INTEGER,
      quality_rating INTEGER,
      would_study INTEGER,
      note TEXT,
      skipped INTEGER DEFAULT 0
    )
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_title_human_ratings_article
      ON medium_title_human_ratings(canonical_article_key)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_title_human_ratings_rater_version_time
      ON medium_title_human_ratings(rater, rating_version, rated_at)
  ")

  db_execute(connection, "
    CREATE TABLE IF NOT EXISTS medium_article_image_assets (
      id INTEGER PRIMARY KEY,
      canonical_article_key TEXT,
      article_id INTEGER,
      medium_post_id TEXT,
      image_url TEXT NOT NULL,
      local_path TEXT,
      image_type TEXT,
      source TEXT,
      download_status TEXT,
      downloaded_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      error TEXT
    )
  ")

  db_execute(connection, "
    CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_article_image_assets_key_url
      ON medium_article_image_assets(canonical_article_key, image_url)
  ")

  db_execute(connection, "
    CREATE INDEX IF NOT EXISTS idx_medium_article_image_assets_post_url
      ON medium_article_image_assets(medium_post_id, image_url)
  ")

  ensure_column(connection, "medium_title_api_scores", "medium_clap_potential", "INTEGER")
  ensure_column(connection, "medium_title_api_scores", "medium_comment_potential", "INTEGER")
  ensure_column(connection, "medium_title_api_scores", "overall_article_potential", "INTEGER")
  ensure_column(connection, "medium_title_human_ratings", "general_rating", "INTEGER")

  db_execute(connection, "DROP VIEW IF EXISTS v_medium_title_prediction_dataset_v2")
  db_execute(connection, "DROP VIEW IF EXISTS v_medium_canonical_articles")

  db_execute(connection, "
    CREATE VIEW v_medium_canonical_articles AS
    WITH article_base AS (
      SELECT
        a.*,
        CASE
          WHEN NULLIF(TRIM(a.medium_post_id), '') IS NOT NULL
            THEN 'post:' || TRIM(a.medium_post_id)
          ELSE 'url:' || LOWER(RTRIM(COALESCE(NULLIF(TRIM(a.canonical_url), ''), NULLIF(TRIM(a.url), '')), '/'))
        END AS canonical_article_key,
        LOWER(RTRIM(COALESCE(NULLIF(TRIM(a.canonical_url), ''), NULLIF(TRIM(a.url), '')), '/')) AS normalized_article_url
      FROM medium_articles a
    ),
    observation_summary AS (
      SELECT
        CASE
          WHEN NULLIF(TRIM(o.medium_post_id), '') IS NOT NULL
            THEN 'post:' || TRIM(o.medium_post_id)
          WHEN o.article_id IS NOT NULL AND NULLIF(TRIM(a.medium_post_id), '') IS NOT NULL
            THEN 'post:' || TRIM(a.medium_post_id)
          ELSE 'url:' || LOWER(RTRIM(COALESCE(NULLIF(TRIM(o.article_url_normalized), ''), NULLIF(TRIM(a.canonical_url), ''), NULLIF(TRIM(a.url), '')), '/'))
        END AS canonical_article_key,
        COUNT(*) AS observation_count,
        MIN(o.observed_at) AS first_observed_at,
        MAX(o.observed_at) AS latest_observed_at,
        MIN(o.page_position) AS best_page_position
      FROM medium_tag_page_observations o
      LEFT JOIN medium_articles a ON a.id = o.article_id
      GROUP BY 1
    ),
    ranked_articles AS (
      SELECT
        b.*,
        os.observation_count,
        os.first_observed_at,
        os.latest_observed_at,
        os.best_page_position,
        COUNT(*) OVER (PARTITION BY b.canonical_article_key) AS duplicate_row_count,
        ROW_NUMBER() OVER (
          PARTITION BY b.canonical_article_key
          ORDER BY
            CASE WHEN os.observation_count IS NOT NULL THEN 1 ELSE 0 END DESC,
            CASE WHEN NULLIF(TRIM(b.title), '') IS NOT NULL THEN 1 ELSE 0 END DESC,
            CASE WHEN NULLIF(TRIM(b.subtitle), '') IS NOT NULL THEN 1 ELSE 0 END DESC,
            CASE WHEN NULLIF(TRIM(b.canonical_url), '') IS NOT NULL THEN 1 ELSE 0 END DESC,
            COALESCE(os.latest_observed_at, b.last_seen_at, b.fetched_at, b.updated_at, b.published_at, '') DESC,
            b.id DESC
        ) AS canonical_rank
      FROM article_base b
      LEFT JOIN observation_summary os ON os.canonical_article_key = b.canonical_article_key
      WHERE b.canonical_article_key IS NOT NULL
    )
    SELECT
      canonical_article_key,
      id AS canonical_article_id,
      NULLIF(TRIM(medium_post_id), '') AS medium_post_id,
      COALESCE(NULLIF(TRIM(canonical_url), ''), NULLIF(TRIM(url), '')) AS canonical_url,
      NULLIF(TRIM(title), '') AS title,
      NULLIF(TRIM(subtitle), '') AS subtitle,
      NULLIF(TRIM(author), '') AS author,
      NULLIF(TRIM(publication), '') AS publication,
      fetched_at,
      COALESCE(latest_observed_at, last_seen_at) AS last_seen_at,
      duplicate_row_count
    FROM ranked_articles
    WHERE canonical_rank = 1
  ")

  db_execute(connection, "
    CREATE VIEW v_medium_title_prediction_dataset_v2 AS
    WITH observations_keyed AS (
      SELECT
        o.*,
        CASE
          WHEN NULLIF(TRIM(o.medium_post_id), '') IS NOT NULL
            THEN 'post:' || TRIM(o.medium_post_id)
          WHEN o.article_id IS NOT NULL AND NULLIF(TRIM(a.medium_post_id), '') IS NOT NULL
            THEN 'post:' || TRIM(a.medium_post_id)
          ELSE 'url:' || LOWER(RTRIM(COALESCE(NULLIF(TRIM(o.article_url_normalized), ''), NULLIF(TRIM(a.canonical_url), ''), NULLIF(TRIM(a.url), '')), '/'))
        END AS canonical_article_key
      FROM medium_tag_page_observations o
      LEFT JOIN medium_articles a ON a.id = o.article_id
    ),
    observation_summary AS (
      SELECT
        canonical_article_key,
        MIN(observed_at) AS first_observed_at,
        MAX(observed_at) AS last_observed_at,
        COUNT(*) AS times_seen,
        COUNT(DISTINCT tag_slug) AS number_of_tags_seen,
        GROUP_CONCAT(DISTINCT tag_slug) AS tags_seen,
        MIN(page_position) AS best_page_position
      FROM observations_keyed
      WHERE canonical_article_key IS NOT NULL
      GROUP BY canonical_article_key
    ),
    latest_observations AS (
      SELECT *
      FROM (
        SELECT
          ok.*,
          ROW_NUMBER() OVER (
            PARTITION BY ok.canonical_article_key
            ORDER BY COALESCE(ok.observed_at, '') DESC, ok.id DESC
          ) AS latest_rank
        FROM observations_keyed ok
        WHERE ok.canonical_article_key IS NOT NULL
      )
      WHERE latest_rank = 1
    ),
    dataset_base AS (
      SELECT
        lo.canonical_article_key,
        c.canonical_article_id AS article_id,
        COALESCE(NULLIF(TRIM(lo.medium_post_id), ''), c.medium_post_id) AS medium_post_id,
        COALESCE(NULLIF(TRIM(lo.article_url_normalized), ''), c.canonical_url) AS url,
        COALESCE(NULLIF(TRIM(lo.title), ''), c.title) AS title,
        COALESCE(NULLIF(TRIM(lo.subtitle), ''), c.subtitle) AS subtitle,
        COALESCE(NULLIF(TRIM(lo.author_name), ''), c.author) AS author,
        NULLIF(TRIM(lo.publication_name), '') AS publication_name,
        NULLIF(TRIM(lo.publication_status), '') AS publication_status,
        lo.tag_slug AS source_tag,
        os.tags_seen,
        os.first_observed_at,
        os.last_observed_at,
        os.times_seen,
        os.number_of_tags_seen,
        os.best_page_position,
        lo.claps,
        lo.responses,
        LOG(1.0 + COALESCE(lo.claps, 0)) + 2.0 * LOG(1.0 + COALESCE(lo.responses, 0)) AS success_score,
        LOG(1.0 + COALESCE(lo.claps, 0)) AS log_claps,
        LOG(1.0 + COALESCE(lo.responses, 0)) AS log_responses,
        lo.published_date_inferred,
        CASE
          WHEN lo.published_date_inferred IS NOT NULL AND os.last_observed_at IS NOT NULL
            THEN CAST(julianday(date(os.last_observed_at)) - julianday(date(lo.published_date_inferred)) AS INTEGER)
          ELSE NULL
        END AS age_days_at_observation,
        COALESCE(NULLIF(TRIM(lo.thumbnail_url), ''), NULLIF(TRIM(a.image_url_manual), ''), NULLIF(TRIM(a.image_url), '')) AS thumbnail_url,
        CASE
          WHEN COALESCE(NULLIF(TRIM(lo.thumbnail_url), ''), NULLIF(TRIM(a.image_url_manual), ''), NULLIF(TRIM(a.image_url), '')) IS NOT NULL
            THEN 1 ELSE 0
        END AS has_thumbnail_url
      FROM latest_observations lo
      JOIN observation_summary os ON os.canonical_article_key = lo.canonical_article_key
      LEFT JOIN v_medium_canonical_articles c ON c.canonical_article_key = lo.canonical_article_key
      LEFT JOIN medium_articles a ON a.id = c.canonical_article_id
    ),
    labeled AS (
      SELECT
        db.*,
        CASE
          WHEN age_days_at_observation IS NULL THEN 'unknown'
          WHEN age_days_at_observation < 0 THEN 'unknown'
          WHEN age_days_at_observation <= 7 THEN '0-7d'
          WHEN age_days_at_observation <= 30 THEN '8-30d'
          WHEN age_days_at_observation <= 90 THEN '31-90d'
          WHEN age_days_at_observation <= 365 THEN '91-365d'
          ELSE '366d+'
        END AS age_bucket,
        CUME_DIST() OVER (ORDER BY success_score DESC) AS success_cume_desc,
        CUME_DIST() OVER (
          PARTITION BY
            CASE
              WHEN age_days_at_observation IS NULL THEN 'unknown'
              WHEN age_days_at_observation < 0 THEN 'unknown'
              WHEN age_days_at_observation <= 7 THEN '0-7d'
              WHEN age_days_at_observation <= 30 THEN '8-30d'
              WHEN age_days_at_observation <= 90 THEN '31-90d'
              WHEN age_days_at_observation <= 365 THEN '91-365d'
              ELSE '366d+'
            END
          ORDER BY success_score DESC
        ) AS success_rank_within_age_bucket
      FROM dataset_base db
    )
    SELECT
      *,
      CASE WHEN success_cume_desc <= 0.20 THEN 1 ELSE 0 END AS top_20_percent,
      CASE WHEN success_cume_desc <= 0.10 THEN 1 ELSE 0 END AS top_10_percent,
      CASE WHEN success_cume_desc <= 0.05 THEN 1 ELSE 0 END AS top_5_percent,
      CASE WHEN COALESCE(claps, 0) >= 50 THEN 1 ELSE 0 END AS over_50_claps,
      CASE WHEN COALESCE(claps, 0) >= 100 THEN 1 ELSE 0 END AS over_100_claps,
      CASE WHEN COALESCE(claps, 0) >= 200 THEN 1 ELSE 0 END AS over_200_claps,
      CASE WHEN success_rank_within_age_bucket <= 0.10 THEN 1 ELSE 0 END AS top_10_percent_within_age_bucket,
      CASE WHEN success_rank_within_age_bucket <= 0.20 THEN 1 ELSE 0 END AS top_20_percent_within_age_bucket
    FROM labeled
  ")
}))

message("Created/updated:")
message("- v_medium_canonical_articles")
message("- v_medium_title_prediction_dataset_v2")
message("- medium_title_api_scores")
message("- medium_title_human_ratings")
message("- medium_article_image_assets")
message("Schema setup complete.")
