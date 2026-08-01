connect_db <- function() {
  con <- dbConnect(SQLite(), db_path)
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

db_add_column_if_missing <- function(con, table, column, definition) {
  columns <- dbGetQuery(con, sprintf("PRAGMA table_info(%s)", dbQuoteIdentifier(con, table)))
  if (!(column %in% columns$name)) {
    dbExecute(con, sprintf(
      "ALTER TABLE %s ADD COLUMN %s %s",
      dbQuoteIdentifier(con, table),
      dbQuoteIdentifier(con, column),
      definition
    ))
  }
}
article_lab_load_generation_preference <- function(con, workflow_key, default_model, default_reasoning, default_mode = "standard") {
  rows <- dbGetQuery(con, "SELECT * FROM article_lab_generation_preferences WHERE workflow_key = ? LIMIT 1", params = list(workflow_key))
  if (nrow(rows) == 0) return(list(model = default_model, reasoning_effort = default_reasoning, reasoning_mode = default_mode, last_supported_reasoning_effort = default_reasoning, last_supported_reasoning_mode = default_mode))
  list(
    model = article_lab_input_string(rows$model[[1]]) %||% default_model,
    reasoning_effort = article_lab_input_string(rows$reasoning_effort[[1]]) %||% default_reasoning,
    reasoning_mode = article_lab_input_string(rows$reasoning_mode[[1]]) %||% default_mode,
    last_supported_reasoning_effort = article_lab_input_string(rows$last_supported_reasoning_effort[[1]]) %||% default_reasoning,
    last_supported_reasoning_mode = article_lab_input_string(rows$last_supported_reasoning_mode[[1]]) %||% default_mode
  )
}

article_lab_save_generation_preference <- function(con, workflow_key, model, reasoning_effort = NULL, reasoning_mode = "standard", last_supported_reasoning_effort = NULL, last_supported_reasoning_mode = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  scalar_or_na <- function(value) {
    value <- article_lab_input_string(value)
    if (length(value) == 0 || is.na(value)) NA_character_ else value
  }
  dbExecute(con, "
    INSERT INTO article_lab_generation_preferences
      (workflow_key, model, reasoning_effort, reasoning_mode, last_supported_reasoning_effort, last_supported_reasoning_mode, created_at, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(workflow_key) DO UPDATE SET
      model = excluded.model,
      reasoning_effort = excluded.reasoning_effort,
      reasoning_mode = excluded.reasoning_mode,
      last_supported_reasoning_effort = COALESCE(excluded.last_supported_reasoning_effort, article_lab_generation_preferences.last_supported_reasoning_effort),
      last_supported_reasoning_mode = COALESCE(excluded.last_supported_reasoning_mode, article_lab_generation_preferences.last_supported_reasoning_mode),
      updated_at = excluded.updated_at",
    params = list(workflow_key, model, scalar_or_na(reasoning_effort), reasoning_mode, scalar_or_na(last_supported_reasoning_effort), scalar_or_na(last_supported_reasoning_mode), timestamp, timestamp)
  )
  invisible(TRUE)
}
