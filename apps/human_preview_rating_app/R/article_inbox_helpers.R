article_inbox_candidate_id <- function() {
  paste0("ac_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(999999L, 1))
}

article_inbox_redirect_section <- function(section) {
  if (identical(section, "idea_inbox")) "research_inbox" else section
}

article_inbox_clean_optional <- function(value) {
  value <- if (length(value) == 0 || is.null(value) || is.na(value[[1]])) "" else trimws(as.character(value[[1]]))
  if (nzchar(value)) value else NA_character_
}

create_quick_idea_candidate <- function(con, working_title, core_idea = NULL, notes = NULL, timestamp = now_utc()) {
  title <- article_inbox_clean_optional(working_title)
  if (is.na(title)) stop("Working title is required.", call. = FALSE)
  candidate_id <- article_inbox_candidate_id()
  dbExecute(con, "
    INSERT INTO article_candidates
      (candidate_id, working_title, core_idea, notes, origin_type, status, created_at, updated_at)
    VALUES (?, ?, ?, ?, 'quick_idea', 'captured', ?, ?)
  ", params = list(candidate_id, title, article_inbox_clean_optional(core_idea), article_inbox_clean_optional(notes), timestamp, timestamp))
  candidate_id
}

promote_research_angle_candidate <- function(con, research_angle_id, timestamp = now_utc()) {
  angle_id <- suppressWarnings(as.integer(research_angle_id))
  if (is.na(angle_id)) stop("A valid research angle is required.", call. = FALSE)
  existing <- dbGetQuery(con, "SELECT candidate_id FROM article_candidates WHERE research_angle_id = ? LIMIT 1", params = list(angle_id))
  if (nrow(existing) > 0) return(existing$candidate_id[[1]])
  angle <- dbGetQuery(con, "
    SELECT a.*, s.source_title,
      (SELECT ss.summary_text FROM research_source_summaries ss WHERE ss.research_source_id = a.research_source_id ORDER BY CASE ss.status WHEN 'confirmed' THEN 0 ELSE 1 END, ss.updated_at DESC LIMIT 1) AS source_summary
    FROM research_article_angles a
    LEFT JOIN research_sources s ON s.research_source_id = a.research_source_id
    WHERE a.research_angle_id = ? LIMIT 1
  ", params = list(angle_id))
  if (nrow(angle) == 0) stop("The selected research angle no longer exists.", call. = FALSE)
  candidate_id <- paste0("ac_ra_", angle_id)
  context <- c(article_inbox_clean_optional(angle$notes[[1]]), article_inbox_clean_optional(angle$source_summary[[1]]))
  context <- paste(context[!is.na(context)], collapse = "\n\nSource summary:\n")
  if (!nzchar(context)) context <- NA_character_
  dbExecute(con, "
    INSERT OR IGNORE INTO article_candidates
      (candidate_id, working_title, core_idea, notes, origin_type, research_source_id, research_angle_id, status, created_at, updated_at)
    VALUES (?, ?, ?, ?, 'research_angle', ?, ?, 'captured', ?, ?)
  ", params = list(candidate_id, angle$angle_title[[1]], angle$main_idea[[1]], context, angle$research_source_id[[1]], angle_id, timestamp, timestamp))
  dbExecute(con, "UPDATE research_article_angles SET updated_at = ?, status = CASE WHEN status = 'idea' THEN 'candidate_added' ELSE status END WHERE research_angle_id = ?", params = list(timestamp, angle_id))
  result <- dbGetQuery(con, "SELECT candidate_id FROM article_candidates WHERE research_angle_id = ? LIMIT 1", params = list(angle_id))
  result$candidate_id[[1]]
}

load_article_candidates <- function(con, origin = "__all__", include_archived = FALSE) {
  where <- if (isTRUE(include_archived)) character() else "c.status <> 'archived'"
  params <- list()
  if (!is.null(origin) && length(origin) == 1 && !is.na(origin) && origin %in% article_candidate_origins) {
    where <- c(where, "c.origin_type = ?")
    params <- list(origin)
  }
  where_sql <- if (length(where) > 0) paste("WHERE", paste(where, collapse = " AND ")) else ""
  query <- paste0("
    SELECT c.*, s.source_title,
      p.article_project_id,
      CASE WHEN p.article_project_id IS NULL THEN c.status ELSE 'in_article_evidence' END AS effective_status
    FROM article_candidates c
    LEFT JOIN research_sources s ON s.research_source_id = c.research_source_id
    LEFT JOIN article_projects p ON p.article_candidate_id = c.candidate_id
    ", where_sql, " ORDER BY c.updated_at DESC, c.created_at DESC")
  if (length(params) > 0) dbGetQuery(con, query, params = params) else dbGetQuery(con, query)
}

load_article_candidate <- function(con, candidate_id) {
  id <- article_inbox_clean_optional(candidate_id)
  if (is.na(id)) return(data.frame())
  rows <- load_article_candidates(con, include_archived = TRUE)
  rows[rows$candidate_id == id, , drop = FALSE]
}

update_article_candidate <- function(con, candidate_id, working_title, core_idea, notes, status, timestamp = now_utc()) {
  id <- article_inbox_clean_optional(candidate_id)
  title <- article_inbox_clean_optional(working_title)
  if (is.na(id) || is.na(title)) stop("Candidate and working title are required.", call. = FALSE)
  if (!(status %in% setdiff(article_candidate_statuses, c("archived", "in_article_evidence")))) stop("Choose a valid editable candidate status.", call. = FALSE)
  project <- dbGetQuery(con, "SELECT article_project_id FROM article_projects WHERE article_candidate_id = ?", params = list(id))
  stored_status <- if (nrow(project) > 0) "in_article_evidence" else status
  changed <- dbExecute(con, "UPDATE article_candidates SET working_title = ?, core_idea = ?, notes = ?, status = ?, updated_at = ? WHERE candidate_id = ?", params = list(title, article_inbox_clean_optional(core_idea), article_inbox_clean_optional(notes), stored_status, timestamp, id))
  if (changed != 1L) stop("The selected article candidate no longer exists.", call. = FALSE)
  invisible(id)
}

archive_article_candidate <- function(con, candidate_id, timestamp = now_utc()) {
  id <- article_inbox_clean_optional(candidate_id)
  changed <- dbExecute(con, "
    UPDATE article_candidates
    SET status_before_archive = CASE WHEN status = 'archived' THEN status_before_archive ELSE status END,
        status = 'archived', archived_at = ?, updated_at = ?
    WHERE candidate_id = ?
  ", params = list(timestamp, timestamp, id))
  if (changed != 1L) stop("The selected article candidate no longer exists.", call. = FALSE)
  invisible(id)
}

restore_article_candidate <- function(con, candidate_id, timestamp = now_utc()) {
  id <- article_inbox_clean_optional(candidate_id)
  changed <- dbExecute(con, "
    UPDATE article_candidates
    SET status = CASE
          WHEN EXISTS (SELECT 1 FROM article_projects p WHERE p.article_candidate_id = article_candidates.candidate_id) THEN 'in_article_evidence'
          WHEN status_before_archive IN ('captured', 'refining', 'ready_for_evidence') THEN status_before_archive
          ELSE 'captured' END,
        status_before_archive = NULL, archived_at = NULL, updated_at = ?
    WHERE candidate_id = ? AND status = 'archived'
  ", params = list(timestamp, id))
  if (changed != 1L) stop("The selected archived candidate could not be restored.", call. = FALSE)
  invisible(id)
}

develop_article_candidate <- function(con, candidate_id, timestamp = now_utc()) {
  id <- article_inbox_clean_optional(candidate_id)
  existing <- dbGetQuery(con, "SELECT article_project_id FROM article_projects WHERE article_candidate_id = ? LIMIT 1", params = list(id))
  if (nrow(existing) > 0) return(existing$article_project_id[[1]])
  candidate <- dbGetQuery(con, "SELECT * FROM article_candidates WHERE candidate_id = ? LIMIT 1", params = list(id))
  if (nrow(candidate) == 0) stop("The selected article candidate no longer exists.", call. = FALSE)
  if (identical(candidate$status[[1]], "archived")) stop("Restore the candidate before developing it.", call. = FALSE)
  summary <- NA_character_
  source_title <- NA_character_
  if (!is.na(candidate$research_source_id[[1]])) {
    source <- dbGetQuery(con, "SELECT source_title FROM research_sources WHERE research_source_id = ?", params = list(candidate$research_source_id[[1]]))
    if (nrow(source) > 0) source_title <- source$source_title[[1]]
    summary_row <- dbGetQuery(con, "SELECT summary_text FROM research_source_summaries WHERE research_source_id = ? ORDER BY CASE status WHEN 'confirmed' THEN 0 ELSE 1 END, updated_at DESC LIMIT 1", params = list(candidate$research_source_id[[1]]))
    if (nrow(summary_row) > 0) summary <- summary_row$summary_text[[1]]
  }
  project_id <- paste0("ap_", gsub("[^A-Za-z0-9]+", "_", id))
  provenance <- jsonlite::toJSON(list(candidate_id = id, origin_type = candidate$origin_type[[1]], research_source_id = candidate$research_source_id[[1]], research_angle_id = candidate$research_angle_id[[1]]), auto_unbox = TRUE, na = "null")
  dbWithTransaction(con, {
    dbExecute(con, "
      INSERT OR IGNORE INTO article_projects
        (article_project_id, article_candidate_id, working_title, core_idea, notes, origin_type, research_source_id, research_angle_id, source_summary_snapshot, provenance_json, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(project_id, id, candidate$working_title[[1]], candidate$core_idea[[1]], candidate$notes[[1]], candidate$origin_type[[1]], candidate$research_source_id[[1]], candidate$research_angle_id[[1]], summary, provenance, timestamp, timestamp))
    if (!is.na(candidate$research_source_id[[1]])) {
      dbExecute(con, "
        INSERT OR IGNORE INTO article_project_evidence_sources
          (article_project_id, research_source_id, link_type, source_title_snapshot, source_summary_snapshot, created_at)
        VALUES (?, ?, 'origin', ?, ?, ?)
      ", params = list(project_id, candidate$research_source_id[[1]], source_title, summary, timestamp))
    }
    dbExecute(con, "UPDATE article_candidates SET status = 'in_article_evidence', updated_at = ? WHERE candidate_id = ?", params = list(timestamp, id))
  })
  result <- dbGetQuery(con, "SELECT article_project_id FROM article_projects WHERE article_candidate_id = ? LIMIT 1", params = list(id))
  result$article_project_id[[1]]
}

load_article_project <- function(con, article_project_id = NULL, candidate_id = NULL) {
  project_id <- article_inbox_clean_optional(article_project_id)
  candidate <- article_inbox_clean_optional(candidate_id)
  if (!is.na(project_id)) return(dbGetQuery(con, "SELECT * FROM article_projects WHERE article_project_id = ? LIMIT 1", params = list(project_id)))
  if (!is.na(candidate)) return(dbGetQuery(con, "SELECT * FROM article_projects WHERE article_candidate_id = ? LIMIT 1", params = list(candidate)))
  data.frame()
}
