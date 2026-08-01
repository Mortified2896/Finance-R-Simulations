load_article_lab_subtitle_targets <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      COALESCE(s.generated_n, 0) AS generated_subtitle_n,
      COALESCE(s.approved_n, 0) AS approved_subtitle_n,
      COALESCE(s.rejected_n, 0) AS rejected_subtitle_n
    FROM article_lab_title_candidates c
    LEFT JOIN (
      SELECT
        candidate_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_subtitle_candidates
      GROUP BY candidate_id
    ) s
      ON s.candidate_id = c.candidate_id
    WHERE c.archived = 0
    ORDER BY c.created_at DESC, c.candidate_id DESC
    " else "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      COALESCE(s.generated_n, 0) AS generated_subtitle_n,
      COALESCE(s.approved_n, 0) AS approved_subtitle_n,
      COALESCE(s.rejected_n, 0) AS rejected_subtitle_n
    FROM article_lab_title_candidates c
    LEFT JOIN (
      SELECT
        candidate_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_subtitle_candidates
      GROUP BY candidate_id
    ) s
      ON s.candidate_id = c.candidate_id
    WHERE c.batch_id = ?
      AND c.archived = 0
    ORDER BY c.created_at DESC, c.candidate_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

load_article_lab_subtitle_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_subtitle_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at,
      s.subtitle,
      s.status AS subtitle_status,
      s.notes,
      s.model,
      s.generation_mode,
      s.approved_at,
      s.rejected_at,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    " else "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at,
      s.subtitle,
      s.status AS subtitle_status,
      s.notes,
      s.model,
      s.generation_mode,
      s.approved_at,
      s.rejected_at,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    WHERE s.batch_id = ?
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

load_article_lab_ready_for_thumbnail_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_subtitle_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.title,
      c.status,
      c.notes,
      GROUP_CONCAT(s.subtitle, '\n') AS approved_subtitles,
      COUNT(*) AS approved_subtitle_n
    FROM article_lab_title_candidates c
    INNER JOIN article_lab_subtitle_candidates s
      ON s.candidate_id = c.candidate_id
     AND s.status = 'approved'
    WHERE c.archived = 0
      AND c.status = 'ready_for_thumbnail'
    GROUP BY c.candidate_id, c.batch_id, c.title, c.status, c.notes
    ORDER BY c.batch_id DESC, c.candidate_id DESC
    " else "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.title,
      c.status,
      c.notes,
      GROUP_CONCAT(s.subtitle, '\n') AS approved_subtitles,
      COUNT(*) AS approved_subtitle_n
    FROM article_lab_title_candidates c
    INNER JOIN article_lab_subtitle_candidates s
      ON s.candidate_id = c.candidate_id
     AND s.status = 'approved'
    WHERE c.archived = 0
      AND c.status = 'ready_for_thumbnail'
      AND c.batch_id = ?
    GROUP BY c.candidate_id, c.batch_id, c.title, c.status, c.notes
    ORDER BY c.batch_id DESC, c.candidate_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

load_article_lab_thumbnail_packages <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_subtitle_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at AS subtitle_created_at,
      s.subtitle,
      s.notes,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      COALESCE(t.generated_n, 0) AS generated_thumbnail_n,
      COALESCE(t.approved_n, 0) AS approved_thumbnail_n,
      COALESCE(t.rejected_n, 0) AS rejected_thumbnail_n
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    LEFT JOIN (
      SELECT
        subtitle_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_thumbnail_candidates
      GROUP BY subtitle_id
    ) t
      ON t.subtitle_id = s.subtitle_id
    WHERE s.status = 'approved'
      AND c.archived = 0
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    " else "
    SELECT
      s.subtitle_id,
      s.candidate_id,
      s.batch_id,
      s.created_at AS subtitle_created_at,
      s.subtitle,
      s.notes,
      c.title,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      COALESCE(t.generated_n, 0) AS generated_thumbnail_n,
      COALESCE(t.approved_n, 0) AS approved_thumbnail_n,
      COALESCE(t.rejected_n, 0) AS rejected_thumbnail_n
    FROM article_lab_subtitle_candidates s
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = s.candidate_id
    LEFT JOIN (
      SELECT
        subtitle_id,
        COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated_n,
        COALESCE(SUM(CASE WHEN status = 'approved' THEN 1 ELSE 0 END), 0) AS approved_n,
        COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected_n
      FROM article_lab_thumbnail_candidates
      GROUP BY subtitle_id
    ) t
      ON t.subtitle_id = s.subtitle_id
    WHERE s.status = 'approved'
      AND c.archived = 0
      AND s.batch_id = ?
    ORDER BY s.created_at DESC, s.subtitle_id DESC
    "
  rows <- if (all_batches) dbGetQuery(con, query) else dbGetQuery(con, query, params = list(batch_id))
  if (nrow(rows) == 0) return(rows)
  rows <- article_lab_normalize_candidate_rows(rows)
  rows[rows$approved_thumbnail_n <= 0, , drop = FALSE]
}

load_article_lab_thumbnail_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_thumbnail_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.status AS thumbnail_status,
      t.notes,
      t.model,
      t.generation_mode,
      t.reasoning_effort,
      t.reasoning_mode,
      t.submitted_prompt,
      t.revised_prompt,
      t.response_id,
      t.image_generation_call_id,
      t.variant_index,
      t.image_settings_json,
      t.local_asset_path,
      t.generation_run_id,
      t.approved_at,
      t.rejected_at,
      s.subtitle,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    " else "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.status AS thumbnail_status,
      t.notes,
      t.model,
      t.generation_mode,
      t.reasoning_effort,
      t.reasoning_mode,
      t.submitted_prompt,
      t.revised_prompt,
      t.response_id,
      t.image_generation_call_id,
      t.variant_index,
      t.image_settings_json,
      t.local_asset_path,
      t.generation_run_id,
      t.approved_at,
      t.rejected_at,
      s.subtitle,
      c.title,
      c.status AS parent_status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    WHERE t.batch_id = ?
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    "
  rows <- if (all_batches) dbGetQuery(con, query) else dbGetQuery(con, query, params = list(batch_id))
  if (nrow(rows) == 0) return(rows)
  approved_packages <- unique(rows$subtitle_id[rows$thumbnail_status == "approved"])
  rows[rows$thumbnail_status == "generated" & !(rows$subtitle_id %in% approved_packages), , drop = FALSE]
}

load_article_lab_ready_for_outline_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_thumbnail_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.notes,
      s.subtitle,
      c.title,
      c.status,
      o.outline_id,
      o.outline_text,
      o.status AS outline_status,
      o.notes AS outline_notes,
      o.model AS outline_model,
      o.generation_mode AS outline_generation_mode,
      o.updated_at AS outline_updated_at,
      o.approved_at AS outline_approved_at,
      t.outline_context_notes AS thumbnail_outline_context_notes
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    LEFT JOIN article_lab_outlines o
      ON o.thumbnail_id = t.thumbnail_id
     AND o.status IN ('draft', 'approved')
     AND (o.archived IS NULL OR o.archived = 0)
    WHERE t.status = 'approved'
      AND c.archived = 0
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    " else "
    SELECT
      t.thumbnail_id,
      t.subtitle_id,
      t.candidate_id,
      t.batch_id,
      t.created_at,
      t.thumbnail_label,
      t.thumbnail_data_uri,
      t.notes,
      s.subtitle,
      c.title,
      c.status,
      o.outline_id,
      o.outline_text,
      o.status AS outline_status,
      o.notes AS outline_notes,
      o.model AS outline_model,
      o.generation_mode AS outline_generation_mode,
      o.updated_at AS outline_updated_at,
      o.approved_at AS outline_approved_at,
      t.outline_context_notes AS thumbnail_outline_context_notes
    FROM article_lab_thumbnail_candidates t
    INNER JOIN article_lab_subtitle_candidates s
      ON s.subtitle_id = t.subtitle_id
    INNER JOIN article_lab_title_candidates c
      ON c.candidate_id = t.candidate_id
    LEFT JOIN article_lab_outlines o
      ON o.thumbnail_id = t.thumbnail_id
     AND o.status IN ('draft', 'approved')
     AND (o.archived IS NULL OR o.archived = 0)
    WHERE t.status = 'approved'
      AND c.archived = 0
      AND t.batch_id = ?
    ORDER BY t.created_at DESC, t.thumbnail_id DESC
    "
  if (all_batches) {
    dbGetQuery(con, query)
  } else {
    dbGetQuery(con, query, params = list(batch_id))
  }
}

load_article_lab_full_text_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_outlines")) return(data.frame())
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- if (all_batches) "
    SELECT
      o.outline_id, o.thumbnail_id, o.subtitle_id, o.candidate_id, o.batch_id,
      o.outline_text, o.updated_at AS outline_updated_at,
      t.thumbnail_label, t.thumbnail_data_uri,
      s.subtitle,
      c.title, c.status,
      d.full_text_draft_id, d.original_generated_text, d.current_draft_text,
      d.status AS draft_status, d.is_approved, d.model AS draft_model,
      d.prompt_key, d.prompt_version, d.generation_mode AS draft_generation_mode,
      d.source_context_mode, d.citation_map_json, d.notes AS draft_notes, d.created_at AS draft_created_at,
      d.updated_at AS draft_updated_at, d.approved_at AS draft_approved_at, d.rejected_at AS draft_rejected_at
    FROM article_lab_outlines o
    INNER JOIN article_lab_thumbnail_candidates t ON t.thumbnail_id = o.thumbnail_id
    INNER JOIN article_lab_subtitle_candidates s ON s.subtitle_id = o.subtitle_id
    INNER JOIN article_lab_title_candidates c ON c.candidate_id = o.candidate_id
    LEFT JOIN article_lab_full_text_drafts d ON d.outline_id = o.outline_id AND d.status != 'rejected'
    WHERE o.status = 'approved' AND c.archived = 0 AND (o.archived IS NULL OR o.archived = 0)
    ORDER BY o.updated_at DESC, d.updated_at DESC, o.outline_id DESC
    " else "
    SELECT
      o.outline_id, o.thumbnail_id, o.subtitle_id, o.candidate_id, o.batch_id,
      o.outline_text, o.updated_at AS outline_updated_at,
      t.thumbnail_label, t.thumbnail_data_uri,
      s.subtitle,
      c.title, c.status,
      d.full_text_draft_id, d.original_generated_text, d.current_draft_text,
      d.status AS draft_status, d.is_approved, d.model AS draft_model,
      d.prompt_key, d.prompt_version, d.generation_mode AS draft_generation_mode,
      d.source_context_mode, d.citation_map_json, d.notes AS draft_notes, d.created_at AS draft_created_at,
      d.updated_at AS draft_updated_at, d.approved_at AS draft_approved_at, d.rejected_at AS draft_rejected_at
    FROM article_lab_outlines o
    INNER JOIN article_lab_thumbnail_candidates t ON t.thumbnail_id = o.thumbnail_id
    INNER JOIN article_lab_subtitle_candidates s ON s.subtitle_id = o.subtitle_id
    INNER JOIN article_lab_title_candidates c ON c.candidate_id = o.candidate_id
    LEFT JOIN article_lab_full_text_drafts d ON d.outline_id = o.outline_id AND d.status != 'rejected'
    WHERE o.status = 'approved' AND c.archived = 0 AND (o.archived IS NULL OR o.archived = 0) AND o.batch_id = ?
    ORDER BY o.updated_at DESC, d.updated_at DESC, o.outline_id DESC
    "
  if (all_batches) dbGetQuery(con, query) else dbGetQuery(con, query, params = list(batch_id))
}

article_lab_full_text_package_rows <- function(rows) {
  if (nrow(rows) == 0) return(rows)
  rows[!duplicated(rows$outline_id), c("outline_id", "thumbnail_id", "subtitle_id", "candidate_id", "batch_id", "outline_text", "thumbnail_label", "thumbnail_data_uri", "subtitle", "title", "status"), drop = FALSE]
}


load_article_lab_publications <- function(con, active_only = TRUE) {
  if (!dbExistsTable(con, "article_lab_publications")) return(data.frame())
  query <- "SELECT publication_id, publication_name, platform, submission_notes, submission_url, is_active, created_at, updated_at FROM article_lab_publications"
  if (isTRUE(active_only)) query <- paste(query, "WHERE is_active = 1")
  query <- paste(query, "ORDER BY publication_name COLLATE NOCASE ASC")
  dbGetQuery(con, query)
}


load_article_lab_review_publish_rows <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_full_text_drafts")) return(data.frame())
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  query <- "
    SELECT
      d.full_text_draft_id, d.outline_id, d.thumbnail_id, d.subtitle_id, d.candidate_id, d.batch_id,
      d.current_draft_text, d.status AS draft_status, d.is_approved, d.approved_at, d.updated_at AS draft_updated_at,
      d.citation_map_json,
      c.title, c.status AS candidate_status, c.archived,
      s.subtitle,
      t.thumbnail_label, t.thumbnail_data_uri,
      ps.publish_settings_id, ps.medium_tags_json, ps.publishing_target, ps.publication_id,
      ps.publication_name_snapshot, ps.monetization, ps.canonical_url, ps.featured_image_alt_text,
      ps.image_credit_source, ps.published_url, ps.publish_status, ps.notes AS publish_notes,
      ps.submitted_at, ps.published_at, ps.updated_at AS publish_updated_at
    FROM article_lab_full_text_drafts d
    INNER JOIN article_lab_title_candidates c ON c.candidate_id = d.candidate_id
    LEFT JOIN article_lab_subtitle_candidates s ON s.subtitle_id = d.subtitle_id
    LEFT JOIN article_lab_thumbnail_candidates t ON t.thumbnail_id = d.thumbnail_id
    LEFT JOIN article_lab_publish_settings ps ON ps.full_text_draft_id = d.full_text_draft_id
    WHERE (d.status = 'approved' OR d.is_approved = 1)
      AND COALESCE(c.archived, 0) = 0
      AND COALESCE(ps.publish_status, '') != 'archived'
  "
  params <- list()
  if (!all_batches) {
    query <- paste(query, "AND d.batch_id = ?")
    params <- list(batch_id)
  }
  query <- paste(query, "ORDER BY d.approved_at DESC, d.updated_at DESC, d.full_text_draft_id DESC")
  if (length(params) == 0) dbGetQuery(con, query) else dbGetQuery(con, query, params = params)
}

article_lab_medium_ready_markdown <- function(row, settings = row) {
  if (nrow(row) == 0) return("")
  title <- article_lab_row_value(row, "title", "Untitled")
  subtitle <- article_lab_row_value(row, "subtitle", "")
  body <- article_lab_row_value(row, "current_draft_text", "")
  alt_text <- article_lab_row_value(settings, "featured_image_alt_text", "")
  credit <- article_lab_row_value(settings, "image_credit_source", "")
  parts <- c(paste0("# ", title))
  if (!is.na(subtitle) && nzchar(subtitle)) parts <- c(parts, paste0("_", subtitle, "_"))
  if (!is.na(alt_text) && nzchar(alt_text)) parts <- c(parts, paste0("![", alt_text, "]()"))
  if (!is.na(credit) && nzchar(credit)) parts <- c(parts, paste0("Image credit/source: ", credit))
  parts <- c(parts, body)
  paste(parts, collapse = "\n\n")
}

article_lab_medium_tags_effective_prompt <- function(row, prompt = NA_character_) {
  if (nrow(row) == 0) return("")
  base_prompt <- article_lab_input_multiline(prompt) %||% article_lab_default_medium_tags_prompt
  input_context <- paste(
    sprintf("full_text_draft_id=%s | candidate_id=%s | batch_id=%s", article_lab_row_value(row, "full_text_draft_id", ""), article_lab_row_value(row, "candidate_id", ""), article_lab_row_value(row, "batch_id", "")),
    sprintf("Title: %s", article_lab_row_value(row, "title", "")),
    sprintf("Subtitle: %s", article_lab_row_value(row, "subtitle", "")),
    "Article body:",
    article_lab_row_value(row, "current_draft_text", ""),
    sep = "\n\n"
  )
  if (grepl("{{input_context}}", base_prompt, fixed = TRUE)) return(article_lab_render_prompt_template(base_prompt, list(input_context = input_context)))
  paste(base_prompt, "Article package:", input_context, sep = "\n\n")
}


article_lab_unscored_candidates <- function(con, batch_id, model, prompt_version, scope) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  dbGetQuery(
    con,
    if (all_batches) "
    SELECT c.candidate_id, c.batch_id, c.title, c.status, c.title_char_count, c.title_length_flag
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    WHERE c.archived = 0
      AND c.promoted = 0
      AND c.status = 'ready_for_api_scoring'
      AND s.candidate_id IS NULL
    ORDER BY c.created_at, c.candidate_id
    " else "
    SELECT c.candidate_id, c.batch_id, c.title, c.status, c.title_char_count, c.title_length_flag
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    WHERE c.batch_id = ?
      AND c.archived = 0
      AND c.promoted = 0
      AND c.status = 'ready_for_api_scoring'
      AND s.candidate_id IS NULL
    ORDER BY c.created_at, c.candidate_id
    ",
    params = if (all_batches) list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope
    ) else list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope,
      batch_id
    )
  )
}


load_latest_article_lab_batch <- function(con, article_project_id = NULL) {
  if (!dbExistsTable(con, "article_lab_title_batches")) return(NULL)
  project_filter_supplied <- !missing(article_project_id)
  project_id <- article_lab_input_string(article_project_id)
  if (project_filter_supplied && is.null(project_id)) return(NULL)
  batch <- if (!is.null(project_id) && nzchar(project_id)) dbGetQuery(con, "
    SELECT batch_id, article_project_id, created_at, prompt, seed_topic, inspiration_source,
      requested_batch_size, model, status, notes, article_context_notes
    FROM article_lab_title_batches
    WHERE article_project_id = ?
    ORDER BY created_at DESC, batch_id DESC
    LIMIT 1
  ", params = list(project_id)) else dbGetQuery(con, "
    SELECT batch_id, article_project_id, created_at, prompt, seed_topic, inspiration_source,
      requested_batch_size, model, status, notes, article_context_notes
    FROM article_lab_title_batches
    ORDER BY created_at DESC, batch_id DESC
    LIMIT 1
  ")
  if (nrow(batch) == 0) NULL else batch[1, , drop = FALSE]
}

load_article_lab_candidates_for_batch <- function(con, batch_id) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame())
  }
  if (identical(batch_id, article_lab_all_batches_value)) {
    return(dbGetQuery(
      con,
      "SELECT candidate_id, title, title_char_count, title_length_flag, status, created_at, batch_id,
         ready_for_human_rating, archived, promoted, notes
       FROM article_lab_title_candidates
       ORDER BY created_at DESC, candidate_id DESC"
    ))
  }
  dbGetQuery(
    con,
    "SELECT candidate_id, title, title_char_count, title_length_flag, status, created_at, batch_id,
       ready_for_human_rating, archived, promoted, notes
     FROM article_lab_title_candidates
     WHERE batch_id = ?
     ORDER BY candidate_id",
    params = list(batch_id)
  )
}

load_article_lab_batches <- function(con, article_project_id = NULL) {
  if (!dbExistsTable(con, "article_lab_title_batches")) return(data.frame())
  project_filter_supplied <- !missing(article_project_id)
  project_id <- article_lab_input_string(article_project_id)
  if (project_filter_supplied && is.null(project_id)) return(data.frame())
  if (!is.null(project_id) && nzchar(project_id)) return(dbGetQuery(con, "
    SELECT batch_id, article_project_id, created_at, requested_batch_size, model, status, seed_topic, inspiration_source, article_context_notes
    FROM article_lab_title_batches
    WHERE article_project_id = ?
    ORDER BY created_at DESC, batch_id DESC
  ", params = list(project_id)))
  dbGetQuery(con, "
    SELECT batch_id, article_project_id, created_at, requested_batch_size, model, status, seed_topic, inspiration_source, article_context_notes
    FROM article_lab_title_batches
    ORDER BY created_at DESC, batch_id DESC
  ")
}

load_article_lab_scoring_rows <- function(con, batch_id, model, prompt_version, scope) {
  if (is.null(batch_id) || is.na(batch_id) || !nzchar(batch_id) || !dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame())
  }
  all_batches <- identical(batch_id, article_lab_all_batches_value)
  rows <- dbGetQuery(
    con,
    if (all_batches) "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.title_char_count,
      c.title_length_flag,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      s.score_id,
      s.scored_at,
      s.model,
      s.prompt_version,
      s.scope,
      s.clarity,
      s.curiosity,
      s.specificity,
      s.beginner_appeal,
      s.credibility,
      s.emotional_pull,
      s.promise_strength,
      s.trust_risk,
      s.medium_clap_potential,
      s.medium_comment_potential,
      s.overall_article_potential,
      s.combined_title_score,
      s.predicted_success_bucket,
      s.short_reason,
      s.error
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    ORDER BY c.created_at DESC, c.candidate_id DESC
    " else "
    SELECT
      c.candidate_id,
      c.batch_id,
      c.created_at,
      c.title,
      c.title_char_count,
      c.title_length_flag,
      c.status,
      c.ready_for_human_rating,
      c.promoted,
      c.archived,
      c.notes,
      s.score_id,
      s.scored_at,
      s.model,
      s.prompt_version,
      s.scope,
      s.clarity,
      s.curiosity,
      s.specificity,
      s.beginner_appeal,
      s.credibility,
      s.emotional_pull,
      s.promise_strength,
      s.trust_risk,
      s.medium_clap_potential,
      s.medium_comment_potential,
      s.overall_article_potential,
      s.combined_title_score,
      s.predicted_success_bucket,
      s.short_reason,
      s.error
    FROM article_lab_title_candidates c
    LEFT JOIN article_lab_title_api_scores s
      ON s.candidate_id = c.candidate_id
     AND s.model = ?
     AND s.prompt_version = ?
     AND s.scope = ?
    WHERE c.batch_id = ?
    ORDER BY c.created_at DESC, c.candidate_id DESC
    ",
    params = if (all_batches) list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope
    ) else list(
      article_lab_input_string(model) %||% article_lab_default_score_model,
      article_lab_input_string(prompt_version) %||% article_lab_default_score_prompt_version,
      article_lab_input_string(scope) %||% article_lab_default_score_scope,
      batch_id
    )
  )
  if (nrow(rows) == 0) return(rows)
  rows$title_char_count <- ifelse(
    is.na(rows$title_char_count),
    article_lab_title_length(rows$title),
    suppressWarnings(as.integer(rows$title_char_count))
  )
  rows$title_length_flag <- ifelse(
    is.na(rows$title_length_flag) |
      rows$title_length_flag == "risky" |
      (rows$title_length_flag == "too_long" & rows$title_char_count <= article_lab_title_max_chars),
    article_lab_title_length_flag(rows$title_char_count),
    rows$title_length_flag
  )
  if (!("combined_title_score" %in% names(rows))) rows$combined_title_score <- NA_real_
  missing_combined <- is.na(rows$combined_title_score) & !is.na(rows$curiosity) & !is.na(rows$emotional_pull) &
    !is.na(rows$medium_comment_potential) & !is.na(rows$overall_article_potential)
  rows$combined_title_score[missing_combined] <- article_lab_combined_title_score(
    curiosity = rows$curiosity[missing_combined],
    emotional_pull = rows$emotional_pull[missing_combined],
    medium_comment_potential = rows$medium_comment_potential[missing_combined],
    overall_article_potential = rows$overall_article_potential[missing_combined],
    trust_risk = rows$trust_risk[missing_combined],
    title_char_count = rows$title_char_count[missing_combined]
  )
  rows
}

article_lab_overview <- function(con) {
  if (!dbExistsTable(con, "article_lab_title_candidates")) {
    return(data.frame(
      saved_batches = 0L,
      saved_candidates = 0L,
      generated = 0L,
      disqualified = 0L,
      ready_for_api_scoring = 0L,
      api_scored = 0L,
      approved_for_subtitle = 0L,
      ready_for_thumbnail = 0L,
      ready_for_outline = 0L,
      ready_for_draft = 0L,
      rejected = 0L,
      archived = 0L
    ))
  }

  dbGetQuery(con, "
    SELECT
      (SELECT COUNT(*) FROM article_lab_title_batches) AS saved_batches,
      COUNT(*) AS saved_candidates,
      COALESCE(SUM(CASE WHEN status = 'generated' THEN 1 ELSE 0 END), 0) AS generated,
      COALESCE(SUM(CASE WHEN status = 'disqualified' THEN 1 ELSE 0 END), 0) AS disqualified,
      COALESCE(SUM(CASE WHEN status = 'ready_for_api_scoring' THEN 1 ELSE 0 END), 0) AS ready_for_api_scoring,
      COALESCE(SUM(CASE WHEN status = 'api_scored' THEN 1 ELSE 0 END), 0) AS api_scored,
      COALESCE(SUM(CASE WHEN status = 'approved_for_subtitle' OR promoted = 1 THEN 1 ELSE 0 END), 0) AS approved_for_subtitle,
      COALESCE(SUM(CASE WHEN status = 'ready_for_thumbnail' THEN 1 ELSE 0 END), 0) AS ready_for_thumbnail,
      COALESCE(SUM(CASE WHEN status = 'ready_for_outline' THEN 1 ELSE 0 END), 0) AS ready_for_outline,
      COALESCE(SUM(CASE WHEN status = 'ready_for_draft' THEN 1 ELSE 0 END), 0) AS ready_for_draft,
      COALESCE(SUM(CASE WHEN status = 'rejected' THEN 1 ELSE 0 END), 0) AS rejected,
      COALESCE(SUM(CASE WHEN archived = 1 THEN 1 ELSE 0 END), 0) AS archived
    FROM article_lab_title_candidates
  ")
}
