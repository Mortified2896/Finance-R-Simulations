initialize_app_database <- local({
  initialized <- FALSE

  function() {
    if (isTRUE(initialized)) return(invisible(TRUE))
    con <- connect_db()
    on.exit(dbDisconnect(con), add = TRUE)

    ensure_rating_schema(con)
    ensure_research_workflow_schema(con)
    ensure_article_inbox_schema(con)
    ensure_article_lab_schema(con)
    if (dbExistsTable(con, "article_lab_title_batches") && dbExistsTable(con, "article_projects") && dbExistsTable(con, "research_article_angles")) {
      dbExecute(con, "
        UPDATE article_lab_title_batches
        SET article_project_id = (
          SELECT p.article_project_id
          FROM research_article_angles a
          JOIN article_candidates c ON c.research_angle_id = a.research_angle_id
          JOIN article_projects p ON p.article_candidate_id = c.candidate_id
          WHERE a.article_lab_batch_id = article_lab_title_batches.batch_id
          LIMIT 1
        )
        WHERE article_project_id IS NULL
          AND EXISTS (
            SELECT 1
            FROM research_article_angles a
            JOIN article_candidates c ON c.research_angle_id = a.research_angle_id
            JOIN article_projects p ON p.article_candidate_id = c.candidate_id
            WHERE a.article_lab_batch_id = article_lab_title_batches.batch_id
          )
      ")
    }
    article_lab_recover_api_pending_candidates(con)
    if (is_dimension_mode) ensure_dimension_pass_queues(con, target_n = default_target_n)

    initialized <<- TRUE
    invisible(TRUE)
  }
})
