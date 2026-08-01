initialize_app_database <- local({
  initialized <- FALSE

  function() {
    if (isTRUE(initialized)) return(invisible(TRUE))
    con <- connect_db()
    on.exit(dbDisconnect(con), add = TRUE)

    ensure_rating_schema(con)
    ensure_article_lab_schema(con)
    ensure_research_workflow_schema(con)
    ensure_article_inbox_schema(con)
    article_lab_recover_api_pending_candidates(con)
    if (is_dimension_mode) ensure_dimension_pass_queues(con, target_n = default_target_n)

    initialized <<- TRUE
    invisible(TRUE)
  }
})
