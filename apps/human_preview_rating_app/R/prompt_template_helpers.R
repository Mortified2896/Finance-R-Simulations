article_lab_prompt_workflows <- c(
  titles = "Title generation", scoring = "Title scoring", subtitles = "Subtitle generation",
  thumbnails = "Thumbnail generation", outlines = "Outline generation", full_text = "Full article generation",
  research_summary = "Research summary", research_claims = "Research claim extraction",
  research_evidence = "Research evidence selection", medium_tags = "Medium tag generation"
)

# Canonical prompt-variable registry. This is the only source of truth for
# editor help, validation and rendering. Source fields are application metadata;
# only the formatted value of a variable referenced by a template is model-visible.
article_lab_prompt_variable_registry <- list(
  titles = list(
    idea_context = list(meaning = "Populated reader-facing article-idea fields.", sources = c("idea fields"), format = "Labeled text lines; internal project IDs omitted.", required = TRUE, conditional = FALSE),
    article_summary = list(meaning = "Selected research/article summary.", sources = c("summary_text"), format = "Plain text.", required = FALSE, conditional = TRUE),
    batch_size = list(meaning = "Requested title count.", sources = c("batch_size"), format = "Integer text.", required = TRUE, conditional = FALSE),
    seed_topic = list(meaning = "Optional user-entered seed topic.", sources = c("seed_topic"), format = "Plain text.", required = FALSE, conditional = TRUE),
    inspiration_source = list(meaning = "Optional selected inspiration source label.", sources = c("inspiration_source"), format = "Plain text.", required = FALSE, conditional = TRUE),
    example_titles = list(meaning = "Optional reference titles.", sources = c("historical title text"), format = "One numbered title per line; record IDs omitted.", required = FALSE, conditional = TRUE),
    max_title_chars = list(meaning = "Hard title character limit.", sources = c("title limit setting"), format = "Integer text.", required = TRUE, conditional = FALSE),
    preferred_title_length = list(meaning = "Preferred title-length range.", sources = c("preferred length settings"), format = "min-max text.", required = TRUE, conditional = FALSE)
  ),
  scoring = list(
    prompt_version = list(meaning = "Selected scoring rubric version.", sources = c("prompt_version"), format = "Plain text.", required = TRUE, conditional = FALSE),
    scope = list(meaning = "Selected scoring scope.", sources = c("scope"), format = "Plain text.", required = TRUE, conditional = FALSE),
    title = list(meaning = "Reader-facing title to score.", sources = c("title"), format = "Plain text; candidate and batch IDs omitted.", required = TRUE, conditional = FALSE)
  ),
  subtitles = list(
    input_context = list(meaning = "Selected titles and any attached summaries.", sources = c("title", "article_summary"), format = "Request-local item aliases with title and optional summary; database IDs omitted.", required = TRUE, conditional = TRUE),
    variants_per_title = list(meaning = "Requested subtitles per title.", sources = c("variants_per_title"), format = "Integer text.", required = TRUE, conditional = FALSE),
    max_subtitle_chars = list(meaning = "Hard subtitle character limit.", sources = c("subtitle limit setting"), format = "Integer text.", required = TRUE, conditional = FALSE)
  ),
  thumbnails = list(
    input_context = list(meaning = "One selected title/subtitle package.", sources = c("title", "subtitle"), format = "Title and subtitle only; database IDs omitted.", required = FALSE, conditional = TRUE)
  ),
  outlines = list(
    input_context = list(meaning = "Selected creative packages and optional text research fallback.", sources = c("title", "subtitle", "thumbnail_label", "article_summary"), format = "Request-local item aliases; raw IDs and file paths omitted.", required = TRUE, conditional = FALSE),
    context_notes = list(meaning = "Optional author-entered outline notes.", sources = c("outline_context_notes"), format = "Plain text, present only when referenced.", required = FALSE, conditional = TRUE)
  ),
  full_text = list(
    input_context = list(meaning = "Selected approved package, outline, and allowed evidence context.", sources = c("title", "subtitle", "thumbnail_label", "outline_text", "checked_evidence"), format = "Request-local item aliases; raw IDs and paths omitted.", required = TRUE, conditional = FALSE)
  ),
  research_summary = list(
    input_context = list(meaning = "Reader-facing source context.", sources = c("source_title", "source_url", "pdf_url", "main_idea", "abstract"), format = "Labeled text; research-source ID and local path omitted.", required = FALSE, conditional = TRUE)
  ),
  research_claims = list(
    max_claims = list(meaning = "Maximum claims to extract.", sources = c("max_claims"), format = "Integer text.", required = TRUE, conditional = FALSE),
    source_title = list(meaning = "Research source title.", sources = c("source_title"), format = "Plain text.", required = FALSE, conditional = TRUE),
    summary_sentence_payload_json = list(meaning = "Numbered summary sentences.", sources = c("summary_text"), format = "JSON array using request-local sentence indexes; database IDs omitted.", required = TRUE, conditional = FALSE)
  ),
  research_evidence = list(
    claim_candidate_payload_json = list(meaning = "Claims and candidate evidence sentences.", sources = c("claim_text", "sentence_text", "page_number"), format = "JSON with request-local claim/sentence aliases; database IDs omitted.", required = TRUE, conditional = FALSE)
  ),
  medium_tags = list(
    input_context = list(meaning = "Approved article text package.", sources = c("title", "subtitle", "current_draft_text"), format = "Labeled title, subtitle and body; database IDs omitted.", required = TRUE, conditional = FALSE)
  )
)

article_lab_prompt_registry_variables <- function(workflow_key) {
  workflow <- article_lab_input_string(workflow_key)
  registry <- article_lab_prompt_variable_registry[[workflow]]
  if (is.null(registry)) stop("Unknown prompt workflow.", call. = FALSE)
  names(registry)
}

article_lab_prompt_registry_help <- function(workflow_key) {
  registry <- article_lab_prompt_variable_registry[[workflow_key]]
  if (is.null(registry)) stop("Unknown prompt workflow.", call. = FALSE)
  vapply(names(registry), function(name) {
    item <- registry[[name]]
    sprintf("{{%s}} — %s Format: %s %s%s", name, item$meaning, item$format,
      if (isTRUE(item$required)) "Required value." else "Optional value.",
      if (isTRUE(item$conditional)) " May be used as a conditional section." else "")
  }, character(1))
}

article_lab_sanitize_canonical_request <- function(request) {
  scrub <- function(value, key = "") {
    if (grepl("api[_-]?key|authorization|secret|credential", key, ignore.case = TRUE)) return("[REDACTED]")
    if (is.list(value)) return(setNames(lapply(names(value), function(name) scrub(value[[name]], name)), names(value)))
    value
  }
  scrub(request)
}

article_lab_record_generation_attempt <- function(con, workflow_key, template_id, template_name, prompt_template,
                                                  resolved_prompt, canonical_request, model, reasoning_effort = NA_character_,
                                                  reasoning_mode = "standard", attachments = list(), request_id = NA_character_,
                                                  response_id = NA_character_, attempt_number = 1L, status = "submitted",
                                                  error_message = NA_character_) {
  timestamp <- now_utc()
  attempt_id <- paste0("ga_", sprintf("%.0f", as.numeric(Sys.time()) * 1000000), "_", sprintf("%06d", sample.int(999999L, 1L)))
  dbExecute(con, "
    INSERT INTO article_lab_generation_attempts
      (attempt_id, workflow_key, template_id, template_name, prompt_template, resolved_prompt,
       canonical_request_json, model, reasoning_effort, reasoning_mode, attachment_references_json,
       openai_request_id, openai_response_id, attempt_number, status, error_message, created_at, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(attempt_id, workflow_key, template_id, template_name, prompt_template, resolved_prompt,
    jsonlite::toJSON(article_lab_sanitize_canonical_request(canonical_request), auto_unbox = TRUE, null = "null"),
    model, reasoning_effort, reasoning_mode,
    jsonlite::toJSON(attachments, auto_unbox = TRUE, null = "null"), request_id, response_id,
    as.integer(attempt_number), status, error_message, timestamp, timestamp))
  attempt_id
}

article_lab_record_current_attempt <- function(workflow_key, prompt_template, resolved_prompt, request_payload,
                                               attachments = list(), status = "failed") {
  con <- getOption("article_lab.generation_connection")
  if (is.null(con) || !DBI::dbIsValid(con) || !DBI::dbExistsTable(con, "article_lab_generation_attempts")) return(NA_character_)
  template_id <- article_lab_prompt_template_active(con, workflow_key)
  rows <- if (is.null(template_id) || is.na(template_id)) data.frame() else article_lab_prompt_template_rows(con, workflow_key)
  row <- if (nrow(rows)) rows[rows$template_id == template_id, , drop = FALSE] else data.frame()
  canonical <- list(model = request_payload$model, input = resolved_prompt)
  if (!is.null(request_payload$reasoning_effort) && !is.na(request_payload$reasoning_effort)) canonical$reasoning <- list(effort = request_payload$reasoning_effort)
  if (identical(request_payload$reasoning_mode, "pro")) canonical$reasoning <- c(canonical$reasoning %||% list(), list(mode = "pro"))
  if (length(attachments)) canonical$input <- list(list(role = "user", content = c(list(list(type = "input_text", text = resolved_prompt)), attachments)))
  article_lab_record_generation_attempt(con, workflow_key, template_id,
    if (nrow(row)) row$template_name[[1]] else NA_character_, prompt_template, resolved_prompt,
    canonical, request_payload$model, request_payload$reasoning_effort, request_payload$reasoning_mode,
    attachments, status = status)
}

article_lab_finish_generation_attempt <- function(attempt_id, status, response_id = NA_character_, request_id = NA_character_, error_message = NA_character_) {
  con <- getOption("article_lab.generation_connection")
  if (is.null(attempt_id) || is.na(attempt_id) || is.null(con) || !DBI::dbIsValid(con)) return(invisible(FALSE))
  DBI::dbExecute(con, "UPDATE article_lab_generation_attempts SET status = ?, openai_response_id = ?, openai_request_id = ?, error_message = ?, updated_at = ? WHERE attempt_id = ?",
    params = list(status, response_id, request_id, error_message, now_utc(), attempt_id))
  invisible(TRUE)
}

article_lab_prompt_text_value <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1]])) return(NULL)
  text <- as.character(value[[1]])
  if (!nzchar(trimws(text))) NULL else text
}

article_lab_prompt_template_rows <- function(con, workflow_key) {
  dbGetQuery(con, "
    SELECT template_id, workflow_key, template_name, prompt_text, created_at, updated_at
    FROM article_lab_prompt_templates
    WHERE workflow_key = ?
    ORDER BY updated_at DESC, template_name COLLATE NOCASE
  ", params = list(workflow_key))
}

article_lab_prompt_template_active <- function(con, workflow_key) {
  rows <- dbGetQuery(con, "SELECT template_id FROM article_lab_prompt_selections WHERE workflow_key = ?", params = list(workflow_key))
  if (nrow(rows) == 0) return(NA_character_)
  rows$template_id[[1]]
}

article_lab_set_active_prompt_template <- function(con, workflow_key, template_id) {
  timestamp <- now_utc()
  dbExecute(con, "
    INSERT INTO article_lab_prompt_selections (workflow_key, template_id, updated_at) VALUES (?, ?, ?)
    ON CONFLICT(workflow_key) DO UPDATE SET template_id = excluded.template_id, updated_at = excluded.updated_at
  ", params = list(workflow_key, template_id, timestamp))
  invisible(template_id)
}

article_lab_create_prompt_template <- function(con, workflow_key, template_name, prompt_text) {
  workflow <- article_lab_input_string(workflow_key)
  name <- article_lab_input_string(template_name)
  text <- article_lab_prompt_text_value(prompt_text)
  if (is.null(workflow) || !workflow %in% names(article_lab_prompt_workflows)) stop("Unknown prompt workflow.", call. = FALSE)
  if (is.null(name)) stop("Template name is required.", call. = FALSE)
  if (is.null(text)) stop("Prompt template cannot be blank.", call. = FALSE)
  duplicate <- dbGetQuery(con, "SELECT 1 FROM article_lab_prompt_templates WHERE workflow_key = ? AND template_name = ? COLLATE NOCASE LIMIT 1", params = list(workflow, name))
  if (nrow(duplicate) > 0) stop("A template with this name already exists in this workflow.", call. = FALSE)
  id <- paste0("pt_", sprintf("%.0f", as.numeric(Sys.time()) * 1000000), "_", sprintf("%06d", sample.int(999999L, 1L)))
  timestamp <- now_utc()
  dbExecute(con, "INSERT INTO article_lab_prompt_templates (template_id, workflow_key, template_name, prompt_text, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?)", params = list(id, workflow, name, text, timestamp, timestamp))
  article_lab_set_active_prompt_template(con, workflow, id)
  invisible(id)
}

article_lab_update_prompt_template <- function(con, template_id, template_name, prompt_text) {
  id <- article_lab_input_string(template_id)
  name <- article_lab_input_string(template_name)
  text <- article_lab_prompt_text_value(prompt_text)
  if (is.null(id)) stop("Select a template first.", call. = FALSE)
  if (is.null(name)) stop("Template name is required.", call. = FALSE)
  if (is.null(text)) stop("Prompt template cannot be blank.", call. = FALSE)
  row <- dbGetQuery(con, "SELECT workflow_key FROM article_lab_prompt_templates WHERE template_id = ?", params = list(id))
  if (nrow(row) == 0) stop("The selected template no longer exists.", call. = FALSE)
  duplicate <- dbGetQuery(con, "SELECT 1 FROM article_lab_prompt_templates WHERE workflow_key = ? AND template_name = ? COLLATE NOCASE AND template_id <> ? LIMIT 1", params = list(row$workflow_key[[1]], name, id))
  if (nrow(duplicate) > 0) stop("A template with this name already exists in this workflow.", call. = FALSE)
  dbExecute(con, "UPDATE article_lab_prompt_templates SET template_name = ?, prompt_text = ?, updated_at = ? WHERE template_id = ?", params = list(name, text, now_utc(), id))
  invisible(id)
}

article_lab_delete_prompt_template <- function(con, template_id) {
  id <- article_lab_input_string(template_id)
  if (is.null(id)) stop("Select a template first.", call. = FALSE)
  row <- dbGetQuery(con, "SELECT workflow_key FROM article_lab_prompt_templates WHERE template_id = ?", params = list(id))
  if (nrow(row) == 0) stop("The selected template no longer exists.", call. = FALSE)
  workflow <- row$workflow_key[[1]]
  dbExecute(con, "DELETE FROM article_lab_prompt_templates WHERE template_id = ?", params = list(id))
  remaining <- article_lab_prompt_template_rows(con, workflow)
  if (nrow(remaining) == 0) {
    dbExecute(con, "DELETE FROM article_lab_prompt_selections WHERE workflow_key = ?", params = list(workflow))
    return(invisible(NA_character_))
  }
  article_lab_set_active_prompt_template(con, workflow, remaining$template_id[[1]])
}

article_lab_validate_prompt_variables <- function(prompt_text, allowed_variables) {
  text <- article_lab_prompt_text_value(prompt_text)
  if (is.null(text)) stop("Prompt template cannot be blank.", call. = FALSE)
  article_lab_parse_prompt_template(text, allowed_variables)
  invisible(TRUE)
}

article_lab_prompt_manager_ui <- function(id, label = "Editable prompt template", height = "170px", workflow_key = NULL, variables = character()) {
  ns <- NS(id)
  if (!is.null(workflow_key)) variables <- article_lab_prompt_registry_variables(workflow_key)
  tagList(
    div(class = "lab-grid",
      div(class = "lab-field", uiOutput(ns("selector"))),
      div(class = "lab-field", textInput(ns("name"), "Template name", value = "", width = "100%"))
    ),
    div(class = "lab-field", textAreaInput(ns("prompt"), label, value = "", width = "100%", height = height)),
    if (length(variables) > 0) tagList(
      p(class = "lab-status-copy", article_lab_prompt_variable_help(variables)),
      if (!is.null(workflow_key)) tags$ul(class = "lab-status-copy", lapply(article_lab_prompt_registry_help(workflow_key), tags$li)) else NULL,
      p(class = "lab-status-copy", "Conditional sections use {{#variable}}…{{/variable}} and are omitted when the value is empty; conditional blocks cannot be nested.")
    ) else NULL,
    div(class = "lab-actions",
      actionButton(ns("save"), "Save changes", class = "lab-primary"),
      actionButton(ns("save_as"), "Save as new", class = "lab-secondary"),
      actionButton(ns("rename"), "Rename", class = "lab-secondary"),
      actionButton(ns("delete_template"), "Delete", class = "lab-danger")
    ),
    uiOutput(ns("status"))
  )
}

article_lab_prompt_manager_server <- function(id, con, workflow_key, allowed_variables = NULL) {
  registry_variables <- article_lab_prompt_registry_variables(workflow_key)
  if (!is.null(allowed_variables) && !identical(as.character(allowed_variables), registry_variables)) stop("Prompt variables must come from the canonical workflow registry.", call. = FALSE)
  allowed_variables <- registry_variables
  moduleServer(id, function(input, output, session) {
    revision <- reactiveVal(0L)
    selected_id <- reactiveVal(NA_character_)
    saved_name <- reactiveVal("")
    saved_prompt <- reactiveVal("")
    error_message <- reactiveVal(NULL)

    refresh <- function(preferred = NULL) {
      rows <- article_lab_prompt_template_rows(con, workflow_key)
      id_value <- article_lab_input_string(preferred) %||% article_lab_prompt_template_active(con, workflow_key)
      if (nrow(rows) == 0) {
        selected_id(NA_character_); saved_name(""); saved_prompt("")
        updateTextInput(session, "name", value = "")
        updateTextAreaInput(session, "prompt", value = "")
      } else {
        if (is.null(id_value) || !id_value %in% rows$template_id) id_value <- rows$template_id[[1]]
        row <- rows[rows$template_id == id_value, , drop = FALSE]
        selected_id(id_value); saved_name(row$template_name[[1]]); saved_prompt(row$prompt_text[[1]])
        article_lab_set_active_prompt_template(con, workflow_key, id_value)
        updateTextInput(session, "name", value = row$template_name[[1]])
        updateTextAreaInput(session, "prompt", value = row$prompt_text[[1]])
      }
      revision(revision() + 1L)
    }

    output$selector <- renderUI({
      revision()
      rows <- article_lab_prompt_template_rows(con, workflow_key)
      selectInput(session$ns("template"), "Saved template", choices = setNames(rows$template_id, rows$template_name), selected = selected_id(), width = "100%")
    })
    output$status <- renderUI({
      revision(); err <- error_message()
      rows <- article_lab_prompt_template_rows(con, workflow_key)
      if (!is.null(err)) return(div(class = "lab-error-box", strong("Prompt template error"), p(err)))
      if (nrow(rows) == 0) return(div(class = "lab-error-box", strong("No prompt template configured"), p("Create and save a valid template before running this workflow.")))
      dirty <- !identical(article_lab_input_string(input$name) %||% "", saved_name()) || !identical(article_lab_prompt_text_value(input$prompt) %||% "", saved_prompt())
      p(class = "lab-status-copy", if (dirty) "Unsaved template changes." else "Template saved.")
    })
    observeEvent(input$template, {
      rows <- article_lab_prompt_template_rows(con, workflow_key)
      row <- rows[rows$template_id == input$template, , drop = FALSE]
      if (nrow(row) == 0) return()
      selected_id(row$template_id[[1]]); saved_name(row$template_name[[1]]); saved_prompt(row$prompt_text[[1]])
      article_lab_set_active_prompt_template(con, workflow_key, row$template_id[[1]])
      updateTextInput(session, "name", value = row$template_name[[1]])
      updateTextAreaInput(session, "prompt", value = row$prompt_text[[1]])
      error_message(NULL); revision(revision() + 1L)
    }, ignoreInit = TRUE)

    persist <- function(create = FALSE, rename_only = FALSE) {
      tryCatch({
        name <- article_lab_input_string(input$name)
        text <- if (rename_only) saved_prompt() else article_lab_prompt_text_value(input$prompt)
        article_lab_validate_prompt_variables(text, allowed_variables)
        id_value <- if (create) article_lab_create_prompt_template(con, workflow_key, name, text) else article_lab_update_prompt_template(con, selected_id(), name, text)
        error_message(NULL); refresh(id_value)
      }, error = function(err) { error_message(conditionMessage(err)); revision(revision() + 1L) })
    }
    observeEvent(input$save, persist(FALSE, FALSE))
    observeEvent(input$save_as, persist(TRUE, FALSE))
    observeEvent(input$rename, persist(FALSE, TRUE))
    observeEvent(input$delete_template, {
      if (is.null(selected_id()) || is.na(selected_id())) return()
      showModal(modalDialog(
        title = "Delete prompt template?",
        sprintf("Delete '%s'? This cannot be undone.", saved_name()),
        footer = tagList(modalButton("Cancel"), actionButton(session$ns("confirm_delete"), "Delete", class = "btn-danger"))
      ))
    })
    observeEvent(input$confirm_delete, {
      removeModal()
      tryCatch({ article_lab_delete_prompt_template(con, selected_id()); error_message(NULL); refresh() }, error = function(err) { error_message(conditionMessage(err)); revision(revision() + 1L) })
    })
    observeEvent(TRUE, refresh(), once = TRUE)

    list(
      prompt = reactive(article_lab_prompt_text_value(input$prompt) %||% ""),
      template_id = reactive(article_lab_input_string(selected_id())),
      template_name = reactive(article_lab_input_string(input$name)),
      valid = reactive({
        if (is.na(selected_id()) || is.null(article_lab_prompt_text_value(input$prompt))) return(FALSE)
        isTRUE(tryCatch({ article_lab_validate_prompt_variables(input$prompt, allowed_variables); TRUE }, error = function(e) FALSE))
      })
    )
  })
}
