article_lab_prompt_workflows <- c(
  titles = "Title generation", scoring = "Title scoring", subtitles = "Subtitle generation",
  thumbnails = "Thumbnail generation", outlines = "Outline generation", full_text = "Full article generation",
  research_summary = "Research summary", research_claims = "Research claim extraction",
  research_evidence = "Research evidence selection", medium_tags = "Medium tag generation"
)

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

article_lab_prompt_manager_ui <- function(id, label = "Editable prompt template", height = "170px", variables = character()) {
  ns <- NS(id)
  tagList(
    div(class = "lab-grid",
      div(class = "lab-field", uiOutput(ns("selector"))),
      div(class = "lab-field", textInput(ns("name"), "Template name", value = "", width = "100%"))
    ),
    div(class = "lab-field", textAreaInput(ns("prompt"), label, value = "", width = "100%", height = height)),
    if (length(variables) > 0) tagList(
      p(class = "lab-status-copy", article_lab_prompt_variable_help(variables)),
      p(class = "lab-status-copy", sprintf("Optional section: {{#%s}}…{{/%s}}. It is omitted when %s is empty; conditional blocks cannot be nested.", variables[[1]], variables[[1]], sprintf("{{%s}}", variables[[1]])))
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

article_lab_prompt_manager_server <- function(id, con, workflow_key, allowed_variables = character()) {
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
