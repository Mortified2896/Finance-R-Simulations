article_lab_count_badge <- function(count, label = "titles") {
  tags$span(class = "lab-count-badge", sprintf("%s %s", count, ifelse(identical(count, 1L), sub("s$", "", label), label)))
}

article_lab_signal_chip <- function(label, value, class_name = "default") {
  tags$span(
    class = paste("lab-chip", class_name),
    sprintf("%s %s", label, ifelse(is.na(value), "\u2014", format(round(as.numeric(value), 1), nsmall = 1, trim = TRUE)))
  )
}

article_lab_table_footer <- function(n, label = "titles") {
  if (n < 1) return(NULL)
  div(
    class = "lab-table-footer",
    sprintf("Showing 1 to %s of %s %s", n, n, ifelse(identical(n, 1L), sub("s$", "", label), label))
  )
}

article_lab_section_card <- function(title, description, body, count = NULL, footer = NULL) {
  div(
    class = "lab-card lab-section-card",
    div(
      class = "lab-section-header",
      div(
        h3(title),
        p(class = "lab-section-copy", description)
      ),
      if (is.null(count)) NULL else article_lab_count_badge(as.integer(count))
    ),
    body,
    footer
  )
}

article_lab_action_bar <- function(..., align = c("start", "split"), class_name = NULL) {
  align <- match.arg(align)
  div(class = paste(c("lab-actions", paste0("lab-actions-", align), class_name), collapse = " "), ...)
}

article_lab_local_notice <- function(copy) {
  if (is.null(copy) || is.na(copy) || !nzchar(copy)) return(NULL)
  div(class = "lab-inline-notice", copy)
}

article_lab_empty_state <- function(title, copy, next_step = NULL) {
  div(
    class = "empty-state lab-empty-state",
    strong(title),
    p(copy),
    if (is.null(next_step) || is.na(next_step) || !nzchar(next_step)) NULL else p(class = "lab-empty-next", next_step)
  )
}

article_lab_prompt_block <- function(...) {
  div(class = "lab-prompt-block", ...)
}

article_lab_button <- function(input_id, label, class = "lab-secondary", onclick = NULL, disabled = FALSE) {
  button <- actionButton(input_id, label, class = class, onclick = onclick)
  if (isTRUE(disabled)) {
    button <- tagAppendAttributes(button, disabled = "disabled", class = "disabled")
  }
  button
}

article_lab_disabled_select <- function(input_id, label, value = "Not supported") {
  htmltools::tagQuery(selectInput(input_id, label, choices = setNames("__unsupported__", value), selected = "__unsupported__", width = "100%"))$
    find("select")$addAttrs(disabled = "disabled")$allTags()
}

article_lab_generation_control_ui <- function(con, workflow_key, model_choices, default_model, model_label = "Model") {
  spec <- article_lab_generation_workflows[[workflow_key]]
  if (is.null(spec)) stop("Unknown generation workflow: ", workflow_key, call. = FALSE)
  preference <- article_lab_load_generation_preference(con, workflow_key, default_model, spec$default_reasoning, spec$default_mode)
  model <- preference$model
  choices <- article_lab_model_choices_with_default(model, model_choices)
  efforts <- article_lab_reasoning_capabilities(model)
  selected_effort <- preference$reasoning_effort
  if (length(efforts) > 0 && !selected_effort %in% efforts) selected_effort <- spec$default_reasoning
  if (length(efforts) > 0 && !selected_effort %in% efforts) selected_effort <- efforts[[1]]
  mode_supported <- article_lab_supports_pro_mode(model)
  selected_mode <- if (mode_supported && identical(preference$reasoning_mode, "pro")) "pro" else "standard"
  div(
    class = "lab-grid lab-generation-controls",
    div(class = "lab-field", selectInput(spec$model_id, model_label, choices = choices, selected = model, width = "100%")),
    div(class = "lab-field", if (length(efforts) > 0) selectInput(spec$reasoning_id, "Reasoning level", choices = efforts, selected = selected_effort, width = "100%") else article_lab_disabled_select(spec$reasoning_id, "Reasoning level")),
    div(class = "lab-field", if (mode_supported) selectInput(spec$mode_id, "Execution mode", choices = c("Standard" = "standard", "Pro" = "pro"), selected = selected_mode, width = "100%") else article_lab_disabled_select(spec$mode_id, "Execution mode"))
  )
}
