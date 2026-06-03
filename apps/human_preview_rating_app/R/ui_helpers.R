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

article_lab_action_bar <- function(..., align = c("start", "split")) {
  align <- match.arg(align)
  div(class = paste("lab-actions", paste0("lab-actions-", align)), ...)
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
