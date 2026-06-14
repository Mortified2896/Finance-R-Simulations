required_packages <- c("shiny", "DBI", "RSQLite", "jsonlite", "DT")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ", paste(missing_packages, collapse = ", "), "\n\n",
    "Install them in R with:\n",
    'install.packages(c("', paste(missing_packages, collapse = '", "'), '"))',
    call. = FALSE
  )
}

library(shiny)
library(DBI)
library(RSQLite)
library(jsonlite)
library(DT)

app_env <- environment()
source("R/text_helpers.R", local = app_env)
source("R/input_helpers.R", local = app_env)
source("R/file_helpers.R", local = app_env)
source("R/app_config.R", local = app_env)
source("R/db_helpers.R", local = app_env)
source("R/ui_helpers.R", local = app_env)
source("R/ui_assets.R", local = app_env)
source("R/status_helpers.R", local = app_env)
source("R/workflow_helpers.R", local = app_env)
source("R/scoring_helpers.R", local = app_env)
source("R/title_subtitle_helpers.R", local = app_env)
source("R/id_helpers.R", local = app_env)
source("R/article_lab_config.R", local = app_env)
source("R/schema_rating.R", local = app_env)
source("R/schema_article_lab.R", local = app_env)
source("R/schema_research.R", local = app_env)
source("R/schema_startup.R", local = app_env)
source("R/table_helpers.R", local = app_env)
source("R/research_helpers.R", local = app_env)
source("R/api_helpers.R", local = app_env)
source("R/db_article_lab_read_helpers.R", local = app_env)
source("R/db_article_lab_write_helpers.R", local = app_env)
source("R/rating_helpers.R", local = app_env)

initialize_app_database()
ui <- fluidPage(
  tags$head(
    tags$title("Medium Preview Rating"),
    tags$style(article_lab_css()),
    tags$script(article_lab_js())
  ),
  div(
    class = paste("topbar", if (article_lab_design_v2) "topbar-v2" else ""),
    div(class = "brand", div(class = "brand-mark", "M"), div("Medium Preview Rating")),
    div(
      class = "top-actions",
      if (article_lab_design_v2) span(class = "v2-env-pill", "Design v2") else span("Focus mode"),
      span("Local SQLite")
    )
  ),
  div(
    class = paste("app-shell", if (article_lab_design_v2) "app-shell-v2" else ""),
    tags$aside(
      class = "sidebar",
      uiOutput("sidebar_nav"),
      uiOutput("sidebar_status_card")
    ),
    tags$main(
      class = "main",
      uiOutput("main_panel")
    ),
    tags$aside(
      class = "guide",
      uiOutput("guide_content")
    )
  )
)

server <- function(input, output, session) {
  con <- connect_db()
  onStop(function() dbDisconnect(con))
  rating_session_id <- if (is_dimension_mode) NULL else resume_or_create_session(con, target_n = default_target_n)
  active_section <- reactiveVal("home")
  active_dimension <- reactiveVal(if (is_dimension_mode) first_incomplete_dimension(con) else NA_character_)
  current <- reactiveVal(NULL)
  shown_started_at <- reactiveVal(Sys.time())
  saved_article_lab_prompt_key <- reactiveVal(article_lab_manual_prompt_key)
  saved_article_lab_prompt <- reactiveVal(load_article_lab_prompt(con, article_lab_manual_prompt_key))
  saved_article_lab_outline_prompt_key <- reactiveVal(article_lab_outline_prompt_key)
  saved_article_lab_outline_prompt <- reactiveVal(load_article_lab_prompt(con, article_lab_outline_prompt_key, article_lab_default_outline_prompt))
  saved_article_lab_full_text_prompt_key <- reactiveVal(article_lab_full_text_prompt_key)
  saved_article_lab_full_text_prompt <- reactiveVal(load_article_lab_prompt(con, article_lab_full_text_prompt_key, article_lab_default_full_text_prompt))
  article_lab_state <- reactiveValues(
    draft = NULL,
    draft_created_at = NULL,
    draft_meta = NULL,
    is_generating = FALSE,
    is_scoring = FALSE,
    is_generating_subtitles = FALSE,
    is_generating_thumbnails = FALSE,
    thumbnail_generation_started_at = NULL,
    thumbnail_generation_estimate = NULL,
    notice = NULL,
    last_outline_generate_error = NULL,
    last_outline_generate_error_at = NULL,
    last_full_text_generate_error = NULL,
    last_full_text_generate_error_at = NULL,
    last_review_publish_archive_error = NULL,
    last_review_publish_archive_error_at = NULL,
    last_research_paperqa_chunks_error = NULL,
    last_research_paperqa_chunks_error_at = NULL,
    last_research_paperqa_chunks = NULL,
    last_research_paperqa_chunks_mode = NULL,
    last_research_paperqa_answer = NULL,
    last_research_paperqa_chunks_file = NULL
  )
  article_lab_refresh <- reactiveVal(0L)
  article_lab_active_outline_thumbnail <- reactiveVal(NULL)

  observeEvent(input$research_summary_prompt_version, {
    updateTextAreaInput(
      session,
      "research_summary_api_prompt",
      value = load_research_summary_prompt(con, input$research_summary_prompt_version)
    )
  }, ignoreInit = FALSE)

  output$article_lab_prompt_save_button <- renderUI({
    current_prompt <- article_lab_input_multiline(input$article_lab_prompt) %||% article_lab_default_prompt
    saved_prompt <- article_lab_input_multiline(saved_article_lab_prompt()) %||% article_lab_default_prompt
    current_key <- article_lab_input_string(input$article_lab_prompt_key) %||% saved_article_lab_prompt_key()
    saved_key <- saved_article_lab_prompt_key()
    has_changes <- !identical(current_prompt, saved_prompt)
    has_key_changes <- !identical(current_key, saved_key)
    actionButton(
      "article_lab_save_prompt",
      if (has_changes || has_key_changes) "Save prompt" else "Prompt saved",
      class = if (has_changes || has_key_changes) "lab-primary" else "lab-secondary",
      disabled = if (has_changes || has_key_changes) NULL else "disabled"
    )
  })

  output$article_lab_prompt_selector <- renderUI({
    keys <- list_article_lab_prompt_keys(con)
    selected <- saved_article_lab_prompt_key()
    selectInput("article_lab_prompt_key_select", "Saved prompt", choices = keys, selected = selected, width = "100%")
  })

  observeEvent(input$article_lab_prompt_key_select, {
    key <- article_lab_input_string(input$article_lab_prompt_key_select) %||% article_lab_manual_prompt_key
    prompt_text <- load_article_lab_prompt(con, key)
    saved_article_lab_prompt_key(key)
    saved_article_lab_prompt(prompt_text)
    updateTextInput(session, "article_lab_prompt_key", value = key)
    updateTextAreaInput(session, "article_lab_prompt", value = prompt_text)
  }, ignoreInit = TRUE)

  output$article_lab_outline_prompt_selector <- renderUI({
    keys <- list_article_lab_prompt_keys(con, article_lab_outline_prompt_key)
    selected <- saved_article_lab_outline_prompt_key()
    selectInput("article_lab_outline_prompt_key_select", "Saved prompt", choices = keys, selected = selected, width = "100%")
  })

  output$article_lab_outline_prompt_save_button <- renderUI({
    current_prompt <- article_lab_input_multiline(input$article_lab_outline_prompt) %||% article_lab_default_outline_prompt
    saved_prompt <- article_lab_input_multiline(saved_article_lab_outline_prompt()) %||% article_lab_default_outline_prompt
    current_key <- article_lab_input_string(input$article_lab_outline_prompt_key) %||% saved_article_lab_outline_prompt_key()
    saved_key <- saved_article_lab_outline_prompt_key()
    has_changes <- !identical(current_prompt, saved_prompt) || !identical(current_key, saved_key)
    actionButton(
      "article_lab_save_outline_prompt",
      if (has_changes) "Save prompt" else "Prompt saved",
      class = if (has_changes) "lab-primary" else "lab-secondary",
      disabled = if (has_changes) NULL else "disabled"
    )
  })

  output$article_lab_outline_context_notes_ui <- renderUI({
    has_selection <- !is.null(article_lab_active_outline_thumbnail())
    ta <- textAreaInput("article_lab_outline_context_notes", "Outline context notes (optional, included in prompt)", value = isolate(input$article_lab_outline_context_notes %||% ""), width = "100%", height = "68px")
    if (has_selection) ta else htmltools::tagAppendAttributes(ta, disabled = "disabled")
  })

  observeEvent(input$article_lab_outline_prompt_key_select, {
    key <- article_lab_input_string(input$article_lab_outline_prompt_key_select) %||% article_lab_outline_prompt_key
    prompt_text <- load_article_lab_prompt(con, key, article_lab_default_outline_prompt)
    saved_article_lab_outline_prompt_key(key)
    saved_article_lab_outline_prompt(prompt_text)
    updateTextInput(session, "article_lab_outline_prompt_key", value = key)
    updateTextAreaInput(session, "article_lab_outline_prompt", value = prompt_text)
  }, ignoreInit = TRUE)

  output$article_lab_full_text_prompt_selector <- renderUI({
    keys <- list_article_lab_prompt_keys(con, article_lab_full_text_prompt_key)
    selected <- saved_article_lab_full_text_prompt_key()
    selectInput("article_lab_full_text_prompt_key_select", "Saved prompt", choices = keys, selected = selected, width = "100%")
  })

  output$article_lab_full_text_prompt_save_button <- renderUI({
    current_prompt <- article_lab_input_multiline(input$article_lab_full_text_prompt) %||% article_lab_default_full_text_prompt
    saved_prompt <- article_lab_input_multiline(saved_article_lab_full_text_prompt()) %||% article_lab_default_full_text_prompt
    current_key <- article_lab_input_string(input$article_lab_full_text_prompt_key) %||% saved_article_lab_full_text_prompt_key()
    saved_key <- saved_article_lab_full_text_prompt_key()
    has_changes <- !identical(current_prompt, saved_prompt) || !identical(current_key, saved_key)
    actionButton("article_lab_save_full_text_prompt", if (has_changes) "Save prompt" else "Prompt saved", class = if (has_changes) "lab-primary" else "lab-secondary", disabled = if (has_changes) NULL else "disabled")
  })

  observeEvent(input$article_lab_full_text_prompt_key_select, {
    key <- article_lab_input_string(input$article_lab_full_text_prompt_key_select) %||% article_lab_full_text_prompt_key
    prompt_text <- load_article_lab_prompt(con, key, article_lab_default_full_text_prompt)
    saved_article_lab_full_text_prompt_key(key)
    saved_article_lab_full_text_prompt(prompt_text)
    updateTextInput(session, "article_lab_full_text_prompt_key", value = key)
    updateTextAreaInput(session, "article_lab_full_text_prompt", value = prompt_text)
  }, ignoreInit = TRUE)

  observeEvent(input$sidebar_nav, {
    valid_sections <- c("home", article_lab_workflow_sections, "settings")
    if (is.character(input$sidebar_nav) && input$sidebar_nav %in% valid_sections) {
      active_section(input$sidebar_nav)
      if (identical(input$sidebar_nav, "home")) refresh_current()
    }
  }, ignoreInit = TRUE)

  observe({
    session$sendCustomMessage("setWorkflowLayout", active_section())
  })

  refresh_current <- function() {
    item <- if (is_dimension_mode) {
      field <- isolate(active_dimension())
      if (is.na(field)) {
        NULL
      } else {
        loaded_item <- load_current_dimension_item(con, field)
        if (is.null(loaded_item)) {
          next_field <- next_incomplete_dimension_after(con, field)
          if (!is.na(next_field)) {
            active_dimension(next_field)
            loaded_item <- load_current_dimension_item(con, next_field)
          }
        }
        loaded_item
      }
    } else {
      prune_article_lab_candidates_from_session(con, rating_session_id)
      append_article_lab_candidates_to_session(con, rating_session_id)
      load_current_item(con, rating_session_id)
    }
    current(item)
    shown_started_at(Sys.time())
    if (is_dimension_mode) {
      updateTextAreaInput(session, "note", value = "")
    } else {
      updateTextInput(session, "note", value = "")
    }
    session$sendCustomMessage("clearRatingFocus", list())
  }

  refresh_current()

  counts <- reactive({
    invalidateLater(1000, session)
    if (is_dimension_mode) {
      field <- active_dimension()
      if (is.na(field)) {
        data.frame(total = 0L, completed = 0L, pending = 0L, skipped = 0L)
      } else {
        dimension_queue_counts(con, field)
      }
    } else {
      queue_counts(con, rating_session_id)
    }
  })

  candidate_stats <- reactive({
    invalidateLater(5000, session)
    if (is_dimension_mode) dimension_candidate_counts(con) else candidate_counts(con)
  })

  output$sidebar_nav <- renderUI({
    current_section <- active_section()
    nav_button <- function(section, icon, label, subtitle, enabled = TRUE) {
      tags$button(
        type = "button",
        class = paste("nav-item", if (identical(current_section, section)) "active" else ""),
        onclick = if (enabled) sprintf("Shiny.setInputValue('sidebar_nav', '%s', {priority: 'event'})", section) else NULL,
        disabled = if (!enabled) "disabled" else NULL,
        span(class = "nav-icon", icon),
        div(
          class = "nav-copy",
          div(class = "nav-title", label),
          div(class = "nav-subtitle", subtitle)
        )
      )
    }

    tagList(
      div(
        class = "sidebar-nav-group",
        nav_button("home", "\u2302", "Home", "Current rating workflow")
      ),
      div(
        class = "sidebar-nav-group",
        div(class = "sidebar-nav-label", "Article Lab"),
        nav_button("research_inbox", "R", "Research Inbox", "Track papers and article angles"),
        nav_button("summary", "S", "Summary", "Check paper summary"),
        nav_button("generate", "\u21bb", "Generate", "Generate & triage titles"),
        nav_button("api_scoring", "\u2699", "API Scoring", "Score with API & approve"),
        nav_button("subtitle_generation", "\u270d", "Subtitle Generation", "Generate subtitles"),
        nav_button("thumbnails", "\u25a7", "Thumbnails", "Generate thumbnails"),
        nav_button("outline", "\u2263", "Outline", "Create article outline"),
        nav_button("full_text", "\u270e", "Full Text", "Write full article"),
        nav_button("review_publish", "\u2611", "Review & Publish", "Review and publish")
      ),
      div(
        class = "sidebar-nav-group",
        nav_button("settings", "\u2699", "Settings", "App settings")
      )
    )
  })

  output$sidebar_status_card <- renderUI({
    if (article_lab_is_workflow_section(active_section()) || identical(active_section(), "settings")) {
      return(div(
        class = "daily-goal static-card",
        div(
          class = "article-lab-helper",
          strong("Article Lab helper"),
          p("Follow each step in order."),
          p(class = "shortcut-copy", "Manually approve at key stages.")
        )
      ))
    }

    div(
      class = "daily-goal",
      strong("Daily goal"),
      htmlOutput("sidebar_progress"),
      uiOutput("progress_bar"),
      uiOutput("sidebar_shortcuts")
    )
  })

  output$main_panel <- renderUI({
    current_section <- active_section()
    if (article_lab_is_workflow_section(current_section) || identical(current_section, "settings")) {
      page_meta <- article_lab_nav_meta(current_section)
      generate_has_rows <- {
        saved_rows <- article_lab_generate_candidates()
        draft_rows <- article_lab_state$draft
        nrow(saved_rows) > 0 || (!is.null(draft_rows) && nrow(draft_rows) > 0)
      }
      generate_prompt_card <- div(
        class = "lab-card lab-setup-card",
        h2("Generation prompt"),
        div(
          class = "lab-grid",
          div(class = "lab-field", uiOutput("article_lab_prompt_selector")),
          div(class = "lab-field", textInput("article_lab_prompt_key", "Prompt key", value = article_lab_manual_prompt_key, width = "100%"))
        ),
        div(
          class = "lab-field",
          textAreaInput(
            "article_lab_prompt",
            label = "Manual/default prompt",
            value = saved_article_lab_prompt(),
            width = "100%",
            height = if (article_lab_design_v2) "170px" else "230px"
          )
        ),
        div(class = "lab-actions", uiOutput("article_lab_prompt_save_button")),
        div(
          class = "lab-grid",
          div(
            class = "lab-field",
            uiOutput("article_lab_research_summary_selector")
          ),
          div(
            class = "lab-field",
            numericInput("article_lab_batch_size", "Batch size", value = 12L, min = 1L, max = 25L, width = "100%")
          ),
          div(
            class = "lab-field",
            selectInput("article_lab_model", "Model", choices = article_lab_title_generation_model_choices, selected = article_lab_default_model, width = "100%")
          ),
          div(
            class = "lab-field",
            textInput("article_lab_seed_topic", "Optional seed/topic (manual mode)", value = "", width = "100%", placeholder = "Optional article idea or angle")
          ),
          div(
            class = "lab-field",
            selectInput(
              "article_lab_inspiration_source",
              "Optional inspiration source (manual mode)",
              choices = c("", "manual prompt", "top performing titles", "custom"),
              selected = "",
              width = "100%"
            )
          )
        ),
        div(
          class = "lab-field",
          textAreaInput(
            "article_lab_context_notes",
            "Article context notes (optional, not saved to general prompt)",
            value = "",
            width = "100%",
            height = if (article_lab_design_v2) "60px" else "80px",
            placeholder = "Specific context for this article batch, e.g. target audience, tone, key angle"
          )
        ),
        uiOutput("article_lab_effective_prompt"),
        article_lab_action_bar(
          uiOutput("article_lab_generate_button"),
          actionButton("article_lab_save", "Save batch", class = "lab-secondary"),
          actionButton("article_lab_clear", "Clear draft", class = "lab-secondary")
        ),
        div(
          class = "lab-field",
          textAreaInput(
            "article_lab_manual_titles",
            "Add title ideas manually",
            value = "",
            width = "100%",
            height = if (article_lab_design_v2) "86px" else "120px",
            placeholder = "Enter one title idea per line"
          )
        ),
        article_lab_action_bar(
          actionButton("article_lab_add_manual_titles", "Add manual titles", class = "lab-secondary")
        ),
        uiOutput("article_lab_notice")
      )
      generate_triage_card <- div(
        class = "lab-card lab-primary-work-card",
        div(
          class = "lab-section-header",
          div(
            h3("Current batch triage"),
            p(class = "lab-section-copy", "Select promising titles, disqualify weak ones, and move ready candidates to API scoring.")
          ),
          if (article_lab_design_v2) article_lab_count_badge(nrow(article_lab_generate_candidates()), "saved titles") else NULL
        ),
        article_lab_action_bar(
          checkboxInput("article_lab_generate_select_all", "Select all", value = FALSE),
          checkboxInput("article_lab_show_disqualified", "Show disqualified titles", value = FALSE),
          article_lab_button("article_lab_save_triage", "Save triage changes", class = "lab-secondary", disabled = !generate_has_rows),
          article_lab_button("article_lab_move_to_api_queue", "Move selected to API queue", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_generate');", disabled = !generate_has_rows)
        ),
        uiOutput("article_lab_latest_titles")
      )
      generate_panel <- if (article_lab_design_v2) {
        tagList(
          div(
            class = "lab-workflow-toolbar",
            div(
              div(class = "lab-workflow-eyebrow", "Active workspace"),
              div(class = "lab-workflow-title", "Triage titles before API scoring")
            ),
            div(class = "lab-workflow-hint", "Prompt setup is available below the table.")
          ),
          generate_triage_card,
          tags$details(
            class = "lab-card lab-collapsible-setup",
            tags$summary("Prompt, model, and manual title setup"),
            generate_prompt_card
          )
        )
      } else {
        tagList(
          div(
            class = "lab-card",
          h2("Generation prompt"),
          div(
            class = "lab-grid",
            div(class = "lab-field", uiOutput("article_lab_prompt_selector")),
            div(class = "lab-field", textInput("article_lab_prompt_key", "Prompt key", value = article_lab_manual_prompt_key, width = "100%"))
          ),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_prompt",
              label = "Manual/default prompt",
              value = saved_article_lab_prompt(),
              width = "100%",
              height = "230px"
            )
          ),
          div(class = "lab-actions", uiOutput("article_lab_prompt_save_button")),
          div(
            class = "lab-grid",
            div(
              class = "lab-field",
              uiOutput("article_lab_research_summary_selector")
            ),
            div(
              class = "lab-field",
              numericInput("article_lab_batch_size", "Batch size", value = 12L, min = 1L, max = 25L, width = "100%")
            ),
            div(
              class = "lab-field",
              selectInput("article_lab_model", "Model", choices = article_lab_title_generation_model_choices, selected = article_lab_default_model, width = "100%")
            ),
            div(
              class = "lab-field",
              textInput("article_lab_seed_topic", "Optional seed/topic (manual mode)", value = "", width = "100%", placeholder = "Optional article idea or angle")
            ),
          div(
            class = "lab-field",
            selectInput(
              "article_lab_inspiration_source",
              "Optional inspiration source (manual mode)",
              choices = c("", "manual prompt", "top performing titles", "custom"),
              selected = "",
              width = "100%"
            )
          )
        ),
        div(
          class = "lab-field",
          textAreaInput(
            "article_lab_context_notes",
            "Article context notes (optional, not saved to general prompt)",
            value = "",
            width = "100%",
            height = "80px",
            placeholder = "Specific context for this article batch, e.g. target audience, tone, key angle"
          )
        ),
        uiOutput("article_lab_effective_prompt"),
        article_lab_action_bar(
          uiOutput("article_lab_generate_button"),
          actionButton("article_lab_save", "Save batch", class = "lab-secondary"),
          actionButton("article_lab_clear", "Clear draft", class = "lab-secondary")
        ),
        div(
          class = "lab-field",
          textAreaInput(
            "article_lab_manual_titles",
            "Add title ideas manually",
            value = "",
            width = "100%",
            height = "120px",
            placeholder = "Enter one title idea per line"
          )
        ),
        article_lab_action_bar(
          actionButton("article_lab_add_manual_titles", "Add manual titles", class = "lab-secondary")
        ),
        uiOutput("article_lab_notice")
      ),
      div(
        class = "lab-card",
          h3("Current batch triage"),
          article_lab_action_bar(
            checkboxInput("article_lab_generate_select_all", "Select all", value = FALSE),
            checkboxInput("article_lab_show_disqualified", "Show disqualified titles", value = FALSE),
            article_lab_button("article_lab_save_triage", "Save triage changes", class = "lab-secondary", disabled = !generate_has_rows),
            article_lab_button("article_lab_move_to_api_queue", "Move selected to API queue", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_generate');", disabled = !generate_has_rows)
          ),
          uiOutput("article_lab_latest_titles")
        )
        )
      }

      api_score_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(class = "lab-grid", uiOutput("article_lab_batch_selector"), div(class = "lab-field", selectInput("article_lab_score_model", "Model", choices = article_lab_score_model_choices, selected = article_lab_default_score_model, width = "100%")), div(class = "lab-field", textInput("article_lab_score_prompt_version", "Prompt version", value = article_lab_default_score_prompt_version, width = "100%")), div(class = "lab-field", textInput("article_lab_score_scope", "Scope", value = article_lab_default_score_scope, width = "100%"))),
          article_lab_action_bar(
            uiOutput("article_lab_score_button"),
            actionButton("article_lab_refresh_scores", "Refresh", class = "lab-secondary")
          ),
          uiOutput("article_lab_score_effective_prompt"),
          div(class = "lab-status-copy", "Only titles in the API queue are scored."),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_score_sections")
      )

      subtitle_generation_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_subtitle_prompt",
              "Prompt",
              value = article_lab_default_subtitle_prompt,
              width = "100%",
              height = "190px"
            )
          ),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_subtitle_model", "Model", choices = article_lab_subtitle_model_choices, selected = article_lab_default_subtitle_model, width = "100%")),
            div(class = "lab-field", numericInput("article_lab_subtitle_variants_per_title", "Subtitle candidates per title", value = 4L, min = 1L, max = 8L, width = "100%"))
          ),
          article_lab_action_bar(
            uiOutput("article_lab_subtitle_generate_button"),
            actionButton("article_lab_refresh_subtitles", "Refresh", class = "lab-secondary")
          ),
          uiOutput("article_lab_subtitle_effective_prompt"),
          tags$hr(class = "lab-divider"),
          div(
            class = "lab-grid",
            div(class = "lab-field", selectizeInput("article_lab_manual_subtitle_candidate_id", "Add manual subtitle for title", choices = character(), selected = NULL, width = "100%")),
            div(
              class = "lab-field",
              textAreaInput(
                "article_lab_manual_subtitle_text",
                "Manual subtitle idea(s)",
                value = "",
                width = "100%",
                height = "110px",
                placeholder = "Enter one subtitle idea per line"
              )
            )
          ),
          article_lab_action_bar(
            actionButton("article_lab_add_manual_subtitles", "Add manual subtitle idea(s)", class = "lab-secondary")
          ),
          div(class = "lab-status-copy", "Generate subtitle variants for approved titles, then approve or reject candidates manually."),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_subtitle_sections")
      )

      thumbnail_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-field",
            textAreaInput(
              "article_lab_thumbnail_prompt",
              "Prompt",
              value = article_lab_default_thumbnail_prompt,
              width = "100%",
              height = "170px"
            )
          ),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_thumbnail_model", "Responses generation model", choices = article_lab_thumbnail_model_choices, selected = article_lab_default_thumbnail_model, width = "100%")),
            div(class = "lab-field", numericInput("article_lab_thumbnail_variants_per_package", "Thumbnail candidates per package", value = article_lab_default_thumbnail_variants, min = 1L, max = 4L, width = "100%"))
          ),
          article_lab_action_bar(
            uiOutput("article_lab_thumbnail_generate_button"),
            actionButton("article_lab_refresh_thumbnails", "Refresh", class = "lab-secondary")
          ),
          uiOutput("article_lab_thumbnail_effective_prompt"),
          div(id = "article_lab_thumbnail_timer", class = "lab-status-copy"),
          div(class = "lab-status-copy", "Generate thumbnail candidates for approved title/subtitle packages, then approve one preview card per package."),
          uiOutput("article_lab_notice")
        )
        ,
        uiOutput("article_lab_thumbnail_sections")
      )

      outline_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-grid",
            div(class = "lab-field", uiOutput("article_lab_outline_prompt_selector")),
            div(class = "lab-field", textInput("article_lab_outline_prompt_key", "Prompt key", value = article_lab_outline_prompt_key, width = "100%"))
          ),
          div(
            class = "lab-field",
            textAreaInput("article_lab_outline_prompt", "Prompt", value = saved_article_lab_outline_prompt(), width = "100%", height = "150px")
          ),
          div(class = "lab-actions", uiOutput("article_lab_outline_prompt_save_button")),
          div(
            class = "lab-field",
            uiOutput("article_lab_outline_context_notes_ui"),
            div(class = "lab-actions", actionButton("article_lab_save_outline_context_notes", "Save context notes", class = "lab-secondary"))
          ),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_outline_model", "Model", choices = article_lab_outline_model_choices, selected = article_lab_default_outline_model, width = "100%")),
            uiOutput("article_lab_outline_context_toggle")
          ),
          article_lab_action_bar(
            actionButton("article_lab_generate_outlines", "Generate selected outline(s)", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_outline_packages');"),
            actionButton("article_lab_save_outlines", "Save outline edits", class = "lab-secondary"),
            actionButton("article_lab_approve_outlines", "Approve selected outline(s)", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_outline_candidates');"),
            actionButton("article_lab_archive_outlines", "Archive selected package(s)", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_outline_archive_packages');"),
            actionButton("article_lab_refresh_outlines", "Refresh", class = "lab-secondary")
          ),
          div(class = "lab-status-copy", "Generate an outline from approved packages, edit/review it here, then approve it to move the package to draft-ready."),
          uiOutput("article_lab_outline_generate_error"),
          uiOutput("article_lab_outline_effective_prompt"),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_outline_sections")
      )

      full_text_panel <- tagList(
        div(
          class = "lab-card",
          h2("Controls"),
          div(
            class = "lab-grid",
            div(class = "lab-field", uiOutput("article_lab_full_text_prompt_selector")),
            div(class = "lab-field", textInput("article_lab_full_text_prompt_key", "Prompt key", value = article_lab_full_text_prompt_key, width = "100%"))
          ),
          div(
            class = "lab-field",
            textAreaInput("article_lab_full_text_prompt", "Prompt", value = saved_article_lab_full_text_prompt(), width = "100%", height = "170px")
          ),
          div(class = "lab-actions", uiOutput("article_lab_full_text_prompt_save_button")),
          div(
            class = "lab-grid",
            uiOutput("article_lab_batch_selector"),
            div(class = "lab-field", selectInput("article_lab_full_text_model", "Model", choices = article_lab_full_text_model_choices, selected = article_lab_default_full_text_model, width = "100%")),
            div(class = "lab-field", checkboxInput("article_lab_full_text_include_context", "Include source PDF and checked evidence (indirect citations enforced)", value = TRUE, width = "100%"))
          ),
          article_lab_action_bar(
            actionButton("article_lab_generate_full_text", "Generate full article draft", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_full_text_packages');"),
            actionButton("article_lab_generate_full_text_variant", "Generate another variant", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_full_text_packages');"),
            actionButton("article_lab_regenerate_full_text_draft", "Regenerate selected draft", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_full_text_drafts');"),
            actionButton("article_lab_save_full_text_drafts", "Save draft edits", class = "lab-secondary"),
            actionButton("article_lab_approve_full_text_draft", "Approve selected draft", class = "lab-secondary", onclick = "window.articleLabSyncSelections('article_lab_full_text_drafts');"),
            actionButton("article_lab_reject_full_text_draft", "Reject selected draft", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_full_text_drafts');"),
            actionButton("article_lab_refresh_full_text", "Refresh", class = "lab-secondary"),
            class_name = "lab-actions-full-text"
          ),
          div(class = "lab-status-copy", "Generate drafts from approved outlines, edit the selected draft directly, save revisions, then approve one draft for Review & Publish."),
          uiOutput("article_lab_full_text_generate_error"),
          uiOutput("article_lab_full_text_effective_prompt"),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_full_text_sections")
      )

      review_publish_panel <- tagList(
        div(
          class = "lab-card",
          h2("Review & Publish"),
          div(class = "lab-status-copy", "Approved full article drafts appear here for local publishing metadata, copy/export, and manual status tracking. The article text is read-only in this tab."),
          uiOutput("article_lab_review_publish_selector"),
          uiOutput("article_lab_notice")
        ),
        uiOutput("article_lab_review_publish_workspace")
      )

      placeholder_panel <- function(copy) {
        div(
          class = "lab-card step-placeholder",
          p(copy),
          p(class = "shortcut-copy", "This step is present in the workflow navigation, but its deeper implementation is intentionally left untouched in this pass.")
        )
      }

      research_inbox_panel <- tagList(
        div(
          class = "lab-card",
          h2("Ranked Queue"),
          div(class = "lab-status-copy", "Ranked sources have a manual sort order. Finished and archived sources are hidden unless that status is selected."),
          div(class = "lab-grid", div(class = "lab-field", selectInput("research_source_status_filter", "Filter by status", choices = c("All" = "__all__", "new", "reading", "angle_ready", "used", "archived"), selected = "__all__", width = "100%"))),
          div(class = "lab-actions", actionButton("research_refresh", "Refresh", class = "lab-secondary"), actionButton("research_ranked_move_up", "Move selected up", class = "lab-secondary"), actionButton("research_ranked_move_down", "Move selected down", class = "lab-secondary"), actionButton("research_remove_from_ranked", "Remove selected from ranked queue", class = "lab-secondary")),
          DT::DTOutput("research_ranked_sources_table")
        ),
        div(
          class = "lab-card",
          h2("Selected Source / Angle Workspace"),
          uiOutput("research_selected_source_summary"),
          uiOutput("research_angle_workspace")
        ),
        div(
          class = "lab-card",
          h2("Unranked Sources"),
          div(class = "lab-status-copy", "Unranked sources have no manual sort order."),
          div(class = "lab-actions", actionButton("research_add_to_ranked", "Add selected to ranked queue", class = "lab-primary")),
          DT::DTOutput("research_unranked_sources_table")
        ),
        div(
          class = "lab-card",
          h3("New source"),
          div(class = "lab-grid", div(class = "lab-field", textInput("research_new_source_title", "Source title", width = "100%")), div(class = "lab-field", textInput("research_new_source_url", "Source URL", width = "100%")), div(class = "lab-field", textInput("research_new_pdf_url", "PDF URL", width = "100%")), div(class = "lab-field", numericInput("research_new_source_sort", "Sort order", value = NULL, width = "100%"))),
          div(class = "lab-field", textAreaInput("research_new_source_main_idea", "Main idea", width = "100%", height = "90px")),
          div(class = "lab-field", textAreaInput("research_new_source_abstract", "Abstract", width = "100%", height = "90px")),
          div(class = "lab-grid", div(class = "lab-field", textInput("research_new_source_status", "Status", value = "new", width = "100%")), div(class = "lab-field", textInput("research_new_source_name", "Source name", value = "", width = "100%"))),
          div(class = "lab-field", textAreaInput("research_new_source_notes", "Notes", width = "100%", height = "80px")),
          div(class = "lab-actions", actionButton("research_add_source", "Add source", class = "lab-primary"))
        ),
        uiOutput("article_lab_notice")
      )

      summary_panel <- tagList(
        div(
          class = "lab-card",
          h2("Research Summary"),
          div(class = "lab-field", uiOutput("research_summary_source_selector")),
          uiOutput("research_summary_selected_source"),
          uiOutput("research_summary_pdf_status"),
          div(
            class = "lab-actions",
            actionButton("research_download_pdf", "Download PDF", class = "lab-secondary"),
            actionButton("research_clear_pdf", "Clear/replace PDF", class = "lab-secondary")
          ),
          div(class = "lab-field", fileInput("research_pdf_upload", "Upload PDF manually", accept = c(".pdf", "application/pdf"), width = "100%")),
          uiOutput("research_summary_pdf_gate"),
          div(
            class = "lab-card",
            h3("API summary generation"),
            div(
              class = "lab-grid",
              div(class = "lab-field", selectInput("research_summary_model", "Model", choices = article_lab_research_summary_model_choices, selected = article_lab_default_research_summary_model, width = "100%")),
              div(class = "lab-field", selectInput("research_summary_prompt_version", "Prompt version", choices = article_lab_research_summary_prompt_version_choices, selected = article_lab_default_research_summary_prompt_version, width = "100%"))
            ),
            div(class = "lab-field lab-editor-textarea", textAreaInput("research_summary_api_prompt", "API prompt", value = article_lab_default_research_summary_prompt, width = "100%", height = "260px")),
            uiOutput("research_summary_effective_prompt"),
            div(class = "lab-actions", actionButton("research_generate_summary_draft", "Generate summary draft", class = "lab-primary"))
          ),
          div(
            class = "lab-field lab-editor-textarea",
            textAreaInput("research_summary_text", "Summary text", value = research_summary_template, width = "100%", height = "620px")
          ),
          div(
            class = "lab-actions",
            actionButton("research_save_summary_draft", "Save summary draft", class = "lab-secondary"),
            actionButton("research_confirm_summary", "Mark summary confirmed", class = "lab-primary"),
            actionButton("research_send_summary_to_generate", "Send confirmed summary to Generate", class = "lab-secondary")
          ),
          div(
            class = "lab-card",
            h3("Evidence markers"),
            div(
              class = "lab-grid",
              div(class = "lab-field", numericInput("research_evidence_max_claims", "Max claims", value = 6L, min = 1L, max = 25L, width = "100%")),
              div(class = "lab-field", numericInput("research_evidence_candidates_per_claim", "Candidate sentences per claim", value = 12L, min = 3L, max = 20L, width = "100%"))
            ),
            h4("Claim sentence marking"),
            div(
              class = "lab-grid",
              div(class = "lab-field", selectInput("research_claim_model", "Model", choices = article_lab_claim_extraction_model_choices, selected = article_lab_default_claim_extraction_model, width = "100%")),
              div(class = "lab-field", selectInput("research_claim_reasoning_effort", "Reasoning effort", choices = article_lab_evidence_reasoning_choices, selected = article_lab_default_evidence_reasoning_effort, width = "100%"))
            ),
            div(class = "lab-field lab-editor-textarea", textAreaInput("research_claim_prompt", "Claim sentence marking prompt", value = article_lab_default_claim_extraction_prompt, width = "100%", height = "210px")),
            h4("Evidence sentence selection"),
            div(
              class = "lab-grid",
              div(class = "lab-field", selectInput("research_evidence_model", "Model", choices = article_lab_evidence_selection_model_choices, selected = article_lab_default_evidence_selection_model, width = "100%")),
              div(class = "lab-field", selectInput("research_evidence_reasoning_effort", "Reasoning effort", choices = article_lab_evidence_reasoning_choices, selected = article_lab_default_evidence_reasoning_effort, width = "100%")),
              div(class = "lab-field", selectInput("research_evidence_fallback_model", "Fallback model", choices = article_lab_evidence_fallback_model_choices, selected = article_lab_default_evidence_fallback_model, width = "100%")),
              div(class = "lab-field", selectInput("research_evidence_fallback_reasoning_effort", "Fallback reasoning", choices = article_lab_evidence_reasoning_choices, selected = "medium", width = "100%"))
            ),
            div(class = "lab-field lab-editor-textarea", textAreaInput("research_evidence_prompt", "Evidence selection prompt", value = article_lab_default_evidence_selection_prompt, width = "100%", height = "210px")),
            uiOutput("research_evidence_effective_prompts"),
            div(
              class = "lab-actions",
              actionButton("research_extract_pdf_sentences", "Extract PDF sentences", class = "lab-secondary"),
              actionButton("research_mark_claim_sentences", "Mark claim sentences", class = "lab-primary"),
              actionButton("research_find_source_sentences", "Find source sentences", class = "lab-primary"),
              actionButton("research_rerun_weak_evidence", "Rerun weak/no-match with fallback", class = "lab-secondary")
            )
          ),
          div(
            class = "lab-card",
            h3("PaperQA2 retrieval"),
            div(class = "lab-status-copy", "Enter a claim or question to retrieve relevant evidence candidates from the selected PDF."),
            uiOutput("research_paperqa_chunks_error"),
            div(class = "lab-field lab-editor-textarea", textAreaInput("research_paperqa_query", "Claim / question", value = "", width = "100%", height = "100px", placeholder = "e.g. What is the main contribution of this paper?")),
            div(
              class = "lab-grid",
              div(class = "lab-field", numericInput("research_paperqa_chunk_chars", "Chunk target chars", value = 1500L, min = 500L, max = 5000L, step = 100L, width = "100%")),
              div(class = "lab-field", numericInput("research_paperqa_chunk_overlap", "Chunk overlap", value = 100L, min = 0L, max = 500L, step = 50L, width = "100%"))
            ),
            div(
              class = "lab-actions",
              actionButton("research_run_paperqa_chunks", "Run PaperQA2 retrieval", class = "lab-primary")
            ),
            uiOutput("research_paperqa_chunks_display")
          )
        ),
        uiOutput("research_summary_inline_evidence"),
        uiOutput("research_summary_evidence_table"),
        uiOutput("article_lab_notice")
      )

      page_body <- switch(
        current_section,
        research_inbox = research_inbox_panel,
        summary = summary_panel,
        generate = generate_panel,
        api_scoring = api_score_panel,
        subtitle_generation = subtitle_generation_panel,
        thumbnails = thumbnail_panel,
        outline = outline_panel,
        full_text = full_text_panel,
        review_publish = review_publish_panel,
        settings = placeholder_panel("Settings remain available from the sidebar."),
        generate_panel
      )

      return(tagList(
        div(
          class = "page-header",
          div(
            h1(page_meta$title %||% page_meta$nav_title),
            div(class = "page-subtitle", page_meta$subtitle %||% page_meta$nav_subtitle)
          ),
          if (article_lab_design_v2) div(class = "page-version-badge", "UI v2 experiment") else NULL
        ),
        page_body,
        if (identical(current_section, "summary")) uiOutput("research_summary_evidence_overlay") else NULL
      ))
    }

    tagList(
      h1("Medium Preview Rating"),
      htmlOutput("progress_line"),
      htmlOutput("mode_line"),
      uiOutput("v2_paused_warning"),
      uiOutput("v2_debug_banner"),
      div(class = "tabs", div(class = "tab active", "For you"), div(class = "tab", "Featured")),
      uiOutput("article_area"),
      uiOutput("rating_panel")
    )
  })

  output$progress_line <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    if (is_dimension_mode) {
      total <- ifelse(is.na(c$total[[1]]), 0, c$total[[1]])
      field <- active_dimension()
      if (is.na(field)) {
        HTML(sprintf("All <span class='current'>%s</span> active dimension passes complete", length(active_dimension_fields)))
      } else if (completed >= total && total > 0) {
        HTML(sprintf("Dimension complete: <span class='current'>%s</span> · %s / %s", dimension_labels[[field]], completed, total))
      } else {
        HTML(sprintf("Dimension progress: <span class='current'>%s</span> / %s", completed + 1L, total))
      }
    } else {
      remaining <- stats$remaining_unrated[[1]]
      total <- completed + remaining
      HTML(sprintf("Article <span class='current'>%s</span> / %s", completed + 1L, total))
    }
  })

  output$mode_line <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    pending <- ifelse(is.na(c$pending[[1]]), 0, c$pending[[1]])
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    if (is_dimension_mode) {
      field <- active_dimension()
      active_label <- if (is.na(field)) "all complete" else field
      if (is_dimension_v2_mode) {
        return(HTML(sprintf(
          "<div class='mode-line'><strong>Mode:</strong> %s · active dimension %s · cohort rows %s · active dimensions %s · dimension progress %s done / %s pending · overall manual ratings %s / %s complete</div>",
          rating_mode,
          active_label,
          stats$total_cohort_rows[[1]],
          stats$total_dimensions[[1]],
          completed,
          pending,
          stats$completed_dimensions[[1]],
          stats$total_dimensions[[1]]
        )))
      }
      HTML(sprintf(
        "<div class='mode-line'><strong>Mode:</strong> %s · active dimension %s · cohort rows %s · usable local thumbnails %s · dimension progress %s done / %s pending · overall %s / %s dimensions complete</div>",
        rating_mode,
        active_label,
        stats$total_cohort_rows[[1]],
        stats$usable_local_thumbnails[[1]],
        completed,
        pending,
        stats$completed_dimensions[[1]],
        stats$total_dimensions[[1]]
      ))
    } else {
      HTML(sprintf(
        "<div class='mode-line'><strong>Mode:</strong> unrated thumbnails only · thumbnail candidates %s · already rated %s · remaining unrated %s · session %s done / %s pending</div>",
        stats$total_thumbnail_candidates[[1]],
        stats$already_rated[[1]],
        stats$remaining_unrated[[1]],
        completed,
        pending
      ))
    }
  })

  output$v2_paused_warning <- renderUI({
    if (!is_dimension_v2_mode) return(NULL)
    NULL
  })

  output$v2_debug_banner <- renderUI({
    if (!is_dimension_v2_mode) return(NULL)
    item <- current()
    field <- active_dimension()
    c <- counts()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- ifelse(is.na(c$total[[1]]), 0, c$total[[1]])
    if (is.na(field) || (total > 0 && completed >= total)) return(NULL)
    if (is.null(item)) {
      return(div(
        class = "v2-debug-banner error",
        strong("dimensions_v2 manifest render debug: "),
        "no current manifest item"
      ))
    }
    info <- v2_render_info(item)
    local_basename <- basename(first_value(item, "local_thumbnail_path_abs", first_value(item, "local_thumbnail_path")))
    short_hash <- function(x) {
      value <- clean_text(x)
      if (length(value) == 0 || is.na(value[[1]])) return("NA")
      substr(value[[1]], 1, 12)
    }
    div(
      class = paste("v2-debug-banner", if (isTRUE(info$valid)) "" else "error"),
      strong("dimensions_v2 manifest render debug: "),
      paste0(
        "queue_position=", first_value(item, "queue_position"),
        " | article_id=", first_value(item, "article_id"),
        " | medium_post_id=", first_value(item, "medium_post_id"),
        " | canonical_article_key=", first_value(item, "canonical_article_key"),
        " | image=", local_basename,
        " | thumbnail_status=", first_value(item, "thumbnail_status"),
        " | hash_matches_manifest=", first_value(item, "hash_matches_manifest"),
        " | image_sha256=", short_hash(first_value(item, "image_sha256")),
        " | current_image_sha256=", short_hash(first_value(item, "current_image_sha256")),
        " | render_valid=", isTRUE(info$valid),
        " | render_reason=", info$reason,
        " | active_dimension=", field
      )
    )
  })

  output$sidebar_progress <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- if (is_dimension_mode) {
      ifelse(is.na(c$total[[1]]), 0, c$total[[1]])
    } else {
      completed + stats$remaining_unrated[[1]]
    }
    HTML(sprintf("<span class='num'>%s</span> / %s", completed, total))
  })

  output$progress_bar <- renderUI({
    c <- counts()
    stats <- candidate_stats()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- if (is_dimension_mode) {
      max(1, ifelse(is.na(c$total[[1]]), 0, c$total[[1]]))
    } else {
      max(1, completed + stats$remaining_unrated[[1]])
    }
    div(
      class = "progress-track",
      div(class = "progress-fill", style = sprintf("width: %.1f%%;", 100 * completed / total))
    )
  })

  output$sidebar_shortcuts <- renderUI({
    if (is_dimension_mode) {
      field <- active_dimension()
      text <- if (!is.na(field) && field == "ai_low_effort_flag") {
        "A/S/J flag, Space skip, U undo"
      } else {
        "A/S/D/F/J rate, Space skip, U undo"
      }
      div(class = "shortcut-copy", text)
    } else {
      div(class = "shortcut-copy", "A/S/D/F/J rate, Space skip, U undo")
    }
  })

  article_lab_saved_batch <- reactive({
    article_lab_refresh()
    load_latest_article_lab_batch(con)
  })

  article_lab_batches <- reactive({
    article_lab_refresh()
    load_article_lab_batches(con)
  })

  observe({
    batches <- article_lab_batches()
    batch_choices <- if (nrow(batches) == 0) character() else setNames(
      batches$batch_id,
      sprintf("%s · %s", batches$batch_id, batches$created_at)
    )
    choices <- c("All batches" = article_lab_all_batches_value, batch_choices)
    selected <- isolate(input$article_lab_selected_batch)
    valid_values <- c(article_lab_all_batches_value, batches$batch_id)
    if (is.null(selected) || !nzchar(selected) || !(selected %in% valid_values)) {
      selected <- article_lab_all_batches_value
    }
    updateSelectInput(session, "article_lab_selected_batch", choices = choices, selected = selected)
  })

  article_lab_selected_batch_id <- reactive({
    selected <- clean_text(input$article_lab_selected_batch)
    if (length(selected) > 0 && !is.na(selected[[1]])) return(selected[[1]])
    batch <- article_lab_saved_batch()
    if (is.null(batch)) return(NA_character_)
    batch$batch_id[[1]]
  })

  article_lab_selected_batch_candidates <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_candidates_for_batch(con, batch_id)
    article_lab_normalize_candidate_rows(rows)
  })

  article_lab_generate_candidates <- reactive({
    article_lab_refresh()
    batch <- article_lab_saved_batch()
    if (is.null(batch) || nrow(batch) == 0) return(data.frame())
    rows <- load_article_lab_candidates_for_batch(con, batch$batch_id[[1]])
    rows <- article_lab_normalize_candidate_rows(rows)
    show_disqualified <- isTRUE(input$article_lab_show_disqualified %||% FALSE)
    keep_statuses <- if (show_disqualified) c("generated", "disqualified") else "generated"
    rows <- rows[rows$normalized_status %in% keep_statuses, , drop = FALSE]
    rows[order(rows$created_at, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_overview_stats <- reactive({
    article_lab_refresh()
    article_lab_overview(con)
  })

  article_lab_scoring_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_scoring_rows(
      con,
      batch_id = batch_id,
      model = input$article_lab_score_model %||% article_lab_default_score_model,
      prompt_version = input$article_lab_score_prompt_version %||% article_lab_default_score_prompt_version,
      scope = input$article_lab_score_scope %||% article_lab_default_score_scope
    )
    rows <- article_lab_normalize_candidate_rows(rows)
    rows <- rows[rows$normalized_status %in% c("ready_for_api_scoring", "api_pending", "api_scored"), , drop = FALSE]
    rows[order(rows$created_at, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_queue_rows <- reactive({
    rows <- article_lab_scoring_rows()
    rows[rows$normalized_status %in% c("ready_for_api_scoring", "api_pending"), , drop = FALSE]
  })

  article_lab_scored_rows <- reactive({
    rows <- article_lab_scoring_rows()
    rows <- rows[rows$normalized_status == "api_scored", , drop = FALSE]
    if (nrow(rows) == 0) return(rows)
    combined_scores <- suppressWarnings(as.numeric(rows$combined_title_score))
    combined_scores[is.na(combined_scores)] <- -Inf
    rows[order(combined_scores, rows$created_at, rows$candidate_id, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  })

  article_lab_subtitle_target_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_subtitle_targets(con, batch_id)
    rows <- article_lab_normalize_candidate_rows(rows)
    rows <- rows[
      rows$normalized_status == "approved_for_subtitle" &
        suppressWarnings(as.integer(rows$generated_subtitle_n)) <= 0 &
        suppressWarnings(as.integer(rows$approved_subtitle_n)) <= 0,
      ,
      drop = FALSE
    ]
    rows[order(rows$created_at, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_pending_subtitle_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_subtitle_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows <- article_lab_normalize_candidate_rows(
      within(rows, {
        status <- parent_status
      })
    )
    rows <- rows[
      rows$subtitle_status == "generated" &
        rows$normalized_status %in% c("approved_for_subtitle", "ready_for_thumbnail"),
      ,
      drop = FALSE
    ]
    title_sort <- tolower(ifelse(is.na(rows$title), "", rows$title))
    created_sort <- xtfrm(rows$created_at)
    subtitle_sort <- xtfrm(rows$subtitle_id)
    rows[order(title_sort, -created_sort, -subtitle_sort), , drop = FALSE]
  })

  article_lab_thumbnail_package_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_thumbnail_packages(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    title_sort <- tolower(ifelse(is.na(rows$title), "", rows$title))
    subtitle_sort <- tolower(ifelse(is.na(rows$subtitle), "", rows$subtitle))
    rows[order(title_sort, subtitle_sort, decreasing = FALSE), , drop = FALSE]
  })

  research_refresh <- reactiveVal(0L)
  selected_research_source_id <- reactiveVal(NA_integer_)
  selected_research_evidence_claim_id <- reactiveVal(NA_integer_)
  selected_research_evidence_group_key <- reactiveVal(NA_character_)

  research_ranked_sources <- reactive({
    research_refresh()
    load_research_sources(con, input$research_source_status_filter %||% "__all__", ranked = TRUE)
  })

  research_unranked_sources <- reactive({
    research_refresh()
    load_research_sources(con, input$research_source_status_filter %||% "__all__", ranked = FALSE)
  })

  research_summary_sources <- reactive({
    research_refresh()
    load_research_sources(con, "__all__", ranked = NULL)
  })

  confirmed_research_summaries <- reactive({
    research_refresh()
    load_confirmed_research_summaries(con)
  })

  selected_generate_summary <- reactive({
    selected_summary_id <- research_input_integer(input$article_lab_research_summary_id)
    rows <- confirmed_research_summaries()
    if (is.na(selected_summary_id) || nrow(rows) == 0 || !(selected_summary_id %in% rows$summary_id)) return(data.frame())
    rows[match(selected_summary_id, rows$summary_id), , drop = FALSE]
  })

  article_lab_effective_generation_inputs <- reactive({
    selected_summary <- selected_generate_summary()
    if (nrow(selected_summary) > 0) {
      return(list(
        mode = "research_summary",
        prompt = research_summary_prompt(selected_summary),
        manual_prompt = input$article_lab_prompt %||% article_lab_default_prompt,
        seed_topic = selected_summary$source_title[[1]],
        inspiration_source = paste0("research_summary:", selected_summary$summary_id[[1]]),
        summary_id = selected_summary$summary_id[[1]],
        source_title = selected_summary$source_title[[1]] %||% "",
        context_notes = input$article_lab_context_notes %||% ""
      ))
    }
    list(
      mode = "manual",
      prompt = input$article_lab_prompt %||% article_lab_default_prompt,
      manual_prompt = "",
      seed_topic = input$article_lab_seed_topic %||% "",
      inspiration_source = input$article_lab_inspiration_source %||% "",
      summary_id = NA_integer_,
      source_title = "",
      context_notes = input$article_lab_context_notes %||% ""
    )
  })

  selected_research_source <- reactive({
    research_refresh()
    load_research_source_by_id(con, selected_research_source_id())
  })

  selected_research_source_summary <- reactive({
    research_refresh()
    load_research_source_summary(con, selected_research_source_id())
  })

  selected_research_pdf_asset <- reactive({
    research_refresh()
    load_research_pdf_asset(con, selected_research_source_id())
  })

  research_angles <- reactive({
    research_refresh()
    source <- selected_research_source()
    if (nrow(source) == 0) return(data.frame())
    load_research_angles(con, source$research_source_id[[1]])
  })

  selected_research_angle <- reactive({
    rows <- research_angles()
    selected <- input$research_angles_table_rows_selected
    if (nrow(rows) == 0 || length(selected) == 0) return(data.frame())
    rows[selected[[1]], , drop = FALSE]
  })

  article_lab_pending_thumbnail_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_thumbnail_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows <- article_lab_normalize_candidate_rows(
      within(rows, {
        status <- parent_status
      })
    )
    title_sort <- tolower(ifelse(is.na(rows$title), "", rows$title))
    subtitle_sort <- tolower(ifelse(is.na(rows$subtitle), "", rows$subtitle))
    created_sort <- xtfrm(rows$created_at)
    rows[order(title_sort, subtitle_sort, -created_sort), , drop = FALSE]
  })

  article_lab_ready_for_thumbnail_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_ready_for_thumbnail_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$batch_id, rows$candidate_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_ready_for_outline_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_ready_for_outline_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$created_at, rows$thumbnail_id, decreasing = TRUE), , drop = FALSE]
  })

  article_lab_full_text_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_full_text_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$outline_updated_at, rows$draft_updated_at, rows$outline_id, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  })

  article_lab_full_text_package_rows_reactive <- reactive({
    article_lab_full_text_package_rows(article_lab_full_text_rows())
  })

  article_lab_review_publish_rows <- reactive({
    article_lab_refresh()
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) return(data.frame())
    rows <- load_article_lab_review_publish_rows(con, batch_id)
    if (nrow(rows) == 0) return(rows)
    rows[order(rows$approved_at, rows$draft_updated_at, rows$full_text_draft_id, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  })

  article_lab_publication_rows <- reactive({
    article_lab_refresh()
    load_article_lab_publications(con, active_only = TRUE)
  })

  article_lab_selected_review_publish_row <- reactive({
    rows <- article_lab_review_publish_rows()
    if (nrow(rows) == 0) return(data.frame())
    selected_id <- article_lab_input_string(input$article_lab_review_publish_draft_id) %||% rows$full_text_draft_id[[1]]
    if (!(selected_id %in% rows$full_text_draft_id)) selected_id <- rows$full_text_draft_id[[1]]
    rows[match(selected_id, rows$full_text_draft_id), , drop = FALSE]
  })

  collect_generate_triage_updates <- function(rows) {
    if (nrow(rows) == 0) return(list(updates = list(), selected_ids = character()))
    updates <- lapply(seq_len(nrow(rows)), function(i) {
      candidate_id <- rows$candidate_id[[i]]
      list(
        candidate_id = candidate_id,
        status = input[[article_lab_row_input_id("article_lab_generate_status", candidate_id)]] %||% rows$normalized_status[[i]],
        notes = input[[article_lab_row_input_id("article_lab_generate_notes", candidate_id)]] %||% rows$notes[[i]],
        selected = isTRUE(input[[article_lab_row_input_id("article_lab_generate_select", candidate_id)]])
      )
    })
    list(
      updates = updates,
      selected_ids = vapply(updates[vapply(updates, function(x) isTRUE(x$selected), logical(1))], `[[`, character(1), "candidate_id")
    )
  }

  collect_candidate_note_updates <- function(rows, prefix) {
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      candidate_id <- rows$candidate_id[[i]]
      list(
        candidate_id = candidate_id,
        notes = input[[article_lab_row_input_id(prefix, candidate_id)]] %||% rows$notes[[i]]
      )
    })
  }

  collect_subtitle_note_updates <- function(rows, prefix) {
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      subtitle_id <- rows$subtitle_id[[i]]
      list(
        subtitle_id = subtitle_id,
        notes = input[[article_lab_row_input_id(prefix, subtitle_id)]] %||% rows$notes[[i]]
      )
    })
  }

  collect_thumbnail_note_updates <- function(rows, prefix) {
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      thumbnail_id <- rows$thumbnail_id[[i]]
      list(
        thumbnail_id = thumbnail_id,
        notes = input[[article_lab_row_input_id(prefix, thumbnail_id)]] %||% rows$notes[[i]]
      )
    })
  }

  collect_outline_updates <- function(rows) {
    if (nrow(rows) == 0 || !("outline_id" %in% names(rows))) return(list())
    rows <- rows[!is.na(rows$outline_id) & nzchar(rows$outline_id) & rows$outline_status == "draft", , drop = FALSE]
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      outline_id <- rows$outline_id[[i]]
      list(
        outline_id = outline_id,
        outline_text = input[[article_lab_row_input_id("article_lab_outline_text", outline_id)]] %||% rows$outline_text[[i]],
        notes = input[[article_lab_row_input_id("article_lab_outline_notes", outline_id)]] %||% rows$outline_notes[[i]]
      )
    })
  }

  collect_full_text_updates <- function(rows) {
    if (nrow(rows) == 0 || !("full_text_draft_id" %in% names(rows))) return(list())
    rows <- rows[!is.na(rows$full_text_draft_id) & nzchar(rows$full_text_draft_id) & rows$draft_status == "draft", , drop = FALSE]
    if (nrow(rows) == 0) return(list())
    lapply(seq_len(nrow(rows)), function(i) {
      draft_id <- rows$full_text_draft_id[[i]]
      list(
        full_text_draft_id = draft_id,
        current_draft_text = input[[article_lab_row_input_id("article_lab_full_text_draft_text", draft_id)]] %||% rows$current_draft_text[[i]],
        notes = input[[article_lab_row_input_id("article_lab_full_text_draft_notes", draft_id)]] %||% rows$draft_notes[[i]]
      )
    })
  }

  collect_selected_ids <- function(rows, prefix, snapshot_ids = NULL, key_col = "candidate_id") {
    if (nrow(rows) == 0) return(character())
    if (!(key_col %in% names(rows))) return(character())
    snapshot_ids <- clean_text(snapshot_ids)
    snapshot_ids <- unique(snapshot_ids[!is.na(snapshot_ids)])
    if (length(snapshot_ids) > 0) {
      return(rows[[key_col]][rows[[key_col]] %in% snapshot_ids])
    }
    selected <- vapply(seq_len(nrow(rows)), function(i) {
      row_id <- rows[[key_col]][[i]]
      isTRUE(input[[article_lab_row_input_id(prefix, row_id)]])
    }, logical(1))
    rows[[key_col]][selected]
  }

  article_lab_apply_select_all <- function(rows, prefix, value, key_col = "candidate_id") {
    for (cid in rows[[key_col]]) {
      updateCheckboxInput(
        session,
        inputId = article_lab_row_input_id(prefix, cid),
        value = value
      )
    }
  }

  observe({
    article_lab_generate_candidates()
    updateCheckboxInput(session, inputId = "article_lab_generate_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_generate_select_all, {
    rows <- article_lab_generate_candidates()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_generate_select", isTRUE(input$article_lab_generate_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_queue_rows()
    updateCheckboxInput(session, inputId = "article_lab_queue_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_queue_select_all, {
    rows <- article_lab_queue_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_queue_select", isTRUE(input$article_lab_queue_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_scored_rows()
    updateCheckboxInput(session, inputId = "article_lab_scored_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_scored_select_all, {
    rows <- article_lab_scored_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_scored_select", isTRUE(input$article_lab_scored_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_subtitle_target_rows()
    updateCheckboxInput(session, inputId = "article_lab_subtitle_title_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_subtitle_title_select_all, {
    rows <- article_lab_subtitle_target_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_subtitle_title_select", isTRUE(input$article_lab_subtitle_title_select_all))
  }, ignoreInit = TRUE)

  observe({
    article_lab_pending_subtitle_rows()
    updateCheckboxInput(session, inputId = "article_lab_subtitle_candidate_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_subtitle_candidate_select_all, {
    rows <- article_lab_pending_subtitle_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_subtitle_candidate_select", isTRUE(input$article_lab_subtitle_candidate_select_all), key_col = "subtitle_id")
  }, ignoreInit = TRUE)

  observeEvent(article_lab_selected_batch_id(), {
    choices <- article_lab_manual_subtitle_choice_map(
      article_lab_subtitle_target_rows(),
      article_lab_pending_subtitle_rows()
    )
    current_value <- article_lab_input_string(input$article_lab_manual_subtitle_candidate_id)
    selected_value <- if (length(choices) > 0L && length(current_value) == 1L && !is.na(current_value) && current_value %in% unname(unlist(choices, use.names = FALSE))) current_value else NULL
    updateSelectizeInput(
      session,
      inputId = "article_lab_manual_subtitle_candidate_id",
      choices = choices,
      selected = selected_value,
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observe({
    article_lab_thumbnail_package_rows()
    updateCheckboxInput(session, inputId = "article_lab_thumbnail_package_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_thumbnail_package_select_all, {
    rows <- article_lab_thumbnail_package_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_thumbnail_package_select", isTRUE(input$article_lab_thumbnail_package_select_all), key_col = "subtitle_id")
  }, ignoreInit = TRUE)

  observe({
    article_lab_pending_thumbnail_rows()
    updateCheckboxInput(session, inputId = "article_lab_thumbnail_candidate_select_all", value = FALSE)
  })

  observeEvent(input$article_lab_thumbnail_candidate_select_all, {
    rows <- article_lab_pending_thumbnail_rows()
    if (nrow(rows) == 0) return()
    article_lab_apply_select_all(rows, "article_lab_thumbnail_candidate_select", isTRUE(input$article_lab_thumbnail_candidate_select_all), key_col = "thumbnail_id")
  }, ignoreInit = TRUE)

  observeEvent(input$research_refresh, {
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_refresh_selected_source, {
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_ranked_sources_table_rows_selected, {
    rows <- research_ranked_sources()
    selected <- input$research_ranked_sources_table_rows_selected
    if (nrow(rows) == 0 || length(selected) == 0) return()
    selected_research_source_id(rows$research_source_id[[selected[[1]]]])
    DT::selectRows(DT::dataTableProxy("research_unranked_sources_table"), NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$research_unranked_sources_table_rows_selected, {
    rows <- research_unranked_sources()
    selected <- input$research_unranked_sources_table_rows_selected
    if (nrow(rows) == 0 || length(selected) == 0) return()
    selected_research_source_id(rows$research_source_id[[selected[[1]]]])
    DT::selectRows(DT::dataTableProxy("research_ranked_sources_table"), NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$research_summary_source_id, {
    selected_research_source_id(research_input_integer(input$research_summary_source_id))
  }, ignoreInit = TRUE)

  observeEvent(selected_research_source_summary(), {
    summary <- selected_research_source_summary()
    value <- if (nrow(summary) == 0) research_summary_template else summary$summary_text[[1]] %||% research_summary_template
    updateTextAreaInput(session, "research_summary_text", value = value)
  }, ignoreInit = FALSE)

  normalize_research_ranked_queue <- function() {
    ids <- dbGetQuery(con, "SELECT research_source_id FROM research_sources WHERE manual_sort_order IS NOT NULL ORDER BY manual_sort_order ASC, updated_at DESC")
    if (nrow(ids) == 0) return(invisible(NULL))
    timestamp <- now_utc()
    for (i in seq_len(nrow(ids))) {
      dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = ? WHERE research_source_id = ?", params = list(timestamp, i, ids$research_source_id[[i]]))
    }
    invisible(NULL)
  }

  observeEvent(input$research_add_to_ranked, {
    id <- research_input_integer(selected_research_source_id())
    if (is.na(id)) {
      article_lab_state$notice <- "Select an unranked source before adding it to the ranked queue."
      return(invisible(NULL))
    }
    current <- dbGetQuery(con, "SELECT manual_sort_order FROM research_sources WHERE research_source_id = ? LIMIT 1", params = list(id))
    if (nrow(current) == 0 || !is.na(current$manual_sort_order[[1]])) {
      article_lab_state$notice <- "Select an unranked source before adding it to the ranked queue."
      return(invisible(NULL))
    }
    max_sort <- dbGetQuery(con, "SELECT COALESCE(MAX(manual_sort_order), 0) AS max_sort FROM research_sources WHERE manual_sort_order IS NOT NULL")
    dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = ? WHERE research_source_id = ?", params = list(now_utc(), as.integer(max_sort$max_sort[[1]]) + 1L, id))
    normalize_research_ranked_queue()
    article_lab_state$notice <- "Source added to ranked queue."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_remove_from_ranked, {
    id <- research_input_integer(selected_research_source_id())
    if (is.na(id)) {
      article_lab_state$notice <- "Select a ranked source before removing it from the ranked queue."
      return(invisible(NULL))
    }
    current <- dbGetQuery(con, "SELECT manual_sort_order FROM research_sources WHERE research_source_id = ? LIMIT 1", params = list(id))
    if (nrow(current) == 0 || is.na(current$manual_sort_order[[1]])) {
      article_lab_state$notice <- "Select a ranked source before removing it from the ranked queue."
      return(invisible(NULL))
    }
    dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = NULL WHERE research_source_id = ?", params = list(now_utc(), id))
    normalize_research_ranked_queue()
    article_lab_state$notice <- "Source removed from ranked queue."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_mark_finished, {
    rows <- selected_research_source()
    if (nrow(rows) == 0) {
      article_lab_state$notice <- "Select a source before marking it finished."
      return(invisible(NULL))
    }
    used_articles <- research_multiline_value(input$research_used_articles)
    if (is.na(used_articles)) {
      article_lab_state$notice <- "Add the article title or URL you wrote from this source before marking it finished."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    dbExecute(con, "
      UPDATE research_sources
      SET updated_at = ?, status = 'used', manual_sort_order = NULL, used_articles = ?, finished_at = ?
      WHERE research_source_id = ?
    ", params = list(timestamp, used_articles, timestamp, rows$research_source_id[[1]]))
    normalize_research_ranked_queue()
    article_lab_state$notice <- "Source marked finished and archived as used."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  move_ranked_source <- function(direction) {
    id <- research_input_integer(selected_research_source_id())
    rows <- dbGetQuery(con, "SELECT research_source_id FROM research_sources WHERE manual_sort_order IS NOT NULL ORDER BY manual_sort_order ASC, updated_at DESC")
    if (is.na(id) || nrow(rows) < 2 || !(id %in% rows$research_source_id)) return(FALSE)
    index <- match(id, rows$research_source_id)
    swap_index <- index + direction
    if (is.na(swap_index) || swap_index < 1L || swap_index > nrow(rows)) return(FALSE)
    ids <- rows$research_source_id
    ids[c(index, swap_index)] <- ids[c(swap_index, index)]
    timestamp <- now_utc()
    for (i in seq_along(ids)) {
      dbExecute(con, "UPDATE research_sources SET updated_at = ?, manual_sort_order = ? WHERE research_source_id = ?", params = list(timestamp, i, ids[[i]]))
    }
    TRUE
  }

  observeEvent(input$research_ranked_move_up, {
    if (move_ranked_source(-1L)) article_lab_state$notice <- "Ranked source moved up." else article_lab_state$notice <- "Select a ranked source that can move up."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_ranked_move_down, {
    if (move_ranked_source(1L)) article_lab_state$notice <- "Ranked source moved down." else article_lab_state$notice <- "Select a ranked source that can move down."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_add_source, {
    title <- research_input_value(input$research_new_source_title)
    if (is.na(title)) {
      article_lab_state$notice <- "Enter a source title before adding a research source."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    dbExecute(con, "
      INSERT INTO research_sources
        (created_at, updated_at, source_title, source_url, pdf_url, main_idea, abstract, source_type, source_name, manual_sort_order, status, notes)
      VALUES (?, ?, ?, ?, ?, ?, ?, 'paper', ?, ?, ?, ?)
    ", params = list(timestamp, timestamp, title, research_input_value(input$research_new_source_url), research_input_value(input$research_new_pdf_url), research_input_value(input$research_new_source_main_idea), research_input_value(input$research_new_source_abstract), research_input_value(input$research_new_source_name), research_input_integer(input$research_new_source_sort), research_input_default(input$research_new_source_status, "new"), research_input_value(input$research_new_source_notes)))
    article_lab_state$notice <- "Research source added."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_save_source, {
    rows <- selected_research_source()
    if (nrow(rows) == 0) {
      article_lab_state$notice <- "Select a source from the table to edit it."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    id <- rows$research_source_id[[1]]
    dbExecute(con, "
      UPDATE research_sources
      SET updated_at = ?, source_title = ?, source_url = ?, pdf_url = ?, main_idea = ?, abstract = ?, manual_sort_order = ?, status = ?, used_articles = ?, notes = ?
      WHERE research_source_id = ?
    ", params = list(timestamp, research_input_default(input$research_edit_source_title, rows$source_title[[1]]), research_input_value(input$research_edit_source_url), research_input_value(input$research_edit_pdf_url), research_input_value(input$research_edit_source_main), research_input_value(input$research_edit_source_abstract), research_input_integer(input$research_edit_source_sort), research_input_default(input$research_edit_source_status, "new"), research_multiline_value(input$research_edit_source_used_articles), research_input_value(input$research_edit_source_notes), id))
    article_lab_state$notice <- "Research source edits saved."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_add_angle, {
    source <- selected_research_source()
    source_id <- if (nrow(source) == 0) NA_integer_ else source$research_source_id[[1]]
    title <- research_input_value(input$research_new_angle_title)
    if (is.na(source_id) || is.na(title)) {
      article_lab_state$notice <- "Select a source and enter an angle title before creating an angle."
      return(invisible(NULL))
    }
    timestamp <- now_utc()
    dbExecute(con, "
      INSERT INTO research_article_angles
        (research_source_id, created_at, updated_at, angle_title, main_idea, manual_sort_order, status, notes)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(source_id, timestamp, timestamp, title, research_input_value(input$research_new_angle_main_idea), research_input_integer(input$research_new_angle_sort), research_input_default(input$research_new_angle_status, "idea"), research_input_value(input$research_new_angle_notes)))
    article_lab_state$notice <- "Research angle created."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_save_angle, {
    rows <- selected_research_angle()
    if (nrow(rows) == 0) return()
    timestamp <- now_utc()
    id <- rows$research_angle_id[[1]]
    dbExecute(con, "
      UPDATE research_article_angles
      SET updated_at = ?, angle_title = ?, main_idea = ?, manual_sort_order = ?, status = ?, notes = ?
      WHERE research_angle_id = ?
    ", params = list(timestamp, research_input_default(input$research_edit_angle_title, rows$angle_title[[1]]), research_input_value(input$research_edit_angle_main), research_input_integer(input$research_edit_angle_sort), research_input_default(input$research_edit_angle_status, "idea"), research_input_value(input$research_edit_angle_notes), id))
    article_lab_state$notice <- "Research angle edits saved."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  save_research_summary <- function(status) {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before saving a summary."
      return(NULL)
    }
    summary_text <- research_multiline_value(input$research_summary_text)
    if (is.na(summary_text)) {
      article_lab_state$notice <- "Enter summary text before saving."
      return(NULL)
    }
    timestamp <- now_utc()
    source_id <- source$research_source_id[[1]]
    if (identical(status, "draft")) {
      existing <- load_research_source_summary(con, source_id, status = "draft")
      if (nrow(existing) > 0) {
        dbExecute(con, "UPDATE research_source_summaries SET updated_at = ?, summary_text = ?, status = 'draft' WHERE summary_id = ?", params = list(timestamp, summary_text, existing$summary_id[[1]]))
        return(existing$summary_id[[1]])
      }
      dbExecute(con, "INSERT INTO research_source_summaries (research_source_id, created_at, updated_at, summary_text, status) VALUES (?, ?, ?, ?, 'draft')", params = list(source_id, timestamp, timestamp, summary_text))
      return(dbGetQuery(con, "SELECT last_insert_rowid() AS summary_id")$summary_id[[1]])
    }
    dbExecute(con, "INSERT INTO research_source_summaries (research_source_id, created_at, updated_at, summary_text, status, confirmed_at) VALUES (?, ?, ?, ?, 'confirmed', ?)", params = list(source_id, timestamp, timestamp, summary_text, timestamp))
    dbGetQuery(con, "SELECT last_insert_rowid() AS summary_id")$summary_id[[1]]
  }

  observeEvent(input$research_save_summary_draft, {
    summary_id <- save_research_summary("draft")
    if (!is.null(summary_id)) {
      article_lab_state$notice <- sprintf("Saved summary draft %s.", summary_id)
      research_refresh(research_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$research_confirm_summary, {
    summary_id <- save_research_summary("confirmed")
    if (!is.null(summary_id)) {
      article_lab_state$notice <- sprintf("Confirmed summary %s.", summary_id)
      research_refresh(research_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$research_download_pdf, {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before downloading a PDF."
      return(invisible(NULL))
    }
    source_id <- source$research_source_id[[1]]
    url <- research_pdf_source_url(source)
    if (is.na(url)) {
      save_research_pdf_asset(con, source_id, "failed", error = "No PDF URL found. Add a PDF URL or use manual upload.")
      article_lab_state$notice <- "No PDF URL found. Add a PDF URL or use manual upload."
      research_refresh(research_refresh() + 1L)
      return(invisible(NULL))
    }
    original_filename <- basename(strsplit(url, "[?#]", perl = TRUE)[[1]][[1]])
    if (!nzchar(original_filename) || identical(original_filename, "/")) original_filename <- NA_character_
    destination <- research_pdf_local_path(source_id, source$source_title[[1]], original_filename)
    temp_path <- tempfile(fileext = ".pdf")
    result <- tryCatch({
      utils::download.file(url, temp_path, mode = "wb", quiet = TRUE)
      if (!research_file_is_pdf(temp_path)) stop("Downloaded file is not a PDF.", call. = FALSE)
      if (!file.copy(temp_path, destination, overwrite = TRUE)) stop("Could not copy downloaded PDF into local folder.", call. = FALSE)
      sha <- research_pdf_sha256(destination)
      save_research_pdf_asset(con, source_id, "downloaded", source_url = url, local_path = destination, original_filename = original_filename, file_sha256 = sha, error = NA_character_)
      sprintf("Downloaded PDF to %s.", destination)
    }, error = function(e) {
      save_research_pdf_asset(con, source_id, "failed", source_url = url, local_path = NA_character_, original_filename = original_filename, file_sha256 = NA_character_, error = conditionMessage(e))
      sprintf("PDF download failed: %s", conditionMessage(e))
    }, finally = {
      if (file.exists(temp_path)) unlink(temp_path)
    })
    article_lab_state$notice <- result
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_pdf_upload, {
    source <- selected_research_source()
    upload <- input$research_pdf_upload
    if (nrow(source) == 0 || is.null(upload) || nrow(upload) == 0) return(invisible(NULL))
    source_id <- source$research_source_id[[1]]
    original_filename <- upload$name[[1]]
    destination <- research_pdf_local_path(source_id, source$source_title[[1]], original_filename)
    result <- tryCatch({
      if (!grepl("\\.pdf$", original_filename, ignore.case = TRUE) && !identical(upload$type[[1]], "application/pdf")) stop("Uploaded file is not a PDF.", call. = FALSE)
      if (!research_file_is_pdf(upload$datapath[[1]])) stop("Uploaded file content is not a PDF.", call. = FALSE)
      if (!file.copy(upload$datapath[[1]], destination, overwrite = TRUE)) stop("Could not copy uploaded PDF into local folder.", call. = FALSE)
      sha <- research_pdf_sha256(destination)
      save_research_pdf_asset(con, source_id, "uploaded", source_url = NA_character_, local_path = destination, original_filename = original_filename, file_sha256 = sha, error = NA_character_)
      sprintf("Uploaded PDF to %s.", destination)
    }, error = function(e) {
      save_research_pdf_asset(con, source_id, "failed", source_url = NA_character_, local_path = NA_character_, original_filename = original_filename, file_sha256 = NA_character_, error = conditionMessage(e))
      sprintf("PDF upload failed: %s", conditionMessage(e))
    })
    article_lab_state$notice <- result
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_clear_pdf, {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before clearing a PDF asset."
      return(invisible(NULL))
    }
    save_research_pdf_asset(con, source$research_source_id[[1]], "missing", source_url = NA_character_, local_path = NA_character_, original_filename = NA_character_, file_sha256 = NA_character_, error = NA_character_)
    article_lab_state$notice <- "PDF asset cleared. Download or upload a replacement PDF."
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_extract_pdf_sentences, {
    source <- selected_research_source()
    asset <- selected_research_pdf_asset()
    result <- tryCatch(research_extract_pdf_sentences(con, source, asset), error = function(e) e)
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste("PDF sentence extraction failed:", conditionMessage(result))
    } else {
      article_lab_state$notice <- sprintf("PDF sentence extraction ready with %s stored sentence%s.", result, ifelse(result == 1L, "", "s"))
    }
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  mark_research_claim_sentences <- function() {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before marking claim sentences."
      return(invisible(NULL))
    }
    summary_id <- save_research_summary("draft")
    if (is.null(summary_id)) return(invisible(NULL))
    summary <- load_research_source_summary(con, source$research_source_id[[1]], status = "draft")
    if (nrow(summary) == 0) summary <- load_research_source_summary(con, source$research_source_id[[1]])
    if (nrow(summary) == 0) {
      article_lab_state$notice <- "Save a summary before generating evidence."
      return(invisible(NULL))
    }

    max_claims <- max(1L, min(25L, suppressWarnings(as.integer(input$research_evidence_max_claims)) %||% 6L))
    timestamp <- now_utc()
    summary_sentence_payload <- research_summary_sentence_payload(summary$summary_text[[1]])
    summary_sentence_payload_json <- toJSON(summary_sentence_payload, auto_unbox = TRUE, null = "null")
    claim_template <- article_lab_input_multiline(input$research_claim_prompt) %||% article_lab_default_claim_extraction_prompt
    claim_prompt <- research_evidence_render_template(claim_template, list(
      max_claims = max_claims,
      research_source_id = source$research_source_id[[1]],
      source_title = source$source_title[[1]],
      summary_id = summary$summary_id[[1]],
      summary_text = summary$summary_text[[1]],
      summary_sentence_payload_json = summary_sentence_payload_json
    ))
    claim_model <- article_lab_input_string(input$research_claim_model) %||% article_lab_default_claim_extraction_model
    claim_reasoning <- article_lab_input_string(input$research_claim_reasoning_effort) %||% article_lab_default_evidence_reasoning_effort
    claim_payload_json <- toJSON(list(
      max_claims = max_claims,
      research_source_id = source$research_source_id[[1]],
      source_title = source$source_title[[1]],
      summary_id = summary$summary_id[[1]],
      summary_text = summary$summary_text[[1]],
      summary_sentence_payload = summary_sentence_payload
    ), auto_unbox = TRUE, null = "null")
    claim_result <- tryCatch(
      research_evidence_api_request("claim-sentence-marking", claim_prompt, claim_model, claim_reasoning, summary$summary_id[[1]], source$research_source_id[[1]]),
      error = function(e) e
    )
    if (inherits(claim_result, "error")) {
      dbExecute(con, "
        INSERT INTO research_summary_claims
          (summary_id, research_source_id, claim_index, claim_text, status, prompt_template, prompt_payload_json, model, reasoning_effort, error_message, created_at, updated_at)
        VALUES (?, ?, 0, '', 'failed', ?, ?, ?, ?, ?, ?, ?)
      ", params = list(summary$summary_id[[1]], source$research_source_id[[1]], claim_template, claim_payload_json, claim_model, claim_reasoning, conditionMessage(claim_result), timestamp, timestamp))
      article_lab_state$notice <- paste("Claim extraction failed:", conditionMessage(claim_result))
      research_refresh(research_refresh() + 1L)
      return(invisible(NULL))
    }

    claims <- claim_result$parsed$claims %||% list()
    if (length(claims) > max_claims) claims <- claims[seq_len(max_claims)]
    if (dbExistsTable(con, "research_summary_claim_evidence_sentences")) {
      dbExecute(con, "
        DELETE FROM research_summary_claim_evidence_sentences
        WHERE evidence_id IN (
          SELECT e.evidence_id
          FROM research_summary_claim_evidence e
          JOIN research_summary_claims c ON c.claim_id = e.claim_id
          WHERE c.summary_id = ?
        )
      ", params = list(summary$summary_id[[1]]))
    }
    dbExecute(con, "DELETE FROM research_summary_claim_evidence WHERE claim_id IN (SELECT claim_id FROM research_summary_claims WHERE summary_id = ?)", params = list(summary$summary_id[[1]]))
    dbExecute(con, "DELETE FROM research_summary_claims WHERE summary_id = ?", params = list(summary$summary_id[[1]]))
    claim_rows <- list()
    for (i in seq_along(claims)) {
      sentence_index <- research_input_integer(claims[[i]]$sentence_index)
      if (is.na(sentence_index) || sentence_index < 1L || sentence_index > length(summary_sentence_payload)) next
      exact_sentence <- summary_sentence_payload[[sentence_index]]$sentence_text
      claim_text <- article_lab_input_string(claims[[i]]$claim_text) %||% exact_sentence
      if (is.null(claim_text) || is.na(claim_text)) next
      original_text <- article_lab_input_string(claims[[i]]$original_text) %||% exact_sentence
      placement_hint <- article_lab_input_string(claims[[i]]$placement_hint) %||% "after_sentence"
      importance <- article_lab_input_string(claims[[i]]$importance) %||% ""
      dbExecute(con, "
        INSERT INTO research_summary_claims
          (summary_id, research_source_id, claim_index, claim_text, original_text, placement_hint, importance, status, prompt_template, prompt_payload_json, model, reasoning_effort, raw_json_response, created_at, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, 'suggested', ?, ?, ?, ?, ?, ?, ?)
      ", params = list(summary$summary_id[[1]], source$research_source_id[[1]], sentence_index, claim_text, original_text, placement_hint, importance, claim_template, claim_payload_json, claim_model, claim_reasoning, claim_result$raw_json, timestamp, timestamp))
      claim_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS claim_id")$claim_id[[1]]
      claim_rows[[length(claim_rows) + 1L]] <- list(claim_id = claim_id, claim_text = claim_text)
    }
    if (length(claim_rows) == 0) {
      article_lab_state$notice <- "Claim sentence marking returned no usable claim sentences."
      research_refresh(research_refresh() + 1L)
      return(invisible(NULL))
    }
    article_lab_state$notice <- sprintf("Marked %s atomic claim%s using %s. Source evidence has not been fetched yet.", length(claim_rows), ifelse(length(claim_rows) == 1L, "", "s"), claim_model)
    research_refresh(research_refresh() + 1L)
  }

  find_research_source_sentences <- function(use_fallback = FALSE) {
    source <- selected_research_source()
    asset <- selected_research_pdf_asset()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before finding source sentences."
      return(invisible(NULL))
    }
    summary <- selected_research_source_summary()
    if (nrow(summary) == 0) {
      article_lab_state$notice <- "Mark claim sentences before finding source sentences."
      return(invisible(NULL))
    }
    claim_rows_df <- load_research_summary_evidence_rows(con, summary$summary_id[[1]])
    claim_rows_df <- research_latest_evidence_by_claim(claim_rows_df)
    claim_rows_df <- claim_rows_df[!is.na(claim_rows_df$claim_id), , drop = FALSE]
    if (isTRUE(use_fallback) && nrow(claim_rows_df) > 0) {
      weak_status <- is.na(claim_rows_df$selection_status) | claim_rows_df$selection_status %in% c("no_match", "failed", "weak_support", "partially_supports", "generally_supported_no_direct_quote", "contradicts")
      weak_confidence <- is.na(claim_rows_df$confidence) | claim_rows_df$confidence %in% c("none", "low")
      claim_rows_df <- claim_rows_df[weak_status | weak_confidence, , drop = FALSE]
    }
    if (nrow(claim_rows_df) == 0) {
      article_lab_state$notice <- if (isTRUE(use_fallback)) "No weak or no-match markers need fallback rerun." else "Mark claim sentences before finding source sentences."
      return(invisible(NULL))
    }

    sentence_count <- tryCatch(research_extract_pdf_sentences(con, source, asset), error = function(e) e)
    if (inherits(sentence_count, "error")) {
      article_lab_state$notice <- paste("PDF sentence extraction failed:", conditionMessage(sentence_count))
      return(invisible(NULL))
    }
    if (sentence_count < 1L) {
      article_lab_state$notice <- "PDF text extraction produced no candidate sentences."
      return(invisible(NULL))
    }

    candidates_per_claim <- max(3L, min(20L, suppressWarnings(as.integer(input$research_evidence_candidates_per_claim)) %||% 12L))
    timestamp <- now_utc()
    claim_rows <- lapply(seq_len(nrow(claim_rows_df)), function(i) list(claim_id = claim_rows_df$claim_id[[i]], claim_text = claim_rows_df$claim_text[[i]]))
    candidate_payload <- lapply(claim_rows, function(claim) {
      candidates <- research_candidate_sentences_for_claim(con, source$research_source_id[[1]], claim$claim_text, limit = candidates_per_claim)
      list(
        claim_id = claim$claim_id,
        claim_text = claim$claim_text,
        candidates = unname(lapply(seq_len(nrow(candidates)), function(i) {
          list(sentence_id = candidates$sentence_id[[i]], page_number = candidates$page_number[[i]], sentence_text = candidates$sentence_text[[i]])
        }))
      )
    })
    allowed_sentence_ids <- setNames(lapply(candidate_payload, function(entry) {
      vapply(entry$candidates, function(candidate) as.integer(candidate$sentence_id), integer(1))
    }), vapply(candidate_payload, function(entry) as.character(entry$claim_id), character(1)))
    evidence_template <- article_lab_input_multiline(input$research_evidence_prompt) %||% article_lab_default_evidence_selection_prompt
    evidence_payload_json <- toJSON(candidate_payload, auto_unbox = TRUE, null = "null")
    evidence_prompt <- research_evidence_render_template(evidence_template, list(claim_candidate_payload_json = evidence_payload_json))
    evidence_model <- if (isTRUE(use_fallback)) article_lab_input_string(input$research_evidence_fallback_model) %||% article_lab_default_evidence_fallback_model else article_lab_input_string(input$research_evidence_model) %||% article_lab_default_evidence_selection_model
    evidence_reasoning <- if (isTRUE(use_fallback)) article_lab_input_string(input$research_evidence_fallback_reasoning_effort) %||% "medium" else article_lab_input_string(input$research_evidence_reasoning_effort) %||% article_lab_default_evidence_reasoning_effort
    evidence_result <- tryCatch(
      research_evidence_api_request("evidence-selection", evidence_prompt, evidence_model, evidence_reasoning, summary$summary_id[[1]], source$research_source_id[[1]]),
      error = function(e) e
    )
    if (inherits(evidence_result, "error")) {
      for (claim in claim_rows) {
        dbExecute(con, "
          INSERT INTO research_summary_claim_evidence
            (claim_id, selection_status, prompt_template, prompt_payload_json, model, reasoning_effort, error_message, created_at, updated_at)
          VALUES (?, 'failed', ?, ?, ?, ?, ?, ?, ?)
        ", params = list(claim$claim_id, evidence_template, evidence_payload_json, evidence_model, evidence_reasoning, conditionMessage(evidence_result), timestamp, timestamp))
      }
      article_lab_state$notice <- paste("Evidence selection failed:", conditionMessage(evidence_result))
      research_refresh(research_refresh() + 1L)
      return(invisible(NULL))
    }
    results <- evidence_result$parsed$results %||% list()
    for (result in results) {
      claim_id <- research_input_integer(result$claim_id)
      if (is.na(claim_id)) next
      sentence_ids <- research_evidence_sentence_ids(result)
      allowed_for_claim <- allowed_sentence_ids[[as.character(claim_id)]] %||% integer()
      sentence_ids <- sentence_ids[sentence_ids %in% allowed_for_claim]
      sentence_ids <- sentence_ids[seq_len(min(3L, length(sentence_ids)))]
      confidence <- article_lab_input_string(result$confidence) %||% "none"
      status <- research_support_status(result$support_status %||% result$selection_status, sentence_ids = sentence_ids, confidence = confidence)
      if (status %in% c("generally_supported_no_direct_quote", "no_match")) sentence_ids <- integer()
      if (length(sentence_ids) == 0 && status %in% c("supports", "partially_supports", "weak_support", "contradicts")) status <- "no_match"
      sentence_id <- if (length(sentence_ids) > 0) sentence_ids[[1]] else NA_integer_
      dbExecute(con, "
        INSERT INTO research_summary_claim_evidence
          (claim_id, sentence_id, selection_status, confidence, selector_reason, prompt_template, prompt_payload_json, model, reasoning_effort, raw_json_response, created_at, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = list(claim_id, if (is.na(sentence_id)) NA_integer_ else sentence_id, status, confidence, article_lab_input_string(result$reason), evidence_template, evidence_payload_json, evidence_model, evidence_reasoning, evidence_result$raw_json, timestamp, timestamp))
      evidence_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS evidence_id")$evidence_id[[1]]
      if (dbExistsTable(con, "research_summary_claim_evidence_sentences") && length(sentence_ids) > 0) {
        quote_pages <- dbGetQuery(con, sprintf(
          "SELECT sentence_id, page_number FROM research_pdf_sentences WHERE sentence_id IN (%s)",
          paste(rep("?", length(sentence_ids)), collapse = ",")
        ), params = as.list(sentence_ids))
        page_by_id <- setNames(as.list(quote_pages$page_number), as.character(quote_pages$sentence_id))
        for (rank in seq_along(sentence_ids)) {
          quote_page <- page_by_id[[as.character(sentence_ids[[rank]])]]
          dbExecute(con, "
            INSERT INTO research_summary_claim_evidence_sentences
              (evidence_id, sentence_id, quote_rank, page_number, created_at)
            VALUES (?, ?, ?, ?, ?)
          ", params = list(evidence_id, sentence_ids[[rank]], rank, quote_page %||% NA_integer_, timestamp))
        }
      }
    }
    article_lab_state$notice <- sprintf("Found source-sentence suggestions for %s marked claim%s using %s.", length(claim_rows), ifelse(length(claim_rows) == 1L, "", "s"), evidence_model)
    research_refresh(research_refresh() + 1L)
  }

  observeEvent(input$research_mark_claim_sentences, {
    mark_research_claim_sentences()
  }, ignoreInit = TRUE)

  observeEvent(input$research_find_source_sentences, {
    find_research_source_sentences(use_fallback = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$research_rerun_weak_evidence, {
    find_research_source_sentences(use_fallback = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$research_verify_evidence, {
    evidence_id <- research_input_integer(input$research_verify_evidence)
    if (is.na(evidence_id)) return(invisible(NULL))
    dbExecute(con, "UPDATE research_summary_claim_evidence SET selection_status = 'verified', verified_at = ?, verified_by = 'manual', updated_at = ? WHERE evidence_id = ?", params = list(now_utc(), now_utc(), evidence_id))
    article_lab_state$notice <- sprintf("Verified evidence %s.", evidence_id)
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_reject_evidence, {
    evidence_id <- research_input_integer(input$research_reject_evidence)
    if (is.na(evidence_id)) return(invisible(NULL))
    dbExecute(con, "UPDATE research_summary_claim_evidence SET selection_status = 'rejected', updated_at = ? WHERE evidence_id = ?", params = list(now_utc(), evidence_id))
    article_lab_state$notice <- sprintf("Rejected evidence %s.", evidence_id)
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  output$research_paperqa_chunks_error <- renderUI({
    err <- article_lab_state$last_research_paperqa_chunks_error
    if (is.null(err)) return(NULL)
    err_at <- article_lab_state$last_research_paperqa_chunks_error_at
    elapsed <- if (is.null(err_at)) "" else format(err_at, "%Y-%m-%d %H:%M:%S")
    kind_label <- switch(
      err$kind %||% "unknown",
      api_failed = "PaperQA2 API call failed",
      paperqa_missing = "PaperQA2 package missing",
      exception = "PaperQA2 chunk retrieval crashed",
      "PaperQA2 chunk retrieval error"
    )
    pdf_available <- if (isTRUE(err$pdf_ok)) "Yes" else if (identical(err$pdf_ok, FALSE)) "No (missing file)" else "Unknown"
    div(
      class = "lab-alert lab-alert-error",
      role = "alert",
      div(
        class = "lab-alert-title",
        span(class = "lab-alert-icon", HTML("&#9888;")),
        strong(kind_label),
        if (nzchar(elapsed)) span(class = "lab-alert-time", sprintf(" at %s", elapsed))
      ),
      div(
        class = "lab-alert-body",
        p(err$reason %||% "Unknown error."),
        tags$ul(
          tags$li(sprintf("Python binary: %s", err$python_bin %||% "unknown")),
          tags$li(sprintf("PDF local path OK: %s", pdf_available)),
          if (nzchar(err$detail %||% "")) tags$li(sprintf("Detail: %s", err$detail)),
          if (nzchar(err$hint %||% "")) tags$li(sprintf("Hint: %s", err$hint)),
          tags$li("No evidence contexts were returned. Check the saved JSON diagnostics, fix the issue, and run PaperQA2 again.")
        ),
        if (nzchar(err$traceback %||% "")) p(code(err$traceback))
      )
    )
  })

  output$research_paperqa_chunks_display <- renderUI({
    chunks <- article_lab_state$last_research_paperqa_chunks
    if (is.null(chunks) || length(chunks) == 0) return(NULL)
    mode <- article_lab_state$last_research_paperqa_chunks_mode %||% "paperqa"
    is_query_mode <- identical(mode, "paperqa_query")
    label <- if (is_query_mode) "contexts" else "chunks"
    header <- h4(sprintf("PaperQA2 %s (%s - %s %s)", label, mode, length(chunks), label))
    answer_text <- article_lab_state$last_research_paperqa_answer
    answer_block <- if (is.null(answer_text) || !nzchar(answer_text)) NULL else div(
      class = "lab-status-copy",
      strong("PaperQA2 answer: "),
      span(answer_text)
    )
    file_path <- article_lab_state$last_research_paperqa_chunks_file
    file_block <- if (is.null(file_path) || !nzchar(file_path)) NULL else div(
      class = "lab-status-copy",
      sprintf("JSON saved to: %s", file_path)
    )
    items <- lapply(seq_along(chunks), function(i) {
      ch <- chunks[[i]]
      if (is.null(ch) || length(ch) == 0) return(NULL)
      preview <- substr(ch$text %||% ch$preview %||% "", 1, 500)
      if (nchar(preview) >= 500) preview <- paste0(preview, "...")
      score <- suppressWarnings(as.numeric(ch$relevance_score %||% NA_real_))
      score_tag <- if (!is.na(score)) span(sprintf("Score: %.3f", score)) else NULL
      page_hint <- ch$page_hint %||% ch$page_number %||% "?"
      chunk_id <- ch$chunk_id %||% ch$chunk_index %||% (i - 1)
      div(
        class = "lab-card",
        style = "margin-bottom: 8px; padding: 8px 12px;",
        div(
          style = "display: flex; gap: 16px; font-size: 0.85em; color: #555; margin-bottom: 6px;",
          span(sprintf("%s %s", if (is_query_mode) "Context" else "Chunk", chunk_id)),
          span(sprintf("Page %s", page_hint)),
          span(sprintf("%s characters", ch$char_count %||% nchar(ch$text %||% ""))),
          score_tag
        ),
        if (!is.null(ch$citation) && nzchar(ch$citation)) div(style = "font-size: 0.8em; color: #666; margin-bottom: 4px;", sprintf("Citation: %s", ch$citation)),
        pre(sprintf("%s", htmltools::htmlEscape(preview)), style = "white-space: pre-wrap; font-size: 0.82em; background: #f8f8f8; padding: 8px; border-radius: 4px; margin: 0; max-height: 120px; overflow-y: auto;")
      )
    })
    items <- Filter(Negate(is.null), items)
    if (length(items) == 0) return(NULL)
    tagList(header, answer_block, file_block, div(style = "max-height: 480px; overflow-y: auto;", items))
  })

  observeEvent(input$research_run_paperqa_chunks, {
    query_text <- article_lab_input_multiline(input$research_paperqa_query) %||% ""
    if (!nzchar(trimws(query_text))) query_text <- NULL
    if (is.null(query_text)) {
      article_lab_state$last_research_paperqa_chunks <- NULL
      article_lab_state$last_research_paperqa_chunks_mode <- NULL
      article_lab_state$last_research_paperqa_answer <- NULL
      article_lab_state$last_research_paperqa_chunks_file <- NULL
      article_lab_state$last_research_paperqa_chunks_error <- list(
        kind = "validation",
        reason = "Enter a claim or question before running PaperQA2 retrieval.",
        python_bin = "not used",
        pdf_ok = NA,
        detail = "",
        hint = "Stage 1 PaperQA2 retrieval is query/claim-based; blank whole-PDF chunking is intentionally disabled.",
        traceback = ""
      )
      article_lab_state$last_research_paperqa_chunks_error_at <- Sys.time()
      article_lab_state$notice <- "PaperQA2 retrieval needs a claim or question."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    article_lab_state$notice <- "Running PaperQA2 evidence retrieval..."
    article_lab_state$last_research_paperqa_chunks_error <- NULL
    article_lab_state$last_research_paperqa_chunks_error_at <- NULL
    article_lab_state$last_research_paperqa_chunks <- NULL
    article_lab_state$last_research_paperqa_chunks_mode <- NULL
    article_lab_state$last_research_paperqa_answer <- NULL
    article_lab_state$last_research_paperqa_chunks_file <- NULL
    article_lab_refresh(article_lab_refresh() + 1L)
    if (is.function(session$flushReact)) session$flushReact()

    source <- selected_research_source()
    asset <- selected_research_pdf_asset()

    result <- tryCatch(
      research_paperqa_chunks_request(
        source = source,
        asset = asset,
        query = query_text,
        chunk_chars = input$research_paperqa_chunk_chars %||% 1500L,
        chunk_overlap = input$research_paperqa_chunk_overlap %||% 100L
      ),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      pdf_ok <- FALSE
      if (nrow(source) > 0 && nrow(asset) > 0) {
        local_path <- research_resolve_local_pdf_path(asset$local_path[[1]])
        pdf_ok <- !is.na(local_path) && file.exists(local_path)
      }
      python_bin <- tryCatch(research_paperqa_resolve_python()$python_bin %||% "unknown", error = function(e) "unknown")
      article_lab_state$last_research_paperqa_chunks_error <- list(
        kind = "exception",
        reason = conditionMessage(result),
        python_bin = python_bin,
        pdf_ok = pdf_ok,
        detail = "",
        hint = "Check debug log and verify Python/PaperQA2 setup.",
        traceback = ""
      )
      article_lab_state$last_research_paperqa_chunks_error_at <- Sys.time()
      article_lab_state$notice <- paste("PaperQA2 retrieval failed:", conditionMessage(result))
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    mode <- result$mode %||% "paperqa"
    is_query_mode <- identical(mode, "paperqa_query")
    chunks <- if (is_query_mode) result$contexts %||% list() else result$chunks %||% list()

    if (identical(mode, "paperqa_missing")) {
      warn_msg <- result$warning %||% ""
      diagnostics <- result$diagnostics %||% list()
      article_lab_state$last_research_paperqa_chunks_error <- list(
        kind = "paperqa_missing",
        reason = warn_msg,
        python_bin = diagnostics$python_executable %||% "unknown",
        pdf_ok = {
          local_path <- if (nrow(asset) > 0) research_resolve_local_pdf_path(asset$local_path[[1]]) else NA_character_
          !is.na(local_path) && file.exists(local_path)
        },
        detail = diagnostics$detail %||% "",
        hint = diagnostics$hint %||% "",
        traceback = ""
      )
      article_lab_state$last_research_paperqa_chunks_error_at <- Sys.time()
    } else {
      article_lab_state$last_research_paperqa_chunks_error <- NULL
      article_lab_state$last_research_paperqa_chunks_error_at <- NULL
    }

    article_lab_state$last_research_paperqa_chunks <- chunks
    article_lab_state$last_research_paperqa_chunks_mode <- mode
    article_lab_state$last_research_paperqa_answer <- result$answer %||% NULL
    article_lab_state$last_research_paperqa_chunks_file <- result$chunks_file %||% NULL

    count_label <- if (is_query_mode) result$context_count %||% length(chunks) else result$chunk_count %||% length(chunks)
    article_lab_state$notice <- sprintf(
      "PaperQA2 retrieval complete: %s %s (%s).",
      count_label,
      if (is_query_mode) "contexts" else "chunks",
      mode
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$research_generate_summary_draft, {
    source <- selected_research_source()
    asset <- selected_research_pdf_asset()
    prompt_version <- input$research_summary_prompt_version
    prompt_text <- article_lab_input_multiline(input$research_summary_api_prompt) %||% article_lab_default_research_summary_prompt
    save_research_summary_prompt(con, prompt_version, prompt_text)
    result <- tryCatch(
      research_summary_api_request(
        source = source,
        asset = asset,
        model = input$research_summary_model,
        prompt_version = prompt_version,
        prompt = prompt_text
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste("Summary generation failed:", conditionMessage(result))
      return(invisible(NULL))
    }

    updateTextAreaInput(session, "research_summary_text", value = result$summary_text)
    timestamp <- now_utc()
    source_id <- source$research_source_id[[1]]
    existing <- load_research_source_summary(con, source_id, status = "draft")
    if (nrow(existing) > 0) {
      dbExecute(con, "
        UPDATE research_source_summaries
        SET updated_at = ?, summary_text = ?, status = 'draft', model = ?, prompt_version = ?, raw_json = ?
        WHERE summary_id = ?
      ", params = list(timestamp, result$summary_text, result$model, result$prompt_version, result$raw_json, existing$summary_id[[1]]))
      summary_id <- existing$summary_id[[1]]
    } else {
      dbExecute(con, "
        INSERT INTO research_source_summaries
          (research_source_id, created_at, updated_at, summary_text, status, model, prompt_version, raw_json)
        VALUES (?, ?, ?, ?, 'draft', ?, ?, ?)
      ", params = list(source_id, timestamp, timestamp, result$summary_text, result$model, result$prompt_version, result$raw_json))
      summary_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS summary_id")$summary_id[[1]]
    }
    article_lab_state$notice <- sprintf("Generated and saved summary draft %s with model %s.", summary_id, result$model)
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  use_confirmed_summary_in_generate <- function(summary_id) {
    rows <- confirmed_research_summaries()
    summary_id_value <- research_input_integer(summary_id)
    if (is.na(summary_id_value) || nrow(rows) == 0 || !(summary_id_value %in% rows$summary_id)) return(FALSE)
    row <- rows[match(summary_id_value, rows$summary_id), , drop = FALSE]
    prompt <- research_summary_prompt(row)
    updateTextAreaInput(session, "article_lab_prompt", value = prompt)
    updateTextInput(session, "article_lab_seed_topic", value = row$source_title[[1]] %||% "")
    updateSelectInput(session, "article_lab_inspiration_source", selected = "")
    updateSelectizeInput(session, "article_lab_research_summary_id", selected = as.character(summary_id_value))
    active_section("generate")
    TRUE
  }

  observeEvent(input$research_send_summary_to_generate, {
    source <- selected_research_source()
    if (nrow(source) == 0) {
      article_lab_state$notice <- "Select a source before sending a summary to Generate."
      return(invisible(NULL))
    }
    confirmed <- load_research_source_summary(con, source$research_source_id[[1]], status = "confirmed")
    if (nrow(confirmed) == 0) {
      article_lab_state$notice <- "Confirm this source summary before sending it to Generate."
      return(invisible(NULL))
    }
    if (use_confirmed_summary_in_generate(confirmed$summary_id[[1]])) {
      article_lab_state$notice <- sprintf("Loaded confirmed summary %s into Generate.", confirmed$summary_id[[1]])
    }
  }, ignoreInit = TRUE)

  observeEvent(input$research_send_to_title_lab, {
    angle_id <- research_input_integer(input$research_send_to_title_lab)
    angle <- dbGetQuery(con, "SELECT * FROM research_article_angles WHERE research_angle_id = ? LIMIT 1", params = list(angle_id))
    if (nrow(angle) == 0 || is.na(angle$research_source_id[[1]])) return()
    source <- dbGetQuery(con, "SELECT * FROM research_sources WHERE research_source_id = ? LIMIT 1", params = list(angle$research_source_id[[1]]))
    if (nrow(source) == 0) return()
    prompt <- research_title_prompt(source, angle)
    inspiration <- paste0("research_angle:", angle_id)
    generated <- generate_title_candidates(con, prompt, batch_size = input$article_lab_batch_size %||% 12L, seed_topic = angle$angle_title[[1]], inspiration_source = inspiration, model = input$article_lab_model %||% article_lab_default_model)
    batch_id <- save_article_lab_batch(con, prompt, angle$angle_title[[1]], inspiration, input$article_lab_batch_size %||% 12L, generated$model %||% input$article_lab_model %||% article_lab_default_model, generated$titles$title, raw_json = generated$raw_json, generation_mode = generated$mode %||% "research_inbox")
    dbExecute(con, "UPDATE research_article_angles SET updated_at = ?, status = 'sent_to_title_lab', article_lab_batch_id = ? WHERE research_angle_id = ?", params = list(now_utc(), batch_id, angle_id))
    updateTextAreaInput(session, "article_lab_prompt", value = prompt)
    updateTextInput(session, "article_lab_seed_topic", value = angle$angle_title[[1]])
    updateSelectInput(session, "article_lab_inspiration_source", selected = "custom")
    active_section("generate")
    article_lab_state$notice <- sprintf("Sent research angle %s to Title Lab as batch %s.", angle_id, batch_id)
    article_lab_refresh(article_lab_refresh() + 1L)
    research_refresh(research_refresh() + 1L)
  }, ignoreInit = TRUE)

  output$article_lab_notice <- renderUI({
    notice <- article_lab_state$notice
    if (is.null(notice) || !nzchar(notice)) return(NULL)
    div(class = "lab-status-copy", notice)
  })

  output$article_lab_outline_generate_error <- renderUI({
    err <- article_lab_state$last_outline_generate_error
    if (is.null(err)) return(NULL)
    err_at <- article_lab_state$last_outline_generate_error_at
    elapsed <- if (is.null(err_at)) "" else format(err_at, "%Y-%m-%d %H:%M:%S")
    kind_label <- switch(
      err$kind %||% "unknown",
      api_failed = "Outline API call failed",
      no_rows = "Outline API returned no usable rows",
      exception = "Outline generation crashed",
      "Outline generation error"
    )
    affected_n <- length(err$selected_ids %||% character())
    div(
      class = "lab-alert lab-alert-error",
      role = "alert",
      div(
        class = "lab-alert-title",
        span(class = "lab-alert-icon", HTML("&#9888;")),
        strong(kind_label),
        if (nzchar(elapsed)) span(class = "lab-alert-time", sprintf(" at %s", elapsed))
      ),
      div(
        class = "lab-alert-body",
        p(err$reason %||% "Unknown error."),
        tags$ul(
          tags$li(sprintf("Model: %s", err$model %||% "unknown")),
          tags$li(sprintf("Mode reported by helper: %s", err$mode %||% "unknown")),
          if (affected_n > 0) tags$li(sprintf("Affected package%s: %s", ifelse(affected_n == 1L, "", "s"), paste(err$selected_ids, collapse = ", "))) else tags$li("No packages were sent to the API."),
          tags$li("The existing outline text was NOT changed. Regenerate again only after the underlying problem is fixed (e.g., billing/quota, model availability, or local network).")
        ),
        p(
          strong("Debug log: "),
          code(".local_gitignored/article_lab_debug.log")
        )
      )
    )
  })

  output$article_lab_full_text_generate_error <- renderUI({
    err <- article_lab_state$last_full_text_generate_error
    if (is.null(err)) return(NULL)
    err_at <- article_lab_state$last_full_text_generate_error_at
    elapsed <- if (is.null(err_at)) "" else format(err_at, "%Y-%m-%d %H:%M:%S")
    kind_label <- switch(
      err$kind %||% "unknown",
      no_selection = "No approved outline selected",
      stale_selection = "Selected outline unavailable",
      api_failed = "Full article API call failed",
      no_rows = "Full article API returned no usable rows",
      exception = "Full article generation crashed",
      "Full article generation error"
    )
    affected_n <- length(err$selected_ids %||% character())
    div(
      class = "lab-alert lab-alert-error",
      role = "alert",
      div(
        class = "lab-alert-title",
        span(class = "lab-alert-icon", HTML("&#9888;")),
        strong(kind_label),
        if (nzchar(elapsed)) span(class = "lab-alert-time", sprintf(" at %s", elapsed))
      ),
      div(
        class = "lab-alert-body",
        p(err$reason %||% "Unknown error."),
        tags$ul(
          tags$li(sprintf("Model: %s", err$model %||% "unknown")),
          tags$li(sprintf("Mode reported by helper: %s", err$mode %||% "unknown")),
          if (affected_n > 0) tags$li(sprintf("Affected outline%s: %s", ifelse(affected_n == 1L, "", "s"), paste(err$selected_ids, collapse = ", "))) else tags$li("No approved outline was sent to the API."),
          tags$li("No full article draft was saved. Approve an outline first, then select exactly one approved outline in this tab and generate again.")
        ),
        p(
          strong("Debug log: "),
          code(".local_gitignored/article_lab_debug.log")
        )
      )
    )
  })

  output$research_summary_source_selector <- renderUI({
    rows <- research_summary_sources()
    choices <- if (nrow(rows) == 0) character() else setNames(
      rows$research_source_id,
      sprintf(
        "%s%s · %s",
        ifelse(is.na(rows$manual_sort_order), "", sprintf("#%s ", rows$manual_sort_order)),
        rows$source_title,
        rows$status
      )
    )
    selected <- selected_research_source_id()
    selectizeInput("research_summary_source_id", "Source", choices = choices, selected = selected, width = "100%")
  })

  output$research_summary_selected_source <- renderUI({
    source <- selected_research_source()
    if (nrow(source) == 0) return(div(class = "lab-status-copy", "Select a source to write or confirm a summary."))
    rank_copy <- if (is.na(source$manual_sort_order[[1]])) "Unranked" else sprintf("Rank #%s", source$manual_sort_order[[1]])
    main_idea <- research_input_value(source$main_idea[[1]])
    abstract <- research_input_value(source$abstract[[1]])
    div(
      class = "lab-status-copy",
      h3(source$source_title[[1]]),
      HTML(sprintf("<strong>%s</strong> · status: %s · %s", htmltools::htmlEscape(rank_copy), htmltools::htmlEscape(source$status[[1]] %||% ""), research_links(source$source_url[[1]], source$pdf_url[[1]]))),
      if (!is.na(main_idea)) p(strong("Main idea: "), main_idea),
      if (!is.na(abstract)) p(strong("Abstract: "), abstract)
    )
  })

  output$research_summary_pdf_status <- renderUI({
    source <- selected_research_source()
    if (nrow(source) == 0) return(NULL)
    asset <- selected_research_pdf_asset()
    status <- if (nrow(asset) == 0) "missing" else research_input_default(asset$status[[1]], "missing")
    status_label <- research_pdf_status_labels[[status]] %||% status
    local_path <- if (nrow(asset) == 0) NA_character_ else research_input_value(asset$local_path[[1]])
    source_url <- if (nrow(asset) == 0) research_pdf_source_url(source) else research_input_value(asset$source_url[[1]])
    error <- if (nrow(asset) == 0) NA_character_ else research_input_value(asset$error[[1]])
    div(
      class = "lab-card",
      h3("PDF"),
      p(strong("PDF status: "), status_label),
      if (!is.na(local_path)) p(strong("Local path: "), local_path),
      if (!is.na(source_url)) p(strong("Source URL used: "), source_url),
      if (identical(status, "failed") && !is.na(error)) p(strong("Error: "), error)
    )
  })

  output$research_summary_pdf_gate <- renderUI({
    asset <- selected_research_pdf_asset()
    ready <- nrow(asset) > 0 && asset$status[[1]] %in% c("downloaded", "uploaded") && !is.na(research_input_value(asset$local_path[[1]]))
    copy <- if (isTRUE(ready)) "PDF ready for summary generation." else "Download or upload a PDF before generating an API summary."
    div(class = "lab-status-copy", copy)
  })

  research_evidence_prompt_preview <- reactive({
    source <- selected_research_source()
    summary <- selected_research_source_summary()
    summary_text <- article_lab_input_multiline(input$research_summary_text) %||% ""
    max_claims <- max(1L, min(25L, suppressWarnings(as.integer(input$research_evidence_max_claims)) %||% 6L))
    candidates_per_claim <- max(3L, min(20L, suppressWarnings(as.integer(input$research_evidence_candidates_per_claim)) %||% 12L))
    summary_id <- if (nrow(summary) == 0) "(summary not saved yet)" else summary$summary_id[[1]]
    variables <- list(
      max_claims = max_claims,
      research_source_id = if (nrow(source) == 0) "" else source$research_source_id[[1]],
      source_title = if (nrow(source) == 0) "" else source$source_title[[1]],
      summary_id = summary_id,
      summary_text = summary_text,
      summary_sentence_payload_json = toJSON(research_summary_sentence_payload(summary_text), auto_unbox = TRUE, null = "null"),
      claim_candidate_payload_json = sprintf("(Generated after local retrieval: up to %s candidate sentences per claim.)", candidates_per_claim)
    )
    claim_template <- article_lab_input_multiline(input$research_claim_prompt) %||% article_lab_default_claim_extraction_prompt
    evidence_template <- article_lab_input_multiline(input$research_evidence_prompt) %||% article_lab_default_evidence_selection_prompt
    list(
      claim_prompt = research_evidence_render_template(claim_template, variables),
      evidence_prompt = research_evidence_render_template(evidence_template, variables),
      max_claims = max_claims,
      candidates_per_claim = candidates_per_claim
    )
  })

  output$research_evidence_effective_prompts <- renderUI({
    preview <- research_evidence_prompt_preview()
    div(
      class = "lab-card",
      h4("Exact prompts before API calls"),
      p(class = "lab-status-copy", "The claim step sends the summary only. The evidence step sends claim text plus locally retrieved candidate sentence IDs/text/page metadata only."),
      tags$details(
        tags$summary("Show exact evidence workflow prompts"),
        h4("Claim extraction request fields"),
        tags$pre(class = "lab-status-copy", paste(
          sprintf("Model: %s", article_lab_input_string(input$research_claim_model) %||% article_lab_default_claim_extraction_model),
          sprintf("Reasoning effort: %s", article_lab_input_string(input$research_claim_reasoning_effort) %||% article_lab_default_evidence_reasoning_effort),
          sep = "\n"
        )),
        h4("Resolved claim extraction prompt"),
        tags$pre(class = "lab-status-copy", preview$claim_prompt),
        h4("Evidence selection request fields"),
        tags$pre(class = "lab-status-copy", paste(
          sprintf("Model: %s", article_lab_input_string(input$research_evidence_model) %||% article_lab_default_evidence_selection_model),
          sprintf("Reasoning effort: %s", article_lab_input_string(input$research_evidence_reasoning_effort) %||% article_lab_default_evidence_reasoning_effort),
          sprintf("Fallback model: %s", article_lab_input_string(input$research_evidence_fallback_model) %||% article_lab_default_evidence_fallback_model),
          sprintf("Fallback reasoning effort: %s", article_lab_input_string(input$research_evidence_fallback_reasoning_effort) %||% "medium"),
          sep = "\n"
        )),
        h4("Evidence selection prompt template preview"),
        tags$pre(class = "lab-status-copy", preview$evidence_prompt)
      )
    )
  })

  observeEvent(input$research_select_evidence_claim, {
    selected_research_evidence_claim_id(research_input_integer(input$research_select_evidence_claim))
  }, ignoreInit = TRUE)

  observeEvent(input$research_select_evidence_group, {
    key <- article_lab_input_string(input$research_select_evidence_group)
    selected_research_evidence_group_key(if (is.na(key) || !nzchar(key)) NA_character_ else key)
  }, ignoreInit = TRUE)

  observeEvent(input$research_close_evidence_drawer, {
    selected_research_evidence_claim_id(NA_integer_)
    selected_research_evidence_group_key(NA_character_)
  }, ignoreInit = TRUE)

  output$research_summary_inline_evidence <- renderUI({
    summary <- selected_research_source_summary()
    summary_text <- article_lab_input_multiline(input$research_summary_text) %||% if (nrow(summary) > 0) summary$summary_text[[1]] else ""
    if (!nzchar(trimws(summary_text))) return(div(class = "lab-status-copy", "Add or select a summary to mark evidence-worthy claim sentences."))
    blocks <- research_summary_line_blocks(summary_text)
    if (length(blocks) == 0) return(div(class = "lab-status-copy", "No summary sentences found."))
    rows <- if (nrow(summary) > 0) research_prepare_evidence_marker_rows(research_latest_evidence_by_claim(load_research_summary_evidence_rows(con, summary$summary_id[[1]]))) else data.frame()
    row_by_index <- list()
    if (nrow(rows) > 0) {
      for (i in seq_len(nrow(rows))) {
        key <- as.character(rows$claim_index[[i]])
        row_by_index[[key]] <- c(row_by_index[[key]] %||% list(), list(rows[i, , drop = FALSE]))
      }
    }
    selected_group_key <- selected_research_evidence_group_key()
    selected_group_rows <- data.frame()
    if (nrow(rows) > 0 && !is.na(selected_group_key) && selected_group_key %in% rows$marker_group_key) {
      selected_group_rows <- rows[rows$marker_group_key == selected_group_key, , drop = FALSE]
    }
    marker_for_group <- function(marker_rows) {
      if (is.null(marker_rows) || length(marker_rows) == 0) return(NULL)
      group_rows <- do.call(rbind, marker_rows)
      group_rows <- group_rows[order(group_rows$display_index, group_rows$claim_id), , drop = FALSE]
      group_key <- group_rows$marker_group_key[[1]]
      label <- research_marker_label(group_rows$display_index)
      status <- research_group_marker_status(group_rows)
      active <- !is.na(selected_group_key) && identical(group_key, selected_group_key)
      if (nrow(group_rows) == 1L) {
        row <- group_rows[1, , drop = FALSE]
        fetched <- !is.na(row$evidence_id[[1]])
        quote_rows <- if (fetched) research_quote_rows_for_evidence(con, row$evidence_id[[1]], row$sentence_id[[1]]) else data.frame()
        has_quote <- nrow(quote_rows) > 0 && !(research_marker_status(row) %in% c("generally_supported_no_direct_quote", "no_match"))
        tooltip <- if (fetched && has_quote) {
          quote_page <- if (is.na(quote_rows$page_number[[1]])) "page unavailable" else paste("page", quote_rows$page_number[[1]])
          span(
            class = "research-citation-tooltip",
            span(class = "research-citation-tooltip-label", sprintf("Marker %s", label)),
            span(class = "research-citation-tooltip-meta", sprintf("%s · confidence: %s", research_status_label(research_marker_status(row)), row$confidence[[1]] %||% "none")),
            span(class = "research-citation-tooltip-quote-preview", research_truncate(quote_rows$sentence_text[[1]], max_chars = 96L)),
            span(class = "research-citation-tooltip-meta", quote_page)
          )
        } else if (fetched) {
          span(
            class = "research-citation-tooltip",
            span(class = "research-citation-tooltip-label", sprintf("Marker %s", label)),
            span(class = "research-citation-tooltip-claim", research_truncate(row$claim_text[[1]], max_chars = 150L)),
            span(class = "research-citation-tooltip-meta", sprintf("%s · confidence: %s", research_status_label(research_marker_status(row)), row$confidence[[1]] %||% "none")),
            span(class = "research-citation-tooltip-meta", "Click to inspect evidence")
          )
        } else {
          span(
            class = "research-citation-tooltip",
            span(class = "research-citation-tooltip-label", sprintf("Marker %s", label)),
            span(class = "research-citation-tooltip-claim", research_truncate(row$claim_text[[1]], max_chars = 150L)),
            span(class = "research-citation-tooltip-meta", sprintf("Evidence not fetched yet · marker model: %s", row$marker_model[[1]] %||% "")),
            span(class = "research-citation-tooltip-meta", "Click to inspect evidence")
          )
        }
      } else {
        tooltip <- span(
          class = "research-citation-tooltip",
          span(class = "research-citation-tooltip-label", sprintf("Markers %s", label)),
          span(class = "research-citation-tooltip-meta", research_group_status_summary(group_rows)),
          span(class = "research-citation-tooltip-meta", "Click to inspect evidence")
        )
      }
      tags$button(
        type = "button",
        class = paste(
          "btn lab-secondary research-citation-marker",
          paste0("status-", gsub("[^a-z0-9_]+", "_", tolower(status))),
          if (isTRUE(active)) "active" else ""
        ),
        onclick = sprintf("Shiny.setInputValue('research_select_evidence_group', %s, {priority:'event'})", toJSON(group_key, auto_unbox = TRUE)),
        span(class = "research-citation-marker-label", label),
        tooltip
      )
    }
    block_nodes <- lapply(blocks, function(block) {
      if (identical(block$type, "blank")) return(div(class = "research-summary-spacer"))
      unit_nodes <- lapply(block$units, function(unit) {
        marker_rows <- row_by_index[[as.character(unit$sentence_index)]] %||% list()
        tagList(span(unit$text), marker_for_group(marker_rows), " ")
      })
      if (identical(block$type, "heading")) {
        div(class = "research-summary-line research-summary-heading", unit_nodes)
      } else if (identical(block$type, "list_item")) {
        div(class = "research-summary-line research-summary-list-item", span(class = "research-summary-list-prefix", block$prefix), span(unit_nodes))
      } else {
        div(class = "research-summary-line research-summary-paragraph", unit_nodes)
      }
    })
    div(
      class = "research-evidence-review-layout",
      div(
        class = "research-evidence-summary-pane lab-card",
        h4("Summary with claim markers"),
        p(class = "lab-status-copy", "Markers show detected claim sentences first; source evidence appears after running Find source sentences."),
          div(class = "lab-status-copy research-inline-summary", block_nodes),
          if (nrow(selected_group_rows) == 0) div(class = "lab-status-copy", "Hover over a marker for a quick preview. Click a marker to inspect evidence in the side panel.") else NULL
      )
    )
  })

  output$research_summary_evidence_table <- renderUI({
    summary <- selected_research_source_summary()
    if (nrow(summary) == 0) return(div(class = "lab-status-copy", "Save or confirm the current summary before generating evidence suggestions."))
    rows <- load_research_summary_evidence_rows(con, summary$summary_id[[1]])
    if (nrow(rows) == 0) return(div(class = "lab-status-copy", "No evidence suggestions yet."))
    tags$details(
      class = "lab-card",
      tags$summary("Debug/review claim rows"),
      lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      status <- row$selection_status[[1]] %||% "suggested"
      quote_rows <- if (!is.na(row$evidence_id[[1]])) research_quote_rows_for_evidence(con, row$evidence_id[[1]], row$sentence_id[[1]]) else data.frame()
      div(
        class = "lab-card",
        h4(sprintf("Claim %s · %s", row$claim_id[[1]], research_status_label(status))),
        p(class = "lab-status-copy", row$claim_text[[1]]),
        if (nrow(quote_rows) > 0) {
          tagList(
            tagList(lapply(seq_len(nrow(quote_rows)), function(qi) {
              quote_page <- if (is.na(quote_rows$page_number[[qi]])) "page unavailable" else paste("page", quote_rows$page_number[[qi]])
              tagList(
                div(class = "research-pdf-quote", quote_rows$sentence_text[[qi]]),
                p(class = "lab-status-copy", quote_page)
              )
            })),
            p(class = "lab-status-copy", sprintf("confidence: %s · model: %s", row$confidence[[1]] %||% "", row$model[[1]] %||% ""))
          )
        } else {
          p(class = "lab-status-copy", "No supporting sentence selected.")
        },
        if (!is.na(row$selector_reason[[1]]) && nzchar(row$selector_reason[[1]])) p(class = "lab-status-copy", strong("Reason: "), row$selector_reason[[1]]) else NULL,
        if (!is.na(row$evidence_id[[1]])) div(
          class = "lab-actions",
          tags$button(type = "button", class = "btn lab-primary", onclick = sprintf("Shiny.setInputValue('research_verify_evidence', %s, {priority:'event'})", row$evidence_id[[1]]), "Verify"),
          tags$button(type = "button", class = "btn lab-secondary", onclick = sprintf("Shiny.setInputValue('research_reject_evidence', %s, {priority:'event'})", row$evidence_id[[1]]), "Reject")
        ) else NULL
      )
      })
    )
  })

  output$research_summary_evidence_overlay <- renderUI({
    if (!identical(active_section(), "summary")) return(NULL)
    summary <- selected_research_source_summary()
    rows <- if (nrow(summary) > 0) research_prepare_evidence_marker_rows(research_latest_evidence_by_claim(load_research_summary_evidence_rows(con, summary$summary_id[[1]]))) else data.frame()
    selected_group_key <- selected_research_evidence_group_key()
    selected_group_rows <- data.frame()
    if (nrow(rows) > 0 && !is.na(selected_group_key) && nzchar(selected_group_key) && selected_group_key %in% rows$marker_group_key) {
      selected_group_rows <- rows[rows$marker_group_key == selected_group_key, , drop = FALSE]
    }

    claim_panel <- function(row) {
      status <- research_marker_status(row)
      fetched <- !is.na(row$evidence_id[[1]])
      quote_rows <- if (fetched) research_quote_rows_for_evidence(con, row$evidence_id[[1]], row$sentence_id[[1]]) else data.frame()
      show_quotes <- fetched && nrow(quote_rows) > 0 && !(status %in% c("generally_supported_no_direct_quote", "no_match"))
      div(
        class = "research-evidence-claim-section",
        div(
          class = "research-evidence-claim-header",
          h5(sprintf("Claim %s", research_marker_label(row$display_index[[1]]))),
          span(class = paste("research-evidence-status-pill", paste0("status-", gsub("[^a-z0-9_]+", "_", tolower(status)))), research_status_label(status))
        ),
        p(class = "research-evidence-claim-text", row$claim_text[[1]]),
        if (show_quotes) {
          tagList(
            div(class = "research-pdf-quote-label", if (identical(status, "rejected")) "Rejected PDF quote(s)" else "PDF quote(s)"),
            tagList(lapply(seq_len(nrow(quote_rows)), function(i) {
              quote_page <- if (is.na(quote_rows$page_number[[i]])) "page unavailable" else paste("page", quote_rows$page_number[[i]])
              tagList(div(class = "research-pdf-quote", quote_rows$sentence_text[[i]]), p(class = "lab-status-copy", quote_page))
            }))
          )
        } else if (fetched) {
          p(class = "lab-status-copy", if (identical(status, "generally_supported_no_direct_quote")) "Paper seems to support this generally, but no direct quote was selected." else "No matching source sentence selected.")
        } else {
          p(class = "lab-status-copy", "Source evidence not fetched yet.")
        },
        if (fetched) p(class = "lab-status-copy", sprintf("Confidence: %s · selector model: %s · reasoning: %s", row$confidence[[1]] %||% "none", row$model[[1]] %||% "", row$reasoning_effort[[1]] %||% "")) else p(class = "lab-status-copy", sprintf("Marker model: %s · reasoning: %s", row$marker_model[[1]] %||% "", row$marker_reasoning_effort[[1]] %||% "")),
        if (fetched && !is.na(row$selector_reason[[1]]) && nzchar(row$selector_reason[[1]])) p(class = "lab-status-copy", strong("Reason: "), row$selector_reason[[1]]) else NULL,
        tags$details(
          class = "lab-secondary-details",
          tags$summary("Prompt/run metadata"),
          h5(if (fetched) "Evidence selector prompt template" else "Claim marker prompt template"),
          tags$pre(class = "lab-status-copy", if (fetched) row$prompt_template[[1]] %||% "" else row$marker_prompt_template[[1]] %||% ""),
          h5(if (fetched) "Evidence selector prompt payload JSON" else "Claim marker prompt payload JSON"),
          tags$pre(class = "lab-status-copy", if (fetched) row$prompt_payload_json[[1]] %||% "" else row$marker_prompt_payload_json[[1]] %||% "")
        ),
        if (fetched && !is.na(row$evidence_id[[1]])) div(
          class = "lab-actions research-evidence-actions",
          tags$button(type = "button", class = "btn lab-primary", onclick = sprintf("Shiny.setInputValue('research_verify_evidence', %s, {priority:'event'})", row$evidence_id[[1]]), "Verify"),
          tags$button(type = "button", class = "btn lab-secondary", onclick = sprintf("Shiny.setInputValue('research_reject_evidence', %s, {priority:'event'})", row$evidence_id[[1]]), "Reject")
        ) else NULL
      )
    }

    if (nrow(selected_group_rows) == 0) {
      return(div(
        class = "research-evidence-side-overlay hidden",
        div(
          class = "research-evidence-side-overlay-header",
          div(div(class = "research-evidence-drawer-title", "Evidence"), div(class = "research-evidence-drawer-subtitle", "No marker selected")),
          tags$button(type = "button", class = "btn lab-secondary research-evidence-drawer-close", onclick = "Shiny.setInputValue('research_close_evidence_drawer', Date.now(), {priority:'event'})", "Close")
        ),
        div(class = "research-evidence-panel-body", p(class = "lab-status-copy", "Click a marker to inspect evidence."))
      ))
    }

    selected_group_rows <- selected_group_rows[order(selected_group_rows$display_index, selected_group_rows$claim_id), , drop = FALSE]
    selected_label <- research_marker_label(selected_group_rows$display_index)
    original_text <- article_lab_input_string(selected_group_rows$original_text[[1]]) %||% ""
    claim_count <- nrow(selected_group_rows)
    div(
      class = "research-evidence-side-overlay",
      div(
        class = "research-evidence-side-overlay-header",
        div(
          div(class = "research-evidence-drawer-title", "Evidence"),
          div(class = "research-evidence-drawer-subtitle", sprintf("Marker %s · %s claim%s · %s", selected_label, claim_count, ifelse(claim_count == 1, "", "s"), research_group_status_summary(selected_group_rows)))
        ),
        tags$button(type = "button", class = "btn lab-secondary research-evidence-drawer-close", onclick = "Shiny.setInputValue('research_close_evidence_drawer', Date.now(), {priority:'event'})", "Close")
      ),
      div(
        class = "research-evidence-panel-body",
        div(class = "research-evidence-selected-summary", strong("Selected summary sentence/bullet"), p(original_text)),
        tagList(lapply(seq_len(nrow(selected_group_rows)), function(i) claim_panel(selected_group_rows[i, , drop = FALSE])))
      )
    )
  })

  output$article_lab_research_summary_selector <- renderUI({
    rows <- confirmed_research_summaries()
    choices <- if (nrow(rows) == 0) character() else setNames(
      rows$summary_id,
      sprintf("%s · %s", rows$source_title, rows$confirmed_at %||% rows$updated_at)
    )
    empty_choice <- stats::setNames("", "")
    selectizeInput("article_lab_research_summary_id", "Research summary inspiration", choices = c(empty_choice, choices), selected = "", width = "100%")
  })

  output$article_lab_effective_prompt <- renderUI({
    effective <- article_lab_effective_generation_inputs()
    summary_mode <- identical(effective$mode, "research_summary")
    mode_copy <- if (summary_mode) {
      sprintf(
        "Research summary mode: Generate will use the manual/default prompt as title guidance, ignore the manual seed/topic and manual inspiration-source dropdown, and use confirmed summary %s (%s) as the article summary.",
        effective$summary_id,
        effective$source_title
      )
    } else {
      "Manual mode: Generate will use the manual/default prompt textarea, optional seed/topic, and optional inspiration-source dropdown below."
    }
    request_additions <- paste(
      sprintf("Batch size: %s", input$article_lab_batch_size %||% 12L),
      sprintf("Model: %s", input$article_lab_model %||% article_lab_default_model),
      sprintf("Seed topic: %s", article_lab_input_string(effective$seed_topic) %||% "(none)"),
      sprintf("Inspiration source: %s", article_lab_input_string(effective$inspiration_source) %||% "(none)"),
      sep = "\n"
    )
    example_titles <- if (identical(article_lab_input_string(effective$inspiration_source), "top performing titles")) {
      article_lab_top_title_examples(con, limit = 8L)
    } else {
      character()
    }
    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", mode_copy),
      tags$details(
        open = if (summary_mode) "open" else NULL,
        tags$summary("Show exact effective prompt"),
        h4("Title helper wrapper"),
        tags$pre(class = "lab-status-copy", paste(
          "You generate Medium-style article title candidates for personal finance and investing.",
          "Return valid JSON only in the shape {\"titles\": [\"...\", \"...\"]}.",
          sprintf("Return exactly %s titles.", input$article_lab_batch_size %||% 12L),
          sprintf("Every title must be at most %s characters, including spaces.", article_lab_title_max_chars),
          sprintf("Prefer %s-%s characters when possible. Do not make titles long unless the extra words clearly improve clarity or curiosity.", article_lab_title_preferred_min_chars, article_lab_title_preferred_max_chars),
          "Do not include explanations, numbering, markdown, or code fences.",
          "Do not copy any example title verbatim.",
          "Keep the titles credible, science-based, beginner-friendly, and not clickbait.",
          "If a title would exceed the limit, rewrite it shorter instead of truncating it.",
          sep = "\n"
        )),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        if (summary_mode && nzchar(trimws(effective$manual_prompt %||% ""))) tagList(
          h4("Manual/default prompt"),
          tags$pre(class = "lab-status-copy", effective$manual_prompt)
        ),
        if (length(example_titles) > 0) tagList(
          h4("Reference examples sent as inspiration"),
          tags$pre(class = "lab-status-copy", paste(sprintf("%s. %s", seq_along(example_titles), example_titles), collapse = "\n"))
        ),
        h4("Article summary"),
        tags$pre(class = "lab-status-copy", effective$prompt),
        if (nzchar(trimws(effective$context_notes %||% ""))) tagList(
          h4("Article context notes"),
          tags$pre(class = "lab-status-copy", effective$context_notes)
        )
      )
    )
  })

  output$research_summary_effective_prompt <- renderUI({
    source <- selected_research_source()
    asset <- selected_research_pdf_asset()
    prompt_text <- article_lab_input_multiline(input$research_summary_api_prompt) %||% article_lab_default_research_summary_prompt
    pdf_status <- if (nrow(asset) == 0) "missing" else research_input_default(asset$status[[1]], "missing")
    local_pdf_path <- if (nrow(asset) == 0) NA_character_ else research_input_value(asset$local_path[[1]])
    resolved_pdf_path <- research_resolve_local_pdf_path(local_pdf_path)
    request_fields <- paste(
      sprintf("Model: %s", article_lab_input_string(input$research_summary_model) %||% article_lab_default_research_summary_model),
      sprintf("Prompt version: %s", article_lab_input_string(input$research_summary_prompt_version) %||% article_lab_default_research_summary_prompt_version),
      sprintf("PDF attachment status: %s", pdf_status),
      sprintf("PDF attachment filename/path: %s", resolved_pdf_path %||% "(none)"),
      sep = "\n"
    )
    metadata_text <- if (nrow(source) == 0) {
      "(No source selected. Select a source to see the exact source metadata sent with the PDF.)"
    } else {
      paste(
        "Source metadata:",
        sprintf("Research source ID: %s", source$research_source_id[[1]] %||% ""),
        sprintf("Source title: %s", article_lab_input_string(source$source_title[[1]]) %||% ""),
        sprintf("Source URL: %s", article_lab_input_string(source$source_url[[1]]) %||% ""),
        sprintf("PDF URL: %s", article_lab_input_string(source$pdf_url[[1]]) %||% ""),
        "Main idea:",
        article_lab_input_multiline(source$main_idea[[1]]) %||% "",
        "",
        "Abstract:",
        article_lab_input_multiline(source$abstract[[1]]) %||% "",
        "",
        "User prompt:",
        prompt_text,
        sep = "\n"
      )
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Summary generation sends the selected PDF as an input_file plus this text metadata/prompt payload."),
      tags$details(
        open = if (nrow(source) > 0) "open" else NULL,
        tags$summary("Show exact research summary API prompt"),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_fields),
        h4("Input text sent with PDF"),
        tags$pre(class = "lab-status-copy", metadata_text)
      )
    )
  })

  output$article_lab_score_effective_prompt <- renderUI({
    queue_rows <- article_lab_queue_rows()
    selected_ids <- collect_selected_ids(
      queue_rows,
      "article_lab_queue_select",
      snapshot_ids = input$article_lab_queue_selected_snapshot
    )
    selected_rows <- if (length(selected_ids) > 0 && nrow(queue_rows) > 0) {
      queue_rows[queue_rows$candidate_id %in% selected_ids, , drop = FALSE]
    } else {
      queue_rows[0, , drop = FALSE]
    }
    model <- article_lab_input_string(input$article_lab_score_model) %||% article_lab_default_score_model
    prompt_version <- article_lab_input_string(input$article_lab_score_prompt_version) %||% article_lab_default_score_prompt_version
    scope <- article_lab_input_string(input$article_lab_score_scope) %||% article_lab_default_score_scope
    request_fields <- paste(
      sprintf("Model: %s", model),
      sprintf("Prompt version: %s", prompt_version),
      sprintf("Scope: %s", scope),
      "Response format: strict JSON schema with curiosity, emotional_pull, medium_comment_potential, overall_article_potential, trust_risk, predicted_success_bucket, and short_reason.",
      sep = "\n"
    )
    user_prompts <- if (nrow(selected_rows) == 0) {
      "(No selected API-queue titles. Select title checkboxes to see the exact per-title user prompt that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_rows)), function(i) {
        paste(
          sprintf("candidate_id=%s | batch_id=%s", selected_rows$candidate_id[[i]], selected_rows$batch_id[[i]]),
          article_lab_score_user_prompt(prompt_version, scope, selected_rows$title[[i]]),
          sep = "\n\n"
        )
      }, character(1)), collapse = "\n\n---\n\n")
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "API scoring sends one request per selected title. Each request uses this system prompt plus the per-title user prompt below."),
      tags$details(
        open = if (nrow(selected_rows) > 0) "open" else NULL,
        tags$summary("Show exact title scoring API prompt"),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_fields),
        h4("System prompt"),
        tags$pre(class = "lab-status-copy", article_lab_score_system_prompt),
        h4("Per-title user prompt"),
        tags$pre(class = "lab-status-copy", user_prompts)
      )
    )
  })

  output$article_lab_subtitle_effective_prompt <- renderUI({
    targets <- article_lab_subtitle_target_rows()
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(targets$batch_id))
    has_summary <- nrow(summary_contexts) > 0
    variants_per_title <- max(1L, min(8L, suppressWarnings(as.integer(input$article_lab_subtitle_variants_per_title)) %||% 4L))
    base_prompt <- article_lab_input_multiline(input$article_lab_subtitle_prompt) %||% article_lab_default_subtitle_prompt
    request_additions <- paste(
      sprintf("Model: %s", article_lab_input_string(input$article_lab_subtitle_model) %||% article_lab_default_subtitle_model),
      sprintf("Subtitle candidates per title: %s", variants_per_title),
      sprintf("Max subtitle characters: %s", article_lab_subtitle_max_chars),
      sep = "\n"
    )
    title_list <- if (nrow(targets) == 0) {
      "(No eligible approved titles in the current batch filter.)"
    } else {
      paste(vapply(seq_len(nrow(targets)), function(i) {
        sprintf("%s. candidate_id=%s | batch_id=%s | title=%s", i, targets$candidate_id[[i]], targets$batch_id[[i]], targets$title[[i]])
      }, character(1)), collapse = "\n")
    }
    summary_copy <- if (has_summary) {
      "Subtitle generation will append the confirmed article summary attached to each title's source batch."
    } else {
      "No attached research summary was found for the eligible titles in the current batch filter. Subtitle generation will use the base prompt and titles only."
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", summary_copy),
      tags$details(
        open = if (has_summary) "open" else NULL,
        tags$summary("Show exact effective prompt"),
        h4("Subtitle prompt"),
        tags$pre(class = "lab-status-copy", base_prompt),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        h4("Titles"),
        tags$pre(class = "lab-status-copy", title_list),
        if (has_summary) tagList(
          h4("Attached article summaries"),
          tags$pre(class = "lab-status-copy", paste(vapply(seq_len(nrow(summary_contexts)), function(i) {
            paste(
              sprintf("Batch: %s", summary_contexts$batch_id[[i]]),
              sprintf("Summary ID: %s", summary_contexts$summary_id[[i]]),
              sprintf("Source title: %s", summary_contexts$source_title[[i]] %||% ""),
              summary_contexts$article_summary[[i]],
              sep = "\n"
            )
          }, character(1)), collapse = "\n\n---\n\n"))
        )
      )
    )
  })

  output$article_lab_thumbnail_effective_prompt <- renderUI({
    packages <- article_lab_thumbnail_package_rows()
    variants_per_package <- max(1L, min(4L, suppressWarnings(as.integer(input$article_lab_thumbnail_variants_per_package)) %||% article_lab_default_thumbnail_variants))
    base_prompt <- article_lab_input_multiline(input$article_lab_thumbnail_prompt) %||% article_lab_default_thumbnail_prompt
    selected_ids <- collect_selected_ids(
      packages,
      "article_lab_thumbnail_package_select",
      snapshot_ids = input$article_lab_thumbnail_packages_selected_snapshot,
      key_col = "subtitle_id"
    )
    selected_packages <- if (length(selected_ids) > 0 && nrow(packages) > 0) {
      packages[packages$subtitle_id %in% selected_ids, , drop = FALSE]
    } else {
      packages[0, , drop = FALSE]
    }
    request_additions <- paste(
      sprintf("Model: %s", article_lab_input_string(input$article_lab_thumbnail_model) %||% article_lab_default_thumbnail_model),
      "Image generation: Responses API built-in image_generation tool",
      sprintf("Thumbnail candidates per package: %s", variants_per_package),
      sep = "\n"
    )
    package_list <- if (nrow(selected_packages) == 0) {
      "(No selected eligible title/subtitle packages. Select package checkboxes to see the exact package context that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        sprintf(
          "%s. subtitle_id=%s | candidate_id=%s | batch_id=%s\nTitle: %s\nSubtitle: %s",
          i,
          selected_packages$subtitle_id[[i]],
          selected_packages$candidate_id[[i]],
          selected_packages$batch_id[[i]],
          selected_packages$title[[i]],
          selected_packages$subtitle[[i]]
        )
      }, character(1)), collapse = "\n\n")
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Thumbnail generation sends this prompt plus the selected title/subtitle package context to the selected Responses model, which calls the built-in image_generation tool."),
      tags$details(
        open = if (nrow(selected_packages) > 0) "open" else NULL,
        tags$summary("Show exact thumbnail API prompt"),
        h4("Thumbnail prompt"),
        tags$pre(class = "lab-status-copy", base_prompt),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        h4("Selected title/subtitle packages"),
        tags$pre(class = "lab-status-copy", package_list)
      )
    )
  })

  output$article_lab_outline_context_toggle <- renderUI({
    packages <- article_lab_ready_for_outline_rows()
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(packages$batch_id))
    pdf_href <- NA_character_
    if (nrow(summary_contexts) > 0) {
      pdf_urls <- clean_text(summary_contexts$pdf_url)
      pdf_urls <- pdf_urls[!is.na(pdf_urls)]
      if (length(pdf_urls) > 0) pdf_href <- pdf_urls[[1]]
      if (is.na(pdf_href)) {
        local_paths <- vapply(summary_contexts$pdf_local_path, research_resolve_local_pdf_path, character(1))
        local_paths <- local_paths[!is.na(local_paths) & file.exists(local_paths)]
        if (length(local_paths) > 0) {
          pdf_href <- paste0("file://", URLencode(normalizePath(local_paths[[1]], mustWork = TRUE), reserved = TRUE))
        }
      }
    }
    pdf_label <- if (is.na(pdf_href)) {
      '<span style="color:#1a73e8;font-size:0.85em;font-weight:700;letter-spacing:0.03em;">PDF</span>'
    } else {
      sprintf(
        '<a href="%s" target="_blank" rel="noopener noreferrer" style="color:#1a73e8;font-size:0.85em;font-weight:700;letter-spacing:0.03em;text-decoration:underline;">PDF</a>',
        htmltools::htmlEscape(pdf_href)
      )
    }
    div(
      class = "lab-field",
      checkboxInput(
        "article_lab_outline_include_context",
        HTML(paste0("Include available research context ", pdf_label, " preferred, text fallback")),
        value = TRUE,
        width = "100%"
      )
    )
  })

  output$article_lab_outline_effective_prompt <- renderUI({
    packages <- article_lab_ready_for_outline_rows()
    selected_ids <- collect_selected_ids(
      packages,
      "article_lab_outline_packages",
      snapshot_ids = input$article_lab_outline_packages_selected_snapshot,
      key_col = "thumbnail_id"
    )
    selected_packages <- if (length(selected_ids) > 0 && nrow(packages) > 0) {
      packages[packages$thumbnail_id %in% selected_ids, , drop = FALSE]
    } else {
      packages[0, , drop = FALSE]
    }
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(packages$batch_id))
    include_context <- isTRUE(input$article_lab_outline_include_context)
    base_prompt <- article_lab_input_multiline(input$article_lab_outline_prompt) %||% article_lab_default_outline_prompt
    request_additions <- paste(
      sprintf("Model: %s", article_lab_input_string(input$article_lab_outline_model) %||% article_lab_default_outline_model),
      sprintf("Include available research context: %s", if (include_context) "yes" else "no"),
      "Response format: JSON with one Markdown outline_text per selected package.",
      sep = "\n"
    )
    context_payload <- if (!include_context) {
      "(Research context is available only if listed above, but the include-context toggle is off.)"
    } else if (nrow(summary_contexts) == 0) {
      "(No research context will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(summary_contexts)), function(i) {
        pdf_path <- research_resolve_local_pdf_path(summary_contexts$pdf_local_path[[i]])
        has_pdf <- !is.na(pdf_path) && file.exists(pdf_path)
        if (has_pdf) {
          paste(
            sprintf("Batch: %s", summary_contexts$batch_id[[i]]),
            "Context sent: PDF file attachment",
            "Text summary sent: no, because the PDF itself is attached",
            sep = "\n"
          )
        } else {
          paste(
            sprintf("Batch: %s", summary_contexts$batch_id[[i]]),
            "Context sent: text summary fallback",
            sprintf("Summary ID: %s", summary_contexts$summary_id[[i]]),
            sprintf("Source title: %s", summary_contexts$source_title[[i]] %||% ""),
            "Exact text sent to API:",
            summary_contexts$article_summary[[i]],
            sep = "\n"
          )
        }
      }, character(1)), collapse = "\n\n---\n\n")
    }
    package_list <- if (nrow(selected_packages) == 0) {
      "(No selected title/subtitle/thumbnail packages. Select Generate outline or Regenerate outline checkboxes to see the exact package context that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        sprintf(
          "%s. thumbnail_id=%s | subtitle_id=%s | candidate_id=%s | batch_id=%s\nTitle: %s\nSubtitle: %s\nThumbnail label: %s",
          i,
          selected_packages$thumbnail_id[[i]],
          selected_packages$subtitle_id[[i]],
          selected_packages$candidate_id[[i]],
          selected_packages$batch_id[[i]],
          selected_packages$title[[i]],
          selected_packages$subtitle[[i]],
          selected_packages$thumbnail_label[[i]] %||% "approved thumbnail"
        )
      }, character(1)), collapse = "\n\n")
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Outline generation sends this prompt plus selected title/subtitle/thumbnail package context. When enabled, a local PDF is attached as a file; summary text is sent only when no local PDF is available."),
      tags$details(
        open = if (nrow(selected_packages) > 0 || nrow(summary_contexts) > 0) "open" else NULL,
        tags$summary("Show exact outline API prompt"),
        h4("Outline prompt"),
        tags$pre(class = "lab-status-copy", base_prompt),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        h4("Selected packages"),
        tags$pre(class = "lab-status-copy", package_list),
        h4("Research context sent"),
        tags$pre(class = "lab-status-copy", context_payload)
      )
    )
  })

  output$article_lab_full_text_effective_prompt <- renderUI({
    rows <- article_lab_full_text_rows()
    packages <- article_lab_full_text_package_rows(rows)
    selected_ids <- collect_selected_ids(packages, "article_lab_full_text_packages", snapshot_ids = input$article_lab_full_text_packages_selected_snapshot, key_col = "outline_id")
    if (length(selected_ids) > 1) selected_ids <- selected_ids[[1]]
    selected_packages <- if (length(selected_ids) > 0 && nrow(packages) > 0) packages[packages$outline_id %in% selected_ids, , drop = FALSE] else packages[0, , drop = FALSE]
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(packages$batch_id))
    include_context <- isTRUE(input$article_lab_full_text_include_context)
    base_prompt <- article_lab_input_multiline(input$article_lab_full_text_prompt) %||% article_lab_default_full_text_prompt
    prompt_key <- article_lab_input_string(input$article_lab_full_text_prompt_key) %||% article_lab_full_text_prompt_key
    request_additions <- paste(
      sprintf("Model: %s", article_lab_input_string(input$article_lab_full_text_model) %||% article_lab_default_full_text_model),
      sprintf("Prompt key/version: %s", prompt_key),
      sprintf("Include available source context: %s", if (include_context) "yes" else "no"),
      "Response format: JSON {\"results\":[{\"outline_id\",\"thumbnail_id\",\"subtitle_id\",\"candidate_id\",\"batch_id\",\"source_context_mode\",\"full_text\",\"citation_map\":[...]}]}",
      "The public article must use indirect citations/paraphrases only. Every in-text citation must appear in citation_map.",
      sep = "\n"
    )
    package_list <- if (nrow(selected_packages) == 0) {
      "(No selected approved outline. Select one outline checkbox to see the exact package context that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        sprintf(
          "%s. outline_id=%s | thumbnail_id=%s | subtitle_id=%s | candidate_id=%s | batch_id=%s\nTitle: %s\nSubtitle: %s\nThumbnail concept: %s\nApproved outline:\n%s",
          i,
          selected_packages$outline_id[[i]], selected_packages$thumbnail_id[[i]], selected_packages$subtitle_id[[i]], selected_packages$candidate_id[[i]], selected_packages$batch_id[[i]],
          selected_packages$title[[i]], selected_packages$subtitle[[i]], selected_packages$thumbnail_label[[i]] %||% "approved thumbnail", selected_packages$outline_text[[i]]
        )
      }, character(1)), collapse = "\n\n---\n\n")
    }
    exact_package_list <- if (nrow(selected_packages) == 0) {
      "(No selected approved outline. Select one outline checkbox to see the exact package context that will be sent.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        context <- if (nrow(summary_contexts) == 0) data.frame() else summary_contexts[summary_contexts$batch_id == selected_packages$batch_id[[i]], , drop = FALSE]
        summary_id <- if (nrow(context) > 0) research_input_integer(context$summary_id[[1]]) else NA_integer_
        checked_evidence <- if (isTRUE(include_context) && !is.na(summary_id)) build_checked_summary_evidence(con, summary_id) else list()
        pdf_path <- if (nrow(context) > 0 && isTRUE(include_context)) research_resolve_local_pdf_path(context$pdf_local_path[[1]]) else NA_character_
        has_pdf <- !is.na(pdf_path) && file.exists(pdf_path)
        has_evidence <- length(checked_evidence) > 0
        source_mode <- if (!isTRUE(include_context)) "none" else if (has_pdf) "pdf_attachment" else if (has_evidence) "checked_summary_evidence" else "none"
        lines <- c(
          sprintf(
            "%s. outline_id=%s | thumbnail_id=%s | subtitle_id=%s | candidate_id=%s | batch_id=%s",
            i,
            selected_packages$outline_id[[i]], selected_packages$thumbnail_id[[i]], selected_packages$subtitle_id[[i]], selected_packages$candidate_id[[i]], selected_packages$batch_id[[i]]
          ),
          sprintf("Title: %s", selected_packages$title[[i]]),
          sprintf("Subtitle: %s", selected_packages$subtitle[[i]]),
          sprintf("Thumbnail concept: %s", selected_packages$thumbnail_label[[i]] %||% "approved thumbnail"),
          "Approved outline:",
          selected_packages$outline_text[[i]],
          sprintf("Source context mode: %s", source_mode)
        )
        if (identical(source_mode, "pdf_attachment")) lines <- c(lines, "Research PDF: attached as input_file")
        if (has_evidence) {
          lines <- c(lines, "Checked summary evidence (use these to ground paper-based claims):")
          for (item in checked_evidence) {
            ids <- paste(item$sentence_ids %||% character(), collapse = ", ")
            page <- item$page %||% "n/a"
            quote <- item$supporting_quote %||% "n/a"
            claim <- item$claim_text %||% "n/a"
            lines <- c(lines, sprintf("- Claim: %s | Quote: %s | page: %s | sentence_ids: [%s] | status: %s | confidence: %s", claim, quote, page, if (nzchar(ids)) ids else "n/a", item$selection_status %||% "n/a", item$confidence %||% "n/a"))
          }
        }
        paste(lines, collapse = "\n")
      }, character(1)), collapse = "\n\n")
    }
    exact_api_prompt <- paste(
      base_prompt,
      "Return valid JSON only.",
      "Return JSON only in this shape: {\"results\":[{\"outline_id\":string,\"thumbnail_id\":string,\"subtitle_id\":string,\"candidate_id\":string,\"batch_id\":string,\"source_context_mode\":\"pdf_attachment\"|\"checked_summary_evidence\"|\"none\",\"full_text\":string,\"citation_map\":[{\"citation_text\":string,\"article_sentence\":string,\"source_title\":string,\"source_author_or_org\":string,\"source_year\":string|null,\"page\":string|null,\"sentence_ids\":[string],\"supporting_quote\":string|null,\"verification_note\":string,\"evidence_status\":\"checked\"|\"unchecked\"}]}]}",
      "Copy ids exactly from the package. The full_text value must be the complete Markdown article draft, not a schema example, MARKDOWN_ARTICLE_HERE, placeholder, excerpt, note, or explanation.",
      "Ignore any earlier placeholder value such as MARKDOWN_ARTICLE_HERE; replace it with the actual full Markdown article.",
      "Return one full article draft per package, preserving all ids exactly.",
      "Packages:",
      exact_package_list,
      sep = "\n\n"
    )
    context_payload <- if (!include_context) {
      "(Source context toggle is off. No PDF or summary text will be sent.)"
    } else if (nrow(selected_packages) == 0) {
      "(Select approved outlines to see source context for those packages.)"
    } else {
      paste(vapply(seq_len(nrow(selected_packages)), function(i) {
        context <- if (nrow(summary_contexts) == 0) data.frame() else summary_contexts[summary_contexts$batch_id == selected_packages$batch_id[[i]], , drop = FALSE]
        summary_id <- if (nrow(context) > 0) research_input_integer(context$summary_id[[1]]) else NA_integer_
        pdf_path <- if (nrow(context) > 0) research_resolve_local_pdf_path(context$pdf_local_path[[1]]) else NA_character_
        has_pdf <- !is.na(pdf_path) && file.exists(pdf_path)
        lines <- c(sprintf("Outline: %s", selected_packages$outline_id[[i]]))
        if (has_pdf) {
          lines <- c(lines, "Context sent: PDF file attachment", sprintf("Attached local file/path: %s", pdf_path))
        }
        evidence <- if (isTRUE(include_context) && !is.na(summary_id)) build_checked_summary_evidence(con, summary_id) else list()
        if (length(evidence) > 0) {
          lines <- c(lines, sprintf("Checked evidence rows: %s", length(evidence)))
        }
        paste(lines, collapse = "\n")
      }, character(1)), collapse = "\n\n---\n\n")
    }

    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Full article generation sends this prompt plus the selected approved outline context. Use indirect citations/paraphrases only. Every reader-facing in-text citation must appear in citation_map. A local PDF is attached when available; checked evidence records from the confirmed summary ground paper-based claims."),
      tags$details(
        open = if (nrow(selected_packages) > 0) "open" else NULL,
        tags$summary("Show exact full article API prompt"),
        div(
          class = "lab-actions",
          tags$button(type = "button", class = "btn lab-secondary", onclick = "window.articleLabCopyTextFromElement('article_lab_full_text_exact_api_prompt', this);", "Copy full API prompt")
        ),
        h4("Exact input_text sent to the API"),
        tags$pre(id = "article_lab_full_text_exact_api_prompt", class = "lab-status-copy", exact_api_prompt),
        h4("Full article prompt"),
        tags$pre(class = "lab-status-copy", base_prompt),
        h4("Full article helper wrapper"),
        tags$pre(class = "lab-status-copy", paste(
          "Return one full article draft for the selected package, preserving all ids exactly.",
          "Required response shape: {\"results\":[{\"outline_id\":...,\"full_text\":...,\"citation_map\":[...]}]}",
          "The full_text value must be the complete Markdown article draft, not MARKDOWN_ARTICLE_HERE, a placeholder, schema example, excerpt, note, or explanation. For a single selected package, a plain Markdown response or single-object {\"full_text\":...} response is accepted as that package's draft.",
          sep = "\n"
        )),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", request_additions),
        h4("Selected approved outlines"),
        tags$pre(class = "lab-status-copy", package_list),
        h4("Source context sent"),
        tags$pre(class = "lab-status-copy", context_payload)
      )
    )
  })

  output$research_ranked_sources_table <- DT::renderDT({
    rows <- research_ranked_sources()
    display <- if (nrow(rows) == 0) {
      data.frame(research_source_id = integer(), Rank = integer(), Status = character(), Title = character(), Used = character(), Links = character(), Angles = integer(), check.names = FALSE)
    } else {
      source_title <- vapply(rows$source_title, research_input_default, character(1), default = "")
      data.frame(
        research_source_id = rows$research_source_id,
        Rank = seq_len(nrow(rows)),
        Status = rows$status,
        Title = sprintf('<span class="research-source-title" title="%s">%s</span>', htmltools::htmlEscape(source_title), htmltools::htmlEscape(vapply(source_title, research_truncate, character(1), max_chars = 240L))),
        Used = vapply(rows$used_articles, research_truncate, character(1), max_chars = 80L),
        Links = sprintf('<span class="research-source-links">%s</span>', mapply(research_links, rows$source_url, rows$pdf_url, USE.NAMES = FALSE)),
        Angles = rows$angles_count,
        check.names = FALSE
      )
    }
    DT::datatable(display, rownames = FALSE, escape = FALSE, class = "compact stripe hover research-source-table", selection = list(mode = "single", target = "row"), options = list(pageLength = 10, lengthMenu = c(5, 10, 25, 50, 100), autoWidth = FALSE, order = list(), columnDefs = list(list(targets = 0, visible = FALSE), list(targets = 1, width = "6%"), list(targets = 2, width = "10%"), list(targets = 3, width = "60%"), list(targets = 4, width = "12%"), list(targets = 5, width = "7%"), list(targets = 6, width = "5%"))))
  })

  output$research_unranked_sources_table <- DT::renderDT({
    rows <- research_unranked_sources()
    display <- if (nrow(rows) == 0) {
      data.frame(research_source_id = integer(), Status = character(), Title = character(), `Main idea` = character(), Used = character(), Links = character(), Angles = integer(), check.names = FALSE)
    } else {
      source_title <- vapply(rows$source_title, research_input_default, character(1), default = "")
      data.frame(
        research_source_id = rows$research_source_id,
        Status = rows$status,
        Title = sprintf('<span class="research-source-title" title="%s">%s</span>', htmltools::htmlEscape(source_title), htmltools::htmlEscape(vapply(source_title, research_truncate, character(1), max_chars = 220L))),
        `Main idea` = vapply(rows$main_idea, research_truncate, character(1), max_chars = 120L),
        Used = vapply(rows$used_articles, research_truncate, character(1), max_chars = 70L),
        Links = sprintf('<span class="research-source-links">%s</span>', mapply(research_links, rows$source_url, rows$pdf_url, USE.NAMES = FALSE)),
        Angles = rows$angles_count,
        check.names = FALSE
      )
    }
    DT::datatable(display, rownames = FALSE, escape = FALSE, class = "compact stripe hover research-source-table", selection = list(mode = "single", target = "row"), options = list(pageLength = 10, lengthMenu = c(5, 10, 25, 50, 100), autoWidth = FALSE, order = list(), columnDefs = list(list(targets = 0, visible = FALSE), list(targets = 1, width = "10%"), list(targets = 2, width = "45%"), list(targets = 3, width = "23%"), list(targets = 4, width = "12%"), list(targets = 5, width = "6%"), list(targets = 6, width = "4%"))))
  })

  output$research_selected_source_summary <- renderUI({
    row <- selected_research_source()
    if (nrow(row) == 0) return(div(class = "lab-status-copy", "Select a ranked or unranked source to edit details and create angles."))
    rank_label <- if (is.na(row$manual_sort_order[[1]])) "Unranked" else paste("Rank", row$manual_sort_order[[1]])
    main_idea <- research_truncate(row$main_idea[[1]], max_chars = 220L)
    div(
      class = "research-selected-summary",
      h3(row$source_title[[1]]),
      div(class = "research-source-links", HTML(research_links(row$source_url[[1]], row$pdf_url[[1]]))),
      div(class = "lab-status-copy", if (nzchar(main_idea)) main_idea else "No main idea saved yet."),
      div(class = "lab-status-copy", sprintf("Status: %s · %s%s", row$status[[1]], rank_label, if (!is.na(row$finished_at[[1]]) && nzchar(row$finished_at[[1]])) paste0(" · Finished ", row$finished_at[[1]]) else "")),
      if (!is.na(row$used_articles[[1]]) && nzchar(row$used_articles[[1]])) div(class = "lab-status-copy", strong("Used for: "), row$used_articles[[1]]) else NULL
    )
  })

  output$research_angle_workspace <- renderUI({
    row <- selected_research_source()
    if (nrow(row) == 0) return(NULL)
    tagList(
      div(class = "lab-status-copy", "Lower angle sort number appears higher."),
      DT::DTOutput("research_angles_table"),
      h3("Finish source"),
      div(class = "lab-field", textAreaInput("research_used_articles", "Article(s) written from this source", value = row$used_articles[[1]] %||% "", width = "100%", height = "70px", placeholder = "One title or URL per line")),
      div(class = "lab-actions", actionButton("research_mark_finished", "Mark selected source finished", class = "lab-secondary")),
      h3("Create angle"),
      div(class = "lab-grid", div(class = "lab-field", textInput("research_new_angle_title", "Angle title", width = "100%")), div(class = "lab-field", numericInput("research_new_angle_sort", "Sort order", value = NULL, width = "100%")), div(class = "lab-field", textInput("research_new_angle_status", "Status", value = "idea", width = "100%"))),
      div(class = "lab-field", textAreaInput("research_new_angle_main_idea", "Angle main idea", width = "100%", height = "90px")),
      div(class = "lab-field", textAreaInput("research_new_angle_notes", "Notes", width = "100%", height = "80px")),
      div(class = "lab-actions", actionButton("research_add_angle", "Create angle from selected source", class = "lab-primary")),
      uiOutput("research_selected_angle_editor"),
      tags$details(
        class = "research-source-details",
        tags$summary("Edit source details"),
        uiOutput("research_selected_source_editor")
      )
    )
  })

  output$research_selected_source_editor <- renderUI({
    row <- selected_research_source()
    if (nrow(row) == 0) return(div(class = "lab-status-copy", "Select a ranked or unranked source to edit details and create angles."))
    div(
      div(class = "lab-status-copy", sprintf("Editing source %s", row$research_source_id[[1]])),
      div(class = "lab-grid", div(class = "lab-field", textInput("research_edit_source_title", "Source title", value = row$source_title[[1]], width = "100%")), div(class = "lab-field", textInput("research_edit_source_url", "Source URL", value = row$source_url[[1]] %||% "", width = "100%")), div(class = "lab-field", textInput("research_edit_pdf_url", "PDF URL", value = row$pdf_url[[1]] %||% "", width = "100%")), div(class = "lab-field", numericInput("research_edit_source_sort", "Sort order", value = research_numeric_default(row$manual_sort_order[[1]]), width = "100%")), div(class = "lab-field", textInput("research_edit_source_status", "Status", value = row$status[[1]], width = "100%"))),
      div(class = "lab-field", textAreaInput("research_edit_source_main", "Main idea", value = row$main_idea[[1]] %||% "", width = "100%", height = "90px")),
      div(class = "lab-field", textAreaInput("research_edit_source_abstract", "Abstract", value = row$abstract[[1]] %||% "", width = "100%", height = "90px")),
      div(class = "lab-field", textAreaInput("research_edit_source_used_articles", "Article(s) written from this source", value = row$used_articles[[1]] %||% "", width = "100%", height = "70px")),
      div(class = "lab-field", textAreaInput("research_edit_source_notes", "Notes", value = row$notes[[1]] %||% "", width = "100%", height = "80px")),
      div(class = "lab-actions", actionButton("research_save_source", "Save selected source", class = "lab-primary"), actionButton("research_refresh_selected_source", "Refresh", class = "lab-secondary"))
    )
  })

  output$research_angles_table <- DT::renderDT({
    rows <- research_angles()
    display <- if (nrow(rows) == 0) {
      data.frame(research_angle_id = integer(), Sort = integer(), Status = character(), `Angle title` = character(), `Main idea` = character(), `Title Lab batch` = character(), Updated = character(), check.names = FALSE)
    } else {
      data.frame(
        research_angle_id = rows$research_angle_id,
        Sort = rows$manual_sort_order,
        Status = rows$status,
        `Angle title` = vapply(rows$angle_title, research_truncate, character(1), max_chars = 80L),
        `Main idea` = vapply(rows$main_idea, research_truncate, character(1), max_chars = 110L),
        `Title Lab batch` = rows$article_lab_batch_id,
        Updated = rows$updated_at,
        check.names = FALSE
      )
    }
    DT::datatable(display, rownames = FALSE, escape = TRUE, selection = list(mode = "single", target = "row"), options = list(pageLength = 8, scrollX = TRUE, order = list(), columnDefs = list(list(targets = 0, visible = FALSE))))
  })

  output$research_selected_angle_editor <- renderUI({
    source <- selected_research_source()
    if (nrow(source) == 0) return(div(class = "lab-status-copy", "Select a source to view and edit its angles."))
    row <- selected_research_angle()
    if (nrow(row) == 0) return(div(class = "lab-status-copy", "Select an angle from the table to edit it, or create a new angle below."))
    id <- row$research_angle_id[[1]]
    div(
      h3("Selected angle"),
      div(class = "lab-grid", div(class = "lab-field", textInput("research_edit_angle_title", "Angle title", value = row$angle_title[[1]], width = "100%")), div(class = "lab-field", numericInput("research_edit_angle_sort", "Sort order", value = research_numeric_default(row$manual_sort_order[[1]]), width = "100%")), div(class = "lab-field", textInput("research_edit_angle_status", "Status", value = row$status[[1]], width = "100%")), div(class = "lab-field", textInput("research_edit_angle_batch", "Article Lab batch", value = row$article_lab_batch_id[[1]] %||% "", width = "100%"))),
      div(class = "lab-field", textAreaInput("research_edit_angle_main", "Main idea", value = row$main_idea[[1]] %||% "", width = "100%", height = "80px")),
      div(class = "lab-field", textAreaInput("research_edit_angle_notes", "Notes", value = row$notes[[1]] %||% "", width = "100%", height = "70px")),
      div(class = "lab-actions", actionButton("research_save_angle", "Save angle edits", class = "lab-secondary"), tags$button(type = "button", class = "btn btn-default action-button lab-primary", onclick = sprintf("Shiny.setInputValue('research_send_to_title_lab', '%s', {priority: 'event'})", id), "Send to Title Lab"))
    )
  })

  output$article_lab_generate_button <- renderUI({
    if (isTRUE(article_lab_state$is_generating)) {
      tags$button(
        id = "article_lab_generate",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Generating..."
      )
    } else {
      actionButton("article_lab_generate", "Generate titles", class = "lab-primary")
    }
  })

  output$article_lab_batch_selector <- renderUI({
    batches <- article_lab_batches()
    batch_choices <- if (nrow(batches) == 0) character() else setNames(
      batches$batch_id,
      sprintf("%s · %s", batches$batch_id, batches$created_at)
    )
    choices <- c("All batches" = article_lab_all_batches_value, batch_choices)
    div(
      class = "lab-field",
      selectInput(
        "article_lab_selected_batch",
        "Batch selector",
        choices = choices,
        selected = article_lab_all_batches_value,
        width = "100%"
      )
    )
  })

  output$article_lab_score_button <- renderUI({
    if (isTRUE(article_lab_state$is_scoring)) {
      tags$button(
        id = "article_lab_score_titles",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Scoring..."
      )
    } else {
      actionButton("article_lab_score_titles", "Score selected API queue", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_queue');")
    }
  })

  output$article_lab_subtitle_generate_button <- renderUI({
    if (isTRUE(article_lab_state$is_generating_subtitles)) {
      tags$button(
        id = "article_lab_generate_subtitles",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Generating..."
      )
    } else {
      actionButton("article_lab_generate_subtitles", "Generate selected subtitle candidates", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_subtitle_titles');")
    }
  })

  output$article_lab_thumbnail_generate_button <- renderUI({
    if (isTRUE(article_lab_state$is_generating_thumbnails)) {
      tags$button(
        id = "article_lab_generate_thumbnails",
        type = "button",
        class = "btn btn-default action-button lab-primary loading",
        disabled = "disabled",
        span(class = "button-spinner"),
        "Generating..."
      )
    } else {
      actionButton("article_lab_generate_thumbnails", "Generate selected thumbnail candidates", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_packages');")
    }
  })

  output$article_lab_latest_titles <- renderUI({
    saved <- article_lab_generate_candidates()
    draft <- article_lab_state$draft
    if (!is.null(draft) && nrow(draft) > 0) {
      draft_rows <- data.frame(
        candidate_id = sprintf("draft_%02d", seq_len(nrow(draft))),
        title = draft$title,
        title_char_count = article_lab_title_length(draft$title),
        title_length_flag = article_lab_title_length_flag(article_lab_title_length(draft$title)),
        status = rep("draft", nrow(draft)),
        created_at = rep(article_lab_state$draft_created_at %||% now_utc(), nrow(draft)),
        batch_id = rep("(draft)", nrow(draft)),
        notes = rep(NA_character_, nrow(draft)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      rows <- draft_rows
    } else {
      rows <- saved
    }
    article_lab_generate_table_ui(rows)
  })

  output$article_lab_score_sections <- renderUI({
    queue_rows <- article_lab_queue_rows()
    scored_rows <- article_lab_scored_rows()

    tagList(
      article_lab_section_card(
        "1. API queue (waiting to be scored)",
        "These titles have not been scored yet.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_queue_select_all", "Select all", value = FALSE)
          ),
          article_lab_score_queue_table_ui(queue_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_archive_queue_titles", "Archive selected titles", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_queue');", disabled = nrow(queue_rows) == 0)
          )
        ),
        count = nrow(queue_rows)
      ),
      article_lab_section_card(
        "2. Scored titles awaiting approval",
        "These titles have been scored by the API. Select the ones you want to approve for subtitle generation.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_scored_select_all", "Select all", value = FALSE)
          ),
          article_lab_score_table_ui(scored_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_approve_for_subtitle", "Approve selected for subtitles", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_scored');", disabled = nrow(scored_rows) == 0),
            article_lab_button("article_lab_archive_scored_titles", "Archive selected titles", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_scored');", disabled = nrow(scored_rows) == 0)
          ),
          div(class = "lab-status-copy", "Approved titles will move to Subtitle Generation.")
        ),
        count = nrow(scored_rows)
      )
    )
  })

  output$article_lab_subtitle_sections <- renderUI({
    target_rows <- article_lab_subtitle_target_rows()
    subtitle_rows <- article_lab_pending_subtitle_rows()

    tagList(
      article_lab_section_card(
        "1. Titles awaiting subtitle generation",
        "These approved titles do not have active subtitle candidates yet.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_subtitle_title_select_all", "Select all", value = FALSE)
          ),
          article_lab_subtitle_target_table_ui(target_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_archive_subtitle_titles", "Archive selected titles", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_subtitle_titles');", disabled = nrow(target_rows) == 0)
          )
        ),
        count = nrow(target_rows)
      ),
      article_lab_section_card(
        "2. Subtitle candidates awaiting approval",
        "Select subtitle candidates to approve for Thumbnails or reject without deleting them.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_subtitle_candidate_select_all", "Select all", value = FALSE)
          ),
          article_lab_subtitle_candidate_table_ui(subtitle_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_approve_subtitles", "Approve selected subtitles", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_subtitle_candidates');", disabled = nrow(subtitle_rows) == 0),
            article_lab_button("article_lab_reject_subtitles", "Reject selected", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_subtitle_candidates');", disabled = nrow(subtitle_rows) == 0)
          ),
          div(class = "lab-status-copy", "Approved subtitle candidates stay available as variants for the Thumbnails step.")
        ),
        count = nrow(subtitle_rows)
      )
    )
  })

  output$article_lab_thumbnail_sections <- renderUI({
    package_rows <- article_lab_thumbnail_package_rows()
    thumbnail_rows <- article_lab_pending_thumbnail_rows()

    tagList(
      article_lab_section_card(
        "1. Title/subtitle packages awaiting thumbnail generation",
        "These approved title/subtitle packages do not have active thumbnail candidates yet.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_thumbnail_package_select_all", "Select all", value = FALSE)
          ),
          article_lab_thumbnail_package_table_ui(package_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_dismiss_thumbnail_packages", "Dismiss selected packages", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_packages');", disabled = nrow(package_rows) == 0)
          )
        ),
        count = nrow(package_rows)
      ),
      article_lab_section_card(
        "2. Thumbnail preview cards awaiting approval",
        "Select one preview card per title/subtitle package to approve for Outline, or reject candidates without deleting them.",
        tagList(
          div(
            class = "lab-actions",
            checkboxInput("article_lab_thumbnail_candidate_select_all", "Select all", value = FALSE)
          ),
          article_lab_thumbnail_candidate_grid_ui(thumbnail_rows),
          article_lab_action_bar(
            article_lab_button("article_lab_approve_thumbnails", "Approve selected thumbnail", class = "lab-primary", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_candidates');", disabled = nrow(thumbnail_rows) == 0),
            article_lab_button("article_lab_reject_thumbnails", "Reject selected", class = "lab-danger", onclick = "window.articleLabSyncSelections('article_lab_thumbnail_candidates');", disabled = nrow(thumbnail_rows) == 0)
          ),
          div(class = "lab-status-copy", "Only one approved thumbnail is allowed per title/subtitle package. Approved packages move to Outline.")
        ),
        count = nrow(thumbnail_rows)
      )
    )
  })

  output$article_lab_outline_sections <- renderUI({
    outline_rows <- article_lab_ready_for_outline_rows()

    article_lab_section_card(
      "Ready for Outline",
      "Approved title/subtitle/thumbnail packages are available here for the next drafting step.",
      article_lab_ready_for_outline_table_ui(outline_rows),
      count = nrow(outline_rows)
    )
  })

  output$article_lab_full_text_sections <- renderUI({
    rows <- article_lab_full_text_rows()
    packages <- article_lab_full_text_package_rows(rows)
    summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(packages$batch_id))
    article_lab_section_card(
      "Approved outlines ready for Full Article",
      "Generate one or more full article variants from each approved outline, edit drafts in place, and approve one for Review & Publish.",
      article_lab_full_text_table_ui(rows, packages, summary_contexts, include_context = isTRUE(input$article_lab_full_text_include_context)),
      count = nrow(packages)
    )
  })

  output$article_lab_review_publish_selector <- renderUI({
    rows <- article_lab_review_publish_rows()
    article_lab_review_publish_selector_ui(rows, input$article_lab_review_publish_draft_id)
  })

  output$article_lab_review_publish_workspace <- renderUI({
    article_lab_review_publish_workspace_ui(article_lab_selected_review_publish_row(), article_lab_publication_rows())
  })

  output$article_lab_medium_tags_effective_prompt <- renderUI({
    row <- article_lab_selected_review_publish_row()
    model <- article_lab_input_string(input$article_lab_medium_tags_model) %||% article_lab_default_medium_tags_model
    prompt <- article_lab_input_multiline(input$article_lab_medium_tags_prompt) %||% article_lab_default_medium_tags_prompt
    exact_prompt <- article_lab_medium_tags_effective_prompt(row, prompt)
    div(
      class = "lab-card",
      h3("Prompt that will be sent to the API"),
      p(class = "lab-status-copy", "Medium tag generation sends the selected approved article package and asks for JSON tags only."),
      tags$details(
        open = if (nrow(row) > 0) "open" else NULL,
        tags$summary("Show exact Medium tags API prompt"),
        h4("Request fields"),
        tags$pre(class = "lab-status-copy", paste(sprintf("Model: %s", model), "Response format: JSON with a tags array, capped to 5 tags on save.", sep = "\n")),
        h4("Exact prompt"),
        tags$pre(class = "lab-status-copy", if (nzchar(exact_prompt)) exact_prompt else "(No approved article selected.)")
      )
    )
  })

  output$article_lab_review_publish_archive_error <- renderUI({
    err <- article_lab_state$last_review_publish_archive_error
    if (is.null(err)) return(NULL)
    err_at <- article_lab_state$last_review_publish_archive_error_at
    elapsed <- if (is.null(err_at)) "" else format(err_at, "%Y-%m-%d %H:%M:%S")
    kind_label <- switch(
      err$kind %||% "unknown",
      no_selection = "No article selected",
      no_rows = "Article archive did not update any rows",
      exception = "Article archive failed",
      "Article archive error"
    )
    affected_ids <- err$selected_ids %||% character()
    div(
      class = "lab-alert lab-alert-error",
      role = "alert",
      div(
        class = "lab-alert-title",
        span(class = "lab-alert-icon", HTML("&#9888;")),
        strong(kind_label),
        if (nzchar(elapsed)) span(class = "lab-alert-time", sprintf(" at %s", elapsed))
      ),
      div(
        class = "lab-alert-body",
        p(err$reason %||% "Unknown error."),
        tags$ul(
          if (length(affected_ids) > 0) tags$li(sprintf("Affected draft%s: %s", ifelse(length(affected_ids) == 1L, "", "s"), paste(affected_ids, collapse = ", "))) else tags$li("No draft was selected."),
          tags$li("The approved article was NOT archived or deleted. It remains in Review & Publish until this succeeds."),
          tags$li("This action is non-destructive: a successful archive sets publish_status to archived; it does not physically delete the draft.")
        )
      )
    )
  })

  observeEvent(input$article_lab_generate, {
    article_lab_state$is_generating <- TRUE
    on.exit({
      article_lab_state$is_generating <- FALSE
    }, add = TRUE)

    selected_summary <- selected_generate_summary()
    effective_inputs <- article_lab_effective_generation_inputs()
    prompt_value <- effective_inputs$prompt
    manual_prompt_value <- effective_inputs$manual_prompt
    seed_topic_value <- effective_inputs$seed_topic
    inspiration_value <- effective_inputs$inspiration_source

    context_notes_value <- article_lab_input_multiline(input$article_lab_context_notes) %||% ""

    generated <- generate_title_candidates(
      con = con,
      prompt = prompt_value,
      batch_size = input$article_lab_batch_size,
      seed_topic = seed_topic_value,
      inspiration_source = inspiration_value,
      model = input$article_lab_model,
      manual_prompt = manual_prompt_value,
      context_notes = context_notes_value
    )
    article_lab_state$draft <- generated$titles
    article_lab_state$draft_created_at <- now_utc()
    article_lab_state$draft_meta <- modifyList(generated, list(
      prompt = prompt_value,
      manual_prompt = manual_prompt_value,
      seed_topic = seed_topic_value,
      inspiration_source = inspiration_value,
      context_notes = context_notes_value,
      notes_extra = if (nrow(selected_summary) > 0) paste(
        sprintf("Research summary: %s.", selected_summary$summary_id[[1]]),
        sprintf("Research source: %s.", selected_summary$research_source_id[[1]]),
        sprintf("Source title: %s.", selected_summary$source_title[[1]] %||% ""),
        sprintf("Source URL: %s.", selected_summary$source_url[[1]] %||% ""),
        sprintf("PDF URL: %s.", selected_summary$pdf_url[[1]] %||% "")
      ) else NULL
    ))
    if (identical(generated$mode, "api")) {
      example_copy <- if (isTRUE(generated$example_titles_used > 0)) {
        sprintf(" Used %s top-performing title examples as inspiration.", generated$example_titles_used)
      } else {
        ""
      }
      retry_copy <- if (isTRUE(generated$retry_used)) {
        " Strict mode triggered one automatic retry to shorten titles above the hard maximum."
      } else {
        ""
      }
      dropped_copy <- if (isTRUE(generated$dropped_n > 0)) {
        sprintf(" Dropped %s title%s over %s characters after strict validation.", generated$dropped_n, ifelse(generated$dropped_n == 1, "", "s"), article_lab_title_max_chars)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "Generated %s draft titles with the OpenAI API using model %s.%s%s%s Save the batch to persist it to SQLite.",
        nrow(generated$titles),
        generated$model %||% article_lab_default_model,
        example_copy,
        retry_copy,
        dropped_copy
      )
    } else {
      dropped_copy <- if (isTRUE(generated$dropped_n > 0)) {
        sprintf(" Dropped %s title%s over %s characters after strict validation.", generated$dropped_n, ifelse(generated$dropped_n == 1, "", "s"), article_lab_title_max_chars)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "API generation was unavailable, so the local stub helper generated %s draft titles instead.%s Reason: %s",
        nrow(generated$titles),
        dropped_copy,
        generated$fallback_reason %||% "unknown error"
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_add_manual_titles, {
    manual_titles <- article_lab_parse_manual_titles(input$article_lab_manual_titles)
    if (length(manual_titles) == 0) {
      article_lab_state$notice <- "Enter at least one manual title idea, with one title per line."
      return(invisible(NULL))
    }

    existing_titles <- if (!is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
      clean_text(article_lab_state$draft$title)
    } else {
      character()
    }
    new_manual_titles <- setdiff(manual_titles, existing_titles)
    if (length(new_manual_titles) == 0) {
      article_lab_state$notice <- "Those manual titles are already in the current draft."
      updateTextAreaInput(session, "article_lab_manual_titles", value = "")
      return(invisible(NULL))
    }
    combined_titles <- unique(c(existing_titles, manual_titles))
    normalized_titles <- article_lab_normalize_titles(combined_titles)
    if (length(normalized_titles) == 0) {
      article_lab_state$notice <- "No usable manual titles were provided."
      return(invisible(NULL))
    }

    article_lab_state$draft <- data.frame(
      row_number = seq_along(normalized_titles),
      title = normalized_titles,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    article_lab_state$draft_created_at <- article_lab_state$draft_created_at %||% now_utc()

    prior_mode <- article_lab_state$draft_meta$mode %||% NA_character_
    next_mode <- if (is.na(prior_mode) || !nzchar(prior_mode)) {
      "manual"
    } else if (identical(prior_mode, "manual")) {
      "manual"
    } else {
      "mixed"
    }
    article_lab_state$draft_meta <- modifyList(
      article_lab_state$draft_meta %||% list(),
      list(
        mode = next_mode,
        raw_json = article_lab_state$draft_meta$raw_json %||% NA_character_
      )
    )

    added_n <- sum(normalized_titles %in% new_manual_titles)
    over_limit_n <- sum(article_lab_title_length(new_manual_titles) > article_lab_title_mobile_safe_chars, na.rm = TRUE)
    length_copy <- if (over_limit_n > 0) {
      sprintf(" %s title%s exceed the %s-character mobile-safe length and were kept with their length flag.", over_limit_n, ifelse(over_limit_n == 1, "", "s"), article_lab_title_mobile_safe_chars)
    } else {
      ""
    }
    article_lab_state$notice <- sprintf(
      "Added %s manual title idea%s to the current draft.%s Save the batch to persist it to SQLite.",
      added_n,
      ifelse(added_n == 1, "", "s"),
      length_copy
    )
    updateTextAreaInput(session, "article_lab_manual_titles", value = "")
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_prompt, {
    prompt_text <- article_lab_input_multiline(input$article_lab_prompt)
    prompt_key <- article_lab_input_string(input$article_lab_prompt_key) %||% article_lab_manual_prompt_key
    if (is.na(prompt_text)) {
      article_lab_state$notice <- "Enter a prompt before saving."
      return(invisible(NULL))
    }
    save_article_lab_prompt(con, prompt_text, prompt_key)
    saved_article_lab_prompt_key(prompt_key)
    saved_article_lab_prompt(prompt_text)
    updateSelectInput(session, "article_lab_prompt_key_select", choices = list_article_lab_prompt_keys(con), selected = prompt_key)
    updateTextInput(session, "article_lab_prompt_key", value = prompt_key)
    article_lab_state$notice <- sprintf("Saved generation prompt '%s'.", prompt_key)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_outline_prompt, {
    prompt_text <- article_lab_input_multiline(input$article_lab_outline_prompt)
    prompt_key <- article_lab_input_string(input$article_lab_outline_prompt_key) %||% article_lab_outline_prompt_key
    if (is.na(prompt_text)) {
      article_lab_state$notice <- "Enter an outline prompt before saving."
      return(invisible(NULL))
    }
    save_article_lab_prompt(con, prompt_text, prompt_key, article_lab_default_outline_prompt)
    saved_article_lab_outline_prompt_key(prompt_key)
    saved_article_lab_outline_prompt(prompt_text)
    updateSelectInput(session, "article_lab_outline_prompt_key_select", choices = list_article_lab_prompt_keys(con, article_lab_outline_prompt_key), selected = prompt_key)
    updateTextInput(session, "article_lab_outline_prompt_key", value = prompt_key)
    article_lab_state$notice <- sprintf("Saved outline prompt '%s'.", prompt_key)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_full_text_prompt, {
    prompt_text <- article_lab_input_multiline(input$article_lab_full_text_prompt)
    prompt_key <- article_lab_input_string(input$article_lab_full_text_prompt_key) %||% article_lab_full_text_prompt_key
    if (is.na(prompt_text)) {
      article_lab_state$notice <- "Enter a full article prompt before saving."
      return(invisible(NULL))
    }
    save_article_lab_prompt(con, prompt_text, prompt_key, article_lab_default_full_text_prompt)
    saved_article_lab_full_text_prompt_key(prompt_key)
    saved_article_lab_full_text_prompt(prompt_text)
    updateSelectInput(session, "article_lab_full_text_prompt_key_select", choices = list_article_lab_prompt_keys(con, article_lab_full_text_prompt_key), selected = prompt_key)
    updateTextInput(session, "article_lab_full_text_prompt_key", value = prompt_key)
    article_lab_state$notice <- sprintf("Saved full article prompt '%s'.", prompt_key)
  }, ignoreInit = TRUE)

  observe({
    batch <- article_lab_saved_batch()
    current_notes <- trimws(input$article_lab_context_notes %||% "")
    if (!is.null(batch) && nrow(batch) == 1 && !nzchar(current_notes)) {
      saved_notes <- article_lab_input_multiline(batch$article_context_notes[[1]]) %||% ""
      if (nzchar(saved_notes)) {
        updateTextAreaInput(session, "article_lab_context_notes", value = saved_notes)
      }
    }
  })

  save_current_article_lab_draft <- function() {
    draft <- article_lab_state$draft
    draft_meta <- article_lab_state$draft_meta %||% list()
    if (is.null(draft) || nrow(draft) == 0) return(NULL)

    batch_id <- save_article_lab_batch(
      con,
      prompt = draft_meta$prompt %||% input$article_lab_prompt,
      seed_topic = draft_meta$seed_topic %||% input$article_lab_seed_topic,
      inspiration_source = draft_meta$inspiration_source %||% input$article_lab_inspiration_source,
      requested_batch_size = input$article_lab_batch_size,
      model = input$article_lab_model,
      titles = draft$title,
      raw_json = if (is.null(draft_meta$raw_json)) NA_character_ else draft_meta$raw_json,
      generation_mode = draft_meta$mode %||% "generated",
      enforce_max_chars = !((draft_meta$mode %||% "") %in% c("manual", "mixed")),
      notes_extra = draft_meta$notes_extra,
      article_context_notes = draft_meta$context_notes %||% NA_character_
    )
    saved_mode <- draft_meta$mode %||% "generated"
    article_lab_state$draft <- NULL
    article_lab_state$draft_created_at <- NULL
    article_lab_state$draft_meta <- NULL
    article_lab_refresh(article_lab_refresh() + 1L)

    list(batch_id = batch_id, mode = saved_mode, title_n = nrow(draft))
  }

  observeEvent(input$article_lab_save, {
    saved <- save_current_article_lab_draft()
    if (is.null(saved)) {
      article_lab_state$notice <- "Nothing to save yet. Generate a draft first."
      return(invisible(NULL))
    }

    article_lab_state$notice <- if (saved$mode %in% c("manual", "mixed")) {
      sprintf(
        "Saved batch %s. Candidates start as New and stay in Generate until you manually move selected titles to the API queue. Generation mode: %s. Overlength manual titles were preserved with their length flag.",
        saved$batch_id,
        saved$mode
      )
    } else {
      sprintf(
        "Saved batch %s. Candidates start as New and stay in Generate until you manually move selected titles to the API queue. Generation mode: %s. Max title length enforced: %s characters.",
        saved$batch_id,
        saved$mode,
        article_lab_title_max_chars
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_clear, {
    article_lab_state$draft <- NULL
    article_lab_state$draft_created_at <- NULL
    article_lab_state$draft_meta <- NULL
    article_lab_state$notice <- "Cleared the unsaved draft."
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_triage, {
    if (!is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
      saved <- save_current_article_lab_draft()
      article_lab_state$notice <- sprintf("Saved draft batch %s with %s title%s. You can now edit statuses or notes.", saved$batch_id, saved$title_n, ifelse(saved$title_n == 1, "", "s"))
      return(invisible(NULL))
    }
    rows <- article_lab_generate_candidates()
    payload <- collect_generate_triage_updates(rows)
    if (length(payload$updates) == 0) {
      article_lab_state$notice <- "No saved titles are visible in the current triage view."
      return(invisible(NULL))
    }
    article_lab_save_generate_triage(con, payload$updates)
    article_lab_state$notice <- sprintf("Saved triage updates for %s title%s.", length(payload$updates), ifelse(length(payload$updates) == 1, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_move_to_api_queue, {
    if (!is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
      draft <- article_lab_state$draft
      selected_indexes <- which(vapply(seq_len(nrow(draft)), function(i) {
        isTRUE(input[[article_lab_row_input_id("article_lab_generate_select", sprintf("draft_%02d", i))]])
      }, logical(1)))
      if (length(selected_indexes) == 0) {
        article_lab_state$notice <- "Select at least one draft title before moving it to the API queue."
        return(invisible(NULL))
      }
      saved <- save_current_article_lab_draft()
      selected_ids <- article_lab_candidate_id(saved$batch_id, selected_indexes)
      result <- article_lab_move_candidates_to_api_queue(con, selected_ids)
      article_lab_state$notice <- sprintf(
        "Saved draft batch %s and moved %s selected title%s to API queue. %s selected title%s were skipped because they were not eligible.",
        saved$batch_id,
        result$moved_n,
        ifelse(result$moved_n == 1, "", "s"),
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s")
      )
      article_lab_refresh(article_lab_refresh() + 1L)
      if (result$moved_n > 0) {
        updateSelectInput(session, "article_lab_selected_batch", selected = saved$batch_id)
        active_section("api_scoring")
      }
      return(invisible(NULL))
    }
    rows <- article_lab_generate_candidates()
    payload <- collect_generate_triage_updates(rows)
    article_lab_save_generate_triage(con, payload$updates)
    snapshot_selected_ids <- collect_selected_ids(
      rows,
      "article_lab_generate_select",
      snapshot_ids = input$article_lab_generate_selected_snapshot
    )
    selected_ids <- unique(c(payload$selected_ids, snapshot_selected_ids))
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one New title before moving it to the API queue."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_move_candidates_to_api_queue(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Moved %s selected title%s to API queue. %s selected title%s were skipped because they were disqualified or not eligible.",
      result$moved_n,
      ifelse(result$moved_n == 1, "", "s"),
      result$skipped_n,
      ifelse(result$skipped_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
    if (result$moved_n > 0) {
      if (length(result$batch_ids) == 1 && nzchar(result$batch_ids[[1]])) {
        updateSelectInput(session, "article_lab_selected_batch", selected = result$batch_ids[[1]])
      }
      active_section("api_scoring")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_score_titles, {
    batch_id <- article_lab_selected_batch_id()
    if (is.na(batch_id) || !nzchar(batch_id)) {
      article_lab_state$notice <- "Select a saved batch before scoring."
      return(invisible(NULL))
    }
    article_lab_state$is_scoring <- TRUE
    on.exit({
      article_lab_state$is_scoring <- FALSE
    }, add = TRUE)

    queue_rows <- article_lab_queue_rows()
    selected_ids <- collect_selected_ids(
      queue_rows,
      "article_lab_queue_select",
      snapshot_ids = input$article_lab_queue_selected_snapshot
    )
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(queue_rows, "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_scored_rows(), "article_lab_scored_notes"))
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-queue title before scoring."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    result <- tryCatch(
      article_lab_score_batch(
        con,
        batch_id = batch_id,
        model = input$article_lab_score_model,
        prompt_version = input$article_lab_score_prompt_version,
        scope = input$article_lab_score_scope,
        candidate_ids = selected_ids
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste("API scoring failed:", conditionMessage(result))
    } else {
      article_lab_state$notice <- if (result$scored_n > 0) {
        sprintf(
          "Scored %s selected API-queue title%s for %s using model %s, prompt %s, scope %s.%s%s",
          result$scored_n,
          ifelse(result$scored_n == 1, "", "s"),
          result$batch_label %||% paste("batch", batch_id),
          result$model %||% article_lab_default_score_model,
          result$prompt_version %||% article_lab_default_score_prompt_version,
          result$scope %||% article_lab_default_score_scope,
          if (result$used_existing_n > 0) sprintf(" %s used an existing saved API score.", result$used_existing_n) else "",
          if (result$failed_n > 0) sprintf(" %s failed and stayed in their previous status.", result$failed_n) else ""
        )
      } else {
        result$message %||% "No titles are currently waiting in the API queue for this selection."
      }
      article_lab_refresh(article_lab_refresh() + 1L)
      if (!is_dimension_mode && !is.null(rating_session_id) && !is.na(rating_session_id)) {
        prune_article_lab_candidates_from_session(con, rating_session_id)
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_for_subtitle, {
    scored_rows <- article_lab_scored_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_queue_rows(), "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(scored_rows, "article_lab_scored_notes"))
    selected_ids <- collect_selected_ids(
      scored_rows,
      "article_lab_scored_select",
      snapshot_ids = input$article_lab_scored_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-scored title before approving it for subtitle generation."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_candidates_for_subtitle(con, selected_ids)
    article_lab_state$notice <- if (result$approved_n > 0 || result$skipped_n == 0) {
      sprintf("Approved %s selected title%s for subtitle generation.", result$approved_n, ifelse(result$approved_n == 1, "", "s"))
    } else {
      sprintf(
        "Approved %s selected title%s for subtitle generation. %s selected title%s were skipped because they were not API scored.",
        result$approved_n,
        ifelse(result$approved_n == 1, "", "s"),
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s")
      )
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_queue_titles, {
    queue_rows <- article_lab_queue_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(queue_rows, "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_scored_rows(), "article_lab_scored_notes"))
    selected_ids <- collect_selected_ids(
      queue_rows,
      "article_lab_queue_select",
      snapshot_ids = input$article_lab_queue_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-queue title before archiving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_archive_api_scored_candidates(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Archived %s selected API-queue title%s. No rows were deleted.",
      result$archived_n,
      ifelse(result$archived_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_scored_titles, {
    scored_rows <- article_lab_scored_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_queue_rows(), "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(scored_rows, "article_lab_scored_notes"))
    selected_ids <- collect_selected_ids(
      scored_rows,
      "article_lab_scored_select",
      snapshot_ids = input$article_lab_scored_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one API-scored title before archiving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_archive_api_scored_candidates(con, selected_ids)
    article_lab_state$notice <- if (result$archived_n > 0 || result$skipped_n == 0) {
      sprintf("Archived %s selected title%s.", result$archived_n, ifelse(result$archived_n == 1, "", "s"))
    } else {
      sprintf(
        "Archived %s selected title%s. %s selected title%s were skipped because they were not API scored.",
        result$archived_n,
        ifelse(result$archived_n == 1, "", "s"),
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s")
      )
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_subtitle_titles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      target_rows,
      "article_lab_subtitle_title_select",
      snapshot_ids = input$article_lab_subtitle_titles_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one subtitle-stage title before archiving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_archive_api_scored_candidates(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Archived %s selected subtitle-stage title%s. No rows were deleted.",
      result$archived_n,
      ifelse(result$archived_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_subtitles, {
    article_lab_state$is_generating_subtitles <- TRUE
    on.exit({
      article_lab_state$is_generating_subtitles <- FALSE
    }, add = TRUE)

    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      target_rows,
      "article_lab_subtitle_title_select",
      snapshot_ids = input$article_lab_subtitle_titles_selected_snapshot
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one approved title before generating subtitle candidates."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    result <- tryCatch(
      article_lab_generate_subtitles_for_titles(
        con,
        candidate_ids = selected_ids,
        model = input$article_lab_subtitle_model,
        prompt = input$article_lab_subtitle_prompt,
        variants_per_title = input$article_lab_subtitle_variants_per_title
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste("Subtitle generation failed:", conditionMessage(result))
    } else {
      fallback_copy <- if (!is.null(result$fallback_reason) && nzchar(result$fallback_reason)) {
        sprintf(" Stub fallback was used because: %s", result$fallback_reason)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "Generated %s subtitle candidate%s for %s selected title%s using model %s.%s %s selected title%s were skipped because they were not eligible or already had active subtitle candidates.%s",
        result$generated_n,
        ifelse(result$generated_n == 1, "", "s"),
        result$title_n,
        ifelse(result$title_n == 1, "", "s"),
        result$model %||% article_lab_default_subtitle_model,
        if (identical(result$mode, "stub")) " The stub helper was used." else "",
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s"),
        fallback_copy
      )
      article_lab_refresh(article_lab_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_add_manual_subtitles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))

    candidate_id <- article_lab_input_string(input$article_lab_manual_subtitle_candidate_id)
    subtitle_text <- input$article_lab_manual_subtitle_text %||% ""
    proposed_subtitles <- article_lab_normalize_subtitle(unlist(strsplit(subtitle_text, "\n", fixed = TRUE)))
    if (is.na(candidate_id) || !nzchar(candidate_id)) {
      article_lab_state$notice <- "Choose a title before adding manual subtitle ideas."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    if (length(proposed_subtitles) == 0) {
      article_lab_state$notice <- sprintf("Enter at least one manual subtitle idea under %s characters.", article_lab_subtitle_max_chars)
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    result <- article_lab_add_manual_subtitles(con, candidate_id, proposed_subtitles)
    if (result$added_n > 0) {
      duplicate_copy <- if (isTRUE(result$duplicate_n > 0)) {
        sprintf(" %s duplicate idea%s were skipped.", result$duplicate_n, ifelse(result$duplicate_n == 1, "", "s"))
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "Added %s manual subtitle idea%s for \"%s\".%s",
        result$added_n,
        ifelse(result$added_n == 1, "", "s"),
        result$title %||% "the selected title",
        duplicate_copy
      )
      updateTextAreaInput(session, "article_lab_manual_subtitle_text", value = "")
    } else if (isTRUE(result$duplicate_n > 0)) {
      article_lab_state$notice <- sprintf(
        "All entered subtitle ideas for \"%s\" already exist in this title's subtitle list.",
        result$title %||% "the selected title"
      )
    } else {
      article_lab_state$notice <- "The selected title is not currently eligible for manual subtitle ideas in this stage."
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_subtitles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_subtitle_candidate_select",
      snapshot_ids = input$article_lab_subtitle_candidates_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one subtitle candidate before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_subtitles(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Approved %s selected subtitle candidate%s. %s title package%s are now ready for Thumbnails.",
      result$approved_n,
      ifelse(result$approved_n == 1, "", "s"),
      length(unique(result$candidate_ids)),
      ifelse(length(unique(result$candidate_ids)) == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_reject_subtitles, {
    target_rows <- article_lab_subtitle_target_rows()
    pending_rows <- article_lab_pending_subtitle_rows()
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(target_rows, "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(pending_rows, "article_lab_subtitle_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_subtitle_candidate_select",
      snapshot_ids = input$article_lab_subtitle_candidates_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one subtitle candidate before rejecting it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_reject_subtitles(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Rejected %s selected subtitle candidate%s. No rows were deleted.",
      result$rejected_n,
      ifelse(result$rejected_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_thumbnails, {
    article_lab_state$is_generating_thumbnails <- TRUE
    on.exit({
      article_lab_state$is_generating_thumbnails <- FALSE
      article_lab_state$thumbnail_generation_started_at <- NULL
      article_lab_state$thumbnail_generation_estimate <- NULL
    }, add = TRUE)

    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      package_rows,
      "article_lab_thumbnail_package_select",
      snapshot_ids = input$article_lab_thumbnail_packages_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one ready title/subtitle package before generating thumbnail candidates."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    variants_per_package <- max(1L, min(4L, suppressWarnings(as.integer(input$article_lab_thumbnail_variants_per_package)) %||% article_lab_default_thumbnail_variants))
    estimate <- article_lab_thumbnail_estimate(length(selected_ids) * variants_per_package)
    started_at <- Sys.time()
    article_lab_state$thumbnail_generation_started_at <- started_at
    article_lab_state$thumbnail_generation_estimate <- estimate
    article_lab_state$notice <- sprintf(
      "Generating thumbnails: requested %s thumbnail%s for %s selected package%s. Initial estimate: %s. Waiting for OpenAI; live completed/remaining progress is not available during this blocking call.",
      estimate$total_expected,
      ifelse(estimate$total_expected == 1L, "", "s"),
      length(selected_ids),
      ifelse(length(selected_ids) == 1L, "", "s"),
      estimate$label
    )
    session$sendCustomMessage(
      "articleLabStartThumbnailTimer",
      list(
        total_expected = estimate$total_expected,
        estimate_label = estimate$label,
        lower_seconds = estimate$lower_seconds,
        upper_seconds = estimate$upper_seconds,
        started_at = paste0(format(as.POSIXct(started_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%OS3", tz = "UTC"), "Z")
      )
    )
    if (is.function(session$flushReact)) session$flushReact()

    result <- tryCatch(
      article_lab_generate_thumbnails_for_packages(
        con,
        subtitle_ids = selected_ids,
        model = input$article_lab_thumbnail_model,
        prompt = input$article_lab_thumbnail_prompt,
        variants_per_package = variants_per_package
      ),
      error = function(e) e
    )
    actual_seconds <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
    comparison <- article_lab_estimate_comparison(actual_seconds, estimate$lower_seconds, estimate$upper_seconds)
    timing_copy <- sprintf(
      "Thumbnail generation finished in %s. Initial estimate was %s, so this run was %s.",
      article_lab_format_duration(actual_seconds),
      estimate$label,
      comparison
    )
    if (inherits(result, "error")) {
      article_lab_state$notice <- paste(timing_copy, "Thumbnail generation failed:", conditionMessage(result))
      session$sendCustomMessage("articleLabStopThumbnailTimer", list(message = article_lab_state$notice))
    } else {
      mode_label <- result$mode %||% "unknown"
      fallback_count <- if (identical(mode_label, "stub")) result$generated_n else 0L
      failure_count <- 0L
      fallback_copy <- if (identical(mode_label, "stub") && !is.null(result$fallback_reason) && nzchar(result$fallback_reason)) {
        sprintf(" Stub fallback reason: %s", result$fallback_reason)
      } else {
        ""
      }
      article_lab_state$notice <- sprintf(
        "%s Generated %s thumbnail candidate%s for %s selected package%s using model %s in %s mode. Fallback count: %s. Failure count: %s. %s selected package%s were skipped because they were not eligible or already had active thumbnail candidates.%s",
        timing_copy,
        result$generated_n,
        ifelse(result$generated_n == 1, "", "s"),
        result$package_n,
        ifelse(result$package_n == 1, "", "s"),
        result$model %||% article_lab_default_thumbnail_model,
        mode_label,
        fallback_count,
        failure_count,
        result$skipped_n,
        ifelse(result$skipped_n == 1, "", "s"),
        fallback_copy
      )
      session$sendCustomMessage("articleLabStopThumbnailTimer", list(message = article_lab_state$notice))
      article_lab_refresh(article_lab_refresh() + 1L)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_dismiss_thumbnail_packages, {
    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      package_rows,
      "article_lab_thumbnail_package_select",
      snapshot_ids = input$article_lab_thumbnail_packages_selected_snapshot,
      key_col = "subtitle_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one ready title/subtitle package before dismissing it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_dismiss_thumbnail_packages(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Dismissed %s selected title/subtitle package%s. No rows were deleted.",
      result$dismissed_n,
      ifelse(result$dismissed_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_thumbnails, {
    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_thumbnail_candidate_select",
      snapshot_ids = input$article_lab_thumbnail_candidates_selected_snapshot,
      key_col = "thumbnail_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one thumbnail preview card before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    selected_rows <- pending_rows[pending_rows$thumbnail_id %in% selected_ids, , drop = FALSE]
    duplicate_subtitle_ids <- names(table(selected_rows$subtitle_id)[table(selected_rows$subtitle_id) > 1L])
    if (length(duplicate_subtitle_ids) > 0) {
      article_lab_state$notice <- "Select only one thumbnail candidate per title/subtitle package before approving."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_thumbnails(con, selected_ids)
    if (!is.null(result$message) && nzchar(result$message)) {
      article_lab_state$notice <- result$message
    } else {
      article_lab_state$notice <- sprintf(
        "Approved %s selected thumbnail%s. %s package%s are now ready for Outline.",
        result$approved_n,
        ifelse(result$approved_n == 1, "", "s"),
        length(unique(result$subtitle_ids)),
        ifelse(length(unique(result$subtitle_ids)) == 1, "", "s")
      )
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_reject_thumbnails, {
    package_rows <- article_lab_thumbnail_package_rows()
    pending_rows <- article_lab_pending_thumbnail_rows()
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(package_rows, "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(pending_rows, "article_lab_thumbnail_candidate_notes"))
    selected_ids <- collect_selected_ids(
      pending_rows,
      "article_lab_thumbnail_candidate_select",
      snapshot_ids = input$article_lab_thumbnail_candidates_selected_snapshot,
      key_col = "thumbnail_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one thumbnail preview card before rejecting it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_reject_thumbnails(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Rejected %s selected thumbnail candidate%s. No rows were deleted.",
      result$rejected_n,
      ifelse(result$rejected_n == 1, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_outlines, {
    started_at <- Sys.time()
    article_lab_debug_log("outline_generate_clicked", list(model = input$article_lab_outline_model, include_context = isTRUE(input$article_lab_outline_include_context)))
    article_lab_state$notice <- "Generating selected outline draft(s). Waiting for OpenAI; this can take a while."
    article_lab_state$last_outline_generate_error <- NULL
    article_lab_state$last_outline_generate_error_at <- NULL
    article_lab_refresh(article_lab_refresh() + 1L)
    if (is.function(session$flushReact)) session$flushReact()

    result <- tryCatch({
      outline_rows <- article_lab_ready_for_outline_rows()
      article_lab_debug_log("outline_generate_rows_loaded", list(ready_rows = nrow(outline_rows)))
      saved_edits_n <- article_lab_update_outlines(con, collect_outline_updates(outline_rows))
      selected_ids <- collect_selected_ids(
        outline_rows,
        "article_lab_outline_packages",
        snapshot_ids = input$article_lab_outline_packages_selected_snapshot,
        key_col = "thumbnail_id"
      )
      article_lab_debug_log("outline_generate_selection", list(saved_edits_n = saved_edits_n, selected_n = length(selected_ids), selected_ids = selected_ids))
      if (length(selected_ids) == 0) {
        list(ok = FALSE, notice = "Select at least one package before generating or regenerating an outline.")
      } else {
        selected_rows <- outline_rows[outline_rows$thumbnail_id %in% selected_ids, , drop = FALSE]
        if (nrow(selected_rows) == 0) {
          list(ok = FALSE, notice = "Selected packages are no longer available for outline generation.")
        } else {
          summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(selected_rows$batch_id))
          selected_rows$article_summary <- NA_character_
          selected_rows$pdf_local_path <- NA_character_
          if (nrow(summary_contexts) > 0) {
            matched_summary <- match(selected_rows$batch_id, summary_contexts$batch_id)
            selected_rows$article_summary <- summary_contexts$article_summary[matched_summary]
            selected_rows$pdf_local_path <- summary_contexts$pdf_local_path[matched_summary]
          }
          article_lab_debug_log("outline_generate_context_loaded", list(
            selected_rows = nrow(selected_rows),
            summary_context_rows = nrow(summary_contexts),
            pdf_context_n = sum(!is.na(selected_rows$pdf_local_path) & nzchar(selected_rows$pdf_local_path)),
            summary_context_n = sum(!is.na(selected_rows$article_summary) & nzchar(selected_rows$article_summary))
          ))
          generated <- generate_outline_drafts(
            selected_rows,
            model = input$article_lab_outline_model,
            prompt = input$article_lab_outline_prompt,
            include_context = isTRUE(input$article_lab_outline_include_context),
            context_notes = input$article_lab_outline_context_notes
          )
          article_lab_debug_log("outline_generate_drafts_returned", list(
            mode = generated$mode %||% "unknown",
            model = generated$model %||% article_lab_default_outline_model,
            generated_rows = nrow(generated$rows),
            fallback_reason = generated$fallback_reason %||% NA_character_
          ))
          if (identical(generated$mode, "failed")) {
            list(
              ok = FALSE,
              notice = paste("Outline API call failed. No generic stub outline was saved.", generated$fallback_reason %||% "See .local_gitignored/article_lab_debug.log for details."),
              error = list(
                kind = "api_failed",
                reason = generated$fallback_reason %||% "Unknown API failure (see .local_gitignored/article_lab_debug.log).",
                mode = generated$mode,
                model = generated$model %||% article_lab_default_outline_model,
                selected_ids = selected_ids
              )
            )
          } else if (nrow(generated$rows) == 0) {
            list(
              ok = FALSE,
              notice = "Outline API call returned no usable outline rows. No generic stub outline was saved. See .local_gitignored/article_lab_debug.log for details.",
              error = list(
                kind = "no_rows",
                reason = "OpenAI returned an empty or unparseable outline response. The existing outline was left unchanged.",
                mode = generated$mode %||% "unknown",
                model = generated$model %||% article_lab_default_outline_model,
                selected_ids = selected_ids
              )
            )
          } else {
          inserted_n <- article_lab_insert_outline_drafts(con, generated$rows)
          article_lab_debug_log("outline_generate_inserted", list(inserted_n = inserted_n))
          list(
            ok = TRUE,
            notice = sprintf(
              "Generated %s outline draft%s using model %s in %s mode.",
              inserted_n,
              ifelse(inserted_n == 1L, "", "s"),
              generated$model %||% article_lab_default_outline_model,
              generated$mode %||% "unknown"
            )
          )
          }
        }
      }
    }, error = function(e) {
      article_lab_debug_log("outline_generate_error", list(
        message = conditionMessage(e),
        call = paste(deparse(conditionCall(e)), collapse = " "),
        elapsed_seconds = as.numeric(difftime(Sys.time(), started_at, units = "secs"))
      ))
      list(
        ok = FALSE,
        notice = paste("Outline generation failed:", conditionMessage(e), "Debug log: .local_gitignored/article_lab_debug.log"),
        error = list(
          kind = "exception",
          reason = conditionMessage(e),
          mode = "exception",
          model = input$article_lab_outline_model %||% article_lab_default_outline_model,
          selected_ids = character()
        )
      )
    })

    article_lab_state$notice <- result$notice
    if (isTRUE(result$ok) || is.null(result$error)) {
      article_lab_state$last_outline_generate_error <- NULL
      article_lab_state$last_outline_generate_error_at <- NULL
    } else {
      article_lab_state$last_outline_generate_error <- result$error
      article_lab_state$last_outline_generate_error_at <- Sys.time()
    }
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observe({
    rows <- article_lab_ready_for_outline_rows()
    if (nrow(rows) == 0) {
      article_lab_active_outline_thumbnail(NULL)
      return()
    }
    checked_ids <- character()
    for (i in seq_len(nrow(rows))) {
      tid <- rows$thumbnail_id[[i]]
      if (isTRUE(input[[article_lab_row_input_id("article_lab_outline_packages", tid)]])) {
        checked_ids <- c(checked_ids, tid)
      }
    }
    article_lab_active_outline_thumbnail(
      if (length(checked_ids) == 1) checked_ids[1] else NULL
    )
  })

  observeEvent(article_lab_active_outline_thumbnail(), {
    active_id <- article_lab_active_outline_thumbnail()
    if (!is.null(active_id)) {
      saved_notes <- article_lab_load_outline_context_notes(con, active_id)
      updateTextAreaInput(session, "article_lab_outline_context_notes", value = saved_notes %||% "")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_outline_context_notes, {
    rows <- article_lab_ready_for_outline_rows()
    if (nrow(rows) == 0) {
      article_lab_state$notice <- "No outline packages available to save context notes."
      return()
    }
    checked_ids <- character()
    for (i in seq_len(nrow(rows))) {
      tid <- rows$thumbnail_id[[i]]
      if (isTRUE(input[[article_lab_row_input_id("article_lab_outline_packages", tid)]])) {
        checked_ids <- c(checked_ids, tid)
      }
    }
    if (length(checked_ids) == 0) {
      article_lab_state$notice <- "Check at least one package before saving context notes."
      return()
    }
    context_notes <- input$article_lab_outline_context_notes %||% ""
    for (tid in checked_ids) {
      article_lab_save_outline_context_notes(con, tid, context_notes)
    }
    article_lab_state$notice <- sprintf("Saved context notes for %s package%s.", length(checked_ids), ifelse(length(checked_ids) == 1L, "", "s"))
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_outlines, {
    updated_n <- article_lab_update_outlines(con, collect_outline_updates(article_lab_ready_for_outline_rows()))
    article_lab_state$notice <- sprintf("Saved %s editable outline%s.", updated_n, ifelse(updated_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_outlines, {
    outline_rows <- article_lab_ready_for_outline_rows()
    article_lab_update_outlines(con, collect_outline_updates(outline_rows))
    draft_rows <- outline_rows[!is.na(outline_rows$outline_id) & nzchar(outline_rows$outline_id) & outline_rows$outline_status == "draft", , drop = FALSE]
    selected_ids <- collect_selected_ids(
      draft_rows,
      "article_lab_outline_candidates",
      snapshot_ids = input$article_lab_outline_candidates_selected_snapshot,
      key_col = "outline_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one outline draft before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_outlines(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Approved %s outline%s. %s package%s moved to draft-ready.",
      result$approved_n,
      ifelse(result$approved_n == 1L, "", "s"),
      length(result$candidate_ids),
      ifelse(length(result$candidate_ids) == 1L, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_outlines, {
    outline_rows <- article_lab_ready_for_outline_rows()
    selected_ids <- collect_selected_ids(
      outline_rows,
      "article_lab_outline_archive_packages",
      snapshot_ids = input$article_lab_outline_archive_packages_selected_snapshot,
      key_col = "candidate_id"
    )
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one package before archiving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_archive_api_scored_candidates(con, selected_ids)
    article_lab_state$notice <- sprintf(
      "Archived %s selected package%s.",
      result$archived_n,
      ifelse(result$archived_n == 1L, "", "s")
    )
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_outlines, {
    updated_n <- article_lab_update_outlines(con, collect_outline_updates(article_lab_ready_for_outline_rows()))
    article_lab_state$notice <- sprintf("Refreshed Outline and saved %s editable outline%s.", updated_n, ifelse(updated_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  generate_selected_full_text <- function(variant = FALSE, selected_rows = NULL) {
    started_at <- Sys.time()
    article_lab_debug_log("full_text_generate_clicked", list(model = input$article_lab_full_text_model, include_context = isTRUE(input$article_lab_full_text_include_context), variant = isTRUE(variant)))
    article_lab_state$notice <- if (variant) "Generating another full article variant. Waiting for OpenAI." else "Generating full article draft. Waiting for OpenAI."
    article_lab_state$last_full_text_generate_error <- NULL
    article_lab_state$last_full_text_generate_error_at <- NULL
    article_lab_refresh(article_lab_refresh() + 1L)
    if (is.function(session$flushReact)) session$flushReact()

    result <- tryCatch({
      selected_ids <- character()
      if (is.null(selected_rows)) {
        rows <- article_lab_full_text_rows()
        packages <- article_lab_full_text_package_rows(rows)
        article_lab_debug_log("full_text_generate_rows_loaded", list(full_text_rows = nrow(rows), package_rows = nrow(packages)))
        selected_ids <- collect_selected_ids(packages, "article_lab_full_text_packages", snapshot_ids = input$article_lab_full_text_packages_selected_snapshot, key_col = "outline_id")
        article_lab_debug_log("full_text_generate_selection", list(selected_n = length(selected_ids), selected_ids = selected_ids))
        if (length(selected_ids) == 0) {
          list(
            ok = FALSE,
            notice = "Select one approved outline before generating a full article draft.",
            error = list(
              kind = "no_selection",
              reason = "No approved outline was selected. If the Outline tab still shows the outline as draft, approve it first; only approved outlines appear in Full Text.",
              mode = "not_started",
              model = input$article_lab_full_text_model %||% article_lab_default_full_text_model,
              selected_ids = character()
            )
          )
        } else {
          if (length(selected_ids) > 1) selected_ids <- selected_ids[[1]]
          selected_rows <- packages[packages$outline_id %in% selected_ids, , drop = FALSE]
          if (nrow(selected_rows) == 0) {
            list(
              ok = FALSE,
              notice = "Selected outlines are no longer available for full article generation.",
              error = list(
                kind = "stale_selection",
                reason = "The selected outline is no longer in the approved Full Text package list. Refresh, approve the outline if needed, then select it again.",
                mode = "not_started",
                model = input$article_lab_full_text_model %||% article_lab_default_full_text_model,
                selected_ids = selected_ids
              )
            )
          } else {
            list(ok = TRUE, selected_rows = selected_rows, selected_ids = selected_ids)
          }
        }
      } else {
        selected_ids <- unique(selected_rows$outline_id %||% character())
        list(ok = TRUE, selected_rows = selected_rows, selected_ids = selected_ids)
      }
      }, error = function(e) {
        list(ok = FALSE, notice = paste("Full article generation failed before the API call:", conditionMessage(e), "Debug log: .local_gitignored/article_lab_debug.log"), error = list(kind = "exception", reason = conditionMessage(e), mode = "exception", model = input$article_lab_full_text_model %||% article_lab_default_full_text_model, selected_ids = character()))
      })
    if (!isTRUE(result$ok)) {
      if (!is.null(result$error)) {
        article_lab_debug_log("full_text_generate_error", list(kind = result$error$kind %||% "unknown", reason = result$error$reason %||% result$notice %||% "unknown", selected_ids = result$error$selected_ids %||% character(), elapsed_seconds = as.numeric(difftime(Sys.time(), started_at, units = "secs"))))
        article_lab_state$last_full_text_generate_error <- result$error
        article_lab_state$last_full_text_generate_error_at <- Sys.time()
      }
      article_lab_state$notice <- result$notice
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    result <- tryCatch({
      selected_rows <- result$selected_rows
      summary_contexts <- load_article_lab_batch_summary_contexts(con, unique(selected_rows$batch_id))
      selected_rows$article_summary <- NA_character_
      selected_rows$pdf_local_path <- NA_character_
      selected_rows$summary_id <- NA_integer_
      if (nrow(summary_contexts) > 0) {
        matched_summary <- match(selected_rows$batch_id, summary_contexts$batch_id)
        selected_rows$article_summary <- summary_contexts$article_summary[matched_summary]
        selected_rows$pdf_local_path <- summary_contexts$pdf_local_path[matched_summary]
        selected_rows$summary_id <- summary_contexts$summary_id[matched_summary]
      }
      article_lab_debug_log("full_text_generate_context_loaded", list(selected_rows = nrow(selected_rows), summary_context_rows = nrow(summary_contexts), pdf_context_n = sum(!is.na(selected_rows$pdf_local_path) & nzchar(selected_rows$pdf_local_path)), summary_context_n = sum(!is.na(selected_rows$article_summary) & nzchar(selected_rows$article_summary))))
      generated <- generate_full_text_drafts(
        con,
        selected_rows,
        model = input$article_lab_full_text_model,
        prompt = input$article_lab_full_text_prompt,
        prompt_key = input$article_lab_full_text_prompt_key,
        include_context = isTRUE(input$article_lab_full_text_include_context)
      )
      article_lab_debug_log("full_text_generate_drafts_returned", list(mode = generated$mode %||% "unknown", model = generated$model %||% article_lab_default_full_text_model, generated_rows = nrow(generated$rows), fallback_reason = generated$fallback_reason %||% NA_character_))
      if (identical(generated$mode, "failed")) {
        list(ok = FALSE, notice = paste("Full article API call failed. No draft was saved.", generated$fallback_reason %||% "See .local_gitignored/article_lab_debug.log for details."), error = list(kind = "api_failed", reason = generated$fallback_reason %||% "Unknown API failure (see .local_gitignored/article_lab_debug.log).", mode = generated$mode, model = generated$model %||% article_lab_default_full_text_model, selected_ids = result$selected_ids))
      } else if (nrow(generated$rows) == 0) {
        list(ok = FALSE, notice = "Full article API call returned no usable draft rows. No draft was saved.", error = list(kind = "no_rows", reason = "OpenAI returned an empty or unparseable full article response. No full article draft was saved.", mode = generated$mode %||% "unknown", model = generated$model %||% article_lab_default_full_text_model, selected_ids = result$selected_ids))
      } else {
        inserted_n <- article_lab_insert_full_text_drafts(con, generated$rows, prompt_key = input$article_lab_full_text_prompt_key, prompt_version = input$article_lab_full_text_prompt_key)
        article_lab_debug_log("full_text_generate_inserted", list(inserted_n = inserted_n))
        warning_copy <- if (length(generated$warnings %||% list()) > 0) paste0(" ", length(generated$warnings), " validation warning(s); see helper stderr for details.") else ""
        list(ok = TRUE, notice = sprintf("Generated %s full article draft%s using model %s in %s mode in %s.%s", inserted_n, ifelse(inserted_n == 1L, "", "s"), generated$model %||% article_lab_default_full_text_model, generated$mode %||% "unknown", article_lab_format_duration(as.numeric(difftime(Sys.time(), started_at, units = "secs"))), warning_copy))
      }
    }, error = function(e) {
      article_lab_debug_log("full_text_generate_error", list(message = conditionMessage(e), call = paste(deparse(conditionCall(e)), collapse = " "), elapsed_seconds = as.numeric(difftime(Sys.time(), started_at, units = "secs"))))
      list(ok = FALSE, notice = paste("Full article generation failed:", conditionMessage(e), "Debug log: .local_gitignored/article_lab_debug.log"), error = list(kind = "exception", reason = conditionMessage(e), mode = "exception", model = input$article_lab_full_text_model %||% article_lab_default_full_text_model, selected_ids = character()))
    })
    article_lab_state$notice <- result$notice
    if (isTRUE(result$ok) || is.null(result$error)) {
      article_lab_state$last_full_text_generate_error <- NULL
      article_lab_state$last_full_text_generate_error_at <- NULL
    } else {
      article_lab_state$last_full_text_generate_error <- result$error
      article_lab_state$last_full_text_generate_error_at <- Sys.time()
    }
    article_lab_refresh(article_lab_refresh() + 1L)
    invisible(NULL)
  }

  observeEvent(input$article_lab_generate_full_text, {
    generate_selected_full_text(variant = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_full_text_variant, {
    generate_selected_full_text(variant = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_regenerate_full_text_draft, {
    rows <- article_lab_full_text_rows()
    draft_rows <- rows[!is.na(rows$full_text_draft_id) & nzchar(rows$full_text_draft_id) & rows$draft_status == "draft", , drop = FALSE]
    selected_ids <- collect_selected_ids(draft_rows, "article_lab_full_text_drafts", snapshot_ids = input$article_lab_full_text_drafts_selected_snapshot, key_col = "full_text_draft_id")
    if (length(selected_ids) != 1L) {
      article_lab_state$notice <- "Select exactly one unapproved full article draft before regenerating it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    selected_draft <- draft_rows[draft_rows$full_text_draft_id %in% selected_ids[[1]], , drop = FALSE]
    package_rows <- article_lab_full_text_package_rows(rows)
    selected_package <- package_rows[package_rows$outline_id %in% selected_draft$outline_id[[1]], , drop = FALSE]
    if (nrow(selected_package) == 0) {
      article_lab_state$notice <- "The selected draft's outline is no longer available for regeneration."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    generate_selected_full_text(variant = TRUE, selected_rows = selected_package)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_save_full_text_drafts, {
    updated_n <- article_lab_update_full_text_drafts(con, collect_full_text_updates(article_lab_full_text_rows()))
    article_lab_state$notice <- sprintf("Saved %s full article draft%s and recorded revision rows for changed text.", updated_n, ifelse(updated_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_approve_full_text_draft, {
    rows <- article_lab_full_text_rows()
    article_lab_update_full_text_drafts(con, collect_full_text_updates(rows))
    draft_rows <- rows[!is.na(rows$full_text_draft_id) & nzchar(rows$full_text_draft_id) & rows$draft_status == "draft", , drop = FALSE]
    selected_ids <- collect_selected_ids(draft_rows, "article_lab_full_text_drafts", snapshot_ids = input$article_lab_full_text_drafts_selected_snapshot, key_col = "full_text_draft_id")
    if (length(selected_ids) != 1L) {
      article_lab_state$notice <- "Select exactly one draft before approving it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- article_lab_approve_full_text_draft(con, selected_ids[[1]])
    article_lab_state$notice <- sprintf("Approved %s full article draft. Package moved to Review & Publish.", result$approved_n)
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_reject_full_text_draft, {
    rows <- article_lab_full_text_rows()
    draft_rows <- rows[!is.na(rows$full_text_draft_id) & nzchar(rows$full_text_draft_id) & rows$draft_status == "draft", , drop = FALSE]
    selected_ids <- collect_selected_ids(draft_rows, "article_lab_full_text_drafts", snapshot_ids = input$article_lab_full_text_drafts_selected_snapshot, key_col = "full_text_draft_id")
    if (length(selected_ids) == 0) {
      article_lab_state$notice <- "Select at least one unapproved draft before rejecting it."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    rejected_n <- sum(vapply(selected_ids, function(id) article_lab_reject_full_text_draft(con, id), numeric(1)), na.rm = TRUE)
    article_lab_state$notice <- sprintf("Rejected %s full article draft%s. No rows were deleted.", rejected_n, ifelse(rejected_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_full_text, {
    updated_n <- article_lab_update_full_text_drafts(con, collect_full_text_updates(article_lab_full_text_rows()))
    article_lab_state$notice <- sprintf("Refreshed Full Article and saved %s editable draft%s.", updated_n, ifelse(updated_n == 1L, "", "s"))
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  article_lab_current_publish_values <- reactive({
    list(
      medium_tags = input$article_lab_publish_medium_tags %||% "",
      publishing_target = input$article_lab_publishing_target %||% "Do not publish yet",
      publication_id = input$article_lab_publication_id %||% "",
      new_publication_name = input$article_lab_new_publication_name %||% "",
      monetization = input$article_lab_monetization %||% "Undecided",
      canonical_url = input$article_lab_canonical_url %||% "",
      featured_image_alt_text = input$article_lab_featured_image_alt_text %||% "",
      image_credit_source = input$article_lab_image_credit_source %||% "",
      published_url = input$article_lab_published_url %||% "",
      publish_status = input$article_lab_publish_status %||% "ready_for_review_publish",
      notes = input$article_lab_publish_notes %||% ""
    )
  })

  observeEvent(input$article_lab_save_publish_settings, {
    row <- article_lab_selected_review_publish_row()
    if (nrow(row) == 0) {
      article_lab_state$notice <- "Select an approved full article draft before saving publish settings."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    tags <- article_lab_parse_medium_tags(input$article_lab_publish_medium_tags %||% "")
    saved_n <- article_lab_save_publish_settings(con, row, article_lab_current_publish_values())
    article_lab_state$last_review_publish_archive_error <- NULL
    article_lab_state$last_review_publish_archive_error_at <- NULL
    tag_note <- if (length(tags) >= 5L) " Medium tags were capped at 5." else ""
    article_lab_state$notice <- sprintf("Saved publish settings for %s approved draft.%s", saved_n, tag_note)
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_archive_review_publish, {
    row <- article_lab_selected_review_publish_row()
    if (nrow(row) == 0) {
      article_lab_state$last_review_publish_archive_error <- list(
        kind = "no_selection",
        reason = "Select an approved full article draft before archiving it.",
        selected_ids = character()
      )
      article_lab_state$last_review_publish_archive_error_at <- Sys.time()
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    draft_id <- article_lab_row_value(row, "full_text_draft_id")
    values <- article_lab_current_publish_values()
    values$publish_status <- "archived"
    saved_n <- tryCatch(
      article_lab_save_publish_settings(con, row, values),
      error = function(e) {
        article_lab_state$last_review_publish_archive_error <- list(
          kind = "exception",
          reason = conditionMessage(e),
          selected_ids = draft_id
        )
        article_lab_state$last_review_publish_archive_error_at <- Sys.time()
        NA_integer_
      }
    )
    if (is.na(saved_n)) {
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    if (saved_n < 1L) {
      article_lab_state$last_review_publish_archive_error <- list(
        kind = "no_rows",
        reason = "The archive write completed but did not update or insert a publish settings row.",
        selected_ids = draft_id
      )
      article_lab_state$last_review_publish_archive_error_at <- Sys.time()
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }

    article_lab_state$last_review_publish_archive_error <- NULL
    article_lab_state$last_review_publish_archive_error_at <- NULL
    article_lab_state$notice <- sprintf("Archived approved article draft %s. No rows were deleted.", draft_id)
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_generate_medium_tags, {
    row <- article_lab_selected_review_publish_row()
    if (nrow(row) == 0) {
      article_lab_state$notice <- "Select an approved full article draft before generating Medium tags."
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    result <- tryCatch(
      article_lab_medium_tags_api_request(
        row,
        model = input$article_lab_medium_tags_model %||% article_lab_default_medium_tags_model,
        prompt = input$article_lab_medium_tags_prompt %||% article_lab_default_medium_tags_prompt
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(result$error)) {
      article_lab_state$notice <- paste("Medium tag generation failed:", result$error)
      article_lab_refresh(article_lab_refresh() + 1L)
      return(invisible(NULL))
    }
    updateTextInput(session, "article_lab_publish_medium_tags", value = paste(result$tags, collapse = ", "))
    article_lab_state$notice <- sprintf("Generated %s Medium tag%s with %s. Review and save publish settings to persist them.", length(result$tags), ifelse(length(result$tags) == 1L, "", "s"), result$model %||% article_lab_default_medium_tags_model)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_publish, {
    article_lab_state$notice <- "Refreshed Review & Publish."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  output$article_lab_export_markdown <- downloadHandler(
    filename = function() {
      row <- article_lab_selected_review_publish_row()
      title <- if (nrow(row) > 0) article_lab_row_value(row, "title", "approved_article") else "approved_article"
      slug <- tolower(gsub("[^A-Za-z0-9]+", "-", title))
      slug <- gsub("(^-+|-+$)", "", slug)
      if (!nzchar(slug)) slug <- "approved-article"
      paste0(slug, "-", format(Sys.Date(), "%Y-%m-%d"), ".md")
    },
    content = function(file) {
      row <- article_lab_selected_review_publish_row()
      text <- article_lab_medium_ready_markdown(row, row)
      writeLines(text, file, useBytes = TRUE)
    }
  )

  observeEvent(input$article_lab_refresh_scores, {
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_queue_rows(), "article_lab_queue_notes"))
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_scored_rows(), "article_lab_scored_notes"))
    article_lab_state$notice <- "Refreshed API Scoring and saved visible notes."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_subtitles, {
    article_lab_update_candidate_notes(con, collect_candidate_note_updates(article_lab_subtitle_target_rows(), "article_lab_subtitle_title_notes"))
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(article_lab_pending_subtitle_rows(), "article_lab_subtitle_candidate_notes"))
    article_lab_state$notice <- "Refreshed Subtitle Generation and saved visible notes."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_refresh_thumbnails, {
    article_lab_update_subtitle_notes(con, collect_subtitle_note_updates(article_lab_thumbnail_package_rows(), "article_lab_thumbnail_package_notes"))
    article_lab_update_thumbnail_notes(con, collect_thumbnail_note_updates(article_lab_pending_thumbnail_rows(), "article_lab_thumbnail_candidate_notes"))
    article_lab_state$notice <- "Refreshed Thumbnails and saved visible notes."
    article_lab_refresh(article_lab_refresh() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$article_lab_open_docs, {
    showModal(modalDialog(
      title = "Article Lab workflow docs",
      p("Source-of-truth docs for this workflow:"),
      tags$ul(
        tags$li("data/analysis/article_lab/2026-05-23_title_lab_scoring_and_workflow_summary.md"),
        tags$li("data/analysis/article_lab/2026-05-23_human_score_and_api_human_combination_notes.md")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)

  output$guide_content <- renderUI({
    if (article_lab_is_workflow_section(active_section()) || identical(active_section(), "settings")) {
      current_section <- active_section()
      overview <- article_lab_overview_stats()
      batches <- article_lab_batches()
      latest_saved_batch <- article_lab_saved_batch()
      selected_batch_id <- article_lab_selected_batch_id()
      selected_candidates <- article_lab_selected_batch_candidates()
      selected_batch <- if (nrow(batches) > 0 && !is.na(selected_batch_id) && nzchar(selected_batch_id)) {
        batches[batches$batch_id == selected_batch_id, , drop = FALSE]
      } else {
        data.frame()
      }
      current_batch_label <- if (identical(current_section, "generate") && !is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
        sprintf("Unsaved draft with %s titles", nrow(article_lab_state$draft))
      } else if (identical(current_section, "generate") && !is.null(latest_saved_batch) && nrow(latest_saved_batch) > 0) {
        sprintf("Latest saved batch %s", latest_saved_batch$batch_id[[1]])
      } else if (identical(selected_batch_id, article_lab_all_batches_value)) {
        "All saved titles across batches"
      } else if (nrow(selected_batch) > 0) {
        sprintf("Saved batch %s", selected_batch$batch_id[[1]])
      } else {
        "No batch saved yet"
      }
      current_batch_meta <- if (identical(current_section, "generate") && !is.null(article_lab_state$draft) && nrow(article_lab_state$draft) > 0) {
        paste("Draft created at", article_lab_state$draft_created_at %||% now_utc())
      } else if (identical(current_section, "generate") && !is.null(latest_saved_batch) && nrow(latest_saved_batch) > 0) {
        paste("Created", latest_saved_batch$created_at[[1]], "\u00b7 model", first_value(latest_saved_batch, "model", article_lab_default_model))
      } else if (identical(selected_batch_id, article_lab_all_batches_value)) {
        "The current selection spans all saved batches."
      } else if (nrow(selected_batch) > 0) {
        paste("Created", selected_batch$created_at[[1]], "\u00b7 model", first_value(selected_batch, "model", article_lab_default_model))
      } else {
        "Generate first, then save to persist candidates."
      }
      ready_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "ready_for_api_scoring", na.rm = TRUE)
      scored_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "api_scored", na.rm = TRUE)
      approved_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "approved_for_subtitle", na.rm = TRUE)
      subtitle_ready_n <- if (nrow(selected_candidates) == 0) 0L else sum(selected_candidates$normalized_status == "ready_for_thumbnail", na.rm = TRUE)

      if (current_section %in% c("research_inbox", "api_scoring", "subtitle_generation", "thumbnails")) {
        return(NULL)
      }

      return(tagList(
        div(
          class = "status-card",
          h3("Article Lab status"),
          div(class = "status-metric", overview$saved_candidates[[1]]),
          p(sprintf("%s saved candidates across %s batches.", overview$saved_candidates[[1]], overview$saved_batches[[1]])),
          p(class = "lab-status-copy", sprintf("%s remain New, %s are in API queue, %s are approved for subtitles, %s are ready for Thumbnails, and %s are ready for Outline.", overview$generated[[1]], overview$ready_for_api_scoring[[1]], overview$approved_for_subtitle[[1]], overview$ready_for_thumbnail[[1]], overview$ready_for_outline[[1]]))
        ),
        div(
          class = "status-card",
          h3("Current selection"),
          p(current_batch_label),
          p(class = "lab-status-copy", current_batch_meta)
        ),
        div(
          class = "status-card",
          h3("Reminder"),
          p("Home remains the separate rating workflow."),
          p(class = "lab-status-copy", sprintf("This pass now covers Generate, API Scoring, Subtitle Generation, and Thumbnails. %s title%s are ready for Thumbnails in the current selection.", subtitle_ready_n, ifelse(subtitle_ready_n == 1, "", "s")))
        )
      ))
    }

    if (is_dimension_mode) {
      field <- active_dimension()
      if (is.na(field)) {
        return(tagList(
          div(class = "guide-section", h3("Dimension pass"), p("All dimension passes are complete.")),
          div(class = "tip", h3("Reminder"), p("No outcome, API, or prior human score data is shown during rating."))
        ))
      }
      return(tagList(
        div(class = "guide-section", h3("Active dimension"), p(dimension_labels[[field]])),
        div(class = "guide-section", h3("Focus"), p(dimension_focus[[field]])),
        div(
          class = "guide-section",
          h3("Hotkeys"),
          if (field == "ai_low_effort_flag") {
            tags$ul(tags$li("A or 1 = yes"), tags$li("S or 2 = unsure"), tags$li("J or 3 = no"))
          } else {
            tags$ul(tags$li("A/S/D/F/J = 1/2/3/4/5"), tags$li("1 through 5 also work"))
          }
        ),
        div(class = "tip", h3("Reminder"), p("Score only the active dimension. Do not judge the other dimensions during this pass."))
      ))
    }

    current_item <- current()
    title_only_home <- !is.null(current_item) && identical(first_value(current_item, "source_type", "dataset"), "article_lab_generated")

    tagList(
      div(
        class = "guide-section",
        h3("How it works"),
        p(if (title_only_home) {
          "Rate this title-only candidate using only the visible headline."
        } else {
          "Rate each preview using only the visible headline, subtitle, and thumbnail."
        })
      ),
      div(
        class = "guide-section",
        h3("Focus on"),
        if (title_only_home) {
          tags$ul(
            tags$li("Headline clarity and hook"),
            tags$li("Specificity and credibility"),
            tags$li("Curiosity without clickbait"),
            tags$li("Your gut reaction to the title alone")
          )
        } else {
          tags$ul(
            tags$li("Headline clarity and hook"),
            tags$li("Topic relevance and appeal"),
            tags$li("Perceived value to readers"),
            tags$li("Your gut feeling")
          )
        }
      ),
      div(
        class = "guide-section",
        h3("Rating guide"),
        p("1 Very weak"),
        p("2 Weak"),
        p("3 Average / unclear"),
        p("4 Strong"),
        p("5 Very strong")
      ),
      div(class = "tip", h3("Tip"), p("There are no right or wrong answers. Consistency is the goal."))
    )
  })

  output$article_area <- renderUI({
    item <- current()
    if (is.null(item)) {
      if (is_dimension_mode) {
        field <- active_dimension()
        if (is.na(field)) {
          return(div(class = "done-state", h2("All dimensions complete"), p("Every dimension pass has been completed for the cohort.")))
        }
        return(div(class = "done-state", h2("Dimension complete"), p(paste("Completed pass:", dimension_labels[[field]]))))
      }
      return(div(class = "done-state", h2("Session complete"), p("All queued previews have been rated or skipped.")))
    }

    field <- if (is_dimension_mode) active_dimension() else NA_character_
    render_info <- if (is_dimension_v2_mode) v2_render_info(item) else NULL
    is_article_lab_title_only <- identical(first_value(item, "source_type", "dataset"), "article_lab_generated")
    thumbnail_path <- item$local_thumbnail_path[[1]]
    thumbnail_path_abs <- if (is_dimension_v2_mode) {
      render_info$path_abs
    } else if ("local_thumbnail_path_abs" %in% names(item)) {
      item$local_thumbnail_path_abs[[1]]
    } else {
      as_abs_path(thumbnail_path)[[1]]
    }
    thumbnail_status <- if ("thumbnail_status" %in% names(item)) item$thumbnail_status[[1]] else NA_character_
    has_thumbnail <- if (is_dimension_v2_mode) {
      isTRUE(render_info$valid)
    } else {
      identical(thumbnail_status, "valid") && !is.na(thumbnail_path_abs) && file.exists(thumbnail_path_abs)
    }
    isolate_title_field <- is_article_lab_title_only || (is_dimension_mode && !is.na(field) && field %in% title_isolation_dimension_fields)
    thumbnail_ui <- if (isolate_title_field) {
      div(class = "thumbnail-placeholder", div(class = "thumbnail-invalid-label", title_only_placeholder_thumbnail_label))
    } else if (has_thumbnail) {
      imageOutput("thumbnail", width = "170px", height = "113px")
    } else if (is_dimension_v2_mode) {
      missing_reason <- render_info$reason %in% c("missing_file", "missing_manifest_hash", "missing_rendered_hash")
      placeholder_label <- if (isTRUE(missing_reason)) {
        "Thumbnail missing: validated manifest image unavailable"
      } else {
        "Thumbnail blocked: manifest/hash mismatch"
      }
      div(class = "thumbnail-placeholder error", div(class = "thumbnail-invalid-label", placeholder_label))
    } else {
      div(class = "thumbnail-placeholder", div(class = "thumbnail-invalid-label", "Invalid or missing thumbnail"))
    }

    subtitle <- if (is_article_lab_title_only) title_only_placeholder_subtitle else displayed_subtitle_for_field(item, field)
    thumbnail_only <- is_dimension_mode && !is.na(field) && field %in% thumbnail_only_dimension_fields
    text_only <- is_article_lab_title_only || (is_dimension_mode && !is.na(field) && field %in% text_only_dimension_fields)

    if (text_only) {
      return(div(
        class = "article-card",
        div(
          h2(class = "article-title", item$title[[1]]),
          if (!is.na(subtitle)) p(class = "article-subtitle", subtitle)
        ),
        div(class = "thumbnail-wrap", thumbnail_ui)
      ))
    }

    if (thumbnail_only) {
      return(div(
        class = "article-card thumbnail-only",
        div(class = "thumbnail-wrap", thumbnail_ui)
      ))
    }

    div(
      class = "article-card",
      div(
        h2(class = "article-title", item$title[[1]]),
        if (!is.na(subtitle)) p(class = "article-subtitle", subtitle)
      ),
      div(class = "thumbnail-wrap", thumbnail_ui)
    )
  })

  output$thumbnail <- renderImage({
    item <- current()
    req(!is.null(item))
    if (is_dimension_v2_mode) {
      info <- v2_render_info(item)
      req(isTRUE(info$valid))
      path_abs <- info$path_abs
    } else {
      path <- item$local_thumbnail_path[[1]]
      path_abs <- if ("local_thumbnail_path_abs" %in% names(item)) {
      item$local_thumbnail_path_abs[[1]]
      } else {
        as_abs_path(path)[[1]]
      }
    }
    req(!is.na(path_abs), file.exists(path_abs))
    list(src = normalizePath(path_abs, mustWork = TRUE), alt = "", width = 170, height = 113)
  }, deleteFile = FALSE)

  output$rating_panel <- renderUI({
    if (!is_dimension_mode) {
      current_item <- current()
      prompt_text <- if (!is.null(current_item) && identical(first_value(current_item, "source_type", "dataset"), "article_lab_generated")) {
        "Based only on the title, how likely is this article to perform well on Medium?"
      } else {
        rating_prompt
      }
      score_label <- function(score, shortcut) {
        if (article_lab_design_v2) {
          tagList(
            span(class = "rating-score-value", as.character(score)),
            span(class = "rating-score-shortcut", shortcut)
          )
        } else {
          as.character(score)
        }
      }
      return(div(
        class = "rating-panel",
        div(class = "prompt", prompt_text),
        div(
          class = "note-row",
          textInput(
            "note",
            "Optional note",
            value = "",
            width = "100%",
            placeholder = "Quick note, e.g. AI thumbnail, strong title, generic topic"
          )
        ),
        div(class = "scale-labels", span("Very weak"), span("Very strong")),
        div(
          class = "rating-buttons",
          actionButton("score_1", score_label(1, "A or 1")),
          actionButton("score_2", score_label(2, "S or 2")),
          actionButton("score_3", score_label(3, "D or 3")),
          actionButton("score_4", score_label(4, "F or 4")),
          actionButton("score_5", score_label(5, "J or 5"))
        ),
        div(
          class = "rating-actions",
          div(
            actionButton("skip", if (article_lab_design_v2) tagList("Skip", span(class = "rating-action-shortcut", "Space")) else "Skip"),
            actionButton("undo", if (article_lab_design_v2) tagList("Undo previous", span(class = "rating-action-shortcut", "U")) else "Undo previous")
          ),
          if (article_lab_design_v2) {
            div(
              class = "rating-shortcut-buttons",
              tags$button(
                type = "button",
                class = "shortcut-key-button",
                onclick = "var note = document.getElementById('note'); if (note) { note.focus(); }",
                span(class = "shortcut-key", "N"),
                span(class = "shortcut-key-label", "Focus note")
              ),
              tags$button(
                type = "button",
                class = "shortcut-key-button",
                tabindex = "-1",
                span(class = "shortcut-key", "Enter"),
                span(class = "shortcut-key-label", "Exit note")
              ),
              tags$button(
                type = "button",
                class = "shortcut-key-button",
                tabindex = "-1",
                span(class = "shortcut-key", "Esc"),
                span(class = "shortcut-key-label", "Exit note")
              )
            )
          } else {
            div(class = "shortcut-copy", "A=1, S=2, D=3, F=4, J=5 · 1-5 also work · Space=skip · U=undo · N=note · Enter/Esc exits note")
          }
        )
      ))
    }

    field <- active_dimension()
    c <- counts()
    completed <- ifelse(is.na(c$completed[[1]]), 0, c$completed[[1]])
    total <- ifelse(is.na(c$total[[1]]), 0, c$total[[1]])

    if (is.na(field)) {
      return(div(
        class = "rating-panel",
        div(class = "dimension-pass-header",
            div(class = "dimension-pass-kicker", "Dimension pass complete"),
            div(class = "dimension-pass-name", "All dimensions complete"),
            div(
              class = "dimension-pass-focus",
              if (is_dimension_v2_mode) {
                sprintf(
                  "Overall manual rating progress: %s / %s ratings complete",
                  candidate_stats()$completed_dimensions[[1]],
                  candidate_stats()$total_dimensions[[1]]
                )
              } else {
                sprintf("Overall active dimension progress: %s / %s dimensions complete", length(active_dimension_fields), length(active_dimension_fields))
              }
            )
        )
      ))
    }

    if (total > 0 && completed >= total) {
      next_field <- next_incomplete_dimension_after(con, field)
      return(div(
        class = "rating-panel",
        div(class = "dimension-pass-header",
            div(class = "dimension-pass-kicker", "Dimension complete"),
            div(class = "dimension-pass-name", dimension_labels[[field]]),
            div(
              class = "dimension-pass-focus",
              if (is_dimension_v2_mode) {
                sprintf(
                  "Dimension progress: %s / %s · Overall manual rating progress: %s / %s ratings complete",
                  completed,
                  total,
                  candidate_stats()$completed_dimensions[[1]],
                  candidate_stats()$total_dimensions[[1]]
                )
              } else {
                sprintf("Dimension progress: %s / %s", completed, total)
              }
            )
        ),
        if (!is.na(next_field)) {
          div(
            class = "next-dimension-cta",
            div(class = "next-dimension-copy", sprintf("This pass is finished. Continue directly into the next dimension: %s.", dimension_labels[[next_field]])),
            actionButton("start_next_dimension", paste("Continue To", dimension_labels[[next_field]]))
          )
        } else {
          div(class = "shortcut-copy", "All dimension passes are complete.")
        }
      ))
    }

    item <- current()
    can_rate_current <- !is_dimension_v2_mode ||
      field %in% text_only_dimension_fields ||
      (!is.null(item) && isTRUE(v2_render_info(item)$valid))

    numeric_buttons <- function(field, enabled = TRUE) {
      div(
        class = "dimension-buttons",
        lapply(1:5, function(score) {
          tags$button(
            type = "button",
            class = "btn dimension-choice",
            `data-field` = field,
            `data-value` = as.character(score),
            disabled = if (isTRUE(enabled)) NULL else "disabled",
            as.character(score)
          )
        })
      )
    }
    flag_buttons <- function(field, enabled = TRUE) {
      choices <- c("yes", "unsure", "no")
      shortcuts <- c(yes = "D", unsure = "F", no = "J")
      div(
        class = "dimension-buttons",
        lapply(choices, function(choice) {
          tags$button(
            type = "button",
            class = "btn dimension-choice flag-choice",
            `data-field` = field,
            `data-value` = choice,
            disabled = if (isTRUE(enabled)) NULL else "disabled",
            span(class = "dimension-choice-label", choice),
            span(class = "dimension-choice-shortcut", shortcuts[[choice]])
          )
        })
      )
    }
    scale_ui <- function(field) {
      scale <- dimension_scale[[field]]
      scale_shortcuts <- c("1" = "A=1", "2" = "S=2", "3" = "D=3", "4" = "F=4", "5" = "J=5")
      flag_shortcuts <- c(yes = "S", unsure = "D", no = "J")
      div(
        class = paste("dimension-scale-list", if (field == "ai_low_effort_flag") "dimension-flag-scale" else ""),
        lapply(names(scale), function(name) {
          if (field == "ai_low_effort_flag") {
            tags$button(
              type = "button",
              class = "btn dimension-scale-item dimension-scale-choice dimension-choice",
              `data-field` = field,
              `data-value` = as.character(name),
              disabled = if (isTRUE(can_rate_current)) NULL else "disabled",
              strong(scale[[name]]),
              span(class = "dimension-scale-shortcut", flag_shortcuts[[as.character(name)]])
            )
          } else {
            tags$button(
              type = "button",
              class = "btn dimension-scale-item dimension-scale-choice dimension-choice",
              `data-field` = field,
              `data-value` = as.character(name),
              disabled = if (isTRUE(can_rate_current)) NULL else "disabled",
              strong(name),
              span(scale[[name]]),
              if (as.character(name) %in% names(scale_shortcuts)) {
                span(class = "dimension-scale-shortcut", scale_shortcuts[[as.character(name)]])
              }
            )
          }
        })
      )
    }

    verification_title <- if (
      !is.null(item) &&
        field %in% thumbnail_only_dimension_fields &&
        "title" %in% names(item) &&
        !is.na(item$title[[1]])
    ) {
      div(
        class = "dimension-verification-title",
        `data-copy-title` = item$title[[1]],
        title = "Click to copy title",
        item$title[[1]]
      )
    } else {
      NULL
    }

    div(
      class = "rating-panel",
      div(
        class = "dimension-pass-header",
        div(class = "dimension-pass-kicker", "Dimension pass"),
        div(class = "dimension-pass-name", paste("Active dimension:", dimension_labels[[field]])),
        div(class = "dimension-pass-focus", strong("Focus: "), dimension_focus[[field]]),
        div(class = "dimension-pass-question", strong("Question: "), dimension_questions[[field]]),
        div(
          class = "dimension-pass-focus",
          if (is_dimension_v2_mode) {
            sprintf(
              "Dimension progress: %s / %s · Overall manual rating progress: %s / %s ratings complete",
              completed,
              total,
              candidate_stats()$completed_dimensions[[1]],
              candidate_stats()$total_dimensions[[1]]
            )
          } else {
            sprintf(
              "Dimension progress: %s / %s · Overall active dimension progress: %s / %s dimensions complete",
              completed,
              total,
              candidate_stats()$completed_dimensions[[1]],
              candidate_stats()$total_dimensions[[1]]
            )
          }
        ),
        verification_title
      ),
      scale_ui(field),
      div(
        class = "note-row",
        textAreaInput(
          "note",
          "Optional note",
          value = "",
          width = "100%",
          height = "54px",
          placeholder = "Optional note"
        )
      ),
      div(
        class = "rating-actions",
        div(actionButton("skip", "Skip"), actionButton("undo", "Undo previous")),
        div(
          class = "shortcut-copy",
          if (field == "ai_low_effort_flag") {
            "S=yes, D=unsure, J=no · 1=yes, 2=unsure, 3=no · Space=skip, U=undo, N=note"
          } else {
            "A=1, S=2, D=3, F=4, J=5 · 1-5 also work · Space=skip, U=undo, N=note"
          }
        )
      )
    )
  })

  handle_score <- function(score) {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (is_dimension_mode) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    save_current_rating(con, item, score = score, note = input$note, skipped = FALSE, shown_started_at = shown_started_at())
    refresh_current()
  }

  observeEvent(input$score_1, handle_score(1L), ignoreInit = TRUE)
  observeEvent(input$score_2, handle_score(2L), ignoreInit = TRUE)
  observeEvent(input$score_3, handle_score(3L), ignoreInit = TRUE)
  observeEvent(input$score_4, handle_score(4L), ignoreInit = TRUE)
  observeEvent(input$score_5, handle_score(5L), ignoreInit = TRUE)
  observeEvent(input$score_key, {
    score_value <- input$score_key
    if (is.list(score_value) && !is.null(score_value$score)) {
      score_value <- score_value$score
    }
    handle_score(as.integer(score_value))
  }, ignoreInit = TRUE)

  observeEvent(input$skip, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    if (is_dimension_mode) {
      save_current_dimension_rating(con, item, active_dimension(), note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    } else {
      save_current_rating(con, item, score = NULL, note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    }
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$skip_key, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    if (is_dimension_mode) {
      save_current_dimension_rating(con, item, active_dimension(), note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    } else {
      save_current_rating(con, item, score = NULL, note = input$note, skipped = TRUE, shown_started_at = shown_started_at())
    }
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$undo, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (is_dimension_mode) undo_previous_dimension_rating(con, active_dimension()) else undo_previous_rating(con, rating_session_id)
    refresh_current()
  }, ignoreInit = TRUE)

  observeEvent(input$undo_key, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (is_dimension_mode) undo_previous_dimension_rating(con, active_dimension()) else undo_previous_rating(con, rating_session_id)
    refresh_current()
  }, ignoreInit = TRUE)

  apply_dimension_value <- function(field, value) {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (!is_dimension_mode || !(field %in% dimension_fields)) return(invisible(NULL))
    if (!identical(field, active_dimension())) return(invisible(NULL))
    item <- current()
    if (is.null(item)) return(invisible(NULL))
    if (
      is_dimension_v2_mode &&
        !(field %in% text_only_dimension_fields) &&
        !isTRUE(v2_render_info(item)$valid)
    ) {
      return(invisible(NULL))
    }
    save_current_dimension_rating(
      con,
      item,
      field,
      value = value,
      note = input$note,
      skipped = FALSE,
      shown_started_at = shown_started_at()
    )
    refresh_current()
  }

  observeEvent(input$dimension_select, {
    value <- input$dimension_select
    if (!is.list(value)) return(invisible(NULL))
    apply_dimension_value(value$field, value$value)
  }, ignoreInit = TRUE)

  observeEvent(input$dimension_key, {
    value <- input$dimension_key
    key <- if (is.list(value)) value$key else value
    field <- active_dimension()
    if (is.na(field)) return(invisible(NULL))
    if (field %in% dimension_numeric_fields) {
      numeric_map <- c(a = 1L, s = 2L, d = 3L, f = 4L, j = 5L)
      score <- if (key %in% names(numeric_map)) numeric_map[[key]] else suppressWarnings(as.integer(key))
      apply_dimension_value(field, score)
    } else {
      flag_map <- c(s = "yes", d = "unsure", j = "no", `1` = "yes", `2` = "unsure", `3` = "no")
      if (key %in% names(flag_map)) apply_dimension_value(field, flag_map[[key]])
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dimension_back_key, {
    invisible(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$dimension_reset_key, {
    invisible(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$start_next_dimension, {
    if (!identical(active_section(), "home")) return(invisible(NULL))
    if (!is_dimension_mode) return(invisible(NULL))
    next_field <- next_incomplete_dimension_after(con, active_dimension())
    if (!is.na(next_field)) active_dimension(next_field)
    refresh_current()
  }, ignoreInit = TRUE)

}

shinyApp(ui, server)
