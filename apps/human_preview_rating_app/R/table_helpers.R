# Article Lab table/card UI helpers.
# Behavior-preserving extraction from app.R.

article_lab_generate_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No titles need triage",
      "Generate and save a new batch, or show disqualified titles to review earlier skips.",
      "Next step: create title candidates above, then move selected titles to the API queue."
    ))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  headers <- c("Select", "Title", "Status", "Notes")
  tagList(
    tags$table(
      class = "lab-table",
      tags$thead(tags$tr(lapply(headers, tags$th))),
      tags$tbody(
        lapply(seq_len(nrow(rows)), function(i) {
          row <- rows[i, , drop = FALSE]
          candidate_id <- row$candidate_id[[1]]
          is_draft <- identical(row$normalized_status[[1]], "draft") || identical(row$batch_id[[1]], "(draft)")
          select_id <- article_lab_row_input_id("article_lab_generate_select", candidate_id)
          status_id <- article_lab_row_input_id("article_lab_generate_status", candidate_id)
          notes_id <- article_lab_row_input_id("article_lab_generate_notes", candidate_id)
          tags$tr(
            `data-selection-group` = "article_lab_generate",
            `data-candidate-id` = candidate_id,
            tags$td(
              class = "select-cell",
              checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
            ),
            tags$td(row$title[[1]]),
            tags$td(
              if (is_draft) {
                article_lab_badge("draft")
              } else {
                selectInput(
                  status_id,
                  label = NULL,
                  choices = article_lab_status_choices(c("generated", "disqualified")),
                  selected = if (row$normalized_status[[1]] %in% c("generated", "disqualified")) row$normalized_status[[1]] else "generated",
                  width = "100%"
                )
              }
            ),
            tags$td(
              if (is_draft) {
                span(class = "lab-status-copy", "Save the batch to start triage.")
              } else {
                textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note")
              }
            )
          )
        })
      )
    )
  )
}

article_lab_badge <- function(value) {
  label <- clean_text(value)
  if (length(label) == 0 || is.na(label[[1]])) label <- "n/a"
  status_key <- tolower(label[[1]])
  class_name <- gsub("[^A-Za-z0-9_]+", "_", status_key)
  tags$span(class = paste("lab-badge", class_name), article_lab_status_label(status_key))
}

article_lab_subtitle_badge <- function(value) {
  label <- clean_text(value)
  if (length(label) == 0 || is.na(label[[1]])) label <- "n/a"
  status_key <- tolower(label[[1]])
  class_name <- paste("subtitle", gsub("[^A-Za-z0-9_]+", "_", status_key), sep = "_")
  tags$span(class = paste("lab-badge", class_name), article_lab_subtitle_status_label(status_key))
}

article_lab_thumbnail_badge <- function(value) {
  label <- clean_text(value)
  if (length(label) == 0 || is.na(label[[1]])) label <- "n/a"
  status_key <- tolower(label[[1]])
  class_name <- paste("thumbnail", gsub("[^A-Za-z0-9_]+", "_", status_key), sep = "_")
  tags$span(class = paste("lab-badge", class_name), article_lab_thumbnail_status_label(status_key))
}

article_lab_score_queue_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No queued titles",
      "No titles are currently waiting in the API queue for this selection.",
      "Next step: move generated titles into the API queue from the Generate tab."
    ))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_queue_select", row$candidate_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_queue_notes", row$candidate_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_queue",
              `data-candidate-id` = row$candidate_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows))
  )
}

article_lab_score_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No scored titles to review",
      "No API-scored titles are currently waiting for approval in this selection.",
      "Next step: score queued titles, then approve the strongest titles for subtitle generation."
    ))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  score_value <- function(x) {
    value <- suppressWarnings(as.numeric(x))
    ifelse(is.na(value), "\u2014", format(round(value, 1), nsmall = 1, trim = TRUE))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table scored-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "score-col", "Combined Score"),
          tags$th(class = "signals-col", "Main Signals"),
          tags$th(class = "trust-col", "Trust Risk"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_scored_select", row$candidate_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_scored_notes", row$candidate_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_scored",
              `data-candidate-id` = row$candidate_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "score-cell score-strong", score_value(row$combined_title_score[[1]])),
              tags$td(
                class = "signals-cell",
                div(
                  class = "lab-chip-row",
                  article_lab_signal_chip("Curiosity", row$curiosity[[1]], "blue"),
                  article_lab_signal_chip("Emotional", row$emotional_pull[[1]], "purple"),
                  article_lab_signal_chip("Comment", row$medium_comment_potential[[1]], "orange"),
                  article_lab_signal_chip("Overall", row$overall_article_potential[[1]], "green")
                )
              ),
              tags$td(class = "score-cell trust-cell", score_value(row$trust_risk[[1]])),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows))
  )
}

article_lab_subtitle_target_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No titles need subtitles",
      "No approved titles currently need subtitle candidates in this selection.",
      "Next step: approve scored titles from API Scoring."
    ))
  }

  rows <- article_lab_normalize_candidate_rows(rows)
  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_subtitle_title_select", row$candidate_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_subtitle_title_notes", row$candidate_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_subtitle_titles",
              `data-candidate-id` = row$candidate_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows))
  )
}

article_lab_subtitle_candidate_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No subtitle candidates",
      "No subtitle candidates are currently waiting for approval in this selection.",
      "Next step: generate subtitle candidates or add manual subtitle ideas above."
    ))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "subtitle-col", "Subtitle"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_subtitle_candidate_select", row$subtitle_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_subtitle_candidate_notes", row$subtitle_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_subtitle_candidates",
              `data-candidate-id` = row$subtitle_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "subtitle-cell", row$subtitle[[1]]),
              tags$td(class = "status-cell", article_lab_subtitle_badge(row$subtitle_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows), label = "subtitle candidates")
  )
}

article_lab_thumbnail_package_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No packages need thumbnails",
      "No title/subtitle packages currently need thumbnail candidates in this selection.",
      "Next step: approve subtitle candidates from Subtitle Generation."
    ))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th("Select"),
          tags$th(class = "title-col", "Title"),
          tags$th(class = "subtitle-col", "Subtitle"),
          tags$th(class = "status-col", "Status"),
          tags$th(class = "notes-col", "Notes")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            select_id <- article_lab_row_input_id("article_lab_thumbnail_package_select", row$subtitle_id[[1]])
            notes_id <- article_lab_row_input_id("article_lab_thumbnail_package_notes", row$subtitle_id[[1]])

            tags$tr(
              `data-selection-group` = "article_lab_thumbnail_packages",
              `data-candidate-id` = row$subtitle_id[[1]],
              tags$td(
                class = "select-cell",
                checkboxInput(select_id, label = NULL, value = FALSE, width = NULL)
              ),
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(class = "subtitle-cell", row$subtitle[[1]]),
              tags$td(class = "status-cell", article_lab_badge(row$normalized_status[[1]])),
              tags$td(class = "notes-cell", textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note"))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows), label = "packages")
  )
}

article_lab_thumbnail_candidate_grid_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No thumbnail preview cards",
      "No thumbnail preview cards are currently waiting for approval in this selection.",
      "Next step: select title/subtitle packages above and generate thumbnail candidates."
    ))
  }

  div(
    class = "thumbnail-preview-grid",
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      select_id <- article_lab_row_input_id("article_lab_thumbnail_candidate_select", row$thumbnail_id[[1]])
      notes_id <- article_lab_row_input_id("article_lab_thumbnail_candidate_notes", row$thumbnail_id[[1]])

      div(
        class = "thumbnail-preview-card",
        `data-selection-group` = "article_lab_thumbnail_candidates",
        `data-candidate-id` = row$thumbnail_id[[1]],
        div(
          class = "thumbnail-preview-topbar",
          checkboxInput(select_id, label = NULL, value = FALSE, width = NULL),
          article_lab_thumbnail_badge(row$thumbnail_status[[1]])
        ),
        div(
          class = "thumbnail-preview-shell",
          div(
            class = "thumbnail-preview-meta medium-preview-card",
            div(class = "preview-kicker", row$thumbnail_label[[1]] %||% "Thumbnail candidate"),
            div(class = "preview-title", row$title[[1]]),
            div(class = "preview-subtitle", row$subtitle[[1]])
          ),
          div(
            class = "thumbnail-preview-image-wrap",
            tags$img(
              class = "thumbnail-preview-image",
              src = row$thumbnail_data_uri[[1]],
              alt = paste("Thumbnail candidate for", row$title[[1]])
            )
          )
        ),
        textInput(notes_id, label = NULL, value = row$notes[[1]] %||% "", width = "100%", placeholder = "Optional note")
      )
    })
  )
}

article_lab_ready_for_outline_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(article_lab_empty_state(
      "No packages ready for Outline",
      "No title/subtitle/thumbnail packages are ready for Outline yet in this selection.",
      "Next step: approve one thumbnail candidate per package."
    ))
  }

  div(
    class = "thumbnail-preview-grid",
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      outline_id <- article_lab_row_value(row, "outline_id")
      thumbnail_id <- article_lab_row_value(row, "thumbnail_id")
      has_outline <- !is.na(outline_id) && nzchar(outline_id)
      outline_status <- article_lab_input_string(article_lab_row_value(row, "outline_status")) %||% "none"
      div(
        class = paste("thumbnail-preview-card approved", if (has_outline) paste0("outline-", outline_status) else "outline-missing"),
        `data-selection-group` = if (has_outline && identical(outline_status, "draft")) "article_lab_outline_candidates" else "article_lab_outline_packages",
        `data-candidate-id` = if (has_outline && identical(outline_status, "draft")) outline_id else thumbnail_id,
        div(
          class = "thumbnail-preview-topbar",
          article_lab_thumbnail_badge("approved"),
          checkboxInput(article_lab_row_input_id("article_lab_outline_packages", thumbnail_id), if (has_outline) "Regenerate outline" else "Generate outline", value = FALSE),
          if (has_outline && identical(outline_status, "draft")) checkboxInput(article_lab_row_input_id("article_lab_outline_candidates", outline_id), "Approve outline", value = FALSE)
        ),
        div(
          class = "thumbnail-preview-shell",
          div(
            class = "thumbnail-preview-meta medium-preview-card",
            div(class = "preview-kicker", article_lab_row_value(row, "thumbnail_label", "Approved thumbnail")),
            div(class = "preview-title", article_lab_row_value(row, "title", "Untitled")),
            div(class = "preview-subtitle", article_lab_row_value(row, "subtitle", ""))
          ),
          div(
            class = "thumbnail-preview-image-wrap",
            tags$img(
              class = "thumbnail-preview-image",
              src = article_lab_row_value(row, "thumbnail_data_uri", ""),
              alt = paste("Approved thumbnail for", article_lab_row_value(row, "title", "untitled article"))
            )
          )
        ),
        if (has_outline) {
          div(
            class = "lab-outline-editor",
            div(class = "lab-status-copy", sprintf("Outline status: %s", article_lab_status_label(outline_status))),
            textAreaInput(
              article_lab_row_input_id("article_lab_outline_text", outline_id),
              "Outline draft",
              value = article_lab_row_value(row, "outline_text", ""),
              width = "100%",
              height = "320px"
            ),
            textInput(
              article_lab_row_input_id("article_lab_outline_notes", outline_id),
              "Review notes",
              value = article_lab_row_value(row, "outline_notes", ""),
              width = "100%"
            )
          )
        } else {
          div(class = "lab-status-copy", "No outline draft yet. Select this package and generate an outline.")
        }
      )
    })
  )
}

article_lab_full_text_source_badge <- function(row, summary_contexts, include_context = TRUE) {
  context <- if (nrow(summary_contexts) == 0) data.frame() else summary_contexts[summary_contexts$batch_id == row$batch_id[[1]], , drop = FALSE]
  pdf_path <- if (nrow(context) > 0) research_resolve_local_pdf_path(context$pdf_local_path[[1]]) else NA_character_
  has_pdf <- !is.na(pdf_path) && file.exists(pdf_path)
  has_summary <- nrow(context) > 0 && !is.na(context$article_summary[[1]]) && nzchar(context$article_summary[[1]])
  if (!isTRUE(include_context)) return(tags$span(class = "lab-chip default", "Source off"))
  if (has_pdf) return(tags$span(class = "lab-chip blue", "PDF available"))
  if (has_summary) return(tags$span(class = "lab-chip green", "Summary fallback"))
  tags$span(class = "lab-chip orange", "No source context")
}

article_lab_full_text_table_ui <- function(rows, packages, summary_contexts, include_context = TRUE) {
  if (nrow(packages) == 0) return(article_lab_empty_state(
    "No outlines ready for Full Text",
    "No approved outlines are ready for Full Article yet in this selection.",
    "Next step: approve an outline from the Outline tab."
  ))
  draft_rows <- rows[!is.na(rows$full_text_draft_id) & nzchar(rows$full_text_draft_id), , drop = FALSE]
  tagList(
    div(
      class = "thumbnail-preview-grid",
      lapply(seq_len(nrow(packages)), function(i) {
        row <- packages[i, , drop = FALSE]
        outline_id <- row$outline_id[[1]]
        package_drafts <- draft_rows[draft_rows$outline_id == outline_id, , drop = FALSE]
        div(
          class = "thumbnail-preview-card approved",
          `data-selection-group` = "article_lab_full_text_packages",
          `data-candidate-id` = outline_id,
          div(
            class = "thumbnail-preview-topbar",
            div(class = "lab-chip-row", article_lab_badge("ready_for_draft"), article_lab_full_text_source_badge(row, summary_contexts, include_context)),
            checkboxInput(article_lab_row_input_id("article_lab_full_text_packages", outline_id), "Generate draft for this outline", value = FALSE)
          ),
          div(
            class = "thumbnail-preview-shell",
            div(
              class = "thumbnail-preview-meta medium-preview-card",
              div(class = "preview-kicker", row$thumbnail_label[[1]] %||% "Approved thumbnail"),
              div(class = "preview-title", row$title[[1]] %||% "Untitled"),
              div(class = "preview-subtitle", row$subtitle[[1]] %||% "")
            ),
            div(
              class = "thumbnail-preview-image-wrap",
              tags$img(class = "thumbnail-preview-image", src = row$thumbnail_data_uri[[1]] %||% "", alt = paste("Approved thumbnail for", row$title[[1]] %||% "untitled article"))
            )
          ),
          div(class = "lab-status-copy", sprintf("Outline ID: %s", outline_id)),
          tags$details(
            tags$summary("Show approved outline"),
            tags$pre(class = "lab-status-copy", row$outline_text[[1]] %||% "")
          ),
          if (nrow(package_drafts) == 0) {
            div(class = "lab-status-copy", "No full article draft yet. Select this outline and generate a draft.")
          } else {
            tagList(lapply(seq_len(nrow(package_drafts)), function(j) {
              draft <- package_drafts[j, , drop = FALSE]
              draft_id <- draft$full_text_draft_id[[1]]
              draft_text_id <- article_lab_row_input_id("article_lab_full_text_draft_text", draft_id)
              editor <- div(
                class = "lab-outline-editor",
                `data-selection-group` = "article_lab_full_text_drafts",
                `data-candidate-id` = draft_id,
                div(
                  class = "thumbnail-preview-topbar",
                  div(class = "lab-chip-row", article_lab_badge(draft$draft_status[[1]] %||% "draft"), tags$span(class = "lab-chip default", draft$source_context_mode[[1]] %||% "none"), tags$span(class = "lab-chip default", draft$draft_model[[1]] %||% "model unknown")),
                  checkboxInput(article_lab_row_input_id("article_lab_full_text_drafts", draft_id), "Select draft", value = FALSE)
                ),
                div(
                  class = "lab-actions",
                  tags$button(type = "button", class = "btn lab-secondary", onclick = sprintf("window.articleLabCopyValueFromElement('%s', this, 'Copied draft');", draft_text_id), "Copy full article draft")
                ),
                textAreaInput(draft_text_id, "Full article draft", value = draft$current_draft_text[[1]] %||% "", width = "100%", height = "720px"),
                textInput(article_lab_row_input_id("article_lab_full_text_draft_notes", draft_id), "Draft notes", value = draft$draft_notes[[1]] %||% "", width = "100%")
              )
              if (j == 1L) editor else tags$details(tags$summary(sprintf("Show older draft variant %s", j)), editor)
            }))
          }
        )
      })
    )
  )
}

article_lab_review_publish_selector_ui <- function(rows, selected_id = NULL) {
  if (nrow(rows) == 0) return(article_lab_empty_state(
    "No approved drafts ready",
    "No approved full article drafts are ready for Review & Publish.",
    "Next step: approve one full article draft from the Full Text tab."
  ))
  labels <- vapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    status <- article_lab_publish_status_label(article_lab_row_value(row, "publish_status", "ready_for_review_publish"))
    sprintf("%s [%s]", article_lab_row_value(row, "title", "Untitled"), status)
  }, character(1))
  choices <- setNames(rows$full_text_draft_id, labels)
  if (is.null(selected_id) || is.na(selected_id) || !(selected_id %in% rows$full_text_draft_id)) selected_id <- rows$full_text_draft_id[[1]]
  selectizeInput("article_lab_review_publish_draft_id", "Approved article", choices = choices, selected = selected_id, width = "100%")
}

article_lab_review_publish_workspace_ui <- function(row, publications) {
  if (nrow(row) == 0) return(article_lab_empty_state("Select an approved draft", "Select an approved full article draft to manage publishing metadata."))
  target <- article_lab_row_value(row, "publishing_target", "Do not publish yet")
  if (is.na(target) || !(target %in% article_lab_publish_target_choices)) target <- "Do not publish yet"
  monetization <- article_lab_row_value(row, "monetization", "Undecided")
  if (is.na(monetization) || !(monetization %in% article_lab_monetization_choices)) monetization <- "Undecided"
  publish_status <- article_lab_row_value(row, "publish_status", "ready_for_review_publish")
  if (is.na(publish_status) || !(publish_status %in% article_lab_publish_status_values)) publish_status <- "ready_for_review_publish"
  publication_choices <- c("No saved publication selected" = "")
  if (nrow(publications) > 0) publication_choices <- c(publication_choices, setNames(publications$publication_id, publications$publication_name))
  selected_publication <- article_lab_row_value(row, "publication_id", "")
  if (is.na(selected_publication) || !(selected_publication %in% unname(publication_choices))) selected_publication <- ""
  markdown_id <- "article_lab_medium_ready_markdown_text"

  tagList(
    div(class = "lab-publish-status-row", article_lab_badge("ready_for_review_publish"), tags$span(class = "lab-chip default", article_lab_publish_status_label(publish_status))),
    div(
      class = "lab-publish-workspace",
      div(
        class = "lab-card lab-publish-preview",
        h2("Approved article package"),
        h3(article_lab_row_value(row, "title", "Untitled")),
        div(class = "page-subtitle", article_lab_row_value(row, "subtitle", "")),
        if (!is.na(article_lab_row_value(row, "thumbnail_data_uri", NA_character_)) && nzchar(article_lab_row_value(row, "thumbnail_data_uri", ""))) {
          div(
            class = "thumbnail-preview-image-wrap",
            tags$img(class = "thumbnail-preview-image", src = article_lab_row_value(row, "thumbnail_data_uri", ""), alt = paste("Featured image for", article_lab_row_value(row, "title", "untitled article")))
          )
        },
        tags$details(
          tags$summary("Read-only approved article preview"),
          tags$pre(class = "lab-status-copy lab-readonly-preview", article_lab_row_value(row, "current_draft_text", ""))
        )
      ),
      div(
        class = "lab-card lab-publish-metadata",
        h2("Publishing metadata"),
        div(
          class = "lab-grid",
          div(class = "lab-field", textInput("article_lab_publish_medium_tags", "Medium tags (max 5, comma or line separated)", value = article_lab_tags_display(article_lab_row_value(row, "medium_tags_json", "")), width = "100%")),
          div(class = "lab-field", selectInput("article_lab_medium_tags_model", "Medium tags model", choices = article_lab_medium_tags_model_choices, selected = article_lab_default_medium_tags_model, width = "100%")),
          div(class = "lab-field", selectInput("article_lab_publishing_target", "Publishing target", choices = article_lab_publish_target_choices, selected = target, width = "100%")),
          div(class = "lab-field", selectInput("article_lab_publish_status", "Publish status", choices = setNames(article_lab_publish_status_values, vapply(article_lab_publish_status_values, article_lab_publish_status_label, character(1))), selected = publish_status, width = "100%")),
          div(class = "lab-field", selectInput("article_lab_monetization", "Monetization", choices = article_lab_monetization_choices, selected = monetization, width = "100%"))
        ),
        conditionalPanel(
          condition = "input.article_lab_publishing_target == 'Submit to Medium publication'",
          div(
            class = "lab-grid",
            div(class = "lab-field", selectInput("article_lab_publication_id", "Medium publication", choices = publication_choices, selected = selected_publication, width = "100%")),
            div(class = "lab-field", textInput("article_lab_new_publication_name", "Add publication name", value = "", width = "100%", placeholder = "Use when missing from the saved list"))
          )
        ),
        div(
          class = "lab-grid",
          div(class = "lab-field", textInput("article_lab_canonical_url", "Canonical URL", value = article_lab_row_value(row, "canonical_url", ""), width = "100%")),
          div(class = "lab-field", textInput("article_lab_published_url", "Published URL", value = article_lab_row_value(row, "published_url", ""), width = "100%")),
          div(class = "lab-field", textInput("article_lab_featured_image_alt_text", "Featured image alt text", value = article_lab_row_value(row, "featured_image_alt_text", ""), width = "100%")),
          div(class = "lab-field", textInput("article_lab_image_credit_source", "Image credit/source", value = article_lab_row_value(row, "image_credit_source", ""), width = "100%"))
        ),
        div(class = "lab-field", textAreaInput("article_lab_publish_notes", "Notes", value = article_lab_row_value(row, "publish_notes", ""), width = "100%", height = "90px")),
        tags$details(
          class = "lab-secondary-details",
          tags$summary("Medium tag generation prompt"),
          div(class = "lab-field", textAreaInput("article_lab_medium_tags_prompt", "Medium tags API prompt", value = article_lab_default_medium_tags_prompt, width = "100%", height = "130px")),
          uiOutput("article_lab_medium_tags_effective_prompt")
        ),
        article_lab_action_bar(
          actionButton("article_lab_save_publish_settings", "Save publish settings", class = "lab-primary"),
          actionButton("article_lab_generate_medium_tags", "Generate Medium tags", class = "lab-secondary"),
          tags$button(type = "button", class = "btn lab-secondary", onclick = sprintf("window.articleLabCopyTextFromElement('%s', this, 'Copied article');", markdown_id), "Copy Medium-ready article"),
          downloadButton("article_lab_export_markdown", "Export Markdown", class = "lab-secondary"),
          actionButton("article_lab_refresh_publish", "Refresh", class = "lab-secondary")
        ),
        tags$details(
          class = "lab-secondary-details",
          tags$summary("Medium-ready Markdown preview"),
          tags$pre(id = markdown_id, class = "lab-status-copy lab-readonly-preview", article_lab_medium_ready_markdown(row, row))
        )
      )
    )
  )
}

article_lab_ready_for_thumbnail_table_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "empty-state", "No title packages are ready for Thumbnails yet in this selection."))
  }

  tagList(
    div(
      class = "lab-table-wrap",
      tags$table(
        class = "lab-table",
        tags$thead(tags$tr(
          tags$th(class = "title-col", "Title"),
          tags$th(class = "subtitle-col", "Approved subtitles"),
          tags$th(class = "status-col", "Status")
        )),
        tags$tbody(
          lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, , drop = FALSE]
            subtitle_lines <- clean_text(strsplit(row$approved_subtitles[[1]] %||% "", "\n", fixed = TRUE)[[1]])
            tags$tr(
              tags$td(class = "title-cell", row$title[[1]]),
              tags$td(
                class = "subtitle-cell",
                div(
                  class = "approved-subtitle-list",
                  lapply(subtitle_lines[!is.na(subtitle_lines)], function(entry) div(class = "approved-subtitle-item", entry))
                )
              ),
              tags$td(class = "status-cell", article_lab_badge(row$status[[1]]))
            )
          })
        )
      )
    ),
    article_lab_table_footer(nrow(rows), label = "title packages")
  )
}
