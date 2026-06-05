article_lab_candidate_status_values <- c(
  "generated",
  "disqualified",
  "ready_for_api_scoring",
  "api_pending",
  "api_scored",
  "approved_for_subtitle",
  "ready_for_thumbnail",
  "ready_for_outline",
  "ready_for_draft",
  "ready_for_review_publish",
  "archived",
  "rejected"
)
article_lab_candidate_status_labels <- c(
  generated = "New",
  disqualified = "Disqualified",
  ready_for_api_scoring = "API queue",
  api_pending = "API scoring",
  api_scored = "API scored",
  approved_for_subtitle = "Approved",
  ready_for_thumbnail = "Ready for thumbnail",
  ready_for_outline = "Ready for outline",
  ready_for_draft = "Ready for draft",
  ready_for_review_publish = "Ready for review",
  archived = "Archived",
  rejected = "Rejected",
  draft = "Draft"
)
article_lab_subtitle_status_labels <- c(
  generated = "Generated",
  approved = "Approved",
  rejected = "Rejected"
)
article_lab_thumbnail_status_labels <- c(
  generated = "Generated",
  approved = "Approved",
  rejected = "Rejected"
)
article_lab_publish_status_values <- c(
  "ready_for_review_publish",
  "ready_to_publish",
  "submitted",
  "published",
  "needs_changes",
  "rejected",
  "archived"
)
article_lab_publish_status_labels <- c(
  ready_for_review_publish = "Ready for Review & Publish",
  ready_to_publish = "Ready to publish",
  submitted = "Submitted",
  published = "Published",
  needs_changes = "Needs changes",
  rejected = "Rejected",
  archived = "Archived"
)

article_lab_normalize_candidate_status <- function(status, ready_for_human_rating = 0, promoted = 0, archived = 0) {
  status_value <- clean_text(status)
  status_value <- if (length(status_value) == 0 || is.na(status_value[[1]])) NA_character_ else status_value[[1]]
  ready_value <- suppressWarnings(as.integer(ready_for_human_rating))
  promoted_value <- suppressWarnings(as.integer(promoted))
  archived_value <- suppressWarnings(as.integer(archived))

  if (!is.na(archived_value) && archived_value == 1L) return("archived")
  if (!is.na(promoted_value) && promoted_value == 1L) return("approved_for_subtitle")
  if (!is.na(status_value) && identical(status_value, "draft")) return("draft")
  if (!is.na(status_value) && status_value %in% c("promoted", "approved", "approved_for_subtitle")) return("approved_for_subtitle")
  if (!is.na(status_value) && identical(status_value, "ready_for_human_rating")) return("api_scored")
  if (!is.na(ready_value) && ready_value == 1L) return("api_scored")
  if (!is.na(status_value) && status_value %in% article_lab_candidate_status_values) return(status_value)
  "generated"
}

article_lab_status_label <- function(status) {
  status <- article_lab_input_string(status)
  if (is.null(status)) return("Unknown")
  if (!(status %in% names(article_lab_candidate_status_labels))) return(status)
  label <- article_lab_candidate_status_labels[[status]]
  if (is.null(label) || is.na(label) || !nzchar(label)) status else label
}

article_lab_subtitle_status_label <- function(status) {
  status <- article_lab_input_string(status)
  if (is.null(status)) return("Unknown")
  if (!(status %in% names(article_lab_subtitle_status_labels))) return(status)
  label <- article_lab_subtitle_status_labels[[status]]
  if (is.null(label) || is.na(label) || !nzchar(label)) status else label
}

article_lab_thumbnail_status_label <- function(status) {
  status <- article_lab_input_string(status)
  if (is.null(status)) return("Unknown")
  if (!(status %in% names(article_lab_thumbnail_status_labels))) return(status)
  label <- article_lab_thumbnail_status_labels[[status]]
  if (is.null(label) || is.na(label) || !nzchar(label)) status else label
}

article_lab_publish_status_label <- function(status) {
  status <- article_lab_input_string(status)
  if (is.null(status)) return("Unknown")
  if (!(status %in% names(article_lab_publish_status_labels))) return(status)
  label <- article_lab_publish_status_labels[[status]]
  if (is.null(label) || is.na(label) || !nzchar(label)) status else label
}

article_lab_status_choices <- function(values) {
  setNames(values, vapply(values, article_lab_status_label, character(1)))
}


article_lab_normalize_candidate_rows <- function(rows) {
  if (nrow(rows) == 0) return(rows)
  ready_column <- if ("ready_for_human_rating" %in% names(rows)) rows$ready_for_human_rating else rep(0L, nrow(rows))
  promoted_column <- if ("promoted" %in% names(rows)) rows$promoted else rep(0L, nrow(rows))
  archived_column <- if ("archived" %in% names(rows)) rows$archived else rep(0L, nrow(rows))
  rows$normalized_status <- vapply(seq_len(nrow(rows)), function(i) {
    article_lab_normalize_candidate_status(
      status = rows$status[[i]],
      ready_for_human_rating = ready_column[[i]],
      promoted = promoted_column[[i]],
      archived = archived_column[[i]]
    )
  }, character(1))
  rows$status_label <- vapply(rows$normalized_status, article_lab_status_label, character(1))
  rows
}
