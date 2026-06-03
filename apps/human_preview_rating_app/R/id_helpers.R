article_lab_batch_id <- function() {
  paste0("alb_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(99999L, 1))
}

article_lab_candidate_id <- function(batch_id, index) {
  paste0("alc_", batch_id, "_", sprintf("%02d", as.integer(index)))
}

article_lab_outline_id <- function(thumbnail_id) {
  paste0("alo_", gsub("[^A-Za-z0-9]+", "_", thumbnail_id), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(99999L, 1))
}

article_lab_full_text_draft_id <- function(outline_id) {
  paste0("alf_", gsub("[^A-Za-z0-9]+", "_", outline_id %||% "outline"), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(99999L, 1))
}

article_lab_full_text_revision_id <- function(full_text_draft_id) {
  paste0("alfr_", gsub("[^A-Za-z0-9]+", "_", full_text_draft_id %||% "draft"), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(99999L, 1))
}

article_lab_publish_settings_id <- function(full_text_draft_id) {
  paste0("alps_", gsub("[^A-Za-z0-9]+", "_", full_text_draft_id %||% "draft"))
}

article_lab_publication_id <- function(publication_name) {
  key <- tolower(gsub("[^A-Za-z0-9]+", "_", article_lab_input_string(publication_name) %||% "publication"))
  paste0("alpub_", gsub("(^_+|_+$)", "", key), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample.int(99999L, 1))
}

article_lab_score_id <- function(candidate_id) {
  paste0("als_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", gsub("[^A-Za-z0-9]+", "_", candidate_id))
}

article_lab_subtitle_id <- function(candidate_id, index = 1L) {
  paste0(
    "alsub_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    gsub("[^A-Za-z0-9]+", "_", candidate_id),
    "_",
    sprintf("%02d", suppressWarnings(as.integer(index)) %||% 1L),
    "_",
    sample.int(99999L, 1)
  )
}

article_lab_thumbnail_id <- function(subtitle_id, index = 1L) {
  paste0(
    "alth_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    gsub("[^A-Za-z0-9]+", "_", subtitle_id),
    "_",
    sprintf("%02d", suppressWarnings(as.integer(index)) %||% 1L),
    "_",
    sample.int(99999L, 1)
  )
}
