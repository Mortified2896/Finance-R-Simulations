article_lab_title_max_chars <- 140L
article_lab_title_preferred_min_chars <- 40L
article_lab_title_preferred_max_chars <- 75L
article_lab_title_mobile_safe_chars <- 45L
article_lab_title_good_chars <- 60L
article_lab_title_long_allowed_chars <- 90L
article_lab_score_fields <- c(
  "clarity",
  "curiosity",
  "specificity",
  "beginner_appeal",
  "credibility",
  "emotional_pull",
  "promise_strength",
  "trust_risk",
  "medium_clap_potential",
  "medium_comment_potential",
  "overall_article_potential"
)

article_lab_title_length_flag <- function(char_count) {
  count <- suppressWarnings(as.integer(char_count))
  ifelse(
    is.na(count),
    NA_character_,
    ifelse(
      count <= article_lab_title_mobile_safe_chars,
      "mobile_safe",
      ifelse(
        count <= article_lab_title_good_chars,
        "good",
        ifelse(
          count <= article_lab_title_long_allowed_chars,
          "long_but_allowed",
          ifelse(count <= article_lab_title_max_chars, "very_long_but_allowed", "too_long")
        )
      )
    )
  )
}

article_lab_normalize_score <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  normalized <- ((value - 1) / 4) * 100
  normalized[is.na(value)] <- NA_real_
  pmax(0, pmin(100, normalized))
}

article_lab_combined_title_score <- function(curiosity, emotional_pull, medium_comment_potential, overall_article_potential, trust_risk, title_char_count = NA_integer_) {
  curiosity_norm <- article_lab_normalize_score(curiosity)
  emotional_norm <- article_lab_normalize_score(emotional_pull)
  comment_norm <- article_lab_normalize_score(medium_comment_potential)
  overall_norm <- article_lab_normalize_score(overall_article_potential)
  trust_norm <- article_lab_normalize_score(trust_risk)
  char_count <- suppressWarnings(as.integer(title_char_count))
  trust_penalty <- ifelse(is.na(trust_norm), 0, 0.18 * trust_norm)
  length_penalty <- ifelse(
    is.na(char_count),
    0,
    ifelse(
      char_count > article_lab_title_max_chars,
      35,
      ifelse(char_count > article_lab_title_long_allowed_chars, 20, ifelse(char_count > article_lab_title_preferred_max_chars, 8, 0))
    )
  )
  raw_score <- (0.25 * curiosity_norm) +
    (0.30 * emotional_norm) +
    (0.25 * comment_norm) +
    (0.20 * overall_norm) -
    trust_penalty -
    length_penalty
  round(pmax(0, pmin(100, raw_score)), 1)
}
