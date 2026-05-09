required_packages <- c("jsonlite")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("jsonlite"))',
    call. = FALSE
  )
}

library(jsonlite)

source(file.path("scripts", "medium_tag_import_helpers.R"))

tag_html_schema_version <- 1L

read_html_text <- function(input_path) {
  paste(readLines(input_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

html_decode <- function(value) {
  value <- clean_text(value)

  if (is.na(value)) {
    return(NA_character_)
  }

  value <- gsub("&nbsp;", " ", value, fixed = TRUE)
  value <- gsub("&amp;", "&", value, fixed = TRUE)
  value <- gsub("&quot;", "\"", value, fixed = TRUE)
  value <- gsub("&#39;", "'", value, fixed = TRUE)
  value <- gsub("&apos;", "'", value, fixed = TRUE)
  value <- gsub("&lt;", "<", value, fixed = TRUE)
  value <- gsub("&gt;", ">", value, fixed = TRUE)

  decode_numeric <- function(text, pattern, base) {
    matches <- gregexpr(pattern, text, perl = TRUE)[[1]]

    if (identical(matches[1], -1L)) {
      return(text)
    }

    pieces <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]

    for (piece in pieces) {
      number_text <- sub("^&#x?", "", sub(";$", "", piece), ignore.case = TRUE)
      codepoint <- suppressWarnings(strtoi(number_text, base = base))

      if (!is.na(codepoint)) {
        text <- sub(piece, intToUtf8(codepoint), text, fixed = TRUE)
      }
    }

    text
  }

  value <- decode_numeric(value, "&#x[0-9A-Fa-f]+;", 16L)
  value <- decode_numeric(value, "&#[0-9]+;", 10L)
  clean_text(value)
}

strip_tags <- function(value) {
  html_decode(gsub("<[^>]+>", " ", value, perl = TRUE))
}

extract_first_match <- function(text, pattern) {
  match <- regexec(pattern, text, perl = TRUE, ignore.case = TRUE)
  parts <- regmatches(text, match)[[1]]

  if (length(parts) < 2) {
    return(NA_character_)
  }

  html_decode(parts[2])
}

extract_all_matches <- function(text, pattern) {
  matches <- gregexpr(pattern, text, perl = TRUE, ignore.case = TRUE)
  raw <- regmatches(text, matches)[[1]]

  if (length(raw) == 1 && identical(raw[1], character(0))) {
    return(character())
  }

  raw
}

split_post_preview_articles <- function(html) {
  starts <- gregexpr("<article[^>]+data-testid=[\"']post-preview[\"'][^>]*>", html, perl = TRUE, ignore.case = TRUE)[[1]]

  if (identical(starts[1], -1L)) {
    return(character())
  }

  articles <- character()

  for (index in seq_along(starts)) {
    start <- starts[[index]]
    next_start <- if (index < length(starts)) starts[[index + 1L]] else nchar(html) + 1L
    remainder <- substring(html, start, nchar(html))
    end_match <- regexpr("</article>", remainder, perl = TRUE, ignore.case = TRUE)

    if (!identical(end_match[1], -1L) && start + end_match[1] - 1L < next_start) {
      articles <- c(articles, substring(remainder, 1L, end_match[1] + attr(end_match, "match.length") - 1L))
    } else {
      articles <- c(articles, substring(html, start, next_start - 1L))
    }
  }

  articles
}

parse_tag_page_info <- function(html, input_path) {
  tag_url <- extract_first_match(html, "<meta[^>]+name=[\"']apple-itunes-app[\"'][^>]+content=[\"'][^\"']*app-argument=([^,\"']+)")
  tag_url <- if (!is.na(tag_url)) sub("^app-argument=", "", tag_url) else NA_character_

  if (is.na(tag_url)) {
    tag_url <- extract_first_match(html, "<link[^>]+rel=[\"']canonical[\"'][^>]+href=[\"']([^\"']+)")
  }

  title <- extract_first_match(html, "<title[^>]*>([\\s\\S]*?)</title>")
  heading <- extract_first_match(html, "<h2[^>]*>\\s*Recommended stories in &quot;([^&]+)&quot;\\s*</h2>")

  if (is.na(heading)) {
    heading <- extract_first_match(html, "<h2[^>]*>\\s*Recommended stories in \"([^\"]+)\"\\s*</h2>")
  }

  tag_slug <- NA_character_

  if (!is.na(tag_url)) {
    path_match <- regexec("/tag/([^/?#]+)(?:/([^/?#]+))?", tag_url, perl = TRUE, ignore.case = TRUE)
    path_parts <- regmatches(tag_url, path_match)[[1]]

    if (length(path_parts) >= 2) {
      tag_slug <- utils::URLdecode(path_parts[2])
    }
  }

  if (is.na(tag_slug) && !is.na(heading)) {
    tag_slug <- tolower(gsub("[^a-z0-9]+", "-", heading))
    tag_slug <- gsub("(^-|-$)", "", tag_slug)
  }

  if (is.na(tag_url) && !is.na(tag_slug)) {
    tag_url <- paste0("https://medium.com/tag/", tag_slug, "/recommended")
  }

  list(
    tag_slug = clean_text(tag_slug),
    page_variant = infer_page_variant_from_url(tag_url),
    tag_url = normalize_medium_url(tag_url),
    page_title = first_non_missing_local(title, paste("Medium tag page", input_path))
  )
}

first_non_missing_local <- function(primary_value, fallback_value) {
  if (!is_missing_text(primary_value)) {
    return(clean_text(primary_value))
  }

  clean_text(fallback_value)
}

parse_source_param <- function(url) {
  value <- clean_text(url)

  if (is.na(value) || !grepl("[?&]source=", value)) {
    return(NA_character_)
  }

  source <- sub("^.*[?&]source=([^&#]+).*$", "\\1", value, perl = TRUE)
  clean_text(utils::URLdecode(source))
}

parse_recommendation_source <- function(source) {
  empty <- list(
    recommendation_source = clean_text(source),
    recommendation_surface = NA_character_,
    recommendation_tag_slug = NA_character_,
    recommendation_position = NA_integer_,
    recommendation_result_set_size = NA_integer_
  )

  if (is_missing_text(source)) {
    return(empty)
  }

  match <- regexec("^(.+?)------([a-z0-9_-]+)---([0-9]+)-([0-9]+)", source, perl = TRUE, ignore.case = TRUE)
  parts <- regmatches(source, match)[[1]]

  if (length(parts) < 5) {
    return(empty)
  }

  list(
    recommendation_source = clean_text(source),
    recommendation_surface = clean_text(parts[2]),
    recommendation_tag_slug = clean_text(parts[3]),
    recommendation_position = as.integer(parts[4]),
    recommendation_result_set_size = as.integer(parts[5])
  )
}

parse_compact_integer <- function(value) {
  value <- clean_text(value)

  if (is.na(value)) {
    return(NA_integer_)
  }

  compact <- toupper(gsub("\\s+", "", gsub(",", "", value)))
  match <- regexec("^([0-9]+(?:\\.[0-9]+)?)([KM])?$", compact, perl = TRUE)
  parts <- regmatches(compact, match)[[1]]

  if (length(parts) < 2) {
    return(NA_integer_)
  }

  multiplier <- if (length(parts) >= 3 && identical(parts[3], "K")) {
    1000
  } else if (length(parts) >= 3 && identical(parts[3], "M")) {
    1000000
  } else {
    1
  }

  as.integer(round(as.numeric(parts[2]) * multiplier))
}

extract_icon_count <- function(article_html, icon_label) {
  pattern <- paste0(
    "<desc[^>]*>[^<]*",
    icon_label,
    "[^<]*</desc>[\\s\\S]{0,1800}?<span[^>]*>([0-9][0-9,.]*\\s*[kKmM]?)</span>"
  )
  raw <- extract_first_match(article_html, pattern)
  parse_compact_integer(raw)
}

extract_primary_article_url <- function(article_html) {
  data_href <- extract_first_match(article_html, "data-href=[\"']([^\"']*[A-Fa-f0-9]{12})(?:[\"'?#]|$)")

  if (!is.na(data_href)) {
    return(normalize_medium_url(data_href))
  }

  hrefs <- extract_all_matches(article_html, "href=[\"'][^\"']*[A-Fa-f0-9]{12}[^\"']*[\"']")
  hrefs <- gsub("^href=[\"']|[\"']$", "", hrefs)
  hrefs <- hrefs[!grepl("/m/signin|/_/bookmark|/_/vote", hrefs, ignore.case = TRUE)]

  if (length(hrefs) == 0) {
    return(NA_character_)
  }

  normalize_medium_url(hrefs[1])
}

extract_recommendation_href <- function(article_html) {
  hrefs <- extract_all_matches(article_html, "href=[\"'][^\"']*source=tag_recommended_stories_page[^\"']*[\"']")
  hrefs <- gsub("^href=[\"']|[\"']$", "", hrefs)
  article_hrefs <- hrefs[grepl("[A-Fa-f0-9]{12}", hrefs) & !grepl("/m/signin|/_/bookmark|/_/vote", hrefs, ignore.case = TRUE)]

  if (length(article_hrefs)) {
    return(html_decode(article_hrefs[1]))
  }

  if (length(hrefs)) {
    return(html_decode(hrefs[1]))
  }

  NA_character_
}

extract_title <- function(article_html) {
  title <- extract_first_match(article_html, "<h2[^>]*>([\\s\\S]*?)</h2>")
  strip_tags(title)
}

extract_subtitle <- function(article_html) {
  subtitle <- extract_first_match(article_html, "<h3[^>]*>([\\s\\S]*?)</h3>")
  strip_tags(subtitle)
}

extract_link_text_by_href_pattern <- function(article_html, href_pattern) {
  pattern <- paste0("<a[^>]+href=[\"'][^\"']*", href_pattern, "[^\"']*[\"'][^>]*>([\\s\\S]*?)</a>")
  value <- extract_first_match(article_html, pattern)
  strip_tags(value)
}

extract_href_by_text <- function(article_html, text_value) {
  if (is_missing_text(text_value)) {
    return(NA_character_)
  }

  escaped_text <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", text_value, perl = TRUE)
  pattern <- paste0("<a[^>]+href=[\"']([^\"']+)[\"'][^>]*>[\\s\\S]{0,500}?", escaped_text, "[\\s\\S]{0,500}?</a>")
  normalize_medium_url(extract_first_match(article_html, pattern))
}

extract_author_publication <- function(article_html) {
  author_name <- extract_link_text_by_href_pattern(article_html, "/@|medium\\.com/@")
  author_url <- extract_href_by_text(article_html, author_name)

  publication_name <- NA_character_
  publication_url <- NA_character_

  in_match <- regexec(
    "In\\s*</p>[\\s\\S]{0,1200}?<a[^>]+href=[\"']([^\"']+)[\"'][^>]*>[\\s\\S]*?<p[^>]*>([\\s\\S]*?)</p>[\\s\\S]{0,1200}?by\\s*</p>",
    article_html,
    perl = TRUE,
    ignore.case = TRUE
  )
  in_parts <- regmatches(article_html, in_match)[[1]]

  if (length(in_parts) >= 3) {
    publication_url <- normalize_medium_url(html_decode(in_parts[2]))
    publication_name <- strip_tags(in_parts[3])
  }

  if (is_missing_text(author_name)) {
    by_match <- regexec("by\\s*</p>[\\s\\S]{0,1200}?<a[^>]+href=[\"']([^\"']+)[\"'][^>]*>[\\s\\S]*?<p[^>]*>([\\s\\S]*?)</p>", article_html, perl = TRUE, ignore.case = TRUE)
    by_parts <- regmatches(article_html, by_match)[[1]]

    if (length(by_parts) >= 3) {
      author_url <- normalize_medium_url(html_decode(by_parts[2]))
      author_name <- strip_tags(by_parts[3])
    }
  }

  list(
    author_name = clean_text(author_name),
    author_url = normalize_medium_url(author_url),
    publication_name = clean_text(publication_name),
    publication_url = normalize_medium_url(publication_url)
  )
}

extract_published_label <- function(article_html) {
  text <- strip_tags(article_html)
  match <- regmatches(
    text,
    regexpr("\\b(?:just now|today|[0-9]+\\s*(?:m|min|mins|minute|minutes|h|hr|hrs|hour|hours)\\s+ago|(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\.?\\s+[0-9]{1,2}(?:,\\s*[0-9]{4})?)\\b", text, perl = TRUE, ignore.case = TRUE)
  )

  if (length(match) == 0 || identical(match, character(0))) {
    return(NA_character_)
  }

  clean_text(match[1])
}

month_index_from_name <- function(value) {
  match(substr(tolower(clean_text(value)), 1L, 3L), tolower(month.abb)) - 1L
}

date_string_from_parts <- function(year, month_index, day) {
  sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month_index) + 1L, as.integer(day))
}

infer_published_date_from_label <- function(label, captured_at) {
  label <- clean_text(label)

  if (is.na(label)) {
    return(list(date = NA_character_, inferred = FALSE))
  }

  lower <- tolower(label)

  if (grepl("^(just now|today|[0-9]+\\s*(m|min|mins|minute|minutes|h|hr|hrs|hour|hours)\\s+ago)$", lower, perl = TRUE)) {
    return(list(date = substr(captured_at, 1L, 10L), inferred = TRUE))
  }

  match <- regexec("^([A-Za-z]{3,9})\\.?\\s+([0-9]{1,2})(?:,\\s*([0-9]{4}))?$", label, perl = TRUE)
  parts <- regmatches(label, match)[[1]]

  if (length(parts) < 3) {
    return(list(date = NA_character_, inferred = FALSE))
  }

  month_index <- month_index_from_name(parts[2])
  day <- as.integer(parts[3])

  if (is.na(month_index) || month_index < 0L || is.na(day)) {
    return(list(date = NA_character_, inferred = FALSE))
  }

  if (length(parts) >= 4 && !is.na(parts[4]) && nzchar(parts[4])) {
    return(list(date = date_string_from_parts(as.integer(parts[4]), month_index, day), inferred = FALSE))
  }

  captured_date <- as.Date(substr(captured_at, 1L, 10L))
  year <- as.integer(format(captured_date, "%Y"))
  candidate <- as.Date(date_string_from_parts(year, month_index, day))

  if (!is.na(candidate) && candidate > captured_date + 1L) {
    year <- year - 1L
  }

  list(date = date_string_from_parts(year, month_index, day), inferred = TRUE)
}

timestamp_from_millis <- function(value) {
  value <- suppressWarnings(as.numeric(value))

  if (is.na(value) || value <= 0) {
    return(NA_character_)
  }

  format(as.POSIXct(value / 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

collect_apollo_post_map <- function(html) {
  start <- regexpr("window\\.__APOLLO_STATE__\\s*=", html, perl = TRUE)

  if (identical(start[1], -1L)) {
    return(list())
  }

  fragment <- substring(html, start[1] + attr(start, "match.length"), nchar(html))
  end <- regexpr("</script>", fragment, perl = TRUE, ignore.case = TRUE)

  if (identical(end[1], -1L)) {
    return(list())
  }

  raw_json <- trimws(substring(fragment, 1L, end[1] - 1L))
  raw_json <- sub(";\\s*$", "", raw_json)
  parsed <- tryCatch(jsonlite::fromJSON(raw_json, simplifyVector = FALSE), error = function(error) NULL)

  if (is.null(parsed)) {
    return(list())
  }

  records <- list()

  for (key in names(parsed)) {
    value <- parsed[[key]]

    if (!grepl("^Post:", key) || !is.list(value)) {
      next
    }

    post_id <- tolower(sub("^Post:", "", key))
    creator_ref <- clean_text(value$creator$`__ref`)
    collection_ref <- clean_text(value$collection$`__ref`)
    creator <- if (!is.na(creator_ref) && creator_ref %in% names(parsed)) parsed[[creator_ref]] else list()
    collection <- if (!is.na(collection_ref) && collection_ref %in% names(parsed)) parsed[[collection_ref]] else list()
    url <- normalize_medium_url(value$mediumUrl)
    image_ref <- clean_text(value$previewImage$`__ref`)
    image <- if (!is.na(image_ref) && image_ref %in% names(parsed)) parsed[[image_ref]] else list()

    tag_names <- character()
    if (is.list(value$tags)) {
      for (tag_ref in value$tags) {
        ref <- clean_text(tag_ref$`__ref`)

        if (!is.na(ref) && ref %in% names(parsed)) {
          tag <- parsed[[ref]]
          tag_names <- c(tag_names, clean_text(tag$normalizedTagSlug))
        }
      }
    }

    record <- list(
      medium_post_id = post_id,
      article_url = url,
      title = clean_text(value$title),
      subtitle = clean_text(value$extendedPreviewContent$subtitle),
      author_name = clean_text(creator$name),
      author_username = clean_text(creator$username),
      author_medium_user_id = clean_text(creator$id),
      publication_name = clean_text(collection$name),
      publication_id = clean_text(collection$id),
      publication_slug = clean_text(collection$slug),
      publication_domain = clean_text(collection$domain),
      publication_subscriber_count = integer_from_json(collection$subscriberCount),
      publication_status = if (!is.na(collection_ref)) "publication" else "self_published_known",
      published_at = timestamp_from_millis(value$firstPublishedAt),
      updated_at = timestamp_from_millis(value$latestPublishedAt),
      read_time_minutes = if (is.null(value$readingTime)) NA_real_ else round(as.numeric(value$readingTime), 1),
      article_tags = tag_names[!is.na(tag_names)],
      claps = integer_from_json(value$clapCount),
      responses = integer_from_json(value$postResponses$count),
      is_member_only = isTRUE(value$isLocked) || identical(toupper(clean_text(value$visibility)), "LOCKED"),
      thumbnail_url = if (!is.na(clean_text(image$id))) paste0("https://miro.medium.com/", clean_text(image$id)) else NA_character_,
      thumbnail_alt = clean_text(image$alt)
    )

    records[[post_id]] <- record

    if (!is.na(url)) {
      records[[url]] <- record
    }
  }

  records
}

find_embedded <- function(article_url, post_id, embedded_map) {
  if (!is.na(post_id) && post_id %in% names(embedded_map)) {
    return(embedded_map[[post_id]])
  }

  if (!is.na(article_url) && article_url %in% names(embedded_map)) {
    return(embedded_map[[article_url]])
  }

  list()
}

parse_card <- function(article_html, index, page_info, captured_at, embedded_map) {
  article_url <- extract_primary_article_url(article_html)
  post_id <- extract_medium_post_id(article_url)
  embedded <- find_embedded(article_url, post_id, embedded_map)
  recommendation_href <- extract_recommendation_href(article_html)
  recommendation <- parse_recommendation_source(parse_source_param(recommendation_href))

  if (is.na(recommendation$recommendation_surface)) {
    recommendation$recommendation_surface <- "tag_recommended_stories_page"
    recommendation$recommendation_tag_slug <- page_info$tag_slug
    recommendation$recommendation_position <- index - 1L
  }

  people <- extract_author_publication(article_html)
  title <- first_non_missing_local(extract_title(article_html), embedded$title)
  subtitle <- first_non_missing_local(extract_subtitle(article_html), embedded$subtitle)
  published_label <- extract_published_label(article_html)
  inferred_date <- infer_published_date_from_label(published_label, captured_at)
  claps <- extract_icon_count(article_html, "clap")
  responses <- extract_icon_count(article_html, "response")

  if (is.na(claps) && !is.na(integer_from_json(embedded$claps))) {
    claps <- integer_from_json(embedded$claps)
  }
  if (is.na(responses) && !is.na(integer_from_json(embedded$responses))) {
    responses <- integer_from_json(embedded$responses)
  }

  list(
    position = index,
    section = "Recommended stories",
    article_url = article_url,
    medium_post_id = post_id,
    title = title,
    subtitle = subtitle,
    author_name = first_non_missing_local(people$author_name, embedded$author_name),
    author_url = people$author_url,
    author_username = clean_text(embedded$author_username),
    author_medium_user_id = clean_text(embedded$author_medium_user_id),
    publication_name = first_non_missing_local(people$publication_name, embedded$publication_name),
    publication_url = people$publication_url,
    publication_id = clean_text(embedded$publication_id),
    publication_slug = clean_text(embedded$publication_slug),
    publication_domain = clean_text(embedded$publication_domain),
    publication_subscriber_count = integer_from_json(embedded$publication_subscriber_count),
    publication_status = {
      publication_name <- first_non_missing_local(people$publication_name, embedded$publication_name)
      embedded_status <- clean_text(embedded$publication_status)
      author_name <- first_non_missing_local(people$author_name, embedded$author_name)
      if (!is.na(publication_name)) "publication" else if (!is.na(embedded_status)) embedded_status else if (!is.na(author_name)) "self_published_assumed" else "unknown"
    },
    published_label = published_label,
    published_at = clean_text(embedded$published_at),
    published_date_inferred = if (is.na(clean_text(embedded$published_at))) inferred_date$date else NA_character_,
    published_date_inferred_from = if (isTRUE(inferred_date$inferred)) published_label else NA_character_,
    published_at_inferred = NA_character_,
    published_at_inferred_precision = NA_character_,
    updated_at = clean_text(embedded$updated_at),
    read_time_minutes = real_from_json(embedded$read_time_minutes),
    article_tags = if (is.null(embedded$article_tags)) list() else embedded$article_tags,
    claps = if (is.na(claps)) 0L else claps,
    responses = if (is.na(responses)) 0L else responses,
    is_member_only = grepl("Member-only story", strip_tags(article_html), fixed = TRUE) || isTRUE(embedded$is_member_only),
    thumbnail_url = first_non_missing_local(extract_first_match(article_html, "<img[^>]+(?:src|currentSrc)=[\"']([^\"']+)"), embedded$thumbnail_url),
    thumbnail_alt = first_non_missing_local(extract_first_match(article_html, "<img[^>]+alt=[\"']([^\"']*)"), embedded$thumbnail_alt),
    recommendation_source = recommendation$recommendation_source,
    recommendation_surface = recommendation$recommendation_surface,
    recommendation_tag_slug = recommendation$recommendation_tag_slug,
    recommendation_position = recommendation$recommendation_position,
    recommendation_result_set_size = recommendation$recommendation_result_set_size
  )
}

medium_tag_html_to_payload <- function(input_path) {
  html <- read_html_text(input_path)
  page_info <- parse_tag_page_info(html, input_path)

  if (is.na(page_info$tag_slug)) {
    stop("Could not detect a Medium tag slug from this saved HTML file.", call. = FALSE)
  }

  articles <- split_post_preview_articles(html)

  if (!length(articles)) {
    stop("No Medium post-preview cards were found in this saved HTML file.", call. = FALSE)
  }

  captured_at <- current_timestamp()
  embedded_map <- collect_apollo_post_map(html)
  cards <- list()

  for (index in seq_along(articles)) {
    card <- parse_card(articles[[index]], index, page_info, captured_at, embedded_map)

    if (is.na(card$article_url) || is.na(card$title)) {
      next
    }

    cards[[length(cards) + 1L]] <- card
  }

  list(
    source_type = "medium_tag_page_bookmarklet",
    schema_version = tag_html_schema_version,
    tag_slug = page_info$tag_slug,
    page_variant = page_info$page_variant,
    tag_url = page_info$tag_url,
    source_url = page_info$tag_url,
    captured_at = captured_at,
    page_title = page_info$page_title,
    source_html_path = normalizePath(input_path, winslash = "/", mustWork = FALSE),
    cards = cards
  )
}
