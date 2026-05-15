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

read_input_file_text <- function(input_path) {
  paste(readLines(input_path, warn = FALSE), collapse = "\n")
}

clean_text <- function(x) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) {
    return(NA_character_)
  }

  value <- trimws(as.character(x)[1])

  if (identical(value, "")) {
    return(NA_character_)
  }

  value
}

run_import_script <- function(script_path, input_path) {
  status <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = shQuote(c(script_path, input_path))
  )

  if (!identical(status, 0L)) {
    quit(status = status)
  }
}

looks_like_stats_html <- function(input_text) {
  grepl("medium\\.com/me/stats", input_text, ignore.case = TRUE) ||
    (grepl(">Story<", input_text, fixed = TRUE) &&
      grepl("Presentations", input_text, ignore.case = TRUE) &&
      grepl("Views", input_text, ignore.case = TRUE) &&
      grepl("Reads", input_text, ignore.case = TRUE))
}

looks_like_tag_page_html <- function(input_text) {
  grepl("data-testid=[\"']post-preview[\"']", input_text, ignore.case = TRUE) &&
    grepl("tag_recommended_stories_page|/tag/|medium\\.com/[^/\"']+/(latest|archive)|rel=[\"']canonical[\"'][^>]+href=[\"']https://medium\\.com/[^/\"']+", input_text, ignore.case = TRUE)
}

detect_json_source_type <- function(input_text) {
  trimmed <- trimws(input_text)

  if (!grepl("^\\{", trimmed)) {
    return(NA_character_)
  }

  parsed <- tryCatch(
    fromJSON(trimmed, simplifyVector = FALSE),
    error = function(error) NULL
  )

  if (is.null(parsed)) {
    return(NA_character_)
  }

  clean_text(parsed$source_type)
}

looks_like_manual_article_json <- function(input_text) {
  trimmed <- trimws(input_text)

  if (!grepl("^\\{", trimmed)) {
    return(FALSE)
  }

  parsed <- tryCatch(
    fromJSON(trimmed, simplifyVector = FALSE),
    error = function(error) NULL
  )

  if (is.null(parsed)) {
    return(FALSE)
  }

  fields <- names(parsed)
  any(fields %in% c("observed_at", "url", "canonical_url", "title", "source"))
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop(
    "Please provide exactly one input file path.\n\n",
    "Example:\n",
    "Rscript scripts/import_medium_path_router.R debug_samples/medium_tag_page_fixture_small.json",
    call. = FALSE
  )
}

input_path <- args[1]

if (!file.exists(input_path)) {
  stop("The input file does not exist:\n\n", input_path, call. = FALSE)
}

input_text <- read_input_file_text(input_path)
file_extension <- tolower(tools::file_ext(input_path))
source_type <- detect_json_source_type(input_text)

if (identical(file_extension, "html") || identical(file_extension, "htm")) {
  if (looks_like_tag_page_html(input_text)) {
    run_import_script(file.path("scripts", "import_medium_tag_page_html.R"), input_path)
    quit(status = 0)
  }

  if (!looks_like_stats_html(input_text)) {
    stop(
      "This saved HTML file is not a recognized Medium import source.\n\n",
      "Supported HTML inputs:\n",
      "- Saved Medium tag/publication page HTML\n",
      "- Saved Medium /me/stats HTML",
      call. = FALSE
    )
  }

  run_import_script(file.path("scripts", "import_medium_own_stats_from_html.R"), input_path)
  quit(status = 0)
}

if (identical(source_type, "medium_tag_page_bookmarklet")) {
  run_import_script(file.path("scripts", "import_medium_tag_page_bookmarklet.R"), input_path)
  quit(status = 0)
}

if (identical(source_type, "medium_search_tags_page")) {
  run_import_script(file.path("scripts", "import_medium_search_tags_snapshot.R"), input_path)
  quit(status = 0)
}

if (looks_like_manual_article_json(input_text)) {
  run_import_script(file.path("scripts", "import_medium_manual_stats.R"), input_path)
  quit(status = 0)
}

stop(
  "Unknown import file type.\n\n",
  "Supported inputs:\n",
  "- Medium tag-page bookmarklet JSON\n",
  "- Medium search-tags watcher JSON\n",
  "- Existing Medium article bookmarklet JSON\n",
  "- Saved Medium tag/publication page HTML\n",
  "- Saved Medium /me/stats HTML\n",
  call. = FALSE
)
