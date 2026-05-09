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

source(file.path("scripts", "medium_tag_html_to_json.R"))

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop(
    "Please provide exactly one saved Medium tag-page HTML file path.\n\n",
    "Example:\n",
    "Rscript scripts/import_medium_tag_page_html.R ~/Downloads/Medium\\ Finance\\ Recommended\\ Tag.html",
    call. = FALSE
  )
}

input_path <- args[1]

if (!file.exists(input_path)) {
  stop("The input file does not exist:\n\n", input_path, call. = FALSE)
}

payload <- medium_tag_html_to_payload(input_path)
safe_base <- gsub("[^A-Za-z0-9_-]+", "-", tools::file_path_sans_ext(basename(input_path)))
html_hash <- compute_source_file_hash(input_path)
output_path <- file.path(tempdir(), paste0(safe_base, "-", html_hash, ".medium-tag-page.json"))

writeLines(
  jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null"),
  output_path,
  useBytes = TRUE
)

claps_missing <- sum(vapply(payload$cards, function(card) is.null(card$claps) || is.na(card$claps), logical(1)))
responses_missing <- sum(vapply(payload$cards, function(card) is.null(card$responses) || is.na(card$responses), logical(1)))
read_time_missing <- sum(vapply(payload$cards, function(card) is.null(card$read_time_minutes) || is.na(card$read_time_minutes), logical(1)))

message("Parsed saved Medium tag-page HTML")
message("---------------------------------")
message("Tag: ", payload$tag_slug)
message("Page variant: ", payload$page_variant)
message("Cards parsed: ", length(payload$cards))
message("Missing claps after HTML parse: ", claps_missing)
message("Missing responses after HTML parse: ", responses_missing)
message("Missing read time after HTML parse: ", read_time_missing)
message("Generated tag-page JSON: ", output_path)
message("")

status <- system2(
  command = file.path(R.home("bin"), "Rscript"),
  args = shQuote(c(file.path("scripts", "import_medium_tag_page_bookmarklet.R"), output_path))
)

if (!identical(status, 0L)) {
  quit(status = status)
}
