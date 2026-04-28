required_packages <- c("xml2", "rvest", "DBI", "RSQLite", "lubridate")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("xml2", "rvest", "DBI", "RSQLite", "lubridate"))',
    call. = FALSE
  )
}

library(DBI)
library(RSQLite)

database_path <- file.path("data", "medium_articles.sqlite")

message("Medium RSS Database Inspector")
message("=============================")

if (!file.exists(database_path)) {
  message("The database does not exist yet.")
  message("Create it by running:")
  message("Rscript scripts/collect_medium_rss.R")
  quit(status = 0)
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

total_rows <- dbGetQuery(connection, "SELECT COUNT(*) AS n FROM medium_articles")$n

message("\nTotal rows")
message("----------")
message(total_rows)

message("\nRows by source tag")
message("------------------")

rows_by_tag <- dbGetQuery(connection, "
  SELECT source_tag, COUNT(*) AS row_count
  FROM medium_articles
  GROUP BY source_tag
  ORDER BY row_count DESC, source_tag ASC
")

if (nrow(rows_by_tag) == 0) {
  message("No articles found yet.")
} else {
  print(rows_by_tag, row.names = FALSE)
}

message("\n20 most recent articles")
message("-----------------------")

recent_articles <- dbGetQuery(connection, "
  SELECT
    source_tag,
    title,
    author,
    published_at,
    updated_at,
    url
  FROM medium_articles
  ORDER BY
    COALESCE(published_at, updated_at, fetched_at) DESC,
    id DESC
  LIMIT 20
")

if (nrow(recent_articles) == 0) {
  message("No articles found yet.")
} else {
  print(recent_articles, row.names = FALSE)
}
