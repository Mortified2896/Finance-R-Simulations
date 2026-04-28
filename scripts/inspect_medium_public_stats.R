required_packages <- c("xml2", "rvest", "DBI", "RSQLite")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("xml2", "rvest", "DBI", "RSQLite"))',
    call. = FALSE
  )
}

library(DBI)
library(RSQLite)

database_path <- file.path("data", "medium_articles.sqlite")

message("Medium Public Stats Inspector")
message("=============================")

if (!file.exists(database_path)) {
  message("The database does not exist yet.")
  message("Create it by running:")
  message("Rscript scripts/collect_medium_rss.R")
  quit(status = 0)
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

if (!dbExistsTable(connection, "medium_article_public_stats")) {
  message("The medium_article_public_stats table does not exist yet.")
  message("Create it by running:")
  message("Rscript scripts/collect_medium_public_stats.R")
  quit(status = 0)
}

total_observations <- dbGetQuery(connection, "
  SELECT COUNT(*) AS n
  FROM medium_article_public_stats
")$n

message("\nTotal observations")
message("------------------")
message(total_observations)

message("\nObservations by date")
message("--------------------")

observations_by_date <- dbGetQuery(connection, "
  SELECT observed_date, COUNT(*) AS observations
  FROM medium_article_public_stats
  GROUP BY observed_date
  ORDER BY observed_date DESC
")

if (nrow(observations_by_date) == 0) {
  message("No observations found yet.")
} else {
  print(observations_by_date, row.names = FALSE)
}

message("\nParse status counts")
message("-------------------")

status_counts <- dbGetQuery(connection, "
  SELECT parse_status, COUNT(*) AS observations
  FROM medium_article_public_stats
  GROUP BY parse_status
  ORDER BY observations DESC, parse_status ASC
")

if (nrow(status_counts) == 0) {
  message("No observations found yet.")
} else {
  print(status_counts, row.names = FALSE)
}

message("\n20 most recent observations")
message("---------------------------")

recent_observations <- dbGetQuery(connection, "
  SELECT
    s.observed_at,
    a.title,
    s.claps_count,
    s.responses_count,
    s.parse_status,
    COALESCE(a.url, s.article_url) AS url
  FROM medium_article_public_stats s
  LEFT JOIN medium_articles a
    ON a.url = s.article_url
  ORDER BY s.observed_at DESC, s.id DESC
  LIMIT 20
")

if (nrow(recent_observations) == 0) {
  message("No observations found yet.")
} else {
  print(recent_observations, row.names = FALSE)
}

message("\nArticles with multiple observations")
message("------------------------------------")

multiple_observations <- dbGetQuery(connection, "
  SELECT
    COALESCE(a.title, s.article_url) AS title,
    COUNT(*) AS observation_count,
    MIN(s.observed_at) AS first_observed_at,
    MAX(s.observed_at) AS last_observed_at
  FROM medium_article_public_stats s
  LEFT JOIN medium_articles a
    ON a.url = s.article_url
  GROUP BY s.article_url
  HAVING COUNT(*) > 1
  ORDER BY observation_count DESC, last_observed_at DESC
  LIMIT 20
")

if (nrow(multiple_observations) == 0) {
  message("No articles have multiple observations yet.")
} else {
  print(multiple_observations, row.names = FALSE)
}
