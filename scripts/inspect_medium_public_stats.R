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

article_columns <- dbGetQuery(connection, "PRAGMA table_info(medium_articles)")$name
stats_columns <- dbGetQuery(connection, "PRAGMA table_info(medium_article_public_stats)")$name

message("\nManual relevance status")
message("-----------------------")

if ("manual_relevance_status" %in% article_columns) {
  relevance_counts <- dbGetQuery(connection, "
    SELECT
      COALESCE(manual_relevance_status, 'unreviewed') AS manual_relevance_status,
      COUNT(*) AS articles
    FROM medium_articles
    GROUP BY COALESCE(manual_relevance_status, 'unreviewed')
    ORDER BY articles DESC, manual_relevance_status ASC
  ")
  print(relevance_counts, row.names = FALSE)

  remaining_candidates <- dbGetQuery(connection, "
    SELECT COUNT(*) AS n
    FROM medium_articles a
    WHERE a.manual_relevance_status IS NULL
      AND NOT EXISTS (
        SELECT 1
        FROM medium_article_public_stats s
        WHERE s.article_url = a.url
          AND s.parse_status = 'ok'
      )
  ")$n

  message("\nRemaining unreviewed/unimported candidates")
  message("------------------------------------------")
  message(remaining_candidates)
} else {
  message("Manual relevance columns have not been added yet.")
  message("Run: Rscript scripts/import_medium_manual_stats.R")
}

message("\n20 most recent observations")
message("---------------------------")

article_publication_expr <- if ("publication" %in% article_columns) "a.publication" else "NULL"
article_publication_status_expr <- if ("publication_status" %in% article_columns) "a.publication_status" else "NULL"
stats_publication_snapshot_expr <- if ("publication_snapshot" %in% stats_columns) "s.publication_snapshot" else "NULL"
stats_publication_status_snapshot_expr <- if ("publication_status_snapshot" %in% stats_columns) "s.publication_status_snapshot" else "NULL"

recent_observations <- dbGetQuery(connection, sprintf("
  SELECT
    s.observed_at,
    a.title,
    %s AS publication,
    %s AS publication_status,
    %s AS publication_snapshot,
    %s AS publication_status_snapshot,
    s.claps_count,
    s.responses_count,
    s.parse_status,
    COALESCE(a.url, s.article_url) AS url
  FROM medium_article_public_stats s
  LEFT JOIN medium_articles a
    ON a.url = s.article_url
  ORDER BY s.observed_at DESC, s.id DESC
  LIMIT 20
", article_publication_expr, article_publication_status_expr, stats_publication_snapshot_expr, stats_publication_status_snapshot_expr))

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

message("\nOwn Medium stats")
message("----------------")

if (!dbExistsTable(connection, "medium_own_story_stats")) {
  message("The medium_own_story_stats table does not exist yet.")
  message("Create it by running:")
  message('Rscript scripts/import_medium_own_stats_from_html.R "debug_samples/Stats Page/Medium Stats Page.html"')
} else {
  total_own_observations <- dbGetQuery(connection, "
    SELECT COUNT(*) AS n
    FROM medium_own_story_stats
  ")$n

  distinct_own_articles <- dbGetQuery(connection, "
    SELECT COUNT(DISTINCT story_url) AS n
    FROM medium_own_story_stats
  ")$n

  latest_own_snapshot <- dbGetQuery(connection, "
    SELECT MAX(observed_at) AS observed_at
    FROM medium_own_story_stats
  ")$observed_at

  own_article_count <- if ("is_own_article" %in% article_columns) {
    dbGetQuery(connection, "
      SELECT COUNT(*) AS n
      FROM medium_articles
      WHERE is_own_article = 1
    ")$n
  } else {
    NA_integer_
  }

  message("Total own story stats observations: ", total_own_observations)
  message("Distinct own articles with private stats: ", distinct_own_articles)
  message("Latest own stats snapshot observed_at: ", ifelse(is.na(latest_own_snapshot), "(none)", latest_own_snapshot))
  message("Own articles in medium_articles: ", ifelse(is.na(own_article_count), "(column not present)", own_article_count))

  top_own_rows <- dbGetQuery(connection, "
    SELECT
      observed_at,
      title_snapshot AS title,
      views_count,
      reads_count,
      earnings_usd,
      story_url AS url
    FROM medium_own_story_stats
    ORDER BY
      COALESCE(earnings_usd, -1) DESC,
      COALESCE(views_count, -1) DESC,
      observed_at DESC
    LIMIT 10
  ")

  message("\nTop own rows by earnings/views")
  message("------------------------------")
  if (nrow(top_own_rows) == 0) {
    message("No own story stats observations found yet.")
  } else {
    print(top_own_rows, row.names = FALSE)
  }
}
