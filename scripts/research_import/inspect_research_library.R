required_packages <- c("DBI", "RSQLite")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Some required R packages are missing.\n\n",
    "Please install them by running this command in R:\n\n",
    'install.packages(c("DBI", "RSQLite"))',
    call. = FALSE
  )
}

library(DBI)
library(RSQLite)

parse_args <- function(args) {
  out <- list(db = file.path("data", "db", "medium_articles.sqlite"))
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--db") {
      i <- i + 1
      if (i > length(args)) stop("--db requires a path", call. = FALSE)
      out$db <- args[[i]]
    } else if (startsWith(arg, "--db=")) {
      out$db <- sub("^--db=", "", arg)
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1
  }
  out
}

print_section <- function(title, rows) {
  cat("\n", title, "\n", sep = "")
  cat(paste(rep("-", nchar(title)), collapse = ""), "\n", sep = "")
  if (nrow(rows) == 0) {
    cat("No rows found.\n")
  } else {
    print(rows, row.names = FALSE)
  }
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
database_path <- args$db

cat("Research Library Inspector\n")
cat("==========================\n")
cat("DB path: ", database_path, "\n", sep = "")

if (!file.exists(database_path)) {
  stop("Could not find database at: ", database_path, call. = FALSE)
}

connection <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(connection), add = TRUE)

if (!dbExistsTable(connection, "research_papers")) {
  stop(
    "Table research_papers does not exist. Run:\n",
    "Rscript scripts/research_setup/apply_research_library_schema.R",
    call. = FALSE
  )
}

total_rows <- dbGetQuery(connection, "SELECT COUNT(*) AS n FROM research_papers")$n

cat("\nTotal research_papers rows\n")
cat("--------------------------\n")
cat(total_rows, "\n", sep = "")

print_section(
  "Counts by source_name",
  dbGetQuery(connection, "
    SELECT source_name, COUNT(*) AS row_count
    FROM research_papers
    GROUP BY source_name
    ORDER BY row_count DESC, source_name ASC
  ")
)

print_section(
  "Counts by link_type",
  dbGetQuery(connection, "
    SELECT COALESCE(link_type, '(missing)') AS link_type, COUNT(*) AS row_count
    FROM research_papers
    GROUP BY COALESCE(link_type, '(missing)')
    ORDER BY row_count DESC, link_type ASC
  ")
)

print_section(
  "Counts by article_suitability",
  dbGetQuery(connection, "
    SELECT article_suitability, COUNT(*) AS row_count
    FROM research_papers
    GROUP BY article_suitability
    ORDER BY row_count DESC, article_suitability ASC
  ")
)

print_section(
  "Published date range",
  dbGetQuery(connection, "
    SELECT MIN(published_date) AS oldest_published_date,
           MAX(published_date) AS newest_published_date
    FROM research_papers
    WHERE published_date IS NOT NULL
  ")
)
