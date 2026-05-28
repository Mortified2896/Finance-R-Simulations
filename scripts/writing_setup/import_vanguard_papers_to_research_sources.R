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
  out <- list(db = file.path("data", "db", "medium_articles.sqlite"), skip_backup = FALSE, dry_run = FALSE)
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--db") {
      i <- i + 1
      if (i > length(args)) stop("--db requires a path", call. = FALSE)
      out$db <- args[[i]]
    } else if (startsWith(arg, "--db=")) {
      out$db <- sub("^--db=", "", arg)
    } else if (arg == "--skip-backup") {
      out$skip_backup <- TRUE
    } else if (arg == "--dry-run") {
      out$dry_run <- TRUE
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    i <- i + 1
  }
  out
}

backup_database <- function(database_path) {
  backup_dir <- file.path(dirname(database_path), "BackupFolder")
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_path <- file.path(
    backup_dir,
    paste0(tools::file_path_sans_ext(basename(database_path)), "_research_sources_import_backup_", timestamp, ".sqlite")
  )
  if (!file.copy(database_path, backup_path, overwrite = FALSE)) {
    stop("Could not create backup at: ", backup_path, call. = FALSE)
  }
  backup_path
}

now_utc <- function() format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

source_exists <- function(con, paper) {
  imported_id <- as.character(paper$paper_id)
  by_import <- dbGetQuery(con, "
    SELECT 1 FROM research_sources
    WHERE imported_from_table = 'research_papers'
      AND imported_from_id = ?
    LIMIT 1
  ", params = list(imported_id))
  if (nrow(by_import) > 0) return(TRUE)

  dbGetQuery(con, "
    SELECT 1 FROM research_sources
    WHERE (NULLIF(TRIM(source_url), '') IS NOT NULL AND source_url = ?)
       OR (NULLIF(TRIM(pdf_url), '') IS NOT NULL AND pdf_url = ?)
       OR (LOWER(TRIM(source_title)) = LOWER(TRIM(?)) AND COALESCE(source_name, '') = 'Vanguard')
    LIMIT 1
  ", params = list(paper$link_url, paper$pdf_url, paper$title)) |> nrow() > 0
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
database_path <- args$db

message("Vanguard Research Source Import")
message("===============================")
message("DB path: ", database_path)

if (!file.exists(database_path)) stop("Could not find database at: ", database_path, call. = FALSE)

con <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(con), add = TRUE)

if (!dbExistsTable(con, "research_papers")) stop("Expected table not found: research_papers", call. = FALSE)
if (!dbExistsTable(con, "research_sources")) {
  stop("Expected table not found: research_sources. Run scripts/writing_setup/apply_research_workflow_schema.R first.", call. = FALSE)
}

papers <- dbGetQuery(con, "
  SELECT paper_id, title, summary, link_url, pdf_url, source_name, source_type, manual_priority, research_status, notes
  FROM research_papers
  WHERE LOWER(COALESCE(source_name, '')) = 'vanguard'
  ORDER BY COALESCE(manual_priority, 999999), updated_at DESC, paper_id DESC
")

if (nrow(papers) == 0) {
  message("No Vanguard rows found in research_papers.")
  quit(status = 0)
}

already_imported <- vapply(seq_len(nrow(papers)), function(i) source_exists(con, papers[i, , drop = FALSE]), logical(1))
to_import <- papers[!already_imported, , drop = FALSE]

message("Vanguard rows found: ", nrow(papers))
message("Already represented in research_sources: ", sum(already_imported))
message("Rows to import: ", nrow(to_import))

if (args$dry_run || nrow(to_import) == 0) {
  message(if (args$dry_run) "Dry run complete; no rows inserted." else "Nothing to import.")
  quit(status = 0)
}

if (!args$skip_backup) {
  backup_path <- backup_database(database_path)
  message("Backup created: ", backup_path)
} else {
  message("Backup skipped because --skip-backup was supplied.")
}

timestamp <- now_utc()
dbBegin(con)
tryCatch({
  for (i in seq_len(nrow(to_import))) {
    paper <- to_import[i, , drop = FALSE]
    dbExecute(con, "
      INSERT INTO research_sources
        (created_at, updated_at, source_title, source_url, pdf_url, main_idea, abstract,
         source_type, source_name, manual_sort_order, status, notes, imported_from_table, imported_from_id)
      VALUES (?, ?, ?, ?, ?, ?, ?, 'paper', 'Vanguard', ?, ?, ?, 'research_papers', ?)
    ", params = list(
      timestamp,
      timestamp,
      paper$title,
      paper$link_url,
      paper$pdf_url,
      paper$summary,
      paper$summary,
      paper$manual_priority,
      ifelse(is.na(paper$research_status) || !nzchar(paper$research_status), "new", paper$research_status),
      paper$notes,
      as.character(paper$paper_id)
    ))
  }
  dbCommit(con)
}, error = function(e) {
  dbRollback(con)
  stop(e)
})

message("Imported rows: ", nrow(to_import))
