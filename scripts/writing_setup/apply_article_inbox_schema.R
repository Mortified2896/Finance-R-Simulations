required_packages <- c("DBI", "RSQLite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) stop("Missing required R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)

library(DBI)
library(RSQLite)

parse_args <- function(args) {
  out <- list(db = file.path("data", "db", "medium_articles.sqlite"), skip_backup = FALSE)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--db")) {
      i <- i + 1L
      if (i > length(args)) stop("--db requires a path", call. = FALSE)
      out$db <- args[[i]]
    } else if (startsWith(arg, "--db=")) {
      out$db <- sub("^--db=", "", arg)
    } else if (identical(arg, "--skip-backup")) {
      out$skip_backup <- TRUE
    } else stop("Unknown argument: ", arg, call. = FALSE)
    i <- i + 1L
  }
  out
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
if (!file.exists(args$db)) stop("Could not find database at: ", args$db, call. = FALSE)

if (!args$skip_backup) {
  backup_dir <- file.path(dirname(args$db), "BackupFolder")
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  backup_path <- file.path(backup_dir, paste0(tools::file_path_sans_ext(basename(args$db)), "_article_inbox_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sqlite"))
  if (!file.copy(args$db, backup_path, overwrite = FALSE)) stop("Could not create backup at: ", backup_path, call. = FALSE)
  message("Backup created: ", backup_path)
}

app_dir <- normalizePath(file.path("apps", "human_preview_rating_app"), mustWork = TRUE)
source(file.path(app_dir, "R", "text_helpers.R"))
source(file.path(app_dir, "R", "schema_article_inbox.R"))

con <- dbConnect(SQLite(), args$db)
on.exit(dbDisconnect(con), add = TRUE)
dbWithTransaction(con, ensure_article_inbox_schema(con))
message("Article Inbox schema is ready: ", args$db)
