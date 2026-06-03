connect_db <- function() {
  con <- dbConnect(SQLite(), db_path)
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

db_add_column_if_missing <- function(con, table, column, definition) {
  columns <- dbGetQuery(con, sprintf("PRAGMA table_info(%s)", dbQuoteIdentifier(con, table)))
  if (!(column %in% columns$name)) {
    dbExecute(con, sprintf(
      "ALTER TABLE %s ADD COLUMN %s %s",
      dbQuoteIdentifier(con, table),
      dbQuoteIdentifier(con, column),
      definition
    ))
  }
}
