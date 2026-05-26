# Research Library

The Research Library is a local-only curation workflow for tracking research papers and useful articles in the existing SQLite content database at `data/db/medium_articles.sqlite`.

This workflow is intentionally narrow. It stores CSV metadata for later review and article planning. It does not download PDFs, parse PDFs, crawl websites, call APIs, score papers, rank papers, or modify existing Medium analysis tables.

## Schema Setup

Run the schema setup once before importing CSV files:

```sh
Rscript scripts/research_setup/apply_research_library_schema.R
```

The script creates the `research_papers` table and indexes if they do not already exist. It is safe to rerun. By default, it creates a timestamped database backup under `data/db/BackupFolder` before schema changes.

Optional arguments:

```sh
Rscript scripts/research_setup/apply_research_library_schema.R --db data/db/medium_articles.sqlite
Rscript scripts/research_setup/apply_research_library_schema.R --skip-backup
```

## CSV Import

Import a CSV with:

```sh
Rscript scripts/research_import/import_research_papers_csv.R data/research/imports/vanguard_research_library_2026-05-25.csv
```

Or import from another local path:

```sh
Rscript scripts/research_import/import_research_papers_csv.R /Users/Jo/Downloads/vanguard_research_library_2026-05-25.csv
```

Use a non-default DB path if needed:

```sh
Rscript scripts/research_import/import_research_papers_csv.R data/research/imports/vanguard_research_library_2026-05-25.csv --db data/db/medium_articles.sqlite
```

The import is idempotent by `link_url`. Re-running the same CSV updates source/title/metadata fields but does not duplicate rows.

## Expected CSV Columns

The importer requires `title` and `link_url` for each imported row. If `source_name` is missing or blank, it is stored as `unknown` because the table requires a source name.

Expected columns are:

```text
source_name
source_type
source_collection
source_page_url
title
authors
topic
published_date
published_date_text
publication_year
summary
link_url
link_type
pdf_url
external_id
doi
research_status
article_suitability
manual_priority
used_in_project
notes
```

Missing optional columns are tolerated and imported as blank values.

## Manual Curation Fields

New rows default to:

```text
research_status = inbox
article_suitability = unknown
```

On reimport, these manual curation fields are preserved and not overwritten by the CSV:

```text
research_status
article_suitability
manual_priority
used_in_project
notes
```

This lets you mark or prioritize papers directly in the database, then safely reimport an updated source CSV later.

## Inspection

Print a quick local summary with:

```sh
Rscript scripts/research_import/inspect_research_library.R
```

The inspector reports total rows, counts by `source_name`, counts by `link_type`, counts by `article_suitability`, and oldest/newest `published_date`.

## Local Files

Local CSV imports and future research assets are intentionally ignored by Git:

```text
data/research/imports/
data/research/pdfs/
data/research/text/
data/analysis/research_library/
```

If a generated result needs to be shared, create a small curated summary in `docs/` rather than committing raw CSV imports, PDFs, parsed text, or analysis outputs.
