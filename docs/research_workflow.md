# Research Workflow

The Research Inbox is a lightweight writing-oriented layer in the local SQLite database.

- `research_sources` is the curated writing inbox for papers and articles worth considering.
- Raw imported paper data remains separate, including the existing `research_papers` table.
- `research_article_angles` stores article premises created from a source.
- Finished sources use `status = 'used'`, keep the article titles/URLs in `used_articles`, and record `finished_at`; they are hidden from the active ranked/unranked queues unless the `used` status filter is selected.
- Selected angles can be sent to the existing Article Lab title-generation flow from the Shiny app.

Lower `manual_sort_order` values appear higher in the Research Inbox. Blank sort values appear after manually ranked items.

Setup:

```sh
Rscript scripts/writing_setup/apply_research_workflow_schema.R
```

Optional Vanguard bridge:

```sh
Rscript scripts/writing_setup/import_vanguard_papers_to_research_sources.R --dry-run
Rscript scripts/writing_setup/import_vanguard_papers_to_research_sources.R
```

Both writing scripts create a timestamped backup in `data/db/BackupFolder` before writing unless `--skip-backup` is passed.
