# Research workflow reference

This describes existing local behavior and useful domain concepts, not a required
Cloudflare UI or schema. The previous Inbox proposal is
[historical design](article_lab_inbox_architecture.md).

The Shiny research workspace is rendered inside Article Inbox and stored in the
local SQLite database. Sources are evidence/context; source-derived angles are
possible article premises, not automatically approved article drafts.

- `research_sources` is the curated writing inbox for papers and articles.
- Raw imported paper metadata remains separate, including `research_papers`.
- `research_article_angles` stores premises derived from sources.
- Finished sources use `status = 'used'`, retain article titles/URLs in `used_articles`, and record `finished_at`; active queues hide them unless selected by the status filter.
- Selected angles can enter the existing title-generation workflow.

Lower `manual_sort_order` values appear first; blank values follow ranked items.
These names describe the existing implementation and need not become the new
application's persistence model.

Existing local setup and optional Vanguard bridge:

```sh
Rscript scripts/writing_setup/apply_research_workflow_schema.R
Rscript scripts/writing_setup/import_vanguard_papers_to_research_sources.R --dry-run
Rscript scripts/writing_setup/import_vanguard_papers_to_research_sources.R
```

The scripts create a timestamped backup in `data/db/BackupFolder` before writes
unless `--skip-backup` is passed. They are not setup steps for the review portal.
