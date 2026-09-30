# Legacy Shiny Article Lab

This is the existing local implementation, currently unused. New product work
targets [`article_lab_web`](../article_lab_web/README.md). This app is a reference,
not a requirement to preserve old architecture, database tables, UI controls, or
ports. There is no blanket requirement to keep it functional during the rewrite.

## Run the existing app when useful

From the repository root:

```sh
Rscript -e 'install.packages(c("shiny", "DBI", "RSQLite", "jsonlite", "DT"))'
./01_manual_tools/rating/rate_medium_previews.command
# Optional old Design v2:
./01_manual_tools/rating/rate_medium_previews_design_v2.command
```

These launchers target ports `3840` and `3844` respectively. Both use the same
Shiny code and, by default, the same local database at
`data/db/medium_articles.sqlite`. Design v2 is not an isolated data sandbox.

Existing targeted checks, when changing the corresponding legacy functionality:

```sh
npm ci
npm run test:article-inbox
npm run test:article-production
npm run validate:medium-v2
```

These require the relevant local R/Node environment; some require local data.
They are not release gates for an unrelated documentation or web-app change.

## Useful implementation references

`app.R` orchestrates the UI/server and sources R helpers. Inspect the relevant
helper rather than treating this README as a complete description of live code.

| Helpers | Reference value |
| --- | --- |
| `R/app_config.R`, `R/text_helpers.R`, `R/file_helpers.R` | Configuration, text formatting, local paths and thumbnail handling |
| `R/db_helpers.R`, `R/schema_*.R` | Existing SQLite connections and schema setup |
| `R/article_inbox_helpers.R` | Candidate capture and article-development handoffs |
| `R/ui_helpers.R`, `R/ui_assets.R`, `R/table_helpers.R` | Existing presentation conventions |
| `../../scripts/writing_api/` | Generation/scoring helpers, prompts and provenance |

The old UI includes workflow-scoped prompt templates and model/reasoning controls.
Those are reusable product ideas, not mandatory React components or current model
capability guarantees. Validate provider capabilities when implementing new AI work.

[Historical guide](../../docs/human_preview_rating_app.md) retains the detailed
rating experiments, controls, tables, and generation/publishing notes. Its old
imperative instructions do not govern the new application.
