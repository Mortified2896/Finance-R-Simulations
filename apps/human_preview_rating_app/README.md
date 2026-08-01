# Human Preview Rating App

`app.R` is the Shiny UI/server orchestrator. It sources focused configuration, schema, database, API, workflow, rating, and UI helpers from `R/` in explicit startup order.

Key helper groups:

- `R/app_config.R`: rating mode constants, dimension labels/questions/scales, default target count, and default app paths.
- `R/text_helpers.R`: text cleanup, row value fallback helpers, UTC timestamp formatting, and duration estimate formatting.
- `R/file_helpers.R`: project root discovery, absolute path handling, image URL normalization, SHA-256 helper, debug logging, and thumbnail-display helpers.
- `R/db_helpers.R`: database connection and low-level add-column helper.
- `R/ui_helpers.R`: small reusable Article Lab UI wrappers such as section cards, empty states, prompt blocks, buttons, and table footers.
- `R/ui_assets.R`: CSS and JavaScript assets for the local Shiny UI, including the compact workflow sidebar and wide-table layout rules.
- `R/schema_rating.R`, `R/schema_article_lab.R`, `R/schema_research.R`, and `R/schema_article_inbox.R`: database schema setup helpers for rating, Article Lab production, research workflow, and the unified Article Inbox / Article Evidence handoff.
- `R/article_inbox_helpers.R`: canonical article-candidate capture, promotion, edit/archive/restore, and idempotent Article Evidence handoff helpers.
- `R/schema_startup.R`: app database initialization orchestration that runs current schema setup, recovery, and dimension queue preparation once per process.
- `R/table_helpers.R`: Article Lab table/card rendering helpers extracted from `app.R`.

Keep `app.R` as the workflow orchestrator unless a focused refactor has tests for the shared reactive state and cross-stage transitions. Persisted IDs and status values are data contracts and should not be renamed casually.
