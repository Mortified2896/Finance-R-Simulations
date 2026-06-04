# Human Preview Rating App

This Shiny app is being modularized in behavior-preserving passes. The first cleanup pass keeps `app.R` as the orchestrator and sources small helper files from `R/` near startup.

Extracted helpers:

- `R/app_config.R`: rating mode constants, dimension labels/questions/scales, default target count, and default app paths.
- `R/text_helpers.R`: text cleanup, row value fallback helpers, UTC timestamp formatting, and duration estimate formatting.
- `R/file_helpers.R`: project root discovery, absolute path handling, image URL normalization, SHA-256 helper, debug logging, and thumbnail-display helpers.
- `R/db_helpers.R`: database connection and low-level add-column helper.
- `R/ui_helpers.R`: small reusable Article Lab UI wrappers such as section cards, empty states, prompt blocks, buttons, and table footers.
- `R/ui_assets.R`: CSS and JavaScript assets for the local Shiny UI, including the compact workflow sidebar and wide-table layout rules.
- `R/schema_rating.R`, `R/schema_article_lab.R`, and `R/schema_research.R`: database schema setup helpers for rating, Article Lab, and research workflow tables.
- `R/schema_startup.R`: app database initialization orchestration that runs schema setup, recovery, and dimension queue preparation once per process.
- `R/table_helpers.R`: Article Lab table/card rendering helpers extracted from `app.R`.

Future rule: verify this behavior-preserving extraction before moving tab modules, reactive state, observers, or doing UI/design/workflow changes. Do not rename persisted IDs or status values during cleanup passes.
