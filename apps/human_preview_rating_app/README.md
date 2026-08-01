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

## OpenAI generation controls

Every model-selectable workflow uses the shared three-control group defined by the Article Lab configuration and UI helpers:

1. Model
2. Reasoning level
3. Execution mode (`Standard` or `Pro`)

The controls are capability-aware. Unsupported reasoning or Pro-mode controls remain visible and disabled as `Not supported`, and unsupported fields are omitted from API requests. GPT-5.6 Sol, Terra, and Luna expose the full reasoning range and Standard/Pro execution modes.

Selections are stored in the local `article_lab_generation_preferences` table under a stable workflow key. Saved selections take precedence over environment defaults and code defaults, and are restored after browser, app, or server restarts. Generated artifacts separately record `model`, `reasoning_effort`, and `reasoning_mode`; `reasoning_mode` must not be conflated with existing workflow `generation_mode` fields.

Defaults are configured with each workflow's existing `OPENAI_*_MODEL` variable plus matching `OPENAI_*_REASONING_EFFORT` and `OPENAI_*_REASONING_MODE` variables. Title generation defaults to `gpt-5.6-terra`, `low`, and `standard`; short generation/scoring workflows otherwise default to low reasoning, while research and long-form workflows default to medium reasoning.

New workflows must use `article_lab_generation_control_ui()` and the centralized capability validation rather than introducing a standalone model selector or constructing unvalidated reasoning parameters.

## Prompt template management

Every user-facing AI workflow uses `article_lab_prompt_manager_ui()` and `article_lab_prompt_manager_server()`. Templates are persisted under a stable workflow key, and selectors must never expose templates belonging to another workflow. The shared manager provides selection, create/save-as, update, rename, confirmed deletion, dirty-state feedback, and persistent validation errors.

Each workflow must declare its allowed `{{variable_name}}` placeholders. Unknown or unresolved variables block saving or execution, and exact-prompt preview and runtime execution must use the same renderer and selected template. A workflow may have an empty template library, but generation must remain unavailable until a valid template is created or selected. Do not introduce standalone editable prompt textareas for AI requests.
