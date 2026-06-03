# Article Lab App Split Investigation

Investigation date: 2026-06-03

Current file: `apps/human_preview_rating_app/app.R`

Current local size: 12,967 lines

Scope: investigation only. Do not change `app.R`, schema, API behavior, statuses, IDs, DB columns, file paths, or production data as part of this document.

## Current App Map

| Section | Current line range | Purpose | Move risk | Recommended timing |
| --- | ---: | --- | --- | --- |
| Startup/package checks and `source()` calls | 1-28 | Checks required R packages, loads packages, and sources existing helper files in startup order. | Medium | Move later |
| `ensure_rating_schema()` | 30-269 | Creates and migrates the original human preview rating, dimension rating, and dimension pass queue tables and indexes. | Low if exact move only | Move soon |
| Prompt/model/default configuration | 271-496 | Defines Article Lab default prompts, prompt keys, model choices, timeout constants, score defaults, batch sentinel, thumbnail variant count, publish targets, and monetization choices. Also includes prompt persistence helpers at 282-326. | Low for constants, medium for prompt DB helpers | Move soon for constants; move later for prompt DB helpers |
| Candidate status normalization helper | 497-512 | Adds normalized candidate status and display label columns for Article Lab candidate row frames. | Low | Move soon or with display/read helpers |
| `ensure_article_lab_schema()` | 514-987 | Creates and migrates Article Lab prompt, title batch, title candidate, API score, subtitle, thumbnail, outline, full-text, publication, and publish settings tables and indexes. Also performs status/data normalization updates. | Medium | Move soon only as an exact move |
| `ensure_research_workflow_schema()` | 989-1115 | Creates and migrates research source, angle, summary, asset, and summary prompt tables and indexes. | Low if exact move only | Move soon |
| Research source / summary / PDF helpers | 1117-1447 | Defines research sort SQL, research DB read helpers, display helpers for links/truncation, summary prompt helpers, PDF path/hash/type helpers, and PDF asset persistence. | Medium | Move later, except read-only helpers can move after schema |
| R-to-Node/Python API wrapper functions | 1448-2449, 3154-3227, 3425-3466 | Calls local Node/Python helper scripts for research summaries, title generation, title scoring, subtitles, thumbnails, outlines, full text, and Medium tags. Handles temp files, working directory changes, API key checks, helper availability, and JSON parsing. | Medium to high | Move later unless signatures remain identical |
| Article Lab DB read helpers | 2486-2802, 3104-3149, 3337-3343, 3364-3394, 4092-4136, 4720-4890 | Loads subtitle targets/rows, thumbnail packages/rows, outline-ready rows, full-text rows/packages, publications, review/publish rows, unscored candidates, latest batch, candidates, batches, scoring rows, and overview data. | Medium | Move later, after schema/config passes |
| Article Lab DB mutation/status transition helpers | 2882-3078, 3229-3317, 3345-3362, 3467-4879, 4928-4949 | Updates notes, syncs parent stage statuses, inserts/updates/approves outlines and full-text drafts, saves publications and publish settings, generates and persists stage candidates, approves/rejects subtitles and thumbnails, recovers pending rows, upserts API scores, moves/archives/approves candidates, saves batches, and updates candidate notes. | High | Do not move yet, except small non-status note helpers after tests exist |
| Article Lab display table/UI helper functions | 4892-5587 | Builds overview values, table/card/grid UI for generation, scoring, subtitle, thumbnail, outline, full-text, review/publish, badges, and thumbnail-ready rows. | Medium | Move later, after CSS/asset pass or in a focused display-helper pass |
| Original rating/dimension workflow helpers | 5625-7134 | Reads thumbnail queues/cohorts, builds lookup maps, loads rating candidates, manages rating queues, creates/resumes sessions, saves/undos ratings, manages dimension pass queues, and saves/undos dimension ratings. | High | Do not move yet |
| UI construction and inline CSS/JS | 7136-8524 | Defines `fluidPage()`, inline CSS, topbar/sidebar/main/guide shell, keyboard shortcuts, layout custom message handlers, thumbnail timer handlers, copy helpers, and static UI outputs. | Medium | Move soon for assets only; layout stays until stable |
| Server initialization | 8526-8552 | Opens DB connection, registers disconnect, creates/resumes rating session, initializes active section/dimension/current item, saved prompt state, Article Lab reactive values, and refresh trigger. | High | Do not move yet |
| Prompt selector observers/renderers | 8554-8643 | Updates summary prompt text and renders/selects/saves generation, outline, and full-text prompt selectors. | High | Do not move yet |
| Navigation/layout observers and main workflow UI | 8645-9313 | Handles sidebar navigation, sends workflow layout messages, defines `refresh_current()`, computes queue/candidate stats, renders sidebar, main panel, progress, debug banners, and shortcuts. `main_panel` contains most Article Lab tab layout construction. | High | Do not move yet |
| Reactive values/state setup and derived workflow reactives | 8530-8552, 8690-8707, 9315-9604, 12257-12271 | Holds global session state, Article Lab refresh state, selected batch/source state, and derived data frames for each workflow step. | High | Do not move yet |
| Server-local collector helpers | 9606-9706 | Collects row-level checkbox, notes, outline, full-text, and publish form values from dynamic Shiny inputs. | High | Do not move yet |
| Selection synchronizing observers | 9708-9799 | Keeps hidden selection snapshots and select-all behavior in sync for Generate, API queue, scored rows, subtitle rows, thumbnail packages, and thumbnail candidates. | High | Do not move yet |
| Research workflow observers/event handlers | 9801-10195 | Refreshes and selects research sources, ranks/unranks/finishes sources, saves sources and angles, saves/confirms summaries, downloads/uploads/clears PDFs, generates summary drafts, and sends research context to title generation. | High | Do not move yet |
| Article Lab `renderUI`/`renderDT` prompt and table blocks | 10197-11177 | Renders notices, summary/source selectors, exact effective prompt panels, research source tables, angle UI, batch selectors, generation/scoring/subtitle/thumbnail/outline/full-text/review/publish sections, and Medium tags prompt preview. | High | Do not move yet |
| Article Lab workflow event handlers | 11179-12364 | Generates and saves titles, saves prompts, triages candidates, moves titles through API scoring, subtitle, thumbnail, outline, full text, and review/publish stages, refreshes panels, and opens docs modal. | High | Do not move yet |
| Guide, article preview, thumbnail, and rating panel renderers | 12366-12843 | Renders right-side guide content, article preview, image output, rating panel, dimension controls, and completion states. | High | Do not move yet |
| Home rating and dimension event handlers | 12845-12965 | Handles score buttons/hotkeys, skip, undo, dimension value selection, reset/back no-ops, and next-dimension flow. | High | Do not move yet |
| `shinyApp(ui, server)` | 12967 | Launches the Shiny app. | High | Do not move yet |

## Split Strategy Options

### A. Layer-Based Split Now

Example targets:

- `R/schema_rating.R`
- `R/schema_article_lab.R`
- `R/schema_research.R`
- `R/prompt_config_helpers.R`
- `R/db_article_lab_read_helpers.R`
- `R/db_article_lab_write_helpers.R`
- `R/api_request_helpers.R`
- `R/display_table_helpers.R`
- `R/ui_asset_helpers.R`

Pros:

- Reduces file size without changing Shiny runtime architecture.
- Keeps the current app orchestration intact.
- Allows exact function moves with no signature or behavior changes.
- Makes future review easier because each pass can be small and diffable.

Cons:

- Source order becomes more important.
- Schema files can still be risky because they contain migration SQL and status normalization updates.
- Extracting DB write helpers too early can hide workflow side effects away from the observers that depend on them.

Assessment: best near-term direction, but only for low-coupling layers first.

### B. Tab/Module Split Later

Example targets:

- `modules/mod_research_inbox.R`
- `modules/mod_summary.R`
- `modules/mod_generate.R`
- `modules/mod_api_scoring.R`
- `modules/mod_subtitle_generation.R`
- `modules/mod_thumbnails.R`
- `modules/mod_outline.R`
- `modules/mod_full_text.R`
- `modules/mod_review_publish.R`

Pros:

- Could eventually isolate UI/server logic by user-facing workflow step.
- Could reduce cross-tab accidental coupling if module interfaces are designed after the workflow stabilizes.
- Could make isolated testing easier later.

Cons:

- Current server state is highly shared: `active_section`, selected batch, refresh counters, prompt state, research selection, and `article_lab_state` cross many tabs.
- Many observers both update DB state and navigate to the next workflow section.
- Dynamic input IDs and hidden selection snapshots cross section boundaries.
- Premature modules would force interface design before the Article Lab workflow is stable.

Assessment: not safe as the next step.

### C. Hybrid Approach

Keep `app.R` as the workflow orchestrator for now. Extract stable layers first: schema, exact prompt/model configuration, UI assets, and possibly API wrappers only if their signatures and side effects remain identical.

Pros:

- Gives immediate size reduction while preserving the current mental model.
- Avoids premature Shiny module boundaries.
- Lets future agents navigate the file by clearer layers and section boundaries.
- Keeps stateful workflow event handlers in one place until their coupling is better understood.

Cons:

- `app.R` remains large for a while.
- Some extracted files will be broad layer files rather than final architecture.
- Source order needs explicit documentation.

Recommendation: use the hybrid/layer-based approach now. Delay Shiny modules until the Article Lab workflow is more stable and the shared reactive state has clearer ownership boundaries.

## Safest Next Extraction Passes

### Pass 1: Schema Exact Move

Move only these functions, preserving exact contents and call order:

- `ensure_rating_schema()` from lines 30-269 to `R/schema_rating.R`.
- `ensure_article_lab_schema()` from lines 514-987 to `R/schema_article_lab.R`.
- `ensure_research_workflow_schema()` from lines 989-1115 to `R/schema_research.R`.

Rules:

- Preserve exact SQL, indexes, column definitions, status strings, defaults, and data normalization updates.
- Preserve startup call order and any helper source order dependencies.
- Do not change schema, index, status, migration, or default behavior.
- Treat `ensure_article_lab_schema()` as medium risk because it includes status/data update statements, even if the move itself is mechanical.

Why first: the schema functions are top-level, have clear boundaries, and can be moved without touching reactive/server logic.

### Pass 2: Prompt/Model Config Extraction

Move default prompt strings, prompt keys, model choices, timeout constants, publish target choices, monetization choices, and other pure config values into a config helper file such as `R/prompt_config_helpers.R`.

Candidates:

- `article_lab_default_prompt`
- `article_lab_manual_prompt_key`
- model defaults and model choice vectors
- research summary prompt defaults and prompt version choices
- subtitle, thumbnail, outline, full-text, and Medium tags default prompts
- `article_lab_default_score_prompt_version`
- `article_lab_default_score_scope`
- `article_lab_all_batches_value`
- `article_lab_default_thumbnail_variants`
- `article_lab_publish_target_choices`
- `article_lab_monetization_choices`

Rules:

- Preserve exact values.
- Do not change environment variable fallback order.
- Do not touch prompt persistence DB functions yet unless a focused review confirms the dependency chain is low-risk.

Why second: pure configuration is lower risk than DB writes and server observers, and it removes a large conceptual block from `app.R`.

### Pass 3: UI Asset/Display Extraction

Investigate moving inline CSS/JS out of `app.R` or into helper/asset files, with no layout behavior changes.

Candidates:

- Inline CSS in `tags$style(HTML(...))`, starting at line 7139.
- Inline JavaScript in `tags$script(HTML(...))`, ending at line 8501.
- Possibly static UI asset wrapper helpers if they do not depend on live server state.

Rules:

- Do not change Shiny input IDs.
- Do not change custom message handler names such as `clearRatingFocus`, `setWorkflowLayout`, `articleLabStartThumbnailTimer`, or `articleLabStopThumbnailTimer`.
- Do not change keyboard shortcut behavior.
- Do not change layout classes or HTML structure unless explicitly requested.

Why third: asset extraction can reduce visual noise in `app.R`, but CSS/JS are tightly coupled to input IDs, custom messages, and dynamic classes, so this should follow schema/config extraction.

## What Not To Move Yet

- Server observers.
- `reactiveValues` and reactive state holders.
- `active_section`, `active_dimension`, `current`, selected source, selected batch, and refresh state.
- `renderUI`, `renderDT`, `renderText`, and `renderImage` blocks.
- Workflow event handlers.
- Mutation-heavy status transition functions.
- DB write functions that update workflow stage status.
- API wrappers if moving them requires signature changes, new return shapes, changed temp-file behavior, changed working directory handling, or changed error messages.
- Shiny tab modules.
- Dynamic input ID conventions.
- Hidden selection snapshot behavior.
- Rating and dimension pass queue functions.

## Agent Navigation Improvement

Recommended practical option: maintain a lightweight `docs/article_lab_app_map.md` generated or manually refreshed from the current `app.R` line map.

Rationale:

- It improves navigation without modifying `app.R` during investigation work.
- It avoids mixing large comment-only edits into future extraction diffs.
- It gives agents a stable index for line ranges, section purpose, and extraction status.
- It can later be replaced or supplemented by section header comments once extraction passes start.

Suggested contents:

- Top-level section map with line ranges.
- Top-level function inventory grouped by layer.
- Current source order for helper files.
- Current extraction status and intended destination file.

If code edits become acceptable in a future task, add minimal section header comments to `app.R` at the same time as an extraction pass, not as a standalone churn-only edit.

## Validation Strategy For Future Split Passes

Required validation for any future split pass:

- Parse `apps/human_preview_rating_app/app.R` with R.
- Parse every changed R helper file.
- Source helper files in app startup order.
- Source `app.R` with `MEDIUM_RATING_DB` pointing to a disposable DB copy.
- Confirm production DB metadata is unchanged.
- Run `git diff --check`.
- Run existing lightweight tests relevant to the touched layer.
- Avoid external API calls.
- Avoid production DB writes.
- Use small commits per extraction pass.

Suggested lightweight commands, adjusted to the files changed:

```sh
Rscript -e 'parse("apps/human_preview_rating_app/app.R")'
git diff --check
npm run test:tag-bookmarklet
npm run test:search-tags
npm run test:tag-watcher
npm run validate:medium-v2
```

Notes:

- `npm run validate:medium-v2` depends on the local ignored SQLite database at `data/db/medium_articles.sqlite`.
- App startup validation must use a disposable DB copy, not the production DB.
- API-triggering observers must not be invoked during split validation.

## Final Recommendation

Keep splitting by layer now, using exact mechanical moves with no behavior changes.

Delay Shiny modules until the Article Lab workflow is more stable and shared state ownership is clearer.

Next concrete implementation pass: move the three schema functions into `R/schema_rating.R`, `R/schema_article_lab.R`, and `R/schema_research.R`, preserving exact SQL and startup call order.

Success looks like this:

- `app.R` is smaller without any user-visible behavior change.
- Schema functions parse and source in the same order as before.
- No schema, index, status, migration, API, DB column, ID, file path, or prompt value changes appear in the diff.
- Production DB metadata remains unchanged.
- The commit is small enough to review as an exact move.
