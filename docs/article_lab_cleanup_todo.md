# Article Lab Cleanup TODO

Planning status for the Finance R Simulations Article Lab / Shiny app cleanup. This document is intentionally investigation-only: no schema, UI, API, status, ID, or behavior changes are proposed as part of this planning pass.

## Completed Cleanup Passes

- First helper extraction into focused files.
- Status/workflow helper extraction.
- Input/coercion helper extraction.
- Scoring helper extraction.
- Title/subtitle normalization helper extraction.

## Current Architecture Status

### Already Extracted Into `apps/human_preview_rating_app/R/`

- `R/app_config.R`: rating mode constants, dimension labels/questions/scales, default target count, and default app paths.
- `R/text_helpers.R`: text cleanup, multiline cleanup, `%||%`, row value fallback helpers, UTC timestamps, duration formatting, and thumbnail timing estimates.
- `R/file_helpers.R`: project root discovery, absolute path resolution, image URL normalization, SHA-256 helpers, debug logging, title-only display helpers, and dimensions v2 render audit helpers.
- `R/db_helpers.R`: SQLite connection setup and `db_add_column_if_missing()`.
- `R/ui_helpers.R`: small reusable Article Lab UI wrappers, including count badges, signal chips, table footers, section cards, action bars, notices, empty states, prompt blocks, and disabled-aware buttons.

### Still In `app.R`

- Startup package checks, library imports, and `source()` calls.
- Rating schema setup in `ensure_rating_schema()`.
- Article Lab schema setup in `ensure_article_lab_schema()`.
- Research workflow schema setup in `ensure_research_workflow_schema()`.
- Article Lab model defaults, model choices, timeout constants, prompt defaults, status values, status label maps, workflow section names, page metadata, score field names, and title length constants.
- Prompt persistence helpers, ID constructors, status/label helpers, workflow metadata helpers, score helpers, title/subtitle/thumbnail/outline/full-text normalization helpers, and row formatting helpers.
- Article Lab API orchestration functions that write temp JSON, call `scripts/writing_api/` helpers with `system2()`, parse stdout JSON, and fall back to local stub helpers where applicable.
- Article Lab DB read/write functions for batches, candidates, API scores, subtitles, thumbnails, outlines, full-text drafts, revisions, publications, publish settings, and Medium tags.
- Research inbox, summary, PDF asset, and research-to-title generation helpers.
- Rating queue/session helpers for the original human preview rating app and dimensions v1/v2 flows.
- Large Shiny UI definition, inline CSS/JS, `server()` reactive values, observers, render blocks, and tab/workflow logic.

### Risky To Move Now

- `server()` observers, `reactiveValues`, reactive expressions, `renderUI()`, `renderDT()`, and tab/workflow sections should stay in place until dependencies are mapped.
- DB mutation functions that update workflow stage status should not move before schema and status behavior are documented, because subtle status transitions are persisted.
- `ensure_rating_schema()`, `ensure_article_lab_schema()`, and `ensure_research_workflow_schema()` are behavior-sensitive because they create tables, add columns, create indexes, and run data backfill/status normalization SQL.
- API request functions are behavior-sensitive because they depend on temp files, working directory changes, timeout handling, stdout/stderr parsing, fallback behavior, and exact JSON payload shapes.
- ID constructors are persisted-contract helpers and should not be renamed or changed unless isolated with tests and explicit migration plans.

### Safe To Move Next

- Pure status and label helpers: `article_lab_normalize_candidate_status()`, `article_lab_status_label()`, `article_lab_subtitle_status_label()`, `article_lab_thumbnail_status_label()`, `article_lab_publish_status_label()`, and `article_lab_status_choices()`.
- Pure workflow/page metadata helpers: `article_lab_nav_meta()`, `article_lab_is_workflow_section()`, and the metadata constants they directly use.
- Pure title length and score helpers: `article_lab_title_length_flag()`, `article_lab_normalize_score()`, and `article_lab_combined_title_score()`.
- Pure candidate row helpers: `article_lab_row_input_id()` and `article_lab_normalize_candidate_rows()`.
- Small display/table helpers that do not own Shiny observers or state can move after the status/workflow extraction is proven.

### Shiny/R To `scripts/writing_api/` Boundary

- R is the orchestrator. It builds request payloads from current Shiny state and SQLite rows, writes a temp JSON request file, changes working directory to `project_root`, calls Node or Python with `system2()`, reads stdout/stderr temp files, parses stdout JSON, then writes results back to SQLite.
- Title generation calls `scripts/writing_api/generate_titles.mjs` through `article_lab_api_request()` and falls back to local stub title generation in `generate_title_candidates()`.
- API title scoring calls `scripts/writing_api/score_article_lab_titles.py` through `article_lab_score_api_request()` after resolving a Python interpreter that can import required packages.
- Research summary generation calls `scripts/writing_api/summarize_research_pdf.mjs` through `research_summary_api_request()`.
- Subtitle generation calls `scripts/writing_api/generate_subtitles.mjs` through `article_lab_subtitle_api_request()` and falls back to local stub subtitles in `generate_subtitle_candidates()`.
- Thumbnail generation calls `scripts/writing_api/generate_thumbnails.mjs` through `article_lab_thumbnail_api_request()` and falls back to local SVG/data-URI stub thumbnails in `generate_thumbnail_candidates()`.
- Outline generation calls `scripts/writing_api/generate_outlines.mjs` through `article_lab_outline_api_request()` and currently returns failed mode rather than a local outline fallback from `generate_outline_drafts()`.
- Full text generation calls `scripts/writing_api/generate_full_text.mjs` through `article_lab_full_text_api_request()` and currently returns failed mode rather than a local full-text fallback from `generate_full_text_drafts()`.
- Medium tag generation is also present in `app.R` through `article_lab_medium_tags_api_request()` and `scripts/writing_api/generate_medium_tags.mjs`, even though it was outside the initial file list.

## Phase 1: Safe Helper Extraction

### TODO: Extract Status And Label Helpers

- Description: Move status value constants, label maps, and pure label/status helpers into `R/status_helpers.R`. Include candidate, subtitle, thumbnail, and publish status labels without changing status strings or labels.
- Risk level: low.
- Suggested validation: source the app helper files in order and verify the moved functions return identical values for representative statuses: `generated`, `ready_for_api_scoring`, `api_scored`, `approved_for_subtitle`, `ready_for_thumbnail`, `ready_for_outline`, `ready_for_draft`, `ready_for_review_publish`, `archived`, `rejected`, unknown, and `NA`.
- Should happen before UI redesign: yes.
- Dependencies: depends on `clean_text()` and `article_lab_input_string()`; either move `article_lab_input_string()` first or keep source order so `text_helpers.R` is loaded before `status_helpers.R`.

### TODO: Extract Workflow/Page Metadata Helpers

- Description: Move `article_lab_workflow_sections`, `article_lab_page_meta`, `article_lab_nav_meta()`, and `article_lab_is_workflow_section()` into `R/workflow_helpers.R`.
- Risk level: low.
- Suggested validation: verify sidebar/main page routing still recognizes `research_inbox`, `summary`, `generate`, `api_scoring`, `subtitle_generation`, `thumbnails`, `outline`, `full_text`, and `review_publish`; no app launch required for first pass if sourcing succeeds.
- Should happen before UI redesign: yes.
- Dependencies: status helper extraction can happen independently, but `app.R` source order must place workflow helpers before `server()` uses them.

### TODO: Extract Title Length And Score Helpers

- Description: Move title length constants plus `article_lab_title_length_flag()`, `article_lab_normalize_score()`, and `article_lab_combined_title_score()` into a focused scoring/title helper file.
- Risk level: low.
- Suggested validation: compare output for boundary character counts `NA`, `45`, `46`, `60`, `61`, `90`, `91`, `140`, `141`; compare score normalization for `NA`, `1`, `3`, `5`, `0`, and `6`.
- Should happen before UI redesign: yes.
- Dependencies: used by schema backfill expectations, title saving, scoring rows, and draft UI; source before any DB load/save helpers.

### TODO: Extract Pure Candidate Row Normalization Helpers

- Description: Move `article_lab_row_input_id()` and `article_lab_normalize_candidate_rows()` after status helper extraction.
- Risk level: low.
- Suggested validation: run a small data frame through candidate row normalization with and without `ready_for_human_rating`, `promoted`, and `archived` columns; verify `normalized_status` and `status_label` match pre-extraction behavior.
- Should happen before UI redesign: yes.
- Dependencies: depends on status helpers and `clean_text()`; many UI and DB reader functions call it, so keep source order early.

### TODO: Extract Pure Table/Data Formatting Helpers

- Description: Move local formatting helpers that do not depend on Shiny state, such as score value formatting, row display helpers, tag parsing/display helpers, and package row shapers where they are pure.
- Risk level: medium.
- Suggested validation: source check plus representative function calls using empty data frames, one-row data frames, and missing optional columns.
- Should happen before UI redesign: yes.
- Dependencies: do after the status/workflow pass so the next diff remains small and easy to verify.

### TODO: Extract Pure Validation Helpers

- Description: Move pure input validation helpers such as title length calculation, title validation, manual title parsing, title normalization, subtitle normalization, Medium tag parsing limits, and input string normalization if dependency order allows.
- Risk level: medium.
- Suggested validation: call validation helpers with blank, duplicate, over-limit, multiline, and non-ASCII inputs; compare counts and normalized values with current behavior.
- Should happen before UI redesign: yes.
- Dependencies: may require moving `article_lab_input_string()` and `article_lab_input_multiline()` earlier, or folding them into `R/text_helpers.R` in a separate tiny pass.

### TODO: Extract Missed Reusable Shiny Tag Helpers

- Description: Move small tag helpers still in `app.R`, such as badges and possibly copy/button wrappers, without moving render blocks or tab sections.
- Risk level: medium.
- Suggested validation: source check and visual spot check later; for planning-only validation, confirm all helper dependencies are sourced before `ui <- fluidPage(...)`.
- Should happen before UI redesign: yes.
- Dependencies: status labels must be extracted first because badge helpers call label helpers.

## Phase 2: Schema And DB Boundary Cleanup

### TODO: Split Rating Schema Setup From Article Lab Schema Setup

- Description: Move `ensure_rating_schema()` to a rating schema helper file and `ensure_article_lab_schema()` to an Article Lab schema helper file, preserving exact SQL and call order.
- Risk level: medium.
- Suggested validation: source check only at first; later run against a disposable SQLite copy and compare table/column/index lists before and after.
- Should happen before UI redesign: yes.
- Dependencies: depends on `db_add_column_if_missing()` from `R/db_helpers.R`; do not edit SQL while moving.

### TODO: Document Schema Setup Side Effects

- Description: Add comments or docs describing that schema setup creates tables/indexes, adds missing columns, backfills title length flags, normalizes legacy status values, and recovers status from related subtitle/thumbnail rows.
- Risk level: low.
- Suggested validation: documentation review only; no DB operation required.
- Should happen before UI redesign: yes.
- Dependencies: should accompany or immediately follow schema file extraction.

### TODO: Keep Schema Behavior Unchanged During Cleanup

- Description: Treat schema helpers as byte-preserving moves where practical. Do not alter defaults, indexes, column definitions, update SQL, status values, or migration logic.
- Risk level: high if violated.
- Suggested validation: compare `git diff --word-diff` for moved SQL and run a disposable DB schema comparison before any real app run.
- Should happen before UI redesign: yes.
- Dependencies: requires discipline across all schema cleanup passes.

### TODO: Avoid Persisted Schema Changes In Cleanup-Only Passes

- Description: Defer any table, column, index, foreign key, default, uniqueness, or migration changes to a separate explicitly approved schema pass.
- Risk level: high if violated.
- Suggested validation: review diff for `CREATE TABLE`, `ALTER TABLE`, `CREATE INDEX`, `UPDATE ... SET status`, and persisted ID/status strings.
- Should happen before UI redesign: yes.
- Dependencies: none beyond review discipline.

## Phase 3: API/Script Boundary Cleanup

### TODO: Document R-To-Script API Calls

- Description: Document each `system2()` boundary, request payload shape, stdout JSON shape, fallback behavior, timeout handling, and persisted tables touched.
- Risk level: low.
- Suggested validation: documentation review only; no API calls.
- Should happen before UI redesign: yes.
- Dependencies: use current `app.R` API request functions as source of truth.

### TODO: Identify Duplicated JS Helpers

- Description: Track duplicated `cleanText()`, `extractText()`, `stripCodeFences()`, `previewText()`, package normalization, and response parsing patterns across `generate_titles.mjs`, `generate_subtitles.mjs`, `generate_thumbnails.mjs`, `generate_outlines.mjs`, and `generate_full_text.mjs`.
- Risk level: low for documentation, medium for eventual implementation.
- Suggested validation: no code change in planning; later add script-level fixture tests before extraction.
- Should happen before UI redesign: no.
- Dependencies: do not extract JS helpers until API behavior is locked down with tests or fixtures.

### TODO: Decide Later Whether To Extract Shared JS Helpers

- Description: Consider a future `scripts/writing_api/shared_text_helpers.mjs` or similar, but only after documenting exact parsing differences, such as full-text support for markdown fences and outline/full-text file attachments.
- Risk level: medium.
- Suggested validation: fixture tests for fenced JSON, raw text fallback, empty output, overlength titles/subtitles, and missing IDs.
- Should happen before UI redesign: no.
- Dependencies: depends on fixtures and agreement that live API behavior will not change.

### TODO: Preserve Live API Behavior During Planning

- Description: Do not change prompts, model defaults, JSON payload names, timeout values, fallback rules, or stdout/stderr parsing in this planning phase.
- Risk level: high if violated.
- Suggested validation: diff review only.
- Should happen before UI redesign: yes.
- Dependencies: none.

## Phase 4: Workflow-Stage Cleanup

### Research Inbox

- Likely UI section: `research_inbox_panel` inside `output$main_panel`, plus `DT::DTOutput()` tables and selected source/angle editor render blocks.
- Likely server/reactive section: `research_refresh`, `selected_research_source_id`, `research_ranked_sources`, `research_unranked_sources`, `selected_research_source`, `research_angles`, selected angle reactive, and research inbox observers.
- DB tables touched: `research_sources`, `research_article_angles`, and indirectly `article_lab_title_batches` via `article_lab_batch_id` linkage.
- API scripts touched: none directly from this stage.
- Modularize soon: wait. It is coupled to research source selection, summary generation, and send-to-title-lab behavior.

### Summary

- Likely UI section: `summary_panel` inside `output$main_panel`, including PDF upload/download controls, API summary prompt controls, and summary editor.
- Likely server/reactive section: `research_summary_sources`, `selected_research_source_summary`, `selected_research_pdf_asset`, summary prompt observer, PDF asset observers, summary generation/save/confirm/send observers.
- DB tables touched: `research_sources`, `research_source_summaries`, `research_source_assets`, and `research_summary_prompts`.
- API scripts touched: `scripts/writing_api/summarize_research_pdf.mjs`.
- Modularize soon: wait. It has file/PDF side effects, API calls, and data handoff to Generate.

### Generate

- Likely UI section: `generate_panel`, `article_lab_generate_table_ui()`, and generation prompt/render outputs.
- Likely server/reactive section: `article_lab_state$draft`, `article_lab_effective_generation_inputs`, prompt selectors/savers, generate/manual-title/save/triage/move observers.
- DB tables touched: `article_lab_prompts`, `article_lab_title_batches`, `article_lab_title_candidates`, plus research summary tables when using confirmed summaries as source context.
- API scripts touched: `scripts/writing_api/generate_titles.mjs`.
- Modularize soon: wait for helper extraction first; stage logic is tightly coupled to draft state and navigation.

### API Scoring

- Likely UI section: `api_score_panel`, `article_lab_score_sections`, `article_lab_score_queue_table_ui()`, and `article_lab_score_table_ui()`.
- Likely server/reactive section: `article_lab_scoring_rows`, `article_lab_queue_rows`, `article_lab_scored_rows`, score button renderer, score/archive/approve observers.
- DB tables touched: `article_lab_title_candidates`, `article_lab_title_api_scores`, and `article_lab_title_batches` status updates.
- API scripts touched: `scripts/writing_api/score_article_lab_titles.py`.
- Modularize soon: wait. It mutates status around `api_pending` and cache reuse and needs tests before extraction.

### Subtitle Generation

- Likely UI section: `subtitle_generation_panel`, `article_lab_subtitle_sections`, subtitle target/candidate table helpers, and manual subtitle controls.
- Likely server/reactive section: `article_lab_subtitle_target_rows`, `article_lab_pending_subtitle_rows`, subtitle choice map update, generate/manual/add/approve/reject/archive observers.
- DB tables touched: `article_lab_title_candidates`, `article_lab_subtitle_candidates`, `article_lab_title_batches`, and research summary context for subtitle generation.
- API scripts touched: `scripts/writing_api/generate_subtitles.mjs`.
- Modularize soon: wait. It is a good later module boundary after helper/DB status extraction.

### Thumbnails

- Likely UI section: `thumbnail_panel`, `article_lab_thumbnail_sections`, package table helper, thumbnail candidate grid helper, and thumbnail timer UI.
- Likely server/reactive section: `article_lab_thumbnail_package_rows`, `article_lab_pending_thumbnail_rows`, thumbnail generate/dismiss/approve/reject observers, timer custom messages.
- DB tables touched: `article_lab_title_candidates`, `article_lab_subtitle_candidates`, `article_lab_thumbnail_candidates`, and `article_lab_title_batches`.
- API scripts touched: `scripts/writing_api/generate_thumbnails.mjs`.
- Modularize soon: wait. The blocking API call, timer state, fallback data URIs, and one-approved-thumbnail rule make it medium/high risk.

### Outline

- Likely UI section: `outline_panel`, `article_lab_outline_sections`, and `article_lab_ready_for_outline_table_ui()`.
- Likely server/reactive section: `article_lab_ready_for_outline_rows`, outline prompt selector/saver, context toggle, generate/save/approve/refresh observers.
- DB tables touched: `article_lab_thumbnail_candidates`, `article_lab_subtitle_candidates`, `article_lab_title_candidates`, `article_lab_outlines`, and research summary/PDF context tables.
- API scripts touched: `scripts/writing_api/generate_outlines.mjs`.
- Modularize soon: wait. It uses optional PDF attachments and moves title candidates to `ready_for_draft`.

### Full Text

- Likely UI section: `full_text_panel`, `article_lab_full_text_sections`, and `article_lab_full_text_table_ui()`.
- Likely server/reactive section: `article_lab_full_text_rows`, `article_lab_full_text_package_rows_reactive`, full-text prompt selector/saver, generate/regenerate/save/approve/reject observers.
- DB tables touched: `article_lab_outlines`, `article_lab_full_text_drafts`, `article_lab_full_text_draft_revisions`, `article_lab_title_candidates`, and research summary/PDF context tables.
- API scripts touched: `scripts/writing_api/generate_full_text.mjs`.
- Modularize soon: wait. It has revision history, context-mode selection, regeneration variants, and handoff to Review & Publish.

### Review & Publish

- Likely UI section: `review_publish_panel`, `article_lab_review_publish_selector_ui()`, `article_lab_review_publish_workspace_ui()`, and Medium tags prompt UI.
- Likely server/reactive section: `article_lab_review_publish_rows`, `article_lab_publication_rows`, selected review row reactive, save publication/settings/status/export/tag generation observers.
- DB tables touched: `article_lab_full_text_drafts`, `article_lab_publish_settings`, `article_lab_publications`, and linked title/subtitle/thumbnail/outline rows.
- API scripts touched: `scripts/writing_api/generate_medium_tags.mjs`.
- Modularize soon: wait. It is a later boundary because it exposes final persisted publishing metadata and tag generation.

## Phase 5: UI/Workflow Polish

### TODO: Standardize Button Placement

- Description: Align generate/save/approve/reject/refresh button placement across stages after helper and workflow cleanup.
- Risk level: medium.
- Suggested validation: manual UI walkthrough on desktop and mobile after cleanup.
- Should happen before UI redesign: no.
- Dependencies: wait until helpers and stage dependencies are mapped.

### TODO: Standardize Card And Table Layout

- Description: Make card widths, table wrapping, section headers, counters, and empty states consistent across Generate, API Scoring, Subtitle, Thumbnail, Outline, Full Text, and Review & Publish.
- Risk level: medium.
- Suggested validation: visual regression screenshots or manual screenshots of each tab.
- Should happen before UI redesign: no.
- Dependencies: do not combine with helper extraction.

### TODO: Standardize Save/Generate/Approve/Reject Pattern

- Description: Make each stage show a predictable path: select rows, generate/save, review, approve/reject, and move to next stage.
- Risk level: medium.
- Suggested validation: manual workflow walkthrough using a disposable or copied DB.
- Should happen before UI redesign: no.
- Dependencies: wait until status/data-flow documentation is complete.

### TODO: Reduce Raw Internal ID Noise

- Description: Hide or collapse raw candidate, batch, subtitle, thumbnail, outline, and full-text draft IDs unless they are useful for debugging or traceability.
- Risk level: low for UI only, high if IDs are renamed or removed from persistence.
- Suggested validation: manual UI review; verify persisted IDs remain unchanged in DB and API payloads.
- Should happen before UI redesign: no.
- Dependencies: no persisted ID renaming.

### TODO: Clarify Status Labels

- Description: Improve visible status labels and next-step copy without changing status values stored in SQLite.
- Risk level: medium.
- Suggested validation: compare DB status values before/after UI-only label changes.
- Should happen before UI redesign: no.
- Dependencies: status helper extraction first.

### TODO: Clarify Next Action Per Tab

- Description: Add consistent next-action copy for empty states and stage headers.
- Risk level: low.
- Suggested validation: manual review across every workflow tab.
- Should happen before UI redesign: no.
- Dependencies: stage mapping should be complete.

### TODO: Standardize Model And Prompt Controls

- Description: Make model selectors, prompt selectors, prompt previews, save buttons, and custom prompt keys consistent across generation stages.
- Risk level: medium.
- Suggested validation: manual prompt save/load test on disposable DB.
- Should happen before UI redesign: no.
- Dependencies: prompt persistence helpers should be isolated first.

### TODO: Clarify Generated Package Labels

- Description: Improve labels for title/subtitle/thumbnail/outline/full-text chains so users can see package lineage without noisy raw IDs.
- Risk level: medium.
- Suggested validation: manual workflow walkthrough with multiple variants per stage.
- Should happen before UI redesign: no.
- Dependencies: no persisted ID renaming; DB joins must stay unchanged.

## Phase 6: Larger Architecture Decisions

### TODO: Decide Shiny Modules Vs Helper-Only Split

- Description: Decide whether to continue with helper-only extraction or introduce Shiny modules per stage after dependencies are mapped.
- Risk level: high.
- Suggested validation: prototype one non-critical read-only module only after helper cleanup; do not start with mutation-heavy stages.
- Should happen before UI redesign: no.
- Dependencies: requires workflow-stage dependency map and stable helper files.

### TODO: Document Workflow Engine/Data Flow

- Description: Create a data-flow document showing status transitions and table relationships from Research Inbox through Review & Publish.
- Risk level: low.
- Suggested validation: compare against actual SQL updates and observer actions.
- Should happen before UI redesign: yes.
- Dependencies: can begin after status helpers are extracted.

### TODO: Document Schema/Data Model

- Description: Document tables, primary IDs, foreign-key-like relationships, status columns, and stage ownership without altering schema.
- Risk level: low.
- Suggested validation: compare with `ensure_*_schema()` definitions.
- Should happen before UI redesign: yes.
- Dependencies: schema helper extraction makes this easier.

### TODO: Add Lightweight Helper Tests

- Description: Add focused tests or check scripts for status normalization, title length flags, score normalization, subtitle normalization, and row ID generation.
- Risk level: medium.
- Suggested validation: run the new focused test command locally without launching the app.
- Should happen before UI redesign: yes.
- Dependencies: helper files must be small enough to source without app startup side effects.

### TODO: Consider JS Helper Extraction For Writing API Scripts

- Description: Later, extract shared JS helpers for text cleanup, response text extraction, code fence stripping, preview truncation, and JSON parsing patterns.
- Risk level: medium.
- Suggested validation: fixture tests for every generation script before and after extraction.
- Should happen before UI redesign: no.
- Dependencies: do not alter live API behavior; fixtures needed first.

## Next Recommended Implementation Pass

Extract persisted ID helpers from `app.R` into a focused file without renaming IDs or changing ID formats.

Recommended files:

- `R/id_helpers.R`: pure ID constructors only, preserving existing prefixes, separators, and sequence formatting.

Why this pass:

- ID helpers are persisted-contract helpers, so isolating them makes the contract easier to review.
- The pass should be behavior-preserving and limited to pure value-in/value-out constructors.
- It should not touch schema, DB readers/writers, statuses, or API payloads.

Suggested validation for that pass:

- Source helper files in the same order as `app.R` startup.
- Run representative ID constructor calls and compare exact strings before/after extraction.
- Run `git diff --check` and `git status --short`.
- Use a disposable SQLite copy for any app startup smoke check.

## Do Not Do Yet

- Do not extract tab modules until dependencies are mapped.
- Do not redesign broad UI before helper and workflow cleanup.
- Do not change DB schema during cleanup-only passes.
- Do not rename persisted statuses or IDs.
- Do not rename DB columns.
- Do not rename candidate IDs, batch IDs, subtitle IDs, thumbnail IDs, outline IDs, or full text draft IDs.
- Do not change API behavior, prompt payloads, stdout parsing, fallback behavior, or model defaults.
- Do not large-rewrite `app.R`.
- Do not combine cleanup with visual redesign.

## Validation For This Planning Pass

- No full app launch required.
- No external OpenAI/API calls.
- No destructive DB operations.
- Basic file validation only: ensure this Markdown file exists and has content.
- Show `git status --short` after writing the file.
