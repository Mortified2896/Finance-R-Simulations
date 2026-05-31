# Script Inventory

This is a navigation aid for future maintenance sessions. It is not an exhaustive API reference.

## Medium Collection And Import

- `collect_medium_rss.R`: collect Medium RSS feed metadata.
- `collect_medium_public_stats.R`: collect public Medium stats from stored/imported article metadata.
- `collect_medium_public_stats_pw.js`: Playwright-based public stats collection helper.
- `watch_medium_tag_page_pw.cjs`: visible/attached browser watcher for Medium tag, search, publication, and article pages.
- `medium_tag_snapshot_watcher_helpers.mjs`: helper functions used by the tag watcher and tests.
- `medium_in_app_tag_watcher_runtime.mjs`: runtime for the in-app watcher snippet.
- `import_medium_manual_stats.R`: interactive/manual Medium stats importer.
- `import_medium_path_router.R`: routes dropped JSON/HTML inputs to the correct importer.
- `import_medium_own_stats_from_html.R`: imports saved Medium stats HTML.
- `import_medium_tag_page_bookmarklet.R`: imports JSON produced by the tag-page bookmarklet.
- `import_medium_tag_page_html.R`: imports saved/rendered Medium tag page HTML.
- `import_medium_search_tags_snapshot.R`: imports Medium search/tag watcher snapshots.
- `import_medium_article_text_snapshot.R`: imports saved article text snapshots.
- `medium_tag_html_to_json.R`: converts Medium tag/search/publication HTML into structured JSON.
- `medium_tag_import_helpers.R`: shared parsing, normalization, and database helper functions.

## Medium Analysis And Scoring

- `apply_medium_analysis_v2_schema.R`: creates or updates Medium Analysis V2 database objects.
- `validate_medium_analysis_v2.R`: validates the local database and V2 workflow assumptions.
- `build_medium_title_prediction_dataset.R`: builds the older title prediction dataset.
- `analyze_medium_title_text_baseline.R` and `analyze_medium_title_text_baseline_v2.R`: title baseline analyses.
- `analyze_medium_title_text_followup.R` and `analyze_medium_title_text_followup_v2.R`: title follow-up analyses.
- `analyze_medium_subtitle_text_analysis.R` and `analyze_medium_subtitle_text_analysis_v2.R`: subtitle analyses.
- `analyze_medium_subtitle_quality_audit.R`: subtitle quality audit.
- `analyze_medium_title_target_sensitivity.R`: target sensitivity analysis.
- `score_medium_titles_api_v2.py`: title/subtitle API scoring.
- `analyze_medium_title_api_scores_v2.R`: evaluates stored title/subtitle API scores.
- `export_medium_analysis_v2_chatgpt_snapshot.R`: exports a clean combined V2 snapshot with dataset rows joined to current API and human rating fields.
- `score_medium_thumbnails_api_v1.py`: thumbnail API scoring.
- `writing_api/generate_thumbnails.mjs`: Article Lab thumbnail image generation via the OpenAI Responses API image generation tool.
- `writing_api/generate_outlines.mjs`: Article Lab outline generation for approved title/subtitle/thumbnail packages.
- `writing_api/generate_full_text.mjs`: Article Lab full article draft generation from approved outlines, with PDF-first source context and summary fallback.
- `analyze_medium_thumbnail_api_scores_v1.R`: evaluates stored thumbnail API scores.
- `audit_medium_thumbnail_assets_v1.R`: checks thumbnail asset availability.
- `audit_medium_thumbnail_mapping.R`: audits dataset-to-queue thumbnail mappings and stored human/API thumbnail paths.
- `build_validated_thumbnail_manifest_v2.R`: builds the clean manifest-verified thumbnail cohort and valid sample CSV.
- `audit_validated_thumbnail_experiment_v2.R`: verifies v2 manual/API thumbnail paths and SHA-256 hashes against the manifest.
- `validate_dimension_v2_thumbnail_queue_mapping.R`: read-only v2 rating integrity check for validated cohort file/hash state and one-to-one queue-to-cohort key mapping.
- `audit_dimension_v2_thumbnail_provenance_row.R`: read-only row-level provenance trace across cohort CSV, source sample CSVs, DB/article/view rows, queue rows, observation tables, and local file identity.
- `rate_medium_titles_terminal.R`: blind terminal human-rating workflow.
- `score_medium_headlines_openai.py`, `analyze_openai_headline_scores.R`, and `analyze_openai_headline_scores_regularized.R`: older headline scoring workflow.
- `langfuse_python.py`: shared Python helper for optional Langfuse tracing and OpenAI client setup across scoring scripts.

## Image Queues And Downloads

- `export_medium_image_download_queue.R`: creates the thumbnail image download queue and preserves validated per-row local paths from prior runs.
- `export_medium_body_image_download_queue.R`: creates the body image download queue.
- `download_medium_images.py`: downloads queued Medium images with robot/crawl-delay awareness. Duplicate image hashes are allowed; when downloaded bytes match an existing file, the downloader copies that file to the current row's expected `image_file_stem` path and records duplicate metadata so local paths remain unique.

## Research Library

- `research_setup/apply_research_library_schema.R`: creates the local `research_papers` table and indexes in the existing Medium SQLite database, with a backup before schema changes by default.
- `research_import/import_research_papers_csv.R`: imports local research-library CSV metadata and idempotently upserts by `link_url` while preserving manual curation fields.
- `research_import/inspect_research_library.R`: prints a quick count summary for the local Research Library table.

## Inspection And Tests

- `inspect_medium_db.R`: quick local Medium DB inspection.
- `inspect_medium_public_stats.R`: public stats inspection helper.
- `test_medium_article_clap_capture.js`: Playwright clap-capture test.
- `test_medium_search_tags_parser.mjs`: parser test for Medium search/tag snapshots.
- `test_medium_tag_bookmarklet_extraction.js`: bookmarklet extraction test.
- `test_medium_tag_snapshot_watcher_helpers.mjs`: watcher helper test.
- `print_medium_search_tags_fixture.mjs`: prints search/tag fixture diagnostics.

## Writing Helpers

- `writing_setup/apply_article_lab_schema.R`: creates or updates the Article Lab title-generation and title-scoring tables and indexes with a database backup first.
- `writing_setup/apply_writing_lab_schema.R`: creates or updates the broader writing-lab schema objects.
- `writing_setup/apply_research_workflow_schema.R`: creates or updates the lightweight Research Inbox tables for curated sources and article angles, with a database backup first.
- `writing_setup/import_vanguard_papers_to_research_sources.R`: idempotently copies Vanguard rows from `research_papers` into the curated `research_sources` inbox; supports `--dry-run` and backs up before writing by default.
- `writing_api/generate_titles.mjs`: live OpenAI title-generation helper for the Article Lab Generate tab.
- `writing_api/generate_subtitles.mjs`: live OpenAI subtitle-generation helper for the Article Lab Subtitle Generation tab.
- `writing_api/summarize_research_pdf.mjs`: live OpenAI PDF summary helper for the Article Lab Summary tab.
- `writing_api/score_article_lab_titles.py`: live OpenAI title-only scoring helper for the Article Lab API score tab using the v2_2 rubric.
- `writing_api/reroll_sentence.mjs`: article sentence/paragraph rewrite helper.
- `writing_api/langfuse.mjs`: shared Node helper for optional Langfuse tracing around writing API calls.
- `writing_api/draft_section.mjs`: placeholder.
- `writing_api/critique_draft.mjs`: placeholder.
