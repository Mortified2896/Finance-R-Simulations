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
- `score_medium_thumbnails_api_v1.py`: thumbnail API scoring.
- `analyze_medium_thumbnail_api_scores_v1.R`: evaluates stored thumbnail API scores.
- `audit_medium_thumbnail_assets_v1.R`: checks thumbnail asset availability.
- `rate_medium_titles_terminal.R`: blind terminal human-rating workflow.
- `score_medium_headlines_openai.py`, `analyze_openai_headline_scores.R`, and `analyze_openai_headline_scores_regularized.R`: older headline scoring workflow.

## Image Queues And Downloads

- `export_medium_image_download_queue.R`: creates the thumbnail image download queue.
- `export_medium_body_image_download_queue.R`: creates the body image download queue.
- `download_medium_images.py`: downloads queued Medium images with robot/crawl-delay awareness.

## Inspection And Tests

- `inspect_medium_db.R`: quick local Medium DB inspection.
- `inspect_medium_public_stats.R`: public stats inspection helper.
- `test_medium_article_clap_capture.js`: Playwright clap-capture test.
- `test_medium_search_tags_parser.mjs`: parser test for Medium search/tag snapshots.
- `test_medium_tag_bookmarklet_extraction.js`: bookmarklet extraction test.
- `test_medium_tag_snapshot_watcher_helpers.mjs`: watcher helper test.
- `print_medium_search_tags_fixture.mjs`: prints search/tag fixture diagnostics.

## Writing Helpers

- `writing_api/reroll_sentence.mjs`: article sentence/paragraph rewrite helper.
- `writing_api/draft_section.mjs`: placeholder.
- `writing_api/critique_draft.mjs`: placeholder.
