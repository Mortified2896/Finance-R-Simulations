# Human Preview Rating App

This local Shiny app supports blind manual ratings of Medium preview cards. It shows only the title, subtitle when available, and local thumbnail. It must not show claps, responses/comments, dates, authors, publications, ranks, page positions, success scores, API scores, old human ratings, reading time, times seen, or any other outcome/performance fields.

## Run

One-score mode:

```sh
01_manual_tools/rating/rate_medium_previews.command
```

Dimension pass mode:

```sh
01_manual_tools/rating/rate_medium_preview_dimensions.command
```

Or run dimension pass mode from the project root:

```sh
HUMAN_RATING_MODE=dimensions_v1 MEDIUM_PROJECT_ROOT="$PWD" Rscript -e 'shiny::runApp("apps/human_preview_rating_app", launch.browser = TRUE, host = "127.0.0.1", port = 3838)'
```

Clean validated dimension v2 mode:

```sh
Rscript scripts/build_validated_thumbnail_manifest_v2.R
HUMAN_RATING_MODE=dimensions_v2 MEDIUM_PROJECT_ROOT="$PWD" Rscript -e 'shiny::runApp("apps/human_preview_rating_app", launch.browser = TRUE, host = "127.0.0.1", port = 3838)'
```

Required R packages:

```r
install.packages(c("shiny", "DBI", "RSQLite"))
```

Article Lab API scoring uses a separate Python helper. By default the app uses the first available `python3`/`python` on `PATH`. If that is not the interpreter where `openai` is installed, start the app with an explicit override such as:

```sh
ARTICLE_LAB_PYTHON="/opt/homebrew/bin/python3" HUMAN_RATING_MODE=dimensions_v2 MEDIUM_PROJECT_ROOT="$PWD" Rscript -e 'shiny::runApp("apps/human_preview_rating_app", launch.browser = TRUE, host = "127.0.0.1", port = 3838)'
```

Install the package into that interpreter with:

```sh
"/opt/homebrew/bin/python3" -m pip install openai
```

## Article Lab Subtitle Stage

In the Article Lab subtitle workflow, the Subtitle Generation controls now include a manual entry path for subtitle ideas. Choose a title from the batch-aware picker, enter one subtitle idea per line, and add them directly into the same candidate queue used by generated subtitles.

Manual subtitle ideas are saved into the normal subtitle-candidate table and then appear under:

- `2. Subtitle candidates awaiting approval`

That keeps manual and API-generated subtitle variants in the same approval flow for rejection, approval, and downstream thumbnail work.

## Article Lab Model Selectors

All Article Lab model selection fields must be dropdown selectors, not free-text inputs. Use the shared model choice list in the app and include any environment-configured default as an available option, so future model fields stay consistent while still honoring local overrides.

## Modes

`feed_preview_1_5` is the original one-score workflow. It writes to `human_preview_ratings` and excludes articles that already have a row in that table.

`human_preview_dimensions_v1` is a separate dimension pass workflow over the thumbnail cohort. It writes one inspectable row per article to `human_preview_dimension_ratings`, with nullable columns for each dimension, and uses `human_preview_dimension_pass_queue` to track completion separately for each dimension. It does not read or mutate `human_preview_ratings` except as a fallback source for the cohort if the all-cohort CSV is missing.

`human_preview_dimensions_v2` is the clean validated-manifest workflow. It reads the validated manifest cohort, writes to `human_preview_dimension_ratings_v2`, and runs the full five-dimension pass order on that cohort. `title_hook_strength` remains title-only in v2 and hides subtitle and thumbnail during that pass. The thumbnail-based dimensions use the validated manifest image for the article.

## Dimension Cohort

For thumbnail analysis, v2 manifest rows are the intended source of truth:

```sh
data/analysis/medium_images/human_rated_thumbnail_valid_cohort_v2.csv
```

Generate it with:

```sh
Rscript scripts/build_validated_thumbnail_manifest_v2.R
```

The builder starts from `data/analysis/title_api_score_samples/human_rated_thumbnail_all_v1.csv`, joins to `v_medium_title_prediction_dataset_v2`, resolves thumbnails through `data/analysis/medium_images/medium_image_download_queue.csv`, computes SHA-256 hashes, and writes invalid/missing rows to `data/analysis/medium_images/human_rated_thumbnail_valid_cohort_v2_invalid_audit.csv`.

The thumbnail downloader treats duplicate image content as valid, but each row still needs its own expected file path. If Medium serves bytes whose SHA-256 matches an existing thumbnail, `scripts/download_medium_images.py` copies the existing file to the current row's `image_file_stem` path, records `duplicate_of_path`, and marks the row downloaded. Duplicate image hashes are allowed because Medium can reuse the same image content. Duplicate `local_image_path` values are not allowed because the manifest, Shiny app, and API scorer must all point to a row-specific validated file.

Important: treat old thumbnail-linked manual/API outputs outside the validated v2 manifest cautiously. Use the validated v2 cohort and manifest-backed image hashes for current manual thumbnail work and any clean downstream analysis.

The matching valid sample for analysis and text scorers is:

```sh
data/analysis/title_api_score_samples/human_rated_thumbnail_valid_v2.csv
```

Dimension pass mode prefers:

```sh
data/analysis/title_api_score_samples/human_rated_thumbnail_all_v1.csv
```

That CSV is intended to represent the 399 human-rated thumbnail articles. The app joins it to `v_medium_title_prediction_dataset_v2` for current `title`, `subtitle`, `thumbnail_url`, and `canonical_article_key`, then resolves local thumbnails through:

```sh
data/analysis/medium_images/medium_image_download_queue.csv
```

Only rows with existing, stem-validated local thumbnails are queued for new sessions. If an existing persisted pending queue item no longer has a valid local thumbnail, the app marks it `ignored_invalid_thumbnail` and excludes it from the active pass. If a row still somehow renders without a valid thumbnail, the app shows a blank white "Invalid or missing thumbnail" placeholder rather than falling back to another article's image. Existing one-score rows in `human_preview_ratings` do not exclude dimension candidates.

Restart the Shiny app after any thumbnail queue/exporter or app mapping change. A running Shiny process keeps the loaded R code in memory, so thumbnail validation fixes do not affect an already-open app session until it is stopped and started again.

## Verification Notes

When verifying this app after code changes, do not rely only on a lightweight shell probe such as `curl` or a bare browser screenshot. The Shiny server can be listening on `127.0.0.1:3838` while the actual session-backed UI state has not been exercised yet.

For app verification:

- First confirm the server is running and the expected queue/rating state exists in `data/db/medium_articles.sqlite`.
- Then verify the visible UI with a real browser-backed Shiny session, ideally by opening the app in a browser and refreshing after restart.
- If browser automation shows a blank or stale page but the DB state is correct, treat that as an incomplete browser/session verification problem rather than proof that the queue logic failed.

For reusable mapping QA, run:

```sh
Rscript scripts/audit_medium_thumbnail_mapping.R
```

It writes `data/analysis/medium_images/thumbnail_mapping_audit.csv`, which is local runtime output and can be used to filter analyses to `thumbnail_status == "valid"`.

Audit the v2 manual/API image contract with:

```sh
Rscript scripts/audit_validated_thumbnail_experiment_v2.R
```

That audit verifies `manual_shown_image_hash == api_scored_image_hash == manifest_image_sha256` for v2 rows and writes `data/analysis/medium_images/validated_thumbnail_experiment_v2_audit.csv`.

## Dimension Pass Order

The app rates one active dimension across the full cohort before moving to the next dimension.

In `human_preview_dimensions_v2`, the pass order is:

1. `ai_low_effort_flag`
2. `visual_hook`
3. `title_hook_strength`
4. `emotional_pull_preview`
5. `personal_click_appeal`

In `human_preview_dimensions_v1`, the historical order is:

1. `ai_low_effort_flag`
2. `visual_hook`
3. `title_hook_strength`
4. `emotional_pull_preview`
5. `personal_click_appeal`

The app starts with the first incomplete dimension. A dimension is complete only when every cohort article has either a value for that dimension or is explicitly skipped for that dimension. After a pass is complete, the app shows a "Start next dimension" button instead of automatically switching.

## Dimension Definitions

`ai_low_effort_flag`: Does the thumbnail look AI-generated, generic, sloppy, or low-effort? Focus on thumbnail only. Values: `yes`, `unsure`, `no`.

`visual_hook`: Does the thumbnail catch attention visually? Focus on thumbnail only. Scale: 1 visually boring, 2 weak, 3 okay, 4 strong, 5 very strong.

`title_hook_strength`: How strong is the title as a hook? Focus on title only. In v2, the app masks subtitle and thumbnail with placeholders during this pass. Scale: 1 weak/generic, 2 below average, 3 okay, 4 strong, 5 excellent. Active in v2.

`emotional_pull_preview`: Does the full preview create curiosity, concern, aspiration, tension, or emotion? Focus on title, subtitle, and thumbnail. Scale: 1 emotionally flat, 2 weak, 3 moderate, 4 strong, 5 very strong.

`personal_click_appeal`: Would I personally want to click/read this based on the preview? Focus on title, subtitle, and thumbnail. Scale: 1 definitely no, 2 probably no, 3 maybe/unclear, 4 probably yes, 5 definitely yes.

The dimension mode intentionally does not include `professional_credibility` or `thumbnail_trust_quality`.

## Controls

One-score mode:

- `A`, `S`, `D`, `F`, `J`: save scores `1`, `2`, `3`, `4`, `5`
- `1`, `2`, `3`, `4`, `5`: save that score
- `Space`: skip current article
- `N`: focus the optional note box
- `U`: undo previous one-score rating or skip
- `Enter` or `Esc` while the note box is focused: exit the note box without clearing the note

Dimension pass mode:

- AI pass: `A=yes`, `S=unsure`, `J=no`; `1=yes`, `2=unsure`, `3=no`
- Numeric passes: `A=1`, `S=2`, `D=3`, `F=4`, `J=5`; `1` through `5` also work
- The first key/button press saves only the active dimension column and advances to the next article in that same dimension
- `Space`: skip current article for the active dimension only
- `U`: undo previous completed item for the active dimension only
- `N`: focus the optional note box
- `Enter` or `Esc` while the note box is focused: blur the note box and preserve text

Shortcuts do not fire while the note input is focused. While typing notes, letter, number, space, undo, reset, and back keys behave as normal text input keys except `Enter` and `Esc`, which blur the field and preserve text.

Notes are stored in `human_dimension_note` with an active-dimension prefix, such as `[visual_hook] note text`, so notes from other dimension passes are preserved.

## Saved Tables

Shared session metadata for the one-score workflow:

- `human_rating_sessions`

One-score mode:

- `human_preview_rating_queue`
- `human_preview_ratings`

Dimension pass mode:

- `human_preview_dimension_ratings`
- `human_preview_dimension_ratings_v2`
- `human_preview_dimension_pass_queue`

`human_preview_dimension_ratings` stores one row per article and `rating_mode`, with nullable columns for `personal_click_appeal`, `title_hook_strength`, `visual_hook`, `emotional_pull_preview`, and `ai_low_effort_flag`. `human_preview_dimension_pass_queue` stores per-dimension queue status, so a skip applies only to the active dimension.

`human_preview_dimension_ratings_v2` is the clean validated-manifest table. It stores `rating_mode = human_preview_dimensions_v2`, `manifest_version = human_rated_thumbnail_valid_cohort_v2`, `shown_thumbnail_path`, and `shown_image_sha256` so manual rows can be checked against API scores and the manifest by hash.

## Warning

Both manual workflows are blind rating workflows. Do not add outcome/API/performance data to the UI, tooltip text, browser console payloads intended for display, or the preview card.

Old rows in `human_preview_ratings` and `human_preview_dimension_ratings` may include stale or wrong thumbnails. Old `medium_thumbnail_api_scores` rows with `prompt_version = thumbnail_v1` may also include stale or wrong thumbnails. Preserve them for audit/history, but do not use them as clean thumbnail evidence. For current manual thumbnail work and clean conclusions, use `human_preview_dimensions_v2` with the validated manifest cohort and manifest-backed image hashes.

Restart the Shiny app after app or manifest changes. A running Shiny process keeps loaded R code and cohort data in memory.
