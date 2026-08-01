# Medium Analysis V2

Medium Analysis V2 adds a durable workflow layer for title/subtitle analysis on top of the local SQLite database at `data/db/medium_articles.sqlite`.

The goal is repeatability. Do not manually patch rows to fix one analysis run. If the database grows or article URLs change shape, rerun the setup and validation scripts.

## V2.1 and V2.2 Notes

V2.1 changes the scoring scale to 1-5 and removes `click_potential` from the active rubric. That field remains in the table as a nullable legacy column, but V2.1 API runs do not require or populate it.

The first V2.1 pilot over-scored most articles. `medium_clap_potential` and `overall_article_potential` had very low variance, and `predicted_success_bucket` classified too many articles as `high`.

V2.2 is the default prompt version for new API scoring. It keeps the same schema, but uses stricter relative calibration against typical Medium personal finance articles:

- most normal articles should receive 2 or 3
- 4 means clearly above average
- 5 is rare, top-tier potential
- `predicted_success_bucket = high` should mean likely top-20-percent potential and should be used sparingly

V2.2 still excludes click potential because competitor views, reads, impressions, and clicks are unavailable. It now supports two scoring scopes:

- `title_only`: for early idea screening when only a headline exists. The API receives only `title` and is told not to infer a subtitle.
- `title_subtitle`: for later screening after writing subtitles or article decks. The API receives only `title` and `subtitle`.

Full text is not required for either title/subtitle API scope. Thumbnail URL availability is enough to build a reusable thumbnail cohort for later image-scoring comparisons.

The new V2.1 API fields are:

- `medium_clap_potential`
- `medium_comment_potential`
- `overall_article_potential`

These map to observed public metrics as follows:

- `medium_clap_potential` maps to claps
- `medium_comment_potential` maps to responses/comments
- `overall_article_potential` maps most closely to the combined success score

The human rating workflow now uses one blind general 1-5 score plus an optional note.

## Added Database Objects

Run the setup script before using the V2 workflows:

```sh
Rscript scripts/apply_medium_analysis_v2_schema.R
```

The setup script creates a timestamped backup under `data/db/BackupFolder` before applying schema changes. It is idempotent and can be rerun.

Views:

- `v_medium_canonical_articles`: one canonical row per real Medium post. It prefers `medium_post_id`, falls back to normalized URL, prefers rows observed on tag pages, and keeps `duplicate_row_count`.
- `v_medium_title_prediction_dataset_v2`: one row per canonical recommended-page article for title/subtitle analysis. It includes title, subtitle, tag observation summary fields, outcomes, labels, and thumbnail URL availability.

Tables:

- `medium_title_api_scores`: cached structured API scores keyed by canonical article, title/subtitle hashes, prompt version, model, and `score_scope`.
- `medium_title_human_ratings`: blind human ratings entered from Terminal.
- `medium_article_image_assets`: schema readiness for Thumbnail V3 image assets. Full image scoring is intentionally out of scope for this pass.
- `medium_thumbnail_api_scores`: cached structured thumbnail/image API scores keyed by canonical article, prompt version, model, `score_scope`, image hash or URL, and any title/subtitle hashes used by the selected scope.

## Validation

```sh
Rscript scripts/validate_medium_analysis_v2.R
```

The validation report prints:

- active DB path
- `medium_articles` row count
- raw observed article counts from `medium_tag_page_observations`
- canonical article view row count
- V2 dataset row count
- duplicate `medium_post_id` groups
- coverage for title, subtitle, claps, responses, publication name, and thumbnail URL
- API score counts by prompt/model/scope
- API counts for `medium_clap_potential`, `medium_comment_potential`, and `overall_article_potential`
- human rating counts by rater/version
- human general_rating coverage
- suspicious `medium_articles.publication` values such as `Search` and `Write`
- reminder that `click_potential` is intentionally excluded in V2.1 because competitor impressions/views/reads are unavailable

If canonicalization reduces the dataset count versus raw observed articles, that is acceptable when it is explained by duplicate Medium post IDs.

## Analysis Output Folders

Generated title/subtitle analysis artifacts are grouped by workflow version:

- V1 analysis outputs: `data/analysis/medium_analysis_v1/`
- V2 analysis outputs: `data/analysis/medium_analysis_v2/`

The V2 lexical analysis scripts write to:

- `data/analysis/medium_analysis_v2/title_baseline/`
- `data/analysis/medium_analysis_v2/title_followup/`
- `data/analysis/medium_analysis_v2/subtitle_analysis/`

The older V1 dataset and matching outputs live under `data/analysis/medium_analysis_v1/` so V1 and V2 analysis files do not mix.

## API Title Scoring

Dry run title-only:

```sh
python3 scripts/score_medium_titles_api_v2.py --dry-run --limit 3 --prompt-version v2_2 --scope title_only
```

Dry run title + subtitle:

```sh
python3 scripts/score_medium_titles_api_v2.py --dry-run --limit 3 --prompt-version v2_2 --scope title_subtitle
```

Dry run a thumbnail-first pilot sample:

```sh
python3 scripts/score_medium_titles_api_v2.py --dry-run --limit 100 --prompt-version v2_2 --sample-mode thumbnail_first
```

Create a fixed 100-article thumbnail cohort without calling the API:

```sh
python3 scripts/score_medium_titles_api_v2.py \
  --dry-run \
  --limit 100 \
  --prompt-version v2_2 \
  --scope title_subtitle \
  --sample-mode thumbnail_only \
  --save-sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv
```

Small live run:

```sh
OPENAI_API_KEY=... python3 scripts/score_medium_titles_api_v2.py --limit 5 --model gpt-5-mini --prompt-version v2_2 --scope title_only
```

The API scoring script reads from `v_medium_title_prediction_dataset_v2` and writes to `medium_title_api_scores`. It skips rows already scored for the same canonical key, title hash, subtitle hash, prompt version, model, and scope unless `--force` is supplied.

Critical leakage rule: `title_only` must only send `title`; `title_subtitle` must only send `title` and `subtitle`. Neither scope may send claps, responses, success_score, rank, page position, publication performance, dates, observations, times seen, thumbnail data, labels, or other outcome/performance fields.

Sampling modes:

- `default`: preserve the current canonical-key order.
- `thumbnail_first`: score unscored rows with usable thumbnails first, then fill the remaining limit with random unscored rows without thumbnails.
- `thumbnail_only`: score only unscored rows with usable thumbnails.
- `random`: score random unscored rows regardless of thumbnail availability.

The script reports the thumbnail criterion it used. For title/subtitle API scoring it uses thumbnail availability from `v_medium_title_prediction_dataset_v2`, preferring `has_thumbnail_url` when available and otherwise falling back to a non-empty `thumbnail_url`. It does not require full text or a downloaded local image for this run.

Sample files:

- `--save-sample-file PATH` writes the selected cohort before scoring. The CSV includes article identity, title/subtitle, thumbnail URL availability, sample mode, and selection timestamp. It intentionally excludes claps, responses, success scores, ranks, dates, and other outcome fields.
- `--sample-file PATH` reuses an exact cohort. It matches by `canonical_article_key` first, with `article_id` and `medium_post_id` fallbacks, then still respects cache, `--limit`, prompt version, model, and score scope.

## API Thumbnail Scoring

The fixed first image cohort is:

- `data/analysis/title_api_score_samples/thumbnail_100_v1.csv`

It contains 100 thumbnail-available articles and should be reused across title and image scopes so comparisons are made on the same canonical articles. Full text is not required.

Thumbnail scopes are separate from title/subtitle scopes:

- `thumbnail_only`: sends only the image. This tests the image alone.
- `title_thumbnail`: sends title plus image.
- `title_subtitle_thumbnail`: sends title, subtitle, and image. This tests the fuller Medium feed package.

Images are API inputs and may cost more because image inputs count as tokens. Prefer existing local files as Base64 data URLs when available, otherwise use remote `thumbnail_url`. Never print Base64 image payloads; dry runs must show only a placeholder such as `data:image/jpeg;base64,<hidden>`.

Audit local thumbnail availability without changing the DB:

```sh
Rscript scripts/audit_medium_thumbnail_assets_v1.R
```

Dry run thumbnail-only scoring:

```sh
python3 scripts/score_medium_thumbnails_api_v1.py \
  --dry-run \
  --limit 3 \
  --prompt-version thumbnail_v1 \
  --scope thumbnail_only \
  --sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv
```

If the dry run is clean and `OPENAI_API_KEY` is already loaded, a one-row smoke test is enough to prove DB insert behavior:

```sh
python3 scripts/score_medium_thumbnails_api_v1.py \
  --limit 1 \
  --prompt-version thumbnail_v1 \
  --scope thumbnail_only \
  --sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv
```

Do not run the full 100-image job automatically.

Evaluate existing thumbnail scores:

```sh
Rscript scripts/analyze_medium_thumbnail_api_scores_v1.R \
  --prompt-version thumbnail_v1 \
  --scope thumbnail_only \
  --sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv \
  --output-mode all
```

Thumbnail evaluation outputs use the same durable folder pattern:

- `data/analysis/thumbnail_api_scores_v1/latest/`
- `data/analysis/thumbnail_api_scores_v1/by_method/<method_key>/`
- `data/analysis/thumbnail_api_scores_v1/runs/<timestamp_method_key>/`

The thumbnail scoring workflow has the same leakage rule as title scoring. It must not send claps, responses, success scores, ranks, page positions, publication performance, dates, observations, times seen, labels, or other outcome/distribution fields.

Recommended next pilot:

```sh
python3 scripts/score_medium_titles_api_v2.py \
  --dry-run \
  --limit 100 \
  --prompt-version v2_2 \
  --scope title_subtitle \
  --sample-mode thumbnail_only \
  --save-sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv

python3 scripts/score_medium_titles_api_v2.py \
  --limit 100 \
  --prompt-version v2_2 \
  --scope title_subtitle \
  --sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv

Rscript scripts/analyze_medium_title_api_scores_v2.R \
  --prompt-version v2_2 \
  --scope title_subtitle \
  --sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv \
  --output-mode all
```

Later, reuse the same cohort for title-only:

```sh
python3 scripts/score_medium_titles_api_v2.py \
  --limit 100 \
  --prompt-version v2_2 \
  --scope title_only \
  --sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv
```

Use `title_only` for early idea screening. Re-score promising titles with `title_subtitle` after writing subtitles or article decks, then compare both scopes on the same sample before deciding whether subtitles add signal. Thumbnail-only and title/subtitle-plus-thumbnail scoring should remain later layers using the same fixed cohort.

Do not run the full dataset until the 100-row V2.2 pilots show better score spread and useful separation against the matching text baselines.

## API Score Evaluation

After API scores have been written to SQLite, run the read-only diagnostic evaluator:

```sh
Rscript scripts/analyze_medium_title_api_scores_v2.R
```

Optional filters:

```sh
Rscript scripts/analyze_medium_title_api_scores_v2.R --prompt-version v2_1 --model gpt-5-mini --min-rows 30
```

Compare all available prompt/model groups:

```sh
Rscript scripts/analyze_medium_title_api_scores_v2.R --prompt-version all --scope all
```

Compare both V2.2 scoring scopes:

```sh
Rscript scripts/analyze_medium_title_api_scores_v2.R --prompt-version v2_2 --scope all
```

Evaluate one fixed sample cohort:

```sh
Rscript scripts/analyze_medium_title_api_scores_v2.R \
  --prompt-version v2_2 \
  --scope title_subtitle \
  --sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv \
  --output-mode all
```

The evaluator joins `medium_title_api_scores` to `v_medium_title_prediction_dataset_v2` by `canonical_article_key`. It does not call OpenAI, does not read `OPENAI_API_KEY`, and opens SQLite read-only.

Evaluation output structure:

- `data/analysis/title_api_scores_v2/latest/`: overwritten by the newest evaluation run.
- `data/analysis/title_api_scores_v2/by_method/<method_key>/`: overwritten only for the same prompt/model/scope/sample combination.
- `data/analysis/title_api_scores_v2/runs/<timestamp_method_key>/`: preserved historical snapshots.

The default `--output-mode all` writes to all three locations. `--output-mode latest`, `by_method`, or `snapshot` can be used for narrower output. Each output folder includes `run_metadata.txt` and `run_metadata.csv` with timestamp, filters, sample file, row count, DB path, and method key.

Each output folder contains:

- `api_eval_summary.txt`
- `api_score_distribution.csv`
- `api_score_correlations.csv`
- `api_score_auc.csv`
- `api_score_bucket_diagnostics.csv`
- `api_predicted_success_bucket_diagnostics.csv`
- false-positive and false-negative review CSVs for overall, clap, and comment potential

Interpretation:

- `medium_clap_potential` should be compared primarily with claps and `log_claps`.
- `medium_comment_potential` should be compared primarily with responses and `log_responses`.
- `overall_article_potential` should be compared primarily with `success_score` and high-performer labels.
- `predicted_success_bucket` is useful only if higher buckets separate actual outcomes.
- `title_only` and `title_subtitle` should be compared on overlapping articles before assuming subtitles improve ranking signal.

API scoring is only useful if it adds signal beyond the drafting-time text baseline. Compare its AUC and error examples against the V2 title-only and title+subtitle lexical analysis outputs before treating it as a durable ranking feature.

## Human Ratings

```sh
Rscript scripts/rate_medium_titles_terminal.R --rater johannes --limit 100 --rating-version v2_general_title_only
```

The Terminal workflow prioritizes unrated articles with thumbnails for that rater/version. The current human pass is title-only and should be stored under `rating_version = v2_general_title_only`. It shows only:

- title

It asks for:

- general rating, 1-5

For title-only ratings, `shown_subtitle` is stored as `NULL` and `subtitle_hash` is the blank-input hash, so these rows do not get mixed with future title+subtitle human ratings.

Controls:

- `s`: skip
- `q`: save and quit
- `b`: undo the previous rating from the current session

Old ratings are not overwritten.

## Convenience Launchers

Double-clickable helpers live in `01_manual_tools/analysis`:

- `analysis/validate_medium_analysis_v2.command`
- `analysis/run_medium_title_api_scoring_v2_test.command`
- `analysis/analyze_medium_title_api_scores_v2.command`
- `analysis/rate_medium_titles_terminal.command`

## Later Comparison

After enough API scores and human ratings exist, compare them against the V2 dataset labels and outcome fields from `v_medium_title_prediction_dataset_v2`. Keep model/human inputs blind. Join by `canonical_article_key`, `title_hash`, `subtitle_hash`, prompt/rating version, and model/rater metadata.
