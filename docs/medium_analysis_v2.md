# Medium Analysis V2

Medium Analysis V2 adds a durable workflow layer for title/subtitle analysis on top of the local SQLite database at `data/db/medium_articles.sqlite`.

The goal is repeatability. Do not manually patch rows to fix one analysis run. If the database grows or article URLs change shape, rerun the setup and validation scripts.

## V2.1 Notes

V2.1 changes the scoring scale to 1-5 and removes `click_potential` from the active rubric. That field remains in the table as a nullable legacy column, but V2.1 API runs do not require or populate it.

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

- `medium_title_api_scores`: cached structured API scores keyed by canonical article, title/subtitle hashes, prompt version, and model.
- `medium_title_human_ratings`: blind human ratings entered from Terminal.
- `medium_article_image_assets`: schema readiness for Thumbnail V3 image assets. Full image scoring is intentionally out of scope for this pass.

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
- API score counts by prompt/model
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

Dry run:

```sh
python3 scripts/score_medium_titles_api_v2.py --dry-run --limit 3
```

Small live run:

```sh
OPENAI_API_KEY=... python3 scripts/score_medium_titles_api_v2.py --limit 5 --model gpt-5-mini --prompt-version title_analysis_v2
```

The API scoring script reads from `v_medium_title_prediction_dataset_v2` and writes to `medium_title_api_scores`. It skips rows already scored for the same canonical key, title hash, subtitle hash, prompt version, and model unless `--force` is supplied.

Critical leakage rule: API scoring must only send `title` and `subtitle`. It must never send claps, responses, success_score, rank, page position, publication performance, dates, observations, times seen, thumbnail data, labels, or other outcome/performance fields.

## Human Ratings

```sh
Rscript scripts/rate_medium_titles_terminal.R --rater johannes --limit 100 --rating-version v2_general
```

The Terminal workflow randomly selects unrated articles for that rater/version. It shows only:

- title
- subtitle

It asks for:

- general rating, 1-5
- optional note

Controls:

- `s`: skip
- `q`: save and quit
- `b`: undo the previous rating from the current session

Old ratings are not overwritten.

## Convenience Launchers

Double-clickable helpers live in `01_manual_tools`:

- `validate_medium_analysis_v2.command`
- `run_medium_title_api_scoring_v2_test.command`
- `rate_medium_titles_terminal.command`

## Later Comparison

After enough API scores and human ratings exist, compare them against the V2 dataset labels and outcome fields from `v_medium_title_prediction_dataset_v2`. Keep model/human inputs blind. Join by `canonical_article_key`, `title_hash`, `subtitle_hash`, prompt/rating version, and model/rater metadata.

## Thumbnail V3 TODO

`medium_article_image_assets` exists so future thumbnail collection can attach image URLs and local paths to canonical articles. Existing image downloader scripts have not been deeply refactored in this pass.
