# Human Preview Rating App

This local Shiny app supports fast blind ratings of Medium preview cards. It shows one article at a time using only the title, subtitle when available, and local thumbnail when available. It does not show claps, comments, dates, authors, publications, ranks, page positions, success scores, API scores, or other outcome fields.

## Run

Double-click:

```sh
01_manual_tools/rating/rate_medium_previews.command
```

Or run from the project root:

```sh
MEDIUM_PROJECT_ROOT="$PWD" Rscript -e 'shiny::runApp("apps/human_preview_rating_app", launch.browser = TRUE, host = "127.0.0.1", port = 3838)'
```

Required R packages:

```r
install.packages(c("shiny", "DBI", "RSQLite"))
```

`DBI` and `RSQLite` are already used by this project. `shiny` is required for the browser app.

## Data Source

The app inspects the local SQLite database at:

```sh
data/db/medium_articles.sqlite
```

The candidate article source is `v_medium_title_prediction_dataset_v2`, using:

- `article_id`
- `medium_post_id`
- `title`
- `subtitle`
- `thumbnail_url`

Local thumbnail paths are resolved from:

```sh
data/analysis/medium_images/medium_image_download_queue.csv
```

The app prefers rows with a non-empty title and an existing local thumbnail file. If fewer than 100 such rows are available, it fills the remaining queue with titled rows that lack a local thumbnail.

## Saved Tables

The app creates these SQLite tables if needed:

- `human_rating_sessions`
- `human_preview_rating_queue`
- `human_preview_ratings`

The default session size is 100 articles. A new session gets a random queue seed, and the resulting queue is persisted in `human_preview_rating_queue`. Refreshing or restarting the app resumes the latest incomplete `human_preview_rating_app_v1` session instead of reshuffling.

## Controls

- `1`, `2`, `3`, `4`, `5`: save that score and move to the next article
- `S`: skip current article
- `N`: focus the optional note box
- `U`: undo previous rating or skip
- `Esc`: clear the note box

Clicking a rating button saves immediately and advances. The note field appears before the rating buttons so notes can be written before one-click rating.

## Limitations

- The app currently uses the generated image download queue CSV for local thumbnail paths because `medium_article_image_assets` exists in the database but has no rows.
- `shiny` is not vendored or installed by this repository.
- Undo deletes the latest saved row for the active session and returns that queue item to `pending`. It does not restore the old note text.
