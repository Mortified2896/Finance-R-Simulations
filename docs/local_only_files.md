# Local-Only Files

This repo intentionally ignores generated data and runtime artifacts so Git stays focused on source code, docs, and reusable workflow files.

Ignored local-only categories include:

- `.local_gitignored/` for scratch files and temporary diagnostics.
- `.env`, logs, `.DS_Store`, and other machine-local files.
- SQLite databases and backups under `data/db/`.
- Browser profiles and raw capture snapshots under `data/`.
- Saved website reference exports under `debug_samples/` and copied Medium homepage references under `design_refs/medium_homepage/`.
- Generated analysis outputs under `data/analysis/medium_analysis_v1/` and `data/analysis/medium_analysis_v2/`.
- Generated scoring and analysis folders such as `data/analysis/title_api_scores_v2/`, `data/analysis/thumbnail_api_scores_v1/`, `data/analysis/title_api_score_samples/`, `data/analysis/title_baseline/`, `data/analysis/title_followup/`, `data/analysis/subtitle_analysis/`, and related output folders.
- Download queues, downloaded images, raw API outputs, and JSONL dumps.

Do not delete these files just because they are ignored. Many are useful local state. If a result needs to be shared or documented, create a small curated summary in `docs/` rather than committing the raw generated artifact.
