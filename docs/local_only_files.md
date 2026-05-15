# Local-Only Files

This repo intentionally ignores generated data and runtime artifacts so Git stays focused on source code, docs, and reusable workflow files.

Ignored local-only categories include:

- `.local_gitignored/` for scratch files and temporary diagnostics.
- `.env`, logs, `.DS_Store`, and other machine-local files.
- SQLite databases and backups under `data/db/`.
- Browser profiles and raw capture snapshots under `data/`.
- Generated analysis outputs under `data/analysis/medium_analysis_v1/` and `data/analysis/medium_analysis_v2/`.
- Download queues, downloaded images, raw API outputs, and JSONL dumps.

Do not delete these files just because they are ignored. Many are useful local state. If a result needs to be shared or documented, create a small curated summary in `docs/` rather than committing the raw generated artifact.
