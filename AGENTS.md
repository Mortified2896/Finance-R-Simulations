# Agent Instructions

This repository contains local Medium collection data and generated analysis outputs. Keep source code, docs, command launchers, and small curated notes trackable. Keep runtime data local-only.

Do not commit:

- SQLite databases, SQLite sidecar files, or database backups.
- Downloaded images or browser/Chrome profile directories.
- Generated queues, raw API outputs, JSONL dumps, or model scoring output folders.
- Generated analysis output folders such as `data/analysis/medium_analysis_v1/` and `data/analysis/medium_analysis_v2/`.
- Local scratch, cache, or runtime files.

Use `.local_gitignored/` for future scratch files, local exports, one-off diagnostics, and temporary runtime artifacts. Everything in that folder is intentionally ignored.

If a generated artifact is needed for documentation, prefer a small summarized `.txt` or `.md` file under `docs/` instead of committing raw databases, images, queues, caches, or API dumps.

Before committing, run:

```sh
git status --short
```

Check the output for accidental data/cache files before staging.
