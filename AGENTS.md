# Agent Instructions

This repository contains local Medium collection data and generated analysis outputs. Keep source code, docs, command launchers, and small curated notes trackable. Keep runtime data local-only.

## Project Map

- `R/`: finance simulation code and its own nested Node package for older Playwright helpers.
- `scripts/`: reusable collection, import, analysis, scoring, validation, and writing-helper scripts.
- `01_manual_tools/`: double-clickable macOS launchers, bookmarklets, watcher snippets, and manual workflow references.
- `article_projects/`: article briefs, drafts, outlines, style rules, and curated project notes.
- `docs/`: durable documentation and summarized workflow notes.
- `reports/`: small curated reports safe to track.
- `data/`: local runtime state. Assume files here are not for Git unless a task explicitly says otherwise.
- `.local_gitignored/`: scratch area for temporary diagnostics, local exports, and one-off artifacts.

Use `README.md` for the high-level map, `docs/script_inventory.md` to find the right script, `docs/local_only_files.md` for local-only policy, and `docs/medium_analysis_v2.md` for the current Medium analysis workflow.

## Development Workflow

- Prefer small documentation updates in `docs/` over committing raw generated outputs.
- Keep command launchers in `01_manual_tools/` and place new launchers in the existing workflow subfolders.
- Keep reusable automation in `scripts/`; avoid hiding source logic inside `.command` launchers.
- Keep local diagnostics, temporary exports, and one-off scratch files in `.local_gitignored/`.
- If adding a new generated data/output location, update `.gitignore`, `docs/local_only_files.md`, and this file if future agents need to know about it.
- If adding or renaming a script, update `docs/script_inventory.md`.
- If moving a manual tool, update `01_manual_tools/manual_tools_index.md` and any hard-coded references in docs, launchers, tests, and scripts.

## Fail Loud

**Every process in the Shiny app that can fail must fail loud, clear, and visible.** This is one of the most important design rules for this app, on par with the local-only policy and the no-secrets policy. A muted grey `lab-status-copy` line is not an acceptable failure surface; use a dedicated, persistent, prominent `.lab-alert-error` (or equivalent) banner driven by a dedicated reactive error state, never by string-matching the notice text.

Before adding or changing any user-triggered workflow step in `apps/human_preview_rating_app`, read the **Fail Loud** section in `docs/human_preview_rating_app.md` and apply the same pattern. The Outline tab's "Regenerate outline" button is the reference implementation. Any new API call, DB write, file operation, validation step, or selection-sync step must wire up the same kind of dedicated error state and loud alert. When in doubt, do not ship a feature without it.

## Useful Checks

Run focused checks that match the files changed. Useful lightweight checks include:

```sh
npm run test:tag-bookmarklet
npm run test:search-tags
npm run test:tag-watcher
npm run validate:medium-v2
```

The validation command depends on the local SQLite database at `data/db/medium_articles.sqlite`, which is intentionally ignored.

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
