# Project Cleanup Checklist

This checklist tracks project-wide cleanup before major feature work. The goal is to keep the repository safe, navigable, and behavior-preserving while local Medium data, generated outputs, and Article Lab workflows continue to evolve.

## Cleanup principles

- [x] Keep cleanup behavior-preserving unless a feature change is explicitly requested.
- [x] Keep runtime data, generated outputs, browser profiles, queues, raw API outputs, and private environment files untracked.
- [x] Prefer small curated summaries in `docs/` over committing raw generated artifacts.
- [x] Keep reusable automation in `scripts/`; keep `.command` launchers as thin manual entry points.
- [x] Run focused validation after each cleanup batch.

## 1. Repository hygiene preflight

Use this section before any larger cleanup pass.

- [x] Confirm the working tree starts clean with `git status --short`.
- [x] Confirm no tracked files are also ignored by `.gitignore`:

  ```sh
  git ls-files -ci --exclude-standard
  ```

- [x] Check for suspicious tracked local/generated/private paths:

  ```sh
  git ls-files | grep -E '(^|/)(\.DS_Store|\.env|node_modules|data|\.local_gitignored|debug_samples|\.opencode|\.playwright-mcp)(/|$)'
  ```

- [x] Check for suspicious tracked data/binary/generated extensions:

  ```sh
  git ls-files | grep -E '\.(sqlite|sqlite3|db|rds|RData|csv|jsonl|log|png|jpg|jpeg|webp|pdf)$'
  ```

- [x] Confirm `.env.example` contains placeholders only.
- [x] Confirm local runtime paths are ignored rather than tracked.

Current preflight result: no bad tracked local/generated files were found. `.env.example` is intentionally tracked as a placeholder file.

## 2. `.gitignore` and local-only policy alignment

- [x] Confirm `.gitignore` covers core private/local/runtime files: `.env`, `.env.*`, `.envrc`, `.DS_Store`, R local state, credentials, SQLite/database files, logs, caches, `node_modules/`, `data/`, `.local_gitignored/`, debug samples, and app-local data.
- [x] Confirm `.gitignore` and `docs/local_only_files.md` mention the same important local-only locations.
- [ ] Add any newly discovered generated-output location to `.gitignore`, `docs/local_only_files.md`, and `AGENTS.md` if future agents need to know about it.
- [ ] Re-run tracked-file hygiene checks after any `.gitignore` change.

## 3. Documentation map cleanup

- [x] Keep `README.md` as the high-level map.
- [x] Keep `AGENTS.md` as future-agent workflow guidance.
- [x] Keep `docs/script_inventory.md` as the script navigation index.
- [x] Keep `docs/local_only_files.md` as the local-only policy.
- [x] Keep `docs/article_lab_cleanup_todo.md` as the Article Lab/Shiny-specific cleanup list.
- [x] Update `docs/script_inventory.md` after any script add, rename, move, or removal.
- [x] Update `01_manual_tools/manual_tools_index.md` after any manual tool move.
- [x] Consider linking this checklist from `README.md` once it becomes the active cleanup source of truth.

## 4. Source / launcher / generated-output separation

- [x] Audit `.command` files and confirm they call reusable scripts rather than hiding source logic.
- [x] Move reusable automation into `scripts/` when practical.
- [ ] Keep temporary diagnostics and local exports in `.local_gitignored/`.
- [ ] Keep generated analysis outputs under ignored `data/` locations.
- [ ] If a generated result is useful for project understanding, summarize it in `docs/` instead of committing the raw output.

## 5. Placeholder and stale workflow audit

- [x] Review documented placeholders:
  - [x] `scripts/writing_api/draft_section.mjs`
  - [x] `scripts/writing_api/critique_draft.mjs`
- [x] Decide for each placeholder whether to keep, remove, document as intentionally reserved, or implement later.
- [x] Search for stale TODO/FIXME/placeholder mentions in tracked source/docs.
- [x] Move legacy-but-useful workflows into clearly documented legacy sections rather than deleting them casually.

Audit summary: see [`stale_workflow_audit.md`](stale_workflow_audit.md). No deletion-only cleanup was taken from the audit; remaining matches are legitimate UI/SQL placeholder language, active cleanup docs, or intentionally reserved future-work placeholders.

## 6. Article Lab / Shiny app cleanup

Detailed app-specific cleanup lives in `docs/article_lab_cleanup_todo.md`.

Project-wide guardrails for that work:

- [x] Keep `apps/human_preview_rating_app/app.R` as the workflow orchestrator for now.
- [x] Delay full Shiny tab modules until the workflow is more stable.
- [x] Extract only one cohesive behavior-preserving helper group per cleanup batch.
- [x] Avoid SQL, schema, prompt, payload, timeout, status, persisted ID, or model-default changes during cleanup-only passes.
- [ ] Use disposable DB copies for startup/smoke checks; do not write to the production local DB during cleanup validation.
- [ ] Avoid external API calls during cleanup validation.

## 7. Verification gate

Run the focused checks that match the cleanup batch. The default project-wide gate is:

```sh
git status --short
npm run test:tag-bookmarklet
npm run test:search-tags
npm run test:tag-watcher
npm run validate:medium-v2
```

Expected baseline from the current cleanup start:

- [x] JavaScript parser/bookmarklet/watcher tests pass.
- [x] Medium Analysis V2 validation completes.
- [x] No suspicious tracked local/generated files are present.

After each cleanup batch:

- [x] Re-run relevant checks for the initial documentation/hygiene cleanup batch.
- [x] Re-run relevant checks for the manual launcher and Article Lab table-helper extraction batch.
- [x] Re-run relevant checks for the README/checklist link and stale-workflow audit documentation batch.
- [x] Re-run relevant checks for the Article Lab schema startup extraction batch.
- [x] Re-run relevant checks for the initial browser-backed Article Lab UI streamlining batch.
- [x] Review `git status --short` for accidental local/generated files.
- [ ] Commit small, focused changes only after validation passes.

## Cleanup log

- 2026-06-03: Created project-wide cleanup checklist after read-only repo inspection. Initial hygiene audit found no bad tracked local/generated files and no tracked ignored files. Existing JS tests and Medium Analysis V2 validation passed before checklist creation.
- 2026-06-03: Linked the Article Lab cleanup TODO to this project-wide checklist and expanded `docs/local_only_files.md` to align with `.gitignore`. Re-ran tracked-file hygiene checks: zero suspicious tracked matches and no tracked ignored files. Re-ran `test:tag-bookmarklet`, `test:search-tags`, `test:tag-watcher`, and `validate:medium-v2`; all completed successfully. Validation still reports the known suspicious `publication` values `Write` and `Search`.
- 2026-06-03: Moved three long `.command` launcher implementations into reusable `scripts/manual_tools/*.zsh` scripts and kept the `.command` files as thin macOS double-click wrappers. Updated `docs/script_inventory.md` and `01_manual_tools/manual_tools_index.md` for the move.
- 2026-06-03: Kept `scripts/writing_api/draft_section.mjs` and `scripts/writing_api/critique_draft.mjs` as intentionally reserved placeholders after confirming they are not referenced by tracked source/docs beyond inventory documentation.
- 2026-06-03: Extracted Article Lab table/card UI helpers from `apps/human_preview_rating_app/app.R` into `apps/human_preview_rating_app/R/table_helpers.R` without SQL/schema/prompt/status changes. Verified `app.R` sources successfully, `zsh -n` passes for manual launchers/scripts, tracked-file hygiene still reports zero suspicious matches, and `test:tag-bookmarklet`, `test:search-tags`, `test:tag-watcher`, and `validate:medium-v2` all pass. Medium V2 validation still reports the known `Write` and `Search` publication warnings.
- 2026-06-03: Linked `docs/project_cleanup_checklist.md` from the root README and added `docs/stale_workflow_audit.md` to summarize the TODO/FIXME/placeholder audit. No stale workflow was deleted; remaining matches are legitimate UI/SQL placeholder language, active cleanup planning, or intentionally reserved future-work placeholders. Re-ran tracked-file hygiene checks, `git diff --check`, `test:tag-bookmarklet`, `test:search-tags`, `test:tag-watcher`, and `validate:medium-v2`; all completed successfully. Medium V2 validation still reports the known `Write` and `Search` publication warnings.
- 2026-06-03: Extracted Article Lab database startup orchestration from `app.R` into `apps/human_preview_rating_app/R/schema_startup.R`. This was an exact move of `initialize_app_database()` and kept schema SQL, migrations, defaults, indexes, statuses, prompts, and API behavior unchanged. Verified `app.R` sources successfully, tracked-file hygiene still reports zero suspicious matches, and the standard JS/Medium V2 validation gate passes with only the known `Write` and `Search` publication warnings.
