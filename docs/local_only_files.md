# Local-Only Files

This repo intentionally ignores generated data and runtime artifacts so Git stays focused on source code, docs, and reusable workflow files.

Ignored local-only categories include:

- `.local_gitignored/` for scratch files, temporary diagnostics, local exports, and one-off artifacts.
- `.opencode/vendor/` for local-only assistant skill/vendor installs. The root `opencode.jsonc` config remains trackable.
- `.playwright-mcp/` for local browser/tooling runtime state.
- `.env`, `.env.*` except `.env.example`, `.envrc`, logs, `.DS_Store`, R local state, and other machine-local files.
- Credential/token files such as `credentials*.json`, `token*.json`, service-account JSON, PEM/key files, and local SSH keys.
- SQLite databases, database sidecars/backups, and serialized R data such as `*.sqlite`, `*.sqlite-*`, `*.db`, `*.db-*`, `*.rds`, and `*.RDS`.
- Browser profiles, raw capture snapshots, queues, downloaded images, raw API outputs, JSONL dumps, and generated runtime state under `data/`.
- App-local runtime data under `apps/human_preview_rating_app/data/`.
- Local Research Library CSV imports, PDFs, parsed text, generated research analysis outputs under `data/research/` and `data/analysis/research_library/`, and Article Lab PDFs under `data/research_pdfs/`.
- Saved website reference exports under `debug_samples/` and copied Medium homepage references under `design_refs/medium_homepage/`.
- Generated analysis outputs under `data/analysis/medium_analysis_v1/` and `data/analysis/medium_analysis_v2/`.
- PaperQA2 chunk retrieval output JSON under `data/research_paperqa_chunks/`.
- Generated scoring and analysis folders such as `data/analysis/title_api_scores_v2/`, `data/analysis/thumbnail_api_scores_v1/`, `data/analysis/title_api_score_samples/`, `data/analysis/title_baseline/`, `data/analysis/title_followup/`, `data/analysis/subtitle_analysis/`, and related output folders.
- Active/private article project work under `article_projects/active/` and per-project `article_projects/*/api_outputs/`.
- Dependency/cache artifacts such as `node_modules/`, `__pycache__/`, `*.pyc`, `cache/`, `tmp/`, `temp/`, `generated/`, and `outputs/`.

Do not delete these files just because they are ignored. Many are useful local state. If a result needs to be shared or documented, create a small curated summary in `docs/` rather than committing the raw generated artifact.
