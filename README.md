# Finance R Simulations

This repository mixes finance/article-writing experiments with a local Medium data collection and analysis workflow.

Most durable work lives in source code, docs, launchers, and curated article notes. Runtime data is intentionally local-only and ignored by Git.

The root Node package is marked `private` because these scripts are local workflow helpers and are not intended for npm publication.

## Project Map

- `R/`: finance simulation code and a separate Node package used by older Playwright collection helpers.
- `scripts/`: reusable import, collection, scoring, analysis, validation, and writing-helper scripts.
- `scripts/writing_api/`: OpenAI-backed article writing helpers.
- `01_manual_tools/`: double-clickable macOS launchers, bookmarklets, watcher snippets, and manual workflow references.
- `article_projects/`: article briefs, drafts, outlines, style rules, and curated notes.
- `docs/`: durable project documentation and summarized workflow notes.
- `reports/`: small curated reports that are safe to keep in Git.
- `data/`: local SQLite databases, raw captures, browser profiles, queues, downloads, and generated analysis outputs. Treat this as local runtime state unless a specific small summary is curated into `docs/`.
- `.local_gitignored/`: scratch space for temporary diagnostics, one-off exports, and local artifacts.

## Common Commands

Install root Node dependencies:

```sh
npm install
```

Run lightweight JavaScript checks:

```sh
npm run test:tag-bookmarklet
npm run test:search-tags
npm run test:tag-watcher
```

Validate the Medium Analysis V2 database workflow:

```sh
npm run validate:medium-v2
```

Run the Medium Analysis V2 setup directly when needed:

```sh
Rscript scripts/apply_medium_analysis_v2_schema.R
```

Before staging or committing, always inspect:

```sh
git status --short
```

Generated data, SQLite files, browser profiles, queues, raw API outputs, downloaded media, and local scratch files should stay uncommitted.

## More Detail

- [Agent instructions](AGENTS.md)
- [Local-only files](docs/local_only_files.md)
- [Medium Analysis V2](docs/medium_analysis_v2.md)
- [Manual tools index](01_manual_tools/manual_tools_index.md)

## License

No root license is currently declared. Reuse rights are not granted unless a license is added later.
