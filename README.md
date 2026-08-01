# Medium Finance Article Lab

This repository supports a local workflow for researching, developing, generating, evaluating, and preparing finance articles for publication on Medium. Its main interface is a Shiny application that combines the Article Inbox, research and evidence handling, title and asset generation, draft review, publishing preparation, and Medium preview analysis.

## Main Application

The main interface lives in `apps/human_preview_rating_app/`. It uses the local SQLite database at `data/db/medium_articles.sqlite` by default and initializes its required schema when it starts.

Install the required R packages once:

```sh
Rscript -e 'install.packages(c("shiny", "DBI", "RSQLite", "jsonlite", "DT"))'
```

### Stable/default local app

Launch the stable interface with the macOS launcher, either by double-clicking it or from the repository root:

```sh
./01_manual_tools/rating/rate_medium_previews.command
```

Open <http://127.0.0.1:3840/> if the browser does not open automatically.

### Experimental Design v2

Launch the experimental interface with:

```sh
./01_manual_tools/rating/rate_medium_previews_design_v2.command
```

It opens at <http://127.0.0.1:3844/> and enables the experimental UI with `ARTICLE_LAB_UI_VERSION=v2`.

These launchers run the same Shiny application code and currently use the same local database. Work performed in Design v2 is therefore not isolated from the stable interface. Design v2 is intended for interface experimentation and visual review; changes intended for the stable app should ultimately be verified on port `3840`.

## Project Map

- `apps/human_preview_rating_app/`: the main Shiny interface and its app-specific R helpers.
- `scripts/`: reusable collection, import, schema, analysis, scoring, validation, test, and writing workflow scripts.
- `scripts/writing_api/`: OpenAI-backed helpers used by Article Lab for titles, subtitles, thumbnails, outlines, full drafts, tags, and scoring.
- `01_manual_tools/`: double-clickable macOS launchers, bookmarklets, watcher snippets, and manual workflow references.
- `article_projects/`: article briefs, outlines, drafts, style guidance, and active project notes; active/private work is ignored.
- `docs/`: durable documentation for the app, research workflow, analysis pipeline, scripts, and local-data policy.
- `data/`: ignored local databases, source material, captures, downloads, queues, and generated analysis output.
- `.local_gitignored/`: temporary diagnostics, one-off exports, and other scratch artifacts that should remain local.

## Common Commands

Install the root Node dependencies used by writing and browser helpers:

```sh
npm install
```

The repository has one Node dependency scope at the root. Browser collectors that use CommonJS have a `.cjs` extension; writing helpers and tests use the root package's ES-module configuration.

Run focused stable/default local app regression checks:

```sh
npm run test:article-inbox
npm run test:article-production
```

Validate the Medium Analysis V2 objects in the current local database:

```sh
npm run validate:medium-v2
```

If validation reports that required V2 schema objects are missing, and the base Medium import tables already exist, run the setup/repair initializer and then validate again:

```sh
Rscript scripts/apply_medium_analysis_v2_schema.R
npm run validate:medium-v2
```

The initializer backs up the database, creates missing V2 tables and columns, and refreshes the V2 cache indexes and views. It does not delete existing analysis rows and is not a routine prerequisite for validation.

These are common entry points, not a complete test suite. The [script inventory](docs/script_inventory.md) and [manual tools index](01_manual_tools/manual_tools_index.md) cover the more specialized collection, import, scoring, and analysis commands.

## Local Data

Databases, browser profiles, downloaded media, raw API outputs, generated queues and analysis artifacts, credentials, caches, and scratch files must remain local and uncommitted. Use `.local_gitignored/` for temporary diagnostics, exports, and one-off artifacts. See the [local-only files policy](docs/local_only_files.md) for the authoritative list and handling guidance.

## Documentation

- [Contributor and agent instructions](AGENTS.md)
- [Main application guide](docs/human_preview_rating_app.md)
- [Manual tools index](01_manual_tools/manual_tools_index.md)
- [Research and article-development workflow](docs/research_workflow.md)
- [Medium Analysis V2 workflow](docs/medium_analysis_v2.md)
- [Script inventory](docs/script_inventory.md)
- [Local-only files policy](docs/local_only_files.md)
- [Writing API helpers](scripts/writing_api/README.md)

## License

No license file is currently included in the repository.
