# Article Lab

Finance-content research, generation, evaluation, and human feedback in one
repository: `Mortified2896/Finance-R-Simulations`.

## Direction and actual status

The next application lives in [`apps/article_lab_web/`](apps/article_lab_web/README.md)
and targets React, TypeScript, Cloudflare Workers, and D1. Its first usable
workflow is a persistent reviewer portal: Google or email-code login, owner
approval, an article inbox, passage comments, general feedback, and saved drafts.

**Implemented so far:** project-local Wrangler tooling and an RTX credential
launcher. The setup report records successful live Worker/D1 smoke tests.
**Not implemented yet:** the React application, production D1 schema, reviewer
login/approval, and feedback UI. Configuration is not a working product.

The existing [`Shiny Article Lab`](apps/human_preview_rating_app/README.md) is
currently unused. It remains available as a reference and source of useful
functionality, not a production service that every change must preserve.
Useful generation and Medium-success analysis features can move over as the new
app develops. There is no requirement for a full port or a second repository.

## Start here

- [Architecture and migration direction](docs/architecture.md)
- [Article review MVP](docs/article-review-mvp.md)
- [Cloudflare development and authentication setup](docs/cloudflare-development.md)
- [O1 implementation handoff](docs/o1-review-mvp-handoff.md)
- [Documentation index: current, supporting, and historical](docs/README.md)

## Cloudflare tooling

On the configured RTX checkout, as `hermes`:

```sh
cd /home/hermes/workspace/repos/Finance-R-Simulations/apps/article_lab_web
npm ci
npm run cf -- whoami
npm run cf -- d1 list
```

Use the credential launcher for authenticated commands; bare Wrangler does not
load the project's private credential file. No frontend `dev`, build, or deploy
script is claimed here until O1 implements and verifies it.

## Repository map

| Location | Purpose |
| --- | --- |
| `apps/article_lab_web/` | New web application and its independent Node tooling |
| `apps/human_preview_rating_app/` | Legacy Shiny UI and R helpers |
| `scripts/` | Existing collection, import, analysis, scoring, and writing helpers |
| `01_manual_tools/` | Existing Mac launchers and browser/manual utilities |
| `docs/` | Current direction, operating notes, methodology, and historical references |
| `data/`, `article_projects/`, `.local_gitignored/` | Ignored local/runtime material where present |

The root `package.json` and lockfile belong to the existing Node helpers.
`apps/article_lab_web/` has its own dependency scope and lockfile. Install in the
package you are working on rather than mixing the two environments.

The [legacy app README](apps/human_preview_rating_app/README.md) contains its run
and test commands. The [script inventory](docs/script_inventory.md) maps existing
analysis tools; their existence does not require a Worker to run them.

## Data and licensing

Keep private article drafts, reviewer data, credentials, database exports,
downloads, and runtime state out of Git. Source-controlled schemas, configuration,
and synthetic test fixtures belong in Git. See [runtime data](docs/local_only_files.md).

No root license is currently declared; see [repository notes](PUBLIC_REPO_NOTES.md).
