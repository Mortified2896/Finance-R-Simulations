# Agent instructions

Article Lab is moving to `apps/article_lab_web/`. The Shiny app is currently
unused and is reference material, not a compatibility requirement. Prefer the
requested product direction over preserving its architecture or workflows.

Keep secrets and private runtime data out of Git; small synthetic test fixtures
are fine. Preserve unrelated work and non-reproducible data. An unused app does
not make its local datasets disposable.

Run relevant checks and report their actual results. Distinguish local tests,
live deployment, and real login verification.

See [architecture](docs/architecture.md) and the relevant app README for context.
Historical documents describe previous implementations; they do not impose
requirements on the new app. Keep this file short and repo-wide.
