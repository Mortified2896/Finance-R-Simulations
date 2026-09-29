# Article Lab direction

Product decision, 2026-09-29. This describes the target, not an already deployed app.

## One repository, a new primary application

`apps/article_lab_web/` will become the Article Lab web application. Its target
stack is React + TypeScript, a Cloudflare Worker API, and D1. The same app can run
locally during development and be deployed to Cloudflare.

The Shiny app is currently unused. Reuse helpful domain concepts, analysis,
formatting, and generation logic; do not copy its architecture or accumulated
SQLite schema automatically. Refactoring or replacing adjacent legacy code is
fine when it helps the new direction. Keeping old ports, screens, and workflows
working is not an acceptance criterion. Existing datasets are a separate concern.

## First product outcome

A reviewer returns to one website, signs in with Google or an email code, and
requests access. The owner approves the account once. Approved reviewers see the
articles made available to them, comment on selected passages, leave general
feedback, resume drafts, and submit. The owner publishes articles and reads
submitted feedback. See the [MVP](article-review-mvp.md).

```text
Browser -> Cloudflare Access -> React UI / Worker API -> D1
                                  |                      |
                            verified identity      account approval,
                                                   articles, feedback

RTX: development/deployment machine, not the website's public origin
Shiny + R/Python/Node tools: reuse/reference; no mandatory runtime dependency
```

The browser and Worker do not receive the RTX deployment credential. Access
proves identity; the Worker checks D1 approval, role, and article visibility.
Pending accounts cannot read article data. Account approval outlives a login session.

## Boundaries that matter

Use fresh, versioned article/review storage, keeping external IDs where useful.
Do not copy private data into static frontend bundles. The reviewer portal must
operate without public HomeLab ingress. Add storage or compute services when a
real feature needs them rather than pre-provisioning the whole Cloudflare catalog.

The feedback workflow is a priority, not a ban on sensible improvements. AI
interviews, generation workflows, and Medium-success evaluation remain possible
extensions; they are not prerequisites for the first usable portal.

## What exists

PR #7 provides deployment tooling and recorded infrastructure verification only.
The [development guide](cloudflare-development.md) separates those results from
unconfigured login/domain settings. The [O1 handoff](o1-review-mvp-handoff.md)
turns this direction into the next implementation task.
