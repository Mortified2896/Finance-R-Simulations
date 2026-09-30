# O1 handoff: build the article review MVP

Implement the [review MVP](article-review-mvp.md) in
`Mortified2896/Finance-R-Simulations`, under `apps/article_lab_web/`.
This is implementation work, not another architecture-only audit.

## Starting point

The infrastructure dependency is PR #7 at `28bd97b`; the documentation cleanup
branch builds on that commit. Fetch and inspect the actual Git state. Work from a
branch containing both infrastructure and this cleanup, or from main after they
are integrated. There is no need to wait for a merge to start implementation from
the combined branch. Preserve unrelated work; do not reset the RTX checkout.

The repo's root instructions are intentionally short. The Shiny app is currently
unused. It is reference material, not a service you must preserve, and its old
port/prompt/model-control rules do not apply to this app.

## Build and verify

Use the existing RTX credential launcher described in
[Cloudflare development](cloudflare-development.md); do not reinstall authentication
or add token permissions just to repeat completed setup tests.

Build the React/TypeScript frontend, Worker APIs, D1 migrations, and tests. Deliver
Google/email login integration, pending accounts/admin approval, a persistent
article inbox, read-only articles, passage annotations, general feedback, autosave,
submission, and a usable admin publishing/feedback view. Start D1 clean; retain
useful external IDs rather than copying the legacy schema.

Choose sensible libraries and simplify adjacent structure as needed. The MVP
specification states the required behavior, not a fixed component implementation.
Small improvements are welcome; do not make a full Shiny rewrite a prerequisite.

## Coordinate the one-time account setup

The RTX token does not administer Access or Google OAuth. Once the Worker exists,
provide Mac Codex/the owner with its actual name/ID and URLs, required Access
application audience/issuer settings, proposed unused custom hostname, and the
one-time admin bootstrap command. The development guide lists the browser steps.
Do not substitute an unprotected public app or fake production login while waiting.

Then verify both real Google and OTP login, owner approval, account linking,
revocation, hostname/TLS routing, API isolation, and persisted drafts/annotations.
Use synthetic data before those checks pass. A protected preview is useful, but is
not the completed acceptance test.

## Return a working result

Commit/push the implementation and provide the branch/PR, actual deployment URL,
working run/build/test/deploy commands, D1 migration/export instructions, and the
results of the MVP acceptance checks. Separate tests run from untested items and
remaining Mac/browser actions. Report any genuine blocker specifically; continue
independent work rather than redoing infrastructure or asking broad product questions.

Do not claim that another agent has been started merely because this handoff exists.
