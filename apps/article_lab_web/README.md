# Article Review — Article Lab

React + TypeScript / Vite frontend, Hono Worker API, Cloudflare Static Assets,
D1 persistence, and Cloudflare Access authentication. No HomeLab service or
Tunnel is involved. The Shiny app remains untouched reference material.

Live hostname: **https://feedback.moneymattersmedia.com**.
See [the login handoff](ACCESS_SETUP.md) before inviting reviewers. The deployed
API fails closed until its Access issuer/audience are configured.

## Local development

Node 22+; dependencies are locked in `package-lock.json`.

```sh
npm ci
npm run dev:local
# Open http://127.0.0.1:5173/__local
```

The Node harness binds **only 127.0.0.1**, displays LOCAL DEVELOPMENT ONLY on its
account picker, and offers synthetic owner/Jane/Paul identities. It runs the
same API against local workerd D1 stored in ignored `.local/d1`. It is not the
production entry point; no deployed flag, environment variable, cookie, or
header can turn this harness on. `worker/index.ts` always exports the Access-
validated application. Never expose the local harness through a reverse proxy.

`npm run dev` instead runs Wrangler with real JWT validation. Configure ignored
`.dev.vars` with the Access settings if using that mode, and apply the migration
locally with `npx wrangler d1 migrations apply article-lab-review --local`.

## Checks

```sh
npm run build        # strict TypeScript + frontend bundle
npm test             # real local D1 + cryptographic JWT tests
npm run test:e2e      # Playwright desktop/mobile reviewer/admin flow
npm run format:check
npm audit
```

Install Chromium once with `npx playwright install chromium`. Browser tests use
only the loopback harness, never production. Runtime fixtures are local-only.
Screenshots go to `/tmp/article-review-*.png`; build output, local databases,
Playwright artifacts, credentials, and raw outputs are ignored.

## Deployment

```sh
npm run cf -- d1 migrations apply article-lab-review --remote
npm run deploy
python3 tools/cloudflare.py review-preflight
```

The existing secure launcher supplies the installed scoped credential only to
Wrangler, and its constrained preflight performs read-only resource inventory.
It never prints the token. Normal CLI results are enabled; request/debug logs
are disabled. No token replacement or broader permissions are necessary.

`wrangler.jsonc` identifies the production D1 database `article-lab-review` and
Worker custom domain. The production database starts empty: synthetic test
articles and identities are **not** deployed. Local D1 is the development
resource; no additional remote development database is needed for this MVP.

## Data and security

`migrations/0001_review.sql` creates users, Access identity links, articles,
immutable article versions, assignments, reviews, and annotations. Internal
UUIDs are foreign keys; normalized verified email merges Google/OTP accounts.
Display name, creation/approval/login times, independent role/status, and Access
subject/issuer links are retained. Login never resets a rejected/disabled account.
Admin can approve/reject/disable/re-enable reviewers. Role elevation is restricted
to explicit owner bootstrap, not a public UI.

Every API request validates an RS256 Access JWT, trusted team issuer, application
audience, expiration and identity claims using `jose` and Cloudflare's rotating
JWKS. Browser email headers are ignored. Mutation requests require same-origin
JSON. Authorization is enforced in the Worker, including assignment ownership,
approved status and admin roles. Queries bind parameters. Request size and all
editable fields are bounded; failures return persistent UI errors without stack
traces. CSP, no-store API responses, framing and MIME protections apply.

Markdown is parsed without raw HTML, sanitized to a safe HTML tree, and stored
alongside its canonical DOM text. Images/embeds are excluded; unsafe links and
scripts cannot render. Rendering is frozen with each version, so future library
changes cannot shift old anchors. New content means a new immutable version.

Anchors use W3C-style text quote/position selectors: exact quote, prefix/suffix
(up to 64 UTF-16 units), start/end offsets, version ID, comment, and timestamps.
The Worker verifies the full selector against the saved version. Highlights
work across formatted text nodes and overlapping selections. Click/keyboard-
activate a highlight to find its comment. No cross-version reanchoring is needed.

A review is created when its assignment opens. Drafts save to D1 after 450ms of
idle time. Client writes are serialized and coalesced, with idempotent mutation
UUIDs and optimistic revision checks. A transactional D1 batch applies the
review and annotations together only when the revision matches. Stale device
writes receive 409 and cannot overwrite newer work. The UI preserves unsaved
input, shows a persistent error and offers retry; copy unsaved text before
reloading a conflict. Back and submission await saves; visibility change flushes,
and closing with unsaved changes prompts the browser. Only **All changes saved**
means D1 acknowledged the draft. Sudden offline device loss before acknowledgment
cannot be guaranteed; browser unload requests are best-effort.

Submission locks the review and its annotations, records its timestamp, and is
idempotent. Admin can view submitted feedback with highlights, quote/comment
pairs, general feedback, reviewer and version. Reopening submitted reviews,
assignment revocation, AI generation, and legacy workflow migration are deferred.

## Main paths

- `src/`: inbox, review, admin, DOM anchoring, serialized autosave.
- `worker/`: Access verification, validated API, D1 authorization/transactions.
- `shared/`: API shapes and safe version rendering.
- `migrations/`: source-controlled D1 schema.
- `tools/local.ts`: isolated synthetic identity harness.
- `tests/`: cryptographic, D1 and browser verification.
