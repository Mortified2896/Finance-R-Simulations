# Article Review — Article Lab

React + TypeScript / Vite frontend, Hono Worker API, Cloudflare Static Assets,
D1 persistence, and Google OAuth through Better Auth. No HomeLab service or
Tunnel is involved. The Shiny app remains untouched reference material.

Live hostname: **https://feedback.moneymattersmedia.com**.
See [Google setup](GOOGLE_SETUP.md) before inviting reviewers. The deployed
API fails closed until its Google OAuth client is configured.

## Visual drafting and private snapshots

In Admin, enter a title and write in the MDXEditor visual editor or import a
`.md`/`.markdown` file. Switch to Markdown source or Review preview to inspect
the draft; the preview uses the same server sanitizer as the immutable snapshot.
Publish version saves that private snapshot, then Assign a review grants an
approved account access. Submitted inline comments appear under View feedback.
Select an existing Article project when creating v2; v1 and its reviews stay fixed.

The mutable article draft is **not autosaved**. Failed publication preserves it,
and leaving Admin, opening submitted feedback, signing out or closing the page
warns before discarding unsaved content. Export .md before leaving to retain a
working copy. Reviewer feedback retains its existing D1 autosave. Markdown
import/export is the current Omnigent bridge; there is no integration API yet.

MDXEditor is pinned to 4.3.1 and loaded only when drafting. The Worker keeps
`script-src 'self'` and authorizes Radix's dynamic scroll-lock stylesheet with a
fresh CSS-only nonce in non-cacheable HTML. Its fixed Select viewport stylesheet
has an exact CSP hash; toolbar browser tests detect dependency changes that break
that policy. No arbitrary inline stylesheet or script permission is enabled.

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
header can turn this harness on. `worker/index.ts` always exports the Better Auth
session-validated application. Never expose the local harness through a reverse proxy.

`npm run dev` instead runs the actual Worker with Better Auth. Use ignored
`.dev.vars` for local OAuth secrets, an explicitly registered local callback and
base URL, and local D1 migrations. The loopback synthetic harness requires no
Google secrets; it is deliberately separate from production login.

## Checks

```sh
npm run build        # strict TypeScript + frontend bundle
npm test             # real local D1 + Better Auth session/OAuth tests
npm run test:e2e      # Playwright desktop/mobile reviewer/admin flow
npm run test:worker  # bundled auth runs in actual workerd + D1
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

`migrations/0001_review.sql` defines the review domain; `0002_better_auth.sql`
adds Better Auth's generated `auth_user`, `auth_account`, `auth_session`,
`auth_verification` (OAuth state, not email OTP), and `auth_rate_limit` tables.
The application `users.auth_user_id` links to the auth identity. Review ownership
continues to use independent internal UUIDs, so providers can change without
rewriting review foreign keys. The standard auth account schema includes an
unused nullable password field; password authentication is disabled.

Better Auth 1.7.6 uses its native D1/Kysely adapter inside the Worker. Sessions
last 30 days, refresh after a day, use Secure/HttpOnly/SameSite=Lax host cookies,
and are checked against D1 rather than cookie-cached. Its normal OAuth state,
CSRF and trusted-origin checks are enabled explicitly. Google tokens are encrypted
at rest. Native schema generation is reproducible with
`npx tsx tools/generate-auth-schema.ts`; migrations are applied through Wrangler,
never automatically during a public request. Normal session refresh cookies are
forwarded from the API to the browser.

Every protected API resolves the Better Auth session server-side, requires a
verified email, and reads current application approval/role. Browser identity
headers and the former Access headers are ignored. Login never resets approval
or rejection. Admin can approve/reject/disable/re-enable reviewers; only explicit
bootstrap can establish the owner admin. Account deletion and provider linking
UI are outside this MVP.

Mutation requests require same-origin JSON. Assignment ownership, approved status
and admin roles are enforced server-side. Queries bind parameters. Request size
and editable fields are bounded; failures stay visible without stack traces.
CSP, no-store API responses, framing and MIME protections apply. Auth endpoints
use Better Auth's D1-backed rate limits. No auth/deployment secret enters React.

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
- `worker/`: Better Auth sessions, validated API, D1 authorization/transactions.
- `shared/`: API shapes and safe version rendering.
- `migrations/`: source-controlled D1 schema.
- `tools/local.ts`: isolated synthetic identity harness.
- `tests/`: cryptographic, D1 and browser verification.
