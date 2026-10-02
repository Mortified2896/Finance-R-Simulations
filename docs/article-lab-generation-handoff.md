# Article Lab subscription-generation MVP — ZCode continuation

## Status and scope

Branch: `article-lab/subscription-generation-mvp-20261002`.
Base inspected: `aa8076a10e882696cd05f0180c53dfc7591f8dd9`.
This is an implemented backend foundation, NOT a finished or deployed UI MVP.
Do not replace it with another architecture/planning exercise.

The user wants the Shiny app to be strong functional/UI guidance for a fast MVP,
without reproducing its architecture. Reuse resolved prompts, candidate batches,
generate/regenerate, select/promote, archive/restore and article-stage context.
No flexible agent/chat sidebar, autonomous orchestration, diffs/proposals,
agent Accept/Reject interface, broad refactor, or benchmark platform in this MVP.

Text helpers use the user's EXISTING Codex/GLM subscription lanes. Full article
writing remains in ChatGPT Pro, with prompt/context export and draft import.
Image concepts are text. Actual image pixels use a separate OpenAI Image API key.
Do not introduce a new ChatGPT-plan-sharing/OAuth integration for this task.

## Implemented

- `shared/generation.ts`: dependency-free request/catalog/output contracts;
  editable resolved prompts; title/subtitle/concept/outline candidates; 140-character
  title cap and legacy 40–75-character guidance; no truncation of overlong output.
- `migrations/0003_generation_foundation.sql`: workspaces on the SAME article IDs,
  safe-save revisions, jobs, immutable generated alternatives, selections and
  recoverable candidate archival. Existing versions, assignments and reviews are
  not changed. No private data migration or destructive legacy cleanup is included.
- `worker/generation.ts`: admin workspace/save/queue/select/archive APIs;
  machine-only claim/complete/fail APIs; atomic queue admission/claims, bounded
  input, request idempotency, qualified returned-model checks, guarded completion,
  queue caps and explicit cancellation of queued jobs. Expired in-flight work
  becomes `uncertain`, never automatically queued again.
- `worker/index.ts`: registers the machine namespace before browser session
  middleware and admin namespace AFTER existing Better Auth/approval/admin guards.
  The machine token never authorizes any other endpoint. CSP and review APIs are
  unchanged. Generation enqueue/claim fails closed without configuration.
- `scripts/article_lab_worker/providers.mjs`: GLM Coding subscription direct and
  GLM via local OmniRoute Chat Completions; Codex subscription via local OmniRoute
  Responses. Exact configured model/response aliases, no provider fallback, no
  shell/tool execution, no generic OpenAI text API credentials. A separately
  configured `generateImage` adapter returns bounded PNG bytes from one Images
  API request; it is NOT yet wired to a Cloudflare image route, R2 or a UI.
- `scripts/article_lab_worker/runner.mjs`: outbound HTTPS polling from RTX,
  single dispatch, private completion spooling, replay of COMPLETION ONLY after
  an upload failure. It does not expose an RTX public endpoint. Spool files contain
  private article content and a lease token: outside Git, directory 0700/files 0600.
- `config.example.json`: placeholders and examples, NOT discovered live settings.
  No credentials are included. Native direct Codex CLI execution is not implemented;
  the current first Codex adapter reuses the existing OmniRoute subscription lane.
- `npm run test:generation`: 41 tests using Node SQLite + mocked provider/HTTP
  responses. Full build, Hono/workerd integration, real Google login, live provider
  calls, remote migrations/R2 and deployment have NOT been run in this pass.

Checks actually run and passed in the authoring container:

```sh
node --experimental-strip-types --test tests/article_lab_generation/*.test.mjs
# 41 passed, 0 failed

tsc --noEmit --strict --target ES2022 --module ESNext \
  --moduleResolution bundler --allowImportingTsExtensions --lib ES2022,DOM \
  apps/article_lab_web/shared/generation.ts apps/article_lab_web/worker/generation.ts
```

The test fixture reads the repository's real `0001_review.sql` and new migration.
This is SQLite contract testing, not a claim of production D1 validation. The
existing Worker source was reconstructed and checked against its original blob
`24bc177d3b36c723905d2f7a4f395028f7403e03` before adding only the route mounts/env
fields/import. No full React app build was possible in the authoring container.

## Source locations checked

Finance repo: `AGENTS.md`, web app README/package/tsconfig, existing Worker API,
`migrations/0001_review.sql`, Shiny README and `scripts/writing_api/generate_titles.mjs`.
The old title script consumes `resolved_prompt`; that contract is preserved.
Before building the screens, inspect the actual corresponding `app.R`/R helpers
and subtitle/thumbnail/outline scripts too. The entire Shiny UI has NOT been
ported or exhaustively audited in this foundation pass.

Current Control Room is `Mortified2896/omnigent`, NOT the archived
`control-room-standalone` application. In particular read
`docs/model-advisor-provider-groups.md`: it documents qualified `codex-direct`,
`glm-direct`, Codex OAuth via OmniRoute and GLM Coding Plan via OmniRoute;
provider/model and transport are distinct. `Mortified2896/HomeLab` owns deployment
and `Mortified2896/omniroute-customizations` owns routing overlays.
Revalidate the relevant live bindings; do not infer aliases by stripping prefixes.

## Finish the functional MVP

### 1. Worktree, real build and integration checks

Continue this branch in a clean isolated worktree. Preserve unrelated work and
existing local datasets. Install locked dependencies in `apps/article_lab_web`,
run generation tests and the existing app checks. Format changed TS/JS files with
the repo's pinned Prettier; it was unavailable in the authoring container.

Extend the actual Hono + local workerd/D1 tests to cover both newly mounted
namespaces: anonymous/reviewer/pending/disabled access, same-origin enforcement,
machine-token scope, queue idempotency, claim competition, stale saves and stale
completion. Confirm the local harness applies all migrations in order. Do not
weaken Google authentication or add a production synthetic-login switch.

### 2. Shiny-guided editor UI on the existing Cloudflare site

Build a functional editor workspace using the existing admin navigation, API
helper and MDXEditor, not a new application. Add/open a workspace on an existing
article ID or create one before any draft exists. Show brief/evidence, titles,
subtitles, thumbnail concepts, outline and draft. Keep stage navigation flexible.

For generated text: editable resolved prompt, configured route/model selection,
candidate count, Generate/Generate more, visible queued/running/succeeded/failed/
uncertain states, Select, Archive and Restore. Label archival as recoverable.
Generated alternatives must never silently replace the chosen item or editor.
Use one stable client-generated UUID per enqueue attempt; retrying transport must
reuse it. A deliberate new generation gets a new UUID. No automatic replay after
provider timeouts/quota errors. Display unknown runner availability honestly:
`runner_configured` means a token exists, not that a daemon is alive. Add a narrow
heartbeat/availability signal before claiming live readiness in the UI.

Wire explicit safe saves first. Keep unsaved edits on 409/offline/errors, warn on
navigation and serialize/coalesce saves if adding autosave. Poll only while jobs
are active; do not wipe unsaved form fields when refreshing candidates. Persist
selected candidates and restore state after reload. Permit manual title/subtitle
editing without mutating historical generated output (store an editorial override
or a separate manual candidate). Keep reviewer draft comments private.

Export context for ChatGPT Pro using the saved brief/evidence/selections/outline.
Import/paste the returned Markdown draft into MDXEditor. Publish a NEW immutable
snapshot through the existing versions endpoint with the SAME article_id; never
mutate old versions or remap old reviewer anchors. Existing reviewer accounts
continue to see only their assigned reviews, not generation controls/settings.

### 3. Reuse live RTX subscription routes, do not rebuild Omnigent

Inspect only the relevant current Control Room/HomeLab route/config/credential
references on RTX. Reuse GLM direct and GLM/Codex OmniRoute subscriptions. Do not
print or commit secrets, copy credential stores into the Finance repo, change
O1/O2 defaults, restart Omnigent or broaden public exposure for this task.

Create private runner config outside the repo using `config.example.json` as a
shape only. Discover the actual loopback OmniRoute port, available subscribed
models and exact response aliases. The current adapter deliberately accepts
only a loopback OmniRoute URL and the GLM Coding endpoint, not the general GLM
balance endpoint. If the actual trusted routing arrangement differs, make the
smallest documented allowlist change, never accept an endpoint from job input.

Set credential environment-variable references to the existing securely supplied
credentials. Text config cannot use `OPENAI_API_KEY`. That name is reserved for
separately billed image calls. Verify OmniRoute itself has no unintended
cross-provider/PAYG fallback; the local adapter does not attempt fallback but
cannot attest a gateway's internal billing configuration by model text alone.

Set `ARTICLE_LAB_RUNNER_TOKEN` to a new narrowly scoped random secret shared only
between this Worker namespace and RTX runner. It is not a replacement for any
existing provider/Cloudflare secret. Set `ARTICLE_LAB_ROUTES` on Cloudflare to the
public route fields normalized by `generationRoutes` (NO URLs or keys). Keep the
same ordered response aliases in worker and runner config.

Use the existing HomeLab service pattern for an isolated, least-privilege RTX
runner, bound only by outbound HTTPS/local gateway access. Private spool path
must be absolute and outside the repository. No Tunnel/Funnel/public RTX listener.
Start with `--once`, then install polling after live checks. On completion upload
failure use `--replay-completion`: it uploads the saved result only and never
regenerates. An expired lease may reject replay; preserve the private result for
manual editorial recovery, do not silently retry the provider request.

### 4. Images and R2

Use `generateImage` as the API adapter, configured with an image model verified
available to the existing OpenAI API account. Do not copy a model slug blindly
from prior chat or the fixture. Enforce server-side allowed models, image sizes,
quality, one image per initial request, explicit Generate action and clear API
billing labeling. No automatic image retries/fallbacks after ambiguous failures.

Add the missing image job/asset flow and private R2 binding. Store bytes in R2,
metadata/provenance/alt text/caption/selections in D1. Validate actual image format
and size/dimensions before storage, not merely a filename or Content-Type. The
adapter's PNG signature check is not a complete decoder/asset-validation layer.
Support upload/drag-and-drop and multiple candidates. Preserve all alternatives.

The current review renderer excludes images. Extend it narrowly to same-origin,
authenticated app assets referenced by the specific immutable snapshot. Enforce
admin or assignment-to-that-version access, not just an unguessable asset URL.
Keep R2 private, reject SVG/raw HTML/arbitrary remote fetch URLs, preserve CSP and
existing text anchors. Do not expose a mutable draft thumbnail to a reviewer of
an earlier frozen version. No public R2 bucket or broad CSP relaxation.

### 5. End-to-end acceptance and deployment

One owner-created article must travel through:
workspace -> saved brief/evidence -> title candidates/selection -> subtitle ->
thumbnail concept -> actual image or upload -> outline -> ChatGPT Pro draft
import -> MDXEditor -> new immutable version -> assigned reviewer comments.
Refresh and return to verify persistence. Test desktop and mobile, request errors,
403/409, selected-candidate safety, archive/restore, provider/model provenance,
no duplicate invocation and historical reviewer anchors.

Run `npm run build`, `npm test`, `npm run test:generation`, `npm run test:e2e`,
`npm run test:worker` and format checks. Separate pre-existing failures from new
regressions; do not remove meaningful tests to obtain green status.

Use the Finance repo's existing secure Cloudflare launcher/scoped credentials.
Take an appropriate private D1 backup/export before remote migration, preserve
existing reviewer data, apply only additive migrations and keep a rollback build.
Confirm required R2 permissions/binding rather than silently widening credentials.
Deploy to the same `feedback.moneymattersmedia.com` hostname. Verify actual owner
Google login; distinguish local synthetic reviewer tests from real reviewer login.
Do not seed fake production accounts, publicly publish test articles, or notify
friends without a separate request. Perform one small live request per admitted
text route and one explicitly configured image smoke test; no benchmark batch.

Return the PR/commit, deployed build/URL, actual checks and exact remaining
blockers. Do not describe the UI as working based only on a build or this foundation.
