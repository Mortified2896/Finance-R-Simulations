# O1 handoff: finish and deploy the MDXEditor review MVP

## Outcome

Finish this implementation and deploy it to the existing
`https://feedback.moneymattersmedia.com` Cloudflare Worker. The owner must be able
to compose/import a Markdown article visually, create a private immutable review
snapshot, assign friends, and inspect their submitted inline comments. A PR or a
local screenshot alone is not the requested outcome.

Repo: `Mortified2896/Finance-R-Simulations`.
Working branch: `codex/mdxeditor-comments-spike`.
Parent: `codex/article-review-mvp` / PR #9 at
`e4c520261fce877437b56be7fea860e88d179772`.
The base remains stacked on PRs #8/#7; do not start from old `main` and accidentally
lose the Cloudflare app. Reconcile any newer work before editing; do not force-push.

Read the short root AGENTS.md and `apps/article_lab_web/README.md`. Do not restart
an editor comparison, rebuild auth, or revive the obsolete Shiny workflow.

## Implemented in this change

- `src/editor/RichMarkdownEditor.tsx`: lazy-loaded MDXEditor 4.3.1, basic prose
  toolbar (H2/H3, bold/italic, lists, quotes and links). No HTML/JSX/component
  authoring and no directly installed Lexical packages.
- `src/editor/MarkdownComposer.tsx`: visual/source/review-preview modes, local
  `.md`/`.markdown` import, `.md` export, parse/load error handling, content limit,
  unsaved-draft warning, and a hidden Markdown field for the existing API contract.
- `src/Admin.tsx`: replaces the Markdown-only form field, prevents publishing an
  invalid draft, and clears the composer only after a successful snapshot POST.
- Preview uses the existing `shared/markdown.ts` sanitizer, not raw editor HTML.
- Unit tests cover draft/import validation. New Playwright tests cover visual
  authoring, private comments, submitted feedback, v1/v2 isolation, import/export
  and failed publication. The older browser fixture now selects the Source tab.

## Important refinement to the earlier plan

MDXEditor is the **authoring surface**, not the review rendering engine. Keep
`Article.tsx`, `ReviewPage.tsx`, frozen `rendered_html`/`anchor_text`, the D1 schema
and the existing server validation. Comments already work on immutable snapshots.
Do not replace that working subsystem merely to add a Lexical plugin.

MDXEditor's current `readOnly` API documentation explicitly recommends rendering
Markdown separately for content consumption:
https://mdxeditor.dev/editor/api/interfaces/MDXEditorProps

This is NOT a claim that MDXEditor has native comments. The application combines
MDXEditor drafting with its existing commentable snapshot view. There are no
comments attached to the live mutable draft and no cross-version re-anchoring.
A later in-editor commenting feature is a separate decision, not a release gate.

## What remains before this is deployable

### 1. Resolve the dependency and lockfile

The editing container had no outbound DNS to GitHub/npm. Consequently the new
`@mdxeditor/editor: 4.3.1` package.json entry is NOT yet reflected in package-lock.
Do not claim `npm ci` or a production build already passed. Do not hand-edit npm
integrity hashes. On RTX, in `apps/article_lab_web`:

```sh
npm install --save-exact @mdxeditor/editor@4.3.1
npm ci
```

Commit the generated app lockfile. Check peer dependencies rather than forcing
or ignoring conflicts. The selected release was verified from the official repo:
https://github.com/mdx-editor/editor/releases/tag/v4.3.1

### 2. Check the real CSP and editor styles

`worker/index.ts` currently sends `style-src 'self'`. Radix-powered toolbar/link
popovers can need inline style attributes. Test the actual Worker response, not
just a relaxed dev page. If needed, make the narrowly scoped addition
`style-src-attr 'unsafe-inline'; style-src-elem 'self'` to that header, leaving
`script-src 'self'`, framing, CSRF, auth, sanitizer, and other protections intact.
Do not add `unsafe-inline` or `unsafe-eval` to scripts. No remote fonts/CDNs are
needed. Do not globally loosen the CSP just to quiet the console.

Inspect the global app button/input styles against MDXEditor toolbar/popovers.
Make scoped CSS fixes. Verify the H2/H3 selector, bold/italic, links, list controls,
source switching, clipboard selection, focus, mobile wrapping, and error recovery.

### 3. Run and fix the checks

```sh
npm run build
npm test
npx playwright install chromium
npm run test:e2e
npm run test:worker
npx prettier --write src/Admin.tsx src/editor tests/markdown-input.test.ts tests/browser/mdxeditor.spec.ts tests/browser/review.spec.ts package.json
npm run format:check
npm audit
git diff --check
```

The new browser tests are authored but have NOT been run. Repair selectors or
real behavior where necessary, without removing coverage. Exercise Chromium and
mobile-size layouts; test Safari/WebKit where available and distinguish browser
engine testing from merely using a mobile viewport.

Test the rendered page for blank/error overlays, console/CSP errors, text/controls
clipping and horizontal overflow. Capture screenshots of the editor, sanitized
preview, submitted editor review, and mobile reviewer. The supplied tests write
synthetic screenshots under `/tmp`; do not commit private article content.

### 4. Preserve privacy and snapshot semantics

Reviewer A must never receive B's feedback in an API response, including HTML,
counts or client-side hidden data. Reviewers have their own assignment/review;
admin can inspect submitted reviews. Keep the `owned()` authorization and
admin-role checks. Test ID guessing directly, not only the UI. Draft feedback
remains private to its reviewer until submission under the existing policy.

Publishing v2 must not edit v1 or move v1 comments. Failed saves must not clear
text. Submissions must still flush autosave, lock the review, and reject stale
writes. Keep the existing immutable triggers and revision/idempotency checks.
No database migration is required by this change.

The compose field is intentionally NOT a persistent working-draft database yet.
Its UI says so and provides export. Ensure failure paths retain the content; do
not describe the compose field as autosaved. A SPA route change can still discard
an unsaved compose buffer; improve the navigation guard only if it can be done
locally without turning this into a draft-sync project.

### 5. Deploy and verify the actual site

Use the existing scoped RTX credential launcher and documented `npm run deploy`.
Do not create another Worker/domain/database, change DNS, expand token scope,
reintroduce Zero Trust, expose the local synthetic-login harness, or alter Google
OAuth secrets casually. There is no schema migration in this change.

Keep the live site functional. Test locally, deploy the new source to the existing
Worker, and verify the actual hostname. Keep the previous known-working deployment
available for rollback if a runtime regression appears; this is not a reason to
leave the requested UI only in an undeployed branch.

Verify owner Google login -> Admin -> visual draft/import -> private snapshot ->
review assignment -> reviewer comment/autosave/reload/submit -> owner inspection.
PR #9 recorded the OAuth app as External/Testing with owner-only acceptance still
pending reviewer verification. Verify current settings. An intended real reviewer
may need to be added to the OAuth test-user list with appropriate authorization.
Never simulate production authorization by deploying a test-login bypass. If no
second identity is available, deploy after automated checks, report that precise
remaining real-user check, and do not falsely mark it verified.

Report the live URL, deployed commit, test results, any remaining limitations, and
steps the owner should click. Finish source-controlled fixes and lockfile before
marking this draft PR ready. Preserve the stacked PR relationship or merge in the
correct dependency order; do not silently omit PR #9.

## Future Article Lab / Omnigent integration boundary

Article Lab owns identities, assignments, immutable review snapshots and feedback.
MDXEditor is an embedded React authoring component, not a separate service.
Omnigent remains the external agent/harness runtime. For now `.md` import/export
is the bridge; there is no fake 'Ask Omnigent' button.

Later expose version-aware draft import/export plus an editor-authorized feedback
bundle with article/version IDs, quotes, comment IDs and editorial decisions.
Omnigent can propose a new Markdown version; only an explicit editor action makes
it a review snapshot or publishes it externally. Reviewer ownership stays
server-enforced. Comments must not be silently resolved by an agent. An aggregate
editor-only view can later join reviews/assignments/users without changing the
basic privacy model. Do not attempt that full integration in this MVP.

## Verification actually performed before this handoff

- Read live PR #9 metadata, current schema/API/rendering and browser-test source.
- Verified MDXEditor v4.3.1 release and the public toolbar/props APIs.
- Strict standalone TypeScript compilation of the dependency-free input helper.
- 12 dependency-free Node checks passed for draft validation and import behavior.
- TypeScript/TSX syntax diagnostics: no errors in the authored implementation at
  handoff. This is NOT dependency-aware typechecking.
- Browser plugin absent. Full Playwright, npm install/build, workerd/auth tests,
  production deployment and real reviewer login were NOT executed here.

Do not upgrade these partial checks into claims of a working deployed editor.
