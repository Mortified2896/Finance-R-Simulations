# ZCode handoff — finish and deploy the Shiny-style Article Lab production port

Continue **PR #11** in `Mortified2896/Finance-R-Simulations`.

Branch: `article-lab/subscription-generation-mvp-20261002`
Implementation commit to continue from: `da5edb371b705952c067cf907e15cda7c938a0ea`

Read these first:
- `docs/article-lab-production-port.md`
- `apps/human_preview_rating_app/app.R`
- `apps/human_preview_rating_app/R/table_helpers.R`
- `apps/human_preview_rating_app/R/ui_helpers.R`
- `apps/human_preview_rating_app/R/workflow_helpers.R`

Do **not** redesign the product again. The Shiny-style production workflow has already been ported. Your job is to integrate, test, fix, deploy and perform live acceptance.

## Product scope already implemented

The Writing area is now split into six stages:

1. **Title Lab**
   - compact candidate table, manual titles, notes, bulk archive/restore
   - direct human approval for subtitles
   - collapsible prompt/model/generation setup
   - **no scoring gate in this pass**

2. **Subtitle Generation**
   - approved titles as generation targets
   - subtitle candidates permanently linked to their title
   - manual subtitles, notes, approve/archive

3. **Thumbnails**
   - title/subtitle packages
   - actual visual image grid, not raw prompt walls
   - concept generation, image generation/upload, one approved thumbnail per package
   - prompt/provenance under collapsed details

4. **Outline**
   - approved package preview with title/subtitle/thumbnail
   - editable outline and notes
   - generate/regenerate alternatives, approve/archive

5. **Full Text**
   - approved package + outline context
   - ChatGPT Pro context export/import
   - MDXEditor draft variants
   - immutable original import, saved edit revisions, approve/reject

6. **Review & Publish**
   - approved article/image preview
   - tags, publication target/name, monetization, status
   - canonical URL, published URL, alt text, image credit, notes
   - copy/export and immutable reviewer snapshot creation

Scoring is **explicitly deferred**. The human editor approves titles directly. Do not add API scoring now.
The flexible Article Agent sidebar is also deferred.
Full-text generation remains manual via ChatGPT Pro for now.

## Architecture already implemented

- Additive migration: `apps/article_lab_web/migrations/0005_production_workflow.sql`
- New domain/API: `shared/production.ts`, `worker/production.ts`
- Existing generation/image/runner implementation preserved in `worker/generation-core.ts`
- `worker/generation.ts` is now only a facade adding production routes
- React production UI lives under `src/production/`
- Existing article IDs, immutable reviewer versions, assignments, comments and auth remain authoritative
- Existing loose results are never auto-parented; adoption must be explicit
- Text generation uses the existing subscription-backed runner routes
- Image generation continues to use the separate OpenAI Image API path
- Production commands use revision checks/idempotency; no silent provider retries

## Checks already completed on the implementation commit

- `node --experimental-strip-types --test tests/article_lab_production/*.test.mjs`
  - 36 passed, 0 failed
- strict standalone TypeScript check of `shared/production.ts` + `worker/production.ts`
  - passed
- authored TS/TSX syntax parsing
  - passed
- static read-only desktop/mobile Chromium layout inspection
  - no horizontal overflow in inspected stages

These are **not** substitutes for the real app build or browser tests.

## Your required work

### 1. Integrate against the real checkout

Use an isolated worktree on the PR branch and preserve unrelated changes and local/private data.

Run from `apps/article_lab_web`:

```sh
npm ci
npm run build
npm test
npm run test:generation
node --experimental-strip-types --test ../../tests/article_lab_production/*.test.mjs
npm run test:worker
npm run test:e2e
npm run format:check
```

Fix all new type/build/integration issues. Do not weaken tests just to make them green. Separate pre-existing failures from regressions.

### 2. Add/repair real browser acceptance for the six-stage workflow

Update the old one-page Writing selectors to the new staged UI.

At minimum test:
- create/open article
- save brief/evidence
- generate/import a title batch
- manual title
- approve title
- generate/manual subtitle
- approve subtitle
- attach/upload/generate at least two image alternatives
- select one thumbnail
- generate/edit/approve outline
- import two draft variants
- edit/save a draft revision
- approve one draft
- enter Review & Publish metadata
- freeze reviewer snapshot
- assign existing synthetic reviewer
- submit a reviewer comment
- verify the old immutable snapshot/comment remains isolated from later drafts

Test both desktop and 390px mobile.

### 3. Verify editor lifecycle carefully

The real MDXEditor lifecycle was not exercised in the authoring environment.

Check specifically:
- polling must not remount or erase a dirty editor
- stage/project/package/variant switches prompt before discarding unsaved changes
- successful saves clear dirty state
- 409/offline/provider failures preserve the user buffer
- cross-tab upstream invalidation must not erase an open dirty draft
- existing `MarkdownComposer` unload behavior may need a saved-baseline prop for production use; preserve legacy callers

### 4. Validate the real D1/Hono/workerd boundary

Exercise the new production facade through the actual local Worker/D1/auth stack.

Verify:
- anonymous/reviewer/pending/disabled users cannot reach admin production routes
- same-origin mutation checks remain enforced
- machine runner token does not grant browser/admin access
- stale revisions and duplicate mutation IDs behave correctly
- cross-article parent/image/draft links fail
- old review/version data is untouched
- the real markdown sanitizer freezes only authorized same-origin image assets

### 5. Revalidate existing live generation routes, do not rebuild provider infrastructure

Use current Control Room/HomeLab runtime evidence.

- Reuse the already configured GLM/Codex subscription lanes
- Do not change O1/O2 defaults
- Do not print, copy or commit credentials
- Do not introduce paid text fallback
- Do not infer aliases from model names; verify actual route/model response aliases
- The production port intentionally keeps reasoning as server-default unless a route reports supported effective controls

Perform only a small live smoke test per enabled route.

### 6. Finish R2/image environment if still blocked

The previous deployment reported:
- R2 bucket/binding not fully enabled because deployment credentials lacked the required narrow R2 permission
- no valid dedicated `OPENAI_IMAGE_API_KEY` on RTX

Resolve only the minimum required scope.

Keep:
- R2 private
- asset delivery behind the app's authorization checks
- RTX non-public
- image API key separate from text/gateway credentials

Then perform one live image generation or upload flow and confirm the visual thumbnail grid works with real bytes.

### 7. Safe production migration/deployment

Before remote migration:
- take a private D1 backup/export
- verify existing users, article versions, reviews and comments

Apply only pending additive migrations, including `0005_production_workflow.sql`.

Deploy through the existing scoped Cloudflare launcher to:
`https://feedback.moneymattersmedia.com`

Retain a rollback build.

### 8. Human acceptance

Verify a real owner Google login and one owner-driven path:

saved brief/evidence
→ GLM titles
→ human title approval
→ subtitles
→ thumbnail/image
→ outline
→ ChatGPT Pro article import
→ draft revision/approval
→ Review & Publish metadata
→ immutable reviewer version
→ existing reviewer assignment/comment flow

Do not seed production users or notify/invite friends.

## Product acceptance criterion

The new interface should feel at least as usable as the old Shiny production workflow:

- compact tables for text candidates
- visual thumbnail cards
- selected package visible while outlining/drafting
- technical model/prompt/provenance information collapsed by default
- alternatives and revisions preserved
- explicit stage progression
- no giant generic candidate-card wall

If the real browser differs materially from those interaction patterns, fix the React/CSS implementation rather than replacing the workflow with another generic UI.

## Final report

Return:
- final commit SHA
- whether PR #11 is ready to merge
- deployed Worker version/URL
- exact test counts and any pre-existing failures
- real owner-login result
- live GLM/Codex/image smoke-test results
- D1 backup/migration verification
- exact remaining blockers

Do not call the port complete solely because it builds. Completion requires the real staged browser workflow and live owner acceptance.
