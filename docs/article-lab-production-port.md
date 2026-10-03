# Shiny production workflow port — 2026-10-03

## Status and branch

Continue PR #11, `article-lab/subscription-generation-mvp-20261002`.
This port was built against `b15f5ab4956bd8daaf7719a517d757c569c02f57`, after
ZCode's initial deployment. This document supersedes the old handoff's product
scope; it does not claim that this new interface has been deployed.

The user approved six production stages: **Title Lab → Subtitle Generation →
Thumbnails → Outline → Full Text → Review & Publish**. Scoring and the flexible
agent sidebar are explicitly deferred. Full article writing remains manual in
ChatGPT Pro; imports become independent draft variants here.

## Implemented product port

The old single scrolling `GenerationWorkspace.tsx` is replaced by a small
entry-point re-export and a `src/production/` component tree. One stage is visible
at a time, with persistent article selection, counts, a compact brief/evidence
editor, hash-based article/stage restoration and guarded navigation.

| Source in `apps/human_preview_rating_app/R/table_helpers.R` | React counterpart |
| --- | --- |
| `article_lab_generate_table_ui` | `TitleLab.tsx`, compact checkboxes/title/length/status/source/notes table, bulk approval/archive/restore, manual input |
| `article_lab_subtitle_target_table_ui`, `article_lab_subtitle_candidate_table_ui` | `SubtitleStage.tsx`, approved input titles, per-title generation jobs, linked subtitle alternatives and manual subtitles |
| `article_lab_thumbnail_package_table_ui`, `article_lab_thumbnail_candidate_grid_ui` | `ThumbnailStage.tsx`, explicit package picker, actual image preview grid, one approved thumbnail per package, review notes, alt text and collapsed provenance |
| `article_lab_ready_for_outline_table_ui` | `OutlineStage.tsx`, approved package preview plus editable outline/notes, alternative selection and approval |
| `article_lab_full_text_table_ui` | `FullTextStage.tsx`, package/outline context, Markdown import into MDXEditor, separate variants, original text and immutable saved revisions |
| `article_lab_review_publish_workspace_ui` | `ReviewPublishStage.tsx`, approved preview, Medium fields, private reviewer snapshot, text copy/export and image download |

`app.R`, `ui_helpers.R`, `workflow_helpers.R`, `prompt_template_helpers.R` and
`article_lab_config.R` supply the workflow/reference patterns. This is a product
port, not a claim of pixel-identical rendering or full legacy-app parity. No
research inbox, PaperQA, blind-rating experiment or scoring engine was added.

### Meaningful behavior, not only layout

- Titles may be approved directly for subtitles; no scoring gate exists.
- Subtitles belong to a particular title. Their approved IDs define packages.
- Concepts, images, outlines and drafts stay attached to those packages.
- Original generated candidates remain immutable in the existing tables.
  `production_items` holds separate editable editorial copies and provenance.
- Manual candidates enter the same workflow. Title limit is 140 characters;
  subtitle editorial limit is the Shiny 90 characters. Existing overlong model
  outputs can be imported and edited, but cannot be approved until shortened.
- One thumbnail and one approved outline/draft per package. Other alternatives
  remain available. Changed upstream text reopens dependent work for review.
- Resolved text requests now include the SAVED topic, brief, evidence and exact
  parent/package context on the server. The UI previews the same context and
  provider wrapper. No generic placeholder context is substituted.
- Named prompts and generation preferences are stored per article/stage. The
  current adapters keep server-default reasoning; capability-aware reasoning
  controls are not invented.
- Draft import preserves the original, manual saves record before/after bodies
  and notes, and approval does not delete sibling variants. Revisions can be
  loaded into the editor and saved as another revision.
- Publishing metadata includes tags, publication name, target, monetization,
  status, canonical/published URLs, alt text, credit and notes. Status timestamps
  persist. There is no automatic Medium publishing or tag-generation pipeline.
- Reviewer snapshots use the EXISTING renderer, `article_versions`, assignments
  and `version_assets`. Reviewer comments continue to target immutable versions.
- Export does not pretend private `/api/assets/` links work on Medium: private
  image placements are marked and the featured image has an authenticated
  download. Images must be uploaded to Medium separately.

## Architecture and migration

`0005_production_workflow.sql` is additive. It does not rewrite existing Shiny,
Cloudflare generation, image, reviewer or authentication rows. Existing loose
workspace results appear in an explicit adoption tray; the user assigns the
correct parent. Old image assets can be attached to a package. The old working
draft can be imported as a new variant. Relationships are never guessed.

`worker/generation-core.ts` is the **byte-identical former generation.ts** from
the inspected base (blob `af3f848781f786649bc1159d42035ec1c7eac641`). The new
`worker/generation.ts` is a facade: it adds `/api/admin/lab/production/...`, then
delegates existing routes and re-exports runner/asset handlers. `worker/index.ts`,
provider adapters, runner service and deployment bindings are unchanged.

`worker/production.ts` adds revision-checked, idempotent editorial commands. Each
command uses one D1 batch and a fresh server attempt token, separate from the
client mutation ID. Concurrent replay cannot repeat sibling writes. Foreign
article links, stale approvals, malformed image references and unsafe URLs are
rejected. Originals/revisions and one-approved constraints have DB enforcement.
The new API queues the same `generation_jobs` / `image_jobs` consumed by the
existing RTX runner, with a `production_job_targets` provenance link.

The UI retains failed actions for **Retry same action** with the same ID. Explicit
new generation has a new ID; no automatic model retry is introduced. Dirty forms
retain buffers across requests. Result synchronization is bounded (12 text + 4
image results per command), with separate follow-up requests when needed. Large
linked bulk actions are rejected before writing rather than partially applied.
D1's documented Free invocation/query limits still need validation through the
real authenticated Worker, including auth middleware overhead; fixture tests are
not a substitute for that check.

## Checks actually performed

```sh
node --experimental-strip-types --test tests/article_lab_production/*.test.mjs
# 36 passed, 0 failed

tsc --noEmit --strict --target ES2022 --module ESNext \
  --moduleResolution bundler --allowImportingTsExtensions --lib ES2022,DOM \
  apps/article_lab_web/shared/production.ts \
  apps/article_lab_web/worker/production.ts
# passed
```

The Node tests execute actual SQL against Node SQLite using byte-identical
baseline migrations 0001/0003/0004 plus 0005. They cover stage transitions,
parent isolation, manual candidates, originals, approvals, stale context, atomic
bulk failures, concurrent duplicate/distinct commands, queue targets, result
sync, drafts/revisions, metadata and immutable snapshots. Snapshot rendering in
these tests is an explicit fixture stub, NOT the real markdown sanitizer.

All authored TS/TSX parsed without syntax errors. A supplemental cross-component
contract check used minimal React type declarations and the existing composer
signature; that is NOT a full application/dependency type check.

Static markup from the actual JSX/CSS was rendered with synthetic state and
inspected in Chromium at 1440px and 390px. Titles, subtitles, thumbnails and
outlines had no document horizontal overflow. This used a read-only JSX harness,
NOT actual React hooks, authentication, provider calls or end-to-end interaction.
Those QA-only stubs and synthetic HTML are outside the repository and are not
part of the app. No live browser or deployment claim follows from them.

A full checkout/dependency install was blocked by network access in the authoring
container. Full Vite build, pinned Prettier, existing Vitest/workerd tests, actual
React Playwright tests, production migrations, Google login and live model/image
calls have **not** been run for this port. Do not describe them as passing.

## ZCode finishing checklist

1. Continue this branch in an isolated worktree; preserve unrelated changes.
   Install locked app dependencies. Format the changed files with pinned
   Prettier. Run the full app build immediately and fix any integration/type
   errors against the actual dependencies.
2. Run the existing generation and app suites, plus the 36 new Node tests.
   Exercise the production facade through real Hono/workerd/D1 auth, not only the
   structural SQLite fixture. Verify reviewer/pending/anonymous denial, scoped
   runner tokens, origin checks, D1 limits and the real sanitized image snapshot.
3. Update old long-page Playwright selectors instead of weakening their
   assertions. Add six-stage acceptance with 12 titles, multiple title/subtitle
   packages, two image alternatives, outline edits, two draft variants, revision
   restore, publish metadata and private reviewer snapshot/assignment.
4. Inspect actual desktop/mobile rendering. The intended reference is the Shiny
   table/package/image-grid interaction, not the discarded giant candidate-card
   page. Keep technical details collapsed. Fix real CSS collisions rather than
   rebuilding the product design.
5. Pay particular attention to real editor lifecycles: polling must not remount
   a dirty MDXEditor; stage/project/package/variant switches must prompt before
   discarding; successful saves must clear dirty state. The existing
   MarkdownComposer still treats nonempty content as unsaved for its own unload
   warning. Introduce an optional saved-baseline prop for production use while
   preserving its legacy callers. Test cross-tab upstream invalidation while an
   editor is dirty; keep its buffer visible even when an approved parent loses
   approval. These cases require real React browser verification.
6. Verify live model/route IDs using current Control Room/HomeLab, without
   reconfiguring O1/O2 or provider credentials. Text still uses existing
   subscription lanes. New model-capability controls may only expose supported
   effective values; the port currently labels reasoning as server-default.
7. Resolve the previously reported R2 binding/permission and dedicated image
   API credential in the existing secure environment. Do not expose R2 or RTX
   publicly, use a gateway token at api.openai.com, silently widen credentials,
   or introduce paid text fallback. Upload availability is now reported
   separately from image-generation availability.
8. Back up production D1 privately, apply only pending additive migrations,
   retain a rollback build and deploy to the existing hostname. Verify old
   reviewer/version/account records and existing article content are untouched.
9. Verify owner Google login and one owner-driven production run: saved brief →
   GLM titles → subtitles → image/thumbnail → outline → ChatGPT Pro import →
   draft save/approval → reviewer version → existing assignment/comment flow.
   Do not seed production accounts or invite/notify friends without a request.
10. Report the final commit/build, actual local/browser/live checks, and exact
    remaining blockers. Keep scoring, the agent sidebar, automatic full-text
    generation and legacy data import out of this pass.
