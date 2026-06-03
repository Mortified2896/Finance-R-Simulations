# Article Lab Cleanup TODO

## Current status

- `app.R` has been partially modularized through behavior-preserving helper extraction.
- `app.R` still remains large because schema setup, DB reads/writes, API orchestration, UI, server logic, observers, and workflow stages are still inside it.
- The goal is not to fully split Shiny tabs yet. The workflow is still evolving, so refactor by layer first.

## Completed cleanup passes

- Initial helper/config extraction.
- Status/workflow helper extraction.
- Input/coercion helper extraction.
- Scoring helper extraction.
- Title/subtitle helper extraction.
- ID helper extraction.
- Startup/smoke validation with disposable DB.

## Next cleanup candidates

1. Display/table formatting helpers

- Pure formatting and UI/table helpers only.
- No observers, render blocks, or reactive state.

2. Schema setup extraction

- Exact move only.
- Possible files: `R/schema_rating.R`, `R/schema_article_lab.R`, `R/schema_research.R`.
- No SQL, migration, default, index, or status changes.

3. API boundary documentation / wrapper cleanup

- Document R-to-Node/Python request/response boundaries.
- Move API wrapper functions only if behavior-preserving.
- No prompt, payload, timeout, stdout/stderr, fallback, or model default changes.

4. Small helper tests

- Status labels.
- Input normalization.
- Title/subtitle validation.
- Scoring helpers.
- ID formats.

## Delay until workflow is stable

- Shiny tab modules.
- Major UI redesign.
- Broad workflow restructuring.
- DB schema changes.
- API prompt/payload/model changes.
- Persisted ID/status renaming.

## Non-negotiable cleanup rules

- Behavior-preserving only unless explicitly requested.
- No production DB writes during cleanup validation.
- Use disposable DB copies for app startup smoke tests.
- No external API calls during cleanup validation.
- Keep `app.R` as the workflow orchestrator for now.
- Full Shiny tab module split should wait until the workflow is more stable.
