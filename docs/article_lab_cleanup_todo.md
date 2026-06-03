# Article Lab Cleanup TODO

Project-wide cleanup is tracked in [`project_cleanup_checklist.md`](project_cleanup_checklist.md). This file is the narrower Article Lab/Shiny app cleanup list.

## Current status

- `app.R` has been partially modularized through behavior-preserving helper extraction.
- `app.R` still remains large because DB reads/writes, API orchestration, UI, server logic, observers, and workflow stages are still inside it.
- The goal is not to fully split Shiny tabs yet. The workflow is still evolving, so refactor by layer first.

## Completed cleanup passes

- Initial helper/config extraction.
- Status/workflow helper extraction.
- Input/coercion helper extraction.
- Scoring helper extraction.
- Title/subtitle helper extraction.
- ID helper extraction.
- Display/table/card helper extraction into `R/table_helpers.R`.
- Schema startup orchestration extraction into `R/schema_startup.R`.
- Startup/smoke validation with disposable DB.

## Next cleanup candidates

1. API boundary documentation / wrapper cleanup

- Document R-to-Node/Python request/response boundaries.
- Move API wrapper functions only if behavior-preserving.
- No prompt, payload, timeout, stdout/stderr, fallback, or model default changes.

2. Small helper tests

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
