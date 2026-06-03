# Stale Workflow / Placeholder Audit

This note records the project-wide TODO/FIXME/placeholder audit used by the cleanup checklist. It is intentionally a summary, not a raw grep dump.

## Audit scope

Searched tracked source and documentation files for:

- `TODO`
- `FIXME`
- `placeholder`

File types reviewed: R, JavaScript, Python, shell, command launchers, JSON/JSONC, and Markdown.

## Summary

No `FIXME` markers were found in the tracked files reviewed.

Most `placeholder` matches are legitimate runtime/UI terms, including:

- SQL parameter placeholder variables in `apps/human_preview_rating_app/app.R`.
- Shiny input placeholder text in `app.R` and `R/table_helpers.R`.
- CSS classes such as `.thumbnail-placeholder` in `R/ui_assets.R`.
- Title-only rating masks such as `title_only_placeholder_subtitle` and `title_only_placeholder_thumbnail_label`.
- Prompt guardrails that explicitly reject placeholder draft text such as `MARKDOWN_ARTICLE_HERE`.

The intentionally reserved placeholder scripts remain:

- `scripts/writing_api/draft_section.mjs`
- `scripts/writing_api/critique_draft.mjs`

They are documented in `docs/script_inventory.md` as future draft-stage helpers and are not referenced by the app or manual launchers.

## Files with notable matches

Highest-count files from the audit:

| Matches | File | Interpretation |
| ---: | --- | --- |
| 47 | `apps/human_preview_rating_app/app.R` | Mostly SQL bind placeholders, UI placeholder labels, and prompt guardrails. |
| 8 | `apps/human_preview_rating_app/R/table_helpers.R` | Shiny text-input placeholder labels. |
| 7 | `scripts/writing_api/generate_full_text.mjs` | Prompt validation language around replacing placeholder draft text. |
| 5 | `apps/human_preview_rating_app/R/ui_assets.R` | CSS classes for placeholder/invalid thumbnail UI. |
| 3 | `apps/human_preview_rating_app/R/app_config.R` | Intentional title-only subtitle/thumbnail masks. |

## Actual cleanup items retained

These are real future-work items rather than accidental stale markers:

1. `docs/medium_analysis_v2.md` has a `Thumbnail V3 TODO` section. Keep this as product/analysis planning unless Thumbnail V3 work is explicitly started.
2. `docs/article_lab_cleanup_todo.md` remains the active Article Lab cleanup list.
3. `docs/project_cleanup_checklist.md` remains the active project-wide cleanup checklist.
4. `scripts/writing_api/draft_section.mjs` and `scripts/writing_api/critique_draft.mjs` remain intentionally reserved placeholders for future draft-stage helpers.

## Current decision

No files should be deleted or renamed based on this audit alone. The remaining matches are either legitimate implementation language, active cleanup documentation, or intentionally reserved future-work placeholders.
