# Documentation map

## Current direction and next implementation

- [Architecture](architecture.md): target application and migration approach.
- [Review MVP](article-review-mvp.md): agreed behavior and acceptance checks; not yet implemented.
- [Cloudflare development](cloudflare-development.md): existing tooling, recorded checks, and remaining setup.
- [O1 handoff](o1-review-mvp-handoff.md): implementation work to start next.
- [Runtime data](local_only_files.md): private/generated data versus tracked source.

## Supporting knowledge, not mandatory Cloudflare architecture

- [Medium Analysis V2](medium_analysis_v2.md): existing research/scoring methodology and local SQLite implementation. Not a verified success predictor or a required new-app schema.
- [Research library](research_library.md): existing local metadata/import workflow.
- [Research workflow](research_workflow.md): sources, angles, and development concepts, with local commands.
- [Script inventory](script_inventory.md): navigation for existing R/Python/Node tools, not a list of features to port.
- [Writing helpers](../scripts/writing_api/README.md) and [manual tools](../01_manual_tools/manual_tools_index.md): existing local workflows.

## Historical references

- [Shiny guide](human_preview_rating_app.md).
- [Previous Inbox design](article_lab_inbox_architecture.md).
- [Infrastructure setup record](cloudflare-setup.md).

The two archived `legacy_*` documents remain in this directory so their original
relative links still resolve. Their original content is preserved, including old
instructions and limitations, but is not an active specification. Read it only
when relevant. Current product requirements live in the MVP and architecture docs.

This cleanup does not revalidate old analysis results or execute local-data tools.
