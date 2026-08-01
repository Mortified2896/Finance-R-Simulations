# Article Lab Inbox Architecture

## Decision status

The intended Article Lab architecture includes a central **Idea Inbox** that accepts both idea-first and paper-first inputs. That product concept has not been replaced by Article Inbox.

The current application does not yet implement that distinct Idea Inbox stage. Its top-level **Article Inbox** is an interim operational screen that combines quick capture, research-source work, and the canonical article-candidate queue. This colocation is current behavior, not an architecture decision that ideas and article candidates are the same entity.

Implementing the planned Idea Inbox remains future work and requires an explicit product/schema decision. In particular, that work must define idea identity and lifecycle, how multiple source-derived angles merge into or relate to an idea, and the promotion boundary from an idea to an article candidate. Until then, do not add compatibility aliases that redirect `idea_inbox` to Article Inbox and do not describe Article Inbox as a replacement for Idea Inbox.

## Intended relationships

| Concept | Responsibility | Current persistence and behavior |
| --- | --- | --- |
| Research Inbox | Intake and triage of papers and other sources. A source is evidence/context, not automatically an article idea or candidate. | `research_sources` plus source assets and research workflow metadata. Currently rendered inside the top-level Article Inbox screen. |
| Source Summary | Source-specific synthesis, claims, and supporting evidence. It remains attached to the research source. | `research_source_summaries` and related claim/evidence tables. The Summary route is opened from a selected Research Inbox source. |
| Idea Inbox | Planned central idea layer for spontaneous ideas and ideas/angles derived from one or more papers. It should support idea-first and paper-first development before commitment to an article. | Not implemented as a distinct current route, table, or lifecycle. The old route was only a disabled placeholder; the legacy table names were speculative import compatibility. |
| Article Inbox | Queue of article candidates that have crossed the idea-to-article boundary and may be refined, archived, or developed. | `article_candidates`. Current UI also hosts quick capture and research work as an interim colocation. |
| Article candidate | A proposed article, with controlled origin and progress, ready to be refined or promoted into active development. | `article_candidates`; currently `quick_idea` or `research_angle` origin. A research angle enters only through explicit promotion. Quick capture currently creates a candidate directly, which is an interim shortcut around the future Idea Inbox. |
| Project | Active article-development workspace created from a candidate. It owns downstream production context and snapshots provenance rather than mutating the originating research records. | One `article_projects` row per candidate, with linked `article_project_evidence_sources`; opened through **Develop Article** and used by Article Evidence and later stages. |

The intended future flow is therefore:

```text
idea-first:  quick capture --------------------> Idea Inbox ----> Article candidate ----> Project
paper-first: Research Inbox -> Source Summary -> Idea Inbox ----> Article candidate ----> Project
```

The current shortcut is:

```text
quick capture --------------------------------> Article candidate ----> Project
research angle -- explicit Add to Candidates -> Article candidate ----> Project
```

## Focused removal audit

The repository cleanup removed only compatibility for the obsolete implementation introduced around the current candidate workflow:

- **Routes:** removed acceptance of `idea_inbox` in the server navigation allowlist and removed the `?section=idea_inbox` / `?page=idea_inbox` redirect to `research_inbox`. Keeping that redirect would incorrectly encode Article Inbox as the destination for the future Idea Inbox concept.
- **UI references:** the actual Idea Inbox sidebar item, page metadata, responsive-section entry, page switch branch, disabled Quick Idea button, placeholder cards, and placeholder Article Evidence copy had already been removed by commit `f4716cc` when the working candidate flow was introduced. The cleanup did not remove a functional Idea Inbox UI.
- **Fields:** removed `article_candidates.legacy_source_table` and `article_candidates.legacy_source_id`. They recorded only speculative import provenance from legacy table names and were null in the local runtime database. Current provenance fields (`origin_type`, `research_source_id`, `research_angle_id`, project snapshots, and `provenance_json`) remain.
- **Tables:** no Idea Inbox table was dropped by the cleanup. Neither `article_ideas` nor `idea_inbox_items` was created by production repository code, and neither existed in the inspected local runtime database. Both names appeared only as inputs to a compatibility scanner (plus a test fixture for `article_ideas`).
- **Index:** removed `idx_article_candidates_legacy`, the partial unique index over the two removed legacy provenance fields. Current candidate, status, origin, research-angle, and project indexes remain.
- **Migration:** removed `article_inbox_migrate_legacy_ideas()` and its generic column-probing helper `article_inbox_first_column()`, along with the test fixture/assertions for legacy import. The migration guessed mappings from multiple possible column names and was not part of the planned Idea Inbox model.
- **Backend dependencies:** removed `article_inbox_redirect_section()` and calls to the legacy importer. No active create, edit, promote, archive, restore, develop, evidence, title-generation, or research-summary dependency used the removed fields or tables.
- **Documentation:** removed claims that the former Idea Inbox had been absorbed by Article Inbox and that the initializer migrated legacy Idea Inbox rows. This document replaces those claims with the explicit current/future boundary.
- **Local runtime schema:** after confirming both legacy fields were null and neither legacy table existed, the two fields and their empty index were removed from the ignored local SQLite database using a backup-first rebuild. No candidate, project, research source, summary, or angle records were removed.

`article_inbox_migrate_promoted_angles()` remains intentionally. It depends on the still-current `research_article_angles.article_lab_batch_id` field and recovers research angles that had already entered Title Lab into the candidate/project workflow. It is not Idea Inbox compatibility.

## Conclusion

The cleanup did not remove a functional current Idea Inbox or a production Idea Inbox schema. It removed an obsolete placeholder route alias and speculative legacy-import machinery. The reusable foundations remain: research sources and summaries, source-derived angles, canonical candidates, provenance links/snapshots, evidence-source links, and projects.

The planned central Idea Inbox is still required future work. Its route, storage model, lifecycle, and promotion rules should be designed explicitly rather than reconstructed from the removed placeholder or compatibility code.
