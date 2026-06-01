# Medium Tag-Page Import Notes

## Install and use the bookmarklet
- Open [`medium_tag_page_bookmarklet.html`](medium_tag_page_bookmarklet.html) in your browser.
- Drag the `Medium Tag Page JSON` bookmarklet to the bookmarks bar.
- Open a supported Medium tag page such as:
- `https://medium.com/tag/finance`
- `https://medium.com/tag/finance/recommended`
- Scroll until enough cards are visible.
- Click the bookmarklet.
- It downloads a JSON file and shows an alert like `Exported 57 Medium tag-page cards.`

## Import the downloaded JSON
- Run [`start_medium_own_stats_importer.command`](start_medium_own_stats_importer.command).
- Drop the downloaded JSON file path into Terminal.
- The shared router detects `source_type = "medium_tag_page_bookmarklet"` and runs `scripts/import_medium_tag_page_bookmarklet.R`.

## What tables are written
- `medium_articles`: canonical article identity and stable metadata.
- `medium_tag_page_snapshots`: one imported tag-page JSON file per snapshot, including `source_file_hash` and `page_variant` when available.
- `medium_tag_page_observations`: one visible card observation per snapshot/article/position, with `article_id` plus normalized URL and post ID.
- `medium_article_import_queue`: one canonical queue row per normalized article URL for later full-article import work.

## How this differs from RSS tracking
- RSS imports discover articles from feeds and store feed-driven metadata/content fields already used by the project.
- Tag-page imports store visible card observations from the topic page at one captured moment.
- Tag-page imports do not store full article body text and do not overwrite historical observations.

## Page variants
- `tag_landing` means the normal tag landing page, for example `https://medium.com/tag/finance`.
- `tag_recommended` means the recommended-story page, for example `https://medium.com/tag/finance/recommended`.
- The bookmarklet stores the current page URL in `tag_url`, so the `/recommended` variant is preserved even when Medium canonical metadata points back to `/tag/finance`.

## Canonical articles, observations, and queue rows
- `medium_articles` stays the canonical deduplicated article table.
- `medium_tag_page_observations` is append-only history showing when and where a card was seen, including page position and visible engagement counts.
- `medium_article_import_queue` tracks articles that still need fuller content import.
- If full content already exists in `content_text` or `content_encoded_html`, the tag importer does not reactivate a completed queue row.

## Duplicate-import debugging
- Each imported tag-page JSON gets a `source_file_hash` in `medium_tag_page_snapshots`.
- Re-importing the same file exits successfully with an `Already imported` summary and the existing `snapshot_id`.
- New import summaries print both `source_file_hash` and `snapshot_id`.

## Debug sample usage
- `debug_samples/medium_tag_page_fixture_small.json` is the first fixture for importer tests. Use it to isolate DB/importer bugs from bookmarklet extraction bugs.
- `debug_samples/medium_tag_page_recommended_fixture_small.json` is the small importer fixture for the recommended layout.
- `debug_samples/Tag Sample` is a saved Medium tag-page reference for the landing layout only.
- `debug_samples/Tag Examples/Tag Sample Recommended` is the local saved recommended-layout reference:
- `debug_samples/Tag Examples/Tag Sample Recommended`
- The sample confirmed:
  - page-level JSON-LD contains `CollectionPage.mainEntity` article records
  - the saved page also embeds richer `Post` objects with clap/response/member-lock/publication/image metadata
  - saved Medium tag-page HTML fallback parsing is intentionally deferred in this pass
- The raw saved HTML and asset folders under `debug_samples/Tag Examples/` are local debug fixtures and should not be committed to Git.
- The bookmarklet should still be tested live in a browser, because the saved HTML is only a selector and structure reference.
