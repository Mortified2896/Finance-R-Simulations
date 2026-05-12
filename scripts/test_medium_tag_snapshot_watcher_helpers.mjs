import assert from "node:assert";
import {
  extractReadTimeMinutes,
  inferPublishedFromLabel,
  parseMediumTagSnapshot,
  parseCompactInteger,
} from "./medium_tag_snapshot_watcher_helpers.mjs";

const fixture = `
- article:
  - link "Lawyers, Guns, and Money In Lawyers, Guns, and Money by Daniel McIntosh, PhD. The Questions that Matter It was never Capitalism versus Socialism Member-only story Apr 6 A clap icon 345 A response icon 20":
    - tooltip "Lawyers, Guns, and Money":
      - link "Lawyers, Guns, and Money":
        - /url: https://medium.com/lawyers-guns-and-money?source=tag_recommended_stories_page------finance---1-107--------------------token--------------
    - paragraph: In
    - tooltip "Lawyers, Guns, and Money":
      - link "Lawyers, Guns, and Money":
        - /url: https://medium.com/lawyers-guns-and-money?source=tag_recommended_stories_page------finance---1-107--------------------token--------------
        - paragraph: Lawyers, Guns, and Money
    - paragraph: by
    - tooltip "Daniel McIntosh, PhD.":
      - link "Daniel McIntosh, PhD.":
        - /url: /@demcintosh?source=tag_recommended_stories_page------finance---1-107--------------------token--------------
        - paragraph: Daniel McIntosh, PhD.
    - link "The Questions that Matter It was never Capitalism versus Socialism":
      - /url: /lawyers-guns-and-money/the-questions-that-matter-0f0b0a442315?source=tag_recommended_stories_page------finance---1-107--------------------token--------------
      - heading "The Questions that Matter" [level=2]
      - heading "It was never Capitalism versus Socialism" [level=3]
    - button "Member-only story":
      - tooltip:
    - generic: Apr 6
    - generic: 14 min read
    - link "A clap icon 345 A response icon 20":
      - /url: /lawyers-guns-and-money/the-questions-that-matter-0f0b0a442315?source=tag_recommended_stories_page------finance---1-107--------------------token--------------
      - tooltip "A clap icon 345":
        - img "A clap icon"
        - generic: "345"
      - tooltip "A response icon 20":
        - img "A response icon"
        - generic: "20"
- article:
  - link "Ignacio de Gregorio Ignacio de Gregorio The AI Washing’s First Victims The New Normal is to Blame AI. But not for long. Member-only story 6h ago A clap icon 2.5K A response icon 57":
    - tooltip "Ignacio de Gregorio":
      - link "Ignacio de Gregorio":
        - /url: /@ignacio.de.gregorio.noblejas?source=tag_recommended_stories_page------finance---0-107--------------------token--------------
    - tooltip "Ignacio de Gregorio":
      - link "Ignacio de Gregorio":
        - /url: /@ignacio.de.gregorio.noblejas?source=tag_recommended_stories_page------finance---0-107--------------------token--------------
    - link "The AI Washing’s First Victims The New Normal is to Blame AI. But not for long.":
      - /url: /@ignacio.de.gregorio.noblejas/the-ai-washings-first-victims-c923429199ca?source=tag_recommended_stories_page------finance---0-107--------------------token--------------
      - heading "The AI Washing’s First Victims" [level=2]
      - heading "The New Normal is to Blame AI. But not for long." [level=3]
    - button "Member-only story":
      - tooltip:
    - generic: 6h ago
    - link "A clap icon 2.5K A response icon 57":
      - /url: /@ignacio.de.gregorio.noblejas/the-ai-washings-first-victims-c923429199ca?source=tag_recommended_stories_page------finance---0-107--------------------token--------------
      - tooltip "A clap icon 2.5K":
        - img "A clap icon"
        - generic: "2.5K"
      - tooltip "A response icon 57":
        - img "A response icon"
        - generic: "57"
`;

assert.strictEqual(parseCompactInteger("2.5K"), 2500);
assert.strictEqual(parseCompactInteger("1,234"), 1234);
assert.strictEqual(extractReadTimeMinutes("    - generic: 16 min read"), null);
assert.strictEqual(extractReadTimeMinutes('      - heading "By Someone · 16 min read" [level=3]'), null);

const inferred = inferPublishedFromLabel("6h ago", new Date("2026-05-10T12:00:00Z"));
assert.strictEqual(inferred.timestamp, "2026-05-10T06:00:00Z");
assert.strictEqual(inferred.precision, "hour_approx");

const payload = parseMediumTagSnapshot(
  fixture,
  "https://medium.com/tag/finance/recommended",
  "Medium",
  { capturedDate: new Date("2026-05-10T12:00:00Z") }
);

assert.strictEqual(payload.source_type, "medium_tag_page_bookmarklet");
assert.strictEqual(payload.schema_version, 20);
assert.strictEqual(payload.tag_slug, "finance");
assert.strictEqual(payload.page_variant, "tag_recommended");
assert.strictEqual(payload.cards.length, 2);

assert.deepStrictEqual(
  {
    title: payload.cards[0].title,
    url: payload.cards[0].article_url,
    postId: payload.cards[0].medium_post_id,
    author: payload.cards[0].author_name,
    publication: payload.cards[0].publication_name,
    status: payload.cards[0].publication_status,
    date: payload.cards[0].published_date_inferred,
    readTime: payload.cards[0].read_time_minutes,
    claps: payload.cards[0].claps,
    responses: payload.cards[0].responses,
  },
  {
    title: "The Questions that Matter",
    url: "https://medium.com/lawyers-guns-and-money/the-questions-that-matter-0f0b0a442315",
    postId: "0f0b0a442315",
    author: "Daniel McIntosh, PhD.",
    publication: "Lawyers, Guns, and Money",
    status: "publication",
    date: "2026-04-06",
    readTime: null,
    claps: 345,
    responses: 20,
  }
);

assert.strictEqual(payload.cards[1].author_name, "Ignacio de Gregorio");
assert.strictEqual(payload.cards[1].publication_status, "self_published_assumed");
assert.strictEqual(payload.cards[1].claps, 2500);
assert.strictEqual(payload.cards[1].responses, 57);
assert.strictEqual(payload.cards[1].published_at_inferred, "2026-05-10T06:00:00Z");

console.log("Medium tag snapshot watcher helper tests passed.");
