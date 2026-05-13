import assert from "node:assert";
import fs from "node:fs";
import path from "node:path";
import {
  detectMediumSearchPageType,
  parseMediumSearchTagsHtml,
} from "./medium_tag_snapshot_watcher_helpers.mjs";

const fixturePath = path.join(
  "debug_samples",
  "medium_search_tags",
  "medium_search_tags:2026-05-13_US-NY_logged-out_search-tags_finance_showmore-0.html"
);

const html = fs.readFileSync(fixturePath, "utf8");
const pageUrl = "https://medium.com/search/tags?q=finance";
const payload = parseMediumSearchTagsHtml(html, pageUrl, "Medium", {
  capturedDate: new Date("2026-05-13T12:00:00Z"),
  trackingContext: "US_NY_logged_out_clean",
});
const defaultContextPayload = parseMediumSearchTagsHtml(html, pageUrl, "Medium", {
  capturedDate: new Date("2026-05-13T12:00:00Z"),
});
const showMoreHtml = html.replace(
  "</body>",
  '<a href="https://medium.com/tag/investing?source=search_tag---------30-----------------------------------">Investing</a></body>'
);
const showMorePayload = parseMediumSearchTagsHtml(showMoreHtml, pageUrl, "Medium", {
  capturedDate: new Date("2026-05-13T12:00:00Z"),
});

assert.strictEqual(detectMediumSearchPageType(pageUrl), "search_tags");
assert.strictEqual(payload.page_type, "search_tags");
assert.strictEqual(payload.search_term, "finance");
assert.strictEqual(payload.source_url, "https://medium.com/search/tags?q=finance");
assert.strictEqual(payload.tracking_context, "US_NY_logged_out_clean");
assert.strictEqual(defaultContextPayload.tracking_context, "unknown_context");
assert.strictEqual(payload.tags.length, 30);
assert.strictEqual(showMorePayload.tags.length, 31);
assert.deepStrictEqual(
  {
    rank: showMorePayload.tags[30].result_rank,
    title: showMorePayload.tags[30].display_title,
    slug: showMorePayload.tags[30].tag_slug,
  },
  {
    rank: 31,
    title: "Investing",
    slug: "investing",
  }
);

assert.deepStrictEqual(
  {
    rank: payload.tags[0].result_rank,
    title: payload.tags[0].display_title,
    slug: payload.tags[0].tag_slug,
  },
  {
    rank: 1,
    title: "Finance",
    slug: "finance",
  }
);

assert.deepStrictEqual(
  {
    rank: payload.tags[1].result_rank,
    title: payload.tags[1].display_title,
    slug: payload.tags[1].tag_slug,
  },
  {
    rank: 2,
    title: "Personal Finance",
    slug: "personal-finance",
  }
);

assert.deepStrictEqual(
  {
    rank: payload.tags[7].result_rank,
    title: payload.tags[7].display_title,
    slug: payload.tags[7].tag_slug,
  },
  {
    rank: 8,
    title: "Financial Independence",
    slug: "financial-independence",
  }
);

const postIds = payload.sidebar_posts.map((post) => post.medium_post_id);
assert.ok(postIds.includes("401b06f251db"));
assert.ok(postIds.includes("13002174f6ee"));
assert.ok(postIds.includes("fa264e989f05"));

assert.strictEqual(payload.sidebar_people.length, 3);

console.log("Medium search-tags parser tests passed.");
