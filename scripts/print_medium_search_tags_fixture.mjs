#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { parseMediumSearchTagsHtml } from "./medium_tag_snapshot_watcher_helpers.mjs";

const inputPath = process.argv[2] || path.join(
  "debug_samples",
  "medium_search_tags",
  "medium_search_tags:2026-05-13_US-NY_logged-out_search-tags_finance_showmore-0.html"
);
const pageUrl = process.argv[3] || "https://medium.com/search/tags?q=finance";
const html = fs.readFileSync(inputPath, "utf8");
const payload = parseMediumSearchTagsHtml(html, pageUrl, "Medium");

console.log("Medium search-tags fixture parse");
console.log("--------------------------------");
console.log(`Search term: ${payload.search_term}`);
console.log(`Tags found: ${payload.tags.length}`);
console.log(`Sidebar posts found: ${payload.sidebar_posts.length}`);
console.log(`Sidebar people found: ${payload.sidebar_people.length}`);
console.log("");
console.log("Top tags:");
for (const tag of payload.tags.slice(0, 10)) {
  console.log(`${tag.result_rank}. ${tag.display_title} -> ${tag.tag_slug}`);
}
console.log("");
console.log("Sidebar post IDs:");
for (const post of payload.sidebar_posts) {
  console.log(`${post.result_rank}. ${post.medium_post_id} ${post.title || post.article_url}`);
}
console.log("");
console.log("Sidebar people:");
for (const person of payload.sidebar_people) {
  console.log(`${person.result_rank}. ${person.display_name} @${person.username}`);
}
