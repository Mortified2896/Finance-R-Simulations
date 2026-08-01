import assert from "node:assert/strict";
import { mapSubtitleResults } from "./writing_api/generate_subtitles.mjs";
import { mapFullTextResults } from "./writing_api/generate_full_text.mjs";

const subtitleItems = [
  { item_alias: "item_A", candidate_id: "cand_A", batch_id: "batch_A" },
  { item_alias: "item_B", candidate_id: "cand_B", batch_id: "batch_B" }
];
const subtitleRaw = JSON.stringify({ results: [
  { item_alias: "item_B", subtitles: ["Second mapped first"] },
  { item_alias: "unknown", subtitles: ["Must be ignored"] }
] });
assert.deepEqual(mapSubtitleResults(subtitleRaw, 4, subtitleItems), [
  { item_alias: "item_B", candidate_id: "cand_B", batch_id: "batch_B", subtitles: ["Second mapped first"] }
]);

const partiallyInvalidSubtitleRaw = JSON.stringify({ results: [
  { item_alias: "item_B", subtitles: ["Valid B", "x".repeat(91)] },
  { item_alias: "missing", subtitles: ["Must be ignored"] },
  { item_alias: "item_A", subtitles: [] }
] });
assert.deepEqual(mapSubtitleResults(partiallyInvalidSubtitleRaw, 4, subtitleItems), [
  { item_alias: "item_B", candidate_id: "cand_B", batch_id: "batch_B", subtitles: ["Valid B"] },
  { item_alias: "item_A", candidate_id: "cand_A", batch_id: "batch_A", subtitles: [] }
]);

const fullItems = [
  { item_alias: "item_A", outline_id: "out_A", thumbnail_id: "thumb_A", subtitle_id: "sub_A", candidate_id: "cand_A", batch_id: "batch_A", source_context_mode: "none" },
  { item_alias: "item_B", outline_id: "out_B", thumbnail_id: "thumb_B", subtitle_id: "sub_B", candidate_id: "cand_B", batch_id: "batch_B", source_context_mode: "none" }
];
const fullRaw = JSON.stringify({ results: [
  { item_alias: "item_B", full_text: "# Correct reordered article", citation_map: [] },
  { item_alias: "unknown", full_text: "# Must be ignored", citation_map: [] }
] });
const mapped = mapFullTextResults(fullRaw, fullItems);
assert.equal(mapped.length, 1);
assert.equal(mapped[0].outline_id, "out_B");
assert.equal(mapped[0].full_text, "# Correct reordered article");
console.log("Prompt alias mapping tests passed.");
