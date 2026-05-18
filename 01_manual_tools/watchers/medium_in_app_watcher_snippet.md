# Medium In-App Watcher Snippet

This snippet is for Codex's in-app browser session. It uses the current visible tab,
captures Medium tag-page snapshots from the accessibility tree, writes watcher JSON
files, and imports them through the existing R importer.

It is not a normal browser bookmarklet. Use it from the Codex-controlled browser
session when you want Codex to watch while you manually scroll.

```js
const { runMediumInAppWatcher } = await import("/Users/Jo/GitHub/Finance R Simulations/scripts/medium_in_app_tag_watcher_runtime.mjs");

await runMediumInAppWatcher(tab, {
  durationMs: 120000,
  intervalMs: 5000,
  minNewUrls: 1
});
```
