#!/bin/zsh

# Double-click this file to open a visible Playwright browser that watches a
# Medium tag page and imports changed rendered cards into the local SQLite DB.

cd "$(dirname "$0")/../.." || exit 1

echo "Starting Medium Tag Page Watcher..."
echo
echo "A Chrome browser window will open."
echo "Log in or navigate manually, then open a Medium /tag/... page, /tag/.../recommended page, publication page, /search/tags?q=... page, or article page."
echo "The watcher imports cards when new article URLs appear and saves article text on article pages."
echo "Type p + Enter to pause/resume, or q + Enter to stop this watcher run."
echo "Restart is disabled in this launcher because restarting would close the Chrome window it owns."
echo "Use start_medium_tag_page_watcher_attached_chrome.command when you need watcher-only restarts."
echo

node scripts/watch_medium_tag_page_pw.cjs --channel chrome --wait-for-medium "$@"
watcher_status=$?

echo
echo "Watcher stopped with exit code $watcher_status."
echo "Watcher closed. You can close this Terminal window."
exit "$watcher_status"
