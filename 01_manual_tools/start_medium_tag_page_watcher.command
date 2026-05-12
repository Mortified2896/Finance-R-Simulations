#!/bin/zsh

# Double-click this file to open a visible Playwright browser that watches a
# Medium tag page and imports changed rendered cards into the local SQLite DB.

cd "$(dirname "$0")/.." || exit 1

echo "Starting Medium Tag Page Watcher..."
echo
echo "A Chrome browser window will open."
echo "Log in or navigate manually, then open a Medium /tag/... page, /tag/.../recommended page, or article page."
echo "The watcher imports tag cards when new article URLs appear and saves article text on article pages."
echo "Type p + Enter to pause/resume, or q + Enter to quit."
echo

node scripts/watch_medium_tag_page_pw.js --channel chrome --wait-for-medium "$@"

echo
echo "Watcher stopped. You can close this Terminal window."
