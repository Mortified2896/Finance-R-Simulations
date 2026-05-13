#!/bin/zsh

# Double-click this file to open a visible Playwright browser that watches a
# Medium tag page and imports changed rendered cards into the local SQLite DB.

cd "$(dirname "$0")/.." || exit 1

while true; do
  echo "Starting Medium Tag Page Watcher..."
  echo
  echo "A Chrome browser window will open."
  echo "Log in or navigate manually, then open a Medium /tag/... page, /tag/.../recommended page, /search/tags?q=... page, or article page."
  echo "The watcher imports tag cards when new article URLs appear and saves article text on article pages."
  echo "Type p + Enter to pause/resume, or q + Enter to stop this watcher run."
  echo

  node scripts/watch_medium_tag_page_pw.cjs --channel chrome --wait-for-medium "$@"
  watcher_status=$?

  echo
  echo "Watcher stopped with exit code $watcher_status."
  if [[ -t 0 ]]; then
    read -k 1 "restart_key?Press r to restart, or any other key to close this Terminal window: "
  else
    echo "Press r to restart, or any other key to close this Terminal window: "
    read -r restart_key
  fi
  echo
  echo

  if [[ "$restart_key" != "r" && "$restart_key" != "R" ]]; then
    echo "Watcher closed. You can close this Terminal window."
    exit "$watcher_status"
  fi
done
