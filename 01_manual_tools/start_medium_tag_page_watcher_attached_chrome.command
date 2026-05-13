#!/bin/zsh

# Double-click this file to start a normal Chrome window with remote debugging
# and attach the Medium tag-page watcher to it. This avoids Playwright launching
# a visibly controlled test browser.

cd "$(dirname "$0")/.." || exit 1

PORT="${MEDIUM_WATCHER_DEBUG_PORT:-9222}"
PROFILE_DIR="$PWD/data/medium_tag_manual_chrome_profile"
CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

if [ ! -x "$CHROME" ]; then
  echo "Google Chrome was not found at:"
  echo "$CHROME"
  echo
  echo "Install Chrome or edit this launcher to point to your browser."
  read "?Press Enter to close this window."
  exit 1
fi

mkdir -p "$PROFILE_DIR"

while true; do
  echo "Starting Medium Tag Page Watcher with attached Chrome..."
  echo
  echo "A Chrome window will open. Log in or navigate manually."
  echo "When you open a Medium /tag/... page, /tag/.../recommended page, /search/tags?q=... page, or article page, tracking starts."
  echo "Type q + Enter in this Terminal window to stop this watcher run."
  echo

  if curl -fsS "http://127.0.0.1:$PORT/json/version" >/dev/null 2>&1; then
    echo "Found existing watcher Chrome on port $PORT. Attaching without opening a new tab."
  else
    open -na "Google Chrome" --args \
      --remote-debugging-port="$PORT" \
      --user-data-dir="$PROFILE_DIR" \
      about:blank
  fi

  echo "Waiting for Chrome remote debugging on port $PORT..."
  for i in {1..40}; do
    if curl -fsS "http://127.0.0.1:$PORT/json/version" >/dev/null 2>&1; then
      break
    fi
    sleep 0.25
  done

  if curl -fsS "http://127.0.0.1:$PORT/json/version" >/dev/null 2>&1; then
    node scripts/watch_medium_tag_page_pw.cjs \
      --connect-cdp "http://127.0.0.1:$PORT" \
      --wait-for-medium "$@"
    watcher_status=$?
  else
    watcher_status=1
    echo "Could not connect to Chrome remote debugging on port $PORT."
    echo "Close the Chrome window and try again."
  fi

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
