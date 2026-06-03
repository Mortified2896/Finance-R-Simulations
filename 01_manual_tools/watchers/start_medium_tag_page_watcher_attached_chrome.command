#!/bin/zsh

# Double-click this file to start a normal Chrome window with remote debugging
# and attach the Medium tag-page watcher to it.

cd "$(dirname "$0")/../.." || exit 1
exec zsh scripts/manual_tools/start_medium_tag_page_watcher_attached_chrome.zsh "$@"
