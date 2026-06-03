#!/bin/zsh

# Double-click this file on macOS to import supported Medium bookmarklet JSON
# or saved /me/stats HTML files through the shared file-drop router.

cd "$(dirname "$0")/../.." || exit 1
exec zsh scripts/manual_tools/start_medium_own_stats_importer.zsh "$@"
