#!/bin/zsh

# Double-click this file on macOS to blind-rate Medium titles/subtitles in Terminal.
# It switches from this helper folder back to the project root first.

cd "$(dirname "$0")/.." || exit 1

echo "Starting Medium Title Human Rating..."
echo

Rscript scripts/rate_medium_titles_terminal.R --rater johannes --limit 100 --rating-version v2_general

echo
echo "Rating workflow closed. You can close this Terminal window."
