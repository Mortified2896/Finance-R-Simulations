#!/bin/zsh

# Double-click this file on macOS to blind-rate Medium titles in Terminal.
# It switches from this helper folder back to the project root first.

cd "$(dirname "$0")/../.." || exit 1

echo "Starting Medium Title Human Rating..."
echo

MEDIUM_TITLE_RATING_TERMINAL=1 Rscript scripts/rate_medium_titles_terminal.R --rater johannes --limit 100 --rating-version v2_general_title_only
status=$?

echo
if [ "$status" -eq 0 ]; then
  echo "Rating workflow closed. You can close this Terminal window."
else
  echo "Rating workflow failed with exit code $status."
  echo "Press Return to close this Terminal window."
  read -r
fi
