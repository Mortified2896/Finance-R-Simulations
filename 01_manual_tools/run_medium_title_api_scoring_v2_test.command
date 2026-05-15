#!/bin/zsh

# Double-click this file on macOS to inspect a safe API scoring V2 dry run.
# It does not call the API or write score rows.

cd "$(dirname "$0")/.." || exit 1

echo "Medium Title API Scoring V2 dry run..."
echo

python3 scripts/score_medium_titles_api_v2.py --dry-run --limit 3 --prompt-version v2_1

echo
echo "Dry run closed. You can close this Terminal window."
