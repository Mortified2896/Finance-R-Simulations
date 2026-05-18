#!/bin/zsh

# Double-click this file on macOS to inspect a safe thumbnail API scoring dry run.
# It does not call the API, write score rows, or print Base64 image payloads.

cd "$(dirname "$0")/../.." || exit 1

echo "Medium Thumbnail API Scoring V1 dry run..."
echo

python3 scripts/score_medium_thumbnails_api_v1.py \
  --dry-run \
  --limit 3 \
  --prompt-version thumbnail_v1 \
  --scope thumbnail_only \
  --sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv

echo
echo "Dry run closed. You can close this Terminal window."
