#!/bin/zsh

# Double-click this file on macOS to run the Medium thumbnail API score V1 evaluation.
# This evaluates existing DB rows only; it does not call the API.

cd "$(dirname "$0")/../.." || exit 1

echo "Running Medium thumbnail API score V1 evaluation..."
echo

Rscript scripts/analyze_medium_thumbnail_api_scores_v1.R \
  --prompt-version thumbnail_v1 \
  --scope thumbnail_only \
  --sample-file data/analysis/title_api_score_samples/thumbnail_100_v1.csv \
  --output-mode all

echo
echo "Analysis closed. You can close this Terminal window."
