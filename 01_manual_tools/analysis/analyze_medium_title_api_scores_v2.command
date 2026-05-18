#!/bin/zsh

# Double-click this file on macOS to run the Medium API score V2 evaluation.

cd "$(dirname "$0")/../.." || exit 1

echo "Running Medium API score V2 evaluation..."
echo

Rscript scripts/analyze_medium_title_api_scores_v2.R

echo
echo "Analysis closed. You can close this Terminal window."
