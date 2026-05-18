#!/bin/zsh

# Double-click this file on macOS to run the Medium title follow-up V2 analysis.

cd "$(dirname "$0")/../.." || exit 1

echo "Running Medium title follow-up V2 analysis..."
echo

Rscript scripts/analyze_medium_title_text_followup_v2.R

echo
echo "Analysis closed. You can close this Terminal window."
