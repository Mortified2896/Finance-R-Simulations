#!/bin/zsh

# Double-click this file on macOS to validate the Medium Analysis V2 workflow.
# It switches from this helper folder back to the project root first.

cd "$(dirname "$0")/../.." || exit 1

echo "Validating Medium Analysis V2..."
echo

Rscript scripts/validate_medium_analysis_v2.R

echo
echo "Validation closed. You can close this Terminal window."
