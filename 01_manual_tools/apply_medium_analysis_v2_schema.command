#!/bin/zsh

# Double-click this file on macOS to install/update the Medium Analysis V2 schema.
# The setup script creates a timestamped DB backup before applying changes.

cd "$(dirname "$0")/.." || exit 1

echo "Applying Medium Analysis V2 schema..."
echo

Rscript scripts/apply_medium_analysis_v2_schema.R

echo
echo "Schema setup closed. You can close this Terminal window."
