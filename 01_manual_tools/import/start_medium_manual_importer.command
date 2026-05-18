#!/bin/zsh

# Double-click this file on macOS to start the Medium manual stats importer.
# It switches from this helper folder back to the project root first.

cd "$(dirname "$0")/../.." || exit 1

echo "Starting Medium Manual Stats Importer..."
echo

Rscript scripts/import_medium_manual_stats.R

echo
echo "Importer closed. You can close this Terminal window."
