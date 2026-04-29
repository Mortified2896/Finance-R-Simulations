#!/bin/zsh

# Double-click this file on macOS to import your saved Medium stats page HTML.
# It switches from this helper folder back to the project root first.

cd "$(dirname "$0")/.." || exit 1

echo "Starting Medium Own Stats HTML Importer..."
echo
echo "Drop the saved Medium stats HTML file here, then press Enter."
echo "Press Enter with no path to use the default fixture:"
echo "debug_samples/Stats Page/Medium Stats Page.html"
echo

read -r "raw_input_path?HTML file path: "
input_path="${(Q)raw_input_path}"

if [[ -z "$input_path" ]]; then
  input_path="debug_samples/Stats Page/Medium Stats Page.html"
  echo
  echo "No file path provided."
  echo "Using default fixture: $input_path"
else
  echo
  echo "Using provided file: $input_path"
fi

if [[ ! -f "$input_path" ]]; then
  echo
  echo "Import failed."
  echo "The HTML file was not found:"
  echo "$input_path"
  echo
  echo "Press Enter to close this window."
  read -r
  exit 1
fi

echo
echo "Running importer..."
echo

if Rscript scripts/import_medium_own_stats_from_html.R "$input_path"; then
  echo
  echo "Import finished successfully."
else
  status=$?
  echo
  echo "Import failed with exit code $status."
fi

echo
echo "Press Enter to close this window."
read -r
