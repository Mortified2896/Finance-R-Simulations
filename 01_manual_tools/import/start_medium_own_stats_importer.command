#!/bin/zsh

# Double-click this file on macOS to import supported Medium bookmarklet JSON
# or saved /me/stats HTML files through the shared file-drop router.
# It switches from this helper folder back to the project root first.

cd "$(dirname "$0")/../.." || exit 1

echo "Starting Medium File Importer..."
echo

while true; do
  echo "Drop one or more Medium bookmarklet JSON or HTML files here and press Enter."
  echo "Type q and press Enter to close."
  echo

  read -r "raw_input_path?Import file path: "
  input_paths=("${(@Q)${(z)raw_input_path}}")

  if [[ -z "$raw_input_path" || "$raw_input_path" == "q" || "$raw_input_path" == "Q" ]]; then
    echo
    echo "Importer closed. You can close this Terminal window."
    exit 0
  fi

  echo
  echo "Files queued: ${#input_paths[@]}"

  missing_files=()
  for input_path in "${input_paths[@]}"; do
    if [[ ! -f "$input_path" ]]; then
      missing_files+=("$input_path")
    fi
  done

  if (( ${#missing_files[@]} > 0 )); then
    echo
    echo "Import failed."
    echo "These files were not found:"
    for missing_file in "${missing_files[@]}"; do
      echo "$missing_file"
    done
    echo
    echo "Press Enter to choose another file, or type q then Enter to close."
    read -r "next_action?> "
    if [[ "$next_action" == "q" || "$next_action" == "Q" ]]; then
      echo
      echo "Importer closed. You can close this Terminal window."
      exit 1
    fi
    echo
    continue
  fi

  echo
  echo "Running importer..."
  echo

  failures=0

  for input_path in "${input_paths[@]}"; do
    echo "Importing:"
    echo "$input_path"
    echo

    if Rscript scripts/import_medium_path_router.R "$input_path"; then
      echo
      echo "Import finished successfully for:"
      echo "$input_path"
    else
      importer_status=$?
      failures=$((failures + 1))
      echo
      echo "Import failed with exit code $importer_status for:"
      echo "$input_path"
    fi

    echo
  done

  if (( failures == 0 )); then
    echo "All imports finished successfully."
  else
    echo "$failures import(s) failed."
  fi

  echo
  echo "Press Enter to import another file, or type q then Enter to close."
  read -r "next_action?> "
  if [[ "$next_action" == "q" || "$next_action" == "Q" ]]; then
    echo
    echo "Importer closed. You can close this Terminal window."
    exit 0
  fi
  echo
done
