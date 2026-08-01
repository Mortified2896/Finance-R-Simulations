#!/bin/zsh

# Double-click this file on macOS to rate Medium preview cards in a local Shiny app.
# Ratings are saved to data/db/medium_articles.sqlite.

cd "$(dirname "$0")/../.." || exit 1

echo "Starting Medium Preview Rating app..."
echo

if ! Rscript -e 'quit(status = ifelse(requireNamespace("shiny", quietly = TRUE), 0, 1))'; then
  echo "The R package 'shiny' is not installed."
  echo
  echo "Install it in R with:"
  echo 'install.packages("shiny")'
  echo
  echo "Press Return to close this Terminal window."
  read -r
  exit 1
fi

MEDIUM_PROJECT_ROOT="$PWD" Rscript -e 'shiny::runApp("apps/human_preview_rating_app", launch.browser = TRUE, host = "127.0.0.1", port = 3840)'
status=$?

echo
if [ "$status" -eq 0 ]; then
  echo "Preview rating app closed. You can close this Terminal window."
else
  echo "Preview rating app failed with exit code $status."
  echo "Press Return to close this Terminal window."
  read -r
fi
