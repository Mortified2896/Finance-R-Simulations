#!/bin/zsh

# Double-click this file on macOS to rate Medium preview cards with the
# human_preview_dimensions_v1 rubric. Ratings are saved separately from the
# one-score workflow in data/db/medium_articles.sqlite.

cd "$(dirname "$0")/../.." || exit 1

echo "Starting Medium Preview Dimension Rating app..."
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

HUMAN_RATING_MODE=dimensions_v1 MEDIUM_PROJECT_ROOT="$PWD" Rscript -e 'shiny::runApp("apps/human_preview_rating_app", launch.browser = TRUE, host = "127.0.0.1", port = 3838)'
status=$?

echo
if [ "$status" -eq 0 ]; then
  echo "Preview dimension rating app closed. You can close this Terminal window."
else
  echo "Preview dimension rating app failed with exit code $status."
  echo "Press Return to close this Terminal window."
  read -r
fi
