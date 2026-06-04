#!/bin/zsh

# Experimental Article Lab / rating UI design. This launches the same app and
# database as the stable version, but opts into ARTICLE_LAB_UI_VERSION=v2 and
# uses a separate port so the old functional app can keep running.

cd "$(dirname "$0")/../.." || exit 1

echo "Starting experimental Medium Preview Rating app design v2..."
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

ARTICLE_LAB_UI_VERSION=v2 MEDIUM_PROJECT_ROOT="$PWD" Rscript -e 'shiny::runApp("apps/human_preview_rating_app", launch.browser = TRUE, host = "127.0.0.1", port = 3844)'
status=$?

echo
if [ "$status" -eq 0 ]; then
  echo "Experimental preview rating app closed. You can close this Terminal window."
else
  echo "Experimental preview rating app failed with exit code $status."
  echo "Press Return to close this Terminal window."
  read -r
fi
