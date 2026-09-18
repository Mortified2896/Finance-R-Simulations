#!/usr/bin/env bash
set -euo pipefail

echo "Verifying clean installable structure and lightweight checks."

if [ ! -d ".git" ] && [ ! -f ".git" ]; then
  echo "This script must run from a Git worktree." >&2
  exit 1
fi

if [ -f "package.json" ]; then
  echo "Detected package.json."
  if [ -f "package-lock.json" ]; then
    npm ci --ignore-scripts
  else
    npm install --ignore-scripts
  fi

  ran_check=0
  for script in test:tag-bookmarklet test:search-tags test:tag-watcher; do
    if npm run "$script" --if-present; then
      ran_check=1
    fi
  done

  if [ "$ran_check" -eq 0 ]; then
    echo "No automated test command was detected."
  fi
else
  echo "No package.json found. No automated test command was detected."
fi
