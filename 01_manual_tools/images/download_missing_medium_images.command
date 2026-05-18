#!/bin/zsh

# Double-click this file on macOS to rebuild Medium image download queues and
# download missing thumbnail/body images.

cd "$(dirname "$0")/../.." || exit 1

pause_before_close() {
  echo
  if [[ -t 0 ]]; then
    read -k 1 "close_key?Press any key to close this Terminal window: "
  else
    echo "Press Enter to close this Terminal window."
    read -r close_key
  fi
  echo
}

run_or_stop() {
  "$@"
  command_status=$?
  if [[ "$command_status" != "0" ]]; then
    echo
    echo "Command failed with exit code $command_status:"
    printf '  %q' "$@"
    echo
    pause_before_close
    exit "$command_status"
  fi
}

is_positive_integer_or_all() {
  local value="$1"
  [[ "$value" == "all" || "$value" == "ALL" || "$value" =~ '^[0-9]+$' ]] && [[ "$value" == "all" || "$value" == "ALL" || "$value" -ge 1 ]]
}

is_non_negative_number() {
  [[ "$1" =~ '^[0-9]+([.][0-9]+)?$' ]]
}

limit_args() {
  local limit_value="$1"
  if [[ "$limit_value" == "all" || "$limit_value" == "ALL" ]]; then
    echo "--all"
  else
    echo "--limit $limit_value"
  fi
}

start_awake_guard() {
  caffeinate_pid=""
  if command -v caffeinate >/dev/null 2>&1; then
    caffeinate -dimsu -w $$ &
    caffeinate_pid=$!
    echo
    echo "Keeping this Mac awake while the image download run is active."
    echo "Normal sleep behavior will resume when this command exits."
  else
    echo
    echo "caffeinate was not found; continuing without macOS sleep prevention."
  fi
}

stop_awake_guard() {
  if [[ -n "$caffeinate_pid" ]]; then
    kill "$caffeinate_pid" >/dev/null 2>&1 || true
    wait "$caffeinate_pid" >/dev/null 2>&1 || true
    caffeinate_pid=""
  fi
}

echo "Medium Missing Image Downloader"
echo "==============================="
echo
echo "This rebuilds the image queues, then downloads only rows not already marked downloaded."
echo "Thumbnail images come from the title prediction dataset."
echo "Body images come from imported article text snapshots."
echo "Configure the run before it starts. Press Enter to keep each default."
echo

if [[ -t 0 ]]; then
  read "thumbnail_limit?Thumbnail images to download [all]: "
  read "body_limit?Article-body images to download [all]: "
  read "sleep_min?Minimum sleep between successful downloads, seconds [5]: "
  read "sleep_max?Maximum sleep between successful downloads, seconds [10]: "
  read "batch_pause_every?Pause after this many successful downloads [25]: "
  read "batch_pause_seconds?Batch pause duration, seconds [180]: "
  read "retries?Retries for transient failures [2]: "
  read "retry_backoffs?Retry backoffs, comma-separated seconds [60,300]: "
  read "max_mb?Max image size, MB [5]: "
else
  thumbnail_limit=""
  body_limit=""
  sleep_min=""
  sleep_max=""
  batch_pause_every=""
  batch_pause_seconds=""
  retries=""
  retry_backoffs=""
  max_mb=""
fi

thumbnail_limit="${thumbnail_limit:-all}"
body_limit="${body_limit:-all}"
sleep_min="${sleep_min:-5}"
sleep_max="${sleep_max:-10}"
batch_pause_every="${batch_pause_every:-25}"
batch_pause_seconds="${batch_pause_seconds:-180}"
retries="${retries:-2}"
retry_backoffs="${retry_backoffs:-60,300}"
max_mb="${max_mb:-5}"

if ! is_positive_integer_or_all "$thumbnail_limit"; then
  echo "Thumbnail image count must be 'all' or a positive integer."
  pause_before_close
  exit 1
fi

if ! is_positive_integer_or_all "$body_limit"; then
  echo "Article-body image count must be 'all' or a positive integer."
  pause_before_close
  exit 1
fi

if ! is_non_negative_number "$sleep_min" || ! is_non_negative_number "$sleep_max"; then
  echo "Sleep values must be non-negative numbers."
  pause_before_close
  exit 1
fi

if ! python3 -c 'import sys; raise SystemExit(0 if float(sys.argv[2]) >= float(sys.argv[1]) else 1)' "$sleep_min" "$sleep_max"; then
  echo "Maximum sleep must be greater than or equal to minimum sleep."
  pause_before_close
  exit 1
fi

if ! [[ "$batch_pause_every" =~ '^[0-9]+$' ]] || (( batch_pause_every < 1 )); then
  echo "Batch pause frequency must be a positive integer."
  pause_before_close
  exit 1
fi

if ! is_non_negative_number "$batch_pause_seconds"; then
  echo "Batch pause duration must be a non-negative number."
  pause_before_close
  exit 1
fi

if ! [[ "$retries" =~ '^[0-9]+$' ]]; then
  echo "Retries must be a non-negative integer."
  pause_before_close
  exit 1
fi

if ! python3 -c 'import sys; values=[float(part) for part in sys.argv[1].split(",") if part.strip()]; raise SystemExit(0 if values and all(value >= 0 for value in values) else 1)' "$retry_backoffs" >/dev/null 2>&1; then
  echo "Retry backoffs must be comma-separated non-negative numbers."
  pause_before_close
  exit 1
fi

if ! is_non_negative_number "$max_mb" || [[ "$max_mb" == "0" || "$max_mb" == "0.0" ]]; then
  echo "Max image size must be a positive number of MB."
  pause_before_close
  exit 1
fi

max_bytes=$(python3 -c 'import sys; print(int(float(sys.argv[1]) * 1024 * 1024))' "$max_mb")
thumbnail_limit_display="$thumbnail_limit"
body_limit_display="$body_limit"
thumbnail_limit_args=("${(@s: :)$(limit_args "$thumbnail_limit")}")
body_limit_args=("${(@s: :)$(limit_args "$body_limit")}")

echo
echo "Run settings"
echo "------------"
echo "Thumbnail images: $thumbnail_limit_display"
echo "Article-body images: $body_limit_display"
echo "Concurrency: 1"
echo "Sleep window: ${sleep_min}-${sleep_max}s"
echo "Batch pause: ${batch_pause_seconds}s after every ${batch_pause_every} successful downloads"
echo "Retries: $retries"
echo "Retry backoffs: ${retry_backoffs}s"
echo "Max image size: ${max_mb} MB"
echo "HTTP 429: stop whole run"
echo "Robots.txt: respected"
echo "Medium /media/ paths: skipped"
echo "Mac sleep: prevented only while the run is active"
echo

if [[ -t 0 ]]; then
  read "confirm?Start download run now? [Y/n]: "
  if [[ "$confirm" == "n" || "$confirm" == "N" ]]; then
    echo "Cancelled before any commands ran."
    pause_before_close
    exit 0
  fi
else
  echo "No interactive Terminal input was detected, so no commands were run."
  echo "Double-click this .command file in Finder to review or change settings before starting."
  exit 0
fi

start_awake_guard
trap stop_awake_guard EXIT INT TERM

echo
echo "1/5 Building title prediction dataset for thumbnail URLs..."
run_or_stop Rscript scripts/build_medium_title_prediction_dataset.R

echo
echo "2/5 Exporting thumbnail download queue..."
run_or_stop Rscript scripts/export_medium_image_download_queue.R

echo
echo "3/5 Downloading missing thumbnails, count=$thumbnail_limit_display..."
run_or_stop python3 scripts/download_medium_images.py \
  --input data/analysis/medium_images/medium_image_download_queue.csv \
  --output-dir data/analysis/medium_images/downloaded \
  --url-column primary_image_url_for_download \
  "${thumbnail_limit_args[@]}" \
  --concurrency 1 \
  --sleep-min "$sleep_min" \
  --sleep-max "$sleep_max" \
  --batch-pause-every "$batch_pause_every" \
  --batch-pause-seconds "$batch_pause_seconds" \
  --retries "$retries" \
  --retry-backoffs "$retry_backoffs" \
  --max-bytes "$max_bytes"

echo
echo "4/5 Exporting article-body image download queue..."
run_or_stop Rscript scripts/export_medium_body_image_download_queue.R

echo
echo "5/5 Downloading missing article-body images, count=$body_limit_display..."
run_or_stop python3 scripts/download_medium_images.py \
  --input data/analysis/medium_body_images/medium_body_image_download_queue.csv \
  --output-dir data/analysis/medium_body_images/downloaded \
  --url-column body_image_url \
  "${body_limit_args[@]}" \
  --concurrency 1 \
  --sleep-min "$sleep_min" \
  --sleep-max "$sleep_max" \
  --batch-pause-every "$batch_pause_every" \
  --batch-pause-seconds "$batch_pause_seconds" \
  --retries "$retries" \
  --retry-backoffs "$retry_backoffs" \
  --max-bytes "$max_bytes"

stop_awake_guard
trap - EXIT INT TERM

echo
echo "Done."
echo "Thumbnail queue: data/analysis/medium_images/medium_image_download_queue.csv"
echo "Thumbnail downloads: data/analysis/medium_images/downloaded"
echo "Body image queue: data/analysis/medium_body_images/medium_body_image_download_queue.csv"
echo "Body image downloads: data/analysis/medium_body_images/downloaded"

pause_before_close
