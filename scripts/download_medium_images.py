#!/usr/bin/env python3
"""Conservative batch downloader for previously collected Medium image URLs.

This script intentionally reads the exported image queue CSV. It does not query
Medium pages, modify SQLite, or participate in normal tracking/import.
"""

from __future__ import annotations

import argparse
import csv
import mimetypes
import random
import re
import time
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import urlparse
from urllib.request import Request, urlopen


DEFAULT_INPUT = Path("data/analysis/medium_images/medium_image_download_queue.csv")
DEFAULT_OUTPUT_DIR = Path("data/analysis/medium_images/downloaded")
DEFAULT_URL_COLUMN = "primary_image_url_for_download"
DEFAULT_LIMIT = 25
DEFAULT_SLEEP_MIN = 8.0
DEFAULT_SLEEP_MAX = 20.0
DEFAULT_TIMEOUT = 30
DEFAULT_RETRIES = 2
DEFAULT_MAX_BYTES = 5 * 1024 * 1024
DEFAULT_STOP_STATUSES = {401, 402, 403, 429, 451}
DEFAULT_SKIP_STATUSES = {404, 410}
DEFAULT_RETRY_STATUSES = {500, 502, 503, 504}
DEFAULT_MAX_CONSECUTIVE_5XX_FAILURES = 3
DEFAULT_USER_AGENT = (
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
    "AppleWebKit/537.36 (KHTML, like Gecko) "
    "Chrome/124.0 Safari/537.36"
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Download a small, rate-limited batch of Medium image URLs from the exported queue."
    )
    parser.add_argument("--input", default=str(DEFAULT_INPUT), help=f"Queue CSV path. Default: {DEFAULT_INPUT}")
    parser.add_argument("--output-dir", default=str(DEFAULT_OUTPUT_DIR), help=f"Image output dir. Default: {DEFAULT_OUTPUT_DIR}")
    parser.add_argument("--url-column", default=DEFAULT_URL_COLUMN, help=f"Queue URL column. Default: {DEFAULT_URL_COLUMN}")
    parser.add_argument("--limit", type=int, default=DEFAULT_LIMIT, help=f"Max images to download. Default: {DEFAULT_LIMIT}")
    parser.add_argument("--all", action="store_true", help="Download all pending images, still one at a time with sleeps.")
    parser.add_argument("--sleep-min", type=float, default=DEFAULT_SLEEP_MIN, help=f"Minimum delay between downloads. Default: {DEFAULT_SLEEP_MIN}")
    parser.add_argument("--sleep-max", type=float, default=DEFAULT_SLEEP_MAX, help=f"Maximum delay between downloads. Default: {DEFAULT_SLEEP_MAX}")
    parser.add_argument("--timeout", type=int, default=DEFAULT_TIMEOUT, help=f"Request timeout seconds. Default: {DEFAULT_TIMEOUT}")
    parser.add_argument("--retries", type=int, default=DEFAULT_RETRIES, help=f"Retries for 5xx/timeouts/resets. Default: {DEFAULT_RETRIES}")
    parser.add_argument(
        "--max-bytes",
        type=int,
        default=DEFAULT_MAX_BYTES,
        help=f"Maximum image response size in bytes. Default: {DEFAULT_MAX_BYTES}",
    )
    parser.add_argument(
        "--stop-statuses",
        default="401,402,403,429,451",
        help="Comma-separated HTTP statuses that stop the batch. Default: 401,402,403,429,451",
    )
    parser.add_argument(
        "--max-consecutive-5xx-failures",
        type=int,
        default=DEFAULT_MAX_CONSECUTIVE_5XX_FAILURES,
        help=f"Stop after this many consecutive images fail with 5xx after retries. Default: {DEFAULT_MAX_CONSECUTIVE_5XX_FAILURES}",
    )
    parser.add_argument("--user-agent", default=DEFAULT_USER_AGENT, help="HTTP User-Agent header.")
    parser.add_argument("--dry-run", action="store_true", help="Print planned downloads without network calls or file writes.")
    parser.add_argument("--overwrite", action="store_true", help="Re-download images even if the local file already exists.")
    return parser.parse_args()


def clean_text(value: str | None) -> str:
    return re.sub(r"\s+", " ", str(value or "")).strip()


def parse_stop_statuses(value: str) -> set[int]:
    statuses: set[int] = set()
    for part in value.split(","):
        part = part.strip()
        if not part:
            continue
        statuses.add(int(part))
    return statuses


def validate_image_url(url: str) -> str:
    parsed = urlparse(url)
    if parsed.scheme not in {"http", "https"} or not parsed.netloc:
        return "URL must be absolute HTTP(S)"
    return ""


def read_queue(path: Path) -> tuple[list[dict[str, str]], list[str]]:
    if not path.exists():
      raise SystemExit(f"Queue CSV not found: {path}. Run scripts/export_medium_image_download_queue.R first.")

    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        rows = list(reader)
        fieldnames = list(reader.fieldnames or [])

    required = ["image_file_stem", "download_status", "local_image_path", "notes"]
    missing = [column for column in required if column not in fieldnames]
    if missing:
        raise SystemExit(f"Queue CSV is missing required column(s): {', '.join(missing)}")

    return rows, fieldnames


def is_already_downloaded(row: dict[str, str]) -> bool:
    status = clean_text(row.get("download_status")).lower()
    local_path = clean_text(row.get("local_image_path"))
    return status == "downloaded" and local_path and Path(local_path).exists()


def pending_rows(rows: list[dict[str, str]], overwrite: bool, url_column: str) -> list[dict[str, str]]:
    pending: list[dict[str, str]] = []
    for row in rows:
        url = clean_text(row.get(url_column))
        if not url:
            continue
        if not overwrite and is_already_downloaded(row):
            continue
        pending.append(row)
    return pending


def extension_from_url(url: str) -> str:
    path = urlparse(url).path
    suffix = Path(path).suffix.lower()
    if suffix in {".jpg", ".jpeg", ".png", ".gif", ".webp"}:
        return ".jpg" if suffix == ".jpeg" else suffix
    return ""


def extension_from_content_type(content_type: str) -> str:
    mime = clean_text(content_type).split(";")[0].lower()
    if mime == "image/jpeg":
        return ".jpg"
    guessed = mimetypes.guess_extension(mime) or ""
    return ".jpg" if guessed == ".jpe" else guessed


def safe_stem(value: str, fallback_index: int) -> str:
    stem = clean_text(value) or f"medium_image_{fallback_index:05d}"
    stem = re.sub(r"[^A-Za-z0-9._-]+", "_", stem)
    stem = re.sub(r"_+", "_", stem).strip("._-")
    return stem or f"medium_image_{fallback_index:05d}"


def destination_path(row: dict[str, str], output_dir: Path, fallback_index: int, content_type: str = "") -> Path:
    url = clean_text(row.get("primary_image_url_for_download") or row.get("body_image_url"))
    extension = extension_from_content_type(content_type) or extension_from_url(url) or ".img"
    return output_dir / f"{safe_stem(row.get('image_file_stem', ''), fallback_index)}{extension}"


def request_image(url: str, user_agent: str, timeout: int):
    request = Request(
        url,
        headers={
            "User-Agent": user_agent,
            "Accept": "image/avif,image/webp,image/apng,image/svg+xml,image/*,*/*;q=0.8",
        },
    )
    return urlopen(request, timeout=timeout)


def read_limited(response, max_bytes: int) -> bytes:
    chunks: list[bytes] = []
    total = 0
    while True:
        chunk = response.read(64 * 1024)
        if not chunk:
            break
        total += len(chunk)
        if total > max_bytes:
            raise ValueError(f"Image response exceeded max-bytes limit ({max_bytes})")
        chunks.append(chunk)
    return b"".join(chunks)


def parse_content_length(value: str) -> int | None:
    cleaned = clean_text(value)
    if not cleaned:
        return None
    try:
        return int(cleaned)
    except ValueError:
        return None


def should_retry_http_status(status: int) -> bool:
    return status in DEFAULT_RETRY_STATUSES


def is_transient_network_error(error: BaseException) -> bool:
    message = str(error).lower()
    return (
        isinstance(error, TimeoutError)
        or isinstance(error, URLError)
        or "timed out" in message
        or "timeout" in message
        or "connection reset" in message
        or "connection aborted" in message
    )


def write_queue(path: Path, rows: list[dict[str, str]], fieldnames: list[str]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    args = parse_args()
    queue_path = Path(args.input)
    output_dir = Path(args.output_dir)
    stop_statuses = parse_stop_statuses(args.stop_statuses)

    if args.limit < 1 and not args.all:
        raise SystemExit("--limit must be at least 1 unless --all is set.")
    if args.retries < 0:
        raise SystemExit("--retries must be non-negative.")
    if args.max_bytes < 1:
        raise SystemExit("--max-bytes must be at least 1.")
    if args.max_consecutive_5xx_failures < 1:
        raise SystemExit("--max-consecutive-5xx-failures must be at least 1.")
    if args.sleep_min < 0 or args.sleep_max < 0 or args.sleep_max < args.sleep_min:
        raise SystemExit("--sleep-min and --sleep-max must be non-negative, with sleep-max >= sleep-min.")

    rows, fieldnames = read_queue(queue_path)
    if args.url_column not in fieldnames:
        raise SystemExit(f"Queue CSV is missing URL column: {args.url_column}")
    candidates = pending_rows(rows, overwrite=args.overwrite, url_column=args.url_column)
    selected = candidates if args.all else candidates[: args.limit]

    print("Medium Image Downloader")
    print("=======================")
    print(f"Queue rows: {len(rows)}")
    print(f"Pending rows: {len(candidates)}")
    print(f"Selected rows: {len(selected)}")
    print(f"URL column: {args.url_column}")
    print(f"Output directory: {output_dir}")
    print(f"Sleep window: {args.sleep_min:.1f}-{args.sleep_max:.1f}s")
    print(f"Retries for transient failures: {args.retries}")
    print(f"Max image bytes: {args.max_bytes}")
    print(f"Stop statuses: {','.join(str(status) for status in sorted(stop_statuses))}")
    print(f"Dry run: {args.dry_run}")

    if not selected:
        print("No pending image URLs to download.")
        return 0

    if args.dry_run:
        for index, row in enumerate(selected, start=1):
            print(f"[dry-run] {index}. {clean_text(row.get(args.url_column))}")
        return 0

    output_dir.mkdir(parents=True, exist_ok=True)
    downloaded = 0
    failed = 0
    skipped = 0
    consecutive_5xx_failures = 0

    for batch_index, row in enumerate(selected, start=1):
        url = clean_text(row.get(args.url_column))
        print(f"[{batch_index}/{len(selected)}] Downloading {url}")

        invalid_reason = validate_image_url(url)
        if invalid_reason:
            row["download_status"] = "invalid_url"
            row["notes"] = invalid_reason
            skipped += 1
            write_queue(queue_path, rows, fieldnames)
            print(f"Skipping invalid URL: {invalid_reason}")
            continue

        last_error = ""
        row_finished = False
        for attempt in range(args.retries + 1):
            try:
                with request_image(url, args.user_agent, args.timeout) as response:
                    status = getattr(response, "status", 200)
                    content_type = response.headers.get("content-type", "")
                    if status in stop_statuses:
                        row["download_status"] = f"stopped_http_{status}"
                        row["notes"] = f"Stopped batch on HTTP {status}"
                        print(f"Stopping on HTTP {status}.")
                        row_finished = True
                        write_queue(queue_path, rows, fieldnames)
                        return 1
                    if status in DEFAULT_SKIP_STATUSES:
                        row["download_status"] = f"skipped_http_{status}"
                        row["notes"] = f"Skipped missing image HTTP {status}"
                        skipped += 1
                        consecutive_5xx_failures = 0
                        row_finished = True
                        break
                    if should_retry_http_status(status):
                        last_error = f"HTTP {status}"
                        if attempt < args.retries:
                            print(f"{last_error}; retrying ({attempt + 1}/{args.retries})")
                            time.sleep(random.uniform(args.sleep_min, args.sleep_max))
                            continue
                        row["download_status"] = f"http_{status}"
                        row["notes"] = f"{last_error} after {args.retries} retries"
                        failed += 1
                        consecutive_5xx_failures += 1
                        row_finished = True
                        break
                    if status < 200 or status >= 300:
                        row["download_status"] = f"http_{status}"
                        row["notes"] = f"HTTP {status}"
                        failed += 1
                        consecutive_5xx_failures = 0
                        row_finished = True
                        break

                    if not clean_text(content_type).lower().startswith("image/"):
                        row["download_status"] = "not_image"
                        row["notes"] = f"Unexpected content-type: {content_type}"
                        skipped += 1
                        consecutive_5xx_failures = 0
                        row_finished = True
                        break

                    response_bytes = parse_content_length(response.headers.get("content-length", ""))
                    if response_bytes is not None:
                        if response_bytes > args.max_bytes:
                            row["download_status"] = "skipped_too_large"
                            row["notes"] = f"Content-Length {response_bytes} exceeds max-bytes {args.max_bytes}"
                            skipped += 1
                            consecutive_5xx_failures = 0
                            row_finished = True
                            print(f"Skipping oversized image: {response_bytes} bytes")
                            break

                    data = read_limited(response, args.max_bytes)
                    destination = destination_path(row, output_dir, batch_index, content_type)
                    if destination.exists() and not args.overwrite:
                        row["download_status"] = "downloaded"
                        row["local_image_path"] = str(destination)
                        row["notes"] = "File already existed"
                        print(f"Already exists: {destination}")
                    else:
                        destination.write_bytes(data)
                        row["download_status"] = "downloaded"
                        row["local_image_path"] = str(destination)
                        row["notes"] = f"{len(data)} bytes; {content_type}"
                        print(f"Saved: {destination} ({len(data)} bytes)")
                    downloaded += 1
                    consecutive_5xx_failures = 0
                    row_finished = True
                    break
            except HTTPError as error:
                if error.code in stop_statuses:
                    row["download_status"] = f"stopped_http_{error.code}"
                    row["notes"] = str(error)
                    print(f"Stopping on HTTP {error.code}.")
                    write_queue(queue_path, rows, fieldnames)
                    return 1
                if error.code in DEFAULT_SKIP_STATUSES:
                    row["download_status"] = f"skipped_http_{error.code}"
                    row["notes"] = str(error)
                    skipped += 1
                    consecutive_5xx_failures = 0
                    row_finished = True
                    print(f"Skipping HTTP {error.code}.")
                    break
                if should_retry_http_status(error.code):
                    last_error = f"HTTP {error.code}: {error}"
                    if attempt < args.retries:
                        print(f"{last_error}; retrying ({attempt + 1}/{args.retries})")
                        time.sleep(random.uniform(args.sleep_min, args.sleep_max))
                        continue
                    row["download_status"] = f"http_{error.code}"
                    row["notes"] = f"{last_error} after {args.retries} retries"
                    failed += 1
                    consecutive_5xx_failures += 1
                    row_finished = True
                    print(f"Failed after retries: HTTP {error.code}")
                    break

                row["download_status"] = f"http_{error.code}"
                row["notes"] = str(error)
                failed += 1
                consecutive_5xx_failures = 0
                row_finished = True
                print(f"HTTP error {error.code}")
                break
            except ValueError as error:
                row["download_status"] = "skipped_too_large"
                row["notes"] = str(error)
                skipped += 1
                consecutive_5xx_failures = 0
                row_finished = True
                print(f"Skipping oversized image: {error}")
                break
            except (URLError, TimeoutError, OSError) as error:
                last_error = str(error)
                if is_transient_network_error(error) and attempt < args.retries:
                    print(f"Transient network error; retrying ({attempt + 1}/{args.retries}): {error}")
                    time.sleep(random.uniform(args.sleep_min, args.sleep_max))
                    continue
                row["download_status"] = "failed"
                row["notes"] = f"{last_error} after {attempt} retries"
                failed += 1
                consecutive_5xx_failures = 0
                row_finished = True
                print(f"Failed: {error}")
                break

        if not row_finished:
            row["download_status"] = "failed"
            row["notes"] = last_error or "Unknown failure"
            failed += 1

        if consecutive_5xx_failures >= args.max_consecutive_5xx_failures:
            row["notes"] = paste_note = (
                f"{row.get('notes', '')}; stopping after "
                f"{consecutive_5xx_failures} consecutive 5xx failures"
            ).strip("; ")
            print(paste_note)
            write_queue(queue_path, rows, fieldnames)
            break

        write_queue(queue_path, rows, fieldnames)

        if batch_index < len(selected):
            sleep_seconds = random.uniform(args.sleep_min, args.sleep_max)
            print(f"Sleeping {sleep_seconds:.1f}s")
            time.sleep(sleep_seconds)

    write_queue(queue_path, rows, fieldnames)
    print(f"Downloaded: {downloaded}")
    print(f"Skipped: {skipped}")
    print(f"Failed: {failed}")
    print(f"Updated queue: {queue_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
