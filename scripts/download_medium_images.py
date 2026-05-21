#!/usr/bin/env python3
"""Conservative batch downloader for previously collected Medium image URLs.

This script intentionally reads the exported image queue CSV. It does not query
Medium pages, modify SQLite, or participate in normal tracking/import.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import mimetypes
import random
import re
import shutil
import time
from pathlib import Path
from urllib import robotparser
from urllib.error import HTTPError, URLError
from urllib.parse import urlparse
from urllib.request import Request, urlopen


DEFAULT_INPUT = Path("data/analysis/medium_images/medium_image_download_queue.csv")
DEFAULT_OUTPUT_DIR = Path("data/analysis/medium_images/downloaded")
DEFAULT_URL_COLUMN = "primary_image_url_for_download"
ROBOTS_REFERENCE = Path("01_manual_tools/reference/medium_robots_reference.md")
DEFAULT_LIMIT = 25
DEFAULT_SLEEP_MIN = 5.0
DEFAULT_SLEEP_MAX = 10.0
DEFAULT_TIMEOUT = 30
DEFAULT_RETRIES = 2
DEFAULT_MAX_BYTES = 5 * 1024 * 1024
DEFAULT_STOP_STATUSES = {429}
DEFAULT_SKIP_STATUSES = {401, 402, 404, 410, 451}
DEFAULT_RETRY_STATUSES = {500, 502, 503, 504}
DEFAULT_BATCH_PAUSE_EVERY = 25
DEFAULT_BATCH_PAUSE_SECONDS = 180.0
DEFAULT_RETRY_BACKOFFS = [60.0, 300.0]
DEFAULT_MAX_CONSECUTIVE_403_FAILURES = 3
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
    parser.add_argument("--concurrency", type=int, default=1, help="Download concurrency. Only 1 is supported.")
    parser.add_argument("--sleep-min", type=float, default=DEFAULT_SLEEP_MIN, help=f"Minimum delay between downloads. Default: {DEFAULT_SLEEP_MIN}")
    parser.add_argument("--sleep-max", type=float, default=DEFAULT_SLEEP_MAX, help=f"Maximum delay between downloads. Default: {DEFAULT_SLEEP_MAX}")
    parser.add_argument("--batch-pause-every", type=int, default=DEFAULT_BATCH_PAUSE_EVERY, help=f"Pause after this many successful downloads. Default: {DEFAULT_BATCH_PAUSE_EVERY}")
    parser.add_argument("--batch-pause-seconds", type=float, default=DEFAULT_BATCH_PAUSE_SECONDS, help=f"Pause duration after each batch. Default: {DEFAULT_BATCH_PAUSE_SECONDS}")
    parser.add_argument("--timeout", type=int, default=DEFAULT_TIMEOUT, help=f"Request timeout seconds. Default: {DEFAULT_TIMEOUT}")
    parser.add_argument("--retries", type=int, default=DEFAULT_RETRIES, help=f"Retries for 5xx/timeouts/resets. Default: {DEFAULT_RETRIES}")
    parser.add_argument(
        "--retry-backoffs",
        default=",".join(str(int(value)) for value in DEFAULT_RETRY_BACKOFFS),
        help="Comma-separated retry backoff seconds. Default: 60,300",
    )
    parser.add_argument(
        "--max-bytes",
        type=int,
        default=DEFAULT_MAX_BYTES,
        help=f"Maximum image response size in bytes. Default: {DEFAULT_MAX_BYTES}",
    )
    parser.add_argument(
        "--stop-statuses",
        default="429",
        help="Comma-separated HTTP statuses that stop the batch. Default: 429",
    )
    parser.add_argument(
        "--max-consecutive-403-failures",
        type=int,
        default=DEFAULT_MAX_CONSECUTIVE_403_FAILURES,
        help=f"Stop after this many consecutive HTTP 403 failures. Default: {DEFAULT_MAX_CONSECUTIVE_403_FAILURES}",
    )
    parser.add_argument(
        "--max-consecutive-5xx-failures",
        type=int,
        default=DEFAULT_MAX_CONSECUTIVE_5XX_FAILURES,
        help=f"Stop after this many consecutive images fail with 5xx after retries. Default: {DEFAULT_MAX_CONSECUTIVE_5XX_FAILURES}",
    )
    parser.add_argument("--user-agent", default=DEFAULT_USER_AGENT, help="HTTP User-Agent header.")
    parser.add_argument("--ignore-robots", action="store_true", help="Do not check robots.txt before downloading.")
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


def parse_retry_backoffs(value: str) -> list[float]:
    backoffs: list[float] = []
    for part in value.split(","):
        part = part.strip()
        if not part:
            continue
        seconds = float(part)
        if seconds < 0:
            raise ValueError("retry backoffs must be non-negative")
        backoffs.append(seconds)
    return backoffs


def validate_image_url(url: str) -> str:
    parsed = urlparse(url)
    if parsed.scheme not in {"http", "https"} or not parsed.netloc:
        return "URL must be absolute HTTP(S)"
    if parsed.netloc.lower() == "medium.com" and parsed.path.startswith("/media/"):
        return f"medium.com /media/ paths are disallowed by robots reference: {ROBOTS_REFERENCE}"
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

    for column in ["download_sha256", "duplicate_of_path"]:
        if column not in fieldnames:
            fieldnames.append(column)
            for row in rows:
                row[column] = ""

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


def destination_path_for_duplicate(
    row: dict[str, str],
    output_dir: Path,
    fallback_index: int,
    duplicate_path: Path,
    content_type: str = "",
) -> Path:
    extension = extension_from_content_type(content_type) or duplicate_path.suffix or extension_from_url(
        clean_text(row.get("primary_image_url_for_download") or row.get("body_image_url"))
    ) or ".img"
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


def robots_url_for(url: str) -> str:
    parsed = urlparse(url)
    return f"{parsed.scheme}://{parsed.netloc}/robots.txt"


def robots_can_fetch(url: str, user_agent: str, timeout: int, cache: dict[str, tuple[bool, str | None, robotparser.RobotFileParser | None]]) -> tuple[bool, str]:
    parsed = urlparse(url)
    cache_key = f"{parsed.scheme}://{parsed.netloc}"
    cached = cache.get(cache_key)
    if cached is None:
        robots = robotparser.RobotFileParser()
        robots_url = robots_url_for(url)
        robots.set_url(robots_url)
        try:
            with request_image(robots_url, user_agent, timeout) as response:
                robots.parse(response.read().decode("utf-8", errors="replace").splitlines())
            cached = (True, None, robots)
        except HTTPError as error:
            if error.code in {401, 403}:
                cached = (False, f"robots.txt returned HTTP {error.code}", None)
            elif error.code == 429:
                cached = (False, "robots.txt returned HTTP 429", None)
            elif 400 <= error.code < 500:
                cached = (True, None, None)
            else:
                cached = (False, f"robots.txt unavailable: HTTP {error.code}", None)
        except Exception as error:
            cached = (False, f"robots.txt unavailable: {error}", None)
        cache[cache_key] = cached

    ok, error_message, robots = cached
    if ok and robots is None:
        return True, "robots.txt not found; no disallow rule present"

    if not ok or robots is None:
        return False, error_message or "robots.txt unavailable"

    return robots.can_fetch(user_agent, url), "robots.txt disallows this URL"


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


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


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


def retry_backoff_seconds(backoffs: list[float], attempt: int) -> float:
    if not backoffs:
        return 0.0
    return backoffs[min(attempt, len(backoffs) - 1)]


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


def mark_skipped(row: dict[str, str], status: str, reason: str) -> None:
    row["download_status"] = status
    row["notes"] = reason


def existing_sha_index(rows: list[dict[str, str]]) -> dict[str, str]:
    index: dict[str, str] = {}
    for row in rows:
        local_path = Path(clean_text(row.get("local_image_path")))
        if clean_text(row.get("download_status")).lower() != "downloaded" or not local_path.exists():
            continue
        digest = clean_text(row.get("download_sha256"))
        if not digest:
            try:
                digest = sha256_file(local_path)
                row["download_sha256"] = digest
            except OSError:
                continue
        index.setdefault(digest, str(local_path))
    return index


def materialize_duplicate_image(
    row: dict[str, str],
    output_dir: Path,
    fallback_index: int,
    duplicate_path_value: str,
    digest: str,
    content_type: str,
) -> tuple[Path, str]:
    duplicate_path = Path(clean_text(duplicate_path_value))
    if not duplicate_path.exists():
        raise OSError(f"Duplicate source file does not exist: {duplicate_path}")

    source_digest = sha256_file(duplicate_path)
    if source_digest != digest:
        raise OSError(
            f"Duplicate source SHA-256 mismatch for {duplicate_path}: expected {digest}, got {source_digest}"
        )

    destination = destination_path_for_duplicate(row, output_dir, fallback_index, duplicate_path, content_type)
    if destination == duplicate_path:
        return destination, "duplicate already materialized at expected path"

    if destination.exists():
        destination_digest = sha256_file(destination)
        if destination_digest != digest:
            raise OSError(
                f"Expected duplicate destination exists with different SHA-256: {destination}"
            )
        return destination, "duplicate expected file already existed"

    destination.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(duplicate_path, destination)
    copied_digest = sha256_file(destination)
    if copied_digest != digest:
        try:
            destination.unlink()
        except OSError:
            pass
        raise OSError(f"Copied duplicate SHA-256 mismatch at {destination}")
    return destination, "copied duplicate content to expected path"


def local_path_matches_stem(path_value: str, stem_value: str) -> bool:
    path_text = clean_text(path_value)
    stem_text = safe_stem(stem_value, 0)
    return bool(path_text and stem_text and Path(path_text).stem.startswith(stem_text))


def materialize_existing_duplicate_rows(rows: list[dict[str, str]], output_dir: Path) -> int:
    repaired = 0
    for index, row in enumerate(rows, start=1):
        duplicate_path = clean_text(row.get("duplicate_of_path"))
        if not duplicate_path:
            continue
        local_path = clean_text(row.get("local_image_path"))
        if (
            clean_text(row.get("download_status")).lower() == "downloaded"
            and local_path_matches_stem(local_path, clean_text(row.get("image_file_stem")))
            and Path(local_path).exists()
        ):
            continue

        digest = clean_text(row.get("download_sha256"))
        if not digest:
            try:
                digest = sha256_file(Path(duplicate_path))
            except OSError:
                continue
            row["download_sha256"] = digest

        try:
            destination, duplicate_note = materialize_duplicate_image(
                row,
                output_dir,
                index,
                duplicate_path,
                digest,
                "",
            )
        except OSError as error:
            row["download_status"] = "duplicate_materialization_failed"
            row["notes"] = str(error)
            continue

        row["download_status"] = "downloaded"
        row["local_image_path"] = str(destination)
        row["notes"] = f"{duplicate_note}; duplicate SHA-256 {digest}"
        repaired += 1
    return repaired


def format_duration(seconds: float) -> str:
    seconds = max(0, int(round(seconds)))
    hours, remainder = divmod(seconds, 3600)
    minutes, secs = divmod(remainder, 60)
    if hours:
        return f"{hours}h {minutes:02d}m {secs:02d}s"
    if minutes:
        return f"{minutes}m {secs:02d}s"
    return f"{secs}s"


def estimate_remaining_seconds(
    active_seconds: float,
    completed_rows: int,
    total_rows: int,
    downloaded: int,
    download_sleep_events: int,
    download_sleep_seconds: float,
    retry_events: int,
    retry_sleep_seconds: float,
    batch_pause_every: int,
    batch_pause_seconds: float,
) -> float | None:
    if completed_rows <= 0:
        return None

    remaining_rows = total_rows - completed_rows
    if remaining_rows <= 0:
        return 0.0

    average_active_seconds_per_row = active_seconds / completed_rows
    estimated = average_active_seconds_per_row * remaining_rows

    download_rate = downloaded / completed_rows
    estimated_future_downloads = remaining_rows * download_rate

    if download_sleep_events > 0 and downloaded > 0:
        average_download_sleep = download_sleep_seconds / download_sleep_events
    else:
        average_download_sleep = (DEFAULT_SLEEP_MIN + DEFAULT_SLEEP_MAX) / 2

    estimated += max(0, estimated_future_downloads - 1) * average_download_sleep

    if retry_events > 0:
        retry_rate = retry_events / completed_rows
        average_retry_sleep = retry_sleep_seconds / retry_events
        estimated += remaining_rows * retry_rate * average_retry_sleep

    if batch_pause_every > 0 and batch_pause_seconds > 0:
        next_pause_at = ((downloaded // batch_pause_every) + 1) * batch_pause_every
        remaining_batch_pauses = 0
        if downloaded < next_pause_at <= downloaded + remaining_rows:
            remaining_batch_pauses = ((downloaded + remaining_rows) - next_pause_at) // batch_pause_every + 1
        estimated += remaining_batch_pauses * batch_pause_seconds

    return estimated


def print_progress_estimate(
    started_at: float,
    planned_sleep_seconds: float,
    download_sleep_events: int,
    download_sleep_seconds: float,
    retry_events: int,
    retry_sleep_seconds: float,
    completed_rows: int,
    total_rows: int,
    downloaded: int,
    skipped: int,
    failed: int,
    args: argparse.Namespace,
) -> None:
    elapsed = time.monotonic() - started_at
    active = max(0.0, elapsed - planned_sleep_seconds)
    remaining = estimate_remaining_seconds(
        active_seconds=active,
        completed_rows=completed_rows,
        total_rows=total_rows,
        downloaded=downloaded,
        download_sleep_events=download_sleep_events,
        download_sleep_seconds=download_sleep_seconds,
        retry_events=retry_events,
        retry_sleep_seconds=retry_sleep_seconds,
        batch_pause_every=args.batch_pause_every,
        batch_pause_seconds=args.batch_pause_seconds,
    )
    remaining_text = "estimating" if remaining is None else format_duration(remaining)
    print(
        "Progress: "
        f"{completed_rows}/{total_rows} rows; "
        f"downloaded={downloaded} skipped={skipped} failed={failed}; "
        f"elapsed={format_duration(elapsed)}; "
        f"active={format_duration(active)}; "
        f"ETA={remaining_text}"
    )


def main() -> int:
    args = parse_args()
    queue_path = Path(args.input)
    output_dir = Path(args.output_dir)
    stop_statuses = parse_stop_statuses(args.stop_statuses)
    try:
        retry_backoffs = parse_retry_backoffs(args.retry_backoffs)
    except ValueError as error:
        raise SystemExit(f"--retry-backoffs is invalid: {error}") from error

    if args.limit < 1 and not args.all:
        raise SystemExit("--limit must be at least 1 unless --all is set.")
    if args.concurrency != 1:
        raise SystemExit("--concurrency must be 1; parallel downloading is intentionally disabled.")
    if args.retries < 0:
        raise SystemExit("--retries must be non-negative.")
    if args.max_bytes < 1:
        raise SystemExit("--max-bytes must be at least 1.")
    if args.batch_pause_every < 1:
        raise SystemExit("--batch-pause-every must be at least 1.")
    if args.batch_pause_seconds < 0:
        raise SystemExit("--batch-pause-seconds must be non-negative.")
    if args.max_consecutive_403_failures < 1:
        raise SystemExit("--max-consecutive-403-failures must be at least 1.")
    if args.max_consecutive_5xx_failures < 1:
        raise SystemExit("--max-consecutive-5xx-failures must be at least 1.")
    if args.sleep_min < 0 or args.sleep_max < 0 or args.sleep_max < args.sleep_min:
        raise SystemExit("--sleep-min and --sleep-max must be non-negative, with sleep-max >= sleep-min.")

    rows, fieldnames = read_queue(queue_path)
    if args.url_column not in fieldnames:
        raise SystemExit(f"Queue CSV is missing URL column: {args.url_column}")

    repaired_existing_duplicates = 0
    if not args.dry_run:
        output_dir.mkdir(parents=True, exist_ok=True)
        repaired_existing_duplicates = materialize_existing_duplicate_rows(rows, output_dir)
        if repaired_existing_duplicates:
            write_queue(queue_path, rows, fieldnames)

    candidates = pending_rows(rows, overwrite=args.overwrite, url_column=args.url_column)
    selected = candidates if args.all else candidates[: args.limit]

    print("Medium Image Downloader")
    print("=======================")
    print(f"Queue rows: {len(rows)}")
    print(f"Existing duplicate aliases materialized: {repaired_existing_duplicates}")
    print(f"Pending rows: {len(candidates)}")
    print(f"Selected rows: {len(selected)}")
    print(f"URL column: {args.url_column}")
    print(f"Output directory: {output_dir}")
    print("Concurrency: 1")
    print(f"Sleep window: {args.sleep_min:.1f}-{args.sleep_max:.1f}s")
    print(f"Batch pause: {args.batch_pause_seconds:.1f}s after every {args.batch_pause_every} downloads")
    print(f"Retries for transient failures: {args.retries}")
    print(f"Retry backoffs: {','.join(str(value) for value in retry_backoffs)}s")
    print(f"Max image bytes: {args.max_bytes}")
    print(f"Stop statuses: {','.join(str(status) for status in sorted(stop_statuses))}")
    print(f"Respect robots.txt: {not args.ignore_robots}")
    print(f"Dry run: {args.dry_run}")

    if not selected:
        print("No pending image URLs to download.")
        return 0

    if args.dry_run:
        for index, row in enumerate(selected, start=1):
            print(f"[dry-run] {index}. {clean_text(row.get(args.url_column))}")
        return 0

    output_dir.mkdir(parents=True, exist_ok=True)
    sha_index = existing_sha_index(rows)
    robots_cache: dict[str, tuple[bool, str | None, robotparser.RobotFileParser | None]] = {}
    seen_urls: set[str] = set()
    downloaded = 0
    failed = 0
    skipped = 0
    duplicate_urls = 0
    duplicate_sha256 = 0
    consecutive_403_failures = 0
    consecutive_5xx_failures = 0
    started_at = time.monotonic()
    planned_sleep_seconds = 0.0
    download_sleep_events = 0
    download_sleep_seconds = 0.0
    retry_events = 0
    retry_sleep_seconds = 0.0

    for batch_index, row in enumerate(selected, start=1):
        url = clean_text(row.get(args.url_column))
        print(f"[{batch_index}/{len(selected)}] Downloading {url}")
        row_downloaded = False

        if url in seen_urls:
            mark_skipped(row, "duplicate_url", "Duplicate URL already handled in this run")
            duplicate_urls += 1
            skipped += 1
            write_queue(queue_path, rows, fieldnames)
            print("Skipping duplicate URL already handled in this run.")
            print_progress_estimate(started_at, planned_sleep_seconds, download_sleep_events, download_sleep_seconds, retry_events, retry_sleep_seconds, batch_index, len(selected), downloaded, skipped, failed, args)
            continue
        seen_urls.add(url)

        invalid_reason = validate_image_url(url)
        if invalid_reason:
            mark_skipped(row, "invalid_url", invalid_reason)
            skipped += 1
            write_queue(queue_path, rows, fieldnames)
            print(f"Skipping invalid URL: {invalid_reason}")
            print_progress_estimate(started_at, planned_sleep_seconds, download_sleep_events, download_sleep_seconds, retry_events, retry_sleep_seconds, batch_index, len(selected), downloaded, skipped, failed, args)
            continue

        if not args.ignore_robots:
            can_fetch, robots_reason = robots_can_fetch(url, args.user_agent, args.timeout, robots_cache)
            if not can_fetch:
                mark_skipped(row, "robots_disallowed", robots_reason)
                skipped += 1
                write_queue(queue_path, rows, fieldnames)
                print(f"Skipping robots-disallowed URL: {robots_reason}")
                print_progress_estimate(started_at, planned_sleep_seconds, download_sleep_events, download_sleep_seconds, retry_events, retry_sleep_seconds, batch_index, len(selected), downloaded, skipped, failed, args)
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
                        row["notes"] = f"Stopped batch on HTTP {status}; possible rate limit"
                        print(f"WARNING: stopping the whole run on HTTP {status}; possible rate limit.")
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
                            backoff = retry_backoff_seconds(retry_backoffs, attempt)
                            print(f"{last_error}; retrying in {backoff:.1f}s ({attempt + 1}/{args.retries})")
                            planned_sleep_seconds += backoff
                            retry_events += 1
                            retry_sleep_seconds += backoff
                            time.sleep(backoff)
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
                    digest = sha256_bytes(data)
                    row["download_sha256"] = digest
                    duplicate_path = sha_index.get(digest)
                    if duplicate_path and not args.overwrite:
                        try:
                            destination, duplicate_note = materialize_duplicate_image(
                                row,
                                output_dir,
                                batch_index,
                                duplicate_path,
                                digest,
                                content_type,
                            )
                        except OSError as error:
                            row["download_status"] = "duplicate_materialization_failed"
                            row["duplicate_of_path"] = duplicate_path
                            row["notes"] = str(error)
                            failed += 1
                            consecutive_403_failures = 0
                            consecutive_5xx_failures = 0
                            row_finished = True
                            print(f"Failed to materialize duplicate image content: {error}")
                            break

                        row["download_status"] = "downloaded"
                        row["duplicate_of_path"] = duplicate_path
                        row["local_image_path"] = str(destination)
                        row["notes"] = f"{duplicate_note}; duplicate SHA-256 {digest}"
                        sha_index.setdefault(digest, str(destination))
                        duplicate_sha256 += 1
                        downloaded += 1
                        row_downloaded = True
                        consecutive_403_failures = 0
                        consecutive_5xx_failures = 0
                        row_finished = True
                        print(f"Materialized duplicate image content: {duplicate_path} -> {destination}")
                        break

                    destination = destination_path(row, output_dir, batch_index, content_type)
                    if destination.exists() and not args.overwrite:
                        try:
                            digest = sha256_file(destination)
                            row["download_sha256"] = digest
                            sha_index.setdefault(digest, str(destination))
                        except OSError:
                            pass
                        row["download_status"] = "downloaded"
                        row["local_image_path"] = str(destination)
                        row["notes"] = "File already existed"
                        print(f"Already exists: {destination}")
                    else:
                        destination.write_bytes(data)
                        row["download_status"] = "downloaded"
                        row["local_image_path"] = str(destination)
                        row["notes"] = f"{len(data)} bytes; {content_type}"
                        sha_index.setdefault(digest, str(destination))
                        print(f"Saved: {destination} ({len(data)} bytes)")
                    downloaded += 1
                    row_downloaded = True
                    consecutive_403_failures = 0
                    consecutive_5xx_failures = 0
                    row_finished = True
                    break
            except HTTPError as error:
                if error.code in stop_statuses:
                    row["download_status"] = f"stopped_http_{error.code}"
                    row["notes"] = f"{error}; possible rate limit"
                    print(f"WARNING: stopping the whole run on HTTP {error.code}; possible rate limit.")
                    write_queue(queue_path, rows, fieldnames)
                    return 1
                if error.code == 403:
                    row["download_status"] = "skipped_http_403"
                    row["notes"] = f"Skipped forbidden image HTTP 403: {error}"
                    skipped += 1
                    consecutive_403_failures += 1
                    consecutive_5xx_failures = 0
                    row_finished = True
                    print("Skipping HTTP 403.")
                    break
                if error.code in DEFAULT_SKIP_STATUSES:
                    row["download_status"] = f"skipped_http_{error.code}"
                    row["notes"] = str(error)
                    skipped += 1
                    consecutive_403_failures = 0
                    consecutive_5xx_failures = 0
                    row_finished = True
                    print(f"Skipping HTTP {error.code}.")
                    break
                if should_retry_http_status(error.code):
                    last_error = f"HTTP {error.code}: {error}"
                    if attempt < args.retries:
                        backoff = retry_backoff_seconds(retry_backoffs, attempt)
                        print(f"{last_error}; retrying in {backoff:.1f}s ({attempt + 1}/{args.retries})")
                        planned_sleep_seconds += backoff
                        retry_events += 1
                        retry_sleep_seconds += backoff
                        time.sleep(backoff)
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
                consecutive_403_failures = 0
                consecutive_5xx_failures = 0
                row_finished = True
                print(f"HTTP error {error.code}")
                break
            except ValueError as error:
                row["download_status"] = "skipped_too_large"
                row["notes"] = str(error)
                skipped += 1
                consecutive_403_failures = 0
                consecutive_5xx_failures = 0
                row_finished = True
                print(f"Skipping oversized image: {error}")
                break
            except (URLError, TimeoutError, OSError) as error:
                last_error = str(error)
                if is_transient_network_error(error) and attempt < args.retries:
                    backoff = retry_backoff_seconds(retry_backoffs, attempt)
                    print(f"Transient network error; retrying in {backoff:.1f}s ({attempt + 1}/{args.retries}): {error}")
                    planned_sleep_seconds += backoff
                    retry_events += 1
                    retry_sleep_seconds += backoff
                    time.sleep(backoff)
                    continue
                row["download_status"] = "failed"
                row["notes"] = f"{last_error} after {attempt} retries"
                failed += 1
                consecutive_403_failures = 0
                consecutive_5xx_failures = 0
                row_finished = True
                print(f"Failed: {error}")
                break

        if not row_finished:
            row["download_status"] = "failed"
            row["notes"] = last_error or "Unknown failure"
            failed += 1

        if consecutive_403_failures >= args.max_consecutive_403_failures:
            row["notes"] = paste_note = (
                f"{row.get('notes', '')}; stopping after "
                f"{consecutive_403_failures} consecutive HTTP 403 failures"
            ).strip("; ")
            print(paste_note)
            write_queue(queue_path, rows, fieldnames)
            return 1

        if consecutive_5xx_failures >= args.max_consecutive_5xx_failures:
            row["notes"] = paste_note = (
                f"{row.get('notes', '')}; stopping after "
                f"{consecutive_5xx_failures} consecutive 5xx failures"
            ).strip("; ")
            print(paste_note)
            write_queue(queue_path, rows, fieldnames)
            return 1

        write_queue(queue_path, rows, fieldnames)
        print_progress_estimate(started_at, planned_sleep_seconds, download_sleep_events, download_sleep_seconds, retry_events, retry_sleep_seconds, batch_index, len(selected), downloaded, skipped, failed, args)

        if row_downloaded and batch_index < len(selected):
            if downloaded % args.batch_pause_every == 0:
                print(f"Batch pause after {downloaded} downloads: sleeping {args.batch_pause_seconds:.1f}s")
                planned_sleep_seconds += args.batch_pause_seconds
                time.sleep(args.batch_pause_seconds)
            else:
                sleep_seconds = random.uniform(args.sleep_min, args.sleep_max)
                print(f"Sleeping {sleep_seconds:.1f}s")
                planned_sleep_seconds += sleep_seconds
                download_sleep_events += 1
                download_sleep_seconds += sleep_seconds
                time.sleep(sleep_seconds)

    write_queue(queue_path, rows, fieldnames)
    print(f"Downloaded: {downloaded}")
    print(f"Skipped: {skipped}")
    print(f"Failed: {failed}")
    print(f"Duplicate URLs skipped: {duplicate_urls}")
    print(f"Duplicate SHA-256 images materialized: {duplicate_sha256}")
    print(f"Total elapsed: {format_duration(time.monotonic() - started_at)}")
    print(f"Updated queue: {queue_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
