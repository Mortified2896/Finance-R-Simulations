#!/usr/bin/env python3
"""
Score Medium thumbnails with optional title/subtitle context.

This setup script reads from v_medium_title_prediction_dataset_v2 and writes to
medium_thumbnail_api_scores. API inputs intentionally exclude all outcome,
rank, observation, date, and publication-performance fields.
"""

from __future__ import annotations

import argparse
import base64
import csv
import hashlib
import json
import mimetypes
import os
import sqlite3
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Any
from urllib.parse import urlparse


DEFAULT_DB = Path("data/db/medium_articles.sqlite")
DEFAULT_MODEL = os.environ.get("OPENAI_THUMBNAIL_SCORING_MODEL", "gpt-5-mini")
DEFAULT_PROMPT_VERSION = "thumbnail_v1"
DEFAULT_VALIDATED_PROMPT_VERSION = "thumbnail_v1_validated"
DEFAULT_SCOPE = "thumbnail_only"
DEFAULT_SAMPLE_FILE = Path("data/analysis/title_api_score_samples/thumbnail_100_v1.csv")
DEFAULT_IMAGE_INPUT_MODE = "auto"
API_URL = "https://api.openai.com/v1/responses"
THUMBNAIL_QUEUE = Path("data/analysis/medium_images/medium_image_download_queue.csv")

VALID_SCOPES = {"thumbnail_only", "title_thumbnail", "title_subtitle_thumbnail"}
VALID_IMAGE_INPUT_MODES = {"auto", "local_base64", "remote_url"}
SCORE_FIELDS = [
    "visual_clarity",
    "visual_hook",
    "visual_relevance",
    "visual_distinctiveness",
    "professional_credibility",
    "emotional_pull_visual",
    "finance_topic_fit",
    "generic_stock_photo_risk",
    "ai_or_low_quality_risk",
    "text_readability",
    "overall_thumbnail_potential",
]


def clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = " ".join(str(value).replace("\u00a0", " ").split()).strip()
    return text or None


def text_hash(value: str | None) -> str:
    normalized = clean_text(value) or ""
    return hashlib.sha256(normalized.encode("utf-8")).hexdigest()


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def utc_now() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Score Medium thumbnails with OpenAI and cache in SQLite.")
    parser.add_argument("--db", default=str(DEFAULT_DB), help="SQLite DB path.")
    parser.add_argument("--limit", type=int, default=10, help="Maximum new rows to score. Keep this small for tests.")
    parser.add_argument("--dry-run", action="store_true", help="Print candidate/payload preview and do not call the API.")
    parser.add_argument("--force", action="store_true", help="Ignore cache and rescore selected rows.")
    parser.add_argument("--model", default=DEFAULT_MODEL, help="OpenAI model.")
    parser.add_argument("--prompt-version", default=None, help="Prompt/cache version.")
    parser.add_argument("--scope", default=DEFAULT_SCOPE, choices=sorted(VALID_SCOPES), help="Allowed input scope.")
    parser.add_argument("--sample-file", default=str(DEFAULT_SAMPLE_FILE), help="Fixed cohort CSV to score.")
    parser.add_argument("--manifest-file", help="Validated thumbnail manifest CSV with local_image_path and image_sha256.")
    parser.add_argument("--image-input-mode", default=DEFAULT_IMAGE_INPUT_MODE, choices=sorted(VALID_IMAGE_INPUT_MODES))
    parser.add_argument("--max-retries", type=int, default=3, help="Retries for transient API failures.")
    args = parser.parse_args()
    if args.prompt_version is None:
        args.prompt_version = DEFAULT_VALIDATED_PROMPT_VERSION if args.manifest_file else DEFAULT_PROMPT_VERSION
    if args.manifest_file and args.image_input_mode == "remote_url":
        raise SystemExit("--manifest-file requires local image scoring; use --image-input-mode auto or local_base64.")
    return args


def connect_db(path: Path) -> sqlite3.Connection:
    if not path.exists():
        raise SystemExit(f"Could not find database at: {path}")
    connection = sqlite3.connect(path)
    connection.row_factory = sqlite3.Row
    return connection


def ensure_objects(connection: sqlite3.Connection) -> None:
    names = {
        row["name"]
        for row in connection.execute("SELECT name FROM sqlite_master WHERE type IN ('table', 'view')")
    }
    required = {"v_medium_title_prediction_dataset_v2", "medium_thumbnail_api_scores"}
    missing = sorted(required - names)
    if missing:
        raise SystemExit(
            "Missing required object(s): "
            + ", ".join(missing)
            + ". Run: Rscript scripts/apply_medium_analysis_v2_schema.R"
        )


def split_multi(value: str | None) -> set[str]:
    text = clean_text(value)
    if not text:
        return set()
    parts = []
    for chunk in text.replace("|", ",").replace(";", ",").split(","):
        part = clean_text(chunk)
        if part:
            parts.append(part)
    return set(parts)


def load_thumbnail_queue(path: Path = THUMBNAIL_QUEUE) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def local_path_matches_stem(local_path: str | None, image_file_stem: str | None) -> bool:
    path_text = clean_text(local_path)
    expected_stem = clean_text(image_file_stem)
    if not path_text or not expected_stem:
        return False
    return Path(path_text).stem.startswith(expected_stem)


def local_path_from_queue(row: sqlite3.Row, queue_rows: list[dict[str, str]]) -> str | None:
    thumbnail_url = clean_text(row["thumbnail_url"])
    article_id = clean_text(row["article_id"])
    medium_post_id = clean_text(row["medium_post_id"])

    valid_rows: list[tuple[dict[str, str], str]] = []
    for queue_row in queue_rows:
        local_path = clean_text(queue_row.get("local_image_path"))
        if (
            not local_path
            or not Path(local_path).exists()
            or not local_path_matches_stem(local_path, queue_row.get("image_file_stem"))
        ):
            continue
        valid_rows.append((queue_row, local_path))

    for queue_row, local_path in valid_rows:
        queue_urls = {
            clean_text(queue_row.get("primary_image_url_for_download")),
            clean_text(queue_row.get("normalized_image_url")),
        }
        if thumbnail_url and thumbnail_url in queue_urls:
            return local_path

    for queue_row, local_path in valid_rows:
        if article_id and article_id in split_multi(queue_row.get("article_ids")):
            return local_path
        if medium_post_id and medium_post_id in split_multi(queue_row.get("medium_post_ids")):
            return local_path
    return None


def fetch_dataset_rows(connection: sqlite3.Connection) -> list[sqlite3.Row]:
    return connection.execute(
        """
        SELECT
          canonical_article_key,
          article_id,
          medium_post_id,
          title,
          subtitle,
          thumbnail_url,
          has_thumbnail_url
        FROM v_medium_title_prediction_dataset_v2
        WHERE NULLIF(TRIM(thumbnail_url), '') IS NOT NULL
        ORDER BY canonical_article_key
        """
    ).fetchall()


def load_sample_rows(connection: sqlite3.Connection, sample_path: Path) -> list[sqlite3.Row]:
    if not sample_path.exists():
        raise SystemExit(f"Sample file not found: {sample_path}")
    with sample_path.open(newline="", encoding="utf-8") as handle:
        sample_records = list(csv.DictReader(handle))
    if not sample_records:
        return []

    dataset_rows = fetch_dataset_rows(connection)
    by_canonical = {str(row["canonical_article_key"]): row for row in dataset_rows if row["canonical_article_key"] is not None}
    by_article_id = {str(row["article_id"]): row for row in dataset_rows if row["article_id"] is not None}
    by_post_id = {str(row["medium_post_id"]): row for row in dataset_rows if clean_text(row["medium_post_id"])}

    rows: list[sqlite3.Row] = []
    seen_keys: set[str] = set()
    for record in sample_records:
        matched = None
        canonical_key = clean_text(record.get("canonical_article_key"))
        article_id = clean_text(record.get("article_id"))
        medium_post_id = clean_text(record.get("medium_post_id"))
        if canonical_key and canonical_key in by_canonical:
            matched = by_canonical[canonical_key]
        elif article_id and article_id in by_article_id:
            matched = by_article_id[article_id]
        elif medium_post_id and medium_post_id in by_post_id:
            matched = by_post_id[medium_post_id]
        if matched is None:
            continue
        key = str(matched["canonical_article_key"])
        if key in seen_keys:
            continue
        seen_keys.add(key)
        rows.append(matched)
    return rows


def manifest_record_key(record: dict[str, str]) -> str | None:
    canonical = clean_text(record.get("canonical_article_key"))
    article_id = clean_text(record.get("article_id"))
    medium_post_id = clean_text(record.get("medium_post_id"))
    if canonical:
        return f"canonical:{canonical}"
    if article_id:
        return f"article:{article_id}"
    if medium_post_id:
        return f"post:{medium_post_id}"
    return None


def load_manifest_records(manifest_path: Path) -> list[dict[str, str]]:
    if not manifest_path.exists():
        raise SystemExit(f"Manifest file not found: {manifest_path}")
    with manifest_path.open(newline="", encoding="utf-8-sig") as handle:
        records = list(csv.DictReader(handle))
    required = {
        "canonical_article_key",
        "article_id",
        "medium_post_id",
        "title",
        "subtitle",
        "thumbnail_url",
        "local_image_path",
        "image_sha256",
        "thumbnail_status",
    }
    missing = sorted(required - set(records[0].keys())) if records else []
    if missing:
        raise SystemExit(f"Manifest is missing required column(s): {', '.join(missing)}")

    rows: list[dict[str, str]] = []
    seen_keys: set[str] = set()
    for record in records:
        if clean_text(record.get("thumbnail_status")) != "valid":
            continue
        key = manifest_record_key(record)
        if not key or key in seen_keys:
            continue
        seen_keys.add(key)
        local_path_text = clean_text(record.get("local_image_path"))
        expected_hash = clean_text(record.get("image_sha256"))
        if not local_path_text:
            raise SystemExit(f"Manifest valid row has no local_image_path for {key}")
        if not expected_hash:
            raise SystemExit(f"Manifest valid row has no image_sha256 for {key}")
        local_path = Path(local_path_text)
        if not local_path.exists():
            raise SystemExit(f"Manifest image file is missing for {key}: {local_path_text}")
        actual_hash = file_sha256(local_path)
        if actual_hash != expected_hash:
            raise SystemExit(
                f"Manifest image hash mismatch for {key}: expected {expected_hash}, got {actual_hash}"
            )
        rows.append(
            {
                "canonical_article_key": clean_text(record.get("canonical_article_key")) or "",
                "article_id": clean_text(record.get("article_id")) or "",
                "medium_post_id": clean_text(record.get("medium_post_id")) or "",
                "title": clean_text(record.get("title")) or "",
                "subtitle": clean_text(record.get("subtitle")) or "",
                "thumbnail_url": clean_text(record.get("thumbnail_url")) or "",
                "has_thumbnail_url": "1" if clean_text(record.get("thumbnail_url")) else "0",
                "local_image_path": local_path_text,
                "image_sha256": expected_hash,
            }
        )
    return rows


def choose_image_input_from_manifest(row: dict[str, str]) -> dict[str, str] | None:
    local_path_text = clean_text(row.get("local_image_path"))
    expected_hash = clean_text(row.get("image_sha256"))
    if not local_path_text or not expected_hash:
        return None
    path = Path(local_path_text)
    if not path.exists():
        return None
    actual_hash = file_sha256(path)
    if actual_hash != expected_hash:
        raise SystemExit(
            f"Manifest image hash mismatch while scoring {row.get('canonical_article_key')}: "
            f"expected {expected_hash}, got {actual_hash}"
        )
    mime = mimetypes.guess_type(path.name)[0] or "image/jpeg"
    with path.open("rb") as handle:
        encoded = base64.b64encode(handle.read()).decode("ascii")
    return {
        "image_url": clean_text(row.get("thumbnail_url")) or "",
        "local_image_path": local_path_text,
        "image_hash": expected_hash,
        "image_input_mode": "local_base64",
        "payload_image_url": f"data:{mime};base64,{encoded}",
    }


def choose_image_input(row: sqlite3.Row, queue_rows: list[dict[str, str]], mode: str) -> dict[str, str] | None:
    thumbnail_url = clean_text(row["thumbnail_url"])
    local_path = local_path_from_queue(row, queue_rows)
    if mode in {"auto", "local_base64"} and local_path:
        path = Path(local_path)
        mime = mimetypes.guess_type(path.name)[0] or "image/jpeg"
        with path.open("rb") as handle:
            encoded = base64.b64encode(handle.read()).decode("ascii")
        return {
            "image_url": thumbnail_url or "",
            "local_image_path": str(path),
            "image_hash": file_sha256(path),
            "image_input_mode": "local_base64",
            "payload_image_url": f"data:{mime};base64,{encoded}",
        }
    if mode == "local_base64":
        return None
    if mode in {"auto", "remote_url"} and thumbnail_url and urlparse(thumbnail_url).scheme in {"http", "https"}:
        return {
            "image_url": thumbnail_url,
            "local_image_path": local_path or "",
            "image_hash": "",
            "image_input_mode": "remote_url",
            "payload_image_url": thumbnail_url,
        }
    return None


def candidate_from_row(row: sqlite3.Row, image_input: dict[str, str], scope: str) -> dict[str, Any] | None:
    title = clean_text(row["title"])
    subtitle = clean_text(row["subtitle"])
    if scope in {"title_thumbnail", "title_subtitle_thumbnail"} and not title:
        return None
    return {
        "row": row,
        "title": title,
        "subtitle": subtitle,
        "title_hash": text_hash(title) if scope in {"title_thumbnail", "title_subtitle_thumbnail"} else "",
        "subtitle_hash": text_hash(subtitle) if scope == "title_subtitle_thumbnail" else "",
        "image": image_input,
    }


def already_scored(connection: sqlite3.Connection, item: dict[str, Any], args: argparse.Namespace) -> bool:
    row = item["row"]
    image = item["image"]
    count = connection.execute(
        """
        SELECT COUNT(*) AS n
        FROM medium_thumbnail_api_scores
        WHERE canonical_article_key = ?
          AND prompt_version = ?
          AND model = ?
          AND score_scope = ?
          AND COALESCE(image_hash, '') = ?
          AND COALESCE(image_url, '') = ?
          AND COALESCE(title_hash, '') = ?
          AND COALESCE(subtitle_hash, '') = ?
        """,
        (
            row["canonical_article_key"],
            args.prompt_version,
            args.model,
            args.scope,
            image.get("image_hash", ""),
            image.get("image_url", ""),
            item["title_hash"],
            item["subtitle_hash"],
        ),
    ).fetchone()["n"]
    return count > 0


def load_candidates(connection: sqlite3.Connection, args: argparse.Namespace) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    if args.manifest_file:
        rows = load_manifest_records(Path(args.manifest_file))
    else:
        rows = load_sample_rows(connection, Path(args.sample_file)) if args.sample_file else fetch_dataset_rows(connection)
    queue_rows = [] if args.manifest_file else load_thumbnail_queue()
    candidates: list[dict[str, Any]] = []
    local_count = 0
    remote_count = 0
    seen_keys: set[str] = set()
    limit = max(args.limit, 0) if args.limit is not None else None
    for row in rows:
        if limit is not None and len(candidates) >= limit:
            break
        key = str(row["canonical_article_key"])
        if key in seen_keys:
            continue
        seen_keys.add(key)
        image_input = choose_image_input_from_manifest(row) if args.manifest_file else choose_image_input(row, queue_rows, args.image_input_mode)
        if image_input is None:
            continue
        item = candidate_from_row(row, image_input, args.scope)
        if item is None:
            continue
        if not args.force and already_scored(connection, item, args):
            continue
        candidates.append(item)
        if image_input["image_input_mode"] == "local_base64":
            local_count += 1
        elif image_input["image_input_mode"] == "remote_url":
            remote_count += 1
    return candidates, {
        "sample_rows_matched": len(rows),
        "queue_rows": len(queue_rows),
        "manifest_file": args.manifest_file or "",
        "local_image_candidates": local_count,
        "remote_image_candidates": remote_count,
    }


def score_schema() -> dict[str, Any]:
    properties: dict[str, Any] = {
        field: {"type": "integer", "minimum": 1, "maximum": 5}
        for field in SCORE_FIELDS
    }
    properties.update(
        {
            "predicted_success_bucket": {"type": "string", "enum": ["low", "medium", "high"]},
            "short_reason": {"type": "string"},
        }
    )
    return {
        "type": "object",
        "properties": properties,
        "required": [*SCORE_FIELDS, "predicted_success_bucket", "short_reason"],
        "additionalProperties": False,
    }


def input_text_for_scope(item: dict[str, Any], prompt_version: str, scope: str) -> str:
    input_fields: dict[str, str] = {}
    if scope in {"title_thumbnail", "title_subtitle_thumbnail"}:
        input_fields["title"] = item["title"] or ""
    if scope == "title_subtitle_thumbnail":
        input_fields["subtitle"] = item["subtitle"] or ""
    context_note = "No title or subtitle is provided. Judge only from the image." if scope == "thumbnail_only" else "Use only the image and listed text fields."
    return (
        f"Prompt version: {prompt_version}\n\n"
        f"Score scope: {scope}\n"
        f"{context_note}\n\n"
        "Allowed text input fields, and no other article data:\n"
        f"{json.dumps(input_fields, ensure_ascii=False, indent=2)}\n\n"
        "Calibrate scores relative to typical Medium personal finance thumbnails, not in isolation.\n\n"
        "Use the full 1-5 scale aggressively:\n"
        "1 = very weak, likely below average\n"
        "2 = below average or generic\n"
        "3 = average / okay\n"
        "4 = clearly above average, likely stronger than most thumbnails\n"
        "5 = exceptional, rare, top-tier visual potential\n\n"
        "Most normal thumbnails should receive 2 or 3. Do not give 4 unless the thumbnail has a clearly strong visual hook, relevance, clarity, and distinctiveness. "
        "Do not give 5 unless the thumbnail looks unusually compelling and would plausibly belong among the strongest thumbnails in the dataset.\n\n"
        "Rubric:\n"
        "- visual_clarity: How clear and easy to understand the image is at Medium feed/card size.\n"
        "- visual_hook: How strongly the thumbnail catches attention visually.\n"
        "- visual_relevance: How relevant the image appears to the article topic based on allowed context. For thumbnail_only, judge topic relevance only from the image itself and do not infer missing text.\n"
        "- visual_distinctiveness: How non-generic and memorable the image feels compared with typical finance thumbnails.\n"
        "- professional_credibility: How polished, trustworthy, and non-scammy the image feels.\n"
        "- emotional_pull_visual: How much the image creates emotion, curiosity, concern, aspiration, surprise, or tension.\n"
        "- finance_topic_fit: How well the image fits personal finance / investing / FI / retirement content.\n"
        "- generic_stock_photo_risk: Risk that the image looks generic, bland, overused, or like a stock-photo cliche. Higher means more risk.\n"
        "- ai_or_low_quality_risk: Risk that the image looks obviously AI-generated, sloppy, distorted, low-quality, or credibility-damaging. Higher means more risk.\n"
        "- text_readability: If the thumbnail contains text, how readable and useful it is at small size. If there is no text, score neutral 3 unless absence of text clearly helps or hurts.\n"
        "- overall_thumbnail_potential: Relative overall thumbnail potential for Medium from the allowed inputs. This should be a ranking judgment, not a quality compliment.\n\n"
        "predicted_success_bucket:\n"
        "- low = likely below median visual contribution\n"
        "- medium = around median to moderately above average\n"
        "- high = likely top 20% visual potential. Use high sparingly.\n\n"
        "Return JSON matching the schema exactly. short_reason must be one short sentence."
    )


def build_payload(model: str, item: dict[str, Any], prompt_version: str, scope: str) -> dict[str, Any]:
    schema_name = f"medium_thumbnail_scores_{prompt_version}_{scope}".replace("-", "_")
    content = [
        {"type": "input_text", "text": input_text_for_scope(item, prompt_version, scope)},
        {"type": "input_image", "image_url": item["image"]["payload_image_url"]},
    ]
    return {
        "model": model,
        "input": [
            {
                "role": "system",
                "content": (
                    "You score Medium finance thumbnails as reader-facing visual packages. "
                    "Use only the allowed inputs for the selected scope. "
                    "Do not infer or use claps, responses, rank, age, publication performance, or observation history. "
                    "Return calibrated JSON scores from 1 to 5."
                ),
            },
            {"role": "user", "content": content},
        ],
        "text": {
            "format": {
                "type": "json_schema",
                "name": schema_name,
                "strict": True,
                "schema": score_schema(),
            }
        },
    }


def sanitized_payload(payload: dict[str, Any]) -> dict[str, Any]:
    clean = json.loads(json.dumps(payload))
    for item in clean.get("input", []):
        content = item.get("content")
        if isinstance(content, list):
            for part in content:
                image_url = part.get("image_url")
                if isinstance(image_url, str) and image_url.startswith("data:") and ";base64," in image_url:
                    prefix = image_url.split(";base64,", 1)[0]
                    part["image_url"] = f"{prefix};base64,<hidden>"
    return clean


def extract_response_text(response: dict[str, Any]) -> str:
    if isinstance(response.get("output_text"), str):
        return response["output_text"]
    chunks: list[str] = []
    for item in response.get("output", []):
        for content in item.get("content", []):
            if content.get("type") in {"output_text", "text"} and isinstance(content.get("text"), str):
                chunks.append(content["text"])
    if not chunks:
        raise ValueError("Could not find output text in API response")
    return "\n".join(chunks)


def call_openai(payload: dict[str, Any], api_key: str, max_retries: int) -> dict[str, Any]:
    encoded = json.dumps(payload).encode("utf-8")
    headers = {"Authorization": f"Bearer {api_key}", "Content-Type": "application/json"}
    for attempt in range(max_retries + 1):
        request = urllib.request.Request(API_URL, data=encoded, headers=headers, method="POST")
        try:
            with urllib.request.urlopen(request, timeout=120) as response:
                return json.loads(response.read().decode("utf-8"))
        except urllib.error.HTTPError as error:
            body = error.read().decode("utf-8", errors="replace")
            if error.code not in {408, 409, 429, 500, 502, 503, 504} or attempt >= max_retries:
                raise RuntimeError(f"OpenAI API request failed with HTTP {error.code}: {body}") from error
        except (urllib.error.URLError, TimeoutError) as error:
            if attempt >= max_retries:
                raise RuntimeError(f"OpenAI API request failed: {error}") from error
        time.sleep(min(30, 2**attempt))
    raise RuntimeError("OpenAI API request failed after retries")


def validate_model_output(parsed: dict[str, Any]) -> None:
    if not isinstance(parsed, dict):
        raise ValueError("Model output is not a JSON object")
    for field in SCORE_FIELDS:
        value = parsed.get(field)
        if not isinstance(value, int) or isinstance(value, bool) or value < 1 or value > 5:
            raise ValueError(f"Invalid {field}: {value!r}")
    if parsed.get("predicted_success_bucket") not in {"low", "medium", "high"}:
        raise ValueError(f"Invalid predicted_success_bucket: {parsed.get('predicted_success_bucket')!r}")
    reason = parsed.get("short_reason")
    if not isinstance(reason, str) or not reason.strip():
        raise ValueError("short_reason must be a non-empty string")


def insert_score(
    connection: sqlite3.Connection,
    item: dict[str, Any],
    args: argparse.Namespace,
    raw_json: dict[str, Any],
    parsed: dict[str, Any],
    error: str | None = None,
) -> None:
    row = item["row"]
    image = item["image"]
    columns = [
        "canonical_article_key",
        "article_id",
        "medium_post_id",
        "prompt_version",
        "model",
        "score_scope",
        "scored_at",
        "title_hash",
        "subtitle_hash",
        "image_url",
        "local_image_path",
        "image_hash",
        "image_input_mode",
        "raw_json",
        *SCORE_FIELDS,
        "predicted_success_bucket",
        "short_reason",
        "error",
    ]
    values = (
        row["canonical_article_key"],
        row["article_id"],
        row["medium_post_id"],
        args.prompt_version,
        args.model,
        args.scope,
        utc_now(),
        item["title_hash"] or None,
        item["subtitle_hash"] or None,
        image.get("image_url") or None,
        image.get("local_image_path") or None,
        image.get("image_hash") or None,
        image.get("image_input_mode") or None,
        json.dumps(raw_json, ensure_ascii=False, sort_keys=True),
        *(parsed.get(field) for field in SCORE_FIELDS),
        parsed.get("predicted_success_bucket"),
        parsed.get("short_reason"),
        error,
    )
    placeholders = ", ".join("?" for _ in columns)
    connection.execute(
        f"INSERT OR REPLACE INTO medium_thumbnail_api_scores ({', '.join(columns)}) VALUES ({placeholders})",
        values,
    )
    connection.commit()


def print_run_header(args: argparse.Namespace, candidates: list[dict[str, Any]], metadata: dict[str, Any]) -> None:
    allowed_fields = ["image"]
    if args.scope in {"title_thumbnail", "title_subtitle_thumbnail"}:
        allowed_fields.append("title")
    if args.scope == "title_subtitle_thumbnail":
        allowed_fields.append("subtitle")
    print("Medium Thumbnail API Scoring V1")
    print("===============================")
    print(f"Prompt version: {args.prompt_version}")
    print(f"Model: {args.model}")
    print(f"Scope: {args.scope}")
    print(f"Manifest file used: {metadata['manifest_file'] or 'none'}")
    print(f"Sample file used: {'none' if metadata['manifest_file'] else (args.sample_file or 'none')}")
    print(f"Image input mode: {args.image_input_mode}")
    print(f"Sample rows matched: {metadata['sample_rows_matched']}")
    print(f"Thumbnail queue rows read: {metadata['queue_rows']}")
    print(f"Number of candidate rows: {len(candidates)}")
    print(f"Number with local image: {metadata['local_image_candidates']}")
    print(f"Number with remote image URL: {metadata['remote_image_candidates']}")
    print(f"Allowed API input fields: {', '.join(allowed_fields)}")
    print("Leakage guard: no claps, responses, success_score, rank, page position, publication performance, dates, observations, times_seen, outcome/distribution fields, or API keys are sent or printed.")


def main() -> int:
    args = parse_args()
    connection = connect_db(Path(args.db))
    try:
        ensure_objects(connection)
        candidates, metadata = load_candidates(connection, args)
        print_run_header(args, candidates, metadata)
        if not candidates:
            return 0
        first_payload = build_payload(args.model, candidates[0], args.prompt_version, args.scope)
        if args.dry_run:
            print("\nDry run payload preview for first candidate:")
            print(json.dumps(sanitized_payload(first_payload), indent=2, ensure_ascii=False))
            print("\nDry run only. No API call was made and no DB rows were written.")
            return 0

        api_key = os.environ.get("OPENAI_API_KEY")
        if not api_key:
            raise SystemExit("OPENAI_API_KEY is not set. Use --dry-run for a no-cost payload check.")

        for index, item in enumerate(candidates, start=1):
            payload = build_payload(args.model, item, args.prompt_version, args.scope)
            response = call_openai(payload, api_key, args.max_retries)
            parsed = json.loads(extract_response_text(response))
            validate_model_output(parsed)
            insert_score(connection, item, args, response, parsed)
            print(f"[{index}/{len(candidates)}] scored {item['row']['canonical_article_key']}")
        print("Scoring complete.")
        return 0
    finally:
        connection.close()


if __name__ == "__main__":
    sys.exit(main())
