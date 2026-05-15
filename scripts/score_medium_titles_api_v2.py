#!/usr/bin/env python3
"""
Score Medium title/subtitle pairs against a small structured rubric.

This V2 workflow reads from v_medium_title_prediction_dataset_v2 and writes to
medium_title_api_scores. The API request intentionally contains only the title
and subtitle, never outcome or distribution fields.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import sqlite3
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


DEFAULT_DB = Path("data/db/medium_articles.sqlite")
DEFAULT_MODEL = os.environ.get("OPENAI_TITLE_SCORING_MODEL", "gpt-5-mini")
DEFAULT_PROMPT_VERSION = "v2_1"
API_URL = "https://api.openai.com/v1/responses"

SCORE_FIELDS = [
    "clarity",
    "curiosity",
    "specificity",
    "beginner_appeal",
    "credibility",
    "emotional_pull",
    "promise_strength",
    "medium_clap_potential",
    "medium_comment_potential",
    "overall_article_potential",
    "trust_risk",
]


def clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = " ".join(str(value).replace("\u00a0", " ").split()).strip()
    return text or None


def text_hash(value: str | None) -> str:
    normalized = clean_text(value) or ""
    return hashlib.sha256(normalized.encode("utf-8")).hexdigest()


def utc_now() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Score Medium titles/subtitles with OpenAI and cache in SQLite.")
    parser.add_argument("--db", default=str(DEFAULT_DB), help="SQLite DB path.")
    parser.add_argument("--limit", type=int, default=10, help="Maximum new rows to score. Keep this small for tests.")
    parser.add_argument("--dry-run", action="store_true", help="Print the first payload and do not call the API.")
    parser.add_argument("--force", action="store_true", help="Ignore cache and rescore selected rows.")
    parser.add_argument("--model", default=DEFAULT_MODEL, help="OpenAI model.")
    parser.add_argument("--prompt-version", default=DEFAULT_PROMPT_VERSION, help="Prompt/cache version.")
    parser.add_argument("--max-retries", type=int, default=3, help="Retries for transient API failures.")
    return parser.parse_args()


def connect_db(path: Path) -> sqlite3.Connection:
    if not path.exists():
        raise SystemExit(f"Could not find database at: {path}")
    connection = sqlite3.connect(path)
    connection.row_factory = sqlite3.Row
    return connection


def ensure_objects(connection: sqlite3.Connection) -> None:
    names = {
        row["name"]
        for row in connection.execute(
            "SELECT name FROM sqlite_master WHERE type IN ('table', 'view')"
        )
    }
    required = {"v_medium_title_prediction_dataset_v2", "medium_title_api_scores"}
    missing = sorted(required - names)
    if missing:
        raise SystemExit(
            "Missing required object(s): "
            + ", ".join(missing)
            + ". Run: Rscript scripts/apply_medium_analysis_v2_schema.R"
        )


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


def build_payload(model: str, title: str, subtitle: str | None, prompt_version: str) -> dict[str, Any]:
    input_fields = {
        "title": title,
        "subtitle": subtitle or "",
    }
    return {
        "model": model,
        "input": [
            {
                "role": "system",
                "content": (
                    "You score the reader-facing pre-click appeal of Medium finance titles. "
                    "Use only the supplied title and subtitle. Do not infer or use claps, responses, rank, age, publication performance, or observation history. "
                    "Do not estimate click potential. Return calibrated JSON scores from 1 to 5."
                ),
            },
            {
                "role": "user",
                "content": (
                    f"Prompt version: {prompt_version}\n\n"
                    "Important measurement note:\n"
                    "Do not estimate click potential. For competitor Medium articles, we do not have impressions, views, reads, or click-through data, so click potential is not directly testable in this dataset.\n\n"
                    "Focus instead on outcomes that can be compared against observed public metrics:\n"
                    "- medium_clap_potential: how likely this article is to receive claps after readers open and read it.\n"
                    "- medium_comment_potential: how likely this article is to receive written responses/comments, especially because it invites disagreement, personal stories, debate, strong opinions, corrections, or follow-up questions.\n"
                    "- overall_article_potential: overall expected Medium performance based on title and subtitle only, considering likely reader interest, clarity, topic strength, credibility, and engagement potential.\n\n"
                    "Score all numeric fields from 1 to 5:\n"
                    "1 = very weak\n"
                    "2 = below average\n"
                    "3 = average / okay\n"
                    "4 = strong\n"
                    "5 = excellent\n\n"
                    "Input fields, and no other article data:\n"
                    f"{json.dumps(input_fields, ensure_ascii=False, indent=2)}\n\n"
                    "Rubric:\n"
                    "- clarity: How clear and immediately understandable the title/subtitle are.\n"
                    "- curiosity: How much the title/subtitle create a genuine desire to know more.\n"
                    "- specificity: How concrete, focused, and non-generic the promise is.\n"
                    "- beginner_appeal: How appealing and accessible the topic sounds for beginner or mainstream personal finance readers.\n"
                    "- credibility: How trustworthy, grounded, and non-hypey the title/subtitle feel.\n"
                    "- emotional_pull: How much the title/subtitle create emotional interest, concern, excitement, surprise, or urgency.\n"
                    "- promise_strength: How strong and valuable the implied benefit or insight seems.\n"
                    "- medium_clap_potential: Estimate how likely readers would be to clap for this article after reading it. Reward titles/subtitles that suggest useful, satisfying, credible, or share-worthy content. Do not treat this as click-through potential.\n"
                    "- medium_comment_potential: Estimate how likely the article is to generate Medium responses/comments. Higher scores should go to titles/subtitles that invite disagreement, debate, personal experiences, strong opinions, corrections, or nuanced discussion. A useful but straightforward article can have high clap potential but low comment potential.\n"
                    "- overall_article_potential: Estimate the article’s general Medium performance potential from title and subtitle only. Consider topic demand, clarity, reader relevance, credibility, emotional pull, and likely engagement. This should map most closely to the combined success score based on claps and responses.\n"
                    "- trust_risk: Risk that the title/subtitle feel exaggerated, misleading, too clickbaity, or credibility-damaging. Higher means more risk.\n\n"
                    "Return JSON matching the schema exactly. short_reason must be one short sentence."
                ),
            },
        ],
        "text": {
            "format": {
                "type": "json_schema",
                "name": "medium_title_scores_v2_1",
                "strict": True,
                "schema": score_schema(),
            }
        },
    }


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
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json",
    }
    for attempt in range(max_retries + 1):
        request = urllib.request.Request(API_URL, data=encoded, headers=headers, method="POST")
        try:
            with urllib.request.urlopen(request, timeout=90) as response:
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


def already_scored(
    connection: sqlite3.Connection,
    row: sqlite3.Row,
    prompt_version: str,
    model: str,
    title_digest: str,
    subtitle_digest: str,
) -> bool:
    count = connection.execute(
        """
        SELECT COUNT(*) AS n
        FROM medium_title_api_scores
        WHERE canonical_article_key = ?
          AND title_hash = ?
          AND subtitle_hash = ?
          AND prompt_version = ?
          AND model = ?
        """,
        (row["canonical_article_key"], title_digest, subtitle_digest, prompt_version, model),
    ).fetchone()["n"]
    return count > 0


def load_candidates(connection: sqlite3.Connection, args: argparse.Namespace) -> list[dict[str, Any]]:
    rows = connection.execute(
        """
        SELECT
          canonical_article_key,
          article_id,
          medium_post_id,
          title,
          subtitle
        FROM v_medium_title_prediction_dataset_v2
        WHERE NULLIF(TRIM(title), '') IS NOT NULL
        ORDER BY canonical_article_key
        """
    ).fetchall()
    candidates: list[dict[str, Any]] = []
    for row in rows:
        title = clean_text(row["title"])
        if not title:
            continue
        subtitle = clean_text(row["subtitle"])
        title_digest = text_hash(title)
        subtitle_digest = text_hash(subtitle)
        if not args.force and already_scored(connection, row, args.prompt_version, args.model, title_digest, subtitle_digest):
            continue
        candidates.append(
            {
                "row": row,
                "title": title,
                "subtitle": subtitle,
                "title_hash": title_digest,
                "subtitle_hash": subtitle_digest,
            }
        )
        if args.limit is not None and len(candidates) >= max(args.limit, 0):
            break
    return candidates


def insert_score(
    connection: sqlite3.Connection,
    item: dict[str, Any],
    args: argparse.Namespace,
    raw_json: dict[str, Any],
    parsed: dict[str, Any],
    error: str | None = None,
) -> None:
    row = item["row"]
    connection.execute(
        """
        INSERT OR REPLACE INTO medium_title_api_scores (
          canonical_article_key,
          article_id,
          medium_post_id,
          prompt_version,
          model,
          scored_at,
          title_hash,
          subtitle_hash,
          input_title,
          input_subtitle,
          raw_json,
          clarity,
          curiosity,
          specificity,
          beginner_appeal,
          credibility,
          emotional_pull,
          promise_strength,
          click_potential,
          medium_clap_potential,
          medium_comment_potential,
          overall_article_potential,
          trust_risk,
          predicted_success_bucket,
          short_reason,
          error
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """,
        (
            row["canonical_article_key"],
            row["article_id"],
            row["medium_post_id"],
            args.prompt_version,
            args.model,
            utc_now(),
            item["title_hash"],
            item["subtitle_hash"],
            item["title"],
            item["subtitle"],
            json.dumps(raw_json, ensure_ascii=False, sort_keys=True),
            parsed.get("clarity"),
            parsed.get("curiosity"),
            parsed.get("specificity"),
            parsed.get("beginner_appeal"),
            parsed.get("credibility"),
            parsed.get("emotional_pull"),
            parsed.get("promise_strength"),
            None,
            parsed.get("medium_clap_potential"),
            parsed.get("medium_comment_potential"),
            parsed.get("overall_article_potential"),
            parsed.get("trust_risk"),
            parsed.get("predicted_success_bucket"),
            parsed.get("short_reason"),
            error,
        ),
    )
    connection.commit()


def main() -> int:
    args = parse_args()
    db_path = Path(args.db)
    connection = connect_db(db_path)
    try:
        ensure_objects(connection)
        candidates = load_candidates(connection, args)

        print("Medium Title API Scoring V2")
        print("===========================")
        print(f"DB path: {db_path}")
        print(f"Model: {args.model}")
        print(f"Prompt version: {args.prompt_version}")
        print("API input fields: title, subtitle")
        print("Leakage guard: no claps, responses, success_score, rank, page position, publication performance, dates, observations, times seen, thumbnail data, or other outcome fields are sent.")
        print(f"Candidate rows after cache/limit: {len(candidates)}")

        if not candidates:
            return 0

        first_payload = build_payload(args.model, candidates[0]["title"], candidates[0]["subtitle"], args.prompt_version)
        if args.dry_run:
            print("\nDry run payload for first candidate:")
            print(json.dumps(first_payload, indent=2, ensure_ascii=False))
            print("\nDry run only. No API call was made and no DB rows were written.")
            return 0

        api_key = os.environ.get("OPENAI_API_KEY")
        if not api_key:
            raise SystemExit("OPENAI_API_KEY is not set. Use --dry-run for a no-cost payload check.")

        for index, item in enumerate(candidates, start=1):
            payload = build_payload(args.model, item["title"], item["subtitle"], args.prompt_version)
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
