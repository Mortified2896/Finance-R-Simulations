#!/usr/bin/env python3
"""
Score Medium titles or title/subtitle pairs against a small structured rubric.

This V2 workflow reads from v_medium_title_prediction_dataset_v2 and writes to
medium_title_api_scores. The API request intentionally contains only the
selected text scope, never outcome or distribution fields.
"""

from __future__ import annotations

import argparse
import csv
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
DEFAULT_PROMPT_VERSION = "v2_2"
DEFAULT_SCOPE = "title_subtitle"
VALID_SCOPES = {"title_only", "title_subtitle"}
DEFAULT_SAMPLE_MODE = "default"
VALID_SAMPLE_MODES = {"default", "thumbnail_first", "thumbnail_only", "random"}
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
    parser.add_argument(
        "--scope",
        default=DEFAULT_SCOPE,
        choices=sorted(VALID_SCOPES),
        help="Text scope to send to the API and cache separately.",
    )
    parser.add_argument(
        "--sample-mode",
        default=DEFAULT_SAMPLE_MODE,
        choices=sorted(VALID_SAMPLE_MODES),
        help="Candidate sampling mode. Default preserves canonical-key ordering.",
    )
    parser.add_argument(
        "--sample-file",
        help="CSV cohort to score. Matches by canonical_article_key, with article_id and medium_post_id fallbacks.",
    )
    parser.add_argument(
        "--save-sample-file",
        help="Write the selected candidate cohort to CSV before scoring. Outcome fields are never included.",
    )
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


def table_columns(connection: sqlite3.Connection, table_name: str) -> set[str]:
    return {row["name"] for row in connection.execute(f"PRAGMA table_info({table_name})")}


def ensure_score_scope_column(connection: sqlite3.Connection) -> None:
    columns = table_columns(connection, "medium_title_api_scores")
    if "score_scope" not in columns:
        connection.execute(
            "ALTER TABLE medium_title_api_scores ADD COLUMN score_scope TEXT NOT NULL DEFAULT 'title_subtitle'"
        )
        connection.commit()
    connection.execute("DROP INDEX IF EXISTS idx_medium_title_api_scores_cache")
    connection.execute(
        """
        CREATE UNIQUE INDEX IF NOT EXISTS idx_medium_title_api_scores_cache
          ON medium_title_api_scores (
            canonical_article_key,
            title_hash,
            subtitle_hash,
            prompt_version,
            model,
            score_scope
          )
        """
    )
    connection.commit()


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


def scope_instruction(scope: str) -> str:
    if scope == "title_only":
        return "You are scoring only the title. Do not infer a subtitle. Treat missing context as missing."
    return "You are scoring the title and subtitle as a reader-facing pair."


def scope_label(scope: str) -> str:
    return "title" if scope == "title_only" else "title/subtitle"


def prompt_content(prompt_version: str, input_fields: dict[str, str], scope: str) -> str:
    text_label = scope_label(scope)
    scope_note = scope_instruction(scope)
    if prompt_version == "v2_2":
        return (
            f"Prompt version: {prompt_version}\n\n"
            f"Score scope: {scope}\n"
            f"{scope_note}\n\n"
            "Important measurement note:\n"
            "Do not estimate click potential. For competitor Medium articles, we do not have impressions, views, reads, or click-through data, so click potential is not directly testable in this dataset.\n\n"
            "Focus instead on outcomes that can be compared against observed public metrics:\n"
            "- medium_clap_potential: how likely this article is to receive claps after readers open and read it.\n"
            "- medium_comment_potential: how likely this article is to receive written responses/comments, especially because it invites disagreement, personal stories, debate, strong opinions, corrections, or follow-up questions.\n"
            f"- overall_article_potential: overall expected Medium performance based on {text_label} only, considering likely reader interest, clarity, topic strength, credibility, and engagement potential.\n\n"
            "Calibrate scores relative to typical Medium personal finance articles, not in isolation.\n\n"
            "Use the full 1-5 scale aggressively:\n"
            "1 = very weak, likely below average\n"
            "2 = below average or generic\n"
            "3 = average / okay for Medium finance\n"
            "4 = clearly above average, likely stronger than most articles\n"
            "5 = exceptional, rare, top-tier potential\n\n"
            "Most normal articles should receive 2 or 3.\n"
            f"Do not give 4 unless the {text_label} has a clearly strong hook, strong topic demand, clear reader payoff, and enough specificity.\n"
            f"Do not give 5 unless the {text_label} looks unusually compelling and would plausibly belong among the strongest articles in the dataset.\n"
            "Avoid defaulting to 4 for merely competent, useful, or credible articles.\n\n"
            "Input fields, and no other article data:\n"
            f"{json.dumps(input_fields, ensure_ascii=False, indent=2)}\n\n"
            "Rubric:\n"
            f"- clarity: How clear and immediately understandable the {text_label} is.\n"
            f"- curiosity: How much the {text_label} creates a genuine desire to know more.\n"
            "- specificity: How concrete, focused, and non-generic the promise is.\n"
            "- beginner_appeal: How appealing and accessible the topic sounds for beginner or mainstream personal finance readers.\n"
            f"- credibility: How trustworthy, grounded, and non-hypey the {text_label} feels.\n"
            f"- emotional_pull: How much the {text_label} creates emotional interest, concern, excitement, surprise, or urgency.\n"
            "- promise_strength: How strong and valuable the implied benefit or insight seems.\n"
            f"- medium_clap_potential: Estimate how likely readers would be to clap after reading. Do not reward generic usefulness alone. A high score requires {text_label} wording that suggests unusually satisfying, insightful, emotionally resonant, practical, or share-worthy content. Use 5 only for rare titles that strongly promise a memorable payoff.\n"
            f"- medium_comment_potential: Estimate how likely the article is to generate written Medium responses/comments. Higher scores should go to {text_label} wording that invites disagreement, debate, personal experiences, corrections, strong opinions, or nuanced discussion. A useful but straightforward article can have high clap potential but low comment potential. Use the full scale.\n"
            f"- overall_article_potential: Estimate overall Medium performance potential from {text_label} only. This should be a relative ranking judgment, not a quality compliment. Consider topic demand, clarity, emotional stakes, specificity, credibility, likely engagement, and whether the {text_label} feels meaningfully differentiated from generic finance content. Use 5 sparingly for likely top-decile potential.\n"
            f"- trust_risk: Risk that the {text_label} feels exaggerated, misleading, too clickbaity, or credibility-damaging. Higher means more risk. This is not the same as low credibility: wording can be credible but still boring, or emotionally sharp but somewhat risky.\n\n"
            "predicted_success_bucket:\n"
            "- low = likely below median or weak relative to typical Medium finance articles.\n"
            "- medium = around median to moderately above average.\n"
            "- high = likely top 20 percent potential. Use high sparingly. Do not classify most articles as high.\n\n"
            "Return JSON matching the schema exactly. short_reason must be one short sentence."
        )

    return (
        f"Prompt version: {prompt_version}\n\n"
        f"Score scope: {scope}\n"
        f"{scope_note}\n\n"
        "Important measurement note:\n"
        "Do not estimate click potential. For competitor Medium articles, we do not have impressions, views, reads, or click-through data, so click potential is not directly testable in this dataset.\n\n"
        "Focus instead on outcomes that can be compared against observed public metrics:\n"
        "- medium_clap_potential: how likely this article is to receive claps after readers open and read it.\n"
        "- medium_comment_potential: how likely this article is to receive written responses/comments, especially because it invites disagreement, personal stories, debate, strong opinions, corrections, or follow-up questions.\n"
        f"- overall_article_potential: overall expected Medium performance based on {text_label} only, considering likely reader interest, clarity, topic strength, credibility, and engagement potential.\n\n"
        "Score all numeric fields from 1 to 5:\n"
        "1 = very weak\n"
        "2 = below average\n"
        "3 = average / okay\n"
        "4 = strong\n"
        "5 = excellent\n\n"
        "Input fields, and no other article data:\n"
        f"{json.dumps(input_fields, ensure_ascii=False, indent=2)}\n\n"
        "Rubric:\n"
        f"- clarity: How clear and immediately understandable the {text_label} is.\n"
        f"- curiosity: How much the {text_label} creates a genuine desire to know more.\n"
        "- specificity: How concrete, focused, and non-generic the promise is.\n"
        "- beginner_appeal: How appealing and accessible the topic sounds for beginner or mainstream personal finance readers.\n"
        f"- credibility: How trustworthy, grounded, and non-hypey the {text_label} feels.\n"
        f"- emotional_pull: How much the {text_label} creates emotional interest, concern, excitement, surprise, or urgency.\n"
        "- promise_strength: How strong and valuable the implied benefit or insight seems.\n"
        f"- medium_clap_potential: Estimate how likely readers would be to clap for this article after reading it. Reward {text_label} wording that suggests useful, satisfying, credible, or share-worthy content. Do not treat this as click-through potential.\n"
        f"- medium_comment_potential: Estimate how likely the article is to generate Medium responses/comments. Higher scores should go to {text_label} wording that invites disagreement, debate, personal experiences, strong opinions, corrections, or nuanced discussion. A useful but straightforward article can have high clap potential but low comment potential.\n"
        f"- overall_article_potential: Estimate the article's general Medium performance potential from {text_label} only. Consider topic demand, clarity, reader relevance, credibility, emotional pull, and likely engagement. This should map most closely to the combined success score based on claps and responses.\n"
        f"- trust_risk: Risk that the {text_label} feels exaggerated, misleading, too clickbaity, or credibility-damaging. Higher means more risk.\n\n"
        "Return JSON matching the schema exactly. short_reason must be one short sentence."
    )


def build_payload(model: str, title: str, subtitle: str | None, prompt_version: str, scope: str) -> dict[str, Any]:
    input_fields = {"title": title}
    if scope == "title_subtitle":
        input_fields["subtitle"] = subtitle or ""
    schema_name = f"medium_title_scores_{prompt_version}_{scope}".replace("-", "_")
    system_scope = (
        "Use only the supplied title. Do not infer a subtitle."
        if scope == "title_only"
        else "Use only the supplied title and subtitle."
    )
    return {
        "model": model,
        "input": [
            {
                "role": "system",
                "content": (
                    "You score the reader-facing pre-click appeal of Medium finance titles. "
                    f"{system_scope} Do not infer or use claps, responses, rank, age, publication performance, or observation history. "
                    "Do not estimate click potential. Return calibrated JSON scores from 1 to 5."
                ),
            },
            {
                "role": "user",
                "content": prompt_content(prompt_version, input_fields, scope),
            },
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
    score_scope: str,
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
          AND score_scope = ?
        """,
        (row["canonical_article_key"], title_digest, subtitle_digest, prompt_version, model, score_scope),
    ).fetchone()["n"]
    return count > 0


def usable_thumbnail_expression(connection: sqlite3.Connection) -> tuple[str, str]:
    dataset_columns = table_columns(connection, "v_medium_title_prediction_dataset_v2")
    if "has_thumbnail_url" in dataset_columns:
        return (
            "(COALESCE(d.has_thumbnail_url, 0) = 1 OR NULLIF(TRIM(d.thumbnail_url), '') IS NOT NULL)",
            "v_medium_title_prediction_dataset_v2 has_thumbnail_url/thumbnail_url",
        )
    return (
        "(NULLIF(TRIM(d.thumbnail_url), '') IS NOT NULL)",
        "v_medium_title_prediction_dataset_v2 thumbnail_url",
    )


def candidate_from_row(row: sqlite3.Row) -> dict[str, Any] | None:
    title = clean_text(row["title"])
    if not title:
        return None
    subtitle = clean_text(row["subtitle"])
    return {
        "row": row,
        "title": title,
        "subtitle": subtitle,
        "title_hash": text_hash(title),
        "subtitle_hash": text_hash(subtitle),
        "has_usable_thumbnail": bool(row["has_usable_thumbnail"]),
    }


def fetch_unscored_rows(
    connection: sqlite3.Connection,
    args: argparse.Namespace,
    thumbnail_expression: str,
    where_extra: str = "",
    order_by: str = "d.canonical_article_key",
    limit: int | None = None,
) -> list[sqlite3.Row]:
    limit_sql = "" if limit is None else "LIMIT ?"
    # SQLite builds on macOS do not consistently expose SHA functions, so cache
    # filtering stays in Python where title/subtitle normalization is defined.
    sql = f"""
        SELECT
          d.canonical_article_key,
          d.article_id,
          d.medium_post_id,
          d.title,
          d.subtitle,
          d.thumbnail_url,
          d.has_thumbnail_url,
          CASE WHEN {thumbnail_expression} THEN 1 ELSE 0 END AS has_usable_thumbnail
        FROM v_medium_title_prediction_dataset_v2 d
        WHERE NULLIF(TRIM(d.title), '') IS NOT NULL
          {where_extra}
        ORDER BY {order_by}
        {limit_sql}
    """
    params = (max(limit, 0),) if limit is not None else ()
    return connection.execute(sql, params).fetchall()


def fetch_dataset_rows(connection: sqlite3.Connection, thumbnail_expression: str) -> list[sqlite3.Row]:
    sql = f"""
        SELECT
          d.canonical_article_key,
          d.article_id,
          d.medium_post_id,
          d.title,
          d.subtitle,
          d.thumbnail_url,
          d.has_thumbnail_url,
          CASE WHEN {thumbnail_expression} THEN 1 ELSE 0 END AS has_usable_thumbnail
        FROM v_medium_title_prediction_dataset_v2 d
        WHERE NULLIF(TRIM(d.title), '') IS NOT NULL
    """
    return connection.execute(sql).fetchall()


def load_sample_rows(connection: sqlite3.Connection, sample_path: Path, thumbnail_expression: str) -> list[sqlite3.Row]:
    if not sample_path.exists():
        raise SystemExit(f"Sample file not found: {sample_path}")

    with sample_path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        sample_records = list(reader)

    if not sample_records:
        return []

    dataset_rows = fetch_dataset_rows(connection, thumbnail_expression)
    by_canonical = {str(row["canonical_article_key"]): row for row in dataset_rows if row["canonical_article_key"] is not None}
    by_article_id = {str(row["article_id"]): row for row in dataset_rows if row["article_id"] is not None}
    by_post_id = {str(row["medium_post_id"]): row for row in dataset_rows if clean_text(row["medium_post_id"])}

    matched_rows: list[sqlite3.Row] = []
    seen_keys: set[str] = set()
    for record in sample_records:
        row = None
        canonical_key = clean_text(record.get("canonical_article_key"))
        article_id = clean_text(record.get("article_id"))
        medium_post_id = clean_text(record.get("medium_post_id"))
        if canonical_key and canonical_key in by_canonical:
            row = by_canonical[canonical_key]
        elif article_id and article_id in by_article_id:
            row = by_article_id[article_id]
        elif medium_post_id and medium_post_id in by_post_id:
            row = by_post_id[medium_post_id]

        if row is None:
            continue
        row_key = str(row["canonical_article_key"])
        if row_key in seen_keys:
            continue
        seen_keys.add(row_key)
        matched_rows.append(row)

    return matched_rows


def filter_unscored(connection: sqlite3.Connection, rows: list[sqlite3.Row], args: argparse.Namespace, needed: int | None) -> list[dict[str, Any]]:
    candidates: list[dict[str, Any]] = []
    seen_keys: set[str] = set()
    for row in rows:
        if needed is not None and len(candidates) >= max(needed, 0):
            break
        if row["canonical_article_key"] in seen_keys:
            continue
        item = candidate_from_row(row)
        if item is None:
            continue
        seen_keys.add(row["canonical_article_key"])
        if not args.force and already_scored(
            connection,
            row,
            args.prompt_version,
            args.model,
            item["title_hash"],
            item["subtitle_hash"],
            args.scope,
        ):
            continue
        candidates.append(item)
    return candidates


def load_candidates(connection: sqlite3.Connection, args: argparse.Namespace) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    limit = None if args.limit is None else max(args.limit, 0)
    thumbnail_expression, thumbnail_criterion = usable_thumbnail_expression(connection)

    metadata: dict[str, Any] = {
        "thumbnail_criterion": thumbnail_criterion,
        "thumbnail_selected": 0,
        "filler_selected": 0,
        "sample_file_read": args.sample_file or "",
        "sample_file_save": args.save_sample_file or "",
    }

    if args.sample_file:
        rows = load_sample_rows(connection, Path(args.sample_file), thumbnail_expression)
        candidates = filter_unscored(connection, rows, args, limit)
    elif args.sample_mode == "default":
        rows = fetch_unscored_rows(connection, args, thumbnail_expression, order_by="d.canonical_article_key")
        candidates = filter_unscored(connection, rows, args, limit)
    elif args.sample_mode == "random":
        rows = fetch_unscored_rows(connection, args, thumbnail_expression, order_by="RANDOM()")
        candidates = filter_unscored(connection, rows, args, limit)
    elif args.sample_mode == "thumbnail_only":
        rows = fetch_unscored_rows(
            connection,
            args,
            thumbnail_expression,
            where_extra=f"AND ({thumbnail_expression})",
            order_by="RANDOM()",
        )
        candidates = filter_unscored(connection, rows, args, limit)
    elif args.sample_mode == "thumbnail_first":
        thumbnail_rows = fetch_unscored_rows(
            connection,
            args,
            thumbnail_expression,
            where_extra=f"AND ({thumbnail_expression})",
            order_by="RANDOM()",
        )
        thumbnail_candidates = filter_unscored(connection, thumbnail_rows, args, limit)
        remaining = None if limit is None else max(limit - len(thumbnail_candidates), 0)
        filler_candidates: list[dict[str, Any]] = []
        if remaining is None or remaining > 0:
            filler_rows = fetch_unscored_rows(
                connection,
                args,
                thumbnail_expression,
                where_extra=f"AND NOT ({thumbnail_expression})",
                order_by="RANDOM()",
            )
            filler_candidates = filter_unscored(connection, filler_rows, args, remaining)
        candidates = thumbnail_candidates + filler_candidates
    else:
        raise ValueError(f"Unknown sample mode: {args.sample_mode}")

    metadata["thumbnail_selected"] = sum(1 for item in candidates if item["has_usable_thumbnail"])
    metadata["filler_selected"] = len(candidates) - metadata["thumbnail_selected"]
    return candidates, metadata


def save_sample_file(path: Path, candidates: list[dict[str, Any]], sample_mode: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    selected_at = utc_now()
    columns = [
        "canonical_article_key",
        "article_id",
        "medium_post_id",
        "title",
        "subtitle",
        "thumbnail_url",
        "has_thumbnail_url",
        "sample_mode",
        "selected_at",
    ]
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=columns)
        writer.writeheader()
        for item in candidates:
            row = item["row"]
            writer.writerow(
                {
                    "canonical_article_key": row["canonical_article_key"],
                    "article_id": row["article_id"],
                    "medium_post_id": row["medium_post_id"],
                    "title": item["title"],
                    "subtitle": item["subtitle"] or "",
                    "thumbnail_url": row["thumbnail_url"],
                    "has_thumbnail_url": row["has_thumbnail_url"],
                    "sample_mode": sample_mode,
                    "selected_at": selected_at,
                }
            )


def insert_score(
    connection: sqlite3.Connection,
    item: dict[str, Any],
    args: argparse.Namespace,
    raw_json: dict[str, Any],
    parsed: dict[str, Any],
    error: str | None = None,
) -> None:
    row = item["row"]
    input_subtitle = item["subtitle"] if args.scope == "title_subtitle" else None
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
        "input_title",
        "input_subtitle",
        "raw_json",
        "clarity",
        "curiosity",
        "specificity",
        "beginner_appeal",
        "credibility",
        "emotional_pull",
        "promise_strength",
        "click_potential",
        "medium_clap_potential",
        "medium_comment_potential",
        "overall_article_potential",
        "trust_risk",
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
        item["title_hash"],
        item["subtitle_hash"],
        item["title"],
        input_subtitle,
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
    )
    if len(columns) != len(values):
        raise RuntimeError(
            "medium_title_api_scores insert mismatch: "
            f"{len(columns)} columns for {len(values)} values"
        )

    column_sql = ",\n          ".join(columns)
    placeholder_sql = ", ".join("?" for _ in columns)
    connection.execute(
        f"""
        INSERT OR REPLACE INTO medium_title_api_scores (
          {column_sql}
        ) VALUES ({placeholder_sql})
        """,
        values,
    )
    connection.commit()


def main() -> int:
    args = parse_args()
    db_path = Path(args.db)
    connection = connect_db(db_path)
    try:
        ensure_objects(connection)
        ensure_score_scope_column(connection)
        candidates, sample_metadata = load_candidates(connection, args)
        if args.save_sample_file:
            save_sample_file(Path(args.save_sample_file), candidates, args.sample_mode)

        print("Medium Title API Scoring V2")
        print("===========================")
        print(f"DB path: {db_path}")
        print(f"Model: {args.model}")
        print(f"Prompt version: {args.prompt_version}")
        print(f"Score scope: {args.scope}")
        print(f"Sample mode: {args.sample_mode}")
        print(f"Requested limit: {args.limit}")
        if args.sample_file:
            print(f"Sample file read: {args.sample_file}")
        if args.save_sample_file:
            print(f"Sample file saved: {args.save_sample_file}")
        print(f"Thumbnail criterion: {sample_metadata['thumbnail_criterion']}")
        print(f"Thumbnail rows selected: {sample_metadata['thumbnail_selected']}")
        print(f"Filler/random rows selected: {sample_metadata['filler_selected']}")
        print("API input fields: title" if args.scope == "title_only" else "API input fields: title, subtitle")
        print("Leakage guard: no claps, responses, success_score, rank, page position, publication performance, dates, observations, times seen, thumbnail data, or other outcome fields are sent.")
        print(f"Candidate rows after cache/limit: {len(candidates)}")

        if not candidates:
            return 0

        first_payload = build_payload(args.model, candidates[0]["title"], candidates[0]["subtitle"], args.prompt_version, args.scope)
        if args.dry_run:
            print("\nDry run payload for first candidate:")
            print(json.dumps(first_payload, indent=2, ensure_ascii=False))
            print("\nDry run only. No API call was made and no DB rows were written.")
            return 0

        api_key = os.environ.get("OPENAI_API_KEY")
        if not api_key:
            raise SystemExit("OPENAI_API_KEY is not set. Use --dry-run for a no-cost payload check.")

        for index, item in enumerate(candidates, start=1):
            payload = build_payload(args.model, item["title"], item["subtitle"], args.prompt_version, args.scope)
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
