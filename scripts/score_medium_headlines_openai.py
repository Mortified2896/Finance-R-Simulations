#!/usr/bin/env python3
"""
Pilot OpenAI scoring for Medium title/subtitle pre-click appeal.

This is intentionally an experiment script, not a production pipeline:
- defaults to a small sample instead of all rows
- reads OPENAI_API_KEY from the environment
- sends only title/subtitle/rubric text to the model
- writes isolated CSV and JSONL outputs under data/analysis/medium_analysis_v1/openai_headline_scoring
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import random
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from langfuse_python import build_openai_client, flush_langfuse, langfuse_enabled, start_langfuse_run


INPUT_PATH = Path("data/analysis/medium_analysis_v1/medium_title_prediction_dataset.csv")
OUTPUT_DIR = Path("data/analysis/medium_analysis_v1/openai_headline_scoring")
CSV_OUTPUT = OUTPUT_DIR / "openai_headline_scores_v2.csv"
JSONL_OUTPUT = OUTPUT_DIR / "openai_headline_scores_raw_v2.jsonl"
RUBRIC_VERSION = "headline_v2"
DEFAULT_MODEL = "gpt-5-mini"
DEFAULT_SAMPLE_SIZE = 200
DEFAULT_SEED = 20260513
DIMENSIONS = [
    ("clarity", "easy to understand on first pass"),
    ("concreteness", "gives enough specific information before the click"),
    ("curiosity_gap", "creates a specific open loop the reader wants closed"),
    ("practical_utility", "promises a useful takeaway, warning, decision aid, or lesson"),
    ("emotional_arousal", "creates activating emotion such as urgency, alarm, relief, surprise, or tension"),
    ("negative_stakes", "highlights risk, loss, regret, mistake, or avoidable downside"),
    ("surprisingness", "feels counterintuitive or novel without obvious hype"),
    ("credibility", "feels believable, proportionate, and trustworthy"),
    (
        "life_outcome_relevance",
        "links finance to real life outcomes such as retirement, security, freedom, family, future self, or costly mistakes",
    ),
    ("technical_narrowness", "feels niche, jargon-heavy, ETF/portfolio/product-first, or expert-first"),
    ("clickbait_risk", "feels manipulative, exaggerated, over-withheld, or trust-eroding"),
    (
        "overall_preclick_appeal",
        "holistic Medium finance pre-click appeal, balancing hook, usefulness, trust, and audience fit. Penalize trust-eroding clickbait even if it creates curiosity.",
    ),
]

SCORE_COLUMNS: list[str] = []
for dimension, _definition in DIMENSIONS:
    SCORE_COLUMNS.extend(
        [
            f"{dimension}_score",
            f"{dimension}_confidence",
            f"{dimension}_evidence",
        ]
    )

CSV_COLUMNS = [
    "article_id",
    "medium_post_id",
    "score_scope",
    "title",
    "subtitle",
    *SCORE_COLUMNS,
    "requires_human_review",
    "summary_note",
    "rubric_version",
    "model",
    "timestamp_utc",
]


def clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value).replace("\u00a0", " ")
    text = " ".join(text.split()).strip()
    if text == "":
        return None
    return text


def parse_bool(value: Any) -> bool | None:
    if value is None:
        return None
    text = str(value).strip().lower()
    if text in {"true", "t", "1", "yes"}:
        return True
    if text in {"false", "f", "0", "no"}:
        return False
    return None


def read_articles(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        raise SystemExit(
            f"Could not find {path}. Run scripts/build_medium_title_prediction_dataset.R first."
        )
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


def get_article_id(row: dict[str, str]) -> str:
    return clean_text(row.get("article_id")) or clean_text(row.get("medium_post_id")) or ""


def get_subtitle(row: dict[str, str]) -> str | None:
    for column in ("subtitle_text_for_analysis", "subtitle", "deck", "description", "snippet"):
        if column in row:
            value = clean_text(row.get(column))
            if value:
                return value
    return None


def article_theme(row: dict[str, str]) -> str:
    text = " ".join(
        value for value in [clean_text(row.get("title")), get_subtitle(row)] if value
    ).lower()
    if any(term in text for term in ["retire", "retirement", "financial independence", "future self"]):
        return "retirement_life_outcome"
    if any(term in text for term in ["etf", "index fund", "indexing", "portfolio"]):
        return "etf_index_portfolio"
    if any(term in text for term in ["mistake", "mistakes", "wrong", "avoid", "problem", "risk", "loss", "regret"]):
        return "mistake_problem_negative_stakes"
    return "general"


def select_sample(rows: list[dict[str, str]], sample_size: int, seed: int) -> list[dict[str, str]]:
    usable = [row for row in rows if clean_text(row.get("title")) and get_article_id(row)]
    if sample_size <= 0 or sample_size >= len(usable):
        return usable

    rng = random.Random(seed)
    groups: dict[tuple[str, str], list[dict[str, str]]] = {}
    for row in usable:
        high_value = parse_bool(row.get("high_performer_top20"))
        high_label = "high_performer" if high_value else "not_high_performer"
        groups.setdefault((high_label, article_theme(row)), []).append(row)

    selected: list[dict[str, str]] = []
    group_keys = sorted(groups)
    base_take = max(1, sample_size // max(1, len(group_keys)))
    for key in group_keys:
        group_rows = groups[key][:]
        rng.shuffle(group_rows)
        selected.extend(group_rows[:base_take])

    if len(selected) < sample_size:
        selected_ids = {get_article_id(row) for row in selected}
        leftovers = [row for row in usable if get_article_id(row) not in selected_ids]
        rng.shuffle(leftovers)
        selected.extend(leftovers[: sample_size - len(selected)])

    rng.shuffle(selected)
    return selected[:sample_size]


def load_cache(path: Path, model: str) -> set[tuple[str, str]]:
    if not path.exists():
        return set()
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        return {
            (row.get("article_id", ""), row.get("score_scope", ""))
            for row in reader
            if row.get("rubric_version") == RUBRIC_VERSION and row.get("model") == model
        }


def build_schema() -> dict[str, Any]:
    score_item = {
        "type": "object",
        "properties": {
            "score": {"type": "integer", "minimum": 1, "maximum": 5},
            "confidence": {"type": "number", "minimum": 0, "maximum": 1},
            "evidence": {"type": "string"},
        },
        "required": ["score", "confidence", "evidence"],
        "additionalProperties": False,
    }
    return {
        "type": "object",
        "properties": {
            "scores": {
                "type": "object",
                "properties": {dimension: score_item for dimension, _definition in DIMENSIONS},
                "required": [dimension for dimension, _definition in DIMENSIONS],
                "additionalProperties": False,
            },
            "diagnostics": {
                "type": "object",
                "properties": {
                    "requires_human_review": {"type": "boolean"},
                    "summary_note": {"type": "string"},
                },
                "required": ["requires_human_review", "summary_note"],
                "additionalProperties": False,
            },
        },
        "required": ["scores", "diagnostics"],
        "additionalProperties": False,
    }


def rubric_text() -> str:
    lines = [
        f"Rubric version: {RUBRIC_VERSION}",
        "",
        "Score the reader-facing pre-click impression of a Medium finance headline.",
        "Use integer scores from 1 to 5. Use confidence from 0 to 1.",
        "",
        "Score scale:",
        "1 = very weak / absent",
        "2 = weak",
        "3 = moderate",
        "4 = strong",
        "5 = very strong",
        "",
        "Confidence means how confidently the title/subtitle text supports your score, not how likely the article is to perform.",
        "Evidence must be a short phrase, not a long explanation.",
        "More is not always better for curiosity_gap, emotional_arousal, negative_stakes, concreteness, or technical_narrowness.",
        "technical_narrowness and clickbait_risk may be negative or nonlinear features. Do not treat higher as automatically better.",
        "",
        "Dimensions:",
    ]
    for dimension, definition in DIMENSIONS:
        lines.append(f"- {dimension}: {definition}")
    return "\n".join(lines)


def build_user_prompt(title: str, subtitle: str | None, score_scope: str) -> str:
    if score_scope == "title_only":
        headline = f"Title:\n{title}"
    else:
        headline = f"Title:\n{title}\n\nSubtitle:\n{subtitle or '(none available)'}"
    return (
        f"{rubric_text()}\n\n"
        f"Score scope: {score_scope}\n\n"
        f"{headline}\n\n"
        "Return JSON matching the schema exactly."
    )


def model_supports_temperature(model: str) -> bool:
    normalized = model.lower()
    return not normalized.startswith("gpt-5")


def build_payload(model: str, title: str, subtitle: str | None, score_scope: str, temperature: float) -> dict[str, Any]:
    user_prompt = build_user_prompt(title, subtitle, score_scope)
    if score_scope == "title_only" and "Subtitle:" in user_prompt:
        raise AssertionError("title_only prompt must not include a Subtitle section")

    payload: dict[str, Any] = {
        "model": model,
        "input": [
            {
                "role": "system",
                "content": (
                    "You are scoring the reader-facing pre-click appeal of Medium finance titles and subtitles for feature engineering. "
                    "Score only the title/subtitle impression, not full article quality. Do not use or infer performance data. "
                    "Do not reward trust-eroding clickbait. Produce calibrated, schema-valid rubric scores."
                ),
            },
            {"role": "user", "content": user_prompt},
        ],
        "text": {
            "format": {
                "type": "json_schema",
                "name": "headline_preclick_scores",
                "strict": True,
                "schema": build_schema(),
            }
        },
    }
    if model_supports_temperature(model):
        payload["temperature"] = temperature
    return payload


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


def call_openai(client: Any, payload: dict[str, Any], *, name: str, metadata: dict[str, Any] | None = None) -> dict[str, Any]:
    request_payload = dict(payload)
    if langfuse_enabled():
        request_payload["name"] = name
        if metadata:
            request_payload["metadata"] = metadata
    try:
        response = client.responses.create(**request_payload, timeout=90)
    except Exception as error:  # noqa: BLE001
        raise RuntimeError(f"OpenAI API request failed: {error}") from error
    return response.model_dump(mode="json")


def validate_scores(parsed: dict[str, Any]) -> None:
    if not isinstance(parsed, dict):
        raise ValueError("Model output is not a JSON object")
    scores = parsed.get("scores")
    if not isinstance(scores, dict):
        raise ValueError("Model output is missing scores object")

    for dimension, _definition in DIMENSIONS:
        item = scores.get(dimension)
        if not isinstance(item, dict):
            raise ValueError(f"Model output is missing score object for {dimension}")

        score = item.get("score")
        confidence = item.get("confidence")
        if not isinstance(score, int) or isinstance(score, bool) or score < 1 or score > 5:
            raise ValueError(f"Invalid score for {dimension}: {score!r}")
        if not isinstance(confidence, (int, float)) or isinstance(confidence, bool) or confidence < 0 or confidence > 1:
            raise ValueError(f"Invalid confidence for {dimension}: {confidence!r}")


def flatten_result(
    row: dict[str, str],
    score_scope: str,
    title: str,
    subtitle: str | None,
    model: str,
    timestamp_utc: str,
    parsed: dict[str, Any],
) -> dict[str, Any]:
    out: dict[str, Any] = {
        "article_id": get_article_id(row),
        "medium_post_id": clean_text(row.get("medium_post_id")) or "",
        "score_scope": score_scope,
        "title": title,
        "subtitle": subtitle or "",
        "requires_human_review": parsed.get("diagnostics", {}).get("requires_human_review"),
        "summary_note": parsed.get("diagnostics", {}).get("summary_note", ""),
        "rubric_version": RUBRIC_VERSION,
        "model": model,
        "timestamp_utc": timestamp_utc,
    }
    scores = parsed.get("scores", {})
    for dimension, _definition in DIMENSIONS:
        item = scores.get(dimension, {})
        out[f"{dimension}_score"] = item.get("score")
        out[f"{dimension}_confidence"] = item.get("confidence")
        out[f"{dimension}_evidence"] = item.get("evidence", "")
    return out


def append_jsonl(path: Path, record: dict[str, Any]) -> None:
    with path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(record, ensure_ascii=False, sort_keys=True) + "\n")


def append_csv_rows(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        return
    file_exists = path.exists()
    with path.open("a", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=CSV_COLUMNS, extrasaction="ignore")
        if not file_exists:
            writer.writeheader()
        writer.writerows(rows)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Score Medium headline title/subtitle pairs with OpenAI.")
    parser.add_argument("--input", default=str(INPUT_PATH), help="Input CSV path.")
    parser.add_argument("--output-dir", default=str(OUTPUT_DIR), help="Output directory.")
    parser.add_argument("--model", default=os.environ.get("OPENAI_HEADLINE_MODEL", DEFAULT_MODEL), help="OpenAI model name.")
    parser.add_argument("--temperature", type=float, default=0.1, help="Low temperature for consistency.")
    parser.add_argument("--sample", type=int, default=DEFAULT_SAMPLE_SIZE, help="Article sample size before expanding scopes.")
    parser.add_argument("--limit", type=int, default=None, help="Maximum new article/scope API calls this run.")
    parser.add_argument("--seed", type=int, default=DEFAULT_SEED, help="Random seed for reproducible sampling.")
    parser.add_argument("--dry-run", action="store_true", help="Print the first API payload without calling OpenAI.")
    parser.add_argument("--max-retries", type=int, default=3, help="Retries for transient API failures.")
    parser.add_argument(
        "--scopes",
        default="title_subtitle_pair,title_only",
        help="Comma-separated scopes. Defaults to title_subtitle_pair,title_only.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    input_path = Path(args.input)
    output_dir = Path(args.output_dir)
    csv_output = output_dir / CSV_OUTPUT.name
    jsonl_output = output_dir / JSONL_OUTPUT.name
    output_dir.mkdir(parents=True, exist_ok=True)

    scopes = [scope.strip() for scope in args.scopes.split(",") if scope.strip()]
    invalid_scopes = set(scopes) - {"title_subtitle_pair", "title_only"}
    if invalid_scopes:
        raise SystemExit(f"Unsupported score scope(s): {', '.join(sorted(invalid_scopes))}")

    articles = read_articles(input_path)
    sample = select_sample(articles, args.sample, args.seed)
    cache = load_cache(csv_output, args.model)

    work_items: list[tuple[dict[str, str], str]] = []
    for row in sample:
        article_id = get_article_id(row)
        for scope in scopes:
            if (article_id, scope) not in cache:
                work_items.append((row, scope))
    if args.limit is not None:
        work_items = work_items[: max(0, args.limit)]

    if args.dry_run:
        if work_items:
            first_row, first_scope = work_items[0]
        elif sample and scopes:
            first_row, first_scope = sample[0], scopes[0]
        else:
            print("Dry run only. No candidate articles found.")
            return 0

        first_title = clean_text(first_row.get("title")) or ""
        first_subtitle = get_subtitle(first_row)
        first_payload = build_payload(args.model, first_title, first_subtitle, first_scope, args.temperature)
        print(json.dumps(first_payload, indent=2, ensure_ascii=False))
        print(
            f"\nDry run only. Candidate articles: {len(sample)}. "
            f"New article/scope pairs after cache and limit: {len(work_items)}."
        )
        return 0

    if not work_items:
        print("No new article/scope pairs to score.")
        return 0

    api_key = os.environ.get("OPENAI_API_KEY")
    if not api_key:
        raise SystemExit("OPENAI_API_KEY is not set. Set it in the environment before a live run.")

    print(f"Scoring {len(work_items)} article/scope pairs with {args.model}.")
    new_rows: list[dict[str, Any]] = []
    client = build_openai_client(api_key, max_retries=args.max_retries)
    try:
        with start_langfuse_run(
            "score-medium-headlines-run",
            input={"work_items": len(work_items), "temperature": args.temperature, "scopes": ",".join(scopes)},
            metadata={
                "script": "scoreMediumHeadlinesOpenai",
                "model": args.model,
                "rubricVersion": RUBRIC_VERSION,
                "inputPath": str(input_path),
            },
            tags=["medium-analysis", "headline-scoring"],
            session_id=str(input_path),
            trace_name="score-medium-headlines-openai",
        ):
            for index, (row, scope) in enumerate(work_items, start=1):
                title = clean_text(row.get("title")) or ""
                subtitle = get_subtitle(row)
                payload = build_payload(args.model, title, subtitle, scope, args.temperature)
                response: dict[str, Any] | None = None
                parsed: dict[str, Any] | None = None
                for validation_attempt in range(args.max_retries + 1):
                    response = call_openai(
                        client,
                        payload,
                        name="score-medium-headline-row",
                        metadata={
                            "article_id": get_article_id(row),
                            "score_scope": scope,
                            "rubric_version": RUBRIC_VERSION,
                        },
                    )
                    parsed = json.loads(extract_response_text(response))
                    try:
                        validate_scores(parsed)
                        break
                    except ValueError:
                        if validation_attempt >= args.max_retries:
                            raise
                        time.sleep(min(30, 2 ** validation_attempt))
                if response is None or parsed is None:
                    raise RuntimeError("OpenAI API returned no validated response")
                timestamp_utc = datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")
                flat = flatten_result(row, scope, title, subtitle, args.model, timestamp_utc, parsed)
                raw_record = {
                    "article_id": flat["article_id"],
                    "medium_post_id": flat["medium_post_id"],
                    "score_scope": scope,
                    "title": title,
                    "subtitle": subtitle,
                    "rubric_version": RUBRIC_VERSION,
                    "model": args.model,
                    "timestamp_utc": timestamp_utc,
                    "model_output": parsed,
                    "response_id": response.get("id"),
                }
                append_jsonl(jsonl_output, raw_record)
                new_rows.append(flat)

                if len(new_rows) >= 10:
                    append_csv_rows(csv_output, new_rows)
                    new_rows = []
                print(f"[{index}/{len(work_items)}] scored article_id={flat['article_id']} scope={scope}")
    finally:
        flush_langfuse()

    append_csv_rows(csv_output, new_rows)
    print(f"Wrote CSV: {csv_output}")
    print(f"Wrote raw JSONL: {jsonl_output}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
