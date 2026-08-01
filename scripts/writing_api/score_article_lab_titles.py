#!/usr/bin/env python3
"""
Score Article Lab generated titles with the same v2_2 title-only rubric used in
the Medium title API scoring workflow.
"""

from __future__ import annotations

import json
import os
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

SCRIPT_ROOT = Path(__file__).resolve().parents[1]
if str(SCRIPT_ROOT) not in sys.path:
    sys.path.insert(0, str(SCRIPT_ROOT))

from langfuse_python import build_openai_client, flush_langfuse, langfuse_enabled, start_langfuse_run

DEFAULT_MODEL = os.environ.get("OPENAI_TITLE_SCORING_MODEL", "gpt-5-mini")
DEFAULT_PROMPT_VERSION = "v2_3"
DEFAULT_SCOPE = "title_only"
VALID_SCOPES = {"title_only"}
MAX_RETRIES = 3

SCORE_FIELDS = [
    "curiosity",
    "emotional_pull",
    "medium_comment_potential",
    "overall_article_potential",
    "trust_risk",
]


def clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = " ".join(str(value).replace("\u00a0", " ").split()).strip()
    return text or None


def utc_now() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def load_api_key() -> str | None:
    env_value = clean_text(os.environ.get("OPENAI_API_KEY"))
    if env_value:
        return env_value
    env_path = Path(".env")
    if not env_path.exists():
        return None
    for raw_line in env_path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        if key.strip() != "OPENAI_API_KEY":
            continue
        parsed = clean_text(value.strip().strip("'").strip('"'))
        if parsed:
            return parsed
    return None


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


def prompt_content(prompt_version: str, title: str, scope: str) -> str:
    if prompt_version == "v2_3":
        return (
            f"Prompt version: {prompt_version}\n\n"
            f"Score scope: {scope}\n"
            "You are scoring only the title. Do not infer a subtitle. Treat missing context as missing.\n\n"
            "Important measurement note:\n"
            "Do not estimate click potential. For competitor Medium articles, we do not have impressions, views, reads, or click-through data, so click potential is not directly testable in this dataset.\n\n"
            "Focus instead on outcomes that can be compared against observed public metrics:\n"
            "- medium_comment_potential: how likely this article is to receive written responses/comments, especially because it invites disagreement, personal stories, debate, strong opinions, corrections, or follow-up questions.\n"
            "- overall_article_potential: overall expected Medium performance based on the title only, considering likely reader interest, topic strength, emotional pull, trust, and engagement potential.\n\n"
            "Calibrate scores relative to typical Medium personal finance articles, not in isolation.\n\n"
            "Use the full 1-5 scale aggressively:\n"
            "1 = very weak, likely below average\n"
            "2 = below average or generic\n"
            "3 = average / okay for Medium finance\n"
            "4 = clearly above average, likely stronger than most articles\n"
            "5 = exceptional, rare, top-tier potential\n\n"
            "Most normal articles should receive 2 or 3.\n"
            "Do not give 4 unless the title has a clearly strong hook, strong topic demand, meaningful emotional or discussion pull, and a clear reader payoff.\n"
            "Do not give 5 unless the title looks unusually compelling and would plausibly belong among the strongest articles in the dataset.\n"
            "Avoid defaulting to 4 for merely competent, useful, or credible articles.\n\n"
            "Input fields, and no other article data:\n"
            f"{json.dumps({'title': title}, ensure_ascii=False, indent=2)}\n\n"
            "Rubric:\n"
            "- curiosity: How much the title creates a genuine desire to know more.\n"
            "- emotional_pull: How much the title creates emotional interest, concern, excitement, surprise, or urgency.\n"
            "- medium_comment_potential: Estimate how likely the article is to generate written Medium responses/comments. Higher scores should go to title wording that invites disagreement, debate, personal experiences, corrections, strong opinions, or nuanced discussion. A useful but straightforward article can have high clap potential but low comment potential. Use the full scale.\n"
            "- overall_article_potential: Estimate overall Medium performance potential from the title only. This should be a relative ranking judgment, not a quality compliment. Consider topic demand, emotional stakes, trust, likely engagement, and whether the title feels meaningfully differentiated from generic finance content. Use 5 sparingly for likely top-decile potential.\n"
            "- trust_risk: Risk that the title feels exaggerated, misleading, too clickbaity, or credibility-damaging. Higher means more risk. A title can create curiosity or emotion while still carrying trust risk.\n\n"
            "predicted_success_bucket:\n"
            "- low = likely below median or weak relative to typical Medium finance articles.\n"
            "- medium = around median to moderately above average.\n"
            "- high = likely top 20 percent potential. Use high sparingly. Do not classify most articles as high.\n\n"
            "Return JSON matching the schema exactly. short_reason must be one short sentence."
        )

    return (
        f"Prompt version: {prompt_version}\n\n"
        f"Score scope: {scope}\n"
        "You are scoring only the title. Do not infer a subtitle. Treat missing context as missing.\n\n"
        "Input fields, and no other article data:\n"
        f"{json.dumps({'title': title}, ensure_ascii=False, indent=2)}\n\n"
        "Return JSON matching the schema exactly."
    )


def render_prompt_template(template: str, *, title: str, prompt_version: str, scope: str) -> str:
    rendered = template.replace("{{title}}", title).replace("{{prompt_version}}", prompt_version).replace("{{scope}}", scope)
    if "{{" in rendered or "}}" in rendered:
        raise ValueError("Scoring prompt contains an unknown or unresolved template variable")
    return rendered


def build_payload(model: str, title: str, prompt_version: str, scope: str, reasoning_effort: str | None = None, reasoning_mode: str = "standard", prompt_template: str | None = None) -> dict[str, Any]:
    schema_name = f"article_lab_title_scores_{prompt_version}_{scope}".replace("-", "_")
    request = {
        "model": model,
        "input": render_prompt_template(prompt_template, title=title, prompt_version=prompt_version, scope=scope) if prompt_template else prompt_content(prompt_version, title, scope),
        "text": {
            "format": {
                "type": "json_schema",
                "name": schema_name,
                "strict": True,
                "schema": score_schema(),
            }
        },
    }
    if reasoning_effort:
        request["reasoning"] = {"effort": reasoning_effort}
    if reasoning_mode == "pro":
        request.setdefault("reasoning", {})["mode"] = "pro"
    return request


def extract_response_text(response: dict[str, Any]) -> str:
    if isinstance(response.get("output_text"), str):
        return response["output_text"]
    chunks: list[str] = []
    for item in response.get("output") or []:
        for content in item.get("content") or []:
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


def load_request(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text(encoding="utf-8"))


def main() -> int:
    if len(sys.argv) != 2:
        print("Usage: python3 scripts/writing_api/score_article_lab_titles.py <request.json>", file=sys.stderr)
        return 1

    request_path = Path(sys.argv[1])
    if not request_path.exists():
        print(f"Request file not found: {request_path}", file=sys.stderr)
        return 1

    api_key = load_api_key()
    if not api_key:
        print("OPENAI_API_KEY is not set.", file=sys.stderr)
        return 1

    payload = load_request(request_path)
    model = clean_text(payload.get("model")) or DEFAULT_MODEL
    prompt_version = clean_text(payload.get("prompt_version")) or DEFAULT_PROMPT_VERSION
    scope = clean_text(payload.get("scope")) or DEFAULT_SCOPE
    reasoning_effort = clean_text(payload.get("reasoning_effort"))
    reasoning_mode = clean_text(payload.get("reasoning_mode")) or "standard"
    prompt_template = clean_text(payload.get("prompt_template"))
    if scope not in VALID_SCOPES:
        print(f"Unsupported scope: {scope}", file=sys.stderr)
        return 1

    raw_candidates = payload.get("candidates")
    if not isinstance(raw_candidates, list) or not raw_candidates:
        print("Request must include a non-empty candidates list.", file=sys.stderr)
        return 1

    scores: list[dict[str, Any]] = []
    errors: list[dict[str, Any]] = []
    client = build_openai_client(api_key, max_retries=MAX_RETRIES)

    try:
        with start_langfuse_run(
            "score-article-lab-titles-run",
            input={"request_path": str(request_path), "candidate_count": len(raw_candidates)},
            metadata={
                "script": "scoreArticleLabTitles",
                "model": model,
                "promptVersion": prompt_version,
                "scoreScope": scope,
                "requestPath": str(request_path),
            },
            tags=["writing-api", "title-scoring"],
            session_id=str(request_path),
            trace_name="score-article-lab-titles",
        ):
            for entry in raw_candidates:
                candidate_id = clean_text(entry.get("candidate_id"))
                batch_id = clean_text(entry.get("batch_id"))
                title = clean_text(entry.get("title"))
                if not candidate_id or not batch_id or not title:
                    errors.append(
                        {
                            "candidate_id": candidate_id,
                            "batch_id": batch_id,
                            "error": "candidate_id, batch_id, and title are required",
                        }
                    )
                    continue

                try:
                    response = call_openai(
                        client,
                        build_payload(model, title, prompt_version, scope, reasoning_effort, reasoning_mode, prompt_template),
                        name="score-article-lab-title",
                        metadata={
                            "candidate_id": candidate_id,
                            "batch_id": batch_id,
                            "prompt_version": prompt_version,
                            "score_scope": scope,
                        },
                    )
                    parsed = json.loads(extract_response_text(response))
                    validate_model_output(parsed)
                    score_row = {
                        "candidate_id": candidate_id,
                        "batch_id": batch_id,
                        "scored_at": utc_now(),
                        "model": model,
                        "reasoning_effort": reasoning_effort,
                        "reasoning_mode": reasoning_mode,
                        "prompt_version": prompt_version,
                        "scope": scope,
                        "predicted_success_bucket": parsed["predicted_success_bucket"],
                        "short_reason": parsed["short_reason"],
                        "raw_json": response,
                    }
                    for field in SCORE_FIELDS:
                        score_row[field] = parsed[field]
                    scores.append(score_row)
                except Exception as error:  # noqa: BLE001
                    errors.append(
                        {
                            "candidate_id": candidate_id,
                            "batch_id": batch_id,
                            "error": str(error),
                        }
                    )
    finally:
        flush_langfuse()

    result = {
        "mode": "api",
        "model": model,
        "reasoning_effort": reasoning_effort,
        "reasoning_mode": reasoning_mode,
        "prompt_version": prompt_version,
        "scope": scope,
        "scored_count": len(scores),
        "failed_count": len(errors),
        "scores": scores,
        "errors": errors,
    }
    sys.stdout.write(json.dumps(result, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
