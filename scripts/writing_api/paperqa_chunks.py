#!/usr/bin/env python3
"""
Lightweight PaperQA2 chunk retrieval runner.

Performs query-based evidence retrieval against one local PDF using
PaperQA2 as a citation/context candidate engine.

Returns structured JSON to stdout. If PaperQA2 is not installed,
returns structured JSON diagnostics instead of crashing
(mode=paperqa_missing).

Usage:
    python3 scripts/writing_api/paperqa_chunks.py <request.json>

Request JSON schema:
{
    "local_pdf_path": "/absolute/path/to/file.pdf",
    "research_source_id": 1,
    "source_title": "Optional title for chunk metadata",
    "query": "Claim or question for evidence retrieval",
    "output_dir": "/path/to/output/dir",
    "chunk_chars": 1500,
    "chunk_overlap": 100
}

Output JSON schema (query mode, PaperQA2 available):
{
    "mode": "paperqa_query",
    "query": "...",
    "research_source_id": 1,
    "source_title": "...",
    "source_pdf_path": "...",
    "answer": "...",
    "contexts": [
        {
            "chunk_id": 0,
            "text": "...",
            "char_count": ...,
            "relevance_score": 0.85,
            "page_hint": 1,
            "citation": "...",
            "source": "..."
        }
    ],
    "created_at": "2026-01-01T00:00:00Z",
    "paperqa_version": "0.x.x",
    "diagnostics": {}
}

Blank whole-PDF chunking is intentionally not implemented in Stage 1.
"""

from __future__ import annotations

import json
import os
import sys
import traceback
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


SCRIPT_ROOT = Path(__file__).resolve().parents[1]
if str(SCRIPT_ROOT) not in sys.path:
    sys.path.insert(0, str(SCRIPT_ROOT))

SCRIPT_NAME = "paperqa_chunks"
DEFAULT_CHUNK_CHARS = 1500
DEFAULT_CHUNK_OVERLAP = 100


def utc_now() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def clean_text(value: Any) -> str | None:
    if value is None:
        return None
    text = " ".join(str(value).replace("\u00a0", " ").split()).strip()
    return text or None


def build_missing_diagnostics(python_exc: str | None = None) -> dict[str, Any]:
    import subprocess
    pip_result = ""
    try:
        pip_check = subprocess.run(
            [sys.executable, "-m", "pip", "list", "--format=columns"],
            capture_output=True, text=True, timeout=15
        )
        pip_result = pip_check.stdout.strip()[:2000] if pip_check.returncode == 0 else pip_check.stderr.strip()[:1000]
    except Exception as e:
        pip_result = f"pip check failed: {e}"
    return {
        "mode": "paperqa_missing",
        "error": "PaperQA2 is not installed.",
        "detail": python_exc or "No module named 'paperqa'",
        "python_version": f"{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}",
        "python_executable": sys.executable,
        "pip_check": pip_result,
        "hint": "pip install paper-qa (requires Python >= 3.11)",
        "diagnostics_printed_at": utc_now(),
    }


def _json_attr(value: Any) -> Any:
    """Return a JSON-safe representation for scalar metadata."""
    if value is None or isinstance(value, (str, int, float, bool)):
        return value
    try:
        return str(value)
    except Exception:
        return repr(value)


def run_paperqa_query(pdf_path: str, query: str) -> tuple[str | None, list[dict[str, Any]], dict[str, Any], str]:
    import paperqa
    pq_version = getattr(paperqa, "__version__", "unknown")
    docs = paperqa.Docs()
    add_method = getattr(docs, "add", None) or getattr(docs, "add_path", None)
    if add_method is None:
        raise RuntimeError("PaperQA Docs object has neither add() nor add_path(); unsupported PaperQA2 API shape.")
    add_method(pdf_path)
    answer = docs.query(query)
    answer_text = (
        getattr(answer, "answer", None)
        or getattr(answer, "formatted_answer", None)
        or getattr(answer, "text", None)
    )
    raw_contexts = getattr(answer, "contexts", None)
    if raw_contexts is None:
        raw_contexts = getattr(answer, "context", None)
    if raw_contexts is None:
        raw_contexts = []
    chunks = []
    for ctx in raw_contexts:
        ctx_text = getattr(ctx, "text", "") or ""
        score = getattr(ctx, "score", None)
        if isinstance(score, float):
            score = round(score, 4)
        page = getattr(ctx, "page_number", None) or getattr(ctx, "page", None) or getattr(ctx, "pages", None)
        citation = getattr(ctx, "citation", None)
        doc_name = getattr(ctx, "name", None) or getattr(ctx, "docname", None) or getattr(ctx, "doc_name", None)
        if doc_name is None:
            doc = getattr(ctx, "doc", None)
            if doc is not None:
                doc_name = getattr(doc, "name", None) or getattr(doc, "docname", None)
                if citation is None:
                    citation = getattr(doc, "citation", None)
        chunks.append({
            "chunk_id": len(chunks),
            "text": ctx_text,
            "char_count": len(ctx_text),
            "relevance_score": _json_attr(score),
            "page_hint": _json_attr(page),
            "citation": _json_attr(citation),
            "source": _json_attr(doc_name),
        })
    diagnostics = {
        "paperqa_version": pq_version,
        "retrieved_contexts": len(chunks),
        "pdf_path": pdf_path,
        "pdf_exists": os.path.isfile(pdf_path),
        "pdf_size_bytes": os.path.getsize(pdf_path) if os.path.isfile(pdf_path) else 0,
    }
    return answer_text, chunks, diagnostics, pq_version


def load_request(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text(encoding="utf-8"))


def main() -> int:
    if len(sys.argv) != 2:
        print(json.dumps({
            "mode": "error",
            "error": "Usage: python3 scripts/writing_api/paperqa_chunks.py <request.json>",
        }, ensure_ascii=False))
        return 1

    request_path = Path(sys.argv[1])
    if not request_path.exists():
        print(json.dumps({
            "mode": "error",
            "error": f"Request file not found: {request_path}",
        }, ensure_ascii=False))
        return 1

    payload = load_request(request_path)
    pdf_path = clean_text(payload.get("local_pdf_path"))
    research_source_id = payload.get("research_source_id")
    source_title = clean_text(payload.get("source_title")) or "untitled"
    query = clean_text(payload.get("query"))
    output_dir_raw = clean_text(payload.get("output_dir"))
    chunk_chars = int(payload.get("chunk_chars", DEFAULT_CHUNK_CHARS))
    chunk_overlap = int(payload.get("chunk_overlap", DEFAULT_CHUNK_OVERLAP))

    if not pdf_path or not os.path.isfile(pdf_path):
        print(json.dumps({
            "mode": "error",
            "error": f"local_pdf_path is missing or not a file: {pdf_path}",
        }, ensure_ascii=False))
        return 1

    project_root = SCRIPT_ROOT
    if not output_dir_raw:
        output_dir_raw = str(project_root / "data" / "research_paperqa_chunks")
    output_dir = Path(output_dir_raw)
    output_dir.mkdir(parents=True, exist_ok=True)

    source_slug = f"source_{research_source_id}" if research_source_id else "source_unknown"
    chunks_file = output_dir / f"{source_slug}_chunks.json"

    def emit(result: dict[str, Any], exit_code: int = 0, save: bool = True) -> int:
        if save:
            chunks_file.write_text(json.dumps(result, ensure_ascii=False, indent=2), encoding="utf-8")
        sys.stdout.write(json.dumps(result, ensure_ascii=False))
        return exit_code

    chunks: list[dict[str, Any]] = []
    diagnostics: dict[str, Any] = {}
    paperqa_version = "unknown"
    answer_text: str | None = None
    mode: str = "paperqa"

    if not query:
        error_result = {
            "mode": "error",
            "error": "Stage 1 PaperQA2 retrieval requires a claim/question query.",
            "research_source_id": research_source_id,
            "source_title": source_title,
            "query": query,
            "source_pdf_path": pdf_path,
            "created_at": utc_now(),
            "chunks_dir": str(output_dir) + "/",
            "chunks_file": str(chunks_file),
            "diagnostics": {"hint": "Pass a non-empty query in the request JSON."},
        }
        return emit(error_result, exit_code=1)

    try:
        answer_text, chunks, diagnostics, paperqa_version = run_paperqa_query(pdf_path, query)
        mode = "paperqa_query"
    except ImportError as exc:
        missing_result = build_missing_diagnostics(str(exc))
        missing_result["research_source_id"] = research_source_id
        missing_result["source_title"] = source_title
        missing_result["query"] = query
        missing_result["source_pdf_path"] = pdf_path
        missing_result["created_at"] = utc_now()
        missing_result["chunks_dir"] = str(output_dir) + "/"
        missing_result["chunks_file"] = str(chunks_file)
        missing_result["contexts"] = []
        missing_result["chunk_count"] = 0
        return emit(missing_result, exit_code=0)
    except Exception as exc:
        traceback_text = traceback.format_exc()
        error_result = {
            "mode": "error",
            "error": str(exc),
            "traceback": traceback_text,
            "research_source_id": research_source_id,
            "source_title": source_title,
            "query": query,
            "source_pdf_path": pdf_path,
            "created_at": utc_now(),
            "chunks_dir": str(output_dir) + "/",
            "chunks_file": str(chunks_file),
        }
        return emit(error_result, exit_code=1)

    result: dict[str, Any] = {
        "mode": mode,
        "query": query,
        "research_source_id": research_source_id,
        "source_title": source_title,
        "source_pdf_path": pdf_path,
        "answer": answer_text,
        "contexts": chunks,
        "context_count": len(chunks),
        "created_at": utc_now(),
        "paperqa_version": paperqa_version,
        "diagnostics": diagnostics,
        "chunks_dir": str(output_dir) + "/",
        "chunks_file": str(chunks_file),
    }

    return emit(result, exit_code=0)


if __name__ == "__main__":
    raise SystemExit(main())
