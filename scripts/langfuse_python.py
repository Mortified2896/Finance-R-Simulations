from __future__ import annotations

from contextlib import ExitStack, nullcontext
import os
from typing import Any, Mapping


def _clean_env(name: str) -> str | None:
    value = os.environ.get(name)
    if value is None:
        return None
    cleaned = str(value).strip()
    return cleaned or None


def _tracing_disabled() -> bool:
    value = (_clean_env("LANGFUSE_TRACING_ENABLED") or "").lower()
    return value in {"0", "false", "no", "off"}


def _normalize_langfuse_env() -> None:
    host = _clean_env("LANGFUSE_HOST")
    if host and not _clean_env("LANGFUSE_BASE_URL"):
        os.environ["LANGFUSE_BASE_URL"] = host


def langfuse_enabled() -> bool:
    if _tracing_disabled():
        return False
    _normalize_langfuse_env()
    return bool(
        _clean_env("LANGFUSE_PUBLIC_KEY")
        and _clean_env("LANGFUSE_SECRET_KEY")
        and _clean_env("LANGFUSE_BASE_URL")
    )


def _missing_package_error(package_names: str) -> RuntimeError:
    return RuntimeError(
        f"Missing Python package(s) required for OpenAI/Langfuse tracing. Install with: python3 -m pip install {package_names}"
    )


def sanitize_metadata(metadata: Mapping[str, Any] | None) -> dict[str, str] | None:
    if not metadata:
        return None
    cleaned: dict[str, str] = {}
    for raw_key, raw_value in metadata.items():
        key = "".join(ch for ch in str(raw_key) if ch.isalnum())
        if not key or raw_value is None:
            continue
        value = str(raw_value).strip()
        if not value:
            continue
        if len(value) > 200:
            value = value[:197] + "..."
        cleaned[key] = value
    return cleaned or None


def build_openai_client(api_key: str, max_retries: int = 0):
    _normalize_langfuse_env()
    try:
        if langfuse_enabled():
            from langfuse.openai import OpenAI
        else:
            from openai import OpenAI
    except ImportError as error:
        package_names = "langfuse openai" if langfuse_enabled() else "openai"
        raise _missing_package_error(package_names) from error
    return OpenAI(api_key=api_key, max_retries=max_retries)


def start_langfuse_run(
    name: str,
    *,
    input: Any | None = None,
    metadata: Mapping[str, Any] | None = None,
    tags: list[str] | None = None,
    session_id: str | None = None,
    user_id: str | None = None,
    trace_name: str | None = None,
):
    if not langfuse_enabled():
        return nullcontext()

    try:
        from langfuse import get_client, propagate_attributes
    except ImportError as error:
        raise _missing_package_error("langfuse openai") from error

    stack = ExitStack()
    stack.enter_context(get_client().start_as_current_observation(as_type="span", name=name, input=input))

    attributes: dict[str, Any] = {}
    cleaned_metadata = sanitize_metadata(metadata)
    if cleaned_metadata:
        attributes["metadata"] = cleaned_metadata
    if tags:
        attributes["tags"] = tags
    if session_id:
        attributes["session_id"] = session_id
    if user_id:
        attributes["user_id"] = user_id
    if trace_name:
        attributes["trace_name"] = trace_name
    if attributes:
        stack.enter_context(propagate_attributes(**attributes))

    return stack


def flush_langfuse() -> None:
    if not langfuse_enabled():
        return
    try:
        from langfuse import get_client
    except ImportError as error:
        raise _missing_package_error("langfuse openai") from error
    get_client().flush()
