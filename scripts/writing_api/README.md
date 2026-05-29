# Writing API Helpers

This folder contains small writing helpers for article projects.

## Current status

- `generate_titles.mjs`: implemented for Article Lab title generation from the OpenAI API.
- `generate_subtitles.mjs`: implemented for Article Lab subtitle generation from the OpenAI API.
- `generate_thumbnails.mjs`: implemented for Article Lab thumbnail generation through the OpenAI Responses API image-generation tool.
- `generate_outlines.mjs`: implemented for Article Lab outline generation from approved title/subtitle/thumbnail packages.
- `score_article_lab_titles.py`: implemented for Article Lab title-only API scoring with the v2_2 rubric.
- `reroll_sentence.mjs`: implemented for rewriting one selected sentence or paragraph.
- `draft_section.mjs`: placeholder only.
- `critique_draft.mjs`: placeholder only.

## Installation

From the repository root, install the minimal Node dependencies:

```sh
npm install
```

This installs:

- `openai`
- `dotenv`
- `@langfuse/openai`
- `@langfuse/otel`
- `@langfuse/tracing`

## API key

Create a local `.env` file in the repository root:

```text
OPENAI_API_KEY=...
```

`.env` must not be committed.

## Optional Langfuse tracing

Tracing is enabled automatically for the Node and Python OpenAI helpers when these environment variables are present:

```text
LANGFUSE_PUBLIC_KEY=pk-lf-...
LANGFUSE_SECRET_KEY=sk-lf-...
LANGFUSE_BASE_URL=https://cloud.langfuse.com
```

`LANGFUSE_HOST` is also accepted and mapped to `LANGFUSE_BASE_URL` automatically.

For the Python scoring scripts, install the runtime packages in your local Python environment:

```sh
python3 -m pip install openai langfuse
```

If the Shiny Article Lab app is using the wrong interpreter, set `ARTICLE_LAB_PYTHON` before launch so the API scoring tab uses the Python executable where `openai` is installed.

## Generate article titles

Run from the repository root:

```sh
node scripts/writing_api/generate_titles.mjs /path/to/request.json
```

The request JSON should include:

```json
{
  "prompt": "Generate Medium-style titles...",
  "batch_size": 12,
  "seed_topic": "optional angle",
  "inspiration_source": "top performing titles",
  "model": "gpt-5-mini",
  "example_titles": ["Example title 1", "Example title 2"]
}
```

The helper prints JSON to stdout with a `titles` array and API metadata. The Shiny app uses this helper behind the Article Lab Generate button.

## Generate Article Lab thumbnails

Run from the repository root:

```sh
node scripts/writing_api/generate_thumbnails.mjs /path/to/request.json
```

The request JSON should include:

```json
{
  "model": "gpt-5.5",
  "prompt": "Generate Medium-style thumbnail candidates...",
  "variants_per_package": 3,
  "packages": [
    {
      "subtitle_id": "als_example",
      "candidate_id": "alc_example",
      "batch_id": "alb_example",
      "title": "Index Fund Mistakes Beginners Make",
      "subtitle": "A practical breakdown without hype"
    }
  ]
}
```

The helper uses `client.responses.create()` with the Responses API built-in `image_generation` tool and prints JSON to stdout. The `model` field is the Responses model that coordinates the generation and calls the image tool. Image-specific models such as `gpt-image-2` are used by the direct Images API, not by this primary Responses API path.

```json
{
  "mode": "api",
  "model": "gpt-5.5",
  "results": [
    {
      "subtitle_id": "als_example",
      "candidate_id": "alc_example",
      "batch_id": "alb_example",
      "title": "Index Fund Mistakes Beginners Make",
      "subtitle": "A practical breakdown without hype",
      "thumbnails": [
        {
          "thumbnail_label": "API concept 1",
          "thumbnail_data_uri": "data:image/png;base64,...",
          "created_at": "2026-05-29T00:00:00Z",
          "model": "gpt-5.5",
          "generation_mode": "api",
          "raw_json": {
            "response_id": "resp_...",
            "output_id": "...",
            "call_id": "...",
            "revised_prompt": null,
            "usage": null,
            "model": "gpt-5.5",
            "variant_index": 1
          }
        }
      ]
    }
  ]
}
```

`thumbnail_data_uri` is stored directly in the existing Article Lab thumbnail table. `raw_json` preserves response and image-generation metadata intended to make a later prompt-edit flow easier to add.

The Shiny thumbnail tab selects from a curated Responses generation model list and defaults to `OPENAI_THUMBNAIL_GENERATION_MODEL` when set. `OPENAI_THUMBNAIL_RESPONSES_MODEL` is also accepted as a secondary fallback, followed by the app default.

If live API generation fails in the Shiny app, the app keeps the existing local SVG stub fallback. The generated rows are marked `generation_mode = "stub"`, and the UI notice includes `stub mode` plus the fallback reason so API failures are visible.

## Generate Article Lab outlines

Run from the repository root:

```sh
node scripts/writing_api/generate_outlines.mjs /path/to/request.json
```

The request JSON includes a `model`, `prompt`, and approved title/subtitle/thumbnail `packages`. The helper prints JSON with one Markdown `outline_text` per package. The Shiny Outline tab stores those drafts in `article_lab_outlines` for editing and approval.

## Score generated Article Lab titles

Run from the repository root:

```sh
python3 scripts/writing_api/score_article_lab_titles.py /path/to/request.json
```

The request JSON should include:

```json
{
  "model": "gpt-5-mini",
  "prompt_version": "v2_2",
  "scope": "title_only",
  "candidates": [
    {
      "candidate_id": "alc_batch_01",
      "batch_id": "alb_batch",
      "title": "Index Fund Mistakes Beginners Make"
    }
  ]
}
```

The helper prints JSON to stdout with per-candidate title scores and raw response payloads. The Shiny app uses this helper behind the Article Lab API score tab.

## Reroll a sentence or paragraph

Run from the repository root:

```sh
node scripts/writing_api/reroll_sentence.mjs "The S&P 500 is not the problem. The problem is treating it as the default."
```

The helper reads:

- `article_projects/sp500-finfluencers/style_rules.md`
- `article_projects/sp500-finfluencers/brief.md`

It prints 8 rewrite alternatives to the terminal and saves a Markdown output file to:

```text
article_projects/sp500-finfluencers/api_outputs/
```

The output filename uses this format:

```text
reroll_sentence_YYYYMMDD_HHMMSS.md
```
