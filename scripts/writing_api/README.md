# Writing API Helpers

This folder contains small writing helpers for article projects.

## Current status

- `generate_titles.mjs`: implemented for Article Lab title generation from the OpenAI API.
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
