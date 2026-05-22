# Writing API Helpers

This folder contains small writing helpers for article projects.

## Current status

- `generate_titles.mjs`: implemented for Article Lab title generation from the OpenAI API.
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

## API key

Create a local `.env` file in the repository root:

```text
OPENAI_API_KEY=...
```

`.env` must not be committed.

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
