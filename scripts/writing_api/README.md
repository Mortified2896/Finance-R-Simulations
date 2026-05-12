# Writing API Helpers

This folder contains small writing helpers for article projects.

## Current status

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
