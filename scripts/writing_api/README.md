# Writing API Placeholders

These scripts are placeholders for later OpenAI API usage:

- `reroll_sentence.mjs`
- `draft_section.mjs`
- `critique_draft.mjs`

The API key should go in a local `.env` file:

```text
OPENAI_API_KEY=...
```

`.env` must not be committed.

Later, these scripts should read from the article project files in `article_projects/sp500-finfluencers/` and save outputs into `article_projects/sp500-finfluencers/api_outputs/`.

No OpenAI API calls are implemented yet.
