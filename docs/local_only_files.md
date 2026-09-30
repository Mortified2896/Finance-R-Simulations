# Runtime data and tracked source

Keep Git focused on source, documentation, reproducible configuration, and small
synthetic test fixtures. Runtime data can live in local storage or Cloudflare;
"not in Git" does not mean "must stay on the HomeServer".

## Keep out of Git

- Credentials, tokens, private keys, browser profiles, session cookies, and private environment files.
- `.env`, private `.env.*`, `.envrc`, `.dev.vars`, `.dev.vars.*`, `.wrangler/`, local D1 state, and database dumps/backups.
- Private article drafts, reviewer email addresses, real annotations, feedback, and raw API/model output.
- Local SQLite/R data, sidecars, downloaded media, research PDFs, captures, queues, caches, logs, and generated analysis artifacts.
- Local tool state such as `.playwright-mcp/`, `.opencode/vendor/`, `node_modules/`, and Python caches.
- Scratch/reference exports in `.local_gitignored/`, `tmp/`, `data/`, `debug_samples/`, and other ignored locations.

The RTX deployment credential is outside the repository; see
[Cloudflare development](cloudflare-development.md). File permissions protect it
from other Unix users, not from every process running as the same user or root.

## Track intentionally

Application source, lockfiles, public Wrangler configuration, SQL migrations,
example environment files containing no real values, and synthetic fixtures are
appropriate. Review fixtures and documentation screenshots for private content.
Verify ignore coverage rather than assuming a new file pattern is already covered.

Ignoring a file is not permission to delete it. The unused Shiny UI can be replaced
without assuming its local research datasets, articles, or feedback are disposable.
This cleanup changes no runtime data.
