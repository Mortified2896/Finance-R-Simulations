# Article Lab Cloudflare tooling

Infrastructure only. No React UI, production Worker, or D1 schema exists yet.
The existing `../human_preview_rating_app/` Shiny application remains unchanged.

On RTX, as `hermes`:

```sh
cd /home/hermes/workspace/repos/Finance-R-Simulations/apps/article_lab_web
npm ci
npx --no-install wrangler --version
```

Wrangler is pinned in `package.json` and `package-lock.json`; `.node-version`
records the installed, supported Node version. No global tool upgrade is needed.

After creating a scoped token in the Cloudflare dashboard, run this yourself in
an interactive RTX terminal (or your existing SSH terminal connected to RTX):

```sh
npm run cf:install-credential
```

The token prompt hides input and does not put the value in a shell command.
Do not paste it into chat or a recorded/shared terminal. Supply the account and
zone IDs from the dashboard at the subsequent prompts. The installer refuses
to overwrite an existing credential and does not claim that the token is valid.

Future sessions use the saved credential without logging in:

```sh
npm run cf -- whoami
npm run cf -- d1 list
# Once an application configuration exists:
npm run cf -- deploy
```

The launcher reads `~/.config/finance-r-simulations/cloudflare.json` (mode 600,
parent directory 700) and provides credentials only to its Wrangler process.
It does not change shell profiles, GitHub authentication, or system services.
Processes running as the same user and root can access this file; filesystem
permissions do not isolate applications sharing the `hermes` account.
Use `npm run cf -- ...` for authenticated commands; bare `npx wrangler` does
not automatically load this custom credential file. Keep debug/request logging
disabled. Never use `wrangler auth token` to display the token.

To rotate, revoke the old token in Cloudflare, remove only this credential file,
and rerun the installer. Revoking an API token does not revoke browser login.

Future code belongs here: `src/` for React/TypeScript, `worker/` for the Worker,
`migrations/` for D1 migrations, and `wrangler.jsonc` for public configuration.
Version-control those sources; keep tokens, `.dev.vars`, `.env` files, local D1
state, and `.wrangler/` out of Git. Do not route traffic to the RTX or add a Tunnel.

See [setup status and remaining verification](../../docs/cloudflare-setup.md).
