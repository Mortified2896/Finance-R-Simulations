# Cloudflare development

## Existing baseline

The [PR #7 setup record](cloudflare-setup.md) reports these checks on 2026-09-29:
persistent RTX authentication, a temporary Worker returning HTTP 200 with the
expected text, D1 creation and `SELECT 1`, zone reads, and deletion of test resources.
These are recorded infrastructure results, not checks re-run by the docs cleanup.
The Article Review MVP is now implemented in `apps/article_lab_web/`, with a
production Worker, D1 migration and custom domain deployed. See the
[app README](../apps/article_lab_web/README.md) and
[Google setup](../apps/article_lab_web/GOOGLE_SETUP.md) for current operation.
Real Google authentication still requires OAuth client configuration.

RTX user: `hermes` on `rtx-omnigent`. Recorded checkout:
`/home/hermes/workspace/repos/Finance-R-Simulations`. The Mac used `codex-rtx` over
its existing SSH route. Verify local Git state before changing branches.

```sh
cd /home/hermes/workspace/repos/Finance-R-Simulations/apps/article_lab_web
npm ci
npm run cf -- whoami
npm run cf -- d1 list
```

Version pins live in the app's `package.json`, lockfile, and `.node-version`.
Build with `npm run build`; deploy with `npm run deploy`. Both Wrangler and the
Vite frontend configuration are source-controlled.

## Credentials and permissions

The launcher reads `~/.config/finance-r-simulations/cloudflare.json`: directory
mode 700, file mode 600, owned by `hermes`. It injects the deployment token into
Wrangler only. Bare `npx wrangler` does not load this file. Same-user processes
and root remain within its trust boundary. Do not inspect or print its value.

The recorded token grants one account Workers Admin + D1 Write, and the selected
zone Workers Routes Write + DNS Read + Zone Read. It does not grant DNS Write,
KV, R2, Access/Zero Trust administration, or account/billing administration.
Expiry: **December 29, 2026**. Rotate before expiry through the existing hidden-input
installer (`npm run cf:install-credential`); it refuses to overwrite a credential.
Plan the replacement/revocation so the old token is not lost before a new one works.

Wrangler's test-Worker deletion succeeded but its subsequent KV inventory call
failed. Confirm deletion through the Worker-specific API if this recurs; do not
interpret the final error as proof the Worker still exists or add KV reflexively.
Request a scoped permission change when an actual feature needs it.

## Application login: Google through Better Auth

The current implementation uses **Better Auth 1.7.6 inside the Worker**, native
D1 persistence, Google-only OAuth and application approval/roles. Access and
Zero Trust setup were abandoned before any agent-created objects or purchases.
The existing deployment token remains unchanged. Do not follow historical
Access setup instructions.

See [Google setup](../apps/article_lab_web/GOOGLE_SETUP.md) and the
[app README](../apps/article_lab_web/README.md). `BETTER_AUTH_SECRET` is installed;
Google OAuth client configuration still requires the owner's Google dashboard.
No email provider, OTP, password login or separate auth service is used.

## Domain and release verification

Existing zone: `moneymattersmedia.com`. Proposed app hostname:
`feedback.moneymattersmedia.com`, now attached to `article-lab-review` and HTTPS-tested.
Inspect it before attachment; preserve existing DNS/mail. No registrar migration,
Tunnel, or HomeLab port forwarding is part of deploying this app.

O1 verifies real hostname/TLS routing, Google login, pending/approved/revoked
states, article isolation, and persistence after reload. A protected empty/synthetic
preview is fine before account setup; do not publish private articles behind a
fake login. Tuta send/receive acceptance is separate from the app implementation.

Official login references in the app setup guide were checked on 2026-09-29; recheck platform
behavior during deployment. Record tested URLs/results rather than marking the
MVP complete from configuration or a local test alone.
