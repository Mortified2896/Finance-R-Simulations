# Cloudflare development

## Existing baseline

The [PR #7 setup record](cloudflare-setup.md) reports these checks on 2026-09-29:
persistent RTX authentication, a temporary Worker returning HTTP 200 with the
expected text, D1 creation and `SELECT 1`, zone reads, and deletion of test resources.
These are recorded infrastructure results, not checks re-run by the docs cleanup.
No production review app, D1 schema, or Google/OTP login is implemented yet.

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
There is not yet a runnable frontend or production deploy configuration.
Once those exist, the authenticated command is `npm run cf -- deploy`.

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

## Application login: remaining Mac/browser setup

Deployment authentication is not reviewer login. The MVP uses Cloudflare Access
with **Google and email one-time PIN**, plus approval/roles in D1.

After O1 supplies the actual Worker and environment details, Mac Codex/the owner:

1. Configures the relevant Zero Trust application and both login methods. Google requires a Google OAuth client and the real Access team-domain callback; follow the [Google setup guide](https://developers.cloudflare.com/cloudflare-one/integrations/identity-providers/google/). Keep its client secret in the identity-provider configuration, not Git or React.
2. Allows identities using those login methods to reach registration, without a per-reviewer email allowlist. Use an authenticated Allow policy, not an Access Bypass. The Worker denies article access until D1 approval. Verify both [Google and OTP](https://developers.cloudflare.com/cloudflare-one/integrations/identity-providers/one-time-pin/) with a previously unknown user.
3. Selects a remembered session up to one month, reviewing effective application/policy/global settings. [Session expiration](https://developers.cloudflare.com/cloudflare-one/access-controls/access-settings/session-management/) requires authentication again, not renewed owner approval.
4. Protects every deployed entry point, including `workers.dev`, custom domains, and preview URLs, or disables unused entry points. O1 supplies the expected audience/issuer configuration and verifies rejection paths.
5. Confirms the owner's authenticated account and uses O1's one-time privileged bootstrap operation to make it admin. Never grant admin to the first public signup or infer the owner from a commit email.

[Current Workers Access docs](https://developers.cloudflare.com/workers/configuration/cloudflare-access/)
note that `ctx.access` is not forwarded through the Static Assets router. For the
planned React/static-assets app, validate the signed Access JWT using its trusted
issuer, application audience, signing keys, and time claims; see
[JWT validation](https://developers.cloudflare.com/cloudflare-one/access-controls/applications/http-apps/authorization-cookie/validating-json/).
Do not rely on a decoded token or a browser-supplied email. O1 may use a supported
runtime identity mechanism instead only after verifying it in the actual deployment.

Successful Access authentication can consume an Access seat before D1 approval.
Check the account's [seat management](https://developers.cloudflare.com/cloudflare-one/team-and-resources/users/seat-management/)
and capacity before opening registration widely. D1 rejection is not seat removal.

## Domain and release verification

Existing zone: `moneymattersmedia.com`. Proposed app hostname:
`feedback.moneymattersmedia.com`, not yet claimed or write-tested by the setup report.
Inspect it before attachment; preserve existing DNS/mail. No registrar migration,
Tunnel, or HomeLab port forwarding is part of deploying this app.

O1 verifies real hostname/TLS routing, both login methods, pending/approved/revoked
states, article isolation, and persistence after reload. A protected empty/synthetic
preview is fine before account setup; do not publish private articles behind a
fake login. Tuta send/receive acceptance is separate from the app implementation.

Official login references above were checked on 2026-09-29; recheck platform
behavior during deployment. Record tested URLs/results rather than marking the
MVP complete from configuration or a local test alone.
