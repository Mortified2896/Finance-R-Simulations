# Article Lab Web

Future primary application. This directory currently contains Cloudflare tooling,
not a React UI, production Worker, or D1 application schema.

## Work to implement

Build the [article review MVP](../../docs/article-review-mvp.md) here.
[Architecture](../../docs/architecture.md) explains the broader direction;
[O1's handoff](../../docs/o1-review-mvp-handoff.md) describes the next work.

A practical layout is `src/` for the frontend, `worker/` for the API, `migrations/`
for D1, and `wrangler.jsonc` for public configuration. Adapt it when the chosen
Cloudflare-supported tooling has a clearer layout; this is not a rigid framework.

## Existing tooling

```sh
# From this directory, on the configured RTX environment:
npm ci
npx --no-install wrangler --version
npm run cf -- whoami
npm run cf -- d1 list
```

`package.json` and `package-lock.json` pin Wrangler; `.node-version` records the
provisioned Node version. This package is separate from the root helper package.

`tools/cloudflare.py` loads the RTX credential only into its Wrangler child
process. Use `npm run cf -- <command>` for authenticated operations. Do not copy
the token into the app, shell profile, frontend, Git, or a Worker secret.

The [development guide](../../docs/cloudflare-development.md) covers credentials,
permissions, login setup, and the remaining live checks. Authentication is already
installed on RTX; do not repeat installation during ordinary development.

Add and document working dev/build/test/deploy commands as implementation lands.
A mock login or successful build alone is not verification of real Google/OTP login.
