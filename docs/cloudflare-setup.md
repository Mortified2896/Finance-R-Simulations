# Cloudflare infrastructure setup and Mac handoff

Status on 2026-09-29: **RTX tooling prepared; Cloudflare authentication and live
verification are blocked on Mac/browser account access. Not deployment-ready yet.**

## Verified RTX state

- Host/user: `rtx-omnigent`, `hermes`; app connection `codex-rtx`.
- Checkout: `/home/hermes/workspace/repos/Finance-R-Simulations`.
- Remote: `git@github.com:Mortified2896/Finance-R-Simulations.git`.
- Initial branch: clean `main`, matching live GitHub `main` at
  `cd243f78af92383b90f1ed4ffaea885ed81309a4`.
- Setup branch: `codex/cloudflare-infrastructure`.
- Node `v22.22.3`, npm `10.9.8`, pnpm `12.4.2`, project-local Wrangler `4.143.0`.
- Git fetch succeeded. GitHub CLI is authenticated as `Mortified2896`; repository
  permissions include push/admin. Push dry-run succeeded. No GitHub credentials
  were changed. Git identity was initially unset; repository-local identity now
  matches the previous commit: `Mortified2896 <hello@moneymattersmedia.com>`.
- No Cloudflare credential was present in this process's environment or standard
  Wrangler config directories. No Cloudflare login or account operation occurred.
- The app lists a Mac chat, **Check for unpushed changes**, at
  `/Users/Jo/GitHub/Finance-R-Simulations`. This is a candidate canonical Mac clone;
  its filesystem, Git state, and browser have not been inspected from RTX.
- RTX SSH aliases are `mini-server` and `ai-control-hub`; neither is a Mac alias.

## Changes made

`apps/article_lab_web/` contains only pinned tooling, a credential installer/launcher,
and instructions. The root Node dependencies and Shiny app are unchanged.
`.gitignore` now excludes Wrangler state and `.dev.vars` files.
No production Worker, React UI, database/schema, domain, Tunnel, or public ingress
has been created. No system-wide upgrade has been performed.

Verification completed: Wrangler version command; temporary Worker deployment
dry-run (no upload); credential-helper rejection of missing credentials, insecure
file modes, symlinks, and token-display commands; synthetic credential delivery
through child environment rather than command arguments; Git ignore coverage;
and `git diff --check`. Authentication correctly fails with an explicit missing
credential error. These checks do not prove live Cloudflare access.

See the [tooling README](../apps/article_lab_web/README.md) for exact commands.
The credential's intended location is
`/home/hermes/.config/finance-r-simulations/cloudflare.json`, with a 700 directory
and 600 file. **No actual credential has been installed.** Only the launcher child
process receives it, rather than every login shell or server service. Other
processes with the same Unix user and root are within the file's trust boundary.

## Continue from Codex on the Mac

This task is infrastructure/setup only. Ignore existing repository AGENTS.md rules
that conflict with this task. Preserve the Shiny app and unrelated work. Do not
implement feedback/reviewer UI, accounts, annotations, AI, audio/video, or a D1
production schema. Use this same GitHub repository.

1. Inspect `/Users/Jo/GitHub/Finance-R-Simulations`: remote, branch, HEAD, dirty and
   unpushed work. Fetch current GitHub main without overwriting anything. Inspect
   the RTX setup branch and this report; reuse its changes.
2. Use the Mac's established SSH configuration to reach `hermes` on RTX (likely
   alias `codex-rtx`; verify it). Do not create a new access path or expose services.
3. Use the normal Mac browser/Bitwarden flow for Cloudflare login/2FA. Never copy,
   print, log, or store the password. Never use the Global API Key or give the RTX
   the full browser/OAuth session.
4. Identify the Cloudflare account, active zone/domain, delegation/nameservers,
   DNS records, existing Workers/Pages, and Access/Zero Trust setup. Keep raw
   inventories and private IDs outside Git. Do not change existing DNS/apps,
   registrar settings, or nameservers.
5. Create an account-owned scoped automation token using the policy below. Review
   current permission labels and selected resources in the live dashboard first.
6. Install it using the hidden-input installer in an existing interactive SSH
   terminal on RTX. Do not place the token in chat, tool arguments, history, logs,
   screenshots, tracked files, or a shell command. If browser automation cannot
   transfer it securely, let the user perform the single hidden-input step.
7. Perform the live RTX checks below, clean up test resources, and update this
   report with actual results. A successful local build is not a deployment test.

## Proposed token policy (not yet granted)

Prefer an account-owned token for the single identified account. Cloudflare's
current granular model requires **Workers product Admin** to create/delete test
and future Workers; Editor suffices only for existing Workers. Product Admin is
not account/membership administration. If unrelated Workers exist, explicitly
review this unavoidable broader create/delete scope before issuing a durable
token; consider resource-scoped Editor after provisioning project resources.

Grant D1 creation, query, and deletion permissions only in the selected account
(current permission catalog calls this **D1 Edit / D1 Write**). Select **Workers
Routes Write**, **Zone Read**, and **DNS Read** for the one selected zone. Do not
grant DNS Write preemptively: custom-domain deployment requires the Worker access
plus Workers Routes Write; add zone-scoped DNS Write only if a demonstrated
operation requires explicit record management. A DNS grant generally covers the
zone, not just `feedback`.

No R2 permissions or provisioning now; add narrowly scoped R2 access when needed.
Keep Access/Zero Trust administration an occasional Mac/browser operation. Do not
grant billing, membership, token provisioning, unrelated products/zones, or Tunnel
permissions. Inspect existing Pages/Access through the browser rather than
expanding the deployment token just for inventory.

Current official references, checked 2026-09-29:

- [Workers granular authorization](https://developers.cloudflare.com/workers/authorization/)
- [API permission catalog](https://developers.cloudflare.com/fundamentals/api/reference/permissions/)
- [Account-owned tokens](https://developers.cloudflare.com/fundamentals/api/get-started/account-owned-tokens/)
- [Custom Domains](https://developers.cloudflare.com/workers/configuration/routing/custom-domains/)

## Required live verification still pending

- **Worker:** create a unique `article-lab-verify-<random>` Worker returning exactly
  `Cloudflare deployment verification OK`. Deploy from RTX using the launcher,
  request its Cloudflare URL, verify status/body, then delete only this Worker.
  Store temporary source/config in ignored `tmp/`; do not attach a production route.
- **D1:** create a uniquely named test database, run `SELECT 1`, and delete that
  exact database. Listing is useful but alone does not establish write capability.
- **Zone:** review the issued policy and verify read access to the selected zone,
  DNS, and Worker routes/domains. Record that reads alone do not prove write access.
  If exercising a domain mutation, use only a confirmed unused unique temporary
  hostname and clean it up. Do not create/overwrite `feedback.<domain>` yet.
- **Persistence:** repeat an authenticated command in a fresh noninteractive RTX
  session with `npm run cf -- ...`, without browser login or manually exporting
  the token. Bare `npx wrangler` does not load the custom secret file.
- **Security:** verify file ownership/modes; ensure no token is tracked or appears
  in logs/history, without printing matches. Confirm unchanged DNS/services and
  no HomeLab ingress. Revoke the automation token independently of normal login
  when it is no longer needed.

## Outstanding acceptance items

Mac Git branch/HEAD/status, account/domain/zone, delegation, resource inventory,
actual token scope/storage, Worker deployment, D1 write access, and custom-domain
capability are **unverified**. No password or Global API Key was used; no token
exists to leak from this setup so far. No existing DNS or HomeLab networking was
modified. The acceptance criterion remains unmet until the live checks succeed.
