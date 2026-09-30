# Google OAuth setup

Authentication is **Better Auth 1.7.6 inside the Cloudflare Worker**, using native
D1 storage. There is no Cloudflare Access/Zero Trust requirement, separate auth
server, email OTP, email service, password login, or paid authentication seat plan.

Production origin: `https://feedback.moneymattersmedia.com`

Production callback: `https://feedback.moneymattersmedia.com/api/auth/callback/google`

## Google Auth Platform

Use the Google account that will own the OAuth project at
https://console.cloud.google.com/auth/overview.

1. Select/create an Article Lab project. Configure branding (Article Lab), support
   email and developer contact using the owner's selected account.
2. Use an **External** audience if reviewers are outside the owner's Workspace.
   Only the normal identity scopes are needed: `openid`, `email`, `profile`.
3. Create a **Web application** OAuth client. Authorized JavaScript origin is the
   production origin above. Authorized redirect URI is the exact callback above.
4. Check Audience and Data Access before inviting reviewers. Google's current
   [Testing policy](https://support.google.com/cloud/answer/15549945) exempts
   Sign in with Google using only `openid`, email and profile from the test-user
   list and seven-day authorization expiry. If additional scopes are requested,
   Testing normally requires listed test users (up to 100). Workspace and
   Advanced Protection restrictions can still block an account. Use the Google
   publishing flow when ready for the broader intended audience.
5. Store `GOOGLE_CLIENT_ID` and `GOOGLE_CLIENT_SECRET` as secrets for Worker
   `article-lab-review`. The client secret belongs only in Google and Worker
   secret storage, never in React, GitHub, screenshots or chat.

The remote Codex session can open the Mac's in-app browser, but currently has no
click/fill/read browser-control tool. Google/Bitwarden login and OAuth client
creation are therefore not claimed as completed. No credentials are scraped or
transferred from the browser. If controlled browser tools become available,
continue through the official dashboard; otherwise owner interaction is needed.

## Worker secrets and bootstrap

`BETTER_AUTH_SECRET` has already been generated securely and installed through
the existing credential launcher. Do not rotate it during normal deployment.
`BETTER_AUTH_URL` is public configuration in `wrangler.jsonc`.

Set the two Google secrets and `BOOTSTRAP_ADMIN_EMAIL` through Cloudflare's
Worker Settings → Variables and Secrets, or the secure launcher:

```sh
npm run cf -- secret put GOOGLE_CLIENT_ID
npm run cf -- secret put GOOGLE_CLIENT_SECRET
npm run cf -- secret put BOOTSTRAP_ADMIN_EMAIL
```

Use the launcher's input prompts privately; never put secret values in command
arguments. Select the bootstrap Google email explicitly before its first login.
Only that verified identity is initially approved/admin. Other new users are
pending. Remove the bootstrap secret after initial owner login; D1 retains the
role. Login never resets approval, rejection or disability.

If the owner already exists as pending, an operator can explicitly promote that
confirmed account through D1 using the secure launcher. There is no public role-
elevation endpoint. Review system foreign keys remain internal application UUIDs.

## Acceptance still requiring the real Google client

Sign in as a new Google reviewer; verify pending. Sign in as the configured
owner, approve and assign an article. Review/comment/save/return/submit. Log out
and sign back in as the reviewer; confirm approval and the same application ID.
Inspect submitted feedback as owner. Confirm a rejected/disabled reviewer cannot
fetch article data and a reviewer cannot access admin or another user's review.

Local tests exercise Better Auth sessions, OAuth initiation/state rejection,
logout/expiry, D1 authorization, and the review UI. They do not replace real
Google consent/callback acceptance on production.

## Abandoned Access setup

No organization, Access application, policy or identity provider was created by
the agent. The last observed Zero Trust browser flow was the incomplete payment
page; no payment or subscription was submitted by the agent. The existing token
returns HTTP 403 for Access organization/application inventory, so owner-created
state cannot be independently certified absent. No unrelated Cloudflare objects
were deleted or modified. The empty legacy Access identity table was removed by
migration 0002 after a remote count confirmed zero rows. No Access secrets existed.

Official sources checked during implementation:
[Google provider](https://better-auth.com/docs/authentication/google),
[D1/schema](https://better-auth.com/docs/concepts/database),
[sessions](https://better-auth.com/docs/concepts/session-management),
[cookies](https://better-auth.com/docs/concepts/cookies).
