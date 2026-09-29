# One-time owner handoff: Google + email sign-in

The Worker and empty production D1 are deployed. The custom hostname is active:
**https://feedback.moneymattersmedia.com**. The deployment token deliberately
cannot administer Access; do these steps in your Mac browser with your existing
Cloudflare/Google sessions. Do not change the RTX token or send any secrets to chat.

## 1. Configure both identity providers

Open Cloudflare **Zero Trust → Integrations → Identity providers**. If Zero Trust
has never been enabled, complete its initial team-domain/account setup first.

- Add **One-time PIN** for email login.
- For **Google**, create a Google Cloud project and OAuth consent configuration
  (External audience for reviewers outside your organization), then a **Web
  application** OAuth client. Use JavaScript origin
  `https://YOUR-TEAM.cloudflareaccess.com` and redirect URI
  `https://YOUR-TEAM.cloudflareaccess.com/cdn-cgi/access/callback`.
  Enter the client ID and secret in Cloudflare's Google identity-provider
  configuration only. Test the provider. If the Google app remains in Testing,
  add invited reviewers as test users or publish its consent configuration as
  appropriate for your audience.

Official guides: [Google](https://developers.cloudflare.com/cloudflare-one/integrations/identity-providers/google/)
and [One-time PIN](https://developers.cloudflare.com/cloudflare-one/integrations/identity-providers/one-time-pin/).

## 2. Protect the entire Worker

In **Workers & Pages → article-lab-review → Access**, select **Protect this Worker
behind Access → All traffic**. This covers custom domains and workers.dev;
preview URLs are disabled in Wrangler. Edit its self-hosted Access application
under **Zero Trust → Access controls → Applications**:

- Enable both Google and One-time PIN login methods. Keep the identity-provider
  chooser visible (do not enable an automatic redirect to Google).
- Add an **Allow** policy with **Include → Everyone** so authenticated new
  identities can reach pending registration. Do not use **Bypass**. The Worker
  independently verifies the JWT and denies all article access until D1 approval.
- Set application session duration to **1 month** (approximately 30 days), and
  policy duration to **Same as application**. Review global session duration;
  it can also be one month, but changing it affects other Access applications.
- Copy the application **AUD** tag and your exact HTTPS Access team domain.

If the account UI offers only hostname-based applications, create **one**
self-hosted application containing both the feedback hostname and this Worker's
workers.dev hostname, and use its AUD. Never leave an alternate entry point
unprotected. [Worker protection and Static Assets limitation](https://developers.cloudflare.com/workers/configuration/cloudflare-access/);
[session settings](https://developers.cloudflare.com/cloudflare-one/access-controls/access-settings/session-management/).

## 3. Configure the Worker and explicit owner

In **Workers & Pages → article-lab-review → Settings → Variables and Secrets**,
add these as **Secrets**, so Wrangler deployments retain the dashboard values:

| Name | Value |
| --- | --- |
| `ACCESS_TEAM_DOMAIN` | `https://YOUR-TEAM.cloudflareaccess.com` (no trailing slash) |
| `ACCESS_AUD` | The exact application AUD tag from step 2 |
| `BOOTSTRAP_ADMIN_EMAIL` | Your explicitly chosen Google/OTP verified owner email |

Deploy the settings. Set the bootstrap email **before the owner's first login**.
Only that verified email is created as approved/admin; everyone else is pending.
No commit email or first-visitor heuristic is used. Remove the bootstrap secret
after first owner login; the admin role stays in D1.

If the owner already signed in and became pending before setting the bootstrap
secret, setting it later deliberately does not override existing account status.
An operator with the existing D1 credential can explicitly promote the known
account using the secure launcher (replace the example email with the confirmed
owner; never infer it):

```sh
npm run cf -- d1 execute article-lab-review --remote --command "UPDATE users SET role='admin', status='approved', approved_at=COALESCE(approved_at, strftime('%Y-%m-%dT%H:%M:%fZ','now')) WHERE email='CONFIRMED_OWNER_EMAIL';"
```

Use a correctly SQL-escaped email if it contains an apostrophe. Verify the
operator-selected account in the UI. This privileged operation is never exposed
as a public endpoint. [Worker JWT validation](https://developers.cloudflare.com/cloudflare-one/access-controls/applications/http-apps/authorization-cookie/validating-json/).

## 4. First real login acceptance

1. Owner signs in, opens **Admin**, publishes a small article.
2. In a separate browser/profile, a new reviewer chooses Google; verify pending.
3. Owner approves them and assigns the article version.
4. Reviewer checks status, opens article, selects text, adds/edits/deletes a
   comment, leaves general feedback, waits for **All changes saved**, and closes.
5. Return using email OTP for the **same verified email**. Confirm the same
   approved account and restored draft without another approval.
6. Submit, confirm read-only, and inspect feedback from Admin.
7. Disable the reviewer and verify article/API access is denied after sign-in.

Real Google/OTP login cannot be claimed as tested until these dashboard steps
and account-backed checks are completed. Cloudflare sessions and D1 approval
are independent. No new article-specific invitation link is required.
