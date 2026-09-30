# Article review MVP

Agreed product direction, 2026-09-29. **Specification, not implemented behavior.**
The [architecture](architecture.md) describes the wider Article Lab migration.

## Reviewer and owner experience

One persistent website. A reviewer signs in with Google or an emailed code, sees
"Waiting for approval" initially, and appears in the owner's pending-user list.
The owner can approve or reject the account. Approval persists independently of
login sessions; repeated login does not create a new request.

Once approved, the reviewer sees articles made available to them: **Not started**,
**In progress**, or **Submitted**. The owner publishes articles in the portal;
there is no per-article invitation-link workflow. Assignment should be quick,
including selecting several reviewers at once rather than repetitive administration.

An article opens read-only, preserving title, optional subtitle, headings,
paragraphs, lists, links, and supported images. The reviewer selects a passage,
adds a comment, and sees the associated highlight. They can add, edit, or delete
comments while drafting and write **General feedback** at the bottom.

Drafts autosave to the server and survive reload, browser closure, logout, and
return visits. Show Saving / Saved / Unable to save clearly. Never silently discard
a change on a failed request. On **Submit feedback**, persist all acknowledged
changes and make the submitted review read-only, with its submission time visible.
An explicit reopen/edit feature can be added later instead of implicit mutation.

The admin area supports pending users, approve/reject/revoke, article creation
through Markdown paste or import, version publication, reviewer assignment,
review status, and submitted feedback with exact quotes and general comments.
Routine article publishing and approval should not require SQL or terminal access.

## Authentication and authorization

Google and email OTP are choices in the same Access application. See
[setup and platform references](cloudflare-development.md). No Authentik server,
password database, or Cloudflare account for each reviewer is needed for this plan.

The Worker accepts only server-verified Access identity. Keep an immutable internal
user ID and a trusted issuer/subject identity mapping; do not make an editable email
or display name the primary authorization key. Email remains the contact address
shown in approval requests. Never take identity, role, approval, or ownership from
client payloads. Account linking between Google and OTP must use verified identity
information and be tested; do not silently merge accounts based on a supplied email.
The same person must retain their approved account across either login method when
Access establishes the same identity. Document any provider-linking action needed.

Unknown verified identities create one pending reviewer record idempotently.
Pending/rejected/revoked accounts can see their own access status, not article
metadata, bodies, assignments, other users, or feedback. Check current D1 approval,
role, and assignment on protected requests so revocation takes effect even while
an Access cookie remains valid. Reviewers read only their own reviews; the owner
can inspect submitted feedback. Drafts are private to their reviewer in V1.

Bootstrap the first admin with a privileged server-side operation against the
owner's confirmed account, never "first user wins". Ordinary signup and profile
updates cannot change role or approval. An admin control may change those fields
only after server-side authorization. Keep an audit timestamp/actor for decisions.

Remember sessions up to the supported one-month period; do not promise permanent
login. Show a recoverable session-expired state during autosave. Approval and draft
records remain after reauthentication. Logout must end this app's Access session.

## Suggested data model

Table names and implementation details may change; these relationships matter.

| Entity | Responsibility and integrity |
| --- | --- |
| `users` | Stable ID, contact email/name, pending/approved/rejected/revoked status, reviewer/admin role, decision timestamps/actor |
| `user_identities` | Trusted issuer + subject uniquely mapped to a user; not raw OAuth tokens |
| `articles` | Article identity, optional external source ID, archive/publication metadata |
| `article_versions` | Immutable reviewable title/subtitle/body, version number, content hash and timestamp; unique per article/version number |
| `review_assignments` | Reviewer and exact version, unique per pair; disabling access need not delete historical feedback |
| `reviews` | One review per assignment; draft/submitted state, general feedback, timestamps, revision for conflict handling |
| `annotations` | Parent review, selected quote, prefix/suffix, position/structural anchor, comment, timestamps |

"Not started" can be derived from an assignment with no draft, rather than another
stored state. Use foreign keys and uniqueness constraints, and validate association
and ownership in the Worker. Historical reviews never silently follow a newer
article version. Editing a published reviewable version creates another version.

New D1 storage starts clean. A simple external ID supports later import from the
Finance workflow; no wholesale legacy-data migration is required. This is not
permission to erase existing local research data.

## Reliable annotations and saving

Anchor to the immutable version's canonical rendered text with quote + surrounding
context + positions (and structural IDs when helpful). Specify text normalization
and offset units. Repeated passages, nested formatting, Unicode, and selections
across elements must not attach a comment to the wrong text. When resolving an
anchor is ambiguous, show an unanchored quote/comment rather than guessing.

Autosave needs idempotent writes and conflict handling so a slow request or another
browser tab cannot overwrite newer feedback. Submission must flush/acknowledge
pending changes and lock the review atomically; delayed writes cannot change a
submitted review. Preserve recoverable unsaved input across temporary failures.

Aim for comfortable desktop and iPhone use. Test real touch selection where a
device is available; a mobile-sized screenshot is not evidence that selection works.
Allow opening and navigating annotations without relying solely on hover.

## Baseline security and operation

Validate payloads and size limits; use bound D1 queries; render Markdown safely and
block unsafe HTML/URLs. Enforce permissions on API routes, not just hidden buttons.
Use same-origin requests, appropriate CSRF protection for cookie-authenticated
writes, and private/non-cacheable responses for article/user data. No real articles
or reviewer details in build fixtures, public static assets, logs, or Git.

Cover custom domains, `workers.dev`, and previews with authentication or disable
unused endpoints. A development identity shortcut must not authenticate production.
Document a simple export/backup path before collecting irreplaceable feedback.
Avoid adding services or token scopes without an actual requirement.

## Acceptance checks

1. Both Google and email OTP work on the real deployment; a new identity creates one pending request, not admin access.
2. Owner approval opens the account's article inbox without per-article links. Reauthentication keeps approval, assignments, and drafts.
3. Reviewer A cannot read or write B's reviews or unassigned articles through modified requests. Pending/rejected/revoked identities cannot read article data.
4. Selected passages retain comments after reload. General feedback survives leaving and returning. Ambiguous/repeated text and Unicode are handled safely.
5. Network failures and session expiry show unsaved state. Out-of-order saves and two-tab conflicts do not silently lose content.
6. Submit is idempotent, records the right version, and prevents delayed/post-submit edits. Admin can inspect the submitted quotes/comments and general feedback.
7. Version updates leave old reviews attached to old text. Revocation applies despite an existing login session. All deployed URL variants enforce access.
8. Type/lint/build, API/database tests, and desktop/mobile interaction checks are reported separately from live identity-provider/domain checks.

## Scope and implementation freedom

Deliver this useful portal first. Reuse or restructure adjacent legacy material
when it helps; Shiny uptime and backwards compatibility are not release gates.
The schema, libraries, and component layout are implementation choices, not frozen
instructions. Record material deviations from the agreed user experience.

AI interviews, ratings, generation, video, and broader Article Lab migration are
future possibilities, not tasks to complete before reviewers can comment on an
article. This priority is not a prohibition on small improvements that help delivery.
