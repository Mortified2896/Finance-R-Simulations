import { useMemo, useState } from "react";
import type { Draft, PublishSettings } from "../../shared/production";
import { EMPTY_PUBLISH, mediumMarkdown, packageText } from "../../shared/production";
import { renderMarkdown } from "../../shared/markdown";
import { useEditor, useWorkspace } from "./state";
import { EmptyState, PackagePreview, StageHeading, StatusBadge } from "./components";
import { downloadText } from "./network";
function ReviewPackage({ draft }: { draft: Draft }) {
  const { detail, submit, busy, report } = useWorkspace();
  const pkg = detail.packages.find((p) => p.id === draft.package_id)!;
  const words = packageText(detail, pkg), asset = detail.assets.find((a) => a.id === pkg.image_asset_id);
  const saved = detail.publishing.find((p) => p.draft_id === draft.id)?.settings ?? { ...EMPTY_PUBLISH, alt_text: asset?.alt_text ?? "", image_credit: asset?.caption ?? "" };
  const form = useEditor(`publish-${draft.id}`, { ...saved, tags_text: saved.tags.join(", ") });
  const [read, setRead] = useState(false);
  const html = useMemo(() => read ? renderMarkdown(draft.body).rendered_html : "", [read, draft.body]);
  const set = <K extends keyof PublishSettings>(key: K, value: PublishSettings[K]) => form.setValue((old) => ({ ...old, [key]: value }));
  // Private reviewer URLs cannot work on Medium. Mark placements explicitly;
  // provide separate authenticated downloads rather than exporting broken URLs.
  const exportBody = draft.body.replace(/!\[([^\]]*)\]\(\/api\/assets\/[0-9a-f-]{36}\)/gi, (_m, alt: string) => `[Image placement: ${alt || "article image"}. Upload the downloaded image in Medium.]`);
  const metadata: PublishSettings = { ...form.value, tags: form.value.tags_text.split(/[,\n]/).map((tag) => tag.trim()).filter(Boolean) };
  const text = mediumMarkdown(words.title, words.subtitle, exportBody, metadata);
  return <>
    <div className="production-publish-grid">
      <section className="production-publish-preview"><PackagePreview pkg={pkg} /><StatusBadge value="approved_draft" />
        <details className="production-details" onToggle={(e) => setRead(e.currentTarget.open)}><summary>Read-only approved article preview</summary>{read && <div className="production-article-body" dangerouslySetInnerHTML={{ __html: html }} />}</details>
        {asset && <a className="production-download" href={`/api/assets/${asset.id}`} download={`thumbnail-${asset.id}`}>Download featured image</a>}
        <p className="production-muted">Image URLs are private. Upload the downloaded files to Medium separately; copying article text does not publish or expose them.</p>
      </section>
      <section className="production-publishing-form"><h2>Publishing metadata</h2>
        <label>Medium tags (max 5)<input value={form.value.tags_text} onChange={(e) => form.setValue((old) => ({ ...old, tags_text: e.target.value }))} placeholder="Investing, Personal Finance" /></label>
        <div className="production-form-grid"><label>Publishing target<select value={form.value.target} onChange={(e) => set("target", e.target.value as PublishSettings["target"])}><option value="undecided">Do not publish yet</option><option value="medium_profile">Own Medium profile</option><option value="medium_publication">Medium publication</option><option value="website">Own website</option></select></label>
          <label>Monetization<select value={form.value.monetization} onChange={(e) => set("monetization", e.target.value as PublishSettings["monetization"])}><option value="undecided">Undecided</option><option value="free">Free article</option><option value="paid">Paywalled article</option></select></label></div>
        {form.value.target === "medium_publication" && <label>Publication<input value={form.value.publication} list="production-publications" onChange={(e) => set("publication", e.target.value)} /><datalist id="production-publications">{[...new Set(detail.publishing.map((p) => p.settings.publication).filter(Boolean))].map((name) => <option key={name}>{name}</option>)}</datalist></label>}
        <label>Publish status<select value={form.value.status} onChange={(e) => set("status", e.target.value as PublishSettings["status"])}><option value="ready_for_review">Ready for review</option><option value="ready_to_publish">Ready to publish</option><option value="submitted">Submitted</option><option value="published">Published</option><option value="needs_changes">Needs changes</option><option value="archived">Archived</option></select></label>
        <label>Canonical URL<input type="url" value={form.value.canonical_url} onChange={(e) => set("canonical_url", e.target.value)} placeholder="https://…" /></label>
        <label>Published URL<input type="url" value={form.value.published_url} onChange={(e) => set("published_url", e.target.value)} placeholder="https://medium.com/…" /></label>
        <label>Featured-image alt text<input value={form.value.alt_text} maxLength={1000} onChange={(e) => set("alt_text", e.target.value)} /></label>
        <label>Image credit / source<input value={form.value.image_credit} maxLength={2000} onChange={(e) => set("image_credit", e.target.value)} /></label>
        <label>Publishing notes<textarea rows={3} value={form.value.notes} onChange={(e) => set("notes", e.target.value)} /></label>
        <div className="production-actions"><button type="button" disabled={busy || !form.dirty} onClick={async () => { const r = await submit("save_publishing", { draft_id: draft.id, settings: metadata }, form.revision); if (r) form.saved(r.revision); }}>Save publish settings</button><button type="button" className="quiet" onClick={form.reload}>Load saved settings</button></div>
      </section>
    </div>
    <section className="production-publish-actions"><h2>Review and export</h2><div className="production-actions">
      <button type="button" disabled={busy || form.dirty} onClick={() => void submit("publish_snapshot", { draft_id: draft.id, version_id: crypto.randomUUID() })}>Create immutable reviewer version</button>
      <button type="button" className="quiet" onClick={async () => { try { await navigator.clipboard.writeText(text); } catch (e) { report(`Copy failed: ${(e as Error).message}. Export the Markdown file instead.`); } }}>Copy Medium-ready article text</button>
      <button type="button" className="quiet" onClick={() => downloadText("medium-article.md", text)}>Export Markdown</button>
    </div><p className="production-muted">Creating a reviewer version freezes the article and its images in this app. It does not publish to Medium. Assign the new version to reviewers in Admin.</p>
    <details className="production-details"><summary>Export preview</summary><pre>{text}</pre></details>
    <h3>Reviewer version history</h3>{detail.snapshots.filter((s) => s.draft_id === draft.id).length ? <ul>{detail.snapshots.filter((s) => s.draft_id === draft.id).map((s) => <li key={s.version_id}>Version {s.version_number} · {new Date(s.created_at).toLocaleString()} · <code>{s.version_id}</code></li>)}</ul> : <p>No reviewer versions created from this variant yet.</p>}
    </section>
  </>;
}
export function ReviewPublishStage() {
  const { detail, contentDirty } = useWorkspace();
  const [focus, setFocus] = useState(""), [archived, setArchived] = useState(false);
  const rows = detail.drafts.filter((d) => d.status === "approved" && (archived || detail.publishing.find((p) => p.draft_id === d.id)?.settings.status !== "archived"));
  const draft = rows.find((d) => d.id === focus) ?? rows[0];
  return <><StageHeading title="Review & Publish" description="Inspect the approved package, set Medium metadata, export the article and prepare private reviewer versions." />
    <label className="production-inline-check"><input type="checkbox" checked={archived} onChange={(e) => { if (!contentDirty || window.confirm("Discard unsaved publishing metadata?")) setArchived(e.target.checked); }} />Show archived publishing entries</label>
    {!draft ? <EmptyState title="No approved draft ready for publishing" next="drafts" nextLabel="Open Full Text">Approve one draft variant. Rejected and unapproved variants remain in Full Text.</EmptyState> : <>
      <label className="production-package-picker">Approved draft<select value={draft.id} onChange={(e) => { if (!contentDirty || window.confirm("Discard unsaved publishing metadata?")) setFocus(e.target.value); }}>{rows.map((d) => <option key={d.id} value={d.id}>{packageText(detail, detail.packages.find((p) => p.id === d.package_id)!).title} · {new Date(d.created_at).toLocaleString()}</option>)}</select></label>
      <ReviewPackage key={draft.id} draft={draft} />
    </>}
  </>;
}
