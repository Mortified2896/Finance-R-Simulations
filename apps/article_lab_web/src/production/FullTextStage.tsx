import { useState } from "react";
import type { Draft, Item } from "../../shared/production";
import { draftSnapshot, packageReady, packageText, writingContext } from "../../shared/production";
import { MarkdownComposer } from "../editor/MarkdownComposer";
import { useEditor, useWorkspace } from "./state";
import { EmptyState, PackagePreview, StageHeading, StatusBadge } from "./components";
import { downloadText, request } from "./network";
const ignoreDirty = (_: boolean) => {};

function DraftEditor({ draft }: { draft: Draft }) {
  const { detail, busy, submit, report } = useWorkspace();
  const form = useEditor(`draft-${draft.id}`, { body: draft.body, notes: draft.notes, acknowledge: false });
  const [valid, setValid] = useState(true), [generation, setGeneration] = useState(0);
  const [revisions, setRevisions] = useState<{ id: string; before_body: string; after_body: string; created_at: string }[]>([]);
  const outline = detail.items.find((i) => i.id === draft.outline_id)!;
  const stale = draft.input_snapshot !== draftSnapshot(detail, outline);
  return <section className="production-draft-editor">
    <div className="production-actions"><h2>Working draft variant</h2><StatusBadge value={draft.status} /><span className="production-muted">Imported {new Date(draft.created_at).toLocaleString()}</span></div>
    {stale && <p className="production-warning">The approved package or outline has changed since this draft was imported. Check it before approving.</p>}
    <MarkdownComposer key={`${draft.id}-${generation}`} disabled={busy} initialMarkdown={form.value.body} heading="Full article draft" onMarkdownChange={(body) => form.setValue((v) => ({ ...v, body }))} onValidityChange={setValid} onDirtyChange={ignoreDirty} />
    <label>Draft notes<input value={form.value.notes} maxLength={10000} onChange={(e) => form.setValue((v) => ({ ...v, notes: e.target.value }))} /></label>
    {stale && <label className="production-inline-check"><input type="checkbox" checked={form.value.acknowledge} onChange={(e) => form.setValue((v) => ({ ...v, acknowledge: e.target.checked }))} />I checked this draft against the current package and outline.</label>}
    <div className="production-actions">
      <button type="button" disabled={busy || !form.dirty || !valid} onClick={async () => { const r = await submit("edit_draft", { draft_id: draft.id, body: form.value.body, notes: form.value.notes, acknowledge_context: form.value.acknowledge }, form.revision); if (r) form.saved(r.revision); }}>Save draft edits</button>
      <button type="button" disabled={busy || form.dirty || !valid || stale || draft.status === "approved"} onClick={() => void submit("draft_status", { draft_id: draft.id, status: "approved" })}>Approve for Review & Publish</button>
      <button type="button" className="quiet" disabled={busy || form.dirty} onClick={() => void submit("draft_status", { draft_id: draft.id, status: draft.status === "rejected" ? "draft" : "rejected" })}>{draft.status === "rejected" ? "Restore draft" : "Reject variant"}</button>
      <button type="button" className="quiet" onClick={() => downloadText("unsaved-article-draft.md", form.value.body)}>Export current text</button>
    </div>
    <p className="production-muted">{form.dirty ? "Unsaved edits; only Save draft edits records a revision." : "Draft saved."} Approving another variant does not delete this one.</p>
    <details className="production-details"><summary>Original imported draft</summary><pre>{draft.original_body}</pre><button type="button" className="quiet" onClick={() => { if (window.confirm("Load the original into this editor? Save afterwards to record it as a new revision.")) { form.setValue((v) => ({ ...v, body: draft.original_body })); setGeneration((v) => v + 1); } }}>Load original into editor</button></details>
    <details className="production-details" onToggle={(e) => { if (e.currentTarget.open) void request<typeof revisions>(`/production/${detail.article_id}/revisions?draft_id=${draft.id}`).then(setRevisions).catch((e) => report((e as Error).message)); }}>
      <summary>Saved revision history</summary>{!revisions.length ? <p>No manual revisions saved yet.</p> : revisions.map((r) => <details key={r.id}><summary>{new Date(r.created_at).toLocaleString()}</summary><pre>{r.after_body}</pre><button type="button" className="quiet" onClick={() => { if (window.confirm("Load this revision into the editor without overwriting history?")) { form.setValue((v) => ({ ...v, body: r.after_body })); setGeneration((v) => v + 1); } }}>Load revision into editor</button></details>)}
    </details>
  </section>;
}
function NewDraft({ outline, onCreated }: { outline: Item; onCreated: (id: string) => void }) {
  const { detail, submit, busy } = useWorkspace();
  const form = useEditor(`draft-new-${outline.id}`, { body: "", notes: "" });
  const [valid, setValid] = useState(true), [generation, setGeneration] = useState(0);
  return <section className="production-draft-editor">
    <h2>Import a new draft variant</h2><p className="production-muted">Paste or import the article written in ChatGPT Pro. It becomes a separate variant; nothing is overwritten.</p>
    {detail.workspace.draft_body && <button type="button" className="quiet" onClick={() => { if (!form.value.body || window.confirm("Replace this unsaved import with the legacy workspace draft?")) { form.setValue((v) => ({ ...v, body: detail.workspace.draft_body })); setGeneration((v) => v + 1); } }}>Load existing workspace draft</button>}
    <MarkdownComposer key={`new-${outline.id}-${generation}`} disabled={busy} initialMarkdown={form.value.body} heading="New article draft" onMarkdownChange={(body) => form.setValue((v) => ({ ...v, body }))} onValidityChange={setValid} onDirtyChange={ignoreDirty} />
    <label>Variant notes<input value={form.value.notes} onChange={(e) => form.setValue((v) => ({ ...v, notes: e.target.value }))} /></label>
    <button type="button" disabled={busy || !valid || !form.value.body.trim()} onClick={async () => {
      const key = crypto.randomUUID(); const r = await submit("new_draft", { draft_id: key, outline_id: outline.id, body: form.value.body, notes: form.value.notes }, form.revision);
      if (r) { form.saved(r.revision); onCreated(key); }
    }}>Save as new draft variant</button>
  </section>;
}
export function FullTextStage() {
  const { detail, contentDirty, report } = useWorkspace();
  const [outlineId, setOutlineId] = useState(""), [draftId, setDraftId] = useState(""), [adding, setAdding] = useState(false), [rejected, setRejected] = useState(false);
  const outlines = detail.items.filter((i) => i.kind === "outline" && i.status === "approved" && detail.packages.some((p) => p.id === i.parent_id && packageReady(detail, p) && p.image_asset_id));
  const outline = outlines.find((o) => o.id === outlineId) ?? outlines[0];
  const pkg = detail.packages.find((p) => p.id === outline?.parent_id);
  const drafts = outline ? detail.drafts.filter((d) => d.package_id === pkg?.id && (rejected || d.status !== "rejected")) : [];
  const draft = drafts.find((d) => d.id === draftId) ?? drafts.find((d) => d.status === "approved") ?? drafts[0];
  const mayLeave = () => !contentDirty || window.confirm("Discard unsaved draft edits? Cancel to save or export first.");
  return <>
    <StageHeading title="Full Text" description="Draft in ChatGPT Pro, then import, edit and approve a variant here. Every original and saved revision remains available." next="publish" nextLabel="Review & Publish" />
    {!outline || !pkg ? <EmptyState title="No approved outline ready for drafting" next="outline" nextLabel="Open Outline">Approve an outline to carry its title, subtitle and thumbnail into Full Text.</EmptyState> : <>
      <label className="production-package-picker">Approved outline / package<select value={outline.id} onChange={(e) => { if (mayLeave()) { setOutlineId(e.target.value); setDraftId(""); setAdding(false); } }}>{outlines.map((o) => <option key={o.id} value={o.id}>{packageText(detail, detail.packages.find((p) => p.id === o.parent_id)!).title} · outline {new Date(o.created_at).toLocaleString()}</option>)}</select></label>
      <PackagePreview pkg={pkg} compact />
      <div className="production-actions"><StatusBadge value={detail.workspace.evidence.trim() ? "evidence_notes" : "no_evidence_notes"} /><button type="button" onClick={async () => { try { await navigator.clipboard.writeText(writingContext(detail, outline)); } catch (e) { report(`Clipboard unavailable: ${(e as Error).message}. Use Download writing context.`); } }}>Copy context for ChatGPT Pro</button><button type="button" className="quiet" onClick={() => downloadText("article-writing-context.md", writingContext(detail, outline))}>Download writing context</button></div>
      <details className="production-details"><summary>Approved outline and exact writing context</summary><pre>{writingContext(detail, outline)}</pre></details>
      <div className="production-filter-bar"><label>Draft variant<select value={adding ? "new" : draft?.id ?? "new"} onChange={(e) => { if (mayLeave()) { setAdding(e.target.value === "new"); setDraftId(e.target.value); } }}><option value="new">Add a new draft variant</option>{drafts.map((d, i) => <option key={d.id} value={d.id}>{i + 1}. {d.status} · {new Date(d.created_at).toLocaleString()}</option>)}</select></label><label className="production-inline-check"><input type="checkbox" checked={rejected} onChange={(e) => { if (mayLeave()) setRejected(e.target.checked); }} />Show rejected variants</label><button type="button" className="quiet" onClick={() => { if (mayLeave()) setAdding(true); }}>Add another variant</button></div>
      {adding || !draft ? <NewDraft key={`new-${outline.id}`} outline={outline} onCreated={(key) => { setDraftId(key); setAdding(false); }} /> : <DraftEditor key={draft.id} draft={draft} />}
    </>}
  </>;
}
