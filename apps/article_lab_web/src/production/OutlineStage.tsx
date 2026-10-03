import { useState } from "react";
import { packageReady, packageText } from "../../shared/production";
import { useEditor, useWorkspace } from "./state";
import { EmptyState, GenerationSetup, ItemEditor, JobHistory, LegacyResults, PackageNotes, PackagePreview, StageHeading, StatusBadge } from "./components";
export function OutlineStage() {
  const { detail, submit, busy, contentDirty, dirty } = useWorkspace();
  const [focus, setFocus] = useState(""), [outlineId, setOutlineId] = useState(""), [archived, setArchived] = useState(false);
  const packages = detail.packages.filter((p) => packageReady(detail, p) && p.image_asset_id && detail.assets.some((a) => a.id === p.image_asset_id && !a.archived_at));
  const pkg = packages.find((p) => p.id === focus) ?? packages[0];
  const outlines = pkg ? detail.items.filter((i) => i.kind === "outline" && i.parent_id === pkg.id && (archived || i.status !== "archived")) : [];
  const outline = outlines.find((i) => i.id === outlineId) ?? outlines.find((i) => i.status === "approved") ?? outlines[outlines.length - 1];
  const manual = useEditor("manual-outline", { text: "" });
  return <>
    <StageHeading title="Outline" description="Keep the approved package in view while you edit and approve the article structure." next="drafts" nextLabel="Full Text" />
    {!pkg ? <EmptyState title="No packages ready for an outline" next="thumbnails" nextLabel="Open Thumbnails">Approve a thumbnail for a title/subtitle package first.</EmptyState> : <>
      <label className="production-package-picker">Approved package<select value={pkg.id} onChange={(e) => { if (!dirty || window.confirm("Discard unsaved outline/package edits?")) { setFocus(e.target.value); setOutlineId(""); manual.setValue({ text: "" }); } }}>{packages.map((p) => <option key={p.id} value={p.id}>{packageText(detail, p).title} / {packageText(detail, p).subtitle}</option>)}</select></label>
      <PackagePreview pkg={pkg} /><PackageNotes key={`notes-${pkg.id}`} pkg={pkg} />
      <GenerationSetup kind="outline" parents={[pkg.id]} label={outlines.length ? "Generate another outline" : "Generate outline"} />
      <div className="production-filter-bar"><label>Outline alternative<select value={outline?.id ?? ""} onChange={(e) => { if (!dirty || window.confirm("Discard unsaved outline edits?")) setOutlineId(e.target.value); }}><option value="">Choose an outline</option>{outlines.map((o, i) => <option key={o.id} value={o.id}>{i + 1}. {o.status} · {new Date(o.created_at).toLocaleString()}</option>)}</select></label><label className="production-inline-check"><input type="checkbox" checked={archived} onChange={(e) => { if (!dirty || window.confirm("Discard unsaved outline edits?")) setArchived(e.target.checked); }} />Show archived</label></div>
      {outline && <>
        <ItemEditor key={outline.id} item={outline} outline />
        <div className="production-actions"><button type="button" disabled={busy || contentDirty || outline.status !== "candidate"} onClick={() => void submit("item_status", { ids: [outline.id], status: "approved" })}>Approve for Full Text</button>
          <button type="button" className="quiet" disabled={busy || contentDirty} onClick={() => void submit("item_status", { ids: [outline.id], status: outline.status === "archived" ? "candidate" : "archived" })}>{outline.status === "archived" ? "Restore outline" : "Archive outline"}</button><StatusBadge value={outline.status} /></div>
      </>}
      <details className="production-details"><summary>Add an outline manually</summary><label>Outline Markdown<textarea rows={8} value={manual.value.text} onChange={(e) => manual.setValue({ text: e.target.value })} /></label><button type="button" disabled={busy || !manual.value.text.trim()} onClick={async () => { const r = await submit("add_items", { kind: "outline", parent_id: pkg.id, items: [{ id: crypto.randomUUID(), text: manual.value.text }] }); if (r) manual.setValue({ text: "" }); }}>Add outline alternative</button></details>
      <LegacyResults kind="outline" parent={pkg.id} /><JobHistory kind="outline" parent={pkg.id} />
    </>}
  </>;
}
