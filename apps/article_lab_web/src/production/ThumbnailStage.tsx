import { useState } from "react";
import type { Asset, Package } from "../../shared/production";
import { packageReady, packageText } from "../../shared/production";
import { useEditor, useWorkspace } from "./state";
import { EmptyState, GenerationSetup, ItemEditor, JobHistory, LegacyResults, PackageNotes, PackagePreview, StageHeading, StatusBadge } from "./components";
import { request } from "./network";

function ImageDetails({ asset }: { asset: Asset }) {
  const { submit, busy } = useWorkspace();
  const form = useEditor(`image-meta-${asset.id}`, { alt_text: asset.alt_text, caption: asset.caption });
  return <details className="production-details"><summary>Generation details / alt text</summary>
    <label>Image alt text<input value={form.value.alt_text} maxLength={1000} onChange={(e) => form.setValue((v) => ({ ...v, alt_text: e.target.value }))} /></label>
    <label>Caption / credit<input value={form.value.caption} maxLength={2000} onChange={(e) => form.setValue((v) => ({ ...v, caption: e.target.value }))} /></label>
    <button type="button" className="quiet" disabled={busy || !form.dirty} onClick={async () => { const r = await submit("asset_metadata", { asset_id: asset.id, ...form.value }, form.revision); if (r) form.saved(r.revision); }}>Save image metadata</button>
    <dl><dt>Source / model</dt><dd>{asset.source} · {asset.model ?? "User upload"}</dd><dt>Dimensions</dt><dd>{asset.width} × {asset.height}</dd><dt>Created</dt><dd>{new Date(asset.created_at).toLocaleString()}</dd></dl>
    <pre>{asset.prompt ?? "No generation prompt: uploaded image."}</pre>
  </details>;
}
function ImageNotes({ pkg, asset, notes }: { pkg: Package; asset: Asset; notes: string }) {
  const { submit, busy } = useWorkspace();
  const form = useEditor(`image-notes-${pkg.id}-${asset.id}`, { notes });
  return <div className="production-image-notes"><label>Review notes<input value={form.value.notes} maxLength={10000} onChange={(e) => form.setValue({ notes: e.target.value })} placeholder="What works? What needs changing?" /></label>
    {form.dirty && <button type="button" className="quiet" disabled={busy} onClick={async () => { const r = await submit("image", { package_id: pkg.id, asset_id: asset.id, operation: "notes", notes: form.value.notes }, form.revision); if (r) form.saved(r.revision); }}>Save image notes</button>}
  </div>;
}
function ImageGeneration({ pkg }: { pkg: Package }) {
  const { detail, catalog, busy, submit } = useWorkspace();
  const concept = detail.items.find((i) => i.id === pkg.concept_id);
  let saved: { prompt?: string; model?: string; size?: string; quality?: string } = {};
  try { saved = JSON.parse(pkg.image_settings_json || "{}"); } catch { /* Empty legacy preferences. */ }
  const form = useEditor(`image-generation-${pkg.id}`, { prompt: saved.prompt ?? concept?.text ?? "", model: saved.model ?? catalog.images.models[0] ?? "", size: saved.size ?? catalog.images.sizes[0] ?? "1536x1024", quality: saved.quality ?? catalog.images.qualities[0] ?? "medium" });
  const [count, setCount] = useState(1);
  return <details className="production-details"><summary>Generate actual thumbnails · separately billed OpenAI API</summary>
    {!catalog.images_configured && <p className="production-warning">Image generation is unavailable until storage and the image API connection are configured. Text concepts do not generate image pixels.</p>}
    <label>Image prompt<textarea rows={5} value={form.value.prompt} onChange={(e) => form.setValue((v) => ({ ...v, prompt: e.target.value }))} /></label>
    {concept && <button type="button" className="quiet" onClick={() => form.setValue((v) => ({ ...v, prompt: concept.text }))}>Use selected concept as prompt</button>}
    <div className="production-form-grid">
      <label>Image model<select value={form.value.model} onChange={(e) => form.setValue((v) => ({ ...v, model: e.target.value }))}>{catalog.images.models.map((m) => <option key={m}>{m}</option>)}</select></label>
      <label>Size<select value={form.value.size} onChange={(e) => form.setValue((v) => ({ ...v, size: e.target.value }))}>{catalog.images.sizes.map((s) => <option key={s}>{s}</option>)}</select></label>
      <label>Quality<select value={form.value.quality} onChange={(e) => form.setValue((v) => ({ ...v, quality: e.target.value }))}>{catalog.images.qualities.map((q) => <option key={q}>{q}</option>)}</select></label>
      <label>Variants<input type="number" min={1} max={4} value={count} onChange={(e) => setCount(Number(e.target.value))} /></label>
    </div>
    <div className="production-actions"><button type="button" disabled={busy || !catalog.images_configured || !form.value.prompt.trim() || !Number.isInteger(count) || count < 1 || count > 4} onClick={async () => {
      const r = await submit("generate_images", { package_id: pkg.id, job_ids: Array.from({ length: count }, () => crypto.randomUUID()), ...form.value }, form.revision);
      if (r) form.saved(r.revision);
    }}>Generate {count} thumbnail{count === 1 ? "" : "s"} (paid API)</button><button type="button" className="quiet" onClick={form.reload}>Load saved image settings</button></div>
    <p className="production-muted">Each variant is a separate API request. Requests are not automatically retried after an uncertain outcome.</p>
  </details>;
}
export function ThumbnailStage() {
  const { detail, catalog, busy, contentDirty, dirty, submit, refresh, report } = useWorkspace();
  const [focus, setFocus] = useState(""), [archived, setArchived] = useState(false), [editingConcept, setEditingConcept] = useState<string | null>(null), [uploading, setUploading] = useState(false);
  const available = detail.packages.filter((p) => packageReady(detail, p));
  const pkg = available.find((p) => p.id === focus) ?? available[0];
  const concepts = pkg ? detail.items.filter((i) => i.kind === "thumbnail_concepts" && i.parent_id === pkg.id && i.status !== "archived") : [];
  const editing = concepts.find((i) => i.id === editingConcept);
  const links = pkg ? detail.image_links.filter((l) => l.package_id === pkg.id) : [];
  const linkedIds = new Set(links.map((l) => l.image_asset_id));
  const assets = detail.assets.filter((a) => !a.archived_at && links.some((l) => l.image_asset_id === a.id && (archived || !l.archived)));
  const unlinked = detail.assets.filter((a) => !a.archived_at && !linkedIds.has(a.id));
  const upload = async (file: File) => {
    if (!pkg || uploading) return;
    if (!file.size || file.size > 10000000 || !["image/png", "image/jpeg", "image/webp"].includes(file.type)) { report("Upload one PNG, JPEG or WebP image up to 10 MB. The server also validates the bytes."); return; }
    setUploading(true);
    try {
      const form = new FormData(); form.set("file", file); form.set("alt_text", ""); form.set("caption", "");
      const result = await request<{ image_asset_id: string }>(`/workspaces/${detail.article_id}/image-upload`, "POST", form);
      // Upload is independent of the production revision; attach uses a fresh
      // state so a background completion cannot lose the uploaded asset.
      await refresh();
      const linked = await submit("image", { package_id: pkg.id, asset_id: result.image_asset_id, operation: "attach" });
      if (!linked) report("The image uploaded, but attaching it failed. It remains in 'Other images in this article'; attach it there after resolving the error.");
    } catch (e) { report((e as Error).message); } finally { setUploading(false); }
  };
  return <>
    <StageHeading title="Thumbnails" description="Compare complete title/subtitle/image previews, not walls of generation text." next="outline" nextLabel="Outline" />
    {!pkg ? <EmptyState title="No title/subtitle packages ready" next="subtitles" nextLabel="Open Subtitle Generation">Approve a subtitle to create a package for thumbnail production.</EmptyState> : <>
      <label className="production-package-picker">Article package<select value={pkg.id} onChange={(e) => { if (!dirty || window.confirm("Discard unsaved image/package edits?")) { setFocus(e.target.value); setEditingConcept(null); } }}>{available.map((p) => <option key={p.id} value={p.id}>{packageText(detail, p).title} / {packageText(detail, p).subtitle}</option>)}</select></label>
      <PackagePreview pkg={pkg} compact /><PackageNotes key={`notes-${pkg.id}`} pkg={pkg} />
      <details className="production-details"><summary>Creative direction / concept alternatives ({concepts.length})</summary>
      <GenerationSetup key={`concept-settings-${pkg.id}`} kind="thumbnail_concepts" parents={[pkg.id]} label="Generate concept alternatives" />
      <div className="production-concepts">{concepts.map((concept) => <div className={`production-concept${pkg.concept_id === concept.id ? " is-selected" : ""}`} key={concept.id}>
        <h3>{concept.text.split("\n")[0].replace(/^#+\s*/, "").slice(0, 100)}</h3>
        <div className="production-actions"><button type="button" className="quiet" disabled={busy || pkg.concept_id === concept.id} onClick={() => void submit("select_concept", { package_id: pkg.id, item_id: concept.id })}>{pkg.concept_id === concept.id ? "Selected concept" : "Use concept"}</button><button type="button" className="quiet" onClick={() => { if (!dirty || window.confirm("Discard unsaved edits before opening another concept?")) setEditingConcept(concept.id); }}>Edit / details</button><button type="button" className="quiet" disabled={busy || pkg.concept_id === concept.id} onClick={() => void submit("item_status", { ids: [concept.id], status: "archived" })}>Archive</button></div>
        <details><summary>Read concept</summary><p className="production-preserve-lines">{concept.text}</p></details>
      </div>)}</div>
      {editing && <ItemEditor key={editing.id} item={editing} onClose={() => setEditingConcept(null)} />}
      <LegacyResults kind="thumbnail_concepts" parent={pkg.id} /></details>
      <h2>Thumbnail candidates</h2>
      <ImageGeneration key={`image-${pkg.id}`} pkg={pkg} />
      <div className="production-upload" onDragOver={(e) => e.preventDefault()} onDrop={(e) => { e.preventDefault(); if (!busy && !uploading && catalog.image_storage_configured && e.dataTransfer.files[0]) void upload(e.dataTransfer.files[0]); }}>
        <label>Upload a thumbnail or drop it here<input type="file" accept="image/png,image/jpeg,image/webp" disabled={busy || uploading || !catalog.image_storage_configured} onChange={(e) => { const file = e.currentTarget.files?.[0]; e.currentTarget.value = ""; if (file) void upload(file); }} /></label>
        {!catalog.image_storage_configured && <p className="production-warning">Image storage is not configured. Uploads and generated thumbnails require the private R2 bucket.</p>}
      </div>
      <label className="production-inline-check"><input type="checkbox" checked={archived} onChange={(e) => { if (!dirty || window.confirm("Discard unsaved image metadata?")) setArchived(e.target.checked); }} />Show archived thumbnails</label>
      <div className="production-thumbnail-grid">{assets.map((asset) => {
        const link = links.find((l) => l.image_asset_id === asset.id)!; const approved = pkg.image_asset_id === asset.id, words = packageText(detail, pkg);
        return <article className={`production-thumbnail-card${approved ? " is-approved" : ""}${link.archived ? " is-archived" : ""}`} key={asset.id}>
          <div className="production-thumbnail-card-top"><StatusBadge value={approved ? "approved" : link.archived ? "archived" : "candidate"} /><small>{asset.width} × {asset.height}</small></div>
          <div className="production-preview-card"><div><h3>{words.title}</h3><p>{words.subtitle}</p></div><img src={`/api/assets/${asset.id}`} alt={asset.alt_text || "Thumbnail candidate"} loading="lazy" /></div>
          <div className="production-actions"><button type="button" disabled={busy || contentDirty || approved || Boolean(link.archived)} onClick={() => void submit("image", { package_id: pkg.id, asset_id: asset.id, operation: "approve" })}>{approved ? "Approved for outline" : "Approve thumbnail"}</button>
            <button type="button" className="quiet" disabled={busy || approved} onClick={() => void submit("image", { package_id: pkg.id, asset_id: asset.id, operation: link.archived ? "restore" : "archive" })}>{link.archived ? "Restore" : "Archive"}</button></div>
          <ImageNotes pkg={pkg} asset={asset} notes={link.notes} /><ImageDetails asset={asset} />
        </article>;
      })}</div>
      {!assets.length && <p className="production-muted">No thumbnails for this package yet. Generate images, upload one, or attach an existing article image below.</p>}
      {unlinked.length > 0 && <details className="production-details"><summary>Other images in this article ({unlinked.length})</summary><div className="production-thumbnail-grid">{unlinked.map((asset) => <figure key={asset.id}><img className="production-library-image" src={`/api/assets/${asset.id}`} alt={asset.alt_text || "Existing article image"} /><figcaption><button type="button" disabled={busy} onClick={() => void submit("image", { package_id: pkg.id, asset_id: asset.id, operation: "attach" })}>Attach to this package</button></figcaption></figure>)}</div></details>}
      <JobHistory kind="thumbnail_concepts" parent={pkg.id} /><JobHistory kind="image" parent={pkg.id} />
    </>}
  </>;
}
