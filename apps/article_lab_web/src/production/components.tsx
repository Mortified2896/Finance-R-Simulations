import { useMemo, useState } from "react";
import type { ReactNode } from "react";
import type { Item, ItemKind, Package, Stage } from "../../shared/production";
import { DEFAULT_PROMPTS, LIMITS, inputSnapshot, packageText, resolvedPrompt } from "../../shared/production";
import { providerPrompt } from "../../shared/generation";
import { useEditor, useWorkspace } from "./state";
import { request } from "./network";

export function StatusBadge({ value }: { value: string }) {
  return <span className={`production-badge production-badge-${value}`}>{value.replaceAll("_", " ")}</span>;
}
export function EmptyState({ title, children, next, nextLabel }: { title: string; children: ReactNode; next?: Stage; nextLabel?: string }) {
  const { setStage } = useWorkspace();
  return <div className="production-empty"><h3>{title}</h3><p>{children}</p>{next && <button type="button" onClick={() => setStage(next)}>{nextLabel ?? "Go to previous stage"}</button>}</div>;
}
export function StageHeading({ title, description, next, nextLabel }: { title: string; description: string; next?: Stage; nextLabel?: string }) {
  const { setStage } = useWorkspace();
  return <div className="production-stage-heading"><div><h1>{title}</h1><p>{description}</p></div>{next && <button className="quiet" type="button" onClick={() => setStage(next)}>{nextLabel} →</button>}</div>;
}
export function PackagePreview({ pkg, compact = false }: { pkg: Package; compact?: boolean }) {
  const { detail } = useWorkspace();
  const words = packageText(detail, pkg), image = detail.assets.find((a) => a.id === pkg.image_asset_id && !a.archived_at);
  return <div className={`production-package-preview${compact ? " compact" : ""}`}>
    <div><span className="production-muted">Article package</span><h3>{words.title}</h3><p>{words.subtitle}</p></div>
    {image ? <img src={`/api/assets/${image.id}`} alt={image.alt_text || "Selected article thumbnail"} loading="lazy" /> : <div className="production-image-empty">No approved thumbnail</div>}
  </div>;
}
export function CandidateTable({ rows, selected, onSelect, titleColumn = false, onEdit }: {
  rows: Item[]; selected: Set<string>; onSelect: (next: Set<string>) => void; titleColumn?: boolean; onEdit: (id: string) => void;
}) {
  const { detail, busy } = useWorkspace();
  const all = rows.length > 0 && rows.every((row) => selected.has(row.id));
  const titleMap = useMemo(() => new Map(detail.items.map((r) => [r.id, r.text])), [detail.items]);
  return <div className="production-table-wrap"><table className="production-table">
    <thead><tr><th className="check-column"><input type="checkbox" aria-label="Select all visible candidates" checked={all} disabled={busy || !rows.length} onChange={(e) => onSelect(e.target.checked ? new Set(rows.map((r) => r.id)) : new Set())} /></th>
      {titleColumn && <th>Title</th>}<th>{titleColumn ? "Subtitle" : "Title"}</th><th>Length</th><th>Status</th><th>Source</th><th>Notes / edit</th></tr></thead>
    <tbody>{rows.map((row) => <tr key={row.id} className={selected.has(row.id) ? "is-selected" : ""}>
      <td><input aria-label={`Select ${row.text}`} type="checkbox" checked={selected.has(row.id)} disabled={busy} onChange={(e) => { const next = new Set(selected); if (e.target.checked) next.add(row.id); else next.delete(row.id); onSelect(next); }} /></td>
      {titleColumn && <td className="production-parent-cell">{titleMap.get(row.parent_id ?? "")}</td>}
      <td className="production-title-cell">{row.text}</td><td className={[...row.text].length > LIMITS[row.kind] ? "over-limit" : ""}>{[...row.text].length}/{LIMITS[row.kind]}</td>
      <td><StatusBadge value={row.status} /></td><td className="production-source">{row.source_candidate_id ? row.model ?? "Generated" : "Manual"}</td>
      <td><span className="production-note-preview" title={row.notes}>{row.notes || "—"}</span><button type="button" className="quiet" disabled={busy} onClick={() => onEdit(row.id)}>Edit</button></td>
    </tr>)}</tbody>
  </table></div>;
}
export function ItemEditor({ item, onClose, outline = false }: { item: Item; onClose?: () => void; outline?: boolean }) {
  const { detail, busy, submit } = useWorkspace();
  const form = useEditor(`item-${item.id}`, { text: item.text, notes: item.notes, acknowledge: false });
  let stale = false;
  try { stale = item.input_snapshot !== inputSnapshot(detail, item.kind, item.parent_id); } catch { stale = true; }
  return <section className={`production-editor${outline ? " outline-editor" : ""}`} aria-label={outline ? "Outline editor" : "Candidate editor"}>
    <div className="production-actions"><h3>{outline ? "Outline draft" : "Edit candidate"}</h3><StatusBadge value={item.status} />{onClose && <button type="button" className="quiet" onClick={() => { if (!form.dirty || window.confirm("Discard these unsaved edits?")) onClose(); }}>Close editor</button>}</div>
    {stale && <p className="production-warning">This alternative was made for earlier inputs. Check it against the current package before approving.</p>}
    <label>{outline ? "Outline text" : "Candidate text"}<textarea className={outline ? "production-outline-input" : ""} rows={outline ? 14 : 3} value={form.value.text} disabled={busy} onChange={(e) => form.setValue((v) => ({ ...v, text: e.target.value }))} /></label>
    <label>Review notes<input value={form.value.notes} maxLength={10000} disabled={busy} onChange={(e) => form.setValue((v) => ({ ...v, notes: e.target.value }))} /></label>
    {stale && <label className="production-inline-check"><input type="checkbox" checked={form.value.acknowledge} onChange={(e) => form.setValue((v) => ({ ...v, acknowledge: e.target.checked }))} />I checked this text against the current inputs.</label>}
    <div className="production-actions">
      <button type="button" disabled={busy || !form.dirty} onClick={async () => { const result = await submit("edit_item", { item_id: item.id, text: form.value.text, notes: form.value.notes, acknowledge_context: form.value.acknowledge }, form.revision); if (result) form.saved(result.revision); }}>Save {outline ? "outline edits" : "candidate edits"}</button>
      <button type="button" className="quiet" disabled={busy} onClick={form.reload}>Load saved text</button>
      <small>{form.dirty ? "Unsaved changes" : "Saved"} · {[...form.value.text].length}/{LIMITS[item.kind]}</small>
    </div>
    {item.status === "approved" && <p className="production-muted">Changing approved text returns its dependent work for review. Old reviewer versions stay frozen.</p>}
    <details className="production-details"><summary>Original output & provenance</summary><dl><dt>Source</dt><dd>{item.source_candidate_id ? item.model ?? "Generated" : "Manual"}</dd><dt>Route</dt><dd>{item.route_id ?? "Not applicable"}</dd><dt>Created</dt><dd>{new Date(item.created_at).toLocaleString()}</dd></dl><pre>{item.original_text}</pre></details>
  </section>;
}
export function SelectionActions({ selected, clear, approveLabel }: { selected: Set<string>; clear: () => void; approveLabel: string }) {
  const { submit, busy, contentDirty } = useWorkspace();
  const apply = async (status: string) => { const result = await submit("item_status", { ids: [...selected], status }); if (result) clear(); };
  return <div className="production-actions selection-actions"><span>{selected.size} selected</span>
    <button type="button" disabled={busy || contentDirty || !selected.size} onClick={() => void apply("approved")}>{approveLabel}</button>
    <button type="button" className="quiet" disabled={busy || contentDirty || !selected.size} onClick={() => void apply("archived")}>Archive selected</button>
    <button type="button" className="quiet" disabled={busy || contentDirty || !selected.size} onClick={() => void apply("candidate")}>Restore to candidates</button>
  </div>;
}
export function GenerationSetup({ kind, parents, label, open = false }: { kind: ItemKind; parents: (string | null)[]; label: string; open?: boolean }) {
  const { detail, catalog, submit, busy, report } = useWorkspace();
  const initial = detail.settings[kind] ?? { route_id: catalog.routes[0]?.id ?? "", count: kind === "outline" ? 1 : kind === "titles" ? 12 : kind === "subtitles" ? 4 : 3, prompt: DEFAULT_PROMPTS[kind], directions: "" };
  const form = useEditor(`settings-${kind}`, initial);
  const [template, setTemplate] = useState(""), [templateName, setTemplateName] = useState("");
  const templates = detail.templates.filter((t) => t.kind === kind);
  let previews: string[] = [], previewError = "";
  try { previews = parents.map((parent) => providerPrompt({ kind, count: form.value.count, resolved_prompt: resolvedPrompt(detail, kind, parent, form.value) })); }
  catch (e) { previewError = (e as Error).message; }
  const generate = async () => {
    if (previewError) { report(previewError); return; }
    const result = await submit("generate", { kind, settings: form.value, jobs: parents.map((parent) => ({ id: crypto.randomUUID(), parent_id: parent })) }, form.revision);
    if (result) form.saved(result.revision);
  };
  return <section className="production-setup" aria-label={`${kind} generation settings`}>
    <div className="production-generation-bar">
      <label>Model<select aria-label={`${kind} model`} value={form.value.route_id} disabled={busy} onChange={(e) => form.setValue((v) => ({ ...v, route_id: e.target.value }))}>
        <option value="">Choose a configured model</option>{catalog.routes.map((r) => <option key={r.id} value={r.id}>{r.label} · {r.model}</option>)}
      </select></label>
      <label>{kind === "outline" ? "Outlines per package" : "Candidates per input"}<input aria-label={`${kind} candidate count`} type="number" min={1} max={kind === "outline" ? 1 : 25} value={form.value.count} disabled={busy || kind === "outline"} onChange={(e) => form.setValue((v) => ({ ...v, count: Number(e.target.value) }))} /></label>
      <button type="button" disabled={busy || !parents.length || !form.value.route_id || Boolean(previewError) || !catalog.runner_configured} onClick={() => void generate()}>{label}{parents.length > 1 ? ` (${parents.length} inputs)` : ""}</button>
    </div>
    {previewError && <p className="production-muted">{previewError}</p>}
    <details className="production-details" open={open || undefined}>
      <summary>Prompt templates, directions & exact request{form.dirty ? " · unsaved settings" : ""}</summary>
      <div className="production-form-grid">
        <label>Saved template<select value={template} onChange={(e) => { setTemplate(e.target.value); const selected = templates.find((t) => t.id === e.target.value); if (selected) { form.setValue((v) => ({ ...v, prompt: selected.prompt })); setTemplateName(selected.name); } }}><option value="">Current prompt</option>{templates.map((t) => <option key={t.id} value={t.id}>{t.name}</option>)}</select></label>
        <label>Template name<input value={templateName} onChange={(e) => setTemplateName(e.target.value)} maxLength={100} placeholder="e.g. Clear, specific, no hype" /></label>
      </div>
      <label>Editable prompt<textarea rows={5} value={form.value.prompt} onChange={(e) => form.setValue((v) => ({ ...v, prompt: e.target.value }))} /></label>
      <label>Additional directions<textarea rows={2} value={form.value.directions} onChange={(e) => form.setValue((v) => ({ ...v, directions: e.target.value }))} /></label>
      <div className="production-actions">
        <button type="button" className="quiet" disabled={busy || !form.dirty} onClick={async () => { const r = await submit("save_settings", { kind, settings: form.value }, form.revision); if (r) form.saved(r.revision); }}>Save generation settings</button>
        <button type="button" className="quiet" disabled={busy || !templateName.trim()} onClick={async () => { await submit("save_template", { kind, template_id: template || crypto.randomUUID(), name: templateName, prompt: form.value.prompt }); }}>Save named template</button>
        {template && <button type="button" className="quiet" disabled={busy} onClick={async () => { if (window.confirm("Delete this saved template? Generated output and current text will remain.")) { const r = await submit("delete_template", { template_id: template }); if (r) setTemplate(""); } }}>Delete template</button>}
        <button type="button" className="quiet" onClick={form.reload}>Load saved settings</button>
      </div>
      <p className="production-muted">Reasoning uses this connection’s server default. Unsupported model controls are not simulated.</p>
      <details><summary>Prompt that will be sent, including wrapper and selected context ({parents.length} requests)</summary>{previews.length ? previews.map((p, i) => <pre key={i}>{p}</pre>) : <p>Select inputs above to see the exact request.</p>}</details>
    </details>
  </section>;
}
export function ManualEntry({ kind, parent, label }: { kind: "titles" | "subtitles"; parent: string | null; label: string }) {
  const { submit, busy } = useWorkspace();
  const form = useEditor(`manual-${kind}`, { text: "" });
  return <details className="production-details"><summary>{label}</summary><label>One candidate per line<textarea rows={3} value={form.value.text} onChange={(e) => form.setValue({ text: e.target.value })} /></label>
    <button type="button" disabled={busy || !form.value.text.trim() || (kind === "subtitles" && !parent)} onClick={async () => {
      const items = form.value.text.split(/\r?\n/).map((v) => v.trim()).filter(Boolean).map((text) => ({ id: crypto.randomUUID(), text }));
      const r = await submit("add_items", { kind, parent_id: parent, items }); if (r) { form.setValue({ text: "" }); }
    }}>Add manual {kind}</button>
    {kind === "subtitles" && !parent && <p>Choose one title above for manual subtitles.</p>}
  </details>;
}
export function LegacyResults({ kind, parent }: { kind: ItemKind; parent: string | null }) {
  const { detail, submit, busy } = useWorkspace();
  const rows = detail.unassigned.filter((r) => r.kind === kind);
  if (!rows.length) return null;
  return <details className="production-details production-legacy"><summary>Existing workspace results not yet assigned to this workflow ({rows.length})</summary>
    <p>Nothing was deleted. Choose the correct title/package before bringing older results into this stage; the app does not guess their relationships.</p>
    <ul className="production-legacy-list">{rows.map((row) => <li key={row.id}><span>{row.value.slice(0, 220)}{row.value.length > 220 ? "…" : ""}</span><button type="button" className="quiet" disabled={busy || (kind !== "titles" && !parent)} onClick={() => void submit("adopt_candidates", { kind, parent_id: parent, ids: [row.id] })}>Use in this stage</button></li>)}</ul>
  </details>;
}
export function JobHistory({ kind, parent }: { kind: string; parent?: string | null }) {
  const { detail, refresh, busy, report } = useWorkspace();
  const jobs = detail.jobs.filter((j) => j.kind === kind && (parent === undefined || j.parent_id === parent));
  const failures = jobs.filter((j) => ["failed", "uncertain"].includes(j.status));
  if (!jobs.length) return null;
  return <div className="production-job-history">
    {failures.length > 0 && <div className="production-warning" role="status">{failures.length} failed or uncertain request(s). Existing candidates and approvals were not replaced. Inspect the details before requesting another generation.</div>}
    <details className="production-details"><summary>Generation history · {jobs.filter((j) => ["queued", "running"].includes(j.status)).length} active / {jobs.length} shown</summary>
      <ul>{jobs.map((job) => <li key={job.id}><div className="production-actions"><StatusBadge value={job.status} /><span>{job.actual_model || job.route_id || "Model pending"} · {new Date(job.created_at).toLocaleString()}</span>
        {job.status === "queued" && <button type="button" className="quiet" disabled={busy} onClick={async () => {
          try { await request(`/workspaces/${detail.article_id}/${job.is_image ? "image-jobs" : "jobs"}`, "PATCH", { id: job.id, action: "cancel" }); await refresh(); } catch (e) { report((e as Error).message); }
        }}>Cancel queued request</button>}
      </div>{job.error_code && <p>{job.error_code}. {job.status === "uncertain" ? "The provider may already have run. No automatic replay." : "Check the connection before trying again."}</p>}
        <details><summary>Exact request / provenance</summary><pre>{job.request_json ?? "Not recorded"}</pre><small>{job.id}</small></details>
      </li>)}</ul>
    </details>
  </div>;
}
export function PackageNotes({ pkg }: { pkg: Package }) {
  const { submit, busy } = useWorkspace(), form = useEditor(`package-notes-${pkg.id}`, { notes: pkg.notes });
  return <details className="production-details"><summary>Package context / notes</summary><label>Additional context notes<textarea rows={3} value={form.value.notes} onChange={(e) => form.setValue({ notes: e.target.value })} /></label>
    <div className="production-actions"><button type="button" disabled={busy || !form.dirty} onClick={async () => { const r = await submit("package", { package_id: pkg.id, notes: form.value.notes }, form.revision); if (r) form.saved(r.revision); }}>Save context notes</button><button type="button" className="quiet" onClick={form.reload}>Load saved notes</button></div>
  </details>;
}
