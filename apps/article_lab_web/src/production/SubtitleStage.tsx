import { useState } from "react";
import { useWorkspace } from "./state";
import { CandidateTable, EmptyState, GenerationSetup, ItemEditor, JobHistory, LegacyResults, ManualEntry, SelectionActions, StageHeading, StatusBadge } from "./components";
export function SubtitleStage() {
  const { detail, busy, dirty } = useWorkspace();
  const titles = detail.items.filter((i) => i.kind === "titles" && i.status === "approved");
  const [targets, setTargets] = useState(new Set<string>()), [selected, setSelected] = useState(new Set<string>()), [focus, setFocus] = useState(""), [archived, setArchived] = useState(false), [edit, setEdit] = useState<string | null>(null);
  const parent = titles.some((t) => t.id === focus) ? focus : titles[0]?.id ?? "";
  const targetIds = [...targets].filter((key) => titles.some((t) => t.id === key));
  const rows = detail.items.filter((i) => i.kind === "subtitles" && (!focus || i.parent_id === focus) && (archived || i.status !== "archived"));
  const item = detail.items.find((i) => i.id === edit);
  return <>
    <StageHeading title="Subtitle Generation" description="Work with one or several approved titles. Each subtitle stays attached to the title that generated it." next="thumbnails" nextLabel="Thumbnails" />
    {!titles.length ? <EmptyState title="No approved titles yet" next="titles" nextLabel="Open Title Lab">Approve at least one title to start subtitle production.</EmptyState> : <>
      <h2>1. Titles ready for subtitles</h2><div className="production-table-wrap"><table className="production-table"><thead><tr><th>Select</th><th>Title</th><th>Status</th><th>Subtitles</th></tr></thead><tbody>{titles.map((t) => <tr key={t.id}><td><input type="checkbox" aria-label={`Generate subtitles for ${t.text}`} checked={targets.has(t.id)} disabled={busy} onChange={(e) => { const n = new Set(targets); if (e.target.checked) n.add(t.id); else n.delete(t.id); setTargets(n); }} /></td><td className="production-title-cell">{t.text}</td><td><StatusBadge value="approved" /></td><td>{detail.items.filter((i) => i.kind === "subtitles" && i.parent_id === t.id && i.status !== "archived").length}</td></tr>)}</tbody></table></div>
      <GenerationSetup kind="subtitles" parents={targetIds} label="Generate subtitles for selected" />
      {!targetIds.length && <p className="production-muted">Check the titles above to generate their subtitle batches.</p>}
    </>}
    <h2>2. Subtitle candidates awaiting approval</h2>
    <div className="production-filter-bar"><label>Title filter / manual target<select value={focus} onChange={(e) => { if (!dirty || window.confirm("Discard unsaved edits before changing the manual subtitle target?")) { setFocus(e.target.value); setSelected(new Set()); setEdit(null); } }}><option value="">All titles</option>{titles.map((t) => <option value={t.id} key={t.id}>{t.text}</option>)}</select></label><label className="production-inline-check"><input type="checkbox" checked={archived} onChange={(e) => { setArchived(e.target.checked); setSelected(new Set()); }} />Show archived</label></div>
    <SelectionActions selected={selected} clear={() => setSelected(new Set())} approveLabel="Approve for thumbnails" />
    {rows.length ? <CandidateTable titleColumn rows={rows} selected={selected} onSelect={setSelected} onEdit={(key) => { if (!dirty || window.confirm("Discard unsaved edits before opening another subtitle?")) setEdit(key); }} /> : <EmptyState title="No subtitle candidates in this view">Generate candidates for the selected titles or add your own subtitle below.</EmptyState>}
    {item && <ItemEditor key={item.id} item={item} onClose={() => setEdit(null)} />}
    <p className="production-muted">Manual/import target: {titles.find((t) => t.id === parent)?.text ?? "Choose an approved title first"}</p>
    <ManualEntry key={`manual-${parent}`} kind="subtitles" parent={parent || null} label="Add a subtitle without AI" />
    <LegacyResults kind="subtitles" parent={parent || null} /><JobHistory kind="subtitles" />
  </>;
}
