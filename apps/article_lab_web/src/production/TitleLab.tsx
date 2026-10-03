import { useState } from "react";
import { useWorkspace } from "./state";
import { CandidateTable, EmptyState, GenerationSetup, ItemEditor, JobHistory, LegacyResults, ManualEntry, SelectionActions, StageHeading } from "./components";
export function TitleLab() {
  const { detail, dirty } = useWorkspace();
  const [selected, setSelected] = useState(new Set<string>()), [archived, setArchived] = useState(false), [filter, setFilter] = useState(""), [edit, setEdit] = useState<string | null>(null);
  const rows = detail.items.filter((i) => i.kind === "titles" && (archived || i.status !== "archived") && i.text.toLowerCase().includes(filter.toLowerCase()));
  const item = detail.items.find((i) => i.id === edit);
  return <>
    <StageHeading title="Title Lab" description="Generate, compare and manually shortlist titles. Scoring is deferred; your approval moves titles to subtitles." next="subtitles" nextLabel="Subtitle Generation" />
    <GenerationSetup kind="titles" parents={[null]} label="Generate titles" />
    <div className="production-filter-bar"><label className="production-search">Filter titles<input placeholder="Search current article’s titles" value={filter} onChange={(e) => { setFilter(e.target.value); setSelected(new Set()); }} /></label><label className="production-inline-check"><input type="checkbox" checked={archived} onChange={(e) => { setArchived(e.target.checked); setSelected(new Set()); }} />Show archived</label><span>{rows.length} titles</span></div>
    <SelectionActions selected={selected} clear={() => setSelected(new Set())} approveLabel="Approve for subtitles" />
    {rows.length ? <CandidateTable rows={rows} selected={selected} onSelect={setSelected} onEdit={(key) => { if (!dirty || window.confirm("Discard unsaved edits before opening another candidate?")) setEdit(key); }} /> : <EmptyState title="No titles in this view">Generate a batch above, add your own titles below, or restore previous results.</EmptyState>}
    {item && <ItemEditor key={item.id} item={item} onClose={() => setEdit(null)} />}
    <ManualEntry kind="titles" parent={null} label="Add title ideas manually" />
    <LegacyResults kind="titles" parent={null} /><JobHistory kind="titles" />
  </>;
}
