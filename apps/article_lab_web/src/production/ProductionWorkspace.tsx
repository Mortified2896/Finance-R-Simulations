import { lazy, Suspense, useCallback, useEffect, useState } from "react";
import { STAGES, STAGE_LABELS } from "../../shared/production";
import type { Stage } from "../../shared/production";
import { request } from "./network";
import type { Catalog } from "./network";
import { WorkspaceState, useEditor, useWorkspace } from "./state";
import { TitleLab } from "./TitleLab";
import { SubtitleStage } from "./SubtitleStage";
import { ThumbnailStage } from "./ThumbnailStage";
import { OutlineStage } from "./OutlineStage";
import "./production.css";
const FullTextStage = lazy(() => import("./FullTextStage").then((m) => ({ default: m.FullTextStage })));
const ReviewPublishStage = lazy(() => import("./ReviewPublishStage").then((m) => ({ default: m.ReviewPublishStage })));
type Project = { article_id: string; topic: string; revision: number; updated_at: string };
const EMPTY_CATALOG: Catalog = { routes: [], runner_configured: false, image_storage_configured: false, images_configured: false, images: { models: [], sizes: [], qualities: [] }, runners: [] };
function locationState() {
  const parts = location.hash.match(/^#writing\/([0-9a-f-]{36})\/([a-z]+)$/i);
  return { article: parts?.[1] ?? "", stage: STAGES.includes(parts?.[2] as Stage) ? parts![2] as Stage : "titles" as Stage };
}
function BriefEditor() {
  const { detail, submit, busy } = useWorkspace();
  const form = useEditor("brief", { topic: detail.workspace.topic, brief: detail.workspace.brief, evidence: detail.workspace.evidence, workspace_revision: detail.workspace.revision });
  return <details className="production-brief production-details"><summary><strong>{detail.workspace.topic}</strong> · Brief & evidence{form.dirty ? " · unsaved" : ""}</summary>
    <label>Working topic<input value={form.value.topic} maxLength={250} onChange={(e) => form.setValue((v) => ({ ...v, topic: e.target.value }))} /></label>
    <div className="production-form-grid"><label>Core brief<textarea rows={6} value={form.value.brief} onChange={(e) => form.setValue((v) => ({ ...v, brief: e.target.value }))} /></label><label>Evidence & notes<textarea rows={6} value={form.value.evidence} onChange={(e) => form.setValue((v) => ({ ...v, evidence: e.target.value }))} /></label></div>
    <div className="production-actions"><button type="button" disabled={busy || !form.dirty} onClick={async () => { const r = await submit("workspace", form.value, form.revision); if (r) form.saved(r.revision); }}>Save brief & evidence</button><button type="button" className="quiet" onClick={form.reload}>Load saved brief</button></div>
    <p className="production-muted">Saved context is used in subsequent generation requests. Existing output and original legacy working drafts are preserved.</p>
  </details>;
}
export function ProductionShell() {
  const { detail, catalog, stage, setStage, refresh, report } = useWorkspace();
  const counts: Record<Stage, number> = {
    titles: detail.items.filter((i) => i.kind === "titles" && i.status !== "archived").length,
    subtitles: detail.items.filter((i) => i.kind === "subtitles" && i.status !== "archived").length,
    thumbnails: detail.image_links.filter((i) => !i.archived).length,
    outline: detail.items.filter((i) => i.kind === "outline" && i.status !== "archived").length,
    drafts: detail.drafts.filter((i) => i.status !== "rejected").length,
    publish: detail.drafts.filter((i) => i.status === "approved").length,
  };
  return <div className="production-shell">
    <aside className="production-sidebar"><div className="production-nav-label">Article Production</div><nav aria-label="Article production stages">{STAGES.map((key, i) => <button type="button" key={key} aria-current={stage === key ? "step" : undefined} className={stage === key ? "is-active" : ""} onClick={() => setStage(key)}><span className="production-step-number">{i + 1}</span><span>{STAGE_LABELS[key]}</span><small>{counts[key]}</small></button>)}</nav>
      <details className="production-connections"><summary>Generation connections</summary><p>{catalog.runner_configured ? catalog.runners[0] ? `Runner last checked in ${new Date(catalog.runners[0].last_seen).toLocaleString()}.` : "Runner configured; no heartbeat yet." : "Runner not configured."}</p><p>A heartbeat does not prove provider availability.</p>{catalog.routes.map((r) => <p key={r.id}>{r.label} · {r.model}</p>)}<p>Image storage: {catalog.image_storage_configured ? "configured" : "not configured"}</p><p>Image generation: {catalog.images_configured ? "configured" : "not configured"}</p></details>
      <button type="button" className="quiet" onClick={() => void refresh().catch((e) => report((e as Error).message))}>Refresh saved state</button>
    </aside>
    <div className="production-main"><BriefEditor /><Suspense fallback={<p>Loading the editor…</p>}>
      {stage === "titles" ? <TitleLab /> : stage === "subtitles" ? <SubtitleStage /> : stage === "thumbnails" ? <ThumbnailStage /> : stage === "outline" ? <OutlineStage /> : stage === "drafts" ? <FullTextStage /> : <ReviewPublishStage />}
    </Suspense></div>
  </div>;
}
export function ProductionWorkspace({ onDirtyChange }: { onDirtyChange: (dirty: boolean) => void }) {
  const [projects, setProjects] = useState<Project[]>([]), [catalog, setCatalog] = useState<Catalog>(EMPTY_CATALOG);
  const [article, setArticle] = useState(() => locationState().article), [stage, changeStage] = useState<Stage>(() => locationState().stage);
  const [newTopic, setNewTopic] = useState(""), [creating, setCreating] = useState(false), [showNew, setShowNew] = useState(false), [error, setError] = useState("");
  const [dirty, setDirty] = useState(false);
  const handleDirty = useCallback((value: boolean) => { setDirty(value); onDirtyChange(value); }, [onDirtyChange]);
  const load = useCallback(async () => {
    const [nextProjects, nextCatalog] = await Promise.all([request<Project[]>("/workspaces"), request<Catalog>("/routes")]);
    setProjects(nextProjects); setCatalog(nextCatalog);
    if (!article && nextProjects.length) setArticle(nextProjects[0].article_id);
  }, [article]);
  useEffect(() => { void load().catch((e) => setError((e as Error).message)); }, []);
  useEffect(() => {
    const timer = window.setInterval(() => { void request<Catalog>("/routes").then(setCatalog).catch(() => {}); }, 30000);
    return () => window.clearInterval(timer);
  }, []);
  useEffect(() => {
    if (article) history.replaceState(null, "", `${location.pathname}${location.search}#writing/${article}/${stage}`);
  }, [article, stage]);
  const selectProject = (id: string) => {
    if (dirty && !window.confirm("Discard unsaved edits before switching articles? Cancel to save or export first.")) return;
    handleDirty(false); setArticle(id); setError("");
  };
  const create = async () => {
    if (dirty && !window.confirm("Discard unsaved edits and create another article?")) return;
    setCreating(true); setError("");
    try { const result = await request<{ article_id: string }>("/workspaces", "POST", { topic: newTopic.trim() }); await load(); handleDirty(false); setArticle(result.article_id); changeStage("titles"); setNewTopic(""); setShowNew(false); }
    catch (e) { setError((e as Error).message); } finally { setCreating(false); }
  };
  return <div className="production">
    <section className="production-project-header" aria-label="Article project"><label>Article project<select aria-label="Open a workspace" value={article} onChange={(e) => selectProject(e.target.value)}><option value="">Choose an article…</option>{projects.map((p) => <option key={p.article_id} value={p.article_id}>{p.topic}</option>)}</select></label><button type="button" className="quiet" onClick={() => setShowNew((v) => !v)}>New article</button><button type="button" className="quiet" onClick={() => void load().catch((e) => setError((e as Error).message))}>Refresh projects</button></section>
    {(showNew || !article) && <section className="production-create"><h2>Create an article project</h2><p>One article keeps its title batches, subtitle packages, thumbnails, outlines and draft variants together.</p><div className="production-actions"><label>Working title<input value={newTopic} maxLength={250} onChange={(e) => setNewTopic(e.target.value)} placeholder="e.g. What we learned ranking finance articles" /></label><button type="button" disabled={creating || !newTopic.trim()} onClick={() => void create()}>Create workspace</button></div></section>}
    {error && <div className="production-error" role="alert"><p>{error}</p></div>}
    {article ? <WorkspaceState key={article} articleId={article} catalog={catalog} stage={stage} setStage={changeStage} onDirtyChange={handleDirty}><ProductionShell /></WorkspaceState> : <div className="production-empty-workflow"><h2>Your production workflow</h2><ol>{STAGES.map((s) => <li key={s}>{STAGE_LABELS[s]}</li>)}</ol><p>Create or open an article above to use these stages. Scoring and the flexible agent sidebar are not part of this pass.</p></div>}
  </div>;
}
