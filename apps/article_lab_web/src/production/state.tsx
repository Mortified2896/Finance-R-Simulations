import { createContext, useCallback, useContext, useEffect, useMemo, useRef, useState } from "react";
import type { ReactNode, Dispatch, SetStateAction } from "react";
import type { Command, ProductionDetail, Stage } from "../../shared/production";
import { request } from "./network";
import type { Catalog } from "./network";

type Result = { revision: number; [key: string]: unknown };
type WorkspaceContext = {
  detail: ProductionDetail; catalog: Catalog; busy: boolean; dirty: boolean; contentDirty: boolean; stage: Stage;
  submit: (action: string, values?: Record<string, unknown>, revision?: number) => Promise<Result | null>;
  refresh: () => Promise<void>; setStage: (stage: Stage) => void;
  registerDirty: (key: string, value: boolean) => void;
  report: (message: string) => void;
};
const Context = createContext<WorkspaceContext | null>(null);
export function useWorkspace() {
  const value = useContext(Context); if (!value) throw new Error("Production workspace is missing."); return value;
}

export function WorkspaceState({ articleId, catalog, stage, setStage, onDirtyChange, children }: {
  articleId: string; catalog: Catalog; stage: Stage; setStage: (stage: Stage) => void;
  onDirtyChange: (dirty: boolean) => void; children: ReactNode;
}) {
  const [detail, setDetail] = useState<ProductionDetail | null>(null);
  const [error, setError] = useState("");
  const [notice, setNotice] = useState("");
  const [busy, setBusy] = useState(false);
  const [pending, setPending] = useState<Command | null>(null);
  const [dirtyCount, setDirtyCount] = useState(0);
  const dirtyKeys = useRef(new Set<string>()), current = useRef(detail), working = useRef(false), epoch = useRef(0), mounted = useRef(true);
  current.current = detail;
  const registerDirty = useCallback((key: string, value: boolean) => {
    if (value) dirtyKeys.current.add(key); else dirtyKeys.current.delete(key);
    onDirtyChange(dirtyKeys.current.size > 0); setDirtyCount(dirtyKeys.current.size);
  }, [onDirtyChange]);
  const refresh = useCallback(async () => {
    const ticket = ++epoch.current;
    const next = await request<ProductionDetail>(`/production/${articleId}`);
    if (mounted.current && ticket === epoch.current) setDetail(next);
  }, [articleId]);
  useEffect(() => {
    mounted.current = true;
    void refresh().catch((e) => setError((e as Error).message));
    return () => { mounted.current = false; epoch.current++; onDirtyChange(false); };
  }, [refresh, onDirtyChange]);
  useEffect(() => {
    const warn = (event: BeforeUnloadEvent) => {
      if (dirtyKeys.current.size) { event.preventDefault(); event.returnValue = ""; }
    };
    window.addEventListener("beforeunload", warn);
    return () => window.removeEventListener("beforeunload", warn);
  }, []);
  const dispatch = useCallback(async (payload: Command): Promise<Result | null> => {
    if (working.current) { setError("The workspace is finishing another action or refresh. Your input is preserved; retry when it finishes."); return null; }
    working.current = true; setBusy(true); setError(""); epoch.current++;
    let committed = false;
    try {
      const result = await request<Result>(`/production/${articleId}/command`, "POST", payload);
      committed = true; setPending(null);
      await refresh();
      if (mounted.current) setNotice(payload.action.startsWith("generate") ? "Queued. Alternatives will appear when the runner finishes; existing approvals are unchanged." : "Saved to Article Lab.");
      return result;
    } catch (e) {
      if (mounted.current) {
        setError(`${committed ? "The action was saved, but refreshing failed. " : ""}${(e as Error).message}`);
        // Replaying exactly this ID is safe even after a successful-but-lost response.
        setPending(payload);
      }
      return null;
    } finally { working.current = false; if (mounted.current) setBusy(false); }
  }, [articleId, refresh]);
  const submit = useCallback(async (action: string, values: Record<string, unknown> = {}, revision?: number) => {
    if (!current.current) return null;
    if (pending) { setError("Resolve the previous failed action first: retry the same action, or dismiss it after checking the saved state. Your unsaved edits remain."); return null; }
    return dispatch({ ...values, id: crypto.randomUUID(), revision: revision ?? current.current.revision, action });
  }, [dispatch, pending]);
  const active = Boolean(detail?.jobs.some((job) => job.status === "queued" || job.status === "running"));
  const sync = useCallback(async () => {
    if (working.current) return;
    working.current = true; setBusy(true);
    try {
      let next = await request<ProductionDetail>(`/production/${articleId}`);
      // Import results only while no editor has unsaved work. Job polling itself
      // can continue; it never replaces a mounted editor's dirty buffer.
      if (!dirtyKeys.current.size && !pending) {
        for (let pass = 0; pass < 10 && !dirtyKeys.current.size; pass++) {
          const result = await request<{ imported: number }>(`/production/${articleId}/command`, "POST", { id: crypto.randomUUID(), revision: next.revision, action: "sync" });
          next = await request<ProductionDetail>(`/production/${articleId}`);
          if (!result.imported) break;
        }
      }
      if (mounted.current) setDetail(next);
    } catch (e) { if (mounted.current) setError((e as Error).message); }
    finally { working.current = false; if (mounted.current) setBusy(false); }
  }, [articleId, pending]);
  useEffect(() => { if (current.current && dirtyCount === 0) void sync(); }, [articleId, Boolean(detail), dirtyCount]); // once after opening; imports results completed while away
  useEffect(() => {
    if (!active) return;
    const timer = window.setInterval(() => { void sync(); }, 5000);
    return () => window.clearInterval(timer);
  }, [active, sync]);
  const navigate = useCallback((next: Stage) => {
    if (dirtyKeys.current.size && !window.confirm("Discard unsaved edits in this stage? Cancel to save or export them first.")) return;
    dirtyKeys.current.clear(); onDirtyChange(false); setError(""); setNotice(""); setStage(next);
  }, [onDirtyChange, setStage]);
  const value = useMemo(() => detail ? { detail, catalog, busy, dirty: dirtyCount > 0, contentDirty: [...dirtyKeys.current].some((k) => /^(item-|draft-|package-notes-|publish-)/.test(k)), stage, submit, refresh, setStage: navigate, registerDirty, report: setError } : null,
    [detail, catalog, busy, dirtyCount, stage, submit, refresh, navigate, registerDirty]);
  return <>
    {error && <div className="production-error" role="alert"><strong>{stage === "publish" ? "Review & Publish" : "Production"} action needs attention</strong><p>{error}</p><div className="production-actions">
      {pending && <button type="button" disabled={busy} onClick={() => void dispatch(pending)}>Retry same action</button>}
      <button type="button" disabled={busy} onClick={() => { void refresh().catch((e) => setError((e as Error).message)); }}>Refresh saved state</button>
      <button type="button" disabled={busy} onClick={() => { setPending(null); setError(""); }}>Dismiss error; keep edits</button>
    </div></div>}
    {notice && <p className="production-notice" role="status">{notice}</p>}
    {!value ? <p>Loading the article’s production stages…</p> : <Context.Provider value={value}>{children}</Context.Provider>}
  </>;
}

/** A dirty form keeps both its text AND its original revision during polling. */
export function useEditor<T>(key: string, initial: T) {
  const { detail, registerDirty } = useWorkspace();
  const incoming = JSON.stringify(initial);
  const [state, setState] = useState({ value: initial, saved: incoming, revision: detail.revision });
  const dirty = JSON.stringify(state.value) !== state.saved;
  useEffect(() => {
    if (!dirty) setState({ value: JSON.parse(incoming) as T, saved: incoming, revision: detail.revision });
    else if (incoming === state.saved && state.revision !== detail.revision) setState((old) => ({ ...old, revision: detail.revision }));
  }, [incoming, detail.revision, dirty]);
  useEffect(() => { registerDirty(key, dirty); return () => registerDirty(key, false); }, [key, dirty, registerDirty]);
  const setValue: Dispatch<SetStateAction<T>> = useCallback((next) => setState((old) => ({ ...old, value: typeof next === "function" ? (next as (v: T) => T)(old.value) : next })), []);
  const saved = (revision: number) => setState((old) => ({ ...old, saved: JSON.stringify(old.value), revision }));
  const reload = () => {
    if (dirty && !window.confirm("Discard these unsaved edits and load the current saved version?")) return;
    setState({ value: JSON.parse(incoming) as T, saved: incoming, revision: detail.revision });
  };
  return { value: state.value, setValue, dirty, revision: state.revision, saved, reload };
}
