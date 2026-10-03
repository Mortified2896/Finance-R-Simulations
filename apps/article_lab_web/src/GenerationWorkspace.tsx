import {
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
  type ReactNode,
} from "react";
import type {
  GenerationCandidate,
  GenerationKind,
  GenerationRoute,
  ImageJobView,
  ImageAsset,
  LabCatalog,
  PromptSettings,
  WorkspaceDetail,
  WorkspaceRow,
} from "../shared/types";
import { api, ApiError } from "./api";
import { MarkdownComposer } from "./editor/MarkdownComposer";

const KINDS: {
  key: GenerationKind;
  heading: string;
  blurb: string;
  countLabel: string;
}[] = [
  {
    key: "titles",
    heading: "1 · Titles",
    blurb:
      "Generate candidate titles, then select the one later stages build on.",
    countLabel: "Title candidates",
  },
  {
    key: "subtitles",
    heading: "2 · Subtitles",
    blurb: "Subtitle candidates for the selected title.",
    countLabel: "Subtitle candidates",
  },
  {
    key: "thumbnail_concepts",
    heading: "3 · Thumbnail concept",
    blurb:
      "Visual concepts with composition and alt text. Pixels come later, via the paid image API or an upload.",
    countLabel: "Concepts",
  },
  {
    key: "outline",
    heading: "4 · Outline",
    blurb: "One reader-facing outline for the selected package.",
    countLabel: "Outline",
  },
];

const DEFAULT_PROMPTS: Record<GenerationKind, string> = {
  titles:
    "Write beginner-friendly personal finance title candidates grounded in the brief and evidence below. Keep claims credible and specific; avoid clickbait, hype and overclaiming. Preserve supplied facts and clearly mark missing evidence.",
  subtitles:
    "Write subtitle candidates that add a concrete promise or angle beyond the selected title without repeating it. Keep them credible and specific to the brief.",
  thumbnail_concepts:
    "Describe one memorable thumbnail concept per candidate: composition, focal subject, any short on-image text, crop-safe placement, and suggested alt text. Concepts only — no image pixels.",
  outline:
    "Write a practical article outline for the selected title, subtitle and thumbnail concept. Use Markdown headings and bullets, a short hook, 4-6 main sections with key points, and a concise closing angle. Do not draft the full article.",
};

const KIND_LIMITS: Record<GenerationKind, number> = {
  titles: 140,
  subtitles: 500,
  thumbnail_concepts: 4000,
  outline: 30000,
};

type Form = { topic: string; brief: string; evidence: string };

function statusLabel(job: { status: string; error_code: string | null }): {
  text: string;
  tone: "busy" | "ok" | "bad" | "warn";
} {
  switch (job.status) {
    case "queued":
      return { text: "Queued — waiting for the runner", tone: "busy" };
    case "running":
      return { text: "Running on the subscription route…", tone: "busy" };
    case "succeeded":
      return { text: "Completed", tone: "ok" };
    case "failed":
      return {
        text: `Failed (${job.error_code ?? "unknown"}) — nothing was changed`,
        tone: "bad",
      };
    case "uncertain":
      return {
        text:
          job.error_code === "lease_expired"
            ? "Uncertain — the runner ran out of time; the provider may have been called. Nothing is replayed automatically."
            : `Uncertain (${job.error_code}) — the provider call could not be confirmed. Nothing is replayed automatically.`,
        tone: "warn",
      };
    default:
      return { text: "Cancelled", tone: "warn" };
  }
}

function Field({
  label,
  hint,
  children,
}: {
  label: string;
  hint?: string;
  children: ReactNode;
}) {
  return (
    <label>
      {label}
      {hint && <small className="field-hint">{hint}</small>}
      {children}
    </label>
  );
}

function CandidateList({
  detail,
  kind,
  selectedId,
  busy,
  onSelect,
  onArchive,
}: {
  detail: WorkspaceDetail;
  kind: GenerationKind;
  selectedId: string | null;
  busy: boolean;
  onSelect: (candidateId: string) => void;
  onArchive: (candidateId: string, archived: boolean) => void;
}) {
  const jobsForKind = detail.jobs.filter((job) => job.kind === kind);
  const candidates = detail.candidates.filter((c) => c.kind === kind);
  const active = candidates.filter((c) => !c.archived_at);
  const archived = candidates.filter((c) => c.archived_at);
  const jobById = new Map(detail.jobs.map((job) => [job.id, job]));
  const limit = KIND_LIMITS[kind];
  return (
    <div className="candidate-area">
      {!active.length && !archived.length && (
        <p className="empty-note">
          No {kind.replace("_", " ")} yet. Generate candidates above.
        </p>
      )}
      {active.length > 0 && (
        <ul className="candidate-list">
          {active.map((candidate) => {
            const job = jobById.get(candidate.job_id);
            const selected = selectedId === candidate.id;
            return (
              <li
                key={candidate.id}
                className={`candidate${selected ? " selected" : ""}${
                  candidate.value.length > limit ? " over-limit" : ""
                }`}
              >
                <label className="candidate-main">
                  <input
                    type="radio"
                    name={`select-${kind}`}
                    checked={selected}
                    disabled={busy}
                    onChange={() => onSelect(candidate.id)}
                  />
                  <span className="candidate-value">{candidate.value}</span>
                </label>
                <div className="candidate-meta">
                  <small>
                    {candidate.value.length}/{limit} ·{" "}
                    {job?.actual_model ?? "model not recorded"} ·{" "}
                    {new Date(candidate.created_at).toLocaleString()}
                  </small>
                  <button
                    type="button"
                    className="quiet"
                    disabled={busy || selected}
                    title={
                      selected
                        ? "Deselect this candidate before archiving it."
                        : "Recoverable: archived candidates stay stored and can be restored."
                    }
                    onClick={() => onArchive(candidate.id, true)}
                  >
                    Archive
                  </button>
                </div>
              </li>
            );
          })}
        </ul>
      )}
      {archived.length > 0 && (
        <details className="archived-candidates">
          <summary>Archived (recoverable) · {archived.length}</summary>
          <ul className="candidate-list archived">
            {archived.map((candidate) => (
              <li key={candidate.id} className="candidate">
                <span className="candidate-value">{candidate.value}</span>
                <button
                  type="button"
                  className="quiet"
                  disabled={busy}
                  onClick={() => onArchive(candidate.id, false)}
                >
                  Restore
                </button>
              </li>
            ))}
          </ul>
        </details>
      )}
      {jobsForKind.length > 0 && (
        <div className="job-trail">
          {jobsForKind.slice(0, 5).map((job) => {
            const status = statusLabel(job);
            return (
              <p key={job.id} className={`job-line tone-${status.tone}`}>
                <span className={`status-dot ${status.tone}`} aria-hidden>
                  ●
                </span>{" "}
                {status.text} · {new Date(job.created_at).toLocaleTimeString()}
                {job.status === "queued" && (
                  <button
                    type="button"
                    className="quiet"
                    disabled={busy}
                    data-cancel-job={job.id}
                    onClick={(event) => {
                      void api(
                        `/admin/lab/workspaces/${detail.workspace.article_id}/jobs`,
                        "PATCH",
                        { id: job.id, action: "cancel" },
                      );
                      event.currentTarget.disabled = true;
                    }}
                  >
                    Cancel
                  </button>
                )}
              </p>
            );
          })}
        </div>
      )}
    </div>
  );
}

function GenerationControls({
  kind,
  settings,
  routes,
  busy,
  onChange,
  onGenerate,
}: {
  kind: GenerationKind;
  settings: { resolved_prompt: string; route_id: string; count: number };
  routes: GenerationRoute[];
  busy: boolean;
  onChange: (next: {
    resolved_prompt: string;
    route_id: string;
    count: number;
  }) => void;
  onGenerate: () => void;
}) {
  const [showPrompt, setShowPrompt] = useState(false);
  return (
    <div className="generation-controls">
      <div className="generation-row">
        <Field label="Route">
          <select
            value={settings.route_id}
            disabled={busy}
            onChange={(event) =>
              onChange({ ...settings, route_id: event.target.value })
            }
          >
            {routes.map((route) => (
              <option key={route.id} value={route.id}>
                {route.label} · {route.model}
              </option>
            ))}
          </select>
        </Field>
        <Field label="Candidates">
          <input
            type="number"
            min={1}
            max={kind === "outline" ? 1 : 25}
            value={settings.count}
            disabled={busy || kind === "outline"}
            onChange={(event) =>
              onChange({ ...settings, count: Number(event.target.value) })
            }
          />
        </Field>
        <button
          type="button"
          disabled={
            busy || !settings.route_id || !settings.resolved_prompt.trim()
          }
          onClick={onGenerate}
        >
          {busy ? "Working…" : "Generate"}
        </button>
      </div>
      <div className="prompt-editor">
        <button
          type="button"
          className="quiet"
          onClick={() => setShowPrompt((value) => !value)}
        >
          {showPrompt ? "Hide" : "Edit"} resolved prompt
        </button>
        {showPrompt && (
          <>
            <textarea
              value={settings.resolved_prompt}
              rows={6}
              disabled={busy}
              onChange={(event) =>
                onChange({ ...settings, resolved_prompt: event.target.value })
              }
            />
            <button
              type="button"
              className="quiet"
              disabled={busy}
              onClick={() =>
                onChange({
                  ...settings,
                  resolved_prompt: DEFAULT_PROMPTS[kind],
                })
              }
            >
              Reset to default prompt
            </button>
          </>
        )}
      </div>
    </div>
  );
}

export function GenerationWorkspace({
  onDirtyChange,
}: {
  onDirtyChange: (dirty: boolean) => void;
}) {
  const [catalog, setCatalog] = useState<LabCatalog | null>(null);
  const [workspaces, setWorkspaces] = useState<
    {
      article_id: string;
      topic: string;
      revision: number;
      updated_at: string;
    }[]
  >([]);
  const [articleId, setArticleId] = useState<string | null>(null);
  const [detail, setDetail] = useState<WorkspaceDetail | null>(null);
  const [form, setForm] = useState<Form>({
    topic: "",
    brief: "",
    evidence: "",
  });
  const [savedSnapshot, setSavedSnapshot] = useState(
      JSON.stringify([{ topic: "", brief: "", evidence: "" }, {}]),
    ),
    [savedRevision, setSavedRevision] = useState(0),
    [savedDraft, setSavedDraft] = useState("");
  const [prompts, setPrompts] = useState<PromptSettings>({});
  const [saveState, setSaveState] = useState("No unsaved workspace changes");
  const [notice, setNotice] = useState("");
  const [error, setError] = useState("");
  const [busy, setBusy] = useState(false);
  const [newTopic, setNewTopic] = useState("");
  const [imageDraft, setImageDraft] = useState({
    prompt: "",
    model: "",
    size: "",
    quality: "",
  });
  const [pendingJobs, setPendingJobs] = useState<Record<string, string>>({});
  const [publish, setPublish] = useState({ title: "", subtitle: "" });
  const [draftMarkdown, setDraftMarkdown] = useState<string | null>(null);
  const [historyKey, setHistoryKey] = useState(0);
  const latestForm = useRef(form),
    latestPrompts = useRef(prompts),
    latestDetail = useRef(detail);
  latestForm.current = form;
  latestPrompts.current = prompts;
  latestDetail.current = detail;
  const dirty = JSON.stringify([form, prompts]) !== savedSnapshot;
  useEffect(() => {
    onDirtyChange(dirty);
  }, [dirty, onDirtyChange]);
  useEffect(() => {
    if (!dirty) return;
    const warn = (event: BeforeUnloadEvent) => {
      event.preventDefault();
      event.returnValue = "";
    };
    window.addEventListener("beforeunload", warn);
    return () => window.removeEventListener("beforeunload", warn);
  }, [dirty]);

  const settingsFor = useCallback(
    (kind: GenerationKind) => {
      const saved = prompts[kind];
      return {
        resolved_prompt: saved?.resolved_prompt ?? DEFAULT_PROMPTS[kind],
        route_id: saved?.route_id ?? catalog?.routes[0]?.id ?? "",
        count:
          saved?.count ??
          (kind === "outline"
            ? 1
            : kind === "titles"
              ? 12
              : kind === "subtitles"
                ? 4
                : 3),
      };
    },
    [prompts, catalog],
  );

  const loadCatalog = useCallback(async () => {
    setCatalog(await api<LabCatalog>("/admin/lab/routes"));
  }, []);
  const loadWorkspaces = useCallback(async () => {
    setWorkspaces(await api("/admin/lab/workspaces"));
  }, []);
  useEffect(() => {
    void loadCatalog().catch((e) => setError((e as Error).message));
    void loadWorkspaces().catch((e) => setError((e as Error).message));
  }, [loadCatalog, loadWorkspaces]);

  const applyDetail = useCallback((next: WorkspaceDetail) => {
    setDetail(next);
    const workspace = next.workspace;
    setDetail(next);
    return workspace;
  }, []);

  const openWorkspace = useCallback(async (id: string) => {
    setError("");
    setNotice("");
    try {
      const next = await api<WorkspaceDetail>(`/admin/lab/workspaces/${id}`);
      const workspace = next.workspace;
      setArticleId(id);
      setDetail(next);
      setForm({
        topic: workspace.topic,
        brief: workspace.brief,
        evidence: workspace.evidence,
      });
      setSavedRevision(workspace.revision);
      setSavedDraft(workspace.draft_body);
      setDraftMarkdown(workspace.draft_body);
      try {
        setPrompts(
          JSON.parse(workspace.prompt_settings || "{}") as PromptSettings,
        );
      } catch {
        setPrompts({});
      }
      setSavedSnapshot(
        JSON.stringify([
          {
            topic: workspace.topic,
            brief: workspace.brief,
            evidence: workspace.evidence,
          },
          JSON.parse(workspace.prompt_settings || "{}"),
        ]),
      );
      setSaveState("Loaded saved workspace");
      setPublish({ title: "", subtitle: "" });
      setPendingJobs({});
    } catch (e) {
      setError((e as Error).message);
    }
  }, []);

  // Poll only while jobs are active. Polling never touches unsaved form text.
  useEffect(() => {
    if (!articleId || !detail) return;
    const active = [...detail.jobs, ...detail.image_jobs].some((job) =>
      ["queued", "running"].includes(job.status),
    );
    if (!active) return;
    const timer = setInterval(async () => {
      try {
        const next = await api<WorkspaceDetail>(
          `/admin/lab/workspaces/${articleId}`,
        );
        setDetail(next);
        if (!Object.values(next.image_jobs).some(() => false)) {
          const completed = next.image_jobs.filter(
            (job) => job.status === "succeeded",
          );
          if (completed.length) setImageDraft((draft) => ({ ...draft }));
        }
      } catch {
        // Transient polling errors keep local state; the next tick retries.
      }
    }, 5000);
    return () => clearInterval(timer);
  }, [articleId, detail]);

  async function createWorkspace() {
    if (!newTopic.trim()) return;
    setBusy(true);
    setError("");
    try {
      const created = await api<{ article_id: string }>(
        "/admin/lab/workspaces",
        "POST",
        {
          topic: newTopic.trim(),
        },
      );
      setNewTopic("");
      await loadWorkspaces();
      await openWorkspace(created.article_id);
      setNotice(
        "Workspace created. Fill in the brief and evidence, then save.",
      );
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }

  async function saveWorkspace() {
    if (!articleId) return;
    setBusy(true);
    setError("");
    try {
      const result = await api<{ revision: number }>(
        `/admin/lab/workspaces/${articleId}`,
        "PUT",
        {
          revision: savedRevision,
          topic: form.topic,
          brief: form.brief,
          evidence: form.evidence,
          draft_body: draftMarkdown ?? savedDraft,
          prompt_settings: prompts,
        },
      );
      setSavedRevision(result.revision);
      setSavedDraft(draftMarkdown ?? savedDraft);
      setSavedSnapshot(JSON.stringify([form, prompts]));
      setSaveState("All changes saved");
      setNotice("");
      await loadWorkspaces();
    } catch (e) {
      if (e instanceof ApiError && e.status === 409)
        setError(
          "The workspace changed elsewhere since you loaded it. Your edits are still shown below — copy anything important, then click “Load saved version” to refresh.",
        );
      else setError((e as Error).message);
      setSaveState("Not saved");
    } finally {
      setBusy(false);
    }
  }

  function reloadSaved() {
    if (!detail) return;
    const workspace = detail.workspace;
    setForm({
      topic: workspace.topic,
      brief: workspace.brief,
      evidence: workspace.evidence,
    });
    setDraftMarkdown(workspace.draft_body);
    setSavedDraft(workspace.draft_body);
    setSavedRevision(workspace.revision);
    let parsed: PromptSettings = {};
    try {
      parsed = JSON.parse(workspace.prompt_settings || "{}") as PromptSettings;
    } catch {
      parsed = {};
    }
    setPrompts(parsed);
    setSavedSnapshot(
      JSON.stringify([
        {
          topic: workspace.topic,
          brief: workspace.brief,
          evidence: workspace.evidence,
        },
        parsed,
      ]),
    );
    setSaveState("Loaded saved workspace");
    setError("");
  }

  async function generate(kind: GenerationKind) {
    if (!articleId) return;
    const settings = settingsFor(kind);
    // One stable UUID per enqueue attempt; a transport retry reuses it.
    const jobId =
      pendingJobs[kind] ??
      (() => {
        const id = crypto.randomUUID();
        setPendingJobs((current) => ({ ...current, [kind]: id }));
        return id;
      })();
    setBusy(true);
    setError("");
    setNotice("");
    try {
      await api(`/admin/lab/workspaces/${articleId}/jobs`, "POST", {
        id: jobId,
        kind,
        count: settings.count,
        resolved_prompt: settings.resolved_prompt,
        route_id: settings.route_id,
      });
      // Admitted: the next Generate is a deliberate new request with a new ID.
      setPendingJobs(({ [kind]: _used, ...rest }) => rest);
      setNotice(
        kind === "outline"
          ? "Outline generation queued."
          : `${settings.count} ${KINDS.find((k) => k.key === kind)?.countLabel.toLowerCase()} queued.`,
      );
      // Refresh from the queue immediately instead of an optimistic insert:
      // the job list must reflect server truth (a duplicate POST admits nothing).
      setDetail(
        await api<WorkspaceDetail>(`/admin/lab/workspaces/${articleId}`),
      );
    } catch (e) {
      setError(
        `${(e as Error).message} Nothing was queued. “Generate” will retry this exact request.`,
      );
    } finally {
      setBusy(false);
    }
  }

  async function selectCandidate(kind: GenerationKind, candidateId: string) {
    if (!articleId || !detail) return;
    setBusy(true);
    setError("");
    try {
      const result = await api<{ revision: number }>(
        `/admin/lab/workspaces/${articleId}/selections`,
        "PUT",
        { candidate_id: candidateId, revision: detail.workspace.revision },
      );
      setSavedRevision(result.revision);
      const next = await api<WorkspaceDetail>(
        `/admin/lab/workspaces/${articleId}`,
      );
      applyDetail(next);
      setNotice("Selection saved.");
    } catch (e) {
      if (e instanceof ApiError && e.status === 409)
        setError(
          "The workspace changed while selecting. Your text edits are safe — press “Load saved version”, then select again.",
        );
      else setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }

  async function archiveCandidate(candidateId: string, archived: boolean) {
    if (!articleId) return;
    setBusy(true);
    setError("");
    try {
      await api(`/admin/lab/workspaces/${articleId}/candidates`, "PATCH", {
        candidate_id: candidateId,
        archived,
      });
      const next = await api<WorkspaceDetail>(
        `/admin/lab/workspaces/${articleId}`,
      );
      applyDetail(next);
      setNotice(
        archived
          ? "Archived. Restore it any time from the archived list."
          : "Restored.",
      );
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }

  const selected = useMemo(() => {
    const map = new Map<GenerationKind, GenerationCandidate>();
    detail?.selections.forEach((selection) => {
      const candidate = detail.candidates.find(
        (c) => c.id === selection.candidate_id,
      );
      if (candidate) map.set(selection.kind, candidate);
    });
    return map;
  }, [detail]);

  const selectedAsset = useMemo(
    () =>
      detail?.thumbnail
        ? detail.image_assets.find(
            (a) => a.id === detail.thumbnail?.image_asset_id,
          )
        : undefined,
    [detail],
  );

  useEffect(() => {
    if (!catalog) return;
    setImageDraft((draft) => ({
      ...draft,
      model: draft.model || catalog.images.models[0] || "",
      size: draft.size || catalog.images.sizes[0] || "",
      quality: draft.quality || catalog.images.qualities[0] || "",
    }));
  }, [catalog]);

  async function generateImage() {
    if (!articleId) return;
    setBusy(true);
    setError("");
    setNotice("");
    try {
      await api(`/admin/lab/workspaces/${articleId}/image-jobs`, "POST", {
        id: crypto.randomUUID(),
        prompt: imageDraft.prompt,
        model: imageDraft.model,
        size: imageDraft.size,
        quality: imageDraft.quality,
      });
      setNotice(
        "Image generation queued. This uses the separately billed OpenAI Image API.",
      );
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }

  async function uploadImage(file: File, altText: string, caption: string) {
    if (!articleId) return;
    setBusy(true);
    setError("");
    try {
      const form_ = new FormData();
      form_.append("file", file);
      form_.append("alt_text", altText);
      form_.append("caption", caption);
      // The browser supplies Origin itself on same-origin POSTs.
      const response = await fetch(
        `/api/admin/lab/workspaces/${articleId}/image-upload`,
        { method: "POST", body: form_ },
      );
      const result = (await response.json()) as { error?: string };
      if (!response.ok) throw new Error(result.error ?? "Upload failed.");
      const next = await api<WorkspaceDetail>(
        `/admin/lab/workspaces/${articleId}`,
      );
      applyDetail(next);
      setNotice("Image uploaded. Select it to use it as the draft thumbnail.");
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }

  async function selectThumbnail(assetId: string) {
    if (!articleId || !detail) return;
    setBusy(true);
    setError("");
    try {
      const result = await api<{ revision: number }>(
        `/admin/lab/workspaces/${articleId}/thumbnail`,
        "PUT",
        { image_asset_id: assetId, revision: detail.workspace.revision },
      );
      setSavedRevision(result.revision);
      const next = await api<WorkspaceDetail>(
        `/admin/lab/workspaces/${articleId}`,
      );
      applyDetail(next);
      setNotice("Draft thumbnail saved.");
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }

  async function saveAssetMeta(
    asset: ImageAsset,
    altText: string,
    caption: string,
  ) {
    if (!articleId) return;
    setBusy(true);
    setError("");
    try {
      await api(`/admin/lab/workspaces/${articleId}/image-assets`, "PATCH", {
        image_asset_id: asset.id,
        alt_text: altText,
        caption,
      });
      const next = await api<WorkspaceDetail>(
        `/admin/lab/workspaces/${articleId}`,
      );
      applyDetail(next);
      setNotice("Image details saved.");
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }

  function exportContext(): string {
    const workspace: WorkspaceRow | undefined = detail?.workspace;
    if (!workspace) return "";
    const lines = [
      `# Writing brief — ${workspace.topic}`,
      "",
      "## Core brief",
      workspace.brief || "(no brief saved)",
      "",
      "## Evidence & notes",
      workspace.evidence || "(no evidence saved)",
      "",
    ];
    const title = selected.get("titles")?.value;
    const subtitle = selected.get("subtitles")?.value;
    const concept = selected.get("thumbnail_concepts")?.value;
    const outline = selected.get("outline")?.value;
    if (title) lines.push("## Selected title", title, "");
    if (subtitle) lines.push("## Selected subtitle", subtitle, "");
    if (concept) lines.push("## Selected thumbnail concept", concept, "");
    if (outline) lines.push("## Selected outline", outline, "");
    lines.push(
      "## Task",
      "Write the complete Markdown article from this material. Credible, beginner-friendly personal finance. Return only the article Markdown.",
    );
    return lines.join("\n");
  }

  function copyContext() {
    const text = exportContext();
    void navigator.clipboard
      .writeText(text)
      .then(() =>
        setNotice(
          "Context copied. Paste it into ChatGPT Pro to draft the article.",
        ),
      )
      .catch(() => {
        const blob = new Blob([text], { type: "text/plain" });
        const url = URL.createObjectURL(blob);
        const a = document.createElement("a");
        a.href = url;
        a.download = "article-context.md";
        a.click();
        URL.revokeObjectURL(url);
        setNotice(
          "Clipboard unavailable — downloaded article-context.md instead.",
        );
      });
  }

  async function publishVersion() {
    if (!articleId || !detail) return;
    setBusy(true);
    setError("");
    try {
      const body = draftMarkdown ?? "";
      await api("/admin/versions", "POST", {
        article_id: articleId,
        title: publish.title,
        subtitle: publish.subtitle,
        body,
        ...(detail.thumbnail
          ? { thumbnail_asset_id: detail.thumbnail.image_asset_id }
          : {}),
      });
      setNotice(
        "Immutable version published. Assign reviewers from the Admin tab.",
      );
      await loadWorkspaces();
      setHistoryKey((key) => key + 1);
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }

  const jobsActive =
    detail?.jobs.some((job) => ["queued", "running"].includes(job.status)) ||
    detail?.image_jobs.some((job) =>
      ["queued", "running"].includes(job.status),
    );

  return (
    <>
      <div className="page-heading">
        <h1>Writing workspace</h1>
        <p>
          Brief, candidates and drafts per article. Selections persist;
          generated alternatives are never silently replaced.
        </p>
      </div>
      {error && (
        <div className="error" role="alert">
          {error}
        </div>
      )}
      {notice && (
        <div className="success" role="status">
          {notice}
        </div>
      )}
      <section className="panel">
        <h2>Workspace</h2>
        <div className="workspace-picker">
          <Field label="Open a workspace">
            <select
              value={articleId ?? ""}
              disabled={busy}
              onChange={(event) => {
                if (
                  dirty &&
                  !window.confirm("Discard unsaved workspace edits?")
                ) {
                  event.preventDefault();
                  return;
                }
                if (event.target.value) void openWorkspace(event.target.value);
                else setArticleId(null);
              }}
            >
              <option value="">Choose…</option>
              {workspaces.map((workspace) => (
                <option key={workspace.article_id} value={workspace.article_id}>
                  {workspace.topic || "(untitled)"} · updated{" "}
                  {new Date(workspace.updated_at).toLocaleDateString()}
                </option>
              ))}
            </select>
          </Field>
          <Field
            label="Or start a new one"
            hint="Creates a fresh article project."
          >
            <input
              value={newTopic}
              placeholder="Working title"
              maxLength={250}
              disabled={busy}
              onChange={(event) => setNewTopic(event.target.value)}
            />
          </Field>
          <button
            type="button"
            disabled={busy || !newTopic.trim()}
            onClick={() => void createWorkspace()}
          >
            Create workspace
          </button>
        </div>
        {catalog && (
          <p className="hint">
            Routes:{" "}
            {catalog.routes.map((route) => route.label).join(", ") ||
              "none configured"}
            .{" "}
            {catalog.runner_configured
              ? catalog.runners.length
                ? `Runner “${catalog.runners[0].runner_id}” last seen ${new Date(catalog.runners[0].last_seen).toLocaleString()}.`
                : "Runner token configured, but no runner has checked in yet."
              : "Runner not configured — generation is disabled."}{" "}
            {catalog.images_configured
              ? `Images: ${catalog.images.models.join(", ")}.`
              : "Image generation not configured; uploads still work."}
          </p>
        )}
      </section>

      {articleId && detail && (
        <>
          <section className="panel">
            <div className="panel-heading-row">
              <h2>Brief & evidence</h2>
              <div className="save-row">
                <span aria-live="polite">{saveState}</span>
                {dirty && (
                  <button
                    type="button"
                    className="quiet"
                    disabled={busy}
                    onClick={reloadSaved}
                  >
                    Load saved version
                  </button>
                )}
                <button
                  type="button"
                  disabled={busy}
                  onClick={() => void saveWorkspace()}
                >
                  Save workspace
                </button>
              </div>
            </div>
            <Field label="Working topic">
              <input
                value={form.topic}
                maxLength={250}
                disabled={busy}
                onChange={(event) =>
                  setForm({ ...form, topic: event.target.value })
                }
              />
            </Field>
            <Field
              label="Core brief"
              hint="Angle, audience, promise. Shipped to ChatGPT Pro with the context export."
            >
              <textarea
                value={form.brief}
                rows={5}
                disabled={busy}
                onChange={(event) =>
                  setForm({ ...form, brief: event.target.value })
                }
              />
            </Field>
            <Field
              label="Evidence & notes"
              hint="Facts, sources, caveats the model must respect."
            >
              <textarea
                value={form.evidence}
                rows={7}
                disabled={busy}
                onChange={(event) =>
                  setForm({ ...form, evidence: event.target.value })
                }
              />
            </Field>
          </section>

          {KINDS.map(({ key, heading, blurb, countLabel }) => {
            const settings = settingsFor(key);
            const latestJob = detail.jobs.find((job) => job.kind === key);
            const running =
              latestJob && ["queued", "running"].includes(latestJob.status);
            return (
              <section className="panel" key={key}>
                <h2>{heading}</h2>
                <p className="hint">{blurb}</p>
                <GenerationControls
                  kind={key}
                  settings={settings}
                  routes={catalog?.routes ?? []}
                  busy={busy || Boolean(running)}
                  onChange={(next) =>
                    setPrompts((current) => ({ ...current, [key]: next }))
                  }
                  onGenerate={() => void generate(key)}
                />
                <CandidateList
                  detail={detail}
                  kind={key}
                  selectedId={selected.get(key)?.id ?? null}
                  busy={busy}
                  onSelect={(candidateId) =>
                    void selectCandidate(key, candidateId)
                  }
                  onArchive={(candidateId, archived) =>
                    void archiveCandidate(candidateId, archived)
                  }
                />
                {selected.get(key) && (
                  <p className="selected-note">
                    Selected: “{selected.get(key)!.value.slice(0, 120)}
                    {selected.get(key)!.value.length > 120 ? "…" : ""}”
                  </p>
                )}
                <small className="count-note">
                  {
                    detail.candidates.filter(
                      (c) => c.kind === key && !c.archived_at,
                    ).length
                  }{" "}
                  active {countLabel.toLowerCase()}
                </small>
              </section>
            );
          })}

          <section className="panel">
            <h2>5 · Thumbnail image</h2>
            <p className="hint">
              Actual pixels use the separately billed OpenAI Image API, or
              upload your own. All alternatives are kept.
            </p>
            {catalog?.images_configured ? (
              <div className="image-controls">
                <Field
                  label="Image prompt"
                  hint="Derived from the selected concept; edit freely."
                >
                  <textarea
                    rows={3}
                    value={imageDraft.prompt}
                    disabled={busy}
                    onChange={(event) =>
                      setImageDraft({
                        ...imageDraft,
                        prompt: event.target.value,
                      })
                    }
                  />
                </Field>
                <div className="generation-row">
                  <Field label="Model">
                    <select
                      value={imageDraft.model}
                      disabled={busy}
                      onChange={(event) =>
                        setImageDraft({
                          ...imageDraft,
                          model: event.target.value,
                        })
                      }
                    >
                      {catalog.images.models.map((model) => (
                        <option key={model} value={model}>
                          {model}
                        </option>
                      ))}
                    </select>
                  </Field>
                  <Field label="Size">
                    <select
                      value={imageDraft.size}
                      disabled={busy}
                      onChange={(event) =>
                        setImageDraft({
                          ...imageDraft,
                          size: event.target.value,
                        })
                      }
                    >
                      {catalog.images.sizes.map((size) => (
                        <option key={size} value={size}>
                          {size}
                        </option>
                      ))}
                    </select>
                  </Field>
                  <Field label="Quality">
                    <select
                      value={imageDraft.quality}
                      disabled={busy}
                      onChange={(event) =>
                        setImageDraft({
                          ...imageDraft,
                          quality: event.target.value,
                        })
                      }
                    >
                      {catalog.images.qualities.map((quality) => (
                        <option key={quality} value={quality}>
                          {quality}
                        </option>
                      ))}
                    </select>
                  </Field>
                  <button
                    type="button"
                    className="billed"
                    disabled={busy || !imageDraft.prompt.trim()}
                    onClick={() => void generateImage()}
                    title="One image per request; billed to the OpenAI API account."
                  >
                    Generate image (paid API)
                  </button>
                </div>
                {detail.image_jobs.slice(0, 4).map((job: ImageJobView) => {
                  const status = statusLabel(job);
                  return (
                    <p key={job.id} className={`job-line tone-${status.tone}`}>
                      <span className={`status-dot ${status.tone}`} aria-hidden>
                        ●
                      </span>{" "}
                      Image job {job.status}
                      {job.error_code ? ` (${job.error_code})` : ""} ·{" "}
                      {job.model} {job.size} ·{" "}
                      {new Date(job.created_at).toLocaleTimeString()}
                      {job.status === "queued" && (
                        <button
                          type="button"
                          className="quiet"
                          disabled={busy}
                          onClick={async () => {
                            try {
                              await api(
                                `/admin/lab/workspaces/${articleId}/image-jobs`,
                                "PATCH",
                                {
                                  id: job.id,
                                  action: "cancel",
                                },
                              );
                            } catch (e) {
                              setError((e as Error).message);
                            }
                          }}
                        >
                          Cancel
                        </button>
                      )}
                    </p>
                  );
                })}
              </div>
            ) : (
              <p className="hint">
                Image generation is not configured on the server.
              </p>
            )}
            <div className="upload-row">
              <Field
                label="Upload an image"
                hint="PNG, JPEG or WebP up to 10 MB."
              >
                <input
                  type="file"
                  accept="image/png,image/jpeg,image/webp"
                  disabled={busy}
                  onChange={(event) => {
                    const file = event.currentTarget.files?.[0];
                    event.currentTarget.value = "";
                    if (file)
                      void uploadImage(
                        file,
                        selected.get("titles")?.value
                          ? `Article image for ${selected.get("titles")!.value}`
                          : "",
                        "",
                      );
                  }}
                />
              </Field>
            </div>
            <div className="asset-grid">
              {detail.image_assets
                .filter((asset) => !asset.archived_at)
                .map((asset) => (
                  <figure
                    key={asset.id}
                    className={`asset-card${detail.thumbnail?.image_asset_id === asset.id ? " selected" : ""}`}
                  >
                    <img
                      src={`/api/assets/${asset.id}`}
                      alt={asset.alt_text || "Article image candidate"}
                      loading="lazy"
                    />
                    <figcaption>
                      <small>
                        {asset.width}×{asset.height} · {asset.source}
                        {asset.model ? ` · ${asset.model}` : ""}
                      </small>
                      <AssetMeta
                        asset={asset}
                        busy={busy}
                        onSave={saveAssetMeta}
                      />
                      <div className="actions">
                        <button
                          type="button"
                          disabled={
                            busy ||
                            detail.thumbnail?.image_asset_id === asset.id
                          }
                          onClick={() => void selectThumbnail(asset.id)}
                        >
                          {detail.thumbnail?.image_asset_id === asset.id
                            ? "Draft thumbnail ✓"
                            : "Use as draft thumbnail"}
                        </button>
                        {detail.thumbnail?.image_asset_id !== asset.id && (
                          <button
                            type="button"
                            className="quiet"
                            disabled={busy}
                            onClick={() =>
                              void archiveCandidate(asset.id, true).then(
                                () => undefined,
                              )
                            }
                          >
                            Archive
                          </button>
                        )}
                      </div>
                    </figcaption>
                  </figure>
                ))}
            </div>
            {detail.image_assets.some((asset) => asset.archived_at) && (
              <details className="archived-candidates">
                <summary>Archived images (recoverable)</summary>
                <div className="asset-grid">
                  {detail.image_assets
                    .filter((asset) => asset.archived_at)
                    .map((asset) => (
                      <figure key={asset.id} className="asset-card archived">
                        <img
                          src={`/api/assets/${asset.id}`}
                          alt={asset.alt_text || "Archived image"}
                          loading="lazy"
                        />
                        <figcaption>
                          <button
                            type="button"
                            className="quiet"
                            disabled={busy}
                            onClick={() => {
                              void api(
                                `/admin/lab/workspaces/${articleId}/image-assets`,
                                "PATCH",
                                {
                                  image_asset_id: asset.id,
                                  archived: false,
                                },
                              )
                                .then(() =>
                                  api<WorkspaceDetail>(
                                    `/admin/lab/workspaces/${articleId}`,
                                  ),
                                )
                                .then((next) => applyDetail(next))
                                .catch((e) => setError((e as Error).message));
                            }}
                          >
                            Restore
                          </button>
                        </figcaption>
                      </figure>
                    ))}
                </div>
              </details>
            )}
          </section>

          <section className="panel">
            <h2>6 · Draft with ChatGPT Pro</h2>
            <p className="hint">
              Export the saved context, draft the full article in ChatGPT Pro
              (unchanged), then import or paste the Markdown back here.
            </p>
            <div className="actions">
              <button
                type="button"
                className="quiet"
                disabled={busy}
                onClick={copyContext}
              >
                Copy writing context
              </button>
            </div>
            <MarkdownComposer
              key={`draft-${articleId}-${savedRevision}-${draftMarkdown === null}`}
              disabled={busy}
              initialMarkdown={detail.workspace.draft_body}
              heading="Working draft"
              onValidityChange={() => undefined}
              onDirtyChange={() => undefined}
              onMarkdownChange={setDraftMarkdown}
            />
            <div className="save-row">
              <button
                type="button"
                disabled={busy}
                onClick={() => void saveWorkspace()}
              >
                Save draft with workspace
              </button>
            </div>
          </section>

          <section className="panel">
            <h2>7 · Publish an immutable version</h2>
            <p className="hint">
              Freezes title, subtitle, rendered body and referenced images for
              review. Existing versions are never modified.
            </p>
            <div className="publish-form">
              <Field label="Title">
                <input
                  value={publish.title}
                  maxLength={250}
                  disabled={busy}
                  onChange={(event) =>
                    setPublish({ ...publish, title: event.target.value })
                  }
                />
              </Field>
              <button
                type="button"
                className="quiet"
                disabled={busy || !selected.get("titles")}
                onClick={() =>
                  setPublish({
                    ...publish,
                    title: selected.get("titles")!.value,
                    ...(selected.get("subtitles")
                      ? { subtitle: selected.get("subtitles")!.value }
                      : {}),
                  })
                }
              >
                Use selected title
                {selected.get("subtitles") ? " & subtitle" : ""}
              </button>
              <Field label="Subtitle">
                <input
                  value={publish.subtitle}
                  maxLength={500}
                  disabled={busy}
                  onChange={(event) =>
                    setPublish({ ...publish, subtitle: event.target.value })
                  }
                />
              </Field>
              {selectedAsset && (
                <p className="hint">
                  Draft thumbnail:{" "}
                  {selectedAsset.alt_text || "(no alt text yet)"} — it is frozen
                  with the version and referenced as{" "}
                  <code>/api/assets/{selectedAsset.id}</code>.
                </p>
              )}
              <button
                type="button"
                disabled={
                  busy || !publish.title.trim() || !(draftMarkdown ?? "").trim()
                }
                onClick={() => void publishVersion()}
              >
                Publish version
              </button>
            </div>
          </section>

          <section className="panel">
            <h2>Publication history</h2>
            <PublicationHistory articleId={articleId} refreshKey={historyKey} />
          </section>

          {jobsActive && (
            <p className="hint" aria-live="polite">
              Generation jobs are active — this page polls every 5 seconds and
              never overwrites your unsaved text.
            </p>
          )}
        </>
      )}
    </>
  );
}

function AssetMeta({
  asset,
  busy,
  onSave,
}: {
  asset: ImageAsset;
  busy: boolean;
  onSave: (asset: ImageAsset, alt: string, caption: string) => Promise<void>;
}) {
  const [alt, setAlt] = useState(asset.alt_text);
  const [caption, setCaption] = useState(asset.caption);
  return (
    <div className="asset-meta">
      <input
        value={alt}
        placeholder="Alt text"
        maxLength={1000}
        disabled={busy}
        onChange={(event) => setAlt(event.target.value)}
      />
      <input
        value={caption}
        placeholder="Caption (optional)"
        maxLength={2000}
        disabled={busy}
        onChange={(event) => setCaption(event.target.value)}
      />
      <button
        type="button"
        className="quiet"
        disabled={busy || (alt === asset.alt_text && caption === asset.caption)}
        onClick={() => void onSave(asset, alt, caption)}
      >
        Save details
      </button>
    </div>
  );
}

function PublicationHistory({
  articleId,
  refreshKey,
}: {
  articleId: string;
  refreshKey: number;
}) {
  const [versions, setVersions] = useState<
    { id: string; version_number: number; title: string; created_at: string }[]
  >([]);
  useEffect(() => {
    let cancelled = false;
    void api<
      {
        id: string;
        article_id: string;
        version_number: number;
        title: string;
        created_at: string;
      }[]
    >("/admin/versions").then((all) => {
      if (!cancelled)
        setVersions(all.filter((version) => version.article_id === articleId));
    });
    return () => {
      cancelled = true;
    };
  }, [articleId, refreshKey]);
  if (!versions.length)
    return <p>No published versions for this article yet.</p>;
  return (
    <ul className="version-list">
      {versions.map((version) => (
        <li key={version.id}>
          v{version.version_number} · {version.title} ·{" "}
          {new Date(version.created_at).toLocaleString()}
        </li>
      ))}
    </ul>
  );
}
