import {
  Component,
  Suspense,
  lazy,
  useEffect,
  useId,
  useMemo,
  useRef,
  useState,
  type ReactNode,
} from "react";
import { renderMarkdown } from "../../shared/markdown";
import {
  importedMarkdown,
  markdownProblem,
  MAX_IMPORT_BYTES,
  MAX_MARKDOWN_CHARS,
} from "./markdownInput";
import "./editor.css";

const RichMarkdownEditor = lazy(() => import("./RichMarkdownEditor"));
type Mode = "visual" | "source" | "preview";

class EditorBoundary extends Component<
  { children: ReactNode; onFailure: (message: string) => void },
  { failed: boolean }
> {
  state = { failed: false };
  static getDerivedStateFromError() {
    return { failed: true };
  }
  componentDidCatch() {
    this.props.onFailure(
      "The visual editor could not load this draft. Your Markdown is preserved. Use Markdown source or export it before reloading.",
    );
  }
  render() {
    return this.state.failed ? null : this.props.children;
  }
}

export function MarkdownComposer({
  disabled,
  onValidityChange,
  onDirtyChange,
}: {
  disabled: boolean;
  onValidityChange: (valid: boolean) => void;
  onDirtyChange: (dirty: boolean) => void;
}) {
  const [markdown, setMarkdown] = useState("");
  const [mode, setMode] = useState<Mode>("visual");
  const [editorError, setEditorError] = useState("");
  const [importError, setImportError] = useState("");
  const [importing, setImporting] = useState(false);
  const [generation, setGeneration] = useState(0);
  const mounted = useRef(true);
  const id = useId();
  const busy = disabled || importing;
  const hasUnsavedContent = Boolean(markdown.trim());
  const problem = markdownProblem(markdown);
  const valid = !problem && !editorError && !importing;
  // The existing sanitizer, not the browser editor's HTML, defines what friends
  // will review. Comments continue to target that exact immutable rendering.
  const preview = useMemo(
    () => (mode === "preview" ? renderMarkdown(markdown).rendered_html : ""),
    [mode, markdown],
  );
  useEffect(() => {
    onValidityChange(valid);
  }, [valid, onValidityChange]);
  useEffect(() => {
    onDirtyChange(hasUnsavedContent);
  }, [hasUnsavedContent, onDirtyChange]);
  useEffect(() => {
    mounted.current = true;
    return () => {
      mounted.current = false;
    };
  }, []);
  useEffect(() => {
    if (!hasUnsavedContent) return;
    const warn = (event: BeforeUnloadEvent) => {
      event.preventDefault();
      event.returnValue = "";
    };
    window.addEventListener("beforeunload", warn);
    return () => window.removeEventListener("beforeunload", warn);
  }, [hasUnsavedContent]);

  function changeMode(next: Mode) {
    setEditorError("");
    setMode(next);
  }
  async function importFile(file?: File) {
    if (!file) return;
    setImportError("");
    if (file.size > MAX_IMPORT_BYTES) {
      setImportError(
        "This file is too large. Import a shorter Markdown article.",
      );
      return;
    }
    if (
      markdown &&
      !window.confirm(
        "Replace the unsaved draft with this file? Export the current draft first to keep it.",
      )
    )
      return;
    setImporting(true);
    try {
      const text = importedMarkdown(file.name, await file.text());
      if (!mounted.current) return;
      setMarkdown(text);
      setEditorError("");
      setGeneration((n) => n + 1);
      setMode("visual");
    } catch (error) {
      if (mounted.current) setImportError((error as Error).message);
    } finally {
      if (mounted.current) setImporting(false);
    }
  }
  function download() {
    const url = URL.createObjectURL(
      new Blob([markdown], { type: "text/markdown;charset=utf-8" }),
    );
    const a = document.createElement("a");
    a.href = url;
    a.download = "article-draft.md";
    document.body.appendChild(a);
    a.click();
    a.remove();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  }

  return (
    <div className="markdown-composer">
      <div className="composer-heading">
        <h3>Article draft</h3>
        <span>Markdown underneath. No Markdown knowledge required.</span>
      </div>
      <input name="body" type="hidden" value={markdown} />
      <div className="composer-actions">
        <div
          className="composer-modes"
          role="tablist"
          aria-label="Article editing mode"
        >
          {(["visual", "source", "preview"] as const).map((value) => (
            <button
              type="button"
              role="tab"
              id={`${id}-${value}`}
              aria-controls={`${id}-panel`}
              aria-selected={mode === value}
              tabIndex={mode === value ? 0 : -1}
              onKeyDown={(event) => {
                const modes: Mode[] = ["visual", "source", "preview"];
                const index = modes.indexOf(value);
                const next =
                  event.key === "ArrowRight"
                    ? (index + 1) % 3
                    : event.key === "ArrowLeft"
                      ? (index + 2) % 3
                      : event.key === "Home"
                        ? 0
                        : event.key === "End"
                          ? 2
                          : -1;
                if (next < 0) return;
                event.preventDefault();
                changeMode(modes[next]);
                document.getElementById(`${id}-${modes[next]}`)?.focus();
              }}
              disabled={busy}
              onClick={() => changeMode(value)}
              key={value}
            >
              {
                {
                  visual: "Visual editor",
                  source: "Markdown source",
                  preview: "Review preview",
                }[value]
              }
            </button>
          ))}
        </div>
        <label className="composer-import">
          Import .md
          <input
            type="file"
            accept=".md,.markdown,text/markdown,text/plain"
            disabled={busy}
            onChange={(event) => {
              const file = event.currentTarget.files?.[0];
              event.currentTarget.value = "";
              void importFile(file);
            }}
          />
        </label>
        <button type="button" disabled={busy || !markdown} onClick={download}>
          Export .md
        </button>
      </div>
      {(editorError || importError) && (
        <p role="alert" className="error">
          {editorError || importError}
        </p>
      )}
      <div role="tabpanel" id={`${id}-panel`} aria-labelledby={`${id}-${mode}`}>
        {mode === "visual" && (
          <EditorBoundary key={generation} onFailure={setEditorError}>
            <Suspense
              fallback={<p aria-live="polite">Loading the visual editor…</p>}
            >
              <RichMarkdownEditor
                initialMarkdown={markdown}
                readOnly={busy}
                onChange={setMarkdown}
                onError={setEditorError}
              />
            </Suspense>
          </EditorBoundary>
        )}
        {mode === "source" && (
          <label>
            Markdown
            <textarea
              aria-label="Markdown"
              rows={14}
              spellCheck={false}
              value={markdown}
              disabled={busy}
              onChange={(event) => {
                setEditorError("");
                setMarkdown(event.target.value);
              }}
            />
          </label>
        )}
        {mode === "preview" && (
          <>
            <p className="hint">
              This is the sanitized rendering that will be frozen for review.
              Unsupported images, embeds and raw HTML are excluded in this MVP.
            </p>
            <div
              className="prose composer-preview"
              aria-label="Review snapshot preview"
              dangerouslySetInnerHTML={{ __html: preview }}
            />
          </>
        )}
      </div>
      <p className="composer-save-note">
        This draft is not autosaved. Publishing a version saves a private review
        snapshot, not a public Medium article. Export .md before leaving without
        publishing.
      </p>
      {markdown && problem && (
        <p role="alert" className="error">
          {problem}
        </p>
      )}
      <small>
        {markdown.length.toLocaleString("en-US")} /{" "}
        {MAX_MARKDOWN_CHARS.toLocaleString("en-US")} characters
      </small>
    </div>
  );
}
