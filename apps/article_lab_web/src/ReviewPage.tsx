import { useCallback, useState } from "react";
import type { Annotation, ReviewDetail } from "../shared/types";
import { Article } from "./Article";
import { useDraft } from "./useDraft";
export function ReviewPage({
  detail,
  onBack,
}: {
  detail: ReviewDetail;
  onBack: () => void;
}) {
  const { version, review, reviewer } = detail,
    d = useDraft(review);
  const [selection, setSelection] = useState<Omit<
      Annotation,
      "id" | "comment"
    > | null>(null),
    [error, setError] = useState(""),
    [busy, setBusy] = useState(false),
    [confirm, setConfirm] = useState(false);
  const select = useCallback(
    (anchor: Omit<Annotation, "id" | "comment">) => setSelection(anchor),
    [],
  );
  async function action(fn: () => Promise<void>) {
    setBusy(true);
    setError("");
    try {
      await fn();
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }
  const readOnly = d.submitted || !!reviewer;
  return (
    <>
      <div className="toolbar">
        <button
          className="quiet"
          disabled={busy}
          onClick={() =>
            void action(async () => {
              await d.flush();
              onBack();
            })
          }
        >
          ← Back
        </button>
        <span role="status">
          {readOnly ? "Submitted · Read-only" : d.state}
        </span>
      </div>
      {(error || d.error) && (
        <div className="error" role="alert">
          {error || d.error}
          <button onClick={() => void action(d.flush)}>Retry save</button>
          <p>
            If another device changed this draft, copy your unsaved feedback
            before reloading.
          </p>
        </div>
      )}
      <div className="review-heading">
        <h1>{version.title}</h1>
        <p>{version.subtitle}</p>
        <small>
          Version {version.version_number}
          {reviewer ? ` · ${reviewer.display_name} (${reviewer.email})` : ""}
          {review.submitted_at
            ? ` · Submitted ${new Date(review.submitted_at).toLocaleString()}`
            : ""}
        </small>
      </div>
      {!readOnly && (
        <p className="hint">
          Select a passage in the article, then choose “Add comment”. Your
          feedback saves automatically.
        </p>
      )}
      <div className="review-layout">
        <section className="paper">
          <Article
            version={version}
            annotations={d.draft.annotations}
            onSelect={readOnly || busy ? undefined : select}
          />
          <div className="general">
            <label htmlFor="general">General feedback</label>
            <p>What works well? What could make this article better?</p>
            <textarea
              id="general"
              rows={7}
              maxLength={30000}
              readOnly={readOnly}
              disabled={busy}
              value={d.draft.general_feedback}
              onChange={(e) =>
                d.update({ ...d.draft, general_feedback: e.target.value })
              }
            />
          </div>
          {!readOnly && (
            <div className="submit-area">
              {confirm ? (
                <>
                  <p>
                    Submit this review? Your comments will become read-only.
                  </p>
                  <button disabled={busy} onClick={() => void action(d.submit)}>
                    Confirm submission
                  </button>
                  <button
                    className="quiet"
                    disabled={busy}
                    onClick={() => setConfirm(false)}
                  >
                    Keep editing
                  </button>
                </>
              ) : (
                <button disabled={busy} onClick={() => setConfirm(true)}>
                  Submit feedback
                </button>
              )}
            </div>
          )}
        </section>
        <aside className="comments">
          <h2>
            Passage comments <span>{d.draft.annotations.length}</span>
          </h2>
          {selection && !readOnly && (
            <div className="selection">
              <blockquote>{selection.exact_quote}</blockquote>
              <button
                disabled={busy}
                onClick={() => {
                  const id = crypto.randomUUID();
                  d.update({
                    ...d.draft,
                    annotations: [
                      ...d.draft.annotations,
                      { ...selection, id, comment: "" },
                    ],
                  });
                  setSelection(null);
                  window.getSelection()?.removeAllRanges();
                  requestAnimationFrame(() => {
                    const field = document.getElementById(id);
                    field?.focus();
                    field?.scrollIntoView({
                      block: "center",
                      behavior: "smooth",
                    });
                  });
                }}
              >
                Add comment
              </button>
              <button className="quiet" onClick={() => setSelection(null)}>
                Cancel
              </button>
            </div>
          )}
          {!d.draft.annotations.length && (
            <p className="muted">No passage comments yet.</p>
          )}
          {d.draft.annotations.map((a, i) => (
            <div className="comment" key={a.id}>
              <blockquote>{a.exact_quote}</blockquote>
              <label htmlFor={a.id}>Comment {i + 1}</label>
              <textarea
                id={a.id}
                rows={3}
                readOnly={readOnly}
                disabled={busy}
                maxLength={10000}
                value={a.comment}
                onChange={(e) =>
                  d.update({
                    ...d.draft,
                    annotations: d.draft.annotations.map((x) =>
                      x.id === a.id ? { ...x, comment: e.target.value } : x,
                    ),
                  })
                }
              />
              {a.updated_at && (
                <small>{new Date(a.updated_at).toLocaleString()}</small>
              )}
              {!readOnly && (
                <button
                  disabled={busy}
                  className="quiet danger"
                  onClick={() =>
                    d.update({
                      ...d.draft,
                      annotations: d.draft.annotations.filter(
                        (x) => x.id !== a.id,
                      ),
                    })
                  }
                >
                  Delete comment {i + 1}
                </button>
              )}
            </div>
          ))}
        </aside>
      </div>
    </>
  );
}
