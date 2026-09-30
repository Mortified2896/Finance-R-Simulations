import { useCallback, useEffect, useRef, useState } from "react";
import type { Annotation, Review } from "../shared/types";
import { api } from "./api";
type Draft = { general_feedback: string; annotations: Annotation[] };
export function useDraft(initial: Review) {
  const [draft, setDraft] = useState<Draft>({
    general_feedback: initial.general_feedback,
    annotations: initial.annotations,
  });
  const latest = useRef(draft),
    saved = useRef(JSON.stringify(draft)),
    revision = useRef(initial.revision),
    pending = useRef<Promise<void> | null>(null);
  const retryPayload = useRef<
    (Draft & { revision: number; mutation_id: string }) | null
  >(null);
  const [state, setState] = useState("All changes saved"),
    [error, setError] = useState(""),
    [submitted, setSubmitted] = useState(initial.status === "submitted");
  const update = useCallback((next: Draft) => {
    latest.current = next;
    setDraft(next);
    setState("Unsaved changes");
  }, []);
  const flush = useCallback(async (): Promise<void> => {
    if (pending.current) {
      await pending.current;
      return flush();
    }
    if (saved.current === JSON.stringify(latest.current)) return;
    setState("Saving…");
    const run = async () => {
      while (saved.current !== JSON.stringify(latest.current)) {
        const payload = retryPayload.current ?? {
          ...latest.current,
          revision: revision.current,
          mutation_id: crypto.randomUUID(),
        };
        retryPayload.current = payload;
        const result = await api<{ revision: number }>(
          `/reviews/${initial.id}`,
          "PUT",
          payload,
        );
        revision.current = result.revision;
        saved.current = JSON.stringify({
          general_feedback: payload.general_feedback,
          annotations: payload.annotations,
        });
        retryPayload.current = null;
      }
      setError("");
      setState("All changes saved");
    };
    pending.current = run();
    try {
      await pending.current;
    } catch (e) {
      setError((e as Error).message);
      setState("Not saved");
      throw e;
    } finally {
      pending.current = null;
    }
  }, [initial.id]);
  useEffect(() => {
    if (submitted || error) return;
    const timer = setTimeout(() => void flush().catch(() => {}), 450);
    return () => clearTimeout(timer);
  }, [draft, submitted, error, flush]);
  useEffect(() => {
    const leave = (e: BeforeUnloadEvent) => {
      if (saved.current !== JSON.stringify(latest.current)) {
        void flush().catch(() => {});
        e.preventDefault();
        e.returnValue = "";
      }
    };
    const hidden = () => {
      if (document.visibilityState === "hidden") void flush().catch(() => {});
    };
    window.addEventListener("beforeunload", leave);
    document.addEventListener("visibilitychange", hidden);
    return () => {
      window.removeEventListener("beforeunload", leave);
      document.removeEventListener("visibilitychange", hidden);
    };
  }, [flush]);
  async function submit() {
    await flush();
    await api(`/reviews/${initial.id}/submit`, "POST", {
      revision: revision.current,
    });
    setSubmitted(true);
    setState("Submitted");
  }
  return { draft, update, flush, submit, submitted, state, error };
}
