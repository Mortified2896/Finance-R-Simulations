import { useEffect, useRef } from "react";
import type { Annotation, ArticleVersion } from "../shared/types";
export function Article({
  version,
  annotations,
  onSelect,
}: {
  version: ArticleVersion;
  annotations: Annotation[];
  onSelect?: (anchor: Omit<Annotation, "id" | "comment">) => void;
}) {
  const root = useRef<HTMLDivElement>(null);
  useEffect(() => {
    const el = root.current!;
    el.innerHTML = version.rendered_html;
    // Work on each original text node; overlaps retain a single visible highlight.
    const walker = document.createTreeWalker(el, NodeFilter.SHOW_TEXT),
      nodes: Text[] = [];
    let node;
    while ((node = walker.nextNode())) nodes.push(node as Text);
    let offset = 0;
    for (const node of nodes) {
      const start = offset,
        end = offset + node.length;
      offset = end;
      const hits = annotations.filter(
        (a) => a.start_offset < end && a.end_offset > start,
      );
      if (!hits.length) continue;
      const points = [
        ...new Set([
          0,
          node.length,
          ...hits.flatMap((a) => [
            Math.max(0, a.start_offset - start),
            Math.min(node.length, a.end_offset - start),
          ]),
        ]),
      ].sort((a, b) => a - b);
      const fragment = document.createDocumentFragment();
      for (let i = 1; i < points.length; i++) {
        const lo = points[i - 1],
          hi = points[i],
          active = hits.filter(
            (a) => a.start_offset < start + hi && a.end_offset > start + lo,
          );
        const text = node.data.slice(lo, hi);
        if (active.length) {
          const mark = document.createElement("mark");
          mark.textContent = text;
          mark.tabIndex = 0;
          mark.setAttribute("role", "button");
          mark.setAttribute("aria-label", "View passage comment");
          const show = () => {
            if (!window.getSelection()?.isCollapsed) return;
            const field = document.getElementById(active[0].id);
            field?.focus();
            field?.scrollIntoView({ block: "center", behavior: "smooth" });
          };
          mark.addEventListener("click", show);
          mark.addEventListener("keydown", (e) => {
            if (e.key === "Enter") {
              e.preventDefault();
              show();
            }
          });
          mark.title = active
            .map((a) => a.comment || "Comment in progress")
            .join("\n");
          fragment.append(mark);
        } else fragment.append(document.createTextNode(text));
      }
      node.replaceWith(fragment);
    }
  }, [version, annotations]);
  useEffect(() => {
    if (!onSelect) return;
    const selected = () => {
      const el = root.current,
        s = window.getSelection();
      if (!el || !s || s.isCollapsed || !s.rangeCount) return;
      const range = s.getRangeAt(0);
      if (
        !el.contains(range.startContainer) ||
        !el.contains(range.endContainer)
      )
        return;
      const before = document.createRange();
      before.selectNodeContents(el);
      before.setEnd(range.startContainer, range.startOffset);
      const start = before.toString().length,
        end = start + range.toString().length,
        text = version.anchor_text;
      if (end > start && end - start <= 10000)
        onSelect({
          start_offset: start,
          end_offset: end,
          exact_quote: text.slice(start, end),
          prefix: text.slice(Math.max(0, start - 64), start),
          suffix: text.slice(end, end + 64),
        });
    };
    document.addEventListener("selectionchange", selected);
    return () => document.removeEventListener("selectionchange", selected);
  }, [onSelect, version]);
  return <div className="prose" ref={root} aria-label="Article text" />;
}
