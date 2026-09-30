import { describe, expect, it } from "vitest";
import {
  importedMarkdown,
  markdownProblem,
  MAX_MARKDOWN_CHARS,
} from "../src/editor/markdownInput";

describe("Markdown draft input", () => {
  it("requires a nonempty draft", () => {
    for (const text of ["", " ", "\n\t"])
      expect(markdownProblem(text)).not.toBeNull();
  });
  it("accepts ordinary article formatting", () => {
    expect(
      markdownProblem(
        "## Heading\n\nA **bold** claim with [evidence](https://example.test).",
      ),
    ).toBeNull();
  });
  it("uses the existing server's character ceiling", () => {
    expect(markdownProblem("a".repeat(MAX_MARKDOWN_CHARS))).toBeNull();
    expect(markdownProblem("a".repeat(MAX_MARKDOWN_CHARS + 1))).toContain(
      "150,000",
    );
  });
  it("rejects null bytes", () => {
    expect(markdownProblem("hello\0world")).toContain("null character");
  });
  it("accepts case-insensitive Markdown extensions", () => {
    expect(importedMarkdown("DRAFT.MD", "hello")).toBe("hello");
    expect(importedMarkdown("draft.markdown", "hello")).toBe("hello");
  });
  it("removes only an initial byte-order mark", () => {
    expect(
      importedMarkdown("draft.md", "\uFEFF## Heading\r\n\r\nUnicode 中文 👋\n"),
    ).toBe("## Heading\r\n\r\nUnicode 中文 👋\n");
  });
  it("does not change quotes or whitespace inside the document", () => {
    const text = '## Heading\n\n  "Quoted"  text.\n';
    expect(importedMarkdown("draft.md", text)).toBe(text);
  });
  it("rejects non-Markdown imports", () => {
    for (const name of ["draft.docx", "draft.mdx", "draft.md.exe", "draft"])
      expect(() => importedMarkdown(name, "text")).toThrow(".md");
  });
  it("does not pretend to be the server sanitizer", () => {
    // Review preview/publication must still pass through renderMarkdown.
    expect(importedMarkdown("draft.md", '<script>alert("x")</script>')).toBe(
      '<script>alert("x")</script>',
    );
  });
  it("rejects an empty imported file without generating replacement content", () => {
    expect(() => importedMarkdown("empty.md", "\uFEFF\n")).toThrow(
      "Write or import",
    );
  });
});
