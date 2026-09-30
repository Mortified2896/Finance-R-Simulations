/** The same character ceiling enforced by POST /api/admin/versions. */
export const MAX_MARKDOWN_CHARS = 150_000;
export const MAX_IMPORT_BYTES = MAX_MARKDOWN_CHARS * 4;

export function markdownProblem(markdown: string): string | null {
  if (!markdown.trim())
    return "Write or import an article before creating a review snapshot.";
  if (markdown.length > MAX_MARKDOWN_CHARS)
    return `The article exceeds the ${MAX_MARKDOWN_CHARS.toLocaleString("en-US")}-character limit.`;
  if (markdown.includes("\0"))
    return "The article contains a null character. Import a UTF-8 text file.";
  return null;
}

/** File extensions are UX validation, not a substitute for the server sanitizer. */
export function importedMarkdown(filename: string, text: string): string {
  if (!/\.(md|markdown)$/i.test(filename))
    throw new Error(
      "Choose a .md or .markdown file. MDX components are not supported in this MVP.",
    );
  const markdown = text.replace(/^\uFEFF/, "");
  const error = markdownProblem(markdown);
  if (error) throw new Error(error);
  return markdown;
}
