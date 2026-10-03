import { unified } from "unified";
import remarkParse from "remark-parse";
import remarkRehype from "remark-rehype";
import rehypeSanitize, { defaultSchema } from "rehype-sanitize";
import { toHtml } from "hast-util-to-html";
import { toString } from "hast-util-to-string";
import type { Schema } from "hast-util-sanitize";
// No raw HTML, embeds, or externally loaded resources. Images are allowed ONLY
// as same-origin app-asset references (`/api/assets/<uuid>`); each request for
// those bytes is authorized against the immutable version that froze them.
// Remote images are removed entirely. Freeze both representations with the
// version; offsets use UTF-16 DOM textContent units, and images contribute no
// anchor text.
const ASSET_SRC =
  /^\/api\/assets\/[0-9a-f]{8}-[0-9a-f]{4}-[1-8][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/;
function dropForeignImages(node: unknown): void {
  const parent = node as { children?: { type: string; url?: string }[] };
  if (!parent.children) return;
  parent.children = parent.children.filter(
    (child) => child.type !== "image" || ASSET_SRC.test(child.url ?? ""),
  );
  for (const child of parent.children) dropForeignImages(child);
}
const schema: Schema = {
  ...defaultSchema,
  tagNames: [...(defaultSchema.tagNames ?? []), "img"],
  attributes: {
    ...defaultSchema.attributes,
    // Defense in depth: even a hand-built tree cannot carry another src.
    img: [["src", ASSET_SRC], "alt"],
  },
};
const processor = unified()
  .use(remarkParse)
  .use(remarkRehype)
  .use(rehypeSanitize, schema);
export function renderMarkdown(markdown: string) {
  const tree = processor.parse(markdown);
  dropForeignImages(tree);
  const frozen = processor.runSync(tree);
  return { rendered_html: toHtml(frozen), anchor_text: toString(frozen) };
}
