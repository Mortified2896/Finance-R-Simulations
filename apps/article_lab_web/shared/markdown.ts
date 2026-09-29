import { unified } from "unified";
import remarkParse from "remark-parse";
import remarkRehype from "remark-rehype";
import rehypeSanitize, { defaultSchema } from "rehype-sanitize";
import { toHtml } from "hast-util-to-html";
import { toString } from "hast-util-to-string";
// No raw HTML, images, embeds, or externally loaded resources. Freeze both
// representations with the version; offsets use UTF-16 DOM textContent units.
const processor = unified()
  .use(remarkParse)
  .use(remarkRehype)
  .use(rehypeSanitize, {
    ...defaultSchema,
    tagNames: defaultSchema.tagNames?.filter((t) => t !== "img"),
  });
export function renderMarkdown(markdown: string) {
  const tree = processor.runSync(processor.parse(markdown));
  return { rendered_html: toHtml(tree), anchor_text: toString(tree) };
}
