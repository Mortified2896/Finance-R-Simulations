import { useMemo } from "react";
import {
  MDXEditor,
  UndoRedo,
  BoldItalicUnderlineToggles,
  BlockTypeSelect,
  ListsToggle,
  CreateLink,
  Separator,
  headingsPlugin,
  listsPlugin,
  quotePlugin,
  linkPlugin,
  linkDialogPlugin,
  thematicBreakPlugin,
  markdownShortcutPlugin,
  toolbarPlugin,
} from "@mdxeditor/editor";
import "@mdxeditor/editor/style.css";
import "./editor.css";

function Toolbar() {
  return (
    <>
      <UndoRedo />
      <Separator />
      <BlockTypeSelect />
      <BoldItalicUnderlineToggles options={["Bold", "Italic"]} />
      <Separator />
      <ListsToggle options={["bullet", "number"]} />
      <CreateLink />
    </>
  );
}

export default function RichMarkdownEditor({
  initialMarkdown,
  readOnly,
  onChange,
  onError,
}: {
  initialMarkdown: string;
  readOnly: boolean;
  onChange: (markdown: string) => void;
  onError: (message: string) => void;
}) {
  // Never feed onChange back through setMarkdown. Switching modes remounts the
  // editor with the current Markdown; ordinary typing stays inside Lexical.
  const plugins = useMemo(
    () => [
      headingsPlugin({ allowedHeadingLevels: [2, 3] }),
      listsPlugin(),
      quotePlugin(),
      linkPlugin(),
      linkDialogPlugin(),
      thematicBreakPlugin(),
      markdownShortcutPlugin(),
      toolbarPlugin({ toolbarContents: Toolbar }),
    ],
    [],
  );
  return (
    <MDXEditor
      markdown={initialMarkdown}
      readOnly={readOnly}
      plugins={plugins}
      suppressHtmlProcessing
      contentEditableClassName="prose article-draft-content"
      placeholder="Write your article here, or import a Markdown draft."
      onChange={onChange}
      onError={({ error }) => onError(error)}
    />
  );
}
