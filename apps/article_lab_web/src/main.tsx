import { createRoot } from "react-dom/client";
import { App } from "./App";
import "./style.css";
// react-style-singleton (Radix scroll lock) reads this standard global before
// inserting styles. This nonce is authorized for styles only, never scripts.
(window as Window & { __webpack_nonce__?: string }).__webpack_nonce__ =
  document.querySelector<HTMLMetaElement>(
    'meta[name="article-style-nonce"]',
  )?.content;
createRoot(document.getElementById("root")!).render(<App />);
