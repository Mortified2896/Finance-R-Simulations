/** Editorial workflow ported from the Shiny Article Lab. Scoring is intentionally absent. */
export const STAGES = ["titles", "subtitles", "thumbnails", "outline", "drafts", "publish"] as const;
export type Stage = (typeof STAGES)[number];
export const STAGE_LABELS: Record<Stage, string> = {
  titles: "Title Lab", subtitles: "Subtitle Generation", thumbnails: "Thumbnails",
  outline: "Outline", drafts: "Full Text", publish: "Review & Publish",
};
export type ItemKind = "titles" | "subtitles" | "thumbnail_concepts" | "outline";
export type ItemStatus = "candidate" | "approved" | "archived";
export type Item = {
  id: string; article_id: string; kind: ItemKind; parent_id: string | null;
  source_candidate_id: string | null; original_text: string; text: string;
  status: ItemStatus; notes: string; input_snapshot: string; created_at: string; updated_at: string;
  model?: string | null; route_id?: string | null; job_id?: string | null;
};
export type Package = {
  id: string; article_id: string; title_id: string; subtitle_id: string;
  image_asset_id: string | null; concept_id: string | null; notes: string;
  archived: number; image_settings_json: string; created_at: string;
};
export type Draft = {
  id: string; article_id: string; package_id: string; outline_id: string;
  original_body: string; body: string; notes: string;
  status: "draft" | "approved" | "rejected"; input_snapshot: string; created_at: string; updated_at: string;
};
export type Asset = {
  id: string; article_id: string; alt_text: string; caption: string;
  source: string; prompt: string | null; model: string | null; created_at: string;
  archived_at: string | null; width: number | null; height: number | null;
};
export type ImageLink = { package_id: string; image_asset_id: string; archived: number; notes: string };
export type Template = { id: string; kind: ItemKind; name: string; prompt: string };
export type GenerationSettings = { route_id: string; count: number; prompt: string; directions: string };
export type PublishSettings = {
  tags: string[]; target: "undecided" | "medium_profile" | "medium_publication" | "website";
  publication: string; monetization: "undecided" | "free" | "paid";
  status: "ready_for_review" | "ready_to_publish" | "submitted" | "published" | "needs_changes" | "archived";
  canonical_url: string; published_url: string; alt_text: string; image_credit: string; notes: string;
};
export const EMPTY_PUBLISH: PublishSettings = {
  tags: [], target: "undecided", publication: "", monetization: "undecided",
  status: "ready_for_review", canonical_url: "", published_url: "", alt_text: "", image_credit: "", notes: "",
};
export type ProductionDetail = {
  article_id: string; revision: number;
  workspace: { topic: string; brief: string; evidence: string; draft_body: string; revision: number };
  items: Item[]; packages: Package[]; drafts: Draft[]; assets: Asset[]; image_links: ImageLink[];
  settings: Partial<Record<ItemKind, GenerationSettings>>; templates: Template[];
  publishing: { draft_id: string; settings: PublishSettings; submitted_at: string | null; published_at: string | null }[];
  snapshots: { draft_id: string; version_id: string; version_number: number; created_at: string }[];
  unassigned: { id: string; kind: ItemKind; value: string; archived_at: string | null; job_id: string }[];
  jobs: { id: string; kind: string; parent_id: string | null; status: string; route_id: string | null;
    actual_model: string | null; error_code: string | null; created_at: string; request_json?: string; is_image: number }[];
};
export type Command = { id: string; revision: number; action: string; [key: string]: unknown };
export class ProductionError extends Error {
  status: number;
  constructor(message: string, status = 400) { super(message); this.status = status; }
}
export const LIMITS: Record<ItemKind, number> = { titles: 140, subtitles: 90, thumbnail_concepts: 4000, outline: 30000 };
export function object(value: unknown): Record<string, unknown> {
  if (!value || typeof value !== "object" || Array.isArray(value)) throw new ProductionError("Expected an object.");
  return value as Record<string, unknown>;
}
export function string(value: unknown, label: string, max: number, empty = false): string {
  if (typeof value !== "string" || value.length > max || (!empty && !value.trim())) throw new ProductionError(`Invalid ${label}.`);
  return value;
}
export function id(value: unknown): string {
  const result = string(value, "ID", 36);
  if (!/^[0-9a-f]{8}-[0-9a-f]{4}-[1-8][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(result)) throw new ProductionError("Invalid ID.");
  return result.toLowerCase();
}
export function ids(value: unknown, max = 50): string[] {
  if (!Array.isArray(value) || !value.length || value.length > max) throw new ProductionError(`Select between 1 and ${max} items.`);
  const result = value.map(id);
  if (new Set(result).size !== result.length) throw new ProductionError("Duplicate selections.");
  return result;
}
export function kind(value: unknown): ItemKind {
  if (!["titles", "subtitles", "thumbnail_concepts", "outline"].includes(String(value))) throw new ProductionError("Unsupported stage.");
  return value as ItemKind;
}
export function itemText(value: unknown, type: ItemKind): string {
  let result = string(value, "candidate text", LIMITS[type] * 2).trim();
  if (type === "titles" || type === "subtitles") result = result.replace(/\s+/gu, " ");
  if ([...result].length > LIMITS[type]) throw new ProductionError(`${type === "subtitles" ? "Subtitle" : "Candidate"} exceeds ${LIMITS[type]} characters. Edit it shorter; it was not truncated.`);
  return result;
}
export const DEFAULT_PROMPTS: Record<ItemKind, string> = {
  titles: "Generate credible, specific, beginner-friendly Medium titles. Prefer 40–75 characters. Avoid clickbait and unsupported claims. Use the article context below.",
  subtitles: "Generate subtitles for this exact title. Add a concrete promise without repeating the title. Every subtitle must be at most 90 characters. Do not invent findings.",
  thumbnail_concepts: "Suggest distinct editorial thumbnail concepts for this title/subtitle package. Start each with a short concept name, then composition and a concise image prompt. Avoid clutter. Keep the title and subtitle outside the image unless specifically requested.",
  outline: "Create a reader-facing Medium outline for this approved package. Use Markdown headings, key arguments and evidence needs. Preserve supplied facts, flag missing evidence and do not write the full article yet.",
};
export function settings(value: unknown, type: ItemKind): GenerationSettings {
  const row = object(value);
  const count = row.count ?? (type === "outline" ? 1 : type === "titles" ? 12 : 4);
  if (!Number.isSafeInteger(count) || (count as number) < 1 || (count as number) > (type === "outline" ? 1 : 25)) throw new ProductionError("Invalid candidate count.");
  return { route_id: string(row.route_id ?? "", "route", 80, true), count: count as number,
    prompt: string(row.prompt, "prompt", 12000), directions: string(row.directions ?? "", "directions", 8000, true) };
}
export function findItem(detail: Pick<ProductionDetail, "items">, key: string): Item {
  const row = detail.items.find((item) => item.id === key);
  if (!row) throw new ProductionError("Candidate not found in this article.", 404);
  return row;
}
export function findPackage(detail: Pick<ProductionDetail, "packages">, key: string): Package {
  const row = detail.packages.find((item) => item.id === key);
  if (!row) throw new ProductionError("Package not found in this article.", 404);
  return row;
}
export function packageReady(detail: Pick<ProductionDetail, "items" | "packages">, pkg: Package): boolean {
  return !pkg.archived && detail.items.some((i) => i.id === pkg.title_id && i.status === "approved") &&
    detail.items.some((i) => i.id === pkg.subtitle_id && i.status === "approved" && i.input_snapshot === inputSnapshot(detail, "subtitles", pkg.title_id));
}
export function packageText(detail: Pick<ProductionDetail, "items">, pkg: Package): { title: string; subtitle: string } {
  return { title: findItem(detail, pkg.title_id).text, subtitle: findItem(detail, pkg.subtitle_id).text };
}
/** The same context builder is used in the visible preview and on the server. */
export function resolvedPrompt(detail: ProductionDetail, type: ItemKind, parent: string | null, config: GenerationSettings): string {
  const blocks = [config.prompt, config.directions && `Additional directions:\n${config.directions}`,
    `Working topic:\n${detail.workspace.topic}`, `Core brief:\n${detail.workspace.brief}`, `Evidence and notes:\n${detail.workspace.evidence}`];
  if (type === "subtitles") {
    const title = findItem(detail, parent ?? "");
    if (title.kind !== "titles" || title.status !== "approved") throw new ProductionError("Approve this title before generating subtitles.", 409);
    blocks.push(`Approved title:\n${title.text}`);
  } else if (type !== "titles") {
    const pkg = findPackage(detail, parent ?? "");
    if (!packageReady(detail, pkg)) throw new ProductionError("This package needs approved title and subtitle inputs.", 409);
    const words = packageText(detail, pkg);
    blocks.push(`Approved title:\n${words.title}`, `Approved subtitle:\n${words.subtitle}`, pkg.notes && `Package notes:\n${pkg.notes}`);
    if (pkg.concept_id) blocks.push(`Selected visual concept:\n${findItem(detail, pkg.concept_id).text}`);
    if (type === "outline") {
      const asset = detail.assets.find((a) => a.id === pkg.image_asset_id && !a.archived_at);
      if (!asset) throw new ProductionError("Approve a thumbnail for this package before creating an outline.", 409);
      blocks.push(`Approved thumbnail description (text context only; image bytes are not sent):\n${asset.alt_text || asset.prompt || "An editor-selected thumbnail; no visual description supplied."}`);
    }
  } else if (parent) throw new ProductionError("Titles do not have a parent candidate.");
  const result = blocks.filter(Boolean).join("\n\n");
  if (result.length > 60000) throw new ProductionError("The combined prompt exceeds 60,000 characters. Shorten the evidence or directions; nothing was truncated.");
  return result;
}
export function writingContext(detail: ProductionDetail, outline: Item): string {
  if (outline.kind !== "outline" || outline.status !== "approved" || !outline.parent_id) throw new ProductionError("Choose an approved outline.");
  const pkg = findPackage(detail, outline.parent_id);
  if (!packageReady(detail, pkg)) throw new ProductionError("Reapprove the package inputs first.");
  const words = packageText(detail, pkg);
  return ["Draft a complete Medium article using the following approved package. Return the article body as Markdown. Do not invent findings, statistics or citations. Clearly flag missing evidence for editorial review.",
    `Title: ${words.title}`, `Subtitle: ${words.subtitle}`, `Brief:\n${detail.workspace.brief}`, `Evidence and notes:\n${detail.workspace.evidence}`,
    `Approved outline:\n${outline.text}`, pkg.notes && `Additional notes:\n${pkg.notes}`].filter(Boolean).join("\n\n");
}
export function publishSettings(value: unknown): PublishSettings {
  const row = object(value);
  if (!Array.isArray(row.tags) || row.tags.length > 5) throw new ProductionError("Use at most five Medium tags.");
  const tags = [...new Set(row.tags.map((tag) => string(tag, "tag", 80).trim()))];
  const pick = <T extends string>(v: unknown, allowed: readonly T[], label: string): T => {
    if (!allowed.includes(v as T)) throw new ProductionError(`Invalid ${label}.`); return v as T;
  };
  const url = (v: unknown) => {
    const s = string(v ?? "", "URL", 2000, true).trim();
    if (s) { let u: URL; try { u = new URL(s); } catch { throw new ProductionError("Use a complete HTTP or HTTPS URL."); }
      if (!["http:", "https:"].includes(u.protocol) || u.username || u.password) throw new ProductionError("Unsafe URL."); }
    return s;
  };
  return { tags, target: pick(row.target, ["undecided", "medium_profile", "medium_publication", "website"], "target"),
    publication: string(row.publication ?? "", "publication", 250, true), monetization: pick(row.monetization, ["undecided", "free", "paid"], "monetization"),
    status: pick(row.status, ["ready_for_review", "ready_to_publish", "submitted", "published", "needs_changes", "archived"], "publish status"),
    canonical_url: url(row.canonical_url), published_url: url(row.published_url),
    alt_text: string(row.alt_text ?? "", "alt text", 1000, true), image_credit: string(row.image_credit ?? "", "image credit", 2000, true),
    notes: string(row.notes ?? "", "publish notes", 10000, true) };
}
export function mediumMarkdown(title: string, subtitle: string, body: string, meta: PublishSettings): string {
  return [`# ${title}`, subtitle && `## ${subtitle}`, body, meta.alt_text && `Featured image alt text: ${meta.alt_text}`,
    meta.image_credit && `Image credit: ${meta.image_credit}`].filter(Boolean).join("\n\n") + "\n";
}

/** Stable parent bindings. Generation results do not silently adopt edited inputs. */
export function inputSnapshot(detail: Pick<ProductionDetail, "items" | "packages">, type: ItemKind, parent: string | null): string {
  if (type === "titles") return "{}";
  if (type === "subtitles") return JSON.stringify({ title: findItem(detail, parent ?? "").text });
  const pkg = findPackage(detail, parent ?? "");
  const words = packageText(detail, pkg);
  return JSON.stringify({ ...words, ...(type === "outline" ? { image: pkg.image_asset_id, notes: pkg.notes } : {}) });
}
export function draftSnapshot(detail: Pick<ProductionDetail, "items" | "packages">, outline: Item): string {
  return JSON.stringify({ package: inputSnapshot(detail, "outline", outline.parent_id), outline: outline.text });
}
