import {
  ProductionError, object, string, id, ids, kind, itemText, settings,
  findItem, findPackage, packageReady, packageText, resolvedPrompt,
  publishSettings, EMPTY_PUBLISH, inputSnapshot, draftSnapshot,
} from "../shared/production.ts";
import type { ProductionDetail, Item, ItemKind, Package, Draft, Command } from "../shared/production.ts";
import { generationRoutes, generationInput, imageCatalog, imageJobInput, referencedAssetIds, InputError } from "../shared/generation.ts";

export interface Statement {
  bind(...values: unknown[]): Statement;
  first<T = Record<string, unknown>>(): Promise<T | null>;
  all<T = Record<string, unknown>>(): Promise<{ results: T[] }>;
  run(): Promise<unknown>;
}
export interface Database { prepare(sql: string): Statement; batch(statements: Statement[]): Promise<unknown[]> }
export type ProductionEnv = { DB: Database; ARTICLE_LAB_ROUTES?: string; ARTICLE_LAB_RUNNER_TOKEN?: string; ARTICLE_LAB_IMAGES?: string; IMAGES?: unknown };
export type Actor = { id: string; role: string; status: string };
export type Render = (markdown: string) => { rendered_html: string; anchor_text: string } | Promise<{ rendered_html: string; anchor_text: string }>;
const sql = (db: Database, query: string, ...args: unknown[]) => db.prepare(query).bind(...args);
const iso = () => new Date().toISOString();
const marks = (values: unknown[]) => values.map(() => "?").join(",");
async function hash(value: unknown) {
  const bytes = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(JSON.stringify(value)));
  return [...new Uint8Array(bytes)].map((b) => b.toString(16).padStart(2, "0")).join("");
}
function access(actor: Actor) {
  if (actor.role !== "admin" || actor.status !== "approved") throw new ProductionError("Approved administrator access required.", 403);
}
async function initialize(db: Database, article: string) {
  if (!await sql(db, "SELECT article_id FROM article_workspaces WHERE article_id=?", article).first()) throw new ProductionError("Workspace not found.", 404);
  await sql(db, "INSERT INTO production_state(article_id,updated_at) VALUES(?,?) ON CONFLICT(article_id) DO NOTHING", article, iso()).run();
}
export async function loadProduction(db: Database, article: string, actor: Actor): Promise<ProductionDetail> {
  access(actor); id(article); await initialize(db, article);
  for (let attempt = 0; attempt < 3; attempt++) {
    const before = await sql(db, "SELECT revision FROM production_state WHERE article_id=?", article).first<{ revision: number }>();
    const [workspace, items, packages, drafts, assets, imageLinks, settingRows, templates, publishing, snapshots, unassigned, jobs] = await Promise.all([
      sql(db, "SELECT topic,brief,evidence,draft_body,revision FROM article_workspaces WHERE article_id=?", article).first<ProductionDetail["workspace"]>(),
      sql(db, `SELECT i.*,j.actual_model model,j.route_id,c.job_id FROM production_items i LEFT JOIN generation_candidates c ON c.id=i.source_candidate_id LEFT JOIN generation_jobs j ON j.id=c.job_id WHERE i.article_id=? ORDER BY i.created_at,i.id`, article).all<Item>(),
      sql(db, "SELECT * FROM production_packages WHERE article_id=? ORDER BY created_at,id", article).all<Package>(),
      sql(db, "SELECT * FROM production_drafts WHERE article_id=? ORDER BY created_at DESC,id", article).all<Draft>(),
      sql(db, "SELECT id,article_id,alt_text,caption,source,prompt,model,created_at,archived_at,width,height FROM image_assets WHERE article_id=? ORDER BY created_at DESC,id", article).all<ProductionDetail["assets"][number]>(),
      sql(db, "SELECT package_id,image_asset_id,archived,notes FROM production_image_links WHERE article_id=?", article).all<ProductionDetail["image_links"][number]>(),
      sql(db, "SELECT kind,settings_json FROM production_settings WHERE article_id=?", article).all<{ kind: ItemKind; settings_json: string }>(),
      sql(db, "SELECT id,kind,name,prompt FROM production_templates WHERE article_id=? ORDER BY kind,name", article).all<ProductionDetail["templates"][number]>(),
      sql(db, "SELECT draft_id,settings_json,submitted_at,published_at FROM production_publishing WHERE article_id=?", article).all<{ draft_id: string; settings_json: string; submitted_at: string | null; published_at: string | null }>(),
      sql(db, "SELECT s.draft_id,s.version_id,v.version_number,s.created_at FROM production_snapshots s JOIN article_versions v ON v.id=s.version_id WHERE s.article_id=? ORDER BY s.created_at DESC", article).all<ProductionDetail["snapshots"][number]>(),
      sql(db, "SELECT c.id,c.kind,c.value,c.archived_at,c.job_id FROM generation_candidates c WHERE c.article_id=? AND NOT EXISTS(SELECT 1 FROM production_items i WHERE i.source_candidate_id=c.id) ORDER BY c.created_at,c.id", article).all<ProductionDetail["unassigned"][number]>(),
      sql(db, `SELECT j.id,j.kind,t.parent_id,j.status,j.route_id,j.actual_model,j.error_code,j.created_at,j.request_json,0 is_image FROM generation_jobs j LEFT JOIN production_job_targets t ON t.job_id=j.id WHERE j.article_id=?
        UNION ALL SELECT j.id,'image',t.parent_id,j.status,NULL,j.model,j.error_code,j.created_at,j.prompt,1 FROM image_jobs j LEFT JOIN production_job_targets t ON t.job_id=j.id WHERE j.article_id=? ORDER BY 8 DESC LIMIT 200`, article, article).all<ProductionDetail["jobs"][number]>(),
    ]);
    const after = await sql(db, "SELECT revision FROM production_state WHERE article_id=?", article).first<{ revision: number }>();
    if (before?.revision !== after?.revision) continue;
    return { article_id: article, revision: after!.revision, workspace: workspace!, items: items.results, packages: packages.results,
      drafts: drafts.results, assets: assets.results, image_links: imageLinks.results, settings: Object.fromEntries(settingRows.results.map((r) => [r.kind, JSON.parse(r.settings_json)])),
      templates: templates.results, publishing: publishing.results.map(({ settings_json, ...rest }) => ({ ...rest, settings: JSON.parse(settings_json) })),
      snapshots: snapshots.results, unassigned: unassigned.results, jobs: jobs.results };
  }
  throw new ProductionError("The article is changing in another session. Retry loading it.", 409);
}

/** Every editorial command is one CAS-guarded D1 batch. No partial bulk promotions. */
export async function executeProductionCommand(env: ProductionEnv, article: string, actor: Actor, raw: unknown, render?: Render) {
  access(actor); id(article);
  const command = object(raw), mutation = id(command.id);
  if (!Number.isSafeInteger(command.revision) || (command.revision as number) < 0) throw new ProductionError("Invalid revision.");
  const expected = command.revision as number, action = string(command.action, "action", 50);
  const db = env.DB, time = iso(), requestHash = await hash([article, actor.id, command]);
  const previous = await sql(db, "SELECT request_hash,result_json FROM production_mutations WHERE id=?", mutation).first<{ request_hash: string; result_json: string }>();
  if (previous) {
    if (previous.request_hash !== requestHash) throw new ProductionError("That action ID belongs to different input.", 409);
    return { ...JSON.parse(previous.result_json), duplicate: true };
  }
  const detail = await loadProduction(db, article, actor);
  if (detail.revision !== expected) throw new ProductionError("This article changed elsewhere. Your edits are still shown. Reload the saved state before retrying.", 409);
  const next = expected + 1, attemptToken = crypto.randomUUID();
  const guard = "EXISTS(SELECT 1 FROM production_state WHERE article_id=? AND revision=? AND last_mutation=?)";
  const guardArgs = [article, next, attemptToken];
  const writes: Statement[] = [];
  let admission = "", admissionArgs: unknown[] = [];
  const result: Record<string, unknown> = { revision: next };
  const insert = (table: string, values: Record<string, unknown>, conflict = "") => {
    const fields = Object.keys(values);
    writes.push(sql(db, `INSERT INTO ${table}(${fields.join(",")}) SELECT ${marks(fields)} WHERE ${guard} ${conflict}`, ...Object.values(values), ...guardArgs));
  };
  const update = (table: string, values: Record<string, unknown>, where: string, args: unknown[]) => {
    writes.push(sql(db, `UPDATE ${table} SET ${Object.keys(values).map((f) => `${f}=?`).join(",")} WHERE ${where} AND ${guard}`, ...Object.values(values), ...args, ...guardArgs));
  };
  const remove = (table: string, where: string, args: unknown[]) => writes.push(sql(db, `DELETE FROM ${table} WHERE ${where} AND ${guard}`, ...args, ...guardArgs));
  const parentFor = (type: ItemKind, value: unknown, requireReady = true): string | null => {
    if (type === "titles") { if (value != null && value !== "") throw new ProductionError("Titles do not have a parent."); return null; }
    const parent = id(value);
    if (type === "subtitles") {
      const title = findItem(detail, parent);
      if (title.kind !== "titles" || (requireReady && title.status !== "approved")) throw new ProductionError("Choose an approved title.", 409);
    } else {
      const pkg = findPackage(detail, parent);
      if (requireReady && !packageReady(detail, pkg)) throw new ProductionError("Approve the title/subtitle package first.", 409);
    }
    return parent;
  };
  const requireImage = (pkg: Package) => {
    if (!packageReady(detail, pkg) || !pkg.image_asset_id || !detail.assets.some((a) => a.id === pkg.image_asset_id && !a.archived_at)) throw new ProductionError("Approve the package and its thumbnail first.", 409);
  };
  const invalidate = (packageIds: string[], clearImage = false) => {
    if (!packageIds.length) return;
    if (clearImage) update("production_packages", { image_asset_id: null }, `id IN (${marks(packageIds)}) AND article_id=?`, [...packageIds, article]);
    update("production_items", { status: "candidate", updated_at: time }, `kind='outline' AND status='approved' AND parent_id IN (${marks(packageIds)}) AND article_id=?`, [...packageIds, article]);
    update("production_drafts", { status: "draft", updated_at: time }, `status='approved' AND package_id IN (${marks(packageIds)}) AND article_id=?`, [...packageIds, article]);
  };
  const addItem = (itemId: string, type: ItemKind, parent: string | null, value: string, source: string | null, snapshot: string, archived = false) => {
    insert("production_items", { id: itemId, article_id: article, kind: type, parent_id: parent, source_candidate_id: source,
      original_text: value, text: value, input_snapshot: snapshot, status: archived ? "archived" : "candidate", created_at: time, updated_at: time }, "ON CONFLICT(source_candidate_id) DO NOTHING");
  };
  const draftById = (key: unknown) => {
    const draft = detail.drafts.find((d) => d.id === id(key));
    if (!draft) throw new ProductionError("Draft not found in this article.", 404); return draft;
  };

  switch (action) {
    case "workspace": {
      const workspaceRevision = command.workspace_revision;
      if (!Number.isSafeInteger(workspaceRevision) || workspaceRevision !== detail.workspace.revision) throw new ProductionError("Workspace brief changed elsewhere.", 409);
      admission = " AND EXISTS(SELECT 1 FROM article_workspaces WHERE article_id=? AND revision=?)"; admissionArgs = [article, workspaceRevision];
      update("article_workspaces", { topic: string(command.topic, "topic", 250).trim(), brief: string(command.brief, "brief", 30000, true), evidence: string(command.evidence, "evidence", 60000, true), revision: Number(workspaceRevision) + 1, updated_at: time }, "article_id=? AND revision=?", [article, workspaceRevision]);
      break;
    }
    case "add_items": {
      const type = kind(command.kind), parent = parentFor(type, command.parent_id);
      if (!Array.isArray(command.items) || !command.items.length || command.items.length > 24) throw new ProductionError("Add between one and 24 manual alternatives at a time.");
      const values = command.items.map((v) => { const row = object(v); return { id: id(row.id), text: itemText(row.text, type) }; });
      if (new Set(values.map((v) => v.id)).size !== values.length || new Set(values.map((v) => v.text.toLowerCase())).size !== values.length) throw new ProductionError("Duplicate alternatives.");
      for (const value of values) addItem(value.id, type, parent, value.text, null, inputSnapshot(detail, type, parent));
      result.ids = values.map((v) => v.id); break;
    }
    case "adopt_candidates": {
      const type = kind(command.kind), parent = parentFor(type, command.parent_id);
      for (const key of ids(command.ids)) {
        const source = detail.unassigned.find((s) => s.id === key && s.kind === type);
        if (!source) throw new ProductionError("Unassigned result not found in this article/stage.", 404);
        // Deliberate adoption assigns legacy output. Raw output remains unchanged.
        addItem(key, type, parent, source.value, key, inputSnapshot(detail, type, parent), Boolean(source.archived_at));
      }
      break;
    }
    case "sync": {
      const textRows = (await sql(db, `SELECT c.*,t.parent_id,t.input_snapshot FROM generation_candidates c JOIN production_job_targets t ON t.job_id=c.job_id WHERE c.article_id=? AND t.article_id=? AND t.kind=c.kind AND NOT EXISTS(SELECT 1 FROM production_items i WHERE i.source_candidate_id=c.id) ORDER BY c.created_at,c.id LIMIT 12`, article, article).all<{ id: string; kind: ItemKind; parent_id: string | null; value: string; input_snapshot: string; archived_at: string | null }>()).results;
      for (const row of textRows) addItem(row.id, row.kind, row.parent_id, row.value, row.id, row.input_snapshot, Boolean(row.archived_at));
      const imageRows = (await sql(db, `SELECT a.id,t.parent_id FROM image_assets a JOIN production_job_targets t ON t.job_id=a.job_id WHERE a.article_id=? AND t.article_id=? AND t.kind='image' AND NOT EXISTS(SELECT 1 FROM production_image_links l WHERE l.image_asset_id=a.id AND l.package_id=t.parent_id) LIMIT 4`, article, article).all<{ id: string; parent_id: string }>()).results;
      for (const row of imageRows) insert("production_image_links", { package_id: row.parent_id, article_id: article, image_asset_id: row.id }, "ON CONFLICT DO NOTHING");
      result.imported = textRows.length + imageRows.length;
      if (!writes.length) return { revision: expected, imported: 0 };
      break;
    }
    case "edit_item": {
      const item = findItem(detail, id(command.item_id));
      const value = itemText(command.text, item.kind), notes = string(command.notes ?? "", "notes", 10000, true);
      const changed = value !== item.text;
      const snapshot = changed || command.acknowledge_context === true ? inputSnapshot(detail, item.kind, item.parent_id) : item.input_snapshot;
      if (changed) {
        if (item.kind === "titles") update("production_items", { status: "candidate", updated_at: time }, "parent_id=? AND kind='subtitles' AND status='approved' AND article_id=?", [item.id, article]);
        const related = detail.packages.filter((p) => p.title_id === item.id || p.subtitle_id === item.id).map((p) => p.id);
        invalidate(related, true);
        if (item.kind === "outline" && item.parent_id) invalidate([item.parent_id]);
      }
      update("production_items", { text: value, notes, input_snapshot: snapshot, status: changed && item.status === "approved" ? "candidate" : item.status, updated_at: time }, "id=? AND article_id=?", [item.id, article]); break;
    }
    case "item_status": {
      const status = string(command.status, "status", 20);
      if (!["candidate", "approved", "archived"].includes(status)) throw new ProductionError("Invalid candidate status.");
      const rows = ids(command.ids).map((key) => findItem(detail, key));
      const outlineParents = new Set<string>();
      if (status === "approved") for (const item of rows) {
        if (item.kind !== "outline") continue;
        if (outlineParents.has(item.parent_id!)) throw new ProductionError("Approve only one outline per package.");
        outlineParents.add(item.parent_id!);
      }
      for (const item of rows) {
        if (item.status === status) continue;
        if (status === "approved") {
          if (item.status === "archived") throw new ProductionError("Restore archived candidates before approving.", 409);
          itemText(item.text, item.kind);
          parentFor(item.kind, item.parent_id);
          if (item.input_snapshot !== inputSnapshot(detail, item.kind, item.parent_id)) throw new ProductionError("This alternative belongs to earlier inputs. Edit/save it after checking the current package before approving.", 409);
          if (item.kind === "subtitles") insert("production_packages", { id: item.id, article_id: article, title_id: item.parent_id, subtitle_id: item.id, created_at: time }, "ON CONFLICT(subtitle_id) DO NOTHING");
          if (item.kind === "outline") {
            const pkg = findPackage(detail, item.parent_id!); requireImage(pkg);
            invalidate([pkg.id]);
          }
        } else {
          if (item.kind === "titles") update("production_items", { status: "candidate", updated_at: time }, "parent_id=? AND kind='subtitles' AND status='approved' AND article_id=?", [item.id, article]);
          const packages = detail.packages.filter((p) => p.title_id === item.id || p.subtitle_id === item.id).map((p) => p.id);
          invalidate(packages, true);
          if (item.kind === "outline" && item.parent_id) invalidate([item.parent_id]);
        }
        update("production_items", { status, updated_at: time }, "id=? AND article_id=?", [item.id, article]);
      }
      break;
    }
    case "package": {
      const pkg = findPackage(detail, id(command.package_id));
      const notes = string(command.notes ?? pkg.notes, "notes", 10000, true);
      const archived = command.archived === undefined ? pkg.archived : command.archived === true ? 1 : command.archived === false ? 0 : -1;
      if (archived < 0) throw new ProductionError("Invalid archive flag.");
      if (notes !== pkg.notes || archived !== pkg.archived) invalidate([pkg.id]);
      update("production_packages", { notes, archived }, "id=? AND article_id=?", [pkg.id, article]); break;
    }
    case "select_concept": {
      const pkg = findPackage(detail, id(command.package_id)), item = findItem(detail, id(command.item_id));
      if (item.kind !== "thumbnail_concepts" || item.parent_id !== pkg.id || item.status === "archived") throw new ProductionError("Choose an active concept for this package.");
      update("production_packages", { concept_id: item.id }, "id=? AND article_id=?", [pkg.id, article]); break;
    }
    case "image": {
      const pkg = findPackage(detail, id(command.package_id)), asset = detail.assets.find((a) => a.id === id(command.asset_id));
      if (!asset || asset.archived_at) throw new ProductionError("Active image not found in this article.", 404);
      const operation = command.operation;
      if (!["attach", "approve", "archive", "restore", "notes"].includes(String(operation))) throw new ProductionError("Invalid image action.");
      if (operation === "approve") {
        if (!packageReady(detail, pkg)) throw new ProductionError("Approve the package inputs first.", 409);
        const link = detail.image_links.find((l) => l.package_id === pkg.id && l.image_asset_id === asset.id);
        if (link?.archived) throw new ProductionError("Restore this image first.", 409);
        if (pkg.image_asset_id !== asset.id) invalidate([pkg.id]);
        update("production_packages", { image_asset_id: asset.id }, "id=? AND article_id=?", [pkg.id, article]);
      }
      if (operation === "archive" && pkg.image_asset_id === asset.id) throw new ProductionError("Approve a replacement before archiving the selected thumbnail.", 409);
      insert("production_image_links", { package_id: pkg.id, article_id: article, image_asset_id: asset.id }, "ON CONFLICT DO NOTHING");
      if (operation === "archive" || operation === "restore") update("production_image_links", { archived: operation === "archive" ? 1 : 0 }, "package_id=? AND image_asset_id=?", [pkg.id, asset.id]);
      if (operation === "notes") update("production_image_links", { notes: string(command.notes ?? "", "notes", 10000, true) }, "package_id=? AND image_asset_id=?", [pkg.id, asset.id]);
      break;
    }
    case "asset_metadata": {
      const asset = detail.assets.find((a) => a.id === id(command.asset_id));
      if (!asset || asset.archived_at) throw new ProductionError("Image not found in this article.", 404);
      if (await sql(db, "SELECT version_id FROM version_assets WHERE image_asset_id=? LIMIT 1", asset.id).first()) throw new ProductionError("This image is frozen in a reviewer version. Set publication-specific alt text in Review & Publish instead.", 409);
      update("image_assets", { alt_text: string(command.alt_text ?? "", "alt text", 1000, true), caption: string(command.caption ?? "", "caption", 2000, true) }, "id=? AND article_id=?", [asset.id, article]); break;
    }
    case "new_draft": {
      const outline = findItem(detail, id(command.outline_id));
      if (outline.kind !== "outline" || outline.status !== "approved") throw new ProductionError("Choose an approved outline.", 409);
      const pkg = findPackage(detail, outline.parent_id!); requireImage(pkg);
      if (outline.input_snapshot !== inputSnapshot(detail, "outline", pkg.id)) throw new ProductionError("Check and approve this outline against the current package first.", 409);
      const body = string(command.body, "article body", 150000);
      const draftId = id(command.draft_id);
      insert("production_drafts", { id: draftId, article_id: article, package_id: pkg.id, outline_id: outline.id, original_body: body, body,
        input_snapshot: draftSnapshot(detail, outline), notes: string(command.notes ?? "", "notes", 10000, true), created_at: time, updated_at: time }); result.draft_id = draftId; break;
    }
    case "edit_draft": {
      const draft = draftById(command.draft_id), body = string(command.body, "article body", 150000), notes = string(command.notes ?? "", "notes", 10000, true);
      const outline = findItem(detail, draft.outline_id);
      insert("production_draft_revisions", { id: crypto.randomUUID(), draft_id: draft.id, mutation_id: mutation,
        before_body: draft.body, after_body: body, before_notes: draft.notes, after_notes: notes, created_at: time });
      update("production_drafts", { body, notes, input_snapshot: body !== draft.body || command.acknowledge_context === true ? draftSnapshot(detail, outline) : draft.input_snapshot,
        status: body !== draft.body && draft.status === "approved" ? "draft" : draft.status, updated_at: time }, "id=? AND article_id=?", [draft.id, article]); break;
    }
    case "draft_status": {
      const draft = draftById(command.draft_id), status = String(command.status);
      if (!["draft", "approved", "rejected"].includes(status)) throw new ProductionError("Invalid draft status.");
      if (status === "approved") {
        const pkg = findPackage(detail, draft.package_id); requireImage(pkg);
        const outline = findItem(detail, draft.outline_id);
        if (outline.status !== "approved" || outline.input_snapshot !== inputSnapshot(detail, "outline", pkg.id) || draft.input_snapshot !== draftSnapshot(detail, outline)) throw new ProductionError("The outline/package changed. Check and save this draft against the current inputs first.", 409);
        update("production_drafts", { status: "draft", updated_at: time }, "package_id=? AND article_id=? AND status='approved'", [pkg.id, article]);
      }
      update("production_drafts", { status, updated_at: time }, "id=? AND article_id=?", [draft.id, article]); break;
    }
    case "save_publishing": {
      const draft = draftById(command.draft_id), value = publishSettings(command.settings);
      const old = detail.publishing.find((p) => p.draft_id === draft.id);
      insert("production_publishing", { draft_id: draft.id, article_id: article, settings_json: JSON.stringify(value),
        submitted_at: old?.submitted_at ?? (value.status === "submitted" ? time : null), published_at: old?.published_at ?? (value.status === "published" ? time : null), updated_at: time },
        "ON CONFLICT(draft_id) DO UPDATE SET settings_json=excluded.settings_json,submitted_at=excluded.submitted_at,published_at=excluded.published_at,updated_at=excluded.updated_at"); break;
    }
    case "save_settings": {
      const type = kind(command.kind), value = settings(command.settings, type);
      insert("production_settings", { article_id: article, kind: type, settings_json: JSON.stringify(value) }, "ON CONFLICT(article_id,kind) DO UPDATE SET settings_json=excluded.settings_json"); break;
    }
    case "save_template": {
      const type = kind(command.kind), templateId = id(command.template_id), name = string(command.name, "template name", 100).trim(), prompt = string(command.prompt, "template", 12000);
      const owned = detail.templates.find((t) => t.id === templateId);
      if (detail.templates.some((t) => t.kind === type && t.name.toLowerCase() === name.toLowerCase() && t.id !== templateId)) throw new ProductionError("A template with this name already exists.", 409);
      if (owned && owned.kind !== type) throw new ProductionError("Wrong template stage.", 409);
      if (owned) update("production_templates", { name, prompt }, "id=? AND article_id=? AND kind=?", [templateId, article, type]);
      else insert("production_templates", { id: templateId, article_id: article, kind: type, name, prompt }); break;
    }
    case "delete_template": {
      const templateId = id(command.template_id);
      if (!detail.templates.some((t) => t.id === templateId)) throw new ProductionError("Template not found.", 404);
      remove("production_templates", "id=? AND article_id=?", [templateId, article]); break;
    }
    case "generate": {
      if (!env.ARTICLE_LAB_RUNNER_TOKEN || env.ARTICLE_LAB_RUNNER_TOKEN.length < 32) throw new ProductionError("Generation runner is not configured.", 503);
      const type = kind(command.kind), config = settings(command.settings, type);
      const route = generationRoutes(JSON.parse(env.ARTICLE_LAB_ROUTES ?? "[]")).find((r) => r.id === config.route_id);
      if (!route) throw new ProductionError("Choose a configured subscription model.");
      if (!Array.isArray(command.jobs) || !command.jobs.length || command.jobs.length > 10) throw new ProductionError("Generate for one to ten selected inputs at a time.");
      const jobs = command.jobs.map((value) => { const row = object(value); return { id: id(row.id), parent: parentFor(type, row.parent_id) }; });
      if (new Set(jobs.map((j) => j.id)).size !== jobs.length || new Set(jobs.map((j) => j.parent)).size !== jobs.length) throw new ProductionError("Duplicate generation targets.");
      admission = " AND (SELECT COUNT(*) FROM generation_jobs WHERE status IN ('queued','running'))+?<=50 AND (SELECT COUNT(*) FROM generation_jobs WHERE article_id=? AND status IN ('queued','running'))+?<=20";
      admissionArgs = [jobs.length, article, jobs.length];
      for (const job of jobs) {
        const input = generationInput({ kind: type, count: config.count, resolved_prompt: resolvedPrompt(detail, type, job.parent, config) });
        insert("generation_jobs", { id: job.id, article_id: article, requested_by: actor.id, kind: type, request_json: JSON.stringify(input), request_hash: await hash([article, actor.id, input, route]), route_json: JSON.stringify(route), route_id: route.id, created_at: time });
        insert("production_job_targets", { job_id: job.id, article_id: article, kind: type, parent_id: job.parent, input_snapshot: inputSnapshot(detail, type, job.parent), created_at: time });
      }
      insert("production_settings", { article_id: article, kind: type, settings_json: JSON.stringify(config) }, "ON CONFLICT(article_id,kind) DO UPDATE SET settings_json=excluded.settings_json");
      result.job_ids = jobs.map((j) => j.id); break;
    }
    case "generate_images": {
      if (!env.IMAGES || !env.ARTICLE_LAB_IMAGES || !env.ARTICLE_LAB_RUNNER_TOKEN || env.ARTICLE_LAB_RUNNER_TOKEN.length < 32) throw new ProductionError("Image storage/generation is not configured. Existing alternatives are unchanged.", 503);
      const pkg = findPackage(detail, id(command.package_id));
      if (!packageReady(detail, pkg)) throw new ProductionError("Approve title and subtitle first.", 409);
      const jobIds = ids(command.job_ids, 4), input = imageJobInput(command, imageCatalog(env.ARTICLE_LAB_IMAGES));
      admission = " AND (SELECT COUNT(*) FROM image_jobs WHERE status IN ('queued','running'))+?<=20"; admissionArgs = [jobIds.length];
      for (const key of jobIds) {
        insert("image_jobs", { id: key, article_id: article, requested_by: actor.id, ...input, request_hash: await hash([article, actor.id, input, key]), created_at: time });
        insert("production_job_targets", { job_id: key, article_id: article, kind: "image", parent_id: pkg.id, input_snapshot: inputSnapshot(detail, "thumbnail_concepts", pkg.id), created_at: time });
      }
      update("production_packages", { image_settings_json: JSON.stringify(input) }, "id=? AND article_id=?", [pkg.id, article]);
      result.job_ids = jobIds; break;
    }
    case "publish_snapshot": {
      const draft = draftById(command.draft_id), pkg = findPackage(detail, draft.package_id), outline = findItem(detail, draft.outline_id);
      requireImage(pkg);
      if (draft.status !== "approved" || outline.status !== "approved" || draft.input_snapshot !== draftSnapshot(detail, outline)) throw new ProductionError("Approve the current draft and outline before preparing a reviewer version.", 409);
      if (!render) throw new ProductionError("Snapshot renderer unavailable.", 503);
      const meta = detail.publishing.find((p) => p.draft_id === draft.id)?.settings ?? EMPTY_PUBLISH;
      const words = packageText(detail, pkg), asset = detail.assets.find((a) => a.id === pkg.image_asset_id)!;
      const escapedAlt = (meta.alt_text || asset.alt_text).replace(/[\[\]\\\r\n]/g, " ");
      let body = draft.body;
      // Include a visible frozen thumbnail unless the body already references it.
      if (!referencedAssetIds(body).includes(asset.id)) body = `![${escapedAlt}](/api/assets/${asset.id})\n\n${body}`;
      if (body.length > 150000) throw new ProductionError("Draft plus thumbnail exceeds the snapshot limit.");
      for (const match of body.matchAll(/!\[[^\]]*\]\(([^)]+)\)/g)) {
        if (!/^\/api\/assets\/[0-9a-f-]{36}$/i.test(match[1].trim())) throw new ProductionError("Replace remote or malformed image references with uploaded Article Lab images before publishing.");
      }
      const assetIds = referencedAssetIds(body);
      for (const key of assetIds) if (!detail.assets.some((a) => a.id === key && !a.archived_at)) throw new ProductionError("Draft references a missing, archived or foreign article image.");
      const rendered = await render(body), version = id(command.version_id);
      writes.push(sql(db, `INSERT INTO article_versions(id,article_id,version_number,title,subtitle,body,body_format,rendered_html,anchor_text,created_at)
        SELECT ?,?,COALESCE(MAX(version_number),0)+1,?,?,?,'markdown',?,?,? FROM article_versions WHERE article_id=? HAVING ${guard}`, version, article, words.title, words.subtitle, body, rendered.rendered_html, rendered.anchor_text, time, article, ...guardArgs));
      for (const key of assetIds) insert("version_assets", { version_id: version, image_asset_id: key, role: key === asset.id ? "thumbnail" : "inline" });
      insert("production_snapshots", { version_id: version, article_id: article, draft_id: draft.id, created_at: time });
      result.version_id = version; break;
    }
    default: throw new ProductionError("Unknown production action.");
  }
  insert("production_mutations", { id: mutation, article_id: article, request_hash: requestHash, revision: next, result_json: JSON.stringify(result), created_at: time });
  if (writes.length > 25) throw new ProductionError("This selection has too many linked changes for one safe request. Select fewer rows and try again; nothing was changed.");
  await db.batch([
    sql(db, `UPDATE production_state SET revision=revision+1,last_mutation=?,updated_at=? WHERE article_id=? AND revision=?${admission}`, attemptToken, time, article, expected, ...admissionArgs),
    ...writes,
  ]);
  const saved = await sql(db, "SELECT result_json,request_hash FROM production_mutations WHERE id=? AND article_id=?", mutation, article).first<{ result_json: string; request_hash: string }>();
  if (!saved) throw new ProductionError(admission ? "The article changed or the generation queue is full. Nothing from this action was applied." : "The article changed elsewhere. Your unsaved input is still available.", 409);
  if (saved.request_hash !== requestHash) throw new ProductionError("That action ID belongs to different input.", 409);
  return JSON.parse(saved.result_json);
}

export async function handleProductionRequest(request: Request, env: ProductionEnv, actor: Actor, render?: Render): Promise<Response> {
  const respond = (data: unknown, status = 200) => new Response(JSON.stringify(data), { status, headers: { "Content-Type": "application/json", "Cache-Control": "no-store" } });
  try {
    access(actor);
    const url = new URL(request.url), match = url.pathname.match(/^\/api\/admin\/lab\/production\/([^/]+)(?:\/(command|revisions))?$/);
    if (!match) throw new ProductionError("Production endpoint not found.", 404);
    const article = id(match[1]);
    if (request.method === "GET" && !match[2]) return respond(await loadProduction(env.DB, article, actor));
    if (request.method === "GET" && match[2] === "revisions") {
      const draftId = id(url.searchParams.get("draft_id"));
      if (!await sql(env.DB, "SELECT id FROM production_drafts WHERE id=? AND article_id=?", draftId, article).first()) throw new ProductionError("Draft not found.", 404);
      return respond((await sql(env.DB, "SELECT id,before_body,after_body,before_notes,after_notes,created_at FROM production_draft_revisions WHERE draft_id=? ORDER BY created_at DESC LIMIT 100", draftId).all()).results);
    }
    if (request.method !== "POST" || match[2] !== "command") throw new ProductionError("Production endpoint not found.", 404);
    if (request.headers.get("Origin") !== url.origin) throw new ProductionError("Request origin rejected.", 403);
    if (!request.headers.get("Content-Type")?.startsWith("application/json")) throw new ProductionError("JSON required.");
    const reader = request.body?.getReader(); if (!reader) throw new ProductionError("Request body required.");
    const chunks: Uint8Array[] = []; let size = 0;
    for (;;) { const { value, done } = await reader.read(); if (done) break; size += value.byteLength;
      if (size > 650000) { await reader.cancel(); throw new ProductionError("Request too large.", 413); } chunks.push(value); }
    const bytes = new Uint8Array(size); let offset = 0; for (const chunk of chunks) { bytes.set(chunk, offset); offset += chunk.length; }
    let input: unknown; try { input = JSON.parse(new TextDecoder().decode(bytes)); } catch { throw new ProductionError("Invalid JSON."); }
    return respond(await executeProductionCommand(env, article, actor, input, render));
  } catch (error) {
    if (error instanceof ProductionError) return respond({ error: error.message }, error.status);
    if (error instanceof InputError) return respond({ error: error.message }, 400);
    return respond({ error: "The production action failed. Existing work was not replaced. Check the server logs and retry the same action ID after recovery." }, 500);
  }
}
