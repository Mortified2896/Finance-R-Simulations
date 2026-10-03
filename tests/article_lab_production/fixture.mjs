import { DatabaseSync } from "node:sqlite";
import fs from "node:fs";
import { loadProduction, executeProductionCommand } from "../../apps/article_lab_web/worker/production.ts";
export function fixture() {
  const native = new DatabaseSync(":memory:");
  for (const name of ["0001_review.sql", "0003_generation_foundation.sql", "0004_image_assets.sql", "0005_production_workflow.sql"]) {
    native.exec(fs.readFileSync(new URL(`../../apps/article_lab_web/migrations/${name}`, import.meta.url), "utf8"));
  }
  class Statement {
    constructor(query, args = []) { this.query = query; this.args = args; }
    bind(...args) { return new Statement(this.query, args); }
    async first() { return native.prepare(this.query).get(...this.args) ?? null; }
    async all() { return { results: native.prepare(this.query).all(...this.args) }; }
    async run() { return { meta: native.prepare(this.query).run(...this.args) }; }
  }
  // Serialize batches as D1 does. Awaiting statement.run inside BEGIN would
  // otherwise manufacture impossible overlapping SQLite transactions in tests.
  let tail = Promise.resolve();
  const db = { prepare(query) { return new Statement(query); }, batch(statements) {
    const operation = tail.then(() => {
      native.exec("BEGIN");
      try { const result = statements.map((s) => ({ meta: native.prepare(s.query).run(...s.args) })); native.exec("COMMIT"); return result; }
      catch (e) { native.exec("ROLLBACK"); throw e; }
    });
    tail = operation.catch(() => {}); return operation;
  }};
  const actor = { id: crypto.randomUUID(), role: "admin", status: "approved" };
  native.prepare("INSERT INTO users(id,email,display_name,status,role,created_at,last_login_at) VALUES(?,?,?,?,?,?,?)").run(actor.id, "owner@example.test", "Owner", actor.status, actor.role, "now", "now");
  const article = crypto.randomUUID();
  native.prepare("INSERT INTO articles VALUES(?,?,?)").run(article, actor.id, "now");
  native.prepare("INSERT INTO article_workspaces(article_id,topic,brief,evidence,draft_body,created_at,updated_at) VALUES(?,?,?,?,?,?,?)").run(article, "Synthetic finance article", "Explain diversification", "Synthetic sources only", "Legacy draft preserved", "now", "now");
  const route = { id: "glm-test", provider: "glm", transport: "direct", model: "glm-test", response_models: ["glm-test"], label: "GLM test" };
  const env = { DB: db, ARTICLE_LAB_ROUTES: JSON.stringify([route]), ARTICLE_LAB_RUNNER_TOKEN: "test-only-not-a-real-secret-".repeat(2), ARTICLE_LAB_IMAGES: JSON.stringify({ models: ["gpt-image-1"] }), IMAGES: {} };
  const view = () => loadProduction(db, article, actor);
  const render = (body) => ({ rendered_html: `<p>${body.replaceAll("<", "&lt;")}</p>`, anchor_text: body });
  const send = async (action, values = {}, overrides = {}) => executeProductionCommand(env, article, actor, { id: crypto.randomUUID(), revision: (await view()).revision, action, ...values, ...overrides }, render);
  const add = async (kind, text, parent_id = null) => { const key = crypto.randomUUID(); await send("add_items", { kind, parent_id, items: [{ id: key, text }] }); return key; };
  const approve = (key) => send("item_status", { ids: [key], status: "approved" });
  const asset = () => { const key = crypto.randomUUID(); native.prepare("INSERT INTO image_assets(id,article_id,source,object_key,content_type,byte_size,width,height,requested_by,created_at,alt_text) VALUES(?,?,'upload',?,'image/png',100,400,300,?,'now','Synthetic image')").run(key, article, key, actor.id); return key; };
  const packageFlow = async () => {
    const title = await add("titles", "The Case for Diversification"); await approve(title);
    const subtitle = await add("subtitles", "Why spreading your investments changes the risk", title); await approve(subtitle);
    const image = asset(); await send("image", { package_id: subtitle, asset_id: image, operation: "approve" });
    const outline = await add("outline", "## Why diversify\nEvidence and caveats", subtitle); await approve(outline);
    return { title, subtitle, package_id: subtitle, image, outline };
  };
  return { native, db, env, actor, article, route, view, send, add, approve, asset, packageFlow, render };
}
