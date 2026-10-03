import test from "node:test";
import assert from "node:assert/strict";
import { fixture } from "./fixture.mjs";
import { handleProductionRequest, executeProductionCommand, loadProduction } from "../../apps/article_lab_web/worker/production.ts";
import { EMPTY_PUBLISH, itemText, publishSettings, resolvedPrompt, writingContext, inputSnapshot } from "../../apps/article_lab_web/shared/production.ts";
const draft = async (f, outline, body = "A synthetic article body.") => {
  const key = crypto.randomUUID(); await f.send("new_draft", { draft_id: key, outline_id: outline, body }); return key;
};

test("additive schema preserves existing article and legacy working draft", async () => {
  const f = fixture(), d = await f.view(); assert.equal(d.workspace.draft_body, "Legacy draft preserved"); assert.equal(d.items.length, 0);
  assert.equal(f.native.prepare("SELECT COUNT(*) n FROM articles").get().n, 1);
});
test("complete manual title -> subtitle -> image -> outline -> draft -> immutable reviewer snapshot", async () => {
  const f = fixture(), p = await f.packageFlow(), key = await draft(f, p.outline);
  await f.send("draft_status", { draft_id: key, status: "approved" });
  await f.send("save_publishing", { draft_id: key, settings: { ...EMPTY_PUBLISH, tags: ["Investing"], alt_text: "An editorial image", target: "medium_profile" } });
  const version = crypto.randomUUID(); await f.send("publish_snapshot", { draft_id: key, version_id: version });
  const view = await f.view(); assert.equal(view.snapshots[0].version_id, version);
  const row = f.native.prepare("SELECT * FROM article_versions WHERE id=?").get(version);
  assert.equal(row.article_id, f.article); assert.match(row.body, /An editorial image/); assert.match(row.body, /synthetic article body/);
  assert.equal(f.native.prepare("SELECT image_asset_id FROM version_assets WHERE version_id=?").get(version).image_asset_id, p.image);
});
test("manual titles and subtitles are real candidates with notes, not fake generation jobs", async () => {
  const f = fixture(), title = await f.add("titles", "A manual title"); await f.approve(title);
  const sub = await f.add("subtitles", "A manual subtitle", title);
  await f.send("edit_item", { item_id: sub, text: "A revised manual subtitle", notes: "Human judgment" });
  const row = (await f.view()).items.find((i) => i.id === sub); assert.equal(row.source_candidate_id, null); assert.equal(row.notes, "Human judgment");
  assert.equal(f.native.prepare("SELECT COUNT(*) n FROM generation_jobs").get().n, 0);
});
test("multiple approved titles and subtitles produce independent packages", async () => {
  const f = fixture(), t1 = await f.add("titles", "First title"), t2 = await f.add("titles", "Second title");
  await f.send("item_status", { ids: [t1, t2], status: "approved" });
  const s1 = await f.add("subtitles", "First subtitle", t1), s2 = await f.add("subtitles", "Second subtitle", t2);
  await f.send("item_status", { ids: [s1, s2], status: "approved" }); const rows = (await f.view()).packages;
  assert.equal(rows.length, 2); assert.equal(rows.find((p) => p.id === s1).title_id, t1); assert.equal(rows.find((p) => p.id === s2).title_id, t2);
});
test("cannot bypass title approval, resurrect archived items or cross article boundaries", async () => {
  const f = fixture(), title = await f.add("titles", "Not approved");
  await assert.rejects(() => f.add("subtitles", "Premature", title), { status: 409 });
  await f.send("item_status", { ids: [title], status: "archived" }); await assert.rejects(() => f.approve(title), { status: 409 });
  await assert.rejects(() => f.send("item_status", { ids: [crypto.randomUUID()], status: "approved" }), { status: 404 });
  await f.send("item_status", { ids: [title], status: "candidate" }); await f.approve(title);
});
test("bulk approval is all-or-nothing when one row is invalid", async () => {
  const f = fixture(), a = await f.add("titles", "Good candidate"), b = await f.add("titles", "Archived candidate");
  await f.send("item_status", { ids: [b], status: "archived" });
  await assert.rejects(() => f.send("item_status", { ids: [a,b], status: "approved" }), { status: 409 });
  assert.equal((await f.view()).items.find((r) => r.id === a).status, "candidate");
});
test("archive and restore retain original content and notes", async () => {
  const f = fixture(), title = await f.add("titles", "Original title");
  await f.send("edit_item", { item_id: title, text: "Edited title", notes: "Keep this" });
  await f.send("item_status", { ids: [title], status: "archived" }); await f.send("item_status", { ids: [title], status: "candidate" });
  const row = (await f.view()).items[0]; assert.equal(row.original_text, "Original title"); assert.equal(row.text, "Edited title"); assert.equal(row.notes, "Keep this");
});
test("subtitle length is the Shiny 90-character rule and text is never truncated", () => {
  assert.throws(() => itemText("x".repeat(91), "subtitles")); assert.equal(itemText("  Clear   subtitle ", "subtitles"), "Clear subtitle");
  assert.equal([...itemText("😀".repeat(140), "titles")].length, 140);
});
test("one approved thumbnail per package and selected image cannot be archived", async () => {
  const f = fixture(), p = await f.packageFlow(), second = f.asset();
  await assert.rejects(() => f.send("image", { package_id: p.package_id, asset_id: p.image, operation: "archive" }), { status: 409 });
  await f.send("image", { package_id: p.package_id, asset_id: second, operation: "approve" });
  const d = await f.view(); assert.equal(d.packages[0].image_asset_id, second); assert.equal(d.items.find((i) => i.id === p.outline).status, "candidate");
  await f.send("image", { package_id: p.package_id, asset_id: p.image, operation: "archive" }); assert.equal((await f.view()).image_links.find((l) => l.image_asset_id === p.image).archived, 1);
});
test("outline alternatives editable; approving another demotes its sibling", async () => {
  const f = fixture(), p = await f.packageFlow(), second = await f.add("outline", "## Another structure", p.package_id);
  await f.send("edit_item", { item_id: second, text: "## Revised structure", notes: "Check sequence" }); await f.approve(second);
  const view = await f.view(); assert.equal(view.items.find((i) => i.id === second).status, "approved"); assert.equal(view.items.find((i) => i.id === p.outline).status, "candidate");
});
test("reject approving two outlines of the same package in one action", async () => {
  const f = fixture(), p = await f.packageFlow(), second = await f.add("outline", "## Alternative", p.package_id);
  await assert.rejects(() => f.send("item_status", { ids: [p.outline, second], status: "approved" }));
});
test("draft variants keep originals, manual revision history and one approved sibling", async () => {
  const f = fixture(), p = await f.packageFlow(), a = await draft(f, p.outline, "First original"), b = await draft(f, p.outline, "Second original");
  await f.send("edit_draft", { draft_id: a, body: "First edited", notes: "New notes" });
  await f.send("draft_status", { draft_id: a, status: "approved" }); await f.send("draft_status", { draft_id: b, status: "approved" });
  const d = await f.view(); assert.equal(d.drafts.find((r) => r.id === a).original_body, "First original"); assert.equal(d.drafts.find((r) => r.id === a).status, "draft");
  assert.equal(d.drafts.find((r) => r.id === b).status, "approved");
  const rev = f.native.prepare("SELECT * FROM production_draft_revisions WHERE draft_id=?").get(a); assert.equal(rev.before_body, "First original"); assert.equal(rev.after_body, "First edited");
  assert.throws(() => f.native.prepare("UPDATE production_draft_revisions SET after_body='bad'").run());
});
test("upstream title changes invalidate downstream approvals but never frozen reviewer versions", async () => {
  const f = fixture(), p = await f.packageFlow(), key = await draft(f, p.outline); await f.send("draft_status", { draft_id: key, status: "approved" });
  const version = crypto.randomUUID(); await f.send("publish_snapshot", { draft_id: key, version_id: version });
  const frozen = f.native.prepare("SELECT * FROM article_versions WHERE id=?").get(version);
  await f.send("edit_item", { item_id: p.title, text: "A different title", notes: "Revisit downstream" });
  const d = await f.view(); assert.equal(d.packages[0].image_asset_id, null); assert.equal(d.drafts[0].status, "draft"); assert.equal(d.items.find((i) => i.id === p.outline).status, "candidate");
  assert.deepEqual(f.native.prepare("SELECT * FROM article_versions WHERE id=?").get(version), frozen);
  await assert.rejects(() => f.send("publish_snapshot", { draft_id: key, version_id: crypto.randomUUID() }), { status: 409 });
});
test("stale parent snapshots require explicit edit/acknowledgment before approval", async () => {
  const f = fixture(), title = await f.add("titles", "Old title"); await f.approve(title); const sub = await f.add("subtitles", "Subtitle for old title", title);
  await f.send("edit_item", { item_id: title, text: "New title", notes: "" }); await f.approve(title);
  await assert.rejects(() => f.approve(sub), { status: 409 });
  await f.send("edit_item", { item_id: sub, text: "Subtitle for old title", notes: "Reviewed", acknowledge_context: true }); await f.approve(sub);
});
test("stale editor save rejects without losing the winning text", async () => {
  const f = fixture(), key = await f.add("titles", "Initial"), revision = (await f.view()).revision;
  await f.send("edit_item", { item_id: key, text: "Winning edit", notes: "" });
  await assert.rejects(() => f.send("edit_item", { item_id: key, text: "Stale edit", notes: "" }, { revision }), { status: 409 });
  assert.equal((await f.view()).items[0].text, "Winning edit");
});
test("identical action replay is idempotent and conflicting IDs are rejected", async () => {
  const f = fixture(); const command = { id: crypto.randomUUID(), revision: 0, action: "add_items", kind: "titles", items: [{ id: crypto.randomUUID(), text: "Exactly once" }] };
  await executeProductionCommand(f.env,f.article,f.actor,command); const retry = await executeProductionCommand(f.env,f.article,f.actor,command); assert.equal(retry.duplicate, true);
  await assert.rejects(() => executeProductionCommand(f.env,f.article,f.actor,{ ...command, items: [{ id: crypto.randomUUID(), text: "Different" }] }), { status: 409 });
  assert.equal((await f.view()).items.length,1);
});
test("simultaneous identical commands apply once, not twice", async () => {
  const f = fixture(); await f.view(); const command = { id: crypto.randomUUID(), revision: 0, action: "add_items", kind: "titles", items: [{ id: crypto.randomUUID(), text: "Same action" }] };
  const results = await Promise.allSettled([executeProductionCommand(f.env,f.article,f.actor,command), executeProductionCommand(f.env,f.article,f.actor,command)]);
  assert.equal(results.filter((r) => r.status === "fulfilled").length, 2); assert.equal((await f.view()).items.length,1);
});
test("simultaneous distinct edits accept one and reject the stale one", async () => {
  const f = fixture(), key = await f.add("titles", "Initial"), rev = (await f.view()).revision;
  const results = await Promise.allSettled(["One","Two"].map((text) => executeProductionCommand(f.env,f.article,f.actor,{ id: crypto.randomUUID(), revision: rev, action: "edit_item", item_id:key,text,notes:"" })));
  assert.equal(results.filter((r) => r.status === "fulfilled").length,1); assert.equal(results.filter((r) => r.status === "rejected")[0].reason.status,409);
});
test("generated batches link results to the original selected parent and preserve exact request", async () => {
  const f = fixture(), title = await f.add("titles", "A specific title"); await f.approve(title);
  const job = crypto.randomUUID(); const config = { route_id:f.route.id,count:2,prompt:"Generate two subtitles",directions:"Keep them clear" };
  await f.send("generate", { kind:"subtitles",settings:config,jobs:[{id:job,parent_id:title}] });
  const queued = f.native.prepare("SELECT * FROM generation_jobs WHERE id=?").get(job); assert.match(JSON.parse(queued.request_json).resolved_prompt,/A specific title/); assert.equal(queued.status,"queued");
  const source = crypto.randomUUID(); f.native.prepare("INSERT INTO generation_candidates(id,article_id,job_id,kind,value,created_at) VALUES(?,?,?,'subtitles',?,'now')").run(source,f.article,job,"A generated subtitle");
  await f.send("sync"); await f.send("sync"); const item = (await f.view()).items.find((i) => i.source_candidate_id===source);
  assert.equal(item.parent_id,title); assert.equal(item.status,"candidate"); assert.equal((await f.view()).items.filter((i)=>i.source_candidate_id===source).length,1);
});
test("multiple generation targets queue atomically; unknown routes fail closed", async () => {
  const f = fixture(), a=await f.add("titles","First title"),b=await f.add("titles","Second title"); await f.send("item_status",{ids:[a,b],status:"approved"});
  const settings={route_id:f.route.id,count:2,prompt:"Subtitles",directions:""};
  await f.send("generate",{kind:"subtitles",settings,jobs:[{id:crypto.randomUUID(),parent_id:a},{id:crypto.randomUUID(),parent_id:b}]});
  assert.equal(f.native.prepare("SELECT COUNT(*) n FROM generation_jobs").get().n,2);
  await assert.rejects(()=>f.send("generate",{kind:"titles",settings:{...settings,route_id:"paid-fallback"},jobs:[{id:crypto.randomUUID(),parent_id:null}]}));
});
test("unknown old candidate parents are not guessed; adoption is explicit", async () => {
  const f=fixture(),t=await f.add("titles","A title"); await f.approve(t);
  const j=crypto.randomUUID(),c=crypto.randomUUID(); f.native.prepare("INSERT INTO generation_jobs(id,article_id,requested_by,kind,request_json,request_hash,route_json,route_id,created_at) VALUES(?,?,?,'subtitles','{}','test','{}','test','now')").run(j,f.article,f.actor.id);
  f.native.prepare("INSERT INTO generation_candidates(id,article_id,job_id,kind,value,created_at) VALUES(?,?,?,'subtitles','Legacy subtitle','now')").run(c,f.article,j);
  await f.send("sync"); assert.equal((await f.view()).unassigned.length,1);
  await f.send("adopt_candidates",{kind:"subtitles",parent_id:t,ids:[c]}); assert.equal((await f.view()).items.find((r)=>r.id===c).parent_id,t); assert.equal((await f.view()).unassigned.length,0);
});
test("paid image jobs require storage and explicit configured image model, preserving package links", async () => {
  const f=fixture(),p=await f.packageFlow(); const key=crypto.randomUUID();
  await f.send("generate_images",{package_id:p.package_id,job_ids:[key],prompt:"Editorial concept",model:"gpt-image-1",size:"1536x1024",quality:"medium"});
  assert.equal(f.native.prepare("SELECT parent_id FROM production_job_targets WHERE job_id=?").get(key).parent_id,p.package_id);
  f.env.IMAGES=undefined; await assert.rejects(()=>f.send("generate_images",{package_id:p.package_id,job_ids:[crypto.randomUUID()],prompt:"Concept",model:"gpt-image-1"}),{status:503});
});
test("publishing validation, metadata persistence and timestamps do not auto-publish externally", async () => {
  const f=fixture(),p=await f.packageFlow(),key=await draft(f,p.outline);
  assert.throws(()=>publishSettings({...EMPTY_PUBLISH,tags:["1","2","3","4","5","6"]})); assert.throws(()=>publishSettings({...EMPTY_PUBLISH,canonical_url:"javascript:alert(1)"}));
  await f.send("save_publishing",{draft_id:key,settings:{...EMPTY_PUBLISH,status:"submitted",publication:"Example publication"}});
  const initial=(await f.view()).publishing[0].submitted_at; await f.send("save_publishing",{draft_id:key,settings:{...EMPTY_PUBLISH,status:"published"}});
  const after=(await f.view()).publishing[0]; assert.equal(after.submitted_at,initial); assert.ok(after.published_at); assert.equal((await f.view()).snapshots.length,0);
});
test("snapshot repeated action creates one immutable version and later edits cannot rewrite it", async () => {
  const f=fixture(),p=await f.packageFlow(),key=await draft(f,p.outline); await f.send("draft_status",{draft_id:key,status:"approved"});
  const cmd={id:crypto.randomUUID(),revision:(await f.view()).revision,action:"publish_snapshot",draft_id:key,version_id:crypto.randomUUID()};
  await executeProductionCommand(f.env,f.article,f.actor,cmd,f.render); await executeProductionCommand(f.env,f.article,f.actor,cmd,f.render);
  assert.equal(f.native.prepare("SELECT COUNT(*) n FROM article_versions").get().n,1);
  await f.send("edit_draft",{draft_id:key,body:"Changed article",notes:""}); assert.equal((await f.view()).drafts[0].status,"draft");
  assert.throws(()=>f.native.prepare("UPDATE article_versions SET body='wrong'").run());
});
test("cross-article assets and malformed image links cannot enter reviewer snapshots", async () => {
  const f=fixture(),p=await f.packageFlow(),key=await draft(f,p.outline,"A draft ![bad](https://foreign.example/image.png)"); await f.send("draft_status",{draft_id:key,status:"approved"});
  await assert.rejects(()=>f.send("publish_snapshot",{draft_id:key,version_id:crypto.randomUUID()}));
  await assert.rejects(()=>f.send("image",{package_id:p.package_id,asset_id:crypto.randomUUID(),operation:"approve"}),{status:404});
});
test("prompt settings/templates persist per stage, with duplicate name checks", async () => {
  const f=fixture(),tid=crypto.randomUUID(); await f.send("save_template",{template_id:tid,kind:"titles",name:"Clear titles",prompt:"Be specific"});
  await f.send("save_settings",{kind:"titles",settings:{route_id:f.route.id,count:12,prompt:"Be specific",directions:"No hype"}});
  let d=await f.view(); assert.equal(d.templates[0].prompt,"Be specific"); assert.equal(d.settings.titles.directions,"No hype");
  await assert.rejects(()=>f.send("save_template",{template_id:crypto.randomUUID(),kind:"titles",name:"clear titles",prompt:"Other"}),{status:409});
  await f.send("delete_template",{template_id:tid}); assert.equal((await f.view()).templates.length,0);
});
test("ChatGPT Pro context contains approved exact outline and evidence, no invented findings",async()=>{
  const f=fixture(),p=await f.packageFlow(),d=await f.view(); const output=writingContext(d,d.items.find(i=>i.id===p.outline));
  assert.match(output,/Synthetic sources only/); assert.match(output,/## Why diversify/); assert.match(output,/Do not invent/);
});
for(const changed of [{role:"reviewer"},{status:"pending"},{status:"disabled"}]) test(`production API rejects ${JSON.stringify(changed)}`,async()=>{
  const f=fixture(); const response=await handleProductionRequest(new Request(`https://lab.example/api/admin/lab/production/${f.article}`),f.env,{...f.actor,...changed},f.render); assert.equal(response.status,403);
});
test("production writes require same-origin JSON; machine credentials do not make an admin",async()=>{
  const f=fixture(),url=`https://lab.example/api/admin/lab/production/${f.article}/command`;
  for(const [headers,status] of [[{"Content-Type":"application/json"},403],[{Origin:"https://evil.example","Content-Type":"application/json"},403],[{Origin:"https://lab.example","Content-Type":"text/plain"},400]]) {
    const res=await handleProductionRequest(new Request(url,{method:"POST",headers,body:"{}"}),f.env,f.actor); assert.equal(res.status,status);
  }
});
test("reference integrity rejects wrong article parents outside the API too",async()=>{
  const f=fixture(),key=await f.add("titles","A title"),other=crypto.randomUUID(); f.native.prepare("INSERT INTO articles VALUES(?,?,?)").run(other,f.actor.id,"now");
  assert.throws(()=>f.native.prepare("INSERT INTO production_items(id,article_id,kind,parent_id,original_text,text,created_at,updated_at) VALUES(?,?,'subtitles',?,'X','X','now','now')").run(crypto.randomUUID(),other,key));
});

test("title edits reopen approved subtitles rather than silently reusing old title context", async () => {
  const f=fixture(),p=await f.packageFlow();
  await f.send("edit_item",{item_id:p.title,text:"A different investment question",notes:""});
  let d=await f.view();assert.equal(d.items.find(i=>i.id===p.subtitle).status,"candidate");
  await f.approve(p.title);
  await assert.rejects(()=>f.approve(p.subtitle),{status:409});
  await f.send("edit_item",{item_id:p.subtitle,text:d.items.find(i=>i.id===p.subtitle).text,notes:"Reviewed",acknowledge_context:true});
  await f.approve(p.subtitle);d=await f.view();assert.equal(d.items.find(i=>i.id===p.subtitle).status,"approved");
});
test("reapproving an already approved outline does not demote an approved draft", async () => {
  const f=fixture(),p=await f.packageFlow(),key=await draft(f,p.outline);
  await f.send("draft_status",{draft_id:key,status:"approved"});await f.approve(p.outline);
  assert.equal((await f.view()).drafts.find(d=>d.id===key).status,"approved");
});
test("image metadata edits persist before freezing and reject after snapshot creation", async () => {
  const f=fixture(),p=await f.packageFlow();
  await f.send("asset_metadata",{asset_id:p.image,alt_text:"A deliberate editorial image",caption:"Original illustration"});
  assert.equal((await f.view()).assets.find(a=>a.id===p.image).alt_text,"A deliberate editorial image");
  const key=await draft(f,p.outline);await f.send("draft_status",{draft_id:key,status:"approved"});
  await f.send("publish_snapshot",{draft_id:key,version_id:crypto.randomUUID()});
  await assert.rejects(()=>f.send("asset_metadata",{asset_id:p.image,alt_text:"changed",caption:""}),{status:409});
});
test("oversized linked actions fail atomically instead of exhausting D1 query allowance", async () => {
  const f=fixture(),titles=[];
  for(let i=0;i<26;i++)titles.push(await f.add("titles",`Synthetic title ${i}`));
  const before=await f.view();
  await assert.rejects(()=>f.send("item_status",{ids:titles,status:"approved"}),/too many linked changes/);
  const after=await f.view();assert.equal(after.revision,before.revision);assert.ok(after.items.every(i=>i.status==="candidate"));
});
