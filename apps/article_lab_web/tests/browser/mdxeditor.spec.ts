import { test, expect, type Page } from "@playwright/test";

const baseURL = "http://127.0.0.1:5173";

async function selectText(page: Page, selector: string, quote: string) {
  await page.locator(selector).evaluate((root, selected) => {
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    const nodes: Text[] = [];
    let node: Node | null;
    let text = "";
    while ((node = walker.nextNode())) { nodes.push(node as Text); text += node.textContent; }
    const start = text.indexOf(selected);
    if (start < 0) throw new Error(`Fixture quote not found: ${selected}`);
    const end = start + selected.length;
    const range = document.createRange();
    let offset = 0;
    let started = false;
    for (const current of nodes) {
      const next = offset + current.length;
      if (!started && start < next) { range.setStart(current, start - offset); started = true; }
      if (started && end <= next) { range.setEnd(current, end - offset); break; }
      offset = next;
    }
    const selection = window.getSelection()!;
    selection.removeAllRanges();
    selection.addRange(range);
    document.dispatchEvent(new Event("selectionchange"));
  }, quote);
}

test("visual draft, private snapshot comments, submitted editor review, version isolation", async ({ browser }) => {
  const ownerContext = await browser.newContext({ baseURL, viewport: { width: 1365, height: 1000 } });
  const janeContext = await browser.newContext({ baseURL });
  const paulContext = await browser.newContext({ baseURL, viewport: { width: 390, height: 844 } });
  const owner = await ownerContext.newPage();
  const jane = await janeContext.newPage();
  const paul = await paulContext.newPage();
  const errors: string[] = [];
  for (const page of [owner, jane, paul]) page.on("pageerror", (error) => errors.push(error.message));
  try {
    // All identities and data are synthetic, loopback-only fixtures.
    await jane.goto("/__local?user=jane@example.test");
    await paul.goto("/__local?user=paul@example.test");
    await owner.goto("/__local?user=owner@example.test");
    const users = await (await owner.request.get("/api/admin/users")).json();
    const janeUser = users.find((u: { email: string }) => u.email === "jane@example.test");
    const paulUser = users.find((u: { email: string }) => u.email === "paul@example.test");
    for (const user of [janeUser, paulUser]) {
      expect(user).toBeTruthy();
      const approved = await owner.request.patch(`/api/admin/users/${user.id}`, {
        headers: { Origin: baseURL }, data: { status: "approved" },
      });
      expect(approved.ok()).toBe(true);
    }
    await owner.getByRole("button", { name: "Admin", exact: true }).click();
    const title = `MDXEditor fixture ${Date.now()}`;
    await owner.getByLabel("Title", { exact: true }).fill(title);
    const editable = owner.locator(".article-draft-content[contenteditable=true]");
    await expect(editable).toBeVisible();
    await editable.fill("The article needs precise language and evidence.");
    await selectText(owner, ".article-draft-content", "precise language");
    await editable.press("Control+b");
    await owner.getByRole("tab", { name: "Markdown source", exact: true }).click();
    await expect(owner.getByLabel("Markdown", { exact: true })).toHaveValue(/\*\*precise language\*\*|__precise language__/);
    await owner.getByRole("tab", { name: "Review preview", exact: true }).click();
    await expect(owner.getByLabel("Review snapshot preview").locator("strong")).toHaveText("precise language");
    await owner.screenshot({ path: "/tmp/article-mdx-preview.png", fullPage: true });
    await owner.getByRole("tab", { name: "Visual editor", exact: true }).click();
    await owner.screenshot({ path: "/tmp/article-mdx-editor.png", fullPage: true });
    await owner.getByRole("button", { name: "Publish version", exact: true }).click();
    await expect(owner.getByRole("status")).toContainText("published");
    await expect(owner.locator("input[name=body]")).toHaveValue("");

    const versions = await (await owner.request.get("/api/admin/versions")).json();
    const v1 = versions.find((v: { title: string }) => v.title === title);
    expect(v1).toBeTruthy();
    for (const user of [janeUser, paulUser]) {
      const assigned = await owner.request.post("/api/admin/assignments", {
        headers: { Origin: baseURL }, data: { version_id: v1.id, user_id: user.id },
      });
      expect(assigned.ok()).toBe(true);
    }
    for (const page of [jane, paul]) {
      await page.reload();
      await page.locator(".inbox-row").filter({ hasText: title }).getByRole("button").click();
    }
    await selectText(jane, '[aria-label="Article text"]', "precise language and evidence");
    await jane.getByRole("button", { name: "Add comment", exact: true }).click();
    await jane.getByLabel("Comment 1", { exact: true }).fill("Jane's private feedback: give one example.");
    await expect(jane.getByRole("status")).toHaveText("All changes saved");
    await expect(jane.locator(".prose mark")).toHaveText(["precise language", " and evidence"]);
    await expect(paul.locator(".prose mark")).toHaveCount(0);
    await expect(paul.getByLabel("Comment 1", { exact: true })).toHaveCount(0);
    const janeAssignments = await (await jane.request.get("/api/assignments")).json();
    const janeAssignment = janeAssignments.find((a: { version_id: string }) => a.version_id === v1.id);
    const denied = await paul.request.post(`/api/reviews/${janeAssignment.id}/open`, {
      headers: { Origin: baseURL }, data: {},
    });
    expect(denied.status()).toBe(404);
    const adminDenied = await paul.request.get(`/api/admin/reviews/${janeAssignment.id}`);
    expect(adminDenied.status()).toBe(403);
    await jane.reload();
    await jane.locator(".inbox-row").filter({ hasText: title }).getByRole("button").click();
    await expect(jane.getByLabel("Comment 1", { exact: true })).toHaveValue("Jane's private feedback: give one example.");
    await jane.getByRole("button", { name: "Submit feedback", exact: true }).click();
    await jane.getByRole("button", { name: "Confirm submission", exact: true }).click();
    await expect(jane.getByRole("status")).toHaveText("Submitted · Read-only");
    await owner.reload();
    await owner.getByRole("button", { name: "Admin", exact: true }).click();
    await owner.getByRole("row").filter({ hasText: title }).filter({ hasText: "jane@example.test" })
      .getByRole("button", { name: "View feedback", exact: true }).click();
    await expect(owner.getByLabel("Comment 1", { exact: true })).toHaveValue("Jane's private feedback: give one example.");
    await owner.screenshot({ path: "/tmp/article-mdx-comments.png", fullPage: true });
    expect(await paul.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
    await paul.screenshot({ path: "/tmp/article-mdx-mobile-review.png", fullPage: true });

    const revised = await owner.request.post("/api/admin/versions", {
      headers: { Origin: baseURL },
      data: { article_id: v1.article_id, title, body: "An entirely revised second draft." },
    });
    expect(revised.status()).toBe(201);
    const v2 = await revised.json();
    expect(v2.id).not.toBe(v1.id);
    const v1Review = await (await owner.request.get(`/api/admin/reviews/${janeAssignment.id}`)).json();
    expect(v1Review.version.id).toBe(v1.id);
    expect(v1Review.review.annotations[0].exact_quote).toBe("precise language and evidence");
    const assignV2 = await owner.request.post("/api/admin/assignments", {
      headers: { Origin: baseURL }, data: { version_id: v2.id, user_id: janeUser.id },
    });
    expect(assignV2.ok()).toBe(true);
    const assignments = await (await jane.request.get("/api/assignments")).json();
    const v2Assignment = assignments.find((a: { version_id: string }) => a.version_id === v2.id);
    const openedV2 = await jane.request.post(`/api/reviews/${v2Assignment.id}/open`, {
      headers: { Origin: baseURL }, data: {},
    });
    const v2Review = await openedV2.json();
    expect(v2Review.review.annotations).toEqual([]);
    expect(errors).toEqual([]);
  } finally {
    await ownerContext.close();
    await janeContext.close();
    await paulContext.close();
  }
});

test("Markdown import/export and failed publication preserve the draft", async ({ page }) => {
  await page.goto("/__local?user=owner@example.test");
  await page.getByRole("button", { name: "Admin", exact: true }).click();
  await page.getByLabel("Title", { exact: true }).fill("Failed publication fixture");
  await page.getByLabel("Import .md", { exact: true }).setInputFiles({
    name: "draft.md", mimeType: "text/markdown", buffer: Buffer.from("## Imported heading\n\nKeep **this draft**."),
  });
  await expect(page.locator(".article-draft-content")).toContainText("Imported heading");
  const download = page.waitForEvent("download");
  await page.getByRole("button", { name: "Export .md", exact: true }).click();
  expect((await download).suggestedFilename()).toBe("article-draft.md");
  await page.route("**/api/admin/versions", async (route) => {
    if (route.request().method() === "POST") await route.fulfill({ status: 503, contentType: "application/json", body: '{"error":"Synthetic save failure"}' });
    else await route.continue();
  });
  await page.getByRole("button", { name: "Publish version", exact: true }).click();
  await expect(page.getByRole("alert")).toContainText("Synthetic save failure");
  await expect(page.locator("input[name=body]")).toHaveValue(/Keep \*\*this draft\*\*/);
});
