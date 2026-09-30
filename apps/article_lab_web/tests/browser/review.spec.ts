import { test, expect } from "@playwright/test";
test("admin approval, assigned passage review, saved return, submit and admin inspection", async ({
  browser,
}) => {
  const admin = await browser.newPage({
      baseURL: "http://127.0.0.1:5173",
      viewport: { width: 1365, height: 1000 },
    }),
    reviewer = await browser.newPage({
      baseURL: "http://127.0.0.1:5173",
      viewport: { width: 1365, height: 1000 },
    });
  const errors: string[] = [];
  for (const p of [admin, reviewer])
    p.on("pageerror", (e) => errors.push(e.message));
  await reviewer.goto("/__local?user=jane@example.test");
  await expect(
    reviewer.getByRole("heading", {
      name: /Waiting for approval|Your articles/,
    }),
  ).toBeVisible();
  await admin.goto("/__local?user=owner@example.test");
  await admin.getByRole("button", { name: "Admin", exact: true }).click();
  const jane = admin
    .locator("table")
    .first()
    .getByRole("row")
    .filter({ hasText: "jane@example.test" });
  await expect(jane).toBeVisible();
  if (
    await jane.getByRole("button", { name: "Approve", exact: true }).count()
  ) {
    await jane.getByRole("button", { name: "Approve", exact: true }).click();
    await expect(jane).toContainText("approved");
  }
  const title = `Review fixture ${Date.now()}`;
  await admin.getByLabel("Title", { exact: true }).fill(title);
  await admin
    .getByLabel("Subtitle", { exact: true })
    .fill("A synthetic article for review");
  await admin
    .getByRole("tab", { name: "Markdown source", exact: true })
    .click();
  await admin
    .getByLabel("Markdown", { exact: true })
    .fill(
      "## The value of a second look\n\nA thoughtful reviewer helps an article become clearer. Select this passage and leave a specific suggestion.\n\nGood feedback tells the writer what works, as well as what needs another look.",
    );
  await admin
    .getByRole("button", { name: "Publish version", exact: true })
    .click();
  await expect(admin.getByRole("status")).toContainText("published");
  const option = await admin
    .locator("select[name=version_id]")
    .locator("option")
    .filter({ hasText: title })
    .getAttribute("value");
  await admin.locator("select[name=version_id]").selectOption(option!);
  const userOption = await admin
    .locator("select[name=user_id]")
    .locator("option")
    .filter({ hasText: "jane@example.test" })
    .getAttribute("value");
  await admin.locator("select[name=user_id]").selectOption(userOption!);
  await admin
    .getByRole("button", { name: "Assign article", exact: true })
    .click();
  await expect(admin.getByRole("status")).toContainText("assigned");
  await reviewer.reload();
  const row = reviewer.locator(".inbox-row").filter({ hasText: title });
  await row.getByRole("button", { name: "Review →", exact: true }).click();
  await expect(reviewer.getByLabel("Article text")).toContainText(
    "thoughtful reviewer",
  );
  await reviewer.getByLabel("Article text").evaluate((el) => {
    const p = el.querySelector("p")!,
      node = p.firstChild!;
    const r = document.createRange();
    r.setStart(node, 2);
    r.setEnd(node, 21);
    const s = window.getSelection()!;
    s.removeAllRanges();
    s.addRange(r);
    document.dispatchEvent(new Event("selectionchange"));
  });
  await reviewer
    .getByRole("button", { name: "Add comment", exact: true })
    .click();
  await reviewer
    .getByLabel("Comment 1", { exact: true })
    .fill("Could you add an example?");
  await reviewer
    .getByLabel("General feedback", { exact: true })
    .fill("Clear and useful. Please add a concrete example.");
  await expect(reviewer.getByRole("status")).toHaveText("All changes saved");
  await expect(reviewer.locator(".prose mark")).toHaveText(
    "thoughtful reviewer",
  );
  await reviewer.screenshot({
    path: "/tmp/article-review-desktop.png",
    fullPage: true,
  });
  await reviewer.reload();
  await reviewer
    .locator(".inbox-row")
    .filter({ hasText: title })
    .getByRole("button", { name: "Continue →", exact: true })
    .click();
  await expect(reviewer.getByLabel("Comment 1", { exact: true })).toHaveValue(
    "Could you add an example?",
  );
  await expect(
    reviewer.getByLabel("General feedback", { exact: true }),
  ).toHaveValue("Clear and useful. Please add a concrete example.");
  await reviewer.setViewportSize({ width: 390, height: 844 });
  await reviewer.screenshot({
    path: "/tmp/article-review-mobile.png",
    fullPage: true,
  });
  expect(
    await reviewer.evaluate(
      () => document.documentElement.scrollWidth <= innerWidth,
    ),
  ).toBe(true);
  await reviewer
    .getByRole("button", { name: "Submit feedback", exact: true })
    .click();
  await reviewer
    .getByRole("button", { name: "Confirm submission", exact: true })
    .click();
  await expect(reviewer.getByRole("status")).toHaveText(
    "Submitted · Read-only",
  );
  await expect(
    reviewer.getByLabel("General feedback", { exact: true }),
  ).toHaveAttribute("readonly", "");
  await admin.reload();
  await admin.getByRole("button", { name: "Admin", exact: true }).click();
  await admin
    .getByRole("row")
    .filter({ hasText: title })
    .getByRole("button", { name: "View feedback", exact: true })
    .click();
  await expect(admin.getByLabel("Comment 1", { exact: true })).toHaveValue(
    "Could you add an example?",
  );
  await expect(
    admin.getByLabel("General feedback", { exact: true }),
  ).toHaveValue("Clear and useful. Please add a concrete example.");
  await admin.screenshot({
    path: "/tmp/article-review-admin.png",
    fullPage: true,
  });
  expect(errors).toEqual([]);
  await admin.close();
  await reviewer.close();
});

test("save failures persist, retry restores D1, and a stale second device cannot overwrite", async ({
  browser,
}) => {
  const baseURL = "http://127.0.0.1:5173";
  const admin = await browser.newContext({
    baseURL,
    extraHTTPHeaders: {
      Cookie: "local-user=owner%40example.test",
      Origin: baseURL,
    },
  });
  const users = await (await admin.request.get("/api/admin/users")).json();
  const jane = users.find(
    (u: { email: string }) => u.email === "jane@example.test",
  );
  const title = `Concurrent draft ${Date.now()}`;
  const version = await (
    await admin.request.post("/api/admin/versions", {
      data: { title, body: "A **formatted passage** for reliable selection." },
    })
  ).json();
  await admin.request.post("/api/admin/assignments", {
    data: { version_id: version.id, user_id: jane.id },
  });
  const a = await browser.newPage({ baseURL }),
    b = await browser.newPage({ baseURL });
  for (const p of [a, b]) {
    await p.goto("/__local?user=jane@example.test");
    await p
      .locator(".inbox-row")
      .filter({ hasText: title })
      .getByRole("button")
      .click();
  }
  let failed = false;
  await a.route("**/api/reviews/*", async (route) => {
    if (route.request().method() === "PUT" && !failed) {
      failed = true;
      await route.abort("failed");
    } else await route.continue();
  });
  await a
    .getByLabel("General feedback", { exact: true })
    .fill("Preserve this through a failed save");
  await expect(a.getByRole("alert")).toContainText("Connection lost");
  await a.waitForTimeout(700);
  await expect(a.getByRole("alert")).toBeVisible();
  await a.getByRole("button", { name: "Retry save", exact: true }).click();
  await expect(a.getByRole("status")).toHaveText("All changes saved");
  await expect(a.getByRole("alert")).toHaveCount(0);
  await b
    .getByLabel("General feedback", { exact: true })
    .fill("Stale device changes");
  await expect(b.getByRole("alert")).toContainText("another tab or device");
  await expect(b.getByLabel("General feedback", { exact: true })).toHaveValue(
    "Stale device changes",
  );
  await a.reload();
  await a
    .locator(".inbox-row")
    .filter({ hasText: title })
    .getByRole("button")
    .click();
  await expect(a.getByLabel("General feedback", { exact: true })).toHaveValue(
    "Preserve this through a failed save",
  );
  await a.close();
  await b.close();
  await admin.close();
});
