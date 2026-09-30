import { test, expect } from "@playwright/test";

test("admin sees every saved version and all submitted comments without self-assignment", async ({
  browser,
}) => {
  const baseURL = "http://127.0.0.1:5173";
  const owner = await browser.newPage({
    baseURL,
    viewport: { width: 1365, height: 1000 },
  });
  const jane = await browser.newPage({ baseURL });
  const paul = await browser.newPage({ baseURL });
  const errors: string[] = [];
  owner.on("pageerror", (e) => errors.push(e.message));
  owner.on("console", (m) => {
    if (m.type() === "error") errors.push(m.text());
  });
  try {
    await jane.goto("/__local?user=jane@example.test");
    await paul.goto("/__local?user=paul@example.test");
    await owner.goto("/__local?user=owner@example.test");
    await expect(
      owner.getByRole("heading", { name: "All articles", exact: true }),
    ).toBeVisible();
    const title = `Admin library ${Date.now()}`;
    const created = await owner.request.post("/api/admin/versions", {
      headers: { Origin: baseURL },
      data: { title, body: "An article everyone can review." },
    });
    expect(created.status()).toBe(201);
    const version = await created.json();
    await owner
      .getByRole("button", { name: "Refresh articles", exact: true })
      .click();
    let card = owner.locator(".inbox-row").filter({ hasText: title });
    await expect(card).toContainText("0 submitted / 0 assigned");
    await card
      .getByRole("button", { name: "Open article", exact: true })
      .click();
    await expect(owner.getByLabel("Article text")).toHaveText(
      "An article everyone can review.",
    );
    await expect(owner.getByLabel("Submitted feedback")).toContainText(
      "No submitted reviews yet",
    );
    expect(await (await owner.request.get("/api/assignments")).json()).toEqual(
      [],
    );
    const users = await (await owner.request.get("/api/admin/users")).json();
    for (const [page, email] of [
      [jane, "jane@example.test"],
      [paul, "paul@example.test"],
    ] as const) {
      const user = users.find((u: { email: string }) => u.email === email);
      expect(
        (
          await owner.request.patch(`/api/admin/users/${user.id}`, {
            headers: { Origin: baseURL },
            data: { status: "approved" },
          })
        ).ok(),
      ).toBe(true);
      expect(
        (
          await owner.request.post("/api/admin/assignments", {
            headers: { Origin: baseURL },
            data: { version_id: version.id, user_id: user.id },
          })
        ).ok(),
      ).toBe(true);
      await page.reload();
      await page
        .locator(".inbox-row")
        .filter({ hasText: title })
        .getByRole("button")
        .click();
      await page
        .getByLabel("General feedback", { exact: true })
        .fill(`General feedback from ${email}`);
      await page.getByLabel("Article text").evaluate((root) => {
        const range = document.createRange();
        range.setStart(root.firstChild!.firstChild!, 3);
        range.setEnd(root.firstChild!.firstChild!, 10);
        const selection = window.getSelection()!;
        selection.removeAllRanges();
        selection.addRange(range);
        document.dispatchEvent(new Event("selectionchange"));
      });
      await page
        .getByRole("button", { name: "Add comment", exact: true })
        .click();
      await page
        .getByLabel("Comment 1", { exact: true })
        .fill(`Inline feedback from ${email}`);
      await expect(page.getByRole("status")).toHaveText("All changes saved");
      expect(
        (await page.request.get(`/api/admin/versions/${version.id}`)).status(),
      ).toBe(403);
      if (page === jane) {
        await page
          .getByRole("button", { name: "Submit feedback", exact: true })
          .click();
        await page
          .getByRole("button", { name: "Confirm submission", exact: true })
          .click();
        await expect(page.getByRole("status")).toHaveText(
          "Submitted · Read-only",
        );
      }
    }
    await owner.getByRole("button", { name: "Back to articles" }).click();
    card = owner.locator(".inbox-row").filter({ hasText: title });
    await expect(card).toContainText("1 submitted / 2 assigned");
    await card
      .getByRole("button", { name: "Open article", exact: true })
      .click();
    await expect(owner.getByLabel("Submitted feedback")).toContainText(
      "Inline feedback from jane@example.test",
    );
    await expect(owner.getByLabel("Submitted feedback")).not.toContainText(
      "Inline feedback from paul@example.test",
    );
    await paul
      .getByRole("button", { name: "Submit feedback", exact: true })
      .click();
    await paul
      .getByRole("button", { name: "Confirm submission", exact: true })
      .click();
    await expect(paul.getByRole("status")).toHaveText("Submitted · Read-only");
    await owner.getByRole("button", { name: "Back to articles" }).click();
    await expect(card).toContainText("2 submitted / 2 assigned");
    await card
      .getByRole("button", { name: "Open article", exact: true })
      .click();
    for (const email of ["jane@example.test", "paul@example.test"]) {
      await expect(owner.getByLabel("Submitted feedback")).toContainText(
        `Inline feedback from ${email}`,
      );
      await expect(owner.getByLabel("Submitted feedback")).toContainText(
        `General feedback from ${email}`,
      );
    }
    await expect(
      owner.getByRole("button", { name: "View passage comment" }),
    ).toHaveCount(1);
    await owner
      .getByRole("button", { name: "View passage comment" })
      .press("Enter");
    await expect(owner.locator(".comment[id]").first()).toBeFocused();
    await owner.screenshot({
      path: "/tmp/article-admin-overview-desktop.png",
      fullPage: true,
    });
    await owner
      .getByRole("button", { name: "View review", exact: true })
      .first()
      .click();
    await expect(
      owner.getByLabel("Comment 1", { exact: true }),
    ).toHaveAttribute("readonly", "");
    await owner.getByRole("button", { name: "Back", exact: false }).click();
    await expect(owner.getByLabel("Submitted feedback")).toBeVisible();
    await owner.setViewportSize({ width: 390, height: 844 });
    expect(
      await owner.evaluate(
        () => document.documentElement.scrollWidth <= innerWidth,
      ),
    ).toBe(true);
    await owner.screenshot({
      path: "/tmp/article-admin-overview-mobile.png",
      fullPage: true,
    });
    // Existing owner assignments remain reachable for finishing old drafts.
    await owner.getByRole("button", { name: "Back to articles" }).click();
    const ownerUser = users.find(
      (u: { email: string }) => u.email === "owner@example.test",
    );
    expect(
      (
        await owner.request.post("/api/admin/assignments", {
          headers: { Origin: baseURL },
          data: { version_id: version.id, user_id: ownerUser.id },
        })
      ).ok(),
    ).toBe(true);
    await owner
      .getByRole("button", { name: "Refresh articles", exact: true })
      .click();
    await card
      .getByRole("button", { name: "Your review", exact: true })
      .click();
    await owner
      .getByLabel("General feedback", { exact: true })
      .fill("Owner's existing personal review");
    await owner
      .getByRole("button", { name: "Submit feedback", exact: true })
      .click();
    await owner
      .getByRole("button", { name: "Confirm submission", exact: true })
      .click();
    await expect(owner.getByRole("status")).toHaveText("Submitted · Read-only");
    await owner.getByRole("button", { name: "Back", exact: false }).click();
    await expect(card).toContainText("3 submitted / 3 assigned");
    await card
      .getByRole("button", { name: "Open article", exact: true })
      .click();
    await expect(owner.getByLabel("Submitted feedback")).toContainText(
      "Owner's existing personal review",
    );
    expect(errors).toEqual([]);
  } finally {
    await owner.close();
    await jane.close();
    await paul.close();
  }
});
