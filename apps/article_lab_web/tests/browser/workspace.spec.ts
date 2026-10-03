import { test, expect } from "@playwright/test";
import { execFileSync } from "node:child_process";

const baseURL = "http://127.0.0.1:5173";
const RUNNER_TOKEN = "local-test-runner-token-0123456789abcdef";

/** Minimal valid PNG with a 640×400 IHDR, accepted by the worker's sniffer. */
function png(): Buffer {
  const bytes = Buffer.alloc(29);
  Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]).copy(bytes, 0);
  bytes.writeUInt32BE(13, 8);
  bytes.set([0x49, 0x48, 0x44, 0x52], 12);
  bytes.writeUInt32BE(640, 16);
  bytes.writeUInt32BE(400, 20);
  return bytes;
}

/** Simulate the RTX runner against the local harness machine API. */
function runMachine(
  path: string,
  payload: unknown,
): { status: number; body: any } {
  const result = execFileSync("curl", [
    "-sS",
    "-w",
    "\n%{http_code}",
    "-X",
    "POST",
    `${baseURL}/api/generation-runner${path}`,
    "-H",
    `Authorization: Bearer ${RUNNER_TOKEN}`,
    "-H",
    "Content-Type: application/json",
    "-d",
    JSON.stringify(payload),
  ])
    .toString()
    .trimEnd();
  const index = result.lastIndexOf("\n");
  return {
    status: Number(result.slice(index + 1)),
    body: JSON.parse(result.slice(0, index) || "{}"),
  };
}

test("writing workspace: brief, candidates, selection, image, draft import, publish, refresh persistence", async ({
  browser,
}) => {
  test.setTimeout(150_000);
  const owner = await browser.newPage({
    baseURL,
    viewport: { width: 1365, height: 1000 },
  });
  const errors: string[] = [];
  owner.on("pageerror", (e) => errors.push(e.message));
  owner.on("console", (m) => {
    // Network 4xx logs are expected: the flow deliberately triggers a 409/400.
    if (m.type() === "error" && !m.text().includes("Failed to load resource"))
      errors.push(m.text());
  });
  owner.on("response", async (response) => {
    const url = response.url();
    if (
      url.includes("/api/admin/") &&
      !url.endsWith("/api/me") &&
      ["POST", "PUT", "PATCH"].includes(response.request().method())
    )
      console.log(
        "LAB CALL:",
        response.status(),
        response.request().method(),
        url.replace(/^.*\/api\//, "/api/"),
        (await response.text()).slice(0, 100),
      );
  });
  try {
    await owner.goto("/__local?user=owner@example.test");
    await owner.getByRole("button", { name: "Writing", exact: true }).click();
    await expect(
      owner.getByRole("heading", { name: "Writing workspace" }),
    ).toBeVisible();

    // Create a workspace and persist the brief.
    const topic = `Index funds deep dive ${Date.now()}`;
    await owner.getByPlaceholder("Working title", { exact: true }).fill(topic);
    await owner.getByRole("button", { name: "Create workspace" }).click();
    await expect(
      owner.getByText("Workspace created", { exact: false }),
    ).toBeVisible();
    await owner
      .getByRole("textbox", { name: "Core brief" })
      .fill("Beginner guide to index funds: low fees, broad market exposure.");
    await owner
      .getByRole("textbox", { name: "Evidence & notes" })
      .fill(
        "Average fee 0.05% vs 1% active. Vanguard founded the first index fund in 1976.",
      );
    await owner
      .getByRole("button", { name: "Save workspace", exact: true })
      .first()
      .click();
    await expect(owner.getByText("All changes saved")).toBeVisible();

    // Unsaved-edit safety: typing marks dirty, reload warns, edits survive cancel.
    await owner
      .getByRole("textbox", { name: "Core brief" })
      .fill("Edited but not saved yet.");
    let warned = false;
    owner.on("dialog", (dialog) => {
      warned = true;
      void dialog.accept();
    });
    await owner.reload();
    expect(warned).toBe(true);
    await expect(
      owner.getByText("Edited but not saved yet.", { exact: false }),
    ).toHaveCount(0);

    // Reload resets to the library; reopen the workspace to continue.
    await owner.getByRole("button", { name: "Writing", exact: true }).click();
    const reopenValue = await owner
      .getByRole("combobox")
      .first()
      .locator("option", { hasText: topic })
      .getAttribute("value");
    await owner.getByRole("combobox").first().selectOption(reopenValue!);
    await expect(
      owner.getByRole("textbox", { name: "Core brief" }),
    ).toHaveValue(
      "Beginner guide to index funds: low fees, broad market exposure.",
    );

    // Titles: generate two candidates via the real queue + simulated runner.
    await owner
      .getByRole("button", { name: "Generate", exact: true })
      .first()
      .click();
    await expect(
      owner.getByText(/Queued — waiting for the runner/),
    ).toBeVisible();
    const claim = runMachine("/claim", {
      runner_id: "e2e-runner",
      route_ids: ["glm-omniroute"],
    });
    expect(claim.status).toBe(200);
    expect(claim.body.job).toBeTruthy();
    await expect(
      owner
        .getByText("Running on the subscription route", { exact: false })
        .first(),
    ).toBeVisible({ timeout: 12_000 });
    const completed = runMachine(`/jobs/${claim.body.job.id}/complete`, {
      lease_token: claim.body.job.lease_token,
      actual_model: "glm-5.3",
      output: JSON.stringify({
        candidates: [
          "Index Funds Explained: The Beginner's Quiet Path to Wealth",
          "Why Low-Cost Index Funds Beat Stock Picking",
        ],
      }),
    });
    expect(completed.status).toBe(200);
    await expect(
      owner.getByText("Index Funds Explained", { exact: false }),
    ).toBeVisible({ timeout: 15000 });
    await expect(
      owner.getByText("model not recorded", { exact: false }),
    ).toHaveCount(0);
    await expect(
      owner.getByText(/actual_model|glm-5\.3 ·/).first(),
    ).toBeVisible();

    // Generate more appends candidates instead of replacing them. Wait for the
    // poller to reflect the finished first job so the button is enabled again.
    await expect(owner.getByText(/^● Completed/).first()).toBeVisible({
      timeout: 12_000,
    });
    await owner
      .getByRole("button", { name: "Generate", exact: true })
      .first()
      .click();
    await expect(
      owner.getByText(/Queued — waiting for the runner/).first(),
    ).toBeVisible({ timeout: 12_000 });
    const claim2 = runMachine("/claim", {
      runner_id: "e2e-runner",
      route_ids: ["glm-omniroute"],
    });
    expect(claim2.body.job).toBeTruthy();
    runMachine(`/jobs/${claim2.body.job.id}/complete`, {
      lease_token: claim2.body.job.lease_token,
      actual_model: "glm-5.3",
      output: JSON.stringify({
        candidates: ["A Third Title About Index Funds"],
      }),
    });
    await expect(
      owner.getByText("A Third Title About Index Funds"),
    ).toBeVisible({
      timeout: 15000,
    });
    await expect(
      owner.getByText("Index Funds Explained", { exact: false }),
    ).toBeVisible();

    // Select one, archive another, then restore it.
    await owner.getByRole("radio", { name: /Index Funds Explained/ }).click();
    await expect(owner.getByText("Selection saved.")).toBeVisible();
    await owner
      .locator(".candidate")
      .filter({ hasText: "Why Low-Cost Index Funds Beat Stock Picking" })
      .getByRole("button", { name: "Archive", exact: true })
      .click();
    await expect(
      owner.getByText("Archived. Restore it any time", { exact: false }),
    ).toBeVisible();
    await owner.getByText(/Archived \(recoverable\)/).click();
    await owner
      .getByRole("button", { name: "Restore", exact: true })
      .first()
      .click();
    await expect(owner.getByText("Restored.")).toBeVisible();

    // Image upload with alt text, then select as draft thumbnail.
    await owner.setInputFiles('input[type="file"][accept*="image"]', {
      name: "cover.png",
      mimeType: "image/png",
      buffer: png(),
    });
    await expect(
      owner.getByText("Image uploaded", { exact: false }),
    ).toBeVisible();
    const assetCard = owner.locator(".asset-card").first();
    await assetCard
      .locator('input[placeholder="Alt text"]')
      .fill("Line chart of index fund growth");
    await assetCard.getByRole("button", { name: "Save details" }).click();
    await expect(owner.getByText("Image details saved.")).toBeVisible();
    await assetCard
      .getByRole("button", { name: /Use as draft thumbnail/ })
      .click();
    await expect(owner.getByText("Draft thumbnail saved.")).toBeVisible();

    // Import the "ChatGPT" markdown draft through the composer and publish.
    await owner.getByRole("button", { name: "Copy writing context" }).click();
    const draft = [
      "## Why index funds win",
      "",
      "Low fees compound. A 1% fee can eat a quarter of a lifetime return.",
      "",
      "![Line chart of index fund growth](/api/assets/not-a-real-asset)",
    ].join("\n");
    await owner.getByRole("tab", { name: "Markdown source" }).click();
    await owner
      .getByRole("textbox", { name: "Markdown", exact: true })
      .fill(draft);
    await owner.getByRole("button", { name: /Use selected title/ }).click();
    await owner.getByRole("button", { name: "Publish version" }).click();
    // The draft references a malformed asset; publishing must be refused.
    await expect(owner.getByText(/invalid image reference/)).toBeVisible();

    // Fix the reference to the real uploaded asset, then publish successfully.
    const assetSrc = await assetCard.locator("img").getAttribute("src");
    await owner
      .getByRole("textbox", { name: "Markdown", exact: true })
      .fill(draft.replace("/api/assets/not-a-real-asset", assetSrc!));
    await owner.getByRole("button", { name: "Publish version" }).click();
    await expect(
      owner.getByText("Immutable version published", { exact: false }),
    ).toBeVisible();
    await expect(owner.getByText(/v1 · Index Funds Explained/)).toBeVisible();

    // Refresh persistence: reload, reopen the workspace, everything is there.
    await owner.reload();
    await owner.getByRole("button", { name: "Writing", exact: true }).click();
    const optionValue = await owner
      .getByRole("combobox")
      .first()
      .locator("option", { hasText: topic })
      .getAttribute("value");
    expect(optionValue).toBeTruthy();
    await owner.getByRole("combobox").first().selectOption(optionValue!);
    await expect(
      owner.getByRole("textbox", { name: "Core brief" }),
    ).toHaveValue(
      "Beginner guide to index funds: low fees, broad market exposure.",
    );
    await expect(
      owner.getByText("Index Funds Explained", { exact: false }).first(),
    ).toBeVisible();
    await expect(
      owner
        .locator(".asset-card")
        .first()
        .locator('input[placeholder="Alt text"]'),
    ).toHaveValue("Line chart of index fund growth");
    await expect(owner.getByText(/Draft thumbnail ✓/)).toBeVisible();
    expect(errors, `page errors: ${errors.join(" | ")}`).toEqual([]);
  } finally {
    await owner.close();
  }
});

test("writing workspace stays usable at mobile width", async ({ browser }) => {
  const owner = await browser.newPage({
    baseURL,
    viewport: { width: 390, height: 844 },
  });
  try {
    await owner.goto("/__local?user=owner@example.test");
    await owner.getByRole("button", { name: "Writing", exact: true }).click();
    await expect(
      owner.getByRole("heading", { name: "Writing workspace" }),
    ).toBeVisible();
    const create = owner.getByRole("button", { name: "Create workspace" });
    await expect(create).toBeVisible();
    const box = await create.boundingBox();
    expect(box).toBeTruthy();
    expect(box!.x).toBeGreaterThanOrEqual(0);
    expect(box!.x + box!.width).toBeLessThanOrEqual(390);
  } finally {
    await owner.close();
  }
});
