import {
  importSnapshot,
  parseMediumTagSnapshot,
  signatureForPayload,
  urlSet,
  writeSnapshot,
} from "./medium_tag_snapshot_watcher_helpers.mjs";

const defaultWorkspace = process.cwd();
const defaultSnapshotDir = `${defaultWorkspace}/data/medium_tag_watcher_snapshots`;

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function summarizePayload(payload) {
  const cards = payload.cards || [];
  return {
    cards: cards.length,
    missingClaps: cards.filter((card) => card.claps === null || card.claps === undefined).length,
    missingResponses: cards.filter((card) => card.responses === null || card.responses === undefined).length,
    missingAuthors: cards.filter((card) => !card.author_name).length,
    publicationRows: cards.filter((card) => card.publication_status === "publication").length,
    selfPublishedAssumedRows: cards.filter((card) => card.publication_status === "self_published_assumed").length,
    unknownPublicationRows: cards.filter((card) => card.publication_status === "unknown").length,
  };
}

async function captureMediumTagPayload(tab) {
  const snapshot = await tab.playwright.domSnapshot();
  return parseMediumTagSnapshot(snapshot, await tab.url(), await tab.title());
}

async function captureAndImportMediumTag(tab, options = {}) {
  const workspace = options.workspace || defaultWorkspace;
  const snapshotDir = options.snapshotDir || defaultSnapshotDir;
  const payload = await captureMediumTagPayload(tab);
  const file = writeSnapshot(payload, snapshotDir);
  const imported = options.import === false
    ? { status: null, stdout: "", stderr: "" }
    : importSnapshot(file, workspace);

  return {
    at: new Date().toISOString(),
    file,
    importStatus: imported.status,
    importOutput: imported.stderr || imported.stdout || "",
    summary: summarizePayload(payload),
    firstTitles: (payload.cards || []).slice(0, 3).map((card) => card.title),
    payload,
  };
}

async function runMediumInAppWatcher(tab, options = {}) {
  const durationMs = options.durationMs ?? 120000;
  const intervalMs = options.intervalMs ?? 5000;
  const minNewUrls = options.minNewUrls ?? 1;
  let lastSignature = "";
  let lastUrls = new Set();
  const events = [];
  const started = Date.now();

  while (Date.now() - started < durationMs) {
    const payload = await captureMediumTagPayload(tab);
    const signature = signatureForPayload(payload);
    const urls = urlSet(payload);
    const newUrls = [...urls].filter((url) => !lastUrls.has(url));
    const shouldImport = (
      payload.cards.length > 0 &&
      signature !== lastSignature &&
      (lastSignature === "" || newUrls.length >= minNewUrls)
    );

    if (shouldImport) {
      const file = writeSnapshot(payload, options.snapshotDir || defaultSnapshotDir);
      const imported = options.import === false
        ? { status: null, stdout: "", stderr: "" }
        : importSnapshot(file, options.workspace || defaultWorkspace);
      const event = {
        at: new Date().toISOString(),
        cards: payload.cards.length,
        newUrls: newUrls.length,
        file,
        importStatus: imported.status,
        summary: summarizePayload(payload),
        firstTitles: payload.cards.slice(0, 3).map((card) => card.title),
      };
      events.push(event);
      lastSignature = signature;
      lastUrls = urls;
      console.log("imported", JSON.stringify(event));
    } else {
      console.log("checked", JSON.stringify({
        at: new Date().toISOString(),
        cards: payload.cards.length,
        newUrls: newUrls.length,
      }));
    }

    await sleep(intervalMs);
  }

  return events;
}

export {
  captureAndImportMediumTag,
  captureMediumTagPayload,
  runMediumInAppWatcher,
  summarizePayload,
};
