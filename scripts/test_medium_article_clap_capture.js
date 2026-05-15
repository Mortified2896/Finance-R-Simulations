#!/usr/bin/env node

import fs from "fs";
import path from "path";
import readline from "readline/promises";
import { stdin as input, stdout as output } from "process";
import { execFileSync } from "child_process";
import { chromium } from "playwright";

const defaultUrl = "https://medium.com/the-investors-handbook/war-in-the-strait-does-nuclear-become-the-new-safe-haven-a10195785fe9";
const defaultDebugDir = path.join("debug_samples", "article_clap_capture");
const defaultDbPath = path.join("data", "db", "medium_articles.sqlite");
const defaultUserDataDir = path.join("data", "medium_article_clap_capture_profile");

function parseArgs(argv) {
  const options = {
    urls: [],
    urlFile: null,
    sampleDb: 0,
    dbPath: defaultDbPath,
    debugDir: defaultDebugDir,
    userDataDir: null,
    useDefaultProfile: false,
    connectCdp: "",
    channel: "",
    headless: false,
    waitMs: 8000,
    manualPause: true,
    writeDb: false,
  };

  for (let index = 2; index < argv.length; index += 1) {
    const arg = argv[index];
    const next = argv[index + 1];

    if (arg === "--url" && next) {
      options.urls.push(next);
      index += 1;
    } else if (arg === "--url-file" && next) {
      options.urlFile = next;
      index += 1;
    } else if (arg === "--sample-db" && next) {
      options.sampleDb = Number(next);
      index += 1;
    } else if (arg === "--db-path" && next) {
      options.dbPath = next;
      index += 1;
    } else if (arg === "--debug-dir" && next) {
      options.debugDir = next;
      index += 1;
    } else if (arg === "--user-data-dir" && next) {
      options.userDataDir = next;
      index += 1;
    } else if (arg === "--use-default-profile") {
      options.useDefaultProfile = true;
    } else if (arg === "--connect-cdp" && next) {
      options.connectCdp = next;
      index += 1;
    } else if (arg === "--channel" && next) {
      options.channel = next;
      index += 1;
    } else if (arg === "--headless") {
      options.headless = true;
    } else if (arg === "--wait-ms" && next) {
      options.waitMs = Number(next);
      index += 1;
    } else if (arg === "--no-manual-pause") {
      options.manualPause = false;
    } else if (arg === "--write-db") {
      options.writeDb = true;
    } else if (arg === "--help" || arg === "-h") {
      printHelpAndExit();
    } else if (/^https?:\/\//i.test(arg)) {
      options.urls.push(arg);
    } else {
      throw new Error(`Unknown option: ${arg}`);
    }
  }

  if (!Number.isFinite(options.waitMs) || options.waitMs < 1000) {
    throw new Error("--wait-ms must be at least 1000.");
  }

  if (!Number.isFinite(options.sampleDb) || options.sampleDb < 0) {
    throw new Error("--sample-db must be a non-negative number.");
  }

  if (options.useDefaultProfile && !options.userDataDir) {
    options.userDataDir = defaultUserDataDir;
  }

  return options;
}

function printHelpAndExit() {
  console.log(`
Medium article clap capture smoke test

Usage:
  node scripts/test_medium_article_clap_capture.js [url] [options]

Options:
  --url <url>                 Article URL to test. Can be repeated.
  --url-file <path>           Text file with one article URL per line.
  --sample-db <n>             Add up to n URLs from medium_articles. Default: 0
  --db-path <path>            SQLite DB path. Default: ${defaultDbPath}
  --debug-dir <path>          Debug output folder. Default: ${defaultDebugDir}
  --use-default-profile       Use persistent profile: ${defaultUserDataDir}
  --user-data-dir <path>      Use a persistent browser profile/session.
  --connect-cdp <url>         Attach to an existing Chrome CDP endpoint.
  --channel <name>            Use installed browser channel, for example: chrome
  --headless                  Run without a visible browser window.
  --wait-ms <ms>              Wait after navigation. Default: 8000
  --no-manual-pause           Do not pause for manual login/browser checks.
  --write-db                  Write observations to article_direct_stat_observations.
`);
  process.exit(0);
}

function cleanText(value) {
  if (value === null || value === undefined) {
    return null;
  }

  const cleaned = String(value).replace(/\s+/g, " ").trim();
  return cleaned.length > 0 ? cleaned : null;
}

function parseCompactNumber(value) {
  const cleaned = cleanText(value);

  if (!cleaned || cleaned === "--") {
    return null;
  }

  const match = cleaned.replace(/,/g, "").replace(/\s+/g, "").match(/^([0-9]+(?:\.[0-9]+)?)([kKmM])?$/);

  if (!match) {
    return null;
  }

  const multiplier = match[2]?.toLowerCase() === "k" ? 1000 : match[2]?.toLowerCase() === "m" ? 1000000 : 1;
  return Math.round(Number(match[1]) * multiplier);
}

function extractMediumPostId(url) {
  const match = String(url || "").match(/-([0-9a-f]{12})(?:[/?#]|$)/i);
  return match ? match[1].toLowerCase() : null;
}

function slugForUrl(url, index) {
  const postId = extractMediumPostId(url);

  if (postId) {
    return postId;
  }

  return `url_${String(index + 1).padStart(2, "0")}`;
}

function readUrlFile(filePath) {
  return fs
    .readFileSync(filePath, "utf8")
    .split(/\r?\n/)
    .map((line) => line.replace(/#.*/, "").trim())
    .filter(Boolean);
}

function runSqlite(dbPath, sql) {
  return execFileSync("sqlite3", ["-json", dbPath, sql], { encoding: "utf8" });
}

function parseSqliteJson(output) {
  const cleaned = cleanText(output);
  return cleaned ? JSON.parse(cleaned) : [];
}

function sampleArticlesFromDb(dbPath, limit) {
  if (limit <= 0) {
    return [];
  }

  const escapedLimit = Math.max(1, Math.min(50, Math.floor(limit)));
  const sql = `
    SELECT id AS article_id, url, medium_post_id
    FROM medium_articles
    WHERE TRIM(COALESCE(url, '')) <> ''
    ORDER BY COALESCE(last_seen_at, visible_article_collected_at, published_at, updated_at, fetched_at) DESC, id DESC
    LIMIT ${escapedLimit}
  `;

  return parseSqliteJson(runSqlite(dbPath, sql));
}

function articleLookupFromDb(dbPath, urls) {
  if (!fs.existsSync(dbPath) || urls.length === 0) {
    return new Map();
  }

  const quotedUrls = urls.map((url) => `'${String(url).replace(/'/g, "''")}'`).join(",");
  const sql = `
    SELECT id AS article_id, url, canonical_url, medium_post_id
    FROM medium_articles
    WHERE url IN (${quotedUrls}) OR canonical_url IN (${quotedUrls})
  `;
  const rows = parseSqliteJson(runSqlite(dbPath, sql));
  const lookup = new Map();

  for (const row of rows) {
    if (row.url) {
      lookup.set(row.url, row);
    }
    if (row.canonical_url) {
      lookup.set(row.canonical_url, row);
    }
  }

  return lookup;
}

function dedupeArticles(articles) {
  const seen = new Set();
  const deduped = [];

  for (const article of articles) {
    const url = cleanText(article.url);

    if (!url || seen.has(url)) {
      continue;
    }

    seen.add(url);
    deduped.push({ ...article, url });
  }

  return deduped;
}

function detectPageState(bodyText, pageTitle) {
  const text = [bodyText, pageTitle].filter(Boolean).join(" ");

  if (/captcha|verify you are human|checking your browser|security check|are you a human/i.test(text)) {
    return "blocked_or_captcha";
  }

  if (/create an account to read the full story|already have an account\?\s*sign in|log in to continue/i.test(text)) {
    return "login_wall_possible";
  }

  if (/member-only|members-only|become a member|get unlimited access/i.test(text)) {
    return "member_only_or_paywall_possible";
  }

  if (/500 apologies|something went wrong on our end|site status/i.test(text)) {
    return "medium_error_page";
  }

  return null;
}

function firstIntegerNearClap(text) {
  const cleaned = cleanText(text);

  if (!cleaned) {
    return { raw: null, value: null, sawDash: false };
  }

  const patterns = [
    /(?:claps?|applause|recommend(?:s|ed|ations?)?)\D{0,80}([0-9][0-9,.]*\s*[kKmM]?|--)/i,
    /([0-9][0-9,.]*\s*[kKmM]?|--)\D{0,80}(?:claps?|applause|recommend(?:s|ed|ations?)?)/i,
    /(?:A clap icon|clap icon)\D{0,80}([0-9][0-9,.]*\s*[kKmM]?|--)/i,
  ];

  for (const pattern of patterns) {
    const match = cleaned.match(pattern);

    if (match?.[1]) {
      return {
        raw: cleanText(match[1]),
        value: parseCompactNumber(match[1]),
        sawDash: cleanText(match[1]) === "--",
      };
    }
  }

  const compactNumbers = cleaned.match(/\b[0-9][0-9,.]*\s*[kKmM]?\b/g) || [];
  const uniqueNumbers = [...new Set(compactNumbers.map(cleanText).filter(Boolean))];

  if (uniqueNumbers.length === 1 && /clap|applause|recommend/i.test(cleaned)) {
    return {
      raw: uniqueNumbers[0],
      value: parseCompactNumber(uniqueNumbers[0]),
      sawDash: false,
    };
  }

  return { raw: null, value: null, sawDash: /--/.test(cleaned) && /clap|applause|recommend/i.test(cleaned) };
}

function chooseClapCandidate(actionBar) {
  for (const candidate of actionBar.candidates) {
    const parsed = firstIntegerNearClap([candidate.label, candidate.text].filter(Boolean).join(" "));

    if (parsed.value !== null || parsed.sawDash) {
      return { ...parsed, source: candidate.reason, candidateText: candidate.text };
    }
  }

  const combined = firstIntegerNearClap(actionBar.visibleText);
  return { ...combined, source: "combined_action_bar_text", candidateText: actionBar.visibleText };
}

async function extractActionBar(page) {
  return page.evaluate(() => {
    const maxTextLength = 1600;
    const maxHtmlLength = 12000;

    function isVisible(node) {
      if (!(node instanceof HTMLElement) && !(node instanceof SVGElement)) {
        return false;
      }

      const style = window.getComputedStyle(node);
      const rect = node.getBoundingClientRect();
      return style.display !== "none" && style.visibility !== "hidden" && rect.width > 0 && rect.height > 0;
    }

    function clean(value) {
      return String(value || "").replace(/\s+/g, " ").trim();
    }

    function labelFor(node) {
      const labelParts = [
        node.getAttribute?.("aria-label"),
        node.getAttribute?.("title"),
        node.getAttribute?.("data-testid"),
        node.innerText,
        node.textContent,
      ];
      return clean(labelParts.filter(Boolean).join(" "));
    }

    function elementText(node) {
      return clean(node.innerText || node.textContent || "");
    }

    function snippetHtml(node) {
      const html = node.outerHTML || "";
      return html.length > maxHtmlLength ? `${html.slice(0, maxHtmlLength)}\n<!-- truncated -->` : html;
    }

    function usefulAncestor(node) {
      let current = node;
      let best = node;

      for (let depth = 0; depth < 5 && current?.parentElement; depth += 1) {
        current = current.parentElement;
        const text = elementText(current);
        const rect = current.getBoundingClientRect();

        if (text.length > 0 && text.length <= maxTextLength && rect.width > 20 && rect.height > 10) {
          best = current;
        }

        if (text.length > 220 || /respond|comment|share|save|listen/i.test(text)) {
          break;
        }
      }

      return best;
    }

    const candidates = [];
    const seen = new Set();

    function nearbyTextFor(node) {
      const rect = node.getBoundingClientRect();
      const nearby = [];

      for (const other of document.querySelectorAll("span, p, div, button, [role='button']")) {
        if (!isVisible(other) || other === node) {
          continue;
        }

        const otherRect = other.getBoundingClientRect();
        const text = elementText(other);

        if (!text || text.length > 80) {
          continue;
        }

        const horizontallyNear = otherRect.left >= rect.left - 10 && otherRect.left <= rect.right + 90;
        const verticallyNear = Math.abs((otherRect.top + otherRect.bottom) / 2 - (rect.top + rect.bottom) / 2) <= 24;

        if (horizontallyNear && verticallyNear) {
          nearby.push(text);
        }
      }

      return [...new Set(nearby)].join(" ");
    }

    function addCandidate(node, reason, label = "") {
      const root = usefulAncestor(node);
      const rect = root.getBoundingClientRect();
      const key = `${Math.round(rect.x)}:${Math.round(rect.y)}:${Math.round(rect.width)}:${Math.round(rect.height)}:${reason}`;

      if (seen.has(key)) {
        return;
      }

      seen.add(key);
      candidates.push({
        reason,
        label: clean(label || labelFor(node)),
        text: clean([elementText(root), nearbyTextFor(node)].filter(Boolean).join(" ")),
        html: snippetHtml(root),
        rect: {
          x: Math.round(rect.x),
          y: Math.round(rect.y),
          width: Math.round(rect.width),
          height: Math.round(rect.height),
        },
      });
    }

    for (const node of document.querySelectorAll("button, [role='button'], a, [aria-label], [title]")) {
      if (!isVisible(node)) {
        continue;
      }

      const label = labelFor(node);

      if (/clap|applause|recommend/i.test(label)) {
        addCandidate(node, "accessible_clap_control", label);
      }
    }

    for (const desc of document.querySelectorAll("svg desc, svg title")) {
      const label = clean(desc.textContent);

      if (/clap|applause|recommend/i.test(label)) {
        const svg = desc.closest("svg");

        if (svg && isVisible(svg)) {
          addCandidate(svg, "svg_clap_description", label);
        }
      }
    }

    for (const node of document.querySelectorAll("span, div, p")) {
      if (!isVisible(node)) {
        continue;
      }

      const text = elementText(node);

      if (/^A clap icon$/i.test(text)) {
        addCandidate(node, "text_clap_icon_description", text);
      }
    }

    for (const button of document.querySelectorAll("button, [role='button']")) {
      if (!isVisible(button)) {
        continue;
      }

      const text = clean(button.textContent);

      if (/^--$|^[0-9][0-9,.]*\s*[kKmM]?$/.test(text)) {
        const parentText = clean(button.parentElement?.textContent);

        if (/clap|applause|recommend/i.test(parentText)) {
          addCandidate(button, "numeric_button_near_clap", parentText);
        }
      }
    }

    if (candidates.length === 0) {
      const bodyText = clean(document.body?.innerText || "");
      const actionWords = ["Listen", "Share", "More", "Responses", "Clap"];
      const firstIndex = actionWords
        .map((word) => bodyText.toLowerCase().indexOf(word.toLowerCase()))
        .filter((index) => index >= 0)
        .sort((a, b) => a - b)[0];

      if (firstIndex !== undefined) {
        candidates.push({
          reason: "body_action_words_fallback",
          label: "",
          text: bodyText.slice(Math.max(0, firstIndex - 250), firstIndex + 450),
          html: "",
          rect: null,
        });
      }
    }

    const visibleText = candidates.map((candidate) => candidate.text).filter(Boolean).join("\n---\n");
    const html = candidates.map((candidate) => `<!-- ${candidate.reason} -->\n${candidate.html}`).filter(Boolean).join("\n\n");

    return {
      candidates,
      visibleText,
      html,
    };
  });
}

async function maybePauseForManualState(page, options, rl) {
  if (!options.manualPause || options.headless) {
    return;
  }

  const bodyText = cleanText(await page.locator("body").innerText({ timeout: 5000 }).catch(() => ""));
  const state = detectPageState(bodyText, await page.title().catch(() => ""));

  if (!state || state === "member_only_or_paywall_possible") {
    return;
  }

  if (!process.stdin.isTTY) {
    console.log(`Manual attention may be needed (${state}), but stdin is not interactive. Continuing without a pause.`);
    return;
  }

  console.log(`Manual attention may be needed (${state}). Complete any visible login/check in the browser, then press Enter here.`);
  await rl.question("").catch(() => {});
  await page.waitForTimeout(options.waitMs);
}

async function browserForOptions(options) {
  if (options.connectCdp) {
    const browser = await chromium.connectOverCDP(options.connectCdp);
    const context = browser.contexts()[0] || await browser.newContext();
    return { browser, context, close: () => browser.close() };
  }

  if (options.userDataDir) {
    fs.mkdirSync(options.userDataDir, { recursive: true });
    const context = await chromium.launchPersistentContext(options.userDataDir, {
      headless: options.headless,
      channel: options.channel || undefined,
      viewport: { width: 1280, height: 900 },
      locale: "en-US",
      slowMo: options.headless ? 0 : 100,
    });
    return { browser: null, context, close: () => context.close() };
  }

  const browser = await chromium.launch({
    headless: options.headless,
    channel: options.channel || undefined,
    slowMo: options.headless ? 0 : 100,
  });
  const context = await browser.newContext({
    viewport: { width: 1280, height: 900 },
    locale: "en-US",
  });

  return { browser, context, close: async () => {
    await context.close();
    await browser.close();
  } };
}

async function testArticle(page, article, options, index, rl) {
  const checkedAt = new Date().toISOString();
  const slug = slugForUrl(article.url, index);
  const screenshotPath = path.join(options.debugDir, `${slug}_screenshot.png`);
  const debugTextPath = path.join(options.debugDir, `${slug}_action_bar_text.txt`);
  const htmlDebugPath = path.join(options.debugDir, `${slug}_action_bar.html`);

  let responseStatus = null;
  let pageTitle = null;
  let actionBar = { candidates: [], visibleText: "", html: "" };
  let extracted = { value: null, raw: null, sawDash: false, source: null, candidateText: null };
  let extractionStatus = "error";
  let statusDetail = "";

  try {
    const response = await page.goto(article.url, {
      waitUntil: "domcontentloaded",
      timeout: 45000,
    });
    responseStatus = response?.status() ?? null;
    await page.waitForLoadState("networkidle", { timeout: 15000 }).catch(() => {});
    await page.waitForTimeout(options.waitMs);
    await maybePauseForManualState(page, options, rl);

    pageTitle = cleanText(await page.title().catch(() => ""));
    const bodyTextForState = cleanText(await page.locator("body").innerText({ timeout: 5000 }).catch(() => ""));
    const pageState = detectPageState(bodyTextForState, pageTitle);
    actionBar = await extractActionBar(page);
    extracted = chooseClapCandidate(actionBar);

    if (extracted.value !== null) {
      extractionStatus = "ok";
      statusDetail = `claps parsed from ${extracted.source}`;
    } else if (extracted.sawDash) {
      extractionStatus = "dash_unknown";
      statusDetail = '"--" shown near clap control';
    } else if (pageState) {
      extractionStatus = pageState;
      statusDetail = "page state detected before a clap count was parsed";
    } else if (responseStatus !== null && responseStatus >= 400) {
      extractionStatus = "http_error";
      statusDetail = `HTTP status ${responseStatus}`;
    } else if (actionBar.candidates.length === 0) {
      extractionStatus = "no_action_bar_found";
      statusDetail = "no visible clap/action-bar candidate found";
    } else {
      extractionStatus = "unknown";
      statusDetail = "action-bar candidates found, but no clap count parsed";
    }
  } catch (error) {
    statusDetail = error.message;
  }

  await page.screenshot({ path: screenshotPath, fullPage: true }).catch(() => {});
  fs.writeFileSync(debugTextPath, actionBar.visibleText || "", "utf8");
  fs.writeFileSync(htmlDebugPath, actionBar.html || "", "utf8");

  return {
    url: article.url,
    article_id: article.article_id ?? null,
    medium_post_id: article.medium_post_id || extractMediumPostId(article.url),
    extracted_claps: extracted.value,
    claps_raw: extracted.raw,
    extraction_status: extractionStatus,
    status_detail: statusDetail,
    page_title: pageTitle,
    http_status: responseStatus,
    date_checked: checkedAt,
    screenshot_path: screenshotPath,
    debug_text_path: debugTextPath,
    html_debug_path: htmlDebugPath,
  };
}

function ensureObservationTable(dbPath) {
  execFileSync("sqlite3", [dbPath, `
    CREATE TABLE IF NOT EXISTS article_direct_stat_observations (
      id INTEGER PRIMARY KEY,
      article_id INTEGER,
      url TEXT,
      medium_post_id TEXT,
      checked_at TEXT,
      claps INTEGER,
      responses INTEGER,
      claps_status TEXT,
      responses_status TEXT,
      extraction_method TEXT,
      debug_screenshot_path TEXT,
      notes TEXT
    )
  `]);
}

function writeObservation(dbPath, result) {
  const values = [
    result.article_id,
    result.url,
    result.medium_post_id,
    result.date_checked,
    result.extracted_claps,
    null,
    result.extraction_status,
    "not_checked",
    "playwright_rendered_article_action_bar",
    result.screenshot_path,
    result.status_detail,
  ].map((value) => value === null || value === undefined ? "" : String(value));

  execFileSync("sqlite3", [dbPath, `
    INSERT INTO article_direct_stat_observations (
      article_id, url, medium_post_id, checked_at, claps, responses,
      claps_status, responses_status, extraction_method, debug_screenshot_path, notes
    ) VALUES (
      ${values[0] ? Number(values[0]) : "NULL"},
      '${values[1].replace(/'/g, "''")}',
      ${values[2] ? `'${values[2].replace(/'/g, "''")}'` : "NULL"},
      '${values[3].replace(/'/g, "''")}',
      ${values[4] ? Number(values[4]) : "NULL"},
      NULL,
      '${values[6].replace(/'/g, "''")}',
      '${values[7].replace(/'/g, "''")}',
      '${values[8].replace(/'/g, "''")}',
      '${values[9].replace(/'/g, "''")}',
      '${values[10].replace(/'/g, "''")}'
    )
  `]);
}

function summarize(results) {
  const extracted = results.filter((result) => result.extracted_claps !== null).length;
  const dash = results.filter((result) => result.extraction_status === "dash_unknown").length;
  const blocked = results.filter((result) => /blocked|captcha|login|error/i.test(result.extraction_status)).length;
  const unknown = results.length - extracted;

  console.log("\nSummary");
  console.log("=======");
  console.log(`URLs tested: ${results.length}`);
  console.log(`Claps extracted: ${extracted}`);
  console.log(`Claps unknown: ${unknown}`);
  console.log(`Dash shown: ${dash}`);
  console.log(`Blocked/login/captcha/error: ${blocked}`);
  console.log("\nValues");
  for (const result of results) {
    const value = result.extracted_claps === null ? "unknown" : String(result.extracted_claps);
    console.log(`- ${value} | ${result.extraction_status} | ${result.url}`);
  }
}

async function main() {
  const options = parseArgs(process.argv);
  fs.mkdirSync(options.debugDir, { recursive: true });

  const articles = [];
  articles.push(...options.urls.map((url) => ({ url })));

  if (options.urlFile) {
    articles.push(...readUrlFile(options.urlFile).map((url) => ({ url })));
  }

  if (options.sampleDb > 0) {
    articles.push(...sampleArticlesFromDb(options.dbPath, options.sampleDb));
  }

  if (articles.length === 0) {
    articles.push({ url: defaultUrl });
  }

  const deduped = dedupeArticles(articles);
  const dbLookup = articleLookupFromDb(options.dbPath, deduped.map((article) => article.url));
  const enriched = deduped.map((article) => ({
    ...article,
    ...(dbLookup.get(article.url) || {}),
    medium_post_id: article.medium_post_id || dbLookup.get(article.url)?.medium_post_id || extractMediumPostId(article.url),
  }));

  const resultsPath = path.join(options.debugDir, "results.jsonl");
  fs.writeFileSync(resultsPath, "", "utf8");

  if (options.writeDb) {
    ensureObservationTable(options.dbPath);
  }

  console.log("Medium Article Clap Capture Test");
  console.log("================================");
  console.log(`Visible browser: ${!options.headless}`);
  console.log(`URLs queued: ${enriched.length}`);
  console.log(`Debug dir: ${options.debugDir}`);
  if (options.userDataDir) {
    console.log(`Persistent profile: ${options.userDataDir}`);
  }

  const session = await browserForOptions(options);
  const page = session.context.pages()[0] || await session.context.newPage();
  const rl = readline.createInterface({ input, output });
  const results = [];

  try {
    for (let index = 0; index < enriched.length; index += 1) {
      const article = enriched[index];
      console.log(`\nTesting ${index + 1}/${enriched.length}: ${article.url}`);
      const result = await testArticle(page, article, options, index, rl);
      results.push(result);
      fs.appendFileSync(resultsPath, `${JSON.stringify(result)}\n`, "utf8");

      if (options.writeDb) {
        writeObservation(options.dbPath, result);
      }

      console.log(`Result: ${result.extracted_claps ?? "unknown"} (${result.extraction_status})`);
    }
  } finally {
    rl.close();
    await session.close();
  }

  summarize(results);
  console.log(`\nResults JSONL: ${resultsPath}`);
}

main().catch((error) => {
  console.error("Medium article clap capture test failed.");
  console.error(error.stack || error.message);
  process.exitCode = 1;
});
