#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const crypto = require("crypto");
const readline = require("readline");
const { chromium } = require("playwright");

const defaultUrl = "https://medium.com/tag/finance/recommended";
const defaultStartUrl = "about:blank";
const defaultSnapshotDir = path.join("data", "medium_tag_watcher_snapshots");
const defaultArticleTextSnapshotDir = path.join("data", "medium_article_text_snapshots");
const defaultUserDataDir = path.join("data", "medium_tag_playwright_profile");
const defaultBrowserHome = path.join("data", "medium_tag_browser_home");

function parseArgs(argv) {
  const options = {
    url: defaultUrl,
    startUrl: defaultUrl,
    waitForMedium: false,
    pollSeconds: 5,
    minNewUrls: 1,
    snapshotDir: defaultSnapshotDir,
    articleTextSnapshotDir: defaultArticleTextSnapshotDir,
    maxArticleTextChars: 100000,
    userDataDir: defaultUserDataDir,
    browserHome: defaultBrowserHome,
    channel: "",
    connectCdp: "",
    importSnapshots: true,
    once: false,
    headless: false,
    snapshotTimeoutMs: 30000,
  };

  for (let index = 2; index < argv.length; index += 1) {
    const arg = argv[index];
    const next = argv[index + 1];

    if (arg === "--url" && next) {
      options.url = next;
      options.startUrl = next;
      index += 1;
    } else if (arg === "--start-url" && next) {
      options.startUrl = next;
      index += 1;
    } else if (arg === "--wait-for-medium") {
      options.waitForMedium = true;
    } else if (arg === "--poll-seconds" && next) {
      options.pollSeconds = Number(next);
      index += 1;
    } else if (arg === "--min-new-urls" && next) {
      options.minNewUrls = Number(next);
      index += 1;
    } else if (arg === "--snapshot-dir" && next) {
      options.snapshotDir = next;
      index += 1;
    } else if (arg === "--article-text-snapshot-dir" && next) {
      options.articleTextSnapshotDir = next;
      index += 1;
    } else if (arg === "--max-article-text-chars" && next) {
      options.maxArticleTextChars = Number(next);
      index += 1;
    } else if (arg === "--user-data-dir" && next) {
      options.userDataDir = next;
      index += 1;
    } else if (arg === "--browser-home" && next) {
      options.browserHome = next;
      index += 1;
    } else if (arg === "--channel" && next) {
      options.channel = next;
      index += 1;
    } else if (arg === "--connect-cdp" && next) {
      options.connectCdp = next;
      index += 1;
    } else if (arg === "--no-import") {
      options.importSnapshots = false;
    } else if (arg === "--once") {
      options.once = true;
    } else if (arg === "--headless") {
      options.headless = true;
    } else if (arg === "--snapshot-timeout-ms" && next) {
      options.snapshotTimeoutMs = Number(next);
      index += 1;
    } else if (arg === "--help" || arg === "-h") {
      printHelpAndExit();
    } else {
      throw new Error(`Unknown option: ${arg}`);
    }
  }

  if (!Number.isFinite(options.pollSeconds) || options.pollSeconds < 1) {
    throw new Error("--poll-seconds must be at least 1.");
  }

  if (!Number.isFinite(options.minNewUrls) || options.minNewUrls < 0) {
    throw new Error("--min-new-urls must be at least 0.");
  }

  if (!Number.isFinite(options.snapshotTimeoutMs) || options.snapshotTimeoutMs < 1000) {
    throw new Error("--snapshot-timeout-ms must be at least 1000.");
  }

  if (!Number.isFinite(options.maxArticleTextChars) || options.maxArticleTextChars < 1000) {
    throw new Error("--max-article-text-chars must be at least 1000.");
  }

  return options;
}

function printHelpAndExit() {
  console.log(`
Medium rendered tag-page watcher

Usage:
  node scripts/watch_medium_tag_page_pw.cjs [options]

Options:
  --url <url>                 Medium tag page to open. Default: ${defaultUrl}
  --start-url <url>           Browser start page. Default follows --url.
  --wait-for-medium           Let you navigate manually; capture Medium tag and article pages.
  --poll-seconds <seconds>    Check interval. Default: 5
  --min-new-urls <count>      Import when at least this many new article URLs appear. Default: 1
  --snapshot-dir <path>       Where JSON snapshots are written. Default: ${defaultSnapshotDir}
  --article-text-snapshot-dir <path>
                              Where article text JSON snapshots are written. Default: ${defaultArticleTextSnapshotDir}
  --max-article-text-chars <n>
                              Maximum visible article text chars per snapshot. Default: 100000
  --user-data-dir <path>      Browser profile directory. Default: ${defaultUserDataDir}
  --browser-home <path>       Browser support-file home. Default: ${defaultBrowserHome}
  --channel <name>            Use installed browser channel, for example: chrome
  --connect-cdp <url>         Attach to an existing Chrome remote-debugging endpoint.
  --no-import                 Write JSON snapshots but do not import into SQLite.
  --once                      Capture one snapshot, optionally import it, then exit.
  --headless                  Run without a visible browser window.
  --snapshot-timeout-ms <ms>  Accessibility snapshot timeout. Default: 30000
`);
  process.exit(0);
}

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function setupKeyboardControls(state) {
  if (!process.stdin.isTTY) {
    return;
  }

  const controls = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: true,
  });

  controls.on("line", (line) => {
    const normalized = String(line || "").trim().toLowerCase();

    if (normalized === "p") {
      state.paused = !state.paused;
      console.log(state.paused ? "Paused. Type p and press Enter to resume." : "Resumed.");
    } else if (normalized === "q") {
      console.log("Stopping watcher.");
      process.exit(0);
    }
  });
}

function countMissing(cards, fieldName) {
  return cards.filter((card) => {
    const value = card[fieldName];
    return value === null || value === undefined || value === "";
  }).length;
}

function summarizePayload(payload) {
  const cards = payload.cards || [];
  return {
    cards: cards.length,
    missingClaps: countMissing(cards, "claps"),
    missingResponses: countMissing(cards, "responses"),
    missingAuthors: countMissing(cards, "author_name"),
    publicationRows: cards.filter((card) => card.publication_status === "publication").length,
    selfPublishedAssumedRows: cards.filter((card) => card.publication_status === "self_published_assumed").length,
    unknownPublicationRows: cards.filter((card) => card.publication_status === "unknown").length,
    readTimeRows: cards.filter((card) => card.read_time_minutes !== null && card.read_time_minutes !== undefined).length,
  };
}

function formatSummary(prefix, payload, extra = {}) {
  const summary = summarizePayload(payload);
  const parts = [
    prefix,
    `cards=${summary.cards}`,
    `new_urls=${extra.newUrls ?? ""}`,
    `missing_claps=${summary.missingClaps}`,
    `missing_responses=${summary.missingResponses}`,
    `missing_authors=${summary.missingAuthors}`,
    `unknown_publication=${summary.unknownPublicationRows}`,
    `read_time_rows=${summary.readTimeRows}`,
  ];

  if (extra.file) {
    parts.push(`file=${extra.file}`);
  }

  return parts.join(" ");
}

function compactTitle(value, maxLength = 90) {
  const cleaned = String(value || "").replace(/\s+/g, " ").trim();

  if (cleaned.length <= maxLength) {
    return cleaned;
  }

  return `${cleaned.slice(0, maxLength - 1)}…`;
}

function payloadTitleSummary(payload) {
  const cards = payload.cards || [];

  if (cards.length === 0) {
    return "";
  }

  if (cards.length === 1) {
    return compactTitle(cards[0].title || cards[0].article_url || "");
  }

  const last = cards[cards.length - 1];
  return `${cards.length} articles; last="${compactTitle(last.title || last.article_url || "")}"`;
}

function cleanLinkName(value) {
  return String(value || "").replace(/\s+/g, " ").trim();
}

function unescapeSnapshotName(value) {
  return String(value || "")
    .replace(/\\"/g, "\"")
    .replace(/\\\\/g, "\\");
}

async function visibleArticleUrls(page) {
  const urls = await page.evaluate(() => {
    const cleanUrl = (value) => {
      if (!value || /\/m\/signin|bookmark|vote|respond/i.test(value)) {
        return "";
      }

      try {
        const parsed = new URL(value, "https://medium.com");
        parsed.search = "";
        parsed.hash = "";
        return parsed.toString().replace(/\/+$/, "");
      } catch (_error) {
        return String(value).split(/[?#]/)[0].replace(/\/+$/, "");
      }
    };

    return [...document.querySelectorAll("a[href]")]
      .map((link) => cleanUrl(link.href || link.getAttribute("href")))
      .filter((href) => /[a-f0-9]{12}\/?$/i.test(href));
  });

  return new Set(urls);
}

function setSignature(values) {
  return [...values].sort().join("\n");
}

function textHash(value) {
  return crypto.createHash("sha256").update(String(value || ""), "utf8").digest("hex");
}

function wordCount(value) {
  const words = String(value || "").trim().match(/\S+/g);
  return words ? words.length : 0;
}

function compactCaptureError(error) {
  const message = String(error && error.message ? error.message : error);

  if (/ariaSnapshot: Timeout|Timeout .* exceeded/i.test(message)) {
    return "snapshot_timeout";
  }

  return message.split("\n")[0] || "snapshot_failed";
}

function formatCaptureError(errorCode) {
  if (errorCode === "snapshot_timeout") {
    return "Medium page snapshot timed out; will retry.";
  }

  return errorCode;
}

function logChangedStatus(state, statusMessage) {
  if (state.lastStatusMessage !== statusMessage) {
    console.log(statusMessage);
    state.lastStatusMessage = statusMessage;
  }
}

async function getAriaSnapshot(page, timeoutMs) {
  const body = page.locator("body");

  if (typeof body.ariaSnapshot !== "function") {
    throw new Error(
      "This Playwright version does not expose locator.ariaSnapshot(). Run `npm install playwright` and try again."
    );
  }

  const snapshot = await body.ariaSnapshot({ mode: "default", depth: 12, timeout: timeoutMs });
  const links = await page.evaluate(() => {
    const clean = (value) => String(value || "").replace(/\s+/g, " ").trim();
    return [...document.querySelectorAll("a[href]")]
      .map((link) => ({
        name: clean(link.getAttribute("aria-label") || link.innerText || link.textContent),
        href: link.href,
      }))
      .filter((link) => link.name && link.href);
  });

  return enrichSnapshotWithUrls(snapshot, links);
}

function enrichSnapshotWithUrls(snapshot, links) {
  const linksByName = new Map();

  for (const link of links) {
    const name = cleanLinkName(link.name);
    if (!name || !link.href) {
      continue;
    }
    if (!linksByName.has(name)) {
      linksByName.set(name, []);
    }
    linksByName.get(name).push(link.href);
  }

  const usage = new Map();
  const lines = String(snapshot || "").split("\n");
  const enriched = [];

  for (let index = 0; index < lines.length; index += 1) {
    const line = lines[index];
    enriched.push(line);

    if (index + 1 < lines.length && /-\s*\/url:/.test(lines[index + 1])) {
      continue;
    }

    const linkMatch = line.match(/^(\s*)-\s+link\s+"((?:\\"|[^"])*)"/);
    if (!linkMatch) {
      continue;
    }

    const name = cleanLinkName(unescapeSnapshotName(linkMatch[2]));
    const candidates = linksByName.get(name);
    if (!candidates || candidates.length === 0) {
      continue;
    }

    const used = usage.get(name) || 0;
    const href = candidates[Math.min(used, candidates.length - 1)];
    usage.set(name, used + 1);
    enriched.push(`${linkMatch[1]}  - /url: ${href}`);
  }

  return enriched.join("\n");
}

async function capturePayload(page, helpers, options) {
  const snapshot = await getAriaSnapshot(page, options.snapshotTimeoutMs);
  return helpers.parseMediumTagSnapshot(snapshot, page.url(), await page.title());
}

function isMediumTagUrl(value) {
  try {
    const parsed = new URL(value);
    return (
      /(^|\.)medium\.com$/i.test(parsed.hostname) &&
      /^\/tag\/[^/]+(?:\/recommended)?\/?$/i.test(parsed.pathname)
    );
  } catch (_error) {
    return false;
  }
}

function isMediumArticleUrl(value) {
  try {
    const parsed = new URL(value);
    return (
      /(^|\.)medium\.com$/i.test(parsed.hostname) &&
      !/^\/(?:tag|m|me|p)\//i.test(parsed.pathname) &&
      /[a-f0-9]{12}\/?$/i.test(parsed.pathname)
    );
  } catch (_error) {
    return false;
  }
}

function contextsToInspect(browser, context) {
  if (browser && typeof browser.contexts === "function") {
    const contexts = browser.contexts();
    if (contexts.length > 0) {
      return contexts;
    }
  }

  return context ? [context] : [];
}

function uniquePages(pages) {
  const seen = new Set();
  const unique = [];

  for (const page of pages) {
    if (!page || page.isClosed()) {
      continue;
    }

    const key = page;
    if (seen.has(key)) {
      continue;
    }

    seen.add(key);
    unique.push(page);
  }

  return unique;
}

function pagesToInspect(browser, context, preferredPage) {
  return uniquePages([
    preferredPage,
    ...contextsToInspect(browser, context)
    .flatMap((candidateContext) => candidateContext.pages())
  ])
    .filter((candidate) => !candidate.isClosed() && isMediumTagUrl(candidate.url()));
}

function articlePagesToInspect(browser, context, preferredPage) {
  return uniquePages([
    preferredPage,
    ...contextsToInspect(browser, context)
    .flatMap((candidateContext) => candidateContext.pages())
  ])
    .filter((candidate) => !candidate.isClosed() && isMediumArticleUrl(candidate.url()));
}

function isMediumUrl(value) {
  try {
    const parsed = new URL(value);
    return /(^|\.)medium\.com$/i.test(parsed.hostname);
  } catch (_error) {
    return false;
  }
}

function visiblePageSummary(browser, context, preferredPage) {
  const urls = uniquePages([
    preferredPage,
    ...contextsToInspect(browser, context)
    .flatMap((candidateContext) => candidateContext.pages())
  ])
    .filter((candidate) => !candidate.isClosed())
    .map((candidate) => candidate.url())
    .filter((url) => url && url !== "about:blank");
  const mediumUrls = urls.filter(isMediumUrl);

  if (mediumUrls.length > 0) {
    return `Waiting: no supported Medium page in watcher Chrome. Medium tabs seen: ${mediumUrls.slice(0, 3).join(" | ")}`;
  }

  return "Waiting: no Medium tabs visible to watcher Chrome. Open a Medium tag page or article page in the Chrome window started by this watcher.";
}

function pageStateFor(page, pageStates) {
  let state = pageStates.get(page);
  if (!state) {
    state = {
      lastSignature: "",
      lastImportedUrls: new Set(),
      lastVisibleUrlSignature: "",
      lastArticleTextHash: "",
      lastStatusMessage: "",
    };
    pageStates.set(page, state);
  }
  return state;
}

async function captureArticleTextPayload(page, options) {
  const articleUrl = page.url();
  const collectedAt = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const extracted = await page.evaluate((maxChars) => {
    const clean = (value) => String(value || "").replace(/\u00a0/g, " ").replace(/\s+/g, " ").trim();
    const pageText = clean(document.body ? document.body.innerText : "");
    const hasHighlightStyle = (element) => {
      const style = window.getComputedStyle(element);
      const background = style.backgroundColor || "";

      if (!background || background === "transparent" || background === "rgba(0, 0, 0, 0)") {
        return false;
      }

      if (/rgb\(\s*255\s*,\s*255\s*,\s*255\s*\)/i.test(background)) {
        return false;
      }

      return true;
    };
    const article = document.querySelector("article") || document.querySelector("main") || document.body;
    const blockSelector = "h1, h2, h3, h4, p, blockquote, pre, li, figcaption";
    const textBlocks = [...article.querySelectorAll(blockSelector)]
      .map((node, index) => {
        const tag = node.tagName.toLowerCase();
        const text = clean(node.innerText || node.textContent);
        if (!text) {
          return null;
        }

        const type = tag === "figcaption" ? "caption" :
          tag === "blockquote" ? "blockquote" :
          tag === "li" ? "list_item" :
          /^h[1-6]$/.test(tag) ? "heading" :
          tag === "pre" ? "code" :
          "paragraph";

        return { position: index + 1, type, tag, text };
      })
      .filter(Boolean);
    const firstLinks = [...document.querySelectorAll("a[href]")]
      .map((link) => ({
        text: clean(link.innerText || link.textContent),
        href: link.href || link.getAttribute("href") || "",
      }))
      .filter((link) => link.text && link.href);
    const authorLink = firstLinks.find((link) => /\/@[^/?#]+/i.test(link.href)) || null;
    const publicationLink = firstLinks.find((link) => {
      try {
        const parsed = new URL(link.href, "https://medium.com");
        return /(^|\.)medium\.com$/i.test(parsed.hostname) &&
          !/^\/(@|tag|m|p)\//i.test(parsed.pathname) &&
          parsed.pathname.split("/").filter(Boolean).length === 1;
      } catch (_error) {
        return false;
      }
    }) || null;
    const articleTags = [...document.querySelectorAll('a[href*="/tag/"]')]
      .map((link) => {
        try {
          const parsed = new URL(link.href || link.getAttribute("href"), "https://medium.com");
          const match = parsed.pathname.match(/^\/tag\/([^/?#]+)/i);
          return match ? decodeURIComponent(match[1]).toLowerCase() : "";
        } catch (_error) {
          return "";
        }
      })
      .filter(Boolean);
    const images = [...article.querySelectorAll("img")]
      .map((image, index) => {
        const src = image.currentSrc || image.src || image.getAttribute("src") || "";
        const figure = image.closest("figure");
        const captionNode = figure ? figure.querySelector("figcaption") : null;
        return {
          position: index + 1,
          src,
          alt: clean(image.getAttribute("alt") || ""),
          caption: clean(captionNode ? captionNode.innerText || captionNode.textContent : ""),
          width: image.naturalWidth || null,
          height: image.naturalHeight || null,
        };
      })
      .filter((image) => image.src && !/^data:/i.test(image.src));
    const readTimeMatch = pageText.match(/(\d+\s+min\s+read)/i);
    const readTime = readTimeMatch ? clean(readTimeMatch[1].toLowerCase()) : "";
    const publishedLabelMatch = pageText.match(/(?:^|[·\n]\s*)(Just now|Today|\d+\s*(?:m|min|mins|minute|minutes|h|hr|hrs|hour|hours)\s+ago|(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\.?\s+\d{1,2}(?:,\s*\d{4})?)(?:\s*[·\n]|$)/i);
    const publishedLabel = publishedLabelMatch ? clean(publishedLabelMatch[1]) : "";
    const highlightCandidates = [...article.querySelectorAll("mark, span, strong, em, p, blockquote")]
      .filter((node) => {
        const className = String(node.className || "");
        return node.tagName.toLowerCase() === "mark" ||
          /highlight/i.test(className) ||
          hasHighlightStyle(node);
      })
      .map((node) => clean(node.innerText || node.textContent))
      .filter((text) => text.length >= 20);
    const highlightedText = [...new Set(highlightCandidates)]
      .filter((text, _index, all) => !all.some((other) => other !== text && other.includes(text)));
    const clone = article ? article.cloneNode(true) : document.body.cloneNode(true);
    clone.querySelectorAll("script, style, noscript, nav, aside, footer, form, button, [role='button'], [aria-label*='clap' i], [aria-label*='response' i], [aria-label*='share' i], [aria-label*='bookmark' i]").forEach((node) => node.remove());
    const title = clean((document.querySelector("h1") || {}).innerText || document.title);
    const rawText = clean(clone.innerText || clone.textContent || "");
    const text = rawText.slice(0, maxChars);
    return {
      title,
      authorName: authorLink ? authorLink.text : "",
      authorUrl: authorLink ? authorLink.href : "",
      publicationName: publicationLink ? publicationLink.text : "",
      publicationUrl: publicationLink ? publicationLink.href : "",
      publishedLabel,
      visibleText: text,
      textBlocks,
      textTruncated: rawText.length > maxChars,
      rawLength: rawText.length,
      readTime,
      articleTags: [...new Set(articleTags)],
      images,
      highlightedText,
      isMemberOnly: /member-only story/i.test(pageText) || Boolean(document.querySelector('[aria-label*="Member-only story" i]')),
      hasPaywallPrompt: /(get unlimited access|become a medium member|start reading with medium|unlock this story|read member-only stories|membership gives you access)/i.test(pageText),
      hasSignInPrompt: /(sign in|sign up|create account)/i.test(pageText),
      hasListenShareSaveControls: /(listen|share|bookmark|save)/i.test(pageText),
    };
  }, options.maxArticleTextChars);
  const normalizedText = String(extracted.visibleText || "").trim();
  const hash = textHash(normalizedText);
  const words = wordCount(normalizedText);
  const readableStatus = (() => {
    if (!normalizedText) {
      return "empty";
    }

    if (extracted.hasPaywallPrompt && words < 500) {
      return "member_preview_only";
    }

    if (extracted.isMemberOnly) {
      return "member_full_visible";
    }

    return "free_full_visible";
  })();
  const captureCompleteness = (() => {
    if (!normalizedText) {
      return "empty";
    }

    if (readableStatus === "member_preview_only") {
      return "preview_only";
    }

    if (extracted.textTruncated) {
      return "truncated_by_collector";
    }

    if (words < 300 && extracted.hasPaywallPrompt) {
      return "preview_only";
    }

    return "full_visible";
  })();
  const errorSignals = [
    extracted.isMemberOnly ? "member_only" : "",
    extracted.hasPaywallPrompt ? "paywall_prompt" : "",
    extracted.hasSignInPrompt ? "sign_in_prompt" : "",
    extracted.textTruncated ? "truncated_by_collector" : "",
  ].filter(Boolean).join(",");

  return {
    source_type: "medium_article_text_snapshot",
    schema_version: 1,
    article_url: articleUrl,
    medium_post_id: (articleUrl.match(/([a-f0-9]{12})(?:\/?$)/i) || [null, ""])[1].toLowerCase(),
    title: extracted.title || "",
    author_name: extracted.authorName || "",
    author_url: extracted.authorUrl || "",
    publication_name: extracted.publicationName || "",
    publication_url: extracted.publicationUrl || "",
    published_label: extracted.publishedLabel || "",
    read_time: extracted.readTime || "",
    text_blocks: extracted.textBlocks || [],
    article_tags: extracted.articleTags || [],
    images: extracted.images || [],
    collected_at: collectedAt,
    extraction_method: "playwright_article_inner_text",
    text_hash: hash,
    visible_text: normalizedText,
    highlighted_text: extracted.highlightedText || [],
    word_count: words,
    text_truncated: Boolean(extracted.textTruncated),
    max_chars: options.maxArticleTextChars,
    capture_completeness: captureCompleteness,
    readable_status: readableStatus,
    error_message: errorSignals,
  };
}

async function openBrowser(options) {
  if (options.connectCdp) {
    const browser = await chromium.connectOverCDP(options.connectCdp);
    const context = browser.contexts()[0] || await browser.newContext();
    const page = context.pages()[0] || await context.newPage();
    return { browser, context, page, ownsContext: false };
  }

  fs.mkdirSync(options.userDataDir, { recursive: true });
  fs.mkdirSync(options.browserHome, { recursive: true });

  const launchOptions = {
    headless: options.headless,
    viewport: { width: 1280, height: 900 },
    locale: "en-US",
    env: {
      ...process.env,
      HOME: path.resolve(options.browserHome),
      XDG_CONFIG_HOME: path.resolve(options.browserHome, ".config"),
      XDG_CACHE_HOME: path.resolve(options.browserHome, ".cache"),
    },
    args: [
      "--disable-crash-reporter",
      "--disable-crashpad",
    ],
  };

  if (options.channel) {
    launchOptions.channel = options.channel;
  }

  const context = await chromium.launchPersistentContext(options.userDataDir, launchOptions);
  const page = context.pages()[0] || await context.newPage();
  return { browser: null, context, page, ownsContext: true };
}

async function main() {
  const options = parseArgs(process.argv);
  const helpers = await import("./medium_tag_snapshot_watcher_helpers.mjs");
  const workspace = process.cwd();
  let context;
  let browser;
  let ownsContext = false;
  const pageStates = new WeakMap();
  const loopState = { lastStatusMessage: "" };

  console.log("Medium rendered tag-page watcher");
  console.log("--------------------------------");
  console.log(`Opening: ${options.startUrl}`);
  if (options.connectCdp) {
    console.log(`Attaching to Chrome: ${options.connectCdp}`);
  } else {
    console.log(`Browser profile: ${options.userDataDir}`);
    console.log(`Browser home: ${options.browserHome}`);
  }
  if (options.waitForMedium) {
    console.log("Navigate to a Medium /tag/... page, /tag/.../recommended page, or article page when you are ready.");
    console.log("The watcher will stay idle until it sees a supported Medium page.");
  }
  console.log(`Polling every ${options.pollSeconds}s. Scroll the opened browser window after tracking starts.`);
  console.log("Type p + Enter to pause/resume, or q + Enter to quit.");
  console.log("");

  try {
    const keyboardState = { paused: false };
    setupKeyboardControls(keyboardState);
    const opened = await openBrowser(options);
    browser = opened.browser;
    context = opened.context;
    ownsContext = opened.ownsContext;
    let page = opened.page;
    context.on("page", (newPage) => {
      page = newPage;
    });

    if (!options.connectCdp && options.startUrl) {
      await page.goto(options.startUrl, { waitUntil: "domcontentloaded", timeout: 60000 });
      await page.waitForLoadState("networkidle", { timeout: 30000 }).catch(() => {});
    }

    while (true) {
      if (keyboardState.paused) {
        await sleep(250);
        continue;
      }

      const pages = pagesToInspect(browser, context, page);
      const articlePages = articlePagesToInspect(browser, context, page);

      if (pages.length === 0 && articlePages.length === 0) {
        logChangedStatus(loopState, visiblePageSummary(browser, context, page));
        await sleep(options.pollSeconds * 1000);
        continue;
      }

      loopState.lastStatusMessage = "";
      let importedAny = false;

      for (const page of pages) {
        const state = pageStateFor(page, pageStates);
        const visibleUrls = await visibleArticleUrls(page).catch(() => new Set());
        const visibleUrlSignature = setSignature(visibleUrls);
        const visibleNewUrls = [...visibleUrls].filter((url) => !state.lastImportedUrls.has(url));
        const visibleUrlsChanged = visibleUrlSignature && visibleUrlSignature !== state.lastVisibleUrlSignature;
        const shouldCapture = (
          state.lastSignature === "" ||
          (visibleUrlsChanged && visibleNewUrls.length >= options.minNewUrls)
        );

        if (!shouldCapture) {
          logChangedStatus(state, `Checked: visible article URLs unchanged; no new imports.`);
          continue;
        }

        const payload = await capturePayload(page, helpers, options).catch((error) => ({
          error: compactCaptureError(error),
          cards: [],
        }));

        if (payload.error) {
          logChangedStatus(state, `Waiting: ${formatCaptureError(payload.error)}`);
          continue;
        }

        const signature = helpers.signatureForPayload(payload);
        const urls = helpers.urlSet(payload);
        const newUrls = [...urls].filter((url) => !state.lastImportedUrls.has(url));
        const changed = Boolean(signature) && signature !== state.lastSignature;
        const shouldImport = (
          payload.cards.length > 0 &&
          changed &&
          (state.lastSignature === "" || newUrls.length >= options.minNewUrls)
        );

        if (payload.cards.length === 0) {
          logChangedStatus(state, `Checked: zero cards on ${page.url()} [${await page.title().catch(() => "")}]`);
          continue;
        }

        if (!shouldImport) {
          logChangedStatus(state, formatSummary("Checked:", payload, {
            newUrls: newUrls.length,
          }));
          continue;
        }

        state.lastStatusMessage = "";
        const snapshotPath = helpers.writeSnapshot(payload, options.snapshotDir);
        state.lastSignature = signature;
        state.lastImportedUrls = urls;
        state.lastVisibleUrlSignature = visibleUrlSignature || setSignature(urls);
        importedAny = true;

        console.log(formatSummary("Snapshot written:", payload, {
          newUrls: newUrls.length,
          file: snapshotPath,
        }));
        console.log(`Tag import article: ${payloadTitleSummary(payload)}`);

        if (options.importSnapshots) {
          const result = helpers.importSnapshot(snapshotPath, workspace);
          if (result.status === 0) {
            console.log("Imported snapshot into SQLite.");
          } else {
            console.log(`Import failed with status ${result.status}.`);
            if (result.stderr) console.log(result.stderr.trim());
            if (result.stdout) console.log(result.stdout.trim());
          }
        }

        if (options.once) {
          return;
        }
      }

      for (const articlePage of articlePages) {
        const state = pageStateFor(articlePage, pageStates);
        const payload = await captureArticleTextPayload(articlePage, options).catch((error) => ({
          error: compactCaptureError(error),
        }));

        if (payload.error) {
          logChangedStatus(state, `Waiting: article text capture failed: ${formatCaptureError(payload.error)}`);
          continue;
        }

        if (payload.text_hash && payload.text_hash === state.lastArticleTextHash) {
          logChangedStatus(state, `Checked: article text unchanged; no new text snapshot.`);
          continue;
        }

        state.lastStatusMessage = "";
        state.lastArticleTextHash = payload.text_hash;
        const textSnapshotPath = helpers.writeArticleTextSnapshot(payload, options.articleTextSnapshotDir);
        importedAny = true;

        console.log(
          `Article text snapshot written: "${compactTitle(payload.title || payload.article_url)}" words=${payload.word_count} status=${payload.readable_status} file=${textSnapshotPath}`
        );

        if (options.importSnapshots) {
          const result = helpers.importArticleTextSnapshot(textSnapshotPath, workspace);
          if (result.status === 0) {
            console.log("Imported article text snapshot into SQLite.");
          } else {
            console.log(`Article text import failed with status ${result.status}.`);
            if (result.stderr) console.log(result.stderr.trim());
            if (result.stdout) console.log(result.stdout.trim());
          }
        }

        if (options.once) {
          return;
        }
      }

      if (!importedAny) {
        const watchedPages = pages.length + articlePages.length;
        const pageSignature = [
          ...pages.map((candidate) => candidate.url()),
          ...articlePages.map((candidate) => candidate.url()),
        ].sort().join("|");
        logChangedStatus(loopState, `Checked ${watchedPages} Medium page${watchedPages === 1 ? "" : "s"}; no new imports. ${pageSignature}`);
      }

      await sleep(options.pollSeconds * 1000);
    }
  } finally {
    if (ownsContext && context) {
      await context.close();
    } else if (browser) {
      await browser.close();
    }
  }
}

main().catch((error) => {
  console.error("Medium tag-page watcher failed.");
  console.error(error.message);
  process.exitCode = 1;
});
