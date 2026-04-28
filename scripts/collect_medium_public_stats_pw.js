const path = require("path");
const readline = require("readline/promises");
const { stdin: input, stdout: output } = require("process");
const { chromium } = require("playwright");
const Database = require("better-sqlite3");

const databasePath = path.join("data", "medium_articles.sqlite");
const maxArticles = 10;
const delaySeconds = 3;
const headless = false;
const slowMo = 250;
const humanPauseOnCheck = true;
const manualNavigateMode = true;
const manualStartUrl = "about:blank";
const parseMethod = "playwright_rendered_human_in_loop";
const manualNavigationParseMethod = "playwright_manual_navigation";

function nowUtcParts() {
  const now = new Date();
  const observedAt = now.toISOString().replace(/\.\d{3}Z$/, "Z");
  const observedDate = observedAt.slice(0, 10);

  return { observedAt, observedDate };
}

function cleanText(value) {
  if (value === undefined || value === null) {
    return null;
  }

  const cleaned = String(value).replace(/\s+/g, " ").trim();
  return cleaned.length === 0 ? null : cleaned;
}

function parseCompactNumber(value) {
  const cleaned = cleanText(value);

  if (!cleaned) {
    return null;
  }

  const compact = cleaned.replace(/,/g, "").replace(/\s+/g, "").toUpperCase();
  const match = compact.match(/^([0-9]+(?:\.[0-9]+)?)([KM])?$/);

  if (!match) {
    return null;
  }

  const numberPart = Number(match[1]);
  const suffix = match[2];
  const multiplier = suffix === "K" ? 1000 : suffix === "M" ? 1000000 : 1;

  return Math.round(numberPart * multiplier);
}

function createPublicStatsTable(db) {
  db.exec(`
    CREATE TABLE IF NOT EXISTS medium_article_public_stats (
      id INTEGER PRIMARY KEY,
      article_url TEXT NOT NULL,
      observed_at TEXT NOT NULL,
      observed_date TEXT NOT NULL,
      claps_count INTEGER,
      responses_count INTEGER,
      claps_raw TEXT,
      responses_raw TEXT,
      parse_status TEXT NOT NULL,
      parse_method TEXT,
      error_message TEXT,
      UNIQUE(article_url, observed_date)
    )
  `);
}

function scoreArticle(article) {
  const preferredTags = new Set(["personal-finance", "investing", "etf", "financial-independence"]);
  const positiveKeywords = [
    "investing",
    "investor",
    "etf",
    "portfolio",
    "retirement",
    "financial independence",
    "money",
    "personal finance",
    "stock market",
  ];
  const negativeKeywords = [
    "crypto recovery",
    "recover my",
    "lost usdt",
    "tax services",
    "accounting services",
    "bookkeeping",
    "corporate compliance",
    "gst",
    "forex broker",
    "quickbooks",
  ];

  const text = [
    article.source_tag,
    article.title,
    article.snippet,
    article.categories,
  ].filter(Boolean).join(" ").toLowerCase();

  let score = preferredTags.has(article.source_tag) ? 20 : 0;

  for (const keyword of positiveKeywords) {
    if (text.includes(keyword)) {
      score += 10;
    }
  }

  for (const keyword of negativeKeywords) {
    if (text.includes(keyword)) {
      score -= 25;
    }
  }

  // Simple non-English signal: keep articles with mostly ASCII titles higher.
  const title = article.title || "";
  const nonAsciiCount = [...title].filter((character) => character.charCodeAt(0) > 127).length;
  if (title.length > 0 && nonAsciiCount / title.length > 0.15) {
    score -= 10;
  }

  return score;
}

function selectArticlesForBatch(db, observedDate) {
  const candidates = db.prepare(`
    SELECT source_tag, title, url, snippet, categories, published_at, updated_at, fetched_at
    FROM medium_articles
    WHERE url NOT IN (
      SELECT article_url
      FROM medium_article_public_stats
      WHERE observed_date = ?
    )
    ORDER BY COALESCE(published_at, updated_at, fetched_at) DESC, id DESC
    LIMIT 50
  `).all(observedDate);

  return candidates
    .map((article) => ({ ...article, score: scoreArticle(article) }))
    .sort((a, b) => {
      if (b.score !== a.score) {
        return b.score - a.score;
      }

      return String(b.published_at || b.updated_at || b.fetched_at || "").localeCompare(
        String(a.published_at || a.updated_at || a.fetched_at || "")
      );
    })
    .slice(0, maxArticles);
}

function articleForManualUrl(db, articleUrl) {
  const storedArticle = db.prepare(`
    SELECT source_tag, title, url, snippet, categories
    FROM medium_articles
    WHERE url = ?
    LIMIT 1
  `).get(articleUrl);

  if (storedArticle) {
    return storedArticle;
  }

  return {
    source_tag: "manual",
    title: "Manual test URL",
    url: articleUrl,
    snippet: null,
    categories: null,
  };
}

function savePublicStats(db, observation) {
  db.prepare(`
    INSERT INTO medium_article_public_stats (
      article_url,
      observed_at,
      observed_date,
      claps_count,
      responses_count,
      claps_raw,
      responses_raw,
      parse_status,
      parse_method,
      error_message
    ) VALUES (
      @article_url,
      @observed_at,
      @observed_date,
      @claps_count,
      @responses_count,
      @claps_raw,
      @responses_raw,
      @parse_status,
      @parse_method,
      @error_message
    )
    ON CONFLICT(article_url, observed_date) DO UPDATE SET
      observed_at = excluded.observed_at,
      claps_count = excluded.claps_count,
      responses_count = excluded.responses_count,
      claps_raw = excluded.claps_raw,
      responses_raw = excluded.responses_raw,
      parse_status = excluded.parse_status,
      parse_method = excluded.parse_method,
      error_message = excluded.error_message
  `).run(observation);
}

function findEngagementRaw(candidates, patterns) {
  for (const candidate of candidates) {
    if (!candidate) {
      continue;
    }

    for (const pattern of patterns) {
      const match = candidate.match(pattern);

      if (match && match[1]) {
        return cleanText(match[1]);
      }
    }
  }

  return null;
}

function parseRenderedStats(candidates) {
  const numberPattern = "([0-9][0-9,.]*\\s*[KkMm]?)";

  const clapsPatterns = [
    new RegExp(`${numberPattern}\\s+(?:claps?|applause|recommends?|recommendations?)\\b`, "i"),
    new RegExp(`\\b(?:claps?|applause|recommends?|recommendations?)\\b\\D{0,50}${numberPattern}`, "i"),
    new RegExp(`\\b(?:clap|recommend)\\b[^\\n]{0,80}\\b${numberPattern}\\b`, "i"),
  ];

  const responsesPatterns = [
    new RegExp(`${numberPattern}\\s+(?:responses?|comments?)\\b`, "i"),
    new RegExp(`\\b(?:responses?|comments?)\\b\\D{0,50}${numberPattern}`, "i"),
    new RegExp(`\\b(?:respond|response|comment)\\b[^\\n]{0,80}\\b${numberPattern}\\b`, "i"),
  ];

  const clapsRaw = findEngagementRaw(candidates, clapsPatterns);
  const responsesRaw = findEngagementRaw(candidates, responsesPatterns);

  return {
    clapsRaw,
    responsesRaw,
    clapsCount: parseCompactNumber(clapsRaw),
    responsesCount: parseCompactNumber(responsesRaw),
  };
}

async function visibleTextCandidates(page) {
  const candidates = [];

  const locatorTexts = await page.locator("button, a, [role='button'], [aria-label], [title]").evaluateAll((nodes) => {
    const isVisible = (node) => {
      const element = node instanceof HTMLElement ? node : null;

      if (!element) {
        return false;
      }

      const style = window.getComputedStyle(element);
      const rect = element.getBoundingClientRect();

      return style.visibility !== "hidden" && style.display !== "none" && rect.width > 0 && rect.height > 0;
    };

    return nodes
      .filter(isVisible)
      .flatMap((node) => [
        node.innerText,
        node.textContent,
        node.getAttribute("aria-label"),
        node.getAttribute("title"),
      ])
      .filter(Boolean);
  });

  candidates.push(...locatorTexts);

  // Fallback only reads rendered visible body text in memory. It is not saved.
  const bodyText = await page.locator("body").innerText({ timeout: 5000 }).catch(() => null);
  candidates.push(bodyText);

  return [...new Set(candidates.map(cleanText).filter(Boolean))];
}

async function visiblePageText(page) {
  return cleanText(await page.locator("body").innerText({ timeout: 5000 }).catch(() => null));
}

function looksLikeBrowserCheck(text) {
  if (!text) {
    return false;
  }

  return /verify you are human|checking your browser|complete the security check|are you a human|captcha/i.test(text);
}

function looksLikeUnreadableMediumPage(text) {
  if (!text) {
    return false;
  }

  return /500 apologies, but something went wrong on our end|something went wrong on our end|check medium's site status/i.test(text);
}

async function waitForManualCheckIfNeeded(page, rl) {
  if (!humanPauseOnCheck) {
    return false;
  }

  const bodyText = await visiblePageText(page);

  if (!looksLikeBrowserCheck(bodyText)) {
    return false;
  }

  console.log("\nA browser check may be shown. Complete it manually in the visible browser, then press Enter here to continue.");
  await rl.question("");
  await page.waitForTimeout(8000);

  return true;
}

function resultMessage(raw, count) {
  return count === null || count === undefined ? "not found" : `${raw} -> ${count}`;
}

function urlsLookDifferent(currentUrl, intendedUrl) {
  try {
    const current = new URL(currentUrl);
    const intended = new URL(intendedUrl);
    const currentPath = current.pathname.replace(/\/$/, "");
    const intendedPath = intended.pathname.replace(/\/$/, "");

    return current.hostname !== intended.hostname || currentPath !== intendedPath;
  } catch (_error) {
    return currentUrl !== intendedUrl;
  }
}

async function parseCurrentPageForArticle(db, page, article, batchNumber, attemptedArticles, rl) {
  const { observedAt, observedDate } = nowUtcParts();

  console.log(`\nArticle ${batchNumber} of ${attemptedArticles}`);
  console.log(`Title: ${article.title}`);
  console.log(`Intended URL: ${article.url}`);
  console.log("Open this URL manually in the Playwright browser. When the article is fully loaded and the clap/comment numbers are visible, press Enter here.");

  await rl.question("");
  await page.waitForTimeout(2000);

  const currentUrl = page.url();
  console.log(`Current page URL: ${currentUrl}`);

  if (urlsLookDifferent(currentUrl, article.url)) {
    console.log("Warning: the current page URL looks different from the intended article URL. Parsing will continue anyway.");
  }

  try {
    const bodyText = await visiblePageText(page);
    const candidates = await visibleTextCandidates(page);
    const stats = parseRenderedStats(candidates);

    if (looksLikeUnreadableMediumPage(bodyText)) {
      savePublicStats(db, {
        article_url: article.url,
        observed_at: observedAt,
        observed_date: observedDate,
        claps_count: null,
        responses_count: null,
        claps_raw: null,
        responses_raw: null,
        parse_status: "fetch_failed",
        parse_method: manualNavigationParseMethod,
        error_message: "Medium rendered an error page",
      });

      console.log("Claps: not found");
      console.log("Responses: not found");
      console.log("Parse status: fetch_failed");

      return "fetch_failed";
    }

    const parseStatus = stats.clapsCount !== null || stats.responsesCount !== null ? "ok" : "not_found";

    savePublicStats(db, {
      article_url: article.url,
      observed_at: observedAt,
      observed_date: observedDate,
      claps_count: stats.clapsCount,
      responses_count: stats.responsesCount,
      claps_raw: stats.clapsRaw,
      responses_raw: stats.responsesRaw,
      parse_status: parseStatus,
      parse_method: manualNavigationParseMethod,
      error_message: null,
    });

    console.log(`Claps: ${resultMessage(stats.clapsRaw, stats.clapsCount)}`);
    console.log(`Responses: ${resultMessage(stats.responsesRaw, stats.responsesCount)}`);
    console.log(`Parse status: ${parseStatus}`);

    return parseStatus;
  } catch (error) {
    savePublicStats(db, {
      article_url: article.url,
      observed_at: observedAt,
      observed_date: observedDate,
      claps_count: null,
      responses_count: null,
      claps_raw: null,
      responses_raw: null,
      parse_status: "error",
      parse_method: manualNavigationParseMethod,
      error_message: error.message,
    });

    console.log("Claps: not found");
    console.log("Responses: not found");
    console.log("Parse status: error");

    return "error";
  }
}

async function collectOneArticle(db, page, article, batchNumber, attemptedArticles, rl) {
  const { observedAt, observedDate } = nowUtcParts();

  console.log(`\nArticle ${batchNumber} of ${attemptedArticles}`);
  console.log(`Title: ${article.title}`);
  console.log(`URL: ${article.url}`);

  try {
    const response = await page.goto(article.url, {
      waitUntil: "domcontentloaded",
      timeout: 45000,
    });

    const status = response ? response.status() : null;
    await page.waitForTimeout(8000);
    const manualPauseTriggered = await waitForManualCheckIfNeeded(page, rl);

    const bodyTextAfterPause = await visiblePageText(page);
    const candidates = await visibleTextCandidates(page);
    const stats = parseRenderedStats(candidates);

    if (
      ((!response || status >= 400) || looksLikeUnreadableMediumPage(bodyTextAfterPause)) &&
      stats.clapsCount === null &&
      stats.responsesCount === null
    ) {
      savePublicStats(db, {
        article_url: article.url,
        observed_at: observedAt,
        observed_date: observedDate,
        claps_count: null,
        responses_count: null,
        claps_raw: null,
        responses_raw: null,
        parse_status: "fetch_failed",
        parse_method: parseMethod,
        error_message: looksLikeUnreadableMediumPage(bodyTextAfterPause)
          ? `Medium rendered an error page${manualPauseTriggered ? " after manual check pause" : ""}`
          : status === null ? "Navigation returned no response." : `HTTP status ${status}${manualPauseTriggered ? " after manual check pause" : ""}`,
      });

      console.log("Claps: not found");
      console.log("Responses: not found");
      console.log("Parse status: fetch_failed");

      return "fetch_failed";
    }
    const parseStatus = stats.clapsCount !== null || stats.responsesCount !== null ? "ok" : "not_found";

    savePublicStats(db, {
      article_url: article.url,
      observed_at: observedAt,
      observed_date: observedDate,
      claps_count: stats.clapsCount,
      responses_count: stats.responsesCount,
      claps_raw: stats.clapsRaw,
      responses_raw: stats.responsesRaw,
      parse_status: parseStatus,
      parse_method: parseMethod,
      error_message: null,
    });

    console.log(`Claps: ${resultMessage(stats.clapsRaw, stats.clapsCount)}`);
    console.log(`Responses: ${resultMessage(stats.responsesRaw, stats.responsesCount)}`);
    console.log(`Parse status: ${parseStatus}`);

    return parseStatus;
  } catch (error) {
    savePublicStats(db, {
      article_url: article.url,
      observed_at: observedAt,
      observed_date: observedDate,
      claps_count: null,
      responses_count: null,
      claps_raw: null,
      responses_raw: null,
      parse_status: "error",
      parse_method: parseMethod,
      error_message: error.message,
    });

    console.log("Claps: not found");
    console.log("Responses: not found");
    console.log("Parse status: error");

    return "error";
  }
}

async function main() {
  console.log("Medium Public Stats Playwright Collector");
  console.log("========================================");

  const db = new Database(databasePath);

  try {
    const hasArticlesTable = db.prepare(`
      SELECT COUNT(*) AS n
      FROM sqlite_master
      WHERE type = 'table' AND name = 'medium_articles'
    `).get().n > 0;

    if (!hasArticlesTable) {
      throw new Error("The medium_articles table does not exist yet. Run the RSS collector first.");
    }

    createPublicStatsTable(db);

    const manualUrl = process.argv[2] ? cleanText(process.argv[2]) : null;
    const { observedDate } = nowUtcParts();
    const articles = manualUrl ? [articleForManualUrl(db, manualUrl)] : selectArticlesForBatch(db, observedDate);

    if (articles.length === 0) {
      console.log("No articles need a public stats observation for today.");
      console.log(`Database path: ${databasePath}`);
      return;
    }

    console.log("\nChosen articles");
    console.log("---------------");
    articles.forEach((article, index) => {
      const scoreText = article.score === undefined ? "" : ` [score ${article.score}]`;
      console.log(`${index + 1}. ${article.title}${scoreText}`);
    });

    const browser = await chromium.launch({
      headless,
      slowMo,
    });

    console.log(`Visible Chromium launched: ${!headless}`);

    const context = await browser.newContext({
      viewport: { width: 1280, height: 900 },
      locale: "en-US",
    });

    const page = await context.newPage();
    const rl = readline.createInterface({ input, output });
    const statuses = [];

    try {
      if (manualNavigateMode) {
        console.log("\nManual navigation mode is on.");
        console.log("Use the opened Playwright browser. If Medium shows a check, complete it manually.");
        console.log("The browser opens to a blank page so you can paste the article URL directly into the address bar.");

        if (manualStartUrl !== "about:blank") {
          await page.goto(manualStartUrl, {
            waitUntil: "domcontentloaded",
            timeout: 45000,
          }).catch((error) => {
            console.log(`Startup navigation to ${manualStartUrl} did not complete cleanly: ${error.message}`);
          });
          await page.waitForTimeout(3000);
        }
      }

      for (let articleIndex = 0; articleIndex < articles.length; articleIndex += 1) {
        const status = manualNavigateMode
          ? await parseCurrentPageForArticle(db, page, articles[articleIndex], articleIndex + 1, articles.length, rl)
          : await collectOneArticle(db, page, articles[articleIndex], articleIndex + 1, articles.length, rl);
        statuses.push(status);

        if (articleIndex < articles.length - 1) {
          await page.waitForTimeout(delaySeconds * 1000);
        }
      }
    } finally {
      rl.close();
      await context.close();
      await browser.close();
    }

    const countStatus = (statusName) => statuses.filter((status) => status === statusName).length;

    console.log("\nSummary");
    console.log("=======");
    console.log(`Articles attempted: ${articles.length}`);
    console.log(`OK count: ${countStatus("ok")}`);
    console.log(`Not found count: ${countStatus("not_found")}`);
    console.log(`Fetch failed count: ${countStatus("fetch_failed")}`);
    console.log(`Error count: ${countStatus("error")}`);
    console.log(`Database path: ${databasePath}`);
  } finally {
    db.close();
  }
}

main().catch((error) => {
  console.error("Playwright public stats collector failed.");
  console.error(error.message);
  process.exitCode = 1;
});
