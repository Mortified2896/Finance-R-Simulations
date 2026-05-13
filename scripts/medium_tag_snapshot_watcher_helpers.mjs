import fs from "node:fs";
import path from "node:path";
import { spawnSync } from "node:child_process";

const sourceType = "medium_tag_page_bookmarklet";
const searchTagsSourceType = "medium_search_tags_page";
const schemaVersion = 20;
const searchTagsSchemaVersion = 1;
const defaultSnapshotDir = path.join("data", "medium_tag_watcher_snapshots");
const defaultArticleTextSnapshotDir = path.join("data", "medium_article_text_snapshots");

function cleanText(value) {
  if (value === null || value === undefined) {
    return "";
  }

  return String(value).replace(/\s+/g, " ").trim();
}

function normalizeMediumUrl(value, baseUrl = "https://medium.com") {
  const cleaned = cleanText(value);

  if (!cleaned) {
    return "";
  }

  try {
    const parsed = new URL(cleaned, baseUrl);
    parsed.search = "";
    parsed.hash = "";
    return parsed.toString().replace(/\/+$/, "");
  } catch (_error) {
    return cleaned.split(/[?#]/)[0].replace(/\/+$/, "");
  }
}

function normalizeSourceUrl(value, baseUrl = "https://medium.com") {
  const cleaned = cleanText(value);

  if (!cleaned) {
    return "";
  }

  try {
    const parsed = new URL(cleaned, baseUrl);
    parsed.hash = "";
    return parsed.toString().replace(/\/+$/, "");
  } catch (_error) {
    return cleaned.split("#")[0].replace(/\/+$/, "");
  }
}

function normalizeTrackingContext(value) {
  return cleanText(value) || "unknown_context";
}

function extractMediumPostId(value) {
  const match = normalizeMediumUrl(value).match(/([a-f0-9]{12})(?:\/?$)/i);
  return match ? match[1].toLowerCase() : "";
}

function titleFromMediumUrl(value) {
  const normalized = normalizeMediumUrl(value);
  const match = normalized.match(/\/([^/?#]+)-([a-f0-9]{12})(?:\/)?$/i);

  if (!match) {
    return "";
  }

  return cleanText(
    decodeURIComponent(match[1])
      .replace(/-/g, " ")
      .replace(/\b\w/g, (letter) => letter.toUpperCase())
  );
}

function profileUrlForUser(user) {
  if (!user) {
    return "";
  }

  const domain = user.customDomainState?.live?.domain;
  if (domain) {
    return normalizeMediumUrl(`https://${domain}`);
  }

  return user.username ? normalizeMediumUrl(`https://medium.com/@${user.username}`) : "";
}

function looksLikeArticlePathUrl(value) {
  const cleaned = cleanText(value);

  if (!cleaned || /\/m\/signin|bookmark|vote|respond/i.test(cleaned)) {
    return false;
  }

  try {
    const parsed = new URL(cleaned, "https://medium.com");
    return /[a-f0-9]{12}\/?$/i.test(parsed.pathname);
  } catch (_error) {
    return false;
  }
}

function parseCompactInteger(value) {
  const cleaned = cleanText(value).replace(/,/g, "").replace(/\s+/g, "").toUpperCase();
  const match = cleaned.match(/^([0-9]+(?:\.[0-9]+)?)([KM])?$/);

  if (!match) {
    return null;
  }

  const multiplier = match[2] === "K" ? 1000 : match[2] === "M" ? 1000000 : 1;
  return Math.round(Number(match[1]) * multiplier);
}

function parseRecommendation(rawUrl, fallbackPosition, fallbackTagSlug) {
  let source = "";

  try {
    source = new URL(rawUrl, "https://medium.com").searchParams.get("source") || "";
  } catch (_error) {
    source = "";
  }

  const match = source.match(/^(.+?)------([a-z0-9_-]+)---([0-9]+)-([0-9]+)/i);

  return {
    recommendation_source: source,
    recommendation_surface: match ? cleanText(match[1]) : "tag_recommended_stories_page",
    recommendation_tag_slug: match ? cleanText(match[2]) : fallbackTagSlug,
    recommendation_position: match ? Number(match[3]) : fallbackPosition - 1,
    recommendation_result_set_size: match ? Number(match[4]) : null,
  };
}

function dateStringFromParts(year, monthIndex, day) {
  return [
    year,
    String(monthIndex + 1).padStart(2, "0"),
    String(day).padStart(2, "0"),
  ].join("-");
}

function inferPublishedFromLabel(label, capturedDate) {
  const text = cleanText(label);

  if (!text || !(capturedDate instanceof Date) || Number.isNaN(capturedDate.getTime())) {
    return { date: "", timestamp: "", precision: "", from: "" };
  }

  const lower = text.toLowerCase();
  const capturedIso = capturedDate.toISOString().replace(/\.\d{3}Z$/, "Z");

  if (/^(just now|today)$/.test(lower)) {
    return {
      date: capturedIso.slice(0, 10),
      timestamp: capturedIso,
      precision: lower === "just now" ? "instant_approx" : "day",
      from: text,
    };
  }

  const relativeMatch = lower.match(/^(\d+)\s*(m|min|mins|minute|minutes|h|hr|hrs|hour|hours)\s+ago$/);
  if (relativeMatch) {
    const amount = Number(relativeMatch[1]);
    const isMinute = /^(m|min|mins|minute|minutes)$/.test(relativeMatch[2]);
    const elapsedMs = amount * (isMinute ? 60 * 1000 : 60 * 60 * 1000);
    const inferred = new Date(capturedDate.getTime() - elapsedMs);
    const inferredIso = inferred.toISOString().replace(/\.\d{3}Z$/, "Z");

    return {
      date: inferredIso.slice(0, 10),
      timestamp: inferredIso,
      precision: isMinute ? "minute_approx" : "hour_approx",
      from: text,
    };
  }

  const monthDayMatch = text.match(/^([A-Za-z]{3,9})\.?\s+(\d{1,2})(?:,\s*(\d{4}))?$/);
  if (monthDayMatch) {
    const monthIndex = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]
      .indexOf(monthDayMatch[1].slice(0, 3).toLowerCase());
    const day = Number(monthDayMatch[2]);

    if (monthIndex >= 0 && day >= 1 && day <= 31) {
      let year = monthDayMatch[3] ? Number(monthDayMatch[3]) : capturedDate.getFullYear();
      const candidateDay = Date.UTC(year, monthIndex, day);
      const captureDay = Date.UTC(capturedDate.getFullYear(), capturedDate.getMonth(), capturedDate.getDate());

      if (!monthDayMatch[3] && candidateDay > captureDay + 24 * 60 * 60 * 1000) {
        year -= 1;
      }

      return {
        date: dateStringFromParts(year, monthIndex, day),
        timestamp: "",
        precision: "",
        from: text,
      };
    }
  }

  return { date: "", timestamp: "", precision: "", from: "" };
}

function firstHeading(block, level) {
  const match = block.match(new RegExp(`heading "([^"]+)" \\[level=${level}\\]`));
  return match ? cleanText(match[1]) : "";
}

function extractUrlAfter(text, markerRegex) {
  const markerMatch = markerRegex.exec(text);

  if (!markerMatch) {
    return "";
  }

  const afterMarker = text.slice(markerMatch.index);
  const urlMatch = afterMarker.match(/- \/url: ([^\n]+)/);
  return urlMatch ? normalizeMediumUrl(urlMatch[1]) : "";
}

function extractAuthorAndPublication(block, title, subtitle) {
  const titleIndex = block.indexOf(`heading "${title}"`);
  const beforeTitle = titleIndex >= 0 ? block.slice(0, titleIndex) : block;
  const inByMatch = beforeTitle.match(/paragraph: In[\s\S]*?tooltip "([^"]+)"[\s\S]*?paragraph: by[\s\S]*?tooltip "([^"]+)"/);

  if (inByMatch) {
    return {
      publication_name: cleanText(inByMatch[1]),
      publication_url: extractUrlAfter(beforeTitle, /paragraph: In/),
      author_name: cleanText(inByMatch[2]),
      author_url: extractUrlAfter(beforeTitle, /paragraph: by/),
      publication_status: "publication",
    };
  }

  const tooltipNames = [...beforeTitle.matchAll(/tooltip "([^"]+)"/g)]
    .map((match) => cleanText(match[1]))
    .filter((value) => {
      if (!value) return false;
      if (value === title || value === subtitle) return false;
      if (/Medium Logo|user options menu|Member-only story|A clap icon|A response icon|Add to list|not interested/i.test(value)) return false;
      return true;
    });

  const authorName = tooltipNames[0] || "";

  return {
    publication_name: "",
    publication_url: "",
    author_name: authorName,
    author_url: authorName ? extractUrlAfter(beforeTitle, new RegExp(`tooltip "${escapeRegExp(authorName)}"`)) : "",
    publication_status: authorName ? "self_published_assumed" : "unknown",
  };
}

function escapeRegExp(value) {
  return String(value).replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function extractPublishedLabel(block) {
  const match = block.match(
    /(?:generic|text|paragraph):\s*(Just now|Today|\d+\s*(?:m|min|mins|minute|minutes|h|hr|hrs|hour|hours)\s+ago|(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\.?\s+\d{1,2}(?:,\s*\d{4})?)/i
  );
  return match ? cleanText(match[1]) : "";
}

function extractReadTimeMinutes(block) {
  // Medium tag pages can expose read-time-looking text inside previews that
  // does not match the actual article page. Treat tag-page read time as absent.
  return null;
}

function parseMediumTagSnapshot(snapshot, pageUrl, pageTitle = "", options = {}) {
  const capturedDate = options.capturedDate instanceof Date ? options.capturedDate : new Date();
  const parsedPageUrl = new URL(pageUrl, "https://medium.com");
  const tagMatch = parsedPageUrl.pathname.match(/^\/tag\/([^/]+)(?:\/([^/]+))?/i);
  const tagSlug = tagMatch ? decodeURIComponent(tagMatch[1]) : "";
  const pageVariant = tagMatch && tagMatch[2] === "recommended" ? "tag_recommended" : "tag_landing";
  const blocks = String(snapshot || "").split(/\n- article:\n/).slice(1);
  const seenUrls = new Set();
  const cards = [];

  for (const block of blocks) {
    const rawUrlMatch = [...block.matchAll(/- \/url: ([^\n]+)/g)]
      .find((match) => looksLikeArticlePathUrl(match[1]));

    if (!rawUrlMatch) {
      continue;
    }

    const rawUrl = cleanText(rawUrlMatch[1]);
    const articleUrl = normalizeMediumUrl(rawUrl);

    if (!articleUrl || seenUrls.has(articleUrl)) {
      continue;
    }

    const title = firstHeading(block, 2);
    const subtitle = firstHeading(block, 3);

    if (!title) {
      continue;
    }

    const clapRaw = (block.match(/A clap icon\s*([0-9][0-9,.]*\s*[KkMm]?)/) || [null, null])[1];
    const responseRaw = (block.match(/A response icon\s*([0-9][0-9,.]*\s*[KkMm]?)/) || [null, null])[1];
    const claps = clapRaw ? parseCompactInteger(clapRaw) : 0;
    const responses = responseRaw ? parseCompactInteger(responseRaw) : 0;
    const publishedLabel = extractPublishedLabel(block);
    const inferred = inferPublishedFromLabel(publishedLabel, capturedDate);
    const readTimeMinutes = extractReadTimeMinutes(block);
    const authorPublication = extractAuthorAndPublication(block, title, subtitle);
    const recommendation = parseRecommendation(rawUrl, cards.length + 1, tagSlug);

    cards.push({
      position: cards.length + 1,
      section: pageVariant === "tag_recommended" ? "Recommended stories" : "",
      article_url: articleUrl,
      medium_post_id: extractMediumPostId(articleUrl),
      title,
      subtitle,
      author_name: authorPublication.author_name,
      author_url: authorPublication.author_url,
      author_username: "",
      author_medium_user_id: "",
      publication_name: authorPublication.publication_name,
      publication_url: authorPublication.publication_url,
      publication_id: "",
      publication_slug: "",
      publication_domain: "",
      publication_subscriber_count: null,
      publication_status: authorPublication.publication_status,
      published_label: publishedLabel,
      published_at: "",
      published_date_inferred: inferred.date,
      published_date_inferred_from: inferred.from,
      published_at_inferred: inferred.timestamp,
      published_at_inferred_precision: inferred.precision,
      updated_at: "",
      read_time_minutes: readTimeMinutes,
      article_tags: [],
      claps: Number.isFinite(claps) ? claps : null,
      responses: Number.isFinite(responses) ? responses : null,
      is_member_only: /Member-only story/i.test(block),
      thumbnail_url: "",
      thumbnail_alt: "",
      ...recommendation,
    });

    seenUrls.add(articleUrl);
  }

  return {
    source_type: sourceType,
    schema_version: schemaVersion,
    tag_slug: tagSlug,
    page_variant: pageVariant,
    tag_url: normalizeMediumUrl(pageUrl),
    source_url: normalizeMediumUrl(pageUrl),
    captured_at: capturedDate.toISOString().replace(/\.\d{3}Z$/, "Z"),
    page_title: cleanText(pageTitle),
    cards,
  };
}

function searchTermFromUrl(pageUrl, fallback = "") {
  try {
    const parsed = new URL(pageUrl, "https://medium.com");
    return cleanText(parsed.searchParams.get("q") || fallback).toLowerCase();
  } catch (_error) {
    return cleanText(fallback).toLowerCase();
  }
}

function detectMediumSearchPageType(pageUrl) {
  try {
    const parsed = new URL(pageUrl, "https://medium.com");
    if (
      /(^|\.)medium\.com$/i.test(parsed.hostname) &&
      /^\/search\/tags\/?$/i.test(parsed.pathname) &&
      parsed.searchParams.get("q")
    ) {
      return "search_tags";
    }
  } catch (_error) {
    return "";
  }

  return "";
}

function refId(item) {
  return item && typeof item === "object" ? item.__ref || "" : "";
}

function deref(state, item) {
  const key = refId(item);
  return key ? state[key] : null;
}

function findSearchSection(searchObject, prefix, preferredLimit = "") {
  if (!searchObject || typeof searchObject !== "object") {
    return null;
  }

  const entries = Object.entries(searchObject)
    .filter(([key, value]) => key.startsWith(prefix) && value && Array.isArray(value.items));

  if (entries.length === 0) {
    return null;
  }

  const preferred = entries.find(([key]) => preferredLimit && key.includes(`limit:${preferredLimit}`));
  return (preferred || entries.sort((left, right) => right[1].items.length - left[1].items.length)[0])[1];
}

function findSearchSections(searchObject, prefix) {
  if (!searchObject || typeof searchObject !== "object") {
    return [];
  }

  return Object.entries(searchObject)
    .filter(([key, value]) => key.startsWith(prefix) && value && Array.isArray(value.items))
    .sort(([leftKey], [rightKey]) => {
      const leftIsMain = /web-main-content/i.test(leftKey) ? 0 : 1;
      const rightIsMain = /web-main-content/i.test(rightKey) ? 0 : 1;
      return leftIsMain - rightIsMain || leftKey.localeCompare(rightKey);
    });
}

function tagRowsFromSearchSectionItems(state, items, searchTerm, pageUrl, seenTags = new Set(), startRank = 1) {
  const rows = [];

  for (const item of items || []) {
    const tag = deref(state, item);
    const tagSlug = cleanText(tag?.normalizedTagSlug || tag?.id || "").toLowerCase();
    if (!tagSlug || seenTags.has(tagSlug)) {
      continue;
    }
    seenTags.add(tagSlug);
    rows.push({
      search_term: searchTerm,
      tag_slug: tagSlug,
      display_title: cleanText(tag?.displayTitle || tagSlug),
      result_rank: startRank + rows.length,
      tag_url: normalizeMediumUrl(`/tag/${encodeURIComponent(tagSlug)}`),
      source_url: normalizeSourceUrl(pageUrl),
    });
  }

  return rows;
}

function parseSearchTagsApolloState(state, pageUrl, pageTitle = "", options = {}) {
  const capturedDate = options.capturedDate instanceof Date ? options.capturedDate : new Date();
  const searchTerm = searchTermFromUrl(pageUrl, options.searchTerm || "");
  const searchObject = Object.values(state || {}).find((value) => value && value.__typename === "Search") || {};
  const tagSections = findSearchSections(searchObject, `tags-${searchTerm}`);
  const postSection = findSearchSection(searchObject, `posts-${searchTerm}`, "3");
  const peopleSection = findSearchSection(searchObject, `people-${searchTerm}`, "3");
  const mainTagSections = tagSections.filter(([key]) => /web-main-content/i.test(key));
  const sectionsForTags = mainTagSections.length > 0 ? mainTagSections : tagSections;
  const seenTagSlugs = new Set();
  const tags = [];

  for (const [_key, section] of sectionsForTags) {
    tags.push(...tagRowsFromSearchSectionItems(state, section.items, searchTerm, pageUrl, seenTagSlugs, tags.length + 1));
  }

  const sidebarPosts = (postSection?.items || [])
    .map((item, index) => {
      const post = deref(state, item);
      if (!post) {
        return null;
      }
      const creator = deref(state, post.creator);
      const postUrl = normalizeMediumUrl(post.mediumUrl || "");
      const postId = cleanText(post.id || extractMediumPostId(postUrl)).toLowerCase();
      if (!postUrl && !postId) {
        return null;
      }
      return {
        search_term: searchTerm,
        source_surface: "search_sidebar_posts",
        result_rank: index + 1,
        article_url: postUrl,
        medium_post_id: postId,
        title: cleanText(post.title || titleFromMediumUrl(postUrl)),
        author_name: cleanText(creator?.name || ""),
        author_url: profileUrlForUser(creator),
        author_username: cleanText(creator?.username || ""),
        is_member_only: post.isLocked === true,
        published_at: post.firstPublishedAt ? new Date(Number(post.firstPublishedAt)).toISOString().replace(/\.\d{3}Z$/, "Z") : "",
        updated_at: post.latestPublishedAt ? new Date(Number(post.latestPublishedAt)).toISOString().replace(/\.\d{3}Z$/, "Z") : "",
      };
    })
    .filter(Boolean);

  const sidebarPeople = (peopleSection?.items || [])
    .map((item, index) => {
      const user = deref(state, item);
      if (!user) {
        return null;
      }
      const profileUrl = profileUrlForUser(user);
      if (!profileUrl && !user.username) {
        return null;
      }
      return {
        search_term: searchTerm,
        source_surface: "search_sidebar_people",
        result_rank: index + 1,
        profile_url: profileUrl,
        username: cleanText(user.username || ""),
        display_name: cleanText(user.name || ""),
        bio_snippet: cleanText(user.bio || ""),
      };
    })
    .filter(Boolean);

  return {
    source_type: searchTagsSourceType,
    schema_version: searchTagsSchemaVersion,
    page_type: "search_tags",
    search_term: searchTerm,
    source_url: normalizeSourceUrl(pageUrl),
    captured_at: capturedDate.toISOString().replace(/\.\d{3}Z$/, "Z"),
    page_title: cleanText(pageTitle),
    tracking_context: normalizeTrackingContext(options.trackingContext),
    tags,
    sidebar_posts: sidebarPosts,
    sidebar_people: sidebarPeople,
  };
}

function extractApolloStateFromHtml(html) {
  const match = String(html || "").match(/window\.__APOLLO_STATE__\s*=\s*({[\s\S]*?})<\/script>/);

  if (!match) {
    return null;
  }

  return JSON.parse(match[1]);
}

function parseSearchTagsDomFallbackFromHtml(html, pageUrl, pageTitle = "", options = {}) {
  const capturedDate = options.capturedDate instanceof Date ? options.capturedDate : new Date();
  const searchTerm = searchTermFromUrl(pageUrl, options.searchTerm || "");
  const seenTags = new Set();
  const tags = [];
  const tagLinkRegex = /<a\b[^>]*href=["']([^"']*\/tag\/([^?"']+)[^"']*)["'][^>]*>([\s\S]*?)<\/a>/gi;
  let match;

  while ((match = tagLinkRegex.exec(String(html || ""))) !== null) {
    const href = normalizeMediumUrl(match[1]);
    const slug = cleanText(decodeURIComponent(match[2] || "")).toLowerCase();
    const title = cleanText(match[3].replace(/<[^>]*>/g, " "));
    if (!slug || !title || seenTags.has(slug)) {
      continue;
    }
    seenTags.add(slug);
    tags.push({
      search_term: searchTerm,
      tag_slug: slug,
      display_title: title,
      result_rank: tags.length + 1,
      tag_url: href,
      source_url: normalizeSourceUrl(pageUrl),
    });
  }

  return {
    source_type: searchTagsSourceType,
    schema_version: searchTagsSchemaVersion,
    page_type: "search_tags",
    search_term: searchTerm,
    source_url: normalizeSourceUrl(pageUrl),
    captured_at: capturedDate.toISOString().replace(/\.\d{3}Z$/, "Z"),
    page_title: cleanText(pageTitle),
    tracking_context: normalizeTrackingContext(options.trackingContext),
    tags,
    sidebar_posts: [],
    sidebar_people: [],
  };
}

function mergeDomVisibleSearchTags(payload, html, options = {}) {
  const domPayload = parseSearchTagsDomFallbackFromHtml(html, payload.source_url, payload.page_title, {
    ...options,
    capturedDate: payload.captured_at ? new Date(payload.captured_at) : options.capturedDate,
    searchTerm: payload.search_term,
    trackingContext: payload.tracking_context,
  });

  if (domPayload.tags.length <= (payload.tags || []).length) {
    return payload;
  }

  return {
    ...payload,
    tags: domPayload.tags,
  };
}

function parseMediumSearchTagsHtml(html, pageUrl = "https://medium.com/search/tags?q=finance", pageTitle = "", options = {}) {
  const state = extractApolloStateFromHtml(html);
  if (state) {
    return mergeDomVisibleSearchTags(
      parseSearchTagsApolloState(state, pageUrl, pageTitle, options),
      html,
      options
    );
  }

  return parseSearchTagsDomFallbackFromHtml(html, pageUrl, pageTitle, options);
}

function signatureForSearchTagsPayload(payload) {
  if (!payload) {
    return "";
  }

  return [
    ...(payload.tags || []).map((tag) => `tag|${tag.search_term}|${tag.tag_slug}|${tag.result_rank}`),
    ...(payload.sidebar_posts || []).map((post) => `post|${post.search_term}|${post.medium_post_id}|${post.article_url}|${post.result_rank}`),
    ...(payload.sidebar_people || []).map((person) => `person|${person.search_term}|${person.username}|${person.profile_url}|${person.result_rank}`),
  ].join("\n");
}

function signatureForPayload(payload) {
  if (!payload || !Array.isArray(payload.cards)) {
    return "";
  }

  return payload.cards
    .map((card) => [
      card.article_url || "",
      card.medium_post_id || "",
      Number.isFinite(card.claps) ? card.claps : "",
      Number.isFinite(card.responses) ? card.responses : "",
      cleanText(card.published_label || card.published_at || card.published_date_inferred),
    ].join("|"))
    .join("\n");
}

function urlSet(payload) {
  return new Set((payload.cards || []).map((card) => card.article_url).filter(Boolean));
}

function safeFilenamePart(value) {
  return cleanText(value).replace(/[^A-Za-z0-9_-]+/g, "-").replace(/^-+|-+$/g, "").toLowerCase() || "medium-tag";
}

function timestampForFilename(date = new Date()) {
  const pad = (value) => String(value).padStart(2, "0");
  const millis = String(date.getMilliseconds()).padStart(3, "0");
  return [
    date.getFullYear(),
    pad(date.getMonth() + 1),
    pad(date.getDate()),
  ].join("-") + "_" + pad(date.getHours()) + pad(date.getMinutes()) + pad(date.getSeconds()) + "-" + millis;
}

function writeSnapshot(payload, snapshotDir = defaultSnapshotDir) {
  fs.mkdirSync(snapshotDir, { recursive: true });
  const filename = payload.source_type === searchTagsSourceType
    ? [
      "medium_search_tags_watch",
      safeFilenamePart(payload.search_term),
      timestampForFilename(),
    ].join("_") + ".json"
    : [
      "medium_tag_watch",
      safeFilenamePart(payload.tag_slug),
      safeFilenamePart(payload.page_variant),
      timestampForFilename(),
    ].join("_") + ".json";
  const outputPath = path.join(snapshotDir, filename);
  fs.writeFileSync(outputPath, JSON.stringify(payload, null, 2));
  return outputPath;
}

function writeArticleTextSnapshot(payload, snapshotDir = defaultArticleTextSnapshotDir) {
  fs.mkdirSync(snapshotDir, { recursive: true });
  const filename = [
    "medium_article_text",
    safeFilenamePart(payload.medium_post_id || payload.title || "article"),
    timestampForFilename(),
  ].join("_") + ".json";
  const outputPath = path.join(snapshotDir, filename);
  fs.writeFileSync(outputPath, JSON.stringify(payload, null, 2));
  return outputPath;
}

function importSnapshot(snapshotPath, cwd = process.cwd()) {
  const result = spawnSync(
    "Rscript",
    [path.join("scripts", "import_medium_tag_page_bookmarklet.R"), snapshotPath],
    {
      cwd,
      encoding: "utf8",
      stdio: "pipe",
    }
  );

  return {
    status: result.status,
    stdout: result.stdout || "",
    stderr: result.stderr || "",
  };
}

function importArticleTextSnapshot(snapshotPath, cwd = process.cwd()) {
  const result = spawnSync(
    "Rscript",
    [path.join("scripts", "import_medium_article_text_snapshot.R"), snapshotPath],
    {
      cwd,
      encoding: "utf8",
      stdio: "pipe",
    }
  );

  return {
    status: result.status,
    stdout: result.stdout || "",
    stderr: result.stderr || "",
  };
}

function importSearchTagsSnapshot(snapshotPath, cwd = process.cwd()) {
  const result = spawnSync(
    "Rscript",
    [path.join("scripts", "import_medium_search_tags_snapshot.R"), snapshotPath],
    {
      cwd,
      encoding: "utf8",
      stdio: "pipe",
    }
  );

  return {
    status: result.status,
    stdout: result.stdout || "",
    stderr: result.stderr || "",
  };
}

export {
  cleanText,
  defaultArticleTextSnapshotDir,
  defaultSnapshotDir,
  detectMediumSearchPageType,
  extractMediumPostId,
  extractReadTimeMinutes,
  importArticleTextSnapshot,
  importSearchTagsSnapshot,
  importSnapshot,
  inferPublishedFromLabel,
  normalizeMediumUrl,
  parseCompactInteger,
  parseMediumTagSnapshot,
  parseMediumSearchTagsHtml,
  parseSearchTagsApolloState,
  searchTagsSourceType,
  signatureForSearchTagsPayload,
  signatureForPayload,
  urlSet,
  writeArticleTextSnapshot,
  writeSnapshot,
};
