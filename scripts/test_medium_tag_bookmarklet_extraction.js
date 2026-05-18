import fs from "node:fs";

const html = fs.readFileSync("01_manual_tools/import/medium_tag_page_bookmarklet.html", "utf8");
const start = "const bookmarkletSource = `";
const end = "`;\n\n    const bookmarkletCode";
const startIndex = html.indexOf(start);
const endIndex = html.indexOf(end, startIndex + start.length);

if (startIndex < 0 || endIndex < 0) {
  throw new Error("Could not find bookmarklet source in helper HTML.");
}

const rawTemplate = html.slice(startIndex + start.length, endIndex);
const bookmarkletSource = Function(
  "return `" + rawTemplate.replace(/`/g, "\\`") + "`;"
)();
const bodyMatch = bookmarkletSource.match(/^\(\(\) => \{\n([\s\S]*)\n\}\)\(\);$/);

if (!bodyMatch) {
  throw new Error("Bookmarklet source did not match the expected IIFE shape.");
}

const body = bodyMatch[1];
const pageInfoIndex = body.lastIndexOf("  const pageInfo = parseTagPageInfo();");

if (pageInfoIndex < 0) {
  throw new Error("Could not find bookmarklet execution section.");
}

const functionSection = body.slice(0, pageInfoIndex);
const sampleUrl =
  "https://medium.com/@quant-author/test-story-abcdef123456?source=tag_recommended_stories_page------quantitative-finance---4-107--------------------token--------------#ignored";

const testSource = `
globalThis.window = {
  location: {
    href: "https://medium.com/tag/quantitative-finance/recommended",
    pathname: "/tag/quantitative-finance/recommended"
  },
  getComputedStyle: () => ({ display: "block", visibility: "visible" })
};
${functionSection}

const recommendation = parseRecommendationSource(${JSON.stringify(sampleUrl)});
if (recommendation.recommendation_surface !== "tag_recommended_stories_page") {
  throw new Error("recommendation_surface was not parsed");
}
if (recommendation.recommendation_tag_slug !== "quantitative-finance") {
  throw new Error("recommendation_tag_slug was not parsed");
}
if (recommendation.recommendation_position !== 4) {
  throw new Error("recommendation_position was not parsed");
}
if (recommendation.recommendation_result_set_size !== 107) {
  throw new Error("recommendation_result_set_size was not parsed");
}

const normalizedUrl = normalizeMediumUrl(${JSON.stringify(sampleUrl)});
if (normalizedUrl.includes("?") || normalizedUrl.includes("#")) {
  throw new Error("normalizeMediumUrl should strip query and hash");
}

const fakePreviewRoot = {
  matches: () => false,
  querySelector: () => null,
  querySelectorAll: (selector) => {
    if (selector === '[role="link"][data-href], [role="link"]') {
      return [{
        getAttribute: (name) => name === "data-href" ? ${JSON.stringify(sampleUrl)} : null,
        href: ""
      }];
    }
    return [];
  }
};

const primaryUrl = findPrimaryArticleUrlFromPreview(fakePreviewRoot);
if (primaryUrl !== ${JSON.stringify(sampleUrl)}) {
  throw new Error("findPrimaryArticleUrlFromPreview should preserve the raw source query");
}

const fallbackRecommendation = withRecommendationFallback(parseRecommendationSource("https://medium.com/@quant-author/test-story-abcdef123456"), "tag_recommended", 5);
if (fallbackRecommendation.recommendation_surface !== "tag_recommended_stories_page") {
  throw new Error("fallback recommendation_surface was not populated");
}
if (fallbackRecommendation.recommendation_tag_slug !== "quantitative-finance") {
  throw new Error("fallback recommendation_tag_slug was not populated");
}
if (fallbackRecommendation.recommendation_position !== 4) {
  throw new Error("fallback recommendation_position should be zero-based from card position");
}

class FakeHTMLElement {}
globalThis.HTMLElement = FakeHTMLElement;
globalThis.Element = FakeHTMLElement;
const fakeControlRoot = {
  querySelectorAll: () => [
    Object.assign(new FakeHTMLElement(), {
      getAttribute: (name) => name === "aria-label" ? "Clap" : null,
      getBoundingClientRect: () => ({ width: 16, height: 16 }),
      innerText: "",
      textContent: ""
    }),
    Object.assign(new FakeHTMLElement(), {
      getAttribute: (name) => name === "aria-label" ? "Response" : null,
      getBoundingClientRect: () => ({ width: 16, height: 16 }),
      innerText: "",
      textContent: ""
    })
  ]
};
if (!hasEngagementControl(fakeControlRoot, "(?:clap|applause|recommend)")) {
  throw new Error("clap control without count should be detected");
}
if (!hasEngagementControl(fakeControlRoot, "(?:response|comment)")) {
  throw new Error("response control without count should be detected");
}
const fakeFooterOnlyRoot = {
  innerText: "Author Title Subtitle 6h ago Member-only story",
  textContent: "Author Title Subtitle 6h ago Member-only story",
  querySelectorAll: () => []
};
if (inferEngagementCount(fakeFooterOnlyRoot, null, null, "(?:clap|applause|recommend)") !== 0) {
  throw new Error("card footer without numeric claps should infer zero claps");
}
if (inferEngagementCount(fakeFooterOnlyRoot, null, null, "(?:response|comment)") !== 0) {
  throw new Error("card footer without numeric responses should infer zero responses");
}
const fakeBareCardRoot = {
  innerText: "Author Title Subtitle 6h ago",
  textContent: "Author Title Subtitle 6h ago",
  querySelectorAll: () => []
};
if (inferEngagementCount(fakeBareCardRoot, null, null, "(?:clap|applause|recommend)") !== 0) {
  throw new Error("tag card without numeric claps should infer zero claps");
}
if (inferEngagementCount(fakeBareCardRoot, null, null, "(?:response|comment)") !== 0) {
  throw new Error("tag card without numeric responses should infer zero responses");
}
const mediumIconFooter = "A clap icon 344 A response icon 20";
if (parseCompactInteger(mediumIconFooter, "(?:claps?|applause|recommendations?)") !== 344) {
  throw new Error("Medium icon footer should parse clap count");
}
if (parseCompactInteger(mediumIconFooter, "(?:responses?|comments?)") !== 20) {
  throw new Error("Medium icon footer should parse response count");
}
const fakeClapDescription = Object.assign(new FakeHTMLElement(), {
  textContent: "A clap icon",
  parentElement: null
});
const fakeClapSpan = Object.assign(new FakeHTMLElement(), {
  innerText: "344",
  textContent: "344",
  getBoundingClientRect: () => ({ width: 20, height: 16 })
});
const fakeClapSvg = Object.assign(new FakeHTMLElement(), {
  parentElement: null,
  querySelectorAll: () => []
});
const fakeClapControl = Object.assign(new FakeHTMLElement(), {
  parentElement: null,
  querySelectorAll: (selector) => selector === "span" ? [fakeClapSpan] : []
});
fakeClapDescription.parentElement = fakeClapSvg;
fakeClapSvg.parentElement = fakeClapControl;
const fakeIconRoot = {
  innerHTML: "<svg><desc>A clap icon</desc></svg><span>344</span>",
  querySelectorAll: (selector) => selector === "desc" ? [fakeClapDescription] : []
};
if (numberNearIconMarkup(fakeIconRoot, "clap") !== 344) {
  throw new Error("icon description markup should parse engagement count");
}
if (numberNearIconDescription(fakeIconRoot, "clap") !== 344) {
  throw new Error("icon description sibling spans should parse engagement count");
}

const fakeZeroBoxPreviewRoot = Object.assign(new FakeHTMLElement(), {
  getBoundingClientRect: () => ({ width: 0, height: 0 }),
  querySelectorAll: () => [
    Object.assign(new FakeHTMLElement(), {
      getBoundingClientRect: () => ({ width: 320, height: 48 })
    })
  ]
});
if (!hasVisibleCardContent(fakeZeroBoxPreviewRoot)) {
  throw new Error("post-preview wrappers with visible children should count as visible cards");
}

const inferredSixHours = inferPublishedTimestampFromLabel("6h ago", new Date("2026-05-08T18:02:30Z"));
const elapsedMs = new Date("2026-05-08T18:02:30Z").getTime() - new Date(inferredSixHours.timestamp).getTime();
if (elapsedMs !== 6 * 60 * 60 * 1000) {
  throw new Error("6h ago should infer an approximate timestamp from capture time");
}
if (inferredSixHours.precision !== "hour_approx") {
  throw new Error("6h ago should be marked with hour_approx precision");
}
const inferredMonthDay = inferPublishedDateFromLabel("Mar 26", new Date("2026-05-09T13:50:00Z"));
if (inferredMonthDay.date !== "2026-03-26" || inferredMonthDay.inferred !== true) {
  throw new Error("month-day labels should infer the capture year");
}
const inferredPreviousYear = inferPublishedDateFromLabel("Nov 22", new Date("2026-05-09T13:50:00Z"));
if (inferredPreviousYear.date !== "2025-11-22" || inferredPreviousYear.inferred !== true) {
  throw new Error("future month-day labels should infer the previous year");
}
`;

new Function(testSource)();
console.log("Medium tag bookmarklet extraction tests passed.");
