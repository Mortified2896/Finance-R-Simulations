# Historical: Medium Article Clap Capture Test

Date checked: 2026-05-15

## Scope

This was a small Playwright smoke test for direct Medium article pages. It does not bypass paywalls, automate login, solve CAPTCHA, use proxies, or scrape full article text.

Test URL:

https://medium.com/the-investors-handbook/war-in-the-strait-does-nuclear-become-the-new-safe-haven-a10195785fe9

Expected manual browser value: 50 claps

## What Was Added

Script:

`scripts/test_medium_article_clap_capture.js`

Default artifacts:

`debug_samples/article_clap_capture/`

The script supports:

- one or more direct URL arguments
- `--url-file`
- `--sample-db <n>`
- visible Chromium by default
- optional persistent profile via `--use-default-profile` or `--user-data-dir`
- optional DB writes behind `--write-db`
- screenshot, action-bar text, action-bar HTML, and JSONL result output

## Result

The first visible Playwright run reached the Medium article page and the screenshot clearly showed `50` beside the clap icon. The page also showed a member-only/login prompt lower down, which is expected for this article.

However, that run hit the script's manual-pause path in a non-interactive terminal before extraction completed, so the recorded JSONL row was `error` with `readline was closed`.

A follow-up run without the manual pause was blocked by Medium and returned:

- HTTP status: 403
- page title: `Just a moment...`
- extraction status: `http_error`

Because Medium started serving the browser check/403, I stopped testing rather than trying to defeat bot detection.

## Debug Artifacts

Observed screenshot:

`debug_samples/article_clap_capture/a10195785fe9_screenshot.png`

Latest JSONL report:

`debug_samples/article_clap_capture/results.jsonl`

The screenshot from the first run is the strongest evidence from this test: visible browser automation can reach a rendered page state where the clap amount is visible as `50`. The completed extraction result is not yet confirmed because the successful page-load run ended before extraction and subsequent runs were blocked.

## Raw/Server Access

This test did not add a new raw HTTP comparison. The motivation still appears valid: the rendered browser can show a clap value where non-browser/raw access may show missing or placeholder values such as `--`.

## Reliability Assessment

Not reliable enough to add to the tracker yet.

Reasons:

- One visible run showed the target value in the screenshot.
- No completed extraction run has yet produced `50` in `results.jsonl`.
- Medium returned a bot-check/403 page on the next run.
- Any production workflow would need to be manually paced and explicitly human-in-the-loop, not an unattended crawler.

## Recommended Next Step

Keep the script as an auditable manual smoke-test tool. Next, run it only when a normal browser session is already open and stable, preferably by attaching to an existing Chrome CDP session or by using the persistent profile after manual login/check completion. Confirm extraction on 3 to 5 manually supplied URLs before considering a DB-backed observation table.
