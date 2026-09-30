import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { Miniflare, convertV4MiniflareOptions } from "miniflare";
const origin = "https://feedback.moneymattersmedia.com";
const mf = new Miniflare(
  convertV4MiniflareOptions({
    modules: true,
    scriptPath: ".local/worker/index.js",
    compatibilityDate: "2026-09-29",
    compatibilityFlags: ["nodejs_compat"],
    d1Databases: ["DB"],
    serviceBindings: {
      ASSETS: async () => {
        const html = await readFile("dist/index.html", "utf8");
        return new Response(html, {
          headers: {
            "Content-Type": "text/html",
            ETag: "synthetic",
            "Content-Length": String(Buffer.byteLength(html)),
          },
        });
      },
    },
    bindings: {
      BETTER_AUTH_URL: origin,
      BETTER_AUTH_SECRET: crypto.randomUUID() + crypto.randomUUID(),
      GOOGLE_CLIENT_ID: "synthetic.apps.googleusercontent.com",
      GOOGLE_CLIENT_SECRET: "synthetic-only",
    },
  }),
);
try {
  const db = await mf.getD1Database("DB");
  for (const file of ["0001_review.sql", "0002_better_auth.sql"])
    await db.exec(
      (await readFile("migrations/" + file, "utf8")).replace(/\n/g, " "),
    );
  const r = await mf.dispatchFetch(origin + "/api/auth/sign-in/social", {
    method: "POST",
    headers: { Origin: origin, "Content-Type": "application/json" },
    body: JSON.stringify({
      provider: "google",
      callbackURL: "/",
      disableRedirect: true,
    }),
  });
  assert.equal(r.status, 200);
  const result = (await r.json()) as { url: string };
  const url = new URL(result.url);
  assert.equal(url.hostname, "accounts.google.com");
  assert.equal(
    url.searchParams.get("redirect_uri"),
    origin + "/api/auth/callback/google",
  );
  assert.match(r.headers.get("set-cookie")!, /HttpOnly/);
  assert.match(r.headers.get("set-cookie")!, /Secure/);
  assert.equal((await mf.dispatchFetch(origin + "/api/me")).status, 401);
  const html = await mf.dispatchFetch(origin + "/");
  assert.equal(html.status, 200);
  assert.equal(html.headers.get("Cache-Control"), "no-store");
  assert.equal(html.headers.get("ETag"), null);
  assert.equal(html.headers.get("Content-Length"), null);
  const nonce = (await html.text()).match(
    /name="article-style-nonce" content="([^"]+)"/,
  )?.[1];
  assert.ok(nonce);
  const policy = html.headers.get("Content-Security-Policy")!;
  assert.ok(policy.includes(`style-src-elem 'self' 'nonce-${nonce}'`));
  assert.ok(policy.includes("script-src 'self';"));
  assert.ok(
    !policy.includes("unsafe-inline") && !policy.includes("unsafe-eval"),
  );
  assert.notEqual(
    (await mf.dispatchFetch(origin + "/")).headers.get(
      "Content-Security-Policy",
    ),
    policy,
  );
  console.log(
    "Actual workerd bundle: D1-backed Google OAuth initiation, secure state cookie, anonymous denial and fresh CSS-only HTML nonce/CSP passed. No Google login simulated or claimed.",
  );
} finally {
  await mf.dispose();
}
