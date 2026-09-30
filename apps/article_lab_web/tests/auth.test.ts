import { beforeAll, afterAll, it, expect } from "vitest";
import { Miniflare, convertV4MiniflareOptions } from "miniflare";
import { readFile } from "node:fs/promises";
import { serializeSignedCookie } from "better-call";
import { getMigrations } from "better-auth/db/migration";
import { createAuth } from "../worker/auth";
import { createApp, type Env } from "../worker/index";
let mf: Miniflare,
  env: Env,
  auth: ReturnType<typeof createAuth>,
  cookie: string,
  userId: string;
const app = createApp(),
  origin = "https://feedback.moneymattersmedia.com";
async function request(
  path: string,
  method = "GET",
  data?: unknown,
  session = cookie,
  requestOrigin = origin,
) {
  return app.fetch(
    new Request(origin + path, {
      method,
      headers: {
        Cookie: session ?? "",
        Origin: requestOrigin,
        "Content-Type": "application/json",
      },
      body: data === undefined ? undefined : JSON.stringify(data),
    }),
    env,
  );
}
beforeAll(async () => {
  mf = new Miniflare(
    convertV4MiniflareOptions({
      modules: true,
      script: "export default {fetch(){return new Response()}}",
      d1Databases: ["DB"],
    }),
  );
  env = {
    DB: (await mf.getD1Database("DB")) as unknown as D1Database,
    ASSETS: {} as Fetcher,
    BETTER_AUTH_URL: origin,
    BETTER_AUTH_SECRET: crypto.randomUUID() + crypto.randomUUID(),
    GOOGLE_CLIENT_ID: "test-client.apps.googleusercontent.com",
    GOOGLE_CLIENT_SECRET: "test-only",
  };
  for (const file of ["0001_review.sql", "0002_better_auth.sql"])
    await env.DB.exec(
      (await readFile("migrations/" + file, "utf8")).replace(/\n/g, " "),
    );
  auth = createAuth(env);
  const ctx = await auth.$context;
  const user = await ctx.internalAdapter.createUser(
    { name: "Jane", email: "jane@example.test", emailVerified: true },
    { method: "oauth", oauth: { providerId: "google" } },
  );
  userId = user.id;
  const session = await ctx.internalAdapter.createSession(user.id);
  cookie = (
    await serializeSignedCookie(
      ctx.authCookies.sessionToken.name,
      session.token,
      env.BETTER_AUTH_SECRET!,
      ctx.authCookies.sessionToken.attributes,
    )
  ).split(";")[0];
});
afterAll(async () => {
  await mf.dispose();
});
it("source-controlled schema matches the installed Better Auth D1 configuration", async () => {
  const migrations = await getMigrations(auth.options);
  expect(migrations.toBeCreated).toHaveLength(0);
  expect(migrations.toBeAdded).toHaveLength(0);
});
it("Google sign-in emits correct OAuth callback, state and secure cookie", async () => {
  const r = await request(
    "/api/auth/sign-in/social",
    "POST",
    { provider: "google", callbackURL: "/", disableRedirect: true },
    "",
  );
  expect(r.status).toBe(200);
  const result = (await r.json()) as { url: string };
  const url = new URL(result.url);
  expect(url.hostname).toBe("accounts.google.com");
  expect(url.searchParams.get("redirect_uri")).toBe(
    origin + "/api/auth/callback/google",
  );
  expect(url.searchParams.get("state")).toBeTruthy();
  const cookies = r.headers.get("set-cookie")!;
  expect(cookies).toContain("HttpOnly");
  expect(cookies).toContain("Secure");
  expect(cookies).toContain("SameSite=Lax");
});
it("rejects untrusted callback origins and forged OAuth state", async () => {
  const r = await request(
    "/api/auth/sign-in/social",
    "POST",
    { provider: "google", callbackURL: "https://evil.test/" },
    "",
  );
  expect(r.status).toBe(403);
  const callback = await request(
    "/api/auth/callback/google?code=forged&state=forged",
    "GET",
    undefined,
    "",
  );
  expect([302, 400, 401]).toContain(callback.status);
  expect(
    await env.DB.prepare("SELECT COUNT(*) n FROM auth_session").first("n"),
  ).toBe(1);
});
it("verified Better Auth session creates pending profile with stable auth ID", async () => {
  const r = await request("/api/me");
  expect(r.status).toBe(200);
  const result = (await r.json()) as any;
  expect(result.user.auth_user_id).toBe(userId);
  expect(result.user.status).toBe("pending");
  expect((await request("/api/assignments")).status).toBe(403);
});
it("approval survives a new session and admin APIs remain forbidden", async () => {
  await env.DB.prepare(
    "UPDATE users SET status='approved' WHERE auth_user_id=?",
  )
    .bind(userId)
    .run();
  const ctx = await auth.$context,
    session = await ctx.internalAdapter.createSession(userId);
  const newCookie = (
    await serializeSignedCookie(
      ctx.authCookies.sessionToken.name,
      session.token,
      env.BETTER_AUTH_SECRET!,
      ctx.authCookies.sessionToken.attributes,
    )
  ).split(";")[0];
  const r = await request("/api/me", "GET", undefined, newCookie);
  expect(((await r.json()) as any).user.status).toBe("approved");
  expect(
    (await request("/api/admin/users", "GET", undefined, newCookie)).status,
  ).toBe(403);
});
it("a verified email change retains the stable profile and approval", async () => {
  const before = (await (await request("/api/me")).json()) as any;
  await env.DB.prepare("UPDATE auth_user SET email=? WHERE id=?")
    .bind("jane-new@example.test", userId)
    .run();
  const after = (await (await request("/api/me")).json()) as any;
  expect(after.user.id).toBe(before.user.id);
  expect(after.user.status).toBe("approved");
  expect(after.user.email).toBe("jane-new@example.test");
});
it("logout revokes the session; forged and expired cookies cannot authenticate", async () => {
  const r = await request("/api/auth/sign-out", "POST", {});
  expect(r.status).toBe(200);
  expect((await request("/api/me")).status).toBe(401);
  expect(
    (
      await request(
        "/api/me",
        "GET",
        undefined,
        "__Secure-better-auth.session_token=forged",
      )
    ).status,
  ).toBe(401);
  const ctx = await auth.$context,
    session = await ctx.internalAdapter.createSession(userId);
  await env.DB.prepare("UPDATE auth_session SET expiresAt=? WHERE id=?")
    .bind(Date.now() - 1000, session.id)
    .run();
  const expired = (
    await serializeSignedCookie(
      ctx.authCookies.sessionToken.name,
      session.token,
      env.BETTER_AUTH_SECRET!,
      ctx.authCookies.sessionToken.attributes,
    )
  ).split(";")[0];
  expect((await request("/api/me", "GET", undefined, expired)).status).toBe(
    401,
  );
});
it("does not expose password or email OTP sign-in", async () => {
  expect(
    (
      await request(
        "/api/auth/sign-in/email",
        "POST",
        { email: "a@b.com", password: "test" },
        "",
      )
    ).status,
  ).not.toBe(200);
  expect(
    (await request("/api/auth/sign-in/email-otp", "POST", {}, "")).status,
  ).toBe(404);
});
