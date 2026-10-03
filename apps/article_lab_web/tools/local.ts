// Local-only Node harness. Never referenced by the production Worker bundle.
import { createServer } from "node:http";
import { readFile, mkdir } from "node:fs/promises";
import { resolve, extname } from "node:path";
import { Miniflare, convertV4MiniflareOptions } from "miniflare";
import { createApp } from "../worker/index";
const port = Number(process.env.PORT ?? 5173);
await mkdir(".local", { recursive: true });
const mf = new Miniflare(
  convertV4MiniflareOptions({
    modules: true,
    script: 'export default {fetch(){return new Response("local database")}}',
    d1Databases: ["DB"],
    d1Persist: ".local/d1",
    r2Buckets: ["IMAGES"],
  }),
);
const DB = await mf.getD1Database("DB");
const IMAGES = await mf.getR2Bucket("IMAGES");
const exists = await DB.prepare(
  "SELECT name FROM sqlite_master WHERE name='users'",
).first();
if (!exists) {
  // Apply every migration in order; the local database must match production
  // shape. Comments must be stripped before collapsing newlines, or the first
  // `--` line would comment out the whole script.
  for (const file of [
    "migrations/0001_review.sql",
    "migrations/0002_better_auth.sql",
    "migrations/0003_generation_foundation.sql",
    "migrations/0004_image_assets.sql",
  ])
    await DB.exec(
      (await readFile(file, "utf8"))
        .split("\n")
        .filter((line) => !line.trimStart().startsWith("--"))
        .join(" "),
    );
}
const accounts = [
  "owner@example.test",
  "jane@example.test",
  "paul@example.test",
];
for (const email of accounts)
  await DB.prepare(
    "INSERT INTO auth_user(id,name,email,emailVerified,createdAt,updatedAt) VALUES(?,?,?,1,?,?) ON CONFLICT DO NOTHING",
  )
    .bind(email, email, email, Date.now(), Date.now())
    .run();
const app = createApp(async (request) => {
  const cookie = request.headers.get("cookie") ?? "";
  const email = decodeURIComponent(
    cookie.match(/(?:^|;\s*)local-user=([^;]+)/)?.[1] ?? "jane@example.test",
  );
  if (!accounts.includes(email)) throw new Error("Unknown local account");
  return {
    email,
    name: email.split("@")[0],
    authId: email,
  };
});
const env = {
  DB,
  BETTER_AUTH_URL: `http://127.0.0.1:${port}`,
  BOOTSTRAP_ADMIN_EMAIL: accounts[0],
  // Local synthetic generation lanes; never credentials. Lets browser tests
  // exercise the workspace against the machine API directly.
  ARTICLE_LAB_ROUTES: JSON.stringify([
    {
      id: "glm-omniroute",
      label: "GLM subscription · OmniRoute (local test)",
      provider: "glm",
      transport: "omniroute",
      model: "glm/glm-5.3",
      response_models: ["glm-5.3", "glm/glm-5.3"],
    },
  ]),
  ARTICLE_LAB_RUNNER_TOKEN:
    process.env.ARTICLE_LAB_RUNNER_TOKEN ??
    "local-test-runner-token-0123456789abcdef",
  ARTICLE_LAB_IMAGES: JSON.stringify({ models: ["gpt-image-1"] }),
  IMAGES,
  ASSETS: {
    fetch: async (request: Request) => {
      let path = resolve("dist", "." + new URL(request.url).pathname);
      if (!path.startsWith(resolve("dist") + "/"))
        path = resolve("dist/index.html");
      let data: Buffer;
      try {
        data = await readFile(path);
      } catch {
        path = resolve("dist/index.html");
        data = await readFile(path);
      }
      const types: Record<string, string> = {
        ".html": "text/html",
        ".js": "text/javascript",
        ".css": "text/css",
      };
      return new Response(data, {
        headers: {
          "Content-Type": types[extname(path)] ?? "application/octet-stream",
        },
      });
    },
  },
};
createServer(async (req, res) => {
  try {
    if (req.url?.startsWith("/__local")) {
      const selected = new URL(
        req.url,
        `http://127.0.0.1:${port}`,
      ).searchParams.get("user");
      if (selected && accounts.includes(selected)) {
        res.writeHead(302, {
          "Set-Cookie": `local-user=${encodeURIComponent(selected)}; Path=/; SameSite=Strict`,
          Location: "/",
        });
        res.end();
        return;
      }
      res.setHeader("Content-Type", "text/html");
      res.end(
        `<h1>LOCAL DEVELOPMENT ONLY</h1>${accounts.map((a) => `<p><a href="/__local?user=${a}">${a}</a></p>`).join("")}`,
      );
      return;
    }
    const chunks: Buffer[] = [];
    for await (const chunk of req) chunks.push(Buffer.from(chunk));
    const request = new Request(`http://127.0.0.1:${port}${req.url}`, {
      method: req.method,
      headers: req.headers as Record<string, string>,
      body: chunks.length ? Buffer.concat(chunks) : undefined,
    });
    const response = await app.fetch(request, env as never);
    res.writeHead(response.status, Object.fromEntries(response.headers));
    res.end(Buffer.from(await response.arrayBuffer()));
  } catch {
    res.writeHead(500);
    res.end("Local harness request failed");
  }
}).listen(port, "127.0.0.1", () =>
  console.log(
    `LOCAL DEVELOPMENT ONLY: http://127.0.0.1:${port}/__local (synthetic accounts, local D1)`,
  ),
);
