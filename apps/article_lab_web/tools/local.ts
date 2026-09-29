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
  }),
);
const DB = await mf.getD1Database("DB");
const exists = await DB.prepare(
  "SELECT name FROM sqlite_master WHERE name='users'",
).first();
if (!exists) {
  const sql = await readFile("migrations/0001_review.sql", "utf8");
  await DB.exec(sql.replace(/\n/g, " "));
}
const accounts = [
  "owner@example.test",
  "jane@example.test",
  "paul@example.test",
];
const app = createApp(async (request) => {
  const cookie = request.headers.get("cookie") ?? "";
  const email = decodeURIComponent(
    cookie.match(/(?:^|;\s*)local-user=([^;]+)/)?.[1] ?? "jane@example.test",
  );
  if (!accounts.includes(email)) throw new Error("Unknown local account");
  return {
    email,
    name: email.split("@")[0],
    subject: email,
    issuer: "local-harness",
  };
});
const env = {
  DB,
  BOOTSTRAP_ADMIN_EMAIL: accounts[0],
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
