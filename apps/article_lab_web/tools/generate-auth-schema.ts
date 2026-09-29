// Generate, never apply, Better Auth's native D1 schema. No real credentials.
import { Miniflare, convertV4MiniflareOptions } from "miniflare";
import { getMigrations } from "better-auth/db/migration";
import { writeFile } from "node:fs/promises";
import { createAuth } from "../worker/auth";
const mf = new Miniflare(
  convertV4MiniflareOptions({
    modules: true,
    script: "export default {fetch(){return new Response()}}",
    d1Databases: ["DB"],
  }),
);
try {
  const auth = createAuth({
    DB: (await mf.getD1Database("DB")) as unknown as D1Database,
    BETTER_AUTH_URL: "https://feedback.moneymattersmedia.com",
    BETTER_AUTH_SECRET: crypto.randomUUID() + crypto.randomUUID(),
    GOOGLE_CLIENT_ID: "schema-only",
    GOOGLE_CLIENT_SECRET: "schema-only",
  });
  const migration = await getMigrations(auth.options);
  const sql = await migration.compileMigrations();
  await writeFile(
    "migrations/0002_better_auth.sql",
    sql +
      "\nALTER TABLE users ADD COLUMN auth_user_id TEXT REFERENCES auth_user(id);\nCREATE UNIQUE INDEX users_auth_user ON users(auth_user_id);\nDROP TABLE access_identities;\n",
  );
  console.log(
    "Generated Better Auth schema. No production data or secrets accessed.",
  );
} finally {
  await mf.dispose();
}
