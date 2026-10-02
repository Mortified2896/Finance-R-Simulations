import fs from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { validateLocalRoutes, executeTextJob, ProviderFailure } from "./providers.mjs";

export function validateOrigin(value) {
  const url = new URL(value);
  if (url.protocol !== "https:" || url.username || url.password || url.search || url.hash || url.pathname !== "/") throw new Error("Configure a bare HTTPS Article Lab origin.");
  return url.origin;
}
export async function runOnce(config, { env = process.env, fetcher = fetch } = {}) {
  const routes = validateLocalRoutes(config.routes), origin = validateOrigin(config.origin);
  const token = env.ARTICLE_LAB_RUNNER_TOKEN;
  if (!token || token.length < 32) throw new Error("ARTICLE_LAB_RUNNER_TOKEN is missing or too short.");
  if (typeof config.runner_id !== "string" || !/^[a-zA-Z0-9_-]{1,80}$/.test(config.runner_id)) throw new Error("Invalid runner ID.");
  if (typeof config.spool_dir !== "string" || !path.isAbsolute(config.spool_dir)) throw new Error("Configure an absolute private spool directory outside the repository.");
  const spool = path.resolve(config.spool_dir);
  await fs.mkdir(spool, { recursive: true, mode: 0o700 });
  await fs.chmod(spool, 0o700);
  const post = async (endpoint, input) => {
    const response = await fetcher(`${origin}/api/generation-runner${endpoint}`, {
      method: "POST", redirect: "error", signal: AbortSignal.timeout(30_000),
      headers: { "Authorization": `Bearer ${token}`, "Content-Type": "application/json" }, body: JSON.stringify(input),
    });
    if (!response.ok) throw new Error(`Article Lab returned HTTP ${response.status}.`);
    return response.json();
  };
  const { job } = await post("/claim", { runner_id: config.runner_id, route_ids: routes.map((route) => route.route.id) });
  if (!job) return { status: "idle" };
  let result;
  try { result = await executeTextJob(job, routes, { env, fetcher }); }
  catch (error) {
    const code = error instanceof ProviderFailure ? error.code : "runner_configuration";
    await post(`/jobs/${encodeURIComponent(job.id)}/fail`, { lease_token: job.lease_token, code });
    return { status: "failed", job_id: job.id, code };
  }
  // Write before upload. A crash after provider success never triggers generation again.
  const saved = path.join(spool, `${job.id}.json`);
  await fs.writeFile(saved, JSON.stringify({ origin, job_id: job.id, result }), { mode: 0o600, flag: "wx" });
  try {
    await post(`/jobs/${job.id}/complete`, result);
    await fs.unlink(saved);
    return { status: "succeeded", job_id: job.id };
  } catch { return { status: "completion_pending", job_id: job.id, saved }; }
}

export async function replayCompletion(config, filename, { env = process.env, fetcher = fetch } = {}) {
  const origin = validateOrigin(config.origin), token = env.ARTICLE_LAB_RUNNER_TOKEN;
  if (!token || token.length < 32) throw new Error("Runner token is missing or too short.");
  const record = JSON.parse(await fs.readFile(filename, "utf8"));
  if (record.origin !== origin || !/^[0-9a-f-]{36}$/i.test(record.job_id)) throw new Error("Spool record does not match the configured Article Lab.");
  const response = await fetcher(`${origin}/api/generation-runner/jobs/${record.job_id}/complete`, {
    method: "POST", redirect: "error", signal: AbortSignal.timeout(30_000),
    headers: { "Authorization": `Bearer ${token}`, "Content-Type": "application/json" }, body: JSON.stringify(record.result),
  });
  if (!response.ok) throw new Error(`Completion not accepted: HTTP ${response.status}. Keep the private spool file; do not regenerate automatically.`);
  await fs.unlink(filename);
  return { status: "completion_replayed", job_id: record.job_id };
}
async function main() {
  const args = process.argv.slice(2), configIndex = args.indexOf("--config");
  if (configIndex < 0 || !args[configIndex + 1]) throw new Error("Usage: node --experimental-strip-types scripts/article_lab_worker/runner.mjs --config /private/config.json [--once | --replay-completion /private/spool.json]");
  const config = JSON.parse(await fs.readFile(args[configIndex + 1], "utf8"));
  const replayIndex = args.indexOf("--replay-completion");
  if (replayIndex >= 0) { console.log(JSON.stringify(await replayCompletion(config, args[replayIndex + 1]))); return; }
  do {
    const result = await runOnce(config); console.log(JSON.stringify(result));
    if (result.status === "completion_pending") { process.exitCode = 1; return; }
    if (args.includes("--once")) return;
    await new Promise((resolve) => setTimeout(resolve, 10_000));
  } while (true);
}
if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  main().catch(() => { console.error("Article Lab runner stopped. Check private configuration/connectivity; credentials and provider payloads are not logged."); process.exitCode = 1; });
}
