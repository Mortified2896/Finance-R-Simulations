// Existing generation/image/runner implementation is preserved byte-for-byte in
// generation-core.ts. This facade adds only approved-admin editorial routes.
export * from "./generation-core";
import { handleLabRequest as handleCoreLabRequest } from "./generation-core";
import type { GenerationEnv, Actor } from "./generation-core";
import { handleProductionRequest } from "./production";

export async function handleLabRequest(request: Request, env: GenerationEnv, actor: Actor): Promise<Response> {
  if (new URL(request.url).pathname.startsWith("/api/admin/lab/production/")) {
    return handleProductionRequest(request, env, actor, async (markdown) => {
      const { renderMarkdown } = await import("../shared/markdown");
      return renderMarkdown(markdown);
    });
  }
  const response = await handleCoreLabRequest(request, env, actor);
  if (new URL(request.url).pathname === "/api/admin/lab/routes" && response.ok) {
    return new Response(JSON.stringify({ ...await response.json() as object, image_storage_configured: Boolean(env.IMAGES) }), { status: response.status, headers: response.headers });
  }
  return response;
}
