import type { GenerationRoute, ImageCatalog } from "../../shared/generation";
export type Catalog = {
  routes: GenerationRoute[]; runner_configured: boolean; image_storage_configured?: boolean;
  images_configured: boolean; images: ImageCatalog;
  runners: { runner_id: string; last_seen: string; route_ids: string[]; image_ready: boolean }[];
};
export class RequestError extends Error {
  status: number;
  constructor(message: string, status: number) { super(message); this.status = status; }
}
export async function request<T>(path: string, method = "GET", body?: unknown, signal?: AbortSignal): Promise<T> {
  const form = body instanceof FormData;
  const response = await fetch(`/api/admin/lab${path}`, {
    method, credentials: "same-origin", signal,
    headers: body === undefined || form ? undefined : { "Content-Type": "application/json" },
    body: body === undefined ? undefined : form ? body : JSON.stringify(body),
  });
  let result: unknown;
  try { result = await response.json(); } catch { throw new RequestError("The server returned an unreadable response. Keep your edits; do not regenerate automatically.", response.status); }
  if (!response.ok) throw new RequestError((result as { error?: string }).error ?? `Request failed (${response.status}).`, response.status);
  return result as T;
}
export function downloadText(name: string, content: string) {
  const url = URL.createObjectURL(new Blob([content], { type: "text/markdown;charset=utf-8" }));
  const anchor = document.createElement("a"); anchor.href = url; anchor.download = name; anchor.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}
