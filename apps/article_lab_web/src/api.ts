export class ApiError extends Error {
  constructor(
    message: string,
    public status: number,
  ) {
    super(message);
  }
}
export async function api<T>(
  path: string,
  method = "GET",
  data?: unknown,
): Promise<T> {
  let response: Response;
  try {
    response = await fetch(`/api${path}`, {
      method,
      headers: method === "GET" ? {} : { "Content-Type": "application/json" },
      body: data === undefined ? undefined : JSON.stringify(data),
      keepalive: data !== undefined && JSON.stringify(data).length < 50000,
    });
  } catch {
    throw new Error(
      "Connection lost. Your changes are not saved. Keep this page open and retry.",
    );
  }
  let result: any;
  try {
    result = await response.json();
  } catch {
    throw new Error(
      "Your session may have expired. Keep this page open, sign in in another tab, then retry saving.",
    );
  }
  if (!response.ok)
    throw new ApiError(result.error ?? "Request failed.", response.status);
  return result as T;
}
