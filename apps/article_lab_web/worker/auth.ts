import { createRemoteJWKSet, jwtVerify, type JWTVerifyGetKey } from "jose";
export type Identity = {
  email: string;
  name: string;
  subject: string;
  issuer: string;
};
const keysets = new Map<string, ReturnType<typeof createRemoteJWKSet>>();
export async function verifyIdentity(
  token: string,
  issuer: string,
  audience: string,
  keys?: JWTVerifyGetKey,
): Promise<Identity> {
  if (
    !/^https:\/\/[a-z0-9-]+\.cloudflareaccess\.com$/.test(issuer) ||
    !audience
  )
    throw new Error("Access configuration missing");
  if (!keys && !keysets.has(issuer))
    keysets.set(
      issuer,
      createRemoteJWKSet(new URL(`${issuer}/cdn-cgi/access/certs`)),
    );
  const { payload } = await jwtVerify(token, keys ?? keysets.get(issuer)!, {
    issuer,
    audience,
    algorithms: ["RS256"],
    requiredClaims: ["exp", "iat", "sub", "email"],
  });
  if (
    typeof payload.email !== "string" ||
    !/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(payload.email) ||
    typeof payload.sub !== "string" ||
    payload.type !== "app"
  )
    throw new Error("Human identity required");
  return {
    email: payload.email.trim().toLowerCase(),
    name:
      typeof payload.name === "string"
        ? payload.name.slice(0, 200)
        : payload.email,
    subject: payload.sub,
    issuer,
  };
}
