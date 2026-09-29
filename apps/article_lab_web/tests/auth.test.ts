import { describe, it, expect, beforeAll } from "vitest";
import { generateKeyPair, SignJWT, createLocalJWKSet, exportJWK } from "jose";
import { verifyIdentity } from "../worker/auth";
let privateKey: CryptoKey, keys: ReturnType<typeof createLocalJWKSet>;
const issuer = "https://test.cloudflareaccess.com";
beforeAll(async () => {
  const pair = await generateKeyPair("RS256");
  privateKey = pair.privateKey;
  keys = createLocalJWKSet({
    keys: [{ ...(await exportJWK(pair.publicKey)), kid: "test", alg: "RS256" }],
  });
});
async function token(extra: Record<string, unknown> = {}) {
  return new SignJWT({ email: "Jane@Example.com", type: "app", ...extra })
    .setProtectedHeader({ alg: "RS256", kid: "test" })
    .setSubject("access-user")
    .setIssuer(issuer)
    .setAudience("review-audience")
    .setIssuedAt()
    .setExpirationTime("1h")
    .sign(privateKey);
}
describe("Access JWT validation", () => {
  it("verifies signature, issuer, audience and normalizes email", async () => {
    expect(
      (await verifyIdentity(await token(), issuer, "review-audience", keys))
        .email,
    ).toBe("jane@example.com");
  });
  it("rejects wrong audience/issuer and service identities", async () => {
    await expect(
      verifyIdentity(await token(), issuer, "wrong", keys),
    ).rejects.toThrow();
    await expect(
      verifyIdentity(
        await token(),
        "https://other.cloudflareaccess.com",
        "review-audience",
        keys,
      ),
    ).rejects.toThrow();
    await expect(
      verifyIdentity(
        await token({ type: "service" }),
        issuer,
        "review-audience",
        keys,
      ),
    ).rejects.toThrow();
  });
  it("rejects forged signature, expired and missing identity claims", async () => {
    const t = await token();
    await expect(
      verifyIdentity(
        t.slice(0, -20) + "a".repeat(20),
        issuer,
        "review-audience",
        keys,
      ),
    ).rejects.toThrow();
    const expired = await new SignJWT({ email: "a@b.com", type: "app" })
      .setProtectedHeader({ alg: "RS256", kid: "test" })
      .setSubject("s")
      .setIssuer(issuer)
      .setAudience("review-audience")
      .setIssuedAt(1)
      .setExpirationTime(2)
      .sign(privateKey);
    await expect(
      verifyIdentity(expired, issuer, "review-audience", keys),
    ).rejects.toThrow();
    await expect(
      verifyIdentity(
        await token({ email: null }),
        issuer,
        "review-audience",
        keys,
      ),
    ).rejects.toThrow();
  });
});
