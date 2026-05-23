import OpenAI from "openai";
import { observeOpenAI } from "@langfuse/openai";
import { LangfuseSpanProcessor } from "@langfuse/otel";
import { propagateAttributes, startActiveObservation } from "@langfuse/tracing";
import { NodeSDK } from "@opentelemetry/sdk-node";

let langfuseSpanProcessor = null;
let tracingInitialized = false;

function cleanEnv(name) {
  const value = process.env[name];
  if (value === undefined || value === null) return null;
  const cleaned = String(value).trim();
  return cleaned.length > 0 ? cleaned : null;
}

function sanitizeMetadata(metadata) {
  if (!metadata) return undefined;
  const cleaned = {};
  for (const [rawKey, rawValue] of Object.entries(metadata)) {
    const key = String(rawKey).replace(/[^a-z0-9]/gi, "");
    if (!key || rawValue === undefined || rawValue === null) continue;
    let value = String(rawValue).trim();
    if (!value) continue;
    if (value.length > 200) value = `${value.slice(0, 197)}...`;
    cleaned[key] = value;
  }
  return Object.keys(cleaned).length > 0 ? cleaned : undefined;
}

function tracingDisabled() {
  const value = (cleanEnv("LANGFUSE_TRACING_ENABLED") ?? "").toLowerCase();
  return value === "0" || value === "false" || value === "no" || value === "off";
}

function normalizeLangfuseEnv() {
  const host = cleanEnv("LANGFUSE_HOST");
  if (host && !cleanEnv("LANGFUSE_BASE_URL")) {
    process.env.LANGFUSE_BASE_URL = host;
  }
}

function langfuseEnabled() {
  if (tracingDisabled()) return false;
  normalizeLangfuseEnv();
  return Boolean(cleanEnv("LANGFUSE_PUBLIC_KEY") && cleanEnv("LANGFUSE_SECRET_KEY") && cleanEnv("LANGFUSE_BASE_URL"));
}

async function ensureTracingInitialized() {
  if (!langfuseEnabled()) return false;
  if (tracingInitialized) return true;

  langfuseSpanProcessor = new LangfuseSpanProcessor();
  const sdk = new NodeSDK({
    spanProcessors: [langfuseSpanProcessor],
  });
  sdk.start();
  tracingInitialized = true;
  return true;
}

function compactAttributes({ metadata, tags, userId, sessionId, traceName }) {
  const attributes = {};
  const cleanedMetadata = sanitizeMetadata(metadata);
  if (cleanedMetadata) attributes.metadata = cleanedMetadata;
  if (Array.isArray(tags) && tags.length > 0) attributes.tags = tags;
  if (userId) attributes.userId = userId;
  if (sessionId) attributes.sessionId = sessionId;
  if (traceName) attributes.traceName = traceName;
  return attributes;
}

export async function createOpenAIClient(apiKey, langfuseConfig = {}) {
  const client = new OpenAI({ apiKey });
  if (!(await ensureTracingInitialized())) return client;
  return observeOpenAI(client, langfuseConfig);
}

export async function withLangfuseRun({ name, input, metadata, tags, userId, sessionId, traceName }, fn) {
  if (!(await ensureTracingInitialized())) return fn();

  return startActiveObservation(name, async (span) => {
    if (input !== undefined) {
      span.update({ input });
    }
    const attributes = compactAttributes({ metadata, tags, userId, sessionId, traceName });
    if (Object.keys(attributes).length === 0) {
      return fn();
    }
    return propagateAttributes(attributes, fn);
  });
}

export async function flushLangfuse() {
  if (!langfuseSpanProcessor) return;
  await langfuseSpanProcessor.forceFlush();
}
