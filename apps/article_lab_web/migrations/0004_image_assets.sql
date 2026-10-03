-- Additive only. Text generation tables from 0003 are untouched.
PRAGMA foreign_keys = ON;
-- Per-workspace editable generation prompts and route/count defaults, keyed by
-- generation kind. Keeps prompt editing persistent without touching 0003 tables.
ALTER TABLE article_workspaces ADD COLUMN prompt_settings TEXT NOT NULL DEFAULT '{}';
-- Narrow availability signal: a heartbeat proves a runner polled recently,
-- never that a provider lane is healthy.
CREATE TABLE runner_heartbeats (
 runner_id TEXT PRIMARY KEY,
 last_seen TEXT NOT NULL,
 route_ids TEXT NOT NULL DEFAULT '[]',
 image_ready INTEGER NOT NULL DEFAULT 0
);
CREATE TABLE image_jobs (
 id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id),
 requested_by TEXT NOT NULL REFERENCES users(id),
 prompt TEXT NOT NULL, model TEXT NOT NULL, size TEXT NOT NULL, quality TEXT NOT NULL,
 request_hash TEXT NOT NULL, status TEXT NOT NULL DEFAULT 'queued'
   CHECK(status IN ('queued','running','succeeded','failed','uncertain','cancelled')),
 lease_token TEXT, runner_id TEXT, lease_expires_at TEXT,
 error_code TEXT, asset_id TEXT,
 created_at TEXT NOT NULL, started_at TEXT, finished_at TEXT
);
CREATE INDEX image_queue ON image_jobs(status,created_at,id);
CREATE TABLE image_assets (
 id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id),
 job_id TEXT REFERENCES image_jobs(id),
 source TEXT NOT NULL CHECK(source IN ('generated','upload')),
 object_key TEXT NOT NULL UNIQUE,
 content_type TEXT NOT NULL CHECK(content_type IN ('image/png','image/jpeg','image/webp')),
 byte_size INTEGER NOT NULL CHECK(byte_size > 0 AND byte_size <= 10000000),
 width INTEGER CHECK(width IS NULL OR width >= 200), height INTEGER CHECK(height IS NULL OR height >= 200),
 alt_text TEXT NOT NULL DEFAULT '', caption TEXT NOT NULL DEFAULT '',
 model TEXT, prompt TEXT, requested_by TEXT NOT NULL REFERENCES users(id),
 created_at TEXT NOT NULL, archived_at TEXT
);
CREATE INDEX image_assets_article ON image_assets(article_id,created_at);
-- Freeze which immutable assets each published version references. Asset
-- serving authorizes against this link, so a later draft thumbnail can never
-- leak to a reviewer of an earlier frozen version.
CREATE TABLE version_assets (
 version_id TEXT NOT NULL REFERENCES article_versions(id),
 image_asset_id TEXT NOT NULL REFERENCES image_assets(id),
 role TEXT NOT NULL DEFAULT 'inline' CHECK(role IN ('inline','thumbnail')),
 PRIMARY KEY(version_id,image_asset_id)
);
CREATE TABLE article_thumbnail (
 article_id TEXT PRIMARY KEY REFERENCES articles(id),
 image_asset_id TEXT NOT NULL REFERENCES image_assets(id),
 selected_at TEXT NOT NULL
);
