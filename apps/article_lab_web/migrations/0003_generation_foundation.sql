-- Additive only. Existing article versions, assignments and reviews are untouched.
PRAGMA foreign_keys = ON;
CREATE TABLE article_workspaces (
 article_id TEXT PRIMARY KEY REFERENCES articles(id),
 topic TEXT NOT NULL, brief TEXT NOT NULL DEFAULT '', evidence TEXT NOT NULL DEFAULT '',
 draft_body TEXT NOT NULL DEFAULT '', revision INTEGER NOT NULL DEFAULT 0 CHECK(revision >= 0),
 created_at TEXT NOT NULL, updated_at TEXT NOT NULL, last_mutation TEXT
);
CREATE TABLE generation_jobs (
 id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id),
 requested_by TEXT NOT NULL REFERENCES users(id),
 kind TEXT NOT NULL CHECK(kind IN ('titles','subtitles','thumbnail_concepts','outline')),
 request_json TEXT NOT NULL, request_hash TEXT NOT NULL, route_json TEXT NOT NULL,
 route_id TEXT NOT NULL, status TEXT NOT NULL DEFAULT 'queued'
   CHECK(status IN ('queued','running','succeeded','failed','uncertain','cancelled')),
 lease_token TEXT, runner_id TEXT, lease_expires_at TEXT,
 actual_model TEXT, completion_hash TEXT, error_code TEXT,
 created_at TEXT NOT NULL, started_at TEXT, finished_at TEXT
);
CREATE INDEX generation_queue ON generation_jobs(status,created_at,id);
CREATE INDEX generation_article ON generation_jobs(article_id,created_at,id);
CREATE TABLE generation_candidates (
 id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id),
 job_id TEXT NOT NULL REFERENCES generation_jobs(id),
 kind TEXT NOT NULL CHECK(kind IN ('titles','subtitles','thumbnail_concepts','outline')),
 value TEXT NOT NULL, archived_at TEXT, created_at TEXT NOT NULL,
 UNIQUE(id,article_id,kind)
);
CREATE INDEX candidates_article ON generation_candidates(article_id,kind,created_at);
CREATE TABLE article_selections (
 article_id TEXT NOT NULL REFERENCES articles(id),
 kind TEXT NOT NULL CHECK(kind IN ('titles','subtitles','thumbnail_concepts','outline')),
 candidate_id TEXT NOT NULL, selected_at TEXT NOT NULL,
 PRIMARY KEY(article_id,kind),
 FOREIGN KEY(candidate_id,article_id,kind) REFERENCES generation_candidates(id,article_id,kind)
);
-- Generation output is immutable. Selection and archival never delete alternatives.
CREATE TRIGGER immutable_candidate BEFORE UPDATE OF id,article_id,job_id,kind,value,created_at
 ON generation_candidates BEGIN SELECT RAISE(ABORT,'Generation candidates are immutable'); END;
CREATE TRIGGER candidate_no_delete BEFORE DELETE ON generation_candidates
 BEGIN SELECT RAISE(ABORT,'Archive candidates instead of deleting them'); END;
