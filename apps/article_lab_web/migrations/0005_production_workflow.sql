-- Additive Shiny production workflow. No old workspace, generation, image,
-- article version, reviewer or authentication records are rewritten or deleted.
PRAGMA foreign_keys = ON;
CREATE TABLE production_state (
 article_id TEXT PRIMARY KEY REFERENCES articles(id), revision INTEGER NOT NULL DEFAULT 0,
 last_mutation TEXT, updated_at TEXT NOT NULL
);
CREATE TABLE production_mutations (
 id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id), request_hash TEXT NOT NULL,
 revision INTEGER NOT NULL, result_json TEXT NOT NULL, created_at TEXT NOT NULL
);
CREATE INDEX production_mutations_article ON production_mutations(article_id,created_at);
CREATE TABLE production_items (
 id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id),
 kind TEXT NOT NULL CHECK(kind IN ('titles','subtitles','thumbnail_concepts','outline')),
 parent_id TEXT REFERENCES production_items(id), source_candidate_id TEXT UNIQUE REFERENCES generation_candidates(id),
 original_text TEXT NOT NULL, text TEXT NOT NULL, input_snapshot TEXT NOT NULL DEFAULT '{}', notes TEXT NOT NULL DEFAULT '',
 status TEXT NOT NULL DEFAULT 'candidate' CHECK(status IN ('candidate','approved','archived')),
 created_at TEXT NOT NULL, updated_at TEXT NOT NULL,
 CHECK((kind='titles' AND parent_id IS NULL) OR (kind!='titles' AND parent_id IS NOT NULL)),
 UNIQUE(id,article_id)
);
CREATE INDEX production_items_article ON production_items(article_id,kind,status,parent_id);
CREATE UNIQUE INDEX one_approved_outline ON production_items(parent_id) WHERE kind='outline' AND status='approved';
CREATE TABLE production_packages (
 id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id),
 title_id TEXT NOT NULL, subtitle_id TEXT NOT NULL UNIQUE,
 image_asset_id TEXT REFERENCES image_assets(id), concept_id TEXT REFERENCES production_items(id),
 image_settings_json TEXT NOT NULL DEFAULT '{}', notes TEXT NOT NULL DEFAULT '', archived INTEGER NOT NULL DEFAULT 0 CHECK(archived IN (0,1)), created_at TEXT NOT NULL,
 CHECK(id=subtitle_id), FOREIGN KEY(title_id,article_id) REFERENCES production_items(id,article_id),
 FOREIGN KEY(subtitle_id,article_id) REFERENCES production_items(id,article_id), UNIQUE(id,article_id)
);
CREATE TABLE production_image_links (
 package_id TEXT NOT NULL, article_id TEXT NOT NULL REFERENCES articles(id),
 image_asset_id TEXT NOT NULL REFERENCES image_assets(id), archived INTEGER NOT NULL DEFAULT 0 CHECK(archived IN (0,1)),
 notes TEXT NOT NULL DEFAULT '', PRIMARY KEY(package_id,image_asset_id),
 FOREIGN KEY(package_id,article_id) REFERENCES production_packages(id,article_id)
);
CREATE TABLE production_job_targets (
 job_id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id),
 kind TEXT NOT NULL CHECK(kind IN ('titles','subtitles','thumbnail_concepts','outline','image')),
 parent_id TEXT REFERENCES production_items(id), input_snapshot TEXT NOT NULL DEFAULT '{}', created_at TEXT NOT NULL
);
CREATE TABLE production_drafts (
 id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id), package_id TEXT NOT NULL,
 outline_id TEXT NOT NULL, original_body TEXT NOT NULL, body TEXT NOT NULL, input_snapshot TEXT NOT NULL,
 notes TEXT NOT NULL DEFAULT '', status TEXT NOT NULL DEFAULT 'draft' CHECK(status IN ('draft','approved','rejected')),
 created_at TEXT NOT NULL, updated_at TEXT NOT NULL,
 FOREIGN KEY(package_id,article_id) REFERENCES production_packages(id,article_id),
 FOREIGN KEY(outline_id,article_id) REFERENCES production_items(id,article_id)
);
CREATE UNIQUE INDEX one_approved_draft ON production_drafts(package_id) WHERE status='approved';
CREATE TABLE production_draft_revisions (
 id TEXT PRIMARY KEY, draft_id TEXT NOT NULL REFERENCES production_drafts(id), mutation_id TEXT NOT NULL,
 before_body TEXT NOT NULL, after_body TEXT NOT NULL, before_notes TEXT NOT NULL, after_notes TEXT NOT NULL,
 created_at TEXT NOT NULL, UNIQUE(draft_id,mutation_id)
);
CREATE TRIGGER immutable_production_revision_update BEFORE UPDATE ON production_draft_revisions BEGIN SELECT RAISE(ABORT,'Draft revisions are immutable'); END;
CREATE TRIGGER immutable_production_revision_delete BEFORE DELETE ON production_draft_revisions BEGIN SELECT RAISE(ABORT,'Draft revisions are immutable'); END;
CREATE TRIGGER immutable_production_original BEFORE UPDATE OF original_body ON production_drafts BEGIN SELECT RAISE(ABORT,'Original draft is immutable'); END;
CREATE TRIGGER immutable_candidate_original BEFORE UPDATE OF original_text,source_candidate_id ON production_items BEGIN SELECT RAISE(ABORT,'Candidate provenance is immutable'); END;
CREATE TABLE production_settings (
 article_id TEXT NOT NULL REFERENCES articles(id), kind TEXT NOT NULL, settings_json TEXT NOT NULL,
 PRIMARY KEY(article_id,kind)
);
CREATE TABLE production_templates (
 id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id), kind TEXT NOT NULL,
 name TEXT NOT NULL, prompt TEXT NOT NULL, UNIQUE(article_id,kind,name)
);
CREATE TABLE production_publishing (
 draft_id TEXT PRIMARY KEY REFERENCES production_drafts(id), article_id TEXT NOT NULL REFERENCES articles(id),
 settings_json TEXT NOT NULL, submitted_at TEXT, published_at TEXT, updated_at TEXT NOT NULL
);
CREATE TABLE production_snapshots (
 version_id TEXT PRIMARY KEY REFERENCES article_versions(id), article_id TEXT NOT NULL REFERENCES articles(id),
 draft_id TEXT NOT NULL REFERENCES production_drafts(id), created_at TEXT NOT NULL
);
-- Defense in depth against cross-article/wrong-stage links outside the HTTP API.
CREATE TRIGGER production_parent_insert BEFORE INSERT ON production_items WHEN NEW.parent_id IS NOT NULL BEGIN
 SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM production_items p WHERE p.id=NEW.parent_id AND p.article_id=NEW.article_id
 AND p.kind=CASE WHEN NEW.kind='subtitles' THEN 'titles' ELSE 'subtitles' END) THEN RAISE(ABORT,'Wrong production parent') END;
END;
CREATE TRIGGER production_parent_update BEFORE UPDATE OF parent_id,article_id,kind ON production_items BEGIN SELECT RAISE(ABORT,'Production parent is immutable'); END;
CREATE TRIGGER production_asset_link BEFORE INSERT ON production_image_links BEGIN
 SELECT CASE WHEN NOT EXISTS(SELECT 1 FROM image_assets a WHERE a.id=NEW.image_asset_id AND a.article_id=NEW.article_id) THEN RAISE(ABORT,'Wrong article asset') END;
END;
