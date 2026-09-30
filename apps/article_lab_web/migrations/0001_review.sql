PRAGMA foreign_keys = ON;
CREATE TABLE users (
 id TEXT PRIMARY KEY, email TEXT NOT NULL UNIQUE COLLATE NOCASE,
 display_name TEXT NOT NULL, status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending','approved','rejected','disabled')),
 role TEXT NOT NULL DEFAULT 'reviewer' CHECK(role IN ('admin','reviewer')),
 created_at TEXT NOT NULL, approved_at TEXT, last_login_at TEXT NOT NULL
);
CREATE TABLE access_identities (
 subject TEXT NOT NULL, issuer TEXT NOT NULL, user_id TEXT NOT NULL REFERENCES users(id),
 PRIMARY KEY(issuer, subject)
);
CREATE TABLE articles (id TEXT PRIMARY KEY, created_by TEXT NOT NULL REFERENCES users(id), created_at TEXT NOT NULL);
CREATE TABLE article_versions (
 id TEXT PRIMARY KEY, article_id TEXT NOT NULL REFERENCES articles(id), version_number INTEGER NOT NULL,
 title TEXT NOT NULL, subtitle TEXT NOT NULL DEFAULT '', body TEXT NOT NULL, body_format TEXT NOT NULL CHECK(body_format='markdown'),
 rendered_html TEXT NOT NULL, anchor_text TEXT NOT NULL, created_at TEXT NOT NULL,
 UNIQUE(article_id, version_number)
);
CREATE TRIGGER immutable_version_update BEFORE UPDATE ON article_versions BEGIN SELECT RAISE(ABORT, 'Article versions are immutable'); END;
CREATE TRIGGER immutable_version_delete BEFORE DELETE ON article_versions BEGIN SELECT RAISE(ABORT, 'Article versions are immutable'); END;
CREATE TABLE review_assignments (
 id TEXT PRIMARY KEY, version_id TEXT NOT NULL REFERENCES article_versions(id), user_id TEXT NOT NULL REFERENCES users(id),
 assigned_by TEXT NOT NULL REFERENCES users(id), created_at TEXT NOT NULL, UNIQUE(version_id,user_id)
);
CREATE INDEX assignments_user ON review_assignments(user_id);
CREATE TABLE reviews (
 id TEXT PRIMARY KEY REFERENCES review_assignments(id), general_feedback TEXT NOT NULL DEFAULT '',
 revision INTEGER NOT NULL DEFAULT 0, status TEXT NOT NULL DEFAULT 'draft' CHECK(status IN ('draft','submitted')),
 created_at TEXT NOT NULL, updated_at TEXT NOT NULL, submitted_at TEXT, last_mutation TEXT
);
CREATE TABLE annotations (
 id TEXT PRIMARY KEY, review_id TEXT NOT NULL REFERENCES reviews(id), version_id TEXT NOT NULL REFERENCES article_versions(id),
 exact_quote TEXT NOT NULL, start_offset INTEGER NOT NULL, end_offset INTEGER NOT NULL,
 prefix TEXT NOT NULL, suffix TEXT NOT NULL, comment TEXT NOT NULL, created_at TEXT NOT NULL, updated_at TEXT NOT NULL,
 CHECK(start_offset>=0 AND end_offset>start_offset)
);
CREATE INDEX annotations_review ON annotations(review_id);
CREATE TRIGGER fixed_assignment BEFORE UPDATE ON review_assignments BEGIN SELECT RAISE(ABORT, 'Assignments are immutable'); END;
CREATE TRIGGER submitted_review_update BEFORE UPDATE ON reviews WHEN OLD.status='submitted' BEGIN SELECT RAISE(ABORT, 'Review is submitted'); END;
CREATE TRIGGER submitted_review_delete BEFORE DELETE ON reviews WHEN OLD.status='submitted' BEGIN SELECT RAISE(ABORT, 'Review is submitted'); END;
CREATE TRIGGER annotation_version BEFORE INSERT ON annotations WHEN NEW.version_id != (SELECT version_id FROM review_assignments WHERE id=NEW.review_id) BEGIN SELECT RAISE(ABORT, 'Wrong article version'); END;
CREATE TRIGGER submitted_annotation_insert BEFORE INSERT ON annotations WHEN (SELECT status FROM reviews WHERE id=NEW.review_id)='submitted' BEGIN SELECT RAISE(ABORT, 'Review is submitted'); END;
CREATE TRIGGER submitted_annotation_update BEFORE UPDATE ON annotations WHEN (SELECT status FROM reviews WHERE id=OLD.review_id)='submitted' BEGIN SELECT RAISE(ABORT, 'Review is submitted'); END;
CREATE TRIGGER submitted_annotation_delete BEFORE DELETE ON annotations WHEN (SELECT status FROM reviews WHERE id=OLD.review_id)='submitted' BEGIN SELECT RAISE(ABORT, 'Review is submitted'); END;
