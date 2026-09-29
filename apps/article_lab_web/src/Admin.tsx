import { useEffect, useState, type FormEvent } from "react";
import type {
  User,
  ArticleVersion,
  Assignment,
  ReviewDetail,
} from "../shared/types";
import { api } from "./api";
export function Admin({ onReview }: { onReview: (d: ReviewDetail) => void }) {
  const [users, setUsers] = useState<User[]>([]),
    [versions, setVersions] = useState<ArticleVersion[]>([]),
    [assignments, setAssignments] = useState<Assignment[]>([]),
    [error, setError] = useState(""),
    [success, setSuccess] = useState(""),
    [busy, setBusy] = useState(false);
  async function load() {
    const [u, v, a] = await Promise.all([
      api<User[]>("/admin/users"),
      api<ArticleVersion[]>("/admin/versions"),
      api<Assignment[]>("/admin/assignments"),
    ]);
    setUsers(u);
    setVersions(v);
    setAssignments(a);
  }
  useEffect(() => {
    void load().catch((e) => setError(e.message));
  }, []);
  async function action(fn: () => Promise<void>, message: string) {
    setBusy(true);
    setError("");
    setSuccess("");
    try {
      await fn();
      await load();
      setSuccess(message);
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }
  async function publish(e: FormEvent<HTMLFormElement>) {
    e.preventDefault();
    const form = e.currentTarget,
      data = new FormData(form);
    await action(async () => {
      await api("/admin/versions", "POST", {
        article_id: data.get("article_id") || undefined,
        title: data.get("title"),
        subtitle: data.get("subtitle"),
        body: data.get("body"),
      });
      form.reset();
    }, "Immutable article version published. Assign it below.");
  }
  const articles = [
    ...new Map(versions.map((v) => [v.article_id, v])).values(),
  ];
  return (
    <>
      <h1>Administration</h1>
      {error && (
        <div role="alert" className="error">
          {error}
        </div>
      )}
      {success && (
        <div className="success" role="status">
          {success}
        </div>
      )}
      <section className="panel">
        <h2>Reviewers</h2>
        <p>
          Approve new accounts once. Their approval persists when they sign in
          again.
        </p>
        {!users.length && <p>No accounts yet.</p>}
        <div className="table-wrap">
          <table>
            <thead>
              <tr>
                <th>Account</th>
                <th>Status / role</th>
                <th>Actions</th>
              </tr>
            </thead>
            <tbody>
              {users.map((u) => (
                <tr key={u.id}>
                  <td>
                    <strong>{u.display_name}</strong>
                    <br />
                    {u.email}
                    <br />
                    <small>
                      First seen: {new Date(u.created_at).toLocaleString()}
                    </small>
                  </td>
                  <td>
                    {u.status} · {u.role}
                  </td>
                  <td>
                    {u.role === "reviewer" && (
                      <div className="actions">
                        {u.status !== "approved" && (
                          <button
                            disabled={busy}
                            onClick={() =>
                              void action(async () => {
                                await api(`/admin/users/${u.id}`, "PATCH", {
                                  status: "approved",
                                });
                              }, "Reviewer approved.")
                            }
                          >
                            Approve
                          </button>
                        )}
                        {u.status !== "rejected" && (
                          <button
                            className="quiet"
                            disabled={busy}
                            onClick={() =>
                              void action(async () => {
                                await api(`/admin/users/${u.id}`, "PATCH", {
                                  status: "rejected",
                                });
                              }, "Reviewer rejected.")
                            }
                          >
                            Reject
                          </button>
                        )}
                        {u.status === "approved" && (
                          <button
                            className="quiet"
                            disabled={busy}
                            onClick={() =>
                              void action(async () => {
                                await api(`/admin/users/${u.id}`, "PATCH", {
                                  status: "disabled",
                                });
                              }, "Reviewer disabled.")
                            }
                          >
                            Disable
                          </button>
                        )}
                      </div>
                    )}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </section>
      <section className="panel">
        <h2>Publish an article version</h2>
        <p>
          Published text stays fixed. Changes create a new version with its own
          assignments and reviews.
        </p>
        <form onSubmit={publish}>
          <label>
            Article project
            <select name="article_id">
              <option value="">Create a new article</option>
              {articles.map((v) => (
                <option key={v.article_id} value={v.article_id}>
                  {v.title}
                </option>
              ))}
            </select>
          </label>
          <label>
            Title
            <input name="title" required maxLength={250} />
          </label>
          <label>
            Subtitle
            <input name="subtitle" maxLength={500} />
          </label>
          <label>
            Markdown
            <textarea name="body" rows={10} required maxLength={150000} />
          </label>
          <button disabled={busy}>Publish version</button>
        </form>
      </section>
      <section className="panel">
        <h2>Assign a review</h2>
        <form
          onSubmit={(e) => {
            e.preventDefault();
            const data = new FormData(e.currentTarget);
            void action(async () => {
              await api("/admin/assignments", "POST", {
                version_id: data.get("version_id"),
                user_id: data.get("user_id"),
              });
            }, "Article assigned.");
          }}
        >
          <label>
            Article version
            <select name="version_id" required>
              <option value="">Choose a version</option>
              {versions.map((v) => (
                <option value={v.id} key={v.id}>
                  {v.title} · v{v.version_number}
                </option>
              ))}
            </select>
          </label>
          <label>
            Reviewer
            <select name="user_id" required>
              <option value="">Choose an approved reviewer</option>
              {users
                .filter((u) => u.status === "approved")
                .map((u) => (
                  <option value={u.id} key={u.id}>
                    {u.display_name} · {u.email}
                  </option>
                ))}
            </select>
          </label>
          <button disabled={busy || !versions.length}>Assign article</button>
        </form>
      </section>
      <section className="panel">
        <h2>Review status & feedback</h2>
        {!assignments.length ? (
          <p>No reviews assigned yet.</p>
        ) : (
          <div className="table-wrap">
            <table>
              <thead>
                <tr>
                  <th>Article</th>
                  <th>Reviewer</th>
                  <th>Status</th>
                  <th>Feedback</th>
                </tr>
              </thead>
              <tbody>
                {assignments.map((a) => (
                  <tr key={a.id}>
                    <td>
                      {a.title}
                      <br />
                      <small>Version {a.version_number}</small>
                    </td>
                    <td>
                      {a.display_name}
                      <br />
                      <small>{a.email}</small>
                    </td>
                    <td>{a.status}</td>
                    <td>
                      {a.status === "submitted" && (
                        <button
                          disabled={busy}
                          onClick={() =>
                            void action(
                              async () =>
                                onReview(
                                  await api<ReviewDetail>(
                                    `/admin/reviews/${a.id}`,
                                  ),
                                ),
                              "",
                            )
                          }
                        >
                          View feedback
                        </button>
                      )}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </section>
    </>
  );
}
