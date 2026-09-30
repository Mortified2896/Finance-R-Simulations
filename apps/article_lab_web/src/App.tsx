import { useEffect, useState } from "react";
import type { User, Assignment, ReviewDetail } from "../shared/types";
import { api, ApiError } from "./api";
import { Admin } from "./Admin";
import { ReviewPage } from "./ReviewPage";
import { createAuthClient } from "better-auth/react";
const authClient = createAuthClient();
export function App() {
  const [draftDirty, setDraftDirty] = useState(false);
  function leaveDraft() {
    if (
      draftDirty &&
      !window.confirm(
        "Discard the unsaved article draft? Cancel and export .md first to keep it.",
      )
    )
      return false;
    return true;
  }
  const [user, setUser] = useState<User | null>(null),
    [loading, setLoading] = useState(true),
    [error, setError] = useState(""),
    [assignments, setAssignments] = useState<Assignment[]>([]),
    [admin, setAdmin] = useState(false),
    [detail, setDetail] = useState<ReviewDetail | null>(null),
    [busy, setBusy] = useState(false);
  async function load() {
    setError("");
    try {
      const data = await api<{ user: User }>("/me");
      setUser(data.user);
      if (data.user.status === "approved")
        setAssignments(await api<Assignment[]>("/assignments"));
    } catch (e) {
      if (e instanceof ApiError && e.status === 401) {
        setUser(null);
        setAssignments([]);
      } else setError((e as Error).message);
    } finally {
      setLoading(false);
    }
  }
  useEffect(() => {
    void load();
    if (new URLSearchParams(location.search).has("error"))
      setError("Google sign-in was not completed. Please try again.");
  }, []);
  async function open(id: string) {
    setBusy(true);
    setError("");
    try {
      setDetail(await api<ReviewDetail>(`/reviews/${id}/open`, "POST", {}));
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }
  async function signIn() {
    setBusy(true);
    setError("");
    try {
      const result = await authClient.signIn.social({
        provider: "google",
        callbackURL: "/",
        errorCallbackURL: "/?error=signin",
      });
      if (result.error)
        throw new Error(result.error.message ?? "Google sign-in failed.");
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }
  async function signOut() {
    if (!leaveDraft()) return;
    setBusy(true);
    setError("");
    try {
      const result = await authClient.signOut();
      if (result.error)
        throw new Error(result.error.message ?? "Sign out failed.");
      setUser(null);
      setAssignments([]);
      setAdmin(false);
      setDraftDirty(false);
    } catch (e) {
      setError((e as Error).message);
    } finally {
      setBusy(false);
    }
  }
  return (
    <>
      <header>
        <div className="brand">
          Article Lab <span>/ Review</span>
        </div>
        <nav>
          {user?.status === "approved" && !detail && (
            <>
              <button
                className={!admin ? "active quiet" : "quiet"}
                onClick={() => {
                  if (!leaveDraft()) return;
                  setDraftDirty(false);
                  setAdmin(false);
                  void load();
                }}
              >
                Articles
              </button>
              {user.role === "admin" && (
                <button
                  className={admin ? "active quiet" : "quiet"}
                  onClick={() => setAdmin(true)}
                >
                  Admin
                </button>
              )}
            </>
          )}
          {user && !detail && (
            <button
              className="quiet"
              disabled={busy}
              onClick={() => void signOut()}
            >
              Sign out
            </button>
          )}
        </nav>
      </header>
      <main>
        {error && (
          <div className="error" role="alert">
            {error}
            <div className="actions">
              <button onClick={() => void load()}>Retry</button>
              <a href="/">Sign in again</a>
            </div>
          </div>
        )}
        {loading ? (
          <p>Loading your account…</p>
        ) : !user ? (
          <section className="welcome">
            <h1>Article Lab</h1>
            <button disabled={busy} onClick={() => void signIn()}>
              Continue with Google
            </button>
          </section>
        ) : user.status !== "approved" ? (
          <section className="welcome">
            <h1>
              {user.status === "pending"
                ? "Waiting for approval"
                : user.status === "rejected"
                  ? "Account not approved"
                  : "Account disabled"}
            </h1>
            <p>
              {user.status === "pending"
                ? "Your account is ready for the administrator to review. Once approved, your articles will appear here."
                : "Contact the administrator if you believe this should change."}
            </p>
            <p>{user.email}</p>
            <button onClick={() => void load()}>Check status</button>
          </section>
        ) : detail ? (
          <ReviewPage
            key={detail.review.id}
            detail={detail}
            onBack={() => {
              setDetail(null);
              void load();
            }}
          />
        ) : admin ? (
          <Admin
            onDirtyChange={setDraftDirty}
            onReview={(review) => {
              if (leaveDraft()) {
                setDraftDirty(false);
                setDetail(review);
              }
            }}
          />
        ) : (
          <>
            <div className="page-heading">
              <h1>Your articles</h1>
              <p>Read closely. Highlight a passage. Share what you think.</p>
            </div>
            {!assignments.length ? (
              <section className="panel">
                <h2>You’re all set.</h2>
                <p>
                  Assigned articles will appear here when the administrator adds
                  them.
                </p>
                <button className="quiet" onClick={() => void load()}>
                  Refresh articles
                </button>
              </section>
            ) : (
              <div className="inbox">
                {assignments.map((a) => (
                  <article className="inbox-row" key={a.id}>
                    <div>
                      <span className={`status ${a.status.replace(" ", "-")}`}>
                        {a.status}
                      </span>
                      <h2>{a.title}</h2>
                      <p>{a.subtitle}</p>
                      <small>Version {a.version_number}</small>
                    </div>
                    <button disabled={busy} onClick={() => void open(a.id)}>
                      {a.status === "submitted"
                        ? "View"
                        : a.status === "in progress"
                          ? "Continue"
                          : "Review"}{" "}
                      →
                    </button>
                  </article>
                ))}
              </div>
            )}
          </>
        )}
      </main>
      <footer>Article Lab · A place for better feedback</footer>
    </>
  );
}
