import { useMemo } from "react";
import type {
  ArticleFeedback,
  ReviewDetail,
  Assignment,
} from "../shared/types";
import { Article } from "./Article";

export function ArticleOverview({
  detail,
  assignments,
  onBack,
  onReview,
}: {
  detail: ArticleFeedback;
  assignments: Assignment[];
  onBack: () => void;
  onReview: (detail: ReviewDetail) => void;
}) {
  const { version, feedback } = detail;
  const annotations = useMemo(
    () => feedback.flatMap((f) => f.review.annotations),
    [feedback],
  );
  return (
    <>
      <div className="toolbar">
        <button className="quiet" onClick={onBack}>
          ← Back to articles
        </button>
        <span>Saved snapshot · Read-only</span>
      </div>
      <div className="review-heading">
        <h1>{version.title}</h1>
        <p>{version.subtitle}</p>
        <small>Version {version.version_number}</small>
      </div>
      <p className="hint">
        All submitted reviews for this version appear below. Unfinished reviewer
        drafts remain private until submission.
      </p>
      <section className="panel" aria-label="Review status">
        <h2>Review status</h2>
        {!assignments.length ? (
          <p>No reviews assigned yet.</p>
        ) : (
          <ul>
            {assignments.map((a) => (
              <li key={a.id}>
                {a.display_name} ({a.email}) · {a.status}
              </li>
            ))}
          </ul>
        )}
      </section>
      <div className="review-layout">
        <section className="paper">
          <Article version={version} annotations={annotations} />
        </section>
        <aside className="comments" aria-label="Submitted feedback">
          <h2>
            Submitted reviews <span>{feedback.length}</span>
          </h2>
          {!feedback.length && (
            <p>
              No submitted reviews yet. You can read this snapshot without
              assigning it to yourself.
            </p>
          )}
          {feedback.map(({ review, reviewer }) => (
            <section className="comment" key={review.id}>
              <h3>{reviewer.display_name}</h3>
              <small>{reviewer.email}</small>
              <button
                className="quiet"
                onClick={() => onReview({ version, review, reviewer })}
              >
                View review
              </button>
              <h4>General feedback</h4>
              <p className="feedback-text">
                {review.general_feedback || "No general feedback."}
              </p>
              {review.annotations.map((a) => (
                <div className="comment" key={a.id} id={a.id} tabIndex={-1}>
                  <blockquote>{a.exact_quote}</blockquote>
                  <p className="feedback-text">
                    {a.comment || "No comment text."}
                  </p>
                </div>
              ))}
            </section>
          ))}
        </aside>
      </div>
    </>
  );
}
