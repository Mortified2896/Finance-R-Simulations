export type User = {
  auth_user_id: string;
  id: string;
  email: string;
  display_name: string;
  status: "pending" | "approved" | "rejected" | "disabled";
  role: "admin" | "reviewer";
  created_at: string;
  approved_at: string | null;
  last_login_at: string;
};
export type ArticleVersion = {
  id: string;
  article_id: string;
  version_number: number;
  title: string;
  subtitle: string;
  body: string;
  rendered_html: string;
  anchor_text: string;
  created_at: string;
};
export type Annotation = {
  id: string;
  exact_quote: string;
  start_offset: number;
  end_offset: number;
  prefix: string;
  suffix: string;
  comment: string;
  created_at?: string;
  updated_at?: string;
};
export type Review = {
  id: string;
  general_feedback: string;
  revision: number;
  status: "draft" | "submitted";
  created_at: string;
  updated_at: string;
  submitted_at: string | null;
  annotations: Annotation[];
};
export type ReviewDetail = {
  version: ArticleVersion;
  review: Review;
  reviewer?: User;
};
export type Assignment = {
  id: string;
  version_id: string;
  title: string;
  subtitle: string;
  version_number: number;
  status: "not started" | "in progress" | "submitted";
  email?: string;
  display_name?: string;
  submitted_at?: string;
};
