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
  reviewer?: Pick<User, "id" | "display_name" | "email">;
};
export type ArticleFeedback = {
  version: ArticleVersion;
  feedback: {
    review: Review;
    reviewer: NonNullable<ReviewDetail["reviewer"]>;
  }[];
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
export type GenerationRoute = {
  id: string;
  label: string;
  provider: "codex" | "glm";
  transport: "omniroute" | "direct";
  model: string;
  response_models: string[];
};
export type GenerationKind =
  "titles" | "subtitles" | "thumbnail_concepts" | "outline";
export type GenerationJobView = {
  id: string;
  article_id: string;
  kind: GenerationKind;
  route_id: string;
  status:
    "queued" | "running" | "succeeded" | "failed" | "uncertain" | "cancelled";
  actual_model: string | null;
  error_code: string | null;
  created_at: string;
  started_at: string | null;
  finished_at: string | null;
};
export type GenerationCandidate = {
  id: string;
  article_id: string;
  job_id: string;
  kind: GenerationKind;
  value: string;
  archived_at: string | null;
  created_at: string;
};
export type Selection = {
  article_id: string;
  kind: GenerationKind;
  candidate_id: string;
  selected_at: string;
};
export type WorkspaceRow = {
  article_id: string;
  topic: string;
  brief: string;
  evidence: string;
  draft_body: string;
  revision: number;
  created_at: string;
  updated_at: string;
  last_mutation: string | null;
  prompt_settings: string;
};
export type ImageJobView = {
  id: string;
  article_id: string;
  status:
    "queued" | "running" | "succeeded" | "failed" | "uncertain" | "cancelled";
  model: string;
  size: string;
  quality: string;
  error_code: string | null;
  asset_id: string | null;
  created_at: string;
  started_at: string | null;
  finished_at: string | null;
};
export type ImageAsset = {
  id: string;
  article_id: string;
  job_id: string | null;
  source: "generated" | "upload";
  object_key: string;
  content_type: string;
  byte_size: number;
  width: number | null;
  height: number | null;
  alt_text: string;
  caption: string;
  model: string | null;
  prompt: string | null;
  requested_by: string;
  created_at: string;
  archived_at: string | null;
};
export type WorkspaceDetail = {
  workspace: WorkspaceRow;
  candidates: GenerationCandidate[];
  selections: Selection[];
  jobs: GenerationJobView[];
  image_jobs: ImageJobView[];
  image_assets: ImageAsset[];
  thumbnail: { image_asset_id: string; selected_at: string } | null;
};
export type LabCatalog = {
  routes: GenerationRoute[];
  runner_configured: boolean;
  images: { models: string[]; sizes: string[]; qualities: string[] };
  images_configured: boolean;
  runners: {
    runner_id: string;
    last_seen: string;
    route_ids: string[];
    image_ready: boolean;
  }[];
};
export type PromptSettings = Partial<
  Record<
    GenerationKind,
    { resolved_prompt: string; route_id: string; count: number }
  >
>;
