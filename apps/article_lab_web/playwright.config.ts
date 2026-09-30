import { defineConfig } from "@playwright/test";
export default defineConfig({
  testDir: "tests/browser",
  use: { baseURL: "http://127.0.0.1:5173" },
  workers: 1,
  reporter: "list",
  webServer: {
    command: "npm run dev:local",
    url: "http://127.0.0.1:5173",
    reuseExistingServer: true,
  },
  timeout: 60000,
});
