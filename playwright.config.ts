// Browser-test harness config: boots the dev server on a side port and runs
// tests.browser.ts in real Chromium and in WebKit, Safari's engine. In Chromium, fake media
// devices stand in for the camera/microphone so Camera()/Microphone() demos are testable
// and CI-safe; WebKit has none, so the camera tests skip there.
import { defineConfig } from "@playwright/test"

export default defineConfig({
  testMatch: "tests.browser.ts",
  // agent worktrees live under .claude/ and carry their own copy of the tests
  testIgnore: ["**/.claude/**"],
  fullyParallel: true,
  timeout: 60_000,
  expect: { timeout: 15_000 }, // the first load waits for the dev server's bundle
  retries: process.env.CI ? 2 : 0,
  use: {
    baseURL: "http://localhost:3210",
  },
  projects: [
    {
      name: "chromium",
      use: {
        browserName: "chromium",
        launchOptions: {
          args: [
            "--use-fake-ui-for-media-stream", // auto-grant camera/mic prompts
            "--use-fake-device-for-media-stream", // synthetic video/audio feed
          ],
        },
        permissions: ["camera", "microphone"],
      },
    },
    { name: "webkit", use: { browserName: "webkit" } },
  ],
  webServer: {
    command: "PORT=3210 bun server.ts",
    port: 3210,
    reuseExistingServer: !process.env.CI,
  },
})
