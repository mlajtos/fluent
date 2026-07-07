// Browser-test harness config: boots the dev server on a side port and runs
// tests.browser.ts in real Chromium. Fake media devices stand in for the
// camera/microphone so Camera()/Microphone() demos are testable and CI-safe.
import { defineConfig } from "@playwright/test"

export default defineConfig({
  testMatch: "tests.browser.ts",
  fullyParallel: true,
  timeout: 60_000,
  expect: { timeout: 15_000 },      // first load compiles the wasm backend
  retries: process.env.CI ? 2 : 0,
  use: {
    baseURL: "http://localhost:3210",
    launchOptions: {
      args: [
        "--use-fake-ui-for-media-stream",     // auto-grant camera/mic prompts
        "--use-fake-device-for-media-stream", // synthetic video/audio feed
      ],
    },
    permissions: ["camera", "microphone"],
  },
  webServer: {
    command: "PORT=3210 bun server.ts",
    port: 3210,
    reuseExistingServer: !process.env.CI,
  },
})
