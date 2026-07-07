// IDE tests: real Chromium against the real dev server (playwright.config.ts
// boots it on :3210). Covers what tests.ts structurally can't – Monaco, the
// output panel, reactive islands, canvas renderers, media sources.
// Run with `bun run test:browser`; `bunx playwright install chromium` once.
import { test, expect, type Page } from "@playwright/test"

// mirror of client.tsx's StringSerialize (js-base64 encodeURI = unpadded base64url)
const open = (page: Page, code: string) =>
  page.goto(`/?code=${Buffer.from(code).toString("base64url")}`)

// the result panel – scoped so Monaco's text and line numbers never match
const panel = (page: Page) => page.getByTestId("print-panel").first()

test("evaluates a program and renders the result", async ({ page }) => {
  await open(page, "1 + 1")
  await expect(panel(page)).toContainText("2")
})

test("a typo'd axis renders an Error, not a silently wrong number", async ({ page }) => {
  await open(page, "sum([[1, 2], [3, 4]], axs)")
  await expect(panel(page)).toContainText("expected a tensor, got unbound symbol 'axs'")
})

test("unparseable code inside backticks shows the parse error, panel stays alive", async ({ page }) => {
  // regression: this threw inside the AST viz and stuck the output panel on
  // the error-boundary fallback until reload
  await open(page, "`}`")
  await expect(panel(page)).toContainText(/expected/i)
  await expect(page.getByText("Something went wrong")).toHaveCount(0)
})

test("backtick literals render as an AST tree", async ({ page }) => {
  await open(page, "`1 + 2`")
  await expect(panel(page).locator("svg").first()).toBeVisible()
})

test("slider drives a reactive recompute", async ({ page }) => {
  await open(page, "x: $(0.5), (Slider(x), x ^ 2)")
  await expect(panel(page)).toContainText("0.25")
  await page.locator("input[type=range]").fill("0.8")
  await expect(panel(page)).toContainText("0.64")
})

test("button click updates a reactive value", async ({ page }) => {
  await open(page, 'x: $(0), (Button("increment", { x(x() + 41) }), x)')
  await expect(panel(page)).toContainText("0")
  await page.getByRole("button", { name: "increment" }).click()
  await expect(panel(page)).toContainText("41")
})

test("camera (fake device) streams into a canvas", async ({ page }) => {
  // --use-fake-device-for-media-stream feeds a synthetic video – no hardware
  await open(page, "Camera(64, 48)")
  await expect(panel(page).locator("canvas").first()).toBeVisible({ timeout: 20_000 })
})
