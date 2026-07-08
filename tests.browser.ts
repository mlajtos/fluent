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

test("Ctrl/Cmd+O opens the example gallery without editor focus", async ({ page }) => {
  // regression: the binding lived only inside Monaco, so with focus anywhere
  // else the browser dialog won (Safari's file picker) instead of the gallery
  await open(page, "1 + 1")
  await expect(panel(page)).toContainText("2")
  // the result can render before Monaco finishes mounting (slow CI) and the
  // shortcut is a no-op until then – keep pressing until the gallery appears
  await expect(async () => {
    await page.keyboard.press("ControlOrMeta+KeyO")
    await expect(page.getByPlaceholder("Select an example to load")).toBeVisible({ timeout: 2_000 })
  }).toPass({ timeout: 20_000 })
  // the ⇧ variant must work too – Safari reserves plain ⌘O in its menu,
  // so ⌘⇧O is the only chord Safari users have
  await page.keyboard.press("Escape")
  await expect(page.getByPlaceholder("Select an example to load")).not.toBeVisible()
  await page.keyboard.press("ControlOrMeta+Shift+KeyO")
  await expect(page.getByPlaceholder("Select an example to load")).toBeVisible()
})

test("README training snippet converges and stays responsive", async ({ page }) => {
  // the adam twin of this snippet froze the tab for ~52s (exponential
  // deferred compilation – see jaxjs notes); sgd jits and must stay fluid,
  // with converged values rendered well within the timeout
  await open(page, `
θ: ~([0, 0]),
𝓛: { Σ((θ - [0.23, 0.47])^2) },
opt: sgd(0.1),
{ opt(𝓛) } ⟳ 100,
θ
`.trim())
  await expect(panel(page)).toContainText("0.23", { timeout: 30_000 })
  await expect(panel(page)).toContainText("0.47")
})

test("camera edge-detection demo produces non-flat output", async ({ page }) => {
  // regression: camera pixels were int32, and jax-js integer arithmetic
  // truncates (int ÷ 255 = 0, mean stays int) – the whole edge response
  // silently collapsed to zeros and rendered as a flat heatmap
  await open(page, `
cam: Camera(320, 240),
k: [[0, 1, 0], [1, -4, 1], [0, 1, 0]],
edges: $({ abs(conv(k, mean(cam(), 2))) }),
edges
`.trim())
  const canvas = panel(page).locator("canvas").first()
  await expect(canvas).toBeVisible({ timeout: 20_000 })
  // the fake feed has structure, so the heatmap must span the colormap
  await expect(async () => {
    const spread = await canvas.evaluate((el: HTMLCanvasElement) => {
      const ctx = el.getContext("2d")
      if (!ctx) { return -1 }
      const { data } = ctx.getImageData(0, 0, el.width, el.height)
      let min = 255, max = 0
      for (let i = 0; i < data.length; i += 4) {
        min = Math.min(min, data[i]!)
        max = Math.max(max, data[i]!)
      }
      return max - min
    })
    expect(spread).toBeGreaterThan(10)
  }).toPass({ timeout: 15_000 })
})
