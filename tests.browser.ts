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

test("hovering a grid cell's empty space highlights the cell's source, not the whole grid", async ({ page }) => {
  await open(page, `Grid(3)(\n    ("a", \`9 - 5\`, 1 + 1),\n    ("b", \`8 ÷ 2\`, 20 + 22),\n)`)
  await expect(panel(page)).toContainText("42")
  const expectHighlightOnLineWith = async (lineText: string) => {
    // the decoration renders as one empty overlay div per affected line – the
    // whole-grid bug painted every line, a cell's own source paints only its
    // row's line, vertically level with it
    const highlighted = page.locator(".ast-hover-highlight")
    await expect(highlighted).toHaveCount(1)
    const hl = (await highlighted.first().boundingBox())!
    const line = (await page.locator(".view-line", { hasText: lineText }).first().boundingBox())!
    expect(Math.abs(hl.y - line.y)).toBeLessThan(3)
  }
  // a value cell: hover its bottom edge, away from the value text, where only
  // the origin wrapper can catch it (16px in from the right: rounded-xl clips
  // hit-testing inside the 12px corner arc)
  const cell = page.getByTestId("print-panel").filter({ hasText: "42" }).last()
  const box = (await cell.boundingBox())!
  await page.mouse.move(box.x + box.width - 16, box.y + box.height - 4)
  await expectHighlightOnLineWith("20 + 22")
  // an AST-viz cell: hover the framed panel's padding, OUTSIDE the svg – the
  // quoted cell's backtick source should highlight from the whole cell extent
  const treePanel = panel(page).locator("svg.ast-tree").last().locator("..")
  const tbox = (await treePanel.boundingBox())!
  await page.mouse.move(tbox.x + tbox.width - 16, tbox.y + tbox.height - 4)
  await expectHighlightOnLineWith("8 ÷ 2")
})

test("hovering one AST graph doesn't tint the others", async ({ page }) => {
  // regression: hoverSubtree keys were bare grid coordinates, shared across
  // every tree on the page – hovering one graph lit the same cells everywhere
  await open(page, "(`1 + 2`, `3 + 4`)")
  const svgs = panel(page).locator("svg")
  await expect(svgs).toHaveCount(2)
  await svgs.first().locator("rect").first().hover()
  const lit = 'rect[stroke="rgba(255,255,255, 0.55)"]'
  await expect(svgs.first().locator(lit).first()).toBeVisible()
  await expect(svgs.nth(1).locator(lit)).toHaveCount(0)
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

test("?example= loads a gallery example by name; ⌘S switches to a ?code= URL", async ({ page }) => {
  await page.goto("/?example=combinators")
  await expect(panel(page)).toContainText("Queer bird")
  // saving replaces the example reference with a self-contained code URL
  await expect(async () => {
    await page.keyboard.press("ControlOrMeta+KeyS")
    expect(page.url()).toContain("code=")
  }).toPass({ timeout: 20_000 })
  expect(page.url()).not.toContain("example=")
})

test("?example= with an unknown name spells 404 in a binary tensor", async ({ page }) => {
  await page.goto("/?example=no-such-bird")
  await expect(page.getByText('example "no-such-bird" not found')).toBeVisible()
  // the 5×11 bit matrix renders as a heatmap in the output panel
  await expect(panel(page).locator(".js-plotly-plot, canvas").first()).toBeVisible()
})

test("printing a documented built-in shows its doc card, never JS internals", async ({ page }) => {
  await open(page, "sum")
  await expect(panel(page)).toContainText("Sum of the elements")
  await expect(panel(page)).toContainText("Σ(x, axis?)")
  await expect(panel(page)).not.toContainText("=>")   // no minified JS source
})

test("printing a Fluent lambda still shows its source", async ({ page }) => {
  await open(page, "double: { x | x × 2 },\ndouble")
  await expect(panel(page)).toContainText("x × 2")
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
edges: $({ abs(conv(mean(cam(), 2), k)) }),
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

test("PointPlot draws an x/y plot (custom x axis)", async ({ page }) => {
  await open(page, "PointPlot(2 ^ (0...7), [2.7, 1.98, 1.08, 0.82, 0.68, 0.62, 0.59, 0.57])")
  await expect(panel(page).locator(".js-plotly-plot").first()).toBeVisible({ timeout: 20_000 })
})

test("PointPlot surfaces an error argument, not a blank chart", async ({ page }) => {
  // `::` was removed; an unbound operator must show the error, not draw empty
  await open(page, "PointPlot(1 ..< 10, 1::10)")
  await expect(panel(page)).toContainText(/not a function/i)
})

test("Layers: a lower Point2D is draggable, not just the top one", async ({ page }) => {
  // regression: overlay wrappers were pointer-events-auto, so the topmost layer
  // swallowed every click and only its Point2D could be dragged. `a` is the
  // LOWER layer (b is on top); dragging a's dot must still move a.
  await open(page, "a: $([0.25, 0.25]), b: $([0.75, 0.75]), r: [[0, 1], [0, 1]], (Layers(fill([40, 40], 0), Point2D(a, r), Point2D(b, r)), a)")
  const box = panel(page).locator('[data-layers]').first()
  await expect(box).toBeVisible({ timeout: 20_000 })
  const bb = (await box.boundingBox())!
  await page.mouse.move(bb.x + bb.width * 0.25, bb.y + bb.height * 0.25)  // over a's dot
  await page.mouse.down()
  await page.mouse.move(bb.x + bb.width * 0.55, bb.y + bb.height * 0.55, { steps: 8 })
  await page.mouse.up()
  await expect(panel(page)).toContainText(/0\.5/)  // a moved off 0.25 toward the centre
})
