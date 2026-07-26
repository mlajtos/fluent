// IDE tests: real Chromium against the real dev server (playwright.config.ts
// boots it on :3210). Covers what tests.ts structurally can't – Monaco, the
// output panel, reactive islands, canvas renderers, media sources.
// Run with `bun run test:browser`; `bunx playwright install chromium` once.
import { test, expect, type Page } from "@playwright/test"

// The dev server injects a <bun-hmr> status overlay that sits above the page
// and swallows pointer events once it has rendered. It is harness chrome, not
// app UI, so tests take it out of the hit-testing path.
const hideDevOverlay = (page: Page) =>
  page.addStyleTag({ content: "bun-hmr { display: none !important; pointer-events: none !important }" })

// mirror of client.tsx's StringSerialize (js-base64 encodeURI = unpadded base64url)
const open = async (page: Page, code: string) => {
  const response = await page.goto(`/?code=${Buffer.from(code).toString("base64url")}`)
  await hideDevOverlay(page)
  return response
}

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

test("a control bound to a ~ variable actually writes to it", async ({ page }) => {
  // regression: SignalUpdate returns an Error for a non-signal and
  // updateWithFresh discarded it, so Slider/Scrubber/Checkbox over a `~`
  // variable rendered fully interactive controls whose every write vanished.
  // Point2D and Trail already accepted both.
  await open(page, "θ: ~(0.25), (Slider(θ), watch(θ) × 100)")
  await expect(panel(page)).toContainText("25")
  await page.locator("input[type=range]").fill("0.75")
  await expect(panel(page)).toContainText("75")
})

test("a checkbox bound to a ~ variable writes to it", async ({ page }) => {
  await open(page, "on: ~(0), (Checkbox(on), watch(on) + 10)")
  await expect(panel(page)).toContainText("10")
  await page.locator("input[type=checkbox]").check()
  await expect(panel(page)).toContainText("11")
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

// ————————————————————————————————————————————————————————————————————
// Embedded editors, reactive cells, and the Tour – the IDE surface the
// tour build flushed out. Each test pins a bug that shipped once.

// type into the Monaco cell whose visible text contains `anchor`:
// select-all + Backspace first (insert-over-selection trips auto-surround),
// insertText (keystrokes trip auto-closing brackets), Escape (a lingering
// suggest widget swallows the next click)
const typeInCell = async (page: Page, anchor: string, code: string) => {
  await page.locator(".view-line", { hasText: anchor }).first().click()
  await page.keyboard.press("ControlOrMeta+KeyA")
  await page.keyboard.press("Backspace")
  await page.keyboard.insertText(code)
  await page.keyboard.press("Escape")
}

test("a reactive cell: lifts inside CodeEvaluate survive slider drags", async ({ page }) => {
  // regression: constants captured by a lift created inside another owned
  // computed were swept by the outer arena ("Referenced tracer Tensor
  // (disposed) freed" on replay; nested lifts errored immediately)
  await open(page, `x: $(0.5),\ncode: $("(Slider(x), 0 ... 100*x)"),\nCodeEvaluate(code)`)
  await expect(page.locator(".js-plotly-plot").first()).toBeVisible()
  await page.locator('input[type="range"]').first().fill("0.9")
  await expect(page.getByText(/disposed|freed|expected a tensor/)).toHaveCount(0)
  await expect(page.locator(".js-plotly-plot").first()).toBeVisible()
})

test("CodeEditor auto mode sizes to its content", async ({ page }) => {
  await open(page, `code: $("a: 1,\nb: 2,\nc: 3,\nd: 4,\na + b + c + d"),\n(CodeEditor(code, "auto"), CodeEvaluate(code))`)
  await expect(panel(page)).toContainText("10")
  const cell = page.locator("section", { hasText: "a: 1" }).first()
  // the fit is measured a frame after mount – poll rather than snapshot
  await expect.poll(async () => (await cell.boundingBox())!.height).toBeGreaterThan(90)
})

test("hover on a value highlights its source in the cell that produced it", async ({ page }) => {
  await open(page, `code: $("1 + 2*3"),\n(CodeEditor(code, "auto"), CodeEvaluate(code))`)
  await expect(panel(page)).toContainText("7")
  await page.getByText("7", { exact: true }).last().hover()
  const hl = page.locator(".ast-hover-highlight").first()
  await expect(hl).toBeVisible()
  expect(await hl.evaluate(el => (el.closest(".monaco-editor")?.textContent ?? "").includes("2*3"))).toBe(true)
})

test("hover docs show exactly one card no matter how many editors mounted", async ({ page }) => {
  // regression: providers registered per editor mount, stacking one more
  // identical doc card onto every hover
  await open(page, `a: $("sum([1, 2, 3])"), b: $("1 + 1"), c: $("2 + 2"),\n(CodeEditor(a, "auto"), CodeEditor(b, "auto"), CodeEditor(c, "auto"), CodeEvaluate(a))`)
  await expect(panel(page)).toContainText("6")
  await page.locator(".view-line", { hasText: "sum([1, 2, 3])" }).first()
    .locator("span", { hasText: /^sum$/ }).first().hover()
  await page.waitForTimeout(800)
  const cards = ((await page.evaluate(() => document.body.innerText)).match(/Sum of the elements/g) ?? []).length
  expect(cards).toBe(1)
})

test("swapping views does not recycle another view's mounted editor", async ({ page }) => {
  // regression: Grid cells keyed by index alone let React reconcile one
  // view's Monaco instance into the other's cell (stale onMount, height
  // signal cross-wired to the old cell)
  await open(page, [
    `step: $(0),`,
    `one: $("1 + 1"), many: $("v: [1, 2, 3],\nw: [4, 5, 6],\nv + w"),`,
    `roomA: Grid(1)(Text("room a"), CodeEditor(one, "auto"), Button("go", { step(1) })),`,
    `roomB: Grid(1)(Text("room b"), CodeEditor(many, "auto")),`,
    `$({ ListGet((roomA, roomB), step()) })`,
  ].join("\n"))
  await expect(panel(page).getByText("room a")).toBeVisible()
  await page.getByRole("button", { name: "go" }).click()
  await expect(panel(page).getByText("room b")).toBeVisible()
  const cell = panel(page).locator(".monaco-editor", { hasText: "v: [1, 2, 3]" }).first()
  await expect(cell).toBeVisible()
  await expect.poll(async () => (await cell.boundingBox())!.height).toBeGreaterThan(55)  // three lines, not room a's one
})

test("an overflowing panel scrolls instead of crushing its plots", async ({ page }) => {
  // regression: auto rows fell back to min-content on overflow, and a
  // scroll container's min-content height is ~zero – plots and button rows
  // were squeezed to 28px slivers
  await open(page, `(\nText("one"), 0 ... 8,\nText("two"), (1 ... 6) ⊗(×) (1 ... 6),\nText("three"), [0, 1, 4, 9, 16, 25],\nText("four"), [25, 16, 9, 4, 1, 0],\nText("five"), 8 ... 0\n)`)
  await expect(page.locator(".js-plotly-plot").first()).toBeVisible()
  for (const plot of await page.locator(".js-plotly-plot").all()) {
    const panelBox = await plot.evaluate(el => el.closest(".rounded-xl")!.getBoundingClientRect().height)
    expect(panelBox).toBeGreaterThan(100)
  }
})

test("prose links can navigate the playground", async ({ page }) => {
  await open(page, `Text("[open lenia](?example=lenia)")`)
  await page.getByRole("link", { name: "open lenia" }).click()
  await page.waitForURL(/example=lenia/)
  expect(page.url()).toContain("example=lenia")
})

test("REPL example: cells evaluate independently and size to content", async ({ page }) => {
  await page.goto("/?example=REPL")
  await expect(page.getByText("2", { exact: true }).first()).toBeVisible()
  await typeInCell(page, "1 + 1", "6 * 7")
  await expect(page.getByText("42", { exact: true })).toBeVisible()
  await expect(page.getByText("2", { exact: true }).first()).toBeVisible()  // siblings untouched
})

test("the Tour opens, checks a challenge, and rejects a cheat", async ({ page }) => {
  await open(page, "Tour")
  await expect(page.getByText("Reading order is meaning")).toBeVisible()
  await typeInCell(page, "make this say 7", "7")
  await expect(page.getByText("Seven.")).toHaveCount(0)   // literal answer must not pass
  await typeInCell(page, "7", "1 + 2*3")
  await expect(page.getByText("Seven.")).toBeVisible()
  // navigation reaches the tree room
  await page.keyboard.press("Escape")
  await page.getByRole("button", { name: "next ▸" }).click()
  await expect(page.locator(".ast-tree").first()).toBeVisible()
})

// A learner doing the whole Tour, room by room: solve every challenge, drive
// every control, and only ever advance with the header button. The per-room
// tests above pin one mechanism each; this one is the only thing that runs
// rooms 2-15 at all, and it is what catches a language change quietly
// breaking a room (a hyphenated binding froze navigation on room 1 with no
// console error, and 211/211 unit tests stayed green).
test("a learner walks the whole Tour, solving every room", async ({ page }) => {
  test.setTimeout(240_000)   // 15 rooms, two of them wall-clock training loops

  const next = async () => {
    await page.keyboard.press("Escape")   // a lingering suggest widget eats the click
    await page.getByRole("button", { name: "next ▸" }).click()
  }
  const room = (heading: string) => expect(page.getByText(heading)).toBeVisible()
  const solved = (done: string) => expect(page.getByText(done)).toBeVisible()

  await open(page, "Tour")

  // 1 · reading order – glue the * so it binds tighter than the spaced +
  await room("Reading order is meaning")
  await typeInCell(page, "make this say 7", "1 + 2*3")
  await solved("You just changed what a program means with a")
  await next()

  // 2 · see the shape – no challenge, the backtick literal must draw a tree
  await room("When in doubt, draw it")
  await expect(page.locator(".ast-tree").first()).toBeVisible()
  await next()

  // 3 · indexing
  await room("A list acts like one number")
  await typeInCell(page, "fish out the 30", "[10, 20, 30] _ 2")
  await solved("Third item, index 2")
  await next()

  // 4 · a run, doubled
  await room("Runs of numbers")
  await typeInCell(page, "turn this into", "1 ... 5 * 2")
  await solved("A run, times two")
  await next()

  // 5 · naming – the check insists on three bananas, not a typed 49
  await room("Name anything with")
  await typeInCell(page, "waste of good bananas", "🍌: 7, 🍌 * 🍌")
  await solved("One name, three bananas")
  await next()

  // 6 · three names for everything
  await room("Every built-in has three names")
  await typeInCell(page, "now sum 1 through 100", "sum(1 ... 100)")
  await solved("Gauss needed a clever trick")
  await next()

  // 7 · operators are functions – a name in operator position, no parens, no +
  await room("Operators aren")
  await typeInCell(page, "now put plus in the MIDDLE", "plus: add, 40 plus 2")
  await solved("You taught the language a word")
  await next()

  // 8 · your own function
  await room("Make your own function")
  await typeInCell(page, "almost. a cube is", "cube: { x | x^3 }, cube(3)")
  await solved("You fixed a function")
  await next()

  // 9 · reactivity – drag the slider past 0.9, no typing
  await room("Make it move")
  await page.locator("input[type=range]").first().fill("0.95")
  await solved("You changed a running program mid-flight")
  await next()

  // 10 · grad reads a function
  await room("The slope machine")
  await typeInCell(page, "the slope of x·x·x, at 2", "f: { x | x^3 }, df: grad(f), df(2)")
  await solved("You differentiated a program by editing a program")
  await next()

  // 11 · the slope, drawn – no challenge, both curves must plot
  await room("The slope, drawn")
  await expect(page.locator(".js-plotly-plot").first()).toBeVisible()
  await next()

  // 12 · find the bottom by feel – scrub the guess from 10 to 42.
  // Scrubber maps 0.1 of a unit per pixel of pointer travel, and it listens on
  // window, so the drag has to be real pointer events rather than a fill().
  await room("Find the bottom by feel")
  const scrubber = page.locator('span[style*="ew-resize"]').first()
  for (let i = 0; i < 8; i++) {
    const shown = Number((await scrubber.innerText()).replace(/_/g, ""))
    if (Math.abs(shown - 42) < 0.5) break
    // each press captures the value it started from, so a drag that runs out
    // of viewport just gets picked up by the next one
    const grip = (await scrubber.boundingBox())!
    const startX = grip.x + grip.width / 2, y = grip.y + grip.height / 2
    const headroom = page.viewportSize()!.width - startX - 8
    const dx = Math.max(-(startX - 8), Math.min(headroom, (42 - shown) * 10))
    await page.mouse.move(startX, y)
    await page.mouse.down()
    await page.mouse.move(startX + dx, y, { steps: 12 })
    await page.mouse.up()
  }
  await solved("Score 0, slope 0, guess 42")
  await next()

  // 13 · walk downhill – each press is one gradient step, g ← 0.4g + 25.2,
  // so it lands inside 1 of 42 in seven presses; press a few extra and stop
  // as soon as the light turns.
  await room("Walk downhill")
  const arrived = page.getByText("That graph is gradient descent")
  for (let i = 0; i < 14 && !(await arrived.isVisible()); i++) {
    await page.getByRole("button", { name: "step downhill" }).click()
  }
  await expect(arrived).toBeVisible()
  await next()

  // 14 · let it run – nothing to press, sgd finds √42 on its own
  await room("Let it run")
  await expect(page.getByText("That is the square root of 42")).toBeVisible({ timeout: 120_000 })
  await next()

  // 15 · the door out – the final header swaps next ▸ for start over ↺
  await room("It keeps going")
  await expect(page.getByRole("button", { name: "start over ↺" })).toBeVisible()
  await expect(page.getByRole("button", { name: "next ▸" })).toHaveCount(0)
  await page.getByRole("button", { name: "start over ↺" }).click()
  await room("Reading order is meaning")
})

test("Center centers its child in the cell", async ({ page }) => {
  await open(page, `Grid([1, 2, 1])(Button("l"), Center(Text("mid")), Button("r"))`)
  const label = panel(page).getByText("mid")
  await expect(label).toBeVisible()
  const box = (await label.boundingBox())!
  const cell = (await panel(page).locator(".place-items-center").first().boundingBox())!
  const labelCenter = box.x + box.width / 2
  const cellCenter = cell.x + cell.width / 2
  expect(Math.abs(labelCenter - cellCenter)).toBeLessThan(8)
})

test("global shortcuts survive transient cell editors", async ({ page }) => {
  // regression: the first-mounted editor claimed the global-shortcut target;
  // in the Tour that is a room's cell editor, which unmounts on navigation –
  // ⌘O and ⌘S then pointed at a disposed editor
  await open(page, "Tour")
  await expect(page.getByText("Reading order is meaning")).toBeVisible()
  await page.keyboard.press("Escape")
  await page.getByRole("button", { name: "next ▸" }).click()
  await expect(page.locator(".ast-tree").first()).toBeVisible()   // room 1 cells are gone now
  await page.keyboard.press("ControlOrMeta+KeyO")
  await expect(page.locator(".quick-input-widget")).toBeVisible() // the gallery picker
})

test("quoted-code trees are not clipped at their edges", async ({ page }) => {
  // regression: the boundary nodes' rect strokes are centered on the svg
  // viewport edge, and the default overflow:hidden ate the outer half – the
  // rightmost node rendered with its border chopped off
  await open(page, "`(f ∘ g) ⍨ x`")
  const svg = page.locator("svg.ast-tree").first()
  await expect(svg).toBeVisible()
  const verdict = await svg.evaluate((el) => {
    if (getComputedStyle(el).overflow !== "visible") { return "svg clips its drawing" }
    const box = el.getBoundingClientRect()
    for (const r of el.querySelectorAll("rect")) {
      const rr = r.getBoundingClientRect()
      if (rr.right > box.right - 1) { return "a node sits on the viewport edge" }
    }
    return "ok"
  })
  expect(verdict).toBe("ok")
})
