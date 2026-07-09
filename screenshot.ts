// Regenerate the README hero image: `bun run hero` (runs under node).
//
// The poster is composed from real app pixels in two captures:
//   1. the code card – shot after Monaco's tokenizer lands, with the doc
//      card hovered open on HOVER_TOKEN
//   2. the background – the same program with its ⟳ count replaced by
//      FREEZE_AT, so the board halts at a photogenic generation
// Both are cropped and composed on a canvas at golden ratio, 2× DPI.
import { chromium } from "playwright"
import { spawn } from "node:child_process"
import { writeFile } from "node:fs/promises"

// ---- the hero program (this is what the README links to) ----
const CODE = `; Conway's Game of Life
box: { g, ax | roll(g, 1, ax) + g + roll(g, -1, ax) },
nbrs: { g | box(box(g, 0), 1) - g },
board: $(rand([192, 192]) < 0.3),
step: {
  c: once(board), a: nbrs(c),
  board ← ((a = 3) + (a = 2)×c),  ; born with 3, survives with 2
},
step ⟳ 100000,
board`
const HOVER_TOKEN = "roll"   // token whose doc card is shown; null for none
const FREEZE_AT = 120        // background stops at this generation
const OUT = "hero.jpg"
const W = 900, H = 556       // golden ratio; ~1:1 in GitHub's README column

const PORT = 3212
const encode = (s: string) =>
  Buffer.from(s).toString("base64").replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "")

// ---- own server, so this works regardless of the dev server ----
const server = spawn("bun", ["server.ts"], { env: { ...process.env, PORT: String(PORT) }, stdio: "pipe" })
await new Promise<void>((resolve, reject) => {
  server.stdout!.on("data", (d) => { if (String(d).includes("Listening")) { resolve() } })
  server.on("exit", () => reject(new Error("server died – is the port free?")))
  setTimeout(() => reject(new Error("server boot timeout")), 20000)
})

try {
  const browser = await chromium.launch()
  const url = (code: string) => `http://localhost:${PORT}/?code=${encode(code)}`
  console.log("hero program URL code:", encode(CODE))

  // ---- capture 1: the code, tokenized, with the hover card open ----
  const codePage = await browser.newPage({ viewport: { width: 1700, height: 1200 }, deviceScaleFactor: 2 })
  await codePage.goto(url(CODE))
  await codePage.waitForSelector(".monaco-editor .view-line", { timeout: 60000 })
  // fluent theme paints identifiers #30CD50 – tokenization has landed when one shows
  await codePage.waitForFunction(() =>
    [...document.querySelectorAll(".monaco-editor .view-line span span")]
      .some(s => getComputedStyle(s).color === "rgb(48, 205, 80)"), { timeout: 30000 })
  await codePage.waitForTimeout(500)
  await codePage.addStyleTag({ content: `
    .monaco-editor .view-overlays .current-line { border: none !important; background: transparent !important; }
    .monaco-editor .cursors-layer { display: none !important; }
  ` })

  const codeBox = (await codePage.locator(".monaco-editor .view-lines").first().boundingBox())!
  const maxLineRight = await codePage.evaluate(() => {
    let right = 0
    for (const l of document.querySelectorAll(".monaco-editor .view-line span")) {
      right = Math.max(right, l.getBoundingClientRect().right)
    }
    return right
  })
  codeBox.width = Math.min(codeBox.width, maxLineRight - codeBox.x + 6)
  const lineHeight = await codePage.evaluate(() =>
    document.querySelector(".monaco-editor .view-line")!.getBoundingClientRect().height)
  codeBox.height = Math.min(codeBox.height, lineHeight * CODE.split("\n").length + 4)

  if (HOVER_TOKEN) {
    const pos = await codePage.evaluate((token) => {
      for (const span of document.querySelectorAll(".monaco-editor .view-line span span")) {
        if (span.textContent === token) {
          const r = span.getBoundingClientRect()
          return { x: r.left + r.width / 2, y: r.top + r.height / 2 }
        }
      }
      return null
    }, HOVER_TOKEN)
    if (!pos) { throw new Error(`hover token '${HOVER_TOKEN}' not found`) }
    await codePage.mouse.move(pos.x - 30, pos.y, { steps: 4 })
    await codePage.mouse.move(pos.x, pos.y, { steps: 4 })
    await codePage.mouse.move(pos.x + 2, pos.y + 1)
    await codePage.waitForSelector(".monaco-hover:not(.hidden)", { state: "visible", timeout: 10000 })
    await codePage.waitForTimeout(900) // markdown + colorized example settle
    const hover = (await codePage.locator(".monaco-hover:not(.hidden)").first().boundingBox())!
    const ux = Math.min(codeBox.x, hover.x) - 4
    const uy = Math.min(codeBox.y, hover.y) + 3 // shave the editor's top hairline
    codeBox.width = Math.max(codeBox.x + codeBox.width, hover.x + hover.width) - ux + 8
    codeBox.height = Math.max(codeBox.y + codeBox.height, hover.y + hover.height) - uy + 8
    codeBox.x = ux
    codeBox.y = uy
  }
  const codeShot = (await codePage.screenshot()).toString("base64")
  await codePage.close()

  // ---- capture 2: the board, frozen at FREEZE_AT generations ----
  const boardPage = await browser.newPage({ viewport: { width: 1700, height: 1200 }, deviceScaleFactor: 2 })
  await boardPage.goto(url(CODE.replace("⟳ 100000", `⟳ ${FREEZE_AT}`)))
  await boardPage.locator("[data-testid=print-panel] canvas").first().waitFor({ timeout: 60000 })
  await boardPage.waitForTimeout(4000)
  const surfaceBox = (await boardPage.locator("[data-testid=print-panel] canvas").first().boundingBox())!
  const surfShot = (await boardPage.screenshot({ timeout: 15000 })).toString("base64")
  await boardPage.close()

  // ---- compose ----
  const compose = await browser.newPage({ viewport: { width: W, height: H }, deviceScaleFactor: 2 })
  await compose.setContent(`<html><body style="margin:0"><canvas id="c" width="${W * 2}" height="${H * 2}"></canvas></body></html>`)
  await compose.evaluate(async ({ codeShot, surfShot, surfaceBox, codeBox, W, H }) => {
    const load = (b64: string) => {
      const i = new Image()
      i.src = "data:image/png;base64," + b64
      return i.decode().then(() => i)
    }
    const [codeImg, surfImg] = await Promise.all([load(codeShot), load(surfShot)])
    const canvas = document.getElementById("c") as HTMLCanvasElement
    const ctx = canvas.getContext("2d")!
    const CW = W * 2, CH = H * 2

    // background, cover-cropped to the frame, cells kept crisp
    ctx.imageSmoothingEnabled = false
    const sx = surfaceBox.x * 2, sy = surfaceBox.y * 2, sw = surfaceBox.width * 2, sh = surfaceBox.height * 2
    const scale = Math.max(CW / sw, CH / sh)
    const cw = CW / scale, ch = CH / scale
    ctx.drawImage(surfImg, sx + (sw - cw) / 2, sy + (sh - ch) / 2, cw, ch, 0, 0, CW, CH)

    // darken toward the card so it reads
    const grad = ctx.createLinearGradient(0, 0, CW * 0.7, 0)
    grad.addColorStop(0, "rgba(8,8,12,0.55)")
    grad.addColorStop(1, "rgba(8,8,12,0)")
    ctx.fillStyle = grad
    ctx.fillRect(0, 0, CW, CH)

    // seamless code card – same surface as the editor background
    const pad = 48
    const cx2 = codeBox.x * 2, cy2 = codeBox.y * 2, cw2 = codeBox.width * 2, ch2 = codeBox.height * 2
    const cardW = cw2 + pad * 2, cardH = ch2 + pad * 2
    const cardX = 56, cardY = (CH - cardH) / 2
    ctx.save()
    ctx.shadowColor = "rgba(0,0,0,0.55)"
    ctx.shadowBlur = 60
    ctx.shadowOffsetY = 18
    ctx.fillStyle = "#1C1C1C"
    ctx.beginPath()
    ctx.roundRect(cardX, cardY, cardW, cardH, 22)
    ctx.fill()
    ctx.restore()
    ctx.imageSmoothingEnabled = true
    ctx.drawImage(codeImg, cx2, cy2, cw2, ch2, cardX + pad, cardY + pad, cw2, ch2)
  }, { codeShot, surfShot, surfaceBox, codeBox, W, H })

  const jpeg = await compose.evaluate(() =>
    (document.getElementById("c") as HTMLCanvasElement).toDataURL("image/jpeg", 0.93).split(",")[1])
  await writeFile(OUT, Buffer.from(jpeg!, "base64"))
  await browser.close()
  console.log(`saved ${OUT} – remember to update the README ?code= links if CODE changed`)
} finally {
  server.kill()
}
