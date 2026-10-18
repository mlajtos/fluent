// Write the font letters draws with – BQN386, the IDE's font – into prelude.fl, as distances.
//   bun font.ts
// Headless Chrome renders each glyph 250 pixels to the font's size; a sample is kept every
// 15 of them, every 0.06 of the size, and holds the distance from its center to the glyph's
// edge on that render. A glyph is a cell of 10 × 20 samples, the line's top first; a sample
// is one character of DrawFontField, its code less 80 the distance in 200ths of the size,
// negative inside. DrawFontCodes holds the characters the glyphs are for, in order.
import { chromium } from "playwright"

const CODES = String.fromCodePoint(...Array.from({ length: 94 }, (_, i) => 33 + i)) + "·…−∞"
const SIZE = 250, STEP = 15

const browser = await chromium.launch({ channel: "chrome", headless: true })
const page = await browser.newPage()
await page.setContent("<!doctype html><meta charset=utf-8>")
const font = Buffer.from(await Bun.file(`${import.meta.dir}/BQN386.ttf`).arrayBuffer()).toString("base64")
// each glyph's coverage, 0–255, in a cell a character wide and whole samples tall
const { w, h, glyphs } = await page.evaluate(async ({ font, codes, SIZE, STEP }) => {
  const face = new FontFace("BQN386", Uint8Array.from(atob(font), (c) => c.charCodeAt(0)).buffer)
  document.fonts.add(await face.load())
  const c = new OffscreenCanvas(1, 1).getContext("2d", { willReadFrequently: true })!
  c.font = `${SIZE}px BQN386`
  const m = c.measureText("0"), w = Math.round(m.width)
  const h = Math.ceil((m.fontBoundingBoxAscent + m.fontBoundingBoxDescent) / STEP) * STEP
  c.canvas.width = w; c.canvas.height = h
  const glyphs = [...codes].map((ch) => {
    c.clearRect(0, 0, w, h); c.font = `${SIZE}px BQN386`; c.fillStyle = "#fff"
    c.fillText(ch, 0, m.fontBoundingBoxAscent)
    return Array.from(c.getImageData(0, 0, w, h).data.filter((_, i) => i % 4 === 3))
  })
  return { w, h, glyphs }
}, { font, codes: CODES, SIZE, STEP })
await browser.close()
if (w % STEP) throw new Error(`a character ${w} pixels wide is not whole samples`)

// squared distance from each pixel to the nearest one that is on (Felzenszwalb–Huttenlocher)
const line = (f: Float64Array) => {
  const n = f.length, d = new Float64Array(n), v = new Int32Array(n), z = new Float64Array(n + 1)
  const cross = (q: number, p: number) => (f[q]! + q * q - f[p]! - p * p) / (2 * q - 2 * p)
  let k = 0; z[0] = -Infinity; z[1] = Infinity
  for (let q = 1; q < n; q++) {
    let s = cross(q, v[k]!)
    while (s <= z[k]!) s = cross(q, v[--k]!)
    v[++k] = q; z[k] = s; z[k + 1] = Infinity
  }
  for (let q = 0, j = 0; q < n; q++) { while (z[j + 1]! < q) j++; d[q] = (q - v[j]!) ** 2 + f[v[j]!]! }
  return d
}
const distance = (on: boolean[]) => {
  const g = Float64Array.from(on, (o) => o ? 0 : 1e20)
  for (let x = 0; x < w; x++) line(Float64Array.from({ length: h }, (_, y) => g[y * w + x]!)).forEach((d, y) => { g[y * w + x] = d })
  for (let y = 0; y < h; y++) g.set(line(g.slice(y * w, y * w + w)), y * w)
  return g.map(Math.sqrt)
}
// each sample: the signed distance at its center pixel, its edge half a pixel from it, as a code
let field = ""
for (const alpha of glyphs) {
  const inside = alpha.map((a) => a >= 128), out = distance(inside), into = distance(inside.map((i) => !i))
  for (let y = (STEP - 1) / 2; y < h; y += STEP) for (let x = (STEP - 1) / 2; x < w; x += STEP) {
    const i = y * w + x, d = (inside[i] ? 0.5 - into[i]! : out[i]! - 0.5) / SIZE
    field += String.fromCharCode(Math.max(35, Math.min(126, 80 + Math.round(d * 200))))
  }
}
const text = (s: string) => `"${s.replace(/[\\"]/g, "\\$&")}"`
const path = `${import.meta.dir}/prelude.fl`, prelude = await Bun.file(path).text()
const written = prelude
  .replace(/^DrawFontCodes: .*$/m, () => `DrawFontCodes: ${text(CODES)},`)
  .replace(/^DrawFontField: .*$/m, () => `DrawFontField: ${text(field)},`)
await Bun.write(path, written)
console.log(`${[...CODES].length} glyphs of ${w / STEP} × ${h / STEP} samples, ${field.length} bytes`)
