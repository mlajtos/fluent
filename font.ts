// Write the font letters draws with – BQN386, the IDE's font – into prelude.fl, as outlines.
//   bun font.ts
// A glyph is its contours, read from the font file: lines as they are, and each quadratic
// curve cut into lines that stray from it at most TOL of the font's size. Its points are in
// thousandths of the size from the top left of its cell, y down, the baseline at the
// font's ascent. A point is four characters of DrawFontPoints, x then y, each two digits
// in base 92 from the character # on; a point that starts a contour has 4232 added to its
// x. DrawFontStarts holds where each glyph's points start, and where the last one's end;
// DrawFontCodes the characters the glyphs are for, in order.
const CODES = String.fromCodePoint(...Array.from({ length: 94 }, (_, i) => 33 + i)) + "·…−∞"
const TOL = 0.002

const font = new DataView(await Bun.file(`${import.meta.dir}/BQN386.ttf`).arrayBuffer())
const u16 = (o: number) => font.getUint16(o), i16 = (o: number) => font.getInt16(o)
const table: Record<string, number> = {}
for (let i = 0; i < u16(4); i++) table[String.fromCharCode(...[0, 1, 2, 3].map((k) => font.getUint8(12 + 16 * i + k)))] = font.getUint32(20 + 16 * i)
const { head, hhea, cmap, loca, glyf } = table as Record<"head" | "hhea" | "cmap" | "loca" | "glyf", number>
const em = u16(head + 18), ascent = i16(hhea + 4)
const at = (g: number) => i16(head + 50) ? font.getUint32(loca + 4 * g) : 2 * u16(loca + 2 * g)
// the glyph for a character, from the Windows Unicode map (format 4)
const map = Array.from({ length: u16(cmap + 2) }, (_, i) => cmap + font.getUint32(cmap + 8 + 8 * i))
  .find((o, i) => u16(cmap + 4 + 8 * i) === 3 && u16(o) === 4)!
const glyphOf = (c: number) => {
  const n = u16(map + 6) / 2
  for (let i = 0; i < n; i++) {
    if (c > u16(map + 14 + 2 * i)) continue
    const start = u16(map + 16 + 2 * n + 2 * i), delta = i16(map + 16 + 4 * n + 2 * i), range = map + 16 + 6 * n + 2 * i
    if (c < start) return 0
    const g = u16(range) ? u16(range + u16(range) + 2 * (c - start)) : c
    return g ? (g + delta) & 0xffff : 0
  }
  return 0
}
type Point = { x: number; y: number; on: boolean }
// a simple glyph's contours, in ems from the top left, y down
const contours = (g: number): Point[][] => {
  let o = glyf + at(g)
  if (at(g + 1) === at(g)) return []
  const n = i16(o)
  if (n < 0) throw new Error(`glyph ${g} is made of other glyphs`)
  const ends = Array.from({ length: n }, (_, i) => u16(o + 10 + 2 * i)), count = ends[n - 1]! + 1
  o += 12 + 2 * n + u16(o + 10 + 2 * n)
  const flags: number[] = []
  while (flags.length < count) {
    const f = font.getUint8(o++)
    flags.push(f)
    if (f & 8) for (let r = font.getUint8(o++); r > 0; r--) flags.push(f)
  }
  const coordinates = (short: number, same: number) => {
    let v = 0
    return flags.map((f) => {
      if (f & short) v += (f & same ? 1 : -1) * font.getUint8(o++)
      else if (!(f & same)) { v += i16(o); o += 2 }
      return v
    })
  }
  const xs = coordinates(2, 16), ys = coordinates(4, 32)
  const points = flags.map((f, i) => ({ x: xs[i]! / em, y: (ascent - ys[i]!) / em, on: (f & 1) === 1 }))
  return ends.map((e, i) => points.slice(i ? ends[i - 1]! + 1 : 0, e + 1))
}
// a contour as the points of its lines, closed: an off-curve point between two others makes
// a quadratic curve, and two off-curve points in a row have an on-curve point between them
const polygon = (c: Point[]): [number, number][] => {
  const mid = (a: Point, b: Point) => ({ x: (a.x + b.x) / 2, y: (a.y + b.y) / 2, on: true })
  const s = c.findIndex((q) => q.on)
  const ring = s < 0 ? [mid(c[0]!, c[1]!), ...c.slice(1), c[0]!] : [...c.slice(s), ...c.slice(0, s)]
  ring.push(ring[0]!)
  let from = ring[0]!
  const out: [number, number][] = [[from.x, from.y]]
  for (let i = 1; i < ring.length; i++) {
    const q = ring[i]!
    if (q.on) { out.push([q.x, q.y]); from = q; continue }
    const to = ring[i + 1]!.on ? ring[++i]! : mid(q, ring[i + 1]!)
    // a quadratic cut into k equal steps of t strays at most |from - 2q + to| ÷ 4k² from it
    const k = Math.ceil(Math.sqrt(Math.hypot(from.x - 2 * q.x + to.x, from.y - 2 * q.y + to.y) / (4 * TOL))) || 1
    for (let j = 1; j <= k; j++) {
      const t = j / k, a = (1 - t) ** 2, b = 2 * t * (1 - t), d = t * t
      out.push([a * from.x + b * q.x + d * to.x, a * from.y + b * q.y + d * to.y])
    }
    from = to
  }
  return out
}
const digits = (v: number) => String.fromCharCode(35 + Math.floor(v / 92), 35 + (v % 92))
let points = ""
const starts = [...CODES].map((ch) => {
  const start = points.length / 4
  for (const line of contours(glyphOf(ch.codePointAt(0)!)).map(polygon)) for (const [i, [x, y]] of line.entries()) {
    const fx = Math.round(x * 1000), fy = Math.round(y * 1000)
    if (fx < 0 || fx > 600 || fy < 0 || fy > 1200) throw new Error(`${ch} leaves its cell`)
    points += digits(fx + (i ? 0 : 4232)) + digits(fy)
  }
  return start
})
starts.push(points.length / 4)
const text = (s: string) => `"${s.replace(/[\\"]/g, "\\$&")}"`
const path = `${import.meta.dir}/prelude.fl`, prelude = await Bun.file(path).text()
const written = prelude
  .replace(/^DrawFontCodes: .*$/m, () => `DrawFontCodes: ${text(CODES)},`)
  .replace(/^DrawFontStarts: .*$/m, () => `DrawFontStarts: [${starts.join(", ")}],`)
  .replace(/^DrawFontPoints: .*$/m, () => `DrawFontPoints: ${text(points)},`)
await Bun.write(path, written)
const most = Math.max(...starts.slice(1).map((s, i) => s - starts[i]!))
console.log(`${CODES.length} glyphs, ${points.length / 4} points, at most ${most} a glyph; ${points.length} bytes`)
