// Ink: what the pen's pages – the copybook, the ruled paper, the canvas – have in common.
// The pen's points and strokes and the cell they are written in; a line of ink and the
// readers that read it (ink/page.py, the dev server's /reading); the scratch that erases;
// the form that read ink goes to; the pen's points, its record and its touches; the program
// the lines' texts are; and the print over a line's ink with the value under it.

import { type Signal, signal, effect } from "@preact/signals-core"
import { useEffect, useRef, type JSX } from "react"
import { type Desc, type ToPage, documentHash, stringRegexp, root } from "./protocol"

// MARK: Ink

// A point of a cell: x from the cell's left edge and y from its baseline, down, in CSS
// pixels; the pen's pressure there, 0 to 1; when, in ms after the stroke began; and how the
// pen leaned – its altitude above the glass and its azimuth on it, in radians
export type Point = [x: number, y: number, pressure: number, ms: number, altitude: number, azimuth: number]
// `at` is when the stroke began, in ms of the epoch
export type Stroke = { at: number, points: Point[] }

// a cell, in CSS pixels: this writer's hand is about 40 tall. A glyph's cell is no wider
// than `width`, and a line's is as wide as the page
export const CELL = { width: 64, height: 96, baseline: 62 }
// the ink's width, in CSS pixels
export const PEN = 2.4
// a dot is a stroke of one point
export const pathOf = (points: readonly (readonly [x: number, y: number, ...rest: number[]])[]) => `M${points.map(([x, y]) => `${x} ${y}`).join("L")}${points.length === 1 ? `L${points[0]![0]} ${points[0]![1]}` : ""}`

// a point of ink, whatever else the pen told of it
type XY = readonly [x: number, y: number, ...rest: number[]]
type Box = { left: number, top: number, right: number, bottom: number }
export const boxOf = (strokes: { points: XY[] }[]): Box => {
  const box = { left: Infinity, top: Infinity, right: -Infinity, bottom: -Infinity }
  for (const { points } of strokes) {
    for (const [x, y] of points) {
      box.left = Math.min(box.left, x); box.right = Math.max(box.right, x)
      box.top = Math.min(box.top, y); box.bottom = Math.max(box.bottom, y)
    }
  }
  return box
}

// MARK: Lines and their readers

// A line of ink a page has: its ink is as the copybook's lines are, a point's x from the
// line's left and its y from the baseline. `strokes` are as the pen made them, and `text`, `of`,
// `sure` and `to` are what the readers made of the strokes `read`: the line's text; of each
// stroke which glyph of the text it is of; of each glyph how sure the readers are, 0 to 1;
// and the stroke each stroke goes to in the hand's form – none for a glyph written in
// another number of strokes than its form has. The readers read the ink as it stands,
// whenever the pen made each stroke.
export type Line = { strokes: Stroke[], read: Stroke[], text: string, of: number[], sure: number[], to: (XY[] | null)[] }
const none: Stroke[] = []
export const BLANK: Line = { strokes: none, read: none, text: "", of: [], sure: [], to: [] }

// why the ink is not read
export const problem = signal("")
// the strokes the readers were asked to read
export const asked = new WeakSet<Stroke[]>()

// Every line of `lines` whose ink is not the ink its text is of goes to the readers, once,
// as the points of its strokes. An answer is for the strokes that were sent: a line that
// has others by then waits for its own.
const ask = async <L extends Line>(lines: Signal<L[]>, strokes: Stroke[]) => {
  asked.add(strokes)
  try {
    const response = await fetch("reading", { method: "POST", body: JSON.stringify(strokes.map(({ points }) => points.map(([x, y]) => [x, y]))) })
    if (!response.ok) { throw new Error(await response.text()) }
    const { text, of, sure, to } = await response.json() as Pick<Line, "text" | "of" | "sure" | "to">
    problem.value = ""
    lines.value = lines.value.map((line) => (line.strokes === strokes ? { ...line, read: strokes, text, of, sure, to } : line))
  } catch (e) {
    asked.delete(strokes)
    problem.value = `the ink is not read: ${String((e as Error).message ?? e)}`
  }
}
export const reading = <L extends Line>(lines: Signal<L[]>) => {
  const all = () => { for (const { strokes, read } of lines.value) { if (read !== strokes && !asked.has(strokes)) { ask(lines, strokes) } } }
  // the readers start as the page opens, and a line they did not answer is sent again
  fetch("reading", { method: "POST", body: "[]" }).catch(() => { })
  const stop = effect(all), again = setInterval(all, 3000)
  return () => { stop(); clearInterval(again) }
}

// MARK: Scratch

const cross = (o: XY, a: XY, b: XY) => (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])

// the convex hull of points, its corners in one sense of rotation
const hullOf = (points: XY[]): XY[] => {
  const sorted = [...points].sort((a, b) => a[0] - b[0] || a[1] - b[1])
  const half = (ps: XY[]) => {
    const out: XY[] = []
    for (const p of ps) {
      while (out.length > 1 && cross(out.at(-2)!, out.at(-1)!, p) <= 0) { out.pop() }
      out.push(p)
    }
    return out.slice(0, -1)
  }
  return [...half(sorted), ...half(sorted.reverse())]
}

// how many times a stroke turns back along an axis: a way back counts from a quarter of
// the stroke's extent along it
const turns = (stroke: { width: number, points: XY[] }, axis: 0 | 1): number => {
  const box = boxOf([stroke]), extent = axis === 0 ? box.right - box.left : box.bottom - box.top
  const least = Math.max(0.25 * extent, 2 * stroke.width)
  let count = 0, way = 0, far = stroke.points[0]![axis]
  for (const point of stroke.points) {
    const at = point[axis]
    if (way === 0) {
      if (Math.abs(at - far) > least) { way = Math.sign(at - far); far = at }
    } else if ((at - far) * way > 0) {
      far = at
    } else if ((far - at) * way > least) {
      way = -way; far = at; count++
    }
  }
  return count
}

// Scaffolding, to be learned: a stroke is a scratch, where it covers ink, when it goes back
// and forth – it turns back four times or more along an axis; or twice, with ink SCRIBBLE
// times as long as its box is across, or longer, as a loose scribble over much ink is. The
// ink a scratch covers is every stroke with half of its points within the scratch's hull.
const SCRIBBLE = 3.8
export const under = <S extends { points: XY[] }>(scratch: { width: number, points: XY[] }, strokes: S[]): Set<S> => {
  const gone = new Set<S>(), box = boxOf([scratch]), slack = 2 * scratch.width, back = Math.max(turns(scratch, 0), turns(scratch, 1))
  const long = scratch.points.reduce((sum, [x, y], i, all) => (i > 0 ? sum + Math.hypot(x - all[i - 1]![0], y - all[i - 1]![1]) : 0), 0)
  if (back < 2 || (back < 4 && long < SCRIBBLE * Math.max(box.right - box.left, box.bottom - box.top))) { return gone }
  const hull = hullOf(scratch.points)
  const within = (p: XY) => p[0] >= box.left - slack && p[0] <= box.right + slack && p[1] >= box.top - slack && p[1] <= box.bottom + slack
    && hull.every((a, i) => {
      const b = hull[(i + 1) % hull.length]!
      return cross(a, b, p) >= -slack * Math.hypot(b[0] - a[0], b[1] - a[1])
    })
  for (const stroke of strokes) {
    if (stroke.points.filter(within).length * 2 >= stroke.points.length) { gone.add(stroke) }
  }
  return gone
}

// MARK: Form
// Ink the readers have read goes to the hand's form: each point of a stroke to the point as
// far along the form's stroke. It goes while the pen writes, and slowly – what is left of
// the way falls to 1/e in EASE ms of writing – so that no eye is on it; and the glyph the
// pen wrote last stays as it is, for it may not be done. What the readers read, and what a
// scratch erases, is the ink as the pen made it.

const EASE = 2000
// a stroke's points as the page shows them, once they have left the pen's
const shownAs = new WeakMap<Stroke, [number, number][]>()
// the ink on the page: of each stroke its glyph's strokes, and where its points go
const drawn = new Map<SVGPathElement, { stroke: Stroke, glyph: Stroke[], aim: XY[] }>()
// the stroke the pen made last
let fresh: Stroke | null = null
export const penned = (stroke: Stroke) => { fresh = stroke }

// where each point of a stroke goes: as far along `to` as it is along its own stroke
const aims = new WeakMap<Stroke, { to: XY[], aim: XY[] }>()
const aimOf = (stroke: Stroke, to: XY[] | null): XY[] => {
  if (!to) { return stroke.points }
  const kept = aims.get(stroke)
  if (kept?.to === to) { return kept.aim }
  const { points } = stroke, along = [0]
  for (let i = 1; i < points.length; i++) { along.push(along[i - 1]! + Math.hypot(points[i]![0] - points[i - 1]![0], points[i]![1] - points[i - 1]![1])) }
  const aim = along.map((far): XY => {
    const at = far / (along.at(-1) || 1) * (to.length - 1), a = to[Math.floor(at)]!, b = to[Math.ceil(at)]!, part = at - Math.floor(at)
    return [a[0] + part * (b[0] - a[0]), a[1] + part * (b[1] - a[1])]
  })
  aims.set(stroke, { to, aim })
  return aim
}

let turn = 0, last = 0
const step = (now: number) => {
  const part = 1 - Math.exp(-(now - last) / EASE)
  last = now
  for (const [path, { stroke, glyph, aim }] of drawn) {
    if (glyph.includes(fresh!) || (aim === stroke.points && !shownAs.has(stroke))) { continue }
    const at = shownAs.get(stroke) ?? stroke.points.map(([x, y]): [number, number] => [x, y])
    let far = 0
    for (const [i, point] of at.entries()) {
      const dx = aim[i]![0] - point[0], dy = aim[i]![1] - point[1]
      far = Math.max(far, Math.abs(dx), Math.abs(dy))
      point[0] += part * dx
      point[1] += part * dy
    }
    if (far < 0.05) { continue }
    shownAs.set(stroke, at)
    path.setAttribute("d", pathOf(at))
  }
}
// a slow way needs few steps a second
const form = (now: number) => {
  turn = requestAnimationFrame(form)
  if (now - last >= 50) { step(now) }
}
// the ink goes to its form from when a stroke of the pen starts until it ends
export const forming = (on: boolean) => {
  cancelAnimationFrame(turn)
  if (turn && !on) { step(performance.now()) }
  last = performance.now()
  turn = on ? requestAnimationFrame(form) : 0
}

// MARK: Pen and fingers

// a finger that lands this soon after the pen was near is the writing hand's, in ms
export const PALM = 300
// a tap is this short, in ms, and moves this little, in CSS pixels
export const TAP = { ms: 350, moved: 16 }
// what is wrong with a line the pen is writing is said when the pen has rested this long, in ms
export const REST = 700

const round = (n: number, digits: number) => Math.round(n * 10 ** digits) / 10 ** digits

// What a finger works in a value – an input, a button, or what shows by its cursor that it
// can be held – the pen works too, and writes nothing there
export const worked = (target: EventTarget | null) => {
  const at = target as Element
  if (!at.closest("[data-value]")) { return false }
  return !!at.closest("input, button, select, textarea") || !["auto", "default", "text"].includes(getComputedStyle(at).cursor)
}

// What the pen did on the page `on` and what the page made of it goes to the server a
// second after the last of it (/heard, paper/pen.jsonl): the record to read when a stroke
// is lost
export const hearing = (on: string) => {
  const heard: unknown[] = []
  let telling: ReturnType<typeof setTimeout> | undefined
  return (e: Event, made: string) => {
    if ((e as PointerEvent).pointerType === "mouse") { return }
    heard.push({ at: Math.round(performance.timeOrigin + e.timeStamp), on, type: e.type, made })
    clearTimeout(telling)
    telling = setTimeout(() => { fetch("heard", { method: "POST", body: JSON.stringify(heard.splice(0)) }).catch(() => { }) }, 1000)
  }
}

// The points the pen tells of go to its stroke, which began `since` on the events' clock;
// `place` is where a point of the screen is in the stroke's line. The pen reports more often
// than the page hears of it, and Safari tells of the same reports twice: a point is one
// later than the last.
export const record = (e: PointerEvent, { points }: Stroke, since: number, place: (x: number, y: number) => [number, number]) => {
  for (const event of [...(e.getCoalescedEvents?.() ?? []), e]) {
    const ms = round(event.timeStamp - since, 1)
    if (points.length > 0 && ms <= points.at(-1)![3]) { continue }
    const lean = event as PointerEvent & { altitudeAngle?: number, azimuthAngle?: number }, [x, y] = place(event.clientX, event.clientY)
    points.push([
      round(x, 2), round(y, 2), event.pointerType === "pen" ? round(event.pressure, 3) : 0.5, ms,
      round(lean.altitudeAngle ?? Math.PI / 2, 3), round(lean.azimuthAngle ?? 0, 3),
    ])
  }
}

// A page takes a touch of the pen that writes, and a finger's off the values: Safari makes
// no gesture of its own of a touch that is taken
export const touched = (hear: (e: Event, made: string) => void) => (e: TouchEvent) => {
  const byPen = [...e.changedTouches].some((t) => (t as Touch & { touchType?: string }).touchType === "stylus")
  if (byPen && e.type !== "touchmove") { hear(e, "") }
  if (byPen ? !worked(e.target) : !(e.target as Element).closest("[data-value]")) { e.preventDefault() }
}

// MARK: Program

// the value each text of the program shows now, by its id
export const values = signal(new Map<number, Desc>())
// a text's parse error, or null when it parses, by the text's hash
export const verdicts = signal(new Map<number, string | null>())

// A text without its strings and comments, which hold no names and no statements
const bare = (text: string) => text.replace(new RegExp(stringRegexp.source, "g"), '""').replace(/;[^\n]*/g, "")

// the statements of a text: one, and one more for each comma outside brackets
const statements = (text: string): number => {
  let depth = 0, count = 1
  for (const c of bare(text)) {
    if ("([{".includes(c)) { depth++ } else if (")]}".includes(c)) { depth-- } else if (c === "," && depth === 0) { count++ }
  }
  return count
}

// What each program sent shows where: its texts in order, and how many of the list's
// values each one gives. A program's last value is its number, so a result names the
// layout it was computed for.
const layouts = new Map<number, { id: number, count: number }[]>()
let serial = 0, sent = ""

export const parsed = (m: Extract<ToPage, { t: "parsed" }>) => {
  verdicts.value = new Map(verdicts.value).set(m.d, m.errors[0]?.message ?? null)
}

// The program of a page's texts: those that parse and say something – a comment alone
// says nothing – are one list, in the order the page has them, and `values` has what each
// shows
export const program = (texts: () => { id: number, text: string }[], run: (source: string) => void, parse: (source: string) => void) => {
  const asked = new Set<number>()
  const stops = [
    // every text is parsed on its own: one that fails stays out of the program
    effect(() => {
      for (const { text } of texts()) {
        const hash = documentHash(text)
        if (text.trim() && !asked.has(hash)) { asked.add(hash); parse(text) }
      }
    }),
    effect(() => {
      const runs = texts().filter(({ text }) => bare(text).trim() && verdicts.value.get(documentHash(text)) === null)
      const key = runs.map(({ id, text }) => `${id} ${text}`).join("\n")
      if (key === sent && serial > 0) { return }
      sent = key
      layouts.set(++serial, runs.map(({ id, text }) => ({ id, count: statements(text) })))
      // a comma has a line of its own: a comment ends with its text's line
      run(`(\n${[...runs.map(({ text }) => text), `"${serial}"`].join("\n,\n")}\n)`)
    }),
    effect(() => {
      const d = root.desc.value, shown = new Map<number, Desc>()
      const results = d?.k === "list" ? d.items : d ? [d] : []
      const mark = results.at(-1), layout = mark?.k === "string" ? layouts.get(Number(mark.v)) : undefined
      if (layout) {
        let at = 0
        for (const { id, count } of layout) { at += count; shown.set(id, results[at - 1]!) }
        for (const older of layouts.keys()) { if (older < Number((mark as { v: string }).v)) { layouts.delete(older) } }
      }
      values.value = shown
    }),
  ]
  return () => { for (const stop of stops) { stop() } }
}

// MARK: Views

// ink the readers have not read, and a value's text
export const PLAIN = "#D8D8D8"

export type Look = { View: (p: { d: Desc }) => JSX.Element, colorsOf: (text: string) => string[] }

export const INK = { fill: "none", strokeWidth: PEN, strokeLinecap: "round", strokeLinejoin: "round" } as const
// The print over the ink: its size and how far over the baseline it stands, in CSS pixels,
// and how faint it is, for the eye to stay on the ink. A glyph the readers are less sure of
// than DOUBT has a short stroke under its print: about one glyph in twenty, and nearly half
// of those they misread.
const PRINT = { size: 16, over: CELL.baseline - 16, faint: 0.5 }
const DOUBT = 0.7
// A line's value is nearer to its own rule than to the next line's print: it starts this
// far under the rule, has this much room, and the next line starts this far under it
export const VALUE = { under: 20, room: 28, gap: 22 }

// What the readers made of a line, glyph by glyph: the glyph, its color, whether a space
// is before it, the strokes it is, and whether the readers doubt it
const glyphsOf = ({ read, text, of, sure }: Line, colorsOf: Look["colorsOf"]) => {
  const colors = colorsOf(text), found: { glyph: string, color: string, spaced: boolean, strokes: Stroke[], doubted: boolean }[] = []
  let at = 0, spaced = false
  for (const glyph of text) {
    if (glyph !== " ") { found.push({ glyph, color: colors[at] ?? PLAIN, spaced, strokes: read.filter((_, stroke) => of[stroke] === found.length), doubted: (sure[found.length] ?? 1) < DOUBT }) }
    spaced = glyph === " "
    at += glyph.length
  }
  return found
}

// a stroke on the page: as the pen made it, or as far to its form as it has gone
function Ink({ stroke, glyph, aim, color }: { stroke: Stroke, glyph: Stroke[], aim: XY[], color: string }): JSX.Element {
  const path = useRef<SVGPathElement>(null)
  useEffect(() => {
    const ink = path.current!
    drawn.set(ink, { stroke, glyph, aim })
    return () => { drawn.delete(ink) }
  }, [stroke, glyph, aim])
  return <path ref={path} d={pathOf(shownAs.get(stroke) ?? stroke.points)} stroke={color} {...INK} />
}

// a line's print and its ink, about the start of its baseline
export function Written({ line, colorsOf }: { line: Line, colorsOf: Look["colorsOf"] }): JSX.Element {
  const glyphs = glyphsOf(line, colorsOf), boxes = glyphs.map(({ strokes }) => boxOf(strokes))
  const read = new Map(glyphs.flatMap((glyph) => glyph.strokes.map((stroke) => [stroke, glyph] as const)))
  return (
    <>
      <g className="font-mono" textAnchor="middle" fontSize={PRINT.size} opacity={PRINT.faint}>
        {glyphs.map(({ glyph, color, spaced, doubted }, i) => {
          const box = boxes[i]!, gap = i > 0 && spaced ? (boxes[i - 1]!.right + box.left) / 2 : null, middle = (box.left + box.right) / 2
          return (
            <g key={i}>
              {gap !== null && <text x={gap} y={-PRINT.over} className="fill-neutral-600">␣</text>}
              <text x={middle} y={-PRINT.over} fill={color}>{glyph}</text>
              {doubted && <line x1={middle - 5} x2={middle + 5} y1={4 - PRINT.over} y2={4 - PRINT.over} stroke={color} strokeWidth={1.5} />}
            </g>
          )
        })}
      </g>
      {line.strokes.map((stroke) => {
        const glyph = read.get(stroke)
        return <Ink key={stroke.at} stroke={stroke} glyph={glyph?.strokes ?? none} aim={aimOf(stroke, glyph ? line.to[line.read.indexOf(stroke)] ?? null : null)} color={glyph?.color ?? PLAIN} />
      })}
    </>
  )
}

// What a line shows under its rule: what is wrong with its text, and its value. A line the
// pen is writing – not `calm` – says nothing of what is wrong yet.
export const shown = ({ text }: Line, id: number, calm: boolean): { value?: Desc, error?: string } => {
  const value = values.value.get(id)
  return { value: calm || value?.k !== "error" ? value : undefined, error: calm && text.trim() ? verdicts.value.get(documentHash(text)) ?? undefined : undefined }
}
export function Shown({ value, error, View }: { value?: Desc, error?: string, View: Look["View"] }): JSX.Element {
  return (
    <>
      {error && <div className="text-red-500 text-xs whitespace-normal max-w-96">{error}</div>}
      {value && <div data-value className="inline-block align-top" style={{ minWidth: value.k === "tensor" && value.shape.length === 0 ? undefined : 256 }}><View d={value} /></div>}
    </>
  )
}
