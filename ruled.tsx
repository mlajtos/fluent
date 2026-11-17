// Ruled paper: lines to write Fluent on with a pen. A line's ink is read as the pen leaves
// it – by the readers of this hand (ink/page.py, the dev server's /reading) – and the
// lines' texts, top to bottom, are one program. Over each glyph of ink stands the glyph it
// was read as, in print; ink and print have the colors the editor gives the text, and under
// a line is its value.
//
// The pen writes, and a scratch of it over ink erases the ink; a finger moves the paper, a
// tap of two takes the pen's last act back, and a finger on a value works it as in the
// playground. A mouse stands in for the pen, and its wheel for the finger.

import { signal, effect } from "@preact/signals-core"
import { useSignals } from "@preact/signals-react/runtime"
import { memo, useEffect, useRef, type JSX } from "react"
import { type Desc, root } from "./protocol"
import { BLANK, CELL, INK, PALM, PEN, PLAIN, REST, TAP, VALUE, type Line, type Look, type Stroke, Shown, Written, asked, boxOf, forming, hearing, pathOf, penned, problem, program, reading, record, shown, touched, under, worked } from "./ink"

const lines = signal<Line[]>([])
// how far the paper is moved up, in CSS pixels
const scrolled = signal(0)
// The line the pen is writing, until it has rested REST ms: a line half written is no
// mistake, so what is wrong with it is said when the pen rests. A value shows at once.
const writing = signal<number | null>(null)
// the paper's left margin, in CSS pixels
const MARGIN = 24
// the lines ruled under the last one with ink
const SPARE = 16
// the lines before each act of the pen, for `undo`
const before: Line[][] = []

// MARK: Ink

// A stroke the pen has finished on a line: a scratch takes the ink it covers and leaves
// none of its own, and any other stroke is ink of the line
const commit = (row: number, stroke: Stroke) => {
  const all = Array.from({ length: Math.max(lines.value.length, row + 1) }, (_, i) => lines.value[i] ?? BLANK)
  const line = all[row]!, gone = under({ width: PEN, points: stroke.points }, line.strokes)
  const strokes = gone.size > 0 ? line.strokes.filter((s) => !gone.has(s)) : [...line.strokes, stroke]
  all[row] = strokes.length > 0 ? { ...line, strokes } : BLANK
  before.push(lines.value)
  lines.value = all
  penned(stroke)
}

// Takes back the pen's last act: a stroke, or a scratch. A line whose readers had not
// answered is read again.
const undo = () => {
  const page = before.pop()
  if (!page) { return }
  for (const { strokes, read } of page) { if (read !== strokes) { asked.delete(strokes) } }
  lines.value = page
}

// MARK: The page as a file
// The dev server keeps the lines' ink (server.ts, /ruled); their texts are read again

const keep = () => {
  let kept: string | null = null, timer: ReturnType<typeof setTimeout> | undefined
  fetch("ruled").then((r) => r.json()).then((page: { lines?: Stroke[][] }) => {
    if (lines.value.length === 0 && page.lines) { lines.value = page.lines.map((strokes) => (strokes.length > 0 ? { ...BLANK, strokes } : BLANK)) }
    kept = JSON.stringify(page.lines ?? [])
  }).catch(() => { kept = "[]" })
  return effect(() => {
    const ink = lines.value.map(({ strokes }) => strokes)
    clearTimeout(timer)
    timer = setTimeout(() => {
      const body = JSON.stringify(ink.slice(0, ink.findLastIndex((strokes) => strokes.length > 0) + 1))
      if (kept === null || body === kept) { return }
      fetch("ruled", { method: "PUT", body: `{"lines":${body}}` }).then(() => { kept = body }).catch(() => { })
    }, 800)
  })
}

// MARK: Pen and fingers

const mount = (ground: HTMLDivElement) => {
  // the stroke under the pen: its line, where the line's baseline starts on the screen,
  // when the stroke began on the events' clock, and its ink on the page
  let wet: { pointer: number, row: number, left: number, base: number, since: number, stroke: Stroke, ink: SVGPathElement } | null = null
  const fingers = new Map<number, number>()
  // a tap: when its first finger landed, the most fingers down at once, and how far they moved
  let tap = { at: 0, most: 0, moved: 0 }
  // when the pen was last near: the hand that holds it rests on the glass, and moves nothing
  let pen = -Infinity
  let resting: ReturnType<typeof setTimeout> | undefined

  const hear = hearing("ruled")
  hear(new Event("open"), `${window.innerWidth}×${window.innerHeight}`)

  const move = (by: number) => { scrolled.value = Math.max(0, scrolled.value - by) }

  // a stroke is of the line whose band it starts in, or nearest to
  const lineAt = (y: number) => {
    let home: Element | undefined, far = Infinity
    for (const band of ground.querySelectorAll("[data-line]")) {
      const { top, bottom } = band.getBoundingClientRect(), off = Math.max(top - y, y - bottom, 0)
      if (off < far) { home = band; far = off }
    }
    return home
  }

  const take = (e: PointerEvent) => {
    if (!wet) { return }
    const { left, base } = wet
    record(e, wet.stroke, wet.since, (x, y) => [x - left, y - base])
    wet.ink.setAttribute("d", pathOf(wet.stroke.points))
  }

  const dry = () => {
    if (!wet) { return }
    const { row, stroke, ink } = wet
    wet = null
    forming(false)
    ink.remove()
    commit(row, stroke)
    resting = setTimeout(() => { writing.value = null }, REST)
  }

  const down = (e: PointerEvent) => {
    const value = (e.target as Element).closest("[data-value]")
    if (e.pointerType === "touch") {
      if (value || wet || performance.now() - pen < PALM) { return }
      if (fingers.size === 0) { tap = { at: performance.now(), most: 0, moved: 0 } }
      fingers.set(e.pointerId, e.clientY)
      tap.most = Math.max(tap.most, fingers.size)
      return
    }
    // the pen writes over a value, but for what it can work there; the mouse works a value as a finger does
    if (e.pointerType === "mouse" ? value || e.button !== 0 : worked(e.target)) { return }
    const home = lineAt(e.clientY)
    if (!home) { return }
    e.preventDefault()
    e.stopPropagation()
    // the fingers down when the pen lands are of the hand that holds it
    fingers.clear()
    // a stroke whose end was not heard ends where the next begins
    dry()
    ground.setPointerCapture(e.pointerId)
    const box = home.getBoundingClientRect()
    const ink = document.createElementNS("http://www.w3.org/2000/svg", "path")
    home.querySelector("[data-wet]")!.append(ink)
    wet = { pointer: e.pointerId, row: Number((home as HTMLElement).dataset.line), left: box.left, base: box.top + CELL.baseline, since: e.timeStamp, stroke: { at: Date.now(), points: [] }, ink }
    clearTimeout(resting)
    writing.value = wet.row
    forming(true)
    take(e)
    hear(e, `a stroke in line ${wet.row}`)
  }

  const moved = (e: PointerEvent) => {
    if (e.pointerType === "pen") { pen = performance.now() }
    if (wet?.pointer === e.pointerId) { take(e); return }
    const from = fingers.get(e.pointerId)
    if (from === undefined) { return }
    fingers.set(e.pointerId, e.clientY)
    tap.moved += Math.abs(e.clientY - from)
    // the paper moves with the middle of the fingers on it
    move((e.clientY - from) / fingers.size)
  }

  const up = (e: PointerEvent) => {
    if (wet?.pointer === e.pointerId) {
      take(e)
      hear(e, `${wet.stroke.points.length} points`)
      dry()
      return
    }
    if (e.pointerType === "pen") { hear(e, "nothing"); return }
    if (!fingers.delete(e.pointerId)) { return }
    if (e.type === "pointercancel") { tap.most = Infinity }
    if (fingers.size === 0 && tap.most === 2 && performance.now() - tap.at < TAP.ms && tap.moved < TAP.moved) { undo() }
  }

  const wheel = (e: WheelEvent) => { e.preventDefault(); move(-e.deltaY) }
  const touch = touched(hear)
  const still = (e: Event) => { e.preventDefault() }

  const stops = new AbortController(), heeded = { signal: stops.signal, passive: false }
  // the pen's stroke starts before anything under it hears of the pen
  ground.addEventListener("pointerdown", down, { ...heeded, capture: true })
  ground.addEventListener("pointermove", moved, heeded)
  ground.addEventListener("pointerup", up, heeded)
  ground.addEventListener("pointercancel", up, heeded)
  ground.addEventListener("wheel", wheel, heeded)
  ground.addEventListener("gesturestart", still, heeded)
  for (const type of ["touchstart", "touchmove", "touchend", "touchcancel"] as const) { ground.addEventListener(type, touch, heeded) }
  return () => { stops.abort() }
}

// MARK: Views

const Row = memo(function Row({ row, line, value, error, View, colorsOf }: { row: number, line: Line, value?: Desc, error?: string } & Look): JSX.Element {
  const left = line.strokes.length > 0 ? Math.max(0, boxOf(line.strokes).left) : 0
  return (
    <div>
      <svg data-line={row} width="100%" height={CELL.height} className="block overflow-visible">
        <g transform={`translate(0 ${CELL.baseline})`}>
          <line x1={-MARGIN} x2="100%" className="stroke-neutral-700" />
          <Written line={line} colorsOf={colorsOf} />
          <g data-wet stroke={PLAIN} {...INK} />
        </g>
      </svg>
      <div className="relative font-mono text-xl" style={{ paddingLeft: left, marginTop: VALUE.under - (CELL.height - CELL.baseline), minHeight: VALUE.room + VALUE.gap, paddingBottom: VALUE.gap }}>
        <Shown value={value} error={error} View={View} />
      </div>
    </div>
  )
})

export function Ruled({ View, colorsOf, run, parse }: Look & { run: (source: string) => void, parse: (source: string) => void }): JSX.Element {
  useSignals()
  const ground = useRef<HTMLDivElement>(null)
  useEffect(() => {
    // the program is the lines' texts, top to bottom; a line's number names its value
    const texts = () => lines.value.map(({ text }, id) => ({ id, text }))
    const stops = [mount(ground.current!), reading(lines), program(texts, run, parse), keep()]
    return () => { for (const stop of stops) { stop() } }
  }, [])
  const count = lines.value.findLastIndex(({ strokes }) => strokes.length > 0) + 1 + SPARE, whole = root.desc.value
  return (
    <div
      ref={ground} className="fixed inset-0 overflow-hidden bg-neutral-900 text-white select-none"
      style={{ touchAction: "none", WebkitUserSelect: "none", WebkitTouchCallout: "none" }}
    >
      <div className="absolute inset-x-0 top-0" style={{ paddingLeft: MARGIN, paddingTop: MARGIN, transform: `translateY(${-scrolled.value}px)` }}>
        {Array.from({ length: count }, (_, row) => {
          const line = lines.value[row] ?? BLANK
          return <Row key={row} row={row} line={line} {...shown(line, row, writing.value !== row)} View={View} colorsOf={colorsOf} />
        })}
      </div>
      {problem.value && <div className="absolute right-2 top-2 max-w-md text-red-500 text-xs">{problem.value}</div>}
      {/* a program that fails whole has no list to show: it says why */}
      {whole?.k === "error" && <div className="absolute left-2 bottom-2 max-w-md"><View d={whole} /></div>}
    </div>
  )
}
