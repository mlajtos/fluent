// Copybook: pages to write the pen's hand in. A row is a text – a glyph of the alphabet, or
// a line of Fluent – in print, with cells on one baseline, and what the pen writes in a
// cell is a sample of the writer's hand for that text. The dev server keeps the samples
// (server.ts, /copybook): they are what a reader of this hand learns from.
//
// The pen writes. Nothing scrolls: a page is the rows that fit, and ‹ › turn it. A finger's
// tap on a cell empties the cell, and a tap of two fingers takes the last act back. A mouse
// stands in for the pen, and with ⌥ for the finger.

import { signal } from "@preact/signals-core"
import { useSignals } from "@preact/signals-react/runtime"
import { memo, useEffect, useRef, type JSX } from "react"
import { CELL, PALM, PEN, TAP, type Stroke, pathOf, record } from "./ink"

type Sample = { strokes: Stroke[] }
// `cells` samples are asked of each text
type Family = { name: string, glyphs: string[], cells: number }

// the height of a line's print, above its cell
const PRINT = 32

const families = signal<Family[]>([])
// the family whose pages show, and the first row of the page that does
const open = signal(0)
const top = signal(0)
// the height the rows have, in CSS pixels
const room = signal(0)
// the cells of each text that has ink
const samples = signal(new Map<string, (Sample | null)[]>())
// the cells whose ink the server does not have as the page has it
const unkept = signal(new Map<string, [glyph: string, cell: number]>())
// why there is no copybook to show
const problem = signal("")

const isLine = (text: string) => [...text].length > 1
const sampleAt = (glyph: string, cell: number) => samples.value.get(glyph)?.[cell] ?? null
const written = ({ glyphs }: Family) => glyphs.reduce((n, glyph) => n + (samples.value.get(glyph)?.filter(Boolean).length ?? 0), 0)
// the rows of a family that fit a page
const fits = ({ glyphs }: Family) => Math.max(1, Math.floor(room.value / (CELL.height + 1 + (glyphs.some(isLine) ? PRINT : 0))))
// a family opens at its first row with a cell to write in
const turn = (to: number) => {
  const family = families.value[to]
  if (!family) { return }
  open.value = to
  top.value = Math.max(0, family.glyphs.findIndex((glyph) => Array.from({ length: family.cells }, (_, cell) => sampleAt(glyph, cell)).includes(null)))
}

// The server has a cell as the page has it when its turn in the line comes: writes of one
// cell arrive in the order they were made
let line = Promise.resolve()
const keep = (glyph: string, cell: number) => {
  line = line.then(async () => {
    const key = `${glyph} ${cell}`, rest = new Map(unkept.value)
    try {
      const response = await fetch("copybook", { method: "PUT", body: JSON.stringify({ glyph, cell, sample: sampleAt(glyph, cell) }) })
      if (!response.ok) { throw new Error(await response.text()) }
      if (rest.delete(key)) { unkept.value = rest }
    } catch {
      unkept.value = rest.set(key, [glyph, cell])
    }
  })
}

// the cells before each act of the writer, for `undo`
const before: { glyph: string, cell: number, sample: Sample | null }[] = []
const put = (glyph: string, cell: number, sample: Sample | null) => {
  const row = Array.from({ length: Math.max(cell + 1, samples.value.get(glyph)?.length ?? 0) }, (_, i) => sampleAt(glyph, i))
  row[cell] = sample
  samples.value = new Map(samples.value).set(glyph, row)
  keep(glyph, cell)
}
const act = (glyph: string, cell: number, sample: Sample | null) => {
  before.push({ glyph, cell, sample: sampleAt(glyph, cell) })
  put(glyph, cell, sample)
}
const undo = () => {
  const last = before.pop()
  if (last) { put(last.glyph, last.cell, last.sample) }
}

const load = () => {
  fetch("copybook").then(async (response) => {
    if (!response.ok) { throw new Error(await response.text()) }
    const book = await response.json() as { families: Family[], samples: Record<string, (Sample | null)[]> }
    samples.value = new Map(Object.entries(book.samples))
    families.value = book.families
    // the book opens where there is something left to write
    turn(Math.max(0, book.families.findIndex((family) => written(family) < family.glyphs.length * family.cells)))
  }).catch((e) => { problem.value = `the copybook did not open: ${String((e as Error).message ?? e)}` })
  // a cell the server did not take is offered again
  const again = setInterval(() => { for (const [glyph, cell] of unkept.value.values()) { keep(glyph, cell) } }, 3000)
  return () => { clearInterval(again) }
}

// MARK: Pen and fingers


const mount = (ground: HTMLDivElement) => {
  // the stroke under the pen: its cell, where the cell's baseline starts on the screen,
  // when the stroke began on the events' clock, and its ink on the page
  let wet: { pointer: number, glyph: string, cell: number, left: number, base: number, since: number, stroke: Stroke, ink: SVGPathElement } | null = null
  const fingers = new Map<number, [number, number]>()
  // a tap: when its first finger landed and on which cell, the most fingers down at once,
  // and how far they moved
  let tap: { at: number, home: HTMLElement | null, most: number, moved: number } = { at: 0, home: null, most: 0, moved: 0 }
  // when the pen was last near: the hand that holds it rests on the glass, and taps nothing
  let pen = -Infinity

  // What the pen and the fingers did and what the page made of it goes to the server a
  // second after the last of it (/heard, paper/pen.jsonl) – the record to read when a
  // stroke is lost. Between two entries the pen moved `near` times above the glass, and
  // `stray` times on it with no stroke to take the move.
  const heard: unknown[] = []
  let telling: ReturnType<typeof setTimeout> | undefined, stray = 0, near = 0
  const hear = (e: Event, made: string) => {
    if ((e as PointerEvent).pointerType === "mouse") { return }
    heard.push({ at: Math.round(performance.timeOrigin + e.timeStamp), on: "copybook", type: e.type, made, stray: stray || undefined, near: near || undefined })
    stray = near = 0
    clearTimeout(telling)
    telling = setTimeout(() => { fetch("heard", { method: "POST", body: JSON.stringify(heard.splice(0)) }).catch(() => { }) }, 1000)
  }
  hear(new Event("open"), `${window.innerWidth}×${window.innerHeight}`)

  const cellOf = (e: Event) => (e.target as Element).closest<HTMLElement>("[data-cell]")
  const empty = (home: HTMLElement) => {
    const glyph = home.dataset.glyph!, cell = Number(home.dataset.cell)
    if (sampleAt(glyph, cell)) { act(glyph, cell, null) }
  }

  const take = (e: PointerEvent) => {
    if (!wet) { return }
    const { left, base } = wet
    record(e, wet.stroke, wet.since, (x, y) => [x - left, y - base])
    wet.ink.setAttribute("d", pathOf(wet.stroke.points))
  }

  const dry = () => {
    if (!wet) { return }
    const { glyph, cell, stroke, ink } = wet
    wet = null
    ink.remove()
    act(glyph, cell, { strokes: [...(sampleAt(glyph, cell)?.strokes ?? []), stroke] })
  }

  const down = (e: PointerEvent) => {
    if (e.pointerType === "touch") {
      if (wet || performance.now() - pen < PALM) { hear(e, "the writing hand"); return }
      if (fingers.size === 0) { tap = { at: performance.now(), home: cellOf(e), most: 0, moved: 0 } }
      fingers.set(e.pointerId, [e.clientX, e.clientY])
      tap.most = Math.max(tap.most, fingers.size)
      hear(e, `finger ${fingers.size}`)
      return
    }
    // a stroke is of the cell it starts in
    const home = cellOf(e)
    if (!home) { hear(e, "no cell"); return }
    if (e.pointerType === "mouse" && (e.button !== 0 || e.altKey)) {
      if (e.altKey) { empty(home) }
      return
    }
    e.preventDefault()
    // the fingers down when the pen lands are of the hand that holds it
    fingers.clear()
    // a stroke whose end was not heard ends where the next begins
    dry()
    ground.setPointerCapture(e.pointerId)
    const box = home.getBoundingClientRect()
    const ink = document.createElementNS("http://www.w3.org/2000/svg", "path")
    home.querySelector("[data-wet]")!.append(ink)
    wet = {
      pointer: e.pointerId, glyph: home.dataset.glyph!, cell: Number(home.dataset.cell), left: box.left, base: box.top + CELL.baseline,
      since: e.timeStamp, stroke: { at: Date.now(), points: [] }, ink,
    }
    take(e)
    hear(e, `a stroke in ${wet.glyph} ${wet.cell}`)
  }

  const move = (e: PointerEvent) => {
    if (e.pointerType === "pen") { pen = performance.now() }
    if (wet?.pointer === e.pointerId) { take(e); return }
    if (e.pointerType === "pen") { if (e.pressure > 0) { stray++ } else { near++ } }
    const from = fingers.get(e.pointerId)
    if (from) { tap.moved += Math.hypot(e.clientX - from[0], e.clientY - from[1]); fingers.set(e.pointerId, [e.clientX, e.clientY]) }
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
    if (fingers.size > 0 || performance.now() - tap.at > TAP.ms || tap.moved > TAP.moved) { return }
    if (tap.most === 2) { undo(); hear(e, "a tap of two: the last act is taken back") }
    if (tap.most === 1 && tap.home) { empty(tap.home); hear(e, `a tap: ${tap.home.dataset.glyph} ${tap.home.dataset.cell} is empty`) }
  }

  // The page takes every touch – Safari scrolls nothing, zooms nothing, selects nothing and
  // waits for no second tap – but a touch on a button, which is the button's click
  const touch = (e: TouchEvent) => {
    const byPen = [...e.changedTouches].some((t) => (t as Touch & { touchType?: string }).touchType === "stylus")
    if (byPen && e.type !== "touchmove") { hear(e, "") }
    if (!(e.target as Element).closest("button")) { e.preventDefault() }
  }
  const still = (e: Event) => { e.preventDefault() }

  const stops = new AbortController(), heeded = { signal: stops.signal, passive: false }
  ground.addEventListener("pointerdown", down, { ...heeded, capture: true })
  ground.addEventListener("pointermove", move, heeded)
  ground.addEventListener("pointerup", up, heeded)
  ground.addEventListener("pointercancel", up, heeded)
  ground.addEventListener("gesturestart", still, heeded)
  for (const type of ["touchstart", "touchmove", "touchend"] as const) { ground.addEventListener(type, touch, heeded) }
  return () => { stops.abort() }
}

// MARK: Views

const INK = { fill: "none", stroke: "currentColor", strokeWidth: PEN, strokeLinecap: "round", strokeLinejoin: "round" } as const

const Cell = memo(function Cell({ glyph, cell, sample }: { glyph: string, cell: number, sample: Sample | null }): JSX.Element {
  return (
    <div data-cell={cell} data-glyph={glyph} className="flex-1 min-w-0 border-l border-neutral-800" style={{ maxWidth: isLine(glyph) ? undefined : CELL.width }}>
      <svg width="100%" height={CELL.height} className="block overflow-visible">
        <g transform={`translate(0 ${CELL.baseline})`}>
          <line x1={0} x2="100%" className="stroke-neutral-700" />
          {sample?.strokes.map((stroke) => <path key={stroke.at} d={pathOf(stroke.points)} {...INK} />)}
          <g data-wet {...INK} />
        </g>
      </svg>
    </div>
  )
})

// A glyph's print stands before its cells, and a line's above its cell, with its spaces
function Row({ glyph, cells }: { glyph: string, cells: number }): JSX.Element {
  const row = samples.value.get(glyph)
  const hand = Array.from({ length: cells }, (_, cell) => <Cell key={cell} glyph={glyph} cell={cell} sample={row?.[cell] ?? null} />)
  if (isLine(glyph)) {
    return (
      <div className="border-b border-neutral-800">
        <div className="font-mono text-2xl whitespace-pre overflow-hidden text-neutral-400" style={{ height: PRINT, lineHeight: `${PRINT}px` }}>{glyph}</div>
        <div className="flex">{hand}</div>
      </div>
    )
  }
  return (
    <div className="flex border-b border-neutral-800">
      <svg width={CELL.width} height={CELL.height} className="shrink-0 text-neutral-400">
        <line x1={0} x2={CELL.width} y1={CELL.baseline} y2={CELL.baseline} className="stroke-neutral-700" />
        <text x={CELL.width / 2} y={CELL.baseline} textAnchor="middle" fontSize={40} fill="currentColor" className="font-mono">{glyph}</text>
      </svg>
      <div className="flex flex-1 min-w-0">{hand}</div>
    </div>
  )
}

export function Copybook(): JSX.Element {
  useSignals()
  const ground = useRef<HTMLDivElement>(null), rows = useRef<HTMLDivElement>(null)
  useEffect(() => {
    const sized = new ResizeObserver(([entry]) => { room.value = entry!.contentRect.height })
    sized.observe(rows.current!)
    const stops = [mount(ground.current!), load(), () => { sized.disconnect() }]
    return () => { for (const stop of stops) { stop() } }
  }, [])
  const family = families.value[open.value], count = family?.glyphs.length ?? 0, page = family ? fits(family) : 0
  const from = Math.min(top.value, Math.max(0, count - 1)), to = Math.min(count, from + page)
  const total = families.value.reduce((n, { glyphs, cells }) => n + glyphs.length * cells, 0)
  return (
    <div
      ref={ground} className="fixed inset-0 flex flex-col overflow-hidden bg-neutral-900 text-white select-none"
      style={{ touchAction: "none", WebkitUserSelect: "none", WebkitTouchCallout: "none" }}
    >
      <div className="flex flex-wrap items-baseline gap-x-4 gap-y-1 px-4 py-2 border-b border-neutral-800 text-sm">
        {families.value.map((each, i) => (
          <button key={each.name} className={i === open.value ? "" : "text-neutral-500"} onClick={() => { turn(i) }}>
            {each.name} <span className="text-xs tabular-nums opacity-60">{written(each)}/{each.glyphs.length * each.cells}</span>
          </button>
        ))}
        <span className="ml-auto flex items-baseline gap-x-1 tabular-nums">
          <button className={`px-3 text-lg ${from > 0 ? "" : "text-neutral-700"}`} onClick={() => { top.value = Math.max(0, from - page) }}>‹</button>
          <span className="text-xs text-neutral-500">{count > 0 ? `${from + 1}–${to} of ${count}` : ""}</span>
          <button className={`px-3 text-lg ${to < count ? "" : "text-neutral-700"}`} onClick={() => { if (to < count) { top.value = to } }}>›</button>
        </span>
        <span className="text-xs tabular-nums text-neutral-500">{families.value.reduce((n, each) => n + written(each), 0)} of {total}</span>
        {unkept.value.size > 0 && <span className="text-xs text-red-500">{unkept.value.size} not kept – is the server there?</span>}
      </div>
      {problem.value && <div className="p-4 text-red-500 text-sm">{problem.value}</div>}
      <div ref={rows} className="flex-1 min-h-0 overflow-hidden px-4">
        {family?.glyphs.slice(from, to).map((glyph) => <Row key={glyph} glyph={glyph} cells={family.cells} />)}
      </div>
    </div>
  )
}
