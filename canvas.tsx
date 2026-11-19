// Canvas: an unbounded plane to write Fluent on with a pen. Where the pen starts in the
// open, a line is ruled for it, as large on the screen as the copybook's lines are: ink
// written zoomed in is finer on the plane, and the readers see every line at the hand's
// size. A line is read by the readers of this hand (ink.tsx), and the lines' texts, top
// to bottom, are one program – a line that reads a name bound further down waits for it,
// as any line of a program does; under a line is its value.
//
// The pen writes, and a scratch of it over ink erases the ink. A finger on a line's ink
// moves the line, and anywhere else the plane; two zoom, a tap of two takes the last act
// back, and a finger on a value works it. Where the pen is near the glass with no line
// under it, the rule a line started there would get shows; over a line, the line shows a
// grip where its rule starts, and the pen on the grip moves the line as a finger does.
// A mouse stands in for the pen, its wheel for the finger, and with ⌃ – a trackpad's pinch –
// for two.
//
// There are many canvases, and ?canvas=<name> is the one shown – ?canvas alone, the one
// kept last. A tab at the left edge opens them all, each as it looked, and a clean one to
// begin.

import { signal, effect } from "@preact/signals-core"
import { useSignals } from "@preact/signals-react/runtime"
import { memo, useEffect, useRef, useState, type JSX } from "react"
import { type Desc, root } from "./protocol"
import { BLANK, PALM, PEN, PLAIN, REST, TAP, VALUE, type Line, type Look, type Stroke, Shown, Tips, Written, asked, boxOf, hearing, inkOf, problem, program, reading, record, shown, touched, traced, under, worked } from "./ink"

// A line of the canvas: where its baseline starts on the plane, how many units of the plane
// one unit of its ink is, and the line as its readers have it
type Placed = Line & { id: number, x: number, y: number, k: number }
type Camera = { x: number, y: number, k: number }

const lines = signal<Placed[]>([])
// a point p of the plane shows at p × k + (x, y)
const camera = signal<Camera>({ x: 0, y: 0, k: 1 })
// the line the pen is writing, until it has rested REST ms
const writing = signal<number | null>(null)
// the line the pen is near, and the line a finger or the pen holds to move it
const neared = signal<number | null>(null)
const holding = signal<number | null>(null)
// when the pen was last near: the hand that holds it rests on the glass, and moves nothing
let pen = -Infinity

// A stroke is of a line when it starts in the line's band – no further over its baseline or
// under it than this, and no further beside its ink, before it or after: room for a name
// and its colon. A line's rule is drawn less far beside the ink, and fades out there: the
// pen near the glass is shown the rest – in a line's band the line shows its grip, and out
// of every band the rule of a new line shows. All in units of the line's ink.
const BAND = { over: 72, under: 34, beside: 128, ruled: 64 }
// The baseline of a line the pen starts is this far under the pen's first touch – the
// middle of where this hand first touches a line it writes – and starts this far before it
const START = { under: 28, before: 24 }
// the grip of a line is where its rule starts, and the pen takes it from this near
const GRIP = 16

const clamp = (v: number, lo: number, hi: number) => Math.min(Math.max(v, lo), hi)

// how far a line's band goes along its baseline, or its rule
const reach = ({ strokes }: Line, beside = BAND.beside) => {
  const { left, right } = boxOf(strokes)
  return { from: left - beside, to: right + beside }
}

// the line whose band has a point of the plane: of two, the one the point is at the height
// a first touch has
const lineAt = (px: number, py: number) => {
  let home: Placed | undefined, far = Infinity
  for (const line of lines.value) {
    const x = (px - line.x) / line.k, y = (py - line.y) / line.k, { from, to } = reach(line), off = Math.abs(y + START.under)
    if (x >= from && x <= to && y >= -BAND.over && y <= BAND.under && off < far) { home = line; far = off }
  }
  return home
}

// the line whose grip a point of the plane is at
const gripAt = (px: number, py: number) => lines.value.find((line) => Math.hypot((px - line.x) / line.k - reach(line, BAND.ruled).from, (py - line.y) / line.k) <= GRIP)
// the line whose ink a point of the plane is on, or no further from than a finger is wide
const FINGER = 12
const inkAt = (px: number, py: number) => lines.value.find((line) => {
  const x = (px - line.x) / line.k, y = (py - line.y) / line.k, { left, right, top, bottom } = boxOf(line.strokes)
  return x >= left - FINGER && x <= right + FINGER && y >= top - FINGER && y <= bottom + FINGER
})

// MARK: Ink

// the lines before each act of the pen or of a finger, for `undo`
const before: Placed[][] = []
let lastId = 0

// A stroke the pen has finished. A scratch takes the ink it covers, of whatever lines and
// wherever it started, and leaves none of its own: a line with no ink left is gone. Any
// other stroke is ink of the line it started in.
const commit = (home: Placed, stroke: Stroke) => {
  // the stroke as a line has it: in units of the line's ink, from the start of its baseline
  const seenFrom = ({ x, y, k }: Placed) => ({ width: PEN * home.k / k, points: stroke.points.map(([px, py]): [number, number] => [(home.x + px * home.k - x) / k, (home.y + py * home.k - y) / k]) })
  const gone = new Set(lines.value.flatMap((line) => [...under(seenFrom(line), line.strokes)]))
  const kept = (line: Placed) => line.strokes.filter((s) => !gone.has(s))
  const line = lines.value.find(({ id }) => id === home.id) ?? home
  before.push(lines.value)
  lines.value = gone.size > 0
    ? lines.value.flatMap((each) => (kept(each).length === each.strokes.length ? [each] : kept(each).length > 0 ? [{ ...each, strokes: kept(each) }] : []))
    : [...lines.value.filter(({ id }) => id !== home.id), { ...line, strokes: [...line.strokes, stroke] }]
}

// Takes back the last act: a stroke, a scratch, or a line moved. A line whose readers had
// not answered is read again.
const undo = () => {
  const page = before.pop()
  if (!page) { return }
  for (const { strokes, read } of page) { if (read !== strokes) { asked.delete(strokes) } }
  lines.value = page
}

// MARK: The canvases as files
// The dev server keeps every canvas as a file (server.ts, /canvas/<name>) – its lines, where
// each is and its ink, its camera, and a picture of it for the cards; the lines' texts are
// read again. A canvas is named by a number, the next when it is begun, and has no file
// until something is on it.

// a canvas as it looked: the plane's markup, and the part of the plane that had something on it
type Picture = { html: string, left: number, top: number, right: number, bottom: number }
type Page = { lines?: Pick<Placed, "id" | "x" | "y" | "k" | "strokes">[], camera?: Camera, picture?: Picture }
type Kept = { name: string, at: number }
const canvases = (): Promise<Kept[]> => fetch("canvases").then((r) => r.json())
// the canvas shown, once it is known which, and whether its file has been read
const open = signal<string | null>(null)
let loaded = false, timer: ReturnType<typeof setTimeout> | undefined
// Which writing of the canvas's file the page has (server.ts, ETag). A page keeps its canvas
// over that writing only: a file written since – by another page, or by ink/canvas.py – is
// read anew, and what this page had of the canvas is gone.
let had: string | null = null, keeping: Promise<unknown> = Promise.resolve()
// the plane's element: the lines as the page shows them
const plane: { current: HTMLDivElement | null } = { current: null }

// The plane as it looks now – ink and print in their colors, and the values – with what a
// value painted as a picture of that
const pictured = (): Picture | undefined => {
  const live = plane.current, { x, y, k } = camera.value
  if (!live || lines.value.length === 0) { return undefined }
  const box = { left: Infinity, top: Infinity, right: -Infinity, bottom: -Infinity }
  for (const el of live.querySelectorAll("path, [data-value]")) {
    const r = el.getBoundingClientRect()
    if (r.width === 0 && r.height === 0) { continue }
    box.left = Math.min(box.left, (r.left - x) / k); box.top = Math.min(box.top, (r.top - y) / k)
    box.right = Math.max(box.right, (r.right - x) / k); box.bottom = Math.max(box.bottom, (r.bottom - y) / k)
  }
  const copy = live.cloneNode(true) as HTMLDivElement, painted = [...live.querySelectorAll("canvas")]
  copy.querySelectorAll("canvas").forEach((canvas, i) => {
    const image = document.createElement("img")
    image.width = canvas.width; image.height = canvas.height; image.className = canvas.className; image.style.cssText = canvas.style.cssText
    // a canvas the worker paints gives no picture of itself
    try { image.src = painted[i]!.toDataURL() } catch { }
    canvas.replaceWith(image)
  })
  return { html: copy.innerHTML, ...box }
}

// reads a canvas's file: nothing is kept meanwhile
const load = async (name: string) => {
  loaded = false
  before.length = 0
  try {
    const kept = await fetch(`canvas/${name}`), page: Page = await kept.json()
    if (open.value !== name) { return }
    had = kept.headers.get("ETag")
    lines.value = (page.lines ?? []).map((line) => ({ ...BLANK, ...line }))
    lastId = Math.max(0, ...lines.value.map(({ id }) => id))
    if (page.camera) { camera.value = page.camera }
  } catch { }
  loaded = true
}

// keeps the canvas shown, as it is now – one writing after another, each over the one before
const save = (): Promise<unknown> => {
  clearTimeout(timer)
  return keeping = keeping.then(async () => {
    const name = open.value
    if (!loaded || name === null) { return }
    const page: Page = { lines: lines.value.map(({ id, x, y, k, strokes }) => ({ id, x, y, k, strokes })), camera: camera.value, picture: pictured() }
    const kept = await fetch(`canvas/${name}`, { method: "PUT", headers: had === null ? {} : { "If-Match": had }, body: JSON.stringify(page) })
    if (kept.status === 412) { await load(name) } else if (kept.ok) { had = kept.headers.get("ETag") }
  }).catch(() => { })
}

// Shows another canvas: the one shown is kept first
const show = async (name: string) => {
  await save()
  loaded = false
  open.value = name
  lines.value = []; camera.value = { x: 0, y: 0, k: 1 }
  had = null
  // the URL's other words stay
  const url = new URL(window.location.href)
  url.searchParams.set("canvas", name)
  window.history.replaceState(null, "", url)
  await load(name)
}

const keep = () => {
  const named = new URLSearchParams(window.location.search).get("canvas")
  void (named ? Promise.resolve(named) : canvases().then((all) => all.toSorted((a, b) => b.at - a.at)[0]?.name ?? "1").catch(() => "1")).then(show)
  return effect(() => {
    void lines.value, camera.value
    clearTimeout(timer)
    if (loaded) { timer = setTimeout(save, 800) }
  })
}

// MARK: Pen and fingers

const mount = (ground: HTMLDivElement, wetInk: SVGGElement, rule: HTMLDivElement) => {
  // the stroke under the pen: its line, where the line's baseline starts on the screen and
  // how large a unit of its ink is there, when the stroke began on the events' clock, and
  // its ink on the page
  let wet: { pointer: number, home: Placed, left: number, base: number, scale: number, since: number, stroke: Stroke, ink: SVGPathElement } | null = null
  const touches = new Map<number, [number, number]>()
  // a tap: when its first finger landed, the most fingers down at once, and how far they moved
  let tap = { at: 0, most: 0, moved: 0 }
  let resting: ReturnType<typeof setTimeout> | undefined
  // where on the screen the baseline is of the rule shown to the pen near the glass
  let shownAt: number | null = null
  // the line a finger or the pen moves: where on the plane it took the line, and where the line was then
  let held: { pointer: number, id: number, px: number, py: number, x: number, y: number, moved: boolean } | null = null

  const hear = hearing("canvas")
  hear(new Event("open"), `${window.innerWidth}×${window.innerHeight}`)

  const planeOf = (x: number, y: number) => [(x - camera.value.x) / camera.value.k, (y - camera.value.y) / camera.value.k] as const
  const inRule = (y: number) => shownAt !== null && y >= shownAt - BAND.over && y <= shownAt + BAND.under

  // Near the glass with no line under it, the pen is shown the rule a line started there
  // would get. The rule stays while the pen is in its band – the pen can go to where its
  // first stroke starts – and goes with a pen that leaves the band.
  const near = (e: PointerEvent | null) => {
    const at = e && !wet && touches.size === 0 && !(e.target as Element).closest("[data-value]") ? planeOf(e.clientX, e.clientY) : null
    const line = at && (gripAt(...at) ?? lineAt(...at))
    neared.value = line?.id ?? null
    if (!e || !at || line) {
      shownAt = null
      rule.hidden = true
      return
    }
    if (!inRule(e.clientY)) { shownAt = e.clientY + START.under }
    rule.hidden = false
    rule.style.transform = `translate(${e.clientX - START.before}px, ${shownAt}px)`
  }
  const hold = (pointer: number, line: Placed, px: number, py: number) => {
    held = { pointer, id: line.id, px, py, x: line.x, y: line.y, moved: false }
    holding.value = line.id
  }
  const letGo = (e: PointerEvent) => {
    if (held?.moved) { hear(e, `line ${held.id} is moved`) }
    held = null
    holding.value = null
  }

  const take = (e: PointerEvent) => {
    if (!wet) { return }
    const { left, base, scale } = wet
    record(e, wet.stroke, wet.since, (x, y) => [(x - left) / scale, (y - base) / scale])
    wet.ink.setAttribute("d", traced(wet.stroke.points))
  }

  const dry = () => {
    if (!wet) { return }
    const { home, stroke, ink } = wet
    wet = null
    ink.remove()
    commit(home, stroke)
    resting = setTimeout(() => { writing.value = null }, REST)
  }

  const down = (e: PointerEvent) => {
    const value = (e.target as Element).closest("[data-value]")
    if (e.pointerType === "touch") {
      if (value || wet || performance.now() - pen < PALM) { return }
      if (touches.size === 0) { tap = { at: performance.now(), most: 0, moved: 0 } }
      // one finger on a line's ink holds the line; a second lets it go, and the two move the plane
      const [px, py] = planeOf(e.clientX, e.clientY), line = touches.size === 0 ? inkAt(px, py) : undefined
      if (line) { hold(e.pointerId, line, px, py) } else { letGo(e) }
      touches.set(e.pointerId, [e.clientX, e.clientY])
      tap.most = Math.max(tap.most, touches.size)
      ground.setPointerCapture(e.pointerId)
      near(null)
      return
    }
    // the pen writes over a value, but for what it can work there; the mouse works a value as a finger does
    if (e.pointerType === "mouse" ? value || e.button !== 0 : worked(e.target)) { return }
    e.preventDefault()
    e.stopPropagation()
    // the fingers down when the pen lands are of the hand that holds it
    touches.clear()
    // a stroke whose end was not heard ends where the next begins
    dry()
    ground.setPointerCapture(e.pointerId)
    // a stroke that starts in no line's band starts a line: on the rule shown, when the pen
    // is in its band, and else under the pen
    const { x, y, k } = camera.value, [px, py] = planeOf(e.clientX, e.clientY), base = inRule(e.clientY) ? shownAt! : e.clientY + START.under
    // the pen on a line's grip writes nothing: it holds the line
    const gripped = gripAt(px, py)
    if (gripped) {
      near(null)
      hold(e.pointerId, gripped, px, py)
      return
    }
    const old = lineAt(px, py), home = old ?? { ...BLANK, id: ++lastId, x: px - START.before / k, y: (base - y) / k, k: 1 / k }
    // the rule of a line that starts shows under its first stroke, until the line has ink to show its own
    near(null)
    if (!old) {
      shownAt = base
      rule.hidden = false
      rule.style.transform = `translate(${e.clientX - START.before}px, ${base}px)`
    }
    const ink = document.createElementNS("http://www.w3.org/2000/svg", "path")
    wetInk.setAttribute("transform", `translate(${home.x} ${home.y}) scale(${home.k})`)
    wetInk.append(ink)
    wet = { pointer: e.pointerId, home, left: x + home.x * k, base: y + home.y * k, scale: home.k * k, since: e.timeStamp, stroke: { at: Date.now(), points: [] }, ink }
    clearTimeout(resting)
    writing.value = home.id
    take(e)
    hear(e, `a stroke in line ${home.id}`)
  }

  const moved = (e: PointerEvent) => {
    if (e.pointerType === "pen") { pen = performance.now() }
    if (wet?.pointer === e.pointerId) { take(e); return }
    const from = touches.get(e.pointerId)
    if (held?.pointer === e.pointerId) {
      const { id, px, py, x, y } = held, [qx, qy] = planeOf(e.clientX, e.clientY)
      if (from) { tap.moved += Math.hypot(e.clientX - from[0], e.clientY - from[1]); touches.set(e.pointerId, [e.clientX, e.clientY]) }
      if (!held.moved) { before.push(lines.value); held.moved = true }
      lines.value = lines.value.map((line) => (line.id === id ? { ...line, x: x + qx - px, y: y + qy - py } : line))
      return
    }
    if (!from) {
      if (e.pointerType !== "touch") { near(e) }
      return
    }
    const centre = (ps: [number, number][]) => [ps.reduce((a, p) => a + p[0], 0) / ps.length, ps.reduce((a, p) => a + p[1], 0) / ps.length] as const
    const spread = (ps: [number, number][]) => (ps.length < 2 ? 1 : Math.hypot(ps[0]![0] - ps[1]![0], ps[0]![1] - ps[1]![1]))
    const old = [...touches.values()]
    touches.set(e.pointerId, [e.clientX, e.clientY])
    const now = [...touches.values()]
    tap.moved += Math.hypot(e.clientX - from[0], e.clientY - from[1])
    // the point of the plane under the fingers' centre stays under it
    const [cx0, cy0] = centre(old), [cx1, cy1] = centre(now), { x, y, k } = camera.value
    const s = clamp(k * spread(now) / Math.max(spread(old), 1), 0.1, 10) / k
    camera.value = { k: k * s, x: cx1 - (cx0 - x) * s, y: cy1 - (cy0 - y) * s }
  }

  const up = (e: PointerEvent) => {
    if (wet?.pointer === e.pointerId) {
      take(e)
      hear(e, `${wet.stroke.points.length} points`)
      dry()
      return
    }
    if (held?.pointer === e.pointerId) { letGo(e) }
    if (e.pointerType === "pen") { hear(e, "nothing"); return }
    if (!touches.delete(e.pointerId)) { return }
    if (e.type === "pointercancel") { tap.most = Infinity }
    if (touches.size === 0 && tap.most === 2 && performance.now() - tap.at < TAP.ms && tap.moved < TAP.moved) { undo() }
  }

  // a wheel pans, and with ⌃ zooms about the pointer
  const wheel = (e: WheelEvent) => {
    e.preventDefault()
    near(null)
    const { x, y, k } = camera.value
    if (!e.ctrlKey) { camera.value = { k, x: x - e.deltaX, y: y - e.deltaY }; return }
    const s = clamp(k * Math.exp(-e.deltaY / 100), 0.1, 10) / k
    camera.value = { k: k * s, x: e.clientX - (e.clientX - x) * s, y: e.clientY - (e.clientY - y) * s }
  }
  const touch = touched(hear)
  // Safari's own pinch would zoom the page
  const still = (e: Event) => { e.preventDefault() }
  const gone = () => { near(null) }

  const stops = new AbortController(), heeded = { signal: stops.signal, passive: false }
  // the pen's stroke starts before anything under it hears of the pen
  ground.addEventListener("pointerdown", down, { ...heeded, capture: true })
  ground.addEventListener("pointermove", moved, heeded)
  ground.addEventListener("pointerup", up, heeded)
  ground.addEventListener("pointercancel", up, heeded)
  ground.addEventListener("pointerleave", gone, heeded)
  ground.addEventListener("wheel", wheel, heeded)
  ground.addEventListener("gesturestart", still, heeded)
  for (const type of ["touchstart", "touchmove", "touchend", "touchcancel"] as const) { ground.addEventListener(type, touch, heeded) }
  return () => { stops.abort() }
}

// MARK: Views

// A line where it is on the plane: its rule, as long as its band and fading out beside the
// ink, its print and its ink, and under the rule its value. A line the pen is near shows
// its grip, and a line that is held is brighter.
const Standing = memo(function Standing({ line, near, held, value, error, View, colorsOf }: { line: Placed, near: boolean, held: boolean, value?: Desc, error?: string } & Look): JSX.Element {
  const { from, to } = reach(line, BAND.ruled), beside = BAND.ruled / (to - from), shade = held ? "#a3a3a3" : "#404040"
  // the picture is as large as what is in it – the band, and ink that goes out of it: a
  // browser paints anew what changes within a picture's box, and not always what hangs out of it
  const ink = boxOf(line.strokes), top = Math.min(-BAND.over, ink.top - 2 * PEN), height = Math.max(BAND.under, ink.bottom + 2 * PEN) - top
  return (
    <div className="absolute left-0 top-0" style={{ transform: `translate(${line.x}px, ${line.y}px) scale(${line.k})`, transformOrigin: "0 0" }}>
      <svg width={to - from} height={height} viewBox={`${from} ${top} ${to - from} ${height}`} className="absolute overflow-visible" style={{ left: from, top }}>
        <linearGradient id={`rule-${line.id}`} gradientUnits="userSpaceOnUse" x1={from} x2={to}>
          <stop offset={0} stopColor={shade} stopOpacity={0} />
          <stop offset={beside} stopColor={shade} />
          <stop offset={1 - beside} stopColor={shade} />
          <stop offset={1} stopColor={shade} stopOpacity={0} />
        </linearGradient>
        <line x1={from} x2={to} stroke={`url(#rule-${line.id})`} />
        {(near || held) && <circle cx={from} r={4} fill={held ? "#e5e5e5" : "#737373"} />}
        <Written line={line} colorsOf={colorsOf} />
      </svg>
      <div className="absolute w-max max-w-md font-mono text-xl" style={{ left: boxOf(line.strokes).left, top: VALUE.under }}>
        <Shown value={value} error={error} View={View} />
      </div>
    </div>
  )
})

// a canvas's picture in small: all that was on it, as large as fits
const SMALL = { width: 152, height: 96 }
const Small = memo(function Small({ name }: { name: string }): JSX.Element {
  const [picture, setPicture] = useState<Picture>()
  useEffect(() => { fetch(`canvas/${name}`).then((r) => r.json()).then((page: Page) => { setPicture(page.picture) }).catch(() => { }) }, [name])
  if (!picture) { return <div style={SMALL} /> }
  const { html, left, top, right, bottom } = picture, s = Math.min((SMALL.width - 8) / Math.max(right - left, 1), (SMALL.height - 8) / Math.max(bottom - top, 1), 1)
  return (
    <div className="relative overflow-hidden pointer-events-none text-white" style={SMALL}>
      <div className="absolute left-0 top-0" style={{ transform: `translate(${4 - left * s}px, ${4 - top * s}px) scale(${s})`, transformOrigin: "0 0" }} dangerouslySetInnerHTML={{ __html: html }} />
    </div>
  )
})

// The canvases, at the left edge: a tab, and behind it all of them, the last begun first,
// each as it looked when it was last kept – and over them a clean one to begin. A tap takes
// one; a tap beside them closes them.
function Canvases(): JSX.Element {
  useSignals()
  const [all, setAll] = useState<Kept[] | null>(null)
  if (all === null) {
    // the hand that holds the pen rests on the glass, and opens nothing
    const list = (e: React.PointerEvent) => { if (e.pointerType !== "touch" || performance.now() - pen > PALM) { save().then(canvases).then(setAll).catch(() => { }) } }
    return <div key="tab" onPointerUp={list} className="fixed left-0 top-1/2 -translate-y-1/2 w-5 h-20 flex items-center"><div className="w-1.5 h-14 rounded-r bg-neutral-600/60" /></div>
  }
  const next = String(Math.max(0, Number(open.value) || 0, ...all.map(({ name }) => Number(name))) + 1)
  const card = "shrink-0 rounded border bg-neutral-900"
  const take = (name: string) => () => { setAll(null); void show(name) }
  return (
    <>
      {/* the touch that closes them must not end on the tab: the two are not one element */}
      <div key="beside" className="fixed inset-0" style={{ touchAction: "none" }} onPointerDown={() => { setAll(null) }} />
      <div className="fixed left-0 inset-y-0 flex flex-col gap-2 p-2 overflow-y-auto select-none bg-neutral-950/95 border-r border-neutral-800">
        <Tips />
        <div onClick={take(next)} className={`${card} border-neutral-700 text-neutral-400 text-2xl flex items-center justify-center`} style={SMALL}>+</div>
        {all.toReversed().map(({ name }) => (
          <div key={name} onClick={take(name)} className={`${card} ${name === open.value ? "border-neutral-300" : "border-neutral-800"}`}><Small name={name} /></div>
        ))}
      </div>
    </>
  )
}

export function Canvas({ View, colorsOf, run, parse }: Look & { run: (source: string) => void, parse: (source: string) => void }): JSX.Element {
  useSignals()
  const ground = useRef<HTMLDivElement>(null), wetInk = useRef<SVGGElement>(null), rule = useRef<HTMLDivElement>(null)
  useEffect(() => {
    // the program is the lines as the plane has them: top to bottom, then left to right
    const texts = () => [...lines.value].sort((a, b) => a.y - b.y || a.x - b.x)
    const stops = [mount(ground.current!, wetInk.current!, rule.current!), reading(lines), program(texts, run, parse), keep()]
    return () => { for (const stop of stops) { stop() } }
  }, [])
  const { x, y, k } = camera.value, whole = root.desc.value
  return (
    <>
    <div
      ref={ground} className="fixed inset-0 overflow-hidden bg-neutral-900 text-white select-none"
      style={{ touchAction: "none", WebkitUserSelect: "none", WebkitTouchCallout: "none" }}
    >
      <div ref={plane} className="absolute left-0 top-0" style={{ transform: `translate(${x}px, ${y}px) scale(${k})`, transformOrigin: "0 0" }}>
        {lines.value.map((line) => <Standing key={line.id} line={line} near={neared.value === line.id} held={holding.value === line.id} {...shown(line, line.id, writing.value !== line.id)} View={View} colorsOf={colorsOf} />)}
      </div>
      {/* the stroke under the pen, on a picture as large as the page */}
      <svg className="absolute inset-0 w-full h-full pointer-events-none"><g transform={`translate(${x} ${y}) scale(${k})`}><g ref={wetInk} {...inkOf(PLAIN)} /></g></svg>
      <div ref={rule} hidden className="absolute left-0 top-0 h-px pointer-events-none" style={{ width: START.before + BAND.ruled, background: `linear-gradient(to right, transparent, #404040 ${START.before}px, transparent)` }} />
      {lines.value.length === 0 && (
        <div className="absolute inset-x-0 bottom-6 text-center text-neutral-600 text-sm pointer-events-none">
          the pen writes, and a line is ruled where it starts · a scratch over ink erases it · a finger on a line moves it, and elsewhere the plane · two fingers zoom · a tap of two fingers undoes
        </div>
      )}
      {problem.value && <div className="absolute right-2 top-2 max-w-md text-red-500 text-xs">{problem.value}</div>}
      {/* a program that fails whole has no list to show: it says why */}
      {whole?.k === "error" && <div className="absolute left-2 bottom-2 max-w-md"><View d={whole} /></div>}
    </div>
    <Canvases />
    </>
  )
}
