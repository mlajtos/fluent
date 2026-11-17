// The page and the evaluation worker talk in plain data. The worker owns every value;
// the page sees descriptions of them, and the live parts – a signal, a variable, a
// callback – as ids it renders, writes back to, or calls.
//
// The worker sends the changes of live values in batches, at most one a frame, and the
// next only after the page has drawn the last – a page that draws slowly gets fewer,
// fresher batches instead of a growing queue. The program's value is live 0: each run
// changes it.
//
// A live id is held by the descriptions that mention it. The worker counts how many
// times it sent an id, the page counts how many it received; when the page stops
// showing one it releases all it received in a single "drop", and the worker lets go
// once its count reaches zero – a release can never race a send. A picture is sent
// once, and dropped once. An id names one thing for as long as the worker runs.

import { signal, type Signal } from "@preact/signals-core"

// Where a value came from: line and column of its start and end, and a hash of the
// document they index into – the page finds the editor whose text hashes the same.
export type Where = { s: [number, number], e: [number, number], d: number }

// A syntax tree as the tree view draws it
export type Tree = { label: string, o: Where, children: Tree[] }

export type Doc = { signature?: string, doc?: string, example?: string, also?: string[] }

export type Desc = { o?: Where } & (
  | { k: "null" }
  | { k: "string", v: string }
  // a symbol, a JS number, anything without a view of its own: shown as text
  | { k: "other", v: string }
  | { k: "list", items: Desc[] }
  | { k: "record", entries: [string, Desc][] }
  | { k: "function", name: string, source?: string, doc?: Doc }
  | { k: "error", chain: { message: string, o?: Where }[], carried?: string }
  // a quoted program: one tree per statement
  | { k: "tree", roots: Tree[] }
  // A number shows its value: here when it was on the host, otherwise in a "tensor"
  // message that follows. A picture with `paint` never crosses: the page hands the worker
  // a canvas to paint it in, and gets numbers for its labels – the values when they are
  // labelled, otherwise the finite range they span. A heatmap's `size` is its picture's,
  // [width, height] in units: its maps laid out and magnified.
  | { k: "tensor", id: number, shape: number[], payload?: Float32Array, paint?: true, size?: [number, number] }
  // a signal or a variable and its value now; later values arrive as "live" messages
  | { k: "live", id: number, now?: Desc }
  | { k: "callback", id: number }
  // a widget: `uid` is the widget's identity – a different widget never reuses another's
  // mounted state
  | { k: "ui", uid: number, type: string, props: Record<string, unknown>, slots: Record<string, Desc>, children: Desc[] }
)

// How a tensor shows – the one rule both sides follow. Rank 0 is a number and rank 1 bars.
// [h, w, 3] and [h, w, 4] are a picture's colors, RGB and RGBA: an image. Anything else is
// a heatmap: a matrix is one map, and a tensor of rank 3 or more shows its last two axes
// as maps side by side, all on one scale (prelude.fl, DrawTiles).
export type TensorView = "number" | "bars" | "heat" | "image"
export const tensorView = (shape: readonly number[]): TensorView =>
  shape.length === 0 ? "number" : shape.length === 1 ? "bars"
    : shape.length === 3 && (shape[2] === 3 || shape[2] === 4) ? "image" : "heat"
// Bars, or the cells of one map, this few are labelled with their values, which cross as
// numbers
export const FEW = 20
export const labelled = (shape: readonly number[]): boolean => shape.length <= 2 && shape.reduce((a, b) => a * b, 1) < FEW
// A picture shows at its size in units, a CSS pixel each. A heatmap's cells are magnified
// by a whole number of units, to about 256 a side; an image under 128 units – a raster of
// data, not a drawing – by the same rule (client.tsx, TensorPlot).
export const heatScale = (height: number, width: number): number => Math.ceil(256 / Math.max(height, width, 1))

// A number as Fluent writes it, on the page and in String alike. A float32 is the shortest
// decimal that reads back as the same float32 – 65.4, not 65.400002 – and any other number
// is JavaScript's shortest; from 1e7 up it takes an exponent, 1e20. `digits` shortens a
// label to that many significant digits, but never drops a digit of the whole part, so a
// label never ends in a 0 the number doesn't have, and a number that isn't 0 never shows as
// 0. The non-numbers show as ∞, -∞ and NaN.
export const numberText = (n: number, digits = Infinity): string => {
  if (!Number.isFinite(n)) { return n > 0 ? "∞" : n < 0 ? "-∞" : "NaN" }
  if (n === 0) { return "0" }
  const same = Math.fround(n) === n ? Math.fround : (x: number) => x
  let p = 1
  while (p < 17 && same(+n.toPrecision(p)) !== n) { p++ }
  const whole = Math.floor(Math.log10(Math.abs(n))) + 1
  const m = +n.toPrecision(Math.min(p, Math.max(digits, Math.abs(n) < 1e7 ? whole : 0)))
  return Math.abs(m) >= 1e7 ? m.toExponential().replace("e+", "e") : String(m)
}

// The version of Fluent a saved program is written in: a link carries it, and a program
// written in an older one is migrated as it loads (migrate.ts)
export const VERSION = 2

// page → worker
export type ToWorker =
  // the program, and the version it is written in when that is older than VERSION
  | { t: "code", source: string, v?: number }
  // parse a document for its editor's markers
  | { t: "parse", source: string }
  // write a live value: a number, numbers, text, or an image. `seq` comes back as the
  // "live" message's `ack`, so the page knows when the worker has seen its writes
  | { t: "set", id: number, value: number | number[] | string | ImageBitmap, seq: number }
  | { t: "call", id: number }
  | { t: "drop", id: number, count: number }
  // a frame from a source the worker asked for: pixels or samples. The next goes once the
  // worker has taken it – a busy worker gets the newest frame, not a queue of them.
  | { t: "frame", id: number, bitmap?: ImageBitmap, samples?: Float32Array }
  | { t: "sampleRate", value: number }
  // the last batch of live values is drawn: send the next
  | { t: "drawn" }
  // a canvas the worker paints pictures into, by a slot the page numbers it with; and
  // the picture to paint there now, with the canvas's size in units
  | { t: "canvas", slot: number, canvas: OffscreenCanvas }
  | { t: "canvasGone", slot: number }
  | { t: "paint", slot: number, id: number, size: [number, number] }
  // the display's pixels a unit (devicePixelRatio), at start and on each change
  | { t: "density", value: number }

export type SourceKind = "camera" | "microphone" | "spectrum" | "pointer"

// worker → page
export type ToPage =
  // the environment's documented names, for hover cards and completion
  | { t: "ready", names: [string, string, Doc | null][] }
  // a program written in an older version, as it is written now
  | { t: "migrated", source: string }
  // a document's parse errors, by its hash
  | { t: "parsed", d: number, errors: { message: string, o: Where }[] }
  // what the program computed that nothing shows: an Error of a statement before the
  // last, a write a control couldn't make – all there are since the program last ran
  | { t: "warnings", warnings: { message: string, o: Where }[] }
  // live values that changed, as they are now; `ack` is the page's last write each has seen
  | { t: "lives", lives: { id: number, desc: Desc, ack: number }[] }
  // a tensor's values, or why they couldn't be computed
  | { t: "tensor", id: number, payload?: Float32Array, error?: string }
  // start or stop a source on the page: the camera, the microphone, the pointer
  | { t: "source", id: number, kind: SourceKind, size: number[], fps?: number }
  | { t: "stop", id: number }
  // the source's last frame is taken
  | { t: "took", id: number }
  | { t: "needSampleRate" }
  // the GPU is gone: the page starts another worker
  | { t: "lost", message: string }

// A short hash of a document's text (cyrb53): an origin names its document by it
export const documentHash = (text: string): number => {
  let h1 = 0xdeadbeef, h2 = 0x41c6ce57
  for (let i = 0; i < text.length; i++) {
    const c = text.charCodeAt(i)
    h1 = Math.imul(h1 ^ c, 2654435761)
    h2 = Math.imul(h2 ^ c, 1597334677)
  }
  h1 = Math.imul(h1 ^ (h1 >>> 16), 2246822507) ^ Math.imul(h2 ^ (h2 >>> 13), 3266489909)
  h2 = Math.imul(h2 ^ (h2 >>> 16), 2246822507) ^ Math.imul(h1 ^ (h1 >>> 13), 3266489909)
  return 4294967296 * (2097151 & h2) + (h1 >>> 0)
}

// MARK: The page's mirror
// The page's half of the count: a mirror of every live value and callback it shows, by
// id, and the numbers of the tensors. Live 0 is held from the start.

type Mirror = {
  // the value now, as the worker last described it
  desc: Signal<Desc | undefined>,
  // descriptions held here that mention it
  refs: number,
  // times the worker sent it: all of them are given back at once, when nothing here
  // mentions it any more
  received: number,
  // the page's last write, and the last one the worker has answered
  sent: number,
  ack: number,
  // what the page wrote, shown until the worker answers
  local: Signal<unknown>,
}
const mirror = (): Mirror => ({ desc: signal(undefined), refs: 0, received: 0, sent: 0, ack: 0, local: signal(undefined) })
export const mirrors = new Map<number, Mirror>()
// the program's value
export const root = mirror()
// a tensor's values, or the message saying why they couldn't be computed
export const payloads = new Map<number, Signal<Float32Array | string | undefined>>()

type Send = (message: ToWorker, transfer?: Transferable[]) => void
let send: Send = () => { }
let whenDrawn = (answer: () => void) => { answer() }

// A worker starts: nothing the last one sent is held. The page answers each batch once
// it is drawn.
export const connect = (post: Send, drawn: (answer: () => void) => void) => {
  send = post
  whenDrawn = drawn
  mirrors.clear()
  payloads.clear()
  root.desc.value = undefined
  mirrors.set(0, root)
}

const parts = (d: Desc): Desc[] =>
  d.k === "list" ? d.items : d.k === "record" ? d.entries.map(([, v]) => v) : d.k === "ui" ? [...Object.values(d.slots), ...d.children] : []
// Every live value and tensor a description mentions is held once more…
const hold = (d: Desc): void => {
  if (d.k === "live" || d.k === "callback") {
    let held = mirrors.get(d.id)
    if (!held) { mirrors.set(d.id, held = mirror()) }
    held.refs++
    held.received++
    if (d.k === "live" && d.now) { show(held, d.now) }
  }
  if (d.k === "tensor") { payloads.set(d.id, signal(d.payload)) }
  parts(d).forEach(hold)
}
// …and once less when it goes. What nothing mentions, the worker may let go of too.
const release = (d: Desc): void => {
  if (d.k === "live" || d.k === "callback") {
    const held = mirrors.get(d.id)
    if (!held || --held.refs > 0) { return }
    mirrors.delete(d.id)
    const now = held.desc.peek()
    if (now) { release(now) }
    send({ t: "drop", id: d.id, count: held.received })
  }
  if (d.k === "tensor") {
    payloads.delete(d.id)
    if (d.paint) { send({ t: "drop", id: d.id, count: 1 }) }
  }
  parts(d).forEach(release)
}
// a mirror holds what its current value mentions
const show = (held: Mirror, desc: Desc) => {
  hold(desc)
  const old = held.desc.peek()
  held.desc.value = desc
  if (old) { release(old) }
}

// The worker's news: live values as they are now, and a tensor's numbers
export const update = (m: Extract<ToPage, { t: "lives" | "tensor" }>) => {
  if (m.t === "tensor") {
    const payload = payloads.get(m.id)
    if (payload) { payload.value = m.error ?? m.payload }
    return
  }
  // once drawn, the worker that sent the batch may send the next
  const answer = send
  whenDrawn(() => answer({ t: "drawn" }))
  for (const { id, desc, ack } of m.lives) {
    const held = mirrors.get(id)
    // nothing here shows it any more: count what it mentions, and give it all back
    if (!held) { hold(desc); release(desc); continue }
    held.ack = ack
    show(held, desc)
  }
}

// The page writes to and calls only what it holds
let writes = 0
export const write = (id: number, value: number | number[] | string | ImageBitmap) => {
  const held = mirrors.get(id)
  if (!held) { return }
  held.sent = ++writes
  const image = typeof value === "object" && !Array.isArray(value)
  if (!image) { held.local.value = value }
  send({ t: "set", id, value, seq: held.sent }, image ? [value] : [])
}
export const call = (id: number) => { if (mirrors.has(id)) { send({ t: "call", id }) } }

// MARK: Lexicon
// The characters each kind of token is made of: the grammar is built from these tables,
// and the editor highlights with the expressions below, so the two never drift apart.

export const OPERATOR_RANGES: Record<string, [string, string]> = {
  BASIC_OPERATORS: ["!", "~"],
  MATH_OPERATORS: ["∀", "⋿"],
  MISCELLANEOUS_TECHNICAL: ["⌀", "⏿"],
  SUPPLEMENTAL_MATH_OPERATORS: ["⨀", "⫿"],
  ARROWS: ["←", "⇿"],
  SUPPLEMENTAL_ARROWS_A: ["⟰", "⟿"],
  SUPPLEMENTAL_ARROWS_B: ["⤀", "⥿"],
  MULTIPLICATION_SIGN: ["×", "×"],
  DIVISION_SIGN: ["÷", "÷"],
  NOT_SIGN: ["¬", "¬"],
  // ⟜ is U+27DC
  MISCELLANEOUS_MATH_SYMBOLS_A: ["⟀", "⟯"],
  MISCELLANEOUS_MATH_SYMBOLS_B: ["⦀", "⧿"],
  // ⚀–⚅; ⚄ is rand
  DIE_FACES: ["⚀", "⚅"],
  MIDDLE_DOT: ["·", "·"],
  // □, the square a pen draws
  WHITE_SQUARE: ["□", "□"],
  // the norm, ‖
  DOUBLE_VERTICAL_LINE: ["‖", "‖"],
  // each, ¨
  DIAERESIS: ["¨", "¨"],
  // ↔︎ is ↔ and U+FE0E
  VARIATION_SELECTORS: ["︀", "️"],
}

export const RESERVED_SYMBOLS = "[|,{}()\\[\\];]"

export const IDENTIFIER_RANGES: Record<string, [string, string]> = {
  MATHEMATICAL_ALPHANUMERIC_SYMBOLS: [String.fromCodePoint(0x1D400), String.fromCodePoint(0x1D7FF)],
  EMOJIS: [String.fromCodePoint(0x1F300), String.fromCodePoint(0x1FFFF)],
}

const charClass = (ranges: Record<string, [string, string]>): string =>
  Object.values(ranges).map(([lo, hi]) => {
    const a = `\\u{${lo.codePointAt(0)!.toString(16)}}`
    return lo === hi ? a : `${a}-\\u{${hi.codePointAt(0)!.toString(16)}}`
  }).join("")
const IDENTIFIER_CHARS = charClass(IDENTIFIER_RANGES)

// a letter or an identifier-range symbol, then letters, digits and those symbols – the
// grammar's `identifier`
export const identifierRegexp = new RegExp(`[\\p{L}${IDENTIFIER_CHARS}][\\p{L}\\p{N}${IDENTIFIER_CHARS}]*`, "u")
export const numberRegexp = /-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/
// A number as a tokenizer finds it, each rule tried in order where the one before fails, a
// group a token. A - before digits is the number's sign where a number can start: at a
// line's start (the ^), and after a space or one of | , { ( [. Glued to anything else –
// `3-4`, `a-4`, `2*-4` – the - is an operator, as the grammar reads it.
export const numberRules: [RegExp, ...("" | "delimiter" | "number")[]][] = [
  [new RegExp(`^${numberRegexp.source}`), "number"],
  [new RegExp(`(\\s+)(${numberRegexp.source})`), "", "number"],
  [new RegExp(`([|,{(\\[])(${numberRegexp.source})`), "delimiter", "number"],
  [new RegExp(numberRegexp.source.replace(/^-\?/, "")), "number"],
]
export const stringRegexp = /"(?:[^"\\]|\\.)*"/
// one character of the grammar's `operator`; identifiers, numbers, strings and delimiters
// are taken first, so the ASCII letters and digits it covers never reach it
export const operatorRegexp = new RegExp(`[${charClass(OPERATOR_RANGES)}]`, "u")
export const delimiterRegexp = new RegExp(RESERVED_SYMBOLS, "u")
