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

export type Doc = { signature?: string, doc?: string, example?: string }

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
  // A number, or a few bars, shows its values: here when they were on the host, otherwise
  // in a "tensor" message that follows. A picture with `paint` never crosses: the page
  // hands the worker a canvas to paint it in, and gets the range it spans as its values.
  | { k: "tensor", id: number, shape: number[], payload?: Float32Array, paint?: true }
  // a signal or a variable and its value now; later values arrive as "live" messages
  | { k: "live", id: number, now?: Desc }
  | { k: "callback", id: number }
  // a widget: `uid` is the widget's identity – a different widget never reuses another's
  // mounted state
  | { k: "ui", uid: number, type: string, props: Record<string, unknown>, slots: Record<string, Desc>, children: Desc[] }
)

// How a tensor shows, by its rank – both sides agree: the worker sends what the view needs
export type TensorView = "number" | "bars" | "heat" | "image" | "shape"
export const tensorView = (shape: readonly number[]): TensorView =>
  (["number", "bars", "heat", "image"] as const)[shape.length] ?? "shape"
// bars or heatmap cells this few show their values, so they cross as numbers
export const FEW = 20

// page → worker
export type ToWorker =
  | { t: "code", source: string }
  // parse a document for its editor's markers
  | { t: "parse", source: string }
  // write a live value: a number, numbers, text, or an image. `seq` comes back as the
  // "live" message's `ack`, so the page knows when the worker has seen its writes
  | { t: "set", id: number, value: number | number[] | string | ImageBitmap, seq: number }
  | { t: "call", id: number }
  | { t: "drop", id: number, count: number }
  // a frame from a source the worker asked for: pixels or samples
  | { t: "frame", id: number, bitmap?: ImageBitmap, samples?: Float32Array }
  | { t: "sampleRate", value: number }
  // the last batch of live values is drawn: send the next
  | { t: "drawn" }
  // a canvas the worker paints pictures into, by a slot the page numbers it with; and
  // the picture to paint there now, with the canvas's size in device pixels
  | { t: "canvas", slot: number, canvas: OffscreenCanvas }
  | { t: "canvasGone", slot: number }
  | { t: "paint", slot: number, id: number, size: [number, number] }

export type SourceKind = "camera" | "microphone" | "spectrum" | "pointer"

// worker → page
export type ToPage =
  // the environment's documented names, for hover cards and completion
  | { t: "ready", names: [string, string, Doc | null][] }
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
  // the norm, ‖
  DOUBLE_VERTICAL_LINE: ["‖", "‖"],
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
export const stringRegexp = /"(?:[^"\\]|\\.)*"/
// one operator symbol – the grammar's `specialChar`; identifiers, numbers, strings and
// delimiters are taken first, so the ASCII letters and digits it covers never reach it
export const operatorRegexp = new RegExp(`[${charClass(OPERATOR_RANGES)}]`, "u")
export const delimiterRegexp = new RegExp(RESERVED_SYMBOLS, "u")
