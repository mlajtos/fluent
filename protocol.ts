// The page and the evaluation worker talk in plain data. The worker owns every value;
// the page sees descriptions of them, and the live parts – a signal, a variable, a
// callback – as ids it renders, writes back to, or calls.
//
// The worker sends the changes of live values in batches, at most one a frame, and the
// next only after the page has drawn the last – a page that draws slowly gets fewer,
// fresher batches instead of a growing queue.
//
// A live id is held by the descriptions that mention it. The worker counts how many
// times it sent an id, the page counts how many it received; when the page stops
// showing one it releases all it received in a single "drop", and the worker lets go
// once its count reaches zero – a release can never race a send.

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
  // A number shows its value: here when it was on the host, otherwise in a "tensor"
  // message that follows. A picture with `paint` never crosses: the page hands the worker
  // a canvas to paint it in, and gets numbers for its labels – the values when they are
  // fewer than FEW, otherwise the finite range they span. An image has no numbers.
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
// bars or heatmap cells this few are labelled with their values, so they cross as numbers
export const FEW = 20

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
  | { t: "result", desc: Desc }
  // a document's parse errors, by its hash
  | { t: "parsed", d: number, errors: { message: string, o: Where }[] }
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
