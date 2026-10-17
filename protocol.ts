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

// A tensor's values as its view draws them: the numbers, or the pixels of an image or
// a heatmap, colored in the worker
export type Payload = Float32Array | Uint8ClampedArray

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
  // the payload is here when the values were on the host; otherwise it follows as a
  // "tensor" message. A picture with `paint` never crosses: the page hands the worker a
  // canvas and asks it to paint the picture there
  | { k: "tensor", id: number, shape: number[], payload?: Payload, paint?: true }
  // a signal or a variable and its value now; later values arrive as "live" messages
  | { k: "live", id: number, now?: Desc }
  | { k: "callback", id: number }
  // a widget: `uid` is the widget's identity – a different widget never reuses another's
  // mounted state
  | { k: "ui", uid: number, type: string, props: Record<string, unknown>, slots: Record<string, Desc>, children: Desc[] }
)

// How a tensor shows, by its shape – both sides agree: the worker sends what the view needs
export type TensorView = "number" | "bars" | "heatPlot" | "heatImage" | "image" | "shape"
export const tensorView = (shape: readonly number[]): TensorView => {
  if (shape.length === 0) { return "number" }
  if (shape.length === 1) { return "bars" }
  if (shape.length === 2) { return (shape[0] ?? 0) * (shape[1] ?? 0) > 400 ? "heatImage" : "heatPlot" }
  if (shape.length === 3) { return "image" }
  return "shape"
}

// page → worker
export type ToWorker =
  | { t: "code", source: string }
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
  // the picture to paint there now
  | { t: "canvas", slot: number, canvas: OffscreenCanvas }
  | { t: "canvasGone", slot: number }
  | { t: "paint", slot: number, id: number }

export type SourceKind = "camera" | "microphone" | "spectrum" | "pointer"

// worker → page
export type ToPage =
  // the environment's documented names, for hover cards and completion
  | { t: "ready", names: [string, string, Doc | null][] }
  | { t: "result", desc: Desc }
  // live values that changed, as they are now; `ack` is the page's last write each has seen
  | { t: "lives", lives: { id: number, desc: Desc, ack: number }[] }
  // a tensor's values, or why they couldn't be computed
  | { t: "tensor", id: number, payload?: Payload, error?: string }
  // start or stop a source on the page: the camera, the microphone, the pointer
  | { t: "source", id: number, kind: SourceKind, size: number[], fps?: number }
  | { t: "stop", id: number }
  | { t: "needSampleRate" }

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
