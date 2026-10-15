// MARK: Evaluation host
// The worker side of the IDE. It evaluates the program, binds the widgets and the live
// sources into the environment, and describes values to the page (protocol.ts). Values
// stay here – tensors, signals, variables; the page draws their descriptions and writes
// back through ids.

import { Signal, computed, effect, untracked } from "@preact/signals-core"
import { interpolateViridis } from "d3-scale-chromatic"
import { rgb } from "d3-color"
import {
  CodeParse, evaluateSyntaxTreeNode, CodeEvaluate, createScope, DefaultEnvironment,
  extendEnvironment, setCodeNodePrinter, evaluateGeneration, registerDisposable,
  getAsSyncList, getMeta, setMeta, getOrigin, SignalCreate, SignalUpdate, StringLiteral,
  engine, FluentVariable, isTensor, TensorScalarLive,
  type Value, type SyntaxTreeNode, type Origin,
} from "./language"
import {
  tensorView, documentHash,
  type Desc, type Doc, type Payload, type SourceKind, type ToPage, type ToWorker, type Tree, type TensorView, type Where,
} from "./protocol"
import { Documentation } from "./documentation"
import { TOUR_SOURCE } from "./tour"

type Post = (message: ToPage, transfer?: Transferable[]) => void
let post: Post = () => { }

const nextFrame: (cb: () => void) => void =
  typeof (globalThis as { requestAnimationFrame?: unknown }).requestAnimationFrame === "function"
    ? (cb) => (globalThis as unknown as { requestAnimationFrame: (cb: () => void) => void }).requestAnimationFrame(cb)
    : (cb) => { setTimeout(cb, 16) }

let nextId = 0

// MARK: Widgets
// A widget is a value like any other: the program builds and composes it, the page draws
// it. Props are plain data; slots and children are values, described for the page.
class Widget {
  constructor(
    readonly type: string,
    readonly props: Record<string, unknown> = {},
    readonly slots: Record<string, Value> = {},
    readonly children: Value[] = [],
  ) { }
}
// a function the page may call – a button's action
class Callback { constructor(readonly fn: Function) { } }
// a quoted program, drawn as its syntax tree
class CodeTree { constructor(readonly node: SyntaxTreeNode) { } }

const widget = (type: string, props?: Record<string, unknown>, slots?: Record<string, Value>, children?: Value[]) =>
  new Widget(type, props, slots, children) as unknown as Value

// MARK: Pixels
// Pixels enter the language as float32 RGB in 0–1, and leave as RGBA bytes.

let pixelCanvas: OffscreenCanvas | null = null
const fromPixels = (image: ImageBitmap): engine.Tensor => {
  const { width, height } = image
  pixelCanvas ??= new OffscreenCanvas(1, 1)
  if (pixelCanvas.width !== width || pixelCanvas.height !== height) {
    pixelCanvas.width = width
    pixelCanvas.height = height
  }
  const context = pixelCanvas.getContext("2d", { willReadFrequently: true }) as OffscreenCanvasRenderingContext2D
  // drawImage composites source-over: clear first, or an image with alpha shows the
  // previous one through
  context.clearRect(0, 0, width, height)
  context.drawImage(image, 0, 0)
  image.close()
  const { data } = context.getImageData(0, 0, width, height)
  const flat = new Float32Array(width * height * 3)
  for (let i = 0, j = 0; i < data.length; i += 4, j += 3) {
    flat[j] = data[i]! / 255
    flat[j + 1] = data[i + 1]! / 255
    flat[j + 2] = data[i + 2]! / 255
  }
  return engine.tensor(flat, { shape: [height, width, 3] })
}

const VIRIDIS = Uint8Array.from(Array.from({ length: 256 }, (_, i) => {
  const c = rgb(interpolateViridis(i / 255))
  return [c.r, c.g, c.b]
}).flat())

// what a tensor's view draws, from its values – always fresh memory, so it can be transferred
const payloadOf = (view: TensorView, shape: readonly number[], data: Float32Array): Payload => {
  if (view === "heatImage") {
    // normalized to the tensor's own range, colored through viridis
    let lo = Infinity, hi = -Infinity
    for (const v of data) { if (v < lo) { lo = v } if (v > hi) { hi = v } }
    const span = Math.max(hi - lo, 1e-6), out = new Uint8ClampedArray(data.length * 4)
    for (let i = 0; i < data.length; i++) {
      const t = (data[i]! - lo) / span
      const j = (t > 0 ? Math.round(Math.min(t, 1) * 255) : 0) * 3
      out[4 * i] = VIRIDIS[j]!
      out[4 * i + 1] = VIRIDIS[j + 1]!
      out[4 * i + 2] = VIRIDIS[j + 2]!
      out[4 * i + 3] = 255
    }
    return out
  }
  if (view === "image") {
    // one channel is gray, three are RGB; anything else shows its first channel
    const [height = 1, width = 1, channels = 1] = shape
    const out = new Uint8ClampedArray(height * width * 4)
    for (let p = 0, s = 0; p < out.length; p += 4, s += channels) {
      out[p] = data[s]! * 255
      out[p + 1] = data[channels === 3 ? s + 1 : s]! * 255
      out[p + 2] = data[channels === 3 ? s + 2 : s]! * 255
      out[p + 3] = 255
    }
    return out
  }
  return data.slice()
}

// MARK: Descriptions

// A document is named by its hash; hashing each once per evaluation
const documentIds = new Map<string, number>()
const where = (origin: Origin | undefined): Where | undefined => {
  if (!origin) { return undefined }
  const text = origin.document
  // no document: the main program
  let d = text === undefined ? 0 : documentIds.get(text)
  if (d === undefined) {
    d = documentHash(text!)
    documentIds.set(text!, d)
  }
  return { s: [origin.start.line, origin.start.column], e: [origin.end.line, origin.end.column], d }
}

// A live value the page shows: a signal or a variable. Its effect marks it dirty when it
// changes, and the next frame sends its new description – one per frame however often it
// changes, none while the tensors of the last one are still being read, and none while
// the page is still drawing the last batch.
type Live = {
  id: number,
  target: Signal<Value> | FluentVariable,
  refs: number,
  stop: () => void,
  reading: number,
  ack: number,
}
const ids = new WeakMap<object, number>()
const idOf = (o: object): number => {
  let id = ids.get(o)
  if (id === undefined) {
    id = ++nextId
    ids.set(o, id)
  }
  return id
}
const lives = new Map<number, Live>()
const callbacks = new Map<number, { fn: Function, refs: number }>()
const dirty = new Set<Live>()
// the live whose value is being described: reads of its tensors pace its updates
let owner: Live | null = null
const describing = new Set<number>()
// the transferable memory of the description being built
let transfers: Transferable[] = []

const send = (message: ToPage) => {
  const t = transfers
  transfers = []
  post(message, t)
}

const valueOf = (target: Signal<Value> | FluentVariable): Value => {
  try {
    return target instanceof FluentVariable ? target.current : target.value
  } catch (e) {
    return e instanceof Error ? e : new Error(String(e))
  }
}

let flushScheduled = false
let pageDrawing = false
const scheduleFlush = () => {
  if (flushScheduled || pageDrawing) { return }
  flushScheduled = true
  nextFrame(flush)
}
const flush = () => {
  flushScheduled = false
  if (pageDrawing) { return }
  const batch: { id: number, desc: Desc, ack: number }[] = []
  for (const live of dirty) {
    if (lives.get(live.id) !== live) { dirty.delete(live); continue }
    if (live.reading > 0) { continue }
    dirty.delete(live)
    batch.push({ id: live.id, desc: untracked(() => describeNow(live)), ack: live.ack })
  }
  if (!batch.length) { return }
  send({ t: "lives", lives: batch })
  pageDrawing = true
}
const markDirty = (live: Live) => {
  dirty.add(live)
  scheduleFlush()
}

const describeNow = (live: Live): Desc => {
  const outer = owner
  owner = live
  describing.add(live.id)
  try {
    return describe(valueOf(live.target))
  } finally {
    describing.delete(live.id)
    owner = outer
  }
}

const describeLive = (target: Signal<Value> | FluentVariable): Desc => {
  const id = idOf(target)
  let live = lives.get(id)
  if (!live) {
    const fresh: Live = { id, target, refs: 0, stop: () => { }, reading: 0, ack: 0 }
    let first = true
    fresh.stop = effect(() => {
      if (target instanceof FluentVariable) { target.version.value } else { valueOf(target) }
      if (first) { first = false; return }
      markDirty(fresh)
    })
    lives.set(id, fresh)
    live = fresh
  }
  live.refs++
  // a value that contains itself is described once
  if (describing.has(id)) { return { k: "live", id } }
  return { k: "live", id, now: describeNow(live) }
}

const describeCallback = (fn: Function): Desc => {
  const id = idOf(fn)
  const known = callbacks.get(id)
  if (known) { known.refs++ } else { callbacks.set(id, { fn, refs: 1 }) }
  return { k: "callback", id }
}

const describeTensor = (t: engine.Tensor): Desc => {
  const id = ++nextId, shape = [...t.shape], view = tensorView(shape)
  if (view === "shape") { return { k: "tensor", id, shape } }
  const onHost = engine.hostValue(t)
  if (onHost) {
    const payload = payloadOf(view, shape, onHost)
    transfers.push(payload.buffer as ArrayBuffer)
    return { k: "tensor", id, shape, payload }
  }
  const live = owner
  if (live) { live.reading++ }
  t.read().then(
    (data) => {
      const payload = payloadOf(view, shape, data)
      post({ t: "tensor", id, payload }, [payload.buffer as ArrayBuffer])
    },
    () => { /* the program moved on */ },
  ).finally(() => {
    if (live) {
      live.reading--
      if (dirty.has(live)) { scheduleFlush() }
    }
  })
  return { k: "tensor", id, shape }
}

const describeError = (error: Error): Desc => {
  const chain: Error[] = [error]
  for (let e: unknown = error.cause; e instanceof Error; e = e.cause) { chain.push(e) }
  // an Error's cause can be any value – what was tried, say – and it reads as the source
  // that reproduces it, ahead of the messages
  const payload = chain[chain.length - 1]!.cause
  return {
    k: "error",
    chain: chain.reverse().map((e) => ({ message: e.message, o: where(getOrigin(e)) })),
    ...(payload !== undefined && !(payload instanceof Error) && { carried: StringLiteral(payload as Value) }),
  }
}

const describeFunction = (fn: Function): Desc => {
  const meta = getMeta(fn)
  // Fluent lambdas carry their source; a built-in's JS internals help nobody
  const source = Object.prototype.hasOwnProperty.call(fn, "toString") ? fn.toString() : undefined
  const doc: Doc | undefined = meta.doc ? { signature: meta.signature, doc: meta.doc, example: meta.example } : undefined
  return { k: "function", name: fn.name || "function", source, doc }
}

const labelOf = (node: SyntaxTreeNode): string => {
  switch (node.type) {
    case "Symbol": return node.content.value
    case "Number": return node.content.value.toString()
    // a computed operator, (a, b) . (+ & -), shows as an explicit apply
    case "Operation": return node.content.operator.type === "Operation" ? "." : labelOf(node.content.operator)
    case "Tensor": return "[]"
    case "Lambda": return "{}"
    case "List": return "()"
    case "String": return `"${node.content.value}"`
    case "Code": return "``"
    case "Error": return "⚠"
    default: throw new Error(`Unknown type ${node.type}`)
  }
}
const childrenOf = (node: SyntaxTreeNode): SyntaxTreeNode[] => {
  switch (node.type) {
    case "Program": return node.content
    case "Operation":
      return node.content.operator.type === "Operation"
        ? [node.content.args, node.content.operator]
        : node.content.args.content.value
    case "Tensor": return node.content.value
    case "Lambda":
      return [
        { type: "List", content: { value: node.content.args }, origin: node.origin },
        ...(node.content.expr as SyntaxTreeNode & { type: "Program" }).content,
      ]
    case "List": return node.content.value
    default: return []
  }
}
const treeOf = (node: SyntaxTreeNode): Tree =>
  ({ label: labelOf(node), o: where(node.origin)!, children: childrenOf(node).map(treeOf) })

const describeTree = (node: SyntaxTreeNode): Desc => {
  // a backtick literal whose program failed to parse holds an Error node
  if (node?.type !== "Program") {
    const message = node?.type === "Error" ? node.content : `CodePrint: expected a program, got ${(node as { type?: string })?.type ?? String(node)}`
    return describeError(new Error(message))
  }
  try {
    return { k: "tree", roots: node.content.map(treeOf) }
  } catch (e) {
    return describeError(e instanceof Error ? e : new Error(String(e)))
  }
}

const describeWidget = (w: Widget, depth: number): Desc => ({
  k: "ui",
  uid: idOf(w),
  type: w.type,
  props: w.props,
  slots: Object.fromEntries(Object.entries(w.slots).map(([k, v]) => [k, describe(v, depth + 1)])),
  children: w.children.map((c) => describe(c, depth + 1)),
})

const describeBare = (value: unknown, depth: number): Desc => {
  if (depth > 64) { return { k: "other", v: "…" } }
  if (value === null || value === undefined) { return { k: "null" } }
  if (typeof value === "string" || value instanceof String) { return { k: "string", v: String(value) } }
  if (Array.isArray(value)) { return { k: "list", items: value.map((x) => describe(x, depth + 1)) } }
  if (typeof value === "function") { return describeFunction(value) }
  if (typeof value !== "object") { return { k: "other", v: String(value) } }
  if (value instanceof Signal || value instanceof FluentVariable) { return describeLive(value as Signal<Value> | FluentVariable) }
  if (isTensor(value)) { return describeTensor(value) }
  if (value instanceof Error) { return describeError(value) }
  if (value instanceof Widget) { return describeWidget(value, depth) }
  if (value instanceof Callback) { return describeCallback(value.fn) }
  if (value instanceof CodeTree) { return describeTree(value.node) }
  if (value instanceof Promise) { return { k: "other", v: "! Promise" } }
  return { k: "record", entries: Object.entries(value).map(([key, v]) => [key, describe(v as Value, depth + 1)]) }
}

const describe = (value: Value, depth = 0): Desc => {
  const desc = describeBare(value, depth)
  const o = where(getOrigin(value))
  if (o) { desc.o = o }
  return desc
}

// MARK: Sources
// The camera, the microphone and the pointer live on the page: a source asks the page to
// stream its frames here. Frames that arrive faster than the program uses them are
// coalesced – each source applies its newest frame once per frame.

type Frame = { bitmap?: ImageBitmap, samples?: Float32Array }
const sources = new Map<number, (frame: Frame) => void>()
const pendingFrames = new Map<number, Frame>()
let framesScheduled = false

const applyFrames = () => {
  framesScheduled = false
  for (const [id, frame] of pendingFrames) {
    pendingFrames.delete(id)
    const apply = sources.get(id)
    if (apply) { apply(frame) } else { frame.bitmap?.close() }
  }
}
const receiveFrame = (id: number, frame: Frame) => {
  pendingFrames.get(id)?.bitmap?.close()
  pendingFrames.set(id, frame)
  if (!framesScheduled) {
    framesScheduled = true
    nextFrame(applyFrames)
  }
}

const pageSource = (kind: SourceKind, size: number[], initial: engine.Tensor, read: (frame: Frame) => engine.Tensor | null, fps?: number): Value => {
  const id = ++nextId
  const source = SignalCreate<engine.Tensor>(initial)
  sources.set(id, (frame) => {
    const next = read(frame)
    if (next) { SignalUpdate(source, next) }
  })
  post({ t: "source", id, kind, size, ...(fps !== undefined && { fps }) })
  registerDisposable(() => {
    sources.delete(id)
    pendingFrames.get(id)?.bitmap?.close()
    pendingFrames.delete(id)
    post({ t: "stop", id })
  })
  return computed(() => source.value) as unknown as Value
}

const numberOr = (v: Value | undefined, fallback: number): number => (v === undefined || v === null ? fallback : getAsSyncList(v) as number)

// Async sources resolve after an arbitrary delay; if the evaluation that created them has
// been retired by then, the result is dropped.
const generationRetired = (): (() => boolean) => {
  let retired = false
  registerDisposable(() => { retired = true })
  return () => retired
}

// CORS fallback: retry a blocked cross-origin load through the dev server
const proxied = (url: string) => `/proxy?url=${encodeURIComponent(url)}`

// A download kept in the origin's private file system (OPFS), so reloads and hot updates
// don't fetch large weight files again; without OPFS it is a plain fetch.
const cachedFetch = async (url: string): Promise<Uint8Array> => {
  const dir = await navigator.storage?.getDirectory?.()
    .then((root) => root.getDirectoryHandle("fluent-downloads", { create: true }))
    .catch(() => null)
  const name = encodeURIComponent(url)
  if (dir) {
    try { return new Uint8Array(await (await (await dir.getFileHandle(name)).getFile()).arrayBuffer()) } catch { /* not cached yet */ }
  }
  const response = await fetch(url)
  if (!response.ok) { throw new Error(`${response.status} ${url}`) }
  const bytes = new Uint8Array(await response.arrayBuffer())
  if (dir) {
    try { const w = await (await dir.getFileHandle(name, { create: true })).createWritable(); await w.write(bytes); await w.close() } catch { /* cache is best effort */ }
  }
  return bytes
}

// safetensors: an 8-byte little-endian header length, a JSON header giving each tensor's
// dtype, shape and byte range, then the raw bytes. Every tensor is read as float32.
const halfToFloat = (h: number): number => {
  const e = (h >> 10) & 0x1f, m = h & 0x3ff, sign = h & 0x8000 ? -1 : 1
  return sign * (e === 0 ? m * 2 ** -24 : e === 31 ? (m ? NaN : Infinity) : (1 + m / 1024) * 2 ** (e - 15))
}
const SAFETENSOR_READERS: Record<string, (b: ArrayBuffer, at: number, n: number) => ArrayLike<number | bigint>> = {
  F32: (b, at, n) => new Float32Array(b.slice(at, at + 4 * n)), F64: (b, at, n) => new Float64Array(b.slice(at, at + 8 * n)),
  I32: (b, at, n) => new Int32Array(b.slice(at, at + 4 * n)), I64: (b, at, n) => new BigInt64Array(b.slice(at, at + 8 * n)),
  I16: (b, at, n) => new Int16Array(b.slice(at, at + 2 * n)), I8: (b, at, n) => new Int8Array(b, at, n),
  U8: (b, at, n) => new Uint8Array(b, at, n), BOOL: (b, at, n) => new Uint8Array(b, at, n),
  F16: (b, at, n) => Array.from(new Uint16Array(b.slice(at, at + 2 * n)), halfToFloat),
  BF16: (b, at, n) => Array.from(new Uint16Array(b.slice(at, at + 2 * n)), (h) => new Float32Array(new Uint32Array([h << 16]).buffer)[0]!),
}
const parseSafetensors = (bytes: Uint8Array): [string, engine.Tensor][] => {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength)
  const headerLength = Number(view.getBigUint64(0, true))
  const header = JSON.parse(new TextDecoder().decode(bytes.subarray(8, 8 + headerLength))) as Record<string, { dtype: string, shape: number[], data_offsets: [number, number] }>
  const body = bytes.slice(8 + headerLength).buffer
  return Object.entries(header).filter(([key]) => key !== "__metadata__").map(([key, t]) => {
    const read = SAFETENSOR_READERS[t.dtype]
    if (!read) { throw new Error(`safetensors: unsupported dtype ${t.dtype} for '${key}'`) }
    const n = t.shape.reduce((a, b) => a * b, 1)
    return [key, engine.tensor(Float32Array.from(read(body, t.data_offsets[0], n) as ArrayLike<number>, Number), { shape: t.shape })]
  })
}

// The audio hardware's sample rate (Hz) is the page's to know: until it answers, 48000;
// when it answers with another rate, a program that read the rate runs again.
let sampleRate = 48000
let sampleRateKnown = false
let sampleRateRead = false

// MARK: Environment

const gridTemplate = (t: Value): string => {
  if (!isTensor(t)) { return "" }
  const v = getAsSyncList(t)
  if (t.ndim === 0) { return `repeat(${v}, minmax(auto, 1fr))` }
  if (t.ndim === 1) { return (v as number[]).map((x) => `minmax(0, ${x}fr)`).join(" ") }
  return ""
}

// a [2] point in a data range [[x0, x1], [y0, y1]], as fractions of the drawing
const pointMapping = (range?: Value) => {
  const [[x0, x1], [y0, y1]] = (range ? getAsSyncList(range) : [[0, 1], [0, 1]]) as [[number, number], [number, number]]
  return { x0, y0, spanX: x1 - x0 || 1, spanY: y1 - y0 || 1 }
}

const Text = (value: Value) => {
  if (typeof value !== "string" && !(value instanceof String)) {
    return new Error(`Text: expected string, got ${typeof value}`)
  }
  return widget("Text", { text: String(value) })
}

const Button = (label?: Value, onClick?: Value) =>
  widget("Button", {}, {
    label: label ?? "Button",
    ...(typeof onClick === "function" && { onClick: new Callback(onClick) as unknown as Value }),
  })

// TextEditor(text) – editable always; TextEditor(text, enabled) – read-only while `enabled` is falsy
const TextEditor = (target: Value, enabled?: Value) =>
  widget("TextEditor", {}, { target, ...(enabled !== undefined && { enabled }) })
const Slider = (target: Value) => widget("Slider", {}, { target })
const Checkbox = (target: Value) => widget("Checkbox", {}, { target })
// sensitivity: 0 is stuck, negative is flipped
const Scrubber = (target: Value, sensitivity?: Value) => {
  const step = numberOr(sensitivity, 1)
  return widget("Scrubber", { step, decimals: Math.max(0, Math.ceil(-Math.log10(Math.abs(step) || 1))) }, { target })
}
// Image picker and drop zone: writes the image into `target` as a [h, w, 3] tensor
const ImageUpload = (target: Value) => widget("ImageUpload", {}, { target })

// Grid(cols, rows)(cells…): lists flatten into cells, and a signal holding a list
// re-renders its cells as it changes – Grid(3)(tasks) works when tasks is $((...))
const Grid = (cols: Value, rows?: Value) => {
  const columns = gridTemplate(cols), rowTemplate = rows === undefined ? "" : gridTemplate(rows)
  const cells = (...children: Value[]) => widget("Grid", { columns, rows: rowTemplate }, {}, children)
  setMeta(cells, { noAutoLift: true })
  return cells
}
// Center(Text("Fluent Tour")): a child centered in its cell, both axes
const Center = (child: Value) => widget("Center", {}, {}, [child])
// Layers(surface, Point2D(θ, range)): the first child defines the size, the rest overlay it
const Layers = (...children: Value[]) => widget("Layers", {}, {}, children)

// A draggable point bound to a [2] variable (or signal), mapped to a data range; dragging
// assigns, and assignments (an optimizer's) move the dot
const Point2D = (target: Value, range?: Value, color?: Value) =>
  widget("Point2D", { ...pointMapping(range), color: String(color ?? "white") }, { target })
// The path a point has taken
const Trail = (target: Value, range?: Value, color?: Value) =>
  widget("Trail", { ...pointMapping(range), color: String(color ?? "white") }, { target })

// PointPlot(x, y) draws y against x with connecting lines and dots; PointPlot(y) alone
// plots y against its index
const PointPlot = (x: Value, y?: Value) => {
  if (x instanceof Error) { return x }
  if (y instanceof Error) { return y }
  const axis = (v: Value, what: string): number[] | Error => {
    const data = getAsSyncList(v)
    if (typeof data === "number") { return [data] }
    if (!Array.isArray(data)) {
      return new Error(`\`PointPlot\`: ${what} must be a tensor – write a list as [ … ]`)
    }
    return data as number[]
  }
  const ys = axis(y ?? x, y === undefined ? "the argument" : "y")
  if (ys instanceof Error) { return ys }
  const xs = y === undefined ? ys.map((_, i) => i) : axis(x, "x")
  if (xs instanceof Error) { return xs }
  return widget("PointPlot", { xs, ys })
}

const Code = (source: Value) => widget("Code", {}, { source })
// CodeEditor(code) fills its container; CodeEditor(code, "auto") sizes itself to its
// content and grows as you type – for editors in auto-sized rows
const CodeEditor = (source: Value, sizing?: Value) =>
  widget("CodeEditor", { auto: String(sizing ?? "") === "auto" }, { source })

const Print = (value: Value) => widget("Print", {}, {}, [value])
const PrettyPrint = (value: Value) => widget("PrettyPrint", {}, {}, [value])
const CodePrint = (node: Value) => new CodeTree(node as unknown as SyntaxTreeNode) as unknown as Value

for (const f of [Button, TextEditor, Slider, Checkbox, Scrubber, ImageUpload, Center, Layers, Point2D, Trail, Code, CodeEditor, Print]) {
  setMeta(f, { noAutoLift: true })
}

const MousePosition = () =>
  pageSource("pointer", [2], engine.tensor([0.5, 0.5]), (f) => (f.samples ? engine.tensor(f.samples) : null))

const Camera = (width?: Value, height?: Value, fps?: Value) => {
  const w = numberOr(width, 640), h = numberOr(height, 480)
  // black until the camera starts, so the program's math has a frame to work on
  return pageSource("camera", [w, h], engine.zeros([h, w, 3]), (f) => (f.bitmap ? fromPixels(f.bitmap) : null), numberOr(fps, 30))
}
const Microphone = (bufferSize?: Value) => {
  const size = numberOr(bufferSize, 2048)
  return pageSource("microphone", [size], engine.zeros([size]), (f) => (f.samples ? engine.tensor(f.samples) : null))
}
// the spectrum's magnitudes, normalized to 0–1
const MicrophoneSpectrum = (bufferSize?: Value) => {
  const size = numberOr(bufferSize, 1024)
  return pageSource("spectrum", [size], engine.zeros([size]), (f) => (f.samples ? engine.tensor(f.samples) : null))
}

// seconds since the program started, every frame
const Time = () => {
  const start = performance.now()
  const time = SignalCreate<engine.Tensor>(TensorScalarLive(0))
  let active = true
  const tick = () => {
    if (!active) { return }
    SignalUpdate(time, TensorScalarLive((performance.now() - start) / 1000))
    nextFrame(tick)
  }
  nextFrame(tick)
  registerDisposable(() => { active = false })
  return computed(() => time.value) as unknown as Value
}

const SampleRate = () => {
  sampleRateRead = true
  if (!sampleRateKnown) { post({ t: "needSampleRate" }) }
  return TensorScalarLive(sampleRate)
}

const Fetch = (url: Value) => {
  const text = SignalCreate<Value>(null)
  fetch(String(url))
    .then((response) => response.text())
    .then((data) => { text.value = data })
    .catch((error) => { text.value = new Error(`Error fetching data: ${error.message}`) })
  return text
}

const LoadTensorFromImageUrl = (url: Value) => {
  const image = SignalCreate<engine.Tensor | null>(null)
  const retired = generationRetired()
  const load = (u: string) => fetch(u).then((r) => {
    if (!r.ok) { throw new Error(`${r.status} ${u}`) }
    return r.blob()
  }).then((blob) => createImageBitmap(blob))
  // when the proxy can't help either, the first failure is the one that says why
  load(String(url))
    .catch((first) => load(proxied(String(url))).catch(() => { throw first }))
    .then((bitmap) => {
      if (retired()) { bitmap.close(); return }
      SignalUpdate(image, fromPixels(bitmap))
    })
    .catch((e) => { if (!retired()) { image.value = new Error(`LoadTensorFromImageUrl: ${e?.message ?? e}`) as unknown as engine.Tensor } })
  return image
}

const LoadSafeTensorFromURL = (rawUrl?: Value) => {
  if (rawUrl == null) { return new Error("loadSafeTensorFromURL: url is required") }
  const url = String(rawUrl)
  // an empty list until it loads – readers can gate on `#(data()) > 0` – and an Error
  // if it can't
  const tensors = SignalCreate<unknown>([])
  const retired = generationRetired()
  cachedFetch(url)
    .catch((first) => cachedFetch(proxied(url)).catch(() => { throw first }))
    .then((bytes) => {
      if (retired()) { return }
      tensors.value = parseSafetensors(bytes)
    })
    .catch((e) => { if (!retired()) { tensors.value = new Error(`LoadSafeTensorFromURL: ${e?.message ?? e}`) } })
  return tensors
}
// both resolve later – lifting would re-invoke them (and re-fetch) on every change
setMeta(LoadSafeTensorFromURL, { noAutoLift: true })
setMeta(LoadTensorFromImageUrl, { noAutoLift: true })

// MARK: Running the program

let source: string | null = null
let stopProgram: (() => void) | null = null
let runScheduled = false

// a burst of edits runs only the latest
const scheduleRun = () => {
  if (runScheduled) { return }
  runScheduled = true
  setTimeout(run, 0)
}

// Every value of the last program is let go: its lives stop sending, and the page drops
// what it shows of them when the new result arrives.
const releaseAll = () => {
  pageDrawing = false
  for (const live of lives.values()) { live.stop() }
  lives.clear()
  callbacks.clear()
  dirty.clear()
  documentIds.clear()
}

// The program runs inside an effect: a signal it reads at its top level – not inside a
// $ or a lambda – runs it again when it changes. once() reads without that.
const run = () => {
  runScheduled = false
  if (source === null) { return }
  stopProgram?.()
  releaseAll()
  const code = source
  let first = true
  stopProgram = effect(() => {
    if (!first) { scheduleRun(); return }
    first = false
    sampleRateRead = false
    let value: Value
    try {
      value = evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(code), createScope()) ?? null)
    } catch (e) {
      value = e instanceof Error ? e : new Error(String(e))
    }
    untracked(() => send({ t: "result", desc: describe(value) }))
  })
}

// a value written by the page, as the language holds it
const fromPage = (value: number | number[] | string | ImageBitmap): Value =>
  typeof value === "number" ? TensorScalarLive(value)
    : Array.isArray(value) ? engine.tensor(value)
      : typeof value === "string" ? value
        : fromPixels(value)

const receive = (m: ToWorker) => {
  switch (m.t) {
    case "code": {
      source = m.source
      scheduleRun()
      return
    }
    case "set": {
      const live = lives.get(m.id)
      if (!live) {
        if (typeof m.value === "object" && !Array.isArray(m.value)) { m.value.close() }
        return
      }
      const value = fromPage(m.value)
      if (live.target instanceof FluentVariable) {
        if (isTensor(value)) { live.target.assign(value) }
      } else {
        SignalUpdate(live.target, value)
      }
      // answered even when the write changed nothing (a read-only signal): the page
      // stops showing its own value once it hears back
      live.ack = m.seq
      markDirty(live)
      return
    }
    case "call": {
      const callback = callbacks.get(m.id)
      if (!callback) { return }
      try { callback.fn() } catch (e) { console.error(e) }
      return
    }
    case "drop": {
      const live = lives.get(m.id)
      if (live) {
        live.refs -= m.count
        if (live.refs <= 0) {
          live.stop()
          lives.delete(m.id)
          dirty.delete(live)
        }
      }
      const callback = callbacks.get(m.id)
      if (callback) {
        callback.refs -= m.count
        if (callback.refs <= 0) { callbacks.delete(m.id) }
      }
      return
    }
    case "frame": {
      receiveFrame(m.id, { bitmap: m.bitmap, samples: m.samples })
      return
    }
    case "drawn": {
      pageDrawing = false
      if (dirty.size) { scheduleFlush() }
      return
    }
    case "sampleRate": {
      sampleRateKnown = true
      if (m.value === sampleRate) { return }
      sampleRate = m.value
      if (sampleRateRead) { scheduleRun() }
      return
    }
  }
}

// every name in the environment, with its doc card – for hover and completion on the page
const environmentNames = (): [string, string, Doc | null][] => {
  const scope = createScope(), names: [string, string, Doc | null][] = []
  for (const key in scope) {
    const value = scope[key]
    const meta = typeof value === "function" ? getMeta(value) : {}
    const detail = meta.signature ?? (typeof value === "function" ? "function" : String(value).slice(0, 50))
    names.push([key, detail, meta.doc ? { signature: meta.signature, doc: meta.doc, example: meta.example } : null])
  }
  return names
}

export const startHost = (postMessage: Post): ((message: ToWorker) => void) => {
  post = postMessage
  setCodeNodePrinter((node) => new CodeTree(node) as unknown as Value)
  extendEnvironment({
    Documentation: Text(Documentation),
    CodePrint,
    Button, Center, Checkbox, Grid, ImageUpload, Layers, MousePosition, Point2D, PointPlot, Trail, Slider, Scrubber,
    Text, TextEditor, Code, CodeEditor,
    Fetch, LoadSafeTensorFromURL, LoadTensorFromImageUrl, Camera, Microphone, MicrophoneSpectrum, SampleRate, Time,
    Print, PrettyPrint,
  } as Record<string, Value>)
  // The Tour is Fluent source: each read builds a fresh one in a fresh scope, so every
  // evaluation gets its own live tour. Non-enumerable, so listing the environment never
  // builds one.
  Object.defineProperty(DefaultEnvironment, "Tour", {
    get: () => CodeEvaluate.call(createScope(), TOUR_SOURCE),
    enumerable: false,
    configurable: true,
  })
  post({ t: "ready", names: environmentNames() })
  return receive
}
