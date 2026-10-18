// MARK: Evaluation host
// The worker side of the IDE. It evaluates the program, binds the widgets and the live
// sources into the environment, and describes values to the page (protocol.ts). Values
// stay here – tensors, signals, variables; the page draws their descriptions and writes
// back through ids.

import {
  CodeParse, getParseErrors, evaluateSyntaxTreeNode, CodeEvaluate, createScope, DefaultEnvironment,
  extendEnvironment, setCodeNodePrinter, evaluateGeneration, registerDisposable,
  getAsSyncList, getMeta, setMeta, getOrigin, SignalCreate, SignalUpdate, StringLiteral,
  Signal, computed, signal, onWrite, awaiting, occurrence, keepingFrom, density,
  engine, isTensor, TensorScalarLive, asError, nextFrame,
  type Value, type SyntaxTreeNode, type Origin, type CurrentScope,
} from "./language"
import {
  tensorView, labelled, heatScale, documentHash,
  type Desc, type Doc, type SourceKind, type TensorView, type ToPage, type ToWorker, type Tree, type Where,
} from "./protocol"
import { Documentation } from "./documentation"
import { TOUR_SOURCE } from "./tour"
import { CpuPainter, GpuPainter, type Picture } from "./painter"

type Post = (message: ToPage, transfer?: Transferable[]) => void
let post: Post = () => { }

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
// Pixels enter the language as float32 RGB in 0–1.

let pixelCanvas: OffscreenCanvas | null = null
const fromPixels = (image: ImageBitmap): engine.Tensor => {
  const { width, height } = image
  pixelCanvas ??= new OffscreenCanvas(1, 1)
  if (pixelCanvas.width !== width || pixelCanvas.height !== height) { Object.assign(pixelCanvas, { width, height }) }
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

// MARK: Descriptions

// A document is named by its hash, each hashed once per evaluation; no document is the
// main program, 0
const documentIds = new Map<string, number>()
const where = (origin: Origin | undefined): Where | undefined => {
  if (!origin) { return undefined }
  const text = origin.document
  let d = text === undefined ? 0 : documentIds.get(text)
  if (d === undefined) { documentIds.set(text!, d = documentHash(text!)) }
  return { s: [origin.start.line, origin.start.column], e: [origin.end.line, origin.end.column], d }
}

// A live value the page shows: a signal or a variable. Showing it is sampling it: after a
// write, the next frame looks at every live and sends the new description of each one
// whose value changed – none while the tensors of the last one are still being read, and
// none while the page is still drawing the last batch.
type Live = {
  id: number,
  target: Signal,
  refs: number,
  // the instant of the value it showed last, and the value it shows next
  at: number,
  sample?: { value: Value },
  // looked at again no sooner than this
  quiet: number,
  reading: number,
  ack: number,
}
const ids = new WeakMap<object, number>()
const idOf = (o: object): number => {
  if (!ids.has(o)) { ids.set(o, ++nextId) }
  return ids.get(o)!
}
const lives = new Map<number, Live>()
const callbacks = new Map<number, { fn: Function, refs: number }>()
const dirty = new Set<Live>()
// the live whose value is being described: reads of its tensors pace its updates
let owner: Live | null = null
// The worker paints pictures into canvases the page hands it: a picture waits here, by
// id, for the page to ask for it, until the page lets go of it
let painter: GpuPainter | CpuPainter = new CpuPainter()
const pictures = new Map<number, Picture>()
const describing = new Set<number>()
// the transferable memory of the description being built
let transfers: Transferable[] = []

const send = (message: ToPage) => {
  const t = transfers
  transfers = []
  post(message, t)
}

const valueOf = (target: Signal): Value => {
  try {
    return target.value
  } catch (e) {
    return asError(e)
  }
}
// A live's value now, if it changed since it was shown. A sample that started reads from
// the GPU (text decoded from codes) is shown once they arrive, and the page keeps the last
// one until then: a value that changes every frame shows a little late, never blank.
// Looking at a live takes at most a twentieth of the worker: after a sample that took
// s ms, its next waits 19s ms – a cheap value is shown every frame, a costly one less
// often, and the program's own steps keep the rest.
const sampleOf = (live: Live): void => {
  const start = performance.now()
  if (start < live.quiet) { stale = true; return }
  const [value, reads] = awaiting(() => valueOf(live.target))
  live.quiet = performance.now() + 19 * (performance.now() - start)
  if (live.target.at === live.at) { return }
  live.at = live.target.at
  live.sample = { value }
  dirty.add(live)
  if (!reads.length) { return }
  live.reading++
  Promise.allSettled(reads).then(() => { live.reading--; markDirty() })
}

let flushScheduled = false
let pageDrawing = false
// a write happened: the next display frame looks at what changed – once a frame, however
// often the program writes
let stale = false
const markDirty = (live?: Live) => {
  if (live) { dirty.add(live) } else { stale = true }
  if (flushScheduled || pageDrawing) { return }
  flushScheduled = true
  nextFrame(() => flush(true))
}
onWrite(() => markDirty())
const flush = (frame = false) => {
  if (frame) { flushScheduled = false }
  // the frame before the last is still on the GPU: the next waits for it
  if (pageDrawing || painter.pending > 1) { if (stale) { markDirty() } return }
  if (stale && !frame) { markDirty() }
  if (stale && frame) {
    stale = false
    // a sample waiting to be shown is shown before the next is taken
    for (const live of lives.values()) { if (live.reading === 0 && !live.sample) { sampleOf(live) } }
    // a live still waiting its turn is looked at in a later frame
    if (stale) { markDirty() }
  }
  const batch: { id: number, desc: Desc, ack: number }[] = []
  for (const live of dirty) {
    if (live.reading > 0) { continue }
    dirty.delete(live)
    batch.push({ id: live.id, desc: describeNow(live), ack: live.ack })
  }
  if (!batch.length) { return }
  send({ t: "lives", lives: batch })
  pageDrawing = true
}

const describeNow = (live: Live): Desc => {
  const outer = owner
  owner = live
  describing.add(live.id)
  try {
    const value = live.sample ? live.sample.value : valueOf(live.target)
    if (!live.sample) { live.at = live.target.at }
    live.sample = undefined
    return describe(value)
  } finally {
    describing.delete(live.id)
    owner = outer
  }
}

const describeLive = (target: Signal): Desc => {
  const id = idOf(target)
  let live = lives.get(id)
  if (!live) {
    live = { id, target, refs: 0, at: -1, quiet: 0, reading: 0, ack: 0 }
    lives.set(id, live)
  }
  live.refs++
  // a value that contains itself is described once
  return describing.has(id) ? { k: "live", id } : { k: "live", id, now: describeNow(live) }
}

const describeCallback = (fn: Function): Desc => {
  const id = idOf(fn), known = callbacks.get(id)
  if (known) { known.refs++ } else { callbacks.set(id, { fn, refs: 1 }) }
  return { k: "callback", id }
}

// A tensor's numbers, as they cross: here when they are on the host, otherwise read and
// sent after – fresh memory either way, so they can be transferred
const numbersOf = (id: number, t: engine.Tensor): { payload?: Float32Array } => {
  const onHost = engine.hostValue(t)?.slice()
  if (onHost) {
    transfers.push(onHost.buffer)
    return { payload: onHost }
  }
  const live = owner
  if (live) { live.reading++ }
  t.read().then(
    (data) => {
      const payload = data.slice()
      post({ t: "tensor", id, payload }, [payload.buffer])
    },
    (e) => { post({ t: "tensor", id, error: asError(e).message }) },
  ).finally(() => {
    if (!live) { return }
    live.reading--
    if (dirty.has(live)) { flush() }
  })
  return {}
}

// How a tensor looks is the prelude's own heat and bars (prelude.fl, Drawing), so a
// program that binds its own doesn't change how values show
let seeing: Record<"DrawHeatOf" | "DrawTiles" | "DrawBarsOf" | "DrawExtent", (...args: Value[]) => Value>
const tensorOf = (v: Value): engine.Tensor => {
  if (v instanceof Error) { throw v }
  return v as engine.Tensor
}

// A vector is drawn through a window a power of two long, NaN past its end, with its
// length as data: the engine compiles a plan for each shape, and a vector that grows a
// value a frame – a loss curve – keeps its plan until it doubles
const windowOf = (t: engine.Tensor): engine.Tensor => {
  const n = t.size, N = 2 ** Math.ceil(Math.log2(n))
  if (N === n) { return t }
  const at = Float32Array.from({ length: N }, (_, i) => Math.min(i, n - 1)), inside = Float32Array.from({ length: N }, (_, i) => +(i < n))
  return engine.where(engine.tensor(inside), engine.take(t, engine.tensor(at)), NaN)
}

// the display's pixels a unit, now
const densityNow = (): number => getAsSyncList(density.peek()) as number

// A tensor's picture, and the numbers that cross with it for the page's labels: its values
// when they are labelled, otherwise the finite range they span. A heatmap and bars are made
// when they are painted, at the display's density – a heatmap with its maps laid out and
// magnified, its size in units, bars for the canvas's size – and live only until they are
// on the screen.
const prepare = (view: TensorView, t: engine.Tensor): { picture: Picture, numbers: engine.Tensor, size?: [number, number] } => {
  const numbers = (range: engine.Tensor) => (labelled(t.shape) ? t : range)
  if (view === "image") {
    const range = tensorOf(seeing.DrawExtent(t))
    engine.materialize([t, range])
    return { picture: t, numbers: range }
  }
  if (!t.size) { return { picture: () => null, numbers: t } }
  if (view === "heat") {
    // the maps' layout is known before anything is computed
    const range = tensorOf(seeing.DrawExtent(t)), [h = 1, w = 1] = tensorOf(seeing.DrawTiles(t)).shape, k = heatScale(h, w)
    engine.materialize([t, range])
    return { picture: () => tensorOf(seeing.DrawHeatOf(t, range, engine.scalar(Math.max(1, Math.round(k * densityNow()))))), numbers: numbers(range), size: [w * k, h * k] }
  }
  const v = windowOf(t), n = engine.scalar(t.size), range = tensorOf(seeing.DrawExtent(v))
  engine.materialize([t, v, range])
  return {
    picture: ([w, h]) => (w >= 1 && h >= 1 ? tensorOf(seeing.DrawBarsOf(v, n, range, ...[w, h, roomy(w), roomy(h), densityNow()].map(engine.scalar))) : null),
    numbers: numbers(range),
  }
}
// Bars are drawn in a picture a little larger than their canvas – each side rounded up to
// 128 pixels – so a canvas resized within that keeps its plan
const roomy = (s: number) => Math.ceil(s / 128) * 128

const describeTensor = (t: engine.Tensor): Desc => {
  const id = ++nextId, shape = [...t.shape], view = tensorView(shape)
  try {
    if (view === "number") { return { k: "tensor", id, shape, ...numbersOf(id, t) } }
    const { picture, numbers, size } = prepare(view, t)
    pictures.set(id, picture)
    return { k: "tensor", id, shape, paint: true, ...numbersOf(id, numbers), ...(size && { size }) }
  } catch (e) {
    return describeError(asError(e))
  }
}

const describeError = (error: Error): Desc => {
  const chain: Error[] = [error]
  for (let e: unknown = error.cause; e instanceof Error; e = e.cause) { chain.push(e) }
  // an Error's cause can be any value – what was tried, say – and it reads as the source
  // that reproduces it, ahead of the messages
  const payload = chain.at(-1)!.cause
  return {
    k: "error",
    chain: chain.reverse().map((e) => ({ message: e.message, o: where(getOrigin(e)) })),
    ...(payload !== undefined && !(payload instanceof Error) && { carried: StringLiteral(payload as Value) }),
  }
}

// What the program computed that nothing shows, marked where it arose: one per place
const warnings = new Map<string, { message: string, o: Where }>()
const warn = (error: Error, origin: Origin | undefined) => {
  const o = where(origin)
  if (o) { warnings.set(JSON.stringify(o), { message: error.message, o }) }
}
const sendWarnings = () => post({ t: "warnings", warnings: [...warnings.values()] })

// a built-in's doc card, from its metadata
const docOf = (value: unknown): Doc | undefined => {
  const meta = typeof value === "function" ? getMeta(value) : {}
  return meta.doc ? { signature: meta.signature, doc: meta.doc, example: meta.example } : undefined
}

// a function shows the expression that made it; a built-in's JS internals help nobody
const describeFunction = (fn: Function): Desc =>
  ({ k: "function", name: fn.name || "function", source: getOrigin(fn)?.source, doc: docOf(fn) })

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
    case "Operation": return node.content.operator.type === "Operation" ? [node.content.args, node.content.operator] : node.content.args.content.value
    case "Tensor": return node.content.value
    case "Lambda": return [
      { type: "List", content: { value: node.content.args }, origin: node.origin },
      ...(node.content.expr as SyntaxTreeNode & { type: "Program" }).content,
    ]
    case "List": return node.content.value
    default: return []
  }
}
const treeOf = (node: SyntaxTreeNode): Tree => ({ label: labelOf(node), o: where(node.origin)!, children: childrenOf(node).map(treeOf) })

const describeTree = (node: SyntaxTreeNode): Desc => {
  // a backtick literal whose program failed to parse holds an Error node
  if (node?.type !== "Program") {
    return describeError(new Error(node?.type === "Error" ? node.content : `CodePrint: expected a program, got ${(node as { type?: string })?.type ?? String(node)}`))
  }
  try {
    return { k: "tree", roots: node.content.map(treeOf) }
  } catch (e) {
    return describeError(asError(e))
  }
}

const describeBare = (value: unknown, depth: number): Desc => {
  const inner = (v: unknown) => describe(v as Value, depth + 1)
  if (depth > 64) { return { k: "other", v: "…" } }
  if (value === null || value === undefined) { return { k: "null" } }
  if (typeof value === "string" || value instanceof String) { return { k: "string", v: String(value) } }
  if (Array.isArray(value)) { return { k: "list", items: value.map(inner) } }
  if (typeof value === "function") { return describeFunction(value) }
  if (typeof value !== "object") { return { k: "other", v: String(value) } }
  if (value instanceof Signal) { return describeLive(value) }
  if (isTensor(value)) { return describeTensor(value) }
  if (value instanceof Error) { return describeError(value) }
  if (value instanceof Widget) {
    const slots = Object.fromEntries(Object.entries(value.slots).map(([k, v]) => [k, inner(v)]))
    return { k: "ui", uid: idOf(value), type: value.type, props: value.props, slots, children: value.children.map(inner) }
  }
  if (value instanceof Callback) { return describeCallback(value.fn) }
  if (value instanceof CodeTree) { return describeTree(value.node) }
  if (value instanceof Promise) { return { k: "other", v: "! Promise" } }
  return { k: "record", entries: Object.entries(value).map(([key, v]) => [key, inner(v)]) }
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
// frames of samples – the pointer's position, the microphone's waveform or spectrum
const samples = (f: Frame) => (f.samples ? engine.tensor(f.samples) : null)

const numberOr = (v: Value | undefined, fallback: number): number => (v === undefined || v === null ? fallback : getAsSyncList(v) as number)

// A value that arrives later: the signal holds `initial` until then, or an Error if it
// can't arrive. An evaluation retired by the time it arrives drops it.
const arriving = (failure: string, initial: unknown, load: () => Promise<unknown>) => {
  const value = SignalCreate<Value>(initial as Value)
  let retired = false
  registerDisposable(() => { retired = true })
  load().then(
    (v) => { if (!retired) { SignalUpdate(value, v as Value) } },
    (e) => { if (!retired) { value.value = new Error(`${failure}: ${e?.message ?? e}`) } },
  )
  return value as unknown as Value
}

// a response that is not ok is an Error
const fetched = async (url: string): Promise<Response> => {
  const response = await fetch(url)
  if (!response.ok) { throw new Error(`${response.status} ${url}`) }
  return response
}
// CORS fallback: a blocked cross-origin load is retried through the dev server; when the
// proxy can't help either, the first failure is the one that says why
const withProxy = <T>(url: string, load: (url: string) => Promise<T>): Promise<T> =>
  load(url).catch((first) => load(`/proxy?url=${encodeURIComponent(url)}`).catch(() => { throw first }))

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
  const bytes = new Uint8Array(await (await fetched(url)).arrayBuffer())
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
const bfloatToFloat = (h: number): number => new Float32Array(Uint32Array.of(h << 16).buffer)[0]!
const SAFETENSOR_DTYPES: Record<string, [{ new(b: ArrayBuffer): ArrayLike<number | bigint>, BYTES_PER_ELEMENT: number }, (x: number) => number]> = {
  F32: [Float32Array, Number], F64: [Float64Array, Number], I32: [Int32Array, Number], I64: [BigInt64Array, Number],
  I16: [Int16Array, Number], I8: [Int8Array, Number], U8: [Uint8Array, Number], BOOL: [Uint8Array, Number],
  F16: [Uint16Array, halfToFloat], BF16: [Uint16Array, bfloatToFloat],
}
const parseSafetensors = (bytes: Uint8Array): [string, engine.Tensor][] => {
  const headerLength = Number(new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength).getBigUint64(0, true))
  const header = JSON.parse(new TextDecoder().decode(bytes.subarray(8, 8 + headerLength))) as Record<string, { dtype: string, shape: number[], data_offsets: [number, number] }>
  const body = bytes.slice(8 + headerLength).buffer
  return Object.entries(header).filter(([key]) => key !== "__metadata__").map(([key, t]) => {
    const dtype = SAFETENSOR_DTYPES[t.dtype]
    if (!dtype) { throw new Error(`safetensors: unsupported dtype ${t.dtype} for '${key}'`) }
    const [View, decode] = dtype, at = t.data_offsets[0], n = t.shape.reduce((a, b) => a * b, 1)
    const raw = new View(body.slice(at, at + n * View.BYTES_PER_ELEMENT)) as ArrayLike<number>
    return [key, engine.tensor(Float32Array.from(raw, decode), { shape: t.shape })]
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

// Text(s): markdown; a live s shows its text as it changes, the widget stays
const Text = (value: Value) =>
  typeof value === "string" || value instanceof String || value instanceof Signal ? widget("Text", {}, { text: value }) : new Error(`Text: expected string, got ${typeof value}`)

const Button = (label?: Value, onClick?: Value) =>
  widget("Button", {}, { label: label ?? "Button", ...(typeof onClick === "function" && { onClick: new Callback(onClick) as unknown as Value }) })

// TextEditor(text) – editable always; TextEditor(text, enabled) – read-only while `enabled` is falsy
const TextEditor = (target: Value, enabled?: Value) => widget("TextEditor", {}, { target, ...(enabled !== undefined && { enabled }) })
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
    return Array.isArray(data) ? data as number[] : new Error(`\`PointPlot\`: ${what} must be a tensor – write a list as [ … ]`)
  }
  const ys = axis(y ?? x, y === undefined ? "the argument" : "y")
  if (ys instanceof Error) { return ys }
  const xs = y === undefined ? ys.map((_, i) => i) : axis(x, "x")
  return xs instanceof Error ? xs : widget("PointPlot", { xs, ys })
}

const Code = (source: Value) => widget("Code", {}, { source })
// CodeEditor(code) fills its container; CodeEditor(code, "auto") sizes itself to its
// content and grows as you type – for editors in auto-sized rows
const CodeEditor = (source: Value, sizing?: Value) => widget("CodeEditor", { auto: String(sizing ?? "") === "auto" }, { source })

const Print = (value: Value) => widget("Print", {}, {}, [value])
const PrettyPrint = (value: Value) => widget("PrettyPrint", {}, {}, [value])
const CodePrint = (node: Value) => new CodeTree(node as unknown as SyntaxTreeNode) as unknown as Value

const MousePosition = () => pageSource("pointer", [2], engine.tensor([0.5, 0.5]), samples)
const Camera = (width?: Value, height?: Value, fps?: Value) => {
  const w = numberOr(width, 640), h = numberOr(height, 480)
  // black until the camera starts, so the program's math has a frame to work on
  return pageSource("camera", [w, h], engine.zeros([h, w, 3]), (f) => (f.bitmap ? fromPixels(f.bitmap) : null), numberOr(fps, 30))
}
const Microphone = (bufferSize?: Value) => {
  const size = numberOr(bufferSize, 2048)
  return pageSource("microphone", [size], engine.zeros([size]), samples)
}
// the spectrum's magnitudes, normalized to 0–1
const MicrophoneSpectrum = (bufferSize?: Value) => {
  const size = numberOr(bufferSize, 1024)
  return pageSource("spectrum", [size], engine.zeros([size]), samples)
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

const Fetch = (url: Value) => arriving("Error fetching data", null, () => fetched(String(url)).then((r) => r.text()))
// the loaders resolve later – lifting would re-invoke them (and re-fetch) on every change
const LoadTensorFromImageUrl = (url: Value) => arriving("LoadTensorFromImageUrl", null, () =>
  withProxy(String(url), (u) => fetched(u).then((r) => r.blob()).then((blob) => createImageBitmap(blob))).then(fromPixels))
// an empty list until it loads – readers can gate on `#(data()) > 0` – and an Error if it can't
const LoadSafeTensorFromURL = (url?: Value) => url == null
  ? new Error("loadSafeTensorFromURL: url is required")
  : arriving("LoadSafeTensorFromURL", [], () => withProxy(String(url), cachedFetch).then(parseSafetensors))

for (const f of [Text, Button, TextEditor, Slider, Checkbox, Scrubber, ImageUpload, Center, Layers, Point2D, Trail, Code, CodeEditor, Print, LoadSafeTensorFromURL, LoadTensorFromImageUrl]) {
  setMeta(f, { noAutoLift: true })
}

// MARK: Running the program

let source: string | null = null
// the last run's top-level scope: an edit keeps its cells by name
let lastScope: CurrentScope | null = null
let runScheduled = false

// a burst of edits runs only the latest
const scheduleRun = () => {
  if (runScheduled) { return }
  runScheduled = true
  setTimeout(run, 0)
}

// The program's value is live 0, which the page holds from the start: each run sends it
const program = signal<Value>(null)
const root: Live = { id: 0, target: program, refs: 0, at: -1, quiet: 0, reading: 0, ack: 0 }
lives.set(0, root)

// A program's statements one after another, as the language runs them: an Error that a
// statement before the last gives is shown nowhere, so it is marked where it is
const evaluateStatements = (tree: SyntaxTreeNode, scope: CurrentScope): Value => {
  const statements = tree.type === "Program" ? tree.content : [tree]
  let value: Value = null
  statements.forEach((statement, i) => {
    value = evaluateSyntaxTreeNode(statement, scope)
    if (value instanceof Error && i < statements.length - 1) { warn(value, statement.origin) }
  })
  return value ?? null
}

// The program runs once, at the instant its text arrives: what it reads at its top level
// is read then, and what it shows live follows. Each run's value replaces the last one's;
// the page lets go of what it showed of the last one as it shows the new one.
const run = () => {
  runScheduled = false
  if (source === null) { return }
  documentIds.clear()
  sampleRateRead = false
  warnings.clear()
  const code = source, scope = createScope(), from = lastScope
  let value: Value
  try {
    // the cells the last run trained are this run's, where the program still makes them
    value = evaluateGeneration(() => keepingFrom(from, scope, () => evaluateStatements(CodeParse(code), scope)))
  } catch (e) {
    value = asError(e)
  }
  lastScope = scope
  // the value goes at once if the page may take it, before any loop's first frame
  program.value = value
  dirty.add(root)
  flush()
  sendWarnings()
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
    case "parse": {
      const d = documentHash(m.source)
      const errors = getParseErrors(m.source).map(({ message, start, end }) => ({ message, o: { s: [start.line, start.column], e: [end.line, end.column], d } as Where }))
      post({ t: "parsed", d, errors })
      return
    }
    case "set": {
      const live = lives.get(m.id)
      if (!live) {
        if (typeof m.value === "object" && !Array.isArray(m.value)) { m.value.close() }
        return
      }
      // answered whether or not the write can be made: the page stops showing its own
      // value once it hears back
      live.ack = m.seq
      markDirty(live)
      const value = fromPage(m.value)
      let written: Value = null
      try {
        written = SignalUpdate(live.target, value)
      } catch (e) {
        written = asError(e)
      }
      // a computed value can't be set: the editor says so where it is computed
      if (written instanceof Error) { warn(written, getOrigin(live.target)); sendWarnings() }
      return
    }
    case "call": {
      // a press is an occurrence
      const fn = callbacks.get(m.id)?.fn
      if (fn) { occurrence(() => fn()) }
      return
    }
    case "drop": {
      // what the page shows no more stops, and is let go of
      const live = lives.get(m.id), callback = callbacks.get(m.id)
      if (live && (live.refs -= m.count) <= 0) { lives.delete(m.id); dirty.delete(live) }
      if (callback && (callback.refs -= m.count) <= 0) { callbacks.delete(m.id) }
      pictures.delete(m.id)
      return
    }
    case "frame": {
      receiveFrame(m.id, { bitmap: m.bitmap, samples: m.samples })
      post({ t: "took", id: m.id })
      return
    }
    case "canvas": { painter.attach(m.slot, m.canvas); return }
    case "canvasGone": { painter.detach(m.slot); return }
    case "paint": {
      // the canvas's box, from units to the display's pixels
      const picture = pictures.get(m.id), D = densityNow()
      if (!picture) { return }
      painter.paint(m.slot, picture, [m.size[0] * D, m.size[1] * D]).then(
        () => { if (dirty.size) { flush() } },
        (e) => { post({ t: "tensor", id: m.id, error: asError(e).message }) },
      )
      return
    }
    case "drawn": {
      // the page answers once a frame, so the next batch can go now
      pageDrawing = false
      flush()
      return
    }
    case "density": {
      // a picture drawn in units follows it, and the page paints its canvases again
      if (m.value !== densityNow()) { density.value = TensorScalarLive(m.value) }
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

export const startHost = (postMessage: Post): ((message: ToWorker) => void) => {
  post = postMessage
  const gpu = engine.engineGpu()
  painter = gpu ? new GpuPainter(gpu.device) : new CpuPainter()
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
  // builds one: it is named with its card instead.
  Object.defineProperty(DefaultEnvironment, "Tour", {
    get: () => CodeEvaluate.call(createScope(), TOUR_SOURCE),
    enumerable: false,
    configurable: true,
  })
  // every name in the environment, with its doc card – for hover and completion on the page
  const scope = createScope(), names: [string, string, Doc | null][] = [["Tour", "Tour", { doc: "The Fluent tour: from 1 + 2 to a value that learns." }]]
  seeing = scope as unknown as typeof seeing
  // the color table, decoded once
  engine.materialize([scope["DrawViridisTable"] as engine.Tensor])
  for (const key in scope) {
    const value = scope[key], meta = typeof value === "function" ? getMeta(value) : {}
    names.push([key, meta.signature ?? (typeof value === "function" ? "function" : String(value).slice(0, 50)), docOf(value) ?? null])
  }
  post({ t: "ready", names })
  return receive
}
