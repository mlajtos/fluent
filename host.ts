// MARK: Evaluation host
// The worker side of the IDE. It evaluates the program, binds the widgets and the live
// sources into the environment, and describes values to the page (protocol.ts). Values
// stay here – tensors, signals, variables; the page draws their descriptions and writes
// back through ids.

import { Signal, computed, effect, untracked } from "@preact/signals-core"
import {
  CodeParse, getParseErrors, evaluateSyntaxTreeNode, CodeEvaluate, createScope, DefaultEnvironment,
  extendEnvironment, setCodeNodePrinter, evaluateGeneration, registerDisposable,
  getAsSyncList, getMeta, setMeta, getOrigin, SignalCreate, SignalUpdate, StringLiteral,
  engine, FluentVariable, isTensor, TensorScalarLive,
  type Value, type SyntaxTreeNode, type Origin,
} from "./language"
import {
  tensorView, documentHash, FEW,
  type Desc, type Doc, type SourceKind, type TensorView, type ToPage, type ToWorker, type Tree, type Where,
} from "./protocol"
import { Documentation } from "./documentation"
import { TOUR_SOURCE } from "./tour"
import { CpuPainter, GpuPainter, type Picture } from "./painter"

type Post = (message: ToPage, transfer?: Transferable[]) => void
let post: Post = () => { }

const nextFrame = (cb: () => void) => { typeof requestAnimationFrame === "function" ? requestAnimationFrame(cb) : setTimeout(cb, 16) }
const asError = (e: unknown): Error => e instanceof Error ? e : new Error(String(e))

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

// A live value the page shows: a signal or a variable. Its effect marks it dirty when it
// changes, and the next frame sends its new description – one per frame however often it
// changes, none while the tensors of the last one are still being read, and none while
// the page is still drawing the last batch. The pictures it showed last are let go when
// it shows new ones.
type Live = {
  id: number,
  target: Signal<Value> | FluentVariable,
  refs: number,
  stop: () => void,
  reading: number,
  ack: number,
  pictures: number[],
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
// id, until the page asks for it
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

const valueOf = (target: Signal<Value> | FluentVariable): Value => {
  try {
    return target instanceof FluentVariable ? target.current : target.value
  } catch (e) {
    return asError(e)
  }
}

let flushScheduled = false
let pageDrawing = false
const markDirty = (live: Live) => {
  dirty.add(live)
  if (flushScheduled || pageDrawing) { return }
  flushScheduled = true
  nextFrame(flush)
}
const flush = () => {
  flushScheduled = false
  // the frame before the last is still on the GPU: the next waits for it
  if (pageDrawing || painter.pending > 1) { return }
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

const describeNow = (live: Live): Desc => {
  for (const id of live.pictures) { pictures.delete(id) }
  live.pictures = []
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
    const fresh: Live = live = { id, target, refs: 0, stop: () => { }, reading: 0, ack: 0, pictures: [] }
    let first = true
    fresh.stop = effect(() => {
      if (target instanceof FluentVariable) { target.version.value } else { valueOf(target) }
      if (first) { first = false; return }
      markDirty(fresh)
    })
    lives.set(id, fresh)
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
let seeing: Record<"heat" | "DrawBarsOf" | "DrawExtent", (...args: Value[]) => Value>
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

// A tensor's picture, and the numbers that cross with it for the page's labels: its values
// when they are few, otherwise the finite range they span; an image has none. A heatmap
// and bars are made when they are painted – bars for the canvas's size – and live only
// until they are on the screen.
const prepare = (view: TensorView, t: engine.Tensor): { picture: Picture, numbers?: engine.Tensor } => {
  if (view === "image") {
    engine.materialize([t])
    return { picture: t }
  }
  if (view === "heat") {
    const range = tensorOf(seeing.DrawExtent(t))
    engine.materialize([t, range])
    return { picture: () => tensorOf(seeing.heat(t, range)), numbers: t.size < FEW ? t : range }
  }
  if (!t.size) { return { picture: () => null, numbers: t } }
  const v = windowOf(t), n = engine.scalar(t.size), range = tensorOf(seeing.DrawExtent(v))
  engine.materialize([t, v, range])
  return {
    picture: ([w, h]) => (w >= 1 && h >= 1 ? tensorOf(seeing.DrawBarsOf(v, n, range, ...[w, h, roomy(w), roomy(h)].map(engine.scalar))) : null),
    numbers: t.size < FEW ? t : range,
  }
}
// Bars are drawn in a picture a little larger than their canvas – each side rounded up to
// 128 pixels – so a canvas resized within that keeps its plan
const roomy = (s: number) => Math.ceil(s / 128) * 128

const describeTensor = (t: engine.Tensor): Desc => {
  const id = ++nextId, shape = [...t.shape], view = tensorView(shape)
  try {
    if (view === "shape") { return { k: "tensor", id, shape } }
    if (view === "number") { return { k: "tensor", id, shape, ...numbersOf(id, t) } }
    const { picture, numbers } = prepare(view, t)
    pictures.set(id, picture)
    owner?.pictures.push(id)
    return { k: "tensor", id, shape, paint: true, ...(numbers && numbersOf(id, numbers)) }
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
  if (value instanceof Signal || value instanceof FluentVariable) { return describeLive(value as Signal<Value> | FluentVariable) }
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

const Text = (value: Value) =>
  typeof value === "string" || value instanceof String ? widget("Text", { text: String(value) }) : new Error(`Text: expected string, got ${typeof value}`)

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

for (const f of [Button, TextEditor, Slider, Checkbox, Scrubber, ImageUpload, Center, Layers, Point2D, Trail, Code, CodeEditor, Print, LoadSafeTensorFromURL, LoadTensorFromImageUrl]) {
  setMeta(f, { noAutoLift: true })
}

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

// a live nothing shows any more stops, and lets go of the pictures it showed
const forget = (live: Live) => {
  live.stop()
  lives.delete(live.id)
  dirty.delete(live)
  for (const id of live.pictures) { pictures.delete(id) }
}

// The program runs inside an effect: a signal it reads at its top level – not inside a
// $ or a lambda – runs it again when it changes. once() reads without that. Every value
// of the last program is let go: its lives stop sending, and the page drops what it
// shows of them when the new result arrives.
const run = () => {
  runScheduled = false
  if (source === null) { return }
  stopProgram?.()
  pageDrawing = false
  lives.forEach(forget)
  callbacks.clear()
  documentIds.clear()
  pictures.clear()
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
      value = asError(e)
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
      const value = fromPage(m.value)
      if (!(live.target instanceof FluentVariable)) { SignalUpdate(live.target, value) } else if (isTensor(value)) { live.target.assign(value) }
      // answered even when the write changed nothing (a read-only signal): the page
      // stops showing its own value once it hears back
      live.ack = m.seq
      markDirty(live)
      return
    }
    case "call": {
      try { callbacks.get(m.id)?.fn() } catch (e) { console.error(e) }
      return
    }
    case "drop": {
      const live = lives.get(m.id), callback = callbacks.get(m.id)
      if (live && (live.refs -= m.count) <= 0) { forget(live) }
      if (callback && (callback.refs -= m.count) <= 0) { callbacks.delete(m.id) }
      return
    }
    case "frame": { receiveFrame(m.id, { bitmap: m.bitmap, samples: m.samples }); return }
    case "canvas": { painter.attach(m.slot, m.canvas); return }
    case "canvasGone": { painter.detach(m.slot); return }
    case "paint": {
      const picture = pictures.get(m.id)
      if (!picture) { return }
      painter.paint(m.slot, picture, m.size).then(
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
  // builds one.
  Object.defineProperty(DefaultEnvironment, "Tour", {
    get: () => CodeEvaluate.call(createScope(), TOUR_SOURCE),
    enumerable: false,
    configurable: true,
  })
  // every name in the environment, with its doc card – for hover and completion on the page
  const scope = createScope(), names: [string, string, Doc | null][] = []
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
