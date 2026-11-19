// MARK: Setup

/*

1. clone the repo: `git clone https://github.com/mlajtos/fluent.git`
2. install dependencies: `cd fluent && bun install`
3. start the development server: `bun dev`
4. open `http://localhost:3000/?code=RG9jdW1lbnRhdGlvbg` in your browser
5. edit `language.ts` and `prelude.fl` to modify the language; the IDE is
   `host.ts` (the worker: evaluation, widgets, sources) and `client.tsx` (this
   file: the page, drawing what the worker describes)
6. share your changes with others

*/

/*

# TODO

## Features

- `Canvas` UI component for rapid tensor creation
  - draw pixels to create image tensors
- `Curve` UI component for interactive function definition
  - same UI as for easing curves in animation tools
- `ImageUpload` should survive code changes (image is lost on re-evaluation)
- syntax tree literals:
  - syntax for symbol literal – `symbol`
  - string interpolation
    - "Hello `user`!"
  - use syntax tree literals for macros / metaprogramming
    - https://rosettacode.org/wiki/Metaprogramming
- more economical syntax for tensors:
  - `[1 2 3]` instead of `[1, 2, 3]`
  - `[[1 2] [3 4]]` instead of `[[1, 2], [3, 4]]`
  - `[1 2, 3 4]` for 2D matrices
  - `[1 (1 + 1) 3]` for expressions
    - brackets `()` would expect a single expression
    - `[1 fn() 3]` – not allowed
    - `[1 (fn()) 3]` – allowed

## Other

- https://ceur-ws.org/Vol-1785/M2.pdf
  - may be relevant
- https://medium.com/data-science/why-not-mse-as-a-loss-function-for-logistic-regression-589816b5e03c
  - might be good demo example
- https://einx.readthedocs.io/en/stable/faq/universal.html
  - think about benefits of alternative universal notation
- https://arxiv.org/pdf/2303.15784
  - ideograph – inspiration for visualization of syntax tree
- https://github.com/Rogue-Frontier/Oblivia
  - APL+JS hybrid language
- https://maggieappleton.com/learnable-programming/
- effective fluent?
  - https://github.com/vahidk/EffectivePyTorch
- JAX autodiff resources
  - https://docs.jax.dev/en/latest/notebooks/autodiff_cookbook.html
  - https://docs.jax.dev/en/latest/autodidax.html
- https://docs.google.com/presentation/d/1m16fO-oKQM_J6G4LZNNJ64kXTurS_T6eU5dcz_3n8XM/edit?slide=id.g361713f9018_1_65#slide=id.g361713f9018_1_65
- Can Tensor Programming Be Liberated from the Fortran Data Paradigm? -- Conal Elliott
  - https://www.youtube.com/watch?v=oaIMMclGuog
- fancy signals
  - https://www.npmjs.com/package/@preact-signals/utils
  - https://rodydavis.com/posts/async-preact-signal
- https://github.com/naver/lispe/wiki/5.3-A-la-APL
  - lispE with APL-like operators
- https://tinyapl.rubenverg.com/docs/info/combinators
  - combinators with diagrams
- https://github.com/aburjg/k.py
  - K golfed in Python

*/

// MARK: Imports

import { signal, computed, effect } from "@preact/signals-core"
import {
  tensorView, labelled, heatScale, numberText, documentHash, VERSION, identifierRegexp, numberRegexp, numberRules, stringRegexp, operatorRegexp, delimiterRegexp,
  connect, mirrors, payloads, root, update, write, call, told,
  type Desc, type Doc, type SourceKind, type ToPage, type ToWorker, type Tree as SyntaxTree, type Where,
} from "./protocol"
import { EXAMPLES } from "./examples"
import { useSignals } from "@preact/signals-react/runtime"
import { ErrorBoundary } from "react-error-boundary"
import { useEffect, type CSSProperties, type JSX, useMemo, useRef, useState } from "react"
import { createRoot } from "react-dom/client"
import { type OnMount, Editor, loader } from "@monaco-editor/react"
import * as monaco from "monaco-editor"
import { type editor } from "monaco-editor"
// @ts-ignore
import { renderMarkdown } from "monaco-editor/esm/vs/base/browser/markdownRenderer.js"
// @ts-ignore
import { IQuickInputService } from "monaco-editor/esm/vs/platform/quickinput/common/quickInput"
import dedent from "ts-dedent"
import { Base64 } from "js-base64"
import "./index.css"
import { parsed as parsedOnPage } from "./ink"
import { Canvas } from "./canvas"
import { Copybook } from "./copybook"
import { Hand } from "./hand"

// The workers' scripts: a build names them by their content (build.ts), the dev server by
// their purpose
declare const MONACO_WORKER: string, FLUENT_WORKER: string
const startWorker = (name: string) => new Worker(new URL(name, document.baseURI), { type: "module" })
;(globalThis as { MonacoEnvironment?: typeof MonacoEnvironment }).MonacoEnvironment = {
  getWorker: () => startWorker(typeof MONACO_WORKER === "string" ? MONACO_WORKER : "monaco.worker.js"),
}

// In Safari, Monaco's clipboard service hands ClipboardItem a promise on every click
// and keypress, then cancels it on the next one, with nothing handling the
// rejection. A ClipboardItem here handles the promises it is given; the clipboard
// still receives them unchanged.
if (typeof ClipboardItem === "function") {
  const Native = ClipboardItem
  ;(globalThis as { ClipboardItem: typeof ClipboardItem }).ClipboardItem = class extends Native {
    constructor(items: Record<string, string | Blob | PromiseLike<string | Blob>>, options?: ClipboardItemOptions) {
      for (const item of Object.values(items)) { if (item instanceof Promise) { item.catch(() => { }) } }
      super(items, options)
    }
  }
}

// @monaco-editor/react uses the bundled monaco-editor, not a CDN copy
loader.config({ monaco })

// MARK: Worker
// The program runs in a worker (worker.ts): the engine, the GPU and every value live
// there. The page draws descriptions of values (protocol.ts), writes back through ids,
// and streams the camera, the microphone and the pointer in.

const environmentNames = signal(new Map<string, { detail: string, doc: Doc | null }>())

// The display's pixels a CSS pixel: the worker draws pictures at it, and hears of each change
// – a move to another screen, a zoom
const density = signal(devicePixelRatio)
const watchDensity = () => matchMedia(`(resolution: ${devicePixelRatio}dppx)`).addEventListener("change", () => {
  density.value = devicePixelRatio
  toWorker({ t: "density", value: devicePixelRatio })
  watchDensity()
}, { once: true })
watchDensity()

// A hot update starts a fresh evaluator and stops the old one's sources; so does a lost GPU
const hot = globalThis as { __fluentWorker?: Worker, __fluentSources?: Map<number, () => void> }
const pageSources = hot.__fluentSources ??= new Map()
let worker: Worker
const spawn = () => {
  hot.__fluentWorker?.terminate()
  for (const stop of pageSources.values()) { stop() }
  pageSources.clear()
  worker = hot.__fluentWorker = startWorker(typeof FLUENT_WORKER === "string" ? FLUENT_WORKER : "fluent.worker.js")
  toWorker({ t: "density", value: density.peek() })
  worker.onmessage = receive
  worker.onerror = (e) => { showError(`The evaluator stopped: ${e.message || "it failed to start"}`) }
  // a batch is drawn by the next frame
  connect(toWorker, (answer) => requestAnimationFrame(answer))
}
const toWorker = (message: ToWorker, transfer: Transferable[] = []) => worker.postMessage(message, transfer)
const showError = (message: string) => { root.desc.value = { k: "error", chain: [{ message }] } }

// what a worker sent after it was replaced names nothing here
function receive({ data: m, target }: MessageEvent<ToPage>) {
  if (target !== worker) { return }
  switch (m.t) {
    case "ready": {
      environmentNames.value = new Map(m.names.map(([name, detail, doc]) => [name, { detail, doc }]))
      return
    }
    case "parsed": { showParseErrors(m); parsedOnPage(m); return }
    case "migrated": { program.value = m.source; return }
    case "warnings": { showWarnings(m); return }
    case "lives": case "tensor": { update(m); return }
    case "told": { told(m); return }
    case "source": { startSource(m.id, m.kind, m.size, m.fps); return }
    case "stop": { pageSources.get(m.id)?.(); pageSources.delete(m.id); return }
    case "took": { taking.delete(m.id); return }
    case "needSampleRate": { toWorker({ t: "sampleRate", value: sampleRate() }); return }
    case "lost": {
      // the GPU took the values with it: another worker runs the program again
      spawn()
      showError(`The GPU was lost: ${m.message}. Starting again…`)
      toWorker({ t: "code", source: program.peek() })
      return
    }
  }
}
spawn()

// What a description shows now – through live values to their current value. Reading it
// in a component subscribes the component.
const resolve = (d: Desc | undefined): Desc | undefined => {
  for (let i = 0; d?.k === "live" && i < 64; i++) { d = mirrors.get(d.id)?.desc.value }
  return d
}
const payloadOf = (d: Desc | undefined): Float32Array | undefined => {
  const p = d?.k === "tensor" ? payloads.get(d.id)?.value : undefined
  return typeof p === "string" ? undefined : p
}
const numberOf = (d: Desc | undefined): number | undefined => payloadOf(resolve(d))?.[0]
const textOf = (d: Desc | undefined): string | undefined => {
  const r = resolve(d)
  return r?.k === "string" ? r.v : undefined
}

// A control bound to a live value shows its own write until the worker has answered it,
// then the worker's value
const useBound = (target: Desc | undefined) => {
  const id = target?.k === "live" ? target.id : undefined
  const mirror = id === undefined ? undefined : mirrors.get(id)
  const local = mirror?.local.value
  return {
    remote: resolve(target),
    local: mirror && mirror.ack < mirror.sent ? local : undefined,
    write: (value: number | number[] | string | ImageBitmap) => { if (id !== undefined) { write(id, value) } },
  }
}
const useText = (target: Desc | undefined) => {
  const { remote, local, write } = useBound(target)
  return { text: (local as string | undefined) ?? (remote?.k === "string" ? remote.v : ""), write }
}

// The audio hardware's sample rate, read once
let knownSampleRate: number | undefined
const sampleRate = (): number => knownSampleRate ??= (() => {
  const context = new AudioContext(), rate = context.sampleRate
  context.close()
  return rate
})()

// The camera, the microphone, the pointer: the worker asks for a source, and the page
// streams its frames there – one per display frame at most, and none while the worker
// has yet to take the last
const taking = new Set<number>()
const startSource = (id: number, kind: SourceKind, size: number[], fps = 30) => {
  let active = true
  const cleanups: (() => void)[] = []
  pageSources.set(id, () => { active = false; taking.delete(id); cleanups.forEach((f) => f()) })
  const everyFrame = (f: (now: number) => void) => {
    const tick = (now: number) => {
      if (!active) { return }
      if (!taking.has(id)) { f(now) }
      requestAnimationFrame(tick)
    }
    requestAnimationFrame(tick)
  }
  const send = (frame: { bitmap?: ImageBitmap, samples?: Float32Array }) => {
    taking.add(id)
    toWorker({ t: "frame", id, ...frame }, frame.bitmap ? [frame.bitmap] : [frame.samples!.buffer as ArrayBuffer])
  }
  // a device granted after the source stopped is released at once
  const media = (constraints: MediaStreamConstraints, use: (stream: MediaStream) => void) =>
    navigator.mediaDevices.getUserMedia(constraints).then((stream) => {
      const stop = () => stream.getTracks().forEach((track) => track.stop())
      if (!active) { stop(); return }
      cleanups.push(stop)
      use(stream)
    }).catch((err) => console.error(`${kind} access denied:`, err))

  if (kind === "pointer") {
    // normalized to the viewport, 0–1
    let latest: Float32Array | null = null
    const onMove = (e: PointerEvent) => { latest = Float32Array.of(e.clientX / window.innerWidth, e.clientY / window.innerHeight) }
    window.addEventListener("pointermove", onMove)
    cleanups.push(() => window.removeEventListener("pointermove", onMove))
    everyFrame(() => {
      if (!latest) { return }
      send({ samples: latest })
      latest = null
    })
    return
  }

  if (kind === "camera") {
    const [width, height] = size
    const video = Object.assign(document.createElement("video"), { autoplay: true, playsInline: true })
    media({ video: { width, height } }, (stream) => {
      video.srcObject = stream
      video.onloadedmetadata = () => video.play()
    })
    cleanups.push(() => { video.srcObject = null })
    let last = 0, grabbing = false
    everyFrame((now) => {
      if (grabbing || now - last < 1000 / fps || video.readyState !== video.HAVE_ENOUGH_DATA) { return }
      last = now
      grabbing = true
      createImageBitmap(video).then((bitmap) => {
        grabbing = false
        if (active) { send({ bitmap }) } else { bitmap.close() }
      }, () => { grabbing = false })
    })
    return
  }

  // the microphone: its waveform, or its spectrum normalized to 0–1
  const [n = 1024] = size
  let analyser: AnalyserNode | null = null
  media({ audio: true }, (stream) => {
    const context = new AudioContext()
    cleanups.push(() => context.close())
    analyser = context.createAnalyser()
    analyser.fftSize = n * 2
    if (kind === "spectrum") { analyser.smoothingTimeConstant = 0.8 }
    context.createMediaStreamSource(stream).connect(analyser)
  })
  const bytes = new Uint8Array(n)
  everyFrame(() => {
    if (!analyser) { return }
    const samples = new Float32Array(n)
    if (kind === "microphone") {
      analyser.getFloatTimeDomainData(samples)
    } else {
      analyser.getByteFrequencyData(bytes)
      for (let i = 0; i < n; i++) { samples[i] = bytes[i]! / 255 }
    }
    send({ samples })
  })
}

// MARK: Colors and prose

const tokenColors = {
  number: "0A81EF",
  identifier: "30CD50",
  operator: "F238E3",
  string: "D8D8D8",
  delimiter: "F94C4A",
  comment: "444444",
}
const COLORS: editor.ITokenThemeRule[] = [
  ...Object.entries(tokenColors).map(([token, foreground]) => ({ token, foreground })),
  { token: "comment.md", foreground: "7F7F7F" },
  { token: "keyword.md", foreground: "7F7F7F" },
]

// a syntax tree node's label, colored as the editor colors the token it starts with
const TOKEN_KINDS = ([[numberRegexp, "number"], [stringRegexp, "string"], [identifierRegexp, "identifier"], [operatorRegexp, "operator"]] as const)
  .map(([pattern, kind]) => [new RegExp(`^(?:${pattern.source})`, pattern.flags), kind] as const)
const colorOf = (label: string): string => {
  const kind = TOKEN_KINDS.find(([pattern]) => pattern.test(label))?.[1]
  return kind ? `#${tokenColors[kind]}` : "#D4D4D4"
}

// each character of a line of Fluent in the color the editor gives it
const colorsOf = (text: string): string[] => {
  const tokens = monaco.editor.tokenize(text, "fluent")[0] ?? []
  return Array.from({ length: text.length }, (_, at) => {
    const kind = tokens.findLast(({ offset }) => offset <= at)?.type.split(".")[0]
    return `#${tokenColors[kind as keyof typeof tokenColors] ?? tokenColors.string}`
  })
}

const StringSerialize = (s: string) => Base64.encodeURI(s)
const StringDeserialize = (s: string) => Base64.decode(s)

// Fluent in prose, colored as the editor colors it: a block in its own div, an inline
// `code` on its line
const colorize = (text: string) => monaco.editor.colorize(text, "fluent", {})
const colorizeBlock = async (language: string, value: string) => {
  const e = document.createElement("div")
  e.className = "overflow-hidden"
  e.style.fontFamily = "monospace"
  try { e.innerHTML = await monaco.editor.colorize(value, language, {}) } catch { e.textContent = value }
  return e
}

// Monaco's markdown renderer refuses relative link targets ("?example=lenia")
// and renders them as plain text. Smuggle them through as an absolute URL on
// a sentinel origin, then rewrite the rendered hrefs back to relative – so
// prose can link into the playground ([gallery](?example=lenia)).
const RELATIVE_LINK_SENTINEL = "https://fluent-relative.internal/"

const convertTextToHTML = async (value: string) => {
  const renderable = value.replace(/\]\((\?[^)\s]+)\)/g, (_, query) => `](${RELATIVE_LINK_SENTINEL}${query})`)
  const element = (await renderMarkdown({ value: renderable }, { codeBlockRenderer: colorizeBlock })).element as HTMLElement
  for (const a of element.querySelectorAll("a")) {
    // the renderer leaves href empty and expects an action handler to run
    // data-href – prose wants real navigation
    const href = a.getAttribute("href") || a.getAttribute("data-href")
    if (!href) { continue }
    const relative = href.startsWith(RELATIVE_LINK_SENTINEL) ? new URL(href).search : null
    a.setAttribute("href", relative ?? href)
    if (relative) { a.setAttribute("title", relative) }
  }
  // a single paragraph is unwrapped, so one line of text sits inline
  const only = element.children[0]
  if (element.children.length === 1 && only?.tagName === "P") { only.replaceWith(...only.childNodes) }
  await Promise.all([...element.querySelectorAll("code:not(.code code)")].map(async (code) => {
    if (!code.textContent) { return }
    try {
      // colorize wraps its lines in divs: take the first one's content, without breaks
      const temp = document.createElement("div")
      temp.innerHTML = await colorize(code.textContent)
      code.innerHTML = (temp.querySelector("div")?.innerHTML ?? temp.innerHTML).replace(/<br\/?>/g, "")
    } catch { /* the plain text stays */ }
  }))
  return element
}

// MARK: Widgets
// What the program's widget values draw as. A widget binds to live values by id: it
// shows them, and its edits go back to the worker.

type UIDesc = Extract<Desc, { k: "ui" }>
type TensorDesc = Extract<Desc, { k: "tensor" }>

// Markdown, rendered off the page's critical path: the previous rendering stays until
// the new one is ready
const Markdown = ({ text }: { text: string }) => {
  const ref = useRef<HTMLDivElement>(null)
  useEffect(() => {
    let current = true
    convertTextToHTML(text).then((element) => { if (current) { ref.current?.replaceChildren(element) } })
    return () => { current = false }
  }, [text])
  return <div className="prose prose-neutral prose-invert font-sans"><div ref={ref}></div></div>
}

const TextView = ({ d }: { d: UIDesc }) => {
  useSignals()
  return <Markdown text={textOf(d.slots.text) ?? ""} />
}

const ButtonView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const action = d.slots.onClick?.k === "callback" ? d.slots.onClick.id : undefined
  return (
    <button
      className="bg-neutral-900 hover:bg-neutral-800 active:bg-neutral-700 rounded-xl border border-neutral-400 hover:border-neutral-300 active:border-neutral-200 disabled:border-neutral-800 disabled:cursor-not-allowed p-2 overflow-hidden"
      onClick={action === undefined ? undefined : () => call(action)} disabled={action === undefined}
    >
      {textOf(d.slots.label) ?? "Button"}
    </button>
  )
}

// read-only while `enabled` is falsy – readOnly, not disabled: the text stays selectable
// and the same <textarea> reconciles in place, so cursor and scroll survive
const TextEditorView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { text, write } = useText(d.slots.target)
  const on = d.slots.enabled === undefined || (numberOf(d.slots.enabled) ?? 1) >= 0.5
  return (
    <div className="grid">
      <textarea
        value={text}
        readOnly={!on}
        title={on ? undefined : "read-only while training"}
        onChange={(e) => { if (on) { write(e.target.value) } }}
        className={`block bg-neutral-800 focus:bg-neutral-900 rounded-xl border border-neutral-600 hover:border-neutral-500 focus:border-neutral-300 outline-none p-2 field-sizing-content ${on ? "" : "opacity-50 cursor-not-allowed"}`}
        // @ts-ignore
        style={{ fieldSizing: "content" }}
        rows={1}
      />
    </div>
  )
}

// the last value seen, while a newer one is read off the GPU
const useNumber = (target: Desc | undefined) => {
  const { remote, local, write } = useBound(target)
  const last = useRef(0)
  const value = (local as number | undefined) ?? numberOf(remote)
  if (value !== undefined) { last.current = value }
  return { value: last.current, write }
}

const SliderView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { value, write } = useNumber(d.slots.target)
  return (
    <div className="grid">
      <input
        type="range" aria-label="Slider" min={0} max={1} step={0.01} value={value}
        onChange={(e) => write(parseFloat(e.target.value))}
        className="bg-neutral-900 focus:bg-neutral-800 rounded-xl border border-neutral-800 hover:border-neutral-700 focus:border-neutral-600 outline-none p-2 w-full h-2 cursor-pointer dark:bg-gray-700 place-self-center"
      />
    </div>
  )
}

const CheckboxView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { value, write } = useNumber(d.slots.target)
  return (
    <input
      type="checkbox" aria-label="Checkbox" checked={value >= 0.5}
      onChange={(e) => write(e.target.checked ? 1 : 0)}
      className="w-5 h-5 accent-white cursor-pointer place-self-start"
    />
  )
}

// a number you drag sideways, `step` per 10 pixels, or step with the arrow keys
const ScrubberView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { step, decimals } = d.props as { step: number, decimals: number }
  const { value, write } = useNumber(d.slots.target)
  const set = (v: number) => write(Math.round(v * 10 ** decimals) / 10 ** decimals)
  const onPointerDown = (e: React.PointerEvent<HTMLSpanElement>) => {
    e.preventDefault()
    const startX = e.clientX, startValue = value
    const onMove = (me: PointerEvent) => set(startValue + (me.clientX - startX) * step * 0.1)
    const onUp = () => {
      window.removeEventListener("pointermove", onMove)
      window.removeEventListener("pointerup", onUp)
    }
    window.addEventListener("pointermove", onMove)
    window.addEventListener("pointerup", onUp)
  }
  const onKeyDown = (e: React.KeyboardEvent) => {
    const by = ({ ArrowRight: 1, ArrowUp: 1, ArrowLeft: -1, ArrowDown: -1 } as Record<string, number>)[e.key]
    if (by) { e.preventDefault(); set(value + by * step) }
  }
  return (
    <span
      role="slider" tabIndex={0} aria-label="Scrubber" aria-valuenow={value}
      style={{ color: `#${tokenColors.number}`, cursor: "ew-resize", userSelect: "none" }}
      onPointerDown={onPointerDown} onKeyDown={onKeyDown}
    >
      {numberText(value)}
    </span>
  )
}

// Drag & drop works even where native file dialogs are unavailable (embedded webviews)
const ImageUploadView = ({ d }: { d: UIDesc }) => {
  const { write } = useBound(d.slots.target)
  const input = useRef<HTMLInputElement>(null)
  const load = async (file: File | null | undefined) => {
    if (file?.type.startsWith("image")) { write(await createImageBitmap(file)) }
  }
  return (
    <>
      <button
        type="button"
        className="text-sm text-neutral-400 hover:text-neutral-200 border border-dashed border-neutral-600 hover:border-neutral-400 rounded-xl p-3 text-center cursor-pointer select-none"
        onClick={() => input.current?.click()}
        onDragOver={(e) => { e.preventDefault() }}
        onDrop={(e) => { e.preventDefault(); load(e.dataTransfer.files?.[0]) }}
      >
        drop an image here – or click to choose
      </button>
      <input ref={input} type="file" accept="image/*" className="hidden" onChange={(e) => load(e.target.files?.[0])} />
    </>
  )
}

// Lists flatten into cells, and a live list re-renders its cells as it changes
const flattenCells = (ds: Desc[]): Desc[] => ds.flatMap((d) => {
  if (d.k === "list") { return flattenCells(d.items) }
  const now = d.k === "live" ? resolve(d) : undefined
  return now?.k === "list" ? flattenCells(now.items) : [d]
})

// Without a row template, rows hug their content (content-start) instead of sharing the
// panel's spare height, and auto-rows-max keeps a tall page of cells from being crushed
// to slivers when it overflows – the grid scrolls instead.
const GridView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { columns, rows } = d.props as { columns: string, rows: string }
  return (
    <div className={`grid gap-2 overflow-scroll h-full ${rows ? "" : "content-start auto-rows-max"}`} style={{ gridTemplateColumns: columns, gridTemplateRows: rows }}>
      {flattenCells(d.children).map((cell, i) => <div key={i} className="contents"><Cell d={cell} /></div>)}
    </div>
  )
}

// Grid cells default to top-left, which reads wrong for headers and captions
const CenterView = ({ d }: { d: UIDesc }) => (
  <div className="grid place-items-center text-center h-full">
    {d.children[0] && <Cell d={d.children[0]} />}
  </div>
)

// Overlays must not eat pointer events, or the topmost layer swallows every click;
// interactive children (a Point2D's dot) re-enable them with pointer-events-auto
const LayersView = ({ d }: { d: UIDesc }) => (
  <div className="relative w-fit" data-layers="true">
    {d.children.map((child, i) => (
      <div key={i} className={i === 0 ? "" : "absolute inset-0 pointer-events-none"}><View d={child} /></div>
    ))}
  </div>
)

type PointProps = { x0: number, y0: number, spanX: number, spanY: number, color: string }

const usePoint = (target: Desc | undefined) => {
  const { remote, local, write } = useBound(target)
  const last = useRef<number[]>([0, 0])
  const known = (local as number[] | undefined) ?? (payloadOf(remote) && [...payloadOf(remote)!])
  if (known && known.length >= 2) { last.current = known }
  return { point: last.current as [number, number], write }
}

// A draggable dot; the layer is transparent to pointer events, so several share a stack
const Point2DView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { x0, y0, spanX, spanY, color } = d.props as PointProps
  const { point: [px, py], write } = usePoint(d.slots.target)
  const place = (event: React.PointerEvent<HTMLDivElement>) => {
    const rect = event.currentTarget.parentElement!.getBoundingClientRect()
    write([x0 + (event.clientX - rect.left) / rect.width * spanX, y0 + (event.clientY - rect.top) / rect.height * spanY])
  }
  // Grab arbitration: the dot whose center is nearest the pointer wins, not the topmost
  // layer (ties go to the earlier layer)
  const grab = (e: React.PointerEvent<HTMLDivElement>) => {
    const distanceTo = (el: Element) => {
      const r = el.getBoundingClientRect()
      return Math.hypot(e.clientX - (r.left + r.width / 2), e.clientY - (r.top + r.height / 2))
    }
    let nearest: Element = e.currentTarget, best = Infinity
    for (const el of e.currentTarget.closest("[data-layers]")?.querySelectorAll("[data-point2d]") ?? []) {
      const distance = distanceTo(el)
      if (distance < best) { best = distance; nearest = el }
    }
    if (nearest === e.currentTarget) { e.currentTarget.setPointerCapture(e.pointerId); return }
    e.stopPropagation()
    nearest.dispatchEvent(new PointerEvent("pointerdown", {
      bubbles: true, clientX: e.clientX, clientY: e.clientY, pointerId: e.pointerId, buttons: e.buttons, isPrimary: true,
    }))
  }
  return (
    <div className="absolute inset-0 pointer-events-none touch-none select-none">
      <div
        data-point2d="true"
        className="absolute pointer-events-auto cursor-grab active:cursor-grabbing"
        style={{
          left: `${(px - x0) / spanX * 100}%`, top: `${(py - y0) / spanY * 100}%`, transform: "translate(-50%, -50%)",
          width: 32, height: 32, display: "grid", placeItems: "center",
        }}
        onPointerDown={grab}
        onPointerMove={(e) => { if (e.buttons) { place(e) } }}
      >
        <div style={{ width: 12, height: 12, borderRadius: 999, background: color, border: "2px solid black", boxShadow: "0 0 6px rgba(0,0,0,0.8)" }} />
      </div>
    </div>
  )
}

// A surface over the layers below it: while a button is held it writes where the pointer is
// into its target, and runs press and release as the button goes down and up. It takes the
// press from the browser, so a drag never lifts the picture under it out as an image.
const PointerView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { x0, y0, spanX, spanY } = d.props as PointProps
  const { write } = useBound(d.slots.target)
  const callback = (slot?: Desc) => slot?.k === "callback" ? slot.id : undefined
  const press = callback(d.slots.press), release = callback(d.slots.release)
  // write the pointer's place, then run the callback
  const at = (e: React.PointerEvent<HTMLDivElement>, then?: number) => {
    const rect = e.currentTarget.getBoundingClientRect()
    write([x0 + (e.clientX - rect.left) / rect.width * spanX, y0 + (e.clientY - rect.top) / rect.height * spanY])
    if (then !== undefined) { call(then) }
  }
  return (
    <div
      className="absolute inset-0 pointer-events-auto touch-none select-none cursor-grab active:cursor-grabbing [-webkit-user-drag:none]"
      onPointerDown={(e) => {
        e.preventDefault()
        e.currentTarget.setPointerCapture(e.pointerId)
        at(e, press)
      }}
      onPointerMove={(e) => { if (e.buttons) { at(e) } }}
      onPointerUp={(e) => at(e, release)}
      onPointerCancel={(e) => at(e, release)}
      onDragStart={(e) => e.preventDefault()}
    />
  )
}

// The path a point has taken: the last 500 places it was shown at
const TrailView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { x0, y0, spanX, spanY, color } = d.props as PointProps
  const { point: [px, py] } = usePoint(d.slots.target)
  const points = useRef<string[]>([])
  const here = `${(px - x0) / spanX * 100},${(py - y0) / spanY * 100}`
  if (points.current.at(-1) !== here) {
    points.current.push(here)
    if (points.current.length > 500) { points.current.shift() }
  }
  return (
    <svg className="absolute inset-0 w-full h-full pointer-events-none" viewBox="0 0 100 100" preserveAspectRatio="none">
      <polyline points={points.current.join(" ")} fill="none" stroke={color} strokeOpacity={0.75} strokeWidth={1.5} vectorEffect="non-scaling-stroke" />
    </svg>
  )
}

// y against x, the points joined in order
const PointPlotView = ({ d }: { d: UIDesc }) => {
  const { xs, ys } = d.props as { xs: number[], ys: number[] }
  const [x0, x1] = extent(xs), [y0, y1] = extent(ys)
  const at = xs.map((x, i) => `${(x - x0) / (x1 - x0 || 1) * 100},${(y1 - ys[i]!) / (y1 - y0 || 1) * 100}`)
  return (
    <Axes y={[y0, y1]} x={[x0, x1]}>
      <svg className="absolute inset-0 w-full h-full overflow-visible" viewBox="0 0 100 100" preserveAspectRatio="none">
        <polyline points={at.join(" ")} fill="none" stroke="#666" strokeWidth={1} vectorEffect="non-scaling-stroke" />
        <path d={at.map((p) => `M${p}h0`).join("")} stroke={`#${tokenColors.number}`} strokeWidth={7} strokeLinecap="round" vectorEffect="non-scaling-stroke" />
      </svg>
    </Axes>
  )
}

const CodeWidgetView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { text, write } = useText(d.slots.source)
  return <CodeView value={text} onChange={write} />
}

const CodeEditorWidgetView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { text, write } = useText(d.slots.source)
  return <CodeEditorView value={text} onChange={write} auto={d.props.auto === true} />
}

const WIDGETS: Record<string, (p: { d: UIDesc }) => JSX.Element> = {
  Text: TextView, Button: ButtonView, TextEditor: TextEditorView, Slider: SliderView, Checkbox: CheckboxView,
  Scrubber: ScrubberView, ImageUpload: ImageUploadView, Grid: GridView, Center: CenterView, Layers: LayersView,
  Point2D: Point2DView, Pointer: PointerView, Trail: TrailView, PointPlot: PointPlotView, Code: CodeWidgetView, CodeEditor: CodeEditorWidgetView,
  Print: ({ d }) => <Framed d={d.children[0] ?? { k: "null" }} />,
  PrettyPrint: ({ d }) => <View d={d.children[0] ?? { k: "null" }} />,
}

// a widget is keyed by its identity: a different widget at the same place never inherits
// another's mounted state (an editor, a plot)
const WidgetView = ({ d }: { d: UIDesc }) => {
  const Widget = WIDGETS[d.type]
  return Widget ? <Widget key={d.uid} d={d} /> : <div className="text-violet-500">{d.type}</div>
}

// MARK: Syntax trees
// A quoted program draws as one tree per statement, read right to left: a node shares its
// row with its first child, and its trunk runs down the gap to the rest.

const TREE = { fontSize: 14, padX: 8, padY: 3, rowGap: 15, colGap: 24, radius: 5 }
const NODE_HEIGHT = TREE.fontSize * 1.2 + 2 * TREE.padY
const ROW_HEIGHT = NODE_HEIGHT + TREE.rowGap
const nodeWidth = (label: string) => label.length * TREE.fontSize * 0.6 + 2 * TREE.padX

type GridNode = { label: string, row: number, col: number, width: number, origin: Where, children: GridNode[], parent: GridNode | null }

// The keys of the subtree under the hovered node – its nodes get brighter frames and its
// edges brighter strokes. Null when nothing is hovered.
const hoverSubtree = signal<Set<string> | null>(null)

// the app's main editor: the global shortcuts' target, and the main program's document
let mainEditor: editor.IStandaloneCodeEditor | null = null

// Every mounted CodeEditor (the main panel AND embedded cells – tour rooms,
// REPL). An origin names the document it indexes into, so a hover routes
// to the editor whose text IS that document instead of always painting the
// main editor. Disposed editors are pruned lazily on lookup – Editor has no
// unmount callback to hook.
const editorRegistry = new Set<editor.IStandaloneCodeEditor>()
const hoverCollections = new WeakMap<editor.IStandaloneCodeEditor, editor.IEditorDecorationsCollection>()
let hoverTarget: editor.IStandaloneCodeEditor | null = null

// The editors whose text is a document. One the text has moved on from shows none of
// its ranges: better no highlight than the right range in the wrong text.
const editorsShowing = (document: number): editor.IStandaloneCodeEditor[] => [...editorRegistry].filter((ed) => {
  const model = ed.getModel()
  if (!model) { editorRegistry.delete(ed) }
  return model && documentHash(model.getValue()) === document
})
const rangeOf = (o: Where) => ({ startLineNumber: o.s[0], startColumn: o.s[1], endLineNumber: o.e[0], endColumn: o.e[1] })

const setHoverHighlight = (origin: Where | null) => {
  // an origin without a document is the main program's
  const target = !origin ? null : origin.d === 0 ? mainEditor : editorsShowing(origin.d)[0] ?? null
  if (hoverTarget && hoverTarget !== target) { hoverCollections.get(hoverTarget)?.set([]) }
  hoverTarget = target
  if (!target || !origin) { return }
  let collection = hoverCollections.get(target)
  if (!collection) { hoverCollections.set(target, collection = target.createDecorationsCollection()) }
  collection.set([{ range: rangeOf(origin), options: { className: "ast-hover-highlight" } }])
}

const markersOf = (list: { message: string, o: Where }[], severity: monaco.MarkerSeverity) =>
  list.map(({ message, o }) => ({ message, severity, ...rangeOf(o) }))
// The worker parses every edit: its errors mark each editor that shows the document
const showParseErrors = ({ d, errors }: Extract<ToPage, { t: "parsed" }>) => {
  for (const ed of editorsShowing(d)) { monaco.editor.setModelMarkers(ed.getModel()!, "fluent-syntax", markersOf(errors, monaco.MarkerSeverity.Error)) }
}
// The warnings since the program last ran mark each editor that shows their document
const showWarnings = ({ warnings }: Extract<ToPage, { t: "warnings" }>) => {
  for (const ed of editorRegistry) {
    const model = ed.getModel(), d = model && documentHash(model.getValue())
    if (model) { monaco.editor.setModelMarkers(model, "fluent-run", markersOf(warnings.filter(({ o }) => o.d === d), monaco.MarkerSeverity.Warning)) }
  }
}

// laid out once, re-tinted on hover
function TreeView({ roots }: { roots: SyntaxTree[] }): JSX.Element {
  return useMemo(() => drawTree(roots), [roots])
}

let treeInstance = 0
function drawTree(roots: SyntaxTree[]): JSX.Element {
  const nodes = layoutGrid(roots)
  if (nodes.length === 0) { return <svg /> }
  // columns run right to left from column 1, each as wide as its widest node
  const maxCol = Math.max(...nodes.map((n) => n.col)), maxRow = Math.max(...nodes.map((n) => n.row))
  const colWidth = Array<number>(maxCol + 1).fill(0), colX = Array<number>(maxCol + 1).fill(0), gapX = Array<number>(maxCol + 1).fill(0)
  for (const n of nodes) { colWidth[n.col] = Math.max(colWidth[n.col]!, n.width) }
  let width = 0
  for (let c = maxCol; c >= 1; c--) {
    colX[c] = width + colWidth[c]! / 2
    width += colWidth[c]!
    if (c > 1) {
      gapX[c - 1] = width + TREE.colGap / 2
      width += TREE.colGap
    }
  }
  const rowY = (r: number) => (r - 0.5) * ROW_HEIGHT, height = maxRow * ROW_HEIGHT
  // hoverSubtree is one global signal and grid coordinates repeat across
  // trees – prefix keys with this tree's identity, or hovering one graph
  // tints the same coordinates in every other graph on the page
  const treeId = ++treeInstance
  const keyOf = (n: GridNode) => `${treeId}:${n.row},${n.col}`
  const subtreeKeys = (n: GridNode, keys = new Set<string>()): Set<string> => {
    keys.add(keyOf(n))
    for (const c of n.children) { subtreeKeys(c, keys) }
    return keys
  }
  // Boundary nodes' rect strokes are CENTERED on the viewport edge (half the
  // stroke lives outside), and glyph labels can run a hair wider than the
  // 0.6em-per-char width estimate. A small bleed gutter plus visible
  // overflow lets the drawing breathe – the surrounding panel's padding
  // absorbs it, so edges stop getting chopped.
  const bleed = 2
  return (
    <svg className="ast-tree" style={{ width: width + 2 * bleed, height, overflow: "visible", shapeRendering: "optimizeSpeed" }} viewBox={`${-bleed} 0 ${width + 2 * bleed} ${height}`}>
      {/* one reactive region: it re-renders on hover to re-tint frames and edges, while the
          layout above is computed once per program */}
      {computed(() => {
        const active = hoverSubtree.value
        return (
          <>
            {nodes.flatMap((n) => n.children.map((c) => (
              <TreeEdge
                key={`${keyOf(n)}-${keyOf(c)}`}
                x1={colX[n.col]! - n.width / 2} y1={rowY(n.row)} x2={colX[c.col]! + c.width / 2} y2={rowY(c.row)}
                trunkX={gapX[n.col]!} active={active?.has(keyOf(n)) ?? false}
              />
            )))}
            {nodes.map((n) => (
              <TreeNode
                key={keyOf(n)} x={colX[n.col]!} y={rowY(n.row)} label={n.label} width={n.width} origin={n.origin}
                active={active?.has(keyOf(n)) ?? false} onEnter={() => { hoverSubtree.value = subtreeKeys(n) }}
              />
            ))}
          </>
        )
      }) as unknown as JSX.Element}
    </svg>
  )
}

// straight across, or down the trunk in the column gap with rounded corners
function TreeEdge({ x1, y1, x2, y2, trunkX, active }: { x1: number, y1: number, x2: number, y2: number, trunkX: number, active: boolean }): JSX.Element {
  const dy = y2 - y1, r = Math.min(TREE.radius, Math.abs(dy) / 2), s = Math.sign(dy) * r
  const d = Math.abs(dy) < 1 ? `M${x1} ${y1} L${x2} ${y2}`
    : `M${x1} ${y1} L${trunkX + r} ${y1} Q${trunkX} ${y1}, ${trunkX} ${y1 + s} L${trunkX} ${y2 - s} Q${trunkX} ${y2}, ${trunkX - r} ${y2} L${x2} ${y2}`
  return <path d={d} fill="none" strokeWidth={1} className={active ? "stroke-neutral-300" : "stroke-neutral-600"} />
}

function TreeNode({ x, y, label, width, origin, active, onEnter }: { x: number, y: number, label: string, width: number, origin: Where, active: boolean, onEnter: () => void }): JSX.Element {
  return (
    <g transform={`translate(${x - width / 2}, ${y - NODE_HEIGHT / 2})`}>
      <rect
        className="cursor-pointer fill-transparent hover:fill-[#101010]"
        width={width}
        height={NODE_HEIGHT}
        rx={NODE_HEIGHT / 4}
        stroke={active ? "rgba(255,255,255, 0.55)" : "rgba(255,255,255, 0.1)"}
        strokeWidth={active ? 1 : 0.5}
        onPointerOver={(e) => { e.stopPropagation(); setHoverHighlight(origin); onEnter() }}
        onPointerOut={(e) => { e.stopPropagation(); setHoverHighlight(null); hoverSubtree.value = null }}
      />
      <text
        transform={`translate(${width / 2}, ${NODE_HEIGHT / 2})`}
        textAnchor="middle"
        dominantBaseline="central"
        fill={colorOf(label)}
        style={{ fontSize: TREE.fontSize }}
        className="font-mono pointer-events-none"
      >
        {label}
      </text>
    </g>
  )
}

// Assigns each node its (row, col) on the grid
function layoutGrid(roots: SyntaxTree[]): GridNode[] {
  const nodes: GridNode[] = []
  const used = new Set<string>()

  // A node sits on the same row as its first child, so the leftmost spine of
  // every subtree is horizontal; its length is how many columns it spans.
  const spineOf = (node: SyntaxTree): SyntaxTree[] => node.children.length ? [node, ...spineOf(node.children[0]!)] : [node]

  // Anchor each subtree at the first row where its spine fits, then pack the
  // remaining children snugly below (childMin = previous child's row + 1).
  //
  // `used` reserves both node cells AND each parent's trunk – the vertical run in
  // the column gap, from the parent's row down to its last child's row. A node
  // may pack into any free row except one where another same-column parent's
  // trunk already passes; that reservation is what keeps unrelated trunks from
  // overlapping, while still letting non-conflicting subtrees interleave tightly.
  function layout(node: SyntaxTree, minRow: number, col: number, parent: GridNode | null): { node: GridNode, maxRow: number } {
    // every spine node but the last leaf owns a trunk
    const spine = spineOf(node)
    const fitsAt = (r: number) => spine.every((s, i) => !used.has(`${r},${col + i}`) && !(s.children.length && used.has(`g:${r},${col + i}`)))
    let row = minRow
    while (!fitsAt(row)) { row++ }

    const self: GridNode = { label: node.label, row, col, width: nodeWidth(node.label), origin: node.o, children: [], parent }
    nodes.push(self)
    used.add(`${row},${col}`)
    let maxRow = row, childMin = row, lastChildRow = row
    for (const child of node.children) {
      const placed = layout(child, childMin, col + 1, self)
      self.children.push(placed.node)
      maxRow = Math.max(maxRow, placed.maxRow)
      lastChildRow = Math.max(lastChildRow, placed.node.row)
      childMin = placed.node.row + 1
    }
    // reserve this parent's trunk (its row → its last child's row) in the gap
    if (node.children.length) {
      for (let r = row; r <= lastChildRow; r++) { used.add(`g:${r},${col}`) }
    }
    return { node: self, maxRow }
  }

  let row = 1
  for (const stmt of roots) { row = layout(stmt, row, 1, null).maxRow + 1 }

  // Pull floating spines down toward their subtree. A node whose first child is
  // small but whose other children were pushed far below (by an unrelated
  // subtree's reserved trunk) dangles high above the bulk of its own subtree –
  // e.g. `⊢ : { … }` leaves `⊢ :` stranded rows above its lambda. Slide each such
  // spine down into the free rows just above its nearest branch child. Only ever
  // moves nodes down into empty cells, so it can't create overlaps or crossings.
  const spineRoots = nodes.filter((n) => n.parent === null || n.parent.children[0] !== n).sort((a, b) => a.row - b.row)
  for (const root of spineRoots) {
    const spine: GridNode[] = []
    for (let s: GridNode | undefined = root; s; s = s.children[0]) { spine.push(s) }
    const nearestBranch = Math.min(...spine.flatMap((s) => s.children.slice(1).map((c) => c.row)))
    // no branch child to hug
    if (!isFinite(nearestBranch)) { continue }
    let newRow = root.row
    for (let r = nearestBranch - 1; r > root.row; r--) {
      if (spine.every((s) => !used.has(`${r},${s.col}`))) { newRow = r; break }
    }
    for (const s of newRow > root.row ? spine : []) {
      used.delete(`${s.row},${s.col}`)
      s.row = newRow
      used.add(`${newRow},${s.col}`)
    }
  }
  return nodes
}

// MARK: Values
// How a value's description draws: text, lists, records, errors, functions, trees,
// tensors – and live values, each its own reactive island.

const frameStyle = "rounded-xl border border-neutral-800 hover:border-neutral-700 p-2"

const Panel = ({ children, className = "", testId }: { children: JSX.Element, className?: string, testId?: string }) =>
  <div className={`${frameStyle} ${className}`} data-testid={testId}>{children}</div>

// hovering a value highlights the source it came from
function WithOrigin({ o, children }: { o?: Where, children: JSX.Element }): JSX.Element {
  if (!o) { return children }
  return (
    <span style={{ display: "contents" }} onPointerOver={(e) => { e.stopPropagation(); setHoverHighlight(o) }} onPointerOut={() => setHoverHighlight(null)}>
      {children}
    </span>
  )
}

function View({ d }: { d: Desc }): JSX.Element {
  return <WithOrigin o={d.o}><ViewInner d={d} /></WithOrigin>
}

function ViewInner({ d }: { d: Desc }): JSX.Element {
  switch (d.k) {
    case "null": return <div className="font-extrabold text-3xl">◌</div>
    case "string": return <div style={{ color: `#${tokenColors.string}` }}>"{d.v}"</div>
    case "other": return <div className="text-violet-500">{d.v}</div>
    case "list": return (
      <div className="grid gap-1 rounded-xl">
        {d.items.map((item, key) => (
          // the origin wrapper sits outside the row box – hovering a row's empty space
          // highlights that item's source, not the whole list
          <WithOrigin key={key} o={item.o}>
            <div className={`${frameStyle} !border-0 grid hover:bg-neutral-900`}><View d={item} /></div>
          </WithOrigin>
        ))}
      </div>
    )
    case "record": return (
      <div>
        {d.entries.map(([key, value]) => <div key={key}><span className="font-bold">{key}:</span> <View d={value} /></div>)}
      </div>
    )
    case "function": return <FunctionView d={d} />
    case "error": return <ErrorView d={d} />
    case "tree": return <TreeView roots={d.roots} />
    case "tensor": return <TensorPlot d={d} />
    case "live": return <LiveView id={d.id} />
    case "callback": return <div className="text-violet-500">function</div>
    case "ui": return <WidgetView d={d} />
  }
}

const Pending = () => <span className="opacity-50">…</span>

// a live value redraws on its own when it changes – nothing around it does
function LiveView({ id }: { id: number }): JSX.Element {
  useSignals()
  const now = mirrors.get(id)?.desc.value
  return now ? <View d={now} /> : <Pending />
}

// A cell of a grid or a center: a widget draws bare, a value inside a framed panel – the
// frame marks the cell
function Cell({ d }: { d: Desc }): JSX.Element {
  useSignals()
  const now = resolve(d) ?? d
  if (now.k === "ui") { return <WithOrigin o={now.o}><WidgetView d={now} /></WithOrigin> }
  // a tree grows with its drawing: no scroll container, which would clip it
  if (now.k === "tree") { return <WithOrigin o={now.o}><Panel><TreeView roots={now.roots} /></Panel></WithOrigin> }
  return <Framed d={now} />
}

// A value in a framed, scrolling panel whose whole extent carries the hover highlight; a
// view that fails to draw shows why, in place
function Framed({ d }: { d: Desc | null }): JSX.Element {
  return (
    <WithOrigin o={d?.o}>
      <Panel className="overflow-scroll" testId="print-panel">
        <ErrorBoundary fallbackRender={({ error }) => <ErrorView d={{ k: "error", chain: [{ message: `This view failed: ${(error as Error)?.message ?? error}` }] }} />} resetKeys={[d]}>
          {d ? <View d={d} /> : <Pending />}
        </ErrorBoundary>
      </Panel>
    </WithOrigin>
  )
}

// the program's value – the playground's output panel
function ResultPanel(): JSX.Element {
  useSignals()
  return <Framed d={root.desc.value ?? null} />
}

// a function's source, whole – a long line wraps – colored as the editor colors it
const SourceView = ({ source }: { source: string }) => {
  const ref = useRef<HTMLDivElement>(null)
  useEffect(() => {
    let current = true
    colorize(source).then((html) => { if (current && ref.current) { ref.current.innerHTML = html } }, () => { })
    return () => { current = false }
  }, [source])
  return <div ref={ref} className="text-sm font-mono whitespace-pre-wrap [overflow-wrap:anywhere]">{source}</div>
}

// a built-in shows its doc card; a Fluent lambda its source
function FunctionView({ d }: { d: Extract<Desc, { k: "function" }> }): JSX.Element {
  const source = d.source !== undefined ? <SourceView source={d.source} /> : null
  if (d.doc) { return <><Markdown text={docCard(d.doc)} />{source}</> }
  return source ?? <Markdown text={`\`${d.name}\` — a built-in without a doc card yet`} />
}

function ErrorView({ d }: { d: Extract<Desc, { k: "error" }> }): JSX.Element {
  return (
    <ol className="list-decimal list-inside flex flex-col gap-2 border rounded-lg border-red-700 bg-red-950 overflow-scroll max-h-24">
      {d.carried !== undefined && <li className="list-item px-2 py-0.5 text-red-700">{d.carried}</li>}
      {d.chain.map((error, index) => (
        <li
          key={index}
          className="list-item px-2 py-0.5 text-red-700 cursor-pointer hover:bg-red-700 hover:text-red-200"
          onPointerOver={() => setHoverHighlight(error.o ?? null)}
          onPointerOut={() => setHoverHighlight(null)}
        >
          {error.message}
        </li>
      ))}
    </ol>
  )
}

// MARK: Plots
// A tensor draws by its rank (tensorView): a number, bars, a heatmap, an image. The worker
// paints the pictures, the page labels them: a number shows its value, and a few bars or
// cells theirs; more show the range they span. The numbers arrive with the description or
// right after it, and until new ones arrive, the view keeps the last.

// three significant digits, for labels
const short = (value: number) => numberText(value, 3)

// the smallest and the largest finite value, [0, 0] when there is none
const extent = (values: ArrayLike<number>): [number, number] => {
  let lo = Infinity, hi = -Infinity
  for (let i = 0; i < values.length; i++) {
    const v = values[i]!
    if (Number.isFinite(v)) {
      lo = Math.min(lo, v)
      hi = Math.max(hi, v)
    }
  }
  return lo > hi ? [0, 0] : [lo, hi]
}

// A plot's frame: the y range to its left – `inset`, between the ends of a plot that keeps
// room above and below – and the x range below
const Axes = ({ y, x, inset = false, children }: { y?: [number, number], x?: [number, number], inset?: boolean, children: JSX.Element }) => (
  <div className="grid gap-x-2 text-xs text-neutral-500 font-mono" style={{ gridTemplateColumns: "auto 1fr" }}>
    <div className={`flex flex-col justify-between text-right ${inset ? "py-4" : ""}`}>{y && <><span>{short(y[1])}</span><span>{short(y[0])}</span></>}</div>
    <div className="relative" style={{ height: "10em" }}>{children}</div>
    {x && <><div /><div className="flex justify-between"><span>{short(x[0])}</span><span>{short(x[1])}</span></div></>}
  </div>
)

// Bars over the range they span, zero in it, with room above and below for a label. A few
// bars are each labelled with its value at its end – a NaN at zero, ∞ at the top; more are
// drawn in the same frame with no labels.
const Bars = ({ id, range: [lo, hi], values }: { id: number, range: [number, number], values?: Float32Array }) => {
  const n = values?.length ?? 0
  const y = (v: number) => (hi - (Number.isNaN(v) ? 0 : Math.min(Math.max(v, lo), hi))) / (hi - lo || 1) * 100
  return (
    <Axes y={[lo, hi]} inset>
      <div className="absolute inset-x-0 top-4 bottom-4">
        <PaintedCanvas id={id} fill />
        {[...values ?? []].map((v, i) => (
          <span key={i} className="absolute text-neutral-300" style={{ left: `${(i + 0.5) / n * 100}%`, top: `${y(v)}%`, transform: `translate(-50%, ${v < 0 ? 0 : -100}%)` }}>{short(v)}</span>
        ))}
      </div>
    </Axes>
  )
}

// A small heatmap, each cell labelled with its value: dark on the bright part of viridis,
// from 0.45 of the range up, where dark text stands out more than light
const Cells = ({ id, values, shape: [rows = 1, columns = 1] }: { id: number, values?: Float32Array, shape: number[] }) => {
  const [lo, hi] = extent(values ?? [])
  return (
    <div className="relative max-w-sm" style={{ aspectRatio: `${columns / rows}` }}>
      <PaintedCanvas id={id} style={{ position: "absolute", width: "100%", height: "100%", imageRendering: "pixelated" }} />
      <div className="absolute inset-0 grid text-xs font-mono" style={{ gridTemplateColumns: `repeat(${columns}, 1fr)` }}>
        {[...(values ?? [])].slice(0, rows * columns).map((v, i) => (
          <div key={i} className={`grid place-items-center ${(v - lo) / (hi - lo) > 0.45 ? "text-neutral-900" : "text-neutral-100"}`} style={{ boxShadow: "inset 0 0 0 0.5px #171717" }}>{short(v)}</div>
        ))}
      </div>
    </div>
  )
}

// A picture's range under it – a heatmap's on the viridis scale – and one value alone when
// they are all the same
const VIRIDIS_CSS = "linear-gradient(to right, #440154, #3b528b, #21918c, #5ec962, #fde725)"
const Legend = ({ range: [lo, hi], ramp }: { range: Float32Array, ramp: boolean }) => lo! > hi! ? null : (
  <>
    {short(lo!)}
    {lo! < hi! && <>{ramp ? <div className="h-1.5 w-16 rounded-full" style={{ background: VIRIDIS_CSS }} /> : <span>…</span>}{short(hi!)}</>}
  </>
)

function TensorPlot({ d }: { d: TensorDesc }): JSX.Element {
  useSignals()
  const view = tensorView(d.shape), few = labelled(d.shape), key = `${view}${few}`
  const fresh = payloads.get(d.id)?.value
  const last = useRef<{ key: string, values: Float32Array } | null>(null)
  if (typeof fresh === "string") { return <ErrorView d={{ k: "error", chain: [{ message: fresh }] }} /> }
  if (fresh) { last.current = { key, values: fresh } }
  // a picture's values when they are labelled, otherwise the range they span
  const values = last.current?.key === key ? last.current.values : undefined

  switch (view) {
    case "number": return <span style={{ color: `#${tokenColors.number}`, opacity: values ? 1 : 0.5 }}>{values ? numberText(values[0]!) : "…"}</span>
    case "bars":
      if (few) { return <Bars id={d.id} range={extent([...values ?? [], 0])} values={values} /> }
      return <Bars id={d.id} range={values ? [Math.min(values[0]!, 0), Math.max(values[1]!, 0)] : [0, 0]} />
    case "heat":
      if (few) { return <Cells id={d.id} values={values} shape={d.shape} /> }
  }
  // A picture shows at its size in units, a CSS pixel each: a heatmap's comes laid out and
  // magnified, and an image has the display's pixels, density of them a unit. An image under
  // 128 units – a raster of data, not a drawing – is magnified as a heatmap is, each pixel a
  // whole number of the display's, sampled nearest-neighbour. A picture wider than its panel
  // scales down, smoothed, so no row or column drops out. Small multiples say the shape they
  // lay out.
  const w = (d.shape[1] ?? 1) / density.value, h = (d.shape[0] ?? 1) / density.value
  const m = d.size || Math.max(w, h) >= 128 ? 1 : heatScale(h, w), [width, height] = d.size ?? [w * m, h * m]
  return (
    <div className="grid gap-1">
      <PaintedCanvas id={d.id} style={{ width: "100%", maxWidth: `${width}px`, minHeight: "4px", aspectRatio: `${width / height}`, imageRendering: m > 1 ? "pixelated" : "auto" }} />
      {/* empty, it takes no row: layers over the picture map onto the picture alone */}
      <div className="flex items-center gap-2 text-xs text-neutral-500 font-mono empty:hidden">
        {values && <Legend range={values} ramp={view === "heat"} />}
        {view === "heat" && d.shape.length > 2 && <span className="ml-2">[{d.shape.join(", ")}]</span>}
      </div>
    </div>
  )
}

// A canvas the worker paints: handed over once, then told which picture to paint as each
// new one arrives, and again at a new density. Its pixels are the worker's to size: a
// picture's own, or – filling its box, as bars do – the box's, sent in units whenever it
// changes.
let nextSlot = 0
const PaintedCanvas = ({ id, style, fill = false }: { id: number, style?: CSSProperties, fill?: boolean }) => {
  useSignals()
  const ref = useRef<HTMLCanvasElement>(null)
  const slot = useRef(0), shown = useRef(id), size = useRef<[number, number]>([0, 0])
  shown.current = id
  useEffect(() => {
    const element = ref.current!, canvas = element.transferControlToOffscreen(), mine = slot.current = ++nextSlot
    toWorker({ t: "canvas", slot: mine, canvas }, [canvas])
    const observer = new ResizeObserver(([entry]) => {
      size.current = [entry!.contentRect.width, entry!.contentRect.height]
      toWorker({ t: "paint", slot: mine, id: shown.current, size: size.current })
    })
    if (fill) { observer.observe(element) }
    return () => {
      observer.disconnect()
      toWorker({ t: "canvasGone", slot: mine })
    }
  }, [])
  useEffect(() => { if (!fill || size.current[0]) { toWorker({ t: "paint", slot: slot.current, id, size: size.current }) } }, [id, density.value])
  return <canvas ref={ref} style={fill ? { width: "100%", height: "100%" } : style} />
}

// MARK: Examples

const getExample = (k: keyof typeof EXAMPLES) => EXAMPLES[k].trim()

// ?example=nope – a binary tensor that spells 404 (2-D tensors print as a
// heatmap, so the bits render as the actual digits)
const example404 = (name: string) => dedent`
  ; example "${name.replace(/\s+/g, " ").slice(0, 60)}" not found
  [[1,0,1, 0, 1,1,1, 0, 1,0,1],
   [1,0,1, 0, 1,0,1, 0, 1,0,1],
   [1,1,1, 0, 1,0,1, 0, 1,1,1],
   [0,0,1, 0, 1,0,1, 0, 0,0,1],
   [0,0,1, 0, 1,1,1, 0, 0,0,1]]
`

// what a ?example= URL resolves to – a gallery example by name, or the 404
const exampleSource = (name: string) =>
  name in EXAMPLES ? getExample(name as keyof typeof EXAMPLES) : example404(name)

// MARK: Editor

const EDITOR_OPTIONS: editor.IStandaloneEditorConstructionOptions = {
  minimap: { enabled: false },
  wordWrap: "on",
  fontSize: 14,
  // BQN386 serves only the symbol blocks (its FontFace unicodeRange, below) – both
  // hook glyphs ⊸/⟜ come from one face instead of two mismatched fallbacks
  fontFamily: '"BQN386", Menlo, Monaco, "Courier New", monospace',
  fixedOverflowWidgets: true,
  scrollBeyondLastLine: false,
  bracketPairColorization: { independentColorPoolPerBracketType: true, enabled: true },
  unicodeHighlight: { ambiguousCharacters: false },
  // a ( or " typed over a selection wraps it, and the ) or " typed at the end steps over the
  // one the wrap added, so typing "(a + 2)" over a selection gives (a + 2)
  autoClosingOvertype: "always",
  // p-2
  padding: { top: 8, bottom: 8 },
  automaticLayout: true,
  scrollbar: { alwaysConsumeMouseWheel: false },
}
// a source shown, not edited: no gutter, cursor, hover, suggestions or scrollbars
const READONLY_OPTIONS: editor.IStandaloneEditorConstructionOptions = {
  ...EDITOR_OPTIONS,
  readOnly: true,
  contextmenu: false,
  lineNumbers: "off",
  stickyScroll: { enabled: false },
  overviewRulerBorder: false,
  overviewRulerLanes: 0,
  folding: false,
  lineDecorationsWidth: 0,
  lineNumbersMinChars: 0,
  cursorBlinking: "solid",
  cursorStyle: "line-thin",
  cursorWidth: 0,
  selectionHighlight: false,
  hover: { enabled: false },
  quickSuggestions: false,
  parameterHints: { enabled: false },
  autoClosingBrackets: "never",
  renderLineHighlight: "none",
  renderLineHighlightOnlyWhenFocus: false,
  renderValidationDecorations: "off",
  renderWhitespace: "none",
  renderControlCharacters: false,
  matchBrackets: "never",
  glyphMargin: false,
  foldingStrategy: "indentation",
  foldingHighlight: false,
  scrollbar: { vertical: "hidden", horizontal: "hidden", useShadows: true },
}

// A read-only editor: a function's source, a Code(…) value
function CodeView({ value, onChange }: { value: string, onChange?: (code: string) => void }) {
  return (
    <Panel className="!p-0 border-none overflow-hidden h-[5em]">
      <Editor
        onMount={editorOnMount}
        height="100%"
        defaultLanguage="fluent"
        theme="fluentThemeReadOnly"
        value={value}
        onChange={(next) => { if (next !== undefined) { onChange?.(next) } }}
        options={READONLY_OPTIONS}
      />
    </Panel>
  )
}

// An editor fills its container – the playground's main panel; with `auto` it sizes
// itself to its content and grows as you type – for editors in auto-sized rows (tour
// cells, REPL cells), where "100%" of an auto row would collapse to nothing.
function CodeEditorView({ value, onChange, auto }: { value: string, onChange: (code: string) => void, auto: boolean }) {
  const [height, setHeight] = useState(auto ? "38px" : "100%")
  const shownHeight = useRef(height)
  const changed = useRef(onChange)
  changed.current = onChange
  // each editor's text is parsed on its own (REPL cells stay independent)
  const validate = (source: string) => toWorker({ t: "parse", source })

  const onMount: OnMount = (editor) => {
    editorOnMount(editor)
    editorRegistry.add(editor)
    // Global shortcuts (⌘O gallery, ⌘S share) target the app's MAIN editor – the one
    // that fills its panel. Auto-grown editors are embedded cells that mount first and
    // unmount on navigation; one of them claiming this would leave the shortcuts wired
    // to a disposed editor.
    if (!auto) { mainEditor ??= editor }
    if (auto) {
      const fit = () => {
        const next = `${Math.min(editor.getContentHeight(), 320)}px`
        if (shownHeight.current === next) { return }
        shownHeight.current = next
        // next frame, not now: a same-frame resize inside a ResizeObserver delivery is
        // the textbook "ResizeObserver loop" error, and the dev overlay blocks the page
        requestAnimationFrame(() => setHeight(next))
      }
      editor.onDidContentSizeChange(fit)
      fit()
      // word-wrap breaks are computed on the first real layout, one frame after mount –
      // measure once more when that layout has run
      requestAnimationFrame(() => requestAnimationFrame(fit))
    }
    validate(editor.getValue())
  }

  return (
    <Editor
      onMount={onMount}
      height={height}
      defaultLanguage="fluent"
      className={`${frameStyle} !p-0 overflow-hidden`}
      theme="fluentTheme"
      value={value}
      onChange={(next) => {
        if (next === undefined) { return }
        changed.current(next)
        validate(next)
      }}
      options={EDITOR_OPTIONS}
    />
  )
}

// The token under the cursor: a letter-identifier (Greek included) via Monaco's
// word detection, or an operator run (..<, ⌈, ⊗, ∇, ←) expanded by hand – the
// language's word pattern only covers letters, so glyphs need this.
const RESERVED_HOVER = new Set(["|", ",", "{", "}", "(", ")", "[", "]", ";", '"', "`"])
const isOperatorChar = (ch: string | undefined): ch is string =>
  !!ch && !/\s/.test(ch) && !/[\p{L}\p{N}]/u.test(ch) && !RESERVED_HOVER.has(ch)
const isIdentChar = (ch: string | undefined): ch is string => !!ch && /[\p{L}\p{N}]/u.test(ch)

function fluentTokenAt(model: editor.ITextModel, position: monaco.Position): string | null {
  const line = model.getLineContent(position.lineNumber)
  const classify = (ch: string | undefined) => isIdentChar(ch) ? isIdentChar : isOperatorChar(ch) ? isOperatorChar : null
  // the character at the cursor, or the one just before it
  let i = position.column - 1
  let same = classify(line[i])
  if (!same) { i -= 1; same = classify(line[i]) }
  if (!same) { return null }
  let start = i, end = i
  while (start > 0 && same(line[start - 1])) { start-- }
  while (end < line.length - 1 && same(line[end + 1])) { end++ }
  return line.slice(start, end + 1)
}

// A hover or completion card: signature, one-liner, the value's other names, worked
// example. Completion's details pane already shows the signature as its header, so its
// body leaves it out.
const fenced = (code?: string) => code && "```fluent\n" + code + "\n```"
const docCard = (m: Doc, signature = true): string =>
  [signature && fenced(m.signature), m.doc, m.also && "Also " + m.also.map((name) => "`" + name + "`").join(", "), fenced(m.example)].filter(Boolean).join("\n\n")

// Monaco's language setup is global, so it runs once, as the page loads – before any
// editor mounts or any text is colorized. register*Provider calls accumulate, each one
// stacking another doc card onto every hover: the providers live on globalThis, and a
// hot-reloaded module replaces its predecessor's instead of adding to them.
const registeredProviders: { dispose(): void }[] =
  ((globalThis as Record<string, unknown>).__fluentMonacoProviders ??= []) as { dispose(): void }[]
for (const p of registeredProviders.splice(0)) {
  try { p.dispose() } catch { /* already gone */ }
}

monaco.languages.register({ id: "fluent" })

// Docs live on the built-ins' metadata (attached inline in the prelude via doc()); the
// worker lists every name in the environment with its card when it starts.
registeredProviders.push(monaco.languages.registerHoverProvider("fluent", {
  provideHover(model, position) {
    const token = fluentTokenAt(model, position)
    const meta = token ? environmentNames.peek().get(token)?.doc : null
    return meta ? { contents: [{ value: docCard(meta) }] } : null
  },
}))

monaco.languages.setMonarchTokensProvider("fluent", {
  defaultToken: "",
  unicode: true,
  tokenizer: {
    root: [
      [/;.*$/, { token: "comment" }],
      ...numberRules.map(([regexp, ...tokens]): [RegExp, string | string[]] => [regexp, tokens.length > 1 ? tokens : tokens[0]!]),
      [/"/, { token: "string", next: "@string" }],
      [identifierRegexp, { token: "identifier" }],
      [delimiterRegexp, { token: "delimiter" }],
      [operatorRegexp, { token: "operator" }],
    ],
    // a backslash escapes the character after it
    string: [
      [/[^"\\]+/, { token: "string" }],
      [/\\./, { token: "string" }],
      [/"/, { token: "string", next: "@pop" }],
    ],
  },
})

const PAIRS = ["{}", "[]", "()"]
monaco.languages.setLanguageConfiguration("fluent", {
  brackets: PAIRS.map(([open, close]) => [open!, close!]),
  autoClosingPairs: [...PAIRS, '""'].map(([open, close]) => ({ open: open!, close: close! })),
  surroundingPairs: [...PAIRS, '""'].map(([open, close]) => ({ open: open!, close: close! })),
  indentationRules: { increaseIndentPattern: /^\s*[\(\{\[]/, decreaseIndentPattern: /^\s*[\)\}\]]/ },
  // identifiers: letter-based symbols
  wordPattern: /(?:\p{L})[\p{L}\p{N}]*/gu,
  comments: { lineComment: ";" },
})

for (const [name, background] of [["fluentTheme", "#1C1C1C"], ["fluentThemeReadOnly", "#1C1C1C00"]]) {
  monaco.editor.defineTheme(name!, {
    base: "vs-dark",
    inherit: true,
    rules: COLORS,
    colors: {
      "editor.foreground": `#${tokenColors.string}`,
      "editorSuggestWidget.foreground": "#D8D8D8",
      "editorSuggestWidget.background": "#1C1C1C",
      "editor.background": background!,
      "focusBorder": "#FF000000",
    },
  })
}
// a page with no editor colors the code in its prose too
monaco.editor.setTheme("fluentTheme")

// BQN386 (dzaima/BQN386, APL385-derived, free for any use) supplies the
// tacit/APL glyph blocks as one matched set – system monospace fonts miss
// ⟜ (U+27DC) and pull its mirror ⊸ (U+22B8) from a different fallback, so
// the pair renders at two sizes. Registered via the FontFace API instead of
// a CSS @font-face because Bun's bundler fails hot-swapping CSS that
// carries a binary asset; /BQN386.ttf is served as a plain static file.
// The unicode-range keeps ASCII and letters on the system mono.
// relative URL, like mnist.safetensors: GitHub Pages serves the app from
// /fluent/, so an absolute /BQN386.ttf would 404 and silently drop the font
const symbolFont = new FontFace("BQN386", 'url("BQN386.ttf")', {
  unicodeRange: "U+2016, U+2190-23FF, U+27C0-27FF, U+2980-29FF, U+2A00-2AFF",
})
document.fonts.add(symbolFont)
// Monaco measures glyph widths at mount – remeasure once the font arrives
// or cursor/selection x-positions drift on lines with ⊸ ⟜ ⍨ ⍥. A failed
// load (e.g. a dev server started before the /BQN386.ttf route existed)
// must degrade to the system fallback fonts, not an error overlay.
symbolFont.load()
  .then(() => monaco.editor.remeasureFonts())
  .catch(() => console.warn("BQN386 failed to load – symbol glyphs fall back to system fonts"))

// ⌘⇧F formats, ⌘K and ⌘P open the command palette, and F1 doesn't
for (const [keybinding, command, when] of [
  [monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyF, "editor.action.formatDocument"],
  [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyK, "editor.action.quickCommand", "editorTextFocus"],
  [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyP, "editor.action.quickCommand", "editorTextFocus"],
  [monaco.KeyCode.F1, "-editor.action.quickCommand", "editorTextFocus"],
] as [number, string, string?][]) {
  monaco.editor.addKeybindingRule({ keybinding, command, when })
}

// Completion offers symbols by name – "alpha" for α – and every name in scope: the
// canonical binding (SignalOnce) and its prelude alias (once) both list, and share one
// doc card, since the doc lives on the value they both name.
// [symbol, name, detail, filter terms]
const GREEK = (letters: string, detail: string) => letters.split(",").map((entry) => {
  const [symbol, name, ...terms] = entry.split(" ")
  return [symbol!, name!, `Greek ${detail} ${name}`, [name, ...terms].join(" ")] as [string, string, string, string]
})
const SYMBOL_TABLE: [string, string, string, string][] = [
  ...GREEK("α alpha a,β beta b,γ gamma g,δ delta d,ε epsilon e,ζ zeta z,η eta,θ theta,ι iota i,κ kappa k,λ lambda l,μ mu m,ν nu n,ξ xi x,π pi p,ρ rho r,σ sigma s,τ tau t,υ upsilon u,φ phi f,χ chi c,ψ psi,ω omega o", "lowercase"),
  ...GREEK("Γ Gamma G,Δ Delta D,Θ Theta,Λ Lambda L,Ξ Xi X,Φ Phi F,Ψ Psi,Ω Omega O", "uppercase"),
  ["Π", "Pi", "Greek uppercase Pi / Product", "Pi P product"],
  ["Σ", "Sigma", "Greek uppercase Sigma / Sum", "Sigma S sum summation"],
  // Calculus & Analysis
  ["∇", "nabla", "Nabla / Gradient", "nabla gradient grad del"],
  ["∂", "partial", "Partial derivative", "partial derivative"],
  ["∫", "integral", "Integral", "integral int"],
  ["∮", "contour", "Contour integral", "contour integral oint"],
  ["∬", "double integral", "Double integral", "double integral iint"],
  ["∞", "infinity", "Infinity", "infinity inf"],
  ["′", "prime", "Prime (derivative)", "prime derivative"],
  ["″", "double prime", "Double prime", "double prime pprime"],
  // Arithmetic operators
  ["×", "times", "Multiplication / Cross product", "times cross multiply x"],
  ["÷", "divide", "Division", "divide division"],
  ["±", "plus-minus", "Plus-minus", "plus minus pm plusminus"],
  ["∓", "minus-plus", "Minus-plus", "minus plus mp minusplus"],
  ["√", "sqrt", "Square root", "sqrt square root"],
  ["∛", "cbrt", "Cube root", "cbrt cube root"],
  ["·", "dot", "Dot product / Multiplication", "dot cdot multiply"],
  ["∘", "compose", "Function composition", "compose circ circle"],
  // Comparison & Relations
  ["≠", "not equal", "Not equal", "not equal neq !="],
  ["≈", "approx", "Approximately equal", "approx approximately"],
  ["≡", "match", "Match – same value, whole", "match equiv equivalent identical same"],
  ["≢", "not equiv", "Not equivalent", "not equiv nequiv"],
  ["≤", "leq", "Less than or equal", "leq less equal <="],
  ["≥", "geq", "Greater than or equal", "geq greater equal >="],
  ["≪", "much less", "Much less than", "much less ll"],
  ["≫", "much greater", "Much greater than", "much greater gg"],
  ["∝", "proportional", "Proportional to", "proportional propto"],
  ["≅", "congruent", "Congruent / Isomorphic", "congruent cong isomorphic"],
  ["∼", "similar", "Similar to", "similar sim tilde"],
  // Set theory
  ["∈", "element of", "Element of", "element of in"],
  ["∉", "not element", "Not element of", "not element notin"],
  ["∋", "contains", "Contains as member", "contains ni"],
  ["⊂", "subset", "Subset", "subset sub"],
  ["⊃", "superset", "Superset", "superset sup"],
  ["⊆", "subseteq", "Subset or equal", "subseteq subset equal"],
  ["⊇", "supseteq", "Superset or equal", "supseteq superset equal"],
  ["∪", "union", "Union", "union cup"],
  ["∩", "intersection", "Intersection", "intersection cap"],
  ["∅", "empty set", "Empty set", "empty set emptyset null"],
  ["∖", "set minus", "Set minus", "set minus setminus difference"],
  // Logic
  ["∀", "forall", "For all", "forall for all universal"],
  ["∃", "exists", "There exists", "exists exist existential"],
  ["∄", "not exists", "There does not exist", "not exists nexists"],
  ["¬", "not", "Logical not", "not neg lnot"],
  ["∧", "and", "Logical and", "and land wedge"],
  ["∨", "or", "Logical or", "or lor vee"],
  ["⊕", "xor", "Exclusive or / Direct sum", "xor oplus direct sum"],
  ["⊗", "tensor", "Tensor product", "tensor otimes product"],
  ["⊥", "perpendicular", "Perpendicular / Bottom", "perpendicular perp bottom false"],
  ["⊤", "top", "Top / True", "top true tautology"],
  ["⊢", "proves", "Proves / Entails", "proves vdash entails turnstile"],
  ["⊨", "models", "Models / Satisfies", "models vDash satisfies"],
  // Arrows
  ["→", "right arrow", "Right arrow", "right arrow to ->"],
  ["←", "left arrow", "Left arrow / Signal write", "left arrow from <- assign write"],
  ["↔", "bidir arrow", "Bidirectional arrow", "bidir arrow leftrightarrow <->"],
  ["⇒", "implies", "Implies", "implies Rightarrow =>"],
  ["⇐", "implied by", "Implied by", "implied by Leftarrow <="],
  ["⇔", "iff", "If and only if", "iff Leftrightarrow biconditional <=>"],
  ["↑", "up arrow", "Up arrow", "up arrow uparrow"],
  ["↓", "down arrow", "Down arrow", "down arrow downarrow"],
  ["↦", "maps to", "Maps to", "maps to mapsto"],
  ["⟳", "loop", "Loop / Repeat", "loop repeat cycle"],
  ["⍣", "power", "Power / Inverse / Until", "power iterate repeat star inverse until fixpoint"],
  ["⍢", "around", "Around – do f in g's world", "around under dual conjugate"],
  // Number sets
  ["ℕ", "naturals", "Natural numbers", "naturals N natural numbers"],
  ["ℤ", "integers", "Integers", "integers Z integer numbers"],
  ["ℚ", "rationals", "Rational numbers", "rationals Q rational numbers"],
  ["ℝ", "reals", "Real numbers", "reals R real numbers"],
  ["ℂ", "complex", "Complex numbers", "complex C complex numbers"],
  // Other useful symbols
  ["°", "degree", "Degree", "degree deg"],
  ["‖", "norm", "Norm", "norm parallel Vert"],
  ["∥", "parallel", "Parallel", "parallel"],
  ["⟨", "langle", "Left angle bracket", "langle left angle bracket"],
  ["⟩", "rangle", "Right angle bracket", "rangle right angle bracket"],
  ["⌊", "floor", "Floor (left)", "floor lfloor"],
  ["⌋", "floor", "Floor (right)", "floor rfloor"],
  ["⌈", "ceil", "Ceiling (left)", "ceil ceiling lceil"],
  ["⌉", "ceil", "Ceiling (right)", "ceil ceiling rceil"],
  ["ℏ", "hbar", "Reduced Planck constant", "hbar planck"],
  ["ℓ", "ell", "Script small l", "ell script l"],
  ["℘", "wp", "Weierstrass p", "wp weierstrass"],
  ["ℵ", "aleph", "Aleph (cardinal)", "aleph cardinal"],
]
const TABLED = new Set(SYMBOL_TABLE.map(([symbol]) => symbol))
registeredProviders.push(monaco.languages.registerCompletionItemProvider("fluent", {
  provideCompletionItems: (model, position) => {
    const word = model.getWordUntilPosition(position)
    const range = { startLineNumber: position.lineNumber, endLineNumber: position.lineNumber, startColumn: word.startColumn, endColumn: word.endColumn }
    return {
      suggestions: [
        ...SYMBOL_TABLE.map(([symbol, name, detail, filterText]) => ({
          label: `${symbol} ${name}`, kind: monaco.languages.CompletionItemKind.Text, insertText: symbol, detail, range, filterText,
        })),
        ...[...environmentNames.peek()].map(([key, { detail, doc }]) => {
          // a glyph the table doesn't name is found by its other names: "each" offers ¨
          const names = /[\p{L}\p{N}\x00-\x7f]/u.test(key) || TABLED.has(key) ? [] : [...doc?.also ?? []].sort((a, b) => a.length - b.length)
          return {
            label: [key, ...names].join(" "), kind: monaco.languages.CompletionItemKind.Function, insertText: key,
            documentation: doc ? { value: docCard(doc, false) } : `Function: ${key}`, detail, range, filterText: [...names, key].join(" "),
          }
        }),
      ],
    }
  },
}))

// the gallery (⌘O) and sharing as a URL (⌘S), for every editor
const editorOnMount = (editor: editor.IStandaloneCodeEditor) => {
  const quickInput = editor.addCommand(0, (accessor, use) => use(accessor.get(IQuickInputService)))!
  // the URL names a pristine example; a saved program is its own document, in ?code=
  const setUrl = (key: string, value: string) => {
    const url = new URL(window.location.href)
    url.searchParams.delete(key === "code" ? "example" : "code")
    url.searchParams.set(key, value)
    if (key === "code") { url.searchParams.set("v", String(VERSION)) } else { url.searchParams.delete("v") }
    window.history.pushState({}, "", url.toString())
  }
  editor.addAction({
    id: "fluent-load-example",
    label: "Load example",
    contextMenuGroupId: "navigation",
    contextMenuOrder: 1.5,
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyO],
    run: (editor) => {
      // an example's first comment line describes it
      const items = Object.entries(EXAMPLES).map(([label, code]) => {
        const first = code.trim().split("\n")[0] ?? ""
        return { label, description: first.startsWith(";") ? first.replace(/^;\s*/, "") : "" }
      })
      editor.trigger("", quickInput, (service: any) => {
        service.pick(items, { placeHolder: "Select an example to load" }).then((selected: any) => {
          if (!selected) { return }
          editor.getModel()?.setValue(getExample(selected.label))
          setUrl("example", selected.label)
        })
      })
    },
  })
  editor.addAction({
    id: "fluent-save-example",
    label: "Save example",
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS],
    run: (editor) => setUrl("code", StringSerialize(editor.getModel()?.getValue() ?? "")),
  })
}

// MARK: Playground

// Shortcuts the browser also wants (Safari especially: ⌘O = open file,
// ⌘S = save page, ⌘P = print). Monaco's own keybindings only hear keys while
// the editor is focused – claim these at the window level, capture phase,
// so they work from anywhere on the page and beat the browser dialog.
// Safari handles ⌘O in its menu BEFORE the page ever sees a keydown, so it
// cannot be intercepted at all there – the ⇧ variants are the escape hatch
// (⌘⇧O opens the gallery everywhere; ⌘⇧P matches VS Code's palette anyway).
const GLOBAL_SHORTCUTS: Record<string, { action: string, withShift?: boolean }> = {
  KeyO: { action: "fluent-load-example", withShift: true },
  KeyS: { action: "fluent-save-example" },
  KeyP: { action: "editor.action.quickCommand", withShift: true },
}

// The one window error the spec itself calls ignorable: Monaco's automaticLayout inside a
// same-frame-resized container (an auto-growing editor cell) reports "ResizeObserver loop
// completed with undelivered notifications", and the dev overlay would block the page.
window.addEventListener("error", (e) => {
  if (String(e.message).includes("ResizeObserver loop")) { e.stopImmediatePropagation(); e.preventDefault() }
}, true)

// The program: from the URL – ?code=, or a gallery ?example= – then every edit. The worker
// runs the latest. A saved program says which version of Fluent it is written in (?v=, 1
// when it doesn't); one written in an older version comes back migrated, and runs as that.
const programInUrl = (): { source: string, v: number } | null => {
  const params = new URLSearchParams(window.location.search)
  const code = StringDeserialize(params.get("code") ?? ""), example = params.get("example")
  return code !== "" ? { source: code, v: Number(params.get("v") ?? 1) } : example !== null ? { source: exampleSource(example), v: VERSION } : null
}
const linked = programInUrl()
// ?copybook is the copybook (copybook.tsx): it has no program
const onCopybook = new URLSearchParams(window.location.search).has("copybook")
// ?hand is the copybook's lines in the hand's form (hand.tsx): it has no program
const onHand = new URLSearchParams(window.location.search).has("hand")
// ?canvas is the canvas (canvas.tsx): its lines are the program
const onCanvas = new URLSearchParams(window.location.search).has("canvas")
const program = signal(linked?.source ?? (onCopybook || onHand || onCanvas ? "" : "Tour"))
let written = linked?.v ?? VERSION
effect(() => { toWorker({ t: "code", source: program.value, v: written }); written = VERSION })

// the main editor follows the program: an edit re-renders it, and nothing else
function MainEditor(): JSX.Element {
  useSignals()
  return <CodeEditorView value={program.value} onChange={(next) => { program.value = next }} auto={false} />
}

function Playground(): JSX.Element {
  useEffect(() => {
    const onKeyDown = (e: KeyboardEvent) => {
      const shortcut = (e.metaKey || e.ctrlKey) && !e.altKey ? GLOBAL_SHORTCUTS[e.code] : undefined
      if (!shortcut || !mainEditor || (e.shiftKey && !shortcut.withShift)) { return }
      e.preventDefault()
      // Monaco would fire the same action again
      e.stopPropagation()
      mainEditor.focus()
      mainEditor.getAction(shortcut.action)?.run()
    }
    // back and forward restore the program from the URL; pushState (⌘S) fires nothing,
    // and the editor already holds that program. A program the editor already shows is
    // left as it is: nothing would be sent, and the link's version would wait for the
    // next edit.
    const onPopState = () => { const l = programInUrl(); if (l && l.source !== program.peek()) { written = l.v; program.value = l.source } }
    window.addEventListener("keydown", onKeyDown, true)
    window.addEventListener("popstate", onPopState)
    return () => {
      window.removeEventListener("keydown", onKeyDown, true)
      window.removeEventListener("popstate", onPopState)
    }
  }, [])

  return (
    <div className="absolute inset-0 z-50 p-2 bg-neutral-900 grid text-white">
      <div className="grid gap-2 overflow-scroll h-full" style={{ gridTemplateColumns: "repeat(2, minmax(auto, 1fr))", gridTemplateRows: "repeat(1, minmax(auto, 1fr))" }}>
        <div className="contents"><ResultPanel /></div>
        <div className="contents"><MainEditor /></div>
      </div>
    </div>
  )
}

// what a page of the pen needs of the playground: a value's view, the editor's colors, and the worker to run and to parse
const toPage = { View, colorsOf, run: (source: string) => { program.value = source }, parse: (source: string) => toWorker({ t: "parse", source }) }
createRoot(document.getElementById("root")!).render(onCopybook ? <Copybook /> : onHand ? <Hand /> : onCanvas ? <Canvas {...toPage} /> : <Playground />)
