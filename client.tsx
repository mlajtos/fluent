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

import { Signal, signal, computed } from "@preact/signals-core"
import {
  getParseErrors, PRELUDE,
  identifierRegexp, numberRegexp, stringRegexp, operatorRegexp, delimiterRegexp,
} from "./language"
import { tensorView, documentHash, type Desc, type Doc, type Payload, type SourceKind, type TensorView, type ToPage, type ToWorker, type Tree as SyntaxTree, type Where } from "./protocol"
import { Documentation } from "./documentation"
import { useSignals, useSignal } from '@preact/signals-react/runtime';
import { ErrorBoundary } from 'react-error-boundary';
import { useEffect, type JSX, useMemo, useRef, useState } from "react";
import { createRoot } from "react-dom/client"
import { type BeforeMount, type Monaco, type OnMount, Editor, loader } from '@monaco-editor/react';
import * as monaco from "monaco-editor";
import { type editor } from "monaco-editor";
// @ts-ignore
import { renderMarkdown } from "monaco-editor/esm/vs/base/browser/markdownRenderer.js"
// @ts-ignore
import { IQuickInputService } from "monaco-editor/esm/vs/platform/quickinput/common/quickInput"
import dedent from "ts-dedent";
import { Base64 } from 'js-base64'
import { type Annotations } from "plotly.js"


// Web Worker setup for monaco-editor
(globalThis as { MonacoEnvironment?: typeof MonacoEnvironment }).MonacoEnvironment = {
  getWorker: () => new Worker(new URL("monaco.worker.js", document.baseURI), { type: "module" }),
}

// Configure @monaco-editor/react to use local monaco-editor package
loader.config({ monaco })

// MARK: Worker
// The program runs in a worker (worker.ts): the engine, the GPU and every value live
// there. The page draws descriptions of values (protocol.ts), writes back through ids,
// and streams the camera, the microphone and the pointer in.

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
const mirrors = new Map<number, Mirror>()
// a tensor's values, or the message saying why they couldn't be computed
const payloads = new Map<number, Signal<Payload | string | undefined>>()
const result = signal<Desc | null>(null)
const environmentNames = signal(new Map<string, { detail: string, doc: Doc | null }>())

// A hot update starts a fresh evaluator and stops the old one's sources
const hot = globalThis as { __fluentWorker?: Worker, __fluentSources?: Map<number, () => void> }
hot.__fluentWorker?.terminate()
const pageSources = hot.__fluentSources ??= new Map()
for (const stop of pageSources.values()) { stop() }
pageSources.clear()
const worker = hot.__fluentWorker = new Worker(new URL("fluent.worker.js", document.baseURI), { type: "module" })
const toWorker = (message: ToWorker, transfer: Transferable[] = []) => worker.postMessage(message, transfer)

// Every live value and tensor a description mentions is held once more…
const hold = (d: Desc): void => {
  switch (d.k) {
    case "live":
    case "callback": {
      let mirror = mirrors.get(d.id)
      if (!mirror) {
        mirror = { desc: signal(undefined), refs: 0, received: 0, sent: 0, ack: 0, local: signal(undefined) }
        mirrors.set(d.id, mirror)
      }
      mirror.refs++
      mirror.received++
      if (d.k === "live" && d.now) { show(mirror, d.now) }
      return
    }
    case "tensor": { payloads.set(d.id, signal(d.payload)); return }
    case "list": { d.items.forEach(hold); return }
    case "record": { d.entries.forEach(([, v]) => hold(v)); return }
    case "ui": { Object.values(d.slots).forEach(hold); d.children.forEach(hold); return }
  }
}
// …and once less when it goes. What nothing mentions, the worker may let go of too.
const release = (d: Desc): void => {
  switch (d.k) {
    case "live":
    case "callback": {
      const mirror = mirrors.get(d.id)
      if (!mirror || --mirror.refs > 0) { return }
      mirrors.delete(d.id)
      const now = mirror.desc.peek()
      if (now) { release(now) }
      toWorker({ t: "drop", id: d.id, count: mirror.received })
      return
    }
    case "tensor": { payloads.delete(d.id); return }
    case "list": { d.items.forEach(release); return }
    case "record": { d.entries.forEach(([, v]) => release(v)); return }
    case "ui": { Object.values(d.slots).forEach(release); d.children.forEach(release); return }
  }
}
// a mirror holds what its current value mentions
const show = (mirror: Mirror, desc: Desc) => {
  hold(desc)
  const old = mirror.desc.peek()
  mirror.desc.value = desc
  if (old) { release(old) }
}

worker.onmessage = ({ data: m }: MessageEvent<ToPage>) => {
  switch (m.t) {
    case "ready": {
      environmentNames.value = new Map(m.names.map(([name, detail, doc]) => [name, { detail, doc }]))
      return
    }
    case "result": {
      // the worker let go of everything the last result held
      mirrors.clear()
      payloads.clear()
      hold(m.desc)
      result.value = m.desc
      return
    }
    case "lives": {
      for (const { id, desc, ack } of m.lives) {
        const mirror = mirrors.get(id)
        // nothing here shows it any more: count what it mentions, and give it all back
        if (!mirror) { hold(desc); release(desc); continue }
        mirror.ack = ack
        show(mirror, desc)
      }
      // drawn by the next frame: then the worker may send the next batch
      requestAnimationFrame(() => toWorker({ t: "drawn" }))
      return
    }
    case "tensor": {
      const payload = payloads.get(m.id)
      if (payload) { payload.value = m.error ?? m.payload }
      return
    }
    case "source": { startSource(m.id, m.kind, m.size, m.fps); return }
    case "stop": { pageSources.get(m.id)?.(); pageSources.delete(m.id); return }
    case "needSampleRate": { toWorker({ t: "sampleRate", value: sampleRate() }); return }
  }
}
worker.onerror = (e) => {
  result.value = { k: "error", chain: [{ message: `The evaluator stopped: ${e.message || "it failed to start"}` }] }
}

let writes = 0
const write = (id: number, value: number | number[] | string | ImageBitmap) => {
  const mirror = mirrors.get(id)
  if (!mirror) { return }
  mirror.sent = ++writes
  const image = typeof value === "object" && !Array.isArray(value)
  if (!image) { mirror.local.value = value }
  toWorker({ t: "set", id, value, seq: mirror.sent }, image ? [value] : [])
}
const call = (id: number) => toWorker({ t: "call", id })

// What a description shows now – through live values to their current value. Reading it
// in a component subscribes the component.
const resolve = (d: Desc | undefined): Desc | undefined => {
  for (let i = 0; d?.k === "live" && i < 64; i++) { d = mirrors.get(d.id)?.desc.value }
  return d
}
const payloadOf = (d: Desc | undefined): Payload | undefined => {
  const p = d?.k === "tensor" ? payloads.get(d.id)?.value : undefined
  return typeof p === "string" ? undefined : p
}
const numberOf = (d: Desc | undefined): number | undefined => {
  const p = payloadOf(resolve(d))
  return p instanceof Float32Array ? p[0] : undefined
}
const textOf = (d: Desc | undefined): string | undefined => {
  const r = resolve(d)
  return r?.k === "string" ? r.v : undefined
}

// A control bound to a live value shows its own write until the worker has answered it,
// then the worker's value
const useBound = (target: Desc | undefined) => {
  const id = target?.k === "live" ? target.id : undefined
  const mirror = id === undefined ? undefined : mirrors.get(id)
  const remote = resolve(target)
  const local = mirror?.local.value
  const pending = mirror !== undefined && mirror.ack < mirror.sent
  return {
    remote,
    local: pending ? local : undefined,
    write: (value: number | number[] | string | ImageBitmap) => { if (id !== undefined) { write(id, value) } },
  }
}

// The audio hardware's sample rate, read once
let knownSampleRate: number | null = null
const sampleRate = (): number => {
  if (knownSampleRate === null) {
    const context = new AudioContext()
    knownSampleRate = context.sampleRate
    context.close()
  }
  return knownSampleRate
}

// The camera, the microphone, the pointer: the worker asks for a source, and the page
// streams its frames there – one per display frame at most
const startSource = (id: number, kind: SourceKind, size: number[], fps = 30) => {
  let active = true
  const cleanups: (() => void)[] = []
  pageSources.set(id, () => { active = false; cleanups.forEach((f) => f()) })
  const everyFrame = (f: (now: number) => void) => {
    const tick = (now: number) => {
      if (!active) { return }
      f(now)
      requestAnimationFrame(tick)
    }
    requestAnimationFrame(tick)
  }
  const send = (frame: { bitmap?: ImageBitmap, samples?: Float32Array }) =>
    toWorker({ t: "frame", id, ...frame }, frame.bitmap ? [frame.bitmap] : [frame.samples!.buffer as ArrayBuffer])

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
    const video = document.createElement("video")
    video.autoplay = true
    video.playsInline = true
    let stream: MediaStream | null = null
    navigator.mediaDevices.getUserMedia({ video: { width, height } }).then((s) => {
      // permission granted after the source stopped: release the camera at once
      if (!active) { s.getTracks().forEach((track) => track.stop()); return }
      stream = s
      video.srcObject = s
      video.onloadedmetadata = () => video.play()
    }).catch((err) => console.error("Camera access denied:", err))
    cleanups.push(() => {
      stream?.getTracks().forEach((track) => track.stop())
      video.srcObject = null
    })
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
  let stream: MediaStream | null = null
  let context: AudioContext | null = null
  let analyser: AnalyserNode | null = null
  navigator.mediaDevices.getUserMedia({ audio: true }).then((s) => {
    if (!active) { s.getTracks().forEach((track) => track.stop()); return }
    stream = s
    context = new AudioContext()
    analyser = context.createAnalyser()
    analyser.fftSize = n * 2
    if (kind === "spectrum") { analyser.smoothingTimeConstant = 0.8 }
    context.createMediaStreamSource(s).connect(analyser)
  }).catch((err) => console.error("Microphone access denied:", err))
  cleanups.push(() => {
    stream?.getTracks().forEach((track) => track.stop())
    context?.close()
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

// react-plotly is weirdly packaged - need to access default twice
// @ts-ignore
const Plot = (await import("react-plotly.js")).default.default as Plot

import "./index.css"

// MARK: Parse

const tokenColors = {
  number: "0A81EF",
  identifier: "30CD50",
  operator: "F238E3",
  string: "D8D8D8",
  delimiter: "F94C4A",
  comment: "444444",
}

const COLORS: editor.ITokenThemeRule[] = [
  ...Object.entries(tokenColors).map(([key, value]) => ({ token: key, foreground: value })),
  { token: "comment.md", foreground: "7F7F7F" },
  { token: "keyword.md", foreground: "7F7F7F" },
]

function getColorForSyntaxTreeNode(code: string) {
  let color: string
  switch (true) {
    case numberRegexp.test(code):
      color = tokenColors["number"]
      break;
    case stringRegexp.test(code):
      color = tokenColors["string"]
      break;
    case identifierRegexp.test(code):
      color = tokenColors["identifier"]
      break;
    case operatorRegexp.test(code):
      color = tokenColors["operator"]
      break;
    default:
      color = "#D4D4D4"
  }

  return "#" + color
}

const StringSerialize = (s: string) => Base64.encodeURI(s)
const StringDeserialize = (s: string) => Base64.decode(s)

// Monaco's markdown renderer refuses relative link targets ("?example=lenia")
// and renders them as plain text. Smuggle them through as an absolute URL on
// a sentinel origin, then rewrite the rendered hrefs back to relative – so
// prose can link into the playground ([gallery](?example=lenia)).
const RELATIVE_LINK_SENTINEL = "https://fluent-relative.internal/"

const convertTextToHTML = async (value: string) => {
  const renderable = value.replace(/\]\((\?[^)\s]+)\)/g, (_, query) => `](${RELATIVE_LINK_SENTINEL}${query})`)
  const result = await renderMarkdown({ value: renderable }, {
    codeBlockRenderer: colorizeCodeAsync,
  })

  const element = result.element as HTMLElement

  element.querySelectorAll("a").forEach((a) => {
    // the renderer leaves href empty and expects an action handler to run
    // data-href – prose wants real navigation
    const href = a.getAttribute("href") || a.getAttribute("data-href")
    if (!href) { return }
    if (href.startsWith(RELATIVE_LINK_SENTINEL)) {
      const relative = new URL(href).search
      a.setAttribute("href", relative)
      a.setAttribute("title", relative)
    } else {
      a.setAttribute("href", href)
    }
  })

  // Unwrap single <p> elements for simple single-line text
  const firstChild = element.children[0]
  if (element.children.length === 1 && firstChild?.tagName === 'P') {
    while (firstChild.firstChild) {
      element.insertBefore(firstChild.firstChild, firstChild)
    }
    firstChild.remove()
  }

  // Post-process: colorize inline <code> elements (not inside code blocks)
  const inlineCodeElements = element.querySelectorAll('code:not(.code code)')

  if (inlineCodeElements.length > 0) {
    const colorize = (await getEditor()).colorize

    await Promise.all(Array.from(inlineCodeElements).map(async (el) => {
      const text = el.textContent ?? ''
      if (text) {
        try {
          const colorized = await colorize(text, 'fluent', {})
          // colorize wraps content in divs, extract inner HTML without line breaks
          const temp = document.createElement('div')
          temp.innerHTML = colorized
          const inner = temp.querySelector('div')?.innerHTML ?? temp.innerHTML
          el.innerHTML = inner.replace(/<br\/?>/g, '')
        } catch {
          // Keep original text as fallback
        }
      }
    }))
  }

  return element
}

const colorizeCodeAsync = async (language: string, value: string) => {
  const colorize = (await getEditor()).colorize
  const e = document.createElement("div")
  e.className = "overflow-hidden"
  e.style.fontFamily = "monospace"
  try {
    e.innerHTML = await colorize(value, language, {})
  } catch (error) {
    e.textContent = value
  }
  return e
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
    convertTextToHTML(text).then((element) => {
      if (current) { ref.current?.replaceChildren(element) }
    })
    return () => { current = false }
  }, [text])
  return <div className="prose prose-neutral prose-invert"><div ref={ref}></div></div>
}

const TextView = ({ d }: { d: UIDesc }) => <Markdown text={String(d.props.text)} />

const ButtonView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const label = textOf(d.slots.label) ?? "Button"
  const action = d.slots.onClick?.k === "callback" ? d.slots.onClick.id : undefined
  return <button className="bg-neutral-900 hover:bg-neutral-800 active:bg-neutral-700 rounded-xl border border-neutral-400 hover:border-neutral-300 active:border-neutral-200 disabled:border-neutral-800 disabled:cursor-not-allowed p-2 overflow-hidden" onClick={action === undefined ? undefined : () => call(action)} disabled={action === undefined}>{label}</button>
}

// read-only while `enabled` is falsy – readOnly, not disabled: the text stays selectable
// and the same <textarea> reconciles in place, so cursor and scroll survive
const TextEditorView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { remote, local, write } = useBound(d.slots.target)
  const text = (local as string | undefined) ?? (remote?.k === "string" ? remote.v : "")
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

const useNumber = (target: Desc | undefined) => {
  const { remote, local, write } = useBound(target)
  // the last value seen, while a newer one is read off the GPU
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
        type="range"
        min={0}
        max={1}
        step={0.01}
        value={value}
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
      type="checkbox"
      checked={value >= 0.5}
      onChange={(e) => write(e.target.checked ? 1 : 0)}
      className="w-5 h-5 accent-white cursor-pointer place-self-start"
    />
  )
}

// a number you drag sideways: `step` per 10 pixels
const ScrubberView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { step, decimals } = d.props as { step: number, decimals: number }
  const { value, write } = useNumber(d.slots.target)
  const factor = Math.pow(10, decimals)
  const handlePointerDown = (e: React.PointerEvent<HTMLSpanElement>) => {
    e.preventDefault()
    const startX = e.clientX, startValue = value
    const onMove = (me: PointerEvent) => {
      const raw = startValue + (me.clientX - startX) * step * 0.1
      write(Math.round(raw * factor) / factor)
    }
    const onUp = () => {
      window.removeEventListener("pointermove", onMove)
      window.removeEventListener("pointerup", onUp)
    }
    window.addEventListener("pointermove", onMove)
    window.addEventListener("pointerup", onUp)
  }
  return (
    <span style={{ color: `#${tokenColors.number}`, cursor: "ew-resize", userSelect: "none" }} onPointerDown={handlePointerDown}>
      {value.toLocaleString("en-US", { maximumFractionDigits: decimals }).replace(/,/g, "_")}
    </span>
  )
}

// Drag & drop works even where native file dialogs are unavailable (embedded webviews)
const ImageUploadView = ({ d }: { d: UIDesc }) => {
  const { write } = useBound(d.slots.target)
  const load = async (file: File | null | undefined) => {
    if (!file || !file.type.startsWith("image")) { return }
    write(await createImageBitmap(file))
  }
  return (
    <div
      className="text-sm text-neutral-400 hover:text-neutral-200 border border-dashed border-neutral-600 hover:border-neutral-400 rounded-xl p-3 text-center cursor-pointer select-none"
      // open the picker explicitly – label→hidden-input forwarding is flaky
      onClick={(e) => { e.currentTarget.querySelector("input")?.click() }}
      onDragOver={(e) => { e.preventDefault() }}
      onDrop={(e) => { e.preventDefault(); load(e.dataTransfer.files?.[0]) }}
    >
      drop an image here – or click to choose
      <input
        type="file"
        accept="image/*"
        className="hidden"
        onClick={(e) => { e.stopPropagation() }}
        onChange={(e) => load(e.target.files?.[0])}
      />
    </div>
  )
}

// Lists flatten into cells, and a live list re-renders its cells as it changes
const flattenCells = (ds: Desc[]): Desc[] => ds.flatMap((d) => {
  if (d.k === "list") { return flattenCells(d.items) }
  const now = d.k === "live" ? resolve(d) : undefined
  return now?.k === "list" ? flattenCells(now.items) : [d]
})

const GridView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { columns, rows } = d.props as { columns: string, rows: string }
  // Without a row template, rows hug their content (content-start) instead of sharing
  // the panel's spare height, and auto-rows-max keeps a tall page of cells from being
  // crushed to slivers when it overflows – the grid scrolls instead.
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

const LayersView = ({ d }: { d: UIDesc }) => (
  <div className="relative w-fit" data-layers="true">
    {d.children.map((child, i) => (
      // Overlays must not eat pointer events, or the topmost layer swallows every click;
      // interactive children (a Point2D's dot) re-enable them with pointer-events-auto
      <div key={i} className={i === 0 ? "" : "absolute inset-0 pointer-events-none"}>
        <View d={child} />
      </div>
    ))}
  </div>
)

type PointProps = { x0: number, y0: number, spanX: number, spanY: number, color: string }

const usePoint = (target: Desc | undefined) => {
  const { remote, local, write } = useBound(target)
  const last = useRef<number[]>([0, 0])
  const known = (local as number[] | undefined) ?? (() => { const p = payloadOf(remote); return p ? Array.from(p) : undefined })()
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
    write([
      x0 + (event.clientX - rect.left) / rect.width * spanX,
      y0 + (event.clientY - rect.top) / rect.height * spanY,
    ])
  }
  return (
    <div className="absolute inset-0 pointer-events-none touch-none select-none">
      <div
        data-point2d="true"
        className="absolute pointer-events-auto cursor-grab active:cursor-grabbing"
        style={{
          left: `${(px - x0) / spanX * 100}%`,
          top: `${(py - y0) / spanY * 100}%`,
          transform: "translate(-50%, -50%)",
          width: 32, height: 32, display: "grid", placeItems: "center",
        }}
        onPointerDown={(e) => {
          // Grab arbitration: the dot whose center is nearest the pointer wins, not the
          // topmost layer (ties go to the earlier layer)
          const layersRoot = e.currentTarget.closest("[data-layers]")
          const dots = layersRoot ? [...layersRoot.querySelectorAll("[data-point2d]")] : []
          const distanceTo = (el: Element) => {
            const r = el.getBoundingClientRect()
            return Math.hypot(e.clientX - (r.left + r.width / 2), e.clientY - (r.top + r.height / 2))
          }
          let nearest: Element = e.currentTarget
          let best = Infinity
          for (const el of dots) {
            const distance = distanceTo(el)
            if (distance < best) { best = distance; nearest = el }
          }
          if (nearest !== e.currentTarget) {
            e.stopPropagation()
            nearest.dispatchEvent(new PointerEvent("pointerdown", {
              bubbles: true, clientX: e.clientX, clientY: e.clientY,
              pointerId: e.pointerId, buttons: e.buttons, isPrimary: true,
            }))
            return
          }
          e.currentTarget.setPointerCapture(e.pointerId)
        }}
        onPointerMove={(e) => { if (e.buttons) { place(e) } }}
      >
        <div style={{ width: 12, height: 12, borderRadius: 999, background: color, border: "2px solid black", boxShadow: "0 0 6px rgba(0,0,0,0.8)" }} />
      </div>
    </div>
  )
}

// The path a point has taken: the last 500 places it was shown at
const TrailView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { x0, y0, spanX, spanY, color } = d.props as PointProps
  const { point: [px, py] } = usePoint(d.slots.target)
  const points = useRef<string[]>([])
  const here = `${(px - x0) / spanX * 100},${(py - y0) / spanY * 100}`
  if (points.current[points.current.length - 1] !== here) {
    points.current.push(here)
    if (points.current.length > 500) { points.current.shift() }
  }
  return (
    <svg className="absolute inset-0 w-full h-full pointer-events-none" viewBox="0 0 100 100" preserveAspectRatio="none">
      <polyline
        points={points.current.join(" ")}
        fill="none" stroke={color} strokeOpacity={0.75} strokeWidth={1.5}
        vectorEffect="non-scaling-stroke"
      />
    </svg>
  )
}

const PointPlotView = ({ d }: { d: UIDesc }) => {
  const { xs, ys } = d.props as { xs: number[], ys: number[] }
  return (
    <Plot
      data={[{
        x: xs,
        y: ys,
        type: "scatter",
        mode: "lines+markers",
        line: { color: "#666", width: 1 },
        marker: { color: ys, colorscale: "Viridis", size: 9, line: { color: "black", width: 1 } },
      }]}
      layout={{
        paper_bgcolor: "transparent",
        plot_bgcolor: "transparent",
        font: { color: "#D4D4D4" },
        xaxis: { gridcolor: "#444444", zeroline: false, automargin: true },
        yaxis: { gridcolor: "#444444", zeroline: false, automargin: true },
        margin: { pad: 6, t: 6, l: 0, r: 6, b: 0 },
      }}
      config={{ staticPlot: true, responsive: false }}
      style={{ width: "100%", height: "12em" }}
    />
  )
}

const CodeWidgetView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { remote, local, write } = useBound(d.slots.source)
  return <CodeView value={(local as string | undefined) ?? (remote?.k === "string" ? remote.v : "")} onChange={write} />
}

const CodeEditorWidgetView = ({ d }: { d: UIDesc }) => {
  useSignals()
  const { remote, local, write } = useBound(d.slots.source)
  return <CodeEditorView value={(local as string | undefined) ?? (remote?.k === "string" ? remote.v : "")} onChange={write} auto={d.props.auto === true} />
}

const PrintView = ({ d }: { d: UIDesc }) => {
  const child = d.children[0] ?? { k: "null" }
  return (
    <WithOrigin o={child.o}>
      <Panel className="overflow-scroll" testId="print-panel">
        <ErrorBoundary fallback={<div>Something went wrong</div>} resetKeys={[child]}>
          <View d={child} />
        </ErrorBoundary>
      </Panel>
    </WithOrigin>
  )
}

const PrettyPrintView = ({ d }: { d: UIDesc }) => <View d={d.children[0] ?? { k: "null" }} />

const WIDGETS: Record<string, (p: { d: UIDesc }) => JSX.Element> = {
  Text: TextView, Button: ButtonView, TextEditor: TextEditorView, Slider: SliderView, Checkbox: CheckboxView,
  Scrubber: ScrubberView, ImageUpload: ImageUploadView, Grid: GridView, Center: CenterView, Layers: LayersView,
  Point2D: Point2DView, Trail: TrailView, PointPlot: PointPlotView, Code: CodeWidgetView,
  CodeEditor: CodeEditorWidgetView, Print: PrintView, PrettyPrint: PrettyPrintView,
}

// a widget is keyed by its identity: a different widget at the same place never inherits
// another's mounted state (an editor, a plot)
const WidgetView = ({ d }: { d: UIDesc }) => {
  const Widget = WIDGETS[d.type]
  return Widget ? <Widget key={d.uid} d={d} /> : <div className="text-violet-500">{d.type}</div>
}

// Layout constants
const TREE = {
  fontSize: 14,
  nodePadding: { x: 8, y: 3 },
  gap: { row: 15, col: 24 },
  cornerRadius: 5,
}
const GLYPH = { width: TREE.fontSize * 0.6, height: TREE.fontSize * 1.2 }
const NODE_HEIGHT = GLYPH.height + 2 * TREE.nodePadding.y
const ROW_HEIGHT = NODE_HEIGHT + TREE.gap.row

// Simple types for layout
type GridNode = {
  label: string, row: number, col: number, width: number, origin: Where,
  children: GridNode[], parent: GridNode | null,
}
type GridEdge = { parentRow: number, parentCol: number, childRow: number, childCol: number }

// The `row,col` keys of the subtree under the hovered AST node – its nodes get
// brighter frames and its edges brighter strokes. Null when nothing is hovered.
const hoverSubtree = signal<Set<string> | null>(null)

// Reference to main editor for global shortcuts and as a highlight fallback
let mainEditorRef: { editor: editor.IStandaloneCodeEditor; monaco: Monaco } | null = null

// Every mounted CodeEditor (the main panel AND embedded cells – tour rooms,
// REPL). An origin names the document it indexes into, so a hover routes
// to the editor whose text IS that document instead of always painting the
// main editor. Disposed editors are pruned lazily on lookup – Editor has no
// unmount callback to hook.
const editorRegistry = new Set<editor.IStandaloneCodeEditor>()
const hoverCollections = new WeakMap<editor.IStandaloneCodeEditor, editor.IEditorDecorationsCollection>()
let hoverTarget: editor.IStandaloneCodeEditor | null = null

const editorForDocument = (document: number): editor.IStandaloneCodeEditor | null => {
  // an origin without a document is the main program's
  if (document === 0) { return mainEditorRef?.editor ?? null }
  for (const ed of editorRegistry) {
    const model = ed.getModel()
    if (!model) { editorRegistry.delete(ed); continue }
    if (documentHash(model.getValue()) === document) { return ed }
  }
  // the document is known but no editor shows it (e.g. a value from an
  // already-edited cell) – better no highlight than the right range in the
  // wrong text
  return null
}

const setHoverHighlight = (origin: Where | null) => {
  const target = origin ? editorForDocument(origin.d) : null
  if (hoverTarget && hoverTarget !== target) {
    hoverCollections.get(hoverTarget)?.set([])
  }
  hoverTarget = target
  if (!target || !origin) { return }

  let collection = hoverCollections.get(target)
  if (!collection) {
    collection = target.createDecorationsCollection()
    hoverCollections.set(target, collection)
  }

  collection.set([{
    range: {
      startLineNumber: origin.s[0],
      startColumn: origin.s[1],
      endLineNumber: origin.e[0],
      endColumn: origin.e[1],
    },
    options: {
      className: 'ast-hover-highlight',
      isWholeLine: false,
    }
  }])
}

function nodeWidth(label: string): number {
  return label.length * GLYPH.width + 2 * TREE.nodePadding.x
}

let treeInstance = 0

// a quoted program, drawn as one tree per statement – laid out once, re-tinted on hover
function TreeView({ roots }: { roots: SyntaxTree[] }): JSX.Element {
  return useMemo(() => drawTree(roots), [roots])
}

function drawTree(roots: SyntaxTree[]): JSX.Element {
  const { nodes, edges } = layoutGrid(roots)
  if (nodes.length === 0) return <svg />

  // Compute column widths (max node width per column)
  const maxCol = nodes.reduce((m, n) => Math.max(m, n.col), 0)
  const colWidth: number[] = Array(maxCol + 1).fill(0)
  for (const n of nodes) colWidth[n.col] = Math.max(colWidth[n.col]!, n.width)

  // Compute X positions right-to-left (col 1 = rightmost)
  const colX: number[] = Array(maxCol + 1).fill(0)
  const gapX: number[] = Array(maxCol + 1).fill(0)
  let x = 0
  for (let c = maxCol; c >= 1; c--) {
    colX[c] = x + colWidth[c]! / 2
    x += colWidth[c]!
    if (c > 1) {
      gapX[c - 1] = x + TREE.gap.col / 2
      x += TREE.gap.col
    }
  }
  const totalWidth = x

  // Compute Y positions
  const maxRow = nodes.reduce((m, n) => Math.max(m, n.row), 0)
  const rowY = (r: number) => (r - 0.5) * ROW_HEIGHT
  const totalHeight = maxRow * ROW_HEIGHT

  // Build node lookup by position
  const nodeAt = new Map(nodes.map(n => [`${n.row},${n.col}`, n]))
  // hoverSubtree is one global signal and grid coordinates repeat across
  // trees – prefix keys with this tree's identity, or hovering one graph
  // tints the same coordinates in every other graph on the page
  const treeId = ++treeInstance
  const keyOf = (n: GridNode) => `${treeId}:${n.row},${n.col}`
  const subtreeKeys = (n: GridNode): Set<string> => {
    const s = new Set<string>()
    const walk = (m: GridNode) => { s.add(keyOf(m)); for (const c of m.children) walk(c) }
    walk(n)
    return s
  }

  // Boundary nodes' rect strokes are CENTERED on the viewport edge (half the
  // stroke lives outside), and glyph labels can run a hair wider than the
  // 0.6em-per-char width estimate. A small bleed gutter plus visible
  // overflow lets the drawing breathe – the surrounding panel's padding
  // absorbs it, so edges stop getting chopped.
  const bleed = 2
  return (
    <svg className="ast-tree" style={{ width: totalWidth + 2 * bleed, height: totalHeight, overflow: "visible", shapeRendering: "optimizeSpeed" }} viewBox={`${-bleed} 0 ${totalWidth + 2 * bleed} ${totalHeight}`}>
      {/* one reactive region: re-renders on hover to re-tint frames/edges, but the
          layout above is computed only once per program */}
      {computed(() => {
        const active = hoverSubtree.value
        return (
          <>
            {edges.map(e => {
              const parent = nodeAt.get(`${e.parentRow},${e.parentCol}`)!
              const child = nodeAt.get(`${e.childRow},${e.childCol}`)!
              return (
                <TreeEdge
                  key={`${e.parentRow},${e.parentCol}-${e.childRow},${e.childCol}`}
                  x1={colX[e.parentCol]! - parent.width / 2}
                  y1={rowY(e.parentRow)}
                  x2={colX[e.childCol]! + child.width / 2}
                  y2={rowY(e.childRow)}
                  trunkX={gapX[e.parentCol]!}
                  active={active?.has(`${treeId}:${e.parentRow},${e.parentCol}`) ?? false}
                />
              )
            })}
            {nodes.map(n => (
              <TreeNode
                key={keyOf(n)}
                x={colX[n.col]!}
                y={rowY(n.row)}
                label={n.label}
                width={n.width}
                origin={n.origin}
                active={active?.has(keyOf(n)) ?? false}
                onEnter={() => { hoverSubtree.value = subtreeKeys(n) }}
              />
            ))}
          </>
        )
      }) as unknown as JSX.Element}
    </svg>
  )
}

function TreeEdge({ x1, y1, x2, y2, trunkX, active }: { x1: number, y1: number, x2: number, y2: number, trunkX: number, active: boolean }): JSX.Element {
  const r = TREE.cornerRadius
  const dy = y2 - y1
  const ady = Math.abs(dy)
  const stroke = active ? "stroke-neutral-300" : "stroke-neutral-600"

  // Straight horizontal line
  if (ady < 1) {
    return <path d={`M${x1} ${y1} L${x2} ${y2}`} fill="none" strokeWidth={1} className={stroke} />
  }

  // Small vertical distance - smooth S-curve
  if (ady < 2 * r) {
    const sr = ady / 2
    const sdy = dy > 0 ? sr : -sr
    return (
      <path
        d={`M${x1} ${y1} L${trunkX + sr} ${y1} Q${trunkX} ${y1}, ${trunkX} ${y1 + sdy} Q${trunkX} ${y2}, ${trunkX - sr} ${y2} L${x2} ${y2}`}
        fill="none" strokeWidth={1} className={stroke}
      />
    )
  }

  // Normal case - stepped path with rounded corners
  const sdy = dy > 0 ? r : -r
  return (
    <path
      d={`M${x1} ${y1} L${trunkX + r} ${y1} Q${trunkX} ${y1}, ${trunkX} ${y1 + sdy} L${trunkX} ${y2 - sdy} Q${trunkX} ${y2}, ${trunkX - r} ${y2} L${x2} ${y2}`}
      fill="none" strokeWidth={1} className={stroke}
    />
  )
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
        fill={getColorForSyntaxTreeNode(label)}
        style={{ fontSize: TREE.fontSize }}
        className="font-mono pointer-events-none"
      >
        {label}
      </text>
    </g>
  )
}

// Grid layout: assigns (row, col) positions to tree nodes
function layoutGrid(roots: SyntaxTree[]): { nodes: GridNode[], edges: GridEdge[] } {
  const nodes: GridNode[] = []
  const edges: GridEdge[] = []
  const used = new Set<string>()

  // A node sits on the same row as its first child, so the leftmost spine of
  // every subtree is horizontal. spineLength = how many columns it spans.
  function spineLength(node: SyntaxTree): number {
    const children = node.children
    return children.length === 0 ? 1 : 1 + spineLength(children[0]!)
  }

  // Anchor each subtree at the first row where its spine fits, then pack the
  // remaining children snugly below (childMin = previous child's row + 1). A node
  // shares a row with its first child, so the leftmost spine is horizontal.
  //
  // `used` reserves both node cells AND each parent's trunk – the vertical run in
  // the column gap, from the parent's row down to its last child's row. A node
  // may pack into any free row except one where another same-column parent's
  // trunk already passes; that reservation is what keeps unrelated trunks from
  // overlapping, while still letting non-conflicting subtrees interleave tightly.
  function layout(node: SyntaxTree, minRow: number, col: number, parent: GridNode | null): { node: GridNode, maxRow: number } {
    const spine = spineLength(node)
    // the spine follows first children, so every spine node but the last leaf
    // owns a trunk – track which spine positions to test against reserved trunks
    const spineHasTrunk: boolean[] = []
    for (let s: SyntaxTree = node, i = 0; i < spine; i++) {
      const kids = s.children
      spineHasTrunk.push(kids.length > 0)
      s = kids[0]!
    }
    const fitsAt = (r: number) => {
      for (let i = 0; i < spine; i++) {
        if (used.has(`${r},${col + i}`)) { return false }
        if (spineHasTrunk[i] && used.has(`g:${r},${col + i}`)) { return false }
      }
      return true
    }
    let row = minRow
    while (!fitsAt(row)) row++

    const label = node.label
    const self: GridNode = { label, row, col, width: nodeWidth(label), origin: node.o, children: [], parent }
    nodes.push(self)
    used.add(`${row},${col}`)

    const children = node.children
    let maxRow = row
    let childMin = row
    let lastChildRow = row
    for (const child of children) {
      const placed = layout(child, childMin, col + 1, self)
      self.children.push(placed.node)
      maxRow = Math.max(maxRow, placed.maxRow)
      lastChildRow = Math.max(lastChildRow, placed.node.row)
      childMin = placed.node.row + 1
    }
    // reserve this parent's trunk (its row → its last child's row) in the gap
    if (children.length > 0) {
      for (let r = row; r <= lastChildRow; r++) used.add(`g:${r},${col}`)
    }
    return { node: self, maxRow }
  }

  let row = 1
  for (const stmt of roots) {
    row = layout(stmt, row, 1, null).maxRow + 1
  }

  // Pull floating spines down toward their subtree. A node whose first child is
  // small but whose other children were pushed far below (by an unrelated
  // subtree's reserved trunk) dangles high above the bulk of its own subtree –
  // e.g. `⊢ : { … }` leaves `⊢ :` stranded rows above its lambda. Slide each such
  // spine down into the free rows just above its nearest branch child. Only ever
  // moves nodes down into empty cells, so it can't create overlaps or crossings.
  const spineRoots = nodes.filter(n => n.parent === null || n.parent.children[0] !== n)
  spineRoots.sort((a, b) => a.row - b.row)
  for (const root of spineRoots) {
    const spine: GridNode[] = []
    for (let s: GridNode | undefined = root; s; s = s.children[0]) spine.push(s)
    let nearestBranch = Infinity
    for (const s of spine) {
      for (let j = 1; j < s.children.length; j++) nearestBranch = Math.min(nearestBranch, s.children[j]!.row)
    }
    if (!isFinite(nearestBranch)) continue      // no branch child to hug
    const target = nearestBranch - 1
    if (target <= root.row) continue
    let newRow = root.row
    for (let r = target; r > root.row; r--) {
      if (spine.every(s => !used.has(`${r},${s.col}`))) { newRow = r; break }
    }
    if (newRow > root.row) {
      for (const s of spine) {
        used.delete(`${s.row},${s.col}`)
        s.row = newRow
        used.add(`${newRow},${s.col}`)
      }
    }
  }

  // Build edges from the final node positions (rows may have shifted above)
  for (const n of nodes) {
    for (const c of n.children) {
      edges.push({ parentRow: n.row, parentCol: n.col, childRow: c.row, childCol: c.col })
    }
  }

  return { nodes, edges }
}

// MARK: Values
// How a value's description draws: text, lists, records, errors, functions, trees,
// tensors – and live values, each its own reactive island.

const frameStyle = "rounded-xl border border-neutral-800 hover:border-neutral-700 p-2"

function Panel({ children, className, testId }: { children: JSX.Element, className?: string, testId?: string }) {
  return (
    <div className={`${frameStyle} ${className ?? ""}`} data-testid={testId}>
      {children}
    </div>
  )
}

// hovering a value highlights the source it came from
function WithOrigin({ o, children }: { o?: Where, children: JSX.Element }): JSX.Element {
  if (!o) { return children }
  return (
    <span
      style={{ display: "contents" }}
      onPointerOver={(e) => { e.stopPropagation(); setHoverHighlight(o) }}
      onPointerOut={() => setHoverHighlight(null)}
    >
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
            <div className={`${frameStyle} !border-0 grid hover:bg-neutral-900 mix-blend-screen`}><View d={item} /></div>
          </WithOrigin>
        ))}
      </div>
    )
    case "record": return (
      <div>
        {d.entries.map(([key, value]) => (
          <div key={key}><span className="font-bold">{key}:</span> <View d={value} /></div>
        ))}
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

// a live value redraws on its own when it changes – nothing around it does
function LiveView({ id }: { id: number }): JSX.Element {
  useSignals()
  const now = mirrors.get(id)?.desc.value
  return now ? <View d={now} /> : <span className="opacity-50">…</span>
}

// A cell of a grid or a center: a widget draws bare, a value inside a framed panel – the
// frame marks the cell, and the panel's whole extent carries the hover highlight
function Cell({ d }: { d: Desc }): JSX.Element {
  useSignals()
  const now = resolve(d) ?? d
  if (now.k === "ui") { return <WithOrigin o={now.o}><WidgetView d={now} /></WithOrigin> }
  // a tree grows with its drawing: no scroll container, which would clip it
  if (now.k === "tree") { return <WithOrigin o={now.o}><Panel><TreeView roots={now.roots} /></Panel></WithOrigin> }
  return (
    <WithOrigin o={now.o}>
      <Panel className="overflow-scroll" testId="print-panel">
        <ErrorBoundary fallback={<div>Something went wrong</div>} resetKeys={[now]}>
          <View d={now} />
        </ErrorBoundary>
      </Panel>
    </WithOrigin>
  )
}

// a built-in shows its doc card; a Fluent lambda its source
function FunctionView({ d }: { d: Extract<Desc, { k: "function" }> }): JSX.Element {
  const source = d.source !== undefined ? <CodeView value={d.source} /> : null
  if (d.doc) { return <><Markdown text={docCard(d.doc)} />{source}</> }
  if (source) { return source }
  return <Markdown text={`\`${d.name}\` — a built-in without a doc card yet`} />
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
// A tensor draws by its shape (tensorView): a number, bars, a heatmap, an image. Its
// values arrive with its description or right after it; until they do, the view keeps
// showing the last ones it had.

function TensorPlot({ d }: { d: TensorDesc }): JSX.Element {
  useSignals()
  const view = tensorView(d.shape)
  const fresh = payloads.get(d.id)?.value
  const last = useRef<{ view: TensorView, shape: number[], payload: Payload } | null>(null)
  if (typeof fresh === "string") { return <ErrorView d={{ k: "error", chain: [{ message: fresh }] }} /> }
  if (fresh) { last.current = { view, shape: d.shape, payload: fresh } }
  const shown = last.current?.view === view ? last.current : null

  if (view === "shape") {
    return (
      <div>
        <span className="font-bold">Tensor:</span> {d.shape.join(", ")}<br />
        <span className="text-neutral-500">rank {d.shape.length} – reshape or slice it to plot</span>
      </div>
    )
  }
  if (view === "number") { return <ScalarView value={shown ? shown.payload[0] : undefined} /> }
  if (view === "bars") { return <BarPlot values={shown ? Array.from(shown.payload) : null} /> }
  if (view === "heatPlot") {
    if (!shown) { return <div style={{ width: "100%", height: "12em" }} /> }
    const [rows = 1, cols = 1] = shown.shape
    const z = Array.from({ length: rows }, (_, i) => Array.from(shown.payload.subarray(i * cols, (i + 1) * cols)))
    return <HeatPlot z={z} />
  }
  const [height = 1, width = 1] = (shown ?? { shape: d.shape }).shape
  // small heatmaps (a 27×32 embedding) upscale by an integer factor to ~256px, so the
  // cells stay crisp; images and large grids show at their own size
  const displayWidth = view === "heatImage" && width < 256 ? width * Math.ceil(256 / width) : width
  return <PixelCanvas rgba={shown?.payload as Uint8ClampedArray | undefined} width={width} height={height} displayWidth={displayWidth} />
}

const formatNumber = (value: number, digits: number) =>
  value.toLocaleString("en-US", { minimumFractionDigits: 0, maximumFractionDigits: digits, useGrouping: true }).replace(/,/g, "_")

const ScalarView = ({ value }: { value: number | undefined }) => {
  if (value === undefined) { return <span style={{ color: `#${tokenColors.number}`, opacity: 0.5 }}>…</span> }
  return <span style={{ color: `#${tokenColors.number}` }}>{formatNumber(value, 6)}</span>
}

const BarPlot = ({ values }: { values: number[] | null }) => {
  if (values === null) { return <div style={{ width: "100%", height: "12em" }} /> }
  const annotations: Partial<Annotations>[] = values.length < 20
    ? values.map((value, i) => ({
      xref: "x1", yref: "y1", yanchor: "bottom", x: i, y: value,
      text: formatNumber(value, 2),
      font: { color: "#D4D4D4", shadow: "0px 0px 1px black" },
      showarrow: false,
    }) as unknown as Partial<Annotations>)
    : []
  return (
    <Plot
      data={[{ y: values, type: "bar", marker: { color: values, colorscale: "Viridis" } }]}
      layout={{
        paper_bgcolor: "transparent",
        plot_bgcolor: "transparent",
        font: { color: "#D4D4D4" },
        annotations,
        xaxis: { automargin: true },
        yaxis: { gridcolor: "#444444", automargin: true },
        margin: { pad: 10, t: 0, l: 0, r: 0, b: 0 },
      }}
      // responsive plots flash in a random position when they update
      config={{ staticPlot: true, responsive: false }}
      style={{ width: "100%", height: "10em" }}
    />
  )
}

const HeatPlot = ({ z }: { z: number[][] }) => {
  const annotations: Partial<Annotations>[] = z.length * (z[0]?.length ?? 0) < 20
    ? z.flatMap((row, i) => row.map((value, j) => ({
      xref: "x1", yref: "y1", x: j, y: i,
      text: value.toFixed(2),
      font: { color: "#D4D4D4", shadow: "0px 0px 1px black" },
      showarrow: false,
    }) as unknown as Partial<Annotations>))
    : []
  return (
    <Plot
      data={[{ z, type: "heatmap", colorscale: "Viridis" }]}
      layout={{
        margin: { pad: 10, t: 0, l: 0, r: 0, b: 0 },
        paper_bgcolor: "transparent",
        plot_bgcolor: "transparent",
        font: { color: "#D4D4D4" },
        annotations,
        xaxis: { automargin: true, side: "top" },
        yaxis: { gridcolor: "#444444", automargin: true, autorange: "reversed" },
      }}
      config={{ staticPlot: true, autosizable: true, responsive: true }}
      // height: 100% of an auto-sized parent is circular and sometimes resolves to 0 –
      // the min-height floor breaks the cycle
      style={{ width: "100%", height: "100%", minHeight: "16em" }}
    />
  )
}

// RGBA pixels, colored in the worker, drawn as they arrive
const PixelCanvas = ({ rgba, width, height, displayWidth }: { rgba?: Uint8ClampedArray, width: number, height: number, displayWidth: number }) => {
  const ref = useRef<HTMLCanvasElement>(null)
  useEffect(() => {
    const canvas = ref.current
    if (!canvas || !rgba || rgba.length !== width * height * 4) { return }
    // resizing clears the canvas: only when the size changes
    if (canvas.width !== width || canvas.height !== height) {
      canvas.width = width
      canvas.height = height
    }
    canvas.getContext("2d")?.putImageData(new ImageData(rgba as Uint8ClampedArray<ArrayBuffer>, width, height), 0, 0)
  }, [rgba, width, height])
  return (
    <canvas
      ref={ref}
      style={{ width: "100%", maxWidth: `${displayWidth}px`, aspectRatio: `${width / height}`, imageRendering: "pixelated" }}
    />
  )
}

// the program's value – the playground's output panel
function ResultPanel(): JSX.Element {
  useSignals()
  const d = result.value
  return (
    <WithOrigin o={d?.o}>
      <Panel className="overflow-scroll" testId="print-panel">
        <ErrorBoundary fallback={<div>Something went wrong</div>} resetKeys={[d]}>
          {d ? <View d={d} /> : <span className="opacity-50">…</span>}
        </ErrorBoundary>
      </Panel>
    </WithOrigin>
  )
}

// MARK: Examples
import { EXAMPLES } from "./examples"

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

// MARK: Generated Code

const generationSystemPrompt = () => `

You are an expert Fluent code generator. Follow these rules when generating Fluent code:

1. You generate Fluent code. Output ONLY valid code, no explanations.
2. Make sure the generated code is syntactically correct.
3. Use built-in functions and environment variables where appropriate.
4. Follow best practices for Fluent code style and structure.
5. Refer to the examples below for guidance.
6. Ensure interactive/reactive elements (Slider, Button, etc.) are part of the program's return value, i.e. inside the final parentheses.
7. Keep the code concise and efficient.
8. If you want to add label to a Slider, use Grid to compose it with a Text element – \`Grid(2)(Text("Label"), Slider(x))\`
9. Slider can use only values between 0 and 1 for its range. To map to other ranges, use arithmetic operations.

Here is some documentation for Fluent:

${Documentation}

\`\`\`fluent
${
  Object.entries(EXAMPLES).map(example => dedent`
    ; ${example[0]}
    ${example[1].trim()}
  `).join("\n\n")
}
\`\`\`

You can use the following built-in functions and environment variables:
${[...environmentNames.peek().keys()].join(", ")}

## Operator Definitions (Prelude)
The following operators are defined as aliases to canonical functions:
\`\`\`fluent
${PRELUDE.trim()}
\`\`\`
`

const GENERATION_COMMENT_REGEX = /;;(.+?);;/g

const ANTHROPIC_API_KEY_STORAGE = "fluent-anthropic-api-key"
const getAnthropicApiKey = () => localStorage.getItem(ANTHROPIC_API_KEY_STORAGE) ?? ""
const setAnthropicApiKey = (key: string) => localStorage.setItem(ANTHROPIC_API_KEY_STORAGE, key)

let pendingGenerationRequests = new Set<string>()

async function generateFluentCode(instruction: string, context: string, apiKey: string): Promise<string> {

  const response = await fetch('https://api.anthropic.com/v1/messages', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'x-api-key': apiKey,
      'anthropic-version': '2023-06-01',
      'anthropic-dangerous-direct-browser-access': 'true',
    },
    body: JSON.stringify({
      model: 'claude-sonnet-5',
      max_tokens: 16000,
      thinking: {
        type: 'enabled',
        budget_tokens: 10000
      },
      system: generationSystemPrompt(),
      messages: [{
        role: 'user',
        content: `Current code context:\n\`\`\`\n${context}\n\`\`\`\n\nGenerate Fluent code for: ${instruction}`
      }]
    })
  })


  const data = await response.json()

  if (data.error) {
    throw new Error(data.error.message)
  }

  // With thinking enabled, find the text block (not thinking block)
  const textBlock = data.content.find((block: any) => block.type === 'text')
  if (!textBlock) {
    throw new Error('No text block in response')
  }

  let result = textBlock.text.trim()

  // Strip markdown code fences if present
  result = result.replace(/^```(?:fluent)?\n?/gm, '').replace(/```$/gm, '').trim()


  return result
}

function processGenerationComments(
  code: string,
  apiKey: string,
  onUpdate: (newCode: string) => void
): string {
  const matches = [...code.matchAll(GENERATION_COMMENT_REGEX)]

  for (const match of matches) {
    const fullMatch = match[0]
    const instruction = match[1]!.trim()

    // Skip if already processing this exact comment
    if (pendingGenerationRequests.has(fullMatch)) {
      continue
    }
    pendingGenerationRequests.add(fullMatch)

    // Fire off async generation
    generateFluentCode(instruction, code, apiKey)
      .then(generatedCode => {
        pendingGenerationRequests.delete(fullMatch)
        const newCode = code.replace(fullMatch, generatedCode)
        onUpdate(newCode)
      })
      .catch(err => {
        pendingGenerationRequests.delete(fullMatch)
        console.error('[Generation] Failed:', err)
        onUpdate(code.replace(fullMatch, `; ERROR: ${err.message}`))
      })
  }

  return code
}


// MARK: Editor

const getEditorOptions: (type: "editable" | "readonly") => editor.IStandaloneEditorConstructionOptions = (type) => ({
  minimap: { enabled: false },
  wordWrap: "on",
  fontSize: 14,
  // BQN386 serves only the symbol blocks (unicode-range in index.css) – both
  // hook glyphs ⊸/⟜ come from one face instead of two mismatched fallbacks
  fontFamily: '"BQN386", Menlo, Monaco, "Courier New", monospace',
  fixedOverflowWidgets: true,
  scrollBeyondLastLine: false,
  bracketPairColorization: {
    independentColorPoolPerBracketType: true,
    enabled: true,
  },
  unicodeHighlight: {
    ambiguousCharacters: false
  },
  padding: { top: 8, bottom: 8 }, // Corresponds to p-2
  automaticLayout: true,
  scrollbar: {
    alwaysConsumeMouseWheel: false
  },
  ...(type === "readonly" && ({
    readOnly: true, // Make the editor read-only
    contextmenu: false,
    lineNumbers: "off",
    stickyScroll: { enabled: false },
    overviewRulerBorder: false,
    minimap: { enabled: false },
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
    fixedOverflowWidgets: true,
    glyphMargin: false,
    foldingStrategy: "indentation",
    foldingHighlight: false,
    scrollbar: {
      vertical: "hidden",
      horizontal: "hidden",
      useShadows: true,
    }
  }))
})

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
        options={getEditorOptions("readonly")}
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

  // each editor validates its own model (REPL cells stay independent)
  const editorRef = useRef<{ editor: editor.IStandaloneCodeEditor; monaco: Monaco } | null>(null)
  const validateCode = (code: string) => {
    const mounted = editorRef.current
    const model = mounted?.editor.getModel()
    if (!mounted || !model) { return }
    const markers = getParseErrors(code).map(error => ({
      startLineNumber: error.start.line,
      startColumn: error.start.column,
      endLineNumber: error.end.line,
      endColumn: error.end.column,
      message: error.message,
      severity: mounted.monaco.MarkerSeverity.Error,
    }))
    mounted.monaco.editor.setModelMarkers(model, "fluent-syntax", markers)
  }

  const handleEditorMount: OnMount = (editor, monaco) => {
    editorOnMount(editor, monaco)
    editorRef.current = { editor, monaco }
    editorRegistry.add(editor)
    // Global shortcuts (⌘O gallery, ⌘S share) target the app's MAIN editor – the one
    // that fills its panel. Auto-grown editors are embedded cells that mount first and
    // unmount on navigation; one of them claiming this would leave the shortcuts wired
    // to a disposed editor.
    if (!auto && !mainEditorRef) { mainEditorRef = { editor, monaco } }

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

    const model = editor.getModel()
    if (model) { validateCode(model.getValue()) }
  }

  return (
    <Editor
      beforeMount={editorBeforeMount}
      onMount={handleEditorMount}
      height={height}
      defaultLanguage="fluent"
      className={`${frameStyle} !p-0 overflow-hidden`}
      theme="fluentTheme"
      value={value}
      onChange={(next) => {
        if (next === undefined) { return }
        changed.current(next)
        validateCode(next)
        // generation comments (;; … ;;) are answered by the model, in place
        const apiKey = getAnthropicApiKey()
        if (apiKey && GENERATION_COMMENT_REGEX.test(next)) {
          GENERATION_COMMENT_REGEX.lastIndex = 0
          processGenerationComments(next, apiKey, (generated) => {
            changed.current(generated)
            validateCode(generated)
          })
        }
      }}
      options={getEditorOptions("editable")}
    />
  )
}

let EditorInstance: Monaco["editor"] | null = null

async function getEditor() {
  while (EditorInstance === null) {
    await new Promise((resolve) => setTimeout(resolve, 0));
  }
  return EditorInstance;
}

// The token under the cursor: a letter-identifier (Greek included) via Monaco's
// word detection, or an operator run (..<, ⌈, ⊗, ∇, ←) expanded by hand – the
// language's word pattern only covers letters, so glyphs need this.
const RESERVED_HOVER = new Set(["|", ",", "{", "}", "(", ")", "[", "]", ";", '"', "`"])
const isOperatorChar = (ch: string | undefined): ch is string =>
  !!ch && !/\s/.test(ch) && !/[\p{L}\p{N}]/u.test(ch) && !RESERVED_HOVER.has(ch)

const isIdentChar = (ch: string | undefined): ch is string => !!ch && /[\p{L}\p{N}\-]/u.test(ch)

function fluentTokenAt(model: editor.ITextModel, position: monaco.Position): string | null {
  const line = model.getLineContent(position.lineNumber)
  const classify = (ch: string | undefined) =>
    isIdentChar(ch) ? isIdentChar : isOperatorChar(ch) ? isOperatorChar : null
  let i = position.column - 1                        // char at/after the cursor (0-indexed)
  let same = classify(line[i])
  if (!same) { i -= 1; same = classify(line[i]) }    // cursor may sit just past the token
  if (!same) { return null }
  let start = i, end = i
  while (start > 0 && same(line[start - 1])) { start-- }
  while (end < line.length - 1 && same(line[end + 1])) { end++ }
  return line.slice(start, end + 1)
}

// Docs live on the built-ins' metadata (attached inline in the prelude via doc()); the
// worker lists every name in the environment with its card when it starts.
const metaFor = (token: string | null): Doc | null =>
  (token ? environmentNames.peek().get(token)?.doc ?? null : null)

// A hover / completion card: signature, one-liner, worked example.
const docCard = (m: Doc): string =>
  [m.signature && "```fluent\n" + m.signature + "\n```", m.doc, m.example && "```fluent\n" + m.example + "\n```"]
    .filter(Boolean).join("\n\n")

// completion's details pane already shows the signature as its header (the
// item's `detail`) – repeating it in the body reads as a glitch
const docCardBody = (m: Doc): string =>
  [m.doc, m.example && "```fluent\n" + m.example + "\n```"]
    .filter(Boolean).join("\n\n")

// Monaco-global setup below runs once per page, NOT once per editor:
// beforeMount fires for every <Editor>, and register*Provider calls
// ACCUMULATE – each extra registration stacks one more identical doc card
// onto every hover. The disposables live on globalThis so a hot-reloaded
// module replaces its predecessor's providers instead of stacking on them.
const registeredProviders: { dispose(): void }[] =
  ((globalThis as Record<string, unknown>).__fluentMonacoProviders ??= []) as { dispose(): void }[]
let monacoConfigured = false

const editorBeforeMount: BeforeMount = (monaco) => {
  EditorInstance = monaco.editor
  if (monacoConfigured) { return }
  monacoConfigured = true
  for (const p of registeredProviders.splice(0)) {
    try { p.dispose() } catch { /* already gone */ }
  }

  monaco.languages.register({ id: "fluent" });

  registeredProviders.push(monaco.languages.registerHoverProvider("fluent", {
    provideHover(model, position) {
      const meta = metaFor(fluentTokenAt(model, position))
      return meta ? { contents: [{ value: docCard(meta) }] } : null
    },
  }));

  monaco.languages.setMonarchTokensProvider("fluent", {
    defaultToken: "",
    unicode: true,
    tokenizer: {
      root: [
        [/;.*$/, { token: "comment" }],
        [numberRegexp, { token: "number" }],
        // https://github.com/microsoft/monaco-editor/issues/5026
        [/"/, { token: "string", next: "@string", /*nextEmbedded: 'markdown'*/ }],
        // [/-/, "operator"],
        [identifierRegexp, { token: "identifier" }],
        [delimiterRegexp, { token: "delimiter" }],
        [operatorRegexp, { token: "operator" }],
      ],
      string: [
        [/[^"]+/, { token: "string" }],
        [/"/, { /* token: '@rematch',*/ token: "string", next: '@pop', /*nextEmbedded: '@pop'*/ }],

      ],
    },
  });

  monaco.languages.setLanguageConfiguration("fluent", {
    brackets: [
      ["{", "}"],
      ["[", "]"],
      ["(", ")"],
    ],
    autoClosingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")" },
      { open: '"', close: '"' },
    ],
    surroundingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")", },
      { open: '"', close: '"' },
    ],
    indentationRules: {
      increaseIndentPattern: /^\s*[\(\{\[]/,
      decreaseIndentPattern: /^\s*[\)\}\]]/,
    },
    // TODO: unify this with RESERVED_SYMBOLS
    // Word pattern for identifiers (letter-based symbols)
    wordPattern: /(?:\p{L})[\p{L}\p{N}\-]*/gu,
    comments: {
      lineComment: ";",
    },
  });

  monaco.editor.defineTheme("fluentTheme", {
    base: "vs-dark",
    inherit: true,
    rules: COLORS,
    colors: {
      "editor.foreground": "#" + (COLORS.find(rule => rule.token === "string")?.foreground ?? "FFFFFF"),
      "editorSuggestWidget.foreground": "#D8D8D8",
      "editorSuggestWidget.background": "#1C1C1C",
      "editor.background": "#1C1C1C",
      "focusBorder": "#FF000000",
    },
  });

  monaco.editor.defineTheme("fluentThemeReadOnly", {
    base: "vs-dark",
    inherit: true,
    rules: COLORS,
    colors: {
      "editor.foreground": "#" + (COLORS.find(rule => rule.token === "string")?.foreground ?? "FFFFFF"),
      "editorSuggestWidget.foreground": "#D8D8D8",
      "editorSuggestWidget.background": "#1C1C1C",
      "editor.background": "#1C1C1C00",
      "focusBorder": "#FF000000",
    },
  });

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

  monaco.editor.addKeybindingRule({
    keybinding: monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyF,
    command: "editor.action.formatDocument",
  });

  monaco.editor.addKeybindingRule({
    keybinding: monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyK,
    command: 'editor.action.quickCommand',
    when: 'editorTextFocus'
  });

  monaco.editor.addKeybindingRule({
    keybinding: monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyP,
    command: 'editor.action.quickCommand',
    when: 'editorTextFocus'
  });

  monaco.editor.addKeybindingRule({
    keybinding: monaco.KeyCode.F1,
    command: '-editor.action.quickCommand',
    when: 'editorTextFocus'
  });

  // [symbol, name, detail, filter terms] – rendered into completion items below
  const SYMBOL_TABLE: [string, string, string, string][] = [
    // Greek lowercase
    ["α", "alpha", "Greek lowercase alpha", "alpha a"],
    ["β", "beta", "Greek lowercase beta", "beta b"],
    ["γ", "gamma", "Greek lowercase gamma", "gamma g"],
    ["δ", "delta", "Greek lowercase delta", "delta d"],
    ["ε", "epsilon", "Greek lowercase epsilon", "epsilon e"],
    ["ζ", "zeta", "Greek lowercase zeta", "zeta z"],
    ["η", "eta", "Greek lowercase eta", "eta"],
    ["θ", "theta", "Greek lowercase theta", "theta"],
    ["ι", "iota", "Greek lowercase iota", "iota i"],
    ["κ", "kappa", "Greek lowercase kappa", "kappa k"],
    ["λ", "lambda", "Greek lowercase lambda", "lambda l"],
    ["μ", "mu", "Greek lowercase mu", "mu m"],
    ["ν", "nu", "Greek lowercase nu", "nu n"],
    ["ξ", "xi", "Greek lowercase xi", "xi x"],
    ["π", "pi", "Greek lowercase pi", "pi p"],
    ["ρ", "rho", "Greek lowercase rho", "rho r"],
    ["σ", "sigma", "Greek lowercase sigma", "sigma s"],
    ["τ", "tau", "Greek lowercase tau", "tau t"],
    ["υ", "upsilon", "Greek lowercase upsilon", "upsilon u"],
    ["φ", "phi", "Greek lowercase phi", "phi f"],
    ["χ", "chi", "Greek lowercase chi", "chi c"],
    ["ψ", "psi", "Greek lowercase psi", "psi"],
    ["ω", "omega", "Greek lowercase omega", "omega o"],
    // Greek uppercase
    ["Γ", "Gamma", "Greek uppercase Gamma", "Gamma G"],
    ["Δ", "Delta", "Greek uppercase Delta", "Delta D"],
    ["Θ", "Theta", "Greek uppercase Theta", "Theta"],
    ["Λ", "Lambda", "Greek uppercase Lambda", "Lambda L"],
    ["Ξ", "Xi", "Greek uppercase Xi", "Xi X"],
    ["Π", "Pi", "Greek uppercase Pi / Product", "Pi P product"],
    ["Σ", "Sigma", "Greek uppercase Sigma / Sum", "Sigma S sum summation"],
    ["Φ", "Phi", "Greek uppercase Phi", "Phi F"],
    ["Ψ", "Psi", "Greek uppercase Psi", "Psi"],
    ["Ω", "Omega", "Greek uppercase Omega", "Omega O"],
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
    ["⍢", "under", "Under – do it in g's world", "under dual conjugate"],
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
  const symbolSuggestions = SYMBOL_TABLE.map(([symbol, name, detail, filterText]) => ({
    label: `${symbol} ${name}`,
    insertText: symbol,
    detail,
    filterText,
  }));
  registeredProviders.push(monaco.languages.registerCompletionItemProvider('fluent', {
    provideCompletionItems: (model, position) => {
      const word = model.getWordUntilPosition(position);

      const range = {
        startLineNumber: position.lineNumber,
        endLineNumber: position.lineNumber,
        startColumn: word.startColumn,
        endColumn: word.endColumn,
      };

      const symbolSuggestionItems = symbolSuggestions.map(item => ({
        label: item.label,
        kind: monaco.languages.CompletionItemKind.Text,
        insertText: item.insertText,
        detail: item.detail,
        range,
        filterText: item.filterText,
      }));

      // Every name in scope – the canonical binding (SignalOnce) and its
      // prelude alias (once) both list. The doc lives on the value, so aliases
      // share it: both cards read the same because they resolve to one object.
      return ({
        suggestions: [
          ...symbolSuggestionItems,
          ...[...environmentNames.peek()].map(([key, { detail, doc }]) => ({
            label: key,
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: key,
            documentation: doc ? { value: docCardBody(doc) } : `Function: ${key}`,
            detail,
            range,
            filterText: key,
          })),
        ]
      })
    }
  }));
}

const editorOnMount: OnMount = (editor, monaco) => {
  // Create command to access quick input service
  const quickInputCommand = editor.addCommand(0, (accessor, func) => {
    const quickInputService = accessor.get(IQuickInputService)
    func(quickInputService)
  })

  editor.addAction({
    id: "fluent-load-example",
    label: "Load example",
    contextMenuGroupId: "navigation",
    contextMenuOrder: 1.5,
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyO],

    run: (editor) => {
      const exampleNames = Object.keys(EXAMPLES) as (keyof typeof EXAMPLES)[]

      // Get first comment line as description
      const getDescription = (code: string) => {
        const firstLine = code.trim().split('\n')[0] ?? ''
        return firstLine.startsWith(';') ? firstLine.replace(/^;\s*/, '') : ''
      }

      const items = exampleNames.map(name => ({
        label: name,
        description: getDescription(EXAMPLES[name])
      }))

      editor.trigger("", quickInputCommand!, (quickInput: any) => {
        quickInput.pick(items, {
          placeHolder: 'Select an example to load'
        }).then((selected: any) => {
          if (selected) {
            editor.getModel()?.setValue(getExample(selected.label))
            // a pristine example is referable by name – the URL says so;
            // ⌘S after editing switches back to a ?code= URL
            const url = new URL(window.location.href)
            url.searchParams.delete("code")
            url.searchParams.set("example", selected.label)
            window.history.pushState({}, "", url.toString())
          }
        })
      })
    },
  });

  editor.addAction({
    id: "fluent-save-example",
    label: "Save example",
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS],
      
    run: function (editor) {
      const code = editor.getModel()?.getValue() ?? "";
      const encodedCode = StringSerialize(code);
      const url = new URL(window.location.href);
      url.searchParams.set("code", encodedCode);
      // saved code is its own document – it no longer speaks for the example
      url.searchParams.delete("example");
      window.history.pushState({}, "", url.toString());
    },
  });

  editor.addAction({
    id: "fluent-set-api-key",
    label: "Set Anthropic API Key",
    contextMenuGroupId: "navigation",
    contextMenuOrder: 1.6,
    run: (editor) => {
      editor.trigger("", quickInputCommand!, (quickInput: any) => {
        quickInput.input({
          placeHolder: 'Enter your Anthropic API key (sk-ant-...)',
          value: getAnthropicApiKey(),
          password: true,
        }).then((value: string | undefined) => {
          if (value !== undefined) {
            setAnthropicApiKey(value)
          }
        })
      })
    },
  });

};

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

// The one window error the spec itself calls ignorable: Monaco's automaticLayout inside a same-frame-resized container (an
// auto-growing editor cell) reports "ResizeObserver loop completed with
// undelivered notifications", and the dev overlay would block the page for it.
window.addEventListener("error", (e) => {
  if (String(e.message).includes("ResizeObserver loop")) { e.stopImmediatePropagation(); e.preventDefault() }
}, true)

export function Playground() {

  useSignals()

  useEffect(() => {
    const onKeyDown = (e: KeyboardEvent) => {
      const shortcut = (e.metaKey || e.ctrlKey) && !e.altKey ? GLOBAL_SHORTCUTS[e.code] : undefined
      if (!shortcut || !mainEditorRef || (e.shiftKey && !shortcut.withShift)) { return }
      e.preventDefault()
      e.stopPropagation() // Monaco would fire the same action again
      mainEditorRef.editor.focus()
      mainEditorRef.editor.getAction(shortcut.action)?.run()
    }
    window.addEventListener("keydown", onKeyDown, true)
    return () => window.removeEventListener("keydown", onKeyDown, true)
  }, [])

  useEffect(() => {
    // back/forward navigation restores the code from the URL. (popstate is the
    // only history event the browser fires – programmatic pushState/replaceState,
    // like Ctrl+S, emit nothing, which is what we want: the editor already
    // holds that code.)
    const handleUrlChange = () => {
      const params = new URLSearchParams(window.location.search);
      const codeFromUrl = StringDeserialize(params.get("code") ?? "");
      const exampleName = params.get("example");
      if (codeFromUrl !== "") {
        code.value = codeFromUrl
      } else if (exampleName !== null) {
        code.value = exampleSource(exampleName)
      }
    };
    window.addEventListener("popstate", handleUrlChange);
    return () => {
      window.removeEventListener("popstate", handleUrlChange);
    };
  }, [])

  const urlParams = new URLSearchParams(window.location.search);
  const codeFromUrl = StringDeserialize(urlParams.get("code") ?? "");
  const exampleFromUrl = urlParams.get("example");
  const code = useSignal<string>(
    codeFromUrl !== "" ? codeFromUrl
      : exampleFromUrl !== null ? exampleSource(exampleFromUrl)
        : "1 + 1"
  )
  // every edit goes to the worker; it runs the latest
  const source = code.value
  useEffect(() => { toWorker({ t: "code", source }) }, [source])

  return (
    <div className="absolute inset-0 z-50 p-2 bg-neutral-900 grid text-white">
      <div className="grid gap-2 overflow-scroll h-full" style={{ gridTemplateColumns: "repeat(2, minmax(auto, 1fr))", gridTemplateRows: "repeat(1, minmax(auto, 1fr))" }}>
        <div className="contents"><ResultPanel /></div>
        <div className="contents"><CodeEditorView value={source} onChange={(next) => { code.value = next }} auto={false} /></div>
      </div>
    </div>
  );
}

// Module evaluation finishes after the backends initialize, which can be
// later than DOMContentLoaded – mount immediately when the DOM is ready.
const mountPlayground = () => {
  const root = document.getElementById("root")

  if (!root) {
    throw new Error("Root element not found")
  }

  createRoot(root).render(<Playground />)
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", mountPlayground)
} else {
  mountPlayground()
}
