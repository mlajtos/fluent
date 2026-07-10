// MARK: Setup

/*

1. clone the repo: `git clone https://github.com/mlajtos/fluent.git`
2. install dependencies: `cd fluent && bun install`
3. start the development server: `bun dev`
4. open `http://localhost:3000/?code=RG9jdW1lbnRhdGlvbg` in your browser
5. edit `client.tsx` (this file) to modify the language and IDE
6. share your changes with others

*/

// MARK: Documentation

const Documentation = `
# Fluent

Fluent is a tiny language + IDE for differentiable tensors and reactive UIs. Fluent is for programmers, researchers, and tinkerers who want to combine math, differentiation, and interactivity in a playful way.

## Quickstart

\`\`\`fluent
; tensors + broadcasting
a: [1, 2, 3],
a + 1, ; [2, 3, 4]

; functions + gradient
square: { x | x^2 },
square(a), ; [1, 4, 9]
∇(square)(a), ; [2, 4, 6]

; reactive UI
x: $(0.5),
Slider(x),
square(x),  ; auto-lifts to reactive!
Button("Reset", { x(0) }),
\`\`\`

## Features

### Syntax
- Tensors
  - multi-dimensional arrays of numbers
  - scalars: \`1\`, \`3.14\`, \`-42\`, \`1e-7\`
  - vectors: \`[1, 2, 3]\`, matrices: \`[[1, 2], [3, 4]]\`
  - auto-broadcasting: \`[1, 2, 3] + 1\` is \`[2, 3, 4]\`
  - indexing with \`_\`: \`a_0\`, \`a_(i + 1)\`, \`a_(-1)\` (last element)
    - glued \`_\` binds tight: \`x × θ_0 + θ_1\` needs no parentheses
  - length with \`#\`: \`#([1, 2, 3])\` is \`3\`
  - range with \`::\`: \`0 :: 10\` is \`[0, 1, 2, ..., 9]\`
- Lists
  - ordered collection of heterogeneous values
  - e.g. \`(1, 2, 3)\`, \`(1, (2, 3), [4])\`, \`()\`, \`(42,)\`
- Functions
  - lambda with \`{}\`: \`{ x | x + 1 }\`, \`{ x, y | x * y }\`, \`{ 42 }\`
  - last expression is the return value: \`{ a: 1, b: 2, a + b }\` returns \`3\`
  - application: \`add(1, 2)\` or infix \`1 add 2\`
  - self-reference: \`self\` refers to the current function (for recursion)
- Symbols
  - letter-based: \`a\`, \`FooBar\`, \`bar-baz-1\`, \`α\`, \`β\`, \`θ\`
  - operator-based: \`+\`, \`≠\`, \`!=\`, \`√\`, \`∇\`
  - assignment with \`:\`: \`a: 23, b: a + 24\`
- Evaluation order – whitespace IS precedence
  - spaced operators go left-to-right, NO numeric precedence: \`1 + 2 * 3\` is \`(1 + 2) * 3\` = \`9\`
  - glued operators bind tighter than spaced ones: \`1 + 2*3\` is \`1 + (2 * 3)\` = \`7\`
    - \`a*b + c*d\` is \`(a * b) + (c * d)\` – hug what belongs together
  - operator glued ONLY to its left operand takes everything to its right:
    - \`a: 1 + 2\` is \`a: (1 + 2)\` – assignment without parentheses
    - chains right: \`a: b: 1 + 2\` is \`a: (b: (1 + 2))\`
  - parentheses always override: \`1 + (2 * 3)\` is \`7\`
  - within a tier, evaluation is left-to-right: \`2*3^2\` is \`(2 * 3) ^ 2\` = \`36\`
  - one trap: glued \`-\` after a letter is part of the name (\`n-1\` is a symbol, kebab-case) – write \`n - 1\`
- Comments
  - single-line with \`;\`: \`a: 1, ; this is a comment\`
- Program structure
  - expressions separated by commas: \`a: 1, b: 2, a + b\`
  - last expression is the result (automatically displayed)

### Semantics
- Differentiable programming
  - gradient with \`∇\`: \`∇({ x | x^2 })(3)\` is \`6\`
  - higher-order: \`∇(∇({ x | x^3 }))(1)\` is \`6\`
- Reactive programming
  - create signal: \`x: $(0.5)\`
  - read signal: \`x()\`
  - update signal: \`x(0.7)\` or \`x ← 0.7\`
  - computed signal: \`y: x + 1\` – auto-lifts to reactive when \`x\` is a signal
  - explicit computed: \`y: $({ x() + 1 })\` – manual version (rarely needed)
  - watch a trainable variable: \`watch(θ)\` – a signal that updates on every assignment
- Iteration
  - repeat N times with \`⟳\`: \`step ⟳ 100\` (async, fire-and-forget)
  - function power with \`⍣\`: \`(double ⍣ 5)(1)\` is \`double(double(...))\` = \`32\`
- Pattern matching
  - \`guard(cond, { value })\`: returns a function that yields value if cond is truthy, else Error
  - use with \`cascade\` for pattern matching: \`cascade((guard(...), { default }))(arg)\`
  - example: \`fact: { n | f: self, cascade((guard(n = 0, { 1 }), { n * f(n - 1) }))() }\`
- Optimization
  - create variable: \`θ: ~([0, 0])\`
  - define loss: \`loss: { sum(θ^2) }\`
  - create optimizer: \`opt: adam(0.01)\`
  - minimize: \`{ opt(loss) } ⟳ 100\`
- Built-in functions
  - tensor math: \`+\`, \`-\`, \`*\`, \`/\`, \`^\`, \`√\`, \`sum\`, \`mean\`, \`max\`, \`min\`, \`sin\`, \`cos\`, \`log\`, \`exp\`, \`dot\`, \`matmul\`, \`transpose\`, \`reshape\`, \`clamp\`, \`sigmoid\`, \`relu\`, \`softmax\`, \`oneHot\`, \`crossEntropy\`, etc.
  - tensor creation: \`::\` (range), \`linspace\`, \`eye\`, \`rand\`, \`randn\`
  - axis variants: \`stack((a, b), axis)\`, \`concat((a, b), axis)\`, \`unstack(x, axis)\`, \`sum(x, axis)\`
  - outer product \`⊗\`: \`a (⊗ f) b\` pairs every cell of a with every cell of b; \`a (f ⊗ k) b\` keeps trailing k axes zipped
  - lists: \`ListConcat\`, \`ListLength\`, \`ListGet\`, \`ListMap\`, \`ListReduce\`
  - UI: \`Slider\`, \`Scrubber\`, \`Checkbox\`, \`Button\`, \`Text\`, \`Grid\`, \`Layers\`, \`Point2D\`, \`Trail\`, \`ImageUpload\`
  - live inputs: \`Camera\`, \`Microphone\`, \`MicrophoneSpectrum\`, \`Time\`, \`MousePosition\`
  - optimizers: \`adam\`, \`adamw\`, \`sgd\` (optional momentum), \`adagrad\`
  - metadata: \`Describe(fn, "doc")\` attaches a doc string, \`Describe(fn)\` queries it
- Ad-hoc operators
  - define custom operators: \`(++): ListConcat, (1, 2) ++ (3, 4)\`

### IDE
- Syntax highlighting
- Live evaluation with error reporting
- Automatic visualization of values (tensors, lists, functions)
- GPU-accelerated tensor operations (via jax-js on WebGPU, with Wasm/WebGL fallback)
- LLM-backed code generation (BYO Anthropic API key)
- Command palette (Ctrl+P or Ctrl+Shift+P)
- Auto-completion (Ctrl+Space)
- Shareable URL links (Ctrl+S)
- examples gallery (Ctrl+O; in Safari use Ctrl+Shift+O — plain ⌘O is reserved by the browser)
`

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

import { Signal, computed, signal } from "@preact/signals-core"
import {
  CodeParse, getParseErrors, evaluateSyntaxTreeNode, CodeEvaluate,
  createScope, PRELUDE, DefaultEnvironment, extendEnvironment, setCodeNodePrinter,
  evaluateGeneration, registerDisposable, disposeScopeTensors, disposeValueTensors,
  getAsSyncList, getMeta, setMeta, setOrigin, getOrigin,
  SignalCreate, SignalComputed, SignalRead, SignalUpdate, SignalOnce,
  identifierRegexp, numberRegexp, stringRegexp, operatorRegexp, delimiterRegexp,
  np, FluentVariable, isTensor, TensorScalarLive,
  type Value, type CurrentScope, type SyntaxTreeNode, type Origin, type FunctionMeta,
} from "./language"
import { useSignals, useSignal, useComputed } from '@preact/signals-react/runtime';
import { ErrorBoundary } from 'react-error-boundary';
import { useEffect, type JSX, isValidElement, useRef } from "react";
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
import { getWebGPUDevice } from "@jax-js/jax"
import { cachedFetch, safetensors } from "@jax-js/loaders"
import { interpolateViridis } from 'd3-scale-chromatic'
import { rgb } from 'd3-color'



// Web Worker setup for monaco-editor
(globalThis as { MonacoEnvironment?: typeof MonacoEnvironment }).MonacoEnvironment = {
  getWorker: () => new Worker(new URL("monaco.worker.js", document.baseURI), { type: "module" }),
}

// Configure @monaco-editor/react to use local monaco-editor package
loader.config({ monaco })

// jax-js backends are initialized by language.ts on import (WebGPU → Wasm → WebGL)

// MARK: Pixel I/O
// jax-js has no tf.browser – bridge ImageData ↔ arrays by hand. Pixels enter
// the language as float32 in 0–1: Fluent is a float32 language, and jax-js
// int32 arithmetic truncates (int ÷ 255 is 0, even mean() stays int) – the
// same trap TensorRange casts arange around. toPixels still accepts int
// arrays (0–255) for display.

const pixelReader = new OffscreenCanvas(1, 1)

// ImageBitmap / <video> / <img> → [h, w, 3] float32 tensor (0–1), untracked –
// the caller owns the result
const fromPixels = (source: ImageBitmap | HTMLVideoElement | HTMLImageElement): np.Array => {
  const width = (source as HTMLVideoElement).videoWidth || (source as { width: number }).width
  const height = (source as HTMLVideoElement).videoHeight || (source as { height: number }).height
  if (pixelReader.width !== width || pixelReader.height !== height) {
    pixelReader.width = width
    pixelReader.height = height
  }
  const context = pixelReader.getContext('2d', { willReadFrequently: true }) as OffscreenCanvasRenderingContext2D
  context.drawImage(source, 0, 0)
  const { data } = context.getImageData(0, 0, width, height)
  const rgbFlat = new Float32Array(width * height * 3)
  for (let i = 0, j = 0; i < data.length; i += 4, j += 3) {
    rgbFlat[j] = data[i]! / 255
    rgbFlat[j + 1] = data[i + 1]! / 255
    rgbFlat[j + 2] = data[i + 2]! / 255
  }
  return np.array(rgbFlat, { shape: [height, width, 3] })
}

// [h, w] | [h, w, 1] | [h, w, 3] tensor → canvas. Borrows `data`; the
// reference is taken synchronously, so the buffer outlives later disposal.
// The async read keeps the main thread free at 60fps (sync reads also
// disable the wasm backend's worker parallelism).
const toPixels = async (data: np.Array, canvas: HTMLCanvasElement) => {
  const [height = 1, width = 1, channels = 1] = data.shape
  const scale = data.dtype === np.int32 || data.dtype === np.uint32 ? 1 : 255
  const flat = await data.ref.data()
  const image = new ImageData(width, height)
  for (let p = 0, s = 0; p < image.data.length; p += 4, s += channels) {
    image.data[p] = (flat[s] as number) * scale
    image.data[p + 1] = (flat[channels < 3 ? s : s + 1] as number) * scale
    image.data[p + 2] = (flat[channels < 3 ? s : s + 2] as number) * scale
    image.data[p + 3] = 255
  }
  canvas.getContext('2d')?.putImageData(image, 0, 0)
}

// Hand a freshly created tensor to a signal: the signal becomes sole owner
const updateWithFresh = (target: Signal<any>, fresh: np.Array) => {
  SignalUpdate(target, fresh)
  fresh.dispose()
}

// MARK: GPU blit
// Render tensors straight from their WebGPU buffers: a fullscreen triangle
// samples the storage buffer per pixel – no readback, no ImageData loop.
// Heatmaps normalize against a GPU-computed [min, max] pair and color
// through the viridis LUT, all on-device. Off-WebGPU falls back to toPixels.

let blitDevice: GPUDevice | null | undefined
const getBlitDevice = (): GPUDevice | null => {
  if (blitDevice === undefined) {
    try { blitDevice = getWebGPUDevice() } catch { blitDevice = null }
  }
  return blitDevice
}

const WGSL_SCALAR: Record<string, string | undefined> = {
  float32: "f32", int32: "i32", uint32: "u32",
}

const blitShader = (scalar: string, mode: "image" | "heat") => /* wgsl */ `
struct BlitInfo { width: u32, height: u32, channels: u32, scale: f32 }
@group(0) @binding(0) var<uniform> info: BlitInfo;
@group(0) @binding(1) var<storage, read> data: array<${scalar}>;
${mode === "heat" ? `
@group(0) @binding(2) var<storage, read> bounds: array<f32>;
@group(0) @binding(3) var<storage, read> lut: array<f32>;
` : ""}

@vertex fn vs(@builtin(vertex_index) i: u32) -> @builtin(position) vec4f {
  var pos = array<vec2f, 3>(vec2f(-1.0, -3.0), vec2f(-1.0, 1.0), vec2f(3.0, 1.0));
  return vec4f(pos[i], 0.0, 1.0);
}

@fragment fn fs(@builtin(position) p: vec4f) -> @location(0) vec4f {
  let base = (u32(p.y) * info.width + u32(p.x)) * info.channels;
${mode === "heat" ? `
  let v = f32(data[base]);
  let t = clamp((v - bounds[0]) / max(bounds[1] - bounds[0], 1e-6), 0.0, 1.0);
  let i = u32(round(t * 255.0)) * 3u;
  return vec4f(lut[i], lut[i + 1u], lut[i + 2u], 1.0);
` : `
  if (info.channels == 1u) {
    let v = f32(data[base]) * info.scale;
    return vec4f(v, v, v, 1.0);
  }
  return vec4f(
    f32(data[base]) * info.scale,
    f32(data[base + 1u]) * info.scale,
    f32(data[base + 2u]) * info.scale,
    1.0);
`}
}
`

const blitPipelines = new Map<string, GPURenderPipeline>()
const getBlitPipeline = (device: GPUDevice, scalar: string, mode: "image" | "heat"): GPURenderPipeline => {
  const key = `${mode}:${scalar}`
  let pipeline = blitPipelines.get(key)
  if (!pipeline) {
    const module = device.createShaderModule({ code: blitShader(scalar, mode) })
    pipeline = device.createRenderPipeline({
      layout: "auto",
      vertex: { module, entryPoint: "vs" },
      fragment: { module, entryPoint: "fs", targets: [{ format: navigator.gpu.getPreferredCanvasFormat() }] },
    })
    blitPipelines.set(key, pipeline)
  }
  return pipeline
}

let viridisGpu: GPUBuffer | null = null
const getViridisGpu = (device: GPUDevice): GPUBuffer => {
  if (!viridisGpu) {
    viridisGpu = device.createBuffer({ size: VIRIDIS_DATA.byteLength, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST })
    device.queue.writeBuffer(viridisGpu, 0, VIRIDIS_DATA)
  }
  return viridisGpu
}

type CanvasBlitState = { context: GPUCanvasContext, meta: GPUBuffer }
const canvasBlitStates = new WeakMap<HTMLCanvasElement, CanvasBlitState | null>()
const getCanvasBlitState = (device: GPUDevice, canvas: HTMLCanvasElement): CanvasBlitState | null => {
  let state = canvasBlitStates.get(canvas)
  if (state === undefined) {
    const context = canvas.getContext("webgpu")
    state = context === null ? null : {
      context,
      meta: device.createBuffer({ size: 16, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST }),
    }
    if (state) {
      state.context.configure({ device, format: navigator.gpu.getPreferredCanvasFormat(), alphaMode: "opaque" })
    }
    canvasBlitStates.set(canvas, state)
  }
  return state
}

const blitToCanvas = (
  device: GPUDevice, canvas: HTMLCanvasElement, scalar: string, mode: "image" | "heat",
  meta: { width: number, height: number, channels: number, scale: number },
  buffers: GPUBuffer[],
): boolean => {
  const state = getCanvasBlitState(device, canvas)
  if (!state) { return false }
  const metaData = new ArrayBuffer(16)
  new Uint32Array(metaData, 0, 3).set([meta.width, meta.height, meta.channels])
  new Float32Array(metaData, 12, 1)[0] = meta.scale
  device.queue.writeBuffer(state.meta, 0, metaData)
  const pipeline = getBlitPipeline(device, scalar, mode)
  const bindGroup = device.createBindGroup({
    layout: pipeline.getBindGroupLayout(0),
    entries: [
      { binding: 0, resource: { buffer: state.meta } },
      ...buffers.map((buffer, i) => ({ binding: i + 1, resource: { buffer } })),
    ],
  })
  const encoder = device.createCommandEncoder()
  const pass = encoder.beginRenderPass({
    colorAttachments: [{ view: state.context.getCurrentTexture().createView(), loadOp: "clear", storeOp: "store" }],
  })
  pass.setPipeline(pipeline)
  pass.setBindGroup(0, bindGroup)
  pass.draw(3)
  pass.end()
  device.queue.submit([encoder.finish()])
  return true
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

const Button = (label?: string | Signal<string>, onClick?: () => void) => {
  return computed(() => {
    const disabled = (typeof onClick !== "function")
    const labelValue = (typeof label === "string" || label instanceof String) ? label : label instanceof Signal ? label.value : "Button"
    return <button className="bg-neutral-900 hover:bg-neutral-800 active:bg-neutral-700 rounded-xl border border-neutral-400 hover:border-neutral-300 active:border-neutral-200 disabled:border-neutral-800 disabled:cursor-not-allowed p-2 overflow-hidden" onClick={onClick} disabled={disabled}>{labelValue}</button>
  })
}
setMeta(Button, { noAutoLift: true })

const convertTextToHTML = async (value: string) => {
  const result = await renderMarkdown({ value }, {
    codeBlockRenderer: colorizeCodeAsync,
  })

  const element = result.element as HTMLElement

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

const NativeDOMElement = ({ fn }: { fn: () => Promise<HTMLElement> }) => {
  const ref = useRef<HTMLDivElement>(null)

  useEffect(() => {
    let mounted = true

    async function mount() {
      if (ref.current && mounted) {
        // Clear existing content first (handles HMR edge cases)
        ref.current.innerHTML = ''
        const element = await fn()
        if (ref.current && mounted) {
          ref.current.appendChild(element)
        }
      }
    }

    mount()

    return () => {
      mounted = false
      if (ref.current) {
        ref.current.innerHTML = ''
      }
    }
  }, [fn])

  return <div ref={ref}></div>
}

const Text = (value: string | String) => {
  if (typeof value !== "string" && !(value instanceof String)) {
    return new Error(`Text: expected string, got ${typeof value}`)
  }
  const str = String(value)  // normalize to primitive
  const fn = () => { return convertTextToHTML(str) }
  return <div className="prose prose-neutral prose-invert"><NativeDOMElement fn={fn} /></div>
}

// TextEditor(text) – editable always; TextEditor(text, enabled) – read-only
// while `enabled` is falsy (readOnly, not disabled: text stays selectable and
// the same <textarea> reconciles in place, so cursor and scroll survive).
const TextEditor = (editedValue: Signal<string>, enabled?: Signal<np.Array> | np.Array) => {
  return SignalComputed(() => {
    const on = enabled === undefined ||
      (getAsSyncList(enabled instanceof Signal ? enabled.value : enabled) as number) >= 0.5
    return <div className="grid">
      <textarea
        value={editedValue.value}
        readOnly={!on}
        title={on ? undefined : "read-only while training"}
        onChange={(e) => { if (on) { SignalUpdate(editedValue, e.target.value) } }}
        className={`block bg-neutral-800 focus:bg-neutral-900 rounded-xl border border-neutral-600 hover:border-neutral-500 focus:border-neutral-300 outline-none p-2 field-sizing-content ${on ? "" : "opacity-50 cursor-not-allowed"}`}
        // @ts-ignore
        style={{ fieldSizing: "content" }}
        rows={1}
      />
    </div>
  })
}
setMeta(TextEditor, { noAutoLift: true })

const Slider = (editedValue: Signal<np.Array>) => {
  return SignalComputed(() => {
    const valueAsList = getAsSyncList(editedValue?.value) as number

    return (
      <div className="grid">
        <input
          type="range"
          min={0}
          max={1}
          step={0.01}
          value={valueAsList}
          onChange={(e) => updateWithFresh(editedValue, TensorScalarLive(parseFloat(e.target.value)))}
          className="bg-neutral-900 focus:bg-neutral-800 rounded-xl border border-neutral-800 hover:border-neutral-700 focus:border-neutral-600 outline-none p-2 w-full h-2 cursor-pointer dark:bg-gray-700 place-self-center"
        />
      </div>
    )
  })
}
setMeta(Slider, { noAutoLift: true })

const Scrubber = (editedValue: Signal<np.Array>, sensitivity?: np.Array) => {
  const step = sensitivity ? getAsSyncList(sensitivity) as number : 1  // 0=stuck, negative=flipped
  const decimals = Math.max(0, Math.ceil(-Math.log10(Math.abs(step) || 1)))
  const factor = Math.pow(10, decimals)
  const color = COLORS.find(rule => rule.token === "number")?.foreground ?? "FFFFFF"

  return SignalComputed(() => {
    const value = getAsSyncList(editedValue?.value) as number

    const handlePointerDown = (e: React.PointerEvent<HTMLSpanElement>) => {
      e.preventDefault()
      const startX = e.clientX, startValue = value

      const onMove = (me: PointerEvent) => {
        const raw = startValue + (me.clientX - startX) * step * 0.1
        updateWithFresh(editedValue, TensorScalarLive(Math.round(raw * factor) / factor))
      }
      const onUp = () => {
        window.removeEventListener('pointermove', onMove)
        window.removeEventListener('pointerup', onUp)
      }
      window.addEventListener('pointermove', onMove)
      window.addEventListener('pointerup', onUp)
    }

    return (
      <span
        style={{ color: `#${color}`, cursor: 'ew-resize', userSelect: 'none' }}
        onPointerDown={handlePointerDown}
      >
        {value.toLocaleString("en-US", { maximumFractionDigits: decimals }).replace(/,/g, "_")}
      </span>
    )
  })
}
setMeta(Scrubber, { noAutoLift: true })

const Checkbox = (editedValue: Signal<np.Array>) => {
  return SignalComputed(() => {
    const checked = (getAsSyncList(editedValue?.value) as number) >= 0.5
    return (
      <input
        type="checkbox"
        checked={checked}
        onChange={(e) => updateWithFresh(editedValue, TensorScalarLive(e.target.checked ? 1 : 0))}
        className="w-5 h-5 accent-white cursor-pointer place-self-start"
      />
    )
  })
}
setMeta(Checkbox, { noAutoLift: true })

// Image picker + drop zone: writes the image into `target` as a [h, w, 3]
// tensor. Drag & drop works even where native file dialogs are unavailable
// (embedded webviews like VS Code's Simple Browser).
const ImageUpload = (target: Signal<np.Array>) => {
  const retired = generationRetired()
  const load = async (file: File | null | undefined) => {
    if (!file || !file.type.startsWith("image")) { return }
    const bitmap = await createImageBitmap(file)
    if (retired()) { bitmap.close(); return }
    updateWithFresh(target, fromPixels(bitmap))
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
setMeta(ImageUpload, { noAutoLift: true })

// Mouse position as a [2] tensor signal, normalized to the viewport (0..1)
function MousePosition(): Signal<np.Array> {
  const initial = np.array([0.5, 0.5])
  const s = SignalCreate<np.Array>(initial)
  initial.dispose()
  const onMove = (e: PointerEvent) => {
    updateWithFresh(s, np.array([e.clientX / window.innerWidth, e.clientY / window.innerHeight]))
  }
  window.addEventListener("pointermove", onMove)
  const result = computed(() => s.value) as Signal<np.Array> & { dispose: () => void }
  result.dispose = () => window.removeEventListener("pointermove", onMove)
  registerDisposable(result.dispose)
  return result
}

const Grid = (cols: np.Array, rows: np.Array) => {
  let gridTemplateColumns = ""
  let gridTemplateRows = ""

  const colsValue = getAsSyncList(cols)
  switch (cols.ndim) {
    case 0:
      gridTemplateColumns = `repeat(${colsValue}, minmax(auto, 1fr))`
      break;

    case 1:
      gridTemplateColumns = (colsValue as number[]).map((v) => `minmax(0, ${v}fr)`).join(" ")
      break;
    default:
      break;
  }

  if (rows !== undefined) {
    const rowsValue = getAsSyncList(rows)


    switch (rows.ndim) {
      case 0:
        gridTemplateRows = `repeat(${rowsValue}, minmax(auto, 1fr))`
        break;

      case 1:
        gridTemplateRows = (rowsValue as number[]).map((v) => `minmax(0, ${v}fr)`).join(" ")
        break;
      default:
        break;
    }
  }

  // noAutoLift: children arrive as signals (Button, TextEditor, ...) and each
  // renders as its own reactive island – one changing cell doesn't rebuild the grid.
  // Lists flatten into cells, and a signal holding a list re-renders the cells
  // dynamically: Grid(3)(tasks) works when tasks is $((...)).
  const flattenCells = (xs: any[]): any[] => xs.flatMap(x => Array.isArray(x) ? flattenCells(x) : [x])
  const isListSignal = (a: any) => a instanceof Signal && Array.isArray((a as Signal<unknown>).peek())
  // Without an explicit row template, rows hug their content (content-start) –
  // the default align-content: stretch would distribute the panel's spare
  // height across auto rows, inflating a small table to fill the whole panel.
  const applyChildren = (...args: any[]) => {
    const buildCells = () => flattenCells(args.map(a => isListSignal(a) ? (a as Signal<unknown>).value : a))
      .map(WrapWithPrintIfNotReactElement)
      .map((cell, i) => <div key={i} className="contents">{cell}</div>)
    return (
      <div className={`grid gap-2 overflow-scroll h-full ${gridTemplateRows ? "" : "content-start"}`} style={{ gridTemplateColumns, gridTemplateRows }}>
        {/* @ts-ignore */}
        {args.some(isListSignal) ? computed(() => <>{buildCells()}</>) : buildCells()}
      </div>
    )
  }
  setMeta(applyChildren, { noAutoLift: true })
  return applyChildren
}

// A quoted-AST cell gets the same framed panel as any printed value: the
// frame marks the cell, and the panel's full extent carries the origin hover
// (the bare svg's box is smaller than the cell). Interactive elements
// (Slider, Button, plots) stay unboxed.
// no overflow-scroll here: a scroll container contributes no minimum height
// to grid track sizing, so the row would size to the text cells and clip the
// tree – the frame must grow with its drawing instead
const frameIfAstTree = (el: any) =>
  el?.props?.className?.includes?.("ast-tree")
    ? <Panel>{el}</Panel>
    : el

function WrapWithPrintIfNotReactElement(child: any): any {
  if (child instanceof Signal) {
    // Reactive island: render EAGERLY inside the same computed that reads the
    // value – a second lazy layer (Print's computed) could otherwise render a
    // tensor after its producer disposed it.
    return computed(() => {
      const value = (child as Signal<unknown>).value
      if (value instanceof Signal) { return WrapWithPrintIfNotReactElement(value) }
      if (isValidElement(value)) { return <WithOriginHighlight obj={value}>{frameIfAstTree(value)}</WithOriginHighlight> }
      // the origin wrapper sits OUTSIDE the panel: hovering the box's empty
      // padding highlights this value's source, instead of bubbling up to
      // the whole surrounding expression (e.g. an entire Grid)
      return (
        <WithOriginHighlight obj={value}>
          <Panel className="overflow-scroll">
            <ErrorBoundary fallback={<div>Something went wrong</div>} resetKeys={[value]}>
              {PrettyPrint(value)}
            </ErrorBoundary>
          </Panel>
        </WithOriginHighlight>
      )
    });
  }
  if (isValidElement(child)) {
    return <WithOriginHighlight obj={child}>{frameIfAstTree(child)}</WithOriginHighlight>;
  } else {
    return Print(child);
  }
}

// Stack visuals on top of each other: the first child defines the size,
// the rest overlay it – Layers(surface, Point2D(θ, range))
const Layers = (...children: any[]) => (
  <div className="relative w-fit" data-layers="true">
    {children.map((child, i) => (
      <div key={i} className={i === 0 ? "" : "absolute inset-0"}>
        {child instanceof Signal
          ? computed(() => PrettyPrint((child as Signal<unknown>).value)) as unknown as JSX.Element
          : PrettyPrint(child)}
      </div>
    ))}
  </div>
)
setMeta(Layers, { noAutoLift: true })

// Shared helpers for components bound to a [2] point in a data range
const pointMapping = (range?: np.Array) => {
  const [[x0, x1], [y0, y1]] = (range
    ? getAsSyncList(range)
    : [[0, 1], [0, 1]]) as [[number, number], [number, number]]
  return { x0, y0, spanX: x1 - x0 || 1, spanY: y1 - y0 || 1 }
}
const readPoint = (target: FluentVariable | Signal<np.Array>): [number, number] => {
  const value = target instanceof Signal ? target.peek() : target
  return getAsSyncList(value) as [number, number]
}

// Draggable 2D point bound to a [2] variable (or signal), mapped to a data
// range [[x0, x1], [y0, y1]]. Dragging the dot assigns; assignments (e.g. by
// an optimizer) move the dot. The layer is transparent to pointer events, so
// several Point2Ds can share one Layers stack.
const Point2D = (target: FluentVariable | Signal<np.Array>, range?: np.Array, color?: Value) => {
  const { x0, y0, spanX, spanY } = pointMapping(range)
  const fill = String(color ?? "white")
  const version: Signal<number> | undefined = target instanceof FluentVariable ? target.version : undefined

  const write = (event: React.PointerEvent<HTMLDivElement>) => {
    const rect = event.currentTarget.parentElement!.getBoundingClientRect()
    const point = np.array([
      x0 + (event.clientX - rect.left) / rect.width * spanX,
      y0 + (event.clientY - rect.top) / rect.height * spanY,
    ])
    if (target instanceof Signal) { updateWithFresh(target, point) }
    else { target.assign(point) }
  }

  return SignalComputed(() => {
    if (version) { version.value }
    if (target instanceof Signal) { target.value }
    const [px, py] = readPoint(target)

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
            // Grab arbitration: stacked points would always give the drag to the
            // topmost layer – instead, the dot whose center is nearest to the
            // pointer wins (ties go to the earlier layer)
            const layersRoot = e.currentTarget.closest('[data-layers]')
            const dots = layersRoot ? [...layersRoot.querySelectorAll('[data-point2d]')] : []
            const distanceTo = (el: Element) => {
              const r = el.getBoundingClientRect()
              return Math.hypot(e.clientX - (r.left + r.width / 2), e.clientY - (r.top + r.height / 2))
            }
            let nearest: Element = e.currentTarget
            let best = Infinity
            for (const el of dots) {
              const d = distanceTo(el)
              if (d < best) { best = d; nearest = el }  // ties go to the earlier layer
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
          onPointerMove={(e) => { if (e.buttons) { write(e) } }}
        >
          <div style={{ width: 12, height: 12, borderRadius: 999, background: fill, border: "2px solid black", boxShadow: "0 0 6px rgba(0,0,0,0.8)" }} />
        </div>
      </div>
    )
  })
}
setMeta(Point2D, { noAutoLift: true })

// The path a point has taken: re-renders on every assignment to the target
const Trail = (target: FluentVariable | Signal<np.Array>, range?: np.Array, color?: Value) => {
  const { x0, y0, spanX, spanY } = pointMapping(range)
  const stroke = String(color ?? "white")
  const version: Signal<number> | undefined = target instanceof FluentVariable ? target.version : undefined
  const points: string[] = []

  return SignalComputed(() => {
    if (version) { version.value }
    if (target instanceof Signal) { target.value }
    const [px, py] = readPoint(target)
    points.push(`${(px - x0) / spanX * 100},${(py - y0) / spanY * 100}`)
    if (points.length > 500) { points.shift() }

    return (
      <svg
        className="absolute inset-0 w-full h-full pointer-events-none"
        viewBox="0 0 100 100" preserveAspectRatio="none"
      >
        <polyline
          points={points.join(" ")}
          fill="none" stroke={stroke} strokeOpacity={0.75} strokeWidth={1.5}
          vectorEffect="non-scaling-stroke"
        />
      </svg>
    )
  })
}
setMeta(Trail, { noAutoLift: true })

const Fetch = (url: string) => {
  const s = SignalCreate<Value>(null)

  fetch(url)
    .then(response => response.text())
    .then(data => {
      s.value = data
    })
    .catch(error => {
      s.value = new Error(`Error fetching data: ${error.message}`)
    });

  return s
}


// CORS fallback: retry blocked cross-origin loads through the dev server
const proxied = (url: string) => `/proxy?url=${encodeURIComponent(url)}`

// Async sources resolve after an arbitrary delay; if the evaluation that
// created them has been retired by then (every keystroke re-evaluates), the
// result must be dropped – tensors written into a dead signal are never
// disposed, and a camera stream nobody stops stays on.
const generationRetired = (): (() => boolean) => {
  let retired = false
  registerDisposable(() => { retired = true })
  return () => retired
}

function LoadTensorFromImageUrl(url: string): Signal<np.Array | null> {
  const s = SignalCreate<np.Array | null>(null);
  const retired = generationRetired()

  const imgElement = document.createElement('img');
  imgElement.crossOrigin = "anonymous";
  imgElement.src = url;

  imgElement.onerror = () => {
    if (!imgElement.src.startsWith(proxied(""))) {
      imgElement.src = proxied(url)
    }
  }
  imgElement.onload = () => {
    if (retired()) { return }
    updateWithFresh(s, fromPixels(imgElement))
  }

  return s
}

// Shared engine for realtime tensor sources (Camera, Microphone, Time):
// a read-only signal driven by requestAnimationFrame that disposes stale
// tensors. `frame` returns a fresh tensor (ownership transfers here), or
// null to keep the current one. Takes ownership of `initial`.
function FrameSignal(initial: np.Array, frame: (time: number) => np.Array | null, cleanup?: () => void): Signal<np.Array> & { dispose: () => void } {
  const source = SignalCreate<np.Array>(initial)
  initial.dispose()
  let active = true

  const tick = (time: number) => {
    if (!active) return

    const next = frame(time)
    if (next) {
      updateWithFresh(source, next)
    }
    requestAnimationFrame(tick)
  }
  requestAnimationFrame(tick)

  const result = computed(() => source.value) as Signal<np.Array> & { dispose: () => void }
  result.dispose = () => {
    active = false
    cleanup?.()
  }
  registerDisposable(result.dispose)
  return result
}

function Camera(width?: np.Array, height?: np.Array, fps?: np.Array): Signal<np.Array> {
  const w = width ? getAsSyncList(width) as number : 640
  const h = height ? getAsSyncList(height) as number : 480
  const frameInterval = 1000 / (fps ? getAsSyncList(fps) as number : 30)

  const video = document.createElement('video')
  video.autoplay = true
  video.playsInline = true

  const retired = generationRetired()
  let stream: MediaStream | null = null
  navigator.mediaDevices.getUserMedia({ video: { width: w, height: h } }).then(s => {
    // permission granted after the evaluation was retired – the cleanup
    // already ran with stream = null, so stop the tracks here or the camera
    // light stays on until the tab closes
    if (retired()) { s.getTracks().forEach(track => track.stop()); return }
    stream = s
    video.srcObject = s
    video.onloadedmetadata = () => video.play()
  }).catch(err => console.error('Camera access denied:', err))

  let lastTime = 0
  // Initialize with black frame so operations don't fail before camera starts
  return FrameSignal(np.zeros([h, w, 3]), (time) => {
    if (time - lastTime < frameInterval || video.readyState !== video.HAVE_ENOUGH_DATA) { return null }
    lastTime = time
    return fromPixels(video)
  }, () => {
    stream?.getTracks().forEach(track => track.stop())
    video.srcObject = null
  })
}

// Shared getUserMedia/AnalyserNode setup for Microphone and MicrophoneSpectrum
function AudioAnalyser(fftSize: number, smoothing?: number): { get: () => AnalyserNode | null, cleanup: () => void } {
  const retired = generationRetired()
  let stream: MediaStream | null = null
  let audioContext: AudioContext | null = null
  let analyser: AnalyserNode | null = null

  navigator.mediaDevices.getUserMedia({ audio: true }).then(s => {
    // permission granted after the evaluation was retired – cleanup already
    // ran with stream = null, so release the microphone here
    if (retired()) { s.getTracks().forEach(track => track.stop()); return }
    stream = s
    audioContext = new AudioContext()
    analyser = audioContext.createAnalyser()
    analyser.fftSize = fftSize
    if (smoothing !== undefined) { analyser.smoothingTimeConstant = smoothing }
    audioContext.createMediaStreamSource(s).connect(analyser)
  }).catch(err => console.error('Microphone access denied:', err))

  return {
    get: () => analyser,
    cleanup: () => {
      stream?.getTracks().forEach(track => track.stop())
      audioContext?.close()
    }
  }
}

function Microphone(bufferSize?: np.Array): Signal<np.Array> {
  const size = bufferSize ? getAsSyncList(bufferSize) as number : 2048
  const audio = AudioAnalyser(size * 2)
  const dataArray = new Float32Array(size)

  // Initialize with silence
  return FrameSignal(np.zeros([size]), () => {
    const analyser = audio.get()
    if (!analyser) { return null }
    analyser.getFloatTimeDomainData(dataArray)
    return np.array(dataArray.slice())
  }, audio.cleanup)
}

function MicrophoneSpectrum(bufferSize?: np.Array): Signal<np.Array> {
  const size = bufferSize ? getAsSyncList(bufferSize) as number : 1024
  const audio = AudioAnalyser(size * 2, 0.8)
  const dataArray = new Uint8Array(size)

  // Initialize with silence; normalized to 0-1 range
  return FrameSignal(np.zeros([size]), () => {
    const analyser = audio.get()
    if (!analyser) { return null }
    analyser.getByteFrequencyData(dataArray)
    return np.trueDivide(np.array(Float32Array.from(dataArray)), 255)
  }, audio.cleanup)
}

function Time(): Signal<np.Array> {
  const startTime = performance.now()
  return FrameSignal(TensorScalarLive(0), () => TensorScalarLive((performance.now() - startTime) / 1000))
}

// The audio hardware's sample rate (Hz) – needed to turn FFT bins into pitches.
// Created lazily on first use so a non-audio program never spins up an AudioContext.
let sampleRateContext: AudioContext | null = null
function SampleRate(): np.Array {
  if (!sampleRateContext) { sampleRateContext = new AudioContext() }
  return TensorScalarLive(sampleRateContext.sampleRate)
}


const LoadSafeTensorFromURL = (rawUrl?: unknown) => {
  if (rawUrl == null) {
    return new Error("loadSafeTensorFromURL: url is required")
  }
  // Fluent strings are String objects, not primitives – coerce before use
  const url = String(rawUrl)
  // Start as an empty list, not a string: readers can gate on `#(data()) > 0`
  // without ListGet/indexing throwing on the loading placeholder (a throw
  // re-mounts the component → re-evaluates the program → re-fetches in a loop).
  const s = SignalCreate([] as unknown[]);
  const retired = generationRetired()

  // @jax-js/loaders: cachedFetch persists the download in OPFS (so reloads /
  // HMR don't re-fetch big weight files), safetensors.parse reads every dtype.
  cachedFetch(url)
    .catch(() => cachedFetch(proxied(url)))
    .then(bytes => {
      if (retired()) { return }
      const { tensors } = safetensors.parse(bytes)
      // every tensor → a float32 np.array (BigInt64/Uint8/… all coerce via Number)
      s.value = Object.entries(tensors).map(([key, t]) => {
        const data = Float32Array.from(t.data as unknown as ArrayLike<number>, Number)
        return [key, np.array(data, { shape: t.shape })]
      })
    })

  return s
}
// returns a signal that resolves async – lifting would re-invoke it (re-fetch)
// on every render, so keep it out of the reactive graph
setMeta(LoadSafeTensorFromURL, { noAutoLift: true })
setMeta(LoadTensorFromImageUrl, { noAutoLift: true })


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
  label: string, row: number, col: number, width: number, origin: Origin,
  children: GridNode[], parent: GridNode | null,
}
type GridEdge = { parentRow: number, parentCol: number, childRow: number, childCol: number }

// The `row,col` keys of the subtree under the hovered AST node – its nodes get
// brighter frames and its edges brighter strokes. Null when nothing is hovered.
const hoverSubtree = signal<Set<string> | null>(null)

// Reference to main editor for hover highlighting
let mainEditorRef: { editor: editor.IStandaloneCodeEditor; monaco: Monaco } | null = null
let hoverDecorationsCollection: editor.IEditorDecorationsCollection | null = null

const setHoverHighlight = (origin: Origin | null) => {
  if (!mainEditorRef) return
  const { editor: ed } = mainEditorRef

  if (!hoverDecorationsCollection) {
    hoverDecorationsCollection = ed.createDecorationsCollection()
  }

  hoverDecorationsCollection.set(origin ? [{
    range: {
      startLineNumber: origin.start.line,
      startColumn: origin.start.column,
      endLineNumber: origin.end.line,
      endColumn: origin.end.column,
    },
    options: {
      className: 'ast-hover-highlight',
      isWholeLine: false,
    }
  }] : [])
}

function nodeWidth(label: string): number {
  return label.length * GLYPH.width + 2 * TREE.nodePadding.x
}

let treeInstance = 0

function Tree(tree: SyntaxTreeNode & { type: "Program" }): JSX.Element {
  const { nodes, edges } = layoutGrid(tree)
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

  return (
    <svg className="ast-tree" style={{ width: totalWidth, height: totalHeight, shapeRendering: "optimizeSpeed" }} viewBox={`0 0 ${totalWidth} ${totalHeight}`}>
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

function TreeNode({ x, y, label, width, origin, active, onEnter }: { x: number, y: number, label: string, width: number, origin: Origin, active: boolean, onEnter: () => void }): JSX.Element {
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

// Grid layout: assigns (row, col) positions to AST nodes
function layoutGrid(tree: SyntaxTreeNode & { type: "Program" }): { nodes: GridNode[], edges: GridEdge[] } {
  const nodes: GridNode[] = []
  const edges: GridEdge[] = []
  const used = new Set<string>()

  function getLabel(node: SyntaxTreeNode): string {
    switch (node.type) {
      case 'Symbol': return node.content.value
      case 'Number': return node.content.value.toString()
      case 'Operation':
        // computed operator (e.g. a (+ & -) b) shows as an explicit apply node
        if (node.content.operator.type === 'Operation') { return '.' }
        return getLabel(node.content.operator)
      case 'Tensor': return '[]'
      case 'Lambda': return '{}'
      case 'List': return '()'
      case 'String': return `"${node.content.value}"`
      case 'Code': return '``'
      case 'Error': return '⚠'
      default: throw new Error(`Unknown type ${node.type}`)
    }
  }

  function getChildren(node: SyntaxTreeNode): SyntaxTreeNode[] {
    switch (node.type) {
      case 'Program': return node.content
      case 'Operation':
        // computed operator: explicit apply – (a,b) . (+ & -)
        if (node.content.operator.type === 'Operation') {
          return [node.content.args, node.content.operator]
        }
        return node.content.args.content.value
      case 'Tensor': return node.content.value
      case 'Lambda':
        return [
          { type: 'List', content: { value: node.content.args }, origin: node.origin },
          ...(node.content.expr as SyntaxTreeNode & { type: "Program" }).content
        ]
      case 'List': return node.content.value
      case 'Symbol':
      case 'Number':
      case 'String':
      case 'Code':
      case 'Error': return []
      default: throw new Error(`Unknown type ${(node as SyntaxTreeNode).type}`)
    }
  }

  // A node sits on the same row as its first child, so the leftmost spine of
  // every subtree is horizontal. spineLength = how many columns it spans.
  function spineLength(node: SyntaxTreeNode): number {
    const children = getChildren(node)
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
  function layout(node: SyntaxTreeNode, minRow: number, col: number, parent: GridNode | null): { node: GridNode, maxRow: number } {
    const spine = spineLength(node)
    // the spine follows first children, so every spine node but the last leaf
    // owns a trunk – track which spine positions to test against reserved trunks
    const spineHasTrunk: boolean[] = []
    for (let s: SyntaxTreeNode = node, i = 0; i < spine; i++) {
      const kids = getChildren(s)
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

    const label = getLabel(node)
    const self: GridNode = { label, row, col, width: nodeWidth(label), origin: node.origin, children: [], parent }
    nodes.push(self)
    used.add(`${row},${col}`)

    const children = getChildren(node)
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
  for (const stmt of tree.content) {
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

// MARK: Runtime

// MARK: PrettyPrint

const frameStyle = "rounded-xl border border-neutral-800 hover:border-neutral-700 p-2"

function Print(obj: Signal<unknown>) {
  // origin wrapper outside the panel – hovering the box's empty padding
  // highlights this value's source (see WrapWithPrintIfNotReactElement)
  return computed(() =>
    <WithOriginHighlight obj={obj instanceof Signal ? obj.value : obj}>
      <Panel className="overflow-scroll" testId="print-panel">
        <ErrorBoundary fallback={<div>Something went wrong</div>} resetKeys={[obj]}>
          {PrettyPrint(obj)}
        </ErrorBoundary>
      </Panel>
    </WithOriginHighlight>
  )
}
setMeta(Print, { noAutoLift: true })

function PrintFunction(fn: Function) {
  // Subscribe to meta version for reactive updates
  if ((fn as any).__meta_v__) (fn as any).__meta_v__.value

  const meta = getMeta(fn)
  // Fluent lambdas override toString with their source; built-ins would show
  // minified JS internals, which help nobody – never display those
  const hasFluentSource = Object.prototype.hasOwnProperty.call(fn, "toString")
  const source = hasFluentSource ? Code(computed(() => fn.toString())) : null
  if (meta.doc) {
    // same card as hover/completion: signature, one-liner, worked example
    return <>{Text(docCard(meta))}{source}</>
  }
  if (source) { return source }
  return Text(`\`${fn.name || "function"}\` — a built-in without a doc card yet`)
}

function PrettyPrintSyntaxTree(node: SyntaxTreeNode): JSX.Element {
  // a backtick literal whose inner program failed to parse yields an Error
  // node (its content is the message string) – render that message; feeding it
  // to the tree layout used to throw and take the whole output panel down
  if (node.type !== "Program") {
    const message = node.type === "Error" ? node.content : `CodePrint: expected a program, got ${(node as { type?: string })?.type ?? String(node)}`
    return PrettyPrint(new Error(message))
  }
  return Tree(node)
}

function WithOriginHighlight({ obj, children }: { obj: any, children: JSX.Element }): JSX.Element {
  const origin = getOrigin(obj)
  if (!origin) return children
  return (
    <span
      style={{ display: 'contents' }}
      onPointerOver={(e) => { e.stopPropagation(); setHoverHighlight(origin) }}
      onPointerOut={() => setHoverHighlight(null)}
    >
      {children}
    </span>
  )
}

function PrettyPrint(obj: any): JSX.Element {
  return <WithOriginHighlight obj={obj}>{PrettyPrintInner(obj)}</WithOriginHighlight>
}

function PrettyPrintInner(obj: any): JSX.Element {
  if (obj === null || obj === undefined) {
    return <div className="font-extrabold text-3xl">◌</div>;
  }

  if (typeof obj === "string" || obj instanceof String) {
    const str = String(obj)  // normalize to primitive for display
    const color = COLORS.find(rule => rule.token === "string")?.foreground;
    return <div style={{ color: `#${color}` }}>"{str}"</div>;
  }

  if (Array.isArray(obj)) {
    return (
      <div className="grid gap-1 rounded-xl">
        {obj.map((item, key) => (
          // origin wrapper outside the row box – hovering a row's empty
          // space highlights that item's source, not the whole list
          <WithOriginHighlight key={key} obj={item}>
            <div className={`${frameStyle} !border-0 grid hover:bg-neutral-900 mix-blend-screen`}>{PrettyPrint(item)}</div>
          </WithOriginHighlight>
        ))}
      </div>
    );
  }

  if (typeof obj === "function") {
    // @ts-ignore
    return PrintFunction(obj);
  }

  if (typeof obj === "object") {

    if (obj instanceof Promise) {
      return <>"! Promise"</>
    }

    if (obj instanceof Signal) {
      // Reactive island: read `.value` inside a fresh computed so outer printers
      // don't subscribe – a signal update re-renders only this subtree instead of
      // the whole output panel. The read can throw (an evaluation error surfacing
      // lazily); catch it into an Error box so the panel keeps rendering and
      // recovers on the next evaluation instead of tripping the error boundary,
      // which has no changing resetKeys to ever come back from.
      return <div className="contents">{computed(() => {
        try {
          return PrettyPrint(obj.value)
        } catch (e) {
          return PrettyPrint(e instanceof Error ? e : new Error(String(e)))
        }
      }) as unknown as JSX.Element}</div>;
    }

    if (isValidElement(obj)) {
      return obj;
    }

    // Reactive variable: create computed that re-reads on version change
    if (obj instanceof FluentVariable) {
      const variable = obj
      let previous: np.Array | null = null
      // the island holds one reference between renders – release it with the
      // generation, or every displayed variable pins its final value forever
      registerDisposable(() => {
        if (previous && previous.refCount > 0) { previous.dispose() }
        previous = null
      })
      const reactiveValue = computed(() => {
        variable.version.value  // subscribe to changes
        if (previous && previous.refCount > 0) { previous.dispose() }
        previous = variable.current.ref
        return previous
      })
      return PrettyPrint(reactiveValue)
    }

    if (isTensor(obj)) {
      if (obj.ndim === 0) {
        const color = COLORS.find(rule => rule.token === "number")?.foreground || "FFFFFF";
        const formattedNumber = (getAsSyncList(obj) as number).toLocaleString("en-US", { minimumFractionDigits: 0, maximumFractionDigits: 6, useGrouping: true }).replace(/,/g, "_");
        return <span style={{ color: `#${color}` }}>{formattedNumber}</span>;
      }

      if (obj.ndim === 1) {
        return (
          <div className="">
            <BarPlot data={obj} />
          </div>
        );
      }

      if (obj.ndim === 2) {
        // Use fast canvas with viridis colormap for large tensors, Plotly for small (debugging)
        const size = (obj.shape[0] ?? 0) * (obj.shape[1] ?? 0)
        if (size > 400) {
          return <HeatCanvas data={obj} />
        }

        return <HeatPlot data={obj} />
      }

      if (obj.ndim === 3) {
        // RGB image - use canvas renderer
        return <TensorCanvas data={obj} />
      }

      if (obj.ndim > 3) {
        console.warn("Tensor with rank > 3 is not supported for plotting", obj);
      }

      return (
        <div className="">
          <span className="font-bold">Tensor:</span> {obj.shape.join(", ")}<br />
          <BarPlot data={obj} />
        </div>
      );
    }

    if (obj instanceof Error) {
      // traverse error causes and collent them into a flat list
      function linearizeError(error: Error): Error[] {
        const errors = [error];
        let currentError: any = error;
        while (currentError.cause && currentError.cause instanceof Error) {
          errors.push(currentError.cause);
          currentError = currentError.cause;
        }
        return errors;
      }

      return <ol className="list-decimal list-inside flex flex-col gap-2 border rounded-lg border-red-700 bg-red-950 overflow-scroll max-h-24">{linearizeError(obj).reverse().map((error, index) => {
        return (
          <li key={index}
            className="list-item px-2 py-0.5 text-red-700 cursor-pointer hover:bg-red-700 hover:text-red-200"
            onPointerOver={() => {
              setHoverHighlight(getOrigin(error) ?? null)
            }}
            onPointerOut={() => {
              setHoverHighlight(null)
            }}>
            {error.message}
            {/* {obj.cause ? <div>{PrettyPrint(obj.cause)}</div> : null} */}
          </li>
        );
      })}</ol>;



    }

    return (
      <div className="">
        {Object.entries(obj).map(([key, value]) => (
          <div key={key}>
            <span className="font-bold">{key}:</span> {PrettyPrint(value)}
          </div>
        ))}
      </div>
    );
  }
  console.warn("Unsupported type for PrettyPrint:", obj);

  return <div className="text-violet-500">{String(obj)}</div>;
}

function Panel({ children, className, testId }: { children: JSX.Element, className?: string, testId?: string }) {
  return (
    <div className={`${frameStyle} ${className ?? ""}`} data-testid={testId}>
      {children}
    </div>
  )
}

// MARK: Plots

const BarPlot = ({ data }: { data: np.Array }) => {

  const annotations: Partial<Annotations>[] = [];
  const values = getAsSyncList(data) as number[]

  if (data.shape.reduce((a, b) => a * b, 1) < 20) {
    // @ts-ignore
    annotations.push(...values.map((value: number, i: number) => ({
      xref: "x1",
      yref: "y1",
      yanchor: "bottom",
      x: i,
      y: value,
      text: value.toLocaleString("en-US", { minimumFractionDigits: 0, maximumFractionDigits: 2, useGrouping: true }).replace(/,/g, "_"),
      font: { color: '#D4D4D4', shadow: "0px 0px 1px black" },
      showarrow: false,
    })));
  }

  return (
    <Plot
      data={[
        {
          y: values,
          type: 'bar',
          marker: {
          color: values,
          colorscale: 'Viridis',
        },
        },
      ]}
      layout={{
        paper_bgcolor: 'transparent',
        plot_bgcolor: 'transparent',
        font: { color: '#D4D4D4', },
        annotations,
        xaxis: {
          automargin: true,
        },
        yaxis: {
          gridcolor: '#444444',
          automargin: true,
        },
        margin: {
          pad: 10,
          t: 0,
          l: 0,
          r: 0,
          b: 0
        }
      }}
      config={{
        staticPlot: true,
        // when responsive is true, the plot flashes in random position when updating
        responsive: false,
      }}
      style={{ width: '100%', height: "10em" }}
    />
  );
}

const HeatPlot = ({ data }: { data: np.Array }) => {
  const z = getAsSyncList(data) as number[][];

  let annotations: Partial<Annotations>[] = [];

  if (data.shape.reduce((a, b) => a * b, 1) < 20) {
    // @ts-ignore
    annotations = z.map((row, i) => {
      return row.map((value, j) => ({
        xref: "x1",
        yref: "y1",
        x: j,
        y: i,
        text: value.toFixed(2),
        font: { color: '#D4D4D4', shadow: "0px 0px 1px black" },
        showarrow: false,
      }));
    }
    ).flat();
  }

  return (
    <Plot
      data={[
        {
          z,
          type: 'heatmap',
          colorscale: 'Viridis',
          // colorscale: "Greys"
        },
      ]}
      layout={{
        margin: {
          pad: 10,
          t: 0,
          l: 0,
          r: 0,
          b: 0
        },
        paper_bgcolor: 'transparent',
        plot_bgcolor: 'transparent',
        font: { color: '#D4D4D4', },
        annotations,
        xaxis: {
          automargin: true,
          side: 'top',
        },
        yaxis: {
          gridcolor: '#444444',
          automargin: true,
          autorange: 'reversed',
        },
      }}
      config={{
        staticPlot: true,
        autosizable: true,
        responsive: true,
      }}
      // height: 100% of an auto-sized parent is circular and sometimes resolves
      // to 0 – Plotly's absolutely-positioned svg then paints at its default
      // size over the panels below. The min-height floor breaks the cycle.
      style={{ width: '100%', height: '100%', minHeight: '16em' }}
    />
  );
}

// Fast canvas-based tensor renderer for real-time use (Camera, etc.)
const TensorCanvas = ({ data }: { data: np.Array }) => {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  const height = data.shape[0] || 100
  const width = data.shape[1] || 100
  const aspect = width / height

  useEffect(() => {
    if (!canvasRef.current || !data || data.refCount === 0) { return }

    const canvas = canvasRef.current

    // Only resize if dimensions changed (resizing clears canvas, causes flicker)
    if (canvas.width !== width || canvas.height !== height) {
      canvas.width = width
      canvas.height = height
    }

    const device = getBlitDevice()
    const scalar = WGSL_SCALAR[data.dtype]
    if (device && scalar && getCanvasBlitState(device, canvas)) {
      const channels = data.shape[2] ?? 1
      const scale = data.dtype === np.int32 || data.dtype === np.uint32 ? 1 / 255 : 1
      // gpuBuffer() consumes a reference; an animated source (⟳) disposes
      // this frame's tensor before the async read resolves, recycling its
      // buffer mid-render. Hold a reference until submit, then release it.
      const keep = data.ref
      data.ref.gpuBuffer().then((buffer) => {
        if (canvasRef.current === canvas) {
          blitToCanvas(device, canvas, scalar, "image", { width, height, channels, scale }, [buffer])
        }
      }).catch((e) => console.warn("blit failed:", e))
        .finally(() => { if (keep.refCount > 0) { keep.dispose() } })
      return
    }

    toPixels(data, canvas)
  }, [data])

  return (
    <canvas
      ref={canvasRef}
      style={{
        width: '100%',
        maxWidth: `${width}px`,
        aspectRatio: `${aspect}`,
        imageRendering: 'pixelated'
      }}
    />
  )
}

// Generate viridis colormap LUT from d3-scale-chromatic (256 entries, RGB 0-1)
const VIRIDIS_DATA = Float32Array.from(
  Array.from({ length: 256 }, (_, i) => {
    const color = rgb(interpolateViridis(i / 255))
    return [color.r / 255, color.g / 255, color.b / 255]
  }).flat()
)
const VIRIDIS_LUT = np.array(VIRIDIS_DATA, { shape: [256, 3] })

// Fast canvas-based heatmap with viridis colormap (GPU-accelerated via LUT)
const HeatCanvas = ({ data }: { data: np.Array }) => {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  const height = data.shape[0] || 100
  const width = data.shape[1] || 100
  const aspect = width / height

  useEffect(() => {
    if (!canvasRef.current || !data || data.refCount === 0) { return }

    const canvas = canvasRef.current

    if (canvas.width !== width || canvas.height !== height) {
      canvas.width = width
      canvas.height = height
    }

    const device = getBlitDevice()
    const scalar = WGSL_SCALAR[data.dtype]
    if (device && scalar && getCanvasBlitState(device, canvas)) {
      // [min, max] computed on-device; the shader normalizes and colors
      // through the LUT per pixel – nothing is ever read back. Both data and
      // bounds buffers are held across the async read (gpuBuffer consumes a
      // reference) so an animated source can't recycle them mid-render.
      const bounds = np.astype(np.stack([np.min(data.ref), np.max(data.ref)]), np.float32)
      const keepData = data.ref
      const keepBounds = bounds.ref
      Promise.all([data.ref.gpuBuffer(), bounds.gpuBuffer()]).then(([dataBuffer, boundsBuffer]) => {
        if (canvasRef.current === canvas) {
          blitToCanvas(device, canvas, scalar, "heat", { width, height, channels: 1, scale: 1 }, [dataBuffer, boundsBuffer, getViridisGpu(device)])
        }
      }).catch((e) => console.warn("blit failed:", e))
        .finally(() => {
          if (keepData.refCount > 0) { keepData.dispose() }
          if (keepBounds.refCount > 0) { keepBounds.dispose() }
        })
      return
    }

    // Apply viridis colormap via lookup table: normalize to [0, 1], scale to
    // LUT indices, gather colors. Move semantics free the intermediates.
    const min = np.min(data.ref)
    const range = np.maximum(np.subtract(np.max(data.ref), min.ref), 1e-6)
    const normalized = np.trueDivide(np.subtract(data.ref, min), range)
    const indices = np.astype(np.clip(np.multiply(normalized, 255), 0, 255), np.int32)
    const colors = np.take(VIRIDIS_LUT.ref, np.reshape(indices, [-1]), 0)
    const image = np.reshape(colors, [height, width, 3])

    toPixels(image, canvas)
    image.dispose()
  }, [data])

  // small tensors (a 27×32 embedding matrix) capped at their natural width
  // render as postage stamps – upscale by an integer factor to ~256px so the
  // cells stay crisp under image-rendering: pixelated; large grids unchanged
  const displayWidth = width < 256 ? width * Math.ceil(256 / width) : width

  return (
    <canvas
      ref={canvasRef}
      style={{
        width: '100%',
        maxWidth: `${displayWidth}px`,
        aspectRatio: `${aspect}`,
        imageRendering: 'pixelated'
      }}
    />
  )
}

// MARK: IDE environment
// Register the UI components and live sources into the language environment,
// and teach Code literals to render as syntax trees.

setCodeNodePrinter(PrettyPrintSyntaxTree as unknown as (node: SyntaxTreeNode) => Value)

extendEnvironment({
  [Symbol.keyFor(Symbol.for("Documentation"))!]: Text(Documentation),
  CodePrint: PrettyPrintSyntaxTree,

  Button,
  Checkbox,
  Grid,
  ImageUpload,
  Layers,
  MousePosition,
  Point2D,
  Trail,
  Slider,
  Scrubber,

  Text,
  TextEditor,

  Code,
  CodeEditor,

  Fetch,
  LoadSafeTensorFromURL,
  LoadTensorFromImageUrl,
  Camera,
  Microphone,
  MicrophoneSpectrum,
  SampleRate,
  Time,

  Print,
  PrettyPrint,
} as Record<string, Value>)

// MARK: Examples

const EXAMPLES = {
  "no-precedence": `
; Left-to-right evaluation, no operator precedence
1 + 2 * 3 - 4 / 5 ^ sin(2)
`,
  "whitespace-precedence": `
; Whitespace IS precedence
(
    1 + 2 * 3,       ; spaced: left-to-right = 9
    1 + 2*3,         ; glued * binds first = 7
    a: 1 + 2,        ; glued ':' takes the whole right side
    θ: [3, 4],
    √(θ_0^2 + θ_1^2) ; tight chains: (θ_0)^2 + (θ_1)^2
)
`,
  "infix-lambda": `
; Using lambda as infix operator
1 {x,y | x+y} 2
`,
  "broadcasting": `
; Tensor broadcasting - scalar added to vector
[1,2,3] + 1
`,
  "vector-addition": `
; Element-wise vector addition
a:[1,2,3], b:[4,5,6], a+b
`,
  "calling-conventions": `
; Multiple ways to call functions
(
    1 + 2,
    1 add 2,
    add(1,2),
    +(1,2),
    (1,2) . +,
    (1,2) apply add,
)
`,
  "max-variations": `
; Different ways to use max function
(
    (⌈): max,
    max(2,3),
    2 max 3,
    (2,3) . max,
    max(max(1,2), 3),
    1 max 2 max 3,
    1 ⌈ 2 ⌈ 3
)
`,
  "gradient": `
; Automatic differentiation with ∇
f : { x | x ^ 2 },
g : ∇(f),
x : (1 :: 10),
(f(x), g(x))
`,
  "ad-hoc-operators": `
; Defining custom operators
(++): ListConcat,
(1, 2) ++ (3, 4)
`,
  "combinators": `
; Combinators – the songbirds of tacit programming.
; Smullyan named them after birds ("To Mock a Mockingbird")
; honoring Haskell Curry, logician and avid birdwatcher.

Grid(3)(
    ("Queer bird",      \`g(f(x))\`,           (+ ∘ √)(9, 16)          ),
    ("Cardinal",        \`f(y, x)\`,           3 (⍨ -) 10              ),
    ("Warbler",         \`f(x, x)\`,           (⍨ ×)(9)                ),
    ("Psi",             \`f(g(x), g(y))\`,     -3 (abs ⍥ =) 3          ),
    ("Starling",        \`f(x, g(x))\`,        (÷ ⟜ √)(16)             ),
    ("Dove",            \`f(x, g(y))\`,        16 (÷ ⟜ √) 4            ),
    ("Violet Starling", \`g(f(x), x)\`,        (√ ⊸ ÷)(16)             ),
    ("Zebra Dove",      \`g(f(x), y)\`,        9 (√ ⊸ ÷) 3             ),
    ("Phoenix",         \`g(f(x), h(x))\`,     Φ(Σ, ÷, #)([1, 2, 3, 4])),
    ("Pheasant",        \`g(f(x,y), h(x,y))\`, 5 Φ(=, ∨, >) 3          ),
    ("Kestrel",         \`x\`,                 3 ⊣ 5                   ),
    ("Kite",            \`y\`,                 3 ⊢ 5                   ),
)
`,
  "lerp": `
; Linear interpolation with custom operators
; lerp: 0.5 ≻ [10, 30] = 20
≻ : { t, v | (v_0 × (1 - t)) + (v_1 × t) },

; invlerp: [1, 3] ≺ 2 = 0.5
≺ : { v, t | (t - (v_0)) ÷ ((v_1) - (v_0)) },

normalize : { x | [min(x), max(x)] ≺ x },
[10, 20, 30] . normalize
`,
  "reactive-slider": `
; Reactive slider with computed value
(
  x: $(0.5),
  Slider(x),
  x ^ 2
)
`,
  "reactive-function-plot": `
; Interactive sine wave - adjust resolution with slider

PI: 3.14159,
resolution: $(0.5),
points: (resolution × 90 + 10),
x: linspace([-PI, PI], points),

(Slider(resolution), sin(x))
`,
  "tasks": `
; TODO App - reactive task management

; Helper operators
(++): ListConcat,
(++=): FunctionNoAutoLift({ a, b | a(a() ++ List(b)) }),

; Task factory - creates editable task with toggle
task-create: { name |
    text: $(name),
    done: $("🔴"),
    Grid([1, 9])(
        Button(done, { done ← "✅" }),
        TextEditor(text),
    )
},

; App state
task-name: $(""),
tasks: $(List()),

; UI
(
    Text("# TODO App"),
    Grid([5, 1])(
        TextEditor(task-name),
        Button("Add Task", {
            tasks ++= task-create(task-name()),
            task-name ← "",
        }),
    ),
    tasks,
)
`,
  "tasks-compressed": `
(++):ListConcat,task-name: $(""),task-create:{name|s: $(name),f: $("🔴"),Grid([1,10])(Button(f,{f("✅")}),TextEditor(s))},tasks: $(()),(Text("# TODO"),Grid([5,1])(TextEditor(task-name),Button("Add",{tasks(tasks()++List(task-create(task-name()))),task-name("")})),tasks)
`,
  "tasks-mini": `
; Minimal TODO app
(++): ListConcat,
n: ($""),
tasks: ($ List()),
(
    TextEditor(n),
    Button("Add",{
        tasks(tasks() ++ List(n())),
        n("")
    }),
    tasks
)
`
  ,
  "calculator": `
; Calculator - builds expression string and evaluates it

(++): StringConcat,

expr: $(""),
result: (expr . CodeEvaluate),
append: { f | { expr(expr() . f) } },
btn: { c | Button(c, { x | x ++ c } . append) },

(
    TextEditor(expr),
    result,
    Grid([2,2,2,1])(
        btn("7"), btn("8"), btn("9"), btn(" ÷ "),
        btn("4"), btn("5"), btn("6"), btn(" × "),
        btn("1"), btn("2"), btn("3"), btn(" - "),
        Button("C", { "" } . append), btn("0"), btn("."), btn(" + "),
    ),
)
`,
  "linear-regression": `
; Linear Regression with gradient descent
; Find θ that minimizes (f(x) - y)²

; Operators
(++): TensorConcat,

; Data: y = 0.23x + 0.47
x: 0 :: 10,
y: x×0.23 + 0.47,

; Model: f(x) = θ₀·x + θ₁
θ: ~([0, 0]),
f: { x | x × θ_0 + θ_1 },

; Loss: mean squared error
𝓛: { mean((f(x) - y)^2) },

; Training
opt: adam(0.03),
losses: $([]),

{ losses(losses() ++ [opt(𝓛)]) } ⟳ 200,

; Results
(
    Text("**Loss:**"), losses,
    Text("**θ (learned):**"), θ,
    Text("**θ (target):**"), [0.23, 0.47],
)
`,
  "linear-regression-compressed": `
x: (0 :: 10),
y: (x × 0.23 + 0.47),
θ: ~([0, 0]),
f: { x | x × (θ_0) + (θ_1) },
𝓛: { μ((y - f(x)) ^ 2) },
minimize: adam(0.03),
losses: $([]),
{ losses(losses() concat [minimize(𝓛)]), } ⟳ 400,
(losses, θ)`,
  "REPL": `
; Multi-cell REPL - each cell evaluates independently
; BUG: CodeEditor's height doesn't auto-adjust on content change

cell: {
    code: $("1 + 1"),
    result: CodeEvaluate(code),
    Grid(1)(Print(result), CodeEditor(code))
},

(cell(), cell(), cell())
`,
  "exponential-decay": `
; Exponential decay: f(t) = b + c·e^(-k·t)
; Interactive visualization with slider

b: 0.1,   ; baseline
c: 1,     ; amplitude
k: 5,     ; decay rate

f: { t | b + (c × exp(-(k) × t)) },

; Interactive plot
t: $(0),
(
  Slider(t),
  f(t),
  f(0::100 / 100)
)
`,
  "deep-delta-learning": `
; Deep Delta Learning - learns sin(x)
; Uses delta rule: x' = x + β·k·(v - k·x)

d: 160,

; Learnable parameters
w: ~(randn([d]) × 0.1),
κ: ~(randn([d]) × 0.1),
ν: ~([0]),
β: ~([0]),

; Activation & delta block
σ: { z | 1 ÷ (1 + exp(-z)) },
Δ: { x |
    k: κ ÷ √(Σ(κ^2) + 1e-7),
    x + (2 × σ(β) × (ν - Σ(k × x)) × k)
},

; Model & data
f: { t | Δ(t × w) },
τ: linspace([0, 6.28], d),
ŷ: sin(τ),

; Training
(++): TensorConcat,
(++=): FunctionNoAutoLift({ a, b | a(a() ++ b) }),

𝓛: { mean((f(τ) - ŷ)^2) },
opt: adam(0.001),
losses: $([]),
pred: $(f(τ)),

{ losses ++= [opt(𝓛)], pred(f(τ)) } ⟳ 500,

; Results
(
    Text("**Loss:**"), losses,
    Text("**Prediction vs Target:**"), (ŷ, pred),
)
`,
"loss-landscape": `
; Touch the Loss Landscape
; ONE loss definition drives both the surface and the descent – edit it live!

𝓛: { p | (p_0^2 + p_1 - 11)^2 + (p_0 + p_1^2 - 7)^2 },   ; Himmelblau – four minima

θ: ~([0, 0]),   ; the ball – grab it!
lr: $(0.4),
step: { sgd(once(lr) × 0.005)({ 𝓛(θ) }) },

; the surface: the same 𝓛 evaluated over the whole view via ⊗
n: 300,
range: [[-5, 5], [-5, 5]],
surface: log(linspace(range_1, n) (⊗ { y, x | 𝓛([x, y]) }) linspace(range_0, n) + 1),

(
  Text("# Touch the Loss Landscape"),
  Grid([1, 5])(Text("lr"), Slider(lr)),
  Button("Descend ×100", { step ⟳ 100 }),
  Layers(surface, Trail(θ, range), Point2D(θ, range)),
  𝓛(watch(θ)),   ; live loss – updates while you drag
)
`,
"optimizer-race": `
; ⚔️ adam vs sgd – same start, same landscape, different characters

𝓛: { p | (p_0^2 + p_1 - 11)^2 + (p_0 + p_1^2 - 7)^2 },   ; Himmelblau

a: ~([0, -0.3]),   ; adam, orange
s: ~([0, -0.3]),   ; sgd, blue
opt: adam(0.08),
step: { opt({ 𝓛(a) }), sgd(0.002)({ 𝓛(s) }) },

n: 300,
range: [[-5, 5], [-5, 5]],
surface: log(linspace(range_1, n) (⊗ { y, x | 𝓛([x, y]) }) linspace(range_0, n) + 1),

(
  Text("# ⚔️ adam vs sgd"),
  Button("Race ×300", { step ⟳ 300 }),
  Layers(
    surface,
    Trail(a, range, "orange"), Trail(s, range, "deepskyblue"),
    Point2D(a, range, "orange"), Point2D(s, range, "deepskyblue"),
  ),
  Grid(2)(Text("**adam**"), Text("**sgd**")),
  Grid(2)(𝓛(watch(a)), 𝓛(watch(s))),
)
`,
"magnets-simulation": `
; Animated Magnetic Field

(≻): { t, v | (v_0 × 1-t) + (v_1 × t) },
(≺): { v, t | (t - v_0) / (v_1 - v_0) },
normalize: { x | [min(x), max(x)] ≺ x },

numMagnets: 3,
n: 700,

; Grid
xs: linspace([-2, 2], n),
ys: linspace([-1.5, 1.5], n),
X: xs ⍴ [n, 1] tile [1, n],
Y: ys ⍴ [1, n] tile [n, 1],

; Magnet positions using lerp
rawPos: rand([numMagnets, 2]),
posT: transpose(rawPos),
magX: posT_0 ≻ [-1.5, 1.5],
magY: posT_1 ≻ [-1, 1],

; Animated angles
t: Time(),
baseAngles: rand([numMagnets]) × 6.28,
speeds: rand([numMagnets]) ≻ [0.1, 0.5],
angles: baseAngles + (t × speeds),

; Pole positions
d: 0.12,
nPoleX: magX + d×cos(angles),
nPoleY: magY + d×sin(angles),
sPoleX: magX - d×cos(angles),
sPoleY: magY - d×sin(angles),

; Reshape for broadcasting
X3: X ⍴ [n, n, 1],
Y3: Y ⍴ [n, n, 1],
nX3: nPoleX ⍴ [1, 1, numMagnets],
nY3: nPoleY ⍴ [1, 1, numMagnets],
sX3: sPoleX ⍴ [1, 1, numMagnets],
sY3: sPoleY ⍴ [1, 1, numMagnets],

; Distances
ε: 0.0001,
rN: √(((X3 - nX3)^2) + ((Y3 - nY3)^2)) + ε,
rS: √(((X3 - sX3)^2) + ((Y3 - sY3)^2)) + ε,

; Potential
;k: 0.5,
; Pulsing strength
k: sin(t × 2) × 0.2 + 0.5,
potential: sum(k/rN - k/rS, 2),

; Visualization
lines: abs(sin(potential × 25)) ^ 0.25,
glow: 1 / (abs(potential) + 0.2),
field: (lines × 0.7) + (glow × 0.3),

(
  Text("# 🧲 Spinning Magnets"),
  lines,
)
`,
"magnets-minimal": `
; Animated Magnetic Field – minimal
; a magnet is two opposite charges; ⊗ is the meshgrid

(≻): { t, v | (v_0 × 1-t) + (v_1 × t) },  ; lerp

m: 3,    ; magnets
n: 600,  ; resolution
t: Time(),

angles: rand([m])×6.28 + t×(rand([m]) ≻ [0.1, 0.5]),
dir: stack(cos(angles), sin(angles)) × 0.12,   ; [2 m] headings
pos: rand([2, m]) ≻ [-1.4, 1.4],               ; [2 m] centers
pole: concat((pos + dir, pos - dir), 1),       ; [2 2m] all six poles
q: concat(fill([m], 1), fill([m], -1)),        ; charge of each pole

grid: linspace([-2, 2], n),
dx: grid (⊗ -) pole_0,          ; [n 2m]
dy: grid (⊗ -) pole_1,          ; [n 2m]
r: √(dx^2 (+ ⊗ 1) dy^2) + 1e-4, ; [n n 2m] – frames cross, poles zip

k: sin(t × 2) × 0.2 + 0.5,   ; pulsing strength
potential: sum(q×k / r, 2),

(
  Text("# 🧲 Spinning & Pulsating Magnets"),
  abs(sin(potential × 25)) ^ 0.25,
)
`,
"mandelbrot": `
; Mandelbrot set – z ← z² + c until |z| escapes
; complex numbers as (re, im) pairs, ⍣ iterates the whole plane at once
; scrub the depth and watch the set sharpen

w: 350,        ; resolution, 7:5 like the view
h: 250,
depth: $(40),  ; iterations – drag me

re: linspace([-2.5, 1], w) ⍴ [1, w] tile [h, 1],
im: linspace([-1.25, 1.25], h) ⍴ [h, 1] tile [1, w],

; one step for every pixel: state is (zx, zy, escape count)
step: { s |
  zx: ListGet(s, 0),
  zy: ListGet(s, 1),
  k:  ListGet(s, 2),
  alive: ((zx^2 + zy^2) ≤ 4),
  (
    where(alive, zx^2 - zy^2 + re, zx),
    where(alive, 2×zx×zy + im, zy),
    k + alive,
  )
},

zero: re × 0,
render: { d |
  n: (1 ⌈ d),
  counts: ListGet((step ⍣ n)((zero, zero, zero)), 2),
  √(counts ÷ n)
},

(
  Text("# 🌀 Mandelbrot"),
  Grid([1, 9])(Text("**depth:**"), Scrubber(depth)),
  render(depth),
)
`,
"game-of-life": `
; Conway's Game of Life

; grid size
n: 256,

; a cell plus its two neighbours along one axis
box: { g, ax | roll(g, 1, ax) + g + roll(g, -1, ax) },

; the 3×3 sum, minus the cell = its 8 neighbours
neighbours: { g | box(box(g, 0), 1) - g },

; random soup
board: $(rand([n, n]) < 0.3),

step: {
  c: once(board),
  a: neighbours(c),

  ; alive next if 3 neighbours, or 2 and already alive
  board ← ((a = 3) ⌈ ((a = 2) × c)),
},

step ⟳ 100000,

(
  Text("# 🦠 Game of Life"),
  board,
)
`,
  "reaction-diffusion": `
; Gray–Scott reaction–diffusion – two chemicals paint Turing patterns.
; v eats u and reproduces (u × v²); both diffuse; explicit Euler.

n: 200,
edges: { x, ax | roll(x, 1, ax) + roll(x, -1, ax) },   ; both neighbours along an axis
Δ: { x | edges(x, 0) + edges(x, 1) - 4×x },             ; the Laplacian

; sprinkle v into a sea of u, then let it self-organise
u: $(1),
v: $(0),
reseed: { s: rand([n, n]) < 0.08 × 0.5, u← 1 - s, v← s },

; drag feed and kill – the whole zoo of patterns hides in a tiny window
F: $(0.055),
K: $(0.062),

tick: {
  a: once(u), b: once(v), f: once(F), k: once(K),
  r: a × b^2,
  u← a + 0.16×Δ(a) - r + f - f×a,
  v← b + 0.08×Δ(b) + r - f×b - k×b,
},

reseed(),
tick ⟳ 100000,

(
  Text("# 🌸 Reaction–Diffusion"),
  Grid([1, 6])(Text("**feed**"), Scrubber(F, 0.002)),
  Grid([1, 6])(Text("**kill**"), Scrubber(K, 0.002)),
  Button("reseed", reseed),
  v,
)
`,
  "lenia": `
; Lenia – continuous cellular automata (Bert Chan). A ring kernel convolves the
; world; a bell-curve growth rule nudges each cell. Smooth, life-like blobs emerge.

n: 128,
R: 13,

; a Gaussian ring kernel, normalised to sum 1
ks: 2×R + 1,
gx: (0 :: ks) ⍴ [ks, 1] tile [1, ks],
gy: (0 :: ks) ⍴ [1, ks] tile [ks, 1],
dist: √((gx - R)^2 + (gy - R)^2) ÷ R,
ring: exp(0 - (dist - 0.5)^2 ÷ (2 × 0.15^2)) × (dist < 1),
K: ring ÷ Σ(ring),

; growth: a bell centred at μ, width σ, mapped to [-1, 1]
μ: 0.15,
σ: 0.017,
growth: { u | 2 × exp(0 - (u - μ)^2 ÷ (2 × σ^2)) - 1 },

world: $(0),
reseed: { world ← rand([n, n]) },

; convolve, grow, and keep the world in [0, 1]
tick: {
  w: once(world),
  world ← clamp(w + 0.1×growth(conv(K, w)), 0, 1),
},

reseed(),
tick ⟳ 100000,

(
  Text("# 🔬 Lenia"),
  Button("reseed", reseed),
  world,
)
`,
  "attention": `
; Scaled dot-product attention, built by hand – the core of a transformer.
; scores = Q·Kᵀ / √d,  weights = softmax(scores).  Drag the temperature knob
; and watch every token's attention go from uniform to one-hot, live.

n: 48,   ; tokens
d: 16,   ; embedding dimension

; sinusoidal positional embeddings, so nearby tokens are similar (K = Q)
pos: 0 :: n,
k: 0 :: (d ÷ 2),
freqs: 1 ÷ (10000 ^ (k ÷ (d ÷ 2))),
angles: pos (⊗ ×) freqs,
Q: concat((sin(angles), cos(angles)), 1),

; every query dotted with every key, scaled
scores: matmul(Q, transpose(Q)) ÷ √(d),

; temperature sharpens (or flattens) the softmax – recomputes as you drag
temp: $(4),
weights: $({ softmax(scores × temp()) }),

(
  Text("# 🧠 Attention"),
  Grid([1, 6])(Text("**temperature**"), Scrubber(temp, 0.5)),
  Text("who attends to whom:"),
  weights,
)
`,
  "b-type": `
; Turing's B-type unorganised machine (1948), educated by gradient descent.
; A network of soft NAND-ish gates learns XOR – which isn't in its toolbox,
; so it must compose it (XOR = OR ∧ ¬AND). "Education" is just backprop.

(++): TensorConcat,

a: [0, 0, 1, 1],
b: [0, 1, 0, 1],
target: [0, 1, 1, 0],

; 12 two-input boolean functions – AND, OR, NAND, NOT, … but no XOR
basis: { p, q | stack((
  p×q, p + q - p×q, 1 - p×q, 1 - (p + q - p×q),
  p×(1 - q), (1 - p)×q, p, q, 1 - p, 1 - q, p×0, p×0 + 1
)) },

; each gate's connection-switch: a learnable softmax over the 12 functions
t1: ~(randn([12]) × 0.3),
t2: ~(randn([12]) × 0.3),
to: ~(randn([12]) × 0.3),

; two hidden gates read the inputs; the output gate reads the hidden gates
fwd: {
  h1: matmul(softmax(t1), basis(a, b)),
  h2: matmul(softmax(t2), basis(a, b)),
  matmul(softmax(to), basis(h1, h2))
},
loss: { mean((fwd() - target)^2) },

opt: adam(0.15),
losses: $([]),
{ losses(losses() ++ [opt(loss)]) } ⟳ 400,

; recompute each training step (subscribe to losses) so the display stays live
output: $({ losses(), fwd() }),
gates: $({ losses(), stack((softmax(t1), softmax(t2), softmax(to))) }),

(
  Text("# 🎓 B-type learns XOR"),
  Text("**loss** – education in progress:"),
  losses,
  Text("**output** vs target (0, 1, 1, 0):"),
  output,
  Text("the three gates' switches, converging on OR · AND · (p ∧ ¬q):"),
  gates,
)
`,
  "mnist": `
; 🔢 A differentiable logic-gate network learns MNIST – the B-type machine,
; scaled up. 512 → 200 soft boolean gates wired at random over 196 pixels;
; each gate's identity (which of the 16 two-input truth tables it computes) is
; a learnable softmax, educated by gradient descent. No neurons, just logic.

(++): TensorConcat,

; --- data: 14×14 binarised MNIST (6000 train, 1000 test) as safetensors ---
; the signal is an empty list until the fetch resolves to [Xtrain,Ytrain,Xtest,Ytest]
data: LoadSafeTensorFromURL("mnist.safetensors"),
loaded: { ListLength(data()) > 0 },
; train on 3000 of the 6000, score on 500 of the 1000 – keeps each frame light
; so the whole thing trains live and smoothly in the browser (pixels × examples)
Xtr: { transpose(slice(ListGet(ListGet(data(), 0), 1), 0, 3000)) },   ; [196, 3000]
Ytr: { transpose(slice(ListGet(ListGet(data(), 1), 1), 0, 3000)) },   ; [10, 3000]
Xte: { transpose(slice(ListGet(ListGet(data(), 2), 1), 0, 500)) },    ; [196, 500]
Yte: { transpose(slice(ListGet(ListGet(data(), 3), 1), 0, 500)) },    ; [10, 500]

; --- fixed random wiring: two source signals per gate ---
G1: 512, G2: 200,
IA1: floor(rand([G1]) × 196),  IB1: floor(rand([G1]) × 196),   ; layer 1 reads pixels
IA2: floor(rand([G2]) × G1),   IB2: floor(rand([G2]) × G1),    ; layer 2 reads layer 1

; the 16 two-input truth tables (row i = binary of i = [f00, f01, f10, f11])
TT: [
  [0,0,0,0], [0,0,0,1], [0,0,1,0], [0,0,1,1],
  [0,1,0,0], [0,1,0,1], [0,1,1,0], [0,1,1,1],
  [1,0,0,0], [1,0,0,1], [1,0,1,0], [1,0,1,1],
  [1,1,0,0], [1,1,0,1], [1,1,1,0], [1,1,1,1]
],
; pool 20 gates per class: GROUP[c, g] = 1 where gate g belongs to class c
GROUP: (0 :: 10) ⊗(=) floor((0 :: G2) ÷ (G2 ÷ 10)),

; a soft gate: expected truth table softmax(θ)·TT, blended over the 4 input corners
gate: { A, B, W | Wt: transpose(W),
  reshape(Wt_0, [#A, 1])×((1 - A)×(1 - B)) + reshape(Wt_1, [#A, 1])×((1 - A)×B)
  + reshape(Wt_2, [#A, 1])×(A×(1 - B)) + reshape(Wt_3, [#A, 1])×(A×B)
},
sm: { th | matmul(softmax(th), TT) },

; the learnable parameters: each gate's softmax logits over the 16 functions
t1: ~(randn([G1, 16]) × 0.1),
t2: ~(randn([G2, 16]) × 0.1),

model: { X |
  h1: gate(gather(X, IA1), gather(X, IB1), sm(t1)),
  h2: gate(gather(h1, IA2), gather(h1, IB2), sm(t2)),
  matmul(GROUP, h2)                                   ; [10, examples] logits
},

; cross-entropy over the 10 classes (numerically-stable log-softmax on axis 0)
logp: { L | L - log(Σ(exp(L), 0)) },
loss: { -mean(Σ(Ytr() × logp(model(Xtr())), 0)) },

; train – each tick is gated on the data being loaded (a no-op 0 until then)
opt: adam(0.05),
losses: $([]),
{ losses(losses() ++ [cascade((guard(loaded(), { opt(loss) }), { 0 }))()]) } ⟳ 1500,

; live accuracy on the held-out test set, recomputed every training step
accuracy: $({ losses(), cascade((guard(loaded(), {
  hits: argmax(model(Xte()), 0) = argmax(Yte(), 0),
  round(mean(hits) × 1000) ÷ 10
}), { 0 }))() }),

(
  Text("# 🔢 Logic gates learn MNIST"),
  Text("A network of soft boolean gates – no neurons – reads handwritten digits."),
  Text("**test accuracy** (%), on digits it never trained on:"),
  accuracy,
  Text("**training loss**:"),
  losses,
)
`,
  "dreamer": `
; 🔮 The Name Dreamer – a 12k-parameter transformer invents names, live.
; The corpus lives in a data slot (~~): the compiled training step receives it
; as an argument each step – so the step stays jit-hot, and pausing, editing
; the text and resuming re-trains on YOUR data without resetting the weights.
; Gradients flow into ~, never into ~~.

(++): TensorConcat,

; --- vocabulary: ' ' → 0, a…z → 1…26 (anything else collapses to space) ---
V: 27, T: 8, C: 32, F: 96,
enc: { s | clamp(StringToCodes(s) - 96, 0, 26) },
dec: { t | CodesToString(where(t = 0, 32, t + 96)) },

; --- the entire dataset is this editable string ---
corpus: $("emma olivia ava isabella sophia charlotte mia amelia harper evelyn abigail emily elizabeth mila ella avery sofia camila aria scarlett victoria madison luna grace chloe penelope layla riley zoey nora lily eleanor hannah lillian addison aubrey ellie stella natalie zoe leah hazel violet aurora savannah audrey brooklyn bella claire skylar lucy paisley everly anna caroline nova genesis emilia kennedy samantha maya willow kinsley naomi aaliyah elena sarah ariana allison gabriella alice madelyn cora ruby eva serenity autumn adeline hailey gianna valentina isla eliana quinn ivy sadie piper lydia alexa josephine emery julia delilah arianna vivian kaylee sophie brielle madeline"),
load: { c | windows(T + 1, enc(c)) },       ; every 9-char window is an example
D: ~~(load(once(corpus))),
SignalEffect({ D := load(corpus()) }),      ; corpus edits reload the slot – the weights stay

; --- the transformer: one pre-norm block, one head ---
E:  ~(randn([V, C]) × 0.1),  P:  ~(randn([T, C]) × 0.1),
Wq: ~(randn([C, C]) × 0.2),  Wk: ~(randn([C, C]) × 0.2),
Wv: ~(randn([C, C]) × 0.2),  Wo: ~(randn([C, C]) × 0.2),
W1: ~(randn([C, F]) × 0.2),  W2: ~(randn([F, C]) × 0.1),
Wu: ~(randn([C, V]) × 0.1),

rms: { x | x ÷ √(mean(x × x, -1, 1) + 0.001) },   ; the 1 keeps the reduced axis
gelu: { x | x × sigmoid(x × 1.702) },
nmask: where((0 :: T) ⊗(≥) (0 :: T), fill([T, T], 0), fill([T, T], -9999)),
att: { x | q: matmul(x, Wq), k: matmul(x, Wk), v: matmul(x, Wv),
  a: softmax((einsum("btc,bsc->bts", q, k) ÷ √(C)) + nmask),
  matmul(matmul(a, v), Wo) },
model: { ids | x0: matmul(oneHot(ids, V), E) + P,
  x1: x0 + att(rms(x0)),
  x2: x1 + matmul(gelu(matmul(rms(x1), W1)), W2),
  matmul(rms(x2), Wu) },   ; [batch, T, V] logits

loss: { crossEntropy(oneHot(slice(D, [0, 1], [-1, T]), V), model(slice(D, [0, 0], [-1, T]))) },

; --- sampling: gumbel-max, with a temperature you can drag ---
codesF: linspace([0, V - 1], V),   ; float 0…26 – argmax is int, gather keeps it float
τ: $(0.5),
pick: { l | codesF _ argmax((l ÷ ((once(τ) × 1.5) ⌈ 0.05)) - log(-log(rand([V]))), -1) },
grow: { s | ctx: s _ ((0 :: T) + (#(s) - T)),
  l: (model(ctx ⍴ [1, T]) ⍴ [T, V]) _ (T - 1),
  s ++ (pick(l) ⍴ [1]) },
gen: { seed, n | (grow ⍣ n)(seed) },
dream: $("### …one moment, it is waking up…"),
dreamNow: { dream(StringConcat("### ", dec(slice(gen(fill([T], 0), 48), T)))) },

; --- training: ⟳-paced, gated by the checkbox ---
training: $(1),
opt: adamw(0.03, 0.001),   ; a little weight decay – memorize less, invent more
losses: $([]),
stride: $(4),
{ i |
  when(once(training), {
    l: opt(loss),
    when((i % once(stride)) = 0, {
      losses(losses() ++ (l ⍴ [1])),
      ; the plot keeps the WHOLE story, bounded: when it fills up,
      ; thin the history to every other point and log half as often
      when((#(losses())) ≥ 480, {
        losses(losses() _ ((0 :: 240) × 2)),
        stride ← (once(stride) × 2)
      })
    }),
    when((i % 200) = 0, dreamNow)
  })
} ⟳ 1000000,

; type a beginning – the model finishes it (works even while paused)
prompt: $("kri"),
finish: $({ dream(), p: prompt(), s: fill([T], 0) ++ enc(p),
  StringConcat("## ", p, "·", dec(slice(gen(s, 10), #(s)))) }),

; what the head attends to while reading a name, live
probeS: fill([T], 0) ++ enc("sophia"),
probe: probeS _ ((0 :: T) + (#(probeS) - T)),
attn: $({ losses(),
  x: rms(matmul(oneHot(probe ⍴ [1, T], V), E) + P),
  softmax((einsum("btc,bsc->bts", matmul(x, Wq), matmul(x, Wk)) ÷ √(C)) + nmask) ⍴ [T, T] }),

editable: $({ 1 - training() }),
(
  Text("# 🔮 The Name Dreamer"),
  Text("A 12k-parameter transformer learns to invent names – live, in your tab."),
  Checkbox(training),
  Text("**training** – uncheck to pause; pause to edit the corpus below"),
  Text("**loss** – the whole run, from first step to now:"),
  losses,
  Text("**it dreams** – regenerated every 200 steps:"),
  Text(dream),
  Button("dream again", dreamNow),
  Text("**finish my name:**"),
  TextEditor(prompt),
  Text(finish),
  Text("**temperature:**"),
  Slider(τ),
  Text("**attention** while it reads “sophia” (row attends to column):"),
  attn,
  Text("**the letter embeddings**, learning:"),
  E,
  Text("**the training data** – uncheck training to edit; resuming keeps the weights:"),
  TextEditor(corpus, editable),
)
`,
  "camera-edges": `
; 📷 Live edge detection – your camera through a Laplacian kernel.
; Allow camera access, then wave: flat areas cancel out, edges glow.

cam: Camera(320, 240),
k: [[0, 1, 0], [1, -4, 1], [0, 1, 0]],

; every frame: grayscale the image (mean over the color axis), convolve
edges: $({ abs(conv(k, mean(cam(), 2))) }),

(
  Text("# 📷 Live Edges"),
  edges,
)
`,
  "spectrum": `
; Live spectrum analyser – FFT the microphone waveform, plot the magnitude.
; Allow mic access, then whistle or play a tone and watch the peak move.

mic: Microphone(512),

; every frame: take the FFT of the waveform, then its magnitude √(re² + im²).
; fft returns [real; imag] stacked, so f_0 is real and f_1 is imaginary.
spectrum: $({ f: fft(mic()), √((f_0)^2 + (f_1)^2) }),

(
  Text("# 🎵 Live Spectrum"),
  spectrum,
)
`,
  "pitch-detector": `
; Pitch detector – FFT the mic, find the loudest note. Allow mic access,
; then whistle or hum a steady tone and watch the note lock on.

size: 8192,
sr: SampleRate(),
mic: Microphone(size),
names: ("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"),

detect: $({
  f: fft(mic()),
  mag: √(f_0^2 + f_1^2),
  bins: 0 :: #(mag),
  freqs: bins × sr ÷ size,
  ; only the musical range, so mic rumble and hiss don't win
  band: mag × (freqs > 55) × (freqs < 2000),
  ; the loudest bin – kept as a float (argmax would be int)
  peak: Σ(bins × (band = max(band))),
  freq: peak × sr ÷ size,
  midi: round(69 + 12×log2(freq ÷ 440)),
  (
    ListGet(names, midi % 12),
    floor(midi ÷ 12) - 1,
    round(freq),
  )
}),

(
  Text("# 🎸 Pitch Detector"),
  Text("**note · octave · Hz**"),
  detect,
)
`,
  "recursion": `
; Recursive functions: factorial and Fibonacci
fact: { n | 
  f: self,
  cascade((
    guard(n = 0, { 1 }),
    { n * f(n - 1) }
  ))()
},

fib: { n | 
  f: self,
  cascade((
    guard(n = 0, { 0 }),
    guard(n = 1, { 1 }),
    { f(n - 1) + f(n - 2) }
  ))()
},

(
  fact(5),
  fib(10)
)
`

} as const

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

const FLUENT_GENERATION_SYSTEM_PROMPT = `

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
${Object.keys(DefaultEnvironment).join(", ")}

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
      system: FLUENT_GENERATION_SYSTEM_PROMPT,
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

function Code(sourceCode: Signal<string>) {
  return SignalComputed(() => {
    return (
      // @ts-ignore
      <Panel className="!p-0 border-none overflow-hidden h-[5em]">
        {/* @ts-ignore */}
        <Editor
          onMount={editorOnMount}
          height="100%"
          defaultLanguage="fluent"
          theme="fluentThemeReadOnly"
          // Fluent strings are String objects (origin tracking) – Monaco needs a primitive
          value={String(SignalRead(sourceCode) ?? "")}
          onChange={(updatedSourceCode) => { SignalUpdate(sourceCode, updatedSourceCode) }}
          options={getEditorOptions("readonly")}
        />
      </Panel>
    )
  })
}
setMeta(Code, { noAutoLift: true })

function CodeEditor(sourceCode: Signal<string>) {
  const height = SignalCreate("100%")

  // each editor validates its own model (REPL cells stay independent)
  let editorRef: { editor: editor.IStandaloneCodeEditor; monaco: Monaco } | null = null
  const validateCode = (code: string) => {
    const model = editorRef?.editor.getModel()
    if (!editorRef || !model) { return }

    const markers = getParseErrors(code).map(error => ({
      startLineNumber: error.start.line,
      startColumn: error.start.column,
      endLineNumber: error.end.line,
      endColumn: error.end.column,
      message: error.message,
      severity: editorRef!.monaco.MarkerSeverity.Error,
    }))
    editorRef.monaco.editor.setModelMarkers(model, "fluent-syntax", markers)
  }

  const handleEditorMount: OnMount = (editor, monaco) => {
    editorOnMount(editor, monaco)
    editorRef = { editor, monaco }
    // hover highlighting targets the first (main) editor
    if (!mainEditorRef) { mainEditorRef = { editor, monaco } }

    // Validate on initial load
    const model = editor.getModel()
    if (model) {
      validateCode(model.getValue())
    }
  }

  return SignalComputed(() => {
    return (
      // @ts-ignore
      <Editor
        beforeMount={editorBeforeMount}
        onMount={handleEditorMount}
        // @ts-ignore
        height={SignalRead(height)}
        defaultLanguage="fluent"
        className={`${frameStyle} !p-0 overflow-hidden`}
        theme="fluentTheme"
        // Fluent strings are String objects (origin tracking) – Monaco needs a primitive
        value={String(SignalRead(sourceCode) ?? "")}
        onChange={(updatedSourceCode) => {
          SignalUpdate(sourceCode, updatedSourceCode)
          if (updatedSourceCode !== undefined) {
            validateCode(updatedSourceCode)
            // Check for generation comments and process them
            const apiKey = getAnthropicApiKey()
            if (apiKey && GENERATION_COMMENT_REGEX.test(updatedSourceCode)) {
              GENERATION_COMMENT_REGEX.lastIndex = 0 // Reset regex state
              processGenerationComments(updatedSourceCode, apiKey, (newCode) => {
                SignalUpdate(sourceCode, newCode)
                validateCode(newCode)
              })
            }
          }
        }}
        options={getEditorOptions("editable")}
      />
    )
  })
}
setMeta(CodeEditor, { noAutoLift: true })

let EditorInstance: Monaco["editor"] | null = null

async function getEditor() {
  while (EditorInstance === null) {
    await new Promise((resolve) => setTimeout(resolve, 0));
  }
  return EditorInstance;
}

// The token under the cursor: a letter-identifier (Greek included) via Monaco's
// word detection, or an operator run (::, ⌈, ⊗, ∇, ←) expanded by hand – the
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

// Docs live on the built-ins' metadata (attached inline in the prelude via
// doc()); resolve a token through a scope and read them back.
const docScope = createScope()
const metaFor = (token: string | null): FunctionMeta | null => {
  const value = token ? docScope[token] : undefined
  const meta = typeof value === "function" ? getMeta(value) : {}
  return meta.doc ? meta : null
}

// A hover / completion card: signature, one-liner, worked example.
const docCard = (m: FunctionMeta): string =>
  [m.signature && `\`${m.signature}\``, m.doc, m.example && "```fluent\n" + m.example + "\n```"]
    .filter(Boolean).join("\n\n")

// completion's details pane already shows the signature as its header (the
// item's `detail`) – repeating it in the body reads as a glitch
const docCardBody = (m: FunctionMeta): string =>
  [m.doc, m.example && "```fluent\n" + m.example + "\n```"]
    .filter(Boolean).join("\n\n")

const editorBeforeMount: BeforeMount = (monaco) => {
  EditorInstance = monaco.editor

  monaco.languages.register({ id: "fluent" });

  monaco.languages.registerHoverProvider("fluent", {
    provideHover(model, position) {
      const meta = metaFor(fluentTokenAt(model, position))
      return meta ? { contents: [{ value: docCard(meta) }] } : null
    },
  });

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
  const symbolFont = new FontFace("BQN386", 'url("/BQN386.ttf")', {
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

  // Scope includes DefaultEnvironment + PRELUDE
  const completionScope = createScope();
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
    ["≡", "equiv", "Equivalent / Identical", "equiv equivalent identical"],
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
    ["⍣", "power", "Function power / Iterate", "power iterate repeat star"],
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
  monaco.languages.registerCompletionItemProvider('fluent', {
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
      const allKeys: string[] = [];
      for (const key in completionScope) { allKeys.push(key); }

      return ({
        suggestions: [
          ...symbolSuggestionItems,
          ...allKeys.map((key) => {
          const value = completionScope[key];
          const meta = typeof value === 'function' ? getMeta(value) : {};
          return ({
            label: key,
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: key,
            documentation: meta.doc ? { value: docCardBody(meta) } : `Function: ${key}`,
            detail: meta.signature ?? (typeof value === 'function' ? 'function' : String(value).slice(0, 50)),
            range,
            filterText: key,
          })
        }),
        ]
      })
    }
  });
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

// jax-js's WebGPU backend pops error scopes fire-and-forget; when a device or
// instance is dropped mid-flight (re-evaluation teardown, HMR, GPU reset) the
// pending pops reject with nothing to catch them, and the dev overlay buries
// the app under "Instance dropped in popErrorScope". Swallow exactly that
// teardown noise – everything else stays loud. (Upstream: jax-js.)
window.addEventListener("unhandledrejection", (e) => {
  if (String(e.reason).includes("popErrorScope")) { e.preventDefault() }
})

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
        SignalUpdate(code, codeFromUrl);
      } else if (exampleName !== null) {
        SignalUpdate(code, exampleSource(exampleName));
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
  const result = useComputed<Value>(() => {
    return evaluateGeneration(() => {
      const scope = createScope()
      const value = evaluateSyntaxTreeNode(CodeParse(code.value), scope) ?? null
      registerDisposable(() => disposeScopeTensors(scope))
      registerDisposable(() => disposeValueTensors(value))
      return value
    })
  })

  return (
    <>
      <div className="absolute inset-0 z-50 p-2 bg-neutral-900 grid text-white">
        {
          Grid(np.array(2), np.array(1))(
            Print(result),
            CodeEditor(code)
          )
        }
      </div>
    </>
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
