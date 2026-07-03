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
  - optimizers: \`adam\`, \`sgd\`, \`adagrad\`
  - metadata: \`Describe(fn, "doc")\` attaches a doc string, \`Describe(fn)\` queries it
- Ad-hoc operators
  - define custom operators: \`(++): ListConcat, (1, 2) ++ (3, 4)\`

### IDE
- Syntax highlighting
- Live evaluation with error reporting
- Automatic visualization of values (tensors, lists, functions)
- GPU-accelerated tensor operations (via TensorFlow.js and WebGL)
- LLM-backed code generation (BYO Anthropic API key)
- Command palette (Ctrl+P)
- Auto-completion (Ctrl+Space)
- Shareable URL links (Ctrl+S)
- examples gallery (Ctrl+O)
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

import { Signal, computed } from "@preact/signals-core"
import {
  CodeParse, getParseErrors, evaluateSyntaxTreeNode, CodeEvaluate,
  createScope, PRELUDE, DefaultEnvironment, extendEnvironment, setCodeNodePrinter,
  evaluateGeneration, registerDisposable, disposeScopeTensors, disposeValueTensors,
  getAsSyncList, getMeta, setMeta, setOrigin, getOrigin,
  SignalCreate, SignalComputed, SignalRead, SignalUpdate, SignalOnce,
  identifierRegexp, numberRegexp, stringRegexp, operatorRegexp, delimiterRegexp,
  type Value, type CurrentScope, type SyntaxTreeNode, type Origin,
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
import * as tf from '@tensorflow/tfjs'
import { interpolateViridis } from 'd3-scale-chromatic'
import { rgb } from 'd3-color'


// Web Worker setup for monaco-editor
(globalThis as { MonacoEnvironment?: typeof MonacoEnvironment }).MonacoEnvironment = {
  getWorker: () => new Worker(new URL("monaco.worker.js", document.baseURI), { type: "module" }),
}

// Configure @monaco-editor/react to use local monaco-editor package
loader.config({ monaco })

// Import tfjs backend dynamically to prevent tree-shaking
await import('@tensorflow/tfjs-backend-webgl')
await tf.setBackend('webgl')

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

const TextEditor = (editedValue: Signal<string>) => {
  return SignalComputed(() =>
    <div className="grid">
      <textarea
        value={editedValue.value}
        onChange={(e) => SignalUpdate(editedValue, e.target.value)}
        className="block bg-neutral-800 focus:bg-neutral-900 rounded-xl border border-neutral-600 hover:border-neutral-500 focus:border-neutral-300 outline-none p-2 field-sizing-content"
        // @ts-ignore
        style={{ fieldSizing: "content" }}
        rows={1}
      />
    </div>
  )
}
setMeta(TextEditor, { noAutoLift: true })

const Slider = (editedValue: Signal<tf.Tensor>) => {
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
          onChange={(e) => SignalUpdate(editedValue, tf.scalar(parseFloat(e.target.value)))}
          className="bg-neutral-900 focus:bg-neutral-800 rounded-xl border border-neutral-800 hover:border-neutral-700 focus:border-neutral-600 outline-none p-2 w-full h-2 cursor-pointer dark:bg-gray-700 place-self-center"
        />
      </div>
    )
  })
}
setMeta(Slider, { noAutoLift: true })

const Scrubber = (editedValue: Signal<tf.Tensor>, sensitivity?: tf.Tensor) => {
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
        SignalUpdate(editedValue, tf.scalar(Math.round(raw * factor) / factor))
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

const Checkbox = (editedValue: Signal<tf.Tensor>) => {
  return SignalComputed(() => {
    const checked = (getAsSyncList(editedValue?.value) as number) >= 0.5
    return (
      <input
        type="checkbox"
        checked={checked}
        onChange={(e) => SignalUpdate(editedValue, tf.scalar(e.target.checked ? 1 : 0))}
        className="w-5 h-5 accent-white cursor-pointer place-self-start"
      />
    )
  })
}
setMeta(Checkbox, { noAutoLift: true })

// File picker that writes the picked image into `target` as a [h, w, 3] tensor
const ImageUpload = (target: Signal<tf.Tensor>) => {
  return (
    <input
      type="file"
      accept="image/*"
      className="text-sm file:bg-neutral-900 file:hover:bg-neutral-800 file:rounded-xl file:border file:border-neutral-400 file:text-white file:px-3 file:py-1.5 file:mr-2 file:cursor-pointer"
      onChange={async (e) => {
        const file = e.target.files?.[0]
        if (!file) { return }
        const bitmap = await createImageBitmap(file)
        SignalUpdate(target, tf.browser.fromPixels(bitmap))
      }}
    />
  )
}
setMeta(ImageUpload, { noAutoLift: true })

// Mouse position as a [2] tensor signal, normalized to the viewport (0..1)
function MousePosition(): Signal<tf.Tensor> {
  const s = SignalCreate<tf.Tensor>(tf.tensor([0.5, 0.5]))
  const onMove = (e: PointerEvent) => {
    const old = s.peek()
    s.value = tf.tensor([e.clientX / window.innerWidth, e.clientY / window.innerHeight])
    if (old) { old.dispose() }
  }
  window.addEventListener("pointermove", onMove)
  const result = computed(() => s.value) as Signal<tf.Tensor> & { dispose: () => void }
  result.dispose = () => window.removeEventListener("pointermove", onMove)
  registerDisposable(result.dispose)
  return result
}

const Grid = (cols: tf.Tensor, rows: tf.Tensor) => {
  let gridTemplateColumns = ""
  let gridTemplateRows = ""

  const colsValue = getAsSyncList(cols)
  switch (cols.rank) {
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


    switch (rows.rank) {
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
  const applyChildren = (...args: any[]) => {
    const buildCells = () => flattenCells(args.map(a => isListSignal(a) ? (a as Signal<unknown>).value : a))
      .map(WrapWithPrintIfNotReactElement)
    return (
      <div className={`grid gap-2 overflow-scroll h-full`} style={{ gridTemplateColumns, gridTemplateRows }}>
        {/* @ts-ignore */}
        {args.some(isListSignal) ? computed(() => <>{buildCells()}</>) : buildCells()}
      </div>
    )
  }
  setMeta(applyChildren, { noAutoLift: true })
  return applyChildren
}

function WrapWithPrintIfNotReactElement(child: any): any {
  if (child instanceof Signal) {
    // Reactive island: render EAGERLY inside the same computed that reads the
    // value – a second lazy layer (Print's computed) could otherwise render a
    // tensor after its producer disposed it.
    return computed(() => {
      const value = (child as Signal<unknown>).value
      if (value instanceof Signal) { return WrapWithPrintIfNotReactElement(value) }
      if (isValidElement(value)) { return value }
      return (
        <Panel className="overflow-scroll">
          <ErrorBoundary fallback={<div>Something went wrong</div>} resetKeys={[value]}>
            {PrettyPrint(value)}
          </ErrorBoundary>
        </Panel>
      )
    });
  }
  if (isValidElement(child)) {
    return child;
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
const pointMapping = (range?: tf.Tensor) => {
  const [[x0, x1], [y0, y1]] = (range
    ? getAsSyncList(range)
    : [[0, 1], [0, 1]]) as [[number, number], [number, number]]
  return { x0, y0, spanX: x1 - x0 || 1, spanY: y1 - y0 || 1 }
}
const readPoint = (target: tf.Variable | Signal<tf.Tensor>): [number, number] => {
  const value = target instanceof Signal ? target.peek() : target
  return getAsSyncList(value as tf.Tensor) as [number, number]
}

// Draggable 2D point bound to a [2] variable (or signal), mapped to a data
// range [[x0, x1], [y0, y1]]. Dragging the dot assigns; assignments (e.g. by
// an optimizer) move the dot. The layer is transparent to pointer events, so
// several Point2Ds can share one Layers stack.
const Point2D = (target: tf.Variable | Signal<tf.Tensor>, range?: tf.Tensor, color?: Value) => {
  const { x0, y0, spanX, spanY } = pointMapping(range)
  const fill = String(color ?? "white")
  const version: Signal<number> | undefined = (target as any).__version__

  const write = (event: React.PointerEvent<HTMLDivElement>) => {
    const rect = event.currentTarget.parentElement!.getBoundingClientRect()
    const point = tf.tensor([
      x0 + (event.clientX - rect.left) / rect.width * spanX,
      y0 + (event.clientY - rect.top) / rect.height * spanY,
    ])
    if (target instanceof Signal) { SignalUpdate(target, point) }
    else { (target as tf.Variable).assign(point) }
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
const Trail = (target: tf.Variable | Signal<tf.Tensor>, range?: tf.Tensor, color?: Value) => {
  const { x0, y0, spanX, spanY } = pointMapping(range)
  const stroke = String(color ?? "white")
  const version: Signal<number> | undefined = (target as any).__version__
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

// A simple JavaScript implementation for reading SafeTensors files.
// This function takes an ArrayBuffer (e.g., from fs.readFileSync().buffer in Node.js or fetch().arrayBuffer() in browser)
// and returns an object with tensor names as keys and objects containing dtype, shape, and data (as TypedArray) as values.
// Note: For 'F16' and 'BF16', the data is returned as Uint16Array (raw bits); you may need additional conversion to floats if required.
// Throws errors on unsupported dtypes or invalid formats.


function LoadSafeTensors(arrayBuffer: ArrayBuffer) {
  if (!(arrayBuffer instanceof ArrayBuffer)) {
    throw new Error('Input must be an ArrayBuffer');
  }

  const dataView = new DataView(arrayBuffer);
  let offset = 0;

  // Read header length: 8-byte little-endian uint64
  const headerLen = Number(dataView.getBigUint64(offset, true));
  offset += 8;

  if (headerLen === 0 || offset + headerLen > arrayBuffer.byteLength) {
    throw new Error('Invalid header length');
  }

  // Extract header bytes and decode as JSON
  const headerArray = new Uint8Array(arrayBuffer, offset, headerLen);
  const decoder = new TextDecoder('utf-8');
  const headerString = decoder.decode(headerArray);
  const header = JSON.parse(headerString);

  offset += headerLen;

  const tensors: Record<string, { dtype: string; shape: number[]; data: ArrayBufferView }> = {};

  for (const key in header) {
    if (key === '__metadata__') { continue } // Skip optional metadata

    const info = header[key];
    if (!info.dtype || !info.shape || !info.data_offsets) {
      throw new Error(`Invalid tensor info for key: ${key}`);
    }

    const { dtype, shape }: { dtype: string; shape: number[] } = info;
    const [start, end] = info.data_offsets;

    if (start < 0 || end <= start || offset + end > arrayBuffer.byteLength) {
      throw new Error(`Invalid data offsets for tensor: ${key}`);
    }

    const tensorBuffer = arrayBuffer.slice(offset + start, offset + end);

    let data;
    switch (dtype) {
      case 'BOOL':
        data = new Uint8Array(tensorBuffer);
        break;
      case 'I8':
        data = new Int8Array(tensorBuffer);
        break;
      case 'U8':
        data = new Uint8Array(tensorBuffer);
        break;
      case 'I16':
        data = new Int16Array(tensorBuffer);
        break;
      case 'U16':
        data = new Uint16Array(tensorBuffer);
        break;
      case 'I32':
        data = new Int32Array(tensorBuffer);
        break;
      case 'U32':
        data = new Uint32Array(tensorBuffer);
        break;
      case 'I64':
        data = new BigInt64Array(tensorBuffer);
        break;
      case 'U64':
        data = new BigUint64Array(tensorBuffer);
        break;
      case 'F16':
      case 'BF16':
        data = new Uint16Array(tensorBuffer); // Raw bits; convert to float if needed
        break;
      case 'F32':
        data = new Float32Array(tensorBuffer);
        break;
      case 'F64':
        data = new Float64Array(tensorBuffer);
        break;
      default:
        throw new Error(`Unsupported dtype: ${dtype}`);
    }

    // Verify the data length matches shape and dtype size
    const expectedSize = shape.reduce((a, b) => a * b, 1);
    const byteSize = data.byteLength / data.BYTES_PER_ELEMENT;
    if (byteSize !== expectedSize) {
      throw new Error(`Data size mismatch for tensor: ${key}`);
    }

    tensors[key] = { dtype, shape, data };
  }

  return tensors;
}

// CORS fallback: retry blocked cross-origin loads through the dev server
const proxied = (url: string) => `/proxy?url=${encodeURIComponent(url)}`

function LoadTensorFromImageUrl(url: string): Signal<tf.Tensor | null> {
  const s = SignalCreate<tf.Tensor | null>(null);

  const imgElement = document.createElement('img');
  imgElement.crossOrigin = "anonymous";
  imgElement.src = url;

  imgElement.onerror = () => {
    if (!imgElement.src.startsWith(proxied(""))) {
      imgElement.src = proxied(url)
    }
  }
  imgElement.onload = () => {
    s.value = tf.browser.fromPixels(imgElement)
  }

  return s
}

// Shared engine for realtime tensor sources (Camera, Microphone, Time):
// a read-only signal driven by requestAnimationFrame that disposes stale tensors.
// `frame` returns the next tensor, or null to keep the current one.
function FrameSignal(initial: tf.Tensor, frame: (time: number) => tf.Tensor | null, cleanup?: () => void): Signal<tf.Tensor> & { dispose: () => void } {
  const source = SignalCreate<tf.Tensor>(initial)
  let active = true

  const tick = (time: number) => {
    if (!active) return

    const next = frame(time)
    if (next) {
      const oldTensor = source.value
      source.value = next
      if (oldTensor) { oldTensor.dispose() }
    }
    requestAnimationFrame(tick)
  }
  requestAnimationFrame(tick)

  const result = computed(() => source.value) as Signal<tf.Tensor> & { dispose: () => void }
  result.dispose = () => {
    active = false
    cleanup?.()
  }
  registerDisposable(result.dispose)
  return result
}

function Camera(width?: tf.Tensor, height?: tf.Tensor, fps?: tf.Tensor): Signal<tf.Tensor> {
  const w = width ? getAsSyncList(width) as number : 640
  const h = height ? getAsSyncList(height) as number : 480
  const frameInterval = 1000 / (fps ? getAsSyncList(fps) as number : 30)

  const video = document.createElement('video')
  video.autoplay = true
  video.playsInline = true

  let stream: MediaStream | null = null
  navigator.mediaDevices.getUserMedia({ video: { width: w, height: h } }).then(s => {
    stream = s
    video.srcObject = s
    video.onloadedmetadata = () => video.play()
  }).catch(err => console.error('Camera access denied:', err))

  let lastTime = 0
  // Initialize with black frame so operations don't fail before camera starts
  return FrameSignal(tf.zeros([h, w, 3], 'int32'), (time) => {
    if (time - lastTime < frameInterval || video.readyState !== video.HAVE_ENOUGH_DATA) { return null }
    lastTime = time
    return tf.browser.fromPixels(video)
  }, () => {
    stream?.getTracks().forEach(track => track.stop())
    video.srcObject = null
  })
}

// Shared getUserMedia/AnalyserNode setup for Microphone and MicrophoneSpectrum
function AudioAnalyser(fftSize: number, smoothing?: number): { get: () => AnalyserNode | null, cleanup: () => void } {
  let stream: MediaStream | null = null
  let audioContext: AudioContext | null = null
  let analyser: AnalyserNode | null = null

  navigator.mediaDevices.getUserMedia({ audio: true }).then(s => {
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

function Microphone(bufferSize?: tf.Tensor): Signal<tf.Tensor> {
  const size = bufferSize ? getAsSyncList(bufferSize) as number : 2048
  const audio = AudioAnalyser(size * 2)
  const dataArray = new Float32Array(size)

  // Initialize with silence
  return FrameSignal(tf.zeros([size]), () => {
    const analyser = audio.get()
    if (!analyser) { return null }
    analyser.getFloatTimeDomainData(dataArray)
    return tf.tensor1d(dataArray)
  }, audio.cleanup)
}

function MicrophoneSpectrum(bufferSize?: tf.Tensor): Signal<tf.Tensor> {
  const size = bufferSize ? getAsSyncList(bufferSize) as number : 1024
  const audio = AudioAnalyser(size * 2, 0.8)
  const dataArray = new Uint8Array(size)

  // Initialize with silence; normalized to 0-1 range
  return FrameSignal(tf.zeros([size]), () => {
    const analyser = audio.get()
    if (!analyser) { return null }
    analyser.getByteFrequencyData(dataArray)
    return tf.tidy(() => tf.tensor1d(dataArray).div(255))
  }, audio.cleanup)
}

function Time(): Signal<tf.Tensor> {
  const startTime = performance.now()
  return FrameSignal(tf.scalar(0), () => tf.scalar((performance.now() - startTime) / 1000))
}


const LoadSafeTensorFromURL = (url?: string) => {
  if (!url) {
    return new Error("loadSafeTensorFromURL: url is required")
  }
  if (typeof url !== "string") {
    return new Error("loadSafeTensorFromURL: url must be a string")
  }
  const s = SignalCreate("Loading tensors...");

  fetch(url)
    .catch(() => fetch(proxied(url)))
    .then(res => res.arrayBuffer())
    .then(buffer => {
      const tensors = LoadSafeTensors(buffer);

      const mu = Object.entries(tensors).map(([key, value]) => {
        // @ts-ignore
        return [key, tf.tensor(value.data, value.shape, "float32")]
      })

      return mu
    })
    .then((a) => {
      // @ts-ignore
      s.value = a
    })

  return s
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
type GridNode = { label: string, row: number, col: number, width: number, origin: Origin }
type GridEdge = { parentRow: number, parentCol: number, childRow: number, childCol: number }

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

  return (
    <svg style={{ width: totalWidth, height: totalHeight, shapeRendering: "optimizeSpeed" }} viewBox={`0 0 ${totalWidth} ${totalHeight}`}>
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
          />
        )
      })}
      {nodes.map(n => (
        <TreeNode
          key={`${n.row},${n.col}`}
          x={colX[n.col]!}
          y={rowY(n.row)}
          label={n.label}
          width={n.width}
          origin={n.origin}
        />
      ))}
    </svg>
  )
}

function TreeEdge({ x1, y1, x2, y2, trunkX }: { x1: number, y1: number, x2: number, y2: number, trunkX: number }): JSX.Element {
  const r = TREE.cornerRadius
  const dy = y2 - y1
  const ady = Math.abs(dy)

  // Straight horizontal line
  if (ady < 1) {
    return <path d={`M${x1} ${y1} L${x2} ${y2}`} fill="none" strokeWidth={1} className="stroke-neutral-600" />
  }

  // Small vertical distance - smooth S-curve
  if (ady < 2 * r) {
    const sr = ady / 2
    const sdy = dy > 0 ? sr : -sr
    return (
      <path
        d={`M${x1} ${y1} L${trunkX + sr} ${y1} Q${trunkX} ${y1}, ${trunkX} ${y1 + sdy} Q${trunkX} ${y2}, ${trunkX - sr} ${y2} L${x2} ${y2}`}
        fill="none" strokeWidth={1} className="stroke-neutral-600"
      />
    )
  }

  // Normal case - stepped path with rounded corners
  const sdy = dy > 0 ? r : -r
  return (
    <path
      d={`M${x1} ${y1} L${trunkX + r} ${y1} Q${trunkX} ${y1}, ${trunkX} ${y1 + sdy} L${trunkX} ${y2 - sdy} Q${trunkX} ${y2}, ${trunkX - r} ${y2} L${x2} ${y2}`}
      fill="none" strokeWidth={1} className="stroke-neutral-600"
    />
  )
}

function TreeNode({ x, y, label, width, origin }: { x: number, y: number, label: string, width: number, origin: Origin }): JSX.Element {
  return (
    <g transform={`translate(${x - width / 2}, ${y - NODE_HEIGHT / 2})`}>
      <rect
        className="cursor-pointer fill-transparent hover:fill-[#101010] stroke-[0.5] hover:stroke-1"
        width={width}
        height={NODE_HEIGHT}
        rx={NODE_HEIGHT / 4}
        stroke="rgba(255,255,255, 0.1)"
        onPointerOver={(e) => { e.stopPropagation(); setHoverHighlight(origin) }}
        onPointerOut={(e) => { e.stopPropagation(); setHoverHighlight(null) }}
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
      case 'Code': return []
      default: throw new Error(`Unknown type ${node.type}`)
    }
  }

  // A node sits on the same row as its first child, so the leftmost spine of
  // every subtree is horizontal. spineLength = how many columns it spans.
  function spineLength(node: SyntaxTreeNode): number {
    const children = getChildren(node)
    return children.length === 0 ? 1 : 1 + spineLength(children[0]!)
  }

  // Snug packing: anchor each subtree at the first row where its whole spine
  // fits, then pack the remaining children greedily below.
  function layout(node: SyntaxTreeNode, minRow: number, col: number): { row: number, maxRow: number } {
    const spine = spineLength(node)
    const fitsAt = (r: number) => {
      for (let i = 0; i < spine; i++) {
        if (used.has(`${r},${col + i}`)) { return false }
      }
      return true
    }
    let row = minRow
    while (!fitsAt(row)) row++

    const label = getLabel(node)
    nodes.push({ label, row, col, width: nodeWidth(label), origin: node.origin })
    used.add(`${row},${col}`)

    let maxRow = row
    let childMin = row
    for (const child of getChildren(node)) {
      const placed = layout(child, childMin, col + 1)
      edges.push({ parentRow: row, parentCol: col, childRow: placed.row, childCol: col + 1 })
      maxRow = Math.max(maxRow, placed.maxRow)
      childMin = placed.row + 1
    }
    return { row, maxRow }
  }

  // Top-level statements keep disjoint row bands
  let row = 1
  for (const stmt of tree.content) {
    row = layout(stmt, row, 1).maxRow + 1
  }

  return { nodes, edges }
}

// MARK: Runtime

// MARK: PrettyPrint

const frameStyle = "rounded-xl border border-neutral-800 hover:border-neutral-700 p-2"

function Print(obj: Signal<unknown>) {
  return computed(() =>
    <Panel className="overflow-scroll">
      <ErrorBoundary fallback={<div>Something went wrong</div>} resetKeys={[obj]}>
        {PrettyPrint(obj)}
      </ErrorBoundary>
    </Panel>
  )
}
setMeta(Print, { noAutoLift: true })

function PrintFunction(fn: Function) {
  // Subscribe to meta version for reactive updates
  if ((fn as any).__meta_v__) (fn as any).__meta_v__.value

  const { doc } = getMeta(fn)
  const sourceCode = computed(() => fn.toString())
  if (doc) {
    return <>{Text(doc)}{Code(sourceCode)}</>
  }
  return Code(sourceCode)
}

function PrettyPrintSyntaxTree(node: SyntaxTreeNode & { type: "Program" }): JSX.Element {
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
        {obj.map((item, key) => <div key={key} className={`${frameStyle} !border-0 grid hover:bg-neutral-900 mix-blend-screen`}>{PrettyPrint(item)}</div>)}
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
      // the whole output panel.
      return <div className="contents">{computed(() => PrettyPrint(obj.value)) as unknown as JSX.Element}</div>;
    }

    if (isValidElement(obj)) {
      return obj;
    }

    // Reactive variable: create computed that re-reads on version change
    if (obj instanceof tf.Tensor && (obj as any).__version__) {
      const version = (obj as any).__version__ as Signal<number>
      const reactiveValue = computed(() => {
        version.value  // subscribe to changes
        return obj.clone()
      })
      return PrettyPrint(reactiveValue)
    }

    if (obj instanceof tf.Tensor) {
      if (obj.rank === 0) {
        const color = COLORS.find(rule => rule.token === "number")?.foreground || "FFFFFF";
        const formattedNumber = (obj.arraySync()).toLocaleString("en-US", { minimumFractionDigits: 0, maximumFractionDigits: 6, useGrouping: true }).replace(/,/g, "_");
        return <span style={{ color: `#${color}` }}>{formattedNumber}</span>;
      }

      if (obj.rank === 1) {
        return (
          <div className="">
            <BarPlot data={obj} />
          </div>
        );
      }

      if (obj.rank === 2) {
        // Use fast canvas with viridis colormap for large tensors, Plotly for small (debugging)
        const size = obj.shape[0] * obj.shape[1]
        if (size > 400) {
          return <HeatCanvas data={obj} />
        }

        return <HeatPlot data={obj} />
      }

      if (obj.rank === 3) {
        // RGB image - use canvas renderer
        return <TensorCanvas data={obj} />
      }

      if (obj.rank > 3) {
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

function Panel({ children, className }: { children: JSX.Element, className?: string }) {
  return (
    <div className={`${frameStyle} ${className ?? ""}`}>
      {children}
    </div>
  )
}

// MARK: Plots

const BarPlot = ({ data }: { data: tf.Tensor }) => {

  const annotations: Partial<Annotations>[] = [];

  if (data.shape.reduce((a, b) => a * b) < 20) {
    // @ts-ignore
    annotations.push(...data.arraySync().map((value: number, i: number) => ({
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
          y: data.arraySync(),
          type: 'bar',
          marker: {
          color: data.arraySync(),
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

const HeatPlot = ({ data }: { data: tf.Tensor }) => {
  const z = getAsSyncList(data) as number[][];

  let annotations: Partial<Annotations>[] = [];

  if (data.shape.reduce((a, b) => a * b) < 20) {
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
      style={{ width: '100%', height: '100%' }}
    />
  );
}

// Fast canvas-based tensor renderer for real-time use (Camera, etc.)
const TensorCanvas = ({ data }: { data: tf.Tensor }) => {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  const height = data.shape[0] || 100
  const width = data.shape[1] || 100
  const aspect = width / height

  useEffect(() => {
    if (!canvasRef.current || !data || data.isDisposed) { return }

    const canvas = canvasRef.current

    // Only resize if dimensions changed (resizing clears canvas, causes flicker)
    if (canvas.width !== width || canvas.height !== height) {
      canvas.width = width
      canvas.height = height
    }

    tf.browser.toPixels(data as tf.Tensor2D | tf.Tensor3D, canvas)
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
const VIRIDIS_LUT = tf.tensor2d(
  Array.from({ length: 256 }, (_, i) => {
    const color = rgb(interpolateViridis(i / 255))
    return [color.r / 255, color.g / 255, color.b / 255]
  })
)

// Fast canvas-based heatmap with viridis colormap (GPU-accelerated via LUT)
const HeatCanvas = ({ data }: { data: tf.Tensor }) => {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  const height = data.shape[0] || 100
  const width = data.shape[1] || 100
  const aspect = width / height

  useEffect(() => {
    if (!canvasRef.current || !data || data.isDisposed) { return }

    const canvas = canvasRef.current

    if (canvas.width !== width || canvas.height !== height) {
      canvas.width = width
      canvas.height = height
    }

    // Apply viridis colormap via lookup table
    const rgb = tf.tidy(() => {
      // Normalize to [0, 1]
      const min = tf.min(data)
      const max = tf.max(data)
      const range = tf.maximum(tf.sub(max, min), 1e-6)
      const normalized = tf.div(tf.sub(data, min), range)

      // Scale to [0, 255] and convert to int for LUT indexing
      const indices = tf.cast(tf.clipByValue(tf.mul(normalized, 255), 0, 255), 'int32')

      // Gather colors from LUT
      const flat = tf.reshape(indices, [-1])
      const colors = tf.gather(VIRIDIS_LUT, flat)

      // Reshape back to image dimensions
      return tf.reshape(colors, [height, width, 3])
    })

    tf.browser.toPixels(rgb as tf.Tensor3D, canvas).then(() => {
      rgb.dispose()
    })
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
  "flip-operator": `
; Commute/flip operator arguments
↔︎ : {⊙ | {x,y| y ⊙ x}},
1 - 2,
1 ↔︎(-) 2
`,
  "partial-application": `
; Partial application with custom operators
(
    ↔ : { ⊙ | { a, b | b ⊙ a } },
    ⊢ : { ⊙, a | { b | a ⊙ b } },
    ⊣ : ↔(⊢),
    (+ ⊢ -1)(3),
    (-1 ⊣ +)(3),
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
(++=): { a, b | a(a() ++ b) },

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

const editorBeforeMount: BeforeMount = (monaco) => {
  EditorInstance = monaco.editor

  monaco.languages.register({ id: "fluent" });

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
      "editor.foreground": COLORS.find(rule => rule.token === "string")?.foreground ?? "FFFFFF",
      "editor.background": "#1C1C1C",
      "focusBorder": "#FF000000",
    },
  });

  monaco.editor.defineTheme("fluentThemeReadOnly", {
    base: "vs-dark",
    inherit: true,
    rules: COLORS,
    colors: {
      "editor.foreground": COLORS.find(rule => rule.token === "string")?.foreground ?? "FFFFFF",
      "editor.background": "#1C1C1C00",
      "focusBorder": "#FF000000",
    },
  });

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

      // Collect all keys from scope including parent scopes (prototype chain)
      const allKeys: string[] = [];
      for (const key in completionScope) {
        allKeys.push(key);
      }

      return ({
        suggestions: [
          ...symbolSuggestionItems,
          ...allKeys.map((key) => {
          const value = completionScope[key];
          return ({
            label: key,
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: key,
            documentation: `Function: ${key}`,
            detail: typeof value === 'function' ? 'function' : String(value).slice(0, 50),
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

export function Playground() {

  useSignals()

  useEffect(() => {
    // register URL search params change listener
    const handleUrlChange = () => {
      const codeFromUrl = StringDeserialize(new URLSearchParams(window.location.search).get("code") ?? "");
      if (codeFromUrl !== "") {
        SignalUpdate(code, codeFromUrl);
      }
    };
    window.addEventListener("popstate", handleUrlChange);
    window.addEventListener("pushstate", handleUrlChange);
    window.addEventListener("replacestate", handleUrlChange);
    return () => {
      window.removeEventListener("popstate", handleUrlChange);
      window.removeEventListener("pushstate", handleUrlChange);
      window.removeEventListener("replacestate", handleUrlChange);
    };
  }, [])

  const codeFromUrl = StringDeserialize(new URLSearchParams(window.location.search).get("code") ?? "");
  const code = useSignal<string>(codeFromUrl !== "" ? codeFromUrl : "1 + 1")
  //const code = useSignal<string>(codeFromUrl !== "" ? codeFromUrl : getExample("REPL"))
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
          Grid(tf.scalar(2), tf.scalar(1))(
            Print(result),
            CodeEditor(code)
          )
        }
      </div>
    </>
  );
}

document.addEventListener("DOMContentLoaded", () => {
  const root = document.getElementById("root")

  if (!root) {
    throw new Error("Root element not found")
  }

  createRoot(root).render(<Playground />)
})
