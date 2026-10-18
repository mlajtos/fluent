# Own engine

Fluent runs on its own tensor engine, `engine/`. No jax-js, optax or TensorFlow.js is
in the dependency tree. This document records what the engine is, where each part of
the language lives, what WebGPU gives and costs, and the decisions behind it.

Unless a row says otherwise, numbers were measured on an Apple M1 (8-core GPU, 16 GB),
Chrome 151, a 60 Hz panel, 2026-10-14 to 16: medians, runs ordered forward, reverse,
forward to catch drift, GPU times from timestamp queries (≈3.5 µs resolution).

---

## 1. Size

| | on jax-js (2026-10-14) | own engine (2026-10-17) |
|---|---|---|
| language core | `language.ts` 3,966, of which the prelude 367 | `language.ts` 2,495 + `prelude.fl` 804 |
| tensor engine | jax-js 0.1.18: 17,369 shipped (20,628 source) | `engine/` 2,336 |
| optimizers, loaders | optax + loaders: 1,313 | the prelude; a safetensors reader and OPFS cache in `host.ts` |
| **total** | **~22.6k** | **5,635** |

The IDE is left out of both columns: `client.tsx` 2,352, `host.ts` 862, `protocol.ts`
106, `worker.ts` 20, `painter.ts` 124, `documentation.ts` 126.

About 950 lines of the old `language.ts` (24%) only adapted Fluent to jax-js: reference
counting at 95 sites, borrow and adopt, disposal scopes, trace bail-outs, jit replay,
the optimizer wrapper. An engine that owns its buffers and computes ∇ as a graph rewrite
needs none of it. jax-js also baked constants into kernels (a new value, a new shader),
submitted once per kernel, kept every cache unbounded, aborted a trace on any data read
(the prelude's einsum made a compiled step eager, ~4× slower), and under ∇ stranded a
buffer per intermediate (eager training ran out of memory in ~28 steps).

## 2. The governing idea: a small kernel, everything else in Fluent

Owning the stack from syntax to compute puts Fluent where Alan Kay's STEPS project put
Frank: a small kernel, and everything else in problem-oriented languages. Fluent's
grammar is written in Ohm, Alessandro Warth's successor to OMeta.

- **The engine is a kernel.** It holds the primitives, the graph and the mechanisms: the
  rewrite engine, ∇, the scheduler, the printers, the executor, memory.
- **The vocabulary is Fluent.** softmax, FFT, the optimizers and most of the library live
  in `prelude.fl`. The engine fuses what the prelude composes, and computes constant-only
  values while the graph is built (§3.2), so metadata code costs nothing per step.
- **The measure** is how small the kernel stays while the language grows. A feature asks
  first whether it can be prelude, and only last whether it must be kernel.

---

## 3. The engine

| file | lines | what it is |
|---|---|---|
| `ir.ts` | 346 | One immutable UOp graph from tensors down to loop nests; every value f32 |
| `rewrite.ts` | 58 | The rewrite engine: bottom-up, innermost, to fixpoint, interning as it goes |
| `opt.ts` | 349 | Exact simplification laws; fusion by substitution, priced by a cost model; plan keys; the memory budget |
| `grad.ts` | 166 | ∇ = transpose ∘ linearize: one JVP table, one transpose table |
| `gpu.ts` | 534 | WGSL printers (map, 16×16 tile, a reduction split over workgroups) and the WebGPU executor |
| `cpu.ts` | 154 | The CPU backend: each kernel printed as a JS function, every value op `fround`ed to match f32 |
| `tensor.ts` | 681 | The Tensor the language calls: placement, folding, realization, and the natives built on the IR |

**Primitives** (`ir.ts`): leaves CONSTANT, BUFFER, IOTA; ADD, MULTIPLY, RECIPROCAL, EXP,
LOG, SQRT, LESS_THAN, WHERE, DETACH; RANDOM (uniform, a pure function of seed and index,
so it fuses anywhere); REDUCE (add, max, product); the views BROADCAST, PERMUTE,
RESHAPE, SLICE, WINDOW, PAD; GATHER and SCATTER_ADD, each other's transpose. MATH is one
table row per function, each with one JS function, one WGSL expression and one
derivative: sin, cos, tanh, asin, acos, atan, asinh, acosh, atanh, floor, ceil, trunc,
round, sign, abs, isnan, expm1, log1p, cbrt, atan2, div, eq, pow. A FUSED node is a loop
nest of the same node type, so every pass reaches inside kernels.

### 3.1 Backends

`init()` in `engine/tensor.ts` asks the platform for a WebGPU adapter and requests the
adapter's maximum limits (the defaults fail silently: a buffer over the limit reads back
as zeros). With no adapter, as in Bun (`tests.ts`) or a browser without WebGPU, every
graph runs on the CPU backend. With one, a graph runs on the CPU when its inputs are on
the host and its work (the summed size of its computed nodes) is at most 2¹⁶ elements,
so the metadata a program computes never waits on the GPU; everything else runs on the
GPU. `worker.ts` calls `init()` before it evaluates anything.

### 3.2 How a value is computed

- **Lazy, immutable.** A Tensor is a graph node, computed when something reads it. A
  computed node gets one stable leaf: a data leaf from the CPU, a device leaf naming its
  buffer from the GPU. The buffer lives as long as the leaf; nothing is freed by hand.
- **Settling.** A value that crosses a step (a variable's value, an optimizer's state)
  becomes a leaf. Without this every graph drags its history along: a 60-step fit took
  691 s.
- **Folding.** A value that depends only on constants and has at most 256 elements is
  computed while it is built, each op exactly as its kernel computes it
  (`engine/tests/fold.bun.ts` checks every op bit for bit). Fluent shape arithmetic went
  from 33–72 µs to 9–11 µs a call.
- **Plans.** A plan (lowered kernels and their slots) is cached by the graph's canonical
  structure: ops, args and shapes in canonical order, leaves numbered by position. A
  training step built afresh each tick finds the plan it compiled on its first tick. The
  GPU keeps 512 plans, least recently used out. A scalar constant is part of the key: the
  printer writes it into the kernel text.
- **Views are index expressions.** A bounds-aware simplifier in the index constructors
  stops views re-dividing the index at every level (`engine/tests/index.bun.ts`); it
  took the dreamer's step from 29 to 17.5 ms and made the weight-gradient tiles 9×
  faster. SLICE's transpose is PAD, an index predicate. As a scatter, one ∇ of a 4k
  slice ran 3.4·10⁷ loop iterations.
- **Contractions.** A contraction is spliced into a reduction's body only when it becomes
  the whole, tileable body: 17.5 → 9.2 ms a dreamer step. From K = 8 a contraction runs
  as a tile (`engine/tests/bench-tile.ts` re-derives the gate), and a loop nest the plan
  materializes is read by every reader, never recomputed in some: the dreamer step's GPU
  time went from 8.3 to 3.5 ms. `conv` is one reduction over a WINDOW view; as shifted
  copies, a 27×27 lenia kernel was 729 graph terms.
- **Sorting.** `sort`, `argsort` and `topK` are natives in `engine/tensor.ts`. Data the
  host has is sorted there. Data on the GPU is sorted there by a bitonic network built
  in TypeScript: log₂m·(log₂m + 1)/2 passes, each one gather and one select with the
  partner computed in the kernel, so nothing is read back. Stable, NaN last.
- **Dispatch.** 32 dispatches per `queue.submit`; maps in workgroups of 256 (64 below
  256 elements). A grid beyond 65,535 workgroups on one axis folds into two, because the
  browser drops the whole command buffer otherwise. A kernel binds at most 10 storage
  buffers (Chrome on Mac).
- **Reads.** `read()` reads asynchronously with `mapAsync`. A synchronous read (`item`,
  `js`, control flow on a GPU value) copies the buffer into a WebGPU canvas, draws it
  onto a 2-D canvas and reads it with `getImageData`. It blocks the worker, not the page.
- **Memory.** A plan that computes into more than 2 GB is refused with an Error, so a
  broadcast one axis too big cannot exhaust a unified-memory machine.

### 3.3 ∇

`∇(f)(x)` evaluates `f` on a traced leaf and applies transpose ∘ linearize to the graph
that comes out. A branch (`guard`, `cond`, `⍣` until) reads the realized value it is
at, so the graph records the branch taken and the derivative is that branch's: JAX's
`grad` semantics. Higher order is the same rewrite again. `∇(loss)()` with no argument
differentiates with respect to the `~` variables the loss reads and gives (value,
variables, gradients) in one pass. A state write while differentiating is an Error.

- The branch of a `where` not taken gets an exact zero cotangent, and a zero cotangent
  times an infinite factor stays zero, so NaN from the branch not taken never leaks. The
  product is `mul0`, in which 0 is strong; its derivative is the product's, so a higher
  derivative still holds where a cotangent is 0.
- `∇max` splits ties evenly, as JAX does.
- The engine has no law x × 0 = 0 (0 × ∞ is NaN).

### 3.4 Numerics

f32 only; there are no dtypes, and bool and int are printing tags. A literal integer
exponent up to 64 is exponentiation by squaring, so `3^2` is exactly 9. `isnan` tests
raw bits, because WGSL may assume no NaN and fold `x != x` away; NaN and ±∞ are bitcast
constants. `=` is exact; `≡` matches within a relative 2⁻²⁰ (APL's tolerant equality).

---

## 4. Where each built-in lives

**Native**, in `language.ts`'s `DefaultEnvironment`, most a thin call into
`engine/tensor.ts`. A built-in stays native when it is a primitive; when it is variadic
(a lambda takes a fixed number of parameters): `List`, `cond`, `cascade`, `concat`,
`stack`, `meshgrid`; when it is evaluation, reflection, signals, strings or I/O; or when
a Fluent version costs too much host time. A Fluent application costs ~1–3 µs of host
time, so code that runs dozens of them per call stays native: `matmul`, `dot`, the
bitonic network (30–110 ms to build in Fluent), `conv`, `oneHot`. einsum is Fluent:
reading a spec takes about 3 ms, and a training step builds its graph afresh every tick
(the plan cache skips lowering, not building), so einsum keeps what it read with `memo`,
per spec and operand shapes. After the first call, a call costs about 0.05 ms.

**Prelude**, in `prelude.fl`: `-`, `neg`, `>`, `≤`, `≥`, `≠`, `∨ ∧ ¬ ⊻ ⍲ ⍱`, `ceil`,
`trunc`, `%`, `hypot`, `sinc`, `erf`, `mean`, `variance`, `std`, `norm`, `softmax`,
`logSoftmax`, `crossEntropy`, `argmax`, `argmin`, `scan` (log₂n doubling passes),
`fft`, `randn`, `roll`, `reverse`, `tile`, `repeat`, `pad`, `moveaxis`, `transpose`,
`linspace`, `fill`, `eye`, `⊗`, `⍤`, `∙`, `mask`, `≡`, `newton`, `solve`, `⍣ -1`, `⍢`,
the drawing words, and every optimizer: `optimizer(init, rule, lr, vars?)` and on it
sgd, adam, adamw, adagrad, rmsprop, adadelta, adamax, nadam, radam, amsgrad, adabelief,
yogi, lion, lamb, muon; the `warmup` and `cosine` schedules; `clipNorm`.

Prelude definitions against the jax-js natives they replaced:

| built-in | definition | max difference |
|---|---|---|
| erf | Abramowitz–Stegun 7.1.26, the polynomial jax-js uses | 0; ∇ 1.3e-6 |
| softmax | `e: exp(x - max(x, ax, 1)), e ÷ Σ(e, ax, 1)` | 0; ∇ 7e-9 |
| fft | DFT matrix; radix-2 butterflies for a power of two from 64 | 1.5e-6; ∇ matches |
| einsum | labels from the spec's codes, `permute` and `⍴`, broadcast `×`, `Σ` | 0 on 17 patterns; ∇ 0 |
| randn | Box–Muller from uniform | 20k samples: μ −0.005, σ 1.0008, kurtosis 3.02 |
| sgd, adagrad, adam | `~`, `~~`, `:=`, `∇` | 0, 0, 3.6e-6 after 10, 10, 20 steps |

---

## 5. The worker and the page

`worker.ts` owns the engine and the GPU and runs `host.ts`, which evaluates the program,
binds the widgets and sources, and describes values to the page. The page
(`client.tsx`) keeps React, Monaco and input, imports the language only to parse, and
never touches the engine. Values cross as plain data (`protocol.ts`).

- **Live values** are ids, reference-counted across the boundary: the worker counts how
  often it sent an id, the page releases all it received at once, so a release never
  races a send. A widget's write carries a sequence number the worker answers; until
  then the widget shows its own value, so typing and dragging are never echoed over.
- **Flow control.** Changes go out at most once a frame, in one batch, and the next batch
  waits until the page has drawn the last. A slow page gets fewer, fresher batches; a
  hidden tab gets none while training goes on.
- **Pictures.** With a GPU, `painter.ts` paints in the worker, on the compute device,
  into an `OffscreenCanvas` the page handed over: the tensor's buffer goes straight to
  the screen. Without one, the worker colors the pixels and sends RGBA bytes.
- **Pacing.** `⟳` runs on the worker's `requestAnimationFrame` with at most two steps in
  flight on the GPU, because a display frame waits behind whatever is queued. The host
  skips a paint while more than one painted frame is pending.
- **Reads stay synchronous.** The evaluator is synchronous, and the synchronous read
  bridge blocks the worker, not the page. A generator trampoline would slow the
  evaluator, which is the measured bottleneck.

Typing while training, 6 s, main-thread build → worker build (2026-10-15, pictures
crossing as RGBA):

| demo | frames | p95 frame | training |
|---|---|---|---|
| looped dreamer | 29–52 → 365–368 | 333–466 → 17.6 ms | 24.5 steps/s either way |
| dreamer | 188–193 → 366–370 | 82 → 18 ms | unchanged |

`tests.browser.ts` gates it: typing while the looped dreamer trains must keep the page
at frame rate.

Training throughput on the engine: the dreamer ran 2,428 steps in 20 s against
jax-js's 1,508 (1.6×; 9.2 ms and 110 kernels a step); after the prelude moves of S5,
8.7 ms a step, and the looped dreamer 37.3 ms.

---

## 6. What WebGPU gives and costs

Sources: the WebGPU and WGSL drafts, Chrome's "What's new in WebGPU" through 155–156,
Dawn sources, WebKit and Mozilla posts, web3dsurvey (survey percentages are its
reports), and measurements on the M1.

### 6.1 Core features

| capability | what it buys | measured |
|---|---|---|
| `createComputePipelineAsync` | compiles never stall a frame; ~4× parallel | 8 sync matmul compiles gave a 283 ms frame with a visible canvas; async, 0 dropped |
| persistent shader cache (Dawn's blob cache, per profile and origin) | a return visit compiles nothing | cold 14 ms (12-op map) / 46 ms (128² matmul); same text again 0.1–0.3 ms, also after a browser relaunch; another origin is cold |
| explicit pipeline layouts | bind groups shared across kernels | `layout: 'auto'` bind groups fit one pipeline only (spec); no compile cost |
| `writeBuffer` | constants and small tensors | 1–2 µs up to 64 KB; 4–6 GB/s at 64 MB, blocking 3.5–9 ms |
| MAP_WRITE staging ring | streaming uploads | 8.8–9.7 GB/s |
| `mapAsync` + staging pool | reads | 16 B in 0.3 ms, 16 MB in 3.8 ms on an idle queue; latency = queue depth |
| `dispatchWorkgroupsIndirect` | GPU-chosen sizes; `⍣`-until without a read per iteration | unmeasured; Dawn adds a validation dispatch per call |
| storage textures (`bgra8unorm-storage`, 96%) | displays drawn on the GPU | 512² frame: 0.06 ms GPU, 0.2 ms JS, vs 2.7 ms JS for readback + JS colormap |
| `importExternalTexture` from a `VideoFrame` in a worker | camera frames without copies | 720p: 0.15 ms GPU, ~0.8 ms JS (RGB; NV12 unmeasured) |
| WebGPU and `requestAnimationFrame` in workers | evaluator, engine and pacing off the main thread | Baseline since 2023 |

Submits: 540 tiny kernels cost 2.8 ms in one submit, 31.5 ms with a submit each, 2.1 ms
in chunks of 64. TVM's sweep peaks at 32 per submit and drops past it; production
runtimes sit at 15–32. The first dispatch after an async create costs 0.3–0.4 ms more.

### 6.2 Optional features

Each is taken only with a core path beside it.

| feature | availability | measured here |
|---|---|---|
| `shader-f16` | 94% (Safari ~100%) | f16 storage + f32 accumulate 1.04 TFLOP/s vs f32 0.81 (+28%); f16 accumulate 1.45–1.52 (1.9×), relative error up to 0.017 |
| immediates (`var<immediate>`, 64 B) | Chrome only | per-dispatch constants without a buffer or bind group; unmeasured |
| `subgroups` | Chrome 83%; not Safari 27, not Firefox | no gain for bandwidth-bound reductions (51–54 vs 52–58 GB/s); shuffle-broadcast matmul a loss (0.26 TFLOP/s) |
| `chromium-experimental-subgroup-matrix` | ~0.06%, behind `--enable-unsafe-webgpu` | f32 0.89–0.96 TFLOP/s staged; 1.20–1.29 with robustness off |
| `timestamp-query` | ~99% (Safari 93%) | quantized to 100 µs without the developer flag |
| WGSL `linear_indexing` | Chrome | simpler index math for a folded 2-D grid; unmeasured |

### 6.3 Hard facts

- **Override constants recompile.** Every distinct `override` value is a full backend
  compile, 13–45 ms (confirmed in Dawn's source). They suit a fixed menu of tile sizes,
  never values.
- **10 storage buffers per kernel** on Chrome/Mac (spec default 8; ≥ 16 on ~80% of other
  adapters). The limit forces fused kernels apart.
- **No reusable command lists.** Every step is re-encoded: ~0.6 µs per dispatch, ~1 µs
  per `setBindGroup`, ~25 µs per submit (native wgpu figures).
- **One queue, no priority**, and on Mac no concurrent dispatch within a pass, so fusion
  is the lever.
- **No float atomics** in WGSL: a scatter-add is avoided (views) or done as a segmented
  sum.
- **WGSL may assume no NaN or Inf** (§15.7.2). There is no `isnan`.
- **Lazy zero-initialization.** On Metal, Dawn clears a buffer before the first pass that
  binds it; a partial `writeBuffer` clears all of it first.
- **Watchdogs.** Windows resets the GPU after 2 s; macOS kills command buffers that hurt
  interactivity; Chrome's ~10 s watchdog counts strikes against the origin and can block
  it from the GPU, a real risk for a playground that runs arbitrary code.
- **No memory-budget API.** The engine keeps its own (§3.2).
- **Bandwidth is the wall.** Reductions and maps over 16M elements ran DRAM-bound at
  50–60 GB/s (M1 peak 68). Vector width and reduction style don't matter; bytes do.
- **Firefox** polls GPU completion every 100 ms (~1 ms per dispatch, ~100 ms per round
  trip, early 2026), so a loop paced by fences collapses there. Pace by
  `requestAnimationFrame`; fences only cap depth.

### 6.4 What smoothness depends on

Headed Chrome, continuous scrolling plus a CSS animation, 2 s per row; compositor frames
from a Chrome trace (`DirectRenderer::DrawFrame`), not just rAF:

| scenario | rAF frames dropped | compositor vsyncs missed |
|---|---|---|
| compute only, no visible canvas: submits of 2–100 ms, depth 1–8, main thread or worker, ALU- or memory-bound | 0 | ≤ 1 |
| visible canvas on the compute device, ≤ ~60 ms of work queued ahead of its draw | 0 | 0 |
| same, 80 ms queued | 4 | 22 |
| same, 150 ms queued | 51–59 | 61–66 |
| compute in a worker that also owns a transferred, visible `OffscreenCanvas` | 70 | 79 |
| separate display device, under every load above | 0 | 0 |
| sync pipeline-compile storm, no canvas | 4–7 | 8–12 |
| async compile storm | 0 | 0 |

On this Mac hitches come from exactly two things: a visible canvas sharing a device with
deep compute, and synchronous compiles. One command buffer per step starving the
compositor (TVM #19342 on an M5: a 166 ms frame with the JS thread idle) did not
reproduce here at up to 100 ms per submit, but it is real elsewhere.

### 6.5 Matmul, f32

The f32 FMA ceiling through WebGPU on the M1 is ~2.4 TFLOP/s. Scalar kernels (a 4×8 or
8×4 register block per thread, K in steps of 16 through workgroup memory, a rolled inner
loop) plateau at 0.85–0.92 TFLOP/s from 1024² to 4096², across ~35 measured variants:
unrolling, deeper K, prefetch, double buffering and direct loads were all slower, and
bounds checks cost nothing measurable. The plateau is the outer-product scheme's limit.
MLX reaches 1.55–1.59 on the same machine with the GPU's matrix units; WebGPU's door to
them is subgroup-matrix (1.20–1.29, behind a flag). Small and skinny matmuls are
bandwidth-bound and belong to GEMV.

---

## 7. Not built yet

The engine compiles pipelines synchronously with `layout: 'auto'`, makes a bind group per
dispatch, prices fusion with fixed constants (`COST` in `opt.ts`), and paints on the
compute device. Where it is heading, with the source of each idea:

- **Never block on a compile.** `createComputePipelineAsync` only; the last good program
  runs until every new pipeline has resolved (compute.toys, Strudel, Bevy); generic
  per-primitive kernels run a new structure on its first frame (Dolphin's ubershaders,
  CubeCL).
- **Constants out of the kernel text.** After folding, every remaining literal goes into
  a per-step constant block (immediates where Chrome has them, else one uniform ring), and
  map kernels become shape-free (MLX shapeless compile, tinygrad PARAMs; TF.js measured
  −60–90% warmup). Typing a number or dragging a slider then never compiles.
- **A separate display device** on the main thread, fed with bytes packed to RGBA8 on the
  GPU: 0 dropped frames under every measured load, ~0.1 ms per MB on the main thread.
- **Explicit layouts and cached bind groups**, keyed by binding signature; a
  liveness-based memory plan stored with the plan (tinygrad); automatic donation, so
  `θ := f(θ)` writes in place (MLX, JAX). To measure first: one arena binding for all
  intermediates, which would lift the 10-binding limit.
- **Roofline pricing**: time = max(bytes ÷ bandwidth, flops ÷ peak) + launch, with three
  constants per adapter calibrated by timestamp queries (XLA priority fusion, Inductor).
- **Multi-output kernels** (Adam's p, m, v in one kernel; softmax with logsumexp) and a
  K-contiguous GEMV with vector loads (MLX `gemv.h`).
- **Bounded command buffers**: about one frame (~16 ms) of estimated GPU time per
  command buffer, and a dispatch estimated over ~20 ms split, against watchdogs.
- **Indirect dispatch** for data-dependent loops (`⍣` until, `solve`): encode N
  iterations, let a convergence kernel zero the rest, read once.
- **Device-loss recovery**: a new adapter, re-evaluation, `~` variables restored from an
  OPFS checkpoint (WebLLM, Figma).
- **Parsing in the worker.** The page re-parses on every keystroke for its markers.
- **A subgroup-matrix matmul** behind feature detection, the day Chrome ships it.

Not copied: tinygrad's WebGPU runtime (a pipeline, layout and bind group per call, with
synchronous waits); general symbolic shapes; tuning in the edit path; any user-visible
`jit`, `eval`, `donate` or `checkpoint` knob; Dynamo-style bytecode capture (the
structural key is the guard); scan, while and cond as IR primitives (branches read
realized values, so reverse mode through data-dependent loops comes free); Taichi-style
AD of fused kernels (it needs float atomics); subgroups (no gain measured, absent in
Safari and Firefox).

---

## 8. Steps

Every step keeps the tests and the gallery green.

| step | what | status |
|---|---|---|
| S0 | `engine/` from Fluent9000 and its jank-free copy: f32 only, 2-D grid fold, split-K register tiles, deterministic WGSL | done |
| S1 | MATH table, counter-hash RNG, product reduce, sorting, the JS-printing CPU backend | done |
| S2 | Fluent's native layer on the engine in Bun, checked call by call against jax-js | done |
| S3 | the browser: device leaves, placement by work, the structural plan cache, the index simplifier | done |
| S4 | jax-js, optax and loaders deleted, with the ownership glue | done |
| S5 | the primitive core: folding, IOTA, built-ins moved to the prelude, GPU sorting, one Tensor type in `engine/tensor.ts`, `prelude.fl` | done |
| S6 | learning in userland: `∇(loss)()`, `optimizer(…)` and the optimizers in the prelude | done |
| S7 | the evaluator, engine and GPU in a worker | done |
| S8 | derivatives and laws as values (`derivative(sin, …)`, `law(…)`) | not taken |
| S9 | numerics in the prelude: Cholesky, LU, QR, solve, eigh, SVD, more distributions, Shampoo | open |
| S10 | the IDE in Fluent: an `Element` primitive, widgets and plots as Fluent values, Plotly gone | open |

S8 is line-neutral (~35 TS rows become ~25 Fluent lines plus ~40 lines of template
machinery), laws as values would add a matcher to replace six one-line rules, and custom
derivatives are niche; ∇ through `⍣ -1` is already exact. It reopens if a demo needs a
custom derivative or ⍤'s copy-per-cell cost shows up in a measurement. S8 and S10 need
records.

Optimizers not written yet: L-BFGS, Adafactor, schedule-free AdamW, LARS, EMA,
lookahead; Shampoo and SOAP wait for S9.

---

## 9. Decisions

1. **f32 only.** No f16 storage or accumulation.
2. **WebGPU where the platform has it, the CPU backend everywhere else.** Chrome first;
   other browsers later. Subgroups and immediates may be used, and every kernel keeps a
   core path.
3. **What can live in user space, does.** A built-in stays native only for the reasons
   in §4.
4. **GPU results are checked in headless Chrome** (`bun engine/tests/run.ts`), never
   only in Deno or Bun, whose WebGPU does not enforce the browser's limits. The CPU
   gates run in Bun (`bun run test:engine`).
5. **Reads stay synchronous** in the worker (Milan, 2026-10-16: a frozen worker is
   basically invisible).
6. **∇ takes one tensor**; `∇(loss)()` covers training many variables at once.
7. **Unused natives move to the prelude**; none are dropped.
8. **Sort runs on the GPU** for data on the GPU, as a bitonic network in
   `engine/tensor.ts`.
9. **The branch of a `where` not taken gets an exact zero cotangent**, so its NaN never
   leaks. This is the one semantic change to ∇ on this list.
10. **`∇max` ties split evenly**, a true subgradient.
11. **Limits:** request the adapter's maxima.
12. **◌ means an argument not given**, for natives and lambdas alike.
13. **Optimizers train the variables their loss reads**; an explicit list stays possible.
14. **The prelude is its own file**, `prelude.fl`; `language.ts` is TypeScript only.
15. **Open: records**, which S8's structural rules and S10's attributes need, and
    Plotly → plots written in Fluent (S10), a visible change.

## 10. Risks

1. **One machine.** Everything was measured on an M1; an M5 behaved differently for TVM.
   Windows, NVIDIA, AMD, Intel and Android numbers are missing, as are NV12 camera
   frames and long-session memory behaviour.
2. **The display shares the compute device.** A visible canvas tolerates ~60 ms of
   queued compute; past that, frames drop (§6.4).
3. **Cold starts.** A first visit compiles every kernel, 14–46 ms each, synchronously;
   a return visit hits the browser's shader cache.
4. **GPU blocklisting** through watchdog strikes from runaway user code, until command
   buffers are bounded.
5. **∇ correctness** for every new primitive and view: `engine/tests/core.ts` checks ∇
   against finite differences on random graphs.
