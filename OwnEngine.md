# Own engine — Fluent without jax-js

*Can Fluent run on its own WebGPU engine — jax-js's good parts without the bad
— and is LoC(Fluent + own engine) < LoC(Fluent + jax-js), with more of the
language written in Fluent itself? Evidence: an audit of jax-js's source at the
pinned version (0.1.18), a census of every jax-js call Fluent makes, an audit of
Fluent9000, WebGPU measurements in Chrome, and prelude definitions run against
today's natives. Measured numbers and estimates are marked as such.*

Machine for local numbers: Apple M1 (8-core GPU, 16 GB), Chrome 151, 60 Hz
panel, 2026-10-14/15. Decided: **WebGPU only, no fallback; Chrome first, other
browsers later; f32 only** (all decisions in §11). Updated 2026-10-15
with platform research and measurements (§4–§8). This builds on
[`JankFree.md`](JankFree.md), which owns the worker, the effect-yielding
evaluator and the UI protocol; this document is about the engine and the
language boundary.

---

## 0. The answer in one paragraph

Yes. Fluent today is ~22.6k lines: 3,966 of `language.ts` on top of 17,369
shipped lines of jax-js and 1,313 of optax + loaders. With its own engine it is
**~5.8–6.8k lines, under a third**. Three things make that true, each measured: (1) about **950 lines (24%)
of `language.ts` exist only to adapt Fluent to jax-js** — reference counting,
borrow/adopt, trace bail-outs, jit replay, the optimizer wrapper — and vanish
when the engine owns its buffers and ∇ is a graph rewrite; (2) about **75 of the
~100 native `Tensor*` builtins can be written in Fluent** on ~20 primitives — over
20 families of them already run and match the natives today, including Adam, FFT,
conv, sort, erf and Box–Muller; (3) **Fluent9000 already is the engine core**
(1,243 lines: one IR, ∇ = transpose ∘ linearize, fusion, WGSL, Qwen3 exact) and
grows to ~3–4k to cover Fluent and the platform layer of §6. The platform
fits: batching submits is a 10–15× win on dispatch overhead over jax-js's
submit-per-kernel; the browser keeps compiled shaders on disk, so a return
visit compiles nothing; and on the measured Mac only two things caused hitches
— a visible canvas sharing a device with deep compute, and synchronous compiles
— both of which the design removes (§4.4, §6). The hard part is not the engine;
it is the evaluator integration (async reads, plan caching) — the same "months,
not days" JankFree.md already priced.

| | today | own engine |
|---|---|---|
| `language.ts` (TS + Fluent prelude) | 3,966 | ~2,660 *(est.)* — prelude 367 → ~540 |
| tensor engine | jax-js: 17,369 shipped, 20,628 source *(measured)* | FL9K: 1,243 → ~3,000–4,000 *(est.)* |
| optimizers, loaders | 1,313 *(measured)* | ~100 *(est.)* — safetensors + cached fetch |
| **total** | **~22.6k** | **~5.8–6.8k** |

`client.tsx` is left out of both columns; about 25 of its lines touch jax-js.
Counting only what Fluent exercises on a WebGPU-only path (~16.5k lines of
jax-js source), the ratio is still about 3×.

### 0.1 The governing idea: a small kernel, everything else in Fluent

Owning the stack from syntax to compute puts Fluent where Alan Kay's STEPS
project put Frank: a small kernel, with everything else written in
problem-oriented languages that a metacompiler turns into code — and the
metacompiler written in itself (META II, OMeta). Fluent already stands in that
line: its grammar is written in Ohm, Alessandro Warth's successor to OMeta from
the same group.

For the engine this means:

- **The engine is a kernel.** It holds the ~20 primitives, the graph, and the
  mechanisms — the rewrite engine, the ∇ driver, the scheduler, the WGSL
  printer, the executor, memory. Anything that touches the platform stays here.
- **The vocabulary is Fluent.** einsum, softmax, conv, FFT, the optimizers live
  in the prelude (§1.2) and are just as fast, because the engine fuses what the
  prelude composes and computes constant-only values while building (§6.3) — so
  code that works on specs, shapes and index tables costs nothing per step.
- **Derivatives and laws become values; the rest of the compiler does not.**
  Once a Fluent tensor *is* an engine graph value, a Fluent function applied to
  graph values builds exactly the graph a TypeScript JVP rule builds, so a
  derivative is a line of Fluent — `derivative(exp, { x, dx | exp(x) × dx })`,
  JAX's `custom_jvp` — and users get custom derivatives with it (exact erf′,
  stable softplus, √ at 0). Simplification laws follow: `law({ x | x × 1 }, { x | x })`
  applies both sides to pattern leaves, matches the left graph and rewrites to
  the right, checked numerically on random inputs when registered. The transpose
  rules stay in the kernel — they are the dualities ∇ itself stands on — and so
  do fusion, printing and execution: a cost model and the platform, not laws.
- **The measure** is how small the kernel stays while the language grows: every
  feature asks first whether it can be prelude, then whether it can be a rule,
  and only last whether it must be kernel.

---

## 1. What Fluent uses from jax-js (census, measured)

**104 distinct entry points** (81 of them `np.*`) at 339 call sites in
`language.ts`, plus ~25 in `client.tsx`. The ownership API alone appears 95
times (`.ref` 45, `.dispose` 27, `.refCount` 23): reference counting is part of
the language implementation, not hidden in the engine.

| group | what | notes |
|---|---|---|
| elementwise | 47 functions | ~24 calls are the hand-written Adam/AdaGrad |
| reductions | sum, prod, mean, min, max | no scans |
| movement | reshape ×16, moveaxis, stack, transpose, broadcastTo, slice, pad, roll, flip, tile, repeat, concatenate | reshape is mostly plumbing for ⊗ ⍤ ∙ |
| indexing | take, where, oneHot, sort, argsort, topK, argmax, argmin | tensordot exists only as the ∇-of-gather workaround |
| contractions | matmul, dot, einsum, conv | einsum: 4 gallery uses, all `"btc,bsc->bts"` |
| special | rfft, erf, softmax, logSoftmax | |
| RNG | key, uniform, normal | |
| AD / tracing | vjp, valueAndGrad, jit | |
| optimizers | optax.sgd, applyUpdates, tree.* | Adam and AdaGrad already hand-written |

### 1.1 The primitive set

About **20 numeric primitives**: Fluent9000's 18 (add, mul, recip, exp, log,
sqrt, less, where, detach, reduce-add/max, broadcast, permute, reshape, slice,
gather, scatter-add, constant/buffer) plus **sin, floor, iota, uniform**. A few
more stay native for accuracy, at one WGSL line and one JVP row each: cos,
tanh, atan, pow. (√ through exp/log gives 3.0000002 for √9; a polynomial sin is
off by 1.5e-6.)

### 1.2 Everything else is Fluent (verified against today's natives)

| builtins | definition sketch | max difference from native |
|---|---|---|
| ⌈ ⌊ abs sign, comparisons, logic | `where(a < b, b, a)`, `(0 < x) - (x < 0)`, … | exact |
| ceil trunc % round (half to even) | from `⌊` | exact |
| log1p expm1 cbrt tanh asinh acosh atanh atan2 hypot sinc | log/exp/√ forms with `where` guards | ≤ 2.6e-6; log1p *more* accurate than jax-js |
| erf | Abramowitz–Stegun 7.1.26 — the polynomial jax-js uses | 0 (∇ 1.3e-6) |
| mean prod min argmax argmin | `Σ(x) ÷ Π(shape(x))`, `min(where(x = max(x), 0 ..< n, n))` | exact |
| softmax logSoftmax crossEntropy | `e: exp(x - max(x, -1, 1)), e ÷ Σ(e, -1, 1)` | 0; ∇ 7e-9 |
| oneHot eye linspace fill meshgrid | `i ⊗(=) (0 ..< n)`, … | exact |
| repeat tile roll reverse concat stack pad transpose | gathers with computed indices | exact, ∇ too |
| matmul dot cumsum | `(× ∙ +)`; cumsum = `x (× ∙ +) ((0 ..< n) ⊗(≤) (0 ..< n))` | exact |
| sort argsort topk | rank counting with `⊗(<)` | exact, but O(n²) |
| conv (1-D, 2-D) | windows / gather-im2col then `(× ∙ +)` | 0, ∇ 0 |
| fft | DFT matrix (3 lines) or radix-2 with `cond`/`guard` (9 lines) | 1.5e-6, ∇ matches |
| einsum | parse the spec's character codes into labels, line each operand up by `moveaxis` and `⍴`, broadcast `×`, `Σ` the labels the output lacks; diagonals by a δ mask, `...` as labels numbered from the right (~50 lines) | 0 on 17 patterns, ∇ 0 |
| randn | Box–Muller from uniform | 20k samples: μ −0.005, σ 1.0008, kurtosis 3.02 |
| sgd adagrad adam | `~`, `~~`, `:=`, `∇` | 0, 0, 3.6e-6 after 10/10/20 steps |

What this costs, honestly:

- **Speed depends on fusion.** A prelude softmax is five ops; it is one kernel
  only if the engine fuses map → reduce → map. `(× ∙ +)` as matmul must become
  one broadcast-multiply-reduce kernel, not today's `∙` that unstacks K slices
  (matmul has 48 gallery uses). On jax-js, which fuses nothing in eager mode,
  moving hot builtins to the prelude would be a slowdown — so the order of the
  plan matters (§10).
- **Sort** is O(n²) as rank counting; it needs a native bitonic routine.
- **Optimizers in Fluent** need ∇ with respect to several variables at once;
  today's ∇ takes one tensor; a list argument gives a list of gradients (§11).
- **erf's exact derivative** needs a custom JVP or accepts 1.3e-6.
- **`x ≠ x` as isNaN** can be folded away by WGSL compilers; the engine tests raw bits instead (§6.6).

---

## 2. jax-js: keep the ideas, drop the package

jax-js at 0.1.18 is 20,628 lines of source (cloc, tests excluded): frontend
1.4k, numpy API 1.9k, jaxpr + jit 1.9k, jvp 0.4k, linearize/transpose 0.9k,
vmap 0.5k, kernel IR 1.5k, views 0.6k, tuner 0.8k, WebGPU 2.1k, **Wasm 3.6k**,
WebGL 0.7k, FFT 0.3k, RNG 0.5k, the rest smaller.

**Worth taking** (~6–7k lines' worth of ideas, mostly already in FL9K):

| idea | jax-js | own engine |
|---|---|---|
| small backend contract (malloc, read, prepare, dispatch) | `backend.ts:155–211` | FL9K executor |
| one primitive table, one rule table per transformation | `core.ts`, `jvp.ts:171`, `linearize.ts:688` | FL9K: JVP table + transpose table |
| hash-consed kernel IR with DAG-memoized rewrites | `alu.ts` | FL9K UOp + one rewrite engine |
| zero-copy views (tinygrad ShapeTracker); conv as views | `shape.ts`, `convolution.ts` | views as index expressions (§6.6), tinygrad's newer design |
| fusion: dispatch only at reductions, gathers, fan-out | `jit.ts:792–1093` | FL9K splice + per-thread cost model |
| tuner heuristics (upcast, unroll, group) | `tuner.ts` | FL9K tile and split-K printers |
| RNG as a stateless ALU op, so it fuses anywhere | threefry, `webgpu/builtins.ts` | `uniform` primitive, counter hash |
| routines for what can't fuse | bitonic sort, radix FFT | bitonic sort native; FFT in the prelude |

**What hurts Fluent**, each confirmed in the source, and how an owned engine
removes it:

| problem | evidence | own engine |
|---|---|---|
| move semantics leak into the language (ops consume inputs; ~950 lines of glue) | `array.ts:234–256` | values immutable; buffers owned by the engine |
| identity ops return their input, stranding borrowed refs | moveaxis, reduce over no axes, concat of one, repeat 1, rot90, swapaxes | no references to strand |
| nothing auto-flushes; pending-graph copying is quadratic | `array.ts:579`; submit only from reads | realize at reads and step boundaries |
| constants baked into kernels — a new value is a new shader (worse under jit) | `array.ts:1347`, `jaxpr.ts:723` | constants lifted to inputs; cache key = structure |
| every cache an unbounded Map | 9 caches | generation-scoped / LRU eviction |
| no gather transpose (fixed only in 0.1.19) | `linearize.ts:846` | FL9K: gather ⟷ scatter |
| data reads abort traces; no control-flow primitives | Tracer has no `item` | one model: branches read realized values (§6.9) |
| one `queue.submit` per kernel, a bind group per dispatch, sync reads via canvas `getImageData` | `webgpu.ts:612`, `reader.ts:99` | batched submits; `mapAsync` only |
| optax's adam calls `count.item()` inside jit (fix unpublished) | `optax/transform.ts:96` | optimizers in the prelude |
| every op inside `jit` is staged, constants too (JAX's omnistaging), so metadata code in the prelude aborts the trace — the prelude einsum's spec parse turns the dreamer's compiled step eager, ~4× slower per step | `makeJaxpr` sets `newDynamic(main)` | values that depend only on constants are computed while the graph is built (§6.3) |
| tracers have no reference count Fluent can read; under ∇ Fluent owned every intermediate and released none — each ∇ stranded one buffer per intermediate value, and eager training ran out of memory in ~28 steps (fixed on the jax-js side 2026-10-15) | `Tracer` API; `JVPTracer.dispose` | no references |
| `reciprocal`'s JVP never consumes its input: every `÷` under ∇ strands its divisor | `jvp.ts`, `reciprocal` rule | own rule table |
| two copies of jax-js install silently | optax lists jax as a dependency | no dependency |

Upgrading to jax-js HEAD (0.1.26) fixes only the gather transpose.

---

## 3. Fluent9000 is the seed

1,243 non-empty lines of core + library (the README's "991" is out of date):
`gpu.ts` 520, `opt.ts` 197, `ir.ts` 137, `cpu.ts` 111, `grad.ts` 106,
`rewrite.ts` 54, `lib/nn.ts` 85. One UOp node type from tensors to loop nests;
18 primitives; complete JVP and transpose rules for them; higher-order ∇ (∇², ∇³
pass); fusion by substitution priced per thread; three WGSL printers (map, 16×16
tile, split-K matvec); dispatches submitted 256 at a time inside a validation
error scope. Tests pass today (`t_core` 46 checks on CPU and GPU, `t_census`,
`t_loc`). Qwen3-0.6B decode matches mlx-lm exactly in f32, and runs at
111 ms/token GPU-busy in f16 (Chrome, Metal-3). In jank-free, a hand-lowered looped-dreamer step ran at ~27
steps/s against jax-js's 12–15 in the playground — different harnesses, so
indicative only.

**Gaps against Fluent** *(line counts estimated)*:

| need | status | fix | lines |
|---|---|---|---|
| sin/cos/atan family | missing | SIN (+ ATAN2) primitives, rest prelude | ~45 |
| floor, ceil, round, %, sign | missing | FLOOR primitive (∇ 0), rest prelude | ~20 |
| uniform/normal RNG | missing | counter-hash RANDOM primitive + CPU twin; normal in the prelude | ~35 |
| pad, concat, roll, flip; ∇ of slice | **quadratic**: slice's transpose is a scatter, O(indices × outputs) — 3.4e7 loop iterations for one ∇ of a 4k slice (measured) | views as index expressions (§6.6) — roll, pad, concat, flip and slice-∇ become index math; WGSL has no float atomics, so no atomic scatter | 150–200 |
| sort, argsort, topK | missing | bitonic routine | ~70 |
| gather on any axis, negative indices | axis 0 only | `where` + permute in the prelude | ~15 |
| rfft, conv, einsum, optimizers | library only | prelude / library | ~200 |
| live evaluation (§6.2–6.4) | build-then-realize only; fresh node ids defeat the caches; scalar literals printed into WGSL | structural cache key, lifted constants, generation eviction, async pipeline creation | ~350 |
| jank-free fixes worth keeping | vendored copy is ~800 lines ahead | 2-D dispatch fold (**a correctness fix:** upstream FL9K fails at 2048×2048, 65,536 workgroups on one axis), two scheduler fixes, split-K and register tiles | ~400 |

The vendored copy also writes `enable f16` without checking the adapter for
`shader-f16` — a portability bug to fix during the merge. Its megakernel and
segment passes measured slower; leave them out.

---

## 4. What WebGPU gives the engine

Researched (the WebGPU and WGSL drafts, Chrome's "What's new in WebGPU" series
through 155–156, Dawn sources, WebKit and Mozilla posts, web3dsurvey) and
measured on the M1 above. Survey percentages are web3dsurvey reports.

### 4.1 Core, everywhere — design around these

| capability | what it buys | evidence |
|---|---|---|
| `createComputePipelineAsync` | compiling never stalls a frame; compiles run ~4× parallel | measured: 8 sync matmul compiles gave a 283 ms frame on a page with a visible canvas; async, 0 dropped |
| persistent shader cache (Dawn's blob cache, per profile and origin) | a return visit compiles nothing: 0.2–0.3 ms per pipeline instead of 14–46 ms | measured across a browser relaunch; another origin is cold again |
| explicit pipeline layouts | bind groups shared across kernels and recompiles | `layout:'auto'` bind groups fit one pipeline only (spec); no compile-time cost (measured) |
| `writeBuffer` | constants and small tensors in 1–2 µs (≤ 64 KB) | measured |
| MAP_WRITE staging ring | streaming uploads at 8–10 GB/s | measured (`writeBuffer` 4–6 GB/s at 64 MB, blocking 3.5–9 ms) |
| `mapAsync` + staging ring | reads in ~0.3 ms on an idle queue; latency = queue depth | measured; spec §5.2 |
| `dispatchWorkgroupsIndirect` | GPU-chosen sizes: compaction, and `⍣`-until loops without a read per iteration | spec; Dawn adds a validation dispatch per call *(unmeasured)* |
| storage textures (`bgra8unorm-storage` on 96%) | displays drawn on the GPU, never read back for drawing | measured: 0.06 ms GPU, 0.2 ms JS per 512² frame, vs 2.7 ms JS for readback + JS colormap |
| `importExternalTexture`, also from a `VideoFrame` in a worker | camera frames into compute without copies | measured: 0.15 ms GPU, ~0.8 ms JS per 720p frame (RGB source; NV12 unmeasured) |
| WebGPU and `requestAnimationFrame` in workers | evaluator, engine and pacing off the main thread | Baseline since 2023 |

### 4.2 Optional — take when offered, always keep a core path

| feature | availability | what it buys | measured here |
|---|---|---|---|
| `shader-f16` | 94% (Safari ~100%) | half the bytes for memory-bound ops; faster matmul | f16 storage + f32 accumulate 1.04 TFLOP/s vs f32 0.81 (+28%); f16 accumulate 1.45–1.52 (1.9×) with relative error up to 0.017 |
| immediates (`var<immediate>`, 64 B) | Chrome only | per-dispatch constants: no buffer, no write, no bind group | — |
| `subgroups` | Chrome 83%; not Safari 27, not Firefox | barrier-free reductions and GEMV | no gain for bandwidth-bound reductions (51–54 vs 52–58 GB/s); shuffle-broadcast matmul a loss (0.26 TFLOP/s) |
| `chromium-experimental-subgroup-matrix` | ~0.06%, behind `--enable-unsafe-webgpu`; no origin trial | 8×8×8 tile matmul — the matrix units MLX uses | f32 staged through workgroup memory 0.89–0.96 TFLOP/s; 1.20–1.29 with robustness off (MLX: 1.55–1.59) |
| `timestamp-query` | ~99% (Safari 93%) | calibrating the cost model | quantized to 100 µs without a developer flag |
| WGSL `linear_indexing` | Chrome | simpler index math for the folded 2-D grid | — |

### 4.3 Hard facts

- **Override constants recompile.** Every distinct `override` value is a full
  backend compile — 13–45 ms measured, confirmed in Dawn's source. They are for
  a fixed menu of tile sizes, never for values.
- **10 storage buffers per kernel** on Chrome/Mac (spec default 8; ≥ 16 on ~80%
  of other adapters). The limit forces fused kernels apart.
- **No reusable compute command lists.** Every step is re-encoded: ~0.6 µs per
  dispatch, ~1 µs per `setBindGroup`, ~25 µs per submit (native wgpu figures).
- **One queue, no priority**, and on Mac no concurrent dispatch within a pass:
  independent kernels don't overlap, so fusion is the lever.
- **No float atomics** in WGSL: a scatter-add must be avoided (views) or done as
  a segmented sum.
- **WGSL may assume no NaN or Inf** (§15.7.2). There is no `isnan`, and `x != x`
  may be folded away.
- **Lazy zero-initialization**: on Metal, Dawn clears an uninitialized buffer
  before the first pass that binds it, and a partial `writeBuffer` clears the
  whole buffer first.
- **Watchdogs**: Windows resets the GPU after 2 s; macOS kills command buffers
  that hurt interactivity after a few seconds; Chrome's ~10 s watchdog counts
  strikes against the origin and can **block the site from the GPU** — a real
  risk for a playground running arbitrary user code.
- **No memory-budget API**; on unified-memory Macs the engine keeps its own.
- **Bandwidth is the wall**: everything except matmul ran DRAM-bound at
  50–60 GB/s (M1 peak 68). Vector width and reduction style don't matter; bytes
  do.

### 4.4 What smoothness depends on

Headed Chrome, continuous scrolling plus a CSS animation, 2 s per row;
compositor frames from a Chrome trace (`DirectRenderer::DrawFrame`), not just
rAF:

| scenario | rAF frames dropped | compositor vsyncs missed |
|---|---|---|
| compute only, no visible canvas — submits of 2–100 ms, depth 1–8, main thread or worker, ALU- or memory-bound | 0 | ≤ 1 |
| visible canvas on the compute device, ≤ ~60 ms of work queued ahead of its draw | 0 | 0 |
| same, 80 ms queued | 4 | 22 |
| same, 150 ms queued | 51–59 | 61–66 |
| compute in a worker that also owns a transferred, visible `OffscreenCanvas` | 70 | 79 |
| **separate display device**, under every load above | **0** | **0** |
| sync pipeline-compile storm, no canvas | 4–7 | 8–12 |
| async compile storm | 0 | 0 |

On this Mac the hitches come from exactly two things: **a visible canvas sharing
a device with deep compute**, and **synchronous compiles**. The third failure in
the literature — one command buffer per step starving the compositor (TVM
#19342 on an M5: a 166 ms frame with the JS thread idle) — did not reproduce here
at up to 100 ms per submit. It is real elsewhere, and the watchdogs make long
command buffers a hazard anyway, so the design bounds them (§6.7).

---

## 5. Ideas taken from other engines

Read in source: tinygrad, MLX, TVM/WebLLM, ONNX Runtime Web, TF.js WebGPU,
Burn/CubeCL, Use.GPU, gpu.cpp, Dawn and wgpu, compute.toys, Strudel, the
Observable runtime, Bevy's pipeline cache; JAX/XLA, PyTorch 2 and Triton from
their sources and docs.

| idea | from | in Fluent | lines *(est.)* | verdict |
|---|---|---|---|---|
| hash-consed nodes; cache the **whole realized plan** (kernels, sizes, memory plan, bind groups) by canonical structure | tinygrad's schedule cache; Burn's relativized op streams | a slider frame = re-evaluate (cheap JS), one lookup, re-encode | ~120 | adopt |
| a plan hit is re-encoded, never replayed blind | ORT graph capture — and its silent-wrong bugs #28325, #33362 | correct by construction; keeps trace ≡ eager | (above) | adopt |
| fold, then lift: literals take part in simplification, then leave the shader text | JAX static/dynamic arguments, tinygrad PARAMs, TF.js shapes-in-uniforms (warmup −60–90%) | typing a number or dragging a slider never compiles | ~60 | adopt |
| shape-free map kernels (`n` in the constant block) | MLX shapeless compile, TF.js | resizing an array never compiles a map kernel | ~40 | adopt |
| views as index expressions with a bounds-aware div/mod simplifier | tinygrad's rangeify | roll, pad, concat, flip and the ∇ of slice become index math, never scatters | ~150–200 | adopt |
| the last good program keeps running until every new pipeline resolved | compute.toys, Strudel, Bevy | an edit never stalls a frame | ~80 | adopt |
| generic per-primitive kernels while fused ones compile | Dolphin's hybrid ubershaders; CubeCL on wasm | a brand-new structure runs on its first frame, unfused | ~150 | adapt |
| roofline fusion pricing: time = max(bytes/bandwidth, flops/peak) + launch | XLA priority fusion, Inductor | replaces FL9K's magic constants; the per-dispatch time bound falls out | ~60 | adopt |
| multi-output kernels | XLA, tinygrad | Adam's p, m, v in one kernel; mean with variance; softmax with logsumexp | ~100 | adopt |
| automatic donation (`θ := f(θ)` in place) | MLX `is_donatable`, JAX donation | half the optimizer memory; stable bind groups | ~50 | adopt |
| liveness-based memory plan stored with the plan | tinygrad's memory planner | deterministic buffers, so bind groups are cached | ~60 | adopt |
| one arena binding addressed by offsets | inferred from the binding limit | 1–3 bindings per kernel; the 10-binding limit stops splitting fusions | ~80 | measure first |
| save vs recompute in ∇ (`dots_saveable`) | JAX checkpoint policies, AOTAutograd | long `⍣` unrolls fit in memory | ~30 | adopt (heuristic) |
| custom derivatives as values | JAX `custom_vjp`, tinygrad's CALL | exact erf′, stable softplus and logsumexp, √ at 0 | ~40 | adopt |
| `⍤` by batching rules, not unstacking | JAX vmap, Futhark | an O(1) graph for `⍤` and `∙` — matmul stops unstacking | ~60–100 | adapt |
| GEMV layout: K-contiguous, vector loads, workgroup or subgroup sums | MLX `gemv.h`, steel GEMM tiles | matvecs ~4× closer to bandwidth than FL9K's column reads | ~100 | adopt |
| one "split" scheduling verb; tuning in idle frames, winners cached in OPFS | tinygrad opts/BEAM, Triton autotune, CubeCL's tune cache | a later performance ratchet, never in the edit path | ~150 | later |
| latest edit wins (a version per edit), per-frame coalescing | the Observable runtime | stale work dropped; a busy worker skips to the newest source | ~40 | adopt |
| demand-driven displays, frames only on demand | Observable, Use.GPU | off-screen displays cost nothing; an idle program costs zero GPU | ~60 | adopt |
| slider-only changes rewrite constants without re-evaluating | Use.GPU's lazy uniforms | the fastest slider path | ~100 | later |
| a hitch gate in CI that sees the compositor | Figma, TVM #19342 | typing while training must stay smooth, checked per PR | ~80 | adopt |
| device-loss recovery | WebLLM, Figma | respawn, re-evaluate, restore `~` variables from OPFS | ~50 | adopt |

**Not copied:** tinygrad's WebGPU runtime (a pipeline, layout and bind group per
call, with synchronous waits); general symbolic shapes; tuning in the edit path;
a jit-versus-eager split or any user-visible `jit`, `eval`, `donate` or
`checkpoint` knob; Dynamo-style bytecode capture (the structural key is the
guard); scan, while and cond as IR primitives (branches read realized values,
so reverse mode through data-dependent loops comes free); Taichi-style AD of
fused kernels (it needs float atomics); subgroups in v1 (no gain measured here,
absent in Safari and Firefox).

---

## 6. The design, tailored to WebGPU

### 6.1 Threads and devices

```
main thread                                  │ evaluation worker
React · Monaco · markers (from the worker)   │ evaluator (effect-yielding), one version per edit
display GPUDevice — draws tensor displays    │ compute GPUDevice — never presents
  from bytes it receives                     │ engine: plans · kernels · pool · scheduler
input ── seeded scalar writes ──────────────►│ worker rAF is the clock; fences cap depth
◄── mirrors + RGBA8 / series bytes, per frame│ colormap and normalize on the GPU before any read
```

The compute device **never presents**. Displays live on a **separate display
device** on the main thread, fed by bytes the worker reads back and transfers:
0 dropped frames under every measured load, against 51–79 when a canvas shares
the compute device — from the main thread or from a worker. The bridge costs
~0.1 ms per MB on the main thread, kept that low by packing pixels to RGBA8 on
the GPU first (0.2 ms vs 2.7 ms for a JS colormap). Its latency equals the
compute queue depth, so depth is bounded for **freshness**, not for smoothness.

### 6.2 Never block

- Pipelines are created only with `createComputePipelineAsync`, many at once.
- An edit builds its new program while **the last good one keeps running**; it
  swaps in at a ⟳ step boundary once every pipeline resolved. Displays show the
  last good values, dimmed while pending. A syntax error changes nothing but
  the markers; training keeps going.
- A brand-new structure can run on generic per-primitive kernels (compiled at
  worker boot, rank-generic, shapes in the constant block) until its fused
  kernels arrive, so a cold edit never waits for a compiler.
- WGSL is **deterministic** — no node ids, stable names and ordering — so the
  browser's persistent cache makes a return visit start warm. One fixed origin
  (GitHub Pages already is one).
- Reads are `mapAsync` only. There is no synchronous read path.

### 6.3 A kernel is its structure

The IR keeps literal values so the laws can fold `x × 1`, `x × 0`, `x ^ 2`.
A value that depends only on constants is computed while the graph is built —
it never becomes a node — so metadata code in the prelude (einsum's spec parse,
shape arithmetic, index tables) costs nothing per step and never stops a plan
from being cached. **After** folding, every remaining constant leaves the WGSL
text and travels in a per-step constant block. Map kernels are shape-free (`n` is in the block).
Reductions and matmuls specialize on a small menu of tile variants chosen by
shape bucket — the only use of override constants. The cache key is the
canonical structure plus the feature tier. Constants travel as immediates where
Chrome offers them, otherwise in one uniform ring written by one `writeBuffer`
at the start of each step and addressed with dynamic offsets.

### 6.4 Plans: evaluate every time, compile once

Nodes are hash-consed per generation, so building the same program twice yields
the same graph. A **plan** is everything realization produces for a graph:
fused kernels, dispatch sizes, a liveness-based memory plan, and the bind
groups over it. Plans are cached by the graph's canonical signature — buffers
numbered by first use, scalars as slots. On a hit (every slider frame, every
training step, most keystrokes) the engine skips simplify, fuse, schedule and
codegen, and only re-encodes the dispatch list. The evaluator still runs every
time (~146 ns per 20-node tree, JankFree F6), so control flow always sees real
values: a cache, never a replay.

### 6.5 Memory and bindings

Explicit canonical layouts keyed by binding signature; bind groups keyed by
(layout, buffer ids that are never reused). A size-bucketed pool scoped to the
evaluation generation, with whole-buffer writes (no lazy-clear surprises),
eviction, and an engine-enforced memory budget. Automatic donation: `θ := f(θ)`
writes into θ's buffer when θ is its only holder and the kernel reads θ only at
the index it writes. **To measure first:** a single arena storage binding for
all intermediates, addressed by offsets. It would lift the 10-binding limit and
leave one bind group for everything, at an unknown cost for giving up read-only
qualifiers.

### 6.6 Kernels

Everything but contractions is bandwidth-bound (50–60 GB/s measured), so the
levers are fewer bytes: fusion, multi-output kernels, recompute instead of
save, f16 storage. Fusion is priced by a roofline model whose three constants
per adapter (bandwidth, FMA rate, launch cost) are calibrated once with
timestamp queries in idle time and cached. Views are index expressions, so
slices, pads, rolls, flips and concatenations never materialize and their
transposes never scatter. Grids beyond 65,535 workgroups fold into two
dimensions. NaN checks test raw bits in a separate kernel; ±Inf are bitcast
constants.

**Matmul, f32.** The f32 FMA ceiling through WebGPU on the M1 is ~2.4 TFLOP/s
(an earlier 1.05 was loop overhead in the probe). Scalar kernels — a 4×8 or 8×4
register block per thread, K in steps of 16 through workgroup memory, a rolled
inner loop — plateau at **0.85–0.92 TFLOP/s** from 1024² to 4096², across ~35
measured variants: unrolling, deeper K, prefetch, double buffering and direct
loads were all slower, and bounds checks cost nothing measurable. That plateau
is the outer-product scheme's limit, not the shader's. MLX reaches 1.55–1.59
because it uses the GPU's matrix units; WebGPU's door to them is
subgroup-matrix, which reaches 1.20–1.29 but only behind a flag (§4.2). So the
core path is the scalar kernel (~0.6× MLX), and a subgroup-matrix kernel is
added behind feature detection the day Chrome ships it. Small and skinny
matmuls are bandwidth-bound and go to GEMV.

### 6.7 Scheduling

- **Clock:** the worker's `requestAnimationFrame`. Fences only cap the depth —
  Firefox polls GPU completion every 100 ms, and a fence-paced loop collapses
  there.
- **Depth:** at most 2 steps in flight; this is the display's freshness bound.
- **Batching:** 16–32 dispatches per submit, plus a cap of about one frame
  (~16 ms) of estimated GPU time per command buffer — the guard against
  watchdogs and against GPUs that behave like TVM's M5.
- **Long kernels:** a dispatch estimated over ~20 ms is split (JankFree F4).
- **Priority:** edits and interactive reads interrupt training — the worker
  yields between steps, and after one queue drain a read is a few milliseconds.
- **Latest wins:** each edit carries a version; a stale evaluation stops at its
  next yield, and stale results are dropped.
- **On demand:** displays outside the viewport stop requesting projections; with
  nothing animating the worker requests no frames and the GPU idles.
- **No hidden submits:** writes and frees never flush the encoder.

### 6.8 Data in and out

Constants and small tensors by `writeBuffer`. Streaming uploads — weights, and
camera frames where import isn't available — through a MAP_WRITE ring in the
worker, in chunks. Camera frames by `importExternalTexture` from a `VideoFrame`
in the worker. Reads through a ring of 3–4 MAP_READ buffers, batched per frame.
Training scalars such as the loss go to a device ring and are mirrored every k
steps. Never `await onSubmittedWorkDone` before `mapAsync`. Data-dependent loops
(`⍣` until convergence, `solve`) are candidates for indirect dispatch: encode N
iterations, let a convergence kernel zero the remaining dispatch counts, read
once.

### 6.9 ∇

`∇(f)(x)` evaluates `f` on a graph leaf and applies FL9K's transpose ∘
linearize to the graph that came out. A branch (`guard`, `cond`, `⍣` until)
reads the realized value it is at, so the graph records the branch taken and the
derivative is that branch's — JAX's `grad` semantics, and the contract today's
`∇` already has on jax-js. Trace and eager agree by construction; the gradient is
itself a graph the AST view can show; higher order is the same rewrite again.
The branch of a `where` not taken gets an exact zero cotangent that stays zero
through its backward chain, so 0·∞ = NaN never leaks from it, and `∇max` splits
ties evenly as JAX does (§11).

### 6.10 The editor itself

The source is parsed in the worker and markers come back with the mirrors —
today `validateCode` re-parses on the main thread on every keystroke. Mirrors
are coalesced per frame and rendered in a React transition. Monaco stays as it
is.

### 6.11 Robustness and tiers

Features are detected once; the feature tier is part of every cache key; every
kernel has a core path (no subgroups, no immediates, no f16). One error scope
per step, popped asynchronously. On `device.lost`: request a new adapter
(Chrome 140+ consumes adapters), respawn, re-evaluate, restore `~` variables
from an OPFS checkpoint. The per-command-buffer time cap also keeps the origin
clear of watchdog strikes.

---

## 7. Measured foundations

Apple M1 (8-core GPU), Chrome 151, 2026-10-14/15. Medians of repeated runs,
forward/reverse/forward to catch drift; GPU times from timestamp queries with
the developer flag (≈3.5 µs resolution). Benchmarks and raw JSON are in the
session scratchpad.

| | measured |
|---|---|
| 540 tiny kernels: one submit / a submit each / chunks of 64 | 2.8 / 31.5 / 2.1 ms |
| pipeline creation, cold: 12-op map / 128² matmul | 14 / 46 ms |
| same text again, same session | 0.1–0.3 ms |
| same text after a browser relaunch, same profile and origin | 0.2–0.3 ms |
| new `override` value | a full compile (13–45 ms) |
| first dispatch after an async create | +0.3–0.4 ms |
| matmul f32, register-blocked 4×8 or 8×4 per thread, K-step 16 | 0.85–0.92 TFLOP/s (1024²–4096²) |
| matmul f16 storage, f32 accumulate / f16 accumulate | 1.04 / 1.45–1.52 TFLOP/s |
| matmul subgroup-matrix f32, staged / robustness off | 0.89–0.96 / 1.20–1.29 TFLOP/s |
| matmul f32, MLX native (Metal, same machine) | 1.55–1.59 TFLOP/s |
| FMA ceiling through WebGPU, f32 | ~2.4 TFLOP/s |
| reductions and maps over 16M elements | 50–60 GB/s (M1 peak 68) |
| upload 4 KB `writeBuffer` / 64 MB staging ring | 1–2 µs / 8.8–9.7 GB/s |
| readback 16 B / 16 MB with a staging pool | 0.3 / 3.8 ms |
| 512² display: compute-to-canvas / readback + JS colormap | 0.06 ms GPU, 0.2 ms JS / 2.7 ms JS |
| display bridge to a separate device, 1 MB | ~0.1 ms main thread |
| camera frame 720p via `importExternalTexture` | 0.15 ms GPU, ~0.8 ms JS |

Unknown: any GPU but Apple Silicon; Windows, Android and Firefox behaviour;
NV12 camera frames; long-session memory behaviour; whether a second device
inside the worker escapes the canvas coupling (only a main-thread display
device was measured).

---

## 8. Corrections to JankFree.md

1. **Chunk size 16–32, not 64.** TVM's sweep peaks at 32 dispatches per submit
   and drops past it; production runtimes sit at 15–32.
2. **Pace by rAF, not by fences.** Fences only cap depth; Firefox's 100 ms
   completion polling breaks fence-paced loops.
3. **Compute alone did not jank this Mac** at up to 100 ms per submit and depth
   8, from either thread. The ≤ 20 ms dispatch rule stays as a portability and
   watchdog guard, not a Mac requirement.
4. **The display-device rule holds and sharpens.** A visible canvas tolerates
   ~60 ms of compute queued ahead of it; 80 ms already drops frames. An
   `OffscreenCanvas` in the compute worker does **not** escape the coupling; a
   separate device does.
5. **There is a persistent shader cache** (per profile and origin).
6. **Override constants recompile** — they can't carry values.
7. Fluent9000 is 1,243 lines, not 991.

---

## 9. Risks

1. **Integration size.** The evaluator work (async reads, plan caching,
   generation-scoped buffers, the worker split) is still the largest part.
2. **One machine.** Everything was measured on an M1; an M5 behaved differently
   for TVM. Windows, NVIDIA/AMD/Intel and Android numbers are needed before the
   per-buffer budget is trusted.
3. **Cold starts.** A first visit compiles every kernel (14–46 ms each, ~4×
   parallel) until the generic kernels and the disk cache take over.
4. **Numerics.** f32 only; f16 accumulation only as an explicit choice;
   NaN/Inf assumptions handled outside fused code.
5. **∇ correctness** for every new primitive and view — finite-difference
   fuzzing, as FL3K did.
6. **GPU blocklisting** through watchdog strikes from runaway user code —
   mitigated by the per-buffer time cap and dispatch splitting.
7. **Firefox** is a functional target only (~1 ms per dispatch, ~100 ms per
   round trip as of early 2026).
8. **Fork drift** between Fluent9000 and the jank-free copy.

---

## 10. Plan — each step ships; jax-js stays the oracle until S4

1. **Consolidate the engine** — *done 2026-10-15*: `engine/`, 1,200 lines
   (Fluent9000 1,243; the jank-free copy 2,019). f32 only; the jank-free fixes
   kept (2-D grid fold, workgroup 256 for maps, split-K register tiles, chained
   weight-grad reductions in tiles and GEMV, Σ-linearity, use counts through
   view chains), the megakernel, segment and f16 paths and the ~40 experiment
   toggles dropped. A reduce dragged along by a shared fork is priced by its
   extent against the per-element budget (it was a flat 10⁶, which split
   softmax rows). WGSL is deterministic across rebuilds. 47 gates pass in
   headless Chrome on WebGPU without flags: `bun engine/tests/run.ts`.
Then the swap, every stage green (Milan, 2026-10-15: "Tests/gallery has to run
as if nothing is broken"):

2. **S1 — engine gaps** — *done 2026-10-15* (35 op gates in Chrome; the CPU backend
   now prints each kernel as a JS function): a table-driven unary op (sin, cos, tanh, atan, floor,
   …: one JS function, one WGSL builtin, one derivative each), a counter-hash
   RNG that fuses anywhere, a product reduce, host routines for sort, argsort
   and topK, and a JS-codegen CPU backend — the tree-walking oracle is far too
   slow for the test suite.
3. **S2 — Fluent's native layer on the engine, in Bun** — *done 2026-10-15
   except one test*: `engine/numpy.ts`, `jax.ts`, `optax.ts` (jax-js's surface,
   checked against jax-js call by call in `engine/tests/oracle.bun.ts`);
   `bun test` 322/323 in 19 s (jax-js: 22 s). What it took: values that cross a
   step are *settled* into data leaves (a variable's value, the optimizer's
   state) — otherwise every graph and every ∇ walk drags the whole history
   along (a 60-step fit took 691 s); the optimizer realizes loss, parameters
   and state in one batch; an op consumes its inputs as in jax-js, so the
   ownership tests keep their meaning until S4; the engine dropped the unsound
   law x × 0 = 0 (0 × ∞ is NaN) and broadcasts 1 to 0 as numpy does; a literal
   integer exponent is exponentiation by squaring in the engine (so `intPow`
   left `language.ts`). Open: `(cos ⍣ ≡)(1)` — with correctly rounded f32 cos
   the Dottie iteration is a 2-cycle (0.73908508 ↔ 0.73908520); jax-js's own
   cos polynomial happens to have a fixed point (§11). The browser still runs
   this on the CPU — do not deploy before S3. values are lazy graph
   nodes realized on read (synchronously on the CPU), ∇ is the engine's `vjp`,
   `jit` disappears into the plan cache, optimizers are graphs. `bun test`
   passes with no jax-js on its path.
4. **S3 — the browser** — *done 2026-10-15*: Playwright 37/37 in Chrome, all 52
   gallery examples run (headless sweep, no engine errors), `bun test` 324/324,
   and the dreamer trains **1.6× faster than on jax-js** (2,428 vs 1,508 steps
   in 20 s; 9.2 ms per step, 110 kernels). What it took: results stay on the
   GPU as device leaves (a buffer per computed node, alive as long as its
   leaf); a small graph over host data runs on the CPU — the placement rule is
   the *work* (Σ of every computed node's size), not the input sizes; the
   read bridge (canvas `getImageData`, as jax-js) only for synchronous reads,
   `data()` reads asynchronously; a step commits without reading anything
   back; a tensor stored in a signal or variable is settled into a leaf; the
   structural plan cache (ops, args and shapes in canonical order, leaves by
   position — a step built afresh each tick finds its compiled plan); buffers
   recycled by size (a fresh one is zero-cleared first); a **bounds-aware
   index simplifier** in the index constructors — views stop re-dividing the
   index at every level (29 → 17.5 ms per step; the weight-grad tiles 9×
   faster); a contraction is **not spliced into a reduction's body** unless it
   becomes the whole, tileable body (17.5 → 9.2 ms); conv is one reduction
   over a `WINDOW` view (a 27×27 lenia kernel was 729 graph terms). Not yet:
   literal scalar constants still sit in kernel text (§6.3's lifted constants);
   what changes between runs — a random seed, a value that crosses time — is
   data, so it rebinds the plan (S5).
5. **S4 — delete** — *done 2026-10-15*: `@jax-js/jax`, `optax` and `loaders`
   are gone from `package.json`; nothing imports them. The loaders became an
   own safetensors reader and OPFS cache (44 lines of `client.tsx`, identical
   to jax-js's on `mnist.safetensors`). The ownership glue went with them —
   borrow/track/arena, disposal scopes, the leak watch, `.ref`, trace
   bail-outs, the jit'd lifted apply: a state write while differentiating is a
   `StateWrite` that ∇ and the optimizer turn into an Error, and a lifted
   application is a `computed`. `≡` matches within float rounding (relative
   2⁻²⁰, APL's tolerant equality); `=` stays exact. Green: `bun test` 322/322,
   Playwright 37/37 in Chrome, all 52 gallery examples, the engine's gates.
   Lines: `language.ts` 4,012 → 3,199; jax-js + optax + loaders (18,682
   shipped) → `engine/` 2,481 (IR, ∇, fusion, CPU and WGSL printers, and the
   numpy-shaped layer Fluent calls) + 44 — Fluent is ~5.7k lines, a quarter of
   the ~22.7k it was. The bundle is 10.23 MB against 10.55 MB (Monaco and
   Plotly are most of both).
From S5 on, each step moves something out of TypeScript into the prelude, or out
of the engine's API layers into the engine's primitives, and the line counts are
the gate (Milan, 2026-10-15: "compress the code via pushing as much as we can to
userland"). Baseline: `language.ts` 2,835 TS lines + a 364-line prelude,
`engine/` 2,481, `client.tsx` 3,171 — 8,487 lines of TS. Census of every native
and engine export: 153 environment names = 38 engine primitives, 35 that must
stay native (evaluation, reflection, signals, strings, I/O), 67 expressible in
Fluent today, 11 expressible with one new primitive.

6. **S5 — The primitive core** — *done 2026-10-16*. What landed:
   - *Folding:* a value that depends only on constants is computed as it is
     built (up to 256 elements), each op as its kernel computes it – a
     property gate checks all 45 ops against the CPU kernels, bit for bit.
     Fluent shape arithmetic went from 33–72 µs to ~9–11 µs a call.
   - *IOTA* is a leaf computed where it is read; ranges, one-hot rows and
     index tables stop uploading arrays. `eq` is one engine row, NaN-safe on
     the GPU (as a composite every guard paid 50 µs for it).
   - *Into the prelude* (each checked against its native on the same
     arguments – 189 cases): − ÷ neg √n log-base, > ≤ ≥ ≠ ∨ ∧ ¬ ⊻, ceil
     trunc hypot sinc erf mean %, ranges linspace fill, roll reverse tile
     repeat pad moveaxis transpose, softmax logSoftmax crossEntropy argmax
     argmin fft randn, ⊗ ⍤ ∙ mask ≡. `∙` is now one fused contraction; `≡`
     reads back one number, not both tensors.
   - *New in the core:* ◌ is an argument not given (natives agree with
     lambdas); isTensor isList isString isError identical; permute broadcast
     padAxis window take scatterAdd detach. The prelude is `prelude.fl`.
   - *Sorting on the GPU:* data on the device sorts by a bitonic network
     (one gather and one select a pass, partners computed in the kernel); data
     the host has sorts there. Stable, NaN last. The HOST op and its
     synchronous read inside a plan are gone.
   - *The facade dissolved:* `engine/tensor.ts` (`numpy.ts`, `jax.ts`,
     `nn.ts`, `optax.ts` gone); one value type – the dtype tags went; the
     test-only executor went (the gates run the shipping path); one plan
     cache for CPU and GPU over placeholder leaves, so a cached plan holds no
     data. Fixed on the way: a zero-size CPU result came back as one element;
     `%` and ∇ uploaded full-size zeros and ones; ⟳'s index was a literal.
   - *Size:* `language.ts` 3,199 → 2,599, prelude 364 → 462, `engine/`
     2,481 → 2,239. Training throughput unchanged (dreamer 8.7 ms a step,
     looped dreamer 37.3 ms). Less than planned moved, for measured reasons:
     a Fluent application costs ~1–3 µs of host time, so code that runs dozens
     of them per call stays native until the evaluator is fast (S7) – matmul,
     dot, einsum (einsum.fl took 12.7 ms a call), the bitonic network (30–110
     ms to build in Fluent). The variadic natives – the combinators, cascade,
     cond, List, concat, meshgrid – stay: a Fluent lambda takes a fixed number
     of arguments (open: a rest-parameter form). So do `_`, reduce and the
     arity dispatchers it is keyed by, and the natives other natives call.
7. **S6 — Learning in userland** — *done 2026-10-16*. `∇(loss)()`, with no
   argument, differentiates with respect to the `~` variables the loss reads
   (a variable's read reports itself while ∇ follows the loss) and gives
   (value, variables, gradients), computed in one pass; `:=` on a list of
   variables commits them together. Two optimizers no longer step each other's
   variables. An optimizer is a value: `optimizer(init, rule, lr, vars?)`, and
   on it sgd (momentum), adam, adamw, adagrad, rmsprop, adadelta, adamax,
   nadam, radam, amsgrad, adabelief, yogi, lion, lamb, muon; warmup and cosine
   schedules (lr as a function of the step); clipNorm (a rule wrapping a
   rule). Checked against references written from optax's definitions.
   `makeOptimizer`, the transforms and the variable registries are gone;
   training throughput unchanged. Not yet: L-BFGS (a history and a line
   search), Adafactor (factored moments), schedule-free AdamW, LARS, EMA and
   lookahead; Shampoo and SOAP wait for linear algebra (S9, skipped).
8. **S7 — JankFree: the worker** (JankFree.md §3, on the smaller core).
   - The evaluator, the engine and the compute GPUDevice run in a worker;
     the main thread keeps React, Monaco and input. Reads are effects
     (JankFree's generator trampoline): the synchronous read bridge and the
     sync realize path are deleted. Signals reach the page as mirrors;
     widgets travel as descriptions; images and heatmaps draw on a separate
     display device from small packed readbacks.
   - Per-step host cost falls with it: state keeps stable leaves (a
     variable's buffer is rebound, its node stays), so a step rebuilds the
     same graph and every pass — ∇, fusion, lowering — is found by structure.
   - Done on the main thread already: a ⟳ step waits until at most two steps
     are queued on the GPU, displays read asynchronously, `CodesToString` of
     codes still on the GPU gives a signal that fills when they arrive (a
     lifted application reads through a signal that holds a signal), the
     host-work estimate is memoized, a random seed and every value that
     crosses time are data rather than kernel text, a rank-0 device result
     has the same shape of leaf as a held host scalar, and the plan key
     ignores whether a leaf is host data or a device buffer. The looped
     dreamer trains at 60 fps (360 frames in 6 s, p95 17.6 ms; jax-js: 86
     frames, p50 67 ms); typing into it drops to ~5 fps, because each
     keystroke re-evaluates the program — two dreams, 58 transformer passes —
     on the main thread.
   - *Gate:* the hitch test in CI — typing and training together hold 60 fps.
9. **S8 — META: rules as values.**
   - `derivative(sin, { x, dx | cos(x) × dx })`: the elementwise and MATH
     JVP rows become prelude values. Each is applied once, at registration, to
     placeholder leaves, giving a graph template the engine instantiates per
     node by substitution — TypeScript speed every step, Fluent as the source.
     The linear ops collapse to one property; transposition, which reads
     axes and slices, stays TypeScript.
   - Users get custom derivatives: stop-gradient and straight-through
     estimators, erf's exact derivative, the implicit-function rule for
     `solve` and for S9's linear algebra.
   - `law({ x | x × 1 }, { x | x })`: the algebraic laws become values,
     compiled to patterns for one matcher; view laws keep their guards.
   - Batching rules give a real vmap: `⍤` and `∙` build one batched graph
     instead of a copy per cell.
   - The primitive table itself (name, arity, JS and WGSL text) becomes data
     a program can extend.
   - *Needs:* records, for anything that reflects a node's structure.
10. **S9 — Numerics in userland.** In the prelude, from existing primitives:
    triangular solve, Cholesky, LU with partial pivoting, QR, `solve`/`inv`/
    `det`/`logdet`, `eigh` (Jacobi), SVD (one-sided Jacobi), matrix roots
    (Newton–Schulz) — each a ⍣ loop of fused kernels, fine to a few hundred
    dimensions; scans by doubling (cumsum, cumprod, cummax in log n
    kernels); radix-2 FFT for large n; categorical, gamma, beta, Dirichlet,
    permutations; then the optimizers that need them (Shampoo, SOAP, natural
    gradient). ~300 Fluent lines, no TypeScript.
11. **S10 — The IDE in Fluent.** An `Element(tag, attrs, children)` primitive
    and an event source; Button, Slider, Checkbox, TextEditor, Grid, Layers,
    Center, Trail, the mouse and the clock as Fluent values; bar, scatter and
    heatmap plots as SVG elements written in Fluent — Plotly already renders
    only static SVG here, so the multi-megabyte dependency goes; the
    documentation and the symbol table generated from the prelude's doc
    cards; the playground itself a Fluent program. `client.tsx` 3,171 →
    ~2,450 TS + ~200 Fluent. GPU-drawn plots come after (Milan: later).
    *Needs:* records, for attributes.

After S10 (estimates): ~6,100 lines of TS (−28%) and a ~1,150-line prelude —
with every optimizer, linear algebra, vmap, GPU sorting, custom derivatives and
the worker added. Beyond: the grammar and the evaluator's rules as Fluent
values (OMeta), and the printers as tables — the language describing itself.

Decisions these steps need (Milan):

1. `◌` means an absent argument everywhere (S5).
2. The dtype layer goes: every value f32, bool and int only as printing tags;
   a NaN condition in `where` is the one observable nuance (S5).
3. Optimizers train the variables their loss reads; an explicit list stays
   possible (S6).
4. Records — their design gates S8's structural rules and S10's attributes.
5. Plotly → plots written in Fluent (S10): a visual change.
6. The prelude in its own `.fl` file, so `language.ts` is TypeScript only.

The platform layer of §6 (async compiles, constant block, shape-free maps,
explicit layouts, memory plans, staging rings, rAF scheduler) and the kernel
set (index-expression views, multi-output kernels, the measured matmul tile,
roofline pricing) land inside S3–S7 where the gallery needs them.

---

## 11. Decisions

Milan, 2026-10-15:

1. **f32 only**, for now. No f16 storage or accumulation.
2. **Chrome first**; other browsers later. Subgroups and immediates (stable
   Chrome, no flag) may be used; every kernel still keeps a core path.
3. **What can live in user space, does** — einsum included. The prelude einsum
   exists (§1.2) and lands with the engine, which computes constant-only values
   while building (§6.3); on jax-js it would make compiled steps eager, so the
   native binding stays until the swap.
4. **Tests run in headless Chrome with WebGPU** — never only in Deno or Bun,
   whose WebGPU doesn't enforce the browser's limits.

Delegated to the engine work (Milan: "no idea"), decided as follows; each is
easy to revisit:

5. **Reads:** the effect-yielding evaluator is the destination (S5). Until it
   lands, a synchronous read bridge — the canvas `getImageData` readback jax-js
   uses today — keeps the gallery green (S3); it is removed in S5.
6. **∇ over several values:** no new form. `∇(f)(x)` where `x` is a list gives
   a list of gradients, so a prelude optimizer is `∇(loss)((θ, φ))`; a list is
   already a value.
7. **Unused natives** move to the prelude; none are dropped.
8. **Sort:** on the GPU, as a bitonic network written in the prelude from
   existing primitives (gather, compare, select) — no routine in the engine
   (revised 2026-10-15 with the S5 plan; Milan: "we definitely want to sort
   on the GPU").
9. **Cotangents through `where`:** the branch not taken gets an exact zero that
   stays zero through its backward chain (0 × ∞ = 0 there), so NaN from the
   branch not taken never leaks. This deletes the double-`where` idiom from the
   prelude (`solve`'s safe division, `log1p`'s guard). It is the one semantic
   change to ∇ in this list — veto it if it surprises you.
10. **`∇max` ties** split evenly, as JAX does — a true subgradient.
11. **Limits:** request the adapter's maxima (buffer size, storage bindings per
    stage); Chrome-first makes portability a later concern.
