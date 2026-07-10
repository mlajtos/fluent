// End-to-end tests: Fluent source in, evaluated result out.
// Runs the real pipeline – grammar → AST → evaluator → tensors – on the Wasm
// backend (language.ts initializes jax-js on import). `bun test` runs this file.

import { describe, test, expect } from "bun:test"
import { Signal } from "@preact/signals-core"
import { CodeParse, evaluateSyntaxTreeNode, evaluateGeneration, createScope, np, FluentVariable, setCodeNodePrinter, type SyntaxTreeNode, type Value } from "./language"

const run = (source: string) =>
  evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(source), createScope()))

// signals unwrap to their current value, tensors to plain JS arrays/numbers
const value = (source: string): any => {
  let v: any = run(source)
  while (v instanceof Signal) { v = v.value }
  if (v instanceof FluentVariable) { v = v.current }
  if (v instanceof np.Array) { return v.ref.js() }
  return v
}

describe("whitespace is precedence", () => {
  test("spaced operators chain left-to-right", () => {
    expect(value("1 + 2 * 3")).toBe(9)
  })
  test("glued operators bind tighter", () => {
    expect(value("1 + 2*3")).toBe(7)
    expect(value("2*3^2")).toBe(36) // tight tier is still left-to-right
  })
  test("glued-left operator takes long right scope", () => {
    expect(value("a: 1 + 2, a")).toBe(3)
    expect(value("a: b: 2 + 3, a")).toBe(5)
    expect(value("x: 1 :: 4, sum(x)")).toBe(6)
  })
  test("assignment evaluates to the assigned value", () => {
    expect(value("a: 1 + 2")).toBe(3)
  })
  test("kebab-case identifiers absorb hyphens", () => {
    expect(value("foo-bar: 7, foo-bar")).toBe(7)
  })
  test("scientific notation", () => {
    expect(value("1e-3 × 1000")).toBe(1)
  })
})

describe("functions", () => {
  test("lambda definition and application", () => {
    expect(value("sq: { x | x^2 }, sq(5)")).toBe(25)
  })
  test("lambda as infix operator", () => {
    expect(value("1 {x, y | x + y} 2")).toBe(3)
  })
  test("recursion via self, cascade and guard", () => {
    const fact = `fact: { n | f: self, cascade((guard(n = 0, { 1 }), { n * f(n - 1) }))() }, fact(5)`
    expect(value(fact)).toBe(120)
  })
  test("function power ⍣ iterates synchronously", () => {
    expect(value("double: { x | x × 2 }, (double ⍣ 5)(1)")).toBe(32)
  })
  test("ad-hoc operators", () => {
    expect(value("(++): ListConcat, ListLength((1, 2) ++ (3, 4))")).toBe(4)
  })
})

describe("tensors", () => {
  test("broadcasting", () => {
    expect(value("[1, 2, 3] + 1")).toEqual([2, 3, 4])
  })
  test("indexing, negative indexing, length", () => {
    expect(value("a: [10, 20, 30], a_1")).toBe(20)
    expect(value("a: [10, 20, 30], a_(-1)")).toBe(30)
    expect(value("#([1, 2, 3])")).toBe(3)
  })
  test("linspace rounds a fractional point count", () => {
    // a live slider drives the count through fractional values – round, don't reject
    expect(value("#(linspace([0, 1], 5))")).toBe(5)
    expect(value("#(linspace([0, 1], 4.6))")).toBe(5)
    expect(value("linspace([0, 10], 3)")).toEqual([0, 5, 10])
  })
  test("reshape and range", () => {
    expect(value("[[1, 2], [3, 4]] ⍴ [4]")).toEqual([1, 2, 3, 4])
    expect(value("0 :: 4")).toEqual([0, 1, 2, 3])
  })
  test("new jax-js builtins: fft, meshgrid, einsum, topk, pad, repeat, flip, sinc", () => {
    // fft returns [real; imag] stacked; a period-4 wave peaks at bin 2
    // (round: jax-js ≥0.1.18 leaves ~1e-16 residuals in the near-zero bins)
    expect((value("f: fft([0, 1, 0, -1, 0, 1, 0, -1]), √(f_0^2 + f_1^2)") as number[]).map(x => Math.round(x))).toEqual([0, 0, 4, 0, 0])
    // meshgrid and topk return lists of tensors – pull one out with ListGet
    expect(value("ListGet(meshgrid(0 :: 3, 0 :: 2), 0)")).toEqual([[0, 1, 2], [0, 1, 2]])
    expect(value("einsum(\"ij,jk->ik\", [[1, 2], [3, 4]], [[5, 6], [7, 8]])")).toEqual([[19, 22], [43, 50]])
    expect(value("ListGet(topk([3, 1, 4, 1, 5, 9, 2], 3), 0)")).toEqual([9, 5, 4])
    expect(value("pad([1, 2, 3], 2)")).toEqual([0, 0, 1, 2, 3, 0, 0])
    expect(value("repeat([1, 2], 3)")).toEqual([1, 1, 1, 2, 2, 2])
    expect(value("flip([1, 2, 3])")).toEqual([3, 2, 1])
    expect(value("sinc(0)")).toBe(1)   // sinc(0) = 1 by definition
  })
  test("stack with axis and broadcasting", () => {
    expect(value("stack(([1, 2], [3, 4]), 1)")).toEqual([[1, 3], [2, 4]])
    expect(value("stack(0 :: 3, 9)")).toEqual([[0, 1, 2], [9, 9, 9]])
  })
  test("comparisons are float 0/1 and compose with arithmetic", () => {
    // jax-js bools promote through uint32, where weak negatives clamp to
    // zero and subtraction silently no-ops – Fluent comparisons are APL 0/1
    expect(value("([1, 2, 3] = 1) + 10")).toEqual([11, 10, 10])
    expect(value("f: { x | (x = 1) ⌈ (x × 0) }, b: [1, 2, 3], sum(abs(f(b) - [1, 0, 0]))")).toBe(0)
  })
  test("logical ops are float 0/1, nonzero counts as true", () => {
    expect(value("[0, 1, 0] ∨ [0, 1, 1]")).toEqual([0, 1, 1])
    expect(value("[0, 1, 1] ∧ [1, 1, 0]")).toEqual([0, 1, 0])
    expect(value("[0, 1, 1] ⊻ [1, 1, 0]")).toEqual([1, 0, 1])
    expect(value("¬([0, 2, -3])")).toEqual([1, 0, 0])
    expect(value("[0, 1, 1] ⍲ [1, 1, 0]")).toEqual([1, 0, 1])
    expect(value("[0, 1, 0] ⍱ [0, 1, 1]")).toEqual([1, 0, 0])
    // like comparisons, results are float 0/1 and compose with arithmetic
    expect(value("([0.5, 0] ∨ [0, 0]) + 10")).toEqual([11, 10])
  })
  test("roll wraps around the torus", () => {
    expect(value("roll([1, 2, 3, 4], 1)")).toEqual([4, 1, 2, 3])
    expect(value("roll([1, 2, 3, 4], -1)")).toEqual([2, 3, 4, 1])
    expect(value("roll([[1, 2], [3, 4]], 1, 0)")).toEqual([[3, 4], [1, 2]])
    // regression: two rolls on different axes over a lazy nested-stack base
    // (jax-js #146 – symbolic-division unsoundness, fixed in 0.1.18)
    expect(value("roll(roll([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 1, 0), 1, 1)"))
      .toEqual([[9, 7, 8], [3, 1, 2], [6, 4, 5]])
  })
  test("concat with axis", () => {
    expect(value("concat(([1, 2], [3, 4]), 0)")).toEqual([1, 2, 3, 4])
  })
  test("outer product ⊗", () => {
    expect(value("(0 :: 3) (⊗ ×) (0 :: 3)")).toEqual([[0, 0, 0], [0, 1, 2], [0, 2, 4]])
  })
  test("table with cell rank – frames cross, cells zip", () => {
    expect(value("[[1,2],[3,4]] (+ ⊗ 1) [[10,20],[30,40]]")).toEqual([
      [[11, 22], [31, 42]],
      [[13, 24], [33, 44]],
    ])
  })
  test("conv is rank-polymorphic and keeps input size (SAME)", () => {
    // the kernel's rank sets the conv's rank
    expect(value("conv([1, 2, 1], 0 :: 6)")).toEqual([1, 4, 8, 12, 16, 14])
    expect(value("conv([[0, 1, 0], [1, -4, 1], [0, 1, 0]], [[0, 0, 0], [0, 1, 0], [0, 0, 0]])"))
      .toEqual([[0, 1, 0], [1, -4, 1], [0, 1, 0]])
  })
})

describe("math functions", () => {
  test("remainder of equal values is zero, negatives take the divisor's sign", () => {
    // on WebGPU, floor(x/x) can miss by an ULP – x % x must still be 0
    expect(value("107 % 107")).toBe(0)
    expect(value("108 % 107")).toBe(1)
    expect(value("(0 - 7) % 3")).toBe(2)
  })
  test("clamp, sigmoid, relu", () => {
    expect(value("clamp(7, 2, 5)")).toBe(5)
    expect(value("sigmoid(0)")).toBe(0.5)
    expect(value("relu(0 - 3)")).toBe(0)
  })
  test("softmax sums to one", () => {
    expect(value("sum(softmax([1, 2, 3]))")).toBeCloseTo(1, 5)
  })
  test("reductions keep the reduced axis with a truthy third argument", () => {
    // mean(x, -1, 1) is the trace-safe unsqueeze(mean(x, -1), …): unsqueeze
    // reads shapes, which a jit trace can't – keepdims never leaves the graph
    expect(value("mean([[1, 2], [3, 4]], -1, 1)")).toEqual([[1.5], [3.5]])
    expect(value("shape(sum([[1, 2], [3, 4]], 0, 1))")).toEqual([1, 2])
  })
  test("oneHot", () => {
    expect(value("oneHot([2], 3) ⍴ [3]")).toEqual([0, 0, 1])
  })
  test("crossEntropy", () => {
    expect(value("crossEntropy([0, 1, 0], [1, 2, 0.5])")).toBeCloseTo(0.4644, 3)
  })
  test("roots, powers, logs", () => {
    expect(value("sqrt(9)")).toBe(3)
    expect(value("square(4)")).toBe(16)
    expect(value("cbrt(27)")).toBeCloseTo(3, 4)
    expect(value("log2(8)")).toBe(3)
    expect(value("log10(1000)")).toBe(3)
    expect(value("expm1(0)")).toBe(0)
    expect(value("log1p(0)")).toBe(0)
    expect(value("trunc(0 - 2.7)")).toBe(-2)  // toward zero, unlike floor
  })
  test("inverse trig, both arc* and short spellings", () => {
    expect(value("arcsin(1)")).toBeCloseTo(1.5708, 3)
    expect(value("asin(1)")).toBeCloseTo(1.5708, 3)   // short alias, same value
    expect(value("arccos(1)")).toBe(0)
    expect(value("arctan2(1, 1)")).toBeCloseTo(0.7854, 3)
    expect(value("hypot(3, 4)")).toBe(5)
    expect(value("deg2rad(180)")).toBeCloseTo(3.14159, 3)
  })
})

// Contracts any future compilation of reactive recomputes must preserve
// (a per-application jit was tried and reverted: materializing every
// computed boundary defeats jax-js's cross-expression lazy fusion)
describe("reactive recomputes stay dynamic", () => {
  test("repeated updates keep recomputing", () => {
    expect(value("x: $(2), y: (x × 3) + 1, x(4), x(5), y")).toBe(16)
  })
  test("payload shapes may change between updates", () => {
    expect(value("x: $([1, 2]), y: sum(x × 2), x([1, 2, 3]), y")).toBe(12)
  })
  test("value-dependent bodies (mask) recompute correctly", () => {
    expect(value("x: $([3, 1, 2]), y: mask(x, x > 1), x([5, 0, 6]), y")).toEqual([5, 6])
  })
  test("randomness stays fresh per recompute", () => {
    const scope = createScope()
    const out = evaluateGeneration(() =>
      evaluateSyntaxTreeNode(CodeParse("x: $(0), y: x + randn([4]), y"), scope)) as any
    const first = JSON.stringify(out.value.ref.js())
    ;(scope["x"] as any).value = np.array(new Float32Array([1, NaN])).slice(0)
    const second = JSON.stringify(out.value.ref.js())
    expect(first).not.toBe(second)
  })
  test("variable reads in lambda bodies see fresh assignments", () => {
    expect(value("θ: ~([5]), f: { v | v + θ_0 }, x: $(1), y: f(x), θ := [9], x(2), y")).toBe(11)
  })
  test("chains of computeds recompute end-to-end", () => {
    expect(value("x: $(2), a: x × 3, b: a + 1, x(4), x(5), b")).toBe(16)
  })
  test("diamond dependencies stay consistent", () => {
    expect(value("x: $(2), a: x × 3, b: a + a, x(4), b")).toBe(24)
  })
  test("untraceable middles keep the chain reactive", () => {
    expect(value("x: $([3, 1, 2]), m: mask(x, x > 1), s: sum(m), x([5, 0, 6]), s")).toBe(11)
  })
  test("chains fed by watch(θ) track assignments", () => {
    expect(value("θ: ~([2]), w: watch(θ), y: w + 1, θ := [7], y")).toEqual([8])
  })
})

describe("reactivity", () => {
  test("signal create, read, write", () => {
    expect(value("x: $(2), x()")).toBe(2)
    expect(value("x: $(2), x(7), x()")).toBe(7)
  })
  test("signal write with ←", () => {
    expect(value("x: $(1), x ← 9, x()")).toBe(9)
  })
  test("computed auto-lifts and unwraps", () => {
    expect(value("x: $(2), y: x + 1, y")).toBe(3)
  })
  test("once reads without subscribing", () => {
    expect(value("x: $(4), once(x) + 1")).toBe(5)
  })
  test("watch bridges a variable into the signal world", () => {
    expect(value("θ: ~([2]), w: watch(θ), θ := [8], w")).toEqual([8])
  })
})

describe("differentiation and optimization", () => {
  test("gradient", () => {
    expect(value("∇({ x | x^2 })(3)")).toBe(6)
    // the three-tier naming: long for discovery, word for habit, glyph for fluency
    expect(value("TensorGradient({ x | x^2 })(3)")).toBe(6)
    expect(value("grad({ x | x^2 })(3)")).toBe(6)
  })
  test("higher-order gradient", () => {
    expect(value("∇(∇({ x | x^3 }))(2)")).toBe(12)
  })
  test("gradient of an elementwise function maps over vectors", () => {
    // TFJS ∇ semantics: non-scalar outputs pull back a ones cotangent
    expect(value("∇({ x | x^2 })([1, 2, 3])")).toEqual([2, 4, 6])
  })
  test("sgd fits y = 3x end-to-end", () => {
    // the optimizer is created ONCE, outside the loop – recreating a stateful
    // optimizer per step resets its moments (and recompiles its jit program)
    const program = `
      x: 0 :: 8,
      y: x × 3,
      θ: ~([0]),
      𝓛: { mean((x × θ_0 - y)^2) },
      opt: sgd(0.01),
      step: { v | opt(𝓛) },
      (step ⍣ 60)(◌),
      θ_0
    `
    expect(value(program)).toBeCloseTo(3, 1)
  })
  // one shared shape for the remaining optimizers: fit a scalar to 3 with a
  // persistent optimizer (state carries across steps)
  const fits = (optimizer: string, steps = 80) => value(`
    θ: ~([0]),
    𝓛: { mean((θ_0 - 3)^2) },
    opt: ${optimizer},
    step: { v | opt(𝓛) },
    (step ⍣ ${steps})(◌),
    θ_0
  `)
  test("adam fits a scalar (the README training snippet's shape)", () => {
    expect(fits("adam(0.1)", 100)).toBeCloseTo(3, 1)
  })
  test("adagrad fits a scalar", () => {
    // regression: the accumulator update leaked one reference per step
    // (tree.map hands leaf ownership to the callback; sqrt borrowed instead)
    expect(fits("adagrad(1)")).toBeCloseTo(3, 1)
  })
  test("adamw fits a scalar", () => {
    expect(fits("adamw(0.2, 0.0001)")).toBeCloseTo(3, 1)
  })
  test("sgd with momentum fits a scalar", () => {
    expect(fits("sgd(0.02, 0.9)")).toBeCloseTo(3, 1)
  })
})

describe("data slots (~~)", () => {
  test("a data slot reads like a variable and is never trained", () => {
    const trained = (final: string) => value(`
      x: ~~([5]),
      θ: ~([0]),
      𝓛: { mean((θ_0 - x_0)^2) },
      opt: sgd(0.2),
      step: { v | opt(𝓛) },
      (step ⍣ 60)(◌),
      ${final}
    `)
    expect(trained("θ_0")).toBeCloseTo(5, 1)
    // the loss pulls θ toward x, so x has a nonzero cotangent path – the slot
    // must come back untouched anyway (grads are w.r.t. params only)
    expect(trained("x_0")).toBe(5)
    // three-tier naming: the word alias makes the same slot
    expect(value("x: data([4]), x_0")).toBe(4)
  })
  test(":= retargets a data slot mid-training and the fit follows", () => {
    // the compiled step receives the slot as a jit argument – fresh payload
    // flows in on the next step, optimizer moments intact
    const program = `
      x: ~~([3]),
      θ: ~([0]),
      𝓛: { mean((θ_0 - x_0)^2) },
      opt: adam(0.2),
      step: { v | opt(𝓛) },
      (step ⍣ 60)(◌),
      x := [7],
      (step ⍣ 120)(◌),
      θ_0
    `
    expect(value(program)).toBeCloseTo(7, 1)
  })
  test(":= to a NEW SHAPE retraces cleanly and training carries on", () => {
    const program = `
      x: ~~([1, 2]),
      θ: ~([0]),
      𝓛: { mean((θ_0 - mean(x))^2) },
      opt: adam(0.3),
      step: { v | opt(𝓛) },
      (step ⍣ 50)(◌),
      x := [10, 10, 10, 10],
      (step ⍣ 120)(◌),
      θ_0
    `
    expect(value(program)).toBeCloseTo(10, 1)
  })
  test("when – a conditional effect: the condition gates, thunk errors stay loud", () => {
    expect(value("when(1, { 5 })")).toBe(5)
    expect(value("when(0, { 5 })")).toBe(null)
    // unlike a guard inside a cascade, a broken thunk surfaces as an Error –
    // silently turning a dead training step into a no-op is the failure mode
    expect(run("when(1, { nonsense(3) })")).toBeInstanceOf(Error)
  })
  test("SignalEffect writes a signal through into a data slot, and re-fires on updates", () => {
    const program = `
      corpus: $([1, 2]),
      D: ~~(once(corpus)),
      SignalEffect({ D := corpus() }),
      corpus ← [7, 8, 9],
      sum(D)
    `
    expect(value(program)).toBe(24)
  })
  test(":= with a non-tensor keeps the variable's last good value", () => {
    // deleting the demo's corpus made load() an Error; := stored it and the
    // ⟳ drain died on Error.ref – the write must refuse instead
    expect(run("x: ~~([1, 2]), x := unbound(3)")).toBeInstanceOf(Error)
    expect(value("x: ~~([1, 2]), r: x := unbound(3), sum(x)")).toBe(3)
  })
  test("passing ~~ in an explicit var list is a loud error, never silent filtering", () => {
    const program = `
      x: ~~([1]),
      θ: ~([0]),
      𝓛: { mean((θ_0 - 1)^2) },
      opt: sgd(0.1, (θ, x)),
      opt(𝓛)
    `
    expect(run(program)).toBeInstanceOf(Error)
  })
})

describe("function power validation", () => {
  test("⍣ rejects a non-scalar count instead of silently doing nothing", () => {
    expect(run("({ x | x × 2 } ⍣ [2, 2])(1)")).toBeInstanceOf(Error)
  })
  test("⍣ rounds a fractional (slider-driven) count, like linspace", () => {
    expect(value("({ x | x × 2 } ⍣ 2.6)(1)")).toBe(8)
  })
})

describe("lists and strings", () => {
  test("ListMap and ListGet", () => {
    expect(value("ListGet(ListMap((1, 2, 3), { x | x × 10 }), 1)")).toBe(20)
  })
  test("ListReduce", () => {
    expect(value("ListReduce((1, 2, 3), { a, b | a + b })")).toBe(6)
  })
  test("strings are values", () => {
    expect(String(value('"hello"'))).toBe("hello")
    expect(value('StringLength("abc")')).toBe(3)
  })
  test("StringToCodes / CodesToString round-trip through tensor land", () => {
    expect(value('StringToCodes("abc")')).toEqual([97, 98, 99])
    expect(String(value('CodesToString(StringToCodes("hello"))'))).toBe("hello")
    // codes are ordinary tensors – a Caesar cipher is one addition
    expect(String(value('CodesToString(StringToCodes("abc") + 1)'))).toBe("bcd")
    expect(run('CodesToString("nope")')).toBeInstanceOf(Error)
    expect(run("StringToCodes([1, 2])")).toBeInstanceOf(Error)
  })
})

describe("prelude list helpers", () => {
  test("ListZip pairs elements up to the shorter length", () => {
    // regression: the pair count used TensorMin (a reduction, so the second
    // argument was an axis) – the Error degraded to NaN and every zip was ()
    expect(value("ListLength(ListZip((1, 2, 3), (4, 5)))")).toBe(2)
    expect(value("ListGet(ListGet(ListZip((1, 2), (30, 40)), 0), 1)")).toBe(30)
    expect(value("ListGet(ListGet(ListZip((1, 2), (30, 40)), 1), 1)")).toBe(40)
  })
  test("ListTake and ListDrop", () => {
    expect(value("ListLength(ListTake((7, 8, 9), 2))")).toBe(2)
    expect(value("ListGet(ListTake((7, 8, 9), 2), 1)")).toBe(8)
    expect(value("ListLength(ListDrop((7, 8, 9), 1))")).toBe(2)
    expect(value("ListGet(ListDrop((7, 8, 9), 1), 0)")).toBe(8)
  })
  test("ListReverse and ListEnumerate", () => {
    expect(value("ListGet(ListReverse((1, 2, 3)), 0)")).toBe(3)
    expect(value("ListGet(ListGet(ListEnumerate((7, 8)), 1), 0)")).toBe(1)
    expect(value("ListGet(ListGet(ListEnumerate((7, 8)), 1), 1)")).toBe(8)
  })
  test("ListGather and ListScan", () => {
    expect(value("ListGet(ListGather((10, 20, 30), (2, 0)), 0)")).toBe(30)
    expect(value("ListLength(ListScan((1, 2, 3), { a, b | a + b }, 0))")).toBe(4)
    expect(value("ListGet(ListScan((1, 2, 3), { a, b | a + b }, 0), 3)")).toBe(6)
  })
})

describe("operator arity", () => {
  // +, -, ×, · overload unary/binary by arity. Regression: a binary op with an
  // unknown operand used to silently fall back to the unary op on the first
  // operand (`1 + a` → abs(1) = 1) instead of erroring.
  test("binary op with an unknown operand is an Error, not a silent unary", () => {
    expect(run("1 + a")).toBeInstanceOf(Error)
    expect(run("a + 1")).toBeInstanceOf(Error)
    expect(run("2 * b")).toBeInstanceOf(Error)
  })
  test("binary and unary forms both still work", () => {
    expect(value("1 + 2")).toBe(3)
    expect(value("5 - 3")).toBe(2)
    expect(value("+([-3, 4])")).toEqual([3, 4])   // unary abs
    expect(value("-([1, 2])")).toEqual([-1, -2])  // unary negate
    expect(value("×(-7)")).toBe(-1)               // unary sign
  })
})

describe("backtick code literals", () => {
  // regression: two adjacent `...` literals used to conflate into one Code node
  // (the inner Program greedily ate the second literal's opening backtick)
  const stmts = (src: string) => (CodeParse(src) as any).content as any[]
  test("adjacent backtick literals stay separate", () => {
    const s = stmts("`1 + 2 * 3 - 4 / 5`,\n\n; precedence\n`1 + 2*3 - 4/5`,")
    expect(s.length).toBe(2)
    expect(s.map(n => n.type)).toEqual(["Code", "Code"])
  })
  test("each literal's content parses as its own program", () => {
    const s = stmts("`a + b`, `c*d`")
    expect(s.map(n => n.type)).toEqual(["Code", "Code"])
    expect(s[0].content.value.type).toBe("Program")
    expect(s[1].content.value.type).toBe("Program")
  })
  test("unparseable inner code yields an Error node, not a crash", () => {
    // the IDE's printer must receive this shape and render the message
    const s = stmts("`}`")
    expect(s[0].type).toBe("Code")
    expect(s[0].content.value.type).toBe("Error")
    expect(typeof s[0].content.value.content).toBe("string")
  })
  test("a throwing code printer degrades to an Error value", () => {
    // regression: a printer bug (e.g. the AST viz choking on an Error node)
    // used to abort the whole evaluation and take the output panel down
    setCodeNodePrinter(() => { throw new Error("printer exploded") })
    try {
      const out = run("`1 + 1`")
      expect(out).toBeInstanceOf(Error)
      expect((out as Error).message).toBe("printer exploded")
    } finally {
      setCodeNodePrinter((node: SyntaxTreeNode) => node as unknown as Value)
    }
  })
})

describe("resource lifetimes", () => {
  test("watch(θ) releases its held reference when the generation retires", async () => {
    const scope = createScope()
    const out = evaluateGeneration(() =>
      evaluateSyntaxTreeNode(CodeParse("θ: ~([2]), w: watch(θ), w"), scope)) as Signal<np.Array>
    out.value // materialize the watch's internal reference
    const payload = (scope["θ"] as FluentVariable).current
    expect(payload.refCount).toBeGreaterThan(1)
    evaluateGeneration(() => null) // retire the generation above
    await Bun.sleep(80)            // its teardown runs on a 50ms timer
    expect(payload.refCount).toBe(0)
  })
})

describe("apply and evaluate", () => {
  test("@ applies to a bare argument and spreads a list", () => {
    expect(value("double: { x | x × 2 }, double @ 5")).toBe(10)
    expect(value("∇({ x | x^2 }) @ 3")).toBe(6)
    expect(value("add @ (1, 2)")).toBe(3)
  })
  test(". pipes and spreads a list, mirrored", () => {
    expect(value("5 . { x | x × 2 }")).toBe(10)
    expect(value("(1, 2) . add")).toBe(3)
  })
})

describe("errors", () => {
  test("parse errors evaluate to Error values", () => {
    expect(run("} {")).toBeInstanceOf(Error)
  })
  test("unresolved symbols stay symbolic", () => {
    expect(typeof run("certainly-not-defined")).toBe("symbol")
  })
  test("calling a non-function is an Error value", () => {
    expect(run("3(4)")).toBeInstanceOf(Error)
  })
})

describe("metadata arguments are strict", () => {
  // regression: getAsSyncList returned undefined for non-tensors, so a typo'd
  // axis/index/count coerced to undefined/NaN and silently changed the op's
  // meaning – the `1 + a` bug class, but for every metadata position
  test("a typo'd axis is an Error, not a full reduction", () => {
    expect(run("sum([[1, 2], [3, 4]], axs)")).toBeInstanceOf(Error)
    expect(run("mean([[1, 2], [3, 4]], nope)")).toBeInstanceOf(Error)
  })
  test("a typo'd index is an Error, not the first element", () => {
    expect(run("ListGet((10, 20, 30), oops)")).toBeInstanceOf(Error)
  })
  test("a typo'd shape or count is an Error", () => {
    expect(run("[1, 2, 3, 4] ⍴ shp")).toBeInstanceOf(Error)
    expect(run("0 :: nope")).toBeInstanceOf(Error)
    expect(run("linspace(5, 3)")).toBeInstanceOf(Error)   // range must be [start, stop]
  })
  test("a guard on a typo'd condition is not silently truthy", () => {
    // the broken guard errors; cascade moves on to the next candidate
    expect(value("cascade((guard(typo, { 1 }), { 2 }))()")).toBe(2)
  })
  test("valid scalar and vector axes still work", () => {
    expect(value("sum([[1, 2], [3, 4]], 0)")).toEqual([4, 6])
    expect(value("sum([[1, 2], [3, 4]], [0, 1])")).toBe(10)
    expect(value("roll([1, 2, 3, 4], 1)")).toEqual([4, 1, 2, 3])
  })
})

describe("null is a value", () => {
  // regression: Symbol.resolve used `??`, so a name bound to ◌ (or to the
  // null result of :=) came back as an unbound symbol instead of null
  test("◌ round-trips through a binding", () => {
    expect(run("x: ◌, x")).toBe(null)
  })
  test("the null identifier resolves to null", () => {
    expect(run("null")).toBe(null)
  })
  test(":= evaluates to ◌ and binds as ◌", () => {
    expect(run("θ: ~([1]), r: (θ := [2]), r")).toBe(null)
  })
  test("missing lambda arguments are ◌, not unbound names", () => {
    expect(run("{ x, y | y }(1)")).toBe(null)
  })
  test("using ◌ as a tensor is an Error, not a silent value", () => {
    expect(run("x: ◌, x + 1")).toBeInstanceOf(Error)
  })
})
