// End-to-end tests: Fluent source in, evaluated result out.
// Runs the real pipeline – grammar → AST → evaluator → tensors – on the Wasm
// backend (language.ts initializes jax-js on import). `bun test` runs this file.

import { describe, test, expect } from "bun:test"
import { Signal } from "@preact/signals-core"
import { CodeParse, evaluateSyntaxTreeNode, evaluateGeneration, flushRetirements, createScope, np, FluentVariable, setCodeNodePrinter, extendEnvironment, beginTensorWatch, endTensorWatch, liveTensorCount, peakTensorCount, type SyntaxTreeNode, type Value } from "./language"
import { EXAMPLES } from "./examples"

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
    expect(value("x: 1 ..< 4, sum(x)")).toBe(6)
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

describe("combinators", () => {
  test("∘ then: composition reads left-to-right (Q, the reversed Bluebird)", () => {
    expect(value("(+ ∘ √)(9, 16)")).toBe(5)   // add, then root
    expect(value("(÷ ∘ -)(1, 4)")).toBe(-0.25) // divide, then negate
  })
  test("⍨ commutes a binary, duplicates for a unary (C, W)", () => {
    expect(value("3 (⍨ -) 10")).toBe(7)
    expect(value("(⍨ ×)(9)")).toBe(81)
  })
  test("⍥ over: preprocessor first, combiner second (Ψ)", () => {
    expect(value("-3 (abs ⍥ =) 3")).toBe(1)
  })
  test("Φ fork: middle tine combines the outer tines (Φ, Φ₁)", () => {
    expect(value("Φ(Σ, ÷, #)([1, 2, 3, 4])")).toBe(2.5)
    expect(value("5 Φ(=, ∨, >) 3")).toBe(1)
    expect(value("5 Φ(=, ∨, >) 9")).toBe(0)
    // the fork inside a table – ≥ rebuilt from its parts
    expect(value("(0 ..< 3) (⊗ Φ(=, ∨, >)) (0 ..< 3)")).toEqual([[1, 0, 0], [1, 1, 0], [1, 1, 1]])
  })
  test("⊸ before and ⟜ after: hooks; a constant operand binds", () => {
    expect(value("(÷ ⟜ √)(16)")).toBe(4)      // S: x ÷ √x
    expect(value("16 (÷ ⟜ √) 4")).toBe(8)     // D: x ÷ √y
    expect(value("(1 ⊸ +)(41)")).toBe(42)     // bind left
    expect(value("(÷ ⟜ 2)(84)")).toBe(42)     // bind right
  })
  test("⊢ ⊣ tacks", () => {
    expect(value("3 ⊢ 5")).toBe(5)
    expect(value("3 ⊣ 5")).toBe(3)
    expect(value("⊢(7)")).toBe(7)
  })
  test("word aliases: array-language and mainstream names", () => {
    expect(value("then(+, √)(9, 16)")).toBe(5)
    expect(value("compose(+, √)(9, 16)")).toBe(5)
    expect(value("3 (swap(-)) 10")).toBe(7)
    expect(value("fork(Σ, ÷, #)([1, 2, 3, 4])")).toBe(2.5)
    expect(value("before(1, +)(41)")).toBe(42)
  })
  test("composed functions iterate under ⍣", () => {
    expect(value("((1 ⊸ +) ⍣ 5)(0)")).toBe(5)
    // ⍣ frees each consumed step (see the leak-watch test); these guard that
    // the freeing is share-checked and never disposes a value still in use
    expect(value("((1 ⊸ +) ⍣ 100)(0)")).toBe(100)           // fresh tensor each step
    expect(value("(⊢ ⍣ 100)(7)")).toBe(7)                    // identity – result IS its input
    expect(value("(({ x | x + 1 }) ⍣ 50)([0, 10])")).toEqual([50, 60]) // vector chain
    expect(value("(√ ⍣ 3)(256)")).toBe(2)                    // 256 → 16 → 4 → 2
  })
  test("prelude functions built on hook-binds keep working", () => {
    // flat is (⍴ ⟜ [-1]); the List helpers map with (list ⊸ ListGet)
    expect(value("flat([[1, 2], [3, 4]])")).toEqual([1, 2, 3, 4])
    expect(value("ListGet(ListReverse((10, 20, 30)), 0)")).toBe(30)
    expect(value("ListLength(ListTake((1, 2, 3, 4), 2))")).toBe(2)
    expect(value("ListGet(ListDrop((1, 2, 3), 1), 0)")).toBe(2)
    expect(value("ListGet(ListGather((5, 6, 7), (2, 0)), 0)")).toBe(7)
  })
  test("ListTake/ListDrop clamp to the list length – no over-take", () => {
    // over-taking used to walk past the end, embedding Error values and
    // reporting the wrong length (ListTake((1,2,3), 5) claimed length 5)
    expect(value("ListLength(ListTake((1, 2, 3), 5))")).toBe(3)
    expect(value("ListGet(ListTake((1, 2, 3), 5), 2)")).toBe(3)
    expect(value("ListLength(ListTake((1, 2, 3), -1))")).toBe(0)
    expect(value("ListLength(ListDrop((1, 2, 3), 5))")).toBe(0)
    expect(value("ListGet(ListDrop((1, 2, 3), 1), 0)")).toBe(2)
  })
  test("isFunction answers the K-lift question, enabling userland bind", () => {
    expect(value("isFunction(√)")).toBe(1)
    expect(value("isFunction(5)")).toBe(0)
    // a userland Kestrel-lift: constants become constant functions
    const lift = "lift: { v | cascade((guard(isFunction(v), { v }), { { x, y | v } }))() }"
    expect(value(`${lift}, lift(7)(1, 2)`)).toBe(7)
    expect(value(`${lift}, lift(√)(16)`)).toBe(4)
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
  test("length is a cascade: tensor, list, or string – #, length, len agree", () => {
    expect(value("length([1, 2, 3])")).toBe(3)          // tensor
    expect(value("length([[1, 2], [3, 4]])")).toBe(2)   // leading axis
    expect(value("length((1, 2, 3))")).toBe(3)          // list
    expect(value('length("abc")')).toBe(3)              // string
    // the three tiers are one function
    expect(value('#("hi")')).toBe(2)
    expect(value('len("hello")')).toBe(5)
    // empties work in every shape
    expect(value("length([])")).toBe(0)
    expect(value("length(())")).toBe(0)
    expect(value('length("")')).toBe(0)
  })
  test("argsort: word, glyph ⍋, and long name all give the grade-up indices", () => {
    expect(value("argsort([3, 1, 2])")).toEqual([1, 2, 0])
    expect(value("⍋([3, 1, 2])")).toEqual([1, 2, 0])
    expect(value("TensorArgSort([3, 1, 2])")).toEqual([1, 2, 0])
    // x_argsort(x) is x sorted ascending
    expect(value("t: [30, 10, 20], t_argsort(t)")).toEqual([10, 20, 30])
  })
  test("chained subscripts index element-wise: a_i_j is (a_i)_j, not a_(ij)", () => {
    // `_` is the subscript operator; it must not swallow the next index as a
    // numeric digit separator – a_0_1 used to lex 0_1 as 01, giving a_1
    expect(value("a: [[1, 2], [3, 4]], a_0_1")).toBe(2)   // element (0, 1)
    expect(value("a: [[1, 2], [3, 4]], a_1_0")).toBe(3)   // element (1, 0)
    expect(value("a: [[10, 20], [30, 40]], a_1_1")).toBe(40)
    // 1D indexing, negative, multi-gather, and float/exponent literals unaffected
    expect(value("v: [10, 20, 30], v_[2, 0]")).toEqual([30, 10])
    expect(value("1e3")).toBe(1000)
    expect(value("3.14")).toBeCloseTo(3.14, 2)
  })
  test("a literal index out of bounds is an Error, not a silent clamp", () => {
    // jax-js `take` clamps out-of-range indices; a typo'd `a_2` on a 2-vector
    // used to return a wrong element instead of erroring
    expect(run("a: [1, 2], a_2")).toBeInstanceOf(Error)
    expect(run("a: [1, 2], a_5")).toBeInstanceOf(Error)
    expect(run("a: [10, 20, 30], a_(-4)")).toBeInstanceOf(Error)
    expect(run("a: [1, 2], a_[2, 9]")).toBeInstanceOf(Error)
    // dynamic (traced) indices stay unchecked so embedding-style lookups work
    expect(value("t: [10, 20, 30], t_(argmax([0, 5, 2], 0))")).toBe(20)
  })
  test("a computed (non-literal) index out of bounds is an Error too", () => {
    // a computed index isn't a seeded literal, so a cache-only check missed it
    // and jax-js `take` read out-of-bounds memory silently
    expect(run("t: [10, 20, 30], t_(argmax([0, 0, 5], 0) + 3)")).toBeInstanceOf(Error)
    expect(run("t: [10, 20, 30], t_(0 ÷ 0)")).toBeInstanceOf(Error) // NaN index
    // an in-bounds computed index still returns the right element
    expect(value("t: [10, 20, 30], t_(argmax([0, 5, 2], 0) + 1)")).toBe(30)
  })
  test("argmax/argmin of an empty tensor is an Error, not jax-js's sentinel index", () => {
    // np.argmax([]) hands back -2147483648, which only trips the gather bounds
    // check downstream – error at the source instead
    expect(run("argmax([])")).toBeInstanceOf(Error)
    expect(run("argmin([])")).toBeInstanceOf(Error)
    expect(run("argmax([], 0)")).toBeInstanceOf(Error)
    // non-empty, with and without an axis, is unaffected
    expect(value("argmax([3, 1, 2])")).toBe(0)
    expect(value("argmin([3, 1, 2])")).toBe(1)
    expect(value("argmax([[1, 9], [8, 2]], 0)")).toEqual([1, 0])
  })
  test("an empty fold returns a known op's identity, but a custom fn has none", () => {
    // every native monoid reduction over [] is its identity – consistent, and
    // the reason max([])/min([]) are ∓∞ (the identity), not null or an error
    expect(value("Σ([])")).toBe(0)                    // +  identity
    expect(value("[] reduce ×")).toBe(1)              // ×  identity
    expect(value("[] reduce ⌈")).toBe(-Infinity)      // max identity
    expect(value("[] reduce ⌊")).toBe(Infinity)       // min identity
    // a custom lambda has no known identity, so folding nothing has no answer
    expect(run("[] reduce { a, b | a + b }")).toBeInstanceOf(Error)
  })
  test("reduce folds a tensor left-to-right; known ops go native, ⌈/⌊ give max/min", () => {
    expect(value("1 ..< 10 reduce +")).toBe(45)               // native np.sum
    expect(value("[1, 2, 3, 4] reduce ×")).toBe(24)          // native np.prod
    expect(value("[3, 1, 2] reduce ⌈")).toBe(3)              // ⌈ reduce = max (native)
    expect(value("[3, 1, 2] reduce ⌊")).toBe(1)              // ⌊ reduce = min
    expect(value("reduce([[1, 2], [3, 4]], +, 0)")).toEqual([4, 6]) // along an axis
    expect(value("reduce([[1, 2], [3, 4]], +, 1)")).toEqual([3, 7])
    // a fold with no axis collapses the leading axis (one rank off), NOT every
    // axis – and the native op must agree with the equivalent lambda
    expect(value("[[1, 2], [3, 4]] reduce +")).toEqual([4, 6])
    expect(value("[[1, 2], [3, 4]] reduce { a, b | a + b }")).toEqual([4, 6])
    expect(value("[[1, 2], [3, 4]] reduce ×")).toEqual([3, 8])
    expect(value("[[1, 2], [3, 4]] reduce ⌈")).toEqual([3, 4])
    // a custom fn folds slice by slice, strictly left-to-right
    expect(value("[10, 3, 1] reduce -")).toBe(6)             // (10-3)-1
    expect(value("[1, 2, 3, 4] reduce { a, b | a - b }")).toBe(-8)
    // empty fold: a known op has an identity, a custom fn doesn't
    expect(value("[] reduce +")).toBe(0)
    expect(run("[] reduce { a, b | a + b }")).toBeInstanceOf(Error)
  })
  test("reduce with a custom function: left-to-right, named or inline, with an axis", () => {
    // a Horner fold proves strict left-to-right accumulation, not just arithmetic
    expect(value("[1, 2, 3] reduce { a, b | a × 10 + b }")).toBe(123) // ((1·10+2)·10+3)
    // a named custom fn folds the same as an inline one
    expect(value("f: { a, b | a - b }, [10, 3, 1] reduce f")).toBe(6)
    // a custom fn folds along an axis too – over slices, not a native kernel
    expect(value("reduce([[1, 2], [3, 4]], { a, b | a - b }, 0)")).toEqual([-2, -2])
    expect(value("reduce([[1, 2], [3, 4]], { a, b | a - b }, 1)")).toEqual([-1, -1])
    // a single element is returned untouched – the custom fn is never called
    expect(value("[5] reduce { a, b | a - b }")).toBe(5)
  })
  test("⌈/⌊ are ceiling/floor (one arg) and max/min (two); max/min reduce with an axis", () => {
    // the axis question: max/min reduce like sum, honoring an optional axis.
    // As a cascade, max(x, axis) used to silently return x – the pairwise
    // candidate ate the axis as a second operand.
    expect(value("max([3, 1, 2])")).toBe(3)
    expect(value("max([[1, 2], [3, 4]], 0)")).toEqual([3, 4])
    expect(value("min([[1, 2], [3, 4]], 1)")).toEqual([1, 3])
    // the glyph is APL's ⌈/⌊: ceiling/floor on one tensor, pairwise on two.
    // (⌈([3,1,2]) is now ceiling – a no-op on ints – not a max-reduction.)
    expect(value("⌈(2.3)")).toBe(3)
    expect(value("⌊(2.7)")).toBe(2)
    expect(value("2 ⌈ 3")).toBe(3)
    expect(value("[1, 5] ⌈ [4, 2]")).toEqual([4, 5])
    expect(value("[1, 5] ⌊ [4, 2]")).toEqual([1, 2])
  })
  test("linspace rounds a fractional point count", () => {
    // a live slider drives the count through fractional values – round, don't reject
    expect(value("#(linspace([0, 1], 5))")).toBe(5)
    expect(value("#(linspace([0, 1], 4.6))")).toBe(5)
    expect(value("linspace([0, 10], 3)")).toEqual([0, 5, 10])
  })
  test("reshape and range", () => {
    expect(value("[[1, 2], [3, 4]] ⍴ [4]")).toEqual([1, 2, 3, 4])
    expect(value("0 ..< 4")).toEqual([0, 1, 2, 3])
  })
  test("range operators: ..< exclusive, ... inclusive, glued and descending", () => {
    expect(value("1 ..< 5")).toEqual([1, 2, 3, 4])      // exclusive of the stop
    expect(value("1 ... 5")).toEqual([1, 2, 3, 4, 5])   // inclusive of the stop
    expect(value("1..<5")).toEqual([1, 2, 3, 4])        // glued
    expect(value("1...5")).toEqual([1, 2, 3, 4, 5])     // glued: the number no longer swallows the dot
    expect(value("5 ... 1")).toEqual([5, 4, 3, 2, 1])   // descending, inclusive
    expect(value("3 ... 3")).toEqual([3])               // single element
    // the number-lexing fix must not break real float literals
    expect(value("1.5 + 0.25")).toBeCloseTo(1.75, 5)
  })
  test("new jax-js builtins: fft, meshgrid, einsum, topk, pad, repeat, flip, sinc", () => {
    // fft returns [real; imag] stacked; a period-4 wave peaks at bin 2
    // (round: jax-js ≥0.1.18 leaves ~1e-16 residuals in the near-zero bins)
    expect((value("f: fft([0, 1, 0, -1, 0, 1, 0, -1]), √(f_0^2 + f_1^2)") as number[]).map(x => Math.round(x))).toEqual([0, 0, 4, 0, 0])
    // meshgrid and topk return lists of tensors – pull one out with ListGet
    expect(value("ListGet(meshgrid(0 ..< 3, 0 ..< 2), 0)")).toEqual([[0, 1, 2], [0, 1, 2]])
    expect(value("einsum(\"ij,jk->ik\", [[1, 2], [3, 4]], [[5, 6], [7, 8]])")).toEqual([[19, 22], [43, 50]])
    expect(value("ListGet(topk([3, 1, 4, 1, 5, 9, 2], 3), 0)")).toEqual([9, 5, 4])
    expect(value("pad([1, 2, 3], 2)")).toEqual([0, 0, 1, 2, 3, 0, 0])
    expect(value("repeat([1, 2], 3)")).toEqual([1, 1, 1, 2, 2, 2])
    expect(value("flip([1, 2, 3])")).toEqual([3, 2, 1])
    expect(value("sinc(0)")).toBe(1)   // sinc(0) = 1 by definition
  })
  test("stack with axis and broadcasting", () => {
    expect(value("stack(([1, 2], [3, 4]), 1)")).toEqual([[1, 3], [2, 4]])
    expect(value("stack(0 ..< 3, 9)")).toEqual([[0, 1, 2], [9, 9, 9]])
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
    expect(value("(0 ..< 3) (⊗ ×) (0 ..< 3)")).toEqual([[0, 0, 0], [0, 1, 2], [0, 2, 4]])
  })
  test("table with cell rank – frames cross, cells zip", () => {
    expect(value("[[1,2],[3,4]] (+ ⊗ 1) [[10,20],[30,40]]")).toEqual([
      [[11, 22], [31, 42]],
      [[13, 24], [33, 44]],
    ])
  })
  test("conv is rank-polymorphic and keeps input size (SAME)", () => {
    // signature is `arr conv kernel` – the data first, then the kernel, whose
    // rank sets the conv's rank
    expect(value("conv(0 ..< 6, [1, 2, 1])")).toEqual([1, 4, 8, 12, 16, 14])
    expect(value("conv([[0, 0, 0], [0, 1, 0], [0, 0, 0]], [[0, 1, 0], [1, -4, 1], [0, 1, 0]])"))
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
      x: 0 ..< 8,
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
  }, 20000) // jit-compiled training loop – slow under CI/load, don't flake on the 5s default
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
  }, 20000)
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
    // and the message names the culprit, not jax-js's Symbol-coercion throw
    expect((run("1 * x") as Error).message).toContain("unknown name: x")
  })
  test("a string or function in a numeric op errors by type, not jax-js's opaque throw", () => {
    const strErr = run('"a" + "b"') as Error
    expect(strErr).toBeInstanceOf(Error)
    expect(strErr.message).toContain("a string")
    expect(strErr.message).not.toContain("Invalid type for full") // the old cryptic leak
    expect((run("({ x | x }) + 1") as Error).message).toContain("a function")
    // text-joining still has a home
    expect(value('StringConcat("a", "b")')).toBe("ab")
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
  test("watch(θ) releases its held reference when the generation retires", () => {
    const scope = createScope()
    const out = evaluateGeneration(() =>
      evaluateSyntaxTreeNode(CodeParse("θ: ~([2]), w: watch(θ), w"), scope)) as Signal<np.Array>
    out.value // materialize the watch's internal reference
    const payload = (scope["θ"] as FluentVariable).current
    expect(payload.refCount).toBeGreaterThan(1)
    evaluateGeneration(() => null) // retire the generation above
    flushRetirements()             // its teardown runs when the renderer flushes
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
  test("names colliding with Object.prototype don't resolve to JS builtins", () => {
    // scopes used to chain up to Object.prototype, so `constructor`/`toString`
    // resolved to native functions and `constructor(5)` silently ran Object(5)
    expect(typeof run("constructor")).toBe("symbol")
    expect(typeof run("toString")).toBe("symbol")
    expect(typeof run("valueOf")).toBe("symbol")
    expect(run("constructor(5)")).toBeInstanceOf(Error)
  })
  test("bindings inside ( … ) and [ … ] are local; reads still inherit", () => {
    // the inner a:2 is scoped to the parens/brackets, so the outer a survives
    expect(value("a: 1, (a: 2, a), a")).toBe(1)
    expect(value("a: 1, [a: 2, a], a")).toBe(1)
    // within one list an earlier binding is still visible to later elements
    expect(value("[t: 3, t × t]")).toEqual([3, 9])
    expect(value("ListGet((t: 3, t × t), 1)")).toBe(9)
    // elements still read names from the enclosing scope
    expect(value("x: 5, [x, x + 1]")).toEqual([5, 6])
    expect(value("x: 5, (x + 1)")).toBe(6)
    // a binding among a call's arguments is local to the call
    expect(value("a: 1, Grid(1)(a: 2, a), a")).toBe(1)
  })
  test("calling a non-function is an Error value", () => {
    expect(run("3(4)")).toBeInstanceOf(Error)
  })
  test("cascade falls through on Error VALUES; a TraceBailout is never swallowed", () => {
    // the protocol is return-an-Error-to-skip, not throw-to-skip
    expect(value("cascade((guard(0, { 1 }), { 2 }))()")).toBe(2)   // guard false → skip
    expect(value("cascade((guard(1, { 1 }), { 2 }))()")).toBe(1)   // guard true → win
    // an unbound call is an Error value, so it legitimately falls through –
    // this is correct behavior, not a swallowed crash
    expect(value("cascade(({ self() }, { 42 }))()")).toBe(42)
    // a candidate that THROWS on an arg-mismatch (not returns an Error) still
    // falls through to the next – TensorMaximum needs two operands, so on one
    // it throws and cascade moves on to # (length)
    expect(value("cascade((TensorMaximum, #))([3, 1, 2])")).toBe(3)
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
    expect(run("0 ..< nope")).toBeInstanceOf(Error)
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

// Leak watch: count Fluent-tracked tensors still holding a device buffer
// (via the public .refCount) across generations. jax-js's own buffer table is
// private, so this Fluent-side signal is the reliable one.
describe("leak watch", () => {
  const gen = (code: string) =>
    evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(code), createScope()))

  test("re-evaluating frees the previous generation's tensors", () => {
    beginTensorWatch()
    try {
      gen("a: (0 ..< 500), Σ(a^2)")           // generation 1 – allocates a range + squares
      const held = liveTensorCount()
      expect(held).toBeGreaterThan(0)
      gen("1 + 1")                            // generation 2 supersedes generation 1
      flushRetirements()                      // the renderer's post-frame flush
      expect(liveTensorCount()).toBeLessThan(held)
    } finally { endTensorWatch() }
  })

  test("a throwing re-evaluation still frees the previous generation", () => {
    // regression for the leak-on-error hole: the previous generation must be
    // retired even when building the next one throws
    beginTensorWatch()
    try {
      gen("a: (0 ..< 500), Σ(a^2)")
      const held = liveTensorCount()
      expect(() => evaluateGeneration(() => { throw new Error("boom") })).toThrow("boom")
      flushRetirements()
      expect(liveTensorCount()).toBeLessThan(held)
    } finally { endTensorWatch() }
  })

  test("⍣ holds O(1) live tensors across iterations, not O(n)", () => {
    beginTensorWatch()
    try {
      // 300 iterations must not retain ~300 live tensors – each step used to
      // accumulate in a single arena freed only when the whole expression swept
      gen("((1 ⊸ +) ⍣ 300)(0)")
      expect(peakTensorCount()).toBeLessThan(30)
    } finally { endTensorWatch() }
  })
})

// Every shipped gallery example, run headless, must parse and evaluate without
// error – a language/prelude change that breaks a demo fails CI here. The IDE
// components examples reference live only in client.tsx, so we register light
// stubs: sources hand back a tensor (they feed real math); widgets self-return,
// so a curried Grid(3)(...) stays callable. (Full-stack behavior – WebGPU,
// hardware, error panels – is the browser suite's job, added later.)
describe("gallery examples smoke suite", () => {
  const widget = function widget(): unknown { return widget }
  const source2d = () => np.zeros([16, 16])
  const scalar = () => np.array(0)
  const stubs: Record<string, Value> = {}
  for (const k of ["Button", "Checkbox", "Grid", "ImageUpload", "Layers", "Point2D", "Trail",
    "Slider", "Scrubber", "Text", "TextEditor", "Code", "CodeEditor", "Print", "PrettyPrint",
    "CodePrint"]) stubs[k] = widget as Value
  for (const k of ["Camera", "Microphone", "MicrophoneSpectrum", "LoadTensorFromImageUrl",
    "LoadSafeTensorFromURL", "Fetch"]) stubs[k] = source2d as Value
  for (const k of ["Time", "SampleRate", "MousePosition"]) stubs[k] = scalar as Value
  extendEnvironment(stubs)

  for (const [name, src] of Object.entries(EXAMPLES)) {
    test(`${name}`, () => {
      const source = src.trim()
      expect(CodeParse(source).type).not.toBe("Error")
      expect(run(source)).not.toBeInstanceOf(Error)
    })
  }
})
