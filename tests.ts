// End-to-end tests: Fluent source in, evaluated result out.
// Runs the real pipeline – grammar → AST → evaluator → tensors – on the engine's CPU
// backend (Bun has no WebGPU). `bun test` runs this file.

import { describe, test, expect, beforeAll, afterAll } from "bun:test"
import { Signal, effect } from "@preact/signals-core"
import { CodeParse, getParseErrors, evaluateSyntaxTreeNode, evaluateGeneration, flushRetirements, createScope, engine, FluentVariable, FluentData, setCodeNodePrinter, setMeta, extendEnvironment, SignalCreate, CodeEvaluate, DefaultEnvironment, NoMatch, type Value } from "./language"
import { startHost } from "./host"
import type { ToPage, ToWorker } from "./protocol"
import { EXAMPLES } from "./examples"
import { TOUR_SOURCE } from "./tour"

// The evaluation host binds the widgets and the live sources, as in the IDE's worker;
// here the page is this inbox
const inbox: ToPage[] = []
const toHost: (message: ToWorker) => void = startHost((message) => { inbox.push(message) })
// there is no server to load from: an image is zeros, and a safetensors file is a
// signal holding an empty list – what the real loader holds until its fetch resolves
const loading = { LoadTensorFromImageUrl: () => SignalCreate(engine.zeros([16, 16, 3])), Fetch: () => SignalCreate(""), LoadSafeTensorFromURL: () => SignalCreate([]) }
const hostFetch = DefaultEnvironment.Fetch
extendEnvironment(loading as Record<string, Value>)

const evaluateIn = (source: string, scope = createScope()) => evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(source), scope))
const run = (source: string) => evaluateIn(source)
// the scope a program leaves behind
const scopeOf = (source: string) => { const scope = createScope(); evaluateIn(source, scope); return scope }
const symbolOf = (n: any): string | undefined => n?.type === "Symbol" ? (n.content?.value as string) : undefined
// signals unwrap to their current value, variables to theirs, tensors to plain JS arrays/numbers
const unwrap = (v: any): any => {
  while (v instanceof Signal) { v = v.value }
  if (v instanceof FluentVariable) { v = v.current }
  return v instanceof engine.Tensor ? v.js() : v
}
const value = (source: string): any => unwrap(run(source))

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
  test("a glued-left operator splits: left and right are maximal spaced chains", () => {
    // "1 + 2* 3 + 4" is "(1 + 2) * (3 + 4)" = 3 * 7 = 21 – the glued * outranks the
    // spaced +s and takes the whole chain on each side, unlike the all-spaced form.
    expect(value("1 + 2* 3 + 4")).toBe(21)
    expect(value("1 + 2 * 3 + 4")).toBe(13)   // all spaced: plain left-to-right
    expect(value("1 + 2 + 3* 4")).toBe(24)    // left reaches back over the chain: (1 + 2 + 3) * 4
  })
  test("glued-left operators are right-associative, spaced ones left-associative", () => {
    expect(value("10- 4- 3")).toBe(9)         // 10 - (4 - 3)
    expect(value("10 - 4 - 3")).toBe(3)       // (10 - 4) - 3
  })
  test("the glued-left operator can be any value (referential transparency)", () => {
    expect(value("add: { x, y | x + y }, 1add 2 * 3")).toBe(7)   // add(1, 2 * 3)
  })
  test("assignment evaluates to the assigned value", () => {
    expect(value("a: 1 + 2")).toBe(3)
  })
  test("a glued hyphen is subtraction, not part of an identifier", () => {
    // - is always the operator; identifiers can't contain it (no kebab-case)
    expect(value("a: 5, b: 3, a-b")).toBe(2)
    expect(value("nx: 6, nx-1")).toBe(5)
    expect(value("a: 5, b: 3, a-b^2")).toBe(4) // (a-b)^2 – tight tier is left-to-right
    expect(value("[0, -4.2]")).toEqual([0, -4.199999809265137]) // negative literals still parse
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
  test("recursion: named (self-visible binding) and anonymous (fix / 𝕐)", () => {
    // a binding is visible in its own body – named recursion needs nothing special
    expect(value(`fact: { n | cascade((guard(n = 0, { 1 }), { n × fact(n - 1) }))() }, fact(5)`)).toBe(120)
    // fix ties the knot for a nameless function; its glyph agrees
    const anon = (name: string) => `${name}({ rec | { n | cascade((guard(n = 0, { 1 }), { n × rec(n - 1) }))() } })(5)`
    expect(value(anon("fix"))).toBe(120)
    expect(value(anon("𝕐"))).toBe(120)
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
  test("⍥ into: preprocessor first, combiner second (Ψ) – and over, its APL order", () => {
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
    expect(value("compose(√, +)(9, 16)")).toBe(5)                    // mathematics' order
    expect(value("3 (swap(-)) 10")).toBe(7)
    expect(value("fork(Σ, ÷, #)([1, 2, 3, 4])")).toBe(2.5)
    expect(value("before(1, +)(41)")).toBe(42)
  })
  test("composed functions iterate under ⍣", () => {
    expect(value("((1 ⊸ +) ⍣ 5)(0)")).toBe(5)
    // ⍣ frees each consumed step, but never a value still in use
    expect(value("((1 ⊸ +) ⍣ 100)(0)")).toBe(100)           // fresh tensor each step
    expect(value("(⊢ ⍣ 100)(7)")).toBe(7)                    // identity – result IS its input
    expect(value("(({ x | x + 1 }) ⍣ 50)([0, 10])")).toEqual([50, 60]) // vector chain
    expect(value("(√ ⍣ 3)(256)")).toBe(2)                    // 256 → 16 → 4 → 2
  })
  test("prelude functions built on hook-binds keep working", () => {
    // flat is (⍴ ⟜ [-1])
    expect(value("flat([[1, 2], [3, 4]])")).toEqual([1, 2, 3, 4])
    expect(value("ListGet(ListReverse((10, 20, 30)), 0)")).toBe(30)
    expect(value("ListLength(ListTake((1, 2, 3, 4), 2))")).toBe(2)
    expect(value("ListGet(ListDrop((1, 2, 3), 1), 0)")).toBe(2)
    expect(value("ListGet(ListGather((5, 6, 7), (2, 0)), 0)")).toBe(7)
  })
  test("ListTake/ListDrop clamp to the list length – no over-take", () => {
    expect(value("ListLength(ListTake((1, 2, 3), 5))")).toBe(3)
    expect(value("ListGet(ListTake((1, 2, 3), 5), 2)")).toBe(3)
    expect(value("ListLength(ListTake((1, 2, 3), -1))")).toBe(0)
    expect(value("ListLength(ListDrop((1, 2, 3), 5))")).toBe(0)
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
  test("length is a cascade: tensor, list, or string – # and length agree", () => {
    expect(value("length([1, 2, 3])")).toBe(3)          // tensor
    expect(value("length([[1, 2], [3, 4]])")).toBe(2)   // leading axis
    expect(value("length((1, 2, 3))")).toBe(3)          // list
    expect(value('length("abc")')).toBe(3)              // string
    // the three tiers are one function
    expect(value('#("hi")')).toBe(2)

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
    // `_` is the subscript operator, never a digit separator: a_0_1 is not a_01
    expect(value("a: [[1, 2], [3, 4]], a_0_1")).toBe(2)   // element (0, 1)
    expect(value("a: [[1, 2], [3, 4]], a_1_0")).toBe(3)   // element (1, 0)
    expect(value("a: [[10, 20], [30, 40]], a_1_1")).toBe(40)
    // 1D indexing, negative, multi-gather, and float/exponent literals unaffected
    expect(value("v: [10, 20, 30], v_[2, 0]")).toEqual([30, 10])
    expect(value("1e3")).toBe(1000)
    expect(value("3.14")).toBeCloseTo(3.14, 2)
  })
  test("a literal index out of bounds is an Error, not a silent clamp", () => {
    expect(run("a: [1, 2], a_2")).toBeInstanceOf(Error)
    expect(run("a: [1, 2], a_5")).toBeInstanceOf(Error)
    expect(run("a: [10, 20, 30], a_(-4)")).toBeInstanceOf(Error)
    expect(run("a: [1, 2], a_[2, 9]")).toBeInstanceOf(Error)
    // dynamic (traced) indices stay unchecked so embedding-style lookups work
    expect(value("t: [10, 20, 30], t_(argmax([0, 5, 2], 0))")).toBe(20)
  })
  test("a computed (non-literal) index out of bounds is an Error too", () => {
    expect(run("t: [10, 20, 30], t_(argmax([0, 0, 5], 0) + 3)")).toBeInstanceOf(Error)
    expect(run("t: [10, 20, 30], t_(0 ÷ 0)")).toBeInstanceOf(Error) // NaN index
    // an in-bounds computed index still returns the right element
    expect(value("t: [10, 20, 30], t_(argmax([0, 5, 2], 0) + 1)")).toBe(30)
  })
  test("argmax/argmin of an empty tensor is an Error, not an index", () => {
    // there is no index to return – error at the source, before any gather
    expect(run("argmax([])")).toBeInstanceOf(Error)
    expect(run("argmin([])")).toBeInstanceOf(Error)
    expect(run("argmax([], 0)")).toBeInstanceOf(Error)
    // non-empty, with and without an axis, is unaffected
    expect(value("argmax([3, 1, 2])")).toBe(0)
    expect(value("argmin([3, 1, 2])")).toBe(1)
    expect(value("argmax([[1, 9], [8, 2]], 0)")).toEqual([1, 0])
  })
  test("an empty fold returns a known op's identity, but a custom fn has none", () => {
    // every native monoid reduction over [] is its identity, so max([]) is -∞
    expect(value("Σ([])")).toBe(0)                    // +  identity
    expect(value("[] reduce ×")).toBe(1)              // ×  identity
    expect(value("[] reduce ⌈")).toBe(-Infinity)      // max identity
    expect(value("[] reduce ⌊")).toBe(Infinity)       // min identity
    // a custom lambda has no known identity, so folding nothing has no answer
    expect(run("[] reduce { a, b | a + b }")).toBeInstanceOf(Error)
    expect(value("[] reduce +")).toBe(0)
  })
  test("reduce folds a tensor left-to-right; known ops go native, ⌈/⌊ give max/min", () => {
    expect(value("1 ..< 10 reduce +")).toBe(45)               // native engine.sum
    expect(value("[1, 2, 3, 4] reduce ×")).toBe(24)          // native engine.prod
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
    // max/min reduce like sum, honoring an optional axis
    expect(value("max([3, 1, 2])")).toBe(3)
    expect(value("max([[1, 2], [3, 4]], 0)")).toEqual([3, 4])
    expect(value("min([[1, 2], [3, 4]], 1)")).toEqual([1, 3])
    // the glyph is APL's ⌈/⌊: ceiling/floor on one tensor, pairwise on two
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
    expect(value("1...5")).toEqual([1, 2, 3, 4, 5])     // glued: the dots aren't a decimal point
    expect(value("5 ... 1")).toEqual([5, 4, 3, 2, 1])   // descending, inclusive
    expect(value("3 ... 3")).toEqual([3])               // single element
    // and a float literal still has its point
    expect(value("1.5 + 0.25")).toBeCloseTo(1.75, 5)
  })
  test("builtins: fft, meshgrid, einsum, topk, pad, repeat, reverse, sinc", () => {
    // fft returns [real; imag] stacked; a period-4 wave peaks at bin 2
    // (rounded: the near-zero bins carry f32 residuals)
    expect((value("f: fft([0, 1, 0, -1, 0, 1, 0, -1]), √(f_0^2 + f_1^2)") as number[]).map(x => Math.round(x))).toEqual([0, 0, 4, 0, 0])
    // meshgrid and topk return lists of tensors – pull one out with ListGet
    expect(value("ListGet(meshgrid(0 ..< 3, 0 ..< 2), 0)")).toEqual([[0, 1, 2], [0, 1, 2]])
    expect(value("einsum(\"ij,jk->ik\", [[1, 2], [3, 4]], [[5, 6], [7, 8]])")).toEqual([[19, 22], [43, 50]])
    // einsum is Fluent: every pattern it reads, and its ∇
    expect(value('einsum("ij->ji", [[1, 2], [3, 4]])')).toEqual([[1, 3], [2, 4]])
    expect(value('einsum("ii->", [[1, 2], [3, 4]])')).toBe(5)
    expect(value('einsum("ii->i", [[1, 2], [3, 4]])')).toEqual([1, 4])
    expect(value('einsum("i,j->ij", [1, 2], [3, 4])')).toEqual([[3, 4], [6, 8]])
    expect(value('einsum("ij,jk", [[1, 2], [3, 4]], [[5, 6], [7, 8]])')).toEqual([[19, 22], [43, 50]])
    expect(value('einsum("bij,bjk->bik", [[[1, 2]], [[3, 4]]], [[[1], [1]], [[2], [0]]])')).toEqual([[[3]], [[6]]])
    expect(value('einsum("...ij,...jk->...ik", [[[1, 2]]], [[1], [1]])')).toEqual([[[3]]])
    expect(value('∇({ x | Σ(einsum("ij,jk->ik", x, [[1], [2]])) })([[1, 2], [3, 4]])')).toEqual([[1, 2], [1, 2]])
    expect(value("ListGet(topk([3, 1, 4, 1, 5, 9, 2], 3), 0)")).toEqual([9, 5, 4])
    expect(value("pad([1, 2, 3], 2)")).toEqual([0, 0, 1, 2, 3, 0, 0])
    expect(value("repeat([1, 2], 3)")).toEqual([1, 1, 1, 2, 2, 2])
    expect(value("reverse([1, 2, 3])")).toEqual([3, 2, 1])
    expect(value("reverse([[1, 2], [3, 4]])")).toEqual([[4, 3], [2, 1]])        // every axis: one gather
    expect(value("reverse([[1, 2], [3, 4]], 1)")).toEqual([[2, 1], [4, 3]])
    expect(value("sinc(0)")).toBe(1)   // sinc(0) = 1 by definition
  })
  test("stack with axis and broadcasting", () => {
    expect(value("stack(([1, 2], [3, 4]), 1)")).toEqual([[1, 3], [2, 4]])
    expect(value("stack(0 ..< 3, 9)")).toEqual([[0, 1, 2], [9, 9, 9]])
  })
  test("comparisons are float 0/1 and compose with arithmetic", () => {
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
    // two rolls on different axes over a lazy nested-stack base
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
  test("rank ⍤ applies f to each rank-k cell over the frame", () => {
    expect(value("(Σ ⍤ 1)([[1,2,3],[4,5,6]])")).toEqual([6, 15])   // sum each row
    expect(value("(Σ ⍤ 2)([[1,2,3],[4,5,6]])")).toBe(21)           // whole matrix is one cell
    expect(value("Σ([[1,2,3],[4,5,6]])")).toBe(21)                 // bare Σ agrees
  })
  test("rank ⍤ lifts a whole-array function to act per row", () => {
    const src = "nrm: { x | (x - min(x)) ÷ (max(x) - min(x)) }, (nrm ⍤ 1)([[1,2,3],[4,5,6],[7,8,9]])"
    expect(value(src)).toEqual([[0, 0.5, 1], [0, 0.5, 1], [0, 0.5, 1]])
  })
  test("rank ⍤ is dyadic, a lone cell broadcasting over the frame", () => {
    expect(value("([[1,2],[3,4]]) (+ ⍤ 1) ([10, 20])")).toEqual([[11, 22], [13, 24]])
  })
  test("rank ⍤ with negative k fixes the frame instead of the cell", () => {
    expect(value("(Σ ⍤ -1)([[[1,2],[3,4]],[[5,6],[7,8]]])")).toEqual([10, 26])   // sum each 2×2
    expect(value("(Σ ⍤ 1)([[[1,2],[3,4]],[[5,6],[7,8]]])")).toEqual([[3, 7], [11, 15]])   // sum each row
  })
  test("inner product ∙ is matmul, and swaps its ring", () => {
    expect(value("[[1,2],[3,4]] (× ∙ +) [[5,6],[7,8]]")).toEqual([[19, 22], [43, 50]])   // ×∙+ = matmul
    expect(value("[1,2,3] (× ∙ +) [4,5,6]")).toBe(32)                                     // vectors → dot
    expect(value("[[0,3,999],[3,0,1],[999,1,0]] (+ ∙ ⌊) [[0,3,999],[3,0,1],[999,1,0]]"))  // min-plus, one relaxation
      .toEqual([[0, 3, 4], [3, 0, 1], [4, 1, 0]])
  })
  test("∙ runs in the order it reads: combine first, then reduce", () => {
    expect(value("[1, 2] (× ∙ +) [3, 4]")).toBe(11)   // 1×3 + 2×4
    expect(value("[1, 2] (+ ∙ ×) [3, 4]")).toBe(24)   // (1+3) × (2+4)
  })
  test("alongAxis applies any f along a named axis", () => {
    expect(value("(Σ alongAxis 0)([[1,2,3],[4,5,6]])")).toEqual([5, 7, 9])    // reducer down columns
    expect(value("(Σ TensorAlongAxis 0)([[1,2,3],[4,5,6]])")).toEqual([5, 7, 9])      // tier-3 descriptive [Type][Function]
    expect(value("(Σ alongAxis 1)([[1,2,3],[4,5,6]])")).toEqual([6, 15])      // reducer across rows
    expect(value("(Σ alongAxis -1)([[1,2,3],[4,5,6]])")).toEqual([6, 15])     // negative axis
    const nrm = "nrm: { x | (x - min(x)) ÷ (max(x) - min(x)) }, "
    expect(value(nrm + "(nrm alongAxis 0)([[1,4],[3,2]])")).toEqual([[0, 1], [1, 0]])   // shape-preserving, per column
  })
  test("conv is rank-polymorphic and keeps input size (SAME)", () => {
    // `arr conv kernel`: the data first, then the kernel, whose rank sets the conv's
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
  test("softplus, gelu, mish – and their descriptive names resolve to the same op", () => {
    expect(value("softplus(0)")).toBeCloseTo(0.6931, 3)   // log 2
    expect(value("TensorSoftplus(0)")).toBeCloseTo(0.6931, 3)
    expect(value("gelu(0)")).toBe(0)
    expect(value("gelu(1)")).toBeCloseTo(0.8412, 3)
    expect(value("TensorGelu(1)")).toBeCloseTo(0.8412, 3) // descriptive name
    expect(value("mish(0)")).toBe(0)
    expect(value("mish(1)")).toBeCloseTo(0.8651, 3)
  })
  test("statistics: variance, std (σ), norm – whole, or along an axis", () => {
    expect(value("variance([1, 2, 3])")).toBeCloseTo(0.6667, 3)
    expect(value("𝕍([1, 2, 3])")).toBeCloseTo(0.6667, 3)  // glyph tier
    expect(value("std([2, 4, 4, 4, 5, 5, 7, 9])")).toBe(2)
    expect(value("σ([2, 4, 4, 4, 5, 5, 7, 9])")).toBe(2)  // glyph tier
    expect(value("norm([3, 4])")).toBe(5)
    expect(value("variance([[1, 2], [3, 5]], 0)")).toEqual([1, 2.25])
    expect(value("σ([[1, 2], [3, 5]], 0)")).toEqual([1, 1.5])
    expect(value("norm([[3, 4], [6, 8]], 1)")).toEqual([5, 10])
  })
  test("eye is the identity matrix, folded from an outer-product equality", () => {
    expect(value("eye(2)")).toEqual([[1, 0], [0, 1]])
    expect(value("𝕀(3)")).toEqual([[1, 0, 0], [0, 1, 0], [0, 0, 1]])  // glyph tier
  })
  test("softmax sums to one", () => {
    expect(value("sum(softmax([1, 2, 3]))")).toBeCloseTo(1, 5)
  })
  test("reductions keep the reduced axis with a truthy third argument", () => {
    // mean(x, -1, 1) is unsqueeze(mean(x, -1), …) without a reshape
    expect(value("mean([[1, 2], [3, 4]], -1, 1)")).toEqual([[1.5], [3.5]])
    expect(value("shape(sum([[1, 2], [3, 4]], 0, 1))")).toEqual([1, 2])
  })
  test("oneHot", () => {
    expect(value("oneHot([2], 3) ⍴ [3]")).toEqual([0, 0, 1])
  })
  test("crossEntropy", () => {
    expect(value("crossEntropy([0, 1, 0], [1, 2, 0.5])")).toBeCloseTo(0.4644, 3)
  })
  test("crossEntropy takes class indices as well as one-hot rows: the same value and the same ∇", () => {
    const L = "[[2, 0, 0], [0, 2, 0], [0, 0, 2]]"
    expect(value(`crossEntropy([0, 1, 2], ${L})`)).toBeCloseTo(0.2395, 4)
    expect(value(`crossEntropy([0, 1, 2], ${L}) = crossEntropy(oneHot([0, 1, 2], 3), ${L})`)).toBe(1)
    expect(value(`(∇{ x | crossEntropy([0, 2, 1], x) })(${L}) ≡ (∇{ x | crossEntropy(oneHot([0, 2, 1], 3), x) })(${L})`)).toBe(1)
    // a [batch, time] label tensor against [batch, time, classes] logits
    const B = "[[[1, 2, 0.5], [0, -1, 3]], [[2, 2, 2], [0.1, 0.2, 0.3]]]"
    expect(value(`crossEntropy([[2, 0], [1, 1]], ${B}) = crossEntropy(oneHot([[2, 0], [1, 1]], 3), ${B})`)).toBe(1)
    expect(run(`crossEntropy([3], [[1, 2, 0.5]])`)).toBeInstanceOf(Error)
  })
  test("roots, powers, logs", () => {
    expect(value("sqrt(9)")).toBe(3)
    expect(value("square(4)")).toBe(16)
    expect(value("cbrt(27)")).toBeCloseTo(3, 4)
    expect(value("log2(8)")).toBe(3)
    expect(value("log(1)")).toBe(0)
    expect(value("log(8, 2)")).toBeCloseTo(3, 5)
    expect(value("8 log 2")).toBeCloseTo(3, 5)          // the base is an optional second argument
    expect(value("27 √ 3")).toBeCloseTo(3, 4)            // the value first, then the degree, like x ^ n
    expect(value("√(9)")).toBe(3)
    expect(value("log10(1000)")).toBe(3)
    expect(value("expm1(0)")).toBe(0)
    expect(value("log1p(0)")).toBe(0)
    expect(value("trunc(0 - 2.7)")).toBe(-2)  // toward zero, unlike floor
  })
  test("prelude-native additions: silu, rsqrt, π", () => {
    expect(value("silu(0)")).toBe(0)
    expect(value("silu(1)")).toBeCloseTo(0.7311, 3)   // 1 · sigmoid(1)
    expect(value("rsqrt(4)")).toBe(0.5)               // 1 / √4
    expect(value("round(π × 1000)")).toBe(3142)
    expect(value("pi")).toBe(value("π"))
  })
  test("descriptive Tensor* names alias the concise ops", () => {
    // the [Type][Function] name holds the implementation
    expect(value("TensorClamp(7, 2, 5)")).toBe(5)
    expect(value("TensorSigmoid(0)")).toBe(0.5)
    expect(value("TensorSquare(4)")).toBe(16)
  })
  test("∇ flows through the Fluent-composed sigmoid", () => {
    expect(value("Σ(∇({ x | Σ(sigmoid(x)) })([0.0]))")).toBeCloseTo(0.25, 5)
  })
  test("batch-2 prelude ops: tan/sinh/cosh, rad2deg, nand/nor (glyph + doc)", () => {
    expect(value("tan(1)")).toBeCloseTo(1.5574, 3)     // sin(1)/cos(1)
    expect(value("sinh(1)")).toBeCloseTo(1.1752, 3)
    expect(value("cosh(1)")).toBeCloseTo(1.5431, 3)
    expect(value("rad2deg(π)")).toBeCloseTo(180, 3)
    expect(value("nand([0, 1, 1], [1, 1, 0])")).toEqual([1, 0, 1])
    expect(value("TensorTangent(1)")).toBeCloseTo(1.5574, 3)   // descriptive name holds the impl
  })
  test("inverse trig, both arc* and short spellings", () => {
    expect(value("asin(1)")).toBeCloseTo(1.5708, 3)
    expect(value("acos(1)")).toBe(0)
    expect(value("atan2(1, 1)")).toBeCloseTo(0.7854, 3)
    expect(value("hypot(3, 4)")).toBe(5)
    expect(value("deg2rad(180)")).toBeCloseTo(3.14159, 3)
  })
})

// Contracts any future compilation of reactive recomputes must preserve
describe("reactive recomputes stay dynamic", () => {
  test("a signal holding a signal reads through to its value", () => {
    expect(value("a: $(1), b: $(0), b(a), b + 1")).toBe(2)
    expect(value("a: $(1), b: $(0), b(a), c: b + 1, a(5), c")).toBe(6)
  })
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
    const scope = createScope(), out = evaluateIn("x: $(0), y: x + randn([4]), y", scope) as any
    const first = JSON.stringify(out.value.js())
    ;(scope["x"] as any).value = engine.tensor(new Float32Array([1, NaN])).slice(0)
    const second = JSON.stringify(out.value.js())
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
  test("⟳ follows a live count without re-running the program that started it", () => {
    // the IDE runs a program inside an effect: a program that read the count would
    // run again – and restart its loop – each time the count moved
    const budget = SignalCreate(engine.scalar(1) as Value)
    let runs = 0
    const stop = effect(() => {
      runs++
      const scope = createScope()
      scope.budget = budget
      evaluateIn("{ 0 } ⟳ budget", scope)
    })
    budget.value = engine.scalar(5) as Value
    budget.value = engine.scalar(9) as Value
    stop()
    expect(runs).toBe(1)
  })
  test("watch bridges a variable into the signal world", () => {
    expect(value("θ: ~([2]), w: watch(θ), θ := [8], w")).toEqual([8])
  })
  test("a signal can hold a list", () => {
    // a signal stores a value, a list included – the tasks demo's list, a loader's empty one
    expect(value("s: $(List(1, 2, 3)), ListLength(s())")).toBe(3)
    expect(value("(++): ListConcat, s: $(List()), s(s() ++ List(7)), ListLength(s())")).toBe(1)
    expect(value("(++): ListConcat, s: $(List()), s ← (s() ++ List(1, 2)), ListLength(s())")).toBe(2)
  })
  test("a list-of-tensors signal survives churn (no use-after-free)", () => {
    // the signal holds each tensor of a list it stores, so none is reclaimed while it does
    expect(value("xy: $((fill([50], 7.0), fill([50], 3.0))), ({ i | xy((fill([50], 1.0), fill([50], 9.0))), i } ⍣ 20)(0), Σ(xy()_1)")).toBe(450)
  })
})

describe("language fixes", () => {
  const message = (source: string): string => {
    const v = run(source)
    return v instanceof Error ? v.message : `not an Error: ${String(v)}`
  }
  const settle = () => new Promise((resolve) => setTimeout(resolve, 150))
  test("◌ used as a tensor fails with a message that says ◌", () => {
    expect(message("x: ◌, x + 1")).toContain("◌")
    expect(message("sin(◌)")).toContain("◌")
  })
  test("an unknown name reached through a native's call is an Error, never ◌", () => {
    const [first] = run("ListMap((oops, 1), (⍨ +))") as Value[]
    expect((first as Error).message).toContain("unknown name: oops")
    expect(message("((⍨ +) ⍣ 1)(oops)")).toContain("unknown name: oops")
  })
  test("call arguments read the bindings made among them, in reading order", () => {
    expect(value("a: 1, f: { x, y | y }, f(a: 2, a)")).toBe(2)
    expect(value("a: 1, f: { x, y | x }, f(a, a: 2)")).toBe(1)
    expect(value("a: 1, [a, a: 2]")).toEqual([1, 2])
  })
  test("NaN is false for a number and for a tensor alike", () => {
    expect(run("when(0 ÷ 0, { 1 })")).toBe(null)
    expect(run("when([0 ÷ 0], { 1 })")).toBe(null)
    expect(run("when([0, 0 ÷ 0], { 1 })")).toBe(null)
    expect(value("when([0, 0 ÷ 0, 2], { 1 })")).toBe(1)
    expect(run("guard([0 ÷ 0], { 1 })()")).toBeInstanceOf(NoMatch)
  })
  test("FunctionNoAutoLift marks a new function, not the one it is given", () => {
    run("FunctionNoAutoLift(sin)")
    expect(value("sin($(0))")).toBe(0)
    expect(message("FunctionNoAutoLift(sin)($(0))")).toContain("signal")
  })
  test("SignalRead and SignalUpdate take the signal itself", () => {
    expect(value("s: $3, SignalRead(s)")).toBe(3)
    expect(value("s: $3, SignalUpdate(s, 4), s()")).toBe(4)
  })
  test("⟳ stops at a step that fails, and its signal holds the Error", async () => {
    const scope = createScope()
    const r = evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(`n: $0, { n(n() + 1), 1 + "a" } ⟳ 5`), scope))
    expect(r).toBeInstanceOf(Signal)
    await settle()
    expect((r as Signal<Value>).value).toBeInstanceOf(Error)
    expect(((scope.n as Signal<Value>).value as engine.Tensor).js()).toBe(1)
    expect(value("{ 0 } ⟳ 3, 7")).toBe(7)
  })
  test("sizes and indices are whole numbers; an empty tensor is not a scalar", () => {
    expect(value("shape(⚄([2.4]))")).toEqual([2])
    expect(value("slice([1, 2, 3, 4], [0.6], [2])")).toEqual([2, 3])
    expect(run("ListGet((10, 20, 30), [])")).toBeInstanceOf(Error)
  })
  test("list and string natives fail as values, not as JS errors", () => {
    expect(message("ListReduce(5, +)")).toContain("a list")
    expect(message("ListReduce((), +)")).toContain("empty")
    expect(value("ListReduce((1, 2), { a, b, i | identical(i, ◌) }, 0)")).toBe(1)
    expect(run("ListConcat((1,), 2)")).toBeInstanceOf(Error)
    expect(message(`StringConcat("a", ◌)`)).toContain("◌")
    expect(run(`StringConcat("a", Error("x"))`)).toBeInstanceOf(Error)
  })
  test("String prints a data slot as ~~", () => {
    expect(value("String(~~[1, 2])")).toBe("~~([1, 2])")
  })
  test("indexing an unknown name names it", () => {
    expect(message("a_0")).toContain("unknown name: a")
  })
  test("parsing keeps nothing alive once it is done", async () => {
    const { heapSize } = await import("bun:jsc")
    const program = Array.from({ length: 2000 }, (_, i) => `f${i}: { x | x × ${i} + 1 },`).join("\n")
    Bun.gc(true)
    const before = heapSize()
    for (let i = 0; i < 3; i++) { getParseErrors(program + " ".repeat(i)) }
    // off this stack first, so a collector that scans it sees no stale reference
    await new Promise((resolve) => setTimeout(resolve, 0))
    Bun.gc(true)
    expect((heapSize() - before) / 1e6).toBeLessThan(20)
  })
  test("a long line costs no more to parse than the same text over many lines", () => {
    const numbers = Array.from({ length: 5000 }, (_, i) => i)
    const time = (source: string) => { const t = performance.now(); CodeParse(source); return performance.now() - t }
    const oneLine = `[${numbers.join(", ")}]`, lines = `[${numbers.join(",\n")}]`
    time(oneLine); time(lines)
    expect(time(oneLine)).toBeLessThan(3 * time(lines))
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
    // a non-scalar output pulls back a ones cotangent
    expect(value("∇({ x | x^2 })([1, 2, 3])")).toEqual([2, 4, 6])
  })
  test("sgd fits y = 3x end-to-end", () => {
    // the optimizer is made once, outside the loop: one made per step starts its moments afresh
    expect(value(`x: 0 ..< 8, y: x × 3, θ: ~([0]), 𝓛: { mean((x × θ_0 - y)^2) },
      opt: sgd(0.01), step: { v | opt(𝓛) }, (step ⍣ 60)(◌), θ_0`)).toBeCloseTo(3, 1)
  })
  test("adam, adagrad, adamw and sgd with momentum fit a scalar, their state carried across steps", () => {
    for (const [optimizer, steps] of [["adam(0.1)", 100], ["adagrad(1)", 80], ["adamw(0.2, 0.0001)", 80], ["sgd(0.02, 0.9)", 80]] as const) {
      const θ = value(`θ: ~([0]), 𝓛: { mean((θ_0 - 3)^2) }, opt: ${optimizer}, step: { v | opt(𝓛) }, (step ⍣ ${steps})(◌), θ_0`)
      expect([optimizer, θ]).toEqual([optimizer, expect.closeTo(3, 1)])
    }
  })
})

describe("training: ∇ follows what the loss reads", () => {
  const plain = (v: any): any => Array.isArray(v) ? v.map(plain) : unwrap(v)
  test("∇(loss)() gives the value, the variables it read, and their gradients", () => {
    expect(plain(value("θ: ~([1, 2]), φ: ~(3), r: ∇({ Σ(θ × φ) })(), (ListGet(r, 0), ListLength(ListGet(r, 1)), ListGet(ListGet(r, 2), 0), ListGet(ListGet(r, 2), 1))")))
      .toEqual([9, 2, [3, 3], 3])
    // a data slot is read like a constant: no gradient for it
    expect(value("θ: ~(2), x: ~~(5), ListLength(ListGet(∇({ θ × x })(), 1))")).toBe(1)
  })
  test(":= takes a list of variables and as many values", () => {
    expect(plain(value("θ: ~([1]), x: ~~([2]), (θ, x) := ([3], [4]), (θ, x)"))).toEqual([[3], [4]])
    expect(run("θ: ~([1]), (θ,) := ([1], [2])")).toBeInstanceOf(Error)
  })
  test("two optimizers each train only what their loss reads", () => {
    // adamw decays its own variables – not the other model's
    const [a, b] = plain(value(`a: ~([0]), b: ~([0]), la: { (a_0 - 1) ^ 2 }, lb: { (b_0 + 1) ^ 2 },
      oa: adamw(0.1, 0.5), ob: sgd(0.1), ({ s | oa(la), ob(lb) } ⍣ 60)(0), (a_0, b_0)`)) as number[]
    expect(a).toBeGreaterThan(0.5)
    expect(b).toBeCloseTo(-1, 2)
  })
  test("every optimizer and schedule in the prelude fits a quadratic", () => {
    // lamb steps relative to the variable's norm, so it settles only as its rate cools
    for (const opt of ["sgd(0.1)", "sgd(0.02, 0.9)", "adam(0.1)", "adamw(0.2, 0.0001)", "adagrad(1)", "rmsprop(0.05)", "adadelta(10)", "adamax(0.1)", "nadam(0.1)",
      "radam(0.1)", "amsgrad(0.1)", "adabelief(0.1)", "yogi(0.1)", "lion(0.01)", "lamb(cosine(0.05, 300))", "muon(0.02)", "adam(warmup(0.1, 50))"]) {
      const θ = value(`θ: ~([0, 0]), 𝓛: { Σ((θ - [3, -1]) ^ 2) }, opt: ${opt}, ({ s | opt(𝓛) } ⍣ 300)(0), θ`) as number[]
      expect([opt, Math.abs(θ[0]! - 3) < 0.02 && Math.abs(θ[1]! + 1) < 0.02]).toEqual([opt, true])
    }
  }, 20_000) // 17 training loops – slow under load, don't flake on the 5s default
  test("amsgrad keeps the largest raw second moment and bias-corrects it, as PyTorch does", () => {
    // torch.optim.Adam(amsgrad=True), in float64, on Σ a(θ - c)²
    const a = [1, 10, 0.1], c = [0.3, 0.1, -0.7], θ = [1, -2, 0.5], m = [0, 0, 0], v = [0, 0, 0], top = [0, 0, 0]
    const [step, read] = run(`θ: ~[${θ}], 𝓛: { Σ([${a}] × (θ - [${c}])^2) }, opt: amsgrad(0.1), ({ opt(𝓛) }, { θ })`) as Value[] as Function[]
    for (let t = 1; t <= 8; t++) {
      step!()
      for (let i = 0; i < 3; i++) {
        const g = 2 * a[i]! * (θ[i]! - c[i]!)
        m[i] = 0.9 * m[i]! + 0.1 * g
        v[i] = 0.999 * v[i]! + 0.001 * g * g
        top[i] = Math.max(top[i]!, v[i]!)
        θ[i] = θ[i]! - 0.1 * (m[i]! / (1 - 0.9 ** t)) / (Math.sqrt(top[i]! / (1 - 0.999 ** t)) + 1e-8)
      }
      const got = unwrap(read!()) as number[]
      got.forEach((x, i) => expect(x).toBeCloseTo(θ[i]!, 4))
    }
  })
  test("opt() is the optimizer's memory: saved with the weights, := resumes training exactly", () => {
    // 5 steps, a snapshot, 5 more; then back to the snapshot and the same 5 again
    const [first, again] = value(`θ: ~[3, -2], 𝓛: { Σ((θ - [1, 2])^2) }, opt: adam(warmup(0.1, 4), (θ,)),
      state: ListConcat((θ,), opt()), steps: { ({ s | opt(𝓛) } ⍣ 5)(0) },
      steps(), saved: ListMap(state, detach), steps(), first: θ + 0,
      state := saved, steps(), stack((first, θ + 0))`) as number[][]
    expect(again).toEqual(first!)
    // given its variables, the memory is there before the first step: a step count and two moments
    expect(value("θ: ~[1, 2], opt: adam(0.1, (θ,)), ListLength(opt())")).toBe(3)
    expect(value("θ: ~[1, 2], opt: adam(0.1), ListLength(opt())")).toBe(0)
  })
  test("an optimizer is a value: optimizer(init, rule, lr)", () => {
    // plain gradient descent, written out
    expect(value("θ: ~([0]), 𝓛: { (θ_0 - 2) ^ 2 }, opt: optimizer({ v | List() }, { g, v, s, t, lr | (v - (lr × g), List()) }, 0.25), opt(𝓛), θ")).toEqual([1])
    // a schedule is a function of the step; the rule sees t from 1
    expect(value("θ: ~([0]), 𝓛: { θ_0 }, opt: optimizer({ v | List() }, { g, v, s, t, lr | (v - lr, List()) }, { t | t × 10 }), opt(𝓛), opt(𝓛), θ")).toEqual([-30])
    // warmup rises to its peak and stays; cosine falls to 0 and stays
    expect(value("θ: ~([0]), 𝓛: { θ_0 }, opt: sgd(warmup(1, 4)), ({ s | opt(𝓛) } ⍣ 6)(0), θ")).toEqual([-4.5])
    expect(value("θ: ~([0]), 𝓛: { θ_0 }, opt: sgd(cosine(1, 2)), ({ s | opt(𝓛) } ⍣ 3)(0), θ")).toEqual([expect.closeTo(-0.5, 6)])
  })
  test("clipNorm scales the gradient to a norm of at most c", () => {
    const θ = value("θ: ~([0, 0]), 𝓛: { Σ(θ × [30, 40]) }, opt: optimizer({ v | List() }, clipNorm({ g, v | (v - g, List()) }, 5), 1), opt(𝓛), θ") as number[]
    expect(θ[0]).toBeCloseTo(-3, 5)
    expect(θ[1]).toBeCloseTo(-4, 5)
  })
  test("the same program trains the same weights on every run", () => {
    // random init, and a fresh random batch every step
    const src = "W: ~randn([3, 2]), 𝓛: { x: ⚄([4, 3]), mean((matmul(x, W) - 1)^2) }, opt: adam(0.1), ({ s | opt(𝓛) } ⍣ 5)(0), W"
    expect(value(src)).toEqual(value(src))
  })
  test("a loss that reads no variable is a loud error", () => {
    expect(run("opt: adam(0.1), opt({ 3 })")).toBeInstanceOf(Error)
  })
})

describe("data slots (~~)", () => {
  test("a data slot reads like a variable and is never trained", () => {
    const trained = (final: string) => value(`x: ~~([5]), θ: ~([0]), 𝓛: { mean((θ_0 - x_0)^2) }, opt: sgd(0.2), step: { v | opt(𝓛) }, (step ⍣ 60)(◌), ${final}`)
    expect(trained("θ_0")).toBeCloseTo(5, 1)
    // the loss pulls θ toward x, so x has a nonzero cotangent path – the slot comes back untouched anyway
    expect(trained("x_0")).toBe(5)
    // three-tier naming: the word alias makes the same slot
    expect(value("x: data([4]), x_0")).toBe(4)
  })
  test(":= retargets a data slot mid-training and the fit follows", () => {
    // the next step reads the slot's new value, optimizer moments intact
    expect(value(`x: ~~([3]), θ: ~([0]), 𝓛: { mean((θ_0 - x_0)^2) }, opt: adam(0.2), step: { v | opt(𝓛) },
      (step ⍣ 60)(◌), x := [7], (step ⍣ 120)(◌), θ_0`)).toBeCloseTo(7, 1)
  }, 20000) // a training loop – slow under CI/load, don't flake on the 5s default
  test(":= to a NEW SHAPE retraces cleanly and training carries on", () => {
    expect(value(`x: ~~([1, 2]), θ: ~([0]), 𝓛: { mean((θ_0 - mean(x))^2) }, opt: adam(0.3), step: { v | opt(𝓛) },
      (step ⍣ 50)(◌), x := [10, 10, 10, 10], (step ⍣ 120)(◌), θ_0`)).toBeCloseTo(10, 1)
  }, 20000)
  test("when – a conditional effect: the condition gates, thunk errors stay loud", () => {
    expect(value("when(1, { 5 })")).toBe(5)
    expect(value("when(0, { 5 })")).toBe(null)
    // not a candidate: a broken thunk is an Error, never a training step quietly skipped
    expect(run("when(1, { nonsense(3) })")).toBeInstanceOf(Error)
  })
  test("SignalEffect writes a signal through into a data slot, and re-fires on updates", () => {
    expect(value("corpus: $([1, 2]), D: ~~(once(corpus)), SignalEffect({ D := corpus() }), corpus ← [7, 8, 9], sum(D)")).toBe(24)
  })
  test(":= with a non-tensor keeps the variable's last good value", () => {
    // deleting the demo's corpus makes load() an Error; := refuses to store it
    expect(run("x: ~~([1, 2]), x := unbound(3)")).toBeInstanceOf(Error)
    expect(value("x: ~~([1, 2]), r: x := unbound(3), sum(x)")).toBe(3)
  })
  test("passing ~~ in an explicit var list is a loud error, never silent filtering", () => {
    expect(run("x: ~~([1]), θ: ~([0]), 𝓛: { mean((θ_0 - 1)^2) }, opt: sgd(0.1, (θ, x)), opt(𝓛)")).toBeInstanceOf(Error)
  })
})

describe("Storage: a signal kept across runs", () => {
  const arrive = () => new Promise((r) => setTimeout(r, 10))
  const data = (v: Value) => JSON.parse(JSON.stringify(unwrap(v).map(unwrap)))
  test("a write is saved as the value it had then; the next run gets it back", async () => {
    const written = run(`θ: ~[1, 2], s: Storage("test: kept"), s((θ, "hi", [[3]])), θ:= [5, 6], s`)
    expect(data(written)).toEqual([[1, 2], "hi", [[3]]])
    const later = run(`Storage("test: kept")`) as Signal<Value>
    expect(later.value).toBe(null)
    await arrive()
    expect(data(later)).toEqual([[1, 2], "hi", [[3]]])
  })
  test("◌ forgets – also written to a signal that holds ◌ – and what can't be saved is an Error", async () => {
    run(`s: Storage("test: forgotten"), s([1])`)
    await arrive()
    run(`s: Storage("test: forgotten"), s(◌)`)
    const later = run(`Storage("test: forgotten")`) as Signal<Value>
    await arrive()
    expect(later.value).toBe(null)
    const fn = run(`s: Storage("test: a function"), s({ x | x }), s`) as Signal<Value>
    await arrive()
    expect(fn.value).toBeInstanceOf(Error)
  })
  test("a run that writes first keeps its value when an older one arrives", async () => {
    run(`s: Storage("test: race"), s(1)`)
    const s = run(`s: Storage("test: race"), s(2), s`) as Signal<Value>
    await arrive()
    expect(unwrap(s)).toBe(2)
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
  test("ListScan keeps every step, in one pass", () => {
    expect(value("TensorStack(ListScan((1, 2, 3), { a, b | a + b }, 0))")).toEqual([0, 1, 3, 6])
    const t = performance.now()
    expect(value("#(ListScan(TensorUnstack(0 ..< 20000), ⊢, 0))")).toBe(20001)
    expect(performance.now() - t).toBeLessThan(5_000)
  }, 30_000)
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
  test("a backslash escapes the next character in a string", () => {
    expect(String(value('"a\\"b"'))).toBe('a"b')          // a string can hold a quote
    expect(value('#("a\\"b")')).toBe(3)                   // …and it counts as one character
    expect(String(value('"\\\\"'))).toBe('\\')            // "\\" is a single backslash
    expect(value('StringToCodes("\\"")')).toEqual([34])   // \" is just the char '"' (code 34)
    expect(String(value('CodesToString(StringToCodes("say \\"hi\\""))'))).toBe('say "hi"')
  })
})

describe("prelude list helpers", () => {
  test("ListZip pairs elements up to the shorter length", () => {
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
  test("windows, chunks, stencil slide over a vector", () => {
    expect(value("windows(2, [1, 2, 3, 4])")).toEqual([[1, 2], [2, 3], [3, 4]])
    expect(value("chunks(2, [1, 2, 3, 4])")).toEqual([[1, 2], [3, 4]])
    // stencil reduces each window with f
    expect(value("stencil(3, Σ, [1, 2, 3, 4, 5])")).toEqual([6, 9, 12])
    expect(value("stencil(2, μ, [2, 4, 6, 8])")).toEqual([3, 5, 7])
    // along the leading axis at any rank: each window of rows reduces to a row
    expect(value("stencil(2, Σ, [[1, 2], [3, 4], [5, 6]])")).toEqual([[4, 6], [8, 10]])
  })
})

describe("operator arity", () => {
  // +, -, ×, · are unary or binary by arity; a binary op with an unknown operand is
  // an Error, never the unary op on the other (`1 + a` is not abs(1))
  test("binary op with an unknown operand is an Error, not a silent unary", () => {
    expect(run("1 + a")).toBeInstanceOf(Error)
    expect(run("a + 1")).toBeInstanceOf(Error)
    expect(run("2 * b")).toBeInstanceOf(Error)
    // and the message names the culprit
    expect((run("1 * x") as Error).message).toContain("unknown name: x")
  })
  test("a string or function in a numeric op errors by type, not an opaque throw", () => {
    const strErr = run('"a" + "b"') as Error
    expect(strErr).toBeInstanceOf(Error)
    expect(strErr.message).toContain("a string")
    expect(strErr.message).not.toContain("Invalid type for full")
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
  // adjacent `...` literals are separate Code nodes – the first doesn't eat the second's backtick
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
    // a printer bug (the AST viz choking on an Error node) must not abort the evaluation
    const previous = setCodeNodePrinter(() => { throw new Error("printer exploded") })
    try {
      const out = run("`1 + 1`")
      expect(out).toBeInstanceOf(Error)
      expect((out as Error).message).toBe("printer exploded")
    } finally {
      setCodeNodePrinter(previous)
    }
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
    expect(typeof run("certainlyNotDefined")).toBe("symbol")
  })
  test("names colliding with Object.prototype don't resolve to JS builtins", () => {
    // a scope doesn't reach Object.prototype: `constructor(5)` is not Object(5)
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
    // chained ':' binds every name — no explicit () means no scope, so both escape
    // (this is what lets (glyph): concise: Descriptive: impl alias in one line)
    expect(value("a: b: 23, [a, b]")).toEqual([23, 23])
    expect(value("x: y: z: 5, x + y + z")).toBe(15)
  })
  test("calling a non-function is an Error value", () => {
    expect(run("3(4)")).toBeInstanceOf(Error)
  })
  test("cascade skips a decline but stops at a failure", () => {
    // the protocol is return-a-NoMatch-to-skip
    expect(value("cascade((guard(0, { 1 }), { 2 }))()")).toBe(2)   // guard false → declines
    expect(value("cascade((guard(1, { 1 }), { 2 }))()")).toBe(1)   // guard true → answers
    // a candidate that FAILS has answered – with an Error – so a typo surfaces
    // instead of the next candidate quietly winning
    const failed = run("cascade(({ missing() }, { 42 }))()")
    expect(failed).toBeInstanceOf(Error)
    expect(failed).not.toBeInstanceOf(NoMatch)
    // a native that THROWS has failed too: TensorMaximum needs two operands
    expect(run("cascade((TensorMaximum, #))([3, 1, 2])")).toBeInstanceOf(Error)
    // try is the explicit way to let a failure mean "not this one"
    expect(value("cascade((try({ missing() }), { 42 }))()")).toBe(42)
  })
})

describe("choosing: a candidate answers or declines", () => {
  test("NoMatch declines; an Error is an answer and surfaces", () => {
    expect(value("cascade(({ x | NoMatch }, { x | x + 1 }))(1)")).toBe(2)
    const boom = run(`cond(guard(0, { 1 }), { Error("boom") })`) as Error
    expect(boom.message).toBe("boom")
    expect(boom).not.toBeInstanceOf(NoMatch)
  })
  test("a matched branch that breaks surfaces – it never falls through to a wrong answer", () => {
    // a shape error in the chosen branch is its answer; the fallback 0 would look like a result
    const broken = run("cond(guard(1, { [1, 2] + [1, 2, 3] }), { 0 })")
    expect(broken).toBeInstanceOf(Error)
    expect(broken).not.toBeInstanceOf(NoMatch)
  })
  test("an exhausted choice declines, so choices nest", () => {
    expect(value("cond({ cond(guard(0, { 1 })) }, { 7 })")).toBe(7)
    expect(value("cascade((cascade((guard(0, { 1 }),)), { 7 }))()")).toBe(7)
  })
  test("a candidate that isn't a function is its own answer", () => {
    expect(value("cond(guard(0, { 1 }), 0)")).toBe(0)
    expect(value("cond(guard(1, 5), 0)")).toBe(5)
    expect(value("cond(NoMatch, 3)")).toBe(3)
    expect(run("cond(guard(typo, { 1 }), 0)")).toBeInstanceOf(Error)
  })
  test("try turns a failure into a decline, and passes arguments through", () => {
    expect(value("cascade((try({ x | x_5 }), { x | -1 }))([1, 2])")).toBe(-1)
    expect(value("cond(try({ 5 }), 0)")).toBe(5)
    expect(value("cond(try({ cond(guard(0, { 1 })) }), 9)")).toBe(9)
    expect(run("try({ missing() })()")).toBeInstanceOf(NoMatch)
  })
  test("Error carries a message and any value as its cause", () => {
    const e = run(`Error("no real root", -4)`) as Error
    expect(e.message).toBe("no real root")
    expect((e.cause as engine.Tensor).js()).toBe(-4)
    expect((run(`Error("outer", Error("inner"))`) as Error).cause).toBeInstanceOf(Error)
  })
  test("a NoMatch used as data is a failure", () => {
    const data = run("NoMatch + 1")
    expect(data).toBeInstanceOf(Error)
    expect(data).not.toBeInstanceOf(NoMatch)
    expect(run("cond({ NoMatch + 1 }, { 0 })")).not.toBeInstanceOf(NoMatch)
  })
  test("length's candidates decline what isn't theirs", () => {
    expect(run("TensorLength((1, 2))")).toBeInstanceOf(NoMatch)
    expect(run("ListLength([1, 2])")).toBeInstanceOf(NoMatch)
    expect(run("StringLength(5)")).toBeInstanceOf(NoMatch)
    expect(run("#(◌)")).toBeInstanceOf(NoMatch)
  })
  test("under ∇ a branch reads the value it is at – the derivative is of the branch taken", () => {
    expect(value("r: { x | cascade((guard(x > 0, { x }), { 0 × x }))() }, ∇(r)(5)")).toBe(1)
    expect(value("r: { x | cascade((guard(x > 0, { x }), { 0 × x }))() }, ∇(r)(-5)")).toBe(0)
    expect(value("∇(∇({ x | cond(guard(x > 0, { x^3 }), { x }) }))(2)")).toBe(12)
    expect(value("θ: ~([5]), r: { x | cond(guard(x > 0, { x }), { 0 × x }) }, opt: sgd(0.1), opt({ Σ(r(θ)^2) }), θ")).toEqual([4])
  })
})

describe("an optimizer step that can't be honoured is a value, not a crash", () => {
  test("a loss that writes a variable", () => {
    const e = run("θ: ~([2]), 𝓛: { θ := [3], θ_0 ^ 2 }, opt: adam(0.1), opt(𝓛)") as Error
    expect(e).toBeInstanceOf(Error)
    expect(e.message).toContain("cannot change state")
    expect(value("θ: ~([2]), 𝓛: { θ := [3], θ_0 ^ 2 }, opt: adam(0.1), opt(𝓛), θ")).toEqual([2])
  })
  test("a loss that branches on a variable trains", () => {
    expect(value("θ: ~([2]), 𝓛: { (cond(guard(θ_0 > 0, { θ_0 }), { 0 }) - 6) ^ 2 }, opt: adam(0.1), opt(𝓛), opt(𝓛), θ_0"))
      .toBeGreaterThan(2)
  })
  test("training through an inverse", () => {
    // Kepler: E − e·sin E = M has no closed form; fit e through the inverse
    expect(value(`orbit: { e, M | E: inverse({ E | E - (e × sin(E)) })(M), cos(E) - e },
      M: linspace([0, 6], 12), obs: orbit(0.6, M),
      θ: ~([0.3]), 𝓛: { μ((orbit(θ_0, M) - obs) ^ 2) }, opt: adam(0.05),
      ({ s | opt(𝓛) } ⍣ 60)(0), θ_0`)).toBeCloseTo(0.6, 1)
  }, 30000)
})

describe("⍣ power: counts, inverses, and fixed points", () => {
  test("a negative count runs f backwards", () => {
    expect(value("({ x | x^3 + x } ⍣ -1)(10)")).toBeCloseTo(2, 5)
    expect(value("(sin ⍣ -2)(0.5)")).toBeCloseTo(Math.asin(Math.asin(0.5)), 5)
    expect(value("({ x | x × 2 } ⍣ -3)(40)")).toBeCloseTo(5, 5)
    expect(value("((1 ⊸ +) ⍣ 0)(7)")).toBe(7)
  })
  test("dyadic: x (f ⍣ n) y iterates f(x, ·), so −, ÷ and log are + × ^ backwards", () => {
    expect(value("3 (+ ⍣ 4) 1")).toBe(13)
    expect(value("2 (+ ⍣ -1) 8")).toBeCloseTo(6, 5)
    expect(value("2 (× ⍣ -1) 8")).toBeCloseTo(4, 5)
    expect(value("2 (^ ⍣ -1) 8")).toBeCloseTo(3, 5)
    expect(value("3 ((⍨ ^) ⍣ -1) 8")).toBeCloseTo(2, 5)
  })
  test("≡ matches within float rounding, so a fixed point reached as a 2-cycle still stops", () => {
    expect(value("1 ≡ 1.0000001")).toBe(1)
    expect(value("1 ≡ 1.001")).toBe(0)
    expect(value("[0.7390851, 2] ≡ [0.7390852, 2]")).toBe(1)
    expect(value("0 ≡ 0.000001")).toBe(0)
    expect(value("1 = 1.0000001")).toBe(0)   // = stays exact
  })
  test("a predicate runs f until predicate(new, old) holds", () => {
    expect(value("(cos ⍣ ≡)(1)")).toBeCloseTo(0.739085, 5)
    expect(value("({ x | x + 1 } ⍣ { a, b | a ≥ 100 })(0)")).toBe(100)
    expect(value("W: [[0, 3, 999, 999], [3, 0, 1, 999], [999, 1, 0, 2], [999, 999, 2, 0]], (({ M | M (+ ∙ ⌊) W }) ⍣ ≡)(W)"))
      .toEqual([[0, 3, 4, 6], [3, 0, 1, 3], [4, 1, 0, 2], [6, 3, 2, 0]])
  })
  test("a predicate that never holds is an Error, not a frozen tab", () => {
    expect((run("({ x | x + 1 } ⍣ { a, b | 0 })(0)") as Error).message).toContain("didn't hold")
  })
  test("the bottom of a valley is the inverse of its slope at zero", () => {
    expect(value("(∇({ x | (x - 3)^2 + 1 }) ⍣ -1)(0)")).toBeCloseTo(3, 5)
  })
  test("∇ flows through ⍣", () => {
    expect(value("∇({ x | ({ y | y × x } ⍣ 3)(1) })(2)")).toBeCloseTo(12, 4)
  })
  test("the loop alone can't run backwards without an inverter", () => {
    expect(run("FunctionRepeat({ x | x }, -1)")).toBeInstanceOf(Error)
  })
})

describe("inverse and ⍢ around", () => {
  test("under, over and compose are around, into and then in the traditional order", () => {
    expect(value("3 (+ under square) 4")).toBeCloseTo(value("3 (square ⍢ +) 4"), 5)
    expect(value("-3 (= over abs) 3")).toBe(1)
    expect(value("(√ compose +)(9, 16)")).toBe(5)
  })
  test("solve finds the x near the seed, or says there is none", () => {
    expect(value("solve(cos, 0, 1)")).toBeCloseTo(Math.PI / 2, 5)
    const none = run("solve(cos, 0, 0)") as Error
    expect(none).toBeInstanceOf(Error)
    expect(none).not.toBeInstanceOf(NoMatch)
    expect(none.message).toContain("no solution near the seed")
    expect(value("inverse({ x | x^3 + x })(10)")).toBeCloseTo(2, 5)
    expect(value("2 inverse(^) 8")).toBeCloseTo(3, 5)
    expect(value("inverse(log)([1, 2, 3])")).toEqual([Math.E, Math.E ** 2, Math.E ** 3].map((x) => expect.closeTo(x, 4)))
  })
  test("a failed seed falls through to another with try", () => {
    expect(value("cond(try({ solve(cos, 0, 0) }), { solve(cos, 0, 1) })")).toBeCloseTo(Math.PI / 2, 5)
  })
  test("inverse starts each element from whichever of y, 0 and 1 lands closest", () => {
    expect(value("(exp ⍣ -1)(100)")).toBeCloseTo(Math.log(100), 4)            // exp(100) overflows: starts at 1
    expect(value("(cos ⍣ -1)(0)")).toBeCloseTo(Math.PI / 2, 5)                // cos is flat at 0: starts at 1
    expect(value("(log ⍣ -1)(-5)")).toBeCloseTo(Math.exp(-5), 6)              // log(-5) is NaN: starts at 1
    expect(value("(sin ⍣ -1)([0.5, -0.9])")).toEqual([expect.closeTo(Math.asin(0.5), 5), expect.closeTo(Math.asin(-0.9), 5)])
    expect(run("(square ⍣ -1)(-4)")).toBeInstanceOf(Error)                     // no real root, from anywhere
  })
  test("where Newton can't take a step from a start, the element tries the next", () => {
    // x^x has no slope at 0, the closest start for 3: Newton can't move from there
    expect(value("({ x | x^x } ⍣ -1)(3)")).toBeCloseTo(1.82545, 4)
    // per element: 1 lands from its own closest start and keeps that answer
    expect(value("({ x | x^x } ⍣ -1)([3, 1])")).toEqual([expect.closeTo(1.82545, 4), 1])
  })
  test("an element that is stuck never stops another from landing", () => {
    // 3 is stuck at its first start while 1.5 travels: Newton runs per element
    expect(value("x: ({ x | x^x } ⍣ -1)([3, 1.5]), x^x")).toEqual([expect.closeTo(3, 4), expect.closeTo(1.5, 4)])
  })
  test("erf's slope at 0 is 2/√π, not the 0 sign and abs give there", () => {
    expect(value("∇(erf)(0)")).toBeCloseTo(2 / Math.sqrt(Math.PI), 4)
    expect(value("erf([-1, 0, 1])")).toEqual([expect.closeTo(-0.8427, 4), 0, expect.closeTo(0.8427, 4)])
  })
  test("g needn't work element by element: each start runs alone", () => {
    // g reads its argument as one number beside a [2] vector – a stack of starts would collide
    expect(value("v: [1, 2], g: { s | Σ(v × s) }, (g ⍣ -1)(9)")).toBeCloseTo(3, 4)
  })
  test("any CDF run backwards is a sampler", () => {
    // the standard normal's quantiles, from its CDF alone
    expect(value("Φ: { x | 0.5 × (1 + erf(x ÷ √(2))) }, (Φ ⍣ -1)([0.01, 0.5, 0.99])"))
      .toEqual([expect.closeTo(-2.32635, 3), expect.closeTo(0, 5), expect.closeTo(2.32635, 3)])
  })
  test("school math is one idea: do it in the right space, then come back", () => {
    expect(value("3 (square ⍢ +) 4")).toBeCloseTo(5, 5)                         // Pythagoras
    expect(value("6 (reciprocal ⍢ +) 3")).toBeCloseTo(2, 5)                     // resistors in parallel
    expect(value("2 (log ⍢ +) 8")).toBeCloseTo(16, 4)                           // the slide rule
    expect(value("(log ⍢ μ)([1, 2, 4, 8])")).toBeCloseTo(2 * Math.SQRT2, 5)     // geometric mean
    expect(value("(reciprocal ⍢ μ)([30, 60])")).toBeCloseTo(40, 4)              // there at 30, back at 60
    expect(value("(square ⍢ μ)([3, 4])")).toBeCloseTo(Math.sqrt(12.5), 5)      // root mean square
    expect(value("(exp ⍢ Σ)([1, 2, 3])")).toBeCloseTo(Math.log(Math.exp(1) + Math.exp(2) + Math.exp(3)), 4)
    expect(value("0.5 (atanh ⍢ +) 0.5")).toBeCloseTo(0.8, 5)                   // relativistic velocities
    expect(value("(log1p ⍢ μ)([0.5, -0.5])")).toBeCloseTo(Math.sqrt(0.75) - 1, 5)
    expect(value("((1 ⊸ -) ⍢ Π)(1/6 ⧉ 4)")).toBeCloseTo(1 - (5 / 6) ** 4, 5)   // a six in four rolls
  })
  test("⍢ keeps each element's branch when f keeps the shape", () => {
    expect(value("(square ⍢ (× ⟜ 4))([-1, 2])")).toEqual([expect.closeTo(-2, 5), expect.closeTo(4, 5)])
  })
  test("⍢ laws: composition, and Under is Over then the inverse", () => {
    expect(value("2 (log ⍢ (square ⍢ +)) 3")).toBeCloseTo(value("2 ((log ∘ square) ⍢ +) 3"), 4)
    expect(value("3 ((square ⍥ +) ∘ (square ⍣ -1)) 4")).toBeCloseTo(value("3 (square ⍢ +) 4"), 5)
  })
  test("∇ flows through ⍢", () => {
    expect(value("∇({ x | x (square ⍢ +) 4 })(3)")).toBeCloseTo(0.6, 4)
    const g = value("∇(log ⍢ μ)([1, 2, 4, 8])") as number[]
    g.forEach((v, i) => expect(v).toBeCloseTo(2 * Math.SQRT2 / (4 * [1, 2, 4, 8][i]!), 4))
  })
  test("the new doc cards read as written", () => {
    // a quote that slips out of the prelude template literal breaks the card's
    // string silently – the example then evaluates to garbage, not an error
    const card = (name: string) => (createScope()[name] as any).__meta__
    expect(card("Error").example).toBe(`x: -4, isError(cond(guard(x ≥ 0, { √(x) }), { Error("no real root", x) })) = 1`)
    expect(card("⍢").signature).toBe("x (g ⍢ f) y")
    expect(card("⍣").example).toContain("2 (^ ⍣ -1) 8 = 3")
    expect(card("try").signature).toBe("try({ value })")
    expect(card("solve").signature).toBe("solve(g, y, seed)")
  })
  test("⍢ edits the part a selection picks, and keeps the rest", () => {
    expect(value("({ x | x_[1, 2, 3] } ⍢ sort)([9, 4, 1, 3, 0])")).toEqual([9, 1, 3, 4, 0])
    expect(value("diagonal: { M | Σ(M × 𝕀(#(M)), 1) }, (diagonal ⍢ (× ⟜ 0))([[1, 2, 3], [4, 5, 6], [7, 8, 9]])"))
      .toEqual([[0, 2, 3], [4, 0, 6], [7, 8, 0]])
    expect(value("(reverse ⍢ (+ ⟜ [100, 0, 0]))([1, 2, 3])")).toEqual([1, 2, 103])
    expect(value("({ M | M_0 } ⍢ normalize)([[3, 4], [1, 1]])")).toEqual([[expect.closeTo(0.6, 5), expect.closeTo(0.8, 5)], [1, 1]])
    // dyadic: the right argument is what gets edited
    expect(value("[10, 20, 30] ({ x | x_[0, 2] } ⍢ +) [1, 2, 3]")).toEqual([11, 2, 33])
  })
  test("an edit nothing can satisfy is an Error, not an average", () => {
    expect(run("({ x | x_[0, 0] } ⍢ { v | [5, 7] })([1, 2])")).toBeInstanceOf(Error)
  })
  test("∇ flows through an edit", () => {
    expect(value("∇({ M | Σ((({ x | x_[1, 2] } ⍢ (× ⟜ 10))(M)) × [1, 2, 3, 4]) })([1, 1, 1, 1])")).toEqual([1, 20, 30, 4])
  })
  test("≡ answers once for the whole value", () => {
    expect(value("[1, 2] ≡ [1, 2]")).toBe(1)
    expect(value("[1, 2] ≡ [1, 3]")).toBe(0)
    expect(value("[1, 1] ≡ 1")).toBe(0)
    expect(value(`(1, "a") ≡ (1, "a")`)).toBe(1)
    expect(value(`"ab" ≡ "ab"`)).toBe(1)
  })
})

describe("metadata arguments are strict", () => {
  // a typo'd axis, index or count is an Error – never undefined or NaN, which would
  // silently change what the op means
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
  test("a guard on a typo'd condition is a failure, not a quiet no", () => {
    // the broken guard is an Error among the candidates – it surfaces
    const v = run("cascade((guard(typo, { 1 }), { 2 }))()") as Error
    expect(v).toBeInstanceOf(Error)
    expect(v.message).toContain("typo")
  })
  test("valid scalar and vector axes still work", () => {
    expect(value("sum([[1, 2], [3, 4]], 0)")).toEqual([4, 6])
    expect(value("sum([[1, 2], [3, 4]], [0, 1])")).toBe(10)
    expect(value("roll([1, 2, 3, 4], 1)")).toEqual([4, 1, 2, 3])
  })
})

describe("null is a value", () => {
  test("◌ is an argument not given, to a native as to a lambda", () => {
    // a lambda's missing parameter arrives as ◌, so passing it on must mean "not given"
    expect(value("{ x, k | Σ(x, k) }([1, 2, 3])")).toBe(6)
    expect(value("{ x, k | Σ(x, k) }([[1, 2], [3, 4]], 0)")).toEqual([4, 6])
    expect(value("log(8, ◌)")).toBeCloseTo(Math.log(8), 5)
  })
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

// What crosses time – a variable's value, a signal's, the parameters an optimizer
// steps – is settled into a leaf. Otherwise every later graph, and every ∇ walk over
// it, drags the whole history along.
describe("values that cross time carry no history", () => {
  const size = (v: unknown) => engine.graphSize((v instanceof FluentVariable ? v.current : v) as engine.Tensor)
  test("a variable assigned in a loop stays a leaf", () => {
    expect(size(scopeOf("θ: ~([1, 2]), ({ s | θ := θ × 2 + 1 } ⍣ 50)(0)")["θ"])).toBe(1)
  })
  test("a signal written in a loop stays a leaf", () => {
    expect(size((scopeOf("s: $([0, 1]), ({ i | s(s() + 1) } ⍣ 50)(0)")["s"] as Signal<unknown>).peek())).toBe(1)
  })
  test("an optimizer's parameters stay leaves however long it trains", () => {
    expect(size(scopeOf("θ: ~([0, 0]), opt: adam(0.1), ({ s | opt({ Σ((θ - [1, 2])^2) }) } ⍣ 30)(0)")["θ"])).toBe(1)
  })
  test("a computed value is its own: a later assignment doesn't reach it", () => {
    expect(value("θ: ~(5), s: Σ(θ), θ := 7, s")).toBe(5)
    expect(value("θ: ~([[1, 2]]), m: moveaxis(θ, 1, 1), θ := [[0, 0]], m")).toEqual([[1, 2]])
  })
  test("∇ leaves what its body read untouched", () => {
    expect(value("θ: ~([1, 2]), c: [3, 4], ∇({ z | Σ(z × c × θ) })([1, 1]), θ")).toEqual([1, 2])
    expect(value("θ: ~([1, 2]), c: [3, 4], ∇({ z | Σ(z × c × θ) })([1, 1]), c")).toEqual([3, 4])
  })
})

describe("adversarial edge cases", () => {
  test("numeric edges: floor-mod sign, div-by-zero", () => {
    expect(value("-7 % 3")).toBe(2)                 // floor-mod, not -1
    expect(value("1 / 0")).toBe(Infinity)           // IEEE, not an Error
  })
  test("logic ∨ is 0/1, unlike ⌈", () => {
    expect(value("3 ∨ 0")).toBe(1)                  // logical or, NOT 3
  })
  test("indexing: OOB is loud, negatives wrap from the end", () => {
    expect(run("[1, 2, 3]_5")).toBeInstanceOf(Error)
    expect(value("[10, 20, 30]_[-1, -2]")).toEqual([30, 20])
  })
  // a 1-D transpose is not a column vector; a reshape that changes the count doesn't pad
  test("shape ops: 1-D transpose no-op, bad reshape errors", () => {
    expect(value("transpose([1, 2, 3])")).toEqual([1, 2, 3])
    expect(run("[1, 2, 3] ⍴ [2, 2]")).toBeInstanceOf(Error)
  })
  test("∇ nests to third order", () => {
    expect(value("∇(∇(∇({ x | x^3 })))([5])")).toEqual([6])
  })
  // a scalar (shape []) is one item, never NaN; an empty tensor (shape [0]) is none
  test("length of a scalar is 1, of an empty tensor is 0", () => {
    expect(value("#(5)")).toBe(1)
    expect(value("#([])")).toBe(0)
  })
  // a scalar IS its one rank-0 item, so index/sort/softmax give the rank-0 identity
  test("a scalar is a uniform rank-0 tensor: index/sort/softmax return the identity", () => {
    expect(value("5_0")).toBe(5)         // the single item (consistent with #(5) = 1)
    expect(value("5_(0 - 1)")).toBe(5)   // -1 wraps to that same item
    expect(value("sort(5)")).toBe(5)     // sorting one element is that element
    expect(value("⍋(5)")).toBe(0)        // its grade-up index is 0
    expect(value("softmax(5)")).toBe(1)  // softmax over a single logit is 1
    expect(value("logSoftmax(5)")).toBe(0)
  })
})

describe("polymorphic index _ (string / list / tensor)", () => {
  // `_` keeps the container's type: `_[i, j]` returns the same kind, `_i` one element
  test("strings index to strings", () => {
    expect(value('"abc"_0')).toBe("a")
    expect(value('"abc"_[0, 1]')).toBe("ab")        // multi-index → a string
    expect(value('"abcde"_(-1)')).toBe("e")         // negative from the end
    expect(value('"abc"_[2, 1, 0]')).toBe("cba")    // reindex (here, reverse)
    expect(run('"abc"_9')).toBeInstanceOf(Error)    // clean, typed bounds error
  })
  test("lists index to lists", () => {
    expect(value("(10, 20, 30)_0")).toBe(10)                  // scalar index → element
    expect(value("(10, 20, 30)_(-1)")).toBe(30)
    expect(value("ListGet((10, 20, 30)_[0, 2], 1)")).toBe(30) // multi → sublist (10, 30)
    expect(run("(1, 2, 3)_9")).toBeInstanceOf(Error)
  })
  test("tensors unchanged; the three names agree; grad still flows", () => {
    expect(value("[10, 20, 30]_[0, 2]")).toEqual([10, 30])
    expect(value('gather("abc", [0, 1])')).toBe("ab")            // gather = _
    expect(value("∇({ x | Σ(x_[0, 0]) })([3, 7])")).toEqual([2, 0])
  })
})

describe("cond (eager variadic cascade)", () => {
  test("runs candidates in order, first non-Error wins, branches stay lazy", () => {
    expect(value("fact: { n | cond(guard(n = 0, { 1 }), { n × fact(n - 1) }) }, fact(5)")).toBe(120)
    expect(value("cond(guard(0, { 1 }), { 42 })")).toBe(42)     // guard fails → default
    expect(value("cond(guard(1, { 99 }), { nope })")).toBe(99)  // matched → the unbound else never runs
    expect(run("cond(guard(0, { 1 }))")).toBeInstanceOf(NoMatch)  // nothing answered: cond declines
  })
})

// Every shipped gallery example, run headless, parses, evaluates and then does its
// work: each ⟳ step runs twice and each button is pressed once, in order, and nothing
// it binds may hold an Error afterwards – a language or prelude change that breaks a
// demo's loss or update fails here. Every ~ must move: a step's failed statement is
// discarded by its sequence, so a loss that breaks shows only as weights that stay
// put. The widgets and the camera, microphone and pointer are the host's own – a
// source is a signal of zeros of the shape it asked for, and nothing streams here –
// and mnist.safetensors is read from disk. (WebGPU, hardware and panels are the
// browser suite's.)
describe("gallery examples smoke suite", () => {
  const steps: Function[] = [], presses: Function[] = []
  const stepLater = (step: Function) => { steps.push(step); return null }
  const button = DefaultEnvironment.Button as Function
  const pressLater = (label: Value, onClick: Value) => { if (typeof onClick === "function") { presses.push(onClick) } return button(label, onClick) }
  setMeta(stepLater, { noAutoLift: true })
  setMeta(pressLater, { noAutoLift: true })
  const real = { FunctionIterate: DefaultEnvironment.FunctionIterate, Button: button }
  beforeAll(async () => {
    // safetensors: an 8-byte header length, a JSON header, then the bytes; mnist is U8
    const bytes = new Uint8Array(await Bun.file(new URL("mnist.safetensors", import.meta.url)).arrayBuffer())
    const length = Number(new DataView(bytes.buffer).getBigUint64(0, true))
    const header = JSON.parse(new TextDecoder().decode(bytes.subarray(8, 8 + length)))
    const mnist = Object.entries(header).map(([name, t]: [string, any]) =>
      [name, engine.tensor(Float32Array.from(bytes.subarray(8 + length + t.data_offsets[0], 8 + length + t.data_offsets[1])), { shape: t.shape })])
    // makemore's names, as the dev server serves them
    const names = await Bun.file(new URL("names.txt", import.meta.url)).text()
    extendEnvironment({ FunctionIterate: stepLater, Button: pressLater, LoadSafeTensorFromURL: () => SignalCreate(mnist), Fetch: (url: Value) => SignalCreate(String(url) === "names.txt" ? names : "") } as Record<string, Value>)
  })
  afterAll(() => { extendEnvironment({ ...real, LoadSafeTensorFromURL: loading.LoadSafeTensorFromURL, Fetch: loading.Fetch } as Record<string, Value>) })

  // Force every value the example binds, following signals, variables and lists, and
  // read each tensor off the device: the top-level value of a demo is almost always a
  // Grid, which hides an op erroring inside it. Returns the first failure found.
  const firstError = (scope: Record<string, Value>): string | null => {
    const seen = new Set<unknown>()
    const walk = (v: any, path: string, depth = 0): string | null => {
      if (v == null || depth > 8 || seen.has(v)) { return null }
      if (typeof v === "object") { seen.add(v) }
      try {
        if (v instanceof Error) { return `${path}: ${v.message}` }
        if (v instanceof Signal) { return walk(v.value, `${path}()`, depth + 1) }
        if (v instanceof FluentVariable) { return walk(v.current, path, depth + 1) }
        if (v instanceof engine.Tensor) { v.js() }
        if (Array.isArray(v)) { for (const [i, x] of v.entries()) { const bad = walk(x, `${path}[${i}]`, depth + 1); if (bad) { return bad } } }
      } catch (e: any) { return `${path}: threw ${e?.message ?? e}` }
      return null
    }
    for (const key of Object.keys(scope)) { const bad = walk(scope[key], key); if (bad) { return bad } }
    return null
  }

  for (const [name, src] of Object.entries(EXAMPLES)) {
    test(name, () => {
      steps.length = presses.length = 0
      const tree = CodeParse(src.trim())
      expect(tree.type).not.toBe("Error")
      const scope = createScope()
      expect(evaluateGeneration(() => evaluateSyntaxTreeNode(tree, scope))).not.toBeInstanceOf(Error)
      expect(firstError(scope)).toBeNull()
      const trained = Object.keys(scope).filter((k) => scope[k] instanceof FluentVariable && !(scope[k] instanceof FluentData))
      const before = trained.map((k) => JSON.stringify((scope[k] as FluentVariable).current.js()))
      const failures: string[] = []
      const call = (what: string, f: Function, ...args: Value[]) => {
        try { const r = f(...args); if (r instanceof Error) { failures.push(`${what}: ${r.message}`) } } catch (e: any) { failures.push(`${what} threw: ${e?.message ?? e}`) }
      }
      const runSteps = () => { for (const step of steps.splice(0)) { call("⟳ step", step, engine.scalar(0)); call("⟳ step", step, engine.scalar(1)) } }
      runSteps()
      for (const press of presses.splice(0)) { call("button", press); runSteps() }
      expect(failures).toEqual([])
      expect(firstError(scope)).toBeNull()
      expect(trained.filter((k, i) => JSON.stringify((scope[k] as FluentVariable).current.js()) === before[i])).toEqual([])
      flushRetirements()
    }, 60_000)
  }

  test("deep-delta-learning learns a function: one number in, one out, right where it never trained", () => {
    steps.length = 0
    const scope = scopeOf(EXAMPLES["deep-delta-learning"])
    for (let i = 0; i < 300; i++) { steps[0]!(engine.scalar(i)) }
    const read = (expression: string) => unwrap(evaluateSyntaxTreeNode(CodeParse(expression), scope))
    // the mean squared error on 200 points between the 24 it trained on; 0.5 is guessing 0
    expect(read("unseen()")).toBeLessThan(0.01)
    expect(read("shape(f([1.5]))")).toEqual([1])
    flushRetirements()
  }, 60_000)
  test("the dreamers train on random minibatches – a corpus of names.txt's size fits – and score a held-out tenth", () => {
    for (const name of ["dreamer", "looped-dreamer"]) {
      steps.length = 0
      const scope = scopeOf(EXAMPLES[name as keyof typeof EXAMPLES])
      const corpus = scope.corpus as Signal<string>
      // ~200k windows: a step over all of them would need gigabytes
      corpus.value = Array(300).fill(corpus.peek()).join(" ")
      for (let i = 0; i < 20; i++) { expect([name, unwrap(steps[0]!(engine.scalar(i)))]).toEqual([name, null]) }
      const [step, train, heldOut] = unwrap(evaluateSyntaxTreeNode(CodeParse("history()_0"), scope)) as number[]
      expect([name, step, train! > 0, heldOut! > 0]).toEqual([name, 20, true, true])
      flushRetirements()
    }
  }, 60_000)
  test("makemore learns, and a new run resumes from the checkpoint the last one saved", async () => {
    // the demo, smaller: 3000 names, a narrower model, an evaluation every 25 steps
    const names = (await Bun.file(new URL("names.txt", import.meta.url)).text()).split("\n").slice(0, 3000).join("\n")
    const fetch = DefaultEnvironment.Fetch!
    extendEnvironment({ Fetch: () => SignalCreate(names) } as Record<string, Value>)
    const small = EXAMPLES.makemore.replace('run: "names",', 'run: "test: names",').replace("B: 64,", "B: 16,")
      .replace("C: 64, F: 256,", "C: 32, F: 64,").replace("every: 100,", "every: 25,").replace("⍣ 64", "⍣ 24")
    const read = (scope: Record<string, Value>, expression: string) => unwrap(evaluateSyntaxTreeNode(CodeParse(expression), scope))
    try {
      steps.length = 0
      const first = scopeOf(small)
      for (let i = 0; i < 75; i++) { expect(unwrap(steps[0]!(engine.scalar(i)))).toBe(null) }
      const history = read(first, "history") as number[][]
      expect(history.map((row) => row[0])).toEqual([25, 50, 75])
      // the held-out loss falls, well under the 3.3 of guessing among 27 characters
      expect(history[2]![2]!).toBeLessThan(history[0]![2]!)
      expect(history[2]![2]!).toBeLessThan(2.8)
      expect(String(read(first, "samples()"))).toMatch(/^[a-z ]{24}$/)
      const weights = read(first, "Wq")
      await new Promise((r) => setTimeout(r, 20))
      // a reload: the same program from the top, at step 0, paused – until the checkpoint arrives
      steps.length = 0
      const second = scopeOf(small)
      ;(second.training as Signal<Value>).value = engine.scalar(0)
      expect(read(second, "step")).toBe(0)
      await new Promise((r) => setTimeout(r, 20))
      steps[0]!(engine.scalar(0))
      expect(read(second, "step")).toBe(75)
      expect(read(second, "Wq")).toEqual(weights)
      expect(read(second, "ListGet(opt(), 0)")).toBe(75)
      expect((read(second, "history") as number[][]).length).toBe(3)
      // and training goes on from there
      ;(second.training as Signal<Value>).value = engine.scalar(1)
      steps[0]!(engine.scalar(1))
      expect(read(second, "step")).toBe(76)
    } finally {
      extendEnvironment({ Fetch: fetch } as Record<string, Value>)
      flushRetirements()
    }
  }, 60_000)
  test("mnist logs no loss before its data has loaded", async () => {
    const loaded = DefaultEnvironment.LoadSafeTensorFromURL!
    extendEnvironment({ FunctionIterate: real.FunctionIterate, LoadSafeTensorFromURL: loading.LoadSafeTensorFromURL } as Record<string, Value>)
    try {
      // the real ⟳ runs its frames; the data never arrives
      const scope = scopeOf(EXAMPLES.mnist)
      await new Promise((r) => setTimeout(r, 50))
      expect(unwrap(scope.losses)).toEqual([])
    } finally {
      extendEnvironment({ FunctionIterate: stepLater, LoadSafeTensorFromURL: loaded } as Record<string, Value>)
      evaluateIn("0")
    }
  })
  test("pitch-detector names a tone's note, and silence as none", () => {
    const microphone = DefaultEnvironment.Microphone
    const hear = (wave: (i: number) => number) => {
      extendEnvironment({ Microphone: (n: Value) => SignalCreate(engine.tensor(Array.from({ length: Number(n) }, (_, i) => wave(i)))) } as Record<string, Value>)
      return ((scopeOf(EXAMPLES["pitch-detector"]).detect as Signal<Value[]>).value).map((v) => v instanceof engine.Tensor ? v.js() : String(v))
    }
    try {
      expect(hear(() => 0)).toEqual(["–", "–", "–"])
      expect(hear((i) => Math.sin(2 * Math.PI * 440 * i / 48000))).toEqual(["A", 4, 439])
    } finally { extendEnvironment({ Microphone: microphone! }) }
  })
})

describe("String is the inverse of CodeEvaluate", () => {
  // The law: CodeEvaluate(String(x)) is x. String emits the SOURCE that reproduces
  // a value, not display text – a tensor's elements joined by commas would re-parse
  // as a sequence. Stated as String ∘ CodeEvaluate ∘ String = String, which holds for
  // every type, lists and empty tensors included, where `=` is not elementwise.
  for (const src of [
    "42", "0.5", "0 - 7",
    "[1, 2, 3]", "[[1, 2], [3, 4]]", "[[[1, 2]]]", "[]", "[1]",
    "(1, 2, 3)", "(1,)", "()", "((1, 2), (3, 4))",
    "1 ÷ 0", "0 - 1 ÷ 0", "◌",
  ]) {
    test(`round-trips ${src}`, () => {
      expect(value(`String(CodeEvaluate(String(${src})))`)).toBe(value(`String(${src})`))
    })
  }

  // and the values themselves, where they can be compared elementwise
  for (const src of ["42", "[1, 2, 3]", "[[1, 2], [3, 4]]", "1 ÷ 0"]) {
    test(`round-trips ${src} by value`, () => {
      expect(value(`min(CodeEvaluate(String(${src})) = (${src}))`)).toBe(1)
    })
  }

  test("a string comes back quoted, and as a string", () => {
    expect(value(`String("hi")`)).toBe('"hi"')
    // CodeEvaluate hands back a String object, not a primitive – coerce to compare
    expect(String(value(`CodeEvaluate(String("hi"))`))).toBe("hi")
    expect(String(value(`CodeEvaluate(String("a\\"b"))`))).toBe('a"b')   // embedded quote survives
  })

  test("◌ round-trips as ◌, not as the host's null", () => {
    expect(value("String(◌)")).toBe("◌")
    expect(run("CodeEvaluate(String(◌))")).toBeNull()
  })

  test("rank survives, which comma-joining could not", () => {
    expect(value("String([[1, 2], [3, 4]])")).toBe("[[1, 2], [3, 4]]")
    expect(value("String((1, 2, 3))")).toBe("(1, 2, 3)")
  })
})

// A name that resolves to nothing is the shape almost every language change leaves
// behind in shipped Fluent source: `self` was deleted, `-` stopped being an identifier
// character, a builtin got renamed. Evaluating the source does NOT catch it – the
// failure is an Error in an intermediate statement, and a Fluent sequence discards
// those, so `neigh-bours: 3` binds `bours`, leaves `neigh` unbound, and every value in
// scope still looks fine. That is how the Tour shipped broken with the suite green.
// And a name the source binds must not be a built-in's: a reader takes it for one.
describe("every name in shipped Fluent source resolves", () => {
  // prelude names are own keys of a scope; the natives, widgets and sources are on
  // its prototype
  const scope = createScope()
  const builtins = new Set([...Object.keys(scope), ...Object.keys(DefaultEnvironment)])
  const names = (source: string) => {
    const tree: any = CodeParse(source)
    const bound = new Set<string>(), used = new Set<string>()
    const collect = (n: any) => {
      if (!n || typeof n !== "object") { return }
      if (Array.isArray(n)) { n.forEach(collect); return }
      // a backtick literal is drawn, not run – its free symbols are the point
      if (n.type === "Code") { return }
      // `name: value` – an Operation over ":" whose first operand is a Symbol – and
      // a lambda's parameters
      if (n.type === "Operation" && symbolOf(n.content?.operator) === ":") { bound.add(symbolOf(n.content?.args?.content?.value?.[0]) ?? "") }
      if (n.type === "Lambda") { for (const a of n.content.args) { bound.add(symbolOf(a) ?? "") } }
      // operators are symbols too; only letter-led names can be "unresolved"
      const s = symbolOf(n)
      if (s && /^[\p{L}\p{N}_]/u.test(s)) { used.add(s) }
      for (const k of Object.keys(n)) { if (k !== "origin") { collect(n[k]) } }
    }
    collect(tree)
    bound.delete("")
    return { parsed: tree.type !== "Error", unresolved: [...used].filter((n) => !bound.has(n) && !builtins.has(n)), shadowed: [...bound].filter((n) => builtins.has(n)) }
  }
  for (const [name, src] of [...Object.entries(EXAMPLES), ["the Tour", TOUR_SOURCE]] as [string, string][]) {
    test(name === "the Tour" ? name : `gallery: ${name}`, () => {
      expect(names(src.trim())).toEqual({ parsed: true, unresolved: [], shadowed: [] })
    })
  }

  test("the README's names at a glance are each one function", async () => {
    // a row's glyph, word and full name are bound to one function – or, for a glyph
    // that is two functions by arity (× is mul and sign), to the one the row names –
    // and a cell left empty (—) has no name in the prelude
    const readme = await Bun.file(new URL("README.md", import.meta.url)).text()
    const table = readme.slice(readme.indexOf("| glyph | word | full name |"), readme.indexOf("</details>"))
    const scope = createScope(), p = engine.tensor([0.5, 2, 3]), q = engine.tensor([3, 2, 1])
    const namesOf = new Map<unknown, string[]>()
    for (const k in scope) { namesOf.set(scope[k], [...(namesOf.get(scope[k]) ?? []), k]) }
    const agree = (f: any, g: any) => {
      if (f === g) { return true }
      const same = (call: (h: any) => any) => { try { return JSON.stringify(call(f).js()) === JSON.stringify(call(g).js()) } catch { return false } }
      return same((h) => h(p)) || same((h) => h(p, q))
    }
    const rows = table.split("\n").slice(2).filter((row) => row.startsWith("|"))
    expect(rows.length).toBeGreaterThan(30)
    for (const row of rows) {
      const [glyph, word, full] = row.split("|").slice(1, 4).map((cell) => cell.match(/^\s*`([^`]+)`/)?.[1])
      const fns = [glyph, word, full].flatMap((n) => n ? [scope[n]] : [])
      const unlisted = (namesOf.get(fns[0]) ?? []).filter((n) => (!glyph && !/^[A-Za-z]/.test(n)) || (!word && /^[a-z]/.test(n)))
      expect([row, fns.every((f) => typeof f === "function" && agree(fns[0], f)), unlisted]).toEqual([row, true, []])
    }
  })
})

// The Tour's challenges check what a learner's code does: each room's solutions light
// it up, and the cheats that only look like one stay dark
describe("the Tour's checks", () => {
  const scope = scopeOf(TOUR_SOURCE)
  // challenge(seed, check, done, hint), wherever the Tour calls it
  const checks: [string, Function][] = []
  const find = (n: any) => {
    if (!n || typeof n !== "object") { return }
    if (n.type === "Operation" && symbolOf(n.content?.operator) === "challenge") {
      const [seed, check] = n.content.args.content.value
      checks.push([seed.content.value, evaluateSyntaxTreeNode(check, scope) as Function])
    }
    for (const k of Object.keys(n)) { if (k !== "origin") { find(n[k]) } }
  }
  find(CodeParse(TOUR_SOURCE))
  const lights = (seed: string, src: string) => {
    const check = checks.find(([s]) => s.includes(seed))![1]
    return unwrap(evaluateGeneration(() => check(CodeEvaluate.call(Object.create(scope), src), src))) === 1
  }
  const rooms: [string, string[], string[]][] = [
    ["make this say 7", ["1 + 2*3"], ["7", "7 ; +*", "1 + 6 ; *", "1 + (2 * 3)"]],
    ["fish out the 30", ["[10, 20, 30] _ 2", "[10, 20, 30]_2"], ["30"]],
    ["turn this into", ["1 ... 5 * 2", "(1 ... 5) × 2"], ["[2, 4, 6, 8, 10]"]],
    ["waste of good bananas", ["🍌: 7, 🍌 * 🍌"], ["🍌: 7, 49"]],
    ["now sum 1 through 100", ["sum(1 ... 100)", "Σ(1 ... 100)", "TensorSum(1 ... 100)"], ["(5050)", "5050"]],
    ["now put plus in the middle", ["plus: add, 40 plus 2", "plus: add,\n40 plus 2 ; ← now put plus in the middle"],
      ["plus: add, 42", "plus: add, 40 + 2", "plus: add, plus(40, 2)"]],
    ["almost. a cube is", ["cube: { x | x^3 }, cube(3)", "cube: { x | x × x × x }, ; ← almost.\ncube(3) ; done"], ["cube: { x | x^2 }, 27"]],
    ["the slope of x·x·x, at 2", ["f: { x | x^3 }, df: grad(f), df(2)", "f: { x | x^3 },\ndf: ∇(f),\ndf(2) ; ← now: the slope of x·x·x, at 2"],
      ["f: { x | x^2 }, df: grad(f), 12", "f: { x | x^3 }, df: { x | 3 × x^2 }, df(2)"]],
  ]
  for (const [seed, solutions, cheats] of rooms) {
    test(`“${seed}”`, () => {
      expect(solutions.map((src) => [src, lights(seed, src)])).toEqual(solutions.map((src) => [src, true]))
      expect(cheats.map((src) => [src, lights(seed, src)])).toEqual(cheats.map((src) => [src, false]))
    })
  }
})

describe("lifts created inside owned computeds (reactive cells)", () => {
  // A lift built during another computed's recompute (a CodeEvaluate cell, a
  // $-thunk) reaches its argument tensors only through its closure, and must
  // still compute when the outer recompute has finished.
  const cell = (x: number, code: string) => scopeOf(`x: $(${x}), code: $("${code}"), r: CodeEvaluate(code), r`) as Record<string, any>

  test("an inner lift's constants survive the cell's recompute", () => {
    const { x, r } = cell(0.5, "x * 100")
    expect(unwrap(r)).toBe(50)
    x.value = engine.tensor(0.25)
    expect(unwrap(r)).toBe(25)
    x.value = engine.tensor(0.1)
    expect(unwrap(r)).toBeCloseTo(10)
  })

  test("a chained lift in a cell: a range over a derived signal", () => {
    const { x, r } = cell(0.3, "0 ... 10*x")
    expect(unwrap(r)).toEqual([0, 1, 2, 3])
    x.value = engine.tensor(0.8)
    expect(unwrap(r)).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8])
  })

  test("editing the cell code rebuilds its lifts cleanly", () => {
    const { x, code, r } = cell(0.5, "x * 100")
    expect(unwrap(r)).toBe(50)
    code.value = "x * 1000"
    expect(unwrap(r)).toBe(500)
    x.value = engine.tensor(0.1)
    expect(unwrap(r)).toBeCloseTo(100)
  })
})

describe("error messages carry context", () => {
  test("an Error in arithmetic says so, and keeps the Error as its cause", () => {
    const e = run(`Error("x") × 2`) as Error
    expect(e.message).toBe("expected a tensor, got an Error")
    expect((e.cause as Error).message).toBe("x")
  })
  test("a cascade that matches nothing names the arguments and the reason", () => {
    const v = run(`cascade((guard(1 = 2, { 7 }),))(5)`) as Error
    expect(v).toBeInstanceOf(Error)
    expect(v.message).toContain("no overload matched")
    expect(v.message).toContain("a number")
    expect(v).toBeInstanceOf(NoMatch)
    expect((v.cause as Error).message).toContain("condition doesn't hold")
  })
  test("a failing application inside a cell says which application failed", () => {
    const v = unwrap(scopeOf(`x: $(0.5), code: $("x ⍴ [2, 2]"), r: CodeEvaluate(code), r`)["r"])
    expect(v).toBeInstanceOf(Error)
    expect(v.message).toContain("applying TensorReshape")
    expect(v.message).toContain("a tensor [2]")
    expect((v.cause as Error).message).toContain("reshape")
  })
  test("a monster parse-expectation list collapses to a summary", () => {
    const [err] = getParseErrors("1 + (")
    expect(err!.message).toContain("expected an expression")
    expect(err!.message).not.toContain("⚀")
  })
  test("a short parse-expectation list stays verbatim", () => {
    const [err] = getParseErrors("(1, 2")
    expect(err!.message).toContain(`expected ")"`)
  })
})

// The host speaks to the page in plain data (protocol.ts): values cross as
// descriptions, live ones as ids the page holds, writes back to, and lets go of.
describe("the evaluation host", () => {
  const after = (start: number, kind: ToPage["t"], timeout = 3000): Promise<any> => new Promise((resolve, reject) => {
    const t0 = Date.now()
    const poll = () => {
      const found = inbox.slice(start).find((m) => m.t === kind)
      if (found) { resolve(found); return }
      if (Date.now() - t0 > timeout) { reject(new Error(`no "${kind}" message in ${timeout} ms`)); return }
      setTimeout(poll, 5)
    }
    poll()
  })
  const evaluate = async (source: string): Promise<any> => {
    const start = inbox.length
    toHost({ t: "code", source })
    return (await after(start, "result")).desc
  }
  const numberIn = (d: any): number | undefined => d?.payload?.[0]
  const pause = (ms: number) => new Promise((r) => setTimeout(r, ms))

  test("it starts by listing the environment, with doc cards", () => {
    const ready = inbox.find((m) => m.t === "ready") as Extract<ToPage, { t: "ready" }>
    const sum = ready.names.find(([name]) => name === "sum")
    expect(sum?.[2]?.doc).toContain("Sum of the elements")
  })

  test("a program's value arrives described, with its values", async () => {
    const d = await evaluate("1 + 1")
    expect(d.k).toBe("tensor")
    expect(numberIn(d)).toBe(2)
  })

  test("a live value the page let go of lets go of its pictures", async () => {
    const d = await evaluate(`x: $(0), (Button("go", { x(x() + 1) }), $({ fill([5, 5], x()) }))`)
    const picture = d.items[1].now
    expect(picture.paint).toBe(true)
    let painted = 0
    const canvas = { width: 0, height: 0, getContext: () => ({ createImageData: (w: number, h: number) => ({ data: new Uint8ClampedArray(w * h * 4) }), putImageData: () => { painted++ } }) }
    toHost({ t: "canvas", slot: 1, canvas: canvas as unknown as OffscreenCanvas })
    toHost({ t: "paint", slot: 1, id: picture.id, size: [0, 0] })
    await pause(50)
    expect(painted).toBe(1)
    toHost({ t: "drop", id: d.items[1].id, count: 1 })
    toHost({ t: "paint", slot: 1, id: picture.id, size: [0, 0] })
    await pause(50)
    expect(painted).toBe(1)
  })

  test("Fetch of a missing file is an Error, not the server's 404 page", async () => {
    const served = Bun.serve({ port: 0, fetch: () => new Response("Not Found", { status: 404 }) })
    extendEnvironment({ Fetch: hostFetch } as Record<string, Value>)
    const d = await evaluate(`Fetch("http://localhost:${served.port}/nope.txt")`)
    const start = inbox.length
    const { lives } = await after(start, "lives")
    served.stop()
    extendEnvironment({ Fetch: loading.Fetch } as Record<string, Value>)
    const desc = lives.find((l: any) => l.id === d.id).desc
    expect(desc).toMatchObject({ k: "error" })
    expect(desc.chain.at(-1).message).toContain("404")
    toHost({ t: "drawn" })
  })

  test("a write is answered, and what depends on it follows", async () => {
    const d = await evaluate("x: $(0.5), (Slider(x), x ^ 2)")
    const [slider, squared] = d.items
    expect(slider.type).toBe("Slider")
    const target = slider.slots.target
    expect(target.k).toBe("live")
    expect(numberIn(target.now)).toBe(0.5)
    expect(numberIn(squared.now)).toBe(0.25)
    const start = inbox.length
    toHost({ t: "set", id: target.id, value: 0.8, seq: 1 })
    const { lives } = await after(start, "lives")
    expect(lives.find((l: any) => l.id === target.id)).toMatchObject({ ack: 1 })
    expect(numberIn(lives.find((l: any) => l.id === squared.id).desc)).toBeCloseTo(0.64)
    toHost({ t: "drawn" })
  })

  test("the next batch waits until the page has drawn the last", async () => {
    const d = await evaluate("x: $(0), (Slider(x), x)")
    const id = d.items[0].slots.target.id
    let start = inbox.length
    toHost({ t: "set", id, value: 0.1, seq: 1 })
    await after(start, "lives")
    start = inbox.length
    toHost({ t: "set", id, value: 0.2, seq: 2 })
    await pause(100)
    expect(inbox.slice(start).some((m) => m.t === "lives")).toBe(false)
    toHost({ t: "drawn" })
    const { lives } = await after(start, "lives")
    const update = lives.find((l: any) => l.id === id)
    expect(update.ack).toBe(2)
    expect(numberIn(update.desc)).toBeCloseTo(0.2)
    toHost({ t: "drawn" })
  })

  test("a live value the page let go of stops sending", async () => {
    const d = await evaluate(`x: $(0), (Button("go", { x(x() + 1) }), x)`)
    const [button, x] = d.items
    const go = button.slots.onClick.id
    let start = inbox.length
    toHost({ t: "call", id: go })
    expect(numberIn((await after(start, "lives")).lives[0].desc)).toBe(1)
    toHost({ t: "drawn" })
    toHost({ t: "drop", id: x.id, count: 1 })
    start = inbox.length
    toHost({ t: "call", id: go })
    await pause(100)
    expect(inbox.slice(start).some((m) => m.t === "lives")).toBe(false)
  })

  // Every shipped demo through the host, as the page gets it: plain data, and no
  // error in any cell it shows – the smoke suite stops at the top-level value, and a
  // demo whose cells error inside a Grid passed it.
  const errorsIn = (d: any, path = ""): string[] => {
    if (!d || typeof d !== "object") { return [] }
    if (d.k === "error") { return [`${path}: ${d.chain.map((c: any) => c.message).join(" ← ")}`] }
    const parts: [string, any][] =
      d.k === "list" ? d.items.map((x: any, i: number) => [`${path}[${i}]`, x])
        : d.k === "record" ? d.entries.map(([k, v]: [string, any]) => [`${path}.${k}`, v])
          : d.k === "ui" ? [
            ...Object.entries(d.slots).map(([k, v]) => [`${path}.${d.type}.${k}`, v] as [string, any]),
            ...d.children.map((c: any, i: number) => [`${path}.${d.type}[${i}]`, c] as [string, any]),
          ]
            : d.k === "live" ? [[`${path}()`, d.now]] : []
    return parts.flatMap(([p, x]) => errorsIn(x, p))
  }
  for (const [name, src] of [...Object.entries(EXAMPLES), ["Tour", "Tour"]] as [string, string][]) {
    test(`${name} crosses to the page whole`, async () => {
      const d = await evaluate(src.trim())
      expect(() => structuredClone(d)).not.toThrow()
      expect(errorsIn(d)).toEqual([])
    })
  }
  test("every message is plain data", async () => {
    // and the last demo's loops retire
    await evaluate("0")
    for (const m of inbox) { structuredClone(m) }
  })
})

describe("numbers come out right", () => {
  test("division is a true quotient: n ÷ n is 1 for every n", () => {
    expect(value("Σ(((1 ..< 1000) ÷ (1 ..< 1000)) ≠ 1)")).toBe(0)
    expect(value("(1 ÷ 3) × 3")).toBe(1)
    expect(value("∇({ y | 2 ÷ y })(4)")).toBe(-0.125)
  })
  test("functions that misbehaved at their edges", () => {
    expect(value("∇(sinc)(0)")).toBe(0)                                   // not NaN from the 0 ÷ 0 branch
    expect(value("∇({ x | where(x = 0, 1, sin(x) ÷ x) })(0)")).toBe(0)
    expect(value("∇({ x | where(x > 0, log(x), x) })([1, 2, 0])")).toEqual([1, 0.5, 1])
    expect(Number.isNaN(value("∇({ x | where(x > 0, √(x), x) × (0 ÷ 0) })(1)"))).toBe(true)
    expect(value("sinh(1e-6)")).toBeCloseTo(1e-6, 12)                     // not cancelled to noise
    expect(value("hypot(1e20, 1e20)")).toBeCloseTo(Math.SQRT2 * 1e20, -14) // not Inf
    expect(value("-8 √ 3")).toBe(-2)                                      // an odd root of a negative
    expect(Number.isNaN(value("-16 √ 4"))).toBe(true)                     // an even one has none
    expect(value("argmax([1, 0 ÷ 0, 3])")).toBe(1)                        // the first NaN, as numpy
    expect(value("argmin([3, 1, 0 ÷ 0])")).toBe(2)
  })
  test("windows and chunks are views of the same numbers", () => {
    expect(value("windows(3, [1, 2, 3, 4, 5])")).toEqual([[1, 2, 3], [2, 3, 4], [3, 4, 5]])
    expect(value("windows(2, [[1, 2], [3, 4], [5, 6]])")).toEqual([[[1, 2], [3, 4]], [[3, 4], [5, 6]]])
    expect(value("chunks(2, [1, 2, 3, 4, 5, 6])")).toEqual([[1, 2], [3, 4], [5, 6]])
  })
  test("fft's fast path agrees with the DFT", () => {
    for (const n of [64, 256]) {
      const x = Array.from({ length: n }, (_, i) => Math.fround(Math.sin(i * 0.37) + ((i * 7919) % 13) / 13 - 0.5))
      const got = value(`fft([${x.join(", ")}])`) as number[][]
      let worst = 0, top = 0
      for (let k = 0; k <= n / 2; k++) {
        let re = 0, im = 0
        for (let j = 0; j < n; j++) { const t = 2 * Math.PI * ((j * k) % n) / n; re += x[j]! * Math.cos(t); im -= x[j]! * Math.sin(t) }
        worst = Math.max(worst, Math.abs(got[0]![k]! - re), Math.abs(got[1]![k]! - im)); top = Math.max(top, Math.abs(re), Math.abs(im))
      }
      expect(worst / top).toBeLessThan(1e-5)
    }
  })
  test("scan is a running fold, ∇ included", () => {
    expect(value("[1, 2, 3, 4] scan +")).toEqual([1, 3, 6, 10])
    expect(value("[3, 1, 4, 1, 5, 9, 2, 6] scan ⌈")).toEqual([3, 3, 4, 4, 5, 9, 9, 9])
    expect(value("scan([[1, 2, 3], [4, 5, 6]], +, 0)")).toEqual([[1, 2, 3], [5, 7, 9]])
    expect(value("∇({ x | Σ((x scan +) × [1, 10, 100]) })([1, 2, 3])")).toEqual([111, 110, 100])
  })
  test("pipe, apply and zerosLike", () => {
    expect(value("(1, 2) pipe add")).toBe(3)
    expect(value("add apply (1, 2)")).toBe(3)
    expect(value("zerosLike([1, 2])")).toEqual([0, 0])
  })
  test("a literal is exactly its numbers", () => {
    const n = 8192
    const got = value(`[${Array.from({ length: n }, (_, i) => i).join(", ")}]`) as number[]
    expect(got.every((v, i) => v === i)).toBe(true)
  }, 30_000)
})

describe("plot: lines on one pair of axes", () => {
  test("a number as the codes of its digits, worked out without reading it back", () => {
    const shown = (x: number) => value(`CodesToString(DrawDigits(${x}))`).trimEnd()
    expect([2.31, 0.052, 1200, -0.5, 9.996, 0.99999, 0.00014, 0, 3, 38.44].map(shown)).toEqual(["2.31", "0.0520", "1200", "-0.500", "10.0", "1.00", "0.00014", "0", "3", "38.4"])
  })
  test("letters are their glyphs' lit cells", () => {
    // "1" lights 8 of its 15 cells, in three channels
    expect(value(`Σ(canvas(4, 5, 0) . draw(letters("1", [0, 0], 1), 1, 1))`)).toBe(24)
    expect(value(`Σ(canvas(8, 5, 0) . draw(letters("i ", [0, 0], 1), 1, 1)) = Σ(canvas(8, 5, 0) . draw(letters("I", [0, 0], 1), 1, 1))`)).toBe(1)
    // several texts in one shape, each at its own corner
    expect(value(`Σ(canvas(8, 5, 0) . draw(letters(("1", [49]), [[0, 0], [4, 0]], 1), 1, 1))`)).toBe(48)
  })
  test("each series draws in its own color, and a series of fewer than 2 points draws nothing", () => {
    // how many pixels of the first two series colors the picture has
    const count = (p: string) => value(`p: ${p}, near: { c | Σ(Σ(abs(p - c), -1) < 0.05) }, stack((near([0.224, 0.529, 0.898]), near([0.851, 0.349, 0.149])))`) as number[]
    const [blue, orange] = count(`plot(0..<50, (0..<50, 50 - 0..<50), ("up", "down"))`)
    expect(blue).toBeGreaterThan(300)
    expect(orange).toBeGreaterThan(300)
    expect(count(`plot(0..<1, [3])`)).toEqual([0, 0])
    expect(value(`shape(plot(linspace([-1, 1], 9), [[1, 2, 3, 4, 5, 6, 7, 8, 9]]))`)).toEqual([160, 520, 3])
  })
})

describe("the engine", () => {
  test("a computation past the memory budget is an Error, not an exhausted machine", () => {
    const big = run("x: fill([1000, 1000], 1), (x ⍴ [1000, 1000, 1, 1]) + (x ⍴ [1, 1, 1000, 1000])") as engine.Tensor
    expect(() => big.readSync()).toThrow(/GB of memory at once/)
  })
  test("the CPU backend runs the kernel it compiled, not a look-alike", async () => {
    // a concatenation of 8192 pieces: 8185 kernels alike but for their offsets, each
    // adding its own piece – a JavaScript engine that runs one's compiled code for
    // another's swaps numbers here
    const ir = await import("./engine/ir.ts")
    const { CPU } = await import("./engine/cpu.ts")
    const n = 8192
    let sum: any = null
    for (let i = 0; i < n; i++) { const piece = ir.pad(ir.reshape(ir.konst(i), [1]), 0, i, n - i - 1); sum = sum ? ir.add(sum, piece) : piece }
    const data = new CPU().realizeSync([sum])[0]!.data
    expect(Array.from(data).every((v, i) => v === i)).toBe(true)
  }, 60_000)
  test("the GPU's kernel text grows with the computation, not with how often a template repeats an operand", async () => {
    // pow's WGSL spells its operand five times; nested, an unshared operand would grow 5ⁿ
    const ir = await import("./engine/ir.ts")
    const { optimizeMany } = await import("./engine/opt.ts")
    const { printFused } = await import("./engine/gpu.ts")
    const wgslOf = (depth: number): number => {
      let v = ir.buffer("x", [64])
      for (let i = 0; i < depth; i++) v = ir.math("pow", ir.add(v, ir.konst(1)), ir.konst(2.2))
      const [k] = optimizeMany([v])
      return printFused(k!, 1).wgsl.length
    }
    expect(wgslOf(8) / wgslOf(4)).toBeLessThan(3)
  })
  test("a distance-field drawing runs each pixel's loop over segments in place", async () => {
    // pixels × segments is the drawing's whole work; written out, it is the frame rate
    const { canonical, placeholders, optimizeMany } = await import("./engine/opt.ts")
    const { toposort, numel } = await import("./engine/ir.ts")
    const drawing = run(`
      W: 640, H: 400,
      P: stack(meshgrid((0 ..< W) + 0.5, (0 ..< H) + 0.5), -1) ⍴ [H, W, 1, 2],
      segments: { a, b | pa: P - a, ba: b - a, h: clamp(Σ(pa × ba, -1, 1) ÷ Σ(ba × ba, -1, 1), 0, 1), min(√(Σ((pa - (h × ba))^2, -1)), -1) },
      xs: linspace([0, 6], 128), pts: stack((xs × 100, sin(xs) × 100 + 200), -1),
      clamp(2 - segments(pts_(0 ..< 127), pts_(1 ..< 128)), 0, 1)
    `) as engine.Tensor
    const { leaves } = canonical([drawing.node])
    const kernels = optimizeMany(placeholders([drawing.node], leaves).roots).flatMap((r) => toposort(r)).filter((u) => u.op === "FUSED")
    expect(Math.max(...kernels.map((k) => numel(k.shape)))).toBeLessThanOrEqual(640 * 400 * 2)
  })
  test("index ramps of different lengths stay different", () => {
    expect(value("concat((0 ..< 300, 0 ..< 500))_600")).toBe(300)
  })
  test("a reshape into a fractional or negative axis is an Error", () => {
    expect(run("[1, 2, 3, 4, 5] ⍴ [-1, 2]")).toBeInstanceOf(Error)
    expect(run("[1, 2, 3, 4, 5, 6] ⍴ [-2, -3]")).toBeInstanceOf(Error)
    expect(value("[1, 2, 3, 4, 5, 6] ⍴ [-1, 2]")).toEqual([[1, 2], [3, 4], [5, 6]])
  })
  test("−0 is a constant of its own, whatever was compiled before", async () => {
    const ir = await import("./engine/ir.ts")
    const { CPU } = await import("./engine/cpu.ts")
    const x = ir.buffer("x", [2]), inputs = new Map([["x", { data: Float32Array.of(1, 2), shape: [2] }]])
    const cpu = new CPU(), at = (z: number) => cpu.realizeSync([ir.recip(ir.mul(x, ir.konst(z)))], inputs)[0]!.data[0]
    expect([at(0), at(-0)]).toEqual([Infinity, -Infinity])
    // x + 0 is not x where x is −0
    expect(Object.is(cpu.realizeSync([ir.add(ir.mul(x, ir.konst(-0)), ir.konst(0))], inputs)[0]!.data[0], 0)).toBe(true)
  })
  test("a kernel computes a value it reads twice once", async () => {
    const ir = await import("./engine/ir.ts")
    const { optimize } = await import("./engine/opt.ts")
    let x = ir.buffer("x", [4096])
    for (let i = 0; i < 6; i++) x = ir.mul(x, x)                          // x^64: 6 multiplies
    const kernels = ir.toposort(optimize(x)).filter((u) => u.op === "FUSED")
    const count = (op: string) => ir.toposort(kernels[0]!.src[1]!).filter((u) => u.op === op).length
    expect([kernels.length, count("MULTIPLY"), count("LOAD")]).toEqual([1, 6, 1])
  })
  const lowered = async (root: any) => {
    const ir = await import("./engine/ir.ts")
    const { lowerMany } = await import("./engine/gpu.ts")
    return ir.toposort(lowerMany([root])[0]!).filter((u) => u.op === "KERNEL").map((u) => u.arg as { wgsl: string; dispatch: number[] })
  }
  const tiled = (kernels: { wgsl: string }[]) => kernels.some((k) => k.wgsl.includes("var<workgroup> As"))
  test("the backward matmuls tile like the forward one", async () => {
    const ir = await import("./engine/ir.ts")
    const { gradAll } = await import("./engine/grad.ts")
    const { matmul } = await import("./engine/tests/vocab.ts")
    const X = ir.buffer("X", [256, 128]), W = ir.buffer("W", [128, 64]), y = matmul(X, W)
    const [dW, dX] = gradAll(ir.sum(ir.mul(y, y)), [W, X])               // ∇'s products are guarded where the cotangent is 0
    expect([tiled(await lowered(y)), tiled(await lowered(dW)), tiled(await lowered(dX))]).toEqual([true, true, true])
  })
  test("a tile grid past 65,535 workgroups on an axis folds into two", async () => {
    const ir = await import("./engine/ir.ts")
    const { matmul } = await import("./engine/tests/vocab.ts")
    const tall = await lowered(matmul(ir.buffer("A", [1 << 20, 64]), ir.buffer("B", [64, 16])))
    expect(tiled(tall) && tall.every((k) => k.dispatch.every((d) => d <= 65535))).toBe(true)
  })
  test("joining n pieces the host does not hold takes about n/7 kernels, not n", async () => {
    const { optimizeMany } = await import("./engine/opt.ts")
    const { toposort } = await import("./engine/ir.ts")
    const pieces = Array.from({ length: 256 }, (_, i) => engine.uniform(i, [1, 300]))
    const joined = engine.concatenate(pieces)
    expect(toposort(optimizeMany([joined.node])[0]!).filter((u) => u.op === "FUSED").length).toBeLessThan(40)
    expect(joined.slice(200).js()).toEqual(pieces[200]!.slice(0).js())
  })
  // a WebGPU device that only counts what the GPU executor asks of it
  const fakeGpu = async () => {
    const { GPU } = await import("./engine/gpu.ts")
    const log = { pipelines: 0, destroyed: 0, done: [] as WeakRef<Promise<void>>[] }
    const buffer = (size: number) => ({ size, destroy: () => { log.destroyed++ } }) as unknown as GPUBuffer
    const pass = { setPipeline() {}, setBindGroup() {}, dispatchWorkgroups() {}, end() {} }
    const dev = {
      createBuffer: ({ size }: { size: number }) => buffer(size), createShaderModule: () => ({}), createBindGroup: () => ({}),
      createComputePipeline: () => { log.pipelines++; return { getBindGroupLayout: () => ({}) } },
      createCommandEncoder: () => ({ beginComputePass: () => pass, copyBufferToBuffer() {}, finish: () => ({}) }),
      queue: { writeBuffer() {}, submit() {}, onSubmittedWorkDone: () => { const p = Promise.resolve(); log.done.push(new WeakRef(p)); return p } },
      pushErrorScope() {}, popErrorScope: async () => null,
    }
    ;(globalThis as any).GPUBufferUsage ??= { STORAGE: 128, COPY_SRC: 4, COPY_DST: 8 }
    return { gpu: new GPU(dev as unknown as GPUDevice, false), log, buffer }
  }
  test("a scatter-add sums each row's own values: an ∞ in one row is not NaN in another", async () => {
    const ir = await import("./engine/ir.ts")
    const { CPU } = await import("./engine/cpu.ts")
    const inputs = new Map([["i", { data: Float32Array.of(0, 1, 1), shape: [3] }], ["v", { data: Float32Array.of(Infinity, 1, 2), shape: [3] }]])
    expect(Array.from(new CPU().realizeSync([ir.scatterAdd(ir.buffer("i", [3]), ir.buffer("v", [3]), 2)], inputs)[0]!.data)).toEqual([Infinity, 3])
  })
  test("roots that compute one value still get a buffer each", async () => {
    const ir = await import("./engine/ir.ts")
    const { gpu, buffer } = await fakeGpu(), W = ir.buffer("W", [64]), w = buffer(256)
    const [a, b] = gpu.computeSync([ir.mul(W, ir.konst(0.9)), ir.mul(W, ir.konst(0.9))], () => w)
    expect(a).not.toBe(b)
  })
  test("the GPU's caches shrink: buffers past the budget, pipelines with their plans, finished work", async () => {
    const ir = await import("./engine/ir.ts")
    const { gpu, log, buffer } = await fakeGpu(), x = ir.buffer("x", [8]), xb = buffer(32)
    for (let i = 0; i < 40; i++) gpu.release(buffer(2 ** 26))           // 40 × 64 MB: 2 GB are kept
    expect(log.destroyed).toBe(8)
    const run = (c: number) => gpu.computeSync([ir.mul(x, ir.konst(c))], () => xb)
    run(1.5)
    run(1.5)
    expect(log.pipelines).toBe(1)
    for (let c = 2; c < 2 + 512; c++) run(c)                             // 512 other plans push the first out
    run(1.5)
    expect(log.pipelines).toBe(514)                                    // and its pipeline with it
    await new Promise((r) => setTimeout(r, 0))
    Bun.gc(true)
    expect(log.done.filter((d) => d.deref()).length).toBe(0)
  })
})

describe("doc cards", () => {
  // every example on every card runs, and the ones that state a value hold:
  // `a = b` as a whole-value match, `a ≈ b` to 1%. An example that binds a name is
  // one program, its last statement the claim; otherwise each statement is its own
  const scope = createScope(), groups = new Map<Function, string[]>()
  for (const k in scope) { const v = scope[k]; if (typeof v === "function") { if (!groups.has(v)) { groups.set(v, []) } groups.get(v)!.push(k) } }
  const split = (s: string, sep: string): string[] => {
    const out: string[] = []
    let depth = 0, quoted = false, last = 0
    for (let i = 0; i < s.length; i++) {
      const c = s[i]!
      if (quoted) { if (c === "\\") { i++ } else if (c === '"') { quoted = false } continue }
      if (c === '"') { quoted = true } else if ("([{".includes(c)) { depth++ } else if (")]}".includes(c)) { depth-- }
      else if (depth === 0 && s.startsWith(sep, i)) { out.push(s.slice(last, i)); last = i + sep.length; i += sep.length - 1 }
    }
    out.push(s.slice(last))
    return out
  }
  const claim = (s: string): string => {
    const eq = split(s, " = "), approx = split(s, " ≈ ")
    return eq.length === 2 ? `(${eq[0]}) ≡ (${eq[1]})`
      : approx.length === 2 ? `max(abs((${approx[0]}) - (${approx[1]}))) ≤ (0.01 × (1 + max(abs(${approx[1]}))))`
        : s
  }
  for (const [fn, names] of groups) {
    const example = (fn as any).__meta__?.example as string | undefined
    if (!example) { continue }
    test(`${names[0]}: ${example.split("\n")[0]}`, () => {
      for (const ex of example.split("\n")) {
        const parts = split(ex, ", ")
        const binds = split(ex, ":").slice(1).some((p) => !p.startsWith("="))
        for (const [head, last] of binds ? [[parts.slice(0, -1), parts.at(-1)!] as const] : parts.map((p) => [[], p] as const)) {
          const src = [...head, claim(last)].join(", ")
          const v = value(src)
          expect([src, v instanceof Error ? v.message : "ok"]).toEqual([src, "ok"])
          if (claim(last) !== last) { expect([src, v]).toEqual([src, 1]) }
          flushRetirements()
        }
      }
    })
  }
})
