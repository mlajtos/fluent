// End-to-end tests: Fluent source in, evaluated result out.
// Runs the real pipeline – grammar → AST → evaluator → tensors – on the engine's CPU
// backend (Bun has no WebGPU). `bun test` runs this file.

import { describe, test, expect, beforeAll, afterAll } from "bun:test"
import { CodeParse, getParseErrors, evaluateSyntaxTreeNode, evaluateGeneration, flushRetirements, createScope, engine, FluentVariable, FluentData, setCodeNodePrinter, setMeta, extendEnvironment, SignalCreate, CodeEvaluate, DefaultEnvironment, NoMatch, getOrigin, PRELUDE, Signal, occurrence, keepingFrom, type Value } from "./language"
import { startHost } from "./host"
import { numberText, tensorView, connect, update, call, write, mirrors, type ToPage, type ToWorker } from "./protocol"
import { EXAMPLES } from "./examples"
import { TOUR_SOURCE } from "./tour"

// The evaluation host binds the widgets and the live sources, as in the IDE's worker.
// The page is the page's own mirror (protocol.ts), plus this inbox of all it was sent;
// each way, messages arrive later and in order, and the page draws a batch at once. A
// test may hold back what the page reads, or its frames.
const inbox: ToPage[] = []
const held: Record<"reads" | "frames", (() => void)[] | null> = { reads: null, frames: null }
const toHost: (message: ToWorker) => void = startHost((message) => {
  inbox.push(message)
  if (message.t !== "lives" && message.t !== "tensor") { return }
  const read = () => update(message)
  held.reads ? held.reads.push(read) : queueMicrotask(read)
})
const toWorker = (message: ToWorker) => { queueMicrotask(() => toHost(message)) }
const drawn = (answer: () => void) => { held.frames ? held.frames.push(answer) : queueMicrotask(answer) }
connect(toWorker, drawn)
// holds them until the function it returns lets them go
const hold = (what: keyof typeof held) => {
  held[what] = []
  return () => { const waiting = held[what] ?? []; held[what] = null; waiting.forEach((f) => f()) }
}
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
// signals and variables unwrap to their current value, tensors to plain JS arrays/numbers
const unwrap = (v: any): any => {
  while (v instanceof Signal) { v = v.value }
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
  // the combinators are prelude Fluent; what they make prints as the call that made it,
  // and that text evaluates back to a function that answers the same
  test("into preprocesses every argument it is given, however many", () => {
    expect(value("(abs ⍥ { a, b, c | a + b + c })(-1, -2, -3)")).toBe(6)
    expect(value("-3 (abs ⍥ =) 3")).toBe(1)
  })
  test("a combinator's function prints as the call that made it, and reads back", () => {
    const cases: [string, string, number][] = [
      ["⍨ -", "3, 10", 7], ["abs ⍥ =", "-3, 3", 1], ["Φ(Σ, ÷, #)", "[1, 2, 3, 4]", 2.5],
      ["1 ⊸ +", "41", 42], ["÷ ⟜ √", "16", 4], ["swap(-)", "3, 10", 7], ["compose(√, +)", "9, 16", 5],
    ]
    for (const [made, args, answer] of cases) {
      expect(value(`String(${made})`)).toBe(made)
      expect(value(`CodeEvaluate(String(${made}))(${args})`)).toBe(answer)
    }
    // a prelude value prints as its definition, never as JavaScript
    expect(value("String(⊢)")).toBe("FunctionArity(({ a, b | b }, { a | a }))")
    expect(value("String(.)")).toBe("{ x, f | f @ x }")
    expect(value("String(compose)")).toBe("⍨ then")
  })
  test("a fork given two tines is an Error, not a constant ◌", () => {
    expect((run("Φ(Σ, ÷)([1, 2, 3, 4])") as Error).message).toContain("takes three tines")
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
  test("ListTake/ListDrop clamp to the list length – no over-take; a negative n counts from the end", () => {
    expect(value("ListLength(ListTake((1, 2, 3), 5))")).toBe(3)
    expect(value("ListGet(ListTake((1, 2, 3), 5), 2)")).toBe(3)
    expect(value("ListLength(ListDrop((1, 2, 3), 5))")).toBe(0)
    expect(value("ListTake((1, 2, 3), -1)_0")).toBe(3)
    expect(value("ListLength(ListDrop((1, 2, 3), -1))")).toBe(2)
    expect(value("ListLength(ListTake((1, 2, 3), -5))")).toBe(3)
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
    expect(value("reverse([[1, 2], [3, 4]])")).toEqual([[3, 4], [1, 2]])        // the items: rows
    expect(value("reverse([[1, 2], [3, 4]], [0, 1])")).toEqual([[4, 3], [2, 1]])
    expect(value("reverse(5)")).toBe(5)
    expect(value("reverse([[1, 2], [3, 4]], 1)")).toEqual([[2, 1], [4, 3]])
    expect(value("sinc(0)")).toBe(1)   // sinc(0) = 1 by definition
  })
  test("memo keeps an answer by the value of its arguments", () => {
    let calls = 0
    const kept = createScope().memo as Function, echo = kept((x: Value) => { calls++; return x })
    for (const x of ["ab", "ab", engine.tensor([1, 2]), engine.tensor([1, 2]), engine.tensor([[1, 2]]), [engine.tensor(1), "ab"], [engine.tensor(1), "ab"]]) echo(x)
    expect(calls).toBe(4)
  })
  test("einsum reads a spec once for the shapes it meets", async () => {
    // a training step calls it every tick: the second call only multiplies and sums
    const { konst } = await import("./engine/ir.ts")
    const f = run(`q: randn([4, 3, 2]), k: randn([4, 3, 2]), { einsum("btc,bsc->bts", q, k) }`) as () => unknown
    f()
    const before = konst(0).id
    f()
    expect(konst(0).id - before).toBeLessThan(40)
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
  test("rank ⍤'s two frames agree as they do under +", () => {
    expect(run("[[1,2,3],[4,5,6]] (+ ⍤ 0) [[10,20],[30,40],[50,60]]")).toBeInstanceOf(Error)   // [2 3] vs [3 2]
    expect(run("[1,2,3,4,5,6] (+ ⍤ 0) [[10,20],[30,40],[50,60]]")).toBeInstanceOf(Error)
    expect(value("[1,2] (+ ⍤ 0) [[10],[20]]")).toEqual(value("[1,2] + [[10],[20]]"))          // f ⍤ 0 is f for a scalar f
  })
  test("rank ⍤ over an empty frame gives an empty result", () => {
    expect(value("(sin ⍤ 0)([])")).toEqual([])
    expect(value("shape((softmax ⍤ 1)(fill([0, 3], 1)))")).toEqual([0, 3])
    expect(value("shape((Σ ⍤ 1)(fill([0, 3], 1)))")).toEqual([0])
    expect((run("stack(())") as Error).message).toBe("stack: needs at least one array")
  })
  // f, counting its calls: a native the scope binds as `spy`
  const counted = (source: string) => {
    let calls = 0
    const scope = createScope()
    scope.spy = (x: Value) => { calls++; return x }
    return { value: unwrap(evaluateIn(source, scope)), calls }
  }
  test("rank ⍤ runs f once, on a cell, and the frame comes free", () => {
    expect(counted("({ v | Σ(spy(v)) } ⍤ 1)(fill([5, 3], 1))")).toEqual({ value: [3, 3, 3, 3, 3], calls: 1 })
    expect(counted("({ m | spy(m) matmul transpose(m) } ⍤ 2)([[[1, 2], [3, 4]], [[1, 0], [0, 1]]])"))
      .toEqual({ value: [[[5, 11], [11, 25]], [[1, 0], [0, 1]]], calls: 1 })
    // every view and gather takes the frame: a row's own picks, its own scatter, its own scan
    expect(value("({ v | v_[2, 0] } ⍤ 1)([[1, 2, 3], [4, 5, 6]])")).toEqual([[3, 1], [6, 4]])
    expect(value("({ v | v_argmax(v) } ⍤ 1)([[1, 9, 3], [7, 2, 1]])")).toEqual([9, 7])
    expect(value("({ v | scatterAdd([0, 0, 1], v, 2) } ⍤ 1)([[1, 2, 3], [4, 5, 6]])")).toEqual([[3, 3], [9, 6]])
    expect(value("({ v | scatterAdd(v, [1, 1], 3) } ⍤ 1)([[0, 2], [1, 1]])")).toEqual([[1, 0, 1], [0, 2, 0]])
    expect(value("({ v | v scan + } ⍤ 1)([[1, 2, 3], [4, 5, 6]])")).toEqual([[1, 3, 6], [4, 9, 15]])
    expect(value("((Σ ⍤ 1) ⍤ 2)(fill([2, 3, 4], 1))")).toEqual([[4, 4, 4], [4, 4, 4]])
  })
  test("rank ⍤ runs f cell by cell when f reads its cell's numbers or writes state", () => {
    // the trace, then each of the two cells
    expect(counted("({ x | cond(guard(Σ(spy(x)) > 0, { x }), { -x }) } ⍤ 1)([[1, 2], [-3, -4]])"))
      .toEqual({ value: [[1, 2], [3, 4]], calls: 3 })
    // a bounds check reads the index: one out of range is the loop's Error
    expect((run("({ v | v_(v_0) } ⍤ 1)([[1, 2, 3], [7, 5, 6]])") as Error).message).toBe("index 7 is out of bounds for an axis of length 3")
    expect(value("c: $0, ({ v | c(c() + 1), Σ(v) } ⍤ 1)([[1, 2], [3, 4], [5, 6]]), c()")).toBe(3)
  })
  test("rank ⍤ draws ⚄ for every cell, and ∇ goes through it", () => {
    const r = value("({ v | ⚄(shape(v)) } ⍤ 1)(fill([3, 2], 0))") as number[][]
    expect(new Set(r.map(String)).size).toBe(3)
    // nested: each outer cell draws its own, shared by its inner cells
    const s = value("({ r | ({ x | x + ⚄([]) } ⍤ 0)(r) } ⍤ 1)(fill([2, 2], 0))") as number[][]
    expect(s[0]![0]).not.toBe(s[1]![0])
    expect(value("(∇{ x | Σ((softmax ⍤ 1)(x) × [1, 2, 3]) })([[1, 2, 3], [3, 2, 1]])"))
      .toEqual(value("(∇{ x | Σ(softmax(x) × [1, 2, 3]) })([[1, 2, 3], [3, 2, 1]])"))
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
  test("a kernel of lower rank runs over the last axes, each row on its own", () => {
    expect(value("[[1,2,3,4],[10,20,30,40]] ⊛ [1,1,1]")).toEqual([[3, 6, 9, 7], [30, 60, 90, 70]])
    expect(value("[[3,1,2],[6,4,5]] ⊛ [1,1]")).toEqual([value("[3,1,2] ⊛ [1,1]"), value("[6,4,5] ⊛ [1,1]")])
    expect(value("shape(fill([2,3,4],1) ⊛ [1,1,1])")).toEqual([2, 3, 4])
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
  test("a write stores a live value as it is now, and does not follow it", () => {
    expect(value("a: $(1), b: $(0), b(a), b + 1")).toBe(2)
    expect(value("a: $(1), b: $(0), b(a), c: b + 1, a(5), c")).toBe(2)
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
  test("chains fed by a computed of θ track assignments", () => {
    expect(value("θ: ~([2]), w: ${ θ }, y: w + 1, θ := [7], y")).toEqual([8])
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
  test("⟳ follows a live count: raised, the loop runs on to the new total", async () => {
    const scope = scopeOf("budget: $1, n: $0, { n(n() + 1) } ⟳ budget")
    await new Promise((r) => setTimeout(r, 120))
    expect(unwrap(scope.n)).toBe(1)
    ;(scope.budget as Signal).value = engine.scalar(4)
    await new Promise((r) => setTimeout(r, 200))
    expect(unwrap(scope.n)).toBe(4)
    flushRetirements()
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

// One model of time: a cell ($, ~, ~~) changes only at an instant, by a write; a
// computed is a function of the cells now; an occurrence (the program's run, a ⟳ tick,
// a press) reads the cells as they are and writes them.
describe("one model of time: cells, computeds, occurrences", () => {
  // the value is looked at first, as the IDE shows it, then the steps run
  const steps = (source: string, step: string, times: number, show: string) => {
    const scope = scopeOf(source)
    unwrap(scope[show])
    for (let i = 0; i < times; i++) { occurrence(() => (scope[step] as Function)()) }
    return unwrap(scope[show])
  }
  test("looking never changes a value", () => {
    expect(value("θ: ~1, j: ${ θ × 2 }, j(), θ:= 5, j()")).toBe(10)
    expect(value("θ: ~1, j: ${ θ × 2 }, θ:= 5, j()")).toBe(10)
  })
  test("a computed of a variable follows it after the program has run", () => {
    expect(steps("θ: ~1, j: ${ θ × 2 }, step: { θ:= θ + 1 }, (θ, j, step)", "step", 2, "j")).toBe(6)
    // the cloth demo's case: a picture of the pose, with no copy of the pose published
    expect(steps("θ: ~[0], mesh: ${ θ × 10 }, step: { θ:= θ + 1 }", "step", 2, "mesh")).toEqual([20])
  })
  test("a write happens whether or not anything shows it", () => {
    expect(value("s: $[5], θ: ~[0], θ:= s, θ")).toEqual([5])
    expect(value("s: $[5], θ: ~[0], r: θ:= s, θ")).toEqual([5])
  })
  test("a computed cannot change state; it may change cells it made", () => {
    const scope = scopeOf("x: $1, c: ${ x(2), 3 }, c")
    expect(unwrap(scope.c)).toBeInstanceOf(Error)
    expect((unwrap(scope.c) as Error).message).toContain("cannot change state")
    expect(unwrap(scope.x)).toBe(1)
    expect(value("c: ${ y: $1, y(2), y() + 1 }, c")).toBe(3)
  })
  test("∇ sees through a computed, every time", () => {
    const plain = (v: any): any => { while (v instanceof Signal) { v = v.value } return Array.isArray(v) ? v.map(plain) : unwrap(v) }
    expect(plain(value("θ: ~[0], view: ${ θ × 10 }, 𝓛: { Σ((view() - 30)^2) }, opt: sgd(0.0025), r1: opt(𝓛), r2: opt(𝓛), (r1, r2, θ)")))
      .toEqual([900, expect.closeTo(225, 3), [expect.closeTo(2.25, 5)]])
    expect(plain(value("θ: ~2, c: ${ θ × 3 }, r1: (∇{ c() })(), r2: (∇{ c() })(), (ListGet(r1, 2), ListGet(r2, 2))"))).toEqual([[3], [3]])
  })
  test("a computed shown while ∇ runs keeps its own value", () => {
    expect(value("θ: ~2, c: ${ θ × 3 }, c(), (∇{ c() })(), c()")).toBe(6)
  })
  test("a derived value is never seen half-updated, whatever reads it first", () => {
    expect(value("a: $1, b: ${ a() × 2 }, c: ${ a() + b() }, a(5), [c(), b()]")).toEqual([15, 10])
    expect(value("a: $1, b: ${ a() × 2 }, c: ${ a() + b() }, a(5), [b(), c()]")).toEqual([10, 15])
  })
  test("every cell reads with () and writes with (v); a variable's write is :=", () => {
    expect(value("θ: ~[1], θ([4]), θ()")).toEqual([4])
    expect(value("θ: ~[1], θ← [7], θ")).toEqual([7])
    expect(run("θ: ~[1], θ(\"text\")")).toBeInstanceOf(Error)
  })
  test("a draw is keyed by its call: looking at another draw moves it not", () => {
    const stepped = (look: boolean) => {
      const scope = scopeOf("noise: ${ ⚄([1]) }, θ: ~[0], step: { θ:= θ + ⚄([1]) }")
      if (look) { unwrap(scope.noise) }
      occurrence(() => (scope.step as Function)())
      return unwrap(scope["θ"])
    }
    expect(stepped(true)).toEqual(stepped(false))
  })
  test("an edit keeps the last run's cells by name, expression and shape, and their memory", () => {
    const program = (head: string, init = "~[0]") => `${head}, θ: ${init}, opt: sgd(0.1, 0.9), go: { opt({ Σ((θ - 3)^2) }) }`
    const rerun = (from: Record<string, Value>, source: string) => {
      const scope = createScope()
      evaluateGeneration(() => keepingFrom(from, scope, () => evaluateSyntaxTreeNode(CodeParse(source), scope)))
      return scope
    }
    const first = scopeOf(program("a: 1"))
    occurrence(() => (first.go as Function)())
    occurrence(() => (first.go as Function)())
    expect(unwrap(first["θ"])[0]).toBeCloseTo(1.62, 5)
    // an edit above it: the same cell, and its momentum carries on – 2.814, not a fresh 1.896
    const second = rerun(first, program("a: 2"))
    expect(second["θ"]).toBe(first["θ"])
    occurrence(() => (second.go as Function)())
    expect(unwrap(second["θ"])[0]).toBeCloseTo(2.814, 4)
    // an edit of its own expression starts it afresh, as does another shape
    expect(unwrap(rerun(second, program("a: 2", "~[5]"))["θ"])).toEqual([5])
    expect(unwrap(rerun(second, program("a: 2", "~[0, 0]"))["θ"])).toEqual([0, 0])
  })
  test("the same occurrences give the same states", () => {
    const program = "x: 0 ..< 8, θ: ~[0, 0], 𝓛: { μ((x × θ_0 + θ_1 - x × 3)^2) }, opt: adam(0.1), step: { opt(𝓛) }"
    const train = () => steps(program, "step", 40, "θ")
    expect(train()).toEqual(train())
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
    expect(value("s: $(1), y: sin(s), s(0), y")).toBe(0)
    expect(value("s: $(0), identical(FunctionNoAutoLift({ a | a })(s), s)")).toBe(1)
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
    expect(message("ListReduce((), { a, b | a + b })")).toContain("empty")
    expect(value("ListReduce((1, 2), { a, b, i | identical(i, ◌) }, 0)")).toBe(1)
    expect(run("ListConcat((1,), 2)")).toBeInstanceOf(Error)
    expect(message(`StringConcat("a", ◌)`)).toContain("◌")
    expect(run(`StringConcat("a", Error("x"))`)).toBeInstanceOf(Error)
  })
  test("String of a cell is the text of its value, and follows it", () => {
    expect(value("String(~~[1, 2])")).toBe("[1, 2]")
    expect(value("x: ~~[1, 2], s: String(x), x:= [3], s")).toBe("[3]")
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
  test("a higher derivative holds where a cotangent is exactly 0", () => {
    // the 0 × ∞ guard keeps the product's derivative, so an outer ∇ still sees it at 0
    expect(value("(∇(∇{ x | x × sin(x) }))(0)")).toBe(2)
    expect(value("(∇(∇gelu))(0)")).toBeCloseTo(0.798, 3)
    expect(value("(∇{ v | (∇{ z | v × z^3 })(2) })(0)")).toBe(12)
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
  test("data that follows a corpus is a computed of it", () => {
    expect(value("corpus: $([1, 2]), D: ${ corpus() × 2 }, corpus ← [7, 8, 9], sum(D())")).toBe(48)
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
  test("window and stencil slide over a vector", () => {
    expect(value("window([1, 2, 3, 4], [2])")).toEqual([[1, 2], [2, 3], [3, 4]])
    // stencil reduces each window with f
    expect(value("stencil(3, Σ, [1, 2, 3, 4, 5])")).toEqual([6, 9, 12])
    expect(value("stencil(2, μ, [2, 4, 6, 8])")).toEqual([3, 5, 7])
    // along the leading axis at any rank: each window of rows reduces to a row
    expect(value("stencil(2, Σ, [[1, 2], [3, 4], [5, 6]])")).toEqual([[4, 6], [8, 10]])
    // a shape-keeping f keeps each window
    expect(value("stencil(3, sort, [3, 1, 2, 5, 4])")).toEqual([[1, 2, 3], [1, 2, 5], [2, 4, 5]])
    // ⌺ is the operator: f and the width give the function
    expect(value("(Σ ⌺ 3)([1, 2, 3, 4, 5])")).toEqual([6, 9, 12])
    expect(value("(μ ⌺ 2)([[1, 2], [3, 4], [5, 6]])")).toEqual([[2, 3], [4, 5]])
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
    expect(value("÷([2, 4])")).toEqual([0.5, 0.25]) // unary reciprocal
    expect(value("^(0)")).toBe(1)                 // unary exp: ^x is e^x
    expect(value("^(1)")).toBeCloseTo(Math.E, 6)
    expect(value("[3, 4] ÷ 2")).toEqual([1.5, 2])
    expect(value("2 ^ [1, 2, 3]")).toEqual([2, 4, 8])
    // resistors in parallel: add the reciprocals, and come back
    expect(value("(∥): ÷ ⍢ +, 6 ∥ 3")).toBeCloseTo(2, 5)
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
  test("a decline is an Error that records no JavaScript stack: a choice makes one per candidate", () => {
    for (const source of ["cascade((guard(0, { 1 }),))(5)", "take(sin, 1)", "NoMatch"]) {
      const v = run(source) as NoMatch
      expect([source, v instanceof NoMatch, v instanceof Error, v.stack, String(v).startsWith("NoMatch: ")]).toEqual([source, true, true, undefined, true])
    }
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
  test("solve finds the x near the seed, or says it didn't land", () => {
    expect(value("solve(cos, 0, 1)")).toBeCloseTo(Math.PI / 2, 5)
    // cos is flat at 0, so Newton can't step from there – though π/2 solves it
    const none = run("solve(cos, 0, 0)") as Error
    expect(none).toBeInstanceOf(Error)
    expect(none).not.toBeInstanceOf(NoMatch)
    expect(none.message).toStartWith("solve: Newton's method didn't reach g(x) = y from the seed")
    expect(none.message).not.toContain("no solution near the seed")
    expect(value("inverse({ x | x^3 + x })(10)")).toBeCloseTo(2, 5)
    expect(value("2 inverse(^) 8")).toBeCloseTo(3, 5)
    expect(value("inverse(log)([1, 2, 3])")).toEqual([Math.E, Math.E ** 2, Math.E ** 3].map((x) => expect.closeTo(x, 4)))
  })
  test("∇ of an inverse is 1 ÷ the slope, also where Newton lands exactly", () => {
    // Newton's residual is exactly 0 when it lands: its cotangent too, which the 0 × ∞ guard must not drop
    expect(value("g: { x | x^3 + x }, (∇{ y | (g ⍣ -1)(y) })([10, 2, 30])")).toEqual([1 / 13, 1 / 4, 1 / 28].map((x) => expect.closeTo(x, 5)))
    expect(value("(∇{ y | (square ⍣ -1)(y) })(9)")).toBeCloseTo(1 / 6, 5)
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
  const size = (v: unknown) => engine.graphSize((v instanceof FluentVariable ? v.value : v) as engine.Tensor)
  test("a variable assigned in a loop stays a leaf", () => {
    expect(size(scopeOf("θ: ~([1, 2]), ({ s | θ := θ × 2 + 1 } ⍣ 50)(0)")["θ"])).toBe(1)
  })
  test("a signal written in a loop stays a leaf", () => {
    expect(size((scopeOf("s: $([0, 1]), ({ i | s(s() + 1) } ⍣ 50)(0)")["s"] as Signal<unknown>).peek())).toBe(1)
  })
  test("an optimizer's parameters stay leaves however long it trains", () => {
    expect(size(scopeOf("θ: ~([0, 0]), opt: adam(0.1), ({ s | opt({ Σ((θ - [1, 2])^2) }) } ⍣ 30)(0)")["θ"])).toBe(1)
  })
  test("a value defined from a variable follows it; one taken in a step is its own", () => {
    expect(value("θ: ~(5), s: Σ(θ), θ := 7, s")).toBe(7)
    expect(value("θ: ~([[1, 2]]), m: moveaxis(θ, 1, 1), θ := [[0, 0]], m")).toEqual([[0, 0]])
    expect(value("θ: ~(5), s: $0, take: { s(Σ(θ)) }, take(), θ := 7, s()")).toBe(5)
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

describe("one vocabulary for items: a tensor's rows, a list's elements, a string's characters", () => {
  const text = (source: string) => String(value(source))
  test("take, drop and reverse rearrange the items of every kind", () => {
    expect(value("take([[1, 2], [3, 4], [5, 6]], 2)")).toEqual([[1, 2], [3, 4]])
    expect(value("take([10, 20, 30], -2)")).toEqual([20, 30])
    expect(value("#take((7, 8, 9), 2)")).toBe(2)
    expect(text('take("hello", 3)')).toBe("hel")
    expect(text('take("ab", 5)')).toBe("ab")                     // past the end: all of them
    expect(text('take("ab", 0)')).toBe("")
    expect(value("drop([1, 2, 3, 4], 1)")).toEqual([2, 3, 4])
    expect(text('drop("hello", -2)')).toBe("hel")
    expect(value("drop((7, 8, 9), 2)_0")).toBe(9)
    expect(value("reverse((1, 2, 3))_0")).toBe(3)
    expect(text('reverse("abc")')).toBe("cba")
    expect(value("take(5, 1)")).toEqual([5])                     // a number is one item
  })
  test("concat joins two of a kind, or the containers listed", () => {
    expect(value("[1, 2] concat [3]")).toEqual([1, 2, 3])
    expect(value("concat(([[1], [2]], [[3], [4]]), 1)")).toEqual([[1, 3], [2, 4]])
    expect(value("#((1, 2) concat (3,))")).toBe(3)
    expect(value("#concat(((1,), (2,), (3,)))")).toBe(3)
    expect(value("#concat(())")).toBe(0)
    expect(text('"ab" concat "cd"')).toBe("abcd")
    expect(text('concat(("ab", "cd"))')).toBe("abcd")
    // along another axis, concat ⍤ k
    expect(value("[[1], [2]] (concat ⍤ 1) [[3], [4]]")).toEqual([[1, 3], [2, 4]])
  })
  test("reduce, scan and fold fold the items, and the scan's last item is the reduction", () => {
    expect(value("(1, 2, 3) reduce +")).toBe(6)
    expect(value("[[1, 2], [3, 4]] reduce +")).toEqual([4, 6])
    expect(text('reverse("abc") reduce StringConcat')).toBe("cba")
    expect(value("TensorStack((1, 2, 3) scan +)")).toEqual([1, 3, 6])
    for (const [x, f] of [["(3, 1, 4, 1, 5)", "⌈"], ['"abc"', "StringConcat"], ["[[1, 5], [4, 2]]", "⌈"]]) {
      expect(value(`x: ${x}, (x scan ${f})_(-1) ≡ (x reduce ${f})`)).toBe(1)
    }
    expect(value("fold((1, 2, 3), { s, v | s × 10 + v }, 0)")).toBe(123)
    expect(value("fold([1, 2, 3], { s, v | s × 10 + v }, 0)")).toBe(123)
    expect(text('fold("abc", { s, c | StringConcat(c, s) }, "")')).toBe("cba")
  })
  test("each kind keeps its own member of every verb, by its own name", () => {
    const scope = createScope()
    for (const verb of ["Length", "Get", "Gather", "Take", "Drop", "Concat", "Reverse", "Reduce", "Scan"]) {
      for (const kind of ["Tensor", "List", "String"]) { expect([kind + verb, typeof scope[kind + verb]]).toEqual([kind + verb, "function"]) }
    }
  })
  test("a member declines another kind's value; one that takes the value and then fails is an Error", () => {
    for (const call of ['TensorTake("ab", 1)', "ListTake([1, 2], 1)", "StringTake((1, 2), 1)", 'ListReverse("ab")', "StringConcat([1], [2])",
      "TensorReduce((1, 2), +)", "StringScan((1, 2), +)", 'ListGather("ab", 0)', "TensorReverse((1, 2))", "TensorScan((1, 2), +)"]) {
      expect([call, run(call) instanceof NoMatch]).toEqual([call, true])
    }
    // the first member that takes the value answers, and its failure stops the cascade
    for (const call of ['take([1, 2], "a")', "(1, 2) concat 3", "ListConcat((1,), 2)", "[1, 2]_5", "ListGet((1, 2), [0, 1])"]) {
      const v = run(call)
      expect([call, v instanceof Error, v instanceof NoMatch]).toEqual([call, true, false])
    }
  })
  test("the list members read the list they are given, not the cells inside it", () => {
    expect(value("x: $1, identical(ListGet(ListConcat((x,), (2,)), 0), x)")).toBe(1)
    expect(value("x: $1, identical(ListGet(ListTake((x, 2), 1), 0), x)")).toBe(1)
  })
  test("folding nothing gives f's identity; with none, an Error says so", () => {
    expect(value("() reduce +")).toBe(0)
    expect(value("[] reduce -")).toBe(0)            // FunctionArity passes its binary's on
    expect(value("[] reduce ÷")).toBe(1)
    expect(value("() reduce ⌈")).toBe(-Infinity)
    expect(value("[] reduce ∨")).toBe(0)
    expect(value("() reduce ∧")).toBe(1)
    expect(text('"" reduce StringConcat')).toBe("")
    expect(value("fill([0, 3], 0) reduce identity({ a, b | a + b }, 1)")).toEqual([1, 1, 1])   // one row's shape
    expect(value("() reduce identity({ a, b | a + b }, 7)")).toBe(7)
    // an empty inner product of a ring whose fold has an identity
    expect(value("fill([2, 0], 0) (× ∙ identity({ a, b | a + b }, 0)) fill([0, 3], 0)")).toEqual([[0, 0, 0], [0, 0, 0]])
    expect((run("() reduce { a, b | a + b }") as Error).message).toBe("an empty fold needs f's identity – give one with identity(f, v)")
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
        if (v instanceof FluentVariable) { return walk(v.value, path, depth + 1) }
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
      const before = trained.map((k) => JSON.stringify((scope[k] as FluentVariable).value.js()))
      const failures: string[] = []
      const call = (what: string, f: Function, ...args: Value[]) => {
        try { const r = occurrence(() => f(...args)); if (r instanceof Error) { failures.push(`${what}: ${r.message}`) } } catch (e: any) { failures.push(`${what} threw: ${e?.message ?? e}`) }
      }
      const runSteps = () => { for (const step of steps.splice(0)) { call("⟳ step", step, engine.scalar(0)); call("⟳ step", step, engine.scalar(1)) } }
      runSteps()
      for (const press of presses.splice(0)) { call("button", press); runSteps() }
      expect(failures).toEqual([])
      expect(firstError(scope)).toBeNull()
      expect(trained.filter((k, i) => JSON.stringify((scope[k] as FluentVariable).value.js()) === before[i])).toEqual([])
      flushRetirements()
    }, 60_000)
  }

  test("a demo you drag descends on the clock: so many steps a second, on any machine", () => {
    const clock = SignalCreate(engine.scalar(0)), budgets: Value[] = []
    const record = (_: Value, n: Value) => { budgets.push(n); return null }
    setMeta(record, { noAutoLift: true })
    const time = DefaultEnvironment.Time
    extendEnvironment({ FunctionIterate: record, Time: () => clock } as Record<string, Value>)
    try {
      const rates = ["differentiable-reach", "differentiable-aim", "gradient-paint", "geometric-median", "differentiable-cloth"].map((name) => {
        budgets.length = 0
        scopeOf(EXAMPLES[name as keyof typeof EXAMPLES])
        clock.value = engine.scalar(0.5)
        const rate = unwrap(budgets[0]) * 2
        clock.value = engine.scalar(0)
        return [name, rate]
      })
      expect(rates).toEqual([["differentiable-reach", 60], ["differentiable-aim", 60], ["gradient-paint", 120], ["geometric-median", 60], ["differentiable-cloth", 240]])
    } finally {
      extendEnvironment({ FunctionIterate: stepLater, Time: time } as Record<string, Value>)
      flushRetirements()
    }
  })

  test("the cloth swings like a mass on springs: its far corner lags, overshoots, then comes to rest within a second", () => {
    steps.length = 0
    const scope = scopeOf(EXAMPLES["differentiable-cloth"])
    const step = steps[0]!, rate = 240
    const corner = () => ((scope.θ as FluentVariable).value.js() as number[][][]).at(-1)![0]!
    for (let i = 0; i < 4 * rate; i++) { step(engine.scalar(i)) }
    const from = corner()[0]!
    ;(scope.pinL as Signal).value = engine.tensor([-0.85, -0.25])
    const xs: number[] = []
    for (let i = 0; i < 2 * rate; i++) { step(engine.scalar(i)); xs.push(corner()[0]!) }
    const to = xs.at(-1)!, travel = to - from
    // a tenth of a second on, it has gone less than half the way; it swings past where it
    // comes to rest, and is within 5% of the way there after a second
    expect((xs[rate / 10]! - from) / travel).toBeLessThan(0.5)
    expect(Math.max(...xs.map((x) => (x - to) / travel))).toBeGreaterThan(0.1)
    expect(Math.max(...xs.slice(rate * 1.2).map((x) => Math.abs(x - to) / Math.abs(travel)))).toBeLessThan(0.05)
    flushRetirements()
  }, 60_000)

  test("deep-delta-learning learns a function: one number in, one out, right where it never trained", () => {
    steps.length = 0
    const scope = scopeOf(EXAMPLES["deep-delta-learning"])
    // the demo's own 600 steps
    for (let i = 0; i < 600; i++) { steps[0]!(engine.scalar(i)) }
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

  // a function prints as the expression that made it – whoever wrote the builder – and a
  // built-in as its name; never as its JavaScript
  test("a function made by a call prints as that call", () => {
    expect(value("String(× ⍤ 1)")).toBe("× ⍤ 1")
    expect(value("String(exp ⍣ -1)")).toBe("exp ⍣ -1")
    expect(value("String(adam(0.01))")).toBe("adam(0.01)")
    expect(value("String(sin ∘ cos)")).toBe("sin ∘ cos")
    expect(value("String(sin)")).toBe("TensorSine")
    expect(value("f: { x | { y | x + y } }, String(f(1))")).toBe("f(1)")
    // a literal keeps its own source, also when a call passes it through
    expect(value("double: doc({ x | x × 2 }, \"double(x)\", \"twice\", \"\"), String(double)")).toBe("{ x | x × 2 }")
    expect(value("String(StringConcat(\"f is \", sin))")).toBe('"f is TensorSine"')
  })
  test("a live value a call makes is named by that call", () => {
    expect(getOrigin(run("x: $(5), x × 2"))?.source).toBe("x × 2")
    expect(getOrigin(run("x: $(5), sin(x)"))?.source).toBe("sin(x)")
  })
  test("a closure round-trips where the names it was made from are in scope", () => {
    expect(value("make: { n | { x | x + n } }, CodeEvaluate(String(make(3)))(1)")).toBe(4)
  })
  test("no function prints JavaScript", () => {
    for (const source of ["sin", "sum", "exp ⍣ -1", "⍨ -", "× ⍤ 1", "adam(0.01)", "grad(sin)", "guard(1, { 2 })", "try({ 1 })", "FunctionNoAutoLift(sin)", "cascade((sin, cos))"]) {
      expect(value(`String(${source})`)).not.toMatch(/=>|function|return|const /)
    }
  })

  test("a number is the shortest decimal that reads back as the same float32", () => {
    // regression: 65.4 printed as 65.4000015258789, its float32 widened to a double
    expect(value("String(65.4)")).toBe("65.4")
    expect(value("String([1 ÷ 3, 1e-7, -3e-8, 1e20, 16777217, 0 - 0])")).toBe("[0.33333334, 1e-7, -3e-8, 1e20, 1.6777216e7, 0]")
  })

  test("every float32 round-trips, from the smallest to the largest", () => {
    const xs = [...Array.from({ length: 2000 }, (_, i) => Math.fround((i % 2 ? -1 : 1) * Math.exp((i - 1000) / 25) * (1 + ((i * 7919) % 1000) / 1000))), 3.4028234663852886e38, 1.401298464324817e-45, 1.1754943508222875e-38]
    const literal = `[${xs.join(", ")}]`
    expect(value(`min(CodeEvaluate(String(${literal})) = ${literal})`)).toBe(1)
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
  // a program's scope sits under the prelude's, whose own keys are the prelude names; the
  // natives, widgets and sources are under that
  const prelude = Object.getPrototypeOf(createScope())
  const builtins = new Set([...Object.keys(prelude), ...Object.keys(DefaultEnvironment)])
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
  test("every take in shipped Fluent takes a count: a gather is x_i", () => {
    // take(x, n) is the first n items; it once took the rows at indices, so a gather left
    // spelled take(x, i) with one index would silently take the first i rows
    const counts: string[] = []
    const walk = (n: any) => {
      if (!n || typeof n !== "object") { return }
      if (Array.isArray(n)) { n.forEach(walk); return }
      if (n.type === "Operation" && ["take", "TensorTake"].includes(symbolOf(n.content?.operator) ?? "")) { counts.push(n.content.args.content.value[1]?.origin?.source) }
      for (const k of Object.keys(n)) { if (k !== "origin") { walk(n[k]) } }
    }
    for (const source of [PRELUDE, TOUR_SOURCE, ...Object.values(EXAMPLES)]) { walk(CodeParse(source)) }
    expect(counts.sort()).toEqual(["#b", "arrow", "dots", "Σ(k)"].sort())
  })

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
  const ring = (body: string) => `dot: dots([[80, 60]]),\nring: { p | ${body} },\ncanvas(160, 120) . draw(ring, [1, 0.8, 0.2], 4)`
  const rooms: [string, string[], string[]][] = [
    ["make this say 7", ["1 + 2*3"], ["7", "7 ; +*", "1 + 6 ; *", "1 + (2 * 3)"]],
    ["fish out the 30", ["[10, 20, 30] _ 2", "[10, 20, 30]_2"], ["30"]],
    ["turn this into", ["1 ... 5 * 2", "(1 ... 5) × 2"], ["[2, 4, 6, 8, 10]"]],
    ["waste of good bananas", ["🍌: 7, 🍌 * 🍌"], ["🍌: 7, 49"]],
    ["now sum 1 through 100", ["sum(1 ... 100)", "Σ(1 ... 100)", "TensorSum(1 ... 100)"], ["(5050)", "5050"]],
    ["now put plus in the middle", ["plus: add, 40 plus 2", "plus: add,\n40 plus 2 ; ← now put plus in the middle"],
      ["plus: add, 42", "plus: add, 40 + 2", "plus: add, plus(40, 2)"]],
    ["almost. a cube is", ["cube: { x | x^3 }, cube(3)", "cube: { x | x × x × x }, ; ← almost.\ncube(3) ; done"], ["cube: { x | x^2 }, 27"]],
    ["a ring: 40 away", [ring("abs(dot(p) - 40)"), ring("abs(40 - dot(p))")], [ring("abs(dot(p) - 0)"), ring("0"), "dot: dots([[80, 60]]), ring: { p | abs(dot(p) - 40) }, 5"]],
    ["the slope of x·x·x, at 2", ["f: { x | x^3 }, df: grad(f), df(2)", "f: { x | x^3 },\ndf: ∇(f),\ndf(2) ; ← now: the slope of x·x·x, at 2"],
      ["f: { x | x^2 }, df: grad(f), 12", "f: { x | x^3 }, df: { x | 3 × x^2 }, df(2)"]],
  ]
  for (const [seed, solutions, cheats] of rooms) {
    test(`“${seed}”`, () => {
      expect(solutions.map((src) => [src, lights(seed, src)])).toEqual(solutions.map((src) => [src, true]))
      expect(cheats.map((src) => [src, lights(seed, src)])).toEqual(cheats.map((src) => [src, false]))
    })
  }
  test("the drawing room lights at any density", () => {
    toHost({ t: "density", value: 2 })
    try {
      expect(lights("a ring: 40 away", ring("abs(dot(p) - 40)"))).toBe(true)
    } finally {
      toHost({ t: "density", value: 1 })
    }
  })
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
  // the chain the IDE lists, innermost first, each link with the document it points into
  const chain = (source: string): { message: string, user: boolean, line?: number }[] => {
    const links: Error[] = []
    for (let e: unknown = run(source); e instanceof Error; e = e.cause) { links.unshift(e) }
    return links.map((e) => ({ message: e.message, user: getOrigin(e)?.document === source, line: getOrigin(e)?.start.line }))
  }
  test("an Error in arithmetic is that Error, unchanged", () => {
    const scope = scopeOf(`e: Error("x"), r: e × 2 + 1`)
    expect(scope["r"]).toBe(scope["e"])
    expect(chain(`Error("x") × 2`)).toEqual([{ message: "x", user: true, line: 1 }])
  })
  test("a verb on items says what it takes, in the reader's words", () => {
    expect(chain("take(sin, 1)")).toEqual([{ message: "expected numbers, a list or a string, got a function", user: true, line: 1 }])
    expect(chain("reverse(◌)")[0]!.message).toBe("expected numbers, a list or a string, got ◌")
    // a member that takes the value and then fails is the answer: numbers can't join a string
    expect(chain(`[1, 2] concat "ab"`)[0]!.message).toBe("expected numbers, got a string")
    expect(chain("(1, 2) concat 3")[0]!.message).toContain("axis")
    // an unknown name is a mistake, not a value of another kind
    expect(chain("length(radious)")[0]!.message).toBe("unknown name: radious")
  })
  test("an Error made in a prelude body is reported at the reader's call that reached it", () => {
    // `-` is the prelude's x + (y × -1): its shape error points at the reader's line
    expect(chain("a: [1, 2],\nb: [1, 2, 3],\na - b")).toEqual([{ message: "elementwise: [2] vs [3]", user: true, line: 3 }])
    expect(chain("x: [[1, 2], [3, 4]],\nμ(x, 5)")).toEqual([{ message: "there is no axis 5: the value has 2 axes", user: true, line: 2 }])
    // one link, not a chain of the prelude's own frames
    expect(chain("softmax((1, 2, 3))")).toEqual([{ message: "shape: expected numbers, got a ( … ) list of 3", user: true, line: 1 }])
  })
  test("an Error the reader's own function makes keeps its line inside a prelude body", () => {
    // 𝕐 is the prelude's: the failing line is the reader's second one, not the call
    expect(chain("𝕐({ rec | { n |\n  n + [1, 2] } })([1, 2, 3])")).toEqual([{ message: "elementwise: [3] vs [2]", user: true, line: 2 }])
  })
  test("a cascade gives back the Error it was handed: the first failure is the cause", () => {
    expect(chain("length(radious + 1)")).toEqual([{ message: "unknown name: radious", user: true, line: 1 }])
    // a decline that reaches arithmetic is a failure, said in the decline's words
    expect(chain("cascade((guard(0, { 1 }),))(5) + 1").map((l) => l.message)).toEqual(["guard: the condition doesn't hold", "no overload matched (a number)"])
  })
  test("calling what isn't a function says what it is, in the tour's words", () => {
    expect(chain("sine(1)")).toEqual([{ message: "unknown name: sine", user: true, line: 1 }])
    expect(chain("x: [1, 2, 3], x[0]")).toEqual([{ message: "a list of 3 numbers is not a function", user: true, line: 1 }])
    // an Error in call position is that Error, not "'Error: …' is not a function"
    expect(chain("(double ⍣ 5)(1)").map((l) => l.message)).toEqual(["`(f ⍣ n)`: `f` must be a function, got the unknown name double"])
    expect(chain("x: [1, 2], W: ~[3, 4], loss: Σ(x × W), opt: adam(0.1), opt(loss)")[0]!.message).toContain("write it in braces")
    expect(chain("x: $(1), x(1, 2)")[0]!.message).toContain("a live value is read with s()")
  })
  test("a NoMatch keeps its own origin, a fresh one per read", () => {
    run("no: { x | NoMatch }, no(1)")
    const later = "say: { x | NoMatch },\n\nsay(2)"
    expect(getOrigin(run(later))?.document).toBe(later)
    expect(value("identical(NoMatch, NoMatch)")).toBe(0)
  })
  test("no JavaScript text reaches a message", () => {
    // writing a computed live value, binding over a getter
    expect(chain("a: $(1), b: a + 1, b(5)")[0]!.message).toBe("this live value is made from others – write the values it is made from")
    expect(value("NoMatch: 5, NoMatch")).toBe(5)
    for (const source of ["sine(1)", "x: [1, 2, 3], x[0]", "2 x 3", "f: x → x^2, f(3)", "(double ⍣ 5)(1)", "x: $(1), x(1, 2)", "a: $(1), b: a + 1, b(5)"]) {
      for (const { message } of chain(source)) { expect(message).not.toMatch(/Symbol\(|Attempted|'1,2,3'|Error: |poor signal/) }
    }
  })
  test("a program's names shadow the prelude's for the program, not inside the prelude's own functions", () => {
    expect(value("max: 10, softmax([0, 0])")).toEqual([0.5, 0.5])
    expect(value("floor: { x | 99 }, [1, 2, 3] scan +")).toEqual([1, 3, 6])
    expect(value("max: 10, max")).toBe(10)
  })
  test("a binding inside a function binds there, and its value is the function's", () => {
    expect(value("f: { x | y: x + 1, y × 2 }, f(1)")).toBe(4)
    expect(value("{ x | x: x × 2 }(3)")).toBe(6)
  })
  test("a cascade that matches nothing names the arguments and the reason", () => {
    const v = run(`cascade((guard(1 = 2, { 7 }),))(5)`) as Error
    expect(v).toBeInstanceOf(Error)
    expect(v.message).toContain("no overload matched")
    expect(v.message).toContain("a number")
    expect(v).toBeInstanceOf(NoMatch)
    expect((v.cause as Error).message).toContain("condition doesn't hold")
  })
  test("a failing application inside a cell points at the cell's own expression", () => {
    const v = unwrap(scopeOf(`x: $(0.5), code: $("x ⍴ [2, 2]"), r: CodeEvaluate(code), r`)["r"])
    expect(v).toBeInstanceOf(Error)
    expect(v.message).toContain("reshape")
    expect(getOrigin(v)).toMatchObject({ source: "x ⍴ [2, 2]", document: "x ⍴ [2, 2]" })
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

describe("pictures", () => {
  test("a shape that measures the whole picture at once is an Error, not an empty canvas", () => {
    // regression: norm(p - c) without its axis is one number for every pixel together, and
    // draw silently drew nothing
    const v = value("canvas(20, 20) . draw({ p | abs(norm(p - [10, 10]) - 6) }, [1, 1, 1], 2)")
    expect(v).toBeInstanceOf(Error)
    expect((v as Error).message).toContain("norm(p - c, -1)")
    expect(value("Σ(canvas(20, 20) . draw({ p | abs(norm(p - [10, 10], -1) - 6) }, [1, 1, 1], 2) ≠ canvas(20, 20))")).toBeGreaterThan(100)
  })
  test("draw is paint in a stroke ink: pixel for pixel the box overlap mixed in linear light", () => {
    // the overlap of a pixel and the stroke, written out
    const boxed = `{ shape, color, width | { picture | D: Density(), d: shape(pixels(picture)) × D, r: width × D ÷ 2,
      cover: unsqueeze(d+0.5 ⌊ r - (d-0.5 ⌈ (-r)) ⌈ 0 ⌊ 1, -1), picture^2.2 × 1-cover + color^2.2×cover ^ 1÷2.2 } }`
    const same = () => [["dots([[10.3, 20.7], [33.1, 40.9]])", "[1, 0, 1]", 7], ["line([[0, 3], [40, 47], [64, 9]])", "[10, 129, 239] ÷ 255", 2.5],
      ['letters("Ag", [4, 4], 18)', "1", 1]].map(([shape, color, width]) => {
      const at = `canvas(64, 48, 0.3) . `
      return (value(`boxed: ${boxed}, a: ${at}draw(${shape}, ${color}, ${width}), (a ≡ (${at}paint(${shape}, stroke(${color}, ${width}))), a ≡ (${at}boxed(${shape}, ${color}, ${width})))`) as any[]).map(unwrap)
    })
    expect(same()).toEqual([[1, 1], [1, 1], [1, 1]])
    toHost({ t: "density", value: 2 })
    try { expect(same()).toEqual([[1, 1], [1, 1], [1, 1]]) } finally { toHost({ t: "density", value: 1 }) }
  })
  test("an ink gives its light, its cover and how far it reaches; one above another is Porter–Duff over", () => {
    expect(value("stroke([1, 0.5, 0], 4)(0) ≡ ([1, 0.5^2.2, 0], 1, 3)")).toBe(1)
    // nothing past its reach: half the width and a pixel for a stroke, 3 radii for a halo
    expect((value("(stroke(1, 4)(2.4)_1 > 0, stroke(1, 4)(3)_1, halo(1, 5)(15)_0 < 2e-4, halo(1, 5)(0)_2, solid(1)(1)_1)") as any[]).map(unwrap)).toEqual([1, 0, 1, 15, 0])
    // light over light: b shows through as much as a leaves uncovered, and the reach is the farther
    expect(value("(stroke(1, 2) above ramp([[0, 0, 1], [0, 0, 1]]))(1) ≡ ([0.5, 0.5, 1], 1, 1÷0)")).toBe(1)
    expect(value("((stroke(1, 2) above halo(1, 1)) above solid(0))(0.5) ≡ (stroke(1, 2) above (halo(1, 1) above solid(0)))(0.5)")).toBe(1)
    // a halo adds light and hides nothing: far from it the picture is as it was
    expect((value("x: canvas(40, 10, 0.5) . paint(dots([[5, 5]]), halo(1, 2)), (x_5_5_0 > 1, abs(x_5_35_0 - 0.5) < 1e-6)") as any[]).map(unwrap)).toEqual([1, 1])
    toHost({ t: "density", value: 2 })
    try { expect(value("stroke(1, 4)(0)_2")).toBe(2.5) } finally { toHost({ t: "density", value: 1 }) }
  })
  test("ramp colors a number from 0 to 1 along its stops, blended in the colors given", () => {
    const stops = "[[0, 0, 0], [1, 0, 0], [1, 1, 1]]"
    // an ink is given each place's number on an axis of its own, [..., 1]
    expect(value(`ramp(${stops})([[-1], [0], [0.25], [0.5], [1], [2]])_0 ≡ [[0, 0, 0], [0, 0, 0], [0.5^2.2, 0, 0], [1, 0, 0], [1, 1, 1], [1, 1, 1]]`)).toBe(1)
    expect((value(`(ramp(${stops})(0.3)_1, ramp(${stops})(0.3)_2)`) as any[]).map(unwrap)).toEqual([1, Infinity])
    expect(value(`shape(canvas(8, 4) . paint({ p | Σ(p, -1) ÷ 12 }, ramp(${stops})))`)).toEqual([4, 8, 3])
  })
  test("line is segments joining each point to the next", () => {
    expect(value("pts: ⚄([9, 2]) × 40, p: pixels(canvas(50, 40)), max(abs(line(pts)(p) - segments(pts_(0..<8), pts_(1 ..< 9))(p)))")).toBe(0)
  })
  test("level makes any function a distance: |f| over its slope, first-order", () => {
    // exact for a straight line, close beside a circle
    expect(value("level({ p | Σ(p × [3, 4], -1) - 10 })([[2, 1], [0, 0]])")).toEqual([0, 2])
    const ring = value("level({ p | Σ(p^2, -1) - 400 })([[20.5, 0], [0, -21]])") as number[]
    expect(ring[0]).toBeCloseTo(0.5, 1)
    expect(ring[1]).toBeCloseTo(1, 1)
    // where f's slope falls to 0 with f – a double root – it is half the distance
    expect(value("level({ p | Σ(p, -1)^2 })([[1, 1]])_0 × 2")).toBeCloseTo(Math.SQRT2, 5)
    expect(value("shape(canvas(20, 10) . paint(level({ p | q: unstack(p, -1), q_1 - 5 - sin(q_0) }), stroke(1, 1)))")).toEqual([10, 20, 3])
  })
  test("noise: 0 on the grid, smooth, the same for the same place, in 2, 3 and 4 dimensions", () => {
    expect(value("noise([[3, 4], [-7, 0], [289, 12]])")).toEqual([0, 0, 0])
    expect(value("noise([[0, 0, 0], [5, -2, 9]])")).toEqual([0, 0])
    const n = value("noise(⚄([400, 2]) × 50 - 25)") as number[]
    expect(Math.max(...n.map(Math.abs))).toBeLessThan(1)
    expect(Math.max(...n.map(Math.abs))).toBeGreaterThan(0.3)
    expect(value("p: ⚄([50, 3]) × 20, max(abs(noise(p) - noise(p + 1e-3)))")).toBeLessThan(0.01)
    expect(value("p: [[0.3, 1.7, 2.2, 5.1]], noise(p) = noise(p)")).toEqual([1])
    // one octave of fbm is noise; two add a finer one at half strength
    expect(value("p: [[0.3, 1.7], [4.2, -3.5]], fbm(p, 2) - (noise(p) + (noise(2×p + 7.31) ÷ 2))")).toEqual([0, 0])
    expect(value("p: [[0.3, 1.7], [4.2, -3.5]], turbulence(p, 2) - (abs(noise(p)) + (abs(noise(2×p + 7.31)) ÷ 2))")).toEqual([0, 0])
  })
  test("∇ flows through noise: its slope is the finite difference's, in 2, 3 and 4 dimensions", () => {
    for (const p of ["[[0.3, 1.7], [5.5, -0.25], [-12.9, 3.3]]", "[[0.3, 1.7, 2.2], [5.5, 0.25, 9.1], [12.9, 3.3, 0.01]]", "[[0.3, 1.7, 2.2, 7.7]]"]) {
      const gap = value(`q: ${p}, e: 1e-3, n: shape(q)_1, I: 0..<n ⊗(=) 0..<n,
        dn: ∇({ x | Σ(noise(x)) })(q),
        fd: stack(ListMap(unstack(I), { u | noise(q + (u × e)) - noise(q - (u × e)) ÷ (2×e) }), -1),
        max(abs(dn - fd))`) as number
      expect([p, gap]).toEqual([p, expect.any(Number)])
      expect(gap).toBeLessThan(1e-3)
    }
  })
  test("a stroke alone has no pull from far away; a halo under it gives one", () => {
    // fit a wave's phase from 1.5 off: a stroke's ink changes only along its own edge, so a
    // stroke that misses the goal's line feels nothing, while a halo reaches across the gap
    const [stroke, halo] = value(`W: 64, H: 40,
      picture: { t, ink |
        wave: { p | q: unstack(p, -1), q_1 - H÷2 + (sin(q_0 × 2×π ÷ W + t) × H÷2.4) },
        canvas(W, H, 0) . paint(level(wave), ink)
      },
      fit: { ink |
        goal: picture(0.9, ink), t: ~(-0.6),
        𝓛: { mean((picture(t, ink) - goal)^2) }, opt: adam(0.03),
        ({ s | opt(𝓛) } ⍣ 150)(0), (𝓛(), t)
      },
      (fit(stroke(1, 3)), fit(stroke(1, 3) above halo(0.5, 4)))`).map((r: any[]) => r.map(unwrap)) as number[][]
    expect(stroke![0]).toBeGreaterThan(0.1)
    expect(Math.abs(stroke![1]! - 0.9)).toBeGreaterThan(1)
    expect(halo![0]).toBeLessThan(1e-5)
    expect(halo![1]).toBeCloseTo(0.9, 2)
  })
  test("viridis has no color for NaN, and the ends' colors past them", () => {
    expect(value("isNaN(viridis(0 ÷ 0))")).toEqual([1, 1, 1])
    expect(value("viridis([-1 ÷ 0, 2]) = viridis([0, 1])")).toEqual([[1, 1, 1], [1, 1, 1]])
  })
  test("heatmaps given one span share one scale", () => {
    expect(value("heat([[1, 2]], [0, 4]) = [[viridis(0.25), viridis(0.5)]]")).toEqual([[[1, 1, 1], [1, 1, 1]]])
  })
  test("maps stacked are small multiples on one scale, a NaN gap between", () => {
    const tiled = value("heat(stack(([[0, 1]], [[2, 3]])))") as number[][][]
    expect(tiled.length).toBe(1)
    expect(tiled[0]!.length).toBe(5)
    expect(value("heat(stack(([[0, 1]], [[2, 3]])))_0_0 = viridis(0)")).toEqual([1, 1, 1])
    expect(value("heat(stack(([[0, 1]], [[2, 3]])))_0_4 = viridis(1)")).toEqual([1, 1, 1])
    expect(value("isNaN(heat(stack(([[0, 1]], [[2, 3]])))_0_2)")).toEqual([1, 1, 1])
    expect(value("shape(heat(fill([2, 3, 4, 5], 1)))")).toEqual([9, 17, 3])
  })
  test("one rule says how a tensor shows: colors are an image, any other rank 2 or more a heatmap", () => {
    expect([[], [3], [2, 3], [2, 3, 3], [2, 3, 4], [2, 3, 2], [2, 3, 4, 5]].map(tensorView))
      .toEqual(["number", "bars", "heat", "image", "image", "heat", "heat"])
  })
})

// The host speaks to the page in plain data (protocol.ts): values cross as
// descriptions, live ones as ids the page holds, writes back to, and lets go of.
describe("the evaluation host", () => {
  const until = <T,>(found: () => T | undefined, what: string, timeout = 3000): Promise<T> => new Promise((resolve, reject) => {
    const t0 = Date.now()
    const poll = () => {
      const it = found()
      if (it !== undefined) { resolve(it); return }
      if (Date.now() - t0 > timeout) { reject(new Error(`no ${what} in ${timeout} ms`)); return }
      setTimeout(poll, 5)
    }
    poll()
  })
  // the first message of a kind since `start` – a batch of lives, one that has `id`
  const after = (start: number, kind: ToPage["t"], id?: number): Promise<any> =>
    until(() => inbox.slice(start).find((m) => m.t === kind && (id === undefined || (m.t === "lives" && m.lives.some((l) => l.id === id)))), `"${kind}" message`)
  const pause = (ms: number) => new Promise((r) => setTimeout(r, ms))
  const descOf = (batch: Extract<ToPage, { t: "lives" }>, id: number): any => batch.lives.find((l) => l.id === id)?.desc
  // the program's value, live 0
  const evaluate = async (source: string): Promise<any> => {
    const start = inbox.length
    toHost({ t: "code", source })
    return descOf(await after(start, "lives", 0), 0)
  }
  const numberIn = (d: any): number | undefined => d?.payload?.[0]
  let painted = 0
  const canvas = { width: 0, height: 0, getContext: () => ({ createImageData: (w: number, h: number) => ({ data: new Uint8ClampedArray(w * h * 4) }), putImageData: () => { painted++ } }) }

  test("it starts by listing the environment, with doc cards", () => {
    const ready = inbox.find((m) => m.t === "ready") as Extract<ToPage, { t: "ready" }>
    const sum = ready.names.find(([name]) => name === "sum")
    expect(sum?.[2]?.doc).toContain("Sum of the elements")
    // the Tour is built only when read, and listed all the same
    expect(ready.names.find(([name]) => name === "Tour")?.[2]?.doc).toContain("tour")
  })

  test("a program's value arrives described, with its values", async () => {
    const d = await evaluate("1 + 1")
    expect(d.k).toBe("tensor")
    expect(numberIn(d)).toBe(2)
  })

  test("a top-level assignment runs the program once, not again on its own write", async () => {
    for (const source of ["θ: ~[0, 0], θ:= [1, 2], θ", "D: ~~[1], D:= [3, 4], D"]) {
      const start = inbox.length
      await evaluate(source)
      await pause(200)
      expect(inbox.slice(start).filter((m) => m.t === "lives" && m.lives.some((l) => l.id === 0)).length).toBe(1)
    }
  })

  test("a picture paints until the page lets go of it, however often its value moves on", async () => {
    const d = await evaluate(`x: $(0), (Button("go", { x(x() + 1) }), $({ fill([5, 5], x()) }))`)
    const go = d.items[0].slots.onClick.id, live = d.items[1].id
    toHost({ t: "canvas", slot: 1, canvas: canvas as unknown as OffscreenCanvas })
    let start = inbox.length
    toHost({ t: "call", id: go })
    const first = descOf(await after(start, "lives", live), live)
    expect(first.paint).toBe(true)
    // the next picture is on its way, and the page still shows the first
    const read = hold("reads")
    start = inbox.length
    toHost({ t: "call", id: go })
    await after(start, "lives", live)
    painted = 0
    toHost({ t: "paint", slot: 1, id: first.id, size: [0, 0] })
    await pause(50)
    expect(painted).toBe(1)
    // the page reads the next, and lets go of the first
    read()
    await pause(10)
    toHost({ t: "paint", slot: 1, id: first.id, size: [0, 0] })
    await pause(50)
    expect(painted).toBe(1)
  })

  // What the painter puts on a canvas for a described picture, at a size in device pixels:
  // [r, g, b, a] per pixel, row by row
  let slots = 100
  const paintOf = async (d: any, size: [number, number] = [0, 0]): Promise<{ width: number, height: number, px: (i: number) => number[], at: (x: number, y: number) => number[], cell: (r: number, c: number, dx?: number) => number[], k: number }> => {
    let image: { width: number, height: number, data: Uint8ClampedArray } | null = null
    const canvas = { width: 0, height: 0, getContext: () => ({ createImageData: (w: number, h: number) => ({ width: w, height: h, data: new Uint8ClampedArray(w * h * 4) }), putImageData: (i: typeof image) => { image = i } }) }
    const slot = ++slots
    toHost({ t: "canvas", slot, canvas: canvas as unknown as OffscreenCanvas })
    toHost({ t: "paint", slot, id: d.id, size })
    for (let i = 0; i < 400 && !image; i++) { await pause(5) }
    toHost({ t: "canvasGone", slot })
    const { width, height, data } = image!
    const at = (x: number, y: number) => [...data.subarray(4 * (Math.floor(y) * width + Math.floor(x)), 4 * (Math.floor(y) * width + Math.floor(x)) + 4)]
    // a map's cell (r, c), at its center or a way from it: a heatmap comes magnified, `size` its pixels
    const k = d.size && d.shape.length === 2 ? d.size[0] / d.shape[1] : 1
    return { width, height, px: (i) => [...data.subarray(4 * i, 4 * i + 4)], at, cell: (r: number, c: number, dx = 0) => at((c + 0.5) * k + dx, (r + 0.5) * k), k }
  }

  test("one ∞ doesn't blank a bar chart: the range is the finite values'", async () => {
    // regression: ∞ set the range, and every bar, the ∞ one too, came out 0 pixels high
    const d = await evaluate("x: 0 ..< 30, x + 1 ÷ (29 - x)")
    expect([...d.payload].map((v: number) => +v.toFixed(3))).toEqual([0.034, 29])
    const { width, height, px } = await paintOf(d, [30, 20])
    const inked = Array.from({ length: width }, (_, x) => Array.from({ length: height }, (_, y) => px(y * width + x)).filter(([r, g, b, a]) => a && !(r === 68 && g === 68 && b === 68)).length)
    expect(inked.reduce((a, b) => a + b)).toBeGreaterThan(50)
    expect(inked.at(-2)).toBe(height - 1)
    expect(inked.at(-1)).toBe(height - 1)
  })

  test("one ∞ doesn't flatten a heatmap, a NaN is a hole, and ±∞ carry a ring", async () => {
    // regression: ∞ set the range, so every finite cell got the minimum's color, and so
    // did the ∞ and the NaN
    const d = await evaluate("x: 0 ..< 25, where(x = 24, 1 ÷ 0, where(x = 6, -1 ÷ 0, where(x = 12, 0 ÷ 0, x))) ⍴ [5, 5]")
    expect([...d.payload]).toEqual([0, 23])
    expect(d.size).toEqual([5 * 52, 5 * 52])
    const { cell, k } = await paintOf(d)
    const at = (i: number, dx = 0) => cell(Math.floor(i / 5), i % 5, dx)
    expect(at(0)).toEqual([68, 1, 84, 255])
    expect(at(23)).toEqual([253, 231, 37, 255])
    expect(at(24)).toEqual([253, 231, 37, 255])
    expect(at(12)[3]).toBe(0)
    expect(new Set(Array.from({ length: 25 }, (_, i) => String(at(i)))).size).toBe(23)
    // a ring a third of the way out: dark on ∞'s yellow, light on -∞'s purple, none on 23
    expect(at(24, 0.3 * k)).toEqual([23, 23, 23, 255])
    expect(at(6, 0.3 * k)).toEqual([242, 242, 242, 255])
    expect(at(23, 0.3 * k)).toEqual([253, 231, 37, 255])
  })

  test("a heatmap of tiny values shows them apart", async () => {
    // regression: the range was at least 1e-6 wide, so values closer than that took one color
    const { cell } = await paintOf(await evaluate("[[0, 1e-8], [2e-8, 3e-8]]"))
    expect(cell(0, 0)).toEqual([68, 1, 84, 255])
    expect(cell(1, 1)).toEqual([253, 231, 37, 255])
  })

  test("a few values cross with their picture, for its labels", async () => {
    const d = await evaluate("[3, 1, 0 ÷ 0]")
    expect(d.paint).toBe(true)
    expect([...d.payload]).toEqual([3, 1, NaN])
    const { width, px } = await paintOf(d, [30, 10])
    expect(px(width - 1)[3]).toBe(0)
  })

  test("the IDE paints a vector's bars as bars(v, width, height) draws them", async () => {
    // the IDE draws through a longer window, into a larger picture, so the plan outlives a
    // growing vector and a resized canvas: what shows is the same
    const source = "sin(0 ..< 300 ÷ 9) × (0 ..< 300) - 20"
    const { width, height, px } = await paintOf(await evaluate(source), [123, 45])
    const drawn = value(`bars(${source}, 123, 45)`) as number[][][]
    expect([width, height]).toEqual([123, 45])
    const differ = drawn.flat().filter((c, i) => String(c.some((x) => x !== x) ? [0, 0, 0, 0] : [...c.map((x) => Math.round(x * 255)), 255]) !== String(px(i))).length
    expect(differ).toBe(0)
  })

  test("a program's own heat doesn't change how its values show", async () => {
    const { cell } = await paintOf(await evaluate("heat: { x | 0 }, [[1, 2], [3, 4]]"))
    expect(cell(0, 0)).toEqual([68, 1, 84, 255])
  })

  test("a tensor of rank 3 or more shows as small multiples on one scale", async () => {
    // regression: rank 3 showed its first channel as an image, clipped, and rank 4 no picture
    const d = await evaluate("reshape(0 ..< 30, [2, 3, 5])")
    expect([d.paint, [...d.payload]]).toEqual([true, [0, 29]])
    // two 3 × 5 maps across, a gap between: 3 × 11 cells, 24 pixels each
    expect(d.size).toEqual([11 * 24, 3 * 24])
    const { at } = await paintOf(d)
    const cell = (r: number, c: number) => at((c + 0.5) * 24, (r + 0.5) * 24)
    expect(cell(0, 0)).toEqual([68, 1, 84, 255])
    expect(cell(1, 5)[3]).toBe(0)
    expect(cell(2, 10)).toEqual([253, 231, 37, 255])
    const four = await evaluate("reshape(0 ..< 16, [2, 2, 2, 2])")
    expect([four.paint, [...four.payload], four.size]).toEqual([true, [0, 15], [5 * 52, 5 * 52]])
  })

  test("[h, w, 3] and [h, w, 4] are images, with the range of their values", async () => {
    const d = await evaluate("reshape(0 ..< 12, [2, 2, 3]) ÷ 22 - 0.25")
    expect([...d.payload]).toEqual([-0.25, 0.25])
    const rgba = await evaluate("fill([2, 2, 4], 0.5)")
    const { px } = await paintOf(rgba)
    expect(px(0)).toEqual([128, 128, 128, 128])
  })

  test("a bar past the scale ends in a caret", async () => {
    const d = await evaluate("x: 0 ..< 24, where(x = 5, 1 ÷ 0, x)")
    const { at } = await paintOf(d, [240, 60])
    // the ∞ bar's center is at 54.5: its caret's tip is one row down from the top
    expect(at(54, 1)).toEqual([23, 23, 23, 255])
    expect(at(54, 6)).toEqual([253, 231, 37, 255])
  })

  // The page tells the worker its density (devicePixelRatio): pictures are drawn in units, a
  // CSS pixel each, with the display's own pixels
  const atDensity = async <T,>(d: number, run: () => Promise<T> | T): Promise<T> => {
    toHost({ t: "density", value: d })
    try { return await run() } finally { toHost({ t: "density", value: 1 }); await pause(20) }
  }
  test("a picture is drawn in units, with the display's pixels", () => atDensity(2, () => {
    expect(value("Density()()")).toBe(2)
    expect(value("shape(canvas(520, 160))")).toEqual([320, 1040, 3])
    expect(value("pixels(canvas(1, 1))")).toEqual([[[0.25, 0.25], [0.75, 0.25]], [[0.25, 0.75], [0.75, 0.75]]])
    // a line 1 unit wide covers two whole pixels, its edges one pixel of the display
    expect(value("transpose((canvas(4, 1, 0) . draw(line([[2, -9], [2, 9]]), 1, 1))_0)_0")).toEqual([0, 0, 0, 1, 1, 0, 0, 0])
    expect(value("shape(bars([3, -1, 2], 30, 20))")).toEqual([40, 60, 3])
    expect(value("shape(plot(0..<3, [1, 2, 3]))")).toEqual([320, 1040, 3])
  }))
  test("text is sized in units: about as much ink a unit at any density", async () => {
    const ink = () => value(`Σ((canvas(120, 30, 0) . draw(letters("Ag 2.31", [2, 2], 18), 1, 1))^2.2) ÷ 3 ÷ Density()^2`) as number
    const one = ink(), two = await atDensity(2, ink)
    expect(Math.abs(two - one) / one).toBeLessThan(0.06)
  })
  test("a drawing follows the display's density, and the IDE paints heatmaps and bars at it", () => atDensity(1, async () => {
    const d = await evaluate("canvas(4, 2) . draw(dots([[1, 1]]), 1, 1)")
    expect([d.k, d.now.shape]).toEqual(["live", [2, 4, 3]])
    const start = inbox.length
    toHost({ t: "density", value: 2 })
    expect(descOf(await after(start, "lives", d.id), d.id).shape).toEqual([4, 8, 3])
    // a heatmap's size is in units, and its picture has the display's pixels
    const heat = await evaluate("[[0, 1], [2, 3]] tile [4, 4]")
    expect(heat.size).toEqual([256, 256])
    expect((await paintOf(heat)).width).toBe(512)
    // bars fill a box the page gives in units
    expect(await paintOf(await evaluate("sin(0 ..< 300)"), [123, 45]).then(({ width, height }) => [width, height])).toEqual([246, 90])
  }))

  test("Fetch of a missing file is an Error, not the server's 404 page", async () => {
    const served = Bun.serve({ port: 0, fetch: () => new Response("Not Found", { status: 404 }) })
    extendEnvironment({ Fetch: hostFetch } as Record<string, Value>)
    const d = await evaluate(`Fetch("http://localhost:${served.port}/nope.txt")`)
    // what the page shows of it, once the fetch has failed
    const shown = (): any => mirrors.get(d.id)?.desc.peek()
    const desc = await until(() => shown()?.k === "error" ? shown() : undefined, "Error")
    served.stop()
    extendEnvironment({ Fetch: loading.Fetch } as Record<string, Value>)
    expect(desc).toMatchObject({ k: "error" })
    expect(desc.chain.at(-1).message).toContain("404")
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
  })

  test("a write to a computed value is answered, and the editor says where it can't be made", async () => {
    const d = await evaluate("x: $(5), Slider(x × 2)")
    const target = d.slots.target
    const start = inbox.length
    toHost({ t: "set", id: target.id, value: 3, seq: 1 })
    const batch = await after(start, "lives", target.id)
    expect(batch.lives.find((l: any) => l.id === target.id).ack).toBe(1)
    expect(numberIn(descOf(batch, target.id))).toBe(10)
    const { warnings } = await after(start, "warnings")
    expect(warnings).toMatchObject([{ o: target.o }])
  })

  test("a source's frame is answered as it is taken: the page sends the next after", async () => {
    const start = inbox.length
    await evaluate("MousePosition()")
    const { id } = await after(start, "source")
    toHost({ t: "frame", id, samples: Float32Array.of(0.25, 0.75) })
    expect(await after(start, "took")).toMatchObject({ id })
  })

  test("an Error of a statement before the last is marked where the statement is", async () => {
    let start = inbox.length
    expect(numberIn(await evaluate("f(x): x^2, 1 + 1"))).toBe(2)
    const { warnings } = await after(start, "warnings")
    expect(warnings).toHaveLength(1)
    expect(warnings[0].o).toMatchObject({ s: [1, 1], e: [1, 10] })
    // the last statement's Error is the value; the next run starts over
    start = inbox.length
    await evaluate("f(x): x^2")
    expect((await after(start, "warnings")).warnings).toEqual([])
  })

  test("a program runs once, and what it shows of a variable follows it", async () => {
    // a top-level read and then a write of the same cell, as an optimizer's step does
    for (const source of ["s: $0, s(s() + 1), s", "θ: ~[0], opt: sgd(0.1), opt({ Σ((θ - 1)^2) }), θ"]) {
      const start = inbox.length
      await evaluate(source)
      await pause(200)
      expect(inbox.slice(start).filter((m) => m.t === "lives" && m.lives.some((l) => l.id === 0)).length).toBe(1)
    }
    let start = inbox.length
    const d = await evaluate("(θ: ~1, j: ${ θ × 2 }, j, θ:= 5, j)")
    expect(numberIn(d.items[1].now)).toBe(10)
    const go = await evaluate(`θ: ~[1], (Button("go", { θ:= θ + 1 }), \${ θ × 2 })`)
    start = inbox.length
    toHost({ t: "call", id: go.items[0].slots.onClick.id })
    const { lives } = await after(start, "lives")
    expect(lives.find((l: any) => l.id === go.items[1].id).desc.payload[0]).toBe(4)
    toHost({ t: "drawn" })
  })

  test("the next batch waits until the page has drawn the last", async () => {
    const d = await evaluate("x: $(0), (Slider(x), x)")
    const id = d.items[0].slots.target.id
    const draw = hold("frames")
    let start = inbox.length
    toHost({ t: "set", id, value: 0.1, seq: 1 })
    await after(start, "lives")
    start = inbox.length
    toHost({ t: "set", id, value: 0.2, seq: 2 })
    await pause(100)
    expect(inbox.slice(start).some((m) => m.t === "lives")).toBe(false)
    draw()
    const batch = await after(start, "lives", id)
    expect(batch.lives.find((l: any) => l.id === id).ack).toBe(2)
    expect(numberIn(descOf(batch, id))).toBeCloseTo(0.2)
  })

  test("a change made while the page draws goes when the page has drawn, not a frame later", async () => {
    const d = await evaluate(`x: $(0), (Button("go", { x(x() + 1) }), x)`)
    const go = d.items[0].slots.onClick.id, x = d.items[1].id
    const draw = hold("frames")
    let start = inbox.length
    toHost({ t: "call", id: go })
    await after(start, "lives", x)
    // no display frame comes: the page's answer alone lets the next batch go
    const frames: ((t: number) => void)[] = []
    ;(globalThis as any).requestAnimationFrame = (f: (t: number) => void) => frames.push(f)
    try {
      start = inbox.length
      toHost({ t: "call", id: go })
      await pause(20)
      draw()
      expect(numberIn(descOf(await after(start, "lives", x), x))).toBe(2)
    } finally {
      delete (globalThis as any).requestAnimationFrame
      frames.forEach((f) => f(performance.now()))
    }
  })

  // A look that takes 2 ms, after a loop's statement – none, or one with steps always
  // waiting – and a frame every so many ms, on a clock moved by hand: of 20 frames, how
  // many show it
  const costlyLooks = async (loop: string, spacing: number): Promise<number> => {
    let now = performance.now()
    const clock = performance.now
    const spin = (v: Value) => { now += 2; return v }
    extendEnvironment({ TestSpin: spin } as Record<string, Value>)
    const d = await evaluate(`x: $(0), ${loop}(Button("go", { x(x() + 1) }), \${ TestSpin(x()) })`)
    const go = d.items[0].slots.onClick.id, y = d.items[1].id
    const frames: ((t: number) => void)[] = []
    ;(globalThis as any).requestAnimationFrame = (f: (t: number) => void) => frames.push(f)
    performance.now = () => now
    try {
      const start = inbox.length
      for (let i = 0; i < 20; i++) {
        toHost({ t: "call", id: go })
        await pause(1)
        now += spacing
        frames.splice(0).forEach((f) => f(now))
      }
      await pause(10)
      return inbox.slice(start).filter((m) => m.t === "lives" && m.lives.some((l) => l.id === y)).length
    } finally {
      performance.now = clock
      delete (globalThis as any).requestAnimationFrame
      frames.forEach((f) => f(performance.now()))
      await evaluate("0")
    }
  }
  test("while steps wait, a look that costs a little over a twentieth of a frame is shown nearly every frame, not every other", async () => {
    // a frame every 38 ms: a twentieth of the worker is 1.9 ms a frame
    expect(await costlyLooks("{ 0 } ⟳ 1000000, ", 36)).toBeGreaterThanOrEqual(16)
  })
  test("with no steps waiting, a look is shown every frame, whatever it costs", async () => {
    expect(await costlyLooks("", 8)).toBe(20)
  })

  test("a run waits for the page as any change does: one batch at most is on its way", async () => {
    const d = await evaluate(`x: $(0), (Button("go", { x(x() + 1) }), x)`)
    const go = d.items[0].slots.onClick.id
    const draw = hold("frames")
    let start = inbox.length
    toHost({ t: "call", id: go })
    await after(start, "lives")
    // the author edits while the page draws
    start = inbox.length
    toHost({ t: "code", source: "2 + 2" })
    await pause(100)
    expect(inbox.slice(start).some((m) => m.t === "lives")).toBe(false)
    draw()
    expect(numberIn(descOf(await after(start, "lives", 0), 0))).toBe(4)
  })

  test("a drop from before a run can't let go of what the run holds", async () => {
    // a built-in keeps its id from run to run – here, the button's action
    const source = `s: $(Button("go", SampleRate)), (Slider(s), s)`
    const d = await evaluate(source)
    const s = d.items[0].slots.target.id, go = d.items[1].now.slots.onClick.id
    const read = hold("reads")
    let start = inbox.length
    // s no longer holds the button: the page will let go of it when it reads that
    toHost({ t: "set", id: s, value: 1, seq: 1 })
    await after(start, "lives", s)
    toHost({ t: "drawn" })
    // the next run's value names the same action, before the page has let go
    start = inbox.length
    toHost({ t: "code", source })
    expect(descOf(await after(start, "lives", 0), 0).items[1].now.slots.onClick.id).toBe(go)
    read()
    await pause(10)
    start = inbox.length
    toHost({ t: "call", id: go })
    await pause(10)
    expect(inbox.slice(start).some((m) => m.t === "needSampleRate")).toBe(true)
  })

  test("a live value the page let go of stops sending", async () => {
    const d = await evaluate(`x: $(0), (Button("go", { x(x() + 1) }), x)`)
    const [button, x] = d.items
    const go = button.slots.onClick.id
    let start = inbox.length
    toHost({ t: "call", id: go })
    expect(numberIn((await after(start, "lives")).lives[0].desc)).toBe(1)
    toHost({ t: "drop", id: x.id, count: 1 })
    start = inbox.length
    toHost({ t: "call", id: go })
    await pause(100)
    expect(inbox.slice(start).some((m) => m.t === "lives")).toBe(false)
  })

  test("the page sends another worker nothing about the ids the last one named", async () => {
    const d = await evaluate(`x: $(0), (Button("go", { x(x() + 1) }), Slider(x))`)
    const go = d.items[0].slots.onClick.id, x = d.items[1].slots.target.id
    const sent: ToWorker[] = []
    connect((message) => { sent.push(message) }, drawn)
    call(go)
    write(x, 1)
    connect(toWorker, drawn)
    expect(sent).toEqual([])
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
  // A demo runs once the last one's loop has finished its step: on the CPU, a model's
  // first step takes seconds
  for (const [name, src] of [...Object.entries(EXAMPLES), ["Tour", "Tour"]] as [string, string][]) {
    test(`${name} crosses to the page whole`, async () => {
      const d = await evaluate(src.trim())
      expect(() => structuredClone(d)).not.toThrow()
      expect(errorsIn(d)).toEqual([])
    }, 60_000)
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
  test("window is a view of the same numbers, over the trailing axes", () => {
    expect(value("window([1, 2, 3, 4, 5], [3])")).toEqual([[1, 2, 3], [2, 3, 4], [3, 4, 5]])
    expect(value("window([[1, 2, 3], [4, 5, 6]], [2])")).toEqual([[[1, 2], [2, 3]], [[4, 5], [5, 6]]])
    expect(value("window([[1, 2, 3], [4, 5, 6]], [2, 2])")).toEqual([[[[1, 2], [4, 5]], [[2, 3], [5, 6]]]])
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
    // along the leading axis, as reduce folds: the scan's last item is the reduction, at any rank
    expect(value("[[1, 2, 3], [4, 5, 6]] scan +")).toEqual([[1, 2, 3], [5, 7, 9]])
    for (const f of ["+", "⌈", "×"]) {
      for (const x of ["[3, 1, 2]", "[[1, 5, 3], [4, 2, 6]]", "fill([2, 3, 2], 2) - (0..<2)"]) {
        expect(value(`x: ${x}, (x scan ${f})_(-1) ≡ (x reduce ${f})`)).toBe(1)
      }
    }
    expect(value("∇({ x | Σ((x scan +) × [1, 10, 100]) })([1, 2, 3])")).toEqual([111, 110, 100])
  })
  test("pipe, apply and zerosLike", () => {
    expect(value("(1, 2) pipe add")).toBe(3)
    expect(value("add apply (1, 2)")).toBe(3)
    expect(value("zerosLike([1, 2])")).toEqual([0, 0])
  })
  test("the page writes numbers as Fluent reads them, and a label never misleads", () => {
    // regression: the page grouped digits with _, which is Fluent's subscript (1_000 is 1),
    // printed a float32 65.4 as 65.400002, and 1e-7 as 0
    const f = Math.fround
    expect(numberText(1234567)).toBe("1234567")
    expect(numberText(f(65.4))).toBe("65.4")
    expect(numberText(f(1e-7))).toBe("1e-7")
    expect(numberText(f(-3e-8), 3)).toBe("-3e-8")
    expect(numberText(f(Math.sin(f(Math.PI))), 3)).toBe("-8.74e-8")
    expect(numberText(-0)).toBe("0")
    expect(numberText(f(1 / 3), 3)).toBe("0.333")
    // a label keeps every digit of the whole part: 1234568, not a made-up 1230000
    expect(numberText(1234567.8, 3)).toBe("1234568")
    expect(numberText(f(1e20), 3)).toBe("1e20")
    expect(numberText(0.1 + 0.2)).toBe("0.30000000000000004")
    expect([Infinity, -Infinity, NaN].map((n) => numberText(n))).toEqual(["∞", "-∞", "NaN"])
  })
  test("a literal is exactly its numbers", () => {
    const n = 8192
    const got = value(`[${Array.from({ length: n }, (_, i) => i).join(", ")}]`) as number[]
    expect(got.every((v, i) => v === i)).toBe(true)
  }, 30_000)
})

describe("plot: lines on one pair of axes", () => {
  test("a number as the codes of its digits, worked out without reading it back", () => {
    const shown = (x: number | string) => value(`CodesToString(DrawDigits(${x}))`).trimEnd()
    expect([2.31, 0.052, 1200, -0.5, 9.996, 0.99999, 0.00123, 0, 3, 38.44].map(shown)).toEqual(["2.31", "0.0520", "1200", "−0.500", "10.0", "1.00", "0.00123", "0", "3", "38.4"])
    // with an exponent from 10 million up and below 0.001; NaN, ∞ and −∞ as such
    expect(["12345678", "9999999", "-1.5e9", "3.3e38", "0.00014", "-4.2e-11", "2e-37"].map(shown)).toEqual(["1.23e7", "1.00e7", "−1.50e9", "3.30e38", "1.40e−4", "−4.20e−11", "2.00e−37"])
    expect(["0 ÷ 0", "1 ÷ 0", "-1 ÷ 0"].map(shown)).toEqual(["NaN", "∞", "−∞"])
    // numbers give a row of codes each
    expect(value(`ListMap(unstack(DrawDigits([1, -2])), CodesToString)`)).toEqual(["1        ", "−2       "])
    expect(value(`shape(DrawDigits([[1, -2]]))`)).toEqual([1, 2, 9])
  })
  test("letters draw the IDE's font, as much of it as each glyph covers", () => {
    // a glyph's coverage, summed in linear light, and the glyph's area: BQN386's W covers
    // 0.177 of a square of the font's size, measured on Chrome's render of it at 240 pixels
    const ink = (text: string, size: number, at = "[0, 0]") => value(`Σ((canvas(${size * text.length}, ${size * 2}, 0) . draw(letters("${text}", ${at}, ${size}), 1, 1))^2.2) ÷ 3`) as number
    expect(ink("W", 24) / (0.177 * 24 * 24)).toBeCloseTo(1, 1)
    // as much ink wherever a text falls among the pixels
    expect(ink("held out 2.31", 12, "[0.3, 0.7]") / ink("held out 2.31", 12)).toBeCloseTo(1, 1)
    // a space, and a character the font lacks, draw nothing
    expect(ink(" é", 12)).toBe(0)
    // the field holds a 10 × 20 cell for each of the font's codes
    expect(value(`length(DrawFontField) ÷ length(DrawFontCodes) = 200`)).toBe(1)
  })
  test("several texts make one shape, each from its own corner, a character 0.6 of the size across", () => {
    const apart = (a: string, b: string) => value(`max(abs(letters(${a})(pixels(canvas(30, 12))) ⌊ 9 - (letters(${b})(pixels(canvas(30, 12))) ⌊ 9)))`) as number
    expect(apart(`"1−∞", [0, 0], 10`, `("1", [8722], "∞"), [[0, 0], [6, 0], [12, 0]], 10`)).toBeLessThan(1e-4)
    // lowercase is a glyph of its own
    expect(apart(`"a", [0, 0], 10`, `"A", [0, 0], 10`)).toBeGreaterThan(1)
  })
  test("each series draws in its own color, and a series of fewer than 2 points draws nothing", () => {
    // how many pixels of the first two series colors the picture has – at density 1, a line
    // 1.25 units wide covers few pixels in full and the rest in part
    const count = (p: string) => value(`p: ${p}, near: { c | Σ(Σ(abs(p - c), -1) < 0.05) }, stack((near([0.224, 0.529, 0.898]), near([0.851, 0.349, 0.149])))`) as number[]
    const [blue, orange] = count(`plot(0..<50, (0..<50, 50 - 0..<50), ("up", "down"))`)
    expect(blue).toBeGreaterThan(100)
    expect(orange).toBeGreaterThan(100)
    expect(count(`plot(0..<1, [3])`)).toEqual([0, 0])
    expect(value(`shape(plot(linspace([-1, 1], 9), [[1, 2, 3, 4, 5, 6, 7, 8, 9]]))`)).toEqual([160, 520, 3])
  })
  test("a series denser than the picture's columns draws at the columns' cost, and keeps its extremes", () => {
    // 20,000 points at density 2: drawn through every point, the picture needs 53 GB at once.
    // The one point at 1 is a spike from the bottom of the box to its top, in blue, at the
    // picture's pixel column 551 – the saw, in orange, stays in the lower half
    toHost({ t: "density", value: 2 })
    try {
      const [h, w, spike] = value(`p: plot(0..<20000, (where(0..<20000 = 12345, 1, 0), 0..<20000 % 7 ÷ 12), ("spike", "saw")),
        blue: Σ(abs(p - [0.224, 0.529, 0.898]), -1) < 0.05,
        concat((take(shape(p), 2), [Σ(slice(blue, [20, 540], [120, 20]))]))`)
      expect([h, w]).toEqual([320, 1040])
      expect(spike).toBeGreaterThan(200)
    } finally {
      toHost({ t: "density", value: 1 })
    }
  }, 30_000)
  test("the line through a column's first, top, bottom and last points looks the same as the line through all of them", () => {
    // the largest difference, the mean, and the share past 0.1, of a white line on black
    const apart = (x: string, y: string) => value(`x: ${x}, y: ${y}, u: x ÷ x_(-1) × 50 + 5, v: y × 20 + 5,
      full: canvas(60, 30, 0) . draw(line(stack((u, v), -1)), 1, 1.25),
      columns: canvas(60, 30, 0) . draw(DrawTrace(u, v, 5, 55, 1.25), 1, 1.25),
      d: abs(full - columns), [max(d), Σ(d) ÷ (#flat(d)), Σ(d > 0.1) ÷ (#flat(d))]`) as number[]
    // at most a point a column: the same line, cut where it crosses from one column to the next
    expect(apart("0..<40", "sin(x)")[0]).toBeLessThan(1e-4)
    // every point in one column, and no point in the columns after it
    expect(value(`canvas(60, 30, 0) . draw(DrawTrace([30, 30, 30], [5, 25, 15], 5, 55, 1.25), 1, 1.25)
      - (canvas(60, 30, 0) . draw(line([[30, 5], [30, 25], [30, 15]]), 1, 1.25)) . abs . max`)).toBeLessThan(1e-4)
    // noise, 6 points a column: a few pixels at the edges differ
    const [most, mean, past] = apart("0..<600", "sin(x × 12.9898 + 1) × 43758.5453 % 1")
    expect(most).toBeLessThan(0.4)
    expect(mean).toBeLessThan(0.006)
    expect(past).toBeLessThan(0.01)
  }, 30_000)
  test("a dense series keeps its range frame and its last value exactly", () => {
    // left of the box, the range's ends; right of it, the names and last values: the same as
    // for four points with the same first, smallest, largest and last values
    expect(value(`y: sin(0..<3000 × 12.9898 + 1) × 43758.5453 % 1 - (0..<3000 ÷ 3000),
      dense: plot(0..<3000, (y, 0..<3000 % 5), ("noise", "saw")),
      four: plot([0, 1, 2, 2999], ([y_0, min(y), max(y), y_(-1)], [0, 0, 4, 4]), ("noise", "saw")),
      apart: { p, q | max(abs(p - q)) },
      [apart(slice(dense, [0, 0], [-1, 61]), slice(four, [0, 0], [-1, 61])), apart(slice(dense, [0, 412]), slice(four, [0, 412]))]`)).toEqual([0, 0])
  }, 30_000)
})

describe("the engine", () => {
  test("a computation past the memory budget is an Error, not an exhausted machine", () => {
    const big = run("x: fill([1000, 1000], 1), (x ⍴ [1000, 1000, 1, 1]) + (x ⍴ [1, 1, 1000, 1000])") as engine.Tensor
    expect(() => big.readSync()).toThrow(/GB of memory at once/)
  })
  test("an op carries the site of the reader's call that built it, also inside the prelude", () => {
    const source = "a: [1, 2, 3],\nb: a + 1,\nc: a - 1,\n(b, c)"
    const [b, c] = run(source) as engine.Tensor[]
    expect(b!.node.site).toMatchObject({ source: "a + 1", start: { line: 2 } })
    // `-` builds its ops in the prelude: they still come from the reader's line 3
    expect(c!.node.site).toMatchObject({ source: "a - 1", start: { line: 3 } })
  })
  test("an engine error raised at a read names the line that built the op", () => {
    // the op is lazy: the read on line 3 forces it, but line 2 made it
    const source = "a: TensorBroadcast(0 ..< 30000, [30000, 30000]),\nb: a + 1,\nString(b)"
    const e = run(source) as Error
    expect(e.message).toContain("line 2 «a + 1»")
    expect(getOrigin(e)).toMatchObject({ source: "a + 1", document: source })
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
  test("a contraction several kernels read is computed once", async () => {
    // an RMS norm of an embedding plus positions: its variance and its scaled output both
    // read the matmul through the sum
    const ir = await import("./engine/ir.ts")
    const { optimizeMany } = await import("./engine/opt.ts")
    const { matmul, rmsNorm } = await import("./engine/tests/vocab.ts")
    const x = ir.add(matmul(ir.buffer("X", [512, 27]), ir.buffer("E", [27, 32])), ir.buffer("P", [512, 32]))
    const y = rmsNorm(x, ir.broadcastTo(ir.buffer("g", [32]), [512, 32]))
    const loops = ir.toposort(optimizeMany([y])[0]!).filter((u) => u.op === "RANGE" && (u.arg as { extent: number }).extent === 27)
    expect(loops.length).toBe(1)
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
    const [dW, dX] = gradAll(ir.sum(ir.mul(y, y)), [W, X])               // ∇'s products are mul0: 0 × ∞ is 0
    expect([tiled(await lowered(y)), tiled(await lowered(dW)), tiled(await lowered(dX))]).toEqual([true, true, true])
  })
  test("a tile grid past 65,535 workgroups on an axis folds into two", async () => {
    const ir = await import("./engine/ir.ts")
    const { matmul } = await import("./engine/tests/vocab.ts")
    const tall = await lowered(matmul(ir.buffer("A", [1 << 20, 64]), ir.buffer("B", [64, 16])))
    expect(tiled(tall) && tall.every((k) => k.dispatch.every((d) => d <= 65535))).toBe(true)
  })
  const forms = async (root: any) => {
    const ir = await import("./engine/ir.ts")
    const { lowerMany } = await import("./engine/gpu.ts")
    return ir.toposort(lowerMany([root])[0]!).filter((u) => u.op === "KERNEL").map((u) => {
      const { wgsl, dispatch } = u.arg as { wgsl: string; dispatch: number[] }
      return { shape: u.shape.join(" "), dispatch: dispatch.join(" "), coop: wgsl.includes("var<workgroup> part") }
    })
  }
  test("a long reduction to few outputs splits over workgroups, and a second kernel combines their partials", async () => {
    const ir = await import("./engine/ir.ts")
    const picture = ir.buffer("p", [800, 1280, 3])
    for (const op of ["ADD", "MAX", "MUL"] as const)                      // a picture's range: 3 outputs, 1,024,000 terms each
      expect(await forms(ir.reduceAxis(ir.reduceAxis(picture, 0, true, op), 1, true, op))).toEqual([
        { shape: "32 3", dispatch: "3 1 32", coop: true }, { shape: "1 1 3", dispatch: "1", coop: false },
      ])
    expect(await forms(ir.sum(ir.buffer("v", [1 << 20])))).toEqual([{ shape: "96 1", dispatch: "1 1 96", coop: true }, { shape: "", dispatch: "1", coop: false }])
    // a picture's range in rows reads its neighbours side by side: k's fastest digit is the column
    const ks = (await import("./engine/gpu.ts")).lowerMany([ir.reduceAxis(ir.reduceAxis(picture, 0, true, "MAX"), 1, true, "MAX")])[0]!
    expect(ir.toposort(ks).some((u) => u.op === "KERNEL" && (u.arg as { wgsl: string }).wgsl.includes("((k) / 1u) % 1280u"))).toBe(true)
  })
  test("a reduction to many outputs side by side in memory runs a thread an output; in rows, a workgroup an output", async () => {
    const ir = await import("./engine/ir.ts")
    const { matmul } = await import("./engine/tests/vocab.ts")
    expect(await forms(ir.reduceAxis(ir.buffer("x", [4096, 1024]), 0, false))).toEqual([{ shape: "1024", dispatch: "4", coop: false }])
    expect(await forms(ir.reduceAxis(ir.buffer("x", [1024, 4096]), 1, false))).toEqual([{ shape: "1024", dispatch: "1024 1", coop: true }])
    expect(await forms(matmul(ir.buffer("h", [1, 1024]), ir.buffer("W", [1024, 512])))).toEqual([{ shape: "1 512", dispatch: "2", coop: false }])
    expect(await forms(ir.reduceAxis(ir.buffer("x", [4096, 64]), 0, false, "MAX"))).toEqual([{ shape: "64", dispatch: "64 1", coop: true }])   // few: the plain nest could not fill the machine
  })
  test("a sum or product over 2–4 elements is straight-line code, the same to the bit", async () => {
    const ir = await import("./engine/ir.ts")
    const { optimize } = await import("./engine/opt.ts")
    const { CPU } = await import("./engine/cpu.ts")
    const x = ir.buffer("x", [1000, 3]), d = Float32Array.from({ length: 3000 }, (_, i) => Math.sin(i) * 10 ** (i % 7))
    const loops = (k: any) => ir.toposort(k).filter((u) => u.op === "ACC").map((u) => (u.src[0]!.arg as { extent: number }).extent)
    const sum3 = ir.reduceAxis(x, 1, false), prod3 = ir.reduceAxis(x, 1, false, "MUL"), max3 = ir.reduceMax(x, 1, false)
    expect([sum3, prod3, max3].map((r) => loops(optimize(r)))).toEqual([[], [], [3]])   // a maximum keeps its loop
    const [s, p] = new CPU().realizeSync([sum3, prod3], new Map([["x", { data: d, shape: [1000, 3] }]])).map((h) => h.data)
    const f = Math.fround, want = (g: (a: number, b: number) => number, start: number) => Float32Array.from({ length: 1000 }, (_, i) => [0, 1, 2].reduce((a, c) => f(g(a, d[i * 3 + c]!)), start))
    expect([s, p]).toEqual([want((a, b) => a + b, 0), want((a, b) => a * b, 1)])
    // a term that reads a contraction runs that loop of its own: the squared distance of each
    // joint of an arm, L·S, from a point (the reach demo's forward kinematics)
    const L = Float32Array.from({ length: 256 }, (_, i) => (i % 17) - 8), S = Float32Array.from({ length: 32 }, (_, i) => (i % 5) - 2)
    const P = ir.add((await import("./engine/tensor.ts")).cellMatmul(ir.buffer("L", [16, 16]), ir.buffer("S", [16, 2])), ir.konst(-3))
    const [far] = new CPU().realizeSync([ir.reduceAxis(ir.mul(P, P), 1, false)], new Map([["L", { data: L, shape: [16, 16] }], ["S", { data: S, shape: [16, 2] }]])).map((h) => h.data)
    const joint = (i: number, c: number) => [...Array(16).keys()].reduce((a, j) => a + L[i * 16 + j]! * S[j * 2 + c]!, 0) - 3
    expect(far).toEqual(Float32Array.from({ length: 16 }, (_, i) => joint(i, 0) ** 2 + joint(i, 1) ** 2))
    // the nearest of 500 points, from each of 1000: a point's own coordinates leave the loop
    const { rangesIn } = await import("./engine/opt.ts")
    const q = ir.buffer("q", [1, 500, 2]), diff = ir.add(ir.buffer("pt", [1000, 1, 2]), ir.mul(q, ir.konst(-1)))
    const k = optimize(ir.reduceMax(ir.mul(ir.reduceAxis(ir.mul(diff, diff), 2, false), ir.konst(-1)), 1, false))
    const slot = ir.kInputs(k).findIndex((b) => b.op === "BUFFER" && ir.bufName(b) === "pt")
    const own = ir.toposort(k.src[1]!).filter((u) => u.op === "LOAD" && (u.arg as { slot: number }).slot === slot)
    expect(loops(k)).toEqual([500])
    expect(own.length && own.every((u) => [...rangesIn(u)].every((r) => r === k.src[0]))).toBeTruthy()
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
  const fakeGpu = async ({ failFirst = false } = {}) => {
    const { GPU } = await import("./engine/gpu.ts")
    const log = { pipelines: 0, destroyed: 0, popped: 0, done: [] as WeakRef<Promise<void>>[] }
    const buffer = (size: number) => ({ size, destroy: () => { log.destroyed++ } }) as unknown as GPUBuffer
    const pass = { setPipeline() {}, setBindGroup() {}, dispatchWorkgroups() {}, end() {} }
    const dev = {
      createBuffer: ({ size }: { size: number }) => buffer(size), createShaderModule: () => ({}), createBindGroup: () => ({}),
      createComputePipeline: () => { log.pipelines++; return { getBindGroupLayout: () => ({}) } },
      createCommandEncoder: () => ({ beginComputePass: () => pass, copyBufferToBuffer() {}, finish: () => ({}) }),
      queue: { writeBuffer() {}, submit() {}, onSubmittedWorkDone: () => { const p = Promise.resolve(); log.done.push(new WeakRef(p)); return p } },
      pushErrorScope() {}, popErrorScope: async () => (failFirst && !log.popped++ ? { message: "invalid" } : null),
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
  test("a scatter-add puts a fractional index in the row a gather reads, folded or not", () => {
    // 256 values fold as they are built; 257 run as a kernel
    expect([value("scatterAdd([1.5], [10], 256)_1"), value("scatterAdd([1.5], [10], 257)_1")]).toEqual([10, 10])
    expect(value("scatterAdd([-0.5, 2.9, 0 ÷ 0], [1, 2, 4], 300)_(0 ..< 4)")).toEqual(value("scatterAdd([-0.5, 2.9, 0 ÷ 0], [1, 2, 4], 4)"))
    expect(value("[10, 20, 30]_([1.5] ⍴ [1])")).toEqual([20])
  })
  test("a GPU validation error belongs to its own plan's results, not the next call", async () => {
    const ir = await import("./engine/ir.ts")
    const { gpu, buffer } = await fakeGpu({ failFirst: true }), x = ir.buffer("x", [8]), xb = buffer(32)
    const [a] = gpu.computeSync([ir.mul(x, ir.konst(2))], () => xb)
    await new Promise((r) => setTimeout(r, 0))
    // an unrelated plan runs: the error is not raised there
    const [b] = gpu.computeSync([ir.add(x, ir.konst(3))], () => xb)
    expect(b).toBeDefined()
    await expect(gpu.readAsync(a!, 8)).rejects.toThrow("gpu validation: invalid")
    expect(() => gpu.readSync(a!, 8)).toThrow("gpu validation: invalid")
    // a plan that computes from the failed result fails the same way
    const [c] = gpu.computeSync([ir.mul(ir.buffer("a", [8]), ir.konst(4))], () => a!)
    await expect(gpu.readAsync(c!, 8)).rejects.toThrow("gpu validation: invalid")
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
