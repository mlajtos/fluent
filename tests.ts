// End-to-end tests: Fluent source in, evaluated result out.
// Runs the real pipeline – grammar → AST → evaluator → tensors – on the Wasm
// backend (language.ts initializes jax-js on import). `bun test` runs this file.

import { describe, test, expect } from "bun:test"
import { Signal } from "@preact/signals-core"
import { CodeParse, evaluateSyntaxTreeNode, evaluateGeneration, createScope, np, FluentVariable } from "./language"

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
  test("reshape and range", () => {
    expect(value("[[1, 2], [3, 4]] ⍴ [4]")).toEqual([1, 2, 3, 4])
    expect(value("0 :: 4")).toEqual([0, 1, 2, 3])
  })
  test("stack with axis and broadcasting", () => {
    expect(value("stack(([1, 2], [3, 4]), 1)")).toEqual([[1, 3], [2, 4]])
    expect(value("stack(0 :: 3, 9)")).toEqual([[0, 1, 2], [9, 9, 9]])
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
  test("prelude conv via windows and ⊗", () => {
    expect(value("conv([1, 2, 1], 0 :: 6)")).toEqual([4, 8, 12, 16])
  })
})

describe("math functions", () => {
  test("clamp, sigmoid, relu", () => {
    expect(value("clamp(7, 2, 5)")).toBe(5)
    expect(value("sigmoid(0)")).toBe(0.5)
    expect(value("relu(0 - 3)")).toBe(0)
  })
  test("softmax sums to one", () => {
    expect(value("sum(softmax([1, 2, 3]))")).toBeCloseTo(1, 5)
  })
  test("oneHot", () => {
    expect(value("oneHot([2], 3) ⍴ [3]")).toEqual([0, 0, 1])
  })
  test("crossEntropy", () => {
    expect(value("crossEntropy([0, 1, 0], [1, 2, 0.5])")).toBeCloseTo(0.4644, 3)
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
  })
  test("higher-order gradient", () => {
    expect(value("∇(∇({ x | x^3 }))(2)")).toBe(12)
  })
  test("sgd fits y = 3x end-to-end", () => {
    const program = `
      x: 0 :: 8,
      y: x × 3,
      θ: ~([0]),
      𝓛: { mean((x × θ_0 - y)^2) },
      step: { v | sgd(0.01)(𝓛) },
      (step ⍣ 60)(◌),
      θ_0
    `
    expect(value(program)).toBeCloseTo(3, 1)
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
