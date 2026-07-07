// MARK: Fluent language core
// Grammar, evaluator, environment and prelude – no DOM, no React, no Monaco.
// Imported by the IDE (client.tsx) and by the end-to-end tests (tests.ts).

import { grammar, type ActionDict } from "ohm-js";
import { toAST as ohmToAST } from "ohm-js/extras";
import { signal, Signal, computed, effect } from "@preact/signals-core"
import dedent from "ts-dedent";
import { numpy as np, nn, lax, random, tree, vjp as jaxVjp, valueAndGrad, jit as jaxJit, init as initBackends, defaultDevice } from "@jax-js/jax"
import * as optax from "@jax-js/optax"

// jax-js compiles kernels per backend – initialize before the first array op.
// Prefer WebGPU, fall back to Wasm (bun, older browsers), then WebGL.
const BACKEND_PREFERENCE = ["webgpu", "wasm", "webgl", "cpu"] as const
const availableBackends = await initBackends()
defaultDevice(BACKEND_PREFERENCE.find((d) => availableBackends.includes(d)) ?? availableBackends[0])

// np.Array extends an abstract Tracer class; inside grad()/jit() the evaluator
// sees tracers instead of concrete arrays. Fluent treats both as tensors –
// only concrete arrays participate in arena ownership below.
const Tracer = Object.getPrototypeOf(np.Array.prototype).constructor as abstract new () => np.Array
const isTensor = (v: unknown): v is np.Array => v instanceof Tracer


const OPERATOR_RANGES: Record<string, [string, string]> = {
  BASIC_OPERATORS: ["\u0021", "\u007E"], // Basic ASCII operators
  MATH_OPERATORS: ["\u2200", "\u22FF"], // Mathematical Operators
  MISCELLANEOUS_TECHNICAL: ["\u2300", "\u23FF"], // Miscellaneous Technical Symbols
  SUPPLEMENTAL_MATH_OPERATORS: ["\u2A00", "\u2AFF"], // Supplemental Mathematical Operators
  ARROWS: ["\u2190", "\u21FF"], // Arrows
  SUPPLEMENTAL_ARROWS_A: ["\u27F0", "\u27FF"], // Supplemental Arrows A
  SUPPLEMENTAL_ARROWS_B: ["\u2900", "\u297F"], // Supplemental Arrows B
  MULTIPLICATION_SIGN: ["\u00D7", "\u00D7"], // Multiplication Sign
  DIVISION_SIGN: ["\u00F7", "\u00F7"], // Division Sign
  MISCELLANEOUS_MATH_SYMBOLS_B: ["\u2980", "\u29FF"],  // Miscellaneous Mathematical Symbols-B
  MIDDLE_DOT: ["\u00B7", "\u00B7"], // Middle Dot (multiplication)
  DOUBLE_VERTICAL_LINE: ["\u2016", "\u2016"], // Norm
  VARIATION_SELECTORS: ["\uFE00", "\uFE0F"], // Variation Selectors (e.g. \u2194\uFE0E = \u2194 + U+FE0E)
};

const RESERVED_SYMBOLS = "[|,{}()\\[\\];]"

const IDENTIFIER_RANGES: Record<string, [string, string]> = {
  MATHEMATICAL_ALPHANUMERIC_SYMBOLS: [String.fromCodePoint(0x1D400), String.fromCodePoint(0x1D7FF)], // Mathematical Alphanumeric Symbols
  EMOJIS: [String.fromCodePoint(0x1F300), String.fromCodePoint(0x1FFFF)], // Emoticons
}
const identifierRegexp = /(?:\p{L})[\p{L}\p{N}\-]*/u

const numberRegexp = /-?\d[\d_]*(?:\.\d[\d_]*)?(?:[eE][+-]?\d[\d_]*)?/
const stringRegexp = /"(?:[^"\\]|\\.)*"/

const getSymbolRange = (range: [string, string]) => {
  // Convert "\u0021-\u007E" to "\u0021".."\u007E"
  const [start, end] = range;
  return `"${start}".."${end}"`;
}

const getSymbolsRange = (symbolRangeObject: Record<string, [string, string]>) => {
  return `(${Object.values(symbolRangeObject).map(range => getSymbolRange(range)).join(" | ")})`;
};

const operatorRegexp = /[^\p{L}\p{N}]/u

const delimiterRegexp = new RegExp(RESERVED_SYMBOLS, "u")

// MARK: Grammar

const CodeGrammar = String.raw`
Fluent {
  Program
    = ListOf<Expr, ","> ","?

  Expr
    = Operation

  Operation
    = LongOperation
    | SpacedOperation

  // operator glued to its left operand, space after it:
  // everything to the right is the right operand – "a: 1 + 2" is "a: (1 + 2)"
  LongOperation
    = TightOperation #(~space) operator #(space) Expr

  // spaced operators chain left-to-right – "1 + 2 * 3" is "(1 + 2) * 3"
  SpacedOperation
    = (SpacedOperation | TightOperation) TightOperation TightOperation --infix
    | TightOperation TightOperation --prefix
    | TightOperation

  // glued operators bind tighter than spaced ones – "1 + 2*3" is "1 + (2 * 3)"
  TightOperation
    = TightOperation #(~space) operator #(~space) Application --infix
    | Application

  // application binds tightest – "f(x)", "∇(f)(x)"
  Application
    = Application #(~space) (List | NestedExpr) --apply
    | Atom

  Atom
    = Number | Lambda | NestedExpr | List | Symbol | String | Code | Tensor | Null

  NestedExpr
    = "(" Expr ")"

  Lambda
    = "{" (LambdaArgs "|")? Program "}"
  LambdaArgs
    = ListOf<Symbol, ",">

  List
    = "(" NonemptyListOf<Expr, ","> ","? ")" --multi
    | "(" Expr "," ")" --single
    | "(" ")" --empty

  Number          = number
  Tensor          = "[" ListOf<Expr, ","> ","? "]"
  Null            = "◌"
  Symbol          = identifier | ${getSymbolsRange(IDENTIFIER_RANGES)} | operator
  identifier      = &letter (alnum | "-")+
  number
    = "-"? digitGroup ("." digitGroup?)? exponent?
  exponent
    = ("e" | "E") ("+" | "-")? digitGroup
  digitGroup
    = digit ("_"? digit)*
  String          = #("\"" (~"\"" any)* "\"")
  Code            = #("${"`"}" (~"${"`"}" any)* "${"`"}")
  reserved        = "|" | "," | "{" | "}" | "(" | ")" | "[" | "]" | ";" | "\"" | "${"`"}"
  operator        = (#(~(reserved | alnum) specialChar))+
  specialChar     = ${getSymbolsRange(OPERATOR_RANGES)}
  space          += ";" (~"\n" any)* -- comment
}
`;

const CodeGrammarCompiled = grammar(CodeGrammar);

type Origin = {
  source: string;
  start: { line: number; column: number };
  end: { line: number; column: number };
}

type SyntaxTreeNode =
  | { type: "Program", content: SyntaxTreeNode[], origin: Origin }
  | { type: "Operation", content: { operator: SyntaxTreeNode, args: SyntaxTreeNode & { type: "List" } }, origin: Origin }
  | { type: "Symbol", content: { value: string }, origin: Origin }
  | { type: "Number", content: { value: number }, origin: Origin }
  | { type: "Tensor", content: { value: SyntaxTreeNode[] }, origin: Origin }
  | { type: "List", content: { value: SyntaxTreeNode[] }, origin: Origin }
  | { type: "Lambda", content: { args: SyntaxTreeNode[], expr: SyntaxTreeNode }, origin: Origin }
  | { type: "String", content: { value: string }, origin: Origin }
  | { type: "Error", content: string, origin: Origin }
  | { type: "Code", content: { value: SyntaxTreeNode }, origin: Origin }

function getLocationOrigin(node: any): Origin {
  const from = node.source.collapsedLeft().getLineAndColumn();
  const to = node.source.collapsedRight().getLineAndColumn();

  return {
    source: node.source.contents,
    start: {
      line: from.lineNum,
      column: from.colNum,
    },
    end: {
      line: to.lineNum,
      column: to.colNum,
    }
  }
}

// Lexical operator (from Tight/Long operations) -> Symbol node
const operatorSymbolNode = (op: { sourceString: string }): SyntaxTreeNode => ({
  type: "Symbol",
  content: { value: op.sourceString },
  origin: getLocationOrigin(op),
})

const syntaxTreeMapping: ActionDict<SyntaxTreeNode> = {
  Program(expressions, _) {
    return {
      type: "Program",
      content: expressions.toAST(this.args.mapping),
      origin: getLocationOrigin(this),
    }
  },
  LongOperation(left, operator, _space, right) {
    return {
      type: "Operation",
      content: {
        operator: operatorSymbolNode(operator),
        args: {
          type: "List",
          origin: getLocationOrigin(this),
          content: {
            value: [left.toAST(this.args.mapping), right.toAST(this.args.mapping)]
          },
        }
      },
      origin: getLocationOrigin(this),
    }
  },
  SpacedOperation_infix(left, operator, right) {
    return {
      type: "Operation",
      content: {
        operator: operator.toAST(this.args.mapping),
        args: {
          type: "List",
          origin: getLocationOrigin(this),
          content: {
            value: [left.toAST(this.args.mapping), right.toAST(this.args.mapping)]
          },
        }
      },
      origin: getLocationOrigin(this),
    }
  },
  TightOperation_infix(left, operator, right) {
    return {
      type: "Operation",
      content: {
        operator: operatorSymbolNode(operator),
        args: {
          type: "List",
          origin: getLocationOrigin(this),
          content: {
            value: [left.toAST(this.args.mapping), right.toAST(this.args.mapping)]
          },
        }
      },
      origin: getLocationOrigin(this),
    }
  },
  Application_apply(left, right) {
    const rightValue: SyntaxTreeNode = right.toAST(this.args.mapping);
    const isList = rightValue.type === "List";
    const args: SyntaxTreeNode = isList ? rightValue : {
      type: "List",
      origin: getLocationOrigin(right),
      content: {
        value: [rightValue]
      },
    }

    return {
      type: "Operation",
      content: {
        operator: left.toAST(this.args.mapping),
        args,
      },
      origin: getLocationOrigin(this),
    }
  },
  SpacedOperation_prefix(left, right) {
    const rightValue: SyntaxTreeNode = right.toAST(this.args.mapping);
    const isList = rightValue.type === "List";
    const args: SyntaxTreeNode = isList ? rightValue : {
      type: "List",
      origin: getLocationOrigin(right),
      content: {
        value: [rightValue]
      },
    }

    return {
      type: "Operation",
      content: {
        operator: left.toAST(this.args.mapping),
        args
      },
      origin: getLocationOrigin(this),
    }
  },
  List_multi(_, exprs, __, ___) {
    return {
      type: "List",
      content: {
        value: exprs.toAST(this.args.mapping),
      },
      origin: getLocationOrigin(this),
    }
  },
  List_single(_, expr, __, ___) {
    return {
      type: "List",
      content: {
        value: [expr.toAST(this.args.mapping)],
      },
      origin: getLocationOrigin(this),
    }
  },
  List_empty(_, __) {
    return {
      type: "List",
      content: {
        value: [],
      },
      origin: getLocationOrigin(this),
    }
  },
  Symbol(value) {
    return {
      type: "Symbol",
      content: {
        value: value.sourceString,
      },
      origin: getLocationOrigin(this),
    }
  },
  Number(value) {
    return {
      type: "Number",
      content: {
        value: +value.sourceString.replace(/_/g, ""), // remove underscores
      },
      origin: getLocationOrigin(this),
    }
  },
  Tensor(_, exprs, __, ___) {
    return {
      type: "Tensor",
      content: {
        value: exprs.toAST(this.args.mapping)
      },
      origin: getLocationOrigin(this),
    }
  },
  Lambda(_, args, __, expr, ___) {
    return {
      type: "Lambda",
      content: {
        args: args.toAST(this.args.mapping) ?? [],
        expr: expr.toAST(this.args.mapping)
      },
      origin: getLocationOrigin(this),
    };
  },
  String(_, value, __) {
    return {
      type: "String",
      content: {
        value: value.sourceString as string,
      },
      origin: getLocationOrigin(this),
    }
  },
  Code(_, value, __) {
    // Parse the raw backtick content as its own Program. Matching raw chars in
    // the grammar (like String) instead of an inline Program keeps PEG greed
    // from swallowing an adjacent literal's opening backtick as a nested one –
    // `a`, `b` used to conflate into a single Code node.
    return {
      type: "Code",
      content: {
        value: CodeParse(value.sourceString),
      },
      origin: getLocationOrigin(this),
    }
  },
} as const;

type ParseError = {
  message: string;
  start: { line: number; column: number };
  end: { line: number; column: number };
}

// Convert a string index to line and column numbers
const indexToLineAndColumn = (str: string, index: number): { line: number; column: number } => {
  const lines = str.slice(0, index).split('\n');
  const lastLine = lines[lines.length - 1] ?? '';
  return {
    line: lines.length,
    column: lastLine.length + 1
  };
}

// One parse per source: CodeParse and getParseErrors share the match result
let matchCache: { input: string, match: ReturnType<typeof CodeGrammarCompiled.match> } | null = null
const matchProgram = (program: string) => {
  const input = String(program)
  if (matchCache?.input === input) { return matchCache.match }
  const match = CodeGrammarCompiled.match(input)
  matchCache = { input, match }
  return match
}

const CodeParse = (program: string): SyntaxTreeNode => {
  const matchResult = matchProgram(program);

  if (matchResult.succeeded()) {
    const result = ohmToAST(matchResult, syntaxTreeMapping) as SyntaxTreeNode;
    return result
  } else {
    // @ts-ignore - getRightmostFailurePosition exists but not in types
    const errorPos = matchResult.getRightmostFailurePosition() as number;
    const errorLocation = indexToLineAndColumn(program, errorPos);

    return {
      type: "Error",
      content: matchResult.shortMessage ?? matchResult.message ?? "Unknown parse error",
      origin: {
        source: program,
        start: { line: errorLocation.line, column: errorLocation.column },
        end: { line: errorLocation.line, column: errorLocation.column + 1 }
      }
    };
  }
}

// Validate code and return parse errors for editor markers
const getParseErrors = (program: string): ParseError[] => {
  const matchResult = matchProgram(program);

  if (matchResult.succeeded()) {
    return [];
  }

  // @ts-ignore - getRightmostFailurePosition exists but not in types
  let errorPos = matchResult.getRightmostFailurePosition() as number;

  // If error is at start of line after newline, back up to previous line
  if (errorPos > 0 && program[errorPos - 1] === '\n') {
    errorPos--;
  }

  const startLocation = indexToLineAndColumn(program, errorPos);
  const endLocation = indexToLineAndColumn(program, program.length);

  return [{
    message: matchResult.shortMessage ?? matchResult.message ?? "Syntax error",
    start: startLocation,
    end: endLocation
  }];
}

// MARK: Evaluate

type Value = np.Array | FluentVariable | Function | Signal<Value> | Error | string | String | symbol | null | Value[]
type CurrentScope = Record<string, Value>

// Unified metadata map on function objects
interface FunctionMeta {
  doc?: string
  signature?: string
  example?: string
  noAutoLift?: boolean
  quotedArgs?: number[]
}

function getMeta(fn: Function): FunctionMeta {
  return (fn as any).__meta__ ?? {}
}

function setMeta(fn: Function, updates: Partial<FunctionMeta>): void {
  const prev = getMeta(fn)
  const changed = Object.keys(updates).some(k => prev[k as keyof FunctionMeta] !== updates[k as keyof FunctionMeta]);
  (fn as any).__meta__ = { ...prev, ...updates }
  if (!(fn as any).__meta_v__) {
    (fn as any).__meta_v__ = signal(0)
  }
  if (changed) {
    const v = (fn as any).__meta_v__
    queueMicrotask(() => v.value++)
  }
}

// WeakMap to store origin information for any object (including frozen ones like JSX)
const originMap = new WeakMap<object, Origin>()

function setOrigin(value: any, origin: Origin): void {
  if (value !== null && typeof value === 'object') {
    originMap.set(value, origin)
  }
}

function getOrigin(value: any): Origin | undefined {
  if (value !== null && typeof value === 'object') {
    return originMap.get(value)
  }
  return undefined
}

function evaluateSyntaxTreeNode(node: SyntaxTreeNode, env: CurrentScope): Value {

  if (node.type === "Error") {
    const error = new Error(node.content)
    setOrigin(error, node.origin)
    return error
  }

  if (node.type === "Program") {
    const values: Array<Value> = node.content.map((e) => evaluateSyntaxTreeNode(e, env))
    // last value is the result of the program - reify so symbols get resolved
    const lastValue = values[values.length - 1] ?? null
    return reify(lastValue, env)
  }

  if (node.type === "Number") {
    const value = track(np.array(node.content.value))
    syncReadCache.set(value, node.content.value)
    setOrigin(value, node.origin)
    return value
  }

  if (node.type === "Tensor") {
    const values = node.content.value.map((e) => evaluateSyntaxTreeNode(e, env))
    if (values.length === 0) {
      const value = track(np.array([]))
      setOrigin(value, node.origin)
      return value
    }

    const value = safeApply(TensorStack, values, env)
    setOrigin(value, node.origin)
    return value
  }

  if (node.type === "Symbol") {
    // Return raw symbol - resolve happens later in safeApply
    // Note: Can't set origin on primitive Symbol
    return Symbol.for(node.content.value)
  }

  if (node.type === "String") {
    // Use String object (not primitive) so we can track origin via WeakMap
    const value = new String(dedent(node.content.value))
    setOrigin(value, node.origin)
    return value
  }

  if (node.type === "List") {
    // Reify elements - List as value should have resolved symbols
    const value = node.content.value.map((e) => reify(evaluateSyntaxTreeNode(e, env), env))
    setOrigin(value, node.origin)
    return value
  }

  if (node.type === "Lambda") {
    // const fn = (...args: any[]) => {
    const fn = function (...args: any[]) {
      const localEnv = node.content.args
        .filter(n => n.type === "Symbol")
        .reduce<CurrentScope>((acc, arg, i) => {
          // @ts-ignore
          acc[arg.content.value] = args[i]
          return acc
        }, Object.create(env) as CurrentScope)

      // FunctionSelf: inject self-reference for anonymous recursion
      localEnv['self'] = fn

      // Reify in localEnv so symbols resolve to local values, not outer scope
      return reify(evaluateSyntaxTreeNode(node.content.expr, localEnv), localEnv)
    }

    fn.toString = () => node.origin.source
    setOrigin(fn, node.origin)

    return fn
  }

  if (node.type === "Operation") {
    const fn = evaluateSyntaxTreeNode(node.content.operator, env)

    // Process args directly (not via List evaluation) - safeApply handles quotedArgs
    const argNodes = node.content.args.content.value
    const args = argNodes.map((argNode: SyntaxTreeNode) =>
      evaluateSyntaxTreeNode(argNode, env)
    )

    const value = safeApply(fn, args, env)

    // Only set origin if value doesn't already have one - this preserves
    // origins from the original source (e.g., ListGet returning an element
    // should keep that element's original origin, not get the ListGet call's origin)
    if (!getOrigin(value)) {
      setOrigin(value, node.origin)
    }

    return value
  }

  if (node.type === "Code") {
    const value: any = codeNodePrinter(node.content.value)
    setOrigin(value, node.origin)
    return value
  }

  return null
}

np.Array.prototype.toString = function () {
  if (this.refCount === 0) { return "Tensor (disposed)" }
  if (this.size <= 32) { return `${getAsSyncList(this)}` }
  return `Tensor [${this.shape.join(" ")}]`
}

// String coercion (`${x}`, String(x)) goes through Symbol.toPrimitive, which
// jax-js only defines for scalars – route it through the printer instead.
;(np.Array.prototype as any)[Symbol.toPrimitive] = function (hint: string) {
  if (hint === "number" && this.ndim === 0 && this.refCount > 0) { return this.ref.item() }
  return this.toString()
}

// Extend the Symbol interface to include 'resolve' and 'assign'
declare global {
  interface Symbol {
    resolve(env: CurrentScope): any;
    assign(env: CurrentScope, value: any): void;
  }
}

// @ts-ignore
Symbol.prototype.resolve = function (this: Symbol, env: CurrentScope): Value {

  const s = this[Symbol.toPrimitive]("symbol")
  const k = Symbol.keyFor(s) ?? ""

  try {
    return env?.[k] ?? s
  } catch (e) {
    return null
  }
}

// @ts-ignore
Symbol.prototype.assign = function (env: CurrentScope, value: any) {

  const s = this[Symbol.toPrimitive]("symbol")
  const k = Symbol.keyFor(s) ?? ""

  Object.assign(env, {
    [k]: value,
  })
}

const reify = (v: Value, env: CurrentScope): Value => {
  if (typeof v === "symbol") {
    // @ts-ignore
    const resolved = v.resolve(env)

    if (resolved === v) {
      return v
    }

    if (typeof resolved === "symbol") {
      return reify(resolved, env)
    }

    return resolved
  }

  return v
}

// MARK: Ownership
// jax-js arrays are reference-counted with move semantics: operations consume
// their array arguments, `.ref` borrows one. Fluent's convention on top:
//
//  - Wrappers BORROW – every op call site takes `.ref`, so arguments stay
//    owned by whatever holds them (scope, list, signal, arena). A failing
//    candidate in a cascade can therefore retry with the same arguments.
//  - Every array a wrapper creates is TRACKED in the active arena. When the
//    arena closes (one operator application), arrays unreachable from its
//    result are disposed; survivors bubble up to the enclosing arena, or to
//    the evaluation generation at the top level. This replaces tf.tidy().
//  - Long-lived containers (signals, variables) OWN one reference of their
//    payload and release it on update.

// While a lifted application is being traced by jit (see makeLiftedApply),
// reads of outside state – signals, variables, the RNG – would be frozen
// into the compiled program. Reads mark the trace non-replayable (the traced
// result is still valid this round); writes abort it outright.
class TraceBailout extends Error {}
let tracingActive = false
let traceTouchedState = false

// Borrow a value for use in an op: +1 on tensors, unwrap variables. This is
// the boundary between Fluent's dynamic values and jax-js – ill-typed
// arguments flow through and the op's validation error becomes an Error value.
const borrow = (v: any): any => {
  if (v instanceof FluentVariable) {
    // a variable holding a tracer is a swapped-in parameter of the ongoing
    // trace (optimizer step) – only reading a CONCRETE value freezes state
    if (tracingActive && v.current instanceof np.Array) { traceTouchedState = true }
    return v.current.ref
  }
  if (isTensor(v)) { return v.ref }
  return v
}

const shapeOf = (v: Value): number[] =>
  (v instanceof FluentVariable ? v.current : (v as np.Array)).shape

type Arena = Set<np.Array>
const arenaStack: Arena[] = []

// Track a freshly created array (or a list holding some) in the active arena –
// the generation arena when no expression arena is open. Tracers pass through.
const track = <T>(value: T): T => {
  const arena = arenaStack[arenaStack.length - 1] ?? currentGeneration?.arena
  if (arena) { addConcreteArrays(value, arena) }
  return value
}

function addConcreteArrays(v: unknown, out: Arena, seen: Set<unknown> = new Set()): void {
  if (v instanceof np.Array) { out.add(v); return }
  if (Array.isArray(v) && !seen.has(v)) {
    seen.add(v)
    for (const item of v) { addConcreteArrays(item, out, seen) }
  }
}

// Arrays reachable from an arena result survive the sweep. Unlike tf.tidy's
// walk this descends into plain objects (JSX props) but treats the reactive
// graph shallowly: signal payloads are peeked, never computed.
function collectLiveArrays(v: unknown, out: Arena, seen: Set<unknown> = new Set()): void {
  if (v === null || typeof v !== "object") { return }
  if (seen.has(v)) { return }
  seen.add(v)
  if (v instanceof np.Array) { out.add(v); return }
  if (v instanceof Tracer) { return }
  if (v instanceof FluentVariable) { out.add(v.current); return }
  if (v instanceof Signal) { collectLiveArrays((v as any)._value, out, seen); return }
  if (v instanceof Error) { collectLiveArrays(v.cause, out, seen); return }
  if (Array.isArray(v)) {
    for (const item of v) { collectLiveArrays(item, out, seen) }
    return
  }
  for (const key of Object.keys(v)) { collectLiveArrays((v as any)[key], out, seen) }
}

// Run fn under a fresh scratch arena. Arrays it creates and keeps (reachable
// from the result) are handed to `owner`; the rest are disposed.
function arenaInto<T>(owner: Arena | null, fn: () => T): T {
  const scratch: Arena = new Set()
  arenaStack.push(scratch)
  let result: T | undefined
  try {
    result = fn()
  } finally {
    arenaStack.pop()
    if (scratch.size > 0) {
      const live: Arena = new Set()
      collectLiveArrays(result, live)
      for (const array of scratch) {
        if (live.has(array)) { owner?.add(array) }
        else if (array.refCount > 0) { array.dispose() }
      }
    }
  }
  return result as T
}

function arena<T>(fn: () => T): T {
  return arenaInto(arenaStack[arenaStack.length - 1] ?? currentGeneration?.arena ?? null, fn)
}

// A computed's cached payload is reachable only through its closure, which
// arenas cannot see – so a computed owns every array its recompute created
// and kept, disposing the previous batch on recompute and at generation end.
// (Handing survivors to the generation instead would pin one batch per frame
// until the next evaluation – exactly what a Time()-driven demo must avoid.)
const computedOwned = <T,>(fn: () => T): Signal<T> => {
  let owned: Arena = new Set()
  const release = () => {
    for (const array of owned) {
      if (array.refCount > 0) { array.dispose() }
    }
    owned = new Set()
  }
  registerDisposable(release)
  return computed(() => {
    release()
    return arenaInto(owned, fn)
  })
}

// Trainable variable: the jax-js replacement for tf.Variable. Owns one
// reference of its current value; assignment takes ownership of the next
// value and bumps the version signal (drag, optimizer step, :=).
class FluentVariable {
  current: np.Array
  readonly version = signal(0)

  // takes ownership of `initial`
  constructor(initial: np.Array) {
    const backed = bufferBackedScalar(initial)
    if (backed) {
      if (initial.refCount > 0) { initial.dispose() }
      initial = backed
    }
    this.current = initial
    trainableVariables.add(this)
    registerDisposable(() => {
      trainableVariables.delete(this)
      if (this.current.refCount > 0) { this.current.dispose() }
    })
  }

  get shape() { return this.current.shape }
  get size() { return this.current.size }

  // takes ownership of `next`
  assign(next: np.Array): void {
    const backed = bufferBackedScalar(next)
    if (backed) {
      if (next.refCount > 0) { next.dispose() }
      next = backed
    }
    const previous = this.current
    this.current = next
    if (previous.refCount > 0) { previous.dispose() }
    this.version.value++
  }

  toString() { return this.current.toString() }
}

// Optimizers with no explicit variable list train everything alive – the
// same contract as TFJS's global variable registry.
const trainableVariables = new Set<FluentVariable>()

// MARK: Evaluation generations
// Each top-level evaluation owns the resources it creates: live sources
// (Time, Camera, ...), pending ⟳ loops, and the tensors bound into its scope.
// A new evaluation retires the previous generation – loops stop, sources
// release their hardware, tensors free their GPU memory.
type Generation = { disposables: Set<() => void>, arena: Arena }
let currentGeneration: Generation | null = null

const registerDisposable = (dispose: () => void) => {
  currentGeneration?.disposables.add(dispose)
}

function evaluateGeneration<T>(evaluate: () => T): T {
  const previous = currentGeneration
  currentGeneration = { disposables: new Set(), arena: new Set() }
  const result = evaluate()
  if (previous) {
    // dispose after the new UI committed, so old islands never touch freed tensors
    setTimeout(() => {
      for (const dispose of previous.disposables) {
        try { dispose() } catch { /* already gone */ }
      }
      // every array the generation's evaluations created and kept lives here –
      // signals and variables released their own references via disposables
      for (const array of previous.arena) {
        if (array.refCount > 0) { array.dispose() }
      }
    }, 50)
  }
  return result
}

const disposeValueTensors = (value: unknown) => {
  const arrays: Arena = new Set()
  collectLiveArrays(value, arrays)
  for (const array of arrays) {
    if (array.refCount > 0) { array.dispose() }
  }
}

const disposeScopeTensors = (scope: CurrentScope) => {
  for (const key of Object.getOwnPropertyNames(scope)) {
    disposeValueTensors(scope[key])
  }
}

// Signals may hide inside lists, so e.g. `concat((a, b), axis)` stays
// reactive when a/b are computed
const containsSignal = (a: Value): boolean =>
  a instanceof Signal || (Array.isArray(a) && a.some(containsSignal))
const unwrapSignals = (a: any): any =>
  a instanceof Signal ? a.value : Array.isArray(a) ? a.map(unwrapSignals) : a

// MARK: Lifted apply
// A signal-driven application re-executes an identical tensor graph on every
// recompute. Trace it once with jit() – fused kernels instead of per-op
// dispatch – and replay the compiled program with the current payloads.
//
// When an argument is itself a lifted computed, its body is INLINED into the
// trace instead of being read: the compiled program spans the whole chain
// down to the true source signals (Time, sliders), jax-js chooses the
// materialization points, and intermediate computeds never execute unless
// something displays them. Shared intermediates (diamonds) trace once via a
// per-trace memo.
//
// Anything a trace cannot capture faithfully drops back to the eager
// evaluator, permanently for this application:
//  - value-dependent control flow (guard, mask, sort counts – any data read)
//  - randomness (a replay would freeze the sample)
//  - signal/variable access inside the body (a replay would freeze the value)
//  - non-tensor results (JSX, lists) and signals nested inside list arguments
type Lift = {
  args: Value[]
  demoted: () => boolean
  // trace this lift's body, resolving leaves through `bind`; memoized per trace
  inline: (bind: (leaf: Signal<Value>) => np.Array) => np.Array
}
const liftRegistry = new WeakMap<Signal<Value>, Lift>()
let currentTraceMemo: Map<Lift, np.Array> | null = null

const makeLiftedApply = (fnValue: Function, argsValue: Value[], env: CurrentScope) => {
  const hasNestedSignals = argsValue.some((a) => !(a instanceof Signal) && containsSignal(a))

  let compiled: (((...xs: np.Array[]) => np.Array) & { dispose: () => void }) | null = null
  let leaves: Signal<Value>[] = []
  let mode: "probe" | "jit" | "eager" = hasNestedSignals ? "eager" : "probe"

  const eagerApply = (unwrapped: any[]): Value => {
    try {
      return fnValue.apply(env, unwrapped)
    } catch (e) {
      return e as Value
    }
  }

  const demote = () => {
    compiled?.dispose()
    compiled = null
    mode = "eager"
  }
  registerDisposable(demote)

  // the source signals this lift's chain depends on: walk through inlinable
  // children, stop at plain signals and demoted (eager) lifts
  const collectLeaves = (args: Value[], out: Set<Signal<Value>>): void => {
    for (const arg of args) {
      if (!(arg instanceof Signal)) { continue }
      const child = liftRegistry.get(arg)
      if (child && !child.demoted()) { collectLeaves(child.args, out) }
      else { out.add(arg) }
    }
  }

  // run the body on traced values: leaves resolve through `bind`, inlinable
  // children splice their own traced bodies in
  const runTraced = (bind: (leaf: Signal<Value>) => np.Array): np.Array => {
    const rebuilt = argsValue.map((arg) => {
      if (!(arg instanceof Signal)) { return arg }
      const child = liftRegistry.get(arg)
      if (child && !child.demoted()) { return child.inline(bind) }
      return bind(arg)
    })
    const out = fnValue.apply(env, rebuilt)
    if (out instanceof Error) { throw out }
    if (!isTensor(out)) { throw new TraceBailout("non-tensor result") }
    return out as np.Array
  }

  const lift: Lift = {
    args: argsValue,
    demoted: () => mode === "eager",
    inline: (bind) => {
      if (mode === "eager") { throw new TraceBailout("eager child") }
      const memo = currentTraceMemo!
      const cached = memo.get(lift)
      if (cached) { return cached.ref }
      const traced = runTraced(bind)
      memo.set(lift, traced)
      return traced.ref
    },
  }

  const recompute = (): Value => {
    if (mode === "eager") {
      // reading .value subscribes this computed to its (possibly derived) args
      return eagerApply(argsValue.map(unwrapSignals))
    }

    if (mode === "probe") {
      const found = new Set<Signal<Value>>()
      collectLeaves(argsValue, found)
      leaves = [...found]
    }

    // reading the leaves subscribes this computed to the chain's sources
    const payloads = leaves.map((s) => s.value)
    if (!payloads.every((p) => p instanceof np.Array)) {
      demote()
      return eagerApply(argsValue.map(unwrapSignals))
    }

    compiled ??= jaxJit((...params: np.Array[]) => {
      const bound = new Map(leaves.map((leaf, i) => [leaf, params[i]!]))
      const previousMemo = currentTraceMemo
      const memo: Map<Lift, np.Array> = new Map()
      currentTraceMemo = memo
      try {
        const result = runTraced((leaf) => {
          const param = bound.get(leaf)
          if (param === undefined) { throw new TraceBailout("unbound leaf") }
          return param.ref
        })
        return result
      } finally {
        currentTraceMemo = previousMemo
        for (const traced of memo.values()) {
          if (traced.refCount > 0) { traced.dispose() }
        }
        for (const param of params) {
          if (param.refCount > 0) { param.dispose() }
        }
      }
    }) as any

    // a compiled call may retrace at any time (new payload shapes), so the
    // trace guards are armed on every call – a pure replay never trips them
    const rngBefore = rngCounter
    const outerTracing = tracingActive
    const outerTouched = traceTouchedState
    tracingActive = true
    traceTouchedState = false
    try {
      const result = track(compiled!(...payloads.map((p) => (p as np.Array).ref)))
      if (traceTouchedState || rngCounter !== rngBefore) {
        // the trace froze outside state or a random draw – this round's
        // result is valid, but a replay would not be
        demote()
      } else {
        mode = "jit"
      }
      return result
    } catch (e) {
      demote()
      // TraceBailout: the body is untraceable – run it for real.
      // Anything else: eager reruns and surfaces it as an Error value.
      return eagerApply(argsValue.map(unwrapSignals))
    } finally {
      tracingActive = outerTracing
      traceTouchedState = outerTouched || traceTouchedState
    }
  }

  return { recompute, lift }
}

function safeApply(fn: Value, args: Value[], env: CurrentScope): Value {
  let errorArgs: Error[] = []

  try {

    let fnValue: Value = reify(fn, env);
    let argsValue: Value[] = args;

    // Read function metadata
    const meta = typeof fnValue === 'function' ? getMeta(fnValue) : {}
    const { quotedArgs = [], noAutoLift = false } = meta

    // Selectively reify arguments - quoted args stay as symbols
    argsValue = args.map((arg, i) =>
      quotedArgs.includes(i) ? arg : reify(arg, env)
    )

    errorArgs = argsValue.filter(a => a instanceof Error)

    if (typeof fnValue !== "function" && !(fnValue instanceof Signal)) {
      throw new Error(`'${String(fnValue)}' is not a function.`)
    }

    if (fnValue instanceof Signal) {

      if (argsValue.length === 0) {
        return SignalRead(fnValue)
      }

      if (argsValue.length === 1) {
        SignalUpdate(fnValue, argsValue[0])
        return null
      }

      throw new Error("What are you trying to do with this poor signal?")
    }

    // Auto-lift: wrap in computed() when args contain Signals. The recompute
    // goes through makeLiftedApply, which jit-compiles traceable bodies and
    // inlines chains of lifted computeds into a single compiled program.
    const hasSignalArgs = argsValue.some(containsSignal)

    if (hasSignalArgs && !noAutoLift) {
      const { recompute, lift } = makeLiftedApply(fnValue, argsValue, env)
      const lifted = computedOwned(recompute)
      liftRegistry.set(lifted as Signal<Value>, lift)
      return lifted
    }

    return arena(() => {
      return fnValue.apply(env, argsValue)
    })
  } catch (e) {
    // inject cause to the caught error from argument errors
    if (e instanceof Error && errorArgs.length > 0) {
      return new Error(e.message, { cause: errorArgs[0] })
    } else {
      // @ts-ignore
      return e
    }
  }
}

const FunctionCascade = (candidates: Function[]) => (a: Value, b: Value) => {
  const noResultSymbol = Symbol('noResult')

  let result: (Value | typeof noResultSymbol) = noResultSymbol

  let candidateIndex = 0

  while (result === noResultSymbol && candidateIndex < candidates.length) {

    let candidateResult: (Value | typeof noResultSymbol) = noResultSymbol

    try {
      const fn = candidates[candidateIndex]
      candidateResult = fn?.(a, b)
    } catch (e) {
      candidateIndex += 1
      continue
    }

    if (candidateResult instanceof Error) {
      candidateIndex += 1
      continue
    } else {
      result = candidateResult
    }
  }

  if (result === noResultSymbol) {
    return new Error('No operator found')
  }

  return result
}

// Synchronous, non-consuming read of a tensor's data as nested JS values.
// jax-js `.js()` consumes the array, so read through a borrowed reference.
// Every read crosses the device boundary (on WebGPU: a canvas readback), and
// arrays are immutable – cache the JS value per array. Number literals are
// pre-seeded at creation, so `x^2` or `sum(x, 2)` never read the device.
const syncReadCache = new WeakMap<object, unknown>()
function getAsSyncList(value: unknown) {
  if (value instanceof FluentVariable) { value = value.current }
  if (value instanceof np.Array) {
    if (syncReadCache.has(value)) { return syncReadCache.get(value) }
    const data = value.ref.js()
    syncReadCache.set(value, data)
    return data
  }
  if (isTensor(value)) {
    // a non-concrete tracer: the body's control flow depends on data a
    // compiled replay wouldn't see – abort the trace, stay eager
    throw new TraceBailout("value read during trace")
  }
}

// MARK: Environment

const evaluateProgramWithScope = (program: string, scope: CurrentScope) => {
  const syntaxTree = CodeParse(program)

  if (syntaxTree.type === "Error") {
    console.error("Error parsing program:", syntaxTree.content);
    return new Error(syntaxTree.content);
  }

  return evaluateSyntaxTreeNode(syntaxTree, scope)
}

const CodeEvaluate = function (this: CurrentScope, program: string) {
  return evaluateProgramWithScope(program, this);
}

// APL-style power operator: FunctionPower(f, n) is { x | f(f(...f(x))) }, n times
const FunctionPower = (fn: Function, n: Value) => {
  if (typeof fn !== "function" || !isTensor(n)) {
    return new Error("`FunctionPower(fn, n)`: `fn` must be a function and `n` must be a scalar Tensor")
  }

  const times = getAsSyncList(n) as number

  return (...args: unknown[]) => {
    let value: unknown = args[0] ?? null
    for (let i = 0; i < times; i++) {
      value = fn(value)
    }
    return value
  }
}

// Async repeat (⟳): runs fn between frames so the UI stays live. The loop
// belongs to the evaluation that started it – a re-evaluation cancels it.
// (For synchronous function iteration f(f(f(x))) use ⍣ / FunctionPower.)
const FunctionIterate = (fn: (index?: np.Array) => void, iterations?: Value) => {
  if (!(typeof fn === "function" && (iterations === undefined || isTensor(iterations)))) {
    throw new Error("`FunctionIterate(fn, iterations)`: `fn` must be a function and `iterations` must be a scalar Tensor");
  }

  const maxIterations = iterations === undefined ? 1 : getAsSyncList(iterations) as number
  const generation = currentGeneration
  let i = 0

  // Pace with requestAnimationFrame: steps align to the display, the loop pauses
  // with a hidden tab, and a batch of steps per frame amortizes the ~4ms one
  // setTimeout(0) would cost on a light step. The batch adapts to the measured
  // inter-frame time – the display realizes the loop's output each frame, so a
  // heavy step (or an expensive plot) self-limits to a small batch while a cheap
  // one runs many. Falls back to setTimeout headless (bun), where there is no rAF.
  const raf: (cb: (t: number) => void) => void =
    typeof (globalThis as any).requestAnimationFrame === "function"
      ? (cb) => (globalThis as any).requestAnimationFrame(cb)
      : (cb) => { setTimeout(() => cb((globalThis as any).performance?.now?.() ?? 0), 0) }

  let batch = 1
  let prev = 0

  const frame = (t: number) => {
    if (generation !== currentGeneration || i >= maxIterations) { return }

    // steer the batch by the inter-frame time. rAF is vsync-locked (~16.7ms), so
    // a frame that hits vsync means there was headroom – grow; a frame that slips
    // to the next vsync (~33ms+) overran – shrink. The cap keeps the per-frame
    // GPU work well under one refresh so the device never backlogs.
    if (prev) {
      const dt = t - prev
      if (dt > 25) { batch = Math.max(1, Math.floor(batch * 0.5)) }
      else { batch = Math.min(64, batch + Math.max(1, batch >> 2)) }
    }
    prev = t

    const end = Math.min(i + batch, maxIterations)
    while (i < end) {
      // each step gets its own arena: per-iteration garbage dies immediately,
      // values that persist do so through signal/variable assignments
      arena(() => {
        const index = track(np.array(i))
        fn(index)
        return null
      })
      i++
    }

    // drain the lazy queue: jax-js never dispatches on its own, so a training
    // loop whose variables nobody displays would grow an unbounded pending
    // graph – realize what the batch retained
    for (const variable of trainableVariables) {
      const held = variable.current.ref
      held.blockUntilReady().then(
        () => { if (held.refCount > 0) { held.dispose() } },
        () => { /* generation retired mid-flight */ },
      )
    }

    raf(frame)
  }
  raf(frame)

  return null
}

// jax-js backs every 1-element array with an inline constant (np.array
// special-cases size === 1 into full()), so kernels consuming it embed the
// value and recompile whenever it changes. Payloads of signals and variables
// change on every update – re-back known scalars with a real device buffer
// (a 2-element upload sliced to a 0-d view) so downstream kernels stay
// stable and the shader cache hits. Returns an owned array, or null when the
// value isn't a scalar with a known JS value.
const alreadyBufferBacked = new WeakSet<object>()
const bufferBackedScalar = (v: unknown): np.Array | null => {
  if (!(v instanceof np.Array) || v.ndim !== 0 || alreadyBufferBacked.has(v)) { return null }
  if (!syncReadCache.has(v)) { return null }
  const value = syncReadCache.get(v)
  if (typeof value !== "number") { return null }
  return TensorScalarLive(value)
}

// Fresh scalar for time-varying sources (Time, sliders, scrubbers): buffer-
// backed so downstream kernels cache, pre-seeded so display reads are free.
// Owned by the caller, not arena-tracked. The NaN sentinel keeps the pair
// from ever being all-equal – jax-js collapses all-equal data (including
// [0, 0]) back into an inline constant.
const TensorScalarLive = (value: number): np.Array => {
  const backed = np.array(new Float32Array([value, NaN])).slice(0)
  syncReadCache.set(backed, value)
  alreadyBufferBacked.add(backed)
  return backed
}

// Signals own one reference of their tensor payload: created with a borrowed
// reference, released on every update. Reads hand out the payload unowned –
// wrappers borrow at their call sites.
const SignalCreate = (<T,>(initial: T) => signal(bufferBackedScalar(initial) ?? borrow(initial))) as typeof signal

const SignalRead = <T,>(s: Signal<T>) => {
  if (!(s instanceof Signal)) {
    return new Error(`'SignalRead': ${String(s)} is not a signal`)
  }
  if (tracingActive) { traceTouchedState = true }
  return s.value
}

const SignalUpdate = <T,>(s: Signal<T>, v: T) => {
  if (s instanceof Signal) {
    if (tracingActive) { throw new TraceBailout("signal write during trace") }
    const previous = s.peek() as unknown
    s.value = (bufferBackedScalar(v) ?? borrow(v)) as T
    if (previous instanceof np.Array && previous.refCount > 0) {
      previous.dispose()
    }
    return
  }

  return new Error(`'SignalUpdate': ${s} is not a signal`)
}

// user-facing computeds own their cached tensors, like auto-lifted ones
const SignalComputed = (<T,>(fn: () => T) => computedOwned(fn)) as typeof computed

const SignalEffect = effect
setMeta(SignalEffect, { noAutoLift: true })

// Read signal value once without creating reactive dependency
const SignalOnce = <T,>(s: Signal<T> | T): T => {
  if (s instanceof Signal) {
    if (tracingActive) { traceTouchedState = true }
    return s.peek()
  }
  return s
}
setMeta(SignalOnce, { noAutoLift: true })

const Reactive = (a: Value) => {
  if (typeof a === "function") {
    // @ts-ignore
    return SignalComputed(a)
  }
  return SignalCreate(a)
}
setMeta(Reactive, { noAutoLift: true })

const SymbolAssign = function (this: CurrentScope, a: Value, b: Value) {
  if (typeof a === "symbol") {
    a.assign(this, b)
    return a.resolve(this)
  } else {
    return new Error(`'SymbolAssign': Left side must be a symbol, got: ${String(a)}`)
  }
}

// First argument (target symbol) should not be reified
setMeta(SymbolAssign, { quotedArgs: [0], noAutoLift: true })

const FunctionEvaluate = function (this: CurrentScope, fn: Value, args: Value[]) {
  return safeApply(fn, args, this)
}

const FunctionApply = function (this: CurrentScope, a: Value, b: Value): Value {
  const arg = a instanceof Array ? a : [a]
  return safeApply(b, arg, this)
}

const FunctionNoAutoLift = (fn: Function) => {
  setMeta(fn, { noAutoLift: true })
  return fn
}
setMeta(FunctionNoAutoLift, { noAutoLift: true })

const FunctionGuard = function (this: CurrentScope, cond: Value, thunk: Function) {
  // Capture condition value and thunk
  const condValue = getAsSyncList(cond)
  const scope = this

  // Return a function that cascade can call as a candidate
  return function () {
    // Check if falsy: 0/false, all-zero tensor, NaN (comparisons are bool dtype)
    const falsy = (v: unknown) => v === 0 || v === false
    const isFalsy = falsy(condValue) ||
      (Array.isArray(condValue) && (condValue.flat(Infinity) as unknown[]).every(falsy)) ||
      Number.isNaN(condValue)

    if (isFalsy) {
      return new Error('Guard condition not met')
    }

    // Condition passed - evaluate thunk
    if (typeof thunk === 'function') {
      return thunk.apply(scope)
    }

    return new Error('FunctionGuard: second argument must be a thunk { value }')
  }
}
setMeta(FunctionGuard, { noAutoLift: true })

const Describe = (target: Value, doc?: string): Value => {
  if (doc === undefined) {
    if (typeof target === 'function') return getMeta(target).doc ?? null
    return null
  }
  if (typeof target === 'function') {
    setMeta(target as Function, { doc: String(doc) })
  }
  return target
}
setMeta(Describe, { noAutoLift: true })

// Attach editor docs to a built-in and return it – authored inline at the
// binding in the prelude, so a name's docs live next to its definition.
// Aliases share the function object, so documenting one covers them all.
const doc = (target: Value, signature: Value, description: Value, example: Value): Value => {
  if (typeof target === 'function') {
    setMeta(target as Function, { signature: String(signature), doc: String(description), example: String(example) })
  }
  return target
}
setMeta(doc, { noAutoLift: true })

const List = (...args: unknown[]) => {
  return args
}

const ListConcat = (...args: Value[]) => {
  return args.reduce((acc, arg) => {
    if (arg instanceof Array) {
      return acc.concat(arg)
    } else {
      return acc.concat(new Error(`'ListConcat': argument is not an array: ${String(arg)}`))
    }
  }, [] as unknown[]
  )
}

const ListLength = (a: unknown[]) => {
  return track(np.array(a.length))
}

const ListGet = (a: any[], b: Value) => {
  if (!Array.isArray(a)) {
    return new Error("'ListGet': 'a' must be an array")
  }

  const index = getAsSyncList(b) as number
  if (index < -a.length || index >= a.length) {
    return new Error(`'ListGet': Index '${index}' out of bounds for list of length ${a.length}`)
  }

  return a.at(index)
}

const ListMap = (a: any[], fn: (value: any, index: Value) => any) => {
  if (typeof fn !== "function") {
    return new Error("'ListMap': 'fn' must be a function")
  }

  if (!Array.isArray(a)) {
    return new Error("'ListMap': 'a' must be a list")
  }

  return a.map((value, index) => {
    // @ts-ignore
    return FunctionEvaluate(fn, [value, TensorScalar(index)])
  })
}

const ListReduce = (a: any[], fn: (acc: any, value: any) => any, initialValue?: any) => {
  if (typeof fn !== "function") {
    return new Error("'ListReduce': `fn` must be a function")
  }

  if (initialValue === undefined) {
    return a.reduce(fn)
  }

  return a.reduce(fn, initialValue)
}

// Read scalar/vector metadata (axis, sizes) out of a tensor argument
const asNumber = (v: Value): number => Number(getAsSyncList(v))
const asNumberList = (v: Value): number[] => ([] as number[]).concat(getAsSyncList(v) as any)

const Tensor = (values: unknown, shape?: Value) => {
  const source: any = isTensor(values) || values instanceof FluentVariable ? borrow(values) : values
  return track(np.array(source, shape === undefined ? undefined : { shape: asNumberList(shape) }))
}
const TensorScalar = (value: number) => {
  const scalar = track(np.array(value))
  syncReadCache.set(scalar, value)
  return scalar
}

// Wrapper factories: borrow the tensor arguments, track the result.
const unaryOp = (op: (x: any) => np.Array) => (a: Value) => track(op(borrow(a)))
const binaryOp = (op: (x: any, y: any) => np.Array) => (a: Value, b: Value) => track(op(borrow(a), borrow(b)))

// stack broadcasts its inputs to a common shape, like arithmetic does:
// stack(x - a, y - b) works even when the pieces only meet by broadcasting.
// Returns owned references, ready to be consumed by stack/concatenate.
const broadcastRefs = (values: Value[]): np.Array[] => {
  const tensors = values.map(v => v instanceof FluentVariable ? v.current : v)
  if (tensors.length === 0 || !tensors.every(isTensor)) { return tensors.map(borrow) as np.Array[] }
  const arrays = tensors as np.Array[]
  const rank = Math.max(...arrays.map(t => t.shape.length))
  const shape = Array.from({ length: rank }, (_, i) =>
    Math.max(...arrays.map(t => t.shape[t.shape.length - rank + i] ?? 1)))
  return arrays.map(t =>
    t.shape.length === rank && t.shape.every((d, i) => d === shape[i]) ? t.ref : np.broadcastTo(t.ref, shape))
}

// stack(a, b, ...) joins along axis 0; stack((a, b, ...), axis) picks the axis
const TensorStack = (...args: unknown[]) => {
  if (Array.isArray(args[0])) {
    const axis = args[1] === undefined ? 0 : asNumber(args[1] as Value)
    return track(np.stack(broadcastRefs(args[0] as Value[]), axis))
  }
  return track(np.stack(broadcastRefs(args as Value[])))
}
const TensorUnstack = (a: Value, b?: Value) => {
  const axis = b === undefined ? 0 : asNumber(b)
  const array = borrow(a) as np.Array
  // the iterator consumes the array and yields slices along the first axis
  return track([...(axis === 0 ? array : np.moveaxis(array, axis, 0))])
}
const TensorConcat = (...args: unknown[]) => {
  if (Array.isArray(args[0])) {
    const axis = args[1] === undefined ? 0 : asNumber(args[1] as Value)
    return track(np.concatenate((args[0] as Value[]).map(borrow) as np.Array[], axis))
  }
  return track(np.concatenate((args as Value[]).map(borrow) as np.Array[]))
}
const TensorTile = (a: Value, reps: Value) => {
  return track(np.tile(borrow(a), asNumberList(reps)))
}

// Generalized outer product – J's "table" adverb. `a ⊗(f) b` applies f between
// every cell of a and every cell of b: result shape [frame(a), frame(b), cell].
// Cells are the trailing `rank` axes (default 0, scalar cells), so with rank 1
// `dx ⊗((+), 1) dy` crosses the frames while zipping the shared trailing axis.
// rank can be a scalar or a [rankA, rankB] pair.
const TensorOuter = function (this: CurrentScope, f: Value, rank?: Value) {
  const r = rank === undefined ? 0 : getAsSyncList(rank) as (number | number[])
  const [rankA, rankB] = Array.isArray(r) ? r : [r, r]
  const scope = this

  return function (a: Value, b: Value) {
    const shapeA = shapeOf(a)
    const split = Math.max(0, shapeA.length - (rankA ?? 0))
    const frameA = shapeA.slice(0, split)
    const cellA = shapeA.slice(split)
    const frameBLength = Math.max(0, shapeOf(b).length - (rankB ?? 0))
    const cellPad = Math.max(0, (rankB ?? 0) - cellA.length)
    const lifted = track(np.reshape(borrow(a), [...frameA, ...Array(frameBLength + cellPad).fill(1), ...cellA]))
    return safeApply(f, [lifted, b], scope)
  }
}

const TensorAdd = binaryOp(np.add)
const TensorSubtract = binaryOp(np.subtract)
const TensorMultiply = binaryOp(np.multiply)
const TensorDivide = binaryOp(np.trueDivide)

// Exponentiation by squaring – consumes `base`, exact where exp(e·ln x) is
// not: 3^2 is 9, (-2)^2 is 4, and x^2 differentiates as x·x.
const intPow = (base: np.Array, exponent: number): np.Array => {
  if (exponent === 0) { return np.onesLike(base) }
  let e = Math.abs(exponent)
  let square = base
  let result: np.Array | null = null
  while (true) {
    if (e & 1) { result = result === null ? square.ref : np.multiply(result, square.ref) }
    e >>= 1
    if (e === 0) { break }
    square = np.multiply(square.ref, square)
  }
  square.dispose()
  return exponent < 0 ? np.reciprocal(result!) : result!
}

const TensorPower = (a: Value, b: Value) => {
  const exponent = b instanceof FluentVariable ? b.current : b
  if (exponent instanceof np.Array && exponent.ndim === 0) {
    const e = asNumber(exponent)
    if (Number.isInteger(e) && Math.abs(e) <= 64) {
      return track(intPow(borrow(a) as np.Array, e))
    }
  }
  return track(np.power(borrow(a), borrow(b)))
}
const TensorRoot = (a: Value, b?: Value) => {
  if (b === undefined) {
    return track(np.sqrt(borrow(a)))
  }
  return TensorPower(b, TensorReciprocal(a))
}
// WGSL permits 2.5 ULP of division error, so on WebGPU floor(x/x) can miss
// and remainder(x, x) comes back as x. A remainder of exactly ±y is
// mathematically impossible – fold it to 0.
const TensorRemainder = (a: Value, b: Value) => {
  const r = np.remainder(borrow(a), borrow(b))
  return track(np.where(np.equal(np.absolute(r.ref), np.absolute(borrow(b))), np.zerosLike(r.ref), r))
}
const TensorMaximum = binaryOp(np.maximum)
const TensorMinimum = binaryOp(np.minimum)

// Comparisons yield float32 0/1, APL-style. jax-js bools ride a
// bool → uint32 lattice where a weak scalar promotes UNSIGNED – and a
// negated weak constant clamps at zero, so `(x = 1) - y` silently
// subtracted nothing. Fluent is a float32 language; 0/1 composes.
const comparisonOp = (op: (x: any, y: any) => np.Array) =>
  (a: Value, b: Value) => track(np.astype(op(borrow(a), borrow(b)), np.float32))

const TensorLess = comparisonOp(np.less)
const TensorGreater = comparisonOp(np.greater)
const TensorLessEqual = comparisonOp(np.lessEqual)
const TensorGreaterEqual = comparisonOp(np.greaterEqual)
const TensorEqual = comparisonOp(np.equal)
const TensorNotEqual = comparisonOp(np.notEqual)

const TensorSine = unaryOp(np.sin)
const TensorCosine = unaryOp(np.cos)
const TensorTangent = unaryOp(np.tan)
const TensorSineHyperbolic = unaryOp(np.sinh)
const TensorCosineHyperbolic = unaryOp(np.cosh)
const TensorTangentHyperbolic = unaryOp(np.tanh)
const TensorSineInverse = unaryOp(np.asin)
const TensorCosineInverse = unaryOp(np.acos)
const TensorTangentInverse = unaryOp(np.atan)
const TensorSineHyperbolicInverse = unaryOp(np.arcsinh)
const TensorCosineHyperbolicInverse = unaryOp(np.arccosh)
const TensorTangentHyperbolicInverse = unaryOp(np.arctanh)
const TensorArcTangent2 = binaryOp(np.arctan2)  // arctan2(y, x): angle of (x, y)
const TensorHypotenuse = binaryOp(np.hypot)
const TensorDegToRad = unaryOp(np.deg2rad)
const TensorRadToDeg = unaryOp(np.rad2deg)

// jax-js reduces bool arrays in bool arithmetic (add is `or`, so a bool sum
// can only be 0 or 1) – promote to float like NumPy, so sum(x > 1) counts
const promoteBool = (v: any): any => {
  const tensor = v instanceof FluentVariable ? v.current : v
  return isTensor(tensor) && tensor.dtype === np.bool ? np.astype(borrow(v), np.float32) : borrow(v)
}

// Reductions take an optional axis (scalar or vector tensor) as second argument
const withOptionalAxis = (op: (a: any, axis?: number | number[]) => np.Array) =>
  (a: Value, b?: Value) =>
    track(b === undefined ? op(promoteBool(a)) : op(promoteBool(a), getAsSyncList(b) as (number | number[])))

const TensorSum = withOptionalAxis(np.sum)      // TensorReduce(a, 0, +)
const TensorProduct = withOptionalAxis(np.prod) // TensorReduce(a, 1, *)
const TensorMean = withOptionalAxis(np.mean)
const TensorMin = withOptionalAxis(np.min)
const TensorMax = withOptionalAxis(np.max)

// argmax/argmin take a single axis (or none, over the flattened array) and
// return int32 indices of the extreme element
const TensorArgMax = (a: Value, axis?: Value) =>
  track(axis === undefined ? np.argmax(promoteBool(a)) : np.argmax(promoteBool(a), asNumber(axis)))
const TensorArgMin = (a: Value, axis?: Value) =>
  track(axis === undefined ? np.argmin(promoteBool(a)) : np.argmin(promoteBool(a), asNumber(axis)))

const TensorNormalize = (a: Value, p?: Value) => {
  const ord = p !== undefined ? asNumber(p) : 2
  return track(np.trueDivide(borrow(a), np.linalg.vectorNorm(borrow(a), { ord })))
}

const TensorNegate = unaryOp(np.negative)
const TensorAbsolute = unaryOp(np.absolute)
const TensorSign = unaryOp(np.sign)
const TensorLogarithm = unaryOp(np.log)
const TensorLog2 = unaryOp(np.log2)
const TensorLog10 = unaryOp(np.log10)
const TensorLog1Plus = unaryOp(np.log1p)      // log(1 + x), accurate near 0
const TensorExponential = unaryOp(np.exp)
const TensorExpMinus1 = unaryOp(np.expm1)     // exp(x) - 1, accurate near 0
const TensorReciprocal = unaryOp(np.reciprocal)
const TensorSquareRoot = unaryOp(np.sqrt)
const TensorSquare = unaryOp(np.square)
const TensorCubeRoot = unaryOp(np.cbrt)
const TensorRound = unaryOp(np.round)
const TensorTruncate = unaryOp(np.trunc)      // toward zero, unlike floor
const TensorCeil = unaryOp(np.ceil)
const TensorFloor = unaryOp(np.floor)
const TensorErrorFunction = unaryOp(lax.erf)
const TensorSigmoid = unaryOp(nn.sigmoid)
const TensorRelu = unaryOp(nn.relu)
const TensorClamp = (x: Value, min: Value, max: Value) =>
  track(np.clip(borrow(x), borrow(min), borrow(max)))
const TensorSoftmax = (x: Value, axis?: Value) =>
  track(nn.softmax(borrow(x), axis === undefined ? undefined : asNumber(axis)))
const TensorOneHot = (indices: Value, depth: Value) =>
  track(nn.oneHot(np.astype(borrow(indices), np.int32), asNumber(depth)))
const TensorCrossEntropy = (labels: Value, logits: Value) =>
  track(np.mean(np.negative(np.sum(np.multiply(borrow(labels), nn.logSoftmax(borrow(logits))), -1))))

const TensorSort = (x: Value) => track(np.sort(borrow(x)))
const TensorSlice = (a: Value, begin: Value, size?: Value) => {
  const beginList = asNumberList(begin)
  const sizeList = size === undefined ? undefined : asNumberList(size)
  const spec = beginList.map((b, i) => {
    const s = sizeList?.[i]
    return (s === undefined || s === -1 ? [b] : [b, b + s]) as [number] | [number, number]
  })
  return track((borrow(a) as np.Array).slice(...spec))
}
const TensorMask = (a: Value, b: Value) => {
  const count = asNumber(TensorSum(TensorBoolean(b)))
  const size = shapeOf(a)[0] ?? 0
  const indices = track(np.arange(size).astype(np.float32))
  const holes = track(np.full([size], -1))
  const mu = TensorWhere(b, indices, holes)
  const top = TensorReverse(TensorSort(mu))
  const valid = TensorSlice(top, TensorScalar(0), TensorScalar(count))
  return TensorGather(a, TensorReverse(valid))
}
const TensorFill = (shape: Value, value: Value) => {
  return track(np.full(asNumberList(shape), asNumber(value)))
}
const TensorBoolean = (a: Value) => track(np.astype(borrow(a), np.bool))

const TensorGradient = (f: Value) => {
  if (typeof f !== "function") {
    return new Error("`gradient(f)`: `f` must be a function")
  }
  return (x: Value) => {
    // vjp with a ones cotangent: scalar outputs give the classic gradient,
    // elementwise outputs get per-element derivatives – ∇({x | x^2}) works
    // on vectors like it did on TFJS
    const [out, pullback] = jaxVjp((primal: np.Array) => {
      const result = (f as Function)(primal)
      const value = result instanceof Signal ? result.peek() : result
      if (value instanceof Error) { throw value }
      if (!isTensor(value)) { throw new Error("`gradient(f)`: `f` must return a tensor") }
      return value as np.Array
    }, [borrow(x) as np.Array])
    try {
      const [dx] = pullback(np.onesLike(out))
      return track(dx)
    } finally {
      pullback.dispose()
    }
  }
}
const TensorTranspose = unaryOp(np.transpose)
const TensorIdentity = (a: Value) => {
  return track(np.eye(asNumber(a)))
}

const TensorRange = (a: Value, b?: Value) => {
  // jax-js arange is int32 and weak-typed floats truncate against it – Fluent
  // tensors are float32 throughout
  if (b === undefined) {
    return track(np.arange(Math.trunc(asNumber(a))).astype(np.float32))
  }

  const start = Math.trunc(asNumber(a))
  const stop = Math.trunc(asNumber(b))
  const step = start <= stop ? 1 : -1
  return track(np.arange(start, stop, step).astype(np.float32))
}

const TensorLinearSpace = (range: Value, steps: Value) => {
  const [start, stop] = getAsSyncList(range) as [number, number]
  // a count has to be a whole number; a live slider naturally drives it
  // through fractional values (resolution × 90 + 10), so round rather than reject
  return track(np.linspace(start, stop, Math.max(0, Math.round(asNumber(steps)))))
}

// Rank-polymorphic convolution: the kernel's rank sets the conv's rank, so the
// same conv slides a 1-D kernel over a vector or a 2-D kernel over an image.
// Backed by lax.conv – a singleton batch/channel wraps the field (NCHW), zero
// padded (SAME) so the output keeps the input's shape.
const TensorConvolution = (kernel: Value, arr: Value) => {
  const k = borrow(kernel) as np.Array
  const a = borrow(arr) as np.Array
  const ks = k.shape
  const spatial = a.shape
  const out = lax.conv(
    np.reshape(a, [1, 1, ...spatial]),
    np.reshape(k, [1, 1, ...ks]),
    spatial.map(() => 1),
    "SAME",
  )
  return track(np.reshape(out, spatial))
}

// Real FFT: a real signal in, the positive-frequency half out. Fluent has no
// complex dtype, so return the two components stacked as [2, bins] – f_0 is the
// real part, f_1 the imaginary, magnitude is √(f_0^2 + f_1^2).
const TensorFFT = (a: Value) => {
  const { real, imag } = np.fft.rfft(borrow(a))
  return track(np.stack([real, imag]))
}

// Coordinate grids: meshgrid(0::w, 0::h) → [X, Y], each broadcast to the full grid
const TensorMeshgrid = (...args: Value[]) => track(np.meshgrid(args.map(borrow)))

const TensorPad = (a: Value, width: Value) => track(np.pad(borrow(a), asNumber(width)))

const TensorRepeat = (a: Value, count: Value) => track(np.repeat(borrow(a), asNumber(count)))

const TensorSinc = unaryOp(np.sinc)

// The k largest values and their indices, as [values, indices]
const TensorTopK = (a: Value, k: Value) => {
  const [values, indices] = lax.topK(borrow(a), asNumber(k))
  return track([values, indices])
}

// Einstein summation: einsum("ij,jk->ik", a, b) is a matrix multiply
const TensorEinsum = (subscripts: Value, ...args: Value[]) =>
  track(np.einsum(String(subscripts), ...args.map(borrow)))

const TensorReshape = (a: Value, b?: Value) => {
  if (b !== undefined) {
    return track(np.reshape(borrow(a), asNumberList(b)))
  }

  return track(np.array(shapeOf(a)))
}

// Toroidal shift – APL's rotate. roll(x, 1) shifts flat; roll(x, s, axis)
// shifts along an axis, wrapping around.
const TensorRoll = (a: Value, shift: Value, axis?: Value) =>
  track(np.roll(borrow(a), getAsSyncList(shift) as number | number[],
    axis === undefined ? undefined : getAsSyncList(axis) as number | number[]))

const TensorReverse = (a: Value, axis?: Value) =>
  track(np.flip(borrow(a), axis === undefined ? undefined : getAsSyncList(axis) as number | number[]))

const TensorMatrixMultiply = binaryOp(np.matmul)
const TensorDotProduct = binaryOp(np.dot)

const TensorLength = (a: Value, b?: Value) => {
  if (b !== undefined) {
    return track(np.array(shapeOf(a)[asNumber(b)] ?? NaN))
  }

  return track(np.array(shapeOf(a)[0] ?? NaN))
}

const TensorShape = (a: Value) => track(np.array(shapeOf(a)))

const TensorGather = (a: Value, b: Value) => {
  const size = shapeOf(a)[0] ?? 0
  const raw = np.astype(borrow(b), np.int32)
  // negative indices count from the end: a_(-1) is the last element. The
  // outer cast makes the dtype strongly int32 – astype keeps weak typing, and
  // weak int32 promotes to float32 in the where().
  const wrapped = np.astype(np.where(np.less(raw.ref, 0), np.add(raw.ref, size), raw), np.int32)
  const array = borrow(a)
  if (array instanceof np.Array) {
    return track(np.take(array, wrapped, 0))
  }
  // inside grad(): jax-js gather has no transpose rule yet, so express the
  // gather as a one-hot contraction, which differentiates
  const selector = nn.oneHot(wrapped, size)
  return track(np.tensordot(selector, array, [[selector.ndim - 1], [0]]))
}

const TensorWhere = (a: Value, b: Value, c: Value) =>
  track(np.where(np.astype(borrow(a), np.bool), borrow(b), borrow(c)))
const TensorIsNaN = (a: Value) => track(np.astype(np.isnan(borrow(a)), np.float32))

const TensorVariable = (a: Value) => {
  if (!isTensor(a) && !(a instanceof FluentVariable)) {
    return new Error("`~(init)`: initial value must be a tensor")
  }
  return new FluentVariable(borrow(a) as np.Array)
}

const TensorAssign = (a: Value, b: Value) => {
  if (!(a instanceof FluentVariable)) {
    return new Error("`:=`: left side must be a variable created with ~")
  }
  if (tracingActive) { throw new TraceBailout("variable write during trace") }
  a.assign(borrow(b) as np.Array)
  return null
}

// Bridge a trainable variable into the reactive world: watch(θ) is a Signal
// that updates on every assignment (drag, optimizer step, :=)
const TensorWatch = (a: Value): Value => {
  if (!(a instanceof FluentVariable)) { return a }
  let previous: np.Array | null = null
  return computed(() => {
    a.version.value
    if (previous && previous.refCount > 0) { previous.dispose() }
    previous = a.current.ref
    return previous
  })
}

// Optimizers: optax gradient transformations driven by valueAndGrad. The loss
// thunk closes over the variables – during tracing each variable temporarily
// carries its tracer, so reads inside the thunk differentiate.
const makeOptimizer = (transform: optax.GradientTransformation, explicitVars?: FluentVariable[]) => {
  let state: optax.OptState | null = null
  let stateVars: FluentVariable[] = []
  registerDisposable(() => {
    if (state !== null) {
      tree.dispose(state)
      state = null
    }
  })

  let compiledStep: ((params: np.Array[], state: optax.OptState) => [np.Array, np.Array[], optax.OptState]) & { dispose: () => void } | null = null
  let stepMode: "probe" | "jit" | "eager" = "probe"
  const demoteStep = () => {
    compiledStep?.dispose()
    compiledStep = null
    stepMode = "eager"
  }
  registerDisposable(demoteStep)

  return (lossThunk: Value) => {
    if (typeof lossThunk !== "function") {
      return new Error("optimizer expects a loss thunk { ... }")
    }
    const vars = explicitVars ?? [...trainableVariables]
    if (vars.length === 0) {
      return new Error("no trainable variables – create one with ~(init)")
    }
    if (state !== null && (stateVars.length !== vars.length || stateVars.some((v, i) => v !== vars[i]))) {
      tree.dispose(state)
      state = null
      demoteStep()
      stepMode = "probe"
    }
    if (state === null) {
      state = transform.init(vars.map(v => v.current.ref))
      stateVars = vars
    }

    // gradient of the loss thunk: each variable temporarily carries its
    // parameter, so reads inside the thunk differentiate
    const lossFromParams = (params: np.Array[]) => {
      const saved = vars.map(v => v.current)
      vars.forEach((v, i) => { v.current = params[i]! })
      try {
        const out = (lossThunk as Function)()
        const value = out instanceof Signal ? out.peek() : out
        if (value instanceof Error) { throw value }
        if (!isTensor(value)) { throw new Error("loss must evaluate to a scalar tensor") }
        return value as np.Array
      } finally {
        vars.forEach((v, i) => { v.current = saved[i]! })
      }
    }

    const eagerStep = (): Value => {
      const [loss, grads] = valueAndGrad(lossFromParams)(vars.map(v => v.current.ref))
      const [updates, nextState] = transform.update(grads as np.Array[], state!, vars.map(v => v.current.ref))
      state = nextState
      const fresh = optax.applyUpdates(vars.map(v => v.current.ref), updates as np.Array[]) as np.Array[]
      vars.forEach((v, i) => { v.assign(fresh[i]!) })
      return track(loss)
    }

    if (stepMode === "eager") { return eagerStep() }

    // compile the whole step – gradient, transform, apply – as one program,
    // retraced automatically when parameter shapes change
    compiledStep ??= jaxJit((params: np.Array[], optState: optax.OptState) => {
      const [loss, grads] = valueAndGrad(lossFromParams)(tree.ref(params) as np.Array[])
      const [updates, nextState] = transform.update(grads as np.Array[], optState, tree.ref(params) as np.Array[])
      const fresh = optax.applyUpdates(params, updates as np.Array[]) as np.Array[]
      return [loss, fresh, nextState] as [np.Array, np.Array[], optax.OptState]
    }) as any

    const rngBefore = rngCounter
    const outerTracing = tracingActive
    const outerTouched = traceTouchedState
    tracingActive = true
    traceTouchedState = false
    try {
      // pass a referenced copy of the state tree: on a failed trace our own
      // references stay valid and the eager fallback still has its state
      const [loss, fresh, nextState] = compiledStep!(vars.map(v => v.current.ref), tree.ref(state!) as optax.OptState)
      tree.dispose(state!)
      state = nextState
      vars.forEach((v, i) => { v.assign(fresh[i]!) })
      if (traceTouchedState || rngCounter !== rngBefore) {
        // the loss froze outside state or a random draw – this step is
        // valid, but a replay would not be
        demoteStep()
      } else {
        stepMode = "jit"
      }
      return track(loss)
    } catch (e) {
      demoteStep()
      return eagerStep()
    } finally {
      tracingActive = outerTracing
      traceTouchedState = outerTouched || traceTouchedState
    }
  }
}

// optax has no adagrad – accumulate squared gradients ourselves
const adagradTransform = (learningRate: number, eps = 1e-7): optax.GradientTransformation => ({
  init: (params) => tree.map((p: np.Array) => np.zerosLike(p), params as any),
  update: (updates, state, params) => {
    if (params) { tree.dispose(params) }
    const accum = tree.map((g: np.Array, s: np.Array) => np.add(np.square(g.ref), s), updates as any, state as any)
    const scaled = tree.map(
      (g: np.Array, s: np.Array) => np.multiply(np.trueDivide(g, np.add(np.sqrt(s.ref), eps)), -learningRate),
      updates as any, tree.ref(accum) as any)
    return [scaled as any, accum]
  },
})

const TensorOptimizationAdam = (a: Value, b?: Value) =>
  makeOptimizer(optax.adam(asNumber(a)), Array.isArray(b) ? b as FluentVariable[] : undefined)

const TensorOptimizationSgd = (a: Value) => makeOptimizer(optax.sgd(asNumber(a)))

const TensorOptimizationAdaGrad = (a: Value) => makeOptimizer(adagradTransform(asNumber(a)))

// Stateful convenience RNG: a fresh Threefry key per call, mirroring the feel
// of TFJS's tf.random* – deterministic per page load, not across reloads.
let rngCounter = 0
const nextRngKey = () => random.key(rngCounter++)
const TensorRandomNormal = (a: Value) => track(random.normal(nextRngKey(), asNumberList(a)))
const TensorRandomUniform = (a: Value) => track(random.uniform(nextRngKey(), asNumberList(a)))

const StringConcat = (...args: any[]) => "".concat(...args)
const StringLength = (a: string) => track(np.array(a.length))

const Null = null


// MARK: Built-in docs
// Authored on the canonical functions themselves, so every prelude alias that
// resolves to one (⌈ → TensorMaximum, once → SignalOnce, …) inherits the same
// card: the doc lives on the value, not the name. Editor hover and completion
// read it back with getMeta. Signatures use the ergonomic notation people type.
doc(SignalOnce, "once(signal)", "Read a signal's current value without subscribing to it.", "x: $(4), once(x) + 1  →  5")
doc(Reactive, "$(value)", "Wrap a value in a signal (or a thunk in a computed signal). Read with x(), write with x(v).", "x: $(0.5), x ^ 2")
doc(TensorVariable, "~(init)", "Make a trainable variable. Assign with :=; optimise with adam / sgd / adagrad.", "θ: ~([0, 0])")
doc(TensorWatch, "watch(variable)", "A signal that updates whenever a variable is assigned – by a drag, an optimizer, or :=.", "θ: ~([2]), w: watch(θ), θ := [8], w  →  [8]")
doc(TensorGradient, "∇(f)", "Gradient of a function. ∇(f)(x) is df/dx, evaluated at x.", "∇({ x | x^2 })(3)  →  6")
doc(TensorSum, "Σ(x, axis?)", "Sum of the elements, over one axis or the whole tensor.", "Σ([1, 2, 3])  →  6")
doc(TensorMaximum, "x ⌈ y", "Element-wise maximum of two tensors.", "[1, 5] ⌈ [4, 2]  →  [4, 5]")
doc(TensorRange, "start :: stop", "Integer range from start (inclusive) to stop (exclusive).", "0 :: 5  →  [0, 1, 2, 3, 4]")
doc(TensorReshape, "x ⍴ shape", "Reshape a tensor to a new shape; one dimension may be -1 to infer it.", "[1, 2, 3, 4] ⍴ [2, 2]  →  [[1, 2], [3, 4]]")
doc(TensorOuter, "a (⊗ f) b", "Table: apply f between every cell of a and every cell of b.", "(0 :: 3) (⊗ ×) (0 :: 3)  →  [[0,0,0],[0,1,2],[0,2,4]]")
doc(TensorRoll, "roll(x, shift, axis?)", "Shift elements along an axis, wrapping around the edge (a torus).", "roll([1, 2, 3, 4], 1)  →  [4, 1, 2, 3]")
doc(TensorSort, "sort(x)", "Sort a vector into ascending order.", "sort([3, 1, 2])  →  [1, 2, 3]")
doc(TensorMask, "mask(x, keep)", "Keep the elements of x where the boolean mask is true, dropping the rest.", "mask([5, 0, 6], [5, 0, 6] > 1)  →  [5, 6]")
doc(TensorWhere, "where(cond, a, b)", "Element-wise choice: take a where cond is true, otherwise b.", "where([1, 0, 1], [1, 2, 3], 0)  →  [1, 0, 3]")


// MARK: Environment

const DefaultEnvironment: Record<string, Value> = {
  [Symbol.keyFor(Symbol.for("Null"))!]: Null,

  SymbolAssign,

  CodeParse,
  CodeEvaluate,

  FunctionIterate,
  FunctionPower,
  FunctionCascade,
  FunctionEvaluate,
  FunctionApply,
  FunctionNoAutoLift,
  FunctionGuard,
  Describe,
  doc,

  // Signals
  Reactive,
  SignalCreate,
  SignalComputed,
  SignalRead,
  SignalUpdate,
  SignalEffect,
  SignalOnce,

  // Tensor operations
  Tensor,
  TensorStack,
  TensorUnstack,
  TensorConcat,
  TensorTile,

  TensorAdd,
  TensorSubtract,
  TensorMultiply,
  TensorDivide,

  TensorPower,
  TensorRoot,
  TensorRemainder,
  TensorMaximum,
  TensorMinimum,

  TensorLess,
  TensorGreater,
  TensorLessEqual,
  TensorGreaterEqual,
  TensorEqual,
  TensorNotEqual,
  TensorSine,
  TensorCosine,
  TensorTangent,
  TensorSineInverse,
  TensorCosineInverse,
  TensorTangentInverse,
  TensorSineHyperbolic,
  TensorCosineHyperbolic,
  TensorTangentHyperbolic,
  TensorSineHyperbolicInverse,
  TensorCosineHyperbolicInverse,
  TensorTangentHyperbolicInverse,
  TensorArcTangent2,
  TensorHypotenuse,
  TensorDegToRad,
  TensorRadToDeg,

  TensorSum,
  TensorProduct,
  TensorMean,
  TensorMin,
  TensorMax,
  TensorArgMax,
  TensorArgMin,
  TensorNormalize,

  TensorNegate,
  TensorAbsolute,
  TensorSign,
  TensorLogarithm,
  TensorLog2,
  TensorLog10,
  TensorLog1Plus,
  TensorExponential,
  TensorExpMinus1,
  TensorReciprocal,
  TensorSquareRoot,
  TensorSquare,
  TensorCubeRoot,
  TensorRound,
  TensorTruncate,
  TensorCeil,
  TensorFloor,
  TensorErrorFunction,
  TensorSigmoid,
  TensorRelu,
  TensorClamp,
  TensorSoftmax,
  TensorOneHot,
  TensorCrossEntropy,
  TensorSort,

  TensorGradient,

  TensorTranspose,
  TensorRange,
  TensorLinearSpace,
  TensorConvolution,
  TensorFFT,
  TensorMeshgrid,
  TensorPad,
  TensorRepeat,
  TensorSinc,
  TensorTopK,
  TensorEinsum,
  TensorReshape,
  TensorLength,
  TensorShape,
  TensorGather,
  TensorWhere,
  TensorIsNaN,
  TensorIdentity,
  TensorMask,
  TensorSlice,
  TensorFill,
  TensorReverse,
  TensorRoll,

  TensorVariable,
  TensorAssign,
  TensorWatch,
  TensorOptimizationAdam,
  TensorOptimizationSgd,
  TensorOptimizationAdaGrad,
  TensorRandomNormal,
  TensorRandomUniform,

  TensorMatrixMultiply,
  TensorDotProduct,
  TensorOuter,

  // List operations
  List,
  ListLength,
  ListConcat,
  ListGet,
  ListMap,
  ListReduce,

  // String operations
  String,
  StringConcat,
  StringLength,

  "◌": Null,
  "null": Null,
}

// The IDE (or tests) extends the environment with UI components, live
// sources and printers before creating scopes.
//
// These come from client.tsx's top-level call, which Bun's HMR does NOT re-run
// when only language.ts is hot-swapped. Without a durable home the fresh module
// instance would have no Text/Slider/etc, so identifiers like `Text` resolve to
// an unbound Symbol.for("Text") ('Symbol(Text) is not a function'). Stashing the
// extensions on globalThis lets a re-evaluated module re-apply them, while the
// built-ins above stay fresh each eval so editing them still hot-reloads.
const persistedExtensions: Record<string, Value> =
  ((globalThis as Record<string, unknown>).__fluentEnvExtensions ??= {}) as Record<string, Value>
Object.assign(DefaultEnvironment, persistedExtensions)

const extendEnvironment = (extra: Record<string, Value>) => {
  Object.assign(persistedExtensions, extra)
  Object.assign(DefaultEnvironment, extra)
}

// Code literals evaluate through this hook so the IDE can render syntax
// trees; headless environments get the raw tree node.
let codeNodePrinter: (node: SyntaxTreeNode) => Value = (node) => node as unknown as Value
const setCodeNodePrinter = (printer: (node: SyntaxTreeNode) => Value) => {
  codeNodePrinter = printer
}

// MARK: Prelude

const PRELUDE = `
SymbolAssign(:, SymbolAssign),
(:=): TensorAssign,

; Functions
(.): FunctionApply,
apply: FunctionApply,
(⟳): FunctionIterate,
iter: FunctionIterate,
(⍣): FunctionPower,
(@): FunctionEvaluate,
eval: FunctionEvaluate,
cascade: FunctionCascade,
guard: FunctionGuard,

; Tensor shape/indexing
(#): TensorLength,
length: TensorLength,
len: TensorLength,
(_): TensorGather,
gather: TensorGather,
(⍴): TensorReshape,
reshape: TensorReshape,
(::): TensorRange,
range: TensorRange,
shape: TensorShape,
slice: TensorSlice,
transpose: TensorTranspose,
reverse: TensorReverse,
(⊗): TensorOuter,
outer: TensorOuter,

; Shape manipulation
flat: { x | x ⍴ [-1] },
squeeze: { x |
  s: shape(x),
  newShape: mask(s, (s ≠ 1)),
  x ⍴ newShape
},
unsqueeze: { x, axis |
  s: shape(x),
  newShape: concat(concat(slice(s, 0, axis), [1]), slice(s, axis)),
  x ⍴ newShape
},

windows: { w, arr |
  starts: (0 :: (#(arr) - w + 1)),
  offsets: (0 :: w),
  indices: (starts ⊗(+) offsets),
  arr _ indices
},

chunks: { w, arr |
  n: (#(arr) / w),
  starts: (0 :: n × w),
  offsets: (0 :: w),
  indices: (starts ⊗(+) offsets),
  arr _ indices
},

stencil: { w, f, arr | unstack(windows(w, arr)) ListMap f . stack },
conv: TensorConvolution,   ; nD convolution – a 1D kernel over a vector, a 2D kernel over an image

; List operations
ListGather: { a, b |
  ListMap(b, { i | ListGet(a, i) })
},
ListZip: { a, b |
  n: TensorMin(ListLength(a), ListLength(b)),
  ListMap(
    TensorUnstack(TensorRange(0, n)),
    { i | List(ListGet(a, i), ListGet(b, i)) }
  )
},
ListTake: { list, n |
  ListMap(
    TensorUnstack(TensorRange(0, n)),
    { i | ListGet(list, i) }
  )
},
ListDrop: { list, n |
  len: ListLength(list),
  ListMap(
    TensorUnstack(TensorRange(n, len)),
    { i | ListGet(list, i) }
  )
},
ListReverse: { list |
  n: ListLength(list),
  ListMap(
    TensorUnstack(TensorReverse(TensorRange(0, n))),
    { i | ListGet(list, i) }
  )
},
ListEnumerate: { list |
  n: ListLength(list),
  ListMap(
    TensorUnstack(TensorRange(0, n)),
    { i | List(i, ListGet(list, i)) }
  )
},
ListScan: { list, f, init |
  ListReduce(
    list,
    { acc, val |
      prev: ListGet(acc, -1),
      ListConcat(acc, List(f(prev, val)))
    },
    List(init)
  ),
},

; Arithmetic
(/): TensorDivide,
(÷): TensorDivide,
div: TensorDivide,
(%): TensorRemainder,
mod: TensorRemainder,
(^): TensorPower,
pow: TensorPower,
(√): TensorRoot,
root: TensorRoot,
add: TensorAdd,
sub: TensorSubtract,
mul: TensorMultiply,
(+): FunctionCascade((TensorAdd, TensorAbsolute)),
(-): FunctionCascade((TensorSubtract, TensorNegate)),
(*): FunctionCascade((TensorMultiply, TensorSign)),
(×): FunctionCascade((TensorMultiply, TensorSign)),
(·): FunctionCascade((TensorMultiply, TensorSign)),

; Math
neg: TensorNegate,
abs: TensorAbsolute,
sign: TensorSign,
round: TensorRound,
trunc: TensorTruncate,
floor: TensorFloor,
ceil: TensorCeil,
reciprocal: TensorReciprocal,
sqrt: TensorSquareRoot,
square: TensorSquare,
cbrt: TensorCubeRoot,
hypot: TensorHypotenuse,
log: TensorLogarithm,
log2: TensorLog2,
log10: TensorLog10,
log1p: TensorLog1Plus,
exp: TensorExponential,
expm1: TensorExpMinus1,
clamp: TensorClamp,
sigmoid: TensorSigmoid,
relu: TensorRelu,
softmax: TensorSoftmax,
oneHot: TensorOneHot,
crossEntropy: TensorCrossEntropy,

; Trigonometry
sin: TensorSine,
cos: TensorCosine,
tan: TensorTangent,
asin: TensorSineInverse,
acos: TensorCosineInverse,
atan: TensorTangentInverse,
arcsin: TensorSineInverse,
arccos: TensorCosineInverse,
arctan: TensorTangentInverse,
atan2: TensorArcTangent2,
arctan2: TensorArcTangent2,
sinh: TensorSineHyperbolic,
cosh: TensorCosineHyperbolic,
tanh: TensorTangentHyperbolic,
asinh: TensorSineHyperbolicInverse,
acosh: TensorCosineHyperbolicInverse,
atanh: TensorTangentHyperbolicInverse,
arcsinh: TensorSineHyperbolicInverse,
arccosh: TensorCosineHyperbolicInverse,
arctanh: TensorTangentHyperbolicInverse,
deg2rad: TensorDegToRad,
rad2deg: TensorRadToDeg,

; Comparison
(<): TensorLess,
less: TensorLess,
lt: TensorLess,
(>): TensorGreater,
greater: TensorGreater,
gt: TensorGreater,
(≤): TensorLessEqual,
(<=): TensorLessEqual,
lessEqual: TensorLessEqual,
le: TensorLessEqual,
(≥): TensorGreaterEqual,
(>=): TensorGreaterEqual,
greaterEqual: TensorGreaterEqual,
ge: TensorGreaterEqual,
(=): TensorEqual,
equal: TensorEqual,
(≠): TensorNotEqual,
(!=): TensorNotEqual,
notEqual: TensorNotEqual,

; Reductions
(∇): TensorGradient,
gradient: TensorGradient,
(Σ): TensorSum,
sum: TensorSum,
(Π): TensorProduct,
prod: TensorProduct,
(μ): TensorMean,
mean: TensorMean,

; Min/max
(⌈): TensorMaximum,
(⌊): TensorMinimum,
max: FunctionCascade((TensorMaximum, TensorMax)),
min: FunctionCascade((TensorMinimum, TensorMin)),
argmax: TensorArgMax,
argmin: TensorArgMin,

; Variables
(~): TensorVariable,
var: TensorVariable,
watch: TensorWatch,

; Tensor ops
sort: TensorSort,
roll: TensorRoll,
flip: TensorReverse,
mask: TensorMask,
where: TensorWhere,
isNaN: TensorIsNaN,
eye: TensorIdentity,
dot: TensorDotProduct,
matmul: TensorMatrixMultiply,
fft: TensorFFT,
meshgrid: TensorMeshgrid,
pad: TensorPad,
repeat: TensorRepeat,
sinc: TensorSinc,
topk: TensorTopK,
einsum: TensorEinsum,

; Creation
rand: TensorRandomUniform,
randn: TensorRandomNormal,
linspace: TensorLinearSpace,
fill: TensorFill,
stack: TensorStack,
unstack: TensorUnstack,
concat: TensorConcat,
tile: TensorTile,

; Optimization
adam: TensorOptimizationAdam,
sgd: TensorOptimizationSgd,
adagrad: TensorOptimizationAdaGrad,

; Misc
($): Reactive,
; ← is defined here in the prelude, so its doc lives here too
(←): doc(FunctionNoAutoLift({ s, v | s(v) }), "signal ← value", "Write a value into a signal – same as signal(value), but reads left-to-right.", "x: $(1), x ← 9, x()  →  9"),
once: SignalOnce,
`

// Parse the prelude once – createScope runs on every evaluation (each keystroke)
const PRELUDE_TREE = CodeParse(PRELUDE)

const createScope = () => {
  const scope = Object.create(DefaultEnvironment)
  evaluateSyntaxTreeNode(PRELUDE_TREE, scope)
  return scope
}


// MARK: Exports

export {
  // tensors
  np,
  FluentVariable,
  isTensor,
  borrow,
  track,
  arena,
  TensorScalar,
  TensorScalarLive,
  // parse
  CodeParse,
  getParseErrors,
  RESERVED_SYMBOLS,
  identifierRegexp,
  numberRegexp,
  stringRegexp,
  operatorRegexp,
  delimiterRegexp,
  getLocationOrigin,
  // evaluate
  evaluateSyntaxTreeNode,
  evaluateProgramWithScope,
  CodeEvaluate,
  safeApply,
  reify,
  getAsSyncList,
  getMeta,
  setMeta,
  setOrigin,
  getOrigin,
  // generations
  evaluateGeneration,
  registerDisposable,
  disposeScopeTensors,
  disposeValueTensors,
  // environment
  DefaultEnvironment,
  extendEnvironment,
  setCodeNodePrinter,
  PRELUDE,
  createScope,
  // signals
  Reactive,
  SignalCreate,
  SignalComputed,
  SignalRead,
  SignalUpdate,
  SignalEffect,
  SignalOnce,
}

export type { Value, CurrentScope, SyntaxTreeNode, Origin, ParseError, FunctionMeta }
