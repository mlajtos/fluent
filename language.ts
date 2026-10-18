// MARK: Fluent language core
// Grammar, evaluator, environment and prelude – no DOM, no React, no Monaco.
// The evaluation worker runs it (host.ts), and the end-to-end tests run it headless
// (tests.ts). Whoever computes starts the engine.

import { grammar, type ActionDict } from "ohm-js"
import { toAST as ohmToAST } from "ohm-js/extras"
import { signal, Signal, computed, effect, untracked } from "@preact/signals-core"
import dedent from "ts-dedent"
import * as engine from "./engine/tensor.ts"
import PRELUDE from "./prelude.fl" with { type: "text" }
import { OPERATOR_RANGES, IDENTIFIER_RANGES } from "./protocol"

const isTensor = (v: unknown): v is engine.Tensor => v instanceof engine.Tensor

// MARK: Grammar
// The characters operators and names are made of live in protocol.ts, shared with the
// editor's highlighting, so the two never drift apart.
const ohmRanges = (ranges: Record<string, [string, string]>): string =>
  `(${Object.values(ranges).map(([lo, hi]) => `"${lo}".."${hi}"`).join(" | ")})`

const CodeGrammar = String.raw`
Fluent {
  Program
    = ListOf<Expr, ","> ","?

  Expr
    = Operation

  // Every operator but a glued one lives here; the space around it picks its tier:
  //  · glued-LEFT "a op· b" is the loosest. It splits: the whole chain on its left, the
  //    whole rest on its right, right-associatively – "a: 1 + 2"; "1 + 2* 3 + 4" is
  //    "(1 + 2) * (3 + 4)". The operator is any Atom: "1add 2 * 3" is add(1, 2*3).
  //  · spaced "a op b" (and an inert glued-RIGHT "a ·op b") chains left to right.
  //  · a leading operator with no left operand is prefix "-a".
  Operation
    = (Operation | TightOperation) #(~space) Atom #(space) Expr --gluedSplit
    | (Operation | TightOperation) TightOperation TightOperation --infix
    | TightOperation TightOperation --prefix
    | TightOperation

  // glued operators bind tighter than spaced ones – "1 + 2*3" is "1 + (2 * 3)"
  TightOperation
    = TightOperation #(~space) operator #(~space) Application --infix
    | Application

  // application binds tightest – "f(x)", "∇(f)(x)". A "(...)" glued to the right of
  // anything is its argument list: the parser has no types, so "x(y)" reads one way
  // whatever x is. A parenthesized function used infix is spaced, "1 (add) 2"; the glued
  // forms are "1add 2" and "1+ 2".
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
    | "(" ")" --empty

  Number          = number
  Tensor          = "[" ListOf<Expr, ","> ","? "]"
  Null            = "◌"
  Symbol          = identifier | operator
  // identifier owns the name symbols (math alphanumerics, emoji), so they glue like
  // names in every slot: "1🔥 2 * 3" is 🔥(1, 2 * 3)
  identifier      = &(letter | idRanges) (alnum | idRanges)+
  idRanges        = ${ohmRanges(IDENTIFIER_RANGES)}
  number
    = "-"? digitGroup ("." digitGroup)? exponent?
  exponent
    = ("e" | "E") ("+" | "-")? digitGroup
  digitGroup
    = digit+
  String          = #("\"" (stringEscape | ~"\"" any)* "\"")
  stringEscape    = "\\" any
  Code            = #("${"`"}" (~"${"`"}" any)* "${"`"}")
  reserved        = "|" | "," | "{" | "}" | "(" | ")" | "[" | "]" | ";" | "\"" | "${"`"}"
  operator        = (#(~(reserved | alnum) specialChar))+
  specialChar     = ${ohmRanges(OPERATOR_RANGES)}
  space          += ";" (~"\n" any)* -- comment
}
`

const CodeGrammarCompiled = grammar(CodeGrammar)

type Origin = {
  source: string
  start: { line: number, column: number }
  end: { line: number, column: number }
  // The whole program the positions index into. Several documents can be live at once
  // (the playground program, each tour or REPL cell); the IDE routes a hover-highlight
  // to the editor whose text is this document.
  document?: string
}

type SyntaxTreeNode =
  | { type: "Program", content: SyntaxTreeNode[], origin: Origin }
  | { type: "Operation", content: { operator: SyntaxTreeNode, args: SyntaxTreeNode & { type: "List" } }, origin: Origin }
  | { type: "Symbol", content: { value: string }, origin: Origin }
  | { type: "Number", content: { value: number }, origin: Origin }
  | { type: "Tensor", content: { value: SyntaxTreeNode[] }, origin: Origin }
  | { type: "List", content: { value: SyntaxTreeNode[], synthetic?: boolean }, origin: Origin }
  | { type: "Lambda", content: { args: SyntaxTreeNode[], expr: SyntaxTreeNode }, origin: Origin }
  | { type: "String", content: { value: string }, origin: Origin }
  | { type: "Error", content: string, origin: Origin }
  | { type: "Code", content: { value: SyntaxTreeNode }, origin: Origin }

// 1-based line and column of an offset, as ohm counts them (a carriage return takes no
// column). The parser asks twice per node, so both come from binary searches over the
// document's line starts and carriage returns, found once per document.
let lines = { text: "", starts: [0], returns: [] as number[] }
// how many of the sorted xs are below x
const below = (xs: number[], x: number): number => {
  let lo = 0, hi = xs.length
  while (lo < hi) { const mid = (lo + hi) >> 1; if (xs[mid]! < x) { lo = mid + 1 } else { hi = mid } }
  return lo
}
const lineAndColumn = (text: string, offset: number): { line: number, column: number } => {
  if (lines.text !== text) {
    lines = { text, starts: [0], returns: [] }
    for (let i = 0; i < text.length; i++) {
      if (text[i] === "\n") { lines.starts.push(i + 1) } else if (text[i] === "\r") { lines.returns.push(i) }
    }
  }
  const { starts, returns } = lines, line = below(starts, offset + 1), start = starts[line - 1]!
  return { line, column: offset - start + 1 - (below(returns, offset) - below(returns, start)) }
}

const originOf = (node: any): Origin => {
  const document: string = node.source.sourceString
  return {
    source: node.source.contents,
    document,
    start: lineAndColumn(document, node.source.startIdx),
    end: lineAndColumn(document, node.source.endIdx),
  }
}

// A backtick literal's inner program is parsed on its own, so its origins count from
// the inner text. This moves them into the outer document – (startLine, startColumn) is
// where the inner text begins; only its first line is offset by the column.
const shiftOrigins = (node: any, startLine: number, startColumn: number, document: string): void => {
  if (!node || typeof node !== "object") { return }
  if (node.origin?.start && node.origin?.end) {
    const map = (p: { line: number, column: number }) =>
      ({ line: startLine + p.line - 1, column: p.line === 1 ? startColumn + p.column - 1 : p.column })
    node.origin = { ...node.origin, document, start: map(node.origin.start), end: map(node.origin.end) }
  }
  for (const key in node) {
    if (key !== "origin") { [node[key]].flat().forEach((c: any) => shiftOrigins(c, startLine, startColumn, document)) }
  }
}

const toAST = (node: any, action: any): any => node.toAST(action.args.mapping)
const leaf = (type: string, value: unknown, action: any): SyntaxTreeNode =>
  ({ type, content: { value }, origin: originOf(action) }) as SyntaxTreeNode
// An infix application. Its operands are not an explicit ( … ), so they don't scope
// their bindings.
const infix = (action: any, left: any, operator: SyntaxTreeNode, right: any): SyntaxTreeNode => ({
  type: "Operation",
  content: {
    operator,
    args: { type: "List", origin: originOf(action), content: { value: [toAST(left, action), toAST(right, action)], synthetic: true } },
  },
  origin: originOf(action),
})
// f(x), f(x, y) and the prefix -x: a bare argument becomes a one-element list
function apply(this: any, left: any, right: any): SyntaxTreeNode {
  const arg: SyntaxTreeNode = toAST(right, this)
  const args = (arg.type === "List" ? arg : { type: "List", origin: originOf(right), content: { value: [arg] } }) as SyntaxTreeNode & { type: "List" }
  return { type: "Operation", content: { operator: toAST(left, this), args }, origin: originOf(this) }
}

const syntaxTreeMapping: ActionDict<SyntaxTreeNode> = {
  Program(exprs, _) { return { type: "Program", content: toAST(exprs, this), origin: originOf(this) } },
  // a glued or spaced operator keeps its own node – a Symbol, a Lambda, any atom
  Operation_gluedSplit(left, operator, _, right) { return infix(this, left, toAST(operator, this), right) },
  Operation_infix(left, operator, right) { return infix(this, left, toAST(operator, this), right) },
  TightOperation_infix(left, operator, right) {
    return infix(this, left, { type: "Symbol", content: { value: operator.sourceString }, origin: originOf(operator) }, right)
  },
  Application_apply: apply,
  Operation_prefix: apply,
  List_multi(_, exprs, __, ___) { return leaf("List", toAST(exprs, this), this) },
  List_empty(_, __) { return leaf("List", [], this) },
  Symbol(value) { return leaf("Symbol", value.sourceString, this) },
  Number(value) { return leaf("Number", +value.sourceString, this) },
  Tensor(_, exprs, __, ___) { return leaf("Tensor", toAST(exprs, this), this) },
  Lambda(_, args, __, expr, ___) {
    return { type: "Lambda", content: { args: toAST(args, this) ?? [], expr: toAST(expr, this) }, origin: originOf(this) }
  },
  String(_, value, __) { return leaf("String", value.sourceString, this) },
  // The raw text between the backticks parses as its own program – matched as raw
  // characters, like a string, so `a`, `b` stay two literals – and its origins move
  // into the outer document.
  Code(_, value, __) {
    const inner = CodeParse(value.sourceString)
    const at = originOf(value).start
    shiftOrigins(inner, at.line, at.column, this.source.sourceString)
    return { type: "Code", content: { value: inner }, origin: originOf(this) }
  },
}

type ParseError = {
  message: string
  start: { line: number, column: number }
  end: { line: number, column: number }
}
type Match = ReturnType<typeof CodeGrammarCompiled.match>

// Ohm's rightmost-failure message lists every alternative at the failure point – here
// whole glyph ranges ("⚀".."⚅", …). A short list helps ("expected \")\""); a long one
// collapses to a summary.
const failureMessage = (match: Match): string => {
  const message = match.shortMessage ?? match.message ?? "Syntax error"
  const at = message.lastIndexOf("expected ")
  if (at < 0 || message.slice(at + "expected ".length).split(", ").length <= 4) { return message }
  return message.slice(0, at) + "expected an expression"
}
// getRightmostFailurePosition exists, but not in ohm's types
const failureAt = (match: Match): number => (match as unknown as { getRightmostFailurePosition(): number }).getRightmostFailurePosition()

const CodeParse = (program: string): SyntaxTreeNode => {
  const text = String(program), match = CodeGrammarCompiled.match(text)
  if (match.succeeded()) { return ohmToAST(match, syntaxTreeMapping) as SyntaxTreeNode }
  const start = lineAndColumn(text, failureAt(match))
  return {
    type: "Error",
    content: failureMessage(match),
    origin: { source: text, document: text, start, end: { line: start.line, column: start.column + 1 } },
  }
}

// The parse error for editor markers: from the failure to the end of the text
const getParseErrors = (program: string): ParseError[] => {
  const text = String(program), match = CodeGrammarCompiled.match(text)
  if (match.succeeded()) { return [] }
  // a failure at the start of a line shows at the end of the line before
  const at = failureAt(match), from = at > 0 && text[at - 1] === "\n" ? at - 1 : at
  return [{ message: failureMessage(match), start: lineAndColumn(text, from), end: lineAndColumn(text, text.length) }]
}

// MARK: Evaluate

type Value = engine.Tensor | FluentVariable | Function | Signal<Value> | Error | string | String | symbol | null | Value[]
type CurrentScope = Record<string, Value>

// What a function says about itself: its card, and how it takes its arguments
interface FunctionMeta {
  doc?: string
  signature?: string
  example?: string
  noAutoLift?: boolean
  quotedArgs?: number[]
}
const NO_META: FunctionMeta = Object.freeze({})
const getMeta = (fn: Function): FunctionMeta => (fn as any).__meta__ ?? NO_META
const setMeta = (fn: Function, updates: Partial<FunctionMeta>): void => { (fn as any).__meta__ = { ...getMeta(fn), ...updates } }

// Where a value came from in the source. A WeakMap, so a function or a frozen value
// has one too – a composed closure prints the source that made it.
const originMap = new WeakMap<object, Origin>()
const isObject = (v: unknown): v is object => v !== null && (typeof v === "object" || typeof v === "function")
const setOrigin = (value: unknown, origin: Origin): void => { if (isObject(value)) { originMap.set(value, origin) } }
const getOrigin = (value: unknown): Origin | undefined => isObject(value) ? originMap.get(value) : undefined
const located = <T,>(value: T, node: { origin: Origin }): T => { setOrigin(value, node.origin); return value }

const asError = (e: unknown): Error => e instanceof Error ? e : new Error(String(e))

function evaluateSyntaxTreeNode(node: SyntaxTreeNode, env: CurrentScope): Value {
  switch (node.type) {
    case "Program": {
      let last: Value = null
      for (const e of node.content) { last = evaluateSyntaxTreeNode(e, env) }
      return last ?? null
    }
    case "Symbol": return resolve(node.content.value, env)
    case "Number": return located(engine.tensor(node.content.value), node)
    // a backslash escapes the next character: a string can hold \" and \\
    case "String": return located(new String(dedent(node.content.value).replace(/\\([\s\S])/g, "$1")), node)
    case "Tensor": {
      // a binding inside [ … ] is local to it
      const local = Object.create(env) as CurrentScope
      const values = node.content.value.map((e) => evaluateSyntaxTreeNode(e, local))
      return located(values.length ? safeApply(TensorStack, values, local) : engine.tensor([]), node)
    }
    case "List": {
      // A binding inside ( … ) is local to it; the elements still read outer names.
      // KNOWN WART: a lone (a: 2) is grouping, not a list, so its binding is not local;
      // the intended fix is the unit axiom (x) ≡ x – a singleton scopes, then demotes.
      const local = Object.create(env) as CurrentScope
      return located(node.content.value.map((e) => evaluateSyntaxTreeNode(e, local)), node)
    }
    case "Lambda": {
      const params = node.content.args.map((arg) => (arg as { content: { value: string } }).content.value)
      const fn = function (...args: Value[]) {
        const local = Object.create(env) as CurrentScope
        // a missing argument is ◌, an argument not given
        params.forEach((p, i) => { local[p] = args[i] ?? null })
        return evaluateSyntaxTreeNode(node.content.expr, local)
      }
      fn.toString = () => node.origin.source
      return located(fn, node)
    }
    case "Operation": {
      const fn = evaluateSyntaxTreeNode(node.content.operator, env)
      // A binding among a call's arguments, f(a: 2, b), is local to the call; infix
      // operands are not an explicit ( … ), so a: b: 23 binds both. The arguments run in
      // reading order; a quoted one – the name `:` binds – stays the name it spells.
      const argsEnv = (node.content.args.content.synthetic ? env : Object.create(env)) as CurrentScope
      const quoted = typeof fn === "function" ? getMeta(fn).quotedArgs : undefined
      const args = node.content.args.content.value.map((arg, i) =>
        quoted?.includes(i) && arg.type === "Symbol" ? Symbol.for(arg.content.value) : evaluateSyntaxTreeNode(arg, argsEnv))
      const value = safeApply(fn, args, env)
      // a value that has an origin keeps it: an element ListGet returns points at itself
      return getOrigin(value) ? value : located(value, node)
    }
    case "Code": {
      // the printer is IDE code (the AST viz): a bug there is an Error value, not an abort
      let value: Value
      try { value = codeNodePrinter(node.content.value) } catch (e) { value = asError(e) }
      return located(value, node)
    }
    case "Error": return located(new Error(node.content), node)
    default: return null // ◌
  }
}

engine.Tensor.prototype.toString = function () {
  return this.size <= 32 ? `${getAsSyncList(this)}` : `Tensor [${this.shape.join(" ")}]`
}
// `${x}` and String(x) print a tensor; a scalar asked for a number gives its value
;(engine.Tensor.prototype as any)[Symbol.toPrimitive] = function (hint: string) {
  return hint === "number" && this.ndim === 0 ? this.item() : this.toString()
}

// A name looked up in the scope chain – `in`, so a name bound to ◌ is null. An unbound
// name is its symbol, a value until arithmetic reaches it. A name bound to an unbound
// name's symbol looks that name up in turn.
const resolve = (name: string, env: CurrentScope): Value => {
  const v = name in env ? env[name] ?? null : Symbol.for(name)
  const next = typeof v === "symbol" ? Symbol.keyFor(v) : undefined
  return next !== undefined && next !== name ? resolve(next, env) : v
}

// MARK: Values
// A tensor is an engine value: an immutable graph node, computed when something reads it,
// its buffers owned by the engine. What crosses time – a variable's value, a signal's –
// is settled into a leaf, so it never drags its history along.

// A differentiated body can't change state: ∇ would have to undo the write along every
// direction it takes. A write while differentiating throws this; ∇ and the optimizer
// turn it into an Error value. It is control flow, so choosing passes it through.
class StateWrite extends Error {}
let differentiating = false

const isTensorish = (v: Value): boolean => isTensor(v) || v instanceof FluentVariable
const isText = (v: Value): v is string | String => typeof v === "string" || v instanceof String

// An op's operand: a variable's value or a tensor. Anything else fails here, with the
// name or the fix in the message.
const operand = (v: any): any => {
  if (v instanceof FluentVariable) { return v.current }
  if (v == null) { throw new Error("expected a tensor, got ◌") }
  if (v instanceof Signal) { throw new Error("expected a tensor, got a signal – read its value with s()") }
  // an unbound name reached a numeric op (1 * x); as a value elsewhere it flows through
  if (typeof v === "symbol") { throw new Error(`unknown name: ${Symbol.keyFor(v) ?? String(v)}`) }
  // a list is a structure, not a bag of numbers – never coerced into a tensor
  if (Array.isArray(v)) { throw new Error(`expected a tensor, got a list of ${v.length} – write it as a tensor [ … ]`) }
  // the Error itself becomes the cause (safeApply), so the chain still says why
  if (v instanceof Error) { throw new Error("expected a tensor, got an Error") }
  return v
}

const shapeOf = (v: Value): number[] => (v instanceof FluentVariable ? v.current : (v as engine.Tensor)).shape

// A scalar is a rank-0 tensor holding a single phantom item (#(5) = 1), so index, sort
// and softmax on it give the rank-0 identity, not an error about an axis it lacks.
const isScalarTensor = (v: Value): boolean => isTensorish(v) && shapeOf(v).length === 0

// While ∇ follows a loss's reads, reading a ~ variable hands back the value ∇
// differentiates with respect to – set by gradientOfReads, null otherwise
let readTracker: ((v: FluentVariable, value: engine.Tensor) => engine.Tensor) | null = null

// Trainable variable: its current value, settled into a leaf; an assignment (drag,
// optimizer step, :=) settles the new one and bumps the version signal.
class FluentVariable {
  #value: engine.Tensor
  readonly version = signal(0)
  constructor(initial: engine.Tensor) { this.#value = engine.settle(initial) }
  get current(): engine.Tensor { return readTracker && !(this instanceof FluentData) ? readTracker(this, this.#value) : this.#value }
  get shape() { return this.#value.shape }
  get size() { return this.#value.size }
  assign(next: engine.Tensor): void {
    this.#value = engine.settle(next)
    this.version.value++
  }
  toString() { return this.#value.toString() }
}

// Data slot (~~): a mutable tensor read like a constant – no gradient flows into it,
// so no optimizer trains it. Gradients flow into ~, never into ~~.
class FluentData extends FluentVariable {}

// MARK: Evaluation generations
// Each top-level evaluation owns what it starts – live sources (Time, Camera, …), ⟳
// loops, effects. The next evaluation retires it: its loops stop, its sources let go of
// their hardware. Its effects stop once the new UI has replaced the old – a still-shown
// old island would recompute against a stopped source – so retirement waits for the next
// frame, or a macrotask headless; tests and the host can flush at once.
type Generation = { disposables: Set<() => void> }
let currentGeneration: Generation | null = null
const registerDisposable = (dispose: () => void) => { currentGeneration?.disposables.add(dispose) }

const pendingRetirements = new Set<Generation>()
const flushRetirements = () => {
  for (const g of pendingRetirements) {
    for (const dispose of g.disposables) { try { dispose() } catch { /* already gone */ } }
  }
  pendingRetirements.clear()
}

// the next display frame, or a macrotask where there is no display
const nextFrame = (cb: (t: number) => void): void => {
  if (typeof globalThis.requestAnimationFrame === "function") { globalThis.requestAnimationFrame(cb) } else { setTimeout(() => cb(performance.now()), 0) }
}
let retirementFlushScheduled = false
const scheduleRetirementFlush = () => {
  if (retirementFlushScheduled) { return }
  retirementFlushScheduled = true
  nextFrame(() => { retirementFlushScheduled = false; flushRetirements() })
}

function evaluateGeneration<T>(evaluate: () => T): T {
  const previous = currentGeneration
  currentGeneration = { disposables: new Set() }
  // every run draws the same random numbers, so the same program trains the same weights
  rngCounter = 0
  try {
    return evaluate()
  } finally {
    // the previous generation retires even if this one threw, so nothing of it leaks
    if (previous) {
      pendingRetirements.add(previous)
      scheduleRetirementFlush()
    }
  }
}

// MARK: Applying
// Signals may hide inside lists, so concat((a, b), axis) stays reactive when a, b are
// computed. A signal may hold a signal – a result still on its way (CodesToString) – so
// a lifted application reads through to the value and recomputes when either changes.
const containsSignal = (a: Value): boolean => a instanceof Signal || (Array.isArray(a) && a.some(containsSignal))
const unwrapSignals = (a: any): any => a instanceof Signal ? unwrapSignals(a.value) : Array.isArray(a) ? a.map(unwrapSignals) : a

// The one way a function is applied. A signal is called to read it, s(), or to write it,
// s(v). An application whose arguments hold signals is a computed that recomputes with
// their current values, unless the function takes signals as they are (noAutoLift); the
// engine fuses whatever a chain of computeds builds, at the read. A throw is an Error
// value, caused by an Error among the arguments when there is one; inside a computed it
// also names the application, since there is no callsite to point at.
// KNOWN WART: arity is not checked, so +(1, 2, 4) is 3 – see arity-plan.md.
function safeApply(fn: Value, args: Value[], env: CurrentScope): Value {
  try {
    if (fn instanceof Signal) {
      if (args.length === 0) { return fn.value }
      if (args.length === 1) { return SignalUpdate(fn, args[0]!) }
      throw new Error("What are you trying to do with this poor signal?")
    }
    if (typeof fn !== "function") { throw new Error(`'${String(fn)}' is not a function.`) }
    if (!getMeta(fn).noAutoLift && args.some(containsSignal)) {
      return computed(() => {
        const unwrapped = args.map(unwrapSignals)
        try {
          return fn.apply(env, unwrapped)
        } catch (e) {
          if (e instanceof StateWrite) { throw e }
          const label = fn.name || getOrigin(fn)?.source || "a function"
          return new Error(`applying ${label} to (${unwrapped.map(describeArg).join(", ")})`, { cause: asError(e) })
        }
      })
    }
    return fn.apply(env, args)
  } catch (e) {
    // a state write while differentiating is control flow, not a value: as an Error,
    // a choice would read it as a candidate declining
    if (e instanceof StateWrite) { throw e }
    const cause = args.find((a) => a instanceof Error)
    return e instanceof Error && cause ? new Error(e.message, { cause }) : asError(e)
  }
}

// MARK: Choosing
// Errors are values: an Error is a result that says "there is no answer here".
// One kind of Error is polite – a NoMatch says "not my case, ask someone else".
//
// A CANDIDATE is a function that answers or declines; it declines by returning a NoMatch.
// guard(c, { v }) declines unless c holds; try({ v }) declines when v fails. cascade and
// cond take the first candidate that answers. A failure IS an answer: it stops the
// choice and surfaces, so a matched branch that breaks is never reread as "not me"
// while a later branch quietly wins. When every candidate declines, the choice declines
// too – so choices nest, and an overload set is a candidate in a bigger one.
class NoMatch extends Error {
  constructor(message: string, options?: { cause?: unknown }) {
    super(message, options)
    this.name = "NoMatch"
  }
}

// The NoMatch a hand-written candidate returns to decline.
const NoMatchValue = new NoMatch("no match")

// The first answer of the candidates, in order. A candidate that isn't a function is its
// own result, so an Error among them (a guard whose condition broke) surfaces and a
// NoMatch declines. A throw is a failure like any other; a native candidate declines by
// RETURNING a NoMatch. When none answers, the last decline is the cause – it says why
// the closest candidate said no.
const chooseFirst = (candidates: Value[], args: Value[], scope: CurrentScope, declined: () => string): Value => {
  let lastDecline: NoMatch | null = null
  for (const candidate of candidates) {
    let result: Value
    try {
      result = typeof candidate === "function" ? candidate.apply(scope, args) : candidate
    } catch (e) {
      if (e instanceof StateWrite) { throw e }
      return asError(e)
    }
    if (!(result instanceof NoMatch)) { return result }
    lastDecline = result
  }
  return new NoMatch(declined(), lastDecline ? { cause: lastDecline } : undefined)
}

const FunctionCascade = (candidates: Value) => {
  if (!Array.isArray(candidates)) {
    return new Error(`\`cascade\`: expected a list of candidates, got ${describeArg(candidates)} – write cascade((f, g, …))`)
  }
  return tacitToString(function (this: CurrentScope, ...args: Value[]) {
    return chooseFirst(candidates, args, this,
      () => `no overload matched (${args.length === 0 ? "no arguments" : args.map(describeArg).join(", ")})`)
  })
}

// cond(a, b, …): cascade((a, b, …))() without the wrapper – its own entry point, so a
// recursion through it pays no setup per call
const FunctionCond = function (this: CurrentScope, ...candidates: Value[]): Value {
  return chooseFirst(candidates, [], this, () => "cond: no branch matched")
}

// reduce's fast path: a binary op with a native reduction, so `reduce +` runs engine.sum
// rather than folding slice by slice. FunctionArity passes it on to its dispatchers.
const nativeReducer = new WeakMap<Function, (x: Value, axis?: Value) => Value>()

// Overloaded by arity: two operands run the binary op, one runs the unary op – how + is
// add and abs. Unlike a cascade, a binary op that fails (1 + a, with a unknown) is not
// retried as the unary one, so it is an Error, not a quiet abs(1).
const FunctionArity = (candidates: Function[]) => {
  const [binary, unary] = candidates
  const dispatch = tacitToString((a: Value, b: Value) => (b == null ? unary?.(a) : binary?.(a, b)))
  // its card is its candidates': `+` shows add and abs in one hover; a doc() on the
  // dispatcher afterwards still wins
  const [b, u] = [binary, unary].map((f): FunctionMeta => typeof f === "function" ? getMeta(f) : {}) as [FunctionMeta, FunctionMeta]
  if (b.signature || u.signature) {
    setMeta(dispatch, {
      signature: [b.signature, u.signature].filter(Boolean).join("  ·  "),
      doc: [b.doc, u.doc && `With one argument: ${u.doc}`].filter(Boolean).join(" "),
      example: [b.example, u.example].filter(Boolean).join("\n") || undefined,
    })
  }
  const fast = typeof binary === "function" && nativeReducer.get(binary)
  if (fast) { nativeReducer.set(dispatch, fast) }
  return dispatch
}

// A tensor's data as nested JS arrays (a number for a scalar), for control flow and
// structural arguments – the engine computes it if it isn't yet
function getAsSyncList(value: unknown) {
  if (value instanceof FluentVariable) { value = value.current }
  if (value instanceof engine.Tensor) { return value.js() }
}

// MARK: Functions

// Run Fluent source in the scope it is called from
const CodeEvaluate = function (this: CurrentScope, program: Value) {
  return evaluateSyntaxTreeNode(CodeParse(String(program)), this)
}

// The loop under ⍣. FunctionRepeat(f, n) is f applied n times; with a predicate p in
// place of the count, f runs until p(next, previous) holds; with a negative count,
// invert(f) runs |n| times – the prelude passes its Newton inverse, so this loop never
// decides how inversion is done. A dyadic x (f ⍣ n) y iterates f(x, ·) from y, as in APL.
const REPEAT_UNTIL_BUDGET = 10000
const FunctionRepeat = function (this: CurrentScope, fn: Value, n: Value, invert?: Value) {
  if (!isFn(fn)) { return new Error("`(f ⍣ n)`: `f` must be a function") }
  const until = isFn(n) ? n : null
  let times = 0
  if (!until) {
    if (!isTensor(n)) { return new Error("`(f ⍣ n)`: `n` must be a scalar count or a predicate { new, old | … }") }
    const count = getAsSyncList(n)
    // a non-scalar or NaN count would skip the loop and give the identity
    if (typeof count !== "number" || !Number.isFinite(count)) {
      return new Error("`(f ⍣ n)`: `n` must be a finite scalar, got " +
        (Array.isArray(count) ? `a tensor of shape [${n.shape.join(" ")}]` : String(count)))
    }
    // a live slider drives counts through fractional values – round, like linspace
    times = Math.round(count)
    if (times < 0 && !isFn(invert ?? null)) {
      return new Error("`FunctionRepeat(f, n, invert)`: a negative count runs f backwards, which needs `invert`")
    }
  }
  const scope = this
  return function (...args: Value[]) {
    const dyadic = args.length >= 2, left = args[0]
    const forward = (z: Value) => safeApply(fn, dyadic ? [left!, z] : [z], scope)
    let step: Function = forward
    if (times < 0) {
      const inverse = safeApply(invert!, [forward], scope)
      if (!isFn(inverse)) { return inverse }
      step = inverse
    }
    let value: Value = (dyadic ? args[1] : args[0]) ?? null
    for (let i = 0; until || i < Math.abs(times); i++) {
      if (until && i >= REPEAT_UNTIL_BUDGET) {
        return new Error(`\`(f ⍣ p)\`: the predicate didn't hold within ${REPEAT_UNTIL_BUDGET} steps`)
      }
      const prev = value
      value = step(prev)
      // a failed step is the answer – iterating an Error only repeats it
      if (value instanceof Error) { return value }
      if (!until) { continue }
      const verdict = safeApply(until, [value, prev], scope)
      if (verdict instanceof Error) { return verdict }
      try { if (isTruthy(verdict)) { break } } catch (e) { return asError(e) }
    }
    return value
  }
}

// MARK: Combinators
// The tacit aviary, after Conor Hoekstra's taxonomy (combinatorylogic.com). Each
// combinator is a plain higher-order value, so a composed function drops into operator
// position like any function: a (f ∘ g) b. Valence pairs (B+B₁, C+W, Σ+Δ, S+D, Φ+Φ₁)
// share one binding – the closure routes on argument count, as FunctionArity does.

const isFn = (v: Value): v is Function => typeof v === "function"

// Hooks and fork tines K-lift a non-function operand to a constant function – BQN's
// bind: (1 ⊸ +) increments, (÷ ⟜ 2) halves.
const constantly = (v: Value): Function => isFn(v) ? v : () => v

// A composed closure prints as the source that built it ((= ⍥ abs), not JS
// internals) – the Operation that created it stamps its origin after return.
const tacitToString = (h: Function): Function => {
  h.toString = () => getOrigin(h)?.source ?? "(tacit function)"
  return h
}

// Q (Queer bird) – then: apply f, then g to the result. Reading order is evaluation
// order – (f ∘ g)(…x) = g(f(…x)) – so this is APL's atop (B/B₁) with the tines swapped
// to read left to right, like the . pipe.
const FunctionThen = (f: Value, g: Value) => {
  if (!isFn(f) || !isFn(g)) { return new Error("`(f ∘ g)`: both operands must be functions") }
  return tacitToString(function (this: CurrentScope, ...args: Value[]) { return safeApply(g, [safeApply(f, args, this)], this) })
}

// C/W (Cardinal/Warbler) – commute: flip a binary's arguments; duplicate a unary's
const FunctionCommute = (f: Value) => {
  if (!isFn(f)) { return new Error("`(⍨ f)`: the operand must be a function") }
  return tacitToString(function (this: CurrentScope, a: Value, b: Value) { return safeApply(f, b == null ? [a, a] : [b, a], this) })
}

// Ψ (Psi) – into: preprocess every argument with g, then combine with f. The
// preprocessor is read first because it runs first: (g ⍥ f)(x, y) = f(g(x), g(y)) –
// APL spells this f⍥g, but Fluent reads left to right.
const FunctionInto = (g: Value, f: Value) => {
  if (!isFn(f) || !isFn(g)) { return new Error("`(g ⍥ f)`: both operands must be functions") }
  return tacitToString(function (this: CurrentScope, ...args: Value[]) {
    return safeApply(f, args.map((a) => safeApply(g, [a], this)), this)
  })
}

// Φ/Φ₁ (Phoenix/Pheasant) – fork: the outer tines f and h see the arguments, the middle
// tine g combines their results – the train reading order
const FunctionFork = (f: Value, g: Value, h: Value) => {
  if (!isFn(g)) { return new Error("`Φ(f, g, h)`: the middle tine `g` must be a function") }
  if (f == null || h == null) { return new Error("`Φ(f, g, h)`: takes three tines") }
  const [fc, hc] = [constantly(f), constantly(h)]
  return tacitToString(function (this: CurrentScope, ...args: Value[]) {
    return safeApply(g, [safeApply(fc, args, this), safeApply(hc, args, this)], this)
  })
}

// Σ/Δ (Violet Starling/Zebra Dove) – before: preprocess the left (or only) argument
// with f, then combine with g
const FunctionBefore = (f: Value, g: Value) => {
  if (!isFn(g)) { return new Error("`(f ⊸ g)`: `g` must be a function") }
  const fc = constantly(f)
  return tacitToString(function (this: CurrentScope, a: Value, b: Value) {
    return safeApply(g, [safeApply(fc, [a], this), b == null ? a : b], this)
  })
}

// S/D (Starling/Dove) – after: preprocess the right (or only) argument with g, then
// combine with f; the monadic case is the J hook
const FunctionAfter = (f: Value, g: Value) => {
  if (!isFn(f)) { return new Error("`(f ⟜ g)`: `f` must be a function") }
  const gc = constantly(g)
  return tacitToString(function (this: CurrentScope, a: Value, b: Value) {
    return safeApply(f, [a, safeApply(gc, [b == null ? a : b], this)], this)
  })
}

// I/K/KI – the tacks: ⊢ yields its rightmost argument, ⊣ its leftmost
const FunctionRight = (...args: Value[]) => args.length === 0 ? new Error("`⊢`: takes at least one argument") : args[args.length - 1]!
const FunctionLeft = (...args: Value[]) => args.length === 0 ? new Error("`⊣`: takes at least one argument") : args[0]!

// What kind of value – 1 or 0, like a comparison, so it composes with guard and where.
// isFunction is the K-lift question, for userland combinators that bind constants the
// way the native hooks do.
const bit = (b: boolean) => engine.tensor(b ? 1 : 0)
const FunctionIs = (v: Value) => bit(isFn(v))
const TensorIs = (v: Value) => bit(isTensorish(v))
const ListIs = (v: Value) => bit(Array.isArray(v))
const StringIs = (v: Value) => bit(isText(v))
const ErrorIs = (v: Value) => bit(v instanceof Error)
// the same value – one variable, one signal, one function – not two equal ones
const Identical = (a: Value, b: Value) => bit(a === b)

// MARK: Loops and signals
// Between steps, scheduler.yield hands the thread to input and rendering and resumes
// AHEAD of other queued work, so typing stays alive inside a heavy batch. Chrome only;
// elsewhere a batch runs unyielded, paced by frames.
const scheduler = (globalThis as { scheduler?: { yield?: () => Promise<void> } }).scheduler
const yieldToInput = typeof scheduler?.yield === "function" ? () => scheduler.yield!() : null

// Async repeat (⟳): runs fn between frames so the UI stays live. The loop belongs to the
// evaluation that started it – a re-evaluation cancels it. It gives back a signal that
// holds ◌ while the steps succeed; a step that fails stops the loop, and the signal
// holds its Error. (⍣ is the synchronous f(f(f(x))).)
const FunctionIterate = function (this: CurrentScope, fn: Value, iterations?: Value) {
  const live = iterations instanceof Signal || iterations instanceof FluentVariable
  if (!isFn(fn) || !(iterations == null || isTensor(iterations) || live)) {
    throw new Error("`FunctionIterate(fn, iterations)`: `fn` must be a function and `iterations` must be a scalar Tensor")
  }
  // The count is a BUDGET: f ⟳ n runs f n times in total. A live count is read fresh –
  // raise it and the loop runs on to the new total, lower it below what has run and it
  // stops – so a constant signal means what the literal means.
  const readTarget = (): number => {
    if (iterations == null) { return 1 }
    const n = getAsSyncList(iterations instanceof Signal ? iterations.value
      : iterations instanceof FluentVariable ? (iterations.version.value, iterations.current) : iterations)
    // a non-scalar count is never reached – an infinite loop, not an error
    if (typeof n !== "number" || Number.isNaN(n)) {
      throw new Error("`FunctionIterate(fn, iterations)`: `iterations` must be a scalar Tensor")
    }
    return n
  }
  // A bad count fails the call. Read untracked: the loop follows a live count itself, and
  // a program that read it would run again – restarting its loop – whenever it moved.
  untracked(readTarget)

  const generation = currentGeneration, scope = this, failure = signal<Value>(null)
  let i = 0, batch = 1, prev = 0, scheduled = false, cancelled = false
  registerDisposable(() => { cancelled = true })
  const gone = () => generation !== currentGeneration || cancelled

  // Paced by display frames: steps align to the display, a hidden tab pauses the loop,
  // and a batch of steps per frame amortizes the frame. The batch grows while frames
  // keep vsync (~16.7 ms) and halves when one slips (~33 ms), so a heavy step or plot
  // runs a few per frame and a cheap one many; the cap keeps the GPU from backlogging.
  const frame = async (t: number) => {
    scheduled = false
    if (gone()) { return }
    // idle, not finished: a live budget may grow again, and its effect re-arms the loop
    const target = readTarget()
    if (i >= target) { return }
    if (prev) { batch = t - prev > 25 ? Math.max(1, batch >> 1) : Math.min(64, batch + Math.max(1, batch >> 2)) }
    prev = t
    // The batch is a plan: it ends once this frame's own step time passes 25 ms (time
    // yielded to input doesn't count), so the first frame after cheap paused ticks
    // doesn't run a grown batch of expensive steps.
    let stepTime = 0
    const end = Math.min(i + batch, target)
    while (i < end) {
      // at most two steps on the GPU: the screen's frame waits behind whatever is queued
      await engine.gpuDrained(2)
      if (gone()) { return }
      const start = performance.now()
      const result = safeApply(fn, [engine.scalar(i)], scope)
      if (result instanceof Error) { cancelled = true; failure.value = result; return }
      i++
      stepTime += performance.now() - start
      if (stepTime > 25) { break }
      if (yieldToInput && i < end) {
        await yieldToInput()
        if (gone()) { return }
      }
    }
    schedule()
  }
  // one pending frame at a time, or every wake-up would start another pump
  const schedule = () => { if (!scheduled && !cancelled) { scheduled = true; nextFrame(frame) } }
  // a live budget re-arms the loop when it changes, also after it went idle
  if (live) { registerDisposable(effect(() => { try { readTarget() } catch { return } schedule() })) }
  schedule()
  return failure
}

// Fresh scalar for time-varying sources (Time, sliders, scrubbers): held as data, never
// folded into a kernel – the clock ticking or a slider moving reuses the compiled plan
const TensorScalarLive = (value: number): engine.Tensor => engine.scalar(value)

// A signal holds data: a tensor stored in it – or in a list in it – is settled into a
// leaf, so a value written back every tick (⟳ with ←) never drags the previous ticks along
const heldValue = (v: Value): Value => Array.isArray(v) ? v.map(heldValue) : isTensor(v) ? engine.settle(v) : v

const SignalCreate = (<T,>(initial: T) => signal(heldValue(initial as Value))) as typeof signal
const SignalComputed = computed
const SignalRead = (s: Value) => s instanceof Signal ? s.value : new Error(`'SignalRead': expected a signal, got ${describeArg(s)}`)
const SignalUpdate = (s: Value, v: Value): Value => {
  if (!(s instanceof Signal)) { return new Error(`'SignalUpdate': expected a signal, got ${describeArg(s)}`) }
  if (differentiating) { throw new StateWrite("a signal write") }
  s.value = heldValue(v)
  return null
}
// effects belong to the evaluation that created them – a re-evaluation stops them, or
// stale effects would keep firing into disposed variables
const SignalEffect = ((fn: () => void) => {
  const dispose = effect(fn)
  registerDisposable(dispose)
  return dispose
}) as typeof effect
// a signal's value, read without subscribing to it
const SignalOnce = (s: Value) => s instanceof Signal ? s.peek() : s
// $v: a signal holding v, or a computed signal of a thunk
const Reactive = (a: Value) => isFn(a) ? SignalComputed(a as () => Value) : SignalCreate(a)

// name: value binds in the scope it is called from; the name arrives quoted
const SymbolAssign = function (this: CurrentScope, name: Value, value: Value) {
  if (typeof name !== "symbol") { return new Error(`'SymbolAssign': Left side must be a symbol, got: ${String(name)}`) }
  this[Symbol.keyFor(name) ?? ""] = value
  return value ?? null
}
setMeta(SymbolAssign, { quotedArgs: [0] })

// f @ x applies f to x; a list spreads as the arguments. x . f is the same, read the
// other way round.
const FunctionApply = function (this: CurrentScope, fn: Value, args: Value) {
  return safeApply(fn, Array.isArray(args) ? args : [args], this)
}

// f, as a new function whose signal arguments arrive as the signals themselves – f
// itself, which every evaluation shares, stays as it is
const FunctionNoAutoLift = (fn: Value) => {
  if (!isFn(fn)) { return new Error("`FunctionNoAutoLift(f)`: f must be a function") }
  const wrapper = function (this: CurrentScope, ...args: Value[]) { return fn.apply(this, args) }
  wrapper.toString = () => String(fn)
  setMeta(wrapper, { ...getMeta(fn), noAutoLift: true })
  const origin = getOrigin(fn)
  return origin ? located(wrapper, { origin }) : wrapper
}

// Fluent's truth, one rule for a number and a tensor: some element is nonzero and not
// NaN. guard, when and the until-form of ⍣ all ask it, and read one number to answer.
const isTruthy = (v: Value): boolean => {
  const t = operand(v)
  if (!isTensor(t)) { throw new Error(`expected a tensor, got ${describeArg(v)}`) }
  const holds = (n: number) => n !== 0 && !Number.isNaN(n)
  return t.size === 1 ? holds(t.item()) : t.size > 0 && holds(engine.max(engine.truthy(t)).item())
}

// guard(cond, { value }) – a candidate that answers with value while cond holds and
// declines otherwise. The condition is read strictly, at the guard: a typo'd name or an
// Error there is a failure, not a quiet "no" – the guard itself becomes the Error, and
// a cascade surfaces it. A body that isn't a thunk is its own value.
const FunctionGuard = function (this: CurrentScope, cond: Value, body: Value) {
  const holds = isTruthy(cond), scope = this
  const candidate = function () {
    if (holds) { return isFn(body) ? body.apply(scope) : body }
    // the guard's own call site, so hovering the decline finds it
    const origin = getOrigin(candidate)
    const decline = new NoMatch("guard: the condition doesn't hold")
    return origin ? located(decline, { origin }) : decline
  }
  return candidate
}

// try({ value }) – a candidate that declines when value FAILS: the failure becomes a
// NoMatch, kept as its cause, so a cascade moves on. A candidate's arguments pass
// through, so try(f) joins an overload set as f would.
const FunctionTry = function (this: CurrentScope, body: Value) {
  const scope = this
  return function (...args: Value[]) {
    let result: Value
    try {
      result = isFn(body) ? body.apply(scope, args) : body
    } catch (e) {
      if (e instanceof StateWrite) { throw e }
      result = asError(e)
    }
    return result instanceof Error && !(result instanceof NoMatch) ? new NoMatch("try: it failed", { cause: result }) : result
  }
}

// when(cond, { … }) – a conditional effect: run the thunk while cond is truthy, yield ◌
// otherwise. Not a candidate: whatever the thunk returns – an Error included – is the
// result, so a broken training step stays loud.
const FunctionWhen = function (this: CurrentScope, cond: Value, thunk: Value) {
  if (!isTruthy(cond)) { return null }
  return isFn(thunk) ? thunk.apply(this) : new Error("`when(cond, { … })`: second argument must be a thunk")
}

// Error(message, cause?) – a failure as a value: no answer, and why. The cause can be
// any value: the Error this one grew from, or what was tried.
const ErrorCreate = (message: Value, cause?: Value): Error =>
  new Error(isText(message) ? String(message) : StringLiteral(message), cause == null ? undefined : { cause })

// Attach a card to a function and give it back – written next to the binding in the
// prelude. Aliases share the function, so one card covers them all.
const doc = (target: Value, signature: Value, description: Value, example: Value): Value => {
  if (isFn(target)) { setMeta(target, { signature: String(signature), doc: String(description), example: String(example) }) }
  return target
}

// These take what they are given as it is – a signal, a thunk, a loop's own count – so a
// signal argument does not turn the call into a computed. ⟳ lifted would be a computed
// that nothing reads, and would run no steps at all.
for (const f of [FunctionCond, FunctionIterate, SignalRead, SignalUpdate, SignalEffect, SignalOnce, Reactive, SymbolAssign,
  FunctionNoAutoLift, FunctionGuard, FunctionTry, FunctionWhen, doc]) { setMeta(f, { noAutoLift: true }) }

// MARK: Lists
const List = (...args: Value[]) => args

const ListConcat = (...args: Value[]) => {
  const bad = args.findIndex((a) => !Array.isArray(a))
  return bad < 0 ? ([] as Value[]).concat(...args) : new Error(`'ListConcat': expected lists, got ${describeArg(args[bad]!)}`)
}

// a string is StringLength's case, counted in characters
const ListLength = (a: Value) => Array.isArray(a) ? engine.tensor(a.length) : new NoMatch(`expected a list, got ${describeArg(a)}`)

const ListGet = (a: Value, b: Value) => {
  if (!Array.isArray(a)) { return new Error("'ListGet': 'a' must be an array") }
  const index = asNumber(b)
  if (index < -a.length || index >= a.length) {
    return new Error(`'ListGet': Index '${index}' out of bounds for list of length ${a.length}`)
  }
  return a.at(index)!
}

const ListMap = function (this: CurrentScope, a: Value, fn: Value) {
  if (!isFn(fn)) { return new Error("'ListMap': 'fn' must be a function") }
  if (!Array.isArray(a)) { return new Error("'ListMap': 'a' must be a list") }
  return a.map((v, i) => safeApply(fn, [v, engine.tensor(i)], this))
}

// Fold a list left to right with f(acc, element), from init – or from the first element
// when there is no init
const ListReduce = function (this: CurrentScope, a: Value, fn: Value, init?: Value) {
  if (!Array.isArray(a)) { return new Error(`'ListReduce': expected a list, got ${describeArg(a)}`) }
  if (!isFn(fn)) { return new Error("'ListReduce': `fn` must be a function") }
  if (init == null && a.length === 0) { return new Error("'ListReduce': an empty list with no init has nothing to fold") }
  let acc: Value = init == null ? a[0]! : init
  for (const v of init == null ? a.slice(1) : a) { acc = fn.call(this, acc, v) }
  return acc
}

// ListReduce that keeps every step: (init, f(init, a₀), f(f(init, a₀), a₁), …)
const ListScan = function (this: CurrentScope, a: Value, fn: Value, init: Value) {
  if (!Array.isArray(a)) { return new Error("'ListScan': `list` must be a list") }
  if (!isFn(fn)) { return new Error("'ListScan': `fn` must be a function") }
  const out = [init]
  for (const v of a) { out.push(fn.call(this, out[out.length - 1]!, v)) }
  return out
}

// MARK: Tensors
// What a value is, for a message
const describeArg = (v: Value): string =>
  typeof v === "symbol" ? `unbound symbol '${Symbol.keyFor(v) ?? String(v)}'`
    : v == null ? "◌"
    : v instanceof Error ? "an Error"
    : v instanceof Signal ? "a signal"
    : Array.isArray(v) ? "a list"
    : isText(v) ? "a string"
    : isFn(v) ? "a function"
    : isTensor(v) ? (v.shape.length === 0 ? "a number" : `a tensor [${v.shape.join(" ")}]`)
    : v instanceof FluentVariable ? "a trainable variable"
    : `a ${typeof v}`

// An axis, a size, an index or a count, read out of a tensor argument. Strict: an unknown
// name or an Error here fails, rather than coerce to NaN and change what the op does.
const requireData = (v: Value): number | unknown[] => {
  const data = getAsSyncList(v)
  if (typeof data !== "number" && !Array.isArray(data)) { throw new Error(`expected a tensor, got ${describeArg(v)}`) }
  return data
}
// a scalar, or a tensor of one element
const asNumber = (v: Value): number => {
  const data = [requireData(v)].flat(Infinity) as number[]
  if (data.length !== 1) { throw new Error(`expected a scalar, got a tensor of shape [${shapeOf(v).join(" ")}]`) }
  if (Number.isNaN(data[0])) { throw new Error("expected a number, got NaN") }
  return data[0]!
}
const asNumberList = (v: Value): number[] => ([] as number[]).concat(requireData(v) as number[])

// Sizes, counts, axes and repeat factors are whole numbers, but Fluent has one dtype and
// a 0..1 slider scaled by arithmetic makes them fractional all the time. They round, as
// linspace and ⍣ do: a tensor with a fractional shape would fail only when computed, at
// read time, outside the evaluator. ±Infinity is not a length.
const finite = (n: number): number => {
  if (!Number.isFinite(n)) { throw new Error(`expected a whole number, got ${n}`) }
  return n
}
const asWhole = (v: Value): number => Math.round(finite(asNumber(v)))
const asWholeList = (v: Value): number[] => asNumberList(v).map((n) => Math.round(finite(n)))

// A numeric operand: a string ("a" + "b") or a function is a mistake here, so the
// message names it instead of an engine's. operand itself stays general – a signal may
// hold a string.
const numericArg = (v: Value): any => {
  if (isText(v)) {
    throw new Error(`a string (${JSON.stringify(String(v))}) is not a tensor – numeric ops like + and × need tensors; use StringConcat to join text`)
  }
  if (isFn(v)) { throw new Error("a function is not a tensor – numeric ops like + and × need tensors") }
  return operand(v)
}

// The engine's own ops, each a native under its name. Comparisons give float32 0/1,
// APL-style: Fluent is a float32 language, and (x = 1) - y is arithmetic.
const natives = (ops: Record<string, Function>, wrap: (op: any) => Function): Record<string, Function> =>
  Object.fromEntries(Object.entries(ops).map(([name, op]) => [name, Object.defineProperty(wrap(op), "name", { value: name })]))
const UNARY = natives({
  TensorSine: engine.sin, TensorCosine: engine.cos, TensorTangentHyperbolic: engine.tanh,
  TensorSineInverse: engine.asin, TensorCosineInverse: engine.acos, TensorTangentInverse: engine.atan,
  TensorSineHyperbolicInverse: engine.asinh, TensorCosineHyperbolicInverse: engine.acosh, TensorTangentHyperbolicInverse: engine.atanh,
  TensorAbsolute: engine.abs, TensorSign: engine.sign, TensorFloor: engine.floor, TensorRound: engine.round,
  TensorNaturalLogarithm: engine.log, TensorLog1Plus: engine.log1p, TensorExponential: engine.exp, TensorExpMinus1: engine.expm1,
  TensorReciprocal: engine.reciprocal, TensorSquareRoot: engine.sqrt, TensorCubeRoot: engine.cbrt, TensorIsNaN: engine.isnan,
  // x, with no derivative – ∇ treats it as a constant
  TensorDetach: engine.detach,
}, (op) => (a: Value) => op(numericArg(a)))
const BINARY = natives({
  TensorAdd: engine.add, TensorMultiply: engine.multiply, TensorDivide: engine.divide,
  // a literal integer exponent is exponentiation by squaring in the engine – exact where
  // exp(e·ln x) is not (3^2 is 9, (-2)^2 is 4), and x^2 differentiates as x·x
  TensorPower: engine.power,
  TensorMaximum: engine.maximum, TensorMinimum: engine.minimum, TensorLess: engine.less, TensorEqual: engine.equal,
  // atan2(y, x): the angle of (x, y)
  TensorArcTangent2: engine.atan2,
  // the contractions stay native until shape logic written in Fluent is cheap: their
  // Fluent versions spend most of a training step's host time reading shapes
  TensorMatrixMultiply: engine.matmul, TensorDotProduct: engine.dot,
}, (op) => (a: Value, b: Value) => op(numericArg(a), numericArg(b)))
// Reductions take an optional axis (a scalar or a vector) and, third, a truthy keep that
// leaves the reduced axes as size 1 – mean(x, -1, 1) is unsqueeze(mean(x, -1), -1).
const REDUCTIONS = natives({ TensorSum: engine.sum, TensorProduct: engine.prod, TensorMin: engine.min, TensorMax: engine.max },
  (op) => (a: Value, b?: Value, keep?: Value) => {
    const axis = b == null ? null : Array.isArray(requireData(b)) ? asWholeList(b) : asWhole(b)
    return op(operand(a), axis, keep == null ? undefined : { keepdims: asNumber(keep) !== 0 })
  })
for (const [op, reduction] of [["TensorAdd", "TensorSum"], ["TensorMultiply", "TensorProduct"], ["TensorMaximum", "TensorMax"], ["TensorMinimum", "TensorMin"]] as const) {
  nativeReducer.set(BINARY[op]!, REDUCTIONS[reduction] as (x: Value, axis?: Value) => Value)
}

// stack broadcasts its inputs to a common shape, as arithmetic does: stack(x - a, y - b)
// works when the pieces only meet by broadcasting
const broadcastRefs = (values: Value[]): engine.Tensor[] => {
  const tensors = values.map((v) => v instanceof FluentVariable ? v.current : v)
  if (tensors.length === 0 || !tensors.every(isTensor)) { return tensors.map(operand) }
  const rank = Math.max(...tensors.map((t) => t.shape.length))
  const shape = Array.from({ length: rank }, (_, i) => Math.max(...tensors.map((t) => t.shape[t.shape.length - rank + i] ?? 1)))
  return tensors.map((t) => t.shape.length === rank && t.shape.every((d, i) => d === shape[i]) ? t : engine.broadcastTo(t, shape))
}
// stack(a, b, …) joins along a new axis 0; stack((a, b, …), axis) picks the axis
const TensorStack = (...args: Value[]) => Array.isArray(args[0])
  ? engine.stack(broadcastRefs(args[0]), args[1] == null ? 0 : asNumber(args[1]))
  : engine.stack(broadcastRefs(args))
// the slices along an axis, as a list
const TensorUnstack = (a: Value, b?: Value): Value[] => {
  const axis = b == null ? 0 : asNumber(b), x = operand(a) as engine.Tensor
  return [...(axis === 0 ? x : engine.moveaxis(x, axis, 0))]
}
const TensorConcat = (...args: Value[]) => {
  const listed = Array.isArray(args[0])
  const xs = ((listed ? args[0] : args) as Value[]).map(operand)
  return engine.concatenate(xs, listed && args[1] != null ? asNumber(args[1]) : 0)
}

// axis 0 as a reusable value – the default a fold collapses
const LEADING_AXIS: Value = engine.tensor(0)

// Fold a tensor left to right with a binary fn, along an axis (0 by default): `1 ..< 10
// reduce +` = 45, `x reduce { a, b | a - b }` = ((x_0-x_1)-x_2)-… A known op takes its
// native reduction, pinned to the leading axis as a fold is – the native reductions
// default to every axis.
const TensorReduce = function (this: CurrentScope, tensor: Value, fn: Value, axis?: Value) {
  if (!isFn(fn)) { return new Error("`reduce(tensor, fn, axis?)`: the second argument must be a function") }
  const fast = nativeReducer.get(fn)
  if (fast) { return fast(tensor, axis == null ? LEADING_AXIS : axis) }
  const parts = TensorUnstack(tensor, axis)
  // a known op has an identity (Σ[] = 0); a custom fn's is unknown
  if (parts.length === 0) { return new Error("`reduce`: cannot fold an empty tensor with a custom function") }
  return ListReduce.call(this, parts, fn)
}

// Index values for a bounds check: read when they are on the host or cheap to compute
// there; values still on the GPU – a minibatch drawn from a data slot – flow through
// unchecked, as trusted data
const cheapValues = (v: Value): Float32Array | undefined => {
  const t = v instanceof FluentVariable ? v.current : v
  return isTensor(t) ? engine.hostValue(t) : undefined
}
// oneHot of an index outside 0..<depth would be an all-zero row: an off-by-one label, or a
// -1 "unknown", would silently drop out of the loss
const TensorOneHot = (indices: Value, depth: Value) => {
  const n = asWhole(depth)
  const bad = cheapValues(indices)?.find((i) => !(i >= 0 && i < n))
  if (bad !== undefined) { return new Error(`oneHot: class ${bad} is out of bounds for a depth of ${n}`) }
  return engine.oneHot(engine.trunc(operand(indices)), n)
}

const TensorSort = (x: Value) => isScalarTensor(x) ? engine.reshape(operand(x), []) : engine.sort(operand(x))
const TensorArgSort = (x: Value) => isScalarTensor(x)
  ? engine.reshape(engine.argsort(engine.reshape(operand(x), [1])), [])
  : engine.argsort(operand(x))
const TensorSlice = (a: Value, begin: Value, size?: Value) => {
  const sizes = size == null ? undefined : asWholeList(size)
  const spec = asWholeList(begin).map((b, i) => {
    const s = sizes?.[i]
    return (s === undefined || s === -1 ? [b] : [b, b + s]) as [number] | [number, number]
  })
  return (operand(a) as engine.Tensor).slice(...spec)
}

// A differentiated body runs with state writes forbidden; one it tries is an Error that
// names the body
const differentiated = (body: string, run: () => Value): Value => {
  const outer = differentiating
  differentiating = true
  try {
    return run()
  } catch (e) {
    if (e instanceof StateWrite) { return new Error(`${body} cannot change state while it is differentiated (${e.message})`) }
    throw e
  } finally {
    differentiating = outer
  }
}
// vjp with a ones cotangent: a scalar output gives the classic gradient, an elementwise
// one per-element derivatives – ∇({x | x^2}) works on vectors
const TensorGradient = (f: Value) => {
  if (!isFn(f)) { return new Error("`grad(f)`: `f` must be a function") }
  return (x: Value) => x == null ? gradientOfReads(f) : differentiated("`grad(f)`: `f`", () => {
    const [out, pullback] = engine.vjp((traced: engine.Tensor) => {
      const value = SignalOnce(f(traced))
      if (value instanceof Error) { throw value }
      if (!isTensor(value)) { throw new Error("`grad(f)`: `f` must return a tensor") }
      return value
    }, [operand(x)])
    return pullback(engine.full(out.shape, 1))[0]!
  })
}
// ∇(loss)(): with nothing to differentiate with respect to, the derivative is with respect
// to the variables the loss reads – found by reading them, as a computed signal finds its
// signals. It comes back with the value, all computed in one pass: (value, variables,
// gradients). An enclosing ∇ still differentiates through it.
const gradientOfReads = (loss: Function): Value => {
  const reads = new Map<FluentVariable, engine.Tensor>(), outer = readTracker
  readTracker = (v, value) => {
    let t = reads.get(v)
    if (!t) { t = engine.traced(outer ? outer(v, value) : value); reads.set(v, t) }
    return t
  }
  let value: Value
  try { value = differentiated("`∇(loss)()`: the loss", () => SignalOnce(loss())) } finally { readTracker = outer }
  if (value instanceof Error) { return value }
  if (!isTensor(value)) { return new Error("`∇(loss)()`: the loss must be a tensor") }
  const variables = [...reads.keys()], grads = engine.gradients(value, [...reads.values()])
  engine.materialize([value, ...grads])
  return [engine.settle(value), variables, grads.map(engine.settle)]
}

// The views: x's elements renamed, nothing moved or computed. permute puts x's axis
// axes_i at position i; broadcast stretches size-1 (and missing leading) axes to shape;
// padAxis adds zeros around one axis; window gives the sliding windows of the trailing
// axes – out[…, o…, k…] = x[…, o + k…] – one per position.
const TensorPermute = (a: Value, axes: Value) => engine.transpose(operand(a), asWholeList(axes))
const TensorBroadcast = (a: Value, shape: Value) => engine.broadcastTo(operand(a), asWholeList(shape))
const TensorPadAxis = (a: Value, axis: Value, before: Value, after: Value) =>
  engine.padAxis(operand(a), asWhole(axis), asWhole(before), asWhole(after))
const TensorWindow = (a: Value, sizes: Value) => engine.windows(operand(a), asWholeList(sizes))

// start ..< stop by 1, or by -1 when stop is below start; the ends truncate
const TensorRange = (a: Value, b?: Value) => {
  const end = (v: Value) => Math.trunc(finite(asNumber(v)))
  if (b == null) { return engine.arange(end(a)) }
  const start = end(a), stop = end(b)
  return engine.arange(start, stop, start <= stop ? 1 : -1)
}

// Rank-polymorphic convolution: the kernel's rank sets the conv's rank, so one conv
// slides a 1-D kernel over a vector or a 2-D kernel over an image, zero padded so the
// output keeps the input's shape.
const TensorConvolution = (arr: Value, kernel: Value) => engine.conv(operand(arr), operand(kernel))
// meshgrid(0 ..< w, 0 ..< h) is [X, Y], each spread over the whole grid
const TensorMeshgrid = (...args: Value[]) => engine.meshgrid(args.map(operand))
// the k largest values and their indices, as (values, indices)
const TensorTopK = (a: Value, k: Value) => engine.topK(operand(a), asWhole(k))
// x ⍴ shape reshapes; ⍴x alone is x's shape
const TensorReshape = (a: Value, b?: Value) => b != null ? engine.reshape(operand(a), asWholeList(b)) : engine.tensor(shapeOf(a))

// A missing axis has size 1, never NaN: a scalar is one item along its phantom leading
// axis, so #(5) is 1 (APL's tally ≢), as broadcasting treats an absent axis. An empty
// tensor stays 0. A negative axis counts from the end. A list or a string is another
// candidate's case.
const TensorLength = (a: Value, b?: Value) => {
  if (!isTensorish(a)) { return new NoMatch(`expected a tensor, got ${describeArg(a)}`) }
  const shape = shapeOf(a), axis = b == null ? 0 : asWhole(b)
  return engine.tensor(shape[axis < 0 ? axis + shape.length : axis] ?? 1)
}
const TensorShape = (a: Value) => isTensorish(a) ? engine.tensor(shapeOf(a)) : new Error(`shape: expected a tensor, got ${describeArg(a)}`)

// `_` on a string or a list keeps the container's type: x_i picks one item, x_[i, j]
// several; a negative index counts from the end. A string's items are its characters.
const pick = (items: Value[], b: Value, kind: string, join: (picked: Value[]) => Value): Value => {
  let indices: number[], multi = true
  if (Array.isArray(b)) { indices = b.map(asNumber) } else {
    let d: unknown
    try { d = getAsSyncList(b) } catch { d = undefined }
    if (Array.isArray(d)) { indices = d.flat(Infinity) as number[] } else { indices = [asNumber(b)]; multi = false }
  }
  const picked: Value[] = []
  for (const raw of indices) {
    const i = Math.trunc(raw) < 0 ? Math.trunc(raw) + items.length : Math.trunc(raw)
    if (!(i >= 0 && i < items.length)) { return new Error(`index ${raw} is out of bounds for a ${kind} of length ${items.length}`) }
    picked.push(items[i]!)
  }
  return multi ? join(picked) : picked[0]!
}
// `_` is polymorphic over the container: a string, a list or a tensor
const TensorGather = (a: Value, b: Value): Value =>
  isText(a) ? pick([...String(a)], b, "string", (cs) => cs.join(""))
    : Array.isArray(a) ? pick(a, b, "list", (xs) => xs)
    // a scalar is a rank-0 tensor with a single phantom item (#(5) = 1): 5_0 = 5
    : TensorTake(isScalarTensor(a) ? engine.reshape(operand(a), [1]) : a, b)
// take(x, indices): the rows of x at indices, any shape of them; a negative index counts
// from the end. Every index on the host or cheap to compute there is bounds-checked – a
// gather has no bounds mode, so an index out of range would read garbage; one still on
// the GPU (an embedding lookup into a minibatch) is trusted data and flows through.
const TensorTake = (a: Value, b: Value): Value => {
  const x = operand(a) as engine.Tensor, size = x.shape[0] ?? 0
  // `!(0 <= w < size)` also rejects a NaN index, which slips past `<` and `>=`
  const bad = cheapValues(b)?.find((i) => { const w = i < 0 ? i + size : i; return !(w >= 0 && w < size) })
  if (bad !== undefined) { return new Error(`index ${bad} is out of bounds for an axis of length ${size}`) }
  const raw = engine.trunc(operand(b))
  return engine.take(x, engine.where(engine.less(raw, 0), engine.add(raw, size), raw), 0)
}
// scatterAdd(indices, values, size): [size, …] zeros with each row of values added at its
// index – take's transpose
const TensorScatterAdd = (indices: Value, values: Value, size: Value) =>
  engine.scatterAdd(operand(indices), operand(values), asWhole(size))
const TensorWhere = (a: Value, b: Value, c: Value) => engine.where(engine.truthy(operand(a)), operand(b), operand(c))

const TensorVariable = (a: Value) =>
  isTensorish(a) ? new FluentVariable(operand(a)) : new Error("`~(init)`: initial value must be a tensor")
const TensorData = (a: Value) =>
  isTensorish(a) ? new FluentData(operand(a)) : new Error("`~~(init)`: initial value must be a tensor – make one slot per tensor")

const TensorAssign = (a: Value, b: Value): Value => {
  // a list of variables takes a list of as many values, computed together
  if (Array.isArray(a)) {
    if (!Array.isArray(b) || b.length !== a.length) { return new Error("`:=`: a list of variables takes a list of as many values") }
    const bad = a.findIndex((v, i) => !(v instanceof FluentVariable) || !isTensorish(b[i]!))
    if (bad >= 0) { return TensorAssign(a[bad]!, b[bad]!) }
    if (differentiating) { throw new StateWrite("a variable write") }
    const values = b.map(operand) as engine.Tensor[]
    engine.materialize(values)
    a.forEach((v, i) => { (v as FluentVariable).assign(values[i]!) })
    return null
  }
  if (!(a instanceof FluentVariable)) { return new Error("`:=`: left side must be a variable created with ~ or ~~") }
  // an Error (an emptied corpus, a failed expression) doesn't corrupt the variable: it
  // keeps its last good value, and the Error surfaces
  if (!isTensorish(b)) { return new Error("`:=`: value must be a tensor", b instanceof Error ? { cause: b } : undefined) }
  if (differentiating) { throw new StateWrite("a variable write") }
  a.assign(operand(b))
  return null
}

// watch(θ): a signal that updates on every assignment to a variable (drag, optimizer
// step, :=)
const TensorWatch = (a: Value): Value => a instanceof FluentVariable ? computed(() => (a.version.value, a.current)) : a

// A fresh key per call, counted from 0 in each evaluation
let rngCounter = 0
const TensorRandomUniform = (a: Value) => engine.uniform(rngCounter++, asWholeList(a))

// MARK: Strings
// A value as the Fluent SOURCE that reproduces it, so CodeEvaluate(String(x)) gives x
// back. This is repr, not display: a string comes back quoted, a list keeps its parens,
// a tensor its rank. Joined with commas, as a JS string coercion does, the text would
// not just lose its brackets – it would re-parse as a SEQUENCE, `[1, 2, 3]` as 3.
//
// The law holds exactly for the values that are data: numbers, tensors of any rank,
// lists, strings, ◌. It cannot hold for values with identity – a signal or a `~` variable
// round-trips to a NEW one, equal but not the same – nor for a closure over names the
// target scope does not have. Those are best effort.
const numberLiteral = (n: number): string =>
  Number.isFinite(n) ? String(n)
    // no literal for these, but these expressions evaluate to them
    : Number.isNaN(n) ? "(0 ÷ 0)"
    : n > 0 ? "(1 ÷ 0)" : "(0 - 1 ÷ 0)"
const nestedLiteral = (d: unknown): string => Array.isArray(d) ? `[${d.map(nestedLiteral).join(", ")}]` : numberLiteral(Number(d))

const StringLiteral = (v: Value): string => {
  if (v == null) { return "◌" }
  if (isText(v)) { return `"${String(v).replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"` }
  if (typeof v === "symbol") { return Symbol.keyFor(v) ?? String(v) }
  if (v instanceof FluentVariable) { return `${v instanceof FluentData ? "~~" : "~"}(${StringLiteral(v.current)})` }
  if (v instanceof Signal) { return `$(${StringLiteral(v.peek())})` }
  if (isTensor(v)) { return nestedLiteral(getAsSyncList(v)) }
  // a one-element list needs its trailing comma, or the parens are just grouping
  if (Array.isArray(v)) { return v.length === 1 ? `(${StringLiteral(v[0]!)},)` : `(${v.map(StringLiteral).join(", ")})` }
  // a function prints its source; an Error has no literal form – it shows itself
  return String(v)
}

// text joined end to end; ◌, an Error or an unknown name is not text
const StringConcat = (...args: Value[]) => {
  const bad = args.findIndex((a) => a == null || a instanceof Error || typeof a === "symbol")
  if (bad >= 0) { throw new Error(`StringConcat: expected text, got ${describeArg(args[bad]!)}`) }
  return args.join("")
}
// characters, not UTF-16 units – as StringToCodes and `_` count them
const StringLength = (a: Value) => isText(a) ? engine.tensor([...String(a)].length) : new NoMatch(`expected a string, got ${describeArg(a)}`)

// length works on whatever it's handed – a tensor's leading axis, a list's element
// count, or a string's characters – by trying each in turn

// Text ↔ tensor: the door between strings and token land. Codes are float32 – small
// integers are exact in float, and float spares downstream math from int-promotion
// surprises. Decode rounds, so model outputs convert directly.
const StringToCodes = (a: Value) =>
  isText(a) ? engine.tensor(Float32Array.from(String(a), (c) => c.codePointAt(0)!)) : new Error("`StringToCodes(text)`: expected a string")

const codesText = (codes: ArrayLike<number>): string | Error => {
  let text = ""
  for (let i = 0; i < codes.length; i++) {
    const code = codes[i]!
    if (!Number.isFinite(code)) { return new Error("`CodesToString(codes)`: expected a tensor of character codes") }
    text += String.fromCodePoint(Math.min(0x10FFFF, Math.max(0, Math.round(code))))
  }
  return text
}
// Text is for showing, so it never makes the page wait: codes still being computed on the
// GPU give a signal that holds "" until they arrive, then the text – whatever reads it
// (StringConcat, Text) recomputes then. Codes already at hand give the string itself.
const CodesToString = (a: Value) => {
  const t = a instanceof FluentVariable ? a.current : a
  if (!isTensor(t)) { return new Error("`CodesToString(codes)`: expected a tensor of character codes") }
  const known = engine.hostValue(t)
  if (known) { return codesText(known) }
  const text = signal<Value>("")
  t.read().then((codes) => { text.value = codesText(codes) }, (e) => { text.value = asError(e) })
  return text
}

// MARK: Storage
// Storage(key) is a signal whose value is kept under key and survives a reload: ◌ until
// a saved value arrives; a write saves it. What is saved is the value at the write – a
// variable's value then, a tensor's numbers (read without blocking), strings and lists as
// they are; ◌ forgets the key. The browser keeps the values in IndexedDB; without it, as
// in Bun, they last as long as the process.
type Stored = null | string | { shape: number[], data: Float32Array } | Stored[]
const storedInMemory = new Map<string, Stored>()
let storageDatabase: Promise<IDBDatabase | null> | undefined
const storageRequest = <T,>(mode: IDBTransactionMode, request: (values: IDBObjectStore) => IDBRequest, fallback: () => T): Promise<T> => {
  storageDatabase ??= typeof indexedDB === "undefined" ? Promise.resolve(null) : new Promise((resolve) => {
    const open = indexedDB.open("fluent-storage", 1)
    open.onupgradeneeded = () => { open.result.createObjectStore("values") }
    open.onsuccess = () => resolve(open.result)
    open.onerror = () => resolve(null)
  })
  return storageDatabase.then((db) => db === null ? fallback() : new Promise<T>((resolve, reject) => {
    const r = request(db.transaction("values", mode).objectStore("values"))
    r.onsuccess = () => resolve(r.result as T)
    r.onerror = () => reject(r.error)
  }))
}
// one queue for every key, so a read sees each write made before it
let storageQueue: Promise<unknown> = Promise.resolve()
const inStorageOrder = <T,>(op: () => Promise<T>): Promise<T> => {
  const next = storageQueue.then(op, op)
  storageQueue = next.catch(() => undefined)
  return next
}
// the value now, as data: a variable is its value at this moment
const storedNow = (v: Value): Value =>
  v instanceof FluentVariable ? v.current : Array.isArray(v) ? v.map(storedNow) : v
const toStored = (v: Value): Promise<Stored> =>
  v == null ? Promise.resolve(null)
    : isText(v) ? Promise.resolve(String(v))
      : isTensor(v) ? v.read().then((data) => ({ shape: [...v.shape], data: Float32Array.from(data) }))
        : Array.isArray(v) ? Promise.all(v.map(toStored))
          : Promise.reject(new Error(`Storage: ${describeArg(v)} can't be stored – only tensors, strings, lists of them and ◌`))
const fromStored = (s: Stored): Value =>
  s === null ? null : typeof s === "string" ? s : Array.isArray(s) ? s.map(fromStored) : engine.tensor(s.data, { shape: s.shape })

// a Storage signal sees every write, also one of the value it already holds
const signalValue = Object.getOwnPropertyDescriptor(Signal.prototype, "value")!
const Storage = (key: Value): Value => {
  if (!isText(key)) { return new Error(`Storage(key): the key is a string, got ${describeArg(key)}`) }
  const name = String(key), value = signal<Value>(null)
  let written = false, gone = false
  const show = (v: Value) => { signalValue.set!.call(value, v) }
  inStorageOrder(() => storageRequest("readonly", (values) => values.get(name), () => storedInMemory.get(name))).then(
    (s) => { if (!gone && !written && s !== undefined) { show(fromStored(s as Stored)) } },
    (e) => { if (!gone && !written) { show(asError(e)) } })
  Object.defineProperty(value, "value", {
    get: () => signalValue.get!.call(value),
    set: (v: Value) => {
      if (v instanceof Error) { show(v); return }
      written = true
      const now = storedNow(v), stored = toStored(now)
      stored.catch(() => undefined)
      show(now)
      inStorageOrder(() => stored.then((s) => storageRequest("readwrite",
        (values) => s === null ? values.delete(name) : values.put(s, name),
        () => s === null ? storedInMemory.delete(name) : storedInMemory.set(name, s))))
        .catch((e) => { if (!gone) { show(asError(e)) } })
    },
  })
  registerDisposable(() => { gone = true })
  return value
}

// MARK: Environment
// Null-prototype: an environment is a bare name→value mapping. A plain `{}` would chain
// to Object.prototype, so `constructor`, `toString` or `valueOf` would resolve to JS
// members instead of staying unbound (and `constructor(5)` would run Object(5)).
const DefaultEnvironment: Record<string, Value> = Object.assign(Object.create(null), {
  Null: null, null: null,
  SymbolAssign, CodeParse, CodeEvaluate,
  FunctionIterate, FunctionRepeat, FunctionThen, FunctionCommute, FunctionInto, FunctionFork, FunctionBefore, FunctionAfter,
  FunctionRight, FunctionLeft, FunctionIs, TensorIs, ListIs, StringIs, ErrorIs, Identical,
  FunctionCascade, FunctionCond, FunctionArity, FunctionApply, FunctionNoAutoLift,
  FunctionGuard, FunctionTry, FunctionWhen, ErrorCreate, NoMatch: NoMatchValue, doc,
  Reactive, SignalCreate, SignalComputed, SignalRead, SignalUpdate, SignalEffect, SignalOnce,
  ...UNARY, ...BINARY, ...REDUCTIONS,
  TensorStack, TensorUnstack, TensorConcat, TensorReduce, TensorOneHot, TensorSort, TensorArgSort, TensorGradient,
  TensorPermute, TensorBroadcast, TensorPadAxis, TensorWindow, TensorTake, TensorScatterAdd, TensorRange,
  TensorConvolution, TensorMeshgrid, TensorTopK, TensorReshape, TensorLength, TensorShape, TensorGather,
  TensorWhere, TensorSlice, TensorVariable, TensorData, TensorAssign, TensorWatch, TensorRandomUniform,
  List, ListLength, ListConcat, ListGet, ListMap, ListReduce, ListScan,
  StringConcat, StringLiteral, StringLength, StringToCodes, CodesToString,
  Storage,
})

// The host (or a test) extends the environment with widgets, live sources and printers
// before it creates scopes. Bun's HMR re-runs only this module when it is the one that
// changed, so the extensions also live on globalThis, and a fresh module re-applies
// them; the built-ins above stay fresh on each load.
const persistedExtensions: Record<string, Value> =
  ((globalThis as Record<string, unknown>).__fluentEnvExtensions ??= {}) as Record<string, Value>
Object.assign(DefaultEnvironment, persistedExtensions)
const extendEnvironment = (extra: Record<string, Value>) => {
  Object.assign(persistedExtensions, extra)
  Object.assign(DefaultEnvironment, extra)
}

// Code literals evaluate through this hook, so the IDE can draw syntax trees; headless,
// a literal is its tree node. Setting one gives back the one it replaces.
type CodeNodePrinter = (node: SyntaxTreeNode) => Value
let codeNodePrinter: CodeNodePrinter = (node) => node as unknown as Value
const setCodeNodePrinter = (printer: CodeNodePrinter): CodeNodePrinter => {
  const previous = codeNodePrinter
  codeNodePrinter = printer
  return previous
}

// MARK: Prelude
// What the language defines in itself lives in prelude.fl, in Fluent. It is parsed once,
// on first use: createScope runs on every evaluation, and a page that only parses never
// needs it.
let preludeTree: SyntaxTreeNode | null = null
const createScope = (): CurrentScope => {
  const scope = Object.create(DefaultEnvironment) as CurrentScope
  evaluateSyntaxTreeNode(preludeTree ??= CodeParse(PRELUDE), scope)
  return scope
}

// MARK: Exports
export {
  engine, FluentVariable, FluentData, isTensor, TensorScalarLive,
  CodeParse, getParseErrors,
  evaluateSyntaxTreeNode, CodeEvaluate, getAsSyncList, getMeta, setMeta, getOrigin,
  evaluateGeneration, flushRetirements, registerDisposable,
  NoMatch, StringLiteral,
  DefaultEnvironment, extendEnvironment, setCodeNodePrinter, PRELUDE, createScope,
  SignalCreate, SignalUpdate,
}
export type { Value, CurrentScope, SyntaxTreeNode, Origin, ParseError, FunctionMeta }
