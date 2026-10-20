// MARK: Fluent language core
// Grammar, evaluator, environment and prelude – no DOM, no React, no Monaco.
// The evaluation worker runs it (host.ts), and the end-to-end tests run it headless
// (tests.ts). Whoever computes starts the engine.

import { grammar, type ActionDict } from "ohm-js"
import { toAST as ohmToAST } from "ohm-js/extras"
import dedent from "ts-dedent"
import * as engine from "./engine/tensor.ts"
import CODE from "./prelude.fl" with { type: "text" }
import FONTS from "./fonts.fl" with { type: "text" }
import { OPERATOR_RANGES, IDENTIFIER_RANGES, numberText } from "./protocol"

// the Prelude: prelude.fl, then the fonts it draws with, which font.ts writes into fonts.fl
const PRELUDE = CODE + FONTS

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
  // a binary op's fold: what folding nothing gives, and a native reduction – `reduce +`
  // runs Σ rather than folding slice by slice
  identity?: Value
  reducer?: (x: Value, axis?: Value) => Value
}
const NO_META: FunctionMeta = Object.freeze({})
const getMeta = (fn: Function): FunctionMeta => (fn as any).__meta__ ?? NO_META
const setMeta = (fn: Function, updates: Partial<FunctionMeta>): void => { (fn as any).__meta__ = { ...getMeta(fn), ...updates } }

// Where a value came from in the source. A WeakMap, so a function or a frozen value
// has one too – a composed closure prints the source that made it.
const originMap = new WeakMap<object, Origin>()
const isObject = (v: unknown): v is object => v !== null && (typeof v === "object" || typeof v === "function")
const setOrigin = (value: unknown, origin: Origin | undefined): void => { if (origin && isObject(value)) { originMap.set(value, origin) } }
const getOrigin = (value: unknown): Origin | undefined => isObject(value) ? originMap.get(value) : undefined
const located = <T,>(value: T, node: { origin: Origin }): T => { setOrigin(value, node.origin); return value }

// A throw as an Error value. An engine error names the site of the op it is about – an op
// built long before a read forced it – and that is where it is reported.
const asError = (e: unknown): Error => {
  const error = e instanceof Error ? e : new Error(String(e))
  if (!getOrigin(error)) { setOrigin(error, (error as { site?: Origin }).site) }
  return error
}

// MARK: Naming
// A value is named by the reader's expression that made it: what an Error reports, what a
// function prints, which line an engine op came from. A function made during a call takes
// that call's origin, whoever wrote the builder, so it prints as the call. An Error made
// in another document – a prelude body, an evaluated string – takes the origin of the
// call that brought it back; inside the prelude, which no reader opens, it keeps its own.
// A NoMatch is a decline, not a failure: it keeps its own origin. Any other value keeps
// the origin it has as it passes through a call, or takes the call's when it has none –
// but a function the call didn't make, a built-in, takes none: it prints by its name.
let born = 0
const birth = new WeakMap<Function, number>()
const made = <F extends Function>(f: F): F => { birth.set(f, ++born); return f }
const isFailure = (v: unknown): v is Error => v instanceof Error && !(v instanceof NoMatch)
const named = (value: Value, node: { origin: Origin }, before: number): Value => {
  if (isFn(value)) { return (birth.get(value) ?? 0) > before ? located(value, node) : value }
  const origin = getOrigin(value)
  if (!origin) { return located(value, node) }
  const crossed = isFailure(value) && origin.document !== node.origin.document && node.origin.document !== PRELUDE
  return crossed ? located(value, node) : value
}

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
      lambdas.add(fn)
      return located(made(fn), node)
    }
    case "Operation": {
      const fn = evaluateSyntaxTreeNode(node.content.operator, env)
      // A binding among a call's arguments, f(a: 2, b), is local to the call; infix
      // operands are not an explicit ( … ), so a: b: 23 binds both. Only an operation can
      // bind, so a call with none among its arguments shares the caller's scope. The
      // arguments run in reading order; a quoted one – the name `:` binds – stays the name
      // it spells.
      const { synthetic, value: argNodes } = node.content.args.content
      const argsEnv = (synthetic || !argNodes.some((a) => a.type === "Operation") ? env : Object.create(env)) as CurrentScope
      const quoted = typeof fn === "function" ? getMeta(fn).quotedArgs : undefined
      const args = argNodes.map((arg, i) =>
        quoted?.includes(i) && arg.type === "Symbol" ? Symbol.for(arg.content.value) : evaluateSyntaxTreeNode(arg, argsEnv))
      return safeApply(fn, args, env, node)
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

// MARK: Cells
// Time is a sequence of instants: the program's run, a ⟳ tick, a button press, a write
// from the page, a frame from a source. A cell – $, ~ or ~~ – holds a value that changes
// only at an instant, by a write. A computed is a function of the cells: read, it runs
// again when a cell it read has been written since, so looking at a value never changes
// one. A read goes to whoever is reading – a computed keeps the cells it read, ∇ hands
// back the values it differentiates with respect to – and with no one reading it is the
// value now. While a value is computed or differentiated it writes only the cells it
// made: any other write would depend on how often it was looked at.
type Reader = { read: (cell: Signal) => unknown }
let reader: Reader | null = null, instant = 0
// In a definition – the program's body, a lambda it calls – a cell named bare is its
// value over time. In an occurrence – a ⟳ step, a press, ∇, a computed's own body – it is
// its value now.
let sampling = false
const occurring = <T,>(now: boolean, run: () => T): T => {
  const outer = sampling
  sampling = now
  try { return run() } finally { sampling = outer }
}
const occurrence = <T,>(run: () => T): T => occurring(true, run)
// the host hears of every write, to show what changed
let written = () => { }
const onWrite = (hear: () => void) => { written = hear }

// A write where it isn't allowed throws this; ∇ and a computed turn it into an Error
// value. It is control flow, so choosing passes it through.
class StateWrite extends Error {}

class Signal<T = Value> {
  // the instant its value last changed, and the evaluation that made it – the one reader
  // that may write it
  at = 0
  readonly maker = reader
  constructor(protected held: T) { }
  // whether ∇ differentiates with respect to it
  get trainable(): boolean { return false }
  get value(): T { return reader ? reader.read(this as Signal) as T : this.peek() }
  set value(next: T) { this.write(next) }
  peek(): T { return this.held }
  protected write(next: T): void {
    if (reader && this.maker !== reader) { throw new StateWrite("a write") }
    if (next !== this.held) { this.held = next; this.at = ++instant; engine.instant(); written() }
  }
}

// A computed runs again only when a cell it read has changed since it last looked – at
// most once an instant – and a run that gives the same value changes nothing downstream
class Computed<T = Value> extends Signal<T> {
  sources: [Signal, number][] = []
  checked = -1
  running = false
  constructor(readonly fn: () => T) { super(undefined as T) }
  // it reads a trainable cell – or may: it has not run yet
  override get trainable(): boolean { return this.checked < 0 || this.sources.some(([s]) => s.trainable) }
  override peek(): T {
    if (this.checked !== instant && (this.checked < 0 || this.sources.some(([s, at]) => (s.peek(), s.at !== at)))) { this.run() }
    this.checked = instant
    return this.held
  }
  protected override write(): void { throw new Error("a computed value is read-only: it follows what it reads") }
  private run(): void {
    if (this.running) { throw new Error("a value that reads itself") }
    const outer = reader, sources: [Signal, number][] = []
    reader = { read: (s) => { try { return s.peek() } finally { sources.push([s, s.at]) } } }
    this.running = true
    let value: T
    try {
      // its value crosses instants, so a tensor in it is settled into a leaf, as a cell's is
      value = heldValue(occurring(true, this.fn) as Value) as T
    } catch (e) {
      if (!(e instanceof StateWrite)) { throw e }
      value = new Error("a computed value cannot change state – change it in a step { … }") as T
    } finally {
      reader = outer
      this.running = false
    }
    if (value !== this.held) { this.held = value; this.at = instant }
    this.sources = sources
  }
}
const signal = <T,>(value: T) => new Signal<T>(value)
const computed = <T,>(fn: () => T) => new Computed<T>(fn)

// A variable (~): a cell holding one tensor, settled into a leaf, that ∇ differentiates
// with respect to. A data slot (~~) is the same cell read like a constant: no gradient
// flows into it, so no optimizer trains it. Both read bare – the name is the value now.
class FluentVariable extends Signal<engine.Tensor> {
  constructor(initial: engine.Tensor) { super(engine.settle(initial)) }
  override get trainable(): boolean { return true }
  get shape() { return this.value.shape }
  get size() { return this.value.size }
  assign(next: engine.Tensor): void { this.value = engine.settle(next) }
  override toString() { return this.peek().toString() }
}
class FluentData extends FluentVariable {
  override get trainable(): boolean { return false }
}

const isTensorish = (v: Value): boolean => isTensor(v) || v instanceof FluentVariable
const isText = (v: Value): v is string | String => typeof v === "string" || v instanceof String

// An op's operand: a variable's value or a tensor. Anything else fails here, in the
// tour's words – numbers, a list of numbers [ … ], a ( … ) list – with the fix in the
// message. An Error is a failure upstream: it passes through as it is.
const operand = (v: any): any => {
  if (v instanceof Signal) { return operand(v.value) }
  if (v == null) { throw new Error("expected numbers, got ◌ – is an argument missing?") }
  // an unbound name reached a numeric op (1 * x); as a value elsewhere it flows through
  if (typeof v === "symbol") { throw new Error(unknownName(v)) }
  // a list is a structure, not a bag of numbers – never coerced into a tensor
  if (Array.isArray(v)) { throw new Error("( … ) holds values side by side but does no math – write the numbers in [ … ]") }
  // a decline that reaches an op is a failure, said in the decline's words
  if (v instanceof NoMatch) { const e = new Error(v.message, { cause: v.cause }); setOrigin(e, getOrigin(v)); throw e }
  if (v instanceof Error) { throw v }
  return v
}
const unknownName = (s: symbol): string => `unknown name: ${Symbol.keyFor(s) ?? s.description}`

const shapeOf = (v: Value): number[] => (v instanceof FluentVariable ? v.value : (v as engine.Tensor)).shape

// A scalar is a rank-0 tensor holding a single phantom item (#(5) = 1), so index, sort
// and softmax on it give the rank-0 identity, not an error about an axis it lacks.
const isScalarTensor = (v: Value): boolean => isTensorish(v) && shapeOf(v).length === 0

// MARK: Evaluation generations
// Each top-level evaluation owns what it starts – live sources (Time, Camera, …) and ⟳
// loops. The next evaluation retires it: its loops stop, its sources let go of their
// hardware, once the new UI has replaced the old – a still-shown old island keeps moving
// until then – so retirement waits for the next frame, or a macrotask headless; tests and
// the host can flush at once.
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
  draws.clear()
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
// In a definition, an application that reads a cell – of any kind – is a computed that
// follows it; a native that computes reads the cells inside lists too, so
// concat((a, b), axis) follows a and b. In an occurrence, a native reads its cells now,
// and a lambda receives the cell itself, which it may write or pass on.
const containsSignal = (a: Value): boolean => a instanceof Signal || (Array.isArray(a) && a.some(containsSignal))
const unwrapSignals = (a: any): any => a instanceof Signal ? unwrapSignals(a.value) : Array.isArray(a) ? a.map(unwrapSignals) : a
const sampled = (a: Value): Value => a instanceof Signal ? sampled(a.value) : a
// What a function reads of its arguments: a native that computes reads the numbers in a
// list too; a lambda, and a function on lists, reads only what it is given – a list of
// cells is a structure, its cells their identities
const lambdas = new WeakSet<Function>(), shallow = new WeakSet<Function>()
const readsCell = (fn: Function, args: Value[]): boolean =>
  lambdas.has(fn) || shallow.has(fn) ? args.some((a) => a instanceof Signal) : args.some(containsSignal)

// The one way a function is applied. A signal is called to read it, s(), or to write it,
// s(v). An application whose arguments hold signals is a computed that recomputes with
// their current values, unless the function takes signals as they are (noAutoLift); the
// engine fuses whatever a chain of computeds builds, at the read. Called for a node, the
// result is named by it, and the engine ops it builds carry its site – a reader's call
// only: inside the prelude the reader's call that reached it stays the site.
// KNOWN WART: arity is not checked, so +(1, 2, 4) is 3 – see arity-plan.md.
function safeApply(fn: Value, args: Value[], env: CurrentScope, node?: { origin: Origin }): Value {
  // a cell holding a function is applied as the function it holds – in a definition, as
  // it holds it over time; a function is written into a cell with ←
  if (fn instanceof Signal && args.length && isFn(fn.peek())) {
    if (sampling) { return safeApply(fn.value, args, env, node) }
    const live = computed(() => safeApply(fn.value, args, env, node))
    return node ? located(live, node) : live
  }
  if (!isFn(fn) || getMeta(fn).noAutoLift) { return appliedAt(fn, args, env, node) }
  if (sampling) {
    if (lambdas.has(fn)) { return appliedAt(fn, args, env, node) }
    // a value on its way makes the result one too: computed now, and again on arrival
    const waiting = args.filter(isArriving), now = () => appliedAt(fn, args.map(sampled), env, node)
    return waiting.length ? new Arriving(now(), Promise.all(waiting.map((a) => a.arrival)).then(now)) : now()
  }
  if (!readsCell(fn, args)) { return appliedAt(fn, args, env, node) }
  // a lifted call is a live value, named by the call like any other; its body runs as an
  // occurrence, so a lambda reads the cells it is given there
  const read = lambdas.has(fn) ? (a: Value) => a : shallow.has(fn) ? sampled : unwrapSignals
  const live = computed(() => appliedAt(fn, args.map(read), env, node))
  return node ? located(live, node) : live
}
const appliedAt = (fn: Value, args: Value[], env: CurrentScope, node?: { origin: Origin }): Value => {
  if (!node) { return outcome(fn, args, env) }
  const before = born
  if (node.origin.document === PRELUDE) { return named(outcome(fn, args, env), node, before) }
  const outer = engine.at.site
  engine.at.site = node.origin
  try { return named(outcome(fn, args, env), node, before) } finally { engine.at.site = outer }
}

// fn applied to args, a throw caught as an Error value. An Error among the arguments is a
// failure upstream: a call it makes fail fails with that Error, unchanged – the first
// failure is the one to fix. A state write while differentiating is control flow, not a
// value: as an Error, a choice would read it as a candidate declining.
const outcome = (fn: Value, args: Value[], env: CurrentScope): Value => {
  let value: Value
  try {
    value = call(fn, args, env)
  } catch (e) {
    if (e instanceof StateWrite) { throw e }
    value = asError(e)
  }
  if (!(value instanceof Error)) { return value }
  const poison = args.find(isFailure)
  return poison && !causedBy(value, poison) ? poison : value
}
const causedBy = (e: unknown, cause: Error): boolean => {
  for (; e instanceof Error; e = e.cause) { if (e === cause) { return true } }
  return false
}
const call = (fn: Value, args: Value[], env: CurrentScope): Value => {
  if (fn instanceof Signal) {
    if (args.length === 0) { return fn.value }
    if (args.length === 1) { return SignalUpdate(fn, args[0]!) }
    throw new Error(`a live value is read with s() and written with s(v) – it takes no more than one argument, got ${args.length}`)
  }
  // calling a failure is that failure
  if (fn instanceof Error) { return fn }
  if (typeof fn === "symbol") { throw new Error(unknownName(fn)) }
  if (!isFn(fn)) { throw new Error(`${describeArg(fn)} is not a function`) }
  return fn.apply(env, args)
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
//
// A choice makes a decline each time it tries a candidate, so NoMatch.of makes one without
// the JavaScript stack an Error records: a decline is an answer, and its stack would cost
// more than the choice.
class NoMatch extends Error {
  // a decline by kind says what the candidate takes, and what it was given
  declare readonly expected?: string[]
  declare readonly got?: string
  static of(message: string, options?: { cause?: unknown, expected?: string[], got?: string }): NoMatch {
    return Object.assign(Object.create(NoMatch.prototype) as NoMatch, { message, ...options })
  }
}
NoMatch.prototype.name = "NoMatch"
// a candidate's decline of a value that isn't its kind, in the reader's words. An unknown
// name is no kind's value but a mistake, and it fails here as it does in arithmetic.
const decline = (expected: string, v: Value): Error =>
  typeof v === "symbol" ? new Error(unknownName(v)) : declining([expected], describeArg(v))
const declining = (expected: string[], got: string): NoMatch => {
  const kinds = expected.length < 2 ? expected.join("") : `${expected.slice(0, -1).join(", ")} or ${expected.at(-1)}`
  return NoMatch.of(`expected ${kinds}, got ${got}`, { expected, got })
}

// The first answer of the candidates, in order. A candidate that isn't a function is its
// own result, so an Error among them (a guard whose condition broke) surfaces and a
// NoMatch declines. A throw is a failure like any other; a native candidate declines by
// RETURNING a NoMatch. When none answers, the choice declines with what the declines say.
const chooseFirst = (candidates: Value[], args: Value[], scope: CurrentScope, declined: (declines: NoMatch[]) => NoMatch): Value => {
  const declines: NoMatch[] = []
  for (const candidate of candidates) {
    let result: Value
    try {
      result = typeof candidate === "function" ? candidate.apply(scope, args) : candidate
    } catch (e) {
      if (e instanceof StateWrite) { throw e }
      return asError(e)
    }
    if (!(result instanceof NoMatch)) { return result }
    declines.push(result)
  }
  return declined(declines)
}

// Candidates that each decline a value of another kind say together what they take:
// "expected numbers, a list or a string, got ◌". Otherwise the last decline is the
// cause – it says why the closest candidate said no.
const FunctionCascade = (candidates: Value) => {
  if (!Array.isArray(candidates)) {
    return new Error(`\`cascade\`: expected a list of candidates, got ${describeArg(candidates)} – write cascade((f, g, …))`)
  }
  return made(function (this: CurrentScope, ...args: Value[]) {
    return chooseFirst(candidates, args, this, (declines) => {
      const got = declines[0]?.got
      if (got && declines.every((d) => d.expected && d.got === got)) { return declining(declines.flatMap((d) => d.expected!), got) }
      return NoMatch.of(`no overload matched (${args.length === 0 ? "no arguments" : args.map(describeArg).join(", ")})`, { cause: declines.at(-1) })
    })
  })
}

// cond(a, b, …): cascade((a, b, …))() without the wrapper – its own entry point, so a
// recursion through it pays no setup per call
const FunctionCond = function (this: CurrentScope, ...candidates: Value[]): Value {
  return chooseFirst(candidates, [], this, (declines) => NoMatch.of("cond: no branch matched", { cause: declines.at(-1) }))
}

// Overloaded by arity: two operands run the binary op, one runs the unary op – how + is
// add and abs. Unlike a cascade, a binary op that fails (1 + a, with a unknown) is not
// retried as the unary one, so it is an Error, not a quiet abs(1).
const FunctionArity = (candidates: Function[]) => {
  const [binary, unary] = candidates
  const dispatch = made((a: Value, b: Value) => (b == null ? unary?.(a) : binary?.(a, b)))
  // its card is its candidates': `+` shows add and abs in one hover; a doc() on the
  // dispatcher afterwards still wins. A fold of it is its binary's.
  const [b, u] = [binary, unary].map((f): FunctionMeta => typeof f === "function" ? getMeta(f) : {}) as [FunctionMeta, FunctionMeta]
  setMeta(dispatch, { identity: b.identity, reducer: b.reducer })
  if (b.signature || u.signature) {
    setMeta(dispatch, {
      signature: [b.signature, u.signature].filter(Boolean).join("  ·  "),
      doc: [b.doc, u.doc && `With one argument: ${u.doc}`].filter(Boolean).join(" "),
      example: [b.example, u.example].filter(Boolean).join("\n") || undefined,
    })
  }
  return dispatch
}

// identity(f, v): f, knowing v as its identity element – what folding nothing with f
// gives. Written beside the card, as doc is, and given back the same way.
const FunctionIdentity = (f: Value, v: Value): Value => {
  if (isFn(f)) { setMeta(f, { identity: v }) }
  return f
}

// A tensor's data as nested JS arrays (a number for a scalar), for control flow and
// structural arguments – the engine computes it if it isn't yet
function getAsSyncList(value: unknown) {
  if (value instanceof Signal) { return getAsSyncList(value.value) }
  if (value instanceof engine.Tensor) { return value.js() }
}

// MARK: Functions

// Run Fluent source in the scope it is called from
// A program is a definition, wherever it is run
const CodeEvaluate = function (this: CurrentScope, program: Value) {
  return occurring(false, () => evaluateSyntaxTreeNode(CodeParse(String(program)), this))
}

// The loop under ⍣. FunctionRepeat(f, n) is f applied n times; with a predicate p in
// place of the count, f runs until p(next, previous) holds; with a negative count,
// invert(f) runs |n| times – the prelude passes its Newton inverse, so this loop never
// decides how inversion is done. A dyadic x (f ⍣ n) y iterates f(x, ·) from y, as in APL.
const REPEAT_UNTIL_BUDGET = 10000
const FunctionRepeat = function (this: CurrentScope, fn: Value, n: Value, invert?: Value) {
  if (!isFn(fn)) { return new Error(`\`(f ⍣ n)\`: \`f\` must be a function, got ${describeArg(fn)}`) }
  const until = isFn(n) ? n : null
  let times = 0
  if (!until) {
    if (!isTensor(n)) { return new Error("`(f ⍣ n)`: `n` must be a count or a predicate { new, old | … }") }
    const count = getAsSyncList(n)
    // a non-scalar or NaN count would skip the loop and give the identity
    if (typeof count !== "number" || !Number.isFinite(count)) {
      return new Error(`\`(f ⍣ n)\`: \`n\` must be one finite number, got ${Array.isArray(count) ? describeArg(n) : String(count)}`)
    }
    // a live slider drives counts through fractional values – round, like linspace
    times = Math.round(count)
    if (times < 0 && !isFn(invert ?? null)) {
      return new Error("`FunctionRepeat(f, n, invert)`: a negative count runs f backwards, which needs `invert`")
    }
  }
  const scope = this
  return made(function (...args: Value[]) {
    const dyadic = args.length >= 2, left = args[0]
    const forward = (z: Value) => safeApply(fn, dyadic ? [left!, z] : [z], scope)
    let step: Function = forward
    if (times < 0) {
      const inverse = safeApply(invert!, [forward], scope)
      if (!isFn(inverse)) { return inverse }
      step = inverse
    }
    let value: Value = (dyadic ? args[1] : args[0]) ?? null
    if ((until || times) && !gradients) {
      const looped = loopStep(step, until, value, until ? REPEAT_UNTIL_BUDGET : Math.abs(times), scope)
      if (looped !== undefined) { return looped }
    }
    for (let i = 0; until || i < Math.abs(times); i++) {
      if (until && i >= REPEAT_UNTIL_BUDGET) {
        return new Error(`\`(f ⍣ p)\`: the predicate didn't hold within ${REPEAT_UNTIL_BUDGET} steps`)
      }
      const prev: Value = value
      value = step(prev)
      // a failed step is the answer – iterating an Error only repeats it
      if (value instanceof Error) { return value }
      if (!until) { continue }
      const verdict = safeApply(until, [value, prev], scope)
      if (verdict instanceof Error) { return verdict }
      try { if (isTruthy(verdict, "⍣")) { break } } catch (e) { return asError(e) }
    }
    return value
  })
}
// The loop as one engine step, wherever no ∇ needs its history: step, and the predicate,
// run once on placeholders shaped like the state – a tensor, or a list of them – and the
// engine runs what they built. A step that reads the state's numbers, writes a cell, draws
// random numbers or gives anything but numbers of the state's shapes runs step by step
// instead (undefined), and the draws it made are taken back.
const loopStep = (step: Function, until: Value, value: Value, n: number, scope: CurrentScope): Value | undefined => {
  const list = Array.isArray(value), xs = list ? value : [value]
  if (!xs.length || !xs.every(isTensor)) { return undefined }
  const outer = reader, before = new Map(draws), drawn = drawCount
  reader = { read: (s) => outer ? outer.read(s) : s.peek() }
  try {
    const out = engine.loop(xs, n, isFn(until), (cells) => {
      const s = list ? cells : cells[0]!, next = step(s)
      const ns = list ? (Array.isArray(next) && next.length === cells.length ? next : []) : [next]
      const verdict = isFn(until) && ns.length && ns.every(isTensor) ? safeApply(until, [next, s], scope) : null
      if (drawCount !== drawn || !ns.length || !ns.every(isTensor) || (isFn(until) && !isTensor(verdict))) { return undefined }
      return isTensor(verdict) ? [...ns, verdict] : ns
    })
    if (out) { return list ? out : out[0]! }
  } catch (e) {
    if (!(e instanceof StateWrite)) { return asError(e) }
  } finally {
    reader = outer
  }
  draws.clear()
  before.forEach((v, k) => draws.set(k, v))
  return undefined
}

// MARK: Memo
// memo(f) is f keeping its answers, found again by the value of the arguments: a string
// by its text, a tensor the host holds by its shape and numbers, a list by its items. An
// argument with no such value – a tensor on the GPU, a function, a signal – is never a
// key, and f runs. For an f whose answer depends on its arguments alone; the 256 answers
// used last are kept.
const MEMO_KEPT = 256, MEMO_NUMBERS = 1024
const memoKey = (v: Value): string | null => {
  if (v === null) { return "◌" }
  if (isText(v)) { return JSON.stringify(String(v)) }
  if (isTensor(v)) {
    const d = v.size <= MEMO_NUMBERS ? engine.hostValue(v) : undefined
    return d ? `[${v.shape}]${d.join(",")}` : null
  }
  if (Array.isArray(v)) {
    const keys = v.map(memoKey)
    return keys.every((k) => k !== null) ? `(${keys.join(";")})` : null
  }
  return null
}
const FunctionMemo = (f: Value) => {
  if (!isFn(f)) { return new Error("`memo(f)`: `f` must be a function") }
  const kept = new Map<string, Value>()
  return made(function (this: CurrentScope, ...args: Value[]) {
    const keys = args.map(memoKey)
    if (keys.some((k) => k === null)) { return safeApply(f, args, this) }
    const key = keys.join("|")
    const value = kept.has(key) ? kept.get(key)! : safeApply(f, args, this)
    kept.delete(key)
    kept.set(key, value)
    if (kept.size > MEMO_KEPT) { kept.delete(kept.keys().next().value!) }
    return value
  })
}

// MARK: Combinators
// The tacit aviary, after Conor Hoekstra's taxonomy (combinatorylogic.com), is written in
// Fluent in prelude.fl, on FunctionArity – all but ∘, below. Each combinator is a plain
// higher-order value, so a composed function drops into operator position like any
// function: a (f ∘ g) b.

const isFn = (v: Value): v is Function => typeof v === "function"

// Q (Queer bird) – then: apply f, then g to the result. Reading order is evaluation
// order – (f ∘ g)(…x) = g(f(…x)) – so this is APL's atop (B/B₁) with the tines swapped
// to read left to right, like the . pipe. It passes on any number of arguments, so
// List ∘ f is a function of any number of them.
const FunctionThen = (f: Value, g: Value) => {
  if (!isFn(f) || !isFn(g)) { return new Error("`(f ∘ g)`: both operands must be functions") }
  return made(function (this: CurrentScope, ...args: Value[]) { return safeApply(g, [safeApply(f, args, this)], this) })
}

// What kind of value – 1 or 0, like a comparison, so it composes with guard and where.
// isFunction is the question the prelude's hooks and fork ask before they hold an
// operand constant.
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

// The loops with steps still to run: while there is one, the display takes only its share
// of the worker (host.ts)
const behind = new Set<object>()
const stepsWaiting = (): boolean => behind.size > 0

// Async repeat (⟳): runs fn between frames so the UI stays live. The loop belongs to the
// evaluation that started it – a re-evaluation cancels it. It gives back a signal that
// holds ◌ while the steps succeed; a step that fails stops the loop, and the signal
// holds its Error. (⍣ is the synchronous f(f(f(x))).)
const FunctionIterate = function (this: CurrentScope, fn: Value, iterations?: Value) {
  const live = iterations instanceof Signal
  if (!isFn(fn) || !(iterations == null || isTensor(iterations) || live)) {
    throw new Error("`FunctionIterate(fn, iterations)`: `fn` must be a function and `iterations` one number")
  }
  // The count is a BUDGET: f ⟳ n runs f n times in total. A live count is read fresh –
  // raise it and the loop runs on to the new total, lower it below what has run and it
  // stops – so a constant signal means what the literal means. The loop samples it: a
  // count is not something the loop is a function of.
  const readTarget = (): number => {
    if (iterations == null) { return 1 }
    const n = getAsSyncList(iterations instanceof Signal ? iterations.peek() : iterations)
    // a non-scalar count is never reached – an infinite loop, not an error
    if (typeof n !== "number" || Number.isNaN(n)) {
      throw new Error("`FunctionIterate(fn, iterations)`: `iterations` must be one number")
    }
    return n
  }
  // a bad count fails the call
  readTarget()

  const generation = currentGeneration, scope = this, failure = signal<Value>(null)
  let i = 0, batch = 1, prev = 0, scheduled = false, cancelled = false
  registerDisposable(() => { cancelled = true; behind.delete(failure) })
  const gone = () => generation !== currentGeneration || cancelled

  // Paced by display frames: steps align to the display, a hidden tab pauses the loop,
  // and a batch of steps per frame amortizes the frame. The batch grows while frames
  // keep vsync (~16.7 ms) and halves when one slips (~33 ms), so a heavy step or plot
  // runs a few per frame and a cheap one many; the cap keeps the GPU from backlogging.
  const frame = async (t: number) => {
    scheduled = false
    if (gone()) { return }
    // idle, not finished: a live budget may grow again, so the loop keeps looking
    const target = readTarget()
    if (i >= target) { behind.delete(failure); if (live) { schedule() } return }
    behind.add(failure)
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
      const result = occurrence(() => safeApply(fn, [engine.scalar(i)], scope))
      if (result instanceof Error) { cancelled = true; behind.delete(failure); failure.value = result; return }
      i++
      stepTime += performance.now() - start
      if (stepTime > 25) { break }
      if (yieldToInput && i < end) {
        await yieldToInput()
        if (gone()) { return }
      }
    }
    if (i >= target) { behind.delete(failure) }
    schedule()
  }
  // one pending frame at a time, or every wake-up would start another pump
  const schedule = () => { if (!scheduled && !cancelled) { scheduled = true; nextFrame(frame) } }
  schedule()
  return failure
}

// Fresh scalar for time-varying sources (Time, sliders, scrubbers): held as data, never
// folded into a kernel – the clock ticking or a slider moving reuses the compiled plan
const TensorScalarLive = (value: number): engine.Tensor => engine.scalar(value)

// The display's density: how many of its pixels make a unit, a CSS pixel – the page's
// devicePixelRatio, 1 where there is no page. The page writes it (host.ts); Density() is
// the cell, read-only, so a picture drawn in units follows it.
const density = signal<Value>(TensorScalarLive(1))
const densityShown = computed(() => density.value)
const Density = () => densityShown

// A signal holds data: a tensor stored in it – or in a list in it – is settled into a
// leaf, so a value written back every tick (⟳ with ←) never drags the previous ticks along
const heldValue = (v: Value): Value => Array.isArray(v) ? v.map(heldValue) : isTensor(v) ? engine.settle(v) : v

const SignalCreate = (<T,>(initial: T) => signal(heldValue(sampled(initial as Value)))) as typeof signal
const SignalComputed = computed
const SignalRead = (s: Value) => s instanceof Signal ? s.value : new Error(`'SignalRead': expected a signal, got ${describeArg(s)}`)
// A write stores the value now – a live value is read, never followed; a list keeps the
// cells it holds, as identities. A value still on its way is written again when it
// arrives. A variable's write is :=, for tensors only.
const SignalUpdate = (s: Value, v: Value): Value => {
  if (s instanceof FluentVariable) { return TensorAssign(s, v) }
  if (!(s instanceof Signal)) { return new Error(`'SignalUpdate': expected a signal, got ${describeArg(s)}`) }
  if (s instanceof Computed) { return new Error("this live value is made from others – write the values it is made from") }
  if (isArriving(v)) { v.arrival.then(() => SignalUpdate(s, v)) }
  s.value = heldValue(sampled(v))
  return null
}
// $v: a signal holding v, or a computed signal of a thunk
const Reactive = (a: Value) => isFn(a) ? SignalComputed(a as () => Value) : SignalCreate(a)

// name: value binds in the scope it is called from; the name arrives quoted. A binding is
// the scope's own, even over a name the environment reads through a getter (Tour).
const SymbolAssign = function (this: CurrentScope, name: Value, made: Value) {
  if (typeof name !== "symbol") { return new Error(`\`name: value\` binds a name, got ${describeArg(name)}`) }
  const key = Symbol.keyFor(name) ?? name.description ?? "", value = kept(this, key, made)
  try { this[key] = value } catch { Object.defineProperty(this, key, { value, writable: true, enumerable: true, configurable: true }) }
  return value ?? null
}

// MARK: Keeping cells across edits
// An edit runs the program again, in a fresh scope. A top-level ~ or ~~ is the cell of the
// run before when it is bound under the same name, made by the same expression, to the
// same shape – so an edit elsewhere keeps what training learned, and an edit of the cell's
// own expression starts it afresh. What an optimizer remembers of a variable stays with
// the variable, so a kept variable keeps its moments too.
let keeping: { from: CurrentScope, into: CurrentScope } | null = null
const keepingFrom = <T,>(from: CurrentScope | null, into: CurrentScope, run: () => T): T => {
  keeping = from && { from, into }
  try { return run() } finally { keeping = null }
}
const kept = (scope: CurrentScope, key: string, value: Value): Value => {
  if (!keeping || scope !== keeping.into || !(value instanceof FluentVariable) || !Object.hasOwn(keeping.from, key)) { return value }
  const old = keeping.from[key]
  return old instanceof FluentVariable && old.constructor === value.constructor && getOrigin(old)?.source === getOrigin(value)?.source
    && old.peek().shape.join() === value.peek().shape.join() ? old : value
}
// the slots an optimizer's init made for a variable, by the init's source
const memories = new WeakMap<FluentVariable, Map<string, Value>>()
const TensorMemoryOf = function (this: CurrentScope, v: Value, init: Value, make: Value): Value {
  const fresh = isFn(make) ? make.apply(this) : make
  if (!(v instanceof FluentVariable) || !Array.isArray(fresh)) { return fresh }
  const key = (isFn(init) && getOrigin(init)?.source) || "", byInit = memories.get(v) ?? new Map<string, Value>()
  memories.set(v, byInit)
  const old = byInit.get(key), shapes = (m: Value) => Array.isArray(m) ? m.map((s) => s instanceof FluentVariable ? s.peek().shape.join() : "").join(";") : ""
  if (Array.isArray(old) && shapes(old) === shapes(fresh)) { return old }
  byInit.set(key, fresh)
  return fresh
}
setMeta(TensorMemoryOf, { noAutoLift: true })
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
  const wrapper = made(function (this: CurrentScope, ...args: Value[]) { return occurrence(() => fn.apply(this, args)) })
  setMeta(wrapper, { ...getMeta(fn), noAutoLift: true })
  return wrapper
}

// Fluent's truth, the engine's one rule for a condition (engine.oneNumber): guard, when and
// the until-form of ⍣ read one number to answer.
const isTruthy = (v: Value, who: string): boolean => {
  const t = operand(v)
  if (!isTensor(t)) { throw new Error(`expected numbers, got ${describeArg(v)}`) }
  const n = engine.oneNumber(t, who).item()
  return n !== 0 && !Number.isNaN(n)
}

// guard(cond, { value }) – a candidate that answers with value while cond holds and
// declines otherwise. The condition is read strictly, at the guard: a typo'd name or an
// Error there is a failure, not a quiet "no" – the guard itself becomes the Error, and
// a cascade surfaces it. A body that isn't a thunk is its own value.
const FunctionGuard = function (this: CurrentScope, cond: Value, body: Value) {
  const holds = isTruthy(cond, "guard"), scope = this
  const candidate = made(function () {
    if (holds) { return isFn(body) ? body.apply(scope) : body }
    // the guard's own call site, so hovering the decline finds it
    const no = NoMatch.of("guard: the condition doesn't hold")
    setOrigin(no, getOrigin(candidate))
    return no
  })
  return candidate
}

// try({ value }) – a candidate that declines when value FAILS: the failure becomes a
// NoMatch, kept as its cause, so a cascade moves on. A candidate's arguments pass
// through, so try(f) joins an overload set as f would.
const FunctionTry = function (this: CurrentScope, body: Value) {
  const scope = this
  return made(function (...args: Value[]) {
    let result: Value
    try {
      result = isFn(body) ? body.apply(scope, args) : body
    } catch (e) {
      if (e instanceof StateWrite) { throw e }
      result = asError(e)
    }
    return isFailure(result) ? NoMatch.of("try: it failed", { cause: result }) : result
  })
}

// when(cond, { … }) – a conditional effect: run the thunk while cond is truthy, yield ◌
// otherwise. Not a candidate: whatever the thunk returns – an Error included – is the
// result, so a broken training step stays loud.
const FunctionWhen = function (this: CurrentScope, cond: Value, thunk: Value) {
  if (!isTruthy(cond, "when")) { return null }
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
for (const f of [FunctionCond, FunctionIterate, SignalRead, SignalUpdate, Reactive, SymbolAssign, Identical,
  FunctionNoAutoLift, FunctionGuard, FunctionTry, FunctionWhen, doc, FunctionIdentity]) { setMeta(f, { noAutoLift: true }) }

// MARK: Lists
const List = (...args: Value[]) => args

const ListMap = function (this: CurrentScope, a: Value, fn: Value) {
  if (!isFn(fn)) { return new Error("'ListMap': 'fn' must be a function") }
  if (!Array.isArray(a)) { return new Error("'ListMap': 'a' must be a list") }
  return a.map((v, i) => safeApply(fn, [v, engine.tensor(i)], this))
}
// a list is a structure: a function on lists reads the list it is given, not the cells
// inside it – ListMap, and the list members of the verbs on items – and List makes one
// of what it is given
shallow.add(ListMap)
setMeta(List, { noAutoLift: true })

// MARK: Tensors
// What a value is, for a message, in the tour's words: a number, a list of numbers
// [ … ], a ( … ) list, a live value $( )
const describeArg = (v: Value): string =>
  typeof v === "symbol" ? `the unknown name ${Symbol.keyFor(v) ?? v.description}`
    : v == null ? "◌"
    : v instanceof Error ? "an Error"
    : v instanceof FluentVariable ? "a trainable variable"
    : v instanceof Signal ? "a live value"
    : Array.isArray(v) ? `a ( … ) list of ${v.length}`
    : isText(v) ? "a string"
    : isFn(v) ? "a function"
    : isTensor(v) ? numbers(v.shape)
    : "a value"
const numbers = (shape: readonly number[]): string =>
  shape.length === 0 ? "a number"
    : shape.length === 1 ? `a list of ${shape[0]} ${shape[0] === 1 ? "number" : "numbers"}`
    : `numbers of shape [${shape.join(" ")}]`

// An axis, a size, an index or a count, read out of a tensor argument. Strict: an unknown
// name or an Error here fails, rather than coerce to NaN and change what the op does.
const requireData = (v: Value): number | unknown[] => {
  const data = getAsSyncList(operand(v))
  if (typeof data !== "number" && !Array.isArray(data)) { throw new Error(`expected numbers, got ${describeArg(v)}`) }
  return data
}
// one number: a scalar, or a tensor of one element
const asNumber = (v: Value): number => {
  const data = [requireData(v)].flat(Infinity) as number[]
  if (data.length !== 1) { throw new Error(`expected one number, got ${describeArg(v)}`) }
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
    throw new Error(`a string (${JSON.stringify(String(v))}) is not a number – + and × need numbers; join text with StringConcat`)
  }
  if (isFn(v)) { throw new Error("a function is not a number – + and × need numbers; call it first, f(x)") }
  return operand(v)
}

// A tensor operand for an op that works on numbers only: anything else is named here, in
// the learner's words, rather than handed to the engine
const tensorArg = (v: Value): engine.Tensor => {
  const t = operand(v)
  if (!isTensor(t)) { throw new Error(`expected numbers, got ${describeArg(v)}`) }
  return t
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
  setMeta(BINARY[op]!, { reducer: REDUCTIONS[reduction] as (x: Value, axis?: Value) => Value })
}

// stack broadcasts its inputs to a common shape, as arithmetic does: stack(x - a, y - b)
// works when the pieces only meet by broadcasting
const broadcastRefs = (values: Value[]): engine.Tensor[] => {
  const tensors = values.map((v) => v instanceof FluentVariable ? v.value : v)
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
// (f ⍤ k): f on each rank-k cell, mapped over the frame of leading axes – a negative k
// fixes the frame's rank instead. Two frames agree as they do under +. f runs once, on a
// cell, and the engine gives the graph it built the frame; an f that reads its cell's
// numbers – a branch on them, a bounds check – or writes state, which it may not do
// while it is traced, runs cell by cell instead. With no cells, f on a cell of zeros
// tells the shape of the result's cells, as in J.
const TensorRank = (f: Value, k?: Value) => {
  if (!isFn(f)) { return new Error(`\`(f ⍤ k)\`: \`f\` must be a function, got ${describeArg(f)}`) }
  const rank = k == null ? 0 : asWhole(k)
  return made(function (this: CurrentScope, a: Value, b?: Value) {
    const args = b == null ? [a] : [a, b], xs = args.map(operand) as engine.Tensor[]
    const frameRank = (r: number) => rank >= 0 ? Math.max(r - rank, 0) : Math.min(-rank, r)
    let F: number[]
    try { F = xs.map((x) => engine.zeros(x.shape.slice(0, frameRank(x.ndim)))).reduce((p, q) => engine.add(p, q)).shape } catch (e) {
      return new Error("`(f ⍤ k)`: dyadic rank needs frames that match or broadcast", { cause: asError(e) })
    }
    if (!F.length) { return safeApply(f, args, this) }
    const framed = xs.map((x) => {
      const r = frameRank(x.ndim), cell = x.shape.slice(r)
      return engine.broadcastTo(engine.reshape(x, [...Array(F.length - r).fill(1), ...x.shape]), [...F, ...cell])
    })
    try {
      const view = differentiating(null, () => engine.overFrame(F, framed, (cells) => safeApply(f, cells, this)))
      if (view) { return view }
    } catch (e) {
      if (!(e instanceof StateWrite)) { throw e }
    }
    const cells = framed.map((x) => x.shape.slice(F.length)), n = F.reduce((p, q) => p * q, 1)
    if (n === 0) {
      const z = safeApply(f, cells.map((c) => engine.zeros(c)), this)
      return isTensorish(z) ? engine.zeros([...F, ...shapeOf(z)]) : z
    }
    const items = framed.map((x, i) => [...engine.reshape(x, [n, ...cells[i]!])])
    const rs = Array.from({ length: n }, (_, j) => safeApply(f, items.map((c) => c[j]!), this))
    // the first cell that failed is the answer; a live cell's result makes the stack live
    const stacked = (done: Value[]): Value => {
      const st = done.find(isFailure) ?? TensorStack(done)
      return isTensor(st) ? engine.reshape(st, [...F, ...st.shape.slice(1)]) : st
    }
    return safeApply(stacked, [rs], this)
  })
}

// Index values for a bounds check: read when they are on the host or cheap to compute
// there; values still on the GPU – a minibatch drawn from a data slot – flow through
// unchecked, as trusted data
const cheapValues = (v: Value): Float32Array | undefined => {
  const t = v instanceof FluentVariable ? v.value : v
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

// A traced body reads through a reader of its own. What it reads is still read for
// whoever reads around it; a trainable cell's value comes back through `trace`; a
// computed that reads a trainable cell runs again inside, so ∇ differentiates its
// definition, not its last value. The body writes only the cells it made: ∇ would have to
// undo a write along every direction it takes.
const differentiating = <T,>(trace: ((cell: Signal, value: unknown) => unknown) | null, run: () => T): T => {
  const outer = reader, inside = new Map<Signal, unknown>(), outerSampling = sampling
  sampling = true
  reader = {
    read: (s) => {
      if (s instanceof Computed && (s.trainable || (s.peek(), s.trainable))) {
        if (!inside.has(s)) { inside.set(s, s.fn()) }
        return inside.get(s)
      }
      const v = outer ? outer.read(s) : s.peek()
      return trace && s.trainable ? trace(s, v) : v
    },
  }
  try { return run() } finally {
    reader = outer
    sampling = outerSampling
  }
}
// a write a differentiated body tries is an Error that names the body; `gradients` counts
// the ∇s running
let gradients = 0
const differentiated = (body: string, trace: ((cell: Signal, value: unknown) => unknown) | null, run: () => Value): Value => {
  gradients++
  try {
    return differentiating(trace, run)
  } catch (e) {
    if (e instanceof StateWrite) { return new Error(`${body} cannot change state while it is differentiated (${e.message})`) }
    throw e
  } finally {
    gradients--
  }
}
// vjp with a ones cotangent: a scalar output gives the classic gradient, an elementwise
// one per-element derivatives – ∇({x | x^2}) works on vectors
// A loss written without braces is a value, read once: grad needs the function that
// recomputes it.
const TensorGradient = (f: Value) => {
  if (!isFn(f)) { return new Error(`\`grad(f)\`: \`f\` must be a function, got ${describeArg(f)} – write it in braces, { … }, so each step recomputes it`) }
  return made((x: Value) => x == null ? gradientOfReads(f) : differentiated("`grad(f)`: `f`", null, () => {
    const [out, pullback] = engine.vjp((traced: engine.Tensor) => {
      const value = unwrapSignals(f(traced))
      if (value instanceof Error) { throw value }
      if (!isTensor(value)) { throw new Error(`\`grad(f)\`: \`f\` must give numbers, got ${describeArg(value)}`) }
      return value
    }, [operand(x)])
    return pullback(engine.full(out.shape, 1))[0]!
  }))
}
// ∇(loss)(): with nothing to differentiate with respect to, the derivative is with respect
// to the variables the loss reads – found by reading them, as a computed finds the cells
// it depends on. It comes back with the value, all computed in one pass: (value,
// variables, gradients). An enclosing ∇ still differentiates through it.
const gradientOfReads = (loss: Function): Value => {
  const reads = new Map<Signal, engine.Tensor>()
  const value = differentiated("`∇(loss)()`: the loss", (v, value) => {
    let t = reads.get(v)
    if (!t) { t = engine.traced(value as engine.Tensor); reads.set(v, t) }
    return t
  }, () => unwrapSignals(loss()))
  if (value instanceof Error) { return value }
  if (!isTensor(value)) { return new Error(`\`∇(loss)()\`: the loss must give numbers, got ${describeArg(value)}`) }
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

const TensorShape = (a: Value) => isTensorish(a) ? engine.tensor(shapeOf(a)) : new Error(`shape: expected numbers, got ${describeArg(a)}`)

// scatterAdd(indices, values, size): [size, …] zeros with each row of values added at its
// index – take's transpose
const TensorScatterAdd = (indices: Value, values: Value, size: Value) =>
  engine.scatterAdd(operand(indices), operand(values), asWhole(size))
const TensorWhere = (a: Value, b: Value, c: Value) => engine.where(engine.truthy(operand(a)), operand(b), operand(c))

// A cell is made with its initial value as it is now: a live value is read, not followed
const TensorVariable = (init: Value) => {
  const a = sampled(init)
  return isTensorish(a) ? new FluentVariable(operand(a)) : new Error(`\`~(init)\`: the initial value must be numbers, got ${describeArg(a)}`)
}
const TensorData = (init: Value) => {
  const a = sampled(init)
  return isTensorish(a) ? new FluentData(operand(a)) : new Error(`\`~~(init)\`: the initial value must be numbers, got ${describeArg(a)} – one slot holds one list of numbers`)
}
for (const f of [TensorVariable, TensorData]) { setMeta(f, { noAutoLift: true }) }

// A write is an occurrence: it stores the value now – a live value is read, not followed
const TensorAssign = (a: Value, live: Value): Value => {
  const b = unwrapSignals(live)
  // a list of variables takes a list of as many values, computed together
  if (Array.isArray(a)) {
    if (!Array.isArray(b) || b.length !== a.length) { return new Error("`:=`: a list of variables takes a list of as many values") }
    const bad = a.findIndex((v, i) => !(v instanceof FluentVariable) || !isTensorish(b[i]!))
    if (bad >= 0) { return TensorAssign(a[bad]!, b[bad]!) }
    const values = b.map(operand) as engine.Tensor[]
    engine.materialize(values)
    a.forEach((v, i) => { (v as FluentVariable).assign(values[i]!) })
    return null
  }
  if (!(a instanceof FluentVariable)) { return new Error(`\`:=\`: the left side must be a variable made with ~ or ~~, got ${describeArg(a)}`) }
  // an Error (an emptied corpus, a failed expression) doesn't corrupt the variable: it
  // keeps its last good value, and the Error surfaces
  if (!isTensorish(b)) { return new Error(`\`:=\`: the value must be numbers, got ${describeArg(b)}`) }
  a.assign(operand(b))
  return null
}
setMeta(TensorAssign, { noAutoLift: true })

// A draw is keyed by where it is made – the reader's call that reached it – and by how
// many draws that call has made in this run: the same program draws the same numbers,
// and a value drawn elsewhere, looked at or not, moves no other draw
const draws = new Map<string, number>()
let drawCount = 0
const TensorRandomUniform = (a: Value) => {
  const site = engine.at.site, here = site ? `${site.start.line}:${site.start.column}:${site.source}` : ""
  const n = draws.get(here) ?? 0
  draws.set(here, n + 1)
  drawCount++
  let key = 2166136261
  for (let i = 0; i < here.length; i++) { key = Math.imul(key ^ here.charCodeAt(i), 16777619) }
  return engine.uniform(Math.imul(key ^ n, 0x9e3779b1) >>> 0, asWholeList(a))
}

// MARK: Strings
// A value as the Fluent SOURCE that reproduces it, so CodeEvaluate(String(x)) gives x
// back. This is repr, not display: a string comes back quoted, a list keeps its parens,
// a tensor its rank. Joined with commas, as a JS string coercion does, the text would
// not just lose its brackets – it would re-parse as a SEQUENCE, `[1, 2, 3]` as 3.
//
// The law holds exactly for the values that are data: numbers, tensors of any rank,
// lists, strings, ◌. A function prints as the expression that made it – a literal, the
// call that built it, a built-in's name – so it comes back wherever those names are in
// scope; never as its JavaScript. It cannot hold for values with identity – a signal or a
// `~` variable round-trips to a NEW one, equal but not the same. Those are best effort.
const numberLiteral = (n: number): string =>
  Number.isFinite(n) ? numberText(n)
    // no literal for these, but these expressions evaluate to them
    : Number.isNaN(n) ? "(0 ÷ 0)"
    : n > 0 ? "(1 ÷ 0)" : "(0 - 1 ÷ 0)"
const nestedLiteral = (d: unknown): string => Array.isArray(d) ? `[${d.map(nestedLiteral).join(", ")}]` : numberLiteral(Number(d))

const StringLiteral = (v: Value): string => {
  if (v == null) { return "◌" }
  if (isText(v)) { return `"${String(v).replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"` }
  if (typeof v === "symbol") { return Symbol.keyFor(v) ?? String(v) }
  if (v instanceof FluentVariable) { return `${v instanceof FluentData ? "~~" : "~"}(${StringLiteral(v.value)})` }
  if (v instanceof Signal) { return `$(${StringLiteral(v.peek())})` }
  if (isTensor(v)) { return nestedLiteral(getAsSyncList(v)) }
  // a one-element list needs its trailing comma, or the parens are just grouping
  if (Array.isArray(v)) { return v.length === 1 ? `(${StringLiteral(v[0]!)},)` : `(${v.map(StringLiteral).join(", ")})` }
  if (isFn(v)) { return getOrigin(v)?.source ?? ((!birth.has(v) && v.name) || "(function)") }
  // an Error has no literal form – it shows itself
  return String(v)
}

// Text ↔ tensor: the door between strings and token land. Codes are float32 – small
// integers are exact in float, and float spares downstream math from int-promotion
// surprises. Decode rounds, so model outputs convert directly.
const StringToCodes = (a: Value) =>
  isText(a) ? engine.tensor(Float32Array.from(String(a), (c) => c.codePointAt(0)!)) : new Error("`StringToCodes(text)`: expected a string")

const codesText = (codes: ArrayLike<number>): string | Error => {
  let text = ""
  for (let i = 0; i < codes.length; i++) {
    const code = codes[i]!
    if (!Number.isFinite(code)) { return new Error("`CodesToString(codes)`: expected a list of character codes") }
    text += String.fromCodePoint(Math.min(0x10FFFF, Math.max(0, Math.round(code))))
  }
  return text
}
// A value on its way: a cell that holds a placeholder until the value arrives. The
// arrival is an occurrence of its own – it writes the cell – and a write made from the
// value waits for it.
class Arriving extends Signal {
  settled = false
  constructor(placeholder: Value, readonly arrival: Promise<Value>) {
    super(placeholder)
    arrival.then((v) => { this.settled = true; this.value = v }, (e) => { this.settled = true; this.value = asError(e) })
  }
}
const isArriving = (a: Value): a is Arriving => a instanceof Arriving && !a.settled
// Text is for showing, so it never makes the page wait: codes still being computed on the
// GPU give text on its way, "" until it arrives. Codes already at hand give the string.
const CodesToString = (a: Value) => {
  const t = a instanceof FluentVariable ? a.value : a
  if (!isTensor(t)) { return new Error(`\`CodesToString(codes)\`: expected a list of character codes, got ${describeArg(a)}`) }
  const known = engine.hostValue(t)
  if (known) { return codesText(known) }
  const arrival = t.read().then(codesText)
  arrivals?.push(arrival)
  return new Arriving("", arrival)
}
// The reads a computation starts that are still on their way: a value that started one
// is not finished until they arrive
let arrivals: Promise<unknown>[] | null = null
const awaiting = <T,>(run: () => T): [T, Promise<unknown>[]] => {
  const outer = arrivals, started: Promise<unknown>[] = arrivals = []
  try { return [run(), started] } finally { arrivals = outer }
}

// MARK: Items
// A container is a row of items: a tensor's rows – a number is a row of one – a list's
// elements, a string's characters. A verb on items has a member for each kind, named for
// it – TensorTake, ListTake, StringTake – that declines a value of another kind, and the
// prelude's verb is the cascade of the three. A member that takes the value and then
// fails gives an Error, which stops the cascade.
const chars = (s: Value): string[] => [...String(s)]
const rowsOf = (a: Value): engine.Tensor => { const x = tensorArg(a); return x.ndim ? x : engine.reshape(x, [1]) }
// where take(n) keeps the items – the first n, or the last -n – or drop(n) leaves them;
// an n past the end takes them all
const span = (length: number, n: Value, take: boolean): [number, number] => {
  const k = Math.max(-length, Math.min(length, asWhole(n))), at = k < 0 ? length + k : k
  return take === k >= 0 ? [0, at] : [at, length]
}
const cut = <T,>(items: T[], n: Value, take: boolean): T[] => items.slice(...span(items.length, n, take))

// A tensor's length along an axis, too: a missing axis has size 1, never NaN, so #(5) is 1
// (APL's tally ≢), as broadcasting treats an absent axis; a negative axis counts from the
// end. A string counts characters, not UTF-16 units, as StringToCodes does.
const TensorLength = (a: Value, b?: Value) => {
  if (!isTensorish(a)) { return decline("numbers", a) }
  const shape = shapeOf(a), axis = b == null ? 0 : asWhole(b)
  return engine.tensor(shape[axis < 0 ? axis + shape.length : axis] ?? 1)
}
const ListLength = (a: Value) => Array.isArray(a) ? engine.tensor(a.length) : decline("a list", a)
const StringLength = (a: Value) => isText(a) ? engine.tensor(chars(a).length) : decline("a string", a)

// x_i picks the item at i, x_[i, j] the items at i and j, in x's own kind – a tensor's
// indices may have any shape. A negative index counts from the end. Every tensor index on
// the host or cheap to compute there is bounds-checked – a gather has no bounds mode, so
// an index out of range would read garbage; one still on the GPU (an embedding lookup into
// a minibatch) is trusted data and flows through.
const TensorGather = (a: Value, b: Value): Value => {
  if (!isTensorish(a)) { return decline("numbers", a) }
  const x = rowsOf(a), size = x.shape[0]!
  // `!(0 <= w < size)` also rejects a NaN index, which slips past `<` and `>=`
  const bad = cheapValues(b)?.find((i) => { const w = i < 0 ? i + size : i; return !(w >= 0 && w < size) })
  if (bad !== undefined) { return new Error(`index ${bad} is out of bounds for an axis of length ${size}`) }
  const raw = engine.trunc(operand(b))
  return engine.take(x, engine.where(engine.less(raw, 0), engine.add(raw, size), raw), 0)
}
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
const ListGather = (a: Value, b: Value) => Array.isArray(a) ? pick(a, b, "list", (xs) => xs) : decline("a list", a)
const StringGather = (a: Value, b: Value) => isText(a) ? pick(chars(a), b, "string", (cs) => cs.join("")) : decline("a string", a)
// one item, at one index
const oneIndex = (i: Value): Value => {
  const t = Array.isArray(i) ? null : operand(i)
  if (!(isTensor(t) && t.size === 1)) { throw new Error(`expected one index, got ${describeArg(i)}`) }
  return engine.reshape(t, [])
}
const TensorGet = (a: Value, i: Value) => isTensorish(a) ? TensorGather(a, oneIndex(i)) : decline("numbers", a)
const ListGet = (a: Value, i: Value) => Array.isArray(a) ? ListGather(a, oneIndex(i)) : decline("a list", a)
const StringGet = (a: Value, i: Value) => isText(a) ? StringGather(a, oneIndex(i)) : decline("a string", a)

// A tensor's first rows, or the rest, are a view of it: nothing is copied
const TensorTake = (a: Value, n: Value) => isTensorish(a) ? rowsFrom(a, n, true) : decline("numbers", a)
const TensorDrop = (a: Value, n: Value) => isTensorish(a) ? rowsFrom(a, n, false) : decline("numbers", a)
const rowsFrom = (a: Value, n: Value, take: boolean) => { const x = rowsOf(a); return x.slice(span(x.shape[0]!, n, take)) }
const ListTake = (a: Value, n: Value) => Array.isArray(a) ? cut(a, n, true) : decline("a list", a)
const ListDrop = (a: Value, n: Value) => Array.isArray(a) ? cut(a, n, false) : decline("a list", a)
const StringTake = (a: Value, n: Value) => isText(a) ? cut(chars(a), n, true).join("") : decline("a string", a)
const StringDrop = (a: Value, n: Value) => isText(a) ? cut(chars(a), n, false).join("") : decline("a string", a)

// TensorReverse is the prelude's, along any axes
const ListReverse = (a: Value) => Array.isArray(a) ? [...a].reverse() : decline("a list", a)
const StringReverse = (a: Value) => isText(a) ? chars(a).reverse().join("") : decline("a string", a)

// a concat b joins a's items and b's, concat(a, b, …) any number of them; concat(xs),
// one list, joins the containers in it – tensors along the axis given after it. The first
// says which kind they are; in a step, a live value among them is read now.
const TensorConcat = (...args: Value[]) => {
  const listed = Array.isArray(args[0]) && !Array.isArray(args[1])
  const xs = (listed ? args[0] : args) as Value[]
  if (!isTensorish(sampled(xs[0] ?? null))) { return decline("numbers", xs[0] ?? null) }
  return engine.concatenate(xs.map(tensorArg), listed && args[1] != null ? asNumber(args[1]) : 0)
}
const joined = (args: Value[]): Value[] => args.length === 1 && Array.isArray(args[0]) ? args[0] : args
const ListConcat = (...args: Value[]) => {
  const xs = joined(args), bad = xs.findIndex((a) => !Array.isArray(a))
  if (bad === 0) { return decline("a list", xs[0]!) }
  return bad < 0 ? ([] as Value[]).concat(...xs) : new Error(`'ListConcat': expected lists, got ${describeArg(xs[bad]!)}`)
}
// text joined end to end, a value after the first that isn't text as the source that
// reproduces it; ◌, an Error or an unknown name is not text
const StringConcat = (...args: Value[]) => {
  const xs = joined(args).map(sampled)
  if (!isText(xs[0] ?? null)) { return decline("a string", xs[0] ?? null) }
  const bad = xs.findIndex((a) => a == null || a instanceof Error || typeof a === "symbol")
  if (bad >= 0) { throw new Error(`StringConcat: expected text, got ${describeArg(xs[bad]!)}`) }
  return xs.map((a) => isText(a) ? String(a) : StringLiteral(a)).join("")
}

// The items folded left to right with f(acc, item), from init – or from the first item
// when there is none. Folding nothing gives f's identity: + folds nothing to 0.
const emptyFold = (fn: Function): Value => getMeta(fn).identity ?? new Error("an empty fold needs f's identity – give one with identity(f, v)")
const ListReduce = function (this: CurrentScope, a: Value, fn: Value, init?: Value) {
  if (!Array.isArray(a)) { return decline("a list", a) }
  if (!isFn(fn)) { return new Error("'ListReduce': `fn` must be a function") }
  if (init == null && a.length === 0) { return emptyFold(fn) }
  let acc: Value = init == null ? a[0]! : init
  for (const v of init == null ? a.slice(1) : a) { acc = fn.call(this, acc, v) }
  return acc
}
const StringReduce = function (this: CurrentScope, a: Value, fn: Value, init?: Value) { return isText(a) ? ListReduce.call(this, chars(a), fn, init) : decline("a string", a) }
// axis 0 as a reusable value – the default a fold collapses
const LEADING_AXIS: Value = engine.tensor(0)
// A tensor's rows, or its slices along the axis given: `1 ..< 10 reduce +` = 45, `x reduce
// { a, b | a - b }` = ((x_0-x_1)-x_2)-… A known op takes its native reduction, pinned to
// the leading axis as a fold is – the native reductions default to every axis. Folding
// no slices gives f's identity in the shape of one.
const TensorReduce = function (this: CurrentScope, a: Value, fn: Value, axis?: Value) {
  if (!isTensorish(a)) { return decline("numbers", a) }
  if (!isFn(fn)) { return new Error("`reduce(x, fn, axis?)`: the second argument must be a function") }
  const { reducer } = getMeta(fn)
  if (reducer) { return reducer(a, axis == null ? LEADING_AXIS : axis) }
  const parts = TensorUnstack(a, axis)
  if (parts.length) { return ListReduce.call(this, parts, fn) }
  const none = emptyFold(fn), shape = shapeOf(a), k = axis == null ? 0 : asWhole(axis)
  return none instanceof Error ? none : engine.broadcastTo(tensorArg(none), shape.filter((_, i) => i !== (k + shape.length) % shape.length))
}
// fold(x, f, start): f walks the items from start. A tensor's reduce takes an axis
// third, so its fold is a member of its own.
const TensorFold = function (this: CurrentScope, a: Value, fn: Value, start?: Value) { return isTensorish(a) ? ListReduce.call(this, [...rowsOf(a)], fn, start) : decline("numbers", a) }

// ListReduce keeping every step: (init, f(init, a₀), …) – from a₀ when there is no init.
// TensorScan is the prelude's.
const ListScan = function (this: CurrentScope, a: Value, fn: Value, init?: Value) {
  if (!Array.isArray(a)) { return decline("a list", a) }
  if (!isFn(fn)) { return new Error("'ListScan': `fn` must be a function") }
  const out = init == null ? a.slice(0, 1) : [init]
  for (const v of init == null ? a.slice(1) : a) { out.push(fn.call(this, out.at(-1)!, v)) }
  return out
}
const StringScan = function (this: CurrentScope, a: Value, fn: Value, init?: Value) { return isText(a) ? ListScan.call(this, chars(a), fn, init) : decline("a string", a) }

// A member the prelude writes in Fluent, for numbers: f, declining any other value
const TensorOnly = (f: Value) => isFn(f)
  ? made(function (this: CurrentScope, a: Value, ...rest: Value[]) { return isTensorish(a) ? f.call(this, a, ...rest) : decline("numbers", a) })
  : new Error("`TensorOnly(f)`: f must be a function")

for (const f of [ListLength, ListGather, ListGet, ListTake, ListDrop, ListReverse, ListConcat, ListReduce, ListScan]) { shallow.add(f) }

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
// the value now, as data: a cell is its value at this moment
const storedNow = (v: Value): Value =>
  v instanceof Signal ? storedNow(v.value) : Array.isArray(v) ? v.map(storedNow) : v
const toStored = (v: Value): Promise<Stored> =>
  v == null ? Promise.resolve(null)
    : isText(v) ? Promise.resolve(String(v))
      : isTensor(v) ? v.read().then((data) => ({ shape: [...v.shape], data: Float32Array.from(data) }))
        : Array.isArray(v) ? Promise.all(v.map(toStored))
          : Promise.reject(new Error(`Storage: ${describeArg(v)} can't be stored – only tensors, strings, lists of them and ◌`))
const fromStored = (s: Stored): Value =>
  s === null ? null : typeof s === "string" ? s : Array.isArray(s) ? s.map(fromStored) : engine.tensor(s.data, { shape: s.shape })

// A Storage cell saves every write, also one of the value it already holds. A saved value
// that arrives after the run wrote its own is dropped.
class StorageCell extends Signal {
  written = false
  gone = false
  constructor(readonly key: string) {
    super(null)
    inStorageOrder(() => storageRequest("readonly", (values) => values.get(key), () => storedInMemory.get(key))).then(
      (s) => { if (!this.gone && !this.written && s !== undefined) { this.show(fromStored(s as Stored)) } },
      (e) => { if (!this.gone && !this.written) { this.show(asError(e)) } })
    registerDisposable(() => { this.gone = true })
  }
  show(v: Value): void { super.write(v) }
  protected override write(v: Value): void {
    if (v instanceof Error) { this.show(v); return }
    const now = storedNow(v)
    this.show(now)
    this.written = true
    const stored = toStored(now)
    stored.catch(() => undefined)
    inStorageOrder(() => stored.then((s) => storageRequest("readwrite",
      (values) => s === null ? values.delete(this.key) : values.put(s, this.key),
      () => s === null ? storedInMemory.delete(this.key) : storedInMemory.set(this.key, s))))
      .catch((e) => { if (!this.gone) { this.show(asError(e)) } })
  }
}
const Storage = (key: Value): Value => {
  const k = sampled(key)
  return isText(k) ? new StorageCell(String(k)) : new Error(`Storage(key): the key is a string, got ${describeArg(k)}`)
}
setMeta(Storage, { noAutoLift: true })

// MARK: Environment
// Null-prototype: an environment is a bare name→value mapping. A plain `{}` would chain
// to Object.prototype, so `constructor`, `toString` or `valueOf` would resolve to JS
// members instead of staying unbound (and `constructor(5)` would run Object(5)).
const DefaultEnvironment: Record<string, Value> = Object.assign(Object.create(null), {
  Null: null, null: null,
  SymbolAssign, CodeParse, CodeEvaluate,
  FunctionIterate, FunctionRepeat, FunctionMemo, FunctionThen, FunctionIs, TensorIs, ListIs, StringIs, ErrorIs, Identical,
  FunctionCascade, FunctionCond, FunctionArity, FunctionApply, FunctionNoAutoLift,
  FunctionGuard, FunctionTry, FunctionWhen, ErrorCreate, doc, FunctionIdentity,
  Reactive, SignalCreate, SignalComputed, SignalRead, SignalUpdate,
  ...UNARY, ...BINARY, ...REDUCTIONS,
  TensorStack, TensorUnstack, TensorRank, TensorOneHot, TensorSort, TensorArgSort, TensorGradient,
  TensorPermute, TensorBroadcast, TensorPadAxis, TensorWindow, TensorScatterAdd, TensorRange,
  TensorConvolution, TensorMeshgrid, TensorTopK, TensorReshape, TensorShape,
  TensorWhere, TensorSlice, TensorVariable, TensorData, TensorAssign, TensorRandomUniform, TensorMemoryOf,
  List, ListMap, StringLiteral, StringToCodes, CodesToString,
  TensorLength, ListLength, StringLength, TensorGet, ListGet, StringGet, TensorGather, ListGather, StringGather,
  TensorTake, ListTake, StringTake, TensorDrop, ListDrop, StringDrop, ListReverse, StringReverse, TensorConcat, ListConcat,
  StringConcat, TensorReduce, ListReduce, StringReduce, TensorFold, ListScan, StringScan, TensorOnly,
  Storage, Density,
})

// NoMatch, the decline a hand-written candidate returns: a fresh one per read, so each
// decline carries the origin of its own call
Object.defineProperty(DefaultEnvironment, "NoMatch", { get: () => NoMatch.of("no match"), enumerable: true, configurable: true })

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
  prelude = null
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
// What the language defines in itself lives in the Prelude, in Fluent. It is evaluated on
// first use – a page that only parses never needs it – and once for the environment it
// reads: every program shares it, so what it works out crosses time, and is worked out
// once. A change to the environment evaluates it again.
let preludeTree: SyntaxTreeNode | null = null, prelude: CurrentScope | null = null
// A program binds in a scope of its own, under the prelude's: its names shadow the
// prelude's for the program, while the prelude's functions keep reading their own –
// a program's `max: 10` doesn't reach into softmax
const createScope = (): CurrentScope => {
  if (!prelude) {
    evaluateSyntaxTreeNode(preludeTree ??= CodeParse(PRELUDE), prelude = Object.create(DefaultEnvironment) as CurrentScope)
    engine.instant()
  }
  return Object.create(prelude) as CurrentScope
}

// MARK: Exports
export {
  engine, FluentVariable, FluentData, isTensor, TensorScalarLive,
  CodeParse, getParseErrors,
  evaluateSyntaxTreeNode, CodeEvaluate, getAsSyncList, getMeta, setMeta, getOrigin,
  evaluateGeneration, flushRetirements, registerDisposable, nextFrame, stepsWaiting, asError,
  NoMatch, StringLiteral,
  DefaultEnvironment, extendEnvironment, setCodeNodePrinter, PRELUDE, createScope,
  SignalCreate, SignalUpdate, Signal, signal, computed, onWrite, awaiting, occurrence, keepingFrom, density,
}
export type { Value, CurrentScope, SyntaxTreeNode, Origin, ParseError, FunctionMeta }
