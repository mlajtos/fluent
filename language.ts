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
  NOT_SIGN: ["\u00AC", "\u00AC"], // Logical Not
  MISCELLANEOUS_MATH_SYMBOLS_A: ["\u27C0", "\u27EF"],  // Miscellaneous Mathematical Symbols-A (\u27DC)
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

const numberRegexp = /-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/
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
  // the glued operator is any Atom, so referential transparency holds: an
  // identifier glued here behaves like the value it is bound to, glued here –
  // "1add 2 * 3" and "1{x,y|x+y} 2 * 3" both mean f(1, 2*3), given add:{x,y|x+y}.
  // (Number can't glue lexically, and a parenthesized value "1(f) 2" is claimed
  // by application, not this rule – see the Application note below.)
  LongOperation
    = TightOperation #(~space) Atom #(space) Expr

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
  // KNOWN LIMITATION (by design, do not "fix" without a plan): a "(...)" glued
  // to the right of any expression is ALWAYS an argument list, because the
  // parser has no types and must resolve "x(y)" the same way regardless of
  // whether x is callable. So a parenthesized function used infix must be
  // SPACED: "1 (add) 2" is add(1,2)=3, but the glued "1(add)2" reads as
  // "call 1 with add" and errors, and symmetrically "(1)add(2)" reads as
  // "1 applied to add(2)" and errors. The glued named-operator forms "1add 2"
  // and "1+ 2" are the intended way to glue infix; parens are for grouping and
  // for first-class function values, and only act as a value when not glued to
  // a callee on their left. See LongOperation above.
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
  Symbol          = identifier | operator
  // identifier owns the IDENTIFIER_RANGES symbols (math-alphanumeric, emoji) so
  // they behave as names everywhere the rule is used – including the glued
  // operator slot of LongOperation. They were previously a separate Symbol
  // alternative, which made "1🔥 2 * 3" fail to glue like "1add 2 * 3".
  identifier      = &(letter | idRanges) (alnum | "-" | idRanges)+
  idRanges        = ${getSymbolsRange(IDENTIFIER_RANGES)}
  number
    = "-"? digitGroup ("." digitGroup?)? exponent?
  exponent
    = ("e" | "E") ("+" | "-")? digitGroup
  digitGroup
    = digit+
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

// A backtick literal's inner Program is parsed on its own (see the Code action),
// so its nodes' origins are relative to the inner text. Shift them into the
// outer document – (startLine, startColumn) is where the inner text begins – so
// editor hover-highlighting maps each viz node back to the right source range.
// Only the first inner line needs the column offset; later lines start at col 1
// in both.
const shiftOrigins = (node: any, startLine: number, startColumn: number): void => {
  if (!node || typeof node !== "object") return
  if (node.origin?.start && node.origin?.end) {
    const map = (p: { line: number; column: number }) => ({
      line: startLine + p.line - 1,
      column: p.line === 1 ? startColumn + p.column - 1 : p.column,
    })
    node.origin = { ...node.origin, start: map(node.origin.start), end: map(node.origin.end) }
  }
  for (const key in node) {
    if (key === "origin") continue
    const child = node[key]
    if (Array.isArray(child)) { for (const c of child) shiftOrigins(c, startLine, startColumn) }
    else if (child && typeof child === "object") shiftOrigins(child, startLine, startColumn)
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
        // toAST (not operatorSymbolNode) so the glued operator keeps its real
        // value node – a Symbol stays a Symbol, a Lambda stays a Lambda, etc.
        // (referential transparency, same handling as SpacedOperation_infix)
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
    // `a`, `b` used to conflate into a single Code node. The sub-parse's origins
    // are relative to the inner text, so shift them back to the outer document.
    const inner = CodeParse(value.sourceString)
    const contentStart = getLocationOrigin(value).start
    shiftOrigins(inner, contentStart.line, contentStart.column)
    return {
      type: "Code",
      content: { value: inner },
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

// One parse per source: CodeParse and getParseErrors share match results. A
// small LRU rather than a single slot – the nested CodeParse a backtick
// literal runs mid-toAST would otherwise evict the outer program between
// evaluation and editor validation, doubling the parse work per keystroke.
const MATCH_CACHE_LIMIT = 8
const matchCache = new Map<string, ReturnType<typeof CodeGrammarCompiled.match>>()
const matchProgram = (program: string) => {
  const input = String(program)
  const cached = matchCache.get(input)
  if (cached) {
    matchCache.delete(input)
    matchCache.set(input, cached) // refresh recency
    return cached
  }
  const match = CodeGrammarCompiled.match(input)
  matchCache.set(input, match)
  if (matchCache.size > MATCH_CACHE_LIMIT) {
    matchCache.delete(matchCache.keys().next().value!)
  }
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

// functions are values too – a composed closure ((f ∘ g), a partial
// application) reads its printable source back from its origin
function setOrigin(value: any, origin: Origin): void {
  if (value !== null && (typeof value === 'object' || typeof value === 'function')) {
    originMap.set(value, origin)
  }
}

function getOrigin(value: any): Origin | undefined {
  if (value !== null && (typeof value === 'object' || typeof value === 'function')) {
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
    // a binding inside a [ … ] literal is local to it, not leaked to the enclosing scope
    const localEnv = Object.create(env) as CurrentScope
    const values = node.content.value.map((e) => evaluateSyntaxTreeNode(e, localEnv))
    if (values.length === 0) {
      const value = track(np.array([]))
      setOrigin(value, node.origin)
      return value
    }

    // Literal fast path: when every element's JS value is already known
    // (number literals, scalar bindings, nested literals – all pre-seeded),
    // build the array straight from data and seed the composite too. The
    // composite then stays readable inside a jit trace – slice bounds and
    // reshape shapes like [0, 1] or [1, T] must not demote the compiled
    // optimizer step – and display reads skip the device round-trip.
    // (Elements are still lazy symbols here – resolve them first.)
    try {
      const resolved = values.map((v) => reify(v, localEnv))
      if (resolved.every((v) => typeof v === "object" && v !== null && syncReadCache.has(v))) {
        const data = resolved.map((v) => syncReadCache.get(v as object))
        const value = track(np.array(data as number[]))
        syncReadCache.set(value, data)
        setOrigin(value, node.origin)
        return value
      }
    } catch { /* unbound symbol, ragged or non-numeric – stack instead */ }

    const value = safeApply(TensorStack, values, localEnv)
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
    // a binding inside ( … ) is local to it, not leaked to the enclosing scope;
    // elements still read outer names, since the child scope inherits.
    //
    // ASYMMETRY WART: this only isolates a ( … ) the parser sees as a List (two+
    // elements, or a trailing comma). A bare single-element paren – (a: 2) –
    // parses as NestedExpr and is unwrapped, so its binding still leaks; single
    // and multi parens scope differently. The intended unification is the unit
    // axiom (x) ≡ x – fold NestedExpr into a List that demotes on a single
    // non-comma element, so a singleton scopes-then-demotes like every other
    // paren (the inverse of what @ already does: bare value → [value], line
    // ~1695). Undecided: whether names inside ( … ) are scoping temps (a pure
    // positional list) or first-class (records / named args), and if the latter
    // whether a named singleton (a: 2) demotes to 2 or stays the record {a: 2}.
    const localEnv = Object.create(env) as CurrentScope
    const value = node.content.value.map((e) => reify(evaluateSyntaxTreeNode(e, localEnv), localEnv))
    setOrigin(value, node.origin)
    return value
  }

  if (node.type === "Lambda") {
    // const fn = (...args: any[]) => {
    const fn = function (...args: any[]) {
      const localEnv = node.content.args
        .filter(n => n.type === "Symbol")
        .reduce<CurrentScope>((acc, arg, i) => {
          // a missing argument is ◌ (no value), not an unbound name
          // @ts-ignore
          acc[arg.content.value] = args[i] ?? null
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

    // a binding among the args (f(a: 2, b)) is local to the call, not leaked out
    const argsEnv = Object.create(env) as CurrentScope
    // Process args directly (not via List evaluation) - safeApply handles quotedArgs
    const argNodes = node.content.args.content.value
    const args = argNodes.map((argNode: SyntaxTreeNode) =>
      evaluateSyntaxTreeNode(argNode, argsEnv)
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
    // the printer is IDE code (the AST viz) – a rendering bug must degrade to
    // an Error value, not abort the whole evaluation
    let value: any
    try {
      value = codeNodePrinter(node.content.value)
    } catch (e) {
      value = e instanceof Error ? e : new Error(String(e))
    }
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

// Fluent identifiers are interned symbols (Symbol.for(name)); these look one up
// in the scope chain. Free functions, not Symbol.prototype methods, so we don't
// mutate the global Symbol for every symbol in the process (and every dependency).
const resolveSymbol = (sym: symbol, env: CurrentScope): Value => {
  const k = Symbol.keyFor(sym) ?? ""
  try {
    // `in` (which walks the scope chain), not `??`: a name bound to ◌ must
    // resolve to null – null is a value, absence is not. With `??` the binding
    // masqueraded as an unbound symbol, so `x: ◌, x` gave back the symbol x.
    return k in env ? (env[k] ?? null) : sym
  } catch {
    return null
  }
}

const assignSymbol = (sym: symbol, env: CurrentScope, value: Value): void => {
  const k = Symbol.keyFor(sym) ?? ""
  env[k] = value
}

const reify = (v: Value, env: CurrentScope): Value => {
  if (typeof v === "symbol") {
    const resolved = resolveSymbol(v, env)

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
  if (typeof v === "symbol") {
    // an unbound name reached a numeric op (e.g. `1 * x`): fail with the name,
    // not jax-js's opaque "Cannot convert a Symbol value to a string". The bare
    // symbol still flows through as a value elsewhere – only coercion errors.
    throw new Error(`unknown name: ${v.description ?? Symbol.keyFor(v) ?? String(v)}`)
  }
  return v
}

const shapeOf = (v: Value): number[] =>
  (v instanceof FluentVariable ? v.current : (v as np.Array)).shape

type Arena = Set<np.Array>
const arenaStack: Arena[] = []

// Leak watch (test/dev-facing, off by default): while a watch is open, track()
// also records every array Fluent adopts, so liveTensorCount() can report how
// many still hold a device buffer (refCount > 0). This is the only reliable
// leak signal we have – jax-js's own buffer table is a hard-private field.
let tensorWatch: Arena | null = null
let tensorWatchPeak = 0
const beginTensorWatch = () => { tensorWatch = new Set(); tensorWatchPeak = 0 }
const endTensorWatch = () => { tensorWatch = null }
const liveTensorCount = (): number => {
  if (!tensorWatch) { return 0 }
  let n = 0
  for (const a of tensorWatch) { if (a.refCount > 0) { n++ } else { tensorWatch.delete(a) } }
  return n
}
// Highest concurrent live count seen since beginTensorWatch. Sampled as track()
// runs, so it catches a transient O(n) spike (e.g. f⍣n retaining every step)
// that liveTensorCount – read after the arena sweep – would miss.
const peakTensorCount = (): number => tensorWatchPeak

// Track a freshly created array (or a list holding some) in the active arena –
// the generation arena when no expression arena is open. Tracers pass through.
const track = <T>(value: T): T => {
  const arena = arenaStack[arenaStack.length - 1] ?? currentGeneration?.arena
  if (arena) { addConcreteArrays(value, arena) }
  if (tensorWatch) {
    addConcreteArrays(value, tensorWatch)
    let live = 0
    for (const a of tensorWatch) { if (a.refCount > 0) { live++ } }
    if (live > tensorWatchPeak) { tensorWatchPeak = live }
  }
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
    const registry = this instanceof FluentData ? dataVariables : trainableVariables
    registry.add(this)
    registerDisposable(() => {
      registry.delete(this)
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

// Data slot (~~): a mutable tensor the compiled optimizer step receives as a
// jit ARGUMENT on every call – read fresh, never differentiated, no optimizer
// state. Gradients flow into ~, never into ~~. Reassigning to a new shape
// costs one clean retrace; same shape replays the compiled step untouched.
class FluentData extends FluentVariable {}
const dataVariables = new Set<FluentData>()

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

// Generations awaiting retirement. A retired generation's effects and tensors
// are freed only once the NEW UI has committed and the old islands unmounted –
// a still-mounted old island could otherwise recompute against a freed tensor.
// The renderer signals that exact moment by calling flushRetirements() from a
// post-commit effect (replacing a 50 ms timer that only guessed at it); tests
// and headless callers call it directly.
const pendingRetirements = new Set<Generation>()

const retireGeneration = (g: Generation) => {
  for (const dispose of g.disposables) {
    try { dispose() } catch { /* already gone */ }
  }
  // every array the generation's evaluations created and kept lives here –
  // signals and variables released their own references via disposables
  for (const array of g.arena) {
    if (array.refCount > 0) { array.dispose() }
  }
}

const flushRetirements = () => {
  for (const g of pendingRetirements) { retireGeneration(g) }
  pendingRetirements.clear()
}

// In the browser, flush on the next animation frame – after this frame's DOM
// work has replaced the old islands, before the next paint. That's aligned to
// the render cycle instead of the old blind 50 ms guess. Headless callers
// (tests, screenshots) have no rAF and flush explicitly.
let retirementFlushScheduled = false
const scheduleRetirementFlush = () => {
  if (retirementFlushScheduled) { return }
  retirementFlushScheduled = true
  const done = () => { retirementFlushScheduled = false; flushRetirements() }
  const raf = (globalThis as { requestAnimationFrame?: (cb: () => void) => void }).requestAnimationFrame
  // Browser: flush on the next frame, aligned to the render cycle. Headless (no
  // rAF): a macrotask, so generations still don't pile up between evaluations –
  // there are no islands to outlive, so nothing to align to.
  if (raf) { raf(done) } else { setTimeout(done, 0) }
}

function evaluateGeneration<T>(evaluate: () => T): T {
  const previous = currentGeneration
  currentGeneration = { disposables: new Set(), arena: new Set() }
  try {
    return evaluate()
  } finally {
    // queue the previous generation even if building this one threw, so its
    // effects, sources, and GPU tensors never leak on an error
    if (previous) {
      pendingRetirements.add(previous)
      scheduleRetirementFlush()
    }
  }
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

const FunctionCascade = (candidates: Function[]) => tacitToString((a: Value, b: Value) => {
  const noResultSymbol = Symbol('noResult')

  let result: (Value | typeof noResultSymbol) = noResultSymbol

  let candidateIndex = 0

  while (result === noResultSymbol && candidateIndex < candidates.length) {

    let candidateResult: (Value | typeof noResultSymbol) = noResultSymbol

    try {
      const fn = candidates[candidateIndex]
      candidateResult = fn?.(a, b)
    } catch (e) {
      // A candidate says "not me" by RETURNING an Error value (handled below).
      // A *throw* is usually a real op/arg mismatch – jax-js throws when an op
      // doesn't fit the args, which is a legitimate "try the next candidate"
      // (max/min dispatch their arity this way). But a TraceBailout is control
      // flow that must reach the tracer: swallowing it as a non-match would
      // compile the wrong branch into a jit trace, so let it propagate.
      if (e instanceof TraceBailout) { throw e }
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
})

// reduce's fast path: a raw binary op → its native tensor reduction. Filled in
// once the reductions are defined below; FunctionArity tags its dispatchers too,
// so `reduce +` and `reduce ⌈` fold via np.sum/np.max instead of slice-by-slice.
const reducerFor = new Map<Function, (x: Value, axis?: Value) => Value>()
const nativeReducer = new WeakMap<Function, (x: Value, axis?: Value) => Value>()

// Overloaded by arity: two operands run the binary op, one runs the unary op –
// used for +, -, ×, · (add/abs, subtract/negate, multiply/sign). Unlike
// FunctionCascade, a binary op that *fails* (e.g. an unbound-symbol operand) is
// NOT silently retried as the unary op, so `1 + a` with unknown `a` is an Error
// rather than a quietly-wrong abs(1).
const FunctionArity = (candidates: Function[]) => {
  const [binary, unary] = candidates
  // tacitToString: a userland-built operator prints as the source that made
  // it (compose2(+, √)), not as this dispatcher's JS internals
  const dispatch = tacitToString((a: Value, b: Value) => (b === undefined ? unary?.(a) : binary?.(a, b)))
  // The dispatcher has no story of its own – its meaning IS its candidates.
  // Synthesize the doc card from theirs, so `+` shows add and abs in one
  // hover. A doc() authored on the dispatcher afterwards still wins.
  const b: FunctionMeta = typeof binary === "function" ? getMeta(binary) : {}
  const u: FunctionMeta = typeof unary === "function" ? getMeta(unary) : {}
  if (b.signature || u.signature) {
    setMeta(dispatch as Function, {
      signature: [b.signature, u.signature].filter(Boolean).join("  ·  "),
      doc: [b.doc, u.doc && `With one argument: ${u.doc}`].filter(Boolean).join(" "),
      example: [b.example, u.example].filter(Boolean).join("\n") || undefined,
    })
  }
  // if the binary op has a native reduction, `reduce`/fold with this dispatcher
  // takes the fast path (e.g. `reduce +` → np.sum, `reduce ⌈` → np.max)
  const nativeReduce = typeof binary === "function" ? reducerFor.get(binary) : undefined
  if (nativeReduce) { nativeReducer.set(dispatch as Function, nativeReduce) }
  return dispatch
}

// Synchronous, non-consuming read of a tensor's data as nested JS values.
// jax-js `.js()` consumes the array, so read through a borrowed reference.
// Every read crosses the device boundary (on WebGPU: a canvas readback), and
// arrays are immutable – cache the JS value per array. Number literals are
// pre-seeded at creation, so `x^2` or `sum(x, 2)` never read the device.
const syncReadCache = new WeakMap<object, unknown>()
function getAsSyncList(value: unknown) {
  if (value instanceof FluentVariable) { value = value.current }
  // the cache check must come before the tracer check: inside a jit trace
  // even literal-built arrays are staged into tracers, but a SEEDED tracer
  // (number literals, literal tensors, shapes) has a statically known value –
  // reading it must not abort the trace
  if (typeof value === "object" && value !== null && syncReadCache.has(value)) {
    return syncReadCache.get(value)
  }
  if (value instanceof np.Array) {
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

  const count = getAsSyncList(n)
  if (typeof count !== "number" || !Number.isFinite(count)) {
    // a non-scalar (or NaN) count would silently skip the loop and return the
    // identity function – surface it instead
    return new Error("`FunctionPower(fn, n)`: `n` must be a finite scalar, got " +
      (Array.isArray(count) ? `a tensor of shape [${(n as np.Array).shape.join(" ")}]` : String(count)))
  }
  // a live slider drives counts through fractional values – round, like linspace
  const times = Math.max(0, Math.round(count))

  return (...args: unknown[]) => {
    let value: unknown = args[0] ?? null
    for (let i = 0; i < times; i++) {
      const prev = value
      // each step in its own arena: per-iteration intermediates die immediately,
      // only the chained value is handed forward (like FunctionIterate/⟳)
      value = arena(() => fn(prev))
      // release the just-consumed input so the chain holds O(1) live tensors,
      // not O(iterations). Skip the caller's original arg (i === 0), and keep any
      // tensor the new value still shares – an identity step like ⊢ returns it.
      if (i > 0) {
        const keep: Arena = new Set()
        collectLiveArrays(value, keep)
        const consumed: Arena = new Set()
        collectLiveArrays(prev, consumed)
        for (const a of consumed) { if (!keep.has(a) && a.refCount > 0) { a.dispose() } }
      }
    }
    return value
  }
}

// MARK: Combinators
// The tacit aviary, after Conor Hoekstra's taxonomy (combinatorylogic.com).
// Each combinator is a plain higher-order value, so a composed function drops
// into operator position like any function: a (f ∘ g) b. Valence pairs
// (B+B₁, C+W, Σ+Δ, S+D, Φ+Φ₁) share one binding – the closure routes on
// argument count, the same dispatch FunctionArity gives + and -.

const isFn = (v: Value): v is Function => typeof v === "function"

// Hooks and fork tines K-lift a non-function operand to a constant function –
// BQN's bind: (1 ⊸ +) increments, (÷ ⟜ 2) halves.
const constantly = (v: Value): Function => isFn(v) ? v : () => v

// A composed closure prints as the source that built it ((= ⍥ abs), not JS
// internals) – the Operation that created it stamps its origin after return.
const tacitToString = (h: Function): Function => {
  h.toString = () => getOrigin(h)?.source ?? "(tacit function)"
  return h
}

// Q (Queer bird) – then: apply f, then g to the result. Reading order is
// evaluation order – (f ∘ g)(…x) = g(f(…x)) – so this is APL's atop (B/B₁)
// with the tines swapped to read left-to-right, like the . pipe.
const FunctionCompose = (f: Value, g: Value) => {
  if (!isFn(f) || !isFn(g)) { return new Error("`(f ∘ g)`: both operands must be functions") }
  return tacitToString(function (this: CurrentScope, ...args: Value[]) {
    return safeApply(g, [safeApply(f, args, this)], this)
  })
}

// C/W (Cardinal/Warbler) – commute: flip a binary's arguments; duplicate a unary's
const FunctionCommute = (f: Value) => {
  if (!isFn(f)) { return new Error("`(⍨ f)`: the operand must be a function") }
  return tacitToString(function (this: CurrentScope, a: Value, b: Value) {
    return b === undefined ? safeApply(f, [a, a], this) : safeApply(f, [b, a], this)
  })
}

// Ψ (Psi) – over: preprocess every argument with g, then combine with f.
// The preprocessor is read first because it runs first: (g ⍥ f)(x, y) =
// f(g(x), g(y)) – APL spells this f⍥g, but Fluent reads left-to-right.
const FunctionOver = (g: Value, f: Value) => {
  if (!isFn(f) || !isFn(g)) { return new Error("`(g ⍥ f)`: both operands must be functions") }
  return tacitToString(function (this: CurrentScope, ...args: Value[]) {
    return safeApply(f, args.map((a) => safeApply(g, [a], this)), this)
  })
}

// Φ/Φ₁ (Phoenix/Pheasant) – fork: outer tines f and h see the arguments,
// the middle tine g combines their results – the train reading order
const FunctionFork = (f: Value, g: Value, h: Value) => {
  if (!isFn(g)) { return new Error("`Φ(f, g, h)`: the middle tine `g` must be a function") }
  if (f === undefined || h === undefined) { return new Error("`Φ(f, g, h)`: takes three tines") }
  const [fc, hc] = [constantly(f), constantly(h)]
  return tacitToString(function (this: CurrentScope, ...args: Value[]) {
    return safeApply(g, [safeApply(fc, args, this), safeApply(hc, args, this)], this)
  })
}

// Σ/Δ (Violet Starling/Zebra Dove) – before: preprocess the left (or only)
// argument with f, then combine with g
const FunctionBefore = (f: Value, g: Value) => {
  if (!isFn(g)) { return new Error("`(f ⊸ g)`: `g` must be a function") }
  const fc = constantly(f)
  return tacitToString(function (this: CurrentScope, a: Value, b: Value) {
    return safeApply(g, [safeApply(fc, [a], this), b === undefined ? a : b], this)
  })
}

// S/D (Starling/Dove) – after: preprocess the right (or only) argument with g,
// then combine with f; the monadic case is the J hook
const FunctionAfter = (f: Value, g: Value) => {
  if (!isFn(f)) { return new Error("`(f ⟜ g)`: `f` must be a function") }
  const gc = constantly(g)
  return tacitToString(function (this: CurrentScope, a: Value, b: Value) {
    return safeApply(f, [a, safeApply(gc, [b === undefined ? a : b], this)], this)
  })
}

// I/K/KI – the tacks: ⊢ yields its rightmost argument, ⊣ its leftmost
const FunctionRight = (...args: Value[]) =>
  args.length === 0 ? new Error("`⊢`: takes at least one argument") : args[args.length - 1]
const FunctionLeft = (...args: Value[]) =>
  args.length === 0 ? new Error("`⊣`: takes at least one argument") : args[0]

// The K-lift question, exposed: userland combinators need to ask "is this
// operand a function?" to bind constants the way the native hooks do.
// Float32 0/1, like a comparison, so it composes with guard and where.
const FunctionIs = (v: Value) => track(np.array(isFn(v) ? 1 : 0))

// Async repeat (⟳): runs fn between frames so the UI stays live. The loop
// belongs to the evaluation that started it – a re-evaluation cancels it.
// (For synchronous function iteration f(f(f(x))) use ⍣ / FunctionPower.)
const FunctionIterate = (fn: (index?: np.Array) => void, iterations?: Value) => {
  if (!(typeof fn === "function" && (iterations === undefined || isTensor(iterations)))) {
    throw new Error("`FunctionIterate(fn, iterations)`: `fn` must be a function and `iterations` must be a scalar Tensor");
  }

  const maxIterations = iterations === undefined ? 1 : getAsSyncList(iterations)
  if (typeof maxIterations !== "number" || Number.isNaN(maxIterations)) {
    // a non-scalar count would make `i >= maxIterations` always false – an
    // accidental infinite loop instead of an error
    throw new Error("`FunctionIterate(fn, iterations)`: `iterations` must be a scalar Tensor");
  }
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

    // The batch count is a plan, not a promise: bail out mid-batch the moment
    // the frame budget is spent. Cheap ticks (a paused, checkbox-gated loop)
    // grow the batch toward the cap – without this, the first frame after
    // resuming would run the whole grown batch of now-expensive steps and
    // freeze the tab for seconds.
    const frameStart = (globalThis as any).performance?.now?.() ?? 0
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
      if ((((globalThis as any).performance?.now?.() ?? 0) - frameStart) > 25) { break }
    }

    // drain the lazy queue: jax-js never dispatches on its own, so a training
    // loop whose variables nobody displays would grow an unbounded pending
    // graph – realize what the batch retained. Defensive: a corrupted
    // variable must never kill the loop itself.
    for (const variable of [...trainableVariables, ...dataVariables]) {
      if (!(variable.current instanceof np.Array)) { continue }
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

// effects belong to the evaluation that created them – a re-evaluation must
// stop them, or stale effects keep firing into disposed variables
const SignalEffect = ((fn: () => void) => {
  const dispose = effect(fn)
  registerDisposable(dispose)
  return dispose
}) as typeof effect
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
    assignSymbol(a, this, b)
    return resolveSymbol(a, this)
  } else {
    return new Error(`'SymbolAssign': Left side must be a symbol, got: ${String(a)}`)
  }
}

// First argument (target symbol) should not be reified
setMeta(SymbolAssign, { quotedArgs: [0], noAutoLift: true })

const FunctionEvaluate = function (this: CurrentScope, fn: Value, args: Value) {
  // a bare argument applies directly – `f @ 5` is f(5); a list spreads
  const list = args instanceof Array ? args : [args]
  return safeApply(fn, list, this)
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
  // Capture condition value and thunk. Strict read: a typo'd symbol used to
  // coerce to undefined, which passed every falsy check – the guard silently
  // held. In a cascade the Error just moves on to the next candidate.
  const condValue = requireData(cond)
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

// when(cond, { … }) – a conditional effect: run the thunk while cond is
// truthy, yield ◌ otherwise. Unlike a guard inside a cascade, a thunk that
// ERRORS stays loud – only the condition gates. (Swallowing thunk errors
// would silently turn a broken training step into a no-op.)
const FunctionWhen = function (this: CurrentScope, cond: Value, thunk: Value) {
  const condValue = requireData(cond)
  const falsy = (v: unknown) => v === 0 || v === false
  const isFalsy = falsy(condValue) ||
    (Array.isArray(condValue) && (condValue.flat(Infinity) as unknown[]).every(falsy)) ||
    Number.isNaN(condValue)
  if (isFalsy) { return null }
  if (typeof thunk !== "function") {
    return new Error("`when(cond, { … })`: second argument must be a thunk")
  }
  return (thunk as Function).apply(this)
}
setMeta(FunctionWhen, { noAutoLift: true })

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

  const index = asNumber(b)
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

// Read scalar/vector metadata (axis, sizes, indices, counts) out of a tensor
// argument. STRICT: a typo'd symbol or an Error flowing in must surface as an
// Error, not coerce to undefined/NaN and silently change the op's meaning –
// `sum(x, axs)` used to sum everything, `ListGet(xs, oops)` returned the first
// element. Same design decision FunctionArity makes for + - × ·. (getAsSyncList
// itself stays lenient: UI components read through it best-effort.)
const describeArg = (v: Value): string =>
  typeof v === "symbol" ? `unbound symbol '${Symbol.keyFor(v as symbol) ?? String(v)}'`
    : v === null ? "◌"
    : v instanceof Error ? "an Error"
    : v instanceof Signal ? "a signal"
    : Array.isArray(v) ? "a list"
    : typeof v === "string" || v instanceof String ? "a string"
    : typeof v === "function" ? "a function"
    : `a ${typeof v}`

const requireData = (v: Value): number | unknown[] => {
  const data = getAsSyncList(v)
  if (typeof data !== "number" && !Array.isArray(data)) {
    throw new Error(`expected a tensor, got ${describeArg(v)}`)
  }
  return data as number | unknown[]
}

const asNumber = (v: Value): number => {
  const data = requireData(v)
  const n = Number(data) // 1-element vectors keep coercing, as they always did
  if (Number.isNaN(n)) {
    throw new Error(typeof data === "number"
      ? "expected a number, got NaN"
      : `expected a scalar, got a tensor of shape [${shapeOf(v).join(" ")}]`)
  }
  return n
}
const asNumberList = (v: Value): number[] => ([] as number[]).concat(requireData(v) as any)

const Tensor = (values: unknown, shape?: Value) => {
  const source: any = isTensor(values) || values instanceof FluentVariable ? borrow(values) : values
  return track(np.array(source, shape === undefined ? undefined : { shape: asNumberList(shape) }))
}
const TensorScalar = (value: number) => {
  const scalar = track(np.array(value))
  syncReadCache.set(scalar, value)
  return scalar
}

// Numeric-operand borrow: a string ("a" + "b") or a function is a user mistake
// here, so name the type instead of letting jax-js surface its opaque "Invalid
// type for full: a". borrow itself stays general – a signal may hold a string.
const numericArg = (v: Value): any => {
  if (typeof v === "string" || v instanceof String) {
    throw new Error(`a string (${JSON.stringify(String(v))}) is not a tensor – numeric ops like + and × need tensors; use StringConcat to join text`)
  }
  if (typeof v === "function") {
    throw new Error(`a function is not a tensor – numeric ops like + and × need tensors`)
  }
  return borrow(v)
}

// Wrapper factories: borrow the tensor arguments, track the result.
const unaryOp = (op: (x: any) => np.Array) => (a: Value) => track(op(numericArg(a)))
const binaryOp = (op: (x: any, y: any) => np.Array) => (a: Value, b: Value) => track(op(numericArg(a), numericArg(b)))

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
  const r = rank === undefined ? 0 : requireData(rank) as (number | number[])
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
  (a: Value, b: Value) => track(np.astype(op(numericArg(a), numericArg(b)), np.float32))

const TensorLess = comparisonOp(np.less)
const TensorGreater = comparisonOp(np.greater)
const TensorLessEqual = comparisonOp(np.lessEqual)
const TensorGreaterEqual = comparisonOp(np.greaterEqual)
const TensorEqual = comparisonOp(np.equal)
const TensorNotEqual = comparisonOp(np.notEqual)

// Logical ops share the comparison contract: any nonzero operand counts as
// true, the result is float32 0/1.
const TensorOr = comparisonOp(np.logicalOr)
const TensorAnd = comparisonOp(np.logicalAnd)
const TensorXor = comparisonOp(np.logicalXor)
const TensorNot = (a: Value) => track(np.astype(np.logicalNot(borrow(a)), np.float32))
const TensorNand = (a: Value, b: Value) => TensorNot(TensorAnd(a, b))
const TensorNor = (a: Value, b: Value) => TensorNot(TensorOr(a, b))

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

// Reductions take an optional axis (scalar or vector tensor) as second
// argument, and a truthy third argument keeps the reduced axis as size 1 –
// mean(x, -1, 1) is the trace-safe unsqueeze(mean(x, -1), …)
const withOptionalAxis = (op: (a: any, axis?: number | number[] | null, opts?: { keepdims?: boolean }) => np.Array) =>
  (a: Value, b?: Value, keep?: Value) =>
    track(op(promoteBool(a),
      b === undefined ? null : requireData(b) as (number | number[]),
      keep === undefined ? undefined : { keepdims: asNumber(keep) !== 0 }))

const TensorSum = withOptionalAxis(np.sum)      // TensorReduce(a, +)
const TensorProduct = withOptionalAxis(np.prod) // TensorReduce(a, ×)
const TensorMean = withOptionalAxis(np.mean)
const TensorMin = withOptionalAxis(np.min)
const TensorMax = withOptionalAxis(np.max)

// The reductions above ARE folds; register the fast native path so that
// `x reduce +` dispatches to np.sum rather than folding slice by slice.
reducerFor.set(TensorAdd, TensorSum)
reducerFor.set(TensorMultiply, TensorProduct)
reducerFor.set(TensorMaximum, TensorMax)
reducerFor.set(TensorMinimum, TensorMin)

// axis 0 as a reusable value – the default a fold collapses. Seeded in the
// sync cache so reading it back is never a device round-trip.
const LEADING_AXIS: Value = np.array(0)
syncReadCache.set(LEADING_AXIS as object, 0)

// Fold a tensor left-to-right with a binary fn, optionally along one axis:
// `1 :: 10 reduce +` = 45, `x reduce { a, b | a - b }` = ((x_0-x_1)-x_2)-…
// A known op takes its native reduction (fast, axis-aware); anything else folds
// over the slices along the axis (default 0).
const TensorReduce = (tensor: Value, fn: Value, axis?: Value) => {
  if (typeof fn !== "function") {
    return new Error("`reduce(tensor, fn, axis?)`: the second argument must be a function")
  }
  const fast = nativeReducer.get(fn as Function) ?? reducerFor.get(fn as Function)
  // a fold collapses the leading axis (axis 0), removing one rank – the same
  // thing TensorUnstack does below. The native reducers default to all-axes, so
  // pin axis 0 when none is given, or `reduce +` on a matrix would total it (a
  // custom lambda folds the leading axis, and the two must agree).
  if (fast) { return fast(tensor, axis === undefined ? LEADING_AXIS : axis) }
  const parts = TensorUnstack(tensor, axis) as unknown[]
  if (parts.length === 0) {
    // a known op has an identity (Σ[] = 0); a custom fn's is unknown, so an
    // empty fold has no answer to hand back
    return new Error("`reduce`: cannot fold an empty tensor with a custom function")
  }
  return ListReduce(parts, (acc: Value, v: Value) => (fn as Function)(acc, v))
}

// argmax/argmin take a single axis (or none, over the flattened array) and
// return int32 indices of the extreme element. Over an empty reduction axis
// there is no index, and jax-js hands back a sentinel int (-2147483648) that
// silently corrupts a downstream gather – so error at the source instead.
const emptyReductionAxis = (a: Value, axis?: Value): boolean => {
  const shape = (a instanceof FluentVariable ? a.current : a as any)?.shape
  if (!Array.isArray(shape)) { return false } // not a tensor – let the op handle it
  if (axis === undefined) { return shape.some((d: number) => d === 0) }
  const n = asNumber(axis)
  return shape[n < 0 ? shape.length + n : n] === 0
}
const TensorArgMax = (a: Value, axis?: Value) =>
  emptyReductionAxis(a, axis) ? new Error("`argmax`: an empty tensor has no maximal index")
    : track(axis === undefined ? np.argmax(promoteBool(a)) : np.argmax(promoteBool(a), asNumber(axis)))
const TensorArgMin = (a: Value, axis?: Value) =>
  emptyReductionAxis(a, axis) ? new Error("`argmin`: an empty tensor has no minimal index")
    : track(axis === undefined ? np.argmin(promoteBool(a)) : np.argmin(promoteBool(a), asNumber(axis)))

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
const TensorArgSort = (x: Value) => track(np.argsort(borrow(x)))
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
    return new Error("`grad(f)`: `f` must be a function")
  }
  return (x: Value) => {
    // vjp with a ones cotangent: scalar outputs give the classic gradient,
    // elementwise outputs get per-element derivatives – ∇({x | x^2}) works
    // on vectors like it did on TFJS
    const [out, pullback] = jaxVjp((primal: np.Array) => {
      const result = (f as Function)(primal)
      const value = result instanceof Signal ? result.peek() : result
      if (value instanceof Error) { throw value }
      if (!isTensor(value)) { throw new Error("`grad(f)`: `f` must return a tensor") }
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
  const bounds = requireData(range)
  if (!Array.isArray(bounds) || bounds.length < 2) {
    throw new Error("`linspace([start, stop], count)`: range must be a 2-element tensor")
  }
  const [start, stop] = bounds as [number, number]
  // a count has to be a whole number; a live slider naturally drives it
  // through fractional values (resolution × 90 + 10), so round rather than reject
  return track(np.linspace(start, stop, Math.max(0, Math.round(asNumber(steps)))))
}

// Rank-polymorphic convolution: the kernel's rank sets the conv's rank, so the
// same conv slides a 1-D kernel over a vector or a 2-D kernel over an image.
// Backed by lax.conv – a singleton batch/channel wraps the field (NCHW), zero
// padded (SAME) so the output keeps the input's shape.
const TensorConvolution = (arr: Value, kernel: Value) => {
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
  track(np.roll(borrow(a), requireData(shift) as number | number[],
    axis === undefined ? undefined : requireData(axis) as number | number[]))

const TensorReverse = (a: Value, axis?: Value) =>
  track(np.flip(borrow(a), axis === undefined ? undefined : requireData(axis) as number | number[]))

const TensorMatrixMultiply = binaryOp(np.matmul)
const TensorDotProduct = binaryOp(np.dot)

const TensorLength = (a: Value, b?: Value) => {
  if (b !== undefined) {
    return track(np.array(shapeOf(a)[asNumber(b)] ?? NaN))
  }

  return track(np.array(shapeOf(a)[0] ?? NaN))
}

const TensorShape = (a: Value) => {
  // shapes are static even for tracers – seed the result so shape arithmetic
  // stays readable during jit traces and never round-trips the device
  const shape = shapeOf(a)
  const value = track(np.array(shape))
  syncReadCache.set(value, shape)
  return value
}

const TensorGather = (a: Value, b: Value) => {
  const size = shapeOf(a)[0] ?? 0
  // Bounds-check every index we can read without aborting a trace – jax-js
  // `take` has no bounds mode, so an out-of-range index otherwise reads garbage
  // (eager) or silently returns 0 (grad path). Seeded literals and concrete
  // arrays – including a computed eager index like `t_(argmax(...))` – are read
  // and validated; only a live tracer index (an embedding lookup mid-trace)
  // bails here, and those come from trusted data, so it flows through unchecked.
  let idx: unknown
  try { idx = getAsSyncList(b) } catch (e) { if (!(e instanceof TraceBailout)) throw e }
  if (idx !== undefined) {
    const flat = (Array.isArray(idx) ? idx.flat(Infinity) : [idx]) as number[]
    // `!(0 <= w < size)` also rejects a NaN index, which slips past `<`/`>=`
    const bad = flat.find((i) => { const w = i < 0 ? i + size : i; return !(w >= 0 && w < size) })
    if (bad !== undefined) {
      return new Error(`index ${bad} is out of bounds for an axis of length ${size}`)
    }
  }
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

const TensorData = (a: Value) => {
  if (!isTensor(a) && !(a instanceof FluentVariable)) {
    return new Error("`~~(init)`: initial value must be a tensor – make one slot per tensor")
  }
  return new FluentData(borrow(a) as np.Array)
}

const TensorAssign = (a: Value, b: Value) => {
  if (!(a instanceof FluentVariable)) {
    return new Error("`:=`: left side must be a variable created with ~ or ~~")
  }
  if (!isTensor(b) && !(b instanceof FluentVariable)) {
    // assigning an Error (an emptied corpus, a failed expression) must not
    // corrupt the variable: it keeps its last good value, the Error surfaces
    return new Error("`:=`: value must be a tensor", b instanceof Error ? { cause: b } : undefined)
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
  // the reference held between recomputes outlives the last one – release it
  // with the generation, or watch(θ) used inline (never bound into the scope,
  // so the teardown walk can't see it) pins θ's final value forever
  registerDisposable(() => {
    if (previous && previous.refCount > 0) { previous.dispose() }
    previous = null
  })
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
  let stepSlots: FluentData[] = []
  registerDisposable(() => {
    if (state !== null) {
      tree.dispose(state)
      state = null
    }
  })

  let compiledStep: ((params: np.Array[], data: np.Array[], state: optax.OptState) => [np.Array, np.Array[], optax.OptState]) & { dispose: () => void } | null = null
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
    if (vars.some((v) => v instanceof FluentData)) {
      // silent filtering would train nothing and report nothing – refuse loudly
      return new Error("~~ holds data and can't be trained – pass variables made with ~")
    }
    // data slots ride into the compiled step as jit arguments: read fresh
    // every call, never differentiated. A changed slot set invalidates the
    // captured closure, but not the per-parameter optimizer state.
    const slots = [...dataVariables]
    if (state !== null && (stateVars.length !== vars.length || stateVars.some((v, i) => v !== vars[i]))) {
      tree.dispose(state)
      state = null
      demoteStep()
      stepMode = "probe"
    }
    if (compiledStep !== null && (stepSlots.length !== slots.length || stepSlots.some((s, i) => s !== slots[i]))) {
      demoteStep()
      stepMode = "probe"
    }
    if (state === null) {
      state = transform.init(vars.map(v => v.current.ref))
      stateVars = vars
    }

    // gradient of the loss thunk: each variable temporarily carries its
    // parameter and each data slot its argument, so reads inside the thunk
    // trace. Only the params leaf differentiates – jax's default argnums is 0,
    // and jax-js stop-gradients the remaining arguments itself.
    const lossFromParams = (params: np.Array[], data: np.Array[]) => {
      const saved = vars.map(v => v.current)
      const savedData = slots.map(s => s.current)
      vars.forEach((v, i) => { v.current = params[i]! })
      slots.forEach((s, i) => { s.current = data[i]! })
      try {
        const out = (lossThunk as Function)()
        const value = out instanceof Signal ? out.peek() : out
        if (value instanceof Error) { throw value }
        if (!isTensor(value)) { throw new Error("loss must evaluate to a scalar tensor") }
        return value as np.Array
      } finally {
        vars.forEach((v, i) => { v.current = saved[i]! })
        slots.forEach((s, i) => { s.current = savedData[i]! })
      }
    }

    const eagerStep = (): Value => {
      const [loss, grads] = valueAndGrad(lossFromParams)(vars.map(v => v.current.ref), slots.map(s => s.current.ref))
      const [updates, nextState] = transform.update(grads as np.Array[], state!, vars.map(v => v.current.ref))
      state = nextState
      const fresh = optax.applyUpdates(vars.map(v => v.current.ref), updates as np.Array[]) as np.Array[]
      vars.forEach((v, i) => { v.assign(fresh[i]!) })
      return track(loss)
    }

    if (stepMode === "eager") { return eagerStep() }

    // compile the whole step – gradient, transform, apply – as one program,
    // retraced automatically when parameter or data shapes change
    if (compiledStep === null) { stepSlots = slots }
    compiledStep ??= jaxJit((params: np.Array[], data: np.Array[], optState: optax.OptState) => {
      const [loss, grads] = valueAndGrad(lossFromParams)(tree.ref(params) as np.Array[], data)
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
      const [loss, fresh, nextState] = compiledStep!(vars.map(v => v.current.ref), slots.map(s => s.current.ref), tree.ref(state!) as optax.OptState)
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

// optax's adam does its bias correction with count.item() – a data read that
// throws inside a jit trace, silently demoting every adam/adamw step to the
// eager path. This transform keeps the correction in-graph (count stays a
// tensor through np.power), so the compiled step survives tracing.
const adamTransform = (learningRate: number, weightDecay = 0, b1 = 0.9, b2 = 0.999, eps = 1e-8): optax.GradientTransformation => ({
  init: (params) => ({
    count: np.array(0),
    mu: tree.map((p: np.Array) => np.zerosLike(p), tree.ref(params) as any),
    nu: tree.map((p: np.Array) => np.zerosLike(p), params as any),
  }) as unknown as optax.OptState,
  update: (updates, state, params) => {
    const { count, mu, nu } = state as unknown as { count: np.Array, mu: np.Array[], nu: np.Array[] }
    const nextCount = np.add(count, 1)
    // tree.map hands leaf ownership to the callback – borrow with .ref only
    // what a later map still needs (the adagrad lesson)
    const newMu = tree.map((g: np.Array, m: np.Array) => np.add(np.multiply(g.ref, 1 - b1), np.multiply(m, b1)), tree.ref(updates) as any, mu as any)
    const newNu = tree.map((g: np.Array, n: np.Array) => np.add(np.multiply(np.square(g), 1 - b2), np.multiply(n, b2)), updates as any, nu as any)
    const c1 = np.subtract(1, np.power(b1, nextCount.ref))
    const c2 = np.subtract(1, np.power(b2, nextCount.ref))
    const scaled = weightDecay === 0
      ? tree.map((m: np.Array, n: np.Array) =>
          np.multiply(np.trueDivide(np.trueDivide(m, c1.ref), np.add(np.sqrt(np.trueDivide(n, c2.ref)), eps)), -learningRate),
          tree.ref(newMu) as any, tree.ref(newNu) as any)
      : tree.map((m: np.Array, n: np.Array, p: np.Array) =>
          np.add(
            np.multiply(np.trueDivide(np.trueDivide(m, c1.ref), np.add(np.sqrt(np.trueDivide(n, c2.ref)), eps)), -learningRate),
            np.multiply(p, -learningRate * weightDecay)),
          tree.ref(newMu) as any, tree.ref(newNu) as any, params as any)
    if (weightDecay === 0 && params) { tree.dispose(params) }
    c1.dispose()
    c2.dispose()
    return [scaled as any, { count: nextCount, mu: newMu, nu: newNu } as unknown as optax.OptState]
  },
})

// optax has no adagrad – accumulate squared gradients ourselves
const adagradTransform = (learningRate: number, eps = 1e-7): optax.GradientTransformation => ({
  init: (params) => tree.map((p: np.Array) => np.zerosLike(p), params as any),
  update: (updates, state, params) => {
    if (params) { tree.dispose(params) }
    const accum = tree.map((g: np.Array, s: np.Array) => np.add(np.square(g.ref), s), updates as any, state as any)
    // tree.map hands leaf OWNERSHIP to the callback – the second map must
    // consume its tree.ref(accum) copy (sqrt(s), not sqrt(s.ref)), or every
    // step leaks one reference per accumulator and the state never frees
    const scaled = tree.map(
      (g: np.Array, s: np.Array) => np.multiply(np.trueDivide(g, np.add(np.sqrt(s), eps)), -learningRate),
      updates as any, tree.ref(accum) as any)
    return [scaled as any, accum]
  },
})

// Arguments after the learning rate: scalars fill the hyperparameters in
// order, a list picks the variables to train (default: every variable alive)
const optimizerRest = (rest: Value[]): { nums: number[], vars: FluentVariable[] | undefined } => ({
  nums: rest.filter((r) => r !== undefined && !Array.isArray(r)).map(asNumber),
  vars: rest.find((r): r is FluentVariable[] => Array.isArray(r)),
})

const TensorOptimizationAdam = (a: Value, ...rest: Value[]) => {
  const { vars } = optimizerRest(rest)
  return makeOptimizer(adamTransform(asNumber(a)), vars)
}

// adamw(lr, weightDecay?, vars?) – Adam with decoupled weight decay
const TensorOptimizationAdamW = (a: Value, ...rest: Value[]) => {
  const { nums: [weightDecay], vars } = optimizerRest(rest)
  return makeOptimizer(adamTransform(asNumber(a), weightDecay ?? 1e-4), vars)
}

// sgd(lr, momentum?, vars?)
const TensorOptimizationSgd = (a: Value, ...rest: Value[]) => {
  const { nums: [momentum], vars } = optimizerRest(rest)
  return makeOptimizer(optax.sgd(asNumber(a), momentum === undefined ? undefined : { momentum }), vars)
}

const TensorOptimizationAdaGrad = (a: Value, ...rest: Value[]) => {
  const { vars } = optimizerRest(rest)
  return makeOptimizer(adagradTransform(asNumber(a)), vars)
}

// Stateful convenience RNG: a fresh Threefry key per call, mirroring the feel
// of TFJS's tf.random* – deterministic per page load, not across reloads.
let rngCounter = 0
const nextRngKey = () => random.key(rngCounter++)
const TensorRandomNormal = (a: Value) => track(random.normal(nextRngKey(), asNumberList(a)))
const TensorRandomUniform = (a: Value) => track(random.uniform(nextRngKey(), asNumberList(a)))

const StringConcat = (...args: any[]) => "".concat(...args)
const StringLength = (a: string) => track(np.array(a.length))

// length works on whatever it's handed – a tensor's leading axis, a list's
// element count, or a string's characters – by trying each in turn.
const Length = FunctionCascade([TensorLength, ListLength, StringLength])

// Text ↔ tensor: the door between strings and token land. Codes are float32 –
// small integers are exact in float, and float spares downstream math from
// int-promotion surprises. Decode rounds, so model outputs convert directly.
const StringToCodes = (a: Value) => {
  // string literals evaluate to boxed Strings (they carry source origin)
  if (typeof a !== "string" && !(a instanceof String)) {
    return new Error("`StringToCodes(text)`: expected a string")
  }
  return track(np.array(Float32Array.from(String(a), (c) => c.codePointAt(0)!)))
}

const CodesToString = (a: Value) => {
  const data = getAsSyncList(a) // TraceBailout on tracers, like any data read
  const codes = Array.isArray(data) ? (data as unknown[]).flat(Infinity) : [data]
  if (!codes.every((v): v is number => typeof v === "number" && Number.isFinite(v))) {
    return new Error("`CodesToString(codes)`: expected a tensor of character codes")
  }
  let text = ""
  for (const code of codes) {
    text += String.fromCodePoint(Math.min(0x10FFFF, Math.max(0, Math.round(code))))
  }
  return text
}

const Null = null


// MARK: Built-in docs
// Authored on the canonical functions themselves, so every prelude alias that
// resolves to one (⌈ → TensorMaximum, once → SignalOnce, …) inherits the same
// card: the doc lives on the value, not the name. Editor hover and completion
// read it back with getMeta. Signatures use the ergonomic notation people type.
doc(SignalOnce, "once(signal)", "Read a signal's current value without subscribing to it.", "x: $(4), once(x) + 1 = 5")
doc(Reactive, "$(value)", "Wrap a value in a signal (or a thunk in a computed signal). Read with x(), write with x(v).", "x: $(0.5), x ^ 2")
doc(TensorVariable, "~(init)", "Make a trainable variable. Assign with :=; optimise with adam / adamw / sgd / adagrad. For data the loss reads but never trains, use ~~.", "θ: ~([0, 0])")
doc(TensorData, "~~(init)", "Make a data slot – every optimizer step reads it fresh, but no gradient flows into it and nothing trains it. Assign with := any time, even to a new shape – training carries on.", "x: ~~([1, 2]), x := [3, 4, 5]")
doc(StringToCodes, "StringToCodes(text)", "Text to a tensor of character codes – the door from strings into tensors.", "StringToCodes(\"abc\") = [97, 98, 99]")
doc(CodesToString, "CodesToString(codes)", "A tensor of character codes back to text. Rounds first, so model outputs decode directly.", "CodesToString([104, 105]) = \"hi\"")
doc(TensorOptimizationAdamW, "adamw(lr, weightDecay?, vars?)", "Adam with decoupled weight decay. A trailing list picks the variables to train.", "opt: adamw(0.01, 0.001)")
doc(TensorOptimizationSgd, "sgd(lr, momentum?, vars?)", "Stochastic gradient descent, with optional momentum. A trailing list picks the variables to train.", "opt: sgd(0.01, 0.9)")
doc(TensorWatch, "watch(variable)", "A signal that updates whenever a variable is assigned – by a drag, an optimizer, or :=.", "θ: ~([2]), w: watch(θ), θ := [8], w = [8]")
doc(TensorGradient, "∇(f)", "Gradient of a function. ∇(f)(x) is df/dx, evaluated at x.", "∇({ x | x^2 })(3) = 6")
doc(TensorSum, "Σ(x, axis?)", "Sum of the elements, over one axis or the whole tensor.", "Σ([1, 2, 3]) = 6")
doc(TensorReduce, "x reduce fn", "Fold a tensor left-to-right with a binary function, optionally along an axis. Known ops (+, ×, ⌈, ⌊) reduce natively; so `x reduce ⌈` is max.", "1 :: 10 reduce + = 45")
doc(TensorMaximum, "x ⌈ y", "Element-wise maximum of two tensors.", "[1, 5] ⌈ [4, 2] = [4, 5]")
doc(TensorMinimum, "x ⌊ y", "Element-wise minimum of two tensors.", "[1, 5] ⌊ [4, 2] = [1, 2]")
doc(TensorMax, "max(x, axis?)", "The largest element, over one axis or the whole tensor.", "max([3, 1, 2]) = 3")
doc(TensorMin, "min(x, axis?)", "The smallest element, over one axis or the whole tensor.", "min([3, 1, 2]) = 1")
doc(TensorCeil, "⌈(x)  ·  ceil(x)", "Round each element up to the nearest integer.", "⌈(2.3) = 3")
doc(TensorFloor, "⌊(x)  ·  floor(x)", "Round each element down to the nearest integer.", "⌊(2.7) = 2")
// Leaf docs for the arity-overloaded operators – FunctionArity synthesizes
// the +, -, ×, · cards from these
doc(TensorAdd, "x + y", "Element-wise addition; shapes broadcast.", "[1, 2] + 10 = [11, 12]")
doc(TensorAbsolute, "abs(x)", "Absolute value of each element.", "abs([-2, 3]) = [2, 3]")
doc(TensorSubtract, "x - y", "Element-wise subtraction; shapes broadcast.", "[3, 4] - 1 = [2, 3]")
doc(TensorNegate, "neg(x)", "Negate each element.", "neg([1, -2]) = [-1, 2]")
doc(TensorMultiply, "x × y", "Element-wise multiplication; shapes broadcast.", "[1, 2] × 3 = [3, 6]")
doc(TensorSign, "sign(x)", "The sign of each element: -1, 0, or 1.", "sign([-5, 0, 3]) = [-1, 0, 1]")
doc(TensorRange, "start :: stop", "Integer range from start (inclusive) to stop (exclusive).", "0 :: 5 = [0, 1, 2, 3, 4]")
doc(TensorReshape, "x ⍴ shape", "Reshape a tensor to a new shape; one dimension may be -1 to infer it.", "[1, 2, 3, 4] ⍴ [2, 2] = [[1, 2], [3, 4]]")
doc(TensorOuter, "a (⊗ f) b", "Table: apply f between every cell of a and every cell of b.", "(0 :: 3) (⊗ ×) (0 :: 3) = [[0,0,0],[0,1,2],[0,2,4]]")
doc(TensorRoll, "roll(x, shift, axis?)", "Shift elements along an axis, wrapping around the edge (a torus).", "roll([1, 2, 3, 4], 1) = [4, 1, 2, 3]")
doc(TensorSort, "sort(x)", "Sort a vector into ascending order.", "sort([3, 1, 2]) = [1, 2, 3]")
doc(TensorArgSort, "argsort(x)", "The indices that sort a vector into ascending order – grade up. x_argsort(x) is x sorted.", "argsort([3, 1, 2]) = [1, 2, 0]")
doc(Length, "length(x)", "How many: a tensor's leading axis, a list's elements, or a string's characters.", "length(\"abc\") = 3")
doc(TensorMask, "mask(x, keep)", "Keep the elements of x where the boolean mask is true, dropping the rest.", "mask([5, 0, 6], [5, 0, 6] > 1) = [5, 6]")
doc(TensorWhere, "where(cond, a, b)", "Element-wise choice: take a where cond is true, otherwise b.", "where([1, 0, 1], [1, 2, 3], 0) = [1, 0, 3]")
doc(TensorOr, "x ∨ y", "Element-wise logical or: 1 where either operand is nonzero, else 0.", "[0, 1, 0] ∨ [0, 1, 1] = [0, 1, 1]")
doc(TensorAnd, "x ∧ y", "Element-wise logical and: 1 where both operands are nonzero, else 0.", "[0, 1, 1] ∧ [1, 1, 0] = [0, 1, 0]")
doc(TensorNot, "¬(x)", "Logical not: 1 where x is zero, else 0.", "¬([0, 2]) = [1, 0]")
doc(TensorXor, "x ⊻ y", "Element-wise exclusive or: 1 where exactly one operand is nonzero.", "[0, 1, 1] ⊻ [1, 1, 0] = [1, 0, 1]")
doc(TensorNand, "x ⍲ y", "Element-wise nand: 0 where both operands are nonzero, else 1. Functionally complete – every gate builds from it.", "[0, 1, 1] ⍲ [1, 1, 0] = [1, 0, 1]")
doc(TensorNor, "x ⍱ y", "Element-wise nor: 1 where both operands are zero, else 0. Functionally complete, like ⍲.", "[0, 1, 0] ⍱ [0, 1, 1] = [1, 0, 0]")

// Control & function machinery – these cards teach the language, not just a
// function: scoping, the three assignments, application, iteration, and the
// errors-are-values control flow have no NumPy analogue to lean on.
doc(SymbolAssign, "name: value", "Bind a name in the current scope. Glued to its left operand, `:` takes everything to its right – `a: 1 + 2` binds 3.", "a: 1 + 2, a × 10 = 30")
doc(TensorAssign, "θ := value", "Assign a new value to a variable (made with ~ or ~~); optimizers do this every step, watch(θ) sees it. Mid-expression, parenthesize the value: θ := (a + b).", "θ: ~([0, 0]), θ := [1, 2]")
doc(FunctionApply, "args . f", "Pipe: apply the function on the right to the value on the left; a list spreads as arguments.", "[3, 1, 2] . sort = [1, 2, 3]")
doc(FunctionEvaluate, "f @ x", "Apply the function on the left to the argument on the right – reads as “f at x”. A list spreads as multiple arguments.", "∇({ x | x^2 }) @ 3 = 6")
doc(FunctionIterate, "step ⟳ n", "Run a thunk n times, paced between display frames so the UI stays live – the loop for training and simulation. Async: a re-evaluation cancels it.", "opt: sgd(0.1), { opt(𝓛) } ⟳ 100")
doc(FunctionPower, "(f ⍣ n)(x)", "Function power: f composed with itself n times – f(f(…f(x))). Synchronous; for a frame-paced loop use ⟳.", "double: { x | x × 2 }, (double ⍣ 5)(1) = 32")

// Combinators – the cards name the birds so the taxonomy stays discoverable
doc(FunctionCompose, "(f ∘ g)", "Then: apply f to the arguments, then g to the result – (f ∘ g)(x, y) = g(f(x, y)). Reading order is evaluation order. The Queer bird – APL's atop, swapped to read left-to-right.", "(+ ∘ √)(9, 16) = 5")
doc(FunctionCommute, "(⍨ f)", "Commute: flip a binary function's arguments – x (⍨ f) y = f(y, x). With one argument, duplicate it: (⍨ f)(x) = f(x, x). The Cardinal and the Warbler.", "3 (⍨ -) 10 = 7")
doc(FunctionOver, "(g ⍥ f)", "Over: preprocess each argument with g, then combine with f – x (g ⍥ f) y = f(g(x), g(y)). The preprocessor is read first because it runs first. The Psi bird.", "-3 (abs ⍥ =) 3 = 1")
doc(FunctionFork, "Φ(f, g, h)", "Fork: apply the outer tines f and h to the arguments, combine the results with the middle tine g – x Φ(f, g, h) y = g(f(x, y), h(x, y)). A non-function tine is held constant. The Phoenix; dyadically the Pheasant.", "Φ(Σ, ÷, #)([1, 2, 3, 4]) = 2.5")
doc(FunctionBefore, "(f ⊸ g)", "Before: preprocess the left (or only) argument with f, then combine with g – (f ⊸ g)(x, y) = g(f(x), y); (f ⊸ g)(x) = g(f(x), x). A constant binds the left argument. The Violet Starling; dyadically the Zebra Dove.", "(1 ⊸ +)(41) = 42")
doc(FunctionAfter, "(f ⟜ g)", "After: preprocess the right (or only) argument with g, then combine with f – (f ⟜ g)(x, y) = f(x, g(y)); (f ⟜ g)(x) = f(x, g(x)) – the J hook. A constant binds the right argument. The Starling; dyadically the Dove.", "(÷ ⟜ √)(16) = 4")
doc(FunctionRight, "x ⊢ y", "Right: yield the rightmost argument; with one argument, the identity.", "3 ⊢ 5 = 5")
doc(FunctionLeft, "x ⊣ y", "Left: yield the leftmost argument; with one argument, the identity.", "3 ⊣ 5 = 3")
doc(FunctionIs, "isFunction(v)", "1 if v is a function, else 0 – the question a combinator asks before treating an operand as a constant. Composes like a comparison.", "isFunction(√) = 1, isFunction(5) = 0")
doc(FunctionCascade, "cascade((f, g, …))", "Try candidates in order; the first result that isn’t an Error wins. Errors are values in Fluent – falling through is intended, not exceptional.", "cascade((guard(n = 0, { 1 }), { n × f(n - 1) }))()")
doc(FunctionGuard, "guard(cond, { value })", "A cascade candidate: yields the value while cond is truthy, an Error otherwise.", "guard(n = 0, { 1 })")
doc(FunctionWhen, "when(cond, { … })", "A conditional effect: run the thunk while cond is truthy, yield ◌ otherwise. Errors from the thunk stay loud – only the condition gates.", "when(training(), { opt(𝓛) })")


// MARK: Environment

// Null-prototype: an environment is a bare name→value mapping. A plain `{}`
// would chain to Object.prototype, so unbound names like `constructor`,
// `toString`, or `valueOf` would resolve to native JS members instead of
// staying unbound symbols (and `constructor(5)` would silently run Object(5)).
const DefaultEnvironment: Record<string, Value> = Object.assign(Object.create(null), {
  [Symbol.keyFor(Symbol.for("Null"))!]: Null,

  SymbolAssign,

  CodeParse,
  CodeEvaluate,

  FunctionIterate,
  FunctionPower,
  FunctionCompose,
  FunctionCommute,
  FunctionOver,
  FunctionFork,
  FunctionBefore,
  FunctionAfter,
  FunctionRight,
  FunctionLeft,
  FunctionIs,
  FunctionCascade,
  FunctionArity,
  FunctionEvaluate,
  FunctionApply,
  FunctionNoAutoLift,
  FunctionGuard,
  FunctionWhen,
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
  TensorOr,
  TensorAnd,
  TensorNot,
  TensorXor,
  TensorNand,
  TensorNor,
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
  TensorReduce,
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
  TensorArgSort,

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
  Length,
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
  TensorData,
  TensorAssign,
  TensorWatch,
  TensorOptimizationAdam,
  TensorOptimizationAdamW,
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
  StringToCodes,
  CodesToString,

  "◌": Null,
  "null": Null,
})

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

; Combinators – glyph, array-language name, mainstream name
(∘): FunctionCompose,
then: FunctionCompose,
compose: FunctionCompose,
(⍨): FunctionCommute,
commute: FunctionCommute,
swap: FunctionCommute,
(⍥): FunctionOver,
over: FunctionOver,
Φ: FunctionFork,
fork: FunctionFork,
phi: FunctionFork,
(⊸): FunctionBefore,
before: FunctionBefore,
(⟜): FunctionAfter,
after: FunctionAfter,
hook: FunctionAfter,
(⊢): FunctionRight,
right: FunctionRight,
(⊣): FunctionLeft,
left: FunctionLeft,
isFunction: FunctionIs,

(@): FunctionEvaluate,
eval: FunctionEvaluate,
cascade: FunctionCascade,
guard: FunctionGuard,
when: FunctionWhen,

; Tensor shape/indexing
(#): Length,
length: Length,
len: Length,
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
flat: (⍴ ⟜ [-1]),
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
conv: TensorConvolution,   ; nD convolution: arr conv kernel – a 1D kernel over a vector, a 2D kernel over an image

; List operations
ListGather: { a, b |
  ListMap(b, { i | ListGet(a, i) })
},
ListZip: { a, b |
  n: TensorMinimum(ListLength(a), ListLength(b)),
  ListMap(
    TensorUnstack(TensorRange(0, n)),
    { i | List(ListGet(a, i), ListGet(b, i)) }
  )
},
ListTake: { list, n |
  ; take the first n, with n clamped to [0, length]
  ListMap(TensorUnstack(TensorRange(0, 0 ⌈ n ⌊ ListLength(list))), { i | ListGet(list, i) })
},
ListDrop: { list, n |
  len: ListLength(list),
  ListMap(TensorUnstack(TensorRange(0 ⌈ n ⌊ len, len)), { i | ListGet(list, i) })
},
ListReverse: { list |
  n: ListLength(list),
  ListMap(TensorUnstack(TensorReverse(TensorRange(0, n))), { i | ListGet(list, i) })
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
(+): FunctionArity((TensorAdd, TensorAbsolute)),
(-): FunctionArity((TensorSubtract, TensorNegate)),
(*): FunctionArity((TensorMultiply, TensorSign)),
(×): FunctionArity((TensorMultiply, TensorSign)),
(·): FunctionArity((TensorMultiply, TensorSign)),

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

; Logical
(∨): TensorOr,
or: TensorOr,
(∧): TensorAnd,
and: TensorAnd,
(¬): TensorNot,
not: TensorNot,
(⊻): TensorXor,
xor: TensorXor,
(⍲): TensorNand,
nand: TensorNand,
(⍱): TensorNor,
nor: TensorNor,

; Reductions
(∇): TensorGradient,
grad: TensorGradient,
(Σ): TensorSum,
sum: TensorSum,
reduce: TensorReduce,
(Π): TensorProduct,
prod: TensorProduct,
(μ): TensorMean,
mean: TensorMean,

; Min/max: the WORD reduces (whole tensor, or one axis like sum) – so
; max(x, axis) works. The GLYPH is APL's ⌈/⌊: ceiling/floor on one tensor,
; pairwise max/min on two. (max and ⌈ can't be one function – max(x, axis)
; and pairwise max(a, b) share a shape, so the axis would be eaten as an operand.)
max: TensorMax,
min: TensorMin,
(⌈): FunctionArity((TensorMaximum, TensorCeil)),
(⌊): FunctionArity((TensorMinimum, TensorFloor)),
argmax: TensorArgMax,
argmin: TensorArgMin,

; Variables
(~): TensorVariable,
var: TensorVariable,
(~~): TensorData,
data: TensorData,
watch: TensorWatch,

; Tensor ops
sort: TensorSort,
argsort: TensorArgSort,
(⍋): TensorArgSort,
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
adamw: TensorOptimizationAdamW,
sgd: TensorOptimizationSgd,
adagrad: TensorOptimizationAdaGrad,

; Misc
($): Reactive,
; ← is defined here in the prelude, so its doc lives here too
(←): doc(FunctionNoAutoLift({ s, v | s(v) }), "signal ← value", "Write a value into a signal – same as signal(value), but reads left-to-right. Chains like any spaced operator: parenthesize compound values, s ← (a + b).", "x: $(1), x ← 9, x() = 9"),
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
  FluentData,
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
  flushRetirements,
  registerDisposable,
  disposeScopeTensors,
  disposeValueTensors,
  // leak watch (test/dev)
  beginTensorWatch,
  endTensorWatch,
  liveTensorCount,
  peakTensorCount,
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
