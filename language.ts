// MARK: Fluent language core
// Grammar, evaluator, environment and prelude – no DOM, no React, no Monaco.
// The evaluation worker runs it (host.ts), the page only parses with it (client.tsx), and
// the end-to-end tests run it headless (tests.ts). Whoever computes starts the engine.

import { grammar, type ActionDict } from "ohm-js";
import { toAST as ohmToAST } from "ohm-js/extras";
import { signal, Signal, computed, effect, untracked } from "@preact/signals-core"
import dedent from "ts-dedent";
import * as engine from "./engine/tensor.ts"
import PRELUDE from "./prelude.fl" with { type: "text" }

const isTensor = (v: unknown): v is engine.Tensor => v instanceof engine.Tensor


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
  DIE_FACES: ["\u2680", "\u2685"], // Die faces \u2680\u2681\u2682\u2683\u2684\u2685 \u2013 \u2684 aliases rand
  MIDDLE_DOT: ["\u00B7", "\u00B7"], // Middle Dot (multiplication)
  DOUBLE_VERTICAL_LINE: ["\u2016", "\u2016"], // Norm
  VARIATION_SELECTORS: ["\uFE00", "\uFE0F"], // Variation Selectors (e.g. \u2194\uFE0E = \u2194 + U+FE0E)
};

const RESERVED_SYMBOLS = "[|,{}()\\[\\];]"

const IDENTIFIER_RANGES: Record<string, [string, string]> = {
  MATHEMATICAL_ALPHANUMERIC_SYMBOLS: [String.fromCodePoint(0x1D400), String.fromCodePoint(0x1D7FF)], // Mathematical Alphanumeric Symbols
  EMOJIS: [String.fromCodePoint(0x1F300), String.fromCodePoint(0x1FFFF)], // Emoticons
}
// Monaco highlights with these regexes; deriving them from the same range tables
// the ohm grammar uses keeps the editor lexer from drifting from the parser.
const rangesToCharClass = (ranges: Record<string, [string, string]>): string =>
  Object.values(ranges).map(([lo, hi]) => {
    const a = "\\u{" + lo.codePointAt(0)!.toString(16) + "}"
    const b = "\\u{" + hi.codePointAt(0)!.toString(16) + "}"
    return lo === hi ? a : a + "-" + b
  }).join("")
const IDENTIFIER_CHAR_CLASS = rangesToCharClass(IDENTIFIER_RANGES)
// identifier: a letter or identifier-range symbol (math-alphanumeric, emoji), then
// letters / digits / those symbols — mirrors the grammar's `identifier` rule. No
// '-', which is an operator now, not an identifier character.
const identifierRegexp = new RegExp(`[\\p{L}${IDENTIFIER_CHAR_CLASS}][\\p{L}\\p{N}${IDENTIFIER_CHAR_CLASS}]*`, "u")

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

// operator: one symbol from the operator ranges (the grammar's `specialChar`); the
// token pipeline peels identifiers/numbers/strings/delimiters off first, so the
// alnum/reserved chars that BASIC_OPERATORS overlaps never reach this rule.
const operatorRegexp = new RegExp(`[${rangesToCharClass(OPERATOR_RANGES)}]`, "u")

const delimiterRegexp = new RegExp(RESERVED_SYMBOLS, "u")

// MARK: Grammar

const CodeGrammar = String.raw`
Fluent {
  Program
    = ListOf<Expr, ","> ","?

  Expr
    = Operation

  // Every non-tight operator lives here; the whitespace around it picks its tier:
  //  · glued-LEFT "a op· b" – glued to its left operand, space on its right – is the
  //    LOOSEST. It splits: the whole chain on its left, the entire rest on its right,
  //    right-associatively. "a: 1 + 2"; "1 + 2* 3 + 4" is "(1 + 2) * (3 + 4)". The
  //    (Operation | TightOperation) left gives the split both a non-recursive seed
  //    (the tight left of "a: …") and a recursive one (the spaced left of "a + b*").
  //    The operator is any Atom, so referential transparency holds – "1add 2 * 3" is
  //    f(1, 2*3). (A number can't glue lexically, and a glued "(...)" is an argument
  //    list – see Application below.)
  //  · spaced "a op b" (and inert glued-RIGHT "a ·op b") chain left-to-right.
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
  identifier      = &(letter | idRanges) (alnum | idRanges)+
  idRanges        = ${getSymbolsRange(IDENTIFIER_RANGES)}
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
  specialChar     = ${getSymbolsRange(OPERATOR_RANGES)}
  space          += ";" (~"\n" any)* -- comment
}
`;

const CodeGrammarCompiled = grammar(CodeGrammar);

type Origin = {
  source: string;
  start: { line: number; column: number };
  end: { line: number; column: number };
  // The whole program this origin's positions index into. Several documents
  // can be live at once (the playground program, each tour/REPL cell) – the
  // IDE routes hover-highlights to the editor whose text IS this document.
  document?: string;
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
// column), found by binary search over the document's line starts – the parser asks
// twice per node, and a scan from the top made a parse quadratic
let lineStarts: { text: string, starts: number[] } = { text: "", starts: [0] }
const lineAndColumn = (text: string, offset: number): { line: number, column: number } => {
  if (lineStarts.text !== text) {
    const starts = [0]
    for (let i = 0; i < text.length; i++) { if (text[i] === "\n") { starts.push(i + 1) } }
    lineStarts = { text, starts }
  }
  const starts = lineStarts.starts
  let lo = 0, hi = starts.length - 1
  while (lo < hi) { const mid = (lo + hi + 1) >> 1; if (starts[mid]! <= offset) { lo = mid } else { hi = mid - 1 } }
  let column = 1
  for (let i = starts[lo]!; i < offset; i++) { if (text[i] !== "\r") { column++ } }
  return { line: lo + 1, column }
}

function getLocationOrigin(node: any): Origin {
  const document: string = node.source.sourceString

  return {
    source: node.source.contents,
    document,
    start: lineAndColumn(document, node.source.startIdx),
    end: lineAndColumn(document, node.source.endIdx),
  }
}

// A backtick literal's inner Program is parsed on its own (see the Code action),
// so its nodes' origins are relative to the inner text. Shift them into the
// outer document – (startLine, startColumn) is where the inner text begins – so
// editor hover-highlighting maps each viz node back to the right source range.
// Only the first inner line needs the column offset; later lines start at col 1
// in both.
const shiftOrigins = (node: any, startLine: number, startColumn: number, document: string): void => {
  if (!node || typeof node !== "object") return
  if (node.origin?.start && node.origin?.end) {
    const map = (p: { line: number; column: number }) => ({
      line: startLine + p.line - 1,
      column: p.line === 1 ? startColumn + p.column - 1 : p.column,
    })
    // positions are now outer-document coordinates, so the document is the
    // outer one too – highlights must land in the editor that shows it
    node.origin = { ...node.origin, document, start: map(node.origin.start), end: map(node.origin.end) }
  }
  for (const key in node) {
    if (key === "origin") continue
    const child = node[key]
    if (Array.isArray(child)) { for (const c of child) shiftOrigins(c, startLine, startColumn, document) }
    else if (child && typeof child === "object") shiftOrigins(child, startLine, startColumn, document)
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
  Operation_gluedSplit(left, operator, _space, right) {
    return {
      type: "Operation",
      content: {
        // toAST (not operatorSymbolNode) so the glued operator keeps its real
        // value node – a Symbol stays a Symbol, a Lambda stays a Lambda, etc.
        // (referential transparency, same handling as Operation_infix)
        operator: operator.toAST(this.args.mapping),
        args: {
          type: "List",
          origin: getLocationOrigin(this),
          content: {
            value: [left.toAST(this.args.mapping), right.toAST(this.args.mapping)],
            synthetic: true,   // infix operands are not an explicit ( … ): don't scope their bindings
          },
        }
      },
      origin: getLocationOrigin(this),
    }
  },
  Operation_infix(left, operator, right) {
    return {
      type: "Operation",
      content: {
        operator: operator.toAST(this.args.mapping),
        args: {
          type: "List",
          origin: getLocationOrigin(this),
          content: {
            value: [left.toAST(this.args.mapping), right.toAST(this.args.mapping)],
            synthetic: true,   // infix operands are not an explicit ( … ): don't scope their bindings
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
            value: [left.toAST(this.args.mapping), right.toAST(this.args.mapping)],
            synthetic: true,   // infix operands are not an explicit ( … ): don't scope their bindings
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
  Operation_prefix(left, right) {
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
        value: +value.sourceString,
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
    shiftOrigins(inner, contentStart.line, contentStart.column, this.source.sourceString)
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

// Ohm's rightmost-failure message enumerates every alternative at the
// failure point – for this grammar that includes whole operator glyph
// ranges ("⚀".."⚅", "⦀".."⧿", …), which reads as line noise. A short
// expectation list is genuinely helpful ("expected \")\"") and stays; a
// long one collapses to the honest summary.
const humanizeParseError = (message: string): string => {
  const at = message.lastIndexOf("expected ")
  if (at < 0) { return message }
  const alternatives = message.slice(at + "expected ".length).split(", ")
  if (alternatives.length <= 4) { return message }
  return message.slice(0, at) + "expected an expression"
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
      content: humanizeParseError(matchResult.shortMessage ?? matchResult.message ?? "Unknown parse error"),
      origin: {
        source: program,
        document: program,
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
    message: humanizeParseError(matchResult.shortMessage ?? matchResult.message ?? "Syntax error"),
    start: startLocation,
    end: endLocation
  }];
}

// MARK: Evaluate

type Value = engine.Tensor | FluentVariable | Function | Signal<Value> | Error | string | String | symbol | null | Value[]
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
    const value = engine.tensor(node.content.value)
    setOrigin(value, node.origin)
    return value
  }

  if (node.type === "Tensor") {
    // a binding inside a [ … ] literal is local to it, not leaked to the enclosing scope
    const localEnv = Object.create(env) as CurrentScope
    const values = node.content.value.map((e) => evaluateSyntaxTreeNode(e, localEnv))
    if (values.length === 0) {
      const value = engine.tensor([])
      setOrigin(value, node.origin)
      return value
    }

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
    // Use String object (not primitive) so we can track origin via WeakMap.
    // A backslash escapes the next character literally – so a string can hold a
    // quote (\") or a backslash (\\). (Minimal: no \n/\t sequences yet.)
    const value = new String(dedent(node.content.value).replace(/\\([\s\S])/g, "$1"))
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

      // Reify in localEnv so symbols resolve to local values, not outer scope
      return reify(evaluateSyntaxTreeNode(node.content.expr, localEnv), localEnv)
    }

    fn.toString = () => node.origin.source
    setOrigin(fn, node.origin)

    return fn
  }

  if (node.type === "Operation") {
    const fn = evaluateSyntaxTreeNode(node.content.operator, env)

    // a binding among a call's args (f(a: 2, b)) is local to the call. Infix operands
    // aren't an explicit ( … ), so they bind in the enclosing scope: a: b: 23 binds both.
    const argsEnv = (node.content.args.content.synthetic ? env : Object.create(env)) as CurrentScope
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

engine.Tensor.prototype.toString = function () {
  if (this.size <= 32) { return `${getAsSyncList(this)}` }
  return `Tensor [${this.shape.join(" ")}]`
}

// String coercion (`${x}`, String(x)) goes through Symbol.toPrimitive: a scalar asked
// for a number gives its value, anything else prints.
;(engine.Tensor.prototype as any)[Symbol.toPrimitive] = function (hint: string) {
  if (hint === "number" && this.ndim === 0) { return this.item() }
  return this.toString()
}

// Fluent identifiers are interned symbols (Symbol.for(name)); these look one up
// in the scope chain. Free functions, not Symbol.prototype methods, so we don't
// mutate the global Symbol for every symbol in the process (and every dependency).
const resolveSymbol = (sym: symbol, env: CurrentScope): Value => {
  const k = Symbol.keyFor(sym) ?? ""
  // `in` walks the scope chain: a name bound to ◌ resolves to null, an unbound one
  // stays its symbol – with no scope at hand, every name is unbound
  return env != null && k in env ? (env[k] ?? null) : sym
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

// MARK: Values
// A tensor is an engine value: an immutable graph node, computed when something reads it,
// its buffers owned by the engine. What crosses time – a variable's value, a signal's –
// is settled into a leaf, so it never drags its history along.

// A differentiated body can't change state: ∇ would have to undo the write along every
// direction it takes. A write while differentiating throws this; ∇ and the optimizer
// turn it into an Error value. It is control flow, so choosing passes it through.
class StateWrite extends Error {}
let differentiating = false

// An op's operand: a variable's value or a tensor. An unbound name or a list fails here,
// with the name or the fix in the message.
const operand = (v: any): any => {
  if (v instanceof FluentVariable) { return v.current }
  if (v == null) { throw new Error("expected a tensor, got ◌") }
  if (typeof v === "symbol") {
    // an unbound name reached a numeric op (e.g. `1 * x`): fail with the name. The bare
    // symbol still flows through as a value elsewhere – only coercion errors.
    throw new Error(`unknown name: ${v.description ?? Symbol.keyFor(v) ?? String(v)}`)
  }
  if (Array.isArray(v)) {
    // Never silently coerce a list into a tensor: a list is a structure, not a number
    // bag – write `[1, 2, 3]` for a tensor.
    throw new Error(`expected a tensor, got a list of ${v.length} – write it as a tensor [ … ]`)
  }
  // the Error itself becomes the cause (safeApply), so the chain still says why
  if (v instanceof Error) { throw new Error("expected a tensor, got an Error") }
  return v
}

const shapeOf = (v: Value): number[] =>
  (v instanceof FluentVariable ? v.current : (v as engine.Tensor)).shape

// A scalar is a rank-0 tensor holding a single phantom item (#(5) = 1), so
// index/sort/softmax on it return the rank-0 identity rather than leaking the
// backend's "axis of length 0" for an axis the scalar doesn't have.
const isScalarTensor = (v: Value): boolean =>
  (isTensor(v) || v instanceof FluentVariable) && shapeOf(v).length === 0

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
// Each top-level evaluation owns the resources it creates: live sources (Time,
// Camera, ...), pending ⟳ loops, effects. A new evaluation retires the previous
// generation – loops stop, sources release their hardware.
type Generation = { disposables: Set<() => void> }
let currentGeneration: Generation | null = null

const registerDisposable = (dispose: () => void) => {
  currentGeneration?.disposables.add(dispose)
}

// Generations awaiting retirement. A retired generation's effects stop only once the
// NEW UI has committed and the old islands unmounted – a still-mounted old island
// could otherwise recompute against a stopped source.
// The renderer signals that exact moment by calling flushRetirements() from a
// post-commit effect (replacing a 50 ms timer that only guessed at it); tests
// and headless callers call it directly.
const pendingRetirements = new Set<Generation>()

const retireGeneration = (g: Generation) => {
  for (const dispose of g.disposables) {
    try { dispose() } catch { /* already gone */ }
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
  currentGeneration = { disposables: new Set() }
  try {
    return evaluate()
  } finally {
    // queue the previous generation even if building this one threw, so its
    // effects and sources never leak on an error
    if (previous) {
      pendingRetirements.add(previous)
      scheduleRetirementFlush()
    }
  }
}

// Signals may hide inside lists, so e.g. `concat((a, b), axis)` stays
// reactive when a/b are computed
const containsSignal = (a: Value): boolean =>
  a instanceof Signal || (Array.isArray(a) && a.some(containsSignal))
// a signal may hold a signal – a result still on its way (CodesToString) – so a lifted
// application reads through to the value, and recomputes when either level changes
const unwrapSignals = (a: any): any =>
  a instanceof Signal ? unwrapSignals(a.value) : Array.isArray(a) ? a.map(unwrapSignals) : a

// MARK: Lifted apply
// An application whose arguments hold signals recomputes with their current values; the
// engine fuses whatever the chain of lifted computeds builds, at the read. A throw names
// the application that failed – a bare engine message with no callsite is undebuggable.
const applyLifted = (fnValue: Function, argsValue: Value[], env: CurrentScope): Value => {
  const unwrapped = argsValue.map(unwrapSignals)
  try {
    return fnValue.apply(env, unwrapped)
  } catch (e) {
    if (e instanceof StateWrite) { throw e }
    const err = e instanceof Error ? e : new Error(String(e))
    const label = (fnValue as { name?: string }).name || getOrigin(fnValue)?.source || "a function"
    return new Error(`applying ${label} to (${unwrapped.map(describeArg).join(", ")})`, { cause: err })
  }
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

    // KNOWN WART: arity is not checked here, so over-application silently drops
    // surplus arguments – `+(1, 2, 4)` is `3`, not an error, because a binary JS
    // op ignores the third arg. `fn.length` can't be the source of truth (it
    // can't tell `()` nullary from `(...args)` variadic), so a proper fix makes
    // arity a declared, first-class attribute of every function. Deferred – the
    // design lives in arity-plan.md.

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

    // Auto-lift: an application whose arguments contain signals is a computed
    if (argsValue.some(containsSignal) && !noAutoLift) {
      return computed(() => applyLifted(fnValue as Function, argsValue, env))
    }

    return fnValue.apply(env, argsValue)
  } catch (e) {
    // a state write while differentiating is control flow, not a value – returned
    // as an Error, cond/cascade would read it as "candidate declines"
    if (e instanceof StateWrite) { throw e }
    // inject cause to the caught error from argument errors
    if (e instanceof Error && errorArgs.length > 0) {
      return new Error(e.message, { cause: errorArgs[0] })
    } else {
      // @ts-ignore
      return e
    }
  }
}

// MARK: Choosing
// Errors are values: an Error is a result that says "there is no answer here".
// One kind of Error is polite – a NoMatch says "not my case, ask someone else".
//
// A CANDIDATE is a function that answers or declines; it declines by returning
// a NoMatch. guard(c, { v }) declines unless c holds; try({ v }) declines when
// v fails. cascade and cond take the first candidate that answers. A failure
// IS an answer: it stops the choice and surfaces, so a matched branch that
// breaks is never reread as "not me" while a later branch quietly wins. When
// every candidate declines, the choice itself declines – so choices nest, and
// an overload set is a candidate in a bigger one.
class NoMatch extends Error {
  constructor(message: string, options?: { cause?: unknown }) {
    super(message, options)
    this.name = "NoMatch"
  }
}

// The NoMatch a hand-written candidate returns to decline.
const NoMatchValue = new NoMatch("no match")

// Run the candidates in order and return the first answer. A candidate that
// isn't a function is its own result – the same constant-lift the hooks and
// forks give a non-function operand – so an Error among the candidates (a guard
// whose condition broke) surfaces, and a NoMatch declines. A throw is a failure
// like any other: a native candidate declines by RETURNING a NoMatch. A state write
// while differentiating is control flow and must reach ∇.
const chooseFirst = (candidates: Value[], args: Value[], scope: CurrentScope, exhausted: (lastDecline: NoMatch | null) => NoMatch): Value => {
  // the last decline – shown as the cause when nothing answers, so the
  // message still says WHY the closest candidate said no
  let lastDecline: NoMatch | null = null
  for (const candidate of candidates) {
    let result: Value
    try {
      result = typeof candidate === "function" ? candidate.apply(scope, args) : candidate
    } catch (e) {
      if (e instanceof StateWrite) { throw e }
      return e instanceof Error ? e : new Error(String(e))
    }
    if (result instanceof NoMatch) { lastDecline = result; continue }
    return result
  }
  return exhausted(lastDecline)
}

const FunctionCascade = (candidates: Value[]) => {
  // `cascade(f)` instead of `cascade((f, g))` used to leave the loop with
  // nothing to iterate and hand back a silent ◌ — the doc card's own shape is
  // a list, so say what was expected
  if (!Array.isArray(candidates)) {
    return new Error(`\`cascade\`: expected a list of candidates, got ${describeArg(candidates as Value)} – write cascade((f, g, …))`)
  }
  return cascadeOver(candidates)
}

const cascadeOver = (candidates: Value[]) => tacitToString(function (this: CurrentScope, ...args: Value[]) {
  return chooseFirst(candidates, args, this, (lastDecline) => {
    const shown = args.length === 0 ? "no arguments" : args.map(describeArg).join(", ")
    return new NoMatch(`no overload matched (${shown})`, lastDecline ? { cause: lastDecline } : undefined)
  })
})

// Eager, variadic cascade: `cond(a, b, …)` runs each candidate in order and
// returns the first answer – `cascade((…))()` without the wrapper. Pairs with
// guard: `cond(guard(n = 0, { 1 }), { n × fact(n - 1) })`. Its own entry point
// (not a rebuilt FunctionCascade dispatcher) so recursion doesn't pay a
// per-call setup cost.
const FunctionCond = function (this: CurrentScope, ...candidates: Value[]): Value {
  return chooseFirst(candidates, [], this, (lastDecline) =>
    new NoMatch("cond: no branch matched", lastDecline ? { cause: lastDecline } : undefined))
}
setMeta(FunctionCond, { noAutoLift: true })

// reduce's fast path: a raw binary op → its native tensor reduction. Filled in
// once the reductions are defined below; FunctionArity tags its dispatchers too,
// so `reduce +` and `reduce ⌈` fold via engine.sum/engine.max instead of slice-by-slice.
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
  const dispatch = tacitToString((a: Value, b: Value) => (b == null ? unary?.(a) : binary?.(a, b)))
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
  // takes the fast path (e.g. `reduce +` → engine.sum, `reduce ⌈` → engine.max)
  const nativeReduce = typeof binary === "function" ? reducerFor.get(binary) : undefined
  if (nativeReduce) { nativeReducer.set(dispatch as Function, nativeReduce) }
  return dispatch
}

// A tensor's data as nested JS arrays (a number for a scalar), for control flow and
// structural arguments – the engine computes it if it isn't yet
function getAsSyncList(value: unknown) {
  if (value instanceof FluentVariable) { value = value.current }
  if (value instanceof engine.Tensor) { return value.js() }
}

// MARK: Environment

const evaluateProgramWithScope = (program: string, scope: CurrentScope) => {
  const syntaxTree = CodeParse(program)

  if (syntaxTree.type === "Error") {
    return new Error(syntaxTree.content);
  }

  return evaluateSyntaxTreeNode(syntaxTree, scope)
}

const CodeEvaluate = function (this: CurrentScope, program: string) {
  return evaluateProgramWithScope(program, this);
}

// The loop under ⍣. FunctionRepeat(f, n) is f applied n times; with a
// predicate p in place of the count, f runs until p(next, previous) holds;
// with a negative count, invert(f) runs |n| times – the prelude passes its
// Newton inverse, so this loop never decides how inversion is done. A dyadic
// call x (f ⍣ n) y iterates f(x, ·) from y, as in APL.
const REPEAT_UNTIL_BUDGET = 10000
const FunctionRepeat = function (this: CurrentScope, fn: Value, n: Value, invert?: Value) {
  if (typeof fn !== "function") {
    return new Error("`(f ⍣ n)`: `f` must be a function")
  }
  const until = typeof n === "function" ? n : null
  let times = 0
  if (!until) {
    if (!isTensor(n)) {
      return new Error("`(f ⍣ n)`: `n` must be a scalar count or a predicate { new, old | … }")
    }
    const count = getAsSyncList(n)
    if (typeof count !== "number" || !Number.isFinite(count)) {
      // a non-scalar (or NaN) count would silently skip the loop and return the
      // identity function – surface it instead
      return new Error("`(f ⍣ n)`: `n` must be a finite scalar, got " +
        (Array.isArray(count) ? `a tensor of shape [${(n as engine.Tensor).shape.join(" ")}]` : String(count)))
    }
    // a live slider drives counts through fractional values – round, like linspace
    times = Math.round(count)
    if (times < 0 && typeof invert !== "function") {
      return new Error("`FunctionRepeat(f, n, invert)`: a negative count runs f backwards, which needs `invert`")
    }
  }
  const scope = this

  return function (...args: Value[]) {
    const dyadic = args.length >= 2
    const left = args[0]
    const forward = (z: Value) => safeApply(fn, dyadic ? [left, z] : [z], scope)
    let step: Function = forward
    if (times < 0) {
      const inverse = safeApply(invert as Function, [forward], scope)
      if (typeof inverse !== "function") { return inverse }
      step = inverse
    }
    let value: Value = (dyadic ? args[1] : args[0]) ?? null
    for (let i = 0; until ? true : i < Math.abs(times); i++) {
      if (until && i >= REPEAT_UNTIL_BUDGET) {
        value = new Error(`\`(f ⍣ p)\`: the predicate didn't hold within ${REPEAT_UNTIL_BUDGET} steps`)
        break
      }
      const prev = value
      value = step(prev)
      // a failed step is the answer – iterating an Error only repeats it
      if (value instanceof Error) { break }
      if (until) {
        try {
          const verdict = safeApply(until, [value, prev], scope)
          if (verdict instanceof Error) { value = verdict; break }
          if (isTruthy(requireData(verdict))) { break }
        } catch (e) {
          if (e instanceof StateWrite) { throw e }
          value = e instanceof Error ? e : new Error(String(e))
          break
        }
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
const FunctionThen = (f: Value, g: Value) => {
  if (!isFn(f) || !isFn(g)) { return new Error("`(f ∘ g)`: both operands must be functions") }
  return tacitToString(function (this: CurrentScope, ...args: Value[]) {
    return safeApply(g, [safeApply(f, args, this)], this)
  })
}

// C/W (Cardinal/Warbler) – commute: flip a binary's arguments; duplicate a unary's
const FunctionCommute = (f: Value) => {
  if (!isFn(f)) { return new Error("`(⍨ f)`: the operand must be a function") }
  return tacitToString(function (this: CurrentScope, a: Value, b: Value) {
    return b == null ? safeApply(f, [a, a], this) : safeApply(f, [b, a], this)
  })
}

// Ψ (Psi) – over: preprocess every argument with g, then combine with f.
// The preprocessor is read first because it runs first: (g ⍥ f)(x, y) =
// f(g(x), g(y)) – APL spells this f⍥g, but Fluent reads left-to-right.
const FunctionInto = (g: Value, f: Value) => {
  if (!isFn(f) || !isFn(g)) { return new Error("`(g ⍥ f)`: both operands must be functions") }
  return tacitToString(function (this: CurrentScope, ...args: Value[]) {
    return safeApply(f, args.map((a) => safeApply(g, [a], this)), this)
  })
}

// Φ/Φ₁ (Phoenix/Pheasant) – fork: outer tines f and h see the arguments,
// the middle tine g combines their results – the train reading order
const FunctionFork = (f: Value, g: Value, h: Value) => {
  if (!isFn(g)) { return new Error("`Φ(f, g, h)`: the middle tine `g` must be a function") }
  if (f == null || h == null) { return new Error("`Φ(f, g, h)`: takes three tines") }
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
    return safeApply(g, [safeApply(fc, [a], this), b == null ? a : b], this)
  })
}

// S/D (Starling/Dove) – after: preprocess the right (or only) argument with g,
// then combine with f; the monadic case is the J hook
const FunctionAfter = (f: Value, g: Value) => {
  if (!isFn(f)) { return new Error("`(f ⟜ g)`: `f` must be a function") }
  const gc = constantly(g)
  return tacitToString(function (this: CurrentScope, a: Value, b: Value) {
    return safeApply(f, [a, safeApply(gc, [b == null ? a : b], this)], this)
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
const FunctionIs = (v: Value) => engine.tensor(isFn(v) ? 1 : 0)
// what kind of value – 1 or 0, like a comparison – for the prelude's overloads
const TensorIs = (v: Value) => engine.tensor(isTensor(v) || v instanceof FluentVariable ? 1 : 0)
const ListIs = (v: Value) => engine.tensor(Array.isArray(v) ? 1 : 0)
const StringIs = (v: Value) => engine.tensor(typeof v === "string" || v instanceof String ? 1 : 0)
const ErrorIs = (v: Value) => engine.tensor(v instanceof Error ? 1 : 0)
// the same value – one variable, one signal, one function – not two equal ones
const Identical = (a: Value, b: Value) => engine.tensor(a === b ? 1 : 0)

// Async repeat (⟳): runs fn between frames so the UI stays live. The loop
// belongs to the evaluation that started it – a re-evaluation cancels it.
// (For synchronous function iteration f(f(f(x))) use ⍣ / FunctionPower.)
const FunctionIterate = (fn: (index?: engine.Tensor) => void, iterations?: Value) => {
  const live = iterations instanceof Signal || iterations instanceof FluentVariable
  if (!(typeof fn === "function" && (iterations == null || isTensor(iterations) || live))) {
    throw new Error("`FunctionIterate(fn, iterations)`: `fn` must be a function and `iterations` must be a scalar Tensor");
  }

  // The count is a BUDGET: `f ⟳ n` runs f n times in total. A live count is the
  // same budget, read fresh – raise the slider and the loop runs on to the new
  // total, lower it past what has already run and it stops. A constant signal
  // therefore behaves exactly like the literal, which is the property that
  // makes the two spellings mean one thing.
  const readTarget = (): number => {
    if (iterations == null) { return 1 }
    const inner = iterations instanceof Signal ? iterations.value
      : iterations instanceof FluentVariable ? (iterations.version.value, iterations.current)
      : iterations
    const n = getAsSyncList(inner)
    // a non-scalar count would make `i >= target` always false – an accidental
    // infinite loop instead of an error
    if (typeof n !== "number" || Number.isNaN(n)) {
      throw new Error("`FunctionIterate(fn, iterations)`: `iterations` must be a scalar Tensor");
    }
    return n
  }
  // a bad count is an error at the call, not silently zero frames. Read untracked: the
  // loop follows a live count itself, and a program that read it would re-run – and
  // restart its loop – every time the count changed
  untracked(readTarget)

  const generation = currentGeneration
  let i = 0
  let scheduled = false
  let cancelled = false
  registerDisposable(() => { cancelled = true })

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

  // Between steps, hand the thread to pending input and rendering and then
  // continue AHEAD of other queued work (scheduler.yield's front-of-queue
  // semantics) – typing and scrolling stay alive inside a heavy batch
  // instead of waiting for it. Chrome-only for now; elsewhere the batch
  // runs unyielded, paced by rAF as before.
  const schedulerYield: (() => Promise<void>) | null =
    typeof (globalThis as { scheduler?: { yield?: () => Promise<void> } }).scheduler?.yield === "function"
      ? () => (globalThis as unknown as { scheduler: { yield: () => Promise<void> } }).scheduler.yield()
      : null

  let batch = 1
  let prev = 0

  const frame = async (t: number) => {
    scheduled = false
    if (generation !== currentGeneration || cancelled) { return }
    // idle, not finished: a live budget may grow again and the effect below
    // re-arms the pump when it does
    const maxIterations = readTarget()
    if (i >= maxIterations) { return }

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
    // the step budget is spent. Cheap ticks (a paused, checkbox-gated loop)
    // grow the batch toward the cap – without this, the first frame after
    // resuming would run the whole grown batch of now-expensive steps and
    // freeze the tab for seconds. The budget counts OUR time only: yielded
    // time belongs to input and rendering and must not starve the batch.
    const now = () => (globalThis as any).performance?.now?.() ?? 0
    let stepTime = 0
    const end = Math.min(i + batch, maxIterations)
    while (i < end) {
      // at most two steps on the GPU: the screen's frame waits behind whatever is queued
      await engine.gpuDrained(2)
      if (generation !== currentGeneration || cancelled) { return }
      const stepStart = now()
      fn(engine.scalar(i))
      i++
      stepTime += now() - stepStart
      if (stepTime > 25) { break }
      if (schedulerYield && i < end) {
        await schedulerYield()
        // the world may have moved on while we yielded
        if (generation !== currentGeneration) { return }
      }
    }

    schedule()
  }

  // one pending frame at a time: without this, every wake-up would start a
  // second pump in the same generation and the tick rate would multiply
  function schedule() {
    if (scheduled || cancelled) { return }
    scheduled = true
    raf(frame)
  }

  // a live budget re-arms the pump when it changes – including after the loop
  // has gone idle at its old total
  if (live) { registerDisposable(effect(() => { try { readTarget() } catch { return } schedule() })) }
  schedule()

  return null
}
// ⟳ drives its own loop and reads its own count, so a signal argument must not
// wrap the call in a lazy computed that nothing subscribes to – that ran the
// loop zero times, silently.
setMeta(FunctionIterate, { noAutoLift: true })

// Fresh scalar for time-varying sources (Time, sliders, scrubbers): held as data, never
// folded into a kernel – the clock ticking or a slider moving reuses the compiled plan
const TensorScalarLive = (value: number): engine.Tensor => engine.scalar(value)

// A signal holds data: a tensor stored in it – or in a list in it – is settled into a
// leaf, so a value written back every tick (⟳ with ←) never drags the previous ticks along
const heldValue = (v: unknown): unknown =>
  Array.isArray(v) ? v.map((e) => heldValue(e)) : v instanceof engine.Tensor ? engine.settle(v) : v

const SignalCreate = (<T,>(initial: T) => signal(heldValue(initial))) as typeof signal

const SignalRead = <T,>(s: Signal<T>) => {
  if (!(s instanceof Signal)) {
    return new Error(`'SignalRead': ${String(s)} is not a signal`)
  }
  return s.value
}

const SignalUpdate = <T,>(s: Signal<T>, v: T) => {
  if (s instanceof Signal) {
    if (differentiating) { throw new StateWrite("a signal write") }
    s.value = heldValue(v) as T
    return
  }

  return new Error(`'SignalUpdate': ${s} is not a signal`)
}

const SignalComputed = computed

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
  if (s instanceof Signal) { return s.peek() }
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

const FunctionApply = function (this: CurrentScope, fn: Value, args: Value) {
  // a bare argument applies directly – `f @ 5` is f(5); a list spreads
  const list = args instanceof Array ? args : [args]
  return safeApply(fn, list, this)
}

const FunctionPipe = function (this: CurrentScope, a: Value, b: Value): Value {
  const arg = a instanceof Array ? a : [a]
  return safeApply(b, arg, this)
}

const FunctionNoAutoLift = (fn: Function) => {
  setMeta(fn, { noAutoLift: true })
  return fn
}
setMeta(FunctionNoAutoLift, { noAutoLift: true })

// Fluent's truth: a number holds when it's nonzero, a tensor when any element
// is (an all-zero tensor doesn't), and NaN never does. guard, when and the
// until-form of ⍣ all ask this one question.
const isTruthy = (data: number | unknown[]): boolean => {
  const falsy = (v: unknown) => v === 0 || v === false
  if (Array.isArray(data)) { return !(data.flat(Infinity) as unknown[]).every(falsy) }
  return !(falsy(data) || Number.isNaN(data))
}

// guard(cond, { value }) – a candidate that answers with value while cond holds
// and declines otherwise. The condition is read strictly, at the guard: a
// typo'd name or an Error there is a failure, not a quiet "no" – the guard
// itself becomes the Error, and a cascade surfaces it instead of skipping.
// A body that isn't a thunk is its own value.
const FunctionGuard = function (this: CurrentScope, cond: Value, body: Value) {
  const holds = isTruthy(requireData(cond))
  const scope = this
  const candidate = function () {
    if (!holds) {
      const decline = new NoMatch("guard: the condition doesn't hold")
      // the guard's own call site, so hovering the decline finds it
      const origin = getOrigin(candidate)
      if (origin) { setOrigin(decline, origin) }
      return decline
    }
    return typeof body === "function" ? body.apply(scope) : body
  }
  return candidate
}
setMeta(FunctionGuard, { noAutoLift: true })

// try({ value }) – a candidate that declines when value FAILS: the failure
// becomes a NoMatch (kept as its cause), so a cascade moves on. The explicit
// way to say "a failure here just means: not this one". A candidate's
// arguments pass through, so try(f) joins an overload set as f would.
const FunctionTry = function (this: CurrentScope, body: Value) {
  const scope = this
  return function (...args: Value[]) {
    let result: Value
    try {
      result = typeof body === "function" ? body.apply(scope, args) : body
    } catch (e) {
      if (e instanceof StateWrite) { throw e }
      result = e instanceof Error ? e : new Error(String(e))
    }
    return result instanceof Error && !(result instanceof NoMatch)
      ? new NoMatch("try: it failed", { cause: result })
      : result
  }
}
setMeta(FunctionTry, { noAutoLift: true })

// when(cond, { … }) – a conditional effect: run the thunk while cond is
// truthy, yield ◌ otherwise. Not a candidate: whatever the thunk returns –
// an Error included – is the result, so a broken training step stays loud.
const FunctionWhen = function (this: CurrentScope, cond: Value, thunk: Value) {
  if (!isTruthy(requireData(cond))) { return null }
  if (typeof thunk !== "function") {
    return new Error("`when(cond, { … })`: second argument must be a thunk")
  }
  return (thunk as Function).apply(this)
}
setMeta(FunctionWhen, { noAutoLift: true })

// Error(message, cause?) – a failure as a value: no answer, and why. The cause
// can be any value: the Error this one grew from, or what was tried.
const ErrorCreate = (message: Value, cause?: Value): Error =>
  new Error(
    typeof message === "string" || message instanceof String ? String(message) : StringLiteral(message),
    cause == null ? undefined : { cause })

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
  // a JS string also has `.length`, so without this guard the Length cascade
  // answered for strings here — in UTF-16 units — before ever reaching
  // StringLength, and disagreed with StringToCodes about how long one is
  if (!Array.isArray(a)) { return new NoMatch(`expected a list, got ${describeArg(a as Value)}`) }
  return engine.tensor(a.length)
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

const ListMap = function (this: CurrentScope, a: any[], fn: (value: any, index: Value) => any) {
  if (typeof fn !== "function") {
    return new Error("'ListMap': 'fn' must be a function")
  }

  if (!Array.isArray(a)) {
    return new Error("'ListMap': 'a' must be a list")
  }

  return a.map((value, index) => safeApply(fn, [value, TensorScalar(index)], this))
}

const ListReduce = (a: any[], fn: (acc: any, value: any) => any, initialValue?: any) => {
  if (typeof fn !== "function") {
    return new Error("'ListReduce': `fn` must be a function")
  }

  if (initialValue == null) {
    return a.reduce(fn)
  }

  return a.reduce(fn, initialValue)
}

// ListReduce that keeps every step: (init, f(init, a₀), f(f(init, a₀), a₁), …)
const ListScan = (a: any[], fn: (acc: any, value: any) => any, init: any) => {
  if (!Array.isArray(a)) {
    return new Error("'ListScan': `list` must be a list")
  }
  if (typeof fn !== "function") {
    return new Error("'ListScan': `fn` must be a function")
  }
  const out = [init]
  for (const v of a) { out.push(fn(out[out.length - 1], v)) }
  return out
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
    : v instanceof engine.Tensor ? (v.shape.length === 0 ? "a number" : `a tensor [${v.shape.join(" ")}]`)
    : v instanceof FluentVariable ? "a trainable variable"
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

// Sizes, counts, axes and repeat factors are whole numbers, but Fluent has one
// dtype and the README teaches scaling a 0..1 slider by arithmetic, so they
// arrive fractional constantly. Round, the policy linspace and ⍣ already state
// in their own comments – the alternative is a tensor with a fractional shape,
// which fails only when it is computed: at read time, outside the evaluator's
// try, so it would take down the panel instead of becoming an Error.
// ±Infinity is not a length; asNumber already rejects NaN.
const asFinite = (v: Value): number => {
  const n = asNumber(v)
  if (!Number.isFinite(n)) { throw new Error(`expected a whole number, got ${n}`) }
  return n
}
const asWhole = (v: Value): number => Math.round(asFinite(v))
const asWholeList = (v: Value): number[] => asNumberList(v).map((n) => {
  if (!Number.isFinite(n)) { throw new Error(`expected whole numbers, got ${n}`) }
  return Math.round(n)
})

const TensorScalar = (value: number) => engine.tensor(value)

// A numeric operand: a string ("a" + "b") or a function is a user mistake here, so
// name the type instead of an opaque engine message. operand itself stays general – a
// signal may hold a string.
const numericArg = (v: Value): any => {
  if (typeof v === "string" || v instanceof String) {
    throw new Error(`a string (${JSON.stringify(String(v))}) is not a tensor – numeric ops like + and × need tensors; use StringConcat to join text`)
  }
  if (typeof v === "function") {
    throw new Error(`a function is not a tensor – numeric ops like + and × need tensors`)
  }
  return operand(v)
}

const unaryOp = (op: (x: any) => engine.Tensor) => (a: Value) => op(numericArg(a))
const binaryOp = (op: (x: any, y: any) => engine.Tensor) => (a: Value, b: Value) => op(numericArg(a), numericArg(b))

// stack broadcasts its inputs to a common shape, like arithmetic does:
// stack(x - a, y - b) works even when the pieces only meet by broadcasting.
const broadcastRefs = (values: Value[]): engine.Tensor[] => {
  const tensors = values.map(v => v instanceof FluentVariable ? v.current : v)
  if (tensors.length === 0 || !tensors.every(isTensor)) { return tensors.map(operand) as engine.Tensor[] }
  const arrays = tensors as engine.Tensor[]
  const rank = Math.max(...arrays.map(t => t.shape.length))
  const shape = Array.from({ length: rank }, (_, i) =>
    Math.max(...arrays.map(t => t.shape[t.shape.length - rank + i] ?? 1)))
  return arrays.map(t =>
    t.shape.length === rank && t.shape.every((d, i) => d === shape[i]) ? t : engine.broadcastTo(t, shape))
}

// stack(a, b, ...) joins along axis 0; stack((a, b, ...), axis) picks the axis
const TensorStack = (...args: unknown[]) => {
  if (Array.isArray(args[0])) {
    const axis = args[1] == null ? 0 : asNumber(args[1] as Value)
    return engine.stack(broadcastRefs(args[0] as Value[]), axis)
  }
  return engine.stack(broadcastRefs(args as Value[]))
}
const TensorUnstack = (a: Value, b?: Value) => {
  const axis = b == null ? 0 : asNumber(b)
  const array = operand(a) as engine.Tensor
  // the iterator yields slices along the first axis
  return [...(axis === 0 ? array : engine.moveaxis(array, axis, 0))]
}
const TensorConcat = (...args: unknown[]) => {
  const listed = Array.isArray(args[0])
  const axis = listed && args[1] != null ? asNumber(args[1] as Value) : 0
  const xs = ((listed ? args[0] : args) as Value[]).map(operand) as engine.Tensor[]
  return engine.concatenate(xs, axis)
}




const TensorAdd = binaryOp(engine.add)
const TensorMultiply = binaryOp(engine.multiply)

// a literal integer exponent is exponentiation by squaring in the engine – exact
// where exp(e·ln x) is not (3^2 is 9, (-2)^2 is 4), and x^2 differentiates as x·x
const TensorPower = binaryOp(engine.power)
const TensorMaximum = binaryOp(engine.maximum)
const TensorMinimum = binaryOp(engine.minimum)

// Comparisons yield float32 0/1, APL-style: Fluent is a float32 language, and 0/1
// composes – `(x = 1) - y` is arithmetic, and prints as numbers, not booleans.
const comparisonOp = (op: (x: any, y: any) => engine.Tensor) =>
  (a: Value, b: Value) => {
    const x = numericArg(a), y = numericArg(b)
    return op(x, y)
  }

const TensorLess = comparisonOp(engine.less)
const TensorEqual = comparisonOp(engine.equal)

const TensorSine = unaryOp(engine.sin)
const TensorCosine = unaryOp(engine.cos)
const TensorTangentHyperbolic = unaryOp(engine.tanh)
const TensorSineInverse = unaryOp(engine.asin)
const TensorCosineInverse = unaryOp(engine.acos)
const TensorTangentInverse = unaryOp(engine.atan)
const TensorSineHyperbolicInverse = unaryOp(engine.asinh)
const TensorCosineHyperbolicInverse = unaryOp(engine.acosh)
const TensorTangentHyperbolicInverse = unaryOp(engine.atanh)
const TensorArcTangent2 = binaryOp(engine.atan2)  // arctan2(y, x): angle of (x, y)

// Reductions take an optional axis (scalar or vector tensor) as second
// argument, and a truthy third argument keeps the reduced axis as size 1 –
// mean(x, -1, 1) is the trace-safe unsqueeze(mean(x, -1), …)
const withOptionalAxis = (op: (a: any, axis?: number | number[] | null, opts?: { keepdims?: boolean }) => engine.Tensor) =>
  (a: Value, b?: Value, keep?: Value) => {
    const axis = b == null ? null : (Array.isArray(requireData(b)) ? asWholeList(b) : asWhole(b))
    const opts = keep == null ? undefined : { keepdims: asNumber(keep) !== 0 }
    const x = operand(a)
    return op(x, axis, opts)
  }

const TensorSum = withOptionalAxis(engine.sum)      // TensorReduce(a, +)
const TensorProduct = withOptionalAxis(engine.prod) // TensorReduce(a, ×)
const TensorMin = withOptionalAxis(engine.min)
const TensorMax = withOptionalAxis(engine.max)

// The reductions above ARE folds; register the fast native path so that
// `x reduce +` dispatches to engine.sum rather than folding slice by slice.
reducerFor.set(TensorAdd, TensorSum)
reducerFor.set(TensorMultiply, TensorProduct)
reducerFor.set(TensorMaximum, TensorMax)
reducerFor.set(TensorMinimum, TensorMin)

// axis 0 as a reusable value – the default a fold collapses
const LEADING_AXIS: Value = engine.tensor(0)

// Fold a tensor left-to-right with a binary fn, optionally along one axis:
// `1 ..< 10 reduce +` = 45, `x reduce { a, b | a - b }` = ((x_0-x_1)-x_2)-…
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
  if (fast) { return fast(tensor, axis == null ? LEADING_AXIS : axis) }
  const parts = TensorUnstack(tensor, axis) as unknown[]
  if (parts.length === 0) {
    // a known op has an identity (Σ[] = 0); a custom fn's is unknown, so an
    // empty fold has no answer to hand back
    return new Error("`reduce`: cannot fold an empty tensor with a custom function")
  }
  return ListReduce(parts, (acc: Value, v: Value) => (fn as Function)(acc, v))
}


const TensorAbsolute = unaryOp(engine.abs)
const TensorSign = unaryOp(engine.sign)
// log(x) is the natural logarithm; log(x, b) takes the base b, so 8 log 2 = 3
const TensorNaturalLogarithm = unaryOp(engine.log)
const TensorLog1Plus = unaryOp(engine.log1p)      // log(1 + x), accurate near 0
const TensorExponential = unaryOp(engine.exp)
const TensorExpMinus1 = unaryOp(engine.expm1)     // exp(x) - 1, accurate near 0
const TensorReciprocal = unaryOp(engine.reciprocal)
const TensorDivide = binaryOp(engine.divide)
const TensorSquareRoot = unaryOp(engine.sqrt)
const TensorCubeRoot = unaryOp(engine.cbrt)
const TensorRound = unaryOp(engine.round)
const TensorFloor = unaryOp(engine.floor)
// oneHot of an index outside 0..<depth is an all-zero row: an off-by-one label
// column, or a -1 "unknown" sentinel, would silently drop out of the loss. Bounds-check
// the way `_` does one screen down.
// Index values for a bounds check: read when they are on the host or cheap to compute
// there; values still on the GPU – a minibatch drawn from a data slot – flow through
// unchecked as trusted data
const cheapValues = (v: Value): Float32Array | undefined => {
  const t = v instanceof FluentVariable ? v.current : v
  return isTensor(t) ? engine.hostValue(t as engine.Tensor) : undefined
}
const TensorOneHot = (indices: Value, depth: Value) => {
  const n = asWhole(depth)
  const flat = cheapValues(indices)
  if (flat !== undefined) {
    const bad = flat.find((i) => !(i >= 0 && i < n))
    if (bad !== undefined) {
      return new Error(`oneHot: class ${bad} is out of bounds for a depth of ${n}`)
    }
  }
  return engine.oneHot(engine.trunc(operand(indices)), n)
}

const TensorSort = (x: Value) =>
  isScalarTensor(x) ? engine.reshape(operand(x), []) : engine.sort(operand(x))
const TensorArgSort = (x: Value) =>
  isScalarTensor(x)
    ? engine.reshape(engine.argsort(engine.reshape(operand(x), [1])), [])
    : engine.argsort(operand(x))
const TensorSlice = (a: Value, begin: Value, size?: Value) => {
  const beginList = asNumberList(begin)
  const sizeList = size == null ? undefined : asNumberList(size)
  const spec = beginList.map((b, i) => {
    const s = sizeList?.[i]
    return (s === undefined || s === -1 ? [b] : [b, b + s]) as [number] | [number, number]
  })
  return (operand(a) as engine.Tensor).slice(...spec)
}

const TensorGradient = (f: Value) => {
  if (typeof f !== "function") {
    return new Error("`grad(f)`: `f` must be a function")
  }
  // vjp with a ones cotangent: scalar outputs give the classic gradient, elementwise
  // outputs per-element derivatives – ∇({x | x^2}) works on vectors
  return (x: Value) => {
    if (x == null) { return gradientOfReads(f as Function) }
    const outer = differentiating
    differentiating = true
    try {
      const [out, pullback] = engine.vjp((traced: engine.Tensor) => {
        const result = (f as Function)(traced)
        const value = result instanceof Signal ? result.peek() : result
        if (value instanceof Error) { throw value }
        if (!isTensor(value)) { throw new Error("`grad(f)`: `f` must return a tensor") }
        return value as engine.Tensor
      }, [operand(x) as engine.Tensor])
      return pullback(engine.full(out.shape, 1))[0]!
    } catch (e) {
      if (e instanceof StateWrite) { return new Error(`\`grad(f)\`: \`f\` cannot change state while being differentiated (${e.message})`) }
      throw e
    } finally {
      differentiating = outer
    }
  }
}
// ∇(loss)(): with nothing to differentiate with respect to, the derivative is with respect
// to the variables the loss reads – found by reading them, as a computed signal finds its
// signals. It comes back with the value, all computed in one pass: (value, variables,
// gradients). An enclosing ∇ still differentiates through it.
const gradientOfReads = (loss: Function): Value => {
  const reads = new Map<FluentVariable, engine.Tensor>()
  const outer = readTracker, outerDifferentiating = differentiating
  readTracker = (v, value) => {
    let t = reads.get(v)
    if (!t) { t = engine.traced(outer ? outer(v, value) : value); reads.set(v, t) }
    return t
  }
  differentiating = true
  let value: Value
  try {
    const result = loss()
    value = result instanceof Signal ? result.peek() : result
  } catch (e) {
    if (e instanceof StateWrite) { return new Error(`\`∇(loss)()\`: the loss cannot change state while it is differentiated (${e.message})`) }
    throw e
  } finally {
    readTracker = outer
    differentiating = outerDifferentiating
  }
  if (value instanceof Error) { return value }
  if (!isTensor(value)) { return new Error("`∇(loss)()`: the loss must be a tensor") }
  const variables = [...reads.keys()], grads = engine.gradients(value as engine.Tensor, [...reads.values()])
  engine.materialize([value as engine.Tensor, ...grads])
  return [engine.settle(value as engine.Tensor), variables, grads.map(engine.settle)]
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
const TensorRange = (a: Value, b?: Value) => {
  // arange tags its result int32 – Fluent tensors are float32 throughout
  if (b == null) {
    return engine.arange(Math.trunc(asFinite(a)))
  }

  const start = Math.trunc(asFinite(a))
  const stop = Math.trunc(asFinite(b))
  const step = start <= stop ? 1 : -1
  return engine.arange(start, stop, step)
}



// Rank-polymorphic convolution: the kernel's rank sets the conv's rank, so the
// same conv slides a 1-D kernel over a vector or a 2-D kernel over an image,
// zero padded so the output keeps the input's shape.
const TensorConvolution = (arr: Value, kernel: Value) => engine.conv(operand(arr), operand(kernel))


// Coordinate grids: meshgrid(0 ..< w, 0 ..< h) → [X, Y], each broadcast to the full grid
const TensorMeshgrid = (...args: Value[]) => engine.meshgrid(args.map(operand))




// The k largest values and their indices, as [values, indices]
const TensorTopK = (a: Value, k: Value) => {
  const [values, indices] = engine.topK(operand(a), asWhole(k))
  return [values, indices]
}


const TensorReshape = (a: Value, b?: Value) => {
  if (b != null) {
    return engine.reshape(operand(a), asNumberList(b))
  }

  return engine.tensor(shapeOf(a))
}




// The contractions stay native until shape logic written in Fluent is cheap: their
// Fluent versions spend most of a training step's host time reading shapes
const TensorMatrixMultiply = binaryOp(engine.matmul)
const TensorDotProduct = binaryOp(engine.dot)
// Einstein summation: einsum("ij,jk->ik", a, b) is a matrix multiply
const TensorEinsum = (subscripts: Value, ...args: Value[]) =>
  engine.einsum(String(subscripts), ...args.map(operand))

const TensorLength = (a: Value, b?: Value) => {
  // a list or a string is another candidate's case – decline it
  if (!isTensor(a) && !(a instanceof FluentVariable)) { return new NoMatch(`expected a tensor, got ${describeArg(a)}`) }
  // A missing axis has size 1, never NaN: a scalar (shape []) is one item along
  // its phantom leading axis, so `#(5)` is 1 (APL's tally ≢), matching how
  // broadcasting treats absent axes as size 1. An empty tensor (shape [0]) stays 0.
  if (b != null) {
    // negative axes count from the end, as they do for Σ, sort and moveaxis
    const shape = shapeOf(a)
    const axis = asWhole(b)
    return engine.tensor(shape[axis < 0 ? axis + shape.length : axis] ?? 1)
  }

  return engine.tensor(shapeOf(a)[0] ?? 1)
}

const TensorShape = (a: Value) =>
  isTensor(a) || a instanceof FluentVariable ? engine.tensor(shapeOf(a)) : new Error(`shape: expected a tensor, got ${describeArg(a)}`)

// `_` reads its index arg as a scalar (`_0`) or a list/tensor of indices (`_[0, 1]`).
const readIndexList = (b: Value): { multi: boolean, idxs: number[] } => {
  if (Array.isArray(b)) return { multi: true, idxs: b.map(asNumber) }
  let d: unknown
  try { d = getAsSyncList(b) } catch { d = undefined }
  if (Array.isArray(d)) return { multi: true, idxs: (d as number[]).flat(Infinity) }
  return { multi: false, idxs: [asNumber(b)] }
}
const wrapIndex = (raw: number, n: number, kind: string): number | Error => {
  const i = Math.trunc(raw) < 0 ? Math.trunc(raw) + n : Math.trunc(raw)
  return !(i >= 0 && i < n) ? new Error(`index ${raw} is out of bounds for a ${kind} of length ${n}`) : i
}
// A string indexes to a string, a list to a list – `_[i, j]` keeps the container's
// type, `_i` picks one element. Same negative-from-the-end and bounds rules as tensors.
const stringGather = (s: string, b: Value): Value => {
  const { multi, idxs } = readIndexList(b)
  // index characters, not UTF-16 units – `"a🍌b"_1` used to return half of an
  // emoji, and the count disagreed with StringToCodes/#
  const source = [...s]
  const chars: string[] = []
  for (const raw of idxs) {
    const i = wrapIndex(raw, source.length, "string")
    if (i instanceof Error) { return i }
    chars.push(source[i]!)
  }
  return multi ? chars.join("") : chars[0]!
}
const listGather = (a: Value[], b: Value): Value => {
  const { multi, idxs } = readIndexList(b)
  const out: Value[] = []
  for (const raw of idxs) {
    const i = wrapIndex(raw, a.length, "list")
    if (i instanceof Error) { return i }
    out.push(a[i]!)
  }
  return multi ? (out as Value) : out[0]!
}

// `_` (TensorGather) is polymorphic over the container type: string / list / tensor.
const TensorGather = (a: Value, b: Value): Value => {
  if (typeof a === "string" || a instanceof String) { return stringGather(String(a), b) }
  if (Array.isArray(a)) { return listGather(a as Value[], b) }
  // a scalar is a rank-0 tensor with a single phantom item (#(5) = 1): 5_0 = 5
  return TensorTake(isScalarTensor(a) ? engine.reshape(operand(a), [1]) : a, b)
}
// take(x, indices): the rows of x at indices, any shape of them; a negative index counts
// from the end. Every index on the host or cheap to compute there is bounds-checked – a
// gather has no bounds mode, so an out-of-range index would read garbage; an index still
// on the GPU (an embedding lookup into a minibatch) is trusted data and flows through.
const TensorTake = (a: Value, b: Value): Value => {
  const x = operand(a) as engine.Tensor, size = x.shape[0] ?? 0
  const flat = cheapValues(b)
  if (flat !== undefined) {
    // `!(0 <= w < size)` also rejects a NaN index, which slips past `<`/`>=`
    const bad = flat.find((i) => { const w = i < 0 ? i + size : i; return !(w >= 0 && w < size) })
    if (bad !== undefined) {
      return new Error(`index ${bad} is out of bounds for an axis of length ${size}`)
    }
  }
  const raw = engine.trunc(operand(b))
  const wrapped = engine.where(engine.less(raw, 0), engine.add(raw, size), raw)
  return engine.take(x, wrapped, 0)
}
// scatterAdd(indices, values, size): a [size, …] of zeros with each row of values added at
// its index – take's transpose
const TensorScatterAdd = (indices: Value, values: Value, size: Value) =>
  engine.scatterAdd(operand(indices), operand(values), asWhole(size))
// detach(x): x, with no derivative – ∇ treats it as a constant
const TensorDetach = unaryOp(engine.detach)

const TensorWhere = (a: Value, b: Value, c: Value) =>
  engine.where(engine.truthy(operand(a)), operand(b), operand(c))
const TensorIsNaN = (a: Value) => engine.isnan(operand(a))

const TensorVariable = (a: Value) => {
  if (!isTensor(a) && !(a instanceof FluentVariable)) {
    return new Error("`~(init)`: initial value must be a tensor")
  }
  return new FluentVariable(operand(a) as engine.Tensor)
}

const TensorData = (a: Value) => {
  if (!isTensor(a) && !(a instanceof FluentVariable)) {
    return new Error("`~~(init)`: initial value must be a tensor – make one slot per tensor")
  }
  return new FluentData(operand(a) as engine.Tensor)
}

const TensorAssign = (a: Value, b: Value): Value => {
  // a list of variables takes a list of as many values, computed together
  if (Array.isArray(a)) {
    if (!Array.isArray(b) || b.length !== a.length) { return new Error("`:=`: a list of variables takes a list of as many values") }
    const bad = a.findIndex((v, i) => !(v instanceof FluentVariable) || !(isTensor(b[i]) || b[i] instanceof FluentVariable))
    if (bad >= 0) { return TensorAssign(a[bad] as Value, b[bad] as Value) }
    if (differentiating) { throw new StateWrite("a variable write") }
    const values = b.map(operand) as engine.Tensor[]
    engine.materialize(values)
    a.forEach((v, i) => { (v as FluentVariable).assign(values[i]!) })
    return null
  }
  if (!(a instanceof FluentVariable)) {
    return new Error("`:=`: left side must be a variable created with ~ or ~~")
  }
  if (!isTensor(b) && !(b instanceof FluentVariable)) {
    // assigning an Error (an emptied corpus, a failed expression) must not
    // corrupt the variable: it keeps its last good value, the Error surfaces
    return new Error("`:=`: value must be a tensor", b instanceof Error ? { cause: b } : undefined)
  }
  if (differentiating) { throw new StateWrite("a variable write") }
  a.assign(operand(b) as engine.Tensor)
  return null
}

// Bridge a trainable variable into the reactive world: watch(θ) is a Signal
// that updates on every assignment (drag, optimizer step, :=)
const TensorWatch = (a: Value): Value => {
  if (!(a instanceof FluentVariable)) { return a }
  return computed(() => {
    a.version.value
    return a.current
  })
}

// Stateful convenience RNG: a fresh key per call – deterministic per page load, not
// across reloads.
let rngCounter = 0
const TensorRandomUniform = (a: Value) => engine.uniform(rngCounter++, asNumberList(a))

// A value as the Fluent SOURCE that reproduces it, so CodeEvaluate(String(x))
// gives x back. This is repr, not display: a string comes back quoted, a list
// keeps its parens, a tensor keeps its rank. Joining with commas (what a JS
// string coercion does) is not just lossy — the text re-parses as a SEQUENCE,
// so `[1, 2, 3]` came back as 3.
//
// The law holds exactly for the values that are data: numbers, tensors of any
// rank, lists, strings, ◌. It cannot hold for values with identity — a signal
// or a `~` variable round-trips to a NEW one, equal but not the same — nor for
// a closure over names the target scope does not have. Those are best effort.
const numberLiteral = (n: number): string =>
  Number.isFinite(n) ? String(n)
    // no literal for these, but these expressions evaluate to them
    : Number.isNaN(n) ? "(0 ÷ 0)"
    : n > 0 ? "(1 ÷ 0)" : "(0 - 1 ÷ 0)"

const nestedLiteral = (d: unknown): string =>
  Array.isArray(d) ? `[${d.map(nestedLiteral).join(", ")}]` : numberLiteral(Number(d))

const StringLiteral = (v: Value): string => {
  if (v === null || v === undefined) { return "◌" }
  if (typeof v === "string" || v instanceof String) {
    return `"${String(v).replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`
  }
  if (typeof v === "symbol") { return Symbol.keyFor(v) ?? String(v) }
  if (v instanceof FluentVariable) { return `~(${StringLiteral(v.current)})` }
  if (v instanceof Signal) { return `$(${StringLiteral(v.peek() as Value)})` }
  if (isTensor(v)) { return nestedLiteral(getAsSyncList(v)) }
  if (Array.isArray(v)) {
    // a one-element list needs its trailing comma, or the parens are just grouping
    const parts = v.map((e) => StringLiteral(e as Value))
    return parts.length === 1 ? `(${parts[0]},)` : `(${parts.join(", ")})`
  }
  if (typeof v === "function") { return String(v) }
  return String(v)   // an Error has no literal form – show it rather than lie
}

const StringConcat = (...args: any[]) => "".concat(...args)
// characters, not UTF-16 units: `#` disagreed with StringToCodes about how
// long a string is (4 vs 3 for "a🍌b"), and `_` handed back lone surrogates
const StringLength = (a: Value) =>
  typeof a === "string" || a instanceof String
    ? engine.tensor([...String(a)].length)
    : new NoMatch(`expected a string, got ${describeArg(a)}`)

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
  return engine.tensor(Float32Array.from(String(a), (c) => c.codePointAt(0)!))
}

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
  t.read().then((codes) => { text.value = codesText(codes) }, (e) => { text.value = e instanceof Error ? e : new Error(String(e)) })
  return text
}

const Null = null


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
  FunctionRepeat,
  FunctionThen,
  FunctionCommute,
  FunctionInto,
  FunctionFork,
  FunctionBefore,
  FunctionAfter,
  FunctionRight,
  FunctionLeft,
  FunctionIs,
  TensorIs,
  ListIs,
  StringIs,
  ErrorIs,
  Identical,
  FunctionCascade,
  FunctionCond,
  FunctionArity,
  FunctionApply,
  FunctionPipe,
  FunctionNoAutoLift,
  FunctionGuard,
  FunctionTry,
  FunctionWhen,
  ErrorCreate,
  NoMatch: NoMatchValue,
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
  TensorStack,
  TensorUnstack,
  TensorConcat,

  TensorAdd,
  TensorMultiply,

  TensorPower,
  TensorMaximum,
  TensorMinimum,

  TensorLess,
  TensorEqual,
  TensorSine,
  TensorCosine,
  TensorSineInverse,
  TensorCosineInverse,
  TensorTangentInverse,
  TensorTangentHyperbolic,
  TensorSineHyperbolicInverse,
  TensorCosineHyperbolicInverse,
  TensorTangentHyperbolicInverse,
  TensorArcTangent2,

  TensorSum,
  TensorProduct,
  TensorMin,
  TensorMax,
  TensorReduce,

  TensorAbsolute,
  TensorSign,
  TensorLog1Plus,
  TensorNaturalLogarithm,
  TensorExponential,
  TensorExpMinus1,
  TensorReciprocal,
  TensorDivide,
  TensorSquareRoot,
  TensorCubeRoot,
  TensorRound,
  TensorFloor,
  TensorOneHot,
  TensorSort,
  TensorArgSort,

  TensorGradient,

  TensorPermute,
  TensorBroadcast,
  TensorPadAxis,
  TensorWindow,
  TensorTake,
  TensorScatterAdd,
  TensorDetach,
  TensorRange,
  TensorConvolution,
  TensorMeshgrid,
  TensorTopK,
  TensorReshape,
  TensorLength,
  Length,
  TensorShape,
  TensorGather,
  TensorWhere,
  TensorIsNaN,
  TensorSlice,

  TensorVariable,
  TensorData,
  TensorAssign,
  TensorWatch,
  TensorRandomUniform,

  TensorMatrixMultiply,
  TensorDotProduct,
  TensorEinsum,

  // List operations
  List,
  ListLength,
  ListConcat,
  ListGet,
  ListMap,
  ListReduce,
  ListScan,

  // String operations
  StringConcat,
  StringLiteral,
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
// trees; headless environments get the raw tree node. Setting one hands back
// the one it replaces.
type CodeNodePrinter = (node: SyntaxTreeNode) => Value
let codeNodePrinter: CodeNodePrinter = (node) => node as unknown as Value
const setCodeNodePrinter = (printer: CodeNodePrinter): CodeNodePrinter => {
  const previous = codeNodePrinter
  codeNodePrinter = printer
  return previous
}

// MARK: Prelude
// What the language defines in itself lives in prelude.fl, in Fluent.

// Parsed once, on first use – createScope runs on every evaluation (each keystroke), and
// a page that only parses never needs it
let preludeTree: SyntaxTreeNode | null = null

const createScope = () => {
  const scope = Object.create(DefaultEnvironment)
  evaluateSyntaxTreeNode(preludeTree ??= CodeParse(PRELUDE), scope)
  return scope
}


// MARK: Exports

export {
  // tensors
  engine,
  FluentVariable,
  FluentData,
  isTensor,
  operand,
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
  // errors
  NoMatch,
  StringLiteral,
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
