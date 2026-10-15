// MARK: Fluent language core
// Grammar, evaluator, environment and prelude – no DOM, no React, no Monaco.
// Imported by the IDE (client.tsx) and by the end-to-end tests (tests.ts).

import { grammar, type ActionDict } from "ohm-js";
import { toAST as ohmToAST } from "ohm-js/extras";
import { signal, Signal, computed, effect } from "@preact/signals-core"
import dedent from "ts-dedent";
import * as engine from "./engine/tensor.ts"

// the engine computes on WebGPU where the platform has it, on the CPU everywhere else
await engine.init()

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
  return v
}

const shapeOf = (v: Value): number[] =>
  (v instanceof FluentVariable ? v.current : (v as engine.Tensor)).shape

// A scalar is a rank-0 tensor holding a single phantom item (#(5) = 1), so
// index/sort/softmax on it return the rank-0 identity rather than leaking the
// backend's "axis of length 0" for an axis the scalar doesn't have.
const isScalarTensor = (v: Value): boolean =>
  (isTensor(v) || v instanceof FluentVariable) && shapeOf(v).length === 0

// Trainable variable: its current value, settled into a leaf; an assignment (drag,
// optimizer step, :=) settles the new one and bumps the version signal.
class FluentVariable {
  current: engine.Tensor
  readonly version = signal(0)
  constructor(initial: engine.Tensor) {
    this.current = engine.settle(initial)
    const registry = this instanceof FluentData ? dataVariables : trainableVariables
    registry.add(this)
    registerDisposable(() => { registry.delete(this) })
  }
  get shape() { return this.current.shape }
  get size() { return this.current.size }
  assign(next: engine.Tensor): void {
    this.current = engine.settle(next)
    this.version.value++
  }
  toString() { return this.current.toString() }
}

// Optimizers with no explicit variable list train every variable alive.
const trainableVariables = new Set<FluentVariable>()

// Data slot (~~): a mutable tensor the optimizer reads fresh on every step – never
// differentiated, no optimizer state. Gradients flow into ~, never into ~~.
class FluentData extends FluentVariable {}
const dataVariables = new Set<FluentData>()

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
    const forward: Function = dyadic ? (z: Value) => (fn as Function)(left, z) : fn as Function
    let step = forward
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
          const verdict = (until as Function)(value, prev)
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
const FunctionIs = (v: Value) => engine.tensor(isFn(v) ? 1 : 0)

// Async repeat (⟳): runs fn between frames so the UI stays live. The loop
// belongs to the evaluation that started it – a re-evaluation cancels it.
// (For synchronous function iteration f(f(f(x))) use ⍣ / FunctionPower.)
const FunctionIterate = (fn: (index?: engine.Tensor) => void, iterations?: Value) => {
  const live = iterations instanceof Signal || iterations instanceof FluentVariable
  if (!(typeof fn === "function" && (iterations === undefined || isTensor(iterations) || live))) {
    throw new Error("`FunctionIterate(fn, iterations)`: `fn` must be a function and `iterations` must be a scalar Tensor");
  }

  // The count is a BUDGET: `f ⟳ n` runs f n times in total. A live count is the
  // same budget, read fresh – raise the slider and the loop runs on to the new
  // total, lower it past what has already run and it stops. A constant signal
  // therefore behaves exactly like the literal, which is the property that
  // makes the two spellings mean one thing.
  const readTarget = (): number => {
    if (iterations === undefined) { return 1 }
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
  readTarget()   // a bad count is an error at the call, not silently zero frames

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
    cause === undefined ? undefined : { cause })

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

const Tensor = (values: unknown, shape?: Value) => {
  const source: any = isTensor(values) || values instanceof FluentVariable ? operand(values) : values
  return engine.tensor(source, shape === undefined ? undefined : { shape: asNumberList(shape) })
}
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
    const axis = args[1] === undefined ? 0 : asNumber(args[1] as Value)
    return engine.stack(broadcastRefs(args[0] as Value[]), axis)
  }
  return engine.stack(broadcastRefs(args as Value[]))
}
const TensorUnstack = (a: Value, b?: Value) => {
  const axis = b === undefined ? 0 : asNumber(b)
  const array = operand(a) as engine.Tensor
  // the iterator yields slices along the first axis
  return [...(axis === 0 ? array : engine.moveaxis(array, axis, 0))]
}
const TensorConcat = (...args: unknown[]) => {
  const listed = Array.isArray(args[0])
  const axis = listed && args[1] !== undefined ? asNumber(args[1] as Value) : 0
  const xs = ((listed ? args[0] : args) as Value[]).map(operand) as engine.Tensor[]
  return engine.concatenate(xs, axis)
}
const TensorTile = (a: Value, reps: Value) => {
  return engine.tile(operand(a), asNumberList(reps))
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
    const lifted = engine.reshape(operand(a), [...frameA, ...Array(frameBLength + cellPad).fill(1), ...cellA])
    return safeApply(f, [lifted, b], scope)
  }
}

// Rank operator ⍤ – Iverson's rank conjunction (J's `"`). `f ⍤ k` applies f to
// each rank-k CELL of its argument and reassembles over the leading FRAME axes,
// so one function works at every rank: `normalize ⍤ 1` does each row, `⍤ 2` each
// matrix, bare `normalize` the whole array. Dyadic `a (f ⍤ k) b` zips matching
// frames. The frame is mapped by unstack→apply→stack, so ∇ flows through it.
const TensorRank = function (this: CurrentScope, f: Value, rank?: Value) {
  const k = rank === undefined ? 0 : asNumber(rank)                            // k ≥ 0 = cell rank (trailing); k < 0 = frame rank (leading), J-style
  const scope = this
  const cellsOf = (a: Value): { frame: number[]; cells: Value[] } => {
    const shape = shapeOf(a), split = k >= 0 ? Math.max(0, shape.length - k) : Math.min(shape.length, -k)
    const frame = shape.slice(0, split), cell = shape.slice(split)
    const flat = engine.reshape(operand(a), [frame.reduce((x, y) => x * y, 1), ...cell])
    return { frame, cells: TensorUnstack(flat) as Value[] }
  }
  return function (a: Value, b?: Value) {
    const A = cellsOf(a)
    let results: Value[], frame = A.frame
    if (b === undefined) { results = A.cells.map((c) => safeApply(f, [c], scope)) }
    else {
      const B = cellsOf(b), na = A.cells.length, nb = B.cells.length
      if (na !== nb && na !== 1 && nb !== 1) { return new Error("`(f ⍤ k)`: dyadic rank needs frames that match or broadcast") }
      const N = Math.max(na, nb); frame = na >= nb ? A.frame : B.frame          // a lone cell broadcasts across the other's frame
      results = Array.from({ length: N }, (_, i) => safeApply(f, [A.cells[na === 1 ? 0 : i]!, B.cells[nb === 1 ? 0 : i]!], scope))
    }
    const err = results.find((r) => r instanceof Error); if (err) { return err }
    const stacked = TensorStack(results)
    return engine.reshape(operand(stacked), [...frame, ...shapeOf(stacked).slice(1)])
  }
}

// Inner product ∙ – the dual of the table ⊗. `a (g ∙ f) b` contracts a's LAST
// axis with b's FIRST: combine with g, then reduce with f – result = f-reduce
// over k of g(a[…,k], b[k,…]). Written in the order it runs, like `a × b reduce +`:
// matrix multiply is `× ∙ +`; swap the ring for the rest – `+ ∙ ⌊` is min-plus
// (shortest paths), `∧ ∙ ∨` is boolean reachability. One combinator, any semiring.
const TensorInner = function (this: CurrentScope, g: Value, f: Value) {
  const scope = this
  const gOuter = (u: Value, v: Value): Value => {                              // g between a rank-r u and rank-s v → the [u-frame, v-frame] table
    const su = shapeOf(u), sv = shapeOf(v)
    const uR = engine.reshape(operand(u), [...su, ...Array(sv.length).fill(1)])
    const vR = engine.reshape(operand(v), [...Array(su.length).fill(1), ...sv])
    return safeApply(g, [uR, vR], scope)
  }
  return function (a: Value, b: Value) {
    const aSlices = TensorUnstack(engine.moveaxis(operand(a), -1, 0)) as Value[]   // a's contracted axis to the front
    const bSlices = TensorUnstack(b) as Value[]                                      // b's contracted axis is already first
    if (aSlices.length !== bSlices.length) { return new Error("`(g ∙ f)`: a's last axis and b's first axis must match") }
    const terms = aSlices.map((u, k) => gOuter(u, bSlices[k]!))
    const err = terms.find((t) => t instanceof Error); if (err) { return err }
    return ListReduce(terms, (acc: Value, t: Value) => safeApply(f, [acc, t], scope))
  }
}

const TensorAdd = binaryOp(engine.add)
const TensorSubtract = binaryOp(engine.subtract)
const TensorMultiply = binaryOp(engine.multiply)
const TensorDivide = binaryOp(engine.divide)

// a literal integer exponent is exponentiation by squaring in the engine – exact
// where exp(e·ln x) is not (3^2 is 9, (-2)^2 is 4), and x^2 differentiates as x·x
const TensorPower = binaryOp(engine.power)
// √x is the square root; x √ n is the n-th root of x – the value first, like x ^ n and x log b
const TensorRoot = (a: Value, b?: Value) => {
  if (b === undefined) {
    return engine.sqrt(operand(a))
  }
  return TensorPower(a, TensorReciprocal(b))
}
// WGSL permits 2.5 ULP of division error, so on WebGPU floor(x/x) can miss
// and remainder(x, x) comes back as x. A remainder of exactly ±y is
// mathematically impossible – fold it to 0.
const TensorRemainder = (a: Value, b: Value) => {
  const r = engine.remainder(operand(a), operand(b))
  return engine.where(engine.equal(engine.absolute(r), engine.absolute(operand(b))), 0, r)
}
const TensorMaximum = binaryOp(engine.maximum)
const TensorMinimum = binaryOp(engine.minimum)

// Comparisons yield float32 0/1, APL-style: Fluent is a float32 language, and 0/1
// composes – `(x = 1) - y` is arithmetic, and prints as numbers, not booleans.
const comparisonOp = (op: (x: any, y: any) => engine.Tensor) =>
  (a: Value, b: Value) => {
    const x = numericArg(a), y = numericArg(b)
    return engine.astype(op(x, y), engine.float32)
  }

const TensorLess = comparisonOp(engine.less)
const TensorGreater = comparisonOp(engine.greater)
const TensorLessEqual = comparisonOp(engine.lessEqual)
const TensorGreaterEqual = comparisonOp(engine.greaterEqual)
const TensorEqual = comparisonOp(engine.equal)
const TensorNotEqual = comparisonOp(engine.notEqual)

// Match ≡ – is it the same value, whole? Tensors match when their shapes and
// every element agree, lists when their elements match pair by pair, strings
// when their text does. 1 or 0, like a comparison – but one answer for the
// whole value, where = answers element by element. It's the question ⍣ asks
// when it iterates until nothing changes.
// ≡ is tolerant, as APL's match is: two numbers match within a few units of f32
// rounding. Iterating to a fixed point, (f ⍣ ≡), can otherwise settle into a cycle
// of neighbouring floats – cos at the Dottie number alternates between two.
const MATCH_TOLERANCE = 2 ** -20
const near = (x: unknown, y: unknown): boolean =>
  x === y || (typeof x === "number" && typeof y === "number" && Math.abs(x - y) <= MATCH_TOLERANCE * Math.max(Math.abs(x), Math.abs(y)))
const valuesMatch = (a: Value, b: Value): boolean => {
  if (a instanceof FluentVariable) { a = a.current }
  if (b instanceof FluentVariable) { b = b.current }
  if (isTensor(a) && isTensor(b)) {
    const sa = (a as engine.Tensor).shape, sb = (b as engine.Tensor).shape
    if (sa.length !== sb.length || sa.some((d, i) => d !== sb[i])) { return false }
    const da = [getAsSyncList(a)].flat(Infinity), db = [getAsSyncList(b)].flat(Infinity)
    return da.every((x, i) => near(x, db[i]))
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    return a.length === b.length && a.every((x, i) => valuesMatch(x as Value, b[i] as Value))
  }
  const text = (v: Value) => typeof v === "string" || v instanceof String
  if (text(a) && text(b)) { return String(a) === String(b) }
  return a === b
}
const Match = (a: Value, b: Value) => TensorScalar(valuesMatch(a, b) ? 1 : 0)

// Logical ops share the comparison contract: any nonzero operand counts as
// true, the result is float32 0/1.
const TensorOr = comparisonOp(engine.logicalOr)
const TensorAnd = comparisonOp(engine.logicalAnd)
const TensorXor = comparisonOp(engine.logicalXor)
const TensorNot = (a: Value) => engine.astype(engine.logicalNot(operand(a)), engine.float32)

const TensorSine = unaryOp(engine.sin)
const TensorCosine = unaryOp(engine.cos)
const TensorTangentHyperbolic = unaryOp(engine.tanh)
const TensorSineInverse = unaryOp(engine.asin)
const TensorCosineInverse = unaryOp(engine.acos)
const TensorTangentInverse = unaryOp(engine.atan)
const TensorSineHyperbolicInverse = unaryOp(engine.arcsinh)
const TensorCosineHyperbolicInverse = unaryOp(engine.arccosh)
const TensorTangentHyperbolicInverse = unaryOp(engine.arctanh)
const TensorArcTangent2 = binaryOp(engine.arctan2)  // arctan2(y, x): angle of (x, y)
const TensorHypotenuse = binaryOp(engine.hypot)

// Reductions take an optional axis (scalar or vector tensor) as second
// argument, and a truthy third argument keeps the reduced axis as size 1 –
// mean(x, -1, 1) is the trace-safe unsqueeze(mean(x, -1), …)
const withOptionalAxis = (op: (a: any, axis?: number | number[] | null, opts?: { keepdims?: boolean }) => engine.Tensor) =>
  (a: Value, b?: Value, keep?: Value) => {
    const axis = b === undefined ? null : (Array.isArray(requireData(b)) ? asWholeList(b) : asWhole(b))
    const opts = keep === undefined ? undefined : { keepdims: asNumber(keep) !== 0 }
    const x = operand(a)
    return op(x, axis, opts)
  }

const TensorSum = withOptionalAxis(engine.sum)      // TensorReduce(a, +)
const TensorProduct = withOptionalAxis(engine.prod) // TensorReduce(a, ×)
const TensorMean = withOptionalAxis(engine.mean)
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
// return the index of the extreme element. Over an empty reduction axis there
// is no index – error at the source rather than hand a downstream gather one.
const emptyReductionAxis = (a: Value, axis?: Value): boolean => {
  const shape = (a instanceof FluentVariable ? a.current : a as any)?.shape
  if (!Array.isArray(shape)) { return false } // not a tensor – let the op handle it
  if (axis === undefined) { return shape.some((d: number) => d === 0) }
  const n = asNumber(axis)
  return shape[n < 0 ? shape.length + n : n] === 0
}
// The arg-ops tag their indices int32; Fluent is float32 throughout, so cast like
// TensorRange does – `_` re-casts its index to int32.
const TensorArgMax = (a: Value, axis?: Value) =>
  emptyReductionAxis(a, axis) ? new Error("`argmax`: an empty tensor has no maximal index")
    : (axis === undefined ? engine.argmax(operand(a)) : engine.argmax(operand(a), asWhole(axis))).astype(engine.float32)
const TensorArgMin = (a: Value, axis?: Value) =>
  emptyReductionAxis(a, axis) ? new Error("`argmin`: an empty tensor has no minimal index")
    : (axis === undefined ? engine.argmin(operand(a)) : engine.argmin(operand(a), asWhole(axis))).astype(engine.float32)

const TensorNegate = unaryOp(engine.negative)
const TensorAbsolute = unaryOp(engine.absolute)
const TensorSign = unaryOp(engine.sign)
// log(x) is the natural logarithm; log(x, b) takes the base b, so 8 log 2 = 3
const TensorNaturalLogarithm = unaryOp(engine.log)
const TensorLogarithm = (a: Value, b?: Value) =>
  b === undefined ? TensorNaturalLogarithm(a) : TensorDivide(TensorNaturalLogarithm(a), TensorNaturalLogarithm(b))
const TensorLog1Plus = unaryOp(engine.log1p)      // log(1 + x), accurate near 0
const TensorExponential = unaryOp(engine.exp)
const TensorExpMinus1 = unaryOp(engine.expm1)     // exp(x) - 1, accurate near 0
const TensorReciprocal = unaryOp(engine.reciprocal)
const TensorSquareRoot = unaryOp(engine.sqrt)
const TensorCubeRoot = unaryOp(engine.cbrt)
const TensorRound = unaryOp(engine.round)
const TensorTruncate = unaryOp(engine.trunc)      // toward zero, unlike floor
const TensorCeil = unaryOp(engine.ceil)
const TensorFloor = unaryOp(engine.floor)
const TensorErrorFunction = unaryOp(engine.erf)
const TensorSoftmax = (x: Value, axis?: Value) =>
  isScalarTensor(x)
    ? engine.reshape(engine.softmax(engine.reshape(operand(x), [1]), 0), [])
    : engine.softmax(operand(x), axis === undefined ? undefined : asWhole(axis))
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
  return engine.oneHot(engine.astype(operand(indices), engine.int32), n)
}
const TensorCrossEntropy = (labels: Value, logits: Value) =>
  engine.mean(engine.negative(engine.sum(engine.multiply(operand(labels), engine.logSoftmax(operand(logits))), -1)))

const TensorSort = (x: Value) =>
  isScalarTensor(x) ? engine.reshape(operand(x), []) : engine.sort(operand(x))
const TensorArgSort = (x: Value) =>
  isScalarTensor(x)
    ? engine.reshape(engine.argsort(engine.reshape(operand(x), [1])), []).astype(engine.float32)
    : engine.argsort(operand(x)).astype(engine.float32)
const TensorSlice = (a: Value, begin: Value, size?: Value) => {
  const beginList = asNumberList(begin)
  const sizeList = size === undefined ? undefined : asNumberList(size)
  const spec = beginList.map((b, i) => {
    const s = sizeList?.[i]
    return (s === undefined || s === -1 ? [b] : [b, b + s]) as [number] | [number, number]
  })
  return (operand(a) as engine.Tensor).slice(...spec)
}
const TensorMask = (a: Value, b: Value) => {
  // Selection is along axis 0 only: every step below (arange over shape[0],
  // sort+flip, slice the first `count` rows) is defensible at rank 1 and
  // meaningless above it, where it used to return a silently garbage tensor of
  // the wrong rank containing rows never selected. A true elementwise mask has
  // a data-dependent output shape, which Fluent has no op for – so say so.
  if (shapeOf(b).length > 1) {
    return new Error(`mask: the mask must be a vector, got a tensor [${shapeOf(b).join(" ")}] – flatten it, or index with \`_\``)
  }
  const count = asNumber(TensorSum(TensorBoolean(b)))
  const size = shapeOf(a)[0] ?? 0
  const indices = engine.arange(size).astype(engine.float32)
  const holes = engine.full([size], -1)
  const mu = TensorWhere(b, indices, holes)
  const top = TensorReverse(TensorSort(mu))
  const valid = TensorSlice(top, TensorScalar(0), TensorScalar(count))
  return TensorGather(a, TensorReverse(valid))
}
const TensorFill = (shape: Value, value: Value) => {
  return engine.full(asWholeList(shape), asNumber(value))
}
const TensorBoolean = (a: Value) => engine.astype(operand(a), engine.bool)

const TensorGradient = (f: Value) => {
  if (typeof f !== "function") {
    return new Error("`grad(f)`: `f` must be a function")
  }
  // vjp with a ones cotangent: scalar outputs give the classic gradient, elementwise
  // outputs per-element derivatives – ∇({x | x^2}) works on vectors
  return (x: Value) => {
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
const TensorTranspose = unaryOp(engine.transpose)
// Move one axis to a new position, keeping the others' relative order (a zero-copy
// view). The general axis-rotation that `transpose` (full reverse) can't express;
// `alongAxis` is built on it. Negative positions count from the end.
const TensorMoveAxis = (a: Value, from: Value, to: Value) => {
  const source = asWhole(from), target = asWhole(to)
  const x = operand(a) as engine.Tensor
  return engine.moveaxis(x, source, target)
}
const TensorRange = (a: Value, b?: Value) => {
  // arange tags its result int32 – Fluent tensors are float32 throughout
  if (b === undefined) {
    return engine.arange(Math.trunc(asFinite(a))).astype(engine.float32)
  }

  const start = Math.trunc(asFinite(a))
  const stop = Math.trunc(asFinite(b))
  const step = start <= stop ? 1 : -1
  return engine.arange(start, stop, step).astype(engine.float32)
}

// Inclusive range: like `start ..< stop`, but the endpoint is included –
// `1 ... 5 = [1, 2, 3, 4, 5]`, `5 ... 1 = [5, 4, 3, 2, 1]`.
const TensorRangeInclusive = (a: Value, b: Value) => {
  const start = Math.trunc(asFinite(a))
  const stop = Math.trunc(asFinite(b))
  const step = start <= stop ? 1 : -1
  return engine.arange(start, stop + step, step).astype(engine.float32)
}

const TensorLinearSpace = (range: Value, steps: Value) => {
  const bounds = requireData(range)
  if (!Array.isArray(bounds) || bounds.length < 2) {
    throw new Error("`linspace([start, stop], count)`: range must be a 2-element tensor")
  }
  const [start, stop] = bounds as [number, number]
  // a count has to be a whole number; a live slider naturally drives it
  // through fractional values (resolution × 90 + 10), so round rather than reject
  return engine.linspace(start, stop, Math.max(0, Math.round(asNumber(steps))))
}

// Rank-polymorphic convolution: the kernel's rank sets the conv's rank, so the
// same conv slides a 1-D kernel over a vector or a 2-D kernel over an image,
// zero padded so the output keeps the input's shape.
const TensorConvolution = (arr: Value, kernel: Value) => engine.conv(operand(arr), operand(kernel))

// Real FFT: a real signal in, the positive-frequency half out. Fluent has no
// complex dtype, so return the two components stacked as [2, bins] – f_0 is the
// real part, f_1 the imaginary, magnitude is √(f_0^2 + f_1^2).
const TensorFFT = (a: Value) => {
  const { real, imag } = engine.fft.rfft(operand(a))
  return engine.stack([real, imag])
}

// Coordinate grids: meshgrid(0 ..< w, 0 ..< h) → [X, Y], each broadcast to the full grid
const TensorMeshgrid = (...args: Value[]) => engine.meshgrid(args.map(operand))

const TensorPad = (a: Value, width: Value) => engine.pad(operand(a), asNumber(width))

const TensorRepeat = (a: Value, count: Value) => {
  const n = asNumber(count)
  const x = operand(a) as engine.Tensor
  return engine.repeat(x, n)
}

const TensorSinc = unaryOp(engine.sinc)

// The k largest values and their indices, as [values, indices]
const TensorTopK = (a: Value, k: Value) => {
  const [values, indices] = engine.topK(operand(a), asWhole(k))
  return [values, indices.astype(engine.float32)]
}

// Einstein summation: einsum("ij,jk->ik", a, b) is a matrix multiply
const TensorEinsum = (subscripts: Value, ...args: Value[]) =>
  engine.einsum(String(subscripts), ...args.map(operand))

const TensorReshape = (a: Value, b?: Value) => {
  if (b !== undefined) {
    return engine.reshape(operand(a), asNumberList(b))
  }

  return engine.tensor(shapeOf(a))
}

// Toroidal shift – APL's rotate. roll(x, 1) shifts flat; roll(x, s, axis)
// shifts along an axis, wrapping around.
const TensorRoll = (a: Value, shift: Value, axis?: Value) =>
  engine.roll(operand(a), requireData(shift) as number | number[],
    axis === undefined ? undefined : requireData(axis) as number | number[])

const TensorReverse = (a: Value, axis?: Value) =>
  engine.flip(operand(a), axis === undefined ? undefined : requireData(axis) as number | number[])

const TensorMatrixMultiply = binaryOp(engine.matmul)
const TensorDotProduct = binaryOp(engine.dot)

const TensorLength = (a: Value, b?: Value) => {
  // a list or a string is another candidate's case – decline it
  if (!isTensor(a) && !(a instanceof FluentVariable)) { return new NoMatch(`expected a tensor, got ${describeArg(a)}`) }
  // A missing axis has size 1, never NaN: a scalar (shape []) is one item along
  // its phantom leading axis, so `#(5)` is 1 (APL's tally ≢), matching how
  // broadcasting treats absent axes as size 1. An empty tensor (shape [0]) stays 0.
  if (b !== undefined) {
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
  if (isScalarTensor(a)) { a = engine.reshape(operand(a), [1]) }
  const size = shapeOf(a)[0] ?? 0
  // Bounds-check every index on the host or cheap to compute there – a gather has
  // no bounds mode, so an out-of-range index would read garbage. Literals and
  // computed indices like `t_(argmax(...))` are validated; an index still on the
  // GPU (an embedding lookup into a minibatch) is trusted data and flows through.
  const flat = cheapValues(b)
  if (flat !== undefined) {
    // `!(0 <= w < size)` also rejects a NaN index, which slips past `<`/`>=`
    const bad = flat.find((i) => { const w = i < 0 ? i + size : i; return !(w >= 0 && w < size) })
    if (bad !== undefined) {
      return new Error(`index ${bad} is out of bounds for an axis of length ${size}`)
    }
  }
  const raw = engine.astype(operand(b), engine.int32)
  // negative indices count from the end: a_(-1) is the last element. The
  // outer cast makes the dtype strongly int32 – astype keeps weak typing, and
  // weak int32 promotes to float32 in the where().
  const wrapped = engine.astype(engine.where(engine.less(raw, 0), engine.add(raw, size), raw), engine.int32)
  return engine.take(operand(a), wrapped, 0)
}

const TensorWhere = (a: Value, b: Value, c: Value) =>
  engine.where(engine.astype(operand(a), engine.bool), operand(b), operand(c))
const TensorIsNaN = (a: Value) => engine.astype(engine.isnan(operand(a)), engine.float32)

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

const TensorAssign = (a: Value, b: Value) => {
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

// Optimizers: update rules driven by valueAndGrad. The loss thunk closes over the
// variables – during the step each variable carries its traced leaf, so
// reads inside the thunk differentiate.
const makeOptimizer = (transform: GradientTransformation, explicitVars?: FluentVariable[]) => {
  let state: OptState | null = null
  let stateVars: FluentVariable[] = []
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
    // data slots are read fresh every step, never differentiated
    const slots = [...dataVariables]
    if (state === null || stateVars.length !== vars.length || stateVars.some((v, i) => v !== vars[i])) {
      state = engine.mapTree((x: engine.Tensor) => engine.settle(x), transform.init(vars.map(v => v.current)) as any) as OptState
      stateVars = vars
    }
    const lossFromParams = (params: engine.Tensor[], data: engine.Tensor[]) => {
      const saved = vars.map(v => v.current), savedData = slots.map(s => s.current)
      vars.forEach((v, i) => { v.current = params[i]! })
      slots.forEach((s, i) => { s.current = data[i]! })
      try {
        const out = (lossThunk as Function)()
        const value = out instanceof Signal ? out.peek() : out
        if (value instanceof Error) { throw value }
        if (!isTensor(value)) { throw new Error("loss must evaluate to a scalar tensor") }
        return value as engine.Tensor
      } finally {
        vars.forEach((v, i) => { v.current = saved[i]! })
        slots.forEach((s, i) => { s.current = savedData[i]! })
      }
    }
    const outer = differentiating
    differentiating = true
    try {
      const [loss, grads] = engine.valueAndGrad(lossFromParams)(vars.map(v => v.current), slots.map(s => s.current))
      const [updates, next] = transform.update(grads as engine.Tensor[], state, vars.map(v => v.current))
      const fresh = vars.map((v, i) => engine.add(v.current, (updates as engine.Tensor[])[i]!))
      // the step's results cross into the next step as data, computed together so they
      // share the forward and backward pass
      const leaves: engine.Tensor[] = []
      engine.mapTree((x: engine.Tensor) => { leaves.push(x) }, next as any)
      engine.materialize([loss as engine.Tensor, ...fresh, ...leaves])
      state = engine.mapTree((x: engine.Tensor) => engine.settle(x), next as any) as OptState
      vars.forEach((v, i) => { v.assign(fresh[i]!) })
      return loss as engine.Tensor
    } catch (e) {
      if (e instanceof StateWrite) { return new Error(`optimizer: the loss cannot change state while it is differentiated (${e.message})`) }
      throw e
    } finally {
      differentiating = outer
    }
  }
}

// An optimizer's update rule: its state from the parameters, and the step that turns
// gradients into updates (added to the parameters) and the next state
type OptState = unknown
interface GradientTransformation {
  init(params: engine.Tensor[]): OptState
  update(grads: engine.Tensor[], state: OptState, params?: engine.Tensor[]): [engine.Tensor[], OptState]
}
const sgdTransform = (learningRate: number, momentum?: number): GradientTransformation => ({
  init: (params) => (momentum ? params.map((p) => engine.multiply(p, 0)) : null),
  update(grads, state) {
    if (!momentum) { return [grads.map((g) => engine.multiply(g, -learningRate)), state] }
    const velocity = grads.map((g, i) => engine.add(engine.multiply((state as engine.Tensor[])[i]!, momentum), g))
    return [velocity.map((v) => engine.multiply(v, -learningRate)), velocity]
  },
})
const adamTransform = (learningRate: number, weightDecay = 0, b1 = 0.9, b2 = 0.999, eps = 1e-8): GradientTransformation => ({
  init: (params) => ({
    count: engine.tensor(0),
    mu: engine.mapTree((p: engine.Tensor) => engine.zerosLike(p), params as any),
    nu: engine.mapTree((p: engine.Tensor) => engine.zerosLike(p), params as any),
  }) as unknown as OptState,
  update: (updates, state, params) => {
    const { count, mu, nu } = state as unknown as { count: engine.Tensor, mu: engine.Tensor[], nu: engine.Tensor[] }
    const nextCount = engine.add(count, 1)
    const newMu = engine.mapTree((g: engine.Tensor, m: engine.Tensor) => engine.add(engine.multiply(g, 1 - b1), engine.multiply(m, b1)), updates as any, mu as any)
    const newNu = engine.mapTree((g: engine.Tensor, n: engine.Tensor) => engine.add(engine.multiply(engine.square(g), 1 - b2), engine.multiply(n, b2)), updates as any, nu as any)
    const c1 = engine.subtract(1, engine.power(b1, nextCount))
    const c2 = engine.subtract(1, engine.power(b2, nextCount))
    const scaled = weightDecay === 0
      ? engine.mapTree((m: engine.Tensor, n: engine.Tensor) =>
          engine.multiply(engine.divide(engine.divide(m, c1), engine.add(engine.sqrt(engine.divide(n, c2)), eps)), -learningRate),
          newMu as any, newNu as any)
      : engine.mapTree((m: engine.Tensor, n: engine.Tensor, p: engine.Tensor) =>
          engine.add(
            engine.multiply(engine.divide(engine.divide(m, c1), engine.add(engine.sqrt(engine.divide(n, c2)), eps)), -learningRate),
            engine.multiply(p, -learningRate * weightDecay)),
          newMu as any, newNu as any, params as any)
    return [scaled as any, { count: nextCount, mu: newMu, nu: newNu } as unknown as OptState]
  },
})

// AdaGrad: accumulate squared gradients
const adagradTransform = (learningRate: number, eps = 1e-7): GradientTransformation => ({
  init: (params) => engine.mapTree((p: engine.Tensor) => engine.zerosLike(p), params as any),
  update: (updates, state, params) => {
    const accum = engine.mapTree((g: engine.Tensor, s: engine.Tensor) => engine.add(engine.square(g), s), updates as any, state as any)
    const scaled = engine.mapTree(
      (g: engine.Tensor, s: engine.Tensor) => engine.multiply(engine.divide(g, engine.add(engine.sqrt(s), eps)), -learningRate),
      updates as any, accum as any)
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
  return makeOptimizer(sgdTransform(asNumber(a), momentum), vars)
}

const TensorOptimizationAdaGrad = (a: Value, ...rest: Value[]) => {
  const { vars } = optimizerRest(rest)
  return makeOptimizer(adagradTransform(asNumber(a)), vars)
}

// Stateful convenience RNG: a fresh key per call – deterministic per page load, not
// across reloads.
let rngCounter = 0
const TensorRandomNormal = (a: Value) => engine.normal(rngCounter++, asNumberList(a))
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
  FunctionCond,
  FunctionArity,
  FunctionEvaluate,
  FunctionApply,
  FunctionNoAutoLift,
  FunctionGuard,
  FunctionTry,
  FunctionWhen,
  ErrorCreate,
  NoMatch: NoMatchValue,
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
  Match,
  TensorOr,
  TensorAnd,
  TensorNot,
  TensorXor,
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
  TensorHypotenuse,

  TensorSum,
  TensorProduct,
  TensorMean,
  TensorMin,
  TensorMax,
  TensorReduce,
  TensorArgMax,
  TensorArgMin,

  TensorNegate,
  TensorAbsolute,
  TensorSign,
  TensorLogarithm,
  TensorLog1Plus,
  TensorExponential,
  TensorExpMinus1,
  TensorReciprocal,
  TensorSquareRoot,
  TensorCubeRoot,
  TensorRound,
  TensorTruncate,
  TensorCeil,
  TensorFloor,
  TensorErrorFunction,
  TensorSoftmax,
  TensorOneHot,
  TensorCrossEntropy,
  TensorSort,
  TensorArgSort,

  TensorGradient,

  TensorTranspose,
  TensorMoveAxis,
  TensorRange,
  TensorRangeInclusive,
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
  TensorRank,
  TensorInner,

  // List operations
  List,
  ListLength,
  ListConcat,
  ListGet,
  ListMap,
  ListReduce,

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
// trees; headless environments get the raw tree node.
let codeNodePrinter: (node: SyntaxTreeNode) => Value = (node) => node as unknown as Value
const setCodeNodePrinter = (printer: (node: SyntaxTreeNode) => Value) => {
  codeNodePrinter = printer
}

// MARK: Prelude

const PRELUDE = `
SymbolAssign(:, doc(SymbolAssign, "name: value", "Bind a name in the current scope. Glued to its left operand, \`:\` takes everything to its right – \`a: 1 + 2\` binds 3.", "a: 1 + 2, a × 10 = 30")),
(:=): doc(TensorAssign, "θ := value", "Assign a new value to a variable (made with ~ or ~~); optimizers do this every step, watch(θ) sees it. Mid-expression, parenthesize the value: θ := (a + b).", "θ: ~([0, 0]), θ := [1, 2]"),

; Functions
(.): apply: doc(FunctionApply, "args . f", "Pipe: apply the function on the right to the value on the left; a list spreads as arguments.", "[3, 1, 2] . sort = [1, 2, 3]"),
(⟳): iter: doc(FunctionIterate, "step ⟳ n", "Run a thunk n times, paced between display frames so the UI stays live – the loop for training and simulation. Async: a re-evaluation cancels it.", "opt: sgd(0.1), { opt(𝓛) } ⟳ 100"),
(⍣): power: FunctionPower: doc({ f, n | FunctionRepeat(f, n, FunctionInverse) }, "(f ⍣ n)(x)", "Power: f applied n times – f(f(…f(x))). A negative n runs f backwards, through its inverse. A predicate in place of n runs f until predicate(new, old) holds – (f ⍣ ≡) iterates to a fixed point. Dyadic x (f ⍣ n) y iterates f(x, ·), as in APL. Synchronous; for a frame-paced loop use ⟳.", "(double ⍣ 5)(1) = 32, 2 (^ ⍣ -1) 8 = 3, (cos ⍣ ≡)(1) ≈ 0.739"),

; Combinators – glyph, array-language name, mainstream name
(∘): then: compose: doc(FunctionCompose, "(f ∘ g)", "Then: apply f to the arguments, then g to the result – (f ∘ g)(x, y) = g(f(x, y)). Reading order is evaluation order. The Queer bird – APL's atop, swapped to read left-to-right.", "(+ ∘ √)(9, 16) = 5"),
(⍨): swap: commute: doc(FunctionCommute, "(⍨ f)", "Commute: flip a binary function's arguments – x (⍨ f) y = f(y, x). With one argument, duplicate it: (⍨ f)(x) = f(x, x). The Cardinal and the Warbler.", "3 (⍨ -) 10 = 7"),
(⍥): over: doc(FunctionOver, "(g ⍥ f)", "Over: preprocess each argument with g, then combine with f – x (g ⍥ f) y = f(g(x), g(y)). The preprocessor is read first because it runs first. The Psi bird.", "-3 (abs ⍥ =) 3 = 1"),
Φ: phi: fork: doc(FunctionFork, "Φ(f, g, h)", "Fork: apply the outer tines f and h to the arguments, combine the results with the middle tine g – x Φ(f, g, h) y = g(f(x, y), h(x, y)). A non-function tine is held constant. The Phoenix; dyadically the Pheasant.", "Φ(Σ, ÷, #)([1, 2, 3, 4]) = 2.5"),
(⊸): before: doc(FunctionBefore, "(f ⊸ g)", "Before: preprocess the left (or only) argument with f, then combine with g – (f ⊸ g)(x, y) = g(f(x), y); (f ⊸ g)(x) = g(f(x), x). A constant binds the left argument. The Violet Starling; dyadically the Zebra Dove.", "(1 ⊸ +)(41) = 42"),
(⟜): hook: after: doc(FunctionAfter, "(f ⟜ g)", "After: preprocess the right (or only) argument with g, then combine with f – (f ⟜ g)(x, y) = f(x, g(y)); (f ⟜ g)(x) = f(x, g(x)) – the J hook. A constant binds the right argument. The Starling; dyadically the Dove.", "(÷ ⟜ √)(16) = 4"),
𝕐: fix: recur: FunctionFix: doc({ f | g: { x | f(x(x)) }, g(g) }, "𝕐(f)", "Fixpoint – anonymous recursion. 𝕐(f) is the x with x = f(x): hand f its own recursion handle so a nameless function can call itself. The Y combinator; equivalently (⍨ @)((⍨ @) ∘ f). Named recursion needs nothing – a binding is visible in its own body.", "𝕐({ rec | { n | cascade((guard(n = 0, { 1 }), { n × rec(n - 1) }))() } })(5) = 120"),
(⊢): right: doc(FunctionRight, "x ⊢ y", "Right: yield the rightmost argument; with one argument, the identity.", "3 ⊢ 5 = 5"),
(⊣): left: doc(FunctionLeft, "x ⊣ y", "Left: yield the leftmost argument; with one argument, the identity.", "3 ⊣ 5 = 3"),
isFunction: doc(FunctionIs, "isFunction(v)", "1 if v is a function, else 0 – the question a combinator asks before treating an operand as a constant. Composes like a comparison.", "isFunction(√) = 1, isFunction(5) = 0"),

; Inverse and Under – ∇ gives every smooth function its slope, so Newton's
; method can run it backwards: no table of inverses, any smooth function.
FunctionSolve: solve: doc({ g, y, seed |
  ; back(x, v) carries an output-shaped v back to the inputs it came from –
  ; the transpose of g at x, by ∇
  back: { x, v | ∇({ z | Σ(v × g(z)) })(x) },
  miss: { x | abs(g(x) - y) ÷ (1 + abs(y)) },
  step: { x |
    ; the smallest move that makes g land on y: s is g's slope when g works
    ; element by element, and how often each element is picked when g selects
    s: ∇(g)(x),
    ; nothing moves where g doesn't look or is flat (s = 0) – and that zero is
    ; kept out of the division, so the derivative of the answer stays clean too
    picked: s ≠ 0,
    safe: where(picked, s, 1),
    Δ0: where(picked, back(x, y - g(x)) ÷ (safe × safe), 0),
    Δ: where(abs(Δ0) < 1e30, Δ0, 0),
    m: miss(x),
    ; halve the step wherever it doesn't help, until nothing is left to halve:
    ; an element must get strictly closer, unless it's already well inside the
    ; tolerance – and a step off the function (NaN) never helps
    shrink: { σ |
      mX: miss(x + (σ × Δ)),
      worse: back(x, isNaN(mX) ∨ ((m > 1e-6) ∧ (mX ≥ m))) ≠ 0,
      where(worse, where(σ < 1e-6, 0, σ ÷ 2), σ)
    },
    x + ((shrink ⍣ ≡)(Δ × 0 + 1) × Δ)
  },
  ; no miss ever grows – staying put is always allowed – so this ends
  x: (step ⍣ { new, old | max(miss(new)) ≥ max(miss(old)) })(seed),
  ; an answer is checked, never assumed: no solution near the seed is an Error
  cond(
    guard(max(miss(x)) ≤ 1e-4, { x }),
    { Error("solve: no solution near the seed", seed) }
  )
}, "solve(g, y, seed)", "The x nearest seed where g(x) = y – Newton's method, with ∇ supplying the slope. When g works element by element, it runs g backwards (the seed picks the branch). When g picks out part of its input – a gather, a slice, the diagonal – it changes that part of seed and keeps the rest. No solution is an Error, never a wrong number.", "solve(cos, 0, 1) ≈ 1.5708"),
FunctionInverse: inverse: doc({ g | FunctionArity((
  { x, y | FunctionInverse({ z | g(x, z) })(y) },
  { y |
    ; each element starts from whichever of y, 0 and 1 lands closest to it
    starts: stack((y, y × 0, y × 0 + 1)),
    off: abs(g(starts) - y),
    pick: argmin(where(isNaN(off), 1e30, off), 0),
    FunctionSolve(g, y, Σ(oneHot(pick, 3) × moveaxis(starts, 0, -1), -1))
  }
)) }, "inverse(g)", "g run backwards – the same as g ⍣ -1. Each element starts from whichever of y, 0 and 1 lands closest. Dyadic x inverse(g) y solves g(x, ?) = y.", "inverse({ x | x^3 + x })(10) = 2"),
FunctionUnder: under: (⍢): doc({ g, f |
  ; the answer is the point nearest the argument – the right one, dyadically –
  ; where g lands on f's result; when f changed the shape (it reduced), the
  ; search starts from the argument's mean instead
  near: { x, t | cond(guard(shape(g(x)) ≡ shape(t), { x }), { μ(x) + (t × 0) }) },
  FunctionArity((
    { a, b | t: f(g(a), g(b)), FunctionSolve(g, t, near(b, t)) },
    { x | t: f(g(x)), FunctionSolve(g, t, near(x, t)) }
  ))
}, "x (g ⍢ f) y", "Under: do f in g's world, then come back. When g transforms values – log, square – it is g⁻¹(f(g(x), g(y))): Pythagoras is + under square, the geometric mean is μ under log. When g picks out a part – a gather, a row, the diagonal – f edits just that part: (diagonal ⍢ (× ⟜ 0)) zeroes the diagonal. One rule for both: the nearest point to the argument where g lands on f's result. Reads left to right: g, then f, then g undone – (g ⍥ f) ∘ (g ⍣ -1).", "3 (square ⍢ +) 4 = 5"),
(≡): match: doc(Match, "x ≡ y", "Match: 1 if x and y are the same value whole – same shape and every element, numbers within a few units of float rounding – else 0. Where = compares element by element and exactly, ≡ answers once. (f ⍣ ≡) iterates f until nothing changes.", "[1, 2] ≡ [1, 2] = 1, [1, 2] ≡ [1, 3] = 0"),

(@): eval: doc(FunctionEvaluate, "f @ x", "Apply the function on the left to the argument on the right – reads as “f at x”. A list spreads as multiple arguments.", "∇({ x | x^2 }) @ 3 = 6"),
; Choosing – a candidate answers or declines (returns NoMatch). A choice takes
; the first answer; a failure (an Error) is an answer too, so it surfaces.
cascade: doc(FunctionCascade, "cascade((f, g, …))", "Try candidates in order with the same arguments; the first that answers wins. A candidate declines by returning NoMatch – guard and try make such candidates. An Error is an answer, not a decline: it stops the cascade and surfaces. When every candidate declines, the cascade declines too, so overload sets nest.", "cascade((guard(n = 0, { 1 }), { n × f(n - 1) }))()"),
cond: doc(FunctionCond, "cond(a, b, …)", "A multi-way conditional: run each candidate in order, the first that answers wins – cascade((…))() without the wrapper. A candidate that isn’t a thunk is its own answer, so cond(guard(c, { x }), 0) defaults to 0.", "cond(guard(n = 0, { 1 }), { n × fact(n - 1) })"),
guard: doc(FunctionGuard, "guard(cond, { value })", "A candidate that answers with value while cond holds, and declines otherwise. A broken condition – a typo, an Error – is a failure, never a quiet no.", "guard(n = 0, { 1 })"),
try: doc(FunctionTry, "try({ value })", "A candidate that declines when value fails: the Error becomes a NoMatch, so cascade and cond move on to the next candidate.", "cond(try({ CodeEvaluate(text) }), 0)"),
when: doc(FunctionWhen, "when(cond, { … })", "A conditional effect: run the thunk while cond is truthy, yield ◌ otherwise. Not a candidate – whatever the thunk returns, an Error included, is the result.", "when(training(), { opt(𝓛) })"),
Error: doc(ErrorCreate, "Error(message, cause?)", "A failure, as a value: no answer, and why. The cause can be any value – the Error this one grew from, or what was tried. An Error stops a cascade and surfaces.", "cond(guard(x ≥ 0, { √(x) }), { Error(\\"no real root\\", x) })"),
; NoMatch (bound natively) is the decline itself: what a candidate returns to
; say “not my case”. cascade and cond skip it; anywhere else it is an Error.

erf: doc(TensorErrorFunction, "erf(x)", "Gauss error function – the area under a normal curve out to x, saturating at ±1. Smooth and differentiable everywhere.", "erf(1) ≈ 0.8427"),

; Tensor shape/indexing
(#): len: length: doc(Length, "length(x)", "How many: a tensor's leading axis, a list's elements, or a string's characters.", "length(\\"abc\\") = 3"),
(_): gather: doc(TensorGather, "x_i", "Index into a string, list, or tensor. \`x_i\` picks one element; \`x_[i, j]\` gathers several, keeping the container's type. Negative indices count from the end.", "[10, 20, 30]_(-1) = 30"),
(⍴): reshape: doc(TensorReshape, "x ⍴ shape", "Reshape a tensor to a new shape; one dimension may be -1 to infer it.", "[1, 2, 3, 4] ⍴ [2, 2] = [[1, 2], [3, 4]]"),
(..<): range: doc(TensorRange, "start ..< stop", "Integer range from start up to (but not including) stop – the \`<\` marks the open end.", "0 ..< 5 = [0, 1, 2, 3, 4]"),
(...): rangeInclusive: doc(TensorRangeInclusive, "start ... stop", "Integer range from start through stop, endpoint included.", "1 ... 5 = [1, 2, 3, 4, 5]"),
shape: TensorShape,
slice: TensorSlice,
transpose: TensorTranspose,
moveaxis: TensorMoveAxis,
reverse: TensorReverse,
(⊗): outer: doc(TensorOuter, "a (⊗ f) b", "Table: apply f between every cell of a and every cell of b.", "(0 ..< 3) (⊗ ×) (0 ..< 3) = [[0,0,0],[0,1,2],[0,2,4]]"),
(⍤): rank: doc(TensorRank, "(f ⍤ k)", "Rank: apply f to each rank-k cell, mapping over the leading frame – one function, every rank. Negative k fixes the frame rank instead. Iverson's rank conjunction.", "(Σ ⍤ 1)([[1,2,3],[4,5,6]]) = [6, 15]"),
(∙): inner: doc(TensorInner, "a (g ∙ f) b", "Inner product: contract a's last axis with b's first – combine with g, then reduce with f, in the order it reads, like a × b reduce +. \`× ∙ +\` is matrix multiply; swap the ring for others: \`+ ∙ ⌊\` is shortest paths.", "[[1,2],[3,4]] (× ∙ +) [[5,6],[7,8]] = [[19,22],[43,50]]"),
alongAxis: { f, k |
  lift: (f ⍤ 1),                                              ; f lifted to run on each trailing fibre
  { x |
    n: #(shape(x)),                                           ; the rank of x
    lifted: lift(moveaxis(x, k, n - 1)),                      ; rotate axis k to the trailing cell, run f
    cascade(
      (guard(#(shape(lifted)) = n, { moveaxis(lifted, n - 1, k) }),   ; shape-preserving f: rotate the axis home
       { lifted })                                                     ; reducer: axis k is gone
    )()
  }
},
doc(alongAxis, "(f alongAxis k)", "Apply f along axis k: f runs on each fibre parallel to axis k, mapping over the rest. Any function, any axis; a reducer drops the axis, a shape-preserving f keeps it.", "(Σ alongAxis 0)([[1,2,3],[4,5,6]]) = [5, 7, 9]"),
(⌸): alongAxis,                  ; tier 1 — terse glyph (APL's key); alongAxis itself is tier 2, the concise word
TensorAlongAxis: alongAxis,      ; tier 3 — descriptive [Type][Function], stem matches (cf. rank/TensorRank)

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

windows: TensorWindows: doc({ w, arr |
  starts: (0 ..< (#(arr) - w + 1)),
  offsets: (0 ..< w),
  indices: (starts ⊗(+) offsets),
  arr _ indices
}, "windows(w, arr)", "The overlapping width-w sliding windows of arr, one per row (step 1).", "windows(2, [1, 2, 3, 4]) = [[1, 2], [2, 3], [3, 4]]"),

chunks: TensorChunks: doc({ w, arr |
  n: (#(arr) / w),
  starts: (0 ..< n × w),
  offsets: (0 ..< w),
  indices: (starts ⊗(+) offsets),
  arr _ indices
}, "chunks(w, arr)", "Split arr into non-overlapping width-w chunks; arr's length should be a multiple of w.", "chunks(2, [1, 2, 3, 4]) = [[1, 2], [3, 4]]"),

(⌺): stencil: TensorStencil: doc({ w, f, arr | (f ⍤ 1)(windows(w, arr)) }, "stencil(w, f, arr)", "Slide a width-w window over arr, reducing each with f – a windowed map. stencil(w, Σ, …) is a moving sum. The glyph is APL's stencil.", "stencil(3, Σ, [1, 2, 3, 4, 5]) = [6, 9, 12]"),
conv: TensorConvolution,   ; nD convolution: arr conv kernel – a 1D kernel over a vector, a 2D kernel over an image
(⊛): doc(TensorConvolution, "arr ⊛ kernel  ·  arr conv kernel", "Convolve an array with a kernel; the kernel's rank sets the conv's – a 1-D kernel runs along a vector, a 2-D kernel over an image. Zero-padded, so the output keeps the input's shape.", "[1, 2, 3, 4] ⊛ [1, 1, 1] = [3, 6, 9, 7]"),

; List operations – a list is a heterogeneous (a, b, …); these are its verbs,
; kept prefixed so they never blur into the tensor vocabulary (ListMap vs ⍤).
List: doc(List, "List(a, b, …)", "Build a list from its arguments – the same list a bare (a, b, …) makes.", "ListLength(List(1, 2, 3)) = 3"),
ListLength: doc(ListLength, "ListLength(list)", "How many elements a list has.", "ListLength((1, 2, 3)) = 3"),
ListConcat: doc(ListConcat, "ListConcat(a, b)", "Join two lists end to end.", "ListConcat((1, 2), (3, 4)) = (1, 2, 3, 4)"),
ListGet: doc(ListGet, "ListGet(list, i)", "The element at index i; a negative i counts from the end.", "ListGet((10, 20, 30), -1) = 30"),
ListMap: doc(ListMap, "ListMap(list, f)", "Apply f to every element, giving a new list – the list-land counterpart of ⍤.", "ListGet(ListMap((1, 2, 3), { x | x × 10 }), 1) = 20"),
ListReduce: doc(ListReduce, "ListReduce(list, f, init)", "Fold a list left to right: start at init, then f(acc, element) across the list.", "ListReduce((1, 2, 3), { a, b | a + b }, 0) = 6"),
ListGather: doc({ a, b |
  ListMap(b, { i | ListGet(a, i) })
}, "ListGather(list, indices)", "Pick the elements at the given indices, in that order – a list-land _.", "ListGather((10, 20, 30), (2, 0)) = (30, 10)"),
ListZip: doc({ a, b |
  n: TensorMinimum(ListLength(a), ListLength(b)),
  ListMap(
    TensorUnstack(TensorRange(0, n)),
    { i | List(ListGet(a, i), ListGet(b, i)) }
  )
}, "ListZip(a, b)", "Pair up elements of a and b, stopping at the shorter length.", "ListZip((1, 2), (3, 4)) = ((1, 3), (2, 4))"),
ListTake: doc({ list, n |
  ; take the first n, with n clamped to [0, length]
  ListMap(TensorUnstack(TensorRange(0, 0 ⌈ n ⌊ ListLength(list))), { i | ListGet(list, i) })
}, "ListTake(list, n)", "The first n elements, with n clamped to [0, length].", "ListTake((7, 8, 9), 2) = (7, 8)"),
ListDrop: doc({ list, n |
  len: ListLength(list),
  ListMap(TensorUnstack(TensorRange(0 ⌈ n ⌊ len, len)), { i | ListGet(list, i) })
}, "ListDrop(list, n)", "All but the first n elements, with n clamped to [0, length].", "ListDrop((7, 8, 9), 1) = (8, 9)"),
ListReverse: doc({ list |
  n: ListLength(list),
  ListMap(TensorUnstack(TensorReverse(TensorRange(0, n))), { i | ListGet(list, i) })
}, "ListReverse(list)", "The list back to front.", "ListReverse((1, 2, 3)) = (3, 2, 1)"),
ListEnumerate: doc({ list |
  n: ListLength(list),
  ListMap(
    TensorUnstack(TensorRange(0, n)),
    { i | List(i, ListGet(list, i)) }
  )
}, "ListEnumerate(list)", "Pair each element with its index, as (index, element).", "ListEnumerate((7, 8)) = ((0, 7), (1, 8))"),
ListScan: doc({ list, f, init |
  ListReduce(
    list,
    { acc, val |
      prev: ListGet(acc, -1),
      ListConcat(acc, List(f(prev, val)))
    },
    List(init)
  )
}, "ListScan(list, f, init)", "Like ListReduce, but keep every step – the running fold, starting from init.", "ListScan((1, 2, 3), +, 0) = (0, 1, 3, 6)"),

; Arithmetic
(/): (÷): div: TensorDivide,
(%): mod: TensorRemainder,
(^): pow: TensorPower,
(√): root: doc(TensorRoot, "x √ n", "The square root of x; with n, the n-th root of x.", "27 √ 3 = 3"),
add: doc(TensorAdd, "x + y", "Element-wise addition; shapes broadcast.", "[1, 2] + 10 = [11, 12]"),
sub: doc(TensorSubtract, "x - y", "Element-wise subtraction; shapes broadcast.", "[3, 4] - 1 = [2, 3]"),
mul: doc(TensorMultiply, "x × y", "Element-wise multiplication; shapes broadcast.", "[1, 2] × 3 = [3, 6]"),
(+): FunctionArity((TensorAdd, TensorAbsolute)),
(-): FunctionArity((TensorSubtract, TensorNegate)),
(*): FunctionArity((TensorMultiply, TensorSign)),
(×): FunctionArity((TensorMultiply, TensorSign)),
(·): FunctionArity((TensorMultiply, TensorSign)),

; Math
π: pi: 3.141592653589793,
neg: doc(TensorNegate, "neg(x)", "Negate each element.", "neg([1, -2]) = [-1, 2]"),
abs: doc(TensorAbsolute, "abs(x)", "Absolute value of each element.", "abs([-2, 3]) = [2, 3]"),
sign: doc(TensorSign, "sign(x)", "The sign of each element: -1, 0, or 1.", "sign([-5, 0, 3]) = [-1, 0, 1]"),
round: TensorRound,
trunc: TensorTruncate,
floor: doc(TensorFloor, "⌊(x)  ·  floor(x)", "Round each element down to the nearest integer.", "⌊(2.7) = 2"),
ceil: doc(TensorCeil, "⌈(x)  ·  ceil(x)", "Round each element up to the nearest integer.", "⌈(2.3) = 3"),
reciprocal: TensorReciprocal,
sqrt: TensorSquareRoot,
square: TensorSquare: { x | x × x },
cbrt: TensorCubeRoot,
hypot: TensorHypotenuse,
log: doc(TensorLogarithm, "log(x, base?)", "The natural logarithm of x; with a base, the logarithm of x in that base.", "8 log 2 = 3"),
log2: TensorLog2: { x | log(x) ÷ log(2) },
log10: TensorLog10: { x | log(x) ÷ log(10) },
log1p: TensorLog1Plus,
exp: TensorExponential,
expm1: TensorExpMinus1,
clamp: TensorClamp: { x, lo, hi | x ⌈ lo ⌊ hi },
; written through tanh, not 1 ÷ (1 + exp(-x)): the naive form's reverse rule
; is Inf ÷ Inf² once exp(-x) overflows float32 (below x ≈ -89), so a saturated
; unit hands back NaN where the true derivative underflows smoothly to 0. Same
; values to float32; softplus below is stable for the same reason.
sigmoid: TensorSigmoid: { x | 0.5 × (1 + tanh(x ÷ 2)) },
relu: TensorRelu: { x | x ⌈ 0 },
silu: TensorSilu: { x | x × sigmoid(x) },
rsqrt: TensorReciprocalSquareRoot: { x | 1 ÷ sqrt(x) },
softmax: TensorSoftmax,
oneHot: TensorOneHot,
crossEntropy: TensorCrossEntropy,

; Trigonometry
sin: TensorSine,
cos: TensorCosine,
tan: TensorTangent: { x | sin(x) ÷ cos(x) },
asin: arcsin: TensorSineInverse,
acos: arccos: TensorCosineInverse,
atan: arctan: TensorTangentInverse,
atan2: arctan2: TensorArcTangent2,
sinh: TensorSineHyperbolic: { x | (exp(x) - exp(-x)) ÷ 2 },
cosh: TensorCosineHyperbolic: { x | (exp(x) + exp(-x)) ÷ 2 },
tanh: TensorTangentHyperbolic,
asinh: arcsinh: TensorSineHyperbolicInverse,
acosh: arccosh: TensorCosineHyperbolicInverse,
atanh: arctanh: TensorTangentHyperbolicInverse,
deg2rad: TensorDegreesToRadians: { x | x × (π ÷ 180) },
rad2deg: TensorRadiansToDegrees: { x | x × (180 ÷ π) },

; Activations (after Trig, so tanh is in scope; sigmoid/relu/silu live up in Math)
softplus: softRelu: TensorSoftplus: doc({ x | log1p(exp(-abs(x))) + (x ⌈ 0) }, "softplus(x)", "A smooth, always-positive ReLU: log(1 + exp x). Differentiable everywhere.", "softplus([-2, 0, 2]) ≈ [0.13, 0.69, 2.13]"),
gelu: TensorGelu: doc({ x | 0.5 × x × (1 + tanh(0.7978845608 × (x + (0.044715 × x^3)))) }, "gelu(x)", "Gaussian Error Linear Unit – a smooth ReLU that gates x by a Gaussian. The transformer default.", "gelu([-1, 0, 1]) ≈ [-0.16, 0, 0.84]"),
mish: TensorMish: doc({ x | x × tanh(softplus(x)) }, "mish(x)", "A self-gated activation, x · tanh(softplus x) – smooth and non-monotonic.", "mish([-1, 0, 1]) ≈ [-0.30, 0, 0.87]"),

; Comparison
(<): lt: less: doc(TensorLess, "x < y", "Element-wise less-than: 1 where x is below y, else 0; shapes broadcast.", "([1, 2, 3] < 2) = [1, 0, 0]"),
(>): gt: greater: doc(TensorGreater, "x > y", "Element-wise greater-than: 1 where x is above y, else 0; shapes broadcast.", "([1, 2, 3] > 2) = [0, 0, 1]"),
(≤): (<=): le: lessEqual: doc(TensorLessEqual, "x ≤ y", "Element-wise less-than-or-equal: 1 where x is at most y, else 0; shapes broadcast.", "([1, 2, 3] ≤ 2) = [1, 1, 0]"),
(≥): (>=): ge: greaterEqual: doc(TensorGreaterEqual, "x ≥ y", "Element-wise greater-than-or-equal: 1 where x is at least y, else 0; shapes broadcast.", "([1, 2, 3] ≥ 2) = [0, 1, 1]"),
(=): equal: doc(TensorEqual, "x = y", "Element-wise equality: 1 where the operands are equal, else 0; shapes broadcast.", "([1, 2, 3] = 2) = [0, 1, 0]"),
(≠): (!=): notEqual: doc(TensorNotEqual, "x ≠ y", "Element-wise inequality: 1 where the operands differ, else 0; shapes broadcast.", "([1, 2, 3] ≠ 2) = [1, 0, 1]"),

; Logical
(∨): or: doc(TensorOr, "x ∨ y", "Element-wise logical or: 1 where either operand is nonzero, else 0.", "[0, 1, 0] ∨ [0, 1, 1] = [0, 1, 1]"),
(∧): and: doc(TensorAnd, "x ∧ y", "Element-wise logical and: 1 where both operands are nonzero, else 0.", "[0, 1, 1] ∧ [1, 1, 0] = [0, 1, 0]"),
(¬): not: doc(TensorNot, "¬(x)", "Logical not: 1 where x is zero, else 0.", "¬([0, 2]) = [1, 0]"),
(⊻): xor: doc(TensorXor, "x ⊻ y", "Element-wise exclusive or: 1 where exactly one operand is nonzero.", "[0, 1, 1] ⊻ [1, 1, 0] = [1, 0, 1]"),
(⍲): nand: TensorNand: doc({ a, b | ¬(a ∧ b) }, "x ⍲ y", "Element-wise nand: 0 where both operands are nonzero, else 1. Functionally complete – every gate builds from it.", "[0, 1, 1] ⍲ [1, 1, 0] = [1, 0, 1]"),
(⍱): nor: TensorNor: doc({ a, b | ¬(a ∨ b) }, "x ⍱ y", "Element-wise nor: 1 where both operands are zero, else 0. Functionally complete, like ⍲.", "[0, 1, 0] ⍱ [0, 1, 1] = [1, 0, 0]"),

; Reductions
(∇): grad: doc(TensorGradient, "∇(f)", "Gradient of a function: ∇(f)(x) is df/dx at x. Works on any function of smooth numeric ops — reshape and indexing pass through; argmax/round/comparisons read as constant (zero gradient). Nests: ∇(∇(f)) is the second derivative.", "∇({ x | x^2 })(3) = 6"),
(Σ): sum: doc(TensorSum, "Σ(x, axis?)", "Sum of the elements, over one axis or the whole tensor.", "Σ([1, 2, 3]) = 6"),
reduce: doc(TensorReduce, "x reduce fn", "Fold a tensor left-to-right with a binary function, optionally along an axis. Known ops (+, ×, ⌈, ⌊) reduce natively; so \`x reduce ⌈\` is max.", "1 ..< 10 reduce + = 45"),
(Π): prod: TensorProduct,
(μ): mean: TensorMean,

; Min/max: the WORD reduces (whole tensor, or one axis like sum) – so
; max(x, axis) works. The GLYPH is APL's ⌈/⌊: ceiling/floor on one tensor,
; pairwise max/min on two. (max and ⌈ can't be one function – max(x, axis)
; and pairwise max(a, b) share a shape, so the axis would be eaten as an operand.)
max: doc(TensorMax, "max(x, axis?)", "The largest element, over one axis or the whole tensor.", "max([3, 1, 2]) = 3"),
min: doc(TensorMin, "min(x, axis?)", "The smallest element, over one axis or the whole tensor.", "min([3, 1, 2]) = 1"),
(⌈): FunctionArity((doc(TensorMaximum, "x ⌈ y", "Element-wise maximum of two tensors.", "[1, 5] ⌈ [4, 2] = [4, 5]"), TensorCeil)),
(⌊): FunctionArity((doc(TensorMinimum, "x ⌊ y", "Element-wise minimum of two tensors.", "[1, 5] ⌊ [4, 2] = [1, 2]"), TensorFloor)),
argmax: TensorArgMax,
argmin: TensorArgMin,

; Statistics
𝕍: variance: TensorVariance: doc({ x | μ((x - μ(x))^2) }, "𝕍(x)", "Mean squared deviation from the mean, μ((x - μx)²).", "variance([1, 2, 3]) ≈ 0.667"),
σ: std: TensorStandardDeviation: doc({ x | √(variance(x)) }, "σ(x)", "Standard deviation, √variance – how far x spreads around its mean.", "σ([2, 4, 4, 4, 5, 5, 7, 9]) = 2"),
norm: l2norm: TensorL2Norm: doc({ x | √(Σ(x × x)) }, "norm(x)", "Euclidean (L2) norm, √(Σ x²) – the length of x as a vector.", "norm([3, 4]) = 5"),
normalize: TensorNormalize: doc({ x | x ÷ norm(x) }, "normalize(x)", "x scaled to unit length, x ÷ norm(x) – same direction, length 1.", "normalize([3, 4]) = [0.6, 0.8]"),

; Variables
(~): var: doc(TensorVariable, "~(init)", "Make a trainable variable. Assign with :=; optimise with adam / adamw / sgd / adagrad. For data the loss reads but never trains, use ~~.", "θ: ~([0, 0])"),
(~~): data: doc(TensorData, "~~(init)", "Make a data slot – every optimizer step reads it fresh, but no gradient flows into it and nothing trains it. Assign with := any time, even to a new shape – training carries on.", "x: ~~([1, 2]), x := [3, 4, 5]"),
watch: doc(TensorWatch, "watch(variable)", "A signal that updates whenever a variable is assigned – by a drag, an optimizer, or :=.", "θ: ~([2]), w: watch(θ), θ := [8], w = [8]"),

; Tensor ops
sort: doc(TensorSort, "sort(x)", "Sort a vector into ascending order.", "sort([3, 1, 2]) = [1, 2, 3]"),
(⍋): argsort: doc(TensorArgSort, "argsort(x)", "The indices that sort a vector into ascending order – grade up. x_argsort(x) is x sorted.", "argsort([3, 1, 2]) = [1, 2, 0]"),
roll: doc(TensorRoll, "roll(x, shift, axis?)", "Shift elements along an axis, wrapping around the edge (a torus).", "roll([1, 2, 3, 4], 1) = [4, 1, 2, 3]"),
flip: TensorReverse,
mask: doc(TensorMask, "mask(x, keep)", "Keep the elements of x where the boolean mask is true, dropping the rest.", "mask([5, 0, 6], [5, 0, 6] > 1) = [5, 6]"),
where: doc(TensorWhere, "where(cond, a, b)", "Element-wise choice: take a where cond is true, otherwise b.", "where([1, 0, 1], [1, 2, 3], 0) = [1, 0, 3]"),
isNaN: TensorIsNaN,
𝕀: eye: TensorIdentity: doc({ n | (0 ..< n) ⊗(=) (0 ..< n) }, "𝕀(n)", "The n×n identity matrix – 1 on the diagonal, 0 elsewhere.", "eye(2) = [[1, 0], [0, 1]]"),
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
(⚄): rand: doc(TensorRandomUniform, "⚄(shape)  ·  rand(shape)", "A tensor of the given shape, each element drawn uniformly from [0, 1). A fresh draw every call.", "⚄([2, 2])"),
randn: TensorRandomNormal,
linspace: TensorLinearSpace,
fill: TensorFill,
stack: TensorStack,
unstack: TensorUnstack,
concat: TensorConcat,
(⧉): tile: doc(TensorTile, "x ⧉ reps  ·  x tile reps", "Tile a tensor: repeat it along each axis, reps giving the count per axis (a scalar reps repeats a vector).", "[1, 2] ⧉ 3 = [1, 2, 1, 2, 1, 2]"),

; Optimization
adam: TensorOptimizationAdam,
adamw: doc(TensorOptimizationAdamW, "adamw(lr, weightDecay?, vars?)", "Adam with decoupled weight decay. A trailing list picks the variables to train.", "opt: adamw(0.01, 0.001)"),
sgd: doc(TensorOptimizationSgd, "sgd(lr, momentum?, vars?)", "Stochastic gradient descent, with optional momentum. A trailing list picks the variables to train.", "opt: sgd(0.01, 0.9)"),
adagrad: TensorOptimizationAdaGrad,

; Strings
StringToCodes: doc(StringToCodes, "StringToCodes(text)", "Text to a tensor of character codes – the door from strings into tensors.", "StringToCodes(\\"abc\\") = [97, 98, 99]"),
CodesToString: doc(CodesToString, "CodesToString(codes)", "A tensor of character codes back to text. Rounds first, so model outputs decode directly.", "CodesToString([104, 105]) = \\"hi\\""),
String: doc(StringLiteral, "String(x)", "The Fluent source that reproduces a value: CodeEvaluate(String(x)) is x again. A string comes back quoted, a list keeps its parens, a tensor keeps its rank. Exact for data; a signal or ~ round-trips to a new one, and a lambda needs its free names in scope.", "String([1, 2]) = \\"[1, 2]\\""),

; Misc
($): doc(Reactive, "$(value)", "Wrap a value in a signal (or a thunk in a computed signal). Read with x(), write with x(v).", "x: $(0.5), x ^ 2"),
; ← is defined here in the prelude, so its doc lives here too
(←): doc(FunctionNoAutoLift({ s, v | s(v) }), "signal ← value", "Write a value into a signal – same as signal(value), but reads left-to-right. Chains like any spaced operator: parenthesize compound values, s ← (a + b).", "x: $(1), x ← 9, x() = 9"),
once: doc(SignalOnce, "once(signal)", "Read a signal's current value without subscribing to it.", "x: $(4), once(x) + 1 = 5"),
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
