// MARK: Documentation

const Documentation = `
# Fluent

Fluent is a tiny language + IDE for differentiable tensors and reactive UIs. Fluent is for programmers, researchers, and tinkerers who want to combine math, differentiation, and interactivity in a playful way.

## Quickstart

- Try in browser: https://mlajtos.mu/fluent

\`\`\`fluent
; tensors + broadcasting
a: [1, 2, 3],
a + 1, ; [2, 3, 4]

; functions + gradient
square: { x | x^2 },
square(a), ; [1, 4, 9]
∇(square)(a), ; [2, 4, 6]

; reactive UI
x: $(0.5),
Slider(x),
$({ square(x()) }),
Button({ x(0) }, "Reset"),
\`\`\`

## Features

### Syntax
- Tensors
  - multi-dimensional arrays of numbers
  - scalars: \`1\`, \`3.14\`, \`-42\`, \`6.02e23\`
  - higher-rank: \`[1, 2, 3]\`, \`[[1, 2], [3, 4]]\`, \`[[[1]], [[2]], [[3]]]\`
  - auto-broadcasting: \`[1, 2, 3] + 1\` is \`[2, 3, 4]\`
- Lists
  - ordered collection of heterogeneous values
  - e.g. \`(1, 2, 3)\`, \`(1, (2, 3), [4])\`, \`()\`, \`(42,)\`
- Functions
  - lambda with \`{}\`: \`{ x | x + 1 }\`, \`{ x, y | x * y }\`, \`{ 42 }\`
  - last expression is the return value: \`{ 1 + 1, 42 }\`
  - application by juxtaposition: \`{ x, y | x * y }(6, 7)\`
  - application by infix: \`6 { x, y | x * y } 7\`
  - left-to-right, no precedence: \`1 + 2 * 3\` is \`(1 + 2) * 3\`
  - single-argument function equivalence: \`√(4)\` and \`(√4)\` are the same
- Symbols
  - e.g. \`a\`, \`FooBar\`, \`bar-baz-1\`, \`α\`, \`Σ𝓜ℂ2\`, \`+\`, \`≠\`, \`!=\`, \`⌈≠⌋\`
  - assignment with \`:\`: \`a: 23, b: (a + 24)\`
  - letter-based (\`a\`, \`α\`, ...) and non-letter-based (\`+\`, \`√\`, ...) do not conflict:
    - e.g. \`foo+bar\`, \`α≠β\`, \`a!!b!!c\` works without whitespace or parentheses
- Grouping
  - parentheses for expressions: \`1 + (2 * 3)\` is \`7\`
- Comments
  - single-line comments with \`;\`: \`1 + 2 ; this is a comment\`

### Semantics
- Differentiable programming
  - get gradient of function with \`∇\`: \`(∇ { x | x^2 }) 1\` is \`2\`
  - higher-order gradients: \`(∇ (∇ { x | x^3 })) 1\` is \`6\`
- Reactive programming
  - signal-based library-level support for reactivity
  - e.g. \`($): Reactive, a: $(1), b: $(2), c: $({ a() + b() }), b(41)\`, \`c()\` is \`42\`
  - paired with UI for interactive programs: \`a: $(0.5), Slider(a)\`
  - fine-grained updates: only parts of the program that depend on changed values are re-evaluated
- Built-in functions
  - lists: \`List\`, \`ListConcat\`, \`ListLength\`, \`ListGet\`, \`ListMap\`, etc.
  - tensors: \`Tensor\`, \`TensorStack\`, \`TensorUnstack\`, \`TensorConcat\`, \`TensorTile\`, etc.
  - tensor math: \`+\`, \`-\`, \`*\`, \`/\`, \`^\`, \`√\`, \`%\`, \`max\`, \`min\`, \`sin\`, \`cos\`, \`log\`, \`exp\`, \`sum\`, \`mean\`, \`<\`, \`>=\`, etc.
  - signals: \`SignalCreate\`, \`SignalRead\`, \`SignalUpdate\`, \`SignalEffect\`, etc.
  - user interface: \`Print\`, \`Slider\`, \`Button\`, \`Text\`, \`Grid\`, \`Image\`, \`Plot\`, etc.
  - ...and more!

### IDE
- Syntax highlight
- Live evaluation with granular error reporting
- Automatic visualization of values (notebook-style output)
- GPU-accelerated execution of tensor operations
- Auto-completion of symbols and built-in functions
- Shareable URL links to programs
- Dark theme
- LLM-assisted code generation (coming soon)
`

/*
## TODO

## Bugs
- shadowing of symbols doesn't work
  - treat assignments as a special case of function application that gets the symbol, rather than the reified value
- Grid can't use dynamically created lists :'(
  - deeper issue: `(...args) => ...` JS lambdas (stack, grid, concat, etc.) are too magical, but very useful

## Features

- LLM-assisted code generation
  - proof-of-concept whether Grok can generate code in Fluent
- good error reporting
  - show stack trace when `Print`ing `Error`
    - the deepest error first
  - red squigly line on bad code
- `Canvas` UI component for rapid tensor creation
- loading tensors (and models) from URLs
  - `fetchFromUrl` / `tensorLoad`
- first-class symbols
  - syntax for symbol literal – 'symbol'
  - (this is probably a subset of syntax tree literal)
- first-class signals
  - so I don't have to wrap everything – `$({ a() + b() })` -> `a + b`
  - how to reference a signal object? if justs a mention of a symbol would automatically retrieve its value, how can i pass an object without looking inside of it? could first-class symbols help here?
    - if signal would be "hot", so `a + b` would mean `a() + b()`, then `{ a }` would be a reference to signal, which could be passed around
    - with "hot" signal, `:=` would be impossible to implement
- string interpolation
  - a: "Hello `user`!"
- literal for syntax tree
  - a: `1 + 1 = 2`

## Other

- https://ceur-ws.org/Vol-1785/M2.pdf
  - may be relevant
- https://medium.com/data-science/why-not-mse-as-a-loss-function-for-logistic-regression-589816b5e03c
  - might be good demo example
- https://einx.readthedocs.io/en/stable/faq/universal.html
  - think about benefits of alternative universal notation
- https://arxiv.org/pdf/2303.15784
  - ideograph – inspiration for visualization of syntax tree

*/

import { grammar, type ActionDict } from "ohm-js";
import { toAST as ohmToAST } from "ohm-js/extras";
import * as tf from "@tensorflow/tfjs";
// for fancy signals
// https://www.npmjs.com/package/@preact-signals/utils
import { signal, Signal, computed, effect } from "@preact/signals-core"
import { useSignals, useSignal, useComputed } from '@preact/signals-react/runtime';
import { ErrorBoundary } from 'react-error-boundary';
import { type BeforeMount, type Monaco, type OnMount, Editor } from '@monaco-editor/react';
import { type editor } from "monaco-editor";
// @ts-ignore
import { renderMarkdown } from "monaco-editor/esm/vs/base/browser/markdownRenderer.js"
import dedent from "ts-dedent";
import { useEffect, type JSX, isValidElement, useRef } from "react";
import { createRoot } from "react-dom/client"
import { Base64 } from 'js-base64'
import { type Annotations } from "plotly.js"

// @ts-ignore
const Plot = (await import("react-plotly.js")).default.default as Plot

import "./index.css"

tf.setBackend('cpu');
// tf.setBackend('webgl');

// MARK: Parse

const colors = {
  number: "0A81EF",
  identifier: "30CD50",
  operator: "F238E3",
  string: "D8D8D8",
  delimiter: "F94C4A",
  comment: "444444",
}

const COLORS: editor.ITokenThemeRule[] = [
  ...Object.entries(colors).map(([key, value]) => ({ token: key, foreground: value })),
  { token: "comment.md", foreground: "7F7F7F" },
  { token: "keyword.md", foreground: "7F7F7F" },
]

function getColorForSyntaxTreeNode(code: string) {
  let color: string
  switch (true) {
    case numberRegexp.test(code):
      color = colors["number"]
      break;
    case stringRegexp.test(code):
      color = colors["string"]
      break;
    case identifierRegexp.test(code):
      color = colors["identifier"]
      break;
    case operatorRegexp.test(code):
      color = colors["operator"]
      break;
    default:
      color = "#D4D4D4"
  }

  return "#" + color
}

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
};

const RESERVED_SYMBOLS = "[|,{}()\\[\\];]"

const IDENTIFIER_RANGES: Record<string, [string, string]> = {
  MATHEMATICAL_ALPHANUMERIC_SYMBOLS: [String.fromCodePoint(0x1D400), String.fromCodePoint(0x1D7FF)], // Mathematical Alphanumeric Symbols
}
const identifierRegexp = /(?:\p{L})[\p{L}\p{N}\-]*/u

const numberRegexp = /-?\d[\d_]*(?:\.\d[\d_]*)?/
const stringRegexp = /"(?:[^"\\]|\\.)*"/

const getSymbolRange = (range: [string, string]) => {
  // Convert "\u0021-\u007E" to "\u0021".."\u007E"
  const [start, end] = range;
  return `"${start}".."${end}"`;
}

const getSymbolsRange = (symbolRangeObject: Record<string, [string, string]>) => {
  // console.log("getSymbolsRange", symbolRangeObject);
  return `(${Object.values(symbolRangeObject).map(range => getSymbolRange(range)).join(" | ")})`;
};

const operatorRegexp = /[^\p{L}\p{L}]/u

const delimiterRegexp = new RegExp(RESERVED_SYMBOLS, "u")

// MARK: Grammar

const CodeGrammar = String.raw`
Fluent {
  Program
    = ListOf<Expr, ","> ","?

  Expr
    = Operation

  Operation
    = 
    | (Expr | Atom) Atom Atom --infix
    | Atom Atom --prefix
    | Atom

  Hack
    = (Atom #(~space) (NestedExpr | List))
    
  Atom
    = Number | Hack | Lambda | NestedExpr | List | Symbol | String | Code | Tensor | Null
    
  NestedExpr
    = "(" Expr ")"

  Lambda
    = "{" (LambdaArgs "|")? Program "}"
  LambdaArgs
    = ListOf<Symbol, ",">

  List
    = "(" NonemptyListOf<Expr, ","> ","? ")" --multi
    | "(" Expr "," ")" --single
    | "()" --empty

  Number          = number
  Tensor          = "[" ListOf<Expr, ","> ","? "]"
  Null            = "◌"
  Symbol          = identifier | ${getSymbolsRange(IDENTIFIER_RANGES)} | operator
  identifier      = &letter (alnum | "-")+
  number
    = "-"? digitGroup ("." digitGroup?)?
  digitGroup
    = digit ("_"? digit)*
  String          = #("\"" (~"\"" any)* "\"")
  Code            = "${"`"}" Program "${"`"}" | "${"`"}" Code "${"`"}"
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

const syntaxTreeMapping: ActionDict<SyntaxTreeNode> = {
  Program(expressions, _) {
    return {
      type: "Program",
      content: expressions.toAST(this.args.mapping),
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
            value: [left.toAST(this.args.mapping), right.toAST(this.args.mapping)]
          },
        }
      },
      origin: getLocationOrigin(this),
    }
  },
  Hack(left, right) {
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
  List_empty(_) {
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
    // console.log("Code node", value.sourceString);
    return {
      type: "Code",
      content: {
        value: value.toAST(this.args.mapping) as SyntaxTreeNode,
      },
      origin: getLocationOrigin(this),
    }
  },
} as const;

const CodeParse = (program: string): SyntaxTreeNode => {
  const matchResult = CodeGrammarCompiled.match(program);

  if (matchResult.succeeded()) {
    // ohmToAST is untyped, so we need to cast it to our SyntaxTreeNode type
    const result = ohmToAST(matchResult, syntaxTreeMapping) as SyntaxTreeNode;
    console.log("Parsed syntax tree:", result);
    return result
  } else {
    return {
      type: "Error",
      content: matchResult.message ?? "Unknown parse error",
      origin: {
        source: program,
        start: { line: 1, column: 1 },
        end: { line: 1, column: program.length + 1 }
      }
    };
  }
}

// MARK: Evaluate

type Value = tf.Tensor | Function | Signal<Value> | Error | string | symbol | null | Value[]
type CurrentScope = Record<string, Value>

function evaluateSyntaxTreeNode(node: SyntaxTreeNode, env: CurrentScope): Value {

  if (node.type === "Error") {
    console.error("Error evaluating node:", node.content);
    return new Error(node.content)
  }

  if (node.type === "Program") {
    const values: Array<Value> = node.content.map((e) => evaluateSyntaxTreeNode(e, env))
    // last value is the result of the program
    return values[values.length - 1] ?? null
  }

  if (node.type === "Number") {
    return tf.scalar(node.content.value)
  }

  if (node.type === "Tensor") {
    const values = node.content.value.map((e) => evaluateSyntaxTreeNode(e, env))
    if (values.length === 0) {
      return tf.tensor([])
    }
    try {
      // let it crash when stacking shit together
      // @ts-ignore
      return tf.stack(values)
    } catch (e) {
      console.error("Error stacking tensors:", e, values);
      // @ts-ignore
      return new Error("Error stacking tensors: " + e.message)
    }
  }

  if (node.type === "Symbol") {
    return Symbol.for(node.content.value).resolve(env)
  }

  if (node.type === "String") {
    return dedent(node.content.value)
  }

  if (node.type === "List") {
    return node.content.value.map((e) => evaluateSyntaxTreeNode(e, env))
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

      return evaluateSyntaxTreeNode(node.content.expr, localEnv)
    }

    fn.toString = () => node.origin.source

    return fn
  }

  if (node.type === "Operation") {
    const fn = evaluateSyntaxTreeNode(node.content.operator, env)
    const args = evaluateSyntaxTreeNode(node.content.args, env) as Value[]

    return safeApply(fn, args, env)
  }

  if (node.type === "Code") {
    console.log("Evaluating embedded code:", node.content.value);
    // @ts-ignore
    return PrettyPrintSyntaxTree(node.content.value)
  }

  return null
}

tf.Tensor.prototype.toString = function () {
  return `${getAsSyncList(this)}`
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
  // console.log("resolve", this, env)

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
  console.log("mutating", this, env, value)

  const s = this[Symbol.toPrimitive]("symbol")
  const k = Symbol.keyFor(s) ?? ""

  Object.assign(env, {
    [k]: value,
  })
}

// Extend SymbolConstructor to include reverseResolve
declare global {
  interface SymbolConstructor {
    reverseResolve(env: CurrentScope, value: any): symbol | undefined;
  }
}

// hierarchically traverse the environment to find the symbol that resolves to the given value
Symbol.reverseResolve = function (env: CurrentScope, value: any): symbol | undefined {
  // console.log("reverseResolve", env, value)

  const symbolKeys = Object.getOwnPropertyNames(env);
  for (const key of symbolKeys) {
    // console.log("checking symbol", key, "with value", env[key], "against", value);

    if (env[key] === value) {
      // console.log("found symbol", key, value)
      return Symbol.for(key);
    }
  }

  // traverse recursively parents

  console.log(env)
  const prototypeEnv = Object.getPrototypeOf(env);
  if (prototypeEnv) {
    // console.log("checking prototype", prototypeEnv);
    const resolved = Symbol.reverseResolve(prototypeEnv, value);
    if (resolved) {
      return resolved;
    }
  }

  return undefined;
}

const reify = (v: Value, env: CurrentScope): Value => {
  if (typeof v === "symbol") {
    // @ts-ignore
    const resolved = v.resolve(env)

    if (resolved === v) {
      // console.log("unresolved symbol", v, Object.keys(env))
      return v
    }

    if (typeof resolved === "symbol") {
      console.log("circular reference", v, resolved)
      return reify(resolved, env)
    }

    return resolved
  }

  return v
}

function safeApply(fn: Value, args: Value[], env: CurrentScope): Value {
  try {
    console.log("safeApply", fn, args, env)

    let fnValue: Value = reify(fn, env);
    let argsValue: Value[] = args;

    // @ts-ignore
    if (!fnValue.receivesNonreifiedSymbols) {
      console.log("reifying args", args)
      argsValue = args.map(arg => reify(arg, env))
      console.log("reifying args done", argsValue)
    }

    const errorArgs = argsValue.filter(a => a instanceof Error)
    if (errorArgs.length > 0) {
      throw errorArgs[0]
    }

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

    return fnValue.apply(env, argsValue)
  } catch (e) {
    return e as Error
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
      console.log("candidateResult", candidateResult)
    } catch (e) {
      console.log("error", e)
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

function getAsSyncList(value: tf.Tensor) {
  if (value instanceof tf.Tensor) {
    return value.arraySync()
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
  console.log("Evaluating program:", program, this);
  return evaluateProgramWithScope(program, this);
}

type PrefixKey<K extends string, P extends string> =
  P extends '' ? K :
  K extends '' ? P :
  `${P}-${K}`;

type PrefixKeys<T extends object, Prefix extends string> = {
  [K in keyof T as PrefixKey<`${Extract<K, string>}`, Prefix>]: T[K];
};

// Usage in function:
function prefixKeys<P extends string, S extends object>(prefix: P, scope: S): PrefixKeys<S, P> {
  const prefixedScope = Object.fromEntries(Object.entries(scope).map(([k, v]) => [[prefix, k].filter(a => a.length > 0).join("-"), v]));
  console.log(prefixedScope);
  return prefixedScope as PrefixKeys<S, P>;
}

const FunctionIterate = async (fn: (index?: tf.Scalar) => void, iterations: tf.Scalar = tf.scalar(1)) => {
  if (!(typeof fn === "function" && iterations instanceof tf.Tensor)) {
    throw new Error("iterate(fn, iterations): fn must be a function and iterations must be a Tensor");
  }

  const maxIterations = getAsSyncList(iterations) as number

  setTimeout(() => {
    for (let i = 0; i < maxIterations; i++) {
      setTimeout(() => {
        return fn(tf.scalar(i))
      }, 0)
    }
  }, 0)

  return null
}

const SignalCreate = signal

const SignalRead = <T,>(s: Signal<T>) => {
  if (!(s instanceof Signal)) {
    return new Error(`'read': ${String(s)} is not a signal`)
  }
  return s.value
}

const SignalUpdate = <T,>(s: Signal<T>, v: T) => {
  if (s instanceof Signal) {
    s.value = v
    return
  }

  return new Error(`'update': ${s} is not a signal`)
}

const SignalComputed = computed
const SignalEffect = effect

const Reactive = (a: Value) => {
  if (typeof a === "function") {
    // @ts-ignore
    return SignalComputed(a)
  }
  return SignalCreate(a)
}

const SymbolAssign = function (this: CurrentScope, a: Value, b: Value) {

  console.log("SymbolAssign", a, b, this)

  if (typeof a === "symbol") {
    // if a is not defined in the current environment
    // if (!Object.getOwnPropertyNames(this).includes(Symbol.keyFor(a))) {
    //   // FIXME: this is should be scope where the symbol is defined not just the proto of this
    //   Object.getPrototypeOf(this)[Symbol.keyFor(a)] = b
    //   return a.resolve(this)
    // }
    a.assign(this, b)
    // return a
    return a.resolve(this)
  } else {
    return new Error(`Left side of assignment must be a symbol, got: ${String(a)}`)
    // reified symbol has a value attached to it
    // found this value in the environment traversing the prototype chain of env
    const aSymbol = Symbol.reverseResolve(this, a)

    if (typeof aSymbol === "symbol") {
      // aSymbol.assign(this, b)
      // console.log(Symbol.keyFor(aSymbol))
      this[Symbol.keyFor(aSymbol)] = b
      // console.log("SymbolAssign", aSymbol, b, this, this[Symbol.keyFor(aSymbol)])
      return aSymbol.resolve(this)
    }
  }

  return b
}

// @ts-ignore
SymbolAssign.receivesNonreifiedSymbols = true

const FunctionEvaluate = function (this: CurrentScope, fn: Value, args: Value[]) {
  return safeApply(fn, args, this)
}

const FunctionApply = function (this: CurrentScope, a: Value, b: Value): Value {
  const arg = a instanceof Array ? a : [a]
  return safeApply(b, arg, this)
}

const List = (...args: unknown[]) => {
  return args
}

const ListConcat = (...args: Value[]) => {
  return args.reduce((acc, arg) => {
    if (arg instanceof Array) {
      return acc.concat(arg)
    } else {
      console.log(arg)
      return acc.concat(new Error(`listConcat: argument is not an array: ${String(arg)}`))
    }
  }, [] as unknown[]
  )

  // does unwanted spread
  // return [].concat(args)
}

const ListLength = (a: unknown[]) => {
  return tf.scalar(a.length)
}

const ListGet = (a: any[], b: tf.Scalar) => {
  if (!Array.isArray(a)) {
    return new Error("list-get: a must be an array")
  }

  const index = getAsSyncList(b) as number
  if (index < -a.length || index >= a.length) {
    return new Error(`Index out of bounds: ${index} for list of length ${a.length}`)
  }

  return a.at(index)
}

const ListMap = (a: any[], fn: (value: any, index: tf.Scalar) => any) => {
  if (typeof fn !== "function") {
    return new Error("ListMap: `fn` must be a function")
  }

  if (!Array.isArray(a)) {
    return new Error("ListMap: `a` must be a list")
  }

  return a.map((value, index) => {
    return FunctionEvaluate(fn, [value, TensorScalar(index)])
  })
}

const ListReduce = (a: any[], fn: (acc: any, value: any) => any, initialValue?: any) => {
  if (typeof fn !== "function") {
    return new Error("list-reduce: fn must be a function")
  }

  if (initialValue === undefined) {
    return a.reduce(fn)
  }

  return a.reduce(fn, initialValue)
}

const Tensor = tf.Tensor
const TensorScalar = tf.scalar
const TensorStack = (...args: tf.Tensor[]) => tf.stack(args)
const TensorUnstack = tf.unstack
const TensorConcat = (...args: tf.Tensor[]) => tf.concat(args)
const TensorTile = (a: tf.Tensor, reps: tf.Tensor) => {
  const repsList = getAsSyncList(reps) as number[]
  return tf.tile(a, repsList)
}

const TensorAdd = tf.add
const TensorSubtract = tf.sub
const TensorMultiply = tf.mul
const TensorDivide = tf.div
const TensorPower = tf.pow
const TensorRoot = (a: tf.Tensor, b: tf.Tensor) => TensorPower(b, TensorReciprocal(a))
const TensorRemainder = tf.mod
const TensorMaximum = tf.maximum
const TensorMinimum = tf.minimum

const TensorLess = tf.less
const TensorGreater = tf.greater
const TensorLessEqual = tf.lessEqual
const TensorGreaterEqual = tf.greaterEqual
const TensorEqual = tf.equal
const TensorNotEqual = tf.notEqual

const TensorSine = tf.sin
const TensorCosine = tf.cos
const TensorTangent = tf.tan
const TensorSineHyperbolic = tf.sinh
const TensorCosineHyperbolic = tf.cosh
const TensorTangentHyperbolic = tf.tanh
const TensorSineHyperbolicInverse = tf.asin
const TensorCosineHyperbolicInverse = tf.acos
const TensorTangentHyperbolicInverse = tf.atan

// TensorReduce(a, 0, +)
const TensorSum = tf.sum
// TensorReduce(a, 1, *)
const TensorProduct = tf.prod
const TensorMean = tf.mean
const TensorMin = tf.min
const TensorMax = tf.max

const TensorNegate = tf.neg
const TensorAbsolute = tf.abs
const TensorSign = tf.sign
const TensorLogarithm = tf.log
const TensorExponential = tf.exp
const TensorReciprocal = tf.reciprocal
const TensorRound = tf.round
const TensorCeil = tf.ceil
const TensorFloor = tf.floor

const TensorGradient = tf.grad
const TensorTranspose = tf.transpose
const TensorIdentity = (a: tf.Tensor) => {
  const size = getAsSyncList(a) as number
  return tf.eye(size)
}

const TensorRange = (a: tf.Tensor, b: tf.Tensor, c: tf.Tensor) => {
  const start = getAsSyncList(tf.cast(a, "int32")) as number
  const stop = getAsSyncList(tf.cast(b, "int32")) as number
  return tf.range(start, stop)
}

const TensorLinearSpace = (range: tf.Tensor, steps: tf.Tensor, c: tf.Tensor) => {
  const [start, stop] = getAsSyncList(range) as [number, number]
  const num = getAsSyncList(steps) as number
  return tf.linspace(start, stop, num)
}

const TensorReshape = (a: tf.Tensor, b: tf.Tensor) => {
  if (b !== undefined) {
    return tf.reshape(a, getAsSyncList(b) as number[])
  }

  return tf.tensor(a.shape)
}

const TensorMatrixMultiply = (a: tf.Tensor, b: tf.Tensor) => tf.matMul(a, b, false, false)
const TensorDotProduct = tf.dot

const TensorLength = (a: tf.Tensor, b?: tf.Tensor) => {
  if (b !== undefined) {
    return tf.scalar(a.shape[getAsSyncList(b) as number])
  }

  return tf.scalar(a.shape[0])
}

const TensorShape = (a: tf.Tensor) => tf.tensor(a.shape)

const TensorGather = (a: tf.Tensor, b: tf.Tensor) => {
  const indices = tf.cast(b, "int32")
  // (__): { L, i | L _ tensorWhere(i < 0, i + #(L), i) },
  return tf.gather(a, indices)
}

const TensorWhere = tf.where
const TensorIsNaN = tf.isNaN

// const tensorVariableAssignOriginal = Variable.prototype.assign

// Variable.prototype.assign = function (newValue: tf.Tensor<tf.Rank>) {
//   update(this.signal, newValue);
//   tensorVariableAssignOriginal.bind(this)(newValue)
// }

const TensorVariable = (a: tf.Tensor) => {
  // const s = signal(a)
  const variableTensor = tf.variable(a);
  // @ts-ignore
  // variableTensor.signal = s;
  return variableTensor
}

const TensorAssign = (a: tf.Variable, b: tf.Tensor) => {
  return a.assign(b)
}

const TensorOptimizationAdam = (a: tf.Tensor, b: tf.Variable[]) => {
  const optimizer = tf.train.adam(getAsSyncList(a) as number, 0.9, 0.999, 1e-8)
  return (fn: () => tf.Scalar) => optimizer.minimize(fn, true, b)
}

const TensorOptimizationSgd = (a: tf.Tensor) => {
  const optimizer = tf.train.sgd(getAsSyncList(a) as number)
  return (fn: () => tf.Scalar) => optimizer.minimize(fn, true)
}

const TensorRandomNormal = (a: tf.Tensor) => tf.randomStandardNormal(getAsSyncList(a) as number[])

// const String = (a: unknown) => `${a.toString()}`
const StringConcat = (...args: any[]) => "".concat(...args)
const StringLength = (a: string) => tf.scalar(a.length)
const StringSerialize = (s: string) => Base64.encodeURI(s)
const StringDeserialize = (s: string) => Base64.decode(s)

const Button = (label?: string | Signal<string>, onClick?: () => void) => {
  return computed(() => {
    const disabled = (typeof onClick !== "function")
    const labelValue = typeof label === "string" ? label : label instanceof Signal ? label.value : "Button"
    return <button className="bg-neutral-900 hover:bg-neutral-800 active:bg-neutral-700 rounded-xl border border-neutral-400 hover:border-neutral-300 active:border-neutral-200 disabled:border-neutral-800 disabled:cursor-not-allowed p-2 overflow-hidden" onClick={onClick} disabled={disabled}>{labelValue}</button>
  })
}

const convertTextToHTML = async (value: string) => {
  const result = await renderMarkdown({ value }, {
    inline: false,
    codeBlockRenderer: colorizeCodeAsync,
  })

  return result.element as HTMLElement
}

const colorizeCodeAsync = async (language: string, value: string) => {
  const colorize = (await getEditor()).colorize
  const e = document.createElement("div")
  e.className = "overflow-hidden"
  e.style.fontFamily = "monospace"
  try {
    e.innerHTML = await colorize(value, language)
  } catch (error) {
    e.textContent = value
  }
  return e
}

const NativeDOMElement = ({ fn }: { fn: () => Promise<HTMLElement> }) => {
  const ref = useRef<HTMLDivElement>(null)

  useEffect(() => {

    async function mount() {
      if (ref.current) {
        const element = await fn()
        ref.current.appendChild(element)
        return () => {
          ref.current?.removeChild(element)
        }
      }
    }

    mount()

    return () => {
      if (ref.current) {
        while (ref.current.firstChild) {
          ref.current.removeChild(ref.current.firstChild)
        }
      }
    }
  }, [fn])

  return <div ref={ref}></div>
}

const Text = (value: string) => {
  const fn = () => { console.log("rendering"); return convertTextToHTML(value) }
  return <div className="prose prose-neutral prose-invert"><NativeDOMElement fn={fn} /></div>
}

const TextEditor = (editedValue: Signal<string>) => {
  return SignalComputed(() =>
    <div className="grid">
      <textarea
        value={editedValue.value}
        onChange={(e) => SignalUpdate(editedValue, e.target.value)}
        className="block bg-neutral-800 focus:bg-neutral-900 rounded-xl border border-neutral-600 hover:border-neutral-500 focus:border-neutral-300 outline-none p-2 field-sizing-content"
        // @ts-ignore
        style={{ fieldSizing: "content" }}
        rows={1}
      />
    </div>
  )
}

const Slider = (editedValue: Signal<tf.Tensor>) => {
  return SignalComputed(() => {
    const valueAsList = getAsSyncList(editedValue?.value) as number

    return (
      <div className="grid">
        <input
          type="range"
          min={0}
          max={1}
          step={0.01}
          value={valueAsList}
          onChange={(e) => SignalUpdate(editedValue, tf.scalar(parseFloat(e.target.value)))}
          className="bg-neutral-900 focus:bg-neutral-800 rounded-xl border border-neutral-800 hover:border-neutral-700 focus:border-neutral-600 outline-none p-2 w-full h-2 cursor-pointer dark:bg-gray-700 place-self-center"
        />
      </div>
    )
  })
}

const Grid = (cols: tf.Tensor, rows: tf.Tensor) => {
  let gridTemplateColumns = ""
  let gridTemplateRows = ""

  const colsValue = getAsSyncList(cols)
  switch (cols.rank) {
    case 0:
      gridTemplateColumns = `repeat(${colsValue}, minmax(auto, 1fr))`
      break;

    case 1:
      gridTemplateColumns = (colsValue as number[]).map((v) => `minmax(0, ${v}fr)`).join(" ")
      break;
    default:
      console.log("zle je")
      break;
  }

  if (rows !== undefined) {
    const rowsValue = getAsSyncList(rows)


    switch (rows.rank) {
      case 0:
        gridTemplateRows = `repeat(${rowsValue}, minmax(auto, 1fr))`
        break;

      case 1:
        gridTemplateRows = (rowsValue as number[]).map((v) => `minmax(0, ${v}fr)`).join(" ")
        break;
      default:
        console.log("zle je")
        break;
    }
  }

  return (...args: any[]) => (
    <div className={`grid gap-2 overflow-scroll h-full`} style={{ gridTemplateColumns, gridTemplateRows }}>
      {/* @ts-ignore */}
      {args.map(WrapWithPrintIfNotReactElement)}
    </div>
  )
}

function WrapWithPrintIfNotReactElement(child: any) {
  if (isValidElement(child)) {
    return child;
  } else {
    return Print(child);
  }
}

const Fetch = (url: string) => {
  const s = SignalCreate<Value>(null)

  fetch(url)
    .then(response => response.text())
    .then(data => {
      s.value = data
    })
    .catch(error => {
      s.value = new Error(`Error fetching data: ${error.message}`)
    });

  return s
}

// A simple JavaScript implementation for reading SafeTensors files.
// This function takes an ArrayBuffer (e.g., from fs.readFileSync().buffer in Node.js or fetch().arrayBuffer() in browser)
// and returns an object with tensor names as keys and objects containing dtype, shape, and data (as TypedArray) as values.
// Note: For 'F16' and 'BF16', the data is returned as Uint16Array (raw bits); you may need additional conversion to floats if required.
// Throws errors on unsupported dtypes or invalid formats.


function LoadSafeTensors(arrayBuffer: ArrayBuffer) {
  if (!(arrayBuffer instanceof ArrayBuffer)) {
    throw new Error('Input must be an ArrayBuffer');
  }

  const dataView = new DataView(arrayBuffer);
  let offset = 0;

  // Read header length: 8-byte little-endian uint64
  const headerLen = Number(dataView.getBigUint64(offset, true));
  offset += 8;

  if (headerLen === 0 || offset + headerLen > arrayBuffer.byteLength) {
    throw new Error('Invalid header length');
  }

  // Extract header bytes and decode as JSON
  const headerArray = new Uint8Array(arrayBuffer, offset, headerLen);
  const decoder = new TextDecoder('utf-8');
  const headerString = decoder.decode(headerArray);
  const header = JSON.parse(headerString);

  offset += headerLen;

  const tensors = {};

  for (const key in header) {
    if (key === '__metadata__') continue; // Skip optional metadata

    const info = header[key];
    if (!info.dtype || !info.shape || !info.data_offsets) {
      throw new Error(`Invalid tensor info for key: ${key}`);
    }

    const { dtype, shape } = info;
    const [start, end] = info.data_offsets;

    if (start < 0 || end <= start || offset + end > arrayBuffer.byteLength) {
      throw new Error(`Invalid data offsets for tensor: ${key}`);
    }

    const tensorBuffer = arrayBuffer.slice(offset + start, offset + end);

    let data;
    switch (dtype) {
      case 'BOOL':
        data = new Uint8Array(tensorBuffer);
        break;
      case 'I8':
        data = new Int8Array(tensorBuffer);
        break;
      case 'U8':
        data = new Uint8Array(tensorBuffer);
        break;
      case 'I16':
        data = new Int16Array(tensorBuffer);
        break;
      case 'U16':
        data = new Uint16Array(tensorBuffer);
        break;
      case 'I32':
        data = new Int32Array(tensorBuffer);
        break;
      case 'U32':
        data = new Uint32Array(tensorBuffer);
        break;
      case 'I64':
        data = new BigInt64Array(tensorBuffer);
        break;
      case 'U64':
        data = new BigUint64Array(tensorBuffer);
        break;
      case 'F16':
      case 'BF16':
        data = new Uint16Array(tensorBuffer); // Raw bits; convert to float if needed
        break;
      case 'F32':
        data = new Float32Array(tensorBuffer);
        break;
      case 'F64':
        data = new Float64Array(tensorBuffer);
        break;
      default:
        throw new Error(`Unsupported dtype: ${dtype}`);
    }

    // Verify the data length matches shape and dtype size
    const expectedSize = shape.reduce((a, b) => a * b, 1);
    const byteSize = data.byteLength / data.BYTES_PER_ELEMENT;
    if (byteSize !== expectedSize) {
      throw new Error(`Data size mismatch for tensor: ${key}`);
    }

    tensors[key] = { dtype, shape, data };
  }

  return tensors;
}

function LoadTensorFromImageUrl(url: string): Signal<tf.Tensor> {
  const s = SignalCreate<tf.Tensor | null>(null);

  const imgElement = document.createElement('img');
  imgElement.crossOrigin = "anonymous";
  imgElement.src = url;

  imgElement.onload = () => {
    s.value = tf.browser.fromPixels(imgElement)
  }

  return s
}


const LoadSafeTensorFromURL = (url?: string) => {
  if (!url) {
    return new Error("loadSafeTensorFromURL: url is required")
  }
  if (typeof url !== "string") {
    return new Error("loadSafeTensorFromURL: url must be a string")
  }
  const s = SignalCreate("Loading tensors...");

  fetch(url)
    .then(res => res.arrayBuffer())
    .then(buffer => {
      const tensors = LoadSafeTensors(buffer);

      const mu = Object.entries(tensors).map(([key, value]) => {
        // @ts-ignore
        return [key, tf.tensor(value.data, value.shape, "float32")]
      })

      console.log(tensors);
      return mu
    })
    .then((a) => {
      // @ts-ignore
      s.value = a
    })

  return s
}

const Null = null


// MARK: Environment

const DefaultEnvironment = {
  [Symbol.keyFor(Symbol.for("Null"))!]: Null,
  [Symbol.keyFor(Symbol.for("Documentation"))!]: Text(Documentation),

  CodeParse,
  CodeEvaluate,
  CodePrint: PrettyPrintSyntaxTree,

  FunctionIterate,
  FunctionCascade,
  FunctionEvaluate,
  FunctionApply,

  // MARK: Signals
  Reactive,
  SignalCreate,
  SignalComputed,
  SignalRead,
  SignalUpdate,
  SignalEffect,

  // MARK: Tensor operations

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
  TensorSineHyperbolic,
  TensorCosineHyperbolic,
  TensorTangentHyperbolic,
  TensorSineHyperbolicInverse,
  TensorCosineHyperbolicInverse,
  TensorTangentHyperbolicInverse,

  TensorSum,
  TensorProduct,
  TensorMean,
  TensorMin,
  TensorMax,

  TensorNegate,
  TensorAbsolute,
  TensorSign,
  TensorLogarithm,
  TensorExponential,
  TensorReciprocal,
  TensorRound,
  TensorCeil,
  TensorFloor,

  TensorGradient,

  TensorTranspose,
  TensorRange,
  TensorLinearSpace,
  TensorReshape,
  TensorLength,
  TensorShape,
  TensorGather,
  TensorWhere,
  TensorIsNaN,
  TensorIdentity,

  TensorVariable,
  TensorAssign,
  TensorOptimizationAdam,
  TensorOptimizationSgd,
  TensorRandomNormal,

  TensorMatrixMultiply,
  TensorDotProduct,

  // MARK: List operations

  List,
  ListLength,
  ListConcat,
  ListGet,
  ListMap,
  ListReduce,
  // ListGather: { a, b | b ListMap { i | a ListGet i } },

  // MARK: String operations

  String,
  StringConcat,
  StringLength,

  // MARK: Prelude

  "◌": Null,
  "null": Null,

  // SymbolAssign(:, SymbolAssign)
  ":": SymbolAssign,
  // (.): FunctionApply
  ".": FunctionApply,
  // (⟳): FunctionIterate
  "⟳": FunctionIterate,
  // (@): FunctionEvaluate
  "@": FunctionEvaluate,

  // (⍴): TensorReshape
  "⍴": TensorReshape,
  // (⌈): TensorMaximum
  // max : { a,b | [a,b] @ (a < b) }
  "⌈": TensorMaximum,
  // (⌊): TensorMinimum
  // min : { a,b | [a,b] @ (a > b) }
  "⌊": TensorMinimum,
  // (_): TensorGather
  "_": TensorGather,
  // (#): TensorLength
  "#": TensorLength,
  // (+): multiDispatch(TensorAdd, TensorAbsolute)
  "+": (a: tf.Tensor, b: tf.Tensor) => {
    if (b === undefined) {
      return TensorAbsolute(a)
    }
    return TensorAdd(a, b)
  },
  // (-): FunctionCascade(TensorSubtract, TensorNegate)
  "-": (a: tf.Tensor, b: tf.Tensor) => {
    if (b === undefined) {
      return TensorNegate(a)
    }
    return TensorSubtract(a, b)
  },
  // (*): FunctionCascade(TensorMultiply, TensorSign)
  "*": (a: tf.Tensor, b: tf.Tensor) => {
    if (b === undefined) {
      return TensorSign(a)
    }
    return TensorMultiply(a, b)
  },
  // (×): FunctionCascade(TensorMultiply, TensorSign)
  "×": (a: tf.Tensor, b: tf.Tensor) => {
    if (b === undefined) {
      return TensorSign(a)
    }
    return TensorMultiply(a, b)
  },
  // (/): TensorDivide
  "/": TensorDivide,
  // (÷): TensorDivide
  "÷": TensorDivide,
  // (%): TensorRemainder
  "%": TensorRemainder,
  // (^): TensorPower
  "^": TensorPower,
  // (√): TensorRoot
  "√": TensorRoot,
  // (<): TensorLess 
  "<": TensorLess,
  // (>): TensorGreater
  ">": TensorGreater,
  // (≤): TensorLessEqual
  "≤": TensorLessEqual,
  // (<=): TensorLessEqual
  "<=": TensorLessEqual,
  // (≥): TensorGreaterEqual
  "≥": TensorGreaterEqual,
  // (>=): TensorGreaterEqual
  ">=": TensorGreaterEqual,
  // (=): TensorEqual,
  // "=": TensorEqual,
  // (≠): TensorNotEqual,
  "≠": TensorNotEqual,
  // (!=): TensorNotEqual,
  "!=": TensorNotEqual,

  // (:=): TensorAssign
  // ":=": TensorAssign,

  // (~): TensorVariable
  "~": TensorVariable,

  // (::): TensorRange
  "::": TensorRange,

  // (∇): TensorGradient
  "∇": TensorGradient,
  // (Σ): TensorSum
  "Σ": TensorSum,
  // (Π): TensorProduct
  "Π": TensorProduct,
  // (μ): TensorMean
  // (μ): { a | ∑(a) / #(a) }
  "μ": TensorMean,

  // max: FunctionCascade(TensorMax, TensorMaximum),
  "max": (a: tf.Tensor, b?: tf.Tensor) => b === undefined ? TensorMax(a) : TensorMaximum(a, b),
  // min: FunctionCascade(TensorMin, TensorMinimum),
  "min": (a: tf.Tensor, b?: tf.Tensor) => b === undefined ? TensorMin(a) : TensorMinimum(a, b),

  "neg": TensorNegate,
  "abs": TensorAbsolute,
  "sign": TensorSign,
  "round": TensorRound,
  "floor": TensorFloor,
  "ceil": TensorCeil,

  "reciprocal": TensorReciprocal,
  "pow": TensorPower,
  "log": TensorLogarithm,
  "exp": TensorExponential,

  "sin": TensorSine,
  "cos": TensorCosine,
  "tan": TensorTangent,
  "sinh": TensorSineHyperbolic,
  "cosh": TensorCosineHyperbolic,
  "tanh": TensorTangentHyperbolic,
  "asinh": TensorSineHyperbolicInverse,
  "acosh": TensorCosineHyperbolicInverse,
  "atanh": TensorTangentHyperbolicInverse,

  // (?): { cond, choice | choice gather (1 sub cond) }
  // TODO: doesn't work for some reason, copying code to editor works
  // "?": evaluate(`{ cond, choice | choice tensorGather (1 tensorSubtract cond) }`),

  // MARK: Reactive + UI Components

  "$": Reactive,

  Button,
  Grid,
  Slider,

  Text,
  TextEditor,

  Code,
  CodeEditor,

  Fetch,
  LoadSafeTensorFromURL,
  LoadTensorFromImageUrl,
  // TensorEditor,
  // TensorCanvas,
  // ImageFromTensor,

  Print,
  PrettyPrint,
  Frame: (children: JSX.Element) => { return <Panel>{children}</Panel> },
  Page: (...args: JSX.Element[]) => (
    <div className="h-full max-h-full w-full max-w-full">{args}</div>
  ),
} as const


const treeNodePaddingVertical = 3
const treeNodePaddingHorizontal = 8

const fontSize = 14
const glyphWidth = fontSize * 0.6
const glyphHeight = fontSize * 1.2

const rectHeight = glyphHeight + (treeNodePaddingVertical * 2)
const rowMargin = 15
const rowHeight = rectHeight + rowMargin
const columnMargin = 5

type TreeNodeDescriptor = {
  node: {
    column: number,
    row: number,
    label: string,
    origin: Origin,
  },
  frame: {
    x: number,
    y: number,
    width: number,
    height: number,
  },
  text: {
    x: number,
    y: number,
    color: string,
    fontSize: number,
    content: string,
  }
}

const highlightedNode = SignalCreate(null as TreeNodeDescriptor | null);

function getColumnWidths(nodes: TreeNodeDescriptor[]): number[] {
  const columnWidths: number[] = [];
  for (const node of nodes) {
    const col = node.node.column - 1; // Adjust for 0-based index
    if (!columnWidths[col]) {
      columnWidths[col] = 0;
    }
    columnWidths[col] = Math.max(columnWidths[col], node.frame.width);
  }
  return columnWidths;
}


function TreeNodeLayout(label: string, column: number, row: number, origin: Origin): TreeNodeDescriptor {
  const textWidth = label.length * glyphWidth;
  const textHeight = glyphHeight;

  const rectWidth = textWidth + (2 * treeNodePaddingHorizontal);
  const rectHeight = textHeight + (2 * treeNodePaddingVertical);

  return {
    node: {
      column,
      row,
      label,
      origin
    },
    frame: {
      x: undefined,
      y: row * rowHeight,
      width: rectWidth,
      height: rectHeight,
    },
    text: {
      x: (rectWidth / 2),
      y: (rectHeight / 2),
      color: getColorForSyntaxTreeNode(label),
      fontSize,
      content: label
    }
  }
}

type TreeConnectionDescriptor = {
  start: {
    column: number,
    row: number,
    x: number,
    y: number,
  },
  end: {
    column: number,
    row: number,
    x: number,
    y: number,
  }
}

function TreeConnectionLayout(fromCol: number, fromRow: number, toCol: number, toRow: number) {
  return {
    start: {
      row: fromRow,
      column: fromCol,
      x: undefined,
      y: undefined,
    },
    end: {
      row: toRow,
      column: toCol,
      x: undefined,
      y: undefined,
    }
  }
}

function Tree(node: SyntaxTreeNode & { type: "Program" }): JSX.Element {

  const { nodes, connections } = layout(node);

  const columnWidths = getColumnWidths(nodes).map((w) => w + columnMargin);
  const columnOffsets = (() => {
    const [_, L] = columnWidths.reduce(([b, K], c) => [b + c, [...K, b + c]], [0, [0]])
    return L
  })()

  const width = columnOffsets[columnOffsets.length - 1];
  const height = (Math.max(...nodes.map(n => n.node.row))) * rowHeight;

  const renderedNodes = nodes.map((node) => {
    node.frame.x = columnOffsets[node.node.column - 1] + (columnWidths[node.node.column - 1] / 2);
    node.frame.y = (node.node.row - 1) * rowHeight;

    return TreeNode(node);
  });

  const renderedConnections = connections.map((c) => {
    c.start.x = columnOffsets[c.start.column - 1] + (columnWidths[c.start.column - 1] / 2);
    c.start.y = (c.start.row - 1) * rowHeight + rectHeight;
    c.end.x = columnOffsets[c.end.column - 1] + (columnWidths[c.end.column - 1] / 2);
    c.end.y = (c.end.row - 1) * rowHeight;

    return TreeConnection(c);
  })

  return (
    <svg style={{ width, height, shapeRendering: "optimizeSpeed" }} viewBox={`0 0 ${width} ${height}`} xmlns="http://www.w3.org/2000/svg">
      {renderedConnections}
      {renderedNodes}
    </svg>
  )
}

function TreeConnection({ start, end }: TreeConnectionDescriptor): JSX.Element {
  const f = Math.sign(Math.abs(start.x - end.x));
  const h = Math.abs(start.y - end.y)
  const o = (h / 2) * f
  const key = `connection-${start.column}-${start.row}-${end.column}-${end.row}`

  return (
    <g key={key} fill="none" strokeWidth={1} className="stroke-neutral-600">
      <path
        d={`
          M${start.x} ${start.y}
          C ${start.x} ${end.y - o},
            ${start.x} ${end.y - o},
            ${start.x + o} ${end.y - o},
          L${end.x - o} ${end.y - o},
          C ${end.x} ${end.y - o},
            ${end.x} ${end.y - o},
            ${end.x} ${end.y}
            `}
      />
    </g>
  )
}

function TreeNode(node: TreeNodeDescriptor): JSX.Element {
  const key = `node-${node.node.column}-${node.node.row}`

  return (
    <g key={key} transform={`translate(${node.frame.x - node.frame.width / 2}, ${node.frame.y})`}>
      <rect
        //className="cursor-pointer fill-[#171717] hover:fill-[#101010] stroke-[0.5] hover:stroke-1"
        className="cursor-pointer fill-[transparent] hover:fill-[#101010] stroke-[0.5] hover:stroke-1"
        width={node.frame.width}
        height={node.frame.height}
        rx={node.frame.height / 4}
        stroke="rgba(255,255,255, 0.1)"
        onPointerOver={(e) => {
          highlightedNode.value = node
        }}
      />
      <text
        transform={`translate(${node.text.x}, ${node.text.y})`}
        textAnchor="middle"
        dominantBaseline="central"
        fill={node.text.color}
        style={{ fontSize: node.text.fontSize }}
        className={`font-mono text-center pointer-events-none`}
      >
        {node.text.content}
      </text>
    </g>
  )
}

function layout(tree: SyntaxTreeNode & { type: "Program" }): { nodes: TreeNodeDescriptor[], connections: TreeConnectionDescriptor[] } {

  function getLabel(node: SyntaxTreeNode) {
    if (node.type === 'Symbol') {
      return node.content.value;
    } else if (node.type === 'Number') {
      return node.content.value.toString();
    } else if (node.type === 'Operation') {
      return getLabel(node.content.operator) // .content.value;
    } else if (node.type === 'Tensor') {
      return '[]';
    } else if (node.type === 'Lambda') {
      return '{}';
    } else if (node.type === 'List') {
      return '()';
    } else {
      throw new Error(`Unknown type ${node.type}`);
    }
  }

  function getChildren(node: SyntaxTreeNode): SyntaxTreeNode[] {
    if (node.type === 'Program') {
      return node.content;
    } else if (node.type === 'Operation') {
      let children = node.content.args.content.value;
      return children;
    } else if (node.type === 'Tensor') {
      return node.content.value;
    } else if (node.type === 'Lambda') {
      return [{ type: 'List', content: { value: node.content.args }, origin: node.origin }, ...(node.content.expr as SyntaxTreeNode & { type: "Program" }).content];
    } else if (['Symbol', 'Number'].includes(node.type)) {
      return [];
    } else if (node.type === 'List') {
      return node.content.value;
    } else {
      throw new Error(`Unknown type ${node.type}`);
    }
  }

  function getFootprint(node: SyntaxTreeNode): Set<string> {
    const fp = new Set<string>();
    fp.add('0,0');
    const children = getChildren(node);
    let child_dc = 0;
    for (const child of children) {
      const child_fp = getFootprint(child);
      for (let pos of child_fp) {
        const [dr, dc] = pos.split(',').map(Number);
        fp.add(`${1 + dr},${child_dc + dc}`);
      }
      child_dc += 1;
    }
    return fp;
  }

  function layout(node: SyntaxTreeNode, row: number, col: number, nodes: TreeNodeDescriptor[], conns: TreeConnectionDescriptor[], used: Set<string>) {
    let label = getLabel(node);

    nodes.push(TreeNodeLayout(label, col, row, node.origin));
    used.add(`${row},${col}`);

    const children = getChildren(node);

    if (children.length > 0) {
      const child_row = row + 1;
      const child_base = col;
      for (let i = 0; i < children.length; i++) {
        let child = children[i];
        let tentative = child_base + i;
        let fp = getFootprint(child);
        while (Array.from(fp).some(pos => {
          let [dr, dc] = pos.split(',').map(Number);
          return used.has(`${child_row + dr},${tentative + dc}`);
        })) {
          tentative += 1;
        }
        conns.push(TreeConnectionLayout(col, row, tentative, child_row));

        layout(child, child_row, tentative, nodes, conns, used);
      }
    }
  }

  const nodes: TreeNodeDescriptor[] = [];
  const conns: TreeConnectionDescriptor[] = [];
  const used = new Set<string>();
  let current_col = 1;
  for (const stmt of tree.content) {
    const fp = getFootprint(stmt);
    let tentative_col = current_col;
    while (Array.from(fp).some(pos => {
      let [dr, dc] = pos.split(',').map(Number);
      return used.has(`${1 + dr},${tentative_col + dc}`);
    })) {
      tentative_col += 1;
    }
    layout(stmt, 1, tentative_col, nodes, conns, used);
    current_col = tentative_col + 1;
  }

  return {
    nodes,
    connections: conns
  };
}

// MARK: Runtime

// MARK: PrettyPrint

const frameStyle = "rounded-xl border border-neutral-800 hover:border-neutral-700 p-2"

function Print(obj: Signal<unknown>) {
  return computed(() =>
    <Panel className="overflow-scroll">
      <ErrorBoundary fallback={<div>Something went wrong</div>}>
        {PrettyPrint(obj)}
      </ErrorBoundary>
    </Panel>
  )
}

function PrintFunction(fn: Function) {
  const sourceCode = computed(() => fn.toString())
  return Code(sourceCode)
}

function PrettyPrintSyntaxTree(node: SyntaxTreeNode & { type: "Program" }): JSX.Element {
  return Tree(node)
}

function PrettyPrint(obj: any): JSX.Element {
  // console.log("PrettyPrint", obj);

  if (obj === null || obj === undefined) {
    return <div className="font-extrabold text-3xl">◌</div>;
  }

  if (typeof obj === "string") {
    const color = COLORS.find(rule => rule.token === "string")?.foreground;
    return <div style={{ color: `#${color}` }}>"{obj}"</div>;
  }

  if (Array.isArray(obj)) {
    return (
      <div className="grid gap-2 rounded-xl">
        {obj.map((item, key) => <div key={key} className={`${frameStyle} !border-0 grid hover:bg-neutral-600 hover:bg-opacity-5`}>{PrettyPrint(item)}</div>)}
      </div>
    );
  }

  if (typeof obj === "function") {
    // @ts-ignore
    return PrintFunction(obj);
  }

  if (typeof obj === "object") {

    if (obj instanceof Promise) {
      return <>"! Promise"</>
    }

    if (obj instanceof Signal) {
      // return <div className="bg-neutral-900 focus:bg-neutral-800 rounded-xl border border-neutral-800 hover:border-neutral-700 focus:border-neutral-600 outline-none p-2">{PrettyPrint(obj.value)}</div>;
      return <div className="contents">{PrettyPrint(obj.value)}</div>;
    }

    if (isValidElement(obj)) {
      return obj;
    }

    if (obj instanceof tf.Tensor) {
      if (obj.rank === 0) {
        const color = COLORS.find(rule => rule.token === "number")?.foreground || "FFFFFF";
        const formattedNumber = (obj.arraySync()).toLocaleString("en-US", { minimumFractionDigits: 0, maximumFractionDigits: 6, useGrouping: true }).replace(/,/g, "_");
        return <span style={{ color: `#${color}` }}>{formattedNumber}</span>;
      }

      if (obj.rank === 1) {
        return (
          <div className="">
            <BarPlot data={obj} />
          </div>
        );
      }

      if (obj.rank === 2) {
        return (
          <HeatPlot data={obj} />
        );
      }

      if (obj.rank > 2) {
        console.warn("Tensor with rank > 2 is not supported for plotting", obj);
      }

      return (
        <div className="">
          <span className="font-bold">Tensor:</span> {obj.shape.join(", ")}<br />
          <BarPlot data={obj} />
        </div>
      );
    }

    if (obj instanceof Error) {
      console.log("Error object:", obj);
      return <div className="border rounded-lg border-red-700 bg-red-950 bg-opacity-30 px-2 py-1 text-red-700"><div>{obj.message}</div>{obj.cause ? <div>{PrettyPrint(obj.cause)}</div> : null}</div>;
    }

    return (
      <div className="">
        {Object.entries(obj).map(([key, value]) => (
          <div key={key}>
            <span className="font-bold">{key}:</span> {PrettyPrint(value)}
          </div>
        ))}
      </div>
    );
  }
  console.warn("Unsupported type for PrettyPrint:", obj);

  return <div className="text-violet-500">{String(obj)}</div>;
}

function Panel({ children, className }: { children: JSX.Element, className?: string }) {
  return (
    <div className={`${frameStyle} ${className}`}>
      {children}
    </div>
  )
}

// MARK: Plots

const BarPlot = ({ data }: { data: tf.Tensor }) => {
  return (
    <Plot
      data={[
        {
          y: data.as1D().arraySync(),
          type: 'bar',
        },
      ]}
      layout={{
        paper_bgcolor: 'transparent',
        plot_bgcolor: 'transparent',
        font: { color: '#D4D4D4', },
        xaxis: {
          automargin: true,
          // // remove 0.5, 1.5, 2.5, etc. from x-axis
          // // tickvals: data.as1D().arraySync().map((_, i) => i + 1),
          // tick0: 0,
          // dtick: 1,
          // //ticktext: data.as1D().arraySync().map((_, i) => i + 1),
          // maxallowed: 20,

          // ticks: 'outside',
          // tickcolor: '#444444',
          // ticklen: 5,
          // tickwidth: 1,
          // tickfont: { size: 12, color: '#D4D4D4' },
        },
        yaxis: {
          gridcolor: '#444444',
          automargin: true,
        },
        margin: {
          pad: 10,
          t: 0,
          l: 0,
          r: 0,
          b: 0
        }
      }}
      config={{
        staticPlot: true,
        // when responsive is true, the plot flashes in random position when updating
        responsive: false,
      }}
      style={{ width: '100%', height: "10em" }}
    />
  );
}

const HeatPlot = ({ data }: { data: tf.Tensor }) => {
  const z = getAsSyncList(data) as number[][];

  let annotations: Partial<Annotations>[] = [];

  if (data.shape.reduce((a, b) => a * b) < 20) {
    // @ts-ignore
    annotations = z.map((row, i) => {
      return row.map((value, j) => ({
        xref: "x1",
        yref: "y1",
        x: j,
        y: i,
        text: value.toFixed(2),
        font: { color: '#D4D4D4', shadow: "0px 0px 1px black" },
        showarrow: false,
      }));
    }
    ).flat();
  }

  return (
    <Plot
      data={[
        {
          z,
          type: 'heatmap',
          colorscale: 'Viridis',
          // colorscale: "Greys"
        },
      ]}
      layout={{
        margin: {
          pad: 10,
          t: 0,
          l: 0,
          r: 0,
          b: 0
        },
        paper_bgcolor: 'transparent',
        plot_bgcolor: 'transparent',
        font: { color: '#D4D4D4', },
        annotations,
        xaxis: {
          automargin: true,
          side: 'top',
        },
        yaxis: {
          gridcolor: '#444444',
          automargin: true,
          autorange: 'reversed',
        },
      }}
      config={{
        staticPlot: true,
        autosizable: true,
        responsive: true,
      }}
      style={{ width: '100%', height: '100%' }}
    />
  );
}


// MARK: Editor

const getEditorOptions: (type: "editable" | "readonly") => editor.IStandaloneEditorConstructionOptions = (type) => ({
  minimap: { enabled: false },
  wordWrap: "on",
  fontSize: 14,
  fixedOverflowWidgets: true,
  scrollBeyondLastLine: false,
  bracketPairColorization: {
    independentColorPoolPerBracketType: true,
    enabled: true,
  },
  unicodeHighlight: {
    ambiguousCharacters: false
  },
  padding: { top: 8, bottom: 8 }, // Corresponds to p-2
  automaticLayout: true,
  scrollbar: {
    alwaysConsumeMouseWheel: false
  },
  ...(type === "readonly" && ({
    readOnly: true, // Make the editor read-only
    contextmenu: false,
    lineNumbers: "off",
    stickyScroll: { enabled: false },
    overviewRulerBorder: false,
    minimap: { enabled: false },
    overviewRulerLanes: 0,
    folding: false,
    lineDecorationsWidth: 0,
    lineNumbersMinChars: 0,
    cursorBlinking: "solid",
    cursorStyle: "line-thin",
    cursorWidth: 0,
    selectionHighlight: false,
    hover: { enabled: false },
    quickSuggestions: false,
    parameterHints: { enabled: false },
    autoClosingBrackets: "never",
    renderLineHighlight: "none",
    renderLineHighlightOnlyWhenFocus: false,
    renderValidationDecorations: "off",
    renderWhitespace: "none",
    renderControlCharacters: false,
    matchBrackets: "never",
    fixedOverflowWidgets: true,
    glyphMargin: false,
    foldingStrategy: "indentation",
    foldingHighlight: false,
    scrollbar: {
      vertical: "hidden",
      horizontal: "hidden",
      useShadows: true,
    }
  }))
})

function Code(sourceCode: Signal<string>) {
  return SignalComputed(() => {
    return (
      // @ts-ignore
      <Panel className="!p-0 border-none overflow-hidden h-[5em]">
        {/* @ts-ignore */}
        <Editor
          onMount={editorOnMount}
          height="100%"
          defaultLanguage="fluent"
          theme="fluentThemeReadOnly"
          // @ts-ignore
          value={SignalRead(sourceCode)}
          onChange={(updatedSourceCode) => { SignalUpdate(sourceCode, updatedSourceCode) }}
          options={getEditorOptions("readonly")}
        />
      </Panel>
    )
  })
}

function CodeEditor(sourceCode: Signal<string>) {
  // console.log("CodeEditor")

  const height = SignalCreate("100%")

  return SignalComputed(() => {
    console.log("rerendering editor", SignalRead(height))

    return (
      // @ts-ignore
      <Editor
        beforeMount={editorBeforeMount}
        onMount={editorOnMount}
        // @ts-ignore
        height={SignalRead(height)}
        defaultLanguage="fluent"
        className={`${frameStyle} !p-0 overflow-hidden`}
        theme="fluentTheme"
        // @ts-ignore
        value={SignalRead(sourceCode)}
        onChange={(updatedSourceCode) => { SignalUpdate(sourceCode, updatedSourceCode) }}
        options={getEditorOptions("editable")}
      />
    )
  })
}

let EditorInstance: Monaco["editor"] | null = null

async function getEditor() {
  while (EditorInstance === null) {
    await new Promise((resolve) => setTimeout(resolve, 0));
  }
  return EditorInstance;
}

const editorBeforeMount: BeforeMount = (monaco) => {
  EditorInstance = monaco.editor

  monaco.languages.register({ id: "fluent" });

  monaco.languages.setMonarchTokensProvider("fluent", {
    defaultToken: "",
    unicode: true,
    tokenizer: {
      root: [
        [/;.*$/, { token: "comment" }],
        [numberRegexp, { token: "number" }],
        // https://github.com/microsoft/monaco-editor/issues/5026
        [/"/, { token: "string", next: "@string", /*nextEmbedded: 'markdown'*/ }],
        // [/-/, "operator"],
        [identifierRegexp, { token: "identifier" }],
        [delimiterRegexp, { token: "delimiter" }],
        [operatorRegexp, { token: "operator" }],
      ],
      string: [
        [/[^"]+/, { token: "string" }],
        [/"/, { /* token: '@rematch',*/ token: "string", next: '@pop', /*nextEmbedded: '@pop'*/ }],

      ],
    },
  });

  monaco.languages.setLanguageConfiguration("fluent", {
    // onEnterRules: [
    //   {
    //     beforeText: /^\s*[\(\{\[]/,
    //     action: { indentAction: monaco.languages.IndentAction.Indent }
    //   },
    //   {
    //     beforeText: /^\s*[\)\}\]]/,
    //     action: { indentAction: monaco.languages.IndentAction.Outdent }
    //   },
    //   {
    //     beforeText: /^\s*;/,
    //     action: { indentAction: monaco.languages.IndentAction.None }
    //   },
    // ],
    brackets: [
      ["{", "}"],
      ["[", "]"],
      ["(", ")"],
    ],
    autoClosingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")" },
      { open: '"', close: '"' },
    ],
    surroundingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")", },
      { open: '"', close: '"' },
    ],
    indentationRules: {
      increaseIndentPattern: /^\s*[\(\{\[]/,
      decreaseIndentPattern: /^\s*[\)\}\]]/,
    },
    // TODO: unify this with RESERVED_SYMBOLS
    wordPattern: /[^|,{}()\\[\\];]+/g,
    comments: {
      lineComment: ";",
    },
  });

  monaco.editor.defineTheme("fluentTheme", {
    base: "vs-dark",
    inherit: true,
    rules: COLORS,
    colors: {
      "editor.foreground": COLORS.find(rule => rule.token === "string")?.foreground ?? "FFFFFF",
      "editor.background": "#1C1C1C",
      "focusBorder": "#FF000000",
    },
  });

  monaco.editor.defineTheme("fluentThemeReadOnly", {
    base: "vs-dark",
    inherit: true,
    rules: COLORS,
    colors: {
      "editor.foreground": COLORS.find(rule => rule.token === "string")?.foreground ?? "FFFFFF",
      "editor.background": "#1C1C1C00",
      "focusBorder": "#FF000000",
    },
  });

  // monaco.editor.addKeybindingRule(
  //   {
  //     // Reindent lines with Ctrl + Shift + F
  //     keybinding: monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyF,
  //     command: "editor.action.reindentlines",
  //   },);

  monaco.editor.addKeybindingRule({
    keybinding: monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyF,
    command: "editor.action.formatDocument",
  });

  monaco.editor.addKeybindingRule({
    keybinding: monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyK,
    command: 'editor.action.quickCommand',
    when: 'editorTextFocus'
  });

  monaco.editor.addKeybindingRule({
    keybinding: monaco.KeyCode.F1,
    command: '-editor.action.quickCommand',
    when: 'editorTextFocus'
  });

  monaco.languages.registerCompletionItemProvider('fluent', {
    triggerCharacters: [""],
    provideCompletionItems: (model, position) => {
      const lineText = model.getLineContent(position.lineNumber);
      // how to recognize "+" and other symbols as a word?
      const word = model.getWordUntilPosition(position);

      // console.log("Text before cursor:", textBeforeCursor);
      // console.log("Word before cursor:", word.word,);

      return ({
        suggestions: Object.entries(DefaultEnvironment).map(([key, value], i) => {
          return ({
            label: `${key}`,
            kind: monaco.languages.CompletionItemKind.Value,
            insertText: `${key}`,
            documentation: `Function: ${key}`,
            detail: `${value}`,
            range: {
              startLineNumber: position.lineNumber,
              endLineNumber: position.lineNumber,
              startColumn: word.startColumn,
              endColumn: word.endColumn// + key.length,
            },
            insertTextRules: monaco.languages.CompletionItemInsertTextRule.KeepWhitespace
          })
        })
      })
    }
  });

  /*
const symbolSuggestions = [
  { label: '÷ division sign', insertText: '÷', detail: 'Division operator' },
  { label: '√ square root', insertText: '√', detail: 'Root function' },
  { label: '∑ summation', insertText: '∑', detail: 'Summation operator' },
  { label: '∏ product', insertText: '∏', detail: 'Product operator' },
  { label: '∞ infinity', insertText: '∞', detail: 'Infinity symbol' },
  { label: '± plus-minus sign', insertText: '±', detail: 'Plus-minus operator' },
  { label: '≠ not equal to', insertText: '≠', detail: 'Not equal operator' },
  { label: '≈ approximately equal to', insertText: '≈', detail: 'Approximately equal operator' },
  { label: '→ right arrow', insertText: '→', detail: 'Right arrow operator' },
  { label: '← left arrow', insertText: '←', detail: 'Left arrow operator' },
  { label: '↔ double arrow', insertText: '↔', detail: 'Double arrow operator' },
];

monaco.languages.registerCompletionItemProvider('fluent', {
  provideCompletionItems: (model, position) => {
    const lineText = model.getLineContent(position.lineNumber);
    const textBeforeCursor = lineText.substring(0, position.column - 1);

    let prefixStart = textBeforeCursor.lastIndexOf('\\');

    console.log("Prefix start:", prefixStart, "Text before cursor:", textBeforeCursor);


    const word = model.getWordUntilPosition(position);
    const range = { startLineNumber: position.lineNumber, endLineNumber: position.lineNumber, startColumn: word.startColumn, endColumn: word.endColumn };

    // remove the trigger character '\' from the model
    // model.pushEditOperations([], [{ range, text: "" }], () => []);

    return {
      suggestions: symbolSuggestions
        .filter(item => item.label.toLowerCase().includes(word.word.toLowerCase().replace('\\', ''))) // Filter post-\
        .map(item => ({
          label: item.label,
          kind: monaco.languages.CompletionItemKind.Text, // Or Snippet for advanced
          insertText: item.insertText,
          documentation: item.detail,
          range: { ...range, endColumn: range.endColumn + item.insertText.length }, // Adjust range to include inserted text
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
        })),
    };
  },
});

*/
}

const editorOnMount: OnMount = (editor, monaco) => {
  editor.addAction({
    id: "fluent-load-example",
    label: "Load example",

    // An optional array of keybindings for the action.
    // keybindings: [
    //   monaco.KeyMod.CtrlCmd | monaco.KeyCode.F10,
    //   // chord
    //   monaco.KeyMod.chord(
    //     monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyK,
    //     monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyM
    //   ),
    // ],

    // A precondition for this action.
    // precondition: null,

    // A rule to evaluate on top of the precondition in order to dispatch the keybindings.
    // keybindingContext: null,
    contextMenuGroupId: "navigation",
    contextMenuOrder: 1.5,

    run: function (editor) {
      // TODO: show dropdown with examples to choose from
      const exampleName = "linear-regression"
      editor.getModel()?.setValue(getExample(exampleName))
    },
  });

  editor.addAction({
    id: "fluent-save-example",
    label: "Save example",
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS],
    run: function (editor) {
      const code = editor.getModel()?.getValue() ?? "";
      const encodedCode = StringSerialize(code);
      const url = new URL(window.location.href);
      url.searchParams.set("code", encodedCode);
      window.history.pushState({}, "", url.toString());
      console.log("Saved example to URL:", url.toString());
    },
  });

  effect(() => {
    console.log("highlightedNode changed", highlightedNode.value);
    if (highlightedNode.value == null) {
      return
    }
    const nodeOrigin = highlightedNode.value.node.origin;

    const model = editor.getModel();

    if (model == null || nodeOrigin == null) {
      return
    }

    monaco.editor.setModelMarkers(model, "fluent-editor", [
      {
        startLineNumber: nodeOrigin.start.line,
        startColumn: nodeOrigin.start.column,
        endLineNumber: nodeOrigin.end.line,
        endColumn: nodeOrigin.end.column,
        message: "",
        severity: monaco.MarkerSeverity.Info,
      }
    ]);
  })
};

// MARK: Playground

export function Playground() {

  useSignals()

  useEffect(() => {
    // register URL search params change listener
    const handleUrlChange = () => {
      const codeFromUrl = StringDeserialize(new URLSearchParams(window.location.search).get("code") ?? "");
      if (codeFromUrl !== "") {
        SignalUpdate(code, codeFromUrl);
      }
    };
    window.addEventListener("popstate", handleUrlChange);
    window.addEventListener("pushstate", handleUrlChange);
    window.addEventListener("replacestate", handleUrlChange);
    return () => {
      window.removeEventListener("popstate", handleUrlChange);
      window.removeEventListener("pushstate", handleUrlChange);
      window.removeEventListener("replacestate", handleUrlChange);
    };
  }, [])

  const codeFromUrl = StringDeserialize(new URLSearchParams(window.location.search).get("code") ?? "");
  const code = useSignal<string>(codeFromUrl !== "" ? codeFromUrl : "1 + 1")
  //const code = useSignal<string>(codeFromUrl !== "" ? codeFromUrl : getExample("REPL"))
  const result = useComputed<Value>(() => {
    // return evaluate(read(code))
    return evaluateSyntaxTreeNode(CodeParse(code.value), Object.create(DefaultEnvironment)) ?? null;
  })

  return (
    <>
      <div className="absolute inset-0 z-50 p-2 bg-neutral-900 grid text-white">
        {
          Grid(tf.scalar(2), tf.scalar(1))(
            Print(result),
            CodeEditor(code)
          )
        }
      </div>
    </>
  );
}


// MARK: Examples

const EXAMPLES = {
  "tasks": `
(
($): reactive,
(++): listConcat,
(=): { a, b | a(b) },
(++=): { a, b | a = (a() ++ list(b)) },

task-name: $(""),

task-create: { name |
    s: $(name),
    f: $("🔴"),

    Grid([1, 9])(
        Button(f, { f = "✅" }),
        TextEditor(s),
    )
},

tasks: $(()),

(
    Text("
        # TODO app in 34 lines of code

        Still *some* lines to spare. Please **suggest** features!
    "),
    Grid([5, 1])(
        TextEditor(task-name),
        Button("Add Task", {
            tasks ++= task-create(task-name()),
            task-name = "",
        }),
    ),
    tasks,
)
)
`,
  "tasks-compressed": `
($):reactive,(++):list-concat,(=):{a,b|a(b)},(++=):{a,b|a=(a()++(b))},task-name: $(""),task-create:{name|s: $(name),f: $("🔴"),Grid([1,10])($({Button(f(),{f="✅"})}),$({TextField(s(),s)}),)},tasks: $(()),(Text("# TODO app in 420 bytes
Still some bytes to spare. Please **suggest** features!"),$({Grid([5,1])(TextField(task-name(),task-name),Button("Add Task",{tasks ++= task-create(task-name()),task-name="",}),)}),tasks)
`,
  "tasks-super-compressed-unrunable": `
(++):list-concat,(++:):{a,b|a:(a++(b))},B:Button,G:Grid,T:Textfield,N:"",C:{n|s:n,f:"🔴",G([1,10])(B(f,{f:"✅"})}),T(s)}),)},t:(),(G([5,1])(T(N)}),B("Add Task",{t++=C(N),N="",}),),t)
`
  ,
  "calculator": `

($): reactive,
(.): apply,
(++): string-concat,

d: $(""),
D: $({ d() . evaluate }),
Δ: { f | { d(d() . f) } },
B: { c | Button(c, { x | x ++ c } . Δ) },

(
    $({ TextField(d(), d) }),
    D,
    Grid([2,2,2,1])(
        B("7"),
        B("8"),
        B("9"),
        B(" ÷ "),

        B("4"),
        B("5"),
        B("6"),
        B(" × "),

        B("1"),
        B("2"),
        B("3"),
        B(" - "),

        Button("Reset",  { "" } . Δ),
        B("0"),
        B("."),
        B(" + "),
    ),
)
`,
  "linear-regression": `
; defs
($): Reactive,
(++): TensorConcat,
(++=): { a, b | a(a() ++ b) },

; data
x: (0 :: 10),
y: (x × 0.23 + 0.47),

; model
θ: ~([0, 0]),
f: { x | x × (θ_0) + (θ_1) },

; loss function
μ: { x | Σ(x) ÷ #(x) },
(≈): { x, y | μ((y - x) ^ 2) },
𝓛: { (f(x) ≈ y) },

; optimizer
; minimize: TensorOptimizationAdam(0.01),
minimize : TensorOptimizationSgd(0.03),

losses: $([0]),
a: $([0,0]),

{
    loss: 𝓛(),
    losses ++= [loss],
    a ++= θ,
    minimize(𝓛),
} ⟳ 100,

(
    losses,
    a,
    θ,
)
`,
  "experiment": `
; defs
($): reactive,
(++): tensor-concat,
(++=): { a, b | a(a() ++ b) },

μ: { x | Σ(x) ÷ #(x) },
(≈): { x, y | μ((y - x) ^ 2) },

(
    x: ~(tensor-random-normal([9,1])),
    ;x: (0 :: 10 tensor-reshape [1, 10]),
    x-trans: tensor-transpose(x),

),

target:
[
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 1, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 0, 0, 0, 0],
    [0, 0, 1, 0, 0, 0, 1, 0, 0],
    [0, 0, 0, 1, 1, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
],

loss:
{ (x-trans * x) ≈ target },
optimizer: tensor-optimization-sgd(0.01),
iterations: 10,

{
  optimizer(loss)
} ⟳ iterations,
`,
  "linreg-debug": `
($): reactive,
(++): tensorConcat,
(++=): { a, b | a(a() ++ b) },

x: (0 :: 10),
y: (x × 0.23 + 0.47),

θ: ~([0, 0]),
f: { x | x × (θ_0) + (θ_1) },

μ: { x | Σ(x) ÷ #(x) },
(≈): { x, y | μ((y - x) ^ 2) },
𝓛: { (f(x) ≈ y) },

minimize: tensorOptimizationSgd(0.03),

iters: $(0),

{ i |
    iters(i),
    loss: 𝓛(),
    minimize(𝓛),
} ⟳ 100,

(
    iters,
    θ,
    minimize,
    𝓛(),
    minimize(𝓛)
)
`,
  "REPL": `
($): reactive,

make-cell: {
    code: $("1 + 1"),
    result: $({ evaluate(code()) }),

    Grid(1)(
        Print(result),
        CodeEditor(code),
    )
},

(
    make-cell(),
    make-cell(),
    make-cell(),
)
`,
  "some math": `
(
($): reactive,
exp: tensorExponential,

b: 0.1,
c1: 1,
k: 5,


a: { t | b + (c1 * exp(-(k) * t))},
b + (c1 * exp(-(k) * 1)),

t: $(0),
Slider(t),
$({ a(t()) }),
a(0::100 / 100)
)
`
} as const

const getExample = (k: keyof typeof EXAMPLES) => EXAMPLES[k].trim()

const examples2 = [
  `
        1 + 2 * 3 - 4 / 5 ^ sin(2)
    `,
  `
        1 {x,y | x+y} 2
    `,
  `
        [1,2,3] + 1
    `,
  `
        commute(/) → \\,
        3 \\ 2
    `,
  `
        a:[1,2,3]
        b:[4,5,6]
        a+b
    `,
  `
        a:[1,2,3]
        a
    `,
  `
(
    x: (0::100),
    y: (2 √ x),
    [x,y]
)
    `,
  `
(
    1 {+}() 2,
    fn: {+},
    fn()(1,2),
    1 fn() 2,
)
    `,
  `
(
    ; max: { x,y | [x,y]_(x<y) },
    (⌈): max,
    max(2,3),
    2 max 3,
    (2,3) . max,
    .((2,3), max),
    max(max(1,2), 3),
    1 max 2 max 3,
    1 ⌈ 2 ⌈ 3
)
    `,
  `
      ↔︎ : {⊙ | {x,y| y ⊙ x}},
      1 - 2,
      1 ↔︎(-) 2,
      1 (- · ↔︎) 2
    `,
  `
      x ← variable(1),
      iterate({ x := (x * 2) }, 10),
      x   
    `,
  `
(
    ↔ : { ⊙ | { a, b | b ⊙ a } },
    ⊢ : { ⊙, a | { b | a ⊙ b } },
    ⊣ : ↔(⊢),
    (+ ⊢ -1)(3),
    (-1 ⊣ +)(3),
)
     `,
  `
(
    x: [23, 47],
    a: 2,
    (a = 2) ? x,
    (a = 200) ? x
)
    `,
  `
(
    1 + 2,
    1 add 2,
    add(1,2),
    +(1,2),
    (1,2) . +,
    (1,2) apply add,
)
    `,
  `
f : { x | x ^ 2 },
g : ∇(f),
x : (1 :: 10),

(
    f(x),
    g(x)
)
    `,
  `
        ; definitions
        assignLeft(:, assignLeft),

        ; lerp: 0.5 ≻ [10, 30] = 20
        ≻ : { t, v |
            (v_0 × (1 - t)) + (v_1 × t)
        },

        ; invlerp: [1, 3] ≺ 2 = 0.5
        ≺ : { v, t |
            (t - (v_0)) ÷ ((v_1) - (v_0))
        },
        ; remap: [1,3] ≺ 2 ≻ [10, 30] = 20
        ≺≻ : { s, t | { x | s ≺ x ≻ t } },
        normalize : { x | [min(x), max(x)] ≺ x},

        [10, 20, 30]
        . normalize
        ;. ([10, 30] ≺≻ [100, 200])
        ;. ([100, 200] ≺≻ [0, 1]),
    `,
  `
($): reactive,
(...): delay,
(^^): iterate,

a: $(0),
A: $({ 1 :: (2 + a()) }),

(
    { i | { a(i) } ... (i * 1_000) } ^^ 10
),

A
    `,
  `
($): reactive,
(.): apply,
(++): string-concat,

(
    M: $(""),
    m: $("message 1"),
    M-a: { m | M(M() ++ " " ++ m) },
    M-c: { m | M("") },
    $({ TextField(m(), m) }),
    $({ TextField(m(), m) }),
    Button("Append", { M-a(m()) }),
    Button("Clear", M-c ),
)
    `,
  `
(
    mu:
        log,
    log: 23,
    log,
    mu
)
`,
  `
($): reactive,
(.): apply,
(++): string-concat,

; lerp: 0.5 ≻ [10, 30] = 20
≻ : { t, v |
    (v_0 × (1 - t)) + (v_1 × t)
},

PI: (3.14159),

a: $(0.5),
aa: $({ a() ≻ [1, 10] }),
A: $({ Slider(a(), a) }),

f: $("{ a | a . sin }"),
F: $({ TextField(f(), f) }),

S: $({ linspace(-(PI), PI, 1 / aa()) }),

G: $({
    S() . evaluate(f())
}),

X: $({ [S(), G()] }),

(
    ;S,
    A,
    F,
    G,
)
    `,
  `
(
    ($): reactive,

    (
        v1: "hello",
        TextField(v1),
        s1: $(v1),
        $({ TextField(s1(), s1) })
    ),

    (
        v2: 0.23,
        Slider(v2),
        s2: $(v2),
        $({ Slider(s2(), s2) })
    ),

    (
        v3: list(1,2,3),
        s3: $(v3),
    )
)
    `,

  `
commute: { f | { a, b | b f a } },
update-value: { s, v | s(v) },
($:): { s, v | magic(s): $(v) },
(:=): update-value,
(+=):{ s, d | s := (s() + d) },

(.): list-get,
(#): list-length,
(!): commute(list-get),
($): reactive,

d: ("Hello", "World", "War", 3),

;i $: 0,
i: $(0),

; v: (d.(i % #(d)))
v: $({ d.(i() % #(d)) }),

;tick: time-interval(1),
; w: (d.(tick % #(d))),
w: $({ d.(tick() % #(d)) }),

(
    (
        w,
        v,
    ),
    Grid(3)(
        Button("←", { i += -1 }),
        Button("---"),
        Button("→", { i += 1 }),
    ),
    ; FIX
    Grid(2)(w, v),
)
    `,
].map((s) => dedent(s));

function PlaygroundRender() {
  const root = document.getElementById("root")

  if (!root) {
    throw new Error("Root element not found")
  }

  createRoot(root).render(<Playground />)
}

PlaygroundRender()