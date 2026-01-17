// MARK: Setup

/*

1. clone the repo: `git clone https://github.com/mlajtos/fluent.git`
2. install dependencies: `cd fluent && bun install`
3. start the development server: `bun dev`
4. open `http://localhost:3000/?code=RG9jdW1lbnRhdGlvbg` in your browser
5. edit `client.tsx` (this file) to modify the language and IDE
6. share your changes with others

*/

// MARK: Documentation

const Documentation = `
# Fluent

Fluent is a tiny language + IDE for differentiable tensors and reactive UIs. Fluent is for programmers, researchers, and tinkerers who want to combine math, differentiation, and interactivity in a playful way.

## Quickstart

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
square(x),  ; auto-lifts to reactive!
Button("Reset", { x(0) }),
\`\`\`

## Features

### Syntax
- Tensors
  - multi-dimensional arrays of numbers
  - scalars: \`1\`, \`3.14\`, \`-42\`
  - vectors: \`[1, 2, 3]\`, matrices: \`[[1, 2], [3, 4]]\`
  - auto-broadcasting: \`[1, 2, 3] + 1\` is \`[2, 3, 4]\`
  - indexing with \`_\`: \`a_0\`, \`a_1\`, \`a_(i + 1)\`, \`a_(-1)\` (last element)
  - length with \`#\`: \`#([1, 2, 3])\` is \`3\`
  - range with \`::\`: \`0 :: 10\` is \`[0, 1, 2, ..., 9]\`
- Lists
  - ordered collection of heterogeneous values
  - e.g. \`(1, 2, 3)\`, \`(1, (2, 3), [4])\`, \`()\`, \`(42,)\`
- Functions
  - lambda with \`{}\`: \`{ x | x + 1 }\`, \`{ x, y | x * y }\`, \`{ 42 }\`
  - last expression is the return value: \`{ a: 1, b: 2, a + b }\` returns \`3\`
  - application: \`add(1, 2)\` or infix \`1 add 2\`
- Symbols
  - letter-based: \`a\`, \`FooBar\`, \`bar-baz-1\`, \`α\`, \`β\`, \`θ\`
  - operator-based: \`+\`, \`≠\`, \`!=\`, \`√\`, \`∇\`
  - assignment with \`:\`: \`a: 23, b: (a + 24)\`
  - letter and operator symbols do not conflict: \`foo+bar\`, \`α≠β\` work without spaces
- Evaluation order
  - EVERYTHING is left-to-right, NO operator precedence: \`1 + 2 * 3\` is \`(1 + 2) * 3\` = \`9\`
  - use parentheses to override: \`1 + (2 * 3)\` is \`7\`
  - CRITICAL: Assignment \`:\` is also just a left-to-right operator!
    - \`a: 23\` ✓ – single value, no parens needed
    - \`b: a + 1\` ✗ – parses as \`(b: a) + 1\`, NOT \`b: (a + 1)\`
    - \`b: (a + 1)\` ✓ – correct, parentheses required
    - \`c: fn(a)\` ✓ – function call is a single expression
    - \`d: fn(a) + fn(b)\` ✗ – parses as \`(d: fn(a)) + fn(b)\`
    - \`d: (fn(a) + fn(b))\` ✓ – correct, parentheses required
  - Rule: if rvalue has ANY operator after it, wrap the ENTIRE rvalue in \`()\`
- Comments
  - single-line with \`;\`: \`a: 1, ; this is a comment\`
- Program structure
  - expressions separated by commas: \`a: 1, b: 2, a + b\`
  - last expression is the result (automatically displayed)

### Semantics
- Differentiable programming
  - gradient with \`∇\`: \`∇({ x | x^2 })(3)\` is \`6\`
  - higher-order: \`∇(∇({ x | x^3 }))(1)\` is \`6\`
- Reactive programming
  - create signal: \`x: $(0.5)\`
  - read signal: \`x()\`
  - update signal: \`x(0.7)\`
  - computed signal: \`y: (x + 1)\` – auto-lifts to reactive when \`x\` is a signal
  - explicit computed: \`y: $({ x() + 1 })\` – manual version (rarely needed)
- Iteration
  - repeat N times with \`⟳\`: \`step ⟳ 100\`
- Optimization
  - create variable: \`θ: ~([0, 0])\`
  - define loss: \`loss: { sum(θ^2) }\`
  - create optimizer: \`opt: adam(0.01)\`
  - minimize: \`{ opt(loss) } ⟳ 100\`
- Built-in functions
  - tensor math: \`+\`, \`-\`, \`*\`, \`/\`, \`^\`, \`√\`, \`sum\`, \`mean\`, \`max\`, \`min\`, \`sin\`, \`cos\`, \`log\`, \`exp\`, \`dot\`, \`matmul\`, \`transpose\`, \`reshape\`, etc.
  - tensor creation: \`::\` (range), \`linspace\`, \`eye\`, \`rand\`, \`randn\`
  - lists: \`ListConcat\`, \`ListLength\`, \`ListGet\`, \`ListMap\`, \`ListReduce\`
  - UI: \`Slider\`, \`Button\`, \`Text\`, \`Grid\`
  - optimizers: \`adam\`, \`sgd\`, \`adagrad\`
- Ad-hoc operators
  - define custom operators: \`(++): ListConcat, (1, 2) ++ (3, 4)\`

### IDE
- Syntax highlighting
- Live evaluation with error reporting
- Automatic visualization of values (tensors, lists, functions)
- GPU-accelerated tensor operations (via TensorFlow.js and WebGL)
- LLM-backed code generation (BYO Anthropic API key)
- Command palette (Ctrl+P)
- Auto-completion (Ctrl+Space)
- Shareable URL links (Ctrl+S)
- examples gallery (Ctrl+O)
`

/*

# TODO

## Bugs
- Grid can't use dynamically created lists :'(
  - deeper issue: `(...args) => ...` JS lambdas (stack, grid, concat, etc.) are too magical, but very useful

## Features

- `Canvas` UI component for rapid tensor creation
- loading tensors (and models) from URLs
  - `fetchFromUrl` / `tensorLoad`
- first-class symbols
  - syntax for symbol literal – 'symbol'
  - (this is probably a subset of syntax tree literal)
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
- https://github.com/Rogue-Frontier/Oblivia
  - APL+JS hybrid language

*/

import { grammar, type ActionDict } from "ohm-js";
import { toAST as ohmToAST } from "ohm-js/extras";

// for fancy signals
// https://www.npmjs.com/package/@preact-signals/utils
import { signal, Signal, computed, effect } from "@preact/signals-core"
import { useSignals, useSignal, useComputed } from '@preact/signals-react/runtime';
import { ErrorBoundary } from 'react-error-boundary';
import { useEffect, type JSX, isValidElement, useRef } from "react";
import { createRoot } from "react-dom/client"

import { type BeforeMount, type Monaco, type OnMount, Editor, loader } from '@monaco-editor/react';
import * as monaco from "monaco-editor";
import { type editor } from "monaco-editor";
// @ts-ignore
import { renderMarkdown } from "monaco-editor/esm/vs/base/browser/markdownRenderer.js"
// @ts-ignore
import { IQuickInputService } from "monaco-editor/esm/vs/platform/quickinput/common/quickInput"

(globalThis as { MonacoEnvironment?: typeof MonacoEnvironment }).MonacoEnvironment = {
  getWorker: () => new Worker(new URL("monaco.worker.js", document.baseURI), { type: "module" }),
}

// Configure @monaco-editor/react to use local monaco-editor package
loader.config({ monaco })

import dedent from "ts-dedent";
import { Base64 } from 'js-base64'

import { type Annotations } from "plotly.js"
import * as tf from '@tensorflow/tfjs'
import { interpolateViridis } from 'd3-scale-chromatic'
import { rgb } from 'd3-color'
// Import backend dynamically to prevent tree-shaking
await import('@tensorflow/tfjs-backend-webgl')
await tf.setBackend('webgl')
// @ts-ignore
const Plot = (await import("react-plotly.js")).default.default as Plot

import "./index.css"

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
  EMOJIS: [String.fromCodePoint(0x1F300), String.fromCodePoint(0x1FFFF)], // Emoticons
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
    = (Atom #(~space) (List | NestedExpr))
    
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
    | "(" ")" --empty

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

const CodeParse = (program: string): SyntaxTreeNode => {
  const matchResult = CodeGrammarCompiled.match(program);

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
  const matchResult = CodeGrammarCompiled.match(program);

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

type Value = tf.Tensor | Function | Signal<Value> | Error | string | symbol | null | Value[]
type CurrentScope = Record<string, Value>

const SymbolOrigin = Symbol.for('Origin')

function evaluateSyntaxTreeNode(node: SyntaxTreeNode, env: CurrentScope): Value {

  if (node.type === "Error") {
    const error = new Error(node.content)
    // @ts-ignore
    error[SymbolOrigin] = node.origin
    return error
  }

  if (node.type === "Program") {
    const values: Array<Value> = node.content.map((e) => evaluateSyntaxTreeNode(e, env))
    // last value is the result of the program - reify so symbols get resolved
    const lastValue = values[values.length - 1] ?? null
    return reify(lastValue, env)
  }

  if (node.type === "Number") {
    return tf.scalar(node.content.value)
  }

  if (node.type === "Tensor") {
    const values = node.content.value.map((e) => evaluateSyntaxTreeNode(e, env))
    if (values.length === 0) {
      //return safeApply(Tensor, [[]], env)
      return tf.tensor([])
    }

    return safeApply(TensorStack, values, env)
    //return tf.stack(values as tf.Tensor[])
  }

  if (node.type === "Symbol") {
    // Return raw symbol - resolve happens later in safeApply
    return Symbol.for(node.content.value)
  }

  if (node.type === "String") {
    return dedent(node.content.value)
  }

  if (node.type === "List") {
    // Reify elements - List as value should have resolved symbols
    return node.content.value.map((e) => reify(evaluateSyntaxTreeNode(e, env), env))
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

      // Reify in localEnv so symbols resolve to local values, not outer scope
      return reify(evaluateSyntaxTreeNode(node.content.expr, localEnv), localEnv)
    }

    fn.toString = () => node.origin.source

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

    if (Object.isExtensible(value)) {
      // @ts-ignore
      value[SymbolOrigin] = {
        source: node.origin.source,
        start: node.origin.start,
        end: node.origin.end,
      }
    }

    return value
  }

  if (node.type === "Code") {
    // console.log("Evaluating embedded code:", node.content.value);
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
      return reify(resolved, env)
    }

    return resolved
  }

  return v
}

function safeApply(fn: Value, args: Value[], env: CurrentScope): Value {
  let errorArgs: Error[] = []

  try {
    // console.log("safeApply", fn, args, env)

    let fnValue: Value = reify(fn, env);
    let argsValue: Value[] = args;

    // Selectively reify arguments - quoted args stay as symbols
    // @ts-ignore
    const quotedArgs: number[] = fnValue.quotedArgs ?? []
    argsValue = args.map((arg, i) =>
      quotedArgs.includes(i) ? arg : reify(arg, env)
    )

    errorArgs = argsValue.filter(a => a instanceof Error)
    // if (errorArgs.length > 0) {
    //    throw new Error(`Error in arguments`, { cause: errorArgs[0] })
    //}

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

    // Auto-lift: wrap in computed() when args contain Signals
    // @ts-ignore
    const noAutoLift: boolean = fnValue.noAutoLift ?? false
    const hasSignalArgs = argsValue.some(a => a instanceof Signal)

    if (hasSignalArgs && !noAutoLift) {
      let previousResult: any = null

      return computed(() => {
        if (previousResult instanceof tf.Tensor) {
          previousResult.dispose()
        }

        const unwrapped = argsValue.map(a => a instanceof Signal ? a.value : a)

        let result: any = null

        try {
          result = tf.tidy(() => {
            return fnValue.apply(env, unwrapped)
          })
        } catch (e) {
          result = e
        }

        previousResult = result

        return result
      })
    }

    return tf.tidy(() => {
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
  // console.log("Evaluating program:", program, this);
  return evaluateProgramWithScope(program, this);
}

const FunctionIterate = (fn: (index?: tf.Scalar) => void, iterations: tf.Scalar = tf.scalar(1)) => {
  if (!(typeof fn === "function" && iterations instanceof tf.Tensor)) {
    throw new Error("`FunctionIterate(fn, iterations)`: `fn` must be a function and `iterations` must be a scalar Tensor");
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
    return new Error(`'SignalRead': ${String(s)} is not a signal`)
  }
  return s.value
}

const SignalUpdate = <T,>(s: Signal<T>, v: T) => {
  if (s instanceof Signal) {
    s.value = v
    return
  }

  return new Error(`'SignalUpdate': ${s} is not a signal`)
}

const SignalComputed = computed

const SignalEffect = effect
// @ts-ignore
SignalEffect.noAutoLift = true

// Read signal value once without creating reactive dependency
const SignalOnce = <T,>(s: Signal<T> | T): T => {
  if (s instanceof Signal) {
    return s.peek()
  }
  return s
}
// @ts-ignore
SignalOnce.noAutoLift = true

const Reactive = (a: Value) => {
  if (typeof a === "function") {
    // @ts-ignore
    return SignalComputed(a)
  }
  return SignalCreate(a)
}
// @ts-ignore
Reactive.noAutoLift = true

const SymbolAssign = function (this: CurrentScope, a: Value, b: Value) {
  if (typeof a === "symbol") {
    a.assign(this, b)
    return a.resolve(this)
  } else {
    return new Error(`'SymbolAssign': Left side must be a symbol, got: ${String(a)}`)
  }
}

// First argument (target symbol) should not be reified
// @ts-ignore
SymbolAssign.quotedArgs = [0]
// @ts-ignore
SymbolAssign.noAutoLift = true

const FunctionEvaluate = function (this: CurrentScope, fn: Value, args: Value[]) {
  return safeApply(fn, args, this)
}

const FunctionApply = function (this: CurrentScope, a: Value, b: Value): Value {
  const arg = a instanceof Array ? a : [a]
  return safeApply(b, arg, this)
}

const FunctionNoAutoLift = (fn: Function) => {
  // @ts-ignore
  fn.noAutoLift = true
  return fn
}
// @ts-ignore
FunctionNoAutoLift.noAutoLift = true  // Don't auto-lift the wrapper itself!

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

  // does unwanted spread
  // return [].concat(args)
}

const ListLength = (a: unknown[]) => {
  return tf.scalar(a.length)
}

const ListGet = (a: any[], b: tf.Scalar) => {
  if (!Array.isArray(a)) {
    return new Error("'ListGet': 'a' must be an array")
  }

  const index = getAsSyncList(b) as number
  if (index < -a.length || index >= a.length) {
    return new Error(`'ListGet': Index '${index}' out of bounds for list of length ${a.length}`)
  }

  return a.at(index)
}

const ListMap = (a: any[], fn: (value: any, index: tf.Scalar) => any) => {
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

const Tensor = tf.tensor
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
const TensorRoot = (a: tf.Tensor, b: tf.Tensor) => {
  if (b === undefined) {
    return tf.sqrt(a)
  }
  return TensorPower(b, TensorReciprocal(a))
} 
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
const TensorSum = (a: tf.Tensor, b?: tf.Tensor) => {
  if (b !== undefined) {
    const axis = getAsSyncList(b) as (number | number[])
    return tf.sum(a, axis)
  }
  return tf.sum(a)
}
// TensorReduce(a, 1, *)
const TensorProduct = (a: tf.Tensor, b: tf.Tensor) => {
  if (b !== undefined) {
    const axis = getAsSyncList(b) as (number | number[])
    return tf.prod(a, axis)
  }
  return tf.prod(a)
}
const TensorMean = (a: tf.Tensor, b: tf.Tensor) => {
  if (b !== undefined) {
    const axis = getAsSyncList(b) as (number | number[])
    return tf.mean(a, axis)
  }
  return tf.mean(a)
}
const TensorMin = (a: tf.Tensor, b?: tf.Tensor) => {
  if (b !== undefined) {
    const axis = getAsSyncList(b) as (number | number[])
    return tf.min(a, axis)
  }
  return tf.min(a)
}
const TensorMax = (a: tf.Tensor, b?: tf.Tensor) => {
  if (b !== undefined) {
    const axis = getAsSyncList(b) as (number | number[])
    return tf.max(a, axis)
  }
  return tf.max(a)
}

const TensorNormalize = (a: tf.Tensor, p?: tf.Tensor) => {
  const pVal = p !== undefined ? getAsSyncList(p) as number : 2
  const norm = tf.norm(a, pVal)
  return tf.div(a, norm)
}

const TensorNegate = tf.neg
const TensorAbsolute = tf.abs
const TensorSign = tf.sign
const TensorLogarithm = tf.log
const TensorExponential = tf.exp
const TensorReciprocal = tf.reciprocal
const TensorRound = tf.round
const TensorCeil = tf.ceil
const TensorFloor = tf.floor
const TensorErrorFunction = tf.erf

const TensorSort = (x: tf.Tensor) => {
  return TensorReverse(tf.topk(x, x.size, true).values)
}
const TensorSlice = (a: tf.Tensor, begin: tf.Tensor, size: tf.Tensor) => {
  const beginList = getAsSyncList(begin) as number[]
  const sizeList = getAsSyncList(size) as number[]
  return tf.slice(a, beginList, sizeList)
}
const TensorMask = (a: tf.Tensor, b: tf.Tensor) => {
  const count = TensorSum(TensorBoolean(b))
  const indices = TensorRange(tf.tensor(0), TensorShape(a));
  const mu = TensorWhere(b, indices, TensorFill(TensorShape(a), tf.tensor(-1)));
  const top = TensorReverse(TensorSort(mu));
  const i = TensorSlice(top, tf.tensor(0), count);
  return TensorGather(a, TensorReverse(tf.cast(i, "int32")));
}
const TensorFill = (shape: tf.Tensor, value: tf.Tensor) => {
  const shapeList = getAsSyncList(shape) as number[]
  const valueScalar = getAsSyncList(value) as number
  return tf.fill(shapeList, valueScalar)
}
const TensorBoolean = (a: tf.Tensor) => tf.cast(a, "bool")

const TensorGradient = tf.grad
const TensorTranspose = tf.transpose
const TensorIdentity = (a: tf.Tensor) => {
  const size = getAsSyncList(a) as number
  return tf.eye(size)
}

const TensorRange = (a: tf.Tensor, b: tf.Tensor) => {
  if (b === undefined) {
    const stop = getAsSyncList(tf.cast(a, "int32")) as number
    return tf.range(0, stop)
  }

  const start = getAsSyncList(tf.cast(a, "int32")) as number
  const stop = getAsSyncList(tf.cast(b, "int32")) as number
  return tf.range(start, stop)
}

const TensorLinearSpace = (range: tf.Tensor, steps: tf.Tensor) => {
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

const TensorReverse = tf.reverse

const TensorMatrixMultiply = (a: tf.Tensor, b: tf.Tensor) => tf.matMul(a, b, false, false)
const TensorDotProduct = tf.dot

const TensorLength = (a: tf.Tensor, b?: tf.Tensor) => {
  if (b !== undefined) {
    // @ts-ignore
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

const TensorWhere = (a: tf.Tensor, b: tf.Tensor, c: tf.Tensor) => tf.where(TensorBoolean(a), b, c)
const TensorIsNaN = tf.isNaN

// const tensorVariableAssignOriginal = Variable.prototype.assign

// Variable.prototype.assign = function (newValue: tf.Tensor<tf.Rank>) {
//   update(this.signal, newValue);
//   tensorVariableAssignOriginal.bind(this)(newValue)
// }

const TensorVariable = (a: tf.Tensor) => {
  const version = signal(0)
  const variableTensor = tf.variable(a, true)

  // Hook assign to trigger reactivity
  const originalAssign = variableTensor.assign.bind(variableTensor)
  variableTensor.assign = (newValue: tf.Tensor) => {
    const result = originalAssign(newValue)
    version.value++
    return result
  }

  // @ts-ignore
  variableTensor.__version__ = version
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

const TensorOptimizationAdaGrad = (a: tf.Tensor) => {
  const optimizer = tf.train.adagrad(getAsSyncList(a) as number)
  return (fn: () => tf.Scalar) => optimizer.minimize(fn, true)  
}

const TensorRandomNormal = (a: tf.Tensor) => tf.randomStandardNormal(getAsSyncList(a) as number[])
const TensorRandomUniform = (a: tf.Tensor) => tf.randomUniform(getAsSyncList(a) as number[])

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
// @ts-ignore
Button.noAutoLift = true

const convertTextToHTML = async (value: string) => {
  const result = await renderMarkdown({ value }, {
    codeBlockRenderer: colorizeCodeAsync,
  })

  const element = result.element as HTMLElement

  // Unwrap single <p> elements for simple single-line text
  if (element.children.length === 1 && element.children[0].tagName === 'P') {
    const p = element.children[0]
    while (p.firstChild) {
      element.insertBefore(p.firstChild, p)
    }
    p.remove()
  }

  // Post-process: colorize inline <code> elements (not inside code blocks)
  const inlineCodeElements = element.querySelectorAll('code:not(.code code)')

  if (inlineCodeElements.length > 0) {
    const colorize = (await getEditor()).colorize

    await Promise.all(Array.from(inlineCodeElements).map(async (el) => {
      const text = el.textContent ?? ''
      if (text) {
        try {
          const colorized = await colorize(text, 'fluent', {})
          // colorize wraps content in divs, extract inner HTML without line breaks
          const temp = document.createElement('div')
          temp.innerHTML = colorized
          const inner = temp.querySelector('div')?.innerHTML ?? temp.innerHTML
          el.innerHTML = inner.replace(/<br\/?>/g, '')
        } catch {
          // Keep original text as fallback
        }
      }
    }))
  }

  return element
}

const colorizeCodeAsync = async (language: string, value: string) => {
  const colorize = (await getEditor()).colorize
  const e = document.createElement("div")
  e.className = "overflow-hidden"
  e.style.fontFamily = "monospace"
  try {
    e.innerHTML = await colorize(value, language, {})
  } catch (error) {
    e.textContent = value
  }
  return e
}

const NativeDOMElement = ({ fn }: { fn: () => Promise<HTMLElement> }) => {
  const ref = useRef<HTMLDivElement>(null)

  useEffect(() => {
    let mounted = true

    async function mount() {
      if (ref.current && mounted) {
        // Clear existing content first (handles HMR edge cases)
        ref.current.innerHTML = ''
        const element = await fn()
        if (ref.current && mounted) {
          ref.current.appendChild(element)
        }
      }
    }

    mount()

    return () => {
      mounted = false
      if (ref.current) {
        ref.current.innerHTML = ''
      }
    }
  }, [fn])

  return <div ref={ref}></div>
}

const Text = (value: string) => {
  if (typeof value !== "string") {
    return new Error(`Text: expected string, got ${typeof value}`)
  }
  const fn = () => { return convertTextToHTML(value) }
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
// @ts-ignore
TextEditor.noAutoLift = true

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
// @ts-ignore
Slider.noAutoLift = true

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
    if (key === '__metadata__') { continue } // Skip optional metadata

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

function Camera(width?: tf.Tensor, height?: tf.Tensor, fps?: tf.Tensor): Signal<tf.Tensor> {
  const w = width ? getAsSyncList(width) as number : 640
  const h = height ? getAsSyncList(height) as number : 480
  const targetFps = fps ? getAsSyncList(fps) as number : 30
  const frameInterval = 1000 / targetFps

  // Initialize with black frame so operations don't fail before camera starts
  const frameSignal = SignalCreate<tf.Tensor>(tf.zeros([h, w, 3], 'int32'))

  const video = document.createElement('video')
  video.autoplay = true
  video.playsInline = true

  let active = true
  let streamRef: MediaStream | null = null

  navigator.mediaDevices.getUserMedia({
    video: { width: w, height: h }
  }).then(stream => {
    streamRef = stream
    video.srcObject = stream

    video.onloadedmetadata = () => {
      video.play()

      let lastTime = 0
      const captureFrame = (time: number) => {
        if (!active) return

        if (time - lastTime >= frameInterval) {
          if (video.readyState === video.HAVE_ENOUGH_DATA) {
            // Dispose old tensor to prevent memory leak
            const oldTensor = frameSignal.value
            const tensor = tf.browser.fromPixels(video)
            frameSignal.value = tensor
            if (oldTensor) { oldTensor.dispose() }
          }
          lastTime = time
        }
        requestAnimationFrame(captureFrame)
      }

      requestAnimationFrame(captureFrame)
    }
  }).catch(err => {
    console.error('Camera access denied:', err)
  })

  // Return read-only signal with dispose method
  const result = computed(() => frameSignal.value) as Signal<tf.Tensor> & { dispose: () => void }
  result.dispose = () => {
    active = false
    if (streamRef) {
      streamRef.getTracks().forEach(track => track.stop())
      streamRef = null
    }
    video.srcObject = null
  }
  return result
}

function Microphone(bufferSize?: tf.Tensor): Signal<tf.Tensor> {
  const size = bufferSize ? getAsSyncList(bufferSize) as number : 2048

  // Initialize with silence
  const audioSignal = SignalCreate<tf.Tensor>(tf.zeros([size]))

  let active = true
  let streamRef: MediaStream | null = null
  let audioContextRef: AudioContext | null = null

  navigator.mediaDevices.getUserMedia({ audio: true }).then(stream => {
    streamRef = stream
    const audioContext = new AudioContext()
    audioContextRef = audioContext
    const source = audioContext.createMediaStreamSource(stream)
    const analyser = audioContext.createAnalyser()
    analyser.fftSize = size * 2

    source.connect(analyser)

    const dataArray = new Float32Array(size)

    const captureAudio = () => {
      if (!active) return

      analyser.getFloatTimeDomainData(dataArray)

      const oldTensor = audioSignal.value
      const tensor = tf.tensor1d(dataArray)
      audioSignal.value = tensor
      if (oldTensor) { oldTensor.dispose() }

      requestAnimationFrame(captureAudio)
    }

    captureAudio()
  }).catch(err => {
    console.error('Microphone access denied:', err)
  })

  const result = computed(() => audioSignal.value) as Signal<tf.Tensor> & { dispose: () => void }
  result.dispose = () => {
    active = false
    if (streamRef) {
      streamRef.getTracks().forEach(track => track.stop())
      streamRef = null
    }
    if (audioContextRef) {
      audioContextRef.close()
      audioContextRef = null
    }
  }
  return result
}

function MicrophoneSpectrum(bufferSize?: tf.Tensor): Signal<tf.Tensor> {
  const size = bufferSize ? getAsSyncList(bufferSize) as number : 1024

  // Initialize with silence
  const spectrumSignal = SignalCreate<tf.Tensor>(tf.zeros([size]))

  let active = true
  let streamRef: MediaStream | null = null
  let audioContextRef: AudioContext | null = null

  navigator.mediaDevices.getUserMedia({ audio: true }).then(stream => {
    streamRef = stream
    const audioContext = new AudioContext()
    audioContextRef = audioContext
    const source = audioContext.createMediaStreamSource(stream)
    const analyser = audioContext.createAnalyser()
    analyser.fftSize = size * 2
    analyser.smoothingTimeConstant = 0.8

    source.connect(analyser)

    const dataArray = new Uint8Array(size)

    const captureSpectrum = () => {
      if (!active) return

      analyser.getByteFrequencyData(dataArray)

      const oldTensor = spectrumSignal.value
      // Normalize to 0-1 range (use tidy to dispose intermediate tensor)
      const tensor = tf.tidy(() => tf.tensor1d(dataArray).div(255))
      spectrumSignal.value = tensor
      if (oldTensor) { oldTensor.dispose() }

      requestAnimationFrame(captureSpectrum)
    }

    captureSpectrum()
  }).catch(err => {
    console.error('Microphone access denied:', err)
  })

  const result = computed(() => spectrumSignal.value) as Signal<tf.Tensor> & { dispose: () => void }
  result.dispose = () => {
    active = false
    if (streamRef) {
      streamRef.getTracks().forEach(track => track.stop())
      streamRef = null
    }
    if (audioContextRef) {
      audioContextRef.close()
      audioContextRef = null
    }
  }
  return result
}

function Time(): Signal<tf.Tensor> {
  const startTime = performance.now()
  const timeSignal = SignalCreate<tf.Tensor>(tf.scalar(0))

  let active = true

  const updateTime = () => {
    if (!active) return

    const oldTensor = timeSignal.value
    const elapsed = (performance.now() - startTime) / 1000
    timeSignal.value = tf.scalar(elapsed)
    if (oldTensor) { oldTensor.dispose() }
    requestAnimationFrame(updateTime)
  }

  requestAnimationFrame(updateTime)

  const result = computed(() => timeSignal.value) as Signal<tf.Tensor> & { dispose: () => void }
  result.dispose = () => { active = false }
  return result
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

  SymbolAssign,

  CodeParse,
  CodeEvaluate,
  CodePrint: PrettyPrintSyntaxTree,

  FunctionIterate,
  FunctionCascade,
  FunctionEvaluate,
  FunctionApply,
  FunctionNoAutoLift,

  // MARK: Signals
  
  Reactive,
  SignalCreate,
  SignalComputed,
  SignalRead,
  SignalUpdate,
  SignalEffect,
  SignalOnce,
  once: SignalOnce,

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
  TensorNormalize,

  TensorNegate,
  TensorAbsolute,
  TensorSign,
  TensorLogarithm,
  TensorExponential,
  TensorReciprocal,
  TensorRound,
  TensorCeil,
  TensorFloor,
  TensorErrorFunction,
  TensorSort,

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
  TensorMask,
  TensorSlice,
  TensorFill,

  TensorVariable,
  TensorAssign,
  TensorOptimizationAdam,
  TensorOptimizationSgd,
  TensorOptimizationAdaGrad,
  TensorRandomNormal,
  TensorRandomUniform,

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

  // MARK: Reactive + UI Components

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
  Camera,
  Microphone,
  MicrophoneSpectrum,
  Time,
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

// MARK: Prelude

const PRELUDE = `
SymbolAssign(:, SymbolAssign),
(:=): TensorAssign,

; Functions
(.): FunctionApply,
apply: FunctionApply,
(⟳): FunctionIterate,
iter: FunctionIterate,
(@): FunctionEvaluate,
eval: FunctionEvaluate,
cascade: FunctionCascade,

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
TensorOuter: { f | { a,b | f(a ⍴ [-1, 1], b ⍴ [1, -1]) } },
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
conv: { kernel, arr | stencil(#(kernel), { w | Σ(w × kernel) }, arr) },

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

; Math
neg: TensorNegate,
abs: TensorAbsolute,
sign: TensorSign,
round: TensorRound,
floor: TensorFloor,
ceil: TensorCeil,
reciprocal: TensorReciprocal,
log: TensorLogarithm,
exp: TensorExponential,

; Trigonometry
sin: TensorSine,
cos: TensorCosine,
tan: TensorTangent,
sinh: TensorSineHyperbolic,
cosh: TensorCosineHyperbolic,
tanh: TensorTangentHyperbolic,
asinh: TensorSineHyperbolicInverse,
acosh: TensorCosineHyperbolicInverse,
atanh: TensorTangentHyperbolicInverse,

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

; Variables
(~): TensorVariable,
var: TensorVariable,

; Tensor ops
sort: TensorSort,
mask: TensorMask,
where: TensorWhere,
isNaN: TensorIsNaN,
eye: TensorIdentity,
dot: TensorDotProduct,
matmul: TensorMatrixMultiply,

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
`

const createScope = () => {
  const scope = Object.create(DefaultEnvironment)
  evaluateProgramWithScope(PRELUDE, scope)
  return scope
}

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

// Reference to main editor for hover highlighting
let mainEditorRef: { editor: editor.IStandaloneCodeEditor; monaco: Monaco } | null = null;

const setHoverHighlight = (origin: Origin | null) => {
  if (!mainEditorRef) { return }
  const { editor, monaco } = mainEditorRef
  const model = editor.getModel()
  if (!model) { return }

  if (!origin) {
    monaco.editor.setModelMarkers(model, "fluent-hover", [])
    return
  }

  monaco.editor.setModelMarkers(model, "fluent-hover", [
    {
      startLineNumber: origin.start.line,
      startColumn: origin.start.column,
      endLineNumber: origin.end.line,
      endColumn: origin.end.column,
      message: "Error source",
      severity: monaco.MarkerSeverity.Error,
    }
  ])
}

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
        onPointerOver={() => {
          setHoverHighlight(node.node.origin)
        }}
        onPointerOut={() => {
          setHoverHighlight(null)
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
    } else if (node.type === 'String') {
      return `"${node.content.value}"`;
    } else if (node.type === 'Code') {
      return '``';
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
    } else if (node.type === 'String') {
      return [];
    } else if (node.type === 'Code') {
      return []
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
// @ts-ignore
Print.noAutoLift = true

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
      <div className="grid gap-1 rounded-xl">
        {obj.map((item, key) => <div key={key} className={`${frameStyle} !border-0 grid hover:bg-neutral-800`}>{PrettyPrint(item)}</div>)}
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

    // Reactive variable: create computed that re-reads on version change
    if (obj instanceof tf.Tensor && (obj as any).__version__) {
      const version = (obj as any).__version__ as Signal<number>
      const reactiveValue = computed(() => {
        version.value  // subscribe to changes
        return obj.clone()
      })
      return PrettyPrint(reactiveValue)
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
        // Use fast canvas with viridis colormap for large tensors, Plotly for small (debugging)
        const size = obj.shape[0] * obj.shape[1]
        if (size > 400) {
          return <HeatCanvas data={obj} />
        }

        return <HeatPlot data={obj} />
      }

      if (obj.rank === 3) {
        // RGB image - use canvas renderer
        return <TensorCanvas data={obj} />
      }

      if (obj.rank > 3) {
        console.warn("Tensor with rank > 3 is not supported for plotting", obj);
      }

      return (
        <div className="">
          <span className="font-bold">Tensor:</span> {obj.shape.join(", ")}<br />
          <BarPlot data={obj} />
        </div>
      );
    }

    if (obj instanceof Error) {
      // traverse error causes and collent them into a flat list
      function linearizeError(error: Error): Error[] {
        const errors = [error];
        let currentError: any = error;
        while (currentError.cause && currentError.cause instanceof Error) {
          errors.push(currentError.cause);
          currentError = currentError.cause;
        }
        return errors;
      }

      return <ol className="list-decimal list-inside flex flex-col gap-2 border rounded-lg border-red-700 bg-red-950 overflow-scroll max-h-24">{linearizeError(obj).reverse().map((error, index) => {
        return (
          <li key={index}
            className="list-item px-2 py-0.5 text-red-700 cursor-pointer hover:bg-red-700 hover:text-red-200"
            onPointerOver={() => {
              // @ts-ignore
              setHoverHighlight(error[SymbolOrigin] || null)
            }}
            onPointerOut={() => {
              setHoverHighlight(null)
            }}>
            {error.message}
            {/* {obj.cause ? <div>{PrettyPrint(obj.cause)}</div> : null} */}
          </li>
        );
      })}</ol>;



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

  const annotations: Partial<Annotations>[] = [];

  if (data.shape.reduce((a, b) => a * b) < 20) {
    // @ts-ignore
    annotations.push(...data.arraySync().map((value: number, i: number) => ({
      xref: "x1",
      yref: "y1",
      yanchor: "bottom",
      x: i,
      y: value,
      text: value.toLocaleString("en-US", { minimumFractionDigits: 0, maximumFractionDigits: 2, useGrouping: true }).replace(/,/g, "_"),
      font: { color: '#D4D4D4', shadow: "0px 0px 1px black" },
      showarrow: false,
    })));
  }

  return (
    <Plot
      data={[
        {
          y: data.arraySync(),
          type: 'bar',
          marker: {
          color: data.arraySync(),
          colorscale: 'Viridis',
        },
        },
      ]}
      layout={{
        paper_bgcolor: 'transparent',
        plot_bgcolor: 'transparent',
        font: { color: '#D4D4D4', },
        annotations,
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

// Fast canvas-based tensor renderer for real-time use (Camera, etc.)
const TensorCanvas = ({ data }: { data: tf.Tensor }) => {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  const height = data.shape[0] || 100
  const width = data.shape[1] || 100
  const aspect = width / height

  useEffect(() => {
    if (!canvasRef.current || !data || data.isDisposed) { return }

    const canvas = canvasRef.current

    // Only resize if dimensions changed (resizing clears canvas, causes flicker)
    if (canvas.width !== width || canvas.height !== height) {
      canvas.width = width
      canvas.height = height
    }

    tf.browser.toPixels(data as tf.Tensor2D | tf.Tensor3D, canvas)
  }, [data])

  return (
    <canvas
      ref={canvasRef}
      style={{
        width: '100%',
        maxWidth: `${width}px`,
        aspectRatio: `${aspect}`,
        imageRendering: 'pixelated'
      }}
    />
  )
}

// Generate viridis colormap LUT from d3-scale-chromatic (256 entries, RGB 0-1)
const VIRIDIS_LUT = tf.tensor2d(
  Array.from({ length: 256 }, (_, i) => {
    const color = rgb(interpolateViridis(i / 255))
    return [color.r / 255, color.g / 255, color.b / 255]
  })
)

// Fast canvas-based heatmap with viridis colormap (GPU-accelerated via LUT)
const HeatCanvas = ({ data }: { data: tf.Tensor }) => {
  const canvasRef = useRef<HTMLCanvasElement>(null)

  const height = data.shape[0] || 100
  const width = data.shape[1] || 100
  const aspect = width / height

  useEffect(() => {
    if (!canvasRef.current || !data || data.isDisposed) { return }

    const canvas = canvasRef.current

    if (canvas.width !== width || canvas.height !== height) {
      canvas.width = width
      canvas.height = height
    }

    // Apply viridis colormap via lookup table
    const rgb = tf.tidy(() => {
      // Normalize to [0, 1]
      const min = tf.min(data)
      const max = tf.max(data)
      const range = tf.maximum(tf.sub(max, min), 1e-6)
      const normalized = tf.div(tf.sub(data, min), range)

      // Scale to [0, 255] and convert to int for LUT indexing
      const indices = tf.cast(tf.clipByValue(tf.mul(normalized, 255), 0, 255), 'int32')

      // Gather colors from LUT
      const flat = tf.reshape(indices, [-1])
      const colors = tf.gather(VIRIDIS_LUT, flat)

      // Reshape back to image dimensions
      return tf.reshape(colors, [height, width, 3])
    })

    tf.browser.toPixels(rgb as tf.Tensor3D, canvas).then(() => {
      rgb.dispose()
    })
  }, [data])

  return (
    <canvas
      ref={canvasRef}
      style={{
        width: '100%',
        maxWidth: `${width}px`,
        aspectRatio: `${aspect}`,
        imageRendering: 'pixelated'
      }}
    />
  )
}

// MARK: Examples

const EXAMPLES = {
  "no-precedence": `
; Left-to-right evaluation, no operator precedence
1 + 2 * 3 - 4 / 5 ^ sin(2)
`,
  "infix-lambda": `
; Using lambda as infix operator
1 {x,y | x+y} 2
`,
  "broadcasting": `
; Tensor broadcasting - scalar added to vector
[1,2,3] + 1
`,
  "vector-addition": `
; Element-wise vector addition
a:[1,2,3], b:[4,5,6], a+b
`,
  "calling-conventions": `
; Multiple ways to call functions
(
    1 + 2,
    1 add 2,
    add(1,2),
    +(1,2),
    (1,2) . +,
    (1,2) apply add,
)
`,
  "max-variations": `
; Different ways to use max function
(
    (⌈): max,
    max(2,3),
    2 max 3,
    (2,3) . max,
    max(max(1,2), 3),
    1 max 2 max 3,
    1 ⌈ 2 ⌈ 3
)
`,
  "gradient": `
; Automatic differentiation with ∇
f : { x | x ^ 2 },
g : ∇(f),
x : (1 :: 10),
(f(x), g(x))
`,
  "ad-hoc-operators": `
; Defining custom operators
(++): ListConcat,
(1, 2) ++ (3, 4)
`,
  "flip-operator": `
; Commute/flip operator arguments
↔︎ : {⊙ | {x,y| y ⊙ x}},
1 - 2,
1 ↔︎(-) 2
`,
  "partial-application": `
; Partial application with custom operators
(
    ↔ : { ⊙ | { a, b | b ⊙ a } },
    ⊢ : { ⊙, a | { b | a ⊙ b } },
    ⊣ : ↔(⊢),
    (+ ⊢ -1)(3),
    (-1 ⊣ +)(3),
)
`,
  "lerp": `
; Linear interpolation with custom operators
; lerp: 0.5 ≻ [10, 30] = 20
≻ : { t, v | (v_0 × (1 - t)) + (v_1 × t) },

; invlerp: [1, 3] ≺ 2 = 0.5
≺ : { v, t | (t - (v_0)) ÷ ((v_1) - (v_0)) },

normalize : { x | [min(x), max(x)] ≺ x },
[10, 20, 30] . normalize
`,
  "reactive-slider": `
; Reactive slider with computed value
(
  x: $(0.5),
  Slider(x),
  x ^ 2
)
`,
  "reactive-function-plot": `
; Interactive sine wave - adjust resolution with slider

PI: 3.14159,
resolution: $(0.5),
points: (resolution × 90 + 10),
x: linspace([-PI, PI], points),

(Slider(resolution), sin(x))
`,
  "tasks": `
; TODO App - reactive task management

; Helper operators
(++): ListConcat,
(=): FunctionNoAutoLift({ a, b | a(b) }),
(++=): FunctionNoAutoLift({ a, b | a(a() ++ List(b)) }),

; Task factory - creates editable task with toggle
task-create: { name |
    text: $(name),
    done: $("🔴"),
    Grid([1, 9])(
        Button(done, { done = "✅" }),
        TextEditor(text),
    )
},

; App state
task-name: $(""),
tasks: $(List()),

; UI
(
    Text("# TODO App"),
    Grid([5, 1])(
        TextEditor(task-name),
        Button("Add Task", {
            tasks ++= task-create(task-name()),
            task-name = "",
        }),
    ),
    tasks,
)
`,
  "tasks-compressed": `
(++):ListConcat,task-name: $(""),task-create:{name|s: $(name),f: $("🔴"),Grid([1,10])(Button(f,{f("✅")}),TextEditor(s))},tasks: $(()),(Text("# TODO"),Grid([5,1])(TextEditor(task-name),Button("Add",{tasks(tasks()++List(task-create(task-name()))),task-name("")})),tasks)
`,
  "tasks-mini": `
; Minimal TODO app
(++): ListConcat,
n: ($""),
tasks: ($ List()),
(
    TextEditor(n),
    Button("Add",{
        tasks(tasks() ++ List(n())),
        n("")
    }),
    tasks
)
`
  ,
  "calculator": `
; Calculator - builds expression string and evaluates it

(++): StringConcat,

expr: $(""),
result: (expr . CodeEvaluate),
append: { f | { expr(expr() . f) } },
btn: { c | Button(c, { x | x ++ c } . append) },

(
    TextEditor(expr),
    result,
    Grid([2,2,2,1])(
        btn("7"), btn("8"), btn("9"), btn(" ÷ "),
        btn("4"), btn("5"), btn("6"), btn(" × "),
        btn("1"), btn("2"), btn("3"), btn(" - "),
        Button("C", { "" } . append), btn("0"), btn("."), btn(" + "),
    ),
)
`,
  "linear-regression": `
; Linear Regression with gradient descent
; Find θ that minimizes (f(x) - y)²

; Operators
(++): TensorConcat,

; Data: y = 0.23x + 0.47
x: (0 :: 10),
y: (x × 0.23 + 0.47),

; Model: f(x) = θ₀·x + θ₁
θ: ~([0, 0]),
f: { x | x × (θ_0) + (θ_1) },

; Loss: mean squared error
𝓛: { mean((f(x) - y) ^ 2) },

; Training
opt: adam(0.03),
losses: $([]),

{ losses(losses() ++ [opt(𝓛)]) } ⟳ 200,

; Results
(
    Text("**Loss:**"), losses,
    Text("**θ (learned):**"), θ,
    Text("**θ (target):**"), [0.23, 0.47],
)
`,
  "linear-regression-compressed": `
x: (0 :: 10),
y: (x × 0.23 + 0.47),
θ: ~([0, 0]),
f: { x | x × (θ_0) + (θ_1) },
𝓛: { μ((y - f(x)) ^ 2) },
minimize: adam(0.03),
losses: $([]),
{ losses(losses() concat [minimize(𝓛)]), } ⟳ 400,
(losses, θ)`,
  "REPL": `
; Multi-cell REPL - each cell evaluates independently
; BUG: CodeEditor's height doesn't auto-adjust on content change

cell: {
    code: $("1 + 1"),
    result: CodeEvaluate(code),
    Grid(1)(Print(result), CodeEditor(code))
},

(cell(), cell(), cell())
`,
  "exponential-decay": `
; Exponential decay: f(t) = b + c·e^(-k·t)
; Interactive visualization with slider

b: 0.1,   ; baseline
c: 1,     ; amplitude
k: 5,     ; decay rate

f: { t | b + (c × exp(-(k) × t)) },

; Interactive plot
t: $(0),
(
  Slider(t),
  f(t),
  f(0::100 / 100)
)
`,
  "deep-delta-learning": `
; Deep Delta Learning - learns sin(x)
; Uses delta rule: x' = x + β·k·(v - k·x)

d: 160,

; Learnable parameters
w: ~(randn([d]) × 0.1),
κ: ~(randn([d]) × 0.1),
ν: ~([0]),
β: ~([0]),

; Activation & delta block
σ: { z | 1 ÷ (1 + exp(-z)) },
Δ: { x |
    k: κ ÷ √(Σ(κ²) + 1e-7),
    x + (2 × σ(β) × (ν - Σ(k × x)) × k)
},

; Model & data
f: { t | Δ(t × w) },
τ: linspace([0, 6.28], d),
ŷ: sin(τ),

; Training
(++): TensorConcat,
(++=): { a, b | a(a() ++ b) },

𝓛: { mean((f(τ) - ŷ)²) },
opt: adam(0.001),
losses: $([]),
pred: $(f(τ)),

{ losses ++= [opt(𝓛)], pred(f(τ)) } ⟳ 500,

; Results
(
    Text("**Loss:**"), losses,
    Text("**Prediction vs Target:**"), (ŷ, pred),
)
`,
"magnets-simulation": `
; Animated Magnetic Field

(≻): { t, v | ((v_0) × (1 - t)) + (((v_1) × t)) },
(≺): { v, t | (t - (v_0)) / ((v_1) - (v_0)) },
normalize: { x | [min(x), max(x)] ≺ x },

numMagnets: 3,
n: 700,

; Grid
xs: linspace([-2, 2], n),
ys: linspace([-1.5, 1.5], n),
X: ((xs ⍴ [n, 1]) tile [1, n]),
Y: ((ys ⍴ [1, n]) tile [n, 1]),

; Magnet positions using lerp
rawPos: rand([numMagnets, 2]),
posT: transpose(rawPos),
magX: ((posT_0) ≻ [-1.5, 1.5]),
magY: ((posT_1) ≻ [-1, 1]),

; Animated angles
t: Time(),
baseAngles: (rand([numMagnets]) × 6.28),
speeds: (rand([numMagnets]) ≻ [0.1, 0.5]),
angles: (baseAngles + (t × speeds)),

; Pole positions
d: 0.12,
nPoleX: (magX + (d × cos(angles))),
nPoleY: (magY + (d × sin(angles))),
sPoleX: (magX - (d × cos(angles))),
sPoleY: (magY - (d × sin(angles))),

; Reshape for broadcasting
X3: (X ⍴ [n, n, 1]),
Y3: (Y ⍴ [n, n, 1]),
nX3: (nPoleX ⍴ [1, 1, numMagnets]),
nY3: (nPoleY ⍴ [1, 1, numMagnets]),
sX3: (sPoleX ⍴ [1, 1, numMagnets]),
sY3: (sPoleY ⍴ [1, 1, numMagnets]),

; Distances
ε: 0.0001,
rN: (√(((X3 - nX3)^2) + ((Y3 - nY3)^2)) + ε),
rS: (√(((X3 - sX3)^2) + ((Y3 - sY3)^2)) + ε),

; Potential
;k: 0.5,
; Pulsing strength
k: (0.5 + (sin(t × 2) × 0.2)),
potential: sum(((k/rN) - (k/rS)), 2),

; Visualization
lines: (abs(sin(potential × 25)) ^ 0.25),
glow: (1 / (abs(potential) + 0.2)),
field: ((lines × 0.7) + (glow × 0.3)),

(
  Text("# 🧲 Spinning Magnets"),
  lines,
)
`
} as const

const getExample = (k: keyof typeof EXAMPLES) => EXAMPLES[k].trim()

// MARK: Generated Code

const FLUENT_GENERATION_SYSTEM_PROMPT = `

You are an expert Fluent code generator. Follow these rules when generating Fluent code:

1. You generate Fluent code. Output ONLY valid code, no explanations.
2. Make sure the generated code is syntactically correct.
3. Use built-in functions and environment variables where appropriate.
4. Follow best practices for Fluent code style and structure.
5. Refer to the examples below for guidance.
6. Ensure interactive/reactive elements (Slider, Button, etc.) are part of the program's return value, i.e. inside the final parentheses.
7. Keep the code concise and efficient.
8. If you want to add label to a Slider, use Grid to compose it with a Text element – \`Grid(2)(Text("Label"), Slider(x))\`
9. Slider can use only values between 0 and 1 for its range. To map to other ranges, use arithmetic operations.

Here is some documentation for Fluent:

${Documentation}

\`\`\`fluent
${
  Object.entries(EXAMPLES).map(example => dedent`
    ; ${example[0]}
    ${example[1].trim()}
  `).join("\n\n")
}
\`\`\`

You can use the following built-in functions and environment variables:
${Object.keys(DefaultEnvironment).join(", ")}

## Operator Definitions (Prelude)
The following operators are defined as aliases to canonical functions:
\`\`\`fluent
${PRELUDE.trim()}
\`\`\`
`

const GENERATION_COMMENT_REGEX = /;;(.+?);;/g

const ANTHROPIC_API_KEY_STORAGE = "fluent-anthropic-api-key"
const getAnthropicApiKey = () => localStorage.getItem(ANTHROPIC_API_KEY_STORAGE) ?? ""
const setAnthropicApiKey = (key: string) => localStorage.setItem(ANTHROPIC_API_KEY_STORAGE, key)

let pendingGenerationRequests = new Set<string>()

async function generateFluentCode(instruction: string, context: string, apiKey: string): Promise<string> {
  console.log('[Generation] Starting...', { instruction })

  const response = await fetch('https://api.anthropic.com/v1/messages', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'x-api-key': apiKey,
      'anthropic-version': '2023-06-01',
      'anthropic-dangerous-direct-browser-access': 'true',
    },
    body: JSON.stringify({
      model: 'claude-opus-4-5-20251101',
      max_tokens: 16000,
      thinking: {
        type: 'enabled',
        budget_tokens: 10000
      },
      system: FLUENT_GENERATION_SYSTEM_PROMPT,
      messages: [{
        role: 'user',
        content: `Current code context:\n\`\`\`\n${context}\n\`\`\`\n\nGenerate Fluent code for: ${instruction}`
      }]
    })
  })

  console.log('[Generation] Response status:', response.status)

  const data = await response.json()
  console.log('[Generation] Response data:', data)

  if (data.error) {
    throw new Error(data.error.message)
  }

  // With thinking enabled, find the text block (not thinking block)
  const textBlock = data.content.find((block: any) => block.type === 'text')
  if (!textBlock) {
    throw new Error('No text block in response')
  }

  let result = textBlock.text.trim()

  // Strip markdown code fences if present
  result = result.replace(/^```(?:fluent)?\n?/gm, '').replace(/```$/gm, '').trim()

  console.log('[Generation] Generated code:', result)

  return result
}

function processGenerationComments(
  code: string,
  apiKey: string,
  onUpdate: (newCode: string) => void
): string {
  console.log('[Generation] Processing comments in code...')
  const matches = [...code.matchAll(GENERATION_COMMENT_REGEX)]
  console.log('[Generation] Found matches:', matches.length)

  for (const match of matches) {
    const fullMatch = match[0]
    const instruction = match[1].trim()
    console.log('[Generation] Processing:', { fullMatch, instruction })

    // Skip if already processing this exact comment
    if (pendingGenerationRequests.has(fullMatch)) {
      console.log('[Generation] Skipping, already pending:', fullMatch)
      continue
    }
    pendingGenerationRequests.add(fullMatch)

    // Fire off async generation
    generateFluentCode(instruction, code, apiKey)
      .then(generatedCode => {
        console.log('[Generation] Success, replacing code')
        pendingGenerationRequests.delete(fullMatch)
        const newCode = code.replace(fullMatch, generatedCode)
        onUpdate(newCode)
      })
      .catch(err => {
        pendingGenerationRequests.delete(fullMatch)
        console.error('[Generation] Failed:', err)
        onUpdate(code.replace(fullMatch, `; ERROR: ${err.message}`))
      })
  }

  return code
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
// @ts-ignore
Code.noAutoLift = true

const validateCode = (code: string) => {
  if (!mainEditorRef) { return }
  const { editor, monaco } = mainEditorRef
  const model = editor.getModel()
  if (!model) { return }

  const errors = getParseErrors(code)

  const markers = errors.map(error => ({
    startLineNumber: error.start.line,
    startColumn: error.start.column,
    endLineNumber: error.end.line,
    endColumn: error.end.column,
    message: error.message,
    severity: monaco.MarkerSeverity.Error,
  }))

  monaco.editor.setModelMarkers(model, "fluent-syntax", markers)
}

function CodeEditor(sourceCode: Signal<string>) {
  const height = SignalCreate("100%")

  const handleEditorMount: OnMount = (editor, monaco) => {
    editorOnMount(editor, monaco)
    mainEditorRef = { editor, monaco }

    // Validate on initial load
    const model = editor.getModel()
    if (model) {
      validateCode(model.getValue())
    }
  }

  return SignalComputed(() => {
    return (
      // @ts-ignore
      <Editor
        beforeMount={editorBeforeMount}
        onMount={handleEditorMount}
        // @ts-ignore
        height={SignalRead(height)}
        defaultLanguage="fluent"
        className={`${frameStyle} !p-0 overflow-hidden`}
        theme="fluentTheme"
        // @ts-ignore
        value={SignalRead(sourceCode)}
        onChange={(updatedSourceCode) => {
          SignalUpdate(sourceCode, updatedSourceCode)
          if (updatedSourceCode !== undefined) {
            validateCode(updatedSourceCode)
            // Check for generation comments and process them
            const apiKey = getAnthropicApiKey()
            if (apiKey && GENERATION_COMMENT_REGEX.test(updatedSourceCode)) {
              GENERATION_COMMENT_REGEX.lastIndex = 0 // Reset regex state
              processGenerationComments(updatedSourceCode, apiKey, (newCode) => {
                SignalUpdate(sourceCode, newCode)
                validateCode(newCode)
              })
            }
          }
        }}
        options={getEditorOptions("editable")}
      />
    )
  })
}
// @ts-ignore
CodeEditor.noAutoLift = true

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
    // Word pattern for identifiers (letter-based symbols)
    wordPattern: /(?:\p{L})[\p{L}\p{N}\-]*/gu,
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
    keybinding: monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyP,
    command: 'editor.action.quickCommand',
    when: 'editorTextFocus'
  });

  monaco.editor.addKeybindingRule({
    keybinding: monaco.KeyCode.F1,
    command: '-editor.action.quickCommand',
    when: 'editorTextFocus'
  });

  // Scope includes DefaultEnvironment + PRELUDE
  const completionScope = createScope();
  const symbolSuggestions = [
    // Greek lowercase
    { label: 'α alpha', insertText: 'α', detail: 'Greek lowercase alpha', filterText: 'alpha a' },
    { label: 'β beta', insertText: 'β', detail: 'Greek lowercase beta', filterText: 'beta b' },
    { label: 'γ gamma', insertText: 'γ', detail: 'Greek lowercase gamma', filterText: 'gamma g' },
    { label: 'δ delta', insertText: 'δ', detail: 'Greek lowercase delta', filterText: 'delta d' },
    { label: 'ε epsilon', insertText: 'ε', detail: 'Greek lowercase epsilon', filterText: 'epsilon e' },
    { label: 'ζ zeta', insertText: 'ζ', detail: 'Greek lowercase zeta', filterText: 'zeta z' },
    { label: 'η eta', insertText: 'η', detail: 'Greek lowercase eta', filterText: 'eta' },
    { label: 'θ theta', insertText: 'θ', detail: 'Greek lowercase theta', filterText: 'theta' },
    { label: 'ι iota', insertText: 'ι', detail: 'Greek lowercase iota', filterText: 'iota i' },
    { label: 'κ kappa', insertText: 'κ', detail: 'Greek lowercase kappa', filterText: 'kappa k' },
    { label: 'λ lambda', insertText: 'λ', detail: 'Greek lowercase lambda', filterText: 'lambda l' },
    { label: 'μ mu', insertText: 'μ', detail: 'Greek lowercase mu', filterText: 'mu m' },
    { label: 'ν nu', insertText: 'ν', detail: 'Greek lowercase nu', filterText: 'nu n' },
    { label: 'ξ xi', insertText: 'ξ', detail: 'Greek lowercase xi', filterText: 'xi x' },
    { label: 'π pi', insertText: 'π', detail: 'Greek lowercase pi', filterText: 'pi p' },
    { label: 'ρ rho', insertText: 'ρ', detail: 'Greek lowercase rho', filterText: 'rho r' },
    { label: 'σ sigma', insertText: 'σ', detail: 'Greek lowercase sigma', filterText: 'sigma s' },
    { label: 'τ tau', insertText: 'τ', detail: 'Greek lowercase tau', filterText: 'tau t' },
    { label: 'υ upsilon', insertText: 'υ', detail: 'Greek lowercase upsilon', filterText: 'upsilon u' },
    { label: 'φ phi', insertText: 'φ', detail: 'Greek lowercase phi', filterText: 'phi f' },
    { label: 'χ chi', insertText: 'χ', detail: 'Greek lowercase chi', filterText: 'chi c' },
    { label: 'ψ psi', insertText: 'ψ', detail: 'Greek lowercase psi', filterText: 'psi' },
    { label: 'ω omega', insertText: 'ω', detail: 'Greek lowercase omega', filterText: 'omega o' },
    // Greek uppercase
    { label: 'Γ Gamma', insertText: 'Γ', detail: 'Greek uppercase Gamma', filterText: 'Gamma G' },
    { label: 'Δ Delta', insertText: 'Δ', detail: 'Greek uppercase Delta', filterText: 'Delta D' },
    { label: 'Θ Theta', insertText: 'Θ', detail: 'Greek uppercase Theta', filterText: 'Theta' },
    { label: 'Λ Lambda', insertText: 'Λ', detail: 'Greek uppercase Lambda', filterText: 'Lambda L' },
    { label: 'Ξ Xi', insertText: 'Ξ', detail: 'Greek uppercase Xi', filterText: 'Xi X' },
    { label: 'Π Pi', insertText: 'Π', detail: 'Greek uppercase Pi / Product', filterText: 'Pi P product' },
    { label: 'Σ Sigma', insertText: 'Σ', detail: 'Greek uppercase Sigma / Sum', filterText: 'Sigma S sum summation' },
    { label: 'Φ Phi', insertText: 'Φ', detail: 'Greek uppercase Phi', filterText: 'Phi F' },
    { label: 'Ψ Psi', insertText: 'Ψ', detail: 'Greek uppercase Psi', filterText: 'Psi' },
    { label: 'Ω Omega', insertText: 'Ω', detail: 'Greek uppercase Omega', filterText: 'Omega O' },
    // Calculus & Analysis
    { label: '∇ nabla', insertText: '∇', detail: 'Nabla / Gradient', filterText: 'nabla gradient grad del' },
    { label: '∂ partial', insertText: '∂', detail: 'Partial derivative', filterText: 'partial derivative' },
    { label: '∫ integral', insertText: '∫', detail: 'Integral', filterText: 'integral int' },
    { label: '∮ contour', insertText: '∮', detail: 'Contour integral', filterText: 'contour integral oint' },
    { label: '∬ double integral', insertText: '∬', detail: 'Double integral', filterText: 'double integral iint' },
    { label: '∞ infinity', insertText: '∞', detail: 'Infinity', filterText: 'infinity inf' },
    { label: '′ prime', insertText: '′', detail: 'Prime (derivative)', filterText: 'prime derivative' },
    { label: '″ double prime', insertText: '″', detail: 'Double prime', filterText: 'double prime pprime' },
    // Arithmetic operators
    { label: '× times', insertText: '×', detail: 'Multiplication / Cross product', filterText: 'times cross multiply x' },
    { label: '÷ divide', insertText: '÷', detail: 'Division', filterText: 'divide division' },
    { label: '± plus-minus', insertText: '±', detail: 'Plus-minus', filterText: 'plus minus pm plusminus' },
    { label: '∓ minus-plus', insertText: '∓', detail: 'Minus-plus', filterText: 'minus plus mp minusplus' },
    { label: '√ sqrt', insertText: '√', detail: 'Square root', filterText: 'sqrt square root' },
    { label: '∛ cbrt', insertText: '∛', detail: 'Cube root', filterText: 'cbrt cube root' },
    { label: '· dot', insertText: '·', detail: 'Dot product / Multiplication', filterText: 'dot cdot multiply' },
    { label: '∘ compose', insertText: '∘', detail: 'Function composition', filterText: 'compose circ circle' },
    // Comparison & Relations
    { label: '≠ not equal', insertText: '≠', detail: 'Not equal', filterText: 'not equal neq !=' },
    { label: '≈ approx', insertText: '≈', detail: 'Approximately equal', filterText: 'approx approximately' },
    { label: '≡ equiv', insertText: '≡', detail: 'Equivalent / Identical', filterText: 'equiv equivalent identical' },
    { label: '≢ not equiv', insertText: '≢', detail: 'Not equivalent', filterText: 'not equiv nequiv' },
    { label: '≤ leq', insertText: '≤', detail: 'Less than or equal', filterText: 'leq less equal <=' },
    { label: '≥ geq', insertText: '≥', detail: 'Greater than or equal', filterText: 'geq greater equal >=' },
    { label: '≪ much less', insertText: '≪', detail: 'Much less than', filterText: 'much less ll' },
    { label: '≫ much greater', insertText: '≫', detail: 'Much greater than', filterText: 'much greater gg' },
    { label: '∝ proportional', insertText: '∝', detail: 'Proportional to', filterText: 'proportional propto' },
    { label: '≅ congruent', insertText: '≅', detail: 'Congruent / Isomorphic', filterText: 'congruent cong isomorphic' },
    { label: '∼ similar', insertText: '∼', detail: 'Similar to', filterText: 'similar sim tilde' },
    // Set theory
    { label: '∈ element of', insertText: '∈', detail: 'Element of', filterText: 'element of in' },
    { label: '∉ not element', insertText: '∉', detail: 'Not element of', filterText: 'not element notin' },
    { label: '∋ contains', insertText: '∋', detail: 'Contains as member', filterText: 'contains ni' },
    { label: '⊂ subset', insertText: '⊂', detail: 'Subset', filterText: 'subset sub' },
    { label: '⊃ superset', insertText: '⊃', detail: 'Superset', filterText: 'superset sup' },
    { label: '⊆ subseteq', insertText: '⊆', detail: 'Subset or equal', filterText: 'subseteq subset equal' },
    { label: '⊇ supseteq', insertText: '⊇', detail: 'Superset or equal', filterText: 'supseteq superset equal' },
    { label: '∪ union', insertText: '∪', detail: 'Union', filterText: 'union cup' },
    { label: '∩ intersection', insertText: '∩', detail: 'Intersection', filterText: 'intersection cap' },
    { label: '∅ empty set', insertText: '∅', detail: 'Empty set', filterText: 'empty set emptyset null' },
    { label: '∖ set minus', insertText: '∖', detail: 'Set minus', filterText: 'set minus setminus difference' },
    // Logic
    { label: '∀ forall', insertText: '∀', detail: 'For all', filterText: 'forall for all universal' },
    { label: '∃ exists', insertText: '∃', detail: 'There exists', filterText: 'exists exist existential' },
    { label: '∄ not exists', insertText: '∄', detail: 'There does not exist', filterText: 'not exists nexists' },
    { label: '¬ not', insertText: '¬', detail: 'Logical not', filterText: 'not neg lnot' },
    { label: '∧ and', insertText: '∧', detail: 'Logical and', filterText: 'and land wedge' },
    { label: '∨ or', insertText: '∨', detail: 'Logical or', filterText: 'or lor vee' },
    { label: '⊕ xor', insertText: '⊕', detail: 'Exclusive or / Direct sum', filterText: 'xor oplus direct sum' },
    { label: '⊗ tensor', insertText: '⊗', detail: 'Tensor product', filterText: 'tensor otimes product' },
    { label: '⊥ perpendicular', insertText: '⊥', detail: 'Perpendicular / Bottom', filterText: 'perpendicular perp bottom false' },
    { label: '⊤ top', insertText: '⊤', detail: 'Top / True', filterText: 'top true tautology' },
    { label: '⊢ proves', insertText: '⊢', detail: 'Proves / Entails', filterText: 'proves vdash entails turnstile' },
    { label: '⊨ models', insertText: '⊨', detail: 'Models / Satisfies', filterText: 'models vDash satisfies' },
    // Arrows
    { label: '→ right arrow', insertText: '→', detail: 'Right arrow', filterText: 'right arrow to ->' },
    { label: '← left arrow', insertText: '←', detail: 'Left arrow', filterText: 'left arrow from <-' },
    { label: '↔ bidir arrow', insertText: '↔', detail: 'Bidirectional arrow', filterText: 'bidir arrow leftrightarrow <->' },
    { label: '⇒ implies', insertText: '⇒', detail: 'Implies', filterText: 'implies Rightarrow =>' },
    { label: '⇐ implied by', insertText: '⇐', detail: 'Implied by', filterText: 'implied by Leftarrow <=' },
    { label: '⇔ iff', insertText: '⇔', detail: 'If and only if', filterText: 'iff Leftrightarrow biconditional <=>' },
    { label: '↑ up arrow', insertText: '↑', detail: 'Up arrow', filterText: 'up arrow uparrow' },
    { label: '↓ down arrow', insertText: '↓', detail: 'Down arrow', filterText: 'down arrow downarrow' },
    { label: '↦ maps to', insertText: '↦', detail: 'Maps to', filterText: 'maps to mapsto' },
    { label: '⟳ loop', insertText: '⟳', detail: 'Loop / Repeat', filterText: 'loop repeat cycle' },
    // Number sets
    { label: 'ℕ naturals', insertText: 'ℕ', detail: 'Natural numbers', filterText: 'naturals N natural numbers' },
    { label: 'ℤ integers', insertText: 'ℤ', detail: 'Integers', filterText: 'integers Z integer numbers' },
    { label: 'ℚ rationals', insertText: 'ℚ', detail: 'Rational numbers', filterText: 'rationals Q rational numbers' },
    { label: 'ℝ reals', insertText: 'ℝ', detail: 'Real numbers', filterText: 'reals R real numbers' },
    { label: 'ℂ complex', insertText: 'ℂ', detail: 'Complex numbers', filterText: 'complex C complex numbers' },
    // Other useful symbols
    { label: '° degree', insertText: '°', detail: 'Degree', filterText: 'degree deg' },
    { label: '‖ norm', insertText: '‖', detail: 'Norm', filterText: 'norm parallel Vert' },
    { label: '∥ parallel', insertText: '∥', detail: 'Parallel', filterText: 'parallel' },
    { label: '⟨ langle', insertText: '⟨', detail: 'Left angle bracket', filterText: 'langle left angle bracket' },
    { label: '⟩ rangle', insertText: '⟩', detail: 'Right angle bracket', filterText: 'rangle right angle bracket' },
    { label: '⌊ floor', insertText: '⌊', detail: 'Floor (left)', filterText: 'floor lfloor' },
    { label: '⌋ floor', insertText: '⌋', detail: 'Floor (right)', filterText: 'floor rfloor' },
    { label: '⌈ ceil', insertText: '⌈', detail: 'Ceiling (left)', filterText: 'ceil ceiling lceil' },
    { label: '⌉ ceil', insertText: '⌉', detail: 'Ceiling (right)', filterText: 'ceil ceiling rceil' },
    { label: 'ℏ hbar', insertText: 'ℏ', detail: 'Reduced Planck constant', filterText: 'hbar planck' },
    { label: 'ℓ ell', insertText: 'ℓ', detail: 'Script small l', filterText: 'ell script l' },
    { label: '℘ wp', insertText: '℘', detail: 'Weierstrass p', filterText: 'wp weierstrass' },
    { label: 'ℵ aleph', insertText: 'ℵ', detail: 'Aleph (cardinal)', filterText: 'aleph cardinal' },
  ];
  monaco.languages.registerCompletionItemProvider('fluent', {
    provideCompletionItems: (model, position) => {
      const word = model.getWordUntilPosition(position);

      const range = {
        startLineNumber: position.lineNumber,
        endLineNumber: position.lineNumber,
        startColumn: word.startColumn,
        endColumn: word.endColumn,
      };

      const symbolSuggestionItems = symbolSuggestions.map(item => ({
        label: item.label,
        kind: monaco.languages.CompletionItemKind.Text,
        insertText: item.insertText,
        detail: item.detail,
        range,
        filterText: item.filterText,
      }));

      // Collect all keys from scope including parent scopes (prototype chain)
      const allKeys: string[] = [];
      for (const key in completionScope) {
        allKeys.push(key);
      }

      return ({
        suggestions: [
          ...symbolSuggestionItems,
          ...allKeys.map((key) => {
          const value = completionScope[key];
          return ({
            label: key,
            kind: monaco.languages.CompletionItemKind.Function,
            insertText: key,
            documentation: `Function: ${key}`,
            detail: typeof value === 'function' ? 'function' : String(value).slice(0, 50),
            range,
            filterText: key,
          })
        }),
        ]
      })
    }
  });
}

const editorOnMount: OnMount = (editor, monaco) => {
  // Create command to access quick input service
  const quickInputCommand = editor.addCommand(0, (accessor, func) => {
    const quickInputService = accessor.get(IQuickInputService)
    func(quickInputService)
  })

  editor.addAction({
    id: "fluent-load-example",
    label: "Load example",
    contextMenuGroupId: "navigation",
    contextMenuOrder: 1.5,
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyO],

    run: (editor) => {
      const exampleNames = Object.keys(EXAMPLES) as (keyof typeof EXAMPLES)[]

      // Get first comment line as description
      const getDescription = (code: string) => {
        const firstLine = code.trim().split('\n')[0] ?? ''
        return firstLine.startsWith(';') ? firstLine.replace(/^;\s*/, '') : ''
      }

      const items = exampleNames.map(name => ({
        label: name,
        description: getDescription(EXAMPLES[name])
      }))

      editor.trigger("", quickInputCommand!, (quickInput: any) => {
        quickInput.pick(items, {
          placeHolder: 'Select an example to load'
        }).then((selected: any) => {
          if (selected) {
            editor.getModel()?.setValue(getExample(selected.label))
          }
        })
      })
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

  editor.addAction({
    id: "fluent-set-api-key",
    label: "Set Anthropic API Key",
    contextMenuGroupId: "navigation",
    contextMenuOrder: 1.6,
    run: (editor) => {
      editor.trigger("", quickInputCommand!, (quickInput: any) => {
        quickInput.input({
          placeHolder: 'Enter your Anthropic API key (sk-ant-...)',
          value: getAnthropicApiKey(),
          password: true,
        }).then((value: string | undefined) => {
          if (value !== undefined) {
            setAnthropicApiKey(value)
            console.log('API key saved to localStorage')
          }
        })
      })
    },
  });

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
    return evaluateSyntaxTreeNode(CodeParse(code.value), createScope()) ?? null;
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

document.addEventListener("DOMContentLoaded", () => {
  const root = document.getElementById("root")

  if (!root) {
    throw new Error("Root element not found")
  }

  createRoot(root).render(<Playground />)
})
