// A program written before a cell applied was its value applied, said the way Fluent says
// it now: a read s() is s, a write s(v) is s← v – (s← v) as an operand – FunctionNoAutoLift(f)
// is f, and a data slot ~~ is a $. A name is a cell where the expression that binds it makes
// one, through its lexical scope. A saved link carries the version it was written in, and an
// older one is migrated as it loads (protocol.ts, VERSION).
import { CodeParse, type SyntaxTreeNode } from "./language"

const MAKERS = new Set(["$", "signal", "SignalCreate", "Reactive", "SignalComputed", "~", "var", "TensorVariable", "~~", "data", "TensorData",
  "Storage", "Time", "MousePosition", "Camera", "Microphone", "MicrophoneSpectrum", "Fetch", "LoadTensorFromImageUrl", "LoadSafeTensorFromURL", "Density"])
const RENAMED = new Map([["~~", "$"], ["data", "$"], ["TensorData", "SignalCreate"]])
type Edit = { from: number, to: number, text: string }
type Scope = { names: Map<string, boolean>, up: Scope | null }
const lookup = (s: Scope | null, n: string): boolean | undefined => s ? s.names.get(n) ?? lookup(s.up, n) : undefined
const nameOf = (n: SyntaxTreeNode): string | undefined => n.type === "Symbol" ? n.content.value : undefined
const isCell = (e: SyntaxTreeNode): boolean => e.type === "Operation"
  && (nameOf(e.content.operator) === ":" ? isCell(e.content.args.content.value[1]!) : MAKERS.has(nameOf(e.content.operator) ?? ""))
// the names a scope binds, outside the lambdas in it
const hoist = (n: SyntaxTreeNode, scope: Scope): void => {
  if (n.type === "Operation") {
    const [name, value] = n.content.args.content.value
    if (nameOf(n.content.operator) === ":" && name && value && nameOf(name)) { scope.names.set(nameOf(name)!, isCell(value)) }
    hoist(n.content.operator, scope)
    n.content.args.content.value.forEach((a) => hoist(a, scope))
  } else if (n.type === "Program") { n.content.forEach((x) => hoist(x, scope)) } else if (n.type === "List" || n.type === "Tensor") { n.content.value.forEach((x) => hoist(x, scope)) }
}

// The edits, by offset into src; `cells` names what is known to hold a cell – a lambda's
// parameter, a name from around the program
export const migrations = (src: string, cells: string[] = []): Edit[] => {
  const tree = CodeParse(src), edits: Edit[] = []
  if (tree.type === "Error") { return edits }
  const starts = [0]
  for (let i = 0; i < src.length; i++) { if (src[i] === "\n") { starts.push(i + 1) } }
  const at = (p: { line: number, column: number }) => starts[p.line - 1]! + p.column - 1
  const walk = (n: SyntaxTreeNode, scope: Scope, operand: boolean): void => {
    if (n.type === "Lambda") {
      const inner: Scope = { names: new Map(n.content.args.map((p) => [nameOf(p)!, cells.includes(nameOf(p)!)])), up: scope }
      hoist(n.content.expr, inner)
      walk(n.content.expr, inner, false)
    } else if (n.type === "Program") { n.content.forEach((x) => walk(x, scope, false)) } else if (n.type === "List" || n.type === "Tensor") {
      n.content.value.forEach((x) => walk(x, scope, false))
    } else if (n.type === "Operation") {
      const op = nameOf(n.content.operator), args = n.content.args.content.value, call = !n.content.args.content.synthetic
      const open = at(n.content.args.origin.start), close = at(n.origin.end) - 1
      if (op && RENAMED.has(op) && lookup(scope, op) === undefined) {
        edits.push({ from: at(n.content.operator.origin.start), to: at(n.content.operator.origin.end), text: RENAMED.get(op)! })
      } else if (call && op && lookup(scope, op) && args.length < 2) {
        if (!args.length) { edits.push({ from: open, to: close + 1, text: "" }) } else {
          if (operand) { edits.push({ from: at(n.origin.start), to: at(n.origin.start), text: "(" }) }
          edits.push({ from: open, to: open + 1, text: "← " }, ...(operand ? [] : [{ from: close, to: close + 1, text: "" }]))
        }
      } else if (call && op === "FunctionNoAutoLift" && args.length === 1) {
        edits.push({ from: at(n.origin.start), to: open + 1, text: "" }, { from: close, to: close + 1, text: "" })
      } else { walk(n.content.operator, scope, false) }
      args.forEach((a) => walk(a, scope, !call))
    }
  }
  const top: Scope = { names: new Map(), up: { names: new Map(cells.map((c) => [c, true])), up: null } }
  hoist(tree, top)
  walk(tree, top, false)
  return edits
}
export const applied = (src: string, edits: Edit[]): string =>
  [...edits].sort((a, b) => b.from - a.from).reduce((s, e) => s.slice(0, e.from) + e.text + s.slice(e.to), src)
export const migrate = (src: string, cells: string[] = []): string => applied(src, migrations(src, cells))
