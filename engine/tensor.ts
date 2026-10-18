// Fluent's tensors over the engine. A Tensor is an immutable graph node of f32 values,
// computed when something reads it. The engine owns the buffers – nothing is freed by hand.

import type { UOp } from "./ir.ts";
import * as ir from "./ir.ts";
import { CPU, type HostData } from "./cpu.ts";
import { GPU } from "./gpu.ts";
import { vjp as graphVjp } from "./grad.ts";

type Axis = number | number[] | null | undefined;
export type Like = Tensor | number | boolean | readonly unknown[];

// A·B at any rank: Σₖ A[…,i,k]·B[…,k,j] over the view algebra; a frame on either operand
// maps the cell over it, and an unframed operand is shared across the frame
export const cellMatmul = (A: UOp, B: UOp): UOp => {
  const as = A.shape, bs = B.shape;
  if (as.length < 2 || bs.length < 2) throw new Error(`matmul: rank ≥ 2 operands, got [${as}]·[${bs}]`);
  const [m, k] = as.slice(-2), [k2, n] = bs.slice(-2);
  if (k !== k2) throw new Error(`matmul: cell [m,k]·[k,n] expected, got [${as.slice(-2)}]·[${bs.slice(-2)}]`);
  const aF = as.slice(0, -2), bF = bs.slice(0, -2);
  if (aF.length && bF.length && String(aF) !== String(bF)) throw new Error(`matmul: frames disagree, [${aF}] vs [${bF}]`);
  const F = aF.length ? aF : bF, T = [...F, m!, k!, n!];
  const p = ir.mul(ir.broadcastTo(ir.reshape(A, as.length > 2 ? [...aF, m!, k!, 1] : [m!, k!, 1]), T),
    ir.broadcastTo(ir.reshape(B, bs.length > 2 ? [...bF, 1, k!, n!] : [1, k!, n!]), T));
  return ir.reshape(ir.reduceAxis(p, F.length + 1, true), [...F, m!, n!]);
};

// ── realization ─────────────────────────────────────────────────────────────────
// A computed node gets one stable leaf that stands for it in every later graph, so nothing
// is computed twice and a long-lived value never drags its history along: a data leaf when
// the CPU computed it, a device leaf naming its buffer when a GPU did. The buffer lives as
// long as that leaf. A small graph over host data runs on the CPU — the metadata a program
// computes (axes, shapes, index tables) never waits on the GPU; the work, not the size of
// the inputs, decides: small weights still make a large training step.
const cpu = new CPU();
let gpu: GPU | null = null;
const SMALL = 1 << 16;
const leafOf = new WeakMap<UOp, UOp>();
const hostData = new WeakMap<UOp, Float32Array>();
const onDevice = new Map<string, GPUBuffer>();
const freeOnCollect = new FinalizationRegistry<string>((name) => { const b = onDevice.get(name); onDevice.delete(name); if (b) gpu?.release(b); });
let nextLeaf = 0;
// A rank-0 leaf is a one-element leaf seen as rank 0, on the host (see `scalar`) and on the
// device alike, so a scalar that crosses time keeps one structure and its step one plan.
const stored = (u: UOp): UOp =>
  u.op === "RESHAPE" && !u.shape.length && (u.src[0]!.op === "CONSTANT" || u.src[0]!.op === "BUFFER") ? u.src[0]! : u;
const heldData = (u: UOp): Float32Array | undefined => { const s = stored(u); return s.op === "CONSTANT" ? (s.arg as Float32Array) : undefined; };
const isLeaf = (u: UOp) => stored(u).op === "BUFFER" || heldData(u) !== undefined;
const rank0 = (leaf: UOp, shape: readonly number[]): UOp => (shape.length ? leaf : ir.reshape(leaf, []));
const dataOf = (u: UOp): Float32Array | undefined => heldData(leafOf.get(u) ?? u) ?? hostData.get(u);
// the leaf standing for a node, if it has one, and whether the host can't read it cheaply
const leaf = (u: UOp): UOp | undefined => leafOf.get(u) ?? (isLeaf(u) ? u : undefined);
const remote = (l: UOp): boolean => stored(l).op === "BUFFER" || ir.numel(l.shape) > SMALL;
// the edges a walk follows: none out of a leaf
const toLeaves = (u: UOp) => (leaf(u) ? [] : u.src);
function cut(roots: UOp[]): { roots: UOp[]; hostOnly: boolean } {
  const memo = new Map<number, UOp>();
  let hostOnly = true, work = 0;
  for (const u of ir.toposort(roots, toLeaves)) {
    const l = leaf(u);
    if (l) { memo.set(u.id, l); hostOnly &&= !remote(l); continue; }
    const srcs = u.src.map((x) => memo.get(x.id)!);
    memo.set(u.id, srcs.every((x, i) => x === u.src[i]) ? u : ir.node(u.op, srcs, u.arg, u.shape, u.site));
    if (u.src.length) work += ir.numel(u.shape);
  }
  return { roots: roots.map((r) => memo.get(r.id)!), hostOnly: hostOnly && work <= SMALL };
}
function compute(nodes: readonly UOp[]): void {
  const need = [...new Set(nodes.filter((n) => !leafOf.has(n) && !isLeaf(n)))];
  if (!need.length) return;
  const { roots, hostOnly } = cut(need);
  if (!gpu || hostOnly) {
    cpu.realizeSync(roots).forEach((t, i) => {
      const n = need[i]!, size = ir.numel(n.shape), d = t.data.length === size ? t.data : t.data.subarray(0, size);   // a kernel writes at least one element
      hostData.set(n, d); leafOf.set(n, held(d, n.shape));
    });
    return;
  }
  gpu.computeSync(roots, (name) => onDevice.get(name)).forEach((b, i) => {
    const n = need[i]!, name = `%v${nextLeaf++}`, buf = ir.buffer(name, n.shape.length ? n.shape : [1]), leaf = rank0(buf, n.shape);
    onDevice.set(name, b); freeOnCollect.register(buf, name);
    leafOf.set(n, leaf); leafOf.set(leaf, leaf);
  });
}
// the site the ops built now come from – the Fluent call the language is making
export const at = ir.at;
// compute without reading: what a step commits stays where it was computed
export const materialize = (arrays: readonly Tensor[]): void => compute(arrays.map((a) => a.node));
// The work a node's graph would take on the host, or Infinity when it reads the GPU or a
// large constant – memoized per node, so asking along a growing chain stays linear.
const hostWorkMemo = new WeakMap<UOp, number>();
const hostWork = (root: UOp): number => {
  for (const u of ir.toposort(root, (u) => (hostWorkMemo.has(u) ? [] : toLeaves(u)))) {
    if (hostWorkMemo.has(u)) continue;
    const l = leaf(u);
    hostWorkMemo.set(u, l ? (remote(l) ? Infinity : 0) : u.src.reduce((w, x) => w + hostWorkMemo.get(x)!, ir.numel(u.shape)));
  }
  return hostWorkMemo.get(root)!;
};
// a value's data when it is on the host or cheap to compute there — never a GPU read
export function hostValue(a: Tensor): Float32Array | undefined {
  const known = dataOf(a.node);
  if (known || leafOf.has(a.node)) return known;
  if (gpu && hostWork(a.node) > SMALL) return undefined;
  compute([a.node]);
  return dataOf(a.node);
}
// a value the GPU computed, read back once and kept on the host beside its leaf
const deviceBuffer = (n: UOp): GPUBuffer => onDevice.get(ir.bufName(stored(leafOf.get(n) ?? n)))!;
const keep = (n: UOp, data: Float32Array): Float32Array => { hostData.set(n, data); hostData.set(leafOf.get(n)!, data); return data; };
async function readAsync(a: Tensor): Promise<Float32Array> {
  compute([a.node]);
  return dataOf(a.node) ?? keep(a.node, await gpu!.readAsync(deviceBuffer(a.node), ir.numel(a.node.shape)));
}
function realize(arrays: readonly Tensor[]): Float32Array[] {
  compute(arrays.map((a) => a.node));
  return arrays.map((a) => dataOf(a.node) ?? keep(a.node, gpu!.readSync(deviceBuffer(a.node), ir.numel(a.node.shape))));
}
// A value that outlives its step stands on its leaf — computed, with no history behind it
// for later graphs, and later ∇ walks, to drag along. A scalar is held as data, never kernel
// text, so its next value binds the same plan.
export function settle(a: Tensor): Tensor {
  compute([a.node]);
  const l = leafOf.get(a.node) ?? a.node;
  return new Tensor(l.op === "CONSTANT" && !l.shape.length ? held(l.arg as Float32Array, []) : l);
}
// how many nodes stand behind a value – 1 for a leaf: what crosses time must not grow
export function graphSize(a: Tensor): number {
  const seen = new Set<number>(), stack = [leafOf.get(a.node) ?? a.node];
  while (stack.length) { const u = stack.pop()!; if (seen.has(u.id)) continue; seen.add(u.id); if (!isLeaf(u)) stack.push(...u.src.map((x) => leafOf.get(x) ?? x)); }
  return seen.size;
}
// the device buffer holding a value, for drawing it without a read
function gpuBufferOf(a: Tensor): GPUBuffer {
  if (!gpu) throw new Error("gpuBuffer: no GPU");
  compute([a.node]);
  const l = stored(leafOf.get(a.node) ?? a.node);
  if (l.op === "CONSTANT") {                                                   // host data: upload it once, as a device leaf
    const [b] = gpu.computeSync([l], () => undefined), name = `%v${nextLeaf++}`, dl = ir.buffer(name, l.shape), r = rank0(dl, a.node.shape);
    onDevice.set(name, b!); freeOnCollect.register(dl, name);
    leafOf.set(a.node, r); leafOf.set(r, r); keep(a.node, l.arg as Float32Array);
  }
  return deviceBuffer(a.node);
}

const nest = (d: ArrayLike<number>, shape: readonly number[]): unknown => {
  if (!shape.length) return d[0]!;
  const st = ir.strides(shape);
  const go = (axis: number, off: number): unknown[] =>
    Array.from({ length: shape[axis]! }, (_, i) => (axis === shape.length - 1 ? d[off + i]! : go(axis + 1, off + i * st[axis]!)));
  return go(0, 0);
};

// ── folding ─────────────────────────────────────────────────────────────────────
// A value that depends only on constants is computed as it is built and never becomes a
// node: shape arithmetic, index tables and literals cost what their arithmetic costs, and
// never keep a plan from being found. Only small values fold – a large one stays lazy and
// fuses into what reads it – and never a value ∇ differentiates with respect to. Each op
// computes what its kernel computes: f32 storage rounds every result, a reduction
// accumulates in order.
const FOLD = 256;
const FOLDS = new Set(["ADD", "MULTIPLY", "RECIPROCAL", "EXP", "LOG", "SQRT", "LESS_THAN", "WHERE", "DETACH", "MATH", "RANDOM", "IOTA", "REDUCE",
  "BROADCAST", "PERMUTE", "RESHAPE", "SLICE", "WINDOW", "PAD", "GATHER", "SCATTER_ADD"]);
const differentiable = new WeakSet<UOp>();
const foldMemo = new WeakMap<UOp, boolean>();
function isData(u: UOp): boolean {
  return u.op === "CONSTANT" || (u.op === "RESHAPE" && !u.shape.length && u.src[0]!.op === "CONSTANT" && ir.sameShape(u.src[0]!.shape, [1]));
}
function foldable(root: UOp): boolean {
  const stack: UOp[] = [root];
  while (stack.length) {
    const u = stack[stack.length - 1]!;
    if (foldMemo.has(u)) { stack.pop(); continue; }
    let f: boolean;
    if (differentiable.has(u)) f = false;
    else if (isData(u)) f = true;
    else if (!FOLDS.has(u.op) || ir.numel(u.shape) > FOLD) f = false;
    else {
      if (u.src.some((s) => foldMemo.get(s) === false)) f = false;
      else {
        const pending = u.src.filter((s) => !foldMemo.has(s));
        if (pending.length) { stack.push(...pending); continue; }
        f = true;
      }
    }
    foldMemo.set(u, f); stack.pop();
  }
  return foldMemo.get(root)!;
}
function evaluate(u: UOp, ins: Float32Array[]): Float32Array {
  const n = ir.numel(u.shape), out = new Float32Array(n), [a, b, c] = ins;
  const map = (f: (i: number) => number) => { for (let i = 0; i < n; i++) out[i] = f(i); return out; };
  switch (u.op) {
    case "CONSTANT": return u.arg as Float32Array;
    case "IOTA": return map((i) => i);
    case "RANDOM": return map((i) => ir.hashUniform(a![0]!, i));
    case "ADD": return map((i) => a![i]! + b![i]!);
    case "MULTIPLY": return map((i) => a![i]! * b![i]!);
    case "RECIPROCAL": return map((i) => 1 / a![i]!);
    case "EXP": return map((i) => Math.exp(a![i]!));
    case "LOG": return map((i) => Math.log(a![i]!));
    case "SQRT": return map((i) => Math.sqrt(a![i]!));
    case "LESS_THAN": return map((i) => (a![i]! < b![i]! ? 1 : 0));
    case "WHERE": return map((i) => (a![i]! !== 0 ? b![i]! : c![i]!));
    case "DETACH": case "RESHAPE": return a!;
    case "MATH": { const f = ir.MATHS[(u.arg as { fn: string }).fn]!.js; return map((i) => (b ? f(a![i]!, b[i]!) : f(a![i]!))); }
    case "BROADCAST": case "PERMUTE": case "SLICE": case "WINDOW": {
      const m = ir.viewMap(ir.node(u.op, [ir.data(a!, u.src[0]!.shape)], u.arg, u.shape)), os = ir.strides(u.shape);
      return map((i) => { let off = m.offset; for (let d = 0; d < os.length; d++) off += Math.floor(i / os[d]!) % u.shape[d]! * m.strides[d]!; return a![off]!; });
    }
    case "PAD": {
      const { axis, before } = u.arg as { axis: number; before: number }, os = ir.strides(u.shape), ss = ir.strides(u.src[0]!.shape), len = u.src[0]!.shape[axis]!;
      return map((i) => {
        let off = 0;
        for (let d = 0; d < os.length; d++) { const k = Math.floor(i / os[d]!) % u.shape[d]! - (d === axis ? before : 0); if (k < 0 || (d === axis && k >= len)) return 0; off += k * ss[d]!; }
        return a![off]!;
      });
    }
    case "REDUCE": {
      const { axis, op } = u.arg as { axis: number; op: "ADD" | "MAX" | "MUL" }, s = u.src[0]!.shape, st = ir.strides(s), len = s[axis]!;
      const outer = ir.numel(s.slice(0, axis)), inner = ir.numel(s.slice(axis + 1));
      for (let o = 0; o < outer; o++) for (let j = 0; j < inner; j++) {
        let acc = op === "MAX" ? -Infinity : op === "MUL" ? 1 : 0;
        for (let k = 0; k < len; k++) { const v = a![o * len * inner + k * st[axis]! + j]!; acc = op === "MAX" ? Math.max(acc, v) : Math.fround(op === "MUL" ? acc * v : acc + v); }
        out[o * inner + j] = acc;
      }
      return out;
    }
    case "GATHER": { const R = ir.numel(u.shape.slice(1)); return map((i) => a![Math.trunc(b![Math.floor(i / R)]!) * R + (i % R)] ?? NaN); }
    case "SCATTER_ADD": {
      const R = ir.numel(u.shape.slice(1));
      for (let r = 0; r < a!.length; r++) for (let j = 0; j < R; j++) { const at = Math.trunc(a![r]!) * R + j; out[at] = out[at]! + b![r * R + j]!; }
      return out;
    }
  }
  throw new Error(`fold: cannot compute ${u.op}`);
}
const constData = (u: UOp): Float32Array => (u.op === "CONSTANT" ? u.arg : u.src[0]!.arg) as Float32Array;
const isLiteral = (u: UOp): boolean => u.op === "CONSTANT" && !u.shape.length;
// the result as data: a scalar from literals alone is a literal; anything else is held data
const folded = (d: Float32Array, shape: readonly number[], literal: boolean): UOp => {
  const v = d.length === ir.numel(shape) ? d : d.slice(0, ir.numel(shape));
  return literal && !shape.length ? ir.data(v, []) : held(v, shape);
};
const VIEWS = new Set(["BROADCAST", "PERMUTE", "RESHAPE", "SLICE"]);
function fold(root: UOp): UOp {
  if (isData(root)) return root;
  if (differentiable.has(root) || !FOLDS.has(root.op) || ir.numel(root.shape) > FOLD) return root;
  // the common case – one op whose sources are data, or a constructor's view of data –
  // computed directly
  if (root.op !== "IOTA" && root.op !== "RANDOM" && root.src.every((s) => isData(s) || (VIEWS.has(s.op) && isData(s.src[0]!) && !differentiable.has(s)))) {
    const ins = root.src.map((s) => (isData(s) ? constData(s) : evaluate(s, [constData(s.src[0]!)])));
    return folded(evaluate(root, ins), root.shape, root.src.every((s) => isLiteral(isData(s) ? s : s.src[0]!)));
  }
  if (!foldable(root)) return root;
  const vals = new Map<UOp, Float32Array>();
  let literal = true;
  for (const u of ir.toposort(root)) {
    if (isData(u) && u.shape.length) literal = false;                         // a tensor or a held scalar: the result is data too
    if (u.op === "IOTA" || u.op === "RANDOM") literal = false;
    vals.set(u, isData(u) ? constData(u) : evaluate(u, u.src.map((s) => vals.get(s)!)));
  }
  return folded(vals.get(root)!, root.shape, literal);
}
// the value ∇ differentiates with respect to: an identity of its own, never folded away
export const traced = (a: Tensor): Tensor => { const n = ir.reshape(a.node, a.node.shape); differentiable.add(n); return new Tensor(n); };

// ── the value ───────────────────────────────────────────────────────────────────
export class Tensor {
  readonly node: UOp;
  constructor(node: UOp) { this.node = fold(node); }
  get shape(): number[] { return this.node.shape.slice(); }
  get ndim(): number { return this.node.shape.length; }
  get size(): number { return ir.numel(this.node.shape); }
  readSync(): Float32Array { return realize([this])[0]!; }
  read(): Promise<Float32Array> { return readAsync(this); }
  async gpuBuffer(): Promise<GPUBuffer> { return gpuBufferOf(this); }
  js(): any { return nest(this.readSync(), this.node.shape); }
  item(): number { return this.readSync()[0]!; }
  // x.slice(i, [a], [a, b], …): an integer drops its axis, [a] runs to the end, [a, b] is a range
  slice(...spec: (number | [number] | [number, number])[]): Tensor {
    let n = this.node;
    const drop: number[] = [];
    spec.forEach((s, axis) => {
      const len = n.shape[axis]!;
      const norm = (v: number) => (v < 0 ? v + len : v);
      const [a, b] = typeof s === "number" ? [norm(s), norm(s) + 1] : [norm(s[0]), s.length > 1 ? norm(s[1]!) : len];
      if (typeof s === "number") drop.push(axis);
      n = ir.slice(n, axis, Math.max(0, a), Math.max(0, Math.min(len, b) - Math.max(0, a)));
    });
    return new Tensor(drop.length ? ir.reshape(n, n.shape.filter((_, i) => !drop.includes(i))) : n);
  }
  *[Symbol.iterator](): IterableIterator<Tensor> { for (let i = 0; i < (this.node.shape[0] ?? 0); i++) yield this.slice(i); }
  toString(): string { return `Tensor([${this.node.shape}])`; }
}

// ── construction ────────────────────────────────────────────────────────────────
const flatten = (v: unknown, out: number[], shape: number[], depth: number): void => {
  if (v instanceof Tensor) { const d = realize([v])[0]!; if (depth === shape.length) shape.push(...v.node.shape); for (let i = 0; i < d.length; i++) out.push(d[i]!); return; }
  if (typeof v === "number" || typeof v === "boolean") { out.push(Number(v)); return; }
  if (ArrayBuffer.isView(v)) { const xs = v as unknown as ArrayLike<number>; if (depth === shape.length) shape.push(xs.length); for (let i = 0; i < xs.length; i++) out.push(Number(xs[i])); return; }
  const xs = v as readonly unknown[];
  if (depth === shape.length) shape.push(xs.length);
  for (const x of xs) flatten(x, out, shape, depth + 1);
};
export function tensor(values: unknown, opts: { shape?: readonly number[] } = {}): Tensor {
  if (values instanceof Tensor) return values;
  let data: Float32Array, shape: number[] = [];
  if (ArrayBuffer.isView(values)) {                                            // a typed array is data already – no trip through a JS array
    data = values instanceof Float32Array ? values.slice() : Float32Array.from(values as unknown as ArrayLike<number>);
    shape = [data.length];
  } else { const flat: number[] = []; flatten(values, flat, shape, 0); data = Float32Array.from(flat); }
  const s = opts.shape ?? shape;
  if (ir.numel(s) !== data.length) throw new Error(`tensor: ${data.length} values do not fill shape [${s}]`);
  return new Tensor(ir.data(data, s));
}
// one value seen at every position: a broadcast constant, never a buffer of copies
export const full = (shape: readonly number[], value: number): Tensor => new Tensor(shape.length ? ir.broadcastTo(ir.konst(value), shape) : ir.konst(value));
export const zeros = (shape: readonly number[]): Tensor => full(shape, 0);
export function arange(start: number, stop?: number, step = 1): Tensor {
  const [a, b] = stop === undefined ? [0, start] : [start, stop];
  const n = Math.max(0, Math.ceil((b - a) / step));
  const i = ir.iota(n), scaled = step === 1 ? i : ir.mul(i, ir.konst(step));
  return new Tensor(a === 0 ? scaled : ir.add(scaled, ir.konst(a)));
}
// a scalar held as data – a one-element buffer seen as rank 0 – never folded into a
// kernel as a literal: a value that changes every frame (a clock, a slider, a computed
// number) reuses the compiled plan
const held = (d: Float32Array, shape: readonly number[]): UOp => (shape.length ? ir.data(d, shape) : ir.reshape(ir.data(d, [1]), []));
export const scalar = (v: number): Tensor => new Tensor(held(Float32Array.of(v), []));

// ── elementwise ─────────────────────────────────────────────────────────────────
export const nodeOf = (v: Like): UOp => (v instanceof Tensor ? v.node : typeof v === "number" || typeof v === "boolean" ? ir.konst(Number(v)) : tensor(v).node);
const wrap = (n: UOp) => new Tensor(n);
const un = (f: (x: UOp) => UOp) => (a: Like): Tensor => wrap(f(nodeOf(a)));
const bin = (f: (x: UOp, y: UOp) => UOp) => (a: Like, b: Like): Tensor => wrap(f(nodeOf(a), nodeOf(b)));
const neg = (x: UOp) => ir.mul(x, ir.konst(-1));
const m1 = (fn: string) => un((x) => ir.math(fn, x));

export const add = bin(ir.add);
export const multiply = bin(ir.mul);
export const subtract = bin((x, y) => ir.add(x, neg(y)));
export const divide = bin((x, y) => ir.math("div", x, y));
export const reciprocal = un(ir.recip);
export const sqrt = un(ir.sqrt);
export const exp = un(ir.exp);
export const log = un(ir.log);
// A literal integer exponent is exponentiation by squaring — exact (3^2 is 9, (-2)^2 is 4)
// and differentiable as products are; any other exponent is pow.
const intPow = (x: UOp, e: number): UOp => {
  if (e === 0) return x.shape.length ? ir.broadcastTo(ir.konst(1), x.shape) : ir.konst(1);
  let n = Math.abs(e), sq = x, r: UOp | null = null;
  for (;;) { if (n & 1) r = r ? ir.mul(r, sq) : sq; n >>= 1; if (!n) break; sq = ir.mul(sq, sq); }
  return e < 0 ? ir.recip(r!) : r!;
};
export const power = (a: Like, b: Like): Tensor => {
  const x = nodeOf(a), y = nodeOf(b);
  const e = y.op === "CONSTANT" && !y.shape.length ? (y.arg as Float32Array)[0]! : NaN;
  if (Number.isInteger(e) && Math.abs(e) <= 64) { const r = intPow(x, e); return wrap(ir.sameShape(r.shape, x.shape) ? r : ir.broadcastTo(r, x.shape)); }
  return wrap(ir.math("pow", x, y));
};
export const sin = m1("sin"), cos = m1("cos"), tanh = m1("tanh"), asin = m1("asin"), acos = m1("acos"), atan = m1("atan");
export const asinh = m1("asinh"), acosh = m1("acosh"), atanh = m1("atanh"), atan2 = bin((y, x) => ir.math("atan2", y, x));
export const floor = m1("floor"), trunc = m1("trunc"), round = m1("round"), sign = m1("sign"), abs = m1("abs");
export const expm1 = m1("expm1"), log1p = m1("log1p"), cbrt = m1("cbrt"), isnan = m1("isnan");
export const maximum = bin((x, y) => ir.where(ir.cmplt(x, y), y, x));
export const minimum = bin((x, y) => ir.where(ir.cmplt(y, x), y, x));
export const less = bin(ir.cmplt);
export const equal = bin((x, y) => ir.math("eq", x, y));
// 1 where a value is nonzero; NaN counts as false
export const truthy = un((x) => ir.where(ir.cmplt(x, ir.konst(0)), ir.konst(1), ir.cmplt(ir.konst(0), x)));
export const where = (c: Like, a: Like, b: Like): Tensor => wrap(ir.where(nodeOf(c), nodeOf(a), nodeOf(b)));

// ── shape ───────────────────────────────────────────────────────────────────────
const normAxis = (axis: number, rank: number): number => {
  const a = axis < 0 ? axis + rank : axis;
  if (a < 0 || a >= rank) throw new Error(rank ? `there is no axis ${axis}: the value has ${rank} ${rank === 1 ? "axis" : "axes"}` : `a single number has no axis ${axis}`);
  return a;
};
export function reshape(a: Like, shape: readonly number[]): Tensor {
  const x = nodeOf(a), n = ir.numel(x.shape), known_ = shape.filter((d) => d !== -1).reduce((p, d) => p * d, 1);
  const s = shape.map((d) => (d === -1 ? n / known_ : d));
  if (ir.numel(s) !== n || !s.every((d) => Number.isInteger(d) && d >= 0)) throw new Error(`cannot reshape [${x.shape}] into [${shape}]`);
  return new Tensor(ir.sameShape(s, x.shape) ? x : ir.reshape(x, s));
}
export const transpose = (a: Like, axes?: readonly number[]): Tensor => { const x = nodeOf(a); return wrap(ir.permute(x, axes ? axes.map((p) => normAxis(p, x.shape.length)) : x.shape.map((_, i) => x.shape.length - 1 - i))); };
export function moveaxis(a: Like, source: number | number[], destination: number | number[]): Tensor {
  const x = nodeOf(a), r = x.shape.length;
  const src = (typeof source === "number" ? [source] : source).map((s) => normAxis(s, r));
  const dst = (typeof destination === "number" ? [destination] : destination).map((d) => normAxis(d, r));
  // the axes that stay, in order, with each moved one inserted where it goes
  const order = x.shape.map((_, i) => i).filter((i) => !src.includes(i));
  dst.map((d, i) => [d, src[i]!] as const).sort((p, q) => p[0] - q[0]).forEach(([d, s]) => order.splice(d, 0, s));
  return wrap(ir.permute(x, order));
}
export const padAxis = (a: Like, axis: number, before: number, after: number): Tensor => { const x = nodeOf(a); return wrap(ir.pad(x, normAxis(axis, x.shape.length), before, after)); };
export const windows = (a: Like, sizes: readonly number[]): Tensor => wrap(ir.windows(nodeOf(a), sizes));
export const detach = (a: Like): Tensor => wrap(ir.detach(nodeOf(a)));
export const scatterAdd = (indices: Like, values: Like, size: number): Tensor => {
  const i = nodeOf(indices), v = nodeOf(values), n = ir.numel(i.shape);
  return wrap(ir.scatterAdd(ir.reshape(i, [n]), ir.reshape(v, [n, ...v.shape.slice(i.shape.length)]), size));
};
export const broadcastTo = (a: Like, shape: readonly number[]): Tensor => { const x = nodeOf(a); return wrap(ir.sameShape(x.shape, shape) ? x : ir.broadcastTo(x, shape)); };
// Pieces whose values the host already holds – a literal's numbers, mostly – are joined by
// copying: as a graph, n pieces are n kernels, each adding the whole result again. Not
// for a piece ∇ follows, whose join must stay in the graph.
const joinOnHost = (arrays: readonly Tensor[], shapes: readonly (readonly number[])[], ax: number, shape: readonly number[]): Tensor | null => {
  const datas = arrays.map((a) => (differentiable.has(a.node) ? undefined : dataOf(a.node)));
  if (datas.some((d) => d === undefined)) return null;
  const out = new Float32Array(ir.numel(shape)), outer = ir.numel(shape.slice(0, ax));
  let at = 0;
  for (let o = 0; o < outer; o++) {
    datas.forEach((d, p) => {
      const block = ir.numel(shapes[p]!.slice(ax));
      out.set(d!.subarray(o * block, (o + 1) * block), at);
      at += block;
    });
  }
  return new Tensor(held(out, shape));
};
export function concatenate(arrays: readonly Tensor[], axis = 0): Tensor {
  if (!arrays.length) throw new Error("concatenate: needs at least one array");
  const r = arrays[0]!.ndim, ax = normAxis(axis, r);
  const total = arrays.reduce((t, a) => t + a.node.shape[ax]!, 0);
  const shape = arrays[0]!.node.shape.map((d, i) => (i === ax ? total : d));
  return joinOnHost(arrays, arrays.map((a) => a.node.shape), ax, shape) ?? new Tensor(join(arrays.map((a) => a.node), ax));
}
// a sum of pads, each piece zero-padded to the whole: a kernel adds eight of them at most,
// so n pieces join in groups of eight, and the groups in groups of eight – about n/7
// kernels, where one sum of n pads would be n − 7
const join = (xs: UOp[], ax: number): UOp => {
  if (xs.length > 8) return join(Array.from({ length: Math.ceil(xs.length / 8) }, (_, i) => join(xs.slice(8 * i, 8 * i + 8), ax)), ax);
  const total = xs.reduce((t, x) => t + x.shape[ax]!, 0);
  let off = 0;
  return xs.map((x) => { const p = ir.pad(x, ax, off, total - off - x.shape[ax]!); off += x.shape[ax]!; return p; }).reduce(ir.add);
};
export function stack(arrays: readonly Tensor[], axis = 0): Tensor {
  if (!arrays.length) throw new Error("stack: needs at least one array");
  const r = arrays[0]!.ndim + 1, ax = normAxis(axis, r);
  const shapes = arrays.map((a) => [...a.shape.slice(0, ax), 1, ...a.shape.slice(ax)]);
  const shape = [...arrays[0]!.shape.slice(0, ax), arrays.length, ...arrays[0]!.shape.slice(ax)];
  return joinOnHost(arrays, shapes, ax, shape) ?? concatenate(arrays.map((a, i) => reshape(a, shapes[i]!)), ax);
}
// take: out = x[..., idx, ...] along `axis`, idx of any shape
export function take(a: Like, indices: Like, axis?: number): Tensor {
  let x = nodeOf(a);
  if (axis === undefined) { x = ir.reshape(x, [ir.numel(x.shape)]); axis = 0; }
  const ax = normAxis(axis, x.shape.length), idx = nodeOf(indices);
  const front = ax === 0 ? x : ir.permute(x, [ax, ...x.shape.map((_, i) => i).filter((i) => i !== ax)]);
  const flatIdx = idx.shape.length === 1 ? idx : ir.reshape(idx, [Math.max(ir.numel(idx.shape), 1)]);
  let g = ir.gather(front, flatIdx);
  const rest = front.shape.slice(1), outFront = [...idx.shape, ...rest];
  if (!ir.sameShape(g.shape, outFront)) g = ir.reshape(g, outFront);
  if (ax === 0) return wrap(g);
  const k = idx.shape.length, pre = x.shape.slice(0, ax).map((_, i) => k + i);
  const order = [...pre, ...idx.shape.map((_, i) => i), ...rest.slice(ax).map((_, i) => k + ax + i)];
  return wrap(ir.permute(g, order));
}
export function meshgrid(arrays: readonly Tensor[]): Tensor[] {
  const ns = arrays.map((a) => a.size);
  const out = ns.slice(); if (out.length >= 2) [out[0], out[1]] = [ns[1]!, ns[0]!];   // "xy" indexing: the first coordinate varies along columns
  return arrays.map((a, i) => {
    const pos = arrays.length >= 2 && i < 2 ? 1 - i : i;
    const sh = out.map((_, j) => (j === pos ? ns[i]! : 1));
    return broadcastTo(reshape(a, sh), out);
  });
}

// ── reductions ──────────────────────────────────────────────────────────────────
const axesOf = (x: UOp, axis: Axis): number[] => (axis === undefined || axis === null ? x.shape.map((_, i) => i) : (typeof axis === "number" ? [axis] : axis).map((a) => normAxis(a, x.shape.length)));
function reduceN(x: UOp, axis: Axis, opts: { keepdims?: boolean } | undefined, op: "ADD" | "MAX" | "MUL"): UOp {
  const axes = axesOf(x, axis);
  for (const ax of axes) x = ir.reduceAxis(x, ax, true, op);
  return opts?.keepdims ? x : ir.reshape(x, x.shape.filter((_, i) => !axes.includes(i)));
}
const reducer = (op: "ADD" | "MAX" | "MUL", f = (x: UOp) => x) => (a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Tensor => wrap(f(reduceN(f(nodeOf(a)), axis, opts, op)));
export const sum = reducer("ADD"), prod = reducer("MUL"), max = reducer("MAX"), min = reducer("MAX", neg);
// ── order: stable, NaN last; values move through a differentiable gather ─────────
// Data the host has, or computes cheaply, is sorted there at once. Data on the GPU sorts
// there, by a bitonic network: log₂m·(log₂m + 1)/2 passes, each pairing every element
// with a partner and keeping the smaller or the larger – one gather and one select, the
// partner computed in the kernel – so nothing is read back.
export function order(d: Float32Array, R: number, n: number, desc: boolean): Float32Array {
  const out = new Float32Array(R * n);
  for (let r = 0; r < R; r++) {
    const row = d.subarray(r * n, r * n + n);
    const idx = Array.from({ length: n }, (_, i) => i).sort((i, j) => {
      const a = row[i]!, b = row[j]!, na = a !== a, nb = b !== b;
      if (na !== nb) return na ? 1 : -1;
      return (na ? 0 : desc ? b - a : a - b) || i - j;                         // equal infinities subtract to NaN: a tie too
    });
    idx.forEach((v, i) => { out[r * n + i] = v; });
  }
  return out;
}
export function bitonic(x: UOp, R: number, n: number, desc: boolean): UOp {
  let p = 0;
  while (1 << p < n) p++;
  const m = 1 << p, k = ir.konst, rows = (u: UOp) => ir.broadcastTo(u, [R, m]);
  const mod2 = (v: UOp) => ir.add(v, ir.mul(ir.math("floor", ir.mul(v, k(0.5))), k(-2)));
  const i = rows(ir.iota(m)), inside = ir.cmplt(i, k(n)), nan = ir.math("isnan", x);
  // compare by (key, tie): the value, negated to sort descending; a NaN keyed +∞ and tied
  // after every number, padding after every NaN, equal values in their order
  const key = ir.where(inside, ir.pad(ir.where(nan, k(Infinity), desc ? ir.mul(x, k(-1)) : x), 1, 0, m - n), k(Infinity));
  const tie = ir.add(i, ir.mul(ir.where(inside, ir.pad(nan, 1, 0, m - n), k(2)), k(m)));
  const pair = (a: UOp, b: UOp) => ir.add(ir.pad(ir.reshape(a, [R, m, 1]), 2, 0, 1), ir.pad(ir.reshape(b, [R, m, 1]), 2, 1, 0));
  const half = (t: UOp, c: number) => ir.reshape(ir.slice(t, 2, c, 1), [R, m]);
  const base = rows(ir.reshape(ir.mul(ir.iota(R), k(m)), [R, 1]));
  let T = pair(key, tie);
  for (let a = 1; a <= p; a++) for (let b = a - 1; b >= 0; b--) {
    const j = 1 << b, up = ir.cmplt(mod2(ir.math("floor", ir.mul(i, k(1 / (1 << a))))), k(0.5));
    const q = ir.add(i, ir.mul(k(j), ir.add(k(1), ir.mul(mod2(ir.math("floor", ir.mul(i, k(1 / j)))), k(-2)))));
    const Tq = ir.reshape(ir.gather(ir.reshape(T, [R * m, 2]), ir.reshape(ir.add(base, q), [R * m])), [R, m, 2]);
    const before = (x1: UOp, t1: UOp, x2: UOp, t2: UOp) => ir.add(ir.cmplt(x1, x2), ir.mul(ir.math("eq", x1, x2), ir.cmplt(t1, t2)));
    const [ks, ts, kq, tq] = [half(T, 0), half(T, 1), half(Tq, 0), half(Tq, 1)];
    const low = ir.math("eq", ir.cmplt(i, q), up);                             // this element keeps the smaller of the pair
    const take = ir.where(low, before(kq, tq, ks, ts), before(ks, ts, kq, tq));
    T = ir.where(ir.broadcastTo(ir.reshape(take, [R, m, 1]), [R, m, 2]), Tq, T);
  }
  const t = half(T, 1);
  return ir.slice(ir.add(t, ir.mul(ir.math("floor", ir.mul(t, k(1 / m))), k(-m))), 1, 0, n);   // tie mod m is the index
}
const scalarRank = (x: UOp) => (x.shape.length ? x : ir.reshape(x, [1]));
export function argsort(a: Like, descending = false): Tensor {
  const x = scalarRank(nodeOf(a)), n = x.shape.at(-1)!, R = ir.numel(x.shape.slice(0, -1));
  const host = hostValue(new Tensor(x));
  if (host) return new Tensor(ir.data(order(host, R, n, descending), x.shape));
  return new Tensor(ir.reshape(bitonic(ir.reshape(x, [R, n]), R, n, descending), x.shape));
}
// gather each row by its own indices: flatten rows, offset each index by its row
function gatherRows(x: UOp, idx: UOp): UOp {
  const n = x.shape.at(-1)!, k = idx.shape.at(-1)!, R = ir.numel(x.shape) / n;
  const offsets = ir.reshape(ir.broadcastTo(ir.reshape(ir.mul(ir.iota(R), ir.konst(n)), [R, 1]), [R, k]), [R * k]);
  const flat = ir.add(ir.reshape(idx, [R * k]), offsets);
  return ir.reshape(ir.gather(ir.reshape(x, [R * n, 1]), flat), [...x.shape.slice(0, -1), k]);
}
export const sort = (a: Like): Tensor => { const x = scalarRank(nodeOf(a)); return wrap(gatherRows(x, argsort(new Tensor(x)).node)); };
export function topK(a: Like, k: number): [Tensor, Tensor] {
  const x = scalarRank(nodeOf(a)), n = x.shape.at(-1)!;
  const all = argsort(new Tensor(x), true).node, idx = k === n ? all : ir.slice(all, x.shape.length - 1, 0, k);
  return [wrap(gatherRows(x, idx)), new Tensor(idx)];
}

// ── contractions ────────────────────────────────────────────────────────────────
export function matmul(a: Like, b: Like): Tensor {
  let x = nodeOf(a), y = nodeOf(b);
  const vecA = x.shape.length === 1, vecB = y.shape.length === 1;
  if (vecA) x = ir.reshape(x, [1, x.shape[0]!]);
  if (vecB) y = ir.reshape(y, [y.shape[0]!, 1]);
  const fx = x.shape.slice(0, -2), fy = y.shape.slice(0, -2);
  if (fx.length && fy.length && !ir.sameShape(fx, fy)) {                       // frames broadcast against each other, numpy-style
    const n = Math.max(fx.length, fy.length), F: number[] = [];
    for (let i = 0; i < n; i++) F.push(Math.max(fx[fx.length - n + i] ?? 1, fy[fy.length - n + i] ?? 1));
    x = ir.broadcastTo(x, [...F, ...x.shape.slice(-2)]); y = ir.broadcastTo(y, [...F, ...y.shape.slice(-2)]);
  }
  let r = cellMatmul(x, y);
  if (vecA || vecB) r = ir.reshape(r, r.shape.filter((_, i) => !((vecA && i === r.shape.length - 2) || (vecB && i === r.shape.length - 1))));
  return wrap(r);
}
// numpy's dot: x's last axis against y's second to last, every other axis kept
export function dot(a: Like, b: Like): Tensor {
  const x = nodeOf(a), y = nodeOf(b), r = y.shape.length;
  if (!x.shape.length || !r) return wrap(ir.mul(x, y));
  if (r === 1) return wrap(reduceN(ir.mul(x, ir.broadcastTo(y, x.shape)), -1, {}, "ADD"));
  const K = x.shape.at(-1)!, freeA = x.shape.slice(0, -1), others = y.shape.map((_, i) => i).filter((i) => i !== r - 2), freeB = others.map((i) => y.shape[i]!);
  const A = ir.reshape(ir.permute(x, x.shape.map((_, i) => i)), [Math.max(ir.numel(freeA), 1), K]);
  const B = ir.reshape(ir.permute(y, [r - 2, ...others]), [K, Math.max(ir.numel(freeB), 1)]);
  return wrap(ir.reshape(cellMatmul(A, B), [...freeA, ...freeB]));
}
// ── composites the natives still call ───────────────────────────────────────────
export function oneHot(a: Like, n: number): Tensor {
  const idx = nodeOf(a), shape = [...idx.shape, n];
  const hit = (x: UOp, y: UOp) => ir.mul(ir.add(ir.konst(1), ir.mul(ir.cmplt(x, y), ir.konst(-1))), ir.add(ir.konst(1), ir.mul(ir.cmplt(y, x), ir.konst(-1))));
  return wrap(hit(ir.broadcastTo(ir.reshape(idx, [...idx.shape, 1]), shape), ir.broadcastTo(ir.iota(n), shape)));
}
// N-d cross-correlation of a field with a kernel, SAME zero padding: every position's
// window of the padded field, times the kernel, summed — one reduction over the taps, the
// windows a view that never materializes. The kernel runs over the field's trailing axes;
// any axes before them are a frame.
export function conv(field: Like, kernel: Like): Tensor {
  const x = nodeOf(field), k = nodeOf(kernel), sp = x.shape, ks = k.shape, off = sp.length - ks.length;
  if (off < 0) throw new Error(`conv: a kernel of rank ${ks.length} needs a field of rank ${ks.length} or more, got [${sp}]`);
  let f = x;
  ks.forEach((K, j) => { const lo = Math.floor((K - 1) / 2); if (K > 1) f = ir.pad(f, off + j, lo, K - 1 - lo); });
  const w = ir.windows(f, ks);
  let out = ir.mul(w, ir.broadcastTo(ir.reshape(k, [...sp.map(() => 1), ...ks]), w.shape));
  for (let d = ks.length - 1; d >= 0; d--) out = ir.reduceAxis(out, sp.length + d, false);
  return wrap(out);
}
// a draw is a RANDOM leaf, so it fuses into what reads it; a seed is a number
const mix = (seed: number, salt: number) => (Math.imul((seed ^ Math.imul(salt, 0x27d4eb2f)) >>> 0, 0x165667b1) ^ salt) >>> 0;
export const uniform = (seed: number, shape: readonly number[]): Tensor => wrap(ir.random(shape, mix(seed, 1)));
// ── ∇: the engine's transpose ∘ linearize on the graph a function builds ─────────
// The derivative is with respect to a traced argument – this use of the value, never a
// copy of it the function closed over – and an outer ∇ still differentiates through it.
export function vjp<F extends (...xs: any[]) => Tensor>(f: F, primals: Tensor[]): [Tensor, (ct: unknown) => Tensor[]] {
  const leaves = primals.map(traced);
  const out = f(...leaves);
  const seed = (ct: unknown): UOp => (ct instanceof Tensor ? nodeOf(ct) : ir.broadcastTo(ir.konst(Number(ct ?? 1)), out.node.shape));
  return [out, (ct: unknown) => graphVjp(out.node, leaves.map((l) => l.node), seed(ct)).map((n) => new Tensor(n))];
}
// the gradients of value with respect to traced leaves – a ones cotangent, so a
// non-scalar value differentiates element by element
export const gradients = (value: Tensor, wrt: readonly Tensor[]): Tensor[] =>
  graphVjp(value.node, wrt.map((l) => l.node), ir.broadcastTo(ir.konst(1), value.node.shape)).map((n) => new Tensor(n));
// ── devices: WebGPU where the platform has it, the CPU everywhere ────────────────
export async function init(): Promise<void> { gpu ??= await GPU.create(); }
export const engineGpu = (): GPU | null => gpu;
// resolves once at most `depth` computations are still running on the GPU
export const gpuDrained = (depth: number): Promise<void> => gpu?.drained(depth) ?? Promise.resolve();
