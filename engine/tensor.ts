// Fluent's tensors over the engine. A Tensor is an immutable graph node, computed when
// something reads it; every value is f32, and `bool`/`int32` are tags on 0/1 and
// whole-number values. The engine owns the buffers – nothing is freed by hand.

import type { UOp } from "./ir.ts";
import * as ir from "./ir.ts";
import { CPU, type HostData } from "./cpu.ts";
import { GPU } from "./gpu.ts";
import { vjp as graphVjp } from "./grad.ts";

export type DType = "float32" | "int32" | "uint32" | "bool";
export const float32 = "float32" as const, int32 = "int32" as const, uint32 = "uint32" as const, bool = "bool" as const;
type Axis = number | number[] | null | undefined;
export type Like = Tensor | number | boolean | readonly unknown[];

// A·B at any rank: Σₖ A[…,i,k]·B[…,k,j] over the view algebra; a frame on either operand
// maps the cell over it, and an unframed operand is shared across the frame
const cellMatmul = (A: UOp, B: UOp): UOp => {
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
export interface Device {
  computeSync(roots: UOp[], resolve: (name: string) => GPUBuffer | undefined): GPUBuffer[];
  readSync(b: GPUBuffer, n: number): Float32Array;
  readAsync(b: GPUBuffer, n: number): Promise<Float32Array>;
  release(b: GPUBuffer): void;
}
const cpu = new CPU();
let device: Device | null = null;
export const useDevice = (d: Device | null): void => { device = d; };
const SMALL = 1 << 16;
const leafOf = new WeakMap<UOp, UOp>();
const hostData = new WeakMap<UOp, Float32Array>();
const onDevice = new Map<string, GPUBuffer>();
const freeOnCollect = new FinalizationRegistry<string>((name) => { const b = onDevice.get(name); onDevice.delete(name); if (b) device?.release(b); });
let nextLeaf = 0;
// A rank-0 leaf is a one-element leaf seen as rank 0, on the host (see `scalar`) and on the
// device alike, so a scalar that crosses time keeps one structure and its step one plan.
const stored = (u: UOp): UOp =>
  u.op === "RESHAPE" && !u.shape.length && (u.src[0]!.op === "CONSTANT" || u.src[0]!.op === "BUFFER") ? u.src[0]! : u;
const heldData = (u: UOp): Float32Array | undefined => { const s = stored(u); return s.op === "CONSTANT" ? (s.arg as Float32Array) : undefined; };
const isLeaf = (u: UOp) => stored(u).op === "BUFFER" || heldData(u) !== undefined;
const rank0 = (leaf: UOp, shape: readonly number[]): UOp => (shape.length ? leaf : ir.reshape(leaf, []));
const dataOf = (u: UOp): Float32Array | undefined => heldData(leafOf.get(u) ?? u) ?? hostData.get(u);
function cut(roots: UOp[]): { roots: UOp[]; hostOnly: boolean } {
  const memo = new Map<number, UOp>();
  let hostOnly = true, work = 0;
  for (const root of roots) {
    const stack: [UOp, boolean][] = [[root, false]];
    while (stack.length) {
      const [u, expanded] = stack.pop()!;
      if (memo.has(u.id)) continue;
      const l = leafOf.get(u) ?? (isLeaf(u) ? u : undefined);
      if (l) { memo.set(u.id, l); if (stored(l).op === "BUFFER" || ir.numel(l.shape) > SMALL) hostOnly = false; continue; }
      if (!u.src.length) { memo.set(u.id, u); continue; }
      if (!expanded) { stack.push([u, true]); for (const x of u.src) if (!memo.has(x.id)) stack.push([x, false]); continue; }
      const srcs = u.src.map((x) => memo.get(x.id)!);
      memo.set(u.id, srcs.every((x, i) => x === u.src[i]) ? u : ir.node(u.op, srcs, u.arg, u.shape));
      work += ir.numel(u.shape);
    }
  }
  return { roots: roots.map((r) => memo.get(r.id)!), hostOnly: hostOnly && work <= SMALL };
}
function compute(nodes: readonly UOp[]): void {
  const need = [...new Set(nodes.filter((n) => !leafOf.has(n) && !isLeaf(n)))];
  if (!need.length) return;
  const { roots, hostOnly } = cut(need);
  if (!device || hostOnly) {
    cpu.realizeSync(roots).forEach((t, i) => {
      const n = need[i]!, size = ir.numel(n.shape), d = t.data.length === size ? t.data : t.data.subarray(0, size);   // a kernel writes at least one element
      hostData.set(n, d); leafOf.set(n, held(d, n.shape));
    });
    return;
  }
  device.computeSync(roots, (name) => onDevice.get(name)).forEach((b, i) => {
    const n = need[i]!, name = `%v${nextLeaf++}`, buf = ir.buffer(name, n.shape.length ? n.shape : [1]), leaf = rank0(buf, n.shape);
    onDevice.set(name, b); freeOnCollect.register(buf, name);
    leafOf.set(n, leaf); leafOf.set(leaf, leaf);
  });
}
// compute without reading: what a step commits stays where it was computed
export const materialize = (arrays: readonly Tensor[]): void => compute(arrays.map((a) => a.node));
// The work a node's graph would take on the host, or Infinity when it reads the GPU or a
// large constant – memoized per node, so asking along a growing chain stays linear.
const hostWorkMemo = new WeakMap<UOp, number>();
const hostWork = (root: UOp): number => {
  const stack: [UOp, boolean][] = [[root, false]];
  while (stack.length) {
    const [u, expanded] = stack.pop()!;
    if (hostWorkMemo.has(u)) continue;
    const l = leafOf.get(u) ?? (isLeaf(u) ? u : undefined);
    if (l) { hostWorkMemo.set(u, stored(l).op === "BUFFER" || ir.numel(l.shape) > SMALL ? Infinity : 0); continue; }
    if (!expanded) { stack.push([u, true]); for (const x of u.src) if (!hostWorkMemo.has(x)) stack.push([x, false]); continue; }
    hostWorkMemo.set(u, Math.min(Infinity, u.src.reduce((w, x) => w + hostWorkMemo.get(x)!, ir.numel(u.shape))));
  }
  return hostWorkMemo.get(root)!;
};
// a value's data when it is on the host or cheap to compute there — never a GPU read
export function hostValue(a: Tensor): Float32Array | undefined {
  const known = dataOf(a.node);
  if (known || leafOf.has(a.node)) return known;
  if (device && hostWork(a.node) > SMALL) return undefined;
  compute([a.node]);
  return dataOf(a.node);
}
async function readAsync(a: Tensor): Promise<Float32Array> {
  compute([a.node]);
  const known = dataOf(a.node);
  if (known) return known;
  const n = a.node, leaf = leafOf.get(n)!, data = await device!.readAsync(onDevice.get(ir.bufName(stored(leaf)))!, ir.numel(n.shape));
  hostData.set(n, data); hostData.set(leaf, data);
  return data;
}
export function realize(arrays: readonly Tensor[]): Float32Array[] {
  compute(arrays.map((a) => a.node));
  return arrays.map((a) => {
    const n = a.node, d = dataOf(n);
    if (d) return d;
    const leaf = leafOf.get(n)!, data = device!.readSync(onDevice.get(ir.bufName(stored(leaf)))!, ir.numel(n.shape));
    hostData.set(n, data); hostData.set(leaf, data);
    return data;
  });
}
// A value that outlives its step stands on its leaf — computed, with no history behind it
// for later graphs, and later ∇ walks, to drag along. A scalar is held as data, never kernel
// text, so its next value binds the same plan.
export function settle(a: Tensor): Tensor {
  compute([a.node]);
  const l = leafOf.get(a.node) ?? a.node;
  return new Tensor(l.op === "CONSTANT" && !l.shape.length ? held(l.arg as Float32Array, []) : l, a.dtype);
}
// how many nodes stand behind a value – 1 for a leaf: what crosses time must not grow
export function graphSize(a: Tensor): number {
  const seen = new Set<number>(), stack = [leafOf.get(a.node) ?? a.node];
  while (stack.length) { const u = stack.pop()!; if (seen.has(u.id)) continue; seen.add(u.id); if (!isLeaf(u)) stack.push(...u.src.map((x) => leafOf.get(x) ?? x)); }
  return seen.size;
}
// the device buffer holding a value, for drawing it without a read
export function gpuBufferOf(a: Tensor): GPUBuffer {
  if (!device) throw new Error("gpuBuffer: no GPU");
  compute([a.node]);
  let leaf = stored(leafOf.get(a.node) ?? a.node);
  if (leaf.op === "CONSTANT") {                                                // host data: upload it once, as a device leaf
    const [b] = device.computeSync([leaf], () => undefined), name = `%v${nextLeaf++}`;
    const dl = ir.buffer(name, leaf.shape), data = leaf.arg as Float32Array;
    onDevice.set(name, b!); freeOnCollect.register(dl, name);
    const l = rank0(dl, a.node.shape);
    leafOf.set(a.node, l); leafOf.set(l, l); hostData.set(a.node, data); hostData.set(l, data);
    leaf = dl;
  }
  return onDevice.get(ir.bufName(leaf))!;
}

const nest = (d: ArrayLike<number>, shape: readonly number[], conv: (v: number) => unknown): unknown => {
  if (!shape.length) return conv(d[0]!);
  const st = ir.strides(shape);
  const go = (axis: number, off: number): unknown[] =>
    Array.from({ length: shape[axis]! }, (_, i) => (axis === shape.length - 1 ? conv(d[off + i]!) : go(axis + 1, off + i * st[axis]!)));
  return go(0, 0);
};

// ── folding ─────────────────────────────────────────────────────────────────────
// A value that depends only on constants is computed as it is built and never becomes a
// node: shape arithmetic, index tables and literals cost what their arithmetic costs, and
// never keep a plan from being found. Only small values fold – a large one stays lazy and
// fuses into what reads it – and never a value ∇ differentiates with respect to. Each op
// computes what its kernel computes: f32 storage rounds every result, a reduction
// accumulates in order.
const FOLD = 1 << 10;
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
function fold(root: UOp): UOp {
  if (isData(root) || !foldable(root)) return root;
  const vals = new Map<UOp, Float32Array>();
  let literal = true;
  for (const u of ir.toposort(root)) {
    if (isData(u) && u.shape.length) literal = false;                         // a tensor or a held scalar: the result is data too
    if (u.op === "IOTA" || u.op === "RANDOM") literal = false;
    vals.set(u, u.op === "RESHAPE" && isData(u) ? (u.src[0]!.arg as Float32Array) : evaluate(u, u.src.map((s) => vals.get(s)!)));
  }
  const d = vals.get(root)!, v = d.length === ir.numel(root.shape) ? d : d.slice(0, ir.numel(root.shape));
  return root.shape.length ? ir.data(v, root.shape) : literal ? ir.data(v, []) : ir.reshape(ir.data(v, [1]), []);
}
// the value ∇ differentiates with respect to: an identity of its own, never folded away
export const traced = (a: Tensor): Tensor => { const n = ir.reshape(a.node, a.node.shape); differentiable.add(n); return new Tensor(n, a.dtype); };

// ── the value ───────────────────────────────────────────────────────────────────
export class Tensor {
  readonly node: UOp;
  readonly dtype: DType;
  constructor(node: UOp, dtype: DType = float32) { this.node = fold(node); this.dtype = dtype; }
  get shape(): number[] { return this.node.shape.slice(); }
  get ndim(): number { return this.node.shape.length; }
  get size(): number { return ir.numel(this.node.shape); }
  readSync(): Float32Array { return realize([this])[0]!; }
  read(): Promise<Float32Array> { return readAsync(this); }
  async gpuBuffer(): Promise<GPUBuffer> { return gpuBufferOf(this); }
  js(): any {
    const conv = this.dtype === bool ? (v: number) => v !== 0 : (v: number) => v;
    return nest(this.readSync(), this.node.shape, conv);
  }
  item(): number { return this.readSync()[0]!; }
  astype(d: DType): Tensor { return astype(this, d); }
  reshape(s: readonly number[]): Tensor { return reshape(this, s); }
  add(x: Like): Tensor { return add(this, x); }
  sub(x: Like): Tensor { return subtract(this, x); }
  mul(x: Like): Tensor { return multiply(this, x); }
  // x.slice(i, [a], [a, b], …): an integer drops its axis, [a] runs to the end, [a, b] is a range
  slice(...spec: (number | [number] | [number, number])[]): Tensor { return Tensor.#slice(this.node, this.dtype, spec); }
  static #slice(n: UOp, dtype: DType, spec: (number | [number] | [number, number])[]): Tensor {
    const drop: number[] = [];
    spec.forEach((s, axis) => {
      const len = n.shape[axis]!;
      const norm = (v: number) => (v < 0 ? v + len : v);
      const [a, b] = typeof s === "number" ? [norm(s), norm(s) + 1] : [norm(s[0]), s.length > 1 ? norm(s[1]!) : len];
      if (typeof s === "number") drop.push(axis);
      n = ir.slice(n, axis, Math.max(0, a), Math.max(0, Math.min(len, b) - Math.max(0, a)));
    });
    return new Tensor(drop.length ? ir.reshape(n, n.shape.filter((_, i) => !drop.includes(i))) : n, dtype);
  }
  *[Symbol.iterator](): IterableIterator<Tensor> {
    const n = this.node.shape[0] ?? 0;
    for (let i = 0; i < n; i++) yield Tensor.#slice(this.node, this.dtype, [i]);
  }
  toString(): string { return `Tensor(${this.dtype}, [${this.node.shape}])`; }
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
const isBoolish = (v: unknown): boolean => typeof v === "boolean" || (Array.isArray(v) && v.length > 0 && isBoolish(v[0]));
export function tensor(values: unknown, opts: { shape?: readonly number[]; dtype?: DType } = {}): Tensor {
  if (values instanceof Tensor) return opts.dtype ? astype(values, opts.dtype) : values;
  let data: Float32Array, shape: number[] = [];
  if (ArrayBuffer.isView(values)) {                                            // a typed array is data already – no trip through a JS array
    data = values instanceof Float32Array ? values.slice() : Float32Array.from(values as unknown as ArrayLike<number>);
    shape = [data.length];
  } else { const flat: number[] = []; flatten(values, flat, shape, 0); data = Float32Array.from(flat); }
  const s = opts.shape ?? shape;
  if (ir.numel(s) !== data.length) throw new Error(`array: ${data.length} values do not fill shape [${s}]`);
  return new Tensor(ir.data(data, s), opts.dtype ?? (isBoolish(values) ? bool : values instanceof Int32Array ? int32 : float32));
}
// one value seen at every position: a broadcast constant, never a buffer of copies
export const full = (shape: readonly number[], value: number, opts: { dtype?: DType } = {}): Tensor =>
  new Tensor(shape.length ? ir.broadcastTo(ir.konst(value), shape) : ir.konst(value), opts.dtype ?? float32);
export const zeros = (shape: readonly number[], opts: { dtype?: DType } = {}): Tensor => full(shape, 0, opts);
export const ones = (shape: readonly number[], opts: { dtype?: DType } = {}): Tensor => full(shape, 1, opts);
export const zerosLike = (a: Tensor): Tensor => zeros(a.shape, { dtype: a.dtype });
export const onesLike = (a: Tensor): Tensor => ones(a.shape, { dtype: a.dtype });
export function arange(start: number, stop?: number, step = 1): Tensor {
  const [a, b] = stop === undefined ? [0, start] : [start, stop];
  const n = Math.max(0, Math.ceil((b - a) / step));
  const i = ir.iota(n), scaled = step === 1 ? i : ir.mul(i, ir.konst(step));
  return new Tensor(a === 0 ? scaled : ir.add(scaled, ir.konst(a)), int32);
}
export const linspace = (start: number, stop: number, num = 50): Tensor =>
  new Tensor(ir.data(Float32Array.from({ length: num }, (_, i) => (num === 1 ? start : start + ((stop - start) * i) / (num - 1))), [num]));
const iota = ir.iota;
// a scalar held as data – a one-element buffer seen as rank 0 – never folded into a
// kernel as a literal: a value that changes every frame (a clock, a slider, a computed
// number) reuses the compiled plan
const held = (d: Float32Array, shape: readonly number[]): UOp => (shape.length ? ir.data(d, shape) : ir.reshape(ir.data(d, [1]), []));
export const scalar = (v: number): Tensor => new Tensor(held(Float32Array.of(v), []));

// ── elementwise ─────────────────────────────────────────────────────────────────
export const nodeOf = (v: Like): UOp => (v instanceof Tensor ? v.node : typeof v === "number" || typeof v === "boolean" ? ir.konst(Number(v)) : tensor(v).node);
const wrap = (n: UOp, dtype: DType = float32) => new Tensor(n, dtype);
const un = (f: (x: UOp) => UOp) => (a: Like): Tensor => wrap(f(nodeOf(a)));
const bin = (f: (x: UOp, y: UOp) => UOp, dtype: DType = float32) => (a: Like, b: Like): Tensor => wrap(f(nodeOf(a), nodeOf(b)), dtype);
const neg = (x: UOp) => ir.mul(x, ir.konst(-1));
const not = (x: UOp) => ir.add(ir.konst(1), neg(x));
const either = (x: UOp, y: UOp) => ir.where(x, ir.konst(1), y);
const isnanN = (x: UOp) => ir.math("isnan", x);
const ne0 = (x: UOp) => either(ir.cmplt(x, ir.konst(0)), ir.cmplt(ir.konst(0), x));   // NaN is false
const eqN = (x: UOp, y: UOp) => ir.where(either(isnanN(x), isnanN(y)), ir.konst(0), ir.mul(not(ir.cmplt(x, y)), not(ir.cmplt(y, x))));
const m1 = (fn: string) => un((x) => ir.math(fn, x));

export const add = bin(ir.add);
export const multiply = bin(ir.mul);
export const subtract = bin((x, y) => ir.add(x, neg(y)));
export const divide = bin((x, y) => ir.mul(x, ir.recip(y)));
export const negative = un(neg);
export const reciprocal = un(ir.recip);
export const square = un((x) => ir.mul(x, x));
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
export const sin = m1("sin"), cos = m1("cos"), tanh = m1("tanh");
export const asin = m1("asin"), acos = m1("acos"), atan = m1("atan"), arcsin = asin, arccos = acos, arctan = atan;
export const asinh = m1("asinh"), acosh = m1("acosh"), atanh = m1("atanh"), arcsinh = asinh, arccosh = acosh, arctanh = atanh;
export const floor = m1("floor"), ceil = m1("ceil"), trunc = m1("trunc"), round = m1("round"), sign = m1("sign");
export const absolute = m1("abs"), abs = absolute, expm1 = m1("expm1"), log1p = m1("log1p"), cbrt = m1("cbrt");
export const isnan = (a: Like): Tensor => wrap(isnanN(nodeOf(a)), bool);
export const remainder = bin((x, y) => ir.add(x, neg(ir.mul(ir.math("floor", ir.mul(x, ir.recip(y))), y))));   // the divisor's sign, as numpy
export const arctan2 = bin((y, x) => ir.math("atan2", y, x)), atan2 = arctan2;
export const hypot = bin((x, y) => ir.sqrt(ir.add(ir.mul(x, x), ir.mul(y, y))));
export const sinc = un((x) => { const px = ir.mul(x, ir.konst(Math.PI)); return ir.where(eqN(x, ir.konst(0)), ir.konst(1), ir.mul(ir.math("sin", px), ir.recip(px))); });
export const maximum = bin((x, y) => ir.where(ir.cmplt(x, y), y, x));
export const minimum = bin((x, y) => ir.where(ir.cmplt(y, x), y, x));
export const clip = (a: Like, lo: Like, hi: Like): Tensor => minimum(maximum(a, lo), hi);
export const less = bin(ir.cmplt, bool);
export const greater = bin((x, y) => ir.cmplt(y, x), bool);
export const equal = bin(eqN, bool);
export const notEqual = bin((x, y) => not(eqN(x, y)), bool);
export const lessEqual = bin((x, y) => either(ir.cmplt(x, y), eqN(x, y)), bool);
export const greaterEqual = bin((x, y) => either(ir.cmplt(y, x), eqN(x, y)), bool);
export const logicalNot = (a: Like): Tensor => wrap(not(ne0(nodeOf(a))), bool);
export const logicalAnd = bin((x, y) => ir.mul(ne0(x), ne0(y)), bool);
export const logicalOr = bin((x, y) => either(ne0(x), ne0(y)), bool);
export const logicalXor = bin((x, y) => ir.math("abs", ir.add(ne0(x), neg(ne0(y)))), bool);
export const where = (c: Like, a: Like, b: Like): Tensor => wrap(ir.where(nodeOf(c), nodeOf(a), nodeOf(b)));
export function astype(a: Like, d: DType): Tensor {
  const x = nodeOf(a), from = a instanceof Tensor ? a.dtype : float32;
  if (d === bool) return wrap(from === bool ? x : ne0(x), bool);
  if ((d === int32 || d === uint32) && from === float32) return wrap(ir.math("trunc", x), d);
  return wrap(x, d);
}

// ── shape ───────────────────────────────────────────────────────────────────────
const normAxis = (axis: number, rank: number): number => {
  const a = axis < 0 ? axis + rank : axis;
  if (a < 0 || a >= rank) throw new Error(`axis ${axis} is out of bounds for an array of rank ${rank}`);
  return a;
};
export function reshape(a: Like, shape: readonly number[]): Tensor {
  const x = nodeOf(a), n = ir.numel(x.shape), known_ = shape.filter((d) => d !== -1).reduce((p, d) => p * d, 1);
  const s = shape.map((d) => (d === -1 ? n / known_ : d));
  if (ir.numel(s) !== n) throw new Error(`cannot reshape [${x.shape}] into [${shape}]`);
  return new Tensor(ir.sameShape(s, x.shape) ? x : ir.reshape(x, s), a instanceof Tensor ? a.dtype : float32);
}
const keepType = (a: Like, n: UOp) => new Tensor(n, a instanceof Tensor ? a.dtype : float32);
export const transpose = (a: Like, axes?: readonly number[]): Tensor => { const x = nodeOf(a); return keepType(a, ir.permute(x, axes ? axes.map((p) => normAxis(p, x.shape.length)) : x.shape.map((_, i) => x.shape.length - 1 - i))); };
export function moveaxis(a: Like, source: number | number[], destination: number | number[]): Tensor {
  const x = nodeOf(a), r = x.shape.length;
  const src = (typeof source === "number" ? [source] : source).map((s) => normAxis(s, r));
  const dst = (typeof destination === "number" ? [destination] : destination).map((d) => normAxis(d, r));
  const rest = x.shape.map((_, i) => i).filter((i) => !src.includes(i));
  const order: number[] = new Array(r);
  dst.forEach((d, i) => { order[d] = src[i]!; });
  let k = 0; for (let i = 0; i < r; i++) if (order[i] === undefined) order[i] = rest[k++]!;
  return keepType(a, ir.permute(x, order));
}
export const padAxis = (a: Like, axis: number, before: number, after: number): Tensor => { const x = nodeOf(a); return keepType(a, ir.pad(x, normAxis(axis, x.shape.length), before, after)); };
export const windows = (a: Like, sizes: readonly number[]): Tensor => keepType(a, ir.windows(nodeOf(a), sizes));
export const detach = (a: Like): Tensor => keepType(a, ir.detach(nodeOf(a)));
export const scatterAdd = (indices: Like, values: Like, size: number): Tensor => {
  const i = nodeOf(indices), v = nodeOf(values), n = ir.numel(i.shape);
  return wrap(ir.scatterAdd(ir.reshape(i, [n]), ir.reshape(v, [n, ...v.shape.slice(i.shape.length)]), size));
};
export const broadcastTo = (a: Like, shape: readonly number[]): Tensor => { const x = nodeOf(a); return keepType(a, ir.sameShape(x.shape, shape) ? x : ir.broadcastTo(x, shape)); };
export function concatenate(arrays: readonly Tensor[], axis = 0): Tensor {
  if (!arrays.length) throw new Error("concatenate: needs at least one array");
  const r = arrays[0]!.ndim, ax = normAxis(axis, r);
  const total = arrays.reduce((t, a) => t + a.node.shape[ax]!, 0);
  let off = 0, sum: UOp | null = null;
  for (const a of arrays) {
    const len = a.node.shape[ax]!, piece = ir.pad(a.node, ax, off, total - off - len);
    sum = sum ? ir.add(sum, piece) : piece;
    off += len;
  }
  return new Tensor(sum!, arrays[0]!.dtype);
}
export function stack(arrays: readonly Tensor[], axis = 0): Tensor {
  const r = arrays[0]!.ndim + 1, ax = normAxis(axis, r);
  return concatenate(arrays.map((a) => reshape(a, [...a.shape.slice(0, ax), 1, ...a.shape.slice(ax)])), ax);
}
export function tile(a: Like, reps: number | readonly number[]): Tensor {
  const x = nodeOf(a), rs = typeof reps === "number" ? [reps] : reps.slice();
  const n = Math.max(rs.length, x.shape.length);
  const sh = [...new Array(n - x.shape.length).fill(1), ...x.shape], rr = [...new Array(n - rs.length).fill(1), ...rs];
  const inter = sh.flatMap((d) => [1, d]), big = sh.flatMap((d, i) => [rr[i]!, d]);
  return keepType(a, ir.reshape(ir.broadcastTo(ir.reshape(x, inter), big), sh.map((d, i) => d * rr[i]!)));
}
export function repeat(a: Like, n: number, axis?: number): Tensor {
  let x = nodeOf(a);
  if (axis === undefined) { x = ir.reshape(x, [ir.numel(x.shape)]); axis = 0; }
  const ax = normAxis(axis, x.shape.length), s = x.shape;
  const up = [...s.slice(0, ax + 1), 1, ...s.slice(ax + 1)], big = [...s.slice(0, ax + 1), n, ...s.slice(ax + 1)];
  return keepType(a, ir.reshape(ir.broadcastTo(ir.reshape(x, up), big), s.map((d, i) => (i === ax ? d * n : d))));
}
export function roll(a: Like, shift: number | number[], axis?: number | number[]): Tensor {
  let x = nodeOf(a);
  const orig = x.shape;
  if (axis === undefined) { x = ir.reshape(x, [ir.numel(orig)]); axis = 0; }
  const shifts = typeof shift === "number" ? [shift] : shift, axes = typeof axis === "number" ? [axis] : axis;
  axes.forEach((ax0, i) => {
    const ax = normAxis(ax0, x.shape.length), len = x.shape[ax]!;
    const s = (((shifts[i] ?? shifts[0]!) % len) + len) % len;
    if (s === 0) return;
    x = ir.add(ir.pad(ir.slice(x, ax, len - s, s), ax, 0, len - s), ir.pad(ir.slice(x, ax, 0, len - s), ax, s, 0));
  });
  return keepType(a, ir.sameShape(x.shape, orig) ? x : ir.reshape(x, orig));
}
export function flip(a: Like, axis?: number | number[]): Tensor {
  let x = nodeOf(a);
  const axes = axis === undefined ? x.shape.map((_, i) => i) : (typeof axis === "number" ? [axis] : axis).map((ax) => normAxis(ax, x.shape.length));
  for (const ax of axes) x = take(new Tensor(x), new Tensor(ir.data(Float32Array.from({ length: x.shape[ax]! }, (_, i) => x.shape[ax]! - 1 - i), [x.shape[ax]!]), int32), ax).node;
  return keepType(a, x);
}
export function pad(a: Like, width: number | number[] | number[][]): Tensor {
  let x = nodeOf(a);
  x.shape.forEach((_, ax) => {
    const w = typeof width === "number" ? [width, width] : typeof width[0] === "number" ? [width[0] as number, (width[1] as number | undefined) ?? (width[0] as number)] : (width[ax] as number[]);
    if (w[0] || w[1]) x = ir.pad(x, ax, w[0]!, w[1]!);
  });
  return keepType(a, x);
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
  if (ax === 0) return keepType(a, g);
  const k = idx.shape.length, pre = x.shape.slice(0, ax).map((_, i) => k + i);
  const order = [...pre, ...idx.shape.map((_, i) => i), ...rest.slice(ax).map((_, i) => k + ax + i)];
  return keepType(a, ir.permute(g, order));
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
export const sum = (a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Tensor => wrap(reduceN(nodeOf(a), axis, opts, "ADD"));
export const prod = (a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Tensor => wrap(reduceN(nodeOf(a), axis, opts, "MUL"));
export const max = (a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Tensor => wrap(reduceN(nodeOf(a), axis, opts, "MAX"));
export const min = (a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Tensor => wrap(neg(reduceN(neg(nodeOf(a)), axis, opts, "MAX")));
export function mean(a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Tensor {
  const x = nodeOf(a), count = axesOf(x, axis).reduce((p, ax) => p * x.shape[ax]!, 1);
  return wrap(ir.mul(reduceN(x, axis, opts, "ADD"), ir.konst(1 / count)));
}
// the first index of the extreme value: candidates hold their index where they win, n elsewhere
function argExtreme(a: Like, axis: number | undefined, wins: (x: UOp, m: UOp) => UOp): Tensor {
  let x = nodeOf(a);
  if (axis === undefined) { x = ir.reshape(x, [ir.numel(x.shape)]); axis = 0; }
  const ax = normAxis(axis, x.shape.length), n = x.shape[ax]!;
  const m = ir.broadcastTo(ir.reduceAxis(wins === lose ? neg(x) : x, ax, true, "MAX"), x.shape);
  const idx = ir.broadcastTo(ir.reshape(iota(n), x.shape.map((d, i) => (i === ax ? n : 1))), x.shape);
  const cand = ir.where(wins(x, wins === lose ? neg(m) : m), idx, ir.broadcastTo(ir.konst(n), x.shape));
  const first = neg(ir.reduceAxis(neg(cand), ax, false, "MAX"));
  return wrap(first, int32);
}
const win = (x: UOp, m: UOp) => not(ir.cmplt(x, m));
const lose = (x: UOp, m: UOp) => not(ir.cmplt(m, x));
export const argmax = (a: Like, axis?: number): Tensor => argExtreme(a, axis, win);
export const argmin = (a: Like, axis?: number): Tensor => argExtreme(a, axis, lose);

// ── order: a routine finds the permutation; values move through a differentiable gather
const rowsOf = (t: HostData): [number, number] => { const n = t.shape.at(-1) ?? 1; return [Math.max(t.data.length / Math.max(n, 1), 1), n]; };
const argsortRows = (desc: boolean) => ([t]: HostData[]): Float32Array => {
  const [R, n] = rowsOf(t!), out = new Float32Array(R * n);
  for (let r = 0; r < R; r++) {
    const row = t!.data.subarray(r * n, r * n + n);
    const order = [...row.keys()].sort((i, j) => { const d = desc ? row[j]! - row[i]! : row[i]! - row[j]!; return d || i - j; });
    order.forEach((v, i) => { out[r * n + i] = v; });
  }
  return out;
};
const scalarRank = (x: UOp) => (x.shape.length ? x : ir.reshape(x, [1]));
export const argsort = (a: Like): Tensor => { const x = scalarRank(nodeOf(a)); return wrap(ir.host("argsort", [x], x.shape, argsortRows(false)), int32); };
// gather each row by its own indices: flatten rows, offset each index by its row
function gatherRows(x: UOp, idx: UOp): UOp {
  const n = x.shape.at(-1)!, k = idx.shape.at(-1)!, R = ir.numel(x.shape) / n;
  const offsets = ir.data(Float32Array.from({ length: R * k }, (_, i) => Math.floor(i / k) * n), [R * k]);
  const flat = ir.add(ir.reshape(idx, [R * k]), offsets);
  return ir.reshape(ir.gather(ir.reshape(x, [R * n, 1]), flat), [...x.shape.slice(0, -1), k]);
}
export const sort = (a: Like): Tensor => { const x = scalarRank(nodeOf(a)); return wrap(gatherRows(x, ir.host("argsort", [x], x.shape, argsortRows(false)))); };
export function topK(a: Like, k: number): [Tensor, Tensor] {
  const x = scalarRank(nodeOf(a)), n = x.shape.at(-1)!;
  const all = ir.host("argsort-desc", [x], x.shape, argsortRows(true));
  const idx = k === n ? all : ir.slice(all, x.shape.length - 1, 0, k);
  return [wrap(gatherRows(x, idx)), wrap(idx, int32)];
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
export function tensordot(a: Like, b: Like, axes: [number[], number[]] | number = 2): Tensor {
  const x = nodeOf(a), y = nodeOf(b);
  const [ax, bx] = typeof axes === "number"
    ? [x.shape.map((_, i) => i).slice(x.shape.length - axes), y.shape.map((_, i) => i).slice(0, axes)]
    : [axes[0].map((i) => normAxis(i, x.shape.length)), axes[1].map((i) => normAxis(i, y.shape.length))];
  const freeA = x.shape.map((_, i) => i).filter((i) => !ax.includes(i)), freeB = y.shape.map((_, i) => i).filter((i) => !bx.includes(i));
  const K = ax.reduce((p, i) => p * x.shape[i]!, 1);
  const A = ir.reshape(ir.permute(x, [...freeA, ...ax]), [Math.max(freeA.reduce((p, i) => p * x.shape[i]!, 1), 1), K]);
  const B = ir.reshape(ir.permute(y, [...bx, ...freeB]), [K, Math.max(freeB.reduce((p, i) => p * y.shape[i]!, 1), 1)]);
  return wrap(ir.reshape(cellMatmul(A, B), [...freeA.map((i) => x.shape[i]!), ...freeB.map((i) => y.shape[i]!)]));
}
export function dot(a: Like, b: Like): Tensor {
  const x = nodeOf(a), y = nodeOf(b);
  if (!x.shape.length || !y.shape.length) return wrap(ir.mul(x, y));
  if (y.shape.length === 1) return wrap(reduceN(ir.mul(x, ir.broadcastTo(y, x.shape)), -1, {}, "ADD"));
  return tensordot(new Tensor(x), new Tensor(y), [[-1], [y.shape.length - 2]]);
}
// Einstein summation: label every axis, line the operands up label by label (a label
// written twice in one operand reads its diagonal), multiply, sum the labels the output lacks
export function einsum(spec: string, ...operands: Tensor[]): Tensor {
  const [lhs, rhsGiven] = spec.replace(/\s/g, "").split("->") as [string, string | undefined];
  const groups = lhs.split(",");
  if (groups.length !== operands.length) throw new Error(`einsum: ${groups.length} operand specs for ${operands.length} operands`);
  const extent = Math.max(0, ...operands.map((o, i) => o.ndim - groups[i]!.replace("...", "").length));
  const dots = Array.from({ length: extent }, (_, i) => String.fromCodePoint(0x2460 + i));   // "..." axes get private labels, right-aligned
  const expand = (g: string, rank: number) => (g.includes("...") ? g.replace("...", dots.slice(extent - (rank - (g.length - 3))).join("")) : g);
  const labels = groups.map((g, i) => [...expand(g, operands[i]!.ndim)]);
  const counts = new Map<string, number>(); for (const l of labels.flat()) counts.set(l, (counts.get(l) ?? 0) + 1);
  const out = rhsGiven === undefined ? [...dots, ...[...counts.keys()].filter((l) => counts.get(l) === 1 && !dots.includes(l)).sort()] : [...expand(rhsGiven, extent + rhsGiven.replace("...", "").length)];
  const all = [...new Set(labels.flat())];
  const size = new Map<string, number>();
  labels.forEach((ls, i) => ls.forEach((l, k) => { const d = operands[i]!.shape[k]!; if (d !== 1 || !size.has(l)) size.set(l, Math.max(size.get(l) ?? 1, d)); }));
  const aligned = operands.map((o, i) => {
    let x = o.node, ls = labels[i]!;
    for (let k = ls.length - 1; k >= 0; k--) {                                // a repeated label: keep the diagonal, sum the copy away
      const j = ls.indexOf(ls[k]!);
      if (j === k) continue;
      const n = x.shape[k]!, eye = ir.data(Float32Array.from({ length: n * n }, (_, q) => (q % (n + 1) === 0 ? 1 : 0)), [n, n]);
      x = ir.reduceAxis(ir.mul(x, ir.broadcastTo(ir.reshape(eye, x.shape.map((d, q) => (q === j || q === k ? n : 1))), x.shape)), k, false);
      ls = ls.filter((_, q) => q !== k);
    }
    const order = all.filter((l) => ls.includes(l)).map((l) => ls.indexOf(l));
    if (order.some((p, q) => p !== q)) x = ir.permute(x, order);
    return ir.reshape(x, all.map((l) => (ls.includes(l) ? x.shape[all.filter((m) => ls.includes(m)).indexOf(l)]! : 1)));
  });
  let z = aligned.reduce((p, q) => ir.mul(p, q));
  z = ir.broadcastTo(z, all.map((l) => size.get(l)!));
  const gone = all.map((l, i) => (out.includes(l) ? -1 : i)).filter((i) => i >= 0);
  for (let i = gone.length - 1; i >= 0; i--) z = ir.reduceAxis(z, gone[i]!, false);
  const kept = all.filter((l) => out.includes(l));
  const perm = out.map((l) => kept.indexOf(l));
  return wrap(perm.some((p, q) => p !== q) ? ir.permute(z, perm) : z);
}

// ── spectra: the real DFT as two matrix products ───────────────────────────────
export const fft = {
  rfft(a: Like): { real: Tensor; imag: Tensor } {
    const x = nodeOf(a), n = x.shape.at(-1)!, m = Math.floor(n / 2) + 1;
    const C = new Float32Array(n * m), S = new Float32Array(n * m);
    for (let t = 0; t < n; t++) for (let k = 0; k < m; k++) { const w = (2 * Math.PI * ((k * t) % n)) / n; C[t * m + k] = Math.cos(w); S[t * m + k] = -Math.sin(w); }
    const rows = ir.reshape(x, [Math.max(ir.numel(x.shape) / n, 1), n]), back = [...x.shape.slice(0, -1), m];
    return { real: wrap(ir.reshape(cellMatmul(rows, ir.data(C, [n, m])), back)), imag: wrap(ir.reshape(cellMatmul(rows, ir.data(S, [n, m])), back)) };
  },
};

// ── composites the natives still call ───────────────────────────────────────────
const lastAxis = (x: UOp, axis?: number) => (axis === undefined ? x.shape.length - 1 : axis < 0 ? axis + x.shape.length : axis);
export function softmax(a: Like, axis?: number): Tensor {
  const x = nodeOf(a), ax = lastAxis(x, axis);
  const e = ir.exp(ir.add(x, ir.mul(ir.broadcastTo(ir.reduceAxis(x, ax, true, "MAX"), x.shape), ir.konst(-1))));
  return wrap(ir.mul(e, ir.recip(ir.broadcastTo(ir.reduceAxis(e, ax, true), x.shape))));
}
export function logSoftmax(a: Like, axis?: number): Tensor {
  const x = nodeOf(a), ax = lastAxis(x, axis);
  const shifted = ir.add(x, ir.mul(ir.broadcastTo(ir.reduceAxis(x, ax, true, "MAX"), x.shape), ir.konst(-1)));
  return wrap(ir.add(shifted, ir.mul(ir.broadcastTo(ir.log(ir.reduceAxis(ir.exp(shifted), ax, true)), x.shape), ir.konst(-1))));
}
export function oneHot(a: Like, n: number): Tensor {
  const idx = nodeOf(a), shape = [...idx.shape, n];
  const hit = (x: UOp, y: UOp) => ir.mul(ir.add(ir.konst(1), ir.mul(ir.cmplt(x, y), ir.konst(-1))), ir.add(ir.konst(1), ir.mul(ir.cmplt(y, x), ir.konst(-1))));
  return wrap(hit(ir.broadcastTo(ir.reshape(idx, [...idx.shape, 1]), shape), ir.broadcastTo(ir.iota(n), shape)));
}
// erf by Abramowitz–Stegun 7.1.26 (|error| < 1.5e-7)
export const erf = (a: Like): Tensor => {
  const x = nodeOf(a), ax = ir.math("abs", x), t = ir.recip(ir.add(ir.konst(1), ir.mul(ir.konst(0.3275911), ax)));
  const poly = [1.061405429, -1.453152027, 1.421413741, -0.284496736, 0.254829592].reduce<UOp>((p, c, i) => (i === 0 ? ir.konst(c) : ir.add(ir.mul(p, t), ir.konst(c))), ir.konst(0));
  const y = ir.add(ir.konst(1), ir.mul(ir.mul(ir.mul(poly, t), ir.exp(ir.mul(ir.mul(ax, ax), ir.konst(-1)))), ir.konst(-1)));
  return wrap(ir.mul(ir.math("sign", x), y));
};
// N-d cross-correlation of a field with a kernel of the same rank, SAME zero padding:
// every position's window of the padded field, times the kernel, summed — one reduction
// over the taps, the windows a view that never materializes
export function conv(field: Like, kernel: Like): Tensor {
  const x = nodeOf(field), k = nodeOf(kernel), sp = x.shape, ks = k.shape;
  let f = x;
  sp.forEach((_, d) => { const K = ks[d]!, lo = Math.floor((K - 1) / 2); if (K > 1) f = ir.pad(f, d, lo, K - 1 - lo); });
  const w = ir.windows(f, ks);
  let out = ir.mul(w, ir.broadcastTo(ir.reshape(k, [...sp.map(() => 1), ...ks]), w.shape));
  for (let d = ks.length - 1; d >= 0; d--) out = ir.reduceAxis(out, sp.length + d, false);
  return wrap(out);
}
// a draw is a RANDOM leaf, so it fuses into what reads it; a seed is a number
const mix = (seed: number, salt: number) => (Math.imul((seed ^ Math.imul(salt, 0x27d4eb2f)) >>> 0, 0x165667b1) ^ salt) >>> 0;
export const uniform = (seed: number, shape: readonly number[]): Tensor => wrap(ir.random(shape, mix(seed, 1)));
export function normal(seed: number, shape: readonly number[]): Tensor {        // Box–Muller
  const u1 = ir.random(shape, mix(seed, 2)), u2 = ir.random(shape, mix(seed, 3));
  const r = ir.sqrt(ir.mul(ir.log(ir.add(ir.konst(1), ir.mul(u1, ir.konst(-1)))), ir.konst(-2)));
  return wrap(ir.mul(r, ir.math("cos", ir.mul(u2, ir.konst(2 * Math.PI)))));
}
// nested lists and records of tensors, mapped leaf by leaf
export const mapTree = (fn: (...xs: any[]) => any, t: unknown, ...rest: unknown[]): unknown => {
  if (t instanceof Tensor) return fn(t, ...rest);
  if (Array.isArray(t)) return t.map((x, i) => mapTree(fn, x, ...rest.map((r) => (r as unknown[])[i])));
  if (t && typeof t === "object") return Object.fromEntries(Object.entries(t).map(([k, v]) => [k, mapTree(fn, v, ...rest.map((r) => (r as Record<string, unknown>)[k]))]));
  return t;
};

// ── ∇: the engine's transpose ∘ linearize on the graph a function builds ─────────
// The derivative is with respect to a traced argument – this use of the value, never a
// copy of it the function closed over – and an outer ∇ still differentiates through it.
export function vjp<F extends (...xs: any[]) => Tensor>(f: F, primals: Tensor[]): [Tensor, (ct: unknown) => Tensor[]] {
  const leaves = primals.map(traced);
  const out = f(...leaves);
  const seed = (ct: unknown): UOp => (ct instanceof Tensor ? nodeOf(ct) : ir.broadcastTo(ir.konst(Number(ct ?? 1)), out.node.shape));
  return [out, (ct: unknown) => graphVjp(out.node, leaves.map((l) => l.node), seed(ct)).map((n) => new Tensor(n))];
}
// valueAndGrad(f)(params, …rest): params is a tree of tensors; the gradients come back in its shape
export function valueAndGrad(f: (...xs: any[]) => Tensor) {
  return (params: unknown, ...rest: unknown[]): [Tensor, unknown] => {
    const leaves: Tensor[] = [];
    const args = mapTree((p: Tensor) => { const l = traced(p); leaves.push(l); return l; }, params);
    const loss = f(args, ...rest);
    const grads = graphVjp(loss.node, leaves.map((l) => l.node), ir.konst(1));
    let i = 0;
    return [loss, mapTree(() => new Tensor(grads[i++]!), params)];
  };
}

// ── devices: WebGPU where the platform has it, the CPU everywhere ────────────────
let gpu: GPU | null = null;
export async function init(): Promise<void> { gpu ??= await GPU.create(); useDevice(gpu); }
export const getWebGPUDevice = (): GPUDevice => { if (!gpu) throw new Error("no WebGPU device"); return gpu.device; };
export const engineGpu = (): GPU | null => gpu;
// resolves once at most `depth` computations are still running on the GPU
export const gpuDrained = (depth: number): Promise<void> => gpu?.drained(depth) ?? Promise.resolve();
