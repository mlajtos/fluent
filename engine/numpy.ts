// numpy's API as Fluent's natives call it, over the engine. An Array is an immutable graph
// node, computed when something reads it; every value is f32, and `bool`/`int32` are tags
// on 0/1 and whole-number values. The engine owns the buffers – nothing is freed by hand.

import type { UOp } from "./ir.ts";
import * as ir from "./ir.ts";
import { matmul as cellMatmul } from "./nn.ts";
import { CPU, type Tensor } from "./cpu.ts";

export type DType = "float32" | "int32" | "uint32" | "bool";
export const float32 = "float32" as const, int32 = "int32" as const, uint32 = "uint32" as const, bool = "bool" as const;
type Axis = number | number[] | null | undefined;
export type Like = Array | number | boolean | readonly unknown[];

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
    cpu.realizeSync(roots).forEach((t, i) => { const n = need[i]!; hostData.set(n, t.data); leafOf.set(n, held(t.data, n.shape)); });
    return;
  }
  device.computeSync(roots, (name) => onDevice.get(name)).forEach((b, i) => {
    const n = need[i]!, name = `%v${nextLeaf++}`, buf = ir.buffer(name, n.shape.length ? n.shape : [1]), leaf = rank0(buf, n.shape);
    onDevice.set(name, b); freeOnCollect.register(buf, name);
    leafOf.set(n, leaf); leafOf.set(leaf, leaf);
  });
}
// compute without reading: what a step commits stays where it was computed
export const materialize = (arrays: readonly Array[]): void => compute(arrays.map((a) => a.node));
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
export function hostValue(a: Array): Float32Array | undefined {
  const known = dataOf(a.node);
  if (known || leafOf.has(a.node)) return known;
  if (device && hostWork(a.node) > SMALL) return undefined;
  compute([a.node]);
  return dataOf(a.node);
}
async function readAsync(a: Array): Promise<Float32Array> {
  compute([a.node]);
  const known = dataOf(a.node);
  if (known) return known;
  const n = a.node, leaf = leafOf.get(n)!, data = await device!.readAsync(onDevice.get(ir.bufName(stored(leaf)))!, ir.numel(n.shape));
  hostData.set(n, data); hostData.set(leaf, data);
  return data;
}
export function realize(arrays: readonly Array[]): Float32Array[] {
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
export function settle(a: Array): Array {
  compute([a.node]);
  const l = leafOf.get(a.node) ?? a.node;
  return new Array(l.op === "CONSTANT" && !l.shape.length ? held(l.arg as Float32Array, []) : l, a.dtype);
}
// how many nodes stand behind a value – 1 for a leaf: what crosses time must not grow
export function graphSize(a: Array): number {
  const seen = new Set<number>(), stack = [leafOf.get(a.node) ?? a.node];
  while (stack.length) { const u = stack.pop()!; if (seen.has(u.id)) continue; seen.add(u.id); if (!isLeaf(u)) stack.push(...u.src.map((x) => leafOf.get(x) ?? x)); }
  return seen.size;
}
// the device buffer holding a value, for drawing it without a read
export function gpuBufferOf(a: Array): GPUBuffer {
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
    Array_.from({ length: shape[axis]! }, (_, i) => (axis === shape.length - 1 ? conv(d[off + i]!) : go(axis + 1, off + i * st[axis]!)));
  return go(0, 0);
};
const Array_ = globalThis.Array;

// ── the value ───────────────────────────────────────────────────────────────────
export class Array {
  readonly node: UOp;
  readonly dtype: DType;
  constructor(node: UOp, dtype: DType = float32) { this.node = node; this.dtype = dtype; }
  get shape(): number[] { return this.node.shape.slice(); }
  get ndim(): number { return this.node.shape.length; }
  get size(): number { return ir.numel(this.node.shape); }
  dataSync(): Float32Array { return realize([this])[0]!; }
  data(): Promise<Float32Array> { return readAsync(this); }
  async gpuBuffer(): Promise<GPUBuffer> { return gpuBufferOf(this); }
  js(): any {
    const conv = this.dtype === bool ? (v: number) => v !== 0 : (v: number) => v;
    return nest(this.dataSync(), this.node.shape, conv);
  }
  item(): number { return this.dataSync()[0]!; }
  astype(d: DType): Array { return astype(this, d); }
  reshape(s: readonly number[]): Array { return reshape(this, s); }
  add(x: Like): Array { return add(this, x); }
  sub(x: Like): Array { return subtract(this, x); }
  mul(x: Like): Array { return multiply(this, x); }
  // x.slice(i, [a], [a, b], …): an integer drops its axis, [a] runs to the end, [a, b] is a range
  slice(...spec: (number | [number] | [number, number])[]): Array { return Array.#slice(this.node, this.dtype, spec); }
  static #slice(n: UOp, dtype: DType, spec: (number | [number] | [number, number])[]): Array {
    const drop: number[] = [];
    spec.forEach((s, axis) => {
      const len = n.shape[axis]!;
      const norm = (v: number) => (v < 0 ? v + len : v);
      const [a, b] = typeof s === "number" ? [norm(s), norm(s) + 1] : [norm(s[0]), s.length > 1 ? norm(s[1]!) : len];
      if (typeof s === "number") drop.push(axis);
      n = ir.slice(n, axis, Math.max(0, a), Math.max(0, Math.min(len, b) - Math.max(0, a)));
    });
    return new Array(drop.length ? ir.reshape(n, n.shape.filter((_, i) => !drop.includes(i))) : n, dtype);
  }
  *[Symbol.iterator](): IterableIterator<Array> {
    const n = this.node.shape[0] ?? 0;
    for (let i = 0; i < n; i++) yield Array.#slice(this.node, this.dtype, [i]);
  }
  toString(): string { return `Array(${this.dtype}, [${this.node.shape}])`; }
}

// ── construction ────────────────────────────────────────────────────────────────
const flatten = (v: unknown, out: number[], shape: number[], depth: number): void => {
  if (v instanceof Array) { const d = realize([v])[0]!; if (depth === shape.length) shape.push(...v.node.shape); for (let i = 0; i < d.length; i++) out.push(d[i]!); return; }
  if (typeof v === "number" || typeof v === "boolean") { out.push(Number(v)); return; }
  if (ArrayBuffer.isView(v)) { const xs = v as unknown as ArrayLike<number>; if (depth === shape.length) shape.push(xs.length); for (let i = 0; i < xs.length; i++) out.push(Number(xs[i])); return; }
  const xs = v as readonly unknown[];
  if (depth === shape.length) shape.push(xs.length);
  for (const x of xs) flatten(x, out, shape, depth + 1);
};
const isBoolish = (v: unknown): boolean => typeof v === "boolean" || (Array_.isArray(v) && v.length > 0 && isBoolish(v[0]));
export function array(values: unknown, opts: { shape?: readonly number[]; dtype?: DType } = {}): Array {
  if (values instanceof Array) return opts.dtype ? astype(values, opts.dtype) : values;
  let data: Float32Array, shape: number[] = [];
  if (ArrayBuffer.isView(values)) {                                            // a typed array is data already – no trip through a JS array
    data = values instanceof Float32Array ? values.slice() : Float32Array.from(values as unknown as ArrayLike<number>);
    shape = [data.length];
  } else { const flat: number[] = []; flatten(values, flat, shape, 0); data = Float32Array.from(flat); }
  const s = opts.shape ?? shape;
  if (ir.numel(s) !== data.length) throw new Error(`array: ${data.length} values do not fill shape [${s}]`);
  return new Array(ir.data(data, s), opts.dtype ?? (isBoolish(values) ? bool : values instanceof Int32Array ? int32 : float32));
}
export const full = (shape: readonly number[], value: number, opts: { dtype?: DType } = {}): Array =>
  new Array(ir.data(new Float32Array(ir.numel(shape)).fill(value), shape), opts.dtype ?? float32);
export const zeros = (shape: readonly number[], opts: { dtype?: DType } = {}): Array => full(shape, 0, opts);
export const ones = (shape: readonly number[], opts: { dtype?: DType } = {}): Array => full(shape, 1, opts);
export const zerosLike = (a: Array): Array => zeros(a.shape, { dtype: a.dtype });
export const onesLike = (a: Array): Array => ones(a.shape, { dtype: a.dtype });
export function arange(start: number, stop?: number, step = 1): Array {
  const [a, b] = stop === undefined ? [0, start] : [start, stop];
  const n = Math.max(0, Math.ceil((b - a) / step));
  return new Array(ir.data(Float32Array.from({ length: n }, (_, i) => a + i * step), [n]), int32);
}
export const linspace = (start: number, stop: number, num = 50): Array =>
  new Array(ir.data(Float32Array.from({ length: num }, (_, i) => (num === 1 ? start : start + ((stop - start) * i) / (num - 1))), [num]));
const iota = (n: number): UOp => ir.data(Float32Array.from({ length: n }, (_, i) => i), [n]);
// a scalar held as data – a one-element buffer seen as rank 0 – never folded into a
// kernel as a literal: a value that changes every frame (a clock, a slider, a computed
// number) reuses the compiled plan
const held = (d: Float32Array, shape: readonly number[]): UOp => (shape.length ? ir.data(d, shape) : ir.reshape(ir.data(d, [1]), []));
export const scalar = (v: number): Array => new Array(held(Float32Array.of(v), []));

// ── elementwise ─────────────────────────────────────────────────────────────────
export const nodeOf = (v: Like): UOp => (v instanceof Array ? v.node : typeof v === "number" || typeof v === "boolean" ? ir.konst(Number(v)) : array(v).node);
const wrap = (n: UOp, dtype: DType = float32) => new Array(n, dtype);
const un = (f: (x: UOp) => UOp) => (a: Like): Array => wrap(f(nodeOf(a)));
const bin = (f: (x: UOp, y: UOp) => UOp, dtype: DType = float32) => (a: Like, b: Like): Array => wrap(f(nodeOf(a), nodeOf(b)), dtype);
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
export const trueDivide = bin((x, y) => ir.mul(x, ir.recip(y)));
export const divide = trueDivide;
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
export const power = (a: Like, b: Like): Array => {
  const x = nodeOf(a), y = nodeOf(b);
  const e = y.op === "CONSTANT" && !y.shape.length ? (y.arg as Float32Array)[0]! : NaN;
  if (Number.isInteger(e) && Math.abs(e) <= 64) { const r = intPow(x, e); return wrap(ir.sameShape(r.shape, x.shape) ? r : ir.broadcastTo(r, x.shape)); }
  return wrap(ir.math("pow", x, y));
};
export const pow = power;
export const sin = m1("sin"), cos = m1("cos"), tan = m1("tan"), tanh = m1("tanh");
export const asin = m1("asin"), acos = m1("acos"), atan = m1("atan"), arcsin = asin, arccos = acos, arctan = atan;
export const asinh = m1("asinh"), acosh = m1("acosh"), atanh = m1("atanh"), arcsinh = asinh, arccosh = acosh, arctanh = atanh;
export const floor = m1("floor"), ceil = m1("ceil"), trunc = m1("trunc"), round = m1("round"), sign = m1("sign");
export const absolute = m1("abs"), abs = absolute, expm1 = m1("expm1"), log1p = m1("log1p"), cbrt = m1("cbrt");
export const isnan = (a: Like): Array => wrap(isnanN(nodeOf(a)), bool);
export const remainder = bin((x, y) => ir.add(x, neg(ir.mul(ir.math("floor", ir.mul(x, ir.recip(y))), y))));   // the divisor's sign, as numpy
export const arctan2 = bin((y, x) => ir.math("atan2", y, x)), atan2 = arctan2;
export const hypot = bin((x, y) => ir.sqrt(ir.add(ir.mul(x, x), ir.mul(y, y))));
export const sinc = un((x) => { const px = ir.mul(x, ir.konst(Math.PI)); return ir.where(eqN(x, ir.konst(0)), ir.konst(1), ir.mul(ir.math("sin", px), ir.recip(px))); });
export const maximum = bin((x, y) => ir.where(ir.cmplt(x, y), y, x));
export const minimum = bin((x, y) => ir.where(ir.cmplt(y, x), y, x));
export const clip = (a: Like, lo: Like, hi: Like): Array => minimum(maximum(a, lo), hi);
export const less = bin(ir.cmplt, bool);
export const greater = bin((x, y) => ir.cmplt(y, x), bool);
export const equal = bin(eqN, bool);
export const notEqual = bin((x, y) => not(eqN(x, y)), bool);
export const lessEqual = bin((x, y) => either(ir.cmplt(x, y), eqN(x, y)), bool);
export const greaterEqual = bin((x, y) => either(ir.cmplt(y, x), eqN(x, y)), bool);
export const logicalNot = (a: Like): Array => wrap(not(ne0(nodeOf(a))), bool);
export const logicalAnd = bin((x, y) => ir.mul(ne0(x), ne0(y)), bool);
export const logicalOr = bin((x, y) => either(ne0(x), ne0(y)), bool);
export const logicalXor = bin((x, y) => ir.math("abs", ir.add(ne0(x), neg(ne0(y)))), bool);
export const where = (c: Like, a: Like, b: Like): Array => wrap(ir.where(nodeOf(c), nodeOf(a), nodeOf(b)));
export function astype(a: Like, d: DType): Array {
  const x = nodeOf(a), from = a instanceof Array ? a.dtype : float32;
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
export function reshape(a: Like, shape: readonly number[]): Array {
  const x = nodeOf(a), n = ir.numel(x.shape), known_ = shape.filter((d) => d !== -1).reduce((p, d) => p * d, 1);
  const s = shape.map((d) => (d === -1 ? n / known_ : d));
  if (ir.numel(s) !== n) throw new Error(`cannot reshape [${x.shape}] into [${shape}]`);
  return new Array(ir.sameShape(s, x.shape) ? x : ir.reshape(x, s), a instanceof Array ? a.dtype : float32);
}
const keepType = (a: Like, n: UOp) => new Array(n, a instanceof Array ? a.dtype : float32);
export const transpose = (a: Like, axes?: readonly number[]): Array => { const x = nodeOf(a); return keepType(a, ir.permute(x, axes ? axes.map((p) => normAxis(p, x.shape.length)) : x.shape.map((_, i) => x.shape.length - 1 - i))); };
export function moveaxis(a: Like, source: number | number[], destination: number | number[]): Array {
  const x = nodeOf(a), r = x.shape.length;
  const src = (typeof source === "number" ? [source] : source).map((s) => normAxis(s, r));
  const dst = (typeof destination === "number" ? [destination] : destination).map((d) => normAxis(d, r));
  const rest = x.shape.map((_, i) => i).filter((i) => !src.includes(i));
  const order: number[] = new Array_(r);
  dst.forEach((d, i) => { order[d] = src[i]!; });
  let k = 0; for (let i = 0; i < r; i++) if (order[i] === undefined) order[i] = rest[k++]!;
  return keepType(a, ir.permute(x, order));
}
export const broadcastTo = (a: Like, shape: readonly number[]): Array => { const x = nodeOf(a); return keepType(a, ir.sameShape(x.shape, shape) ? x : ir.broadcastTo(x, shape)); };
export function concatenate(arrays: readonly Array[], axis = 0): Array {
  if (!arrays.length) throw new Error("concatenate: needs at least one array");
  const r = arrays[0]!.ndim, ax = normAxis(axis, r);
  const total = arrays.reduce((t, a) => t + a.node.shape[ax]!, 0);
  let off = 0, sum: UOp | null = null;
  for (const a of arrays) {
    const len = a.node.shape[ax]!, piece = ir.pad(a.node, ax, off, total - off - len);
    sum = sum ? ir.add(sum, piece) : piece;
    off += len;
  }
  return new Array(sum!, arrays[0]!.dtype);
}
export function stack(arrays: readonly Array[], axis = 0): Array {
  const r = arrays[0]!.ndim + 1, ax = normAxis(axis, r);
  return concatenate(arrays.map((a) => reshape(a, [...a.shape.slice(0, ax), 1, ...a.shape.slice(ax)])), ax);
}
export function tile(a: Like, reps: number | readonly number[]): Array {
  const x = nodeOf(a), rs = typeof reps === "number" ? [reps] : reps.slice();
  const n = Math.max(rs.length, x.shape.length);
  const sh = [...new Array_(n - x.shape.length).fill(1), ...x.shape], rr = [...new Array_(n - rs.length).fill(1), ...rs];
  const inter = sh.flatMap((d) => [1, d]), big = sh.flatMap((d, i) => [rr[i]!, d]);
  return keepType(a, ir.reshape(ir.broadcastTo(ir.reshape(x, inter), big), sh.map((d, i) => d * rr[i]!)));
}
export function repeat(a: Like, n: number, axis?: number): Array {
  let x = nodeOf(a);
  if (axis === undefined) { x = ir.reshape(x, [ir.numel(x.shape)]); axis = 0; }
  const ax = normAxis(axis, x.shape.length), s = x.shape;
  const up = [...s.slice(0, ax + 1), 1, ...s.slice(ax + 1)], big = [...s.slice(0, ax + 1), n, ...s.slice(ax + 1)];
  return keepType(a, ir.reshape(ir.broadcastTo(ir.reshape(x, up), big), s.map((d, i) => (i === ax ? d * n : d))));
}
export function roll(a: Like, shift: number | number[], axis?: number | number[]): Array {
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
export function flip(a: Like, axis?: number | number[]): Array {
  let x = nodeOf(a);
  const axes = axis === undefined ? x.shape.map((_, i) => i) : (typeof axis === "number" ? [axis] : axis).map((ax) => normAxis(ax, x.shape.length));
  for (const ax of axes) x = take(new Array(x), new Array(ir.data(Float32Array.from({ length: x.shape[ax]! }, (_, i) => x.shape[ax]! - 1 - i), [x.shape[ax]!]), int32), ax).node;
  return keepType(a, x);
}
export function pad(a: Like, width: number | number[] | number[][]): Array {
  let x = nodeOf(a);
  x.shape.forEach((_, ax) => {
    const w = typeof width === "number" ? [width, width] : typeof width[0] === "number" ? [width[0] as number, (width[1] as number | undefined) ?? (width[0] as number)] : (width[ax] as number[]);
    if (w[0] || w[1]) x = ir.pad(x, ax, w[0]!, w[1]!);
  });
  return keepType(a, x);
}
// numpy take: out = x[..., idx, ...] along `axis`, idx of any shape
export function take(a: Like, indices: Like, axis?: number): Array {
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
export function meshgrid(arrays: readonly Array[]): Array[] {
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
export const sum = (a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Array => wrap(reduceN(nodeOf(a), axis, opts, "ADD"));
export const prod = (a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Array => wrap(reduceN(nodeOf(a), axis, opts, "MUL"));
export const max = (a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Array => wrap(reduceN(nodeOf(a), axis, opts, "MAX"));
export const min = (a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Array => wrap(neg(reduceN(neg(nodeOf(a)), axis, opts, "MAX")));
export function mean(a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Array {
  const x = nodeOf(a), count = axesOf(x, axis).reduce((p, ax) => p * x.shape[ax]!, 1);
  return wrap(ir.mul(reduceN(x, axis, opts, "ADD"), ir.konst(1 / count)));
}
// the first index of the extreme value: candidates hold their index where they win, n elsewhere
function argExtreme(a: Like, axis: number | undefined, wins: (x: UOp, m: UOp) => UOp): Array {
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
export const argmax = (a: Like, axis?: number): Array => argExtreme(a, axis, win);
export const argmin = (a: Like, axis?: number): Array => argExtreme(a, axis, lose);

// ── order: a routine finds the permutation; values move through a differentiable gather
const rowsOf = (t: Tensor): [number, number] => { const n = t.shape.at(-1) ?? 1; return [Math.max(t.data.length / Math.max(n, 1), 1), n]; };
const argsortRows = (desc: boolean) => ([t]: Tensor[]): Float32Array => {
  const [R, n] = rowsOf(t!), out = new Float32Array(R * n);
  for (let r = 0; r < R; r++) {
    const row = t!.data.subarray(r * n, r * n + n);
    const order = [...row.keys()].sort((i, j) => { const d = desc ? row[j]! - row[i]! : row[i]! - row[j]!; return d || i - j; });
    order.forEach((v, i) => { out[r * n + i] = v; });
  }
  return out;
};
const scalarRank = (x: UOp) => (x.shape.length ? x : ir.reshape(x, [1]));
export const argsort = (a: Like): Array => { const x = scalarRank(nodeOf(a)); return wrap(ir.host("argsort", [x], x.shape, argsortRows(false)), int32); };
// gather each row by its own indices: flatten rows, offset each index by its row
function gatherRows(x: UOp, idx: UOp): UOp {
  const n = x.shape.at(-1)!, k = idx.shape.at(-1)!, R = ir.numel(x.shape) / n;
  const offsets = ir.data(Float32Array.from({ length: R * k }, (_, i) => Math.floor(i / k) * n), [R * k]);
  const flat = ir.add(ir.reshape(idx, [R * k]), offsets);
  return ir.reshape(ir.gather(ir.reshape(x, [R * n, 1]), flat), [...x.shape.slice(0, -1), k]);
}
export const sort = (a: Like): Array => { const x = scalarRank(nodeOf(a)); return wrap(gatherRows(x, ir.host("argsort", [x], x.shape, argsortRows(false)))); };
export function topK(a: Like, k: number): [Array, Array] {
  const x = scalarRank(nodeOf(a)), n = x.shape.at(-1)!;
  const all = ir.host("argsort-desc", [x], x.shape, argsortRows(true));
  const idx = k === n ? all : ir.slice(all, x.shape.length - 1, 0, k);
  return [wrap(gatherRows(x, idx)), wrap(idx, int32)];
}

// ── contractions ────────────────────────────────────────────────────────────────
export function matmul(a: Like, b: Like): Array {
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
export function tensordot(a: Like, b: Like, axes: [number[], number[]] | number = 2): Array {
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
export function dot(a: Like, b: Like): Array {
  const x = nodeOf(a), y = nodeOf(b);
  if (!x.shape.length || !y.shape.length) return wrap(ir.mul(x, y));
  if (y.shape.length === 1) return wrap(reduceN(ir.mul(x, ir.broadcastTo(y, x.shape)), -1, {}, "ADD"));
  return tensordot(new Array(x), new Array(y), [[-1], [y.shape.length - 2]]);
}
// Einstein summation: label every axis, line the operands up label by label (a label
// written twice in one operand reads its diagonal), multiply, sum the labels the output lacks
export function einsum(spec: string, ...operands: Array[]): Array {
  const [lhs, rhsGiven] = spec.replace(/\s/g, "").split("->") as [string, string | undefined];
  const groups = lhs.split(",");
  if (groups.length !== operands.length) throw new Error(`einsum: ${groups.length} operand specs for ${operands.length} operands`);
  const extent = Math.max(0, ...operands.map((o, i) => o.ndim - groups[i]!.replace("...", "").length));
  const dots = Array_.from({ length: extent }, (_, i) => String.fromCodePoint(0x2460 + i));   // "..." axes get private labels, right-aligned
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
  rfft(a: Like): { real: Array; imag: Array } {
    const x = nodeOf(a), n = x.shape.at(-1)!, m = Math.floor(n / 2) + 1;
    const C = new Float32Array(n * m), S = new Float32Array(n * m);
    for (let t = 0; t < n; t++) for (let k = 0; k < m; k++) { const w = (2 * Math.PI * ((k * t) % n)) / n; C[t * m + k] = Math.cos(w); S[t * m + k] = -Math.sin(w); }
    const rows = ir.reshape(x, [Math.max(ir.numel(x.shape) / n, 1), n]), back = [...x.shape.slice(0, -1), m];
    return { real: wrap(ir.reshape(cellMatmul(rows, ir.data(C, [n, m])), back)), imag: wrap(ir.reshape(cellMatmul(rows, ir.data(S, [n, m])), back)) };
  },
};
