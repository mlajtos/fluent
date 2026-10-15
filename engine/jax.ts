// The numerical surface Fluent's natives use — numpy, nn, lax, random, tree and the
// transformations — over the engine. ∇ is the engine's transpose ∘ linearize on the graph a
// function builds; there is no jit: computing a graph already fuses it, and a step built
// again finds its compiled plan.

import * as ir from "./ir.ts";
import { vjp as graphVjp } from "./grad.ts";
import * as numpy from "./numpy.ts";
import { GPU } from "./gpu.ts";
import { Array, nodeOf, type Like } from "./numpy.ts";

export { numpy };
export type { DType } from "./numpy.ts";

const wrap = (n: ir.UOp) => new Array(n);

// ── nn ──────────────────────────────────────────────────────────────────────────
const lastAxis = (x: ir.UOp, axis?: number) => (axis === undefined ? x.shape.length - 1 : axis < 0 ? axis + x.shape.length : axis);
export const nn = {
  softmax(a: Like, axis?: number): Array {
    const x = nodeOf(a), ax = lastAxis(x, axis);
    const e = ir.exp(ir.add(x, ir.mul(ir.broadcastTo(ir.reduceAxis(x, ax, true, "MAX"), x.shape), ir.konst(-1))));
    return wrap(ir.mul(e, ir.recip(ir.broadcastTo(ir.reduceAxis(e, ax, true), x.shape))));
  },
  logSoftmax(a: Like, axis?: number): Array {
    const x = nodeOf(a), ax = lastAxis(x, axis);
    const shifted = ir.add(x, ir.mul(ir.broadcastTo(ir.reduceAxis(x, ax, true, "MAX"), x.shape), ir.konst(-1)));
    return wrap(ir.add(shifted, ir.mul(ir.broadcastTo(ir.log(ir.reduceAxis(ir.exp(shifted), ax, true)), x.shape), ir.konst(-1))));
  },
  oneHot(a: Like, n: number): Array {
    const idx = nodeOf(a), shape = [...idx.shape, n];
    const iota = ir.data(Float32Array.from({ length: n }, (_, i) => i), [n]);
    const hit = (x: ir.UOp, y: ir.UOp) => ir.mul(ir.add(ir.konst(1), ir.mul(ir.cmplt(x, y), ir.konst(-1))), ir.add(ir.konst(1), ir.mul(ir.cmplt(y, x), ir.konst(-1))));
    return wrap(hit(ir.broadcastTo(ir.reshape(idx, [...idx.shape, 1]), shape), ir.broadcastTo(iota, shape)));
  },
};

// ── lax ─────────────────────────────────────────────────────────────────────────
// erf by Abramowitz–Stegun 7.1.26 (|error| < 1.5e-7) — the polynomial jax-js uses
const erf = (a: Like): Array => {
  const x = nodeOf(a), ax = ir.math("abs", x), t = ir.recip(ir.add(ir.konst(1), ir.mul(ir.konst(0.3275911), ax)));
  const poly = [1.061405429, -1.453152027, 1.421413741, -0.284496736, 0.254829592].reduce<ir.UOp>((p, c, i) => (i === 0 ? ir.konst(c) : ir.add(ir.mul(p, t), ir.konst(c))), ir.konst(0));
  const y = ir.add(ir.konst(1), ir.mul(ir.mul(ir.mul(poly, t), ir.exp(ir.mul(ir.mul(ax, ax), ir.konst(-1)))), ir.konst(-1)));
  return wrap(ir.mul(ir.math("sign", x), y));
};
// N-d cross-correlation of a [1, 1, …spatial] field with a [1, 1, …kernel] kernel, SAME
// zero padding: every position's window of the padded field, times the kernel, summed —
// one reduction over the taps, the windows a view that never materializes
function conv(lhs: Like, rhs: Like, _strides: number[], _padding: "SAME"): Array {
  const x = nodeOf(lhs), k = nodeOf(rhs), sp = x.shape.slice(2), ks = k.shape.slice(2);
  let f = ir.reshape(x, sp);
  sp.forEach((_, d) => { const K = ks[d]!, lo = Math.floor((K - 1) / 2); if (K > 1) f = ir.pad(f, d, lo, K - 1 - lo); });
  const w = ir.windows(f, ks);
  let out = ir.mul(w, ir.broadcastTo(ir.reshape(k, [...sp.map(() => 1), ...ks]), w.shape));
  for (let d = ks.length - 1; d >= 0; d--) out = ir.reduceAxis(out, sp.length + d, false);
  return wrap(ir.reshape(out, x.shape));
}
export const lax = {
  erf,
  conv,
  topK: (a: unknown, k: number): [Array, Array] => numpy.topK(a as Array, k),
};

// ── random: a key is a seed; a draw is a RANDOM leaf, so it fuses into its reader ──
const mix = (seed: number, salt: number) => (Math.imul((seed ^ Math.imul(salt, 0x27d4eb2f)) >>> 0, 0x165667b1) ^ salt) >>> 0;
export const random = {
  key: (seed: number): { seed: number } => ({ seed: seed >>> 0 }),
  uniform: (key: { seed: number }, shape: readonly number[]): Array => wrap(ir.random(shape, mix(key.seed, 1))),
  normal(key: { seed: number }, shape: readonly number[]): Array {                // Box–Muller
    const u1 = ir.random(shape, mix(key.seed, 2)), u2 = ir.random(shape, mix(key.seed, 3));
    const r = ir.sqrt(ir.mul(ir.log(ir.add(ir.konst(1), ir.mul(u1, ir.konst(-1)))), ir.konst(-2)));
    return wrap(ir.mul(r, ir.math("cos", ir.mul(u2, ir.konst(2 * Math.PI)))));
  },
};

// ── tree: nested arrays and records of Arrays ───────────────────────────────────
type Tree = unknown;
const mapTree = (fn: (...xs: any[]) => any, t: Tree, ...rest: Tree[]): Tree => {
  if (t instanceof Array) return fn(t, ...rest);
  if (globalThis.Array.isArray(t)) return t.map((x, i) => mapTree(fn, x, ...rest.map((r) => (r as unknown[])[i])));
  if (t && typeof t === "object") return Object.fromEntries(Object.entries(t).map(([k, v]) => [k, mapTree(fn, v, ...rest.map((r) => (r as Record<string, unknown>)[k]))]));
  return t;
};
export const tree = { map: mapTree };

// ── transformations ─────────────────────────────────────────────────────────────
// A traced argument is a fresh node — an identity reshape: the derivative is with respect
// to this use of the value, never to a copy of it the function closed over, and an outer ∇
// still differentiates through it (∇ of ∇). Realizing simplifies it away.
const leaf = (a: Array): Array => new Array(ir.reshape(a.node, a.node.shape), a.dtype);
const seedLike = (out: Array, ct: unknown): ir.UOp => (ct instanceof Array ? nodeOf(ct) : ir.broadcastTo(ir.konst(Number(ct ?? 1)), out.node.shape));
export function vjp<F extends (...xs: any[]) => Array>(f: F, primals: Array[]): [Array, (ct: unknown) => Array[]] {
  const leaves = primals.map(leaf);
  const out = f(...leaves);
  return [out, (ct: unknown) => graphVjp(out.node, leaves.map((l) => l.node), seedLike(out, ct)).map((n) => new Array(n))];
}
// valueAndGrad(f)(params, …rest): params is a tree of Arrays; the gradients come back in its shape
export function valueAndGrad(f: (...xs: any[]) => Array) {
  return (params: Tree, ...rest: unknown[]): [Array, Tree] => {
    const leaves: Array[] = [];
    const traced = mapTree((p: Array) => { const l = leaf(p); leaves.push(l); return l; }, params);
    const loss = f(traced, ...rest);
    const grads = graphVjp(loss.node, leaves.map((l) => l.node), ir.konst(1));
    let i = 0;
    return [loss, mapTree(() => new Array(grads[i++]!), params)];
  };
}
// ── devices: WebGPU where the platform has it, the CPU everywhere ────────────────
let gpu: GPU | null = null;
export async function init(): Promise<string[]> {
  gpu ??= await GPU.create();
  numpy.useDevice(gpu);
  return gpu ? ["webgpu", "cpu"] : ["cpu"];
}
export const getWebGPUDevice = (): GPUDevice => { if (!gpu) throw new Error("no WebGPU device"); return gpu.device; };
export const engineGpu = (): GPU | null => gpu;
// resolves once at most `depth` computations are still running on the GPU
export const gpuDrained = (depth: number): Promise<void> => gpu?.drained(depth) ?? Promise.resolve();

