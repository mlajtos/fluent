// ∇ = transpose ∘ linearize — reverse mode DERIVED, not hand-written (the JAX construction).
// `linearize` pushes tangent seeds forward through the JVP table (all the calculus lives
// there, stated once, forward); the result is LINEAR in the seeds, and `transpose` is the
// contravariant involution on that linear graph — a tiny fixed table of dualities:
// copy⟷add, scale(self), broadcast⟷reduce, permute⟷inverse, reshape⟷reshape, slice⟷pad,
// gather⟷scatter, where-routes. One seeded multi-target driver serves vjp, grad, gradAll.

import type { UOp, Fold } from "./ir.ts";
import { node, konst, buffer, iota, add, mul, recip, sqrt, log, where, broadcastTo, reduceAxis, reshape, permute, slice, pad, math, strides, sameShape, toposort } from "./ir.ts";

const zeros = (s: readonly number[]): UOp => (s.length ? broadcastTo(konst(0), s) : konst(0));
const scale = (coef: UOp, d: UOp | null): UOp | null => d && mul(coef, d);
const plus = (a: UOp | null, b: UOp | null): UOp | null => (a && b ? add(a, b) : a ?? b ?? null);
const neg = (a: UOp): UOp => mul(a, konst(-1));
const sq = (a: UOp): UOp => mul(a, a);
// df = Σ ∂f/∂xᵢ·dxᵢ per MATHS row, the partials listed in operand order; a missing row is
// a function that is flat almost everywhere
const MATH_JVP: Record<string, (u: UOp, x: UOp, y: UOp) => UOp[]> = {
  sin: (_u, x) => [math("cos", x)],
  cos: (_u, x) => [neg(math("sin", x))],
  tanh: (u) => [add(konst(1), neg(sq(u)))],
  asin: (_u, x) => [recip(sqrt(add(konst(1), neg(sq(x)))))],
  acos: (_u, x) => [neg(recip(sqrt(add(konst(1), neg(sq(x))))))],
  atan: (_u, x) => [recip(add(konst(1), sq(x)))],
  asinh: (_u, x) => [recip(sqrt(add(sq(x), konst(1))))],
  acosh: (_u, x) => [recip(sqrt(add(sq(x), konst(-1))))],
  atanh: (_u, x) => [recip(add(konst(1), neg(sq(x))))],
  abs: (_u, x) => [math("sign", x)],
  expm1: (u) => [add(u, konst(1))],
  log1p: (_u, x) => [recip(add(konst(1), x))],
  cbrt: (u) => [recip(mul(konst(3), sq(u)))],
  atan2: (_u, y, x) => { const r = recip(add(sq(x), sq(y))); return [mul(x, r), neg(mul(y, r))]; },  // (x·dy − y·dx) ÷ (x² + y²)
  div: (u, _x, y) => { const r = recip(y); return [r, neg(mul(u, r))]; },  // dx ÷ y − (x ÷ y)·dy ÷ y
  pow: (u, x, y) => [mul(y, math("pow", x, add(y, konst(-1)))), mul(u, log(x))],  // y·xʸ⁻¹·dx + xʸ·ln x·dy
  mul0: (_u, x, y) => [y, x],                                                // a product's: y·dx + x·dy
};

// v at idx along axis ax, counted from the end: out[o] = v[o with o_ax = idx[o]], 1 at ax;
// idx may lead with axes v lacks. It moves into what v is made of – maps, broadcasts, views,
// gathers and sums over a point's coordinates – so the derivative a maximum reads is taken at
// its winner alone, and its transpose meets only the winners' terms. What it cannot move
// into, it sums against a one-hot of idx.
const MAP = new Set<UOp["op"]>(["ADD", "MULTIPLY", "RECIPROCAL", "EXP", "LOG", "SQRT", "LESS_THAN", "WHERE", "DETACH", "MATH"]);
const along = (v: UOp, ax: number, idx: UOp, memo: Map<string, UOp>): UOp => {
  const key = `${v.id} ${ax} ${idx.id}`, hit = memo.get(key);
  if (hit) return hit;
  const r = v.shape.length, n = Math.max(r, idx.shape.length), lead = n - r, at = (s: readonly number[], i: number) => s[s.length - n + i] ?? 1;
  const R = Array.from({ length: n }, (_, i) => (i === n + ax ? 1 : Math.max(at(v.shape, i), at(idx.shape, i))));
  const fit = (w: UOp) => (sameShape(w.shape, R) ? w : broadcastTo(w, R));
  const free = (d: number) => at(idx.shape, lead + d) === 1;                    // idx is the same along v's axis d
  const g = v.arg as { axis: number; keepdims: boolean; op: Fold; start: number; len: number; before: number; after: number };
  let out: UOp | undefined;
  if (v.shape.at(ax) === 1) out = fit(v);
  else if (MAP.has(v.op)) out = node(v.op, v.src.map((s) => along(s, ax, idx, memo)), v.arg, R);
  else if (v.op === "BROADCAST") { const s = v.src[0]!; out = fit(-ax > s.shape.length ? s : along(s, ax, idx, memo)); }
  else if (v.op === "IOTA") out = fit(idx);
  else if (v.op === "REDUCE" && g.keepdims && free(g.axis) && v.src[0]!.shape[g.axis]! <= 4) out = reduceAxis(along(v.src[0]!, ax, idx, memo), lead + g.axis, true, g.op);
  else if (v.op === "SLICE" && g.axis === r + ax) out = along(v.src[0]!, ax, add(idx, konst(g.start)), memo);
  else if ((v.op === "SLICE" || v.op === "PAD") && free(g.axis)) { const t = along(v.src[0]!, ax, idx, memo); out = v.op === "SLICE" ? slice(t, lead + g.axis, g.start, g.len) : pad(t, lead + g.axis, g.before, g.after); }
  else if (v.op === "PERMUTE") {
    const p = v.arg as number[], front = Array.from({ length: lead }, (_, i) => i);
    const ix = permute(idx, [...front, ...p.map((_, j) => p.indexOf(j) + lead)]);   // idx in the source's axis order
    out = permute(along(v.src[0]!, p[r + ax]! - r, ix, memo), [...front, ...p.map((j) => j + lead)]);
  } else if (v.op === "RESHAPE") {
    // a reshape that only adds or drops unit axes: idx follows it
    const s = v.src[0]!, nu = (sh: readonly number[]) => sh.flatMap((d, i) => (d === 1 ? [] : [i]));
    const a = nu(v.shape), b = nu(s.shape);
    if (a.length === b.length && a.every((i, k) => v.shape[i] === s.shape[b[k]!]) && v.shape.every((d, i) => d !== 1 || free(i))) {
      const is = [...idx.shape.slice(0, Math.max(0, idx.shape.length - r)), ...s.shape.map(() => 1)];
      a.forEach((i, k) => { is[is.length - s.shape.length + b[k]!] = at(idx.shape, lead + i); });
      out = reshape(along(s, b[a.indexOf(r + ax)]! - s.shape.length, reshape(idx, is), memo), R);
    }
  } else if (v.op === "GATHER" && r + ax === 0 && v.shape.every((_, d) => !d || free(d))) {
    // the row each winner names – an index names the row of its whole part – then the table at it
    const [t, i] = v.src as [UOp, UOp], rows = math("trunc", along(i, -1, reshape(idx, idx.shape.slice(0, idx.shape.length - r + 1)), memo));
    out = along(t, -t.shape.length, reshape(rows, [...rows.shape, ...t.shape.slice(1).map(() => 1)]), memo);
  }
  if (!out) {                                                                  // a one-hot of idx, summed
    const K = v.shape.at(ax)!, k = reshape(iota(K), [K, ...Array(-ax - 1).fill(1)]);
    out = reduceAxis(where(math("eq", k, idx), v, konst(0)), n + ax, true);
  }
  memo.set(key, out);
  return out;
};

type Jvp = (u: UOp, d: (UOp | null)[]) => UOp | null;
const linear: Jvp = (u, [a = null]) => a && node(u.op, [a], u.arg, u.shape);  // a view of the tangent, the same view
export const JVP: Partial<Record<UOp["op"], Jvp>> = {
  CONSTANT: () => null, BUFFER: () => null, DETACH: () => null, LESS_THAN: () => null, RANDOM: () => null, IOTA: () => null,
  ADD: (_u, [a = null, b = null]) => plus(a, b),
  MULTIPLY: (u, [a = null, b = null]) => plus(scale(u.src[1]!, a), scale(u.src[0]!, b)),   // product rule
  RECIPROCAL: (u, [a = null]) => scale(mul(mul(u, u), konst(-1)), a),          // −u²·da  (u = 1/x)
  EXP: (u, [a = null]) => scale(u, a),
  LOG: (u, [a = null]) => scale(recip(u.src[0]!), a),
  SQRT: (u, [a = null]) => scale(mul(konst(0.5), recip(u)), a),               // da/(2√x) — reads its own output
  WHERE: (u, [, a = null, b = null]) => (a || b ? where(u.src[0]!, a ?? zeros(u.shape), b ?? zeros(u.shape)) : null),
  MATH: (u, d) => (MATH_JVP[(u.arg as { fn: string }).fn]?.(u, u.src[0]!, u.src[1]!) ?? []).reduce<UOp | null>((t, c, i) => plus(t, scale(c, d[i] ?? null)), null),
  REDUCE: (u, [a = null]) => {
    if (!a) return null;
    const g = u.arg as { axis: number; keepdims: boolean; op: string };
    if (g.op === "ADD") return node("REDUCE", [a], u.arg, u.shape);            // Σ is linear
    if (g.op === "MUL") {                                                      // d Πx = Π·Σ(dx ÷ x)
      return mul(u, reduceAxis(mul(a, recip(u.src[0]!)), g.axis, g.keepdims));
    }
    if (g.op === "ARGMAX") return null;                                       // an index: flat almost everywhere
    // a maximum's tangent is its winner's: ARGMAX finds the winner the way a maximum is found,
    // comparing the values with each other – never a recomputed value with the maximum, which
    // on a GPU may differ in its last bits – and a tie goes to the first
    const t = along(a, g.axis - a.shape.length, reduceAxis(u.src[0]!, g.axis, true, "ARGMAX"), new Map());
    return g.keepdims ? t : reshape(t, u.shape);
  },
  BROADCAST: linear, PERMUTE: linear, RESHAPE: linear, SLICE: linear, PAD: linear, WINDOW: linear,
  GATHER: (u, [t = null]) => t && node("GATHER", [t, u.src[1]!], u.arg, u.shape),          // linear in the table; idx is primal
  SCATTER_ADD: (u, [, v = null]) => v && node("SCATTER_ADD", [u.src[0]!, v], u.arg, u.shape),
};

// A cotangent of exactly 0 carries nothing back, whatever it meets. `where` routes 0 to
// the branch it didn't take, and that branch's slope may be infinite or NaN there – the
// slope of sin(x) ÷ x at 0 – where 0 × ∞ would poison the answer. So the transpose is the
// product in which 0 is strong, mul0; its derivative is the product's, so an outer ∇ still
// sees ct × f change with ct at ct = 0. A NaN the cotangent itself carries still
// propagates; a finite constant factor needs no guard.
const finite = (f: UOp): boolean =>
  (f.op === "CONSTANT" && Array.from(f.arg as Float32Array).every(Number.isFinite)) ||
  ((f.op === "BROADCAST" || f.op === "RESHAPE") && finite(f.src[0]!));
const times = (ct: UOp, f: UOp): UOp => (finite(f) ? mul(ct, f) : math("mul0", ct, f));

type Tr = (u: UOp, ct: UOp, isLin: (s: UOp) => boolean) => (UOp | null)[];
export const TRANSPOSE: Partial<Record<UOp["op"], Tr>> = {
  ADD: (_u, ct) => [ct, ct],                                                   // T[copy] = add, so T[add] = copy
  MULTIPLY: (u, ct, isLin) => (isLin(u.src[0]!) ? [times(ct, u.src[1]!), null] : [null, times(ct, u.src[0]!)]),
  WHERE: (u, ct) => { const z = zeros(u.shape); return [null, where(u.src[0]!, ct, z), where(u.src[0]!, z, ct)]; },
  REDUCE: (u, ct) => {
    const { axis, keepdims } = u.arg as { axis: number; keepdims: boolean }, s = u.src[0]!.shape;
    const kd = keepdims || axis === 0 ? ct : reshape(ct, s.map((d, i) => (i === axis ? 1 : d)));
    return [broadcastTo(kd, s)];
  },
  BROADCAST: (u, ct) => {
    const inSh = u.src[0]!.shape, off = u.shape.length - inSh.length;
    let g = ct;
    for (let d = u.shape.length - 1; d >= off; d--) if (inSh[d - off] === 1 && u.shape[d]! > 1) g = reduceAxis(g, d, true);
    for (let d = 0; d < off; d++) g = reduceAxis(g, 0, false);
    return [g];
  },
  PERMUTE: (u, ct) => { const p = u.arg as number[], inv = p.map(() => 0); p.forEach((v, i) => { inv[v] = i; }); return [permute(ct, inv)]; },
  RESHAPE: (u, ct) => [reshape(ct, u.src[0]!.shape)],
  SLICE: (u, ct) => { const { axis, start, len } = u.arg as { axis: number; start: number; len: number }; return [pad(ct, axis, start, u.src[0]!.shape[axis]! - start - len)]; },
  PAD: (u, ct) => { const { axis, before } = u.arg as { axis: number; before: number }; return [slice(ct, axis, before, u.src[0]!.shape[axis]!)]; },
  WINDOW: (u, ct) => {                                                         // overlap-add: every tap's plane, shifted back to where it read
    const ks = u.arg as number[], x = u.src[0]!, r = x.shape.length, n = ks.length, out = u.shape.slice(r - n, r);
    const tapSt = strides(ks);
    let sum: UOp | null = null;
    for (let t = 0; t < ks.reduce((a, b) => a * b, 1); t++) {
      const at = ks.map((k, d) => Math.floor(t / tapSt[d]!) % k);
      let plane = ct;
      for (let d = n - 1; d >= 0; d--) plane = reshape(slice(plane, r + d, at[d]!, 1), plane.shape.filter((_, i) => i !== r + d));
      for (let d = 0; d < n; d++) plane = pad(plane, r - n + d, at[d]!, x.shape[r - n + d]! - out[d]! - at[d]!);
      sum = sum ? add(sum, plane) : plane;
    }
    return [sum];
  },
  GATHER: (u, ct) => [node("SCATTER_ADD", [u.src[1]!, ct], { size: u.src[0]!.shape[0] }, u.src[0]!.shape), null],
  SCATTER_ADD: (u, ct) => [null, node("GATHER", [ct, u.src[0]!], null, u.src[1]!.shape)],
};

// The one driver: seed a cotangent on `root`, read cotangents for every target — one
// forward linearization (all seeds live), one reverse transpose (cotangents summed at
// forks: the tape is literally the set of summed forks).
export function vjp(root: UOp, targets: UOp[], seed: UOp): UOp[] {
  const dts = new Map(targets.map((t) => [t.id, buffer(`%dt${t.id}`, t.shape)] as const));
  const tan = new Map<number, UOp | null>();
  for (const u of toposort(root)) {
    const s = dts.get(u.id);
    if (s) { tan.set(u.id, s); continue; }
    const rule = JVP[u.op] ?? ((): never => { throw new Error(`∇: no JVP rule for ${u.op}`); })();
    tan.set(u.id, rule(u, u.src.map((x) => tan.get(x.id) ?? null)));
  }
  const df = tan.get(root.id) ?? null;
  if (!df) return targets.map((t) => zeros(t.shape));
  const order = toposort(df), dtIds = new Set([...dts.values()].map((d) => d.id));
  const lin = new Set<number>(dtIds);
  for (const u of order) if (u.src.some((x) => lin.has(x.id))) lin.add(u.id);
  const isLin = (x: UOp) => lin.has(x.id);
  const cot = new Map<number, UOp>([[df.id, seed]]);
  for (let i = order.length - 1; i >= 0; i--) {
    const u = order[i]!;
    if (!isLin(u) || dtIds.has(u.id)) continue;
    const c = cot.get(u.id);
    if (!c) continue;
    const rule = TRANSPOSE[u.op] ?? ((): never => { throw new Error(`∇: no transpose rule for linear op ${u.op}`); })();
    const cs = rule(u, c, isLin);
    u.src.forEach((s, k) => { if (isLin(s) && cs[k]) cot.set(s.id, cot.has(s.id) ? add(cot.get(s.id)!, cs[k]!) : cs[k]!); });
  }
  return targets.map((t) => cot.get(dts.get(t.id)!.id) ?? zeros(t.shape));
}
export const gradAll = (root: UOp, targets: UOp[]): UOp[] => {
  if (root.shape.length) throw new Error("∇: the root must be a scalar");
  return vjp(root, targets, konst(1));
};
export const grad = (root: UOp, target: UOp): UOp => gradAll(root, [target])[0]!;
