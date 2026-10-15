// ∇ = transpose ∘ linearize — reverse mode DERIVED, not hand-written (the JAX construction).
// `linearize` pushes tangent seeds forward through the JVP table (all the calculus lives
// there, stated once, forward); the result is LINEAR in the seeds, and `transpose` is the
// contravariant involution on that linear graph — a tiny fixed table of dualities:
// copy⟷add, scale(self), broadcast⟷reduce, permute⟷inverse, reshape⟷reshape, slice⟷pad,
// gather⟷scatter, where-routes. One seeded multi-target driver serves vjp, grad, gradAll.

import type { UOp } from "./ir.ts";
import { node, konst, buffer, add, mul, recip, sqrt, log, where, cmplt, broadcastTo, reduceAxis, reshape, permute, slice, pad, math, strides, toposort } from "./ir.ts";

const zeros = (s: readonly number[]): UOp => (s.length ? broadcastTo(konst(0), s) : konst(0));
const scale = (coef: UOp, d: UOp | null): UOp | null => d && mul(coef, d);
const plus = (a: UOp | null, b: UOp | null): UOp | null => (a && b ? add(a, b) : a ?? b ?? null);
const neg = (a: UOp): UOp => mul(a, konst(-1));
const sq = (a: UOp): UOp => mul(a, a);
// d f(x) = f′(x)·dx per MATHS row; a missing row is a function that is flat almost everywhere
const MATH_JVP: Record<string, (u: UOp, x: UOp) => UOp> = {
  sin: (_u, x) => math("cos", x),
  cos: (_u, x) => neg(math("sin", x)),
  tan: (u) => add(konst(1), sq(u)),
  tanh: (u) => add(konst(1), neg(sq(u))),
  asin: (_u, x) => recip(sqrt(add(konst(1), neg(sq(x))))),
  acos: (_u, x) => neg(recip(sqrt(add(konst(1), neg(sq(x)))))),
  atan: (_u, x) => recip(add(konst(1), sq(x))),
  asinh: (_u, x) => recip(sqrt(add(sq(x), konst(1)))),
  acosh: (_u, x) => recip(sqrt(add(sq(x), konst(-1)))),
  atanh: (_u, x) => recip(add(konst(1), neg(sq(x)))),
  abs: (_u, x) => math("sign", x),
  expm1: (u) => add(u, konst(1)),
  log1p: (_u, x) => recip(add(konst(1), x)),
  cbrt: (u) => recip(mul(konst(3), sq(u))),
};

type Jvp = (u: UOp, d: (UOp | null)[]) => UOp | null;
export const JVP: Partial<Record<UOp["op"], Jvp>> = {
  CONSTANT: () => null, BUFFER: () => null, DETACH: () => null, LESS_THAN: () => null, RANDOM: () => null, IOTA: () => null, HOST: () => null,
  ADD: (_u, [a = null, b = null]) => plus(a, b),
  MULTIPLY: (u, [a = null, b = null]) => plus(scale(u.src[1]!, a), scale(u.src[0]!, b)),   // product rule
  RECIPROCAL: (u, [a = null]) => scale(mul(mul(u, u), konst(-1)), a),          // −u²·da  (u = 1/x)
  EXP: (u, [a = null]) => scale(u, a),
  LOG: (u, [a = null]) => scale(recip(u.src[0]!), a),
  SQRT: (u, [a = null]) => scale(mul(konst(0.5), recip(u)), a),               // da/(2√x) — reads its own output
  WHERE: (u, [, a = null, b = null]) => (a || b ? where(u.src[0]!, a ?? zeros(u.shape), b ?? zeros(u.shape)) : null),
  MATH: (u, [a = null, b = null]) => {
    const fn = (u.arg as { fn: string }).fn;
    if (fn === "atan2") {                                                      // d atan2(y, x) = (x·dy − y·dx) ÷ (x² + y²)
      const [y, x] = u.src as [UOp, UOp], r = recip(add(sq(x), sq(y)));
      return plus(scale(mul(x, r), a), scale(neg(mul(y, r)), b));
    }
    if (fn === "pow") {                                                        // d(xʸ) = y·xʸ⁻¹·dx + xʸ·ln x·dy
      const [x, y] = u.src as [UOp, UOp];
      return plus(scale(mul(y, math("pow", x, add(y, konst(-1)))), a), scale(mul(u, log(x)), b));
    }
    const rule = MATH_JVP[fn];
    return rule ? scale(rule(u, u.src[0]!), a) : null;
  },
  REDUCE: (u, [a = null]) => {
    if (!a) return null;
    const g = u.arg as { axis: number; keepdims: boolean; op: string };
    if (g.op === "ADD") return node("REDUCE", [a], u.arg, u.shape);            // Σ is linear
    if (g.op === "MUL") {                                                      // d Πx = Π·Σ(dx ÷ x)
      return mul(u, reduceAxis(mul(a, recip(u.src[0]!)), g.axis, g.keepdims));
    }
    const x = u.src[0]!, km = g.keepdims ? u : reshape(u, x.shape.map((d, i) => (i === g.axis ? 1 : d)));
    const isMax = where(cmplt(x, broadcastTo(km, x.shape)), zeros(x.shape), broadcastTo(konst(1), x.shape));
    const ties = reduceAxis(isMax, g.axis, g.keepdims);                       // tied maxima share the tangent evenly
    return mul(reduceAxis(mul(a, isMax), g.axis, g.keepdims), recip(ties));
  },
  BROADCAST: (u, [a = null]) => a && node("BROADCAST", [a], u.arg, u.shape),
  PERMUTE: (u, [a = null]) => a && node("PERMUTE", [a], u.arg, u.shape),
  RESHAPE: (u, [a = null]) => a && node("RESHAPE", [a], u.arg, u.shape),
  SLICE: (u, [a = null]) => a && node("SLICE", [a], u.arg, u.shape),
  PAD: (u, [a = null]) => a && node("PAD", [a], u.arg, u.shape),
  WINDOW: (u, [a = null]) => a && node("WINDOW", [a], u.arg, u.shape),
  GATHER: (u, [t = null]) => t && node("GATHER", [t, u.src[1]!], u.arg, u.shape),          // linear in the table; idx is primal
  SCATTER_ADD: (u, [, v = null]) => v && node("SCATTER_ADD", [u.src[0]!, v], u.arg, u.shape),
};

type Tr = (u: UOp, ct: UOp, isLin: (s: UOp) => boolean) => (UOp | null)[];
export const TRANSPOSE: Partial<Record<UOp["op"], Tr>> = {
  ADD: (_u, ct) => [ct, ct],                                                   // T[copy] = add, so T[add] = copy
  MULTIPLY: (u, ct, isLin) => (isLin(u.src[0]!) ? [mul(ct, u.src[1]!), null] : [null, mul(ct, u.src[0]!)]),
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
export const grad = (root: UOp, target: UOp): UOp => {
  if (root.shape.length) throw new Error("grad: root must be scalar");
  return vjp(root, [target], konst(1))[0]!;
};
export const gradAll = (root: UOp, targets: UOp[]): UOp[] => {
  if (root.shape.length) throw new Error("gradAll: root must be scalar");
  return vjp(root, targets, konst(1));
};
