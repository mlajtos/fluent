// The user-facing vocabulary — every derived op is a COMPOSITION of the 18 generators
// (Backus: derived op = function; the laws fire on its body). Dimensions are never
// passed: the graph already knows them (a shape argument appears only where a tensor is
// born). Rank polymorphism the Iverson way: an op has an intrinsic CELL rank; extra
// leading axes are a FRAME it is mapped over (matmul's cell is [m,k]·[k,n]).

import type { UOp } from "./ir.ts";
import {
  konst, data, buffer, add, mul, recip, exp, sqrt, detach, where, cmplt,
  reshape, permute, broadcastTo, slice, reduceAxis, reduceMax, sum, gather,
} from "./ir.ts";
import { simplifyMany } from "./opt.ts";

export const neg = (a: UOp): UOp => mul(a, konst(-1));
export const sub = (a: UOp, b: UOp): UOp => add(a, neg(b));
export const div = (a: UOp, b: UOp): UOp => mul(a, recip(b));
export const rsqrt = (a: UOp): UOp => recip(sqrt(a));
export const max = (a: UOp, b: UOp): UOp => where(cmplt(a, b), b, a);
export const relu = (a: UOp): UOp => max(a, konst(0));
export const silu = (a: UOp): UOp => mul(a, recip(add(konst(1), exp(neg(a)))));   // x·σ(x)
export { konst, data, buffer, add, mul, recip, exp, sqrt, detach, where, cmplt, reshape, permute, broadcastTo, slice, reduceAxis, reduceMax, sum, gather };

// A·B at any rank: the defining law Σₖ A[…,i,k]·B[…,k,j] over the view algebra. A frame
// on either operand maps the cell over it; an unframed operand is shared across the frame
// (its ∇ sums the frame away — broadcast's transpose is reduce, for free).
export const matmul = (A: UOp, B: UOp): UOp => {
  const as = A.shape, bs = B.shape;
  if (as.length < 2 || bs.length < 2) throw new Error(`matmul: rank ≥ 2 operands, got [${as}]·[${bs}]`);
  const [m, k] = as.slice(-2), [k2, n] = bs.slice(-2);
  if (k !== k2) throw new Error(`matmul: cell [m,k]·[k,n] expected, got [${as.slice(-2)}]·[${bs.slice(-2)}]`);
  const aF = as.slice(0, -2), bF = bs.slice(0, -2);
  if (aF.length && bF.length && String(aF) !== String(bF)) throw new Error(`matmul: frames disagree, [${aF}] vs [${bF}]`);
  const F = aF.length ? aF : bF, T = [...F, m!, k!, n!];
  const p = mul(broadcastTo(reshape(A, as.length > 2 ? [...aF, m!, k!, 1] : [m!, k!, 1]), T),
    broadcastTo(reshape(B, bs.length > 2 ? [...bF, 1, k!, n!] : [1, k!, n!]), T));
  return reshape(reduceAxis(p, F.length + 1, true), [...F, m!, n!]);
};
export const linear = (x: UOp, W: UOp, b: UOp): UOp => add(matmul(x, W), b);   // b [n] broadcasts right-aligned

// RMSNorm over the last axis of x[m,d] with scale g[d]
export const rmsNorm = (x: UOp, g: UOp, eps = 1e-6): UOp => {
  const d = x.shape.at(-1)!;
  const inv = rsqrt(add(mul(reduceAxis(mul(x, x), x.shape.length - 1, true), konst(1 / d)), konst(eps)));
  return mul(mul(x, broadcastTo(inv, x.shape)), g);
};
export const layerNorm = (x: UOp, g: UOp, b: UOp, eps = 1e-5): UOp => {
  const d = x.shape.at(-1)!, ax = x.shape.length - 1;
  const mean = broadcastTo(mul(reduceAxis(x, ax, true), konst(1 / d)), x.shape);
  const xc = sub(x, mean);
  const v = mul(reduceAxis(mul(xc, xc), ax, true), konst(1 / d));
  return add(mul(mul(xc, broadcastTo(rsqrt(add(v, konst(eps))), x.shape)), g), b);
};
// numerically-stable softmax over the last axis (the max is DETACHed: softmax is
// shift-invariant, so ∇ needn't flow through it, and exp stays in f32 range)
export const softmaxRow = (x: UOp): UOp => {
  const ax = x.shape.length - 1;
  const e = exp(sub(x, detach(broadcastTo(reduceMax(x, ax, true), x.shape))));
  return div(e, broadcastTo(reduceAxis(e, ax, true), x.shape));
};

// RoPE (GPT-NeoX split): x⊙cos + (x·R)⊙sin, R the fixed rotate-half matrix
export const ropeMatrix = (d: number): UOp => {
  const R = new Float32Array(d * d), h = d / 2;
  for (let j = 0; j < d; j++) { const i = j < h ? j + h : j - h; R[i * d + j] = j < h ? -1 : 1; }
  return data(R, [d, d]);
};
export const ropeRow = (pos: number, d: number, base: number): { cos: Float32Array; sin: Float32Array } => {
  const cos = new Float32Array(d), sin = new Float32Array(d), h = d / 2;
  for (let i = 0; i < d; i++) { const a = pos * base ** (-2 * (i % h) / d); cos[i] = Math.cos(a); sin[i] = Math.sin(a); }
  return { cos, sin };
};
export const causalMask = (S: number): UOp => {
  const m = new Float32Array(S * S);
  for (let i = 0; i < S; i++) for (let j = 0; j < S; j++) m[i * S + j] = j <= i ? 0 : -1e30;
  return data(m, [S, S]);
};

// one Adam update, as graph — the optimizer lowers to WGSL like everything else
export const adamStep = (p: UOp, g: UOp, m: UOp, v: UOp, bc1: UOp, bc2: UOp, lr: UOp, b1 = 0.9, b2 = 0.999, eps = 1e-8): { p: UOp; m: UOp; v: UOp } => {
  const m2 = add(mul(m, konst(b1)), mul(g, konst(1 - b1)));
  const v2 = add(mul(v, konst(b2)), mul(mul(g, g), konst(1 - b2)));
  return { p: sub(p, mul(mul(lr, mul(m2, bc1)), recip(add(sqrt(mul(v2, bc2)), konst(eps))))), m: m2, v: v2 };
};

// Program equality by normalization: apply both to ONE set of fresh leaves, simplify with
// shared interning, compare node ids. `false` means NOT PROVABLY equal under the EXACT
// laws — never "different". CSE is the decision procedure; it was already there.
export const provablyEq = (f: (...xs: UOp[]) => UOp, g: (...xs: UOp[]) => UOp, sig: readonly number[][]): boolean => {
  const xs = sig.map((s, i) => buffer(`%eq${i}`, s));
  const [a, b] = simplifyMany([f(...xs), g(...xs)]);
  return a!.id === b!.id;
};
