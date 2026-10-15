// Gates for the ops beyond the 18 generators: the MATHS table, RANDOM, PAD, the product
// reduce and HOST routines — CPU == GPU, values == reference, ∇ == finite differences.
//   bun engine/tests/run.ts engine/tests/ops.ts

import { buffer, konst, add, mul, math, MATHS, random, pad, slice, reshape, reduceAxis, gather, host, sum, windows, broadcastTo, type UOp } from "../ir.ts";
import { grad } from "../grad.ts";
import { CPU, type Tensor } from "../cpu.ts";
import { GPU } from "../gpu.ts";

export default async function (): Promise<{ fails: number }> {
  let fails = 0;
  const check = (name: string, ok: boolean, detail = "") => { if (!ok) fails++; console.log(`  ${ok ? "✓" : "✗"} ${name}${detail ? "  " + detail : ""}`); };
  const cpu = new CPU(), gpu = await GPU.create();
  console.log(`engine op gates (cpu${gpu ? " + gpu" : ""})`);
  const T = (d: Float32Array, sh: readonly number[]): Tensor => ({ data: d, shape: sh });
  const maxDiff = (a: ArrayLike<number>, b: ArrayLike<number>) => { let m = 0; for (let i = 0; i < a.length; i++) { const x = a[i]!, y = b[i]!; if (x === y || (Number.isNaN(x) && Number.isNaN(y))) continue; if (Number.isNaN(x) || Number.isNaN(y) || !Number.isFinite(x) || !Number.isFinite(y)) return Infinity; m = Math.max(m, Math.abs(x - y) / (1 + Math.abs(y))); } return m; };
  const both = async (g: UOp, inputs: Map<string, Tensor>) => {
    const a = (await cpu.realize(g, inputs)).data;
    const b = gpu ? (await gpu.realize(g, inputs)).data : a;
    return { a, b };
  };

  // ── 1. every MATHS row: CPU == Math.*, GPU == CPU ────────────────────────────────
  const domain: Record<string, [number, number]> = { acosh: [1.1, 4], asin: [-0.9, 0.9], acos: [-0.9, 0.9], atanh: [-0.9, 0.9], tan: [-1.2, 1.2], log1p: [-0.5, 3] };
  const n = 64, xs = buffer("x", [n]), ys = buffer("y", [n]);
  for (const [fn, m] of Object.entries(MATHS)) {
    const [lo, hi] = domain[fn] ?? [-3, 3];
    const xd = Float32Array.from({ length: n }, (_, i) => lo + ((hi - lo) * i) / (n - 1));
    const yd = Float32Array.from({ length: n }, (_, i) => [0, 1, 2, 3, 0.5, -1, -2, 2.5][i % 8]!);
    if (fn === "pow") for (let i = 0; i < n; i++) xd[i] = [-2, -1.5, 0, 0.5, 2, 3][i % 6]!;
    if (fn === "round") for (let i = 0; i < n; i++) xd[i] = [-2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 0.4, -0.6][i % 8]!;
    const g = m.arity === 1 ? math(fn, xs) : math(fn, xs, ys);
    const { a, b } = await both(g, new Map([["x", T(xd, [n])], ["y", T(yd, [n])]]));
    const ref = Float32Array.from({ length: n }, (_, i) => (m.arity === 1 ? m.js(xd[i]!) : m.js(xd[i]!, yd[i]!)));
    const tol = fn === "expm1" || fn === "log1p" || fn === "cbrt" || fn === "pow" ? 2e-5 : 2e-6;
    check(`${fn}: cpu == Math, gpu == cpu`, maxDiff(a, ref) < 1e-6 && maxDiff(b, a) < tol, `cpu ${maxDiff(a, ref).toExponential(1)} gpu ${maxDiff(b, a).toExponential(1)}`);
  }

  // ── 2. derivatives of the MATHS rows == finite differences ─────────────────────────
  {
    let worst = 0, worstName = "";
    for (const fn of ["sin", "cos", "tan", "tanh", "asin", "acos", "atan", "asinh", "acosh", "atanh", "abs", "expm1", "log1p", "cbrt", "pow"]) {
      const s = buffer("s", []);
      const at = fn === "acosh" ? 1.7 : fn === "pow" ? 1.3 : 0.37;
      const g = fn === "pow" ? math("pow", s, konst(2.5)) : math(fn, s);
      const d = (await cpu.realize(grad(g, s), new Map([["s", T(new Float32Array([at]), [])]]))).data[0]!;
      const h = 1e-3, m = MATHS[fn]!;
      const f = (x: number) => (fn === "pow" ? m.js(x, 2.5) : m.js(x));
      const fd = (f(at + h) - f(at - h)) / (2 * h), e = Math.abs(d - fd) / (1 + Math.abs(fd));
      if (e > worst) { worst = e; worstName = fn; }
    }
    check("∇ of every smooth MATHS row == finite-diff", worst < 1e-3, `worst ${worst.toExponential(1)} (${worstName})`);
  }

  // ── 3. RANDOM: CPU and GPU draw the same bits; uniform in [0, 1) ───────────────────
  {
    const r = random([4096], 1234);
    const { a, b } = await both(r, new Map());
    let mean = 0, lo = 1, hi = 0; for (const v of a) { mean += v / a.length; lo = Math.min(lo, v); hi = Math.max(hi, v); }
    check("random: cpu == gpu bit for bit", maxDiff(a, b) === 0);
    check("random: in [0, 1), mean ≈ ½", lo >= 0 && hi < 1 && Math.abs(mean - 0.5) < 0.02, `mean ${mean.toFixed(4)}`);
    const other = (await cpu.realize(random([4096], 1235), new Map())).data;
    check("random: another seed, another draw", maxDiff(a, other) > 0.1);
    const fusedMean = (await cpu.realize(mul(sum(random([4096], 1234)), konst(1 / 4096)), new Map())).data[0]!;
    check("random fuses into its reader (same draw)", Math.abs(fusedMean - mean) < 1e-5);
  }

  // ── 4. PAD: values, concat as pad + pad, slice ∘ pad = id, ∇ both ways ────────────
  {
    const x = buffer("x", [3, 4]), xd = Float32Array.from({ length: 12 }, (_, i) => i + 1);
    const inp = new Map([["x", T(xd, [3, 4])]]);
    const p = pad(x, 1, 2, 1);
    const { a, b } = await both(p, inp);
    const ref = new Float32Array(3 * 7);
    for (let i = 0; i < 3; i++) for (let j = 0; j < 4; j++) ref[i * 7 + j + 2] = xd[i * 4 + j]!;
    check("pad: values (cpu, gpu)", maxDiff(a, ref) === 0 && maxDiff(b, ref) === 0);
    const cat = add(pad(x, 0, 0, 2), pad(slice(x, 0, 1, 2), 0, 3, 0));   // concat(x, x[1:3]) along axis 0
    const c = await both(cat, inp);
    const cref = Float32Array.from([...xd, ...xd.slice(4, 12)]);
    check("concat = pad + pad, one kernel's worth (cpu, gpu)", maxDiff(c.a, cref) === 0 && maxDiff(c.b, cref) === 0);
    const back = await both(slice(pad(x, 1, 2, 1), 1, 2, 4), inp);
    check("slice ∘ pad = id", maxDiff(back.a, xd) === 0 && maxDiff(back.b, xd) === 0);
    const w = buffer("w", [3, 7]), wd = Float32Array.from({ length: 21 }, (_, i) => Math.sin(i));
    const gs = await both(grad(sum(mul(slice(x, 1, 1, 2), slice(w, 1, 3, 2))), x), new Map([["x", T(xd, [3, 4])], ["w", T(wd, [3, 7])]]));
    const gref = new Float32Array(12); for (let i = 0; i < 3; i++) for (let j = 0; j < 2; j++) gref[i * 4 + 1 + j] = wd[i * 7 + 3 + j]!;
    check("∇ slice = pad (no scatter)", maxDiff(gs.a, gref) < 1e-6 && maxDiff(gs.b, gref) < 1e-6);
    const gp = await both(grad(sum(mul(pad(x, 1, 2, 1), w)), x), new Map([["x", T(xd, [3, 4])], ["w", T(wd, [3, 7])]]));
    const pref = new Float32Array(12); for (let i = 0; i < 3; i++) for (let j = 0; j < 4; j++) pref[i * 4 + j] = wd[i * 7 + 2 + j]!;
    check("∇ pad = slice", maxDiff(gp.a, pref) < 1e-6 && maxDiff(gp.b, pref) < 1e-6);
  }

  // ── 5. product reduce and its ∇ ───────────────────────────────────────────────────
  {
    const x = buffer("x", [2, 3]), xd = Float32Array.from([1, 2, 3, -1, 0.5, 4]);
    const inp = new Map([["x", T(xd, [2, 3])]]);
    const pr = await both(reduceAxis(x, 1, false, "MUL"), inp);
    check("Π over an axis (cpu, gpu)", maxDiff(pr.a, [6, -2]) === 0 && maxDiff(pr.b, [6, -2]) === 0);
    const g = (await cpu.realize(grad(sum(reduceAxis(x, 1, false, "MUL")), x), inp)).data;
    check("∇Π = Π ÷ x", maxDiff(g, [6, 3, 2, 2, -4, -0.5]) < 1e-6);
  }

  // ── 6. HOST: sort as gather by a routine's argsort — so ∇ sort is ∇ gather ─────────
  {
    const x = buffer("x", [6]), xd = Float32Array.from([3, -1, 4, 1, -5, 9]);
    const argsort = host("argsort", [x], [6], ([t]) => Float32Array.from([...t!.data.keys()].sort((i, j) => t!.data[i]! - t!.data[j]!)));
    const sorted = gather(reshape(x, [6, 1]), argsort);
    const inp = new Map([["x", T(xd, [6])]]);
    const s = await both(reshape(sorted, [6]), inp);
    check("sort through HOST argsort (cpu, gpu)", maxDiff(s.a, [-5, -1, 1, 3, 4, 9]) === 0 && maxDiff(s.b, [-5, -1, 1, 3, 4, 9]) === 0);
    const w = Float32Array.from([1, 2, 3, 4, 5, 6]);
    const gw = await both(grad(sum(mul(reshape(sorted, [6]), buffer("w", [6]))), x), new Map([["x", T(xd, [6])], ["w", T(w, [6])]]));
    check("∇ through sort routes back to the original positions", maxDiff(gw.a, [4, 2, 5, 3, 1, 6]) === 0 && maxDiff(gw.b, [4, 2, 5, 3, 1, 6]) === 0);
  }

  // ── 7. WINDOW: conv as one reduction over a window view; ∇ to field and kernel ────
  {
    const X = buffer("X", [5, 6]), K = buffer("K", [3, 3]), Wt = buffer("Wt", [3, 4]);
    const conv = (x: UOp, k: UOp) => { const w = windows(x, [3, 3]); return reduceAxis(reduceAxis(mul(w, broadcastTo(reshape(k, [1, 1, 3, 3]), w.shape)), 3, false), 2, false); };
    const loss = (x: UOp, k: UOp) => sum(mul(conv(x, k), Wt));
    const xd = Float32Array.from({ length: 30 }, (_, i) => Math.sin(i * 0.7)), kd = Float32Array.from({ length: 9 }, (_, i) => Math.cos(i * 1.3)), wd = Float32Array.from({ length: 12 }, (_, i) => (i % 5) - 2);
    const at = (x: Float32Array, k: Float32Array) => new Map([["X", T(x, [5, 6])], ["K", T(k, [3, 3])], ["Wt", T(wd, [3, 4])]]);
    const direct = new Float32Array(12);
    for (let i = 0; i < 3; i++) for (let j = 0; j < 4; j++) { let a = 0; for (let p = 0; p < 3; p++) for (let q = 0; q < 3; q++) a += xd[(i + p) * 6 + j + q]! * kd[p * 3 + q]!; direct[i * 4 + j] = a; }
    const v = await both(conv(X, K), at(xd, kd));
    check("conv through WINDOW == direct sum (cpu, gpu)", maxDiff(v.a, direct) < 1e-5 && maxDiff(v.b, direct) < 1e-5);
    const fd = async (which: "x" | "k", n: number) => {
      const out = new Float32Array(n), h = 1e-3;
      for (let j = 0; j < n; j++) {
        const up = (which === "x" ? xd : kd).slice(), dn = up.slice(); up[j]! += h; dn[j]! -= h;
        const f = async (d: Float32Array) => (await cpu.realize(loss(X, K), which === "x" ? at(d, kd) : at(xd, d))).data[0]!;
        out[j] = ((await f(up)) - (await f(dn))) / (2 * h);
      }
      return out;
    };
    const gx = await both(grad(loss(X, K), X), at(xd, kd)), gk = await both(grad(loss(X, K), K), at(xd, kd));
    check("∇ conv w.r.t. the field == finite-diff (cpu, gpu)", maxDiff(gx.a, await fd("x", 30)) < 1e-2 && maxDiff(gx.b, gx.a) < 1e-5);
    check("∇ conv w.r.t. the kernel == finite-diff (cpu, gpu)", maxDiff(gk.a, await fd("k", 9)) < 1e-2 && maxDiff(gk.b, gk.a) < 1e-5);
  }

  return { fails };
}
