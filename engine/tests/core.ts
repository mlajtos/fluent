// Core gates: CPU↔GPU parity per op family, fusion counts (the schedule's shape), ∇ vs
// finite differences on random graphs, and a training smoke. Runs in headless Chrome:
//   bun engine/tests/run.ts

import { buffer, konst, data, add, mul, recip, exp, sqrt, where, cmplt, reshape, permute, transpose, broadcastTo, slice, pad, reduceAxis, reduceMax, sum, gather, scatterAdd, iota, math, toposort, numel, type UOp } from "../ir.ts";
import { optimize, optimizeMany, simplifyMany, rangesIn, accCost, COST } from "../opt.ts";
import { grad, gradAll } from "../grad.ts";
import { CPU, type HostData } from "../cpu.ts";
import { realize, realizeMany } from "./realize.ts";
import { GPU, lowerMany } from "../gpu.ts";
import { matmul, rmsNorm, softmaxRow, silu, relu, sub, adamStep, provablyEq } from "./vocab.ts";

export default async function (): Promise<{ fails: number }> {
  let seed = 0x9e3779b1;
  const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 2 ** 32);
  const rv = (n: number, s = 1) => Float32Array.from({ length: n }, () => (rnd() - 0.5) * 2 * s);
  const T = (d: Float32Array, sh: readonly number[]): HostData => ({ data: d, shape: sh });
  const rel = (got: ArrayLike<number>, exp: ArrayLike<number>) => {
    let md = 0, on = 0;
    for (let i = 0; i < exp.length; i++) { md = Math.max(md, Math.abs(got[i]! - exp[i]!)); on += exp[i]! ** 2; }
    return md / (Math.sqrt(on / Math.max(exp.length, 1)) + 1e-9);
  };
  let fails = 0;
  const check = (name: string, ok: boolean, detail = "") => { if (!ok) fails++; console.log(`  ${ok ? "✓" : "✗"} ${name}${detail ? "  " + detail : ""}`); };

  const cpu = new CPU(), gpu = await GPU.create();
  console.log(`engine core gates (cpu${gpu ? " + gpu" : ""})`);

  // ── 1. parity battery: every op family, CPU == GPU ─────────────────────────────
  const M = 3, N = 4;
  const x = buffer("x", [M, N]), y = buffer("y", [M, N]), v = buffer("v", [N]);
  const ins = () => new Map<string, HostData>([["x", T(rv(M * N), [M, N])], ["y", T(rv(M * N), [M, N])], ["v", T(rv(N), [N])]]);
  const CASES: [string, UOp][] = [
    ["elementwise chain", add(mul(exp(mul(x, konst(0.5))), y), recip(add(mul(x, x), konst(2))))],
    ["relu/where chain (fused ALU)", add(mul(relu(exp(x)), y), konst(1))],
    ["min/max/compare", where(cmplt(x, y), sub(y, x), sqrt(add(mul(x, x), konst(1))))],
    ["broadcast row", mul(x, broadcastTo(v, [M, N]))],
    ["transpose read", mul(transpose(x), transpose(y))],
    ["reshape+slice read", mul(slice(reshape(x, [N, M]), 0, 1, 2), konst(3))],
    ["reduce axis0", reduceAxis(x, 0, false)],
    ["reduce axis1 keepdims", reduceAxis(x, 1, true)],
    ["reduce max", reduceMax(x, 1, false)],
    ["reduce of view", reduceAxis(transpose(x), 0, true)],
    ["rmsNorm (one kernel)", rmsNorm(x, v)],
    ["softmax row (stable)", softmaxRow(x)],
    ["matmul 2d", matmul(x, transpose(y))],
    ["matmul Bᵀ view", matmul(x, permute(y, [1, 0]))],
    ["sum to scalar", sum(mul(x, y))],
  ];
  for (const [name, g] of CASES) {
    const i1 = ins();
    const a = await realize(cpu, g, i1);
    const b = gpu ? await realize(gpu, g, i1) : a;
    check(`parity: ${name}`, rel(b.data, a.data) < 1e-5, `relΔ ${rel(b.data, a.data).toExponential(1)}`);
  }
  // gather/scatter
  {
    const W = buffer("W", [6, 4]), ids = buffer("ids", [5]);
    const i2 = new Map<string, HostData>([["W", T(rv(24), [6, 4])], ["ids", T(new Float32Array([2, 0, 4, 2, 5]), [5])]]);
    const g1 = gather(W, ids), s1 = scatterAdd(ids, gather(W, ids), 6);
    for (const [nm, g] of [["gather", g1], ["scatterAdd", s1]] as const) {
      const a = await realize(cpu, g, i2), b = gpu ? await realize(gpu, g, i2) : a;
      check(`parity: ${nm}`, rel(b.data, a.data) < 1e-5);
    }
    { // an index names the row of its whole part; NaN names none
      const at = new Map<string, HostData>([["ids", T(new Float32Array([2.5, -0.5, 4.9, NaN, 5]), [5])], ["V", T(rv(20), [5, 4])]]);
      const s2 = scatterAdd(ids, buffer("V", [5, 4]), 6), a = await realize(cpu, s2, at), b = gpu ? await realize(gpu, s2, at) : a;
      check("parity: scatterAdd at fractional and NaN indices", rel(b.data, a.data) < 1e-5);
    }
    const tbl = (await realize(cpu, g1, i2)).data;
    const Wd = i2.get("W")!.data;
    check("gather values", [0, 1, 2, 3].every((c) => tbl[c] === Wd[2 * 4 + c]));
    // a gather read twice, a gather of a computed table, a gather of a gather, and a gather of
    // one row named in the graph: the same numbers on both devices as the plain gather gives
    const twice = add(mul(g1, g1), exp(g1)), ofExp = gather(exp(W), ids), ofGather = gather(g1, buffer("j", [3]));
    const row = gather(W, reshape(konst(4), [1]));
    i2.set("j", T(new Float32Array([4, 0, 3]), [3]));
    for (const [nm, g] of [["gather read twice", twice], ["gather of exp", ofExp], ["gather of a gather", ofGather], ["gather of a constant row", row]] as const) {
      const a = await realize(cpu, g, i2), b = gpu ? await realize(gpu, g, i2) : a;
      check(`parity: ${nm}`, rel(b.data, a.data) < 1e-5);
    }
    const ofGatherV = (await realize(cpu, ofGather, i2)).data, rowV = (await realize(cpu, row, i2)).data, idsV = [2, 0, 4, 2, 5];
    check("gather of a gather: rows ids[j]", [4, 0, 3].every((j, r) => [0, 1, 2, 3].every((c) => ofGatherV[r * 4 + c] === Wd[idsV[j]! * 4 + c])));
    check("gather of a constant row: that row", [0, 1, 2, 3].every((c) => rowV[c] === Wd[4 * 4 + c]));
    // a batch: each of 3 tables of 5 rows read at its own 4 indices, and a scatter of each
    // batch's 4 rows into its own 5
    const B = new Map<string, HostData>([["Tb", T(rv(30), [3, 5, 2])], ["ib", T(Float32Array.from({ length: 12 }, (_, i) => (i * 7) % 5), [3, 4])], ["Vb", T(rv(24), [3, 4, 2])]]);
    const gb = gather(buffer("Tb", [3, 5, 2]), buffer("ib", [3, 4])), sb = scatterAdd(buffer("ib", [3, 4]), buffer("Vb", [3, 4, 2]), 5);
    for (const [nm, g] of [["batched gather", gb], ["batched scatterAdd", sb]] as const) {
      const a = await realize(cpu, g, B), b = gpu ? await realize(gpu, g, B) : a;
      check(`parity: ${nm}`, rel(b.data, a.data) < 1e-5);
    }
    const [Tb, ib, Vb] = ["Tb", "ib", "Vb"].map((k) => B.get(k)!.data), gv = (await realize(cpu, gb, B)).data, sv = (await realize(cpu, sb, B)).data;
    const want = new Float32Array(30);
    for (let b = 0; b < 3; b++) for (let j = 0; j < 4; j++) for (let c = 0; c < 2; c++) want[(b * 5 + ib[b * 4 + j]!) * 2 + c] += Vb[(b * 4 + j) * 2 + c]!;
    check("batched gather: each batch's own rows", [...gv].every((v, i) => v === Tb[(Math.floor(i / 8) * 5 + ib[Math.floor(i / 2)]!) * 2 + (i % 2)]));
    check("batched scatterAdd: into each batch's own rows", rel(sv, want) < 1e-6);
  }
  { // −0 is a constant of its own: 1 ÷ (p·0) is ∞, 1 ÷ (p·−0) is −∞
    const p = buffer("p", [2]), at = new Map([["p", T(Float32Array.of(1, 2), [2])]]);
    for (const z of [0, -0]) {
      const g = recip(mul(p, konst(z))), a = (await realize(cpu, g, at)).data, b = gpu ? (await realize(gpu, g, at)).data : a;
      check(`parity: 1 ÷ (p·${Object.is(z, -0) ? "−0" : "0"})`, a[0] === 1 / z && b[0] === 1 / z, `cpu ${a[0]} gpu ${b[0]}`);
    }
  }
  // framed matmul vs looped reference
  {
    const B = 3, m = 4, k = 5, n = 6;
    const A = buffer("A", [B, m, k]), Bb = buffer("B", [B, k, n]), Sh = buffer("S", [k, n]);
    const Ad = rv(B * m * k), Bd = rv(B * k * n), Sd = rv(k * n);
    const i3 = new Map<string, HostData>([["A", T(Ad, [B, m, k])], ["B", T(Bd, [B, k, n])], ["S", T(Sd, [k, n])]]);
    const ref = new Float32Array(B * m * n), refS = new Float32Array(B * m * n);
    for (let b = 0; b < B; b++) for (let i = 0; i < m; i++) for (let j = 0; j < n; j++) {
      let s = 0, s2 = 0;
      for (let q = 0; q < k; q++) { s += Ad[b * m * k + i * k + q]! * Bd[b * k * n + q * n + j]!; s2 += Ad[b * m * k + i * k + q]! * Sd[q * n + j]!; }
      ref[b * m * n + i * n + j] = s; refS[b * m * n + i * n + j] = s2;
    }
    const got = await realize(cpu, matmul(A, Bb), i3), gotS = await realize(cpu, matmul(A, Sh), i3);
    check("matmul frame [3]", rel(got.data, ref) < 1e-4);
    check("matmul frame, B shared", rel(gotS.data, refS) < 1e-4);
    if (gpu) {
      const gg = await realize(gpu, matmul(A, Bb), i3);
      check("matmul frame gpu parity", rel(gg.data, got.data) < 1e-5);
    }
  }

  // ── 1b. long reductions: the GPU splits them over workgroups, the CPU runs one loop ──────
  // A maximum is the same in any order, so the split's is the loop's to the bit, with NaN,
  // ±∞ and ±0 among the values; so is a sum of integers, and a product that neither rounds
  // nor meets 0 · ∞.
  {
    const same = (a: Float32Array, b: Float32Array, tol = 0) => a.length === b.length && a.every((v, i) => Object.is(v, b[i]) || (Number.isNaN(v) && Number.isNaN(b[i]!)) || Math.abs(v - b[i]!) <= tol * Math.abs(b[i]!));
    const H = 300, W = 700, picture = buffer("p", [H, W, 3]), flat = buffer("f", [H * W * 3]), n = H * W * 3;
    const neg = (u: UOp) => mul(u, konst(-1));
    const all = (u: UOp, op: "ADD" | "MAX" | "MUL") => reduceAxis(reduceAxis(u, 0, true, op), 1, true, op);
    const fill = (f: (i: number) => number) => Float32Array.from({ length: n }, (_, i) => f(i));
    const DATA: [string, Float32Array, boolean][] = [                       // whether sums and products are exact too
      ["random", fill(() => (rnd() - 0.5) * 2), false],
      ["a NaN", fill((i) => (i === 123457 ? NaN : rnd())), false],
      ["±∞", fill((i) => (i === 5 ? Infinity : i === 400001 ? -Infinity : 1 + rnd())), false],
      ["0 and −0", fill((i) => (i % 7 ? -0 : 0)), true],
      ["only −0", fill(() => -0), true],
      ["one 0 among −0s", fill((i) => (i ? -0 : 0)), true],
      ["integers", fill((i) => (i % 13) - 6), true],
      ["±1 and a few 2s", fill((i) => (i % 9973 === 0 ? 2 : i % 5 ? 1 : -1)), true],
    ];
    for (const [name, d, integral] of DATA) {
      const at = new Map<string, HostData>([["p", T(d, [H, W, 3])], ["f", T(d, [n])]]);
      const roots = [all(picture, "MAX"), neg(all(neg(picture), "MAX")), reduceAxis(flat, 0, false, "MAX"), neg(reduceAxis(neg(flat), 0, false, "MAX")), all(picture, "ADD"), reduceAxis(flat, 0, false, "ADD"), all(picture, "MUL")];
      const a = await realizeMany(cpu, roots, at), b = gpu ? await realizeMany(gpu, roots, at) : a;
      // a sum of fractions rounds in another order
      check(`long reductions over ${name}: GPU = CPU${integral ? ", to the bit" : ", max and min to the bit"}`, a.every((x, i) => same(b[i]!.data, x.data, i < 4 || integral ? 0 : 1e-5)),
        a.map((x, i) => `${x.data[0]}/${b[i]!.data[0]}`).join(" "));
    }
    { // the larger of 0 and −0 is 0, either way round, as Math.max says
      const z = buffer("z", [2]), g = reduceAxis(z, 0, false, "MAX");
      const got = await Promise.all([[0, -0], [-0, 0]].map(async (d) => (await realize(gpu ?? cpu, g, new Map([["z", T(Float32Array.from(d), [2])]]))).data[0]!));
      check("max(0, −0) and max(−0, 0) are 0", got.every((v) => Object.is(v, 0)), got.map((v) => (Object.is(v, -0) ? "−0" : v)).join(", "));
    }
    { // an empty reduction is its identity; ∇ of a split maximum is the loop's
      const e = buffer("e", [0, 3]), at = new Map<string, HostData>([["e", T(new Float32Array(0), [0, 3])]]);
      const roots = [reduceAxis(e, 0, false, "ADD"), reduceAxis(e, 0, false, "MAX"), reduceAxis(e, 0, false, "MUL")];
      const a = await realizeMany(cpu, roots, at), b = gpu ? await realizeMany(gpu, roots, at) : a;
      check("empty reductions: 0, −∞, 1 on both", a.every((x, i) => same(x.data, b[i]!.data)) && [0, -Infinity, 1].every((v, i) => a[i]!.data.every((x) => x === v)));
      const d = fill((i) => Math.floor(i / 3) % 1001), atP = new Map<string, HostData>([["p", T(d, [H, W, 3])]]);
      const g = grad(sum(all(picture, "MAX")), picture), ga = await realize(cpu, g, atP), gb = gpu ? await realize(gpu, g, atP) : ga;
      check("∇ of a split maximum: GPU = CPU, to the bit", same(ga.data, gb.data), `Σ∇ ${ga.data.reduce((s, v) => s + v, 0)}`);
    }
  }

  // ── 2. the schedule's shape: kernel counts ───────────────────────────────────────
  const kernels = (g: UOp): UOp[] => toposort(optimize(g)).filter((u) => u.op === "FUSED");
  const wgslOf = (g: UOp): string[] => { const out: string[] = []; for (const u of toposort(lowerMany([g])[0]!)) if (u.op === "KERNEL") out.push((u.arg as { wgsl: string }).wgsl); return out; };
  check("relu chain fuses to ONE kernel", kernels(add(mul(relu(exp(x)), y), konst(1))).length === 1);
  check("rmsNorm is ONE kernel", kernels(rmsNorm(x, v)).length === 1);
  check("softmax row is ONE kernel", kernels(softmaxRow(x)).length === 1);
  check("matmul is ONE kernel", kernels(matmul(x, transpose(y))).length === 1);
  check("matvec + bias + silu epilogue is ONE kernel", kernels(silu(add(matmul(buffer("h", [1, 32]), buffer("Wg", [32, 16])), buffer("b", [16])))).length === 1);
  { // small inputs share a binding: twelve small tables over a picture are one kernel, read
    // from two blocks of them, not the device's limit of bindings split into many kernels
    const P = buffer("P", [64, 96]), tables = Array.from({ length: 12 }, (_, i) => data(rv(96, i + 1), [96]));
    const g = tables.reduce<UOp>((s, t, i) => add(s, mul(i % 2 ? exp(mul(P, konst(0.01 * i))) : P, broadcastTo(t, [64, 96]))), konst(0));
    const ks = kernels(g), top = ks.at(-1)!;
    check("twelve small inputs: ONE kernel reading P and two blocks", ks.length === 3 && top.src.length - 2 === 3, `${ks.length} kernels`);
    const at = new Map<string, HostData>([["P", T(rv(64 * 96), [64, 96])]]);
    const a = await realize(cpu, g, at), b = gpu ? await realize(gpu, g, at) : a;
    check("parity: twelve small inputs", rel(b.data, a.data) < 1e-5, `relΔ ${rel(b.data, a.data).toExponential(1)}`);
  }
  { // one value per (node, index): a fork of pixels × pieces read three times at one index is
    // computed once, in the kernel that sums it – never stored
    const P = 4096, E = 64, px = buffer("px", [P, 1]), ed = buffer("ed", [1, E]);
    const a = add(mul(exp(add(broadcastTo(px, [P, E]), mul(broadcastTo(ed, [P, E]), konst(-1)))), konst(0.5)), konst(1));
    const g = reduceAxis(add(mul(a, a), sqrt(a)), 1, false), ks = kernels(g);
    check("a fork of pixels × pieces read at one index is computed in the kernel that sums it: ONE kernel", ks.length === 1, `${ks.length} kernels, the largest [${ks.map((k) => k.shape).sort((x, y) => numel(y) - numel(x))[0]}]`);
    const at = new Map<string, HostData>([["px", T(rv(P, 2), [P, 1])], ["ed", T(rv(E, 2), [1, E])]]);
    const c = await realize(cpu, g, at), d = gpu ? await realize(gpu, g, at) : c;
    check("parity: a fork of pixels × pieces", rel(d.data, c.data) < 1e-5);
    // read two ways that take each element once – the two coordinates of a cell's corners –
    // it is computed where each way reads it
    const pts = buffer("pts", [P, 1, 2]), corners = data(Float32Array.of(0, 0, 1, 0, 0, 1, 1, 1), [4, 2]);
    const cell = add(exp(mul(broadcastTo(pts, [P, 4, 2]), konst(0.3))), broadcastTo(corners, [P, 4, 2]));
    const mix = (u: UOp) => mul(add(mul(u, konst(34)), konst(1)), u);
    const coords = (j: number) => reshape(slice(cell, 2, j, 1), [P, 4]);
    const n2 = reduceAxis(add(mix(coords(0)), mix(coords(1))), 1, false), kn = kernels(n2);
    check("a fork read as two coordinates is computed where each is read: ONE kernel", kn.length === 1, `${kn.length} kernels`);
  }
  { // two strokes of one distance, one above the other, over a picture's three colors: the
    // loop to the nearest of 8 points under both covers is computed once, in a kernel of its
    // own, and each cover where the picture reads it – not the loop in a kernel of each cover
    const H = 40, W = 96, K = 8, D = reshape(buffer("D", [1]), []), pts = buffer("pts", [K, 2]);
    const min = (a: UOp, b: UOp) => where(cmplt(b, a), b, a), max = (a: UOp, b: UOp) => where(cmplt(a, b), b, a);
    const at = (n: number, s: number[]) => reshape(broadcastTo(reshape(add(iota(n), konst(0.5)), s), [H, W]), [H, W, 1]);
    const p = reshape(math("div", add(pad(at(W, [1, W]), 2, 0, 1), pad(at(H, [H, 1]), 2, 1, 0)), broadcastTo(D, [H, W, 2])), [H, W, 1, 2]);
    const diff = sub(broadcastTo(p, [H, W, K, 2]), broadcastTo(pts, [H, W, K, 2]));
    const e = mul(mul(reduceMax(mul(sqrt(reduceAxis(mul(diff, diff), 3, false)), konst(-1)), 2, true), konst(-1)), D);
    const cover = (r: string) => { const R = reshape(buffer(r, [1]), []); return min(max(sub(min(add(e, konst(0.5)), R), max(add(e, konst(-0.5)), mul(R, konst(-1)))), konst(0)), konst(1)); };
    const top = cover("r1"), under = cover("r2"), colors = (c: string, x: UOp) => mul(broadcastTo(buffer(c, [3]), [H, W, 3]), broadcastTo(x, [H, W, 3]));
    const g = add(mul(buffer("pic", [H, W, 3]), broadcastTo(sub(konst(1), add(mul(under, sub(konst(1), top)), top)), [H, W, 3])), add(mul(colors("dark", under), broadcastTo(sub(konst(1), top), [H, W, 3])), colors("light", top)));
    const ks = kernels(g);
    check("two strokes of one distance compute its loop once: TWO kernels", ks.length === 2 && ks.filter((k) => accCost(k.src[1]!) === K).length === 1, `${ks.length} kernels, loops ${ks.map((k) => accCost(k.src[1]!)).join(", ")}`);
    const at2 = new Map<string, HostData>([["D", T(Float32Array.of(2), [1])], ["pts", T(rv(K * 2, 40), [K, 2])], ["r1", T(Float32Array.of(7), [1])], ["r2", T(Float32Array.of(11), [1])],
      ["pic", T(rv(H * W * 3), [H, W, 3])], ["dark", T(rv(3), [3])], ["light", T(rv(3), [3])]]);
    const c = await realize(cpu, g, at2), d = gpu ? await realize(gpu, g, at2) : c;
    check("parity: two strokes of one distance", rel(d.data, c.data) < 1e-5, `relΔ ${rel(d.data, c.data).toExponential(1)}`);
  }
  { // a chain of sums over a body with a loop of its own folds into its reader only within
    // COST.perThread steps a thread: 3 outputs, each a sum over four axes of 8 of a product
    // with a sum over a fifth, is not one 32,768-step loop on each of 3 threads
    const Y = buffer("Y", [3, 8, 8, 8, 8, 8]), X = buffer("X", [3, 8, 8, 8, 8]);
    let r: UOp = mul(reduceAxis(Y, 5, false), X);
    for (let ax = 4; ax >= 1; ax--) r = reduceAxis(r, ax, false);
    const per = kernels(r).map((k) => accCost(k.src[1]!));
    check("chained sums: no thread runs past the per-element budget", Math.max(...per) <= COST.perElem, `steps a thread: ${per.join(", ")}`);
    const at = new Map<string, HostData>([["Y", T(rv(3 * 8 ** 5), [3, 8, 8, 8, 8, 8])], ["X", T(rv(3 * 8 ** 4), [3, 8, 8, 8, 8])]]);
    const c = await realize(cpu, r, at), d = gpu ? await realize(gpu, r, at) : c;
    check("parity: chained sums", rel(d.data, c.data) < 1e-4, `relΔ ${rel(d.data, c.data).toExponential(1)}`);
  }
  { // butterflies: each stage reads the one before at two indices, so computed in place the
    // copies double at each stage down. The plan stores a stage once its copies cost more than
    // storing it, so no stage recomputes more than storing costs: 12 stages of 4096 points
    const Nf = 4096, H = Nf / 2, S = 12;
    let f: UOp = buffer("f", [Nf]);
    for (let s = 0; s < S; s++) {
      const a = slice(f, 0, 0, H), b = mul(slice(f, 0, H, H), data(Float32Array.from({ length: H }, (_, i) => Math.cos(i + s)), [H]));
      f = add(pad(add(a, b), 0, 0, H), pad(sub(a, b), 0, H, 0));
    }
    const body = kernels(f).reduce((n, k) => n + toposort(k.src[1]!).length, 0);
    check("butterflies: no stage recomputes more than storing it costs", body <= S * COST.recompute, `${body} ops in the plan's kernels`);
    const at = new Map<string, HostData>([["f", T(Float32Array.from({ length: Nf }, (_, i) => Math.sin(i)), [Nf])]]);
    const c = await realize(cpu, f, at), d = gpu ? await realize(gpu, f, at) : c;
    check("parity: butterflies", rel(d.data, c.data) < 1e-4, `relΔ ${rel(d.data, c.data).toExponential(1)}`);
  }
  { // a kernel's body is a graph: a value it reads twice is one value, and printing walks it
    // once – a term squared 30 times is 30 nodes, not 2³⁰ paths. Both shapes a reduction can
    // print as: a contraction's tile, and a long sum's cooperative workgroup
    const square = (t: UOp, n: number): UOp => { for (let i = 0; i < n; i++) t = mul(t, t); return t; };
    const A = buffer("A", [32, 128, 1]), B = buffer("B", [1, 128, 32]), Xr = buffer("Xr", [4, 4096]);
    const tile = reduceAxis(square(mul(broadcastTo(A, [32, 128, 32]), broadcastTo(B, [32, 128, 32])), 22), 1, false), coop = reduceAxis(square(Xr, 30), 1, false);
    for (const [name, g] of [["a contraction's tile", tile], ["a long sum", coop]] as const) {
      const t0 = performance.now();
      lowerMany([g]);
      const ms = performance.now() - t0;
      check(`${name} over a term squared again and again is printed in time linear in its nodes`, ms < 500, `${ms.toFixed(0)} ms`);
    }
  }
  { // a gather is a read: never stored only to be read back
    const W = buffer("W", [6, 4]), ids = buffer("ids", [5]), g = gather(W, ids);
    check("a gather read twice is ONE kernel", kernels(add(mul(g, g), exp(g))).length === 1);
    check("a gather of a gather is ONE kernel", kernels(exp(gather(g, buffer("j", [3])))).length === 1);
    check("a gather of a table it takes about once computes the table in place: ONE kernel", kernels(gather(exp(W), ids)).length === 1);
    check("a gather of a table it takes many times reads it stored: TWO kernels", kernels(gather(exp(W), buffer("many", [64]))).length === 2);
    check("a gather of one row named in the graph is a view of the row", !toposort(simplifyMany([gather(W, reshape(konst(4), [1]))])).some((u) => u.op === "GATHER"));
  }
  {
    const fat = wgslOf(matmul(buffer("A", [128, 64]), buffer("B", [64, 128])));   // a grid that fills the GPU, K past COST.tileMinK
    const thin = wgslOf(matmul(buffer("h", [1, 1024]), buffer("W", [1024, 512]))), rows = wgslOf(matmul(buffer("h", [1, 1024]), permute(buffer("W", [512, 1024]), [1, 0])));
    check("fat-M matmul is TILED (As/Bs tiles)", fat.length === 1 && fat[0]!.includes("var<workgroup> As"));
    check("M=1 matvec is NOT tiled: a thread an output where they read side by side, a workgroup an output where each reads a row", thin.length === 1 && !thin[0]!.includes("var<workgroup> As") && !thin[0]!.includes("var<workgroup> part") && rows.length === 1 && rows[0]!.includes("var<workgroup> part") && rows[0]!.includes("workgroup_id"));
  }
  { // a tile's term is any function of its two reads: ∇'s mul0 products, and exp(a·b), which is not 0 where the staged reads are 0 past K
    const Xb = buffer("X", [256, 128]), Wb = buffer("W", [128, 64]), y = matmul(Xb, Wb), P = buffer("P", [128, 72]), Q = buffer("Q", [72, 96]);
    const [dW, dX] = gradAll(sum(mul(y, y)), [Wb, Xb]) as [UOp, UOp];
    const e = reduceAxis(exp(mul(broadcastTo(reshape(P, [128, 72, 1]), [128, 72, 96]), broadcastTo(reshape(Q, [1, 72, 96]), [128, 72, 96]))), 1, false);
    const at = new Map<string, HostData>([["X", T(rv(256 * 128), [256, 128])], ["W", T(rv(128 * 64), [128, 64])], ["P", T(rv(128 * 72, 0.3), [128, 72])], ["Q", T(rv(72 * 96, 0.3), [72, 96])]]);
    const tiles = (g: UOp) => wgslOf(g).some((w) => w.includes("var<workgroup> As"));
    check("∇W, ∇X and Σ exp(a·b) tile", tiles(dW) && tiles(dX) && tiles(e));
    if (gpu) for (const [nm, g] of [["∇W", dW], ["∇X", dX], ["Σ exp(a·b), K % 16 ≠ 0", e]] as const) {
      const a = await realize(cpu, g, at), b = await realize(gpu, g, at);
      check(`parity: tiled ${nm}`, rel(b.data, a.data) < 1e-5, `relΔ ${rel(b.data, a.data).toExponential(1)}`);
    }
  }
  if (gpu) { // two inputs read at the same place are two reads: Σₖ A·C·B reads A and C at (row, k)
    const g = matmul(mul(buffer("A", [128, 64]), buffer("C", [128, 64])), buffer("B", [64, 128]));
    const at = new Map<string, HostData>([["A", T(rv(128 * 64), [128, 64])], ["C", T(rv(128 * 64), [128, 64])], ["B", T(rv(64 * 128), [64, 128])]]);
    const a = await realize(cpu, g, at), b = await realize(gpu, g, at);
    check("parity: a contraction reading two inputs at one place", rel(b.data, a.data) < 1e-5, `relΔ ${rel(b.data, a.data).toExponential(1)}`);
  }
  if (gpu) { // the synchronous read, through canvases: a whole canvas, whole rows and a partial row
    const n = 256 * 256 + 17 * 256 + 112, d = Float32Array.from({ length: n }, (_, i) => Math.sin(i) * 1e3);
    d[5] = NaN; d[6] = -Infinity; d[7] = -0;
    const src = gpu.device.createBuffer({ size: n * 4, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC | GPUBufferUsage.COPY_DST });
    gpu.device.queue.writeBuffer(src, 0, d);
    const got = gpu.readSync(src, n);
    check("readSync returns the buffer's bits", got.every((v, i) => Object.is(v, d[i]) || (Number.isNaN(v) && Number.isNaN(d[i]!))));
    src.destroy();
  }
  if (gpu) { // a tile grid past 32,768 workgroups folds into two axes, the last workgroups idle
    const m = 8208, k = 64, n = 1024, Ad = rv(m * k), Bd = rv(k * n);
    const out = (await realize(gpu, matmul(buffer("A", [m, k]), buffer("B", [k, n])), new Map([["A", T(Ad, [m, k])], ["B", T(Bd, [k, n])]]))).data;
    let worst = 0;
    for (const i of [0, 1, 4095, 8191, 8192, 8200, m - 1]) for (const j of [0, 17, n - 1]) {
      let s = 0; for (let q = 0; q < k; q++) s += Ad[i * k + q]! * Bd[q * n + j]!;
      worst = Math.max(worst, Math.abs(out[i * n + j]! - s));
    }
    check("a folded tile grid computes every row", worst < 1e-4, `worst ${worst.toExponential(1)}`);
  }
  { // the same program built twice prints the same WGSL — the browser's shader cache is keyed by text
    const prog = () => {
      const h = buffer("h", [4, 64]), W = buffer("W", [64, 128]), A = buffer("A", [128, 64]), B = buffer("B", [64, 128]);
      return [rmsNorm(h, buffer("g", [64])), softmaxRow(matmul(h, W)), matmul(A, B), silu(matmul(buffer("x", [1, 1024]), buffer("V", [1024, 64])))];
    };
    const text = () => prog().flatMap(wgslOf).join("\n----\n");
    const t1 = text(), t2 = text();
    check("WGSL is deterministic across rebuilds", t1 === t2 && t1.length > 0, `${t1.length} chars`);
  }
  { // a weight gradient feeding Adam's three updates is contracted once, not once per update
    const B = 4096, X = buffer("X", [B, 32]), W = buffer("W", [32, 16]), y = matmul(X, W);
    const g = gradAll(add(sum(mul(y, y)), sum(mul(W, W))), [W])[0]!;
    const a = adamStep(W, g, buffer("m", [32, 16]), buffer("v", [32, 16]), buffer("bc1", []), buffer("bc2", []), buffer("lr", []));
    const seen = new Set<number>(); let n = 0;
    for (const r of optimizeMany([a.p, a.m, a.v])) for (const u of toposort(r)) { if (seen.has(u.id)) continue; seen.add(u.id); if (u.op === "ACC" && (u.src[0]!.arg as { extent: number }).extent === B) n++; }
    check("∇W shared by Adam's 3 updates is contracted ONCE", n === 1, `${n} contractions`);
  }
  {
    const q = matmul(buffer("h", [1, 64]), buffer("Wqkv", [64, 48]));
    const roots = [slice(q, 1, 0, 16), slice(q, 1, 16, 16), slice(q, 1, 32, 16)].map((p) => mul(p, konst(2)));
    const seen = new Set<number>(); let n = 0;
    for (const r of roots.map((r) => optimize(r))) for (const u of toposort(r)) if (u.op === "FUSED" && !seen.has(u.id)) { seen.add(u.id); n++; }
    check("QKV slices: shared projection + 3 maps, no copies", n <= 4, `${n} kernels`);
  }

  // ── 3. ∇ vs finite differences (random graphs) + higher order ────────────────────
  const safePos = (a: UOp) => add(konst(1.3), mul(a, a));
  function randGraph(t: UOp, d: number): UOp {
    if (d <= 0 || rnd() < 0.3) return t;
    const r = rnd(), a = () => randGraph(t, d - 1);
    if (r < 0.12) return add(a(), konst(rnd() * 2 - 1));
    if (r < 0.24) return mul(a(), konst(rnd() * 2 - 1));
    if (r < 0.36) { const q = a(); return mul(q, q); }
    if (r < 0.46) return exp(mul(konst(0.4), a()));
    if (r < 0.56) return recip(safePos(a()));
    if (r < 0.64) return sqrt(safePos(a()));
    if (r < 0.74) return relu(a());
    return add(a(), a());
  }
  {
    let worst = 0, worstG = 0;
    for (let t = 0; t < 14; t++) {
      const p = buffer("p", [4]);
      const lossG = sum(mul(randGraph(p, 4), p));
      const P0 = rv(4);
      const i4 = () => new Map<string, HostData>([["p", T(P0.slice(), [4])]]);
      const g = (await realize(cpu, grad(lossG, p), i4())).data;
      const eps = 1e-3;
      for (let j = 0; j < 4; j++) {
        const up = P0.slice(); up[j]! += eps;
        const dn = P0.slice(); dn[j]! -= eps;
        const fd = ((await realize(cpu, lossG, new Map([["p", T(up, [4])]]))).data[0]! - (await realize(cpu, lossG, new Map([["p", T(dn, [4])]]))).data[0]!) / (2 * eps);
        worst = Math.max(worst, Math.abs(g[j]! - fd) / (1 + Math.abs(fd)));
      }
      if (gpu) { const gg = (await realize(gpu, grad(lossG, p), i4())).data; worstG = Math.max(worstG, rel(gg, g)); }
    }
    check("∇ == finite-diff on 14 random graphs", worst < 3e-2, `worst ${worst.toExponential(1)}`);
    if (gpu) check("∇ cpu == gpu", worstG < 1e-3, `worst ${worstG.toExponential(1)}`);
  }
  { // view-algebra grads: matmul (W forked into BOTH operands) / slice+reduce / gather
    const Wd = rv(12), Id = new Float32Array([1, 3, 1]);
    for (const [nm, lossF] of [
      ["matmul (fork)", (W: UOp) => sum(mul(matmul(W, transpose(W)), konst(0.5)))],
      ["slice+reduce", (W: UOp) => sum(mul(slice(W, 0, 1, 2), broadcastTo(reduceAxis(W, 0, false), [2, 3])))],
      ["gather", (W: UOp) => sum(mul(gather(W, buffer("ids", [3])), konst(2)))],
    ] as const) {
      const W = buffer("W", [4, 3]), loss = lossF(W);
      const at = (d: Float32Array) => new Map<string, HostData>([["W", T(d, [4, 3])], ["ids", T(Id, [3])]]);
      const g = (await realize(cpu, grad(loss, W), at(Wd.slice()))).data;
      const eps = 1e-3, fd = new Float32Array(12);
      for (let j = 0; j < 12; j++) {
        const up = Wd.slice(); up[j]! += eps; const dn = Wd.slice(); dn[j]! -= eps;
        fd[j] = ((await realize(cpu, loss, at(up))).data[0]! - (await realize(cpu, loss, at(dn))).data[0]!) / (2 * eps);
      }
      let w = 0; for (let j = 0; j < 12; j++) w = Math.max(w, Math.abs(g[j]! - fd[j]!) / (1 + Math.abs(fd[j]!)));
      check(`∇ ${nm} == finite-diff`, w < 3e-2, `worst ${w.toExponential(1)}`);
      if (gpu) { const gg = (await realize(gpu, grad(loss, W), at(Wd.slice()))).data; check(`∇ ${nm} cpu == gpu`, rel(gg, g) < 1e-3); }
    }
  }
  { // ∇ of a minimum flows through its winner: P pixels, each nearest to one of K points. The
    // derivative is taken at each pixel's winner alone – never at every (pixel, point) pair –
    // and it is the exact one, on the CPU and on the GPU, where recomputed distances differ in
    // their last bits from the minimum's
    const P = 6000, K = 37, px = buffer("px", [P, 2]), th = buffer("th", [K, 2]), w = buffer("w", [P]);
    const pair = add(broadcastTo(reshape(px, [P, 1, 2]), [P, K, 2]), mul(broadcastTo(reshape(th, [1, K, 2]), [P, K, 2]), konst(-1)));
    const d = reshape(sqrt(add(reduceAxis(mul(pair, pair), 2, true), konst(0.25))), [P, K]);
    const loss = sum(mul(mul(reduceMax(mul(d, konst(-1)), 1, false), konst(-1)), w));
    const g = grad(loss, th);
    const Pd = rv(P * 2, 50), Td = rv(K * 2, 50), Wd = rv(P);
    const at = new Map<string, HostData>([["px", T(Pd, [P, 2])], ["th", T(Td, [K, 2])], ["w", T(Wd, [P])]]);
    const want = new Float64Array(K * 2);
    for (let p = 0; p < P; p++) {
      let best = Infinity, k = 0;
      for (let j = 0; j < K; j++) { const dx = Pd[2 * p]! - Td[2 * j]!, dy = Pd[2 * p + 1]! - Td[2 * j + 1]!, e = Math.sqrt(dx * dx + dy * dy + 0.25); if (e < best) { best = e; k = j; } }
      for (let c = 0; c < 2; c++) want[2 * k + c]! += Wd[p]! * (Td[2 * k + c]! - Pd[2 * p + c]!) / best;
    }
    const a = (await realize(cpu, g, at)).data, b = gpu ? (await realize(gpu, g, at)).data : a;
    check("∇ of a minimum, through its winner: CPU and GPU = exact", rel(a, want) < 1e-4 && rel(b, want) < 1e-4, `relΔ ${rel(a, want).toExponential(1)} / ${rel(b, want).toExponential(1)}`);
    let worst = 0;
    for (const k of toposort(optimize(g)).filter((u) => u.op === "FUSED"))
      for (const u of toposort(k.src[1]!)) if (u.op === "RECIPROCAL") worst = Math.max(worst, [...rangesIn(u)].filter((r) => r !== k.src[0]).reduce((n, r) => n * (r.arg as { extent: number }).extent, numel(k.shape)));
    check("∇ of a minimum: the derivative is taken at each pixel's coordinates, not at every pair", worst <= 2 * P, `${worst} evaluations of the distance's slope`);
  }
  { // higher order: f = x³ → ∇²=6x, ∇³=6
    const s = buffer("s", []);
    const cube = mul(s, mul(s, s));
    const d2 = grad(grad(cube, s), s), d3 = grad(grad(grad(cube, s), s), s);
    const at = new Map([["s", T(new Float32Array([1.5]), [])]]);
    check("∇² x³ = 6x", Math.abs((await realize(cpu, d2, at)).data[0]! - 9) < 1e-4);
    check("∇³ x³ = 6", Math.abs((await realize(cpu, d3, at)).data[0]! - 6) < 1e-4);
  }

  // ── 4. training smoke: linear regression via graph-Adam, each step's state fed back in ──
  for (const be of [cpu, ...(gpu ? [gpu] : [])]) {
    const K = 2, NS = 8;
    const Xd = rv(NS * K), Yd = new Float32Array(NS);
    for (let i = 0; i < NS; i++) Yd[i] = 2 * Xd[i * K]! - 1 * Xd[i * K + 1]!;
    const w = buffer("w", [K, 1]), Xb = buffer("X", [NS, K]), Yb = buffer("Y", [NS, 1]);
    const d = sub(matmul(Xb, w), Yb);
    const loss = mul(sum(mul(d, d)), konst(1 / NS));
    const g = gradAll(loss, [w])[0]!;
    const bc1 = buffer("bc1", []), bc2 = buffer("bc2", []), lr = buffer("lr", []);
    const a = adamStep(w, g, buffer("m_w", [K, 1]), buffer("v_w", [K, 1]), bc1, bc2, lr);
    const state = new Map<string, HostData>([["w", T(new Float32Array(K), [K, 1])], ["m_w", T(new Float32Array(K), [K, 1])], ["v_w", T(new Float32Array(K), [K, 1])]]);
    let l0 = 0, lN = 0;
    for (let it = 1; it <= 200; it++) {
      const inputs = new Map<string, HostData>([...state, ["X", T(Xd, [NS, K])], ["Y", T(Yd, [NS, 1])],
        ["bc1", T(new Float32Array([1 / (1 - 0.9 ** it)]), [])], ["bc2", T(new Float32Array([1 / (1 - 0.999 ** it)]), [])], ["lr", T(new Float32Array([0.05]), [])]]);
      const [p, m, v, l] = await realizeMany(be, [a.p, a.m, a.v, loss], inputs);
      state.set("w", p!); state.set("m_w", m!); state.set("v_w", v!);
      if (it === 1) l0 = l!.data[0]!;
      if (it === 200) lN = l!.data[0]!;
    }
    const wf = state.get("w")!.data;
    check(`train [${be.name}]: loss ${l0.toFixed(3)} → ${lN.toExponential(1)}, w → [${wf[0]!.toFixed(2)}, ${wf[1]!.toFixed(2)}]`, lN < l0 * 1e-3 && Math.abs(wf[0]! - 2) < 0.05 && Math.abs(wf[1]! + 1) < 0.05);
  }

  // ── 5. the algebra says hello ─────────────────────────────────────────────────────
  check("provablyEq: x·(2·3) ≡ 6·x", provablyEq((t) => mul(t, mul(konst(2), konst(3))), (t) => mul(t, konst(6)), [[M, N]]));
  check("provablyEq: x·1+(−0) ≡ x", provablyEq((t) => add(mul(t, konst(1)), konst(-0)), (t) => t, [[M, N]]));
  check("provablyEq: x+0 ≢ x (negative: −0 + 0 is 0)", !provablyEq((t) => add(t, konst(0)), (t) => t, [[M, N]]));
  check("provablyEq: rmsNorm ≡ rmsNorm∘reshape", provablyEq((t, w) => rmsNorm(t, w), (t, w) => rmsNorm(reshape(t, [M, N]), w), [[M, N], [N]]));
  check("provablyEq: x·2 ≢ x·3 (negative)", !provablyEq((t) => mul(t, konst(2)), (t) => mul(t, konst(3)), [[M, N]]));

  return { fails };
}
