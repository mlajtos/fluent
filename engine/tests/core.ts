// Core gates: CPU↔GPU parity per op family, fusion counts (the schedule's shape), ∇ vs
// finite differences on random graphs, and a training smoke. Runs in headless Chrome:
//   bun engine/tests/run.ts

import { buffer, konst, add, mul, recip, exp, sqrt, where, cmplt, reshape, permute, transpose, broadcastTo, slice, reduceAxis, reduceMax, sum, gather, scatterAdd, toposort, type UOp } from "../ir.ts";
import { optimize, optimizeMany } from "../opt.ts";
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
