// Re-derives the two constants that bound fusion: COST.recompute, what a node's store and
// first read cost in ops – a node read at `a` indices is recomputed while work·(a − 1) stays
// under it, plus a – and COST.perThread, the most steps a chain of sums folded into one
// thread may take. Each form is timed on the GPU both ways; it fails when a derived value and
// its constant are more than a factor of 2 apart.
//   CHROME_ARGS=--enable-webgpu-developer-features bun engine/tests/run.ts engine/tests/bench-fuse.ts
import { buffer, add, mul, konst, slice, reduceAxis, toposort, type UOp } from "../ir.ts";
import { engineGpu, init } from "../tensor.ts";
import { GPU } from "../gpu.ts";
import { COST, optimize, accCost } from "../opt.ts";

export default async function (): Promise<{ fails: number }> {
  await init();
  const dev = engineGpu()?.device;
  if (!dev?.features.has("timestamp-query")) { console.log("no GPU timestamps"); return { fails: 1 }; }
  const given = { ...COST };
  // the median GPU time of the root's kernels, with the constants changed (a fresh GPU, so no plan is shared)
  const time = async (root: UOp, bufs: Map<string, GPUBuffer>, knobs: Partial<typeof COST>): Promise<number> => {
    Object.assign(COST, knobs);
    const gpu = new GPU(dev, true), ms: number[] = [];
    for (let i = 0; i < 15; i++) { gpu.profile = []; for (const o of gpu.computeSync([root], (n) => bufs.get(n))) gpu.release(o); ms.push((await gpu.profiled()).reduce((t, r) => t + r.ms, 0)); }
    Object.assign(COST, given);
    return ms.sort((x, y) => x - y)[7]!;
  };
  const data = (n: number) => { const g = dev.createBuffer({ size: n * 4, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST }); dev.queue.writeBuffer(g, 0, Float32Array.from({ length: n }, Math.random)); return g; };
  let fails = 0;

  // recompute: v, `w` ops on each of N elements, read at `a` neighbouring indices; stored, or
  // computed at each. The crossover work w* gives recompute = w*·(a − 1) − a
  const N = 1 << 20, bufs = new Map([["x", data(N + 8)]]), x = buffer("x", [N + 8]);
  const derived: number[] = [];
  console.log(`v of w ops over ${N} elements, read at a indices, median GPU ms: stored / recomputed`);
  for (const a of [2, 4]) {
    let crossed = Infinity;
    for (const w of [12, 48, 192, 768, 3072]) {
      let v: UOp = x;
      for (let i = 0; i < w / 3; i++) v = add(mul(mul(v, v), konst(0.5 + i * 1e-6)), konst(0.25));   // squares: no reassociation folds them
      let out: UOp = slice(v, 0, 0, N);
      for (let j = 1; j < a; j++) out = add(out, slice(v, 0, j, N));
      const [s, r] = [await time(out, bufs, { recompute: -Infinity }), await time(out, bufs, { recompute: Infinity })];
      console.log(`  a = ${a}, w = ${String(w).padStart(4)}  ${s.toFixed(3)} / ${r.toFixed(3)}`);
      if (s < r && crossed === Infinity) crossed = w;
    }
    if (crossed < Infinity) derived.push(crossed * (a - 1) - a);
  }
  const recompute = derived.length ? derived.reduce((p, q) => p + q, 0) / derived.length : Infinity;
  console.log(`derived recompute ≈ ${recompute.toFixed(0)}; COST.recompute = ${COST.recompute}`);
  if (!(recompute <= 2 * COST.recompute && recompute >= COST.recompute / 2)) fails++;

  // perThread: B outputs, each a sum over d axes of 8 of a product with a sum over one more;
  // folded into one thread an output, or the chain stored where it reaches perThread steps
  let fold = 0, streak = true;
  for (const B of [3, 384]) for (const d of [1, 2, 3, 4]) {
    const Y = buffer("Y", [B, ...Array(d + 1).fill(8)]), X = buffer("X", [B, ...Array(d).fill(8)]);
    let r: UOp = mul(reduceAxis(Y, d + 1, false), X);
    for (let ax = d; ax >= 1; ax--) r = reduceAxis(r, ax, false);
    const io = new Map([["Y", data(B * 8 ** (d + 1))], ["X", data(B * 8 ** d)]]);
    const [f, s] = [await time(r, io, { perThread: Infinity }), await time(r, io, { perThread: 8 })];
    const shape = (knobs: Partial<typeof COST>) => { Object.assign(COST, knobs); const ks = toposort(optimize(r)).filter((u) => u.op === "FUSED").map((k) => accCost(k.src[1]!)); Object.assign(COST, given); return ks.join("+"); };
    console.log(`  ${B} outputs, 8^${d + 1} = ${String(8 ** (d + 1)).padStart(6)} steps, median GPU ms folded / split  ${f.toFixed(3)} / ${s.toFixed(3)}   steps a thread ${shape({ perThread: Infinity })} / ${shape({ perThread: 8 })}`);
    streak &&= B !== 3 || f <= 1.1 * s;
    if (B === 3 && streak) fold = 8 ** (d + 1);
    for (const g of io.values()) g.destroy();
  }
  console.log(`derived perThread = ${fold}: the most steps for which folded is within 10% of split; COST.perThread = ${COST.perThread}`);
  if (!(fold <= 2 * COST.perThread && fold >= COST.perThread / 2)) fails++;
  for (const g of bufs.values()) g.destroy();
  return { fails };
}
