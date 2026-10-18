// Re-derives the constants that pick a reduction's form on the GPU: one reduction, timed as
// the plain nest (a thread per output) and as the coop (a workgroup per output, in K-groups
// where the outputs are few), and the split-K tile at each split. It fails when a
// measurement and a constant disagree. GPU times this short vary by about 20% run to run.
//   CHROME_ARGS=--enable-webgpu-developer-features bun engine/tests/run.ts engine/tests/bench-reduce.ts
import { buffer, reduceAxis, type UOp } from "../ir.ts";
import { cellMatmul, engineGpu, init } from "../tensor.ts";
import { GPU } from "../gpu.ts";
import { COST } from "../opt.ts";

export default async function (): Promise<{ fails: number }> {
  await init();
  const dev = engineGpu()?.device;
  if (!dev?.features.has("timestamp-query")) { console.log("no GPU timestamps"); return { fails: 1 }; }
  const given = { ...COST };
  // the median GPU time of the root's kernels, with the constants changed (a fresh GPU, so no plan is shared)
  const time = async (root: UOp, bufs: Map<string, GPUBuffer>, knobs: Partial<typeof COST> = {}): Promise<number> => {
    Object.assign(COST, knobs);
    const gpu = new GPU(dev, true), ms: number[] = [];
    for (let i = 0; i < 21; i++) { gpu.profile = []; for (const o of gpu.computeSync([root], (n) => bufs.get(n))) gpu.release(o); ms.push((await gpu.profiled()).reduce((t, r) => t + r.ms, 0)); }
    Object.assign(COST, given);
    return ms.sort((x, y) => x - y)[10]!;
  };
  const data = (n: number) => { const g = dev.createBuffer({ size: n * 4, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST }); dev.queue.writeBuffer(g, 0, Float32Array.from({ length: n }, Math.random)); return g; };
  // x [K, N] reduced over K: neighbouring outputs are neighbours in memory; in rows, x [N, K], each output's K is
  const reduction = <R>(K: number, N: number, op: "ADD" | "MAX", rows: boolean, f: (root: UOp, bufs: Map<string, GPUBuffer>) => Promise<R>): Promise<R> => {
    const bufs = new Map([["x", data(K * N)]]), root = rows ? reduceAxis(buffer("x", [N, K]), 1, false, op) : reduceAxis(buffer("x", [K, N]), 0, false, op);
    return f(root, bufs).finally(() => bufs.get("x")!.destroy());
  };
  const plain = { groupSteps: Infinity }, coop = { coopFew: Infinity }, WG = 128;
  let fails = 0;

  // coopFew: the least number of outputs, side by side in memory, from which on the plain nest is no slower
  console.log("outputs side by side, K = 4096, median GPU ms: plain / coop");
  let few = Infinity, streak = true;
  for (const N of [1024, 512, 256, 128, 64, 32]) {
    const [p, c] = await reduction(4096, N, "ADD", false, async (r, b) => [await time(r, b, plain), await time(r, b, coop)]);
    streak &&= p <= c;
    if (streak) few = N;
    console.log(`  N = ${String(N).padStart(4)}  ${p.toFixed(4)} / ${c.toFixed(4)}`);
  }
  console.log(`derived coopFew = ${few}; COST.coopFew = ${COST.coopFew}`);
  if (few !== COST.coopFew) fails++;

  // groupSteps: from K = 128·groupSteps on, the coop is the faster for few outputs, and no slower in rows
  const K0 = WG * COST.groupSteps;
  console.log(`K = 128 · groupSteps = ${K0}, median GPU ms: plain / coop`);
  for (const [N, rows] of [[1, false], [3, false], [256, true], [1024, true]] as const) {
    const [p, c] = await reduction(K0, N, "ADD", rows, async (r, b) => [await time(r, b, plain), await time(r, b, coop)]);
    const ok = rows ? c <= 1.25 * p : c < 0.9 * p;
    if (!ok) fails++;
    console.log(`  N = ${String(N).padStart(4)}${rows ? " in rows" : "        "}  ${p.toFixed(4)} / ${c.toFixed(4)}  ${ok ? "" : "– the constant is off"}`);
  }

  // fill and groupSteps: the split they pick runs within 25% of the best split – few outputs, and the tile
  const split = async (name: string, root: UOp, bufs: Map<string, GPUBuffer>, units: number) => {
    const row: string[] = [], first = await time(root, bufs);
    let least = Infinity, at = 0;
    for (const G of [1, 4, 16, 32, 64, 128, 256]) {
      const t = await time(root, bufs, { fill: G * units, groupSteps: 1 });
      row.push(`${G}: ${t.toFixed(3)}`);
      if (t < least) { least = t; at = G; }
    }
    const ours = Math.min(first, await time(root, bufs)), ok = ours <= 1.25 * least;   // timed before and after the sweep: the GPU's clock drifts
    if (!ok) fails++;
    console.log(`  ${name.padEnd(26)} ${row.join("  ")}   best G = ${at}; the constants' ${ours.toFixed(3)}${ok ? "" : " – off the plateau"}`);
  };
  console.log("K-groups: G → median GPU ms");
  for (const [K, N, op] of [[1 << 20, 1, "MAX"], [1 << 20, 3, "MAX"], [1 << 18, 1, "ADD"], [1 << 16, 1, "ADD"], [1 << 16, 3, "ADD"]] as const)
    await reduction(K, N, op, false, (r, b) => split(`${op} K = ${K}, N = ${N}`, r, b, N));
  { // a weight gradient: [32, K]·[K, 96] is 12 tiles
    const K = 5344, bufs = new Map([["a", data(32 * K)], ["b", data(K * 96)]]);
    await split(`tile [32,${K}]·[${K},96]`, cellMatmul(buffer("a", [32, K]), buffer("b", [K, 96])), bufs, 12);
    for (const g of bufs.values()) g.destroy();
  }
  return { fails };
}
