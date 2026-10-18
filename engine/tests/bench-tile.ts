// Re-derives COST.tileMinK, the K below which a contraction runs as a plain loop nest rather
// than a tile: one matmul, both forms, timed on the GPU at each K, with B read as stored and
// read transposed (∇'s form). It fails when the measurement and the constant disagree.
//   CHROME_ARGS=--enable-webgpu-developer-features bun engine/tests/run.ts engine/tests/bench-tile.ts
import { buffer, permute } from "../ir.ts";
import { cellMatmul, engineGpu, init } from "../tensor.ts";
import { GPU } from "../gpu.ts";
import { COST } from "../opt.ts";

export default async function (): Promise<{ fails: number }> {
  await init();
  const dev = engineGpu()?.device;
  if (!dev?.features.has("timestamp-query")) { console.log("no GPU timestamps"); return { fails: 1 }; }
  const M = 5344, N = 32, gate = COST.tileMinK;
  // the median GPU time of the matmul, with the gate at `minK` (a fresh GPU, so no plan is shared)
  const time = async (minK: number, K: number, transposed: boolean): Promise<number> => {
    COST.tileMinK = minK;
    const gpu = new GPU(dev, true), a = buffer("a", [M, K]), b = buffer("b", transposed ? [N, K] : [K, N]);
    const data = (n: number) => { const g = dev.createBuffer({ size: n * 4, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST }); dev.queue.writeBuffer(g, 0, Float32Array.from({ length: n }, Math.random)); return g; };
    const bufs = new Map([["a", data(M * K)], ["b", data(K * N)]]), root = cellMatmul(a, transposed ? permute(b, [1, 0]) : b);
    const ms: number[] = [];
    for (let i = 0; i < 15; i++) { gpu.profile = []; for (const o of gpu.computeSync([root], (n) => bufs.get(n))) gpu.release(o); ms.push((await gpu.profiled()).reduce((t, r) => t + r.ms, 0)); }
    COST.tileMinK = gate;
    for (const g of bufs.values()) g.destroy();
    return ms.sort((x, y) => x - y)[7]!;
  };
  console.log(`[${M},K]·[K,${N}], median GPU ms: nest / tile, B as stored and B transposed`);
  // the derived gate: the least K from which on the tile wins by over 10% in both layouts
  let derived = Infinity, streak = true;
  for (const K of [64, 32, 16, 8, 6, 4, 3, 2]) {
    const [n, t, nT, tT] = [await time(Infinity, K, false), await time(1, K, false), await time(Infinity, K, true), await time(1, K, true)];
    const wins = t < 0.9 * n && tT < 0.9 * nT;
    streak &&= wins;
    if (streak) derived = K;
    console.log(`  K = ${String(K).padStart(2)}  ${n.toFixed(3)} / ${t.toFixed(3)}   ${nT.toFixed(3)} / ${tT.toFixed(3)}   ${wins ? "tile" : "no clear win"}`);
  }
  console.log(`derived tileMinK = ${derived}; COST.tileMinK = ${gate}`);
  return { fails: derived === gate ? 0 : 1 };
}
