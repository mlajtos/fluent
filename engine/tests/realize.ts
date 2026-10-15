// The gates run the path the app ships: the CPU realizes synchronously; the GPU computes
// into buffers of its own, which come back by an asynchronous read.
import { numel, type UOp } from "../ir.ts";
import { CPU, type HostData } from "../cpu.ts";
import type { GPU } from "../gpu.ts";

export async function realizeMany(be: CPU | GPU, roots: UOp[], inputs = new Map<string, HostData>()): Promise<HostData[]> {
  if (be instanceof CPU) return be.realizeSync(roots, inputs);
  const dev = be.device, bufs = new Map<string, GPUBuffer>();
  for (const [name, t] of inputs) {
    const b = dev.createBuffer({ size: Math.max(t.data.length, 1) * 4, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC | GPUBufferUsage.COPY_DST });
    dev.queue.writeBuffer(b, 0, t.data as unknown as GPUAllowSharedBufferSource);
    bufs.set(name, b);
  }
  const outs = be.computeSync(roots, (name) => bufs.get(name));
  const data = await Promise.all(outs.map((b, i) => be.readAsync(b, numel(roots[i]!.shape))));
  for (const b of outs) be.release(b);
  for (const b of bufs.values()) b.destroy();
  return data.map((d, i) => ({ data: d, shape: roots[i]!.shape }));
}
export const realize = async (be: CPU | GPU, root: UOp, inputs = new Map<string, HostData>()): Promise<HostData> => (await realizeMany(be, [root], inputs))[0]!;
