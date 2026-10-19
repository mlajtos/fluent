// plot at density 2, two series: the time to draw it, the memory its largest plan needs, and
// what a plot that grows a point at a time compiles – a training loop's history redrawn as it
// grows. It fails when a redraw within a power of two compiles a shader or builds a plan.
//   bun engine/tests/run.ts engine/tests/bench-plot.ts
import { CodeParse, evaluateSyntaxTreeNode, evaluateGeneration, createScope, Signal, density } from "../../language.ts";
import { init, scalar, engineGpu } from "../tensor.ts";
import { lowerMany } from "../gpu.ts";
import { toposort, numel, type UOp } from "../ir.ts";
import { canonical } from "../opt.ts";

const median = (xs: number[]) => [...xs].sort((a, b) => a - b)[xs.length >> 1]!;
export default async function (): Promise<{ fails: number }> {
  await init();
  const gpu = engineGpu()!, run = gpu.computeSync.bind(gpu), dev = gpu.device as GPUDevice & Record<string, unknown>;
  // every shader compiled, every plan built, and the memory of the largest
  let shaders = 0, plans = 0, bytes = 0;
  const seen = new Set<string>(), compile = dev.createComputePipeline.bind(dev);
  dev.createComputePipeline = (d: GPUComputePipelineDescriptor) => { shaders++; return compile(d); };
  gpu.computeSync = (roots: UOp[], get: Parameters<typeof run>[1]) => {
    const key = canonical(roots).key;
    if (!seen.has(key)) {
      seen.add(key); plans++;
      bytes = Math.max(bytes, toposort(lowerMany(roots)).filter((u) => u.op === "KERNEL").reduce((s, u) => s + numel(u.shape) * 4, 0));
    }
    return run(roots, get);
  };
  const draw = async (source: string): Promise<number> => {
    const t0 = performance.now();
    const v = evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(source), createScope()));
    const picture = (v instanceof Signal ? v.value : v) as { read(): Promise<unknown> } | Error;
    if (picture instanceof Error) throw picture;
    await picture.read();
    return performance.now() - t0;
  };
  density.value = scalar(2);
  let fails = 0;
  const waves = (n: number) => `x: 0..<${n}, plot(x, (sin(x ÷ ${n / 30}), cos(x ÷ ${n / 30}) + (sin(x × 12.9898 + 1) × 43758.5453 % 1 × 0.2)), ("sin", "cos"))`;
  for (const n of [50, 300, 1500, 10000, 100000]) {
    bytes = 0;
    await draw(waves(n));
    const times: number[] = [];
    for (let r = 0; r < 5; r++) times.push(await draw(waves(n)));
    console.log(`${n} points: ${median(times).toFixed(1)} ms to draw (median of 5), ${(bytes / 2 ** 20).toFixed(0)} MB in its largest plan`);
  }
  // a history of rows (step, train, held out) that grows a row at a time, as the Dreamer's does
  const rows = (n: number) => "[" + Array.from({ length: n }, (_, i) => `[${i * 20}, ${(Math.exp(-i / 80) + 0.1 * Math.sin(i * 12.9898)).toFixed(4)}, ${(Math.exp(-i / 120) + 0.2 + 0.1 * Math.cos(i * 7.233)).toFixed(4)}]`).join(", ") + "]";
  const grow = async (name: string, source: (n: number) => string, plansAllowed: number) => {
    await draw(source(200));
    shaders = 0; plans = 0;
    const times: number[] = [];
    for (let n = 201; n <= 240; n++) times.push(await draw(source(n)));
    const ok = shaders === 0 && plans <= plansAllowed * 40;
    if (!ok) fails++;
    console.log(`${ok ? "ok  " : "FAIL"} ${name}, 201 → 240 points: ${(shaders / 40).toFixed(1)} shaders and ${(plans / 40).toFixed(1)} plans a redraw, ${median(times).toFixed(1)} ms a redraw`);
  };
  await grow("waves computed from 0..<n", waves, 0);
  // the history is a new constant at each length, so its plans are new; their shaders are not
  await grow("a history's columns", (n) => `history: ${rows(n)}, h: transpose(history), plot(h_0, (h_1, h_2), ("train", "held out"))`, 3);
  density.value = scalar(1);
  return { fails };
}
