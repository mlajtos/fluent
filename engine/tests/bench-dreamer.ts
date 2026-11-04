// A dreamer's training step through Fluent on the engine's GPU: steps per second, the CPU's
// share and the GPU's, and where one step's GPU time goes, kernel by kernel, against the
// roofline (QUERY=example=looped-dreamer for the other).
//   CHROME_ARGS=--enable-webgpu-developer-features bun engine/tests/run.ts engine/tests/bench-dreamer.ts
import { CodeParse, evaluateSyntaxTreeNode, evaluateGeneration, createScope } from "../../language.ts";
import { EXAMPLES } from "../../examples.ts";
import { engineGpu, init } from "../tensor.ts";
import NAMES from "../../names.txt" with { type: "text" };

export default async function (): Promise<{ fails: number }> {
  await init();
  const name = new URLSearchParams(globalThis.location?.search).get("example") ?? "dreamer";
  const src = (EXAMPLES as Record<string, string>)[name]!, head = src.slice(0, src.indexOf("; --- sampling"));
  // names.txt, which the page would fetch, in the program: a name a word
  const program = `${head.replace("Fetch(\"names.txt\")", `"${NAMES.trim().replace(/\n/g, " ")}"`)}opt: adamw(0.03, 0.001),\n({ opt(loss) }, { 0 })`;
  const [step] = evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(program), createScope())) as unknown as [() => unknown];
  const engine = engineGpu();
  if (!engine) { console.log("no GPU"); return { fails: 1 }; }
  const device = engine.device;
  for (let i = 0; i < 5; i++) step();                                         // warm: compile every kernel
  await device.queue.onSubmittedWorkDone();
  const N = 50, t0 = performance.now();
  let cpu = 0;
  for (let i = 0; i < N; i++) { const s = performance.now(); step(); cpu += performance.now() - s; }
  await device.queue.onSubmittedWorkDone();
  const ms = (performance.now() - t0) / N;
  console.log(`${name} step: ${ms.toFixed(2)} ms (${(1000 / ms).toFixed(1)} steps/s), ${(cpu / N).toFixed(2)} ms of it in the CPU`);
  {
    // the step's plans replayed with no evaluator in front of them: the GPU's share
    const calls: Parameters<typeof engine.computeSync>[] = [], run = engine.computeSync.bind(engine);
    engine.computeSync = (...a) => { calls.push(a); return run(...a); };
    step();
    Reflect.deleteProperty(engine, "computeSync");
    const r0 = performance.now();
    for (let i = 0; i < N; i++) for (const a of calls) for (const b of run(...a)) engine.release(b);
    await device.queue.onSubmittedWorkDone();
    console.log(`replay: ${((performance.now() - r0) / N).toFixed(2)} ms a step, ${calls.length} plans`);
  }
  {
    engine.profile = [];
    step();
    const rows = await engine.profiled(), by = new Map<string, { ms: number; n: number; bytes: number; work: number }>();
    for (const r of rows) { const e = by.get(r.label) ?? { ms: 0, n: 0, bytes: 0, work: 0 }; e.ms += r.ms; e.n++; e.bytes += r.bytes; e.work += r.work; by.set(r.label, e); }
    const total = rows.reduce((a, r) => a + r.ms, 0), bytes = rows.reduce((a, r) => a + r.bytes, 0);
    console.log(`one step: ${rows.length} kernels, ${total.toFixed(2)} ms GPU, ${(bytes / 1e6).toFixed(1)} MB bound`);
    // GB/s over the bytes of the buffers a kernel binds; GFLOP/s at 2 flops a loop step
    for (const [label, e] of [...by].sort((a, b) => b[1].ms - a[1].ms).slice(0, 15))
      console.log(`  ${e.ms.toFixed(3).padStart(7)} ms  ×${e.n}  ${label.padEnd(24)} ${(e.bytes / e.ms / 1e6).toFixed(1).padStart(6)} GB/s ${(2 * e.work / e.ms / 1e6).toFixed(1).padStart(7)} GFLOP/s`);
    if (globalThis.location?.search.includes("wgsl")) { const worst = rows.reduce((a, b) => (b.ms > a.ms ? b : a)); console.log(`\nslowest kernel (${worst.ms.toFixed(3)} ms, ${worst.label}):\n${worst.wgsl}`); }
  }
  return { fails: 0 };
}
