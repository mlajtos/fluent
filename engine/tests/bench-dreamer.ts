// The dreamer's training step through Fluent on the engine's GPU: steps per second, and
// where one step's GPU time goes, kernel by kernel.
//   CHROME_ARGS=--enable-webgpu-developer-features bun engine/tests/run.ts engine/tests/bench-dreamer.ts
import { CodeParse, evaluateSyntaxTreeNode, evaluateGeneration, createScope } from "../../language.ts";
import { EXAMPLES } from "../../examples.ts";
import { engineGpu } from "../tensor.ts";

export default async function (): Promise<{ fails: number }> {
  const src = EXAMPLES["dreamer"]!, head = src.slice(0, src.indexOf("; --- sampling"));
  const program = `${head}opt: adamw(0.03, 0.001),\n({ opt(loss) }, { 0 })`;
  const [step] = evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(program), createScope())) as unknown as [() => unknown];
  const engine = engineGpu();
  if (!engine) { console.log("no GPU"); return { fails: 1 }; }
  const device = engine.device;
  for (let i = 0; i < 5; i++) step();                                         // warm: compile every kernel
  await device.queue.onSubmittedWorkDone();
  const N = 50, t0 = performance.now();
  for (let i = 0; i < N; i++) step();
  await device.queue.onSubmittedWorkDone();
  const ms = (performance.now() - t0) / N;
  console.log(`dreamer step: ${ms.toFixed(2)} ms (${(1000 / ms).toFixed(1)} steps/s)`);
  {
    engine.profile = [];
    step();
    const rows = await engine.profiled(), by = new Map<string, { ms: number; n: number }>();
    for (const r of rows) { const e = by.get(r.label) ?? { ms: 0, n: 0 }; e.ms += r.ms; e.n++; by.set(r.label, e); }
    const total = rows.reduce((a, r) => a + r.ms, 0);
    console.log(`one step: ${rows.length} kernels, ${total.toFixed(2)} ms GPU`);
    for (const [label, e] of [...by].sort((a, b) => b[1].ms - a[1].ms).slice(0, 15)) console.log(`  ${e.ms.toFixed(3).padStart(7)} ms  ×${e.n}  ${label}`);
    if (globalThis.location?.search.includes("wgsl")) { const worst = rows.reduce((a, b) => (b.ms > a.ms ? b : a)); console.log(`\nslowest kernel (${worst.ms.toFixed(3)} ms, ${worst.label}):\n${worst.wgsl}`); }
  }
  return { fails: 0 };
}
