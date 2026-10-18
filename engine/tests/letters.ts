// Text and plots drawn on the GPU match the CPU. The CPU keeps a fused kernel's
// intermediates in doubles, the GPU in f32, so a Prelude function that leans on a value
// only a double can hold – 2 to the power -200 – draws differently: this gate catches it.
//   bun engine/tests/run.ts engine/tests/letters.ts
import { CodeParse, evaluateSyntaxTreeNode, evaluateGeneration, createScope } from "../../language.ts";
import { init } from "../tensor.ts";

const value = (source: string) => evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(source), createScope())) as { read(): Promise<Float32Array> };
const CASES = [
  'Σ(canvas(520, 160, 0) . draw(letters("12", [10, 4], 12), 1, 1) > 0.3)',
  'Σ(canvas(520, 160, 0) . draw(letters(("−0.0520 ∞ … ·", "NaN g|"), [[10.5, 4.25], [200, 40]], 10), 1, 1))',
  'Σ(canvas(520, 160, 0) . draw(letters("Ag{}_~", [3, 100], 24), 1, 2))',
  'Σ(plot(0..<10, (0..<10, 9 - 0..<10), ("up", "down")) > 0.3)',
  'Σ(plot(0..<10 × 1e8, (0..<10 - 5, 9 - 0..<10), ("up", "down")) > 0.3)',
];

export default async function (): Promise<{ fails: number }> {
  const cpu: number[] = [];
  for (const source of CASES) cpu.push((await value(source).read())[0]!);
  await init();
  let fails = 0;
  for (const [i, source] of CASES.entries()) {
    const gpu = (await value(source).read())[0]!;
    const ok = Math.abs(gpu - cpu[i]!) <= 1;
    if (!ok) fails++;
    console.log(`${ok ? "ok  " : "FAIL"} cpu ${cpu[i]} gpu ${gpu}  ${source}`);
  }
  return { fails };
}
