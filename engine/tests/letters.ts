// Text and plots drawn on the GPU match the CPU. The CPU keeps a fused kernel's
// intermediates in doubles, the GPU in f32, so a Prelude function that leans on a value
// only a double can hold – 2 to the power -200, a coverage of exactly 1 – draws
// differently: this gate catches it.
// Each case draws at the display densities 1 and 2.
//   bun engine/tests/run.ts engine/tests/letters.ts
import { CodeParse, evaluateSyntaxTreeNode, evaluateGeneration, createScope, Signal, density } from "../../language.ts";
import { init, scalar, type Tensor } from "../tensor.ts";

// a picture drawn in units follows the density: its value is a cell's
const value = (source: string): Tensor => {
  const v = evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(source), createScope()));
  return (v instanceof Signal ? v.value : v) as Tensor;
};
const CASES = [
  'Σ(canvas(520, 160, 0) . draw(letters("12", [10, 4], 12), 1, 1) > 0.3)',
  'Σ(canvas(520, 160, 0) . draw(letters(("−0.0520 ∞ … ·", "NaN g|"), [[10.5, 4.25], [200, 40]], 10), 1, 1))',
  'Σ(canvas(520, 160, 0) . draw(letters("Ag{}_~", [3, 100], 24), 1, 2))',
  'Σ(plot(0..<10, (0..<10, 9 - 0..<10), ("up", "down")) > 0.3)',
  'z: Σ(⚄([4]) × 0), Σ(canvas(520, 160, 0) . draw(letters(unstack(DrawDigits([z ÷ z, -1 ÷ z, z - 1.5e9, z - 0.052, z - 4.2e-11, z + 2e-37])), [[0, 0], [0, 20], [0, 40], [0, 60], [0, 80], [0, 100]], 12), 1, 1))',
  'Σ(plot(0..<10 × 1e8, (0..<10 - 5, 9 - 0..<10), ("up", "down")) > 0.3)',
  // a color with a 0 channel over a lit background: coverage that rounds past 1 in f32 made NaN
  'Σ(isNaN(canvas(40, 20, 0.5) . draw(dots([[10, 10], [30, 10]]), [1, 149 ÷ 255, 0], 6)))',
];

const RUNS = [1, 2].flatMap((d) => CASES.map((source) => [d, source] as const));
const at = async (d: number, source: string): Promise<number> => {
  density.value = scalar(d);
  return (await value(source).read())[0]!;
};

export default async function (): Promise<{ fails: number }> {
  const cpu: number[] = [];
  for (const [d, source] of RUNS) cpu.push(await at(d, source));
  await init();
  let fails = 0;
  for (const [i, [d, source]] of RUNS.entries()) {
    const gpu = await at(d, source);
    const ok = Math.abs(gpu - cpu[i]!) <= 1;
    if (!ok) fails++;
    console.log(`${ok ? "ok  " : "FAIL"} density ${d} cpu ${cpu[i]} gpu ${gpu}  ${source}`);
  }
  density.value = scalar(1);
  return { fails };
}
