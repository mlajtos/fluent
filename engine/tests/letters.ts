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
  'Σ(canvas(520, 160, 0) . paint(letters("12", [10, 4], 12), solid(1)) > 0.3)',
  'Σ(canvas(520, 160, 0) . paint(letters(("−0.0520 ∞ … ·", "NaN g|"), [[10.5, 4.25], [200, 40]], 10), solid(1)))',
  // glyphs that come near their cells' sides, side by side; an outline, bold, and a glow
  'Σ(canvas(520, 160, 0) . paint(letters("Ag{}_~ ∞∞WW@@mm__", [3, 100], 24), solid(1)))',
  'Σ(canvas(520, 160, 0) . draw(letters("Ag{}_~", [3, 100], 24), 1, 2))',
  'Σ(canvas(520, 160, 0) . paint(letters("bold", [7.3, 20.6], 18), solid(1) above stroke(1, 2)))',
  'Σ(canvas(520, 160, 0) . paint(letters(("glow", "far"), [[40, 40], [300, 90]], 16), solid(1) above halo([1, 0.5, 0], 6)))',
  'Σ(plot(0..<10, (0..<10, 9 - 0..<10), ("up", "down")) > 0.3)',
  'z: Σ(⚄([4]) × 0), Σ(canvas(520, 160, 0) . paint(letters(unstack(DrawDigits([z ÷ z, -1 ÷ z, z - 1.5e9, z - 0.052, z - 4.2e-11, z + 2e-37])), [[0, 0], [0, 20], [0, 40], [0, 60], [0, 80], [0, 100]], 12), solid(1)))',
  'Σ(plot(0..<10 × 1e8, (0..<10 - 5, 9 - 0..<10), ("up", "down")) > 0.3)',
  // more points than the picture has columns: each column keeps its first, top, bottom and last
  'Σ(plot(0..<3000, (0..<3000 % 7, 0..<3000 % 600), ("seven", "saw")) > 0.3)',
  // a color with a 0 channel over a lit background: coverage that rounds past 1 in f32 made NaN
  'Σ(isNaN(canvas(40, 20, 0.5) . draw(dots([[10, 10], [30, 10]]), [1, 149 ÷ 255, 0], 6)))',
  // every byte to light and back as the screen shows it, and how far each lands from its byte
  'b: 0..<256 + Σ(⚄([4]) × 0), Σ(abs(screen(light(b ÷ 255)) × 255 - b)) × 1000',
  // noise hashes whole numbers mod 289 exactly in f32: a corner hashed differently is a
  // patch of the picture a unit wide, far from the coordinates' origin as well
  'Σ(abs(noise(pixels(canvas(160, 100)) ÷ 7 + [300, -250])))',
  'Σ(abs(fbm(pixels(canvas(160, 100)) ÷ 20 + 1000, 4)))',
  'p: pixels(canvas(80, 50)) ÷ 9, q: unstack(p, -1), Σ(abs(noise(stack((q_0, q_1, q_0 - q_1 + 77), -1))))',
  'p: pixels(canvas(80, 50)) ÷ 9, q: unstack(p, -1), Σ(abs(noise(stack((q_0, q_1, q_1 × 0.5, q_0 + 3), -1))))',
  // inks: a stroke above a halo, a ramp over a field, a curve made a distance by level
  'Σ(canvas(160, 100, 0) . paint(dots([[40, 50], [120, 50]]), stroke([1, 0, 0.3], 3) above halo([0, 0.5, 1], 12)))',
  'Σ(canvas(160, 100) . paint({ p | turbulence(p ÷ 30 + 250, 4) }, ramp([[0, 0, 0], [1, 0.5, 0], [1, 1, 1]])))',
  'Σ(canvas(160, 100, 0) . paint(level({ p | q: unstack(p, -1), q_1 - 50 + (20 × sin(q_0 ÷ 15)) }), stroke(1, 2)))',
  // outlines: filled with a hole, outlined and glowing, corners on the pixels' centers
  'a: 0..<140 ÷ 140 × 2×π, o: stack((cos(a), sin(a)), -1), Σ(canvas(520, 160, 0) . paint(polygon((o × 70 + [260, 80], o × 30 + [250, 75])), solid(1)))',
  'a: 0..<140 ÷ 140 × 2×π, o: stack((cos(a), sin(a)), -1), Σ(canvas(520, 160, 0) . paint(polygon(o × 70 + [260, 80]), stroke(1, 2) above halo([1, 0.5, 0], 8)))',
  'Σ(canvas(160, 100, 0) . paint(polygon([[10.5, 10.5], [150.5, 10.5], [80.5, 90.5], [40.5, 50.5], [20.5, 50.5]]), solid(1)))',
  // curves: a cubic stroked, and filled; a slice of a disk, an arc and its center
  'Σ(canvas(520, 160, 0) . draw(line(bezier([[20, 140], [150, -60], [330, 220], [500, 20]], 86)), 1, 2))',
  'Σ(canvas(520, 160, 0) . paint(polygon(bezier([[20, 140], [150, -60], [330, 220], [500, 20]], 86)), solid(1)))',
  'Σ(canvas(520, 160, 0) . paint(polygon(concat((arc([260, 80], 70, [0.3, 4], 49), [[260, 80]]))), solid(1) above halo(1, 4)))',
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
