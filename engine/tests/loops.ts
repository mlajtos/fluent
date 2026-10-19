// Loop gates: a loop runs its step's plan on leaves it owns and gives back each state once
// the next is computed, so on the device it holds about two states however many steps it
// runs – an inner loop too, inside each step of an outer one. Runs in headless Chrome:
//   bun engine/tests/run.ts engine/tests/loops.ts

import { init, engineGpu, loop, add, multiply, less, full, deviceBytes, type Tensor } from "../tensor.ts";

export default async function (): Promise<{ fails: number }> {
  let fails = 0;
  const check = (name: string, ok: boolean, detail = "") => { if (!ok) fails++; console.log(`  ${ok ? "✓" : "✗"} ${name}${detail ? "  " + detail : ""}`); };
  await init();
  console.log(`engine loop gates (${engineGpu() ? "gpu" : "cpu"})`);
  // a state of 0.5 MB: past what the host computes, so every step runs on the device
  const n = 1 << 17, state = n * 4, mb = (b: number) => `${(b / 2 ** 20).toFixed(1)} MB`;
  const held = (run: () => Float32Array | Promise<Float32Array>) => async () => { const live = deviceBytes().live, v = await run(); return { v, held: deviceBytes().peak - live }; };

  // 2000 steps: longer than a count loop unrolls
  const count = await held(() => loop([full([n], 0)], 2000, false, ([x]) => [add(x!, 1)])![0]!.read())();
  check("2000 steps hold two states", count.held <= 2.5 * state, `${mb(count.held)} for a ${mb(state)} state`);
  check("a count loop runs every step", count.v[n - 1] === 2000);

  const until = await held(() => loop([full([n], 0)], 10000, true, ([x]) => { const y = add(x!, 1); return [y, less(199.5, y)]; })![0]!.read())();
  check("a loop until holds two states", until.held <= 2.5 * state, mb(until.held));
  check("it stops where its verdict holds", until.v[n - 1] === 200);

  // 50 outer steps, each of 3 inner ones
  const inner = (x: Tensor) => loop([multiply(x, 1)], 10000, true, ([y]) => { const z = add(y!, 1); return [z, less(add(x, 2.5), z)]; })![0]!;
  const nested = await held(() => loop([full([n], 0)], 10000, true, ([x]) => { const z = inner(x!); return [z, less(149.5, z)]; })![0]!.read())();
  check("an inner loop's states go back with each outer step", nested.held <= 4.5 * state, mb(nested.held));
  check("the inner loop runs in every outer step", nested.v[n - 1] === 150);
  return { fails };
}
