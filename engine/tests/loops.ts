// Loop gates: a loop runs its step's plan on leaves it owns and gives back each state once
// the next is computed, so on the device it holds about two states however many steps it
// runs – an inner loop too, inside each step of an outer one. Runs in headless Chrome:
//   bun engine/tests/run.ts engine/tests/loops.ts

import { init, engineGpu, loop, overFrame, tensor, scalar, add, subtract, multiply, maximum, less, full, deviceBytes, type Tensor } from "../tensor.ts";

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

  // A loop on a cell over a frame: each pixel of a Mandelbrot runs until it escapes, a lane
  // each – a small frame on the host, a large one on the device – against the same steps in
  // f32 here. Its state is each lane's step count.
  const N = 30;
  const escape = (w: number, h: number) => {
    const re = Float32Array.from({ length: w * h }, (_, i) => -2.5 + (3.5 * (i % w)) / (w - 1)), im = Float32Array.from({ length: w * h }, (_, i) => -1.25 + (2.5 * Math.floor(i / w)) / (h - 1));
    const counts = overFrame([h, w], [tensor(re, { shape: [h, w] }), tensor(im, { shape: [h, w] })], ([cr, ci]) => {
      const z = scalar(0);
      return loop([z, z, z], 10000, true, ([x, y, k]) => {
        const nx = add(subtract(multiply(x!, x!), multiply(y!, y!)), cr!), ny = add(multiply(multiply(x!, 2), y!), ci!), nk = add(k!, 1);
        return [nx, ny, nk, maximum(less(4, add(multiply(nx, nx), multiply(ny, ny))), less(N - 0.5, nk))];
      })![2];
    })!;
    const f = Math.fround, want = Array.from(re, (cr, i) => {
      const ci = im[i]!;
      let x = 0, y = 0, k = 0;
      for (;;) { const nx = f(f(f(x * x) - f(y * y)) + cr), ny = f(f(f(x * 2) * y) + ci); x = nx; y = ny; k++; if (4 < f(f(x * x) + f(y * y)) || k > N - 0.5) return k; }
    });
    return { counts, want };
  };
  for (const [w, h, where] of [[12, 8, "host"], [256, 128, "device"]] as const) {
    const { counts, want } = escape(w, h), got = await counts.read(), off = want.filter((s, i) => got[i] !== s).length;
    const steps = want.reduce((a, b) => a + b, 0), masked = Math.max(...want) * want.length;
    check(`each lane stops at its own step, ${w} × ${h} on the ${where}`, where === "host" ? off === 0 : off <= want.length / 1000,
      `${off} of ${want.length} lanes off by float rounding; ${steps} lane-steps where every lane runs ${masked}`);
  }
  return { fails };
}
