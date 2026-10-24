// What a frame of live values costs on the GPU: the IDE's host runs a program, a frame at a
// time, and every live value it shows is sampled and described, as the page asks. A program
// that reads the clock moves with it; one with a slider or scrubber has it dragged a step a
// frame. Per frame: kernels dispatched, submits, and the time from the frame to the GPU
// done. QUERY="?frames=60&only=drawing" for another count, or one program.
//   bun engine/tests/run.ts engine/tests/bench-live.ts
import { startHost } from "../../host.ts";
import { EXAMPLES } from "../../examples.ts";
import { engineGpu, init } from "../tensor.ts";
import { extendEnvironment } from "../../language.ts";

const PROGRAMS: Record<string, string> = {
  drawing: (EXAMPLES as Record<string, string>).drawing!,
  mandelbrot: (EXAMPLES as Record<string, string>).mandelbrot!,
  "softmax of a live value": "t: Time(), x: sin(t + 0..<4096 ÷ 100), softmax(x)",
  // each training step counted
  dreamer: (EXAMPLES as Record<string, string>).dreamer!.replace("opt(loss),", "opt(loss), BenchStep(),"),
};

// the first control a description holds, by the id of the live value it writes
const controlIn = (d: any): number | undefined => {
  if (!d || typeof d !== "object") { return undefined }
  if (d.k === "ui" && ["Slider", "Scrubber"].includes(d.type) && d.slots?.target?.k === "live") { return d.slots.target.id }
  for (const v of Object.values(d)) { const id = Array.isArray(v) ? v.map(controlIn).find((x) => x !== undefined) : controlIn(v); if (id !== undefined) { return id } }
  return undefined
};

export default async function (): Promise<{ fails: number }> {
  await init();
  const gpu = engineGpu();
  if (!gpu) { console.log("no GPU"); return { fails: 1 }; }
  const device = gpu.device;
  // frames come when the bench says so
  const frames: FrameRequestCallback[] = [];
  (globalThis as { requestAnimationFrame: unknown }).requestAnimationFrame = (f: FrameRequestCallback) => frames.push(f);
  let dispatches = 0, submits = 0;
  const pass = GPUComputePassEncoder.prototype, queue = GPUQueue.prototype;
  const dispatch = pass.dispatchWorkgroups, submit = queue.submit;
  pass.dispatchWorkgroups = function (...a: Parameters<typeof dispatch>) { dispatches++; return dispatch.apply(this, a); };
  queue.submit = function (...a: Parameters<typeof submit>) { submits++; return submit.apply(this, a); };
  // the page: it draws each batch at once, and knows the program's first control
  let control: number | undefined;
  const send = startHost((m) => {
    if (m.t !== "lives") { return; }
    for (const l of m.lives) { if (l.id === 0 && control === undefined) { control = controlIn(l.desc); } }
    queueMicrotask(() => send({ t: "drawn" }));
  });
  let steps = 0;
  extendEnvironment({ BenchStep: () => { steps++; return null; } });
  const settle = () => new Promise((r) => setTimeout(r, 0));
  const query = new URLSearchParams(globalThis.location?.search), n = Number(query.get("frames") ?? 40), only = query.get("only");
  for (const [name, source] of Object.entries(PROGRAMS)) {
    if (only && !name.startsWith(only)) { continue; }
    control = undefined;
    send({ t: "code", source });
    for (let i = 0; i < 20 && control === undefined; i++) { for (const f of frames.splice(0)) f(performance.now()); await settle(); }
    let seq = 0;
    // a frame: a drag, if there is a control; what waits for the frame runs – the clock's
    // tick, the look at what changed – then the page's answer, then the GPU's work
    let busy = 0;
    const tick = async () => {
      const t0 = performance.now();
      if (control !== undefined) { send({ t: "set", id: control, value: 40 + (++seq % 2), seq }); }
      for (const f of frames.splice(0)) f(performance.now());
      busy += performance.now() - t0;
      await settle(); await settle();
      await device.queue.onSubmittedWorkDone();
      return performance.now() - t0;
    };
    for (let i = 0; i < 8; i++) await tick();                                  // warm: every plan compiled
    const d0 = dispatches, s0 = submits, ms: number[] = [], t0 = performance.now();
    busy = 0; steps = 0;
    for (let i = 0; i < n; i++) ms.push(await tick());
    const total = performance.now() - t0;
    // the GPU's own time for the kernels of a few frames, each kernel timed alone
    let gpuMs = 0;
    const by = new Map<string, number>();
    for (let i = 0; i < 5; i++) {
      gpu.profile = []; await tick();
      for (const r of await gpu.profiled()) {
        gpuMs += r.ms / 5; by.set(r.label, (by.get(r.label) ?? 0) + r.ms / 5);
        if (i === 3 && query.has("wgsl")) { for (const line of `WGSL ${r.label} ${r.ms.toFixed(3)} ms ${r.bytes} bytes ${r.work} work\n${r.wgsl}`.split("\n")) console.log(line); }
      }
    }
    if (query.has("kernels")) { for (const [label, k] of [...by].sort((a, b) => b[1] - a[1])) console.log(`  ${k.toFixed(3)} ms  ${label}`); }
    ms.sort((a, b) => a - b);
    console.log(`${name}: ${((dispatches - d0) / n).toFixed(1)} kernels a frame, ${((submits - s0) / n).toFixed(1)} submits, ${ms[n >> 1]!.toFixed(2)} ms median (${ms[Math.floor(n * 0.9)]!.toFixed(2)} p90), ${(busy / n).toFixed(2)} ms of it in the frame's callbacks, ${gpuMs.toFixed(3)} ms of GPU kernels${steps ? `; ${steps} steps, ${(steps / total * 1000).toFixed(1)} a second, ${((dispatches - d0) / steps).toFixed(1)} kernels a step` : ""}`);
    send({ t: "code", source: "0" });
    for (let i = 0; i < 4; i++) { for (const f of frames.splice(0)) f(performance.now()); await settle(); }
  }
  pass.dispatchWorkgroups = dispatch; queue.submit = submit;
  return { fails: 0 };
}
