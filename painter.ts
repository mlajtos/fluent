// MARK: Painter
// A picture the page shows is painted by the worker into a canvas the page handed over:
// on the GPU the tensor's buffer goes straight to the screen, and only its range is read
// back, for the page's label. A picture is an image – one channel is gray, three are RGB,
// anything else shows its first channel – a heatmap, its values colored through viridis
// over their range, or bars, one per value, from zero and colored the same way. A picture larger than the canvas
// allows shows every n-th pixel.

import { interpolateViridis } from "d3-scale-chromatic"
import { rgb } from "d3-color"
import * as engine from "./engine/tensor.ts"
import type { TensorView } from "./protocol.ts"

const VIRIDIS = Uint8Array.from(Array.from({ length: 256 }, (_, i) => {
  const c = rgb(interpolateViridis(i / 255))
  return [c.r, c.g, c.b]
}).flat())
const ZERO = [68, 68, 68]

// what a picture paints from: its values, and for a heatmap or bars the range they span,
// a NaN taking no part in it
export type Picture = { view: TensorView, values: engine.Tensor, range?: engine.Tensor }
export const prepare = (view: TensorView, t: engine.Tensor): Picture => {
  if (view === "image") {
    engine.materialize([t])
    return { view, values: t }
  }
  const nan = engine.isnan(t)
  const range = engine.stack([engine.min(engine.where(nan, Infinity, t)), engine.max(engine.where(nan, -Infinity, t))])
  engine.materialize([t, range])
  return { view, values: t, range }
}

// The canvas a picture takes: bars fill the page's canvas, `size` in device pixels; an
// image or a heatmap takes its own size, every `step`-th pixel past `max`. `w` is the
// picture's own width.
type Layout = { w: number, channels: number, step: number, width: number, height: number }
const layout = ({ view, values }: Picture, size: [number, number], max: number): Layout => {
  const [h = 1, w = 1, channels = 1] = values.shape
  const step = view === "bars" ? 1 : Math.ceil(Math.max(w, h) / max)
  const [width, height] = view === "bars" ? size.map((s) => Math.min(max, Math.round(s))) : [Math.ceil(w / step), Math.ceil(h / step)]
  return { w, channels: view === "heat" ? 1 : channels, step, width: width!, height: height! }
}

// One way to paint, two ways to draw: on the GPU, or from the values on the host
abstract class Painter<Target extends { canvas: OffscreenCanvas }> {
  // painted frames not yet on the screen
  pending = 0
  protected targets = new Map<number, Target>()
  constructor(readonly max: number) { }
  abstract attach(slot: number, canvas: OffscreenCanvas): void
  detach(slot: number): void { this.targets.delete(slot) }
  // resolves once it is painted
  async paint(slot: number, picture: Picture, size: [number, number]): Promise<void> {
    const target = this.targets.get(slot), at = layout(picture, size, this.max)
    if (!target || at.width < 1 || at.height < 1) { return }
    if (target.canvas.width !== at.width || target.canvas.height !== at.height) { Object.assign(target.canvas, { width: at.width, height: at.height }) }
    this.pending++
    try {
      await this.draw(target, picture, at)
    } finally {
      this.pending--
    }
  }
  protected abstract draw(target: Target, picture: Picture, at: Layout): Promise<void>
}

const SHADER = /* wgsl */ `
struct Picture { mode: u32, width: u32, channels: u32, step: u32, n: u32, w: u32, h: u32 }
@group(0) @binding(0) var<storage, read> values: array<f32>;
@group(0) @binding(1) var<storage, read> range: array<f32>;
@group(0) @binding(2) var<storage, read> viridis: array<f32>;
@group(0) @binding(3) var<uniform> picture: Picture;

// one triangle that covers the whole canvas
@vertex fn corner(@builtin(vertex_index) i: u32) -> @builtin(position) vec4f {
  return vec4f(vec2f(f32((i << 1u) & 2u), f32(i & 2u)) * 2.0 - 1.0, 0.0, 1.0);
}

// a value's color: its place in the range, through viridis
fn color(x: f32) -> vec4f {
  let t = (x - range[0]) / max(range[1] - range[0], 1e-6);
  let j = select(0u, u32(round(min(t, 1.0) * 255.0)), t > 0.0) * 3u;
  return vec4f(viridis[j], viridis[j + 1u], viridis[j + 2u], 1.0);
}

// a column narrower than its values shows their extremes, colored by the one farther
// from zero; bars 4 pixels wide or more keep a gap between them
fn bar(p: vec2f) -> vec4f {
  let n = f32(picture.n);
  let w = f32(picture.w);
  let h = f32(picture.h);
  let i0 = u32((p.x - 0.5) * n / w);
  let i1 = max(i0 + 1u, u32((p.x + 0.5) * n / w));
  var lo = 3.4e38;
  var hi = -3.4e38;
  for (var i = i0; i < min(i1, picture.n); i++) {
    let x = values[i];
    if (x == x) { lo = min(lo, x); hi = max(hi, x); }
  }
  let top = max(range[1], 0.0);
  let span = max(top - min(range[0], 0.0), 1e-30);
  let v = top - p.y / h * span;
  if (abs(v) <= 0.5 * span / h) { return vec4f(${ZERO.map((c) => c / 255)}, 1.0); }
  let slot = fract(p.x * n / w);
  let gap = n * 4.0 <= w && (slot < 0.1 || slot > 0.9);
  if (!gap && v <= max(hi, 0.0) && v >= min(lo, 0.0)) { return color(select(lo, hi, hi >= -lo)); }
  return vec4f(0.0);
}

@fragment fn pixel(@builtin(position) p: vec4f) -> @location(0) vec4f {
  if (picture.mode == 2u) { return bar(p.xy); }
  let at = (u32(p.y) * picture.width + u32(p.x)) * picture.step * picture.channels;
  if (picture.mode == 1u) { return color(values[at]); }
  let rgb = select(vec3u(at), vec3u(at, at + 1u, at + 2u), picture.channels == 3u);
  return vec4f(clamp(vec3f(values[rgb.x], values[rgb.y], values[rgb.z]), vec3f(0.0), vec3f(1.0)), 1.0);
}
`
const MODES: Partial<Record<TensorView, number>> = { heat: 1, bars: 2 }

type GpuTarget = { canvas: OffscreenCanvas, context: GPUCanvasContext, picture: GPUBuffer }
export class GpuPainter extends Painter<GpuTarget> {
  #device: GPUDevice
  #format = navigator.gpu.getPreferredCanvasFormat()
  #pipeline: GPURenderPipeline
  #viridis: GPUBuffer
  #noRange: GPUBuffer

  constructor(device: GPUDevice) {
    super(device.limits.maxTextureDimension2D)
    this.#device = device
    const module = device.createShaderModule({ code: SHADER })
    this.#pipeline = device.createRenderPipeline({
      layout: "auto",
      vertex: { module, entryPoint: "corner" },
      fragment: { module, entryPoint: "pixel", targets: [{ format: this.#format }] },
    })
    const storage = (data: Float32Array<ArrayBuffer>) => {
      const b = device.createBuffer({ size: data.byteLength, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST })
      device.queue.writeBuffer(b, 0, data)
      return b
    }
    this.#viridis = storage(Float32Array.from(VIRIDIS, (c) => c / 255))
    this.#noRange = storage(Float32Array.of(0, 1))
  }

  attach(slot: number, canvas: OffscreenCanvas): void {
    const context = canvas.getContext("webgpu") as GPUCanvasContext
    context.configure({ device: this.#device, format: this.#format, alphaMode: "premultiplied" })
    const picture = this.#device.createBuffer({ size: 32, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST })
    this.targets.set(slot, { canvas, context, picture })
  }

  override detach(slot: number): void {
    const target = this.targets.get(slot)
    target?.context.unconfigure()
    target?.picture.destroy()
    super.detach(slot)
  }

  // a validation error is the painting's failure, not a silent blank canvas
  protected async draw({ context, picture: uniform }: GpuTarget, picture: Picture, { w, channels, step, width, height }: Layout): Promise<void> {
    const device = this.#device
    const [values, range] = await Promise.all([picture.values.gpuBuffer(), picture.range?.gpuBuffer() ?? this.#noRange])
    device.pushErrorScope("validation")
    device.queue.writeBuffer(uniform, 0, Uint32Array.of(MODES[picture.view] ?? 0, w, channels, step, picture.values.size, width, height, 0))
    const bindings = device.createBindGroup({
      layout: this.#pipeline.getBindGroupLayout(0),
      entries: [values, range, this.#viridis, uniform].map((buffer, binding) => ({ binding, resource: { buffer } })),
    })
    const encoder = device.createCommandEncoder()
    const pass = encoder.beginRenderPass({
      colorAttachments: [{ view: context.getCurrentTexture().createView(), loadOp: "clear", storeOp: "store", clearValue: [0, 0, 0, 0] }],
    })
    pass.setPipeline(this.#pipeline)
    pass.setBindGroup(0, bindings)
    pass.draw(3)
    pass.end()
    device.queue.submit([encoder.finish()])
    const failed = device.popErrorScope()
    await device.queue.onSubmittedWorkDone()
    const error = await failed
    if (error) { throw new Error(`painting failed: ${error.message}`) }
  }
}

// Without a GPU the same pictures are painted from the values on the host, as the shader
// paints them
type CpuTarget = { canvas: OffscreenCanvas, context: OffscreenCanvasRenderingContext2D }
export class CpuPainter extends Painter<CpuTarget> {
  constructor() { super(4096) }

  attach(slot: number, canvas: OffscreenCanvas): void {
    this.targets.set(slot, { canvas, context: canvas.getContext("2d")! })
  }

  protected async draw({ context }: CpuTarget, picture: Picture, { w, channels, step, width, height }: Layout): Promise<void> {
    const [values, [lo = 0, hi = 1]] = await Promise.all([picture.values.read(), picture.range?.read() ?? [0, 1]])
    const image = context.createImageData(width, height), out = image.data
    const put = (p: number, r: number, g: number, b: number) => { out[p] = r; out[p + 1] = g; out[p + 2] = b; out[p + 3] = 255 }
    const span = Math.max(hi - lo, 1e-6)
    const color = (p: number, v: number) => { const t = (v - lo) / span, j = (t > 0 ? Math.round(Math.min(t, 1) * 255) : 0) * 3; put(p, VIRIDIS[j]!, VIRIDIS[j + 1]!, VIRIDIS[j + 2]!) }
    if (picture.view === "bars") {
      const n = values.length, top = Math.max(hi, 0), extent = Math.max(top - Math.min(lo, 0), 1e-30)
      for (let x = 0; x < width; x++) {
        const i0 = Math.floor(x * n / width), i1 = Math.max(i0 + 1, Math.floor((x + 1) * n / width))
        let a = Infinity, b = -Infinity
        for (let i = i0; i < Math.min(i1, n); i++) { const v = values[i]!; if (v === v) { a = Math.min(a, v); b = Math.max(b, v) } }
        const slot = (x + 0.5) * n / width % 1, gap = n * 4 <= width && (slot < 0.1 || slot > 0.9)
        for (let y = 0; y < height; y++) {
          const v = top - (y + 0.5) / height * extent, p = 4 * (y * width + x)
          if (Math.abs(v) <= 0.5 * extent / height) { put(p, ZERO[0]!, ZERO[1]!, ZERO[2]!) }
          else if (!gap && v <= Math.max(b, 0) && v >= Math.min(a, 0)) { color(p, b >= -a ? b : a) }
        }
      }
    } else {
      const c = channels === 3 ? 1 : 0
      for (let y = 0, p = 0; y < height; y++) {
        for (let x = 0; x < width; x++, p += 4) {
          const at = (y * w + x) * step * channels
          if (picture.view === "heat") {
            color(p, values[at]!)
          } else {
            put(p, values[at]! * 255, values[at + c]! * 255, values[at + 2 * c]! * 255)
          }
        }
      }
    }
    context.putImageData(image, 0, 0)
  }
}
