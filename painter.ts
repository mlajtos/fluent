// MARK: Painter
// A picture the page shows is painted by the worker into a canvas the page handed over:
// on the GPU the tensor's buffer goes straight to the screen, and only its range is read
// back, for the page's label. A picture is an image – one channel is gray, three are RGB,
// anything else shows its first channel – a heatmap, its values colored through viridis
// over their range, or bars, one per value, from zero. A picture larger than the canvas
// allows shows every n-th pixel.

import { interpolateViridis } from "d3-scale-chromatic"
import { rgb } from "d3-color"
import * as engine from "./engine/tensor.ts"
import type { TensorView } from "./protocol.ts"

const VIRIDIS = Uint8Array.from(Array.from({ length: 256 }, (_, i) => {
  const c = rgb(interpolateViridis(i / 255))
  return [c.r, c.g, c.b]
}).flat())
const BAR = [10, 129, 239], ZERO = [68, 68, 68]

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
// image or a heatmap takes its own size, every `step`-th pixel past `max`
const layout = ({ view, values }: Picture, size: [number, number], max: number) => {
  const [h = 1, w = 1, channels = 1] = values.shape
  const step = view === "bars" ? 1 : Math.ceil(Math.max(w, h) / max)
  const [width, height] = view === "bars" ? size.map((s) => Math.min(max, Math.round(s))) as [number, number] : [Math.ceil(w / step), Math.ceil(h / step)]
  return { w, channels: view === "heat" ? 1 : channels, step, width, height }
}

export interface Painter {
  // painted frames not yet on the screen
  readonly pending: number
  attach(slot: number, canvas: OffscreenCanvas): void
  detach(slot: number): void
  // resolves once it is painted
  paint(slot: number, picture: Picture, size: [number, number]): Promise<void>
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

// a column narrower than its values shows their extremes; bars 4 pixels wide or more
// keep a gap between them
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
  if (!gap && v <= max(hi, 0.0) && v >= min(lo, 0.0)) { return vec4f(${BAR.map((c) => c / 255)}, 1.0); }
  return vec4f(0.0);
}

@fragment fn pixel(@builtin(position) p: vec4f) -> @location(0) vec4f {
  if (picture.mode == 2u) { return bar(p.xy); }
  let at = (u32(p.y) * picture.width + u32(p.x)) * picture.step * picture.channels;
  if (picture.mode == 1u) {
    let t = (values[at] - range[0]) / max(range[1] - range[0], 1e-6);
    let j = select(0u, u32(round(min(t, 1.0) * 255.0)), t > 0.0) * 3u;
    return vec4f(viridis[j], viridis[j + 1u], viridis[j + 2u], 1.0);
  }
  let rgb = select(vec3u(at), vec3u(at, at + 1u, at + 2u), picture.channels == 3u);
  return vec4f(clamp(vec3f(values[rgb.x], values[rgb.y], values[rgb.z]), vec3f(0.0), vec3f(1.0)), 1.0);
}
`
const MODES: Partial<Record<TensorView, number>> = { heat: 1, bars: 2 }

export class GpuPainter implements Painter {
  #device: GPUDevice
  #format: GPUTextureFormat
  #pipeline: GPURenderPipeline
  #viridis: GPUBuffer
  #noRange: GPUBuffer
  #canvases = new Map<number, { canvas: OffscreenCanvas, context: GPUCanvasContext, picture: GPUBuffer }>()
  pending = 0

  constructor(device: GPUDevice) {
    this.#device = device
    this.#format = navigator.gpu.getPreferredCanvasFormat()
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
    this.#canvases.set(slot, { canvas, context, picture })
  }

  detach(slot: number): void {
    const target = this.#canvases.get(slot)
    if (!target) { return }
    target.context.unconfigure()
    target.picture.destroy()
    this.#canvases.delete(slot)
  }

  async paint(slot: number, picture: Picture, size: [number, number]): Promise<void> {
    const target = this.#canvases.get(slot), device = this.#device
    const { w, channels, step, width, height } = layout(picture, size, device.limits.maxTextureDimension2D)
    if (!target || width < 1 || height < 1) { return }
    resize(target.canvas, width, height)
    const [values, range] = await Promise.all([picture.values.gpuBuffer(), picture.range?.gpuBuffer() ?? this.#noRange])
    device.pushErrorScope("validation")
    device.queue.writeBuffer(target.picture, 0, Uint32Array.of(MODES[picture.view] ?? 0, w, channels, step, picture.values.size, width, height, 0))
    const bindings = device.createBindGroup({
      layout: this.#pipeline.getBindGroupLayout(0),
      entries: [values, range, this.#viridis, target.picture].map((buffer, binding) => ({ binding, resource: { buffer } })),
    })
    const encoder = device.createCommandEncoder()
    const pass = encoder.beginRenderPass({
      colorAttachments: [{ view: target.context.getCurrentTexture().createView(), loadOp: "clear", storeOp: "store", clearValue: [0, 0, 0, 0] }],
    })
    pass.setPipeline(this.#pipeline)
    pass.setBindGroup(0, bindings)
    pass.draw(3)
    pass.end()
    device.queue.submit([encoder.finish()])
    const failed = device.popErrorScope()
    this.pending++
    try {
      await device.queue.onSubmittedWorkDone()
    } finally {
      this.pending--
    }
    const error = await failed
    if (error) { throw new Error(`painting failed: ${error.message}`) }
  }
}

const resize = (canvas: OffscreenCanvas, width: number, height: number) => {
  if (canvas.width !== width || canvas.height !== height) {
    canvas.width = width
    canvas.height = height
  }
}

// Without a GPU the same pictures are painted from the values on the host
export class CpuPainter implements Painter {
  #canvases = new Map<number, { canvas: OffscreenCanvas, context: OffscreenCanvasRenderingContext2D }>()
  pending = 0

  attach(slot: number, canvas: OffscreenCanvas): void {
    this.#canvases.set(slot, { canvas, context: canvas.getContext("2d")! })
  }

  detach(slot: number): void { this.#canvases.delete(slot) }

  async paint(slot: number, picture: Picture, size: [number, number]): Promise<void> {
    const target = this.#canvases.get(slot)
    const { w, channels, step, width, height } = layout(picture, size, 4096)
    if (!target || width < 1 || height < 1) { return }
    this.pending++
    try {
      const [values, [lo = 0, hi = 1]] = await Promise.all([picture.values.read(), picture.range?.read() ?? [0, 1]])
      resize(target.canvas, width, height)
      const image = target.context.createImageData(width, height), out = image.data
      const put = (p: number, r: number, g: number, b: number) => { out[p] = r; out[p + 1] = g; out[p + 2] = b; out[p + 3] = 255 }
      if (picture.view === "bars") {
        // as the shader paints them
        const n = values.length, top = Math.max(hi, 0), span = Math.max(top - Math.min(lo, 0), 1e-30)
        for (let x = 0; x < width; x++) {
          const i0 = Math.floor(x * n / width), i1 = Math.max(i0 + 1, Math.floor((x + 1) * n / width))
          let a = Infinity, b = -Infinity
          for (let i = i0; i < Math.min(i1, n); i++) { const v = values[i]!; if (v === v) { a = Math.min(a, v); b = Math.max(b, v) } }
          const slot = (x + 0.5) * n / width % 1, gap = n * 4 <= width && (slot < 0.1 || slot > 0.9)
          for (let y = 0; y < height; y++) {
            const v = top - (y + 0.5) / height * span, p = 4 * (y * width + x)
            if (Math.abs(v) <= 0.5 * span / height) { put(p, ZERO[0]!, ZERO[1]!, ZERO[2]!) }
            else if (!gap && v <= Math.max(b, 0) && v >= Math.min(a, 0)) { put(p, BAR[0]!, BAR[1]!, BAR[2]!) }
          }
        }
      } else {
        const span = Math.max(hi - lo, 1e-6), c = channels === 3 ? 1 : 0
        for (let y = 0, p = 0; y < height; y++) {
          for (let x = 0; x < width; x++, p += 4) {
            const at = (y * w + x) * step * channels
            if (picture.view === "heat") {
              const t = (values[at]! - lo) / span, j = (t > 0 ? Math.round(Math.min(t, 1) * 255) : 0) * 3
              put(p, VIRIDIS[j]!, VIRIDIS[j + 1]!, VIRIDIS[j + 2]!)
            } else {
              put(p, values[at]! * 255, values[at + c]! * 255, values[at + 2 * c]! * 255)
            }
          }
        }
      }
      target.context.putImageData(image, 0, 0)
    } finally {
      this.pending--
    }
  }
}
