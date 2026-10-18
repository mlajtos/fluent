// MARK: Painter
// A picture the page shows is painted by the worker into a canvas the page handed over. A
// picture is an image, [h, w, 3] or [h, w, 4] in 0–1: RGB or RGBA. A NaN is a hole, and the
// canvas stays clear there. On the GPU the image's buffer goes straight to the screen. An
// image larger than the canvas allows shows every n-th pixel.

import * as engine from "./engine/tensor.ts"
import { MATHS } from "./engine/ir.ts"

// A picture is an image, or a way to make one for a canvas of a size in device pixels. One
// made for a canvas that has a size – bars – fills it, and may be made larger than it:
// its top left corner shows. An image made for a painting is the painter's, and is freed
// once it is on the screen; none is nothing to paint.
export type Picture = engine.Tensor | ((size: [number, number]) => engine.Tensor | null)

// The canvas an image takes: the size it was made for, or its own, every `step`-th pixel
// past `max`. `w` is the image's own width.
type Layout = { w: number, channels: number, step: number, width: number, height: number }
const layout = (image: engine.Tensor, max: number, size?: [number, number]): Layout => {
  const [h = 1, w = 1, channels = 1] = image.shape
  if (size) { return { w, channels, step: 1, width: size[0], height: size[1] } }
  const step = Math.ceil(Math.max(w, h) / max)
  return { w, channels, step, width: Math.ceil(w / step), height: Math.ceil(h / step) }
}

// One way to paint, two ways to draw: on the GPU, or from the values on the host
abstract class Painter<Target extends { canvas: OffscreenCanvas }> {
  // painted frames not yet on the screen
  pending = 0
  protected targets = new Map<number, Target>()
  // the newest paint each canvas was asked for: one asked for again before it starts – a
  // canvas resized every frame – is painted once, the newest
  #asked = new Map<number, object>()
  constructor(readonly max: number) { }
  abstract attach(slot: number, canvas: OffscreenCanvas): void
  detach(slot: number): void { this.targets.delete(slot) }
  // resolves once it is painted
  async paint(slot: number, picture: Picture, size: [number, number]): Promise<void> {
    const ask = {}
    this.#asked.set(slot, ask)
    await new Promise((go) => setTimeout(go))
    const target = this.targets.get(slot)
    if (!target || this.#asked.get(slot) !== ask) { return }
    this.#asked.delete(slot)
    const made = typeof picture === "function", fit = size.map((s) => Math.min(this.max, Math.round(s))) as [number, number]
    const image = made ? picture(fit) : picture
    if (!image) { return }
    const at = layout(image, this.max, made && fit[0] && fit[1] ? fit : undefined)
    if (at.width < 1 || at.height < 1) { return }
    if (target.canvas.width !== at.width || target.canvas.height !== at.height) { Object.assign(target.canvas, { width: at.width, height: at.height }) }
    this.pending++
    try {
      await this.draw(target, image, at)
    } finally {
      this.pending--
      if (made) { engine.free(image) }
    }
  }
  protected abstract draw(target: Target, image: engine.Tensor, at: Layout): Promise<void>
}

const nan = MATHS.isnan!.wgsl
const SHADER = /* wgsl */ `
struct Image { width: u32, channels: u32, step: u32 }
@group(0) @binding(0) var<storage, read> values: array<f32>;
@group(0) @binding(1) var<uniform> image: Image;

// one triangle that covers the whole canvas
@vertex fn corner(@builtin(vertex_index) i: u32) -> @builtin(position) vec4f {
  return vec4f(vec2f(f32((i << 1u) & 2u), f32(i & 2u)) * 2.0 - 1.0, 0.0, 1.0);
}

@fragment fn pixel(@builtin(position) p: vec4f) -> @location(0) vec4f {
  let at = (u32(p.y) * image.width + u32(p.x)) * image.step * image.channels;
  let c = vec4f(values[at], values[at + 1u], values[at + 2u], select(1.0, values[at + image.channels - 1u], image.channels == 4u));
  if (${["x", "y", "z", "w"].map((k) => nan(`c.${k}`)).join(" + ")} > 0.0) { return vec4f(0.0); }
  let a = clamp(c.w, 0.0, 1.0);
  return vec4f(clamp(c.rgb, vec3f(0.0), vec3f(1.0)) * a, a);
}
`

type GpuTarget = { canvas: OffscreenCanvas, context: GPUCanvasContext, uniform: GPUBuffer }
export class GpuPainter extends Painter<GpuTarget> {
  #device: GPUDevice
  #format = navigator.gpu.getPreferredCanvasFormat()
  #pipeline: GPURenderPipeline

  constructor(device: GPUDevice) {
    super(device.limits.maxTextureDimension2D)
    this.#device = device
    const module = device.createShaderModule({ code: SHADER })
    this.#pipeline = device.createRenderPipeline({
      layout: "auto",
      vertex: { module, entryPoint: "corner" },
      fragment: { module, entryPoint: "pixel", targets: [{ format: this.#format }] },
    })
  }

  attach(slot: number, canvas: OffscreenCanvas): void {
    const context = canvas.getContext("webgpu") as GPUCanvasContext
    context.configure({ device: this.#device, format: this.#format, alphaMode: "premultiplied" })
    const uniform = this.#device.createBuffer({ size: 16, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST })
    this.targets.set(slot, { canvas, context, uniform })
  }

  override detach(slot: number): void {
    const target = this.targets.get(slot)
    target?.context.unconfigure()
    target?.uniform.destroy()
    super.detach(slot)
  }

  // a validation error is the painting's failure, not a silent blank canvas
  protected async draw({ context, uniform }: GpuTarget, image: engine.Tensor, { w, channels, step }: Layout): Promise<void> {
    const device = this.#device
    const values = await image.gpuBuffer()
    device.pushErrorScope("validation")
    device.queue.writeBuffer(uniform, 0, Uint32Array.of(w, channels, step, 0))
    const bindings = device.createBindGroup({
      layout: this.#pipeline.getBindGroupLayout(0),
      entries: [values, uniform].map((buffer, binding) => ({ binding, resource: { buffer } })),
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

// Without a GPU the same pictures are painted from the values on the host
type CpuTarget = { canvas: OffscreenCanvas, context: OffscreenCanvasRenderingContext2D }
export class CpuPainter extends Painter<CpuTarget> {
  constructor() { super(4096) }

  attach(slot: number, canvas: OffscreenCanvas): void {
    this.targets.set(slot, { canvas, context: canvas.getContext("2d")! })
  }

  protected async draw({ context }: CpuTarget, image: engine.Tensor, { w, channels, step, width, height }: Layout): Promise<void> {
    const values = await image.read()
    const picture = context.createImageData(width, height), out = picture.data
    for (let y = 0, p = 0; y < height; y++) {
      for (let x = 0; x < width; x++, p += 4) {
        const at = (y * w + x) * step * channels, r = values[at]!, g = values[at + 1]!, b = values[at + 2]!, a = channels === 4 ? values[at + 3]! : 1
        if (r !== r || g !== g || b !== b || a !== a) { continue }
        out[p] = r * 255
        out[p + 1] = g * 255
        out[p + 2] = b * 255
        out[p + 3] = a * 255
      }
    }
    context.putImageData(picture, 0, 0)
  }
}
