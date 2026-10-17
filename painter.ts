// MARK: Painter
// A picture the page shows is painted by the worker, on the GPU, into a canvas the page
// handed over: the tensor's buffer goes straight to the screen, and nothing is read back.
// A picture is an image – one channel is gray, three are RGB, anything else shows its
// first channel – or a heatmap, its values colored through viridis.

import { interpolateViridis } from "d3-scale-chromatic"
import { rgb } from "d3-color"
import * as engine from "./engine/tensor.ts"

const SHADER = /* wgsl */ `
struct Picture { width: u32, channels: u32, heat: u32 }
@group(0) @binding(0) var<storage, read> values: array<f32>;
@group(0) @binding(1) var<storage, read> viridis: array<f32>;
@group(0) @binding(2) var<uniform> picture: Picture;

// one triangle that covers the whole canvas
@vertex fn corner(@builtin(vertex_index) i: u32) -> @builtin(position) vec4f {
  return vec4f(vec2f(f32((i << 1u) & 2u), f32(i & 2u)) * 2.0 - 1.0, 0.0, 1.0);
}

@fragment fn pixel(@builtin(position) p: vec4f) -> @location(0) vec4f {
  let at = (u32(p.y) * picture.width + u32(p.x)) * picture.channels;
  if (picture.heat == 1u) {
    let t = values[at];
    let j = select(0u, u32(round(min(t, 1.0) * 255.0)), t > 0.0) * 3u;
    return vec4f(viridis[j], viridis[j + 1u], viridis[j + 2u], 1.0);
  }
  let rgb = select(vec3u(at), vec3u(at, at + 1u, at + 2u), picture.channels == 3u);
  return vec4f(clamp(vec3f(values[rgb.x], values[rgb.y], values[rgb.z]), vec3f(0.0), vec3f(1.0)), 1.0);
}
`

// a heatmap's values, spread over 0–1 by their own range; a NaN takes no part in it
const spread = (x: engine.Tensor): engine.Tensor => {
  const nan = engine.isnan(x)
  const lo = engine.min(engine.where(nan, Infinity, x)), hi = engine.max(engine.where(nan, -Infinity, x))
  return engine.divide(engine.subtract(x, lo), engine.maximum(engine.subtract(hi, lo), 1e-6))
}

export class Painter {
  #device: GPUDevice
  #format: GPUTextureFormat
  #pipeline: GPURenderPipeline
  #viridis: GPUBuffer
  #canvases = new Map<number, { canvas: OffscreenCanvas, context: GPUCanvasContext, picture: GPUBuffer }>()
  // painted frames the GPU hasn't finished
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
    const lut = Float32Array.from(Array.from({ length: 256 }, (_, i) => {
      const c = rgb(interpolateViridis(i / 255))
      return [c.r / 255, c.g / 255, c.b / 255]
    }).flat())
    this.#viridis = device.createBuffer({ size: lut.byteLength, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST })
    device.queue.writeBuffer(this.#viridis, 0, lut)
  }

  // the tensor a picture paints from: its values, or a heatmap's spread over 0–1
  prepare(t: engine.Tensor, heat: boolean): engine.Tensor {
    const shown = heat ? spread(t) : t
    engine.materialize([shown])
    return shown
  }

  attach(slot: number, canvas: OffscreenCanvas): void {
    const context = canvas.getContext("webgpu") as GPUCanvasContext
    context.configure({ device: this.#device, format: this.#format, alphaMode: "opaque" })
    const picture = this.#device.createBuffer({ size: 16, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST })
    this.#canvases.set(slot, { canvas, context, picture })
  }

  detach(slot: number): void {
    const target = this.#canvases.get(slot)
    if (!target) { return }
    target.context.unconfigure()
    target.picture.destroy()
    this.#canvases.delete(slot)
  }

  // resolves once the GPU has painted it
  async paint(slot: number, t: engine.Tensor, heat: boolean): Promise<void> {
    const target = this.#canvases.get(slot)
    if (!target) { return }
    const [height = 1, width = 1, channels = 1] = t.shape
    if (target.canvas.width !== width || target.canvas.height !== height) {
      target.canvas.width = width
      target.canvas.height = height
    }
    const values = await t.gpuBuffer()
    this.#device.queue.writeBuffer(target.picture, 0, Uint32Array.of(width, heat ? 1 : channels, heat ? 1 : 0, 0))
    const bindings = this.#device.createBindGroup({
      layout: this.#pipeline.getBindGroupLayout(0),
      entries: [
        { binding: 0, resource: { buffer: values } },
        { binding: 1, resource: { buffer: this.#viridis } },
        { binding: 2, resource: { buffer: target.picture } },
      ],
    })
    const encoder = this.#device.createCommandEncoder()
    const pass = encoder.beginRenderPass({
      colorAttachments: [{ view: target.context.getCurrentTexture().createView(), loadOp: "clear", storeOp: "store", clearValue: [0, 0, 0, 1] }],
    })
    pass.setPipeline(this.#pipeline)
    pass.setBindGroup(0, bindings)
    pass.draw(3)
    pass.end()
    this.#device.queue.submit([encoder.finish()])
    this.pending++
    try {
      await this.#device.queue.onSubmittedWorkDone()
    } finally {
      this.pending--
    }
  }
}
