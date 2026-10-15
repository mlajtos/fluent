// The evaluation worker: it owns the engine and the GPU, and runs the host (host.ts).
// Messages that arrive while the GPU starts wait for it.

import * as engine from "./engine/tensor.ts"
import { startHost } from "./host.ts"
import type { ToPage, ToWorker } from "./protocol.ts"

const scope = globalThis as unknown as {
  onmessage: ((e: MessageEvent<ToWorker>) => void) | null,
  postMessage: (message: ToPage, transfer: Transferable[]) => void,
}

const early: ToWorker[] = []
scope.onmessage = (e) => { early.push(e.data) }

// the engine computes on WebGPU where the platform has it, on the CPU everywhere else
await engine.init()
const receive = startHost((message, transfer) => scope.postMessage(message, transfer ?? []))
scope.onmessage = (e) => receive(e.data)
for (const message of early.splice(0)) { receive(message) }
