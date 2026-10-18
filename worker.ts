// The evaluation worker: it owns the engine and the GPU, and runs the host (host.ts).
// Messages that arrive while the GPU starts wait for it.

import * as engine from "./engine/tensor.ts"
import { startHost } from "./host.ts"
import type { ToPage, ToWorker } from "./protocol.ts"

const scope = globalThis as unknown as {
  onmessage: ((e: MessageEvent<ToWorker>) => void) | null,
  postMessage: (message: ToPage, transfer: Transferable[]) => void,
  addEventListener: (type: "error", listener: (e: ErrorEvent) => void) => void,
  dispatchEvent: (e: Event) => boolean,
}

const early: ToWorker[] = []
scope.onmessage = (e) => { early.push(e.data) }

// the engine computes on WebGPU where the platform has it, on the CPU everywhere else;
// a lost GPU ends this worker, and the page starts another
await engine.init()
engine.engineGpu()?.device.lost.then((info) => scope.postMessage({ t: "lost", message: info.message || info.reason }, []))
const receive = startHost((message, transfer) => scope.postMessage(message, transfer ?? []))
// Once it runs, an exception is logged and goes no further: the worker keeps running, and
// the page keeps what it shows. The waiting messages arrive as the others do, each its own
// event.
scope.addEventListener("error", (e) => { console.error(e.error ?? e.message); e.preventDefault() })
scope.onmessage = (e) => receive(e.data)
for (const data of early.splice(0)) { scope.dispatchEvent(new MessageEvent("message", { data })) }
