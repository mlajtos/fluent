import { serve } from "bun";
import index from "./index.html";

// Bundle Monaco editor worker for web worker context
const monacoWorkerPath = Bun.fileURLToPath(import.meta.resolve("monaco-editor/esm/vs/editor/editor.worker.js"));
const { outputs: [monacoWorker] } = await Bun.build({
  entrypoints: [monacoWorkerPath],
});

if (!monacoWorker) throw new Error("Failed to bundle Monaco worker");

const server = serve({
  routes: {
    "/": index,
    "/monaco.worker.js": new Response(monacoWorker, {
      headers: { "Content-Type": "application/javascript" },
    }),
  },
  development: true,
});

console.log(`Listening on ${server.url}`);
