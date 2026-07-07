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
    // dataset for the MNIST example (build.ts copies it into dist/ for prod)
    "/mnist.safetensors": () => new Response(Bun.file("./mnist.safetensors")),
    // CORS proxy for LoadTensorFromImageUrl / LoadSafeTensorFromURL
    "/proxy": async (req: Request) => {
      const target = new URL(req.url).searchParams.get("url");
      if (!target) return new Response("missing ?url=", { status: 400 });
      const upstream = await fetch(target);
      return new Response(upstream.body, {
        status: upstream.status,
        headers: { "Content-Type": upstream.headers.get("Content-Type") ?? "application/octet-stream" },
      });
    },
  },
  development: true,
});

console.log(`Listening on ${server.url}`);
