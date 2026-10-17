import { serve } from "bun";
import index from "./index.html";

const javascript = (body: BodyInit) => new Response(body, { headers: { "Content-Type": "application/javascript" } });

// Monaco's editor worker, bundled once
const { outputs: [monacoWorker] } = await Bun.build({
  entrypoints: [Bun.fileURLToPath(import.meta.resolve("monaco-editor/esm/vs/editor/editor.worker.js"))],
});
if (!monacoWorker) throw new Error("Failed to bundle Monaco worker");

// The evaluation worker, bundled on every request: a reload runs the current evaluator
const evaluationWorker = async () => {
  const { success, logs, outputs: [bundle] } = await Bun.build({ entrypoints: ["./worker.ts"], target: "browser", format: "esm", sourcemap: "inline" });
  return javascript(success && bundle ? bundle : `throw new Error(${JSON.stringify(logs.map(String).join("\n"))})`);
};

const server = serve({
  routes: {
    "/": index,
    "/monaco.worker.js": javascript(monacoWorker),
    "/fluent.worker.js": evaluationWorker,
    // dataset for the MNIST example (build.ts copies it into dist/ for prod)
    "/mnist.safetensors": () => new Response(Bun.file("./mnist.safetensors")),
    // symbol font for the editor and AST viz (build.ts copies it for prod)
    "/BQN386.ttf": () => new Response(Bun.file("./BQN386.ttf")),
    // CORS proxy for LoadTensorFromImageUrl / LoadSafeTensorFromURL. It
    // fetches whatever it is handed, so it is confined to what it is for:
    // http(s) only, or `file:` and `data:` would make it an arbitrary local
    // file reader, and the socket below is loopback-only so it is not a relay
    // for the rest of the network.
    "/proxy": async (req: Request) => {
      const target = new URL(req.url).searchParams.get("url");
      if (!target) return new Response("missing ?url=", { status: 400 });
      let scheme: string
      try { scheme = new URL(target).protocol } catch { return new Response("bad ?url=", { status: 400 }) }
      if (scheme !== "http:" && scheme !== "https:") {
        return new Response(`refusing to proxy ${scheme}`, { status: 403 });
      }
      const upstream = await fetch(target);
      return new Response(upstream.body, {
        status: upstream.status,
        headers: { "Content-Type": upstream.headers.get("Content-Type") ?? "application/octet-stream" },
      });
    },
  },
  // the message below says localhost; without this the socket is *:PORT and
  // the proxy above is reachable from the whole network
  hostname: "127.0.0.1",
  development: true,
});

console.log(`Listening on ${server.url}`);
