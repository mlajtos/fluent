import { rm } from "node:fs/promises";
import tailwind from "bun-plugin-tailwind";

const outdir = "./dist";
await rm(outdir, { recursive: true, force: true });

// One bundle, or the build stops with its logs
const bundle = async (what: string, options: Parameters<typeof Bun.build>[0]) => {
  const built = await Bun.build({ outdir, minify: true, ...options });
  if (!built.success) {
    console.error(`${what} build failed:`);
    for (const log of built.logs) { console.error(log); }
    process.exit(1);
  }
  return built.outputs;
};
const fileName = (output: { path: string }) => output.path.split("/").pop()!;

// The workers are named by their content, and the page learns the names from the build: a
// page cached from one deploy never starts a worker from another
const monacoWorker = await bundle("Monaco worker", {
  entrypoints: [Bun.fileURLToPath(import.meta.resolve("monaco-editor/esm/vs/editor/editor.worker.js"))],
  naming: "monaco.worker-[hash].[ext]",
});
const evaluator = await bundle("Evaluation worker", {
  entrypoints: ["./worker.ts"], target: "browser", format: "esm", naming: "fluent.worker-[hash].[ext]",
});
const app = await bundle("App", {
  entrypoints: ["./index.html"],
  sourcemap: "linked",
  plugins: [tailwind],
  define: { MONACO_WORKER: JSON.stringify(fileName(monacoWorker[0]!)), FLUENT_WORKER: JSON.stringify(fileName(evaluator[0]!)) },
});

// Copy static datasets the examples fetch at runtime (the deployed site is
// static – no /mnist.safetensors route like the dev server has)
await Bun.write(`${outdir}/mnist.safetensors`, Bun.file("./mnist.safetensors"));
await Bun.write(`${outdir}/BQN386.ttf`, Bun.file("./BQN386.ttf"));

console.log("Build complete!");
console.log(`Output: ${outdir}`);
for (const output of [...app, ...monacoWorker, ...evaluator]) {
  console.log(`  ${output.path.replace(process.cwd(), ".")} (${(output.size / 1024).toFixed(1)} KB)`);
}
