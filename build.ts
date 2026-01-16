import { rm } from "node:fs/promises";
import tailwind from "bun-plugin-tailwind";

const outdir = "./dist";

// Clean output directory
await rm(outdir, { recursive: true, force: true });

// Bundle main app with Tailwind CSS processing
const appBuild = await Bun.build({
  entrypoints: ["./index.html"],
  outdir,
  minify: true,
  sourcemap: "linked",
  plugins: [tailwind],
});

if (!appBuild.success) {
  console.error("App build failed:");
  for (const log of appBuild.logs) {
    console.error(log);
  }
  process.exit(1);
}

// Bundle Monaco worker
const monacoWorkerPath = Bun.fileURLToPath(
  import.meta.resolve("monaco-editor/esm/vs/editor/editor.worker.js")
);
const workerBuild = await Bun.build({
  entrypoints: [monacoWorkerPath],
  outdir,
  minify: true,
  naming: "monaco.worker.js",
});

if (!workerBuild.success) {
  console.error("Monaco worker build failed:");
  for (const log of workerBuild.logs) {
    console.error(log);
  }
  process.exit(1);
}

console.log("Build complete!");
console.log(`Output: ${outdir}`);
for (const output of [...appBuild.outputs, ...workerBuild.outputs]) {
  const size = (output.size / 1024).toFixed(1);
  console.log(`  ${output.path.replace(process.cwd(), ".")} (${size} KB)`);
}
