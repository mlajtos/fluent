// Run the engine's test files in headless Chrome with WebGPU — the browser is the target,
// and only the browser enforces its limits.
//   bun engine/tests/run.ts                   every *.ts in engine/tests except this runner
//   bun engine/tests/run.ts engine/tests/core.ts
// Each test file default-exports `async () => ({ fails })` and logs with console.log.
import { chromium } from "playwright"
import { readdirSync, rmSync } from "node:fs"
import { basename, join, resolve } from "node:path"
import { tmpdir } from "node:os"

const here = import.meta.dir
const files = (process.argv.length > 2 ? process.argv.slice(2) : readdirSync(here).filter((f) => f.endsWith(".ts") && f !== "run.ts" && !f.endsWith(".bun.ts") && !f.startsWith("bench")).map((f) => join(here, f)))
  .map((f) => resolve(f))
const outdir = join(tmpdir(), `engine-tests-${process.pid}`)
const built = await Bun.build({ entrypoints: files, outdir, target: "browser", format: "esm", naming: "[name].js" })
if (!built.success) { for (const m of built.logs) console.error(m); process.exit(1) }

const server = Bun.serve({
  port: 0,
  fetch: (req) => {
    const path = new URL(req.url).pathname
    if (path === "/") return new Response("<!doctype html><meta charset=utf-8><title>engine tests</title>", { headers: { "content-type": "text/html" } })
    const file = Bun.file(join(outdir, basename(path)))
    return file.size ? new Response(file, { headers: { "content-type": "text/javascript" } }) : new Response(null, { status: 404 })
  },
})

const browser = await chromium.launch({ channel: "chrome", headless: true, args: (process.env.CHROME_ARGS ?? "").split(" ").filter(Boolean) })
let failed = 0
try {
  for (const file of files) {
    const name = basename(file, ".ts")
    const page = await browser.newPage()
    page.on("console", (m) => console.log(m.text()))
    page.on("pageerror", (e) => console.error(`${name}: ${e.message}`))
    await page.goto(`http://localhost:${server.port}/${process.env.QUERY ?? ""}`)
    const { fails, gpu } = await page.evaluate(async (n) => {
      const adapter = await (navigator as any).gpu?.requestAdapter()
      const run = (await import(`/${n}.js`)).default as () => Promise<{ fails: number }>
      return { ...(await run()), gpu: adapter ? `${adapter.info?.vendor ?? ""} ${adapter.info?.architecture ?? ""}`.trim() || "yes" : null }
    }, name)
    console.log(`${name}: ${fails === 0 ? "PASS" : `FAIL (${fails})`}  [WebGPU: ${gpu ?? "none"}]\n`)
    if (!gpu) { console.error(`${name}: no WebGPU adapter — the GPU half did not run`); failed++ }
    failed += fails
    await page.close()
  }
} finally {
  await browser.close()
  server.stop()
  rmSync(outdir, { recursive: true, force: true })
}
process.exit(failed ? 1 : 0)
