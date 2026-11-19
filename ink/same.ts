// reader.fl and hand.fl beside page.py and hand.py: the lines of a canvas read and formed
// by both, on the engine this process has – the CPU, where a line takes seconds – and by
// the readers the paper's server keeps.
//   bun ink/same.ts [paper/canvases/1.json] [how many lines]
import { readFileSync, readdirSync } from "node:fs"
import { CodeParse, evaluateSyntaxTreeNode, evaluateGeneration, createScope, engine, type Value } from "../language"
import { inked, told, placed, type Read, type Places } from "../readers"

const HERE = import.meta.dir
const [file = `${HERE}/../paper/canvases/1.json`, many = "Infinity"] = process.argv.slice(2)

// what page.py kept of a reader (host.ts, parseSafetensors): floats, and the alphabet's code points
const kept = (path: string): [string, engine.Tensor][] => {
  const bytes = new Uint8Array(readFileSync(path)), body = 8 + Number(new DataView(bytes.buffer).getBigUint64(0, true))
  const header = JSON.parse(new TextDecoder().decode(bytes.subarray(8, body))) as Record<string, { dtype: string, shape: number[], data_offsets: [number, number] }>
  return Object.entries(header).filter(([name]) => name !== "__metadata__").map(([name, { dtype, shape, data_offsets: [from, to] }]) => {
    const raw = bytes.slice(body + from, body + to).buffer
    return [name, engine.tensor(dtype === "I32" ? Float32Array.from(new Int32Array(raw)) : new Float32Array(raw), { shape })]
  })
}

const scope = createScope()
const run = (source: string) => evaluateGeneration(() => evaluateSyntaxTreeNode(CodeParse(source), scope))
scope.files = readdirSync(`${HERE}/../paper/ink`).filter((name) => /^canvas-\d+\.safetensors$/.test(name)).sort().map((name) => kept(`${HERE}/../paper/ink/${name}`)) as Value
scope.forms = kept(`${HERE}/../paper/ink/forms.safetensors`) as Value
run(readFileSync(`${HERE}/reader.fl`, "utf8") + readFileSync(`${HERE}/hand.fl`, "utf8") + "says: read(files), places: formed(forms)")

const lines = (JSON.parse(readFileSync(file, "utf8")).lines as { strokes: { points: number[][] }[] }[]).filter(({ strokes }) => strokes.length).slice(0, Number(many))
let same = 0
for (const line of lines) {
  const strokes = line.strokes.map(({ points }) => points.map(([x, y]) => [x!, y!]))
  for (const [name, { shape, values }] of Object.entries(inked(strokes))) { scope[name] = engine.tensor(values, { shape }) }
  const began = performance.now()
  const numbers = (source: string) => { const value = run(source) as engine.Tensor; return { shape: [...value.shape], values: Float32Array.from((value.js() as number[][]).flat()) } }
  const said = numbers("says(strokes, real)")
  scope.said = engine.tensor(said.values, { shape: said.shape })
  const mine = { ...told(said, strokes.length), to: placed(numbers("places(said, strokes, real)"), strokes.length) }
  const took = performance.now() - began
  const theirs = await (await fetch("https://localhost:3210/reading", { method: "POST", body: JSON.stringify(strokes), tls: { rejectUnauthorized: false } } as RequestInit)).json() as Read & { to: Places }
  const far = Math.max(0, ...mine.sure.map((sure, g) => Math.abs(sure - (theirs.sure[g] ?? Infinity))))
  // a place is the same within a point's rounding, a tenth of a unit
  const off = Math.max(0, ...mine.to.map((to, s) => (!to !== !theirs.to[s] ? Infinity : Math.max(0, ...(to ?? []).flatMap(([x, y], p) => [Math.abs(x - theirs.to[s]![p]![0]), Math.abs(y - theirs.to[s]![p]![1])])))))
  const as = mine.text === theirs.text && String(mine.of) === String(theirs.of) && far < 0.002 && off < 0.11
  same += Number(as)
  console.log(`${as ? "same" : "OTHER"}  ${(took / 1000).toFixed(1)} s  ${JSON.stringify(mine.text)}${as ? "" : `  page.py: ${JSON.stringify(theirs.text)}, sure ${far.toFixed(3)} apart, places ${off.toFixed(2)} apart`}`)
}
console.log(`${same} of ${lines.length} lines read as page.py reads them: the text, the strokes of each glyph, how sure, and where the strokes go`)
