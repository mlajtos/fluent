// The page reading its ink by itself: ink/reader.fl and ink/hand.fl, with the weights
// page.py kept and the forms hand.py kept, in the worker that runs the program. Here is
// what stays outside Fluent – a line's ink into a shape, and what comes back into a line's
// text and the places its strokes go to. A line is read first, and formed when no line
// waits to be read: its text is not kept waiting for its form.

import { ask } from "./protocol"

export type Read = { text: string, of: number[], sure: number[] }
export type Places = ([number, number][] | null)[]
type Numbers = { shape: number[], values: Float32Array }

// A line's ink in a shape that few lines differ in – every shape is a plan the engine
// makes once: `strokes` [S, P, 2], a stroke filled up with its last point, and `real` [S],
// 1 for the line's strokes. S and P are the least of these that hold the line.
const STROKES = [32, 64, 128], POINTS = [256, 512, 1024]
export const inked = (strokes: number[][][]): { strokes: Numbers, real: Numbers } => {
  const S = STROKES.find((s) => s >= strokes.length), P = POINTS.find((p) => strokes.every((points) => p >= points.length))
  if (!S || !P) { throw new Error(`a line of ${strokes.length} strokes, the longest of ${Math.max(...strokes.map((points) => points.length))} points, is more than the reader takes`) }
  const ink = new Float32Array(S * P * 2), real = new Float32Array(S)
  strokes.forEach((points, s) => {
    real[s] = 1
    for (let p = 0; p < P; p++) { ink.set(points[Math.min(p, points.length - 1)]!, (s * P + p) * 2) }
  })
  return { strokes: { shape: [S, P, 2], values: ink }, real: { shape: [S], values: real } }
}

// What reader.fl's read says of a line, [S, 5], as the page has a line. A row is: which
// glyph the stroke is of; then, of the place: whether a glyph starts there, after a space,
// its code point, how sure
export const told = ({ values }: Numbers, strokes: number): Read => {
  const rows = Array.from({ length: values.length / 5 }, (_, row) => values.subarray(5 * row, 5 * row + 5)), glyphs = rows.filter((row) => row[1])
  return {
    text: glyphs.map((row) => (row[2] ? " " : "") + String.fromCodePoint(row[3]!)).join(""),
    of: rows.slice(0, strokes).map((row) => row[0]!),
    sure: glyphs.map((row) => Math.round(row[4]! * 1000) / 1000),
  }
}
// What hand.fl's formed says of it, [S, 1 + 2×N]: of each stroke whether it has a place in
// its glyph's form, and that place's N points
export const placed = ({ shape, values }: Numbers, strokes: number): Places => {
  const wide = shape[1]!
  return Array.from({ length: strokes }, (_, s) => values.subarray(wide * s, wide * (s + 1)))
    .map((row) => (row[0] ? Array.from({ length: (wide - 1) / 2 }, (_, p) => [Math.round(row[1 + 2 * p]! * 10) / 10, Math.round(row[2 + 2 * p]! * 10) / 10] as [number, number]) : null))
}

// the readers and the forms the server keeps go to the worker once, with the Fluent that
// reads with them – and once more to a worker that does not know them
let known: Promise<unknown> | null = null
const got = async (url: string): Promise<Response> => {
  const response = await fetch(url)
  if (!response.ok) { throw new Error(`${response.status} ${url}`) }
  return response
}
const know = async () => {
  const names = await (await got("readers")).json() as string[]
  const [reader, hand, forms, ...files] = await Promise.all([
    got("ink/reader.fl").then((r) => r.text()), got("ink/hand.fl").then((r) => r.text()),
    ...["forms", ...names.map((name) => `readers/${name}`)].map((url) => got(url).then((r) => r.arrayBuffer())),
  ]) as [string, string, ...ArrayBuffer[]]
  await ask(`${reader}${hand}says: read(files), places: formed(forms)`, { files, forms: forms! }, [forms!, ...files])
}
// One question at a time, a line to read before a line to form: how long each took says
// what it costs, not how many waited. The first line of a shape waits for the engine to
// plan it.
type Job = () => Promise<void>
const waiting: { read: Job[], form: Job[] } = { read: [], form: [] }
let busy = false
const next = async () => {
  if (busy) { return }
  busy = true
  for (let job; (job = waiting.read.shift() ?? waiting.form.shift());) { await job() }
  busy = false
}
// with an answer: how long it took here, in ms, and of that the worker's two parts – and
// how long the worker took to know the readers, when this question was the one to wait for it
type Timed = { ms: number, took: [evaluated: number, computed: number], knew?: number }
const answered = <T,>(kind: "read" | "form", question: () => Promise<T & { took: Timed["took"] }>): Promise<T & Timed> =>
  new Promise((said, failed) => {
    waiting[kind].push(async () => {
      try {
        const first = !known, before = performance.now()
        await (known ??= know())
        const began = performance.now(), answer = await question()
        said({ ...answer, ms: performance.now() - began, ...(first && { knew: began - before }) })
      } catch (e) {
        known = null
        failed(e)
      }
    })
    void next()
  })
// a line's text and, of it, what its forming is asked with
export const read = (strokes: number[][][]) => answered("read", async () => {
  const ink = inked(strokes), said = await ask("says(strokes, real)", ink)
  if (!said) { throw new Error("the reader said no numbers") }
  return { ...told(said, strokes.length), took: said.took, shape: ink.strokes.shape, form: () => answered("form", async () => {
    const places = await ask("places(said, strokes, real)", { said: { shape: said.shape, values: said.values }, ...ink })
    if (!places) { throw new Error("the forms said no numbers") }
    return { to: placed(places, strokes.length), took: places.took }
  }) }
})
