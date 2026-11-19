// How long the readers took, from the pen's record (paper/pen.jsonl): of each time a page
// was opened, who read – the page itself or the server – on what, and the middle and the
// slow end of what a line took to read and to form, in ms. The first line of a shape waits
// for the engine's plan, so it is counted apart.
//   bun ink/took.ts [how many of the last openings = 8] [the record = paper/pen.jsonl]
import { readFileSync } from "node:fs"

type Noted = { at: number, on: string, type: string, by?: string, agent?: string, touches?: number, gpu?: boolean, ms?: number, evaluated?: number, computed?: number, knew?: number, shape?: number[] }
const noted = readFileSync(process.argv[3] ?? `${import.meta.dir}/../paper/pen.jsonl`, "utf8").split("\n").filter(Boolean).map((line) => JSON.parse(line) as Noted).filter(({ on }) => on === "readers")

// the openings, each with what was read after it
const openings: { open: Noted, after: Noted[] }[] = []
for (const event of noted) {
  if (event.type === "open") { openings.push({ open: event, after: [] }) } else { openings.at(-1)?.after.push(event) }
}
// an iPad's Safari says it is a Mac's: it is the one with fingers
const device = ({ agent = "", touches = 0 }: Noted) => (/iPad|iPhone/.test(agent) || (/Macintosh/.test(agent) && touches > 1) ? "iPad" : /Macintosh/.test(agent) ? "Mac" : "?")
  + (/Chrome/.test(agent) ? " Chrome" : agent.match(/Version\/([\d.]+)/) ? ` Safari ${agent.match(/Version\/([\d.]+)/)![1]}` : "")
const at = (sorted: number[], part: number) => Math.round(sorted[Math.min(sorted.length - 1, Math.floor(part * sorted.length))]!)
const said = (label: string, all: number[]) => {
  const sorted = [...all].sort((a, b) => a - b)
  return sorted.length ? `${label} ${at(sorted, 0.5)} (${at(sorted, 0.9)})` : ""
}

for (const { open, after } of openings.slice(-Number(process.argv[2] ?? 8))) {
  if (!after.length) { continue }
  const first = new Set<string>(), reads = after.filter(({ type }) => type === "read"), forms = after.filter(({ type }) => type === "formed")
  // a line that waited for a plan: the first of its shape, and the first to form
  const planned = reads.filter(({ shape }) => { const key = String(shape); const is = !first.has(key); first.add(key); return is && shape !== undefined })
  const warm = reads.filter((read) => !planned.includes(read)), ms = (events: Noted[], of: "ms" | "evaluated" | "computed") => events.flatMap((event) => (event[of] === undefined ? [] : [event[of]]))
  console.log(`${new Date(open.at).toLocaleString("sv")}  ${device(open)}${open.gpu ? "" : ", no WebGPU"}, read by the ${open.by}: ${reads.length} lines`)
  console.log("  " + [
    said("read", ms(warm, "ms")), said("– evaluated", ms(warm, "evaluated")), said("computed", ms(warm, "computed")),
    said("· formed", ms(forms.slice(1), "ms")), said("– evaluated", ms(forms.slice(1), "evaluated")), said("computed", ms(forms.slice(1), "computed")),
  ].filter(Boolean).join(" ") + "   ms, the middle (and 9 in 10 under)")
  if (planned.length) { console.log(`  with a plan to make: ${planned.map(({ ms, shape }) => `${Math.round(ms!)} ms [${shape}]`).join(", ")}${reads[0]?.knew !== undefined ? `; the readers known in ${Math.round(reads[0].knew)} ms` : ""}`) }
}
