// The hand's form: the copybook's lines as the pen wrote them, and as their reader writes
// them back – each glyph it read, in the mean of that glyph's cells, where the pen wrote it
// (ink/hand.py; the dev server gives its file at /hand). One slider takes every line from
// the one ink to the other.

import { effect, signal } from "@preact/signals-core"
import { useSignals } from "@preact/signals-react/runtime"
import { memo, useEffect, useRef, type JSX } from "react"
import { CELL, PEN } from "./ink"

// a point of a line: x from the line's left edge and y from its ruled baseline, down, in
// CSS pixels, and the pen's pressure there, 0 to 1
type Point = [x: number, y: number, pressure: number]
// A glyph the reader found: its name, the ink the pen wrote and the ink of its form. With
// `morph` the two have the same strokes of as many points, and a point goes to its form;
// without, the one ink fades to the other.
type Glyph = { glyph: string, morph: boolean, written: Point[][], form: Point[][] }
// `read` is what the reader made of the ink with no text, and `right` whether that parses
// to the tree of `text`, the line that was dictated
type Line = { text: string, read: string, right: boolean, glyphs: Glyph[] }

const lines = signal<Line[]>([])
// how far the ink is from the pen's to the form's, 0 to 1
const formed = signal(0)
// the width the lines have, and the width their ink needs, in CSS pixels
const room = signal(0)
const wide = signal(1)
// why there is no hand to show
const problem = signal("")

// the height of a line's print, above its ink
const PRINT = 28
// the page's ground and its ink, as red, green and blue
const GROUND = [23, 23, 23], INK = [255, 255, 255]
// a pen pressed as hard as this hand's median, 0.17, draws the copybook's width
const widthOf = (pressure: number) => PEN * (0.55 + 2.6 * pressure)

const load = async () => {
  const answer = await fetch("/hand").catch(() => null)
  if (!answer?.ok) { problem.value = (await answer?.text()) ?? "the server is not there"; return }
  const hand = (await answer.json() as { lines: Line[] }).lines
  let right = 1
  for (const { glyphs } of hand) { for (const { written } of glyphs) { for (const stroke of written) { for (const [x] of stroke) { right = Math.max(right, x) } } } }
  wide.value = 16 + right
  lines.value = hand
}

// A stroke, as dark as `shade` says, 0 to 1. A faint stroke is nearer the ground's color
// and as opaque as a dark one: where its segments meet they do not add up.
const trace = (ink: CanvasRenderingContext2D, points: Point[], shade: number) => {
  if (shade <= 0) { return }
  ink.strokeStyle = `rgb(${GROUND.map((ground, i) => Math.round(ground + shade * (INK[i]! - ground))).join(" ")})`
  for (let i = Math.min(1, points.length - 1); i < points.length; i++) {
    const from = points[Math.max(0, i - 1)]!, to = points[i]!
    ink.lineWidth = widthOf((from[2] + to[2]) / 2)
    ink.beginPath()
    ink.moveTo(from[0], from[1])
    ink.lineTo(to[0], to[1])
    ink.stroke()
  }
}

const draw = (canvas: HTMLCanvasElement, { glyphs }: Line, t: number, zoom: number) => {
  const dots = window.devicePixelRatio * zoom, width = Math.round(canvas.clientWidth * window.devicePixelRatio), height = Math.round(CELL.height * dots)
  if (canvas.width !== width || canvas.height !== height) { canvas.width = width; canvas.height = height }
  const ink = canvas.getContext("2d")!
  ink.setTransform(1, 0, 0, 1, 0, 0)
  ink.clearRect(0, 0, width, height)
  ink.setTransform(dots, 0, 0, dots, 0, CELL.baseline * dots)
  ink.lineCap = "round"
  ink.strokeStyle = "rgb(64 64 64)"
  ink.lineWidth = 1
  ink.beginPath()
  ink.moveTo(0, 0)
  ink.lineTo(width / dots, 0)
  ink.stroke()
  for (const { morph, written, form } of glyphs) {
    if (morph) {
      for (const [k, stroke] of written.entries()) {
        trace(ink, stroke.map((point, i) => point.map((from, at) => from + t * (form[k]![i]![at]! - from)) as Point), 1)
      }
      continue
    }
    // the fainter of the two inks is under the other
    for (const [strokes, shade] of (t < 0.5 ? [[form, t], [written, 1 - t]] : [[written, 1 - t], [form, t]]) as [Point[][], number][]) {
      for (const stroke of strokes) { trace(ink, stroke, shade) }
    }
  }
}

// A line's ink is drawn while the line is in view
const Row = memo(function Row({ line }: { line: Line }): JSX.Element {
  useSignals()
  const canvas = useRef<HTMLCanvasElement>(null)
  const zoom = Math.min(1, room.value / wide.value)
  useEffect(() => {
    const seen = signal(false)
    const watch = new IntersectionObserver(([entry]) => { seen.value = entry!.isIntersecting })
    watch.observe(canvas.current!)
    const stop = effect(() => { if (seen.value && room.value > 0) { draw(canvas.current!, line, formed.value, Math.min(1, room.value / wide.value)) } })
    return () => { watch.disconnect(); stop() }
  }, [line])
  return (
    <div className="border-b border-neutral-800">
      <div className="font-mono text-lg whitespace-pre overflow-hidden" style={{ height: PRINT, lineHeight: `${PRINT}px` }}>
        <span className={line.right ? "text-neutral-400" : "text-red-400"}>{line.read}</span>
        {!line.right && <span className="text-neutral-600">{"    "}{line.text}</span>}
      </div>
      <canvas ref={canvas} className="block w-full" style={{ height: CELL.height * zoom }} />
    </div>
  )
})

export function Hand(): JSX.Element {
  useSignals()
  const rows = useRef<HTMLDivElement>(null)
  useEffect(() => {
    const sized = new ResizeObserver(([entry]) => { room.value = entry!.contentRect.width })
    sized.observe(rows.current!)
    void load()
    return () => { sized.disconnect() }
  }, [])
  return (
    <div className="fixed inset-0 flex flex-col bg-neutral-900 text-white select-none" style={{ WebkitUserSelect: "none", WebkitTouchCallout: "none" }}>
      <div className="flex items-center gap-x-4 px-4 py-2 border-b border-neutral-800 text-sm">
        <span>as written</span>
        <input
          type="range" min={0} max={1} step={0.001} defaultValue={0} className="flex-1 h-8 accent-white"
          onInput={(e) => { formed.value = Number(e.currentTarget.value) }}
        />
        <span>form</span>
        <span className="text-xs tabular-nums text-neutral-500">{lines.value.filter(({ right }) => right).length} of {lines.value.length} lines read to their text's tree</span>
      </div>
      {problem.value && <div className="p-4 text-red-500 text-sm">{problem.value}</div>}
      <div className="flex-1 min-h-0 overflow-y-auto px-4">
        <div ref={rows}>{lines.value.map((line) => <Row key={line.text} line={line} />)}</div>
      </div>
    </div>
  )
}
