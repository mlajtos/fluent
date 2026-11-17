// What the copybook asks a pen to write (copybook.tsx, the dev server's /copybook): the
// alphabet, a family a page; glyphs the alphabet does not have yet, to see how the hand
// makes them; and lines of Fluent – those written, and the next to write, picked for what
// the readers of this hand would learn from them.

import { existsSync, readdirSync, readFileSync } from "node:fs"
import { join } from "node:path"
import { stringRegexp } from "./protocol"
import { CodeParse, type SyntaxTreeNode } from "./language"

// The pen's alphabet: the keyboard's characters, the Greek letters, and the glyphs – every
// other character the Prelude's code is written in
const GREEK = /[Α-Ωα-ω]/
const PRELUDE = readFileSync(join(import.meta.dir, "prelude.fl"), "utf8")
const BARE = PRELUDE.replace(new RegExp(stringRegexp.source, "g"), '""').replace(/;[^\n]*/g, "")
const CODE = [...new Set(BARE)].sort()
const GLYPHS = CODE.filter((c) => c > "~" && !GREEK.test(c))

// The alphabet as the copybook has it (copybook.tsx), a family a page. The keyboard's signs
// are the ones the Prelude's code uses, with the two that open its strings and its comments.
// A Greek capital the Latin alphabet has in the same shape is the Latin one.
const ALPHABET = [
  { name: "digits", glyphs: [..."0123456789"] },
  { name: "small letters", glyphs: [..."abcdefghijklmnopqrstuvwxyz"] },
  { name: "capitals", glyphs: [..."ABCDEFGHIJKLMNOPQRSTUVWXYZ"] },
  { name: "signs", glyphs: [...new Set([...CODE.filter((c) => c > " " && c <= "~" && !/[A-Za-z0-9]/.test(c)), '"', ";"])].sort() },
  { name: "glyphs", glyphs: GLYPHS },
  { name: "Greek", glyphs: [..."αβγδεζηθικλμνξοπρστυφχψωΓΔΘΛΞΠΣΦΨΩ"] },
]

// What a pen writes another way: × is an x to a hand, so `*` multiplies, and the dice, the
// stencil and the double-struck letters go by their names. A pen has one rho, the sign ⍴: a
// text with the letter ρ is not for it.
const PEN: Record<string, string> = { "×": "*", "⚄": "rand", "⌺": "moving", "𝕀": "eye", "𝕍": "variance", "𝕐": "fix" }
const penned = (text: string) => text.replace(/[×⚄⌺𝕀𝕍𝕐]/gu, (glyph) => PEN[glyph]!)
const unpenned = /ρ/

// The lines the copybook can dictate: expressions of the Prelude – of its code, and of the
// usage and the example on each of its doc cards, where a · stands between two of them –
// as a pen writes their source, spaces and all, each short enough for a line of the page. A
// hand writes one width of gap, so a line has no two spaces in a row. `count` lines are
// asked for at a time.
const LINE = { shortest: 3, longest: 22, count: 40 }
const expressions = (code: string) => {
  const written = new Set([...ALPHABET.flatMap(({ glyphs }) => glyphs), " "]), found = new Set<string>()
  const walk = (node: SyntaxTreeNode) => {
    if (node.type === "Program") { node.content.forEach(walk) }
    if (node.type === "Tensor" || node.type === "List") { node.content.value.forEach(walk) }
    if (node.type === "Lambda") { walk(node.content.expr) }
    if (node.type === "Code") { walk(node.content.value) }
    if (node.type === "Operation") {
      const { operator, args } = node.content
      walk(args)
      if (operator.type === "Symbol" && operator.content.value === "doc") {
        for (const told of [args.content.value[1], args.content.value[3]]) { if (told?.type === "String") { told.content.value.split(/\s+·\s+/).forEach((part) => { walk(CodeParse(part)) }) } }
      }
    }
    if (node.type === "Operation" || node.type === "Lambda" || node.type === "Tensor" || (node.type === "List" && !node.content.synthetic)) {
      const source = node.origin.source.trim(), text = penned(source), size = [...text].length
      if (!unpenned.test(source) && size >= LINE.shortest && size <= LINE.longest && !text.includes("  ") && [...text].every((c) => written.has(c))) { found.add(text) }
    }
  }
  walk(CodeParse(code))
  return found
}
// What the readers made of the lines they had not learned from (ink/page.py): those lines,
// and of each glyph how many were in them and how many of those they misread
const MISREAD = join(import.meta.dir, "paper/ink/misread.json")
const misread: { lines: string[], seen: Record<string, number>, wrong: Record<string, number> } =
  existsSync(MISREAD) ? JSON.parse(readFileSync(MISREAD, "utf8")) : { lines: [], seen: {}, wrong: {} }
// how likely the reader is to misread a glyph in a line: a coin's toss for one it saw in none
const doubt = (glyph: string) => ((misread.wrong[glyph] ?? 0) + 1) / ((misread.seen[glyph] ?? 0) + 2)

// A canonical name – ListMap, TensorSpectrumTurn – is many letters for a hand and little for
// the reader: no line asked for has one.
const canonical = /\b[A-Z][a-z]+[A-Z]/

// How the reader learns a glyph, as measured on the cells and on the lines alike: its
// misreadings of the glyph fall to 0.61 of what they were as the glyph's writings double –
// to ((n + k) / n)^-LEARNS of them after k more writings on top of n. The copybook's cells
// are the first CELLS of the n.
const LEARNS = 0.71, CELLS = 10
const tally = (text: string, into = new Map<string, number>()) => {
  for (const glyph of text) { if (glyph !== " ") { into.set(glyph, (into.get(glyph) ?? 0) + 1) } }
  return into
}
const words = (text: string) => text.match(/[A-Za-z]{2,}/g) ?? []

// The lines of the copybook: every line that is written, and `count` more to write, the
// one worth the most first – a pen that stops early has written the best of them. A line
// is worth the misreadings of real code it takes away, a glyph written: a glyph counts as
// often as the Prelude's code has it and as the reader misreads it, and less with each
// writing it has had, in the lines written and in the lines asked for before this one. No
// line has a word that a line asked for before it has, or a part of one: to the reader a
// word written again is worth what a new one is, and to the hand it is the same line again.
// A line whose ink was taken away is one to write again: those are asked for first.
const lines = (() => {
  const book = join(import.meta.dir, "paper/copybook")
  const kept = (existsSync(book) ? readdirSync(book) : []).filter((name) => /^line-.*\.json$/.test(name))
    .map((name) => JSON.parse(readFileSync(join(book, name), "utf8")) as { glyph: string, samples: ({ strokes: { at: number }[] } | null)[] })
  const written = kept.flatMap(({ glyph, samples }) => (samples[0]?.strokes[0] ? [{ glyph, at: samples[0].strokes[0].at }] : []))
    .sort((a, b) => a.at - b.at).map(({ glyph }) => glyph)
  const again = kept.filter(({ samples }) => !samples[0]?.strokes[0]).map(({ glyph }) => glyph)
  const often = tally(penned(BARE).replace(/ρ/g, "⍴").replace(/\s/g, "")), had = tally(written.join(""))
  // a glyph's part of the code's misreadings after `more` writings of it
  const part = (glyph: string, more: number) => (often.get(glyph) ?? 0) * doubt(glyph) * (1 + more / (CELLS + (had.get(glyph) ?? 0))) ** -LEARNS
  const rest = [...expressions(PRELUDE)].filter((text) => !written.includes(text) && !again.includes(text) && !canonical.test(text))
  const asked = [...again], more = again.reduce((into, text) => tally(text, into), new Map<string, number>()), used = again.flatMap(words)
  const worth = (text: string) => {
    const kinds = [...tally(text)]
    return kinds.reduce((sum, [glyph, n]) => sum + part(glyph, more.get(glyph) ?? 0) - part(glyph, (more.get(glyph) ?? 0) + n), 0) / kinds.reduce((sum, [, n]) => sum + n, 0)
  }
  const fresh = (text: string) => words(text).every((word) => used.every((other) => !word.includes(other) && !other.includes(word)))
  while (asked.length < LINE.count) {
    const free = rest.filter(fresh)
    if (free.length === 0) { break }
    const best = free.reduce((a, b) => (worth(b) > worth(a) ? b : a))
    rest.splice(rest.indexOf(best), 1)
    asked.push(best)
    tally(best, more)
    used.push(...words(best))
  }
  return [...written, ...asked]
})()

// Glyphs the alphabet does not have, written to see how the hand makes them and what the
// reader takes them for. ⊙ is a ◌ of two strokes, a circle and one dot. The seven after it
// are made of two the hand writes, a square or a circle with a sign in it. ⌋ and ⌉ are the
// right-hand floor and ceiling, to see whether a hand tells them from ) better than it
// tells ⌊ from (.
export const CANDIDATES = [..."□?⊥∞→↑↓&⋈⌂⊙⊡⊞⊟⊠⍰⊚⊕⌋⌉"]

// What the copybook asks for: `cells` samples of each text of a family – a glyph, or a line
export const families = [...ALPHABET.map((family) => ({ ...family, cells: 10 })), { name: "candidates", glyphs: CANDIDATES, cells: 10 }, { name: "lines", glyphs: lines, cells: 1 }]
