// What the parser makes of texts, for the tests of the readers of lines. Texts come
// on stdin, one a line; of each, a line of JSON goes out: a number that two texts share
// when their syntax trees are the same, and of each place between two glyphs whether it
// matters – whether a space put there, or the space taken from there, changes the tree.
//   bun ink/trees.ts < texts
import { CodeParse } from "../language"

const tree = (text: string): string => {
  try { return Bun.hash(JSON.stringify(CodeParse(text), (key, value) => key === "origin" ? undefined : value)).toString(16) }
  catch (error) { return `error: ${error}` }
}

for (const text of (await Bun.stdin.text()).split("\n")) {
  const whole = tree(text), glyphs = [...text], matters: boolean[] = []
  for (let i = 1; i < glyphs.length; i++) {
    if (glyphs[i] === " " || glyphs.slice(0, i).every(glyph => glyph === " ")) continue
    const spaced = glyphs[i - 1] === " "
    matters.push(tree([...glyphs.slice(0, spaced ? i - 1 : i), ...(spaced ? [] : [" "]), ...glyphs.slice(i)].join("")) !== whole)
  }
  console.log(JSON.stringify({ tree: whole, matters }))
}
