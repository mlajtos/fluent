// The index simplifier against the expressions it simplifies: random view-like index
// expressions (sums of scaled ranges, divided and reduced, nested), built once through the
// simplifying constructors and once raw, must agree at every value their ranges take.
//   bun engine/tests/index.bun.ts
import { range, iconst, iadd, imul, idiv, imod, node, type UOp } from "../ir.ts"

let seed = 7
const rnd = (n: number) => ((seed = (seed * 1103515245 + 12345) >>> 0) % n)
const raw = { add: (a: UOp, b: UOp) => node("IADD", [a, b], null, []), mul: (a: UOp, b: UOp) => node("IMUL", [a, b], null, []), div: (a: UOp, b: UOp) => node("IDIV", [a, b], null, []), mod: (a: UOp, b: UOp) => node("IMOD", [a, b], null, []) }
const smart = { add: iadd, mul: imul, div: idiv, mod: imod }
const evalIdx = (u: UOp, env: Map<UOp, number>): number => {
  switch (u.op) {
    case "RANGE": return env.get(u)!
    case "ICONST": return u.arg as number
    case "IADD": return evalIdx(u.src[0]!, env) + evalIdx(u.src[1]!, env)
    case "IMUL": return evalIdx(u.src[0]!, env) * evalIdx(u.src[1]!, env)
    case "IDIV": return Math.floor(evalIdx(u.src[0]!, env) / evalIdx(u.src[1]!, env))
    case "IMOD": return evalIdx(u.src[0]!, env) % evalIdx(u.src[1]!, env)
  }
  throw new Error(u.op)
}
let fails = 0, cases = 0, smaller = 0
const size = (u: UOp): number => 1 + u.src.reduce((a, s) => a + size(s), 0)
for (let t = 0; t < 3000; t++) {
  const rs = [range(1 + rnd(6)), range(1 + rnd(5)), range(1 + rnd(4))]
  // one recipe, two builds
  const recipe: ((ops: typeof smart) => UOp)[] = []
  const pick = () => recipe[rnd(recipe.length)]!
  for (const r of rs) recipe.push(() => r)
  for (let k = 0; k < 6; k++) {
    const a = pick(), b = pick(), c = 1 + rnd(12), kind = rnd(5)
    recipe.push(kind === 0 ? (o) => o.add(a(o), b(o)) : kind === 1 ? (o) => o.mul(a(o), iconst(c)) : kind === 2 ? (o) => o.div(a(o), iconst(c)) : kind === 3 ? (o) => o.mod(a(o), iconst(c)) : (o) => o.add(o.mul(o.div(a(o), iconst(c)), iconst(c)), o.mod(a(o), iconst(c))))
  }
  const top = recipe[recipe.length - 1]!
  const e1 = top(smart), e2 = top(raw)
  if (size(e1) < size(e2)) smaller++
  const ext = rs.map((r) => (r.arg as { extent: number }).extent)
  for (let i = 0; i < ext[0]!; i++) for (let j = 0; j < ext[1]!; j++) for (let k = 0; k < ext[2]!; k++) {
    const env = new Map([[rs[0]!, i], [rs[1]!, j], [rs[2]!, k]])
    cases++
    if (evalIdx(e1, env) !== evalIdx(e2, env)) { fails++; if (fails < 5) console.log(`✗ at (${i},${j},${k}): simplified ${evalIdx(e1, env)} vs raw ${evalIdx(e2, env)}`) ; break }
  }
}
console.log(`${fails ? "FAIL" : "PASS"}: ${cases} evaluations over 3000 random index expressions, ${fails} disagreements; ${smaller} came out smaller`)
process.exit(fails ? 1 : 0)
