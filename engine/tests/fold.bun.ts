// Folding against the kernels: every op, on random constants, folded as it is built and
// computed by the CPU backend's printed kernel, must give the same bits.
//   bun engine/tests/fold.bun.ts
import * as ir from "../ir.ts"
import { CPU } from "../cpu.ts"
import { Tensor } from "../tensor.ts"

let seed = 11
const rnd = () => ((seed = (seed * 1103515245 + 12345) >>> 0) / 4294967296)
const data = (shape: number[], lo = -3, hi = 3) => ir.data(Float32Array.from({ length: ir.numel(shape) }, () => lo + (hi - lo) * rnd()), shape)
const index = (n: number, size: number) => ir.data(Float32Array.from({ length: n }, () => Math.floor(rnd() * size)), [n])
// an index that is not a whole number, or names no row
const ragged = (n: number, values: number[]) => ir.data(Float32Array.from({ length: n }, () => values[Math.floor(rnd() * values.length)]!), [n])
const cpu = new CPU()

const cases: [string, () => ir.UOp][] = [
  ["add", () => ir.add(data([3, 4]), data([4]))],
  ["mul", () => ir.mul(data([2, 3]), data([2, 1]))],
  ["recip", () => ir.recip(data([5]))],
  ["exp", () => ir.exp(data([5]))],
  ["log", () => ir.log(data([5], 0.1, 4))],
  ["sqrt", () => ir.sqrt(data([5], 0, 9))],
  ["less", () => ir.cmplt(data([6]), data([6]))],
  ["where", () => ir.where(ir.cmplt(data([6]), ir.konst(0)), data([6]), data([1]))],
  ["detach", () => ir.detach(data([3]))],
  ...Object.entries(ir.MATHS).map(([fn, m]): [string, () => ir.UOp] => [`math ${fn}`, () => (m.arity === 2 ? ir.math(fn, data([7]), data([7])) : ir.math(fn, data([7], fn.startsWith("a") ? -0.9 : -3, fn.startsWith("a") ? 0.9 : 3)))]),
  ["random", () => ir.random([9], 1234)],
  ["iota", () => ir.iota(9)],
  ["reduce add", () => ir.reduceAxis(data([3, 5, 2]), 1, false, "ADD")],
  ["reduce max", () => ir.reduceAxis(data([3, 5, 2]), 2, true, "MAX")],
  ["reduce mul", () => ir.reduceAxis(data([4, 3], 0.5, 1.5), 0, false, "MUL")],
  ["broadcast", () => ir.broadcastTo(data([3, 1]), [2, 3, 4])],
  ["permute", () => ir.permute(data([2, 3, 4]), [2, 0, 1])],
  ["reshape", () => ir.reshape(data([2, 6]), [3, 4])],
  ["slice", () => ir.slice(data([4, 5]), 1, 1, 3)],
  ["window", () => ir.windows(data([2, 6, 5]), [3, 2])],
  ["pad", () => ir.pad(data([3, 4]), 1, 2, 1)],
  ["gather", () => ir.gather(data([5, 3]), index(7, 5))],
  ["scatter add", () => ir.scatterAdd(index(8, 4), data([8, 2]), 4)],
  ["gather, fractional index", () => ir.gather(data([5, 2]), ragged(6, [0, 1.5, 3.7, -0.5, 4.99]))],
  ["scatter add, fractional, negative and NaN index", () => ir.scatterAdd(ragged(8, [0, 1.5, 2.9, 3, -0.5, -1, 4, NaN]), data([8, 2]), 4)],
  ["gather, batched", () => ir.gather(data([3, 5, 2]), ir.reshape(index(12, 5), [3, 4]))],
  ["scatter add, batched", () => ir.scatterAdd(ir.reshape(index(12, 4), [3, 4]), data([3, 4, 2]), 4)],
  ["view chain", () => ir.add(ir.slice(ir.permute(data([4, 3]), [1, 0]), 1, 1, 2), ir.iota(2))],
]

let fails = 0
for (const [name, make] of cases) {
  for (let t = 0; t < 20; t++) {
    const u = make()
    const folded = new Tensor(u).node
    const want = cpu.realizeSync([u])[0]!.data.subarray(0, ir.numel(u.shape))
    const got = (folded.op === "RESHAPE" ? folded.src[0]! : folded).arg as Float32Array
    const same = folded !== u && got.length === want.length && got.every((v, i) => Object.is(v, want[i]) || (Number.isNaN(v) && Number.isNaN(want[i]!)))
    if (!same) { fails++; console.log(`✗ ${name}: folded ${folded === u ? "(not folded)" : Array.from(got).slice(0, 6)} vs kernel ${Array.from(want).slice(0, 6)}`); break }
  }
}
console.log(fails ? `fold: ${fails} of ${cases.length} ops differ` : `fold: all ${cases.length} ops agree with their kernels, bit for bit`)
if (fails) process.exit(1)
