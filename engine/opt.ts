// simplify (value-preserving EXACT laws, on the engine) + the SCHEDULE (cost-driven fusion).
// After optimize a graph is leaves + FUSED loop nests, nothing else — one kernel form.
//
// Fusion is ONE mechanism: inline = substitute, materialize = LOAD, the cost model picks.
// That single splice rule IS the algebra's fusion laws — map∘map (L1), reduce-absorbs-map
// (L2), view-folds-into-the-read (L4), broadcast-back reduce inlining (rmsNorm/softmax as
// one kernel) and matmul epilogues (bias/activation/residual into the ACC's consumer) all
// fall out of "a single-use kernel folds into its reader, guarded by recompute cost".

import type { UOp, Op, Fold } from "./ir.ts";
import {
  node, konst, buffer, numel, strides, sameShape, permute, reshape, broadcastTo, slice, toposort, viewMap, bcIndex,
  range, iconst, iadd, imul, idiv, imod, load, acc, iidx, ival, fused, kInputs, rebuild, at, siteLabel,
} from "./ir.ts";
import { graphRewriteMany, type Rule } from "./rewrite.ts";

// ---- simplify: the generators' law table (EXACT only — bit-preserving) --------
const scalarOf = (u: UOp): number | null =>
  u.op === "CONSTANT" && u.shape.length === 0 ? (u.arg as Float32Array)[0]! : u.op === "BROADCAST" ? scalarOf(u.src[0]!) : null;
const is = (u: UOp, v: number): boolean => Object.is(scalarOf(u), v);
const UNROLL = 4;
const SIMPLIFY: Rule[] = [
  // a reduction over nothing is where its loop starts – 0 for a sum, 1 for a product, -∞
  // for a maximum, 0 for its winner – and a value with no elements needs no kernel: a
  // kernel over a zero-length axis would divide by that length
  (u) => {
    if (u.op !== "REDUCE") return null;
    const g = u.arg as { axis: number; op: Fold };
    return u.src[0]!.shape[g.axis] === 0 ? broadcastTo(konst(g.op === "MAX" ? -Infinity : g.op === "MUL" ? 1 : 0), u.shape) : null;
  },
  (u) => (numel(u.shape) === 0 && u.op !== "BROADCAST" && u.op !== "CONSTANT" && u.op !== "BUFFER" ? broadcastTo(konst(0), u.shape) : null),
  (u) => (u.op === "DETACH" ? u.src[0]! : null),                              // ∇ captured this already; identity forward
  (u) => (u.op === "MULTIPLY" && is(u.src[0]!, 1) ? u.src[1]! : null),
  (u) => (u.op === "MULTIPLY" && is(u.src[1]!, 1) ? u.src[0]! : null),
  // x + −0 is x for every x; x + 0 is not where x is −0
  (u) => (u.op === "ADD" && is(u.src[0]!, -0) ? u.src[1]! : null),
  (u) => (u.op === "ADD" && is(u.src[1]!, -0) ? u.src[0]! : null),
  (u) => ((u.op === "ADD" || u.op === "MULTIPLY") && u.src[0]!.op === "CONSTANT" && u.src[1]!.op === "CONSTANT" && !u.shape.length
    ? konst(u.op === "ADD" ? (u.src[0]!.arg as Float32Array)[0]! + (u.src[1]!.arg as Float32Array)[0]! : (u.src[0]!.arg as Float32Array)[0]! * (u.src[1]!.arg as Float32Array)[0]!) : null),
  (u) => (u.op === "LESS_THAN" && !u.shape.length && scalarOf(u.src[0]!) !== null && scalarOf(u.src[1]!) !== null ? konst(scalarOf(u.src[0]!)! < scalarOf(u.src[1]!)! ? 1 : 0) : null),
  (u) => { const c = u.op === "WHERE" ? scalarOf(u.src[0]!) : null; return c === null ? null : c !== 0 ? u.src[1]! : u.src[2]!; },   // NaN selects the first, as both backends do
  (u) => (u.op === "IVAL" && u.src[0]!.op === "ICONST" ? konst(u.src[0]!.arg as number) : null),
  // the view laws — pure indexing, so they may run in the default (bit-exact) pipeline
  (u) => (u.op === "RESHAPE" && sameShape(u.src[0]!.shape, u.shape) ? u.src[0]! : null),
  (u) => (u.op === "BROADCAST" && sameShape(u.src[0]!.shape, u.shape) ? u.src[0]! : null),
  (u) => (u.op === "PERMUTE" && (u.arg as number[]).every((p, i) => p === i) ? u.src[0]! : null),
  (u) => (u.op === "PERMUTE" && u.src[0]!.op === "PERMUTE" ? permute(u.src[0]!.src[0]!, (u.arg as number[]).map((i) => (u.src[0]!.arg as number[])[i]!)) : null),
  // a gather of one row named when the graph is built is a view of that row
  (u) => {
    if (u.op !== "GATHER" || u.shape[0] !== 1 || u.src[1]!.shape.length !== 1) return null;
    const i = u.src[1]!, c = i.op === "RESHAPE" ? i.src[0]! : i, v = c.op === "CONSTANT" && numel(c.shape) === 1 ? (c.arg as Float32Array)[0]! : NaN;
    return Number.isInteger(v) && v >= 0 && v < u.src[0]!.shape[0]! ? slice(u.src[0]!, 0, v, 1) : null;
  },
  // a sum or product of UNROLL terms at most – a point's coordinates, a color's channels – is
  // its terms, in the loop's order: straight-line code, which the schedule prices as the map
  // it is. A maximum and its winner keep their loops: no elementwise op is maxf
  (u) => {
    const g = u.arg as { axis: number; keepdims: boolean; op: Fold }, x = u.src[0]!;
    if (u.op !== "REDUCE" || (g.op !== "ADD" && g.op !== "MUL") || x.shape[g.axis]! > UNROLL) return null;
    let t = broadcastTo(konst(g.op === "ADD" ? 0 : 1), x.shape.map((d, i) => (i === g.axis ? 1 : d)));
    for (let k = 0; k < x.shape[g.axis]!; k++) t = node(g.op === "ADD" ? "ADD" : "MULTIPLY", [t, slice(x, g.axis, k, 1)], null, t.shape);
    return g.keepdims ? t : reshape(t, u.shape);
  },
];
export const simplifyMany = (roots: UOp[]): UOp[] => graphRewriteMany(roots, SIMPLIFY);

// The memory a plan computes into, all of it, is refused past a budget: a broadcast one
// axis too big must be an Error, not exhaust the machine's memory (on Apple silicon the GPU
// shares it with everything else). `materialized` names the nodes the backend allocates.
export const MEMORY_BUDGET = 2 ** 31;
export const checkMemory = (roots: readonly UOp[], materialized: Op): void => {
  const pieces = toposort(roots).filter((u) => u.op === materialized), bytes = pieces.reduce((n, u) => n + numel(u.shape) * 4, 0);
  if (bytes <= MEMORY_BUDGET) return;
  const big = pieces.reduce((a, u) => (numel(u.shape) > numel(a.shape) ? u : a)), gb = (b: number) => `${(b / 2 ** 30).toFixed(1)} GB`;
  const where = big.site ? ` from ${siteLabel(big.site)}` : "";
  throw Object.assign(new Error(`this needs ${gb(bytes)} of memory at once – more than the ${MEMORY_BUDGET / 2 ** 30} GB Fluent allows; the largest piece, [${big.shape.join(" ")}]${where}, takes ${gb(numel(big.shape) * 4)}: compute it in smaller pieces`), { site: big.site });
};

// Σ-linearity at the SCHEDULE level (reordering float summation is a schedule license —
// the tile and coop tree-reduces already take it). ∇'s fork-summed cotangents (several
// reduce chains added per weight) become ONE chain whose sum-of-products body the split-K
// tile printer catches: ADD(W a, W b) → W(ADD(a, b)) for one linear wrapper W – a Σ, or a
// reshape or permute in the way of one. A body joins only while one kernel can read it: a
// weight a loop reads every round has a cotangent a round, and their products joined past
// MAX_INPUTS split into partial sums the size of the product – [M, k, n] for a matrix's.
const SCHED_REWRITES: Rule[] = [(u) => {
  const [x, y] = u.src as [UOp, UOp];
  if (u.op !== "ADD" || x.op !== y.op || !(x.op === "RESHAPE" || x.op === "PERMUTE" || (x.op === "REDUCE" && (x.arg as { op: string }).op === "ADD"))) return null;
  const a = x.src[0]!, b = y.src[0]!;
  if (JSON.stringify(x.arg) !== JSON.stringify(y.arg) || !sameShape(a.shape, b.shape) || loadsOf(b, loadsOf(a)).size > MAX_INPUTS) return null;
  return node(x.op, [node("ADD", [a, b], null, a.shape)], x.arg, u.shape);
}];
// what a body fused into one kernel loads: the values it reaches through elementwise ops and
// views, a small constant aside
function loadsOf(u: UOp, out = new Set<number>(), seen = new Set<number>()): Set<number> {
  if (seen.has(u.id)) return out;
  seen.add(u.id);
  if (ELEM.has(u.op) || VIEW.has(u.op)) for (const s of u.src) loadsOf(s, out, seen);
  else if (!small(u)) out.add(u.id);
  return out;
}

// ---- the schedule ----------------------------------------------------------------
// Measured platform constants: inlining a kernel that carries a loop trades a dispatch for
// `indexSpace·loopExtent` recomputed flops — pays under ~2e7. A contraction shorter than
// tileMinK runs as a plain nest, not a tile: tests/bench-tile.ts re-derives that one. The
// workgroups that fill the machine (fill), the fewest steps a thread of a split loop runs
// (groupSteps) and the outputs, side by side in memory, from which a thread each beats a
// workgroup each (coopFew) pick a reduction's form on the GPU: tests/bench-reduce.ts
// re-derives them. A node read more often than it has elements is recomputed while that
// costs fewer ops an element than its store and read (recompute), and a chain of loops
// folded into one thread stops at perThread steps: tests/bench-fuse.ts re-derives both.
export const COST = { reduceBudget: 2e7, perElem: 4096, tile: 16, tileMinM: 2, tileMinK: 8, fill: 96, groupSteps: 4, coopFew: 256, recompute: 300, perThread: 64 };
const ELEM = new Set<Op>(["ADD", "MULTIPLY", "RECIPROCAL", "EXP", "LOG", "SQRT", "LESS_THAN", "WHERE", "MATH"]);
const VIEW = new Set<Op>(["BROADCAST", "PERMUTE", "RESHAPE", "SLICE", "WINDOW"]);
const INDEX = new Set<Op>(["IADD", "IMUL", "IDIV", "IMOD"]);
const MAX_INPUTS = 6;                                                          // soft cap on kernel growth (computing in place stops here; mandatory reads may still add)
const HARD_INPUTS = 8;                                                         // past this a kernel is rebuilt materialize-everything. THE TARGET IS THE BROWSER: Chrome/Dawn caps storage buffers at 10/stage (Deno's Metal wgpu falsely reports 31), so inputs+output ≤ 9+1 = 10
const REGION = 1024;                                                           // the most ops a kernel computes in place
// An input this small – a point, a color, a table of corners – shares a binding: a kernel
// with more inputs than the device binds reads its small ones from blocks of HARD_INPUTS
// (packMany), so they cost one binding a block
const SMALL_INPUT = 256;
const small = (x: UOp): boolean => (x.op === "BUFFER" || x.op === "CONSTANT") && numel(x.shape) <= SMALL_INPUT;
const bindings = (xs: readonly UOp[]): number => { const s = xs.filter(small).length; return xs.length - s + Math.ceil(s / HARD_INPUTS); };

// How many ops read each node, a view's reads counted at the node it views, and through how
// many ways: as it is laid out (itself, or a view that only relabels it), through each other
// view, as each gather's table, by each pad. Reads one way are reads at one index; another
// way reads it at other indices. A way names the elements it takes: as many as its view has,
// where no element repeats in it (no broadcast, no window), else all of them, endlessly.
function readers(roots: UOp[]): { reads: Map<number, number>; ways: Map<number, Map<string, number>> } {
  const reads = new Map<number, number>(), ways = new Map<number, Map<string, number>>();
  for (const u of toposort(roots)) {
    if (VIEW.has(u.op)) continue;
    u.src.forEach((s0, i) => {
      let s = s0, plain = true;
      while (VIEW.has(s.op)) { plain &&= s.op !== "WINDOW"; s = s.src[0]!; }
      const vm = viewMap(s0), w = ways.get(s.id) ?? new Map<string, number>(), other = u.op === "PAD" || (u.op === "GATHER" && i === 0) || vm.base !== s;
      reads.set(s.id, (reads.get(s.id) ?? 0) + 1);
      ways.set(s.id, w);
      w.set(other ? `#${u.id}:${i}` : vm.offset === 0 && sameShape(vm.strides, strides(s0.shape)) ? "=" : `${vm.offset}:${vm.strides}:${s0.shape}`,
        !other && plain && vm.strides.every((st, d) => st || s0.shape[d] === 1) ? numel(s0.shape) : Infinity);
    });
  }
  return { reads, ways };
}

const accMemo = new WeakMap<UOp, number>();                                     // values are immutable; the memo keeps the DAG walk linear and dies with the nodes
// the loop work one element of a kernel costs: a loop is its extent times its body's, a
// nest multiplies down, side-by-side loops count once – the costliest of them
export const accCost = (v: UOp): number => {
  const m = accMemo.get(v); if (m !== undefined) return m;
  let c = 1;
  for (const s of v.src) c = Math.max(c, accCost(s));
  if (v.op === "ACC") c *= (v.src[0]!.arg as { extent: number }).extent;
  accMemo.set(v, c);
  return c;
};

// the loop variables an index reads
const rangeMemo = new WeakMap<UOp, ReadonlySet<UOp>>();
export const rangesIn = (u: UOp): ReadonlySet<UOp> => {
  const m = rangeMemo.get(u); if (m) return m;
  const out = new Set<UOp>(u.op === "RANGE" ? [u] : []);
  for (const s of u.src) for (const r of rangesIn(s)) out.add(r);
  rangeMemo.set(u, out);
  return out;
};

// the loop steps a value at index ix runs for each of its kernel's threads: the loops its
// index reads, the thread's own index aside
const steps = (ix: UOp, gid: UOp): number => { let n = 1; for (const r of rangesIn(ix)) if (r !== gid) n *= (r.arg as { extent: number }).extent; return n; };
// The maps a plan starts by storing. One read more than one way – at other indices – whose
// ways take more elements than it has: computed in place it would be computed again at
// each, and its sources at each of theirs; a pass frees it where that costs less. Ways that
// take each element once at most, the coordinates of a point, cost nothing to compute in
// place. And, settled, one whose expression down to what is stored is larger than REGION
// ops, where it is no larger than its sources – never a product broadcast to be summed: its
// size is at most its sources' sizes added up, and at most the stretch of the topological
// order its nodes lie in; past REGION by both, its nodes are counted one by one.
function starts(roots: UOp[], ways: Map<number, Map<string, number>>, settled: Set<number>): Set<number> {
  const taken = (w?: Map<string, number>) => (w && w.size > 1 ? [...w.values()].reduce((p, q) => p + q, 0) : 0);
  const order = toposort(roots), store = new Set(order.filter((u) => ELEM.has(u.op) && taken(ways.get(u.id)) > numel(u.shape)).map((u) => u.id));
  const at = new Map(order.map((u, i) => [u.id, i])), base = (x: UOp): UOp => { while (VIEW.has(x.op)) x = x.src[0]!; return x; };
  const srcs = order.map((u) => u.src.map((x) => at.get(base(x).id)!)), size = new Int32Array(order.length), first = new Int32Array(order.length), seen = new Int32Array(order.length);
  for (let i = 0; i < order.length; i++) {
    let n = 1, from = i;
    for (const j of srcs[i]!) if (!store.has(order[j]!.id)) { n += size[j]!; from = Math.min(from, first[j]!); }
    first[i] = from;
    if (n > REGION && i - from + 1 > REGION) {
      const stack = [i];
      n = 0;
      while (stack.length && n <= REGION) { const x = stack.pop()!; if (seen[x] === i + 1) continue; seen[x] = i + 1; n++; for (const y of srcs[x]!) if (!store.has(order[y]!.id)) stack.push(y); }
      if (n > REGION && ELEM.has(order[i]!.op) && numel(order[i]!.shape) <= Math.max(...srcs[i]!.map((j) => numel(order[j]!.shape)))) { store.add(order[i]!.id); settled.add(order[i]!.id); n = 0; first[i] = i; }
    }
    size[i] = Math.min(n, i - from + 1);
  }
  return store;
}
// a kernel's work for one of its elements: its ops, each as often as the loops it reads run –
// a loop's variables as bits, a node's the bits of what it reads
const workMemo = new WeakMap<UOp, number>();
const work = (k: UOp): number => {
  let n = workMemo.get(k);
  if (n !== undefined) return n;
  const bit = new Map<number, number>(), extents: number[] = [], mask = new Map<number, number>();
  n = 0;
  for (const u of toposort(k.src[1]!)) {
    let m = 0;
    if (u.op === "RANGE" && u !== k.src[0] && bit.size < 30) { bit.set(u.id, bit.size); extents.push((u.arg as { extent: number }).extent); m = 1 << (bit.size - 1); }
    for (const s of u.src) m |= mask.get(s.id) ?? 0;
    mask.set(u.id, u.op === "ACC" ? m & ~(1 << (bit.get(u.src[0]!.id) ?? 31)) : m);
    if (u.op !== "RANGE" && u.op !== "ICONST" && u.op !== "CONSTANT") { let s = 1; for (let i = 0; i < extents.length; i++) if (m & (1 << i)) s *= extents[i]!; n += s; }
  }
  workMemo.set(k, n);
  return n;
};
// Whether a node of `work` ops an element, computed `a` times an element, costs more than
// storing it and reading it back: an element's store and first read cost COST.recompute
// ops, and each read after it, from the cache, one op.
const stored = (work: number, a: number): boolean => work * (a - 1) > COST.recompute + a;

// Fusion builds each kernel from the graph. A node read at an index is computed there, once
// for each (node, index) in a kernel – a fork read twice at one index is one value, x·x·x·x
// is 2 multiplies – unless the plan stores it: then every reader loads it. `store` names the
// nodes a plan stores; `settled`, those a pass has freed or stored by what its plan shows,
// which no pass frees again. optimizeMany passes again until the plan stops changing. A
// kernel of the last pass (`kept`) is kept when nothing it computes or loads, and none of its
// inputs, changed (`changed`).
// `planNotes` counts the copies the plans have noted – their work, the same on any machine
let notes = 0;
export const planNotes = (): number => notes;
type Built = { outN: number; gid: UOp; here: [UOp, UOp, number][]; loads: [UOp, UOp][] };
type Kept = { kernels: Map<number, { k: UOp; rec?: Built }>; owner: Map<UOp, UOp> };
function fuseWith(roots: UOp[], reads: Map<number, number>, store: Set<number>, settled: Set<number>, kept: Kept, changed: Set<number>): UOp[] {
  const built = new Map<number, UOp>(), owner = kept.owner;                  // a node's kernel this pass, and a kernel's node
  let fresh = 0; // the nodes this pass stores as it builds
  // per kernel: its threads, its index, and each node it computes in place or loads, at its index
  const computed = new Map<UOp, Built>();
  // an index by its structure – the same index built twice is one key, a number – and a value
  // in it, a gather's row, by identity
  const iKeys = new WeakMap<UOp, string>(), iNums = new WeakMap<UOp, number>(), byKey = new Map<string, number>();
  const iKey = (u: UOp): string => {
    let k = iKeys.get(u);
    if (k === undefined) { k = u.op === "ICONST" ? `${u.arg}` : INDEX.has(u.op) ? `${u.op}(${u.src.map(iKey).join(",")})` : `#${u.id}`; iKeys.set(u, k); }
    return k;
  };
  const iNum = (u: UOp): number => {
    let n = iNums.get(u);
    if (n === undefined) { const k = iKey(u); n = byKey.get(k); if (n === undefined) byKey.set(k, (n = byKey.size)); iNums.set(u, n); }
    return n;
  };

  // one kernel's body: `outN` threads, `gid` a thread's index, `cap` its bindings. Computing
  // in place stops at the cap, but mandatory reads may still push past it – the caller
  // retries an over-wide kernel with cap 0 (load everything not mandatory).
  const body = (outN: number, gid: UOp, cap: number) => {
    const inputs: UOp[] = [], slot = new Map<number, number>(), memo = new Map<number, Map<number, UOp>>(), keys: [number, number][] = [], here: [UOp, UOp, number][] = [], loads: [UOp, UOp][] = [];
    const over = new Set<number>();                                           // the maps whose reads took the kernel past its cap
    const loops = new Map<number, number>(), loop = (u: UOp) => loops.get(u.id) ?? 1;   // the steps a value's loops run, for each of its own
    const input = (b: UOp): number => { let i = slot.get(b.id); if (i === undefined) { i = inputs.length; inputs.push(b); slot.set(b.id, i); if (small(b)) smalls++; } return i; };
    let smalls = 0;
    const wide = () => inputs.length - smalls + Math.ceil(smalls / HARD_INPUTS) > cap;           // the bindings, as `bindings` counts them, past the cap
    // The plan's rule for storing a node, as the body grows: each node's finished copies here
    // – the computations they make, the most work one does – and a node whose next copy
    // would take them past what storing it costs is stored now, and that copy loads it. The
    // plan counts at least as many, so it would store the node too; deciding it here keeps a
    // chain of forks read at two indices each, butterflies, from building 2^depth copies first.
    // Each note keeps the count it replaces, so an undone copy takes its notes back.
    const tally = new Map<number, [number, number]>(), noted: [number, [number, number] | undefined][] = [];
    const note = ([x, i, w]: [UOp, UOp, number]) => { notes++; const t = tally.get(x.id); noted.push([x.id, t]); tally.set(x.id, [(t?.[0] ?? 0) + outN * steps(i, gid), Math.max(t?.[1] ?? 0, w)]); };
    const costlier = (v: UOp, ix: UOp): boolean => {
      const t = tally.get(v.id);
      if (!t || v.op === "GATHER" || (reads.get(v.id) ?? 1) < 2) return false;
      const n = Math.max(numel(v.shape), 1), e = t[0] + outN * steps(ix, gid);
      if (e <= n || !stored(t[1], e / n)) return false;
      store.add(v.id); settled.add(v.id); fresh++;
      return true;
    };
    // whether v, read at ix, is computed here or loaded from its own kernel. A map is computed
    // where it is read – a loop it reads decides for itself – and so is a gather, a read of
    // its table
    const inPlace = (v: UOp, ix: UOp, whole: boolean): boolean => {
      if (store.has(v.id) || over.has(v.id) || v.op === "SCATTER_ADD") return false;
      if (v.op !== "REDUCE") return true;
      const b = kernelOf(v), c = accCost(b.src[1]!);
      if (bindings([...inputs, ...kInputs(b).filter((x) => !slot.has(x.id))]) > cap) return false;
      // a loop nest more than one op reads, or one the plan stores, is computed once
      if ((reads.get(v.id) ?? 1) > 1 && c > UNROLL) return false;
      // two budgets: TOTAL recompute (dispatch-vs-flops) and PER-THREAD work – a loop that
      // squares per-thread loops at low thread counts starves the GPU (2048 threads × 8192
      // sequential ops is a 100 ms kernel however you dispatch it)
      const evals = outN * steps(ix, gid), perElem = (evals / outN) * c, amp = evals / Math.max(numel(v.shape), 1);
      // At amplification ≈ 1 (each element taken once) a reduction that is the reader's
      // whole body does the two-stage work minus the round trip – traffic elimination, so it
      // is exempt from the recompute budget: a chain of sums over a body with no loop of its
      // own is what the split-K tile and the split reduction print. Over a body with a loop,
      // nothing splits it, so it folds only within COST.perThread steps a thread. A scalar
      // output (a loss) never folds: the graph would run as one serial thread.
      const chain = (x: UOp): boolean => (x.op === "ACC" ? chain(x.src[1]!) : accCost(x) === 1);
      const traffic = whole && amp <= 1.01 && outN >= 2 && (chain(b.src[1]!) || perElem <= COST.perThread);
      // a loop computed inside another runs once per step of the outer loop, on one thread –
      // stored it runs on its own, in parallel; only a short one pays
      const nested = evals > outN && c > 16;
      // TOTAL recompute: what the reader evaluates beyond computing v once. A loop as long as
      // a tile is priced whole: stored, a contraction runs as a tile
      const extra = (c >= COST.tile ? evals : evals - Math.max(numel(v.shape), 1)) * c;
      return traffic || (!nested && perElem <= COST.perElem && extra <= COST.reduceBudget);
    };
    // v at flat index ix, `whole` while v is all the kernel computes
    const value = (v: UOp, ix: UOp, whole = false): UOp => {
      const s = scalarOf(v);
      if (s !== null) return konst(s);                                         // a scalar constant is a literal — no buffer, no slot
      const at = iNum(ix);
      let o = memo.get(at)?.get(v.id);
      if (o) return o;
      if (VIEW.has(v.op)) {                                                    // a view folds into the index; an unfoldable reshape reads its source's
        const vm = viewMap(v), e = bcIndex(vm.strides, v.shape, ix);
        o = vm.base === v ? value(v.src[0]!, ix, whole) : value(vm.base, vm.offset ? iadd(e, iconst(vm.offset)) : e, whole);
      } else if (v.op === "CONSTANT" || v.op === "BUFFER") o = load(input(v), ix);
      else if (v.op === "IOTA") o = ival(ix);
      else if (v.op === "RANDOM") o = node("RAND", [ix, load(input(v.src[0]!), iconst(0))], null, []);
      else if (v.op === "PAD") {                                               // the source where the index lands inside it, 0 around it
        const { axis, before } = v.arg as { axis: number; before: number }, src = v.src[0]!;
        const pst = strides(v.shape), sst = strides(src.shape), len = src.shape[axis]!;
        let srcIdx: UOp = iconst(0);
        for (let d = 0; d < v.shape.length; d++) srcIdx = iadd(srcIdx, imul(imod(idiv(ix, iconst(pst[d]!)), iconst(v.shape[d]!)), iconst(sst[d]!)));
        const c = ival(imod(idiv(ix, iconst(pst[axis]!)), iconst(v.shape[axis]!)));
        const below = node("LESS_THAN", [c, konst(before + len)], null, []);
        const inside = before ? node("MULTIPLY", [below, node("ADD", [konst(1), node("MULTIPLY", [node("LESS_THAN", [c, konst(before)], null, []), konst(-1)], null, [])], null, [])], null, []) : below;
        o = node("WHERE", [inside, value(src, iadd(srcIdx, iconst(-before * sst[axis]!))), konst(0)], null, []);   // wraps below 0 only where `inside` is 0
      } else if (inPlace(v, ix, whole) && !costlier(v, ix)) {
        // a map whose reads take the kernel past its cap is undone, and loaded
        const mark = [inputs.length, keys.length, here.length, noted.length] as const, entry: [UOp, UOp, number] = [v, ix, 0];
        here.push(entry);
        o = own(v, ix, whole);
        entry[2] = (here.length - mark[2]) * loop(o);                     // its work: the values it computes here, each as often as its loops run
        if (v.op !== "REDUCE" && wide()) {
          for (const x of inputs.splice(mark[0])) { slot.delete(x.id); if (small(x)) smalls--; }
          for (const [i, id] of keys.splice(mark[1])) memo.get(i)!.delete(id);
          here.length = mark[2];
          for (const [id, t] of noted.splice(mark[3]).reverse()) t ? tally.set(id, t) : tally.delete(id);
          over.add(v.id);
          o = load(input(kernelOf(v)), ix);
        } else note(entry);
      } else { o = load(input(kernelOf(v)), ix); loads.push([v, ix]); }
      let row = memo.get(at);
      if (!row) memo.set(at, (row = new Map()));
      row.set(v.id, o); keys.push([at, v.id]);
      return o;
    };
    // v's own op at ix
    const own = (v: UOp, ix: UOp, whole: boolean): UOp => {
      if (ELEM.has(v.op)) { const s = v.src.map((x) => value(x, ix)), o = node(v.op, s, v.arg, []); loops.set(o.id, Math.max(...s.map(loop))); return o; }
      if (v.op === "REDUCE") {                                                 // an ACC over the reduced axis, its source computed in the body
        const a = v.arg as { axis: number; keepdims: boolean; op: Fold }, inSh = v.src[0]!.shape, inSt = strides(inSh), k = range(inSh[a.axis]!);
        const mapped = v.shape.map((_, d) => inSt[a.keepdims ? d : d < a.axis ? d : d + 1]!);
        const b = value(v.src[0]!, iadd(bcIndex(mapped, v.shape, ix), imul(k, iconst(inSt[a.axis]!))), whole), o = acc(a.op, k, b);
        loops.set(o.id, inSh[a.axis]! * loop(b));
        return o;
      }
      // out[i] = table[idx[i]]: the index computed where it is read, the table read at the row
      // it names, through its views. A gather that takes each element of the table's base
      // about once computes a computed table where it takes it – a gather of a gather is one
      // read; one that takes them many times reads them stored
      const [t, i] = v.src as [UOp, UOp], b = i.shape.length - 1, R = Math.max(numel(t.shape.slice(b + 1)), 1), vm = viewMap(t);
      const j = idiv(ix, iconst(R)), at = iadd(imul(iidx(value(i, j)), iconst(R)), imod(ix, iconst(R)));
      // a batch's rows lie after the rows of the batches before it
      const row = b ? iadd(at, imul(idiv(j, iconst(i.shape[b]!)), iconst(t.shape[b]! * R))) : at;
      if (numel(v.shape) <= 2 * Math.max(numel(vm.base.shape), 1)) return value(t, row);
      const e = bcIndex(vm.strides, t.shape, row);
      return load(input(kernelOf(vm.base)), vm.offset ? iadd(e, iconst(vm.offset)) : e);
    };
    return { inputs, here, loads, value, own };
  };

  const kernelOf = (u: UOp): UOp => {
    const m = built.get(u.id); if (m) return m;
    const last = kept.kernels.get(u.id), moved = (x: UOp) => changed.has(x.id);
    if (last && !last.rec?.here.some(([x]) => moved(x)) && !last.rec?.loads.some(([x]) => moved(x)) && kInputs(last.k).every((x) => x.op !== "FUSED" || kernelOf(owner.get(x)!) === x)) {
      built.set(u.id, last.k);
      if (last.rec) computed.set(last.k, last.rec);
      return last.k;
    }
    const outer = at.site;
    at.site = u.site;                                                          // a kernel comes from the node it computes
    let out: UOp;
    if (u.op === "CONSTANT" || u.op === "BUFFER") out = u;
    else if (u.op === "SCATTER_ADD") {                                         // out[v,c] = Σ_i (idx[i]==v ? values[i,c] : 0) — GATHER's transpose, as one nest
      const idx = u.src[0]!, B = idx.shape.length - 1, k = idx.shape[B]!, size = (u.arg as { size: number }).size, R = Math.max(numel(u.shape.slice(B + 1)), 1);
      const gid = range(Math.max(numel(u.shape), 1)), i = range(k), q = idiv(gid, iconst(R));
      // a batch's rows take the batch's own k indices, never every batch's
      const [v, at] = B ? [imod(q, iconst(size)), iadd(imul(idiv(q, iconst(size)), iconst(k)), i)] : [q, i];
      // an index names the row of its whole part, the row GATHER reads; NaN names none
      const hit = node("MATH", [node("MATH", [load(0, at)], { fn: "trunc" }, []), ival(v)], { fn: "eq" }, []);
      // selected, not multiplied: an ∞ in another row's values is not 0·∞ here
      const b = node("WHERE", [hit, load(1, iadd(imul(at, iconst(R)), imod(gid, iconst(R)))), konst(0)], null, []);
      out = fused(gid, acc("ADD", i, b), [kernelOf(u.src[0]!), kernelOf(u.src[1]!)], u.shape);
    } else {
      // the kernel computes its own node at each element; a view that reaches here is a root,
      // a strided copy, and an unfoldable reshape copies its source's region
      const top = VIEW.has(u.op) && viewMap(u).base === u ? u.src[0]! : u;
      const mk = (cap: number): UOp => {
        const n = Math.max(numel(u.shape), 1), gid = range(n), b = body(n, gid, cap);
        const k = fused(gid, ELEM.has(top.op) || top.op === "REDUCE" || top.op === "GATHER" ? b.own(top, gid, true) : b.value(top, gid, true), b.inputs, u.shape);
        computed.set(k, { outN: n, gid, here: b.here, loads: b.loads });
        return k;
      };
      out = mk(MAX_INPUTS);
      if (bindings(kInputs(out)) > HARD_INPUTS) out = mk(0);                   // mandatory reads overflowed the device's binding slots: load everything
    }
    built.set(u.id, out); owner.set(out, u); kept.kernels.set(u.id, { k: out, rec: computed.get(out) });
    at.site = outer;
    return out;
  };
  // Pre-build every loop nest and every node the plan stores, in topological order, so
  // kernelOf() seldom recurses deeper than one kernel — real graphs are thousands of nodes deep.
  for (const u of toposort(roots)) if (u.op === "REDUCE" || u.op === "SCATTER_ADD" || store.has(u.id)) kernelOf(u);
  // How often the plan computes each node in place, and how often it loads each: readers
  // first, a kernel whose node this pass frees counted as often as its node would be
  // computed in place, so a fork freed under a freed fork is counted at both multiples.
  const plan = roots.map(kernelOf), kernels = toposort(plan, (u) => (u.op === "FUSED" ? kInputs(u) : [])).filter((u) => u.op === "FUSED").reverse();
  const here = new Map<UOp, number>(), loads = new Map<UOp, number>(), works = new Map<UOp, number>(), times = new Map<number, number>(), top = new Set<UOp>(), crowded = new Set<UOp>();
  const add = (m: Map<UOp, number>, v: UOp, n: number) => m.set(v, (m.get(v) ?? 0) + n);
  for (const k of kernels) {
    const c = computed.get(k), v = owner.get(k)!;
    if (!c) continue;
    // its node, decided once every kernel that reads it is counted: a loop nest one kernel
    // loads is computed once, and every reader loads it; a stored fork computed where it is
    // read for less than it costs stored is freed
    const n = Math.max(numel(v.shape), 1), e = loads.get(v) ?? 0;
    if (v.op === "REDUCE" && e) store.add(v.id);
    if (ELEM.has(v.op) && store.has(v.id) && !settled.has(v.id) && e && !stored(work(k), e / n)) { store.delete(v.id); settled.add(v.id); times.set(v.id, e / n); }
    const m = times.get(v.id) ?? 1;
    const once = new Map<UOp, number>();
    for (const [x, ix, w] of c.here) { const t = c.outN * steps(ix, c.gid); add(here, x, m * t); add(once, x, t); works.set(x, Math.max(works.get(x) ?? 0, w)); if (t <= c.outN) top.add(x); }
    for (const [x, t] of once) if (t > Math.max(numel(x.shape), 1)) crowded.add(x);
    for (const [x, ix] of c.loads) add(loads, x, m * c.outN * steps(ix, c.gid));
  }
  // a node more than one op reads, computed in place more often than it has elements, is
  // stored where that costs less than recomputing it. A gather is a read and never stored,
  // and neither is a node only loops read, each of its elements once a kernel – pixels ×
  // pieces, read where they are reduced: it is computed as often as its loops are, so a
  // loop, or what reads it, is stored instead. A loop nest computed more often than it has
  // elements, each of them once a kernel – in two kernels – is stored, computed once as one
  // more than one op reads is, by a pass that stores nothing else: a node it stores may
  // compute the loop once, in its own kernel
  const more = [...here].map(([v, e]) => [v, e / Math.max(numel(v.shape), 1)] as const).filter(([, a]) => a > 1);
  const forks = more.filter(([v, a]) => (reads.get(v.id) ?? 1) > 1 && v.op !== "GATHER" && (crowded.has(v) || top.has(v)) && stored(works.get(v)!, a));
  const loops = forks.length || fresh ? [] : more.filter(([v]) => v.op === "REDUCE" && !crowded.has(v) && accCost(built.get(v.id)!.src[1]!) > UNROLL);
  for (const [v] of [...forks, ...loops]) { store.add(v.id); settled.add(v.id); }
  return plan;
}

// ---- small inputs share a binding -----------------------------------------------------
// A kernel with more inputs than the device binds reads its small ones from blocks: a block
// is a kernel laying up to HARD_INPUTS of them one after another, and a read of one becomes
// a read of its block at its offset.
const block = (xs: readonly UOp[]): { k: UOp; at: number[] } => {
  const at: number[] = [];
  let total = 0;
  for (const x of xs) { at.push(total); total += numel(x.shape); }
  const g = range(Math.max(total, 1)), c = ival(g);
  let v: UOp = konst(0);
  xs.forEach((x, j) => {
    const below = (n: number) => node("LESS_THAN", [c, konst(n)], null, []);
    const inside = node("MULTIPLY", [below(at[j]! + numel(x.shape)), node("ADD", [konst(1), node("MULTIPLY", [below(at[j]!), konst(-1)], null, [])], null, [])], null, []);
    v = node("WHERE", [inside, load(j, iadd(g, iconst(-at[j]!))), v], null, []);   // wraps below the offset only where `inside` is 0
  });
  return { k: fused(g, v, [...xs], [total]), at };
};
const packMany = (plan: UOp[]): UOp[] => {
  const memo = new Map<number, UOp>();
  for (const k of toposort(plan, (u) => (u.op === "FUSED" ? kInputs(u) : []))) {
    if (k.op !== "FUSED") continue;
    const ins = kInputs(k).map((x) => memo.get(x.id) ?? x);
    let out = ins.every((x, i) => x === kInputs(k)[i]) ? k : node("FUSED", [k.src[0]!, k.src[1]!, ...ins], null, k.shape, k.site);
    const smalls = ins.map((x, i) => (small(x) ? i : -1)).filter((i) => i >= 0);
    if (ins.length > HARD_INPUTS && smalls.length > 1) {
      const rest = ins.map((_, i) => i).filter((i) => !small(ins[i]!)), slot = new Map<number, [number, number]>(rest.map((i, j) => [i, [j, 0]]));
      const blocks: UOp[] = [];
      for (let b = 0; b < smalls.length; b += HARD_INPUTS) {
        const part = smalls.slice(b, b + HARD_INPUTS), { k: bk, at } = block(part.map((i) => ins[i]!));
        part.forEach((i, j) => slot.set(i, [rest.length + blocks.length, at[j]!]));
        blocks.push(bk);
      }
      const seen = new Map<number, UOp>();
      const move = (v: UOp): UOp => {
        let w = seen.get(v.id);
        if (w) return w;
        const src = v.src.map(move);
        if (v.op === "LOAD") { const [s, off] = slot.get((v.arg as { slot: number }).slot)!; w = load(s, off ? iadd(src[0]!, iconst(off)) : src[0]!); }
        else w = src.every((x, i) => x === v.src[i]) ? v : rebuild(v.op, src, v.arg, v.shape);
        seen.set(v.id, w);
        return w;
      };
      out = node("FUSED", [k.src[0]!, move(k.src[1]!), ...rest.map((i) => ins[i]!), ...blocks], null, k.shape, k.site);
    }
    memo.set(k.id, out);
  }
  return plan.map((r) => memo.get(r.id) ?? r);
};

// ---- optimize: simplify → schedule → fuse → pack; result = leaves + FUSED only ---------
export function optimizeMany(roots: UOp[]): UOp[] {
  const s = graphRewriteMany(simplifyMany(roots), SCHED_REWRITES), { reads, ways } = readers(s), settled = new Set<number>(), store = starts(s, ways, settled);
  // a node the plan stores is computed once: fuse again, every reader loading what the last
  // plan stores, until the plan stops changing – three passes at most, the last plan built
  // from what the one before it decided
  const kept: Kept = { kernels: new Map(), owner: new Map() };
  let g: UOp[], changed = new Set<number>(), passes = 0;
  do {
    const before = new Set(store);
    g = fuseWith(s, reads, store, settled, kept, changed);
    changed = new Set([...before].filter((x) => !store.has(x)).concat([...store].filter((x) => !before.has(x))));
  } while (changed.size && ++passes < 3);
  g = packMany(g);
  for (const u of toposort(g))                                                 // legality: only leaves and kernels may reach the executor
    if (u.op !== "FUSED" && u.op !== "CONSTANT" && u.op !== "BUFFER" && u.shape.length) throw new Error(`optimize: ${u.op} survived to the kernel boundary`);
  return g;
}
export const optimize = (root: UOp): UOp => optimizeMany([root])[0]!;

// ---- plans: a graph's compiled form, found again by structure -----------------------
// A plan's identity is its structure: ops, args and shapes in a canonical order, leaves
// numbered by position — a device buffer or a tensor constant is rebound on every run, so
// a training step built afresh each tick finds the plan it compiled on the first. A scalar
// constant is part of the structure: the printer folds it into the kernel text.
export function canonical(roots: UOp[]): { key: string; leaves: UOp[] } {
  const index = new Map<number, number>(), leaves: UOp[] = [];
  const parts = toposort(roots).map((u, i) => {
    index.set(u.id, i);
    if (u.op === "BUFFER" || (u.op === "CONSTANT" && u.shape.length)) return `L${leaves.push(u) - 1}[${u.shape}]`;
    if (u.op === "CONSTANT") { const v = (u.arg as Float32Array)[0]!; return `C${Object.is(v, -0) ? "-0" : v}`; }
    return `${u.op}${u.arg === null || u.arg === undefined ? "" : JSON.stringify(u.arg)}[${u.shape}](${u.src.map((x) => index.get(x.id))})`;
  });
  return { key: `${parts.join(";")}|${roots.map((r) => index.get(r.id))}`, leaves };
}

// A plan is built over placeholder leaves, never a run's own: a cached plan holds no data,
// and every run binds its leaves by position.
export const placeholders = (roots: UOp[], leaves: UOp[]): { roots: UOp[]; slots: UOp[] } => {
  const slots = leaves.map((l, i) => buffer(`%slot${i}`, l.shape)), memo = new Map<number, UOp>(leaves.map((l, i) => [l.id, slots[i]!]));
  for (const u of toposort(roots)) {
    if (memo.has(u.id)) continue;
    const src = u.src.map((s) => memo.get(s.id)!);
    memo.set(u.id, src.every((s, i) => s === u.src[i]) ? u : node(u.op, src, u.arg, u.shape, u.site));
  }
  return { roots: roots.map((r) => memo.get(r.id)!), slots };
};
// A backend's plans, the 512 used last: for a graph, its plan and the run's leaves bound to
// the plan's slots. A plan pushed out is handed to `drop`. `plansBuilt` counts the plans
// built: a structure found again builds none.
let built = 0;
export const plansBuilt = (): number => built;
export function planner<P>(build: (roots: UOp[]) => P, drop: (plan: P) => void = () => {}): (roots: UOp[]) => { plan: P; bind: Map<number, UOp> } {
  const plans = new Map<string, { plan: P; slots: UOp[] }>();
  return (roots) => {
    const { key, leaves } = canonical(roots);
    let hit = plans.get(key);
    if (hit) plans.delete(key);                                                // most recently used last
    else {
      const p = placeholders(roots, leaves);
      hit = { plan: build(p.roots), slots: p.slots };
      built++;
      if (plans.size >= 512) { const [k, old] = plans.entries().next().value!; plans.delete(k); drop(old.plan); }
    }
    plans.set(key, hit);
    return { plan: hit.plan, bind: new Map(hit.slots.map((s, i) => [s.id, leaves[i]!])) };
  };
}

