// simplify (value-preserving EXACT laws, on the engine) + the SCHEDULE (cost-driven fusion).
// After optimize a graph is leaves + FUSED loop nests, nothing else — one kernel form.
//
// Fusion is ONE mechanism: inline = substitute, materialize = LOAD, the cost model picks.
// That single splice rule IS the algebra's fusion laws — map∘map (L1), reduce-absorbs-map
// (L2), view-folds-into-the-read (L4), broadcast-back reduce inlining (rmsNorm/softmax as
// one kernel) and matmul epilogues (bias/activation/residual into the ACC's consumer) all
// fall out of "a single-use kernel folds into its reader, guarded by recompute cost".

import type { UOp, Op } from "./ir.ts";
import {
  node, konst, buffer, numel, strides, sameShape, permute, broadcastTo, toposort, viewMap, bcIndex,
  range, iconst, iadd, imul, idiv, imod, load, acc, iidx, ival, fused, kInputs, rebuild, at, siteLabel,
} from "./ir.ts";
import { graphRewriteMany, type Rule } from "./rewrite.ts";

// ---- simplify: the generators' law table (EXACT only — bit-preserving) --------
const scalarOf = (u: UOp): number | null =>
  u.op === "CONSTANT" && u.shape.length === 0 ? (u.arg as Float32Array)[0]! : u.op === "BROADCAST" ? scalarOf(u.src[0]!) : null;
const is = (u: UOp, v: number): boolean => Object.is(scalarOf(u), v);
const SIMPLIFY: Rule[] = [
  (u) => (u.op === "DETACH" ? u.src[0]! : null),                              // ∇ captured this already; identity forward
  (u) => (u.op === "MULTIPLY" && is(u.src[0]!, 1) ? u.src[1]! : null),
  (u) => (u.op === "MULTIPLY" && is(u.src[1]!, 1) ? u.src[0]! : null),
  // x + −0 is x for every x; x + 0 is not where x is −0
  (u) => (u.op === "ADD" && is(u.src[0]!, -0) ? u.src[1]! : null),
  (u) => (u.op === "ADD" && is(u.src[1]!, -0) ? u.src[0]! : null),
  (u) => ((u.op === "ADD" || u.op === "MULTIPLY") && u.src[0]!.op === "CONSTANT" && u.src[1]!.op === "CONSTANT" && !u.shape.length
    ? konst(u.op === "ADD" ? (u.src[0]!.arg as Float32Array)[0]! + (u.src[1]!.arg as Float32Array)[0]! : (u.src[0]!.arg as Float32Array)[0]! * (u.src[1]!.arg as Float32Array)[0]!) : null),
  // the view laws — pure indexing, so they may run in the default (bit-exact) pipeline
  (u) => (u.op === "RESHAPE" && sameShape(u.src[0]!.shape, u.shape) ? u.src[0]! : null),
  (u) => (u.op === "BROADCAST" && sameShape(u.src[0]!.shape, u.shape) ? u.src[0]! : null),
  (u) => (u.op === "PERMUTE" && (u.arg as number[]).every((p, i) => p === i) ? u.src[0]! : null),
  (u) => (u.op === "PERMUTE" && u.src[0]!.op === "PERMUTE" ? permute(u.src[0]!.src[0]!, (u.arg as number[]).map((i) => (u.src[0]!.arg as number[])[i]!)) : null),
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
// reshape or permute in the way of one.
const SCHED_REWRITES: Rule[] = [(u) => {
  const [x, y] = u.src as [UOp, UOp];
  if (u.op !== "ADD" || x.op !== y.op || !(x.op === "RESHAPE" || x.op === "PERMUTE" || (x.op === "REDUCE" && (x.arg as { op: string }).op === "ADD"))) return null;
  const a = x.src[0]!, b = y.src[0]!;
  return JSON.stringify(x.arg) === JSON.stringify(y.arg) && sameShape(a.shape, b.shape) ? node(x.op, [node("ADD", [a, b], null, a.shape)], x.arg, u.shape) : null;
}];

// ---- the schedule ----------------------------------------------------------------
// Measured platform constants: inlining a kernel that carries a loop trades a dispatch for
// `indexSpace·loopExtent` recomputed flops — pays under ~2e7. A contraction shorter than
// tileMinK runs as a plain nest, not a tile: tests/bench-tile.ts re-derives that one. The
// workgroups that fill the machine (fill), the fewest steps a thread of a split loop runs
// (groupSteps) and the outputs, side by side in memory, from which a thread each beats a
// workgroup each (coopFew) pick a reduction's form on the GPU: tests/bench-reduce.ts
// re-derives them.
export const COST = { reduceBudget: 2e7, perElem: 4096, tile: 16, tileMinM: 2, tileMinK: 8, fill: 96, groupSteps: 4, coopFew: 256 };
const ELEM = new Set<Op>(["ADD", "MULTIPLY", "RECIPROCAL", "EXP", "LOG", "SQRT", "LESS_THAN", "WHERE", "MATH"]);
const VIEW = new Set<Op>(["BROADCAST", "PERMUTE", "RESHAPE", "SLICE", "WINDOW"]);
const MAX_INPUTS = 6;                                                          // soft cap on kernel growth (inline/splice stop here; mandatory reads may still add)
const HARD_INPUTS = 8;                                                         // past this a kernel is rebuilt materialize-everything. THE TARGET IS THE BROWSER: Chrome/Dawn caps storage buffers at 10/stage (Deno's Metal wgpu falsely reports 31), so inputs+output ≤ 9+1 = 10

function useCounts(roots: UOp[]): Map<number, number> {
  const uc = new Map<number, number>();
  for (const u of toposort(roots)) for (const s of u.src) uc.set(s.id, (uc.get(s.id) ?? 0) + 1);
  return uc;
}
// Materialize a shared elementwise fork iff computing once + traffic beats recomputing it
// in each of k consumers. Approximate (region ops ≈ leaves+1 makes its two known errors
// cancel — measured in the ancestor project); the honest form is a per-kernel cost table.
function schedule(roots: UOp[], uc: Map<number, number>): Set<number> {
  const mat = new Set<number>();
  const regionSize = (u: UOp): number => { let n = 1; for (const s of u.src) if (ELEM.has(s.op) && (uc.get(s.id) ?? 0) <= 1) n += regionSize(s); return n; };
  // A reduction the fork's single-use region reads is recomputed, per element, by every
  // consumer the fork is inlined into — its extent each time. Past the per-element work
  // budget that costs more than materializing: a weight-grad reduce above the optimizer's
  // three updates is computed once, a softmax row's max is not split out of its kernel.
  const viewBase = (x: UOp): UOp => { while (VIEW.has(x.op)) x = x.src[0]!; return x; };
  const regionReduce = (u: UOp): number => {
    let n = 0;
    for (const s of u.src) {
      const b = viewBase(s);
      if (ELEM.has(s.op) && (uc.get(s.id) ?? 0) <= 1) n += regionReduce(s);
      else if (b.op === "REDUCE" && (uc.get(s.id) ?? 0) <= 1 && (uc.get(b.id) ?? 0) <= 1) n += b.src[0]!.shape[(b.arg as { axis: number }).axis]!;
    }
    return n;
  };
  for (const u of toposort(roots)) {
    const k = uc.get(u.id) ?? 1;
    if (ELEM.has(u.op) && k > 1 && (regionSize(u) * (k - 1) > k + 1 || regionReduce(u) * (k - 1) > COST.perElem)) mat.add(u.id);
  }
  return mat;
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

// `loaded` names the loop nests a plan materializes anyway: every reader loads them, none
// recomputes one. The pass adds the nests its own plan loads.
function fuseWith(roots: UOp[], uc: Map<number, number>, mat: Set<number>, loaded: Set<number>): UOp[] {
  const built = new Map<number, UOp>(), source = new Map<UOp, number>();

  // Build one kernel's scalar value for `top` over the index space `regionShape`, read at
  // index expression `at`; `loop` is the reduction's loop variable, if the kernel has one.
  // `cap` bounds growth: inlining and splicing stop there, but mandatory reads may still
  // push past it — the caller retries an over-wide kernel with cap 1 (materialize
  // everything non-mandatory).
  const kernel = (top: UOp, regionShape: readonly number[], at: UOp, cap = MAX_INPUTS, outN = Math.max(numel(regionShape), 1), loop?: UOp): { inputs: UOp[]; value: UOp } => {
    const inputs: UOp[] = [], slot = new Map<number, number>(), spliced = new Map<string, UOp>();
    let grown = 0;                                                             // ops inlined so far — bounds the region BEFORE its reads materialize
    const addInput = (b: UOp): number => { let i = slot.get(b.id); if (i === undefined) { i = inputs.length; inputs.push(b); slot.set(b.id, i); } return i; };
    const iKey = (u: UOp): string => `${u.op}:${u.op === "RANGE" ? u.id : u.op === "ICONST" ? u.arg : ""}(${u.src.map(iKey).join(",")})`;
    const splice = (f: UOp, ix: UOp): UOp => {                                 // substitute f's gid with ix, remapping LOAD slots into this kernel
      const key = `${f.id}|${iKey(ix)}`;                                       // two reads of one base at one index share the spliced value (an ACC computes once)
      const hit = spliced.get(key); if (hit) return hit;
      const gid = f.src[0]!, ins = kInputs(f), memo = new Map<number, UOp>();
      const go = (v: UOp): UOp => {
        if (v.id === gid.id) return ix;
        const m = memo.get(v.id); if (m) return m;
        const o = v.op === "RANGE" ? range((v.arg as { extent: number }).extent)   // fresh loop vars per splice site
          : v.op === "LOAD" ? load(addInput(ins[(v.arg as { slot: number }).slot]!), go(v.src[0]!))
          : v.src.length ? rebuild(v.op, v.src.map(go), v.arg, v.shape) : v;
        memo.set(v.id, o); return o;
      };
      const out = go(f.src[1]!);
      spliced.set(key, out); return out;
    };
    // Use counts survive the view collapse: a multi-consumer reshape over a
    // single-consumer REDUCE reads as shared, so the reduce is never spliced into each
    // of the reshape's consumers. The maximum use along the whole view chain counts.
    const chainUse = (x: UOp): number => {
      let m = uc.get(x.id) ?? 1;
      while (VIEW.has(x.op)) { x = x.src[0]!; m = Math.max(m, uc.get(x.id) ?? 1); }
      return m;
    };
    // fold the view chain, then compute, splice or LOAD the base; `ix` is a flat index
    // into `shape` (the region's, except where a PAD reads its smaller source)
    const read = (v: UOp, ix: UOp, shape: readonly number[] = regionShape, whole = false): UOp => {
      const vm = viewMap(v);
      let e = bcIndex(vm.strides, shape, ix);
      if (vm.offset) e = iadd(e, iconst(vm.offset));
      if (vm.base.op === "RANDOM") return node("RAND", [e, load(addInput(build(vm.base.src[0]!)), iconst(0))], null, []);
      if (vm.base.op === "IOTA") return ival(e);
      if (vm.base.op === "PAD") return readPad(vm.base, e);
      const b = build(vm.base);
      // the inputs a splice adds: a second read of a spliced kernel adds none
      const fresh = () => kInputs(b).filter((x) => !slot.has(x.id)).length;
      const c = b.op === "FUSED" ? accCost(b.src[1]!) : 0;                     // c = 1: a pure map folds free; a loop folds while recompute stays cheap
      // a loop nest the plan materializes is loaded, never also recomputed
      if (b.op === "FUSED" && chainUse(v) === 1 && slot.get(b.id) === undefined && inputs.length + fresh() <= cap && !(c > 1 && loaded.has(vm.base.id))) {
        // a spliced value runs once a thread, and again each step of the reduction's loop
        // only where its index reads the loop variable – the printers hoist it otherwise
        const evals = outN * (loop && rangesIn(e).has(loop) ? (loop.arg as { extent: number }).extent : 1);
        // two budgets: TOTAL recompute (dispatch-vs-flops) and PER-OUTPUT-ELEMENT work —
        // a splice that squares per-thread loops at low thread counts starves the GPU
        // (2048 threads × 8192 sequential ops is a 100 ms kernel however you dispatch it)
        const perElem = (evals / outN) * c;
        // perElem prices RECOMPUTE, not inherent contraction length. At amplification ≈ 1
        // (each producer element consumed exactly once) a reduce-into-reduce splice that
        // becomes the reader's whole body does the two-stage work minus the intermediate
        // round trip — traffic elimination, so it is exempt from both budgets: the folded
        // chain of sums is what the split-K tile printer catches. Spliced into an
        // elementwise body instead, the contraction would run as serial loops per thread
        // where, materialized, it runs as a tile. A scalar output (a loss) never folds: the
        // graph would run as one serial thread.
        const amp = evals / Math.max(numel(b.shape), 1);
        const traffic = whole && amp <= 1.01 && outN >= 2;
        // a loop spliced into a reduction's body runs once per step of the outer loop, on
        // one thread – materialized it runs on its own, in parallel; only a short one pays
        const nested = evals > outN && c > 16;
        // TOTAL recompute: what the reader evaluates beyond computing b once. A loop as long
        // as a tile is priced whole: materialized, a contraction runs as a tile
        const extra = (c >= COST.tile ? evals : evals - Math.max(numel(b.shape), 1)) * c;
        if (c === 1 || traffic || (!nested && perElem <= COST.perElem && extra <= COST.reduceBudget)) return splice(b, e);
      }
      return load(addInput(b), e);
    };
    // a padded read: the source where the index lands inside it, 0 around it
    const readPad = (p: UOp, e: UOp): UOp => {
      const { axis, before } = p.arg as { axis: number; before: number }, src = p.src[0]!;
      const pst = strides(p.shape), sst = strides(src.shape), len = src.shape[axis]!;
      let srcIdx: UOp = iconst(0);
      for (let d = 0; d < p.shape.length; d++) srcIdx = iadd(srcIdx, imul(imod(idiv(e, iconst(pst[d]!)), iconst(p.shape[d]!)), iconst(sst[d]!)));
      const c = ival(imod(idiv(e, iconst(pst[axis]!)), iconst(p.shape[axis]!)));
      const below = node("LESS_THAN", [c, konst(before + len)], null, []);
      const inside = before ? node("MULTIPLY", [below, node("ADD", [konst(1), node("MULTIPLY", [node("LESS_THAN", [c, konst(before)], null, []), konst(-1)], null, [])], null, [])], null, []) : below;
      const at = iadd(srcIdx, iconst(-before * sst[axis]!));                 // wraps below 0 only where `inside` is 0
      return node("WHERE", [inside, read(src, at, src.shape), konst(0)], null, []);
    };
    // every value is read at `at`, so a node reached twice is one value: x·x·x·x is 2 multiplies
    const vals = new Map<number, UOp>();
    const val = (v: UOp, whole = false): UOp => {
      let o = vals.get(v.id);
      if (o) return o;
      const s = scalarOf(v);
      if (s !== null) o = konst(s);                                            // a scalar constant is a literal — no buffer, no slot
      else if (ELEM.has(v.op) && !mat.has(v.id) && grown < cap * 3) {          // a fork the schedule did NOT materialize is recomputed in place
        grown++;
        o = node(v.op, v.src.map((x) => val(x)), v.arg, []);
      } else o = read(v, at, regionShape, whole);
      vals.set(v.id, o);
      return o;
    };
    const value = ELEM.has(top.op) ? node(top.op, top.src.map((x) => val(x)), top.arg, []) : val(top, true);
    return { inputs, value };
  };

  const build = (u: UOp): UOp => {
    const m = built.get(u.id); if (m) return m;
    const outer = at.site;
    at.site = u.site;                                                          // a kernel comes from the node it computes
    let out: UOp;
    if (u.op === "CONSTANT" || u.op === "BUFFER") out = u;
    else if (ELEM.has(u.op) || VIEW.has(u.op) || u.op === "PAD" || u.op === "RANDOM" || u.op === "IOTA") {   // a map kernel; a view reaching build is a root → a strided copy nest
      // an UNFOLDABLE reshape (a genuine reinterpret of a non-contiguous view) is still
      // flat-identical to its source: copy the SOURCE region and relabel the shape.
      const top = VIEW.has(u.op) && viewMap(u).base === u ? u.src[0]! : u;
      const mk = (cap: number) => { const gid = range(Math.max(numel(u.shape), 1)); const { inputs, value } = kernel(top, top.shape, gid, cap); return fused(gid, value, inputs, u.shape); };
      out = mk(MAX_INPUTS);
      if (out.src.length - 2 > HARD_INPUTS) out = mk(0);                       // mandatory reads overflowed the device's binding slots: materialize everything
    } else if (u.op === "REDUCE") {                                            // an ACC over the reduced axis; the producer chain fuses into its body
      const a = u.arg as { axis: number; keepdims: boolean; op: "ADD" | "MAX" | "MUL" };
      const inSh = u.src[0]!.shape, inSt = strides(inSh), outSh = u.shape, outSt = strides(outSh);
      const mapped = outSh.map((_, d) => inSt[a.keepdims ? d : (d < a.axis ? d : d + 1)]!);
      const mk = (cap: number) => {
        const gid = range(Math.max(numel(outSh), 1)), k = range(inSh[a.axis]!);
        let base: UOp = iconst(0);
        for (let d = 0; d < outSh.length; d++) base = iadd(base, imul(imod(idiv(gid, iconst(outSt[d]!)), iconst(outSh[d]!)), iconst(mapped[d]!)));
        const { inputs, value } = kernel(u.src[0]!, inSh, iadd(base, imul(k, iconst(inSt[a.axis]!))), cap, Math.max(numel(outSh), 1), k);
        return fused(gid, acc(a.op, k, value), inputs, u.shape);
      };
      out = mk(MAX_INPUTS);
      if (out.src.length - 2 > HARD_INPUTS) out = mk(0);
    } else if (u.op === "GATHER") {                                            // out[i] = table[idx[i]] — the table read through ITS view map, never copied
      const rest = u.shape.slice(1), R = Math.max(numel(rest), 1), rs = strides(rest);
      const gid = range(Math.max(numel(u.shape), 1));
      const tv = viewMap(u.src[0]!), tb = build(tv.base), ib = build(u.src[1]!);
      const row = iidx(load(1, idiv(gid, iconst(R))));
      let addr = iadd(iconst(tv.offset), imul(row, iconst(tv.strides[0]!)));
      for (let d = 0; d < rest.length; d++) addr = iadd(addr, imul(imod(idiv(imod(gid, iconst(R)), iconst(rs[d]!)), iconst(rest[d]!)), iconst(tv.strides[d + 1]!)));
      out = fused(gid, load(0, addr), [tb, ib], u.shape);                      // slots: 0 = table base, 1 = idx
    } else if (u.op === "SCATTER_ADD") {                                       // out[v,c] = Σ_i (idx[i]==v ? values[i,c] : 0) — GATHER's transpose, as one nest
      const rest = u.shape.slice(1), R = Math.max(numel(rest), 1), N = u.src[0]!.shape[0]!;
      const gid = range(Math.max(numel(u.shape), 1));
      // an index names the row of its whole part, the row GATHER reads; NaN names none
      const i = range(N), v = ival(idiv(gid, iconst(R))), hit = node("MATH", [node("MATH", [load(0, i)], { fn: "trunc" }, []), v], { fn: "eq" }, []);
      // selected, not multiplied: an ∞ in another row's values is not 0·∞ here
      const body = node("WHERE", [hit, load(1, iadd(imul(i, iconst(R)), imod(gid, iconst(R)))), konst(0)], null, []);
      out = fused(gid, acc("ADD", i, body), [build(u.src[0]!), build(u.src[1]!)], u.shape);
    } else throw new Error(`fuse: no kernel form for ${u.op}`);
    built.set(u.id, out); source.set(out, u.id);
    at.site = outer;
    return out;
  };
  // Pre-build every node a `read` could reach as a base, in topological order, so build()
  // never recurses deeper than one kernel — real graphs are thousands of nodes deep.
  for (const u of toposort(roots)) if (u.op === "REDUCE" || u.op === "GATHER" || u.op === "SCATTER_ADD" || ELEM.has(u.op)) build(u);
  const plan = roots.map(build);
  for (const k of toposort(plan, (u) => (u.op === "FUSED" ? kInputs(u) : [])))
    for (const x of k.op === "FUSED" ? kInputs(k) : []) if (x.op === "FUSED" && accCost(x.src[1]!) > 1) loaded.add(source.get(x)!);
  return plan;
}

// ---- optimize: simplify → schedule → fuse; result = leaves + FUSED only --------------
export function optimizeMany(roots: UOp[]): UOp[] {
  const s = graphRewriteMany(simplifyMany(roots), SCHED_REWRITES);
  const uc = useCounts(s);
  const mat = schedule(s, uc), loaded = new Set<number>();
  // a loop nest the plan materializes is computed once: fuse again, every reader loading
  // the nests the last plan loads, until a plan loads no new nest
  let g: UOp[], n: number;
  do { n = loaded.size; g = fuseWith(s, uc, mat, loaded); } while (loaded.size > n);
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
// the plan's slots. A plan pushed out is handed to `drop`.
export function planner<P>(build: (roots: UOp[]) => P, drop: (plan: P) => void = () => {}): (roots: UOp[]) => { plan: P; bind: Map<number, UOp> } {
  const plans = new Map<string, { plan: P; slots: UOp[] }>();
  return (roots) => {
    const { key, leaves } = canonical(roots);
    let hit = plans.get(key);
    if (hit) plans.delete(key);                                                // most recently used last
    else {
      const p = placeholders(roots, leaves);
      hit = { plan: build(p.roots), slots: p.slots };
      if (plans.size >= 512) { const [k, old] = plans.entries().next().value!; plans.delete(k); drop(old.plan); }
    }
    plans.set(key, hit);
    return { plan: hit.plan, bind: new Map(hit.slots.map((s, i) => [s.id, leaves[i]!])) };
  };
}

