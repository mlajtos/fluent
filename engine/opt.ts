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
  range, iconst, iadd, imul, idiv, imod, load, acc, iidx, ival, fused, kInputs, rebuild,
} from "./ir.ts";
import { graphRewriteMany, type Rule } from "./rewrite.ts";

// ---- simplify: the generators' law table (EXACT only — bit-preserving) --------
const scalarOf = (u: UOp): number | null =>
  u.op === "CONSTANT" && u.shape.length === 0 ? (u.arg as Float32Array)[0]! : u.op === "BROADCAST" ? scalarOf(u.src[0]!) : null;
const SIMPLIFY: Rule[] = [
  (u) => (u.op === "DETACH" ? u.src[0]! : null),                              // ∇ captured this already; identity forward
  (u) => (u.op === "MULTIPLY" && scalarOf(u.src[0]!) === 1 ? u.src[1]! : null),
  (u) => (u.op === "MULTIPLY" && scalarOf(u.src[1]!) === 1 ? u.src[0]! : null),
  (u) => (u.op === "ADD" && scalarOf(u.src[0]!) === 0 ? u.src[1]! : null),
  (u) => (u.op === "ADD" && scalarOf(u.src[1]!) === 0 ? u.src[0]! : null),
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
  let bytes = 0;
  const seen = new Set<number>();
  for (const r of roots) for (const u of toposort(r)) {
    if (seen.has(u.id)) continue;
    seen.add(u.id);
    if (u.op === materialized) bytes += numel(u.shape) * 4;
  }
  if (bytes > MEMORY_BUDGET) {
    throw new Error(`this needs ${(bytes / 2 ** 30).toFixed(1)} GB of memory at once – more than the ${MEMORY_BUDGET / 2 ** 30} GB Fluent allows; compute it in smaller pieces`);
  }
};

// Σ-linearity at the SCHEDULE level (reordering float summation is a schedule license —
// the tile and coop tree-reduces already take it). ∇'s fork-summed cotangents (several
// reduce chains added per weight) become ONE chain whose sum-of-products body the split-K
// tile printer catches. The view rules commute identical wrappers inward to expose the
// reduces.
const SCHED_REWRITES: Rule[] = [
  (u) => {                                                                     // ADD(RESHAPE a, RESHAPE b) → RESHAPE(ADD(a, b))
    if (u.op !== "ADD" || u.src[0]!.op !== "RESHAPE" || u.src[1]!.op !== "RESHAPE") return null;
    const a = u.src[0]!.src[0]!, b = u.src[1]!.src[0]!;
    return sameShape(a.shape, b.shape) ? node("RESHAPE", [node("ADD", [a, b], null, a.shape)], u.shape.slice(), u.shape) : null;
  },
  (u) => {                                                                     // ADD(PERMUTE_p a, PERMUTE_p b) → PERMUTE_p(ADD(a, b))
    if (u.op !== "ADD" || u.src[0]!.op !== "PERMUTE" || u.src[1]!.op !== "PERMUTE") return null;
    const p = u.src[0]!.arg as number[], q = u.src[1]!.arg as number[];
    if (p.length !== q.length || p.some((v, i) => v !== q[i])) return null;
    const a = u.src[0]!.src[0]!, b = u.src[1]!.src[0]!;
    return node("PERMUTE", [node("ADD", [a, b], null, a.shape)], p.slice(), u.shape);
  },
  (u) => {                                                                     // ADD(Σ_ax a, Σ_ax b) → Σ_ax(ADD(a, b))
    if (u.op !== "ADD") return null;
    const [ra, rb] = [u.src[0]!, u.src[1]!];
    if (ra.op !== "REDUCE" || rb.op !== "REDUCE") return null;
    const aa = ra.arg as { axis: number; keepdims: boolean; op: string }, ba = rb.arg as { axis: number; keepdims: boolean; op: string };
    if (aa.op !== "ADD" || ba.op !== "ADD" || aa.axis !== ba.axis || aa.keepdims !== ba.keepdims) return null;
    const a = ra.src[0]!, b = rb.src[0]!;
    return sameShape(a.shape, b.shape) ? node("REDUCE", [node("ADD", [a, b], null, a.shape)], ra.arg, u.shape) : null;
  },
];

// ---- the schedule ----------------------------------------------------------------
// Measured platform constants (tests/bench re-derive them): inlining a kernel that carries
// a loop trades a dispatch for `indexSpace·loopExtent` recomputed flops — pays under ~2e7.
export const COST = { reduceBudget: 2e7, perElem: 4096, tile: 16, tileMinM: 2 };
const ELEM = new Set<Op>(["ADD", "MULTIPLY", "RECIPROCAL", "EXP", "LOG", "SQRT", "LESS_THAN", "WHERE", "MATH"]);
const VIEW = new Set<Op>(["BROADCAST", "PERMUTE", "RESHAPE", "SLICE", "WINDOW"]);
const MAX_INPUTS = 6;                                                          // soft cap on kernel growth (inline/splice stop here; mandatory reads may still add)
const HARD_INPUTS = 8;                                                         // past this a kernel is rebuilt materialize-everything. THE TARGET IS THE BROWSER: Chrome/Dawn caps storage buffers at 10/stage (Deno's Metal wgpu falsely reports 31), so inputs+output ≤ 9+1 = 10

function useCounts(roots: UOp[]): Map<number, number> {
  const uc = new Map<number, number>(), seen = new Set<number>();
  const visit = (u: UOp) => { if (seen.has(u.id)) return; seen.add(u.id); for (const s of u.src) { uc.set(s.id, (uc.get(s.id) ?? 0) + 1); visit(s); } };
  for (const r of roots) visit(r);
  return uc;
}
// Materialize a shared elementwise fork iff computing once + traffic beats recomputing it
// in each of k consumers. Approximate (region ops ≈ leaves+1 makes its two known errors
// cancel — measured in the ancestor project); the honest form is a per-kernel cost table.
function schedule(roots: UOp[], uc: Map<number, number>): Set<number> {
  const mat = new Set<number>(), seen = new Set<number>();
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
  for (const r of roots) for (const u of toposort(r)) {
    if (seen.has(u.id)) continue; seen.add(u.id);
    const k = uc.get(u.id) ?? 1;
    if (ELEM.has(u.op) && k > 1 && (regionSize(u) * (k - 1) > k + 1 || regionReduce(u) * (k - 1) > COST.perElem)) mat.add(u.id);
  }
  return mat;
}

const accMemo = new WeakMap<UOp, number>();                                     // values are immutable; the memo keeps the DAG walk linear and dies with the nodes
// the loop work one element of a kernel costs: a loop is its extent times its body's, a
// nest multiplies down, side-by-side loops count once – the costliest of them
const accCost = (v: UOp): number => {
  const m = accMemo.get(v); if (m !== undefined) return m;
  let c = 1;
  for (const s of v.src) c = Math.max(c, accCost(s));
  if (v.op === "ACC") c *= (v.src[0]!.arg as { extent: number }).extent;
  accMemo.set(v, c);
  return c;
};

// the loop variables an index reads
const rangeMemo = new WeakMap<UOp, ReadonlySet<UOp>>();
const rangesIn = (u: UOp): ReadonlySet<UOp> => {
  const m = rangeMemo.get(u); if (m) return m;
  const out = new Set<UOp>(u.op === "RANGE" ? [u] : []);
  for (const s of u.src) for (const r of rangesIn(s)) out.add(r);
  rangeMemo.set(u, out);
  return out;
};

function fuseWith(roots: UOp[], uc: Map<number, number>, mat: Set<number>): UOp[] {
  const built = new Map<number, UOp>();

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
      if (b.op === "FUSED" && chainUse(v) === 1 && slot.get(b.id) === undefined && inputs.length + fresh() <= cap) {
        const c = accCost(b.src[1]!);                                          // c = 1: a pure map folds free; a loop folds while recompute stays cheap
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
        // where, materialized, it runs as a tile. The occupancy floor (outN ≥ 1024) keeps
        // a near-scalar output (a loss) from folding the graph into one serial thread.
        const amp = evals / Math.max(numel(b.shape), 1);
        const traffic = whole && amp <= 1.01 && outN >= 1024;
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
    const val = (v: UOp, ix: UOp, whole = false): UOp => {
      const s = scalarOf(v);
      if (s !== null) return konst(s);                                         // a scalar constant is a literal — no buffer, no slot
      if (ELEM.has(v.op) && !mat.has(v.id) && grown < cap * 3) {               // a fork the schedule did NOT materialize is recomputed in place
        grown++;
        return node(v.op, v.src.map((x) => val(x, ix)), v.arg, []);
      }
      return read(v, ix, regionShape, whole);
    };
    const value = ELEM.has(top.op) ? node(top.op, top.src.map((x) => val(x, at)), top.arg, []) : val(top, at, true);
    return { inputs, value };
  };

  const build = (u: UOp): UOp => {
    const m = built.get(u.id); if (m) return m;
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
        const gid = range(Math.max(numel(outSh), 1));
        let base: UOp = iconst(0);
        for (let d = 0; d < outSh.length; d++) base = iadd(base, imul(imod(idiv(gid, iconst(outSt[d]!)), iconst(outSh[d]!)), iconst(mapped[d]!)));
        const k = range(inSh[a.axis]!);
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
    } else if (u.op === "SCATTER_ADD") {                                       // out[v,c] = Σ_i (idx[i]==v)·values[i,c] — GATHER's transpose, as one nest
      const rest = u.shape.slice(1), R = Math.max(numel(rest), 1), N = u.src[0]!.shape[0]!;
      const gid = range(Math.max(numel(u.shape), 1));
      const i = range(N), v = ival(idiv(gid, iconst(R))), e = load(0, i);
      const nlt = (x: UOp, y: UOp) => node("ADD", [konst(1), node("MULTIPLY", [node("LESS_THAN", [x, y], null, []), konst(-1)], null, [])], null, []);
      const eq = node("MULTIPLY", [nlt(e, v), nlt(v, e)], null, []);           // (¬a<b)·(¬b<a) = [a==b]
      const body = node("MULTIPLY", [eq, load(1, iadd(imul(i, iconst(R)), imod(gid, iconst(R))))], null, []);
      out = fused(gid, acc("ADD", i, body), [build(u.src[0]!), build(u.src[1]!)], u.shape);
    } else throw new Error(`fuse: no kernel form for ${u.op}`);
    built.set(u.id, out);
    return out;
  };
  // Pre-build every node a `read` could reach as a base, in topological order, so build()
  // never recurses deeper than one kernel — real graphs are thousands of nodes deep.
  const seen = new Set<number>();
  for (const r of roots) for (const u of toposort(r)) {
    if (seen.has(u.id)) continue; seen.add(u.id);
    if (u.op === "REDUCE" || u.op === "GATHER" || u.op === "SCATTER_ADD" || ELEM.has(u.op)) build(u);
  }
  return roots.map(build);
}

// ---- optimize: simplify → schedule → fuse; result = leaves + FUSED only --------------
export function optimizeMany(roots: UOp[]): UOp[] {
  const s = graphRewriteMany(simplifyMany(roots), SCHED_REWRITES);
  const uc = useCounts(s);
  const g = fuseWith(s, uc, schedule(s, uc));
  for (const r of g) for (const u of toposort(r))                              // legality: only leaves and kernels may reach the executor
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
  const index = new Map<number, number>(), leaves: UOp[] = [], parts: string[] = [];
  for (const root of roots) {
    const stack: [UOp, boolean][] = [[root, false]];
    while (stack.length) {
      const [u, expanded] = stack.pop()!;
      if (index.has(u.id)) continue;
      if (!expanded && u.src.length) { stack.push([u, true]); for (let i = u.src.length - 1; i >= 0; i--) if (!index.has(u.src[i]!.id)) stack.push([u.src[i]!, false]); continue; }
      let part: string;
      if (u.op === "BUFFER" || (u.op === "CONSTANT" && u.shape.length)) { part = `L${leaves.length}[${u.shape}]`; leaves.push(u); }
      else if (u.op === "CONSTANT") part = `C${(u.arg as Float32Array)[0]}`;
      else part = `${u.op}${u.arg === null || u.arg === undefined ? "" : JSON.stringify(u.arg)}[${u.shape}](${u.src.map((x) => index.get(x.id)).join(",")})`;
      index.set(u.id, parts.length); parts.push(part);
    }
  }
  return { key: `${parts.join(";")}|${roots.map((r) => index.get(r.id)).join(",")}`, leaves };
}

// A plan is built over placeholder leaves, never a run's own: a cached plan holds no data,
// and every run binds its leaves by position.
export const placeholders = (roots: UOp[], leaves: UOp[]): { roots: UOp[]; slots: UOp[] } => {
  const slots = leaves.map((l, i) => buffer(`%slot${i}`, l.shape)), memo = new Map<number, UOp>(leaves.map((l, i) => [l.id, slots[i]!]));
  for (const r of roots) for (const u of toposort(r)) {
    if (memo.has(u.id)) continue;
    const src = u.src.map((s) => memo.get(s.id)!);
    memo.set(u.id, src.every((s, i) => s === u.src[i]) ? u : node(u.op, src, u.arg, u.shape));
  }
  return { roots: roots.map((r) => memo.get(r.id)!), slots };
};

