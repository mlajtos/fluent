// Fluent's tensors over the engine. A Tensor is an immutable graph node of f32 values,
// computed when something reads it. The engine owns the buffers – nothing is freed by hand.

import type { UOp } from "./ir.ts";
import * as ir from "./ir.ts";
import { CPU, type HostData } from "./cpu.ts";
import { GPU } from "./gpu.ts";
import { vjp as graphVjp } from "./grad.ts";
import { batch } from "./batch.ts";

type Axis = number | number[] | null | undefined;
export type Like = Tensor | number | boolean | readonly unknown[];

// A·B at any rank: Σₖ A[…,i,k]·B[…,k,j] over the view algebra; a frame on either operand
// maps the cell over it, and an unframed operand is shared across the frame
export const cellMatmul = (A: UOp, B: UOp): UOp => {
  const as = A.shape, bs = B.shape;
  if (as.length < 2 || bs.length < 2) throw new Error(`matmul: rank ≥ 2 operands, got [${as}]·[${bs}]`);
  const [m, k] = as.slice(-2), [k2, n] = bs.slice(-2);
  if (k !== k2) throw new Error(`matmul: cell [m,k]·[k,n] expected, got [${as.slice(-2)}]·[${bs.slice(-2)}]`);
  const aF = as.slice(0, -2), bF = bs.slice(0, -2);
  if (aF.length && bF.length && String(aF) !== String(bF)) throw new Error(`matmul: frames disagree, [${aF}] vs [${bF}]`);
  const F = aF.length ? aF : bF, T = [...F, m!, k!, n!];
  const p = ir.mul(ir.broadcastTo(ir.reshape(A, as.length > 2 ? [...aF, m!, k!, 1] : [m!, k!, 1]), T),
    ir.broadcastTo(ir.reshape(B, bs.length > 2 ? [...bF, 1, k!, n!] : [1, k!, n!]), T));
  return ir.reshape(ir.reduceAxis(p, F.length + 1, true), [...F, m!, n!]);
};

// ── realization ─────────────────────────────────────────────────────────────────
// A computed node gets one stable leaf that stands for it in every later graph, so nothing
// is computed twice and a long-lived value never drags its history along: a data leaf when
// the CPU computed it, a device leaf naming its buffer when a GPU did. The buffer lives as
// long as that leaf. A small graph over host data runs on the CPU — the metadata a program
// computes (axes, shapes, index tables) never waits on the GPU; the work, not the size of
// the inputs, decides: small weights still make a large training step.
const cpu = new CPU();
let gpu: GPU | null = null;
const SMALL = 1 << 16;
const leafOf = new WeakMap<UOp, UOp>();
const hostData = new WeakMap<UOp, Float32Array>();
// the device buffers values stand on, by leaf name; their bytes, and the most there were
// since the last ask (deviceBytes)
const onDevice = new Map<string, GPUBuffer>();
let live = 0, peak = 0;
const stand = (name: string, b: GPUBuffer): void => { onDevice.set(name, b); live += b.size; peak = Math.max(peak, live); };
const unstand = (name: string): GPUBuffer | undefined => { const b = onDevice.get(name); if (b) { onDevice.delete(name); live -= b.size; } return b; };
const freeOnCollect = new FinalizationRegistry<string>((name) => { const b = unstand(name); if (b) gpu?.release(b); });
let nextLeaf = 0;
// A rank-0 leaf is a one-element leaf seen as rank 0, on the host (see `scalar`) and on the
// device alike, so a scalar that crosses time keeps one structure and its step one plan.
const stored = (u: UOp): UOp =>
  u.op === "RESHAPE" && !u.shape.length && (u.src[0]!.op === "CONSTANT" || u.src[0]!.op === "BUFFER") ? u.src[0]! : u;
const heldData = (u: UOp): Float32Array | undefined => { const s = stored(u); return s.op === "CONSTANT" ? (s.arg as Float32Array) : undefined; };
const isLeaf = (u: UOp) => stored(u).op === "BUFFER" || heldData(u) !== undefined;
const rank0 = (leaf: UOp, shape: readonly number[]): UOp => (shape.length ? leaf : ir.reshape(leaf, []));
const dataOf = (u: UOp): Float32Array | undefined => heldData(leafOf.get(u) ?? u) ?? hostData.get(u);
// the leaf standing for a node, if it has one, and whether the host can't read it cheaply
const leaf = (u: UOp): UOp | undefined => leafOf.get(u) ?? (isLeaf(u) ? u : undefined);
const remote = (l: UOp): boolean => stored(l).op === "BUFFER" || ir.numel(l.shape) > SMALL;
// the edges a walk follows: none out of a leaf
const toLeaves = (u: UOp) => (leaf(u) ? [] : u.src);
// a loop the walk reaches runs there, and is a leaf from then on
const running = (u: UOp) => { if (u.op === "LOOP" && !leafOf.has(u)) runLoop(u); return toLeaves(u); };
// A value made at an earlier instant than the graph that reads it crosses time, as a cell's
// value does: it is computed as a root too (`kept`) and stands on its leaf from then on, so
// what the top of a program works out is not worked out again by every step that reads it.
// Not one larger than what it is made of: a broadcast's expansion costs less recomputed.
let since = 0;
// the language's time moves on: the nodes made from now are the new instant's
export const instant = (): void => { since = ir.made(); };
const base = (u: UOp): UOp => (VIEWS.has(u.op) ? base(u.src[0]!) : u);
function cut(roots: UOp[]): { roots: UOp[]; hostOnly: boolean; kept: UOp[] } {
  const memo = new Map<number, UOp>(), kept = new Set<UOp>();
  let hostOnly = true, work = 0;
  for (const u of ir.toposort(roots, running)) {
    const l = leaf(u);
    if (l) { memo.set(u.id, l); hostOnly &&= !remote(l); continue; }
    const srcs = u.src.map((x) => memo.get(x.id)!);
    memo.set(u.id, srcs.every((x, i) => x === u.src[i]) ? u : ir.node(u.op, srcs, u.arg, u.shape, u.site));
    if (u.src.length) work += ir.numel(u.shape);
    if (u.id >= since) for (const s of u.src) if (s.id < since && !leaf(s) && ir.numel(s.shape) <= Math.max(...s.src.map((x) => ir.numel(base(x).shape)))) kept.add(s);
  }
  const more = [...kept].filter((s) => !roots.includes(s));
  return { roots: [...roots, ...more].map((r) => memo.get(r.id)!), hostOnly: hostOnly && work <= SMALL, kept: more };
}
// A placeholder holds no numbers: a read of one, on either device, throws and marks the
// trace it stands in for – ⍤ then runs f cell by cell (overFrame), ⍣ step by step (loop)
type Trace = { read: boolean };
const placeholderOf = new WeakMap<UOp, Trace>();
let tracing = 0;
const placeholder = (t: Trace, name: string, shape: readonly number[]): UOp => { const p = ir.buffer(name, shape); placeholderOf.set(p, t); return p; };
const tracesIn = (nodes: readonly UOp[]): Trace[] => (tracing ? ir.toposort(nodes, toLeaves).flatMap((u) => placeholderOf.get(u) ?? []) : []);
const readCells = (nodes: readonly UOp[]): void => {
  const ts = tracesIn(nodes);
  for (const t of ts) t.read = true;
  if (ts.length) throw new Error("a traced function reads the numbers of its argument");
};
function compute(nodes: readonly UOp[]): void {
  readCells(nodes);
  // a detach has its source's numbers: the source is computed, and the two share its leaf
  const own = (n: UOp): UOp => (n.op === "DETACH" ? own(n.src[0]!) : n);
  computeOwn([...new Set(nodes.map(own).filter((n) => !leafOf.has(n) && !isLeaf(n)))]);
  for (const n of nodes) { const s = own(n), l = leafOf.get(s); if (s !== n && l && !leafOf.has(n)) { leafOf.set(n, l); const d = hostData.get(s); if (d) hostData.set(n, d); } }
}
function computeOwn(fresh: UOp[]): void {
  if (!fresh.length) return;
  const cutAll = cut(fresh), hostOnly = cutAll.hostOnly, all = [...fresh, ...cutAll.kept];
  // a root that was a loop ran in the cut
  const need = all.filter((n) => !leafOf.has(n)), roots = cutAll.roots.filter((_, i) => !leafOf.has(all[i]!));
  if (!need.length) return;
  if (!gpu || hostOnly) {
    cpu.realizeSync(roots).forEach((t, i) => {
      const n = need[i]!, size = ir.numel(n.shape), d = t.data.length === size ? t.data : t.data.subarray(0, size);   // a kernel writes at least one element
      hostData.set(n, d); leafOf.set(n, held(d, n.shape));
    });
    return;
  }
  gpu.computeSync(roots, (name) => onDevice.get(name)).forEach((b, i) => {
    const n = need[i]!, name = `%v${nextLeaf++}`, buf = ir.buffer(name, n.shape.length ? n.shape : [1]), leaf = rank0(buf, n.shape);
    stand(name, b); freeOnCollect.register(buf, name);
    leafOf.set(n, leaf); leafOf.set(leaf, leaf);
    if (!cutAll.kept.includes(n)) steps.at(-1)?.add(leaf);
  });
}
// the site the ops built now come from – the Fluent call the language is making
export const at = ir.at;
// compute without reading: what a step commits stays where it was computed
export const materialize = (arrays: readonly Tensor[]): void => compute(arrays.map((a) => a.node));
// The work a node's graph would take on the host, or Infinity when it reads the GPU or a
// large constant – memoized per node, so asking along a growing chain stays linear.
const hostWorkMemo = new WeakMap<UOp, number>();
const hostWork = (root: UOp): number => {
  for (const u of ir.toposort(root, (u) => (hostWorkMemo.has(u) ? [] : toLeaves(u)))) {
    if (hostWorkMemo.has(u)) continue;
    const l = leaf(u);
    hostWorkMemo.set(u, l ? (remote(l) ? Infinity : 0) : u.src.reduce((w, x) => w + hostWorkMemo.get(x)!, ir.numel(u.shape)));
  }
  return hostWorkMemo.get(root)!;
};
// a value's data when it is on the host or cheap to compute there — never a GPU read
export function hostValue(a: Tensor): Float32Array | undefined {
  const known = dataOf(a.node);
  if (known || leafOf.has(a.node)) return known;
  readCells([a.node]);
  if (gpu && hostWork(a.node) > SMALL) return undefined;
  compute([a.node]);
  return dataOf(a.node);
}
// a value the GPU computed, read back once and kept on the host beside its leaf
const deviceBuffer = (n: UOp): GPUBuffer => onDevice.get(ir.bufName(stored(leafOf.get(n) ?? n)))!;
const keep = (n: UOp, data: Float32Array): Float32Array => { hostData.set(n, data); hostData.set(leafOf.get(n)!, data); return data; };
async function readAsync(a: Tensor): Promise<Float32Array> {
  compute([a.node]);
  return dataOf(a.node) ?? keep(a.node, await gpu!.readAsync(deviceBuffer(a.node), ir.numel(a.node.shape)));
}
// a computed node's numbers
const readNode = (n: UOp): Float32Array => dataOf(n) ?? keep(n, gpu!.readSync(deviceBuffer(n), ir.numel(n.shape)));
function realize(arrays: readonly Tensor[]): Float32Array[] {
  compute(arrays.map((a) => a.node));
  return arrays.map((a) => readNode(a.node));
}
// A value that outlives its step stands on its leaf — computed, with no history behind it
// for later graphs, and later ∇ walks, to drag along. A scalar is held as data, never kernel
// text, so its next value binds the same plan.
export function settle(a: Tensor): Tensor {
  compute([a.node]);
  const l = leafOf.get(a.node) ?? a.node;
  return new Tensor(l.op === "CONSTANT" && !l.shape.length ? held(l.arg as Float32Array, []) : l);
}
// how many nodes stand behind a value – 1 for a leaf: what crosses time must not grow
export function graphSize(a: Tensor): number {
  const seen = new Set<number>(), stack = [leafOf.get(a.node) ?? a.node];
  while (stack.length) { const u = stack.pop()!; if (seen.has(u.id)) continue; seen.add(u.id); if (!isLeaf(u)) stack.push(...u.src.map((x) => leafOf.get(x) ?? x)); }
  return seen.size;
}
// the device buffer holding a value, for drawing it without a read
function gpuBufferOf(a: Tensor): GPUBuffer {
  if (!gpu) throw new Error("gpuBuffer: no GPU");
  compute([a.node]);
  const l = stored(leafOf.get(a.node) ?? a.node);
  if (l.op === "CONSTANT") {                                                   // host data: upload it once, as a device leaf
    const [b] = gpu.computeSync([l], () => undefined), name = `%v${nextLeaf++}`, dl = ir.buffer(name, l.shape), r = rank0(dl, a.node.shape);
    stand(name, b!); freeOnCollect.register(dl, name);
    leafOf.set(a.node, r); leafOf.set(r, r); keep(a.node, l.arg as Float32Array);
  }
  return deviceBuffer(a.node);
}
// A value nothing reads again – a picture once it is on the screen – gives its device
// buffer back now, not when it is collected
const give = (l: UOp): void => { const s = stored(l), b = s.op === "BUFFER" ? unstand(ir.bufName(s)) : undefined; if (b) gpu!.release(b); };
export function free(a: Tensor): void { const l = leafOf.get(a.node); if (l) give(l); }
export const deviceBytes = (): { live: number; peak: number } => { const p = peak; peak = live; return { live, peak: p }; };

// ── loops ───────────────────────────────────────────────────────────────────────
// A condition is one number, which holds when it is nonzero and not NaN: ⍣, guard and when
// ask it of the whole value, and under ⍤ of each cell. Any other count of numbers is an
// Error, known from the shape alone, that names the ways to ask many.
export function oneNumber(t: Tensor, who: string): Tensor {
  if (t.size === 1) return t;
  const ways = who === "⍣" ? "min(…) waits for all, max(…) stops at the first, and (f ⍣ p) ⍤ 0 lets each element stop on its own"
    : "min(…) holds when all do, max(…) when any does, and f ⍤ 0 runs f, the function it is in, on each element";
  throw new Error(`\`${who === "⍣" ? "(f ⍣ p)" : who}\`: the condition gave ${t.size} numbers, but ${who} asks one question of the whole value: ${ways}`);
}
// Whether ∇ follows any of nodes: they read a value a running ∇ differentiates with respect
// to – through what a cell of ⍤ stands for, never through a detach. What a node reads is
// known once, until a value stops being followed.
const followed = new Set<UOp>(), reads = new Map<number, boolean>();
const follows = (nodes: readonly UOp[]): boolean => {
  if (!followed.size) return false;
  const first = Math.min(...[...followed].map((u) => u.id)), stack = nodes.map((n): [UOp, boolean] => [n, false]);
  const next = (u: UOp): readonly UOp[] => { const cell = traces.find((t) => t.cells.has(u))?.cells.get(u); return cell ? [cell] : u.src; };
  while (stack.length) {
    const [u, back] = stack.pop()!;
    if (reads.has(u.id)) continue;
    if (u.id < first || u.op === "DETACH" || followed.has(u)) reads.set(u.id, followed.has(u));
    else if (back) reads.set(u.id, next(u).some((x) => reads.get(x.id)));
    else stack.push([u, true], ...next(u).map((x): [UOp, boolean] => [x, false]));
  }
  return nodes.some((n) => reads.get(n.id));
};
// ⍣ as one engine step: f runs once, on placeholders shaped like the state xs, and gives
// the next state – and, for a loop until, the verdict: one number, which holds when it is
// nonzero and not NaN. A loop runs where it is built, unless a trace holds what it reads.
// undefined when f reads the state's numbers or gives other shapes, or when ∇ follows the
// loop, which needs its steps: the caller runs it step by step.
export function loop(xs: readonly Tensor[], n: number, until: boolean, f: (xs: Tensor[]) => Tensor[] | undefined): Tensor[] | undefined {
  const t: Trace = { read: false }, carries = xs.map((x) => placeholder(t, "%carry", x.node.shape)), k = xs.length;
  let out: Tensor[] | undefined;
  tracing++;
  try { out = f(carries.map((c) => new Tensor(c))); } finally { tracing--; }
  if (t.read || !out || out.length !== k + (until ? 1 : 0) || out.slice(0, k).some((o, j) => !ir.sameShape(o.node.shape, xs[j]!.node.shape))) return undefined;
  const outs = out.map((o) => o.node);
  if (until) outs[k] = reshape(truthy(oneNumber(out[k]!, "⍣")), []).node;
  const nodes = ir.loop(xs.map((x) => x.node), carries, outs, n), { body } = nodes[0]!.arg as ir.LoopArg;
  // a count loop of 1,024 ops at most is straight-line code: n copies of its step, where read
  const stop = new Set([...body.carries, ...body.frontier]);
  if (!until && n * ir.toposort(body.outs, (u) => (stop.has(u) ? [] : u.src)).length <= 1024) {
    const bound = new Map(body.frontier.map((p) => [p, p] as const));
    let state = xs.map((x) => x.node);
    for (let i = 0; i < n; i++) { body.carries.forEach((c, j) => bound.set(c, state[j]!)); state = substitute(body.outs, bound); }
    return state.map((u) => new Tensor(u));
  }
  if (follows([...nodes[0]!.src, ...body.outs])) return undefined;
  runWhereBuilt(nodes);
  return nodes.map((u) => new Tensor(u));
}
// the loops a value reads run now, unless a trace holds what they read: a loop's Error is
// the value of the call that built it
const runWhereBuilt = (roots: readonly UOp[]): void => {
  const order = ir.toposort(roots, toLeaves);
  if (!order.some((u) => placeholderOf.has(u))) compute(order.filter((u) => u.op === "LOOP"));
};
// A loop with lanes – a verdict for each, the frame of the ⍤ that batched it – runs as a loop
// of one more state, which lanes are done: a lane that is done keeps its state, and the loop
// ends when every lane is done
const laned = (b: ir.Body): { carries: UOp[]; outs: UOp[] } => {
  const k = b.carries.length, v = b.outs[k]!, done = ir.buffer("%done", v.shape), now = ir.where(done, done, v);
  const keep = (o: UOp, c: UOp) => ir.where(ir.broadcastTo(ir.reshape(done, [...v.shape, ...o.shape.slice(v.shape.length).map(() => 1)]), o.shape), c, o);
  return { carries: [...b.carries, done], outs: [...b.outs.slice(0, k).map((o, j) => keep(o, b.carries[j]!)), now, min(new Tensor(now)).node] };
};
// A loop runs on leaves it owns. Each step stands the states' leaves in for the body's
// carries and computes the next states and the verdict as one graph, whose plan is found
// again by its structure: every step runs the plan the first one built. A synchronous loop
// waits for no collection, so what a step computed that no later step reads – the state
// before it, the verdict, an inner loop's leaves – goes back to the device's pool at once,
// and the device holds about two states however long the loop runs. What a loop ends with
// passes to the step around it, if it runs inside one.
// the device leaves each running step computed, and each loop's last run, which its other
// states share
const steps: Set<UOp>[] = [];
const ran = new WeakMap<ir.Body, { src: readonly UOp[]; state: UOp[] }>();
const substitute = (roots: readonly UOp[], bound: ReadonlyMap<UOp, UOp>): UOp[] => {
  const memo = new Map<number, UOp>();
  for (const u of ir.toposort(roots, (u) => (bound.has(u) ? [] : u.src))) {
    const b = bound.get(u), src = b ? [] : u.src.map((s) => memo.get(s.id)!);
    memo.set(u.id, b ?? (src.every((s, i) => s === u.src[i]) ? u : ir.node(u.op, src, u.arg, u.shape, u.site)));
  }
  return roots.map((r) => memo.get(r.id)!);
};
function runLoop(u: UOp): void {
  const { body, n, out } = u.arg as ir.LoopArg, k = body.carries.length, until = body.outs.length > k;
  let last = ran.get(body);
  if (!last || last.src.some((s, i) => s !== u.src[i])) {
    const lanes = until ? body.outs[k]!.shape : [], { carries, outs } = lanes.length ? laned(body) : body, K = carries.length;
    // the first step computes what the body closes over too, once, and any start not
    // computed yet; a literal start is held as data
    const frontier = u.src.slice(k), bound = new Map(body.frontier.map((p, m) => [p, frontier[m]!] as const)), owned = new Set<UOp>(), kept: UOp[] = [];
    let state = u.src.slice(0, k).map((x) => { const l = leaf(x) ?? x; return l.op === "CONSTANT" && !l.shape.length ? held(l.arg as Float32Array, []) : l; });
    if (lanes.length) state.push(ir.broadcastTo(ir.konst(0), lanes));
    for (let i = 0; until || i < n; i++) {
      if (i >= n) throw new Error(`\`(f ⍣ p)\`: the predicate didn't hold within ${n} steps${lanes.length ? ` in ${readNode(state[k]!).filter((d) => !d).length} of ${ir.numel(lanes)} cells` : ""}`);
      carries.forEach((c, j) => bound.set(c, state[j]!));
      const roots = substitute(outs, bound), made = new Set<UOp>();
      let done = false;
      steps.push(made);
      try { compute(i ? roots : [...roots, ...frontier]); done = until && readNode(roots[K]!)[0] !== 0; } finally { steps.pop(); }
      if (!i) body.frontier.forEach((p, m) => { const l = leaf(frontier[m]!)!; bound.set(p, l); if (made.delete(l)) kept.push(l); });
      const next = roots.slice(0, K).map((r) => leaf(r)!);
      for (const l of [...owned, ...made]) if (!next.includes(l)) { owned.delete(l); give(l); }
      for (const l of next) if (made.has(l)) owned.add(l);
      state = next;
      if (done) break;
    }
    compute(state);
    for (const l of [...owned, ...kept]) steps.at(-1)?.add(l);
    ran.set(body, last = { src: u.src, state: state.slice(0, k).map((x) => leaf(x)!) });
  }
  leafOf.set(u, last.state[out]!);
}

const nest = (d: ArrayLike<number>, shape: readonly number[]): unknown => {
  if (!shape.length) return d[0]!;
  const st = ir.strides(shape);
  const go = (axis: number, off: number): unknown[] =>
    Array.from({ length: shape[axis]! }, (_, i) => (axis === shape.length - 1 ? d[off + i]! : go(axis + 1, off + i * st[axis]!)));
  return go(0, 0);
};

// ── folding ─────────────────────────────────────────────────────────────────────
// A value that depends only on constants is computed as it is built and never becomes a
// node: shape arithmetic, index tables and literals cost what their arithmetic costs, and
// never keep a plan from being found. Only small values fold – a large one stays lazy and
// fuses into what reads it – and never a value ∇ differentiates with respect to. Each op
// computes what its kernel computes: f32 storage rounds every result, a reduction
// accumulates in order.
const FOLD = 256;
const FOLDS = new Set(["ADD", "MULTIPLY", "RECIPROCAL", "EXP", "LOG", "SQRT", "LESS_THAN", "WHERE", "DETACH", "MATH", "RANDOM", "IOTA", "REDUCE",
  "BROADCAST", "PERMUTE", "RESHAPE", "SLICE", "WINDOW", "PAD", "GATHER", "SCATTER_ADD"]);
const differentiable = new WeakSet<UOp>();
const foldMemo = new WeakMap<UOp, boolean>();
function isData(u: UOp): boolean {
  return u.op === "CONSTANT" || (u.op === "RESHAPE" && !u.shape.length && u.src[0]!.op === "CONSTANT" && ir.sameShape(u.src[0]!.shape, [1]));
}
function foldable(root: UOp): boolean {
  const stack: UOp[] = [root];
  while (stack.length) {
    const u = stack[stack.length - 1]!;
    if (foldMemo.has(u)) { stack.pop(); continue; }
    let f: boolean;
    if (differentiable.has(u)) f = false;
    else if (isData(u)) f = true;
    else if (!FOLDS.has(u.op) || ir.numel(u.shape) > FOLD) f = false;
    else {
      if (u.src.some((s) => foldMemo.get(s) === false)) f = false;
      else {
        const pending = u.src.filter((s) => !foldMemo.has(s));
        if (pending.length) { stack.push(...pending); continue; }
        f = true;
      }
    }
    foldMemo.set(u, f); stack.pop();
  }
  return foldMemo.get(root)!;
}
function evaluate(u: UOp, ins: Float32Array[]): Float32Array {
  const n = ir.numel(u.shape), out = new Float32Array(n), [a, b, c] = ins;
  const map = (f: (i: number) => number) => { for (let i = 0; i < n; i++) out[i] = f(i); return out; };
  switch (u.op) {
    case "CONSTANT": return u.arg as Float32Array;
    case "IOTA": return map((i) => i);
    case "RANDOM": return map((i) => ir.hashUniform(a![0]!, i));
    case "ADD": return map((i) => a![i]! + b![i]!);
    case "MULTIPLY": return map((i) => a![i]! * b![i]!);
    case "RECIPROCAL": return map((i) => 1 / a![i]!);
    case "EXP": return map((i) => Math.exp(a![i]!));
    case "LOG": return map((i) => Math.log(a![i]!));
    case "SQRT": return map((i) => Math.sqrt(a![i]!));
    case "LESS_THAN": return map((i) => (a![i]! < b![i]! ? 1 : 0));
    case "WHERE": return map((i) => (a![i]! !== 0 ? b![i]! : c![i]!));
    case "DETACH": case "RESHAPE": return a!;
    case "MATH": { const f = ir.MATHS[(u.arg as { fn: string }).fn]!.js; return map((i) => (b ? f(a![i]!, b[i]!) : f(a![i]!))); }
    case "BROADCAST": case "PERMUTE": case "SLICE": case "WINDOW": {
      const m = ir.viewMap(ir.node(u.op, [ir.data(a!, u.src[0]!.shape)], u.arg, u.shape)), os = ir.strides(u.shape);
      return map((i) => { let off = m.offset; for (let d = 0; d < os.length; d++) off += Math.floor(i / os[d]!) % u.shape[d]! * m.strides[d]!; return a![off]!; });
    }
    case "PAD": {
      const { axis, before } = u.arg as { axis: number; before: number }, os = ir.strides(u.shape), ss = ir.strides(u.src[0]!.shape), len = u.src[0]!.shape[axis]!;
      return map((i) => {
        let off = 0;
        for (let d = 0; d < os.length; d++) { const k = Math.floor(i / os[d]!) % u.shape[d]! - (d === axis ? before : 0); if (k < 0 || (d === axis && k >= len)) return 0; off += k * ss[d]!; }
        return a![off]!;
      });
    }
    case "REDUCE": {
      const { axis, op } = u.arg as { axis: number; op: ir.Fold }, s = u.src[0]!.shape, st = ir.strides(s), len = s[axis]!;
      const outer = ir.numel(s.slice(0, axis)), inner = ir.numel(s.slice(axis + 1));
      for (let o = 0; o < outer; o++) for (let j = 0; j < inner; j++) {
        let acc = op === "MAX" ? -Infinity : op === "MUL" ? 1 : 0, m = -Infinity;
        for (let k = 0; k < len; k++) {
          const v = a![o * len * inner + k * st[axis]! + j]!;
          if (op === "ARGMAX") { if (v > m || (v !== v && m === m)) { m = v; acc = k; } }
          else acc = op === "MAX" ? Math.max(acc, v) : Math.fround(op === "MUL" ? acc * v : acc + v);
        }
        out[o * inner + j] = acc;
      }
      return out;
    }
    case "GATHER": {
      const ix = u.src[1]!.shape, B = ix.length - 1, k = ix[B]!, n = u.src[0]!.shape[B]!, R = ir.numel(u.shape.slice(B + 1));
      return map((i) => { const j = Math.floor(i / R); return a![(Math.floor(j / k) * n + Math.trunc(b![j]!)) * R + (i % R)] ?? NaN; });
    }
    case "SCATTER_ADD": {
      const ix = u.src[0]!.shape, B = ix.length - 1, k = ix[B]!, n = (u.arg as { size: number }).size, R = ir.numel(u.shape.slice(B + 1));
      for (let r = 0; r < a!.length; r++) for (let j = 0; j < R; j++) { const at = (Math.floor(r / k) * n + Math.trunc(a![r]!)) * R + j; out[at] = out[at]! + b![r * R + j]!; }
      return out;
    }
  }
  throw new Error(`fold: cannot compute ${u.op}`);
}
const constData = (u: UOp): Float32Array => (u.op === "CONSTANT" ? u.arg : u.src[0]!.arg) as Float32Array;
const isLiteral = (u: UOp): boolean => u.op === "CONSTANT" && !u.shape.length;
// the result as data: a scalar from literals alone is a literal; anything else is held data
const folded = (d: Float32Array, shape: readonly number[], literal: boolean): UOp => {
  const v = d.length === ir.numel(shape) ? d : d.slice(0, ir.numel(shape));
  return literal && !shape.length ? ir.data(v, []) : held(v, shape);
};
const VIEWS = new Set(["BROADCAST", "PERMUTE", "RESHAPE", "SLICE"]);
function fold(root: UOp): UOp {
  if (isData(root)) return root;
  if (differentiable.has(root) || !FOLDS.has(root.op) || ir.numel(root.shape) > FOLD) return root;
  // the common case – one op whose sources are data, or a constructor's view of data –
  // computed directly
  if (root.op !== "IOTA" && root.op !== "RANDOM" && root.src.every((s) => isData(s) || (VIEWS.has(s.op) && isData(s.src[0]!) && !differentiable.has(s)))) {
    const ins = root.src.map((s) => (isData(s) ? constData(s) : evaluate(s, [constData(s.src[0]!)])));
    return folded(evaluate(root, ins), root.shape, root.src.every((s) => isLiteral(isData(s) ? s : s.src[0]!)));
  }
  if (!foldable(root)) return root;
  const vals = new Map<UOp, Float32Array>();
  let literal = true;
  for (const u of ir.toposort(root)) {
    if (isData(u) && u.shape.length) literal = false;                         // a tensor or a held scalar: the result is data too
    if (u.op === "IOTA" || u.op === "RANDOM") literal = false;
    vals.set(u, isData(u) ? constData(u) : evaluate(u, u.src.map((s) => vals.get(s)!)));
  }
  return folded(vals.get(root)!, root.shape, literal);
}
// the value ∇ differentiates with respect to: an identity of its own, never folded away,
// and followed until ∇ has the graph it differentiates
export const traced = (a: Tensor): Tensor => { const n = ir.reshape(a.node, a.node.shape); differentiable.add(n); followed.add(n); return new Tensor(n); };
export const untraced = (ts: readonly Tensor[]): void => { for (const t of ts) followed.delete(t.node); reads.clear(); };

// ── the value ───────────────────────────────────────────────────────────────────
export class Tensor {
  readonly node: UOp;
  constructor(node: UOp) { this.node = fold(node); }
  get shape(): number[] { return this.node.shape.slice(); }
  get ndim(): number { return this.node.shape.length; }
  get size(): number { return ir.numel(this.node.shape); }
  readSync(): Float32Array { return realize([this])[0]!; }
  read(): Promise<Float32Array> { return readAsync(this); }
  async gpuBuffer(): Promise<GPUBuffer> { return gpuBufferOf(this); }
  js(): any { return nest(this.readSync(), this.node.shape); }
  item(): number { return this.readSync()[0]!; }
  // x.slice(i, [a], [a, b], …): an integer drops its axis, [a] runs to the end, [a, b] is a range
  slice(...spec: (number | [number] | [number, number])[]): Tensor {
    let n = this.node;
    const drop: number[] = [];
    spec.forEach((s, axis) => {
      const len = n.shape[axis]!;
      const norm = (v: number) => (v < 0 ? v + len : v);
      const [a, b] = typeof s === "number" ? [norm(s), norm(s) + 1] : [norm(s[0]), s.length > 1 ? norm(s[1]!) : len];
      if (typeof s === "number") drop.push(axis);
      n = ir.slice(n, axis, Math.max(0, a), Math.max(0, Math.min(len, b) - Math.max(0, a)));
    });
    return new Tensor(drop.length ? ir.reshape(n, n.shape.filter((_, i) => !drop.includes(i))) : n);
  }
  *[Symbol.iterator](): IterableIterator<Tensor> { for (let i = 0; i < (this.node.shape[0] ?? 0); i++) yield this.slice(i); }
  toString(): string { return `Tensor([${this.node.shape}])`; }
}

// ── construction ────────────────────────────────────────────────────────────────
const flatten = (v: unknown, out: number[], shape: number[], depth: number): void => {
  if (v instanceof Tensor) { const d = realize([v])[0]!; if (depth === shape.length) shape.push(...v.node.shape); for (let i = 0; i < d.length; i++) out.push(d[i]!); return; }
  if (typeof v === "number" || typeof v === "boolean") { out.push(Number(v)); return; }
  if (ArrayBuffer.isView(v)) { const xs = v as unknown as ArrayLike<number>; if (depth === shape.length) shape.push(xs.length); for (let i = 0; i < xs.length; i++) out.push(Number(xs[i])); return; }
  const xs = v as readonly unknown[];
  if (depth === shape.length) shape.push(xs.length);
  for (const x of xs) flatten(x, out, shape, depth + 1);
};
export function tensor(values: unknown, opts: { shape?: readonly number[] } = {}): Tensor {
  if (values instanceof Tensor) return values;
  let data: Float32Array, shape: number[] = [];
  if (ArrayBuffer.isView(values)) {                                            // a typed array is data already – no trip through a JS array
    data = values instanceof Float32Array ? values.slice() : Float32Array.from(values as unknown as ArrayLike<number>);
    shape = [data.length];
  } else { const flat: number[] = []; flatten(values, flat, shape, 0); data = Float32Array.from(flat); }
  const s = opts.shape ?? shape;
  if (ir.numel(s) !== data.length) throw new Error(`tensor: ${data.length} values do not fill shape [${s}]`);
  return new Tensor(ir.data(data, s));
}
// one value seen at every position: a broadcast constant, never a buffer of copies
export const full = (shape: readonly number[], value: number): Tensor => new Tensor(shape.length ? ir.broadcastTo(ir.konst(value), shape) : ir.konst(value));
export const zeros = (shape: readonly number[]): Tensor => full(shape, 0);
export function arange(start: number, stop?: number, step = 1): Tensor {
  const [a, b] = stop === undefined ? [0, start] : [start, stop];
  const n = Math.max(0, Math.ceil((b - a) / step));
  const i = ir.iota(n), scaled = step === 1 ? i : ir.mul(i, ir.konst(step));
  // counting down, 0 × step is -0: the start is added, though it is 0
  return new Tensor(a === 0 && step > 0 ? scaled : ir.add(scaled, ir.konst(a)));
}
// a scalar held as data – a one-element buffer seen as rank 0 – never folded into a
// kernel as a literal: a value that changes every frame (a clock, a slider, a computed
// number) reuses the compiled plan
const held = (d: Float32Array, shape: readonly number[]): UOp => (shape.length ? ir.data(d, shape) : ir.reshape(ir.data(d, [1]), []));
export const scalar = (v: number): Tensor => new Tensor(held(Float32Array.of(v), []));

// ── elementwise ─────────────────────────────────────────────────────────────────
export const nodeOf = (v: Like): UOp => (v instanceof Tensor ? v.node : typeof v === "number" || typeof v === "boolean" ? ir.konst(Number(v)) : tensor(v).node);
const wrap = (n: UOp) => new Tensor(n);
const un = (f: (x: UOp) => UOp) => (a: Like): Tensor => wrap(f(nodeOf(a)));
const bin = (f: (x: UOp, y: UOp) => UOp) => (a: Like, b: Like): Tensor => wrap(f(nodeOf(a), nodeOf(b)));
const neg = (x: UOp) => ir.mul(x, ir.konst(-1));
const m1 = (fn: string) => un((x) => ir.math(fn, x));

export const add = bin(ir.add);
export const multiply = bin(ir.mul);
export const subtract = bin((x, y) => ir.add(x, neg(y)));
export const divide = bin((x, y) => ir.math("div", x, y));
export const reciprocal = un(ir.recip);
export const sqrt = un(ir.sqrt);
export const exp = un(ir.exp);
export const log = un(ir.log);
// A literal integer exponent is exponentiation by squaring — exact (3^2 is 9, (-2)^2 is 4)
// and differentiable as products are; any other exponent is pow.
const intPow = (x: UOp, e: number): UOp => {
  if (e === 0) return x.shape.length ? ir.broadcastTo(ir.konst(1), x.shape) : ir.konst(1);
  let n = Math.abs(e), sq = x, r: UOp | null = null;
  for (;;) { if (n & 1) r = r ? ir.mul(r, sq) : sq; n >>= 1; if (!n) break; sq = ir.mul(sq, sq); }
  return e < 0 ? ir.recip(r!) : r!;
};
export const power = (a: Like, b: Like): Tensor => {
  const x = nodeOf(a), y = nodeOf(b);
  const e = y.op === "CONSTANT" && !y.shape.length ? (y.arg as Float32Array)[0]! : NaN;
  if (Number.isInteger(e) && Math.abs(e) <= 64) { const r = intPow(x, e); return wrap(ir.sameShape(r.shape, x.shape) ? r : ir.broadcastTo(r, x.shape)); }
  return wrap(ir.math("pow", x, y));
};
export const sin = m1("sin"), cos = m1("cos"), tanh = m1("tanh"), asin = m1("asin"), acos = m1("acos"), atan = m1("atan");
export const asinh = m1("asinh"), acosh = m1("acosh"), atanh = m1("atanh"), atan2 = bin((y, x) => ir.math("atan2", y, x));
export const floor = m1("floor"), trunc = m1("trunc"), round = m1("round"), sign = m1("sign"), abs = m1("abs");
export const expm1 = m1("expm1"), log1p = m1("log1p"), cbrt = m1("cbrt"), isnan = m1("isnan");
export const maximum = bin((x, y) => ir.where(ir.cmplt(x, y), y, x));
export const minimum = bin((x, y) => ir.where(ir.cmplt(y, x), y, x));
export const less = bin(ir.cmplt);
export const equal = bin((x, y) => ir.math("eq", x, y));
// 1 where a value is nonzero; NaN counts as false
export const truthy = un((x) => ir.where(ir.cmplt(x, ir.konst(0)), ir.konst(1), ir.cmplt(ir.konst(0), x)));
export const where = (c: Like, a: Like, b: Like): Tensor => wrap(ir.where(nodeOf(c), nodeOf(a), nodeOf(b)));

// ── shape ───────────────────────────────────────────────────────────────────────
const normAxis = (axis: number, rank: number): number => {
  const a = axis < 0 ? axis + rank : axis;
  if (a < 0 || a >= rank) throw new Error(rank ? `there is no axis ${axis}: the value has ${rank} ${rank === 1 ? "axis" : "axes"}` : `a single number has no axis ${axis}`);
  return a;
};
export function reshape(a: Like, shape: readonly number[]): Tensor {
  const x = nodeOf(a), n = ir.numel(x.shape), known_ = shape.filter((d) => d !== -1).reduce((p, d) => p * d, 1);
  const s = shape.map((d) => (d === -1 ? n / known_ : d));
  if (ir.numel(s) !== n || !s.every((d) => Number.isInteger(d) && d >= 0)) throw new Error(`cannot reshape [${x.shape}] into [${shape}]`);
  return new Tensor(ir.sameShape(s, x.shape) ? x : ir.reshape(x, s));
}
export const transpose = (a: Like, axes?: readonly number[]): Tensor => { const x = nodeOf(a); return wrap(ir.permute(x, axes ? axes.map((p) => normAxis(p, x.shape.length)) : x.shape.map((_, i) => x.shape.length - 1 - i))); };
export function moveaxis(a: Like, source: number | number[], destination: number | number[]): Tensor {
  const x = nodeOf(a), r = x.shape.length;
  const src = (typeof source === "number" ? [source] : source).map((s) => normAxis(s, r));
  const dst = (typeof destination === "number" ? [destination] : destination).map((d) => normAxis(d, r));
  // the axes that stay, in order, with each moved one inserted where it goes
  const order = x.shape.map((_, i) => i).filter((i) => !src.includes(i));
  dst.map((d, i) => [d, src[i]!] as const).sort((p, q) => p[0] - q[0]).forEach(([d, s]) => order.splice(d, 0, s));
  return wrap(ir.permute(x, order));
}
export const padAxis = (a: Like, axis: number, before: number, after: number): Tensor => { const x = nodeOf(a); return wrap(ir.pad(x, normAxis(axis, x.shape.length), before, after)); };
export const windows = (a: Like, sizes: readonly number[]): Tensor => wrap(ir.windows(nodeOf(a), sizes));
export const detach = (a: Like): Tensor => wrap(ir.detach(nodeOf(a)));
export const scatterAdd = (indices: Like, values: Like, size: number): Tensor => {
  const i = nodeOf(indices), v = nodeOf(values), n = ir.numel(i.shape);
  return wrap(ir.scatterAdd(ir.reshape(i, [n]), ir.reshape(v, [n, ...v.shape.slice(i.shape.length)]), size));
};
export const broadcastTo = (a: Like, shape: readonly number[]): Tensor => { const x = nodeOf(a); return wrap(ir.sameShape(x.shape, shape) ? x : ir.broadcastTo(x, shape)); };
// Pieces whose values the host already holds – a literal's numbers, mostly – are joined by
// copying: as a graph, n pieces are n kernels, each adding the whole result again. Not
// for a piece ∇ follows, whose join must stay in the graph.
const joinOnHost = (arrays: readonly Tensor[], shapes: readonly (readonly number[])[], ax: number, shape: readonly number[]): Tensor | null => {
  if (follows(arrays.map((a) => a.node))) return null;
  const datas = arrays.map((a) => dataOf(a.node));
  if (datas.some((d) => d === undefined)) return null;
  const out = new Float32Array(ir.numel(shape)), outer = ir.numel(shape.slice(0, ax));
  let at = 0;
  for (let o = 0; o < outer; o++) {
    datas.forEach((d, p) => {
      const block = ir.numel(shapes[p]!.slice(ax));
      out.set(d!.subarray(o * block, (o + 1) * block), at);
      at += block;
    });
  }
  return new Tensor(held(out, shape));
};
export function concatenate(arrays: readonly Tensor[], axis = 0): Tensor {
  if (!arrays.length) throw new Error("concatenate: needs at least one array");
  const r = arrays[0]!.ndim, ax = normAxis(axis, r);
  const total = arrays.reduce((t, a) => t + a.node.shape[ax]!, 0);
  const shape = arrays[0]!.node.shape.map((d, i) => (i === ax ? total : d));
  return joinOnHost(arrays, arrays.map((a) => a.node.shape), ax, shape) ?? new Tensor(join(arrays.map((a) => a.node), ax));
}
// a sum of pads, each piece zero-padded to the whole: a kernel adds eight of them at most,
// so n pieces join in groups of eight, and the groups in groups of eight – about n/7
// kernels, where one sum of n pads would be n − 7
const join = (xs: UOp[], ax: number): UOp => {
  if (xs.length > 8) return join(Array.from({ length: Math.ceil(xs.length / 8) }, (_, i) => join(xs.slice(8 * i, 8 * i + 8), ax)), ax);
  const total = xs.reduce((t, x) => t + x.shape[ax]!, 0);
  let off = 0;
  return xs.map((x) => { const p = ir.pad(x, ax, off, total - off - x.shape[ax]!); off += x.shape[ax]!; return p; }).reduce(ir.add);
};
export function stack(arrays: readonly Tensor[], axis = 0): Tensor {
  if (!arrays.length) throw new Error("stack: needs at least one array");
  const r = arrays[0]!.ndim + 1, ax = normAxis(axis, r);
  const shapes = arrays.map((a) => [...a.shape.slice(0, ax), 1, ...a.shape.slice(ax)]);
  const shape = [...arrays[0]!.shape.slice(0, ax), arrays.length, ...arrays[0]!.shape.slice(ax)];
  return joinOnHost(arrays, shapes, ax, shape) ?? concatenate(arrays.map((a, i) => reshape(a, shapes[i]!)), ax);
}
// take: out = x[..., idx, ...] along `axis`, idx of any shape
export function take(a: Like, indices: Like, axis?: number): Tensor {
  let x = nodeOf(a);
  if (axis === undefined) { x = ir.reshape(x, [ir.numel(x.shape)]); axis = 0; }
  const ax = normAxis(axis, x.shape.length), idx = nodeOf(indices);
  const front = ax === 0 ? x : ir.permute(x, [ax, ...x.shape.map((_, i) => i).filter((i) => i !== ax)]);
  const flatIdx = idx.shape.length === 1 ? idx : ir.reshape(idx, [Math.max(ir.numel(idx.shape), 1)]);
  let g = ir.gather(front, flatIdx);
  const rest = front.shape.slice(1), outFront = [...idx.shape, ...rest];
  if (!ir.sameShape(g.shape, outFront)) g = ir.reshape(g, outFront);
  if (ax === 0) return wrap(g);
  const k = idx.shape.length, pre = x.shape.slice(0, ax).map((_, i) => k + i);
  const order = [...pre, ...idx.shape.map((_, i) => i), ...rest.slice(ax).map((_, i) => k + ax + i)];
  return wrap(ir.permute(g, order));
}
export function meshgrid(arrays: readonly Tensor[]): Tensor[] {
  const ns = arrays.map((a) => a.size);
  const out = ns.slice(); if (out.length >= 2) [out[0], out[1]] = [ns[1]!, ns[0]!];   // "xy" indexing: the first coordinate varies along columns
  return arrays.map((a, i) => {
    const pos = arrays.length >= 2 && i < 2 ? 1 - i : i;
    const sh = out.map((_, j) => (j === pos ? ns[i]! : 1));
    return broadcastTo(reshape(a, sh), out);
  });
}

// ── reductions ──────────────────────────────────────────────────────────────────
const axesOf = (x: UOp, axis: Axis): number[] => (axis === undefined || axis === null ? x.shape.map((_, i) => i) : (typeof axis === "number" ? [axis] : axis).map((a) => normAxis(a, x.shape.length)));
function reduceN(x: UOp, axis: Axis, opts: { keepdims?: boolean } | undefined, op: "ADD" | "MAX" | "MUL"): UOp {
  const axes = axesOf(x, axis);
  for (const ax of axes) x = ir.reduceAxis(x, ax, true, op);
  return opts?.keepdims ? x : ir.reshape(x, x.shape.filter((_, i) => !axes.includes(i)));
}
const reducer = (op: "ADD" | "MAX" | "MUL", f = (x: UOp) => x) => (a: Like, axis?: Axis, opts?: { keepdims?: boolean }): Tensor => wrap(f(reduceN(f(nodeOf(a)), axis, opts, op)));
export const sum = reducer("ADD"), prod = reducer("MUL"), max = reducer("MAX"), min = reducer("MAX", neg);
// ── order: stable, NaN last; values move through a differentiable gather ─────────
// Data the host has, or computes cheaply, is sorted there at once. Data on the GPU, and a
// value still traced – ⍣'s step, ⍤'s cell – sort in the graph, by a bitonic network:
// log₂m·(log₂m + 1)/2 passes, each pairing every element with a partner and keeping the
// smaller or the larger – one gather and one select, the partner computed in the kernel –
// so nothing is read back.
export function order(d: Float32Array, R: number, n: number, desc: boolean): Float32Array {
  const out = new Float32Array(R * n);
  for (let r = 0; r < R; r++) {
    const row = d.subarray(r * n, r * n + n);
    const idx = Array.from({ length: n }, (_, i) => i).sort((i, j) => {
      const a = row[i]!, b = row[j]!, na = a !== a, nb = b !== b;
      if (na !== nb) return na ? 1 : -1;
      return (na ? 0 : desc ? b - a : a - b) || i - j;                         // equal infinities subtract to NaN: a tie too
    });
    idx.forEach((v, i) => { out[r * n + i] = v; });
  }
  return out;
}
export function bitonic(x: UOp, R: number, n: number, desc: boolean): UOp {
  let p = 0;
  while (1 << p < n) p++;
  const m = 1 << p, k = ir.konst, rows = (u: UOp) => ir.broadcastTo(u, [R, m]);
  const mod2 = (v: UOp) => ir.add(v, ir.mul(ir.math("floor", ir.mul(v, k(0.5))), k(-2)));
  const i = rows(ir.iota(m)), inside = ir.cmplt(i, k(n)), nan = ir.math("isnan", x);
  // compare by (key, tie): the value, negated to sort descending; a NaN keyed +∞ and tied
  // after every number, padding after every NaN, equal values in their order
  const key = ir.where(inside, ir.pad(ir.where(nan, k(Infinity), desc ? ir.mul(x, k(-1)) : x), 1, 0, m - n), k(Infinity));
  const tie = ir.add(i, ir.mul(ir.where(inside, ir.pad(nan, 1, 0, m - n), k(2)), k(m)));
  const pair = (a: UOp, b: UOp) => ir.add(ir.pad(ir.reshape(a, [R, m, 1]), 2, 0, 1), ir.pad(ir.reshape(b, [R, m, 1]), 2, 1, 0));
  const half = (t: UOp, c: number) => ir.reshape(ir.slice(t, 2, c, 1), [R, m]);
  let T = pair(key, tie);
  for (let a = 1; a <= p; a++) for (let b = a - 1; b >= 0; b--) {
    const j = 1 << b, up = ir.cmplt(mod2(ir.math("floor", ir.mul(i, k(1 / (1 << a))))), k(0.5));
    const q = ir.add(i, ir.mul(k(j), ir.add(k(1), ir.mul(mod2(ir.math("floor", ir.mul(i, k(1 / j)))), k(-2)))));
    const Tq = ir.gather(T, q);
    const before = (x1: UOp, t1: UOp, x2: UOp, t2: UOp) => ir.add(ir.cmplt(x1, x2), ir.mul(ir.math("eq", x1, x2), ir.cmplt(t1, t2)));
    const [ks, ts, kq, tq] = [half(T, 0), half(T, 1), half(Tq, 0), half(Tq, 1)];
    const low = ir.math("eq", ir.cmplt(i, q), up);                             // this element keeps the smaller of the pair
    const take = ir.where(low, before(kq, tq, ks, ts), before(ks, ts, kq, tq));
    T = ir.where(ir.broadcastTo(ir.reshape(take, [R, m, 1]), [R, m, 2]), Tq, T);
  }
  const t = half(T, 1);
  return ir.slice(ir.add(t, ir.mul(ir.math("floor", ir.mul(t, k(1 / m))), k(-m))), 1, 0, n);   // tie mod m is the index
}
const scalarRank = (x: UOp) => (x.shape.length ? x : ir.reshape(x, [1]));
export function argsort(a: Like, descending = false): Tensor {
  const x = scalarRank(nodeOf(a)), n = x.shape.at(-1)!, R = ir.numel(x.shape.slice(0, -1));
  const host = tracesIn([x]).length ? undefined : hostValue(new Tensor(x));
  if (host) return new Tensor(ir.data(order(host, R, n, descending), x.shape));
  return new Tensor(ir.reshape(bitonic(ir.reshape(x, [R, n]), R, n, descending), x.shape));
}
// gather each row by its own indices: the rows are a batch
function gatherRows(x: UOp, idx: UOp): UOp {
  const n = x.shape.at(-1)!, k = idx.shape.at(-1)!, R = ir.numel(x.shape) / n;
  return ir.reshape(ir.gather(ir.reshape(x, [R, n]), ir.reshape(idx, [R, k])), [...x.shape.slice(0, -1), k]);
}
export const sort = (a: Like): Tensor => { const x = scalarRank(nodeOf(a)); return wrap(gatherRows(x, argsort(new Tensor(x)).node)); };
export function topK(a: Like, k: number): [Tensor, Tensor] {
  const x = scalarRank(nodeOf(a)), n = x.shape.at(-1)!;
  const all = argsort(new Tensor(x), true).node, idx = k === n ? all : ir.slice(all, x.shape.length - 1, 0, k);
  return [wrap(gatherRows(x, idx)), new Tensor(idx)];
}

// ── contractions ────────────────────────────────────────────────────────────────
export function matmul(a: Like, b: Like): Tensor {
  let x = nodeOf(a), y = nodeOf(b);
  const vecA = x.shape.length === 1, vecB = y.shape.length === 1;
  if (vecA) x = ir.reshape(x, [1, x.shape[0]!]);
  if (vecB) y = ir.reshape(y, [y.shape[0]!, 1]);
  const fx = x.shape.slice(0, -2), fy = y.shape.slice(0, -2);
  // a frame on the left only: its rows join the matrix's rows – one product, whose
  // gradient for y sums over one axis instead of a frame's every axis
  if (fx.length && !fy.length) {
    const [m, k] = x.shape.slice(-2) as [number, number];
    x = ir.reshape(cellMatmul(ir.reshape(x, [ir.numel(fx) * m, k]), y), [...fx, m, y.shape.at(-1)!]);
    return wrap(vecB ? ir.reshape(x, x.shape.slice(0, -1)) : x);
  }
  if (fx.length && fy.length && !ir.sameShape(fx, fy)) {                       // frames broadcast against each other, numpy-style
    const n = Math.max(fx.length, fy.length), F: number[] = [];
    for (let i = 0; i < n; i++) F.push(Math.max(fx[fx.length - n + i] ?? 1, fy[fy.length - n + i] ?? 1));
    x = ir.broadcastTo(x, [...F, ...x.shape.slice(-2)]); y = ir.broadcastTo(y, [...F, ...y.shape.slice(-2)]);
  }
  let r = cellMatmul(x, y);
  if (vecA || vecB) r = ir.reshape(r, r.shape.filter((_, i) => !((vecA && i === r.shape.length - 2) || (vecB && i === r.shape.length - 1))));
  return wrap(r);
}
// numpy's dot: x's last axis against y's second to last, every other axis kept
export function dot(a: Like, b: Like): Tensor {
  const x = nodeOf(a), y = nodeOf(b), r = y.shape.length;
  if (!x.shape.length || !r) return wrap(ir.mul(x, y));
  if (r === 1) return wrap(reduceN(ir.mul(x, ir.broadcastTo(y, x.shape)), -1, {}, "ADD"));
  const K = x.shape.at(-1)!, freeA = x.shape.slice(0, -1), others = y.shape.map((_, i) => i).filter((i) => i !== r - 2), freeB = others.map((i) => y.shape[i]!);
  const A = ir.reshape(ir.permute(x, x.shape.map((_, i) => i)), [Math.max(ir.numel(freeA), 1), K]);
  const B = ir.reshape(ir.permute(y, [r - 2, ...others]), [K, Math.max(ir.numel(freeB), 1)]);
  return wrap(ir.reshape(cellMatmul(A, B), [...freeA, ...freeB]));
}
// ── composites the natives still call ───────────────────────────────────────────
export function oneHot(a: Like, n: number): Tensor {
  const idx = nodeOf(a), shape = [...idx.shape, n];
  const hit = (x: UOp, y: UOp) => ir.mul(ir.add(ir.konst(1), ir.mul(ir.cmplt(x, y), ir.konst(-1))), ir.add(ir.konst(1), ir.mul(ir.cmplt(y, x), ir.konst(-1))));
  return wrap(hit(ir.broadcastTo(ir.reshape(idx, [...idx.shape, 1]), shape), ir.broadcastTo(ir.iota(n), shape)));
}
// N-d cross-correlation of a field with a kernel, SAME zero padding: every position's
// window of the padded field, times the kernel, summed — one reduction over the taps, the
// windows a view that never materializes. The kernel runs over the field's trailing axes;
// any axes before them are a frame.
export function conv(field: Like, kernel: Like): Tensor {
  const x = nodeOf(field), k = nodeOf(kernel), sp = x.shape, ks = k.shape, off = sp.length - ks.length;
  if (off < 0) throw new Error(`conv: a kernel of rank ${ks.length} needs a field of rank ${ks.length} or more, got [${sp}]`);
  let f = x;
  ks.forEach((K, j) => { const lo = Math.floor((K - 1) / 2); if (K > 1) f = ir.pad(f, off + j, lo, K - 1 - lo); });
  const w = ir.windows(f, ks);
  let out = ir.mul(w, ir.broadcastTo(ir.reshape(k, [...sp.map(() => 1), ...ks]), w.shape));
  for (let d = ks.length - 1; d >= 0; d--) out = ir.reduceAxis(out, sp.length + d, false);
  return wrap(out);
}
// a draw is a RANDOM leaf, so it fuses into what reads it; a seed is a number
const mix = (seed: number, salt: number) => (Math.imul((seed ^ Math.imul(salt, 0x27d4eb2f)) >>> 0, 0x165667b1) ^ salt) >>> 0;
// drawn while ⍤ runs f on a cell, a draw is one more cell: every cell draws its own numbers
export const uniform = (seed: number, shape: readonly number[]): Tensor =>
  wrap(traces.reduce((u, t) => cellOf(t, u), ir.random([...traces.flatMap((t) => t.F), ...shape], mix(seed, 1))));
// ── rank: a function of one cell, over a frame ──────────────────────────────────
// f runs once, on placeholders shaped like one cell, and batch gives the graph it built
// the frame F: xs are [...F, ...cell]. A placeholder holds no numbers, so a read of one – f
// branching on its cell's values – throws, and the call gives undefined: the caller runs f
// cell by cell. Traces nest, the outermost first.
type CellTrace = Trace & { F: readonly number[]; cells: Map<UOp, UOp> };
const traces: CellTrace[] = [];
const cellOf = (t: CellTrace, u: UOp): UOp => { const p = placeholder(t, "%cell", u.shape.slice(t.F.length)); t.cells.set(p, u); return p; };
export function overFrame(F: readonly number[], xs: readonly Tensor[], f: (cells: Tensor[]) => unknown): Tensor | undefined {
  const t: CellTrace = { read: false, F, cells: new Map() };
  traces.push(t); tracing++;
  let out: unknown;
  try { out = f(xs.map((x) => new Tensor(cellOf(t, x.node)))); } finally { traces.pop(); tracing--; }
  if (!(out instanceof Tensor) || t.read) return undefined;
  const { root, loops } = batch(out.node, t.cells, F);
  if (loops.length) runWhereBuilt(loops);
  return new Tensor(root);
}
// ── ∇: the engine's transpose ∘ linearize on the graph a function builds ─────────
// The derivative is with respect to a traced argument – this use of the value, never a
// copy of it the function closed over – and an outer ∇ still differentiates through it.
export function vjp<F extends (...xs: any[]) => Tensor>(f: F, primals: Tensor[]): [Tensor, (ct: unknown) => Tensor[]] {
  const leaves = primals.map(traced);
  let out: Tensor;
  try { out = f(...leaves); } finally { untraced(leaves); }
  const seed = (ct: unknown): UOp => (ct instanceof Tensor ? nodeOf(ct) : ir.broadcastTo(ir.konst(Number(ct ?? 1)), out.node.shape));
  return [out, (ct: unknown) => graphVjp(out.node, leaves.map((l) => l.node), seed(ct)).map((n) => new Tensor(n))];
}
// the gradients of value with respect to traced leaves – a ones cotangent, so a
// non-scalar value differentiates element by element
export const gradients = (value: Tensor, wrt: readonly Tensor[]): Tensor[] =>
  graphVjp(value.node, wrt.map((l) => l.node), ir.broadcastTo(ir.konst(1), value.node.shape)).map((n) => new Tensor(n));
// ── devices: WebGPU where the platform has it, the CPU everywhere ────────────────
export async function init(): Promise<void> { gpu ??= await GPU.create(); }
export const engineGpu = (): GPU | null => gpu;
// resolves once at most `depth` computations are still running on the GPU
export const gpuDrained = (depth: number): Promise<void> => gpu?.drained(depth) ?? Promise.resolve();
