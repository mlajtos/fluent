// Print loop nests as WGSL and run them on WebGPU. Codegen is a rewrite (FUSED → KERNEL);
// the executor never mentions an op. Index math, view folding and reduce loops are all
// decided in the IR — cpu.ts walks the same nodes. Every value is f32.

import type { UOp } from "./ir.ts";
import { node, numel, kInputs, bufName, toposort, MATHS, WGSL_HASH_UNIFORM, range, iconst, iadd, imul, idiv, imod, load, rebuild } from "./ir.ts";
import { graphRewriteMany } from "./rewrite.ts";
import { optimizeMany, COST, planner, checkMemory, rangesIn, MEMORY_BUDGET } from "./opt.ts";

// Dispatches per queue.submit. Submitting each kernel alone costs ~15× in dispatch
// overhead; one submit per step wedges some drivers and starves the compositor. 16–32 is
// where measured runtimes sit (TVM's sweep peaks at 32).
const SUBMIT_CHUNK = 32;
// Chrome on Mac allows 10 storage buffers per stage: 9 inputs and the output; and 65,535
// workgroups per dispatch axis. Past either it drops the command buffer, without an error.
const MAX_BINDINGS = 10, MAX_GROUPS = 65535;

// WGSL rejects NaN and ±∞ from constant expressions, and the shader compiler may fold a
// −0.0 literal to 0.0; these build them at runtime. WGSL's max may drop a NaN: maxf keeps
// it, as Math.max does on the CPU.
const WGSL_SPECIALS = `fn nanf() -> f32 { var b = 0x7fc00000u; return bitcast<f32>(b); }
fn inff() -> f32 { var b = 0x7f800000u; return bitcast<f32>(b); }
fn negzf() -> f32 { var b = 0x80000000u; return bitcast<f32>(b); }
fn maxf(a: f32, b: f32) -> f32 { return select(max(a, b), nanf(), (bitcast<u32>(a) & 0x7fffffffu) > 0x7f800000u || (bitcast<u32>(b) & 0x7fffffffu) > 0x7f800000u); }`;
const wgslF32 = (v: number): string => {
  if (!Number.isFinite(v)) return v > 0 ? "inff()" : v < 0 ? "(-inff())" : "nanf()";
  if (Object.is(v, -0)) return "negzf()";
  const s = v.toString().replace("e+", "e");
  return /[.e]/.test(s) ? s : s + ".0";
};

// One renderer for both algebras: index ops print u32 expressions, value ops f32.
//
// A shared subexpression is inlined unless it is big: the Metal compiler CSEs better than
// the printer does, and every `let` pins a value into a register — so only a shared subtree
// large enough that re-inlining would bloat the shader becomes a `let`, emitted at the
// OUTERMOST scope its loop variables allow (loop-invariant reductions hoist out).
interface Ctx { names: Map<number, string>; ctr: { n: number }; uses: Map<number, number>; deps: Map<number, Set<number>>; size: Map<number, number>; scopeOf: Map<number, number>; scopes: string[][]; bound: Map<number, string> }
const ctx = (names: Map<number, string>, scopeOf = new Map<number, number>()): Ctx =>
  ({ names, ctr: { n: 0 }, uses: new Map(), deps: new Map(), size: new Map(), scopeOf, scopes: [[]], bound: new Map() });
const planUses = (value: UOp, c: Ctx): void => {                               // one pre-pass: use counts, the RANGEs each node needs, subtree sizes
  const seen = new Set<number>();
  const walk = (v: UOp): void => {
    c.uses.set(v.id, (c.uses.get(v.id) ?? 0) + 1);
    if (seen.has(v.id)) return;
    seen.add(v.id);
    const d = new Set<number>();
    if (v.op === "RANGE") d.add(v.id);
    for (const s of v.src) { walk(s); for (const x of c.deps.get(s.id)!) d.add(x); }
    if (v.op === "ACC") d.delete(v.src[0]!.id);                                // an ACC's own range is bound inside it
    c.deps.set(v.id, d);
    let sz = 1; for (const s of v.src) sz += c.size.get(s.id) ?? 1; c.size.set(v.id, sz);
  };
  walk(value);
};
const scopeIdx = (u: UOp, c: Ctx): number => { let s = 0; for (const r of c.deps.get(u.id)!) s = Math.max(s, c.scopeOf.get(r) ?? 0); return s; };
function render(u: UOp, c: Ctx): string {
  const hit = c.bound.get(u.id);
  if (hit) return hit;
  const r = (s: UOp) => render(s, c);
  let e: string;
  switch (u.op) {
    case "RANGE": return c.names.get(u.id) ?? ((): never => { throw new Error("gpu: unbound RANGE"); })();
    case "ICONST": { const v = u.arg as number; return `${v < 0 ? 4294967296 + v : v}u`; }   // a negative offset wraps, as u32 arithmetic does
    case "CONSTANT": return wgslF32((u.arg as Float32Array)[0]!);
    case "IADD": e = `(${r(u.src[0]!)} + ${r(u.src[1]!)})`; break;
    case "IMUL": e = `(${r(u.src[0]!)} * ${r(u.src[1]!)})`; break;
    case "IDIV": e = `(${r(u.src[0]!)} / ${r(u.src[1]!)})`; break;
    case "IMOD": e = `(${r(u.src[0]!)} % ${r(u.src[1]!)})`; break;
    case "IIDX": e = `u32(${r(u.src[0]!)})`; break;
    case "IVAL": e = `f32(${r(u.src[0]!)})`; break;
    case "LOAD": e = `a${(u.arg as { slot: number }).slot}[${r(u.src[0]!)}]`; break;
    case "ADD": e = `(${r(u.src[0]!)} + ${r(u.src[1]!)})`; break;
    case "MULTIPLY": e = `(${r(u.src[0]!)} * ${r(u.src[1]!)})`; break;
    case "RECIPROCAL": e = `(1.0 / ${r(u.src[0]!)})`; break;
    case "EXP": e = `exp(${r(u.src[0]!)})`; break;
    case "LOG": e = `log(${r(u.src[0]!)})`; break;
    case "SQRT": e = `sqrt(${r(u.src[0]!)})`; break;
    case "LESS_THAN": e = `select(0.0, 1.0, ${r(u.src[0]!)} < ${r(u.src[1]!)})`; break;
    case "WHERE": e = `select(${r(u.src[2]!)}, ${r(u.src[1]!)}, ${r(u.src[0]!)} != 0.0)`; break;
    case "MATH": {
      // a template may spell an operand more than once – pow's does five times – so an
      // operand that isn't a name or a number is computed once, as a `let`
      const operand = (s: UOp): string => {
        const a = r(s);
        if (/^-?[\w.]+$/.test(a)) return a;
        const name = `v${c.ctr.n++}`;
        c.scopes[scopeIdx(s, c)]!.push(`  let ${name} = ${a};`);
        c.bound.set(s.id, name);
        return name;
      };
      e = MATHS[(u.arg as { fn: string }).fn]!.wgsl(...u.src.map(operand));
      break;
    }
    case "RAND": e = `hashUniform(u32(${r(u.src[1]!)}), ${r(u.src[0]!)})`; break;
    case "ACC": {
      const k = c.ctr.n++, rg = u.src[0]!, n = (rg.arg as { extent: number }).extent, op = (u.arg as { op: string }).op;
      c.names.set(rg.id, `r${k}`);
      c.scopes.push([]); c.scopeOf.set(rg.id, c.scopes.length - 1);
      const v = render(u.src[1]!, c);
      const inner = c.scopes.pop()!;
      c.names.delete(rg.id); c.scopeOf.delete(rg.id);
      const name = `acc${k}`;
      c.scopes[scopeIdx(u, c)]!.push(`  var ${name}: f32 = ${op === "MAX" ? "-inff()" : op === "MUL" ? "1.0" : "0.0"};\n  for (var r${k}: u32 = 0u; r${k} < ${n}u; r${k} = r${k} + 1u) {\n${inner.join("\n")}\n    ${name} = ${op === "MAX" ? `maxf(${name}, ${v})` : `${name} ${op === "MUL" ? "*" : "+"} ${v}`};\n  }`);
      c.bound.set(u.id, name);
      return name;
    }
    default: throw new Error(`gpu: cannot render ${u.op}`);
  }
  if ((c.uses.get(u.id) ?? 1) > 1 && (c.size.get(u.id) ?? 1) > 12) {
    const name = `v${c.ctr.n++}`;
    c.scopes[scopeIdx(u, c)]!.push(`  let ${name} = ${e};`);
    c.bound.set(u.id, name);
    return name;
  }
  return e;
}

const binds = (n: number): string =>
  Array.from({ length: n }, (_, i) => `@group(0) @binding(${i}) var<storage, read> a${i}: array<f32>;`).join("\n");

// One thread per output element. Workgroups of 256 for anything that fills them; a grid
// beyond 65,535 workgroups on one axis folds into two (the browser drops the whole command
// buffer otherwise).
export function printFused(u: UOp, nIn: number): { wgsl: string; dispatch: number[] } {
  const n = Math.max(numel(u.shape), 1);
  const WG = n >= 256 ? 256 : 64;
  const groups = Math.ceil(n / WG), GX = Math.min(groups, 32768), ny = Math.ceil(groups / GX);
  const c = ctx(new Map([[u.src[0]!.id, "i"]]), new Map([[u.src[0]!.id, 0]]));
  planUses(u.src[1]!, c);
  const body = render(u.src[1]!, c);
  return {
    wgsl: `${binds(nIn)}
@group(0) @binding(${nIn}) var<storage, read_write> o: array<f32>;
@compute @workgroup_size(${WG})
fn main(@builtin(global_invocation_id) g: vec3<u32>) {
  let i = ${ny > 1 ? `g.y * ${GX * WG}u + g.x` : "g.x"}; if (i >= ${n}u) { return; }
${c.scopes[0]!.join("\n")}
  o[i] = ${body};
}`, dispatch: ny > 1 ? [GX, ny] : [groups],
  };
}

// ── tiling as a schedule rule: match the LOOP NEST, never a name. A tiled matmul is a
// schedule for `for i,j: out[i,j] = Σₖ f(a[…], b[…])`, visible in the IR as an ADD-ACC whose
// body is a sum of terms, each an elementwise function of two reads: a product, or ∇'s
// product guarded where the cotangent is 0. Cooperative staging requires a[…] independent
// of col and b[…] of row — PROVED by rewriting each index over fresh row, col and k
// variables: the index simplifier drops a variable the index does not read, and a read
// that still names both is declined (a wrong assumption here is a silent miscompile).
const VALUE = new Set<UOp["op"]>(["CONSTANT", "ADD", "MULTIPLY", "RECIPROCAL", "EXP", "LOG", "SQRT", "LESS_THAN", "WHERE", "MATH"]);
const extent = (r: UOp): number => (r.arg as { extent: number }).extent;
const idxKey = (v: UOp): string => `${v.op}${v.op === "RANGE" ? v.id : v.op === "ICONST" ? v.arg : ""}(${v.src.map(idxKey)})`;
interface SplitK { wgsl: string; dispatch: number[]; pShape: number[]; wgsl2: string; dispatch2: number[] }
function tileNest(u: UOp, nIn: number): { wgsl: string; dispatch: number[]; split?: SplitK } | null {
  // a CHAIN of pure ADD-ACCs (a folded weight gradient: sum over w of sum over t)
  // flattens to one K; each range is its mixed-radix digit of flat k
  const chain: UOp[] = [];
  let inner = u.src[1]!;
  while (inner.op === "ACC" && (inner.arg as { op: string }).op === "ADD") { chain.push(inner.src[0]!); inner = inner.src[1]!; }
  if (!chain.length) return null;
  // the body may be a SUM of terms (Σ-linearity merges ∇'s fork terms), each staged
  // through its own slab pair
  const terms: UOp[] = [];
  const flat = (v: UOp): void => { if (v.op === "ADD") { flat(v.src[0]!); flat(v.src[1]!); } else terms.push(v); };
  flat(inner);
  if (terms.length > 4) return null;
  // unit axes don't count: a keepdims matmul ends in [...,1], and its N is the last
  // non-unit axis
  const shT = u.shape.filter((d) => d !== 1), sh = shT.length >= 2 ? shT : u.shape;
  const N = sh.at(-1) ?? 1, M = Math.floor(Math.max(numel(u.shape), 1) / Math.max(N, 1)), K = chain.reduce((p, r) => p * extent(r), 1);
  if (M < Math.max(2, COST.tileMinM) || N < 2 || K < 2) return null;   // M = 1 keeps the streaming nest — a tile would idle 15/16 threads
  if (K < 64 && chain.length === 1) return null;                       // tiny K cannot amortize tile staging (K=32 tiles lose to the plain nest)
  // Occupancy: a tile grid smaller than the machine (weight-grad shapes like [32,96] with
  // K = 5344 give 12 workgroups) leaves the GPU idle on long serial tile loops. A K-GROUP
  // axis fixes it: workgroup z owns a K-slice, runs the same coalesced tile loop over it,
  // and writes a [G,M,N] partial that a second kernel sums. G is the smallest split that
  // fills the machine with ≥ 4 tile steps per group.
  const T = COST.tile, NT = Math.ceil(N / T), tiles = NT * Math.ceil(M / T);
  let G = 1;
  if (tiles < 48) {
    G = Math.min(Math.ceil(96 / tiles), Math.floor(Math.ceil(K / T) / 4));
    if (G < 2) return null;                                            // K too short to split — the plain nest is fine
  }
  const row = range(M), col = range(N), k = range(K), sub = new Map<number, UOp>([[u.src[0]!.id, iadd(imul(row, iconst(N)), col)]]);
  let stride = K;
  for (const r of chain) { stride /= extent(r); sub.set(r.id, imod(idiv(k, iconst(stride)), iconst(extent(r)))); }
  const subst = (v: UOp): UOp => { let o = sub.get(v.id); if (!o) { o = v.src.length ? rebuild(v.op, v.src.map(subst), v.arg, v.shape) : v; sub.set(v.id, o); } return o; };
  // per term: two reads, one of (row, k) only, the other of (col, k) only
  type Read = { at: UOp; ids: number[] };
  const pairs: { term: UOp; a: Read; b: Read }[] = [];
  for (const term of terms) {
    const reads = new Map<string, Read>();
    const walk = (v: UOp): boolean => {
      if (v.op !== "LOAD") return VALUE.has(v.op) && v.src.every(walk);
      const at = load((v.arg as { slot: number }).slot, subst(v.src[0]!)), key = idxKey(at), seen = reads.get(key);
      if (seen) seen.ids.push(v.id); else reads.set(key, { at, ids: [v.id] });
      return true;
    };
    if (!walk(term) || reads.size !== 2) return null;
    const [x, y] = [...reads.values()] as [Read, Read], of = (r: Read, not: UOp) => !rangesIn(r.at).has(not);
    if (of(x, col) && of(y, row)) pairs.push({ term, a: x, b: y });
    else if (of(y, col) && of(x, row)) pairs.push({ term, a: y, b: x });
    else return null;
  }
  const named = (...m: [UOp, string][]) => ctx(new Map(m.map(([r, s]) => [r.id, s])));
  const steps = Math.ceil(K / T), SP = Math.ceil(steps / G), MN = M * N;
  // register-blocked micro-tiles: each thread owns an RT×RT block of outputs — RT² FMAs
  // per pair of shared reads, RT² accumulators of instruction-level parallelism
  const RT = 2, L = T / RT;                                                    // L lanes per axis
  const slabDecls = pairs.map((_, j) => `var<workgroup> As${j}: array<f32, ${T * T}>;\nvar<workgroup> Bs${j}: array<f32, ${T * T}>;`).join("\n");
  const stageCells: string[] = [], accDecls: string[] = [], accUpd: string[] = [], ctr = { n: 0 };
  for (let i = 0; i < RT; i++) for (let jj = 0; jj < RT; jj++) {
    const r = `(l.y * ${RT}u + ${i}u)`, cc = `(l.x * ${RT}u + ${jj}u)`;
    pairs.forEach(({ a, b }, j) => {
      stageCells.push(`    As${j}[${r} * ${T}u + ${cc}] = select(0.0, ${render(a.at, named([row, `(rowB + ${r})`], [k, `(t * ${T}u + ${cc})`]))}, (rowB + ${r}) < ${M}u && (t * ${T}u + ${cc}) < ${K}u);`);
      stageCells.push(`    Bs${j}[${r} * ${T}u + ${cc}] = select(0.0, ${render(b.at, named([col, `(colB + ${cc})`], [k, `(t * ${T}u + ${r})`]))}, (t * ${T}u + ${r}) < ${K}u && (colB + ${cc}) < ${N}u);`);
    });
    // each output adds every term, its reads bound to the slabs
    const c = ctx(new Map());
    c.ctr = ctr;
    pairs.forEach(({ term, a, b }, j) => {
      for (const id of a.ids) c.bound.set(id, `As${j}[${r} * ${T}u + kk]`);
      for (const id of b.ids) c.bound.set(id, `Bs${j}[kk * ${T}u + ${cc}]`);
      planUses(term, c);
    });
    const e = pairs.map(({ term }) => render(term, c)).join(" + ");
    accDecls.push(`var acc${i}${jj} = 0.0;`);
    accUpd.push(...c.scopes[0]!, `      acc${i}${jj} = acc${i}${jj} + ${e};`);
  }
  // a term need not vanish on the zeros staged past K, so the last step stops at K; a grid
  // past 65,535 workgroups on one axis folds into two, as printFused's does
  const kEnd = K % T ? `min(${T}u, ${K}u - t * ${T}u)` : `${T}u`, GX = Math.min(tiles, 32768), GY = Math.ceil(tiles / GX);
  const program = (t0: string, t1: string, outIdx: (r: string, c: string, acc: string) => string) => {
    const writes: string[] = [];
    for (let i = 0; i < RT; i++) for (let jj = 0; jj < RT; jj++)
      writes.push(`  if ((rowB + l.y * ${RT}u + ${i}u) < ${M}u && (colB + l.x * ${RT}u + ${jj}u) < ${N}u) { ${outIdx(`(rowB + l.y * ${RT}u + ${i}u)`, `(colB + l.x * ${RT}u + ${jj}u)`, `acc${i}${jj}`)} }`);
    return `${binds(nIn)}
@group(0) @binding(${nIn}) var<storage, read_write> o: array<f32>;
${slabDecls}
@compute @workgroup_size(${L}, ${L})
fn main(@builtin(local_invocation_id) l: vec3<u32>, @builtin(workgroup_id) w: vec3<u32>) {
  let tile = w.y * ${GX}u + w.x;${GX * GY > tiles ? ` if (tile >= ${tiles}u) { return; }` : ""}
  let rowB = (tile / ${NT}u) * ${T}u; let colB = (tile % ${NT}u) * ${T}u;
  ${accDecls.join(" ")}
  for (var t = ${t0}; t < ${t1}; t = t + 1u) {
${stageCells.join("\n")}
    workgroupBarrier();
    for (var kk = 0u; kk < ${kEnd}; kk = kk + 1u) {
${accUpd.join("\n")}
    }
    workgroupBarrier();
  }
${writes.join("\n")}
}`;
  };
  if (G === 1) return { wgsl: program("0u", `${steps}u`, (r, c, a) => `o[${r} * ${N}u + ${c}] = ${a};`), dispatch: [GX, GY] };
  return {
    wgsl: "", dispatch: [],
    split: {
      wgsl: program(`w.z * ${SP}u`, `min(${steps}u, (w.z + 1u) * ${SP}u)`, (r, c, a) => `o[w.z * ${MN}u + ${r} * ${N}u + ${c}] = ${a};`),
      dispatch: [GX, GY, G],
      pShape: [G, M, N],
      wgsl2: `${binds(1)}
@group(0) @binding(1) var<storage, read_write> o: array<f32>;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) g: vec3<u32>) {
  let i = g.x; if (i >= ${MN}u) { return; }
  var acc = 0.0;
  for (var z = 0u; z < ${G}u; z = z + 1u) { acc = acc + a0[z * ${MN}u + i]; }
  o[i] = acc;
}`,
      dispatch2: [Math.ceil(MN / 64)],
    },
  };
}

// ── cooperative split-K GEMV: the vector-output reduction tileNest declines. A value that
// is ONE sum-reduce — `for gid in [N]: o[gid] = Σ_k body(gid,k)` — is, per output, a long
// SERIAL dependent accumulation on one thread: latency-bound, with only N threads to hide
// memory latency behind (10× off bandwidth). A whole WORKGROUP per output instead: its
// threads split the K-loop and tree-reduce in shared memory. It matches the NEST, not a
// name — matvec, rmsNorm's variance, softmax's sum all take it.
const GEMV_WG = 128;
function coopGemv(u: UOp, nIn: number): { wgsl: string; dispatch: number[] } | null {
  const gid = u.src[0]!, value = u.src[1]!;
  if (value.op !== "ACC" || (value.arg as { op: string }).op !== "ADD") return null;   // one ADD-reduce IS the whole value (no epilogue)
  // a CHAIN of pure ADD-reduces (a weight gradient's sum over w of sum over t) flattens
  // into ONE split K by mixed-radix index decomposition
  const chainR: UOp[] = [];
  let inner: UOp = value;
  while (inner.op === "ACC" && (inner.arg as { op: string }).op === "ADD") { chainR.push(inner.src[0]!); inner = inner.src[1]! }
  const extents = chainR.map((r) => (r.arg as { extent: number }).extent);
  const K = extents.reduce((a, b) => a * b, 1), N = Math.max(numel(u.shape), 1), WG = GEMV_WG;
  if (K < 4 * WG || N < 2) return null;                                        // too little K to split, or too few outputs — the plain nest is fine
  const hasAcc = (v: UOp): boolean => v.op === "ACC" || v.src.some(hasAcc);
  if (hasAcc(inner)) return null;                                              // reduces below non-ACC ops: the plain printer's job
  const names = new Map<number, string>([[gid.id, "gid"]]);
  let stride = K;
  chainR.forEach((r, i) => {
    stride /= extents[i]!;
    names.set(r.id, chainR.length === 1 ? "k" : `(((k) / ${stride}u) % ${extents[i]}u)`);
  });
  // CSE lets are load-bearing here: a flattened nested reduce reads through mixed-radix
  // index expressions that would otherwise duplicate combinatorially
  const c = ctx(names);
  planUses(inner, c);
  const expr = render(inner, c), pre = c.scopes[0]!.join("\n      ");
  const GX = Math.min(N, 32768);                                               // 2-D workgroup grid fold width
  return {
    wgsl: `${binds(nIn)}
@group(0) @binding(${nIn}) var<storage, read_write> o: array<f32>;
var<workgroup> part: array<f32, ${WG}>;
@compute @workgroup_size(${WG})
fn main(@builtin(workgroup_id) wid: vec3<u32>, @builtin(local_invocation_id) lid: vec3<u32>) {
  let gid = wid.y * ${GX}u + wid.x; var acc = 0.0;
  if (gid < ${N}u) {
    for (var k = lid.x; k < ${K}u; k = k + ${WG}u) {
      ${pre}
      acc = acc + ${expr};
    }
  }
  part[lid.x] = acc;
  workgroupBarrier();
  for (var s = ${WG >> 1}u; s > 0u; s = s >> 1u) { if (lid.x < s) { part[lid.x] = part[lid.x] + part[lid.x + s]; } workgroupBarrier(); }
  if (lid.x == 0u && gid < ${N}u) { o[gid] = part[0]; }
}`, dispatch: [GX, Math.ceil(N / GX)],
  };
}

const withHelpers = (wgsl: string): string =>
  `${wgsl.includes("hashUniform(") ? WGSL_HASH_UNIFORM + "\n" : ""}${/\b(nanf|inff|negzf|maxf)\(/.test(wgsl) ? WGSL_SPECIALS + "\n" : ""}${wgsl}`;

// codegen as a rewrite: FUSED → KERNEL (src = inputs; wgsl + dispatch in arg)
export function lowerMany(roots: UOp[]): UOp[] {
  return graphRewriteMany(optimizeMany(roots), [(u) => {
    if (u.op !== "FUSED") return null;
    const ins = kInputs(u);
    if (ins.length + 1 > MAX_BINDINGS) throw new Error(`gpu: kernel needs ${ins.length + 1} bindings — over the ${MAX_BINDINGS} storage buffers per stage (shape [${u.shape}])`);
    const kernel = (src: readonly UOp[], wgsl: string, dispatch: number[], shape: readonly number[]): UOp => {
      if (dispatch.some((d) => d > MAX_GROUPS)) throw new Error(`gpu: dispatch [${dispatch}] is over ${MAX_GROUPS} workgroups on an axis (shape [${u.shape}])`);
      return node("KERNEL", src, { wgsl: withHelpers(wgsl), dispatch }, shape);
    };
    const k = tileNest(u, ins.length) ?? coopGemv(u, ins.length) ?? printFused(u, ins.length);
    const sp = (k as { split?: SplitK }).split;
    // split-K tile: partials kernel + G-sum kernel, chained
    return sp ? kernel([kernel(ins, sp.wgsl, sp.dispatch, sp.pShape)], sp.wgsl2, sp.dispatch2, u.shape) : kernel(ins, k.wgsl, k.dispatch, u.shape);
  }]);
}


// ── the synchronous read: WebGPU has none, so a buffer is copied into the texture of a
// WebGPU canvas, drawn onto a 2-D canvas and read with getImageData (TF.js's trick, which
// jax-js uses too) — once as opaque RGB, once premultiplied for alpha. Slow, and only a
// bridge: the evaluator reads asynchronously once it runs in a worker (OwnEngine.md S5).
class SyncReader {
  static readonly W = 256;
  static readonly H = 256;
  #dev: GPUDevice;
  #device: { canvas: OffscreenCanvas; context: GPUCanvasContext; alpha: "opaque" | "premultiplied" }[] | null = null;
  #host: OffscreenCanvasRenderingContext2D | null = null;
  constructor(dev: GPUDevice) { this.#dev = dev; }
  read(buffer: GPUBuffer, bytes: number): Uint8Array {
    if (!this.#device) {
      this.#device = (["opaque", "premultiplied"] as const).map((alpha) => {
        const canvas = new OffscreenCanvas(SyncReader.W, SyncReader.H), context = canvas.getContext("webgpu") as GPUCanvasContext;
        context.configure({ device: this.#dev, format: "bgra8unorm", usage: GPUTextureUsage.COPY_DST, alphaMode: alpha });
        return { canvas, context, alpha };
      });
      this.#host = new OffscreenCanvas(SyncReader.W, SyncReader.H).getContext("2d", { willReadFrequently: true })!;
    }
    const pixels = Math.ceil(bytes / 4), out = new Uint8Array(pixels * 4), perCanvas = SyncReader.W * SyncReader.H;
    // whole canvases, then whole rows, then the last partial row: a copy reads full rows only
    const pieces: [number, number, number][] = [];                               // [first pixel, width, rows]
    let at = 0;
    for (; pixels - at >= perCanvas; at += perCanvas) pieces.push([at, SyncReader.W, SyncReader.H]);
    const rows = Math.floor((pixels - at) / SyncReader.W);
    if (rows) { pieces.push([at, SyncReader.W, rows]); at += rows * SyncReader.W; }
    if (pixels > at) pieces.push([at, pixels - at, 1]);
    for (const { canvas, context, alpha } of this.#device) {
      for (const [first, width, rows] of pieces) {
        const n = width * rows;
        const enc = this.#dev.createCommandEncoder();
        enc.copyBufferToTexture({ buffer, offset: first * 4, bytesPerRow: SyncReader.W * 4 }, { texture: context.getCurrentTexture() }, { width, height: rows });
        this.#dev.queue.submit([enc.finish()]);
        this.#host!.clearRect(0, 0, width, rows);
        this.#host!.drawImage(canvas, 0, 0);
        const px = this.#host!.getImageData(0, 0, width, rows).data;
        for (let k = 0; k < n * 4; k += 4) {
          const o = first * 4 + k;
          if (alpha === "premultiplied") out[o + 3] = px[k + 3]!;
          else { out[o] = px[k + 2]!; out[o + 1] = px[k + 1]!; out[o + 2] = px[k]!; }
        }
      }
    }
    return out.subarray(0, bytes);
  }
}

// ── the executor: plans, buffers, bind groups, dispatches in chunked submits ──────────
const USAGE = () => GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC | GPUBufferUsage.COPY_DST;
export class GPU {
  readonly name = "gpu";
  #dev: GPUDevice;
  #timestamps: boolean;                                                        // the device can time passes (profiling)
  constructor(dev: GPUDevice, timestamps: boolean) { this.#dev = dev; this.#timestamps = timestamps; }
  static async create(): Promise<GPU | null> {
    const adapter = await (globalThis.navigator as { gpu?: { requestAdapter(): Promise<GPUAdapter | null> } })?.gpu?.requestAdapter();
    if (!adapter) return null;
    const L = adapter.limits, ts = adapter.features.has("timestamp-query");
    // the defaults fail silently (a buffer over the limit reads back as zeros) — ask for the adapter's maxima
    const dev = await adapter.requestDevice({
      requiredFeatures: (ts ? ["timestamp-query"] : []) as GPUFeatureName[],
      requiredLimits: {
        maxBufferSize: L.maxBufferSize, maxStorageBufferBindingSize: L.maxStorageBufferBindingSize,
        maxStorageBuffersPerShaderStage: L.maxStorageBuffersPerShaderStage,
        maxComputeInvocationsPerWorkgroup: L.maxComputeInvocationsPerWorkgroup,
        maxComputeWorkgroupSizeX: L.maxComputeWorkgroupSizeX, maxComputeWorkgroupSizeY: L.maxComputeWorkgroupSizeY,
        maxComputeWorkgroupStorageSize: L.maxComputeWorkgroupStorageSize,
      },
    });
    return new GPU(dev, ts);
  }
  get device(): GPUDevice { return this.#dev; }
  #bytes = (u: UOp): number => Math.max(numel(u.shape), 1) * 4;

  // A plan is its kernels in waves (a wave reads only earlier waves, so it runs as one pass)
  // and its leaves. A pipeline lives while some plan runs its text.
  #pipes = new Map<string, { pipe: GPUComputePipeline; plans: number }>();
  #plan = planner((roots) => {
    const lowered = lowerMany(roots), waves: UOp[][] = [], wave = new Map<number, number>(), leaves: UOp[] = [];
    checkMemory(lowered, "KERNEL");
    for (const u of toposort(lowered)) {
      if (u.op === "BUFFER" || u.op === "CONSTANT") { leaves.push(u); continue; }
      if (u.op !== "KERNEL") throw new Error(`gpu: ${u.op} at the kernel boundary`);
      const w = Math.max(0, ...u.src.map((x) => (wave.get(x.id) ?? -1) + 1));
      wave.set(u.id, w);
      (waves[w] ??= []).push(u);
    }
    const codes = [...new Set(waves.flat().map((k) => (k.arg as { wgsl: string }).wgsl))];
    for (const code of codes) {
      const p = this.#pipes.get(code);
      if (p) p.plans++;
      else this.#pipes.set(code, { pipe: this.#dev.createComputePipeline({ layout: "auto", compute: { module: this.#dev.createShaderModule({ code }), entryPoint: "main" } }), plans: 1 });
    }
    return { lowered, waves, leaves, codes };
  }, (plan) => { for (const code of plan.codes) if (--this.#pipes.get(code)!.plans === 0) this.#pipes.delete(code); });

  // Buffers are recycled by size: a fresh buffer is zero-cleared by the browser before its
  // first use – a pass over memory every step for nothing – and queue order makes reusing
  // a buffer in a later submission safe. The pool holds one plan's worth at most: past
  // MEMORY_BUDGET it destroys the sizes released longest ago.
  #spare = new Map<number, GPUBuffer[]>();
  #pooled = 0;
  #take(bytes: number): GPUBuffer {
    const b = this.#spare.get(bytes)?.pop();
    if (b) this.#pooled -= bytes;
    return b ?? this.#dev.createBuffer({ size: bytes, usage: USAGE() });
  }
  release(b: GPUBuffer): void {
    const list = this.#spare.get(b.size) ?? [];
    this.#spare.delete(b.size);
    this.#spare.set(b.size, list);
    list.push(b);
    this.#pooled += b.size;
    for (const [size, l] of this.#spare) {
      while (this.#pooled > MEMORY_BUDGET && l.length) { l.pop()!.destroy(); this.#pooled -= size; }
      if (!l.length) this.#spare.delete(size);
      if (this.#pooled <= MEMORY_BUDGET) break;
    }
  }

  #reader: SyncReader | null = null;
  readSync(b: GPUBuffer, n: number): Float32Array {
    this.#raiseLate();
    this.#reader ??= new SyncReader(this.#dev);
    const bytes = this.#reader.read(b, Math.max(n, 1) * 4);
    return new Float32Array(bytes.buffer, bytes.byteOffset, Math.max(n, 1)).slice(0, n);
  }
  async readAsync(b: GPUBuffer, n: number): Promise<Float32Array> {
    this.#raiseLate();
    const size = Math.max(n, 1) * 4, staging = this.#dev.createBuffer({ size, usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ }), enc = this.#dev.createCommandEncoder();
    enc.copyBufferToBuffer(b, 0, staging, 0, size);
    this.#dev.queue.submit([enc.finish()]);
    await staging.mapAsync(GPUMapMode.READ);
    const out = new Float32Array(staging.getMappedRange()).slice(0, n);
    staging.unmap(); staging.destroy();
    return out;
  }
  // Profiling: while `profile` collects, each kernel runs in a pass of its own between two
  // timestamps, labelled by its shape and form – measured attribution of a step's GPU time.
  profile: { label: string; ms: number; wgsl: string }[] | null = null;
  #pending: Promise<void>[] = [];
  async profiled(): Promise<{ label: string; ms: number; wgsl: string }[]> { await Promise.all(this.#pending); this.#pending = []; const p = this.profile ?? []; this.profile = null; return p; }
  // Work in flight, one promise per computeSync until it is done. A display frame waits
  // behind whatever is queued ahead of it, so a loop that keeps submitting starves the
  // screen: it should wait for `drained(depth)` before its next step.
  #inflight = new Set<Promise<void>>();
  async drained(depth: number): Promise<void> { while (this.#inflight.size > depth) await Promise.race(this.#inflight); }
  #uploaded = new WeakMap<UOp, GPUBuffer>();
  #freeOnCollect = new FinalizationRegistry<GPUBuffer>((b) => b.destroy());
  #late: Error | null = null;
  #raiseLate(): void { const e = this.#late; if (e) { this.#late = null; throw e; } }

  // ── the live path: synchronous, results stay on the device ────────────────────────
  // computeSync runs the graph and returns one fresh buffer per root, which the caller owns
  // (free it with release). A BUFFER leaf binds the buffer `resolve` names; a CONSTANT is
  // uploaded once and kept as long as its node lives. Validation errors arrive late and are
  // raised by the next call.
  computeSync(roots: UOp[], resolve: (name: string) => GPUBuffer | undefined): GPUBuffer[] {
    this.#raiseLate();
    const { plan, bind } = this.#plan(roots), dev = this.#dev;
    dev.pushErrorScope("validation");
    const store = new Map<number, GPUBuffer>(), transient: GPUBuffer[] = [];
    const bufOf = (u0: UOp): GPUBuffer => {
      const u = bind.get(u0.id) ?? u0;
      if (u.op === "BUFFER") return resolve(bufName(u)) ?? ((): never => { throw new Error(`gpu: unbound leaf '${bufName(u)}'`); })();
      if (u.op === "CONSTANT") {
        let b = this.#uploaded.get(u);
        if (!b) {
          b = dev.createBuffer({ size: this.#bytes(u), usage: USAGE() });
          dev.queue.writeBuffer(b, 0, u.arg as unknown as GPUAllowSharedBufferSource);
          this.#uploaded.set(u, b); this.#freeOnCollect.register(u, b);
        }
        return b;
      }
      let b = store.get(u.id);
      if (!b) { b = this.#take(this.#bytes(u)); store.set(u.id, b); transient.push(b); }
      return b;
    };
    for (const r of plan.lowered) if (r.op === "KERNEL" && !store.has(r.id)) store.set(r.id, this.#take(this.#bytes(r)));   // the roots' buffers outlive the call
    for (const u of plan.leaves) bufOf(u);
    const kernels = plan.waves.flat();
    let enc = dev.createCommandEncoder(), dispatched = 0;
    const flush = () => { dev.queue.submit([enc.finish()]); enc = dev.createCommandEncoder(); dispatched = 0; };
    const timed = this.profile && this.#timestamps && kernels.length ? { qs: dev.createQuerySet({ type: "timestamp", count: Math.min(4096, 2 * kernels.length) }), labels: [] as string[], wgsl: [] as string[] } : null;
    for (const ks of plan.waves) {
      const tw = (j: number) => (timed ? { timestampWrites: { querySet: timed.qs, beginningOfPassWriteIndex: 2 * (timed.labels.length + j), endOfPassWriteIndex: 2 * (timed.labels.length + j) + 1 } } : undefined);
      let p = enc.beginComputePass(tw(0));
      ks.forEach((u, j) => {
        if (timed && j) { p.end(); p = enc.beginComputePass(tw(j)); }
        const a = u.arg as { wgsl: string; dispatch: number[] }, pipe = this.#pipes.get(a.wgsl)!.pipe;
        const entries: GPUBindGroupEntry[] = [...u.src, u].map((x, i) => ({ binding: i, resource: { buffer: bufOf(x) } }));
        p.setPipeline(pipe); p.setBindGroup(0, dev.createBindGroup({ layout: pipe.getBindGroupLayout(0), entries }));
        p.dispatchWorkgroups(a.dispatch[0]!, a.dispatch[1] ?? 1, a.dispatch[2] ?? 1); dispatched++;
      });
      p.end();
      if (timed) for (const u of ks) { const w = (u.arg as { wgsl: string }).wgsl; timed.wgsl.push(w); timed.labels.push(`[${u.shape}] ${w.includes("var<workgroup> As") ? "tile" : w.includes("var<workgroup> part") ? "gemv" : "map"}/${u.src.length}in`); }
      if (dispatched >= SUBMIT_CHUNK) flush();
    }
    // every root returns a buffer of its own: a root that is a leaf (the graph simplified to
    // its input), or a second root equal to the first, is copied into one
    const given = new Set<GPUBuffer>();
    const result = plan.lowered.map((r) => {
      const b = r.op === "KERNEL" ? store.get(r.id)! : null;
      if (b && !given.has(b)) { given.add(b); return b; }
      const own = this.#take(this.#bytes(r));
      enc.copyBufferToBuffer(bufOf(r), 0, own, 0, this.#bytes(r));
      return own;
    });
    if (timed) {
      const n = timed.labels.length, resolve = dev.createBuffer({ size: 16 * n, usage: GPUBufferUsage.QUERY_RESOLVE | GPUBufferUsage.COPY_SRC });
      const read = dev.createBuffer({ size: 16 * n, usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ });
      enc.resolveQuerySet(timed.qs, 0, 2 * n, resolve, 0); enc.copyBufferToBuffer(resolve, 0, read, 0, 16 * n);
      const profile = this.profile!;
      this.#pending.push((async () => {
        await dev.queue.onSubmittedWorkDone(); await read.mapAsync(GPUMapMode.READ);
        const t = new BigUint64Array(read.getMappedRange().slice(0)); read.unmap();
        timed.labels.forEach((label, j) => profile.push({ label, ms: Number(t[2 * j + 1]! - t[2 * j]!) / 1e6, wgsl: timed.wgsl[j]! }));
        read.destroy(); resolve.destroy(); timed.qs.destroy();
      })());
    }
    flush();
    const done: Promise<void> = dev.queue.onSubmittedWorkDone().then(() => { this.#inflight.delete(done); });
    this.#inflight.add(done);
    // a pop rejects only when its device is gone (a reload, a GPU reset): nothing is left to validate
    dev.popErrorScope().then((e) => { if (e) this.#late = new Error(`gpu validation: ${e.message}`); }, () => {});
    for (const b of transient) this.release(b);
    return result;
  }
}
