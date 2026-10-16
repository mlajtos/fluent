// One immutable UOp graph, from tensors down to loop nests. 18 numeric primitives
// generate everything; derived ops are library functions, so the laws fire on their
// compositions. A FUSED node is a loop nest of the SAME node type one level down
// (`for gid: out[gid] = value`), and every body hangs off `src` — one toposort sees the
// whole program, so every pass reaches inside kernels. Every value is f32.

export type Op =
  | "CONSTANT" | "BUFFER"                                    // leaves — CONSTANT: Float32Array; BUFFER arg: {name}
  | "ADD" | "MULTIPLY" | "RECIPROCAL" | "EXP" | "LOG" | "SQRT"
  | "LESS_THAN" | "WHERE" | "DETACH"                         // mask (∂=0) · route · stop-gradient
  | "MATH"                                                   // arg {fn}: one row of MATHS — sin, floor, pow, …
  | "RANDOM"                                                 // src [seed]: uniform [0,1), a pure function of (seed, index)
  | "IOTA"                                                   // leaf [n]: element i is i — an index ramp no buffer holds
  | "REDUCE"                                                 // arg {axis, keepdims, op: "ADD"|"MAX"|"MUL"}
  | "BROADCAST" | "PERMUTE" | "RESHAPE" | "SLICE" | "WINDOW" // the view algebra — pure stride maps, never compute
  | "PAD"                                                    // arg {axis, before, after}: zeros around — SLICE's transpose, an index predicate
  | "GATHER" | "SCATTER_ADD"                                 // runtime-indexed rows; each other's transpose
  | "FUSED"                                                  // src = [gid, value, ...inputs] — ONE kernel
  | "KERNEL"                                                 // arg {wgsl, dispatch} — printed FUSED; src = inputs
  | "RANGE" | "ICONST" | "IADD" | "IMUL" | "IDIV" | "IMOD"   // index algebra; a RANGE's identity IS the node
  | "LOAD" | "ACC" | "IIDX" | "IVAL"                         // read input[slot]; fold body over a range; idx↔val casts
  | "RAND";                                                  // uniform [0,1) from (index, seed)

export interface UOp { readonly op: Op; readonly src: readonly UOp[]; readonly arg: unknown; readonly shape: readonly number[]; readonly id: number }
let NEXT = 0;
export const node = (op: Op, src: readonly UOp[], arg: unknown, shape: readonly number[]): UOp => ({ op, src, arg, shape, id: NEXT++ });

export const numel = (s: readonly number[]): number => s.reduce((a, b) => a * b, 1);
export const sameShape = (a: readonly number[], b: readonly number[]): boolean => a.length === b.length && a.every((x, i) => x === b[i]);
export const strides = (s: readonly number[]): number[] => { const st = s.map(() => 1); for (let d = s.length - 2; d >= 0; d--) st[d] = st[d + 1]! * s[d + 1]!; return st; };
const sh = (s: readonly number[]) => `[${s.join(" ")}]`;
const err = (m: string): never => { throw new Error(m); };

// ---- leaves ----------------------------------------------------------------
export const konst = (v: number): UOp => node("CONSTANT", [], new Float32Array([v]), []);
export const data = (v: Float32Array, shape?: readonly number[]): UOp => node("CONSTANT", [], v, shape ?? [v.length]);
export const buffer = (name: string, shape: readonly number[]): UOp => node("BUFFER", [], { name }, shape);
export const bufName = (u: UOp): string => (u.arg as { name: string }).name;

// ---- the view algebra (zero-copy stride maps) --------------------------------
export const reshape = (a: UOp, s: readonly number[]): UOp =>
  numel(s) !== numel(a.shape) ? err(`reshape: ${sh(a.shape)} → ${sh(s)} changes numel`) : node("RESHAPE", [a], s.slice(), s.slice());
export const permute = (a: UOp, p: readonly number[]): UOp =>
  p.length !== a.shape.length || [...p].sort((x, y) => x - y).some((v, i) => v !== i) ? err(`permute: ${JSON.stringify(p)} not a permutation for ${sh(a.shape)}`)
    : node("PERMUTE", [a], p.slice(), p.map((i) => a.shape[i]!));
export const transpose = (a: UOp): UOp => permute(a, a.shape.map((_, i) => a.shape.length - 1 - i));
export const broadcastTo = (a: UOp, s: readonly number[]): UOp => {
  const off = s.length - a.shape.length;
  if (off < 0) err(`broadcast: cannot lower rank ${sh(a.shape)} → ${sh(s)}`);
  a.shape.forEach((d, i) => { if (d !== s[i + off] && d !== 1) err(`broadcast: axis ${i} size ${d} ↛ ${s[i + off]} (${sh(a.shape)} → ${sh(s)})`); });
  return node("BROADCAST", [a], s.slice(), s.slice());
};
export const slice = (a: UOp, axis: number, start: number, len: number): UOp => {
  const n = a.shape[axis] ?? err(`slice: axis ${axis} out of range for ${sh(a.shape)}`);
  if (start < 0 || len < 0 || start + len > n) err(`slice: [${start},${start + len}) out of axis ${axis} of ${sh(a.shape)}`);
  return node("SLICE", [a], { axis, start, len }, a.shape.map((d, i) => (i === axis ? len : d)));
};
// sliding windows over the last ks.length axes: out[…, o…, k…] = a[…, o + k…] — the
// neighbourhood of every position, as overlapping strides (tinygrad's pool view)
export const windows = (a: UOp, ks: readonly number[]): UOp => {
  const r = a.shape.length, n = ks.length, sp = a.shape.slice(r - n);
  if (n > r || ks.some((k, i) => k < 1 || k > sp[i]!)) err(`windows: [${ks}] does not fit ${sh(a.shape)}`);
  return node("WINDOW", [a], ks.slice(), [...a.shape.slice(0, r - n), ...sp.map((d, i) => d - ks[i]! + 1), ...ks]);
};
// zeros around: out[…, i, …] = a[…, i − before, …] inside, 0 outside. Not a stride map —
// an index predicate the fusion folds into each read, so padding never materializes.
export const pad = (a: UOp, axis: number, before: number, after: number): UOp => {
  if (axis < 0 || axis >= a.shape.length) err(`pad: axis ${axis} out of range for ${sh(a.shape)}`);
  if (before < 0 || after < 0) err(`pad: negative padding ${before}, ${after}`);
  return node("PAD", [a], { axis, before, after }, a.shape.map((d, i) => (i === axis ? d + before + after : d)));
};

// A view map is the AFFINE map off = offset + Σ coord_d·strides[d] — a view names elements,
// it never computes. Chains collapse to one map; an unfoldable reshape becomes the base.
export interface ViewMap { base: UOp; offset: number; strides: number[] }
export const viewMap = (v: UOp): ViewMap => {
  if (v.op === "PERMUTE") { const m = viewMap(v.src[0]!); return { base: m.base, offset: m.offset, strides: (v.arg as number[]).map((p) => m.strides[p]!) }; }
  if (v.op === "BROADCAST") {
    const m = viewMap(v.src[0]!), s = v.src[0]!.shape, off = v.shape.length - s.length;
    return { base: m.base, offset: m.offset, strides: v.shape.map((_, d) => { const i = d - off; return i < 0 || s[i] === 1 ? 0 : m.strides[i]!; }) };
  }
  if (v.op === "WINDOW") {
    const m = viewMap(v.src[0]!), n = (v.arg as number[]).length, r = v.src[0]!.shape.length;
    return { base: m.base, offset: m.offset, strides: [...m.strides, ...m.strides.slice(r - n)] };
  }
  if (v.op === "SLICE") { const { axis, start } = v.arg as { axis: number; start: number }; const m = viewMap(v.src[0]!); return { base: m.base, offset: m.offset + start * m.strides[axis]!, strides: m.strides }; }
  if (v.op === "RESHAPE") {
    const m = viewMap(v.src[0]!), ss = v.src[0]!.shape;
    if (sameShape(m.strides, strides(ss))) return { base: m.base, offset: m.offset, strides: strides(v.shape) };
    const nu = (s: readonly number[]) => s.filter((d) => d !== 1);            // inserting/dropping unit axes is a stride map whatever the source
    if (sameShape(nu(ss), nu(v.shape))) { const st = ss.map((d, i) => [d, m.strides[i]!] as const).filter(([d]) => d !== 1); let i = 0; return { base: m.base, offset: m.offset, strides: v.shape.map((d) => (d === 1 ? 0 : st[i++]![1])) }; }
  }
  return { base: v, offset: 0, strides: strides(v.shape) };
};

// ---- elementwise (right-aligned broadcasting at construction) ----------------
const joinShape = (a: readonly number[], b: readonly number[]): number[] => {
  const n = Math.max(a.length, b.length), r: number[] = [];
  for (let i = 0; i < n; i++) { const x = a[a.length - n + i] ?? 1, y = b[b.length - n + i] ?? 1; if (x !== y && x !== 1 && y !== 1) err(`elementwise: ${sh(a)} vs ${sh(b)}`); r.push(x === 1 ? y : x); }   // 1 stretches, even to 0
  return r;
};
const bc2 = (x: UOp, y: UOp): [UOp, UOp] => { const s = joinShape(x.shape, y.shape); const f = (u: UOp) => sameShape(u.shape, s) ? u : broadcastTo(u, s); return [f(x), f(y)]; };
const elem = (op: Op) => (x: UOp, y: UOp): UOp => { const [a, b] = bc2(x, y); return node(op, [a, b], null, a.shape); };
export const add = elem("ADD");
export const mul = elem("MULTIPLY");
export const cmplt = elem("LESS_THAN");                                       // a < b → 1.0 else 0.0; ∂ = 0
const un = (op: Op) => (a: UOp): UOp => node(op, [a], null, a.shape);
export const recip = un("RECIPROCAL"); export const exp = un("EXP"); export const log = un("LOG"); export const sqrt = un("SQRT"); export const detach = un("DETACH");

// ---- the math table: one row per function, its JS and its WGSL ------------------
// (derivatives live in grad.ts's JVP table, keyed by the same name)
export const MATHS: Record<string, { arity: 1 | 2; js: (...a: number[]) => number; wgsl: (...a: string[]) => string }> = {
  sin: { arity: 1, js: Math.sin, wgsl: (x) => `sin(${x})` },
  cos: { arity: 1, js: Math.cos, wgsl: (x) => `cos(${x})` },
  tanh: { arity: 1, js: Math.tanh, wgsl: (x) => `tanh(${x})` },
  asin: { arity: 1, js: Math.asin, wgsl: (x) => `asin(${x})` },
  acos: { arity: 1, js: Math.acos, wgsl: (x) => `acos(${x})` },
  atan: { arity: 1, js: Math.atan, wgsl: (x) => `atan(${x})` },
  asinh: { arity: 1, js: Math.asinh, wgsl: (x) => `asinh(${x})` },
  acosh: { arity: 1, js: Math.acosh, wgsl: (x) => `acosh(${x})` },
  atanh: { arity: 1, js: Math.atanh, wgsl: (x) => `atanh(${x})` },
  floor: { arity: 1, js: Math.floor, wgsl: (x) => `floor(${x})` },
  ceil: { arity: 1, js: Math.ceil, wgsl: (x) => `ceil(${x})` },
  trunc: { arity: 1, js: Math.trunc, wgsl: (x) => `trunc(${x})` },
  round: { arity: 1, js: (x) => { const r = Math.round(x); return r - x === 0.5 && r % 2 !== 0 ? r - 1 : r; }, wgsl: (x) => `round(${x})` },   // half to even
  sign: { arity: 1, js: (x) => (x > 0 ? 1 : x < 0 ? -1 : x), wgsl: (x) => `sign(${x})` },
  abs: { arity: 1, js: Math.abs, wgsl: (x) => `abs(${x})` },
  isnan: { arity: 1, js: (x) => (x !== x ? 1 : 0), wgsl: (x) => `select(0.0, 1.0, (bitcast<u32>(${x}) & 0x7fffffffu) > 0x7f800000u)` },   // WGSL may fold x != x away
  expm1: { arity: 1, js: Math.expm1, wgsl: (x) => `select(exp(${x}) - 1.0, ${x} * (1.0 + ${x} * (0.5 + ${x} / 6.0)), abs(${x}) < 1e-3)` },
  log1p: { arity: 1, js: Math.log1p, wgsl: (x) => `select(log(1.0 + ${x}), ${x} * (1.0 - ${x} * (0.5 - ${x} / 3.0)), abs(${x}) < 1e-3)` },
  cbrt: { arity: 1, js: Math.cbrt, wgsl: (x) => `(sign(${x}) * pow(abs(${x}), 1.0 / 3.0))` },
  atan2: { arity: 2, js: Math.atan2, wgsl: (y, x) => `atan2(${y}, ${x})` },
  // a true quotient: x × (1 ÷ y) rounds twice, and 1 ÷ 3 × 3 is not 1
  div: { arity: 2, js: (a, b) => a / b, wgsl: (a, b) => `(${a} / ${b})` },
  // equal, and never for NaN – spelled out, as WGSL may assume a float is never NaN
  eq: { arity: 2, js: (a, b) => (a === b ? 1 : 0), wgsl: (a, b) => `select(0.0, 1.0, ${a} == ${b} && (bitcast<u32>(${a}) & 0x7fffffffu) <= 0x7f800000u)` },
  // WGSL pow is exp2(y·log2 x): undefined for x < 0 and for 0^0 — spell out what Math.pow does
  pow: { arity: 2, js: Math.pow, wgsl: (a, b) => `select(select(select(pow(abs(${a}), ${b}), -pow(abs(${a}), ${b}), ${a} < 0.0 && fract(${b} * 0.5) != 0.0), nanf(), ${a} < 0.0 && ${b} != trunc(${b})), 1.0, ${b} == 0.0)` },
};
export const math = (fn: string, ...xs: UOp[]): UOp => {
  const m = MATHS[fn] ?? err(`math: unknown function ${fn}`);
  if (xs.length !== m.arity) err(`math: ${fn} takes ${m.arity} operand${m.arity > 1 ? "s" : ""}`);
  if (xs.length === 1) return node("MATH", xs, { fn }, xs[0]!.shape);
  const [a, b] = bc2(xs[0]!, xs[1]!);
  return node("MATH", [a, b], { fn }, a.shape);
};

// A random tensor's element i is hash(seed, i) — no state, no buffer of its own, so it
// fuses into whatever reads it and the CPU and GPU draw the same bits. The seed is data (a
// one-element input, an exact integer below 2²⁴), never kernel text: a fresh draw reuses
// the compiled plan.
export const random = (shape: readonly number[], seed: number): UOp => node("RANDOM", [data(Float32Array.of(seed & 0xffffff), [1])], null, shape.slice());
// the hash both backends print: a 32-bit integer mix, its top 24 bits as [0, 1)
export const hashUniform = (seed: number, i: number): number => {
  let h = (Math.imul(i >>> 0, 0x9e3779b1) ^ Math.imul(seed >>> 0, 0x85ebca77)) >>> 0;
  h = Math.imul(h ^ (h >>> 16), 0x7feb352d) >>> 0;
  h = Math.imul(h ^ (h >>> 15), 0x846ca68b) >>> 0;
  h = (h ^ (h >>> 16)) >>> 0;
  return (h >>> 8) / 16777216;
};
export const WGSL_HASH_UNIFORM = `fn hashUniform(seed: u32, i: u32) -> f32 {
  var h = (i * 0x9e3779b1u) ^ (seed * 0x85ebca77u);
  h = (h ^ (h >> 16u)) * 0x7feb352du;
  h = (h ^ (h >> 15u)) * 0x846ca68bu;
  h = h ^ (h >> 16u);
  return f32(h >> 8u) * (1.0 / 16777216.0);
}`;
// element i is i: computed where it is read, like a random draw — ranges, one-hot rows and
// index tables never upload an array
export const iota = (n: number): UOp => node("IOTA", [], null, [n]);
export const where = (c: UOp, a: UOp, b: UOp): UOp => {
  const s = joinShape(joinShape(c.shape, a.shape), b.shape), f = (u: UOp) => sameShape(u.shape, s) ? u : broadcastTo(u, s);
  return node("WHERE", [f(c), f(a), f(b)], null, s);
};

// ---- reduce ------------------------------------------------------------------
export const reduceAxis = (a: UOp, axis: number, keepdims = true, op: "ADD" | "MAX" | "MUL" = "ADD"): UOp => {
  if (axis < 0 || axis >= a.shape.length) err(`reduce: axis ${axis} out of ${sh(a.shape)}`);
  const s = a.shape.slice(); if (keepdims) s[axis] = 1; else s.splice(axis, 1);
  return node("REDUCE", [a], { axis, keepdims, op }, s);
};
export const reduceMax = (a: UOp, axis: number, keepdims = true): UOp => reduceAxis(a, axis, keepdims, "MAX");
export const sum = (a: UOp): UOp => { let r = a; while (r.shape.length) r = reduceAxis(r, 0, false); return r; };

// ---- runtime indexing (a transpose pair, like broadcast⟷reduce) ---------------
export const gather = (table: UOp, idx: UOp): UOp =>
  idx.shape.length !== 1 ? err(`gather: idx must be rank-1, got ${sh(idx.shape)}`) : node("GATHER", [table, idx], null, [idx.shape[0]!, ...table.shape.slice(1)]);
export const scatterAdd = (idx: UOp, values: UOp, size: number): UOp =>
  idx.shape.length !== 1 || values.shape[0] !== idx.shape[0] ? err(`scatterAdd: idx ${sh(idx.shape)} vs values ${sh(values.shape)}`)
    : node("SCATTER_ADD", [idx, values], { size }, [size, ...values.shape.slice(1)]);

// ---- the kernel language: a loop nest, as UOps --------------------------------
// Index builders simplify as they build, so an index expression stays the closed form a
// human would write; the printer (gpu.ts) and the walker (cpu.ts) both just read it.
// Every range knows its extent, so every index knows its bounds, and the division laws
// apply where they hold: x / c is 0 and x % c is x when 0 ≤ x < c; (c·q + r) / c is
// q + r / c and (c·q + r) % c is r % c for r ≥ 0; (x / a) / b is x / (a·b); x % (c·k) / c
// is (x / c) % k; and (x / c)·c + x % c is x. A view composed of views no longer
// re-divides the index at every level — and integer division is costly on a GPU.
export const range = (extent: number): UOp => node("RANGE", [], { extent }, []);
export const iconst = (v: number): UOp => node("ICONST", [], v, []);
const iv = (u: UOp): number | null => (u.op === "ICONST" ? (u.arg as number) : null);
const boundsMemo = new WeakMap<UOp, readonly [number, number]>();
export const bounds = (u: UOp): readonly [number, number] => {
  const hit = boundsMemo.get(u); if (hit) return hit;
  let b: [number, number];
  const [x, y] = [u.src[0], u.src[1]];
  switch (u.op) {
    case "ICONST": b = [u.arg as number, u.arg as number]; break;
    case "RANGE": b = [0, (u.arg as { extent: number }).extent - 1]; break;
    case "IIDX": b = [0, Infinity]; break;
    case "IADD": { const p = bounds(x!), q = bounds(y!); b = [p[0] + q[0], p[1] + q[1]]; break; }
    case "IMUL": { const p = bounds(x!), q = bounds(y!), c = [p[0] * q[0], p[0] * q[1], p[1] * q[0], p[1] * q[1]].map((v) => (Number.isNaN(v) ? 0 : v)); b = [Math.min(...c), Math.max(...c)]; break; }
    case "IDIV": { const p = bounds(x!), c = iv(y!); b = c && c > 0 ? [Math.floor(p[0] / c), Math.floor(p[1] / c)] : [-Infinity, Infinity]; break; }
    case "IMOD": { const p = bounds(x!), c = iv(y!); b = c && c > 0 && p[0] >= 0 ? (p[1] < c ? [p[0], p[1]] : [0, c - 1]) : [-Infinity, Infinity]; break; }
    default: b = [-Infinity, Infinity];
  }
  boundsMemo.set(u, b);
  return b;
};
// an index as Σ coefficient·atom + constant
type Linear = { terms: [number, UOp][]; k: number };
const linear = (u: UOp): Linear => {
  const c = iv(u);
  if (c !== null) return { terms: [], k: c };
  if (u.op === "IADD") { const p = linear(u.src[0]!), q = linear(u.src[1]!); return { terms: [...p.terms, ...q.terms], k: p.k + q.k }; }
  if (u.op === "IMUL") {
    const f = iv(u.src[1]!) ?? iv(u.src[0]!);
    if (f !== null) { const l = linear(iv(u.src[1]!) !== null ? u.src[0]! : u.src[1]!); return { terms: l.terms.map(([t, a]) => [t * f, a]), k: l.k * f }; }
  }
  return { terms: [[1, u]], k: 0 };
};
const fromLinear = ({ terms, k }: Linear): UOp => {
  let out: UOp = iconst(k);
  for (const [c, a] of terms) out = addRaw(out, c === 1 ? a : mulRaw(a, iconst(c)));
  return out;
};
const nonneg = (l: Linear): boolean => l.k >= 0 && l.terms.every(([c, a]) => c >= 0 && bounds(a)[0] >= 0);
const addRaw = (a: UOp, b: UOp): UOp => { const x = iv(a), y = iv(b); return x !== null && y !== null ? iconst(x + y) : x === 0 ? b : y === 0 ? a : node("IADD", [a, b], null, []); };
const mulRaw = (a: UOp, b: UOp): UOp => { const x = iv(a), y = iv(b); return x !== null && y !== null ? iconst(x * y) : x === 0 || y === 0 ? iconst(0) : x === 1 ? b : y === 1 ? a : node("IMUL", [a, b], null, []); };
export const iadd = (a: UOp, b: UOp): UOp => {
  const sum = addRaw(a, b);
  if (sum.op !== "IADD") return sum;
  // (x / c)·t·c + (x % c)·t is x·t: a decomposed coordinate put back together
  const l = linear(sum), used = new Set<number>();
  let fired = false;
  for (let i = 0; i < l.terms.length; i++) {
    const [ci, ai] = l.terms[i]!;
    if (ai.op !== "IMOD" || used.has(i)) continue;
    const c = iv(ai.src[1]!);
    if (c === null) continue;
    const j = l.terms.findIndex(([cj, aj], jj) => !used.has(jj) && aj.op === "IDIV" && aj.src[0] === ai.src[0] && iv(aj.src[1]!) === c && cj === ci * c);
    if (j < 0) continue;
    used.add(i); used.add(j); l.terms.push([ci, ai.src[0]!]); fired = true;
  }
  return fired ? fromLinear({ terms: l.terms.filter((_, i) => !used.has(i)), k: l.k }) : sum;
};
export const imul = mulRaw;
const gcd = (a: number, b: number): number => (b ? gcd(b, a % b) : a);
// the index as g·x + r with g a factor of c and 0 ≤ r < g
const shared = (l: Linear, c: number): { g: number; x: UOp; r: UOp } | null => {
  for (const [t] of l.terms) {
    const g = gcd(c, t);
    if (g <= 1 || g === c) continue;
    const r = fromLinear({ terms: l.terms.filter(([u]) => u % g !== 0), k: l.k % g });
    const [lo, hi] = bounds(r);
    if (lo >= 0 && hi < g) return { g, x: fromLinear({ terms: l.terms.filter(([u]) => u % g === 0).map(([u, v]) => [u / g, v]), k: Math.floor(l.k / g) }), r };
  }
  return null;
};
export const idiv = (a: UOp, b: UOp): UOp => {
  const x = iv(a), c = iv(b);
  if (x !== null && c !== null) return iconst(Math.floor(x / c));
  if (c === 1) return a;
  if (c === null || c <= 0) return node("IDIV", [a, b], null, []);
  const [lo, hi] = bounds(a);
  if (lo >= 0 && hi < c) return iconst(0);
  if (a.op === "IDIV" && iv(a.src[1]!) !== null) return idiv(a.src[0]!, iconst(iv(a.src[1]!)! * c));
  if (a.op === "IMOD" && lo >= 0 && bounds(a.src[0]!)[0] >= 0) { const m = iv(a.src[1]!); if (m !== null && m % c === 0) return imod(idiv(a.src[0]!, b), iconst(m / c)); }
  const l = linear(a);
  if (nonneg(l)) {
    const whole = l.terms.filter(([t]) => t % c === 0), rest = l.terms.filter(([t]) => t % c !== 0);
    if (whole.length || l.k >= c) {
      const q = fromLinear({ terms: whole.map(([t, u]) => [t / c, u]), k: Math.floor(l.k / c) });
      return iadd(q, idiv(fromLinear({ terms: rest, k: l.k % c }), b));
    }
    // (g·x + r) / (g·m) is x / m: a factor the divisor shares with the index divides out
    const s = shared(l, c);
    if (s) return idiv(s.x, iconst(c / s.g));
  }
  return node("IDIV", [a, b], null, []);
};
export const imod = (a: UOp, b: UOp): UOp => {
  const x = iv(a), c = iv(b);
  if (x !== null && c !== null) return iconst(x % c);
  if (c === 1) return iconst(0);
  if (c === null || c <= 0) return node("IMOD", [a, b], null, []);
  const [lo, hi] = bounds(a);
  if (lo >= 0 && hi < c) return a;
  if (a.op === "IMOD" && lo >= 0) { const m = iv(a.src[1]!); if (m !== null && m % c === 0) return imod(a.src[0]!, b); }
  const l = linear(a);
  if (nonneg(l) && (l.k >= c || l.terms.some(([t]) => t >= c))) {           // a coefficient or constant c or more: reduce it mod c
    {
      const r = fromLinear({ terms: l.terms.filter(([t]) => t % c !== 0).map(([t, u]) => [t % c, u]), k: l.k % c });
      const [rl, rh] = bounds(r);
      return rl >= 0 && rh < c ? r : imod(r, b);
    }
  }
  // (g·x + r) % (g·m) is g·(x % m) + r
  const s = nonneg(l) ? shared(l, c) : null;
  if (s) return iadd(imul(imod(s.x, iconst(c / s.g)), iconst(s.g)), s.r);
  return node("IMOD", [a, b], null, []);
};
// rebuild a kernel node from new sources, simplifying index arithmetic as it goes
export const rebuild = (op: Op, src: UOp[], arg: unknown, shape: readonly number[]): UOp =>
  op === "IADD" ? iadd(src[0]!, src[1]!) : op === "IMUL" ? imul(src[0]!, src[1]!) : op === "IDIV" ? idiv(src[0]!, src[1]!) : op === "IMOD" ? imod(src[0]!, src[1]!) : node(op, src, arg, shape);
export const load = (slot: number, idx: UOp): UOp => node("LOAD", [idx], { slot }, []);
export const acc = (op: "ADD" | "MAX" | "MUL", r: UOp, body: UOp): UOp => node("ACC", [r, body], { op }, []);
export const iidx = (v: UOp): UOp => node("IIDX", [v], null, []);              // value → index (u32 cast)
export const ival = (i: UOp): UOp => node("IVAL", [i], null, []);              // index → value (f32 cast)

// FUSED src = [gid, value, ...inputs]; LOAD's slot is positional into `inputs`.
export const fused = (gid: UOp, value: UOp, inputs: UOp[], shape: readonly number[]): UOp => node("FUSED", [gid, value, ...inputs], null, shape.slice());
export const kInputs = (f: UOp): readonly UOp[] => f.src.slice(2);
// the one flat-index → strided-read expression: off = Σ ((gid/outSt[d]) % outSh[d]) · st[d]
export const bcIndex = (st: readonly number[], outSh: readonly number[], gid: UOp): UOp => {
  const os = strides(outSh);
  if (os.every((s, d) => st[d] === s || outSh[d] === 1)) return gid;           // contiguous read: the index IS gid
  let off: UOp = iconst(0);
  for (let d = 0; d < outSh.length; d++) { if (st[d] === 0 || outSh[d] === 1) continue; off = iadd(off, imul(imod(idiv(gid, iconst(os[d]!)), iconst(outSh[d]!)), iconst(st[d]!))); }
  return off;
};

export function toposort(root: UOp): UOp[] {                                   // iterative: real graphs are thousands deep
  const out: UOp[] = [], seen = new Set<number>(), stack: [UOp, number][] = [[root, 0]];
  while (stack.length) {
    const top = stack[stack.length - 1]!;
    if (top[1] === 0) {
      top[1] = 1;
      if (seen.has(top[0].id)) { stack.pop(); continue; }
      seen.add(top[0].id);
      for (let i = top[0].src.length - 1; i >= 0; i--) if (!seen.has(top[0].src[i]!.id)) stack.push([top[0].src[i]!, 0]);
    } else { out.push(top[0]); stack.pop(); }
  }
  return out;
}
