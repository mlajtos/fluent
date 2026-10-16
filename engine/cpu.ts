// The CPU backend and reference oracle. After optimize a graph is leaves + FUSED, so
// the CPU prints each kernel as a JavaScript function — the same loop nest the WGSL printer
// prints — with every value op fround'd so it matches the GPU's f32 arithmetic.

import type { UOp } from "./ir.ts";
import { numel, kInputs, bufName, toposort, MATHS, hashUniform, bounds } from "./ir.ts";
import { optimizeMany, canonical, placeholders, checkMemory } from "./opt.ts";

export interface HostData { data: Float32Array; shape: readonly number[] }

const jsNumber = (v: number): string => (Number.isNaN(v) ? "NaN" : v === Infinity ? "Infinity" : v === -Infinity ? "-Infinity" : Object.is(v, -0) ? "-0" : String(Math.fround(v)));

// A shared subexpression becomes a `const` at the outermost scope its loop variables
// allow; everything else inlines.
interface Ctx { names: Map<number, string>; ctr: { n: number }; uses: Map<number, number>; deps: Map<number, Set<number>>; scopeOf: Map<number, number>; scopes: string[][]; bound: Map<number, string> }
const planUses = (value: UOp, c: Ctx): void => {
  const seen = new Set<number>();
  const walk = (v: UOp): void => {
    c.uses.set(v.id, (c.uses.get(v.id) ?? 0) + 1);
    if (seen.has(v.id)) return;
    seen.add(v.id);
    const d = new Set<number>();
    if (v.op === "RANGE") d.add(v.id);
    for (const s of v.src) { walk(s); for (const x of c.deps.get(s.id)!) d.add(x); }
    if (v.op === "ACC") d.delete(v.src[0]!.id);
    c.deps.set(v.id, d);
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
    case "RANGE": return c.names.get(u.id) ?? ((): never => { throw new Error("cpu: unbound RANGE"); })();
    case "ICONST": return String(u.arg as number);
    case "CONSTANT": return jsNumber((u.arg as Float32Array)[0]!);
    case "IADD": e = `(${r(u.src[0]!)} + ${r(u.src[1]!)})`; break;
    case "IMUL": e = `(${r(u.src[0]!)} * ${r(u.src[1]!)})`; break;
    case "IDIV": e = `Math.floor(${r(u.src[0]!)} / ${r(u.src[1]!)})`; break;
    case "IMOD": e = `(${r(u.src[0]!)} % ${r(u.src[1]!)})`; break;
    case "IIDX": e = `Math.trunc(${r(u.src[0]!)})`; break;
    case "IVAL": {
      // an integer below 2²⁴ already is a float32: nothing to round
      const [lo, hi] = bounds(u.src[0]!);
      e = lo > -(2 ** 24) && hi < 2 ** 24 ? `(${r(u.src[0]!)})` : `f(${r(u.src[0]!)})`;
      break;
    }
    case "LOAD": e = `a${(u.arg as { slot: number }).slot}[${r(u.src[0]!)}]`; break;
    case "ADD": e = `f(${r(u.src[0]!)} + ${r(u.src[1]!)})`; break;
    case "MULTIPLY": e = `f(${r(u.src[0]!)} * ${r(u.src[1]!)})`; break;
    case "RECIPROCAL": e = `f(1 / ${r(u.src[0]!)})`; break;
    case "EXP": e = `f(Math.exp(${r(u.src[0]!)}))`; break;
    case "LOG": e = `f(Math.log(${r(u.src[0]!)}))`; break;
    case "SQRT": e = `f(Math.sqrt(${r(u.src[0]!)}))`; break;
    case "LESS_THAN": e = `(${r(u.src[0]!)} < ${r(u.src[1]!)} ? 1 : 0)`; break;
    case "WHERE": e = `(${r(u.src[0]!)} !== 0 ? ${r(u.src[1]!)} : ${r(u.src[2]!)})`; break;
    case "MATH": e = `f(M.${(u.arg as { fn: string }).fn}(${u.src.map(r).join(", ")}))`; break;
    case "RAND": e = `H(${r(u.src[1]!)}, ${r(u.src[0]!)})`; break;
    case "ACC": {
      const k = c.ctr.n++, rg = u.src[0]!, n = (rg.arg as { extent: number }).extent, op = (u.arg as { op: string }).op;
      c.names.set(rg.id, `r${k}`);
      c.scopes.push([]); c.scopeOf.set(rg.id, c.scopes.length - 1);
      const v = render(u.src[1]!, c);
      const inner = c.scopes.pop()!;
      c.names.delete(rg.id); c.scopeOf.delete(rg.id);
      const name = `acc${k}`;
      const step = op === "MAX" ? `Math.max(${name}, ${v})` : `f(${name} ${op === "MUL" ? "*" : "+"} ${v})`;
      c.scopes[scopeIdx(u, c)]!.push(`let ${name} = ${op === "MAX" ? "-Infinity" : op === "MUL" ? "1" : "0"};\nfor (let r${k} = 0; r${k} < ${n}; r${k}++) {\n${inner.join("\n")}\n${name} = ${step};\n}`);
      c.bound.set(u.id, name);
      return name;
    }
    default: throw new Error(`cpu: cannot render ${u.op}`);
  }
  if ((c.uses.get(u.id) ?? 1) > 1) {
    const name = `v${c.ctr.n++}`;
    c.scopes[scopeIdx(u, c)]!.push(`const ${name} = ${e};`);
    c.bound.set(u.id, name);
    return name;
  }
  return e;
}

type Compiled = (ins: Float32Array[], out: Float32Array) => void;
const MATH_JS = Object.fromEntries(Object.entries(MATHS).map(([k, m]) => [k, m.js]));
const compiled = new Map<string, Compiled>();
export function printKernel(k: UOp): string {
  const gid = k.src[0]!, n = Math.max(numel(k.shape), 1), nIn = kInputs(k).length;
  const c: Ctx = { names: new Map([[gid.id, "i"]]), ctr: { n: 0 }, uses: new Map(), deps: new Map(), scopeOf: new Map([[gid.id, 0]]), scopes: [[]], bound: new Map() };
  planUses(k.src[1]!, c);
  const body = render(k.src[1]!, c);
  const loads = Array.from({ length: nIn }, (_, i) => `const a${i} = ins[${i}];`).join(" ");
  return `(ins, o) => {\n${loads}\nfor (let i = 0; i < ${n}; i++) {\n${c.scopes[0]!.join("\n")}\no[i] = ${body};\n}\n}`;
}
const kernelFn = new WeakMap<UOp, Compiled>();                                 // a plan's kernels are printed once
const compile = (k: UOp): Compiled => {
  let fn = kernelFn.get(k);
  if (fn) return fn;
  const src = printKernel(k);
  fn = compiled.get(src);
  if (!fn) {
    if (compiled.size > 4096) compiled.clear();
    fn = new Function("f", "M", "H", `return ${src}`)(Math.fround, MATH_JS, hashUniform) as Compiled;
    compiled.set(src, fn);
  }
  kernelFn.set(k, fn);
  return fn;
};

export class CPU {
  readonly name = "cpu";
  #eval(opt: UOp[], inputs: Map<string, HostData>, bind: Map<number, UOp>): HostData[] {
    // one global evaluation order + last-use, so a freed intermediate is reclaimed the moment
    // its last consumer runs — peak memory becomes max-live, not the sum of all intermediates.
    const order: UOp[] = [], seen = new Set<number>();
    for (const root of opt) for (const u of toposort(root)) if (!seen.has(u.id)) { seen.add(u.id); order.push(u); }
    const lastUse = new Map<number, number>();
    order.forEach((u, i) => { for (const s of u.src) { const p = lastUse.get(s.id); if (p === undefined || p < i) lastUse.set(s.id, i); } });
    const keep = new Set(opt.map((r) => r.id));                               // outputs must survive to the end
    const deaths: number[][] = Array.from({ length: order.length }, () => []);
    for (const u of order) { const d = lastUse.get(u.id); if (d !== undefined && !keep.has(u.id)) deaths[d]!.push(u.id); }
    const memo = new Map<number, HostData>();
    order.forEach((u, i) => {
      if (u.op === "CONSTANT") memo.set(u.id, { data: u.arg as Float32Array, shape: u.shape });
      else if (u.op === "BUFFER") {
        const l = bind.get(u.id) ?? u;
        memo.set(u.id, l.op === "CONSTANT" ? { data: l.arg as Float32Array, shape: l.shape } : inputs.get(bufName(l)) ?? ((): never => { throw new Error(`cpu: missing input '${bufName(l)}'`); })());
      }
      else if (u.op === "FUSED") {
        const out = new Float32Array(Math.max(numel(u.shape), 1));
        compile(u)(kInputs(u).map((s) => memo.get(s.id)!.data), out);
        memo.set(u.id, { data: out, shape: u.shape });
      }
      for (const id of deaths[i]!) memo.delete(id);                            // free the dead
    });
    return opt.map((r) => memo.get(r.id)!);
  }
  // a graph seen before, by structure, runs its plan with this run's leaves bound in
  #plans = new Map<string, { opt: UOp[]; slots: UOp[] }>();
  realizeSync(roots: UOp[], inputs = new Map<string, HostData>()): HostData[] {
    const { key, leaves } = canonical(roots);
    let plan = this.#plans.get(key);
    if (!plan) {
      if (this.#plans.size > 512) this.#plans.delete(this.#plans.keys().next().value!);
      const p = placeholders(roots, leaves);
      plan = { opt: optimizeMany(p.roots), slots: p.slots };
      checkMemory(plan.opt, "FUSED");
      this.#plans.set(key, plan);
    } else { this.#plans.delete(key); this.#plans.set(key, plan); }
    return this.#eval(plan.opt, inputs, new Map(plan.slots.map((s, i) => [s.id, leaves[i]!])));
  }
}
