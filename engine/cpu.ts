// The CPU backend and reference oracle. After optimize a graph is leaves + FUSED, so
// the CPU prints each kernel as a JavaScript function — the same loop nest the WGSL printer
// prints — with every value op fround'd so it matches the GPU's f32 arithmetic.

import type { UOp, Syntax } from "./ir.ts";
import { numel, kInputs, bufName, toposort, MATHS, hashUniform, bounds, nest, planUses, render, INDEX_SYNTAX } from "./ir.ts";
import { optimizeMany, planner, checkMemory } from "./opt.ts";

export interface HostData { data: Float32Array; shape: readonly number[] }

const jsNumber = (v: number): string => (Number.isNaN(v) ? "NaN" : v === Infinity ? "Infinity" : v === -Infinity ? "-Infinity" : Object.is(v, -0) ? "-0" : String(Math.fround(v)));
const rounded = (o: string) => (r: (i: number) => string) => `f(${r(0)} ${o} ${r(1)})`, call = (fn: string) => (r: (i: number) => string) => `f(${fn}(${r(0)}))`;
// every shared subexpression is a `const`
const JS: Syntax = {
  op: {
    ...INDEX_SYNTAX,
    ICONST: (_, u) => String(u.arg as number),
    CONSTANT: (_, u) => jsNumber((u.arg as Float32Array)[0]!),
    IDIV: (r) => `Math.floor(${r(0)} / ${r(1)})`,
    IIDX: (r) => `Math.trunc(${r(0)})`,
    // an integer below 2²⁴ already is a float32: nothing to round
    IVAL: (r, u) => { const [lo, hi] = bounds(u.src[0]!); return lo > -(2 ** 24) && hi < 2 ** 24 ? `(${r(0)})` : `f(${r(0)})`; },
    ADD: rounded("+"), MULTIPLY: rounded("*"), RECIPROCAL: (r) => `f(1 / ${r(0)})`, EXP: call("Math.exp"), LOG: call("Math.log"), SQRT: call("Math.sqrt"),
    LESS_THAN: (r) => `(${r(0)} < ${r(1)} ? 1 : 0)`,
    WHERE: (r) => `(${r(0)} !== 0 ? ${r(1)} : ${r(2)})`,
    MATH: (r, u) => `f(M.${(u.arg as { fn: string }).fn}(${u.src.map((_, i) => r(i)).join(", ")}))`,
    RAND: (r) => `H(${r(1)}, ${r(0)})`,
  },
  acc: (a, op, r, n, body, v) => op === "ARGMAX"
    ? `let ${a} = 0, ${a}m = -Infinity;\nfor (let ${r} = 0; ${r} < ${n}; ${r}++) {\n${body.join("\n")}\nconst ${a}v = ${v};\nif (${a}v > ${a}m || (${a}v !== ${a}v && ${a}m === ${a}m)) { ${a}m = ${a}v; ${a} = ${r}; }\n}`
    : `let ${a} = ${op === "MAX" ? "-Infinity" : op === "MUL" ? "1" : "0"};\nfor (let ${r} = 0; ${r} < ${n}; ${r}++) {\n${body.join("\n")}\n${a} = ${op === "MAX" ? `Math.max(${a}, ${v})` : `f(${a} ${op === "MUL" ? "*" : "+"} ${v})`};\n}`,
  let: (name, e) => `const ${name} = ${e};`,
  named: (uses) => uses > 1,
};

type Compiled = (ins: Float32Array[], out: Float32Array) => void;
const MATH_JS = Object.fromEntries(Object.entries(MATHS).map(([k, m]) => [k, m.js]));
const compiled = new Map<string, Compiled>();
export function printKernel(k: UOp): string {
  const c = nest(JS, new Map([[k.src[0]!.id, "i"]]));
  planUses(k.src[1]!, c);
  const body = render(k.src[1]!, c), loads = kInputs(k).map((_, i) => `const a${i} = ins[${i}];`).join(" ");
  return `(ins, o) => {\n${loads}\nfor (let i = 0; i < ${Math.max(numel(k.shape), 1)}; i++) {\n${c.scopes[0]!.join("\n")}\no[i] = ${body};\n}\n}`;
}
const compile = (k: UOp): Compiled => {
  const src = printKernel(k);
  let fn = compiled.get(src);
  if (!fn) {
    if (compiled.size > 4096) compiled.clear();
    fn = new Function("f", "M", "H", `return ${src}`)(Math.fround, MATH_JS, hashUniform) as Compiled;
    compiled.set(src, fn);
  }
  return fn;
};
const inputsOf = (u: UOp): readonly UOp[] => (u.op === "FUSED" ? kInputs(u) : u.src);

export class CPU {
  readonly name = "cpu";
  // A plan is its kernels compiled, in one evaluation order with each value's last use, so an
  // intermediate is freed the moment its last reader has run: peak memory is the most alive
  // at once, not the sum of all intermediates.
  #plan = planner((roots) => {
    const opt = optimizeMany(roots);
    checkMemory(opt, "FUSED");
    const order = toposort(opt, inputsOf), last = new Map<number, number>(), free = order.map((): number[] => []), keep = new Set(opt.map((r) => r.id));
    order.forEach((u, i) => { for (const s of inputsOf(u)) last.set(s.id, i); });
    for (const [id, i] of last) if (!keep.has(id)) free[i]!.push(id);
    return { roots: opt, steps: order.map((u, i) => ({ u, run: u.op === "FUSED" ? compile(u) : null, free: free[i]! })) };
  });
  realizeSync(roots: UOp[], inputs = new Map<string, HostData>()): HostData[] {
    const { plan, bind } = this.#plan(roots), memo = new Map<number, HostData>();
    for (const { u, run, free } of plan.steps) {
      const l = bind.get(u.id) ?? u;
      if (run) {
        const out = new Float32Array(Math.max(numel(u.shape), 1));
        run(kInputs(u).map((s) => memo.get(s.id)!.data), out);
        memo.set(u.id, { data: out, shape: u.shape });
      } else memo.set(u.id, l.op === "CONSTANT" ? { data: l.arg as Float32Array, shape: l.shape } : inputs.get(bufName(l)) ?? ((): never => { throw new Error(`cpu: missing input '${bufName(l)}'`); })());
      for (const id of free) memo.delete(id);
    }
    return plan.roots.map((r) => memo.get(r.id)!);
  }
}
