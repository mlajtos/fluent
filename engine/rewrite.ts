// THE engine: bottom-up, innermost, to fixpoint, interning as it goes. Value-preserving
// rewrites ride this; the schedule (opt.ts::fuse) and ∇ (grad.ts) are deliberately not
// fixpoints — recompute-vs-materialize needs a cost model, reverse accumulation sums
// cotangents at forks, and neither has a normal form.

import type { UOp, Op } from "./ir.ts";
import { node } from "./ir.ts";

export type Rule = (u: UOp) => UOp | null;

// A RANGE is a loop VARIABLE: {extent} says how far it counts, not which loop it is —
// CSE merging two equal-extent ranges would collapse a nested loop into one variable.
const argKey = (u: UOp): string => {
  if (u.op === "RANGE") return `#${u.id}`;
  if (u.arg instanceof Float32Array)                                          // scalar constants key by VALUE (two konst(0.5) are one node); tensors by identity
    return u.shape.length === 0 ? (Number.isNaN(u.arg[0]!) ? "C:NaN" : Object.is(u.arg[0], -0) ? "C:-0" : `C:${u.arg[0]}`) : `#${u.id}`;
  return JSON.stringify(u.arg ?? null);
};
const structKey = (u: UOp) => `${u.op}|${u.src.map((s) => s.id).join(",")}|${argKey(u)}`;

// Innermost bottom-up rewrite to fixpoint. `done` memoizes by INPUT id (each node of a
// shared DAG rewrites once — without it, diamonds go exponential); `cache` by structure
// (CSE of rule outputs); shared across roots so common subgraphs intern to one node.
// Iterative (explicit stack): real graphs run thousands of nodes deep.
export function graphRewriteMany(roots: UOp[], rules: Rule[]): UOp[] {
  const intern = new Map<string, UOp>(), cache = new Map<string, UOp>(), done = new Map<number, UOp>();
  let steps = 0;
  interface Frame { u: UOp; stage: 0 | 1 | 2; key?: string; fired?: UOp }
  const run = (root: UOp): void => {
    const stack: Frame[] = [{ u: root, stage: 0 }];
    while (stack.length) {
      const f = stack[stack.length - 1]!, u = f.u;
      if (f.stage === 0) {
        if (done.has(u.id)) { stack.pop(); continue; }
        if (++steps > 5_000_000) throw new Error("graphRewrite: rules did not terminate");
        f.stage = 1;
        for (let i = u.src.length - 1; i >= 0; i--) if (!done.has(u.src[i]!.id)) stack.push({ u: u.src[i]!, stage: 0 });
      } else if (f.stage === 1) {
        const built = u.src.length ? node(u.op, u.src.map((s) => done.get(s.id)!), u.arg, u.shape) : u;
        const key = structKey(built);
        const hit = cache.get(key);
        if (hit) { done.set(u.id, hit); stack.pop(); continue; }
        const cur = intern.get(key) ?? (intern.set(key, built), built);
        let fired: UOp | null = null;
        for (const r of rules) { const o = r(cur); if (o) { fired = o; break; } }
        if (!fired) { cache.set(key, cur); done.set(u.id, cur); stack.pop(); continue; }
        f.stage = 2; f.key = key; f.fired = fired;                             // the rule's output is itself rewritten in full
        stack.push({ u: fired, stage: 0 });
      } else {
        const res = done.get(f.fired!.id)!;
        cache.set(f.key!, res); done.set(u.id, res); stack.pop();
      }
    }
  };
  for (const r of roots) run(r);
  return roots.map((r) => done.get(r.id)!);
}
export const graphRewrite = (root: UOp, rules: Rule[]): UOp => graphRewriteMany([root], rules)[0]!;
