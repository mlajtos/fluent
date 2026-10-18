// A graph built for one cell, run over a frame – the rank conjunction ⍤ as a view, the
// way JAX and Futhark vectorize. Every node a cell placeholder reaches gets the frame F in
// front of its shape: an elementwise op takes it along, a reduce, slice or pad shifts its
// axis past it, a permute keeps it in front, a reshape or broadcast prefixes it, and a
// gather or scatter offsets its indices by cell. A node no cell reaches is shared by every
// cell, so the frame sees it broadcast.

import type { UOp } from "./ir.ts";
import * as ir from "./ir.ts";

export function batch(root: UOp, cells: ReadonlyMap<UOp, UOp>, F: readonly number[]): UOp {
  const f = F.length, N = ir.numel(F), memo = new Map<UOp, UOp>(cells);
  const lift = (s: UOp): UOp => memo.get(s) ?? ir.broadcastTo(s, [...F, ...s.shape]);
  // the frame's cells laid end to end, n rows each: cell c's row r is row c·n + r
  const rows = (s: UOp): UOp => ir.reshape(lift(s), [N * s.shape[0]!, ...s.shape.slice(1)]);
  const offset = (idx: UOp, n: number): UOp =>
    ir.reshape(ir.add(ir.reshape(lift(idx), [N, idx.shape[0]!]), ir.reshape(ir.mul(ir.iota(N), ir.konst(n)), [N, 1])), [N * idx.shape[0]!]);
  const outer = ir.at.site;
  try {
    for (const u of ir.toposort(root)) {
      if (memo.has(u) || !u.src.some((s) => memo.has(s))) continue;
      ir.at.site = u.site;                                                     // what the frame builds comes from the node it rewrites
      const B = [...F, ...u.shape], a = u.arg as { axis: number; size: number };
      let v: UOp;
      switch (u.op) {
        case "ADD": case "MULTIPLY": case "RECIPROCAL": case "EXP": case "LOG": case "SQRT":
        case "LESS_THAN": case "WHERE": case "DETACH": case "MATH":
          v = ir.node(u.op, u.src.map(lift), u.arg, B); break;
        case "REDUCE": case "SLICE": case "PAD": v = ir.node(u.op, [lift(u.src[0]!)], { ...a, axis: a.axis + f }, B); break;
        case "PERMUTE": v = ir.permute(lift(u.src[0]!), [...F.keys(), ...(u.arg as number[]).map((p) => p + f)]); break;
        case "RESHAPE": v = ir.reshape(lift(u.src[0]!), B); break;
        case "WINDOW": v = ir.windows(lift(u.src[0]!), u.arg as number[]); break;
        case "BROADCAST": { const s = u.src[0]!.shape; v = ir.broadcastTo(ir.reshape(lift(u.src[0]!), [...F, ...Array(u.shape.length - s.length).fill(1), ...s]), B); break; }
        case "GATHER": {
          // a table of the cell's own is read at the cell's rows; a shared one as it is
          const [t, i] = u.src as [UOp, UOp], own = memo.has(t);
          v = ir.reshape(ir.gather(own ? rows(t) : t, own ? offset(i, t.shape[0]!) : ir.reshape(lift(i), [N * i.shape[0]!])), B); break;
        }
        case "SCATTER_ADD": { const [i, x] = u.src as [UOp, UOp]; v = ir.reshape(ir.scatterAdd(offset(i, a.size), rows(x), N * a.size), B); break; }
        default: throw new Error(`batch: no rule for ${u.op}`);
      }
      memo.set(u, v);
    }
  } finally { ir.at.site = outer; }
  return lift(root);
}
