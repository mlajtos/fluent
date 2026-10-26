// A graph built for one cell, run over a frame – the rank conjunction ⍤ as a view, the
// way JAX and Futhark vectorize. Every node a cell placeholder reaches gets the frame F in
// front of its shape: an elementwise op takes it along, a reduce, slice or pad shifts its
// axis past it, a permute keeps it in front, a reshape or broadcast prefixes it, and a
// gather or scatter takes it as a batch: each cell reads, and adds into, its own rows. A
// loop on a cell is the loop on the frame, a lane per cell: its states, its body and its
// verdict take the frame, and each lane stops at its own verdict. A node no cell reaches is
// shared by every cell, so the frame sees it broadcast.

import type { UOp } from "./ir.ts";
import * as ir from "./ir.ts";

// the graph over the frame, and the loops it reads – not those inside another loop's body
export const batch = (root: UOp, cells: ReadonlyMap<UOp, UOp>, F: readonly number[]): { root: UOp; loops: UOp[] } => {
  const { roots: [r], loops } = frame([root], new Map(cells), F);
  return { root: r!, loops: [...loops.values()].flat() };
};

// `memo` holds the nodes framed already; `loops`, the loops framed, by the body their states share
function frame(roots: readonly UOp[], memo: Map<UOp, UOp>, F: readonly number[]): { roots: UOp[]; loops: Map<ir.Body, UOp[]> } {
  const f = F.length, loops = new Map<ir.Body, UOp[]>();
  const lift = (s: UOp): UOp => memo.get(s) ?? ir.broadcastTo(s, [...F, ...s.shape]);
  const outer = ir.at.site;
  try {
    for (const u of ir.toposort(roots)) {
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
        case "GATHER": v = ir.gather(lift(u.src[0]!), lift(u.src[1]!)); break;
        case "SCATTER_ADD": v = ir.scatterAdd(lift(u.src[0]!), lift(u.src[1]!), a.size); break;
        case "LOOP": {
          // the body reads what the loop closes over as the loop's src, framed here already
          const { body, n, out } = u.arg as ir.LoopArg, k = body.carries.length;
          let ls = loops.get(body);
          if (!ls) {
            const carries = body.carries.map((c) => ir.buffer("%carry", [...F, ...c.shape]));
            const outs = frame(body.outs, new Map([...memo, ...body.carries.map((c, j) => [c, carries[j]!] as const)]), F).roots;
            loops.set(body, ls = ir.loop(u.src.slice(0, k).map(lift), carries, outs, n));
          }
          v = ls[out]!; break;
        }
        default: throw new Error(`batch: no rule for ${u.op}`);
      }
      memo.set(u, v);
    }
  } finally { ir.at.site = outer; }
  return { roots: roots.map(lift), loops };
}
