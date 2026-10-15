// The part of optax Fluent uses: the transformation shape, SGD (with optional momentum)
// and applying updates — graphs like everything else, realized when the step commits.

import { Array, multiply, add } from "./numpy.ts";
import { tree } from "./jax.ts";

export type OptState = unknown;
export interface GradientTransformation {
  init(params: unknown): OptState;
  update(updates: unknown, state: OptState, params?: unknown): [unknown, OptState];
}

export function sgd(learningRate: number, opts?: { momentum?: number }): GradientTransformation {
  const momentum = opts?.momentum;
  return {
    init: (params) => (momentum ? tree.map((p: Array) => multiply(p, 0), params) : null),
    update(updates, state) {
      if (!momentum) return [tree.map((g: Array) => multiply(g, -learningRate), updates), state];
      const velocity = tree.map((v: Array, g: Array) => add(multiply(v, momentum), g), state, updates);
      return [tree.map((v: Array) => multiply(v, -learningRate), velocity), velocity];
    },
  };
}
export const applyUpdates = (params: unknown, updates: unknown): unknown => tree.map((p: Array, u: Array) => add(p, u), params, updates);
