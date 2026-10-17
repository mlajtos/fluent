# Arity – design plan

Status: planned, not implemented. Today's behaviour is a documented wart (below, and the comment in `safeApply` in `language.ts`).

## The wart

```clojure
+(1, 2, 4)                ; 3 – the 4 is dropped
add(1, 2, 3)              ; 3
{ x, y | x + y }(1, 2, 3) ; 3
```

A function called with more arguments than it takes ignores the surplus: the JS function underneath ignores extra arguments, and nothing checks the count. A stray or mis-counted argument vanishes with no error.

## Two facts the design must respect

1. **A missing argument is ◌.** A lambda called with fewer arguments than parameters binds the rest to ◌ (`args[i] ?? null` in the lambda factory), and the prelude relies on it: `softmax` is `{ x, k | … }`, `μ` is `{ x, k, keep | … }`, `adam` is `{ lr, a | … }`, and each reads a missing argument as its default (`softmax` tests `identical(k, ◌)`). `softmax([1, 2])`, `μ(x)` and `adam(0.01)` are valid calls. Natives follow the same rule. So under-application is not an error in Fluent: a function that needs an argument answers ◌ with its own Error.
2. **Some natives offer optional extra arguments.** `ListMap(list, f)` calls `f(value, index)` through `FunctionApply`, and the prelude passes it one-parameter lambdas (`ListMap(vs, { v | make(v) })`, 11 of them in `prelude.fl`). A check on every call would reject these.

So the only error is over-application: more arguments than the function takes.

## Why not `fn.length`

- `fn.length` is a JS reflection artifact, not a Fluent contract. It counts the parameters before the first default or rest parameter.
- It cannot tell nullary `()` from variadic `(...args)`: both report 0. Lambdas, the combinators, the curried builtins and the variadic natives are all written with `(...args)` and report 0, so `args > fn.length` would reject valid tacit code like `(+ ∘ √)(9, 16)`.
- Patching around it (`length || Infinity`, a `variadic` flag, a `meta.arity ?? fn.length` fallback, special-casing 0) piles shims on a mismatch.

Arity is a contract that must be declared, not inferred from JS.

## The design

Arity is a declared attribute of a Fluent function value, set where the function is made, derived by the combinators, and checked once in `safeApply`.

### The contract

The most arguments a function takes, stored in `FunctionMeta` (`getMeta`/`setMeta`):

```ts
type Arity = number // the most arguments; Infinity for variadic
```

- Fixed binary: 2. Optional arguments (`sum(x, axis?, keep?)`): 3. Variadic (`List`): Infinity. Nullary (`Time`): 0, distinct from variadic. Overloaded `+`: 2.
- No minimum: by fact 1, fewer arguments are always legal.
- No arity in the metadata means unknown, and unknown is never checked. Nothing is read from `fn.length`, so no phase can reject a call it did not declare.

### Where arity is declared

1. **Lambdas**, in the lambda factory: arity = the number of parameters (`LambdaArgs`). A future rest parameter would give Infinity.
2. **`FunctionArity((binary, unary))`**: the larger of its candidates' arities, so 2 for `+`.
3. **Natives**, beside their definitions (`language.ts`; `host.ts` for the widgets and sources), with `setMeta`. Prelude built-ins are lambdas and get theirs from (1). The signature strings in the doc cards are not parsed.
4. **Combinators** derive the result's arity from their operands (table below).
5. **Curried results** (a function a function returns) declare their own arity where they are built. A lambda that returns a lambda already does, by (1).

### Callers that offer extra arguments

A native that offers an optional argument passes it only when the callee takes it. `ListMap` passes `(value, index)` when `f`'s arity is 2 or more, or unknown, and `(value)` when it is 1. This lands with the lambda declarations in Phase 1, since those are what would otherwise reject `ListMap`'s one-parameter callbacks. `ListReduce` and `ListScan` call `f` directly, outside `safeApply`, and are not checked.

### Combinator arity rules

| combinator | law | result arity |
|---|---|---|
| `f ∘ g` | `g(f(args))` | arity(f); g gets 1 |
| `⍨ f` | `f(y, x)` / `f(x, x)` | 2 |
| `g ⍥ f` | `f(g(x), g(y))` | arity(f); g gets 1 |
| `Φ(f, g, h)` | `g(f(args), h(args))` | the smaller of arity(f) and arity(h); g gets 2 |
| `f ⊸ g` | `g(f(x), y)` / `g(f(x), x)` | 2 |
| `f ⟜ g` | `f(x, g(y))` / `f(x, g(x))` | 2 |

A constant operand (a non-function tine, which the combinators already detect with `isFn`) binds an argument and lowers the result's arity: `(1 ⊸ +)(41)` is `+(1, 41)`, so `(1 ⊸ +)` takes 1, and `(1 ⊸ +)(x, y)`, which today drops x, becomes an error. In `Φ`, a constant tine takes any number of arguments and does not limit the result. The derivation sits beside the existing function-or-constant logic.

### Enforcement

In `safeApply`, after the "is not a function" guard and before the apply:

```ts
const most = getMeta(fnValue).arity // undefined: unknown, not checked
if (most !== undefined && argsValue.length > most)
  return new Error(`${nameOf(fnValue)} takes at most ${most} arguments, got ${argsValue.length}`)
```

The Error is a value, like every Fluent failure. The message names the function from its metadata.

## What this replaces

The code reasons about arity by hand in several places, and a declared arity replaces that reasoning:

- `FunctionArity`: `b == null ? unary(a) : binary(a, b)`
- `⍨`: `b == null ? f(a, a) : f(b, a)`
- `⊸` / `⟜`: `b == null ? a : b`
- the combinators' "a non-function tine is held constant"

With a declared arity, dispatch matches the count against the contract, and over-application errors come with it. Arity becomes one more attribute the combinators transform lawfully.

## Rollout

Each phase only adds coverage. Undeclared functions stay unchecked, so no phase can reject a valid call.

- **Phase 0 (done):** the wart is documented in this file and in `safeApply`.
- **Phase 1:** the `arity` field, the `safeApply` guard, arity on lambdas and `FunctionArity`, and `ListMap` passing the index only to a function that takes it. Fixes `+(1, 2, 4)` and `{ x, y | … }(1, 2, 3)`.
- **Phase 2:** arity on the natives.
- **Phase 3:** the combinator rules, so combinator results are checked too.
- **Phase 4 (optional):** `FunctionArity` and the combinators dispatch by arity, and the ad hoc `b == null` branches go.

## Tests, per phase

- Over-application is an Error for the declared set: `+(1, 2, 4)`, `sum(x, 0, 1, 2)`, `{ x, y | … }(1, 2, 3)`, `Time(5)`.
- Under-application stays legal: `softmax([1, 2])`, `μ(x)`, `adam(0.01)`, `sum(x)`, `{ x, y | y }(1)` is ◌.
- `ListMap` with a one-parameter and a two-parameter lambda, and with an undeclared native.
- Valid calls do not change: `1 ..< 5`, `(+ ∘ √)(9, 16)`, `List(…20 items…)`, `meshgrid(xs, ys, zs)`, `Time()`, and every undeclared function.

## Decisions

- The contract is a maximum, a number or Infinity. There is no minimum, because a missing argument is ◌.
- Natives declare arity with `setMeta` beside their definitions; doc signature strings are not parsed.
- Coverage is opt-in: `fn.length` is never consulted, and an undeclared function is never rejected.

## Non-goals

- A type system. This is arity only.
- Checking undeclared functions. Coverage grows only as declarations are added.
