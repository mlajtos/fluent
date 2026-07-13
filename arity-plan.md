# Arity — design plan

Status: **planned, not implemented.** Today's behaviour is a documented wart (see below and the comment in `safeApply`, language.ts).

## The wart

```clojure
+(1, 2, 4)      ; 3   — the 4 is silently dropped
add(1, 2, 3)    ; 3
{ x, y | x + y }(1, 2, 3)   ; 3
```

A binary function called with too many arguments ignores the surplus, because the underlying JS function ignores extra args and nothing checks the count. It's a typo-class bug (a stray or mis-counted argument vanishes with no error).

## Why the quick fix was rejected

The tempting fix — check `argsValue.length` against `fn.length` in `safeApply` — rests on a foundation that can't hold:

- **`fn.length` is a JS reflection artifact, not a Fluent arity contract.** It reports the count of parameters before the first default/rest — an accident of how each wrapper was written, not a declaration of intent.
- **It can't represent Fluent's arity space.** `()` (nullary) and `(...args)` (variadic) both report `0`, so it structurally cannot distinguish "takes nothing" from "takes anything". Every function built with the `(...args)` idiom — **lambdas, the `compose`/`fork`/`over` combinators, variadic builtins, curried builtins** — reports `0`. A naive `args > fn.length` check would false-positive and reject valid tacit code like `(+ ∘ √)(9, 16)`.
- Patching around this (`length || Infinity`, a `variadic` flag, a `meta.arity ?? fn.length` fallback, special-casing `0`) is a pile of shims over a mismatch. It also gives incomplete, surprising coverage.

The principled position: **arity is a semantic contract that must be *declared*, not inferred from JS.**

## The design

Arity is a **declared, first-class attribute of every Fluent function value**, set at construction, composed by the combinators, and enforced once at the application choke point.

### The contract

A range, stored in the existing function-metadata (`FunctionMeta`, via `getMeta`/`setMeta`):

```ts
type Arity = { min: number, max: number }   // max may be Infinity
```

- Fixed: `{2, 2}`. Optional: `{1, 2}`. Variadic: `{0, Infinity}`. Nullary: `{0, 0}` — now distinct from variadic. Overloaded (`+`): `{1, 2}`.
- **Ranges, not sets:** every real Fluent arity is a contiguous range, so `{min, max}` suffices. If a genuinely non-contiguous arity ever appears, widen to a predicate `(n) => boolean` — not now.
- **Absent metadata = unknown = unchecked.** Declaration is the only source of truth; nothing is read from `fn.length`. A function with no declared arity is never rejected (so no phase can false-positive).

### Where arity is declared (construction sites)

1. **Lambdas** — at the lambda factory: `setMeta(fn, { arity: { min: n, max: n } })`, `n = LambdaArgs.length`. (Fluent lambdas are fixed-arity today; a future rest-param would set `max: Infinity`.)
2. **`FunctionArity((binary, unary))`** — `{ min: min(candidate mins), max: max(candidate maxes) }`, i.e. `{1, 2}`, derived from the candidates' own declared arities.
3. **Builtins** — declared next to `doc()`. The doc signatures already encode arity (`"x - y"` → `{2,2}`, `"sum(x, axis?)"` → `{1,2}`, `"conv(arr, kernel)"` → `{2,2}`). Add an explicit `arity` field to `doc()` rather than parsing the string (robust > cute). Undocumented builtins stay unchecked.
4. **Combinators** — derive the result's arity from the operands (rules below). This is just making explicit what each combinator already does.
5. **Curried builtins** — the returned function (e.g. `sgd(0.1)`) declares its own arity where it's built.

### Combinator arity rules (these *are* the combinators' semantics)

Given the operand arities, the result arity is derived. Function operands contribute their arity; a **constant** operand (a non-function tine — combinators already gate on `isFunction`) binds an argument and so *lowers* the effective arity.

| combinator | law | result arity |
|---|---|---|
| `f ∘ g` | `g(f(args))` | `arity(f)` (args go to `f`; `g` must accept 1) |
| `⍨ f` | `f(y, x)` / `f(x, x)` | `{1, 2}` (needs `2 ∈ arity(f)`) |
| `g ⍥ f` | `f(g(x), g(y))` | `arity(f)` (`g` accepts 1) |
| `Φ(f, g, h)` | `g(f(args), h(args))` | `arity(f) ∩ arity(h)` (`g` accepts 2) |
| `f ⊸ g` | `g(f(x), y)` / `g(f(x), x)` | `{1, 2}` |
| `f ⟜ g` | `f(x, g(y))` / `f(x, g(x))` | `{1, 2}` |

Constant-operand nuance: `(1 ⊸ +)` binds `+`'s left argument, so `(1 ⊸ +)(41) = +(1, 41)` — the result is unary. The rules above assume function operands; the constant case shifts the range down by the number of bound arguments. The combinators already track function-vs-constant, so the arity derivation lives beside that logic.

### Enforcement

In `safeApply`, after the "is not a function" guard, before the apply:

```ts
const spec = getMeta(fnValue).arity            // undefined = unknown = skip
if (spec && (argsValue.length < spec.min || argsValue.length > spec.max))
  return new Error(`${nameOf(fnValue)} takes ${spec.min}–${spec.max} arguments, got ${argsValue.length}`)
```

- Errors are **values** (Fluent's errors-as-values), consistent with the rest.
- The message uses the function's name/signature from meta for a good error.
- Both bounds checked — the range gives lower-bound (`+()`, too few) for free.

## What this buys beyond the check

The codebase already hand-rolls arity reasoning in several places — a first-class arity **replaces** it rather than adding to it:

- `FunctionArity`: `(a, b) => b === undefined ? unary(a) : binary(a, b)`
- `⍨`: `b === undefined ? f(a, a) : f(b, a)`
- `⊸` / `⟜`: `b === undefined ? a : b`
- combinators' "a non-function tine is held constant"

Once arity is a declared attribute, dispatch becomes "match the count against the contract", and over-application errors fall out for free. This lines up with the algebra-of-programs direction: arity is another lawful attribute the combinators transform.

## Rollout (each phase is independently shippable and only *adds* coverage — undeclared = unchecked, so no phase can false-positive)

- **Phase 0 (done):** document the wart; no behaviour change. This file + the `safeApply` comment.
- **Phase 1:** add the `Arity` type, the `meta.arity` field, and the `safeApply` guard. Declare arity on **lambdas** and **`FunctionArity`**. Ships the `+(1, 2, 4)` fix and `{x, y | …}(1, 2, 3)` for the declared set. Nothing else is checked.
- **Phase 2:** declare arity on all documented builtins (add the field to `doc()`).
- **Phase 3:** combinator propagation rules — now combinator results are checked too.
- **Phase 4 (optional):** refactor `FunctionArity`/combinator dispatch to be arity-driven, deleting the ad-hoc `b === undefined` branches.

## Tests (per phase)

- Over-application errors for the newly-declared set: `+(1, 2, 4)`, `sum(x, 0, 1, 2)`, `{x, y | …}(1, 2, 3)`.
- Under-application errors: `+()`.
- No regression on valid calls: optional-arg builtins (`sum(x)`, `1 ..< 5` = `TensorRange` with 1 or 2), combinators (`(+ ∘ √)(9, 16)`), variadics (`List(…20 items…)`, `stack(a, b)`), nullary (`Time()`), and — until each is declared — everything undeclared stays silently accepted.
- Nullary vs variadic: `Time(5)` errors (`{0,0}`), `List(1, 2, 3)` does not (`{0, ∞}`).

## Decisions locked

- `{min, max}` range, not a set or predicate. Revisit only if a non-contiguous arity appears.
- Explicit `arity` field on `doc()`, not signature-string parsing.
- Both bounds enforced from the start (the range has `min` for free).
- Declaration is opt-in coverage; **`fn.length` is never consulted.** Undeclared functions are unchecked, never rejected.

## Non-goals

- A type system. This is arity only.
- Checking undeclared functions. Coverage grows only as declarations are added.
