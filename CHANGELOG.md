# Changelog

## 2026-11-05 – Fluent runs on its own engine

324 commits, from 2026-10-15 to 2026-11-05. The commit messages say each change in detail; this is the map.

### Changes that touch existing programs

- `Describe` and `Tensor` are gone.
- `window` replaces `windows` and `chunks`.
- `scan` runs along the leading axis, as `reduce` does, and is always a left-to-right fold; `associativeScan` is the fast one, for an f you vouch is associative.
- `←` and `:=` are one write; `$( )` makes a place.
- Every array is 32-bit float; the dtype tags are gone.
- A program binds under the Prelude, not over it.
- A range is the whole numbers between its ends.
- A saved link carries the version it was written in, and an older link is migrated as it loads.

### Engine

- Fluent computes on its own engine, in `engine/`, on the CPU and on WebGPU; jax-js is gone. [OwnEngine.md](OwnEngine.md) describes it.
- Kernels are fused and planned: a value read twice is computed once, contractions are tiled, a reduction to few outputs splits over workgroups, and small inputs share a binding.
- `⍣` runs as one engine loop step, and as one loop over a `⍤` frame.
- `∇` grows linearly through `⍢`, `⍣` and gathers under `⍤`. A maximum's derivative is its winner's, and 0 × ∞ is 0, so higher derivatives hold at 0.
- A value nothing reads frees its device buffer at once. A plan that needs more than 2 GB at once is an Error.
- The same program draws the same random numbers on every run.
- The CPU and the GPU agree on −0, on NaN and on a maximum; tanh is ±1 far from 0 on Metal.

### Language

- The Prelude is Fluent, in [prelude.fl](prelude.fl): arithmetic, comparisons, shape functions, `⊗`, `⍤`, `∙`, the tacit combinators, optimizers, drawing, `plot`, `einsum` and the FFT. `language.ts` is 41% smaller.
- One vocabulary for arrays, lists and strings: `length`, `take`, `drop`, `reverse`, `concat`, `x_i`.
- Each, reduce and scan walk the items, data first: `x ¨ f`, `x ⌿ f`, `x ⍀ f`. They also type as ASCII, as in K: `x ' f`, `x / f`, `x \ f`. `/` divides by anything but a function.
- `++` is `concat`, `⍕` is `format` and `⍎` is `execute`; `⍎(⍕x)` is x for data.
- `÷` and `^` have one-argument forms; `⌺` is an operator.
- `⍣ -1` inverts a function by Newton's method, per element; `⍢` is Under.
- An array applied is its linear map.
- Errors are values. `◌` is an argument not given; an unknown name fails with its name; a failing statement fails its block; `cascade`, `cond` and `try` choose between candidates that answer or decline.
- An optimizer is a value, `optimizer(init, rule)`, and it trains what its loss reads. sgd, adam, adamw, adagrad, rmsprop, adadelta, adamax, nadam, radam, amsgrad, adabelief, yogi, lion, lamb and muon are built on it. `opt()` is its memory, so a checkpoint can hold it.
- Cells, computeds and occurrences make one model of state; [Time.md](Time.md) describes it. `Storage(key)` keeps a value in the browser across reloads.
- `memo` keeps a function's answers; a function carries its identity element, and folding nothing gives it.
- `StringFind` and `StringSplit`.

### Pictures

- A picture is a Fluent value: `draw`, `dots`, `line`, `polygon`, `bezier`, `arc` and `letters`, lit with inks. They are drawn in units, at the display's own pixels.
- Fonts are values, in [fonts.fl](fonts.fl): BQN386 and Hyperlegible.
- `plot`, `viridis`, `heat` and `bars` are in the Prelude; Plotly is gone.
- `drag(model, spots, move, view)` lets a hand move a state.

### IDE

- The program runs in a worker, which paints every picture and parses for the editor. A lost GPU starts another worker.
- The page opens on the Tour.
- Typing a glyph's name offers the glyph: `each` gives `¨`.
- An Error is marked where it happens, in the editor and in the view.

### Gallery and Tour

- New examples: Caustics, Differentiable Bézier, Moiré secrets, Learning in frequencies, The Looped Sudoku Solver, makemore, The Descent Path, Around, Drawing as values, and eleven where you drag a state by hand.
- The Looped Dreamer learns makemore's 32,033 names.
- `?example=gallery` lists every example in a table.
- The Tour has a Draw it room, and its checks test what the code does.
- [Style.md](Style.md) says how to lay out Fluent so the notation does the work, and the gallery follows it.
