# Time

How Fluent holds state and lets it change: `$`, `~`, `~~`, `:=`, `←`, `⟳`, buttons and
sources, ∇ and the optimizers, and what the IDE shows. One model covers all of them. This
document states the model, where each part lives, what it gives by construction, and
what it doesn't.

Each claim names its test in `tests.ts` or `tests.browser.ts`, or is marked as reasoning.

---

## 1. The model

**Time is a sequence of instants.** Each instant is one *occurrence*:

- the program's run;
- a `⟳` step;
- a button press;
- a write from the page (a slider, a drag, typing);
- a frame from a source (camera, microphone, pointer, the `Time()` clock);
- an arrival (a fetch, a value read back from the GPU).

Nothing changes between instants.

**A cell** – `$v`, `~v` or `~~v` – holds a value that changes only at an instant, by a
write. Its meaning is a step function of time: `c(t)` is the last value written at or
before `t` (Fran's `stepper`). A cell has an identity: it is the same cell at every
instant. The three kinds differ in one fact:

| | ∇ moves it |
|---|---|
| `~v` | yes |
| `~~v`, `$v` | no |

So a `~` is a cell plus an identity that ∇ can move along. `~~` and `$` are read the
same way; `:=` writes the tensor cells (`~`, `~~`), and lists of them at once.

**A computed** – `${ … }`, or an application that reads a cell where the program is
being defined (§2) – is a function of the cells: `b(t)` is its body with every cell
read at `t`. Read, it runs again only if a cell it read has been written since, at most
once an instant. Its value is settled into a leaf, as a cell's is, because it is kept
across instants. A computed may write only the cells it made while it ran; any other
write is an Error value ("a computed value cannot change state – change it in a step
{ … }"). So looking at a value never changes one.

**An occurrence** runs to completion. Inside it, reads see the cells as they are now,
including the writes made earlier in the same step: reading order is evaluation order.
A write takes effect at once. The display samples only between occurrences, so it never
shows a step half done.

**∇ with respect to a `~`.** For a loss `L` at instant `t`, let `F(θ₁…θₙ)` be `L`
evaluated with the trainable cells it reads set to `θ`, every other cell held at its
value at `t`. `∇(L)()` is `(F(θ(t)), [θ₁…θₙ], ∂F/∂θ at θ(t))`. The variables are the
loss's support – the trainable cells it reads, found by reading them, the same way a
computed finds its sources. A computed between the loss and the cells is part of `F`
through its definition, not its last value.

**The program** is definitions plus initial writes, evaluated once, at the instant its
text arrives. A top-level `s()` is the value at that instant; a bare `s` or a `${ … }`
is live. The program does not run again when something it read changes; an edit runs
it again. Its value is the IDE's live 0.

**The IDE shows a sample** of each live value, taken after an occurrence, at most once a
display frame.

---

## 2. Where a cell is named decides how it is read

The same name means the value over time where the program is defined, and the value now
where something happens.

| | where | a cell named bare |
|---|---|---|
| definition | the program body; a program `CodeEvaluate` runs (a REPL or Tour cell); a lambda called from either | a native that reads it follows it: the application is a computed |
| occurrence | a ⟳ step; a press; a ∇ body; a computed's own body; a function marked `FunctionNoAutoLift` | a native reads it now; a lambda receives the cell itself |

In `θ: ~1, d: θ × 2`, `d` follows θ; in a step, `θ × 2` is a number. `$` and `~` behave
the same way: in a step, `board← next(board)` needs no `()`.

What a function reads of its arguments decides when it lifts, as in Fran, where lifting
follows the types:

- **A native that computes** (`×`, `sin`, `stack`, `concat`) reads the numbers inside
  lists too, so `concat((a, b))` follows `a` and `b`.
- **A lambda, and a function on lists** (`ListMap`, `ListConcat`, `ListGet`, …), reads
  only what it is given. A list of cells is a structure, and its cells are their
  identities: `state: ListConcat((step, history), params, opt())` is a list of cells that
  `state := saved` can write.
- **A function that makes a cell** – `~`, `~~`, `$`, `Storage` – takes its initial value
  as it is now; `List`, like `( … )`, takes what it is given. Making an optimizer makes
  its memory, and a step writes it:
  `optimizer` and every constructor (`sgd`, `adam`, …) are occurrences
  (`FunctionNoAutoLift`), so a live learning rate is read at each step.
- **A cell holding a function** is applied as the function it holds: in a definition, as
  it holds it over time. `←` writes a function into a cell.

Tests: "one model of time" block; "a value defined from a variable follows it; one taken
in a step is its own".

---

## 3. One write semantics

A write – `s(v)`, `s← v`, `θ:= v`, and making a cell – stores the value now. A live value
is read, never followed; a list keeps the cells it holds as identities. To follow a
value, define it: `s: ${ x × 2 }`.

A value read back from the GPU arrives later. `CodesToString` gives an `Arriving` cell,
"" until the codes land. A native applied to one in an occurrence gives another, computed
now with the placeholder and again on arrival. A write of one writes the placeholder now,
and the value when it arrives – the arrival is an occurrence of its own. This is how a
dream reaches `dream` in the Dreamers.

A write made in a definition from a value still arriving stores the placeholder. No demo
does this.

Tests: "a write happens whether or not anything shows it", "a write stores a live value as
it is now, and does not follow it".

---

## 4. The display

The program's value is live 0 (protocol.ts); every value it shows that is a cell or a
computed is a live of its own. After a write, a sample of each live whose value changed
is taken as soon as the page can show it: on the next display frame, or when the page has
drawn the last batch. Three rules pace it:

- **Reads pace updates.** A sample whose numbers or text need a GPU read is shown when
  the read lands; the page keeps the last one until then. A value that changes every
  frame – the Dreamers' live completion – shows a little late, never blank. Test: "text
  decoded on the GPU from a value that changes every frame still shows" (browser).
- **The page paces batches.** None while the page still draws the last one. Test: "a
  change made while the page draws goes when the page has drawn, not a frame later".
- **While steps wait, a look costs at most a twentieth of the worker.** While a ⟳ loop is
  behind its count – training, as fast as it can go – a sample that took `s` ms makes
  that live's next one due `20·s` ms after this one was due, or at once if the live was
  idle. A cheap value is shown every frame, a costly one less often, and the steps keep
  the rest. With no steps waiting – a loop paced by `Time()`, an animation, a drag – every
  change is shown. Tests: "while steps wait, a look that costs a little over a twentieth
  of a frame is shown nearly every frame, not every other", "with no steps waiting, a
  look is shown every frame, whatever it costs".

`Text(s)` takes its text as a live slot, like `Button`'s label, so a text still arriving
shows when it lands.

What live values cost (Apple M1, Chrome, training steps a second over 10 s, two runs
each): the Dreamer 125–129; makemore 132; the Looped Dreamer 60, or 80–82 with only
its step count shown – its live completion and attention maps, three rounds of the
model each, take the difference. The cost rule caps each live at a twentieth of the
worker; a lower share shows them less often and trains faster.

---

## 5. Randomness

A draw is keyed by the reader's call that made it and by how many draws that call has
made in this run; each run starts the counts at 0. The same program draws the same
numbers on every run, and a draw elsewhere – a `${ ⚄(…) }` on the screen, sampled as often
as it is shown – moves no other draw. A computed that draws gets new numbers each time it
runs, like a spreadsheet's `RAND`.

Tests: "the same program trains the same weights on every run", "a draw is keyed by its
call: looking at another draw moves it not".

---

## 6. An edit keeps what training learned

An edit runs the program again, in a fresh scope. A top-level `~` or `~~` is the cell of
the run before when it is bound under the same name, made by the same expression, to the
same shape. An edit elsewhere keeps the trained weights; an edit of the cell's own
expression, or of its shape, starts it afresh.

What an optimizer's init made for a variable stays with the variable
(`TensorMemoryOf`), so a kept variable keeps its moments and step count, and the next step
continues the run.

Tests: "an edit keeps the last run's cells by name, expression and shape, and their
memory". In the IDE, a run that trains θ to 3 keeps 3 when the line above it is edited.

A reload is not an edit: the page starts a new worker. `Storage` keeps a checkpoint
across reloads (makemore).

---

## 7. The core

`language.ts`, MARK: Cells – 58 code lines: `Signal`, `Computed`, the reader hook,
`onWrite` and the occurrence flag. `preact` is not in the worker; the page keeps it for
React (`useSignals`).

- **One read hook serves reactivity and ∇.** A read goes to whoever is reading: a
  computed records the cell; ∇ substitutes the traced value of a trainable cell and
  records the variable, and runs a computed that reads one again inside, uncached.
- **Version stamps, no push edges.** A computed re-checks its sources at most once an
  instant and runs only if one changed; a run that gives the same value changes nothing
  downstream. A computed made inside a step holds no reference from its cells, so it is
  garbage when the step ends.
- **The host hears every write** (`onWrite`) and looks at the lives on the next frame.

---

## 8. What it gives by construction

| Property | Test |
|---|---|
| Looking never changes a value (a computed that draws excepted, §5) | "looking never changes a value" |
| A computed of a `~` follows it after the program has run | "a computed of a variable follows it after the program has run" |
| A write happens whether or not anything shows it | "a write happens whether or not anything shows it" |
| A computed cannot change state | "a computed cannot change state; it may change cells it made" |
| ∇ sees through a computed, every time | "∇ sees through a computed, every time" |
| A derived value is never seen half-updated | "a derived value is never seen half-updated, whatever reads it first" |
| The program runs once, even when it reads and writes a cell | "a program runs once, and what it shows of a variable follows it" |
| Every cell reads with `()` and writes with `(v)`; a variable's write is `:=` | "every cell reads with () and writes with (v)" |
| The same occurrences give the same states | "the same occurrences give the same states" |
| An edit keeps the trained cells | §6 |

What it does not give:

- **Effect order.** There are no user effects; two occurrences in one frame (two ⟳
  loops) run in the order the program made them. Reasoning.
- **A full checkpoint for free.** Every piece of training state is a cell, but the
  optimizer's are reached through its closure: `opt()` hands them out, and makemore saves
  them with the weights. Reasoning.
- **Undo.** Every write goes through `Signal.write`, so a write log is small, and tensors
  are immutable, so it holds references; it costs GPU memory. Not built.

---

## 9. Prior art

- **Fran** (Elliott & Hudak 1997), **push-pull FRP** (Elliott 2009): behaviors as
  functions of time, lifting, a stepper changes just after its occurrence; pull-evaluated
  computeds. Fluent's lifting is Fran's.
- **Elm, "A Farewell to FRP"** (2016): one state, an update, a view; signals were a
  stumbling block. Occurrences as thunks are enough.
- **Sodium, reactive-banana, S.js, Jane Street Incremental**: a held value changes after
  the transaction; observers see only stabilized states. Fluent's display samples between
  occurrences; inside one, reading order rules, as in **Céu**.
- **Adapton, self-adjusting computation, Svelte 5** (`state_unsafe_mutation`): the inner,
  memoized layer may not write. That is §1's rule for computeds.
- **Spreadsheets**: formulas can't write cells; `RAND` is volatile (§5). **Observable**:
  `mutable` and `viewof` are `~`/`:=` and widgets. **Pluto.jl**: untracked mutation goes
  stale – what reading a `~` without tracking did.
- **TensorFlow 2's `GradientTape`** watches the trainable variables read inside it, as
  `∇(loss)()` does; **Flax NNX** filters `Param` from other variable types, as `~` from
  `~~`; **PyTorch** forbids an in-place write to a leaf under autograd, as ∇ forbids writes.

---

## 10. Open

- **`FunctionNoAutoLift`** now means "an occurrence: it takes its arguments as they are".
  The name predates the model.
- **A computed's value is settled**, so a chain of lifted applications computes link by
  link instead of fusing at the read. Training is unaffected; a live drawing pays a kernel
  per link.
- **Two optimizers with the same init on one variable** share its memory (§6).
