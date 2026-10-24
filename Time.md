# Time

How Fluent holds state and lets it change: `$`, `~`, `←`, `⟳`, buttons and sources, ∇
and the optimizers, and what the IDE shows. One model covers all of them. This document
states the model, where each part lives, what it gives by construction, and what it
doesn't.

Each claim names its test in `tests.ts` or `tests.browser.ts`, or is marked as reasoning.

---

## 1. The model

**Time is a sequence of instants.** Each instant is one *occurrence*:

- the program's run;
- a `⟳` step;
- a button press;
- a write from the page (a slider, a drag, typing);
- a frame from a source (camera, microphone, pointer, the `Time()` clock), or a new
  density of the display (`Density()`);
- an arrival (a fetch, a value read back from the GPU).

Nothing changes between instants.

**A cell** – `$v` or `~v` – holds a value that changes only at an instant, by a write.
Its meaning is a step function of time: `c(t)` is the last value written at or before `t`
(Fran's `stepper`). A cell has an identity: it is the same cell at every instant. The two
kinds differ in one fact: ∇ moves a `~` unless it is told which cells to move. A `~`
holds numbers; a `$` holds any value.

**A cell's name is its value.** Wherever a value is used, a cell stands for what it holds:
`s × 2` doubles it, and a cell applied is its value applied – `W(x)` applies the numbers W
holds as their linear map, and writes nothing. Any value applied to nothing is itself, so
`s()` is `s`. Test: "a cell applied is its value applied, so W(x) in a step applies W and
writes nothing".

**A write** is `c← v`, also spelled `c:= v`: one write for every cell. A list of cells
takes a list of as many values, every one read before any is written – `(a, b) ← (b, a)`
swaps. Test: "← and := are one write: any cell, and a list of cells at once".

**A live value** – a `${ … }`, or an application in a definition that read cells and
changed none (§2) – is a function of the cells: `b(t)` is its body with every cell read at
`t`. Read, it runs again only if a cell it read has been written since, at most once an
instant. Its value is the graph it built, not settled into a leaf: a chain of live values
builds one graph, which the engine fuses where it is read, and computes once where a later
instant reads it. A live value may write only the cells it made while it ran; any other
write is an Error value ("a computed value cannot change state – change it in a step
{ … }"). So looking at a value never changes one.

**An occurrence** runs to completion. Inside it, reads see the cells as they are now,
including the writes made earlier in the same step: reading order is evaluation order. A
write takes effect at once. The display samples only between occurrences, so it never
shows a step half done.

**∇ along cells.** For a loss `L` at instant `t` and a set `S` of cells – the cells listed,
or else the `~` cells `L` reads – let `F(v)` be `L` evaluated with `S` set to `v`, every
other cell held at its value at `t`. `∇(L)()` and `(∇L)(cells)` are `(F(S(t)), S,
∂F/∂S at S(t))`. A live value made from a cell in `S` is part of `F` through its
definition, not its last value: it runs again inside. An optimizer trains the cells it is
given, a `$` as well as a `~`. Tests: "∇ sees through a computed, every time", "∇ and an
optimizer take any listed cell, and run again every value made from it".

**The program** is definitions and initial writes, evaluated once, at the instant its text
arrives. What it defines from cells follows them; what it writes or makes happens then,
once – a `⟳` it starts starts at the run. The program does not run again when something
it read changes; an edit runs it again. Its value is the IDE's live 0. Test: "a program
runs once, and what it shows of a variable follows it".

**The IDE shows a sample** of each live value, taken after an occurrence, at most once a
display frame.

---

## 2. Following is observed

The same name means the value over time where the program is defined, and the value now
where something happens.

| | where | an application |
|---|---|---|
| definition | the program body; a program `CodeEvaluate` runs (a REPL or Tour cell); the body of a lambda either calls | runs now, watched: if it read cells and changed none, it is a live value that follows them, its first run its value; if it wrote a cell or made one, it ran once |
| occurrence | a ⟳ step; a press; a ∇ body; a live value's own body | runs now; never watched |

In `θ: ~1, d: θ × 2`, `d` follows θ; in a step, `θ × 2` is a number, and `board← next(board)`
reads `board` now. A lambda's body is part of the definition, so a function given a cell
follows it, or writes it: `inc: { c | c← c + 1 }, inc(s)` adds one to `s`, and `f(θ)` whose
body both writes a cell and computes from θ writes once and follows θ. A choice is an
application like any other, so a closure that branches on a cell follows the branch it
takes. Tests: "a lambda's body is part of the definition: given a cell, it writes it or
follows it", "what reads a cell follows it: a choice in a closure, a top-level ∇".

A live value made inside the prelude is named by the reader's call that reached it, and
runs again there: what it fails with later is reported on the reader's line. Test: "a live
value made inside a prelude body is named by the reader's call, and so is what it fails
with later".

**What a function is given.** A lambda gets its arguments as they are, cells included, and
its body reads a cell where it uses it. A native reads every cell it is given, except in
its *places*: an argument it takes as the cell itself. The places are few – the cell `←`
writes, the name `:` binds and the value it binds, `List`'s items, both of `identical`'s
values, `⟳`'s live count, the cell an optimizer's memory is for, and the slots of the
widgets (`Slider`'s cell, `Text`'s text, `Layers`' children). A list keeps the cells it
holds as cells, so a function on lists sees the structure.

**Three ways to name what a cell holds** (the Tour's ninth room):

- `old: s` is another name for the cell: it follows every write;
- `(s,)` is a list holding the cell: it follows too;
- `$s` is a new cell holding what `s` holds now: it moves only when something writes it.
  Making a cell reads what it is given.

`s()` is `s` applied to nothing – the same as `s`. Test: "a top-level s() follows its cell,
and the snapshot is $s, anywhere".

---

## 3. One write semantics

A write stores the value now – a live value is read, never followed; a list keeps the
cells it holds. To follow a value, define it: `s: ${ x × 2 }`. A write reads its place now,
as it reads its value: a live value that holds cells writes the cells it holds now
(Strachey's computed L-value) – after `which: ${ cond(guard(left, { a }), { b }) }`,
`which := 23` writes `a`, and makemore's `state ← saved` writes the cells its live list
holds. Tests: "a write stores a live value as it is now, and does not follow it", "a write
goes through a live value to the cell it picks now".

A statement whose value nothing reads, and that applies numbers or text – an old `n(5)`,
a `W([1, 1])` before the last line – does nothing at all: it is an Error that says so, and
names the write. Test: "a dropped statement that applies numbers or text is an Error that
names the write"; "no statement in shipped Fluent applies numbers or text and drops the
result".

A value read back from the GPU arrives later. `CodesToString` gives an `Arriving` cell, ""
until the codes land. A native applied to one in an occurrence gives another, computed now
with the placeholder and again on arrival. A write of one writes the placeholder now, and
the value when it arrives – the arrival is an occurrence of its own. This is how a dream
reaches `dream` in the Dreamers. A write made in a definition from a value still arriving
stores the placeholder. No demo does this.

---

## 4. The display

The program's value is live 0 (protocol.ts); every value it shows that is a cell or a live
value is a live of its own. After a write, a sample of each live whose value changed is
taken as soon as the page can show it: on the next display frame, or when the page has
drawn the last batch. Three rules pace it:

- **Reads pace updates.** A sample whose numbers or text need a GPU read is shown when the
  read lands; the page keeps the last one until then. A value that changes every frame –
  the Dreamers' live completion – shows a little late, never blank. Test: "text decoded on
  the GPU from a value that changes every frame still shows" (browser).
- **The page paces batches.** None while the page still draws the last one. Test: "a change
  made while the page draws goes when the page has drawn, not a frame later".
- **While steps wait, a look costs at most a twentieth of the worker.** While a ⟳ loop is
  behind its count – training, as fast as it can go – a look that took `s` ms makes that
  live's next one due `20·s` ms after this one was due, or at once if the live was idle. A
  look is the sample and its description: a live value is the graph it builds, and its
  numbers are computed where it is described. A cheap value is shown every frame, a costly
  one less often, and the steps keep the rest. With no steps waiting – a loop paced by
  `Time()`, an animation, a drag – every change is shown. Tests: "while steps wait, a look
  that costs a little over a twentieth of a frame is shown nearly every frame, not every
  other", "while steps wait, a look costs what describing it costs", "with no steps
  waiting, a look is shown every frame, whatever it costs".

A widget's slots are places, so a widget is made once and a dragged point is never rebuilt
under the pointer, while the picture under it follows. Test: "a point and a slider on a
live value write the cell it holds now, and a drag never rebuilds the point" (browser).

`Text(s)` takes its text as a live slot, like `Button`'s label, so a text still arriving
shows when it lands.

**Pictures are drawn in units**, a CSS pixel each. `Density()` is a source cell: how many
of the display's pixels make a unit – the page's devicePixelRatio, written at the start and
whenever it changes (another screen, a zoom), and 1 with no page, as in the tests.
`canvas`, `pixels`, `draw`, `bars`, `plot` and the IDE's heatmaps and bars read it: a
picture has the display's own pixels, and its edges are one of them wide. A picture defined
in the program reads it where the program is defined, so it follows it like any live value
and draws again at a new density; the page shows a picture with its pixels as the
display's.

The cost is that a picture's shape depends on the display: `shape(canvas(520, 160))` is
[320, 1040, 3] at density 2. Code that draws with shapes never notices; code that indexes
pixels sees Density()² as many. A drawing at density 2 is 4 times the pixels.

Tests: "a picture is drawn in units, with the display's pixels", "a drawing follows the
display's density, and the IDE paints heatmaps and bars at it"; in the browser, "a drawing
has the display's pixels, and follows a change of density".

What a frame of live values costs (Apple M1, headless Chrome, `engine/tests/bench-live.ts`,
the IDE's host a frame at a time): the drawing 5 kernels and 6.9 ms a frame, 2.3 ms of
them on the GPU; the Mandelbrot dragged a step a frame, about 72 kernels and 3.3 ms;
softmax of a live value, 5 kernels and 0.9 ms; the Dreamer training at about 200 steps a
second, 108 kernels a step.

---

## 5. Randomness

A draw is keyed by the reader's call that made it and by how many draws that call has
made in this run; each run starts the counts at 0. The same program draws the same numbers
on every run, and a draw elsewhere – a `${ ⚄(…) }` on the screen, sampled as often as it
is shown – moves no other draw. A live value that draws gets new numbers each time it
runs, like a spreadsheet's `RAND`.

Tests: "the same program trains the same weights on every run", "a draw is keyed by its
call: looking at another draw moves it not".

---

## 6. An edit keeps what training learned

An edit runs the program again, in a fresh scope. A top-level `~` is the cell of the run
before when it is bound under the same name, made by the same expression, to the same
shape. A top-level `$` that holds data – numbers, text, ◌ and lists of them – is kept the
same way, if something wrote it: a value nothing wrote is the program's, and the program
makes it again. An edit elsewhere keeps the trained weights and what the page set; an edit
of the cell's own expression, or of its shape, starts it afresh.

What an optimizer's init made for a cell stays with the cell (`TensorMemoryOf`), so a kept
variable keeps its moments and step count, and the next step continues the run.

Tests: "an edit keeps the last run's cells by name, expression and shape, and their
memory", "a top-level $ survives an edit only if something wrote it".

A reload is not an edit: the page starts a new worker. `Storage` keeps a checkpoint across
reloads (makemore).

A saved link carries the version of Fluent it is written in (`?v=`); a link written before
cells were their values is rewritten as it loads, by its syntax tree (`migrate.ts`): `s()`
is `s`, `s(v)` is `s← v`, and `~~` is `$`. Tests: "a link written in an older version comes
back migrated, and runs as that"; in the browser, "a link saved before ?v= is migrated as
it loads".

---

## 7. The core

`language.ts`, MARK: Cells – 88 code lines: `Signal`, `Computed`, the reader hook, `onWrite`
and the occurrence flag. MARK: Applying – the watch, `given` and the places. `preact` is
not in the worker; the page keeps it for React (`useSignals`).

- **One read hook serves following and ∇.** A read goes to whoever is reading: a watched
  application or a live value records the cell; ∇ substitutes the traced value of a cell it
  moves along and records it, and runs a live value made from one again inside, uncached.
- **Version stamps, no push edges.** A live value re-checks its sources at most once an
  instant and runs only if one changed; a run that gives the same value changes nothing
  downstream. A live value made inside a step holds no reference from its cells, so it is
  garbage when the step ends.
- **The host hears every write** (`onWrite`) and looks at the lives on the next frame.

---

## 8. What it gives by construction

| Property | Test |
|---|---|
| Looking never changes a value (a live value that draws excepted, §5) | "looking never changes a value" |
| A live value of a `~` follows it after the program has run | "a computed of a variable follows it after the program has run" |
| A write happens whether or not anything shows it | "a write happens whether or not anything shows it" |
| A live value cannot change state | "a computed cannot change state; it may change cells it made" |
| ∇ sees through a live value, every time | "∇ sees through a computed, every time" |
| A derived value is never seen half-updated | "a derived value is never seen half-updated, whatever reads it first" |
| The program runs once, even when it reads and writes a cell | "a program runs once, and what it shows of a variable follows it" |
| A cell's name is its value; a write is `←` or `:=` | "a cell is written with ← or :=, and a variable takes numbers only" |
| A dropped application of numbers or text is an Error | "a dropped statement that applies numbers or text is an Error that names the write" |
| The same occurrences give the same states | "the same occurrences give the same states" |
| An edit keeps the trained cells, and the written ones | §6 |

What it does not give:

- **Effect order.** There are no user effects; two occurrences in one frame (two ⟳ loops)
  run in the order the program made them. Reasoning.
- **A full checkpoint for free.** Every piece of training state is a cell, but the
  optimizer's are reached through its closure: `opt()` hands them out, and makemore saves
  them with the weights. Reasoning.
- **Undo.** Every write goes through `Signal.write`, so a write log is small, and tensors
  are immutable, so it holds references; it costs GPU memory. Not built.

---

## 9. Prior art

- **Fran** (Elliott & Hudak 1997), **push-pull FRP** (Elliott 2009): behaviors as functions
  of time, lifting, a stepper changes just after its occurrence; pull-evaluated live values.
  Lifting follows the meaning, so a choice over a behavior is lifted too.
- **Reflex**: `sample` and `hold` live in the build monad – making a cell is an effect and is
  never lifted, as here.
- **Solid, MobX**: every expression reacts on its own, tracked by observation, conditional
  reads included – as a definition's applications are watched here.
- **Elm, "A Farewell to FRP"** (2016): one state, an update, a view; signals were a
  stumbling block. Occurrences as thunks are enough.
- **Sodium, reactive-banana, S.js, Jane Street Incremental**: a held value changes after the
  transaction; observers see only stabilized states. Fluent's display samples between
  occurrences; inside one, reading order rules, as in **Céu**.
- **Adapton, self-adjusting computation, Svelte 5** (`state_unsafe_mutation`): the inner,
  memoized layer may not write. That is §1's rule for live values.
- **Spreadsheets**: formulas can't write cells; `RAND` is volatile (§5); a parameter, not
  the call, decides whether it takes a reference (ROW) or a value (SUM) – Fluent's places.
  **Observable**: `mutable` and `viewof` are places at the use; here the function declares
  them. **Pluto.jl**: untracked mutation goes stale.
- **Strachey, Algol 68**: an L-value by position; a computed L-value, `a[i] := v`, as a write
  through a live value.
- **TensorFlow 2's `GradientTape`** watches the trainable variables read inside it, as
  `∇(loss)()` does; **Flax NNX** filters `Param` from other variable types, as `~` from
  `$`; **PyTorch** forbids an in-place write to a leaf under autograd, as ∇ forbids writes.

---

## 10. Open

- **The engine's schedule on a fused picture.** A live value's graph reaches the engine
  whole, so its fusion decides what is stored. The drawing's frame takes 2.3 ms of GPU
  kernels where coarser live values, each settled, took 2.1: the blend that writes three
  channels computes the strokes' coverage, square root and exponential included, once a
  channel. The cost model counts every op as one.
- **A spaced write** – `c ← c + 1` – is `(c ← c) + 1`, as any spaced chain is: it writes c
  to itself and adds 1 to nothing. A dropped `◌ + 1` fails a step's block, so a step shows
  it; at a program's top level it is a warning.
- **A failed effect in a definition** – a write or an optimizer step that fails before it
  writes – read cells and changed none, so it is a live value holding its Error, and runs
  again when those cells change.
- **Two optimizers with the same init on one cell** share its memory (§6).
