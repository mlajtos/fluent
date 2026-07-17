// The Tour: a challenge-checked walkthrough of the language, written IN the
// language. Evaluating the symbol `Tour` in the playground yields this whole
// interactive value – prose, live editable cells, checked challenges,
// navigation. client.tsx binds it as a lazy environment entry, so each
// evaluation builds a fresh instance and an unused Tour costs nothing.
//
// The arc: rooms 1–9 are the everyday language (reading order → lists →
// names → functions → reactivity), rooms 10–14 are one continuous trick
// (slope → feel the bottom → walk there by hand → let it run), room 15 is
// the door to the gallery.
//
// Authoring rules the grammar imposes on this string:
// – Fluent strings run to the next `"` and have no escapes, so the prose
//   uses typographic quotes (“ ”) and never a straight double quote – and a
//   CELL can never contain one either (button labels live at tour level).
// – Backticks appear inside cell code (quoted ASTs); in this template
//   literal they must be written as \` – a regular template literal keeps
//   them as plain backticks (String.raw would keep the backslash too).
// – A challenge's check reads the cell's SOURCE as well as its value (via
//   StringToCodes), so a challenge about naming can insist on names instead
//   of accepting a typed-in answer. Keep checked characters out of the
//   cell's seed comments, or the seed itself satisfies the check.
export const TOUR_SOURCE = `
; ————————————————————————————————————————————————————————————————
; The Fluent tour – fifteen rooms, from 1 + 2 to a value that learns.
; ————————————————————————————————————————————————————————————————

step: $(0),

; The challenge light: check is a thunk re-run whenever anything it reads
; changes; a throwing or erroring check simply counts as “not yet”.
status: { check, done, hint |
    $({ cond(guard(cond(check, { 0 }), { done }), { hint }) })
},

; A checked cell: its own editor, its own scope, a live result, a light.
; The check lambda receives (value, source) – value equality alone is an
; honor system, so most challenges also read the source. Returns a flat
; (editor, result, light) list: Grid flattens it into its own cells, and a
; nested Grid would collapse in an auto-sized row.
challenge: { src, check, done, hint |
    code: $(src),
    result: CodeEvaluate(code),
    (CodeEditor(code, "auto"), result, status({ check(result(), code()) }, done, hint))
},

; An unchecked cell – editor and live result only.
demo: { src |
    code: $(src),
    (CodeEditor(code, "auto"), CodeEvaluate(code))
},

nav: Grid([1, 2, 1])(
    Button("◂ back", { step(0 ⌈ (step() - 1)) }),
    Text(""),
    Button("next ▸", { step((step() + 1) ⌊ 14) })
),
nav-final: Grid([1, 2, 1])(
    Button("◂ back", { step(0 ⌈ (step() - 1)) }),
    Text(""),
    Button("start over ↺", { step(0) })
),

; character codes the checks look for:
; ( 40   * 42   + 43   . 46   : 58   _ 95   { 123   × 215   ∇ 8711   🍌 127820

; ———————————————————— 1 · reading order ———————————————————————————

s1: Grid(1)(
    Text("● ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ · 1 / 15

### Reading order is meaning

**Fluent runs left to right, the way you read.** No precedence tables, nothing to memorize: in \`1 + 2 * 3\` the \`1\` and \`2\` meet first, so it says 9. Want a piece to go first? **Glue it** — hug the parts together with no space around the operator. (Parens work too. But where is the fun in that.)

*This tour is interactive: every code box is a real editor. Change things and see what happens.*"),
    challenge("1 + 2 * 3   ; ← make this say 7",
        { r, s | c: StringToCodes(s),
            (min(r = 7)) ∧ ((Σ(c = 40)) = 0) ∧ ((Σ(c = 43)) ≥ 1) ∧ (((Σ(c = 42)) + (Σ(c = 215))) ≥ 1) },
        Text("✅ **Seven.** You just changed what a program means with a *space*. Let that sink in."),
        Text("🎯 *Make the box say **7** — keep the \`+\` and the \`*\`, no parentheses. The check reads your code.*")
    ),
    nav
),

; ———————————————————— 2 · see the shape ———————————————————————————

s2: Grid(1)(
    Text("● ● ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ · 2 / 15

### When in doubt, draw it

**Not sure how a line will group?** Put it in backticks and Fluent stops running it and *draws* it instead. The tree you see is not a diagram **of** the program — it **is** the program. Hover the nodes — the source they came from lights up in the box. Re-space the code. Watch the shape follow your spaces."),
    demo("\`1 + 2*3\`   ; ← put spaces around the * and watch the tree"),
    Text("✅ **Nothing to solve here.** Just look. The shape you see is the order it runs — that sentence is the whole language."),
    nav
),

; —————————————————— 3 · lists act like one number ————————————————

s3: Grid(1)(
    Text("● ● ● ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ · 3 / 15

### A list acts like one number

**One number at a time is no way to live.** Square brackets make a list — and a list does math as one thing, no loops: \`[1, 2, 3] + 10\` lands the 10 on *each*. To reach inside, use a spaced \`_\` — item 0 comes first."),
    demo("[1, 2, 3] + 10   ; the 10 lands on each"),
    challenge("[10, 20, 30] _ 0   ; ← fish out the 30",
        { r, s | c: StringToCodes(s), (min(r = 30)) ∧ ((Σ(c = 95)) ≥ 1) },
        Text("✅ **30.** Third item, index 2. You will never be off by one again. (You will.)"),
        Text("🎯 *Make the lower box say **30** — with \`_\`, not by typing 30. Counting starts at 0.*")
    ),
    nav
),

; ———————————————————— 4 · runs of numbers ————————————————————————

s4: Grid(1)(
    Text("● ● ● ● ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ · 4 / 15

### Runs of numbers

**Writing long lists by hand is beneath you: \`1 ... 5\` counts from 1 through 5.** A run is a list like any other, so everything from the last room applies to it. Mind the spaces around \`...\` — you know why by now."),
    challenge("1 ... 5   ; ← turn this into [2, 4, 6, 8, 10]",
        { r, s | c: StringToCodes(s), (min(r = [2, 4, 6, 8, 10])) ∧ ((Σ(c = 46)) ≥ 3) },
        Text("✅ **The evens.** A run, times two — you composed two ideas and no loop appeared. None ever will."),
        Text("🎯 *Make it say **[2, 4, 6, 8, 10]** — grow it from a range, don’t type the list.*")
    ),
    nav
),

; ———————————————————— 5 · naming things ——————————————————————————

s5: Grid(1)(
    Text("● ● ● ● ● ○ ○ ○ ○ ○ ○ ○ ○ ○ ○ · 5 / 15

### Name anything with \`:\`

**Anything worth keeping deserves a name.** A \`:\` gives one to anything, and then you spend the name instead of retyping the value. Words work, kebab-case works, Greek works — type \`theta\`, press **Tab**, get \`θ\`. Even \`🍌\` works."),
    challenge("🍌: 7,
🍌 + 🍌   ; ← a waste of good bananas. make 49.",
        { r, s | c: StringToCodes(s), (min(r = 49)) ∧ ((Σ(c = 127820)) ≥ 3) },
        Text("✅ **49.** One name, three bananas, not a digit wasted. Naming things is compression."),
        Text("🎯 *Make the box say **49** — out of bananas, not out of numbers.*")
    ),
    nav
),

; —————————————— 6 · three names for everything ————————————————————

s6: Grid(1)(
    Text("● ● ● ● ● ● ○ ○ ○ ○ ○ ○ ○ ○ ○ · 6 / 15

### Every built-in has three names

**Your names sit next to the ones the language shipped with — and each built-in has three.** Long to discover, word for habit, glyph for speed: \`TensorSum\` is \`sum\` is \`Σ\`, one function in three sizes. **Hover any of them below** — the doc card pops up right here. (Type \`sigma\` + **Tab** for \`Σ\`.)"),
    demo("(
    TensorSum([1, 2, 3]),   ; a long name to discover…
    sum([1, 2, 3]),         ; …a word for habit…
    Σ(1 ... 3),             ; …a glyph for speed. Same function.
)"),
    challenge("sum([1, 2, 3])   ; ← now sum 1 through 100. all of them.",
        { r, s | c: StringToCodes(s), (min(r = 5050)) ∧ ((Σ(c = 40)) ≥ 1) },
        Text("✅ **5050.** Gauss needed a clever trick. You needed eleven keystrokes. History will decide whose was cooler."),
        Text("🎯 *Make the lower box say **5050** — the sum of every whole number from 1 to 100, summed, not typed.*")
    ),
    nav
),

; ———————————————— 7 · operators are functions ——————————————————————

s7: Grid(1)(
    Text("● ● ● ● ● ● ● ○ ○ ○ ○ ○ ○ ○ ○ · 7 / 15

### Operators aren’t a special club

**A secret about the names you’ve been using: \`+\` is one of them.** An operator is just a function with a short name — call it like one, or put any function’s name in the middle of two values. And that works for *your* names too."),
    demo("(
    1 + 2,        ; an operator…
    add(1, 2),    ; …is a function with a short name…
    +(1, 2),      ; …callable by its symbol, too…
    1 add 2,      ; …and any function can sit in the middle
)"),
    challenge("plus: add,
plus(40, 2)   ; ← now put plus in the MIDDLE",
        { r, s | c: StringToCodes(s),
            (min(r = 42)) ∧ ((Σ(c = 58)) ≥ 1) ∧ ((Σ(c = 40)) = 0) ∧ ((Σ(c = 43)) = 0) },
        Text("✅ **42.** You taught the language a word. Next room: teach it a whole function."),
        Text("🎯 *Make it say **42** with \`plus\` *between* the numbers — no parentheses, no \`+\`.*")
    ),
    nav
),

; ———————————————— 8 · make your own function ——————————————————————

s8: Grid(1)(
    Text("● ● ● ● ● ● ● ● ○ ○ ○ ○ ○ ○ ○ · 8 / 15

### Make your own function

**Renaming built-ins is nice. Minting your own is better.** Braces make a function: \`{ x | x^2 }\` reads *take an \`x\`, hand back \`x^2\`*. Name it, call it — and lists flow through it the way they flow through \`+\`: one call, every item."),
    demo("square: { x | x^2 },
square([1, 2, 3, 4])   ; every item, one call"),
    challenge("cube: { x | x^2 },   ; ← almost. a cube is x·x·x.
cube(3)",
        { r, s | c: StringToCodes(s), (min(r = 27)) ∧ ((Σ(c = 123)) ≥ 1) },
        Text("✅ **27.** You fixed a function. From here on, everything in the language is either a value or one of these."),
        Text("🎯 *\`cube\` doesn’t cube yet — one character stands between it and **27**.*")
    ),
    nav
),

; ———————————————————— 9 · make it move ————————————————————————————

x: $(0.2),
s9: Grid(1)(
    Text("● ● ● ● ● ● ● ● ● ○ ○ ○ ○ ○ ○ · 9 / 15

### Make it move

**Everything so far ran once and sat still. Wrap a value in \`$( )\` and it comes alive.** Here \`x: $(0.2)\` made \`x\` a live value. The box holds one *writer* — the slider — and two *readers*: a percentage, and a whole wave. Drag — every reader follows, nobody re-runs anything."),
    demo("(
    Slider(x),                 ; ← writes x. drag it.
    x * 100,                   ; reads x
    sin((0 ... 79) * x ÷ 2)    ; reads x, draws a wave
)"),
    status({ x() > 0.9 },
        Text("✅ **Alive.** You changed a running program mid-flight and it didn’t even flinch. This is the trick behind every demo in the gallery."),
        Text("🎯 *No typing this time — **drag the slider** and pin \`x\` above 0.9.*")
    ),
    nav
),

; ———————————————————— 10 · the slope machine ——————————————————————

s10: Grid(1)(
    Text("● ● ● ● ● ● ● ● ● ● ○ ○ ○ ○ ○ · 10 / 15

### The slope machine

*That was the everyday language — all of it. The rest of the tour is a single trick: teaching a value to find its own way to a goal. The trick starts with a slope.*

**\`grad\` is a function that reads a function.** Hand it \`f\` and it hands back the function of \`f\`’s *slope* — not an estimate, not a table lookup: it reads the code and builds the answer. Name what it hands you — \`df\` below — and the slope is an ordinary function: the slope of \`x^2\` is \`2·x\`, so \`df(3)\` says 6. (Like every built-in, \`grad\` has a glyph: \`∇\`, typed \`nabla\` + **Tab**.)"),
    challenge("f: { x | x^2 },
df: grad(f),
df(3)   ; ← now: the slope of x·x·x, at 2",
        { r, s | c: StringToCodes(s),
            (min(r = 12)) ∧ ((Σ(c = 123)) ≥ 1) ∧ (((Σ(c = 8711)) + (Σ(c = 103))) ≥ 1) },
        Text("✅ **12.** You differentiated a program by editing a program. Some people sit through a whole semester before they believe this is allowed."),
        Text("🎯 *Make the box say **12** — the slope of \`x^3\` at 2, computed by \`grad\`, not by you.*")
    ),
    nav
),

; ———————————————————— 11 · the slope, drawn ———————————————————————

s11: Grid(1)(
    Text("● ● ● ● ● ● ● ● ● ● ● ○ ○ ○ ○ · 11 / 15

### The slope, drawn

**Slopes come whole, not point by point.** Below: a curve, and its slope, made by handing \`grad(f)\` the same run of numbers. Where the curve falls, the slope is below zero; where the curve bottoms out, the slope **crosses zero**. Keep that crossing in mind — the next three rooms stand on it."),
    demo("f: { x | x^2 ÷ 10 },
xs: -10 ... 10,
(f(xs), grad(f)(xs))   ; ← try wrapping f in grad( ) once more"),
    Text("✅ **Nothing to solve.** A parabola’s slope is a straight line — you may have been told that once. Now you can just look at it."),
    nav
),

; ———————————————————— 12 · find the bottom by feel ————————————————

far: { g | (g - 42)^2 },
dfar: grad(far),
dial: $(10),
s12: Grid(1)(
    Text("● ● ● ● ● ● ● ● ● ● ● ● ○ ○ ○ · 12 / 15

### Find the bottom by feel

**Put the slope to work.** \`far\` scores a guess by its distance from 42, and \`dfar: grad(far)\` is its slope function — named just like \`df\` last room. Below: the score, the slope, and the guess itself, **live and draggable**. Slide it. Negative slope: too low. Positive: too high. **Zero: you have arrived.**"),
    demo("(far(dial), dfar(dial), Scrubber(dial))   ; ← score · slope · drag me"),
    status({ (abs(dial() - 42)) < 0.5 },
        Text("✅ **Found it.** Score 0, slope 0, guess 42. You just solved an equation by feel — your hand did what the slope said."),
        Text("🎯 *No typing — **drag the rightmost number** until the slope reads **0**.*")
    ),
    nav
),

; ———————————————————— 13 · walk downhill ——————————————————————————

guess: $(10),
hist: $([10]),
; a cell can never hold a double quote (Fluent strings have no escapes),
; so the button labels are named out here – the names ARE the labels
step-downhill: "step downhill",
back-to-10: "back to 10",
s13: Grid(1)(
    Text("● ● ● ● ● ● ● ● ● ● ● ● ● ○ ○ · 13 / 15

### Walk downhill

**You found the bottom by feel. Now walk there deliberately.** Each press of the first button is one honest step: read the slope where you stand, scale it down, move the *other* way, remember the footprint. Watch the path: long strides far out, baby steps near the bottom — the slope itself shrinks as you close in."),
    demo("(
    Button(step-downhill, {
        here: guess(),                 ; where the guess stands
        there: here - dfar(here)*0.3,  ; a small step AGAINST the slope
        guess(there),                  ; move
        hist(hist() concat [there])    ; remember the footprint
    }),
    Button(back-to-10, { guess(10), hist([10]) })
)"),
    hist,
    status({ (abs(guess() - 42)) < 1 },
        Text("✅ **Arrived.** Look at the path: the step size *is* the slope, so the walk brakes by itself. That graph is gradient descent, drawn by you pressing the button."),
        Text("🎯 *No typing — press **step downhill** until the path lands within 1 of 42. (Overshot? Edit the 0.3 and see why.)*")
    ),
    nav
),

; ———————————————————— 14 · let it run —————————————————————————————

answer: ~(0.1),
answer-live: watch(answer),
trail: $([0.1]),
; a line plot, not the annotated bar chart – the trail grows by one point
; per FRAME, and re-annotating hundreds of bars each frame would throttle
; the very training loop the room wants you to watch
trail-plot: PointPlot(trail),
s14: Grid(1)(
    Text("● ● ● ● ● ● ● ● ● ● ● ● ● ● ○ · 14 / 15

### Let it run

**Now hand the button to the machine.** The question: *what number, squared, is 42?* The \`~\` marks \`answer\` as **trainable** — it starts at 0.1, knowing nothing. \`score\` is written exactly like \`far\` was, \`sgd\` presses the step button for you, and \`iter\` presses *that* two hundred times, between frames. Watch the line climb."),
    demo("score: { (answer*answer - 42)^2 },   ; how far answer·answer misses 42
opt: sgd(0.0005),
{ opt(score), trail(trail() concat [answer]) } iter 200,
watch(answer)"),
    trail-plot,
    status({ (abs((answer-live() * answer-live()) - 42)) < 0.1 },
        Text("✅ **6.4807….** That is the square root of 42, and nobody typed it — \`answer\` walked there, pressed downhill by its own slope, two hundred times. This is machine learning entire; the rest is scale."),
        Text("🎯 *Nothing to press — watch until \`answer · answer\` sits within 0.1 of 42. (Impatient? Raise the 0.0005.)*")
    ),
    nav
),

; ———————————————————— 15 · it keeps going —————————————————————————

s15: Grid(1)(
    Text("● ● ● ● ● ● ● ● ● ● ● ● ● ● ● · 15 / 15

### It keeps going

**That was the tour** — numbers, lists, names, functions; then a slope, a bottom found by hand, a walk, and a value that found √42 on its own.

**Now open the gallery: press Ctrl+O** — or walk straight in: [a Mandelbrot you can sharpen](?example=mandelbrot), [Conway’s Game of Life](?example=game-of-life), [your webcam, edge-detected live](?example=camera-edges), [two optimizers racing down one loss surface](?example=optimizer-race), or [the Name Dreamer](?example=dreamer) — a small transformer, trained right in this tab, dreaming up names that don’t exist. Each demo is a page or two of the language you just learned, in an editor like the boxes here.

Want the reference instead? Evaluate \`Documentation\`. And the box below is yours:"),
    demo("1 + 1   ; ← your turn"),
    Text("✅ **Fin.** It’s your language now. ∎"),
    nav-final
),

; ———————————————————————————— the tour ————————————————————————————

rooms: (s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15),
$({ ListGet(rooms, step()) })
`
