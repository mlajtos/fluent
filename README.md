# Fluent

**Learnable math.** A small language where math reads the way you read — left to right, no tables to memorize. Learn the whole thing in five minutes, right in a browser tab.

**[Open the playground](https://mlajtos.github.io/fluent/)** and follow along. Every box below is live — hit **run** to open it, then poke at it.

<!-- code fences say "clojure" because GitHub already has a language named
     Fluent (Mozilla's FTL) and paints ours as errors; clojure is the nearest
     look-alike (; comments, brackets, numbers) -->

## Take the tour

**Fluent runs left to right, the way you read.** No precedence tables, no acronyms. Want a piece to go first? Glue it — hug the parts together with no space. Or, you know. Parens.

```clojure
(
    1 + 2 * 3,     ; 9 — spaced, straight left to right
    1 + 2*3,       ; 7 — glue the * and it goes first
    1 + (2 * 3),   ; 7 — or, you know. parens
)
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=KAogICAgMSArIDIgKiAzLCAgICAgOyA5IOKAlCBzcGFjZWQsIHN0cmFpZ2h0IGxlZnQgdG8gcmlnaHQKICAgIDEgKyAyKjMsICAgICAgIDsgNyDigJQgZ2x1ZSB0aGUgKiBhbmQgaXQgZ29lcyBmaXJzdAogICAgMSArICgyICogMyksICAgOyA3IOKAlCBvciwgeW91IGtub3cuIHBhcmVucwop)

**Not sure how a line will group?** Drop it in backticks and Fluent draws it as a tree — you see exactly how it flows. Hover the nodes; the shape you see *is* the order it runs.

```clojure
`1 + 2*3`
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=YDEgKyAyKjNg)

**A list of numbers goes in square brackets — and acts like one number.** Do the math to the whole list at once; reach inside with a spaced `_`, where `_ 0` is the 0th item (Fluent counts from zero).

```clojure
(
    [1, 2, 3] + 10,     ; [11, 12, 13] — the 10 lands on each
    [10, 20, 30] _ 0,   ; 10 — the 0th item
    [10, 20, 30] _ 2,   ; 30 — and the 2nd
)
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=KAogICAgWzEsIDIsIDNdICsgMTAsICAgICA7IFsxMSwgMTIsIDEzXSDigJQgdGhlIDEwIGxhbmRzIG9uIGVhY2gKICAgIFsxMCwgMjAsIDMwXSBfIDAsICAgOyAxMCDigJQgdGhlIDB0aCBpdGVtCiAgICBbMTAsIDIwLCAzMF0gXyAyLCAgIDsgMzAg4oCUIGFuZCB0aGUgMm5kCik)

**Need a run of numbers?** `1 ... 5` counts from 1 through 5.

```clojure
1 ... 5    ; [1, 2, 3, 4, 5]
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=MSAuLi4gNSAgICA7IFsxLCAyLCAzLCA0LCA1XQ)

**Name anything with a `:`.** Then reuse the name as often as you like.

```clojure
(
    v: [1, 2, 3],
    v + v          ; [2, 4, 6] — name once, use twice
)
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=KAogICAgdjogWzEsIDIsIDNdLAogICAgdiArIHYgICAgICAgICAgOyBbMiwgNCwgNl0g4oCUIG5hbWUgb25jZSwgdXNlIHR3aWNlCik)

**Every built-in has three names** — a long one to discover, a word for habit, a glyph for speed. `TensorSum` is `sum` is `Σ`: all the same function. **Hover any name and its doc card pops up, right in the editor** — all three open the same card. Spell it out while you're learning; shrink it to the glyph once it's muscle memory.

```clojure
(
    TensorSum([1, 2, 3]),   ; 6
    sum([1, 2, 3]),         ; 6 — same function, shorter name
    Σ(1...3),               ; 6 — same function, just a glyph
)
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=KAogICAgVGVuc29yU3VtKFsxLCAyLCAzXSksICAgOyA2CiAgICBzdW0oWzEsIDIsIDNdKSwgICAgICAgICA7IDYg4oCUIHNhbWUgZnVuY3Rpb24sIHNob3J0ZXIgbmFtZQogICAgzqMoMS4uLjMpLCAgICAgICAgICAgICAgIDsgNiDigJQgc2FtZSBmdW5jdGlvbiwganVzdCBhIGdseXBoCik) · *([the full list of names](#the-names-at-a-glance))*

**Operators aren't a special club.** `+` is really just a function — so you can call it by name, or name your own. Even an emoji works.

```clojure
(
    1 + 2,        ; 3
    add(1, 2),    ; 3 — + is really a function, called add
    +(1, 2),      ; 3 — so you can call it like one
    1 add 2,      ; 3 — or put its name in the middle
    🐱: add,      ; name your own operator...
    2 🐱 3,       ; 5 — ...even an emoji
)
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=KAogICAgMSArIDIsICAgICAgICA7IDMKICAgIGFkZCgxLCAyKSwgICAgOyAzIOKAlCArIGlzIHJlYWxseSBhIGZ1bmN0aW9uLCBjYWxsZWQgYWRkCiAgICArKDEsIDIpLCAgICAgIDsgMyDigJQgc28geW91IGNhbiBjYWxsIGl0IGxpa2Ugb25lCiAgICAxIGFkZCAyLCAgICAgIDsgMyDigJQgb3IgcHV0IGl0cyBuYW1lIGluIHRoZSBtaWRkbGUKICAgIPCfkLE6IGFkZCwgICAgICA7IG5hbWUgeW91ciBvd24gb3BlcmF0b3IuLi4KICAgIDIg8J-QsSAzLCAgICAgICA7IDUg4oCUIC4uLmV2ZW4gYW4gZW1vamkKKQ)

**Want it to move?** Wrap a value in `$( )` and it comes alive — everything that touches it recomputes when it changes. No render loop, no state to wire up. Drag the slider.

```clojure
(
    x: $(0.5),
    Slider(x),
    0 ... 100*x
)
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=KAogICAgeDogJCgwLjUpLAogICAgU2xpZGVyKHgpLAogICAgMCAuLi4gMTAwKngKKQ)

**And there's no `print`.** Every value just shows itself — a row of numbers becomes a plot, a grid becomes an image, quoted code becomes that tree from before.

```clojure
[0, 1, 4, 9, 16, 25, 16, 9, 4, 1, 0]
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=WzAsIDEsIDQsIDksIDE2LCAyNSwgMTYsIDksIDQsIDEsIDBd)

That's the whole idea. Here's what people build with it.

## Example gallery

| | demo | |
|---|---|---|
| 🌀 | **Mandelbrot** — the Mandelbrot set over the whole plane, sharper as you drag the depth | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=mandelbrot) |
| 🦠 | **Game of Life** — Conway's Game of Life on a large grid | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=game-of-life) |
| 🔮 | **The Name Dreamer** — a small transformer that invents new names from a word list | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=dreamer) |
| 🐦 | **Combinators** — combine functions without naming their arguments | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=combinators) |
| 📷 | **Live edge detection** — your webcam, edge-detected in real time | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=camera-edges) |
| 🎵 | **Live spectrum** — your microphone's frequency spectrum, live | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=spectrum) |
| 🎸 | **Pitch detector** — the pitch of a note you hum | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=pitch-detector) |
| 📈 | **Linear regression** — a line fit to points by gradient descent | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=linear-regression) |
| 🏔 | **Touch the loss landscape** — a ball you roll downhill on a loss surface | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=loss-landscape) |
| ⚔️ | **adam vs sgd** — two optimizers descending the same surface from the same start | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=optimizer-race) |
| 🌸 | **Reaction–diffusion** — two reacting chemicals that settle into spots and stripes | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=reaction-diffusion) |
| 🔬 | **Lenia** — smooth, continuous cellular automata | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=lenia) |
| 🧲 | **Spinning magnets** — an animated vector field | [▶&nbsp;run](https://mlajtos.github.io/fluent/?example=magnets-minimal) |

More in the playground: press <kbd>Ctrl</kbd>+<kbd>O</kbd> for the full gallery.

## Where the tour goes next

The tour stops at the basics; the language keeps going. `∇` differentiates any function you built from smooth numeric ops — reshape and indexing included — and nests, so `∇(∇(f))` is the second derivative. `~` makes a trainable variable and an optimizer (`sgd`, `adam`, `adamw`, `adagrad`) minimizes a loss; `⟳` runs the loop between frames so the UI stays live. That's the whole of the linear-regression and Name Dreamer demos above.

```clojure
θ: ~([0, 0]),
𝓛: { Σ((θ - [0.23, 0.47])^2) },
opt: sgd(0.1),
{ opt(𝓛) } ⟳ 100,
θ
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=zrg6IH4oWzAsIDBdKSwK8J2TmzogeyDOoygozrggLSBbMC4yMywgMC40N10pXjIpIH0sCm9wdDogc2dkKDAuMSksCnsgb3B0KPCdk5spIH0g4p-zIDEwMCwKzrg)

Then hover any built-in for its docs, or [**open the built-in Documentation**](https://mlajtos.github.io/fluent/?code=RG9jdW1lbnRhdGlvbg) for the full tour.

<details id="the-names-at-a-glance">
<summary>the names, at a glance</summary>

| glyph | word | full name |
|---|---|---|
| `∇` | `grad` | `TensorGradient` |
| `Σ` | `sum` | `TensorSum` |
| `Π` | `prod` | `TensorProduct` |
| `μ` | `mean` | `TensorMean` |
| `⌈` | `ceil` | `TensorCeil` |
| `⌊` | `floor` | `TensorFloor` |
| — | `max` | `TensorMax` |
| — | `min` | `TensorMin` |
| `#` | `length` | `TensorLength` |
| `_` | `gather` | `TensorGather` |
| `⍴` | `reshape` | `TensorReshape` |
| `..<` | `range` | `TensorRange` |
| `...` | — | `TensorRangeInclusive` |
| `⊗` | `outer` | `TensorOuter` |
| `^` | `pow` | `TensorPower` |
| `√` | `root` | `TensorRoot` |
| `%` | `mod` | `TensorRemainder` |
| `÷` | `div` | `TensorDivide` |
| `×` | `mul` | `TensorMultiply` |
| `~` | `var` | `TensorVariable` |
| `:=` | — | `TensorAssign` |
| `⟳` | `iter` | `FunctionIterate` |
| `⍣` | — | `FunctionPower` |
| `.` | `apply` | `FunctionApply` |
| `@` | `eval` | `FunctionEvaluate` |
| `$` | — | `Reactive` |
| — | `watch` | `TensorWatch` |
| — | `conv` | `TensorConvolution` |
| — | `once` | `SignalOnce` |

Some cells are still empty — the language is young, and names are earned.

</details>

## IDE

Live evaluation on every keystroke, hover docs, unicode completion (type `alpha`, get `α`), syntax trees for quoted code, camera and microphone as tensor sources, and LLM code generation — write `;;a bouncing ball;;` and it appears (bring your own Anthropic API key, set via the command palette).

<kbd>Ctrl</kbd>+<kbd>O</kbd> examples · <kbd>Ctrl</kbd>+<kbd>S</kbd> share as URL · <kbd>Ctrl</kbd>+<kbd>P</kbd> commands · <kbd>Ctrl</kbd>+<kbd>Space</kbd> complete — Safari reserves ⌘O, use <kbd>⇧</kbd><kbd>⌘</kbd><kbd>O</kbd> there

## Run locally

```sh
git clone https://github.com/mlajtos/fluent.git
cd fluent && bun install
bun dev   # → http://localhost:3000
```

The whole thing fits in a handful of files — read it, change it:

| file | what |
|---|---|
| [`language.ts`](language.ts) | the language — grammar ([Ohm](https://ohmjs.org)), evaluator, prelude; tensors via [jax-js](https://github.com/ekzhang/jax-js), reactivity via [preact signals](https://preactjs.com/guide/v10/signals/) |
| [`client.tsx`](client.tsx) | the IDE — components, visualizers, Monaco editor, playground |
| [`tests.ts`](tests.ts) | language tests (`bun test ./tests.ts`) |
| [`tests.browser.ts`](tests.browser.ts) | IDE tests in real Chromium (`bun run test:browser`) |
</content>
