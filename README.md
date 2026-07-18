# Fluent

**Learnable math.** A small language where math reads the way you read — left to right, no tables to memorize. Learn the whole thing in five minutes, right in a browser tab.

**[Take the tour](https://mlajtos.github.io/fluent/?code=VG91cg)** — fifteen interactive rooms, from `1 + 2` to a value that finds √42 on its own. Or just **[open the playground](https://mlajtos.github.io/fluent/)** and type.

Fluent is the working end of *[New Kind of Paper](https://mlajtos.mu/posts/new-kind-of-paper)*, an essay series about what math could feel like if pen, paper, and calculator were one instrument ([part two](https://mlajtos.mu/posts/new-kind-of-paper-2), [three](https://mlajtos.mu/posts/new-kind-of-paper-3), [four](https://mlajtos.mu/posts/new-kind-of-paper-4)).

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

Live evaluation on every keystroke, hover docs, unicode completion (type `alpha`, get `α`), syntax trees for quoted code, camera and microphone as tensor sources, and LLM code generation — write `;;a bouncing ball;;` and it appears (bring your own Anthropic API key, set via the command palette). Hover any built-in for its doc card, or open the built-in [Documentation](https://mlajtos.github.io/fluent/?code=RG9jdW1lbnRhdGlvbg) for the reference.

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
| [`tour.ts`](tour.ts) | the tour — fifteen rooms, written in Fluent itself |
| [`tests.ts`](tests.ts) | language tests (`bun test ./tests.ts`) |
| [`tests.browser.ts`](tests.browser.ts) | IDE tests in real Chromium (`bun run test:browser`) |
