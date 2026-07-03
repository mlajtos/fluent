# Fluent Killer Demos

What makes a demo a *Fluent* demo? Fluent sits on an intersection nobody else occupies:

**APL-dense tensor notation × automatic differentiation × reactive UI × zero-install GPU in a browser tab.**

JAX has the math but no liveness. Observable has liveness but no autodiff. Desmos has liveness but no programmable tensors. Shadertoy has GPU but no gradients. A killer Fluent demo is one that is *impossible to build in an afternoon anywhere else* — and it has one structural superpower the others lack: **the demo link carries its own source** (`?code=` base64 URL). Every demo is simultaneously a toy, a tweet, and an editable program.

Criteria for the list below:

1. **Fits in a screen** of code — the source is part of the show.
2. **Moves on its own** — something animates or trains without user input.
3. **Invites touch** — a slider, a button, a draggable thing; touching it teaches something.
4. **Uses the gradient** — ∇ is the soul of the language; the best demos make differentiation *felt*.

---

## ⭐ 1. Touch the Loss Landscape

*The single most recognizable image in ML pedagogy — the ball rolling down the loss surface — as a live, editable program.*

A 2D loss surface rendered as a GPU heatmap. A parameter vector `θ` descending it. A slider for learning rate. A button to step. And the loss function itself sitting in the editor — **edit the math while the ball is rolling**. Change the valley, watch the ball adapt. No tool on the internet does this.

```fluent
; Touch the Loss Landscape
lr: $(0.5),

; the surface – banana valley (evaluate loss over a grid)
n: 200,
xs: (linspace([-2, 2], n) ⍴ [1, n]) tile [n, 1],
ys: (linspace([-1, 3], n) ⍴ [n, 1]) tile [1, n],
surface: log((1 - xs)^2 + ((ys - xs^2)^2 × 5) + 0.05),

; the ball
θ: ~([-1.5, 2.5]),
𝓛: { (1 - θ_0)^2 + ((θ_1 - θ_0^2)^2 × 5) },

; sgd is stateless – safe to build fresh with the live slider value
step: { sgd(once(lr) × 0.05)(𝓛) },

(
  Text("# Touch the Loss Landscape"),
  Slider(lr),
  Button("Step ×100", { step ⟳ 100 }),
  surface,
  θ, 𝓛(),
)
```

**Runnable today** as heatmap + θ readout. Becomes *unforgettable* with two primitives (see wishlist): a draggable point (grab θ, drop it on a ridge, watch it roll) and a plot-overlay layer (the optimizer's trail drawn on the surface).

Variants once it works: side-by-side `sgd` vs `adam` racing on the same surface; a "momentum" slider; saddle points; a loss with a local minimum trap.

---

## 2. Fit My Whistle

*Differentiable programming meets your voice. Conference-demo magic.*

`MicrophoneSpectrum()` is already a reactive tensor. Define a tiny parametric model (two Gaussian peaks over frequency), and let gradient descent chase your whistle in real time — the fitted curve overlaid on the live spectrum, `μ` locking onto the pitch as you slide it.

```fluent
; sketch – train one step per mic frame
spec: MicrophoneSpectrum(256),
bins: linspace([0, 1], 256),

μ: ~([0.2, 0.5]), h: ~([0.5, 0.5]),
fit: { h_0 × exp(-((bins - μ_0)^2 / 0.003)) + (h_1 × exp(-((bins - μ_1)^2 / 0.003)) ) },
𝓛: { mean((fit() - once(spec))^2) },

SignalEffect({ spec(), sgd(0.3)(𝓛), ◌ }),   ; one descent step per frame

(spec, fit()) ; overlay would be nicer – see wishlist
```

Nobody watching this forgets it: **the model audibly chases you**. Follow-up: fit a sum of sines to the raw `Microphone()` waveform and resynthesize it (needs audio out).

## 3. Reach — Inverse Kinematics via ∇

A two-joint arm whose fingertip chases a target. The "solver" is four lines: forward kinematics, a distance loss, and gradient descent on the joint angles. This is the cleanest possible demonstration that *differentiation replaces algebra* — nobody derives arctangent IK formulas, the gradient just finds them.

```fluent
target: $([0.5, 1.2]),          ; drag me (Point2D wishlist) – slider-per-axis works today
θ: ~([0.3, 0.8]),               ; joint angles

tip: { [cos(θ_0) + cos(θ_0 + θ_1), sin(θ_0) + sin(θ_0 + θ_1)] },
𝓛: { Σ((tip() - once(target))^2) },
SignalEffect({ target(), sgd(0.5)(𝓛), ◌ }),
```

Scales beautifully: 3 joints, 10 joints — the code barely changes, which *is* the point.

## 4. Kernel Lab — Live Camera Convolution

`Camera()` is a reactive tensor. Put a 3×3 kernel next to it made of nine `Scrubber`s — drag the numbers, and your face becomes an edge map / emboss / blur *while you drag*. The APL-style `conv`/`stencil` from the prelude carries the story: convolution is not a black box, it's nine numbers you can touch. (2D conv needs a `conv2d` binding — 1D `conv` on `Microphone()` works today.)

## 5. Petri Dish — Neural Cellular Automata

Train a tiny update rule so a single pixel *grows into an emoji* and regenerates when you erase part of it (Mordvintsev's growing-CA, miniaturized). Fluent is the only place the whole loop — perception convolution, update rule, training, poking the dish — fits on one screen next to its own source. The flashiest possible answer to "why differentiable + reactive?". Needs `conv2d` + a `Canvas`/eraser interaction (both on the wishlist / TODO already).

## 6. Wave Tank

A 2D wave-equation integrator on a variable tensor, rendered to the heatmap every frame — ripples, interference, reflections. Click to drop a stone (or scrub a "wind" slider). No gradients needed, pure tensor-notation flex: the laplacian-stencil update is ~3 lines of ⍴/slice arithmetic, and it *runs on the GPU from a URL*. Great "shader you can read" energy.

## 7. Paint by Optimization

Upload an image; approximate it with 50 translucent gaussians/strokes optimized by adam. Watching abstract blobs *become* the photo is hypnotic, and the loss curve tells the story. This is the demo for the artist crowd. Needs `ImageUpload` (already on the TODO list).

## 8. Learnable Easing Curve

An animation easing curve defined by a tiny spline; a ball bouncing according to it; drag control points OR click "make it feel like this reference" and let the gradient fit the curve. Ties directly into the planned `Curve` component — the demo and the component justify each other.

## 9. Epicycles — Draw with Fourier

Draw a squiggle, FFT it, replay it as rotating circles stacked tip-to-tail. The eternal crowd-pleaser, and legitimately *at home* here: the whole thing is one `rfft` call plus a `Time()`-driven complex sum. Needs an `fft` binding (tf.js has `tf.spectral.rfft`) and draw-input (`Canvas`).

## 10. The APL Wall

Not one demo — a gallery page of **tweet-sized Fluent programs**, each with its live output: the magnets field, life-in-one-line, a sorting visualization, a fractal. The point is cultural: Fluent inherits APL's "notation as a tool of thought," and a wall of one-liners with living outputs states it better than any manifesto. Cheap to build: it's just curated `?code=` URLs.

---

## Primitive wishlist (ranked by demos unlocked)

| Primitive | Unlocks | Notes |
|---|---|---|
| `Point2D` / draggable marker (or `MousePosition`) | #1 drag-the-ball, #3 target, #8 control points | already on the TODO list as MousePosition |
| Plot overlays (scatter/path layered on heatmap/line) | #1 optimizer trail, #2 fit-over-spectrum | biggest bang for plotting effort |
| `ImageUpload` | #7, #5 targets | on the TODO list |
| `conv2d` binding | #4, #5 | one line: expose `tf.conv2d` |
| `fft` binding | #9, spectrograms | one line: expose `tf.spectral.rfft` |
| `Speaker` (tensor → audio out) | #2 resynthesis, additive synths | `AudioContext` mirror of Microphone |
| `Canvas` (draw-to-tensor) | #5 eraser, #9 input | on the TODO list |

## Suggested order

1. **Loss landscape MVP** — runnable today, ship it as the front-page `?code=` URL.
2. **Point2D + overlays** — small components, they upgrade the MVP into the killer.
3. **Fit My Whistle** — the live-demo/conference closer.
4. **Petri Dish** — the ML-twitter moment, once `conv2d` and `Canvas` exist.
