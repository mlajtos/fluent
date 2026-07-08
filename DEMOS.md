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

## ⭐ 1. Touch the Loss Landscape — **SHIPPED** (`loss-landscape` in the gallery)

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

**Shipped**: `Layers` + `Point2D` (draggable point bound to a tensor variable, with trail) landed, and the gallery version is the full experience — grab θ, drop it on a ridge, watch it roll, with the optimizer's trail drawn on the surface. The final form is 22 lines: one `𝓛` lambda drives both the surface (evaluated over the view via `⊗`) and the descent.

**Shipped variant**: `optimizer-race` — `adam` vs `sgd` racing on the same surface, two trails, two live losses. Still open: a momentum slider, saddle points, a local-minimum trap.

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

; the optimizer lives OUTSIDE the loop – rebuilding it per step resets its
; state and recompiles its program
opt: sgd(0.3),
SignalEffect({ spec(), opt(𝓛), ◌ }),   ; one descent step per frame

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
opt: sgd(0.5),
SignalEffect({ target(), opt(𝓛), ◌ }),
```

Scales beautifully: 3 joints, 10 joints — the code barely changes, which *is* the point.

## 4. Kernel Lab — Live Camera Convolution — **MVP SHIPPED** (`camera-edges` in the gallery)

`Camera()` is a reactive tensor. Put a 3×3 kernel next to it made of nine `Scrubber`s — drag the numbers, and your face becomes an edge map / emboss / blur *while you drag*. The APL-style `conv`/`stencil` from the prelude carries the story: convolution is not a black box, it's nine numbers you can touch.

**Shipped**: `conv` is rank-polymorphic (a 2D kernel over an image just works), and `camera-edges` ships the fixed-Laplacian version. Still open: the nine-scrubber editable kernel, which is the actual "lab".

## 5. Petri Dish — Neural Cellular Automata

Train a tiny update rule so a single pixel *grows into an emoji* and regenerates when you erase part of it (Mordvintsev's growing-CA, miniaturized). Fluent is the only place the whole loop — perception convolution, update rule, training, poking the dish — fits on one screen next to its own source. The flashiest possible answer to "why differentiable + reactive?". Needs `conv2d` + a `Canvas`/eraser interaction (both on the wishlist / TODO already).

## 6. Wave Tank

A 2D wave-equation integrator on a variable tensor, rendered to the heatmap every frame — ripples, interference, reflections. Click to drop a stone (or scrub a "wind" slider). No gradients needed, pure tensor-notation flex: the laplacian-stencil update is ~3 lines of ⍴/slice arithmetic, and it *runs on the GPU from a URL*. Great "shader you can read" energy.

## 7. Paint by Optimization

Upload an image; approximate it with 50 translucent gaussians/strokes optimized by adam. Watching abstract blobs *become* the photo is hypnotic, and the loss curve tells the story. This is the demo for the artist crowd. Needs `ImageUpload` (already on the TODO list).

## 8. Learnable Easing Curve

An animation easing curve defined by a tiny spline; a ball bouncing according to it; drag control points OR click "make it feel like this reference" and let the gradient fit the curve. Ties directly into the planned `Curve` component — the demo and the component justify each other.

## 9. Epicycles — Draw with Fourier

Draw a squiggle, FFT it, replay it as rotating circles stacked tip-to-tail. The eternal crowd-pleaser, and legitimately *at home* here: the whole thing is one `fft` call plus a `Time()`-driven complex sum. `fft` shipped (it powers `spectrum` and `pitch-detector`) — only draw-input (`Canvas`) is missing.

## 10. The APL Wall

Not one demo — a gallery page of **tweet-sized Fluent programs**, each with its live output: the magnets field, life-in-one-line, a sorting visualization, a fractal. The point is cultural: Fluent inherits APL's "notation as a tool of thought," and a wall of one-liners with living outputs states it better than any manifesto. Cheap to build: it's just curated `?code=` URLs.

---

## Primitive wishlist (ranked by demos unlocked)

| Primitive | Unlocks | Notes |
|---|---|---|
| ~~`Point2D` / draggable marker~~ | #1 drag-the-ball, #3 target, #8 control points | **shipped**, with `MousePosition` and `Trail` |
| ~~Plot overlays~~ | #1 optimizer trail, #2 fit-over-spectrum | **shipped** as `Layers` |
| ~~`ImageUpload`~~ | #7, #5 targets | **shipped** (image still lost on re-eval — TODO) |
| ~~`conv` (nD)~~ | #4, #5 | **shipped**, rank-polymorphic via `lax.conv` |
| ~~`fft`~~ | #9, spectrograms | **shipped** (`[real; imag]` stacked — no complex dtype yet) |
| `Speaker` (tensor → audio out) | #2 resynthesis, additive synths | `AudioContext` mirror of Microphone |
| `Canvas` (draw-to-tensor) | #5 eraser, #9 input | on the TODO list; unlocks the two biggest remaining demos |

## Suggested order

1. ~~**Loss landscape**~~ — **shipped**, plus the `optimizer-race` variant.
2. ~~**Point2D + overlays**~~ — **shipped** (`Point2D`, `Trail`, `Layers`).
3. **Fit My Whistle** — the live-demo/conference closer; everything it needs exists now.
4. **Kernel Lab** — upgrade `camera-edges` with a nine-scrubber editable kernel.
5. **Petri Dish** — the ML-twitter moment; only `Canvas` is missing now.
