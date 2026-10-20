// Gallery examples: Fluent source, one per key. Extracted from client.tsx so
// both the IDE and the test suite can import the same corpus (tests.ts runs
// them headless as a smoke suite).

export const EXAMPLES = {
  "no-precedence": `
; Left-to-right evaluation – no operator precedence
1 + 2 * 3 - 4 / 5 ^ sin(2)
`,
  "whitespace-precedence": `
; Whitespace is precedence – a glued operator binds before a spaced one
(
  1 + 2 * 3, ; spaced: left-to-right = 9
  1 + 2*3, ; glued * binds first = 7
  a: 1 + 2, ; glued ':' takes the whole right side
  θ: [3, 4],
  √(θ_0^2 + θ_1^2), ; tight chains: (θ_0)^2 + (θ_1)^2
)
`,
  "infix-lambda": `
; Infix lambda – using a lambda as an infix operator
1 { x, y | x + y } 2
`,
  "broadcasting": `
; Tensor broadcasting – scalar added to vector
[1, 2, 3] + 1
`,
  "vector-addition": `
; Vector addition – element-wise
a: [1, 2, 3], b: [4, 5, 6],
a + b
`,
  "calling-conventions": `
; Calling conventions – multiple ways to call functions
(
  1 + 2,
  1 add 2,
  add(1, 2),
  +(1, 2),
  (1, 2) . +,
  (1, 2) pipe add,
)
`,
  "max-variations": `
; Max variations – max/min reduce a tensor; ⌈/⌊ are APL ceiling/floor (one arg), max/min (two)
(
  max([3, 1, 2]), ; 3 – the largest element
  max([[1, 2], [3, 4]], 0), ; [3, 4] – reduce along an axis, like sum
  ⌈(2.3), ; 3 – ceiling
  ⌊(2.7), ; 2 – floor
  2 ⌈ 3, ; 3 – pairwise max
  [1, 5] ⌈ [4, 2], ; [4, 5] – element-wise max
)
`,
  "gradient": `
; Gradient – automatic differentiation with ∇
f: { x | x^2 },
g: ∇f,
x: 1 ..< 10,
(
  f(x),
  g(x),
)
`,
  "ad-hoc-operators": `
; Ad-hoc operators – defining custom operators
(++): concat,
(1, 2) ++ (3, 4)
`,
  "combinators": `
; Combinators – the songbirds of tacit programming.
; Smullyan named them after birds ("To Mock a Mockingbird")
; honoring Haskell Curry, logician and avid birdwatcher.

Grid(3)(
  ("Queer bird",      \`g(f(x))\`,           9 (+ ∘ √) 16            ),
  ("Cardinal",        \`f(y, x)\`,           3 (⍨ -) 10              ),
  ("Warbler",         \`f(x, x)\`,           (⍨ ×) 9                 ),
  ("Psi",             \`f(g(x), g(y))\`,     -3 (+ ⍥ =) 3            ),
  ("Starling",        \`f(x, g(x))\`,        (÷ ⟜ √) 16              ),
  ("Dove",            \`f(x, g(y))\`,        16 (÷ ⟜ √) 4            ),
  ("Violet Starling", \`g(f(x), x)\`,        (√ ⊸ ÷) 16              ),
  ("Zebra Dove",      \`g(f(x), y)\`,        9 (√ ⊸ ÷) 3             ),
  ("Phoenix",         \`g(f(x), h(x))\`,     Φ(Σ, ÷, #) [1, 2, 3, 4] ),
  ("Pheasant",        \`g(f(x,y), h(x,y))\`, 5 Φ(=, ∨, >) 3          ),
  ("Kestrel",         \`x\`,                 3 ⊣ 5                   ),
  ("Kite",            \`y\`,                 3 ⊢ 5                   ),
)
`,
  "rank": `
; ⍤ Rank – Iverson's rank conjunction. \`f ⍤ k\` applies f to each rank-k cell of
; its argument and maps over the leading frame. Write a function once, run it at
; any rank: the same code re-scopes itself. (Dyadic \`a (f ⍤ k) b\` zips cells,
; a lone cell broadcasting over the frame.)

M: [[1, 4, 2, 8],
    [5, 3, 9, 1],
    [7, 2, 6, 4],
    [3, 8, 1, 5]],
nrm: { x | x - min(x) ÷ (max(x) - min(x)) }, ; scales an array to 0..1

(
  Text("# ⍤ Rank polymorphism"),
  Text("**One** function \`nrm\`, run at three ranks — no rewrite, the rank re-scopes it."),
  Text("**nrm(M)** — the whole grid shares one min/max:"),
  nrm(M),
  Text("**(nrm ⍤ 1)(M)** — each ROW normalized on its own (every row now spans 0..1):"),
  (nrm ⍤ 1)(M),
  Text("**(Σ ⍤ 1)(M)** collapses each row to its sum; **Σ(M)** collapses the lot:"),
  ((Σ ⍤ 1)(M), Σ(M)),
)
`,
  "inner-product": `
; ∙ Inner product – matrix multiply is just \`× ∙ +\`: multiply, then add up.
; Keep the contraction, swap the ring, and the same operation becomes something
; else. One combinator, every semiring – arithmetic, shortest paths, reachability.

A: [[1, 2],
    [3, 4]],
B: [[5, 6],
    [7, 8]],

; a weighted graph – 999 stands in for "no edge"
W: [[  0,   3, 999, 999],
    [  3,   0,   1, 999],
    [999,   1,   0,   2],
    [999, 999,   2,   0]],
hop: { M | M (+ ∙ ⌊) W }, ; one min-plus relaxation = allow one more edge

(
  Text("# ∙ One product, every semiring"),
  Text("**A (× ∙ +) B** — ordinary matrix multiply (multiply, then add up):"),
  A (× ∙ +) B,
  Text("**edge weights** W (999 = no edge):"),
  W,
  Text("**W (+ ∙ ⌊) W** — swap to min-plus (add the legs, keep the shortest): one round of relaxation:"),
  W (+ ∙ ⌊) W,
  Text("**all-pairs shortest paths** — iterate min-plus with ⍣ until nothing changes:"),
  (hop ⍣ ≡)(W),
)
`,
  "under": `
; ⍢ Around – the simple thing, in the right space.
;   x (g ⍢ f) y = g⁻¹(f(g(x), g(y)))
; Fluent can differentiate any smooth g, so it can also run g backwards:
; Newton's method, with ∇ supplying the slope. No table of inverses.
; And when g picks out a part of x, f edits just that part.

dB: { x | 10 ^ (x ÷ 10) },
logit: { p | log(p ÷ (1 - p)) },
(∥): reciprocal ⍢ +,

target: $3,
row: { name, formula, value | (Text(name), Text(formula), value) },
M: [[3, 1, 4],
    [1, 5, 9],
    [2, 6, 5]],
diagonal: { m | Σ(m × 𝕀(#m), 1) },

(
  Text("# ⍢ Around – the simple thing, in the right space"),
  Grid(3)(
    row("Pythagoras",                 "√(x² + y²)",          3 (square ⍢ +) 4         ),
    row("resistors in parallel",      "1 ÷ (1/x + 1/y)",     6 ∥ 3                    ),
    row("the slide rule",             "x · y",               2 (log ⍢ +) 8            ),
    row("two 10% raises",             "(1+x)(1+y) − 1",      0.1 (log1p ⍢ +) 0.1      ),
    row("+50% then −50%, on average", "√((1+x)(1+y)) − 1",   (log1p ⍢ μ)([0.5, -0.5]) ),
    row("there at 30, back at 60",    "harmonic mean",       (reciprocal ⍢ μ)([30, 60])),
    row("geometric mean",             "ⁿ√(x · y · …)",       (log ⍢ μ)([1, 2, 4, 8])  ),
    row("root mean square",           "√(mean of x²)",       (square ⍢ μ)([3, 4])     ),
    row("60 dB + 60 dB",              "10 log(10⁶ + 10⁶)",   60 (dB ⍢ +) 60           ),
    row("½c ⊕ ½c, relativistically",  "(u + v) ÷ (1 + uv)",  0.5 (atanh ⍢ +) 0.5      ),
    row("a six in four rolls",        "1 − (1 − p)⁴",        (1 ⊸ - ⍢ Π)(1/6 ⧉ 4)   ),
    row("two 80% witnesses",          "odds multiply",       0.8 (logit ⍢ +) 0.8      ),
    row("smooth max",                 "log(Σ eˣ)",           (exp ⍢ Σ)([1, 2, 3])     ),
  ),
  Text("# ⍣ -1 – run any function backwards"),
  Grid(3)(
    row("x³ + x = 10",                "no formula needed",   ({ x | x^3 + x } ⍣ -1)(10)),
    row("xˣ = 3",                     "Lambert W, normally", ({ x | x^x } ⍣ -1)(3)     ),
    row("2 + ? = 8, 2 × ? = 8, 2 ^ ? = 8", "−, ÷, log",      [2 (+ ⍣ -1) 8, 2 (× ⍣ -1) 8, 2 (^ ⍣ -1) 8]),
    row("bottom of (x − 3)² + 1",     "where the slope is 0", ((∇{ x | (x - 3)^2 + 1 }) ⍣ -1)(0)),
    row("press cos until it stops",   "the Dottie number",   (cos ⍣ ≡)(1)              ),
  ),
  Grid([2, 5])(Text("**drag the answer, xˣ =**"), Scrubber(target, 0.05)),
  Grid([2, 5])(Text("**and x follows:**"), ({ x | x^x } ⍣ -1)(target)),
  Text("# ⍢ on a part – f edits just what g picks"),
  Grid(3)(
    row("sort only the middle",       "x_[1, 2, 3]",         ({ x | x_[1, 2, 3] } ⍢ sort)([9, 4, 1, 3, 0])),
    row("zero the diagonal",          "diagonal",            (diagonal ⍢ (× ⟜ 0))(M)       ),
    row("scale one row to length 1",  "the first row",       ({ m | m_0 } ⍢ normalize)(M)  ),
  ),
)
`,
  "lerp": `
; Lerp – linear interpolation with custom operators

; lerp: 0.5 ≻ [10, 30] = 20
(≻): { t, v | v_0 × (1 - t) + v_1×t },

; invlerp: [1, 3] ≺ 2 = 0.5
(≺): { v, t | t - v_0 ÷ (v_1 - v_0) },

rescale: { x | [min(x), max(x)] ≺ x },
[10, 20, 30] . rescale
`,
  "reactive-slider": `
; Reactive slider – with a computed value
(
  x: $0.5,
  Slider(x),
  x^2,
)
`,
  "reactive-function-plot": `
; Interactive sine wave – adjust the resolution with the slider

resolution: $0.5,
points: resolution × 90 + 10,
x: linspace([-π, π], points),

(
  Slider(resolution),
  sin(x),
)
`,
  "drawing": `
; Drawing as values – a picture is colors, a shape is a distance, an ink makes it light
W: 640, H: 400,
t: Time(),

; the playground's own colors
blue: [10, 129, 239] ÷ 255,
magenta: [242, 56, 227] ÷ 255,
gray: [68, 68, 68] ÷ 255,
dark: [23, 23, 23] ÷ 255,

; y = sin(x + t) over a full turn, in pixels (the screen's y runs down)
x: linspace([0, 2×π], 128),
y: sin(x + t),
curve: stack((x×W ÷ 2×π, H÷2 - y×H÷2.4), -1),
axis: [[0, H÷2], [W, H÷2]],
marks: curve_(0..<8 × 18),

(
  Text("# ✏️ Drawing as values"),
  canvas(W, H)
    . draw(line(axis), gray, 1)
    . paint(line(curve), stroke(blue, 2.5) above halo(blue × 0.5, 8))
    . paint(dots(marks), stroke(magenta, 7) above stroke(dark, 11)),
)
`,
  "tasks": `
; TODO App – reactive task management

; Helper operators
(++): concat,
(++=): FunctionNoAutoLift({ a, b | a(a() ++ List(b)) }),

; Task factory – creates an editable task with a toggle
taskCreate: { name |
  text: $name,
  done: $"🔴",
  Grid([1, 9])(
    Button(done, { done← "✅" }),
    TextEditor(text),
  )
},

; App state
taskName: $"",
tasks: $List(),

; UI
(
  Text("# TODO App"),
  Grid([5, 1])(
    TextEditor(taskName),
    Button("Add Task", {
      tasks ++= taskCreate(taskName()),
      taskName← "",
    }),
  ),
  tasks,
)
`,
  "tasks-compressed": `
(++):concat,taskName: $"",taskCreate:{name|s: $name,f: $"🔴",Grid([1,10])(Button(f,{f("✅")}),TextEditor(s))},tasks: $(()),(Text("# TODO"),Grid([5,1])(TextEditor(taskName),Button("Add",{tasks(tasks()++List(taskCreate(taskName()))),taskName("")})),tasks)
`,
  "tasks-mini": `
; Minimal TODO app – type a task, add it to the list
(++): concat,
n: $"",
tasks: $List(),
(
  TextEditor(n),
  Button("Add", {
    tasks(tasks() ++ List(n())),
    n("")
  }),
  tasks,
)
`
  ,
  "calculator": `
; Calculator – builds an expression string and evaluates it

(++): concat,

expr: $"",
result: expr . CodeEvaluate,
append: { f | { expr(expr() . f) } },
btn: { c | Button(c, { x | x ++ c } . append) },

(
  TextEditor(expr),
  result,
  Grid([2, 2, 2, 1])(
    btn("7"), btn("8"), btn("9"), btn(" ÷ "),
    btn("4"), btn("5"), btn("6"), btn(" × "),
    btn("1"), btn("2"), btn("3"), btn(" - "),
    Button("C", { "" } . append), btn("0"), btn("."), btn(" + "),
  ),
)
`,
  "linear-regression": `
; Linear regression – with gradient descent
; Find θ that minimizes (f(x) - y)²

; Operators
(++): concat,

; Data: y = 0.23x + 0.47
x: 0 ..< 10,
y: x×0.23 + 0.47,

; Model: f(x) = θ₀·x + θ₁
θ: ~[0, 0],
f: { x | x × θ_0 + θ_1 },

; Loss: mean squared error
𝓛: { mean((f(x) - y)^2) },

; Training
opt: adam(0.03),
losses: $[],

{ losses(losses() ++ [opt(𝓛)]) } ⟳ 200,

; Results
(
  Text("**Loss:**"),
  losses,
  Text("**θ (learned):**"),
  θ,
  Text("**θ (target):**"),
  [0.23, 0.47],
)
`,
  "linear-regression-compressed": `
x: 0 ..< 10,
y: x×0.23 + 0.47,
θ: ~[0, 0],
f: { x | x × θ_0 + θ_1 },
𝓛: { μ((y - f(x))^2) },
minimize: adam(0.03),
losses: $[],
{ losses(losses() concat [minimize(𝓛)]) } ⟳ 400,
(losses, θ)`,
  "REPL": `
; Multi-cell REPL – each cell evaluates independently

cell: {
  code: $"1 + 1",
  result: CodeEvaluate(code),
  Grid(1)(Print(result), CodeEditor(code, "auto"))
},

(
  cell(),
  cell(),
  cell(),
)
`,
  "exponential-decay": `
; Exponential decay – f(t) = b + c·e^(-k·t)
; Interactive visualization with slider

b: 0.1, ; baseline
c: 1, ; amplitude
k: 5, ; decay rate

f: { t | b + c×exp((-k) × t) },

; Interactive plot
t: $0,
(
  Slider(t),
  f(t),
  f(0..<100 / 100),
)
`,
  "box-cox": `
; 📈 Box–Cox – one dial λ reshapes the data, and ln sits at its center.
; p_λ(x) = (x^λ − 1) / λ. Scrub λ: 0 → ln, ½ → √, 1 → linear, 2 → parabola.
; It's a family of powers (x^λ). exp is not in it – exp lives in the inverse
; transform, whose own λ→0 center is exp. So p₂(x) = (x²−1)/2 is a parabola, not eˣ.

x: linspace([0.05, 4], 240), ; Box–Cox is defined for x > 0
λ: $0, ; the dial – scrub me: try 0, 1, 2, −1

; the transform. at λ = 0 the 0/0 limit is exactly ln(x), so branch there.
; (x × 0 broadcasts the scalar test across the 240 sample points)
p: { l | where(abs(l)<0.001 + x×0, log(x), x^l - 1 ÷ l) },

; wrap the plot in a computed so it re-draws as you scrub λ (like Scrubber does)
curve: \${ PointPlot(x, p(λ())) },

(
  Text("# 📈 Box–Cox transform"),
  Text("One dial **λ** reshapes data toward a bell curve. **λ→0 is ln** (the smooth center), 1 is linear, 2 is a parabola. A power family — **exp is not in it**."),
  Grid([1, 6])(Text("**λ =**"), Scrubber(λ, 0.02)),
  curve,
)
`,
  "half-exp": `
; 🌗 The half of exp – a net φ trained on one rule: φ(φ(x)) = eˣ.
; No labels. The only signal is self-consistency: apply φ twice, land on exp.
; φ settles halfway – it nudges each x partway toward eˣ, and twice completes the
; trip. This √exp has no elementary closed form (Kneser built it with complex
; analysis); we just fit it, straight from its own equation.

(++): concat,

d: 16,
x: linspace([-1, 1], 96),
X: x ⍴ [96, 1],
T: exp(x) ⍴ [96, 1],

; residual MLP: φ(z) = z + net(z). net starts at 0 (W3 = 0), so φ begins as identity
; and grows the half-step from there – which picks the increasing branch of √exp.
W1: ~(randn([1, d]) × 0.6),
b1: ~fill([d], 0),
W2: ~(randn([d, d]) × 0.15),
b2: ~fill([d], 0),
W3: ~fill([d, 1], 0),
b3: ~[0],
φ: { z | z + (matmul(silu(matmul(silu(matmul(z, W1) + b1), W2) + b2), W3) + b3) },

; the only loss: be self-consistent. no target for φ itself, only for φ∘φ.
𝓛: { mean((φ(φ(X)) - T)^2) },
opt: adam(0.02),

losses: $[],
half: \${ φ(X) ⍴ [96] }, ; φ(x) – climbs to sit between x and eˣ
whole: \${ φ(φ(X)) ⍴ [96] }, ; φ(φ(x)) – snaps onto eˣ
{ losses(losses() ++ [opt(𝓛)]) } ⟳ 2000,

(
  Text("# 🌗 The half of exp"),
  Text("A net **φ** trained on one rule — **φ(φ(x)) = eˣ** — with no labels, only self-consistency. It learns √exp, a function with no closed form."),
  Text("**loss** — self-consistency, converging:"),
  losses,
  Text("**φ(x)** (middle) settles between **x** (bottom) and **eˣ** (top) — genuinely halfway:"),
  plot(x, (x, half, exp(x)), ("x", "phi(x)", "exp(x)")),
  Text("**φ(φ(x))** lands on **eˣ**:"),
  plot(x, (exp(x), whole), ("exp(x)", "phi(phi(x))")),
)
`,
  "deep-delta-learning": `
; Deep Delta Learning – a stack of delta blocks learns sin(x) from 24 points.
; A delta block edits its state x along one direction k, by the delta rule:
;   x' = x + β·k·(v − k·x)
; It reads from x where to write (k, a unit vector), what to write (v), and how
; hard (β, from 0 to 2): β = 0 keeps x, 1 replaces its k-part with v, 2 reflects it.

d: 16, depth: 3,

; Learnable parameters: an embedding, Wk, wv and wβ for each block, a readout
E: ~(randn([d]) × 1.5),
e: ~randn([d]),
Wk: ~(randn([depth, d, d]) ÷ √(d)),
wv: ~(randn([depth, d]) ÷ √(d)),
wβ: ~(randn([depth, d]) ÷ √(d)),
u: ~(randn([d]) ÷ √(d)),

; Delta block i, on a batch of states x [n, d]
Δ: { x, i |
  q: matmul(x, Wk_i), k: q ÷ (norm(q, -1, 1) + 1e-6),
  v: Σ(x × wv_i, -1, 1),
  β: 2 × sigmoid(Σ(x × wβ_i, -1, 1)),
  x + (β × (v - Σ(k × x, -1, 1)) × k)
},

; The model: one number in, d features, the blocks in turn, one number out
f: { t | h: tanh(unsqueeze(t, -1) × E + e), Σ(fold(0..<depth, Δ, h) × u, -1) },

; Data: 24 points to learn from, and 200 in between that it never sees
xs: ⚄([24]) × 2×π,
grid: linspace([0, 2×π], 200),

; Training
(++): concat,
𝓛: { mean((f(xs) - sin(xs))^2) },
unseen: { mean((f(grid) - sin(grid))^2) },
opt: adam(0.01),
losses: $[],
tests: $[],
pred: \${ f(grid) },
; a step at a time; every 10th, the errors are logged
{ i |
  l: opt(𝓛),
  when(i % 10 = 0, { losses(losses() ++ [l]), tests(tests() ++ [unseen()]) })
} ⟳ 600,

; Results
(
  Text("**sin(x) and the model**, on the 200 points it never trained on:"),
  plot(grid, (sin(grid), pred), ("sin(x)", "model")),
  Text("**How far off it is** (root-mean-square), on the 24 training points and on the 200 unseen ones:"),
  \${ plot(0..<(#losses()) × 10, (√(losses()), √(tests())), ("train", "unseen")) },
)
`,
"loss-landscape": `
; Touch the Loss Landscape – one loss definition drives both the surface and the descent.
; Edit it live.

𝓛: { p | (p_0^2 + p_1 - 11)^2 + (p_0 + p_1^2 - 7)^2 }, ; Himmelblau – four minima

θ: ~[0, 0], ; the ball – grab it
lr: $0.4,
step: { sgd(lr() × 0.005)({ 𝓛(θ) }) },

; the surface: the same 𝓛 evaluated over the whole view via ⊗
n: 300,
view: [[-5, 5], [-5, 5]],
surface: log(linspace(view_1, n) (⊗ { y, x | 𝓛([x, y]) }) linspace(view_0, n) + 1),

(
  Text("# Touch the Loss Landscape"),
  Grid([1, 5])(Text("lr"), Slider(lr)),
  Button("Descend ×100", { step ⟳ 100 }),
  Layers(surface, Trail(θ, view), Point2D(θ, view)),
  \${ 𝓛(θ) }, ; live loss – updates while you drag
)
`,
"geometric-median": `
; 📍 Geometric median – drag the three orange anchors. The blue point rolls
; downhill on the total-distance field to the spot closest to all three,
; re-optimizing (adam) live as you drag. The field below is that distance,
; recomputed each frame; the blue point descends it.

view: [[-2, 2], [-2, 2]],
n: 140,
soft: 0.02, ; softens the distance at 0 so the gradient stays finite

; three draggable anchors, each a 2-vector
a: $[-1.2, 0.8],
b: $[1.3, 1.0],
c: $[0.1, -1.3],

; total distance from a point p to every anchor (indexed form is ⊗-friendly)
dist: { p, q | √((p_0 - q_0)^2 + (p_1 - q_1)^2 + soft) },
F: { p | dist(p, a()) + dist(p, b()) + dist(p, c()) },

; the blue point descends F – the only trainable thing here
p: ~[1.6, 1.6],
opt: adam(0.06),
𝓛: { F(p) },
{ opt(𝓛) } ⟳ Time()×60, ; 60 steps a second

; the distance field as a heatmap, recomputed as the anchors move. Read the
; anchors to concrete tensors first (av/bv/cv), so the ⊗ lambda works on plain
; tensors.
g: linspace([-2, 2], n),
field: \${
  av: a(), bv: b(), cv: c(),
  Fc: { q | dist(q, av) + dist(q, bv) + dist(q, cv) },
  g (⊗ { y, x | Fc([x, y]) }) g
},

(
  Text("# 📍 Geometric median"),
  Text("Drag the **orange** anchors — the **blue** point keeps finding the spot closest to all three."),
  Layers(
    field,
    Point2D(a, view, "orange"),
    Point2D(b, view, "orange"),
    Point2D(c, view, "orange"),
    Trail(p, view, "deepskyblue"),
    Point2D(p, view, "deepskyblue"),
  ),
)
`,
"differentiable-reach": `
; 🦾 Differentiable reach – gradient descent bends an arm to the target and away from the obstacle
n: 16, ; joints
len: 0.85 ÷ n, ; segment length
θ: ~fill([n], 0.15), ; the only trainable thing

tril: 0..<n ⊗(≥) 0..<n, ; prefix-sum matrix
cumsum: { v | matmul(tril, v ⍴ [n, 1]) ⍴ [n] }, ; scan
fk: { a | ; forward kinematics
  ang: cumsum(a), ; cumulative angle
  step: len × stack((cos(ang), sin(ang)), 1), ; xy step
  matmul(tril, step) - [0.45, 0] ; joint positions
},

target: $[0.55, 0.35], ; drag me (orange)
obstacle: $[-0.1, -0.3], ; drag me (red)
last: oneHot(n - 1, n) ⍴ [n, 1], ; picks the tip

𝓛: { ; loss
  P: fk(θ), ; joint positions
  tip: Σ(P × last, 0), ; the hand
  reach: Σ((tip - target())^2), ; hand → target
  avoid: Σ(1 ÷ (Σ((P - obstacle())^2, 1) + 0.02)), ; repel
  reach + 0.005×avoid
},

opt: OptimizerMuon(0.03), ; optimiser
joints: \${ fk(θ) }, ; live pose for the picture
{ opt(𝓛) } ⟳ Time()×60, ; 60 steps a second

view: [[-1, 1], [-1, 1]],
S: 300, ; the picture is S × S units, the view's -1 … 1 across it
arm: \${ ; the arm as a lit line from its base, a bead at every joint
  P: concat(([[-0.45, 0]], joints())) + 1 × S÷2,
  canvas(S, S)
    . paint(line(P), stroke([0.55, 0.8, 1], 4) above halo([0.05, 0.4, 0.9] × 0.8, 14))
    . draw(dots(P), [0.92, 0.96, 1], 5)
    . draw(dots(P_[0]), [0.92, 0.96, 1], 14)
},

(
  Text("# 🦾 Differentiable reach"),
  Text("Drag the **orange** target — the arm gradient-descends to it. Drag the **red** obstacle — it bends away. FRP × AD, one graph."),
  Layers(arm, Point2D(target, view, "orange"), Point2D(obstacle, view, "red")),
)
`,
"differentiable-aim": `
; 🎯 Aim by calculus – ∇ through a physics simulation finds the launch that lands on the target
T: 48, dt: 0.04, g: [0, 4.2], ; steps · timestep · gravity (down is +y on screen)
p0: [-0.78, 0.55], ; the muzzle at the bottom-left (fixed)
vel: ~[1.7, -2.3], ; launch velocity (up-and-right) – the only trainable thing

t: 0..<T × dt, ; sample times [T]
fly: { u | p0 + (t ⊗(×) u) + (0.5 × (t^2 ⊗(×) g)) }, ; the ballistic path, [T, 2]

target: $[0.72, -0.12], ; drag me (orange)
𝓛: { Σ((fly(vel)_(T - 1) - target())^2) }, ; squared miss of where the shot lands

opt: adam(0.06), ; optimiser
path: \${ fly(vel) }, ; live trajectory for the picture
{ opt(𝓛) } ⟳ Time()×60, ; descend the miss, 60 steps a second

view: [[-1, 1], [-1, 1]],
S: 280, ; the picture is S × S units, the view's -1 … 1 across it
shot: \${ ; the trajectory as a lit trail, a bead at every step, the muzzle at its start
  P: path() + 1 × S÷2,
  canvas(S, S)
    . paint(line(P), stroke([0.6, 1, 0.75], 1.5) above halo([0.1, 0.8, 0.5] × 0.6, 10))
    . draw(dots(P), [0.85, 1, 0.9], 4)
    . draw(dots(P_[0]), [0.85, 1, 0.9], 10)
},

(
  Text("# 🎯 Aim by calculus"),
  Text("Drag the **orange** target. The launch velocity gradient-descends *through a physics simulation* until the shot lands on it — no ballistics algebra, just ∇ of the miss. FRP × AD in one graph."),
  Layers(shot, Point2D(target, view, "orange")),
)
`,
"differentiable-bezier": `
; 〰️ Differentiable Bézier – ∇ bends a curve until it runs through every ring
; The curve runs from the first gold ring to the last, and ∇ moves its two middle control
; points, 60 steps a second. Drag the orange points to bend the rings: the curve follows
gray: [0.45, 0.45, 0.45],
blue: [0.35, 0.65, 1],
magenta: [0.95, 0.4, 0.8], gold: [1, 0.78, 0.3],

bend: $[80, 20], twist: $[180, 150], ; drag me (orange)
targets: bezier(stack(([30, 130], bend, twist, [230, 30])), 6),
middle: ~[[110, 140], [150, 20]],
c: concat((targets_[0], middle, targets_[-1])),
𝓛: { mean(line(bezier(c, 0.1))(targets)^2) }, opt: adam(3),
{ opt(𝓛) } ⟳ Time()×60, ; 60 steps a second

curve: canvas(260, 160)
  .draw(line(c), gray, 1)
  .paint(
    line(bezier(c, 0.1)),
    stroke(blue, 2.5) above halo(blue × 0.4, 8)
  )
  .draw(dots(c), magenta, 7)
  .draw({ p | abs(dots(targets)(p) - 6) }, gold, 1.5),

view: [[0, 260], [0, 160]],
(
  Text("# 〰️ Differentiable Bézier"),
  Text("∇ moves the **pink** control points until the curve runs through every gold ring. Drag the **orange** points to bend the rings."),
  Layers(curve, Point2D(bend, view, "orange"), Point2D(twist, view, "orange")),
)
`,
"gradient-paint": `
; 🖌 Differentiable rasterisation – soft blobs rearrange so the picture they paint matches a target
K: 14, blobW: 0.008,
; the loss is scored on a coarse grid (cheap ∇); the picture is drawn on a fine one
lres: 40, vres: 100,
lg: linspace([-1, 1], lres), vg: linspace([-1, 1], vres),

center: $[0, 0], ; drag me (orange) – moves the target ring
ring: { g, n, c | ; the goal image: a bright ring of radius 0.5 around c, on an n×n grid
  gx: g ⍴ [1, n], gy: g ⍴ [n, 1],
  r: √((gx - c_0)^2 + (gy - c_1)^2),
  exp(-((r - 0.5)^2) ÷ 0.006)
},
blobs: { p, g, n | ; K Gaussian blobs at centers p [K, 2] onto an n×n image
  q: transpose(p),
  dx: g ⍴ [1, n, 1] - (q_0 ⍴ [1, 1, K]),
  dy: g ⍴ [n, 1, 1] - (q_1 ⍴ [1, 1, K]),
  Σ(exp(-(dx^2 + dy^2) ÷ blobW), 2)
},

a0: 0..<K × (2×π ÷ K), ; blobs start as a tiny huddle at the middle
θ: ~stack((0.15 × cos(a0), 0.15 × sin(a0)), 1), ; K blob centers – the only trainable thing

𝓛: { Σ((blobs(θ, lg, lres) - ring(lg, lres, center()))^2) }, ; coarse pixel mismatch

opt: adam(0.02),
shown: \${ blobs(θ, vg, vres) }, ; the fine picture, for display
{ opt(𝓛) } ⟳ Time()×120, ; descend the mismatch, 120 steps a second

view: [[-1, 1], [-1, 1]],
(
  Text("# 🖌 Painting with gradients"),
  Text("Fourteen soft blobs start in a huddle. Gradient descent moves them until the image they *paint* matches a target ring — differentiable rasterisation. Drag the **orange** dot to move the goal; the blobs chase it live."),
  Layers(shown, Point2D(center, view, "orange")),
)
`,
"differentiable-cloth": `
; 🧵 Differentiable cloth – a mass-spring sheet hangs from two corners and swings when you
; drag one; ∇ descends its elastic energy, with momentum
nx: 6, ny: 6,
xs: linspace([-0.45, 0.45], nx),
ys: linspace([-0.4, 0.4], ny), ; row 0 is the top edge (+y is downward on screen)
θ: ~stack(meshgrid(xs, ys), 2), ; node positions [ny, nx, 2] – the only trainable thing

restH: 0.9 ÷ (nx - 1), ; horizontal spring rest length
restV: 0.8 ÷ (ny - 1), ; vertical rest length
restD: √(restH^2 + restV^2), ; shear-diagonal rest length
rest: [restH, restV, restD, restD] ⍴ [4, 1, 1], ; one per neighbour, ready to broadcast
grav: 0.06,
pinW: 40,

z: fill([ny, nx], 1), ; broadcaster / node weights
inCol: 0..<nx < (nx - 1) ⍴ [1, nx], ; drop the last column
inRow: 0..<ny < (ny - 1) ⍴ [ny, 1], ; drop the last row
inColL: 0..<nx > 0 ⍴ [1, nx], ; drop the first column – mask off roll's wrap-around

pinL: $[-0.55, -0.4], ; drag me (orange) – the top-left corner
pinR: $[0.55, -0.4], ; drag me (cyan) – the top-right corner

𝓛: {
  D: roll(θ, -1, 0), ; the row below
  nbr: stack((roll(θ, -1, 1), D, roll(D, -1, 1), roll(D, 1, 1)), 0), ; four neighbours → ↓ ↘ ↙ [4, ny, nx, 2]
  keep: stack((inCol × z, inRow × z, inRow × inCol, inRow × inColL), 0), ; which of those neighbours are real edges
  stretch: norm(nbr - θ, 3) - rest, ; how far each spring is from its rest length
  springs: Σ(keep × stretch^2), ; every spring, all four families at once
  gravity: grav × Σ(θ × [0, -1]), ; every node pulled down (screen +y)
  pins: Σ((θ_0_0 - pinL())^2) + Σ((θ_0_(nx - 1) - pinR())^2), ; the two top corners follow the dots
  springs + gravity + pinW×pins
},

opt: sgd(0.02, 0.975), ; momentum is the sheet's inertia, 1 - 0.975 its friction
{ opt(𝓛) } ⟳ Time()×240, ; 240 steps a second

view: [[-1, 1], [-1, 1]],
S: 480, ; the picture is S × S units, the view's -1 … 1 across it
mesh: \${ ; every spring across and down a lit thread, every node a bead
  P: θ + 1 × S÷2,
  a: concat((slice(P, [0, 0], [ny, nx - 1]) ⍴ [-1, 2], slice(P, [0, 0], [ny - 1, nx]) ⍴ [-1, 2])),
  b: concat((slice(P, [0, 1], [ny, nx - 1]) ⍴ [-1, 2], slice(P, [1, 0], [ny - 1, nx]) ⍴ [-1, 2])),
  canvas(S, S)
    . paint(segments(a, b), stroke([1, 0.72, 0.45], 2) above halo([0.85, 0.35, 0.1] × 0.45, 7))
    . draw(dots(P ⍴ [-1, 2]), [1, 0.93, 0.8], 6)
},

(
  Text("# 🧵 Differentiable cloth"),
  Text("Drag either **corner** — the elastic sheet swings and settles. Springs on every edge, shear diagonals to keep it from folding flat, and gravity add up to one energy. Gradient descent with momentum rolls the node positions down it like a heavy ball: the momentum is the sheet's inertia, so it sways before it comes to rest."),
  Layers(mesh, Point2D(pinL, view, "orange"), Point2D(pinR, view, "cyan")),
)
`,
"caustics": `
; 🔆 Caustics – ∇ shapes a sheet of glass until the light through it writes your word
; Light falls through the glass onto a wall. Where the glass tilts it bends the light, and
; the bent rays crowd into bright lines – a caustic, like sunlight on the floor of a pool.
; ∇ of how far the light is from the goal reshapes the glass, 60 times a second.
W: 320, H: 240, gap: 5, ; the wall, in units, and a ray every gap units across it
depth: 40, ; how far the light travels from the glass to the wall
r: 1.2 × gap, ; how wide a ray lands

word: $"Fluent",
at: $[160, 120], ; the middle of the word – drag it
face: $0, ; 1: the camera's picture is the goal
cam: Camera(160, 120),

; the rays leave the glass on a grid; its height there is all that trains
xs: 0 ..< W÷gap + 0.5 × gap, ys: 0 ..< H÷gap + 0.5 × gap,
glass: ~(noise(stack(meshgrid(xs, ys), -1) ÷ 40) × 8),

; the caustic: the light at the points (x, y) of the wall. Each ray lands moved by the
; glass's slope and glows there – a halo, exp(-d²/r²) – and a glow is its glow across
; times its glow down, so the light of every ray at every point is one matrix product
caustic: { x, y, r |
  dx: roll(glass, -1, 1) - glass, dy: roll(glass, -1, 0) - glass,
  X: flat(xs ⍴ [1, -1] + (depth × dx ÷ gap)), Y: flat(ys ⍴ [-1, 1] + (depth × dy ÷ gap)),
  glow: { p, P | exp(-((p ⊗(-) P)^2 ÷ r^2)) },
  matmul(glow(y, Y), transpose(glow(x, X))) × gap^2 ÷ π ÷ r^2
},

; the goal at the rays' points – your word in light, or your face – with as much light
; as the glass lets through
goal: \${
  p: stack(meshgrid(xs, ys), -1),
  cond(
    guard(face(), {
      f: cam(), fh: shape(f)_0, fw: shape(f)_1, s: fh ÷ H ⌊ (fw ÷ W),
      row: clamp(floor(ys - at()_1 × s + (fh ÷ 2)), 0, fh - 1),
      col: clamp(floor(at()_0 - xs × s + (fw ÷ 2)), 0, fw - 1),
      seen: mean((f ⍴ [-1, 3])_(row ⍴ [-1, 1] × fw + col), -1) + 0.01,
      seen ÷ mean(seen)
    }),
    {
      n: length(word()), size: H×0.4 ⌊ (W×0.9 ÷ 0.6 ÷ n),
      ink: clamp(size×0.06 - letters(word(), at() - [0.3×size×n, 0.6×size], size)(p), 0, 1) + 0.05,
      ink ÷ mean(ink)
    }
  )
},

𝓛: { mean((caustic(xs, ys, r) - goal())^2) },
opt: adam(0.1),
{ opt(𝓛) } ⟳ Time()×60,

; the wall at the display's pixels: the light at twice the rays' density, a little sharper
; than ∇ sees it so each ray shows, stretched to the pixels between – a stretch is a
; matrix product too – and colored as sunlight
x2: 0 ..< 2×W÷gap + 0.5 × gap÷2, y2: 0 ..< 2×H÷gap + 0.5 × gap÷2,
tent: { p, P | 1 - abs(p ⊗(-) P ÷ gap÷2) ⌈ 0 },
sun: [[0.09, 0.09, 0.09], [0.55, 0.22, 0.05], [1, 0.7, 0.3], [1, 1, 0.9]],
wall: \${
  canvas(W, H) . paint({ p |
    q: unstack(p, -1), x: q_0_0, y: transpose(q_1)_0,
    matmul(matmul(tent(y, y2), caustic(x2, y2, r×0.7)), transpose(tent(x, x2))) ÷ 5
  }, ramp(sun))
},

(
  Text("# 🔆 Caustics"),
  Text("Light falls through a sheet of glass onto a wall. ∇ reshapes the glass, 60 times a second, until the light it bends writes your word. Type a word, drag the **orange** dot, or tick the box to let the light draw your face from the camera."),
  Grid([1, 3])(Text("**word**"), TextEditor(word)),
  Grid([1, 3])(Text("**📷 your face**"), Checkbox(face)),
  Layers(wall, Point2D(at, [[0, W], [0, H]], "orange")),
  Text("**the glass** – its height where each ray leaves it"),
  glass,
)
`,
"honest-fit": `
; 📈 Honest fit – the curve through your numbers, how sure it is, and when it gets there
; Paste your numbers between the quotes, from any spreadsheet: one column, or x and then y
numbers: "
week	sign-ups
1	2033
2	2312
3	2404
4	2598
5	2717
6	3086
7	3623
8	3785
9	4260
10	4422
11	4847
12	5201
13	5031
14	6385
",
; the shapes to start from: a formula of x and its numbers, and a rough first guess at the
; numbers from x and y – the fit needs only a start in the right neighborhood
rise: { u, v | Σ((u - μ(u)) × (v - μ(v))) ÷ Σ((u - μ(u))^2) }, ; the slope of v against u
straight: ({ x, a, b | a + b×x }, { x, y | [μ(y), rise(x, y)] }),
exponential: ({ x, a, b | a × exp(b×x) }, { x, y | ly: log(abs(y) ⌈ 1e-9), b: rise(x, ly), [exp(μ(ly) - b×μ(x)), b] }),
sCurve: ({ x, top, mid, rate | top ÷ (1 + exp(rate × (mid - x))) }, { x, y |
  [1.2 × max(y), x_(argmin(abs(y - max(y)÷2))), 4 ÷ (max(x) - min(x)) × sign(rise(x, y))]
}),
doseResponse: ({ x, bottom, top, mid, slope | top - bottom ÷ (1 + 10^(slope × (x - mid))) + bottom }, { x, y |
  [min(y), max(y), x_(argmin(abs(y - (min(y) + max(y) ÷ 2)))), -(sign(rise(x, y)))]
}),
powerLaw: ({ x, a, b | a × x^b }, { x, y |
  k: x>0 ∧ y>0, lx: log(mask(x, k)), ly: log(mask(y, k)), b: rise(lx, ly), [exp(μ(ly) - b×μ(lx)), b]
}),
; ⬇ the shape your numbers follow: one of the five above, or your own – a formula of x, and a
; rough guess at its numbers, in order: ({ x, a, b, c | a × exp(b×x) + c }, [5, -0.1, 0])
model: exponential,
goal: 10000, ; a level to ask about: when does the curve reach it? Drag it on the picture

; a text's numbers as a table – a row a line, a column each number in it – and the columns'
; names, from the line above the first row. Columns part at a tab or a semicolon; without
; either, at a space or a comma. Inside a number, a space groups thousands, and so does a comma,
; when the number has a point or every such comma has three digits after it, as in 12,450;
; otherwise a comma is the decimal point, as in 0,875. A line with another count of numbers
; than most lines have – a header, a note – is not a row
readTable: { s |
  c: StringToCodes(s), m: #c,
  ahead: { v, k | concat((drop(v, k), fill([k], 0))) }, behind: { v | concat(([0], drop(v, -1))) },
  digit: c≥48 ∧ c≤57, point: c=46 ∧ ahead(digit, 1), minus: c=45 ∨ (c=8722),
  ; between two digits, where a tab or a semicolon parts the columns: a comma, or a space that
  ; groups thousands, as in 1 204,5
  between: behind(digit) ∧ ahead(digit, 1) ∧ (Σ(c=9 ∨ (c=59)) > 0),
  within: c=44 ∧ between, joins: (c=32 ∨ (c=160) ∨ (c=8239)) ∧ between,
  ; three digits after the comma and the number ends – but 0,875 is never a thousand
  grouped: within ∧ ahead(digit, 2) ∧ ahead(digit, 3) ∧ (¬ahead(digit, 4)) ∧ (¬(behind(c=48) ∧ (¬behind(behind(digit))))),
  comma: within × (Σ(point) = 0 ∧ (Σ(within ∧ (¬grouped)) > 0)),
  mark: point ∨ comma, part: digit ∨ mark ∨ within ∨ joins ∨ minus,
  start: part ∧ (¬behind(part)) ∨ minus, at: mask(0..<m, start), T: #at,
  ; each number's characters, up to 24 from where it starts: [numbers, 24]
  r: at ⊗(+) 0..<24, i: r ⌊ m-1, own: (start scan +)_i - 1 = (0..<T ⍴ [-1, 1]) ∧ part_i ∧ r<m,
  d: own ∧ digit_i, D: TensorScan(d, +, 1), past: d ∧ (TensorScan(own ∧ mark_i, +, 1) > 0),
  ; a number's digits make one whole number, and the digits after its mark divide it: 1,27 is
  ; 127 ÷ 10², as near to 1.27 as the number can be held
  ten: { k | [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000]_(clamp(k, 0, 7)) × 10^(k - 7 ⌈ 0) },
  whole: Σ(where(d, ten(Σ(d, -1, 1) - D) × (c_i - 48), 0), -1),
  v: whole ÷ ten(Σ(past, -1)) × (1 - 2×minus_at),
  keep: Σ(d, -1) > 0, lineOf: (c = 10) scan +, lineAt: mask(lineOf_at, keep),
  count: scatterAdd(lineAt, lineAt×0 + 1, max(lineOf) + 1)_lineAt,
  most: argmax(Σ(count ⊗(=) 0..<25, 0)), row: count = most,
  ; the names: the words on the line above the first row, parted at a tab, else a semicolon,
  ; else a comma, else spaces; a name's spaces before its first word and after its last are not its own
  h: mask(c, lineOf = min(mask(lineAt, row))-1 ∧ (c ≠ 10) ∧ (c ≠ 13)),
  names: cond(guard((#h) = 0, { () }), {
    sep: cond(guard(Σ(h = 9) > 0, { h = 9 }), guard(Σ(h = 59) > 0, { h = 59 }), guard(Σ(h = 44) > 0, { h = 44 }), { h = 32 }),
    field: sep scan +, word: h≠32 ∧ (¬sep), F: max(field),
    inside: (word × (field+1) scan ⌈) = field+1 ∧ (reverse(reverse(word × (F - field + 1)) scan ⌈) = (F - field + 1)),
    ListMap(unstack(0 ..< F+1), { k | CodesToString(mask(h, inside ∧ field=k)) })
  }),
  (mask(mask(v, keep), row) ⍴ [-1, most], names)
},

; what the picture shows: numbers, a shape for them and a level – the program's own, until a
; button below shows others
shown: $((numbers, model, goal)),
read: \${ readTable(shown()_0) }, table: \${ read()_0 }, columns: \${ shape(table())_1 },
; x is the first column – the row's number when there is only one – and each other column is a y at that x
x: \${ k: columns(), cond(guard(k = 1, { 1 ... (#table()) }), { transpose(table())_0 ⧉ (k-1) }) },
y: \${ k: columns(), flat(transpose(table())_((k > 1) ..< k)) },
named: { i, other | n: read()_1, cond(guard((#n) > i, { n_i }), { other }) },
xName: \${ cond(guard(columns() = 1, { "row" }), { named(0, "x") }) },
yName: \${ named(columns() > 1, "y") },

; the fit, and K refits beside it: copies of the formula's numbers in one array, [p, K + 1, 1]
K: 64,
formula: \${ shown()_1_0 },
guess: \${ g: shown()_1_1, cond(guard(isFunction(g), { g(x(), y()) }), { g }) },
; the formula with the numbers θ at the points u: [K + 1, points]
F: { u, θ | (u,) concat unstack(θ) . formula() },
; how each point moves with each number, by one ∇: every point gets its own copy of them
J: { θ | (∇{ T | Σ(F(x(), T)) })(θ + 0×x()) },
; conjugate gradients: as many steps as there are numbers solve A(v) = b, for every copy at once
cg: { A, b | ({ s | r: s_1, d: s_2, Ad: A(d), rr: Σ(r^2, 0, 1), α: rr ÷ (Σ(d × Ad, 0, 1) ⌈ 1e-30), r2: r - α×Ad,
  (s_0 + α×d, r2, r2 + (Σ(r2^2, 0, 1) ÷ (rr ⌈ 1e-30) × d)) } ⍣ (#b))((0×b, b, b))_0 },
; a spring for each point: it pulls in step with its miss up to the slack c, then gives way –
; the further past the slack, the less it pulls – so a misread number can't drag the curve
spring: { r, c | log(1 + (r ÷ c)^2) }, give: { r, c | 1 ÷ √(1 + (r ÷ c)^2) },
springs: { t, θ, c | Σ(spring(t - F(x(), θ), c), -1, 1) },
; Levenberg–Marquardt toward the numbers t: the step that would fix the misses were the formula
; straight, each point weighed by how hard its spring pulls, held back by λ
lm: { θ, λ, t, c, j |
  r: t - F(x(), θ), w: give(r, c),
  jw: j × w, d: √(Σ(jw^2, -1, 1)) ⌈ 1e-30, u: jw ÷ d,
  next: θ + (cg({ v | Σ(u × Σ(u × v, 0, 1), -1, 1) + λ×v }, Σ(u × w×r, -1, 1)) ÷ d),
  better: springs(t, next, c) < springs(t, θ, c),
  (where(better, next, θ), where(better, λ ÷ 3, λ × 4))
},
; how much the fit leans on each point, 0 to 1: the hat matrix's diagonal
lean: { j, w |
  jw: j × w, u: jw ÷ (√(Σ(jw^2, -1, 1)) ⌈ 1e-30), p: #j,
  G: cg({ v | Σ(u × Σ(u × v, 0, 1), -1, 1) }, (0..<p ⊗(=) 0..<p) ⍴ [p, p, 1]),
  Σ(u × Σ(G × moveaxis(u, 0, 1), 1, 1), 0)
},
; how far off a reading typically is, as a bell curve's spread: 1.48 times the middle miss, where
; misses grow with the curve's height h as h to the power γ – the slope of the misses against
; the heights on a log scale, held between 0 and 1
typical: { m, f |
  middle: { v | sort(v)_(floor((#v) ÷ 2)) }, a: abs(m), h: abs(f) ⌈ 0.1×middle(abs(f)),
  γ: clamp(rise(log(h), log(a ⌈ 1e-9×max(a))), 0, 1), s: h^γ,
  1.48 × middle(a ÷ s) × s ⌈ 1e-9×max(abs(y()))
},
; what else your numbers could have been: each miss of the fit falls up or down at random – the
; wild bootstrap. A refit's miss is between the point's own – grown where the fit leans on the
; point – and a typical one, and all grow when there are few points, as a few points understate
; how far off readings can be
flip: \${ sign(⚄([K, #y()]) - 0.5) },
fitStep: { s |
  θ: s_0, f: F(x(), θ)_0, miss: y() - f, j: J(θ), scale: typical(miss, f),
  ; the springs start with almost no slack, so the curve settles first where most points agree
  c: scale × (4 ⌊ 2^(s_2 - 3)), w: give(miss, c),
  own: miss ÷ (1 - (lean(slice(j, [0, 0], [-1, 1]), w) ⌊ 0.99)),
  ν: (#miss) - (#θ), grow: √(ν ÷ (ν - 2 ⌈ 0.5)),
  t: concat((y() ⍴ [1, -1], f + (flip() × grow × √((own^2 + (scale^2 × (#miss) ÷ ν)) ÷ 2)))),
  next: lm(θ, s_1, t, c, j), (next_0, next_1, s_2 + 1)
},
; each copy starts from the guess; a new paste, shape or example starts them all again
fresh: \${ (guess() ⍴ [-1, 1, 1] × fill([1, K + 1, 1], 1), fill([K + 1, 1], 0.01), 0) },
state: $(fresh()),
{ when(state()_2 < 48, { state← (fitStep ⍣ 2)(state()) }) } ⟳ Time()×60,
θ: \${ state()_0 },

; the fit's misses; a point that misses by five typical misses is one the others disagree with
fit: \${ F(x(), θ())_0 },
miss: \${ y() - fit() },
typ: \${ typical(miss(), fit()) },
doubt: \${ abs(miss()) ÷ typ() },
; misses in long runs on one side of the curve, against the runs a fair coin would make: z below
; -2 is a shape wrong for the numbers
pattern: \${
  s: miss()_(argsort(x())) > 0, n: #s, a: Σ(s), b: n - a,
  runs: 1 + Σ(drop(s, 1) ≠ drop(s, -1)), e: 2×a×b ÷ n + 1,
  (runs, e, runs - e ÷ √((e - 1) × (e - 2) ÷ (n - 1) ⌈ 1e-9))
},

; the view: from the first x to as far again as half their span past the last, and the
; numbers and the level with room above and below; M points across it
M: 128,
xs: \${ lo: min(x()), hi: max(x()), [lo, hi + 0.5×(hi - lo)] },
ys: \${ lo: min(y()) ⌊ shown()_2, hi: max(y()) ⌈ shown()_2, s: hi - lo ⌈ 1e-9, [lo - 0.3×s, hi + 0.3×s] },
gx: \${ linspace(xs(), M) },
copies: \${ slice(θ(), [0, 1]) }, ; the K refits' numbers, without the fit's
refits: \${ F(gx(), copies()) }, ; [K, M]
; when each copy reaches a level: the first step of a long grid, 20 spans of the numbers to
; either side, where its curve crosses the level, then two steps of Newton's method inside that
; step – the formula run backwards, its slope by ∇. ∞ where a curve doesn't cross it there
reach: { g |
  span: max(x()) - min(x()), u: linspace([min(x()) - 20×span, max(x()) + 20×span], 2048),
  a: F(u, θ()) - g, s: a ≥ 0, turn: slice(s, [0, 1]) ≠ slice(s, [0, 0], [-1, 2047]),
  i: argmax(turn, -1), lo: u_i ⍴ [-1, 1], hi: u_(i + 1) ⍴ [-1, 1],
  at: { k | flat(a)_(k + 2048×(0..<(#k))) ⍴ [-1, 1] },
  slope: ∇{ t | Σ(F(t, θ())) },
  near: { t | clamp(t - (F(t, θ()) - g ÷ slope(t)), lo, hi) },
  t: near(near(lo + (hi - lo × at(i) ÷ (at(i) - at(i + 1))))),
  where(max(turn, -1) > 0, t ⍴ [-1], 1÷0)
},
; where one more reading would narrow the light most: a reading at x j, as far off as readings
; there typically are, tells the refits' disagreement at x i the share of it that goes along
; with theirs at j – the light narrows there by 1 - √(1 - share), on average across the picture
worth: \${
  dF: refits() - μ(refits(), 0, 1), v: Σ(dF^2, 0) ÷ K + 1e-30,
  C: matmul(transpose(dF), dF) ÷ K,
  hum: typ()_(argmin(abs(gx() ⊗(-) x()), 1))^2,
  μ(1 - √(1 - (C^2 ÷ (v ⍴ [-1, 1]) ÷ (v + hum)) ⌈ 0), 0)
},
best: \${ argmax(worth()) },

; the picture, in units: the numbers' box is L to R across, T to B down
W: 600, H: 330, L: 60, R: 588, T: 14, B: 300,
sx: { v | v - xs()_0 ÷ (xs()_1 - xs()_0) × (R - L) + L },
sy: { v | ys()_1 - v ÷ (ys()_1 - ys()_0) × (B - T) + T },
; the level: drag the orange dot
at: $[R - 14, sy(goal)],
aim: \${ ys()_1 - (at()_1 - T ÷ (B - T) × (ys()_1 - ys()_0)) },
whens: \${ reach(aim()) },
; from each point of the picture, how far each refit passes: [h, K, w] units – straight up or
; down, less where the curve is steep
gaps: { p |
  q: unstack(p, -1), u: q_0_0, v: transpose(q_1)_0,
  Y: sy(F(u - L ÷ (R - L) × (xs()_1 - xs()_0) + xs()_0, copies())),
  slope: roll(Y, -1, 1) - Y ÷ (u_1 - u_0),
  (v ⍴ [-1, 1, 1]) - Y ÷ √(1 + slope^2)
},
; every refit is a thread of light; where they agree, their light adds up
thread: stroke([1, 0.86, 0.62], 1) above halo([0.5, 0.35, 0.2], 2.5),
threads: \${ lit: Σ(thread(unsqueeze(gaps(pixels(canvas(W, H))), -1))_0, 1) ÷ K, canvas(W, H) + 1 - exp(-3×lit) },
; the light of many points at once, however many: a glow around each, exp(-d²/r²), is its glow
; across times its glow down, so the glow of all of them at every pixel is one matrix product
spots: { picture, pts, r |
  D: Density(), h: shape(picture)_0, w: shape(picture)_1,
  glow: { p, P | exp(-((p ⊗(-) P)^2 ÷ r^2)) },
  matmul(glow(0..<h + 0.5 ÷ D, transpose(pts)_1), transpose(glow(0..<w + 0.5 ÷ D, transpose(pts)_0)))
},
; a step from picture to picture: a dot of color c at each point, about r units across, or a glow
marks: { pts, r, c | { picture | k: unsqueeze(clamp(spots(picture, pts, r) - 0.35 × 3, 0, 1), -1), picture × (1 - k) + (light(c) × k) } },
glows: { pts, r, c | { picture | picture + (light(c) × unsqueeze(spots(picture, pts, r), -1)) } },
gray: [0.54, 0.53, 0.51], orange: [1, 0.6, 0.2], green: [0.45, 0.9, 0.55], red: [1, 0.3, 0.25], ink: [0.93, 0.92, 0.88],
; on the light: the numbers, the axes and where to measure next – and over that, the level and
; when each refit reaches it, which alone follow a drag
still: \${
  p: stack((sx(x()), sy(y())), -1), odd: unsqueeze(doubt() > 5, -1), bx: sx(gx()_best()),
  ; the axes span the numbers and are labeled at their ends
  ends: [min(y()), max(y()), min(x()), max(x())], e: [sy(ends_1), sy(ends_0), sx(ends_2), sx(ends_3)],
  threads()
    . draw(line([[L - 8, e_0], [L - 8, e_1]]), gray, 1)
    . draw(line([[e_2, B + 8], [e_3, B + 8]]), gray, 1)
    . draw(line([[bx, T], [bx, B + 8]]), green × 0.3, 1)
    . draw(dots([[bx, B + 8]]), green, 8)
    . glows(where(odd, p, -99), 10, red)
    . marks(p, 3.5, ink)
    . paint(letters(unstack(DrawDigits(ends)), [[2, e_1 - 7], [2, e_0 - 7], [e_2 - 4, B + 14], [e_3 - 20, B + 14]], 12), solid(gray))
    . paint(letters((yName(), xName(), "measure here next"), [[L, T - 8], [0.5×(L + R) - 20, B + 14], [bx + 8 ⌊ (R - 126), B - 16]], 12), solid(ink))
},
wall: \${
  gy: sy(aim()), t: whens()_(1 ..< K+1), on: (t ≥ xs()_0) ∧ (t ≤ xs()_1),
  still()
    . draw(line([[L, gy], [R, gy]]), orange × 0.7, 1)
    . marks(stack((where(on, sx(t), -99), fill([K], gy)), -1), 2.5, orange)
    . paint(letters(unstack(DrawDigits([aim()])), [[L + 4, gy - 19]], 12), solid(gray))
},
; the misses under the curve, each as far up or down as it misses in typical misses: above the
; curve blue, below it orange, in doubt red. Long runs of one color are a shape that doesn't fit
blue: [0.5, 0.72, 1],
strip: \${
  m: clamp(miss() ÷ typ(), -3.5, 3.5), p: stack((sx(x()), 30 - (8×m)), -1), up: unsqueeze(m > 0, -1),
  canvas(W, 62)
    . draw(line([[L, 30], [R, 30]]), gray, 1)
    . marks(where(up, p, -99), 3.5, blue)
    . marks(where(up, -99, p), 3.5, orange)
    . glows(where(unsqueeze(doubt() > 5, -1), p, -99), 8, red)
    . paint(letters("misses", [4, 23], 12), solid(gray))
},

; the words: character codes built on the GPU and read back once, so the page never waits for
; them. A number shows the three digits that matter, padded with spaces that the text shows as
; one; a choice between sentences is a choice between their codes, the shorter padded too
codes: { parts | concat(ListMap(parts, { p | cond(guard(isString(p), { StringToCodes(p) }), { DrawDigits(p) ⍴ [-1] }) })) },
either: { c, a, b | n: (#a) ⌈ (#b), padded: { s | concat((s, fill([n - (#s)], 32))) }, where(c, padded(a), padded(b)) },
; the words wait for the fit to settle
words: { said | cond(guard(state()_2 < 48, { "" }), { CodesToString(said()) }) },
told: \${ words({
  t: sort(whens()_(1 ..< K+1)), lo: t_2, hi: t_(K-3), never: Σ(t = 1÷0),
  either(never > K-3, codes(("**It never reaches** ", aim(), " on a curve of this shape through these numbers.")),
    either(hi = 1÷0, codes(("**It may never reach** ", aim(), " – ", never, " of the ", K, " threads never get there, the rest from ", xName(), " ", lo, " on")),
      codes(("**It reaches** ", aim(), " between ", xName(), " ", lo, " and ", hi, " in 9 threads of 10"))))
}) },
doubtful: \${ words({
  i: argmax(doubt()),
  either(Σ(doubt() > 5) = 0, codes(("",)), codes(("**In doubt:** ", xName(), " ", x()_i, " – your numbers say ", y()_i, " but the others say about ", fit()_i, " so the curve leaves it out")))
}) },
fits: \${ words({
  z: pattern(),
  either(z_2 < -2,
    codes(("**This shape may be wrong for your numbers:** the misses run in long stretches above and below the curve – ", z_0, " runs where about ", z_1, " would be likely. Try another shape.")),
    codes(("The misses fall above and below the curve as if by chance: the shape fits.",)))
}) },
advice: \${ words({ codes(("**Measure here next:** ", xName(), " ", gx()_best(), " – one more reading there narrows the light by about ", round(100 × worth()_best()), " percent")) }) },
ranges: \${ words({
  p: #θ(), f: slice(θ(), [0, 0], [-1, 1]) ⍴ [p], q: transpose(sort(copies() ⍴ [p, K])),
  each: ListMap(unstack(0..<p), { i | (cond(guard(i > 0, { " · " }), { " " }), f_i, " from ", q_2_i, " to ", q_(K-3)_i) }),
  codes(("**The formula's numbers**, and where 9 threads in 10 put them:",) concat (each reduce concat))
}) },

; more numbers to try, each with its shape and a level to ask about
slowing: "
week	sign-ups
1	508
2	795
3	1110
4	1644
5	2508
6	3709
7	4752
8	5731
9	7113
10	8523
11	9897
12	10306
13	10714
14	11157
15	11598
16	11354
",
plate: "
log dose	response	response	response
-0.5	100	101	94
0	100	93	96
0.5	99	98	105
1	90	99	94
1.5	83	82	94
2	63	62	65
2.5	36	29	42
3	19	14	20
",
decay: "
time	volts
0	4.94
10	4.03
20	3.26
30	2.62
40	2.15
50	1.27
60	1.35
70	1.15
80	0.93
90	0.77
100	0.64
110	0.49
120	0.39
",
; a button shows other numbers, or another shape, and starts the fit again
begin: { text, form, g | shown← (text, form, g), state← fresh(), at← [R - 14, sy(g)] },
numbersButton: { label, text, form, g | Button(label, { begin(text, form, g) }) },
shapeButton: { label, form | Button(label, { begin(shown()_0, form, aim()) }) },

(
  Text("# 📈 Honest fit"),
  Text("Paste your numbers into the program, between the quotes of **numbers**. Each thread of light is the curve fitted to them again, with every miss nudged up or down at random: where the threads agree, trust the curve; where they fan out, it is a guess. Drag the **orange** dot to ask when it gets there."),
  Layers(wall, Point2D(at, [[0, W], [0, H]], "orange")),
  strip,
  Text(told),
  Text(doubtful),
  Text(fits),
  Text(advice),
  Text(ranges),
  Grid([2, 3, 4, 3, 2])(Text("**numbers**"), numbersButton("yours", numbers, model, goal), numbersButton("growth that slows", slowing, exponential, 15000), numbersButton("a dose plate", plate, doseResponse, 50), numbersButton("a decay", decay, exponential, 2.5)),
  Grid([2, 2, 3, 2, 3, 2, 2])(Text("**shape**"), shapeButton("straight", straight), shapeButton("exponential", exponential), shapeButton("S-curve", sCurve), shapeButton("dose-response", doseResponse), shapeButton("power", powerLaw), shapeButton("yours", model)),
)
`,
"growing-automata": `
; 🌱 Morphogenesis – a learned local rule grows a global shape from one lit cell

H: 16, W: 16, S: H × W, ; grid
C: 8, ; channels
Hd: 24, ; hidden
N: 20, ; rule steps

cx: 7.5, cy: 7.5, rad: 4.5, ; the target: a filled disk
rr: 0..<H ⊗(+) fill([W], 0), ; row of each cell [H, W]
xs: fill([H], 0) ⊗(+) 0..<W, ; column of each cell [H, W]
dxc: xs - cx, dyc: rr - cy,
targetG: dxc^2 + dyc^2 < rad^2, ; the disk as an [H, W] mask

seedOne: oneHot(H×cy + cx, S), ; one lit cell in the middle …
seed: seedOne ⊗(×) fill([C], 1) ⍴ [H, W, C], ; … all channels on [H, W, C]

W1: ~(randn([4 × C, Hd]) × 0.5), ; the rule – a tiny per-cell MLP
W2: ~(randn([Hd, C]) × 0.01), ; near-zero out ⇒ still self at start

; one synchronous CA tick, [H, W, C] → [H, W, C]
step: { s |
  l: roll(s, 1, 0), r: roll(s, -1, 0), u: roll(s, 1, 1), d: roll(s, -1, 1), ; toroidal shifts
  lap: l + r + u + d - 4×s, ; a Laplacian stencil, per channel
  gx: r - l, gy: d - u, ; Sobel-ish gradients
  per: concat((s ⍴ [S, C], gx ⍴ [S, C], gy ⍴ [S, C], lap ⍴ [S, C]), 1), ; perceive: [S, 4C]
  h: relu(matmul(per, W1)), ; hidden layer
  delta: matmul(h, W2), ; per-cell state delta
  s + (0.2×delta ⍴ [H, W, C]) ; residual update – the cell nudges itself
},

grow: { transpose((step ⍣ N)(seed) ⍴ [S, C])_0 ⍴ [H, W] }, ; run N ticks, read the visible channel
𝓛: { Σ((grow() - targetG)^2) }, ; how far the grown shape is from the disk

opt: adam(0.03), ; optimise the local rule …
grid: \${ grow() }, ; live grown state for the heatmap
{ opt(𝓛) } ⟳ 100000, ; … one step

view: [[-1, 1], [-1, 1]], gres: 192, ; heatmap resolution
idx: ⌊(0..<gres × H ÷ gres), ; nearest-neighbour upsample indices
up: { g | transpose(transpose(g_idx)_idx) }, ; [H, W] → [gres, gres]
field: \${ up(grid()) + 0.18×up(targetG) }, ; grown shape over a faint target ghost

(
  Text("# 🌱 Growing automata"),
  Text("A single lit cell, one **local** update rule, applied 20 times. The rule — a per-cell MLP that sees only its own neighbourhood — gradient-descends until the colony **grows into the target disk** (the faint ghost). Morphogenesis by ∇: a global form emerges from a rule that never sees the whole. FRP × AD in one graph."),
  Layers(field),
)
`,
"optimizer-race": `
; ⚔️ adam vs sgd – same start, same landscape, different characters

𝓛: { p | (p_0^2 + p_1 - 11)^2 + (p_0 + p_1^2 - 7)^2 }, ; Himmelblau

a: ~[0, -0.3], ; adam, orange
s: ~[0, -0.3], ; sgd, blue
; both optimizers are made once, outside the step – sgd(0.002) inside it would
; be a new optimizer, with new state, every iteration
optA: adam(0.08),
optS: sgd(0.002),
𝓛a: { 𝓛(a) },
𝓛s: { 𝓛(s) },
step: { optA(𝓛a), optS(𝓛s) },

n: 300,
view: [[-5, 5], [-5, 5]],
surface: log(linspace(view_1, n) (⊗ { y, x | 𝓛([x, y]) }) linspace(view_0, n) + 1),

(
  Text("# ⚔️ adam vs sgd"),
  Button("Race ×300", { step ⟳ 300 }),
  Layers(
    surface,
    Trail(a, view, "orange"), Trail(s, view, "deepskyblue"),
    Point2D(a, view, "orange"), Point2D(s, view, "deepskyblue"),
  ),
  Grid(2)(Text("**adam**"), Text("**sgd**")),
  Grid(2)(\${ 𝓛(a) }, \${ 𝓛(s) }),
)
`,
"magnets-simulation": `
; Animated Magnetic Field – spinning, pulsing magnets and their field lines

(≻): { t, v | v_0 × (1 - t) + v_1×t },
(≺): { v, t | t - v_0 / (v_1 - v_0) },
rescale: { x | [min(x), max(x)] ≺ x },

numMagnets: 3,
n: 700,

; Grid
xs: linspace([-2, 2], n),
ys: linspace([-1.5, 1.5], n),
X: xs ⍴ [n, 1] tile [1, n],
Y: ys ⍴ [1, n] tile [n, 1],

; Magnet positions using lerp
rawPos: rand([numMagnets, 2]),
posT: transpose(rawPos),
magX: posT_0 ≻ [-1.5, 1.5],
magY: posT_1 ≻ [-1, 1],

; Animated angles
t: Time(),
baseAngles: rand([numMagnets]) × 2×π,
speeds: rand([numMagnets]) ≻ [0.1, 0.5],
angles: baseAngles + t×speeds,

; Pole positions
d: 0.12,
nPoleX: magX + d×cos(angles),
nPoleY: magY + d×sin(angles),
sPoleX: magX - d×cos(angles),
sPoleY: magY - d×sin(angles),

; Reshape for broadcasting
X3: X ⍴ [n, n, 1],
Y3: Y ⍴ [n, n, 1],
nX3: nPoleX ⍴ [1, 1, numMagnets],
nY3: nPoleY ⍴ [1, 1, numMagnets],
sX3: sPoleX ⍴ [1, 1, numMagnets],
sY3: sPoleY ⍴ [1, 1, numMagnets],

; Distances
ε: 0.0001,
rN: √((X3 - nX3)^2 + (Y3 - nY3)^2) + ε,
rS: √((X3 - sX3)^2 + (Y3 - sY3)^2) + ε,

; Potential, with a pulsing strength
k: sin(t × 2) × 0.2 + 0.5,
potential: sum(k/rN - k/rS, 2),

; Visualization
lines: abs(sin(potential × 25))^0.25,

(
  Text("# 🧲 Spinning Magnets"),
  lines,
)
`,
"magnets-minimal": `
; Animated Magnetic Field – minimal
; a magnet is two opposite charges; ⊗ is the meshgrid

(≻): { t, v | v_0 × (1 - t) + v_1×t }, ; lerp

m: 3, ; magnets
n: 600, ; resolution
t: Time(),

angles: rand([m]) × 2×π + t×(rand([m]) ≻ [0.1, 0.5]),
dir: stack(cos(angles), sin(angles)) × 0.12, ; [2, m] headings
pos: rand([2, m]) ≻ [-1.4, 1.4], ; [2, m] centers
pole: concat((pos + dir, pos - dir), 1), ; [2, 2m] all six poles
q: concat(fill([m], 1), fill([m], -1)), ; charge of each pole

grid: linspace([-2, 2], n),
dx: grid (⊗ -) pole_0, ; [n, 2m]
dy: grid (⊗ -) pole_1, ; [n, 2m]
r: √(dx^2 (+ ⊗ 1) dy^2) + 1e-4, ; [n, n, 2m] – frames cross, poles zip

k: sin(t × 2) × 0.2 + 0.5, ; pulsing strength
potential: sum(q×k / r, 2),

(
  Text("# 🧲 Spinning & Pulsating Magnets"),
  abs(sin(potential × 25))^0.25,
)
`,
"mandelbrot": `
; Mandelbrot set – z ← z² + c until |z| escapes
; complex numbers as (re, im) pairs, ⍣ iterates the whole plane at once
; scrub the depth and watch the set sharpen

w: 350, h: 250, ; resolution, 7:5 like the view
depth: $40, ; iterations – drag me

re: linspace([-2.5, 1], w) ⍴ [1, w] tile [h, 1],
im: linspace([-1.25, 1.25], h) ⍴ [h, 1] tile [1, w],

; one step for every pixel: state is (zx, zy, escape count)
step: { s |
  zx: s_0, zy: s_1, k: s_2,
  alive: zx^2 + zy^2 ≤ 4,
  (
    where(alive, zx^2 - zy^2 + re, zx),
    where(alive, 2×zx×zy + im, zy),
    k + alive,
  )
},

zero: zerosLike(re),
render: { d |
  n: 1 ⌈ d,
  counts: (step ⍣ n)((zero, zero, zero))_2,
  √(counts ÷ n)
},

(
  Text("# 🌀 Mandelbrot"),
  Grid([1, 9])(Text("**depth:**"), Scrubber(depth)),
  render(depth),
)
`,
"game-of-life": `
; Conway's Game of Life – each cell lives or dies by counting its 8 neighbours

; grid size
n: 256,

; a cell plus its two neighbours along one axis
box: { g, ax | roll(g, 1, ax) + g + roll(g, -1, ax) },

; the 3×3 sum, minus the cell = its 8 neighbours
neighbours: { g | box(box(g, 0), 1) - g },

; random soup
board: $(rand([n, n]) < 0.3),

step: {
  c: board(),
  a: neighbours(c),

  ; alive next if 3 neighbours, or 2 and already alive
  board← a=3 ∨ (a=2 ∧ c),
},

step ⟳ 100000,

(
  Text("# 🦠 Game of Life"),
  board,
)
`,
  "game-of-life-tacit": `
; Game of Life, tacitly – the whole rule as one point-free function

next: ⊛ ⟜ (1 ⧉ [3, 3]) ⊸ Φ(= ⟜ 3, ∨, = ⟜ (3 ⊸ +)),
board: $(⚄([256, 256]) < 0.3),
{ board← next(board()) } ⟳ 100000,
(
  Text("# 🐦 Game of Life, tacitly"),
  board,
)
`,
  "reaction-diffusion": `
; Gray–Scott reaction–diffusion – two chemicals paint Turing patterns.
; v eats u and reproduces (u × v²); both diffuse; explicit Euler.

n: 200,
edges: { x, ax | roll(x, 1, ax) + roll(x, -1, ax) }, ; both neighbours along an axis
Δ: { x | edges(x, 0) + edges(x, 1) - 4×x }, ; the Laplacian

; sprinkle v into a sea of u, then let it self-organise
u: $1,
v: $0,
reseed: { s: rand([n, n]) < 0.08 × 0.5, u← 1 - s, v← s },

; drag feed and kill – the whole zoo of patterns hides in a tiny window
F: $0.055,
K: $0.062,

tick: {
  a: u(), b: v(), f: F(), k: K(),
  r: a × b^2,
  u← a + 0.16×Δ(a) - r + f - f×a,
  v← b + 0.08×Δ(b) + r - f×b - k×b,
},

reseed(),
tick ⟳ 100000,

(
  Text("# 🌸 Reaction–Diffusion"),
  Grid([1, 6])(Text("**feed**"), Scrubber(F, 0.002)),
  Grid([1, 6])(Text("**kill**"), Scrubber(K, 0.002)),
  Button("reseed", reseed),
  v,
)
`,
  "lenia": `
; Lenia – continuous cellular automata (Bert Chan). A ring kernel convolves the
; world; a bell-curve growth rule nudges each cell. Smooth, life-like blobs emerge.

n: 128,
R: 13,

; a Gaussian ring kernel, normalised to sum 1
ks: 2×R + 1,
gx: 0..<ks ⍴ [ks, 1] tile [1, ks],
gy: 0..<ks ⍴ [1, ks] tile [ks, 1],
dist: hypot(gx - R, gy - R) ÷ R,
ring: exp(-((dist - 0.5)^2) ÷ (2 × 0.15^2)) × (dist < 1),
K: ring ÷ Σ(ring),

; growth: a bell centred at m, width s, mapped to [-1, 1]
m: 0.15,
s: 0.017,
growth: { u | 2 × exp(-((u - m)^2) ÷ (2 × s^2)) - 1 },

world: $0,
reseed: { world← rand([n, n]) },

; convolve, grow, and keep the world in [0, 1]
tick: {
  w: world(),
  world← clamp(w + 0.1×growth(conv(w, K)), 0, 1),
},

reseed(),
tick ⟳ 100000,

(
  Text("# 🔬 Lenia"),
  Button("reseed", reseed),
  world,
)
`,
  "attention": `
; Scaled dot-product attention, built by hand – the core of a transformer.
; scores = Q·Kᵀ / √d, weights = softmax(scores). Drag the temperature knob
; and watch every token's attention go from uniform to one-hot, live.

n: 48, ; tokens
d: 16, ; embedding dimension

; sinusoidal positional embeddings, so nearby tokens are similar (K = Q)
pos: 0 ..< n,
k: 0 ..< d÷2,
freqs: 1 ÷ 10000^(k ÷ d÷2),
angles: pos (⊗ ×) freqs,
Q: concat((sin(angles), cos(angles)), 1),

; every query dotted with every key, scaled
scores: matmul(Q, transpose(Q)) ÷ (√d),

; temperature sharpens (or flattens) the softmax – recomputes as you drag
temp: $4,
weights: \${ softmax(scores × temp()) },

(
  Text("# 🧠 Attention"),
  Grid([1, 6])(Text("**temperature**"), Scrubber(temp, 0.5)),
  Text("who attends to whom:"),
  weights,
)
`,
  "b-type": `
; Turing's B-type unorganised machine (1948) – educated by gradient descent.
; A network of soft NAND-ish gates learns XOR – which isn't in its toolbox,
; so it must compose it (XOR = OR ∧ ¬AND). "Education" is just backprop.

(++): concat,

a: [0, 0, 1, 1],
b: [0, 1, 0, 1],
target: [0, 1, 1, 0],

; 12 two-input boolean functions – AND, OR, NAND, NOT, … but no XOR
basis: { p, q |
  stack((
    p×q, p + q - p×q, 1 - p×q, 1 - (p + q - p×q),
    p×(1 - q), (1 - p)×q, p, q, 1 - p, 1 - q, p×0, p×0 + 1
  ))
},

; each gate's connection-switch: a learnable softmax over the 12 functions
t1: ~(randn([12]) × 0.3),
t2: ~(randn([12]) × 0.3),
to: ~(randn([12]) × 0.3),

; two hidden gates read the inputs; the output gate reads the hidden gates
fwd: {
  h1: matmul(softmax(t1), basis(a, b)),
  h2: matmul(softmax(t2), basis(a, b)),
  matmul(softmax(to), basis(h1, h2))
},
loss: { mean((fwd() - target)^2) },

opt: adam(0.15),
losses: $[],
{ losses(losses() ++ [opt(loss)]) } ⟳ 400,

output: \${ fwd() },
gates: \${ stack((softmax(t1), softmax(t2), softmax(to))) },

(
  Text("# 🎓 B-type learns XOR"),
  Text("**loss** – education in progress:"),
  losses,
  Text("**output** vs target (0, 1, 1, 0):"),
  output,
  Text("the three gates' switches, converging on OR · AND · (p ∧ ¬q):"),
  gates,
)
`,
  "mnist": `
; 🔢 A differentiable logic-gate network learns MNIST – the B-type machine,
; scaled up. 512 → 200 soft boolean gates wired at random over 196 pixels;
; each gate's identity (which of the 16 two-input truth tables it computes) is
; a learnable softmax, educated by gradient descent. No neurons, just logic.

(++): concat,

; --- data: 14×14 binarised MNIST (6000 train, 1000 test) as safetensors ---
; the signal is an empty list until the fetch resolves to [Xtrain, Ytrain, Xtest, Ytest]
mnist: LoadSafeTensorFromURL("mnist.safetensors"),
loaded: { (#mnist()) > 0 },
; train on 3000 of the 6000, score on 500 of the 1000 – keeps each frame light
; so the whole thing trains live and smoothly in the browser (pixels × examples)
Xtr: { transpose(slice(mnist()_0_1, 0, 3000)) }, ; [196, 3000]
Ytr: { transpose(slice(mnist()_1_1, 0, 3000)) }, ; [10, 3000]
Xte: { transpose(slice(mnist()_2_1, 0, 500)) }, ; [196, 500]
Yte: { transpose(slice(mnist()_3_1, 0, 500)) }, ; [10, 500]

; --- fixed random wiring: two source signals per gate ---
G1: 512, G2: 200,
; layer 1 reads pixels
IA1: floor(rand([G1]) × 196),
IB1: floor(rand([G1]) × 196),
; layer 2 reads layer 1
IA2: floor(rand([G2]) × G1),
IB2: floor(rand([G2]) × G1),

; the 16 two-input truth tables: row i is i in binary, [f00, f01, f10, f11]
TT: floor(0..<16 (⊗ ÷) [8, 4, 2, 1]) % 2,
; pool 20 gates per class: GROUP[c, g] = 1 where gate g belongs to class c
GROUP: 0..<10 ⊗(=) floor(0..<G2 ÷ (G2 ÷ 10)),

; a soft gate: expected truth table softmax(θ)·TT, blended over the 4 input corners
gate: { A, B, W |
  Wt: transpose(W),
  reshape(Wt_0, [#A, 1])×((1 - A)×(1 - B))
    + reshape(Wt_1, [#A, 1])×((1 - A)×B)
    + reshape(Wt_2, [#A, 1])×(A×(1 - B))
    + reshape(Wt_3, [#A, 1])×(A×B)
},
sm: { th | matmul(softmax(th), TT) },

; the learnable parameters: each gate's softmax logits over the 16 functions
t1: ~(randn([G1, 16]) × 0.1),
t2: ~(randn([G2, 16]) × 0.1),

model: { X |
  h1: gate(gather(X, IA1), gather(X, IB1), sm(t1)),
  h2: gate(gather(h1, IA2), gather(h1, IB2), sm(t2)),
  matmul(GROUP, h2) ; [10, examples] logits
},

; cross-entropy over the 10 classes (numerically-stable log-softmax on axis 0)
loss: { -mean(Σ(Ytr() × logSoftmax(model(Xtr()), 0), 0)) },

; train 1500 steps once the data has loaded – the budget is 0 until then
opt: adam(0.05),
losses: $[],
budget: \${ loaded() × 1500 },
{ losses(losses() ++ [opt(loss)]) } ⟳ budget,

; live accuracy on the held-out test set
accuracy: \${
  cond(guard(loaded(), {
    hits: argmax(model(Xte()), 0) = argmax(Yte(), 0),
    round(mean(hits) × 1000) ÷ 10
  }), 0)
},

(
  Text("# 🔢 Logic gates learn MNIST"),
  Text("A network of soft boolean gates – no neurons – reads handwritten digits."),
  Text("**test accuracy** (%), on digits it never trained on:"),
  accuracy,
  Text("**training loss**:"),
  losses,
)
`,
  "error-diffusion": `
; 🧠 A brain-plausible network learns MNIST – with no backpropagation.
; Dale's principle: every weight is non-negative, and each neuron is purely
; excitatory or purely inhibitory (four sub-matrices per layer, +p −n / +n −p).
; Credit is assigned by diffusing the output error straight to the hidden
; units – no ∇, no weight transport, no transposed backward pass. Each layer's
; update is a local, Hebbian outer product: ΔW ∝ Aᵀ·(φ'(z)⊙routed-error).
; After "Diffusing Blame", Yamada et al., Sakana AI (arXiv:2606.31700).

(++): concat,

; --- data: 14×14 binarised MNIST as safetensors (an empty list until it loads) ---
mnist: LoadSafeTensorFromURL("mnist.safetensors"),
loaded: { (#mnist()) > 0 },
Xtr: { mnist()_0_1 }, ; [6000, 196] pixels in {0, 1}
Ytr: { mnist()_1_1 }, ; [6000, 10] one-hot labels
Xte: { mnist()_2_1 }, ; [1000, 196]
Yte: { mnist()_3_1 }, ; [1000, 10]

Din: 196, C: 10, H1: 200, H2: 100,
α: 6, lr: 3, B: 128,

; layer-specific sigmoid width α: wide sigmoids keep the derivative alive, or
; the error attenuates ~25× per layer and nothing learns (the paper's key knob).
; φ(z) = 1/(1+e^(-2z/α)); φ'(a) = (2/α)·a·(1-a) – read straight off the activation.
φ: { z | sigmoid(z × (2 ÷ α)) },
dφ: { a | a × (1 - a) × (2 ÷ α) },

; modulo error routing: hidden unit i is wired to output class (i mod C). The
; routing matrix M[i,c] = 1 iff i mod C = c; the hidden error is R = S·Mᵀ.
route: { H | 0..<H % C ⊗(=) 0..<C }, ; [H, C]
M1: route(H1), M2: route(H2),

; asymmetric non-negative init: U[0,1)/√fan-in, excitatory ×1.5, inhibitory ×0.5
; (a 3:1 E/I head start); the output layer stays symmetric to avoid saturation.
ini: { fan, s | rand([fan, s]) ÷ (√fan) },
Wpp1: $(ini(Din, H1) × 1.5), Wnp1: $(ini(Din, H1) × 0.5),
Wnn1: $(ini(Din, H1) × 1.5), Wpn1: $(ini(Din, H1) × 0.5),
Wpp2: $(ini(H1, H2) × 1.5), Wnp2: $(ini(H1, H2) × 0.5),
Wnn2: $(ini(H1, H2) × 1.5), Wpn2: $(ini(H1, H2) × 0.5),
Wppo: $ini(H2, C), Wnpo: $ini(H2, C),
Wnno: $ini(H2, C), Wpno: $ini(H2, C),

; one dual-stream layer: positive stream is excited by p, inhibited by n; the
; negative stream mirrors it. The −signs are structural (Dale), never learned.
fwd: { p, n, Wpp, Wnp, Wnn, Wpn |
  (φ(matmul(p, Wpp) - matmul(n, Wnp)), φ(matmul(n, Wnn) - matmul(p, Wpn)))
},

; the ED weight step: a local outer product of presynaptic activity A and the
; postsynaptic drive U, then clamp ≥ 1e-4 so weights can never turn negative.
edUp: { W, A, U | W + (matmul(transpose(A), U) × (lr ÷ B)) ⌈ 0.0001 },

; readout for scoring: the output positive stream's logit, argmax over classes.
model: { X |
  l1: fwd(X, X, Wpp1(), Wnp1(), Wnn1(), Wpn1()),
  l2: fwd(l1_0, l1_1, Wpp2(), Wnp2(), Wnn2(), Wpn2()),
  matmul(l2_0, Wppo()) - matmul(l2_1, Wnpo()) ; [n, 10]
},

; One training step – a full forward, one diffused error, twelve local updates.
train: {
  idx: floor(rand([B]) × 6000),
  X: Xtr()_idx, Y: Ytr()_idx,
  ; forward, keeping every stream's activation for its local update
  l1: fwd(X, X, Wpp1(), Wnp1(), Wnn1(), Wpn1()),
  p1: l1_0, n1: l1_1,
  l2: fwd(p1, n1, Wpp2(), Wnp2(), Wnn2(), Wpn2()),
  p2: l2_0, n2: l2_1,
  lo: fwd(p2, n2, Wppo(), Wnpo(), Wnno(), Wpno()),
  po: lo_0, no: lo_1,
  ; batch-centred one-vs-all error at the output (prediction = positive stream)
  E: Y - po,
  S: E - mean(E, 0, 1),
  ; the same output error, routed to each hidden layer – no backward pass
  R1: matmul(S, transpose(M1)),
  R2: matmul(S, transpose(M2)),
  ; postsynaptic drives: positive stream chases +R, negative stream chases −R
  Up1: dφ(p1) × R1,
  Un1: dφ(n1) × (-R1),
  Up2: dφ(p2) × R2,
  Un2: dφ(n2) × (-R2),
  Upo: dφ(po) × S,
  Uno: dφ(no) × (-S),
  ; twelve local updates (inhibitory pathways carry the opposite-sign drive)
  Wpp1(edUp(Wpp1(), X, Up1)), Wnp1(edUp(Wnp1(), X, -Up1)),
  Wnn1(edUp(Wnn1(), X, Un1)), Wpn1(edUp(Wpn1(), X, -Un1)),
  Wpp2(edUp(Wpp2(), p1, Up2)), Wnp2(edUp(Wnp2(), n1, -Up2)),
  Wnn2(edUp(Wnn2(), n1, Un2)), Wpn2(edUp(Wpn2(), p1, -Un2)),
  Wppo(edUp(Wppo(), p2, Upo)), Wnpo(edUp(Wnpo(), n2, -Upo)),
  Wnno(edUp(Wnno(), n2, Uno)), Wpno(edUp(Wpno(), p2, -Uno)),
  mean(abs(S)) ; how much blame is left to diffuse
},

; --- the live displays ---
training: $1,
errs: $[],
estride: $2,
{ i |
  when(loaded(), {
    when(training(), {
      s: train(), ; train every frame; log periodically
      when(i % estride() = 0, {
        errs(errs() ++ (s ⍴ [1])),
        when((#errs()) ≥ 480, { ; bound the plot: thin + slow logging
          errs(errs()_(0..<240 × 2)),
          estride← estride() × 2
        })
      })
    })
  })
} ⟳ 1000000,

; test accuracy on held-out digits
accuracy: \${
  cond(guard(loaded(), {
    hits: argmax(model(Xte()), -1) = argmax(Yte(), -1),
    round(mean(hits) × 1000) ÷ 10
  }), 0)
},

; emergent E/I balance: mean excitatory vs inhibitory hidden weight. Starts at
; 3:1 (the init), and learning drives it toward the cortical 1:1 on its own.
eibal: \${
  mean(Wpp1()) + mean(Wnn1()) + mean(Wpp2()) + mean(Wnn2())
    ÷ (mean(Wnp1()) + mean(Wpn1()) + mean(Wnp2()) + mean(Wpn2()))
},

; look inside: the ten class "amplifiers" – for each class, the mean excitatory
; input field of its 20 routed hidden units, reshaped to 14×14 and laid in a row.
amp: \${
  m: mean(reshape(Wpp1(), [Din, 20, C]), 1), ; [196, 10] per-class excitatory field
  ; the ten 14×14 tiles side by side: a 14×140 filmstrip
  concat(unstack(transpose(m) ⍴ [C, 14, 14]), 1)
},

(
  Text("# 🧠 Learning MNIST without backprop"),
  Text("A dual-stream, Dale-constrained network — every weight non-negative — trained by **Error Diffusion**: the output error is routed straight to the hidden units, no backward pass. After *Diffusing Blame* (Sakana AI)."),
  Checkbox(training),
  Text("**training** — uncheck to pause"),
  Text("**test accuracy** (%), on digits it never trained on — climbing with no ∇:"),
  accuracy,
  Text("**the blame, diffusing** — mean output error, shrinking as credit finds its way home:"),
  errs,
  Text("**excitatory / inhibitory balance** — starts at 3:1, self-organises toward the cortical 1:1:"),
  eibal,
  Text("**the ten class amplifiers** — each excitatory stream's mean input field, learning a digit template live:"),
  amp,
)
`,
  "dreamer": `
; 🔮 The Name Dreamer – a 12k-parameter transformer invents names, live.
; The training data follows the corpus: pausing, editing the text and resuming
; re-trains on your data without resetting the weights.

(++): concat,

; --- vocabulary: ' ' → 0, a…z → 1…26 (anything else collapses to space) ---
V: 27, T: 8, C: 32, F: 96,
enc: { s | clamp(StringToCodes(s) - 96, 0, 26) },
dec: { t | CodesToString(where(t = 0, 32, t + 96)) },

; --- the entire dataset is this editable string ---
corpus: $"emma olivia ava isabella sophia charlotte mia amelia harper evelyn abigail emily elizabeth mila ella avery sofia camila aria scarlett victoria madison luna grace chloe penelope layla riley zoey nora lily eleanor hannah lillian addison aubrey ellie stella natalie zoe leah hazel violet aurora savannah audrey brooklyn bella claire skylar lucy paisley everly anna caroline nova genesis emilia kennedy samantha maya willow kinsley naomi aaliyah elena sarah ariana allison gabriella alice madelyn cora ruby eva serenity autumn adeline hailey gianna valentina isla eliana quinn ivy sadie piper lydia alexa josephine emery julia delilah arianna vivian kaylee sophie brielle madeline",
load: { c | window(enc(c), [T + 1]) }, ; every 9-char window is an example
D: \${ load(corpus()) }, ; follows corpus edits – the weights stay

; --- the transformer: one pre-norm block, one head ---
E: ~(randn([V, C]) × 0.1),
P: ~(randn([T, C]) × 0.1),
Wq: ~(randn([C, C]) × 0.2),
Wk: ~(randn([C, C]) × 0.2),
Wv: ~(randn([C, C]) × 0.2),
Wo: ~(randn([C, C]) × 0.2),
W1: ~(randn([C, F]) × 0.2),
W2: ~(randn([F, C]) × 0.1),
Wu: ~(randn([C, V]) × 0.1),

rms: { x | x ÷ √(mean(x × x, -1, 1) + 0.001) }, ; the 1 keeps the reduced axis
quickGelu: { x | x × sigmoid(x × 1.702) }, ; GELU, through a sigmoid
nmask: where(0..<T ⊗(≥) 0..<T, fill([T, T], 0), fill([T, T], -9999)),
att: { x |
  q: matmul(x, Wq),
  k: matmul(x, Wk),
  v: matmul(x, Wv),
  a: softmax(einsum("btc,bsc->bts", q, k) ÷ (√C) + nmask),
  matmul(matmul(a, v), Wo)
},
model: { ids |
  x0: matmul(oneHot(ids, V), E) + P,
  x1: x0 + att(rms(x0)),
  x2: x1 + matmul(quickGelu(matmul(rms(x1), W1)), W2),
  matmul(rms(x2), Wu) ; [batch, T, V] logits
},

; --- what it learns from: B random windows a step; a tenth held out to score it ---
B: 64,
cut: { floor((#D) × 0.9) }, ; windows past the cut are held out – never trained on
lossOn: { d | crossEntropy(slice(d, [0, 1], [-1, T]), model(slice(d, [0, 0], [-1, T]))) },
loss: { lossOn(D_floor(⚄([B]) × cut())) },
; a score: 256 windows spread evenly over n windows from from – steady, at a bounded cost
score: { from, n | lossOn(D_(from + floor(linspace([0, n - 1], 256)))) },

; --- sampling: gumbel-max, with a temperature you can drag ---
τ: $0.5,
pick: { l | argmax(l ÷ (τ() × 1.5 ⌈ 0.05) - log(-log(rand([V]))), -1) },
; the likeliest letter: no dice, so a live value can use it
best: { l | argmax(l, -1) },
grow: { choose | { s |
  ctx: slice(s, [-T]),
  l: model(ctx ⍴ [1, T]) ⍴ [T, V] _ T-1,
  s ++ (choose(l) ⍴ [1])
} },
gen: { seed, n, choose | (grow(choose) ⍣ n)(seed) },
dream: $"### …one moment, it is waking up…",
dreamNow: { dream(StringConcat("### ", dec(slice(gen(fill([T], 0), 48, pick), T)))) },

; --- training: ⟳-paced, gated by the checkbox ---
training: $1,
opt: adamw(0.03, 0.001), ; a little weight decay – memorize less, invent more
step: $0,
; a row each time it measures: (step, training score, held-out score)
history: $fill([0, 3], 0),
stride: $20, ; steps between measurements
{
  when(training(), {
    opt(loss),
    step← step() + 1,
    s: step(),
    when(s % stride() = 0, {
      measured: [s, score(0, cut()), score(cut(), (#D) - cut())],
      history(concat((history(), measured ⍴ [1, 3]))),
      ; the plot keeps the whole story, bounded: when it fills up,
      ; thin the history to every other row and measure half as often
      when((#history()) ≥ 240, {
        history(history()_(0..<120 × 2)),
        stride← stride() × 2
      })
    }),
    when(s % 200 = 0, dreamNow)
  })
} ⟳ 1000000,
curves: \${ h: transpose(history()), plot(h_0, (h_1, h_2), ("train", "held out")) },

; type a beginning – the model finishes it, the likeliest way (works even while paused)
prompt: $"kri",
finish: \${
  p: prompt(),
  s: fill([T], 0) ++ enc(p),
  StringConcat("## ", p, "·", dec(slice(gen(s, 10, best), #s)))
},

; what the head attends to while reading a name, live
probeS: fill([T], 0) ++ enc("sophia"),
probe: slice(probeS, [-T]),
attn: \${
  x: rms(matmul(oneHot(probe ⍴ [1, T], V), E) + P),
  softmax(einsum("btc,bsc->bts", matmul(x, Wq), matmul(x, Wk)) ÷ (√C) + nmask) ⍴ [T, T]
},

editable: \${ 1 - training() },
(
  Text("# 🔮 The Name Dreamer"),
  Text("A 12k-parameter transformer learns to invent names – live, in your tab."),
  Checkbox(training),
  Text("**training** – uncheck to pause; pause to edit the corpus below"),
  Text("**loss** – on windows it trains on, and on the tenth it never sees; when held out climbs as train falls, it is memorizing:"),
  curves,
  Text("**it dreams** – regenerated every 200 steps:"),
  Text(dream),
  Button("dream again", dreamNow),
  Text("**finish my name:**"),
  TextEditor(prompt),
  Text(finish),
  Text("**temperature:**"),
  Slider(τ),
  Text("**attention** while it reads “sophia” (row attends to column):"),
  attn,
  Text("**the letter embeddings**, learning:"),
  E,
  Text("**the training data** – uncheck training to edit; resuming keeps the weights:"),
  TextEditor(corpus, editable),
)
`,
  "looped-dreamer": `
; 🔁 The Looped Dreamer – a transformer that thinks in rounds.
; Two physical blocks (b1, b2), stored once, are revisited R=3 times, so the
; unrolled network is N = K·R = 6 blocks deep – for a third of the weights.
; Every sublayer visit is post-norm, x ← rms(α·x + f(x)), with the DeepLoop
; scaling rule (p = ½): skip α = (2N)^½, per-matrix init gain β = (8N)^-½ –
; strong skips and gentle starts keep 12 sublayer visits stable.

(++): concat,

; --- vocabulary: ' ' → 0, a…z → 1…26 (anything else collapses to space) ---
V: 27, T: 8, C: 32, F: 96,
K: 2, R: 3, N: K × R, ; physical blocks × rounds = unrolled depth
α: √(2 × N), β: 1 ÷ √(8 × N), ; the scaling rule
enc: { s | clamp(StringToCodes(s) - 96, 0, 26) },
dec: { t | CodesToString(where(t = 0, 32, t + 96)) },

; --- the entire dataset is this editable string ---
corpus: $"emma olivia ava isabella sophia charlotte mia amelia harper evelyn abigail emily elizabeth mila ella avery sofia camila aria scarlett victoria madison luna grace chloe penelope layla riley zoey nora lily eleanor hannah lillian addison aubrey ellie stella natalie zoe leah hazel violet aurora savannah audrey brooklyn bella claire skylar lucy paisley everly anna caroline nova genesis emilia kennedy samantha maya willow kinsley naomi aaliyah elena sarah ariana allison gabriella alice madelyn cora ruby eva serenity autumn adeline hailey gianna valentina isla eliana quinn ivy sadie piper lydia alexa josephine emery julia delilah arianna vivian kaylee sophie brielle madeline",
load: { c | window(enc(c), [T + 1]) }, ; every 9-char window is an example
D: \${ load(corpus()) }, ; follows corpus edits – the weights stay

; --- K=2 physical blocks: this is all the depth there is ---
E: ~(randn([V, C]) × 0.1),
P: ~(randn([T, C]) × 0.1),
Wu: ~(randn([C, V]) × 0.1),
Wq1: ~(randn([C, C]) × β),
Wk1: ~(randn([C, C]) × β),
Wv1: ~(randn([C, C]) × β),
Wo1: ~(randn([C, C]) × β),
Wf1: ~(randn([C, F]) × β),
Wg1: ~(randn([F, C]) × β),
Wq2: ~(randn([C, C]) × β),
Wk2: ~(randn([C, C]) × β),
Wv2: ~(randn([C, C]) × β),
Wo2: ~(randn([C, C]) × β),
Wf2: ~(randn([C, F]) × β),
Wg2: ~(randn([F, C]) × β),

rms: { x | x ÷ √(mean(x × x, -1, 1) + 0.001) }, ; the 1 keeps the reduced axis
quickGelu: { x | x × sigmoid(x × 1.702) }, ; GELU, through a sigmoid
nmask: where(0..<T ⊗(≥) 0..<T, fill([T, T], 0), fill([T, T], -9999)),
att: { x, Wq, Wk, Wv, Wo |
  q: matmul(x, Wq),
  k: matmul(x, Wk),
  v: matmul(x, Wv),
  a: softmax(einsum("btc,bsc->bts", q, k) ÷ (√C) + nmask),
  matmul(matmul(a, v), Wo)
},
b1: { x |
  h: rms(x×α + att(x, Wq1, Wk1, Wv1, Wo1)),
  rms(h×α + matmul(quickGelu(matmul(h, Wf1)), Wg1))
},
b2: { x |
  h: rms(x×α + att(x, Wq2, Wk2, Wv2, Wo2)),
  rms(h×α + matmul(quickGelu(matmul(h, Wf2)), Wg2))
},
cycle: { x | b2(b1(x)) }, ; one round: the whole stack, once

embed: { ids | matmul(oneHot(ids, V), E) + P },
model: { ids | matmul(cycle(cycle(cycle(embed(ids)))), Wu) }, ; training unrolls R=3 rounds
modelAt: { ids, r | matmul((cycle ⍣ r)(embed(ids)), Wu) }, ; dreaming picks its own r

; --- what it learns from: B random windows a step; a tenth held out to score it ---
B: 64,
cut: { floor((#D) × 0.9) }, ; windows past the cut are held out – never trained on
lossOn: { d | crossEntropy(slice(d, [0, 1], [-1, T]), model(slice(d, [0, 0], [-1, T]))) },
loss: { lossOn(D_floor(⚄([B]) × cut())) },
; a score: 256 windows spread evenly over n windows from from – steady, at a bounded cost
score: { from, n | lossOn(D_(from + floor(linspace([0, n - 1], 256)))) },

; --- sampling: gumbel-max, with temperature and rounds you can drag ---
τ: $0.5,
rounds: $0.4, ; 1 + 0.4×5 = the 3 rounds it is trained at
pick: { l | argmax(l ÷ (τ() × 1.5 ⌈ 0.05) - log(-log(rand([V]))), -1) },
; the likeliest letter: no dice, so a live value can use it
best: { l | argmax(l, -1) },
grow: { choose | { s |
  ctx: slice(s, [-T]),
  l: modelAt(ctx ⍴ [1, T], 1 + rounds()×5) ⍴ [T, V] _ T-1,
  s ++ (choose(l) ⍴ [1])
} },
gen: { seed, n, choose | (grow(choose) ⍣ n)(seed) },
dream: $"### …one moment, it is waking up…",
dreamNow: { dream(StringConcat("### ", dec(slice(gen(fill([T], 0), 48, pick), T)))) },

; --- training: ⟳-paced, gated by the checkbox ---
training: $1,
; post-norm runs hotter than pre-norm: a cooler lr, and no
; weight decay – rms-normalized blocks shrink until they blow up
opt: adamw(0.01, 0),
step: $0,
; a row each time it measures: (step, training score, held-out score)
history: $fill([0, 3], 0),
stride: $20, ; steps between measurements
{
  when(training(), {
    opt(loss),
    step← step() + 1,
    s: step(),
    when(s % stride() = 0, {
      measured: [s, score(0, cut()), score(cut(), (#D) - cut())],
      history(concat((history(), measured ⍴ [1, 3]))),
      ; the plot keeps the whole story, bounded: when it fills up,
      ; thin the history to every other row and measure half as often
      when((#history()) ≥ 240, {
        history(history()_(0..<120 × 2)),
        stride← stride() × 2
      })
    }),
    when(s % 200 = 0, dreamNow)
  })
} ⟳ 1000000,
curves: \${ h: transpose(history()), plot(h_0, (h_1, h_2), ("train", "held out")) },

; type a beginning – the model finishes it, the likeliest way (works even while paused)
prompt: $"kri",
finish: \${
  p: prompt(),
  s: fill([T], 0) ++ enc(p),
  StringConcat("## ", p, "·", dec(slice(gen(s, 10, best), #s)))
},

; the same block 1, watched in round 1 and in round 3: shared weights, different work
probeS: fill([T], 0) ++ enc("sophia"),
probe: slice(probeS, [-T]),
attProbe: { x |
  softmax(einsum("btc,bsc->bts", matmul(x, Wq1), matmul(x, Wk1)) ÷ (√C) + nmask) ⍴ [T, T]
},
attn1: \${ attProbe(embed(probe ⍴ [1, T])) },
attn3: \${ attProbe(cycle(cycle(embed(probe ⍴ [1, T])))) },

editable: \${ 1 - training() },
(
  Text("# 🔁 The Looped Dreamer"),
  Text("Two transformer blocks, stored once, revisited three times – a 6-block-deep name dreamer for a third of the weights (~22k parameters)."),
  Checkbox(training),
  Text("**training** – uncheck to pause; pause to edit the corpus below"),
  Text("**loss** – on windows it trains on, and on the tenth it never sees; when held out climbs as train falls, it is memorizing:"),
  curves,
  Text("**it dreams** – regenerated every 200 steps:"),
  Text(dream),
  Button("dream again", dreamNow),
  Text("**rounds of thought** while dreaming – 1 on the left, 6 on the right; it trained at 3:"),
  Slider(rounds),
  Text("**finish my name:**"),
  TextEditor(prompt),
  Text(finish),
  Text("**temperature:**"),
  Slider(τ),
  Text("**block 1's attention** reading “sophia” – round 1 beside round 3, on one scale (row attends to column). Same weights, different work:"),
  \${ stack((attn1(), attn3())) },
  Text("**the training data** – uncheck training to edit; resuming keeps the weights:"),
  TextEditor(corpus, editable),
)
`,
  "makemore": `
; makemore, the nanoGPT way – a small transformer learns 32,033 names and makes up
; new ones, with what a real training run has: a dataset fetched and split, random
; minibatches, AdamW, warmup and cosine decay, gradient clipping, evaluation, a log,
; a checkpoint that survives a reload, sampling and a clock. A program draws the
; same random numbers each time it runs, so a run from step 0 repeats exactly.

(++): concat,

; --- config ---
run: "names", ; the checkpoint's name – rename it to start a fresh run
B: 64, T: 16, ; windows a step, characters a window
V: 27, C: 64, F: 256, ; vocabulary, width, hidden width
steps: 4000, warm: 200, peak: 0.003, ; the learning rate's schedule
every: 100, ; steps between evaluations, checkpoints and samples

; --- data: makemore's names.txt, a name a line, from the most common to the rarest ---
text: Fetch("names.txt"), ; ◌ until it arrives
loaded: { cond(guard(isString(text()), { (#text()) > 0 }), 0) },
; a character as a token: a newline, which ends a name, is 0; a to z are 1 to 26
encode: { s | clamp(StringToCodes(s) - 96, 0, 26) },
decode: { t | CodesToString(where(t = 0, 32, t + 96)) },
; every tenth name is held out – never trained on, only scored: once the text has
; arrived, its two token streams
streams: \${ when(loaded(), {
  ids: encode(text()),
  name: (ids = 0) scan +, ; which name each character belongs to
  held: name % 10 = 9,
  (mask(ids, ¬held), mask(ids, held))
}) },
; a window is T+1 characters in a row: each of the first T is followed by the next
train: { window(streams()_0, [T + 1]) },
val: { window(streams()_1, [T + 1]) },
batch: { d | d_floor(⚄([B]) × (#d)) }, ; B windows of d, at random

; --- the model: one transformer block, as in nanoGPT, its weights drawn small ---
E: ~(randn([V, C]) × 0.02), ; a vector for each token
P: ~(randn([T, C]) × 0.02), ; and for each position
Wq: ~(randn([C, C]) × 0.02),
Wk: ~(randn([C, C]) × 0.02),
Wv: ~(randn([C, C]) × 0.02),
Wo: ~(randn([C, C]) × 0.02),
W1: ~(randn([C, F]) × 0.02),
W2: ~(randn([F, C]) × 0.02),
Wu: ~(randn([C, V]) × 0.02),
g1: ~fill([C], 1), g2: ~fill([C], 1), g3: ~fill([C], 1), ; the norms' gains

rms: { x, g | x ÷ √(mean(x × x, -1, 1) + 1e-5) × g },
causal: where(0..<T ⊗(≥) 0..<T, 0, -1e9), ; each position sees itself and before
attend: { x |
  q: matmul(x, Wq), k: matmul(x, Wk), v: matmul(x, Wv),
  a: softmax(matmul(q, permute(k, [0, 2, 1])) ÷ √(C) + causal),
  matmul(matmul(a, v), Wo)
},
model: { ids |
  x0: E_ids + P,
  x1: x0 + attend(rms(x0, g1)),
  x2: x1 + matmul(gelu(matmul(rms(x1, g2), W1)), W2),
  matmul(rms(x2, g3), Wu) ; [windows, T, V] scores for each next character
},
lossOn: { d | crossEntropy(slice(d, [0, 1], [-1, T]), model(slice(d, [0, 0], [-1, T]))) },
loss: { lossOn(batch(train())) },
; the loss on 1024 windows spread evenly over a split: steady, at a bounded cost
score: { d | lossOn(d_floor(linspace([0, (#d) - 1], 1024))) },

; --- optimization: AdamW, decaying only the matrices; warmup, cosine decay; clipping ---
params: (E, P, Wq, Wk, Wv, Wo, W1, W2, Wu, g1, g2, g3),
decay: { v | 0.1 × ((#shape(v)) ≥ 2) }, ; the gains don't decay
; up to peak in warm steps, then down along a cosine to a tenth of it
schedule: { t | warmup(1, warm)(t) × (0.1×peak + cosine(0.9×peak, steps)(t)) },
; each step's gradients scaled to a norm of at most 1
rule: clipNorm({ g, v, s, t, lr | adamRule(decay(v))(g, v, s, t, lr) }, 1),
opt: optimizer(adamInit, rule, schedule, params),

; --- the run: everything a checkpoint keeps ---
step: ~~0,
history: ~~fill([0, 3], 0), ; a row an evaluation: (step, train loss, val loss)
state: ListConcat((step, history), params, opt()), ; opt() is the optimizer's memory
; kept in this browser, under the run's name and the model's shapes
checkpoint: Storage(StringConcat(run, String(ListMap(params, shape)))),

; --- sampling: a character at a time, from the model's softmax at temperature τ ---
τ: $0.8,
next: { s |
  l: model(slice(s, [-T]) ⍴ [1, T]) ⍴ [T, V] _ T-1, ; scores for the next character
  ; the Gumbel trick: the largest of l ÷ τ plus noise is a draw from softmax(l ÷ τ)
  s ++ (argmax(l ÷ (τ() ⌈ 0.05) - log(-log(⚄([V])))) ⍴ [1])
},
samples: $"",
sample: { samples(decode(slice((next ⍣ 64)(fill([T], 0)), [T]))) },

; --- training ---
training: $1,
clock: Time(),
since: $0,
speed: $0, ; tokens a second, over the steps since the last evaluation
{
  ; resume: a checkpoint ahead of this run – one read from storage – becomes the run
  saved: checkpoint(),
  when(isList(saved), { when(saved_0 > step, { state := saved }) }),
  when(training() ∧ isList(streams()) ∧ step<steps, {
    when(step % every = 0, { since← clock() }),
    opt(loss),
    step:= step + 1,
    when(step % every = 0, {
      speed← round(B × T × every ÷ (clock() - since())),
      history:= concat((history, [[step, score(train()), score(val())]])),
      checkpoint(state),
      sample()
    })
  })
} ⟳ 1000000,

; --- what it shows ---
about: \${ cond(
  guard(isError(text()), { text() }),
  guard(isList(streams()), {
    StringConcat(String(#streams()_0), " characters to train on, ", String(#streams()_1), " held out")
  }),
  { "fetching names.txt…" }
) },
curves: \${ h: transpose(history), plot(h_0, (h_1, h_2), ("train", "val")) },
rate: \${ schedule(step) },
(
  Text("# makemore, the nanoGPT way"),
  Text("A one-block transformer learns 32,033 names and makes up new ones. Reload the page: training resumes from its last checkpoint."),
  Text(about),
  Grid(3)(Text("**step**"), Text("**tokens a second**"), Text("**learning rate**"), step, speed, rate),
  Grid([1, 6])(Checkbox(training), Text("**training** – uncheck to pause")),
  Text("**loss** every 100 steps, on names it trains on and on the tenth it never sees:"),
  curves,
  Text("**new names**, sampled at each evaluation:"),
  Text(samples),
  Grid([1, 6])(Text("**temperature**"), Slider(τ)),
  Button("more names", sample),
)
`,
  "camera-edges": `
; 📷 Live edge detection – your camera through a Laplacian kernel.
; Allow camera access, then wave: flat areas cancel out, edges glow.

cam: Camera(320, 240),
k: [[0,  1, 0],
    [1, -4, 1],
    [0,  1, 0]],

; every frame: grayscale the image (mean over the color axis), convolve
edges: \${ abs(conv(mean(cam(), 2), k)) },

(
  Text("# 📷 Live Edges"),
  edges,
)
`,
  "spectrum": `
; Live spectrum analyser – FFT the microphone waveform, plot the magnitude.
; Allow mic access, then whistle or play a tone and watch the peak move.

mic: Microphone(512),

; every frame: take the FFT of the waveform, then its magnitude √(re² + im²).
; fft returns [real; imag] stacked, so f_0 is real and f_1 is imaginary.
spectrum: \${ f: fft(mic()), √(f_0^2 + f_1^2) },

(
  Text("# 🎵 Live Spectrum"),
  spectrum,
)
`,
  "pitch-detector": `
; Pitch detector – FFT the mic, find the loudest note. Allow mic access,
; then whistle or hum a steady tone and watch the note lock on.

size: 8192,
sr: SampleRate(),
mic: Microphone(size),
names: ("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"),

detect: \${
  f: fft(mic()),
  mag: √(f_0^2 + f_1^2),
  freqs: 0 ..< (#mag) × sr ÷ size,
  ; only the musical range, so mic rumble and hiss don't win
  band: mag × (freqs > 55) × (freqs < 2000),
  ; silence has no pitch: the loudest bin must stand out of the noise
  cond(guard(max(band) < 1, { ("–", "–", "–") }), {
    freq: freqs_argmax(band),
    midi: round(69 + 12×log2(freq ÷ 440)),
    (
      names_(midi % 12),
      floor(midi ÷ 12) - 1,
      round(freq),
    )
  })
},

(
  Text("# 🎸 Pitch Detector"),
  Text("**note · octave · Hz**"),
  detect,
)
`,
  "recursion": `
; Recursion – two ways.

; Named – a binding is visible inside its own body, so it just calls its
; own name. This is the everyday way to recurse:
fact: { n | cond(guard(n = 0, { 1 }), { n × fact(n-1) }) },
fib: { n | cond(guard(n < 2, { n }), { fib(n-1) + fib(n-2) }) },

(
  fact(5), ; 120
  fib(10), ; 55

  ; Anonymous – a nameless function has no name to call, so 'fix' (the
  ; Y combinator, also 𝕐) hands it one: fix({ f | … f … }) ties the knot
  ; with no binding in sight. Euclid's gcd, defined and applied on the
  ; spot without ever being named:
  fix({ gcd |
    { a, b | cond(guard(b = 0, { a }), { gcd(b, a % b) }) }
  })(48, 36), ; 12
)
`

} as const
