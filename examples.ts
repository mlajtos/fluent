// Gallery examples: Fluent source, one per key. Extracted from client.tsx so
// both the IDE and the test suite can import the same corpus (tests.ts runs
// them headless as a smoke suite).

export const EXAMPLES = {
  "no-precedence": `
; Left-to-right evaluation, no operator precedence
1 + 2 * 3 - 4 / 5 ^ sin(2)
`,
  "whitespace-precedence": `
; Whitespace IS precedence
(
    1 + 2 * 3,       ; spaced: left-to-right = 9
    1 + 2*3,         ; glued * binds first = 7
    a: 1 + 2,        ; glued ':' takes the whole right side
    θ: [3, 4],
    √(θ_0^2 + θ_1^2) ; tight chains: (θ_0)^2 + (θ_1)^2
)
`,
  "infix-lambda": `
; Using lambda as infix operator
1 {x,y | x+y} 2
`,
  "broadcasting": `
; Tensor broadcasting - scalar added to vector
[1,2,3] + 1
`,
  "vector-addition": `
; Element-wise vector addition
a:[1,2,3], b:[4,5,6], a+b
`,
  "calling-conventions": `
; Multiple ways to call functions
(
    1 + 2,
    1 add 2,
    add(1,2),
    +(1,2),
    (1,2) . +,
    (1,2) apply add,
)
`,
  "max-variations": `
; max/min reduce a tensor; ⌈/⌊ are APL ceiling/floor (one arg), max/min (two)
(
    max([3, 1, 2]),            ; 3 – the largest element
    max([[1, 2], [3, 4]], 0),  ; [3, 4] – reduce along an axis, like sum
    ⌈(2.3),                    ; 3 – ceiling
    ⌊(2.7),                    ; 2 – floor
    2 ⌈ 3,                     ; 3 – pairwise max
    [1, 5] ⌈ [4, 2]            ; [4, 5] – element-wise max
)
`,
  "gradient": `
; Automatic differentiation with ∇
f : { x | x ^ 2 },
g : ∇(f),
x : (1 ..< 10),
(f(x), g(x))
`,
  "ad-hoc-operators": `
; Defining custom operators
(++): ListConcat,
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
; ⍤ Rank — Iverson's rank conjunction. \`f ⍤ k\` applies f to each rank-k CELL of
; its argument and maps over the leading FRAME. Write a function once, run it at
; any rank: the SAME code re-scopes itself. (Dyadic \`a (f ⍤ k) b\` zips cells,
; a lone cell broadcasting over the frame.)

M: [[1, 4, 2, 8], [5, 3, 9, 1], [7, 2, 6, 4], [3, 8, 1, 5]],
nrm: { x | (x - min(x)) ÷ (max(x) - min(x)) },   ; scales an array to 0..1

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
; ∙ Inner product — matrix multiply is just \`+ ∙ ×\`: reduce with +, combine with ×.
; Keep the contraction, swap the RING, and the SAME operation becomes something
; else. One combinator, every semiring — arithmetic, shortest paths, reachability.

A: [[1, 2], [3, 4]],
B: [[5, 6], [7, 8]],

; a weighted graph — 999 stands in for "no edge"
W: [[0, 3, 999, 999], [3, 0, 1, 999], [999, 1, 0, 2], [999, 999, 2, 0]],
hop: { M | M (⌊ ∙ +) W },        ; one min-plus relaxation = allow one more edge

(
  Text("# ∙ One product, every semiring"),
  Text("**A (+ ∙ ×) B** — ordinary matrix multiply (reduce +, combine ×):"),
  A (+ ∙ ×) B,
  Text("**edge weights** W (999 = no edge):"),
  W,
  Text("**W (⌊ ∙ +) W** — swap to min-plus: one round of shortest-path relaxation:"),
  W (⌊ ∙ +) W,
  Text("**all-pairs shortest paths** — iterate min-plus with ⍣ until it converges:"),
  (hop ⍣ 3)(W),
)
`,
  "lerp": `
; Linear interpolation with custom operators
; lerp: 0.5 ≻ [10, 30] = 20
≻ : { t, v | (v_0 × (1 - t)) + (v_1 × t) },

; invlerp: [1, 3] ≺ 2 = 0.5
≺ : { v, t | (t - (v_0)) ÷ ((v_1) - (v_0)) },

normalize : { x | [min(x), max(x)] ≺ x },
[10, 20, 30] . normalize
`,
  "reactive-slider": `
; Reactive slider with computed value
(
  x: $(0.5),
  Slider(x),
  x ^ 2
)
`,
  "reactive-function-plot": `
; Interactive sine wave - adjust resolution with slider

PI: 3.14159,
resolution: $(0.5),
points: (resolution × 90 + 10),
x: linspace([-PI, PI], points),

(Slider(resolution), sin(x))
`,
  "tasks": `
; TODO App - reactive task management

; Helper operators
(++): ListConcat,
(++=): FunctionNoAutoLift({ a, b | a(a() ++ List(b)) }),

; Task factory - creates editable task with toggle
task-create: { name |
    text: $(name),
    done: $("🔴"),
    Grid([1, 9])(
        Button(done, { done ← "✅" }),
        TextEditor(text),
    )
},

; App state
task-name: $(""),
tasks: $(List()),

; UI
(
    Text("# TODO App"),
    Grid([5, 1])(
        TextEditor(task-name),
        Button("Add Task", {
            tasks ++= task-create(task-name()),
            task-name ← "",
        }),
    ),
    tasks,
)
`,
  "tasks-compressed": `
(++):ListConcat,task-name: $(""),task-create:{name|s: $(name),f: $("🔴"),Grid([1,10])(Button(f,{f("✅")}),TextEditor(s))},tasks: $(()),(Text("# TODO"),Grid([5,1])(TextEditor(task-name),Button("Add",{tasks(tasks()++List(task-create(task-name()))),task-name("")})),tasks)
`,
  "tasks-mini": `
; Minimal TODO app
(++): ListConcat,
n: ($""),
tasks: ($ List()),
(
    TextEditor(n),
    Button("Add",{
        tasks(tasks() ++ List(n())),
        n("")
    }),
    tasks
)
`
  ,
  "calculator": `
; Calculator - builds expression string and evaluates it

(++): StringConcat,

expr: $(""),
result: (expr . CodeEvaluate),
append: { f | { expr(expr() . f) } },
btn: { c | Button(c, { x | x ++ c } . append) },

(
    TextEditor(expr),
    result,
    Grid([2,2,2,1])(
        btn("7"), btn("8"), btn("9"), btn(" ÷ "),
        btn("4"), btn("5"), btn("6"), btn(" × "),
        btn("1"), btn("2"), btn("3"), btn(" - "),
        Button("C", { "" } . append), btn("0"), btn("."), btn(" + "),
    ),
)
`,
  "linear-regression": `
; Linear Regression with gradient descent
; Find θ that minimizes (f(x) - y)²

; Operators
(++): TensorConcat,

; Data: y = 0.23x + 0.47
x: 0 ..< 10,
y: x×0.23 + 0.47,

; Model: f(x) = θ₀·x + θ₁
θ: ~([0, 0]),
f: { x | x × θ_0 + θ_1 },

; Loss: mean squared error
𝓛: { mean((f(x) - y)^2) },

; Training
opt: adam(0.03),
losses: $([]),

{ losses(losses() ++ [opt(𝓛)]) } ⟳ 200,

; Results
(
    Text("**Loss:**"), losses,
    Text("**θ (learned):**"), θ,
    Text("**θ (target):**"), [0.23, 0.47],
)
`,
  "linear-regression-compressed": `
x: (0 ..< 10),
y: (x × 0.23 + 0.47),
θ: ~([0, 0]),
f: { x | x × (θ_0) + (θ_1) },
𝓛: { μ((y - f(x)) ^ 2) },
minimize: adam(0.03),
losses: $([]),
{ losses(losses() concat [minimize(𝓛)]), } ⟳ 400,
(losses, θ)`,
  "REPL": `
; Multi-cell REPL - each cell evaluates independently

cell: {
    code: $("1 + 1"),
    result: CodeEvaluate(code),
    Grid(1)(Print(result), CodeEditor(code, "auto"))
},

(cell(), cell(), cell())
`,
  "exponential-decay": `
; Exponential decay: f(t) = b + c·e^(-k·t)
; Interactive visualization with slider

b: 0.1,   ; baseline
c: 1,     ; amplitude
k: 5,     ; decay rate

f: { t | b + (c × exp(-(k) × t)) },

; Interactive plot
t: $(0),
(
  Slider(t),
  f(t),
  f(0..<100 / 100)
)
`,
  "box-cox": `
; 📈 Box–Cox — one dial λ reshapes the data, and ln sits at its center.
; p_λ(x) = (x^λ − 1) / λ.  Scrub λ:  0 → ln,  ½ → √,  1 → linear,  2 → parabola.
; It's a family of POWERS (x^λ). exp is NOT in it — exp lives in the INVERSE
; transform, whose own λ→0 center is exp. So p₂(x) = (x²−1)/2 is a parabola, not eˣ.

x: linspace([0.05, 4], 240),                 ; Box–Cox is defined for x > 0
λ: $(0),                                      ; the dial — scrub me: try 0, 1, 2, −1

; the transform. at λ = 0 the 0/0 limit is exactly ln(x), so branch there.
; (x × 0 broadcasts the scalar test across the 240 sample points)
p: { l | where((abs(l) < 0.001) + (x × 0), log(x), (x^l - 1) ÷ l) },

; wrap the plot in a computed so it re-draws as you scrub λ (like Scrubber does)
curve: $({ PointPlot(x, p(λ())) }),

(
  Text("# 📈 Box–Cox transform"),
  Text("One dial **λ** reshapes data toward a bell curve. **λ→0 is ln** (the smooth center), 1 is linear, 2 is a parabola. A power family — **exp is not in it**."),
  Grid([1, 6])(Text("**λ =**"), Scrubber(λ, 0.02)),
  curve,
)
`,
  "half-exp": `
; 🌗 The half of exp — a net φ trained on ONE rule: φ(φ(x)) = eˣ.
; No labels. The only signal is self-consistency: apply φ twice, land on exp.
; φ settles HALFWAY — it nudges each x partway toward eˣ, and twice completes the
; trip. This √exp has no elementary closed form (Kneser built it with complex
; analysis); we just fit it, straight from its own equation.

(++): TensorConcat,

d: 16,
x: linspace([-1, 1], 96),
X: x ⍴ [96, 1],
T: exp(x) ⍴ [96, 1],

; residual MLP: φ(z) = z + net(z). net starts at 0 (W3 = 0), so φ begins as identity
; and grows the half-step from there – which picks the increasing branch of √exp.
σ: { z | z × sigmoid(z) },                       ; silu
W1: ~(randn([1, d]) × 0.6),   b1: ~(fill([d], 0)),
W2: ~(randn([d, d]) × 0.15),  b2: ~(fill([d], 0)),
W3: ~(fill([d, 1], 0)),       b3: ~([0]),
φ: { z | z + (matmul(σ(matmul(σ(matmul(z, W1) + b1), W2) + b2), W3) + b3) },

; the ONLY loss: be self-consistent. no target for φ itself, only for φ∘φ.
𝓛: { mean((φ(φ(X)) - T)^2) },
opt: adam(0.02),

losses: $([]),
half:  $(φ(X) ⍴ [96]),        ; φ(x)     — climbs to sit between x and eˣ
whole: $(φ(φ(X)) ⍴ [96]),     ; φ(φ(x))  — snaps onto eˣ
{ losses(losses() ++ [opt(𝓛)]), half(φ(X) ⍴ [96]), whole(φ(φ(X)) ⍴ [96]) } ⟳ 2000,

(
  Text("# 🌗 The half of exp"),
  Text("A net **φ** trained on one rule — **φ(φ(x)) = eˣ** — with no labels, only self-consistency. It learns √exp, a function with no closed form."),
  Text("**loss** — self-consistency, converging:"),
  losses,
  Text("**φ(x)** (middle) settles between **x** (line) and **eˣ** (top) — genuinely halfway:"),
  (x, half, exp(x)),
  Text("**φ(φ(x))** lands on **eˣ**:"),
  (exp(x), whole),
)
`,
  "deep-delta-learning": `
; Deep Delta Learning - learns sin(x)
; Uses delta rule: x' = x + β·k·(v - k·x)

d: 160,

; Learnable parameters
w: ~(randn([d]) × 0.1),
κ: ~(randn([d]) × 0.1),
ν: ~([0]),
β: ~([0]),

; Activation & delta block
σ: { z | 1 ÷ (1 + exp(-z)) },
Δ: { x |
    k: κ ÷ √(Σ(κ^2) + 1e-7),
    x + (2 × σ(β) × (ν - Σ(k × x)) × k)
},

; Model & data
f: { t | Δ(t × w) },
τ: linspace([0, 6.28], d),
ŷ: sin(τ),

; Training
(++): TensorConcat,
(++=): FunctionNoAutoLift({ a, b | a(a() ++ b) }),

𝓛: { mean((f(τ) - ŷ)^2) },
opt: adam(0.001),
losses: $([]),
pred: $(f(τ)),

{ losses ++= [opt(𝓛)], pred(f(τ)) } ⟳ 500,

; Results
(
    Text("**Loss:**"), losses,
    Text("**Prediction vs Target:**"), (ŷ, pred),
)
`,
"loss-landscape": `
; Touch the Loss Landscape
; ONE loss definition drives both the surface and the descent – edit it live!

𝓛: { p | (p_0^2 + p_1 - 11)^2 + (p_0 + p_1^2 - 7)^2 },   ; Himmelblau – four minima

θ: ~([0, 0]),   ; the ball – grab it!
lr: $(0.4),
step: { sgd(once(lr) × 0.005)({ 𝓛(θ) }) },

; the surface: the same 𝓛 evaluated over the whole view via ⊗
n: 300,
range: [[-5, 5], [-5, 5]],
surface: log(linspace(range_1, n) (⊗ { y, x | 𝓛([x, y]) }) linspace(range_0, n) + 1),

(
  Text("# Touch the Loss Landscape"),
  Grid([1, 5])(Text("lr"), Slider(lr)),
  Button("Descend ×100", { step ⟳ 100 }),
  Layers(surface, Trail(θ, range), Point2D(θ, range)),
  𝓛(watch(θ)),   ; live loss – updates while you drag
)
`,
"geometric-median": `
; 📍 Geometric median – drag the three orange anchors. The blue point rolls
; downhill on the total-distance field to the spot closest to all three,
; re-optimizing (adam) live as you drag. The field below is that distance,
; recomputed each frame; the blue point descends it.

range: [[-2, 2], [-2, 2]],
n: 140,
soft: 0.02,   ; softens the distance at 0 so the gradient stays finite

; three draggable anchors, each a 2-vector
a: $([-1.2, 0.8]),
b: $([1.3, 1.0]),
c: $([0.1, -1.3]),

; total distance from a point p to every anchor (indexed form is ⊗-friendly)
dist: { p, q | √((p_0 - q_0)^2 + (p_1 - q_1)^2 + soft) },
F: { p | dist(p, a()) + dist(p, b()) + dist(p, c()) },

; the blue point descends F – the only trainable thing here
p: ~([1.6, 1.6]),
opt: adam(0.06),
{ opt({ F(p) }) } ⟳ 100000,

; the distance field as a heatmap, recomputed as the anchors move. Read the
; anchors to CONCRETE tensors first (av/bv/cv) – reading a signal inside the ⊗
; lambda would lift the trace and leak a tracer.
g: linspace([-2, 2], n),
field: $({ av: a(), bv: b(), cv: c(),
  Fc: { q | dist(q, av) + dist(q, bv) + dist(q, cv) },
  g (⊗ { y, x | Fc([x, y]) }) g }),

(
  Text("# 📍 Geometric median"),
  Text("Drag the **orange** anchors — the **blue** point keeps finding the spot closest to all three."),
  Layers(
    field,
    Point2D(a, range, "orange"),
    Point2D(b, range, "orange"),
    Point2D(c, range, "orange"),
    Trail(p, range, "deepskyblue"),
    Point2D(p, range, "deepskyblue"),
  ),
)
`,
"differentiable-reach": `
n: 16,  ; joints
len: 0.85 ÷ n,  ; segment length
θ: ~(fill([n], 0.15)),  ; the only trainable thing

tril: (0 ..< n) ⊗(≥) (0 ..< n),  ; prefix-sum matrix
cumsum: { v | matmul(tril, v ⍴ [n, 1]) ⍴ [n] },  ; scan
fk: { a |  ; forward kinematics
  ang: cumsum(a),  ; cumulative angle
  step: len × stack((cos(ang), sin(ang)), 1),  ; xy step
  matmul(tril, step) - [0.45, 0]  ; joint positions
},

target: $([0.55, 0.35]),  ; drag me (orange)
obstacle: $([-0.1, -0.3]),  ; drag me (red)
last: oneHot(n - 1, n) ⍴ [n, 1],  ; picks the tip

𝓛: {  ; loss
  P: fk(θ),  ; joint positions
  tip: Σ(P × last, 0),  ; the hand
  reach: Σ((tip - target())^2),  ; hand → target
  avoid: Σ(1 ÷ (Σ((P - obstacle())^2, 1) + 0.02)),  ; repel
  reach + 0.004 × avoid
},

opt: adam(0.05),  ; optimiser
joints: $(fk(θ)),  ; live pose for the glow
{ opt(𝓛), joints(fk(θ)) } ⟳ 100000,  ; step, then publish

range: [[-1, 1], [-1, 1]],  ; view box
gres: 300,  ; glow resolution
g: linspace([-1, 1], gres),  ; grid axis
arm: $({  ; the glowing arm
  q: transpose(joints()),  ; xs, ys as rows
  dx: (g ⍴ [1, gres, 1]) - (q_0 ⍴ [1, 1, n]),
  dy: (g ⍴ [gres, 1, 1]) - (q_1 ⍴ [1, 1, n]),
  Σ(exp(0 - (dx^2 + dy^2) ÷ 0.005), 2)  ; Σ gaussians
}),

(
  Text("# 🦾 Differentiable reach"),
  Text("Drag the **orange** target — the arm gradient-descends to it. Drag the **red** obstacle — it bends away. FRP × AD, one graph."),
  Layers(arm, Point2D(target, range, "orange"), Point2D(obstacle, range, "red")),
)
`,
"differentiable-aim": `
; 🎯 aim by calculus — ∇ through a physics simulation finds the launch that lands on the target
T: 48, dt: 0.04, g: [0, -4.2],   ; steps · timestep · gravity
p0: [-0.78, -0.55],              ; the muzzle (fixed)
vel: ~([1.7, 2.3]),              ; launch velocity — the only trainable thing

t: (0 ..< T) × dt,               ; sample times [T]
fly: { u | p0 + (t ⊗(×) u) + (0.5 × ((t^2) ⊗(×) g)) },   ; the ballistic path, [T, 2]

target: $([0.72, 0.12]),         ; drag me (orange)
𝓛: { Σ((fly(vel)_(T - 1) - target())^2) },   ; squared miss of where the shot lands

opt: adam(0.06),                 ; optimiser
path: $(fly(vel)),               ; live trajectory for the glow
{ opt(𝓛), path(fly(vel)) } ⟳ 100000,   ; descend the miss, then publish the arc

range: [[-1, 1], [-1, 1]], gres: 280,
gg: linspace([-1, 1], gres),
shot: $({                        ; glow along the trajectory — a Gaussian at every sample
  q: transpose(path()),
  dx: (gg ⍴ [1, gres, 1]) - (q_0 ⍴ [1, 1, T]),
  dy: (gg ⍴ [gres, 1, 1]) - (q_1 ⍴ [1, 1, T]),
  Σ(exp(0 - (dx^2 + dy^2) ÷ 0.0005), 2)
}),

(
  Text("# 🎯 Aim by calculus"),
  Text("Drag the **orange** target. The launch velocity gradient-descends *through a physics simulation* until the shot lands on it — no ballistics algebra, just ∇ of the miss. FRP × AD in one graph."),
  Layers(shot, Point2D(target, range, "orange")),
)
`,
"optimizer-race": `
; ⚔️ adam vs sgd – same start, same landscape, different characters

𝓛: { p | (p_0^2 + p_1 - 11)^2 + (p_0 + p_1^2 - 7)^2 },   ; Himmelblau

a: ~([0, -0.3]),   ; adam, orange
s: ~([0, -0.3]),   ; sgd, blue
opt: adam(0.08),
step: { opt({ 𝓛(a) }), sgd(0.002)({ 𝓛(s) }) },

n: 300,
range: [[-5, 5], [-5, 5]],
surface: log(linspace(range_1, n) (⊗ { y, x | 𝓛([x, y]) }) linspace(range_0, n) + 1),

(
  Text("# ⚔️ adam vs sgd"),
  Button("Race ×300", { step ⟳ 300 }),
  Layers(
    surface,
    Trail(a, range, "orange"), Trail(s, range, "deepskyblue"),
    Point2D(a, range, "orange"), Point2D(s, range, "deepskyblue"),
  ),
  Grid(2)(Text("**adam**"), Text("**sgd**")),
  Grid(2)(𝓛(watch(a)), 𝓛(watch(s))),
)
`,
"magnets-simulation": `
; Animated Magnetic Field

(≻): { t, v | (v_0 × 1-t) + (v_1 × t) },
(≺): { v, t | (t - v_0) / (v_1 - v_0) },
normalize: { x | [min(x), max(x)] ≺ x },

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
baseAngles: rand([numMagnets]) × 6.28,
speeds: rand([numMagnets]) ≻ [0.1, 0.5],
angles: baseAngles + (t × speeds),

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
rN: √(((X3 - nX3)^2) + ((Y3 - nY3)^2)) + ε,
rS: √(((X3 - sX3)^2) + ((Y3 - sY3)^2)) + ε,

; Potential
;k: 0.5,
; Pulsing strength
k: sin(t × 2) × 0.2 + 0.5,
potential: sum(k/rN - k/rS, 2),

; Visualization
lines: abs(sin(potential × 25)) ^ 0.25,
glow: 1 / (abs(potential) + 0.2),
field: (lines × 0.7) + (glow × 0.3),

(
  Text("# 🧲 Spinning Magnets"),
  lines,
)
`,
"magnets-minimal": `
; Animated Magnetic Field – minimal
; a magnet is two opposite charges; ⊗ is the meshgrid

(≻): { t, v | (v_0 × 1-t) + (v_1 × t) },  ; lerp

m: 3,    ; magnets
n: 600,  ; resolution
t: Time(),

angles: rand([m])×6.28 + t×(rand([m]) ≻ [0.1, 0.5]),
dir: stack(cos(angles), sin(angles)) × 0.12,   ; [2 m] headings
pos: rand([2, m]) ≻ [-1.4, 1.4],               ; [2 m] centers
pole: concat((pos + dir, pos - dir), 1),       ; [2 2m] all six poles
q: concat(fill([m], 1), fill([m], -1)),        ; charge of each pole

grid: linspace([-2, 2], n),
dx: grid (⊗ -) pole_0,          ; [n 2m]
dy: grid (⊗ -) pole_1,          ; [n 2m]
r: √(dx^2 (+ ⊗ 1) dy^2) + 1e-4, ; [n n 2m] – frames cross, poles zip

k: sin(t × 2) × 0.2 + 0.5,   ; pulsing strength
potential: sum(q×k / r, 2),

(
  Text("# 🧲 Spinning & Pulsating Magnets"),
  abs(sin(potential × 25)) ^ 0.25,
)
`,
"mandelbrot": `
; Mandelbrot set – z ← z² + c until |z| escapes
; complex numbers as (re, im) pairs, ⍣ iterates the whole plane at once
; scrub the depth and watch the set sharpen

w: 350,        ; resolution, 7:5 like the view
h: 250,
depth: $(40),  ; iterations – drag me

re: linspace([-2.5, 1], w) ⍴ [1, w] tile [h, 1],
im: linspace([-1.25, 1.25], h) ⍴ [h, 1] tile [1, w],

; one step for every pixel: state is (zx, zy, escape count)
step: { s |
  zx: ListGet(s, 0),
  zy: ListGet(s, 1),
  k:  ListGet(s, 2),
  alive: ((zx^2 + zy^2) ≤ 4),
  (
    where(alive, zx^2 - zy^2 + re, zx),
    where(alive, 2×zx×zy + im, zy),
    k + alive,
  )
},

zero: re × 0,
render: { d |
  n: (1 ⌈ d),
  counts: ListGet((step ⍣ n)((zero, zero, zero)), 2),
  √(counts ÷ n)
},

(
  Text("# 🌀 Mandelbrot"),
  Grid([1, 9])(Text("**depth:**"), Scrubber(depth)),
  render(depth),
)
`,
"game-of-life": `
; Conway's Game of Life

; grid size
n: 256,

; a cell plus its two neighbours along one axis
box: { g, ax | roll(g, 1, ax) + g + roll(g, -1, ax) },

; the 3×3 sum, minus the cell = its 8 neighbours
neighbours: { g | box(box(g, 0), 1) - g },

; random soup
board: $(rand([n, n]) < 0.3),

step: {
  c: once(board),
  a: neighbours(c),

  ; alive next if 3 neighbours, or 2 and already alive
  board ← ((a = 3) ⌈ ((a = 2) × c)),
},

step ⟳ 100000,

(
  Text("# 🦠 Game of Life"),
  board,
)
`,
  "game-of-life-tacit": `
next: (⊛ ⟜ (1 ⧉ [3, 3])) ⊸ Φ(= ⟜ 3, ∨, = ⟜ (3 ⊸ +)),
board: $(⚄([256, 256]) < 0.3),
{ board ← next(once(board)) } ⟳ 100000,
(Text("# 🐦 Game of Life, tacitly"), board)
`,
  "reaction-diffusion": `
; Gray–Scott reaction–diffusion – two chemicals paint Turing patterns.
; v eats u and reproduces (u × v²); both diffuse; explicit Euler.

n: 200,
edges: { x, ax | roll(x, 1, ax) + roll(x, -1, ax) },   ; both neighbours along an axis
Δ: { x | edges(x, 0) + edges(x, 1) - 4×x },             ; the Laplacian

; sprinkle v into a sea of u, then let it self-organise
u: $(1),
v: $(0),
reseed: { s: rand([n, n]) < 0.08 × 0.5, u← 1 - s, v← s },

; drag feed and kill – the whole zoo of patterns hides in a tiny window
F: $(0.055),
K: $(0.062),

tick: {
  a: once(u), b: once(v), f: once(F), k: once(K),
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
gx: (0 ..< ks) ⍴ [ks, 1] tile [1, ks],
gy: (0 ..< ks) ⍴ [1, ks] tile [ks, 1],
dist: √((gx - R)^2 + (gy - R)^2) ÷ R,
ring: exp(0 - (dist - 0.5)^2 ÷ (2 × 0.15^2)) × (dist < 1),
K: ring ÷ Σ(ring),

; growth: a bell centred at μ, width σ, mapped to [-1, 1]
μ: 0.15,
σ: 0.017,
growth: { u | 2 × exp(0 - (u - μ)^2 ÷ (2 × σ^2)) - 1 },

world: $(0),
reseed: { world ← rand([n, n]) },

; convolve, grow, and keep the world in [0, 1]
tick: {
  w: once(world),
  world ← clamp(w + 0.1×growth(conv(w, K)), 0, 1),
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
; scores = Q·Kᵀ / √d,  weights = softmax(scores).  Drag the temperature knob
; and watch every token's attention go from uniform to one-hot, live.

n: 48,   ; tokens
d: 16,   ; embedding dimension

; sinusoidal positional embeddings, so nearby tokens are similar (K = Q)
pos: 0 ..< n,
k: 0 ..< (d ÷ 2),
freqs: 1 ÷ (10000 ^ (k ÷ (d ÷ 2))),
angles: pos (⊗ ×) freqs,
Q: concat((sin(angles), cos(angles)), 1),

; every query dotted with every key, scaled
scores: matmul(Q, transpose(Q)) ÷ √(d),

; temperature sharpens (or flattens) the softmax – recomputes as you drag
temp: $(4),
weights: $({ softmax(scores × temp()) }),

(
  Text("# 🧠 Attention"),
  Grid([1, 6])(Text("**temperature**"), Scrubber(temp, 0.5)),
  Text("who attends to whom:"),
  weights,
)
`,
  "b-type": `
; Turing's B-type unorganised machine (1948), educated by gradient descent.
; A network of soft NAND-ish gates learns XOR – which isn't in its toolbox,
; so it must compose it (XOR = OR ∧ ¬AND). "Education" is just backprop.

(++): TensorConcat,

a: [0, 0, 1, 1],
b: [0, 1, 0, 1],
target: [0, 1, 1, 0],

; 12 two-input boolean functions – AND, OR, NAND, NOT, … but no XOR
basis: { p, q | stack((
  p×q, p + q - p×q, 1 - p×q, 1 - (p + q - p×q),
  p×(1 - q), (1 - p)×q, p, q, 1 - p, 1 - q, p×0, p×0 + 1
)) },

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
losses: $([]),
{ losses(losses() ++ [opt(loss)]) } ⟳ 400,

; recompute each training step (subscribe to losses) so the display stays live
output: $({ losses(), fwd() }),
gates: $({ losses(), stack((softmax(t1), softmax(t2), softmax(to))) }),

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

(++): TensorConcat,

; --- data: 14×14 binarised MNIST (6000 train, 1000 test) as safetensors ---
; the signal is an empty list until the fetch resolves to [Xtrain,Ytrain,Xtest,Ytest]
data: LoadSafeTensorFromURL("mnist.safetensors"),
loaded: { ListLength(data()) > 0 },
; train on 3000 of the 6000, score on 500 of the 1000 – keeps each frame light
; so the whole thing trains live and smoothly in the browser (pixels × examples)
Xtr: { transpose(slice(ListGet(ListGet(data(), 0), 1), 0, 3000)) },   ; [196, 3000]
Ytr: { transpose(slice(ListGet(ListGet(data(), 1), 1), 0, 3000)) },   ; [10, 3000]
Xte: { transpose(slice(ListGet(ListGet(data(), 2), 1), 0, 500)) },    ; [196, 500]
Yte: { transpose(slice(ListGet(ListGet(data(), 3), 1), 0, 500)) },    ; [10, 500]

; --- fixed random wiring: two source signals per gate ---
G1: 512, G2: 200,
IA1: floor(rand([G1]) × 196),  IB1: floor(rand([G1]) × 196),   ; layer 1 reads pixels
IA2: floor(rand([G2]) × G1),   IB2: floor(rand([G2]) × G1),    ; layer 2 reads layer 1

; the 16 two-input truth tables (row i = binary of i = [f00, f01, f10, f11])
TT: [
  [0,0,0,0], [0,0,0,1], [0,0,1,0], [0,0,1,1],
  [0,1,0,0], [0,1,0,1], [0,1,1,0], [0,1,1,1],
  [1,0,0,0], [1,0,0,1], [1,0,1,0], [1,0,1,1],
  [1,1,0,0], [1,1,0,1], [1,1,1,0], [1,1,1,1]
],
; pool 20 gates per class: GROUP[c, g] = 1 where gate g belongs to class c
GROUP: (0 ..< 10) ⊗(=) floor((0 ..< G2) ÷ (G2 ÷ 10)),

; a soft gate: expected truth table softmax(θ)·TT, blended over the 4 input corners
gate: { A, B, W | Wt: transpose(W),
  reshape(Wt_0, [#A, 1])×((1 - A)×(1 - B)) + reshape(Wt_1, [#A, 1])×((1 - A)×B)
  + reshape(Wt_2, [#A, 1])×(A×(1 - B)) + reshape(Wt_3, [#A, 1])×(A×B)
},
sm: { th | matmul(softmax(th), TT) },

; the learnable parameters: each gate's softmax logits over the 16 functions
t1: ~(randn([G1, 16]) × 0.1),
t2: ~(randn([G2, 16]) × 0.1),

model: { X |
  h1: gate(gather(X, IA1), gather(X, IB1), sm(t1)),
  h2: gate(gather(h1, IA2), gather(h1, IB2), sm(t2)),
  matmul(GROUP, h2)                                   ; [10, examples] logits
},

; cross-entropy over the 10 classes (numerically-stable log-softmax on axis 0)
logp: { L | L - log(Σ(exp(L), 0)) },
loss: { -mean(Σ(Ytr() × logp(model(Xtr())), 0)) },

; train – each tick is gated on the data being loaded (a no-op 0 until then)
opt: adam(0.05),
losses: $([]),
{ losses(losses() ++ [cascade((guard(loaded(), { opt(loss) }), { 0 }))()]) } ⟳ 1500,

; live accuracy on the held-out test set, recomputed every training step
accuracy: $({ losses(), cascade((guard(loaded(), {
  hits: argmax(model(Xte()), 0) = argmax(Yte(), 0),
  round(mean(hits) × 1000) ÷ 10
}), { 0 }))() }),

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
; 🧠 A brain-plausible network learns MNIST — with NO backpropagation.
; Dale's principle: every weight is non-negative, and each neuron is purely
; excitatory or purely inhibitory (four sub-matrices per layer, +p −n / +n −p).
; Credit is assigned by DIFFUSING the output error straight to the hidden
; units — no ∇, no weight transport, no transposed backward pass. Each layer's
; update is a local, Hebbian outer product: ΔW ∝ Aᵀ·(φ'(z)⊙routed-error).
; After "Diffusing Blame", Yamada et al., Sakana AI (arXiv:2606.31700).

(++): TensorConcat,

; --- data: 14×14 binarised MNIST as safetensors (an empty list until it loads) ---
data: LoadSafeTensorFromURL("mnist.safetensors"),
loaded: { ListLength(data()) > 0 },
Xtr: { ListGet(ListGet(data(), 0), 1) },   ; [6000, 196]  pixels in {0,1}
Ytr: { ListGet(ListGet(data(), 1), 1) },   ; [6000, 10]   one-hot labels
Xte: { ListGet(ListGet(data(), 2), 1) },   ; [1000, 196]
Yte: { ListGet(ListGet(data(), 3), 1) },   ; [1000, 10]

Din: 196, C: 10, H1: 200, H2: 100, α: 6, lr: 3, B: 128,

; layer-specific sigmoid width α: wide sigmoids keep the derivative alive, or
; the error attenuates ~25× per layer and nothing learns (the paper's key knob).
; φ(z) = 1/(1+e^(-2z/α));  φ'(a) = (2/α)·a·(1-a)  — read straight off the activation.
φ:  { z | sigmoid(z × (2 ÷ α)) },
dφ: { a | (a × (1 - a)) × (2 ÷ α) },

; modulo error routing: hidden unit i is wired to output class (i mod C). The
; routing matrix M[i,c] = 1 iff i mod C = c; the hidden error is R = S·Mᵀ.
route: { H | ((0 ..< H) % C) ⊗(=) (0 ..< C) },   ; [H, C]
M1: route(H1), M2: route(H2),

; asymmetric non-negative init: U[0,1)/√fan-in, excitatory ×1.5, inhibitory ×0.5
; (a 3:1 E/I head start); the output layer stays symmetric to avoid saturation.
ini: { fan, s | rand([fan, s]) ÷ √(fan) },
Wpp1: $(ini(Din, H1) × 1.5), Wnp1: $(ini(Din, H1) × 0.5),
Wnn1: $(ini(Din, H1) × 1.5), Wpn1: $(ini(Din, H1) × 0.5),
Wpp2: $(ini(H1, H2) × 1.5),  Wnp2: $(ini(H1, H2) × 0.5),
Wnn2: $(ini(H1, H2) × 1.5),  Wpn2: $(ini(H1, H2) × 0.5),
Wppo: $(ini(H2, C)),         Wnpo: $(ini(H2, C)),
Wnno: $(ini(H2, C)),         Wpno: $(ini(H2, C)),

; one dual-stream layer: positive stream is excited by p, inhibited by n; the
; negative stream mirrors it. The −signs are structural (Dale), never learned.
fwd: { p, n, Wpp, Wnp, Wnn, Wpn |
  (φ(matmul(p, Wpp) - matmul(n, Wnp)), φ(matmul(n, Wnn) - matmul(p, Wpn))) },

; the ED weight step: a local outer product of presynaptic activity A and the
; postsynaptic drive U, then clamp ≥ 1e-4 so weights can never turn negative.
edUp: { W, A, U | (W + (matmul(transpose(A), U) × (lr ÷ B))) ⌈ 0.0001 },

; readout for scoring: the output positive stream's logit, argmax over classes.
model: { X | l1: fwd(X, X, Wpp1(), Wnp1(), Wnn1(), Wpn1()),
  l2: fwd(l1_0, l1_1, Wpp2(), Wnp2(), Wnn2(), Wpn2()),
  matmul(l2_0, Wppo()) - matmul(l2_1, Wnpo()) },   ; [n, 10]

; ONE training step — a full forward, one diffused error, twelve local updates.
train: { idx: floor(rand([B]) × 6000),
  X: Xtr() _ idx, Y: Ytr() _ idx,
  ; forward, keeping every stream's activation for its local update
  l1: fwd(X, X, Wpp1(), Wnp1(), Wnn1(), Wpn1()), p1: l1_0, n1: l1_1,
  l2: fwd(p1, n1, Wpp2(), Wnp2(), Wnn2(), Wpn2()), p2: l2_0, n2: l2_1,
  lo: fwd(p2, n2, Wppo(), Wnpo(), Wnno(), Wpno()), po: lo_0, no: lo_1,
  ; batch-centred one-vs-all error at the output (prediction = positive stream)
  E: Y - po, S: E - mean(E, 0, 1),
  ; the SAME output error, routed to each hidden layer — no backward pass
  R1: matmul(S, transpose(M1)), R2: matmul(S, transpose(M2)),
  ; postsynaptic drives: positive stream chases +R, negative stream chases −R
  Up1: dφ(p1) × R1, Un1: dφ(n1) × (0 - R1),
  Up2: dφ(p2) × R2, Un2: dφ(n2) × (0 - R2),
  Upo: dφ(po) × S,  Uno: dφ(no) × (0 - S),
  ; twelve local updates (inhibitory pathways carry the opposite-sign drive)
  Wpp1(edUp(Wpp1(), X, Up1)),   Wnp1(edUp(Wnp1(), X, 0 - Up1)),
  Wnn1(edUp(Wnn1(), X, Un1)),   Wpn1(edUp(Wpn1(), X, 0 - Un1)),
  Wpp2(edUp(Wpp2(), p1, Up2)),  Wnp2(edUp(Wnp2(), n1, 0 - Up2)),
  Wnn2(edUp(Wnn2(), n1, Un2)),  Wpn2(edUp(Wpn2(), p1, 0 - Un2)),
  Wppo(edUp(Wppo(), p2, Upo)),  Wnpo(edUp(Wnpo(), n2, 0 - Upo)),
  Wnno(edUp(Wnno(), n2, Uno)),  Wpno(edUp(Wpno(), p2, 0 - Uno)),
  mean(abs(S)) },   ; how much blame is left to diffuse

; --- the live displays ---
training: $(1),
errs: $([]), estride: $(2),
{ i | when(loaded(), { when(once(training), {
  s: train(),                                  ; train EVERY frame; log periodically
  when((i % once(estride)) = 0, {
    errs(errs() ++ (s ⍴ [1])),
    when((#(errs())) ≥ 480, {                  ; bound the plot: thin + slow logging
      errs(errs() _ ((0 ..< 240) × 2)),
      estride ← (once(estride) × 2)
    })
  })
}) }) } ⟳ 1000000,

; test accuracy on held-out digits, recomputed as the error curve grows
accuracy: $({ errs(), cascade((guard(loaded(), {
  hits: argmax(model(Xte()), -1) = argmax(Yte(), -1),
  round(mean(hits) × 1000) ÷ 10
}), { 0 }))() }),

; emergent E/I balance: mean excitatory vs inhibitory hidden weight. Starts at
; 3:1 (the init), and learning drives it toward the cortical 1:1 on its own.
eibal: $({ errs(),
  (mean(Wpp1()) + mean(Wnn1()) + mean(Wpp2()) + mean(Wnn2()))
  ÷ (mean(Wnp1()) + mean(Wpn1()) + mean(Wnp2()) + mean(Wpn2())) }),

; look inside: the ten class "amplifiers" — for each class, the mean excitatory
; input field of its 20 routed hidden units, reshaped to 14×14 and laid in a row.
amp: $({ errs(),
  m: mean(reshape(Wpp1(), [Din, 20, C]), 1),   ; [196, 10] per-class excitatory field
  ; List(...) keeps the ten tiles a list (a [...] literal would stack them into one
  ; tensor); concat lays them side by side into a 14×140 filmstrip.
  concat(List( reshape(slice(m, [0, 0], [Din, 1]), [14, 14]), reshape(slice(m, [0, 1], [Din, 1]), [14, 14]),
    reshape(slice(m, [0, 2], [Din, 1]), [14, 14]), reshape(slice(m, [0, 3], [Din, 1]), [14, 14]),
    reshape(slice(m, [0, 4], [Din, 1]), [14, 14]), reshape(slice(m, [0, 5], [Din, 1]), [14, 14]),
    reshape(slice(m, [0, 6], [Din, 1]), [14, 14]), reshape(slice(m, [0, 7], [Din, 1]), [14, 14]),
    reshape(slice(m, [0, 8], [Din, 1]), [14, 14]), reshape(slice(m, [0, 9], [Din, 1]), [14, 14]) ), 1) }),

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
; The corpus lives in a data slot (~~): the compiled training step receives it
; as an argument each step – so the step stays jit-hot, and pausing, editing
; the text and resuming re-trains on YOUR data without resetting the weights.
; Gradients flow into ~, never into ~~.

(++): TensorConcat,

; --- vocabulary: ' ' → 0, a…z → 1…26 (anything else collapses to space) ---
V: 27, T: 8, C: 32, F: 96,
enc: { s | clamp(StringToCodes(s) - 96, 0, 26) },
dec: { t | CodesToString(where(t = 0, 32, t + 96)) },

; --- the entire dataset is this editable string ---
corpus: $("emma olivia ava isabella sophia charlotte mia amelia harper evelyn abigail emily elizabeth mila ella avery sofia camila aria scarlett victoria madison luna grace chloe penelope layla riley zoey nora lily eleanor hannah lillian addison aubrey ellie stella natalie zoe leah hazel violet aurora savannah audrey brooklyn bella claire skylar lucy paisley everly anna caroline nova genesis emilia kennedy samantha maya willow kinsley naomi aaliyah elena sarah ariana allison gabriella alice madelyn cora ruby eva serenity autumn adeline hailey gianna valentina isla eliana quinn ivy sadie piper lydia alexa josephine emery julia delilah arianna vivian kaylee sophie brielle madeline"),
load: { c | windows(T + 1, enc(c)) },       ; every 9-char window is an example
D: ~~(load(once(corpus))),
SignalEffect({ D := load(corpus()) }),      ; corpus edits reload the slot – the weights stay

; --- the transformer: one pre-norm block, one head ---
E:  ~(randn([V, C]) × 0.1),  P:  ~(randn([T, C]) × 0.1),
Wq: ~(randn([C, C]) × 0.2),  Wk: ~(randn([C, C]) × 0.2),
Wv: ~(randn([C, C]) × 0.2),  Wo: ~(randn([C, C]) × 0.2),
W1: ~(randn([C, F]) × 0.2),  W2: ~(randn([F, C]) × 0.1),
Wu: ~(randn([C, V]) × 0.1),

rms: { x | x ÷ √(mean(x × x, -1, 1) + 0.001) },   ; the 1 keeps the reduced axis
gelu: { x | x × sigmoid(x × 1.702) },
nmask: where((0 ..< T) ⊗(≥) (0 ..< T), fill([T, T], 0), fill([T, T], -9999)),
att: { x | q: matmul(x, Wq), k: matmul(x, Wk), v: matmul(x, Wv),
  a: softmax((einsum("btc,bsc->bts", q, k) ÷ √(C)) + nmask),
  matmul(matmul(a, v), Wo) },
model: { ids | x0: matmul(oneHot(ids, V), E) + P,
  x1: x0 + att(rms(x0)),
  x2: x1 + matmul(gelu(matmul(rms(x1), W1)), W2),
  matmul(rms(x2), Wu) },   ; [batch, T, V] logits

loss: { crossEntropy(oneHot(slice(D, [0, 1], [-1, T]), V), model(slice(D, [0, 0], [-1, T]))) },

; --- sampling: gumbel-max, with a temperature you can drag ---
codesF: linspace([0, V - 1], V),   ; float 0…26 – argmax is int, gather keeps it float
τ: $(0.5),
pick: { l | codesF _ argmax((l ÷ ((once(τ) × 1.5) ⌈ 0.05)) - log(-log(rand([V]))), -1) },
grow: { s | ctx: s _ ((0 ..< T) + (#(s) - T)),
  l: (model(ctx ⍴ [1, T]) ⍴ [T, V]) _ (T - 1),
  s ++ (pick(l) ⍴ [1]) },
gen: { seed, n | (grow ⍣ n)(seed) },
dream: $("### …one moment, it is waking up…"),
dreamNow: { dream(StringConcat("### ", dec(slice(gen(fill([T], 0), 48), T)))) },

; --- training: ⟳-paced, gated by the checkbox ---
training: $(1),
opt: adamw(0.03, 0.001),   ; a little weight decay – memorize less, invent more
losses: $([]),
stride: $(4),
{ i |
  when(once(training), {
    l: opt(loss),
    when((i % once(stride)) = 0, {
      losses(losses() ++ (l ⍴ [1])),
      ; the plot keeps the WHOLE story, bounded: when it fills up,
      ; thin the history to every other point and log half as often
      when((#(losses())) ≥ 480, {
        losses(losses() _ ((0 ..< 240) × 2)),
        stride ← (once(stride) × 2)
      })
    }),
    when((i % 200) = 0, dreamNow)
  })
} ⟳ 1000000,

; type a beginning – the model finishes it (works even while paused)
prompt: $("kri"),
finish: $({ dream(), p: prompt(), s: fill([T], 0) ++ enc(p),
  StringConcat("## ", p, "·", dec(slice(gen(s, 10), #(s)))) }),

; what the head attends to while reading a name, live
probeS: fill([T], 0) ++ enc("sophia"),
probe: probeS _ ((0 ..< T) + (#(probeS) - T)),
attn: $({ losses(),
  x: rms(matmul(oneHot(probe ⍴ [1, T], V), E) + P),
  softmax((einsum("btc,bsc->bts", matmul(x, Wq), matmul(x, Wk)) ÷ √(C)) + nmask) ⍴ [T, T] }),

editable: $({ 1 - training() }),
(
  Text("# 🔮 The Name Dreamer"),
  Text("A 12k-parameter transformer learns to invent names – live, in your tab."),
  Checkbox(training),
  Text("**training** – uncheck to pause; pause to edit the corpus below"),
  Text("**loss** – the whole run, from first step to now:"),
  losses,
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
; Two physical blocks (φ1, φ2), stored once, are revisited R=3 times, so the
; unrolled network is N = K·R = 6 blocks deep – for a third of the weights.
; Every sublayer visit is post-norm, x ← rms(α·x + f(x)), with the DeepLoop
; scaling rule (p = ½): skip α = (2N)^½, per-matrix init gain β = (8N)^-½ –
; strong skips and gentle starts keep 12 sublayer visits stable.

(++): TensorConcat,

; --- vocabulary: ' ' → 0, a…z → 1…26 (anything else collapses to space) ---
V: 27, T: 8, C: 32, F: 96,
K: 2, R: 3, N: K × R,             ; physical blocks × rounds = unrolled depth
α: √(2 × N), β: 1 ÷ √(8 × N),     ; the scaling rule
enc: { s | clamp(StringToCodes(s) - 96, 0, 26) },
dec: { t | CodesToString(where(t = 0, 32, t + 96)) },

; --- the entire dataset is this editable string ---
corpus: $("emma olivia ava isabella sophia charlotte mia amelia harper evelyn abigail emily elizabeth mila ella avery sofia camila aria scarlett victoria madison luna grace chloe penelope layla riley zoey nora lily eleanor hannah lillian addison aubrey ellie stella natalie zoe leah hazel violet aurora savannah audrey brooklyn bella claire skylar lucy paisley everly anna caroline nova genesis emilia kennedy samantha maya willow kinsley naomi aaliyah elena sarah ariana allison gabriella alice madelyn cora ruby eva serenity autumn adeline hailey gianna valentina isla eliana quinn ivy sadie piper lydia alexa josephine emery julia delilah arianna vivian kaylee sophie brielle madeline"),
load: { c | windows(T + 1, enc(c)) },       ; every 9-char window is an example
D: ~~(load(once(corpus))),
SignalEffect({ D := load(corpus()) }),      ; corpus edits reload the slot – the weights stay

; --- K=2 physical blocks: this is ALL the depth there is ---
E:   ~(randn([V, C]) × 0.1),  P:   ~(randn([T, C]) × 0.1),  Wu: ~(randn([C, V]) × 0.1),
Wq1: ~(randn([C, C]) × β),  Wk1: ~(randn([C, C]) × β),  Wv1: ~(randn([C, C]) × β),
Wo1: ~(randn([C, C]) × β),  Wf1: ~(randn([C, F]) × β),  Wg1: ~(randn([F, C]) × β),
Wq2: ~(randn([C, C]) × β),  Wk2: ~(randn([C, C]) × β),  Wv2: ~(randn([C, C]) × β),
Wo2: ~(randn([C, C]) × β),  Wf2: ~(randn([C, F]) × β),  Wg2: ~(randn([F, C]) × β),

rms: { x | x ÷ √(mean(x × x, -1, 1) + 0.001) },   ; the 1 keeps the reduced axis
gelu: { x | x × sigmoid(x × 1.702) },
nmask: where((0 ..< T) ⊗(≥) (0 ..< T), fill([T, T], 0), fill([T, T], -9999)),
att: { x, Wq, Wk, Wv, Wo | q: matmul(x, Wq), k: matmul(x, Wk), v: matmul(x, Wv),
  a: softmax((einsum("btc,bsc->bts", q, k) ÷ √(C)) + nmask),
  matmul(matmul(a, v), Wo) },
b1: { x | h: rms((x × α) + att(x, Wq1, Wk1, Wv1, Wo1)),
  rms((h × α) + matmul(gelu(matmul(h, Wf1)), Wg1)) },
b2: { x | h: rms((x × α) + att(x, Wq2, Wk2, Wv2, Wo2)),
  rms((h × α) + matmul(gelu(matmul(h, Wf2)), Wg2)) },
cycle: { x | b2(b1(x)) },                   ; one round: the whole stack, once

embed: { ids | matmul(oneHot(ids, V), E) + P },
model: { ids | matmul(cycle(cycle(cycle(embed(ids)))), Wu) },   ; training unrolls R=3 rounds
modelAt: { ids, r | matmul((cycle ⍣ r)(embed(ids)), Wu) },      ; dreaming picks its own r

loss: { crossEntropy(oneHot(slice(D, [0, 1], [-1, T]), V), model(slice(D, [0, 0], [-1, T]))) },

; --- sampling: gumbel-max, with temperature and ROUNDS you can drag ---
codesF: linspace([0, V - 1], V),   ; float 0…26 – argmax is int, gather keeps it float
τ: $(0.5),
rounds: $(0.4),                    ; 1 + 0.4×5 = the 3 rounds it is trained at
pick: { l | codesF _ argmax((l ÷ ((once(τ) × 1.5) ⌈ 0.05)) - log(-log(rand([V]))), -1) },
grow: { s | ctx: s _ ((0 ..< T) + (#(s) - T)),
  l: (modelAt(ctx ⍴ [1, T], 1 + (once(rounds) × 5)) ⍴ [T, V]) _ (T - 1),
  s ++ (pick(l) ⍴ [1]) },
gen: { seed, n | (grow ⍣ n)(seed) },
dream: $("### …one moment, it is waking up…"),
dreamNow: { dream(StringConcat("### ", dec(slice(gen(fill([T], 0), 48), T)))) },

; --- training: ⟳-paced, gated by the checkbox ---
training: $(1),
opt: adamw(0.01, 0),   ; post-norm runs hotter than pre-norm: a cooler lr, and no
                       ; weight decay – rms-normalized blocks shrink until they blow up
losses: $([]),
stride: $(4),
{ i |
  when(once(training), {
    l: opt(loss),
    when((i % once(stride)) = 0, {
      losses(losses() ++ (l ⍴ [1])),
      ; the plot keeps the WHOLE story, bounded: when it fills up,
      ; thin the history to every other point and log half as often
      when((#(losses())) ≥ 480, {
        losses(losses() _ ((0 ..< 240) × 2)),
        stride ← (once(stride) × 2)
      })
    }),
    when((i % 200) = 0, dreamNow)
  })
} ⟳ 1000000,

; type a beginning – the model finishes it (works even while paused)
prompt: $("kri"),
finish: $({ dream(), p: prompt(), s: fill([T], 0) ++ enc(p),
  StringConcat("## ", p, "·", dec(slice(gen(s, 10), #(s)))) }),

; the SAME block 1, watched in round 1 and in round 3: shared weights, different work
probeS: fill([T], 0) ++ enc("sophia"),
probe: probeS _ ((0 ..< T) + (#(probeS) - T)),
attProbe: { x | softmax((einsum("btc,bsc->bts", matmul(x, Wq1), matmul(x, Wk1)) ÷ √(C)) + nmask) ⍴ [T, T] },
attn1: $({ losses(), attProbe(embed(probe ⍴ [1, T])) }),
attn3: $({ losses(), attProbe(cycle(cycle(embed(probe ⍴ [1, T])))) }),

editable: $({ 1 - training() }),
(
  Text("# 🔁 The Looped Dreamer"),
  Text("Two transformer blocks, stored once, revisited three times – a 6-block-deep name dreamer for a third of the weights (~22k parameters)."),
  Checkbox(training),
  Text("**training** – uncheck to pause; pause to edit the corpus below"),
  Text("**loss** – the whole run, from first step to now:"),
  losses,
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
  Text("**block 1's attention** reading “sophia” – in round 1, then in round 3 (row attends to column). Same weights, different work:"),
  attn1,
  attn3,
  Text("**the training data** – uncheck training to edit; resuming keeps the weights:"),
  TextEditor(corpus, editable),
)
`,
  "camera-edges": `
; 📷 Live edge detection – your camera through a Laplacian kernel.
; Allow camera access, then wave: flat areas cancel out, edges glow.

cam: Camera(320, 240),
k: [[0, 1, 0], [1, -4, 1], [0, 1, 0]],

; every frame: grayscale the image (mean over the color axis), convolve
edges: $({ abs(conv(mean(cam(), 2), k)) }),

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
spectrum: $({ f: fft(mic()), √((f_0)^2 + (f_1)^2) }),

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

detect: $({
  f: fft(mic()),
  mag: √(f_0^2 + f_1^2),
  bins: 0 ..< #(mag),
  freqs: bins × sr ÷ size,
  ; only the musical range, so mic rumble and hiss don't win
  band: mag × (freqs > 55) × (freqs < 2000),
  ; the loudest bin – kept as a float (argmax would be int)
  peak: Σ(bins × (band = max(band))),
  freq: peak × sr ÷ size,
  midi: round(69 + 12×log2(freq ÷ 440)),
  (
    ListGet(names, midi % 12),
    floor(midi ÷ 12) - 1,
    round(freq),
  )
}),

(
  Text("# 🎸 Pitch Detector"),
  Text("**note · octave · Hz**"),
  detect,
)
`,
  "recursion": `
; Recursion two ways.

; NAMED — a binding is visible inside its own body, so it just calls its
; own name. This is the everyday way to recurse:
fact: { n | cascade((guard(n = 0, { 1 }), { n × fact(n - 1) }))() },
fib:  { n | cascade((guard(n < 2, { n }), { fib(n - 1) + fib(n - 2) }))() },

(
  fact(5),   ; 120
  fib(10),   ; 55

  ; ANONYMOUS — a nameless lambda has no name to call, so it grabs ITSELF
  ; with 'self'. Capture it first (rec: self): inside the guard's thunk,
  ; a bare 'self' would mean the thunk, not the function. Euclid's gcd,
  ; defined and applied on the spot without ever being named:
  ({ a, b | rec: self, cascade((guard(b = 0, { a }), { rec(b, a % b) }))() })(48, 36),   ; 12
)
`

} as const
