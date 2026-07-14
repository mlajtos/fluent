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
; BUG: CodeEditor's height doesn't auto-adjust on content change

cell: {
    code: $("1 + 1"),
    result: CodeEvaluate(code),
    Grid(1)(Print(result), CodeEditor(code))
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
