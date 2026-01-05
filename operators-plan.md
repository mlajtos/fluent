# The Grand Grimoire of Operators

A comprehensive exploration across array languages, functional programming, category theory, signal processing, and beyond.

**Sources**: APL, J, K/Q, BQN, Haskell, Julia, NumPy/PyTorch, R, Mathematica, MATLAB, Forth, Lisp, Category Theory, Signal Processing, Statistics, Linear Algebra, and pure mathematical notation.

---

## 1. Higher-Order Function Combinators

### Scan (Prefix Reduction) — `\` or `⍀`
**APL**: `+\` gives running sums. Essential for cumulative operations.
```fluent
(⍀): TensorScan,        ; or (\)
+⍀ [1,2,3,4]            ; → [1, 3, 6, 10]
*⍀ [1,2,3,4]            ; → [1, 2, 6, 24] (running product)
```
**Implementation**: `tf.cumsum`, `tf.cumprod`, or generic scan with any binary op.

### Outer Product — `∘.` or `⊗`
**APL**: `∘.×` creates multiplication table. Fundamental for combinatorics.
```fluent
(⊗): TensorOuter,
[1,2,3] ⊗(*) [1,2,3]    ; → [[1,2,3],[2,4,6],[3,6,9]]
[1,2,3] ⊗(-) [1,2,3]    ; → [[0,-1,-2],[1,0,-1],[2,1,0]]
```
**Implementation**: Reshape + broadcast multiplication.

### Inner Product — `f.g`
**APL**: `+.×` is matrix multiply. Generalized: any two ops.
```fluent
A +.× B                  ; standard matmul
A ∨.∧ B                  ; boolean matrix "or-and" product
A max.+ B                ; tropical semiring (shortest path)
```
**Implementation**: Already have `matmul`, but generalized version is powerful.

### Fork — `(f g h)`
**BQN/J**: Apply three functions, combine results: `(f x) g (h x)`
```fluent
fork: { f, g, h | { x | g(f(x), h(x)) } },
avg: fork(sum, ÷, #),    ; sum(x) / length(x)
avg([1,2,3,4,5])         ; → 3
```

### Hook — `(f g)`
**J**: `x f (g x)` — apply g first, then f with original
```fluent
hook: { f, g | { x | f(x, g(x)) } },
demean: hook(-, μ),      ; x - mean(x)
demean([1,2,3,4,5])      ; → [-2,-1,0,1,2]
```

### Compose — `∘`
```fluent
(∘): { f, g | { x | f(g(x)) } },
sincos: sin ∘ cos,
```

### Flip/Commute — `⍨` or `˜`
**APL**: Swap arguments or duplicate single argument
```fluent
(⍨): { f | { x, y | f(y, x) } },
3 -⍨ 10                  ; → 7 (10 - 3, not 3 - 10)
-⍨ 5                     ; → 0 (5 - 5, self-application)
```

---

## 2. Array Manipulation

### Rotate — `⌽` (reverse) and `⊖` (rotate by n)
```fluent
(⌽): TensorReverse,      ; already have this
(⊖): TensorRotate,       ; rotate by n positions
2 ⊖ [1,2,3,4,5]          ; → [3,4,5,1,2]
```
**Implementation**: `tf.concat(tf.slice(...), tf.slice(...))`

### Take/Drop — `↑` / `↓`
**APL**: First/last n elements, negative for end
```fluent
(↑): TensorTake,
(↓): TensorDrop,
3 ↑ [1,2,3,4,5]          ; → [1,2,3]
-2 ↑ [1,2,3,4,5]         ; → [4,5]
2 ↓ [1,2,3,4,5]          ; → [3,4,5]
```

### Replicate/Compress — `/` (dyadic)
**APL**: Filter by boolean mask or repeat by counts
```fluent
[1,0,1,0,1] / [10,20,30,40,50]  ; → [10,30,50]
[2,0,3] / [1,2,3]               ; → [1,1,3,3,3]
```
Note: Already have `mask`, but replicate is more general.

### Unique/Nub — `∪`
```fluent
(∪): TensorUnique,
∪ [1,2,2,3,3,3]          ; → [1,2,3]
```
**Implementation**: `tf.unique`

### Membership/Element of — `∈`
```fluent
(∈): TensorMembership,
[2,5] ∈ [1,2,3,4]        ; → [1,0] (2 is in, 5 is not)
```

### Index of — `⍳` (dyadic)
**APL**: Find positions of elements
```fluent
(⍳): FunctionCascade((TensorIndexOf, TensorIota)),
⍳ 5                      ; → [0,1,2,3,4] (iota, same as 0::5)
[1,2,3] ⍳ [2,4]          ; → [1, 3] (index of 2 is 1, 4 not found → length)
```

### Grade — `⍋` (up) and `⍒` (down)
**APL**: Return indices that would sort the array
```fluent
(⍋): TensorGradeUp,
(⍒): TensorGradeDown,
⍋ [30,10,20]             ; → [1,2,0] (indices to sort ascending)
⍒ [30,10,20]             ; → [0,2,1] (indices to sort descending)
```
**Implementation**: `tf.topk` or argsort equivalent

### Enclose/Disclose — `⊂` / `⊃`
**APL**: Box/unbox for nested arrays
```fluent
(⊂): TensorEnclose,      ; wrap as single element
(⊃): TensorDisclose,     ; unwrap / first element
⊃ [[1,2],[3,4]]          ; → [1,2]
```

---

## 3. Mathematical Operations

### Factorial/Binomial — `!`
```fluent
(!): FunctionCascade((TensorBinomial, TensorFactorial)),
! 5                      ; → 120
3 ! 5                    ; → 10 (5 choose 3)
```
**Implementation**: `tf.exp(tf.lgamma(n+1))` for factorial

### GCD/LCM — `∨` / `∧`
**APL**: Greatest common divisor, least common multiple
```fluent
(∨): TensorGCD,
(∧): TensorLCM,
12 ∨ 18                  ; → 6
12 ∧ 18                  ; → 36
```

### Complex Numbers — `ℑ`, `ℜ`, `∠`
```fluent
(ℑ): TensorImaginary,
(ℜ): TensorReal,
(∠): TensorAngle,        ; or TensorPhase
complex: { r, i | r + (i × 1j) },
```

### Logarithm Base — `⍟`
**APL**: `x ⍟ y` is log base x of y
```fluent
(⍟): TensorLogBase,
2 ⍟ 8                    ; → 3
10 ⍟ 1000                ; → 3
```

### Clamp/Clip
```fluent
clamp: { lo, hi, x | lo ⌈ (x ⌊ hi) },
; or as operator
(⊏): TensorClamp,
[0, 1] ⊏ [-0.5, 0.5, 1.5]  ; → [0, 0.5, 1]
```

### Softmax — `σ`
```fluent
(σ): TensorSoftmax,
σ [1, 2, 3]              ; → [0.09, 0.24, 0.67]
```
**Implementation**: `tf.softmax`

### Norm — `‖`
```fluent
(‖): TensorNorm,
‖ [3, 4]                 ; → 5 (L2 norm)
2 ‖ [3, 4]               ; → 5 (explicit L2)
1 ‖ [3, 4]               ; → 7 (L1 norm)
```

---

## 4. Structural Operations

### Ravel/Flatten — `,`
**APL**: Flatten to 1D
```fluent
(,): TensorFlatten,
, [[1,2],[3,4]]          ; → [1,2,3,4]
```
**Implementation**: `tf.reshape(x, [-1])`

### Split — `⊆`
```fluent
(⊆): TensorSplit,
3 ⊆ [1,2,3,4,5,6,7,8,9]  ; → [[1,2,3],[4,5,6],[7,8,9]]
```
**Implementation**: `tf.split`

### Pad
```fluent
pad: TensorPad,
[[1,1], [2,2]] pad [[1,2],[3,4]]  ; pad with zeros
```
**Implementation**: `tf.pad`

### Diagonal — `⌹` (monadic: diagonal, dyadic: solve)
**APL**: Extract diagonal or solve linear system
```fluent
(⌹): FunctionCascade((TensorSolve, TensorDiagonal)),
⌹ [[1,2],[3,4]]          ; → [1,4] (diagonal)
A ⌹ b                    ; → x where Ax = b
```

### Broadcast/Expand
```fluent
(⤢): TensorBroadcast,
[3, 1] ⤢ [1, 2, 3]       ; → [[1,2,3],[1,2,3],[1,2,3]]
```

---

## 5. Logic & Boolean

### All/Any — `∀` / `∃`
```fluent
(∀): TensorAll,
(∃): TensorAny,
∀ [1, 1, 1]              ; → 1 (all true)
∃ [0, 0, 1]              ; → 1 (any true)
```
**Implementation**: `tf.all`, `tf.any`

### Not — `¬`
```fluent
(¬): TensorNot,
¬ [0, 1, 0]              ; → [1, 0, 1]
```
**Implementation**: `tf.logicalNot`

### And/Or/Xor — `∧` / `∨` / `⊻`
Could overload `∧`/`∨` for booleans vs GCD/LCM for integers.
```fluent
(⊻): TensorXor,
[1,0,1] ⊻ [1,1,0]        ; → [0,1,1]
```

---

## 6. Special & Fun

### Stencil/Convolution — `⌺`
**APL2**: Apply function to sliding windows
```fluent
(⌺): TensorStencil,
3 ⌺(μ) [1,2,3,4,5]       ; → [1.5, 2, 3, 4, 4.5] (moving average)
```
**Implementation**: Convolution or manual windowing

### Life/Cellular Automata helper
```fluent
neighbors: { grid |
  ; count of 8-neighbors for each cell
  ...
},
```

### Random Choice — `?`
**APL**: `?n` gives random from 0 to n-1, `m?n` gives m unique randoms
```fluent
(?): FunctionCascade((TensorDeal, TensorRoll)),
? 6                      ; → random 0-5
3 ? 10                   ; → 3 unique randoms from 0-9
```

### Interval — `⸤` `⸥` or `[]`
```fluent
interval: { lo, hi | { x | (x ≥ lo) * (x ≤ hi) } },
[0, 1] interval 0.5      ; → 1 (in range)
```

---

## Priority Recommendations

### Tier 1 (High Value, Easy to Implement)
1. **`⍀` Scan** — cumsum/cumprod are common, tf.js has them
2. **`↑`/`↓` Take/Drop** — fundamental, easy with slice
3. **`⍋`/`⍒` Grade** — argsort is essential for many algorithms
4. **`∪` Unique** — tf.unique exists
5. **`∀`/`∃` All/Any** — tf.all/tf.any exist
6. **`,` Ravel/Flatten** — simple reshape

### Tier 2 (High Value, Moderate Effort)
1. **`⊗` Outer Product** — extremely powerful for combinatorics
2. **`⍨` Flip** — elegant, can be pure Fluent
3. **Fork/Hook** — define in prelude, no new primitives needed
4. **`⊖` Rotate** — useful for cyclic operations
5. **`!` Factorial/Binomial** — mathematical completeness

### Tier 3 (Nice to Have)
1. **`⌺` Stencil** — powerful but complex
2. **`⌹` Solve/Diagonal** — linear algebra completeness
3. **`∈` Membership** — set operations
4. **`⍳` Index Of** — searching

---

## Implementation Notes

Many can be defined in PRELUDE without new TypeScript:
```fluent
; Combinators (pure Fluent)
flip: { f | { x, y | f(y, x) } },
fork: { f, g, h | { x | g(f(x), h(x)) } },
hook: { f, g | { x | f(x, g(x)) } },
compose: { f, g | { x | f(g(x)) } },
(⍨): flip,
(∘): compose,

; Using existing ops
flatten: { x | x ⍴ [-1] },
(,): flatten,
```

For tf.js-backed ones, need new TypeScript functions:
- `TensorCumSum`, `TensorCumProd` (tf.cumsum, tf.cumprod)
- `TensorUnique` (tf.unique)
- `TensorArgSort` / `TensorGradeUp` (tf.topk based)
- `TensorAll`, `TensorAny` (tf.all, tf.any)
- `TensorSplit` (tf.split)

---

## 7. J Language — Tacit Programming Mastery

### Gerunds — `\`` (verb trains)
**J**: Package verbs as data for later application
```fluent
gerund: List,            ; verbs as list
(+, -, ×) @ 0            ; apply first verb
```

### Agenda — `@.`
**J**: Conditional verb selection
```fluent
(@.): { conds, funcs, x | funcs_(conds(x))(x) },
(neg, +1, abs) @. sign   ; neg if negative, +1 if zero, abs if positive
```

### Bond/Curry — `&`
**J**: Partial application
```fluent
(&): { f, a | { x | f(a, x) } },
double: 2 & ×,
double(5)                ; → 10
```

### Rank — `"`
**J**: Control rank of operation (apply at specific dimensions)
```fluent
("): TensorRank,
sum " 1                  ; sum along axis 1
f " [0, 1]               ; apply f at different ranks for each arg
```

### Under — `&.`
**J**: Conjugation — apply f, transform, apply g, untransform
```fluent
(&.): { f, g | { x | g⁻¹(f(g(x))) } },
round &. (×100)          ; round to 2 decimal places
sort &. ⊂               ; sort boxed arrays
```

### Obverse — `:.`
**J**: Define inverse of a function
```fluent
(:.): { f, f_inv | ... },
encode :. decode,
```

### Power — `^:`
**J**: Apply function n times (already have ⟳, but conditional version)
```fluent
(^:): { f, n | { x | f ⟳ n } },
f ^: _ x                 ; apply until convergence
f ^: (cond) x            ; apply while condition holds
```

---

## 8. K/Q Language — Extreme Terseness

### Each-Left/Each-Right — `\:` / `/:`
**K**: Map with one fixed argument
```fluent
(\:): { f | { x, ys | ys ListMap { y | f(x, y) } } },
(/:): { f | { xs, y | xs ListMap { x | f(x, y) } } },
10 -\: [1,2,3]           ; → [9, 8, 7]
[1,2,3] -/: 10           ; → [-9, -8, -7]
```

### Over/Scan — `/` `\` as adverbs
**K**: Reduce and scan with initial value option
```fluent
+/ [1,2,3,4]             ; → 10 (reduce)
+\ [1,2,3,4]             ; → [1,3,6,10] (scan)
0 +/ [1,2,3]             ; → 6 (with initial)
```

### Converge — `/` (monadic)
**K**: Apply until fixed point
```fluent
converge: { f, x | ... until f(x) = x },
{x % 2}/[1000]           ; → 0 (halve until 0)
```

### Windows — `'`
**K**: Sliding windows
```fluent
('): TensorWindows,
3 ' [1,2,3,4,5]          ; → [[1,2,3],[2,3,4],[3,4,5]]
```

### Prior — `':`
**K**: Apply to each pair (current, previous)
```fluent
(':): TensorPrior,
-': [1,3,6,10]           ; → [1,2,3,4] (differences)
```

### Cross — `,\:`
**K**: Cartesian product
```fluent
cross: { a, b | a ,\: b },
[1,2] cross ["a","b"]    ; → [[1,"a"],[1,"b"],[2,"a"],[2,"b"]]
```

---

## 9. BQN — Modern Array Language

### Before/After — `⊸` / `⟜`
**BQN**: Compose with argument binding
```fluent
(⊸): { f, g | { x | f(g(x), x) } },  ; (g x) f x
(⟜): { f, g | { x | f(x, g(x)) } },  ; x f (g x)
-⊸÷                      ; (neg x) / x = -1
÷⟜2                      ; x / 2
```

### Atop/Over — `∘` / `○`
**BQN**: Function composition variants
```fluent
(∘): { f, g | { x, y | f(g(x, y)) } },     ; f(g(x,y))
(○): { f, g | { x, y | f(g(x), g(y)) } },  ; f(g(x), g(y))
+○abs                    ; abs(x) + abs(y)
```

### Repeat — `⍟`
**BQN**: Apply n times or until condition
```fluent
(⍟): { f, n | f ⟳ n },
double ⍟ 3               ; double 3 times
```

### Cells — `˘`
**BQN**: Apply to major cells (leading axis)
```fluent
(˘): TensorCells,
reverse˘ [[1,2],[3,4]]   ; reverse each row
```

### Each — `¨`
**BQN**: Map (explicit)
```fluent
(¨): ListMap,
+1¨ [1,2,3]              ; → [2,3,4]
```

### Table — `⌜`
**BQN**: Outer product (all combinations)
```fluent
(⌜): TensorTable,
×⌜ [1,2,3]               ; multiplication table
```

### Fold — `´`
**BQN**: Explicit reduce
```fluent
(´): ListReduce,
+´ [1,2,3,4]             ; → 10
```

### Insert — `˝`
**BQN**: Reduce along first axis
```fluent
(˝): TensorReduceFirst,
+˝ [[1,2],[3,4]]         ; → [4,6]
```

### Group — `⊔`
**BQN**: Group elements by key
```fluent
(⊔): TensorGroup,
[0,1,0,1] ⊔ [1,2,3,4]    ; → [[1,3],[2,4]]
```

### Classify — `⊐` / `⊒`
**BQN**: Index of / progressive index of
```fluent
(⊐): TensorClassify,
"abcabc" ⊐ "abc"         ; → [0,1,2,0,1,2]
```

### Occurrence Count — `⊒`
**BQN**: Count of each element so far
```fluent
(⊒): TensorOccurrence,
⊒ "abcabc"               ; → [0,0,0,1,1,1]
```

### Mark Firsts — `⊑`
**BQN**: Boolean mask of first occurrences
```fluent
(⊑): TensorMarkFirsts,
⊑ [1,2,1,3,2]            ; → [1,1,0,1,0]
```

---

## 10. Haskell — Functional Purity

### Functor Map — `<$>` or `fmap`
```fluent
(<$>): { f, x | f(x) },  ; just map in our context
```

### Applicative — `<*>`
```fluent
(<*>): { fs, xs | ... }, ; apply list of functions to list of values
[(+1), (*2)] <*> [1,2]   ; → [2,3,2,4]
```

### Bind/FlatMap — `>>=`
```fluent
(>>=): { xs, f | flatten(xs ListMap f) },
[1,2] >>= { x | [x, x*2] }  ; → [1,2,2,4]
```

### Kleisli Composition — `>=>`
```fluent
(>=>): { f, g | { x | f(x) >>= g } },
```

### Arrow Combinators — `&&&`, `***`, `|||`
```fluent
(&&&): { f, g | { x | (f(x), g(x)) } },      ; fanout
(***): { f, g | { (x,y) | (f(x), g(y)) } },  ; split
(|||): { f, g | { e | either(f, g, e) } },   ; fanin
```

### Fix Point — `fix`
**Haskell**: Y combinator, self-referential definitions
```fluent
fix: { f | f(fix(f)) },
factorial: fix({ f, n | n ≤ 1 ? 1 : n × f(n-1) }),
```

### Memoize
```fluent
memo: { f | ... cached version ... },
fib: memo({ f, n | n ≤ 1 ? n : f(n-1) + f(n-2) }),
```

### Zip/ZipWith
```fluent
zip: { a, b | ... },
zipWith: { f, a, b | ... },
[1,2,3] zipWith(+) [4,5,6]  ; → [5,7,9]
```

### Unzip
```fluent
unzip: { pairs | (pairs ListMap first, pairs ListMap second) },
```

---

## 11. Category Theory Abstractions

### Identity — `id` or `𝟙`
```fluent
id: { x | x },
(𝟙): id,
```

### Constant — `const` or `K`
```fluent
const: { a | { _ | a } },
K: const,
K(5)(anything)           ; → 5
```

### Flip/C combinator
```fluent
C: { f | { x, y | f(y, x) } },
```

### Compose/B combinator
```fluent
B: { f, g | { x | f(g(x)) } },
```

### Substitution/S combinator
```fluent
S: { f, g | { x | f(x)(g(x)) } },
```

### Duplicator/W combinator
```fluent
W: { f | { x | f(x)(x) } },
```

### Blackbird/B1 combinator
```fluent
B1: { f, g, h | { x | f(g(h(x))) } },
```

### Phoenix/Φ combinator
```fluent
Φ: { f, g, h | { x | f(g(x), h(x)) } },  ; same as fork!
```

### Psi/on combinator
```fluent
on: { f, g | { x, y | f(g(x), g(y)) } },
compare on abs,          ; compare by absolute value
```

### Bifunctor — `bimap`
```fluent
bimap: { f, g, (a, b) | (f(a), g(b)) },
```

### Profunctor — `dimap`
```fluent
dimap: { f, g, h | { x | g(h(f(x))) } },
```

---

## 12. Signal Processing

### FFT/IFFT — `ℱ` / `ℱ⁻¹`
```fluent
(ℱ): TensorFFT,
(ℱ⁻¹): TensorIFFT,
ℱ [1,0,1,0]              ; frequency domain
ℱ⁻¹(ℱ(x))               ; → x (round-trip)
```

### Convolution — `⊛` or `∗`
```fluent
(⊛): TensorConvolve,
signal ⊛ kernel,
[1,2,3] ⊛ [0.5, 0.5]     ; moving average
```

### Correlation — `⋆`
```fluent
(⋆): TensorCorrelate,
signal ⋆ template,       ; cross-correlation
signal ⋆ signal,         ; auto-correlation
```

### Differentiate/Integrate (discrete)
```fluent
(∂): TensorDiff,         ; differences
(∫): TensorCumSum,       ; cumulative sum (discrete integral)
∂ [1,3,6,10]             ; → [2,3,4]
∫ [2,3,4]                ; → [2,5,9]
```

### Interpolate
```fluent
lerp: { t, a, b | a + t × (b - a) },
interp: { xs, ys, x | ... },  ; interpolate
```

### Resample
```fluent
resample: { x, n | ... },
upsample: { x, n | ... },
downsample: { x, n | ... },
```

### Filter (IIR/FIR)
```fluent
fir: { coeffs, x | x ⊛ coeffs },
iir: { a, b, x | ... },
```

### Window Functions
```fluent
hann: { n | 0.5 × (1 - cos(2π × (0::n) / n)) },
hamming: { n | 0.54 - 0.46 × cos(2π × (0::n) / n) },
blackman: { n | ... },
kaiser: { n, β | ... },
```

---

## 13. Statistics & Probability

### Variance/StdDev — `σ²` / `σ`
```fluent
(σ²): TensorVariance,
(σ): TensorStdDev,
σ [1,2,3,4,5]            ; → 1.414...
```

### Covariance/Correlation Matrix
```fluent
cov: TensorCovariance,
corr: TensorCorrelation,
```

### Percentile/Quantile — `℘`
```fluent
(℘): TensorQuantile,
0.5 ℘ [1,2,3,4,5]        ; → 3 (median)
[0.25, 0.5, 0.75] ℘ x    ; quartiles
```

### Histogram
```fluent
hist: { x, bins | ... },
```

### Random Distributions
```fluent
uniform: { lo, hi, shape | ... },
normal: { μ, σ, shape | ... },
poisson: { λ, shape | ... },
bernoulli: { p, shape | ... },
categorical: { probs | ... },
```

### Sampling
```fluent
sample: { x, n | ... },          ; sample n from x
sampleWith: { x, n | ... },      ; with replacement
shuffle: { x | ... },
```

### Moments
```fluent
moment: { x, k | μ(x^k) },       ; k-th moment
skewness: { x | ... },
kurtosis: { x | ... },
```

### Z-score / Standardize
```fluent
zscore: { x | (x - μ(x)) / σ(x) },
standardize: zscore,
```

### Moving Statistics
```fluent
movingAvg: { x, n | ... },
movingStd: { x, n | ... },
ewma: { x, α | ... },            ; exponentially weighted moving average
```

---

## 14. Linear Algebra Deep Dive

### Decompositions
```fluent
svd: TensorSVD,                  ; singular value decomposition
qr: TensorQR,                    ; QR decomposition
lu: TensorLU,                    ; LU decomposition
cholesky: TensorCholesky,        ; Cholesky decomposition
eig: TensorEigen,                ; eigenvalues/vectors
```

### Matrix Properties
```fluent
det: TensorDeterminant,
tr: TensorTrace,                 ; trace (sum of diagonal)
rank: TensorRank,
cond: TensorConditionNumber,
```

### Matrix Operations
```fluent
inv: TensorInverse,
pinv: TensorPseudoInverse,       ; Moore-Penrose
(†): TensorConjugateTranspose,   ; Hermitian transpose
```

### Norms
```fluent
norm: { x, p | ... },
frobenius: { x | norm(x, "fro") },
nuclear: { x | Σ(svd(x)_1) },    ; nuclear norm
spectral: { x | max(svd(x)_1) },
```

### Special Matrices
```fluent
zeros: { shape | fill(shape, 0) },
ones: { shape | fill(shape, 1) },
diag: { v | ... },               ; diagonal matrix from vector
triu: TensorUpperTriangular,
tril: TensorLowerTriangular,
toeplitz: { c, r | ... },
hankel: { c, r | ... },
circulant: { c | ... },
vandermonde: { x, n | ... },
```

### Kronecker/Hadamard Products
```fluent
(⊗): TensorKronecker,            ; Kronecker product
(⊙): TensorHadamard,             ; element-wise (already have ×)
```

### Solve Systems
```fluent
solve: { A, b | A ⌹ b },
lstsq: { A, b | ... },           ; least squares
```

---

## 15. Set Operations

### Union/Intersection/Difference
```fluent
(∪): TensorUnion,
(∩): TensorIntersection,
(∖): TensorSetDifference,
(△): TensorSymmetricDifference,
```

### Subset/Superset
```fluent
(⊂): TensorSubset,               ; proper subset
(⊆): TensorSubsetEq,
(⊃): TensorSuperset,
(⊇): TensorSupersetEq,
```

### Power Set
```fluent
(℘): TensorPowerSet,
℘ [1,2,3]                        ; → [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]
```

---

## 16. String/Sequence Operations (if extended beyond tensors)

### Match/Find
```fluent
match: { pattern, text | ... },
findAll: { pattern, text | ... },
```

### Split/Join
```fluent
split: { sep, s | ... },
join: { sep, xs | ... },
```

### Trim/Pad
```fluent
trim: { s | ... },
padLeft: { s, n, c | ... },
padRight: { s, n, c | ... },
```

---

## 17. Control Flow & Iteration

### While/Until
```fluent
while: { cond, f, x | ... },
until: { cond, f, x | ... },
```

### Iterate with Index
```fluent
imap: { f, xs | xs ListMap.indexed f },  ; f receives (index, value)
```

### Find First
```fluent
find: { pred, xs | ... },
findIndex: { pred, xs | ... },
```

### Take While / Drop While
```fluent
takeWhile: { pred, xs | ... },
dropWhile: { pred, xs | ... },
```

### Span / Break
```fluent
span: { pred, xs | (takeWhile(pred, xs), dropWhile(pred, xs)) },
break: { pred, xs | span(¬ ∘ pred, xs) },
```

### Partition
```fluent
partition: { pred, xs | (filter(pred, xs), filter(¬∘pred, xs)) },
```

### Group By
```fluent
groupBy: { f, xs | ... },
[1,2,3,4,5] groupBy odd  ; → [[1,3,5], [2,4]]
```

### Chunks / Windows
```fluent
chunks: { n, xs | ... },         ; non-overlapping
windows: { n, xs | ... },        ; overlapping (sliding)
```

---

## 18. Geometry & Graphics

### Vector Operations
```fluent
cross3: { a, b | ... },          ; 3D cross product (dot already exists)
normalize: { v | v / ‖v‖ },
project: { a, b | ... },         ; project a onto b
reject: { a, b | ... },          ; component perpendicular to b
reflect: { v, n | ... },
```

### Angles
```fluent
angle: { a, b | acos(dot(a,b) / (‖a‖ × ‖b‖)) },
atan2: TensorAtan2,
```

### Rotation Matrices
```fluent
rot2d: { θ | [[cos(θ), -sin(θ)], [sin(θ), cos(θ)]] },
rotX: { θ | ... },
rotY: { θ | ... },
rotZ: { θ | ... },
```

### Distance Functions
```fluent
euclidean: { a, b | ‖a - b‖ },
manhattan: { a, b | Σ(abs(a - b)) },
chebyshev: { a, b | max(abs(a - b)) },
cosine: { a, b | 1 - dot(a,b)/(‖a‖×‖b‖) },
```

### Bounding / Clipping
```fluent
bbox: { points | (min(points), max(points)) },
clip: { lo, hi, x | lo ⌈ (x ⌊ hi) },
```

---

## 19. Differential Equations & Calculus

### Numerical Derivatives
```fluent
deriv: { f, x, h | (f(x+h) - f(x-h)) / (2×h) },
grad: TensorGradient,            ; already have ∇
jacobian: { f, x | ... },
hessian: { f, x | ... },
```

### Integration
```fluent
trapz: { x, y | ... },           ; trapezoidal rule
simpson: { x, y | ... },
romberg: { f, a, b | ... },
```

### ODE Solvers
```fluent
euler: { f, y0, t | ... },
rk4: { f, y0, t | ... },         ; Runge-Kutta 4
```

---

## 20. Neural Network Primitives

### Activations
```fluent
relu: { x | x ⌈ 0 },
leakyRelu: { α, x | x ⌈ (α × x) },
gelu: { x | ... },
silu: { x | x × sigmoid(x) },
sigmoid: { x | 1 / (1 + exp(-x)) },
softmax: TensorSoftmax,
softplus: { x | log(1 + exp(x)) },
```

### Loss Functions
```fluent
mse: { y, ŷ | μ((y - ŷ)²) },
mae: { y, ŷ | μ(abs(y - ŷ)) },
crossEntropy: { y, ŷ | -Σ(y × log(ŷ)) },
binaryCrossEntropy: { y, ŷ | ... },
huber: { δ, y, ŷ | ... },
```

### Layers (as higher-order functions)
```fluent
dense: { w, b | { x | x matmul w + b } },
conv2d: { kernel | { x | x ⊛ kernel } },
dropout: { p | { x | x × (rand(shape(x)) > p) / (1-p) } },
batchNorm: { γ, β | { x | γ × zscore(x) + β } },
layerNorm: { γ, β | { x | ... } },
```

### Attention
```fluent
attention: { Q, K, V | softmax(Q matmul K† / √d) matmul V },
multiHead: { heads, Wq, Wk, Wv, Wo | ... },
```

---

## 21. Bit Manipulation (if integers are supported)

```fluent
(⊕): TensorBitXor,
(⊖): TensorBitOr,              ; if not used for rotate
(⊗): TensorBitAnd,
(≪): TensorShiftLeft,
(≫): TensorShiftRight,
popcount: TensorPopCount,       ; count 1 bits
clz: TensorCountLeadingZeros,
ctz: TensorCountTrailingZeros,
```

---

## 22. Lazy / Infinite Sequences (if supported)

```fluent
iterate: { f, x | ... },        ; [x, f(x), f(f(x)), ...]
repeat: { x | ... },            ; [x, x, x, ...]
cycle: { xs | ... },            ; [xs..., xs..., ...]
naturals: iterate((+1), 0),
primes: { ... },
fibonacci: { ... },
```

---

## 23. Monadic Error Handling

```fluent
maybe: { default, f, x | x = null ? default : f(x) },
either: { onLeft, onRight, e | ... },
try: { f, x | ... },            ; returns (result, error)
catch: { handler, f, x | ... },
```

---

## 24. Unicode Operator Aesthetic Menu

For maximum APL aesthetic, here are beautiful Unicode operators:

| Symbol | Name | Meaning |
|--------|------|---------|
| `⊕` | circled plus | XOR, direct sum |
| `⊖` | circled minus | rotate, symmetric diff |
| `⊗` | circled times | outer product, tensor product |
| `⊘` | circled division | ??? |
| `⊙` | circled dot | Hadamard product |
| `⊚` | circled ring | ??? |
| `⊛` | circled asterisk | convolution |
| `⊜` | circled equals | ??? |
| `⊝` | circled dash | ??? |
| `⟨⟩` | angle brackets | vector literal |
| `⟪⟫` | double angle | matrix literal |
| `⌈⌉` | ceiling | round up |
| `⌊⌋` | floor | round down |
| `∂` | partial | derivative |
| `∫` | integral | integration |
| `∬` | double integral | ??? |
| `∮` | contour integral | ??? |
| `∇` | nabla | gradient |
| `△` | triangle | symmetric difference, Laplacian |
| `□` | box | modal necessity? |
| `◇` | diamond | modal possibility? |
| `★` | star | special operation |
| `☆` | white star | conjugate? |
| `♯` | sharp | cardinal, count |
| `♭` | flat | flatten |
| `♮` | natural | ??? |
| `†` | dagger | conjugate transpose |
| `‡` | double dagger | ??? |
| `⁺` | superscript plus | positive part |
| `⁻` | superscript minus | negative part, inverse |
| `ℕ` | naturals | natural numbers |
| `ℤ` | integers | integers |
| `ℚ` | rationals | rationals |
| `ℝ` | reals | real numbers |
| `ℂ` | complex | complex numbers |
| `∞` | infinity | infinity |
| `∅` | empty set | null, empty |
| `⊤` | top | true |
| `⊥` | bottom | false |
| `⊢` | right tack | right identity, right |
| `⊣` | left tack | left identity, left |
| `⫽` | double slash | parallel |
| `⟂` | perpendicular | orthogonal |

---

## Summary: The Ultimate Fluent Wishlist

### Must Have (Foundation)
1. Scan `⍀` — running reductions
2. Grade `⍋⍒` — argsort
3. Take/Drop `↑↓` — slicing
4. Unique `∪` — deduplication
5. All/Any `∀∃` — boolean reduce
6. Flatten `,` — ravel

### Should Have (Power)
1. Outer product `⊗` — combinatorics
2. Flip `⍨` — argument swap
3. Fork/Hook — tacit programming
4. Windows/Prior — sliding operations
5. Group `⊔` — categorization

### Could Have (Polish)
1. Each-left/right `\:/:` — partial mapping
2. Under `&.` — conjugation
3. FFT `ℱ` — signal processing
4. SVD/decompositions — linear algebra
5. Distributions — statistics

### Dream Features
1. Stencil `⌺` — cellular automata
2. Lazy sequences — infinite streams
3. Pattern matching — destructuring
4. Macros — code generation
5. Dependent types — proof carrying code
