# Fluent style — a proposal

*How to lay out Fluent so the notation does the work. Sources: a verified
catalogue of the grammar's layout rules (every example below was run), a survey
of the gallery, the Prelude and the Tour, and prior art from APL, J, K, BQN,
Uiua, Smalltalk, Rebol, Fortress, Nim, Go, TeX and mathematical writing.*
2026-10-17

## 0. The idea

In most languages, layout is a courtesy to the reader and the parser ignores it.
In Fluent the layout **is** the parse. Glue binds, a space sequences, and a
spaced chain runs left to right. So the formatting is not decoration. Each time
you write a line you decide which parts of the formula are **terms**, read at a
glance, and which are **steps**, read in order.

When those decisions follow the mathematics, the code reads like the formula:

```
√(θ_0^2 + θ_1^2)                 ; the length of θ
x×W ÷ (2×π)                      ; x times the width, over a full turn
(picture^γ × (1 - cover) + color^γ×cover) ^ (1 ÷ γ)
```

The last line is the stroke mix from `draw` in the Prelude: the two colors mixed
in linear light, then encoded for the screen. Its terms are `picture^γ` and
`color^γ×cover`, and its steps are `×`, `+` and `^`. Fluent would read it the
same way without any of its parentheses. They are there for a reader who
learned that × comes before +, and with them that reader reads it right too.

Four rules carry the guide:

1. **Glue a term, space a step.**
2. **Write it in the order it happens.**
3. **Keep a parenthesis that changes the parse or that a reader trained on
   school precedence needs. Drop the rest.**
4. **Name what recurs, a binary operation included: an operator of your own
   is welcome.**

## 1. Space is grammar

**1.1 Two widths: none or one space.** Never put two spaces inside an
expression. Never pad comments or bindings to line them up: extra width
suggests a tier that does not exist, and alignment makes every later edit touch
the neighbouring lines.

```
loss: Σ(err^2) ÷ n,
```
not `loss:  Σ(err^2)  ÷  n,`

The exception is **data laid out as a table**. You may pad after the commas so
the columns line up, in the rows of a matrix or in a table of calls such as the
gallery's `under`:

```
M: [[ 1,  0, -1],
    [10, 20, 30]],
```

**1.2 Glue a term.** A term is what a mathematician typesets without space:
- a power `x^2`
- an index `θ_0`
- a range `0..<n`
- a short product or quotient `2×π`, `h×ab`, `H÷2`
- an offset `n-1`
- a call `f(x)`

```
√(θ_0^2 + θ_1^2)                 ; = 5 for θ = [3, 4]
curve: stack((x×W ÷ (2×π), H÷2 - y×H÷2.4), -1),
```

**1.3 A term reads left to right too.** The tight tier has no precedence
either. `1-x^2` is `(1-x)^2`, which is 4 for x = 3, not −8. So:
- glue only what reads correctly left to right;
- keep a term short: two or three atoms;
- when a term would need grouping inside it, space it out or name a piece.

```
1 - x^2                          ; -8 for x = 3: the term is x^2
(1-x)^2                          ;  4: what 1-x^2 means, written so it reads that way
```

**1.4 Space a step.** A spaced operator is a step in a sentence read left to
right:

```
0..<K ÷ K × 2×π                  ; the first K integers, over K, times a full turn
```

**1.5 Glue both sides or neither. Right-only glue is forbidden.** A space before
an operator and none after it (`a ^2`) parses the same as a spaced operator, so
it lies about grouping. It also changes meaning before `(` (`x ×(y + 1)` is a
call) and before a digit (`x -1` is a call of x).

The one sanctioned asymmetry is a **binder** glued to its name: `name: …`,
`s← …` and `θ:= …`. A binder written this way takes the whole right side:

```
a: 1 + 2                         ; a = 3
s← 1 + 2                         ; s = 3
```
A spaced binder takes only the next term:
```
x : 1 ..< 10                     ; x = 1
s ← 1 + 2                        ; s = 1
```

**1.6 A function with a name is called: `f(x)`, `sin(x)`, `Σ(x)`.** `Σ`, `Π` and
`μ` are names, Greek letters, so `Σx` is one unbound name; call them like
words.

**1.7 A prefix glyph is applied by juxtaposition: `(√x)`, not `√(x)`.** This
covers `√ - # ∇ ¬ ~ $`. The two forms parse to the same tree. `(√x)` reads as
the mathematics does, and its parentheses close the application, so nothing
after it can be swallowed:

```
(√x) + 1                         ; 3 for x = 4
2 × (-x)                         ; -4 for x = 2
(#p) - 1
(∇f)(3)                          ; 6 for f = x²
```

When the application is a whole argument, element or right side, it needs no
parentheses:

```
exp(-x)
[√x, 1]
y: √x,
θ: ~[0, 0],
s: $0.5,
g: ∇f,
```

The operand is a single atom: a name, a number, a call, a literal or a
parenthesized group. Anything longer takes the parenthesized form, because the
glyph takes the whole glued term after it. `(√x*4)` is √(4x), which a reader
takes for (√x)·4:

```
√(x*4)
√(a + b)
```

Bare `√x + 1` is not Fluent yet. It errors, because x becomes an operator.

**1.8 Break lines where there is already a space:** after a comma, or before a
spaced operator. A pipe continues on the next line with a leading `.`:

```
canvas(W, H)
  . draw(line(axis), gray, 1)
  . draw(line(curve), blue, 2.5)
```

Never break right after a glued operator. The break becomes a space and changes
the parse:

```
x^
2 + 1                            ; 27 for x = 3: x^(2 + 1)
```

**1.9 A newline is not a comma.** Every statement ends with `,`, a comment
included: `a: 1 ; one` on one line and `b: 2` on the next is an error.

## 2. Reading order

**2.1 The order you read is the order it runs.** A Fluent line should be
readable as a sentence from left to right, each step acting on everything
before it.

**2.2 Pipe a value through steps.** When each step is a function of the value
so far, `.` lets the code read in data-flow order instead of inside out:

```
canvas(W, H) . draw(line(axis), gray, 1) . draw(dots(marks), magenta, 7)
```

**2.3 Order a program for exposition:** constants, data, model, loss, update,
and the view last. A reader should never have to read upwards.

## 3. Parentheses

A parenthesis earns its place in one of two ways. It changes the parse, or it
makes the reading of someone trained on school precedence agree with Fluent's.
Anything else is noise, and worse than noise: it suggests a grouping that isn't
there.

**3.1 Needed by the parse:**
- a right-hand sub-chain that must happen first: `x × (y + 1)`,
  `(x < 1) ∧ (y > 2)`
- an operator-valued expression used as a function or an operand:
  `(f ∘ g)(x)`, `(⍨ -)`, `a (⊗ f) b`, `(double ⍣ 5)(1)`
- a tuple: `stack((a, b), -1)`, `(1, 2) . add`
- a negative number glued after an operator, where the glyphs would merge:
  `2^(-1)`, or space it: `2 ^ -1`

**3.2 Welcome for a reader trained on school precedence.** That reader applies
`^` first, then `× ÷`, then `+ -`, then comparisons, then `¬ ∧ ∨`, whatever the
spacing. Wherever a spaced chain or a glued term means something else in
Fluent, add the parentheses that make both readings agree. The tree doesn't
change:

```
(a + b) × c                      ; Fluent reads a + b × c the same way
(t - v_0) ÷ (v_1 - v_0)          ; t - v_0 ÷ (v_1 - v_0) is the same tree
(1-x)^2                          ; 1-x^2 is the same tree
x×W ÷ (2×π)                      ; school arithmetic reads x×W ÷ 2×π as (xW ÷ 2)·π
((a × t - b) × t + c) × t        ; Horner, which Fluent reads unparenthesized
```

Gluing can make both readings agree without any parentheses. This is the one
systematic misreading:

```
θ - lr×g(θ)                      ; 8 for θ = 10, lr = 0.1, g = 2x
θ - lr × g(θ)                    ; 198: (θ - lr) × g(θ)
```

Operators that school arithmetic doesn't know (`⍴ _ ..< ⌈ ⌊ ⊗ ⍣ . @`) carry no
expectation, so they need no extra parentheses beyond the parse.

**3.3 Noise:**
- around a glued term: `(θ_0)`, `(x^2)`, `(zx^2 + zy^2)` where nothing
  misreads it
- doubled: `((a + b))`
- around a call, around the right side of `name:`, around a lambda body
- around an application that is a whole argument: `exp((-x))`

```
alive: zx^2 + zy^2 ≤ 4,          ; not ((zx^2 + zy^2) ≤ 4)
board← a=3 ⌈ (a=2 × c),          ; not board ← ((a = 3) ⌈ ((a = 2) × c))
```

**3.4 Comparisons chain left to right.** `0 < x < 1` compares a boolean with 1,
and `x < 1 ∧ y > 2` is `((x < 1) ∧ y) > 2`. Glue each comparison and space the
logic. Then both readings agree:

```
x<1 ∧ y>2                        ; 1 for x = 0.5, y = 3; the spaced form gives 0
0<x ∧ x<1
```

## 4. Operators of your own

**Define an operator whenever a binary operation recurs or has a sign of its
own.** An operator is a value like any other. Bind it in parentheses, then use
it infix. It obeys the same tiers as `+`: glued it is a term, spaced it is a
step, with no precedence of its own.

```
(∥): reciprocal ⍢ +,             ; resistors in parallel: 6 ∥ 3 = 2
(≻): { t, v | v_0 × (1 - t) + v_1×t },   ; lerp: 0.25 ≻ [0, 8] = 2
(++): ListConcat,
```

- Bind it in parentheses: `(⊕): …`. A glued `⊕:` lexes as one token.
- The glyph must come from the operator blocks: mathematical operators,
  miscellaneous technical, arrows, and their supplements. A letter is a name.
- Point-free is natural for an operator built from others (`reciprocal ⍢ +`).
  A lambda `{ a, b | … }` works when the operands need names.
- Any value works infix, so a word does too: `a matmul b`, `1 add 2`. A
  section works the same way: `3 (⍨ -) 10`.
- Pick the glyph for its shape. `∥` looks like parallel resistors and `≻`
  points from one end to the other. A reader should be able to guess what it
  does.
- Keep one meaning per glyph within a program, and say what it is in a comment
  where it's bound.

## 5. Names

**5.1 One binding per line, the name at the left edge, the colon glued.** The
left column then reads as the program's outline. Constants that belong together
may share a line: `W: 640, H: 400,`.

**5.2 Name a concept, inline a step.** Name something when the reader will treat
it as a unit or when it recurs (`dist`, `cover`, `along`). Don't name a one-off
intermediate.

**5.3 Write what a constant means:** `2×π`, not `6.2832` or `6.28`; `π`, not
`PI: 3.14159`. A color can be written as you would look it up:
`[10, 129, 239] ÷ 255`. A number used twice, or one whose meaning isn't
obvious, gets a name. Fluent has no `τ`, `e` or `∞`: an unbound name evaluates
to a bare symbol, with no error until arithmetic meets it.

**5.4 Make name length grow with scope.** Use maths-short names for
mathematical things in a small scope (`x`, `i`, `θ`, `ab`) and words for
program-level things (`curve`, `marks`, `losses`).

**5.5 Use the Prelude's words** instead of re-deriving them: `norm`, `hypot`,
`meshgrid`, `zerosLike`, `slice(s, [-T])` for the last T, `cond`, `scan`,
`silu`, `sigmoid`, `logSoftmax`. Don't redefine a Prelude name with a different
meaning (`normalize`, `σ`, `μ`).

**5.6 Glyph or word.** Use the glyph where it is the established sign with one
spoken reading (`Σ √ π ∇ ⍴ × ÷`), in dense mathematics. Use the word for domain
concepts (`softmax`, `adam`). Pick one per concept within a program.

## 6. Shape

**6.1 A line is one thought,** about seven operations at most, and a
definition fits on a screen.

**6.2 Lambdas.** Write a one-liner as `{ x | x^2 }`. A multi-line body puts one
binding per line, indented two spaces, with `}` on its own line:

```
line: { points |
  n: length(points) - 1,
  a: points_(0..<n),
  ab: points_(1 ..< n+1) - a,
  along: { ap | clamp(Σ(ap × ab, -1, 1) ÷ (Σ(ab × ab, -1, 1) ⌈ 1e-12), 0, 1) },
  { p | ap: unsqueeze(p, -2) - a, min(norm(ap - along(ap)×ab, -1), -1) }
},
```

**6.3 Indent two spaces,** everywhere.

**6.4 Sections:** separate them with a blank line, and start each with a
full-line comment.

**6.5 Literals:** `[1, 2, 3]`, with a space after each comma. A matrix puts one
row per line, and may align its columns (1.1).

**6.6 The program's value** is a final tuple, one item per line, with a
trailing comma:

```
(
  Text("# ✏️ Drawing as values"),
  canvas(W, H)
    . draw(line(curve), blue, 2.5),
)
```

## 7. Comments

- One space after the code, never aligned with other comments.
- Say why, or state something the reader can check: a shape (`; [n, 2m]`), an
  identity, a unit. Don't restate the code.
- Describe the code as it is now. History goes in the commit message.
- A program opens with `; Title – what it shows`, using an en dash.
- Never write `;;…;;`: the IDE reads it as a prompt for the model.
- Plain words: no capitals for emphasis, no exclamation marks.

## 8. Traps

Every row is verified. The left column is what a careful person writes when
tidying up.

| looks right | means | write |
|---|---|---|
| `1-x^2` | `(1-x)^2` | `1 - x^2`, or `(1-x)^2` if that is meant |
| `0..<n+1` | `(0..<n) + 1` | `0 ..< n+1` |
| `x*θ_0` | `(x*θ)_0` | `x × θ_0` |
| `a_i+1` | `(a_i) + 1` | `a_(i + 1)` |
| `θ - lr × g(θ)` | `(θ - lr) × g(θ)` | `θ - lr×g(θ)` |
| `x < 1 ∧ y > 2` | `((x < 1) ∧ y) > 2` | `x<1 ∧ y>2` |
| `0 < x < 1` | `(0 < x) < 1` | `0<x ∧ x<1` |
| `2^3^2` | `(2^3)^2` = 64 | `2^(3^2)` if a tower is meant |
| `x : 1 ..< 10` | x = 1 | `x: 1 ..< 10` |
| `s ← a + b` | s = a | `s← a + b` |
| `-x + 3`, `√x + 1`, `#p - 1` | an error | `(-x) + 3`, `(√x) + 1`, `(#p) - 1` |
| `2 × √x` | a parse error | `2 × (√x)` |
| `(√x*4)` | √(4x) | `√(x*4)`, or `(√x)*4` |
| `3×-2`, `2^-1`, `a_-1` | one merged glyph, an error | `3 × -2`, `2^(-1)`, `a_(-1)` |
| `x ×(y + 1)` | a call of x | `x × (y + 1)` |
| `x^` then a newline | the break is a space | break before or after a spaced operator |
| `2π`, `2x` | a call of 2 | `2×π`, `2×x` |
| `Σx`, `μx` | one unbound name | `Σ(x)`, `μ(x)` |
| `{ x }` as identity | a thunk returning the name x | `{ x | x }` |
| `τ`, `e`, `∞` | unbound symbols | `2×π`, `exp(1)`, `1 ÷ 0` |

## 9. Before and after

Each rewrite parses to the same tree as its original, except where a note says
it trades a word for a clearer one; those were checked by value. All were run
with Bun.

The drawing I first showed you:
```
toPx: { x, y | stack((x ÷ 6.2832 × W, (1.2 - y) ÷ 2.4 × H), -1) },
xs: linspace([0, 6.2832], 128),
paint: { base, cover, color | c: cover ⍴ [H, W, 1], base × (1 - c) + (c × color) },
```
as it is in the gallery now:
```
x: linspace([0, 2×π], 128),
y: sin(x + t),
curve: stack((x×W ÷ (2×π), H÷2 - y×H÷2.4), -1),
```

Linear regression, compressed:
```
y: (x × 0.23 + 0.47),
f: { x | x × (θ_0) + (θ_1) },
𝓛: { μ((y - f(x)) ^ 2) },
```
```
y: x×0.23 + 0.47,
f: { x | x × θ_0 + θ_1 },
𝓛: { μ((y - f(x))^2) },
```

Lerp:
```
≻ : { t, v | (v_0 × (1 - t)) + (v_1 × t) },
```
```
(≻): { t, v | v_0 × (1 - t) + v_1×t },
```

The Game of Life update. Besides the layout, `⌈` and `×` become `∨` and `∧`,
which say what they mean and give the same 0s and 1s:
```
board ← ((a = 3) ⌈ ((a = 2) × c)),
```
```
board← a=3 ∨ (a=2 ∧ c),
```

Growing automata:
```
targetG: (((dxc^2) + (dyc^2)) < (rad^2)),
lap: (((l + r) + u) + d) - (4 × s),
idx: ⌊((((0 ..< gres) × H) ÷ gres)),
```
```
targetG: dxc^2 + dyc^2 < rad^2,
lap: l + r + u + d - 4×s,
idx: ⌊(0..<gres × H ÷ gres),
```

## 10. What the guide asks of tooling

A formatter for Fluent may never move glue: every space it adds or removes can
change the program. It may only:
- collapse repeated spaces;
- normalize the gap before a `;` comment;
- add or remove a parenthesis that provably leaves the tree unchanged;
- flag, never fix, the shapes in §8.

Every rewrite in this guide's spirit can be checked mechanically: a style change
must leave the parse tree unchanged.

## 11. Open

1. **Bare prefixes.** With a little more parsing, `√4 + 1` could mean
   `(√4) + 1`. Then 1.7 would shrink to "write `√x`".
2. **Missing words the corpus keeps re-deriving:** a single concatenation
   operator (`++` is defined 14 times with 3 meanings), appending to a bounded
   history, named fields on grouped state, lerp. Each is a design question, not
   a style one.
