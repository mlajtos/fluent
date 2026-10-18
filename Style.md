# Fluent style

In Fluent, layout is grammar. A glued operator binds tighter than a spaced one,
and spaced operators run left to right, with no precedence. Writing Fluent well
means choosing what to glue.

## 1. Glue a term, space a step

Glue what you would write without spaces in mathematics: `x^2`, `θ_0`, `2×π`,
`n-1`, `0..<n`. Space the steps between them. Read the line left to right:

```
√(θ_0^2 + θ_1^2)     ; 5 for θ = [3, 4]
x×W ÷ 2×π            ; xW over 2π
1 + 2*3              ; 7
1 + 2 * 3            ; 9
```

A glued run also reads left to right: `1-x^2` is `(1-x)^2`. Keep glued runs
short.

Use one space or none, and never pad to line things up. The exception is the
rows of a matrix or table. Never glue one side only (`a ^2`, `x -1`): it hides
how the line groups.

## 2. Parentheses

Use a parenthesis only when it changes the parse:

```
x × (y + 1)
a + b × c            ; already (a + b) × c
```

Gluing often does the job instead. Spaced, `θ - lr × g(θ)` is
`(θ - lr) × g(θ)`. To subtract the product, write `θ - lr×g(θ)`.

Teaching material is the exception. There, a parenthesis that helps a reader
used to school precedence is welcome: `(a + b) × c`.

## 3. Calling a function

`f(x)` calls f. A function of one argument can also be put next to its
argument: `(f x)`, `(√x)`, `(-x)` mean the same as `f(x)`, `√(x)`, `-(x)`.
Pick the form that reads better: `(√x) + 1`, `√(a + b)`, `sin(x)`.

Next to its argument, the function takes the whole glued run that follows:
`(√x*4)` is `√(x*4)`. This form stands alone only as a whole argument, list
element or right side: `exp(-x)`, `y: √x`. Inside a longer expression, wrap it:
`(√x) + 1`. A bare `√x + 1` is an error.

## 4. Bindings and lines

`name: value` binds the name to everything to its right: `a: 1 + 2` is 3. Keep
the colon glued to the name. Spaced, `a : 1 + 2` binds `a` to 1. The same holds
for `s← …` and `θ:= …`.

Write one binding per line. Short related constants may share one:
`W: 640, H: 400,`.

Every statement ends with a comma; a line break does not end one. To break a long
line, break before a spaced operator, never after a glued one: `x^` followed by
`2 + 1` on the next line is `x^(2 + 1)`.

## 5. Your own operators

Define an operator whenever a binary operation recurs or has a sign of its own.
Bind the glyph in parentheses and use it infix:

```
(∥): ÷ ⍢ +,                          ; 6 ∥ 3 = 2, resistors in parallel
(≻): { t, v | v_0 × 1-t + v_1×t },   ; 0.25 ≻ [0, 8] = 2
```

It follows the same spacing rules as `+`. Any function works infix, words too:
`A matmul B`.

## 6. Names and constants

- Use short names for mathematics in a small scope (`x`, `θ`, `i`), and words
  for the things a whole program talks about (`curve`, `losses`).
- Write a constant as what it means: `2×π`, not `6.28`.
- Fluent has no `τ` or `e`. An unknown name is a symbol, and it fails only
  when arithmetic reaches it.
- Don't reuse a built-in name for something else.

## 7. Layout

- Indent two spaces.
- In a long lambda, put one binding per line and the `}` on its own line.
- A pipe continues on the next line with a leading `.`:

```
canvas(W, H)
  . draw(line(axis), gray, 1)
  . draw(line(curve), blue, 2.5)
```

A program ends with its value. Write a final tuple with one item per line and a
trailing comma.

## 8. Comments

- Put `; ` one space after the code, and never align comments.
- Say why, or give what the reader can check: a shape, a unit.
- Describe the code as it is now.
- Open a program with `; Title – what it shows`.

## 9. Traps

| looks like | is | write |
|---|---|---|
| `1-x^2` | `(1-x)^2` | `1 - x^2` |
| `0..<n+1` | `(0..<n) + 1` | `0 ..< n+1` |
| `x*θ_0` | `(x*θ)_0` | `x × θ_0` |
| `θ - lr × g(θ)` | `(θ - lr) × g(θ)` | `θ - lr×g(θ)` |
| `x < 1 ∧ y > 2` | `((x < 1) ∧ y) > 2` | `x<1 ∧ y>2` |
| `0 < x < 1` | `(0 < x) < 1` | `0<x ∧ x<1` |
| `2^3^2` | `(2^3)^2` | `2^(3^2)` |
| `x : 1 ..< 10` | `x` is 1 | `x: 1 ..< 10` |
| `√x + 1`, `-x + 3` | an error | `(√x) + 1`, `(-x) + 3` |
| `3×-2`, `2^-1` | one unknown glyph | `3 × -2`, `2^(-1)` |
| `2π`, `Σx` | a call of 2; one name | `2×π`, `Σ(x)` |
| `{ x }` | a thunk that returns the name x | `{ x \| x }` |
