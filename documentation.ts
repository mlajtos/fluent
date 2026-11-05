// The language's own documentation: bound as `Documentation` in every program.

export const Documentation = `
# Fluent

Fluent is a tiny language + IDE for differentiable tensors and reactive UIs. Fluent is for programmers, researchers, and tinkerers who want to combine math, differentiation, and interactivity in a playful way.

## Quickstart

\`\`\`fluent
; tensors + broadcasting
a: [1, 2, 3],
a + 1, ; [2, 3, 4]

; functions + gradient
square: { x | x^2 },
square(a), ; [1, 4, 9]
∇(square)(a), ; [2, 4, 6]

; reactive UI
x: $(0.5),
Slider(x),
square(x),  ; auto-lifts to reactive!
Button("Reset", { x(0) }),
\`\`\`

## Features

### Syntax
- Tensors
  - multi-dimensional arrays of numbers
  - scalars: \`1\`, \`3.14\`, \`-42\`, \`1e-7\`
  - vectors: \`[1, 2, 3]\`, matrices: \`[[1, 2], [3, 4]]\`
  - auto-broadcasting: \`[1, 2, 3] + 1\` is \`[2, 3, 4]\`
  - indexing with \`_\`: \`a_0\`, \`a_(i + 1)\`, \`a_(-1)\` (last element)
    - glued \`_\` binds tight: \`x × θ_0 + θ_1\` needs no parentheses
  - length with \`#\`: \`#([1, 2, 3])\` is \`3\`
  - ranges: \`..<\` exclusive (\`1 ..< 5\` is \`[1, 2, 3, 4]\`), \`...\` inclusive (\`1 ... 5\` is \`[1, 2, 3, 4, 5]\`)
- Lists
  - ordered collection of heterogeneous values
  - e.g. \`(1, 2, 3)\`, \`(1, (2, 3), [4])\`, \`()\`, \`(42,)\`
- Functions
  - lambda with \`{}\`: \`{ x | x + 1 }\`, \`{ x, y | x * y }\`, \`{ 42 }\`
  - last expression is the return value: \`{ a: 1, b: 2, a + b }\` returns \`3\`
  - application: \`add(1, 2)\` or infix \`1 add 2\`
  - recursion: a binding is visible in its own body, so \`fact: { n | … fact(n - 1) … }\` just works. For an ANONYMOUS function use the prelude's fixpoint: \`fix\` (also \`𝕐\`)
- Symbols
  - letter-based: \`a\`, \`FooBar\`, \`barBaz1\`, \`α\`, \`β\`, \`θ\` – no \`-\`, which is always the subtraction operator
  - operator-based: \`+\`, \`≠\`, \`!=\`, \`√\`, \`∇\`
  - assignment with \`:\`: \`a: 23, b: a + 24\`
- Evaluation order – whitespace IS precedence
  - spaced operators go left-to-right, NO numeric precedence: \`1 + 2 * 3\` is \`(1 + 2) * 3\` = \`9\`
  - glued operators bind tighter than spaced ones: \`1 + 2*3\` is \`1 + (2 * 3)\` = \`7\`
    - \`a*b + c*d\` is \`(a * b) + (c * d)\` – hug what belongs together
  - operator glued ONLY to its left operand takes everything to its right:
    - \`a: 1 + 2\` is \`a: (1 + 2)\` – assignment without parentheses
    - chains right: \`a: b: 1 + 2\` is \`a: (b: (1 + 2))\`
  - parentheses always override: \`1 + (2 * 3)\` is \`7\`
  - within a tier, evaluation is left-to-right: \`2*3^2\` is \`(2 * 3) ^ 2\` = \`36\`
  - \`-\` is never part of a name: \`n-1\` is \`n - 1\` glued, so it binds tighter than a spaced operator
- Comments
  - single-line with \`;\`: \`a: 1, ; this is a comment\`
- Program structure
  - expressions separated by commas: \`a: 1, b: 2, a + b\`
  - last expression is the result (automatically displayed)

### Semantics
- Differentiable programming
  - gradient with \`∇\`: \`∇({ x | x^2 })(3)\` is \`6\`
  - higher-order: \`∇(∇({ x | x^3 }))(1)\` is \`6\`
- Reactive programming
  - make a cell: \`x: $(0.5)\` – its name is its value: \`x × 2\` is 1
  - write it: \`x← 0.7\` (also \`x:= 0.7\`) – in a step or a press, or as the program runs
  - follow it: \`y: x + 1\` – a definition that reads a cell is a live value that follows it
  - keep what it holds now: \`old: $x\` – a new cell, holding x's value; \`old: x\` is another name for x
  - a live value of a block: \`y: \${ x + 1 }\` – the whole block follows what it reads
  - follow a trainable variable: \`\${ θ × 2 }\` – a live value that reads θ follows every write
- Iteration
  - repeat N times with \`⟳\`: \`step ⟳ 100\` (async, fire-and-forget)
  - power with \`⍣\`: \`(double ⍣ 5)(1)\` is \`double(double(...))\` = \`32\`
    - a negative count runs a function backwards: \`({ x | x^3 + x } ⍣ -1)(10)\` is \`2\`
    - dyadic \`x (f ⍣ n) y\` iterates \`f(x, ·)\`: \`2 (^ ⍣ -1) 8\` is \`3\`
    - a predicate runs until it holds: \`(cos ⍣ ≡)(1)\` is the fixed point \`0.739…\`
- Inverse and Under – any smooth function runs backwards (Newton's method, slope from \`∇\`)
  - \`inverse(f)\` is \`f ⍣ -1\`; \`solve(g, y, seed)\` is the x near seed with g(x) = y
  - around with \`⍢\`: \`x (g ⍢ f) y\` is \`g⁻¹(f(g(x), g(y)))\` – go into g's world, do f, come back; \`f under g\` is the same in APL's order
  - \`3 (square ⍢ +) 4\` is \`5\`, \`(log ⍢ μ)(x)\` is the geometric mean, \`6 (reciprocal ⍢ +) 3\` is \`2\`
  - when g picks out a part, f edits just that part: \`({ x | x_[1, 2, 3] } ⍢ sort)(v)\` sorts only the middle
- Choosing – a candidate answers or declines
  - \`guard(cond, { value })\`: a candidate that answers while cond holds, declines (\`NoMatch\`) otherwise
  - \`cond(a, b, …)\` and \`cascade((a, b, …))(args)\` take the first candidate that answers
  - an Error is an answer, not a decline: it stops the choice and surfaces
  - \`try({ value })\`: a candidate that declines when value fails – \`cond(try({ risky() }), 0)\`
  - \`Error(message, cause?)\` makes a failure; \`NoMatch\` is the decline itself
  - example: \`fact: { n | cond(guard(n = 0, { 1 }), { n * fact(n - 1) }) }\`
- Optimization
  - create variable: \`θ: ~([0, 0])\`
  - define loss: \`loss: { sum(θ^2) }\`
  - create optimizer: \`opt: adam(0.01)\`
  - minimize: \`{ opt(loss) } ⟳ 100\`
- Built-in functions
  - tensor math: \`+\`, \`-\`, \`*\`, \`/\`, \`^\`, \`√\`, \`sum\`, \`mean\`, \`max\`, \`min\`, \`sin\`, \`cos\`, \`log\`, \`exp\`, \`dot\`, \`matmul\`, \`transpose\`, \`reshape\`, \`clamp\`, \`sigmoid\`, \`relu\`, \`softmax\`, \`oneHot\`, \`crossEntropy\`, etc.
  - tensor creation: \`..<\`/\`...\` (range), \`linspace\`, \`eye\`, \`rand\`, \`randn\`
  - axis variants: \`stack((a, b), axis)\`, \`concat((a, b), axis)\`, \`unstack(x, axis)\`, \`sum(x, axis)\`
  - outer product \`⊗\`: \`a (⊗ f) b\` pairs every cell of a with every cell of b; \`a (f ⊗ k) b\` keeps trailing k axes zipped
  - inner product \`∙\`: \`a (g ∙ f) b\` combines with g, then reduces with f – \`A (× ∙ +) B\` is matrix multiply, \`W (+ ∙ ⌊) W\` is shortest paths
  - items – a tensor's rows, a list's elements, a string's characters: \`#\`, \`_\`, \`take\`, \`drop\`, \`reverse\`, \`concat\` (\`++\`), \`reduce\`, \`scan\` and \`fold(x, f, start)\` work on all three; each kind's own is named for it (\`TensorTake\`, \`ListTake\`, \`StringTake\`); \`ListMap\` maps a list
  - each, reduce and scan as glyphs, data first: \`x ¨ f\`, \`x ⌿ f\`, \`x ⍀ f\`; given only f, each is the function that does it – \`⌿(⌊)\` is the minimum. All three also type as ASCII, as in K: \`x ' f\`, \`x / f\`, \`x \\ f\` – \`/\` divides by anything but a function
  - text: \`StringSplit(s, separator)\` cuts it into a list, \`StringFind(s, part)\` is where part starts (the length when it isn't there), \`StringConcat\` joins, \`format\` (\`⍕\`) writes a value as source and \`execute\` (\`⍎\`) runs it, \`StringToCodes\` and \`CodesToString\` go to numbers and back
  - the playground's examples: \`Examples\`, a list of (name, source) pairs
  - UI: \`Slider\`, \`Scrubber\`, \`Checkbox\`, \`Button\`, \`Text\`, \`TextEditor\`, \`Code\`, \`CodeEditor\`, \`CodePrint\`, \`Print\`, \`PrettyPrint\`, \`Grid\`, \`Layers\`, \`Center\`, \`Point2D\`, \`Pointer\`, \`Trail\`, \`PointPlot\`, \`ImageUpload\`
  - live inputs: \`Camera\`, \`Microphone\`, \`MicrophoneSpectrum\`, \`Time\`, \`MousePosition\`, \`Density\`
  - optimizers: \`sgd\` (optional momentum), \`adam\`, \`adamw\`, \`adagrad\`, \`rmsprop\`, \`lion\`, \`muon\` and more – each built on \`optimizer(init, rule)\`
  - metadata: \`doc(fn, signature, description, example)\` attaches the card hover shows
- Ad-hoc operators
  - define custom operators: \`(+-): { a, b | (a - b, a + b) }, 10 +- 3\`

### IDE
- Syntax highlighting
- Live evaluation with error reporting
- Automatic visualization of values (tensors, lists, functions)
- GPU-accelerated tensor operations (Fluent's own engine on WebGPU, with a CPU fallback)
- Command palette (Ctrl+P or Ctrl+Shift+P)
- Auto-completion (Ctrl+Space)
- Shareable URL links (Ctrl+S)
- examples gallery (Ctrl+O; in Safari use Ctrl+Shift+O — plain ⌘O is reserved by the browser)
`
