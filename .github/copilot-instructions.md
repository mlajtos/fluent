# Copilot Instructions for Fluent

## Project Overview

Fluent is a tiny language + IDE for differentiable tensors and reactive UIs. It combines math, automatic differentiation, and interactivity in a playful way for programmers, researchers, and tinkerers.

## Tech Stack

- **Runtime**: Bun (JavaScript runtime)
- **Language**: TypeScript with strict mode enabled
- **Frontend**: React (via transitive dependencies) with Preact Signals for reactivity
- **Editor**: Monaco Editor (from VS Code)
- **Grammar**: Ohm.js for parsing the Fluent language
- **ML/Tensors**: TensorFlow.js (with CPU, WASM, and WebGL backends)
- **Styling**: Tailwind CSS v4.x
- **Build**: Bun's built-in bundler with Tailwind plugin

## Project Structure

```
.
├── server.ts        # Bun development server
├── client.tsx       # Main IDE implementation (3763 lines)
├── index.html       # HTML entry point
├── index.css        # Global styles
├── package.json     # Dependencies and scripts
├── tsconfig.json    # TypeScript configuration
└── bunfig.toml      # Bun configuration with Tailwind plugin
```

## Development Setup

1. **Install dependencies**: `bun install`
2. **Start dev server**: `bun dev`
3. **Access IDE**: Open `http://localhost:3000` in browser
4. **With example code**: Use `?code=<base64>` query parameter

## Code Style and Conventions

### TypeScript

- Use strict TypeScript settings (all strict options enabled)
- Target ESNext with module preservation
- JSX transform: `react-jsx`
- No unused locals/parameters checking is disabled for flexibility
- Index signatures allow property access without checking

### Language Design (Fluent)

The Fluent language has unique characteristics:

- **Left-to-right evaluation**: NO operator precedence (`1 + 2 * 3` = 9, not 7)
- **Assignment operator** `:` requires parentheses for complex right-hand sides
  - Correct: `b: (a + 1)`, `d: (fn(a) + fn(b))`
  - Incorrect: `b: a + 1` (parses as `(b: a) + 1`)
- **Symbols**: Can use Unicode (α, β, θ, ∇, √, etc.)
- **Comments**: Single-line with `;`
- **Functions**: Lambda syntax with `{ params | body }` or `{ body }`
- **Tensors**: Multi-dimensional arrays with broadcasting
- **Reactive**: Signals created with `$(value)`, accessed with `x()`
- **Differentiable**: Gradient operator `∇` for automatic differentiation

### Code Organization

- **client.tsx** contains the entire IDE implementation:
  - Documentation (lines 16-114)
  - Grammar definition (Ohm.js)
  - Parser and evaluator
  - Built-in functions and operators
  - UI components (Slider, Button, Text, Grid)
  - Monaco Editor integration
  - TensorFlow.js tensor operations

- **server.ts** is minimal: just serves the app with Bun

## Key Features to Preserve

1. **Live evaluation**: Code evaluates as you type
2. **Automatic visualization**: Values display automatically
3. **GPU acceleration**: Tensor ops use TensorFlow.js backends
4. **Shareable URLs**: Code can be shared via base64-encoded URLs
5. **Auto-completion**: Monaco Editor provides suggestions
6. **Syntax highlighting**: Custom language definition

## Dependencies Management

- Use `bun install` (not npm/yarn)
- Lock file: `bun.lock` (binary format)
- Keep TensorFlow.js backends synchronized (all at same version)

## Testing and Building

- **No formal test suite**: This is an experimental/research project
- **Manual testing**: Run `bun dev` and test in browser
- **Type checking**: `bun tsc --noEmit` (TypeScript compiler check)

## Important Notes

- Single-file architecture: Most code is in `client.tsx` for simplicity
- The Fluent language itself is embedded in the client
- Grammar changes require updating Ohm.js definitions
- UI components are built with React and Preact Signals
- Tensor operations delegate to TensorFlow.js

## Common Tasks

- **Add built-in function**: Add to built-ins object in client.tsx
- **Modify syntax**: Update Ohm.js grammar definition
- **Add UI component**: Create React component with signal support
- **Change styling**: Use Tailwind utility classes (v4 syntax)

## Potential Issues

- Grid component has limitations with dynamically created lists
- JavaScript lambdas in built-ins are "magical" but useful
- No operator precedence can be surprising for new users
