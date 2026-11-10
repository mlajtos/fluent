# Copilot instructions for Fluent

Fluent is a small array language with autodiff and an IDE that runs in the browser. Tensors run on Fluent's own engine. There is no TensorFlow.js and no jax-js.

## Layout

- `language.ts`: grammar (Ohm, `CodeGrammar`), evaluator, the native built-ins (`DefaultEnvironment`), and loading of the prelude. No DOM, no React.
- `prelude.fl`: the standard library, written in Fluent. `createScope` in `language.ts` evaluates it into every program's scope.
- `engine/`: the tensor engine. `ir.ts` (one immutable graph from tensors to loop nests, every value f32), `rewrite.ts` (the rewrite engine), `opt.ts` (exact simplification laws, fusion, plan keys), `grad.ts` (∇ = transpose ∘ linearize), `gpu.ts` (WGSL printers and the WebGPU executor), `cpu.ts` (the CPU backend: each kernel printed as a JS function), `tensor.ts` (the Tensor type the language calls, and the choice of backend). einsum is Fluent, in `prelude.fl`.
- `worker.ts`: the evaluation worker. It starts the engine (WebGPU where the platform has it, the CPU everywhere else) and runs the host.
- `host.ts`: the evaluation host, on the worker side. It evaluates the program, binds the widgets and live sources into the environment, and describes values to the page.
- `protocol.ts`: the plain-data messages between the page and the worker, and the lexicon the grammar and the editor share.
- `client.tsx`: the IDE page (React, Monaco). It draws what the worker describes; the worker parses for it too. It imports neither the language nor the engine.
- `painter.ts`: paints pictures in the worker, on the GPU, into a canvas the page handed over.
- `tour.ts` (the Tour), `examples.ts` (the gallery), `documentation.ts` (the `Documentation` page).
- `server.ts`: the dev server, with a `/proxy` CORS relay.

## Adding to the language

- Prelude first: write a new built-in in Fluent in `prelude.fl`, with a `doc(…)` card.
- Only a true primitive goes native: define it in `language.ts`, add it to `DefaultEnvironment`, and give it its doc card in `prelude.fl`. Tensor work belongs in `engine/tensor.ts`.
- A glyph operator must be inside `OPERATOR_RANGES` in `protocol.ts`, or the grammar rejects it.

## Commands

- `bun install`, then `bun dev` (http://localhost:3000; `?code=<base64url>&v=<VERSION>` opens a program, and one without `v` is read as version 1 and migrated).
- `bun test ./tests.ts` (also `bun run test`): the language end to end, on the CPU backend.
- `bun run test:engine`: the engine gates in `engine/tests/` (`index.bun.ts`, `fold.bun.ts`, `core.ts`, `ops.ts`) on the CPU. `bun engine/tests/run.ts` runs the gates in headless Chrome on WebGPU.
- `bun run test:browser`: Playwright IDE tests (`tests.browser.ts`).
- `bun run typecheck`: `tsc --noEmit`.

Bun 1.3.14 or newer (`engines` in `package.json`). `bun.lock` is a text lockfile.

## Style

- Fluent code follows `Style.md`: layout is grammar (glued binds tighter than spaced, spaced runs left to right, no precedence).
- Comments describe the code as it is now. History goes in the commit message.
- No space padding to align code or comments.
