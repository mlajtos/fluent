import { serve } from "bun";
import index from "./index.html";
import { families, CANDIDATES } from "./dictation.ts";
import { mkdirSync, appendFileSync, existsSync, readFileSync, writeFileSync } from "node:fs";
import { createServer, connect } from "node:net";

const javascript = (body: BodyInit) => new Response(body, { headers: { "Content-Type": "application/javascript" } });

// Monaco's editor worker, bundled once
const { outputs: [monacoWorker] } = await Bun.build({
  entrypoints: [Bun.fileURLToPath(import.meta.resolve("monaco-editor/esm/vs/editor/editor.worker.js"))],
});
if (!monacoWorker) throw new Error("Failed to bundle Monaco worker");

// The evaluation worker, bundled on every request: a reload runs the current evaluator
const evaluationWorker = async () => {
  const { success, logs, outputs: [bundle] } = await Bun.build({ entrypoints: ["./worker.ts"], target: "browser", format: "esm", sourcemap: "inline" });
  return javascript(success && bundle ? bundle : `throw new Error(${JSON.stringify(logs.map(String).join("\n"))})`);
};

mkdirSync("paper/copybook/candidates", { recursive: true });
// a glyph's file is named by its code point, a line's by a hash of its text. A candidate's
// file is in a folder of its own: the readers learn the files of paper/copybook itself.
const samplesOf = (text: string) => `paper/copybook/${CANDIDATES.includes(text) ? "candidates/" : ""}${[...text].length === 1 ? text.codePointAt(0)!.toString(16) : `line-${Bun.hash(text).toString(16)}`}.json`;

// a page the browser keeps here as one JSON file
const page = (file: string) => ({
  GET: async () => new Response(await Bun.file(file).exists() ? Bun.file(file) : "{}"),
  PUT: async (req: Request) => {
    await Bun.write(file, await req.text());
    return new Response("kept");
  },
});

// The readers of lines (ink/serve.py): a process that takes a written line's strokes on a
// line of its stdin and answers on a line of its stdout, in the order asked. It starts when
// a line is first read, and again when it is gone – readers that learned since take over
// when the process is killed.
type Reading = { text: string, of: number[], sure: number[], to: ([number, number][] | null)[] };
type Readers = { process: Bun.Subprocess<"pipe", "pipe", "inherit">, waiting: { said: (reading: Reading) => void, failed: (why: Error) => void }[] };
let readers: Readers | null = null;
const reading = (strokes: unknown): Promise<Reading> => {
  if (!readers) {
    const mine: Readers = readers = { process: Bun.spawn(["python3", "ink/serve.py"], { stdin: "pipe", stdout: "pipe", stderr: "inherit" }), waiting: [] };
    (async () => {
      let rest = "";
      for await (const chunk of mine.process.stdout.pipeThrough(new TextDecoderStream())) {
        const said = (rest + chunk).split("\n");
        rest = said.pop()!;
        for (const line of said) mine.waiting.shift()?.said(JSON.parse(line));
      }
      if (readers === mine) readers = null;
      for (const { failed } of mine.waiting.splice(0)) failed(new Error("the readers are gone"));
    })();
  }
  const { process, waiting } = readers;
  return new Promise((said, failed) => {
    waiting.push({ said, failed });
    process.stdin.write(JSON.stringify(strokes) + "\n");
    process.stdin.flush();
  });
};

// The socket is loopback-only unless $HOST opens it – HOST=0.0.0.0 lets a tablet on the
// same network write on the paper
const HOST = process.env.HOST ?? "127.0.0.1";

const server = serve({
  routes: {
    "/": index,
    "/monaco.worker.js": javascript(monacoWorker),
    "/fluent.worker.js": evaluationWorker,
    // dataset for the MNIST example (build.ts copies it into dist/ for prod)
    "/mnist.safetensors": () => new Response(Bun.file("./mnist.safetensors")),
    // names for the makemore example: makemore's names.txt, 32,033 names (build.ts copies it too)
    "/names.txt": () => new Response(Bun.file("./names.txt")),
    // symbol font for the editor and AST viz (build.ts copies it for prod)
    "/BQN386.ttf": () => new Response(Bun.file("./BQN386.ttf")),
    // CORS proxy for LoadTensorFromImageUrl / LoadSafeTensorFromURL. It
    // fetches whatever it is handed, so it is confined to what it is for:
    // http(s) only, or `file:` and `data:` would make it an arbitrary local
    // file reader, and it answers a loopback-only socket only so it is not a
    // relay for the rest of the network.
    "/proxy": async (req: Request) => {
      if (HOST !== "127.0.0.1") return new Response("the proxy answers a loopback-only server", { status: 403 });
      const target = new URL(req.url).searchParams.get("url");
      if (!target) return new Response("missing ?url=", { status: 400 });
      let scheme: string
      try { scheme = new URL(target).protocol } catch { return new Response("bad ?url=", { status: 400 }) }
      if (scheme !== "http:" && scheme !== "https:") {
        return new Response(`refusing to proxy ${scheme}`, { status: 403 });
      }
      const upstream = await fetch(target);
      return new Response(upstream.body, {
        status: upstream.status,
        headers: { "Content-Type": upstream.headers.get("Content-Type") ?? "application/octet-stream" },
      });
    },
    // what the pen did on a page and what the page made of it, a line an event
    "/heard": async (req: Request) => {
      appendFileSync("paper/pen.jsonl", (await req.json() as unknown[]).map((event) => JSON.stringify(event) + "\n").join(""));
      return new Response("heard");
    },
    // The copybook (copybook.tsx): the families of texts to write – the alphabet's glyphs,
    // and lines of Fluent – and the samples written of each, a file a text in
    // paper/copybook. A sample is put in its cell alone, and the one it takes the place of
    // goes to paper/copybook/taken.jsonl.
    "/copybook": {
      GET: () => Response.json({
        families,
        samples: Object.fromEntries(families.flatMap(({ glyphs }) => glyphs).filter((glyph) => existsSync(samplesOf(glyph)))
          .map((glyph) => [glyph, JSON.parse(readFileSync(samplesOf(glyph), "utf8")).samples])),
      }),
      PUT: async (req: Request) => {
        const { glyph, cell, sample } = await req.json() as { glyph: string, cell: number, sample: unknown };
        const family = families.find(({ glyphs }) => glyphs.includes(glyph));
        if (!family || !Number.isInteger(cell) || cell < 0 || cell >= family.cells) {
          return new Response("the copybook has no such cell", { status: 400 });
        }
        const file = samplesOf(glyph), samples: unknown[] = existsSync(file) ? JSON.parse(readFileSync(file, "utf8")).samples : [];
        const taken = samples[cell] ?? null, strokes = (of: unknown) => (of as { strokes: unknown[] } | null)?.strokes ?? [];
        // a sample that grows by a stroke takes the place of none
        const grown = JSON.stringify(strokes(sample).slice(0, strokes(taken).length)) === JSON.stringify(strokes(taken));
        if (taken && !grown) appendFileSync("paper/copybook/taken.jsonl", JSON.stringify({ at: Date.now(), glyph, cell, sample: taken }) + "\n");
        samples[cell] = sample;
        writeFileSync(file, JSON.stringify({ glyph, samples: Array.from(samples, (s) => s ?? null) }));
        return new Response("kept");
      },
    },
    // the readers of the ruled paper and of the canvas (ink.tsx): a written line's strokes in,
    // each its points, and out its text, of each stroke which glyph of the text it is of,
    // how sure the readers are of each glyph, and where each stroke goes in the hand's form
    "/reading": async (req: Request) => {
      if (req.method !== "POST") return new Response("POST a line's strokes", { status: 405 });
      try {
        return Response.json(await reading(await req.json()));
      } catch (e) {
        return new Response(String((e as Error).message ?? e), { status: 502 });
      }
    },
    // the ruled paper's page: the ink of its lines
    "/ruled": page("paper/ruled.json"),
    // the canvas (canvas.tsx): its lines – where each is, how large, and its ink – and its camera
    "/canvas": page("paper/canvas.json"),
    // the copybook's lines as the pen wrote them and in the hand's form (hand.tsx), as
    // ink/hand.py left them
    "/hand": async () => await Bun.file("paper/ink/hand.json").exists()
      ? new Response(Bun.file("paper/ink/hand.json"))
      : new Response("there is no hand yet: python3 ink/hand.py writes it", { status: 404 }),
  },
  hostname: HOST,
  // Off this machine a browser gives a page its GPU and its clipboard over https only:
  // $TLS_CERT and $TLS_KEY name a certificate's files
  tls: process.env.TLS_CERT ? { cert: Bun.file(process.env.TLS_CERT), key: Bun.file(process.env.TLS_KEY!) } : undefined,
  // no hot reload: with its socket open over TLS, Bun 1.4.3 spins a core for as long as a page is open
  development: { hmr: false },
});

// One address for a tablet, whatever scheme its browser picks: $FRONT_PORT takes both. A
// connection that opens with a TLS hello (its first byte is 0x16) is passed to the server
// as it is; any other is http, and is sent to the same address in https.
if (process.env.TLS_CERT && process.env.FRONT_PORT) {
  createServer((socket) => {
    socket.on("error", () => { });
    socket.once("data", (first) => {
      if (first[0] === 0x16) {
        const inner = connect(server.port!, "127.0.0.1");
        inner.on("error", () => socket.destroy());
        socket.on("close", () => inner.destroy());
        inner.write(first);
        socket.pipe(inner).pipe(socket);
        return;
      }
      const request = first.toString("latin1");
      const path = /^\S+ (\S+)/.exec(request)?.[1] ?? "/", host = /\r\nhost: *([^\r\n]+)/i.exec(request)?.[1];
      socket.end(host
        ? `HTTP/1.1 307 Temporary Redirect\r\nLocation: https://${host}${path}\r\nContent-Length: 0\r\nConnection: close\r\n\r\n`
        : "HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n");
    });
  }).listen(Number(process.env.FRONT_PORT), HOST);
}

console.log(`Listening on ${server.url}`);
