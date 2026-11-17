# The writer and the reader of lines, as a pair, on the quarter of the dictated lines that
# neither has seen.
#   python3 ink/pair.py write [steps=6000] [quarter ...] [seed=0] [wide=64] [deep=3] [curve=1000] [reader=line240] [name=linewriter]
# A writer learns the hand – the cells, and the glyphs of the lines it may see – and names
# what the reader of lines cut out of the other lines: ink is the glyph under whose name its
# moves are likeliest. Its names are tried alone, added to the reader's, and among the five
# names the reader finds likeliest.
#   python3 ink/pair.py teach [steps=3000] [quarter ...] [seed=0] [wide=128] [times=10] [real=1] [curve=0] [writer=linewriter] [pen=64] [deep=3] [name=taught]
# A reader of lines learns from what the writer writes: of every glyph of the hand, `times`
# written ones of its name, with the hand's own beside them or, with real=0, without.
import os, sys
import numpy as np
import mlx.core as mx
import mlx.nn as nn
from mlx.utils import tree_flatten
from ink import KEPT, Reader, descend, said
from line import FIRST, LATER, LONG, SPACED, LineReader, edits, even, given, hand, inked, lessons, loss, quarter, tried
from write import CELL, LINE, Writer, likely, moves, seen, strokes, taught, write

# what the reader makes of a line's strokes: of each glyph it cuts out, whether a space is
# before it, how likely the reader finds each name, and the ink
def pieces(model, line):
    heard, played = model(*given([np.stack([even(stroke) for stroke in line[:LONG]])]))
    heard, role = np.array(nn.log_softmax(heard[0], -1)), np.array(played[0]).argmax(1)
    role[0] = FIRST
    starts = list(np.flatnonzero(role != LATER))
    return [(role[a] == SPACED, heard[a:b].sum(0), inked(line[a:b])) for a, b in zip(starts, starts[1:] + [len(role)])]

# of the glyphs named `guess`, cut out of lines of `sizes` glyphs each, how many are wrong
# against the lines' `texts`
def wrong(guess, sizes, texts, glyphs):
    at, off = 0, 0
    for size, text in zip(sizes, texts):
        off += sum(map(len, edits(text.replace(" ", ""), "".join(glyphs[g] for g in guess[at:at + size])))); at += size
    return off

if __name__ == "__main__":
    mode = sys.argv[1]
    named = dict(word.split("=") for word in sys.argv[2:] if "=" in word)
    rest = [int(word) for word in sys.argv[2:] if "=" not in word]
    steps, quarters = rest[0] if rest else {"write": 6000, "teach": 3000}[mode], rest[1:] or range(4)
    seed, curve = int(named.get("seed", 0)), int(named.get("curve", 1000 if mode == "write" else 0))
    glyphs, lines, cells, ink, count, label, whole, gaps = hand()
    for k in quarters:
        mx.random.seed(seed); rng = np.random.default_rng(k + 4 * seed)
        _, mine, mine_gaps, often, held = quarter(k, lines, cells, whole, gaps)
        where = (mine >= cells).astype(np.int32)
        texts = [lines[l][0] for l in held]
        if mode == "write":
            wide, deep = int(named.get("wide", 64)), int(named.get("deep", 3))
            reader = LineReader(len(glyphs)); reader.load_weights(os.path.join(KEPT, f"{named.get('reader', 'line240')}-{k}.safetensors")); reader.eval()
            cut = [pieces(reader, lines[l][1]) for l in held]
            sizes = [len(line) for line in cut]
            heard = np.stack([names for line in cut for _, names, _ in line])
            pen, n = map(np.array, zip(*[one for line in cut for _, _, one in line]))
            moved, long = moves(pen, n)
            # a name the writer has not been shown is no name to it
            known = np.where(np.isin(np.arange(len(glyphs)), label[mine]), 0, -np.inf)
            total = sum(len(text.replace(" ", "")) for text in texts)
            def tell(model, step):
                found = likely(model, moved, long, len(glyphs), LINE) + known
                five = np.full(heard.shape, -np.inf); np.put_along_axis(five, np.argsort(-heard, 1)[:, :5], 0, 1)
                told = [f"{name} {100 * wrong(guess.argmax(1), sizes, texts, glyphs) / total:.1f} %" for name, guess in [("the reader", heard), ("the writer", found), ("both", heard + found), ("the writer, of the reader's five", found + five)]]
                print(f"  step {step}: glyphs wrong of {total} in {len(held)} lines – " + ", ".join(told), flush=True)
            model = taught(ink[mine], count[mine], label[mine], where, len(glyphs), steps, rng, wide, deep, True, tell if curve else None, curve)
            print(f"a writer {wide} wide, {deep} deep: {sum(v.size for _, v in tree_flatten(model.parameters()))} weights", flush=True)
            model.save_weights(os.path.join(KEPT, f"{named.get('name', 'linewriter')}-{k}.safetensors"))
            print(f"quarter {k}:", end="", flush=True); tell(model, steps)
        if mode == "teach":
            wide, times, real = int(named.get("wide", 128)), int(named.get("times", 10)), named.get("real", "1") == "1"
            writer = Writer(len(glyphs), d=int(named.get("pen", 64)), layers=int(named.get("deep", 3))); writer.load_weights(os.path.join(KEPT, f"{named.get('writer', 'linewriter')}-{k}.safetensors")); writer.eval()
            asked = np.tile(np.arange(len(mine)), times)
            drawn = []
            for i in range(0, len(asked), 512):
                for place in (CELL, LINE):
                    some = asked[i:i + 512][where[asked[i:i + 512]] == place]
                    if len(some): drawn += [(pick, strokes(row)) for pick, row in zip(some, write(writer, label[mine][some], place))]
            drawn = [(pick, one) for pick, one in drawn if one]
            judge = Reader(len(glyphs)); judge.load_weights(os.path.join(KEPT, "reader.safetensors")); judge.eval()
            right = said(judge, *seen([one for _, one in drawn]), np.ones(len(drawn), np.float32)).argmax(1) == label[mine][[pick for pick, _ in drawn]]
            print(f"{len(drawn)} glyphs written of {len(asked)} asked for; the reader of cells takes {100 * right.mean():.1f} % for their name", flush=True)
            # the hand to learn from: the cells' glyphs, then the lines' – the written ones, and the hand's own
            taken = [(place, *inked(one), label[mine][pick]) for pick, one in drawn for place in [where[pick]]]
            if real: taken += [(where[i], ink[m], count[m], label[m]) for i, m in enumerate(mine)]
            taken.sort(key=lambda one: one[0])
            place, pen, n, name = (np.array(column) for column in zip(*taken))
            model = LineReader(len(glyphs), d=wide)
            def watch(step): print(f"  step {step}: {tried(model, lines, held, glyphs)}", flush=True)
            descend(model, loss, lessons(pen, n, name, int((place == CELL).sum()), [], mine_gaps, often, rng), steps, 1e-3, say=500, watch=watch if curve else None, every=curve)
            model.save_weights(os.path.join(KEPT, f"{named.get('name', 'taught')}-{k}.safetensors"))
            print(f"quarter {k}, a reader {wide} wide of {sum(v.size for _, v in tree_flatten(model.parameters()))} weights, taught by {'the hand and ' if real else ''}{times} written a glyph: {tried(model, lines, held, glyphs)}", flush=True)
