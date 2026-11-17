# The writer reads a line by itself. A line is glyphs one after another, and each has a name,
# a place after the ink before it, and its moves under that name. Every run of strokes is
# tried as one glyph under every name, and the line is cut where the whole is likeliest.
#   python3 ink/cuts.py [steps=6000] [quarter ...] [seed=0] [wide=64] [deep=3] [own=0] [also=<copybook>] [reader=line240] [name=cutter]
# A writer learns the cells and the glyphs of three quarters of the lines, and reads the
# quarter kept from it – and, with `also`, the lines of another copybook whose ink this one
# does not have. The reader of lines kept under `reader` reads the same lines beside it.
# The lines it learns are first cut into their text's glyphs by the reader of cells. With
# `own`, it then cuts them itself, that many times, and each time a new writer learns its cuts.
import json, math, os, sys, time
import numpy as np
import mlx.core as mx
from mlx.utils import tree_flatten
from ink import COPYBOOK, KEPT
from line import FIRST, GONE, KINDS, LONG, SAME, SPACED, LineReader, edits, hand, inked, parsed, quarter, tried
from write import LINE, MOVE, moves, taught, unlikely

# how far a run of strokes starts after all the ink before it
def gap(strokes, a, b): return min(s[:, 0].min() for s in strokes[a:b]) - max(s[:, 0].max() for s in strokes[:a])

# Where the hand puts a glyph: of lines cut into glyphs – their strokes, how many each glyph
# has and the kind of place before each – how likely, in nats, a gap of MOVE px is before a
# glyph, and whether a gap is a space. A kind's gaps are bells on the gaps seen.
def places(cut):
    left = {kind: [] for kind in KINDS}
    for strokes, counts, firsts in cut:
        ends = np.cumsum(counts)
        for k in range(1, len(counts)): left[firsts[k]].append(gap(strokes, ends[k - 1], ends[k]))
    often = {kind: len(left[kind]) / sum(map(len, left.values())) for kind in KINDS}
    def bells(kind):
        seen = np.array(left[kind], np.float64); width = 1.06 * seen.std() * len(seen) ** -0.2
        return lambda x: math.log(often[kind] * np.exp(-0.5 * ((x - seen) / width) ** 2).mean() / (width * math.sqrt(2 * math.pi)) * MOVE + 1e-12)
    at = {kind: bells(kind) for kind in KINDS}
    return (lambda x: np.logaddexp.reduce([at[kind](x) for kind in KINDS])), (lambda x: at[SPACED](x) > at[FIRST](x))

# every run of at most `most` strokes of each line, as moves
def runs(some, most):
    which = [(l, a, b) for l, strokes in enumerate(some) for a in range(min(len(strokes), LONG)) for b in range(a + 1, min(a + most, len(strokes), LONG) + 1)]
    pen, n = map(np.array, zip(*[inked(some[l][a:b]) for l, a, b in which]))
    return which, *moves(pen, n)

# how likely, in nats, the moves of run `run[i]` are under the name `name[i]`
def under(model, moved, long, run, name, at_once=256):
    out, order = np.zeros(len(run), np.float32), np.argsort(long[run])
    for i in range(0, len(order), at_once):
        pick = order[i:i + at_once]; rows = run[pick]
        out[pick] = -np.array(unlikely(model, mx.array(moved[rows, :long[rows].max()]), mx.array(long[rows]), mx.array(name[pick]), mx.full(len(pick), LINE, mx.int32)))
    return out

# What the writer reads in lines of strokes, with no text: of every way to cut a line into
# glyphs of the `names` it knows, the likeliest – each name as likely as another before the
# ink is seen – and a space where the gap before a glyph is likelier one than none.
def read(model, some, names, most, placed, spaced, glyphs):
    which, moved, long = runs(some, most)
    found = under(model, moved, long, np.repeat(np.arange(len(which)), len(names)), np.tile(names, len(which))).reshape(len(which), len(names))
    worth = {key: one.max() - math.log(len(names)) for key, one in zip(which, found)}
    who = {key: names[one.argmax()] for key, one in zip(which, found)}
    texts = []
    for l, strokes in enumerate(some):
        n = min(len(strokes), LONG)
        best, back = [0.0] + [-np.inf] * n, [0] * (n + 1)
        for b in range(1, n + 1):
            for a in range(max(0, b - most), b):
                value = best[a] + worth[l, a, b] + (placed(gap(strokes, a, b)) if a else 0)
                if value > best[b]: best[b], back[b] = value, a
        ends = [n]
        while ends[-1]: ends.append(back[ends[-1]])
        texts.append("".join((" " if a and spaced(gap(strokes, a, b)) else "") + glyphs[who[l, a, b]] for a, b in zip(ends[:0:-1], ends[-2::-1])))
    return texts

# How the writer cuts lines whose glyphs are known, `wanted` of each: how many strokes each
# glyph has in the likeliest cut, or nothing where no cut gives every glyph its strokes
def cut(model, some, wanted, most, placed):
    which, moved, long = runs(some, most)
    asked = [(i, name) for i, (l, _, _) in enumerate(which) for name in set(wanted[l])]
    found = under(model, moved, long, *map(np.array, zip(*asked)))
    worth = {(*which[i], name): value for (i, name), value in zip(asked, found)}
    counts = []
    for l, strokes in enumerate(some):
        n = min(len(strokes), LONG)
        best = np.full((len(wanted[l]) + 1, n + 1), -np.inf); best[0, 0] = 0
        back = np.zeros(best.shape, int)
        for g, name in enumerate(wanted[l], 1):
            for b in range(g, n + 1):
                for a in range(max(g - 1, b - most), b):
                    if not np.isfinite(best[g - 1, a]): continue
                    value = best[g - 1, a] + worth[l, a, b, name] + (placed(gap(strokes, a, b)) if a else 0)
                    if value > best[g, b]: best[g, b], back[g, b] = value, b - a
        if n < len(strokes) or not np.isfinite(best[-1, -1]): counts.append(None); continue
        one, b = [], n
        for g in range(len(wanted[l]), 0, -1): one.append(back[g, b]); b -= back[g, b]
        counts.append(np.array(one[::-1]))
    return counts

# the lines of another copybook whose ink this one does not have, as the pen writes them
def other(book):
    def inks(book):
        kept = [json.load(open(os.path.join(book, file))) for file in sorted(os.listdir(book)) if file.startswith("line-")]
        return {one["glyph"]: one["samples"][0]["strokes"] for one in kept if one["samples"] and one["samples"][0]}
    mine = inks(COPYBOOK)
    new = [("".join(SAME.get(c, c) for c in text), [np.array([p[:2] for p in stroke["points"]], np.float32) for stroke in strokes]) for text, strokes in inks(book).items() if text not in mine or mine[text][0]["at"] != strokes[0]["at"]]
    return [(text, strokes) for text, strokes in new if not set(GONE) & set(text)]

# how lines were read, in words
def told(some, texts):
    off = sum(sum(map(len, edits(text.replace(" ", ""), guess.replace(" ", "")))) for (text, _), guess in zip(some, texts))
    total = sum(len(text.replace(" ", "")) for text, _ in some)
    trees = [one["tree"] for one in parsed([text for text, _ in some] + texts)]
    same = sum(len(text.replace(" ", "")) == len(guess.replace(" ", "")) for (text, _), guess in zip(some, texts))
    return f"{off} of {total} glyphs wrong, {100 * off / total:.1f} %; {sum(a == b for a, b in zip(trees, trees[len(some):]))} of {len(some)} lines parse right, {same} are cut into as many glyphs as their text has"

if __name__ == "__main__":
    named = dict(word.split("=") for word in sys.argv[1:] if "=" in word)
    rest = [int(word) for word in sys.argv[1:] if "=" not in word]
    steps, quarters = rest[0] if rest else 6000, rest[1:] or range(4)
    seed, wide, deep, own = (int(named.get(key, default)) for key, default in [("seed", 0), ("wide", 64), ("deep", 3), ("own", 0)])
    name = named.get("name", "cutter")
    # runs of every length would each leave the GPU's memory of their size behind
    mx.set_cache_limit(256 << 20)
    glyphs, lines, cells, hands, strokes_of, label, whole, gaps = hand()
    index = {glyph: i for i, glyph in enumerate(glyphs)}
    for k in quarters:
        mx.random.seed(seed); rng = np.random.default_rng(k + 4 * seed)
        ink, count = hands.copy(), strokes_of.copy()
        seen, mine, _, _, held = quarter(k, lines, cells, whole, gaps)
        where, names = (mine >= cells).astype(np.int32), np.unique(label[mine])
        known = [l for l in seen if not set(GONE) & set(lines[l][0])]
        tests = [(f"quarter {k}", [lines[l] for l in held])] + ([("the other copybook", other(named["also"]))] if "also" in named else [])
        if "reader" in named:
            reader = LineReader(len(glyphs)); reader.load_weights(os.path.join(KEPT, f"{named['reader']}-{k}.safetensors")); reader.eval()
            for test, some in tests: print(f"{test}, the reader of lines: {tried(reader, some, range(len(some)), glyphs)}", flush=True)
        for turn in range(own + 1):
            most = int(count[mine].max())
            placed, spaced = places([(lines[l][1], count[whole[l][0]], whole[l][1]) for l in known])
            model = taught(ink[mine], count[mine], label[mine], where, len(glyphs), steps, rng, wide, deep)
            model.save_weights(os.path.join(KEPT, f"{name}{turn or ''}-{k}.safetensors"))
            whose = ["the reader of cells", "the first writer"][turn] if turn < 2 else "the writer before it"
            for test, some in tests:
                began = time.time()
                texts = read(model, [strokes for _, strokes in some], names, most, placed, spaced, glyphs)
                print(f"{test}, a writer of {sum(v.size for _, v in tree_flatten(model.parameters()))} weights taught the cuts of {whose}: {told(some, texts)}; {(time.time() - began) / len(some):.1f} s a line", flush=True)
                for (text, _), guess in zip(some, texts): print(f"  {text}   ⟶   {guess}")
            if turn == own: break
            found = cut(model, [lines[l][1] for l in known], [[index[c] for c in lines[l][0] if c != " "] for l in known], most, placed)
            moved = glyphs_moved = 0
            for l, counts in zip(known, found):
                if counts is None or (counts == count[whole[l][0]]).all(): continue
                moved += 1; glyphs_moved += int((np.cumsum(counts) != np.cumsum(count[whole[l][0]])).sum())
                for i, a, n in zip(whole[l][0], np.cumsum(counts) - counts, counts): ink[i], count[i] = inked(lines[l][1][a:a + n])
            print(f"the writer cuts {moved} of {len(known)} lines another way than they were cut, {glyphs_moved} glyphs end at another stroke; {sum(c is None for c in found)} it cannot cut", flush=True)
