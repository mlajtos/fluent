# A reader of a line's ink: which strokes are one glyph, which glyph that is, and where the
# spaces are. It sees every stroke of the line and no text. The lines it learns on are the
# copybook's glyphs in random order, so what it knows of a glyph's neighbours is their ink.
# A space is one where the dictated text has one.
#   python3 ink/line.py [random] [steps=3000] [quarter ...] [seed=0] [wide=128] [curve=0] [name=line]
# `curve` is every how many steps a reader is tried, as it learns, on the lines kept from it.
# A reader of quarter k is kept as paper/ink/<name>-k.safetensors.
import json, math, os, subprocess, sys, zlib
import numpy as np
import mlx.core as mx
import mlx.nn as nn
from ink import FLOOR, HERE, KEPT, STROKES, Block, Reader, cut, descend, dictated, vary, written

# a stroke is M points, even steps along its ink; a line has at most LONG strokes
M = 32
LONG = 64
# what a stroke is to its glyph: one of its later strokes, its first, or its first after a space
LATER, FIRST, SPACED = 0, 1, 2
# what the pen writes no more: × is `*` to it, and the double-struck letters go by their
# names. Their ink is no lesson, and a line with one of them is not read
GONE = "×𝕀𝕍𝕐"
# what the pen writes as another glyph: it has one rho, the sign
SAME = {"ρ": "⍴"}
# a stroke's x is given as turns of these lengths, in units of 32 px
TURNS = 0.5 * 2.0 ** np.arange(9)

def even(stroke):
    along = np.concatenate([[0], np.cumsum(np.hypot(*np.diff(stroke, axis=0).T))])
    at = np.linspace(0, along[-1], M)
    return np.stack([np.interp(at, along, stroke[:, 0]), np.interp(at, along, stroke[:, 1])], 1).astype(np.float32)

# a glyph's strokes as one array of STROKES strokes: past its last stroke, that stroke's end
def inked(strokes):
    ink = np.stack([even(stroke) for stroke in strokes[:STROKES]])
    return np.concatenate([ink, np.broadcast_to(ink[-1, -1], (STROKES - len(ink), M, 2))]), len(ink)

# What the reader is given of lines, each [strokes, M, 2]. A point: where it is in its
# stroke's box, and the step to it. A stroke: its box's size and place over the baseline,
# how far along the line it is, and how it follows the ink before it – the pen's jump from
# the stroke before, and the gap its box leaves after that stroke's box and after all the
# earlier ink. `index` says which stroke is where in which line; a line shorter than the
# longest has the number past the last stroke there. With `fit`, a stroke's points are the
# fewer that `fit` makes of its M.
def given(lines, fit=None):
    P = np.concatenate(lines)
    low, high = P.min(1), P.max(1)
    middle, size = (low + high) / 2, np.maximum((high - low).max(1), FLOOR)
    shape = (P - middle[:, None]) / size[:, None, None]
    if fit is not None: shape = np.einsum("tm,nmc->ntc", fit, shape)
    step = np.zeros_like(shape); step[:, 1:] = shape[:, 1:] - shape[:, :-1]
    whole = np.stack([np.log(size / 32), middle[:, 1] / 32], 1)
    turn = 2 * math.pi * middle[:, :1] / 32 / TURNS
    after, index, at = np.zeros((len(P), 4)), np.full((len(lines), max(map(len, lines))), len(P)), 0
    for l, line in enumerate(lines):
        mine = slice(at + 1, at + len(line))
        after[mine, :2] = line[1:, 0] - line[:-1, -1]
        after[mine, 2] = low[mine, 0] - high[at:at + len(line) - 1, 0]
        after[mine, 3] = low[mine, 0] - np.maximum.accumulate(high[at:at + len(line) - 1, 0])
        index[l, :len(line)] = np.arange(at, at + len(line)); at += len(line)
    where = np.concatenate([whole, (high - low) / 32, after / 16, np.sin(turn), np.cos(turn)], 1)
    return [mx.array(np.concatenate([shape * 2, step * shape.shape[1] / 4], 2).astype(np.float32)), mx.array(whole.astype(np.float32)), mx.array(where.astype(np.float32)), mx.array(index)]

# Two transformers: one over a stroke's points, whose first token comes to the stroke's
# vector, and one over the line's strokes in the order they were written. Of each stroke
# the second says which glyph it is of, and what it is to that glyph.
class LineReader(nn.Module):
    def __init__(self, classes, d=128, heads=4, drop=0.1):
        super().__init__()
        self.point, self.whole, self.where = nn.Linear(4, d), nn.Linear(2, d), nn.Linear(8 + 2 * len(TURNS), d)
        self.place, self.order = mx.random.normal((M + 1, d)) * 0.02, mx.random.normal((LONG, d)) * 0.02
        self.stroke, self.line = [Block(d, heads, drop) for _ in range(2)], [Block(d, heads, drop) for _ in range(4)]
        self.norm, self.glyph, self.role = nn.LayerNorm(d), nn.Linear(d, classes), nn.Linear(d, 3)
    def __call__(self, points, whole, where, index):
        x = mx.concatenate([self.whole(whole)[:, None], self.point(points)], 1) + self.place
        for block in self.stroke: x = block(x)
        x = mx.concatenate([x[:, 0] + self.where(where), mx.zeros((1, x.shape[-1]))])[index] + self.order[:index.shape[1]]
        none = (index == points.shape[0])[:, None, None, :] * -1e9
        for block in self.line: x = block(x, none)
        x = self.norm(x)
        return self.glyph(x), self.role(x)

def loss(model, points, whole, where, index, glyph, role):
    said, played = model(points, whole, where, index)
    real = index < points.shape[0]
    each = nn.losses.cross_entropy(said, glyph, label_smoothing=0.1, reduction="none") + nn.losses.cross_entropy(played, role, reduction="none")
    return (each * real).sum() / real.sum()

# Lines to learn on, from a hand: its glyphs' ink, how many strokes each has and which
# glyph each is. A line is glyphs of the hand, what the first stroke of each is, and the
# gap before each – from the glyph before it, right edge to left edge – or no gaps, for
# glyphs that stay where the pen put them. Every glyph is varied, and then the line.
EACH = np.repeat(np.arange(STROKES), M)
def composed(ink, count, label, lines, rng):
    picks = np.concatenate([chosen for chosen, _, _ in lines])
    varied = vary(ink[picks].reshape(len(picks), -1, 2), np.broadcast_to(EACH, (len(picks), len(EACH))), np.ones(len(picks), np.float32), rng).reshape(len(picks), STROKES, M, 2)
    made, glyph, role, at = [], [], [], 0
    for chosen, firsts, gaps in lines:
        strokes, right = [], 0.0
        for j, pick in enumerate(chosen):
            one = varied[at, :count[pick]]; at += 1
            if gaps is not None: one = one + np.float32([right + gaps[j] - one[..., 0].min(), 0])
            right = one[..., 0].max()
            strokes.append(one)
        line = np.concatenate(strokes) * math.exp(rng.normal(0, 0.08))
        line[..., 1] += rng.normal(0, 1.5) + rng.normal(0, 0.01) * line[..., 0]
        if gaps is None: line[..., 0] += rng.normal(0, 5)
        made.append(line)
        glyph.append(np.repeat(label[chosen], count[chosen]))
        role.append(np.concatenate([[first] + [LATER] * (count[pick] - 1) for pick, first in zip(chosen, firsts)]))
    longest = max(map(len, made))
    pad = lambda rows: mx.array(np.stack([np.concatenate([row, np.zeros(longest - len(row), row.dtype)]) for row in rows]).astype(np.int32))
    return given(made) + [pad(glyph), pad(role)]

# A lesson is `lines` lines: one in four a line the hand wrote, when `real` has any, and
# the others glyphs drawn at random – half of them from the hand's first `cells` glyphs, the
# copybook's cells, and half from the rest, which were written in lines. Before each glyph
# is a place: `often` is how often a place is FIRST or SPACED, and `gaps` are the gaps the
# hand left at places of each kind.
KINDS = [FIRST, SPACED]
def lessons(ink, count, label, cells, real, gaps, often, rng, lines=32):
    def some():
        lesson = []
        for _ in range(lines):
            if real and rng.random() < 0.25: lesson.append(real[rng.integers(len(real))]); continue
            n = rng.integers(3, 21)
            chosen = np.where((rng.random(n) < 0.5) | (cells == len(ink)), rng.integers(0, cells, n), rng.integers(cells, max(len(ink), cells + 1), n))
            chosen = chosen[np.cumsum(count[chosen]) <= LONG]
            firsts = rng.choice(KINDS, len(chosen), p=often); firsts[0] = FIRST
            wide = np.select([firsts == kind for kind in KINDS], [rng.choice(gaps[kind], len(chosen)) for kind in KINDS]); wide[0] = rng.uniform(0, 40)
            lesson.append((chosen, firsts, wide))
        return composed(ink, count, label, lesson, rng)
    return some

# What a reader makes of a line's strokes: the text, and at which stroke each glyph starts.
# Readers in a list read as one: what each finds likely, multiplied.
def read(model, strokes, glyphs):
    line, said, played = given([np.stack([even(stroke) for stroke in strokes[:LONG]])]), 0, 0
    for one in model if isinstance(model, list) else [model]:
        glyph, role = one(*line)
        said, played = said + nn.log_softmax(glyph[0], -1), played + nn.log_softmax(role[0], -1)
    said, role = np.array(said), np.array(played).argmax(1)
    role[0] = FIRST
    starts = list(np.flatnonzero(role != LATER))
    text = "".join((" " if role[a] == SPACED else "") + glyphs[said[a:b].sum(0).argmax()] for a, b in zip(starts, starts[1:] + [len(role)]))
    return text, starts

# the fewest edits from `guess` to `truth`: what was taken for what, what was missed, what was made up
def edits(truth, guess):
    far = np.zeros((len(truth) + 1, len(guess) + 1), int)
    far[:, 0], far[0] = np.arange(len(truth) + 1), np.arange(len(guess) + 1)
    for i in range(1, len(truth) + 1):
        for j in range(1, len(guess) + 1):
            far[i, j] = min(far[i - 1, j] + 1, far[i, j - 1] + 1, far[i - 1, j - 1] + (truth[i - 1] != guess[j - 1]))
    taken, missed, made, i, j = [], [], [], len(truth), len(guess)
    while i or j:
        if i and j and far[i, j] == far[i - 1, j - 1] + (truth[i - 1] != guess[j - 1]):
            if truth[i - 1] != guess[j - 1]: taken.append((truth[i - 1], guess[j - 1]))
            i, j = i - 1, j - 1
        elif i and far[i, j] == far[i - 1, j] + 1: missed.append(truth[i - 1]); i -= 1
        else: made.append(guess[j - 1]); j -= 1
    return taken, missed, made

def most(things, least=2):
    seen = {}
    for thing in things: seen[thing] = seen.get(thing, 0) + 1
    return "  ·  ".join(f"{thing} {n}" for thing, n in sorted(seen.items(), key=lambda kv: -kv[1]) if n >= least)

# What the dictation asks of the reads (dictation.ts): the lines read, and of each glyph how
# many were in them and how many of those were read as another or not at all
def misread(reads):
    seen, wrong = {}, {}
    for text, guess in reads:
        truth = text.replace(" ", "")
        taken, missed, _ = edits(truth, guess.replace(" ", ""))
        for c in truth: seen[c] = seen.get(c, 0) + 1
        for c in [a for a, _ in taken] + missed: wrong[c] = wrong.get(c, 0) + 1
    return {"lines": [text for text, _ in reads], "seen": seen, "wrong": wrong}

# what the parser makes of texts (trees.ts asks it): of each, a `tree` that texts of one
# syntax tree share, and which places between its glyphs `matters`
def parsed(texts):
    said = subprocess.run(["bun", os.path.join(HERE, "trees.ts")], input="\n".join(texts), capture_output=True, text=True, check=True).stdout
    return [json.loads(row) for row in said.splitlines()]

# of each glyph of a text, whether a space stands before it
def spaced(text): return [before == " " for before, c in zip(" " + text, text) if c != " "]

# `text`'s glyphs with the spaces of `guess`, when the two have as many glyphs
def respaced(text, guess):
    mine, theirs = text.replace(" ", ""), guess.replace(" ", "")
    if len(mine) != len(theirs): return None
    return "".join((" " if space else "") + c for space, c in zip(spaced(guess), mine))

# The hand of the copybook: the glyphs' names, the dictated lines – text and strokes, a glyph
# the hand has changed in its new way (`renewed`) – and
# every glyph the pen wrote, the first `cells` of them in cells and the others in the lines,
# cut into their text's glyphs. A line is cut as the page reader that had not learned from
# it cut it – page.py keeps those cuts in CUTS, of each stroke its glyph, with the ink the
# cut is of and whether that reader `read` the line as its text – and a line with no such cut by the reader of cells. Of each glyph: its ink, how many strokes it has and
# which glyph it is. Of each line: `whole`, the glyphs it is made of and what the first
# stroke of each is – after a space where the text has one – and `gaps`, the gaps the hand
# left at places of each kind.
CUTS = os.path.join(KEPT, "cuts.json")
# the dictated lines as the pen wrote them, in the pen's alphabet
def penned(): return [("".join(SAME.get(c, c) for c in text), strokes) for text, strokes in dictated()]

# A glyph the hand writes another way since a moment (ms of the epoch) – n, whose arch now
# leaves the stem at its top. Its ink from before is this hand's no more: wherever the
# readers see a line written before, in lessons and in tests, such a glyph is one written
# since – from a line of the same quarter, so that a reader is tried on none it learned
# from – as large as the old ink and where it stood. `cuts` says of each stroke its glyph;
# a line gives its glyphs only if it is `right`, read as its text: the hand slips to the old
# way, and writes a line other than it was told.
CHANGED = {"n": 1794920400000}
def renewed(lines, cuts, begun, right):
    for glyph, since in CHANGED.items():
        places = lambda text: [g for g, c in enumerate(text.replace(" ", "")) if c == glyph]
        new = {}
        for l, (text, strokes) in enumerate(lines):
            if begun[l] < since or not right[l]: continue
            for g in places(text): new.setdefault(part(text), []).append([even(stroke) for stroke, of in zip(strokes, cuts[l]) if of == g])
        every = sum(new.values(), [])
        for l, (text, strokes) in enumerate(lines):
            if begun[l] >= since or not every or not places(text): continue
            rng, strokes, of = np.random.default_rng(zlib.crc32(text.encode())), list(strokes), list(cuts[l])
            for g in places(text):
                mine, some = [s for s, o in enumerate(of) if o == g], new.get(part(text), every)
                was, now = np.concatenate([strokes[s] for s in mine]), some[rng.integers(len(some))]
                box = np.concatenate(now); scale = np.ptp(was[:, 1]) / max(np.ptp(box[:, 1]), 1)
                made = [((stroke - [(box[:, 0].min() + box[:, 0].max()) / 2, box[:, 1].max()]) * scale + [(was[:, 0].min() + was[:, 0].max()) / 2, was[:, 1].max()]).astype(np.float32) for stroke in now]
                strokes, of = [s for i, s in enumerate(strokes) if i not in mine], [o for i, o in enumerate(of) if i not in mine]
                strokes[mine[0]:mine[0]], of[mine[0]:mine[0]] = made, [g] * len(made)
            lines[l], cuts[l] = (text, strokes), of
    return lines, cuts

def hand():
    cells, glyphs = written()
    lines = penned()
    kept = json.load(open(CUTS)) if os.path.exists(CUTS) else {}
    cuts = {l: kept[text]["of"] for l, (text, strokes) in enumerate(lines) if text in kept and kept[text]["ink"] == [len(stroke) for stroke in strokes]}
    rest = [l for l in range(len(lines)) if l not in cuts]
    reader = Reader(len(glyphs)); reader.load_weights(os.path.join(KEPT, "reader.safetensors")); reader.eval()
    for l, pieces in zip(rest, cut(reader, [lines[l] for l in rest], glyphs)): cuts[l] = [g for g, (*_, n) in enumerate(pieces) for _ in range(n)]
    lines, cuts = renewed(lines, cuts, [at for *_, at in dictated(True)], [kept.get(text, {}).get("read", False) for text, _ in lines])
    same = {glyphs.index(a): glyphs.index(b) for a, b in SAME.items()}
    cells = [(same.get(glyph, glyph), cell, strokes) for glyph, cell, strokes in cells if glyphs[glyph] not in GONE]
    ink, count, label = map(list, zip(*[(*inked(strokes), glyph) for glyph, _, strokes in cells]))
    whole, gaps = {}, {}
    for l, (text, strokes) in enumerate(lines):
        firsts = [FIRST] + [SPACED if space else FIRST for space in spaced(text)[1:]]
        chosen, right, gaps[l] = [], None, {kind: [] for kind in KINDS}
        for g, (glyph, first) in enumerate(zip([glyphs.index(c) for c in text if c != " "], firsts)):
            one, n = inked([stroke for stroke, of in zip(strokes, cuts[l]) if of == g])
            if right is not None: gaps[l][first].append(one[..., 0].min() - right)
            right = one[..., 0].max()
            if glyphs[glyph] in GONE: continue
            chosen.append(len(ink)); ink.append(one); count.append(n); label.append(glyph)
        whole[l] = (np.array(chosen), np.array(firsts), None)
    return glyphs, lines, len(cells), np.stack(ink), np.array(count), np.array(label), whole, gaps

# which quarter a line is of: its text says, so a line written or taken away moves no other
def part(text): return zlib.crc32(text.encode()) % 4

# What a reader of quarter `k` may see of the hand – the lines of the other quarters: the
# glyphs of the cells and of those lines, the gaps in those lines of each kind and how often
# a place is of each kind – and the lines it is tried on
def quarter(k, lines, cells, whole, gaps):
    seen = [l for l in range(len(lines)) if part(lines[l][0]) != k]
    mine = np.concatenate([np.arange(cells)] + [whole[l][0] for l in seen])
    mine_gaps = {kind: np.array(sum((gaps[l][kind] for l in seen), [])) for kind in KINDS}
    often = np.array([len(mine_gaps[kind]) for kind in KINDS]) / sum(map(len, mine_gaps.values()))
    return seen, mine, mine_gaps, often, [l for l in range(len(lines)) if part(lines[l][0]) == k and not set(GONE) & set(lines[l][0])]

# how a reader does on lines it reads with no text: what it reads, in words
def tried(model, lines, held, glyphs):
    guesses = [read(model, lines[l][1], glyphs)[0] for l in held]
    off = sum(sum(map(len, edits(lines[l][0].replace(" ", ""), guess.replace(" ", "")))) for l, guess in zip(held, guesses))
    trees = [text["tree"] for text in parsed([lines[l][0] for l in held] + guesses)]
    return f"{100 * off / sum(len(lines[l][0].replace(' ', '')) for l in held):.1f} % of glyphs wrong, {sum(a == b for a, b in zip(trees, trees[len(held):]))} of {len(held)} lines parse right"

# The dictated lines, four times: a reader learns from the cells and from three quarters
# of the lines – cut into their text's glyphs by the reader of cells – and reads the
# quarter it has not seen, with no text. With `random`, no lesson has a line as it was
# written: the lines give their glyphs and their gaps, and never their text.
if __name__ == "__main__":
    named = dict(word.split("=") for word in sys.argv[1:] if "=" in word)
    rest = [word for word in sys.argv[1:] if word != "random" and "=" not in word]
    steps = int(rest[0]) if rest else 3000
    quarters = [int(k) for k in rest[1:]] or range(4)
    seed, wide, curve, name = int(named.get("seed", 0)), int(named.get("wide", 128)), int(named.get("curve", 0)), named.get("name", "line")
    mx.random.seed(seed)
    glyphs, lines, cells, ink, count, label, whole, gaps = hand()
    every = {kind: sum((gaps[l][kind] for l in gaps), []) for kind in KINDS}
    print(f"{len(lines)} lines, {len(ink) - cells} glyphs in them. Places and their gaps in px: " + "; ".join(f"{name} {len(every[kind])}, {np.percentile(every[kind], [5, 50, 95]).round(1)}" for name, kind in [("no space", FIRST), ("a space", SPACED)]), flush=True)
    wrong = long = same = spaces = counted = lines_read = 0
    taken, missed, made, reads = [], [], [], []
    for k in quarters:
        seen, mine, mine_gaps, often, held = quarter(k, lines, cells, whole, gaps)
        renumber = {old: new for new, old in enumerate(mine)}
        real = [] if "random" in sys.argv else [(np.array([renumber[i] for i in whole[l][0]]), whole[l][1], None) for l in seen if not set(GONE) & set(lines[l][0])]
        def watch(step): print(f"  step {step}: {tried(model, lines, held, glyphs)}", flush=True)
        model = LineReader(len(glyphs), d=wide)
        descend(model, loss, lessons(ink[mine], count[mine], label[mine], cells, real, mine_gaps, often, np.random.default_rng(k + 4 * seed)), steps, 1e-3, say=500, watch=watch if curve else None, every=curve)
        model.save_weights(os.path.join(KEPT, f"{name}-{k}.safetensors"))
        guesses = [read(model, lines[l][1], glyphs)[0] for l in held]
        again = [respaced(lines[l][0], guess) for l, guess in zip(held, guesses)]
        trees = [text["tree"] for text in parsed([lines[l][0] for l in held] + guesses + [text or "" for text in again])]
        for i, (l, guess) in enumerate(zip(held, guesses)):
            text = lines[l][0]
            reads.append((text, guess))
            t, m, e = edits(text.replace(" ", ""), guess.replace(" ", ""))
            wrong += len(t) + len(m) + len(e); long += len(text.replace(" ", ""))
            taken += [f"{a}→{b}" for a, b in t]; missed += m; made += e
            right, fits = trees[i] == trees[len(held) + i], again[i] is not None and trees[i] == trees[2 * len(held) + i]
            same += right; spaces += fits; counted += again[i] is not None; lines_read += 1
            print(f"  {'=' if right else '~' if fits else '≠'} {text}   ⟶   {guess}", flush=True)
        print(f"after quarter {k}: {100 * wrong / long:.1f} % of glyphs wrong; {same} of {lines_read} lines parse to their text's tree; of {counted} lines cut into as many glyphs as their text has, the spaces of {spaces} keep the tree", flush=True)
    print("taken for another:", most(taken))
    print("missed:", most(missed), " made up:", most(made))
    # every line read by a reader that had not learned from it: the dictation follows
    if len(quarters) == 4 and name == "line": json.dump(misread(reads), open(os.path.join(KEPT, "misread.json"), "w"), ensure_ascii=False)
