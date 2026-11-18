# A reader of a line's ink as it stands on the page: which strokes are one glyph, which glyph
# that is, and where the spaces are. What it reads is the ink and not how the ink came: the
# strokes are taken left to right, whenever the pen made them, and each says which stroke
# its glyph starts with – so a glyph may be touched up, gone over or finished late, and two
# glyphs may reach into each other. It learns on the copybook's glyphs in random order
# (line.py's hand), with what a pen does to a glyph it is not happy with.
#   python3 ink/page.py [steps=3000] [quarter ...] [seed=0] [wide=128] [curve=0] [name=page] [stray=1] [runs=0.06] [all=0]
# `curve` is every how many steps a reader is tried, as it learns, on the lines kept from it.
# A reader of quarter k is kept as paper/ink/<name>-k.safetensors.
import json, math, os, sys
import numpy as np
import mlx.core as mx
import mlx.nn as nn
from ink import KEPT, LONGEST, STROKES, Block, descend, vary
from line import CUTS, FIRST, KINDS, LATER, M, SPACED, TURNS, edits, even, given, hand, misread, most, parsed, penned, quarter, respaced, spaced

# a line has at most LONG strokes
LONG = 96

# A stroke is given to the reader as a Bézier curve: PIECES cubic pieces end to end, the
# curve nearest the stroke's M even points when each piece takes an equal part of its
# length – T control points in place of M points. A reader of 7 control points reads as one
# of 32 points does (3.1 % of held-out glyphs wrong, three seeds of each), and learns 2.3
# times as fast. What a pen does to a glyph, and how lessons vary it, is done to the points.
PIECES = 2
T = 3 * PIECES + 1
def curve():
    at = np.linspace(0, 1, M) * PIECES
    piece = np.minimum(at.astype(int), PIECES - 1); u = at - piece
    B = np.zeros((M, T))
    for i in range(M): B[i, 3 * piece[i]:3 * piece[i] + 4] = [(1 - u[i]) ** 3, 3 * u[i] * (1 - u[i]) ** 2, 3 * u[i] ** 2 * (1 - u[i]), u[i] ** 3]
    return B
FIT = np.linalg.pinv(curve())

# the order the reader takes strokes in: by where each begins on the left, the higher first
def across(ink): return np.lexsort((ink[..., 1].min(1), ink[..., 0].min(1)))

# Two transformers: one over a stroke's control points, whose first token comes to the
# stroke's vector, and one over the line's strokes, which knows of each where it is and nothing of
# when it was made. Of each stroke the second says which glyph it is of, what it is to that
# glyph, and – `link` – which stroke of the line that glyph starts with.
class PageReader(nn.Module):
    def __init__(self, classes, d=128, heads=4, drop=0.1):
        super().__init__()
        self.point, self.whole, self.where = nn.Linear(4, d), nn.Linear(2, d), nn.Linear(8 + 2 * len(TURNS), d)
        self.place = mx.random.normal((T + 1, d)) * 0.02
        self.stroke, self.line = [Block(d, heads, drop) for _ in range(2)], [Block(d, heads, drop) for _ in range(4)]
        self.norm, self.glyph, self.role = nn.LayerNorm(d), nn.Linear(d, classes), nn.Linear(d, 3)
        self.asks, self.answers = nn.Linear(d, d), nn.Linear(d, d)
    def __call__(self, points, whole, where, index):
        x = mx.concatenate([self.whole(whole)[:, None], self.point(points)], 1) + self.place
        for block in self.stroke: x = block(x)
        x = mx.concatenate([x[:, 0] + self.where(where), mx.zeros((1, x.shape[-1]))])[index]
        none = (index == points.shape[0])[:, None, None, :] * -1e9
        for block in self.line: x = block(x, none)
        x = self.norm(x)
        link = self.asks(x) @ self.answers(x).transpose(0, 2, 1) / math.sqrt(x.shape[-1]) + none[:, 0]
        return self.glyph(x), self.role(x), link

def loss(model, points, whole, where, index, glyph, role, root):
    said, played, link = model(points, whole, where, index)
    real = index < points.shape[0]
    each = nn.losses.cross_entropy(said, glyph, label_smoothing=0.1, reduction="none") + nn.losses.cross_entropy(played, role, reduction="none") + nn.losses.cross_entropy(link, root, reduction="none")
    return (each * real).sum() / real.sum()

# What a pen does to a glyph it has written, and how often: a tick where a stroke begins or
# ends, a part of a stroke drawn again, a whole stroke gone over, a stroke made in two goes.
STRAY = {"tick": 0.03, "again": 0.03, "over": 0.015, "two": 0.02}
def along(stroke, a, b): return np.stack([np.interp(np.linspace(a, b, M) * (M - 1), np.arange(M), stroke[:, k]) for k in range(2)], 1).astype(np.float32)
# a glyph's strokes [n, M, 2] with one such thing done to it, `much` times as often
def strayed(one, rng, much=1.0):
    if len(one) >= STROKES: return one
    kinds = list(STRAY)
    kind = rng.choice(kinds + [""], p=(lambda p: p + [1 - sum(p)])([min(STRAY[k] * much, 0.24) for k in kinds]))
    if not kind: return one
    s = int(rng.integers(len(one))); stroke = one[s]
    if kind == "tick":
        walk = np.cumsum(rng.normal(0, 1, (M, 2)), 0); walk -= walk[0]
        more = stroke[0 if rng.random() < 0.7 else -1] + walk / max(np.abs(walk).max(), 1e-6) * rng.uniform(0.5, 4) + rng.normal(0, 0.6, 2)
    elif kind == "again":
        a, long = rng.uniform(0, 0.7), rng.uniform(0.15, 0.5)
        more = along(stroke, a, min(a + long, 1)) + rng.normal(0, 1.0, 2)
        if rng.random() < 0.5: more = more[::-1]
    elif kind == "over":
        more = (stroke - stroke.mean(0)) * math.exp(rng.normal(0, 0.03)) + stroke.mean(0) + rng.normal(0, 1.2, 2)
    else:
        cut = rng.uniform(0.25, 0.75)
        more = along(stroke, cut + rng.uniform(-0.04, 0.04), 1) + rng.normal(0, 0.8, 2)
        one = one.copy(); one[s] = along(stroke, 0, cut)
    return np.concatenate([one, more[None].astype(np.float32)])

# Lines to learn on, from a hand (line.py's `composed`): every glyph is varied, some have a
# stray stroke, and then the line is varied. The strokes are given left to right; of each,
# `glyph` is its glyph, `root` is where in the line that glyph's first stroke is, and `role`
# is what a first stroke is to the glyph before it, as the lesson has it.
EACH = np.repeat(np.arange(STROKES), M)
def composed(ink, count, label, lines, rng, stray):
    picks = np.concatenate([chosen for chosen, _, _ in lines])
    varied = vary(ink[picks].reshape(len(picks), -1, 2), np.broadcast_to(EACH, (len(picks), len(EACH))), np.ones(len(picks), np.float32), rng).reshape(len(picks), STROKES, M, 2)
    made, glyph, role, root, at = [], [], [], [], 0
    for chosen, firsts, gaps in lines:
        strokes, of, right = [], [], 0.0
        for j, pick in enumerate(chosen):
            one = varied[at, :count[pick]]; at += 1
            if stray: one = strayed(one, rng, stray)
            one = one + np.float32([right + gaps[j] - one[..., 0].min(), 0])
            right = one[..., 0].max()
            strokes.append(one); of += [j] * len(one)
        line = np.concatenate(strokes) * math.exp(rng.normal(0, 0.08))
        line[..., 1] += rng.normal(0, 1.5) + rng.normal(0, 0.01) * line[..., 0]
        order = across(line)
        of = np.array(of)[order]
        first = np.array([np.flatnonzero(of == g)[0] for g in of])
        kinds = np.where(first == np.arange(len(of)), np.asarray(firsts)[of], LATER); kinds[0] = FIRST
        made.append(line[order]); glyph.append(label[chosen][of]); role.append(kinds); root.append(first)
    longest = max(map(len, made))
    pad = lambda rows: mx.array(np.stack([np.concatenate([row, np.zeros(longest - len(row), row.dtype)]) for row in rows]).astype(np.int32))
    return given(made, FIT) + [pad(glyph), pad(role), pad(root)]

# A lesson is `lines` lines of glyphs drawn at random (line.py's `lessons`): half of them
# from the hand's first `cells` glyphs, the copybook's cells, and half from the rest, which
# were written in lines. Before each glyph is a place: `often` is how often a place is FIRST
# or SPACED, and `gaps` are the gaps the hand left at places of each kind. At `runs`
# of a line's places, a glyph the hand wrote twice in a row stands twice – one in three
# times thrice – as far apart as the hand left it there (`twice`: of each such place, the
# glyph and the gap): two dots side by side are two dots.
def lessons(ink, count, label, cells, gaps, often, rng, stray, runs, twice, lines=32):
    same = {}
    for i, g in enumerate(label): same.setdefault(int(g), []).append(i)
    def some():
        lesson = []
        for _ in range(lines):
            n = rng.integers(3, 21)
            chosen = np.where((rng.random(n) < 0.5) | (cells == len(ink)), rng.integers(0, cells, n), rng.integers(cells, max(len(ink), cells + 1), n))
            apart = {}
            for j in np.flatnonzero(rng.random(n - 1) < runs if len(twice) else []) + 1:
                glyph, gap = twice[rng.integers(len(twice))]
                last = min(j + (rng.random() < 1 / 3), n - 1)
                for at in range(j - 1, last + 1): chosen[at] = rng.choice(same[glyph])
                for at in range(j, last + 1): apart[at] = gap * rng.uniform(0.8, 1.25)
            chosen = chosen[np.cumsum(count[chosen]) <= 64]
            firsts = rng.choice(KINDS, len(chosen), p=often); firsts[0] = FIRST
            wide = np.select([firsts == kind for kind in KINDS], [rng.choice(gaps[kind], len(chosen)) for kind in KINDS]); wide[0] = rng.uniform(0, 40)
            for at, gap in apart.items():
                if at < len(chosen): firsts[at], wide[at] = FIRST, gap
            lesson.append((chosen, firsts, wide))
        return composed(ink, count, label, lesson, rng, stray)
    return some

# What a reader makes of a line's strokes, in any order: the text, of each stroke which
# glyph of the text it is of, and of each glyph how sure the reader is – the share its name
# has among all names. Readers in a list read as one: what each finds likely, multiplied;
# how sure they are is that product's root, as many readers as there are – the product
# itself says 1 of nearly every glyph, right or wrong.
def read(model, strokes, glyphs):
    ink = np.stack([even(stroke) for stroke in strokes[:LONG]])
    order = across(ink)
    line, said, played, linked = given([ink[order]], FIT), 0, 0, 0
    for one in model if isinstance(model, list) else [model]:
        glyph, role, link = one(*line)
        said, played, linked = said + nn.log_softmax(glyph[0], -1), played + nn.log_softmax(role[0], -1), linked + nn.log_softmax(link[0], -1)
    said, played, to = np.array(said), np.array(played), np.array(linked).argmax(1)
    # a stroke's glyph starts where its links end; links that go round start at their leftmost
    root = np.arange(len(to))
    for i in range(len(to)):
        r, seen = i, []
        while to[r] != r and r not in seen: seen.append(r); r = int(to[r])
        root[i] = r if to[r] == r else min(seen)
    roots = sorted(set(root.tolist()))
    named = [said[root == r].sum(0) for r in roots]
    text = "".join((" " if g and played[r, SPACED] > played[r, FIRST] else "") + glyphs[name.argmax()] for g, (r, name) in enumerate(zip(roots, named)))
    of = np.zeros(len(to), int); of[order] = [roots.index(r) for r in root.tolist()]
    readers = len(model) if isinstance(model, list) else 1
    return text, of.tolist(), [float(1 / np.exp((name - name.max()) / readers).sum()) for name in named]

# Which glyph of a line's text each of its strokes is of. The strokes are taken in a row,
# each glyph the next few of it, and of every such cut the reader's likeliest is found: each
# stroke of its glyph, and pointing at the leftmost stroke of its group. The row is the
# strokes as the pen made them – glyphs in the text's order, each in one go – or as they
# stand, left to right, which a glyph written late or touched up needs: the cut is the
# likelier of the two.
def cut(model, text, strokes, glyphs):
    if len(strokes) > LONG: return None
    ink = np.stack([even(stroke) for stroke in strokes])
    order = across(ink); place = np.argsort(order)
    glyph, _, link = model(*given([ink[order]], FIT))
    said, linked = np.array(nn.log_softmax(glyph[0], -1)), np.array(nn.log_softmax(link[0], -1))
    wanted = [glyphs.index(c) for c in text if c != " "]
    # `row`: where each stroke of the row stands; the cut says of each place which glyph is there
    def along(row):
        best = np.full((len(wanted) + 1, len(row) + 1), -np.inf); best[0, 0] = 0
        back = np.zeros(best.shape, int)
        for g, glyph in enumerate(wanted, 1):
            for end in range(g, len(row) + 1):
                for count in range(1, min(LONGEST, end) + 1):
                    group = row[end - count:end]
                    value = best[g - 1, end - count] + said[group, glyph].sum() + linked[group, group.min()].sum()
                    if value > best[g, end]: best[g, end], back[g, end] = value, count
        of, end = np.zeros(len(row), int), len(row)
        for g in range(len(wanted), 0, -1): of[row[end - back[g, end]:end]] = g - 1; end -= back[g, end]
        return best[-1, -1], of
    likely, of = max(along(place), along(np.arange(len(strokes))), key=lambda found: found[0])
    return of[place].tolist() if np.isfinite(likely) else None

# every dictated line, as the pen wrote it, cut by the reader that has not learned from it,
# with the ink the cut is of and whether that reader read the line as its text, but for spaces
def cuts(readers, held, glyphs):
    lines = penned()
    found = {}
    for reader, quarter in zip(readers, held):
        for l in quarter:
            of = cut(reader, *lines[l], glyphs)
            if of: found[lines[l][0]] = {"ink": [len(stroke) for stroke in lines[l][1]], "of": of, "read": read(reader, lines[l][1], glyphs)[0].replace(" ", "") == lines[l][0].replace(" ", "")}
    return found

# how a reader does on lines it reads with no text: what it reads, and in words how well
def tried(model, lines, held, glyphs, inks=None):
    guesses = [read(model, inks[l] if inks else lines[l][1], glyphs)[0] for l in held]
    off = sum(sum(map(len, edits(lines[l][0].replace(" ", ""), guess.replace(" ", "")))) for l, guess in zip(held, guesses))
    trees = [text["tree"] for text in parsed([lines[l][0] for l in held] + guesses)]
    return guesses, f"{100 * off / sum(len(lines[l][0].replace(' ', '')) for l in held):.1f} % of glyphs wrong, {sum(a == b for a, b in zip(trees, trees[len(held):]))} of {len(held)} lines parse right"

# The dictated lines, four times, as line.py has them: a reader learns from the cells and
# from the glyphs and gaps of three quarters of the lines, and reads the quarter it has not
# seen, with no text – as the pen wrote it, and `marred`: one glyph in three with a stray
# stroke. Then it cuts that quarter into its text's glyphs, for the readers that learn next.
# With `all`, that many readers learn from every line, and none is tried. With `cut`, none
# learns: the readers kept as page-k cut the lines again.
if __name__ == "__main__":
    named = dict(word.split("=") for word in sys.argv[1:] if "=" in word)
    rest = [word for word in sys.argv[1:] if "=" not in word]
    if rest == ["cut"]:
        glyphs, lines, cells, _, _, _, whole, gaps = hand()
        readers = [PageReader(len(glyphs)) for _ in range(4)]
        for k, reader in enumerate(readers): reader.load_weights(os.path.join(KEPT, f"page-{k}.safetensors")); reader.eval()
        json.dump(cuts(readers, [quarter(k, lines, cells, whole, gaps)[4] for k in range(4)], glyphs), open(CUTS, "w"), ensure_ascii=False)
        sys.exit()
    steps = int(rest[0]) if rest else 3000
    quarters = [int(k) for k in rest[1:]] or range(4)
    seed, wide, curve, name = int(named.get("seed", 0)), int(named.get("wide", 128)), int(named.get("curve", 0)), named.get("name", "page")
    stray, runs, every = float(named.get("stray", 1)), float(named.get("runs", 0.06)), int(named.get("all", 0))
    mx.random.seed(seed)
    glyphs, lines, cells, ink, count, label, whole, gaps = hand()
    marred = {}
    for l in whole:
        rng = np.random.default_rng(l)
        marred[l] = [stroke for i in whole[l][0] for stroke in strayed(ink[i][:count[i]], rng, 4)]
    print(f"{len(lines)} lines, {len(ink) - cells} glyphs in them; stray {stray}, runs {runs}, wide {wide}, seed {seed}", flush=True)
    wrong = long = same = spaces = counted = lines_read = spoiled = 0
    # places between two glyphs, and those read as the text has them: where a space changes no tree, and where it does
    places, kept = [0, 0], [0, 0]
    taken, missed, made, reads, cut_as = [], [], [], [], {}
    for k in range(every) if every else quarters:
        seen, mine, mine_gaps, often, held = quarter(-1 if every else k, lines, cells, whole, gaps)
        twice = [(int(label[a]), float(ink[b, ..., 0].min() - ink[a, ..., 0].max())) for l in seen for a, b in zip(whole[l][0], whole[l][0][1:]) if label[a] == label[b]]
        def watch(step): print(f"  step {step}: {tried(model, lines, held, glyphs)[1]}; marred: {tried(model, lines, held, glyphs, marred)[1]}", flush=True)
        model = PageReader(len(glyphs), d=wide)
        descend(model, loss, lessons(ink[mine], count[mine], label[mine], cells, mine_gaps, often, np.random.default_rng(k + 4 * seed), stray, runs, twice), steps, 1e-3, say=500, watch=watch if curve and not every else None, every=curve)
        model.save_weights(os.path.join(KEPT, f"{name}-{k}.safetensors"))
        if every: continue
        cut_as.update(cuts([model], [held], glyphs))
        guesses, _ = tried(model, lines, held, glyphs)
        harmed, _ = tried(model, lines, held, glyphs, marred)
        again = [respaced(lines[l][0], guess) for l, guess in zip(held, guesses)]
        texts = parsed([lines[l][0] for l in held] + guesses + [text or "" for text in again])
        trees = [text["tree"] for text in texts]
        for i, (l, guess) in enumerate(zip(held, guesses)):
            text = lines[l][0]
            reads.append((text, guess))
            t, m, e = edits(text.replace(" ", ""), guess.replace(" ", ""))
            wrong += len(t) + len(m) + len(e); long += len(text.replace(" ", ""))
            spoiled += sum(map(len, edits(text.replace(" ", ""), harmed[i].replace(" ", ""))))
            taken += [f"{a}→{b}" for a, b in t]; missed += m; made += e
            right, fits = trees[i] == trees[len(held) + i], again[i] is not None and trees[i] == trees[2 * len(held) + i]
            same += right; spaces += fits; counted += again[i] is not None; lines_read += 1
            if again[i] is not None:
                for mine, theirs, matter in zip(spaced(text)[1:], spaced(guess)[1:], texts[i]["matters"]): places[matter] += 1; kept[matter] += mine == theirs
            print(f"  {'=' if right else '~' if fits else '≠'} {text}   ⟶   {guess}", flush=True)
        print(f"after quarter {k}: {100 * wrong / long:.1f} % of glyphs wrong, marred {100 * spoiled / long:.1f} %; {same} of {lines_read} lines parse to their text's tree; of {counted} lines cut into as many glyphs as their text has, the spaces of {spaces} keep the tree: a place is as the text has it at {kept[1]} of {places[1]} where a space changes the tree, at {kept[0]} of {places[0]} where it does not", flush=True)
    if every: sys.exit()
    print("taken for another:", most(taken))
    print("missed:", most(missed), " made up:", most(made))
    # every line read, and cut, by a reader that had not learned from it: the dictation
    # follows the reads, and the next readers learn from the cuts
    if len(quarters) == 4 and name == "page":
        json.dump(misread(reads), open(os.path.join(KEPT, "misread.json"), "w"), ensure_ascii=False)
        json.dump(cut_as, open(CUTS, "w"), ensure_ascii=False)
