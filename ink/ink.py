# A reader of one glyph's ink, trained on the copybook (paper/copybook): which glyph of the
# pen's alphabet is this? It sees the ink and where the ink sits over the baseline, and
# nothing of what is written around it.
import json, math, os, sys, time
import numpy as np
import mlx.core as mx
import mlx.nn as nn
import mlx.optimizers as optim
from mlx.utils import tree_flatten

# this machine swaps: the reader's training keeps to 1 GB, and to 256 MB of freed buffers
mx.set_memory_limit(1 << 30)
mx.set_cache_limit(256 << 20)

HERE = os.path.dirname(os.path.abspath(__file__))
# the copybook to learn from: the one the pen writes in, or a kept one that COPYBOOK names
COPYBOOK = os.environ.get("COPYBOOK") or os.path.join(HERE, "../paper/copybook")
KEPT = os.path.join(HERE, "../paper/ink")

# a glyph is N points, even steps along its ink, in the order it was written
N = 64
STROKES = 8
# ink smaller than this many px is small, not a shape to stretch
FLOOR = 8

def resample(strokes, n=N):
    lengths = [float(np.hypot(*np.diff(s, axis=0).T).sum()) for s in strokes]
    total = sum(lengths) or 1.0
    # a stroke has points by its share of the ink, and at least one
    share = [max(1, round(n * length / total)) for length in lengths]
    while sum(share) > n: share[int(np.argmax(share))] -= 1
    while sum(share) < n: share[int(np.argmax(lengths))] += 1
    P, S = [], []
    for i, (stroke, count) in enumerate(zip(strokes, share)):
        along = np.concatenate([[0], np.cumsum(np.hypot(*np.diff(stroke, axis=0).T))])
        at = np.linspace(0, along[-1], count) if count > 1 else np.array([along[-1] / 2])
        P.append(np.stack([np.interp(at, along, stroke[:, 0]), np.interp(at, along, stroke[:, 1])], 1))
        S.append(np.full(count, min(i, STROKES - 1), np.int8))
    return np.concatenate(P).astype(np.float32), np.concatenate(S)

# the copybook's cells as the pen made them: the glyph's number, the cell, and the strokes
# in px, y from the baseline (down is +)
def written():
    cells, glyphs = [], []
    for file in sorted(os.listdir(COPYBOOK)):
        if not file.endswith(".json") or file.startswith("line-"): continue
        kept = json.load(open(os.path.join(COPYBOOK, file)))
        glyphs.append(kept["glyph"])
        for cell, sample in enumerate(kept["samples"][:10]):
            if sample: cells.append((len(glyphs) - 1, cell, [np.array([p[:2] for p in stroke["points"]], np.float32) for stroke in sample["strokes"]]))
    return cells, glyphs

# the copybook's glyphs, each N points, and the cell of each
def copybook():
    cells, glyphs = written()
    even = [resample(strokes) for _, _, strokes in cells]
    return np.stack([P for P, _ in even]), np.stack([S for _, S in even]), np.array([label for label, _, _ in cells]), np.array([cell for _, cell, _ in cells]), glyphs

# The same glyph as another hand's moment would have it: bent, leaned, stretched, its
# strokes a little apart, and – where it has a baseline – a little up or down
def vary(P, S, placed, rng):
    B = len(P)
    low, high = P.min(1, keepdims=True), P.max(1, keepdims=True)
    middle, size = (low + high) / 2, np.maximum((high - low).max(2, keepdims=True), FLOOR)
    Q0 = P - middle
    Q = Q0.copy()
    for _ in range(2):
        wave = rng.uniform(0.3, 1.2, (B, 1, 2)) * rng.choice([-1, 1], (B, 1, 2))
        Q += rng.normal(0, 0.03, (B, 1, 2)) * size * np.sin(2 * math.pi * ((Q0 / size * wave).sum(2, keepdims=True) + rng.uniform(0, 1, (B, 1, 1))))
    Q += np.take_along_axis(rng.normal(0, 0.02, (B, STROKES, 2)) * size, np.repeat(S[:, :, None].astype(np.int64), 2, 2), 1)
    turn, lean = rng.normal(0, math.radians(2), B), rng.normal(0, 0.08, B)
    wide, tall = np.exp(rng.normal(0, 0.08, B)), np.exp(rng.normal(0, 0.08, B))
    A = np.stack([np.stack([np.cos(turn) * wide, np.cos(turn) * lean - np.sin(turn) * tall], 1),
                  np.stack([np.sin(turn) * wide, np.sin(turn) * lean + np.cos(turn) * tall], 1)], 1)
    Q = np.einsum("bij,bnj->bni", A, Q)
    middle = middle + np.stack([np.zeros(B), rng.normal(0, 1.2, B) * placed], 1)[:, None]
    return (Q + middle).astype(np.float32)

# What the reader is given. A point: where it is in the glyph's box, the step to it, and
# whether a stroke starts or ends there. The whole: the box's size and its height over the
# baseline – nothing, for ink that has no baseline.
def given(P, S, placed):
    low, high = P.min(1), P.max(1)
    middle, size = (low + high) / 2, np.maximum((high - low).max(1), FLOOR)
    shape = (P - middle[:, None]) / size[:, None, None]
    start = np.ones(S.shape, bool); start[:, 1:] = S[:, 1:] != S[:, :-1]
    end = np.ones(S.shape, bool); end[:, :-1] = start[:, 1:]
    step = np.zeros_like(shape); step[:, 1:] = shape[:, 1:] - shape[:, :-1]; step[start] = 0
    points = np.concatenate([shape * 2, step * N / 4, start[..., None], end[..., None]], 2).astype(np.float32)
    whole = (np.stack([np.log(size / 32), middle[:, 1] / 32, np.ones(len(P))], 1) * placed[:, None]).astype(np.float32)
    return mx.array(points), mx.array(S.astype(np.int32)), mx.array(whole)

class Block(nn.Module):
    def __init__(self, d, heads, drop):
        super().__init__()
        self.before, self.after = nn.LayerNorm(d), nn.LayerNorm(d)
        self.attend = nn.MultiHeadAttention(d, heads)
        self.wide, self.narrow, self.drop = nn.Linear(d, 4 * d), nn.Linear(4 * d, d), nn.Dropout(drop)
    def __call__(self, x, mask=None):
        y = self.before(x)
        x = x + self.drop(self.attend(y, y, y, mask))
        return x + self.drop(self.narrow(nn.gelu(self.wide(self.after(x)))))

# a transformer over the glyph's points and one more token, the whole; what that token
# comes to is the glyph's vector, and `head` names it
class Reader(nn.Module):
    def __init__(self, classes, d=128, layers=4, heads=4, drop=0.1):
        super().__init__()
        self.point, self.whole, self.stroke = nn.Linear(6, d), nn.Linear(3, d), nn.Embedding(STROKES, d)
        self.place = mx.random.normal((N + 1, d)) * 0.02
        self.blocks = [Block(d, heads, drop) for _ in range(layers)]
        self.norm, self.head = nn.LayerNorm(d), nn.Linear(d, classes)
    def vector(self, points, strokes, whole):
        x = mx.concatenate([self.whole(whole)[:, None], self.point(points) + self.stroke(strokes)], 1) + self.place
        for block in self.blocks: x = block(x)
        return self.norm(x[:, 0])
    def __call__(self, points, strokes, whole):
        return self.head(self.vector(points, strokes, whole))

# AdamW down the `loss` of what `some()` gives, the rate warming up and then falling as a cosine.
# With `watch`, every `every` steps the model, as one that reads, is shown to it.
def descend(model, loss, some, steps, peak, say=None, watch=None, every=0):
    warm = min(200, steps // 10)
    rate = optim.join_schedules([optim.linear_schedule(1e-6, peak, warm), optim.cosine_decay(peak, steps - warm)], [warm])
    optimizer = optim.AdamW(learning_rate=rate, weight_decay=0.05)
    step = nn.value_and_grad(model, loss)
    model.train()
    began = time.time()
    for i in range(steps):
        value, grads = step(model, *some())
        optimizer.update(model, grads)
        mx.eval(model.parameters(), optimizer.state, value)
        if say and (i + 1) % say == 0: print(f"  step {i + 1}/{steps} loss {value.item():.3f} {time.time() - began:.0f} s", flush=True)
        if watch and (i + 1) % every == 0: model.eval(); watch(i + 1); model.train()
    model.eval()

def train(model, P, S, y, placed, steps, peak, seed=0, batch=128, say=None):
    rng = np.random.default_rng(seed)
    order, at = rng.permutation(len(P)), 0
    def some():
        nonlocal order, at
        if at + batch > len(order): order, at = rng.permutation(len(P)), 0
        pick = order[at:at + batch]; at += batch
        return *given(vary(P[pick], S[pick], placed[pick], rng), S[pick], placed[pick]), mx.array(y[pick])
    loss = lambda model, points, strokes, whole, y: nn.losses.cross_entropy(model(points, strokes, whole), y, label_smoothing=0.1, reduction="mean")
    descend(model, loss, some, steps, peak, say)

def said(model, P, S, placed, batch=512):
    return np.concatenate([np.array(model(*given(P[i:i + batch], S[i:i + batch], placed[i:i + batch]))) for i in range(0, len(P), batch)])

def keep(model, name): model.save_weights(os.path.join(KEPT, name + ".safetensors"))

# a model with the weights kept under `name`, all but the head: its classes are others
def kept(model, name):
    weights = [(k, v) for k, v in mx.load(os.path.join(KEPT, name + ".safetensors")).items() if not k.startswith("head.")]
    model.load_weights(weights, strict=False)
    return model

# the copybook's test: of a glyph's ten cells, cells k and k + 5 are held out in fold k
FOLDS = 5
def folds(cells):
    for k in range(FOLDS):
        held = cells % FOLDS == k
        yield k, ~held, held

def pairs(truth, guess, glyphs, least=2):
    mixed = {}
    for t, g in zip(truth, guess):
        if t != g:
            pair = " ".join(sorted([glyphs[t], glyphs[g]]))
            mixed[pair] = mixed.get(pair, 0) + 1
    return "  ·  ".join(f"{pair} {n}" for pair, n in sorted(mixed.items(), key=lambda kv: -kv[1]) if n >= least)

def nearest(P, S, y, cells, glyphs):
    placed = np.ones(len(P), np.float32)
    for name, flat in [("shape alone", lambda P: np.array(given(P, S, placed)[0][:, :, :2]).reshape(len(P), -1)),
                       ("with size and baseline", lambda P: (P - np.stack([(P[:, :, 0].min(1) + P[:, :, 0].max(1)) / 2, np.zeros(len(P))], 1)[:, None]).reshape(len(P), -1))]:
        X, right = flat(P), 0
        for _, learn, held in folds(cells):
            far = ((X[held][:, None] - X[learn][None]) ** 2).sum(2)
            right += (y[learn][far.argmin(1)] == y[held]).sum()
        print(f"nearest neighbour, {name}: {100 * right / len(P):.1f} %")

# the copybook's dictated lines: the text, and the strokes as the pen made them – and, with
# `begun`, when the pen began the line, in ms of the epoch
def dictated(begun=False):
    lines = []
    for file in sorted(os.listdir(COPYBOOK)):
        if not file.startswith("line-"): continue
        kept = json.load(open(os.path.join(COPYBOOK, file)))
        if kept["samples"] and kept["samples"][0]:
            strokes = kept["samples"][0]["strokes"]
            lines.append((kept["glyph"], [np.array([p[:2] for p in stroke["points"]], np.float32) for stroke in strokes]) + ((strokes[0]["at"],) if begun else ()))
    return lines

# Which strokes of a line are which glyph of its text, when the pen wrote the glyphs in
# the text's order and each in one go: of every way to cut the strokes into the text's
# glyphs, the one the reader finds likeliest. A glyph has at most LONGEST strokes. Of each
# glyph: its number, its points, what the reader would call it, and how many strokes it has.
LONGEST = 7
def cut(model, lines, glyphs):
    index = {glyph: i for i, glyph in enumerate(glyphs)}
    P, S, of = [], [], []
    for l, (_, strokes) in enumerate(lines):
        for end in range(1, len(strokes) + 1):
            for count in range(1, min(LONGEST, end) + 1):
                p, s = resample(strokes[end - count:end])
                P.append(p); S.append(s); of.append((l, end, count))
    P, S = np.stack(P), np.stack(S)
    scores = said(model, P, S, np.ones(len(P), np.float32))
    scores = scores - np.log(np.exp(scores - scores.max(1, keepdims=True)).sum(1, keepdims=True)) - scores.max(1, keepdims=True)
    score = {key: i for i, key in enumerate(of)}
    found = []
    for l, (text, strokes) in enumerate(lines):
        wanted = [index[c] for c in text if c != " "]
        best = np.full((len(wanted) + 1, len(strokes) + 1), -np.inf); best[0, 0] = 0
        back = np.zeros(best.shape, int)
        for g, glyph in enumerate(wanted, 1):
            for end in range(g, len(strokes) + 1):
                for count in range(1, min(LONGEST, end) + 1):
                    value = best[g - 1, end - count] + scores[score[(l, end, count)], glyph]
                    if value > best[g, end]: best[g, end], back[g, end] = value, count
        if not np.isfinite(best[-1, -1]): found.append(None); continue
        groups, end = [], len(strokes)
        for g in range(len(wanted), 0, -1):
            i = score[(l, end, back[g, end])]
            groups.append((wanted[g - 1], P[i], S[i], int(scores[i].argmax()), int(back[g, end])))
            end -= back[g, end]
        found.append(groups[::-1])
    return found

def hwrt(part):
    data = np.load(os.path.join(KEPT, f"hwrt-{part}.npz"))
    return data["points"].astype(np.float32), data["strokes"], data["labels"], data["hands"]

if __name__ == "__main__":
    what, rest = sys.argv[1], sys.argv[2:]
    mx.random.seed(0)
    if what == "nearest":
        nearest(*copybook())
    # others' hands first: the HWRT symbols, to learn what ink is before the copybook's ten
    if what == "before":
        steps = int(rest[0]) if rest else 20000
        P, S, labels, _ = hwrt("train")
        names = np.unique(labels)
        y, none = np.searchsorted(names, labels), np.zeros(len(P), np.float32)
        model = Reader(len(names))
        train(model, P, S, y, none, steps, 1e-3, say=500)
        keep(model, "before")
        tP, tS, tl, _ = hwrt("test")
        guess = said(model, tP, tS, np.zeros(len(tP), np.float32))
        top3 = np.argsort(-guess, 1)[:, :3]
        ty = np.searchsorted(names, tl)
        print(f"HWRT test: top-1 {100 * (top3[:, 0] == ty).mean():.1f} %, top-3 {100 * (top3 == ty[:, None]).any(1).mean():.1f} %")
    # The dictated lines. A reader of all the cells cuts each line into its text's glyphs,
    # and names each piece on its own: how well do the cells teach the lines? Then, four
    # times, a reader learns the cells and the pieces of three quarters of the lines, and
    # names the pieces of the quarter it has not seen.
    if what == "lines":
        steps = int(rest[0]) if rest else 600
        P, S, y, cells, glyphs = copybook()
        ones = lambda n: np.ones(n, np.float32)
        model = Reader(len(glyphs))
        train(model, P, S, y, ones(len(P)), steps, 1e-3)
        keep(model, "reader")
        lines = dictated()
        found = cut(model, lines, glyphs)
        whole = [(l, piece) for l, pieces in enumerate(found) if pieces for piece in pieces]
        print(f"{len(lines)} lines, {sum(pieces is None for pieces in found)} with fewer strokes than glyphs:", " | ".join(lines[l][0] for l, pieces in enumerate(found) if pieces is None))
        truth, guess = np.array([piece[0] for _, piece in whole]), np.array([piece[3] for _, piece in whole])
        print(f"a reader of the cells names {100 * (truth == guess).mean():.1f} % of the lines' {len(whole)} glyphs")
        print("mixed:", pairs(truth, guess, glyphs))
        of, lP, lS = np.array([l for l, _ in whole]), np.stack([piece[1] for _, piece in whole]), np.stack([piece[2] for _, piece in whole])
        again = np.zeros(len(whole), int)
        for k in range(4):
            held = of % 4 == k
            model = Reader(len(glyphs))
            train(model, np.concatenate([P, lP[~held]]), np.concatenate([S, lS[~held]]), np.concatenate([y, truth[~held]]), ones(len(P) + (~held).sum()), steps, 1e-3, seed=k)
            again[held] = said(model, lP[held], lS[held], ones(held.sum())).argmax(1)
        print(f"a reader of the cells and of the other lines names {100 * (truth == again).mean():.1f} %")
        print("mixed:", pairs(truth, again, glyphs))
    # the copybook, fold by fold: from nothing, or from what `before` kept. `per` is how
    # many of a glyph's eight learning cells the reader may see
    if what in ("scratch", "after"):
        steps, per = int(rest[0]) if rest else 3000, int(rest[1]) if len(rest) > 1 else 8
        P, S, y, cells, glyphs = copybook()
        placed, truth, guess = np.ones(len(P), np.float32), [], []
        for k, learn, held in folds(cells):
            if per < 8:
                seen = [(k + 1 + i) % FOLDS + j for i in range((per + 1) // 2) for j in (0, 5)][:per]
                learn = learn & np.isin(cells, seen)
            model = Reader(len(glyphs))
            if what == "after": kept(model, "before")
            train(model, P[learn], S[learn], y[learn], placed[learn], steps, 1e-3 if what == "scratch" else 5e-4, seed=k)
            answer = said(model, P[held], S[held], placed[held]).argmax(1)
            truth += list(y[held]); guess += list(answer)
            print(f"  fold {k}: {100 * (answer == y[held]).mean():.1f} % of {held.sum()} ({learn.sum()} learned)", flush=True)
        truth, guess = np.array(truth), np.array(guess)
        print(f"{what}, {per} samples a glyph, {steps} steps: {100 * (truth == guess).mean():.1f} % right")
        print("mixed:", pairs(truth, guess, glyphs))
