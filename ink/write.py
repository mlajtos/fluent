# A writer of one glyph's ink. The ink is the pen's moves, one after another, and the writer
# says how likely each move is after the moves before it, for a glyph of a given name. So it
# writes – a glyph drawn move by move is a new sample of the hand – and it reads: ink is the
# glyph under whose name its moves are likeliest.
#   python3 ink/write.py [steps=4000] [fold ...] [seed=0] [wide=64] [deep=3] [curve=500] [varied=1] [name=writer]
# A fold's writer learns eight of a glyph's ten cells and reads the two kept from it; it is
# kept as paper/ink/<name>-<fold>.safetensors, and a sheet of what it writes beside it.
import math, os, sys
import numpy as np
import mlx.core as mx
import mlx.nn as nn
from mlx.utils import tree_flatten
from ink import FOLDS, KEPT, STROKES, Block, Reader, descend, pairs, resample, said, vary, written
from line import EACH, M, even, inked

# The pen's moves. A stroke starts with a jump from where the pen was – at first the baseline,
# under the glyph's first point – X then Y, each a whole number of MOVE px and at most REACH of
# them; then the pen goes MOVE px at a time, each time one of TURNS ways. END ends the glyph.
# A glyph is at most LONGEST moves.
MOVE, TURNS, REACH, LONGEST = 3.0, 32, 18, 160
X, Y = TURNS, TURNS + 2 * REACH + 1
END = Y + 2 * REACH + 1
MOVES = END + 1

# the moves of glyphs – ink [STROKES, M, 2], `count` strokes of it – and how many each has.
# The pen aims each move at where the ink is that far along, so what it draws stays on the ink.
def moves(ink, count):
    ink = ink - np.stack([ink[:, 0, 0, 0], np.zeros(len(ink))], 1)[:, None, None]
    pen, rows = np.zeros((len(ink), 2)), [[] for _ in ink]
    for s in range(count.max()):
        has = np.flatnonzero(count > s)
        stroke, each = ink[has, s], np.arange(len(has))
        jump = np.clip(np.round((stroke[:, 0] - pen[has]) / MOVE), -REACH, REACH).astype(int)
        at = pen[has] + jump * MOVE
        long = np.hypot(*np.diff(stroke, axis=1).transpose(2, 0, 1)).sum(1)
        n = np.round(long / MOVE).astype(int)
        turns = np.zeros((len(has), n.max()), int)
        for i in range(n.max()):
            where = np.minimum((i + 1) * MOVE / np.maximum(long, 1e-6), 1) * (M - 1)
            low = np.minimum(where.astype(int), M - 2)
            part = (where - low)[:, None]
            aim = stroke[each, low] * (1 - part) + stroke[each, low + 1] * part - at
            turns[:, i] = np.round(np.arctan2(aim[:, 1], aim[:, 0]) * TURNS / (2 * math.pi)).astype(int) % TURNS
            angle = turns[:, i] * 2 * math.pi / TURNS
            at = at + (i < n)[:, None] * MOVE * np.stack([np.cos(angle), np.sin(angle)], 1)
        pen[has] = at
        for j, b in enumerate(has): rows[b] += [X + REACH + jump[j, 0], Y + REACH + jump[j, 1], *turns[j, :n[j]]]
    rows = [row[:LONGEST - 1] + [END] for row in rows]
    padded = np.full((len(rows), LONGEST), END, np.int32)
    for b, row in enumerate(rows): padded[b, :len(row)] = row
    return padded, np.array([len(row) for row in rows])

# the strokes that moves draw, whatever the moves
def strokes(moved):
    made, pen, one = [], np.zeros(2), None
    for move in moved:
        if move == END: break
        if move >= X and one is not None and move < Y: made.append(np.array(one, np.float32)); one = None
        if move >= Y: pen = pen + [0, (move - Y - REACH) * MOVE]; one = [pen]
        elif move >= X: pen = pen + [(move - X - REACH) * MOVE, 0]
        else:
            angle = move * 2 * math.pi / TURNS
            one = one or [pen]
            pen = pen + MOVE * np.array([math.cos(angle), math.sin(angle)])
            one.append(pen)
    return made + ([np.array(one, np.float32)] if one is not None else [])

# where a glyph is written: alone in a cell of the copybook, or in a line
CELL, LINE = 0, 1

# A transformer over the moves so far, each with the glyph's name and where it is written
# added: of every move, how likely each next one is. Its attention looks only back.
class Writer(nn.Module):
    def __init__(self, classes, d=64, layers=3, heads=4, drop=0.1):
        super().__init__()
        self.move, self.glyph, self.where = nn.Embedding(MOVES + 1, d), nn.Embedding(classes, d), nn.Embedding(2, d)
        self.place = mx.random.normal((LONGEST, d)) * 0.02
        self.blocks = [Block(d, heads, drop) for _ in range(layers)]
        self.norm, self.head = nn.LayerNorm(d), nn.Linear(d, MOVES)
    def __call__(self, moved, glyph, where):
        before = mx.concatenate([mx.full((moved.shape[0], 1), MOVES, mx.int32), moved[:, :-1]], 1)
        x = self.move(before) + (self.glyph(glyph) + self.where(where))[:, None] + self.place[:moved.shape[1]]
        back = nn.MultiHeadAttention.create_additive_causal_mask(moved.shape[1])
        for block in self.blocks: x = block(x, back)
        return self.head(self.norm(x))

# how unlikely, in nats, each glyph's moves are under its name
def unlikely(model, moved, long, glyph, where):
    each = nn.losses.cross_entropy(model(moved, glyph, where), moved, reduction="none")
    return (each * (mx.arange(moved.shape[1]) < long[:, None])).sum(1)

# how likely the moves of each ink, written `where`, are under every name: [inks, classes] nats
def likely(model, moved, long, classes, where, at_once=8):
    order, out = np.argsort(long), np.zeros((len(moved), classes), np.float32)
    names = mx.arange(classes)
    for i in range(0, len(order), at_once):
        pick = order[i:i + at_once]
        some = mx.array(np.repeat(moved[pick, :long[pick].max()], classes, 0))
        out[pick] = -np.array(unlikely(model, some, mx.array(np.repeat(long[pick], classes)), mx.concatenate([names] * len(pick)), mx.full(len(some), where, mx.int32))).reshape(len(pick), classes)
    return out

# glyphs of the given names, drawn move by move as written `where`: the moves. A `warmth`
# under 1 keeps the pen to its likelier moves.
def write(model, glyph, where, warmth=1.0):
    glyph, moved = mx.array(glyph), mx.zeros((len(glyph), 0), mx.int32)
    where, ended = mx.full(len(glyph), where, mx.int32), np.zeros(len(glyph), bool)
    while moved.shape[1] < LONGEST and not ended.all():
        ahead = model(mx.concatenate([moved, mx.zeros((len(glyph), 1), mx.int32)], 1), glyph, where)[:, -1]
        moved = mx.concatenate([moved, mx.random.categorical(ahead / warmth)[:, None].astype(mx.int32)], 1)
        ended |= np.array(moved[:, -1]) == END
    return np.array(moved)

# A writer that has learned a hand: its glyphs' ink, how many strokes each has, which glyph
# each is and where it was written. With `varied`, every glyph it is shown is first bent,
# leaned and stretched a little (ink.py's `vary`).
def taught(ink, count, label, where, classes, steps, rng, wide=64, deep=3, varied=True, watch=None, every=0, batch=128):
    model = Writer(classes, d=wide, layers=deep)
    def some():
        pick = rng.integers(0, len(ink), batch)
        one = vary(ink[pick].reshape(batch, -1, 2), np.broadcast_to(EACH, (batch, len(EACH))), np.ones(batch, np.float32), rng).reshape(batch, STROKES, M, 2) if varied else ink[pick]
        m, n = moves(one, count[pick])
        return mx.array(m[:, :n.max()]), mx.array(n), mx.array(label[pick]), mx.array(where[pick])
    loss = lambda model, m, n, g, w: unlikely(model, m, n, g, w).sum() / n.sum()
    descend(model, loss, some, steps, 2e-3, say=every or 500, watch=(lambda step: watch(model, step)) if watch else None, every=every)
    return model

# the hand's glyphs in the reader's eyes (ink.py): each N points, and their strokes
def seen(glyphs_strokes):
    P, S = zip(*[resample(one) for one in glyphs_strokes])
    return np.stack(P), np.stack(S)

def sheet(rows, path, cell=70):
    colors = ["#000", "#c00", "#06c", "#080", "#a0a", "#c60", "#088", "#888"]
    out = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{cell * max(len(r) for _, r in rows) + 40}" height="{cell * len(rows)}"><rect width="100%" height="100%" fill="white"/>']
    for r, (name, inks) in enumerate(rows):
        out.append(f'<text x="4" y="{cell * r + 40}" font-size="16">{name.replace("&", "&amp;").replace("<", "&lt;")}</text>')
        for c, one in enumerate(inks):
            left = min(float(s[:, 0].min()) for s in one) if one else 0
            for i, s in enumerate(one):
                s = s if len(s) > 1 else np.concatenate([s, s + [0.5, 0]])
                points = " ".join(f"{40 + cell * c + 12 + x - left:.1f},{cell * r + 52 + y:.1f}" for x, y in s)
                out.append(f'<polyline points="{points}" fill="none" stroke="{colors[i % 8] if c >= 3 else "#999"}" stroke-width="1.3" stroke-linecap="round"/>')
    open(path, "w").write("\n".join(out + ["</svg>"]))

if __name__ == "__main__":
    named = dict(word.split("=") for word in sys.argv[1:] if "=" in word)
    rest = [int(word) for word in sys.argv[1:] if "=" not in word]
    steps, folds = rest[0] if rest else 4000, rest[1:] or range(FOLDS)
    seed, wide, deep, curve = (int(named.get(key, default)) for key, default in [("seed", 0), ("wide", 64), ("deep", 3), ("curve", 500)])
    varied, name = named.get("varied", "1") == "1", named.get("name", "writer")
    cells, glyphs = written()
    ink, count = map(np.array, zip(*[inked(one) for _, _, one in cells]))
    label, cell = np.array([glyph for glyph, _, _ in cells]), np.array([c for _, c, _ in cells])
    moved, long = moves(ink, count)
    back = [strokes(row) for row in moved]
    off = np.mean([np.hypot(*(even(a) - b).T).mean() for one, n, drawn in zip(ink, count, back) if len(drawn) == n for a, b in zip(drawn, one - np.float32([one[0, 0, 0], 0]))])
    print(f"{len(cells)} cells of {len(glyphs)} glyphs as moves: {long.mean():.0f} moves a glyph, {(long == LONGEST).sum()} cut short; drawn back, {sum(len(d) == n for d, n in zip(back, count))} have their strokes and a point is {off:.2f} px off", flush=True)
    judge = Reader(len(glyphs)); judge.load_weights(os.path.join(KEPT, "reader.safetensors")); judge.eval()
    truth, guess = [], []
    for k in folds:
        mx.random.seed(seed); rng = np.random.default_rng(k + FOLDS * seed)
        held = cell % FOLDS == k
        mine, kept = np.flatnonzero(~held), np.flatnonzero(held)
        def watch(model, step):
            scores = likely(model, moved[kept], long[kept], len(glyphs), CELL)
            own = scores[np.arange(len(kept)), label[kept]]
            print(f"  step {step}: the kept cells are {-own.mean():.1f} nats a glyph ({-own.sum() / long[kept].sum():.3f} a move), {100 * (scores.argmax(1) == label[kept]).mean():.1f} % read right", flush=True)
        model = taught(ink[mine], count[mine], label[mine], np.zeros(len(mine), np.int32), len(glyphs), steps, rng, wide, deep, varied, watch if curve else None, curve)
        if k == folds[0]: print(f"a writer {wide} wide, {deep} deep: {sum(v.size for _, v in tree_flatten(model.parameters()))} weights", flush=True)
        model.save_weights(os.path.join(KEPT, f"{name}-{k}.safetensors"))
        scores = likely(model, moved[kept], long[kept], len(glyphs), CELL)
        truth += list(label[kept]); guess += list(scores.argmax(1))
        top = np.argsort(-scores, 1)[:, :5]
        print(f"fold {k}: {100 * (scores.argmax(1) == label[kept]).mean():.1f} % of {len(kept)} kept cells read right, the name among the 2 / 5 likeliest {100 * (top[:, :2] == label[kept][:, None]).any(1).mean():.1f} / {100 * (top == label[kept][:, None]).any(1).mean():.1f} %", flush=True)
        # What it writes: does a reader of the cells take a written glyph for its name, and is
        # it as far from the cells the writer learned as the hand's own kept cells are?
        flat = lambda P: (P - np.stack([(P[:, :, 0].min(1) + P[:, :, 0].max(1)) / 2, np.zeros(len(P))], 1)[:, None]).reshape(len(P), -1)
        known = flat(seen([cells[i][2] for i in mine])[0])
        near = lambda P, of: np.array([np.sqrt(((known[label[mine] == g] - p) ** 2).sum(1).min() / P.shape[1]) for p, g in zip(flat(P), of)])
        hand = near(seen([cells[i][2] for i in kept])[0], label[kept])
        every = np.repeat(np.arange(len(glyphs)), 4)
        for warmth in (1.0, 0.7):
            drawn = [strokes(row) for row in write(model, every, CELL, warmth)]
            whole = np.array([len(one) > 0 for one in drawn])
            P, S = seen([one for one in drawn if one])
            named_right = said(judge, P, S, np.ones(len(P), np.float32)).argmax(1) == every[whole]
            print(f"  written at warmth {warmth}: {100 * named_right.mean():.1f} % read as their name by the reader of cells; a point is {np.median(near(P, every[whole])):.1f} px from the nearest cell learned (the hand's kept cells: {np.median(hand):.1f} px)", flush=True)
        if k == folds[0]:
            shown = [glyphs.index(g) for g in "aG0l⌊⍴∇#⊗θ" if g in glyphs]
            warm, calm = write(model, np.repeat(shown, 5), CELL, 1.0), write(model, np.repeat(shown, 3), CELL, 0.5)
            sheet([(glyphs[g], [cells[i][2] for i in mine[label[mine] == g][:3]] + [strokes(row) for row in warm[5 * j:5 * j + 5]] + [strokes(row) for row in calm[3 * j:3 * j + 3]]) for j, g in enumerate(shown)], os.path.join(KEPT, f"{name}.svg"))
    truth, guess = np.array(truth), np.array(guess)
    print(f"{100 * (truth == guess).mean():.1f} % of {len(truth)} kept cells read right by the writers")
    print("mixed:", pairs(truth, guess, glyphs))
