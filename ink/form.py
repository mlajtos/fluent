# A writer of one glyph's form. A stroke is two curves, x and y along its ink, and each curve
# is a sum of cosines: the first says where the stroke is, the next how it leans, the later
# ones ever finer bends. The form is those sums' amounts, a pair for x and y at a time, from
# the coarsest on, each rounded to STEP px. The writer says how likely each amount is after
# the amounts before it, for a glyph of a given name. So it writes, and it reads: ink is the
# glyph under whose name its form is likeliest.
#   python3 ink/form.py [steps=4000] [fold ...] [seed=0] [wide=64] [deep=3] [curve=500] [varied=1] [soft=1] [step=1] [name=form]
# A fold's writer learns eight of a glyph's ten cells and reads the two kept from it; it is
# kept as paper/ink/<name>-<fold>.safetensors, and a sheet of what it writes beside it.
import math, os, sys
import numpy as np
import mlx.core as mx
import mlx.nn as nn
from mlx.utils import tree_flatten
from ink import FOLDS, KEPT, STROKES, Block, Reader, descend, pairs, said, vary, written
from line import EACH, M, inked
from write import CELL, seen, sheet

# An amount is a whole number of STEP px, at most REACH of them either way. A glyph is told in
# at most LONGEST words.
STEP, REACH, LONGEST = 1.0, 64, 192
# what a word is: an amount, the end of a stroke with another to come, or the end of the glyph
AMOUNT, NEXT, END = 0, 1, 2

# a stroke's M points of its M amounts, and its amounts of its points
POINTS = np.cos(math.pi * np.arange(M)[None] * (np.arange(M)[:, None] + 0.5) / M)
AMOUNTS = np.linalg.inv(POINTS)

# The forms of glyphs – ink [STROKES, M, 2], `count` strokes of it – and how many words each
# has. A word: what it is, its amount, and its slot – which pair of its stroke, x or y, which
# stroke. A stroke is told as far as it has an amount that is not 0; x is from the middle of
# the first stroke, y from the baseline.
def forms(ink, count):
    ink = ink - np.stack([ink[:, 0, :, 0].mean(1), np.zeros(len(ink))], 1)[:, None, None]
    amounts = np.clip(np.round(np.einsum("ki,bsid->bskd", AMOUNTS, ink) / STEP), -REACH, REACH).astype(int)
    rows = []
    for one, n in zip(amounts, count):
        row = []
        for s in range(n):
            some = np.flatnonzero(np.abs(one[s]).sum(1))
            told = some[-1] + 1 if len(some) else 1
            row += [(AMOUNT, one[s, k, a], k, a, s) for k in range(told) for a in (0, 1)] + [(NEXT, 0, told, 0, s)]
        row = row[:LONGEST]
        rows.append(row[:-1] + [(END, 0, *row[-1][2:])])
    padded = np.zeros((len(rows), LONGEST, 5), np.int32); padded[:, :, 0] = END
    for b, row in enumerate(rows): padded[b, :len(row)] = row
    return padded, np.array([len(row) for row in rows])

# the strokes that a form draws
def strokes(told):
    made, one = [], np.zeros((M, 2))
    for kind, amount, pair, axis, _ in told:
        if kind == AMOUNT: one[pair, axis] = amount * STEP; continue
        made.append((POINTS @ one).astype(np.float32)); one = np.zeros((M, 2))
        if kind == END: return made
    return made + [(POINTS @ one).astype(np.float32)]

# what an amount of px is to the writer: itself, and where it is in waves of these lengths
WAVES = np.float32([4, 8, 16, 32, 64, 128])
def felt(px):
    turn = 2 * math.pi * px[..., None] / mx.array(WAVES)
    return mx.concatenate([px[..., None] / 32, mx.sin(turn), mx.cos(turn)], -1)

# How likely the writer finds each amount: a mix of MIX bells, each with a weight, a middle
# and a spread in px. `mix` is the three of what the writer's head says.
MIX = 5
def mix(said):
    weight, middle, spread = mx.split(said, 3, -1)
    return nn.log_softmax(weight, -1), middle * 16, mx.exp(mx.clip(spread, -3, 5))

# how likely, in nats, an amount of px is under a mix: what of each bell falls within STEP of it
def within(said, px):
    weight, middle, spread = mix(said)
    high, low, mid = ((px[..., None] + d - middle) / spread for d in (STEP / 2, -STEP / 2, 0))
    inside = mx.sigmoid(high) - mx.sigmoid(low)
    # far out in a bell's tail the difference is lost to rounding: the bell's height there, times STEP
    far = mid - mx.log(spread) - 2 * nn.softplus(mid) + math.log(STEP)
    return mx.logsumexp(weight + mx.where(inside > 1e-5, mx.log(mx.maximum(inside, 1e-12)), far), -1)

# A transformer over the words so far, each with the next word's slot, the glyph's name and
# where it is written added: of every word, how likely each next one is. Its attention looks
# only back. A `soft` writer takes an amount as a number and says a mix of bells for the next;
# one that is not takes every amount as a word of its own.
class FormWriter(nn.Module):
    def __init__(self, classes, d=64, layers=3, heads=4, drop=0.1, soft=True):
        super().__init__()
        self.soft = soft
        self.kind, self.glyph, self.where = nn.Embedding(4, d), nn.Embedding(classes, d), nn.Embedding(2, d)
        self.amount = nn.Linear(2 * len(WAVES) + 1, d) if soft else nn.Embedding(2 * REACH + 1, d)
        self.pair, self.axis, self.stroke = nn.Embedding(M + 1, d), nn.Embedding(2, d), nn.Embedding(STROKES, d)
        self.blocks = [Block(d, heads, drop) for _ in range(layers)]
        self.norm, self.head = nn.LayerNorm(d), nn.Linear(d, 3 + (3 * MIX if soft else 2 * REACH + 1))
    def __call__(self, told, glyph, where):
        kind = mx.concatenate([mx.full((told.shape[0], 1), 3, mx.int32), told[:, :-1, 0]], 1)
        amount = mx.concatenate([mx.zeros((told.shape[0], 1), mx.int32), told[:, :-1, 1]], 1)
        before = self.amount(felt(amount * STEP)) if self.soft else self.amount(amount + REACH)
        x = self.kind(kind) + before * (kind == AMOUNT)[..., None] + self.pair(told[..., 2]) + self.axis(told[..., 3]) + self.stroke(told[..., 4]) + (self.glyph(glyph) + self.where(where))[:, None]
        back = nn.MultiHeadAttention.create_additive_causal_mask(told.shape[1])
        for block in self.blocks: x = block(x, back)
        return self.head(self.norm(x))

# how unlikely, in nats, each glyph's form is under its name
def unlikely(model, told, long, glyph, where):
    said = model(told, glyph, where)
    amount = -within(said[..., 3:], told[..., 1] * STEP) if model.soft else nn.losses.cross_entropy(said[..., 3:], told[..., 1] + REACH, reduction="none")
    each = nn.losses.cross_entropy(said[..., :3], told[..., 0], reduction="none") + amount * (told[..., 0] == AMOUNT)
    return (each * (mx.arange(told.shape[1]) < long[:, None])).sum(1)

# how likely the form of each ink, written `where`, is under every name: [inks, classes] nats
def likely(model, told, long, classes, where, at_once=8):
    order, out = np.argsort(long), np.zeros((len(told), classes), np.float32)
    names = mx.arange(classes)
    for i in range(0, len(order), at_once):
        pick = order[i:i + at_once]
        some = mx.array(np.repeat(told[pick, :long[pick].max()], classes, 0))
        out[pick] = -np.array(unlikely(model, some, mx.array(np.repeat(long[pick], classes)), mx.concatenate([names] * len(pick)), mx.full(len(some), where, mx.int32))).reshape(len(pick), classes)
    return out

# Glyphs of the given names, told word by word as written `where`: the forms. A `warmth` under
# 1 keeps the writer to its likelier words, and its amounts nearer the middles of their bells.
def write(model, glyph, where, rng, warmth=1.0):
    chosen = lambda logs: (logs / warmth + rng.gumbel(size=logs.shape)).argmax(1)
    glyph, where, each = mx.array(glyph), mx.full(len(glyph), where, mx.int32), np.arange(len(glyph))
    told, slot, ended = np.zeros((len(glyph), 0, 5), np.int32), np.zeros((len(glyph), 3), np.int32), np.zeros(len(glyph), bool)
    while told.shape[1] < LONGEST and not ended.all():
        ahead = np.concatenate([told, np.concatenate([np.zeros((len(slot), 2), np.int32), slot], 1)[:, None]], 1)
        said = model(mx.array(ahead), glyph, where)[:, -1]
        pair, axis, stroke = slot.T
        # a stroke has a first pair, a pair has its y, and a stroke of M pairs is told
        kinds = np.array(said[:, :3]).astype(np.float64)
        kinds[(axis == 1) | (pair == 0), 1:] = -np.inf
        kinds[pair == M, AMOUNT] = -np.inf
        kind = chosen(kinds)
        kind[(kind == NEXT) & (stroke == STROKES - 1)] = END
        kind[ended] = END
        if model.soft:
            weight, middle, spread = (np.array(part).astype(np.float64) for part in mix(said[:, 3:]))
            bell, odds = chosen(weight), rng.uniform(1e-6, 1 - 1e-6, len(each))
            amount = np.round((middle[each, bell] + warmth * spread[each, bell] * np.log(odds / (1 - odds))) / STEP)
        else: amount = chosen(np.array(said[:, 3:]).astype(np.float64)) - REACH
        amount = np.clip(amount, -REACH, REACH).astype(np.int32) * (kind == AMOUNT)
        told = np.concatenate([told, np.stack([kind, amount, pair, axis, stroke], 1)[:, None]], 1).astype(np.int32)
        slot = np.stack([(pair + axis) * (kind == AMOUNT), (1 - axis) * (kind == AMOUNT), stroke + (kind == NEXT)], 1).astype(np.int32)
        ended |= kind == END
    return told

# A writer that has learned a hand: its glyphs' ink, how many strokes each has, which glyph
# each is and where it was written. With `varied`, every glyph it is shown is first bent,
# leaned and stretched a little (ink.py's `vary`).
def taught(ink, count, label, where, classes, steps, rng, wide=64, deep=3, varied=True, soft=True, watch=None, every=0, batch=128):
    model = FormWriter(classes, d=wide, layers=deep, soft=soft)
    def some():
        pick = rng.integers(0, len(ink), batch)
        one = vary(ink[pick].reshape(batch, -1, 2), np.broadcast_to(EACH, (batch, len(EACH))), np.ones(batch, np.float32), rng).reshape(batch, STROKES, M, 2) if varied else ink[pick]
        told, n = forms(one, count[pick])
        return mx.array(told[:, :n.max()]), mx.array(n), mx.array(label[pick]), mx.array(where[pick])
    loss = lambda model, told, n, g, w: unlikely(model, told, n, g, w).sum() / n.sum()
    descend(model, loss, some, steps, 2e-3, say=every or 500, watch=(lambda step: watch(model, step)) if watch else None, every=every)
    return model

if __name__ == "__main__":
    named = dict(word.split("=") for word in sys.argv[1:] if "=" in word)
    rest = [int(word) for word in sys.argv[1:] if "=" not in word]
    steps, folds = rest[0] if rest else 4000, rest[1:] or range(FOLDS)
    seed, wide, deep, curve = (int(named.get(key, default)) for key, default in [("seed", 0), ("wide", 64), ("deep", 3), ("curve", 500)])
    varied, soft, name = named.get("varied", "1") == "1", named.get("soft", "1") == "1", named.get("name", "form")
    STEP = float(named.get("step", 1))
    cells, glyphs = written()
    ink, count = map(np.array, zip(*[inked(one) for _, _, one in cells]))
    label, cell = np.array([glyph for glyph, _, _ in cells]), np.array([c for _, c, _ in cells])
    told, long = forms(ink, count)
    back = [strokes(row) for row in told]
    off = np.mean([np.hypot(*(a - b).T).mean() for one, n, drawn in zip(ink, count, back) if len(drawn) == n for a, b in zip(drawn, one - np.float32([one[0, :, 0].mean(), 0]))])
    print(f"{len(cells)} cells of {len(glyphs)} glyphs as forms, an amount a whole number of {STEP} px: {long.mean():.0f} words a glyph, {(long == LONGEST).sum()} cut short; drawn back, {sum(len(d) == n for d, n in zip(back, count))} have their strokes and a point is {off:.2f} px off", flush=True)
    judge = Reader(len(glyphs)); judge.load_weights(os.path.join(KEPT, "reader.safetensors")); judge.eval()
    truth, guess = [], []
    for k in folds:
        mx.random.seed(seed); rng = np.random.default_rng(k + FOLDS * seed)
        held = cell % FOLDS == k
        mine, kept = np.flatnonzero(~held), np.flatnonzero(held)
        def watch(model, step):
            scores = likely(model, told[kept], long[kept], len(glyphs), CELL)
            own = scores[np.arange(len(kept)), label[kept]]
            print(f"  step {step}: the kept cells are {-own.mean():.1f} nats a glyph ({-own.sum() / long[kept].sum():.3f} a word), {100 * (scores.argmax(1) == label[kept]).mean():.1f} % read right", flush=True)
        model = taught(ink[mine], count[mine], label[mine], np.zeros(len(mine), np.int32), len(glyphs), steps, rng, wide, deep, varied, soft, watch if curve else None, curve)
        if k == folds[0]: print(f"a writer {wide} wide, {deep} deep: {sum(v.size for _, v in tree_flatten(model.parameters()))} weights", flush=True)
        model.save_weights(os.path.join(KEPT, f"{name}-{k}.safetensors"))
        scores = likely(model, told[kept], long[kept], len(glyphs), CELL)
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
            P, S = seen([strokes(row) for row in write(model, every, CELL, rng, warmth)])
            named_right = said(judge, P, S, np.ones(len(P), np.float32)).argmax(1) == every
            print(f"  written at warmth {warmth}: {100 * named_right.mean():.1f} % read as their name by the reader of cells; a point is {np.median(near(P, every)):.1f} px from the nearest cell learned (the hand's kept cells: {np.median(hand):.1f} px)", flush=True)
        if k == folds[0]:
            shown = [glyphs.index(g) for g in "aG0l⌊⍴∇#⊗θ" if g in glyphs]
            warm, calm = write(model, np.repeat(shown, 5), CELL, rng, 1.0), write(model, np.repeat(shown, 3), CELL, rng, 0.5)
            sheet([(glyphs[g], [cells[i][2] for i in mine[label[mine] == g][:3]] + [strokes(row) for row in warm[5 * j:5 * j + 5]] + [strokes(row) for row in calm[3 * j:3 * j + 3]]) for j, g in enumerate(shown)], os.path.join(KEPT, f"{name}.svg"))
    truth, guess = np.array(truth), np.array(guess)
    print(f"{100 * (truth == guess).mean():.1f} % of {len(truth)} kept cells read right by the writers")
    print("mixed:", pairs(truth, guess, glyphs))
