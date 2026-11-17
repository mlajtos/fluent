# The middle of a hand: of the cells of one glyph, the ink that is the mean of them.
import json, os, sys, collections, itertools
import numpy as np
BOOK = os.path.join(os.path.dirname(os.path.abspath(__file__)), "../paper/copybook")
KEPT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "../paper/ink")
M = 48

def even(points, n=M):
    xy = points[:, :2]
    along = np.concatenate([[0], np.cumsum(np.hypot(*np.diff(xy, axis=0).T))])
    at = np.linspace(0, along[-1], n) if along[-1] > 0 else np.zeros(n)
    return np.stack([np.interp(at, along, points[:, k]) for k in range(points.shape[1])], 1)

def cells():
    book = {}
    for file in sorted(os.listdir(BOOK)):
        if not file.endswith(".json") or file.startswith("line-"): continue
        kept = json.load(open(os.path.join(BOOK, file)))
        rows = []
        for cell, sample in enumerate(kept["samples"][:10]):
            if not sample or not sample["strokes"]: continue
            strokes = [np.array([p[:3] for p in s["points"]], np.float64) for s in sample["strokes"]]
            # where the ink is across the cell says nothing: its middle is 0
            box = np.concatenate(strokes)[:, 0]
            mid = (box.min() + box.max()) / 2
            for s in strokes: s[:, 0] -= mid
            rows.append((cell, strokes))
        book[kept["glyph"]] = rows
    return book

# the steps of a that go with each step of b
def warp(a, b):
    far = ((a[:, None, :2] - b[None, :, :2]) ** 2).sum(2)
    n, m = far.shape
    cost = np.full((n + 1, m + 1), np.inf); cost[0, 0] = 0
    for i in range(n):
        for j in range(m):
            cost[i + 1, j + 1] = far[i, j] + min(cost[i, j], cost[i, j + 1], cost[i + 1, j])
    i, j, path = n, m, []
    while i > 0 and j > 0:
        path.append((i - 1, j - 1))
        step = np.argmin([cost[i - 1, j - 1], cost[i - 1, j], cost[i, j - 1]])
        i, j = (i - 1, j - 1) if step == 0 else (i - 1, j) if step == 1 else (i, j - 1)
    return path

# Of each stroke of `mean`, which of a sample's strokes – each M points – goes with it, and
# whether the other way round: the pairing that brings the sample nearest the mean
def paired(strokes, mean):
    far = lambda a, b: ((a[:, :2] - b[:, :2]) ** 2).sum()
    way = [[min((far(s, m), 0), (far(s[::-1], m), 1)) for m in mean] for s in strokes]
    order = min(itertools.permutations(range(len(mean))), key=lambda o: sum(way[i][k][0] for k, i in enumerate(o))) if len(mean) <= 6 else range(len(mean))
    return [(i, way[i][k][1]) for k, i in enumerate(order)]

# a sample's strokes in the order and each the way round that brings them nearest `mean`'s
def laid(strokes, mean): return [strokes[i][::-1] if back else strokes[i] for i, back in paired(strokes, mean)]

# The mean of the samples that have as many strokes as most of them, each laid on the mean
# so far. A point is the mean of the points as far along their strokes (`step` False), or
# of the points in step with it (DTW).
def middle(samples, step=False, turns=3):
    count = collections.Counter(len(s) for s in samples).most_common(1)[0][0]
    some = [[even(stroke) for stroke in s] for s in samples if len(s) == count]
    flat = np.array([np.concatenate(s)[:, :2].ravel() for s in some])
    mean = [stroke.copy() for stroke in some[int(((flat[:, None] - flat[None]) ** 2).sum(2).sum(1).argmin())]]
    for _ in range(turns):
        rows = [laid(s, mean) for s in some]
        for k in range(count):
            if not step: mean[k] = np.mean([s[k] for s in rows], 0); continue
            total, times = np.zeros_like(mean[k]), np.zeros(M)
            for s in rows:
                for i, j in warp(s[k], mean[k]): total[j] += s[k][i]; times[j] += 1
            mean[k] = total / times[:, None]
    return mean, len(some)

def flat(strokes, n=64):
    lengths = [float(np.hypot(*np.diff(s[:, :2], axis=0).T).sum()) for s in strokes]
    total = sum(lengths) or 1.0
    share = [max(1, round(n * length / total)) for length in lengths]
    while sum(share) > n: share[int(np.argmax(share))] -= 1
    while sum(share) < n: share[int(np.argmax(lengths))] += 1
    return np.concatenate([even(s, c)[:, :2] if c > 1 else s[len(s) // 2:len(s) // 2 + 1, :2] for s, c in zip(strokes, share)]).ravel()

# how much a pen turns along its ink, in turns of the whole circle: a tremble adds to it
def turning(strokes):
    total = 0
    for s in strokes:
        d = np.diff(even(s)[:, :2], axis=0); angle = np.unwrap(np.arctan2(d[:, 1], d[:, 0]))
        total += np.abs(np.diff(angle)).sum()
    return total / (2 * np.pi)

if __name__ == "__main__":
    book = cells()
    glyphs = sorted(book)
    # Is the mean easier to read than a cell? A reader that knows half of the cells names
    # the cells of the other half, and the mean of that other half.
    right = {"cell": 0, "mean": 0}; asked = {"cell": 0, "mean": 0}
    for half in (0, 1):
        known = [(g, flat(s)) for g in glyphs for cell, s in book[g] if (cell < 5) != (half == 0)]
        X, y = np.array([x for _, x in known]), [g for g, _ in known]
        name = lambda x: y[int(((X - x) ** 2).sum(1).argmin())]
        for g in glyphs:
            rest = [s for cell, s in book[g] if (cell < 5) == (half == 0)]
            if not rest: continue
            for s in rest: right["cell"] += name(flat(s)) == g; asked["cell"] += 1
            right["mean"] += name(flat(middle(rest)[0])) == g; asked["mean"] += 1
    for k in right: print(f"nearest neighbour names a {k} right: {right[k]} of {asked[k]}, {100 * right[k] / asked[k]:.1f} %")
    for step in (True, False):
        means = {g: middle([s for _, s in book[g]], step) for g in glyphs}
        ratio = [turning(means[g][0]) / np.median([turning(s) for _, s in book[g] if len(s) == len(means[g][0])]) for g in glyphs if turning(means[g][0]) > 0.2]
        print(f"in step {step}: the mean turns {100 * np.median(ratio):.0f} % as much as the median cell with as many strokes")
    print("glyphs whose mean is of fewer than 7 cells:", " ".join(f"{g}:{n}" for g, (_, n) in means.items() if n < 7))
    json.dump({g: [s.round(2).tolist() for s in m] for g, (m, _) in means.items()}, open(os.path.join(KEPT, "means.json"), "w"), ensure_ascii=False)
    show = "a g k x 2 5 8 0 { } # ξ ζ ψ θ λ Σ ⍴ ∇ √ ⌊ ⊢ ⍣ ⊗ ← ≠ R f".split()
    W, H, per = 80, 140, 2
    out = []
    def draw(strokes, x0, y0, color, scale):
        for s in strokes:
            p = s[:, :2] * scale + [x0, y0]
            w = (0.6 + 2.2 * s[:, 2]) * scale
            for a, b, wa, wb in zip(p[:-1], p[1:], w[:-1], w[1:]):
                out.append(f'<line x1="{a[0]:.1f}" y1="{a[1]:.1f}" x2="{b[0]:.1f}" y2="{b[1]:.1f}" stroke="{color}" stroke-width="{(wa + wb) / 2:.2f}" stroke-linecap="round"/>')
    rows = (len(show) + 1) // per
    for r, g in enumerate(show):
        left, base = (r % per) * 13.4 * W + 20, (r // per) * H + 90
        out.append(f'<line x1="{left}" y1="{base}" x2="{left + 12.6 * W}" y2="{base}" stroke="#d4d4ee" stroke-width="0.6"/>')
        for c, (cell, s) in enumerate(book[g]): draw(s, left + c * W + W / 2, base, "#9a9a9a", 0.8)
        draw(means[g][0], left + 11.3 * W, base, "#101010", 1.5)
    width, height = per * 13.4 * W + 40, rows * H + 60
    open(os.path.join(KEPT, "mean.svg"), "w").write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {height}" width="{width}" height="{height}"><rect width="100%" height="100%" fill="white"/>' + "".join(out) + "</svg>")
    print("drawn")
