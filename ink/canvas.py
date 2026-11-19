# A canvas written from its texts, in the hand's own ink: every glyph is one the pen wrote
# in a dictated line – as the cuts page.py kept have it (line.py, CUTS), of the lines their
# reader read as their text – where it stood on that line's baseline, with its pressure and
# its time; a glyph no such line has is one of its cells, as large as the hand of lines.
# Two glyphs stand as far apart as the hand leaves that pair – the middle of its gaps – or,
# a pair it wrote fewer than LEAST times, as it leaves glyphs at such a place, a space or
# none. A line is written again, of other ink, until the canvas's readers (serve.py, behind
# the server's /reading) read it as its text, or as that but for spaces the parser makes
# nothing of: this hand leaves little room before a } and the readers see none.
#   python3 ink/canvas.py texts [name]
# `texts` is the canvas as a file: a row is a line, its text as many units from the left as
# INDENT for each space before it, and every row – an empty one too – is ROW units lower
# than the one before. It is kept as paper/canvases/<name>.json, the next number with no name.
import json, os, ssl, sys, time, urllib.request
import numpy as np
from ink import COPYBOOK, HERE
from line import CHANGED, CUTS, SAME, edits, parsed, spaced

CANVASES = os.path.join(HERE, "../paper/canvases")
INDENT, ROW = 50, 64
# a line's ink starts this far after the start of its baseline, as where a pen starts one (canvas.tsx, START)
BEFORE = 24
LEAST, TRIES = 5, 20

across = lambda strokes: [p[0] for stroke in strokes for p in stroke["points"]]
high = lambda strokes: np.ptp([p[1] for stroke in strokes for p in stroke["points"]])

# Of each glyph the ink the hand has of it, and how far apart glyphs stand: `pairs` of two
# glyphs with a space between them or none, `gaps` at any such place
def hand():
    cuts, lines, cells, pairs, gaps = json.load(open(CUTS)), {}, {}, {}, {False: [], True: []}
    for file in sorted(os.listdir(COPYBOOK)):
        if not file.endswith(".json"): continue
        kept = json.load(open(os.path.join(COPYBOOK, file)))
        samples = [sample["strokes"] for sample in kept["samples"] if sample and sample["strokes"]]
        if not file.startswith("line-"): cells[kept["glyph"]] = samples; continue
        text = "".join(SAME.get(c, c) for c in kept["glyph"]); cut = cuts.get(text)
        if not samples or not cut or not cut["read"] or cut["ink"] != [len(stroke["points"]) for stroke in samples[0]]: continue
        last = None
        for g, (glyph, space) in enumerate(zip(text.replace(" ", ""), spaced(text))):
            one = [stroke for stroke, of in zip(samples[0], cut["of"]) if of == g]
            if last:
                gap = min(across(one)) - max(across(last[1]))
                pairs.setdefault((last[0], glyph, space), []).append(gap); gaps[space].append(gap)
            last = (glyph, one)
            # a glyph the hand writes another way now is taken as it writes it now
            if samples[0][0]["at"] >= CHANGED.get(glyph, 0): lines.setdefault(glyph, []).append(one)
    both = [glyph for glyph in lines if cells.get(glyph) and np.median([high(one) for one in cells[glyph]]) >= 12]
    larger = float(np.median([np.median([high(one) for one in lines[glyph]]) / np.median([high(one) for one in cells[glyph]]) for glyph in both]))
    for glyph, samples in cells.items():
        if glyph not in lines and samples[0][0]["at"] >= CHANGED.get(glyph, 0):
            lines[glyph] = [[{**stroke, "points": [[p[0] * larger, p[1] * larger, *p[2:]] for p in stroke["points"]]} for stroke in one] for one in samples]
    return lines, pairs, gaps

# a text as ink: its strokes, the points' x from the start of the line's baseline, each begun at `at` or after
def written(text, ink, pairs, gaps, rng, at):
    strokes, last, right = [], None, BEFORE
    for glyph, space in zip(text.replace(" ", ""), spaced(text)):
        if glyph not in ink: raise SystemExit(f"{text}: the hand has no {glyph}")
        one = ink[glyph][rng.integers(len(ink[glyph]))]
        apart = pairs.get((last, glyph, space), [])
        left = right + (float(np.median(apart if len(apart) >= LEAST else gaps[space])) if last else 0)
        by = left - min(across(one))
        for stroke in one:
            strokes.append({"at": at, "points": [[round(p[0] + by, 1), round(p[1], 1), *p[2:]] for p in stroke["points"]]})
            at += int(stroke["points"][-1][3]) + 150
        last, right = glyph, max(across(one)) + by
    return strokes

def read(strokes):
    asked = urllib.request.Request("https://localhost:3210/reading", json.dumps([[p[:2] for p in stroke["points"]] for stroke in strokes]).encode(), method="POST")
    return json.load(urllib.request.urlopen(asked, context=ssl._create_unverified_context()))["text"]

# whether a line read as `said` is its text: but for spaces, and those say nothing to the parser
def right(text, said):
    if said == text: return True
    if said.replace(" ", "") != text.replace(" ", ""): return False
    mine, theirs = parsed([text, said])
    return mine["tree"] == theirs["tree"]

if __name__ == "__main__":
    rows = open(sys.argv[1]).read().split("\n")
    name = sys.argv[2] if len(sys.argv) > 2 else str(1 + max([int(file[:-5]) for file in os.listdir(CANVASES) if file[:-5].isdigit()], default=0))
    ink, pairs, gaps = hand()
    rng, at, lines = np.random.default_rng(0), int(time.time() * 1000) - 3_600_000, []
    for row, text in enumerate(rows):
        if not text.strip(): continue
        tried = []
        for _ in range(TRIES):
            strokes = written(text.strip(), ink, pairs, gaps, rng, at)
            said = read(strokes)
            tried.append((sum(map(len, edits(text.strip(), said))) if not right(text.strip(), said) else 0, len(tried), said, strokes))
            if not tried[-1][0]: break
        wrong, tries, said, strokes = min(tried)
        print(f"{'read' if said == text.strip() else ('read as ' if not wrong else 'MISREAD AS ') + json.dumps(said, ensure_ascii=False)}  {text.strip()}  ({len(tried)} written)")
        lines.append({"id": row + 1, "x": float(INDENT * (len(text) - len(text.lstrip()))), "y": float(ROW * row), "k": 1, "strokes": strokes})
        at = strokes[-1]["at"] + 2000
    json.dump({"lines": lines, "camera": {"x": 60, "y": 110, "k": 0.8}}, open(os.path.join(CANVASES, f"{name}.json"), "w"), ensure_ascii=False, separators=(",", ":"))
    print(f"{len(lines)} lines, canvas {name}")
