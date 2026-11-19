# A line in the hand's form. A reader that has not learned from the line (page.py) says
# which of its strokes are one glyph and names the glyphs, with no text. The form of each
# glyph is the mean of that glyph's cells (mean.py), where the pen wrote the glyph: as large
# as the hand of this line, and on one baseline. paper/ink/hand.json has both inks of every
# line, for the page ?hand (hand.tsx); the readers of the canvas give a line's form
# with its text (serve.py).
import json, os, sys
import numpy as np
import mlx.core as mx
from ink import COPYBOOK, KEPT, written
from line import SAME, parsed, part
from mean import cells, even, middle, paired
from page import LONG, kept_as, read

# the dictated lines in the order line.py has them: of each its text, when the pen began
# it, and its strokes as points of x, y from the baseline, and pressure
def lines():
    found = []
    for file in sorted(os.listdir(COPYBOOK)):
        if not file.startswith("line-"): continue
        kept = json.load(open(os.path.join(COPYBOOK, file)))
        if kept["samples"] and kept["samples"][0]:
            strokes = kept["samples"][0]["strokes"]
            found.append((kept["glyph"], strokes[0]["at"] if strokes else 0, [np.array([p[:3] for p in s["points"]], np.float64) for s in strokes], os.path.getmtime(os.path.join(COPYBOOK, file))))
    return found

def box(strokes):
    xy = np.concatenate(strokes)[:, :2]
    return xy.min(0), xy.max(0)
size = lambda strokes: float(np.subtract(*box(strokes)[::-1]).max())
middle_of = lambda strokes: np.add(*box(strokes)) / 2
inked = lambda strokes: [np.column_stack([s[:, :2].round(1), s[:, 2].round(2)]).tolist() for s in strokes]

# the form of each glyph: the mean of its cells. A glyph with no cell written has none.
def shapes(glyphs):
    book = cells()
    return {glyph: middle([strokes for _, strokes in book[glyph]])[0] for glyph in glyphs if book.get(glyph)}

# The forms as they are kept for the readers of lines – serve.py, and hand.fl in the page
# that reads by itself – in paper/ink/forms.safetensors: `glyphs`, the code point of each
# glyph that has a form; `strokes`, how many strokes its form has; and `forms`, of each
# stroke its points' x and y – a form of fewer strokes than the most is filled up with its
# first stroke, which leaves its box as it is.
FORMS = os.path.join(KEPT, "forms.safetensors")
def keep_forms(forms):
    most = max(map(len, forms.values()))
    mx.save_safetensors(FORMS, {
        "glyphs": mx.array([ord(glyph) for glyph in forms]),
        "strokes": mx.array([len(form) for form in forms.values()]),
        "forms": mx.array(np.array([[stroke[:, :2] for stroke in form + form[:1] * (most - len(form))] for form in forms.values()], np.float32)),
    })
def kept_forms():
    kept = mx.load(FORMS)
    return {chr(code): [np.array(stroke, np.float64) for stroke in form[:count]] for code, count, form in zip(kept["glyphs"].tolist(), kept["strokes"].tolist(), kept["forms"])}

# A line as its reader read it – `said`, and `of` each stroke which glyph of that – in the
# hand's form. Of each glyph: its name, which strokes of the line it is, its form where the
# pen wrote it, and – when the pen wrote it in as many strokes as the form has – of each
# stroke of the form which of the glyph's strokes goes to it, and whether the other way
# round. A glyph that has no form has no strokes in it. With them, how large the hand of
# this line is beside the cells'.
def formed(said, of, strokes, forms):
    names = said.replace(" ", "")
    pieces = [[s for s, g in enumerate(of) if g == at] for at in range(len(names))]
    ink = lambda piece: [strokes[s] for s in piece]
    # the dots and the commas say nothing of how large the hand is
    known = [(glyph, piece) for glyph, piece in zip(names, pieces) if glyph in forms]
    larger = [size(ink(piece)) / size(forms[glyph]) for glyph, piece in known if size(forms[glyph]) >= 12]
    scale = float(np.median(larger)) if larger else 1.0
    # how far under the ruled baseline the line runs
    base = float(np.median([middle_of(ink(piece))[1] - scale * middle_of(forms[glyph])[1] for glyph, piece in known])) if known else 0.0
    found = []
    for glyph, piece in zip(names, pieces):
        if glyph not in forms: found.append((glyph, piece, [], None)); continue
        form = [np.column_stack([(s[:, 0] - middle_of(forms[glyph])[0]) * scale + middle_of(ink(piece))[0], s[:, 1] * scale + base, s[:, 2:]]) for s in forms[glyph]]
        found.append((glyph, piece, form, paired([even(strokes[s]) for s in piece], form) if len(piece) == len(form) else None))
    return found, scale

if __name__ == "__main__":
    # `forms` alone keeps the forms and no more
    if sys.argv[1:] == ["forms"]: keep_forms(shapes(written()[1])); sys.exit()
    pages = [kept_as(f"page-{k}") for k in range(4)]
    readers, glyphs = [reader for reader, _ in pages], pages[0][1]
    forms = shapes(glyphs)
    keep_forms(forms)
    learned = os.path.getmtime(os.path.join(KEPT, "page-0.safetensors"))
    hand, moved, sizes, morphed, faded = [], [], [], 0, 0
    for l, (text, at, strokes, kept) in enumerate(lines()):
        # the reader of the line's quarter had no lesson from it
        said, of, _ = read(readers[part("".join(SAME.get(c, c) for c in text))], [s[:, :2].astype(np.float32) for s in strokes], glyphs)
        if len(strokes) > LONG: print(f"{text}: the reader sees {LONG} of its {len(strokes)} strokes")
        glyphs_found, scale = formed(said, of, strokes, forms)
        sizes.append(scale)
        row = []
        for glyph, piece, form, pairs in glyphs_found:
            # ink of as many strokes as the form goes to it point by point, and other ink fades to it
            ink = [strokes[s] for s in piece]
            if pairs:
                ink = [even(ink[i])[::-1] if back else even(ink[i]) for i, back in pairs]
                moved.append(float(np.mean([np.hypot(*(a[:, :2] - b[:, :2]).T).mean() for a, b in zip(ink, form)])))
            morphed += bool(pairs); faded += not pairs
            row.append({"glyph": glyph, "morph": bool(pairs), "written": inked(ink), "form": inked(form)})
        hand.append({"text": text, "read": said, "at": at, "late": kept > learned, "glyphs": row})
    trees = [tree["tree"] for tree in parsed(["".join(SAME.get(c, c) for c in line["text"]) for line in hand] + [line["read"] for line in hand])]
    for i, line in enumerate(hand): line["right"] = trees[i] == trees[len(hand) + i]
    late = sum(line["late"] for line in hand)
    print(f"{len(hand)} lines, {sum(line['right'] for line in hand)} read to their text's tree" + (f"; {late} were written after the readers learned, and which reader has not seen a line is no longer known" if late else ""))
    print(f"{morphed} glyphs go to their form point by point, {faded} have another number of strokes and fade to it")
    print(f"a line's hand is {np.median(sizes):.2f} of the cells' hand (5 %, 95 %: {np.percentile(sizes, 5):.2f}, {np.percentile(sizes, 95):.2f}); a point goes {np.median(moved):.1f} px to its form (median of glyphs, 95 %: {np.percentile(moved, 95):.1f})")
    hand.sort(key=lambda line: line["at"])
    json.dump({"lines": [{k: line[k] for k in ("text", "read", "right", "glyphs")} for line in hand]}, open(os.path.join(KEPT, "hand.json"), "w"), ensure_ascii=False, separators=(",", ":"))
