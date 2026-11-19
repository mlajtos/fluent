# The readers of lines, kept running for the canvas (server.ts,
# /reading). A line of JSON comes in – one written line's strokes, each its points, x and y
# from the baseline – and a line of JSON goes out: the text; `of` each stroke which glyph of
# the text it is of; how `sure` the readers are of each glyph, 0 to 1; and where each stroke
# goes `to` in the hand's form (hand.py) – the
# points of the form's stroke, from the end the pen began at – or null for a stroke of a
# glyph the pen wrote in another number of strokes than its form has. The readers are those
# that learned from every line, which page.py kept as canvas-<number> in paper/ink
# (`all=4 name=canvas`), and they read as one.
import json, os, re, sys
import numpy as np
from ink import KEPT
from hand import formed, kept_forms
from page import kept_as, read

kept = [kept_as(file[:-12]) for file in sorted(os.listdir(KEPT)) if re.fullmatch(r"canvas-\d+\.safetensors", file)]
readers, glyphs = [reader for reader, _ in kept], kept[0][1]
forms = kept_forms()
print(f"{len(readers)} readers of the page read", file=sys.stderr, flush=True)

for asked in sys.stdin:
    strokes = [np.array(stroke, np.float32)[:, :2] for stroke in json.loads(asked) if stroke]
    text, of, sure = read(readers, strokes, glyphs) if strokes else ("", [], [])
    to = [None] * len(strokes)
    for _, piece, form, pairs in formed(text, of, strokes, forms)[0]:
        for stroke, (i, back) in zip(form, pairs or []): to[piece[i]] = (stroke[::-1] if back else stroke)[:, :2].round(1).tolist()
    print(json.dumps({"text": text, "of": of, "sure": [round(s, 3) for s in sure], "to": to}, ensure_ascii=False), flush=True)
