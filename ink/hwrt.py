# The HWRT symbols (zenodo.org/records/50022, ODbL: 369 symbols by many hands, most of them
# Detexify's) as arrays the reader trains on. The csv comes on stdin:
#   tar -xOf paper/ink/hwrt.tar train-data.csv | python3 ink/hwrt.py paper/ink/hwrt-train.npz
import json, sys
import numpy as np
from ink import N, resample

decode = json.JSONDecoder().raw_decode
points, strokes, labels, hands = [], [], [], []
sys.stdin.readline()
for line in sys.stdin:
    symbol, hand, rest = line.split(";", 2)
    try:
        written = [np.array([[p["x"], p["y"]] for p in stroke], np.float32) for stroke in decode(rest)[0] if stroke]
    except (ValueError, KeyError, TypeError):
        continue
    if not written:
        continue
    # no symbol here has a baseline or a size: each is 32 px on its longer side, around 0
    every = np.concatenate(written)
    low, high = every.min(0), every.max(0)
    size = max((high - low).max(), 1)
    P, S = resample([(stroke - (low + high) / 2) * 32 / size for stroke in written])
    points.append(P.astype(np.float16)); strokes.append(S); labels.append(int(symbol)); hands.append(int(hand))
np.savez(sys.argv[1], points=np.stack(points), strokes=np.stack(strokes), labels=np.array(labels), hands=np.array(hands))
print(len(points), "symbols written", N, "points each")
