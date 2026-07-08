# 🌊 Fluent

**Reactive programming for differentiable tensors.**

Fluent is a tiny language + IDE for people who like to tinker with math — plots, simulations, little machines that learn. You write plain expressions; they render, move, and differentiate as you type.

**[▶ Open the playground](https://mlajtos.github.io/fluent/)** — nothing to install, and it's fast: your GPU does the work, right in the browser.

[![square: { x | x^2 }, ∇(square)([1, 2, 3]) — evaluated live in the playground](hero.png)](https://mlajtos.github.io/fluent/?code=c3F1YXJlOiB7IHggfCB4XjIgfSwK4oiHKHNxdWFyZSkoWzEsIDIsIDNdKSAgIDsgWzIsIDQsIDZd)

[**▶ run this**](https://mlajtos.github.io/fluent/?code=c3F1YXJlOiB7IHggfCB4XjIgfSwK4oiHKHNxdWFyZSkoWzEsIDIsIDNdKSAgIDsgWzIsIDQsIDZd) — every snippet in this README is a link that opens live in the playground.

<!-- code fences say "clojure" because GitHub already has a language named
     Fluent (Mozilla's FTL) and paints ours as errors; clojure is the nearest
     look-alike (; comments, brackets, numbers) -->

## Why it feels different

- **Reading order is meaning.** There is no precedence table: spaced operators run left-to-right, glued ones bind tighter — that's the whole story. Code you've never seen evaluates the way you read it, pieces compose without surprises, and any glyph can become an operator of your own.
- **Everything is differentiable.** `∇` works on any function, including ones you just wrote. Higher-order too: `∇(∇(f))`.
- **Everything is reactive.** Drag a slider and exactly the expressions that depend on it re-run — nothing else, nothing forgotten.
- **Results are visible by default.** Tensors render as plots, heatmaps, and images; the AST renders as a tree; there is no `print`.

## See it do things

| | demo | |
|---|---|---|
| 🌀 | **Mandelbrot** — the whole plane iterated at once with `⍣`; scrub the depth | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBNYW5kZWxicm90IHNldCDigJMgeiDihpAgesKyICsgYyB1bnRpbCB8enwgZXNjYXBlcwo7IGNvbXBsZXggbnVtYmVycyBhcyAocmUsIGltKSBwYWlycywg4o2jIGl0ZXJhdGVzIHRoZSB3aG9sZSBwbGFuZSBhdCBvbmNlCjsgc2NydWIgdGhlIGRlcHRoIGFuZCB3YXRjaCB0aGUgc2V0IHNoYXJwZW4KCnc6IDM1MCwgICAgICAgIDsgcmVzb2x1dGlvbiwgNzo1IGxpa2UgdGhlIHZpZXcKaDogMjUwLApkZXB0aDogJCg0MCksICA7IGl0ZXJhdGlvbnMg4oCTIGRyYWcgbWUKCnJlOiBsaW5zcGFjZShbLTIuNSwgMV0sIHcpIOKNtCBbMSwgd10gdGlsZSBbaCwgMV0sCmltOiBsaW5zcGFjZShbLTEuMjUsIDEuMjVdLCBoKSDijbQgW2gsIDFdIHRpbGUgWzEsIHddLAoKOyBvbmUgc3RlcCBmb3IgZXZlcnkgcGl4ZWw6IHN0YXRlIGlzICh6eCwgenksIGVzY2FwZSBjb3VudCkKc3RlcDogeyBzIHwKICB6eDogTGlzdEdldChzLCAwKSwKICB6eTogTGlzdEdldChzLCAxKSwKICBrOiAgTGlzdEdldChzLCAyKSwKICBhbGl2ZTogKCh6eF4yICsgenleMikg4omkIDQpLAogICgKICAgIHdoZXJlKGFsaXZlLCB6eF4yIC0genleMiArIHJlLCB6eCksCiAgICB3aGVyZShhbGl2ZSwgMsOXenjDl3p5ICsgaW0sIHp5KSwKICAgIGsgKyBhbGl2ZSwKICApCn0sCgp6ZXJvOiByZSDDlyAwLApyZW5kZXI6IHsgZCB8CiAgbjogKDEg4oyIIGQpLAogIGNvdW50czogTGlzdEdldCgoc3RlcCDijaMgbikoKHplcm8sIHplcm8sIHplcm8pKSwgMiksCiAg4oiaKGNvdW50cyDDtyBuKQp9LAoKKAogIFRleHQoIiMg8J-MgCBNYW5kZWxicm90IiksCiAgR3JpZChbMSwgOV0pKFRleHQoIioqZGVwdGg6KioiKSwgU2NydWJiZXIoZGVwdGgpKSwKICByZW5kZXIoZGVwdGgpLAop) |
| 🦠 | **Game of Life** on a 256² torus — three lines of rules | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBDb253YXkncyBHYW1lIG9mIExpZmUKCjsgZ3JpZCBzaXplCm46IDI1NiwKCjsgYSBjZWxsIHBsdXMgaXRzIHR3byBuZWlnaGJvdXJzIGFsb25nIG9uZSBheGlzCmJveDogeyBnLCBheCB8IHJvbGwoZywgMSwgYXgpICsgZyArIHJvbGwoZywgLTEsIGF4KSB9LAoKOyB0aGUgM8OXMyBzdW0sIG1pbnVzIHRoZSBjZWxsID0gaXRzIDggbmVpZ2hib3VycwpuZWlnaGJvdXJzOiB7IGcgfCBib3goYm94KGcsIDApLCAxKSAtIGcgfSwKCjsgcmFuZG9tIHNvdXAKYm9hcmQ6ICQocmFuZChbbiwgbl0pIDwgMC4zKSwKCnN0ZXA6IHsKICBjOiBvbmNlKGJvYXJkKSwKICBhOiBuZWlnaGJvdXJzKGMpLAoKICA7IGFsaXZlIG5leHQgaWYgMyBuZWlnaGJvdXJzLCBvciAyIGFuZCBhbHJlYWR5IGFsaXZlCiAgYm9hcmQg4oaQICgoYSA9IDMpIOKMiCAoKGEgPSAyKSDDlyBjKSksCn0sCgpzdGVwIOKfsyAxMDAwMDAsCgooCiAgVGV4dCgiIyDwn6agIEdhbWUgb2YgTGlmZSIpLAogIGJvYXJkLAop) |
| 📷 | **Live edge detection** — your camera through a Laplacian kernel | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyDwn5O3IExpdmUgZWRnZSBkZXRlY3Rpb24g4oCTIHlvdXIgY2FtZXJhIHRocm91Z2ggYSBMYXBsYWNpYW4ga2VybmVsLgo7IEFsbG93IGNhbWVyYSBhY2Nlc3MsIHRoZW4gd2F2ZTogZmxhdCBhcmVhcyBjYW5jZWwgb3V0LCBlZGdlcyBnbG93LgoKY2FtOiBDYW1lcmEoMzIwLCAyNDApLAprOiBbWzAsIDEsIDBdLCBbMSwgLTQsIDFdLCBbMCwgMSwgMF1dLAoKOyBldmVyeSBmcmFtZTogZ3JheXNjYWxlIHRoZSBpbWFnZSAobWVhbiBvdmVyIHRoZSBjb2xvciBheGlzKSwgY29udm9sdmUKZWRnZXM6ICQoeyBhYnMoY29udihrLCBtZWFuKGNhbSgpLCAyKSkpIH0pLAoKKAogIFRleHQoIiMg8J-TtyBMaXZlIEVkZ2VzIiksCiAgZWRnZXMsCik) |
| 🎵 | **Live spectrum** of your microphone — whistle and watch the peak move | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBMaXZlIHNwZWN0cnVtIGFuYWx5c2VyIOKAkyBGRlQgdGhlIG1pY3JvcGhvbmUgd2F2ZWZvcm0sIHBsb3QgdGhlIG1hZ25pdHVkZS4KOyBBbGxvdyBtaWMgYWNjZXNzLCB0aGVuIHdoaXN0bGUgb3IgcGxheSBhIHRvbmUgYW5kIHdhdGNoIHRoZSBwZWFrIG1vdmUuCgptaWM6IE1pY3JvcGhvbmUoNTEyKSwKCjsgZXZlcnkgZnJhbWU6IHRha2UgdGhlIEZGVCBvZiB0aGUgd2F2ZWZvcm0sIHRoZW4gaXRzIG1hZ25pdHVkZSDiiJoocmXCsiArIGltwrIpLgo7IGZmdCByZXR1cm5zIFtyZWFsOyBpbWFnXSBzdGFja2VkLCBzbyBmXzAgaXMgcmVhbCBhbmQgZl8xIGlzIGltYWdpbmFyeS4Kc3BlY3RydW06ICQoeyBmOiBmZnQobWljKCkpLCDiiJooKGZfMCleMiArIChmXzEpXjIpIH0pLAoKKAogIFRleHQoIiMg8J-OtSBMaXZlIFNwZWN0cnVtIiksCiAgc3BlY3RydW0sCik) |
| 🎸 | **Pitch detector** — hum a note, it tells you which one | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBQaXRjaCBkZXRlY3RvciDigJMgRkZUIHRoZSBtaWMsIGZpbmQgdGhlIGxvdWRlc3Qgbm90ZS4gQWxsb3cgbWljIGFjY2VzcywKOyB0aGVuIHdoaXN0bGUgb3IgaHVtIGEgc3RlYWR5IHRvbmUgYW5kIHdhdGNoIHRoZSBub3RlIGxvY2sgb24uCgpzaXplOiA4MTkyLApzcjogU2FtcGxlUmF0ZSgpLAptaWM6IE1pY3JvcGhvbmUoc2l6ZSksCm5hbWVzOiAoIkMiLCAiQyMiLCAiRCIsICJEIyIsICJFIiwgIkYiLCAiRiMiLCAiRyIsICJHIyIsICJBIiwgIkEjIiwgIkIiKSwKCmRldGVjdDogJCh7CiAgZjogZmZ0KG1pYygpKSwKICBtYWc6IOKImihmXzBeMiArIGZfMV4yKSwKICBiaW5zOiAwIDo6ICMobWFnKSwKICBmcmVxczogYmlucyDDlyBzciDDtyBzaXplLAogIDsgb25seSB0aGUgbXVzaWNhbCByYW5nZSwgc28gbWljIHJ1bWJsZSBhbmQgaGlzcyBkb24ndCB3aW4KICBiYW5kOiBtYWcgw5cgKGZyZXFzID4gNTUpIMOXIChmcmVxcyA8IDIwMDApLAogIDsgdGhlIGxvdWRlc3QgYmluIOKAkyBrZXB0IGFzIGEgZmxvYXQgKGFyZ21heCB3b3VsZCBiZSBpbnQpCiAgcGVhazogzqMoYmlucyDDlyAoYmFuZCA9IG1heChiYW5kKSkpLAogIGZyZXE6IHBlYWsgw5cgc3Igw7cgc2l6ZSwKICBtaWRpOiByb3VuZCg2OSArIDEyw5dsb2cyKGZyZXEgw7cgNDQwKSksCiAgKAogICAgTGlzdEdldChuYW1lcywgbWlkaSAlIDEyKSwKICAgIGZsb29yKG1pZGkgw7cgMTIpIC0gMSwKICAgIHJvdW5kKGZyZXEpLAogICkKfSksCgooCiAgVGV4dCgiIyDwn464IFBpdGNoIERldGVjdG9yIiksCiAgVGV4dCgiKipub3RlIMK3IG9jdGF2ZSDCtyBIeioqIiksCiAgZGV0ZWN0LAop) |
| 📈 | **Linear regression** — fit a line by gradient descent, loss falling live | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBMaW5lYXIgUmVncmVzc2lvbiB3aXRoIGdyYWRpZW50IGRlc2NlbnQKOyBGaW5kIM64IHRoYXQgbWluaW1pemVzIChmKHgpIC0geSnCsgoKOyBPcGVyYXRvcnMKKCsrKTogVGVuc29yQ29uY2F0LAoKOyBEYXRhOiB5ID0gMC4yM3ggKyAwLjQ3Cng6IDAgOjogMTAsCnk6IHjDlzAuMjMgKyAwLjQ3LAoKOyBNb2RlbDogZih4KSA9IM644oKAwrd4ICsgzrjigoEKzrg6IH4oWzAsIDBdKSwKZjogeyB4IHwgeCDDlyDOuF8wICsgzrhfMSB9LAoKOyBMb3NzOiBtZWFuIHNxdWFyZWQgZXJyb3IK8J2TmzogeyBtZWFuKChmKHgpIC0geSleMikgfSwKCjsgVHJhaW5pbmcKb3B0OiBhZGFtKDAuMDMpLApsb3NzZXM6ICQoW10pLAoKeyBsb3NzZXMobG9zc2VzKCkgKysgW29wdCjwnZObKV0pIH0g4p-zIDIwMCwKCjsgUmVzdWx0cwooCiAgICBUZXh0KCIqKkxvc3M6KioiKSwgbG9zc2VzLAogICAgVGV4dCgiKirOuCAobGVhcm5lZCk6KioiKSwgzrgsCiAgICBUZXh0KCIqKs64ICh0YXJnZXQpOioqIiksIFswLjIzLCAwLjQ3XSwKKQ) |
| 🏔 | **Touch the loss landscape** — grab the ball, drag the learning rate, watch gradient descent roll | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBUb3VjaCB0aGUgTG9zcyBMYW5kc2NhcGUKOyBPTkUgbG9zcyBkZWZpbml0aW9uIGRyaXZlcyBib3RoIHRoZSBzdXJmYWNlIGFuZCB0aGUgZGVzY2VudCDigJMgZWRpdCBpdCBsaXZlIQoK8J2TmzogeyBwIHwgKHBfMF4yICsgcF8xIC0gMTEpXjIgKyAocF8wICsgcF8xXjIgLSA3KV4yIH0sICAgOyBIaW1tZWxibGF1IOKAkyBmb3VyIG1pbmltYQoKzrg6IH4oWzAsIDBdKSwgICA7IHRoZSBiYWxsIOKAkyBncmFiIGl0IQpscjogJCgwLjQpLApzdGVwOiB7IHNnZChvbmNlKGxyKSDDlyAwLjAwNSkoeyDwnZObKM64KSB9KSB9LAoKOyB0aGUgc3VyZmFjZTogdGhlIHNhbWUg8J2TmyBldmFsdWF0ZWQgb3ZlciB0aGUgd2hvbGUgdmlldyB2aWEg4oqXCm46IDMwMCwKcmFuZ2U6IFtbLTUsIDVdLCBbLTUsIDVdXSwKc3VyZmFjZTogbG9nKGxpbnNwYWNlKHJhbmdlXzEsIG4pICjiipcgeyB5LCB4IHwg8J2TmyhbeCwgeV0pIH0pIGxpbnNwYWNlKHJhbmdlXzAsIG4pICsgMSksCgooCiAgVGV4dCgiIyBUb3VjaCB0aGUgTG9zcyBMYW5kc2NhcGUiKSwKICBHcmlkKFsxLCA1XSkoVGV4dCgibHIiKSwgU2xpZGVyKGxyKSksCiAgQnV0dG9uKCJEZXNjZW5kIMOXMTAwIiwgeyBzdGVwIOKfsyAxMDAgfSksCiAgTGF5ZXJzKHN1cmZhY2UsIFRyYWlsKM64LCByYW5nZSksIFBvaW50MkQozrgsIHJhbmdlKSksCiAg8J2Tmyh3YXRjaCjOuCkpLCAgIDsgbGl2ZSBsb3NzIOKAkyB1cGRhdGVzIHdoaWxlIHlvdSBkcmFnCik) |
| ⚔️ | **adam vs sgd** race down Himmelblau — same start, different characters | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyDimpTvuI8gYWRhbSB2cyBzZ2Qg4oCTIHNhbWUgc3RhcnQsIHNhbWUgbGFuZHNjYXBlLCBkaWZmZXJlbnQgY2hhcmFjdGVycwoK8J2TmzogeyBwIHwgKHBfMF4yICsgcF8xIC0gMTEpXjIgKyAocF8wICsgcF8xXjIgLSA3KV4yIH0sICAgOyBIaW1tZWxibGF1CgphOiB-KFswLCAtMC4zXSksICAgOyBhZGFtLCBvcmFuZ2UKczogfihbMCwgLTAuM10pLCAgIDsgc2dkLCBibHVlCm9wdDogYWRhbSgwLjA4KSwKc3RlcDogeyBvcHQoeyDwnZObKGEpIH0pLCBzZ2QoMC4wMDIpKHsg8J2TmyhzKSB9KSB9LAoKbjogMzAwLApyYW5nZTogW1stNSwgNV0sIFstNSwgNV1dLApzdXJmYWNlOiBsb2cobGluc3BhY2UocmFuZ2VfMSwgbikgKOKKlyB7IHksIHggfCDwnZObKFt4LCB5XSkgfSkgbGluc3BhY2UocmFuZ2VfMCwgbikgKyAxKSwKCigKICBUZXh0KCIjIOKalO-4jyBhZGFtIHZzIHNnZCIpLAogIEJ1dHRvbigiUmFjZSDDlzMwMCIsIHsgc3RlcCDin7MgMzAwIH0pLAogIExheWVycygKICAgIHN1cmZhY2UsCiAgICBUcmFpbChhLCByYW5nZSwgIm9yYW5nZSIpLCBUcmFpbChzLCByYW5nZSwgImRlZXBza3libHVlIiksCiAgICBQb2ludDJEKGEsIHJhbmdlLCAib3JhbmdlIiksIFBvaW50MkQocywgcmFuZ2UsICJkZWVwc2t5Ymx1ZSIpLAogICksCiAgR3JpZCgyKShUZXh0KCIqKmFkYW0qKiIpLCBUZXh0KCIqKnNnZCoqIikpLAogIEdyaWQoMiko8J2Tmyh3YXRjaChhKSksIPCdk5sod2F0Y2gocykpKSwKKQ) |
| 🌸 | **Reaction–diffusion** — Gray–Scott chemistry paints Turing patterns; drag *feed* and *kill* | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBHcmF54oCTU2NvdHQgcmVhY3Rpb27igJNkaWZmdXNpb24g4oCTIHR3byBjaGVtaWNhbHMgcGFpbnQgVHVyaW5nIHBhdHRlcm5zLgo7IHYgZWF0cyB1IGFuZCByZXByb2R1Y2VzICh1IMOXIHbCsik7IGJvdGggZGlmZnVzZTsgZXhwbGljaXQgRXVsZXIuCgpuOiAyMDAsCmVkZ2VzOiB7IHgsIGF4IHwgcm9sbCh4LCAxLCBheCkgKyByb2xsKHgsIC0xLCBheCkgfSwgICA7IGJvdGggbmVpZ2hib3VycyBhbG9uZyBhbiBheGlzCs6UOiB7IHggfCBlZGdlcyh4LCAwKSArIGVkZ2VzKHgsIDEpIC0gNMOXeCB9LCAgICAgICAgICAgICA7IHRoZSBMYXBsYWNpYW4KCjsgc3ByaW5rbGUgdiBpbnRvIGEgc2VhIG9mIHUsIHRoZW4gbGV0IGl0IHNlbGYtb3JnYW5pc2UKdTogJCgxKSwKdjogJCgwKSwKcmVzZWVkOiB7IHM6IHJhbmQoW24sIG5dKSA8IDAuMDggw5cgMC41LCB14oaQIDEgLSBzLCB24oaQIHMgfSwKCjsgZHJhZyBmZWVkIGFuZCBraWxsIOKAkyB0aGUgd2hvbGUgem9vIG9mIHBhdHRlcm5zIGhpZGVzIGluIGEgdGlueSB3aW5kb3cKRjogJCgwLjA1NSksCks6ICQoMC4wNjIpLAoKdGljazogewogIGE6IG9uY2UodSksIGI6IG9uY2UodiksIGY6IG9uY2UoRiksIGs6IG9uY2UoSyksCiAgcjogYSDDlyBiXjIsCiAgdeKGkCBhICsgMC4xNsOXzpQoYSkgLSByICsgZiAtIGbDl2EsCiAgduKGkCBiICsgMC4wOMOXzpQoYikgKyByIC0gZsOXYiAtIGvDl2IsCn0sCgpyZXNlZWQoKSwKdGljayDin7MgMTAwMDAwLAoKKAogIFRleHQoIiMg8J-MuCBSZWFjdGlvbuKAk0RpZmZ1c2lvbiIpLAogIEdyaWQoWzEsIDZdKShUZXh0KCIqKmZlZWQqKiIpLCBTY3J1YmJlcihGLCAwLjAwMikpLAogIEdyaWQoWzEsIDZdKShUZXh0KCIqKmtpbGwqKiIpLCBTY3J1YmJlcihLLCAwLjAwMikpLAogIEJ1dHRvbigicmVzZWVkIiwgcmVzZWVkKSwKICB2LAop) |
| 🔬 | **Lenia** — continuous cellular automata; smooth, life-like blobs emerge | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBMZW5pYSDigJMgY29udGludW91cyBjZWxsdWxhciBhdXRvbWF0YSAoQmVydCBDaGFuKS4gQSByaW5nIGtlcm5lbCBjb252b2x2ZXMgdGhlCjsgd29ybGQ7IGEgYmVsbC1jdXJ2ZSBncm93dGggcnVsZSBudWRnZXMgZWFjaCBjZWxsLiBTbW9vdGgsIGxpZmUtbGlrZSBibG9icyBlbWVyZ2UuCgpuOiAxMjgsClI6IDEzLAoKOyBhIEdhdXNzaWFuIHJpbmcga2VybmVsLCBub3JtYWxpc2VkIHRvIHN1bSAxCmtzOiAyw5dSICsgMSwKZ3g6ICgwIDo6IGtzKSDijbQgW2tzLCAxXSB0aWxlIFsxLCBrc10sCmd5OiAoMCA6OiBrcykg4o20IFsxLCBrc10gdGlsZSBba3MsIDFdLApkaXN0OiDiiJooKGd4IC0gUileMiArIChneSAtIFIpXjIpIMO3IFIsCnJpbmc6IGV4cCgwIC0gKGRpc3QgLSAwLjUpXjIgw7cgKDIgw5cgMC4xNV4yKSkgw5cgKGRpc3QgPCAxKSwKSzogcmluZyDDtyDOoyhyaW5nKSwKCjsgZ3Jvd3RoOiBhIGJlbGwgY2VudHJlZCBhdCDOvCwgd2lkdGggz4MsIG1hcHBlZCB0byBbLTEsIDFdCs68OiAwLjE1LArPgzogMC4wMTcsCmdyb3d0aDogeyB1IHwgMiDDlyBleHAoMCAtICh1IC0gzrwpXjIgw7cgKDIgw5cgz4NeMikpIC0gMSB9LAoKd29ybGQ6ICQoMCksCnJlc2VlZDogeyB3b3JsZCDihpAgcmFuZChbbiwgbl0pIH0sCgo7IGNvbnZvbHZlLCBncm93LCBhbmQga2VlcCB0aGUgd29ybGQgaW4gWzAsIDFdCnRpY2s6IHsKICB3OiBvbmNlKHdvcmxkKSwKICB3b3JsZCDihpAgY2xhbXAodyArIDAuMcOXZ3Jvd3RoKGNvbnYoSywgdykpLCAwLCAxKSwKfSwKCnJlc2VlZCgpLAp0aWNrIOKfsyAxMDAwMDAsCgooCiAgVGV4dCgiIyDwn5SsIExlbmlhIiksCiAgQnV0dG9uKCJyZXNlZWQiLCByZXNlZWQpLAogIHdvcmxkLAop) |
| 🧲 | **Spinning magnets** — an animated field from outer products | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBBbmltYXRlZCBNYWduZXRpYyBGaWVsZCDigJMgbWluaW1hbAo7IGEgbWFnbmV0IGlzIHR3byBvcHBvc2l0ZSBjaGFyZ2VzOyDiipcgaXMgdGhlIG1lc2hncmlkCgoo4om7KTogeyB0LCB2IHwgKHZfMCDDlyAxLXQpICsgKHZfMSDDlyB0KSB9LCAgOyBsZXJwCgptOiAzLCAgICA7IG1hZ25ldHMKbjogNjAwLCAgOyByZXNvbHV0aW9uCnQ6IFRpbWUoKSwKCmFuZ2xlczogcmFuZChbbV0pw5c2LjI4ICsgdMOXKHJhbmQoW21dKSDiibsgWzAuMSwgMC41XSksCmRpcjogc3RhY2soY29zKGFuZ2xlcyksIHNpbihhbmdsZXMpKSDDlyAwLjEyLCAgIDsgWzIgbV0gaGVhZGluZ3MKcG9zOiByYW5kKFsyLCBtXSkg4om7IFstMS40LCAxLjRdLCAgICAgICAgICAgICAgIDsgWzIgbV0gY2VudGVycwpwb2xlOiBjb25jYXQoKHBvcyArIGRpciwgcG9zIC0gZGlyKSwgMSksICAgICAgIDsgWzIgMm1dIGFsbCBzaXggcG9sZXMKcTogY29uY2F0KGZpbGwoW21dLCAxKSwgZmlsbChbbV0sIC0xKSksICAgICAgICA7IGNoYXJnZSBvZiBlYWNoIHBvbGUKCmdyaWQ6IGxpbnNwYWNlKFstMiwgMl0sIG4pLApkeDogZ3JpZCAo4oqXIC0pIHBvbGVfMCwgICAgICAgICAgOyBbbiAybV0KZHk6IGdyaWQgKOKKlyAtKSBwb2xlXzEsICAgICAgICAgIDsgW24gMm1dCnI6IOKImihkeF4yICgrIOKKlyAxKSBkeV4yKSArIDFlLTQsIDsgW24gbiAybV0g4oCTIGZyYW1lcyBjcm9zcywgcG9sZXMgemlwCgprOiBzaW4odCDDlyAyKSDDlyAwLjIgKyAwLjUsICAgOyBwdWxzaW5nIHN0cmVuZ3RoCnBvdGVudGlhbDogc3VtKHHDl2sgLyByLCAyKSwKCigKICBUZXh0KCIjIPCfp7IgU3Bpbm5pbmcgJiBQdWxzYXRpbmcgTWFnbmV0cyIpLAogIGFicyhzaW4ocG90ZW50aWFsIMOXIDI1KSkgXiAwLjI1LAop) |
| 🧠 | **Attention**, built by hand — drag the temperature, watch softmax sharpen | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBTY2FsZWQgZG90LXByb2R1Y3QgYXR0ZW50aW9uLCBidWlsdCBieSBoYW5kIOKAkyB0aGUgY29yZSBvZiBhIHRyYW5zZm9ybWVyLgo7IHNjb3JlcyA9IFHCt0vhtYAgLyDiiJpkLCAgd2VpZ2h0cyA9IHNvZnRtYXgoc2NvcmVzKS4gIERyYWcgdGhlIHRlbXBlcmF0dXJlIGtub2IKOyBhbmQgd2F0Y2ggZXZlcnkgdG9rZW4ncyBhdHRlbnRpb24gZ28gZnJvbSB1bmlmb3JtIHRvIG9uZS1ob3QsIGxpdmUuCgpuOiA0OCwgICA7IHRva2VucwpkOiAxNiwgICA7IGVtYmVkZGluZyBkaW1lbnNpb24KCjsgc2ludXNvaWRhbCBwb3NpdGlvbmFsIGVtYmVkZGluZ3MsIHNvIG5lYXJieSB0b2tlbnMgYXJlIHNpbWlsYXIgKEsgPSBRKQpwb3M6IDAgOjogbiwKazogMCA6OiAoZCDDtyAyKSwKZnJlcXM6IDEgw7cgKDEwMDAwIF4gKGsgw7cgKGQgw7cgMikpKSwKYW5nbGVzOiBwb3MgKOKKlyDDlykgZnJlcXMsClE6IGNvbmNhdCgoc2luKGFuZ2xlcyksIGNvcyhhbmdsZXMpKSwgMSksCgo7IGV2ZXJ5IHF1ZXJ5IGRvdHRlZCB3aXRoIGV2ZXJ5IGtleSwgc2NhbGVkCnNjb3JlczogbWF0bXVsKFEsIHRyYW5zcG9zZShRKSkgw7cg4oiaKGQpLAoKOyB0ZW1wZXJhdHVyZSBzaGFycGVucyAob3IgZmxhdHRlbnMpIHRoZSBzb2Z0bWF4IOKAkyByZWNvbXB1dGVzIGFzIHlvdSBkcmFnCnRlbXA6ICQoNCksCndlaWdodHM6ICQoeyBzb2Z0bWF4KHNjb3JlcyDDlyB0ZW1wKCkpIH0pLAoKKAogIFRleHQoIiMg8J-noCBBdHRlbnRpb24iKSwKICBHcmlkKFsxLCA2XSkoVGV4dCgiKip0ZW1wZXJhdHVyZSoqIiksIFNjcnViYmVyKHRlbXAsIDAuNSkpLAogIFRleHQoIndobyBhdHRlbmRzIHRvIHdob206IiksCiAgd2VpZ2h0cywKKQ) |
| 🎓 | **Turing's B-type machine** (1948) learns XOR by backprop — it isn't in its toolbox, so it composes it | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyBUdXJpbmcncyBCLXR5cGUgdW5vcmdhbmlzZWQgbWFjaGluZSAoMTk0OCksIGVkdWNhdGVkIGJ5IGdyYWRpZW50IGRlc2NlbnQuCjsgQSBuZXR3b3JrIG9mIHNvZnQgTkFORC1pc2ggZ2F0ZXMgbGVhcm5zIFhPUiDigJMgd2hpY2ggaXNuJ3QgaW4gaXRzIHRvb2xib3gsCjsgc28gaXQgbXVzdCBjb21wb3NlIGl0IChYT1IgPSBPUiDiiKcgwqxBTkQpLiAiRWR1Y2F0aW9uIiBpcyBqdXN0IGJhY2twcm9wLgoKKCsrKTogVGVuc29yQ29uY2F0LAoKYTogWzAsIDAsIDEsIDFdLApiOiBbMCwgMSwgMCwgMV0sCnRhcmdldDogWzAsIDEsIDEsIDBdLAoKOyAxMiB0d28taW5wdXQgYm9vbGVhbiBmdW5jdGlvbnMg4oCTIEFORCwgT1IsIE5BTkQsIE5PVCwg4oCmIGJ1dCBubyBYT1IKYmFzaXM6IHsgcCwgcSB8IHN0YWNrKCgKICBww5dxLCBwICsgcSAtIHDDl3EsIDEgLSBww5dxLCAxIC0gKHAgKyBxIC0gcMOXcSksCiAgcMOXKDEgLSBxKSwgKDEgLSBwKcOXcSwgcCwgcSwgMSAtIHAsIDEgLSBxLCBww5cwLCBww5cwICsgMQopKSB9LAoKOyBlYWNoIGdhdGUncyBjb25uZWN0aW9uLXN3aXRjaDogYSBsZWFybmFibGUgc29mdG1heCBvdmVyIHRoZSAxMiBmdW5jdGlvbnMKdDE6IH4ocmFuZG4oWzEyXSkgw5cgMC4zKSwKdDI6IH4ocmFuZG4oWzEyXSkgw5cgMC4zKSwKdG86IH4ocmFuZG4oWzEyXSkgw5cgMC4zKSwKCjsgdHdvIGhpZGRlbiBnYXRlcyByZWFkIHRoZSBpbnB1dHM7IHRoZSBvdXRwdXQgZ2F0ZSByZWFkcyB0aGUgaGlkZGVuIGdhdGVzCmZ3ZDogewogIGgxOiBtYXRtdWwoc29mdG1heCh0MSksIGJhc2lzKGEsIGIpKSwKICBoMjogbWF0bXVsKHNvZnRtYXgodDIpLCBiYXNpcyhhLCBiKSksCiAgbWF0bXVsKHNvZnRtYXgodG8pLCBiYXNpcyhoMSwgaDIpKQp9LApsb3NzOiB7IG1lYW4oKGZ3ZCgpIC0gdGFyZ2V0KV4yKSB9LAoKb3B0OiBhZGFtKDAuMTUpLApsb3NzZXM6ICQoW10pLAp7IGxvc3Nlcyhsb3NzZXMoKSArKyBbb3B0KGxvc3MpXSkgfSDin7MgNDAwLAoKOyByZWNvbXB1dGUgZWFjaCB0cmFpbmluZyBzdGVwIChzdWJzY3JpYmUgdG8gbG9zc2VzKSBzbyB0aGUgZGlzcGxheSBzdGF5cyBsaXZlCm91dHB1dDogJCh7IGxvc3NlcygpLCBmd2QoKSB9KSwKZ2F0ZXM6ICQoeyBsb3NzZXMoKSwgc3RhY2soKHNvZnRtYXgodDEpLCBzb2Z0bWF4KHQyKSwgc29mdG1heCh0bykpKSB9KSwKCigKICBUZXh0KCIjIPCfjpMgQi10eXBlIGxlYXJucyBYT1IiKSwKICBUZXh0KCIqKmxvc3MqKiDigJMgZWR1Y2F0aW9uIGluIHByb2dyZXNzOiIpLAogIGxvc3NlcywKICBUZXh0KCIqKm91dHB1dCoqIHZzIHRhcmdldCAoMCwgMSwgMSwgMCk6IiksCiAgb3V0cHV0LAogIFRleHQoInRoZSB0aHJlZSBnYXRlcycgc3dpdGNoZXMsIGNvbnZlcmdpbmcgb24gT1IgwrcgQU5EIMK3IChwIOKIpyDCrHEpOiIpLAogIGdhdGVzLAop) |
| 🔢 | A differentiable **logic-gate network learns MNIST** in your tab — no neurons, just soft boolean gates | [▶&nbsp;run](https://mlajtos.github.io/fluent/?code=OyDwn5SiIEEgZGlmZmVyZW50aWFibGUgbG9naWMtZ2F0ZSBuZXR3b3JrIGxlYXJucyBNTklTVCDigJMgdGhlIEItdHlwZSBtYWNoaW5lLAo7IHNjYWxlZCB1cC4gNTEyIOKGkiAyMDAgc29mdCBib29sZWFuIGdhdGVzIHdpcmVkIGF0IHJhbmRvbSBvdmVyIDE5NiBwaXhlbHM7CjsgZWFjaCBnYXRlJ3MgaWRlbnRpdHkgKHdoaWNoIG9mIHRoZSAxNiB0d28taW5wdXQgdHJ1dGggdGFibGVzIGl0IGNvbXB1dGVzKSBpcwo7IGEgbGVhcm5hYmxlIHNvZnRtYXgsIGVkdWNhdGVkIGJ5IGdyYWRpZW50IGRlc2NlbnQuIE5vIG5ldXJvbnMsIGp1c3QgbG9naWMuCgooKyspOiBUZW5zb3JDb25jYXQsCgo7IC0tLSBkYXRhOiAxNMOXMTQgYmluYXJpc2VkIE1OSVNUICg2MDAwIHRyYWluLCAxMDAwIHRlc3QpIGFzIHNhZmV0ZW5zb3JzIC0tLQo7IHRoZSBzaWduYWwgaXMgYW4gZW1wdHkgbGlzdCB1bnRpbCB0aGUgZmV0Y2ggcmVzb2x2ZXMgdG8gW1h0cmFpbixZdHJhaW4sWHRlc3QsWXRlc3RdCmRhdGE6IExvYWRTYWZlVGVuc29yRnJvbVVSTCgibW5pc3Quc2FmZXRlbnNvcnMiKSwKbG9hZGVkOiB7IExpc3RMZW5ndGgoZGF0YSgpKSA-IDAgfSwKOyB0cmFpbiBvbiAzMDAwIG9mIHRoZSA2MDAwLCBzY29yZSBvbiA1MDAgb2YgdGhlIDEwMDAg4oCTIGtlZXBzIGVhY2ggZnJhbWUgbGlnaHQKOyBzbyB0aGUgd2hvbGUgdGhpbmcgdHJhaW5zIGxpdmUgYW5kIHNtb290aGx5IGluIHRoZSBicm93c2VyIChwaXhlbHMgw5cgZXhhbXBsZXMpClh0cjogeyB0cmFuc3Bvc2Uoc2xpY2UoTGlzdEdldChMaXN0R2V0KGRhdGEoKSwgMCksIDEpLCAwLCAzMDAwKSkgfSwgICA7IFsxOTYsIDMwMDBdCll0cjogeyB0cmFuc3Bvc2Uoc2xpY2UoTGlzdEdldChMaXN0R2V0KGRhdGEoKSwgMSksIDEpLCAwLCAzMDAwKSkgfSwgICA7IFsxMCwgMzAwMF0KWHRlOiB7IHRyYW5zcG9zZShzbGljZShMaXN0R2V0KExpc3RHZXQoZGF0YSgpLCAyKSwgMSksIDAsIDUwMCkpIH0sICAgIDsgWzE5NiwgNTAwXQpZdGU6IHsgdHJhbnNwb3NlKHNsaWNlKExpc3RHZXQoTGlzdEdldChkYXRhKCksIDMpLCAxKSwgMCwgNTAwKSkgfSwgICAgOyBbMTAsIDUwMF0KCjsgLS0tIGZpeGVkIHJhbmRvbSB3aXJpbmc6IHR3byBzb3VyY2Ugc2lnbmFscyBwZXIgZ2F0ZSAtLS0KRzE6IDUxMiwgRzI6IDIwMCwKSUExOiBmbG9vcihyYW5kKFtHMV0pIMOXIDE5NiksICBJQjE6IGZsb29yKHJhbmQoW0cxXSkgw5cgMTk2KSwgICA7IGxheWVyIDEgcmVhZHMgcGl4ZWxzCklBMjogZmxvb3IocmFuZChbRzJdKSDDlyBHMSksICAgSUIyOiBmbG9vcihyYW5kKFtHMl0pIMOXIEcxKSwgICAgOyBsYXllciAyIHJlYWRzIGxheWVyIDEKCjsgdGhlIDE2IHR3by1pbnB1dCB0cnV0aCB0YWJsZXMgKHJvdyBpID0gYmluYXJ5IG9mIGkgPSBbZjAwLCBmMDEsIGYxMCwgZjExXSkKVFQ6IFsKICBbMCwwLDAsMF0sIFswLDAsMCwxXSwgWzAsMCwxLDBdLCBbMCwwLDEsMV0sCiAgWzAsMSwwLDBdLCBbMCwxLDAsMV0sIFswLDEsMSwwXSwgWzAsMSwxLDFdLAogIFsxLDAsMCwwXSwgWzEsMCwwLDFdLCBbMSwwLDEsMF0sIFsxLDAsMSwxXSwKICBbMSwxLDAsMF0sIFsxLDEsMCwxXSwgWzEsMSwxLDBdLCBbMSwxLDEsMV0KXSwKOyBwb29sIDIwIGdhdGVzIHBlciBjbGFzczogR1JPVVBbYywgZ10gPSAxIHdoZXJlIGdhdGUgZyBiZWxvbmdzIHRvIGNsYXNzIGMKR1JPVVA6ICgwIDo6IDEwKSDiipcoPSkgZmxvb3IoKDAgOjogRzIpIMO3IChHMiDDtyAxMCkpLAoKOyBhIHNvZnQgZ2F0ZTogZXhwZWN0ZWQgdHJ1dGggdGFibGUgc29mdG1heCjOuCnCt1RULCBibGVuZGVkIG92ZXIgdGhlIDQgaW5wdXQgY29ybmVycwpnYXRlOiB7IEEsIEIsIFcgfCBXdDogdHJhbnNwb3NlKFcpLAogIHJlc2hhcGUoV3RfMCwgWyNBLCAxXSnDlygoMSAtIEEpw5coMSAtIEIpKSArIHJlc2hhcGUoV3RfMSwgWyNBLCAxXSnDlygoMSAtIEEpw5dCKQogICsgcmVzaGFwZShXdF8yLCBbI0EsIDFdKcOXKEHDlygxIC0gQikpICsgcmVzaGFwZShXdF8zLCBbI0EsIDFdKcOXKEHDl0IpCn0sCnNtOiB7IHRoIHwgbWF0bXVsKHNvZnRtYXgodGgpLCBUVCkgfSwKCjsgdGhlIGxlYXJuYWJsZSBwYXJhbWV0ZXJzOiBlYWNoIGdhdGUncyBzb2Z0bWF4IGxvZ2l0cyBvdmVyIHRoZSAxNiBmdW5jdGlvbnMKdDE6IH4ocmFuZG4oW0cxLCAxNl0pIMOXIDAuMSksCnQyOiB-KHJhbmRuKFtHMiwgMTZdKSDDlyAwLjEpLAoKbW9kZWw6IHsgWCB8CiAgaDE6IGdhdGUoZ2F0aGVyKFgsIElBMSksIGdhdGhlcihYLCBJQjEpLCBzbSh0MSkpLAogIGgyOiBnYXRlKGdhdGhlcihoMSwgSUEyKSwgZ2F0aGVyKGgxLCBJQjIpLCBzbSh0MikpLAogIG1hdG11bChHUk9VUCwgaDIpICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA7IFsxMCwgZXhhbXBsZXNdIGxvZ2l0cwp9LAoKOyBjcm9zcy1lbnRyb3B5IG92ZXIgdGhlIDEwIGNsYXNzZXMgKG51bWVyaWNhbGx5LXN0YWJsZSBsb2ctc29mdG1heCBvbiBheGlzIDApCmxvZ3A6IHsgTCB8IEwgLSBsb2cozqMoZXhwKEwpLCAwKSkgfSwKbG9zczogeyAtbWVhbijOoyhZdHIoKSDDlyBsb2dwKG1vZGVsKFh0cigpKSksIDApKSB9LAoKOyB0cmFpbiDigJMgZWFjaCB0aWNrIGlzIGdhdGVkIG9uIHRoZSBkYXRhIGJlaW5nIGxvYWRlZCAoYSBuby1vcCAwIHVudGlsIHRoZW4pCm9wdDogYWRhbSgwLjA1KSwKbG9zc2VzOiAkKFtdKSwKeyBsb3NzZXMobG9zc2VzKCkgKysgW2Nhc2NhZGUoKGd1YXJkKGxvYWRlZCgpLCB7IG9wdChsb3NzKSB9KSwgeyAwIH0pKSgpXSkgfSDin7MgMTUwMCwKCjsgbGl2ZSBhY2N1cmFjeSBvbiB0aGUgaGVsZC1vdXQgdGVzdCBzZXQsIHJlY29tcHV0ZWQgZXZlcnkgdHJhaW5pbmcgc3RlcAphY2N1cmFjeTogJCh7IGxvc3NlcygpLCBjYXNjYWRlKChndWFyZChsb2FkZWQoKSwgewogIGhpdHM6IGFyZ21heChtb2RlbChYdGUoKSksIDApID0gYXJnbWF4KFl0ZSgpLCAwKSwKICByb3VuZChtZWFuKGhpdHMpIMOXIDEwMDApIMO3IDEwCn0pLCB7IDAgfSkpKCkgfSksCgooCiAgVGV4dCgiIyDwn5SiIExvZ2ljIGdhdGVzIGxlYXJuIE1OSVNUIiksCiAgVGV4dCgiQSBuZXR3b3JrIG9mIHNvZnQgYm9vbGVhbiBnYXRlcyDigJMgbm8gbmV1cm9ucyDigJMgcmVhZHMgaGFuZHdyaXR0ZW4gZGlnaXRzLiIpLAogIFRleHQoIioqdGVzdCBhY2N1cmFjeSoqICglKSwgb24gZGlnaXRzIGl0IG5ldmVyIHRyYWluZWQgb246IiksCiAgYWNjdXJhY3ksCiAgVGV4dCgiKip0cmFpbmluZyBsb3NzKio6IiksCiAgbG9zc2VzLAop) |

More in the playground: press <kbd>Ctrl</kbd>+<kbd>O</kbd> for the full gallery.

## The language in a minute

**Spacing is grouping** — hug what belongs together:

```clojure
1 + 2 * 3,   ; 9 — spaced operators run left-to-right
1 + 2*3      ; 7 — glued operators bind tighter
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=MSArIDIgKiAzLCAgIDsgOSDigJQgc3BhY2VkIG9wZXJhdG9ycyBydW4gbGVmdC10by1yaWdodAoxICsgMiozICAgICAgOyA3IOKAlCBnbHVlZCBvcGVyYXRvcnMgYmluZCB0aWdodGVy)

**Tensors are the only numbers** — a scalar is just a small one, arithmetic broadcasts, and a glued `_` indexes:

```clojure
v: [10, 20, 30],
v + 1,         ; [11, 21, 31]
v_0,           ; 10 — a glued _ indexes
v_(-1),        ; 30 — from the end
v_[2, 0],      ; [30, 10] — with a tensor of indices
Σ(v)           ; 60 — and Σ, μ, sort, fft, conv… are built in
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=djogWzEwLCAyMCwgMzBdLAp2ICsgMSwgICAgICAgICA7IFsxMSwgMjEsIDMxXQp2XzAsICAgICAgICAgICA7IDEwIOKAlCBhIGdsdWVkIF8gaW5kZXhlcwp2XygtMSksICAgICAgICA7IDMwIOKAlCBmcm9tIHRoZSBlbmQKdl9bMiwgMF0sICAgICAgOyBbMzAsIDEwXSDigJQgd2l0aCBhIHRlbnNvciBvZiBpbmRpY2VzCs6jKHYpICAgICAgICAgICA7IDYwIOKAlCBhbmQgzqMsIM68LCBzb3J0LCBmZnQsIGNvbnbigKYgYXJlIGJ1aWx0IGlu)

**Functions and operators are the same thing** — anything can be called, anything can sit between its arguments:

```clojure
1 + 2,                ; 3
+(1, 2),              ; 3 — an operator, called
1 add 2,              ; 3 — a function, infix
1 {x, y | x + y} 2    ; 3 — even a lambda
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=MSArIDIsICAgICAgICAgICAgICAgIDsgMworKDEsIDIpLCAgICAgICAgICAgICAgOyAzIOKAlCBhbiBvcGVyYXRvciwgY2FsbGVkCjEgYWRkIDIsICAgICAgICAgICAgICA7IDMg4oCUIGEgZnVuY3Rpb24sIGluZml4CjEge3gsIHkgfCB4ICsgeX0gMiAgICA7IDMg4oCUIGV2ZW4gYSBsYW1iZGE)

Defining your own operator is just a binding: `(++): ListConcat`.

**Everything has three names** — long for discovery, a word for habit, a glyph for fluency:

```clojure
TensorSum(0 :: 10),   ; 45
sum(0 :: 10),         ; 45 — same function
Σ(0 :: 10)            ; 45 — same hover card
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=VGVuc29yU3VtKDAgOjogMTApLCAgIDsgNDUKc3VtKDAgOjogMTApLCAgICAgICAgIDsgNDUg4oCUIHNhbWUgZnVuY3Rpb24KzqMoMCA6OiAxMCkgICAgICAgICAgICA7IDQ1IOKAlCBzYW1lIGhvdmVyIGNhcmQ)

Long names make things findable; the more you use one, the shorter you want it. `TensorGradient` is `grad` is `∇` — all in scope, all sharing one doc card, so your notation can tighten as you go. And names are unicode throughout: `θ`, `𝓛`, `ŷ` are fine.

<details>
<summary>the name tiers at a glance</summary>

| glyph | word | full name |
|---|---|---|
| `∇` | `grad` | `TensorGradient` |
| `Σ` | `sum` | `TensorSum` |
| `Π` | `prod` | `TensorProduct` |
| `μ` | `mean` | `TensorMean` |
| `⌈` | `max` | `TensorMaximum` |
| `⌊` | `min` | `TensorMinimum` |
| `#` | `length` | `TensorLength` |
| `_` | `gather` | `TensorGather` |
| `⍴` | `reshape` | `TensorReshape` |
| `::` | `range` | `TensorRange` |
| `⊗` | `outer` | `TensorOuter` |
| `^` | `pow` | `TensorPower` |
| `√` | `root` | `TensorRoot` |
| `%` | `mod` | `TensorRemainder` |
| `÷` | `div` | `TensorDivide` |
| `×` | `mul` | `TensorMultiply` |
| `~` | `var` | `TensorVariable` |
| `:=` | — | `TensorAssign` |
| `⟳` | `iter` | `FunctionIterate` |
| `⍣` | — | `FunctionPower` |
| `.` | `apply` | `FunctionApply` |
| `@` | `eval` | `FunctionEvaluate` |
| `$` | — | `Reactive` |
| — | `watch` | `TensorWatch` |
| — | `conv` | `TensorConvolution` |
| — | `once` | `SignalOnce` |

Some cells are still empty — the language is young, and names are earned.

</details>

**Signals make it live** — `$(…)` makes a signal; whatever touches it recomputes when it changes:

```clojure
x: $(0.5),
(Slider(x), x ^ 2)
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=eDogJCgwLjUpLAooU2xpZGVyKHgpLCB4IF4gMik)

**Training is a few lines** — `~` makes a trainable variable, an optimizer (`adam`, `adamw`, `sgd`, `adagrad`) minimizes a loss thunk, `⟳` runs it between frames so the UI stays live. This one fits a line to data:

```clojure
x: 0 :: 10,
y: x×0.23 + 0.47,
θ: ~([0, 0]),
𝓛: { mean((x × θ_0 + θ_1 - y)^2) },
opt: adam(0.1),
{ opt(𝓛) } ⟳ 100,
θ
```

[▶&nbsp;run](https://mlajtos.github.io/fluent/?code=eDogMCA6OiAxMCwKeTogeMOXMC4yMyArIDAuNDcsCs64OiB-KFswLCAwXSksCvCdk5s6IHsgbWVhbigoeCDDlyDOuF8wICsgzrhfMSAtIHkpXjIpIH0sCm9wdDogYWRhbSgwLjEpLAp7IG9wdCjwnZObKSB9IOKfsyAxMDAsCs64)

The full tour lives in the playground — [**open the built-in Documentation**](https://mlajtos.github.io/fluent/?code=RG9jdW1lbnRhdGlvbg), or hover any built-in for its card.

## IDE

Live evaluation on every keystroke, hover docs, unicode completion (type `alpha`, get `α`), syntax trees for quoted code, camera and microphone as tensor sources, and LLM code generation — write `;;a bouncing ball;;` and it appears (bring your own Anthropic API key, set via the command palette).

<kbd>Ctrl</kbd>+<kbd>O</kbd> examples · <kbd>Ctrl</kbd>+<kbd>S</kbd> share as URL · <kbd>Ctrl</kbd>+<kbd>P</kbd> commands · <kbd>Ctrl</kbd>+<kbd>Space</kbd> complete — Safari reserves ⌘O, use <kbd>⇧</kbd><kbd>⌘</kbd><kbd>O</kbd> there

## Run locally

```sh
git clone https://github.com/mlajtos/fluent.git
cd fluent && bun install
bun dev   # → http://localhost:3000
```

The whole thing fits in a handful of files — read it, change it:

| file | what |
|---|---|
| [`language.ts`](language.ts) | the language — grammar ([Ohm](https://ohmjs.org)), evaluator, prelude; tensors via [jax-js](https://github.com/ekzhang/jax-js), reactivity via [preact signals](https://preactjs.com/guide/v10/signals/) |
| [`client.tsx`](client.tsx) | the IDE — components, visualizers, Monaco editor, playground |
| [`tests.ts`](tests.ts) | language tests (`bun test ./tests.ts`) |
| [`tests.browser.ts`](tests.browser.ts) | IDE tests in real Chromium (`bun run test:browser`) |

## License

[MIT](LICENSE) © [Milan Lajtoš](https://mlajtos.mu)
