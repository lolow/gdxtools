# Benchmark results

Reference run on 2026-04-28, Linux x86_64, R 4.x, GAMS 40.3.0.
Workload: 5 sparse 2-D parameters (~28k rows each) + 1 1-D parameter (500 rows)
+ 2 sets (500 / 200 elements) + 1 variable v with 50 000 records.

| operation                                | legacy 0.7.1 | new 1.0.0 | ratio |
|-----------------------------------------:|-------------:|----------:|------:|
| `write2.gdx` (params + sets)             |        93 ms |    356 ms |  0.26× |
| `write.gdx` full (params + sets + vars)  |     18 770 ms |   1 480 ms | **12.7×** |
| `gdx()` (open metadata)                  |       0.3 ms |     38 ms |  0.008× |
| `extract` parameter                       |       7.2 ms |    1.6 ms | **4.4×** |
| `extract` variable level                  |      11.6 ms |    2.3 ms | **5.1×** |

## Interpretation

* **`write.gdx` with variables** is the biggest win. The legacy path generated a
  temporary `.gms` script and ran the `gams` process; the new path writes
  directly through the GDX library.
* **`extract`** is 4-5× faster because the gamstransfer Container caches
  records on first read; legacy code re-issued an `rgdx` C call per item.
* **`gdx()`** is slower because it now eagerly populates the Container with
  records, whereas the legacy `gdxInfo`-based constructor only loaded metadata.
  Net effect on a typical workflow (open once, extract several symbols):
  break-even at ~5 extract calls, faster end-to-end beyond that.
* **`write2.gdx`** (params/sets only) is slower in absolute terms but still
  sub-second on this dataset. Container construction overhead is the cost.

To reproduce, see `bench_compare.R` in this directory.
