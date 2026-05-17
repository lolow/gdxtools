# gdxtools 1.1.0

## Lazy loading by default

* `gdx(filename)` now opens the file in lazy mode by default: only symbol
  metadata (names, dimensions, domains, descriptions) is read up front,
  and each symbol's records are fetched on first access via `[.gdx` /
  `extract()`. Pass `lazy = FALSE` to restore the 1.0.0 behavior (all
  records loaded at open time).
* On large gdx files (e.g. WITCH results with ~4500 symbols), this halves
  the `gdx()` open cost: in our benchmark, ~3.7 s lazy vs. ~6.6 s eager.
  Per-extract cost stays sub-50 ms when records have not been loaded yet,
  and is essentially free on cache hit.
* Internally a *second* `gamstransfer::Container` is kept for record
  reads. Reading symbols into the metadata container is O(symCount) per
  call because gamstransfer revalidates the existing symbol table;
  reading into a dedicated empty container is O(1) — ~10 ms vs. ~800 ms
  per extract on the WITCH gdx.
* `batch_extract()` now performs a single bulk `read()` per file
  (pre-loading all requested items) before iterating, which is one
  C++ round-trip per gdx instead of one per `(item, gdx)` pair.
* New `load_records(gdx, symbols = NULL)` helper: explicitly batch-load
  records when you know up front which symbols you'll need. With
  `symbols = NULL` it loads everything (equivalent to opening eagerly,
  but deferrable).
* `gdx()` now does a single-pass describe via `Container$getSymbols()`
  instead of five separate `list*()` calls followed by per-symbol
  dimension / description vapply — about 2× faster on a gdx with many
  equations.
* `all_items()` now returns the cached name vectors stored on the gdx
  object rather than re-querying the container.

**Behavior note:** in lazy mode, accessing the underlying container
directly via `mygdx$.container[name]$records` returns `NULL` until the
symbol has been extracted at least once. Code that bypasses `extract` /
`[` and reaches into `.container` for records should either call
`load_records()` first or open with `lazy = FALSE`. Domain / class /
dimension queries on `.container` are unaffected.

# gdxtools 1.0.0

## Major change — gamstransfer backend

* The package is now backed by [gamstransfer](https://cran.r-project.org/package=gamstransfer),
  the GDX I/O package maintained by GAMS Software GmbH. No compiled code
  ships in this package any more (`src/` removed).
* The high-level API is preserved: `gdx()`, `[.gdx`, `extract()`,
  `all_items()`, `batch_extract()`, `write.gdx()`, `write2.gdx()`. Existing
  user code calling these should not need changes.
* `write.gdx()` no longer shells out to a GAMS process; the `removeLST`,
  `usetempdir` and `digits` arguments are accepted for compatibility but
  ignored. `compress = TRUE` still produces a compressed GDX.
* `write2.gdx()` is now a thin alias of `write.gdx()` (both use the same
  fast path).
* `igdx()` and `gams()` are kept as thin compatibility shims (PATH-based
  GAMS discovery + `system2` runner). The lower-level gdxrrw-shaped
  exports `rgdx`, `wgdx`, `gdxInfo`, `rgdx.param`, `rgdx.set` and
  `rgdx.scalar` have been removed.
* Behavior change: extracting a sparse parameter no longer densifies the
  result with explicit zeros. Only entries actually stored in the GDX are
  returned. (Variables and equations are unaffected.)

## New options

* `na = c("drop", "keep", "error")` on `write.gdx()` / `write2.gdx()` —
  controls how NA / NaN values in parameter `value` columns are handled.
  Defaults to `"drop"` (legacy v0.7 semantic: silently discarded, GAMS
  reads 0 for those keys). `"keep"` preserves them as GAMS NA / undef;
  `"error"` stops with an informative message.
* `dup = c("first", "last", "error")` on `write.gdx()` / `write2.gdx()` —
  controls duplicate-key collapsing. Defaults to `"first"` (legacy
  `write2.gdx` / `wgdx` semantic, which auto-selected in 95% of v0.7
  calls). `"last"` matches the legacy `write.gdx` GAMS-process path;
  `"error"` stops when duplicates are present. A warning fires for both
  `first` and `last` so dropped rows are always audible.

## Bug fixes

* `write.gdx()` drops default-valued records for free variables
  (level=0, lower=-Inf, upper=+Inf) to match legacy v0.7 semantic
  (`subset(v, value!=0)` for level, `subset(v, !is.infinite(value))`
  for bounds). Downstream tools (notably WITCH's witchtools time-period
  extension) rely on the absence of zero-level records to back-fill
  early years from the first non-default record.
* `gdx()` strips the position suffix gamstransfer adds to relaxed-string
  domain columns (`witch13_3` → `witch13`). Callers look up the bare
  set name (`df$witch13`) and previously fell back silently when the
  suffix was present, which left WITCH parameters un-aggregated.
* `write.gdx()` warns when an index column is double-precision numeric
  with non-integer content. Catches a common scripting bug where a
  stray numeric column (e.g. `low` / `high` left over from a parquet
  read) silently becomes an extra UEL-string dimension. Integer-valued
  doubles (year stored as `1850.0`) are still silent.
* `write.gdx()` collapses duplicate set rows the same way as parameter
  and variable keys.
* `write.gdx()` collapses duplicate index keys with `dup = "first"`
  (default) and warns with the dropped-row count.
* `write.gdx()` writes empty variables as a declaration of the right
  dimension instead of silently dropping them.
* `write.gdx()` handles duplicate index column names (e.g. `(*, *, value)`
  from a data.table) by `make.unique`-ing the records while preserving
  the original domain reference on the symbol.
* `write.gdx()` no longer leaks set column names as separate symbols
  in the output GDX (e.g. an explicit `set r` no longer produced a
  stray `r` parameter).
* `write.gdx()` pre-flight checks for NA values in index columns and
  stops with a clear error instead of letting gamstransfer's C++ layer
  abort the R session with SIGABRT.
* `gdx()` suppresses the gamstransfer acronym warning emitted at load
  time for symbols that use the GAMS Acronym facility.

# gdxtools 0.7.1

* Code cleaning 
* batch_extract allows now for with extra arguments for extract
* Faster write.gdx with variables

# gdxtools 0.7.0

* Includes gdxrrw 1.0.8

# gdxtools 0.6.0

* Faster write gdx when only parameters and/or sets using gdxrrw
* Fast write2 gdx function

# gdxtools 0.5.1

* Can write set with one dimension
* Fix writing with sets containing digits with different size
* Allows for space and dots in set names when writing gdx

# gdxtools 0.4.0

* Includes gdxrrw

# gdxtools 0.3.5

* speed up in extract gdx
* gdx compression by default
* add some tests
* handle empty parameter

# gdxtools 0.3.4

* add variables in write.gdx (deal with .l, .lo, .up)

# gdxtools 0.3.3

* add digits in write.gdx to control the precision

# gdxtools 0.2

* Change license to EPL to comply with gdxrrw
* Fix a lot of bugs on the extraction
* Add many tests


# gdxtools 0.1

* first version
