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
