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
