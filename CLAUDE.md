# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`gdxtools` is an R package for reading and writing GDX files (GAMS Database
Exchange format). As of 1.0.0 it is a thin layer over
[`gamstransfer`](https://cran.r-project.org/package=gamstransfer), the GAMS-
maintained GDX I/O package. **No compiled code is shipped** — there is no
`src/` directory and no GAMS install is required for read/write (only for the
optional `gams()` runner).

## Common Commands

```bash
# Install
R CMD INSTALL .

# Run tests
Rscript -e "testthat::test_dir('tests/testthat')"

# Single test file
Rscript -e "testthat::test_file('tests/testthat/test_gdx.R')"

# Rebuild docs from roxygen
Rscript -e "roxygen2::roxygenise()"

# Full package check
R CMD build . && R CMD check --no-manual gdxtools_*.tar.gz

# Benchmarks
Rscript inst/benchmarks/bench_gdx.R              # current backend only
Rscript inst/benchmarks/bench_compare.R          # vs. legacy gdxtools
```

## Architecture

**Three R source files:**

- `R/gdx.R` — the `gdx` S3 class. `gdx(filename)` defaults to **lazy
  mode**: it reads only metadata via `Container$read(file, records=FALSE)`
  and keeps that live container on `.container`. A *second* empty
  `Container` is kept on `.records_container` and accumulates records as
  symbols are accessed. The split matters because `read(symbols=…)` on
  the metadata container is O(symCount) per call; on an empty container
  it's O(1) (~80× faster on a 4500-symbol gdx). Pass `lazy = FALSE` to
  load everything up front; in that mode the two container fields point
  at the same object. `extract.gdx` / `[.gdx` look up records via
  `.records_container` (after auto-loading the symbol on first access);
  `load_records(g, symbols)` exposes the bulk pre-load explicitly.

- `R/gdxtools.R` — `batch_extract()`, `write.gdx()`, `write2.gdx()`. Both
  writers funnel through `.write_container()`, which builds a fresh
  `Container`, auto-registers a universe-domain set for every index column
  whose name is a valid GAMS identifier, and then writes the gdx.

- `R/gdxrrw.R` — backward-compatibility shims for `igdx()` (now a PATH
  discovery helper, no side effects) and `gams()` (now a `system2()`
  wrapper). The legacy gdxrrw-shaped exports `rgdx`, `wgdx`, `gdxInfo`,
  `rgdx.param`, `rgdx.set` and `rgdx.scalar` were dropped in 1.0.0.

**Backward-compat translations to know about:**

- `extract.gdx` renames gamstransfer's `uni` / `uni_<i>` columns
  (universe-domain) to `V1, V2, …`, matching the legacy gdxrrw column naming.
- `field=` in `[.gdx` accepts `l, m, lo, up`; mapped internally to
  `level, marginal, lower, upper`.
- `write.gdx`'s `removeLST`, `usetempdir` and `digits` arguments are accepted
  but ignored (the GAMS-process write path is gone — gamstransfer preserves
  full numeric precision).

**Data convention:** extracted data is a plain `data.frame` with domain
columns + a `value` column (sets have no `value`). The `gams` attribute
carries the GAMS symbol description text.

**Sparse-parameter behavior change (1.0.0):** extracting a parameter no
longer densifies missing entries with explicit zeros — only entries actually
stored in the GDX are returned. Variables and equations are unaffected.
