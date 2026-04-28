# Benchmark gdxtools read/write paths against an alternative installation.
#
# Usage:
#   Rscript inst/benchmarks/bench_gdx.R
#
# What it measures (microbenchmark, default 10 iterations):
#   * write.gdx    — full-featured writer (params + sets + variables)
#   * write2.gdx   — params/sets-only fast path
#   * gdx() + extract — open and pull each parameter / variable
#
# The script generates a synthetic GDX in tempdir(); nothing is committed.
#
# Cross-version comparison: install the version of gdxtools you want as a
# baseline (e.g. the legacy gdxrrw-backed 0.7.1) and rerun this script. The
# numbers below are timings for the gamstransfer-backed 1.0.0 only.

suppressPackageStartupMessages({
  library(gdxtools)
  library(microbenchmark)
})

set.seed(1)

# Sizing — comfortably big enough to dwarf per-call overhead, small enough
# to run on a laptop in a few seconds.
n_idx        <- 500L     # |I|
n_idx2       <- 200L     # |J|
n_par_2d     <- 5L       # number of 2-D parameters of size |I|*|J|
n_var_rows   <- 50000L   # rows in the variable's records
sparsity_par <- 0.4      # fraction of |I|*|J| populated in each parameter

idx_i <- sprintf("i%05d", seq_len(n_idx))
idx_j <- sprintf("j%04d", seq_len(n_idx2))

make_param <- function() {
  full <- expand.grid(i = idx_i, j = idx_j, KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  keep <- runif(nrow(full)) < sparsity_par
  data.frame(
    i = full$i[keep],
    j = full$j[keep],
    value = stats::rnorm(sum(keep)),
    stringsAsFactors = FALSE
  )
}

params <- setNames(
  lapply(seq_len(n_par_2d), function(k) make_param()),
  sprintf("p%d", seq_len(n_par_2d))
)
# A scalar and a 1-D parameter for variety
params$scal <- data.frame(value = 42)
params$onev <- data.frame(i = idx_i, value = stats::rnorm(n_idx),
                          stringsAsFactors = FALSE)

sets <- list(
  i_set = data.frame(i = idx_i, stringsAsFactors = FALSE),
  j_set = data.frame(j = idx_j, stringsAsFactors = FALSE)
)

full_grid <- expand.grid(i = idx_i, j = idx_j, KEEP.OUT.ATTRS = FALSE,
                         stringsAsFactors = FALSE)
var_rows <- full_grid[sample.int(nrow(full_grid), n_var_rows), ]
var_rows$value <- stats::rnorm(nrow(var_rows))
rownames(var_rows) <- NULL
vars_l  <- list(v = var_rows)
vars_lo <- list(v = transform(var_rows, value = 0))
vars_up <- list(v = transform(var_rows, value = abs(value) + 1))

bench_dir <- tempdir()
out_full  <- file.path(bench_dir, "bench_full.gdx")
out_fast  <- file.path(bench_dir, "bench_fast.gdx")

# Warm up + prime caches.
write.gdx(out_full, params = params, sets = sets,
          vars_l = vars_l, vars_lo = vars_lo, vars_up = vars_up)
write2.gdx(out_fast, params = params, sets = sets)
g_full <- gdx(out_full)

cat("=== Synthetic GDX summary ===\n")
cat(sprintf("  parameters: %d (avg %.0f rows)\n",
            length(params),
            mean(vapply(params, nrow, numeric(1)))))
cat(sprintf("  sets:       %d\n", length(sets)))
cat(sprintf("  variable v: %d rows\n", nrow(var_rows)))
cat(sprintf("  full gdx:   %.1f KB\n", file.info(out_full)$size / 1024))
cat(sprintf("  fast gdx:   %.1f KB\n\n", file.info(out_fast)$size / 1024))

cat("=== Write benchmarks ===\n")
write_bench <- microbenchmark(
  write.gdx_full = write.gdx(out_full, params = params, sets = sets,
                             vars_l = vars_l, vars_lo = vars_lo,
                             vars_up = vars_up),
  write2.gdx     = write2.gdx(out_fast, params = params, sets = sets),
  write.gdx_compress = write.gdx(out_full, params = params, sets = sets,
                                 compress = TRUE),
  times = 10
)
print(write_bench)

cat("\n=== Read benchmarks ===\n")
read_bench <- microbenchmark(
  open_only       = gdx(out_full),
  extract_param   = g_full["p1"],
  extract_var_lev = g_full["v"],
  extract_var_up  = g_full["v", "up"],
  times = 10
)
print(read_bench)

cat("\n=== Full open + extract-all roundtrip ===\n")
roundtrip <- microbenchmark(
  open_and_extract_all = {
    g <- gdx(out_full)
    for (nm in names(params)) g[nm]
    invisible(g["v"])
  },
  times = 10
)
print(roundtrip)

invisible(file.remove(out_full, out_fast))
