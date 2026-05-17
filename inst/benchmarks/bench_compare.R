# Side-by-side benchmark: legacy gdxrrw-backed gdxtools (<= 0.7.1) vs the
# new gamstransfer-backed 1.0.0.
#
# How to run:
#   1. Install the legacy gdxtools into a separate library:
#        mkdir -p /tmp/gdxtools-old-lib
#        git clone -b master <repo> /tmp/gdxtools-old
#        R CMD INSTALL --library=/tmp/gdxtools-old-lib /tmp/gdxtools-old
#   2. Install the current branch into your default library:
#        R CMD INSTALL .
#   3. Rscript inst/benchmarks/bench_compare.R
#
# The script reports microbenchmark timings for both versions on the same
# synthetic GDX so absolute numbers are directly comparable.

library(microbenchmark)

set.seed(1)
n_idx <- 500L; n_idx2 <- 200L
n_par_2d <- 5L; sparsity_par <- 0.4
n_var_rows <- 50000L
idx_i <- sprintf("i%05d", seq_len(n_idx))
idx_j <- sprintf("j%04d", seq_len(n_idx2))

make_param <- function() {
  full <- expand.grid(i = idx_i, j = idx_j, KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  keep <- runif(nrow(full)) < sparsity_par
  data.frame(i = full$i[keep], j = full$j[keep],
             value = stats::rnorm(sum(keep)), stringsAsFactors = FALSE)
}

params <- setNames(lapply(seq_len(n_par_2d), function(k) make_param()),
                   sprintf("p%d", seq_len(n_par_2d)))
params$onev <- data.frame(i = idx_i, value = stats::rnorm(n_idx),
                          stringsAsFactors = FALSE)

sets <- list(i_set = data.frame(i = idx_i, stringsAsFactors = FALSE),
             j_set = data.frame(j = idx_j, stringsAsFactors = FALSE))

full_grid <- expand.grid(i = idx_i, j = idx_j, KEEP.OUT.ATTRS = FALSE,
                         stringsAsFactors = FALSE)
var_rows <- full_grid[sample.int(nrow(full_grid), n_var_rows), ]
var_rows$value <- stats::rnorm(nrow(var_rows))

bench_dir <- tempdir()

old_lib <- "/tmp/gdxtools-old-lib"
if (dir.exists(file.path(old_lib, "gdxtools"))) {
  cat("=== LEGACY (gdxrrw-backed) ===\n")
  library(gdxtools, lib.loc = old_lib)
  if (exists("igdx", where = "package:gdxtools")) {
    suppressMessages(gdxtools::igdx(dirname(Sys.which("gams"))))
  }
  out_old <- file.path(bench_dir, "old.gdx")

  cat("write2.gdx (params + sets):\n")
  print(microbenchmark(
    legacy_write2 = gdxtools::write2.gdx(out_old, params = params, sets = sets),
    times = 5
  ))

  cat("\nwrite.gdx (full, with vars; legacy uses GAMS process — 3 iters):\n")
  print(microbenchmark(
    legacy_write_full = gdxtools::write.gdx(
      out_old, params = params, sets = sets,
      vars_l = list(v = var_rows),
      vars_lo = list(v = transform(var_rows, value = 0)),
      vars_up = list(v = transform(var_rows, value = abs(value) + 1))
    ),
    times = 3
  ))

  cat("\nopen + extract:\n")
  g_old <- gdxtools::gdx(out_old)
  print(microbenchmark(
    legacy_open        = gdxtools::gdx(out_old),
    legacy_extract     = g_old["p1"],
    legacy_extract_var = g_old["v"],
    times = 5
  ))

  detach("package:gdxtools", unload = TRUE)
} else {
  cat("Skipping legacy benchmark — no install at ", old_lib, "\n", sep = "")
}

cat("\n\n=== NEW (gamstransfer-backed) ===\n")
library(gdxtools)
out_new <- file.path(bench_dir, "new.gdx")

cat("write2.gdx (params + sets):\n")
print(microbenchmark(
  new_write2 = write2.gdx(out_new, params = params, sets = sets),
  times = 5
))

cat("\nwrite.gdx (full, with vars):\n")
print(microbenchmark(
  new_write_full = write.gdx(
    out_new, params = params, sets = sets,
    vars_l = list(v = var_rows),
    vars_lo = list(v = transform(var_rows, value = 0)),
    vars_up = list(v = transform(var_rows, value = abs(value) + 1))
  ),
  times = 5
))

cat("\nopen + extract:\n")
g_new <- gdx(out_new)
print(microbenchmark(
  new_open        = gdx(out_new),
  new_extract     = g_new["p1"],
  new_extract_var = g_new["v"],
  times = 5
))
