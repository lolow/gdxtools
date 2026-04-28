library(gdxtools)

# Sort a data.frame by all non-`value`/`gdx` columns. Used to compare
# results without depending on internal row order, which is a backend choice.
sort_by_keys <- function(df) {
  if (nrow(df) == 0) return(df)
  key_cols <- setdiff(names(df), c("value", "gdx"))
  if (length(key_cols) == 0) return(df)
  df[do.call(order, lapply(key_cols, function(k) df[[k]])), , drop = FALSE]
}

context("gdx reading")

test_that("gdx() warns on missing file but returns an object", {
  expect_warning(g <- gdx("not_existing.gdx"))
  expect_s3_class(g, "gdx")
})

g <- gdx("ampl.gdx")

test_that("metadata is exposed via the gdx object", {
  expect_equal(g$symCount, 18)
  expect_equal(sort(g$variables$name),  sort(c("x", "s", "profit")))
  expect_equal(sort(g$equations$name),  sort(c("limit", "balance", "obj")))
  expect_equal(sort(g$parameters$name), sort(c("b", "d", "f", "m", "a", "c")))
  expect_equal(sort(g$sets$name),
               sort(c("p", "r", "tl", "t", "first", "last")))
})

test_that("all_items() returns lists of names by symbol type", {
  ai <- all_items(g)
  expect_equal(ai$variables,  c("x", "s", "profit"))
  expect_equal(ai$equations,  c("limit", "balance", "obj"))
  expect_equal(ai$sets,       c("p", "r", "tl", "t", "first", "last"))
  expect_equal(ai$parameters, c("b", "d", "f", "m", "a", "c"))
})

test_that("variable extraction returns domain columns + `value`", {
  x <- g["x"]
  expect_true(is.character(x$p))
  expect_true(is.character(x$tl))
  expect_true(is.numeric(x$value))
  expect_equal(sort(unique(x$p)), sort(c("nuts", "bolts", "washers")))

  expect_true(is.numeric(g["x", "m"]$value))
  expect_equal(sum(g["x", "lo"]$value), 0)
  expect_true(is.infinite(g["x", "up"]$value[1]))
})

test_that("equation extraction supports field=l/m/lo/up", {
  expect_true(is.numeric(g["limit"]$value))
  expect_true(is.infinite(g["limit", "lo"]$value[1]))
  expect_equal(g["limit", "up"]$value[1], 123)
})

test_that("invalid field raises an error", {
  expect_error(g["x", "bogus"], "invalid field")
})

test_that("sets return as character columns and no `value`", {
  pset <- g["p"]
  expect_true(is.character(pset[, 1]))
  expect_false("value" %in% names(pset))
})

test_that("parameter extraction returns named domain + `value`", {
  expect_true(is.numeric(g["b"]$value))
  expect_equal(names(g["b"]), c("r", "value"))
})

test_that("scalar parameters round-trip as a 1-row data.frame", {
  m <- g["m"]
  expect_equal(names(m), "value")
  expect_equal(m$value, 123)
})

test_that("missing item triggers a warning and returns NULL", {
  expect_warning(res <- g["does_not_exist"])
  expect_null(res)
})

test_that("addgdx tags rows with the source filename", {
  b_tagged <- g["b", addgdx = TRUE]
  expect_true("gdx" %in% names(b_tagged))
  expect_equal(unique(b_tagged$gdx), "ampl.gdx")
})

context("gdx writing")

# Round-trip the parameters from the test gdx and confirm key/value parity.
params_in <- list(
  b = g["b"], c = g["c"], d = g["d"],
  e = data.frame(id = c("with space", "with.dot"), value = c(133, 233))
)
write.gdx("out_param.gdx", params = params_in)
gp <- gdx("out_param.gdx")

test_that("parameters round-trip", {
  expect_equal(sort_by_keys(gp["b"]), sort_by_keys(g["b"]),
               ignore_attr = TRUE)
  expect_equal(sort_by_keys(gp["c"])$value, sort_by_keys(g["c"])$value)
  expect_equal(sort_by_keys(gp["d"]), sort_by_keys(g["d"]),
               ignore_attr = TRUE)
})

test_that("parameter domain values can contain spaces and dots", {
  e_back <- gp["e"]
  expect_equal(subset(e_back, get(names(e_back)[1]) == "with space")$value, 133)
  expect_equal(subset(e_back, get(names(e_back)[1]) == "with.dot")$value, 233)
})

# Round-trip variables (level + lower + upper).
vars       <- list(x = g["x"], s = g["s"], profit = g["profit"])
vars_lower <- list(x = g["x", "lo"], s = g["s", "lo"], profit = g["profit", "lo"])
vars_upper <- list(x = g["x", "up"], s = g["s", "up"], profit = g["profit", "up"])
write.gdx("out_var.gdx",
          params = params_in,
          vars_l = vars, vars_lo = vars_lower, vars_up = vars_upper)
gv <- gdx("out_var.gdx")

test_that("variables round-trip across level/lower/upper", {
  for (v in c("x", "s", "profit")) {
    nz_old <- subset(g[v], value != 0)$value
    nz_new <- subset(gv[v], value != 0)$value
    expect_equal(sort(nz_new), sort(nz_old), info = paste("level of", v))
    expect_equal(sort(gv[v, "lo"]$value), sort(g[v, "lo"]$value),
                 info = paste("lower of", v))
    expect_equal(sort(gv[v, "up"]$value), sort(g[v, "up"]$value),
                 info = paste("upper of", v))
  }
})

test_that("compressed write produces a readable gdx", {
  write.gdx("out_compressed.gdx", params = params_in, compress = TRUE)
  gc_ <- gdx("out_compressed.gdx")
  expect_equal(sort_by_keys(gc_["b"])$value, sort_by_keys(g["b"])$value)
  file.remove("out_compressed.gdx")
})

test_that("write2.gdx is a fast path equivalent of write.gdx for params/sets", {
  write2.gdx("out_w2.gdx", params = params_in)
  gw2 <- gdx("out_w2.gdx")
  expect_equal(sort_by_keys(gw2["b"])$value, sort_by_keys(g["b"])$value)
  expect_equal(sort_by_keys(gw2["c"])$value, sort_by_keys(g["c"])$value)
  file.remove("out_w2.gdx")
})

test_that("scalar parameters can be written and re-read", {
  write.gdx("out_scalar.gdx",
            params = list(s1 = data.frame(value = 42)))
  gs <- gdx("out_scalar.gdx")
  expect_equal(gs["s1"]$value, 42)
  file.remove("out_scalar.gdx")
})

test_that("write with explicit sets succeeds", {
  city <- data.frame(uni = c("london", "paris", "tahiti"))
  road <- data.frame(a = c("london", "paris", "tahiti"),
                     b = c("tahiti", "tahiti", "paris"))
  write.gdx("out_sets.gdx", sets = list(city = city, road = road))
  expect_true(file.exists("out_sets.gdx"))
  gs <- gdx("out_sets.gdx")
  expect_true("city" %in% gs$sets$name)
  expect_true("road" %in% gs$sets$name)
  expect_equal(nrow(gs["city"]), 3)
  expect_equal(nrow(gs["road"]), 3)
  file.remove("out_sets.gdx")
})

test_that("set column names do not leak as separate symbols (README example 2)", {
  myset1 <- data.frame(a = c("london", "paris", "tahiti"))
  myset2 <- data.frame(a = c("london", "paris", "tahiti"),
                       b = c("tahiti", "tahiti", "paris"))
  write.gdx("out_readme.gdx", sets = list(city = myset1, road = myset2))
  g <- gdx("out_readme.gdx")
  # Only the user's two sets — no `a` or `b` symbol leaked.
  expect_equal(sort(g$sets$name), c("city", "road"))
  # Universe domain → V1 / V2 column naming on round-trip.
  expect_equal(names(g["city"]), "V1")
  expect_equal(names(g["road"]), c("V1", "V2"))
  file.remove("out_readme.gdx")
})

test_that("write.gdx accepts duplicate `*` column names (incl. empty data)", {
  # data.tables happily allow duplicate column names; an empty 2-D parameter
  # with two "*" columns and one "value" column previously failed with
  # 'Dimensionality of records 2 is inconsistent with parameter domain
  # specification 1' because setdiff() collapsed the duplicates.
  empty_dup <- data.frame(check.names = FALSE,
                          a = character(0), b = character(0),
                          value = numeric(0))
  names(empty_dup) <- c("*", "*", "value")
  write.gdx("out_dup_empty.gdx", params = list(carbonprice = empty_dup))
  g <- gdx("out_dup_empty.gdx")
  expect_equal(g$parameters$dim[g$parameters$name == "carbonprice"], 2L)
  expect_equal(nrow(g["carbonprice"]), 0L)
  file.remove("out_dup_empty.gdx")

  full_dup <- data.frame(check.names = FALSE,
                         a = c("x", "y"), b = c("u", "v"),
                         value = c(1, 2))
  names(full_dup) <- c("*", "*", "value")
  write.gdx("out_dup_full.gdx", params = list(p = full_dup))
  g2 <- gdx("out_dup_full.gdx")
  expect_equal(g2$parameters$dim[g2$parameters$name == "p"], 2L)
  expect_equal(sort(g2["p"]$value), c(1, 2))
  file.remove("out_dup_full.gdx")
})

test_that("parameter column name becomes a relaxed domain reference", {
  # Mirrors the legacy GAMS-process output: `parameter b(r) /.../;` produces a
  # GDX where b's domain is named "r" without a separate set "r" symbol.
  b_in <- data.frame(r = c("iron", "nickel"), value = c(35.8, 7.32))
  write.gdx("out_relax.gdx", params = list(b = b_in))
  g <- gdx("out_relax.gdx")
  expect_equal(g$parameters$name, "b")
  expect_equal(nrow(g$sets), 0)               # no leaked "r" set
  expect_equal(names(g["b"]), c("r", "value"))
  file.remove("out_relax.gdx")
})

test_that("legacy `data.frame(`*`=...)` set syntax still produces a gdx", {
  # historical workaround in user code: column name `*` becomes `X.` in R
  myset1 <- data.frame(`*` = c("london", "paris", "tahiti"))
  myset2 <- data.frame(`*` = c("london", "paris", "tahiti"),
                       b   = c("tahiti", "tahiti", "paris"))
  expect_silent(
    write.gdx("out_legacy_sets.gdx",
              sets = list(city = myset1, road = myset2))
  )
  expect_true(file.exists("out_legacy_sets.gdx"))
  file.remove("out_legacy_sets.gdx")
})

test_that("batch_extract concatenates rows across files", {
  res <- batch_extract("b", files = c("ampl.gdx", "out_param.gdx"))
  expect_true("b" %in% names(res))
  expect_true("gdx" %in% names(res$b))
  expect_equal(sort(unique(res$b$gdx)), sort(c("ampl.gdx", "out_param.gdx")))
  expect_equal(nrow(res$b), 2 * nrow(g["b"]))
})

test_that("write.gdx preserves the GAMS description text", {
  p <- data.frame(x = c("a", "b"), value = c(1, 2))
  attr(p, "gams") <- "an annotated parameter"
  write.gdx("out_desc.gdx", params = list(myp = p))
  gd <- gdx("out_desc.gdx")
  expect_equal(gd$parameters$text[gd$parameters$name == "myp"],
               "an annotated parameter")
  file.remove("out_desc.gdx")
})

test_that("NA / NaN values are preserved (mapped to GAMS NA)", {
  p <- data.frame(i = c("a", "b", "c", "d"),
                  value = c(1, NA, NaN, 4))
  expect_silent(write.gdx("out_na_val.gdx", params = list(p = p)))
  g <- gdx("out_na_val.gdx")
  out <- g["p"][order(g["p"]$i), ]
  expect_equal(out$i, c("a", "b", "c", "d"))
  expect_equal(out$value[1], 1)
  expect_equal(out$value[4], 4)
  expect_true(is.na(out$value[2]) || is.nan(out$value[2]))
  expect_true(is.na(out$value[3]) || is.nan(out$value[3]))
  file.remove("out_na_val.gdx")
})

test_that("zero values are still dropped (legacy semantic)", {
  p <- data.frame(i = c("a", "b", "c"), value = c(0, 1, 0))
  write.gdx("out_zero.gdx", params = list(p = p))
  g <- gdx("out_zero.gdx")
  expect_equal(g["p"]$i, "b")
  expect_equal(g["p"]$value, 1)
  file.remove("out_zero.gdx")
})

test_that("NA in an index column raises an informative error (no SIGABRT)", {
  # gamstransfer's C++ layer aborts the R process with SIGABRT when an index
  # value is NA — guard against that with a pre-flight check.
  bad_par <- data.frame(i = c("a", NA, "c"), value = c(1, 2, 3))
  expect_error(
    write.gdx("out_na_par.gdx", params = list(p = bad_par)),
    "NA values"
  )

  bad_var <- data.frame(i = c("a", "b", NA), value = c(1, 2, 3))
  expect_error(
    write.gdx("out_na_var.gdx", vars_l = list(v = bad_var)),
    "NA values"
  )

  bad_set <- data.frame(i = c("a", NA))
  expect_error(
    write.gdx("out_na_set.gdx", sets = list(s = bad_set)),
    "NA values"
  )
})

test_that("empty variables are written as a declaration of the right dim", {
  # Empty input previously got silently dropped; legacy declared the
  # variable in the GDX even when no records were provided.
  empty_v <- data.frame(check.names = FALSE,
                        a = character(0), b = character(0),
                        value = numeric(0))
  names(empty_v) <- c("*", "*", "value")
  write.gdx("out_empty_var.gdx", vars_l = list(CPRICE = empty_v))
  g <- gdx("out_empty_var.gdx")
  expect_true("CPRICE" %in% g$variables$name)
  expect_equal(g$variables$dim[g$variables$name == "CPRICE"], 2L)
  expect_equal(nrow(g["CPRICE"]), 0L)
  file.remove("out_empty_var.gdx")
})

test_that("write.gdx accepts +Inf upper bounds", {
  vu <- data.frame(i = c("a", "b"), value = c(Inf, Inf))
  vl <- data.frame(i = c("a", "b"), value = c(0, 0))
  vlev <- data.frame(i = c("a", "b"), value = c(1, 2))
  write.gdx("out_inf.gdx",
            vars_l = list(v = vlev), vars_lo = list(v = vl),
            vars_up = list(v = vu))
  gi <- gdx("out_inf.gdx")
  ups <- sort(gi["v", "up"]$value)
  expect_true(all(is.infinite(ups)))
  file.remove("out_inf.gdx")
})

# Cleanup
for (f in c("out_param.gdx", "out_var.gdx")) if (file.exists(f)) file.remove(f)
