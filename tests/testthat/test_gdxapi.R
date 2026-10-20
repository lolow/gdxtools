library(gdxtools)

# gdxapi.gdx is written through the GDX C API (cgdxdatawritestr +
# gdxSymbolSetDomainX), as done by the Julia make_data scripts of WITCH
# (input/WitchData.jl), instead of gamstransfer. Parameter domains are
# relaxed (ssp, year, iso3), sets use the universe domain, and the records
# hold NA and +/-Inf special values.

context("gdx written by the GDX C API")

g <- gdx("gdxapi.gdx")

test_that("symbols are listed with their types", {
  expect_equal(sort(g$parameters$name), c("p", "sc"))
  expect_equal(g$sets$name, "meta_param")
})

test_that("relaxed domains name the parameter columns", {
  p <- g["p"]
  expect_equal(names(p), c("ssp", "year", "iso3", "value"))
  expect_true(is.character(p$year))
  expect_equal(attributes(p)$gams, "julia param")
})

test_that("special values are mapped to R values", {
  p <- g["p"]
  p <- p[order(p$ssp, p$year), ]
  expect_equal(p$value[1], 1.5)
  expect_true(is.na(p$value[2]))
  expect_equal(p$value[3], Inf)
  expect_equal(p$value[4], -Inf)
})

test_that("universe-domain sets use V1, V2, ... columns", {
  s <- g["meta_param"]
  expect_equal(names(s), c("V1", "V2", "V3"))
  expect_equal(s$V2, c("nagg", "nweight"))
})

test_that("scalars are read as a one-value data.frame", {
  expect_equal(g["sc"]$value, 4.2)
})
