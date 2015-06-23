library("gdxtools")
library("gdxrrw")
suppressMessages(igdx(dirname(Sys.which("gams"))))

context("gdxrrw dependencies")

test_that("gdxrrw is loaded and well configured", {
  expect_true(exists("igdx"))
  expect_equal(gdxrrw::igdx(),1)
})
