library("gdxtools")
library("gdxrrw")
suppressMessages(igdx(dirname(Sys.which("gams"))))

context("gdx manipulation")

test_that("define a gdx", {
  expect_match(gdx('ampl.gdx'),"gdx")

})
