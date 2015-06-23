library("gdxtools")

context("gdx manipulation")

test_that("define a gdx", {
  expect_match(gdx('ampl.gdx'),"gdx")
  expect_warning(gdx('not_existing.gdx'))
})

g = gdx('ampl.gdx')

test_that("get information", {
  expect_equal(all_items(g)$variables,c("x","s","profit"))
  expect_equal(all_items(g)$equations,c("limit","balance","obj"))
})
