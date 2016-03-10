library("gdxtools")

context("gdx manipulation")

test_that("define a gdx", {
  #expect_match(gdx('ampl.gdx'),"gdx")
  expect_warning(gdx('not_existing.gdx'))
})

g = gdx('ampl.gdx')

test_that("get information", {
  expect_equal(all_items(g)$variables,c("x","s","profit"))
  expect_equal(all_items(g)$equations,c("limit","balance","obj"))
  expect_equal(all_items(g)$sets,c("p","r","tl","t","first","last"))
  expect_equal(all_items(g)$parameters,c("b","d","f","m","a","c"))
})

test_that("get data on variable", {
  expect_true(is.numeric(g["x"]$value))
  expect_true(is.character(g["x"]$tl))
  expect_equal(unique(g["x"]$p),c("nuts","bolts","washers"))
  expect_true(is.numeric(g["x","m"]$value))
  expect_equal(sum(g["x","lo"]$value),0)
  expect_true(is.infinite(g["x","up"]$value[1]))
})

test_that("get data on equations", {
  expect_true(is.numeric(g["limit"]$value))
  expect_true(is.infinite(g["limit","lo"]$value[1]))
  expect_equal(g["limit","up"]$value[1],123)
})

test_that("get data on sets", {
  expect_true(is.character(g["p"][,1]))
})

test_that("get data on parameters", {
  expect_true(is.numeric(g["b"]$value))
  expect_equal(names(g["b"]),c("r","value"))
})
