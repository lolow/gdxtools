library(gdxtools)

context("igdx")

test_that("igdx() is deprecated but still returns the sysdir", {
  expect_warning(d <- igdx("/opt/gams", silent = TRUE, returnStr = TRUE),
                 "deprecated")
  expect_equal(d, "/opt/gams")
})
