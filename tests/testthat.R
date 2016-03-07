if(!require(testthat)){
  install.packages("testthat", repos="http://cran.r-project.org")
  library(testthat)
}

library(gdxtools)

test_check("gdxtools")
