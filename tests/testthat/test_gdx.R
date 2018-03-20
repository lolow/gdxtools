library(gdxtools)
igdx(dirname(Sys.which('gams')))

context("gdx reading")

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

context("gdx writing")

params = list(b=g["b"],c=g["c"],d=g["d"],
              e=data.frame(id=c("with space","with.dot"),value=c(133,233)))
write.gdx("out_param.gdx",params=params)
gp = gdx('out_param.gdx')

vars = list(x=g["x"],s=g["s"],profit=g["profit"])
vars_lower = list(x=g["x",field="lo"],s=g["s","lo"],profit=g["profit","lo"])
vars_upper = list(x=g["x",field="up"],s=g["s","up"],profit=g["profit","up"])
write.gdx("out_var.gdx",
          params=params, vars_l=vars,
          vars_lo = vars_lower,
          vars_up = vars_upper)
gv = gdx('out_var.gdx')

test_that("write_gdx", {
  expect_equal(gp["b"],g["b"])
  expect_equal(gp["c"]$value,g["c"][order(g["c"]$p),]$value)
  expect_equal(gp["d"],g["d"])
  expect_equal(gv["b"],g["b"])
  expect_equal(gv["c"],g["c"])
  expect_equal(gv["d"],g["d"])
  expect_equal(subset(gv["x"],value!=0)$value,subset(g["x"],value!=0)$value)
  expect_equal(subset(gv["s"],value!=0)$value,subset(g["s"],value!=0)$value)
  expect_equal(subset(gv["profit"],value!=0)$value,subset(g["profit"],value!=0)$value)
  expect_equal(gv["x","lo"]$value,g["x","lo"]$value)
  expect_equal(gv["x","up"]$value,g["x","up"]$value)
  expect_equal(gv["s","lo"]$value,g["s","lo"]$value)
  expect_equal(gv["s","up"]$value,g["s","up"]$value)
  expect_equal(gv["profit","lo"]$value,g["profit","lo"]$value)
  expect_equal(gv["profit","up"]$value,g["profit","up"]$value)
  expect_equal(subset(gv["e"],id=="with space")$value,133)
  expect_equal(subset(gv["e"],id=="with.dot")$value,233)
})

file.remove("out_param.gdx")
file.remove("out_var.gdx")

test_that("write_gdx different digits", {

  param1 = data.frame(x=c(1,2,22),value=1:3)
  params = list(param1=param1)
  write.gdx(basename("test.gdx"), params = params)

  expect_true(file.exists("test.gdx"))

})

file.remove("test.gdx")

test_that("write_gdx sets", {

myset1 = data.frame(`*`=c('london','paris','tahiti'))
myset2 = data.frame(`*`=c('london','paris','tahiti'),b=c('tahiti','tahiti','paris'))
write.gdx("test.gdx",sets=list(city=myset1,road=myset2))

expect_true(file.exists("test.gdx"))

})

file.remove("test.gdx")
