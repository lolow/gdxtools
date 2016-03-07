
#' GDX file
#'
#' @export
#'
#' @param filename filename of the gdx file
gdx <- function(filename, ...) {
  if(!file.exists(filename)) warning(paste(filename,"does not exist!"))
  structure(list(filename = filename, ...), class = "gdx")
}

#' @export
print.gdx <- function(x, ...) {
    cat("<gdx: ", x$filename, ">\n", sep = "")
}

#' Extract information
#'
#' @export
extract <- function(x, ...) {
    UseMethod("extract", x)
}

#' Wrap extract.gdx
#'
#' @export
`[.gdx` <- function(x, ...) {
    extract.gdx(x, ...)
}


#' Extract parameter or variable data from the gdx file
#'
#' @export
#' @param x the gdx object
#' @param item the name of the item to extract
#' @param field the field of the variable to be extracted. Can be 'l', 'm', 'lo'
#'   'up', respectively for level, marginal, lower bound, upper bound.
#'   Defaults to level.
#' @param addgdxname if \code{TRUE}, the data.frame will get an extra column
#'   with the filename of the gdx.
#' @examples
#'  \dontrun{
#'     mygdx <- gdx('results.gdx')
#'     travel_cost <- mygdx["travel_cost"]
#'     travel_cost <- extract(mygdx,"travel_cost")
#'  }
#'
extract.gdx <- function(x, item, field = "l", addgdxname = F, ...) {
  # mem.all_items = memoise(all_items.gdx) allitems = mem.all_items(x)
  allitems = all_items(x)
  if (item %in% c(allitems[["variables"]],allitems[["equations"]])) {
    res = gdxrrw::rgdx(x$filename, list(name = item, field = field), squeeze = F)
  } else {
    res = gdxrrw::rgdx(x$filename, list(name = item), squeeze = F)
  }
  if (res$dim == 0) {
    return(data.frame(value=res$val[,res$dim+1]))
  }
  ldf = list()
  for (i in 1:res$dim) {
    if (res$domains[i] == "*") {
      colname = paste("V", i, sep = "")
    } else {
      colname = res$domains[i]
    }
    l = factor(res$val[, i])
    labidx = as.numeric(levels(l))
    levels(l) = res$uels[[i]][labidx]
    l = list(as.character(l))
    names(l) = colname
    ldf = c(ldf,l)
  }
  df = data.frame(ldf,stringsAsFactors=F)
  if(!res$type %in% c("set")){
    df$value = res$val[, res$dim + 1]
  }
  if (addgdxname){
    df$gdx = x$filename
  }
  return(df)
}

#' Return the list of all items
#'
#' @export
all_items <- function(x, ...) {
    UseMethod("all_items", x)
}

#' Return the list of all items
#'
#' @export
#' @param x the gdx object
#' @examples
#'  \dontrun{
#'     mygdx <- gdx('results.gdx')
#'     all_items(mygdx)
#'  }
#'
all_items.gdx <- function(x, ...) {
    info = gdxrrw::gdxInfo(x$filename, dump = F, returnList = T)
    return(list(variables = info[["variables"]],
      parameters = info[["parameters"]],
      sets = info[["sets"]],
      equations = info[["equations"]]))
}
