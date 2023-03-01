#' Extract parameter or variable data from the gdx file
#'
#' @export
#' @author Laurent Drouet
#' @param x the gdx object
#' @param item the name of the item to extract
#' @param field the field of the variable to be extracted. Can be 'l', 'm', 'lo'
#'   'up', respectively for level, marginal, lower bound, upper bound.
#'   Defaults to level.
#' @param addgdx if \code{TRUE}, the data.frame will get an extra column
#'   with the filename of the gdx.
#' @examples
#'  \dontrun{
#'     mygdx <- gdx('results.gdx')
#'     travel_cost <- mygdx["travel_cost"]
#'     travel_cost <- extract(mygdx,"travel_cost")
#'  }
#'
extract_gdxrrw.gdx <- function(x, item, field = "l", addgdx = F, ...) {
  text = ""
  if(item %in% x$variables$name){
    res = rgdx(x$filename, list(name = item, field = field), squeeze = F)
    #TODO text is not existing
    #if("text" %in% colnames(x$variables)) text = x$variables$text[item==x$variables$name]
  } else if(item %in% x$equations$name){
    res = rgdx(x$filename, list(name = item, field = field), squeeze = F)
    if("text" %in% colnames(x$equations)) text = x$equations$text[item==x$equations$name]
  } else if(item %in% x$parameters$name){
    res = rgdx(x$filename, list(name = item), squeeze = F)
    if("text" %in% colnames(x$parameters)) text = x$parameters$text[item==x$parameters$name]
  } else if(item %in% x$sets$name){
    res = rgdx(x$filename, list(name = item), squeeze = F)
    if("text" %in% colnames(x$sets)) text = x$sets$text[item==x$sets$name]
  } else {
    warning("item not found")
    return(NULL)
  }
  if (res$dim == 0) {
    df = data.frame(value=res$val[,res$dim+1])
  } else {
    ldf = list()
    for (i in 1:res$dim) {
      if (res$domains[i] == "*") {
        colname = paste("V", i, sep = "")
      } else {
        colname = res$domains[i]
      }
      l = list(res$uels[[i]][res$val[, i]])
      names(l) = colname
      ldf = c(ldf,l)
    }
    df = data.frame(ldf,stringsAsFactors=F)
    if(!res$type %in% c("set")){
      df$value = res$val[, res$dim + 1]
    }
  }
  if (addgdx){
    if(nrow(df)==0){
      df$gdx = character()
    } else {
      df$gdx = x$filename
    }
  }
  attributes(df) = c(attributes(df),gams=text)
  return(df)
}
