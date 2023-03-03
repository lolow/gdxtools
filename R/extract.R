#' Extract data from a gdx
#'
#' @export
extract <- function(x, ...) {
    UseMethod("extract", x)
}

#' Extract data from a gdx
#'
#' @author Laurent Drouet
#' @examples
#'
#'   \dontrun{
#'     mygdx <- gdx('results.gdx')
#'     travel_cost <- mygdx["travel_cost"]
#'   }
#' @export
`[.gdx` <- function(x, ...) {
    extract.gdx(x, ...)
}


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
extract.gdx <- function(x, item, field = "l", addgdx = F, ...) {
  if (!getOption("gdxtools.gamstransfer")) {
    return(extract_gdxrrw.gdx(x, item, field, addgdx, ...))
  }
  text = ""
  m <- gamstransfer::ConstContainer$new()
  m$read(x$filename, symbols = item)
  res <- m$data[[item]]$records
  # Is it a set?
  if ("element_text" %in% colnames(res)) {
    res <- subset(res, select=-element_text)
    colnames(res) <- paste0("V",1:length(colnames(res)))
  } else {
    # Is it a variable or an equation?
    if ("level" %in% colnames(res)) {
      var_cols <- c(l = "level", m = "marginal", lo = "lower", up = "upper", sc = "scale")
      res <- res[, !(names(res) %in% var_cols[names(var_cols) != field])]
      if (is.numeric(res)) {
        res <- data.frame(value=res)
      } else {
        colnames(res)[which(colnames(res)==var_cols[field])] <- "value"
      }
    }
    not_value <- which(colnames(res)!="value")
    colnames(res)[not_value] <- gsub("_\\d+$","",colnames(res)[not_value])
  }
  df <- data.frame(res,stringsAsFactors=F)
  if (addgdx){
    if(nrow(df)==0){
      df$gdx = character()
    } else {
      df$gdx = x$filename
    }
  }
  attributes(df) = c(attributes(df), gams = text)
  return(df)
}
