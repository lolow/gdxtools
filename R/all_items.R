#' Return the list of all items
#'
#' @author Laurent Drouet
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
  info = gdxInfo(x$filename, dump = F, returnList = T)
  return(list(variables = info[["variables"]],
              parameters = info[["parameters"]],
              sets = info[["sets"]],
              equations = info[["equations"]]))
}
