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
  return(list(variables = x$variables$name,
              parameters = x$parameters$name,
              sets = x$sets$name,
              equations = x$equations$name))
}
