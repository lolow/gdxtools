#' GDX file
#'
#' @param filename filename of the gdx file
#' @examples
#'
#'   \dontrun{
#'     mygdx <- gdx('results.gdx')
#'   }
#' @export
gdx <- function(filename, ...) {
  if (!getOption("gdxtools.gamstransfer")) {
    return(gdx_gdxrrw(filename, ...))
  }
  if(file.exists(filename)){
    m <- gamstransfer::ConstContainer$new()
    m$read(filename, records = FALSE)
    return(structure(list(filename = filename,
                          sets = m$listSets(),
                          parameters = m$listParameters(),
                          variables = m$listVariables(),
                          equations = m$listEquations(),
                          aliases = m$listAliases(),
                          symCount = length(unique(c(m$listParameters(),
                                                     m$listVariables(),
                                                     m$listEquations(),
                                                     m$listAliases()))),
                          container = m,
                          ...), class = "gdx"))
  }else{
    warning(paste(filename,"does not exist!"))
    return(structure(list(filename = filename, ...), class = "gdx"))
  }
}

#' @export
print.gdx <- function(x, ...) {
    cat("<gdx: ", x$filename, ", ", x$symCount," symbol",ifelse(x$symCount>1,"s",""),">\n", sep = "")
}
