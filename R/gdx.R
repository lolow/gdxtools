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
                          sets = data.frame(name = m$listSets()),
                          parameters = data.frame(name = m$listParameters()),
                          variables = data.frame(name = m$listVariables()),
                          equations = data.frame(name = m$listEquations()),
                          aliases = data.frame(name = m$listAliases()),
                          symCount = length(m$listSymbols()),
                          container = m,
                          ...), class = "gdx"))
  }else{
    warning(paste(filename,"does not exist!"))
    return(structure(list(filename = filename, ...), class = "gdx"))
  }
}

#' @export
print.gdx <- function(x, ...) {
    cat("<gdx: ",
        x$filename,
        ", ",
        x$symCount,
        " symbol",
        ifelse(x$symCount>1,"s",""),
        ", ",
        ifelse("container" %in% names(x), "GAMS transfert R", "gdxrrw"),
        ">\n",
        sep = "")
}
