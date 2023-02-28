#' GDX file
#'
#' @param filename filename of the gdx file
#' @examples
#'
#'   \dontrun{
#'     mygdx <- gdx('results.gdx')
#'   }
gdx_gdxrrw <- function(filename, ...) {
  if(file.exists(filename)){
    info = gdxInfo(filename, dump = F, returnDF = T)
    return(structure(list(filename = filename,
                          sets = info$sets,
                          parameters = info$parameters,
                          variables = info$variables,
                          equations = info$equations,
                          aliases = info$aliases,
                          symCount = info$symCount,
                          ...), class = "gdx"))
  } else{
    warning(paste(filename, "does not exist!"))
    return(structure(list(filename = filename, ...), class = "gdx"))
  }
}
