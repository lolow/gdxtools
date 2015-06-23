#' gdxtools
#'
#' @name gdxtools
#' @docType package
#' @import gdxrrw
NULL

.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()
  packageStartupMessage(paste("gdxtools",utils::packageVersion("gdxtools")))
  # try to specify the gams installation
  igdx(dirname(Sys.which("gams")))
}
