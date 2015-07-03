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

#' Extract a list of items from many GDX
#'
#' @export
#' @param items vector or list of items to extract
#' @param files list of files, if not defined gdxs should be defined
#' @param gdxs list of gdxs, if not defined files should be defined
batch_extract <- function(items,files=NULL,gdxs=NULL){
  if(is.null(gdxs)){
    gdxs = lapply(files, gdx)
  }
  lall = list()
  for(item in items){
    tt = lapply(gdxlist,extract,item,addgdx=T)
    tt = do.call("rbind",tt)
    tt = list(tt)
    names(tt) <- item
    lall <- c(lall,tt)
  }
  return(lall)
}
