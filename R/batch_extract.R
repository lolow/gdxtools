#' Extract a list of items from many GDX
#'
#' @param items vector or list of items to extract
#' @param files list of files, if not defined gdxs should be defined
#' @param gdxs list of gdxs, if not defined files should be defined
#' @author Laurent Drouet
#' @examples
#'
#'   \dontrun{
#'     myfiles = c("test1.gdx","test2.gdx")
#'     allparam <- batch_extract("myparam",files=myfiles)
#'   }
#' @export
batch_extract <- function(items,files=NULL,gdxs=NULL,...){
  if(is.null(gdxs)){
    gdxs <- lapply(files, gdx)
  }
  lall = list()
  for(item in items){
    tt <- lapply(gdxs,extract,item,addgdx=T,...)
    tt <- do.call("rbind",tt)
    tt <- list(tt)
    names(tt) <- item
    lall <- c(lall,tt)
  }
  return(lall)
}
