#' gdxtools
#'
#' @name gdxtools
#' @docType package
#' @import gdxrrw
#'
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

#' Write a list of parameters in a gdx
#'
#' @export
#' @param file the filename of the gdx to save
#' @param params named list of parameters
#' @param usetempdir uses system temp dir for the temporary files, otherwise use local file "tmp.gms"
#' @param removeLST remove temporary lst file
#' @examples
#'  \dontrun{
#'     param1 = data.frame(x=c('1','2'),value=1:10)
#'     param2 = data.frame(a=c('london','paris','tahiti'),value=c(50,0.2,1e-2))
#'     write.gdx("test.gdx",list(param1=param1,param2=param2))
#'  }
#'
write.gdx <- function(file, params=list(), removeLST=T, usetempdir=T){
  # Create a temporary gams file
  if(usetempdir){
    gms = tempfile(pattern = "wgdx", fileext = ".gms")
    lst = tempfile(pattern = "wgdx", fileext = ".lst")
  } else {
    gms = "tmp.gms"
    lst = "tmp.lst"
  }
  fgms = file(gms, "w")
  writeLines("$offdigit", fgms)
  # collect and write sets
  allsets = unique(unlist(lapply(params, names)))
  allsets = subset(allsets, !allsets %in% c('*','value'))
  for(i in seq_along(allsets)){
    s = allsets[i]
    writeLines(paste("set", s, "/"), fgms)
    lvalues = lapply(params, function(x) unique((as.character(x[[s]]))))
    values = unique(unlist(lvalues))
    writeLines(values, fgms)
    writeLines("/;", fgms)
  }
  # Write parameters
  for(i in seq_along(params)){
    p = params[[i]]
    text = ifelse("gams" %in% names(attributes(p)),attributes(p)$gams,"")
    if(length(colnames(p))==1){
      writeLines(paste("scalar", names(params)[i], " '", text, "' /", as.numeric(p[1]), "/;"), fgms)
    } else {
      indices = subset(colnames(p), colnames(p) != "value")
      writeLines(paste0("parameter ", names(params)[i],
                        "(", paste(indices, collapse=","), ") ", " '", text, "' /"), fgms)
      concatenate <- function(row, len) paste(paste(row[1:len],collapse="."),row[len+1])
      writeLines(apply(p,1,concatenate, len=length(indices)), fgms)
      writeLines("/;", fgms)
    }
  }
  # save into a gdx
  writeLines(paste0('execute_unload "',file,'"\n',paste(names(params),collapse="\n"),"\n;"), fgms)
  close(fgms)
  res = gams(paste0(gms," output=",lst))
  if(res!=0) stop(paste("write gdx failed -",gms))
  if(removeLST) file.remove(lst)
}

