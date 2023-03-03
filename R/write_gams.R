#' Write a list of parameters in a gdx (old version which uses a temporary gams file)
#'
#' @param file the filename of the gdx to save
#' @param params named list of parameters
#' @param sets named list of sets
#' @param vars_l named list of variable levels
#' @param vars_lo named list of variable lower bounds
#' @param vars_up named list of variable upper bounds
#' @param usetempdir uses system temp dir for the temporary files, otherwise use local file "tmp.gms"
#' @param removeLST remove temporary lst file
#' @param digit number of digits to use
#' @param compress compress GDX
#' @author Laurent Drouet
#' @examples
#'  \dontrun{
#'     param1 = data.frame(x=c('1','2'),value=1:10)
#'     param2 = data.frame(a=c('london','paris','tahiti'),value=c(50,0.2,1e-2))
#'     write.gdx("test.gdx",list(param1=param1,param2=param2))
#'  }
#'
write_gams.gdx <- function(file, params=list(),
                      vars_l=list(),
                      vars_lo=list(),
                      vars_up=list(),
                      sets=list(),
                      removeLST=T, usetempdir=T, digits=16, compress=F){
  value = NULL
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
  writeLines("$onempty", fgms)
  # collect and write sets
  alllists = c(params,vars_l,vars_lo,vars_up,sets)
  allsets = unique(unlist(lapply(alllists, names)))
  allsets = subset(allsets, !allsets %in% c('*','value'))
  for(i in seq_along(allsets)){
    s = allsets[i]
    writeLines(paste("set", s, "/"), fgms)
    lvalues = lapply(alllists, function(x) unique((as.character(x[[s]]))))
    values = unique(unlist(lvalues))
    writeLines(paste0("'",values,"'"), fgms)
    writeLines("/;", fgms)
  }
  # Write sets
  for(i in seq_along(sets)){
    s = sets[[i]]
    writeLines(paste0("set ", names(sets)[i],
                      " (",paste(rep('*',length(names(s))),collapse=","),")/"), fgms)
    if(ncol(s)==1){
      writeLines(paste0("'",trimws(s[,1]),"'"), fgms)
    }
    if(ncol(s)>1){
      writeLines(paste0("'",paste(apply(as.data.frame(s)[,names(s)],1,paste,collapse="'.'")),"'"), fgms)
    }
    writeLines("/;", fgms)
  }
  # Write parameters
  for(i in seq_along(params)){
    p = params[[i]]
    text = ifelse("gams" %in% names(attributes(p)),attributes(p)$gams,"")
    name = ifelse(names(params)[i]=="",paste0("param",i),names(params)[i])
    if(length(colnames(p))==1){
      writeLines(paste("scalar", name, " '", text, "' /", format(as.numeric(p[1]),digits=digits), "/;"), fgms)
    } else {
      indices = subset(colnames(p), colnames(p) != "value")
      p[[length(indices)+1]] = format(p[[length(indices)+1]],digits=digits)
      writeLines(paste0("parameter ", name,
                        "(", paste(indices, collapse=","), ") ", " '", text, "' /"), fgms)
      concatenate1 <- function(row, len) paste(paste(paste0("'",trimws(row[1:len]),"'"),collapse="."), row[len+1])
      writeLines(apply(subset(p,value!=0),1,concatenate1, len=length(indices)), fgms)
      writeLines("/;", fgms)
    }
  }
  # Write variables
  allvars = c(vars_l,vars_lo,vars_up)
  varnames = c()
  for(i in seq_along(allvars)){
    if(!names(allvars)[i] %in% varnames){
      v = allvars[[i]]
      text = ifelse("gams" %in% names(attributes(v)),attributes(v)$gams,"")
      if(length(colnames(v))==1){
        writeLines(paste0("variable ", names(allvars)[i], " '", text, "' ;"), fgms)
      }else{
        indices = subset(colnames(v), colnames(v) != "value")
        writeLines(paste0("variable ", names(allvars)[i],
                          "(", paste(indices, collapse=","), ") ", " '", text, "' ;"), fgms)
      }
      varnames = c(varnames,names(allvars)[i])
    }
  }
  concatenate2 <- function(row, len, vname, vext) {
    paste0(vname,vext,"(",
           paste(paste0("'",trimws(row[1:len]),"'"),collapse=","),
           ")=",row[len+1],";")
  }
  for(i in seq_along(vars_l)){
    v = vars_l[[i]]
    v = subset(v,value!=0)
    if(nrow(v)>0){
      if(length(colnames(v))==1){
        writeLines(paste0(names(vars_l)[i],".l = ",format(as.numeric(v[1]),digits=digits),";"), fgms)
      } else {
        indices = subset(colnames(v), colnames(v) != "value")
        v[[length(indices)+1]] = format(v[[length(indices)+1]],digits=digits)
        writeLines(apply(v,1,concatenate2, len=length(indices), vname=names(vars_l)[i], vext=".l"), fgms)
      }
    }
  }
  for(i in seq_along(vars_lo)){
    v = vars_lo[[i]]
    v = subset(v,!is.infinite(value))
    if(nrow(v)>0){
      if(length(colnames(v))==1){
        writeLines(paste0(names(vars_lo)[i],".lo = ",format(as.numeric(v[1]),digits=digits),";"), fgms)
      } else {
        indices = subset(colnames(v), colnames(v) != "value")
        v[[length(indices)+1]] = format(v[[length(indices)+1]],digits=digits)
        writeLines(apply(v,1,concatenate2, len=length(indices), vname=names(vars_lo)[i], vext=".lo"), fgms)
      }
    }
  }
  for(i in seq_along(vars_up)){
    v = vars_up[[i]]
    v = subset(v,!is.infinite(value))
    if(nrow(v)>0){
      if(length(colnames(v))==1){
        writeLines(paste0(names(vars_up)[i],".up = ",format(as.numeric(v[1]),digits=digits),";"), fgms)
      } else {
        indices = subset(colnames(v), colnames(v) != "value")
        v[[length(indices)+1]] = format(v[[length(indices)+1]],digits=digits)
        writeLines(apply(v,1,concatenate2, len=length(indices), vname=names(vars_up)[i], vext=".up"), fgms)
      }
    }
  }
  # save into a gdx
  writeLines(paste0('execute_unload "',file,'"\n',
                    paste(names(alllists),collapse="\n"),
                    "\n;"), fgms)
  writeLines("$offempty", fgms)
  close(fgms)
  res = gams(paste0(gms,ifelse(compress," gdxcompress=1","")," output=",lst))
  if(res!=0) stop(paste("write gdx failed -",gms))
  if(removeLST) file.remove(lst)
  return(res)
}
