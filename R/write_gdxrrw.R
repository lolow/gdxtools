#' Write a list of parameters or sets in a gdx, using gdxrrw (faster than write.gdx, but less options)
#'
#' @export
#' @param file the filename of the gdx to save
#' @param params named list of parameters
#' @param sets named list of sets
#' @author Laurent Drouet
#' @examples
#'  \dontrun{
#'     param1 = data.frame(x=c('1','2'),value=1:10)
#'     param2 = data.frame(a=c('london','paris','tahiti'),value=c(50,0.2,1e-2))
#'     write.gdx("test.gdx",list(param1=param1,param2=param2))
#'  }
#'
write_gdxrrw.gdx <- function(file, params=list(),
                      sets=list()){
  value = NULL
  coll = list()
  # sets
  for(i in seq_along(sets)){
    s = sets[[i]]
    text = ifelse("gams" %in% names(attributes(sets[i])),attributes(sets[i])$gams,"")
    name = ifelse(names(sets)[i]=="",paste0("set",i),names(sets)[i])
    dim = ncol(s)
    uels = list()
    val = matrix(0,ncol=ncol(s),nrow=nrow(s))
    domains = rep('*',dim)
    for(j in 1:dim){
      f = factor(s[[j]])
      uels = c(uels, list(levels(f)))
      val[,j] = as.numeric(f)
    }
    coll = c(coll,list(list(name=name, type='set', dim=dim, val=val,
                       uels=uels, domains=domains, ts=text)))
  }
  # parameters
  for(i in seq_along(params)){
    p = subset(params[[i]], value!=0)
    text = ifelse("gams" %in% names(attributes(params[[i]])),attributes(params[[i]])$gams,"")
    name = ifelse(names(params)[[i]]=="",paste0("param",i),names(params)[[i]])
    dim = ncol(p)-1
    if(dim>0){
      uels = list()
      val = matrix(0,ncol=ncol(p),nrow=nrow(p))
      domains = names(p)[names(p)!="value"]
      for(j in 1:dim){
        f = factor(p[[j]])
        uels = c(uels, list(levels(f)))
        val[,j] = as.numeric(f)
      }
      val[,dim+1] = p$value
      coll = c(coll,list(list(name=name, type='parameter', form='sparse',
                              dim=dim, val=val, uels=uels, domains=domains, ts=text)))
    } else {
      coll = c(coll,list(list(name=name, type='parameter', form='full',
                              dim=0, val=p$value, ts=text)))
    }
  }
  # write into a gdx
  return(wgdx(file,coll))
}
