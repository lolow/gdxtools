#' include some parts of gdxrrw from https://support.gams.com/gdxrrw:interfacing_gams_and_r
#' @useDynLib gdxtools, gamsExt, gdxInfoExt, igdxExt, rgdxExt, wgdxExt

#' @author Steve Dirkse
#' @export
rgdx <- function(gdxName, requestList = NULL, squeeze=TRUE, useDomInfo=TRUE,
                 followAlias=TRUE)
{
  if (is.null(requestList) && (gdxName == '?')) {
    invisible(.External(rgdxExt, gdxName=gdxName, requestList=NULL,
                        squeeze=squeeze, useDomInfo=useDomInfo,
                        followAlias=followAlias))
  }
  else {
    .External(rgdxExt, gdxName=gdxName, requestList=requestList,
              squeeze=squeeze, useDomInfo=useDomInfo,
              followAlias=followAlias)
  }
}

#' @author Steve Dirkse
#'@export
wgdx <- function(gdxName, ..., squeeze='y')
{
  invisible(.External(wgdxExt, gdxName=gdxName, ..., squeeze=squeeze))
}

#'@author Steve Dirkse
#'@export
gams <- function(gmsAndArgs)
{
  .External(gamsExt, gmsAndArgs)
  #system2(paste(Sys.which('gams'),gmsAndArgs))
}

#' @author Steve Dirkse
#'@export
gdxInfo <- function(gdxName = NULL, dump=TRUE, returnList=FALSE, returnDF=FALSE)
{
  d <- as.logical(dump)
  if (is.na(d)) {
    stop ("gdxInfo: argument dump=", print(dump), " not a good logical value")
  }
  rl <- as.logical(returnList)
  if (is.na(rl)) {
    stop ("gdxInfo: argument returnList=", print(returnList), " not a good logical value")
  }
  rdf <- as.logical(returnDF)
  if (is.na(rdf)) {
    stop ("gdxInfo: argument returnDF=", print(returnDF), " not a good logical value")
  }
#  print (paste('gdxInfo: dump=',d,'returnList=',rl,'returnDF=',rdf))
  if (! (rl || rdf)) {
    invisible(.External(gdxInfoExt, gdxName=gdxName, dump=d, returnList=rl,
                        returnDF=rdf))
  }
  else {
    .External(gdxInfoExt, gdxName=gdxName, dump=d, returnList=rl,
              returnDF=rdf)
  }
} # gdxInfo

#' @author Steve Dirkse
#'@export
igdx <- function(gamsSysDir = NULL, silent = FALSE, returnStr = FALSE)
{
  invisible(.External(igdxExt, gamsSysDir, silent=silent, returnStr=returnStr))
}

#' @author Steve Dirkse
#'@export
rgdx.param <- function(gdxName, symName, names=NULL, compress=FALSE,
                       ts=FALSE, squeeze=TRUE, useDomInfo=TRUE,
                       check.names=TRUE)
{
  sym <- rgdx(gdxName, list(name=symName,compress=compress,ts=ts),squeeze=squeeze,useDomInfo=useDomInfo)
  if (sym$type != "parameter") {
    stop ("Expected to read a parameter: symbol ", symName, " is a ", sym$type)
  }
  symDim <- sym$dim
  if (symDim < 1) {
    stop ("Symbol ", symName, " is a scalar: data frame output not possible")
  }

  fnames <- list()
  if (is.null(names)) {
    ## no names passed via args
    domainNames <- getOption('gdx.domainNames',default=T)
    if (domainNames) {
      domainNames <- ! ( ("NA"==sym$domInfo) ||
                         ("none"==sym$domInfo) ||
                         ("unknown"==sym$domInfo) )
    }
    if (domainNames) {
      fnames <- sym$domains
      if (check.names) {
        fnames <- patchNames(fnames,symDim)
      }
      fnames[[symDim+1]] <- sym$name
    }
    else {
      fnames <- defNames(symDim,T)
    }
  } else {
    # process the user-provided names
    if (is.vector(names)) {
      namlen <- length(names)
      d2 <- 1
      for (d in c(1:symDim)) {
        fnames[[d]] <- as.character(names[d2])
        d2 <- d2+1
        if (d2 > namlen) d2 <- 1
      }
      # consider 2 cases: names provided just for the index cols,
      # or for the data column too
      if (namlen <= symDim) {
        fnames[[symDim+1]] <- "value"
      }
      else {
        fnames[[symDim+1]] <- as.character(names[d2])
      }
    } else if (is.list(names)) {
      namlen <- length(names)
      d2 <- 1
      for (d in c(1:symDim)) {
        fnames[[d]] <- as.character(names[[d2]])
        d2 <- d2+1
        if (d2 > namlen) d2 <- 1
      }
      # consider 2 cases: names provided just for the index cols,
      # or for the data column too
      if (namlen <= symDim) {
        fnames[[symDim+1]] <- "value"
      }
      else {
        fnames[[symDim+1]] <- as.character(names[[d2]])
      }
    } else {
      for (d in c(1:symDim)) {
        fnames[[d]] <- paste(as.character(names),d,sep=".")
      }
      fnames[[symDim+1]] <- "value"
    }
  }
  if (check.names) {
    fnames <- make.names(fnames,unique=TRUE)
  }

  dflist <- list()
  for (d in c(1:symDim)) {
    nUels <- length(sym$uels[[d]])
    # first arg to factor must be integer, not numeric: different as.character results
    dflist[[d]] <- factor(as.integer(sym$val[,d]), seq(to=nUels), labels=sym$uels[[d]])
  }
  dflist[[symDim+1]] <- sym$val[,symDim+1]
  names(dflist) <- fnames
  symDF <- data.frame(dflist, check.names=check.names)
  attr(symDF,"symName") <- sym$name
  attr(symDF,"domains") <- sym$domains
  ## for now, make domInfo conditional
  if (is.character(sym$domInfo)) {
    attr(symDF,"domInfo") <- sym$domInfo
  }
  if (ts) {
    attr(symDF,"ts") <- sym$ts
  }
  return(symDF)
} # rgdx.param

#'@export
rgdx.scalar <- function(gdxName, symName, ts=FALSE)
{
  request <- list(name=symName,ts=ts)
  readsym <- rgdx(gdxName, request)
  if (readsym$type != "parameter") {
    stop ("Expected to read a scalar: symbol ", symName, " is a ", readsym$type)
  }
  dimsym <- readsym$dim
  if (dimsym > 0) {
    stop ("Parameter ", symName, " has dimension ", dimsym, ": scalar output not possible")
  }
  c <- 0
  if (1 == dim(readsym$val)[1]) {
    c <- readsym$val[1,1]
  }
  attr(c,"symName") <- readsym$name
  if (ts) {
    attr(c,"ts") <- readsym$ts
  }
  return(c)
} # rgdx.scalar

# replace * in domain names with .i, .i4, etc.
# good to use before make.names gets the *'s
patchNames <- function(dNames,n)
{
  if (n > 3) {
    for (d in c(1:n)) {
      if ("*" == dNames[[d]]) {
        dNames[[d]] <- paste0(".i",d)
      }
    }
    return(dNames)
  }

  if ("*" == dNames[[1]]) {
    dNames[[1]] <- paste0(".i")
  }
  if (1 == n) {
    return(dNames)
  }
  if ("*" == dNames[[2]]) {
    dNames[[2]] <- paste0(".j")
  }
  if (2 == n) {
    return(dNames)
  }
  if ("*" == dNames[[3]]) {
    dNames[[3]] <- paste0(".k")
  }
  return(dNames)
} # patchNames

defNames <- function(n,isPar)
{
  if (1 == n) {
    dnames <- list("i")
  } else if (2 == n) {
    dnames <- list("i","j")
  } else if (3 == n) {
    dnames <- list("i","j","k")
  } else {
    dnames <- list()
    for (d in c(1:n)) {
      dnames[[d]] <- paste0("i",d)
    }
  }
  if (isPar) {
    dnames[[n+1]] <- "value"
  }
  return(dnames)
} # defNames

#' @author Steve Dirkse
#'@export
rgdx.set <- function(gdxName, symName, names=NULL, compress=FALSE,
                     ts=FALSE, useDomInfo=TRUE, check.names=TRUE, te=FALSE)
{
  sym <- rgdx(gdxName, list(name=symName,compress=compress,ts=ts,te=te), useDomInfo=useDomInfo)
  if (sym$type != "set") {
    stop ("Expected to read a set: symbol ", symName, " is a ", sym$type)
  }
  symDim <- sym$dim

  fnames <- list()
  if (is.null(names)) {
    ## no names passed via args
    domainNames <- getOption('gdx.domainNames',default=T)
    if (domainNames) {
      domainNames <- ! ( ("NA"==sym$domInfo) ||
                         ("none"==sym$domInfo) ||
                         ("unknown"==sym$domInfo) )
    }
    if (domainNames) {
      fnames <- sym$domains
      if (check.names) {
        fnames <- patchNames(fnames,symDim)
      }
    }
    else {
      fnames <- defNames(symDim,F)
    }
  } else {
    # process the user-provided names
    if (is.vector(names)) {
      namlen <- length(names)
      d2 <- 1
      for (d in c(1:symDim)) {
        fnames[[d]] <- as.character(names[d2])
        d2 <- d2+1
        if (d2 > namlen) d2 <- 1
      }
    } else if (is.list(names)) {
      namlen <- length(names)
      d2 <- 1
      for (d in c(1:symDim)) {
        fnames[[d]] <- as.character(names[[d2]])
        d2 <- d2+1
        if (d2 > namlen) d2 <- 1
      }
    } else {
      for (d in c(1:symDim)) {
        fnames[[d]] <- paste(as.character(names),d,sep=".")
      }
    }
  } # end processing of user-provided names
  if (check.names) {
    fnames <- make.names(fnames,unique=TRUE)
  }

  dflist <- list()
  for (d in c(1:symDim)) {
    nUels <- length(sym$uels[[d]])
    # first arg to factor must be integer, not numeric: different as.character results
    dflist[[d]] <- factor(as.integer(sym$val[,d]), seq(to=nUels), labels=sym$uels[[d]])
  }
  if (te) {
    dflist[[symDim+1]] <- sym$te
    fnames[[symDim+1]] <- ".te"
  }
  names(dflist) <- fnames
  symDF <- data.frame(dflist, check.names=check.names, stringsAsFactors=F)
  attr(symDF,"symName") <- sym$name
  attr(symDF,"domains") <- sym$domains
  if (is.character(sym$domInfo)) {
    attr(symDF,"domInfo") <- sym$domInfo
  }
  if (ts) {
    attr(symDF,"ts") <- sym$ts
  }
  return(symDF)
} # rgdx.set
