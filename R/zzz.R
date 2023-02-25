.onAttach <- function(libname, pkgname) {

  if (!interactive()) return()

  packageStartupMessage(paste("gdxtools", utils::packageVersion("gdxtools")))

  # Determine GAMS binary filename
  if (Sys.info()[["sysname"]] == 'Windows'){
    options(gdxtools.gamsname = 'gams.exe')
  } else {
    options(gdxtools.gamsname = 'gams')
  }
  gname <- getOption("gdxtoools.gamsname")

  # Check if GAMStransfer is installed
  options(gdxtools.gamstransfer = (system.file(package='gamstrsanfert') != ""))

  if (!getOption("gdxtools.gamstransfer")) {

    # Try to find GAMS path and load libraries
    .res <- ''
    # try to specify the gams installation from PATH
    .res = igdx('',TRUE,TRUE)
    if(.res=='' | !file.exists(file.path(.res,gname))) {
      .res = igdx(dirname(Sys.which(gname)),TRUE,TRUE)
    }
    if(.res=='' | !file.exists(file.path(.res,gname))) {
      if(Sys.info()[['sysname']]=='Windows'){
        #try to load the latest version of GAMS
        guess_gams_path = rev(sort(Sys.glob("C:\\GAMS\\win**\\**")))
        if(length(guess_gams_path)>0){
          .res=igdx(guess_gams_path[1],TRUE,TRUE)
        }
      }
    }
    if(.res=='' | !file.exists(file.path(.res,gname))){
      packageStartupMessage('Please specify the GAMS directory with the function "igdx"')
    } else {
      packageStartupMessage(paste('GDX library load path:',.res))
    }
    options(gdxtools.gamspath = .res)

    # If gams version > 41
    if ( .res != "" ) {
      res <- system2(file.path(.res, gname),
                     stdout = TRUE, stderr = FALSE)
      print(res)
    }
  }



}
