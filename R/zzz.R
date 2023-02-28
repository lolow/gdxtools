
# Launched first: (gdxtools::gdx)
.onLoad <- function(libname, pkgname) {
  # Check if GAMStransfer is installed
  options(gdxtools.gamstransfer = (system.file(package='gamstransfer') != ""))
}

# Launch with library(gdxtools)
.onAttach <- function(libname, pkgname) {

  if (!interactive()) return()

  packageStartupMessage(paste("gdxtools", utils::packageVersion("gdxtools")))

  # Determine GAMS binary filename
  if (Sys.info()[["sysname"]] == 'Windows'){
    options(gdxtools.gamsname = 'gams.exe')
  } else {
    options(gdxtools.gamsname = 'gams')
  }
  gname <- getOption("gdxtools.gamsname")

  if (!getOption("gdxtools.gamstransfer")) {

    packageStartupMessage("Using GDXRRW.")

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
      idx <- which(grepl("GAMS Release     :",res))
      if (length(idx) == 1) {
        gams_ver <- regmatches(res[idx],regexpr("[0-9]+",res[idx]))
        if (as.numeric(gams_ver) >= 41) {
          packageStartupMessage(paste0("Since GAMS 41, GDXRRW is deprecated, ",
                                       "gdxtools should use GAMS Transfert R."))
          packageStartupMessage("Please install it with:")
          packageStartupMessage(paste0("install.packages(\"",file.path(.res,
                    "apifiles/R/gamstransfer/source/gamstransfer_r.tar.gz\""),
                    ", dependencies=TRUE)"))
        }
      }
    }
  } else {
    packageStartupMessage("Using GAMS Transfert R.")
  }

}
