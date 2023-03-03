#' Write a list of parameters in a gdx (old version which uses a temporary gams file)
#'
#' @export
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
write.gdx <- function(file, params=list(),
                      vars_l=list(),
                      vars_lo=list(),
                      vars_up=list(),
                      sets=list(),
                      removeLST=T, usetempdir=T, digits=16, compress=F){
  value = NULL
  # switch to faster write function when possible
  if(!compress & length(vars_l)==0 & length(vars_lo)==0 & length(vars_up)==0){
    return(write_gdxrrw.gdx(file, params = params, sets = sets))
  }
  # Write GDX using a temporary gams file
  return(write_gams.gdx(file, params = params, vars_l = vars_l, vars_lo = vars_lo,
                 vars_up = vars_up, sets = sets, removeLST = removeLST,
                 usetempdir = usetempdir, digits = digits,
                 compress = compress))
}
