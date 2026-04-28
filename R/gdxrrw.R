#' GAMS sysdir locator (kept for backward compatibility)
#'
#' Pre-1.0 gdxtools relied on the gdxrrw API and required the user to point
#' it at a GAMS install. The gamstransfer backend is self-contained and does
#' not need this. \code{igdx} now simply discovers \code{gams} on the PATH
#' (or uses the directory the user supplies) and returns it; calling it has
#' no side effect on gdx I/O.
#'
#' @param gamsSysDir path to the GAMS system directory (optional)
#' @param silent if \code{TRUE}, do not print the discovered path
#' @param returnStr if \code{TRUE}, return the discovered path as a string
#' @return the discovered GAMS system directory, or \code{""} if none found
#' @export
igdx <- function(gamsSysDir = NULL, silent = FALSE, returnStr = FALSE) {
  if (is.null(gamsSysDir) || identical(gamsSysDir, "")) {
    gams_path <- Sys.which(if (.Platform$OS.type == "windows") "gams.exe" else "gams")
    sysdir <- if (nzchar(gams_path)) dirname(gams_path) else ""
  } else {
    sysdir <- gamsSysDir
  }
  if (!silent && nzchar(sysdir)) {
    message("GAMS system directory: ", sysdir)
  }
  if (returnStr) sysdir else invisible(sysdir)
}

#' Run the GAMS executable
#'
#' Thin wrapper around \code{system2("gams", ...)}. Returns the integer exit
#' status from GAMS.
#'
#' @param gmsAndArgs command-line string passed to \code{gams}
#' @return exit status (integer)
#' @export
gams <- function(gmsAndArgs) {
  args <- strsplit(gmsAndArgs, "\\s+")[[1]]
  args <- args[nzchar(args)]
  gams_bin <- Sys.which(if (.Platform$OS.type == "windows") "gams.exe" else "gams")
  if (!nzchar(gams_bin)) stop("could not locate the 'gams' executable on PATH")
  system2(gams_bin, args = args)
}
