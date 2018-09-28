
#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("RTseries version ", packageVersion("RTseries"), " loaded")
    packageStartupMessage("Send bug reports to wqmeeker@iastate.edu")
}
