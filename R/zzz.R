.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    if("ggsegExtra" %in% rownames(installed.packages())){
      packageStartupMessage(paste("ggseg by default only loads atlases dkt, yeo7, and yeo17 in 2 and 3D.",
                                  "To install the remaining atlases, use 'ggseg::install_atlases()'"))
    }
  }
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("installed.packages"))
}
