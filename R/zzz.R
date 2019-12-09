.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(paste("ggseg by default only loads atlases dkt and aseg.",
                                "To install the remaining atlases, use 'ggseg::install_atlases()'"))
  }
}

