.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(paste("ggseg by default only loads atlases dkt and aseg.",
                                "To install other available atlases, check out LCBC-UiO/ggsegExtra'"))
  }
}

