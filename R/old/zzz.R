.onLoad <- function(libname, pkgname) {
  packageStartupMessage("ggseg by default only loads atlases dkt, yeo7, yeo17 and glasser in 2 and 3D. To install the remaining atlases, use 'ggseg::install_atlases()'")
}
