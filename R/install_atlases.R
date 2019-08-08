
#' Convenience function to install all atlases from ggsegExtra-repo
#'
#' \code{install_atlases} calls devtools::install_github() with repo = "LCBC-UiO/ggsegExtra"
#' @author Athanasia Mowinckel and Didac Pineiro
#'
#' @param repo repository to install via github
#' @param ... other options to install_github
#' @export
install_atlases = function(repo="LCBC-UiO/ggsegExtra",...){
  remotes::install_github(repo=repo, ...)
}
