#' Extract unique names of brain regions
#'
#' @param x brain atlas
#' @export
brain_regions <- function(x){
  UseMethod("brain_regions")
}

#' @export
brain_regions.ggseg_atlas <- function(x){
  x <- unique(x$region)
  x <- x[!is.na(x)]
  x[order(x)]
}

#' @export
brain_regions.brain_atlas <- function(x){
  x <- unique(x$data$region)
  x <- x[!is.na(x)]
  x[order(x)]
}
