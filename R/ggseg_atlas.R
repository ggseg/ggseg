#' `ggseg_atlas` class
#' @param x dataframe to be made a ggseg-atlas
#' @param return return logical
#'
#' @description
#' The `ggseg_atlas` class is a subclass of [`data.frame`][base::data.frame()],
#' created in order to have different default behaviour. It heavily relieas on
#' the "tibble" [`tbl_df`][tibble::tibble()].
#' [tidyverse](https://www.tidyverse.org/packages/), including
#' [dplyr](http://dplyr.tidyverse.org/),
#' [ggplot2](http://ggplot2.tidyverse.org/),
#' [tidyr](http://tidyr.tidyverse.org/), and
#' [readr](http://readr.tidyverse.org/).
#'
#' @section Properties of `ggseg_atlas`:
#'
#' Objects of class `ggseg_atlas` have:
#' * A `class` attribute of `c("ggseg_atlas", "tbl_df", "tbl", "data.frame")`.
#' * A base type of `"list"`, where each element of the list has the same
#'   [NROW()].
#' * Alot of this script and its functions are taken from the
#'   [`tibble`][tibble::tibble()]-package
#'
#' @name ggseg_atlas-class
#' @aliases ggseg_atlas ggseg_atlas-class
#' @export
ggseg_atlas <- function(x = data.frame(.long = double(),
                                       .lat = double(),
                                       .id = character(),
                                       .subid = character(),
                                       atlas = character(),
                                       type = character(),
                                       region = character(),
                                       hemi = character(),
                                       side = character())
) {

  stopifnot(is.data.frame(x))

  necessaries <- c(".long", ".lat", ".id", ".subid",
                   "atlas", "hemi", "region", "side", "type")

  miss <- necessaries %in% names(x)

  if(!all(miss)){
    miss <- stats::na.omit(necessaries[!miss])

    stop(paste0("There are missing necessary columns in the data.frame for it to be a ggseg_atlas: '",
                paste0(as.character(miss), "'", collapse=" '")))
  }

  x <- dplyr::group_by(x, across(!starts_with(".")))
  x <- tidyr::nest(x)
  x <- dplyr::rename(x, ggseg = data)
  x <- dplyr::ungroup(x)

  structure(x,
            class = c("ggseg_atlas", class(x))
  )
}

#' Validate ggseg_atlas
#' @param x an object
#' @export
is_ggseg_atlas <- function(x) inherits(x, 'ggseg_atlas')

#' Validate ggseg atlas
#' @export
#' @inheritParams is_ggseg_atlas
is.ggseg_atlas <- is_ggseg_atlas


## as_ggseg_atlas ----
#' Create ggseg atlas
#'
#' @param x object to make into a ggseg_atlas
#'
#' @export
as_ggseg_atlas <- function(x){
  UseMethod("as_ggseg_atlas")
}

#' @export
as_ggseg_atlas.default <- function(x){
  warning(paste("Cannot make object of class", class(x)[1], "into a ggseg_atlas"),
          call. = FALSE)
}

#' @export
as_ggseg_atlas.data.frame <- function(x){
  ggseg_atlas(x)
}

#' @export
as_ggseg_atlas.ggseg_atlas <- function(x){
  x <- tidyr::unnest(x, ggseg)

  if(any("type" != names(x))){
    x$type <- guess_type(x)
  }

  ggseg_atlas(x)
}

#' @export
as_ggseg_atlas.brain_atlas <- function(x){
  x <- as_tibble(x)
  x <- tidyr::unnest(x, ggseg)

  x$geometry <- NULL

  ggseg_atlas(x)
}


#' @export
#' @importFrom stats na.omit
#' @importFrom utils capture.output
format.ggseg_atlas <- function(x, ...) {
  c(
    sprintf("# ggseg atlas"),
    capture.output(as_tibble(x))[-1]
  )
}

#' @export
print.ggseg_atlas <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
#' @importFrom ggplot2 aes
#' @importFrom tidyr as_tibble
plot.ggseg_atlas <- function(x, ..., package = "ggseg"){
  ggseg(atlas = x,
        colour = "grey30",
        mapping = aes(fill = region),
        ...) +
    labs( title = paste(unique(x$atlas), unique(x$type), "atlas"))
}

# quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("x", "ggseg_3d", ".subid"))
}
