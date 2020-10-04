#' `ggseg_atlas` class
#' @param x data.frame to be made a ggseg-atlas
#'
#' @description
#' The `ggseg_atlas` class is a subclass of [`data.frame`][data.frame()],
#' created in order to have different default behaviour. It heavily relieas on
#' the "tibble" [`tbl_df`][tibble()].
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
#'   [`tibble`][tibble()]-package
#'
#' @name ggseg_atlas
#'
#' @importFrom dplyr starts_with mutate across group_by
#' @importFrom dplyr everything select ungroup
#' @importFrom tidyr unnest
#' @importFrom stats na.omit
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
    miss <- na.omit(necessaries[!miss])

    stop(paste0("There are missing necessary columns in the data.frame for it to be a ggseg_atlas: '",
                paste0(as.character(miss), "'", collapse=" '")))
  }

  x <- group_by(x, across(!starts_with(".")))
  x <- nest(x)
  x <- rename(x, ggseg = data)
  x <- ungroup(x)
  x <- select(x, atlas, type, hemi, side, region,
                     everything())

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

  if(any("type" != names(x)))
    x$type <- guess_type(x)

  ggseg_atlas(x)
}

#' @export
as_ggseg_atlas.brain_atlas <- function(x){

  dt <- sf2coords(x$data)
  dt$atlas <- x$atlas
  dt$type <- x$type

  dt <- tidyr::unnest(dt, ggseg)

  ggseg_atlas(dt)
}


#' @export
format.ggseg_atlas <- function(x, ...) {
  c(
    sprintf("# ggseg atlas"),
    capture.output(dplyr::as_tibble(x))[-1]
  )
}

#' @export
print.ggseg_atlas <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
plot.ggseg_atlas <- function(x, ..., package = "ggseg"){
  ggseg(atlas = x,
        colour = "grey30",
        mapping = ggplot2::aes(fill = region),
        ...) +
    ggplot2::labs( title = paste(unique(x$atlas), unique(x$type), "atlas"))
}

# quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("x", "ggseg_3d", ".subid",
                           "type"))
}
