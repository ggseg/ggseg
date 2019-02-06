#' @importFrom methods setOldClass
#' @exportClass ggseg_atlas
setOldClass(c("ggseg_atlas","tbl_df", "tbl", "data.frame"))

#' `ggseg_atlas` class
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
#'
#' @section Behavior of `ggseg_atlas`:
#'
#'
#' @name ggseg_atlas-class
#' @aliases ggseg_atlas ggseg_atlas-class
#' @seealso [tibble()], [as_tibble()], [tribble()], [print.tbl()],
#'   [glimpse()]
NULL


ggseg_atlas <- function(x = data.frame(long = NA,
                                       lat = NA,
                                       order = NA,
                                       piece = NA,
                                       id = NA,
                                       hemi = NA,
                                       area = NA,
                                       label = NA,
                                       side = NA,
                                       atlas = NA,
                                       pos = NA)) {
  stopifnot(is.data.frame(x))
  stopifnot(all(c("long", "lat", "id", "hemi", "area", "side") %in% names(x)))

  structure(
    x,
    class = "ggseg_atlas"
  )
}
