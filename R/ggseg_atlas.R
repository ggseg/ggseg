#' `ggseg_atlas` class
#' @param x dataframe to be made a ggseg-atlas
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
#' @importFrom dplyr one_of select everything
#' @aliases ggseg_atlas ggseg_atlas-class
#' @seealso [tibble()], [as_tibble()], [tribble()], [print.tbl()], [glimpse()]
as_ggseg_atlas <- function(x = data.frame(long = double(),
                                       lat = double(),
                                       id = character(),
                                       hemi = character(),
                                       side = character())) {
  stopifnot(is.data.frame(x))
  necessaries <- c("long", "lat", "id", "hemi", "area", "side")
  miss <- necessaries %in% names(x)
  if(!all(miss)){
    miss <- na.omit(necessaries[!miss])
    stop(paste0("There are missing necessary columns in the data.frame for it to be a ggseg_atlas: '",
                paste0(as.character(miss), "'", collapse=" '"))
    )
  }

  x <- select(x,
              one_of(c(necessaries, "label","atlas")), everything())
  class(x) <- c("ggseg_atlas", "tbl_df", "tbl", "data.frame")
  return(x)

}

