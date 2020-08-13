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
#' @seealso [tibble()], [as_tibble()], [tribble()], [print.tbl()], [glimpse()]
as_ggseg_atlas <- function(x = data.frame(.long = double(),
                                          .lat = double(),
                                          .id = character(),
                                          .subid = character(),
                                          atlas = character(),
                                          type = character(),
                                          region = character(),
                                          hemi = character(),
                                          side = character()),
                           return = FALSE
) {
  stopifnot(is.data.frame(x))
  ret <- TRUE
  if("ggseg" %in% names(x)) x <- tidyr::unnest(x, cols = c(ggseg))

  necessaries <- c(".long", ".lat", ".id", ".subid",
                   "atlas", "hemi", "region", "side", "type")
  miss <- necessaries %in% names(x)
  if(!all(miss)){
    if(any(c("long", "lat", "id", "subid") %in% names(x))){
      #warning("Old naming convention found, renaming to new")
    }else{
      miss <- stats::na.omit(necessaries[!miss])

      if(!return){
        stop(paste0("There are missing necessary columns in the data.frame for it to be a ggseg_atlas: '",
                    paste0(as.character(miss), "'", collapse=" '")))
      }else{
        ret <- FALSE
      }
    }
  }

  # Variables to group the df by
  group_variables <- c("atlas", "type", "region", "hemi", "side", "label")
  group_variables <- group_variables[group_variables %in% names(x)]

  # columns to be renamed to avoid possible name collisions
  renames <- names(x)[!names(x) %in% group_variables]
  renames <- renames[!grepl("^[.]", renames)]

  x <- suppressWarnings(
    dplyr::select(x,
           dplyr::one_of(c(necessaries, "label","atlas")), dplyr::everything())
  )

  if(length(renames) != 0) x <- dplyr::rename_at(x,
                                          dplyr::vars(dplyr::one_of(renames)),
                                          rename_ggseg_cols)

  x <- suppressWarnings(
    dplyr::group_by_at(x, dplyr::vars(dplyr::one_of(group_variables)))
  )
  x <- tidyr::nest(x)
  x <- dplyr::rename(x, ggseg = data)

  class(x) <- c("ggseg_atlas", "tbl_df", "tbl", "data.frame")

  if(!return){
    return(x)
  }else{
    return(ret)
  }
}

rename_ggseg_cols <- function(x) paste0(".", x)

#' Check if is ggseg_atlas-class
#'
#' @param x atlas object to check
#'
#' @return logical
#' @export
is_ggseg_atlas <- function(x){

  # try to convert to check
  k <- suppressWarnings(
    as_ggseg_atlas(x, return = TRUE)
    )

  # check if class is set
  j <- class(x)[1] == "ggseg_atlas"

  # Both should be true
  all(c(k,j))
}

# quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("x", "ggseg_3d", ".subid"))
}
