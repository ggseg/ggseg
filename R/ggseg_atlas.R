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
#' @importFrom dplyr tibble as_tibble one_of select everything group_by_at rename_at
#' @importFrom tidyr nest unnest
#' @aliases ggseg_atlas ggseg_atlas-class
#' @export
#' @seealso [tibble()], [as_tibble()], [tribble()], [print.tbl()], [glimpse()]
as_ggseg_atlas <- function(x = data.frame(.long = double(),
                                          .lat = double(),
                                          .id = character(),
                                          area = as.character(),
                                          hemi = character(),
                                          side = character())
) {
  stopifnot(is.data.frame(x))

  if("ggseg" %in% names(x)) x <- unnest(x, cols = c(ggseg))

  necessaries <- c(".long", ".lat", ".id", "hemi", "area", "side")
  miss <- necessaries %in% names(x)
  if(!all(miss)){
    if(any(c("long", "lat", "id") %in% names(x))){
      warning(paste0("Old naming convention found, renaming to new"))
    }else{
      miss <- na.omit(necessaries[!miss])
      stop(paste0("There are missing necessary columns in the data.frame for it to be a ggseg_atlas: '",
                  paste0(as.character(miss), "'", collapse=" '"))
      )
    }
  }

  # Variables to group the df by
  group_variables <- c("atlas", "area", "hemi", "side", "label")
  group_variables <- group_variables[group_variables %in% names(x)]

  # columns to be renamed to avoid possible name collisions
  renames <- names(x)[!names(x) %in% group_variables]
  renames <- renames[!grepl("^[.]", renames)]

  x <- suppressWarnings(
    select(x,
           one_of(c(necessaries, "label","atlas")), everything())
  )

  if(length(renames) != 0) x <- rename_at(x,
                                          vars(one_of(renames)),
                                          rename_ggseg_cols)

  x <- group_by_at(x, vars(one_of(group_variables))) %>%
    nest() %>%
    rename(ggseg = data)

  class(x) <- c("ggseg_atlas", "tbl_df", "tbl", "data.frame")
  return(x)

}

rename_ggseg_cols <- function(x) paste0(".", x)

#' `ggseg3d_atlas` class
#' @param x dataframe to be made a ggseg-atlas
#'
#' @description
#' The `ggseg_3datlas` class is a subclass of [`data.frame`][base::data.frame()],
#' created in order to have different default behaviour. It heavily relieas on
#' the "tibble" [`tbl_df`][tibble::tibble()].
#' [tidyverse](https://www.tidyverse.org/packages/), including
#' [dplyr](http://dplyr.tidyverse.org/),
#' [ggplot2](http://ggplot2.tidyverse.org/),
#' [tidyr](http://tidyr.tidyverse.org/), and
#' [readr](http://readr.tidyverse.org/).
#'
#' @section Properties of `ggseg3d_atlas`:
#'
#' Objects of class `ggseg3d_atlas` have:
#' * A `class` attribute of `c("ggseg3d_atlas", "tbl_df", "tbl", "data.frame")`.
#' * A base type of `"list"`, where each element of the list has the same
#'   [NROW()].
#' * Alot of this script and its functions are taken from the
#'   [`tibble`][tibble::tibble()]-package
#'
#' @name ggseg3d_atlas-class
#' @importFrom dplyr tibble as_tibble one_of select everything rename
#' @importFrom tidyr unnest nest
#' @aliases ggseg3d_atlas ggseg3d_atlas-class
#' @export
#' @seealso [tibble()], [as_tibble()], [tribble()], [print.tbl()], [glimpse()]
as_ggseg3d_atlas <- function(x = data.frame(atlas = character(),
                                            surf = character(),
                                            hemi = character())
) {
  stopifnot(is.data.frame(x))

  if("ggseg_3d" %in% names(x)) x <- unnest(x, cols = c(ggseg_3d))

  necessaries <- c("atlas", "surf", "hemi", "area", "colour", "mesh")
  miss <- necessaries %in% names(x)
  if(!all(miss)){
    miss <- na.omit(necessaries[!miss])
    stop(paste0("There are missing necessary columns in the data.frame for it to be a ggseg3d_atlas: '",
                paste0(as.character(miss), "'", collapse=" '"))
    )
  }

  names(x$mesh[[1]]) = c("vb", "it")

  x <- group_by(x, atlas, surf, hemi) %>%
    select(one_of(c(necessaries, "label")),
           everything()) %>%
    nest() %>%
    rename(ggseg_3d = data)

  class(x) <- c("ggseg_atlas", "tbl_df", "tbl", "data.frame")
  return(x)
}



#' Check if is ggseg_atlas-class
#'
#' @param x atlas object to check
#'
#' @return logical
#' @export
is_ggseg_atlas <- function(x){
  class(x)[1] == "ggseg_atlas"
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("x", "ggseg_3d"))
}
