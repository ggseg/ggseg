# ggseg-atlas methods here to avoid sf dependency on ggseg.formats

#' `ggseg_atlas` class
#' @param x data.frame to be made a ggseg-atlas
#'
#' @description
#' The `ggseg_atlas` class is a subclass of [`data.frame`][data.frame()],
#' created in order to have different default behaviour. It heavily relies on
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
#' * A lot of this script and its functions are taken from the
#'   [`tibble`][tibble()]-package
#'
#' @name ggseg_atlas
#' @return a tibble with polygon coordinates for plotting brain regions
#' @importFrom dplyr starts_with mutate across group_by
#' @importFrom dplyr everything select ungroup rename
#' @importFrom tidyr unnest
#' @importFrom stats na.omit
#' @export
ggseg_atlas <- function(x){

  stopifnot(is.data.frame(x))

  necessaries <- c(".long", ".lat", ".id", ".subid",
                   "atlas", "hemi", "region", "side", "type")
  miss <- necessaries %in% names(x)

  if(!all(miss)){
    miss <- na.omit(necessaries[!miss])

    cli::cli_abort(
      sprintf("There are missing necessary columns in the data.frame for it to be a ggseg_atlas: '%s",
                paste0(as.character(miss), "'", collapse=" '"))
    )
  }

  x <- group_by(x, across(!starts_with(".")))
  x <- nest(x)
  x <- rename(x, ggseg = data)
  x <- ungroup(x)
  x <- select(x, atlas, type, hemi, side, region,
                     everything())
  x$ggseg <- brain_polygon(x$ggseg)
  structure(x,
            class = c("ggseg_atlas", "tbl_df", "tbl", "data.frame")
  )
}

#' @export
#' @importFrom dplyr ungroup left_join group_split select
#' @importFrom tidyr unnest
#' @importFrom sf st_geometry st_as_sf
as_brain_atlas.ggseg_atlas <- function(x){
  dt <- x[, !names(x) %in% c("atlas", "type")]
  dt$lab <- 1:nrow(dt)
  dt_l <- group_by(dt, lab)
  dt_l <- unnest(dt_l, ggseg)
  dt_l <- group_split(dt_l)
  geom <- lapply(dt_l, coords2sf)
  geom <- do.call(rbind, geom)
  dt <- cbind(select(dt, -ggseg), geom)
  dt <- st_as_sf(dt)

  names(dt)[length(names(dt))] <- "geometry"
  sf::st_geometry(dt) <- "geometry"

  dt$lab <- NULL
  brain_atlas(unique(x$atlas),
              ggseg.formats:::guess_type(x),
              dt)
}


#' Validate ggseg_atlas
#' @param x an object
#' @return logical if object is of class 'ggseg_atlas'
#' @export
is_ggseg_atlas <- function(x){
  inherits(x, 'ggseg_atlas')
}

## as_ggseg_atlas ----
#' Create ggseg atlas
#'
#' @param x object to make into a ggseg_atlas
#' @return Object of class 'ggseg_atlas'
#' @export
as_ggseg_atlas <- function(x){
  UseMethod("as_ggseg_atlas")
}

#' @export
#' @rdname as_ggseg_atlas
as_ggseg_atlas.default <- function(x){
  cli::cli_abort(
    sprintf("Cannot make object of class '%s' into a ggseg_atlas",
            class(x)[1])
  )
}

#' @export
#' @rdname as_ggseg_atlas
as_ggseg_atlas.data.frame <- function(x){
  ggseg_atlas(x)
}

#' @export
#' @rdname as_ggseg_atlas
#' @importFrom tidyr unnest
as_ggseg_atlas.ggseg_atlas <- function(x){
  x <- unnest(x, ggseg)

  if(any("type" != names(x)))
    x$type <- ggseg.formats:::guess_type(x)

  ggseg_atlas(x)
}

#' @export
#' @rdname as_ggseg_atlas
#' @importFrom tidyr unnest
as_ggseg_atlas.brain_atlas <- function(x){

  dt <- sf2coords(x$data)
  dt$atlas <- x$atlas
  dt$type <- x$type

  dt <- unnest(dt, ggseg)

  ggseg_atlas(dt)
}


#' @export
#' @importFrom dplyr as_tibble
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
#' @importFrom ggplot2 labs aes
plot.ggseg_atlas <- function(x, ..., package = "ggseg"){
  ggseg(atlas = x,
        colour = "grey30",
        mapping = aes(fill = region),
        ...) +
    labs( title = paste(unique(x$atlas), unique(x$type), "atlas"))
}

# brain polygon ----

b_poly <- function(x = data.frame(.long = numeric(),
                                  .lat = numeric(),
                                  .id = character(),
                                  .subid = character(),
                                  .order = integer())
){
  stopifnot(all(c(".long", ".lat", ".subid", ".id", ".order") %in% names(x)))
  stopifnot(is.numeric(x$.long) && is.numeric(x$.lat) && is.integer(x$.order))
  invisible(x)
}

#' @importFrom vctrs vec_assert new_vctr
brain_poly <- function(x = list()){
  x <- lapply(x, b_poly)

  vec_assert(x, list())
  new_vctr(x, class = "brain_polygon")
}


#' @importFrom vctrs vec_cast
brain_polygon <- function(x){
  x <- vec_cast(x, list())
  brain_poly(x)
}

as_brain_polygon <- brain_polygon

# Validate brain data
is_brain_polygon <- function(x) inherits(x, 'brain_polygon')

#' @export
#' @importFrom vctrs vec_data
format.brain_polygon <- function(x, ...) {
  out <- vec_data(x)
  v <- sapply(out, nrow)
  p <- sapply(out, function(c) length(unique(c$.poly)))
  sprintf("< p:% 3d - v:% 3d>", p, v)
}

#' @export
print.brain_polygon <- function(x, ...) {
  cat(format(x), sep = "\n")
}


# quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("x", "ggseg_3d", ".subid",
                           "type"))
}
