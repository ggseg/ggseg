#' Brain geom
#'
#' call to \code{\link[ggplot2]{geom_sf}}
#'
#' @param mapping argument to pass to \code{\link[ggplot2]{aes}} to map
#'        variables from the supplied data to the plot
#' @param data data.frame with data to plot
#' @param atlas object of type brain_atlas to plot
#' @param position position of the data. Default is "identity" but can be
#'        changed by \code{\link{position_brain}}.
#' @param show.legend logical. Should legend be added or not.
#' @param inherit.aes logical. if aes should be inherited from the
#'        main ggplot call or not
#' @param ... arguments to \code{\link[ggplot2]{geom_sf}}
#'
#' @return ggplot object#'
#' @rdname ggbrain
#' @export
#'
# #' @examples
geom_brain <- function (mapping = aes(), data = NULL,
                        atlas = NULL,
                        position = "identity",
                        show.legend = NA,
                        inherit.aes = TRUE, ...)
{
  c(layer_brain(geom = GeomBrain,
                data = data,
                mapping = mapping,
                stat = "sf",
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(na.rm = FALSE,
                              atlas = atlas,
                              ...)),
    coord_sf(default = TRUE, clip = "off")
  )
}


# geom ----
#' @export
#' @rdname ggbrain
#' @usage NULL
#' @format NULL
GeomBrain <- ggproto("GeomBrain", Geom,
                     default_aes = aes(
                       shape = NULL,
                       colour = NULL,
                       fill = NULL,
                       size = NULL,
                       linetype = 1,
                       alpha = NA,
                       stroke = 0.5
                     ),

                     draw_panel = function(data, atlas, panel_params, coord, legend = NULL,
                                           lineend = "butt", linejoin = "round", linemitre = 10,
                                           na.rm = TRUE) {
                       if (!inherits(coord, "CoordSf") ) {
                         stop("geom_brain() must be used with coord_sf()", call.. = FALSE)
                       }

                       coord <- coord$transform(data, panel_params)
                       brain_grob(coord,
                                  lineend = lineend,
                                  linejoin = linejoin,
                                  linemitre = linemitre,
                                  na.rm = na.rm)
                     },

                     draw_key = function(data, params, size) {
                       data <- modify_list(default_aesthetics(params$legend), data)
                       if (params$legend == "point") {
                         draw_key_point(data, params, size)
                       } else if (params$legend == "line") {
                         draw_key_path(data, params, size)
                       } else {
                         draw_key_polygon(data, params, size)
                       }
                     }
)

#' @export
#' @rdname ggbrain
geom_brain_label <- function(mapping = aes(),
                          data = NULL,
                          atlas = NULL,
                          stat = "sf_coordinates",
                          position = "identity",
                          ...,
                          parse = FALSE,
                          nudge_x = 0,
                          nudge_y = 0,
                          label.padding = unit(0.25, "lines"),
                          label.r = unit(0.15, "lines"),
                          label.size = 0.25,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          fun.geometry = NULL) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      abort("Specify either `position` or `nudge_x`/`nudge_y`")
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  layer_brain(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      atlas = atlas,
      ...
    )
  )
}

#' @export
#' @rdname ggbrain
geom_brain_text <- function(mapping = aes(),
                         data = NULL,
                         atlas = NULL,
                         stat = "sf_coordinates",
                         position = "identity",
                         ...,
                         parse = FALSE,
                         nudge_x = 0,
                         nudge_y = 0,
                         check_overlap = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         fun.geometry = NULL) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  layer_brain(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      atlas = atlas,
      ...
    )
  )
}

# helpers ----
default_aesthetics <- function(type) {
    modify_list(GeomPolygon$default_aes,
                list(fill = "grey90", colour = "grey35"))
}


# adapted from ggplot2::sf_grob
brain_grob <- function (x, lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = TRUE) {
  type <- "other"
  names(type) <- "MULTIPOLYGON"
  is_other <- type == "other"
  type_ind <- match(type, c("point", "line", "other", "collection"))

  defaults <- modify_list(GeomPolygon$default_aes,
                          list(colour = "grey35",
                               size = 0.2))

  alpha <- if(!is.null(x$alpha)) x$alpha else defaults$alpha[type_ind]
  col <- if(!is.null(x$colour)) x$colour else defaults$colour[type_ind]

  fill <- if(!is.null(x$fill)) x$fill else defaults$fill[type_ind]
  fill <- alpha(fill, alpha)
  size <- if(!is.null(x$size)) x$size else defaults$size[type_ind]
  point_size <- size

  stroke <- if(!is.null(x$stroke)) x$stroke else defaults$stroke[1]
  stroke <- stroke * .stroke/2
  lwd <- size * .pt
  lty <- if(!is.null(x$linetype)) x$linetype else defaults$linetype[type_ind]
  gp <- grid::gpar(col = col, fill = fill, lwd = lwd,
             lty = lty, lineend = lineend, linejoin = linejoin,
             linemitre = linemitre)
  sf::st_as_grob(x$geometry, gp = gp)
}

detect_missing <- function(df, vars, finite = FALSE) {
  vars <- intersect(vars, names(df))
  !cases(df[, vars, drop = FALSE], if (finite) is_finite else is_complete)
}

modify_list <- function (old, new)
{
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

# quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("GeomPolygon", ".stroke",
                           ".pt", "cases", "is_finite",
                           "is_complete", "coord_sf",
                           "warn", "GeomBrain"))
}
