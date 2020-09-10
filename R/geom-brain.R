#' Brain geom
#'
#' call to \code{\link[sfr]{geom_sf}}
#'
#' @param ... arguments to \code{\link[sfr]{geom_sf}}
#'
#' @return ggplot object
#' @export
#'
# #' @examples
geom_brain <- function (mapping = aes(), data = NULL, atlas = NULL,
                        stat = "sf", position = "identity",
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...)
{
  c(layer_brain(geom = GeomBrain,
                data = data,
                mapping = mapping,
                stat = stat,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(na.rm = na.rm,
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

                       df2 <- dplyr::group_by(data, label)

                       coord <- coord$transform(data, panel_params)
                       brain_grob(coord,
                                  lineend = lineend,
                                  linejoin = linejoin,
                                  linemitre = linemitre,
                                  na.rm = na.rm)
                     },

                     draw_key = function(data, params, size) {
                       data <- ggplot2:::modify_list(default_aesthetics(params$legend), data)
                       if (params$legend == "point") {
                         draw_key_point(data, params, size)
                       } else if (params$legend == "line") {
                         draw_key_path(data, params, size)
                       } else {
                         draw_key_polygon(data, params, size)
                       }
                     }
)

default_aesthetics <- function(type) {
  if (type == "point") {
    GeomPoint$default_aes
  } else if (type == "line") {
    GeomLine$default_aes
  } else  {
    ggplot2:::modify_list(GeomPolygon$default_aes, list(fill = "grey90", colour = "grey35"))
  }
}

brain_grob <- function(...) {
 ggplot2:::sf_grob(...)
}

