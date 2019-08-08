# GeomBrain <- ggproto("GeomBrain", Geom,
#                      required_aes = c("atlas"),
#
#                      default_aes = aes(
#                        colour = NA, fill = "grey20", size = 0.5,
#                        linetype = 1, alpha = 1,
#                        position = "stacked", view = NULL,
#                        hemisphere = NULL,
#                      ),
#
#                      draw_key = draw_key_polygon,
#
#                      draw_group = function(data, panel_params, coord, atlas) {
#                        n <- nrow(data)
#                        if (n <= 2) return(grid::nullGrob())
#
#                        data2 <- data_merge(data, atlas)
#                        cat(names(data2))
#                        coords <- coord$transform(data2, panel_params)
#
#                        # A polygon can only have a single colour, fill, etc, so take from first row
#                        first_row <- coords[1, , drop = FALSE]
#
#                        grid::polygonGrob(
#                          coords$lat, coords$long,
#                          default.units = "native",
#                          gp = grid::gpar(
#                            col = first_row$colour,
#                            fill = scales::alpha(first_row$fill, first_row$alpha),
#                            lwd = first_row$size * .pt,
#                            lty = first_row$linetype
#                          )
#                        )
#                      }
# )
#
# geom_brain <- function(mapping = NULL, data = NULL, stat = "identity", atlas = dkt,
#                        position = "identity", na.rm = FALSE, show.legend = NA,
#                        inherit.aes = TRUE, ...) {
#   layer(
#     geom = GeomBrain, mapping = mapping, data = data, stat = stat,
#     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, atlas = atlas, ...)
#   )
# }

#'
#' #' @rdname ggplot2-ggproto
#' #' @format NULL
#' #' @usage NULL
#' #' @export
#' StatBrain <- ggproto(
#'   "StatBrain",
#'   Stat,
#'   required_aes = c("atlas"),
#'   default_aes = aes(
#'     x = stat(x),
#'     y = stat(y),
#'     group = stat(id),
#'     fill = stat(area)
#'   ),
#'
#'   compute_panel = function(data, scales, params, atlas) {
#'     data2 <- tidyr::unnest(atlas, ggseg)
#'
#'     data.frame(x = data2$.long,
#'                y = data2$.lat,
#'                id = data2$.id,
#'                area = data2$area)
#'   }
#' )
#'
#'
#' stat_brain <- function(mapping = NULL,
#'                        data = NULL,
#'                        geom = "polygon",
#'                        position = "identity",
#'                        na.rm = FALSE,
#'                        show.legend = NA,
#'                        inherit.aes = TRUE,
#'                        atlas = NULL,
#'                        ...) {
#'   layer(stat = StatBrain,
#'         data = data,
#'         mapping = mapping,
#'         geom = geom,
#'         position = position,
#'         show.legend = show.legend,
#'         inherit.aes = inherit.aes,
#'         params = list(
#'           na.rm = na.rm,
#'           atlas = atlas,
#'           ...
#'           )
#'         )
#' }
