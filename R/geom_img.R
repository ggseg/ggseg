#' library(png); library(grid);
#' library(tidyverse)
#'
#' #' @export
#' #' @rdname geom_tile
#' geom_img <- function(mapping = NULL, data = NULL,
#'                      stat = "identity", position = "identity",
#'                      ...,
#'                      na.rm = FALSE,
#'                      show.legend = NA,
#'                      inherit.aes = TRUE) {
#'
#'   layer(
#'     data = data,
#'     mapping = mapping,
#'     stat = stat,
#'     geom = GeomImg,
#'     position = position,
#'     show.legend = show.legend,
#'     inherit.aes = inherit.aes,
#'     params = list(
#'       na.rm = na.rm,
#'       ...
#'     )
#'   )
#'
#' }
#'
#' #' @rdname ggplot2-ggproto
#' #' @format NULL
#' #' @usage NULL
#' #' @export
#' #' @import ggplot
#' #' @import grid
#' GeomImg <- ggproto("GeomImg", Geom,
#'                    default_aes = aes(img = NA, colour = NA, fill = "grey35",
#'                                      size = 0.5, linetype = 1,
#'                                      alpha = NA),
#'
#'                    required_aes = c("img", "xmin", "xmax", "ymin", "ymax"),
#'
#'                    draw_panel = function(self, data, panel_params, coord) {
#'
#'                      coords <- coord$transform(data, panel_params)
#'
#'                      grid::rasterGrob(
#'                        image = img,
#'                        x = coords$xmin, y= coords$ymax,
#'                        width = coords$xmax - coords$xmin,
#'                        height = coords$ymax - coords$ymin,
#'                        just = c("left", "top"),
#'                        interpolate = TRUE
#'                      )
#'
#'                    },
#'
#'                    draw_key = draw_key_polygon
#' )
#'
#'
#'
#' #' @rdname fortify.raster
#' #' @export
#' #' @method fortify RasterLayer
#' gather_raster <- function(img, xmin , xmax, ymin, ymax){
#'   x <- readPNG(img)
#'   x <- as.raster(x)
#'   x <- apply(raster, 2, function(x) x)
#'   x <- as.data.frame(x)
#'   names(x) = seq(xmin, xmax, length.out = ncol(x))
#'
#'   x <- mutate(x, y = seq(ymax, ymin, length.out = nrow(x)))
#'
#'   x <- gather(x, x, hex, -y, convert = T)
#'   mutate(x, img=img)
#' }
#'
#'
#'
#' ff <- tibble( img = list.files("tests/testthat/img/single/")) %>%
#'   mutate(x = 1:nrow(.),
#'          y = 1,
#'          name = str_replace_all(img, "[a-z]|[[:punct:]]", ""))
