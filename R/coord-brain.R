#' #' @export
#' #' @rdname ggsf
#' #' @usage NULL
#' #' @format NULL
#' CoordBrain <- ggproto("CoordBrain", CoordCartesian,
#'
#'                    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
#'                      browser()
#'                      # Bounding box of the data
#'                      expansion_x <- ggplot2:::default_expansion(scale_x, expand = self$expand)
#'                      x_range <- ggplot2:::expand_limits_scale(scale_x, expansion_x, coord_limits = self$limits$x)
#'                      expansion_y <- ggplot2:::default_expansion(scale_y, expand = self$expand)
#'                      y_range <- ggplot2:::expand_limits_scale(scale_y, expansion_y, coord_limits = self$limits$y)
#'                      bbox <- c(
#'                        x_range[1], y_range[1],
#'                        x_range[2], y_range[2]
#'                      )
#'
#'                      # Generate graticule and rescale to plot coords
#'                      graticule <- sf::st_graticule(
#'                        bbox,
#'                        crs = params$crs,
#'                        lat = scale_y$breaks %|W|% NULL,
#'                        lon = scale_x$breaks %|W|% NULL,
#'                        datum = self$datum,
#'                        ndiscr = self$ndiscr
#'                      )
#'
#'                       sf::st_geometry(graticule) <- sf_rescale01(sf::st_geometry(graticule),
#'                                                                  x_range, y_range)
#'                      graticule$x_start <- sf_rescale01_x(graticule$x_start, x_range)
#'                      graticule$x_end <- sf_rescale01_x(graticule$x_end, x_range)
#'                      graticule$y_start <- sf_rescale01_x(graticule$y_start, y_range)
#'                      graticule$y_end <- sf_rescale01_x(graticule$y_end, y_range)
#'
#'                      list(
#'                        x_range = x_range,
#'                        y_range = y_range,
#'                        graticule = graticule,
#'                        crs = params$crs,
#'                        label_axes = self$label_axes,
#'                        label_graticule = self$label_graticule
#'                      )
#'                    },
#'
#'                    labels = function(labels, panel_params) labels,
#'
#'                    render_axis_h = function(self, panel_params, theme) {
#'                      graticule <- panel_params$graticule
#'
#'                      # top axis
#'                      id1 <- id2 <- integer(0)
#'                      # labels based on panel side
#'                      id1 <- c(id1, which(graticule$type == panel_params$label_axes$top & graticule$y_start > 0.999))
#'                      id2 <- c(id2, which(graticule$type == panel_params$label_axes$top & graticule$y_end > 0.999))
#'
#'                      # labels based on graticule direction
#'                      if ("S" %in% panel_params$label_graticule) {
#'                        id1 <- c(id1, which(graticule$type == "E" & graticule$y_start > 0.999))
#'                      }
#'                      if ("N" %in% panel_params$label_graticule) {
#'                        id2 <- c(id2, which(graticule$type == "E" & graticule$y_end > 0.999))
#'                      }
#'                      if ("W" %in% panel_params$label_graticule) {
#'                        id1 <- c(id1, which(graticule$type == "N" & graticule$y_start > 0.999))
#'                      }
#'                      if ("E" %in% panel_params$label_graticule) {
#'                        id2 <- c(id2, which(graticule$type == "N" & graticule$y_end > 0.999))
#'                      }
#'
#'                      ticks1 <- graticule[unique(id1), ]
#'                      ticks2 <- graticule[unique(id2), ]
#'                      tick_positions <- c(ticks1$x_start, ticks2$x_end)
#'                      tick_labels <- c(ticks1$degree_label, ticks2$degree_label)
#'
#'                      if (length(tick_positions) > 0) {
#'                        top <- draw_axis(
#'                          tick_positions,
#'                          tick_labels,
#'                          axis_position = "top",
#'                          theme = theme
#'                        )
#'                      } else {
#'                        top <- zeroGrob()
#'                      }
#'
#'                      # bottom axis
#'                      id1 <- id2 <- integer(0)
#'                      # labels based on panel side
#'                      id1 <- c(id1, which(graticule$type == panel_params$label_axes$bottom & graticule$y_start < 0.001))
#'                      id2 <- c(id2, which(graticule$type == panel_params$label_axes$bottom & graticule$y_end < 0.001))
#'
#'                      # labels based on graticule direction
#'                      if ("S" %in% panel_params$label_graticule) {
#'                        id1 <- c(id1, which(graticule$type == "E" & graticule$y_start < 0.001))
#'                      }
#'                      if ("N" %in% panel_params$label_graticule) {
#'                        id2 <- c(id2, which(graticule$type == "E" & graticule$y_end < 0.001))
#'                      }
#'                      if ("W" %in% panel_params$label_graticule) {
#'                        id1 <- c(id1, which(graticule$type == "N" & graticule$y_start < 0.001))
#'                      }
#'                      if ("E" %in% panel_params$label_graticule) {
#'                        id2 <- c(id2, which(graticule$type == "N" & graticule$y_end < 0.001))
#'                      }
#'
#'                      ticks1 <- graticule[unique(id1), ]
#'                      ticks2 <- graticule[unique(id2), ]
#'                      tick_positions <- c(ticks1$x_start, ticks2$x_end)
#'                      tick_labels <- c(ticks1$degree_label, ticks2$degree_label)
#'
#'                      if (length(tick_positions) > 0) {
#'                        bottom <- draw_axis(
#'                          tick_positions,
#'                          tick_labels,
#'                          axis_position = "bottom",
#'                          theme = theme
#'                        )
#'                      } else {
#'                        bottom <- zeroGrob()
#'                      }
#'
#'                      list(top = top, bottom = bottom)
#'                    },
#'
#'                    render_axis_v = function(self, panel_params, theme) {
#'                      graticule <- panel_params$graticule
#'
#'                      # right axis
#'                      id1 <- id2 <- integer(0)
#'                      # labels based on panel side
#'                      id1 <- c(id1, which(graticule$type == panel_params$label_axes$right & graticule$x_end > 0.999))
#'                      id2 <- c(id2, which(graticule$type == panel_params$label_axes$right & graticule$x_start > 0.999))
#'
#'                      # labels based on graticule direction
#'                      if ("N" %in% panel_params$label_graticule) {
#'                        id1 <- c(id1, which(graticule$type == "E" & graticule$x_end > 0.999))
#'                      }
#'                      if ("S" %in% panel_params$label_graticule) {
#'                        id2 <- c(id2, which(graticule$type == "E" & graticule$x_start > 0.999))
#'                      }
#'                      if ("E" %in% panel_params$label_graticule) {
#'                        id1 <- c(id1, which(graticule$type == "N" & graticule$x_end > 0.999))
#'                      }
#'                      if ("W" %in% panel_params$label_graticule) {
#'                        id2 <- c(id2, which(graticule$type == "N" & graticule$x_start > 0.999))
#'                      }
#'
#'                      ticks1 <- graticule[unique(id1), ]
#'                      ticks2 <- graticule[unique(id2), ]
#'                      tick_positions <- c(ticks1$y_end, ticks2$y_start)
#'                      tick_labels <- c(ticks1$degree_label, ticks2$degree_label)
#'
#'                      if (length(tick_positions) > 0) {
#'                        right <- draw_axis(
#'                          tick_positions,
#'                          tick_labels,
#'                          axis_position = "right",
#'                          theme = theme
#'                        )
#'                      } else {
#'                        right <- zeroGrob()
#'                      }
#'
#'                      # left axis
#'                      id1 <- id2 <- integer(0)
#'                      # labels based on panel side
#'                      id1 <- c(id1, which(graticule$type == panel_params$label_axes$left & graticule$x_end < 0.001))
#'                      id2 <- c(id2, which(graticule$type == panel_params$label_axes$left & graticule$x_start < 0.001))
#'
#'                      # labels based on graticule direction
#'                      if ("N" %in% panel_params$label_graticule) {
#'                        id1 <- c(id1, which(graticule$type == "E" & graticule$x_end < 0.001))
#'                      }
#'                      if ("S" %in% panel_params$label_graticule) {
#'                        id2 <- c(id2, which(graticule$type == "E" & graticule$x_start < 0.001))
#'                      }
#'                      if ("E" %in% panel_params$label_graticule) {
#'                        id1 <- c(id1, which(graticule$type == "N" & graticule$x_end < 0.001))
#'                      }
#'                      if ("W" %in% panel_params$label_graticule) {
#'                        id2 <- c(id2, which(graticule$type == "N" & graticule$x_start < 0.001))
#'                      }
#'
#'                      ticks1 <- graticule[unique(id1), ]
#'                      ticks2 <- graticule[unique(id2), ]
#'                      tick_positions <- c(ticks1$y_end, ticks2$y_start)
#'                      tick_labels <- c(ticks1$degree_label, ticks2$degree_label)
#'
#'                      if (length(tick_positions) > 0) {
#'                        left <- draw_axis(
#'                          tick_positions,
#'                          tick_labels,
#'                          axis_position = "left",
#'                          theme = theme
#'                        )
#'                      } else {
#'                        left <- zeroGrob()
#'                      }
#'
#'                      list(left = left, right = right)
#'                    }
#' )
#'
#' #shamefully stolen from ggplot2
#' sf_rescale01 <- function(x, x_range, y_range) {
#'   if (is.null(x)) {
#'     return(x)
#'   }
#'
#'   sf::st_normalize(x, c(x_range[1], y_range[1], x_range[2], y_range[2]))
#' }
#'
#' #shamefully stolen from ggplot2
#' sf_rescale01_x <- function(x, range) {
#'   (x - range[1]) / diff(range)
#' }
#'
#'
#' #' @inheritParams coord_cartesian
#' #' @export
#' #' @rdname ggbrain
#' coord_brain <- function(xlim = NULL,
#'                         ylim = NULL,
#'                         expand = TRUE,
#'                         label_x = waiver(),
#'                         label_y = waiver(),
#'                         default = FALSE,
#'                         clip = "on") {
#'
#'   if (ggplot2:::is.waive(label_x) && ggplot2:::is.waive(label_y)) {
#'     # if both `label_graticule` and `label_axes` are set to waive then we
#'     # use the default of labels on the left and at the bottom
#'     labels <- list(x = "",
#'                    y = "")
#'   } else {
#'     # if at least one is set we ignore the other
#'     label_graticule <- label_graticule %|W|% ""
#'     label_axes <- label_axes %|W|% ""
#'   }
#'
#'   ggproto(NULL, CoordBrain,
#'           limits = list(x = xlim, y = ylim),
#'           labels = labels,
#'           expand = expand,
#'           default = default,
#'           clip = clip
#'   )
#' }
#'
