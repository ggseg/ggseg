
#' @export
#' @rdname ggbrain
#' @usage NULL
#' @format NULL
CoordBrain <- ggproto("CoordBrain", CoordSf,
                      setup_params = function(self, data) {
                        params <- list(
                          crs = NULL,
                          default_crs = NULL,
                          data = data
                        )
                        self$params <- params

                        params
                      },

                      labels = function(labels, panel_params) labels,

                      # labels = function(self, labels, panel_params) {
                      #   browser()
                      #   positions_x <- c("top", "bottom")
                      #   positions_y <- c("left", "right")
                      #
                      #   list(
                      #     x = lapply(c(1, 2), function(i) {
                      #       panel_guide_label(
                      #         panel_params$guides,
                      #         position = positions_x[[i]],
                      #         default_label = labels$x[[i]]
                      #       )
                      #     }),
                      #     y = lapply(c(1, 2), function(i) {
                      #       panel_guide_label(
                      #         panel_params$guides,
                      #         position = positions_y[[i]],
                      #         default_label = labels$y[[i]])
                      #     })
                      #   )
                      # },

                      # internal function used by setup_panel_params,
                      # overrides the graticule labels based on scale settings if necessary
                      fixup_graticule_labels = function(self, graticule, scale_x, scale_y, params = list()) {
                        browser()
                        needs_parsing <- rep(FALSE, nrow(graticule))
                        needs_autoparsing <- rep(FALSE, nrow(graticule))

                        x_breaks <- graticule$degree[graticule$type == "E"]
                        if (is.null(scale_x$labels)) {
                          x_labels <- rep(NA, length(x_breaks))
                        } else if (is.waive(scale_x$labels)) {
                          x_labels <- graticule$degree_label[graticule$type == "E"]
                          needs_autoparsing[graticule$type == "E"] <- TRUE
                        } else {
                          if (is.function(scale_x$labels)) {
                            x_labels <- scale_x$labels(x_breaks)
                          } else {
                            x_labels <- scale_x$labels
                          }

                          # all labels need to be temporarily stored as character vectors,
                          # but expressions need to be parsed afterwards
                          needs_parsing[graticule$type == "E"] <- !(is.character(x_labels) || is.factor(x_labels))
                          x_labels <- as.character(x_labels)
                        }

                        if (length(x_labels) != length(x_breaks)) {
                          abort("Breaks and labels along x direction are different lengths")
                        }
                        graticule$degree_label[graticule$type == "E"] <- x_labels


                        y_breaks <- graticule$degree[graticule$type == "N"]
                        if (is.null(scale_y$labels)) {
                          y_labels <- rep(NA, length(y_breaks))
                        } else if (is.waive(scale_y$labels)) {
                          y_labels <- graticule$degree_label[graticule$type == "N"]
                          needs_autoparsing[graticule$type == "N"] <- TRUE
                        } else {
                          if (is.function(scale_y$labels)) {
                            y_labels <- scale_y$labels(y_breaks)
                          } else {
                            y_labels <- scale_y$labels
                          }

                          # all labels need to be temporarily stored as character vectors,
                          # but expressions need to be parsed afterwards
                          needs_parsing[graticule$type == "N"] <- !(is.character(y_labels) || is.factor(y_labels))
                          y_labels <- as.character(y_labels)
                        }

                        if (length(y_labels) != length(y_breaks)) {
                          abort("Breaks and labels along y direction are different lengths")
                        }
                        graticule$degree_label[graticule$type == "N"] <- y_labels

                        # Parse labels if requested/needed
                        has_degree <- grepl("\\bdegree\\b", graticule$degree_label)
                        needs_parsing <- needs_parsing | (needs_autoparsing & has_degree)
                        if (any(needs_parsing)) {
                          labels <- as.list(graticule$degree_label)
                          labels[needs_parsing] <- parse_safe(graticule$degree_label[needs_parsing])
                          graticule$degree_label <- labels
                        }

                        graticule
                      }
)



# #' @inheritParams coord_cartesian
#' @export
#' @rdname ggbrain
coord_brain <- function(xlim = NULL,
                        ylim = NULL,
                        expand = TRUE,
                        label_x = waiver(),
                        label_y = waiver(),
                        default = FALSE,
                        clip = "on") {

  if (class(label_x) == "waive" && class(label_y) == "waive") {
    # if both `label_graticule` and `label_axes` are set to waive then we
    # use the default of labels on the left and at the bottom
    labels <- list(x = "",
                   y = "")
  } #else {
  #   # if at least one is set we ignore the other
  #   label_graticule <- label_graticule %|W|% ""
  #   label_axes <- label_axes %|W|% ""
  # }

  ggproto(NULL, CoordBrain,
          limits = list(x = xlim, y = ylim),
          labels = labels,
          expand = expand,
          default = default,
          clip = clip
  )
}

# Stolen from ggplot2
# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

grobTree <- function(...){
  grid::grobTree(...)
}

len0_null <- function(...){
 ggplot2:::len0_null(...)
}
