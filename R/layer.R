# layer ----
#' Create a new sf layer that auto-maps geometry data
#'
#' The `layer_sf()` function is a variant of [`layer()`] meant to be used by
#' extension developers who are writing new sf-based geoms or stats.
#' The sf layer checks whether the data contains a geometry column, and
#' if one is found it is automatically mapped to the `geometry` aesthetic.
# #' @include layer.r
# #' @inheritParams layer
#' @keywords internal
#' @export
layer_brain <- function(geom = NULL, stat = NULL,
                        data = NULL, mapping = NULL,
                        position = NULL, params = list(),
                        inherit.aes = TRUE,
                        check.aes = TRUE,
                        check.param = TRUE,
                        show.legend = NA) {
  layer(
    geom = geom, stat = stat, data = data, mapping = mapping,
    position = position, params = params, inherit.aes = inherit.aes,
    check.aes = check.aes, check.param = check.param,
    show.legend = show.legend, layer_class = LayerBrain
  )
}

LayerBrain <- ggproto("LayerBrain", ggplot2:::Layer,

                      setup_layer = function(self, data, plot) {
                        # process generic layer setup first
                        data <- ggproto_parent(ggplot2:::Layer, self)$setup_layer(data, plot)

                        common_vars <- names(data)[names(data) %in% names(self$geom_params$atlas)]

                        if(length(common_vars) > 0){
                          cat("merging atlas and data by '", common_vars, "'\n", sep="")

                          data <- dplyr::left_join(self$geom_params$atlas, data, by = common_vars)
                        }else{
                          data <- self$geom_params$atlas
                        }

                        # automatically determine the name of the geometry column
                        # and add the mapping if it doesn't exist
                        if ((isTRUE(self$inherit.aes) && is.null(self$mapping$geometry) && is.null(plot$mapping$geometry)) ||
                            (!isTRUE(self$inherit.aes) && is.null(self$mapping$geometry))) {
                          if (ggplot2:::is_sf(data)) {
                            geometry_col <- attr(data, "sf_column")
                            self$mapping$geometry <- as.name(geometry_col)
                          }
                        }

                        if ((isTRUE(self$inherit.aes) && is.null(self$mapping$hemi) && is.null(plot$mapping$hemi)) ||
                            (!isTRUE(self$inherit.aes) && is.null(self$mapping$hemi))) {
                          self$mapping$hemi <- as.name("hemi")
                        }

                        if ((isTRUE(self$inherit.aes) && is.null(self$mapping$side) && is.null(plot$mapping$side)) ||
                            (!isTRUE(self$inherit.aes) && is.null(self$mapping$side))) {
                          self$mapping$side <- as.name("side")
                        }

                        if ((isTRUE(self$inherit.aes) && is.null(self$mapping$type) && is.null(plot$mapping$type)) ||
                            (!isTRUE(self$inherit.aes) && is.null(self$mapping$type))) {
                          self$mapping$type <- as.name("type")
                        }

                        if ((isTRUE(self$inherit.aes) && is.null(self$mapping$fill) && is.null(plot$mapping$fill)) ||
                            (!isTRUE(self$inherit.aes) && is.null(self$mapping$fill))) {
                          self$mapping$fill <- as.name("region")
                        }


                        # work around for later merging.
                        # shitty solution
                        self$mapping$label <- as.name("label")

                        # automatically determine the legend type
                        if (is.na(self$show.legend) || isTRUE(self$show.legend)) {
                          if (ggplot2:::is_sf(data)) {
                            sf_type <- ggplot2:::detect_sf_type(data)
                            if (sf_type == "point") {
                              self$geom_params$legend <- "point"
                            } else if (sf_type == "line") {
                              self$geom_params$legend <- "line"
                            } else {
                              self$geom_params$legend <- "polygon"
                            }
                          }
                        } else if (is.character(self$show.legend)) {
                          self$geom_params$legend <- self$show.legend
                          self$show.legend <- TRUE
                        }
                        data
                      }
)
