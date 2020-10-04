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
                        dt <- ggproto_parent(ggplot2:::Layer, self)$setup_layer(data, plot)
                        atlas <- as.data.frame(self$geom_params$atlas)

                        if(class(dt)[1] != "waiver"){
                          common_vars <- names(dt)[names(dt) %in% names(atlas)]

                          cat("merging atlas and data by ", paste(common_vars, collapse = ", "), "\n", sep="")

                          if(dplyr::is.grouped_df(dt)){

                            data2 <- tidyr::nest(dt)
                            data2$data <- lapply(1:nrow(data2), function(x) dplyr::left_join(atlas,
                                                                                             data2$data[[x]],
                                                                                             by = common_vars))

                            data <- tidyr::unnest(data2, data)

                            # browser()
                          }else{
                            data <- dplyr::left_join(atlas,
                                                     data,
                                                     by = common_vars)
                          }


                        }else{
                          data <- as.data.frame(self$geom_params$atlas)

                        }

                        data <- sf::st_as_sf(data)

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


# helper function to find the geometry column
geom_column <- function(data) {
  w <- which(vapply(data, inherits, TRUE, what = "sfc"))
  if (length(w) == 0) {
    "geometry" # avoids breaks when objects without geometry list-column are examined
  } else {
    # this may not be best in case more than one geometry list-column is present:
    if (length(w) > 1)
      warn("more than one geometry column present: taking the first")
    w[[1]]
  }
}

# quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("layer"))
}
