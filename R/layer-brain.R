#' @noRd
layer_brain <- function(geom = NULL, stat = NULL,
                        data = NULL, mapping = NULL,
                        position = NULL, params = list(),
                        inherit.aes = TRUE,
                        check.aes = TRUE,
                        check.param = TRUE,
                        show.legend = NA) {
  ggplot2::layer(
    geom = geom, stat = stat, data = data, mapping = mapping,
    position = position, params = params, inherit.aes = inherit.aes,
    check.aes = check.aes, check.param = check.param,
    show.legend = show.legend, layer_class = LayerBrain
  )
}

#' @noRd
LayerBrain <- ggproto("LayerBrain", ggplot2:::Layer,

                      setup_layer = function(self, data, plot) {
                        # process generic layer setup first
                        dt <- ggproto_parent(ggplot2:::Layer, self)$setup_layer(data, plot)

                        atlas <- as.data.frame(self$geom_params$atlas)
                        if(is.null(atlas) | nrow(atlas) == 0)
                          stop("No atlas supplied, please provide a brain atlas to the geom.",
                               call. = FALSE)

                        if(!is.null(self$geom_params$hemi)){
                          hemi <- match.arg(self$geom_params$hemi, unique(atlas$hemi))
                          atlas <- atlas[atlas$hemi %in% hemi,]
                        }

                        if(!is.null(self$geom_params$side)){
                          side <- match.arg(self$geom_params$side, unique(atlas$side), several.ok = TRUE)
                          atlas <- atlas[atlas$side %in% side,]
                        }

                        if(class(dt)[1] != "waiver"){

                          data <- brain_join(dt, atlas)

                          merge_errs <- sapply(data$geometry,
                                               function(x) ifelse(length(!is.na(x)) > 0,
                                                                  TRUE, FALSE))

                          if(any(!merge_errs)){
                            k <- data[!merge_errs,]
                            k <- k[,apply(k, 2, function(x) all(!is.na(x)))]
                            k$geometry <- NULL
                            k <- paste(utils::capture.output(k), collapse="\n")

                            warning(paste("Some data not merged. Check for spelling mistakes in:\n",
                                          k, collapse="\n "),
                                    call. = FALSE)
                            data <- data[merge_errs,]
                          }

                        }else{
                          data <- atlas
                        }

                        data <- sf::st_as_sf(data)

                        # automatically determine the name of the geometry column
                        # and add the mapping if it doesn't exist
                        if ((isTRUE(self$inherit.aes) && is.null(self$computed_mapping$geometry) && is.null(plot$computed_mapping$geometry)) ||
                            (!isTRUE(self$inherit.aes) && is.null(self$computed_mapping$geometry))) {
                          if (ggplot2:::is_sf(data)) {
                            geometry_col <- attr(data, "sf_column")
                            self$computed_mapping$geometry <- as.name(geometry_col)
                          }
                        }

                        if ((isTRUE(self$inherit.aes) && is.null(self$computed_mapping$hemi) && is.null(plot$computed_mapping$hemi)) ||
                            (!isTRUE(self$inherit.aes) && is.null(self$computed_mapping$hemi))) {
                          self$computed_mapping$hemi <- as.name("hemi")
                        }

                        if ((isTRUE(self$inherit.aes) && is.null(self$computed_mapping$side) && is.null(plot$computed_mapping$side)) ||
                            (!isTRUE(self$inherit.aes) && is.null(self$computed_mapping$side))) {
                          self$computed_mapping$side <- as.name("side")
                        }

                        if ((isTRUE(self$inherit.aes) && is.null(self$computed_mapping$type) && is.null(plot$computed_mapping$type)) ||
                            (!isTRUE(self$inherit.aes) && is.null(self$computed_mapping$type))) {
                          self$computed_mapping$type <- as.name("type")
                        }

                        if ((isTRUE(self$inherit.aes) && is.null(self$computed_mapping$fill) && is.null(plot$computed_mapping$fill)) ||
                            (!isTRUE(self$inherit.aes) && is.null(self$computed_mapping$fill))) {
                          self$computed_mapping$fill <- as.name("region")
                        }

                        # work around for later merging.
                        # shitty solution
                        self$computed_mapping$label <- as.name("label")

                        # automatically determine the legend type
                        self$geom_params$legend <- "polygon"

                        data
                      }
)


# quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("layer"))
}
