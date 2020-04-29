#' Brain geom
#'
#' call to \code{\link[sfr]{geom_sf}}
#'
#' @param ... arguments to \code{\link[sfr]{geom_sf}}
#'
#' @return
#' @export
#'
#' @examples
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
    coord_sf(default = TRUE, clip = "off"))
}

# layer ----
#' Create a new sf layer that auto-maps geometry data
#'
#' The `layer_sf()` function is a variant of [`layer()`] meant to be used by
#' extension developers who are writing new sf-based geoms or stats.
#' The sf layer checks whether the data contains a geometry column, and
#' if one is found it is automatically mapped to the `geometry` aesthetic.
#' @include layer.r
#' @inheritParams layer
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

# geom ----
#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
GeomBrain <- ggproto("GeomBrain", Geom,
                     required_aes = c("geometry","hemi","side"),
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
                       if (!inherits(coord, "CoordSf")) {
                         #TODO replace with own coords that detect side/hemi
                         abort("geom_brain() must be used with coord_sf()")
                       }

                       df2 <- dplyr::group_by(data, label)

                       coord <- coord$transform(data, panel_params)
                       ggplot2:::sf_grob(coord,
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

brain_grob <- function(x, lineend = "butt", linejoin = "round",
                       linemitre = 10, na.rm = TRUE) {
  type <- sf_types[sf::st_geometry_type(x$geometry)]
  is_point <- type == "point"
  is_line <- type == "line"
  is_other <- type == "other"
  is_collection <- type == "collection"
  type_ind <- match(type, c("point", "line", "other", "collection"))
  remove <- rep_len(FALSE, nrow(x))
  remove[is_point] <- detect_missing(x, c(GeomPoint$required_aes, GeomPoint$non_missing_aes))[is_point]
  remove[is_line] <- detect_missing(x, c(GeomPath$required_aes, GeomPath$non_missing_aes))[is_line]
  remove[is_other] <- detect_missing(x, c(GeomPolygon$required_aes, GeomPolygon$non_missing_aes))[is_other]
  if (any(remove)) {
    if (!na.rm) {
      warning_wrap(
        "Removed ", sum(remove), " rows containing missing values (geom_brain)."
      )
    }
    x <- x[!remove, , drop = FALSE]
    type_ind <- type_ind[!remove]
    is_collection <- is_collection[!remove]
  }
  defaults <- list(
    GeomPoint$default_aes,
    GeomLine$default_aes,
    modify_list(GeomPolygon$default_aes, list(fill = "grey90", colour = "grey35"))
  )
  defaults[[4]] <- modify_list(
    defaults[[3]],
    rename(GeomPoint$default_aes, c(size = "point_size", fill = "point_fill"))
  )
  default_names <- unique(unlist(lapply(defaults, names)))
  defaults <- lapply(setNames(default_names, default_names), function(n) {
    unlist(lapply(defaults, function(def) def[[n]] %||% NA))
  })
  alpha <- x$alpha %||% defaults$alpha[type_ind]
  col <- x$colour %||% defaults$colour[type_ind]
  col[is_point | is_line] <- alpha(col[is_point | is_line], alpha[is_point | is_line])
  fill <- x$fill %||% defaults$fill[type_ind]
  fill <- alpha(fill, alpha)
  size <- x$size %||% defaults$size[type_ind]
  point_size <- ifelse(is_collection, x$size %||% defaults$point_size[type_ind], size)
  stroke <- (x$stroke %||% defaults$stroke[1]) * .stroke / 2
  fontsize <- point_size * .pt + stroke
  lwd <- ifelse(is_point, stroke, size * .pt)
  pch <- x$shape %||% defaults$shape[type_ind]
  lty <- x$linetype %||% defaults$linetype[type_ind]
  gp <- gpar(
    col = col, fill = fill, fontsize = fontsize, lwd = lwd, lty = lty,
    lineend = lineend, linejoin = linejoin, linemitre = linemitre
  )
  sf::st_as_grob(x$geometry, pch = pch, gp = gp)
}


# position ----

position_brain <- function(position = "horizontal") {
  ggproto(NULL, PositionBrain, position = position)
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionBrain <- ggproto("PositionBrain", ggplot2:::Position,
                         position = "horizontal",

                         setup_params = function(self, data) {
                           list(position = self$position)
                         },

                         compute_layer = function(self, data, params, layout) {

                           df3 <- frame_2_position(params, data)

                           # rescale layout to reflect new coordinates
                           if(is.null(layout$coord$limits$y))
                             layout$coord$limits$y <- df3$box[c(2,4)]

                           # rescale layout to reflect new coordinates
                           if(is.null(layout$coord$limits$x))
                             layout$coord$limits$x <- df3$box[c(1,3)]

                           df3$df
                         }
)

# scales ----


# geometry movers ----

position_formula <- function(pos){
  # browser()

  chosen <- all.vars(pos, unique = FALSE)
  chosen <- chosen[!grepl("\\.", chosen)]

  if(any(duplicated(chosen)))
    stop("Cannot position brain with the same data as columns and rows",
         call. = FALSE)

  if(length(chosen) < 2)
    stop(paste0("position formula not correct. ",
                "Missing '", c("side","hemi")[!c("side","hemi") %in% chosen], "'")
    )

  position <- if(length(grep("\\+", pos))>0){
    ifelse(grep("\\+", pos) == 2,
           "rows", "columns")
  }else{
    chosen
  }

  if(all(sum(grepl("\\.|~", pos)) != 2 & position %in% c("rows", "columns")))
    stop("Formula for a single row or column must contain both a . and ~")

  return(list(position = position,
              chosen = chosen))

}

frame_2_position <- function(params, data){

  pos <- position_formula(params$position)

  df2 <- dplyr::group_by_at(data, pos$chosen)
  df2 <- dplyr::group_split(df2)

  # get all into same 0-space
  df2 <- purrr::map(df2, ~ gather_geometry(.x))

  df3 <- if(length(pos$position) == 2){
    stack_grid(df2, pos$position[1], pos$position[2])
  }else{
    switch(pos$position,
           "rows" = stack_vertical(df2),
           "columns" = stack_horizontal(df2)
    )
  }
  df3
}

gather_geometry <- function(df){
  bbx <- sf::st_bbox(df$geometry)
  df$geometry <- df$geometry - bbx[c("xmin", "ymin")]
  df
}


stack_horizontal <- function(df){

  bx <- list()
  for(k in 1:length(df)){
    df[[k]]$geometry <- df[[k]]$geometry + c((k-1)*350, 0)
    bx[[k]] <- sf::st_bbox(df[[k]]$geometry )
  }

  return(
    list(
      df = do.call(rbind, df),
      box = get_box(bx)
    )
  )
}

stack_vertical <- function(df){
  bx <- list()
  for(k in 1:length(df)){
    df[[k]]$geometry <- df[[k]]$geometry + c(0, (k-1)*250)
    bx[[k]] <- sf::st_bbox(df[[k]]$geometry )
  }

  return(
    list(
      df = do.call(rbind, df),
      box = get_box(bx)
    )
  )
}

stack_grid <- function(df, rows, columns){
  bx <- list()

  if(length(df) == 4){
    # switch columns if they are not the same
    # so grid aligns properly
    if(unique(df[[3]][columns]) != unique(df[[2]][columns])){
      df <- list(df[[1]], df[[2]],
                 df[[4]], df[[3]])
    }
  }

  # move second and fourth on x
  for(k in c(2,3)){
    df[[k]]$geometry <- df[[k]]$geometry + c(350,0)
  }

  # move third and fourth on y
  for(k in c(3,4)){
    df[[k]]$geometry <- df[[k]]$geometry + c(0,250)
  }

  bx <- lapply(df, function(x) sf::st_bbox(x$geometry ))

  return(
    list(
      df = do.call(rbind, df),
      box = get_box(bx)
    )
  )
}

get_box <- function(bx, pad = 20){
  bx <- do.call(rbind, bx)
  bx <- c(-pad, -pad,
          ceiling(max(bx[,"xmax"]))+pad,
          ceiling(max(bx[,"ymax"])+pad))
  10*round(bx/10)
}

