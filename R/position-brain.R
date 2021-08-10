# position ----

#' Reposition brain slices
#'
#' Function for repositioning
#' pre-joined atlas data (i.e. data and atlas
#' already joined to a single data frame).
#' This makes it possible for users to
#' reposition the geometry data for the atlas
#' for control over final plot layout. For even
#' more detailed control over the positioning,
#' the "hemi" and "side" columns should be
#' converted into factors and ordered by wanted
#' order of appearance.
#'
#' @param data sf-data.frame of joined brain atlas and data
#' @param position position formula for slices
#'
#' @return sf-data.frame with repositioned slices
#' @export
#'
#' @examples
#' reposition_brain(dk, hemi ~ side)
#' reposition_brain(dk, side ~ hemi)
#' reposition_brain(dk, hemi + side ~ .)
#' reposition_brain(dk, . ~ hemi + side)
reposition_brain <- function(data, position = "horizontal"){
  data <- as.data.frame(data)

  pos <- position_formula(position, unique(data$type))

  frame_2_position(data, pos)
}


#' Alter brain atlas position
#'
#' Function to be used in the position argument in geom_brain
#' to alter the position of the brain slice/views.
#'
#' @param position formula describing the rows ~ columns organisation.
#'
#' @export
#' @return a ggproto object
#' @importFrom ggplot2 ggproto
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   geom_brain(atlas = dk, aes(fill = region),
#'              position = position_brain(. ~ side + hemi ),
#'              show.legend = FALSE)
#'
#' ggplot() +
#'   geom_brain(atlas = dk, aes(fill = region),
#'              position = position_brain(side ~ hemi ),
#'              show.legend = FALSE)
position_brain <- function(position = "horizontal") {
  ggproto(NULL, PositionBrain, position = position)
}

PositionBrain <- ggplot2::ggproto("PositionBrain", ggplot2:::Position,
                                  position = hemi + side ~ .,

                                  setup_params = function(self, data) {
                                    list(position = self$position)
                                  },

                                  compute_layer = function(self, data, params, layout) {

                                    pos <- position_formula(params$position, unique(data$type))

                                    df3 <- frame_2_position(data, pos)
                                    bbx <- sf::st_bbox(df3$geometry)

                                    # rescale layout to reflect new coordinates
                                    if(is.null(layout$coord$limits$y))
                                      layout$coord$limits$y <- bbx[c(2,4)]

                                    # rescale layout to reflect new coordinates
                                    if(is.null(layout$coord$limits$x))
                                      layout$coord$limits$x <- bbx[c(1,3)]

                                    data <- df3

                                    df3
                                  }
)

# geometry movers ----

position_formula <- function(pos, type){
  chosen <- all.vars(pos, unique = FALSE)
  chosen <- chosen[!grepl("\\.", chosen)]

  if(any(duplicated(chosen)))
    stop("Cannot position brain with the same data as columns and rows",
         call. = FALSE)

  if(type == "cortical"){
    if(length(chosen) < 2)
      stop("position formula not correct. ",
           "Missing ", paste0(c("side","hemi")[!c("side","hemi") %in% chosen], collapse=" & "), "",
           call. = FALSE
      )

    position <- if(length(grep("\\+", pos))>0){
      ifelse(grep("\\+", pos) == 2,
             "rows", "columns")
    }else{
      chosen
    }
  }else{
    stop("Don't know how to position subcortical data",
         call. = FALSE)
  }

  if(all(sum(grepl("\\.|~", pos)) != 2 & position %in% c("rows", "columns")))
    stop("Formula for a single row or column must contain both a . and ~",
         call. = FALSE)

  list(
    position = position,
    chosen = chosen
  )

}


frame_2_position <- function(data, pos){
  df2 <- dplyr::group_by_at(data, pos$chosen)
  df2 <- dplyr::group_split(df2)

  posi <- ifelse(length(pos$position) > 1,
                 "grid", pos$position)

  # get all into same 0-space
  df2 <- lapply(df2, gather_geometry)
  df3 <- switch(posi,
                rows = stack_vertical(df2),
                columns = stack_horizontal(df2),
                grid = stack_grid(df2, pos$position[1], pos$position[2])
  )

  df4 <- st_as_sf(df3$df)
  attr(sf::st_geometry(df4), "bbox") = df3$box

  df4
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

  list(
    df = do.call(rbind, df),
    box = get_box(bx)
  )
}

stack_vertical <- function(df){
  bx <- list()
  for(k in 1:length(df)){
    df[[k]]$geometry <- df[[k]]$geometry + c(0, (k-1)*250)
    bx[[k]] <- sf::st_bbox(df[[k]]$geometry )
  }

  list(
    df = do.call(rbind, df),
    box = get_box(bx)
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
  df <- do.call(rbind, df)

  df[, c("xmin", "xmax", "ymin", "ymax")] <- NULL

  list(
    df = df,
    box = get_box(bx)
  )
}

get_box <- function(bx, pad = 10){
  bx <- do.call(rbind, bx)
  bx <- c(-pad, -pad,
          ceiling(max(bx[,"xmax"]))+pad,
          ceiling(max(bx[,"ymax"])+pad))
  x <- stats::setNames(10*round(bx/10), c("xmin", "ymin", "xmax", "ymax"))
  class(x) <- "bbox"
  x
}

