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
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # pos <- position_formula(position, data)

  frame_2_position(data, position)
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

                                    df3 <- frame_2_position(data, params$position)
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

position_formula <- function(pos, data){
  chosen <- all.vars(pos, unique = FALSE)
  chosen <- chosen[!grepl("\\.", chosen)]

  if(any(duplicated(chosen)))
    stop("Cannot position brain with the same data as columns and rows",
         call. = FALSE)

  if(unique(data$type) == "cortical"){
    if(length(chosen) < 2)
      stop("position formula not correct. ",
           "Missing ", paste0(c("side","hemi")[!c("side","hemi") %in% chosen], collapse=" & "), "",
           call. = FALSE
      )
    position <- if(length(grep("\\+", pos)) > 0){
      ifelse(grep("^\\.", pos) == 2,
             "columns", "rows")
    }else{
      chosen
    }
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
    dfpos <- split_data(data, pos)
    df2 <- lapply(dfpos$data, gather_geometry)
    posi <- ifelse(length(dfpos$position) > 1, "grid", dfpos$position)
    df3 <- switch(posi,
                  rows    = stack_vertical(df2),
                  columns = stack_horizontal(df2),
                  grid    = stack_grid(df2, dfpos$position[1], dfpos$position[2])
    )

  df4 <- st_as_sf(df3$df)
  attr(sf::st_geometry(df4), "bbox") = df3$box

  df4
}

split_data <- function(data, position){
  if(class(position) == "formula"){
    pos <- position_formula(position, data)
    df2 <- dplyr::group_by_at(data, pos$chosen)
    df2 <- dplyr::group_split(df2)
    pos <- pos$position
  }else{
    if(length(position) == 1){
      if(position %in% c("horizontal", "vertical"))
        position <- default_order(data)
    }
    pos <- as.data.frame(strsplit(position, " "),stringsAsFactors = FALSE)
    if(unique(data$type) == "cortical"){
      k <- cbind(pos[2,] %in% data$side,
                 pos[1,] %in% data$hemi)
      k <- sapply(1:nrow(k), function(x) sum(k[x,]))
      pos <- pos[ifelse(k == 2, TRUE, FALSE)]

      df2 <- lapply(pos, function(x){
        data[data$hemi == x[1] & data$side == x[2],]
      })
    }else{
      df2 <- lapply(pos, function(x){
        data[data$side == x,]
      })
    }
    pos <- unique(ifelse(position == "vertical", "rows", "columns"))
  }

  return(list(data = df2, position = pos))
}

gather_geometry <- function(df){
  bbx <- sf::st_bbox(df$geometry)
  df$geometry <- df$geometry - bbx[c("xmin", "ymin")]
  df
}

stack_horizontal <- function(df){
  sep <- get_sep(df)

  bx <- list()
  for(k in 1:length(df)){
    df[[k]]$geometry <- df[[k]]$geometry + c((k-1)*sep[1], 0)
    bx[[k]] <- sf::st_bbox(df[[k]]$geometry )
  }

  list(
    df = do.call(rbind, df),
    box = get_box(bx)
  )
}

stack_vertical <- function(df){
  sep <- get_sep(df)

  bx <- list()
  for(k in 1:length(df)){
    df[[k]]$geometry <- df[[k]]$geometry + c(0, (k-1)*sep[2])
    bx[[k]] <- sf::st_bbox(df[[k]]$geometry )
  }

  list(
    df = do.call(rbind, df),
    box = get_box(bx)
  )
}

stack_grid <- function(df, rows, columns){
  bx <- list()
  sep <- get_sep(df)

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
    df[[k]]$geometry <- df[[k]]$geometry + c(sep[1],0)
  }

  # move third and fourth on y
  for(k in c(3,4)){
    df[[k]]$geometry <- df[[k]]$geometry + c(0,sep[2])
  }

  bx <- lapply(df, function(x) sf::st_bbox(x$geometry ))
  df <- do.call(rbind, df)

  df[, c("xmin", "xmax", "ymin", "ymax")] <- NULL

  list(
    df = df,
    box = get_box(bx)
  )
}

get_box <- function(bx){
  bx <- do.call(rbind, bx)
  pad <- max(bx)*.01
  bx <- c(-pad, -pad,
          max(bx[,"xmax"]) + pad,
          max(bx[,"ymax"]) + pad)
  x <- stats::setNames(bx, c("xmin", "ymin", "xmax", "ymax"))
  class(x) <- "bbox"
  x
}

get_sep <- function(data){
  sep <- sapply(data, function(x) sf::st_bbox(x$geometry))
  sep <- c(max(sep[3,]), max(sep[4,]))
  c("x" = sep[1] + sep[1]*.2, "y" = sep[2] + sep[2]*.2)
}

default_order <- function(data){
  if(unique(data$type) == "cortical"){
    return(c("left lateral", "left medial", "right medial", "right lateral"))
  }else{
    return(unique(data$side))
  }
}
