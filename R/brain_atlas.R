# brain_atlas ----
#' Constructor for brain atlas
#' @param atlas atlas short name, length one
#' @param type atlas type, cortical or subcortical, length one
#' @param data data.frame with atlas data
#' @param palette named character vector of colours
#'
#' @export
brain_atlas <- function(atlas, type, data, palette = NULL) {
  type <- match.arg(type,
                    c("cortical", "subcortical"))

  if(!is.null(palette)) stopifnot(length(palette) == length(unique(stats::na.omit(data$region))))
  if(!is.null(palette)) stopifnot(all(unique(names(data$region)) %in% names(palette)))

  stopifnot(length(atlas) == 1)

  structure(list(
    atlas = atlas,
    type = type,
    data = brain_data(data),
    palette = palette
  ),
  class = 'brain_atlas'
  )
}


#' Validate brain atlas
#' @param x an object
#' @export
is_brain_atlas <- function(x) inherits(x, "brain_atlas")

#' Validate brain atlas
#' @export
#' @inheritParams is_brain_atlas
is.brain_atlas <- is_brain_atlas


#' @export
#' @importFrom stats na.omit
#' @importFrom utils capture.output
format.brain_atlas <- function(x, ...) {
  dt <- x$data

  sf <- ifelse(any("geometry" %in% names(dt)), TRUE, FALSE)
  dt$geometry <- NULL

  idx <- !grepl("ggseg|geometry", names(dt))
  dt <- dplyr::as_tibble(dt)
  dt <- dt[!is.na(dt$region), idx]
  dt_print <- utils::capture.output(dt)[-1]

  c(
    sprintf("# %s %s brain atlas", x$atlas, x$type),
    sprintf("  regions: %s ", length(stats::na.omit(unique(x$data$region)))),
    sprintf("  hemispheres: %s ", paste0(unique(x$data$hemi), collapse = ", ")),
    sprintf("  side views: %s ", paste0(unique(x$data$side), collapse = ", ")),
    sprintf("  palette: %s ", ifelse(is.null(x$palette), "no", "yes")),
    sprintf("  use: %s ", ifelse(sf, "ggplot() + geom_brain()", "ggseg()")),
    "----",
    dt_print
  )
}

#' @export
print.brain_atlas <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
plot.brain_atlas <- function(x,  ...){

  if("geometry" %in% names(x$data)){
    p <- ggplot2::ggplot() +
      geom_brain(atlas = x,
                 ...) +
      ggplot2::labs(title = paste(x$atlas, x$type, "atlas"))

    if(!is.null(x$palette))
      p <- p + scale_fill_brain2(x$palette)

    p
  }else{
    x <- as_ggseg_atlas(x)
    graphics::plot(x, ...)
  }
}

#' @export
as.data.frame.brain_atlas <- function(x, ...){
  cbind.data.frame(
    data.frame(
      atlas = rep(x$atlas, nrow(x$data)),
      type = rep(x$type, nrow(x$data)),
      stringsAsFactors = FALSE
    ),
    x$data
  )
}


#' @export
as.list.brain_atlas <- function(x, ...){
  list(
    atlas = x$atlas,
    type = x$type,
    data = x$data,
    palette = x$palette
  )
}

## as_brain_atlas ----
#' Create brain atlas
#'
#' @param x object to make into a brain_atlas
#'
#' @export
as_brain_atlas <- function(x){
  UseMethod("as_brain_atlas")
}

#' @export
as_brain_atlas.default <- function(x){
  warning(paste("Cannot make object of class", class(x)[1], "into a brain_atlas"),
          call. = FALSE)
}

#' @export
as_brain_atlas.data.frame <- function(x){

  if(is.null(names(x)) | !all(c("atlas", "hemi", "region", "side", "label") %in% names(x)))
    stop("Cannot make object to brain_atlas", call. = FALSE)

  if(!any(c("ggseg", "geometry") %in% names(x)))
    stop("Object does not contain a 'ggseg' og 'geometry' column.", call. = FALSE)

  type <- guess_type(x)

  dt <- x[, !names(x) %in% c("atlas", "type")]

  brain_atlas(unique(x$atlas), type, dt)
}

#' @export
as_brain_atlas.ggseg_atlas <- function(x){

  dt <- x[, !names(x) %in% c("atlas", "type")]
  dt$lab <- 1:nrow(dt)
  dt_l <- dplyr::group_by(dt, lab)
  dt_l <- dplyr::group_split(dt_l)

  geom <- lapply(dt_l, coords2sf)
  geom <- do.call(rbind, geom)
  dt <- dplyr::left_join(dplyr::select(dt, -ggseg), geom, by="lab")
  dt <- st_as_sf(dt)

  names(dt)[length(names(dt))] <- "geometry"
  sf::st_geometry(dt) <- "geometry"

  dt$lab <- NULL
  brain_atlas(unique(x$atlas), guess_type(x), dt)
}


#' @export
as_brain_atlas.list <- function(x){

  if(is.null(names(x)) | !all(c("atlas", "type", "data") %in% names(x)))
    stop("Cannot make object to brain_atlas", call. = FALSE)

  type <- if("type" %in% names(x)){
    x$type
  }else{
    ifelse(any("medial" %in% x$side), "cortical", "subcortical")
  }

  dt <- x$data[, !names(x$data) %in% c("atlas", "type")]

  brain_atlas(unique(x$atlas), type, dt)
}

#' @export
as_brain_atlas.brain_atlas <- function(x){
  brain_atlas(x$atlas, x$type, x$data, x$palette)
}


# brain data ----
#' `brain_data` class
#' @param x dataframe to be made a brain_data
#'
#' @name brain_data-class
#' @aliases brain_data brain_data-class
brain_data <- function(x = data.frame(atlas = character(),
                                      type = character(),
                                      region = character(),
                                      hemi = character(),
                                      side = character(),
                                      ggseg = character(),
                                      geometry = character())
) {

  stopifnot(is.data.frame(x))
  stopifnot(all(c("hemi", "region", "side") %in% names(x)))
  stopifnot(any(c("ggseg", "geometry") %in% names(x)))

  if("ggseg" %in% names(x)){
    x$ggseg <- lapply(x$ggseg, brain_polygon)
  }

  if("geometry" %in% names(x)){
    stopifnot(inherits(x$geometry, 'sfc_MULTIPOLYGON'))
  }

  structure(
    x,
    class = c("brain_data", class(x))
  )
}

as_brain_data <- function(x) brain_data(x)

brain_polygon <- function(x = data.frame(.long = numeric(),
                                         .lat = numeric(),
                                         .id = character(),
                                         .subid = character(),
                                         .order = integer())
){

  stopifnot(all(c(".long", ".lat", ".subid", ".id", ".order") %in% names(x)))
  stopifnot(is.numeric(x$.long) & is.numeric(x$.lat) & is.integer(x$.order))

  structure(x, class = c("brain_polygon", class(x)))
}

as_brain_polygon <- function(x) brain_polygon(x)

# Validate brain data
is_brain_polygon <- function(x) inherits(x, 'brain_polygon')

# Validate brain data
is.brain_polygon <- is_brain_polygon

format.brain_polygon <- function(x, ...) {
  c(
    sprintf("# brain polygon with %s vertices", nrow(x)),
    utils::capture.output(dplyr::tibble(x))[-1]
  )
}

print.brain_polygon <- function(x, ...) {
  cat(format(x), sep = "\n")
}

# sf ----
# import sf methods
#' @importFrom sf st_as_sf st_as_sfc
NULL

## quiets concerns of R CMD checks
utils::globalVariables(c("region", "lab"))
