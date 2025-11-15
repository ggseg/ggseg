# brain_atlas ----
#' Constructor for brain atlas
#'
#' Creates an object of class 'brain_atlas'
#' that is compatible for plotting using the
#' ggseg-package plot functions
#'
#' @param atlas atlas short name, length one
#' @param type atlas type, cortical or subcortical, length one
#' @param data data.frame with atlas data
#' @param palette named character vector of colours
#' @return an object of class 'brain_atlas' containing information
#'        on atlas name, type, data and palette. To be used in plotting
#'        with \code{\link{geom_brain}}.
#' @export
brain_atlas <- function(atlas, type, data, palette = NULL) {
  type <- match.arg(type, c("cortical", "subcortical"))

  if (!is.null(palette)) {
    stopifnot(length(palette) == length(unique(stats::na.omit(data$region))))
  }
  if (!is.null(palette)) {
    stopifnot(
      all(brain_regions(data) %in% names(palette)) &&
        all(names(palette) %in% brain_regions(data))
    )
  }

  stopifnot(length(atlas) == 1)

  structure(
    list(
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
#' @return logical if object is of class 'brain_atlas'
#' @export
is_brain_atlas <- function(x) inherits(x, "brain_atlas")

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
  dt_print <- utils::capture.output(print(dt, ...))[-1]

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
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
plot.brain_atlas <- function(x, ...) {
  if ("geometry" %in% names(x$data)) {
    p <- ggplot2::ggplot() +
      geom_brain(atlas = x, ...) +
      ggplot2::labs(title = paste(x$atlas, x$type, "atlas"))

    if (!is.null(x$palette)) {
      p <- p + scale_fill_brain2(x$palette)
    }

    p
  } else {
    stop(
      "This is not a correctly formatted brain atlas. ",
      "It is missing geometry data, and cannot be plotted.",
      call. = FALSE
    )
  }
}

#' @export
as.data.frame.brain_atlas <- function(x, ...) {
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
as.list.brain_atlas <- function(x, ...) {
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
#' Coerce object into an object of class
#' 'brain_atlas'.
#'
#' @param x object to make into a brain_atlas
#' @return an object of class 'brain_atlas'.
#' @export
as_brain_atlas <- function(x) {
  UseMethod("as_brain_atlas")
}


#' @export
as_brain_atlas.default <- function(x) {
  stop(
    paste("Cannot make object of class", class(x)[1], "into a brain_atlas"),
    call. = FALSE
  )
}

#' @export
as_brain_atlas.data.frame <- function(x) {
  if (
    is.null(names(x)) |
      !all(c("atlas", "hemi", "region", "side", "label") %in% names(x))
  ) {
    stop("Cannot make object to brain_atlas", call. = FALSE)
  }

  if (!any(c("ggseg", "geometry") %in% names(x))) {
    stop(
      "Object does not contain a 'ggseg' og 'geometry' column.",
      call. = FALSE
    )
  }

  type <- guess_type(x)

  dt <- x[, !names(x) %in% c("atlas", "type")]

  brain_atlas(unique(x$atlas), type, dt)
}

#' @export
#' @importFrom dplyr ungroup left_join group_split select
#' @importFrom sf st_geometry st_as_sf
as_brain_atlas.ggseg_atlas <- function(x) {
  dt <- x[, !names(x) %in% c("atlas", "type")]
  dt$lab <- 1:nrow(dt)
  dt_l <- group_by(dt, lab)
  dt_l <- group_split(dt_l)

  geom <- lapply(dt_l, coords2sf)
  geom <- do.call(rbind, geom)
  dt <- left_join(select(dt, -ggseg), geom, by = "lab")
  dt <- st_as_sf(dt)

  names(dt)[length(names(dt))] <- "geometry"
  sf::st_geometry(dt) <- "geometry"

  dt$lab <- NULL
  brain_atlas(unique(x$atlas), guess_type(x), dt)
}


#' @export
as_brain_atlas.list <- function(x) {
  if (is.null(names(x)) | !all(c("atlas", "type", "data") %in% names(x))) {
    stop("Cannot make object to brain_atlas", call. = FALSE)
  }

  if (is.na(x$type)) {
    x$type <- ifelse(any("medial" %in% x$side), "cortical", "subcortical")
  }

  dt <- x$data[, !names(x$data) %in% c("atlas", "type")]

  brain_atlas(unique(x$atlas), x$type, dt)
}

#' @export
as_brain_atlas.brain_atlas <- function(x) {
  brain_atlas(x$atlas, x$type, x$data, x$palette)
}


# brain data ----
#' \code{brain_data} class
#' @param x data.frame to be made a brain_data
#' @return object of class brain_data, consisting of sf polygon
#'        data for brain atlas plotting.
#' @name brain_data-class
#' @noRd
brain_data <- function(x) {
  stopifnot(is.data.frame(x))
  stopifnot(all(c("hemi", "region", "side") %in% names(x)))
  stopifnot(any(c("geometry") %in% names(x)))
  stopifnot(inherits(x$geometry, 'sfc_MULTIPOLYGON'))

  x <- sf::st_as_sf(x)

  structure(
    x,
    class = c("brain_data", class(x))
  )
}

as_brain_data <- brain_data


# sf ----
# import sf methods
#' @importFrom sf st_as_sf st_as_sfc
NULL

## quiets concerns of R CMD checks
utils::globalVariables(c("region", "lab"))
