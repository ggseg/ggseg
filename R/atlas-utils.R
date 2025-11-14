#' Extract unique names of brain regions
#'
#' Convenience function to extract names of
#' brain regions from a \code{\link{brain_atlas}}
#'
#' @param x brain atlas
#' @return Character vector of brain region names
#' @export
brain_regions <- function(x) {
  UseMethod("brain_regions")
}

#' @export
#' @rdname brain_regions
brain_regions.ggseg_atlas <- function(x) {
  x <- unique(x$region)
  x <- x[!is.na(x)]
  x[order(x)]
}

#' @export
#' @rdname brain_regions
brain_regions.brain_atlas <- function(x) {
  x <- unique(x$data$region)
  x <- x[!is.na(x)]
  x[order(x)]
}

#' @export
#' @rdname brain_regions
brain_regions.data.frame <- function(x) {
  x <- unique(x$region)
  x <- x[!is.na(x)]
  x[order(x)]
}


#' Extract unique labels of brain regions
#'
#' Convenience function to extract names of
#' brain labels from a \code{\link{brain_atlas}}.
#' Brain labels are usually default naming obtained
#' from the original atlas data.
#'
#' @param x brain atlas
#' @return Character vector of atlas region labels
#' @export
brain_labels <- function(x) {
  UseMethod("brain_labels")
}

#' @export
#' @rdname brain_labels
brain_labels.ggseg_atlas <- function(x) {
  x <- unique(x$label)
  x <- x[!is.na(x)]
  x[order(x)]
}

#' @export
#' @rdname brain_labels
brain_labels.brain_atlas <- function(x) {
  x <- unique(x$data$label)
  x <- x[!is.na(x)]
  x[order(x)]
}


#' Detect atlas type
#' @keywords internal
#' @noRd
atlas_type <- function(x) {
  UseMethod("atlas_type")
}

#' @keywords internal
#' @noRd
atlas_type.ggseg_atlas <- function(x) {
  guess_type(x)
}

#' @keywords internal
#' @noRd
atlas_type.brain_atlas <- function(x) {
  guess_type(x)
}

#' @keywords internal
#' @noRd
guess_type <- function(x) {
  k <- if ("type" %in% names(x)) {
    unique(x$type)
  }

  if (is.na(k)) {
    warning("atlas type not set, attempting to guess type", call. = FALSE)
    k <- ifelse(any("medial" %in% x$side), "cortical", "subcortical")
  }

  unique(k)
}
