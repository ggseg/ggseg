#' Extract unique names of brain regions
#'
#' @param x brain atlas
#' @export
brain_regions <- function(x){
  UseMethod("brain_regions")
}

#' @export
brain_regions.ggseg_atlas <- function(x){
  x <- unique(x$region)
  x <- x[!is.na(x)]
  x[order(x)]
}

#' @export
brain_regions.brain_atlas <- function(x){
  x <- unique(x$data$region)
  x <- x[!is.na(x)]
  x[order(x)]
}

#' @export
brain_regions.data.frame <- function(x){
  x <- unique(x$region)
  x <- x[!is.na(x)]
  x[order(x)]
}


#' Extract unique labels of brain regions
#'
#' @param x brain atlas
#' @export
brain_labels <- function(x){
  UseMethod("brain_labels")
}

#' @export
brain_labels.ggseg_atlas <- function(x){
  x <- unique(x$label)
  x <- x[!is.na(x)]
  x[order(x)]
}

#' @export
brain_labels.brain_atlas <- function(x){
  x <- unique(x$data$label)
  x <- x[!is.na(x)]
  x[order(x)]
}


#' Detect atlas type
#'
#' @param x brain atlas
#' @export
atlas_type <- function(x){
  UseMethod("atlas_type")
}

#' @export
atlas_type.ggseg_atlas <- function(x){
  guess_type(x)
}

#' @export
atlas_type.brain_atlas <- function(x){
  guess_type(x)
}


guess_type <- function(x){
  k <- if("type" %in% names(x))
    unique(x$type)

  if(is.na(k)){
    warning("atlas type not set, attempting to guess type",
            call. = FALSE)
    k <- ifelse(any("medial" %in% x$side), "cortical", "subcortical")
  }

  unique(k)
}
