# brain_atlas ----
#' Constructor for brain atlas
#' @param atlas atlas short name, length one
#' @param type atlas type, cortical or subcortical, length one
#' @param data data.frame with atlas data
#'
#' @export
brain_atlas <- function(atlas, type, data, palette) {
  type <- match.arg(type,
                    c("cortical", "subcortical"))

  stopifnot(length(palette) == length(unique(na.omit(data$region))))
  stopifnot(all(unique(names(data$region)) %in% names(palette)))

  structure(list(
    atlas = atlas,
    type = type,
    data = data,
    palette = palette
  ),
  class = 'brain_atlas')
}

#' Create brain atlas
#' @param x list to make brain atlas
#'
#' @export
as_brain_atlas <- function(x) {
  if(!is.null(names(x)) &
     all(names(x) %in% c("atlas", "type", "data"))){
    brain_atlas(x$atlas, x$type, x$data)
  }else{
    stop("Cannot make object to brain atlas")
  }
}

#' Validate brain atlas
#' @param x an object
#' @export
is_brain_atlas <- function(x) inherits(x,'brain_atlas')

#' Validate brain atlas
#' @export
#' @inheritParams is_brain_atlas
is.brain_atlas <- function(x) is_brain_atlas(x)


#' @export
#' @importFrom stats na.omit
#' @importFrom utils capture.output
format.brain_atlas <- function(x, ...) {
  dt <- x$data
  idx <- grep("ggseg", names(dt))
  dt <- dt[!is.na(dt$region), -idx]

  c(
    sprintf("# %s %s brain atlas", x$atlas, x$type),
    sprintf("  regions: %s ", length(na.omit(x$data$region))),
    sprintf("  hemispheres: %s ", paste0(unique(x$data$hemi), collapse = ", ")),
    sprintf("  side views: %s ", paste0(unique(x$data$side), collapse = ", ")),
    "----",
    capture.output(dt)[-1])
}

#' @export
print.brain_atlas <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
#' @importFrom ggplot2 aes
#' @importFrom tidyr as_tibble
plot.brain_atlas <- function(x, ...){
  atl <- as_tibble(x)
  atl <- as_ggseg_atlas(atl)
  ggseg(atlas = atl,
        colour = "grey30",
        mapping = aes(fill = region),
        ...) +
    scale_fill_brain2(x$palette)
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

# brain_atlas_data ----


# brain_polygon ----
#' Create brain polygon
#' @param x data.frame
#' @export
brain_polygon <- function(x) {
  vec_assert(x)
  stopifnot(all(c(".long", ".lat", ".id", ".subid",".order") %in% names(x)))
  stopifnot(!is.null(names(x)))
  invisible(x)

  new_vctr(x, class =  c('ggseg_brain_polygon', class(x)))
}

#' coerce to brain polygon
#' @param x dataframe
#' @export
as_brain_polygon <- function(x) {
  brain_polygon(x)
}

#' Validate brain polygon
#'  @export
#' @inheritParams is.brain_polygon
is_brain_polygon <- function(x) inherits(x,'brain_polygon')

#' validate brain polygon
#' @param x object
#' @export
is.brain_polygon <- function(x) is_brain_atlas(x)

#' @export
#' @importFrom tidyr as_tibble
#' @importFrom utils capture.output
format.brain_polygon <- function(x, ...) {
  c(
    sprintf("# brain polygon with %s vertices", nrow(x)),
    capture.output(as_tibble(x))[-1])
}


#' @export
print.brain_polygon <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @name brain-vctrs
#' @export
vec_is.brain_polygon <- function(x) TRUE


#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name brain-vctrs
NULL

# register_vctrs_methods = function() {
#   register_s3_method("vctrs", "vec_proxy", "sfc")
#   register_s3_method("vctrs", "vec_restore", "sfc")
#   register_s3_method("vctrs", "vec_ptype2", "sfc")
#   register_s3_method("vctrs", "vec_cast", "sfc")
# }

## quiets concerns of R CMD checks
utils::globalVariables(c("region"))
