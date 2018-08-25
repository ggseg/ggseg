#' Colour scales from the ggbrain atlases
#'
#' @description
#' The `brain` scales provides colours for the different atlases in the
#' package, according to the colours used in the papers where the
#' atlases where first introduced.
#'
#' @section Palettes:
#' The following palettes are available for use with these scales:
#' \describe{
#'   \item{Qualitative}{dkt, yeo7, yeo17, aseg}
#' }
#'
#' @family colour scales
#'
#' @rdname scale_brain
#' @export
#' @importFrom ggplot2 scale_colour_manual
scale_colour_brain <- function(palette = "dkt", n="all", unname = F, direction = 1, aesthetics = "colour") {
  ggplot2::scale_colour_manual(values = brain_pal(palette, n=n, direction=direction, unname=unname))
}

#' @export
#' @rdname scale_brain
#' @importFrom ggplot2 scale_fill_manual
scale_fill_brain <- function(palette = "dkt", n="all", unname = F, direction = 1, aesthetics = "fill") {
  ggplot2::scale_fill_manual(values = brain_pal(palette, n=n, direction=direction, unname=unname))
}
