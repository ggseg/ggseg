#' Colour scales from the ggseg atlases
#'
#' @description
#' The `brain` scales provides colours for the different atlases in the
#' package, according to the colours used in the papers where the
#' atlases where first introduced.
#'
#' @section Palettes:
#' The following palettes are available for use with these scales:
#' \describe{
#'   \item{Qualitative}{dkt, yeo7, yeo17, aseg, midsagittal}
#' }
#'
#' @param name String name of atlas
#' @param na.value string name or hex for the colour of NA entries
#' @param ... additional arguments to pass to \code{\link{brain_pal}}
#'
#' @rdname scale_brain
#' @export
#' @importFrom ggplot2 scale_colour_manual
#' @examples
#' scale_brain()
#' scale_colour_brain()
#' scale_fill_brain()
scale_colour_brain <- function(...) {
  scale_brain(..., aesthetics = "colour")
  }

#' @rdname scale_brain
#' @export
#' @importFrom ggplot2 scale_color_manual
scale_color_brain <- function(...) {
  scale_brain(..., aesthetics = "color")
}

#' @export
#' @rdname scale_brain
#' @importFrom ggplot2 scale_fill_manual
scale_fill_brain <- function(...) {
  scale_brain(..., aesthetics = "fill")
}

#' @export
#' @param aesthetics You can scale the brain more generally with \code{scale_brain}
#' and a switch off the aesthetics.
#' @rdname scale_brain
scale_brain = function(name = "dkt", na.value="grey", ..., aesthetics = c("colour", "color", "fill")) {
  pal = brain_pal(name = name, ...)
  aesthetics = match.arg(aesthetics)
  func = switch(aesthetics,
                color =   ggplot2::scale_color_manual,
                colour =  ggplot2::scale_colour_manual,
                fill =    ggplot2::scale_fill_manual)
  func(values = pal, na.value=na.value)
}


