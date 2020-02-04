#' Colour and fill scales from the ggseg atlases
#'
#' @description
#' The `brain` palette scales provides scales for the different atlases in the
#' package. Colours are according to the colours used in the papers where the
#' atlases where first introduced.
#'
#' @section Palettes:
#' The following palettes are available for use with these scales:
#' \describe{
#'   \item{ggseg - }{dkt, aseg}
#'   \item{ggsegExtra - }{tracula, jhu, yeo7, yeo17, glasser, chenAr, chenTh,}
#' }
#'
#' @param name String name of atlas
#' @param na.value String name or hex for the colour of NA entries
#' @param aesthetics String vector of which aesthetics to scale c("colour", "color", "fill").
#' @param ... additional arguments to pass to \code{\link{brain_pal}}
#'
#' @rdname scale_brain
#' @export
#' @examples
#' scale_brain()
#' scale_colour_brain()
#' scale_fill_brain()
#'

scale_brain = function(name = "dkt", na.value="grey", ..., aesthetics = c("colour", "color", "fill")) {
  pal = brain_pal(name = name, ...)
  aesthetics = match.arg(aesthetics)
  func = switch(aesthetics,
                color =   ggplot2::scale_color_manual,
                colour =  ggplot2::scale_colour_manual,
                fill =    ggplot2::scale_fill_manual)
  func(values = pal, na.value=na.value)
}

#' @rdname scale_brain
#' @export
scale_colour_brain <- function(...) {
  scale_brain(..., aesthetics = "colour")
  }

#' @rdname scale_brain
#' @export
scale_color_brain <- function(...) {
  scale_brain(..., aesthetics = "color")
}

#' @export
#' @rdname scale_brain
scale_fill_brain <- function(...) {
  scale_brain(..., aesthetics = "fill")
}

