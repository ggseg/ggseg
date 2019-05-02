
# Axis scales ----
#' Axis and label scales from the ggseg atlases
#'
#' @description
#' The `brain` axis and label scales provides scales for the different atlases in the
#' package. These add axis labels and tick labels corresponding to the different atlases.
#'
#' @param atlas data.frame containing the atlas
#' @param position Character of either "dispersed" or "stacked".
#' @param aesthetics String vector of which aesthetics to scale "x", "y", or "labs".
#' @param ... additional arguments to pass to \code{\link{adapt_scales}}
#'
#' @rdname scale_continous_brain
#' @export
#' @importFrom ggplot2 scale_colour_manual
#' @examples
#' \dontrun{
#' scale_x_brain()
#' scale_y_brain()
#' scale_labs_brain()
#' }
#'
scale_continous_brain = function(atlas = dkt, position = "dispersed",
                                 aesthetics = c("y", "x")) {
  positions = adapt_scales(atlas, position, aesthetics)
  aesthetics = match.arg(aesthetics)
  func = switch(aesthetics,
                y =  ggplot2::scale_y_continuous,
                x =   ggplot2::scale_x_continuous
  )
  func(breaks = positions$breaks,
       labels = positions$labels)
}

#' @export
#' @rdname scale_continous_brain
#' @importFrom ggplot2 scale_x_continuous
scale_x_brain <- function(...) {
  scale_continous_brain(..., aesthetics = "x")
}

#' @export
#' @rdname scale_continous_brain
#' @importFrom ggplot2 scale_y_continuous
scale_y_brain <- function(...) {
  scale_continous_brain(..., aesthetics = "y")
}

#' @export
#' @rdname scale_continous_brain
#' @importFrom ggplot2 labs
scale_labs_brain <- function(atlas = dkt, position = "dispersed", aesthetics = "labs") {

  positions = adapt_scales(atlas, position, aesthetics)

  aesthetics = match.arg(aesthetics)
  func = switch(aesthetics, labs =  ggplot2::labs)
  func(x = positions$x, y = positions$y)
}
