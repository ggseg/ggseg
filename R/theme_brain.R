#' ggseg plot theme
#'
#' @description a set of themes created for the ggseg plots. Use ggplot2::theme() to tweak.
#' @author Athanasia Mowinckel
#'
#' @param text.size Specify size of plot text
#' @param text.colour Speficy colour of plot text
#' @param text.family Speficy font family
#' @param plot.background Specify fill of plot background (`theme_custombrain` only)
#'
#' @details
#' \describe{
#'
#' \item{`theme_brain`}{
#' Default theme for ggseg. Transparent background, no axis lines, and no grid.}
#'
#' \item{`theme_darkbrain`}{
#' Dark equivalent to theme_brain, with black background, and light text.}
#'
#' \item{`theme_custombrain`}{
#' Theme for easy customisation of the brain themes.}
#'
#' }
#'
#' @seealso [ggplot()], [aes()], [geom_polygon()], [coord_fixed()] from the ggplot2 package

#' @importFrom ggplot2 theme element_blank element_text
#' @export
#' @rdname ggtheme
theme_brain = function(text.size=12,
                       text.family="mono"){

  ggplot2::theme(
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),

    text = ggplot2::element_text(family=text.family,
                                 size=text.size),

    axis.text = ggplot2::element_text(family=text.family,
                                      size=text.size)
  )
}

#' @export
#' @rdname ggtheme
#' @importFrom ggplot2 theme element_blank element_rect element_text
theme_darkbrain = function(text.size=12,
                           text.family="mono"){

  ggplot2::theme(
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),

    legend.background = ggplot2::element_rect(fill="black"),
    plot.background = ggplot2::element_rect(fill="black"),

    text = ggplot2::element_text(colour="lightgrey"),
    axis.text = ggplot2::element_text(colour="lightgrey",
                                      family=text.family,
                                      size=text.size)
  )

}

#' @export
#' @rdname ggtheme
#' @importFrom ggplot2 theme element_blank element_rect element_text
theme_custombrain = function(plot.background="white",
                             text.colour="darkgrey",
                             text.size=12,
                             text.family="mono"){

  ggplot2::theme(
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),

    legend.background = ggplot2::element_rect(fill=plot.background),
    plot.background = ggplot2::element_rect(fill=plot.background),

    text = ggplot2::element_text(colour=text.colour,
                                 family=text.family,
                                 size=text.size),

    axis.text = ggplot2::element_text(colour=text.colour,
                                      family=text.family,
                                      size=text.size)
  )

}
