#' ggseg plot theme
#'
#' @description a set of themes created for the ggseg plots. Use theme() to tweak.
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
#' @export
#' @importFrom ggplot2 theme element_blank element_rect element_text
theme_brain = function(text.size=12,
                       text.family="mono"){

  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),

    text = element_text(family=text.family,
                                 size=text.size),

    axis.text = element_text(family=text.family,
                                      size=text.size)
  )
}

#' @export
#' @rdname theme_brain
#' @importFrom ggplot2 theme element_blank element_rect element_text
theme_darkbrain = function(text.size=12,
                           text.family="mono"){

  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),

    legend.background = element_rect(fill="black"),
    plot.background = element_rect(fill="black"),

    text = element_text(colour="lightgrey"),
    axis.text = element_text(colour="lightgrey",
                                      family=text.family,
                                      size=text.size)
  )

}

#' @export
#' @rdname theme_brain
#' @importFrom ggplot2 theme element_blank element_rect element_text
theme_custombrain = function(plot.background="white",
                             text.colour="darkgrey",
                             text.size=12,
                             text.family="mono"){

  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),

    legend.background = element_rect(fill=plot.background),
    plot.background = element_rect(fill=plot.background),

    text = element_text(colour=text.colour,
                        family=text.family,
                        size=text.size),

    axis.text = element_text(colour=text.colour,
                             family=text.family,
                             size=text.size)
  )

}

#' @export
#' @rdname theme_brain
#' @importFrom ggplot2 theme element_blank element_rect element_text
theme_brain2 = function(plot.background="white",
                             text.colour="darkgrey",
                             text.size=12,
                             text.family="mono"){

  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),

    legend.background = element_rect(fill=plot.background),
    plot.background = element_rect(fill=plot.background),

    text = element_text(colour=text.colour,
                        family=text.family,
                        size=text.size),

    axis.text = element_blank()
  )

}
