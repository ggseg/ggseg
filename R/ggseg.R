#' Plot brain parcellations
#'
#' \code{ggseg} plots and returns a ggplot object of plotted
#' aparc regions. Is superseded by the new \code{\link{geom_brain}}.
#' @author Athanasia Mo Mowinckel and Didac Vidal-Piñeiro
#'
#' @param .data A .data.frame to use for plot aesthetics. Should include a
#' column called "region" corresponding to aparc regions.
#'
#' @param atlas Either a string with the name of atlas to use,
#' or a .data.frame containing atlas information (i.e. pre-loaded atlas).
#' @param ... other options sent to geom_polygon for plotting, including
#' mapping aes (cannot include x, y, and group aesthetics).
#' @param hemisphere String to choose hemisphere to plot. Any of c("left","right")[default].
#' @param view String to choose view of the .data. Any of c("lateral","medial")[default].
#' @param position String choosing how to view the .data. Either "dispersed"[default] or "stacked".
#' @param adapt_scales if \code{TRUE}, then the axes will
#' be hemisphere without ticks.  If \code{FALSE}, then will be latitude
#' longitude values.  Also affected by \code{position} argument
#'
#' @importFrom dplyr as_tibble select filter across
#' @importFrom ggplot2 ggplot aes geom_polygon coord_fixed
#' @importFrom tidyr unnest
#'
#' @details
#' \describe{
#'
#' \item{`dk`}{
#' The Desikan-Killiany Cortical Atlas [default], FreeSurfer cortical segmentations.}
#'
#' \item{`aseg`}{
#' FreeSurfer automatic subcortical segmentation of a brain volume}
#' }
#'
#' @return a ggplot object
#'
#' @examples
#' library(ggplot2)
#' ggseg()
#' ggseg(mapping=aes(fill=region))
#' \dontrun{
#' ggseg(colour="black", size=.7, mapping=aes(fill=region)) + theme_void()
#' ggseg(position = "stacked")
#' ggseg(adapt_scales = FALSE)
#' }
#' @seealso [ggplot2][ggplot], [aes][aes],
#' [geom_polygon][geom_polygon], [coord_fixed][coord_fixed]
#'
#' @export
ggseg = function(
  .data = NULL,
  atlas = "dk",
  position = "dispersed",
  view = NULL,
  hemisphere = NULL,
  adapt_scales = TRUE,
  ...
) {
  # Grab the atlas, even if it has been provided as character string
  atlas <- if (!is.character(atlas)) {
    atlas
  } else {
    get(atlas)
  }

  if (!is_ggseg_atlas(atlas)) {
    atlas <- as_ggseg_atlas(atlas)
  }

  atlas <- unnest(atlas, ggseg)

  stack <- match.arg(position, c("stacked", "dispersed"), several.ok = FALSE)

  if (stack == "stacked") {
    atlas <- stack_brain(atlas)
  }

  # Remove .data we don't want to plot
  if (!is.null(hemisphere)) {
    atlas <- filter(atlas, hemi %in% hemisphere)
  }

  if (!is.null(view)) {
    atlas <- filter(atlas, grepl(view, side))

    # Lateral sides are on the far of eachother, squish them together
    if (
      view == "lateral" &
        (all(c("left", "right") %in% hemisphere) | is.null(hemisphere)) &
        stack == "dispersed"
    ) {
      atlas <- squish_position(atlas, hemisphere)
    }
  }

  # If .data has been supplied, merge it
  if (!is.null(.data)) {
    if (is_brain_atlas(.data) | is_ggseg_atlas(.data)) {
      stop("Atlas given as '.data', did you mean to give it to 'atlas'?")
    }
    atlas <- brain_join(.data, atlas)
    atlas <- filter(atlas, !is.na(.long))
  }

  # Create the plot
  gg <- ggplot(
    data = atlas,
    aes(x = .long, y = .lat, group = .id, subgroup = .subid)
  ) +
    geom_polygon(...) +
    coord_fixed()

  # Scales may be adapted, for more convenient viewing
  if (adapt_scales) {
    gg <- gg +
      scale_y_brain(atlas, stack) +
      scale_x_brain(atlas, stack) +
      scale_labs_brain(atlas, stack)
  }

  gg + theme_brain()
}


## quiets concerns of R CMD check
if (getRversion() >= "2.15.1") {
  globalVariables(c(".data", "dk"))
}
