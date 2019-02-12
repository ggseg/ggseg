#' Plot brain parcellations
#'
#' \code{ggseg} plots and returns a ggplot object of plotted
#' aparc areas.
#' @author Athanasia Mowinckel and Didac Pineiro
#'
#' @param .data A .data.frame to use for plot aesthetics. Should include a
#' column called "area" corresponding to aparc areas.
#'
#' @param atlas Either a string with the name of atlas to use,
#' or a .data.frame containing atlas information (i.e. pre-loaded atlas).
#' @param ... other options sent to ggplot2::geom_polygon for plotting, including
#' mapping aes (cannot include x, y, and group aethetics).
#' @param hemisphere String to choose hemisphere to plot. Any of c("left","right")[default].
#' @param view String to choose view of the .data. Any of c("lateral","medial")[default].
#' @param position String choosing how to view the .data. Either "dispersed"[default] or "stacked".
#' @param adapt_scales if \code{TRUE}, then the axes will
#' be hemisphere without ticks.  If \code{FALSE}, then will be latitude
#' longitude values.  Also affected by \code{position} argument

#'
#' @details
#' \describe{
#'
#' \item{`dkt`}{
#' The Desikan-Killiany Cortical Atlas [default], Freesurfer cortical segmentations.}
#'
#' \item{`aseg`}{
#' Freesurfer automatic subcortical segmentation of a brain volume}
#'
#' }
#'
#' @return a ggplot object
#'
#' @import ggplot2
#' @importFrom dplyr select group_by summarise_at vars funs mutate filter full_join distinct summarise
#' @importFrom tidyr unite_
#' @importFrom magrittr "%>%"
#' @importFrom stats na.omit sd
#'
#' @examples
#' library(ggplot2)
#' ggseg()
#' ggseg(mapping=aes(fill=area))
#' ggseg(colour="black", size=.7, mapping=aes(fill=area)) + theme_void()
#' ggseg(adapt_scales = FALSE, position = "stacked")
#' ggseg(adapt_scales = TRUE, position = "stacked")
#' ggseg(adapt_scales = TRUE)
#' ggseg(adapt_scales = FALSE)
#'
#' @seealso [ggplot2][ggplot2::ggplot], [aes][ggplot2::aes],
#' [geom_polygon][ggplot2::geom_polygon], [coord_fixed][ggplot2::coord_fixed]
#'
#' @export
ggseg = function(.data = NULL,
                 atlas = "dkt",
                 position = "dispersed",
                 view = NULL,
                 hemisphere = NULL,
                 adapt_scales = TRUE,
                 ...){

  # Grab the atlas, even if it has been provided as character string
  geobrain <- if(!is.character(atlas)){
    atlas
  }else{
    get(atlas)
  }

  # Check atlas has necessary components
  geobrain <- as_ggseg_atlas(geobrain)

  if(position=="stacked"){
    if(any(!geobrain %>% dplyr::select(side) %>% unique %>% unlist() %in% c("medial","lateral"))){
      warning("Cannot stack atlas. Check if atlas has medial views.")
    }else{
      geobrain <- stack_brain(geobrain)
    } # If possible to stack
  } # If stacked

  # Remove .data we don't want to plot
  if(!is.null(hemisphere)) geobrain <- dplyr::filter(geobrain, hemi %in% hemisphere)
  if(!is.null(view)) geobrain <- dplyr::filter(geobrain, side %in% view)

  # If .data has been supplied, merge it
  if(!is.null(.data)){
    geobrain <- data_merge(.data, geobrain)
  }

  # Create the plot
  gg <- ggplot2::ggplot(data = geobrain, ggplot2::aes(x=long, y=lat, group=id)) +
    ggplot2::geom_polygon(...) +
    ggplot2::coord_fixed()


  # Scales may be adapted, for more convenient vieweing
  if(adapt_scales){
    gg <- gg +
      scale_y_brain(geobrain, position) +
      scale_x_brain(geobrain, position) +
      scale_labs_brain(geobrain, position)
  }

  gg + theme_brain()

}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(".data","dkt", "lat_sd", "long_sd", "id"))
}

