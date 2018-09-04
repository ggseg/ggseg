#' Plot brain parcellations
#'
#' \code{ggseg} plots and returns a ggplot object of plotted
#' aparc areas.
#' @author Athanasia Mowinckel and Didac Pineiro
#'
#' @param data A data.frame to use for plot aesthetics. Must include a
#' column called "area" corresponding to aparc areas.
#'
#' @param atlas Either a string with the name of atlas to use,
#' or a data.frame containing atlas information (i.e. pre-loaded atlas).
#' @param plot.areas Character vector, plots only areas specified in the vector.
#' @param ... other options sent to ggplot2::geom_poly for plotting, including
#' mapping aes (cannot include x and y aethetics).
#' @param hemisphere String to choose hemisphere to plot. Any of c("left","right")[default].
#' @param view String to choose view of the data. Any of c("lateral","medial")[default].
#' @param position String choosing how to view the data. Either "dispersed"[default] or "stacked".
#' @param adapt.scales if \code{TRUE}, then the axes will
#' be hemisphere without ticks.  If \code{FALSE}, then will be latitude
#' longitude values.  Also affected by \code{position} argument

#'
#' @details
#' \describe{
#'
#' \item{`dkt`}{
#' The Desikan-Killiany Cortical Atlas [default], Freesurfer cortical segmentations.}
#'
#' \item{`yeo7`}{
#' Seven resting-state networks from Yeo et al. 2011, J. Neurophysiology}
#'
#' \item{`yeo17`}{
#' Seventeen resting-state networks from Yeo et al. 2011, J. Neurophysiology}
#'
#' \item{`aseg`}{
#' Freesurfer automatic subcortical segmentation of a brain volume}
#'
#' }
#'
#' @return a ggplot object
#'
#' @import ggplot2
#' @importFrom dplyr select group_by summarise_at vars funs mutate filter left_join "%>%"
#' @importFrom stats na.omit
#'
#' @examples
#' library(ggplot2)
#' ggseg()
#' ggseg(mapping=aes(fill=area))
#' ggseg(colour="black", size=.7, mapping=aes(fill=area)) + theme_void()
#' ggseg(atlas="yeo7")
#' ggseg(adapt.scales = FALSE, position = "stacked")
#' ggseg(adapt.scales = TRUE, position = "stacked")
#' ggseg(adapt.scales = TRUE)
#' ggseg(adapt.scales = FALSE)
#'
#' @seealso [ggplot()], [aes()], [geom_polygon()], [coord_fixed()] from the ggplot2 package
#'
#' @export
ggseg = function(data = NULL,atlas="dkt",
                 plot.areas=NULL,
                 position="dispersed",
                 view=c("lateral","medial","axial","sagittal"),
                 hemisphere = c("right","left"),
                 adapt.scales=TRUE,...){

  geobrain = if(!is.character(atlas)){
    atlas
  }else{
    get(atlas)
  }

  if(position=="stacked"){
    if(any(!geobrain %>% dplyr::select(side) %>% unique %>% unlist() %in% c("medial","lateral"))){
      stop("Cannot stack atlas. Check if atlas has medial and axial views.")
    }
    # Alter coordinates of the left side to stack ontop of right side
    stack = geobrain %>%
      dplyr::group_by(hemi,side) %>%
      dplyr::summarise_at(dplyr::vars(long,lat),dplyr::funs(min,max))
    stack$lat_max[1] = ifelse(stack$lat_max[1] < 4.5,
                              stack$lat_max[1]+.5,
                              stack$lat_max[1])

    geobrain = geobrain %>%

      # Move right side over to the left
      dplyr::mutate(lat=ifelse(hemi %in% "right",
                               lat + (stack$lat_max[1]), lat)) %>%

      # move right side on top of left, and swap the places of medial and lateral
      dplyr::mutate(long=ifelse(hemi %in% "right" & side %in% "lateral" ,
                                long - stack$long_min[3], long),
                    long=ifelse(hemi %in% "right" & side %in% "medial" ,
                                long +(stack$long_min[2]-stack$long_min[4]), long)
      )
  } # If stacked

  # Remove data we don't want to plot
  geobrain = geobrain %>%
    dplyr::filter(hemi %in% hemisphere, side %in% view)

  # If data has been supplied, merge it
  if(!is.null(data))
    geobrain = suppressWarnings(suppressMessages(
      geobrain %>%
        dplyr::full_join(data, copy=TRUE)
    ))

  # Filter data to single area if that is all you want.
  if(!is.null(plot.areas)){
    if(any(!plot.areas %in% geobrain$area)){
      stop(paste("There is no", plot.areas,
                 "in", atlas,"data. Check spelling. Options are:",
                 paste0(geobrain$area %>% unique,collapse=", ")))
    }
    geobrain = geobrain %>% dplyr::filter(area %in% plot.areas)
  }

  # Create the plot
  gg = ggplot2::ggplot(data = geobrain, ggplot2::aes(x=long, y=lat, group=group)) +
    ggplot2::geom_polygon(...) +
    ggplot2::coord_fixed()


  # Scales may be adapted, for more convenient vieweing
  if(adapt.scales){

    if(position == "stacked"){

      pos = list(
        x=geobrain %>%
          dplyr::group_by(hemi) %>%
          dplyr::summarise(val=mean(lat)),
        y=geobrain %>%
          dplyr::group_by(side) %>%
          dplyr::summarise(val=mean(long))
      )

      gg = gg +
        ggplot2::scale_y_continuous(
          breaks=pos$x$val,
          labels=pos$x$hemi) +
        ggplot2::scale_x_continuous(
          breaks=pos$y$val,
          labels=pos$y$side
        ) +
        ggplot2::labs(x="side", y="hemisphere")
    }else{

      pos = geobrain %>%
        dplyr::group_by(hemi) %>%
        dplyr::summarise_at(dplyr::vars(long,lat),dplyr::funs(mean))

      gg = gg +
        ggplot2::scale_x_continuous(
          breaks=pos$long,
          labels=pos$hemi) +
        ggplot2::scale_y_continuous(breaks=NULL)+
        ggplot2::labs(y=NULL, x="hemisphere")
    }
  }

  gg + theme_brain()

}





