#' Plot brain parcellations
#'
#' \code{ggbrain} plots and returns a ggplot object of plotted
#' aparc areas.
#' @author Athanasia Mowinckel and Didac Pineiro
#'
#' @param data A data.frame to use for plot aesthetics. Must include a
#' column called "area" corresponding to aparc areas.
#'
#' @param atlas Either a string with the name of atlas to use,
#' or a data.frame containing atlas information (i.e. pre-loaded atlas).
#' @param plot.areas Character vector, plots only areas specified in the vector.
#' @param mapping ggplot2 aethetics (cannot include x and y aethetics)
#' @param hemisphere String to choose hemisphere to plot. Any of c("left","right")[default].
#' @param view String to choose view of the data. Any of c("lateral","medial")[default].
#' @param position String choosing how to view the data. Either "dispersed"[default] or "stacked".
#' @param na.fill String or HEX code for the fill of the area without values
#' @param na.alpha Transparency for the fill of the area without values
#' @param colour String or HEX code for the colour of the outlines of each area
#' @param size Numeric, size of the line outlining each area
#' @param show.legend logical, toggle on or off legend.
#' @param ...
#'
#' @details
#' \describe{
#'
#' \item{`dkt`}{
#' The Desikan-Killiany Cortical Atlas [default]. Part of Freesurfer segmentations.}
#'
#' \item{`yeo7`}{
#' Seven resting-state networks from Yeo et al. 2011, J. Neurophysiology}
#'
#' \item{`yeo17`}{
#' Seventeen resting-state networks from Yeo et al. 2011, J. Neurophysiology}
#' }
#'
#'
#' @return a ggplot object
#'
#' @importFrom dplyr filter summarise mutate left_join group_by
#' @importFrom ggplot2 ggplot aes geom_polygon coord_fixed scale_y_continuous scale_x_continuous labs
#' @importFrom stats na.omit
#' @importFrom tidyr gather spread
#' @importFrom magrittr "%>%"
#'
#' @examples
#' ggbrain()
#' ggbrain(na.fill = "transparent",mapping=aes(fill=area))
#' ggbrain(colour="black", size=.7, mapping=aes(fill=area)) + theme_void()
#' ggbrain(atlas=yeo7)
#'
#' @seealso [ggplot()], [aes()], [geom_polygon()], [coord_fixed()] from the ggplot2 package
#'
#' @export

ggbrain = function(data = NULL,atlas="dkt",
                   plot.areas=NULL,
                   position="dispersed",
                   view=c("lateral","medial"),
                   hemisphere = c("right","left"),
                   mapping = NULL, na.alpha=NA,
                   colour="white", size=.5, show.legend = NA,
                   na.fill="grey",...){

  # Load the segmentation to use
  geobrain = if(class(atlas) == "data.frame"){
                    atlas
    }else{
         eval(as.name(atlas))
    } %>%
    dplyr::filter(hemi %in% hemisphere) %>%
    dplyr::filter(side %in% view)

  if(position=="stacked"){
    # Alter coordinates of the left side to stack ontop of right side
    stack = geobrain %>%
      dplyr::filter(hemi %in% "right") %>%
      dplyr::summarise(ymax=max(lat),xmax=max(long)) %>%
      round(0)+1

    geobrain = geobrain %>%
      dplyr::mutate(lat=ifelse(hemi %in% "left",
                               lat + stack$ymax, lat),
                    long=ifelse(hemi %in% "left",
                                long - stack$xmax, long)
      )

    # If stacked, and lateral view only, change coordinates some more for stacking.
    if(length(view)==1){
      if(view=="lateral"){
        stack = geobrain %>%
          dplyr::filter(hemi %in% "left") %>%
          dplyr::summarise(xmin=min(long)) %>%
          round(0)

        geobrain = geobrain %>%
          dplyr::mutate(long=ifelse(hemi %in% "left",
                                    long - stack$xmin, long)
          )
      }else if(view=="medial"){
        stack = geobrain %>%
          dplyr::filter(hemi %in% "left") %>%
          dplyr::summarise(xmax=max(long)) %>%
          round(0)

        geobrain = geobrain %>%
          dplyr::mutate(long=ifelse(hemi %in% "left",
                                    long + stack$xmax, long)
          )

      } # which view
    } # if view
  } # If stacked


    # Filter data to single area if that is all you want.
    if(!is.null(plot.areas)){
      if(any(!plot.areas %in% geobrain$area)){
        stop(paste("There is no", plot.areas,
                   "in", atlas,"data. Check spelling. Options are:",
                   paste0(geobrain$area %>% unique,collapse=", ")))
      }
      geobrain = geobrain %>% dplyr::filter(area %in% plot.areas)
    }


    gg = ggplot2::ggplot(data = geobrain, ggplot2::aes(x=long, y=lat, group=id)) +
      ggplot2::geom_polygon(
        size=size,
        colour=colour,
        fill=na.fill,
        alpha=na.alpha) +
      ggplot2::coord_fixed()

    if(!is.null(mapping)){

      geoData = geobrain
      if(!is.null(data))
          geoData = suppressWarnings(suppressMessages(
            geoData %>%
          dplyr::left_join(data)
        ))

      gg = gg +
        ggplot2::geom_polygon(
          data=geoData %>% stats::na.omit(),
          mapping=mapping,
          size=size,
          colour=colour,
          show.legend = show.legend)
    }


    pos = geobrain %>%
      dplyr::group_by(hemi,side) %>%
      dplyr::summarise(y=mean(lat), x=mean(long)) %>%
      tidyr::gather(key,val, -c(1:2)) %>%
      dplyr::group_by(hemi,key) %>%
      dplyr::summarise(m=mean(val)) %>%
      tidyr::spread(key,m)

    if(position == "stacked"){
      gg = gg +
        ggplot2::scale_y_continuous(
          breaks=pos$y,
          labels=pos$hemi) +
        ggplot2::scale_x_continuous(breaks=NULL) +
        ggplot2::labs(x=NULL, y="Hemisphere")
    }else{
      gg = gg +
        ggplot2::scale_x_continuous(
          breaks=pos$x,
          labels=pos$hemi) +
        ggplot2::scale_y_continuous(breaks=NULL)+
        ggplot2::labs(y=NULL, x="Hemisphere")
    }

    gg + theme_brain()

  }





