#' Plot 3D brain parcellations
#'
#' \code{ggseg3d} plots and returns a plotly mesh3d object.
#' @author Athanasia Mowinckel and Didac Pineiro
#'
#' @param .data A data.frame to use for plot aesthetics. Must include a
#' column called "area" corresponding to areas.
#' @param atlas Either a string with the name of a 3d atlas to use.
#' @param hemisphere String. Hemisphere to plot. Either "left" or "right"[default],
#' can also be "subcort".
#' @param surface String. Which surface to plot. Either "pial","white", or "inflated"[default]
#' @param remove.axes Logical. Should axis and grid be removed.
#' @param label String. Quoted name of column in atlas/data that should be used to name traces
#' @param text String. Quoated name of column in atlas/data that should be added as extra
#' information in the hover text.
#' @param colour String. Quoted name of column from which colour should be supplied
#' @param palette String. Either name of paletteer palette or vector of hex colours,
#' used if colour is numeric.
#' @param na.colour String. Either name, hex of RGB for colour of NA in colour.
#' @param na.alpha Numeric. A number between 0 and 1 to control transparency of NA-regions.
#' @param show.legend Logical. Toggle legend if colour is numeric.
#' @param camera String of "medial" or "lateral", or list of x, y, and z positions
#' for initial camera position.
#'
#' @details
#' \describe{
#' \strong{Available atlases:}
#'   \item{`dkt_3d`}{
#'     The Desikan-Killiany Cortical Atlas [default], Freesurfer cortical segmentations, in 3dmesh format
#'   }
#'   \item{`yeo7_3d`}{
#'     Yeo 2011 7 resting-state networks, in 3dmesh format
#'   }
#'   \item{`yeo17_3d`}{
#'     Yeo 2011 17 resting-state networks, in 3dmesh format
#'   }
#' }
#'
#' \strong{Available surfaces:}
#' \itemize{
#' \item `inflated` Fully inflated surface
# \item `semi-inflated` Semi-inflated surface
#' \item `white` white matter surface
#'  }
#'
#' @return a plotly object
#'
#' @importFrom dplyr filter full_join select distinct summarise
#' @importFrom gplots col2hex
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom scales colour_ramp brewer_pal rescale gradient_n_pal
#' @importFrom tidyr unite_
#' @importFrom magrittr "%>%"
#'
#' @examples
#' library(ggplot2)
#' ggseg3d()
#' ggseg3d(surface="white")
#' ggseg3d(surface="inflated")
#' ggseg3d(remove.axes = FALSE)
#'
#' @seealso \code{\link[plotly]{plot_ly}}, \code{\link[plotly]{add_trace}}, \code{\link[plotly]{layout}}, the plotly package
#'
#' @export
ggseg3d <- function(.data=NULL, atlas="dkt_3d", surface = "LCBC", hemisphere = c("right","subcort"),
                    label = "area", text = NULL, colour = "colour",
                    palette = NULL, na.colour = "darkgrey", na.alpha = 1,
                    remove.axes = TRUE, show.legend = TRUE,
                    camera = "lateral") {


  # Grab the atlas, even if it has been provided as character string
  atlas3d = if(!is.character(atlas)){
    atlas
  }else{
    get(atlas)
  }


  if(!any(atlas3d$surf %in% surface)){
    stop(paste0("There is no surface '",surface,"' in this atlas." ))
  }

  if(!any(atlas3d$hemi %in% hemisphere)){
    stop(paste0("There is no data for the ",hemisphere," hemisphere in this atlas." ))
  }

  # grab the correct surface and hemisphere
  atlas3d = atlas3d %>%
    dplyr::filter(surf %in% surface & hemi %in% hemisphere) %>%
    tidyr::unnest()

  # If data has been supplied, merge it
  if(!is.null(.data)){

    # Find columns they have in common
    cols = names(atlas3d)[names(atlas3d) %in% names(.data)]

    # Merge the brain with the data
    atlas3d = atlas3d %>%
      dplyr::full_join(.data, by = cols, copy=TRUE)

    # Find if there are instances of those columns that
    # are not present in the atlas. Maybe mispelled?
    errs = atlas3d %>%
      dplyr::filter(unlist(lapply(atlas3d$mesh, is.null))) %>%
      dplyr::select(!!cols) %>%
      dplyr::distinct() %>%
      tidyr::unite_("tt", cols, sep = " - ") %>%
      dplyr::summarise(value = paste0(tt, collapse = ", "))

    if(errs != ""){
      warning(paste("Some data is not merged properly into the atlas. Check for spelling mistakes in:",
                    errs$value))

      atlas3d = atlas3d %>%
        dplyr::filter(!unlist(lapply(atlas3d$mesh, is.null)))
    }
  }

  # If colour column is numeric, calculate the gradient
  if(is.numeric(unlist(atlas3d[,colour]))){

    if(is.null(palette)){
      palette = c("skyblue", "dodgerblue")
    }

    pal.colours = palette
    # pal.colours = if(length(palette)==1){
    #   if(!palette %in% unlist(lapply(paletteers$palettes, function(x) x$palette))){
    #     stop(paste0("No such palette '", palette, "'. Choose one from the paletteer package."))
    #   }
    #
    #   get_paletteer(palette)
    # }else{
    #   palette
    # }


    pal.colours = data.frame(seq(0,1, length.out = length(pal.colours)),
                             pal.colours, stringsAsFactors = F)
    names(pal.colours) = NULL

    atlas3d$new_col = scales::gradient_n_pal(pal.colours[,2], NULL,"Lab")(
      scales::rescale(x=unlist(atlas3d[,colour])))

    fill = "new_col"
  }else{
    fill = colour
  }


  # initiate plot
  p = plotly::plot_ly()

  # add one trace per file inputed
  for(tt in 1:nrow(atlas3d)){

    col = rep(unlist(atlas3d[tt, fill]), length(atlas3d$mesh[[tt]]$it[1,]))

    col = ifelse(is.na(col), na.colour, col)

    op = ifelse(is.na(unlist(atlas3d[tt, fill])), na.alpha, 1)

    txt = if(is.null(text)){
      text
    }else{
      paste0(text, ": ", unlist(atlas3d[tt, text]))
    }

    p = plotly::add_trace(p,
                          x = atlas3d$mesh[[tt]]$vb["xpts",],
                          y = atlas3d$mesh[[tt]]$vb["ypts",],
                          z = atlas3d$mesh[[tt]]$vb["zpts",],

                          i = atlas3d$mesh[[tt]]$it[1,]-1,
                          j = atlas3d$mesh[[tt]]$it[2,]-1,
                          k = atlas3d$mesh[[tt]]$it[3,]-1,

                          facecolor = col,
                          type = "mesh3d",
                          text = txt,
                          showscale = FALSE,
                          opacity = op,
                          name = unlist(atlas3d[tt, label])
    )
  }

  # work around to get legend
  if(show.legend & is.numeric(unlist(atlas3d[,colour]))){

    p = plotly::add_trace(p,
                          x = c(min(atlas3d$mesh[[1]]$vb["xpts",]), max(atlas3d$mesh[[1]]$vb["xpts",])),
                          y = c(min(atlas3d$mesh[[1]]$vb["ypts",]), max(atlas3d$mesh[[1]]$vb["ypts",])),
                          z = c(min(atlas3d$mesh[[1]]$vb["zpts",]), max(atlas3d$mesh[[1]]$vb["zpts",])),

                          intensity = c(min(atlas3d[,colour],na.rm=T),
                                        max(atlas3d[,colour],na.rm=T)),
                          colorscale = pal.colours,
                          type = "mesh3d"
    )
  }

  if(remove.axes){
    # Axix removal
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE,
      showbackground = FALSE
    )

    p = plotly::layout(p,
                       scene = list(
                         xaxis=ax,
                         yaxis=ax,
                         zaxis=ax,
                         plot_bgcolor='transparent',
                         paper_bgcolor='transparent'))
  }

  views = if(class(camera) != "list"){
    switch(camera,
           "lateral" = list(eye = list(x = 2, y = 0, z = 1)),
           "medial" = list(eye = list(x = -2.25, y = -0.5, z = -0.5))
    )
  }else{
    camera
  }


  # create final plotly plot
  p %>%
    plotly::layout(
      scene = list(camera = views)
    )
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("tt", "surf", "mesh", "new_col"))
}
