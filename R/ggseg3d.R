#' Plot 3D brain parcellations
#'
#' \code{ggseg3d} plots and returns a plotly mesh3d object.
#' @author Athanasia Mowinckel and Didac Pineiro
#'
#'
#' @param atlas3d Either a string with the name of atlas to use,
#' or a data.frame containing atlas information (i.e. pre-loaded atlas).
#' @param plot.areas Character vector, plots only areas specified in the vector.
#' @param hemisphere String. Hemisphere to plot. Either "left" or "right"[default].
#' @param surface String. Which surface to plot. Either "pial","white", or "inflated"[default]
#' @param remove.axes Logical. Should axis and grid be removed.
#' @param name String. Quoted name of column in atlas3d that should be used to name traces
#' @param facecolour String. Quoted name of column from which colour should be supplied

#'
#' @details
#' \describe{
#'
#' \item{`dkt3d`}{
#' The Desikan-Killiany Cortical Atlas [default], Freesurfer cortical segmentations, in 3dmesh format}
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
#' ggseg3d(surface="pial")
#' ggseg3d(remove.axes = F)
#'
#' @seealso [ggplot()], [aes()], [geom_polygon()], [coord_fixed()] from the ggplot2 package
#'
#' @export
ggseg3d <- function(data=NULL, atlas="dkt3d", surface = "inflated", hemisphere = "right",
                    label = "area", text=NULL, facecolour="colour",
                    pal.colours = c("firebrick","goldenrod"), pal.values=NULL, na.color = "darkgrey",
                    remove.axes = TRUE, show.legend = FALSE, ...) {

  # Axix removal
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    showbackground = FALSE
  )


  # Grab the atlas, even if it has been provided as character string
  atlas3d = if(!is.character(atlas)){
    atlas
  }else{
    get(atlas)
  }

  # grab the correct surface and hemisphere
  atlas3d = dplyr::filter(atlas3d[[surface]], hemi %in% hemisphere)

  # If data has been supplied, merge it
  if(!is.null(data)){

    # Find columns they have in common
    cols = names(atlas3d)[names(atlas3d) %in% names(data)]

    # Merge the brain with the data
    atlas3d = atlas3d %>%
      dplyr::full_join(data, by = cols, copy=TRUE)

    # Find if there are instances of those columns that
    # are not present in the atlas. Maybe mispelled?
    errs = atlas3d %>%
      dplyr::filter(is.na(mesh)) %>%
      dplyr::select(!!cols) %>%
      dplyr::distinct() %>%
      tidyr::unite_("tt", cols, sep = " - ") %>%
      dplyr::summarise(value = paste0(tt, collapse = ", "))

    if(errs != ""){
      warning(paste("Some data is not merged properly into the atlas. Check for spelling mistakes in:",
                    errs$value))

      atlas3d = atlas3d %>%
        dplyr::filter(!is.na(atlas3d))
    }
  }

  # If facecolour column is numeric, calculate the gradient
  if(is.numeric(data[,facecolour])){

    atlas3d$new_col = scales::gradient_n_pal(pal.colours, pal.values, "Lab")(
      scales::rescale(x=atlas3d[,facecolour])
    )

    atlas3d$new_col[is.na(atlas3d$new_col)] = ifelse(
      grepl("^#", na.color), na.color, gplots::col2hex(na.color))

    fill = "new_col"
  }else{
    fill = facecolour
  }

  # initiate plot
  p = plotly::plot_ly(...)

  if(show.legend & is.numeric(atlas3d[,facecolour])){
    # word around to get legend
    p = plotly::add_bars(x=seq(1, nrow(atlas3d)),
                         y=atlas3d[,facecolour],
                         color=atlas3d[,facecolour],
                         visible="legendonly")
  }

  # add one trace per file inputed
  for(tt in 1:nrow(atlas3d)){

    col = rep(atlas3d[tt, fill], length(atlas3d[[tt,"mesh"]]$it[1,]))

    nm = rep(atlas3d[tt, label], length(atlas3d[[tt,"mesh"]]$it[1,]))

    txt = if(is.null(text)){
      text
    }else{
      rep(paste0(text, ": ", atlas3d[tt, text]), length(atlas3d[[tt,"mesh"]]$it[1,]))
    }

    p = plotly::add_trace(p,
                          x = atlas3d[[tt,"mesh"]]$vb["xpts",],
                          y = atlas3d[[tt,"mesh"]]$vb["ypts",],
                          z = atlas3d[[tt,"mesh"]]$vb["zpts",],

                          i = atlas3d[[tt,"mesh"]]$it[1,]-1,
                          j = atlas3d[[tt,"mesh"]]$it[2,]-1,
                          k = atlas3d[[tt,"mesh"]]$it[3,]-1,

                          facecolor = col,
                          type = "mesh3d",
                          text = txt,
                          name = nm
    )
  }

  if(remove.axes){
    p = plotly::layout(p,
                       scene = list(
                         xaxis=ax,
                         yaxis=ax,
                         zaxis=ax))
  }

  p
}
