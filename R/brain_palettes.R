#' Generate palettes from the ggseg atlases
#'
#' \code{brain_pal} return HEX colours for the different ggseg atlases.
#'
#' @param name String name of atlas
#' @param n Number of colours to return (or "all" [default])
#' @param direction Direction of  HEX, -1 reverses order
#' @param unname logical, if colours are to be unnamed before returning.
#' Neccessary if applying palette to other data than the brain atlas it comes from.
#'
#' @export
brain_pal <- function(name,n="all",direction=1,unname=FALSE){

  # for global variable notes
  brain.pal.info = ggseg::brain.pal.info
  brain.pals = ggseg::brain.pals

  if(!(name %in% brain.pal.info$atlas)){
    stop(paste(name,"is not a valid palette name for brain.pal\n"))
  }

  if(length(n)>1){
    n = n
  }else if(n == "all"){
    n = seq(1,brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"])
  }else if(n < 3){
    warning("minimal value for n is 3, returning requested palette with 3 different levels\n")
    n = seq(1:3)
  }else if(n > brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"]){
    warning(paste("n too large, allowed maximum for palette",name,"is",
                  brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"],
                  "\nReturning the palette you asked for with that many colors\n"))
    n = seq(1,brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"])
  }else{
    n = seq(1,n)
  }

  pal = brain.pals[[name]]

  if (direction == -1) {
    pal <- rev(pal)
  }

  if(unname){
    pal = unname(pal)
  }

  pal[n]
}

#' Plot the colours of the atlases for selection
#'
#' \code{display.brain.pal} plots all the colours for each atlas.
#'
#' @param name String name of atlas
#' @param n Number of colours to return (or "all" [default])
#'
#' @export
display.brain.pal <- function (name="all",
                               n="all") {

  # for global variable notes
  brain.pal.info = ggseg::brain.pal.info
  brain.pals = ggseg::brain.pals

  pals = do.call(dplyr::bind_rows,
                 lapply(names(brain.pals),
                        function(nm) data.frame(atlas = nm,
                                                colour = brain.pals[[nm]],
                                                x = seq_along(brain.pals[[nm]]),
                                                stringsAsFactors = FALSE)))

  if(name != "all"){
    if(!(name %in% brain.pal.info$atlas)){
      stop(paste(name,"is not a valid palette name for brain.pal\n"))
    }

    if(n == "all") n = seq(1,brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"])

    if(length(n)>1){
      n = n
    }else if(n < 3){
      warning("minimal value for n is 3, returning requested palette with 3 different levels\n")
      n = seq(1,3)
    }else  if(n > brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"]){
      warning(paste("n too large, allowed maximum for palette",name,"is",
                    brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"],
                    "\nReturning the palette you asked for with that many colors\n"))
      n = seq(1:unname(brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"]))
    }

  }else{
    name = brain.pal.info$atlas

    if(length(n) > 1){
      n = n
    }else if(n == "all"){
      n = seq(1, max(brain.pal.info$maxcol))
    }else{
      n = seq(1,n)
    }
  }# if name


  pals %>%
    dplyr::filter(atlas %in% name) %>%
    dplyr::filter(x %in% n) %>%
    ggplot2::ggplot(ggplot2::aes(x=as.numeric(x), y=atlas, fill=I(colour))) +
    ggplot2::geom_tile() + theme_brain() + ggplot2::labs(x="")
}

