#' Generate palettes from the ggbrain atlases
#'
#' \code{brain_pal} return HEX colours for the different ggbrain atlases.
#'
#' @param name String name of atlas
#' @param n Number of colours to return (or "all" [default])
#' @param direction Direction of  HEX, -1 reverses order
#' @param unname logical, if colours are to be unnamed before returning
#'
#' @export
brain_pal <- function(name,n="all",direction=1,unname=F){

  if(!(name %in% brain.pal.info$atlas)){
    stop(paste(name,"is not a valid palette name for brain.pal\n"))
  }

  if(length(n)>1){
    n = n
  }else   if(n == "all"){
    n = seq(1,brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"])
  }else if(n < 3){
    warning("minimal value for n is 3, returning requested palette with 3 different levels\n")
    n = seq(1:3)
  }else if(n > brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"]){
    warning(paste("n too large, allowed maximum for palette",name,"is",
                  brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"],
                  "\nReturning the palette you asked for with that many colors\n"))
    n = seq(1,brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"])
  }

  pal = brain.pals[[name]][n]

  if (direction == -1) {
    pal <- rev(pal)
  }

  if(unname){
    pal = unname(pal)
  }

  pal
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

  pals = as.data.frame(matrix(ncol=3,nrow=length(brain.pals)))
  for(i in 1:nrow(pals)){
    pals = stats::na.omit(rbind(pals,
                                cbind(names(brain.pals)[i],
                                      brain.pals[[i]],
                                      seq(1,length(brain.pals[[i]])))))
  } #for i
  names(pals) = c("atlas","colour","x")

  if(name != "all"){
    if(!(name %in% brain.pal.info$atlas)){
      stop(paste(name,"is not a valid palette name for brain.pal\n"))
    }

    if(n == "all") n = brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"]

    if(n < 3){
      warning("minimal value for n is 3, returning requested palette with 3 different levels\n")
      n = 3
    }

    if(n > brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"]){
      warning(paste("n too large, allowed maximum for palette",name,"is",
                    brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"],
                    "\nReturning the palette you asked for with that many colors\n"))
      n = unname(brain.pal.info[brain.pal.info$atlas %in% name,"maxcol"])
    }

    pals = pals %>%
      dplyr::filter(atlas %in% name) %>%
      dplyr::filter(x %in% seq(1,n))
  } # if name


  pals %>%
    ggplot2::ggplot(ggplot2::aes(x=as.numeric(x), y=atlas, fill=I(colour))) +
    ggplot2::geom_tile() + theme_brain() + ggplot2::labs(x="")
}

