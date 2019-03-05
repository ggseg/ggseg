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
brain_pal <- function(name=NULL, n="all", direction=1, unname=FALSE){

  info <- brain_pals_info()

  if(!(name %in% info$atlas)){
    stop(paste(name,"is not a valid palette name for brain_pal\n"))
  }

  if(length(n)>1){
    n = n
  }else if(n == "all"){
    n = seq(1,info[info$atlas %in% name,"maxcol"])
  }else if(n < 3){
    warning("minimal value for n is 3, returning requested palette with 3 different levels\n")
    n = seq(1:3)
  }else if(n > info[info$atlas %in% name,"maxcol"]){
    warning(paste("n too large, allowed maximum for palette",name,"is",
                  info[info$atlas %in% name,"maxcol"],
                  "\nReturning the palette you asked for with that many colors\n"))
    n = seq(1,info[info$atlas %in% name,"maxcol"])
  }else{
    n = seq(1,n)
  }

  pal = brain_pals[[name]]

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
#' \code{display_brain_pal} plots all the colours for each atlas.
#'
#' @param name String name of atlas
#' @param n Number of colours to return (or "all" [default])
#' @importFrom dplyr row_number bind_rows group_by mutate filter
#' @importFrom ggplot2 ggplot geom_tile labs aes
#' @export
#' @examples
#' display_brain_pal()
#' display_brain_pal("dkt", 1:8)
display_brain_pal <- function (name="all",
                               n="all") {


  info <- brain_pals_info()
  if(name == "all"){
    name = info$atlas
  }else if(!(any(name %in% info$atlas))){

    name = paste0("'", name[!(name %in% info$atlas)], "'", collapse=", ")

    stop(paste0("Did you specify the correct atlases, could not find ",
                name))
  }

  pals = do.call(bind_rows,
                 lapply(info$atlas,
                        function(nm) data.frame(atlas = nm,
                                                colour = brain_pal(nm, n=n, unname=T),
                                                stringsAsFactors = FALSE))
  )

  pals %>%
    group_by(atlas) %>%
    mutate(x = row_number()) %>%
    filter(atlas %in% name) %>%

    ggplot(aes(x=x, y=atlas, fill=I(colour))) +
    geom_tile() +
    theme_brain() +
    labs(x="")
}


#' Get info on brain palettes
#'
#' @return data.frame
#' @export
#'
#' @examples
#' brain_pals_info()

brain_pals_info <- function(){
  data.frame(
    atlas=names(unlist(lapply(brain_pals,length))),
    maxcol=unname(unlist(lapply(brain_pals,length))),
    category="qual",
    colorblind=FALSE,
    stringsAsFactors = FALSE)
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("info","brain_pals"))
}
