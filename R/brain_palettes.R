
brain.pals = list(
  dkt  = c(`banks superior temporal`	= "#196428ff",
           `caudal anterior cingulate`	= "#7D64A0ff",
           `caudal middle frontal`	= "#641900ff",
           cuneus	= "#DC1464ff",
           entorhinal	= "#DC140Aff",
           fusiform	= "#B4DC8Cff",
           `inferior parietal`	= "#DC3CDCff",
           `inferior temporal`	= "#B42878ff",
           `isthmus cingulate`	= "#8C148Cff",
           lateraloccipital	= "#141E8Cff",
           `lateralorbito frontal`	= "#234B32ff",
           lingual	= "#E18C8Cff",
           `medial orbito frontal`	= "#C8234Bff",
           `middle temporal`	= "#A06432ff",
           parahippocampal	= "#14DC3Cff",
           `para central`	= "#3CDC3Cff",
           `pars opercularis`	= "#DCB48Cff",
           `pars orbitalis`	= "#146432ff",
           `pars triangularis`	= "#DC3C14ff",
           pericalcarine	= "#78643Cff",
           `post central`	= "#DC1414ff",
           `posterior cingulate`	= "#DCB4DCff",
           `pre central`	= "#3C14DCff",
           precuneus	= "#A08CB4ff",
           `rostral anterior cingulate`	= "#50148Cff",
           `rostral middle frontal`	= "#4B327Dff",
           `superior frontal`	= "#14DCA0ff",
           `superior parietal`	= "#14B48Cff",
           `superior temporal`	= "#8CDCDCff",
           supramarginal	= "#50A014ff",
           `frontal pole`	= "#640064ff",
           `temporal pole`	= "#464646ff",
           `transverse temporal`	= "#9696C8ff",
           insula	= "#FFC020ff"),
  yeo7 = c(visual = "#a153a2ff",
           somatomotor = "#6fabd2ff",
           `dorsal attention`= "#2c8b4bff",
           `ventral attention` = "#b77fb4ff",
           limbic = "#e7edcaff",
           frontoparietal = "#edaf5eff",
           default = "#e27283ff"),
  yeo17 = c("1" = "#762284ff",
            "2" = "#fc2a1cff",
            "3" = "#4984b2ff",
            "4" = "#36cca5ff",
            "5" = "#4d9a42ff",
            "6" = "#0d7418ff",
            "7" = "#c145f6ff",
            "8" = "#fd9bd4ff",
            "9" = "#ddf7a9ff",
            "10" = "#798639ff",
            "11" = "#778dafff",
            "12" = "#e39333ff",
            "13" = "#86334aff",
            "14" = "#143efaff",
            "15" = "#041b80ff",
            "16" = "#fff937ff",
            "17" = "#cb4051ff"),
  aseg =  c(Amygdala	= "#67FFFFff",
            Caudate	= "#7ABADCff",
            Hippocampus	= "#DCD814ff",
            `Lateral Ventricle`	= "#781286ff",
            Pallidum	= "#0D30FFff",
            Putamen	= "#EC0DB0ff",
            `Thalamus Proper`	= "#00760Eff",
            VentralDC	= "#A52A2Aff")
)


brain.pal.info <- data.frame(atlas=names(unlist(lapply(brain.pals,length))),
                             maxcol=unname(unlist(lapply(brain.pals,length))),
                             category="qual",
                             colorblind=FALSE)

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

  pal = brain.pals[[name]][1:n]

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

