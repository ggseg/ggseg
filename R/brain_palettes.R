
pal_list <- c( 40,7,17,8)
names(pal_list) <- c("dkt","yeo7","yeo17","aseg")

brain.pal.info <- data.frame(maxcol=unname(pal_list),
                             category="qual",
                             row.names=names(pal_list),
                             colorblind=FALSE)

brain.pals = list(
  #dkt  = c(),
  #aseg =  c(),
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
            "17" = "#cb4051ff")
)


brain.pal <- function(n,name){

  if(!(name %in% names(pal_list))){
    stop(paste(name,"is not a valid palette name for brain.pal\n"))
  }

  if(n < 3){
    warning("minimal value for n is 3, returning requested palette with 3 different levels\n")
    n=3
  }

  if(n > pal_list[name]){
    warning(paste("n too large, allowed maximum for palette",name,"is",unname(pal_list[name]),
                  "\nReturning the palette you asked for with that many colors\n"))
    n = unname(pal_list[name])
  }

  brain.pals[[name]][1:n]
}


display.brain.pal <-
  function (n="all", type="all", select=NULL, exact.n=TRUE, colorblindFriendly=FALSE) {

    if(!(name %in% names(pal_list))){
      stop(paste(name,"is not a valid palette name for brain.pal\n"))
    }

    if(n=="all") n = length(brain.pals[[name]])
    if( n < 3 ){
      warning("minimal value for n is 3, displaying requested palette with 3 different levels\n")
      n = 3
    }

    if(n > pal_list[name]){
      warning(paste("n too large, allowed maximum for palette",name,"is",unname(pal_list[name]),
                    "\nDisplaying the palette you asked for with that many colors\n"))
      n = unname(pal_list[name])
    }
    pals = as.data.frame(matrix(ncol=3,nrow=length(brain.pals)))
    for(i in 1:nrow(pals)){
      pals = na.omit(rbind(pals,
                           cbind(names(brain.pals)[i],
                                 brain.pals[[i]],
                                 seq(1,length(brain.pals[[i]])))))
    }
    names(pals) = c("atlas","colour","x")

    ggplot(pals, aes(x=as.numeric(x), y=atlas, fill=I(colour))) +
      geom_tile() + theme_brain() + labs(x="")
  }
