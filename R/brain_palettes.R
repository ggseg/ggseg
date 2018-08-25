
pal_list <- c( 40,7,17,8)
names(pal_list) <- c("dkt","yeo7","yeo17","aseg")

brain.pal.info <- data.frame(maxcol=unname(pal_list),
                             category="qual",
                             row.names=names(pal_list),
                             colorblind=FALSE)

brain.pals = list(
dkt  = c(`medial wall`	= "#19051900"
          `banks superior temporal`	= "#19642800"
          `caudal anterior cingulate`	= "#7D64A00"0
          `caudal middle frontal`	= "#64190000"
          cuneus	= "#DC146400"
          entorhinal	= "#DC140A00"
          fusiform	= "#B4DC8C00"
          `inferior parietal`	= "#DC3CDC00"
          `inferior temporal`	= "#B4287800"
          `isthmus cingulate`	= "#8C148C00"
          lateraloccipital	= "#141E8C00"
          `lateralorbito frontal`	= "#234B3200"
          lingual	= "#E18C8C00"
          `medial orbito frontal`	= "#C8234B00"
          `middle temporal`	= "#A0643200"
          parahippocampal	= "#14DC3C00"
          `para central`	= "#3CDC3C00"
          `pars opercularis`	= "#DCB48C00"
          `pars orbitalis`	= "#14643200"
          `pars triangularis`	= "#DC3C1400"
          pericalcarine	= "#78643C00"
          `post central`	= "#DC141400"
          `posterior cingulate`	= "#DCB4DC00"
          `pre central`	= "#3C14DC00"
          precuneus	= "#A08CB400"
          `rostral anterior cingulate`	= "#50148C00"
          `rostral middle frontal`	= "#4B327D00"
          `superior frontal`	= "#14DCA000"
          `superior parietal`	= "#14B48C00"
          `superior temporal`	= "#8CDCDC00"
          supramarginal	= "#50A01400"
          `frontal pole`	= "#64006400"
          `temporal pole`	= "#46464600"
          `transverse temporal`	= "#9696C800"
          insula	= "#FFC02000"),
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
  aseg =  c(Amygdala	= "#67FFFF00"
            Caudate	= "#7ABADC00"
            Hippocampus	= "#DCD81400"
            `Lateral Ventricle`	= "#78128600"
            Pallidum	= "#0D30FF00"
            Putamen	= "#EC0DB000"
            `Thalamus Proper`	= "#00760E00"
            VentralDC	= "#A52A2A00")
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

display.brain.pal<-function(n,name){
  if(!(name %in% names(pal_list))){
    stop(paste(name,"is not a valid palette name for brain.pal\n"))
  }

  if( n < 3 ){
    warning("minimal value for n is 3, displaying requested palette with 3 different levels\n")
    n = 3
  }

  if(n > pal_list[name]){
    warning(paste("n too large, allowed maximum for palette",name,"is",unname(pal_list[name]),
                  "\nDisplaying the palette you asked for with that many colors\n"))
    n = unname(pal_list[name])
  }

  image(1:n,1,as.matrix(1:n),col=brain.pal(n,name),
        xlab=paste(name,"(qualitative)"),ylab="",xaxt="n",yaxt="n",bty="n")
}

display.brewer.all <-
  function (n=NULL, type="all", select=NULL, exact.n=TRUE, colorblindFriendly=FALSE) {
    gaplist <- ""

    totallist <- c(divlist, gaplist, quallist, gaplist, seqlist)
    names(totallist) <- c(names(divlist),"gap1",names(quallist),"gap2",names(seqlist))
    gapnum <- max(c(divnum,palnum,seqnum))
    totnum <- c(divnum, gapnum, palnum, gapnum, seqnum)
    names(totnum) <- names(totallist)

    if (!(type %in% c("div","qual","seq","all"))) {
      stop(paste(type, "is not a valid name for a color list\n"))
    }
    colorlist <- switch(type, div=divlist,
                        qual=quallist, seq=seqlist,
                        all=totallist)
    maxnum <- switch(type, div=divnum,
                     qual=palnum,
                     seq=seqnum,
                     all=totnum)
    if(!is.null(select)){colorlist <- colorlist[select]
    maxnum <- maxnum[select]
    if(any(is.na(colorlist)))
      stop(paste("Illegal value(s) of select: ",
                 paste(select[is.na(colorlist)],
                       collapse=" ")))
    }

    if (colorblindFriendly) {
      colorlist <- colorlist[names(colorlist) %in% c(colorblindlist,"gap1","gap2")]
      maxnum <- maxnum[names(maxnum) %in% c(colorblindlist,"gap1","gap2")]
    }





    palattr <-  switch(type,  qual="qualitative",  div
                       ="divergent", seq="sequential",
                       all="qualitative+divergent+sequential")

    if(is.null(n))n <- maxnum
    if(length(n)==1)n <- rep(n, length(colorlist))

    if(exact.n){
      keep <- n<=maxnum
      colorlist <- colorlist[keep]
      n <- n[keep]
      maxnum <- maxnum[keep]
    }



    if (any(n < 3) | exact.n & any(n>maxnum)|
        length(n)!=length(colorlist)){
      warning("Illegal vector of color numbers")
      print(paste(n, collapse=" "))
    }
    n[n<3] <- 3
    n[n>maxnum] <- maxnum[n>maxnum]

    nr <- length(colorlist)
    nc <- max(n)

    ylim <- c(0,nr)
    oldpar <- par(mgp=c(2,0.25,0))
    on.exit(par(oldpar))
    plot(1,1,xlim=c(0,nc),ylim=ylim,type="n", axes=FALSE, bty="n",
         xlab="",ylab="")
    for(i in 1:nr)
    {nj <- n[i]
    if (colorlist[i]=="") next
    shadi <- brain.pal(nj, colorlist[i])
    rect(xleft=0:(nj-1), ybottom=i-1, xright=1:nj, ytop=i-0.2, col=shadi,
         border="light grey")
    }
    text(rep(-0.1,nr),(1:nr)-0.6, labels=colorlist, xpd=TRUE, adj=1)
  }
