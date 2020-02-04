#' Scale ggseg plot axes.
#'
#' \code{adapt_scales} returns a list of coordinate breaks and labels
#' for axes or axes label manipulation of the ggseg brain atlases.
#'
#' @param geobrain a data.frame containing atlas information.
#' @param position String choosing how to view the data. Either "dispersed"[default] or "stacked".
#' @param aesthetics String of which aesthetics to adapt scale of, either "x","y", or "labs".
#'
#' @return nested list
adapt_scales = function(geobrain, position = "dispersed", aesthetics = "labs"){

  atlas = ifelse(any(names(geobrain) %in% "atlas"),
                 unique(geobrain$atlas),
                 "unknown")

   if(!".pos" %in% names(geobrain)){
     y <- dplyr::group_by(geobrain, hemi)
     y <- dplyr::summarise(y, val=gap(.lat))

     x <- dplyr::group_by(geobrain, side)
     x <- dplyr::summarise(x, val=gap(.long))

    stk = list(
      y = y,
      x = x
    )

    disp <- dplyr::group_by(geobrain, hemi)
    disp <- dplyr::summarise_at(disp, dplyr::vars(.long,.lat), list(gap))

    ad_scale <- list(
      stacked =
        list(x = list(breaks = stk$x$val,
                      labels = stk$x$side),
             y = list(breaks = stk$y$val,
                      labels = stk$y$hemi),
             labs = list(y = "hemisphere", x = "side")
        ),

      dispersed =
        list(x = list(breaks = disp$.long,
                      labels = disp$hemi),
             y = list(breaks = NULL,
                      labels = ""),
             labs = list(y = NULL, x = "hemisphere")
        )
    )
  }else{
    ad_scale = geobrain$.pos[[1]]
  }

 if(is.null(ad_scale[[position]])){
 warning("No such position for this atlas. Returning only available scale.")
  ad_scale[[names(ad_scale)]]
 }else{
    ad_scale[[position]][[aesthetics]]
  }
}

gap <- function(x){
(min(x) + max(x)) / 2
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("atlas_scale_positions"))
}
