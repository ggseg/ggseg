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
#'
#' @examples
#' adapt_scales(dkt, position="stacked", aesthetics="y")
#'
#' @importFrom dplyr group_by summarise summarise_at vars funs
#' @importFrom tidyr unnest
#'
#' @export
adapt_scales = function(geobrain, position = "dispersed", aesthetics = "labs"){

  atlas = ifelse(any(names(geobrain) %in% "atlas"),
                 unique(geobrain$atlas),
                 "unknown")

  if(atlas == "unknown"){
    warning("Unknown atlas, attempting to adapt scales in the blind.")
    stk = list(
      x=geobrain %>%
        dplyr::group_by(hemi) %>%
        dplyr::summarise(val=mean(.lat)),
      y=geobrain %>%
        dplyr::group_by(side) %>%
        dplyr::summarise(val=mean(.long))
    )

    disp = geobrain %>%
      dplyr::group_by(hemi) %>%
      dplyr::summarise_at(dplyr::vars(.long,.lat), list(mean))

    ad_scale <- list(
      stacked =
        list(x = list(breaks = stk$x$val,
                      labels = stk$x$hemi),
             y = list(breaks = stk$y$val,
                      labels = stk$y$side),
             labs = list(y = "side", x = "hemisphere")
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

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("atlas_scale_positions"))
}
