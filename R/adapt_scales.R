#' Scale ggseg plot axes.
#'
#' \code{adapt_scales} returns a list of coordinate breaks and labels
#' for axes or axes label manipulation of the ggseg brain atlases.
#'
#' @param atlas a data.frame containing atlas information.
#' @param position String choosing how to view the data. Either "dispersed"[default] or "stacked".
#' @param aesthetics String of which aesthetics to adapt scale of, either "x","y", or "labs".
#'
#' @return nested list
#'
#' @examples
#' adapt_scales(dkt, position="stacked", aesthetics="y")
#'
#' @importFrom dplyr group_by summarise summarise_at vars funs
#'
#' @export
adapt_scales = function(atlas = dkt, position = "dispersed", aesthetics = "labs"){

  geobrain = atlas

  atlas = ifelse(any(names(atlas) %in% "atlas"),
                 unique(atlas$atlas),
                 "unknown")

  ad_scale = switch(
    atlas,
    "dkt" = {
      list(stacked = list(x = list(breaks = c(2.5, 9.2),
                                   labels = c("lateral","medial")),
                          y = list(breaks = c(2.5, 7.5),
                                   labels = c("left","right")),
                          labs = list(x = "side",
                                      y = "hemisphere")),
           dispersed = list(x = list(breaks = c(6.2, 19),
                                     labels = c("left","right")),
                            y = list(breaks = NULL,
                                     labels = NULL),
                            labs = list(x = "hemisphere",
                                        y = NULL))
      )
    },

    "yeo7" = {
      list(stacked = list(x = list(breaks = c(0.55, 2.1),
                                   labels = c("lateral","medial")),
                          y = list(breaks = c(0.45, 1.8),
                                   labels = c("left","right")),
                          labs = list(x = "side",
                                      y = "hemisphere")),
           dispersed = list(x = list(breaks = c(1.42, 4.25),
                                     labels = c("left","right")),
                            y = list(breaks = NULL,
                                     labels = NULL),
                            labs = list(x = "hemisphere",
                                        y = NULL))
      )
    },

    "yeo17" = {
      list(stacked = list(x = list(breaks = c(0.35, 1.4),
                                   labels = c("lateral","medial")),
                          y = list(breaks = c(0.3, 1.35),
                                   labels = c("left","right")),
                          labs = list(x = "side",
                                      y = "hemisphere")),
           dispersed = list(x = list(breaks = c(.9, 2.75),
                                     labels = c("left","right")),
                            y = list(breaks = NULL,
                                     labels = NULL),
                            labs = list(x = "hemisphere",
                                        y = NULL))
      )
    },

    "aseg" = {
      list(dispersed = list(x = list(breaks = c(9.2, 11.4),
                                     labels = c("left","right")),
                            y = list(breaks = NULL,
                                     labels = NULL),
                            labs = list(x = "hemisphere",
                                        y = NULL))
      )
    },

    "midsagittal" = {
      list(dispersed = list(x = list(breaks = NULL,
                                     labels = NULL),
                            y = list(breaks = NULL,
                                     labels = NULL),
                            labs = list(x = "midline sagittal",
                                        y = NULL))
      )
    },

    "glasser" = {
      list(stacked = list(x = list(breaks = c(1.1, 4.5),
                                   labels = c("lateral","medial")),
                          y = list(breaks = c(0.9, 3.4),
                                   labels = c("left","right")),
                          labs = list(x = "side",
                                      y = "hemisphere")),
           dispersed = list(x = list(breaks = c(3, 9.1),
                                     labels = c("left","right")),
                            y = list(breaks = NULL,
                                     labels = NULL),
                            labs = list(x = "hemisphere",
                                        y = NULL))
      )
    },

    "jhu" = {
      list(dispersed = list(x = list(breaks = c(7.5, 10.75, 14),
                                     labels = c("upper coronal","axial", "lower coronal")),
                            y = list(breaks = NULL,
                                     labels = NULL),
                            labs = list(x = "side",
                                        y = NULL))
      )
    },

    "unknown" = {
      warning("Unknown atlas, attempting to adapt scales in the blind.")
      list(stacked = {
        pos = list(
          x=geobrain %>%
            dplyr::group_by(hemi) %>%
            dplyr::summarise(val=mean(lat)),
          y=geobrain %>%
            dplyr::group_by(side) %>%
            dplyr::summarise(val=mean(long))
        )

        list(x = list(breaks = pos$x$val,
                      labels = pos$x$hemi),
             y = list(breaks = pos$y$val,
                      labels = pos$y$side),
             labs = list(y = "side", x = "hemisphere")
        )

      },
      dispersed = {
        pos = geobrain %>%
          dplyr::group_by(hemi) %>%
          dplyr::summarise_at(dplyr::vars(long,lat),dplyr::funs(mean))

        list(x = list(breaks = pos$long,
                      labels = pos$hemi),
             y = list(breaks = NULL,
                      labels = ""),
             labs = list(y = NULL, x = "hemisphere")
        )
      }
      )
    }
  )

  if(is.null(ad_scale[[position]])){
    warning("No such position for this atlas. Returning only available scale.")
    ad_scale[[names(ad_scale)]]
  }else{
    ad_scale[[position]][[aesthetics]]
  }
}
