#' @importFrom dplyr group_by summarise_at mutate
#' @importFrom stats sd
squish_position <- function(geobrain, hemisphere, stack){
  mm <- group_by(geobrain, hemi)
  mm <- summarise_at(mm, vars(.long),
                     list(max = max, min = min, sd = sd))
  diff <- mm$min[2] - mm$max[1]

  mutate(geobrain,
         .long = ifelse(hemi == "right",
                        .long - diff + mm$sd[1]*.5,
                        .long))
}

#' @importFrom dplyr group_by summarise_at mutate vars
stack_brain <- function (atlas){
  stack <- group_by(atlas, hemi, side)
  stack <- summarise_at(stack,
                        vars(.long,
                             .lat),
                        list(min = min, max = max, sd = stats::sd))
  stack <- mutate(stack, sd = .lat_sd + .long_sd)

  stack$.lat_max[1] = ifelse(stack$.lat_max[1]/4.5 < stack$.lat_sd[1],
                             stack$.lat_max[1] + stack$.lat_sd[1],
                             stack$.lat_max[1])

  atlas = mutate(atlas,
                 .lat = ifelse(hemi %in% "right",
                               .lat + (stack$.lat_max[1]), .lat),
                 .long = ifelse(hemi %in% "right" & side %in% "lateral",
                                .long - stack$.long_min[3], .long),
                 .long = ifelse(hemi %in% "right" & side %in%  "medial",
                                .long + (stack$.long_min[2] - stack$.long_min[4]),
                                .long))
  return(atlas)
}

#' @importFrom dplyr ungroup group_by  filter select distinct summarise full_join is_grouped_df
#' @importFrom tidyr unnest nest unite unite_
#' @importFrom purrr map
#' @importFrom stats na.omit
data_merge <- function(.data, atlas){

  # Find columns they have in common
  cols = names(atlas)[names(atlas) %in% names(.data)]

  if(is_grouped_df(.data)){

    .data <- nest(.data)

    cols = na.omit(cols[!names(.data) %in% cols])

    atlas <- mutate(.data,
                    data = map(data,
                               ~full_join(atlas, ., by=cols, copy=TRUE)))
    atlas <- unnest(atlas, cols = c(data))
    atlas <- ungroup(atlas)

  }else{
    # Merge the brain with the .data
    atlas = full_join(atlas, .data, by = cols, copy=TRUE)
  }

  # Find if there are instances of those columns that
  # are not present in the atlas. Maybe mispelled?
  errs <- filter(atlas, is.na(.lat))
  errs <- select(errs, !!cols)
  errs <- distinct(errs)
  errs <- unite_(errs, "tt", cols, sep = " - ")
  errs <- summarise(errs, value = paste0(tt, collapse = ", "))

  if(errs != ""){
    warning(paste("Some .data is not merged properly into the atlas. Check for spelling mistakes in:",
                  errs$value))
  }

  return(atlas)
}



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
#' @importFrom dplyr group_by summarise vars
adapt_scales = function(geobrain, position = "dispersed", aesthetics = "labs"){

  atlas = ifelse(any(names(geobrain) %in% "atlas"),
                 unique(geobrain$atlas),
                 "unknown")

  if(!".pos" %in% names(geobrain)){
    y <- group_by(geobrain, hemi)
    y <- summarise(y, val=gap(.lat))

    x <- group_by(geobrain, side)
    x <- summarise(x, val=gap(.long))

    stk = list(
      y = y,
      x = x
    )

    disp <- group_by(geobrain, hemi)
    disp <- summarise_at(disp, vars(.long,.lat), list(gap))

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

## quiets concerns of R CMD checs
utils::globalVariables(c("area", "atlas", "colour", "group", "hemi", ".lat", ".long",
                         ".id", "side", "x", ".data", "dkt", ".lat_sd", ".long_sd", "data",
                         "tt", "atlas_scale_positions"))

