squish_position <- function(geobrain, hemisphere, stack){
  mm <- dplyr::group_by(geobrain, hemi)
  mm <- dplyr::summarise_at(mm, dplyr::vars(.long),
                            list(max = max, min = min, sd = stats::sd))
  diff <- mm$min[2] - mm$max[1]

  dplyr::mutate(geobrain,
                .long = ifelse(hemi == "right",
                               .long - diff + mm$sd[1]*.5,
                               .long))
}


stack_brain <- function (atlas){
  if(unique(atlas$type) == "cortical"){
    stack <- dplyr::group_by(atlas, hemi, side)
    stack <- calc_stack(stack)

    atlas = dplyr::mutate(atlas,
                          .lat = ifelse(hemi %in% "right",
                                        .lat + (stack$.lat_max[1]), .lat),
                          .long = ifelse(hemi %in% "right" & side %in% "lateral",
                                         .long - stack$.long_min[3], .long),
                          .long = ifelse(hemi %in% "right" & side %in%  "medial",
                                         .long + (stack$.long_min[2] - stack$.long_min[4]),
                                         .long))

  }else if(unique(atlas$type) == "subcortical"){
    stack <- dplyr::group_by(atlas, side)
    stack <- calc_stack(stack)
    stack <- arrange(stack, .long_min)

    for(k in 1:nrow(stack)){
      atlas <-  dplyr::mutate(atlas,
                            .lat = ifelse(side %in% stack$side[k],
                                          .lat + mean(stack$.lat_max)*k, .lat),
                            .long = ifelse(side %in% stack$side[k],
                                           .long - stack$.long_mean[k],
                                           .long))
    }
    # browser()
  }else{
    cat("Atlas .type not set, stacking not possible.")
  }


  return(atlas)
}

calc_stack <- function(stack){
  stack <- dplyr::summarise_at(stack,
                               dplyr::vars(.long,
                                           .lat),
                               list(min = min, max = max, sd = stats::sd, mean = mean))
  stack <- dplyr::mutate(stack, sd = .lat_sd + .long_sd)

  stack$.lat_max[1] = ifelse(stack$.lat_max[1]/4.5 < stack$.lat_sd[1],
                             stack$.lat_max[1] + stack$.lat_sd[1],
                             stack$.lat_max[1])
  return(stack)
}

data_merge <- function(.data, atlas){

  # Find columns they have in common
  cols = names(atlas)[names(atlas) %in% names(.data)]

  if(dplyr::is_grouped_df(.data)){

    .data <- tidyr::nest(.data)

    cols = stats::na.omit(cols[!names(.data) %in% cols])

    atlas <- dplyr::mutate(.data,
                           data = purrr::map(data,
                                             ~dplyr::full_join(atlas, ., by=cols, copy=TRUE)))
    atlas <- tidyr::unnest(atlas, cols = c(data))
    atlas <- dplyr::ungroup(atlas)

  }else{
    # Merge the brain with the .data
    atlas = dplyr::full_join(atlas, .data, by = cols, copy=TRUE)
  }

  # Find if there are instances of those columns that
  # are not present in the atlas. Maybe mispelled?
  errs = dplyr::filter(atlas, is.na(.lat))
  errs <- dplyr::select(errs, !!cols)
  errs <- dplyr::distinct(errs)
  errs <- tidyr::unite_(errs, "tt", cols, sep = " - ")
  errs <- dplyr::summarise(errs, value = paste0(tt, collapse = ", "))

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
adapt_scales = function(geobrain, position = "dispersed", aesthetics = "labs"){

  atlas = ifelse(any(names(geobrain) %in% "atlas"),
                 unique(geobrain$atlas),
                 "unknown")

  if(unique(geobrain$type) == "cortical"){
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
  }else if(unique(geobrain$type) == "subcortical"){
    y <- dplyr::group_by(geobrain, side)
    y <- dplyr::summarise(y, val=gap(.lat))

    x <- dplyr::group_by(geobrain, side)
    x <- dplyr::summarise(x, val=gap(.long))

    stk = list(
      y = y,
      x = x
    )

    disp <- dplyr::group_by(geobrain, side)
    disp <- dplyr::summarise_at(disp, dplyr::vars(.long,.lat), list(gap))

    ad_scale <- list(
      stacked =
        list(x = list(breaks = NULL,
                      labels = ""),
             y = list(breaks = stk$y$val,
                      labels = stk$y$side),
             labs = list(y = "side", x = NULL)
        ),

      dispersed =
        list(x = list(breaks = disp$.long,
                      labels = disp$side),
             y = list(breaks = NULL,
                      labels = ""),
             labs = list(y = NULL, x = "side")
        )
    )
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

guess_type <- function(x){
  x <- if("type" %in% names(x)){
    x$type
  }else{
    ifelse(any("medial" %in% x$side), "cortical", "subcortical")
  }
  unique(x)
}


## quiets concerns of R CMD checs
utils::globalVariables(c("area", "atlas", "colour", "group", "hemi", ".lat", ".long",
                         ".id", "side", "x", ".data", "dkt", ".lat_sd", ".long_sd", "data",
                         "tt", "atlas_scale_positions"))

