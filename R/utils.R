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
  stack <- dplyr::group_by(atlas, hemi, side)
  stack <- dplyr::summarise_at(stack,
                               dplyr::vars(.long,
                                           .lat),
                               list(min = min, max = max, sd = stats::sd))
  stack <- dplyr::mutate(stack, sd = .lat_sd + .long_sd)

  stack$.lat_max[1] = ifelse(stack$.lat_max[1]/4.5 < stack$.lat_sd[1],
                             stack$.lat_max[1] + stack$.lat_sd[1],
                             stack$.lat_max[1])

  atlas = dplyr::mutate(atlas,
                        .lat = ifelse(hemi %in% "right",
                                      .lat + (stack$.lat_max[1]), .lat),
                        .long = ifelse(hemi %in% "right" & side %in% "lateral",
                                       .long - stack$.long_min[3], .long),
                        .long = ifelse(hemi %in% "right" & side %in%  "medial",
                                       .long + (stack$.long_min[2] - stack$.long_min[4]),
                                       .long))
  return(atlas)
}


data_merge <- function(.data, geobrain){

  # Find columns they have in common
  cols = names(geobrain)[names(geobrain) %in% names(.data)]

  if(dplyr::is_grouped_df(.data)){

    .data <- tidyr::nest(.data)

    cols = stats::na.omit(cols[!names(.data) %in% cols])

    geobrain <- dplyr::mutate(.data,
                              data = purrr::map(data,
                                                ~dplyr::full_join(geobrain, ., by=cols, copy=TRUE)))
    geobrain <- tidyr::unnest(geobrain, cols = c(data))
    geobrain <- dplyr::ungroup(geobrain)

  }else{
    # Merge the brain with the .data
    geobrain = dplyr::full_join(geobrain, .data, by = cols, copy=TRUE)
  }

  # Find if there are instances of those columns that
  # are not present in the geobrain. Maybe mispelled?
  errs = dplyr::filter(geobrain, is.na(.lat))
  errs <- dplyr::select(errs, !!cols)
  errs <- dplyr::distinct(errs)
  errs <- tidyr::unite_(errs, "tt", cols, sep = " - ")
  errs <- dplyr::summarise(errs, xvalue = paste0(tt, collapse = ", "))

  if(errs != ""){
    warning(paste("Some .data is not merged properly into the geobrain. Check for spelling mistakes in:",
                  errs$value))
  }

  return(geobrain)
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


#' Convenience function to install all atlases from ggsegExtra-repo
#'
#' \code{install_atlases} calls devtools::install_github() with repo = "LCBC-UiO/ggsegExtra"
#' @author Athanasia Mowinckel and Didac Pineiro
#'
#' @param repo repository to install via github
#' @param ... other options to install_github
#' @export
install_atlases = function(repo="LCBC-UiO/ggsegExtra",...){
  remotes::install_github(repo=repo, ...)
}


gap <- function(x){
  (min(x) + max(x)) / 2
}

## quiets concerns of R CMD checs
utils::globalVariables(c("area", "atlas", "colour", "group", "hemi", ".lat", ".long",
                         ".id", "side", "x", ".data", "dkt", ".lat_sd", ".long_sd", "data",
                         "tt", "atlas_scale_positions"))

