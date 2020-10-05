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
    stack <- dplyr::arrange(stack, .long_min)

    for(k in 1:nrow(stack)){
      atlas <-  dplyr::mutate(atlas,
                       .lat = ifelse(side %in% stack$side[k],
                                     .lat + mean(stack$.lat_max)*k, .lat),
                       .long = ifelse(side %in% stack$side[k],
                                      .long - stack$.long_mean[k],
                                      .long))
    }
  }else{
    cat("Atlas .type not set, stacking not possible.")
  }

  return(atlas)
}

calc_stack <- function(stack){
  stack <- dplyr::summarise_at(stack,
                        ggplot2::vars(.long,
                             .lat),
                        list(min = min, max = max, sd = stats::sd, mean = mean))
  stack <- dplyr::mutate(stack, sd = .lat_sd + .long_sd)

  stack$.lat_max[1] = ifelse(stack$.lat_max[1]/4.5 < stack$.lat_sd[1],
                             stack$.lat_max[1] + stack$.lat_sd[1],
                             stack$.lat_max[1])
  return(stack)
}

#' @importFrom dplyr is_grouped_df mutate full_join ungroup filter select distinct
#' @importFrom dplyr distinct
#' @importFrom tidyr nest unnest unite_
#' @importFrom stats na.omit
data_merge <- function(.data, atlas){

  # Find columns they have in common
  cols = names(atlas)[names(atlas) %in% names(.data)]

  if(dplyr::is_grouped_df(.data)){

    .data <- tidyr::nest(.data)

    cols = na.omit(cols[!names(.data) %in% cols])

    atlas <- dplyr::mutate(.data,
                    data = lapply(data,
                    function(x) dplyr::full_join(atlas, x, by=cols, copy=TRUE)
                    ))
    atlas <- unnest(atlas, cols = c(data))
    atlas <- ungroup(atlas)

  }else{
    # Merge the brain with the .data
    atlas = dplyr::full_join(atlas, .data, by = cols, copy=TRUE)
  }

  # Find if there are instances of those columns that
  # are not present in the atlas. Maybe mispelled?
  errs <- dplyr::filter(atlas, is.na(.lat))
  errs <- dplyr::select(errs, !!cols)
  errs <- dplyr::distinct(errs)
  errs <- tidyr::unite_(errs, "tt", cols, sep = " - ")
  errs <- dplyr::summarise(errs, value = paste0(tt, collapse = ", "))

  if(errs$value != ""){
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
#' @importFrom dplyr group_by summarise
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
    y <- group_by(geobrain, side)
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

coords2sf <- function(coords, vertex_size_limits = NULL) {
  dt <- tidyr::unnest(coords, ggseg)
  dt <- dt[,c(".long", ".lat", ".id", ".subid")]
  dt <- dplyr::group_by(dt, .subid, .id)
  dt <- dplyr::group_split(dt)

  if(!is.null(vertex_size_limits)){
    if(!is.na(vertex_size_limits[1]))
      dt <- dt[sapply(dt, function(x) nrow(x) > vertex_size_limits[1])]

    if(!is.na(vertex_size_limits[2]))
      dt <- dt[sapply(dt, function(x) nrow(x) < vertex_size_limits[2])]
  }

  dt <- lapply(dt, as.matrix)
  dt <- lapply(dt, function(x) rbind(x[,1:4], x[1, 1:4]))

  dt <- sf::st_polygon(dt)
  dt <- sf::st_sfc(dt)
  dt <- sf::st_sf(dt)
  dt <- sf::st_zm(dt)
  dt <- sf::st_cast(dt, "MULTIPOLYGON")
  dt$lab <- coords$lab
  dt
}

sf2coords <- function(x){

  x$ggseg <- lapply(1:nrow(x),
                    function(y) to_coords(x$geometry[[y]], y)
  )
  x$geometry <- NULL

  x
}


to_coords <- function(x, n){
  k <- sf::st_combine(x)
  k <- sf::st_coordinates(k)
  k <- dplyr::as_tibble(k)
  k$L2 <- n * 10000 + k$L2

  k <- dplyr::group_by(k, L2)
  k <- dplyr::mutate(k, .order = dplyr::row_number())
  k <- dplyr::ungroup(k)

  names(k) <- c(".long", ".lat",  ".subid", ".id", ".poly", ".order")

  as_brain_polygon(k)
}

## quiets concerns of R CMD checks
utils::globalVariables(c("area", "atlas", "colour", "group", "hemi", ".lat", ".long",
                  ".id", "side", "x", ".data", "dkt", ".lat_sd", ".long_sd", "data",
                  "tt", "atlas_scale_positions", ".long_min", "L2"))

