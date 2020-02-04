stack_brain <- function (atlas)
{
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

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(".data","dkt", ".id", ".lat", ".long",
                           ".lat_sd", ".long_sd"))
}
