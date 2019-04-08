stack_brain <- function (geobrain)
  {
  stack = geobrain %>%
    dplyr::group_by(hemi, side) %>%
    dplyr::summarise_at(dplyr::vars(.long,
                                    .lat),
                        list(min = min, max = max, sd = sd)) %>%
    dplyr::mutate(sd = .lat_sd +
                    .long_sd)

  stack$.lat_max[1] = ifelse(stack$.lat_max[1]/4.5 < stack$.lat_sd[1],
                             stack$.lat_max[1] + stack$.lat_sd[1],
                             stack$.lat_max[1])

  geobrain = geobrain %>%
    dplyr::mutate(.lat = ifelse(hemi %in% "right",
                                .lat + (stack$.lat_max[1]), .lat),
                  .long = ifelse(hemi %in% "right" & side %in% "lateral",
                                 .long - stack$.long_min[3], .long),
                  .long = ifelse(hemi %in% "right" & side %in%  "medial",
                                 .long + (stack$.long_min[2] - stack$.long_min[4]),
                                 .long))
  return(geobrain)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(".data","dkt", ".id", ".lat", ".long",
                           ".lat_sd", ".long_sd"))
}
