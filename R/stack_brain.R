stack_brain <- function(geobrain){

  # Alter coordinates of the left side to stack ontop of right side
  stack = geobrain %>%
    dplyr::group_by(hemi,side) %>%
    dplyr::summarise_at(dplyr::vars(.long,.lat),dplyr::funs(min, max, sd)) %>%
    dplyr::mutate(sd = .lat_sd + .long_sd)

  # Distance between hemispheres less than 3 looks untidy
  stack$.lat_max[1] = ifelse(stack$.lat_max[1] < 3,
                            stack$.lat_max[1] + stack$.lat_sd[1],
                            stack$.lat_max[1])

  geobrain = geobrain %>%

    # Move right side over to the left
    dplyr::mutate(.lat=ifelse(hemi %in% "right",
                             .lat + (stack$.lat_max[1]), .lat)) %>%

    # move right side on top of left, and swap the places of medial and .lateral
    dplyr::mutate(.long=ifelse(hemi %in% "right" & side %in% "lateral" ,
                              .long - stack$.long_min[3], .long),
                  .long=ifelse(hemi %in% "right" & side %in% "medial" ,
                              .long +(stack$.long_min[2]-stack$.long_min[4]), .long)
    )

  return(geobrain)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(".data","dkt", ".id", ".lat", ".long",
                           ".lat_sd", ".long_sd"))
}
