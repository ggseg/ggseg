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
