squish_position <- function(geobrain, hemisphere, stack){
    mm <- geobrain %>%
      group_by(hemi) %>%
      summarise_at(vars(.long), list(max = max, min = min, sd = sd))
    diff <- mm$min[2] - mm$max[1]

    geobrain %>%
      mutate(.long = ifelse(hemi == "right",
                            .long - diff + mm$sd[1]*.5,
                            .long))
}
