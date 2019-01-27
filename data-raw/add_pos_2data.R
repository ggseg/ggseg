library(tidyverse)

# DKT ----
dkt <- dkt %>%
 # select(-group) %>%
  mutate(pos = list(x = 1))

for(i in 1:nrow(dkt)){
  dkt$pos[[i]] = list(
    stacked = list(x = list(breaks = c(2.5, 9.2),
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
}
usethis::use_data(dkt, internal = FALSE, overwrite = TRUE)


# aseg ----
aseg <- aseg %>%
  #select(-group) %>%
  mutate(pos = list(x = 1))

for(i in 1:nrow(aseg)){
  aseg$pos[[i]] = list(
    dispersed = list(x = list(breaks = c(9.2, 11.4),
                              labels = c("left","right")),
                     y = list(breaks = NULL,
                              labels = NULL),
                     labs = list(x = "hemisphere",
                                 y = NULL))
  )
}
usethis::use_data(aseg, internal = FALSE, overwrite = TRUE)
