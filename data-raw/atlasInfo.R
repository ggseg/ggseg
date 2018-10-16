library(ggseg)
library(tidyverse)
atlas.info = list(dkt = dkt,
                  yeo7 = yeo7,
                  yeo17 = yeo17,
                  aseg = aseg,
                  midsagittal = midsagittal,
                  glasser = glasser) %>%
  lapply(function(x) x %>% select(area,hemi,side) %>% unique %>% na.omit()) #%>%
  #bind_rows(.id = "atlas")

save(atlas.info, file="data/atlas.info.RData")
