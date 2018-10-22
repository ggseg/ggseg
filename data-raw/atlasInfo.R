library(ggseg)
library(tidyverse)
atlas.info = list(dkt = dkt,
                  yeo7 = yeo7,
                  yeo17 = yeo17,
                  glasser = glasser,
                  jhu = jhu,
                  aseg = aseg,
                  midsagittal = midsagittal) %>%
  lapply(function(x) x %>% select(area,hemi,side,label) %>% unique %>% na.omit())

save(atlas.info, file="data/atlas.info.RData")
