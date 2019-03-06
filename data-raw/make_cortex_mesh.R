library(tidyverse)
library(geomorph)
library(ggseg)

mesh = lapply(list.files("data-raw/mesh3d/lh.cortex.LCBC.fsaverage5/",
                         pattern="ply", full.names = T), geomorph::read.ply, ShowSpecimen = F)

lh <- tibble(files = list.files("data-raw/mesh3d/lh.cortex.LCBC.fsaverage5",
                                pattern="roi", full.names = F),
             atlas = "cortex_3d",
             hemi = "left",
             colour = "#cecece",
             surf = "LCBC",
             area = "cortex") %>%
  separate(files, sep="roi\\.", into=c(NA, "roi"), remove = F) %>%
  separate(roi, sep="[.]", into=c("roi", NA), remove = F)


lh$mesh = list(vb=1)
for(i in 1:length(mesh)){
  lh$mesh[[i]] = list(vb=mesh[[i]]$vb,
                      it=mesh[[i]]$it
  )
}

mesh = lapply(list.files("data-raw/mesh3d/rh.cortex.LCBC.fsaverage5/",
                         pattern="ply", full.names = T), geomorph::read.ply, ShowSpecimen = F)

rh <- tibble(files = list.files("data-raw/mesh3d/rh.cortex.LCBC.fsaverage5",
                                pattern="roi", full.names = F),
             atlas = "cortex_3d",
             hemi = "right",
             colour = "#cecece",
             surf = "LCBC",
             area = "cortex") %>%
  separate(files, sep="roi\\.", into=c(NA, "roi"), remove = F) %>%
  separate(roi, sep="[.]", into=c("roi", NA), remove = F)


rh$mesh = list(vb=1)
for(i in 1:length(mesh)){
  rh$mesh[[i]] = list(vb=mesh[[i]]$vb,
                      it=mesh[[i]]$it
  )
}

cortex_3d <- lh %>%
  bind_rows(rh) %>%
  group_by(atlas, surf, hemi) %>%
  nest() %>%
  as_ggseg3d_atlas()

usethis::use_data(cortex_3d, internal = T, overwrite = TRUE, compress = "xz")
