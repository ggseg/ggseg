library(tidyverse)
library(geomorph)
devtools::load_all(".")
source("data-raw/get_surface.R")

# A. M. Winkler created the scripts that converterd Freesurfer
# `.srf`  files to  `.ply`.
# All files are made with Freesurfer's fsaverage5.

# Function to grab all the data and create a nested tibble


## DKT ----
dkt_3d = get_surface("data-raw/mesh3d/DKT/", atlasname = "dkt_3d")

t = data.frame(ggseg::brain.pals$dkt)
names(t)[1] = "colour"
t = t %>%
  rownames_to_column(var = "area") %>%
  mutate_all(as.character)

t = dkt %>%
  left_join(t) %>%
  select(label, acronym, lobe, area, colour) %>%
  distinct()

dkt_3d = dkt_3d %>%
  mutate(data = map(data, ~left_join(., t, by="label"))) %>%
  mutate(data = map(data, ~mutate(., acronym = ifelse(annot == "corpuscallosum","cc", acronym),
                                  area = ifelse(annot == "corpuscallosum","corpus callosum", area))
  ))

## aseg ----
aseg_3d = list.files("data-raw/mesh3d/aseg/", pattern="ply", full.names = T) %>%
  data.frame(files = ., stringsAsFactors = F) %>%
  separate(files, c("DEL","DEL1","DEL2","DEL3", "DEL4", "roi", "DEL5"), remove = F) %>%
  select(-contains("DEL")) %>%
  mutate(surf="inflated", hemi="subcort", atlas="aseg_3d")

rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)

aseg_3d = aseg_3d %>%
  left_join(
    read.table("data-raw/mesh3d/aseg/annot2filename.csv", sep="\t",header=T,stringsAsFactors = F) %>%
      mutate(colour = rgb2hex(R,G,B),
             no = str_pad(no, 3, side = "left", pad="0")) %>%
      rename(roi=no) %>%
      select(roi, label, colour)
  )

mesh = lapply(aseg_3d$files, read.ply, ShowSpecimen = F)

for(i in 1:length(mesh)){
  aseg_3d$mesh[[i]] = list(vb=mesh[[i]]$vb,
                           it=mesh[[i]]$it
  )
}

aseg_3d = aseg_3d %>%
  mutate(area=label,
         surf="LCBC") %>%
  group_by(atlas, surf, hemi) %>%
  nest()
#save(aseg_3d, file="data/aseg_3d.RData", compress = "xz")


usethis::use_data(dkt_3d, aseg_3d,
                  internal = FALSE, overwrite = TRUE, compress = "xz")



