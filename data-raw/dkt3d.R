library(tidyverse)
library(geomorph)

# All files were downloaded from
# https://brainder.org/research/brain-for-blender/
# https://s3.us-east-2.amazonaws.com/brainder/software/brain4blender/all_ply.tar.bz2
# A. M. Winkler created the 3d "ply" files here used as templates.

get_surface = function(files){
  mesh = lapply(files, read.ply, ShowSpecimen = F)

  data = data.frame(files=files) %>%
    separate(files, c("Del1", "DEL2", "DEL3", "DEL4","DEL5", "hemi", "type", "DELY", "raw", "DELX")) %>%
    select(-contains("DEL")) %>%
    unite(label, c("hemi", "raw"), remove=F) %>%
    left_join(ggseg::dkt %>%
                select(label, area, acronym, lobe) %>%
                distinct()) %>%
    mutate(hemi = ifelse(hemi =="lh", "left", "right"))

  t = as.data.frame(ggseg::brain.pals$dkt)
  names(t)[1] = "colour"
  t = t %>%
    rownames_to_column(var = "area") %>%
    mutate_all(as.character)

  data = data %>%
    left_join(t)

  for(i in 1:length(mesh)){
    data$mesh[[i]] = mesh[[i]]
  }

  data
}

# Inflated --
inflated = get_surface(list.files("data-raw/mesh3d/inflated_DKT", full.names = T))

# Pial --
pial =  get_surface(list.files("data-raw/mesh3d/pial_DKT/", full.names = T))

# White --
white = get_surface(list.files("data-raw/mesh3d/white_DKT/", full.names = T))

dkt3d = list(pial=pial,
             inflated=inflated,
             white=white)
save(dkt3d, file="data/dkt3d.RData")
