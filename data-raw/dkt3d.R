library(tidyverse)
library(geomorph)

# A. M. Winkler created the scripts that converterd Freesurfer
# `.srf`  files to  `.ply`.
# All files are made with Freesurfer's fsaverage5.

# Function to grab all the data and create a nested tibble
get_surface = function(folder, atlasname){

  surfs = list.files(folder, full.names = T)

  hemi = sapply(surfs, list.files, full.names = T) %>% as.character()

  files = sapply(hemi, list.files, pattern="*roi*", full.names = T) %>% as.character()

  mesh = lapply(files, read.ply, ShowSpecimen = F)


  annots = sapply(hemi, list.files, pattern="*.csv", full.names = T) %>%
    as.character() %>%
    lapply(read_csv) %>%
    bind_rows()


  data = data.frame(files=files) %>%
    separate(files, remove=F,
             c("Del1", "DEL2", "DEL3", "atlas","DEL5", "DEL7", "hemi", "surf", "DEL6", "roi", "DELX")) %>%
    separate(files, c("DEL1","DEL2","DEL#","DEL4","DEL5","DEL6","filename"), sep="/") %>%
    select(-contains("DEL")) %>%
    left_join(annots, by="filename") %>%
    mutate(annot = ifelse(annot == "unknown", "medialwall", annot)) %>%
    mutate(label = paste(hemi, annot, sep="_")) %>%
    left_join(dkt %>% select(label, acronym, lobe, area) %>% distinct,
            by="label") %>%
    mutate(hemi = ifelse(hemi =="lh", "left", "right"),
           acronym = ifelse(annot == "corpuscallosum","cc", acronym),
           area = ifelse(annot == "corpuscallosum","corpus callosum", area),
           atlas = atlasname) %>%
    distinct()


  t = as.data.frame(ggseg::brain.pals$dkt)
  names(t)[1] = "colour"
  t = t %>%
    rownames_to_column(var = "area") %>%
    mutate_all(as.character)

  data = data %>%
    left_join(t)

  for(i in 1:length(mesh)){
    #data$mesh[[i]] = mesh[[i]]

    data$mesh[[i]] = list(vb=mesh[[i]]$vb,
                          it=mesh[[i]]$it
    )
  }

  data %>%
    select(-filename) %>%
    group_by(surf, hemi) %>%
    nest()
}


dkt_3d = get_surface("data-raw/mesh3d/DKT/", atlasname = "dkt_3d")
save(dkt_3d, file="data/dkt_3d.RData")

yeo7_3d = get_surface("data-raw/mesh3d/DKT/", atlasname = "dkt3d")
save(dkt3d, file="data/dkt3d.RData")

yeo17_3d = get_surface("data-raw/mesh3d/DKT/", atlasname = "dkt3d")
save(dkt3d, file="data/dkt3d.RData")

dkt3d = get_surface("data-raw/mesh3d/DKT/", atlasname = "dkt3d")
save(dkt3d, file="data/dkt3d.RData")
