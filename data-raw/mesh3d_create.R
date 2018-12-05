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
    mutate(label = paste(hemi, annot, sep="_"),
           hemi = ifelse(hemi =="lh", "left", "right"),
           atlas = atlasname) %>%
    distinct()

  for(i in 1:length(mesh)){
    data$mesh[[i]] = list(vb=mesh[[i]]$vb,
                          it=mesh[[i]]$it
    )
  }

  data %>%
    select(-filename) %>%
    group_by(surf, hemi) %>%
    nest()
}


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
save(dkt_3d, file="data/dkt_3d.RData")

## Yeo 7 ----
yeo7_3d = get_surface("data-raw/mesh3d/Yeo20117NetworksN1000/", atlasname = "yeo7_3d")

t = data.frame(ggseg::brain.pals$yeo7)
names(t)[1] = "colour"
t = t %>%
  rownames_to_column(var = "area") %>%
  mutate_all(as.character)

t = yeo7 %>%
  left_join(t) %>%
  select(label, area, colour) %>%
  distinct() %>%
  na.omit()

yeo7_3d = yeo7_3d %>%
  mutate(data = map(data, ~left_join(., t, by="label")))
save(yeo7_3d, file="data/yeo7_3d.RData")


## Yeo 17 ----
yeo17_3d = get_surface("data-raw/mesh3d/Yeo201117NetworksN1000/", atlasname = "yeo17_3d")

t = data.frame(ggseg::brain.pals$yeo17)
names(t)[1] = "colour"
t = t %>%
  rownames_to_column(var = "area") %>%
  mutate_all(as.character)

t = yeo17 %>%
  left_join(t) %>%
  select(label, area, colour) %>%
  distinct() %>%
  na.omit()

yeo17_3d = yeo17_3d %>%
  mutate(data = map(data, ~left_join(., t, by="label")))
save(yeo17_3d, file="data/yeo17_3d.RData")

## Schaefer 7 ----
schaefer7_3d = get_surface("data-raw/mesh3d/Schaefer2018400Parcels7Networksorder/",
                           atlasname = "schaefer7_3d") %>%
  unnest() %>%
  separate(annot, c("DEL", "DEL2", "network", "no"), remove = F) %>%
  unite(area, c("network", "no"), remove = F) %>%
  select(-contains("DEL"), -no) %>%
  mutate_all(funs(ifelse(grepl("Defined", .), "medialwall", .))) %>%
  group_by(surf, hemi) %>%
  nest()

lut = rio::import_list("data-raw/mesh3d/Schaefer2018_400Parcels_ctabs.xlsx") %>%
  bind_rows() %>%
  select(annot, HEX) %>%
  rename(colour = HEX) %>%
  filter(!grepl("Wall$", annot))

schaefer7_3d = schaefer7_3d %>%
  mutate(data = map(data, ~left_join(., lut) %>%
                      select(1:4, colour, everything()) %>%
                      mutate(annot = ifelse(grepl("Wall$",annot), "medialwall", annot))
  )
  )
save(schaefer7_3d, file="data/schaefer7_3d.RData")

## Schaefer 17 ----
schaefer17_3d = get_surface("data-raw/mesh3d/Schaefer2018400Parcels17Networksorder/",
                            atlasname = "schaefer17_3d") %>%
  unnest() %>%
  separate(annot, c("DEL", "DEL2", "network", "no"), remove = F) %>%
  unite(area, c("network", "no"), remove = F) %>%
  select(-contains("DEL"), -no) %>%
  mutate_all(funs(ifelse(grepl("Defined", .), "medialwall", .))) %>%
  group_by(surf, hemi) %>%
  nest()

schaefer17_3d = schaefer17_3d %>%
  mutate(data = map(data, ~left_join(., lut) %>%
                      select(1:4, colour, everything()) %>%
                      mutate(annot = ifelse(grepl("Wall$",annot), "medialwall", annot))
  )
  )
save(schaefer17_3d, file="data/schaefer17_3d.RData")

## aseg ----
aseg_3d = list.files("data-raw/mesh3d/aseg/", pattern="ply", full.names = T) %>%
  data.frame(files = ., stringsAsFactors = F) %>%
  mutate()
                            atlasname = "aseg_3d")

# No palettes yet ----

desterieux_3d = get_surface("data-raw/mesh3d/Desterieux/",
                            atlasname = "desterieux_3d")
#save(desterieux_3d, file="data/desterieux_3d.RData")

# buggy ----
## Glasser ----
glasser_3d = get_surface("data-raw/mesh3d/HCPMMP1/", atlasname = "glasser_3d") %>%
  mutate(data = map(data, ~ separate(., annot, c("DEL","area", "DEL2"), remove = F) %>%
                      select(-contains("DEL"))))

t = data.frame(ggseg::brain.pals$glasser)
names(t)[1] = "colour"
t = t %>%
  rownames_to_column(var = "area") %>%
  mutate_all(as.character)

t = glasser %>%
  left_join(t) %>%
  select(area, colour) %>%
  distinct() %>%
  na.omit()

glasser_3d = glasser_3d %>%
  mutate(data = map(data, ~left_join(., t, ) %>%
                      select(1:5, colour, everything()) %>%
                      mutate(area = ifelse(area == "", "medialwall", area))
  )
  )
save(glasser_3d, file="data/glasser_3d.RData")

