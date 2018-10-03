load("~/Downloads/hcp_brain.Rda")

glasser = geobrain_hcp %>%
  separate(aparc, c("hemi","area", "DEL"), sep="_", remove = F) %>%
  select(-DEL) %>%
  mutate(hemi = ifelse(hemi == "L", "left", "right"),
         side="medial",
         lat = lat - min(lat),
         long = long - min(long)) %>%
  mutate(side = ifelse(id <= 150, "lateral", side)) %>%
  mutate(side = ifelse(id %in% c(2000:2500), "lateral", side),
         area = ifelse(grepl("\\?", area), NA, area))
save(glasser, file="data/glasser.RData")

