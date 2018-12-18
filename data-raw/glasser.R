glasser = geobrain_hcp %>%
  separate(aparc, c("hemi","area", "DEL"), sep="_", remove = F) %>%
  select(-DEL, -piece, -meas) %>%
  mutate(hemi = ifelse(hemi == "L", "left", "right"),
         side="medial",
         lat = lat - min(lat),
         long = long - min(long)) %>%
  mutate(side = ifelse(id <= 150, "lateral", side)) %>%
  mutate(side = ifelse(id %in% c(2000:2500), "lateral", side),
         area = ifelse(grepl("\\?", area), NA, area)) %>%
  rename(label=aparc) %>%
  select(long, lat, id, hemi, area, side, label, everything())
save(glasser, file="data/glasser.RData", compress = "xz")
