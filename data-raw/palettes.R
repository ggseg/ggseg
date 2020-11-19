
j <- dplyr::slice(ggseg3d::dk_3d, 1) %>%
  tidyr::unnest(ggseg_3d) %>%
  select(region, colour) %>%
  distinct() %>%
  na.omit()

dk_palette <- setNames(
  j$colour,
  j$region
)


<<<<<<< HEAD
# <<<<<<< HEAD
# aseg full palette
j <- read.table(file.path(freesurfer::fs_dir(), "ASegStatsLUT.txt"),
                skip = 7,
                col.names = c("roi","label","r", "g","b","a")) %>%
  mutate(colour = rgb(r,g,b, maxColorValue = 255)) %>%
  filter(label != "Unknown",
         label != "Right-Cerebral-White-Matter",
         label != "Left-Cerebral-White-Matter") %>%

#j <- dplyr::slice(ggseg3d::aseg_3d, 1) %>%
 # tidyr::unnest(ggseg_3d) %>%
  #select(region, colour) %>%
  mutate(region = gsub("-|_", " ", label),
         region = tolower(region),
         region = gsub("left |right ", "", region),
         region = gsub("cc ", "CC ", region),
         region = gsub("inf", "inferior", region),
         region = gsub("lat", "lateral", region),
         region = gsub("non wm", "non-wm", region),
         region = gsub("ventraldc", "ventral DC", region),
         region = ifelse(region == "cerebral cortex", NA, region)
# =======
# j <- dplyr::slice(ggseg3d::aseg_3d, 1) %>%
#   tidyr::unnest(ggseg_3d) %>%
#   select(region, colour) %>%
#   mutate(region = gsub("-|_", " ", region),
#          region = tolower(region),
#          region = gsub("left |right ", "", region),
#          region = gsub("cc ", "CC ", region),
#          region = gsub("inf", "", region),
#          region = gsub("ventraldc", "ventral DC", region)
# >>>>>>> 9330d878f2fd8bca2b91eab6cb80021f5e3d370a
=======
j <- dplyr::slice(ggseg3d::aseg_3d, 1) %>%
  tidyr::unnest(ggseg_3d) %>%
  select(region, colour) %>%
  mutate(region = gsub("-|_", " ", region),
         region = tolower(region),
         region = gsub("left |right ", "", region),
         region = gsub("cc ", "CC ", region),
         region = gsub("inf", "", region),
         region = gsub("ventraldc", "ventral DC", region)
>>>>>>> refs/remotes/origin/master
         ) %>%
  distinct() %>%
  na.omit()

aseg_palette <- setNames(
  j$colour,
  j$region
)

brain_pals = list(
  dk  = dk_palette,
  aseg = aseg_palette
)

usethis::use_data(brain_pals,
                  internal = TRUE, overwrite = TRUE, compress="xz")


