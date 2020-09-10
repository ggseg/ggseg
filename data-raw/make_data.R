# dk ----
devtools::load_all("../ggsegExtra/")
devtools::load_all(".")

dk <- make_ggseg3d_2_ggseg(ggseg3d::dk_3d,
                           steps = 7,
                           tolerance = .5,
                           smoothness = 5,
                           output_dir = "data-raw")

ggseg(atlas=dk, show.legend = FALSE,
      colour = "black", #position="stacked",
      mapping = aes(fill=region)) +
  scale_fill_brain()

plot(dk)

ggplot() +
  geom_brain(atlas = dk, aes(fill = region),
             show.legend = FALSE) +
  scale_fill_brain()

ggplot() +
  geom_brain(atlas = dk, aes(fill = region),
             position = position_brain(hemi ~ side),
             show.legend = FALSE) +
  scale_fill_brain()

tibble(
  region = rep(c("transverse temporal", "insula",
                 "precentral","superior parietal"),2),
  p = sample(seq(0,.5,.001), 8),
  AgeG = c(rep("Young",4), rep("Old",4))
  ) %>%
  group_by(AgeG) %>%

  ggplot() +
  geom_brain(atlas = dk, aes(fill = p),
             position = position_brain(hemi + side ~ .),
             show.legend = FALSE) +
  facet_wrap(~AgeG)

usethis::use_data(dk,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress="xz")


# aseg ----
# aseg_n <- mutate(aseg,
#        kp = case_when(
#          grepl("Left", label) & hemi != "left" ~  FALSE,
#          grepl("Right", label) & hemi != "right" ~ FALSE,
#          TRUE ~ TRUE)) %>%
#   filter(kp) %>%
#   select(-kp)
# aseg_n <- unnest(aseg_n, ggseg)
# aseg_n <- group_by(aseg_n, label, hemi,  side, region, .id)
# aseg_n <- nest(aseg_n)
# aseg_n <- group_by(aseg_n, hemi,  side, region)
# aseg_n <- mutate(aseg_n, .subid = row_number())
# aseg_n <- unnest(aseg_n, data)
# aseg_n <- ungroup(aseg_n)
# aseg_n <- as_ggseg_atlas(aseg_n)

#
# aseg2 <- sf::st_as_sf(unnest(aseg, ggseg), coords = c(".long", ".lat")) %>%
#   group_by( label, .id, .subid) %>%
#   summarize(do_union=FALSE) %>%
#   sf::st_cast("POLYGON") %>%
#   ungroup() %>%
#   select(-.id, -.subid) %>%
#   group_by(label) %>%
#   summarise(geometry = sf::st_combine(geometry)) %>%
#   ungroup()
#
#
# aseg_n <- aseg2 %>%
#   left_join(aseg) %>%
#   select(atlas, hemi, side, region, label, ggseg, geometry) %>%
#   mutate(type = "subcortical",
#          atlas = "aseg")

aseg_n <- make_subcort_ggseg(output_dir = "data-raw/aseg",
                             steps = 8, tolerance = .4, smoothness = 4, dilate = 5)

# Do some data cleanup
aseg_n$data <- aseg_n$data %>%
  filter(!is.na(region)) %>%
  mutate(region = gsub("cc ", "CC ", region),
         region = gsub("dc", " DC", region),
         region = ifelse(region == "cerebral cortex", NA, region)
  ) %>%
  filter(!grepl("white|csf", region))


names(aseg_n$palette) <- gsub("cc", "CC", names(aseg_n$palette))
names(aseg_n$palette) <- gsub("dc", "DC", names(aseg_n$palette))
aseg_n$palette <- aseg_n$palette[!grepl("white|csf|cerebral cortex", names(aseg_n$palette))]

aseg_n %>%
  ggseg(atlas = ., show.legend = TRUE,
        colour = "black",
        # position = "s",
        mapping = aes(fill=region))

plot(aseg_n)

ggplot() +
  geom_brain(atlas = aseg_n)

aseg <- aseg_n
usethis::use_data(aseg,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress="xz")


# aseg$ggseg <-  lapply(aseg$ggseg, dplyr::mutate, .type = "subcortical")
# aseg <- as_ggseg_atlas(aseg)
# ggseg(atlas = aseg)
# usethis::use_data(aseg,
#                   internal = FALSE,
#                   overwrite = TRUE,
#                   compress="xz")
