# dk ----
devtools::load_all("../ggsegExtra/")
<<<<<<< HEAD
devtools::load_all(".")

dk <- make_ggseg3d_2_ggseg(ggseg3d::dk_3d,
                           steps = 7,
                           tolerance = .5,
                           smoothness = 5,
                           output_dir = "data-raw")

ggseg(atlas=dk, show.legend = FALSE,
      colour = "black", #position="stacked",
=======

someData <- data.frame(
  region = c("transverse temporal", "insula",
             "precentral","superior parietal",
             "transverse temporal", "insula",
             "precentral","superior parietal"),
  p = sample(seq(0,.5,.001), 8),
  Group = c(rep("G1",4), rep("G2",4)),
  stringsAsFactors = FALSE)

dk <- ggsegExtra::make_ggseg3d_2_ggseg(ggseg3d::dk_3d,
                                       steps = 7,
                                       tolerance = .5,
                                       smoothness = 5,
                                       keep = 0.05,
                                       output_dir = "~/Desktop/test/")


ggseg(atlas=dk, show.legend = FALSE,
      colour = "black", position="stacked",
>>>>>>> 9330d878f2fd8bca2b91eab6cb80021f5e3d370a
      mapping = aes(fill=region)) +
  scale_fill_brain()

plot(dk)

ggplot() +
<<<<<<< HEAD
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
=======
  geom_brain(data = dk, aes(fill = region),
             position = position_brain("vertical"),
             show.legend = FALSE) +
  scale_fill_brain()


ggplot(someData) +
  geom_brain(data = someData, atlas = dk, aes(fill = region), show.legend = FALSE) +
  scale_fill_brain()
>>>>>>> 9330d878f2fd8bca2b91eab6cb80021f5e3d370a

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

<<<<<<< HEAD
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

# Do some data clea-nup
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
=======

aseg2 <- sf::st_as_sf(unnest(aseg, ggseg), coords = c(".long", ".lat")) %>%
  group_by( label, .id, .subid) %>%
  summarize(do_union=FALSE) %>%
  sf::st_cast("POLYGON") %>%
  ungroup() %>%
  select(-.id, -.subid) %>%
  group_by(label) %>%
  summarise(geometry = sf::st_combine(geometry)) %>%
  ungroup()


aseg_n <- aseg2 %>%
  left_join(aseg) %>%
  select(atlas, hemi, side, region, label, ggseg, geometry)
>>>>>>> 9330d878f2fd8bca2b91eab6cb80021f5e3d370a

aseg_n %>%
  ggseg(atlas = ., show.legend = TRUE,
        colour = "black",
<<<<<<< HEAD
        # position = "s",
        mapping = aes(fill=region))

plot(aseg_n)

ggplot() +
  geom_brain(atlas = aseg_n)
=======
        mapping = aes(fill=region)) +
  scale_fill_brain("aseg")

ggplot() +
  geom_brain(data = aseg_n) +
  scale_fill_brain("aseg")
>>>>>>> 9330d878f2fd8bca2b91eab6cb80021f5e3d370a

aseg <- aseg_n
usethis::use_data(aseg,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress="xz")


<<<<<<< HEAD
# aseg$ggseg <-  lapply(aseg$ggseg, dplyr::mutate, .type = "subcortical")
# aseg <- as_ggseg_atlas(aseg)
# ggseg(atlas = aseg)
# usethis::use_data(aseg,
#                   internal = FALSE,
#                   overwrite = TRUE,
#                   compress="xz")
=======
>>>>>>> 9330d878f2fd8bca2b91eab6cb80021f5e3d370a
