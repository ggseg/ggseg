# dk ----
devtools::load_all("../ggsegExtra/")

someData <- data.frame(
  region = c("transverse temporal", "insula",
             "precentral","superior parietal",
             "transverse temporal", "insula",
             "precentral","superior parietal"),
  p = sample(seq(0,.5,.001), 8),
  Group = c(rep("G1",4), rep("G2",4)),
  stringsAsFactors = FALSE)

dk <- make_ggseg3d_2_ggseg(ggseg3d::dk_3d,
                           steps = 6:7,
                           tolerance = .5,
                           smoothness = 5,
                           output_dir = "data-raw")


ggseg(atlas=dk, show.legend = FALSE,
      colour = "black", position="stacked",
      mapping = aes(fill=region)) +
  scale_fill_brain()

plot(dk)

ggplot() +
  geom_brain(atlas = dk, aes(fill = region),
             position = position_brain("vertical"),
             show.legend = FALSE) +
  scale_fill_brain()


ggplot() +
  geom_brain(atlas = dk, show.legend = FALSE)

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

aseg_n %>%
  ggseg(atlas = ., show.legend = TRUE,
        colour = "black",
        mapping = aes(fill=region)) +
  scale_fill_brain("aseg")

ggplot() +
  geom_brain(data = aseg_n) +
  scale_fill_brain("aseg")

aseg <- aseg_n
usethis::use_data(aseg,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress="xz")


