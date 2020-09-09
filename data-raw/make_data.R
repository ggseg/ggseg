# dk ----
devtools::load_all("../ggsegExtra/")
devtools::load_all(".")

dk <- make_ggseg3d_2_ggseg(ggseg3d::dk_3d,
                           steps = 7,
                           tolerance = .5,
                           smoothness = 5,
                           output_dir = "data-raw")
# dk <- mutate(dk, type = "cortical")
dk2 <- dk
dk2$geometry <- NULL
dk2 <- as_ggseg_atlas(dk2)
ggseg(atlas=dk2, show.legend = FALSE,
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
ggplot() +
  geom_brain(atlas = dk, aes(fill = region),
             position = position_brain(hemi + side ~ .),
             show.legend = FALSE) +
  scale_fill_brain()



ggplot() +
  geom_brain(atlas = dk, show.legend = FALSE)

dk <- as_ggseg_atlas(dk)
ggseg(atlas = dk)
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
                             steps = 8) %>%
  filter(!is.na(region)) %>%
  mutate(ggseg = ifelse(side == "sagittal",
                        purrr::map(ggseg, ~ mutate(.x, .long = .long + 20)),
                        ggseg),
         region = gsub("cc ", "CC ", region),
         region = gsub("dc", " DC", region),
         region = ifelse(region == "cerebral cortex", NA, region)
  )

aseg_n %>%
  filter(!grepl("white|csf", region)) %>%
  ggseg(atlas = ., show.legend = TRUE,
        colour = "black",
        # position = "s",
        mapping = aes(fill=region)) +
  scale_fill_brain("aseg")

plot(aseg_n)

ggplot() +
  geom_brain(data = aseg_n) +
  scale_fill_brain("aseg")

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
