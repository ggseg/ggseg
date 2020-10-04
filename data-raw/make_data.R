devtools::load_all(".")
devtools::load_all("../ggsegExtra/")

# dk ----
dk <- make_ggseg3d_2_ggseg(ggseg3d::dk_3d,
                           steps = 4:7,
                           tolerance = .5,
                           smoothness = 5,
                           output_dir = here::here("data-raw"))


ggseg(atlas=dk, show.legend = FALSE,
      colour = "black", position="stacked",
      mapping = aes(fill=region)) +
  scale_fill_brain()

plot(dk)

ggplot() +
  geom_brain(data = dk, aes(fill = region),
             position = position_brain("vertical"),
             show.legend = FALSE) +
  scale_fill_brain()


someData <- dplyr::tibble(
  region = c("transverse temporal", "insula",
             "precentral","superior parietal",
             "transverse temporal", "insula",
             "precentral","superior parietal"),
  p = sample(seq(0,.5,.001), 8),
  Group = c(rep("G1",4), rep("G2",4))
)
someData %>%
  ggplot() +
  geom_brain(atlas = dk, aes(fill = p),
             show.legend = FALSE) +
  facet_wrap(~Group)


usethis::use_data(dk,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress="xz")


# aseg ----
aseg_n <- aseg %>%
  mutate(atlas = "aseg") %>%
  as_brain_atlas()
aseg_n$palette <- brain_pals$aseg


aseg_n <- make_subcort_ggseg(output_dir = "data-raw/aseg",
                             steps = 5:8,
                             tolerance = 1,
                             smoothness = 2,
                             vertex_size_limits = c(15, NA))
#
# # Do some data clean-up
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


