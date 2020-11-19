devtools::load_all(".")
devtools::load_all("../ggsegExtra/")


# dk ----
dk_3d <- make_aparc_2_3datlas(output_dir = "data-raw") %>%
  mutate(atlas = "dk_3d")%>%
  unnest(ggseg_3d) %>%
  select(-region) %>%
  left_join(select(dk$data, hemi, region, label)) %>%
  nest_by(atlas, surf, hemi, .key = "ggseg_3d") %>%
  as_ggseg3d_atlas()

dk <- make_ggseg3d_2_ggseg(ggseg3d::dk_3d,
                           steps = 1:7,
                           tolerance = 1.5,
                           smoothness = 4,
                           output_dir = here::here("data-raw"))
dk$data$ggseg <- NULL
plot(dk)



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


