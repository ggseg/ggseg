library(ggseg)
library(dplyr)
library(ggsegExtra)
devtools::load_all("../ggsegExtra/")


# dk ----
dk_3d <- make_aparc_2_3datlas(output_dir = "data-raw") |>
  mutate(atlas = "dk_3d") |>
  unnest(ggseg_3d) |>
  select(-region) |>
  left_join(select(dk$data, hemi, region, label)) |>
  nest_by(atlas, surf, hemi, .key = "ggseg_3d") |>
  as_ggseg3d_atlas()

dk <- make_ggseg3d_2_ggseg(
  ggseg3d::dk_3d,
  steps = 1:7,
  tolerance = 1.5,
  smoothness = 4,
  output_dir = here::here("data-raw")
)
dk$data$ggseg <- NULL
plot(dk)


usethis::use_data(dk, internal = FALSE, overwrite = TRUE, compress = "xz")


# aseg ----
aseg_n <- aseg |>
  mutate(atlas = "aseg") |>
  as_brain_atlas()
aseg_n$palette <- brain_pals$aseg

lab_file <- file.path(
  "/Applications/freesurfer/",
  "7.2.0/subjects/fsaverage5/mri/aseg.mgz"
)

aseg_n <- make_volumetric_ggseg(
  label_file = lab_file,
  output_dir = "data-raw/aseg",
  steps = 8,
  tolerance = 1,
  smoothness = 2,
  vertex_size_limits = c(20, NA),
  ncores = 16,
  slices = dplyr::tribble(
    ~x  , ~y  , ~z  , ~view     ,
    130 , 130 , 130 , "axial"   ,
    128 , 136 , 100 , "axial"   ,
    127 , 135 , 128 , "coronal"
  )
)

aseg2 <- aseg_n
# # Do some data clean-up
aseg2$data <- aseg_n$data |>
  filter(!is.na(region)) |>
  mutate(
    region = gsub("cc ", "CC ", region),
    region = gsub("dc", " DC", region),
    region = ifelse(region == "cerebral cortex", NA, region)
  ) |>
  filter(!grepl("white|csf", region)) |>
  filter(side != "sagittal") |>
  rbind(
    aseg$data |>
      filter(side == "sagittal") |>
      mutate(roi = NA),
    all = TRUE
  )


names(aseg2$palette) <- gsub("cc", "CC", names(aseg2$palette))
names(aseg2$palette) <- gsub("dc", "DC", names(aseg2$palette))
aseg2$palette <- aseg2$palette[
  !grepl("white|csf|cerebral cortex", names(aseg2$palette))
]

ggseg(
  atlas = aseg2,
  show.legend = TRUE,
  colour = "black",
  mapping = aes(fill = region)
)

plot(aseg2)

ggplot() +
  geom_brain(atlas = aseg2)

aseg <- aseg2

usethis::use_data(aseg, internal = FALSE, overwrite = TRUE, compress = "xz")
