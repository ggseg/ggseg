j <- dplyr::slice(ggseg3d::dk_3d, 1) |>
  tidyr::unnest(ggseg_3d) |>
  select(region, colour) |>
  distinct() |>
  na.omit()

dk_palette <- setNames(
  j$colour,
  j$region
)

j <- dplyr::slice(ggseg3d::aseg_3d, 1) |>
  tidyr::unnest(ggseg_3d) |>
  select(region, colour) |>
  mutate(
    region = gsub("-|_", " ", region),
    region = tolower(region),
    region = gsub("left |right ", "", region),
    region = gsub("cc ", "CC ", region),
    region = gsub("inf", "", region),
    region = gsub("ventraldc", "ventral DC", region)
  ) |>
  distinct() |>
  na.omit()

aseg_palette <- setNames(
  j$colour,
  j$region
)

brain_pals = list(
  dk = dk_palette,
  aseg = aseg_palette
)

usethis::use_data(
  brain_pals,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz"
)
