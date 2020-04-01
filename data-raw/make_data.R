# dk ----
library(dplyr)
library(tidyr)
library(ggplot2)
devtools::load_all("../ggsegExtra/")

dk <- ggsegExtra::make_ggseg3d_2_ggseg(ggseg3d::dk_3d,
                                       steps = 5:6,
                                       smoothness = 8,
                                       output_dir = "~/Desktop/test/")
dk <- mutate(dk, atlas = "dk")
dk <- as_ggseg_atlas(dk)

ggseg(atlas=dk, show.legend = FALSE,
      colour = "black", position="stacked",
      mapping = aes(fill=region)) +
  scale_fill_brain()


usethis::use_data(dk,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress="xz")


# aseg ----
aseg_n <- mutate(aseg,
       kp = case_when(
         grepl("Left", label) & hemi != "left" ~  FALSE,
         grepl("Right", label) & hemi != "right" ~ FALSE,
         TRUE ~ TRUE)) %>%
  filter(kp) %>%
  select(-kp)
aseg_n <- unnest(aseg_n, ggseg)
aseg_n <- group_by(aseg_n, label, hemi,  side, region, .id)
aseg_n <- nest(aseg_n)
aseg_n <- group_by(aseg_n, hemi,  side, region)
aseg_n <- mutate(aseg_n, .subid = row_number())
aseg_n <- unnest(aseg_n, data)
aseg_n <- ungroup(aseg_n)
aseg_n <- as_ggseg_atlas(aseg_n)

aseg_n %>%
  ggseg(atlas = ., show.legend = TRUE,
      colour = "black",
      mapping = aes(fill=region)) +
  scale_fill_brain("aseg", na.value="black")


aseg <- aseg_n
usethis::use_data(aseg,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress="xz")


