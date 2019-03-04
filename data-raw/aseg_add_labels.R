aseg_labs <- data.frame(
  label = c(
  "Left-Lateral-Ventricle",
  "Left-Thalamus-Proper",
  "Left-Caudate",
  "Left-Putamen",
  "Left-Pallidum",
  "Left-Hippocampus",
  "Left-Amygdala",
  "Left-VentralDC",
  "Right-Lateral-Ventricle",
  "Right-Thalamus-Proper",
  "Right-Caudate",
  "Right-Putamen",
  "Right-Pallidum",
  "Right-Hippocampus",
  "Right-Amygdala",
  "Right-VentralDC"),
  area =  c(
    "lateral ventricle",
    "thalamus proper",
    "caudate",
    "putamen",
    "pallidum",
    "hippocampus",
    "amygdala",
    "ventral DC",
    "lateral ventricle",
    "thalamus proper",
    "caudate",
    "putamen",
    "pallidum",
    "hippocampus",
    "amygdala",
    "ventral DC")
)

aseg <- aseg %>%
  left_join(aseg_labs) %>%
  as_ggseg_atlas() %>%
  select(-group, -piece)
usethis::use_data(aseg, internal = FALSE, overwrite = TRUE)
