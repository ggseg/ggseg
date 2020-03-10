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
    "Right-VentralDC",
    "x3rd-Ventricle",
    "x4th-Ventricle",
    "brain-stem",
    "cc-anterior",
    "cc-central",
    "cc-mid-anterior",
    "cc-mid-posterior",
    "cc-posterior",
    "right-cerebellum-white-matter",
    "right-cerebellum-cortex"
  ),
  region =  c(
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
    "ventral DC",
    "3rd ventricle",
    "4th ventricle",
    "brain stem",
    "CC anterior",
    "CC central",
    "CC mid anterior",
    "CC mid posterior",
    "CC posterior",
    "cerebellum cortex",
    "cerebellum white matter")
)

# aseg <- aseg %>%
#   left_join(aseg_labs) %>%
#   as_ggseg_atlas() %>%
#   select(-group, -piece)
#
# aseg <- aseg %>%
#   filter(!(grepl("right", hemi) & grepl("Left", label))) %>%
#   filter(!(grepl("left", hemi) & grepl("Right", label)))

# add midsagittal to aseg
aseg <- ggseg::aseg %>%
  unnest(ggseg) %>%
  mutate(.lat = .lat-min(.lat),
         .long = .long-min(.long)-.5,
         .id = as.integer(.id)) %>%
  select(-.pos) %>%
  bind_rows(
    ggsegExtra::midsagittal %>%
      unnest(ggseg) %>%
      mutate(.lat = (.lat-min(.lat)) * 1.18,
             .long = ((.long-min(.long)) * 1.18) + 3.5,
             .id = as.integer(.id) + 30,
             hemi = "midline",
             region = ifelse(region %in% "cerebral cortex", NA, region)) %>%
      select(-.pos)
  )  %>%
  select(-label) %>%
  left_join(aseg_labs, by = "region") %>%
  as_ggseg_atlas()


aseg <- aseg %>% rename(region = area)

usethis::use_data(aseg, internal = FALSE, overwrite = TRUE)
