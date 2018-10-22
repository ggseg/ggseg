jhu = geobrain_JHU %>%
  mutate(aparc = gsub("ifl", "ilf", aparc)) %>%
  separate(aparc, c("hemi","acronym"), remove = FALSE) %>%
  mutate(acronym = ifelse(!hemi %in% c("lh","rh"), hemi, acronym),
         acronym = ifelse(acronym %in% "gm", NA, acronym),
         aparc = ifelse(aparc %in% "gm", NA, aparc),
         hemi = ifelse(hemi %in% "lh", "left",
                       ifelse(hemi %in% "rh", "right", "center")),
  ) %>%
  mutate(side = ifelse(id %in% c(200:240), "upper coronal",
                       ifelse(id %in% c(1:13), "lower coronal","axial")),
         atlas = "jhu"
  ) %>%
  mutate(area = ifelse(id %in% c(5,8,217),
                       "Superior longitudinal fasciculus (temporal part)", area))

tmp = data.frame(area = c("Anterior thalamic radiation", "Corticospinal tract","Cingulum (cingulate gyrus)",
                          "Cingulum (hippocampus)","Forceps major","Forceps minor",
                          "Inferior fronto-occipital fasciculus","Inferior longitudinal fasciculus","Superior longitudinal fasciculus",
                          "Uncinate fasciculus","Superior longitudinal fasciculus (temporal part)","Cerebral spinal fluid"),
                 acronym=c("atr","cst","ccg","cab","fmajor","fminor","ifof","ilf","slf","unc","slft", "csf"))

jhu = jhu %>%
  left_join(tmp) %>%
  rename(label=aparc) %>%
  select(lat, long, area, hemi, side, acronym, atlas, everything()) %>%
  select(-meas, -piece)
save(jhu, file="data/jhu.RData")
