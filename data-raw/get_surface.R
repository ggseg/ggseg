get_surface = function(folder, atlasname){

  surfs = list.dirs(folder, full.names = T, recursive = F)

  hemi = sapply(surfs, list.files, full.names = T) %>% as.character()

  files = sapply(hemi, list.files, pattern="*roi*", full.names = T) %>% as.character()

  mesh = lapply(files, read.ply, ShowSpecimen = F)

  annots = sapply(hemi, list.files, pattern="annot*", full.names = T) %>%
    as.character() %>%
    lapply(read_csv) %>%
    bind_rows()


  data = data.frame(files=files) %>%
    separate(files,
             c("Del1", "DEL2", "DEL3", "atlas","DEL5", "DEL7", "hemi", "surf", "DEL6", "roi", "DELX"),
             remove=F) %>%
    separate(files, remove=F,
             c("Del1", "DEL2", "DEL3", "atlas","DEL5", "DEL7", "hemi", "surf", "DEL6", "roi", "DELX")) %>%
    separate(files, c("DEL1","DEL2","DEL#","DEL4","DEL5","DEL6","filename"), sep="/") %>%
    select(-contains("DEL")) %>%
    left_join(annots, by="filename") %>%
    mutate(annot = ifelse(annot == "unknown", "medialwall", annot)) %>%
    mutate(label = paste(hemi, annot, sep="_"),
           hemi = ifelse(hemi =="lh", "left", "right"),
           atlas = atlasname) %>%
    distinct()

  for(i in 1:length(mesh)){
    data$mesh[[i]] = list(vb=mesh[[i]]$vb,
                          it=mesh[[i]]$it
    )
  }

  data %>%
    select(-filename, -ply) %>%
    group_by(atlas, surf, hemi) %>%
    nest()
}

