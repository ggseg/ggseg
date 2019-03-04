library(ggseg)
library(tidyverse)
atlas_info = list(dkt = dkt,
                  aseg = aseg
                  ) %>%
  lapply(function(x) x %>%
           select(one_of(c("area","hemi","side","label") )) %>%
           unique %>% na.omit())
usethis::use_data(atlas_info, internal = FALSE, overwrite = TRUE)

### Work in progress. get all data in one tibble?
# d <- data(package = "ggseg")
# ## names of data sets in the package
# d = data.frame(atlas = d$results[, "Item"], stringsAsFactors = F) %>%
#   filter(!grepl("atlas|pal", atlas)) %>%
#   mutate(type = ifelse(grepl("_3d$", atlas),"plotly","ggplot")) %>%
#   group_by_all() %>%
#   nest %>%
#   arrange(type)
#
# clean_it = function(x){
#   tt = get(x)
#   if("side" %in% names(tt)){
#     tt = tt %>%
#       group_by_at(vars(one_of(c("atlas","side","area","label")))) %>%
#       nest()
#   }else{
#     tt
#   }
# }
#
# tt = lapply(d$atlas, function(x) clean_it(x) )
#
# for(i in 1:nrow(d)){
#   d$data[[i]] = tt[[i]]
# }
#
# atlas.info = d
