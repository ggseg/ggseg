library(ggseg)
library(tidyverse)
atlas.info = list(dkt = dkt,
                  yeo7 = yeo7,
                  yeo17 = yeo17,
                  glasser = glasser,
                  jhu = jhu,
                  aseg = aseg,
                  midsagittal = midsagittal) %>%
  lapply(function(x) x %>% select(area,hemi,side,label) %>% unique %>% na.omit())

### WOrk in progress. get all data in one tibble?
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
save(atlas.info, file="data/atlas.info.RData")
