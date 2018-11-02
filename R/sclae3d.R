#' @importFrom paletteer paletteer_d paletteer_dynamic paletteer_c
#' @importFrom purrr map is_empty
#' @importFrom rlang "!!"
scale3d = function(palette){

  pals = ggseg:::paletteers

  pal_src = paste0("^", palette, "$")

  idx = purrr::map(pals$palettes, function(x) grep(pal_src, x$palette))
  type_indx = which(unlist(lapply(idx, function(x) !purrr::is_empty(x))))

  idx = unlist(idx)
  pkg = pals$palettes[[type_indx]][idx,"package"]

  switch(pals$type[type_indx],
         "discrete" = {
           paletteer::paletteer_d(package = !!pkg,
                                  palette = !!palette,
                                  )
         },
         "dynamic" = {
           paletteer::paletteer_dynamic(package = !!pkg,
                                  palette = !!palette)
         },
         "continuous" = {
           paletteer::paletteer_c(package = !!pkg,
                                  palette = !!palette,
                                  5)
         }
  )
}
