#' Get information about atlas
#'
#' @param name name of atlas as character string. If left to NULL gives all.
#'
#' @return data.frame
#' @export
#' @importFrom dplyr as_tibble select one_of bind_rows
atlas_info = function(name = NULL){

  n <- brain_pals_info()$atlas

  if(is.null(name)){
    name = n

  }else if(! name %in% n){
    name = paste0("'", name[!(name %in% n)], "'", collapse=", ")

    stop(paste0("Did you specify the correct atlases, could not find ",
                name))
  }

  t <- lapply(name,
              function(x)
                suppressWarnings(
                  get(x) %>%
                    select(one_of(c("area","hemi","side","label") )) %>%
                    unique %>% na.omit())
  )
  names(t) <- name

  suppressWarnings(
    as_tibble(bind_rows(t, .id="atlas"))
  )

}
