data_merge <- function(.data, geobrain){

  # Find columns they have in common
  cols = names(geobrain)[names(geobrain) %in% names(.data)]

  if(dplyr::is_grouped_df(.data)){

    .data <- .data %>%
      tidyr::nest()

    cols = stats::na.omit(cols[!names(.data) %in% cols])

    geobrain <- .data %>%
      dplyr::mutate(data = purrr::map(data,
                                      ~dplyr::full_join(geobrain, ., by=cols, copy=TRUE))) %>%
      tidyr::unnest(cols = c(data)) %>%
      dplyr::ungroup()

  }else{
    # Merge the brain with the .data
    geobrain = dplyr::full_join(geobrain, .data, by = cols, copy=TRUE)
  }

  # Find if there are instances of those columns that
  # are not present in the geobrain. Maybe mispelled?
  errs = geobrain %>%
    dplyr::filter(is.na(.lat)) %>%
    dplyr::select(!!cols) %>%
    dplyr::distinct() %>%
    tidyr::unite_("tt", cols, sep = " - ") %>%
    dplyr::summarise(value = paste0(tt, collapse = ", "))

  if(errs != ""){
    warning(paste("Some .data is not merged properly into the geobrain. Check for spelling mistakes in:",
                  errs$value))
  }

    return(geobrain)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("data"))
}
