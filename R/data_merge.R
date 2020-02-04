data_merge <- function(.data, geobrain){

  # Find columns they have in common
  cols = names(geobrain)[names(geobrain) %in% names(.data)]

  if(dplyr::is_grouped_df(.data)){

    .data <- tidyr::nest(.data)

    cols = stats::na.omit(cols[!names(.data) %in% cols])

    geobrain <- dplyr::mutate(.data,
                              data = purrr::map(data,
                                                ~dplyr::full_join(geobrain, ., by=cols, copy=TRUE)))
    geobrain <- tidyr::unnest(geobrain, cols = c(data))
    geobrain <- dplyr::ungroup(geobrain)

  }else{
    # Merge the brain with the .data
    geobrain = dplyr::full_join(geobrain, .data, by = cols, copy=TRUE)
  }

  # Find if there are instances of those columns that
  # are not present in the geobrain. Maybe mispelled?
  errs = dplyr::filter(geobrain, is.na(.lat))
  errs <- dplyr::select(errs, !!cols)
  errs <- dplyr::distinct(errs)
  errs <- tidyr::unite_(errs, "tt", cols, sep = " - ")
  errs <- dplyr::summarise(errs, xvalue = paste0(tt, collapse = ", "))

  if(errs != ""){
    warning(paste("Some .data is not merged properly into the geobrain. Check for spelling mistakes in:",
                  errs$value))
  }

  return(geobrain)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("data", "tt"))
}
