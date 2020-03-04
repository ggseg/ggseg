
#' Read in raw Freesurfer stats file
#'
#' Freesurfer atlas stats files have a format
#' that can be difficult to easily read in to R.
#' This function takes a raw stats-file from the
#' subjects directory and reads it in as a
#' data.frame.
#'
#' @param path path to stats file
#' @param rename logical. rename headers for ggseg compatibility
#'
#' @return tibble
#' @export
read_freesurfer_stats <- function(path, rename = TRUE){

  # get headers
  headers <- readLines(path)
  headers <- headers[grepl("^#", headers)]

  headers <- gsub("# ", "", headers)[length(headers)]
  headers <- strsplit(headers, " ")[[1]]

  # header cleanup for files with off headers
  headers <- headers[!grepl("ColHeaders", headers)]
  headers <- headers[headers != ""]

  data <- dplyr::as_tibble(utils::read.table(path,
                                      stringsAsFactors = FALSE))
  names(data) <- headers

  if(rename) data <- dplyr::rename(data, label = StructName)

  return(data)
}

#' Read in atlas data from all subjects
#'
#' Recursively reads in all stats files for
#' an atlas (given a unique character string),
#' for all subjects in the subjects directory.
#' Will add hemisphere and subject id to the data.
#'
#' @param subjects_dir Freesurfer subject directory
#' @param atlas unique character combination identifying the atlas
#'
#' @return tibble
#' @export
read_atlas_files <- function(subjects_dir, atlas){
  stats_files <- list.files(subjects_dir, pattern = atlas,
                            full.names = TRUE, recursive = TRUE)
  stats_files <- stats_files[grepl("stats$", stats_files)]

  stats <- lapply(stats_files, read_freesurfer_stats)

  subject <- gsub(subjects_dir, "", stats_files)
  subject <- unname(sapply(subject, find_subject_fromdir))
  hemi <- unname(sapply(stats_files, find_hemi_fromfile))

  if(all(hemi %in% c("rh", "lh"))){
    names(stats) <- paste(subject, hemi, sep = "_")

    stats <- dplyr::bind_rows(stats, .id = "id")
    stats <- tidyr::separate(stats, id, c("subject", "hemi"))
    stats <- tidyr::unite(stats, label, hemi, label)

  }else{
    names(stats) <- subject
    stats <- dplyr::bind_rows(stats, .id = "subject")
  }

  return(stats)
}


#' Read in stats table from Freesurfer
#'
#' Freesurfer has functions to create
#' tables from raw stats files. If you have
#' data already merged using the \code{aparcstats2table}
#' or \code{asegstats2table} from Freesurfer,
#' this function will read in the data and prepare it
#' for ggseg.
#'
#' @param path path to the table file
#' @param measure which measure is the table of
#' @param ... additional arguments to \code{utils::read.table}
#'
#' @return tibble
#' @export
read_freesurfer_table <- function(path, measure = NULL, ...){
  dat <- utils::read.table(path, header = TRUE, ...)
  names(dat)[1] <- "subject"

  dat <- tidyr::gather(dat, label, value, -subject)

  if(!is.null(measure)){
    dat <- dplyr::mutate(dat, label = gsub(paste0("_", measure), "", label))

    names(dat)[names(dat) %in% "value"] <- measure
  }

  if(any(grepl("\\.", dat$label))){
    dat$label <- gsub("\\.", "-", dat$label)
  }

  dplyr::as_tibble(dat)
}


#' helper function to easily grab subject information from directory path
#' @param path file path
find_subject_fromdir <- function(path){
  strsplit(path, "/")[[1]][2]
}

#' helper function to easily grab hemisphere information from file path
#'
#' @param path file path
find_hemi_fromfile <- function(path){
  strsplit(basename(path), "\\.")[[1]][1]
}


## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("id", "label", "StructName",
                           "subject", "value"))
}
